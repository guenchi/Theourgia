#!r6rs
;; Copyright 2026 guenchi
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; The store: the log and the reduction joined. This is the read-only
;; half -- open a store, replay what is durable into a reduction, and
;; hand back the state. The write side is a separate section.
(library (theourgia store)
  (export open-and-reduce with-store-write store-init! nearest-ids store-snapshot!
          store-check store-adopt! store-search store-refs store-log store-tags parse-cut store-diff store-conflicts)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs io ports) (rnrs files)
          (only (theourgia md) md-refs)
          (rnrs arithmetic fixnums) (rnrs unicode) (rnrs bytevectors)
          (only (theourgia log)
                log-open load-deliver! load-commit!
                load-snapshot-cut load-snapshot-rows
                log-begin log-end! session-view session-view-refusal session-append! session-applied!
                session-epoch make-frame atomic-write! segment-file-name
                session-snapshot! log-open load-writers load-prefix load-commit!
                discovery-end-seq discovery-integrity discovery-torn
                snapshot-read snapshot-cut-supported? segment-file-number
                log-error-kind log-error-segment log-error-offset log-error-detail
                store-id-of adopt! registry-inside-store?
                instance-install! owner-install! writer-directory store-writers
                store-register!
                view-revision view-epoch view-writer view-expect-seq)
          (only (theourgia ffi) mkdir-p! wall-clock-ms process-id directory-entries)
          (only (igropyr crypto) sha256 bytevector->hex)
          (theourgia reduce))

  ;; WHAT THE REDUCER ANSWERS AND WHAT THE LOAD ASKS ARE NOT THE SAME
  ;; QUESTION. The load asks "did this record change the applied state",
  ;; and the reducer answers "did I accept it" -- accepted covers both
  ;; applied and waiting for a premise that has not been read yet.
  ;; Reporting accepted as applied would move the applied cursor over a
  ;; record still sitting in pending.
  (define (applied? r writer seq)
    (let ((e (assoc writer (reduce-applied-cut r))))
      (and e (>= (cdr e) seq))))

  ;; A RECORD OUTSIDE THE CUT IS NOT FED TO THE REDUCER AT ALL, rather
  ;; than fed and then subtracted: a reduction is a function of the
  ;; records it was given, and the whole point of reading at a cut is to
  ;; be given exactly the records the cut names.
  (define (within? cut writer seq)
    (or (not cut)
        (let ((e (assoc writer cut)))
          (and e (<= seq (cdr e))))))

  (define (deliver-into r cut)
    (lambda (writer seg off seq ts actor deps payload)
      (if (not (within? cut writer seq))
          'skipped
          (let ((answer (reduce-apply! r writer seq deps payload)))
            (cond
              ((eq? answer 'accepted)
               (if (applied? r writer seq) 'applied 'pending))
              ;; A record the log delivers twice in one pass is the log's
              ;; business, not a reason to stop the writer: it is already
              ;; in the state, so the honest answer is applied.
              ((and (pair? answer) (eq? (cadr answer) 'already-applied)) 'applied)
              (else (list 'rejected (cadr answer))))))))

  ;; A READER TAKES THE SHARED LOCK, NOT THE EXCLUSIVE ONE. log-begin is
  ;; the write session's door: it claims the store, runs the takeover
  ;; barrier and the metadata barrier, and delivers the whole log from
  ;; the empty cut. A reader needs none of that and must not hold the
  ;; store against writers -- and delivering from the empty cut means
  ;; the snapshot could never be used, which is the whole reason a
  ;; snapshot exists.
  ;; SEEDED FROM THE SNAPSHOT WHEN THERE IS ONE, and replay then starts
  ;; after the snapshot's cut rather than at the beginning.
  (define (replay store cut)
    (let ((ls (log-open store)))
      (let* ((rows (and (not cut) (load-snapshot-rows ls)))
             (r (if rows (rows->state rows) (reduce-empty)))
             (from (if rows (load-snapshot-cut ls) '())))
        (load-deliver! ls from (deliver-into r cut))
        (load-commit! ls)
        r)))

  ;; READING AT A CUT DOES NOT USE THE SNAPSHOT. The snapshot stands at
  ;; whatever cut it was written at, which may be after the one being
  ;; asked for; seeding from it would put records into the answer that
  ;; the caller's cut excludes. Replaying from the beginning is the only
  ;; answer that is right for every requested cut.
  ;; TWO PASSES WHEN A CUT IS ASKED FOR, and the first one is what
  ;; decides usability. Whether a cut is causally closed is a question
  ;; about the records that exist, not about the ones the cut selects --
  ;; asking it of the restricted reduction would call every cut closed,
  ;; because the premises it omits were never read.
  (define (open-and-reduce store . rest)
    (let ((cut (if (null? rest) #f (car rest))))
      (if (not cut)
          (replay store #f)
          (let* ((whole (replay store #f))
                 (verdict (cut-usable? whole cut)))
            (if (eq? verdict 'usable)
                (replay store cut)
                verdict)))))

  ;; ---- the write side ------------------------------------------------------

  ;; AN INTENT IS WHAT THE CALLER WANTS, NOT A RECORD. Resolving one --
  ;; deriving the block id from the sequence number actually reserved,
  ;; the ord from the siblings actually present, the deps from the
  ;; causally closed prefix actually applied -- depends on state that
  ;; only exists inside the lock, and on state that CHANGES between one
  ;; intent and the next in a batch. A caller handed frames to fill in
  ;; would be resolving against a view that is already one record old by
  ;; its second item.
  ;;
  ;;   (insert <parent> <after-or-#f> <fields-alist>)
  ;;   (set <id> <field> <value>) | (set <id> <field>)
  ;;   (move <id> <parent> <after-or-#f>)
  ;;   (del <id>)
  ;;   (link <from> <rel> <to>) | (unlink <from> <rel> <to>)
  ;;   (expect <block-hash> <intent>)
  ;;
  ;; THE ANSWER IS PER INTENT, ALWAYS -- a list, one entry per intent
  ;; attempted. A single command renders its one answer and a batch
  ;; renders the list; neither needs the library to behave differently
  ;; depending on how many intents it was given.

  ;; THE NEAREST IDS TO ONE THAT IS NOT HERE. A block id is
  ;; <writer>.<sequence>, and a mistyped id is almost always mistyped in
  ;; the tail -- so an id from the same writer is nearer than any id from
  ;; another writer, however much text they happen to share, and among
  ;; the same writer's ids the nearest are those whose sequence is
  ;; closest. An earlier version ranked by shared prefix alone, which
  ;; approximated "the same writer" and then answered `.1 .2 .3` for a
  ;; request for `.7`: every candidate shared the whole writer, so the
  ;; tie fell to lexicographic order and the three furthest sequences
  ;; came back.
  ;;
  ;; Ties on distance go to the lower sequence, so the answer is total
  ;; and two runs of the same store cannot differ.
  ;; THE TAIL IS SCANNED BEFORE IT IS CONVERTED, so only ASCII digits
  ;; ever reach `string->number` and the numeric syntax -- `#e` and an
  ;; exponent above all -- cannot enter through an id. The work is then
  ;; proportional to the text, which is the property the shape test buys.
  (define (split-id id)
    (let loop ((i (- (string-length id) 1)))
      (cond
        ((< i 0) (values id #f))
        ((char=? (string-ref id i) #\.)
         (let ((tail (substring id (+ i 1) (string-length id))))
           (values (substring id 0 i)
                   (let scan ((k 0))
                     (cond
                       ((= k (string-length tail))
                        (and (> (string-length tail) 0) (string->number tail 10)))
                       ((char<=? #\0 (string-ref tail k) #\9) (scan (+ k 1)))
                       (else #f))))))
        (else (loop (- i 1))))))

  (define (nearest-ids state id)
    (define (shared a b)
      (let loop ((i 0))
        (if (or (>= i (string-length a)) (>= i (string-length b))
                (not (char=? (string-ref a i) (string-ref b i))))
            i
            (loop (+ i 1)))))
    (define (take3 xs)
      (let loop ((xs xs) (n 0) (out '()))
        (if (or (null? xs) (= n 3)) (reverse out) (loop (cdr xs) (+ n 1) (cons (car xs) out)))))
    (let-values (((writer seq) (split-id id)))
      (let* ((ids (map cadr (state-datum state)))
             (same (if (and writer seq)
                       (filter (lambda (other)
                                 (let-values (((w s) (split-id other)))
                                   (and w s (string=? w writer))))
                               ids)
                       '())))
        (if (not (null? same))
            (take3 (list-sort
                     (lambda (a b)
                       (let-values (((wa sa) (split-id a)) ((wb sb) (split-id b)))
                         (let ((da (abs (- sa seq))) (db (abs (- sb seq))))
                           (if (= da db) (< sa sb) (< da db)))))
                     same))
            (take3 (list-sort
                     (lambda (a b)
                       (let ((sa (shared id a)) (sb (shared id b)))
                         (if (= sa sb) (string<? a b) (> sa sb))))
                     ids))))))


  ;; ---- search ---------------------------------------------------------------

  ;; SEARCH IS A READ, AND ITS RULES LIVE HERE. What "matches" means is
  ;; not the command's business: a second caller must not be able to come
  ;; to a different answer about the same store.
  ;;
  ;; A QUERY IS TOKENS AND EVERY TOKEN MUST HIT. Tokens are split on
  ;; whitespace; a hit is a case-insensitive substring, so `cat' hits
  ;; `concatenate'. A repeated hit does not add score -- a field is
  ;; either hit or it is not -- so title is worth 2, src is worth 1, and
  ;; a block hit in both scores 3.
  ;;
  ;; WITH MORE THAN ONE TOKEN the fields still score as a whole: the
  ;; block matches when every token hits somewhere in it, and the score
  ;; says which fields were hit at all. For a single token, the case the
  ;; rule was fixed for, the two readings coincide.
  ;;
  ;; THE ORDER IS TOTAL -- score descending, then id ascending -- so
  ;; nothing in the output can depend on the order a hash table or an
  ;; alist happens to hand back. Two runs over one store agree.
  ;;
  ;; A QUERY IS ONLY EVER A STRING. No part of it reaches a numeric
  ;; parser, so a token like `#e1e99999999' is text to match, not a
  ;; number to build.
  (define (whitespace? c)
    (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
        (char=? c #\return) (char=? c #\page)))

  (define (tokens-of query)
    (let loop ((i 0) (start #f) (out (quote ())))
      (cond
        ((= i (string-length query))
         (reverse (if start (cons (substring query start i) out) out)))
        ((whitespace? (string-ref query i))
         (loop (+ i 1) #f (if start (cons (substring query start i) out) out)))
        (else (loop (+ i 1) (or start i) out)))))

  (define (contains-ci? text token)
    (let* ((t (string-downcase text))
           (q (string-downcase token))
           (n (string-length t))
           (m (string-length q)))
      (and (<= m n)
           (let loop ((i 0))
             (cond
               ((> (+ i m) n) #f)
               ((string=? (substring t i (+ i m)) q) #t)
               (else (loop (+ i 1))))))))

  ;; ONLY TEXT IS SEARCHED. A field whose value is not a string is not
  ;; text, and a field in conflict offers every candidate that is.
  (define (field-strings block name)
    (let* ((fields (cdr (assq (quote fields) block)))
           (e (assq name fields)))
      (cond
        ((not e) (quote ()))
        ((string? (cdr e)) (list (cdr e)))
        ((and (pair? (cdr e)) (eq? (car (cdr e)) (quote conflict)))
         (filter string? (map car (cadr (cdr e)))))
        (else (quote ())))))

  (define (any-hit? strings token)
    (exists (lambda (text) (contains-ci? text token)) strings))

  (define (lines-of-text text)
    (let loop ((i 0) (start 0) (out (quote ())))
      (cond
        ((= i (string-length text)) (reverse (cons (substring text start i) out)))
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
        (else (loop (+ i 1) start out)))))

  (define snippet-limit 80)

  (define (collapse-whitespace text)
    (let loop ((i 0) (gap #f) (out (quote ())))
      (cond
        ((= i (string-length text))
         (list->string (reverse (if (and (pair? out) (char=? (car out) #\space))
                                    (cdr out)
                                    out))))
        ((whitespace? (string-ref text i))
         (loop (+ i 1) #t out))
        (else
         (loop (+ i 1) #f
               (cons (string-ref text i)
                     (if (and gap (pair? out)) (cons #\space out) out)))))))

  (define (clip text)
    (if (> (string-length text) snippet-limit)
        (substring text 0 snippet-limit)
        text))

  ;; THE SNIPPET IS THE FIRST LINE THAT HITS, title before src, with its
  ;; whitespace collapsed and cut to the limit.
  (define (snippet-for block tokens)
    (let* ((titles (field-strings block (quote title)))
           (srcs (field-strings block (quote src)))
           (lines (append titles
                          (apply append (map lines-of-text srcs)))))
      (let loop ((ls lines))
        (cond
          ((null? ls) "")
          ((exists (lambda (tk) (contains-ci? (car ls) tk)) tokens)
           (clip (collapse-whitespace (car ls))))
          (else (loop (cdr ls)))))))

  (define (store-search store query)
    (let* ((state (open-and-reduce store))
           (tokens (tokens-of query)))
      (if (null? tokens)
          (quote ())
          (let ((hits
                  (let loop ((ds (state-datum state)) (out (quote ())))
                    (if (null? ds)
                        out
                        (let* ((id (cadr (car ds)))
                               (block (state-read state id))
                               (titles (field-strings block (quote title)))
                               (srcs (field-strings block (quote src)))
                               (in-title (exists (lambda (tk) (any-hit? titles tk)) tokens))
                               (in-src (exists (lambda (tk) (any-hit? srcs tk)) tokens))
                               (every-token
                                 (for-all (lambda (tk)
                                            (or (any-hit? titles tk) (any-hit? srcs tk)))
                                          tokens)))
                          (loop (cdr ds)
                                (if every-token
                                    (cons (list id
                                                (+ (if in-title 2 0) (if in-src 1 0))
                                                (snippet-for block tokens))
                                          out)
                                    out)))))))
            (list-sort (lambda (a b)
                         (if (= (cadr a) (cadr b))
                             (string<? (car a) (car b))
                             (> (cadr a) (cadr b))))
                       hits)))))

  (define (known? state id) (and (state-read state id) #t))
  ;; ---- conflicts ------------------------------------------------------------

  ;; THE THREE WAYS A STORE CAN BE HOLDING SOMETHING IT CANNOT SHOW.
  ;;
  ;; A STRUCTURAL CONFLICT is read from the same place the outline reads
  ;; it, so the two can never disagree about which blocks are in one: a
  ;; second opinion here would be a second answer to "what does this
  ;; store look like". `cycle` is a parent chain that closes on itself;
  ;; `unplaced` is a block whose position never settled, which is what
  ;; two concurrent moves leave behind.
  ;;
  ;; AN ORPHAN has a parent it cannot hang under -- deleted, or never
  ;; received. Its records are applied and readable; what it has lost is
  ;; a place in the tree.
  ;;
  ;; A PENDING RECORD is waiting for premises. EVERY missing premise is
  ;; listed, not the first one: an implementation that stopped at the
  ;; first would send an operator to fetch one record and leave them
  ;; exactly where they started.
  (define (missing-deps state deps)
    (let loop ((ds deps) (out (quote ())))
      (cond
        ((null? ds) (reverse out))
        (else
         (let* ((writer (car (car ds)))
                (need (cdr (car ds)))
                (have (assoc writer (reduce-applied-cut state))))
           (loop (cdr ds)
                 (if (and have (>= (cdr have) need))
                     out
                     (cons (car ds) out))))))))

  (define (store-conflicts store)
    (let* ((state (open-and-reduce store))
           (structure (state-structure state))
           (cyclic (cdr (assq (quote conflicts) structure)))
           (unplaced (cdr (assq (quote unplaced) structure)))
           (orphans (cdr (assq (quote orphans) structure)))
           (pending (reduce-pending state)))
      (append
        (map (lambda (id) (list (quote conflict) id (quote cycle))) cyclic)
        (map (lambda (id) (list (quote conflict) id (quote unplaced))) unplaced)
        (map (lambda (id) (list (quote orphan) id)) orphans)
        (apply append
               (map (lambda (rec)
                      (let ((writer (car rec)) (seq (cadr rec)) (deps (caddr rec)))
                        (map (lambda (d)
                               (list (quote pending)
                                     (list (quote event) writer seq)
                                     (list (quote missing) (car d) (cdr d))))
                             (missing-deps state deps))))
                    pending)))))


  ;; ---- diff -----------------------------------------------------------------

  ;; TWO STATES, EACH BUILT BY READING TO ITS OWN CUT. Not the current
  ;; state with something subtracted: a cut names what had been applied
  ;; at a moment, and the only way to know what the store said then is to
  ;; read it then.
  ;;
  ;; THE PROJECTION IS STATED, because "changed" is only meaningful
  ;; against a list of what counts. A block is title, src and level; where
  ;; it sits, which is parent and ord; and what it points at, which is the
  ;; edge set. Comparing only the scalars would call two states identical
  ;; when an edge had been added, which is a change anybody looking at the
  ;; outline would see.
  (define (field-datum block name)
    (let ((e (assq name (cdr (assq (quote fields) block)))))
      (and e (cdr e))))

  (define (position-of block)
    (let ((p (cdr (assq (quote position) block))))
      (if (and (pair? p) (eq? (car p) (quote conflict)))
          (cons (quote conflict) (quote conflict))
          p)))

  (define (projection state id)
    (let ((b (state-read state id)))
      (and b
           (not (cdr (assq (quote deleted) b)))
           (list (cons (quote title) (field-datum b (quote title)))
                 (cons (quote src) (field-datum b (quote src)))
                 (cons (quote level) (field-datum b (quote level)))
                 (cons (quote parent) (car (position-of b)))
                 (cons (quote ord) (cdr (position-of b)))
                 (cons (quote links) (cdr (assq (quote edges) b)))))))

  (define projected-fields (quote (title src level parent ord links)))

  (define (ids-of state) (map cadr (state-datum state)))

  (define (union-ids a b)
    (list-sort string<?
               (let loop ((xs (append (ids-of a) (ids-of b))) (out (quote ())))
                 (cond ((null? xs) out)
                       ((member (car xs) out) (loop (cdr xs) out))
                       (else (loop (cdr xs) (cons (car xs) out)))))))

  (define (diff-states from to)
    (let loop ((ids (union-ids from to)) (out (quote ())))
      (if (null? ids)
          (reverse out)
          (let* ((id (car ids))
                 (a (projection from id))
                 (b (projection to id)))
            (loop (cdr ids)
                  (cond
                    ((and (not a) b) (cons (list (quote added) id) out))
                    ((and a (not b)) (cons (list (quote removed) id) out))
                    ((not a) out)
                    (else
                     (let inner ((fs projected-fields) (acc out))
                       (if (null? fs)
                           acc
                           (inner (cdr fs)
                                  (if (equal? (cdr (assq (car fs) a))
                                              (cdr (assq (car fs) b)))
                                      acc
                                      (cons (list (quote changed) id (car fs)) acc)))))))))))) 

  ;; A CUT ARGUMENT IS EITHER A LITERAL OR A NAME. The literal is parsed
  ;; by shape; a name is looked up among the tags, and an unsettled name
  ;; is refused rather than resolved to one of its candidates -- picking
  ;; one here would answer a question about history with a guess.
  (define (resolve-cut store which text)
    (let ((literal (parse-cut text)))
      (if literal
          (list (quote ok) literal)
          (let ((found (filter (lambda (t) (equal? (cadr t) text)) (store-tags store))))
            (cond
              ((null? found)
               (list (quote error) (quote unknown-tag) text (list (quote cut) which)))
              ((eq? (car (car found)) (quote unsettled))
               (list (quote error) (quote tag-unsettled) text (list (quote cut) which)))
              (else (list (quote ok) (caddr (car found)))))))))

  (define (store-diff store from-text to-text)
    (let ((from (resolve-cut store (quote from) from-text))
          (to (resolve-cut store (quote to) to-text)))
      (cond
        ((eq? (car from) (quote error)) from)
        ((eq? (car to) (quote error)) to)
        (else
         (let* ((state (open-and-reduce store))
                (bad (let loop ((cs (list (cons (quote from) (cadr from))
                                          (cons (quote to) (cadr to)))))
                       (cond
                         ((null? cs) #f)
                         ((let ((u (cut-usable? state (cdr (car cs)))))
                            (and (not (eq? u (quote usable)))
                                 (list (quote error) (quote cut-unavailable)
                                       (list (quote cut) (car (car cs)))
                                       (list (quote reason)
                                             (if (pair? u) (cadr u) u)))))
                          => (lambda (answer) answer))
                         (else (loop (cdr cs))))))) 
           (if bad
               bad
               (cons (quote ok)
                     (list (diff-states (replay store (cadr from))
                                        (replay store (cadr to)))))))))))


  ;; ---- cuts on the command line ---------------------------------------------

  ;; A CUT LITERAL IS PARSED BY HAND, NEVER BY `read`. The reader
  ;; implements the whole of Scheme's numeric syntax, and `#e` with an
  ;; exponent asks it to build an exact integer of any size at all: eleven
  ;; characters of argument can ask for hours of allocation, and nothing
  ;; placed after the call ever runs because the call does not return.
  ;; Measured on this implementation: `#e1e100000` 3 ms, `#e1e1000000`
  ;; 74 ms, `#e1e4000000` 541 ms, with the text the same length
  ;; throughout. The same hazard was closed in the numeric arguments; a
  ;; cut literal is the other door into it.
  ;;
  ;; So the shape is checked as it is read: parentheses, a quoted writer
  ;; name of the characters a writer name may hold, a dot, ASCII digits of
  ;; bounded length. Nothing else is accepted and nothing is converted
  ;; before it has been.
  (define cut-digit-limit 18)

  (define (writer-char? c)
    (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9)
        (char=? c #\-) (char=? c #\_)))

  (define (skip-blank text i)
    (let loop ((i i))
      (if (and (< i (string-length text)) (whitespace? (string-ref text i)))
          (loop (+ i 1))
          i)))

  (define (expect text i ch)
    (let ((i (skip-blank text i)))
      (and (< i (string-length text)) (char=? (string-ref text i) ch) (+ i 1))))

  (define (parse-writer text i)
    (let ((i (skip-blank text i)))
      (and (< i (string-length text))
           (char=? (string-ref text i) #\")
           (let loop ((j (+ i 1)) (out (quote ())))
             (cond
               ((>= j (string-length text)) #f)
               ((char=? (string-ref text j) #\")
                (and (pair? out)
                     (cons (list->string (reverse out)) (+ j 1))))
               ((writer-char? (string-ref text j))
                (loop (+ j 1) (cons (string-ref text j) out)))
               (else #f))))))

  (define (parse-seq text i)
    (let ((i (skip-blank text i)))
      (let loop ((j i) (n 0))
        (cond
          ((and (< j (string-length text)) (char<=? #\0 (string-ref text j) #\9))
           (if (> n cut-digit-limit) #f (loop (+ j 1) (+ n 1))))
          ((= n 0) #f)
          (else (cons (string->number (substring text i j) 10) j))))))

  ;; ONE ENTRY AT A TIME, so the shape is obvious rather than counted:
  ;; ("writer" . 12) with every piece checked as it is taken.
  (define (parse-entry text i)
    (let* ((open (expect text i #\())
           (w (and open (parse-writer text open)))
           (dot (and w (expect text (cdr w) #\.)))
           (sq (and dot (parse-seq text dot)))
           (shut (and sq (expect text (cdr sq) #\)))))
      (and shut (cons (cons (car w) (car sq)) shut))))

  ;; (("writer" . 12) ("other" . 3))
  (define (parse-cut text)
    (let ((i (expect text 0 #\()))
      (and i
           (let loop ((i i) (out (quote ())))
             (let ((close (expect text i #\))))
               (if close
                   (and (= (skip-blank text close) (string-length text))
                        (reverse out))
                   (let ((e (parse-entry text i)))
                     (and e (loop (cdr e) (cons (car e) out))))))))))

  (define (store-tags store)
    (let ((state (open-and-reduce store)))
      (map (lambda (t)
             (let ((name (car t)) (candidates (cadr t)))
               (if (= 1 (length candidates))
                   (list (quote settled) name (car (car candidates)))
                   (list (quote unsettled) name candidates))))
           (state-tags state))))


  ;; ---- log ------------------------------------------------------------------

  ;; WHAT THIS STORE HAS APPLIED, IN THE ORDER IT WAS DELIVERED.
  ;;
  ;; THIS ONE REPLAYS EVERYTHING, and it is the only read here that does.
  ;; Two facts force it. The delivery callback is the only place `ts` and
  ;; `actor` exist -- the reducer never sees them, because nothing it
  ;; decides depends on them -- and a read seeded from the snapshot never
  ;; replays the records the snapshot covers at all. Seeding would give a
  ;; log that silently began in the middle, which is the failure this
  ;; store treats as the worst kind: an answer that looks complete. The
  ;; cost is real and belongs in the verb's documentation, not in a
  ;; surprise.
  ;;
  ;; ONLY WHAT WAS APPLIED IS LISTED. A record can arrive, wait for its
  ;; premises and be applied later, so the question is not what the
  ;; delivery said at the time but what the finished reduction holds --
  ;; while the ORDER is the order of delivery, which is what `outline`
  ;; and every other reader also walked.
  (define (collect-into r collected)
    (let ((inner (deliver-into r #f)))
      (lambda (writer seg off seq ts actor deps payload)
        (let ((answer (inner writer seg off seq ts actor deps payload)))
          (vector-set! collected 0
                       (cons (list writer seq ts actor payload)
                             (vector-ref collected 0)))
          answer))))

  (define (entry-writer e) (car e))
  (define (entry-seq e) (cadr e))
  (define (entry-ts e) (caddr e))
  (define (entry-actor e) (cadddr e))
  (define (entry-payload e) (car (cddddr e)))

  (define (entry-verb e)
    (let ((payload (entry-payload e)))
      (if (pair? payload) (car payload) (quote unknown))))

  ;; WHICH BLOCK A RECORD TOUCHED, by what the intent says rather than by
  ;; where the id happens to appear. A record whose deps name a block has
  ;; not touched it, and neither has one whose text merely mentions it:
  ;; both would make `log <id>` list records that never changed it.
  (define (touches? e id)
    (let* ((payload (entry-payload e))
           (verb (entry-verb e))
           (args (if (pair? payload) (cdr payload) (quote ()))))
      (case verb
        ((put) (equal? id (block-id (entry-writer e) (entry-seq e))))
        ((set move) (and (pair? args) (equal? id (car args))))
        ((del) (and (pair? args) (equal? id (car args))))
        ((link unlink)
         (and (pair? args) (pair? (cdr args)) (pair? (cddr args))
              (or (equal? id (car args)) (equal? id (caddr args)))))
        (else #f))))

  (define (store-log store id)
    (let* ((r (reduce-empty))
           (collected (vector (quote ())))
           (ls (log-open store)))
      (load-deliver! ls (quote ()) (collect-into r collected))
      (load-commit! ls)
      (let ((applied (reduce-trace r)))
        (if (and id (not (known? r id)))
            (list (quote error) (quote unknown-id) id
                  (list (quote nearest) (nearest-ids r id)))
            (cons (quote ok)
                  (list
                    (let loop ((es (reverse (vector-ref collected 0))) (out (quote ())))
                      (cond
                        ((null? es) (reverse out))
                        ((and (member (cons (entry-writer (car es)) (entry-seq (car es)))
                                      applied)
                              (or (not id) (touches? (car es) id)))
                         (loop (cdr es) (cons (car es) out)))
                        (else (loop (cdr es) out))))))))))



  ;; ---- refs -----------------------------------------------------------------

  ;; WHAT REFERS TO A BLOCK, from both places a reference can live.
  ;;
  ;; A LINK RECORD IS AN EDGE SOMEONE WROTE, and `unlink` removes it. A
  ;; REFERENCE IN THE TEXT IS A SENTENCE, and nothing removes it but
  ;; editing the sentence -- so the two are reported with the source they
  ;; came from rather than merged. Deriving the text ones at read time is
  ;; deliberate: materialising them as link records at import would give
  ;; one fact two suppliers, and `unlink` could then delete the edge
  ;; while the sentence still said it. You can unlink an edge; you cannot
  ;; unlink a sentence.
  ;;
  ;; OUT-EDGES ARE NOT REFERENCES TO THIS BLOCK. state-refs filters on
  ;; the far end for exactly that reason.
  (define (md-referrers state id)
    (let loop ((ds (state-datum state)) (out (quote ())))
      (if (null? ds)
          out
          (let* ((from (cadr (car ds)))
                 (block (state-read state from))
                 (srcs (field-strings block (quote src)))
                 (keys (apply append (map (lambda (text) (map caddr (md-refs text))) srcs))))
            (loop (cdr ds)
                  (if (and (not (equal? from id)) (member id keys))
                      (cons from out)
                      out))))))

  (define (store-refs store id)
    (let ((state (open-and-reduce store)))
      (if (not (known? state id))
          (list (quote error) (quote unknown-id) id
                (list (quote nearest) (nearest-ids state id)))
          (let ((linked (map (lambda (p) (list (car p) (cdr p) (quote link)))
                             (state-refs state id)))
                (derived (map (lambda (from) (list from (quote ref) (quote md)))
                              (md-referrers state id))))
            (cons (quote ok)
                  (list (list-sort
                          (lambda (a b)
                            (if (string=? (car a) (car b))
                                (string<? (symbol->string (cadr a)) (symbol->string (cadr b)))
                                (string<? (car a) (car b))))
                          (append linked derived))))))))



  (define (deleted? state id)
    (let ((b (state-read state id)))
      (and b (cdr (assq 'deleted b)))))

  ;; THE DEPS ARE THE APPLIED CUT WITHOUT THIS WRITER. The cut is
  ;; causally closed by construction -- it is what the reduction has
  ;; applied -- and a writer's own previous event is a premise whether it
  ;; is named or not, so naming it would be the one entry that says
  ;; nothing. With only a local writer this is the empty list; with a
  ;; mirrored writer it carries that writer's last applied sequence.
  ;; A TAG'S PREMISES ARE WHAT IT BINDS. Every other record names the
  ;; applied cut without its own writer, because a writer's own previous
  ;; event is a premise whether it is named or not. A tag is the
  ;; exception on purpose: the cut is not context for this record, it is
  ;; the record's content, and a reader trusting the name would otherwise
  ;; be trusting a list the log never required to be present.
  (define (deps-for-payload state writer payload)
    (if (and (pair? payload) (eq? (car payload) 'tag))
        (caddr payload)
        (deps-for state writer)))

  (define (deps-for state writer)
    (list-sort (lambda (a b) (string<? (car a) (car b)))
               (remp (lambda (e) (string=? (car e) writer))
                     (reduce-applied-cut state))))

  (define (siblings state parent)
    (list-sort (lambda (a b) (< (car a) (car b)))
               (map (lambda (row) (cons (cadr row) (caddr row)))
                    (filter (lambda (row) (equal? (car row) parent))
                            (state-outline state)))))

  ;; PLACING IS ONE RULE WITH TWO CALLERS. insert and move both have to
  ;; answer "what ord goes between these siblings", and writing it twice
  ;; is how the two come to disagree about the end of the list.
  (define (ord-for state parent after)
    (let ((sibs (siblings state parent)))
      (cond
        ((not after)
         (ord-between (if (null? sibs) #f (car (car (reverse sibs)))) #f))
        (else
         (let loop ((xs sibs))
           (cond
             ((null? xs) (list 'error 'unknown-sibling after))
             ((equal? (cdr (car xs)) after)
              (ord-between (car (car xs))
                           (if (null? (cdr xs)) #f (car (car (cdr xs))))))
             (else (loop (cdr xs)))))))))

  (define (unwrap intent)
    (if (eq? (car intent) 'expect) (caddr intent) intent))

  (define (expectation intent)
    (and (eq? (car intent) 'expect) (cadr intent)))

  ;; THE BLOCK AN INTENT IS ABOUT, or #f when it makes a new one.
  (define (subject intent)
    (let ((i (unwrap intent)))
      (case (car i)
        ((insert) #f)
        ((set del move) (cadr i))
        ((link unlink) (cadr i))
        (else #f))))

  (define (check-expectation state intent)
    (let ((want (expectation intent))
          (id (subject intent)))
      (cond
        ((not want) #f)
        ((not id) (list 'error 'no-subject))
        ((not (known? state id)) (list 'error 'unknown-id id (list 'nearest (nearest-ids state id))))
        ((deleted? state id) (list 'error 'deleted id))
        ((not (equal? want (block-hash state id)))
         (list 'error 'changed (list 'current (block-hash state id))))
        (else #f))))

  ;; RESOLUTION ANSWERS EITHER A PAYLOAD OR AN ERROR, and it does the
  ;; whole check before the sequence number is reserved: a refusal after
  ;; a reservation leaves a hole in the log that the next open has to
  ;; explain away.
  (define (resolve state writer seq intent)
    (define (missing id)
      (list 'error 'unknown-id id (list 'nearest (nearest-ids state id))))
    (let ((i (unwrap intent)))
      (case (car i)
        ((insert)
         (let ((parent (cadr i)) (after (caddr i)) (fields (cadddr i)))
           (cond
             ((and (not (eq? parent 'root)) (not (known? state parent))) (missing parent))
             ((and (not (eq? parent 'root)) (deleted? state parent))
              (list 'error 'deleted parent))
             (else
              (let ((ord (ord-for state parent after)))
                (if (and (pair? ord) (memq (car ord) '(error refused)))
                    (if (eq? (car ord) 'error) ord (cons 'error (cdr ord)))
                    (list 'put (append fields (list (cons 'parent parent)
                                                    (cons 'ord ord))))))))))
        ;; A TAG BINDS THE CUT THIS STORE HAS APPLIED, which is not the
        ;; frontier it has discovered: a name that pointed at records
        ;; the reducer had not applied would name a state no reader of
        ;; this store could produce. The cut is taken here, before the
        ;; record exists, so a tag is never inside the cut it binds.
        ((tag)
         (list 'tag (cadr i) (reduce-applied-cut state)))
        ((set)
         (let ((id (cadr i)))
           (cond
             ((not (known? state id)) (missing id))
             ((null? (cdddr i)) (list 'set id (caddr i)))
             (else (list 'set id (caddr i) (cadddr i))))))
        ((del)
         (if (known? state (cadr i)) (list 'del (cadr i)) (missing (cadr i))))
        ((move)
         (let ((id (cadr i)) (parent (caddr i)) (after (cadddr i)))
           (cond
             ((not (known? state id)) (missing id))
             ((and (not (eq? parent 'root)) (not (known? state parent))) (missing parent))
             (else
              (let ((ord (ord-for state parent after)))
                (if (and (pair? ord) (memq (car ord) '(error refused)))
                    (if (eq? (car ord) 'error) ord (cons 'error (cdr ord)))
                    (list 'move id parent ord)))))))
        ((link unlink)
         (let ((from (cadr i)) (to (cadddr i)))
           (cond
             ((not (known? state from)) (missing from))
             ;; A LINK TO A TOMBSTONED BLOCK IS ALLOWED and comes out
             ;; dangling; only an id nobody has ever seen is an error.
             ((not (known? state to)) (missing to))
             (else (list (car i) from (caddr i) to)))))
        (else (list 'error 'unknown-verb (car i))))))

  ;; A REFUSAL AN OPERATOR CANNOT ACT ON IS HALF AN ANSWER. These are
  ;; the reasons where there is one thing to do about it, and saying so
  ;; is the difference between "this failed" and "run adopt".
  ;; THE TABLE IS HERE BECAUSE THE ANSWER IS THIS LAYER'S CONTRACT. The
  ;; log reports what it found; what a caller should do about it is part
  ;; of the shape this library promises.
  (define (remedy-for why)
    (case why
      ((integrity registry-ahead missing-generation) 'adopt)
      ((registry-inside-store) 'move-the-registry-outside-the-store)
      (else #f)))

  (define (block-ids-of payload writer seq)
    (case (car payload)
      ((put) (list (block-id writer seq)))
      ((set del move) (list (cadr payload)))
      ((link unlink) (list (cadr payload) (cadddr payload)))
      (else '())))

  (define (state-report state ids)
    (map (lambda (id) (cons id (block-hash state id)))
         (list-sort string<? (let dedupe ((xs ids) (out '()))
                               (cond ((null? xs) (reverse out))
                                     ((member (car xs) out) (dedupe (cdr xs) out))
                                     (else (dedupe (cdr xs) (cons (car xs) out))))))))

;; THE ACTOR COMES FROM THE CALLER. A published library must not put
  ;; anybody's name in its default: whoever ran the command is a fact
  ;; only the caller has, and a constant baked in here would appear in
  ;; every record every user ever writes. "unknown" is what a caller who
  ;; declines to say gets -- it is not a person, and it is visibly not
  ;; one.
  (define (with-store-write store proc . rest)
    (let ((actor (if (null? rest) "unknown" (car rest)))
          (state (reduce-empty)))
      (let ((s (log-begin store (deliver-into state #f))))
        ;; THE FRONTIER IS REPORTED ONCE DELIVERY IS OVER, not inferred
        ;; from the per-record answers. A record is answered as it
        ;; arrives, and a record whose premise has not arrived yet is
        ;; answered `pending` -- truthfully. But applying a later record
        ;; can drain it, and nothing goes back to revise the earlier
        ;; answer. Without this line the session's applied cursor stops
        ;; at the first such record forever, `predecessor-applied?` stays
        ;; false, and `session-view` hands back #f: the store cannot be
        ;; written to at all. It bites exactly when the local writer's
        ;; own record declares a dep on a writer the load delivers
        ;; AFTER it, which is decided by nothing more than the two
        ;; writers' names.
        (session-applied! s (session-epoch s) (reduce-applied-cut state))
        (let ((answers
                (guard (e (#t (log-end! s) (raise e)))
                  (run-intents! s state actor (proc state (session-view s))))))
          (log-end! s)
          answers))))

;; A BATCH CAN BUILD A TREE, so an intent must be able to name a block
  ;; an earlier intent in the same batch created. The id of a new block
  ;; is derived from the sequence number it is written at, which nobody
  ;; knows until it commits -- so the caller writes `(from <n>)` for the
  ;; nth intent of this batch and the resolution happens here, where the
  ;; answers are.
  ;; THE ALTERNATIVE WAS TWO CALLS, one to make the parents and one to
  ;; make the children, which would take the lock twice and leave a
  ;; state on disk between them that no single import ever intended.
  (define (resolve-from made x)
    (if (and (pair? x) (eq? (car x) 'from))
        (let ((e (assv (cadr x) made)))
          (if e (cdr e) (list 'error 'no-such-intent (cadr x))))
        x))

;; BOTH THE PARENT AND THE SIBLING CAN BE A BACK-REFERENCE. A batch that
  ;; builds a tree also orders it: the section a new one follows may
  ;; itself have been created two intents ago, and `after` is how the
  ;; caller says so. Resolving only the parent put every new section at
  ;; the end of its parent's children whatever the file said.
  (define (intent-refs intent)
    (let ((i (unwrap intent)))
      (case (car i)
        ((insert) (list (cadr i) (caddr i)))
        ((move) (list (caddr i) (cadddr i)))
        (else '()))))

  (define (intent-with-refs intent parent after)
    (let ((i (unwrap intent)))
      (case (car i)
        ((insert) (list 'insert parent after (cadddr i)))
        ((move) (list 'move (cadr i) parent after))
        (else i))))

  (define (run-intents! s state actor intents)
    (let loop ((is intents) (n 0) (made '()) (out '()))
      (if (null? is)
          (reverse out)
          (let* ((raw (car is))
                 (rs (intent-refs raw))
                 (fixed
                   (if (null? rs)
                       raw
                       (let ((p (resolve-from made (car rs)))
                             (a (resolve-from made (cadr rs))))
                         (cond
                           ((and (pair? p) (eq? (car p) 'error)) p)
                           ((and (pair? a) (eq? (car a) 'error)) a)
                           (else (intent-with-refs raw p a))))))
                 (answer (if (and (pair? fixed) (eq? (car fixed) 'error))
                             fixed
                             (one-intent! s state actor fixed))))
            ;; A FAILED INTENT STOPS THE REST. Later intents were written
            ;; against a state this one was meant to produce; running them
            ;; anyway asks each to be judged against a history its author
            ;; did not have.
            (if (eq? (car answer) 'error)
                (reverse (cons answer out))
                (loop (cdr is) (+ n 1)
                      (let ((ids (cadr (assq 'state (cdr answer)))))
                        (if (and (eq? 'insert (car (unwrap fixed))) (pair? ids))
                            (cons (cons n (car (car ids))) made)
                            made))
                      (cons answer out)))))))

  (define (one-intent! s state actor intent)
    (let ((v (session-view s)))
      (if (not v)
          ;; WHY THERE IS NO VIEW IS PART OF THE ANSWER. "No view" alone
          ;; sends a caller to look for a bug in its own sequencing when
          ;; the store is telling it something about its history.
          (let ((why (session-view-refusal s)))
            (if why (list 'error 'no-view why) (list 'error 'no-view)))
          (let ((writer (view-writer v))
                (seq (view-expect-seq v)))
            (let ((bad (check-expectation state intent)))
              (if bad
                  bad
                  (let ((payload (resolve state writer seq intent)))
                    (if (eq? (car payload) 'error)
                        payload
                        (let* ((deps (deps-for-payload state writer payload))
                               (frame (make-frame (view-revision v) (view-epoch v)
                                                  writer seq actor deps payload))
                               (outcome (session-append! s frame)))
                          ;; NOT EVERY FAILURE MEANS NOTHING HAPPENED.
                          ;; `refused-before-reserve` is the only outcome
                          ;; that says the log is untouched; the record
                          ;; does not exist and the answers given so far
                          ;; are the exact committed prefix.
                          ;; `written-fsync-failed` and `partial-write`
                          ;; leave bytes that a later replay may well
                          ;; read back as a committed record. Answering
                          ;; those as a plain error claims a prefix that
                          ;; replay then contradicts -- the caller is
                          ;; told intent k failed and finds it applied.
                          ;; They get their own answer, and it says the
                          ;; outcome is not known rather than known to
                          ;; be nothing.
                          (if (not (eq? (car outcome) 'committed))
                              (if (eq? (car outcome) 'refused-before-reserve)
                                  (let ((why (if (pair? (cdr outcome)) (cadr outcome) '())))
                                    (append (list 'error 'refused why)
                                            (let ((r (remedy-for why)))
                                              (if r (list (list 'remedy r)) '()))))
                                  (list 'error 'indeterminate (car outcome)
                                        (list 'sequence seq)))
                              (begin
                                (reduce-apply! state writer seq deps payload)
                                (session-applied! s (session-epoch s)
                                                  (reduce-applied-cut state))
                                (list 'ok
                                      (list 'events (list (cons writer seq)))
                                      (list 'state (state-report
                                                     state (block-ids-of payload writer seq)))
                                      (list 'cursor (cons writer seq))
                                      (list 'replay #f)))))))))))))

  ;; ---- init ----------------------------------------------------------------

  ;; A NAME NOBODY ELSE WILL PICK, derived rather than drawn: the store's
  ;; own path, the process and the millisecond go through sha256 and the
  ;; first eight base-36 digits come out. THE PATH IS IN THE SEED because
  ;; without it one process initialising two stores inside the same
  ;; millisecond gives both the same writer name; with it, colliding
  ;; needs the same process to initialise the same path twice in the same
  ;; millisecond, which the already-initialised check refuses first.
  (define (derive-id salt store)
    (let* ((seed (string-append salt "|" store
                                "|" (number->string (process-id))
                                "|" (number->string (wall-clock-ms))))
           (hex (bytevector->hex (sha256 (string->utf8 seed))))
           (digits "0123456789abcdefghijklmnopqrstuvwxyz"))
      (let loop ((i 0) (acc 0))
        (if (= i 12)
            (let build ((n 8) (v acc) (out '()))
              (if (= n 0)
                  (list->string out)
                  (build (- n 1) (div v 36)
                         (cons (string-ref digits (mod v 36)) out))))
            (loop (+ i 1)
                  (+ (* acc 16)
                     (let ((c (string-ref hex i)))
                       (if (char<=? #\0 c #\9)
                           (- (char->integer c) (char->integer #\0))
                           (+ 10 (- (char->integer c) (char->integer #\a)))))))))))

  ;; THE FOREIGN-WRITER CASE IS NOT "ALREADY INITIALISED". A directory
  ;; holding a writer whose owner names another machine's instance is a
  ;; store that travelled -- copied, restored, or mounted from elsewhere.
  ;; Minting a second writer beside it would make two writers believe
  ;; they own the same history, so the answer names adopt instead.
  (define (foreign-writer store)
    (let loop ((ws (store-writers store)))
      (cond
        ((null? ws) #f)
        ((let* ((path (string-append (writer-directory store (car ws)) "/owner.sexp")))
           (and (file-exists? path) (car ws)))
         (car ws))
        (else (loop (cdr ws))))))

  (define (store-init! store)
    (cond
      ((file-exists? (string-append store "/meta.sexp"))
       (list 'error 'already-initialised (list 'store store)))
      ((foreign-writer store)
       => (lambda (w)
            (list 'error 'foreign-writer (list 'writer w) (list 'remedy 'adopt))))
      (else
       (let ((sid (derive-id "store" store))
             (writer (derive-id "writer" store)))
         (mkdir-p! (string-append store "/writers/" writer))
         (mkdir-p! (string-append store "/snap"))
         (mkdir-p! (string-append store "/blobs"))
         ;; THE LOCK FILE IS NEVER DELETED AND NEVER REPLACED, so it is
         ;; created directly rather than written atomically: an atomic
         ;; write renames a new inode over the name, and every process
         ;; already holding the old one would be locking a file nobody
         ;; else can see.
         (call-with-port (open-file-output-port (string-append store "/lock")
                                                (file-options no-fail))
           (lambda (p) (if #f #f)))
         (atomic-write! (string-append store "/meta.sexp")
                        (string->utf8 (string-append "((format 1) (store-id \"" sid "\"))\n"))
                        'registry)
         (let ((nonce (instance-install! store)))
           (owner-install! store writer nonce))
         (store-register! store)
         ;; THE CURRENT SEGMENT EXISTS AND IS EMPTY. Without a segment
         ;; file the writer has no append target, and the first write
         ;; into a freshly initialised store is refused before it
         ;; reserves -- correct about a store nobody finished making.
         (call-with-port (open-file-output-port
                           (string-append store "/writers/" writer "/" (segment-file-name 1))
                           (file-options no-fail))
           (lambda (p) (if #f #f)))
         (list 'ok (list 'store sid) (list 'writer writer))))))

  ;; ---- snapshots -----------------------------------------------------------

  ;; THE THREE PARTS OF THE ENVELOPE ARE TAKEN TOGETHER, from a state
  ;; nothing has touched since. The cut is the reduction's own applied
  ;; cursor -- causally closed by construction, because it is what the
  ;; reducer actually applied -- and the rows are that same reduction
  ;; serialised. Taking them at two moments is how a snapshot comes to
  ;; describe a state that never existed.
  ;;
  ;; IT IS A WRITE SESSION EVEN THOUGH IT WRITES NO RECORD. Only the
  ;; holder of the exclusive lock may produce a snapshot: the cut has to
  ;; mean something for the length of time it takes to write it down,
  ;; and a reader's shared lock does not stop anybody appending.
  (define (store-snapshot! store . opts)
    (let ((actor (if (pair? opts) (car opts) "unknown"))
          (state (reduce-empty)))
      (let ((s (log-begin store (deliver-into state #f))))
        (session-applied! s (session-epoch s) (reduce-applied-cut state))
        (let ((answer
                (guard (e (#t (log-end! s) (raise e)))
                  (let ((v (session-view s)))
                    (if (not v)
                        (list 'refused 'no-local-writer)
                        (session-snapshot!
                          s (list v (reduce-applied-cut state) (state->rows state))))))))
          (log-end! s)
          answer))))

  ;; ---- check ---------------------------------------------------------------

  ;; READ-ONLY, AND IT PRINTS WHAT IT FOUND EVEN WHEN THE ANSWER IS BAD.
  ;; A checker that only said "damaged" would leave the caller to go
  ;; looking; the point of running it is to be told which writer, which
  ;; segment, and which snapshot.
  ;;
  ;; SNAPSHOTS ARE REPORTED BESIDE THE WRITERS, NOT INSIDE THEM. They
  ;; live in one directory for the whole store and each one's cut names
  ;; several writers, so filing a snapshot under a writer would either
  ;; duplicate it or pick one of its writers arbitrarily.
  (define (describe-error e)
    (append (list (log-error-kind e))
            (if (log-error-segment e) (list (list 'segment (log-error-segment e))) '())
            (if (log-error-offset e) (list (list 'offset (log-error-offset e))) '())
            (let ((d (log-error-detail e)))
              (if (and (pair? d) (pair? (car d)))
                  (map (lambda (p) (list (car p) (cdr p))) d)
                  '()))))

  (define (store-check store)
    (let* ((ls (log-open store))
           (writers (load-writers ls))
           (prefixes (map (lambda (w) (cons w (load-prefix ls w))) writers))
           (coverage (map (lambda (e)
                            (cons (car e) (if (cdr e) (discovery-end-seq (cdr e)) 0)))
                          prefixes))
           (per-writer
             (map (lambda (e)
                    (let ((p (cdr e)))
                      (list (car e)
                            (list 'end (if p (discovery-end-seq p) 0))
                            (list 'torn (and p (discovery-torn p) #t))
                            (list 'integrity
                                  (if p (map describe-error (discovery-integrity p)) '())))))
                  prefixes))
           (snapshots (check-snapshots store coverage))
           (damaged? (exists (lambda (w) (pair? (cadr (assq 'integrity (cdr w)))))
                             per-writer)))
      (load-commit! ls)
      (list 'check
            (list 'store (or (store-id-of store) 'unknown))
            (list 'writers per-writer)
            (list 'snapshots snapshots)
            ;; REPORTED SEPARATELY FROM WRITER DAMAGE, because it is not
            ;; damage: nothing in the store is wrong. What is wrong is
            ;; where the registry was put, and the store cannot see it
            ;; from the inside -- which is exactly why it is worth
            ;; saying out loud.
            (list 'registry (if (registry-inside-store?) 'inside-store 'outside-store))
            (list 'verdict (if (or damaged? (registry-inside-store?)) 'damaged 'ok)))))

  ;; A SNAPSHOT THAT CANNOT BE USED IS NOT DAMAGE TO THE STORE -- the log
  ;; still loads and the state is still right, it just has to be rebuilt
  ;; from further back. So these are reported and do not decide the
  ;; verdict; the writers' integrity does.
  (define (check-snapshots store coverage)
    (let ((dir (string-append store "/snap")))
      (if (not (file-exists? dir))
          '()
          (let loop ((ns (list-sort < (filter (lambda (n) n)
                                              (map segment-file-number
                                                   (directory-entries dir)))))
                     (out '()))
            (if (null? ns)
                (reverse out)
                (let ((path (string-append dir "/" (segment-file-name (car ns)))))
                  (let-values (((cut rows) (snapshot-read path)))
                    (loop (cdr ns)
                          (cons (list (segment-file-name (car ns))
                                      (cond
                                        ((not cut) (list 'unusable rows))
                                        ((not (snapshot-cut-supported? cut coverage))
                                         (list 'unusable 'unsupported-cut))
                                        (else (list 'usable (list 'cut cut)))))
                                out)))))))))

  ;; The store-level name for the log layer's transaction. It adds
  ;; nothing: adopt is decided and performed entirely from verified
  ;; state, and there is no block-level fact to contribute.
  (define (store-adopt! store) (adopt! store))
)
