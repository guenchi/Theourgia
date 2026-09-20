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
  (export store-resident-cache! open-and-reduce with-store-write store-publish-hook!
          store-init! nearest-ids store-snapshot!
          batch-answer
          store-check store-adopt! store-search store-refs store-log store-tags parse-cut store-diff store-conflicts store-evidence
          make-write-request write-request? store-successors store-intervals
          request-verdict)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs conditions) (rnrs io ports) (rnrs files)
          (only (theourgia md) md-refs)
          (theourgia request)
          (theourgia evidence-index)
          (only (theourgia wire) decode-line storable-decode)
          (rnrs arithmetic fixnums) (rnrs unicode) (rnrs bytevectors) (rnrs hashtables)
          (only (theourgia log)
                log-open load-deliver! load-commit! load-fingerprint
                load-snapshot-cut load-snapshot-rows
                log-begin log-end! session-view session-view-refusal session-append! session-applied!
                session-epoch make-frame atomic-write! segment-file-name
                session-snapshot! log-open load-writers load-prefix load-commit! local-writer-of
                discovery-end-seq discovery-integrity discovery-torn
                enumerate-segment-files discovery-quarantine discover-prefix
                manifest-segments read-manifest
                snapshot-read snapshot-cut-supported? segment-file-number
                log-error-kind log-error-segment log-error-offset log-error-detail
                store-id-of adopt! registry-inside-store?
                instance-install! owner-install! writer-directory store-writers
                store-register!
                uncertain-load run-barrier! retired-successor retired-of
                session-commit! session-pending-count-set! note-written-for!
                session-write-started? session-written-events
                session-writer discovery-physical-current discovery-segment-ranges
                view-revision view-epoch view-writer view-expect-seq)
          (only (theourgia ffi) mkdir-p! wall-clock-ms process-id directory-entries
                file-is-directory? report-fault? trace-event!)
          (only (theourgia digest) sha256 bytevector->hex)
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

  ;; ONE PLACE TURNS A STORED RECORD BACK INTO A PAYLOAD, and both paths
  ;; that read records use it.
  ;;
  ;; THE ENCODING WAS ONE-WAY. `storable-encode` is applied when a record
  ;; is appended; delivery never applied its inverse, so the SAME record
  ;; reduced two ways: written and reduced in memory it was
  ;; `(("#%char" 97))`, and read back from disk it was
  ;; `(("#%quote" ("#%char" 97)))`. Both reduce without complaint, and
  ;; their state hashes differ -- so a successful write returned a hash
  ;; that reopening the store immediately contradicted.
  ;;
  ;; THE EVIDENCE PATH HAD ITS OWN DECODE and this is now that decode:
  ;; the same defect was found and fixed once, in one of the two readers,
  ;; and the other was never searched for. A shared definition is what
  ;; stops the next reader being written without one.
  (define (stored->payload x)
    (guard (e (#t x)) (storable-decode x)))

  ;; A WRITE SESSION'S LOAD CONFIRMS ONLY ITS FINAL CUT. A record this
  ;; pass applied can be taken back by a later one in the same pass -- a
  ;; duplicate arriving after it contests its slot -- so a session that
  ;; answered `applied` as each record went by would be promising about
  ;; a state the rest of the pass may still change.
  (define (deliver-into r cut . defer)
    (lambda (writer seg off seq ts actor deps payload)
      (if (not (within? cut writer seq))
          'skipped
          (let ((answer (reduce-apply! r writer seq deps (stored->payload payload) actor)))
            (cond
              ((eq? answer 'accepted)
               (if (and (null? defer) (applied? r writer seq)) 'applied 'pending))
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
  (define resident-enabled? #f)
  (define residents (make-hashtable string-hash string=?))
  (define (store-resident-cache! enabled?)
    (set! resident-enabled? (and enabled? #t))
    (hashtable-clear! residents))
  (define (complete-frontier? state ls)
    (for-all (lambda (writer)
               (let ((p (assoc writer (reduce-applied-cut state))))
                 (= (if p (cdr p) 0) (discovery-end-seq (load-prefix ls writer)))))
             (load-writers ls)))
  (define (replay store cut)
    (let* ((ls (log-open store))
           (key (and resident-enabled? (not cut) (load-fingerprint ls)))
           (previous (and key (hashtable-ref residents store #f)))
           (cached (and previous (equal? (car previous) key) (cdr previous)))
           (rows (and (not cached) (not cut) (load-snapshot-rows ls)))
           (usable (and rows (assq 'request-history rows) rows))
           (r (or cached (if usable (rows->state usable) (reduce-empty))))
           (from (cond (cached (reduce-applied-cut cached)) (usable (load-snapshot-cut ls)) (else '()))))
      (when resident-enabled?
        (trace-event! (if cached 'resident-hit (if previous 'resident-reset 'resident-load)) store #f))
      (load-deliver! ls from (deliver-into r cut))
      (load-commit! ls)
      (when key
        ;; Pending history cannot use the completed-frontier fast path.
        (if (complete-frontier? r ls) (hashtable-set! residents store (cons key r))
            (hashtable-delete! residents store)))
      r))

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

  ;; ---- normalisation, and what counts as a word boundary -------------------
  ;;
  ;; NEVER: TWO SPELLINGS OF ONE WORD ARE ONE WORD. Measured before this
  ;; existed: a three-character CJK word written against `reaper` with no space
  ;; did not find the same word with a space, because the query was
  ;; split on whitespace and there is none between a CJK character and the
  ;; Latin one beside it. A fullwidth capital A and an ASCII `A` were two
  ;; things, as were a ligature and the letters it stands for.
  ;;
  ;; `prepare` puts text in NFKC and writes a space at every CJK/ASCII
  ;; boundary. IT DOES NOT FOLD CASE, on purpose: matching is
  ;; case-insensitive and folds at comparison time, while the SCORE needs to
  ;; see `parseBlock` as two words, and folding first would erase the only
  ;; evidence of that.
  ;; NOTE: WHAT THIS DOES NOT COVER, SAID OUT LOUD. Han, kana and the CJK
  ;; compatibility block are here; HANGUL (AC00-D7AF) IS NOT. Korean text
  ;; therefore gets no seam spacing and no bigram matching -- it is matched as
  ;; a substring, which finds what it finds and ranks by the Latin rule. That
  ;; is a gap rather than a decision about Korean, and it is written here
  ;; rather than left for a reader to infer from a range.
  (define (cjk-char? ch)
    (let ((c (char->integer ch)))
      (or (and (>= c #x3400) (<= c #x4DBF))
          (and (>= c #x4E00) (<= c #x9FFF))
          (and (>= c #xF900) (<= c #xFAFF))
          (and (>= c #x3040) (<= c #x30FF)))))

  (define (ascii-word-char? ch)
    (or (char-numeric? ch)
        (and (char-alphabetic? ch) (< (char->integer ch) 128))))

  (define (prepare text)
    (let* ((nf (string-normalize-nfkc text))
           (n (string-length nf)))
      (let loop ((i 0) (out (quote ())))
        (if (= i n)
            (list->string (reverse out))
            (let ((ch (string-ref nf i)))
              (loop (+ i 1)
                    (cond
                      ((= i 0) (cons ch out))
                      ((or (and (cjk-char? ch) (ascii-word-char? (string-ref nf (- i 1))))
                           (and (ascii-word-char? ch) (cjk-char? (string-ref nf (- i 1)))))
                       (cons ch (cons #\space out)))
                      (else (cons ch out)))))))))

  ;; A hit begins a word when nothing precedes it, when what precedes it is
  ;; not a word character -- a space, a `-`, a `_`, punctuation -- or when it
  ;; is the upper-case letter that starts the second half of `camelCase`.
  (define (boundary-at? text i token)
    (or (= i 0)
        (let ((before (string-ref text (- i 1)))
              (here (string-ref text i)))
          (or (not (ascii-word-char? before))
              (and (char-alphabetic? before) (char-lower-case? before)
                   (char-alphabetic? here) (char-upper-case? here))))))

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

  ;; ---- what a token hits, and how well ------------------------------------
  ;;
  ;; A token hits a text when the text contains it, case folded -- THE SAME
  ;; RULE AS BEFORE, so nothing that could be found yesterday is lost today.
  ;; What is new is the TIER: a hit that begins a word is worth more than one
  ;; buried inside another word, because `cat` in `Concatenate` is a weaker
  ;; answer than `cat` in `cat and dog` and both are answers.
  ;;
  ;; A CJK token of two characters or more is matched by its bigrams, all of
  ;; them: a three-character word asks for its first two characters and its
  ;; last two. That is WIDER than a substring
  ;; and so cannot lose a hit either. A single character has no bigram and
  ;; falls back to the substring rule.
  ;; NEVER: A FOLD THAT CHANGES THE LENGTH MOVES EVERY POSITION AFTER IT.
  ;; `string-foldcase` is not length-preserving -- German sharp s folds to two
  ;; letters -- and the match position found in the folded text was being used
  ;; to index the UNFOLDED one, where it no longer means the same place.
  ;; Measured: a title reading `Stra<sharp-s>e cat` puts `cat` at folded index
  ;; 8, where the unfolded text holds the `a` of `cat` instead of its start, so
  ;; the boundary was judged on the wrong characters and the hit scored a tier
  ;; too low. With four sharp s in front of the word the folded index runs off
  ;; the end of the unfolded string entirely and `string-ref` RAISES -- one
  ;; search, no answer.
  ;;
  ;; Folding character by character keeps every index meaning the same place.
  ;; The cost is that a sharp s no longer matches the two letters it folds to,
  ;; which is a narrower case-insensitivity than R6RS gives; correct positions
  ;; are worth more than that pair.
  (define (fold-preserving s)
    (list->string (map char-foldcase (string->list s))))

  (define (token-tier text token)
    (let* ((t (prepare text))
           (folded (fold-preserving t))
           (q (fold-preserving (prepare token)))
           (n (string-length folded))
           (m (string-length q)))
      (cond
        ((= m 0) #f)
        ;; NEVER: AND THE WHOLE PHRASE OUTRANKS ITS PIECES. Measured on the
        ;; first version of this: a two-character query scored 2 on a block
        ;; whose title held the three-character word, while a ONE-character
        ;; query scored 3 on that same block -- the more specific query ranked
        ;; LOWER, because bigrams
        ;; always answered `inside` and a lone character fell through to the
        ;; ASCII rule and found itself at what that rule calls a word
        ;; boundary. CJK has no word boundary to find; what it has is the
        ;; difference between a phrase that is there and one whose halves
        ;; merely both occur, which is what the two tiers say here.
        ((for-all cjk-char? (string->list q))
         (cond
           ((substring-at? folded q) 'boundary)
           ((< m 2) #f)
           (else
             (let loop ((i 0))
               (cond ((> (+ i 2) m) 'inside)
                     ((substring-at? folded (substring q i (+ i 2))) (loop (+ i 1)))
                     (else #f))))))
        (else
          (let loop ((i 0) (best #f))
            (cond
              ((> (+ i m) n)
               ;; NEVER: NORMALISING CAN TAKE A MATCH AWAY, AND THE FLOOR SAYS
               ;; IT MAY NOT. NFKC COMPOSES, so a title written as `e` plus a
               ;; combining acute becomes one character and no longer contains
               ;; the letter `e` at all: measured, that block scored 2 for the
               ;; query `e` before this segment and nothing after it. When the
               ;; prepared text has no match, the raw text is asked -- at the
               ;; LOWER tier, which is exactly the score that field had before
               ;; tiers existed, so the floor is kept and nothing outranks a
               ;; hit that the normalised text can actually see.
               (or best (and (substring-at? (fold-preserving text)
                                            (fold-preserving token))
                             'inside)))
              ((string=? (substring folded i (+ i m)) q)
               (if (boundary-at? t i q) 'boundary (loop (+ i 1) 'inside)))
              (else (loop (+ i 1) best))))))))

  (define (substring-at? text needle)
    (let ((n (string-length text)) (m (string-length needle)))
      (and (<= m n)
           (let loop ((i 0))
             (cond ((> (+ i m) n) #f)
                   ((string=? (substring text i (+ i m)) needle) #t)
                   (else (loop (+ i 1))))))))

  ;; The best tier this token reaches anywhere in a field's strings.
  (define (field-tier strings token)
    (let loop ((l strings) (best #f))
      (cond ((null? l) best)
            (else
              (let ((tier (token-tier (car l) token)))
                (cond ((eq? tier 'boundary) 'boundary)
                      ((eq? tier 'inside) (loop (cdr l) 'inside))
                      (else (loop (cdr l) best))))))))

  ;; NEVER: THE TWO TIERS OF A FIELD STRADDLE THE OLD SINGLE VALUE. The lower
  ;; tier is exactly what that field scored before, so a block whose hits are
  ;; all mid-word keeps the score it had; only a block that begins a word
  ;; moves up. A round of scoring that lowered anything would have made
  ;; yesterday's answers worse, which is not what this is for.
  (define (tier-score field tier)
    (case field
      ((keywords) (case tier ((boundary) 4) ((inside) 3) (else 0)))
      ((title) (case tier ((boundary) 3) ((inside) 2) (else 0)))
      ((src) (case tier ((boundary) 2) ((inside) 1) (else 0)))
      (else 0)))

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
  ;; NOTE: KEYWORDS COME FIRST IN THE LINES SEARCHED, so a block matched on
  ;; its keywords shows them. A snippet drawn from the prose for a
  ;; keyword hit would show a line that does not contain the word the
  ;; caller searched for, which reads as a wrong result.
  (define (snippet-for block tokens)
    (let* ((titles (field-strings block (quote title)))
           (srcs (field-strings block (quote src)))
           (kws (field-strings block (quote keywords)))
           (lines (append kws titles
                          (apply append (map lines-of-text srcs)))))
      ;; NEVER: THE SNIPPET ASKS THE SAME QUESTION THE SCORE DID. This looked
      ;; for the token as a raw substring while the score had already found it
      ;; through normalisation, so a block could be reported as a hit and come
      ;; back with an EMPTY snippet: measured, `\x6062;\x590d;\x65e7;reaper`
      ;; scored 3 against a title reading `\x6062;\x590d;\x65e7; reaper
      ;; \x7684;\x505a;\x6cd5;` and showed the reader nothing, and a query
      ;; matched by its bigrams did the same. Two rules for one question is
      ;; how the answer comes to disagree with itself.
      ;; NEVER: AND THE UNIT THE SNIPPET SEARCHES IS THE UNIT THE SCORE
      ;; SEARCHED. The score reads a field whole; this reads it line by line,
      ;; so a query whose bigrams sit on two different lines scored a hit and
      ;; came back with nothing to show -- the same disagreement as before,
      ;; one level down. A line is still preferred, because a line is what a
      ;; reader wants; the whole field is the fallback rather than a blank.
      (let loop ((ls lines))
        (cond
          ((null? ls)
           (let whole ((fs (append kws titles srcs)))
             (cond ((null? fs) "")
                   ((exists (lambda (tk) (token-tier (car fs) tk)) tokens)
                    (clip (collapse-whitespace (car fs))))
                   (else (whole (cdr fs))))))
          ((exists (lambda (tk) (token-tier (car ls) tk)) tokens)
           (clip (collapse-whitespace (car ls))))
          (else (loop (cdr ls)))))))

  (define (store-search store query)
    (let* ((state (open-and-reduce store))
           ;; NEVER: THE QUERY IS PREPARED BEFORE IT IS SPLIT, NOT AFTER.
           ;; `tokens-of` cuts on whitespace, and `prepare` WRITES whitespace
           ;; at a CJK/Latin seam -- so splitting first left the seam inside a
           ;; single token, which then had to be found as one contiguous run.
           ;; Measured: against a title reading "<three CJK characters> then
           ;; reaper" the compact query found nothing while the spaced one
           ;; found the block. The equivalence this segment promises held only
           ;; when the two words happened to be adjacent in the text as well.
           (tokens (tokens-of (prepare query))))
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
                               ;; KEY: KEYWORDS SCORE 3, ABOVE TITLE'S 2 AND
                               ;; SOURCE'S 1. They are the one field a
                               ;; writer chose FOR being found by, so a
                               ;; block whose keywords match is a better
                               ;; answer than one whose prose happens to.
                               (kws (field-strings block (quote keywords)))
                               ;; The tier a field reaches is the best any
                               ;; token reaches in it.
                               (best (lambda (strings)
                                       (let loop ((l tokens) (out #f))
                                         (cond ((null? l) out)
                                               (else
                                                 (let ((tier (field-tier strings (car l))))
                                                   (cond ((eq? tier 'boundary) 'boundary)
                                                         ((eq? tier 'inside) (loop (cdr l) 'inside))
                                                         (else (loop (cdr l) out)))))))))
                               (title-tier (best titles))
                               (src-tier (best srcs))
                               (kw-tier (best kws))
                               ;; NEVER: EVERY TOKEN STILL HAS TO HIT SOMEWHERE.
                               ;; Keywords widen where a token may be
                               ;; found; they do not turn the query into
                               ;; an OR across tokens.
                               (every-token
                                 (for-all (lambda (tk)
                                            (or (field-tier titles tk) (field-tier srcs tk)
                                                (field-tier kws tk)))
                                          tokens)))
                          (loop (cdr ds)
                                (if every-token
                                    (cons (list id
                                                (+ (tier-score 'title title-tier)
                                                   (tier-score 'src src-tier)
                                                   (tier-score 'keywords kw-tier))
                                                (snippet-for block tokens))
                                          out)
                                    out)))))))
            (list-sort (lambda (a b)
                         (if (= (cadr a) (cadr b))
                             (string<? (car a) (car b))
                             (> (cadr a) (cadr b))))
                       hits)))))

  (define (known? state id) (and (state-read state id) #t))
  ;; ---- evidence for a request ----------------------------------------------

  ;; EVERY READABLE RECORD THAT NAMES THE IDENTITY, wherever it is.
  ;;
  ;; The question a request asks is "did this run", and a record that ran
  ;; is evidence whether or not this store can currently deliver it: in
  ;; the valid history, waiting in quarantine, on a torn tail, copied
  ;; aside into damaged/, still sitting in incoming/, or in a segment the
  ;; manifest does not list. Looking only where delivery looks would
  ;; answer "no" for a request whose record is lying in plain sight a
  ;; directory away -- and "no" means run it again.
  ;;
  ;; WHERE IT WAS FOUND TRAVELS WITH IT, because two of the three
  ;; evidence sets are defined by where, and because `unknown` has to be
  ;; able to say which place it could not verify.
  ;;
  ;; THE CHAIN, NOT THE WRITER. After an adopt the records are carried by
  ;; a successor, and the identity in them still names the origin -- so
  ;; the scan follows every writer this store holds and the retirement
  ;; records that link them, rather than asking one writer.
  (define (lines-of-segment bytes)
    (let ((n (bytevector-length bytes)))
      (let loop ((start 0) (out (quote ())))
        (if (>= start n)
            (reverse out)
            (let ((nl (let scan ((i start))
                        (cond ((>= i n) #f)
                              ((= 10 (bytevector-u8-ref bytes i)) i)
                              (else (scan (+ i 1)))))))
              (if (not nl)
                  ;; a final line with no newline is a torn tail
                  (reverse (cons (cons (subbytes bytes start n) (quote torn)) out))
                  (loop (+ nl 1)
                        (cons (cons (subbytes bytes start (+ nl 1)) (quote whole)) out))))))))

  (define (subbytes bv from to)
    (let ((o (make-bytevector (- to from))))
      (bytevector-copy! bv from o 0 (- to from))
      o))

  (define (read-file-bytes path)
    (and (file-exists? path)
         (guard (e (#t #f))
           (call-with-port (open-file-input-port path)
             (lambda (in)
               (let ((b (get-bytevector-all in)))
                 (if (eof-object? b) (make-bytevector 0) b)))))))

  ;; THE DELIVERED CUT IS AN ARGUMENT, NOT SOMETHING THIS COMPUTES. Its
  ;; one caller from outside a session reads the store to get it; its
  ;; caller from INSIDE one already has it, and re-reading the store
  ;; there would take the store's shared lock while the session holds it
  ;; exclusively -- against yourself a lock does not fail, it hangs, and
  ;; a hang is the failure a suite reports worst. It hung the first time
  ;; this was wired up.
  (define (store-evidence store identity)
    (let ((state (open-and-reduce store)))
      (evidence-for store identity (reduce-applied-cut state) (reduce-gates state))))

  (define (evidence-for store identity delivered . marks)
    (map (lambda (rec)
           (make-evidence (car rec) (cadr rec) (caddr rec) (cadddr rec)
                          (list-ref rec 4)
                          (and (eq? (list-ref rec 4) 'valid-history)
                               (let ((have (assoc (caar rec) delivered)))
                                 (and have (<= (cdar rec) (cdr have)))))
                          (let ((m (and (pair? marks) (assoc (car rec) (car marks)))))
                            (if m (list (cdr m)) '()))))
         (indexed-records store identity)))

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
           (nested (cdr (assq (quote nested-documents) structure)))
           (pending (reduce-pending state)))
      (append
        (map (lambda (id) (list (quote conflict) id (quote cycle))) cyclic)
        (map (lambda (id) (list (quote conflict) id (quote unplaced))) unplaced)
        (map (lambda (id) (list (quote orphan) id)) orphans)
        ;; A DOCUMENT SOMEWHERE A DOCUMENT MAY NOT BE. The write path
        ;; refuses to make one, so this is history from before that rule
        ;; or from another store: something the store holds and cannot
        ;; show the way its own rules say it should, which is exactly
        ;; what this verb is for.
        (map (lambda (id) (list (quote nested-document) id)) nested)
        (apply append
               (map (lambda (rec)
                      (let ((writer (car rec)) (seq (cadr rec)) (deps (caddr rec)))
                        (map (lambda (d)
                               (list (quote pending)
                                     (list (quote event) writer seq)
                                     (list (quote missing) (car d) (cdr d))))
                             (missing-deps state deps))))
                    pending))
        ;; A RECORD THIS BUILD DOES NOT UNDERSTAND IS SOMETHING THE STORE
        ;; HOLDS AND CANNOT SHOW, which is what this verb is for. It used
        ;; to be applied silently and change nothing, so an older build
        ;; reading a newer store reported a state missing those records
        ;; without saying anything was missing.
        (reduce-noted state))))


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

  ;; A DOCUMENT IS A FILE, AND A FILE IS NOT INSIDE ANOTHER FILE. Import
  ;; never produces a document under a document, and export has no file
  ;; for one: it writes one file per top-level document, so a nested one
  ;; is folded into its ancestor's file as a section -- losing its front
  ;; matter and taking an empty heading, because a document has no title
  ;; to make one from. Read on its own it is still a document and keeps
  ;; its front matter.
  ;;
  ;; THE TWO READINGS ARE BOTH DEFENSIBLE, WHICH IS THE PROBLEM. A state
  ;; that means two things is refused at the entrance rather than given
  ;; two interpretations, because every later rule would then have to
  ;; choose one and they would not all choose the same.
  ;; THE PARENT A BLOCK HAS NOW, and whether a block is a document.
  ;; `nested-document?` asks about a block being DESCRIBED; these two ask
  ;; about one that already exists, which is what `set` needs.
  (define (block-parent state id)
    (let ((b (state-read state id)))
      (and b (let ((p (assq 'position (cdr b))))
               (and p (cadr p))))))

  (define (document? state id)
    (let ((e (assq 'kind (block-fields state id))))
      (and e (eq? (cdr e) 'doc))))

  (define (nested-document? state parent fields)
    (and (not (eq? parent 'root))
         (let ((e (assq 'kind fields)))
           (and e (eq? (cdr e) 'doc)))))

  ;; The fields a block has now, as an alist, for asking about a block
  ;; the caller named rather than one it is describing.
  (define (block-fields state id)
    (let ((b (state-read state id)))
      (if b (cdr (assq 'fields b)) '())))

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
             ((nested-document? state parent fields)
              (list 'error 'doc-must-be-top-level (list 'parent parent)))
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
        ;; `set` CAN MAKE A BLOCK INTO A DOCUMENT, and that is the third
        ;; way to arrive at a shape the other two refuse. `insert` and
        ;; `move` both ask `nested-document?`; setting `kind` to `doc` on
        ;; a block that already sits under one reached the same shape
        ;; without passing either, so the store held a nested document
        ;; and `conflicts` reported it -- while the README said the write
        ;; path refuses to create one.
        ;;
        ;; A RULE ENFORCED AT TWO OF ITS THREE ENTRANCES is not enforced.
        ((set)
         (let ((id (cadr i)))
           (cond
             ((not (known? state id)) (missing id))
             ((and (memq (caddr i) '(name doc))
                   (equal? (assq 'kind (block-fields state id)) '(kind . code))
                   (equal? (assq 'mode (block-fields state id)) '(mode . text)))
              (list 'error 'derived-field (list 'field (caddr i))))
             ((and (memq (caddr i) '(name names))
                   (equal? (assq 'kind (block-fields state id)) '(kind . code))
                   (equal? (assq 'mode (block-fields state id)) '(mode . datum)))
              (list 'error 'derived-field (list 'field (caddr i))))
             ((and (eq? (caddr i) 'mode) (assq 'mode (block-fields state id))
                   (not (equal? (cdr (assq 'mode (block-fields state id)))
                                (and (pair? (cdddr i)) (cadddr i)))))
              (list 'error 'mode-mismatch '(remedy create-new-file)))
             ((and (eq? (caddr i) 'src) (pair? (cdddr i)) (string? (cadddr i))
                   (equal? (assq 'mode (block-fields state id)) '(mode . text)))
              (list 'set id 'src (string->utf8 (cadddr i))))
             ;; ANY NON-ROOT PARENT, not merely a parent that is itself a
             ;; document. `insert` and `move` refuse a document anywhere
             ;; but the root; asking only about the immediate parent let
             ;; `document -> section -> section` be relabelled, and the
             ;; store then held a nested document by a third route while
             ;; the other two were shut. A rule stated three ways is a
             ;; rule with two of them wrong.
             ((and (eq? (caddr i) 'kind)
                   (pair? (cdddr i))
                   (eq? (cadddr i) 'doc)
                   (let ((pos (block-parent state id)))
                     (and pos (not (eq? pos 'root)))))
              (list 'error 'doc-must-be-top-level
                    (list 'parent (block-parent state id))))
             ((null? (cdddr i)) (list 'set id (caddr i)))
             (else (list 'set id (caddr i) (cadddr i))))))
        ((del)
         (if (known? state (cadr i)) (list 'del (cadr i)) (missing (cadr i))))
        ((move)
         (let ((id (cadr i)) (parent (caddr i)) (after (cadddr i)))
           (cond
             ((not (known? state id)) (missing id))
             ((and (not (eq? parent 'root)) (not (known? state parent))) (missing parent))
             ((nested-document? state parent (block-fields state id))
              (list 'error 'doc-must-be-top-level (list 'parent parent)))
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
        ;; SPELLED, FOR THE SAME REASON AS THE OTHER ONE. An intent's verb
        ;; is whatever the caller wrote, so the refusal that names it must
        ;; not be the thing that carries it back.
        (else (list 'error 'unknown-verb
                    (list 'spelling (datum-spelling (car i))))))))

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

  ;; THE TWO HALVES OF ONE OUTCOME GET TWO ANSWERS, because they state
  ;; opposite facts. `reserved-not-written` says the log is untouched, so
  ;; a resend finds no evidence and runs -- which is right, and a plain
  ;; error says it. The other two leave bytes a later replay may read
  ;; back as a committed record, and the only word for that is `unknown`:
  ;; it is the one answer whose protocol is "ask me again". One word for
  ;; both left a caller holding a term the response table does not
  ;; define, in the state that most needs a defined one.
  ;;
  ;; IT IS ONE FUNCTION BECAUSE THERE ARE TWO CALLERS. The records a
  ;; caller names and the records the store writes for itself -- the
  ;; plan, the receipt -- reach the disk the same way and must be
  ;; answered for the same way; the second of them kept the raw outcome
  ;; for a while, and a torn receipt was reported in a word the table
  ;; does not define.
  ;; AND A REFUSAL IS NOT AN OUTCOME AT ALL. `refused-before-reserve`
  ;; means the append never started: the store declined it, for a reason
  ;; the caller can act on -- a registry standing ahead of the log asks
  ;; for an adopt. Translating that into `unknown` would be true about
  ;; the disk and useless to the caller, who would re-ask a question
  ;; already answered. The path that writes a caller's own records has
  ;; always kept the reason; the path that writes the store's own
  ;; records reached this helper with every outcome, so a receipt
  ;; refused for a nameable reason came back as `unknown`.
  (define (write-outcome->answer outcome seq)
    (cond
      ((eq? (car outcome) 'refused-before-reserve)
       (let ((why (if (pair? (cdr outcome)) (cadr outcome) '())))
         (append (list 'error 'refused why)
                 (let ((r (remedy-for why)))
                   (if r (list (list 'remedy r)) '())))))
      ((eq? (car outcome) 'reserved-not-written)
       (list 'error 'not-written (car outcome) (list 'sequence seq)))
      (else
       (list 'error 'unknown (list (car outcome) (list 'sequence seq))))))

  (define (block-ids-of payload writer seq)
    (case (car payload)
      ((put) (list (block-id writer seq)))
      ((set del move) (list (cadr payload)))
      ((link unlink) (list (cadr payload) (cadddr payload)))
      (else '())))

  ;; COMMITTING AND DESCRIBING ARE TWO ACTS, AND ONLY THE FIRST DECIDES
  ;; THE ANSWER'S HEAD. Section 7.3 says `ok` means the work is durable;
  ;; read the other way, an error that is not `unknown` has to mean no
  ;; record. Building the report runs AFTER the barrier, and any failure
  ;; in it used to turn a durable write into an error -- so a client
  ;; retried a write that had already happened, and a `set` gained a
  ;; second record.
  ;;
  ;; MEASURED, BEFORE THIS SPLIT: a relation name that the wire writer
  ;; will not emit bare made `block-hash` raise while the answer was
  ;; being assembled. The record was correct and on disk; the caller was
  ;; told `(error internal ...)`. Records went from three to four.
  ;;
  ;; SO THE HASHES ARE A SECTION THAT MAY BE MISSING. `(state unavailable
  ;; (reason ...))` says the work was done and this part could not be
  ;; described, which is the truth; an error said something that was not.
  (define (failure-text e)
    (cond
      ((and (vector? e) (= 3 (vector-length e)) (eq? (vector-ref e 0) 'sexpr-error))
       (vector-ref e 1))
      ((and (condition? e) (message-condition? e)) (condition-message e))
      (else "unexpected failure")))

  (define (state-section state payload writer seq)
    (guard (e (#t (list 'state 'unavailable (list 'reason (failure-text e)))))
      (if (report-fault?)
          (list 'state 'unavailable (list 'reason "injected report failure"))
          (list 'state (state-report state (block-ids-of payload writer seq))))))

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
  ;; A REQUEST THE CALLER CAN REPEAT. `who` and `after` are the client's,
  ;; `verb` and `args` are the whole of what it asked for, and `req-id`
  ;; is the name it will use again if it never hears an answer. Together
  ;; they are the identity and the fingerprint -- derived here and
  ;; nowhere else, so a retry that computes them from the same five
  ;; values gets the same two.
  ;; A LIST, LIKE EVERY OTHER SMALL RECORD IN THIS TREE, so that it can
  ;; be written into a trace or a fixture without a constructor.
  ;; THERE IS NO PLAN SIZE HERE, and that is a statement rather than an
  ;; omission. What this path writes is one sub-operation belonging to no
  ;; plan, which is what the actor's `single` says; a request of several
  ;; needs a plan record and a receipt before the first of them, and
  ;; those carry their own count. A field standing for a plan that does
  ;; not exist could only ever be wrong: set to 1 it makes the decision
  ;; look for sub-operation 0 and find a record that calls itself
  ;; `single`, which reads as a plan with a hole in it.
  (define (make-write-request who verb args req-id after)
    (unless (req-id-ok? req-id)
      (assertion-violation 'make-write-request "not a request id" req-id))
    (unless (and (pair? after) (string? (car after))
                 (integer? (cdr after)) (exact? (cdr after)))
      (assertion-violation 'make-write-request "not a cursor" after))
    (list 'write-request who verb args req-id after))

  (define (write-request? x)
    (and (list? x) (= 6 (length x)) (eq? (car x) 'write-request)))

  (define (write-request-who r) (list-ref r 1))
  (define (write-request-verb r) (list-ref r 2))
  (define (write-request-args r) (list-ref r 3))
  (define (write-request-req-id r) (list-ref r 4))
  (define (write-request-after r) (list-ref r 5))

  ;; EVERY WRITER'S UNCERTAIN STRETCHES, AND THE REPORT THAT GOES WITH
  ;; THEM. A stretch on any writer can stand between this request and an
  ;; answer, so the test is over all of them and not over the local one.
  (define (store-intervals store)
    (let loop ((ws (store-writers store)) (out '()) (bad '()))
      (if (null? ws)
          (list (apply append (reverse out)) (reverse bad))
          (let ((loaded (uncertain-load store (car ws))))
            (loop (cdr ws) (cons (car loaded) out)
                  (if (cadr loaded) (cons (cadr loaded) bad) bad))))))

  ;; THE WRITERS THIS ONE WAS RETIRED IN FAVOUR OF, followed to the end
  ;; of the chain. A request's possible positions include every position
  ;; on every one of them, so a chain cut short here is a stretch nobody
  ;; tests against.
  ;; `(<writers> <problem or #f>)`. THE PROBLEM IS NOT A DIAGNOSTIC. A
  ;; retirement record that cannot be read is indistinguishable, to
  ;; `retired-successor`, from a writer that was never retired -- and the
  ;; two lead to opposite answers: a chain that stops early leaves a
  ;; generation's uncertain stretches out of the test, so a request that
  ;; could have landed there is told to run.
  ;;
  ;; A CYCLE IS NOT AN ORDER EITHER. One successor per writer means the
  ;; walk has already seen every distinct writer by the time it repeats
  ;; one, so nothing is missing from the list -- but a cycle is a
  ;; generation model that cannot be true, and certifying ancestry from
  ;; it would be reading a shape the store should not have produced.
  (define (store-successors store writer)
    (let loop ((w writer) (out '()) (seen (list writer)))
      (cond
        ((retirement-unreadable? store w)
         (list (reverse out) (list 'retirement-unreadable w)))
        (else
         (let ((next (retired-successor store w)))
           (cond
             ((not next) (list (reverse out) #f))
             ((member next seen)
              (list (reverse out) (list 'retirement-cycle next)))
             (else (loop next (cons next out) (cons next seen)))))))))

  (define (retirement-unreadable? store w)
    (let ((r (retired-of store w)))
      (and (pair? r) (eq? (car r) 'malformed))))

  ;; WHAT THE STORE SAYS ABOUT A REQUEST IT MAY HAVE ALREADY RUN. It is
  ;; asked under the store's exclusive lock, after the session has caught
  ;; up -- before that, "no evidence" means "no evidence has been
  ;; delivered yet", which is a fact about the reader.
  ;; A CACHE THIS STORE CANNOT READ IS NOT A CACHE MISS. Two of the five
  ;; kinds of uncertain stretch -- the rollback interval and the
  ;; identity-mismatch interval -- exist only in `uncertain.sexp`; no
  ;; derivation can rebuild them, because their coordinates existed only
  ;; at the moment an adopt computed them. So a file that cannot be read
  ;; takes those away, and a retried request that fell in one would be
  ;; told to run again. The report cannot be a warning that execution
  ;; proceeds past.
  ;;
  ;; A stale cache is different and is not an obstacle: the union has
  ;; already repaired it, and every interval the records still imply is
  ;; in the list.
  (define (blocking-uncertainty store)
    (let loop ((rs (cadr (store-intervals store))))
      (cond
        ((null? rs) #f)
        ((eq? (cadr (car rs)) 'unreadable) (car rs))
        (else (loop (cdr rs))))))

  (define (request-verdict store req delivered plan-size . marks)
    (guard (e (#t (list 'unknown (list 'evidence-unavailable (failure-text e)))))
    (let* ((after (write-request-after req))
           (identity (request-identity after (write-request-req-id req)))
           (fingerprint (request-fingerprint (write-request-who req)
                                             (write-request-verb req)
                                             (write-request-args req)
                                             after))
           (loaded (store-intervals store))
           (successors (store-successors store (car after))))
      (cond
        ((blocking-uncertainty store)
         => (lambda (why) (list 'unknown why)))
        ((cadr successors)
         => (lambda (why) (list 'unknown why)))
        ;; A REQUEST OF SEVERAL INTENTS IS A BATCH, AND A BATCH IS ASKED
        ;; THE BATCH'S QUESTION. Its own identity names the receipt; its
        ;; items live under identities of their own, so the evidence has
        ;; to be gathered for both and the rules that decide it are the
        ;; receipt's, not a plan's.
        ((and plan-size (not (and (> (length marks) 1) (eq? (cadr marks) 'plan))))
         (batch-verdict
           (batch-state plan-size
                        (batch-evidence store identity req plan-size delivered
                                        (if (pair? marks) (car marks) '()))
                        fingerprint after (car loaded) #t (car successors))))
        (else
         (request-decision identity fingerprint (write-request-who req) after
                           (evidence-for store identity delivered
                                         (if (pair? marks) (car marks) '()))
                           (car loaded)
                           (and (> (length marks) 1) (eq? (cadr marks) 'plan) plan-size)
                           (car successors)))))))

  ;; RESERVING MORE THAN THE REQUEST USES IS ORDINARY, AND IT COST A
  ;; FORMAT CHANGE TO MAKE IT SO. While the registry held one number,
  ;; reserving five positions and writing two left that number above the
  ;; log's end -- which is exactly what the rollback gate reads as
  ;; history the store was told it had and no longer has. It would have
  ;; demanded an adopt for a store nothing went wrong with, and any
  ;; refusal (which writes nothing at all) would have triggered it.
  ;;
  ;; The entry now says both things: `authorised` is leave to write,
  ;; `written` is how far records reached the disk, and the gate reads
  ;; the second. So a request may reserve its whole range before it knows
  ;; how much of it it will use.
  ;;
  ;; THE REGISTRY IS ASKED ONCE FOR THE WHOLE REQUEST. It used to be read,
  ;; raised and rewritten per record -- two more flushes each -- so
  ;; removing the log's own fsync alone left the cost still growing with
  ;; the number of records. The count is the records this request will
  ;; write: one per intent, and one more for the receipt when there is a
  ;; batch.
  ;;
  ;; OVER-RESERVING IS SAFE AND UNDER-RESERVING IS NOT: the uncertain
  ;; interval a rollback records is `(durable, mark]`, so positions
  ;; reserved and never written are already covered by it, while a
  ;; position written outside the reservation is one nothing vouches for.
  ;; IT ANNOUNCES THE COUNT; IT DOES NOT RESERVE. The reservation is
  ;; taken by the first append, after every check that has to precede
  ;; one -- the instance, the generation chain, integrity, reset. Taken
  ;; here it would raise `authorised` for a request about to be refused
  ;; for a reason that has nothing to do with the registry, and would
  ;; report that refusal in the registry's words instead of its own.
  ;;
  ;; The count is the records this request will write: one per intent,
  ;; and one more for the receipt when there is a batch.
  (define (announce-count! s intents)
    (let ((n (length intents)))
      (session-pending-count-set! s (if (> n 1) (+ n 1) n))))

  ;; NO SUCCESS IS ANSWERED BEFORE THE BARRIER. A request of two intents
  ;; whose first was written and whose second was refused must not hand
  ;; back an `ok` for the first: the caller would take it as durable, and
  ;; nothing has made it so yet. And a barrier that fails makes the WHOLE
  ;; request answer one `unknown` -- not an `ok` for the part that got
  ;; through, because what got through is exactly what nobody can now
  ;; promise.
  ;;
  ;; A REQUEST THAT WROTE NOTHING DOES NOT PASS IT. Nothing it describes
  ;; is claimed to survive a crash, and putting the store's whole
  ;; recovery closure on the way out of every refusal would charge
  ;; refusals for a promise they do not make.
  ;; IT TAKES A THUNK, AND THAT IS THE WHOLE OF THE FIX. As an argument
  ;; the answers were computed BEFORE the call, so a raise anywhere in
  ;; the intent loop unwound past this function and the barrier never
  ;; ran -- which is the one state in which no honest answer is left to
  ;; give. `unknown` means "send it again and I will tell you whether it
  ;; ran", and that promise rests entirely on the records still being
  ;; there to be found: records on disk with no barrier behind them are
  ;; records a resend may not see, and not seeing them means doing the
  ;; work a second time. The barrier is not the tail of the happy path.
  ;; It is what every exit owes.
  (define (commit-then s thunk)
    (let* ((answers (guard (e (#t (vector 'raised e))) (thunk)))
           (outcome (guard (e (#t 'barrier-failed)) (session-commit! s))))
      (cond
        ((eq? outcome 'barrier-failed)
         (list (list 'error 'unknown (list 'commit-barrier-failed))))
        ;; THE RAISE IS PASSED ON, BECAUSE SOMEBODY ELSE ALREADY ASKS
        ;; THIS QUESTION. `with-store-write`'s own guard encloses this
        ;; call; it tests the same flag, answers the same word, and adds
        ;; the section this arm could not: the events it wrote. A second
        ;; arm here was a second supplier of one answer, and the two did
        ;; not agree.
        ;;
        ;; MEASURED, with a reducer made to raise on a receipt payload --
        ;; the one place a raise reaches here with bytes already on the
        ;; disk, since `write-line!` catches everything from the moment
        ;; the flag goes up. With the arm:
        ;;
        ;;   (error unknown (interrupted "..."))
        ;;
        ;; Without it, through the enclosing guard:
        ;;
        ;;   (error unknown (execution-failed "...") (events (("..." . 2))))
        ;;
        ;; `unknown` promises that a resend will find the records, and
        ;; the events are what a caller looks for them by. The flag-down
        ;; readings were identical either way: the raise travels out.
        ;;
        ;; The commit above still runs first, so what this passes on is a
        ;; failure over a log that has already been flushed.
        ((vector? answers)
         (raise (vector-ref answers 1)))
        (else answers))))

  ;; THE BATCH RULES AND THE REQUEST RULES ANSWER IN DIFFERENT WORDS FOR
  ;; THE SAME FACTS, so one of them is translated rather than both being
  ;; understood at every call site. `not-committed` is the batch's way of
  ;; saying what `execute` says: nothing ran, and the positions it could
  ;; have occupied are clear.
  ;;
  ;; `partial` AND `planned` ARE NOT REFUSALS EITHER, but this path
  ;; cannot yet act on them: resuming a batch means running the items
  ;; from k+1 under their own identities, and that is not written. Until
  ;; it is they are reported as they are rather than quietly re-run from
  ;; the beginning, which is the one answer that would duplicate work.
  (define (batch-verdict v)
    (case (car v)
      ((not-committed) (list 'execute))
      ((committed) (list 'replay (caddr v)))
      (else v)))

  ;; THE RECEIPT AND EVERY ITEM. The receipt is a record of the batch's
  ;; own identity; each item is a record of `(batch <req> k)`, which is a
  ;; different identity -- so a scan for one of them finds none of the
  ;; others, and a batch asked about only its own identity would see a
  ;; receipt and conclude that nothing had run.
  (define (batch-evidence store identity req n delivered marks)
    (append
      (evidence-for store identity delivered marks)
      (let loop ((k 0) (out '()))
        (if (= k n)
            (apply append (reverse out))
            (loop (+ k 1)
                  (cons (evidence-for store
                                      (cons (car (write-request-after req))
                                            (list 'batch (write-request-req-id req) k))
                                      delivered marks)
                        out))))))

  ;; NEVER: PUBLISHED WHILE THE LOCK IS STILL HELD AND BEFORE THE ANSWER GOES
  ;; BACK. Both halves are load-bearing. Under the lock, because a value
  ;; published after the unwind would describe a store another session
  ;; was already free to change. Before the answer, because that is what
  ;; makes a client's own writes visible to its own next read: the answer
  ;; is the client's evidence that the write happened, and by the time it
  ;; has that evidence the new value is already the one every reader
  ;; sees.
  ;;
  ;; KEY: AND IT IS THE LIVE FOLD, NOT A REPLAY OF WHAT WAS JUST WRITTEN.
  ;; `state` is the reduction this session has been applying records into
  ;; -- `reduce-apply!` updates it in place -- so it knows things a fresh
  ;; replay would have to rediscover, the consumption index of a plan
  ;; that has just finished among them. Publishing a re-read would be a
  ;; second supplier of the same fact, and the two would differ exactly
  ;; where this one is interesting.
  ;;
  ;; NOTE: PUBLISHING A MUTABLE OBJECT IS SAFE HERE FOR ONE REASON, AND IT
  ;; IS WORTH WRITING DOWN BECAUSE IT READS LIKE A DEFECT: every call to
  ;; `with-store-write` builds its OWN `(reduce-empty)` and folds into
  ;; that, so once a value has been published nothing ever mutates it
  ;; again. NEVER: It does not need a defensive copy, and a reader that
  ;; changed it would be changing what every other reader sees -- which
  ;; is why the readers are handed it as a value they may not modify.
  ;;
  ;; NOTE: A RAISE PUBLISHES NOTHING. The body turns every failure it can
  ;; describe into an answer, so a raise that gets past it is one nothing
  ;; here can say the shape of -- and a value folded from a session in
  ;; that condition is not one to hand to readers.
  ;; NEVER: AND THERE IS NO ROW BEHIND "IT MUST BE THE LIVE FOLD", because
  ;; the claim turned out not to be true. It was written down as "only
  ;; the live fold knows the consumption index of a plan that has just
  ;; finished", and that was hoped rather than measured. Measured, on a
  ;; full `init -> insert -> write -> commit`, comparing this value with
  ;; a fresh `open-and-reduce` taken afterwards: `drafts`, the outline,
  ;; the applied cuts, `state-consumed?` and `state-consumed-completions`
  ;; ALL AGREE. The consumption index is rebuilt from the plan record,
  ;; which carries what it consumed, so a replay finds it.
  ;;
  ;; KEY: SO PUBLISHING THE LIVE FOLD IS AN ECONOMY, NOT A CORRECTNESS
  ;; PROPERTY: it saves folding the whole log a second time on every
  ;; write. NEVER: A row here would have to be green against both versions,
  ;; and a row that cannot fail says nothing -- so the reason is written
  ;; here instead of a row being written that looks like cover.
  ;;
  ;; NOTE: THE OTHER VERSION ALSO CANNOT BE BUILT. This runs INSIDE the
  ;; store's lock; a `open-and-reduce` here would ask for the same lock
  ;; on a second descriptor, which flock refuses to the same process --
  ;; so under a daemon's non-blocking strategy it does not publish a
  ;; wrong value, it fails with `store-busy`.
  (define (publish-after state thunk)
    (let ((answers (thunk)))
      (publish-hook state)
      answers))

  ;; NEVER: A PLAIN VARIABLE SET ONCE, NEVER: NOT A PARAMETER. A parameter would
  ;; say that this can differ between callers, and it cannot: there is
  ;; one publication for the whole VM and only one process ever gets
  ;; here. It would also be saying something untrue about how it is
  ;; scoped -- measured, on the locking hooks: Chez parameters are per OS
  ;; THREAD, and every green thread shares one, so a `parameterize` is
  ;; visible to whatever else runs during its extent and gone afterwards.
  ;; Neither half is what a per-caller override would need, and neither
  ;; is what this wants.
  ;;
  ;; The default does nothing: a CLI run has one reader, itself, and it
  ;; already holds the value.
  (define publish-hook (lambda (state) (if #f #f)))
  (define (store-publish-hook! procedure)
    (unless (procedure? procedure)
      (assertion-violation 'store-publish-hook! "not a procedure" procedure))
    (set! publish-hook procedure))

  (define (with-store-write store proc . rest)
    (let ((actor (if (null? rest) "unknown" (car rest)))
          (req (and (pair? rest) (pair? (cdr rest)) (cadr rest)))
          (preflight (and (> (length rest) 2) (list-ref rest 2)))
          (plan? (and (> (length rest) 3) (list-ref rest 3)))
          ;; WHAT THIS REQUEST CONSUMED, IF IT CONSUMED ANYTHING. It is
          ;; the caller's to say -- the store does not go looking for
          ;; drafts -- and it travels no further than the plan record it
          ;; is written into.
          (consumes (and (> (length rest) 4) (list-ref rest 4)))
          (state (reduce-empty)))
      (let ((s (log-begin store (deliver-into state #f))))
        ;; THE SESSION IS GIVEN BACK BY THE UNWIND, NOT BY A LINE ON EACH
        ;; PATH. Written once per exit it got written wrong twice in one
        ;; sitting: one version raised past every `log-end!` and held the
        ;; store's exclusive lock for the life of the process, and a
        ;; later one ended the session inside a handler AND on the way
        ;; out, so the second call raised `this session has ended` -- a
        ;; failure manufactured inside the recovery from a failure. One
        ;; place that cannot be skipped is the only shape that is right
        ;; by construction.
        (dynamic-wind
          (lambda () (if #f #f))
          (lambda ()
            (publish-after
              state
              (lambda ()
            ;; A RAISE THAT GOT PAST EVERY INNER GUARD IS STILL AN ANSWER
            ;; WHEN BYTES WERE WRITTEN. `unknown` is a promise the store
            ;; can only keep about records that survive -- it means "send
            ;; it again and I will tell you whether it ran" -- so the
            ;; barrier runs before the answer is given and a resend will
            ;; find them.
            ;;
            ;; AND IT IS NOT THE ANSWER WHEN NOTHING WAS WRITTEN. There
            ;; is nothing to promise and nothing for a resend to find,
            ;; and sending every caller back to re-ask about a request
            ;; that provably did nothing is how a word meaning "ask me
            ;; again" stops meaning anything. That request gets the
            ;; failure it actually had.
            (guard (e (#t (if (session-write-started? s)
                              (begin
                                (guard (inner (#t #f)) (session-commit! s))
                                (list (list 'error 'unknown
                                            (list 'execution-failed (failure-text e))
                                            (list 'events (session-written-events s)))))
                              (raise e))))
              ;; THE FRONTIER IS REPORTED ONCE DELIVERY IS OVER, not
              ;; inferred from the per-record answers. A record is
              ;; answered as it arrives, and a record whose premise has
              ;; not arrived yet is answered `pending` -- truthfully. But
              ;; applying a later record can drain it, and nothing goes
              ;; back to revise the earlier answer. Without this line the
              ;; session's applied cursor stops at the first such record
              ;; forever, `predecessor-applied?` stays false, and
              ;; `session-view` hands back #f: the store cannot be written
              ;; to at all. It bites exactly when the local writer's own
              ;; record declares a dep on a writer the load delivers AFTER
              ;; it, which is decided by nothing more than the two
              ;; writers' names.
              (session-applied! s (session-epoch s) (reduce-applied-cut state))
              ;; THE QUESTION IS ASKED HERE AND NOT BEFORE. Delivery has
              ;; just finished, so "there is no evidence of this request"
              ;; is a statement about the store rather than about how far
              ;; the reader had got -- and the lock has been held
              ;; throughout, so nothing can arrive between the answer and
              ;; the act.
              ;; THE INTENTS ARE ASKED FOR BEFORE THE VERDICT, because how
              ;; many of them there are is part of the question: a request
              ;; of several has a plan, and the rules about a plan's holes
              ;; do not run for a request that declares none. `proc` only
              ;; reads the state and answers a list -- it writes nothing,
              ;; so asking it early costs nothing and changes nothing.
              (let* ((intents (proc state (session-view s)))
                     ;; KEY: A COMMIT IS A PLAN EVEN WHEN IT CARRIES ONE
                     ;; SUB-OPERATION, and both of the two places that
                     ;; decide that have to say so. This one gives the
                     ;; verdict the request's SIZE: passing #f for a
                     ;; one-intent commit made a retry ask the question a
                     ;; single-record request asks, and the answer to
                     ;; that question about a plan is `unknown`.
                     (verdict (and req (request-verdict store req
                                                        (reduce-applied-cut state)
                                                        (if plan?
                                                            (length intents)
                                                            (and (not (= (length intents) 1)) (length intents)))
                                                        (reduce-gates state)
                                                        (and (or plan? (null? intents)) 'plan)))))
                (if (and verdict (not (eq? (car verdict) 'execute))
                         (not (and (eq? (car verdict) 'complete) (>= (length verdict) 4))))
                    ;; THE ANSWER IS MADE BEFORE THE SESSION ENDS. The
                    ;; unwind releases the store's exclusive lock, and the
                    ;; answer to a replay performs a barrier -- so
                    ;; computing it afterwards would certify, outside the
                    ;; lock, a state another session was free to change in
                    ;; between.
                    ;;
                    ;; AND A BARRIER THAT FAILS HERE IS THE SAME EVENT IT
                    ;; IS ON THE WRITE PATH, so it gets the same word. A
                    ;; replay answer IS a promise of durability -- that is
                    ;; the whole of what a replay tells a client -- so
                    ;; failing to make it good is `unknown` and not an
                    ;; internal error. The two paths answered differently
                    ;; for one fault.
                    (list (guard (e (#t (list 'error 'unknown
                                              (list 'replay-barrier-failed
                                                    (failure-text e)))))
                            (request-answer s store verdict)))
                    (commit-then s
                      (lambda ()
                        (let ((bad (and (not (and verdict (eq? (car verdict) 'complete)))
                                        (or (and preflight (preflight state))
                                            (begin (announce-count! s intents)
                                                   (and req (cursor-unreachable store s req)))))))
                          (cond
                            (bad (list bad))
                            ;; KEY: A PLAN THAT IS ALREADY PERSISTED IS
                            ;; FINISHED FROM THE PLAN. The premises are
                            ;; NOT checked again: the blocks this request
                            ;; already wrote have moved, and re-checking
                            ;; would read the request's own progress as
                            ;; somebody else's edit. The frozen
                            ;; declaration is the only input.
                            ((and verdict (eq? (car verdict) 'complete) (>= (length verdict) 4))
                             (complete-plan! s state req verdict))
                            ;; A REQUEST OF ONE SUB-OPERATION HAS NO PLAN,
                            ;; and says so: `single`, no plan event.
                            ;; Zero operations still have an identity: the
                            ;; empty plan is their complete durable evidence.
                            ;; One operation needs only its single record.
                            ;; AND THIS ONE CHOOSES THE PATH. `plan?` is
                            ;; the caller saying "this is a commit"; the
                            ;; number of blocks it names is not what
                            ;; makes it one.
                            ((and req (or (null? intents) plan?))
                             (session-pending-count-set! s (+ 1 (length intents)))
                             (write-plan-then! s state req intents consumes))
                            ((null? intents) '())
                            ((or (not req) (= 1 (length intents)))
                             (run-intents! s state
                                           (lambda (n)
                                             (or (request-actor req 'single #f) actor))
                                           intents))
                            ;; SEVERAL INTENTS UNDER ONE REQUEST ARE A
                            ;; BATCH, AND A BATCH IS NOT ONE REQUEST WITH
                            ;; SEVERAL SUB-OPERATIONS. Each item is its
                            ;; own request, under its own identity
                            ;; `(batch <req> k)`, and what holds them
                            ;; together is the receipt.
                            ;;
                            ;; The plan record is a different level: it
                            ;; belongs inside an item that expands into
                            ;; several records of its own. Writing one
                            ;; batch-level plan would give the items one
                            ;; identity between them, and a retry could
                            ;; then only ever say how far the whole batch
                            ;; got -- never which item.
                            (else
                             (write-batch! s state req intents))))))))))))
          (lambda () (guard (e (#t #f)) (log-end! s)))))))

  ;; THE ACTOR A REQUEST WRITES. Without it the record carries only a
  ;; name, `store-evidence` finds nothing when the request comes back,
  ;; and the store executes it a second time -- so the identity that
  ;; makes the decision possible has to be IN the record, not beside it.
  ;; A single sub-operation belongs to no plan, which is what `single`
  ;; and the absent plan event say.
  ;; A CURSOR THE RECORD COULD NOT HAVE BEEN WRITTEN AT IS REFUSED BEFORE
  ;; ANYTHING IS WRITTEN. The cursor is the client's: it says where its
  ;; earlier attempt would have landed, and every later decision measures
  ;; from it. A client that declares `(W . 100)` on a writer standing at
  ;; 1 gets its record at W:2 -- and when that record is later lost to a
  ;; rollback, the interval the adopt records ends far below 100, so the
  ;; retry's range test finds nothing in its way and the store runs the
  ;; work a second time. The cursor is not decoration; it is the bottom
  ;; of the stretch that protects the request.
  ;;
  ;; What must be true: the position about to be written is strictly
  ;; above the cursor on the cursor's own writer, or it is on a writer
  ;; that succeeded it -- where there is no cursor to compare and every
  ;; position is reachable. Sequence numbers are never compared across
  ;; writers.
  (define (cursor-unreachable store s req)
    (let* ((v (session-view s))
           (after (write-request-after req))
           (writer (and v (view-writer v)))
           (seq (and v (view-expect-seq v)))
           (chain (store-successors store (car after))))
      (cond
        ((not v) #f)
        ((cadr chain) (list 'error 'unknown (cadr chain)))
        ((and (string=? writer (car after)) (> seq (cdr after))) #f)
        ((member writer (car chain)) #f)
        (else
         (list 'error 'cursor-unreachable
               (list 'after after) (list 'writing (cons writer seq)))))))

  ;; `sub` IS `single` FOR A REQUEST OF ONE, THE INDEX FOR A PLAN'S
  ;; sub-operation, and `plan` for the plan record itself; `plan-event`
  ;; is absent for the first two shapes and the plan's own event id for a
  ;; sub-operation. A record that called itself `single` while pointing
  ;; at a plan, or an indexed one pointing at nothing, would be a record
  ;; whose two statements about where it belongs disagree -- and the Q
  ;; review found that exact shape reachable through a different door.
  (define (request-actor req sub plan-event)
    (and req
         (list (write-request-who req)
               (request-identity (write-request-after req)
                                 (write-request-req-id req))
               sub
               (request-fingerprint (write-request-who req)
                                    (write-request-verb req)
                                    (write-request-args req)
                                    (write-request-after req))
               plan-event
               (write-request-after req))))

  ;; A BATCH: ONE RECEIPT, THEN THE ITEMS, EACH ITS OWN REQUEST.
  ;;
  ;; THE RECEIPT IS WRITTEN AND MADE DURABLE BEFORE THE FIRST ITEM RUNS,
  ;; which is what lets a retry say anything at all: without it a crash
  ;; between two items leaves a set of records and no statement of what
  ;; the set was supposed to be, so "item 3 is missing" and "there were
  ;; only three items" are the same picture.
  ;;
  ;; ITS ENTRIES ARE COMPUTED BEFORE ANYTHING IS WRITTEN, from the
  ;; position the session is standing at -- the lock is held, so the
  ;; sequence numbers are known. Each item's cursor is the position
  ;; before it: the receipt takes p, item k takes p+1+k, and item k is
  ;; written against p+k.
  ;;
  ;; AND EACH POSITION IS CHECKED BEFORE ITS RECORD IS WRITTEN, not
  ;; after. The timetable is frozen; a record that would land somewhere
  ;; else is refused before the deviating record exists, because
  ;; afterwards the deviation is on disk with a receipt that disagrees
  ;; with it.
  (define (write-batch! s state req intents)
    (let* ((v (session-view s))
           (writer (and v (view-writer v)))
           (p (and v (view-expect-seq v))))
      (if (not v)
          (let ((why (session-view-refusal s)))
            (list (if why (list 'error 'no-view why) (list 'error 'no-view))))
          (let* ((entries (let loop ((k 0) (out '()))
                            (if (= k (length intents))
                                (reverse out)
                                (loop (+ k 1)
                                      (cons (cons k (cons writer (+ p k))) out)))))
                 (payload (list 'batch
                                (write-request-req-id req)
                                (request-fingerprint (write-request-who req)
                                                     (write-request-verb req)
                                                     (write-request-args req)
                                                     (write-request-after req))
                                (write-request-after req)
                                entries))
                 (answer (append-payload! s state (request-actor req 'plan #f) payload)))
            (if (eq? (car answer) 'error)
                (list answer)
                ;; THE RECEIPT IS MADE DURABLE BEFORE THE FIRST ITEM RUNS.
                ;; "Persist the receipt before executing any item" is
                ;; otherwise a false sentence: a crash between the
                ;; receipt's write and the first item would leave the
                ;; items on disk and the statement of what they were
                ;; supposed to be not -- which is the one picture the
                ;; receipt exists to prevent.
                (let ((c (guard (e (#t 'barrier-failed)) (session-commit! s))))
                  (if (eq? c 'barrier-failed)
                      (list (list 'error 'unknown (list 'receipt-barrier-failed)))
                      (run-items! s state req intents entries))))))))

  ;; EACH ITEM UNDER ITS OWN IDENTITY, and its actor says `single`
  ;; because one item of one intent is one sub-operation belonging to no
  ;; plan. An item that expanded into several records would write its own
  ;; plan first; nothing does that yet, and when something does it is
  ;; that item's business and not the batch's.
  (define (run-items! s state req intents entries)
    (let loop ((is intents) (k 0) (made '()) (out '()))
      (if (null? is)
          (reverse out)
          (let* ((declared (cdr (assv k entries)))
                 (v (session-view s))
                 (at (and v (cons (view-writer v) (view-expect-seq v))))
                 (raw (car is))
                 ;; THE SAME CHECK THE OTHER LOOP MAKES, AND IT HAS TO BE
                 ;; MADE HERE TOO. One DEFINITION of the rule is the thing
                 ;; worth having; one CALL SITE is not, and insisting on it
                 ;; left this path -- the tracked batch of several items --
                 ;; reading `(car raw)` on whatever arrived.
                 ;;
                 ;; AND IT MATTERS MORE HERE THAN THERE. The receipt is
                 ;; already written and committed by the time this loop
                 ;; runs, so a raise out of it does not merely produce a
                 ;; worse message: it discards the answers of the items
                 ;; that already succeeded and leaves the request without
                 ;; the commit that ends it.
                 (rs (if (malformed-intent? raw) '() (intent-refs raw)))
                 (fixed (if (null? rs)
                            raw
                            (let ((pa (resolve-from made (car rs)))
                                  (af (resolve-from made (cadr rs))))
                              (cond
                                ((and (pair? pa) (eq? (car pa) 'error)) pa)
                                ((and (pair? af) (eq? (car af) 'error)) af)
                                (else (intent-with-refs raw pa af))))))
                 (answer
                   (cond
                     ;; THE REASON DESCRIBES ITSELF, so the intent does not
                     ;; have to be sent back to explain it -- and a refusal
                     ;; stops being unreadable to the one reader that needs
                     ;; it, by construction rather than by remembering to
                     ;; spell things carefully.
                     ((intent-reason raw)
                      => (lambda (why)
                           (list 'error 'malformed-intent why)))
                     ((and (pair? fixed) (eq? (car fixed) 'error)) fixed)
                     ;; THE POSITION IS CHECKED BEFORE THE RECORD EXISTS.
                     ((not (equal? at (cons (car declared) (+ 1 (cdr declared)))))
                      (list 'error 'receipt-timetable
                            (list 'item k) (list 'declared declared) (list 'writing at)))
                     (else
                     ;; A RAISE IN ONE SUB-OPERATION IS THAT SUB-OPERATION'S
                     ;; ANSWER. The loop already stops at the first failed
                     ;; intent, and stopping with an answer keeps the ones
                     ;; that succeeded before it -- a raise instead threw
                     ;; away the whole request's answers, including the
                     ;; `ok`s for records already on the disk.
                     ;;
                     ;; AND IT ASKS THE SAME QUESTION THE OUTER ONE DOES,
                     ;; because it is reached first and would otherwise
                     ;; answer on the outer one's behalf. `unknown` is
                     ;; owed only where bytes may be on the disk; with
                     ;; none, this hands the failure on rather than
                     ;; dressing it as a request whose outcome is in
                     ;; doubt. An inner guard that decided for itself
                     ;; would make the gate above unreachable.
                     (guard (e (#t (if (session-write-started? s)
                                       (list 'error 'unknown
                                             (list 'interrupted (failure-text e)))
                                       (raise e))))
                       (one-intent! s state (item-actor req k declared) fixed))))))
            (if (eq? (car answer) 'error)
                (reverse (cons answer out))
                (loop (cdr is) (+ k 1)
                      ;; THE ID COMES FROM THE EVENT, NOT FROM THE REPORT.
                      ;; Which block an insert made is decided by the
                      ;; record's own coordinates and is knowable the
                      ;; moment it commits; the hashes beside it are a
                      ;; description built afterwards, and that
                      ;; description is allowed to be missing. Reading
                      ;; the id out of it made a failure to DESCRIBE a
                      ;; write delete a block: the back-reference then
                      ;; resolved to nothing, the intent naming it
                      ;; answered `no-such-intent`, and the caller was
                      ;; told one of its own blocks had never been asked
                      ;; for.
                      (let ((ev (cadr (assq 'events (cdr answer)))))
                        (if (and (eq? 'insert (car (unwrap fixed))) (pair? ev))
                            (cons (cons k (block-id (car (car ev)) (cdr (car ev)))) made)
                            made))
                      (cons answer out)))))))

  (define (item-actor req k after)
    (list (write-request-who req)
          (cons (car (write-request-after req))
                (list 'batch (write-request-req-id req) k))
          'single
          (request-fingerprint (write-request-who req)
                               (write-request-verb req)
                               (write-request-args req)
                               (write-request-after req))
          #f
          after))

  ;; THE PLAN IS WRITTEN AS ONE MORE RECORD, through the same append the
  ;; sub-operations use, so it takes a position in the log like anything
  ;; else and the sub-operations can point at it.
  ;;
  ;; A PLAN THAT COULD NOT BE WRITTEN STOPS THE REQUEST. Running the
  ;; sub-operations anyway would leave records whose actors point at a
  ;; plan that is not there -- and a retry reading them would find a set
  ;; of records claiming indices in a plan nobody can produce.
  ;; FINISHING A PLAN SOMEBODY ELSE STARTED -- or that this process
  ;; started before it died. The members that are already there stay;
  ;; the ones that are not are written at their own declared indices,
  ;; under the same plan event, from the text the plan froze.
  ;; NEVER: THE FROZEN TEXT IS CHECKED AGAINST THE NAME THE RECORD GAVE IT.
  ;;
  ;; A plan says both "this is the text" and "this was version V of the
  ;; draft", and V is `sha256(text || based-on || cut)`. If they disagree
  ;; the record is not describing one draft, and completing it would
  ;; carry out something nobody sent. §7.5.14 asks for the check at the
  ;; two moments the text is used: here, and in `restore`.
  (define (consumes-mismatch entries consumes)
    (and consumes
         (let ((items (caddr consumes)))
           (let loop ((es entries))
             (cond
               ((null? es) #f)
               (else
                (let* ((body (cdar es))
                       (block (and (pair? body) (pair? (cdr body)) (eq? 'set (car body))
                                   (cadr body)))
                       (text (and block (= 4 (length body)) (eq? 'src (caddr body))
                                  (cadddr body)))
                       (item (and block (assoc block items))))
                  (if (and text item (string? text)
                           (not (equal? (cadr item)
                                        (draft-version (string->utf8 text)
                                                       (caddr item) (cadddr item)))))
                      block
                      (loop (cdr es))))))))))

  (define (complete-plan! s state req verdict)
    (let* ((present (cadr verdict))
           (plan-event (caddr verdict))
           (entries (list-ref verdict 3))
           (consumes (and (>= (length verdict) 5) (list-ref verdict 4)))
           (missing (filter (lambda (e) (not (memv (car e) present))) entries))
           (indices (map car missing))
           (bad (consumes-mismatch missing consumes)))
      (if bad
          (list (list 'error 'consumes-version-mismatch (list 'block bad)))
          (begin
            (session-pending-count-set! s (length missing))
            (run-intents! s state
                          (lambda (n) (request-actor req (list-ref indices n) plan-event))
                          (map cdr missing))))))

  (define (write-plan-then! s state req intents . rest)
    (let* ((entries (plan-entries intents))
           (consumes (and (pair? rest) (car rest)))
           ;; THE SIXTH ELEMENT IS WRITTEN ONLY WHEN THERE IS ONE. A plan
           ;; without it consumed nothing, which is exactly what every
           ;; plan written before this field existed means -- so absence
           ;; keeps its meaning and no record has to be rewritten.
           (payload (append
                      (list 'plan
                            (write-request-req-id req)
                            (request-fingerprint (write-request-who req)
                                                 (write-request-verb req)
                                                 (write-request-args req)
                                                 (write-request-after req))
                            (write-request-after req)
                            entries)
                      (if consumes (list consumes) '())))
           (answer (append-payload! s state (request-actor req 'plan #f) payload)))
      (if (eq? (car answer) 'error)
          (list answer)
          (let ((plan-event (cadr (assq 'event (cdr answer))))
                (durable? (guard (failure (#t #f)) (session-commit! s) #t)))
            (if (not durable?)
                (list '(error unknown (plan-barrier-failed)))
                ;; NEVER: THE PLAN RECORD IS NOT A SUB-OPERATION. A request
                ;; with no intents still has an identity and still writes
                ;; one, but answering with an `ok` DESCRIBING THAT RECORD
                ;; put a sub-operation in the list that no caller asked
                ;; for: a tracked empty batch answered `(done 1)` for zero
                ;; work. Measured before this line changed.
                ;;
                ;; NEVER: AND A REPLAY OF AN EMPTY REQUEST DOES NOT ANSWER
                ;; WHAT THE FIRST ONE DID -- an earlier version of this
                ;; comment claimed it does, and nothing had measured it.
                ;; Measured:
                ;;
                ;;   first   (batch () (done 0))
                ;;   replay  (batch ((ok (replay #t) (event …))))
                ;;
                ;; An empty plan is complete the moment it is written, so
                ;; the second call is a replay and takes the receipt path,
                ;; which carries no count. The two answers differ and both
                ;; are right; what the empty list fixes is only the first
                ;; one, where a synthetic item used to be counted as work.
                ;; NEVER: A REQUEST OF NO SUB-OPERATIONS STILL WRITES ONE
                ;; EMPTY PLAN, AND THE FIRST ANSWER NAMES IT. Returning an
                ;; empty list here removed that receipt, and four existing
                ;; rows said so at once -- `empty-plan`'s QE-01 and three in
                ;; `q7`, which pin that a client places its cursor from this
                ;; answer's `events` and `cursor`.
                ;;
                ;; What was wrong was the COUNT, not the receipt: this item
                ;; describes the plan record rather than work anyone asked
                ;; for, so `batch-answer` answers `(done 0)` for it instead
                ;; of counting it. Measured:
                ;;
                ;;   first   (batch ((ok (events (E)) (state ()) (cursor E)
                ;;                        (replay #f))) (done 0))
                ;;   replay  (batch ((ok (replay #t) (event E))))
                ;;
                ;; Both are right: an empty plan is complete when written, so
                ;; asking again is a replay and takes the receipt path, which
                ;; carries no count at all.
                (if (null? intents)
                    (list (list 'ok (list 'events (list plan-event))
                                (list 'state '()) (list 'cursor plan-event)
                                (list 'replay #f)))
                    (run-intents! s state
                                  (lambda (n) (request-actor req n plan-event))
                                  intents)))))))

  ;; ONE RECORD, NO INTENT BEHIND IT. The plan is not something a caller
  ;; asked for as a verb; it is the store writing down what it is about
  ;; to do, so it goes round `resolve` rather than through it.
  (define (append-payload! s state actor payload)
    (let ((v (session-view s)))
      (if (not v)
          (let ((why (session-view-refusal s)))
            (if why (list 'error 'no-view why) (list 'error 'no-view)))
          (let* ((writer (view-writer v))
                 (seq (view-expect-seq v))
                 (deps (deps-for-payload state writer payload))
                 (frame (make-frame (view-revision v) (view-epoch v)
                                    writer seq actor deps payload))
                 (outcome (session-append! s frame)))
            ;; THE SAME THREE WORDS AS EVERY OTHER APPEND. This path
            ;; writes the records a caller did not ask for by name --
            ;; the plan, the receipt -- and it answered with the raw
            ;; outcome, so a receipt whose write tore left the caller
            ;; holding `(error partial-write ...)`: bytes on the disk
            ;; and a word that is neither `ok` nor `unknown`, which is
            ;; exactly the combination the answer vocabulary exists to
            ;; prevent. The translation is the one `one-intent!` uses,
            ;; because it is the same question about the same disk.
            (if (not (eq? (car outcome) 'committed))
                (write-outcome->answer outcome seq)
                ;; AND IT IS APPLIED BEFORE THE NEXT APPEND. A session
                ;; holds one unconfirmed record at a time: until the
                ;; reducer has taken this one the view is not ready, and
                ;; the sub-operations that follow would every one of them
                ;; be refused for a reason that has nothing to do with
                ;; them.
                (begin
                  ;; KEY: THE ACTOR GOES IN. The replay path supplies it and
                  ;; this one did not, so a reduction built by writing
                  ;; disagreed with the same reduction built by reading:
                  ;; the consumption index is fed from a record's actor --
                  ;; that is how a member knows which plan it belongs to
                  ;; -- and without it a live write completed no plan at
                  ;; all. It only looked right because a fixture that
                  ;; reopens the store is reading a replay.
                  (reduce-apply! state writer seq deps payload actor)
                  (session-applied! s (session-epoch s) (reduce-applied-cut state))
                  (list 'ok (list 'event (cons writer seq)))))))))

  ;; WHAT THE PLAN DECLARES IS THE INTENT, with every back-reference
  ;; rewritten as the marker that will be bound from the record that
  ;; satisfies it. `(from k)` says "the block intent k makes", which is
  ;; exactly what `("#%new" k)` says -- the first is how a caller writes
  ;; it inside one batch, the second is how it survives on disk and is
  ;; read back by someone who was not there.
  (define (plan-entries intents)
    (let loop ((is intents) (n 0) (out '()))
      (if (null? is)
          (reverse out)
          (loop (cdr is) (+ n 1)
                (cons (cons n (declared-intent (car is))) out)))))

  (define (declared-intent intent)
    (let ((rs (intent-refs intent)))
      (if (null? rs)
          (unwrap intent)
          (intent-with-refs intent (as-marker (car rs)) (as-marker (cadr rs))))))

  (define (as-marker x)
    (if (and (pair? x) (eq? (car x) 'from))
        (list "#%new" (cadr x))
        x))

  ;; A VERDICT THAT IS NOT `execute` IS THE ANSWER, and each kind says a
  ;; different thing to a client holding a request it may have sent once
  ;; already.
  ;;
  ;; `replay` IS THE ONE THAT PROMISES SOMETHING. It tells the client the
  ;; work is done and durable, so it passes the barrier before the word
  ;; is said -- the same table a publish goes through, because it is the
  ;; same promise. Nothing else here claims anything survives a crash:
  ;; the refusals describe a store that will not act, and flushing on the
  ;; way out of them would put the whole recovery closure on the path of
  ;; every request the store declines.
  (define (request-answer s store verdict)
    (case (car verdict)
      ((replay)
       (barrier-for store (cadr verdict))
       (list 'ok (list 'replay #t) (list 'event (cadr verdict))))
      ((complete) (cons 'error (cons 'incomplete-request (cdr verdict))))
      ((unknown) (cons 'error (cons 'unknown (cdr verdict))))
      ((req-mismatch) (cons 'error (cons 'req-mismatch (cdr verdict))))
      ((resolved-executed) (cons 'error (cons 'resolved-executed (cdr verdict))))
      (else (cons 'error (cons 'unknown (cdr verdict))))))

  ;; THE BARRIER GOES WHERE THE RECORD IS, not where the writer happens
  ;; to be now. The record a replay promises may sit in an earlier
  ;; segment, or on a generation this store has since retired -- and
  ;; flushing the current writer's current segment says nothing about
  ;; either. The supporting record is the one the decision found, so the
  ;; answer carries it and the barrier follows it.
  ;;
  ;; AND IT FAILS CLOSED. A segment that cannot be located is not a
  ;; barrier with nothing to do: the promise cannot be made, so the word
  ;; is not said. Answering `replay` after a silent no-op would be the
  ;; exact inverse of what the barrier is for.
  (define (barrier-for store event)
    (let* ((writer (car event))
           (p (guard (e (#t #f)) (discover-prefix store writer 'held-exclusive)))
           (segment (and p (segment-holding p (cdr event)))))
      (unless segment
        (assertion-violation 'barrier-for
                             "cannot locate the record a replay would promise"
                             event))
      (run-barrier! store writer segment 'commit 'commit)
      ;; AND THE REGISTRY IS TOLD, because this answer is an
      ;; acknowledgement. The attempt that first wrote this record may
      ;; have crashed between its barrier and its own reconciliation,
      ;; leaving `written` behind it -- and an acknowledged record that
      ;; the registry does not count is one a restore can take away
      ;; without the rollback gate noticing.
      (note-written-for! store writer (cdr event))))

  ;; `discovery-segment-ranges` is `((segment first last) ...)`.
  (define (segment-holding p seq)
    (let loop ((rs (discovery-segment-ranges p)))
      (cond
        ((null? rs) #f)
        ((and (<= (cadr (car rs)) seq) (<= seq (caddr (car rs)))) (car (car rs)))
        (else (loop (cdr rs))))))

;; A BATCH CAN BUILD A TREE, so an intent must be able to name a block
  ;; an earlier intent in the same batch created. The id of a new block
  ;; is derived from the sequence number it is written at, which nobody
  ;; knows until it commits -- so the caller writes `(from <n>)` for the
  ;; nth intent of this batch and the resolution happens here, where the
  ;; answers are.
  ;; THE ALTERNATIVE WAS TWO CALLS, one to make the parents and one to
  ;; make the children, which would take the lock twice and leave a
  ;; state on disk between them that no single import ever intended.
  ;; A BACK-REFERENCE IS PART OF THE SHAPE TOO. `(from)` reaches `(cadr
  ;; x)` on a one-element list and raises -- the same defect as an empty
  ;; intent, one level further in, and not reachable from the intent's
  ;; own arity because the reference sits INSIDE an argument the arity
  ;; check has already counted.
  ;;
  ;; IT ANSWERS RATHER THAN REFUSES EARLIER, because both loops already
  ;; treat an error value here as this item's answer: `(from 3)` naming
  ;; an intent that does not exist is answered the same way, and a
  ;; malformed one is the same kind of thing said worse.
  (define (resolve-from made x)
    (if (and (pair? x) (eq? (car x) 'from))
        (let ((n (proper-length x)))
          (cond
            ((or (not n) (not (= n 2)))
             (list 'error 'malformed-intent
                   (list 'back-reference-not-a-form
                         (list 'spelling (datum-spelling x)))))
            ((not (and (integer? (cadr x)) (exact? (cadr x)) (>= (cadr x) 0)))
             (list 'error 'malformed-intent
                   (list 'back-reference-not-an-index
                         (list 'spelling (datum-spelling (cadr x))))))
            (else
             (let ((e (assv (cadr x) made)))
               (if e (cdr e) (list 'error 'no-such-intent (cadr x)))))))
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

  ;; THE WRAPPER SURVIVES THE REWRITE. This rewrites an intent's
  ;; references and used to hand back the bare intent, dropping any
  ;; `expect` around it -- and it runs for EVERY insert and move, because
  ;; those are exactly the verbs that have references. So an expectation
  ;; on a move was never checked: `check-expectation` reads the wrapper,
  ;; and by then there was none.
  ;;
  ;; WHAT THAT COST: `(expect <stale> (set ...))` was refused and
  ;; `(expect <stale> (move ...))` succeeded. An optimistic caller was
  ;; told its premise had been checked for one verb and silently not for
  ;; the other -- which is worse than not offering the check at all,
  ;; because the caller writes its code as though it had it.
  (define (intent-with-refs intent parent after)
    (let* ((i (unwrap intent))
           (rewritten
             (case (car i)
               ((insert) (list 'insert parent after (cadddr i)))
               ((move) (list 'move (cadr i) parent after))
               (else i))))
      (if (and (pair? intent) (eq? (car intent) 'expect))
          (list 'expect (cadr intent) rewritten)
          rewritten)))

  ;; THE ACTOR IS PER INTENT NOW, because a request of several
  ;; sub-operations gives each its own index and they all point at one
  ;; plan. `actor-at` is handed the index and answers what that record
  ;; should carry; for a write with no request it answers the caller's
  ;; name, as it always did.
  ;; AN INTENT THAT IS NOT A FORM IS ANSWERED, NOT RAISED. Every reader
  ;; below starts with `(car i)`, which raises on anything that is not a
  ;; pair -- and a raise from in here reaches a caller as
  ;; `(error internal ...)`, which says nothing about the input that
  ;; caused it. `theourgia batch` with the text "()" produced exactly
  ;; that: one empty intent, and an internal error for a malformed
  ;; request.
  ;;
  ;; IT IS CHECKED ONCE, HERE, because this is the first thing to look
  ;; inside an intent. `resolve` further down answers an intent whose
  ;; verb it does not KNOW; a second copy of this check beside that one
  ;; would be a second place for the rule to live and a second place to
  ;; forget it.
  ;; THE SHAPE IS CHECKED BEFORE `unwrap`, because `unwrap` is itself a
  ;; reader: it asks whether the head is `expect`, which raises on
  ;; anything that is not a pair. A predicate that has to unwrap before
  ;; it can judge cannot judge the one value it exists to judge.
  ;; AND THE ARITY IS PART OF THE SHAPE. Checking only the head left
  ;; every SHORT intent raising: each arm below reaches straight for
  ;; `(cadr i)` or `(cadddr i)`, so `(insert root)` produced
  ;; `(error internal (condition "incorrect list structure ~s"))` -- the
  ;; same defect as the empty intent, one argument further in. A guard
  ;; that answers for one arity and raises for another is a guard whose
  ;; comment is wider than the check.
  ;;
  ;; THE TABLE IS THE MINIMUM LENGTH EACH VERB READS, and it is beside
  ;; nothing: the arms that do the reading are the only other place these
  ;; numbers appear, so this table and those arms have to be changed
  ;; together. `set` is the one with two lengths -- `(set id value)` and
  ;; `(set id field value)` -- and three is the shorter.
  (define intent-arity
    '((insert . 4) (set . 3) (del . 2) (move . 4)
      (link . 4) (unlink . 4) (tag . 2)))

  ;; AND THE POSITIONS WHOSE TYPE IS FIXED. Arity alone still lets
  ;; `(set <id> ((title . "x")))` through -- the right length, the wrong
  ;; thing in the field position -- and the store then raised where it
  ;; expected a symbol. A field name and a relation name are the two
  ;; places a caller writing an intent by hand naturally puts something
  ;; else, because both read like values.
  ;;
  ;; ONLY POSITIONS WITH ONE ADMISSIBLE TYPE ARE LISTED. An id is a
  ;; string or `root` or a `(from n)` reference depending on the verb,
  ;; and those already have answers further in; duplicating that
  ;; judgement here would be a second place for it to live.
  (define intent-symbol-positions
    '((set . (2)) (link . (2)) (unlink . (2))))

  ;; THE PAYLOAD IS CHECKED WITH THE REDUCER'S OWN PREDICATE, not with a
  ;; second copy of its rules living here. `payload-reason` is exported by
  ;; (theourgia reduce) and is the same procedure the reducer consults
  ;; before it applies a record -- so "the write path will not append what
  ;; the reducer cannot apply" is one fact with one owner, and the two
  ;; cannot drift into disagreeing about a verb's shape.
  ;;
  ;; THE FIELD COLLECTION, WHICH IS THE ONE THAT COULD DESTROY A STORE.
  ;; `insert` carries an alist of fields, and the reducer walks it asking
  ;; each entry for its `car`. A collection that is not a proper list of
  ;; pairs headed by symbols therefore raises IN THE REDUCER -- and the
  ;; write path appends before it reduces, so the record was already
  ;; durable by then.
  ;;
  ;; WHAT THAT COST, MEASURED: `(insert root #f (7))` encoded cleanly,
  ;; was appended, and then no reader could process it. `outline` and
  ;; every later `insert` answered an internal error; the store was
  ;; readable and writable before that one item and neither afterwards.
  ;; A record that is validly framed and cannot be reduced is the worst
  ;; thing an append-only store can be made to hold, because nothing
  ;; downstream can refuse it any more.
  (define intent-alist-positions '((insert . (3))))

  ;; THE WRITE PATH ASKS THE CALLER'S QUESTION, which is stricter than the
  ;; reducer's: `parent` and `ord` are legitimate in a record on disk and
  ;; are never legitimate coming from a caller, because the write path is
  ;; about to compute them. Same owner, two questions, asked by name.
  (define (field-alist? x) (not (caller-fields-reason x)))

  ;; AN ID POSITION HOLDS WHAT THAT POSITION MAY HOLD, and the three
  ;; kinds are not interchangeable.
  ;;
  ;; THE FIRST VERSION WAS ONE WIDE PREDICATE -- string, `root`, `#f` or
  ;; a `(from n)` reference, accepted everywhere -- on the stated ground
  ;; that the inner layers decide which belongs where. THEY DO NOT.
  ;; `(del root)`, `(del #f)`, `(del (from 0))` and `(set (from) title
  ;; "x")` all passed the boundary and then raised in the string-only
  ;; diagnostic, exactly as `(del 7)` had. The reason I gave for the
  ;; wide check was a reason I had not measured.
  ;;
  ;; BACK-REFERENCES ARE RESOLVED IN TWO POSITIONS ONLY -- an insert's
  ;; parent and predecessor, and a move's -- because those are the two
  ;; `intent-refs` rewrites. A `(from n)` anywhere else is never
  ;; substituted and reaches the reader as a list.
  (define (plain-id? x) (string? x))
  (define (parent-id? x)
    (or (string? x) (eq? x 'root) (and (pair? x) (eq? (car x) 'from))))
  (define (after-id? x)
    (or (string? x) (eq? x #f) (and (pair? x) (eq? (car x) 'from))))

  ;; `(<verb> (<position> . <predicate-name>) ...)`, one entry per
  ;; position whose admissible kinds differ from the others.
  (define intent-id-positions
    (list (cons 'insert (list (cons 1 parent-id?) (cons 2 after-id?)))
          (cons 'move (list (cons 1 plain-id?) (cons 2 parent-id?) (cons 3 after-id?)))
          (cons 'set (list (cons 1 plain-id?)))
          (cons 'del (list (cons 1 plain-id?)))
          (cons 'link (list (cons 1 plain-id?) (cons 3 plain-id?)))
          (cons 'unlink (list (cons 1 plain-id?) (cons 3 plain-id?)))))

  ;; THEY ANSWER WHICH POSITION, NOT WHETHER. A refusal has to be able to
  ;; say what it is about, and the only place that knows is the test that
  ;; failed: a caller told merely `not-an-id` has to guess which of an
  ;; intent's several id positions was meant. While these answered a
  ;; boolean, the answer compensated by carrying the whole intent back --
  ;; which is what made refusals unreadable, because the datum a refusal
  ;; is about is exactly the datum the wire layer will not carry.
  ;;
  ;; #f still means well formed, so every use of these as a test is
  ;; unchanged; an index is truthy.
  (define (id-positions-bad? u n)
    (let ((ps (assq (car u) intent-id-positions)))
      (and ps n
           (let loop ((is (cdr ps)))
             (cond ((null? is) #f)
                   ((and (> n (car (car is)))
                         (not ((cdr (car is)) (list-ref u (car (car is))))))
                    (car (car is)))
                   (else (loop (cdr is))))))))

  (define (positions-bad? u n table ok?)
    (let ((ps (assq (car u) table)))
      (and ps n
           (let loop ((is (cdr ps)))
             (cond ((null? is) #f)
                   ((and (> n (car is)) (not (ok? (list-ref u (car is)))))
                    (car is))
                   (else (loop (cdr is))))))))

  (define (proper-length x)
    (let loop ((y x) (n 0))
      (cond ((null? y) n)
            ((pair? y) (loop (cdr y) (+ n 1)))
            (else #f))))

  ;; -> #f when the intent is well formed, else a REASON.
  ;;
  ;; IT ANSWERS A REASON AND NOT A BOOLEAN, and that is not a nicety. The
  ;; rules below overlap: a caller-supplied `ord` is both a reserved name
  ;; and -- once the write path appends its own -- a repeated one, and
  ;; either rule alone refuses it. While this answered a boolean, the
  ;; refusal said only `(malformed-intent <the intent>)`, so DELETING ANY
  ;; ONE OF THESE RULES CHANGED NOTHING OBSERVABLE and every one of them
  ;; survived being removed. The reason was computed and thrown away.
  ;;
  ;; TWO RULES THAT CATCH THE SAME INPUT ARE NOT INTERCHANGEABLE when
  ;; they send an operator to different places: "you used a name the
  ;; store computes" and "you wrote that key twice" are both true of
  ;; `((ord . "x"))`, and only the first tells them what to do.
  (define (intent-reason i)
    (cond
      ((not (pair? i))
       (list 'intent-not-a-form (list 'spelling (datum-spelling i))))
      ((not (symbol? (car i)))
       (list 'verb-not-a-symbol (list 'spelling (datum-spelling (car i)))))
      ;; `expect` wraps an intent and is read as `(expect want intent)`,
      ;; so it has its own minimum before `unwrap` may be called on it.
      ;;
      ;; AND ITS HASH IS CHECKED FOR TYPE, NOT MERELY FOR PRESENCE.
      ;; `expectation` answers the value it finds, so a wrapper holding
      ;; #f was indistinguishable from no wrapper at all and the check
      ;; SILENTLY DID NOT RUN -- `(expect #f (set <id> title "Two"))`
      ;; wrote. A caller that asks for a premise to be verified and is
      ;; given no answer either way is worse off than one that never
      ;; asked, because it will not look again.
      ((and (eq? (car i) 'expect) (not (proper-length i)))
       (list 'intent-not-a-proper-list (list 'verb 'expect)))
      ((and (eq? (car i) 'expect) (< (proper-length i) 3))
       (list 'expect-too-short (list 'given (- (proper-length i) 1))))
      ((and (eq? (car i) 'expect) (not (string? (cadr i))))
       (list 'expectation-not-a-hash
             (list 'spelling (datum-spelling (cadr i)))))
      (else
       (let ((u (unwrap i)))
         (cond
           ((not (pair? u))
            (list 'intent-not-a-form (list 'spelling (datum-spelling u))))
           ((not (symbol? (car u)))
            (list 'verb-not-a-symbol (list 'spelling (datum-spelling (car u)))))
           (else
            (let ((n (proper-length u))
                  (need (assq (car u) intent-arity)))
              (cond
                ;; AN IMPROPER LIST IS NOT A SHORT ONE. `(set "a" title
                ;; "x" . junk)` has every argument it needs and is still
                ;; not a form; answering `too-few-arguments` sends the
                ;; caller to add an argument, which cannot help. The
                ;; payload validator already separates these two, and the
                ;; two validators should not describe the same defect
                ;; differently.
                ((not n) (list 'intent-not-a-proper-list (list 'verb (car u))))
                ((and need (< n (cdr need)))
                 (list 'too-few-arguments (list 'verb (car u))
                       (list 'given (- n 1)) (list 'needs (- (cdr need) 1))))
                ((positions-bad? u n intent-symbol-positions symbol?)
                 => (lambda (k)
                      (list 'name-not-a-symbol (list 'argument k)
                            (list 'spelling (datum-spelling (list-ref u k))))))
                ((id-positions-bad? u n)
                 => (lambda (k)
                      (list 'not-an-id (list 'argument k)
                            (list 'spelling (datum-spelling (list-ref u k))))))
                ((and (assq (car u) intent-alist-positions) n (> n 3))
                 (caller-fields-reason (list-ref u 3)))
                (else #f)))))))))

  (define (malformed-intent? i) (and (intent-reason i) #t))

  (define (run-intents! s state actor-at intents)
    (let loop ((is intents) (n 0) (made '()) (out '()))
      (if (null? is)
          (reverse out)
          (let* ((raw (car is))
                 (rs (if (malformed-intent? raw) '() (intent-refs raw)))
                 (fixed
                   (if (null? rs)
                       raw
                       (let ((p (resolve-from made (car rs)))
                             (a (resolve-from made (cadr rs))))
                         (cond
                           ((and (pair? p) (eq? (car p) 'error)) p)
                           ((and (pair? a) (eq? (car a) 'error)) a)
                           (else (intent-with-refs raw p a))))))
                 (answer (cond
                           ((intent-reason raw)
                            => (lambda (why)
                                 (list 'error 'malformed-intent why)))
                           ((and (pair? fixed) (eq? (car fixed) 'error)) fixed)
                           (else
                     ;; A RAISE IN ONE SUB-OPERATION IS THAT SUB-OPERATION'S
                     ;; ANSWER. The loop already stops at the first failed
                     ;; intent, and stopping with an answer keeps the ones
                     ;; that succeeded before it -- a raise instead threw
                     ;; away the whole request's answers, including the
                     ;; `ok`s for records already on the disk.
                     ;;
                     ;; AND IT ASKS THE SAME QUESTION THE OUTER ONE DOES,
                     ;; because it is reached first and would otherwise
                     ;; answer on the outer one's behalf. `unknown` is
                     ;; owed only where bytes may be on the disk; with
                     ;; none, this hands the failure on rather than
                     ;; dressing it as a request whose outcome is in
                     ;; doubt. An inner guard that decided for itself
                     ;; would make the gate above unreachable.
                     (guard (e (#t (if (session-write-started? s)
                                       (list 'error 'unknown
                                             (list 'interrupted (failure-text e)))
                                       (raise e))))
                       (one-intent! s state (actor-at n) fixed))))))
            ;; A FAILED INTENT STOPS THE REST. Later intents were written
            ;; against a state this one was meant to produce; running them
            ;; anyway asks each to be judged against a history its author
            ;; did not have.
            (if (eq? (car answer) 'error)
                (reverse (cons answer out))
                (loop (cdr is) (+ n 1)
                      ;; THE ID COMES FROM THE EVENT, NOT FROM THE REPORT.
                      ;; Which block an insert made is decided by the
                      ;; record's own coordinates and is knowable the
                      ;; moment it commits; the hashes beside it are a
                      ;; description built afterwards, and that
                      ;; description is allowed to be missing. Reading
                      ;; the id out of it made a failure to DESCRIBE a
                      ;; write delete a block: the back-reference then
                      ;; resolved to nothing, the intent naming it
                      ;; answered `no-such-intent`, and the caller was
                      ;; told one of its own blocks had never been asked
                      ;; for.
                      (let ((ev (cadr (assq 'events (cdr answer)))))
                        (if (and (eq? 'insert (car (unwrap fixed))) (pair? ev))
                            (cons (cons n (block-id (car (car ev)) (cdr (car ev)))) made)
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
                    (cond
                      ((eq? (car payload) 'error) payload)
                      ;; THE PAYLOAD IS CHECKED, NOT ONLY THE INTENT. The
                      ;; boundary above judges what the CALLER wrote; this
                      ;; judges what `resolve` produced from it, which is
                      ;; the thing about to be appended. They are not the
                      ;; same value: `(tag 7)` is an intent of the right
                      ;; length whose payload carries a name no reader can
                      ;; sort, and it was appended and acknowledged because
                      ;; only the caller's fields were being examined.
                      ;;
                      ;; SAME PREDICATE THE REDUCER USES, so "appended"
                      ;; and "applicable" cannot come apart.
                      ((caller-payload-reason payload)
                       => (lambda (why)
                            (list 'error 'malformed-intent
                                  (if (pair? why) why (list why)))))
                      (else
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
                              (write-outcome->answer outcome seq)
                              (begin
                                ;; THE ACTOR GOES IN HERE TOO -- see the
                                ;; note in `append-payload!`.
                                (reduce-apply! state writer seq deps payload actor)
                                (session-applied! s (session-epoch s)
                                                  (reduce-applied-cut state))
                                (list 'ok
                                      (list 'events (list (cons writer seq)))
                                      (state-section state payload writer seq)
                                      (list 'cursor (cons writer seq))
                                      (list 'replay #f))))))))))))))

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
                        (let ((result (session-snapshot!
                                        s (list v (reduce-applied-cut state) (state->rows state)))))
                          ;; A failed derived checkpoint cannot revoke the snapshot.
                          (when (eq? (car result) 'written)
                            (guard (e (#t #f)) (index-checkpoint! store)))
                          result))))))
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

  ;; THE INNER READ CANNOT BE ALLOWED TO KEEP THE OUTER SESSION OPEN. The
  ;; check holds a shared session for its own scan and opens a second one
  ;; to reduce the log; a failure in the second used to unwind past the
  ;; first, leaving its descriptor and its shared lock held for the life
  ;; of the process -- so a later writer waiting for exclusive access
  ;; would wait on a session nobody was using.
  ;; NEVER: ONE PLACE BUILDS THE PARTIAL ANSWER, and every route that can
  ;; produce one comes through it -- the `batch` verb, a plan completed from
  ;; a previous request, a commit, and the two imports. Four sites assembling
  ;; the same shape would make the count and the list two suppliers of one
  ;; fact, free to disagree; here the count is a FUNCTION of the list beside
  ;; it and cannot be.
  ;;
  ;; NEVER: `done` IS THE NUMBER OF SUB-OPERATIONS THAT ANSWERED `ok`.
  ;;
  ;; TODAY EVERY ROUTE STOPS AT THE FIRST FAILURE. All four hand their
  ;; intents to `with-store-write`, which runs them through `run-intents!`,
  ;; and that returns as soon as one answers `error` -- so every list here
  ;; is a run of `ok` followed by at most one `error`, and the count of
  ;; `ok`s is also the length of the leading run. An earlier version of this
  ;; comment said a commit or an import kept going; measured, they do not.
  ;;
  ;; The definition is the COUNT rather than the leading run so that it
  ;; stays correct if some route later does continue past a failure.
  ;;
  ;; NEVER: `done` BELONGS TO THIS ANSWER AND IS NOT A POSITION IN THE
  ;; REQUEST. An earlier version of this comment said that while every route
  ;; stops at the first failure, resuming from n+1 is well defined. It is not,
  ;; on the route that completes a plan an earlier request left behind: that
  ;; answer counts the sub-operations THIS completion applied, and the ones
  ;; the first attempt had already written are not in it. Adding one to this
  ;; number and indexing the original request's intents with it would skip
  ;; whatever the first attempt did. What to do next is read from the answer's
  ;; own entries, never computed from n.
  ;; NEVER: THREE OF THE FOUR ROUTES HAVE NO CELL THAT CAN GO RED. `commit`,
  ;; `import-code` and `import-datum` call this function, and their partial
  ;; answers carry `done` -- but nothing measures it, because reaching their
  ;; partial branch is harder than it looks. All three validate every item
  ;; BEFORE the per-item loop, so the cheap failures never get there:
  ;;
  ;;   two blocks, one undrafted   -> (error no-draft (blocks "...2"))
  ;;   a version for only one      -> (error bad-request req-needs-versions ...)
  ;;   both versions, one wrong    -> (error working-version-changed (blocks ...))
  ;;
  ;; Each refuses the whole request. The partial branch is reached only by a
  ;; failure that happens DURING the loop, which needs a frozen plan whose
  ;; versions no longer match -- one apparatus, shared by all three, recorded
  ;; as F31. THE ABSENCE IS DELIBERATE AND THIS IS WHERE IT IS WRITTEN DOWN;
  ;; the `batch` verb's own rows are in `cli1.sc`.
  ;; NEVER: A REPLAY RECEIPT IS NOT A LIST OF SUB-OPERATIONS AND CARRIES NO
  ;; COUNT. A request whose whole work was already applied is answered with
  ;; ONE request-level receipt, `(ok (replay #t) (event …))`, and counting
  ;; that receipt said `(done 1)` for a request that had written three --
  ;; measured, before this clause existed. `(replay #t)` ALREADY SAYS every
  ;; sub-operation of that request is written, so there is nothing for a
  ;; count to add; a request seen but only partly applied does not answer
  ;; `replay #t` at all, it is finished by the completion path, and THAT
  ;; answer is a list of sub-operations and does carry `done`.
  ;;
  ;; The test stays here rather than at the call site so that this remains
  ;; the only place that builds the shape.
  ;; NEVER: THE TEST IS THE EXACT SHAPE, NOT "HAS A REPLAY CLAUSE". This
  ;; function now reads its argument two ways -- a list of sub-operations, or
  ;; the single receipt that `request-answer` builds at the one call site in
  ;; `commit-then` -- and the only safe way to tell them apart is to insist
  ;; on the whole shape that site produces: one item, `ok`, then exactly
  ;; `(replay #t)` and `(event …)` and nothing else. A sub-operation that
  ;; happened to mention `replay` must not be able to suppress the count.
  ;;
  ;; NEVER: AND THE EVENT CLAUSE IS READ THROUGH, NOT JUST ITS TAG. `(event …)`
  ;; with anything after it, or carrying something other than a
  ;; (writer . seq) pair, is not what that site builds -- and every shape this
  ;; predicate accepts is a shape whose count disappears.
  (define (replay-receipt? items)
    (and (pair? items)
         (null? (cdr items))
         (let ((item (car items)))
           (and (pair? item)
                (eq? (car item) 'ok)
                (pair? (cdr item))
                (pair? (cddr item))
                (null? (cdddr item))
                (let ((a (cadr item))
                      (b (caddr item)))
                  (and (pair? a)
                       (eq? (car a) 'replay)
                       (pair? (cdr a))
                       (null? (cddr a))
                       (eq? (cadr a) #t)
                       (pair? b)
                       (eq? (car b) 'event)
                       (pair? (cdr b))
                       (null? (cddr b))
                       (pair? (cadr b))
                       (string? (car (cadr b)))
                       (integer? (cdr (cadr b)))))))))

  ;; NEVER: WHETHER A LIST IS THE PLAN RECEIPT CANNOT BE READ OFF ITS SHAPE.
  ;; A `tag` intent touches no block, so `block-ids-of` answers `()` for it and
  ;; one-intent! builds EXACTLY the shape the empty-plan receipt has:
  ;;
  ;;   (ok (events ((w . 1))) (state ()) (cursor (w . 1)) (replay #f))
  ;;
  ;; Measured: a one-item batch of a single tag answered `(done 0)` while that
  ;; tag had succeeded. Tightening the shape test cannot fix this, because the
  ;; shapes are the same -- the fact needed is WHERE THE LIST CAME FROM, and
  ;; only the caller knows it. `run-batch` holds the intents, so it says.
  (define (batch-answer items . rest)
    (let ((no-intents? (and (pair? rest) (car rest))))
      (cond
        ((replay-receipt? items) (list 'batch items))
        (else
         (list 'batch items
               (list 'done
                     (if no-intents?
                         0
                         (let loop ((is items) (n 0))
                           (cond
                             ((null? is) n)
                             ((and (pair? (car is)) (eq? (car (car is)) 'ok))
                              (loop (cdr is) (+ n 1)))
                             (else (loop (cdr is) n)))))))))))
  (define (store-check store)
    (let ((ls (log-open store)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda () (store-check-with store ls))
        ;; `load-commit!` ON A SESSION ALREADY FINISHED IS THE ONE THING
        ;; THIS MUST NOT TURN INTO A NEW FAILURE, so it is guarded: the
        ;; inner read may have closed things on its way out, and a
        ;; cleanup that raised would replace the answer -- or the
        ;; original failure -- with one manufactured here.
        (lambda () (guard (e (#t #f)) (load-commit! ls))))))

  (define (store-check-with store ls)
    (let* ((ignored #f)
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
           ;; WHAT THE REDUCTION COULD NOT APPLY IS DAMAGE TOO. The rows
           ;; above read the bytes -- torn tails, checksums, things the
           ;; scan can see without understanding them. A record that
           ;; reads perfectly and contradicts the plan above it is
           ;; invisible to all of that, and it is the store holding
           ;; something it cannot show: two records in one slot, or a
           ;; record whose plan never declared the slot it claims. An
           ;; operator running `check` on such a store was told `ok`.
           (notes (reduce-noted (open-and-reduce store)))
           (damaged? (exists (lambda (w) (pair? (cadr (assq 'integrity (cdr w)))))
                             per-writer)))
      (append
        (list 'check
              (list 'store (or (store-id-of store) 'unknown)))
            ;; NEVER: A STORE CAN HAVE WRITERS AND NONE OF THEM THIS MACHINE'S.
            ;; `local` is not about the machine: a writer is local when
            ;; `writers/<id>/owner.sexp` exists INSIDE THE STORE, and that
            ;; file travels with the store. So a copy received from
            ;; somewhere else has writers, has history, and has nobody here
            ;; to write as -- which is the case a client with a cursor to
            ;; place needs told apart from "there is one, and it is this".
            ;;
            ;; NEVER: WHICH ALSO MEANS A PLAIN COPY STILL REPORTS ITS WRITER
            ;; AS LOCAL. Copying a directory tree carries `owner.sexp` along
            ;; with everything else, so the copy names the same writer as the
            ;; original -- two stores, both saying that writer is this
            ;; machine's. What makes a writer somebody else's is arriving
            ;; through the publish path, which writes `published.sexp` and no
            ;; owner file. This field answers "is there a writer here I may
            ;; write as", not "did this store come from somewhere else".
            ;;
            ;; NEVER: AND WHEN THERE IS NONE THE CLAUSE IS ABSENT ALTOGETHER.
            ;; Not "", not 0, not `none`: a reader that finds the clause can
            ;; use it, and one that does not has to ask rather than guess. A
            ;; spelled-out empty value is the shape that gets used by
            ;; accident.
        (let ((local (local-writer-of store ls)))
          (if local (list (list 'local-writer local)) '()))
        (list
            (list 'writers per-writer)
            (list 'snapshots snapshots)
            ;; REPORTED SEPARATELY FROM WRITER DAMAGE, because it is not
            ;; damage: nothing in the store is wrong. What is wrong is
            ;; where the registry was put, and the store cannot see it
            ;; from the inside -- which is exactly why it is worth
            ;; saying out loud.
            (list 'registry (if (registry-inside-store?) 'inside-store 'outside-store))
            (list 'notes notes)
            (list 'verdict (if (or damaged? (pair? notes) (registry-inside-store?))
                               'damaged 'ok))))))

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
