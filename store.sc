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
  (export store-resident-cache! open-and-reduce with-store-write premises-preflight premises-also intent-ref-positions
          judgement-refusal?
          premises-gate? premises-gate-check premises-gate-enter store-raise-answer-hook! store-publish-hook!
          obtain-state seal-state sealed-state? sealed-state-state sealed-state-notes sealed-state-unsealed? sealed-state-name store-withhold-hook!
          state-incomplete-notes
          ;; The interface pinned at dispatch (F77c; cells v3e look these up
          ;; in this library): re-exported from (theourgia incomplete).
          incomplete-accepted incomplete-reduction? incomplete-reduction-notes
          defs-index defs-index-build-count defs-index-skipped-count name-bearing-kinds
          text-decode-skipped-count prepared-generation-count prepared-token-now
          prepared-hit-count prepared-miss-count
          store-init! nearest-ids store-snapshot!
          batch-answer
          datum-mode-block? draft-on-datum-refusal
          store-check store-adopt! store-search search-state field-strings store-search-report search-hit-limit store-grep store-refs store-log store-tags parse-cut store-diff store-state-at-cut store-conflicts store-evidence
          library-locator
          make-write-request write-request? store-successors store-intervals
          request-verdict failure-text)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs conditions) (rnrs io ports) (rnrs files)
          (only (theourgia md) md-refs)
          (only (theourgia template-read) template-problem)
          (only (rnrs eval) eval environment)
          (theourgia request)
          (theourgia evidence-index)
          (only (theourgia wire) decode-line storable-decode)
          (rnrs arithmetic fixnums) (rnrs unicode) (rnrs bytevectors) (rnrs hashtables)
          (only (rnrs records syntactic) define-record-type fields mutable)
          (only (theourgia log)
                log-open load-deliver! load-commit! load-fingerprint remember-load-unreadable!
                load-unreadable session-load
                unreadable-behind merge-unreadable
                tell-load-notes! tell-load-refused! load-declaration-of
                session-delivered? session-reset-pending load-outcome
                load-snapshot-cut load-snapshot-rows
                log-begin log-end! session-view session-view-refusal session-append! session-applied!
                session-epoch make-frame atomic-write! segment-file-name
                session-snapshot! log-open load-writers load-prefix load-commit! local-writer-of
                discovery-end-seq discovery-integrity discovery-torn discovery-origin
                enumerate-segment-files discovery-quarantine discover-prefix
                manifest-segments read-manifest
                snapshot-read snapshot-cut-supported? segment-file-number
                log-error-kind log-error-segment log-error-offset log-error-detail
                store-id-of adopt! registry-inside-store?
                instance-install! owner-install! writer-directory store-writers
                store-register!
                uncertain-load run-barrier! retired-successor retired-of
                session-commit! session-pending-count-set! note-written-for!
                session-dry-clone session-dry?
                session-write-started? session-written-events
                session-writer discovery-physical-current discovery-segment-ranges
                view-revision view-epoch view-writer view-expect-seq)
          (only (theourgia answers) with-written)
          (only (theourgia incomplete) declared? make-incomplete-reduction incomplete-refused refusing-notes
                incomplete-accepted incomplete-reduction? incomplete-reduction-notes)
          (only (theourgia ffi) mutation-record)
          (only (theourgia ffi) mkdir-p! wall-clock-ms process-id directory-entries
                file-is-directory? report-fault? trace-event! entry-type overwrite-entry!
                hold-point!)
          (only (theourgia digest) sha256 bytevector->hex)
          ;; NEVER: THE NAMES A BLOCK DEFINES ARE DERIVED, NOT STORED.
          ;; `code-project.sc` says it outright: a code block's `name` is
          ;; derived from its source and is not in the stored fields at all.
          ;; Search and `whereis` both need those names, so both read the
          ;; view -- one derivation, two callers. No cycle: `view` imports
          ;; `reduce`, `languages`, `text-code` and `datum-code`, and never
          ;; this library.
          ;;
          ;; Measured on a store of this tree's own sources, 906 blocks:
          ;; `view-read` over every block takes 2 ms, the same order as
          ;; `state-read`. Deriving names is not what makes search cost
          ;; anything, so nothing is cached for that reason.
          (only (theourgia view) view-read)
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
  ;; AN UNREADABLE WRITER HAS NO FRONTIER TO BE COMPLETE AT. Its end is not
  ;; known, so the reduction is not kept; asking for its end raises, and
  ;; that would fail a read the other writers answer -- only where the
  ;; resident cache is on, which is the daemon.
  ;;
  ;; AND A LOAD THAT COULD NOT READ SOMETHING IS NOT COMPLETE ANYWHERE: a
  ;; writer stopped by a segment it cannot read keeps its origin and its
  ;; readable end, but what lies past the stop is unknown (K11).
  (define (complete-frontier? state ls)
    (and (null? (load-unreadable ls))
         (for-all (lambda (writer)
                    (let ((p (assoc writer (reduce-applied-cut state)))
                          (prefix (load-prefix ls writer)))
                      (and (not (eq? (discovery-origin prefix) 'unreadable))
                           (= (if p (cdr p) 0) (discovery-end-seq prefix)))))
                  (load-writers ls))))
  (define (replay store cut . declaration)
    (let* ((ls (log-open store (and (pair? declaration) (car declaration))))
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
      ;; THE REDUCTION REMEMBERS WHAT ITS LOAD COULD NOT READ, for answers
      ;; built from it after the load is gone: the daemon answers reads
      ;; from a reduction it published earlier (K10).
      (remember-load-unreadable! r ls)
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
  ;;
  ;; THE UNDECLARED FORM (F77c). open-and-reduce says nothing about
  ;; accepting a reduction that is missing a writer, so on an incomplete
  ;; store it is refused -- unless the request it runs in declared (plan
  ;; amendment A2: the declaration rides on the store object the dispatcher
  ;; owns). A caller that has not been handed a declaration fails CLOSED.
  (define (open-and-reduce store . rest)
    (apply obtain-state store #f #f rest))

  ;; ---- obtaining state (F77c, F77 R2e) --------------------------------------
  ;;
  ;; A SUPPLIED STATE IS HANDED ON SEALED. The daemon's published state, and
  ;; any state a caller gives rpc dispatch, reach a handler as a sealed
  ;; value holding the reduction and its notes. Nothing that reads a
  ;; reduction accepts a seal, so a handler that used one without unsealing
  ;; it fails on the first row that reaches it, not quietly. POSSESSING A
  ;; STATE IS NOT CONSUMING IT (design review r2): a verb that never unseals
  ;; is never refused for holding one, and its answer carries no clause.
  ;; NAME is the publication the state is, (<revision> <token>), when the
  ;; daemon sealed it from its cell; #f for any other seal. read --rev takes
  ;; it from here, so the rev it answers is the state its answer was built
  ;; from, whoever dispatched.
  (define-record-type sealed-state (fields state notes (mutable unsealed?) name))
  ;; The interface name (F77c): a reduction's notes, '() when it is complete.
  ;; Defined here and not in (theourgia log): the cells import the log library
  ;; whole and look this name up for themselves.
  (define (state-incomplete-notes value) (unreadable-behind value))
  (define (seal-state state extra-notes . name)
    (make-sealed-state state (merge-unreadable (unreadable-behind state) extra-notes) #f
                       (and (pair? name) (car name))))

  ;; THE ONE UNSEALER, AND THE ONE PLACE A CONSUMER OBTAINS STATE.
  ;; (obtain-state store supplied declaration . cut)
  ;;   supplied: a sealed state, a bare reduction, or #f (then it loads)
  ;;   declaration: incomplete-accepted, or anything else for "not declared";
  ;;     the request's own declaration (A2) counts as well
  ;; A state missing a writer is handed to a declared consumer, which tells
  ;; the request's listener what it is missing -- exactly as a load does, so
  ;; a nested dispatch's unsealing reaches the enclosing answer -- and is
  ;; refused to any other, the refusal told to the listener first.
  ;; A CUT IS A DIFFERENT REDUCTION: it is always loaded, never taken from
  ;; a supplied state (see reduction-for in the rpc layer).
  (define (obtain-state store supplied declaration . rest)
    (let ((cut (if (null? rest) #f (car rest))))
      (if (and supplied (not cut))
          (let* ((is-sealed (sealed-state? supplied))
                 (state (if is-sealed (sealed-state-state supplied) supplied))
                 (notes (if is-sealed (sealed-state-notes supplied) (unreadable-behind supplied))))
            (when is-sealed (sealed-state-unsealed?-set! supplied #t))
            (let ((refusing (refusing-notes
                              notes
                              (or (eq? declaration incomplete-refused)
                                  (eq? (load-declaration-of store) incomplete-refused)))))
              (cond
                ((null? notes) state)
                ((or (declared? declaration) (declared? (load-declaration-of store))
                     (null? refusing))
                 (tell-load-notes! store notes)
                 state)
                (else
                 ;; Refused by the refusing notes, naming every note the
                 ;; state holds (as open-load does).
                 (let ((c (make-incomplete-reduction notes)))
                   (tell-load-refused! store c)
                   (raise c))))))
          (if (not cut)
              (replay store #f declaration)
              (let* ((whole (replay store #f declaration))
                     (verdict (cut-usable? whole cut)))
                (if (eq? verdict 'usable)
                    (replay store cut declaration)
                    verdict))))))

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

  ;; THE NORMALISED FORM OF A TEXT, COMPUTED ONCE PER GENERATION.
  ;;
  ;; `token-tier` used to normalise its text on every call -- `prepare`, then
  ;; `fold-preserving`, and a third pass over the raw text in the fallback --
  ;; and it is called once per BLOCK per FIELD per QUERY TOKEN. Measured on a
  ;; store of 3531 blocks holding 3,446,421 characters of searched text: of
  ;; the 391 ms a single-token query spent deciding tiers, 275 ms was
  ;; normalising text that had not changed. A four-token query did it four
  ;; times over.
  ;;
  ;; TWO KEYS, AND THEY ARE NOT THE SAME KIND OF KEY. The generation is
  ;; keyed by the APPLIED CUT, like `defs-index` -- object identity is wrong
  ;; for that and this batch has the two readings that say why, one in each
  ;; direction. Inside a generation the entry is keyed by the string OBJECT,
  ;; which is sound for a different reason: while the cut is unchanged the
  ;; reduction is the same object graph, so a field's value is the same
  ;; string every time it is fetched. A worse case than staleness is not
  ;; possible here -- a missed hit costs a recomputation and nothing else.
  ;;
  ;; THE ENTRY HOLDS WHAT THE THREE PASSES PRODUCED, and the third is filled
  ;; only if something asks: the raw fold is needed by the fallback for
  ;; composed characters, which most queries never reach.
  ;;
  ;; A CACHED STRING IS NEVER MUTATED. Field values come out of the
  ;; reduction and nothing writes into them; if that ever stops being true
  ;; this table is wrong, and that is why the sentence is here.
  ;; WHAT THIS TABLE CAN AND CANNOT REUSE, measured rather than assumed.
  ;;
  ;; The first two designs were both keyed for reuse that does not exist.
  ;; `state-read` calls `copy-datum` on every field value, so a field's
  ;; text is a FRESH string on every read -- checked across 401 blocks of a
  ;; real store, two reads of one resident reduction: 0 shared objects, 401
  ;; fresh copies. The same query run five times against that reduction
  ;; missed 6733 times on every run, the same number each time.
  ;;
  ;; (The check that first said otherwise asked ONE block, and that block
  ;; had no title: it compared #f with #f and read "absent" as "shared".)
  ;;
  ;; So nothing survives a read, and a table kept across searches would
  ;; hold a whole store's normalised text that can never be hit again --
  ;; measured at 254 MB still held after five queries and a full
  ;; collection, against 58 MB when the table is dropped.
  ;;
  ;; THE REUSE THAT DOES EXIST IS WITHIN ONE SEARCH. `field-strings` is
  ;; called once per block per field, and every query token is then asked
  ;; against that same string. That is what the table is for, and it is
  ;; what the numbers show: the per-token cost of a query fell from 201 ms
  ;; to 39 ms, while a single-token query -- which has no second token to
  ;; reuse anything -- moved much less.
  ;;
  ;; WHAT THE MEASUREMENTS ABOVE DO AND DO NOT SETTLE, because the first
  ;; version of this note claimed more than they support.
  ;;
  ;; They settle that a table keyed by the string OBJECT cannot be reused
  ;; across searches: the copy makes every read a new object, so the key is
  ;; never the same twice. They also explain the 254 MB -- an object-keyed
  ;; table admits every fresh copy as a new entry, so it grew by a whole
  ;; store's text per query.
  ;;
  ;; They say NOTHING about a table keyed by (block id, field name) and
  ;; dropped when the applied cut moves. That table holds one entry per
  ;; field however many times the field is read, so the copy does not reach
  ;; it: the copy defeats object identity, not a lookup by name. It has not
  ;; been tried here and it is not ruled out; it is written up as an input
  ;; to the work on search candidates, where it has to be decided together
  ;; with any index, because two tables over the same text with two
  ;; invalidation rules is the arrangement that goes wrong quietly.
  ;;
  ;; WHY IT IS NOT DONE HERE. It needs a key the per-search table does not
  ;; carry, and it buys nothing at all for a one-shot CLI command, where
  ;; the building and the saving are settled inside one process. Its case
  ;; is a long-lived process answering repeated queries.
  (define prepared-table (make-eq-hashtable))
  (define prepared-generations 0)
  (define (prepared-generation-count) prepared-generations)
  ;; A TOKEN PER TABLE, AND IT EXISTS FOR ONE ROW IN THE FIXTURES.
  ;;
  ;; This is not a product interface. `cli3.sc`'s N6b asks that two searches
  ;; did not share a table, and it has to ask about IDENTITY rather than
  ;; about a count: the review was asked whether a count could be evaded and
  ;; answered with a change small enough to apply -- delete the line that
  ;; replaces the table, keep the line that increments the counter, and a
  ;; module-level table survives every search while all four rows stay
  ;; green. A token that changes when the table is replaced cannot be
  ;; separated from the table that way.
  (define prepared-token (list 'prepared))
  (define (prepared-token-now) prepared-token)
  (define (prepared-begin!)
    (set! prepared-table (make-eq-hashtable))
    (set! prepared-token (list 'prepared))
    (set! prepared-generations (+ prepared-generations 1)))
  ;; AND THE TABLE IS LET GO WHEN THE SEARCH RETURNS, rather than when the
  ;; next search replaces it.
  ;;
  ;; Replacing it at the start of the next search is enough to bound what
  ;; the table can grow to, and that is all the first version did -- so a
  ;; process that answered a query and then waited went on holding the whole
  ;; of that query's normalised text. Measured on 3531 blocks holding
  ;; 3,446,421 characters, with the reduction warmed separately so the
  ;; reading is about the table and not about the store: 56,952,080 bytes
  ;; held between two searches, against 320,656 bytes when the table is
  ;; emptied on the way out. Fifty-four megabytes, held for nothing, in
  ;; exactly the long-lived process the table was least able to help.
  (define (prepared-end! result)
    (hashtable-clear! prepared-table)
    result)
  (define raw-fold-not-yet (string->symbol "raw-fold-not-yet"))
  ;; THE TABLE SAYS WHETHER IT IS WORKING. A cache that is never hit costs
  ;; memory and buys nothing, and it looks exactly like one that is working
  ;; unless something counts. These two are what a row asks.
  (define prepared-hits 0)
  (define prepared-misses 0)
  (define (prepared-hit-count) prepared-hits)
  (define (prepared-miss-count) prepared-misses)
  (define (prepared-of text)
    (let ((found (hashtable-ref prepared-table text #f)))
      (if found
          (begin (set! prepared-hits (+ prepared-hits 1)) found)
          (let* ((t (prepare text))
                 (v (vector t (fold-preserving t) raw-fold-not-yet text)))
            (set! prepared-misses (+ prepared-misses 1))
            (hashtable-set! prepared-table text v)
            v))))
  (define (prepared-raw-fold v)
    (let ((cached (vector-ref v 2)))
      (if (eq? cached raw-fold-not-yet)
          (let ((computed (fold-preserving (vector-ref v 3))))
            (vector-set! v 2 computed)
            computed)
          cached)))

  (define (token-tier text token)
    (let* ((tv (prepared-of text))
           (qv (prepared-of token))
           (t (vector-ref tv 0))
           (folded (vector-ref tv 1))
           (q (vector-ref qv 1))
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
               (or best (and (substring-at? (prepared-raw-fold tv)
                                            (prepared-raw-fold qv))
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
  ;; boundary beats inside beats nothing; the first boundary settles it.
  (define (collapse-field-row row)
    (let loop ((l row) (out #f))
      (cond ((null? l) out)
            ((eq? (car l) 'boundary) 'boundary)
            ((eq? (car l) 'inside) (loop (cdr l) 'inside))
            (else (loop (cdr l) out)))))

  ;; exact beats prefix beats nothing.
  (define (collapse-name-row row)
    (let loop ((l row) (out #f))
      (cond ((null? l) out)
            ((eq? (car l) 'exact) 'exact)
            ((eq? (car l) 'prefix) (loop (cdr l) 'prefix))
            (else (loop (cdr l) out)))))

  ;; Walks the rows in step, one token position at a time: every position
  ;; must have some field that took it. The rows are the same length by
  ;; construction -- each is one entry per query token.
  (define (every-token-hit? rows)
    (let loop ((rs rows))
      (cond ((null? (car rs)) #t)
            ((exists car rs) (loop (map cdr rs)))
            (else #f))))

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
  ;; NEVER: A SHAPE IS CHECKED, NOT CAUGHT. A field in conflict is
  ;; `(conflict ((<value> <writer> <seq>) ...))` and this took the second
  ;; element and mapped `car` over it -- which raises on anything else, and
  ;; `set <id> title (conflict)` is accepted by the write path. `store-search`
  ;; reads titles, srcs and keywords through here, from `state-read`, so a
  ;; single such field took down every query in the store. It is NOT behind the
  ;; guarded read: that one covers the view, and this does not go through it.
  ;;
  ;; The repair is to test the shape rather than to wrap it in a `guard`, and
  ;; the difference matters. A raise here would mean "we wrote the branch
  ;; wrong" just as readily as "the data is odd", and catching it would file
  ;; both under the same count -- so a real mistake of ours would hide inside a
  ;; number that is supposed to mean "a block could not be read".
  ;;
  ;; For the same reason a field whose shape does not fit contributes NOTHING
  ;; and is not counted in `defs-index-skipped`: that counter means a BLOCK
  ;; could not be read, and mixing "this one field looks strange" into it would
  ;; leave both of its bounds describing nothing.
  ;;
  ;; TWO PRODUCERS SHARE THE TAG AND DO NOT SHARE THE SHAPE. `reduce.sc` builds
  ;; `(conflict (<triple> ...))` for a field, and `(conflict <count>)` for a
  ;; POSITION. They never meet today -- a position is not a field -- but the
  ;; count form would have raised here in exactly the same way, and the test
  ;; below rejects it rather than relying on them staying apart.
  ;; KEY: THE SHAPE SAYS HOW TO READ A CONFLICT, NEVER WHETHER THERE IS ONE:
  ;; field-strings asks it only of a field the reducer says is contested.
  (define (conflict-candidates value)
    (and (pair? value)
         (eq? (car value) (quote conflict))
         (pair? (cdr value))
         (let ((cs (cadr value)))
           (and (list? cs)
                (for-all pair? cs)
                cs))))

  ;; BYTES BECOME TEXT HERE, WHICH IS WHERE THEY WERE BEING LOST.
  ;;
  ;; A code block imported in text mode holds its source as a bytevector.
  ;; This function had no branch for one, so it fell to `else` and answered
  ;; the empty list -- and every verb that reads a field as text reads it
  ;; through here. That is the whole mechanism of "a text block's source is
  ;; not searched": nothing declined to search it, the value was thrown away
  ;; one step before the search ever saw it.
  ;;
  ;; VALIDITY IS DECIDED BY A ROUND TRIP, not by a scanner written here.
  ;; `utf8->string` never raises: it substitutes U+FFFD for any byte it
  ;; cannot read, so "it decoded" is not the same question as "those bytes
  ;; were text". Re-encoding the result and comparing answers that question
  ;; exactly, and it is right in the case a hand-written check gets wrong --
  ;; a source that genuinely contains U+FFFD re-encodes to the bytes it came
  ;; from and is text, while a stray #xFF does not and is not.
  ;;
  ;; This directory already holds six partial readers of one syntax or
  ;; another, and a seventh -- a UTF-8 validator -- would have to be right
  ;; about overlong forms, surrogates and truncation to be worth more than
  ;; the round trip that is right about all of them by construction.
  ;;
  ;; A FIELD THAT IS NOT TEXT IS SKIPPED AND COUNTED, under its own counter.
  ;; `defs-index-skipped` means a BLOCK could not be read; this means a
  ;; block was read perfectly well and one field of it is not text. One
  ;; counter, one fact.
  ;;
  ;; IT COUNTS ATTEMPTS, NOT BLOCKS, and a field in conflict can cost more
  ;; than one: each candidate that is bytes is decoded on its own, so a
  ;; conflict holding two undecodable candidates moves the counter by two.
  ;; That was left as it is on purpose. The question was whether the count
  ;; depends on the order fields and tokens are evaluated in -- the search
  ;; now evaluates every field/token pair where it used to stop early -- and
  ;; it does not, because decoding happens in `field-strings`, once per
  ;; block per field, outside the token loops. Measured on a store with
  ;; three valid and three undecodable sources, five queries of one to four
  ;; tokens, under both the old evaluation order and the new: +3 on every
  ;; query, 15 in total, identical in both. A counter that moved with the
  ;; evaluation order would have been noise rather than a diagnostic, and
  ;; would have been changed to count blocks instead.
  ;; COUNT?, when #f, leaves the counter alone: a rehearsal's copy of a state
  ;; is read by a rule check of a write that may never happen, and a
  ;; diagnostic of the store's readers does not count it.
  (define (decoded-text bv . count?)
    (let ((text (utf8->string bv)))
      (if (bytevector=? (string->utf8 text) bv)
          text
          (begin (when (or (null? count?) (car count?))
                   (set! text-decode-skipped (+ text-decode-skipped 1)))
                 #f))))

  ;; THE TEXTS OF ONE FIELD OF A BLOCK: a string, a UTF-8 bytevector decoded,
  ;; or, for a field two writers left at once, each candidate's. STATE and ID
  ;; are the block's, because only the reducer can say a field is contested
  ;; (state-field-contested?): a value written once can have the conflict's
  ;; shape, and its "candidates" are then no text of the block.
  (define (field-strings state id block name)
    (let* ((fields (cdr (assq (quote fields) block)))
           (e (assq name fields)))
      (cond
        ((not e) (quote ()))
        ((string? (cdr e)) (list (cdr e)))
        ((bytevector? (cdr e))
         (let ((text (decoded-text (cdr e) (not (reduce-rehearsal? state)))))
           (if text (list text) (quote ()))))
        ((and (state-field-contested? state id name (cdr e)) (conflict-candidates (cdr e)))
         => (lambda (cs)
              (let loop ((l cs) (out (quote ())))
                (cond
                  ((null? l) (reverse out))
                  ((string? (car (car l))) (loop (cdr l) (cons (car (car l)) out)))
                  ((bytevector? (car (car l)))
                   (let ((text (decoded-text (car (car l)) (not (reduce-rehearsal? state)))))
                     (loop (cdr l) (if text (cons text out) out))))
                  (else (loop (cdr l) out))))))
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
  ;; NEVER: THE SNIPPET LOOKS AT EXACTLY WHAT THE SCORE LOOKED AT. This has
  ;; now been wrong three times in one batch, each time for the same reason
  ;; and each time one field further on: the score learned to read a field and
  ;; the snippet did not, so a block came back with a number and an empty
  ;; string. The fix that lasts is not another field added here -- it is that
  ;; the caller hands over the strings it scored, so there is nothing left to
  ;; forget.
  ;;
  ;; The order is preference, not truth: a keyword, then a title, then a line
  ;; of source, then the names, the doc, and last the printed body -- which is
  ;; a whole datum and the least readable of them.
  (define (snippet-for strings tokens)
    (let loop ((ls strings))
      (cond
        ((null? ls) "")
        ((exists (lambda (tk) (token-tier (car ls) tk)) tokens)
         (clip (collapse-whitespace (car ls))))
        (else (loop (cdr ls))))))

  ;; WHICH KINDS CARRY NAMES. Two verbs ask -- the index to decide what to
  ;; walk, search to decide what may score at the name tier -- so it is defined
  ;; once and both read it. How it came to be a table, and why the kind census
  ;; in `facade-gate` counts two rather than three, is written at that row;
  ;; this is not the place to keep a second copy of that history.
  ;; NEVER: AND THE TABLE IS THE DISPATCH, NOT A GATE BESIDE IT. The first
  ;; version of this was a set of kinds plus a `cond` that branched on the same
  ;; kinds -- the rule said twice, once as data and once as control flow. A
  ;; mutation that deleted the GATE survived, because the `cond`'s final clause
  ;; already returned nothing for every other kind: the table was a restatement
  ;; and nothing could tell whether it was there.
  ;;
  ;; A table that IS the dispatch cannot come apart from the branches, because
  ;; there are no branches: "which kinds have names" and "how their names are
  ;; read" are the same fact here.
  ;;
  ;; NEVER: BUT ADDING A READER IS NOT THE WHOLE OF ADDING A KIND, AND THIS
  ;; COMMENT SAID IT WAS. The index below still chooses the RECORD a kind
  ;; produces with its own `eq?` branches, and its final clause is silent -- so
  ;; a third reader makes `search` score a block at the name tier that
  ;; `whereis` cannot name, which is the disagreement this table exists to
  ;; prevent. Measured by a reviewer: a `decision` reader plus a `decision`
  ;; block gave search 12 and the index no entry at all.
  ;;
  ;; Adding a kind is three edits: a reader here, a record branch in the index,
  ;; and the pinned-keys row in `facade-gate`. That row is the reason this
  ;; cannot happen silently -- it fails the moment the keys change -- but it
  ;; asks about the keys, not about the index, so it will not tell you which of
  ;; the other two you forgot. Making the record shape table-driven as well is
  ;; the way to close that, and it is deliberately not done here: it reaches
  ;; further than this round, and belongs with the work that rebuilds the
  ;; index.
  (define (read-code-names fld)
    ;; A datum block carries BOTH the plural `names` and the singular `name`,
    ;; so reading both put every definition in twice -- measured, `whereis
    ;; reaper-start` answered with the same def record listed two times. The
    ;; plural is the complete answer where it exists; the singular is what a
    ;; TEXT-mode block has instead, and text is what `import-code` writes by
    ;; default.
    (let ((plural (names-in (fld (quote names)))))
      (if (null? plural) (names-in (fld (quote name))) plural)))

  ;; A LIBRARY'S NAMES ARE ITS EXPORTS AND NOTHING ELSE. Its `name` field is
  ;; the library's own name and it is a LIST of symbols -- `(probe d)` -- so a
  ;; route that put it through `names-in` scored each component as a defined
  ;; name: measured, `search probe` answered `(hit "..." 12 "probe")`, the top
  ;; of the scale, about a library that defines nothing of the sort, while
  ;; `whereis probe` correctly said the name was unknown.
  (define (read-library-names fld)
    (names-in (fld (quote exports))))

  (define name-readers
    (list (cons (quote code) read-code-names)
          (cons (quote library) read-library-names)))
  (define name-bearing-kinds (map car name-readers))

  ;; THE NAMES A BLOCK HAS, IN ONE FUNCTION, BECAUSE TWO VERBS ASK IT.
  ;;
  ;; NEVER: A GUARD PROTECTS THE DOOR THAT HOLDS IT, NOT THE PARSER IT WRAPS.
  ;; The index used to carry the per-block guard and the kind rules itself, and
  ;; `store-search` reached the same parser by a second route with neither.
  ;; Measured on the store this tree's own N4g fixture builds -- one library
  ;; whose export list has a rename clause with an improper tail:
  ;;
  ;;     whereis kept -> (def "..." (library (probe broken)) (name kept) ...)
  ;;     search  kept -> (error internal (condition "~s is not a proper list"))
  ;;     search  x    -> (error internal (condition "~s is not a proper list"))
  ;;
  ;; ONE unreadable block took down EVERY query in that store, while the verb
  ;; with the guard was unharmed. Three times in one round a hazard turned out
  ;; to have a second entrance -- tombstones, then which kinds have names, then
  ;; this -- and each time the repair was made at the door that had been
  ;; noticed. So the rule and the guard live here, both verbs come through, and
  ;; the second door does not exist to be forgotten.
  ;;
  ;; A LIBRARY'S NAMES ARE ITS EXPORTS AND NOTHING ELSE. Its `name` field is
  ;; the library's own name and it is a LIST of symbols -- `(probe d)` -- so a
  ;; route that put it through `names-in` scored each component as a defined
  ;; name: measured, `search probe` answered `(hit "..." 12 "probe")`, the top
  ;; of the scale, about a library that defines nothing called `probe`, while
  ;; `whereis probe` correctly said the name was unknown.
  ;; NEVER: THE GUARD BELONGS AT THE READ, NOT AT THE VERB THAT NOTICED. The
  ;; previous round put it around NAME PARSING and had both verbs come through
  ;; that -- and a malformed block does not reach name parsing. It raises while
  ;; the FIELDS are being derived, and both verbs read fields outside that
  ;; guard: the index takes a block's `kind` through its own reader BEFORE
  ;; asking for names, and search reads `doc` and `body` AFTER. The outline
  ;; reads a label through a third path again.
  ;;
  ;; The shape that does it is a NESTED improper list, not the outer one the
  ;; old comment used as its example:
  ;;
  ;;     (define-record-type thing (fields (mutable x . broken)))
  ;;
  ;; `datum-code.sc` tests `(list? body)` at the outer level, so
  ;; `(fields . broken)` is caught there and returns nothing. One level in, the
  ;; mutator arity is computed as `(> (length field) 3)` behind a `pair?` test
  ;; with no `list?`, and `length` on an improper list raises.
  ;;
  ;; So: ONE place reads a block, and it is guarded. `block-names`,
  ;; `derived-strings` and the index's field reader are all built on this, and
  ;; `view-read` has exactly one call site in this library. A block that cannot
  ;; be read has no fields, which every caller already knows how to handle.
  (define (viewed-fields state live id)
    (guard (e (#t (set! defs-index-skipped (+ defs-index-skipped 1)) (quote ())))
      (if (not (hashtable-ref live id #f))
          (quote ())
          (let ((b (view-read state id)))
            (if (and b (assq (quote fields) b))
                (cdr (assq (quote fields) b))
                (quote ()))))))

  ;; TWO GUARDS, BECAUSE THERE ARE TWO STEPS THAT CAN FAIL, AND THEY FAIL ON
  ;; DIFFERENT SHAPES. `viewed-fields` covers deriving the fields, which is
  ;; where a nested improper list raises. This one covers PARSING the names out
  ;; of those fields, which is where an improper export clause raises -- a
  ;; block whose fields read perfectly well and whose `exports` is
  ;; `(kept (rename (inner outer) . oops))`.
  ;;
  ;; This is not the duplicate-guard mistake of the previous round. That was
  ;; two guards over the SAME step, where removing either changed nothing and
  ;; neither could be tested. These two have a mutation each and a fixture
  ;; each: N4k raises in the read and N4g raises in the parse, and taking away
  ;; either guard reds its own rows and not the other's.
  (define (block-names state live id)
    (guard (e (#t (set! defs-index-skipped (+ defs-index-skipped 1)) (quote ())))
      (let* ((fs (viewed-fields state live id))
             (fld (lambda (n) (let ((e (assq n fs))) (and e (cdr e)))))
             (reader (assq (fld (quote kind)) name-readers)))
        (if reader ((cdr reader) fld) (quote ())))))

  ;; ---- the defs index: where a name lives -----------------------------------
  ;;
  ;; TWO KINDS OF ANSWER, BECAUSE A NAME CAN BE IN A LIBRARY WITHOUT BEING
  ;; DEFINED THERE. Three of this tree's own libraries -- `digest`, `json`,
  ;; `sched` -- define nothing at all: they name what they re-export and the
  ;; definitions live elsewhere, in igropyr. An index built only from
  ;; definitions answers "unknown" about `json-ref*`, which is written plainly
  ;; in an export list one line long. So a def record says where a name is
  ;; DEFINED and an export record says which library CARRIES it, and the
  ;; caller is told which it got.
  ;;
  ;; NEVER: BUILT AFTER REDUCTION AND NOT WRITTEN DOWN. An index on disk is a
  ;; second copy of the truth with its own staleness; this one is derived from
  ;; the reduction each time it is wanted, at the cost measured above.
  ;; ---- and it is built once per reduction, not once per question -----------
  ;;
  ;; NEVER: THE KEY IS THE APPLIED CUT, NOT THE REDUCTION OBJECT. When the
  ;; resident cache is on, a write DELIVERS INTO THE SAME reduction object
  ;; rather than making a new one, so an index remembered against that object
  ;; would answer about a store that has since changed -- and the row that
  ;; catches staleness dispatches twice in one process with a write between,
  ;; which is exactly that case. The applied cut moves with every record, so
  ;; it is what identifies the state an index describes.
  ;;
  ;; Measured on a store of this tree's own sources, 906 blocks: one build is
  ;; 14 ms, and `whereis` was rebuilding it on every call -- 8.7 seconds per
  ;; question before the walk was fixed, and a rebuild per question after.
  (define defs-index-builds 0)
  (define defs-index-skipped 0)
  (define (defs-index-skipped-count) defs-index-skipped)
  ;; A FIELD WHOSE BYTES ARE NOT TEXT. Counted apart from the block-level
  ;; counter above because they answer different questions: that one means
  ;; a block could not be read at all, this one means a readable block has
  ;; a field that cannot be searched.
  (define text-decode-skipped 0)
  (define (text-decode-skipped-count) text-decode-skipped)
  (define (defs-index-build-count) defs-index-builds)
  (define cached-index #f)
  (define cached-index-cut #f)

  (define (defs-index state)
    (let ((cut (reduce-applied-cut state)))
      ;; NEVER: AND THE KEY CANNOT BE THE OBJECT. Two readings, opposite ways:
      ;; with the resident cache ON a write delivers into the SAME reduction
      ;; object, so object identity says "unchanged" about a store that
      ;; changed; with it OFF every call replays into a NEW object, so object
      ;; identity says "changed" on every question and the memo never hits --
      ;; measured, two answers in one process rebuilt the index twice. The
      ;; applied cut is the thing that moves exactly when the store does, and
      ;; a cut carries writer ids that are generated per store, so two stores
      ;; cannot collide on one.
      (if (and cached-index (equal? cached-index-cut cut))
          cached-index
          (let ((built (build-defs-index state)))
            (set! cached-index built)
            (set! cached-index-cut cut)
            (set! defs-index-builds (+ defs-index-builds 1))
            built))))

  ;; THE LIBRARY A BLOCK BELONGS TO: the nearest enclosing block whose kind
  ;; is library, walking the outline's parents, or #f when there is none.
  ;; -> a procedure from a block id to that library's id or #f, built once
  ;; for a state. KIND-OF reads a block's kind; the caller decides how (the
  ;; definitions index reads it through its own guarded field memo). Both
  ;; the definitions index and name use answer "which library" with this one
  ;; walk.
  ;; NEVER: THE ROWS ARE WALKED ONCE. The first version asked
  ;; `parent-in-outline` for every step of every block's ancestry, and that
  ;; rescans the whole outline each time -- measured on a store of this
  ;; tree's own sources, 906 blocks: ONE INDEX BUILD TOOK 8.5 SECONDS, and
  ;; `whereis` rebuilds it per call, so the verb answered in 8.7. The same
  ;; walk done once, into a parent table, is the shape the markdown
  ;; projection already uses for the same reason.
  (define (library-locator state kind-of)
    (let ((parent (make-hashtable string-hash string=?)))
      (for-each (lambda (r) (hashtable-set! parent (caddr r) (car r))) (state-outline state))
      (lambda (id)
        (let loop ((up (hashtable-ref parent id #f)))
          (cond ((not up) #f)
                ((eq? up (quote root)) #f)
                ((eq? (quote library) (kind-of up)) up)
                (else (loop (hashtable-ref parent up #f))))))))

  (define (build-defs-index state)
    (let ((by-name (make-hashtable string-hash string=?))
          (live (make-hashtable string-hash string=?))
          ;; Each block's fields are derived ONCE. `view-read` is cheap --
          ;; 2 ms over every block in that store -- but not when it is called
          ;; per ancestor step per block.
          (fields (make-hashtable string-hash string=?)))
      ;; The memo wraps the one guarded reader rather than replacing it: this
      ;; used to call `view-read` itself, which is how a malformed block took
      ;; the index down through `kind` before `block-names` was ever reached.
      (define (fields-of id)
        (or (hashtable-ref fields id #f)
            (let ((fs (viewed-fields state live id)))
              (hashtable-set! fields id fs)
              fs)))
      (define (field id n)
        (let ((e (assq n (fields-of id))))
          (and e (cdr e))))
      (define (add! name record)
        (let ((key (symbol->string name)))
          (hashtable-set! by-name key
                          (append (hashtable-ref by-name key (quote ())) (list record)))))
      ;; NEVER: AN ANCESTOR THAT IS GONE IS NOT THE ANSWER. This walked the
      ;; ancestry without asking whether each step still exists, so deleting a
      ;; LIBRARY block left its name reported by every definition under it --
      ;; measured, after deleting only the library:
      ;;
      ;;     (def "cntj2d0l.2" (library (probe gone)) (name kept2) (kind code))
      ;;
      ;; The `export` record went, correctly, because that record IS the
      ;; library; the `def` record went on naming one that is not there. A dead
      ;; step is walked THROUGH rather than stopped at: a block whose library
      ;; was deleted is still inside whatever encloses that.
      ;;
      ;; NEVER: AND IT DOES NOT CHECK LIVENESS ITSELF, BECAUSE IT CANNOT BE THE
      ;; ONE THAT DECIDES. It used to, and a mutation removing that check
      ;; SURVIVED: `field` reads through `viewed-fields`, which returns nothing
      ;; for a block that is not in the outline, so a dead ancestor has no
      ;; `kind` and this walk passes over it regardless. Two authorities on one
      ;; question, and the one written here was never the live one. The
      ;; property is held by the check in `viewed-fields`, and the mutation
      ;; that removes THAT one reds this row along with the tombstone rows.
      ;;
      ;; NEVER: AND ASKING "IS IT A LIBRARY" VIA EMPTY FIELDS MERGES TWO CASES.
      ;; This asks whether the ancestor has `kind` of `library`, and gets its
      ;; fields from a reader that returns NOTHING for two different reasons --
      ;; the block is gone, or the block could not be read. Both now mean "not
      ;; a library here, keep walking", so a live-but-unreadable library
      ;; ancestor would have its definitions attributed to whatever encloses
      ;; IT. That looks unreachable today, because the view derives only for
      ;; `kind` of `code` and a `library` block does not go through the path
      ;; that raises -- but "unreachable today" is a reason, not a promise, and
      ;; the merge is written here rather than left to be rediscovered.
      (define library-of (library-locator state (lambda (up) (field up (quote kind)))))
      (for-each (lambda (r)
                  ;; NEVER: A DELETED BLOCK IS NOT AN ANSWER. `state-datum`
                  ;; lists tombstones and `state-outline` does not, and this
                  ;; read the first and checked neither -- measured, a deleted
                  ;; block defining `ghost` produced
                  ;; `(def "deleted" (library #f) (name ghost))`, so the verb
                  ;; sent a reader to a block that is not there. The outline is
                  ;; the set of blocks that exist.
                  (hashtable-set! live (caddr r) #t))
                (state-outline state))
      ;; ONE MALFORMED BLOCK MAY NOT TAKE THE WHOLE VERB WITH IT: a block that
      ;; cannot be read is skipped and counted, and the count is readable so a
      ;; row can see that skipping happened at all. The guard that does it is
      ;; in `viewed-fields`, which every reader here comes through; what is left
      ;; in this loop is the RECORD each kind produces, which is the part the
      ;; two verbs legitimately differ about.
      ;;
      ;; NEVER: AND THE EXAMPLE THIS COMMENT USED TO GIVE DID NOT RAISE. It
      ;; named `(define-record-type thing (fields . broken))`, which is caught
      ;; by a `list?` test at the outer level and quietly yields no names. The
      ;; shape that actually raises is nested -- `(fields (mutable x . broken))`
      ;; -- and the difference is not pedantry: the fixture for the guard was
      ;; written from that example, so it exercised a path the guard already
      ;; handled and left the real one untested for a round. A worked example
      ;; that does not reproduce the failure it illustrates is worse than none.
      (for-each
        (lambda (row)
          ;; NEVER: AND THE LIVE CHECK IS IN ONE PLACE TOO. This kept its own
          ;; `(and (hashtable-ref live id #f) ...)` after the rule moved into
          ;; `block-names`, so there were two authorities on whether a block
          ;; exists and the index was relying on the older one. A mutation that
          ;; removed the check inside `block-names` SURVIVED, which is how that
          ;; was found: a redundant guard does not make a tree safer, it makes
          ;; the guard that matters impossible to test.
          (let* ((id (cadr row))
                 (kind (field id (quote kind)))
                 (found (block-names state live id)))
            (cond
              ((null? found) (quote ()))
              ((eq? kind (quote code))
               (let ((lib (library-of id)))
                 (for-each
                   (lambda (n)
                     (add! n (list (quote def) id
                                   (list (quote library) (or (and lib (field lib (quote name))) (quote #f)))
                                   (list (quote name) n)
                                   (list (quote kind) (quote code)))))
                   found)))
              ((eq? kind (quote library))
               (let ((libname (field id (quote name))))
                 (for-each
                   (lambda (n)
                     (add! n (list (quote export) id
                                   (list (quote library) libname)
                                   (list (quote name) n))))
                   found)))
              (else (quote ())))))
        (state-datum state))
      by-name))

  ;; ---- what a name match is worth -------------------------------------------
  ;;
  ;; A NAME IS NOT PROSE, so it is not scored like prose. Asking for
  ;; `store-search` and being given the block that DEFINES it is a different
  ;; kind of answer from being given a paragraph that mentions it, and the
  ;; scores say so -- the numbers are at `name-score` below, which is the one
  ;; place that sets them. A name is compared whole, not as a substring,
  ;; because `cat` matching `concatenate` is a reasonable prose hit and a poor
  ;; answer to "where is cat defined".
  ;;
  ;; CASE IS FOLDED HERE AND NOT IN `whereis`, ON PURPOSE. Searching is asking
  ;; what a store is about, and a reader typing `Reaper` means `reaper`;
  ;; `whereis` is a lookup of an identifier, and in Scheme two spellings that
  ;; differ by case are two different names. So `search Direct` takes the name
  ;; tier where `whereis Direct` refuses -- the two verbs differ here because
  ;; they are being asked different questions, and the README says so.
  (define (name-tier names token)
    (let ((q (fold-preserving (prepare token))))
      (let loop ((l names) (best #f))
        (cond
          ((null? l) best)
          (else
            (let ((n (fold-preserving (prepare (car l)))))
              (cond
                ((string=? n q) 'exact)
                ((and (>= (string-length n) (string-length q))
                      (string=? (substring n 0 (string-length q)) q))
                 (loop (cdr l) (or best 'prefix)))
                (else (loop (cdr l) best)))))))))

  ;; NEVER: A DEFINITION HAS TO OUTRANK A MENTION, AND THE NUMBERS ARE CHOSEN
  ;; AGAINST THE BEST PROSE CAN DO, NOT AGAINST TODAY'S PROSE. Measured on the
  ;; first version: a block carrying the query in its title, its keywords AND
  ;; its source scores 4 + 3 + 2 = 9, while an exact definition scored 5 -- so
  ;; the block that merely talks about the name beat the block that defines
  ;; it, which is the one claim this segment is named for.
  ;;
  ;; The numbers were then 10 and 8, chosen against that 9 -- and that was
  ;; choosing against a number the tree happens to produce TODAY. A text-mode
  ;; block's source and doc are not searched yet only because they are held as
  ;; bytevectors; when they are read, one block reaches
  ;; keywords 4 + title 3 + src 2 + doc 1 = 10 with no name match at all, and
  ;; 10 was exactly the weakest name match. The ceiling moved and the gap
  ;; closed to a tie decided by id order.
  ;;
  ;; So: exact 12, prefix 10, chosen against the ceiling prose will have rather
  ;; than the one it has. An exact definition stays above fully-loaded prose
  ;; once that gap closes; a prefix match ties with it, which is deliberate --
  ;; `reaper-start-all` is a worse answer to `reaper-start` than a page about
  ;; it, and a tie broken by id is an honest way to say so.
  ;;
  ;; THAT GAP HAS NOW CLOSED, AND THE NUMBERS DID NOT MOVE. The paragraph
  ;; above was written while a text-mode source could not be searched, and
  ;; said what would happen when it could. It can now: `field-strings`
  ;; decodes a bytevector.
  ;;
  ;; THE CEILING IS STILL 11, BUT THE DERIVATION IS NOT THE ONE IT WAS, and
  ;; a reader who checks only the number will think nothing happened. `src 2`
  ;; was always a term in that sum and was never REACHABLE for a text-mode
  ;; block -- the value was thrown away before scoring, so no run could have
  ;; shown it. Decoding did not add a term; it gave an existing term a second
  ;; route to be reached by.
  ;;
  ;; That is why the re-derivation is measured as an EQUALITY rather than as
  ;; a maximum: the question is whether bytes and text score the same, and
  ;; `cli3.sc`'s N5c rows ask exactly that, with a twin saying a word in a
  ;; source is a source hit and not a name hit. If those two ever came apart,
  ;; the sum above would be right about one kind of block and wrong about the
  ;; other, and the number 11 would quietly be about only one of them.
  (define (name-score tier)
    (case tier ((exact) 12) ((prefix) 10) (else 0)))

  ;; The names a block defines, derived rather than stored, and the printed
  ;; form of a datum body -- the two things a code block has that a prose
  ;; block does not.
  ;; NEVER: ONE RULE FOR WHAT A NAME FIELD CONTAINS, READ BY BOTH SIDES. The
  ;; index and the search scored from two different readings of the same
  ;; field, so a `(rename (local public))` clause handled in one was still
  ;; dropped by the other -- `search` found the alias and `whereis` did not,
  ;; which is the same disagreement between the two verbs that this round is
  ;; fixing one line above.
  ;;
  ;; A block imported in TEXT mode carries a singular `name` and no `names`;
  ;; an export list may hold a rename clause whose usable name is the SECOND
  ;; element. Both are here, once.
  (define (names-in value)
    (cond
      ((symbol? value) (list value))
      ;; NEVER: A NAME IS NOT ALWAYS A SYMBOL, AND THE COMMON CASE IS THE
      ;; STRING. A DATUM block's names are read out of the form and come back
      ;; as symbols; a TEXT block's name is derived by its language from the
      ;; source and comes back as a STRING. This cond began at `symbol?` and
      ;; fell straight through to `(not (list? value))`, so a text-mode block
      ;; contributed no names at all -- and `--datum` is the OPT-IN, so text
      ;; is what `import-code` writes by default. Measured before the repair:
      ;; `whereis helper-fn` answered `(error unknown-name helper-fn (nearest))`
      ;; about a block the outline lists under exactly that name, and
      ;; `search helper-fn` found nothing. Every cell in this segment imported
      ;; with `--datum`, so nothing in the tree had ever taken the default path.
      ((string? value) (list (string->symbol value)))
      ((not (list? value)) (quote ()))
      (else
        (apply append
               (map (lambda (n)
                      (cond
                        ((symbol? n) (list n))
                        ((and (pair? n) (eq? (car n) (quote rename)))
                         (apply append
                                (map (lambda (pair)
                                       (if (and (pair? pair) (pair? (cdr pair))
                                                (symbol? (cadr pair)))
                                           (list (cadr pair))
                                           (quote ())))
                                     (cdr n))))
                        (else (quote ()))))
                    value)))))

  (define (derived-strings state live id field)
    (let* ((fs (viewed-fields state live id))
           (e (assq field fs)))
      (cond
        ((not e) (quote ()))
        ;; NEVER: BYTES ARE NOT TEXT, AND SPELLING THEM IS WORSE THAN DROPPING
        ;; THEM. A text-mode block keeps its `src` -- and the `doc` derived
        ;; from it -- as a BYTEVECTOR, and the fallback below printed whatever
        ;; it was handed: the doc went into the search as
        ;; `#vu8(59 59 32 119 111 ...)`, so `search vu8` returned a hit whose
        ;; snippet was a wall of byte numbers and `search 119` matched a byte
        ;; VALUE. Not finding the words in that source is a gap; answering
        ;; with its bytes is a wrong answer, and the two are not the same
        ;; size of wrong. The gap is named in the round's delivery note and
        ;; closes when the index is built; this clause is only here so that
        ;; until then the gap stays a gap.
        ((bytevector? (cdr e)) (quote ()))
        ((string? (cdr e)) (list (cdr e)))
        (else (list (datum-spelling (cdr e)))))))

  ;; -> the hits. A caller that also wants to know what was looked at asks
  ;; `store-search-report`, which answers an alist.
  ;;
  ;; TWO SHAPES, AND THE DEFAULT ONE IS THE OLD ONE. `store-search` answers
  ;; a list of hits and is called that way from several fixtures; a verb
  ;; that also wants to say what was looked at asks for the report instead.
  ;; Changing the single return into a report would have made every caller
  ;; read one field to get what it already had.
  ;;
  ;; THE CAP IS A PARAMETER, NOT A NAME. There were two report functions for
  ;; one day: `store-search-report`, and `store-search-report/all` taking a
  ;; boolean. They read as two operations while differing only in a limit,
  ;; and the limit was invisible at the call site.
  ;;
  ;; NEVER: WHAT THAT COST. `store-search-report` passed a constant down to
  ;; `store-search`, and when `store-search` learned to read that constant
  ;; differently, `store-search-report` began capping its answer at ten --
  ;; AN EXPORTED FUNCTION WHOSE TEXT HAD NOT CHANGED BY ONE CHARACTER. A
  ;; function's behaviour can change while its own lines stand still,
  ;; whenever it hands a value on and the reader of that value changes its
  ;; mind about what the value means. So the limit is named where it is
  ;; decided: `#f` is no cap, a positive integer is the cap, and the verb
  ;; that wants ten says ten.
  ;; HOW MANY HITS AN ANSWER CARRIES WHEN NOBODY SAID.
  ;;
  ;; Ten, and it is not the same question `grep`'s caps answer. Grep counts
  ;; LINES, and its two caps exist because one block can hold hundreds of
  ;; them -- measured, 418 in a single file -- so a line budget alone lets
  ;; one block starve the rest. Search counts BLOCKS, and a block appears at
  ;; most once however many of its fields matched, so there is no starving
  ;; to prevent: the only question is how many ranked answers a reader wants
  ;; before they would rather narrow the query.
  ;;
  ;; Ten because the answer is RANKED and the scoring is built to separate
  ;; the top of it: an exact name match scores 12 and the most a block with
  ;; no name can reach is 11, so the blocks a reader is looking for are at
  ;; the head of the list rather than spread through it. A budget larger
  ;; than that mostly carries blocks the ranking has already decided are
  ;; worse answers.
  (define search-hit-limit 10)

  ;; ONE CONSTRUCTOR, AND BOTH BRANCHES GO THROUGH IT.
  ;;
  ;; NEVER: A REPORT'S KEYS ARE NOT A PROPERTY OF THE BRANCH THAT BUILT IT.
  ;; `store-search` answers from two branches -- one for a query that has no
  ;; tokens, one for a query that has some -- and each used to spell its own
  ;; alist. The empty one left out `omitted-hits`, so a caller that read
  ;; that key without asking whether it was there met `(cdr #f)`: `search
  ;; ""` and `search "   "` answered `(error internal ...)`, with or without
  ;; `--all`. Nothing said so, because no row had ever asked an empty query.
  ;; One answer with two shapes is the defect this batch has spent its
  ;; length taking apart, one level below where it was being taken apart.
  ;;
  ;; The fixture asks for the KEY SET rather than for the presence of the
  ;; one key that was missing: a row that only checked `omitted-hits` would
  ;; go on being green the next time a branch grows a key of its own.
  ;;
  ;; LIVE BLOCKS, NOT RECORDS, in `scanned`. `state-datum` lists tombstones
  ;; and the hit loop walks them, skipping each one; `state-outline` is the
  ;; set of blocks that exist. Reporting the walk's length made `scanned`
  ;; mean something different here than it means for `grep`, which counts
  ;; what it looked at -- one clause name with two definitions. Measured:
  ;; delete the only block in a store and search answered `(scanned (blocks
  ;; 1))` with no live block left.
  ;;
  ;; `defs-names` is how many names the definitions index -- what `whereis`
  ;; and the name tier read -- holds for this answer. It is the fact
  ;; `coverage` reports. It replaced `defs-built`, a boolean; the paragraph
  ;; at `defs-names` below says why.
  ;; `unreadable-blocks` IS A FACT ABOUT THIS ANSWER, and the process counter
  ;; is not. `text-decode-skipped-count` counts DECODE ATTEMPTS and keeps
  ;; counting across every search a process makes -- measured, one block
  ;; with one undecodable `src` moves it by one on each of four queries,
  ;; including a query that matched nothing. That is a diagnostic about a
  ;; process. What a caller reading one answer needs is how many of the
  ;; blocks THIS scan looked at had text it could not read, counted once per
  ;; block. Two facts, two places; the counter's own comment already says it
  ;; counts attempts.
  ;;
  ;; It is derived FROM that counter, per block: the count is read before a
  ;; block's fields and after them, and the block is unreadable if it moved.
  ;; That is once per block by construction, whatever route the fields took,
  ;; and it costs no second decode.
  ;; `derived` is (<tables read> (<via> ...) <stale>) when the caller gave
  ;; the search an editor's keywords to consult, else #f: the scanned
  ;; fields name derived-keywords only when a table was read.
  (define (search-report state shown omitted scanned unreadable . derived)
    (append
    (list (cons (quote items) shown)
          (cons (quote omitted-hits) omitted)
          (cons (quote scanned-blocks) scanned)
          (cons (quote unreadable-blocks) unreadable)
          (cons (quote fields)
                (if (and (pair? derived) (car derived) (> (car (car derived)) 0))
                    (quote (title keywords derived-keywords src names doc body))
                    (quote (title keywords src names doc body))))
          (cons (quote cut) (reduce-applied-cut state))
          ;; THE STATE ITSELF, so the answer's receipt hashes its hits in the
          ;; state they were found in, not in a second fold.
          (cons (quote state) state)
          ;; HOW MANY NAMES THE INDEX HOLDS, not whether it exists.
          ;;
          ;; NEVER: THE OLD ANSWER HAD A VALUE NOTHING COULD PRODUCE.
          ;; `coverage` carried `(defs built)` or `(defs absent)`, and
          ;; `absent` was unreachable: `build-defs-index` always returns its
          ;; table, guarding per block, so `(and (defs-index state) #t)` is
          ;; `#t` for every store there is -- measured over a 3564-block
          ;; markdown corpus, an 86-block code corpus, a two-block store and
          ;; an EMPTY one, all four built for the question and read in
          ;; `archive/theourgia-s-b2b-c3-2026-09-21/NOTES.md`, which is where
          ;; the numbers in this comment can be checked. The clause existed
          ;; to separate "nothing in this store answers to that" from "there
          ;; was nothing to consult", and it could only ever say the first.
          ;;
          ;; The number says both, and the second is the common case rather
          ;; than a corner: all four of those stores hold ZERO names. A store
          ;; of 3564 blocks and an empty store used to give a caller the same
          ;; word.
          ;;
          ;; NEVER: AND THE REASON WRITTEN HERE USED TO BE WRONG. It said
          ;; "only a `--datum` import writes name-bearing blocks". It does
          ;; not: what decides is the CONTENT. Overturned by a fixture --
          ;; `d4e` in `test/cli3.sc` imports a library WITHOUT `--datum` and
          ;; the clause answers `(names 1)`, because the source defines a
          ;; procedure and the name is derived from the source. The corpora
          ;; below hold zero names because of what is in them, not because of
          ;; the flag they were imported with.
          ;;
          ;; NEVER: AND ZERO STILL HAS TWO SOURCES THIS ANSWER CANNOT TELL
          ;; APART. A store with nothing nameable in it reads `(names 0)`,
          ;; and so does a store whose nameable blocks could not be read:
          ;; `build-defs-index` guards each block, counts the failures into
          ;; `defs-index-skipped`, and contributes no names for them. So
          ;; "none" and "we could not tell" are one number again -- the very
          ;; shape this clause replaced `(defs built)` to fix. Naming the
          ;; second is scheduled; until it lands, a reader of `(names 0)`
          ;; does not know which one it has.
          (cons (quote defs-names) (defs-index-name-count state)))
    (if (and (pair? derived) (car derived))
        (list (cons (quote derived-tables) (car (car derived)))
              (cons (quote derived-via) (cadr (car derived)))
              (cons (quote derived-stale) (caddr (car derived))))
        (quote ()))))

  ;; -> how many distinct names the definitions index holds for this state.
  (define (defs-index-name-count state)
    (let ((ix (defs-index state)))
      (if ix (vector-length (hashtable-keys ix)) 0)))

  ;; The report, with the cap the caller chose. `#f` asks for every hit.
  ;;
  ;; NEVER: THE LIMIT IS CHECKED HERE, WHERE THE CALLER IS. An unexpected
  ;; value used to be a silent change of shape -- `#t` once meant "report"
  ;; and later meant "report, capped" -- and the reading a caller got
  ;; depended on which build it was linked against. A wrong limit is now a
  ;; refusal at the door rather than a different answer.
  ;; `derived`, when given, is a procedure the caller supplies: given the
  ;; state searched, it answers #f (nothing to consult) or
  ;; (<hook> <tables read> <stale>), the hook as store-search describes it.
  ;; This library does not read those tables itself; the caller does.
  (define (store-search-report store query limit . derived)
    (if (not (or (eq? limit #f)
                 (and (integer? limit) (exact? limit) (positive? limit))))
        (assertion-violation 'store-search-report
                             "the limit is #f for every hit, or a positive integer"
                             limit)
        (apply store-search store query limit derived)))

  ;; THE SEARCH OF ONE STATE: what store-search answers, over a state the
  ;; caller already holds; store-search opens the store and asks it.
  (define (store-search store query . rest)
    (apply search-state (open-and-reduce store) query rest))

  (define (search-state state query . rest)
    (let* (
           ;; NEVER: THE QUERY IS PREPARED BEFORE IT IS SPLIT, NOT AFTER.
           ;; `tokens-of` cuts on whitespace, and `prepare` WRITES whitespace
           ;; at a CJK/Latin seam -- so splitting first left the seam inside a
           ;; single token, which then had to be found as one contiguous run.
           ;; Measured: against a title reading "<three CJK characters> then
           ;; reaper" the compact query found nothing while the spaced one
           ;; found the block. The equivalence this segment promises held only
           ;; when the two words happened to be adjacent in the text as well.
           (tokens (tokens-of (prepare query)))
           ;; ONE TABLE PER SEARCH. It is built here and released by
           ;; `prepared-end!` on the way out, on both exits. See the note at
           ;; `prepared-begin!` for why it cannot usefully outlive one
           ;; search, and what would have to change for it to.
           (ignored-generation (prepared-begin!))
           ;; NEVER: THE SET OF BLOCKS THAT EXIST IS THE OUTLINE, AND THIS
           ;; LOOP HAD TO LEARN IT SEPARATELY. `state-datum` lists tombstones.
           ;; The index learned this when a deleted block kept answering
           ;; `whereis`; this loop reads the same list and learned nothing, so
           ;; search kept answering about deleted blocks after the index had
           ;; stopped. Measured, with a control on each side: the outline
           ;; listed the block before `del` and not after, and `search` still
           ;; returned `(hit ... 3 "a page about nothing")` for its title.
           ;; A guard's comment is a map of the entrance nobody guarded.
           (live (let ((t (make-hashtable string-hash string=?)))
                   (for-each (lambda (r) (hashtable-set! t (caddr r) #t))
                             (state-outline state))
                   t))
           ;; HOW MANY LIVE BLOCKS THIS SCAN COULD NOT READ THE TEXT OF.
           ;; One search, one count; it is raised once per block by the
           ;; loop below and read by the report at the end.
           (unreadable-here 0)
           ;; AN EDITOR'S KEYWORDS, WHEN THE CALLER GAVE THEM: consulted
           ;; only for a query that looks at blocks at all.
           (derived (and (pair? rest) (pair? (cdr rest)) (cadr rest) (pair? tokens)
                         ((cadr rest) state)))
           ;; (hook <id> <author keywords> <score>) -> #f or
           ;; (<row> <tier> (<via> ...) ("<word>" ...)); the rule for which
           ;; blocks use an editor's words, and how, is the hook's.
           (derived-words (if derived (car derived) (lambda (id kws score) #f)))
           ;; id -> the provenances of the derived words a hit was found by
           (derived-used (make-hashtable string-hash string=?))
           ;; THE CALLER'S FILTER, a procedure of the state folded here: given
           ;; a block id it answers whether to leave the block out, and a
           ;; block it names is skipped before it can be a hit, so it cannot
           ;; use up the cap or count as omitted. The caller keeps the count.
           (skip? (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest)) (caddr rest) (pair? tokens)
                       ((caddr rest) state))))
      (if (null? tokens)
          (let ((none (prepared-end! (quote ()))))
            (if (pair? rest)
                ;; An empty query looks at nothing, leaves nothing out,
                ;; and reads no block's text.
                (search-report state none 0 0 0 #f)
                none))
          (let ((hits
                  (let loop ((ds (state-datum state)) (out (quote ())))
                    (if (null? ds)
                        out
                        (let* ((id (cadr (car ds)))
                               (block (state-read state id))
                               (alive (hashtable-ref live id #f))
                               ;; READ BEFORE THIS BLOCK'S FIELDS, compared
                               ;; after them: the process counter moves once
                               ;; per failed decode, and what this needs is
                               ;; whether it moved at all for THIS block.
                               (decodes-before (text-decode-skipped-count))
                               (titles (field-strings state id block (quote title)))
                               (srcs (field-strings state id block (quote src)))
                               ;; KEY: KEYWORDS SCORE 3, ABOVE TITLE'S 2 AND
                               ;; SOURCE'S 1. They are the one field a
                               ;; writer chose FOR being found by, so a
                               ;; block whose keywords match is a better
                               ;; answer than one whose prose happens to.
                               (kws (field-strings state id block (quote keywords)))
                               ;; The tier a field reaches is the best any
                               ;; token reaches in it.
                               ;; ONE MATRIX, TWO PROJECTIONS OF IT.
                               ;;
                               ;; A field's score wants the BEST tier any
                               ;; token reached in it, and the answer to
                               ;; "did every token hit something" wants,
                               ;; for each token, whether ANY field took
                               ;; it. Those are two projections of the same
                               ;; (field x token) table, along different
                               ;; axes, and the code used to compute the
                               ;; table twice -- once collapsed per field,
                               ;; then again inside `every-token`, which
                               ;; re-ran `field-tier` over all six fields.
                               ;; Measured at 196 ms of a 910 ms query, on
                               ;; work that had just been done.
                               ;;
                               ;; NEVER DERIVE ONE PROJECTION FROM THE
                               ;; OTHER. The per-field best has already
                               ;; thrown away WHICH token reached it, so
                               ;; "every token hit something" cannot be
                               ;; recovered from it: two tokens, one
                               ;; matching only the title and the other
                               ;; only the source, is a hit, and a rule
                               ;; asked of the collapsed values would have
                               ;; to find one field that both reached. A
                               ;; single-token query can never show the
                               ;; difference, which is why the row that
                               ;; does uses two tokens in two fields.
                               (row (lambda (f strings)
                                      (map (lambda (tk) (f strings tk)) tokens)))
                               (title-row (row field-tier titles))
                               (src-row (row field-tier srcs))
                               (kw-row (row field-tier kws))
                               ;; AN EDITOR'S KEYWORDS, one call to the hook per
                               ;; live block, scored as this search scores a
                               ;; field; #f when the block has none to use.
                               (dk (and alive
                                        (derived-words id kws
                                                       (lambda (strings)
                                                         (let ((r (row field-tier strings)))
                                                           (values r (collapse-field-row r)))))))
                               (dk-row (if dk (car dk) (row field-tier (quote ()))))
                               ;; NEVER: AND THE THINGS ONLY A CODE BLOCK HAS.
                               ;; `names` is derived from the source rather
                               ;; than stored, so it is read from the view;
                               ;; `doc` and a datum `body` are stored, and the
                               ;; body is a datum, so it is compared as the
                               ;; text it prints as. Measured before this:
                               ;; three searches for code found nothing at
                               ;; all, because none of these was looked at.
                               ;; NEVER: AND THE TWO VERBS ANSWER ABOUT THE
                               ;; SAME NAMES. `whereis` learned to report a
                               ;; library that CARRIES a name without defining
                               ;; it, and search did not -- measured, a name
                               ;; only re-exported was found by one verb and
                               ;; not the other, over the same store. A block's
                               ;; names are what it defines together with what
                               ;; it exports.
                               ;; NEVER: AND SEARCH DOES NOT HAVE ITS OWN IDEA
                               ;; OF WHAT A NAME IS. It used to append three
                               ;; fields here and gate them on a kind rule of
                               ;; its own, which is how it came to disagree with
                               ;; the index three separate times: about blocks
                               ;; imported the default way, about which kinds
                               ;; have names, and about a library's own name
                               ;; being a name. There is one function that
                               ;; answers this and both verbs call it.
                               (names (map symbol->string (block-names state live id)))
                               (docs (derived-strings state live id (quote doc)))
                               (bodies (derived-strings state live id (quote body)))
                               ;; EVERY FIELD OF THIS BLOCK HAS BEEN READ BY
                               ;; HERE, including the derived ones, so this is
                               ;; the point where the comparison is complete.
                               (block-unreadable
                                 (> (text-decode-skipped-count) decodes-before))
                               (ignored-unreadable
                                 (when (and alive block-unreadable)
                                   (set! unreadable-here (+ unreadable-here 1))))
                               (name-row (row name-tier names))
                               (doc-row (row field-tier docs))
                               (body-row (row field-tier bodies))
                               ;; The collapses come after every row exists,
                               ;; so the matrix is complete before either
                               ;; projection is taken from it.
                               (title-tier (collapse-field-row title-row))
                               (src-tier (collapse-field-row src-row))
                               (kw-tier (collapse-field-row kw-row))
                               (dk-tier (and dk (cadr dk)))
                               (doc-tier (collapse-field-row doc-row))
                               (body-tier (collapse-field-row body-row))
                               (nm-tier (collapse-name-row name-row))
                               ;; NEVER: EVERY TOKEN STILL HAS TO HIT SOMEWHERE.
                               ;; Keywords widen where a token may be
                               ;; found; they do not turn the query into
                               ;; an OR across tokens.
                               (every-token
                                 (every-token-hit?
                                   (list title-row src-row kw-row dk-row
                                         name-row doc-row body-row)))
                               (ignored-derived
                                 (when (and alive every-token dk-tier)
                                   (hashtable-set! derived-used id (caddr dk)))))
                          (loop (cdr ds)
                                (if (and alive every-token (not (and skip? (skip? id))))
                                    (cons (list id
                                                (+ (tier-score 'title title-tier)
                                                   (tier-score 'src src-tier)
                                                   (tier-score 'keywords kw-tier)
                                                   (tier-score 'src dk-tier)
                                                   (name-score nm-tier)
                                                   (if doc-tier 1 0)
                                                   (if body-tier 1 0))
                                                (snippet-for
                                                  ;; NEVER: AND THE WHOLE
                                                  ;; FIELD STAYS AT THE END.
                                                  ;; A line is what a reader
                                                  ;; wants, but a query whose
                                                  ;; parts land on two lines
                                                  ;; matches neither of them
                                                  ;; alone -- the fallback is
                                                  ;; why that hit has anything
                                                  ;; to show. Collapsing this
                                                  ;; list to lines only lost
                                                  ;; that, and the row for it
                                                  ;; went red the same hour.
                                                  (append kws (if dk (cadddr dk) (quote ())) titles
                                                          (apply append (map lines-of-text srcs))
                                                          names docs bodies srcs)
                                                  tokens)
                                                ;; WHICH FIELDS THIS BLOCK
                                                ;; MATCHED IN, added after
                                                ;; the snippet so that a
                                                ;; reader taking the first
                                                ;; four positions goes on
                                                ;; taking the same four.
                                                ;;
                                                ;; NEVER: THIS IS NOT THE
                                                ;; `fields` OF `scanned`.
                                                ;; That one says which
                                                ;; fields the SEARCH looked
                                                ;; at, the same list for
                                                ;; every answer; this says
                                                ;; which fields THIS BLOCK
                                                ;; was found in, and it
                                                ;; differs from block to
                                                ;; block. Two clauses of one
                                                ;; name in one answer is
                                                ;; exactly the shape this
                                                ;; batch has spent its
                                                ;; length taking apart, so
                                                ;; it is allowed here only
                                                ;; because a row measures
                                                ;; that the two are not the
                                                ;; same list -- see the
                                                ;; fixture row that asks for
                                                ;; an input where they
                                                ;; differ. If they could not
                                                ;; differ, one of them would
                                                ;; have to be renamed.
                                                ;;
                                                ;; The order is fixed rather
                                                ;; than the order they were
                                                ;; tested in, so two answers
                                                ;; about one block read the
                                                ;; same.
                                                (list 'fields
                                                      (filter (lambda (x) x)
                                                              (list (and title-tier 'title)
                                                                    (and kw-tier 'keywords)
                                                                    (and dk-tier 'derived-keywords)
                                                                    (and src-tier 'src)
                                                                    (and nm-tier 'names)
                                                                    (and doc-tier 'doc)
                                                                    (and body-tier 'body)))))
                                          out)
                                    out)))))))
            (let ((sorted (prepared-end!
                            (list-sort (lambda (a b)
                                         (if (= (cadr a) (cadr b))
                                             (string<? (car a) (car b))
                                             (> (cadr a) (cadr b))))
                                       hits))))
              (if (pair? rest)
                  (let* ((limit (car rest))
                         (total (length sorted))
                         (shown (if (or (not limit) (<= total limit))
                                    sorted
                                    (let take ((l sorted) (k limit) (out (quote ())))
                                      (if (or (null? l) (= k 0))
                                          (reverse out)
                                          (take (cdr l) (- k 1) (cons (car l) out)))))))
                    (search-report state shown
                                   (- total (length shown))
                                   (length (state-outline state))
                                   unreadable-here
                                   (and derived
                                        (list (cadr derived)
                                              (apply append
                                                     (map (lambda (h) (hashtable-ref derived-used (car h) (quote ())))
                                                          shown))
                                              (caddr derived)))))
                  sorted))))))

  ;; ---- grep: lines, where search answers with blocks ----------------------
  ;;
  ;; `search` answers the question "which blocks are about this", scored and
  ;; ranked. `grep` answers "which lines say this", literally and in order.
  ;; They are different questions and they carry different item tags, which
  ;; is a rule the facade gate now keeps: a reader that keys on the tag must
  ;; not be able to take one for the other.
  ;;
  ;; LITERAL, NOT A PATTERN LANGUAGE. A `.` matches a dot. There is no
  ;; regular expression here and no plan for one: a pattern language is a
  ;; second language inside this one, and the fixtures pin the literal rule
  ;; with a row that greps for `.` and gets only the lines that contain one.
  ;;
  ;; TWO CAPS, AND BOTH OF THEM WERE MEASURED RATHER THAN CHOSEN.
  ;;
  ;; Counting matching lines over the two corpora this store is verified
  ;; against: in the prose arm (3531 blocks) a query for an ordinary word
  ;; matches tens of thousands of lines spread thin -- `the` gives 31614
  ;; lines over 3135 blocks, a median of 7 per block and at most 49. In the
  ;; code arm (86 blocks) the same total arrives concentrated: `let` gives
  ;; 1996 lines over 39 blocks with 418 of them in ONE block.
  ;;
  ;; So a total cap alone is the wrong instrument: on the code arm the
  ;; largest block would spend the whole budget and the other 38 files would
  ;; not appear at all, while the answer said only that some lines were
  ;; dropped. The per-block cap is what stops one block starving the rest.
  ;;
  ;; 20 is the per-block cap because the medians measured above -- 17 in the
  ;; code arm, 2 to 7 in the prose arm -- are shown whole. 200 is the total,
  ;; so at least ten blocks reach the reader whatever else is true.
  (define grep-block-limit 20)
  (define grep-line-limit 200)

  ;; THE TEXT OF A BLOCK, IN A FIXED ORDER, and the order matters only for a
  ;; case that does not yet occur: measured over both corpora, no block
  ;; carries more than one of these fields -- 0 of 3531 and 0 of 86 hold
  ;; both `src` and `doc`, and none holds `body`. A line number is the nth
  ;; line of this sequence, so the order is written down now rather than
  ;; discovered later by whoever first writes two of them.
  (define (grep-text-of state live id)
    (let ((stored (lambda (name)
                    (let ((b (state-read state id)))
                      (if b (field-strings state id b name) (quote ())))))
          (derived (lambda (name) (derived-strings state live id name))))
      (append (stored (quote src))
              (derived (quote doc))
              (derived (quote body)))))

  ;; -> AN ALIST, NOT A TUPLE. The caller reads `items`, `omitted-lines`,
  ;; `unseen-blocks`, `scanned-blocks`, `fields` and `cut` by name. A tuple
  ;; would mean every later addition either moves a position or adds one
  ;; more thing to count, and the reason is the same one that made
  ;; `(truncated (lines n) (blocks m))` carry names rather than a bare
  ;; integer: a value that is identified by where it sits can only be read
  ;; by something that already knows the shape.
  ;;
  ;; An item is `(id line-number text)`. A block that matched but shows no
  ;; line at all is counted in `unseen-blocks`, which is the dimension a
  ;; count of lines cannot carry.
  ;; `hooks`, optional: the caller's filter, as store-search takes it. A
  ;; block it names among those with a matching line is skipped before the
  ;; caps; it is still scanned.
  (define (store-grep store pattern under all? . hooks)
    (let* ((state (open-and-reduce store))
           (rows (state-outline state))
           (live (let ((t (make-hashtable string-hash string=?)))
                   (for-each (lambda (r) (hashtable-set! t (caddr r) #t)) rows)
                   t))
           ;; THE ORDER IS THE OUTLINE'S, so two runs over one store answer
           ;; in the same order and a truncated answer is a prefix of the
           ;; whole one rather than an arbitrary sample.
           (ids (map caddr rows))
           ;; `#f` IS THIS CALLER'S OWN: no `--under`, no filter. The walk
           ;; never answers it, so a root that names no block filters to
           ;; itself alone rather than switching the filter off.
           (wanted (and under
                        (let ((t (make-hashtable string-hash string=?)))
                          (for-each (lambda (x) (hashtable-set! t x #t))
                                    (outline-subtree rows under))
                          t)))
           (q (fold-preserving (prepare pattern)))
           (skip? (and (pair? hooks) (car hooks) (> (string-length q) 0) ((car hooks) state))))
      ;; `unreadable` MEANS HERE WHAT IT MEANS IN A SEARCH ANSWER: how many
      ;; of the blocks THIS scan looked at had text it could not read,
      ;; counted once per block. `grep` reads `src` through the same
      ;; `field-strings`, so the same failure reaches it, and a clause that
      ;; meant one thing in one verb's answer and another in another verb's
      ;; is the shape this batch exists to take apart.
      (define (report items omitted unseen scanned unreadable)
        (list (cons (quote items) items)
              (cons (quote omitted-lines) omitted)
              (cons (quote unseen-blocks) unseen)
              (cons (quote scanned-blocks) scanned)
              (cons (quote unreadable-blocks) unreadable)
              (cons (quote fields) (quote (src doc body)))
              (cons (quote cut) (reduce-applied-cut state))
              (cons (quote state) state)))
      (if (= 0 (string-length q))
          (report (quote ()) 0 0 0 0)
          (let loop ((l ids) (out (quote ())) (shown 0) (omitted 0) (unseen 0) (scanned 0)
                     (unreadable 0))
            (cond
              ((null? l) (report (reverse out) omitted unseen scanned unreadable))
              ((and wanted (not (hashtable-ref wanted (car l) #f)))
               (loop (cdr l) out shown omitted unseen scanned unreadable))
              (else
                (let* ((id (car l))
                       ;; Read before this block's text and compared after
                       ;; it, exactly as the search loop does.
                       (decodes-before (text-decode-skipped-count))
                       (lines (apply append
                                     (map lines-of-text (grep-text-of state live id))))
                       (block-unreadable
                         (> (text-decode-skipped-count) decodes-before))
                       (matching
                         (let scan ((ls lines) (n 1) (acc (quote ())))
                           (cond
                             ((null? ls) (reverse acc))
                             ((substring-at? (fold-preserving (prepare (car ls))) q)
                              (scan (cdr ls) (+ n 1) (cons (list id n (car ls)) acc)))
                             (else (scan (cdr ls) (+ n 1) acc)))))
                       (matching (if (and skip? (pair? matching) (skip? id)) (quote ()) matching))
                       (count (length matching))
                       (room (if all? count (max 0 (- grep-line-limit shown))))
                       (take-n (if all? count (min count grep-block-limit room)))
                       (taken (let cut ((m matching) (k take-n) (acc (quote ())))
                                (if (or (null? m) (= k 0))
                                    (reverse acc)
                                    (cut (cdr m) (- k 1) (cons (car m) acc))))))
                  (loop (cdr l)
                        (append (reverse taken) out)
                        (+ shown take-n)
                        (+ omitted (- count take-n))
                        (+ unseen (if (and (> count 0) (= take-n 0)) 1 0))
                        (+ scanned 1)
                        (+ unreadable (if block-unreadable 1 0))))))))))

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
        ;; A TEMPLATE BLOCK THAT CANNOT BE READ: every verb that consults
        ;; the template behaves as if there were none, and this says why.
        (let ((problem (template-problem state)))
          (if problem (list (list (quote template) problem)) (quote ())))
        ;; A RELATION NAME TWO WRITERS DECLARED DIFFERENTLY, neither having
        ;; seen the other: its edges have no effect until one declares again.
        (state-relation-contested state)
        ;; A RULE TWO WRITERS DECLARED DIFFERENTLY: not evaluated until one
        ;; declares it again.
        (state-rule-contested state)
        ;; TWO IDENTITIES FOR ONE PATH: two alive datum libraries, or two
        ;; alive text files, that an export would write to the same file.
        (state-duplicated-paths state)
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
        (reduce-noted state)
        ;; A WRITER WHOSE HISTORY WAS CUT BY DAMAGE is the same kind of
        ;; fact: the store holds records after the cut and cannot show
        ;; them. One row per cut writer, from the reduction's notes:
        ;; (cut <writer> <path> <kind> <after>).
        (apply append
               (map (lambda (n)
                      (if (and (list? n) (= (length n) 4) (pair? (cadddr n)) (eq? (car (cadddr n)) (quote cut)))
                          (list (list (quote cut) (car n) (cadr n) (cadr (cadddr n)) (caddr (cadddr n))))
                          (quote ())))
                    (unreadable-behind state))))))


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
  ;; THE TAGS ARE LOOKED UP IN THE STATE THE CALLER IS ANSWERING FROM, when
  ;; it has one (a cut read on the daemon has the published reduction), and
  ;; otherwise in a fresh fold made only if the text is not a literal --
  ;; diff's and tag's answers and their cost are what they were.
  (define (resolve-cut store which text . state)
    (let ((literal (parse-cut text)))
      (if literal
          (list (quote ok) literal)
          (let ((found (filter (lambda (t) (equal? (cadr t) text)) (apply store-tags store state))))
            (cond
              ((null? found)
               (list (quote error) (quote unknown-tag) text (list (quote cut) which)))
              ((eq? (car (car found)) (quote unsettled))
               (list (quote error) (quote tag-unsettled) text (list (quote cut) which)))
              (else (list (quote ok) (caddr (car found)))))))))

  ;; THE STATE A READ AT A CUT ANSWERS FROM: the cut text resolved (a
  ;; literal, or a settled tag) and judged usable against the state the
  ;; caller already holds -- the daemon's publication, unsealed here, or a
  ;; fresh fold when there is none -- then ONE restricted replay to that
  ;; cut, which never seeds from the snapshot or the resident cache (see
  ;; replay). -> a reduction, or the refusal:
  ;;   (error unknown-tag <text> (cut cut)) | (error tag-unsettled ...)
  ;;   (error cut-unavailable (cut cut) (reason duplicate-writer|not-received|not-closed|malformed))
  ;; the shapes diff gives, or
  ;;   (error cut-moved (asked <cut>) (served <cut>))
  ;;
  ;; NEVER: A CUT THAT MOVED IS NOT SERVED AS IF IT HAD NOT. The check reads
  ;; one state and the replay opens the log again; a record quarantined in
  ;; between (log.sc, the fork's ceiling) is not delivered, so the replay
  ;; stops short of the cut that was judged usable and the read answered ok
  ;; for a cut it did not serve. The served state's cut is asked again after
  ;; the replay: when it does not cover the cut asked, the read is refused
  ;; with both. The hold stage read-cut-before-replay sits in the window.
  ;;
  ;; NEVER: A WRITER ASKED AT 0 IS NOT MISSING FROM WHAT WAS SERVED. The
  ;; replay delivers none of its records, and a reduction names a writer
  ;; only once one of its records applies, so the served cut has no entry
  ;; for it; cut-covers? reads an absent writer as covering nothing, and a
  ;; cut such as ((A . 1) (B . 0)) on an unchanged store was refused. Only
  ;; the entries above 0 are asked to be covered; the refusal names the
  ;; cut as it was asked.
  (define (store-state-at-cut store supplied text)
    (let* ((base (obtain-state store supplied #f))
           (resolved (resolve-cut store (quote cut) text base)))
      (if (eq? (car resolved) (quote error))
          resolved
          (let ((verdict (cut-usable? base (cadr resolved))))
            (if (eq? verdict (quote usable))
                (begin
                  (hold-point! (quote read-cut-before-replay))
                  (let ((past (replay store (cadr resolved) #f)))
                    (if (cut-covers? (reduce-applied-cut past)
                                     (filter (lambda (e) (> (cdr e) 0)) (cadr resolved)))
                        past
                        (list (quote error) (quote cut-moved)
                              (list (quote asked) (cadr resolved))
                              (list (quote served) (reduce-applied-cut past))))))
                (list (quote error) (quote cut-unavailable)
                      (list (quote cut) (quote cut))
                      (list (quote reason) (if (pair? verdict) (cadr verdict) verdict))))))))

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

  (define (store-tags store . given)
    (let ((state (if (and (pair? given) (car given)) (car given) (open-and-reduce store))))
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
                        ;; EACH ENTRY WITH ITS CAUSAL COORDINATES, taken while
                        ;; this reduction is in hand: (entry cut past), the
                        ;; causal cut right after the event and right before it.
                        ((and (member (cons (entry-writer (car es)) (entry-seq (car es)))
                                      applied)
                              (or (not id) (touches? (car es) id)))
                         (let ((event (cons (entry-writer (car es)) (entry-seq (car es)))))
                           (loop (cdr es) (cons (list (car es) (state-event-cut r event) (state-event-past r event))
                                                out))))
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
                 (srcs (field-strings state from block (quote src)))
                 (keys (apply append (map (lambda (text) (map caddr (md-refs text))) srcs))))
            (loop (cdr ds)
                  (if (and (not (equal? from id)) (member id keys))
                      (cons from out)
                      out))))))

  ;; The state may be handed in, so a caller that says which state it read
  ;; (a receipt) reads its rows from that same state.
  (define (store-refs store id . given)
    (let ((state (if (pair? given) (car given) (open-and-reduce store))))
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

  ;; NEVER: ONE WRITER CANNOT PUT A BLOCK UNDER ITSELF. A move whose parent
  ;; is the block, or one of its descendants in the reduction this write is
  ;; checked against, is refused here, with the chain from the parent up to
  ;; the block. Two writers whose moves are each legal in their own history
  ;; can still make a cycle together; that one is a structural conflict
  ;; after the merge, as designed, and not this rule's.
  ;; THE WALK goes up settled parents from `parent`, and STOPS WITHOUT A
  ;; REFUSAL at root, at a position in conflict (the store does not choose
  ;; a candidate), at a block the reduction does not hold, and at a block
  ;; already visited (a cycle that is not this move's). A deleted ancestor
  ;; is a step: deletion keeps the position.
  ;; -> the chain `(<parent> ... <id>)`, or #f.
  (define (move-cycle state id parent)
    (cond
      ((eq? parent 'root) #f)
      ((equal? parent id) (list id))
      (else
       (let walk ((x parent) (chain (list parent)) (seen (list parent)))
         (let ((b (state-read state x)))
           (and b
                (let ((pos (cdr (assq 'position b))))
                  (and (pair? pos) (not (eq? (car pos) 'conflict))
                       (let ((up (car pos)))
                         (cond
                           ((or (eq? up 'root) (equal? up "root")) #f)
                           ((equal? up id) (reverse (cons id chain)))
                           ((member up seen) #f)
                           (else (walk up (cons up chain) (cons up seen)))))))))))))

  ;; HOW MANY LOGICAL EDGES A link OR unlink FINDS, before it is applied:
  ;; 0 or 1, since two link records of one (rel . to) are one edge.
  (define (edges-matched state payload)
    (let* ((b (state-read state (cadr payload)))
           (edges (if b (cdr (assq 'edges b)) '())))
      (if (member (cons (caddr payload) (cadddr payload)) edges) 1 0)))

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
;; A `set` of a text-mode block's src given as a string. The src of such a
  ;; block is its bytes; the string is written as its UTF-8. Only a proper,
  ;; non-empty list is one: a malformed intent -- an improper tail (r1
  ;; review), or an empty list, whose car would raise (r2 review) -- is left
  ;; for the validator that refuses it.
  (define (text-src-string? state i)
    (and (pair? i) (list? i) (eq? (car i) 'set)
         (pair? (cdr i)) (string? (cadr i))
         (pair? (cddr i)) (eq? (caddr i) 'src)
         (pair? (cdddr i)) (string? (cadddr i))
         (known? state (cadr i))
         (equal? (assq 'mode (block-fields state (cadr i))) '(mode . text))))

  ;; THE ONE PLACE A TEXT-MODE SRC BECOMES BYTES, and the intent is otherwise
  ;; returned as it is -- the same object, so nothing else about it changes.
  ;; resolve asks it of every `set`; write-plan-then! asks it before the
  ;; plan is written, against the state the plan starts from (F119). That
  ;; settles a set of an existing text-mode block's src. It does not settle
  ;; two others: a plan that first gives a block its mode and then sets its
  ;; src (the declaration is read before the mode exists), and an `expect`
  ;; wrapper with a fourth element, which is accepted by intent-reason but
  ;; not rebuilt here, so its declaration stays the string it was (NOTES,
  ;; KNOWN OPEN and ROUND 3).
  ;;
  ;; NEVER: A MALFORMED INTENT IS NOT REPAIRED HERE. An `expect` wrapper is
  ;; rebuilt only when it is a proper three-element list and its intent
  ;; changed; anything else goes on as it came, to be refused by
  ;; run-intents! as before (F119 r1 review: `(expect H (set ...) . junk)`
  ;; lost its tail and ran).
  (define (canonical-intent state intent)
    (cond
      ((and (list? intent) (= 3 (length intent)) (eq? (car intent) 'expect))
       (let ((inner (canonical-intent state (caddr intent))))
         (if (eq? inner (caddr intent)) intent (list 'expect (cadr intent) inner))))
      ((text-src-string? state intent)
       (list 'set (cadr intent) 'src (string->utf8 (cadddr intent))))
      (else intent)))

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
             ((text-src-string? state i) (canonical-intent state i))
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
             ((move-cycle state id parent)
              => (lambda (chain)
                   (list 'error 'would-cycle (list 'id id) (list 'parent parent) (list 'through chain))))
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
        ;; A DECLARATION NAMES NO BLOCK. A built-in's kind is its own and a
        ;; reserved name is never an edge, so neither is declared; a kind
        ;; outside the seven is refused with them, as a block's kind is.
        ;; The rest of the value's shape is the reducer's (payload-reason).
        ((relation)
         (let ((name (cadr i)) (value (caddr i)))
           (cond
             ((memq name effect-relation-names)
              (list 'error 'relation-is-built-in (list 'relation name)))
             ((memq name reserved-relation-names)
              (list 'error 'reserved-relation (list 'relation name)))
             ((and (pair? value) (not (memq (car value) declaration-kinds)))
              (list 'error 'malformed-intent
                    (list 'kind-not-known (list 'kind (datum-spelling (car value)))
                          (list 'known declaration-kinds))))
             (else (list 'relation name value)))))
        ;; A RULE NAMES NO BLOCK EITHER. Its value is checked by the query
        ;; library's own reading of a goal (query.sc, rule-value-check),
        ;; entered on use, so the verb and a batch's intent are judged by
        ;; the same procedure. THE VALUE MUST BE IN ITS ONE FORM, the class
        ;; included: the record is the intent, so a plan's member is the
        ;; intent it declared (request.sc, intent-produced?), and a value in
        ;; another order is refused with the form it would have.
        ((rule)
         (let ((v (and (= (length i) 3) ((eval 'rule-value-check (environment '(theourgia query))) (caddr i)))))
           (cond
             ((not v)
              (list 'error 'malformed-intent (list 'too-many-arguments (list 'verb 'rule) (list 'given (- (length i) 1)) '(needs 2))))
             ((and (pair? v) (eq? (car v) 'error)) v)
             ((not (equal? v (caddr i)))
              (list 'error 'bad-request 'rule-not-in-its-form (list 'form v)))
             (else (list 'rule (cadr i) v)))))
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
      ;; no-instance: a checkout of a store kept in git has no
      ;; instance.sexp until adopt mints one.
      ((integrity registry-ahead missing-generation no-instance) 'adopt)
      ((registry-inside-store) 'move-the-registry-outside-the-store)
      ;; the store's files were replaced under a daemon (daemon.sc,
      ;; segment-replaced): it serves what it no longer holds
      ((store-replaced) 'restart-the-daemon)
      (else #f)))

  ;; THE DETAIL A REASON KEEPS in the answer, after the reason and before the
  ;; remedy. Every other reason is answered by its name alone, as it always
  ;; was; store-replaced names the segment that was replaced.
  (define (detail-for why outcome)
    (case why
      ((store-replaced) (if (pair? (cdr outcome)) (cddr outcome) '()))
      (else '())))

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
                 (detail-for why outcome)
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
      ;; THE FAULT SEAM IS THE REAL RUN'S: a rehearsal does not consult it, so
      ;; it cannot consume a fault meant for the write that follows.
      (if (and (not (reduce-rehearsal? state)) (report-fault?))
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
  ;; THE PUBLISHED REDUCTION CARRIES WHAT ITS LOAD COULD NOT READ (K10).
  ;; A read the daemon answers from it later opens no load of its own, so
  ;; the reduction is the only place left that can say a writer is
  ;; missing; the session's load at the moment of publishing is the one
  ;; the state was delivered from.
  ;; THE GATE (F77c, design reviews r4 and r5): a session whose load was not
  ;; delivered -- aborted, or waiting on a reset -- has a partial state, and
  ;; a partial state is never published, whatever stopped it (ruling
  ;; Q-r5-1). The previous publication stays, the withholding is traced by
  ;; name, and the withhold hook asks for a fresh fold, which is judged like
  ;; any other load.
  (define (publish-after state s thunk)
    (let ((answers (thunk)))
      (if (session-delivered? s)
          (begin
            (remember-load-unreadable! state (session-load s))
            (publish-hook state))
          (begin
            (trace-event! 'publish-withheld
                          (if (session-reset-pending s)
                              'reset-pending
                              (load-outcome (session-load s)))
                          #f)
            (withhold-hook)))
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
  ;; WHAT A WITHHELD PUBLICATION ASKS FOR, set once like the publish hook.
  ;; The daemon's store process asks itself to reload; a CLI run holds no
  ;; publication and does nothing.
  (define withhold-hook (lambda () (if #f #f)))
  (define (store-withhold-hook! procedure)
    (unless (procedure? procedure)
      (assertion-violation 'store-withhold-hook! "not a procedure" procedure))
    (set! withhold-hook procedure))

  ;; HOW A RAISE IS ANSWERED, as the dispatcher answers it: rpc.sc installs
  ;; its own conversion here when it is loaded, on every route that
  ;; dispatches. Without it a raise goes on as it came.
  (define raise-answer-hook (lambda (e) (raise e)))
  (define (store-raise-answer-hook! procedure)
    (unless (procedure? procedure)
      (assertion-violation 'store-raise-answer-hook! "not a procedure" procedure))
    (set! raise-answer-hook procedure))

  ;; A GATE is the premises' preflight: #(premises-gate <check> <enter>
  ;; <also> <blocks>), made by (theourgia premises). with-store-write calls
  ;; ENTER once its session has begun, then asks CHECK; a plain procedure is
  ;; still a preflight. BLOCKS are the block ids the set holds, which a rule
  ;; check reads. A tagged vector, so nothing here loads the premises library.
  (define (premises-gate? p) (and (vector? p) (= (vector-length p) 5) (eq? (vector-ref p 0) 'premises-gate)))
  (define (premises-gate-check p) (vector-ref p 1))
  (define (premises-gate-enter p) (vector-ref p 2))
  (define (premises-gate-blocks p) (vector-ref p 4))

  ;; A SITE'S OWN CHECK, asked after the premises: the gate composes it
  ;; (keeping its entry); without a set, the plain composition.
  (define (premises-also gate own)
    (cond ((premises-gate? gate) ((vector-ref gate 3) own))
          (gate (lambda (state) (or (gate state) (own state))))
          (else own)))

  ;; THE PREMISES A COMMITTED WRITE IS ACCEPTED ON (`--premises <datum>`):
  ;;   (premises-preflight store text own) -> preflight, finish, run
  ;; Without the option, OWN, the identity and a plain runner, and nothing is
  ;; loaded; with it, the three faces of one compilation, made by (theourgia
  ;; premises), premises-faces, where what each face does is written.
  (define (premises-preflight store text own)
    (if (not text)
        (values own (lambda (answer) answer) (lambda (thunk) (thunk)))
        ((eval 'premises-faces (environment '(theourgia premises)))
         store text own (lambda (e) (raise-answer-hook e)))))

;; ---- the rehearsal --------------------------------------------------------
  ;;
  ;; WHEN A RULE OR A TYPED DECLARATION IS IN FORCE, A WRITE RUNS TWICE
  ;; THROUGH ONE CODE PATH. The rehearsal runs the writing branch against a
  ;; copy of the reduction (reduce-clone) and a dry session
  ;; (session-dry-clone): the same code, on equal inputs, handing out the
  ;; same ids. What it produced -- the copy after the branch, which holds the
  ;; prefix a failing expectation leaves -- is judged (theourgia rules); a
  ;; refusal is the write's answer and nothing is written. Otherwise the
  ;; branch runs again for real, under the same lock, and its answer is the
  ;; write's. A store with neither makes no copy and no dry session.
;; A REFUSAL OF THE JUDGEMENT: a rule's (error refused rule-violation |
  ;; rule-unevaluable ...) or a typed endpoint's (error bad-request
  ;; relation-endpoint ...). Every route answers it as it is, as it answers a
  ;; premise's refusal: a batch does not wrap it in its items, a template's
  ;; apply does not wrap it in template-apply-failed.
  (define (judgement-refusal? a)
    (and (list? a) (>= (length a) 3) (eq? (car a) 'error)
         (or (and (eq? (cadr a) 'refused) (memq (caddr a) '(rule-violation rule-unevaluable relation-endpoint-unevaluable)) #t)
             (and (eq? (cadr a) 'bad-request) (eq? (caddr a) 'relation-endpoint)))))

  (define (rehearsal-live? state)
    (or (pair? (state-declared-rules state))
        (exists (lambda (d) (and (= (length d) 4)
                                 (or (pair? (cadr (caddr d))) (pair? (cadr (cadddr d))))))
                (state-declared-relations state))))

  (define rules-judgement #f)
  (define (write-judgement-procedure)
    (or rules-judgement
        (begin (set! rules-judgement (eval 'write-judgement (environment '(theourgia rules))))
               rules-judgement)))

  ;; -> #f, or the refusal the write answers with.
  (define (rehearsal-refusal state s branch receipt)
    (and (rehearsal-live? state)
         (let ((clone (reduce-clone state))
               (dry (session-dry-clone s))
               (before (session-written-events s)))
           (trace-event! 'rehearsal (length before) '(rehearsal))
           (branch clone dry)
           ((write-judgement-procedure)
            state clone
            (write-targets clone (filter (lambda (e) (not (member e before))) (session-written-events dry)))
            receipt))))

  ;; THE TARGETS OF A WRITE: every block whose records it adds -- created,
  ;; set, moved, deleted, and either end of a linked or unlinked edge -- that
  ;; is live after it, read from the state it produced: a block it deletes is
  ;; no target (rules speak of live blocks), and neither is one it creates and
  ;; deletes, but a deletion the reduction did not apply leaves its block a
  ;; target. EVENTS are the write's own, ((<writer> . <seq>) ...), one
  ;; writer's and consecutive. -> ids, sorted.
  (define (write-targets post events)
    (let loop ((rs (if (null? events) '()
                       (state-written-records post (car (car events)) (cdr (car events))
                                              (+ 1 (cdr (list-ref events (- (length events) 1)))))))
               (ids '()) (writer (and (pair? events) (car (car events)))))
      (if (null? rs)
          (list-sort string<?
                     (filter (lambda (id)
                               (let ((row (state-read post id)))
                                 (and row (not (cdr (assq 'deleted row))))))
                             ids))
          (let* ((seq (caar rs)) (p (cdar rs))
                 (add (lambda (xs) (fold-left (lambda (acc x) (if (and (string? x) (not (member x acc))) (cons x acc) acc))
                                              ids xs))))
            (case (and (pair? p) (car p))
              ((put) (loop (cdr rs) (add (list (block-id writer seq))) writer))
              ((set move del) (loop (cdr rs) (add (list (cadr p))) writer))
              ((link unlink) (loop (cdr rs) (add (list (cadr p) (cadddr p))) writer))
              (else (loop (cdr rs) ids writer)))))))

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
      (let* ((s (log-begin store (deliver-into state #f)))
             ;; THE RECEIPT A RULE CHECK READS: the blocks the write's premise
             ;; set holds, or #f when it carried none.
             (receipt (and (premises-gate? preflight) (premises-gate-blocks preflight)))
             ;; ENTERED: the session has begun. A gate is told so here, and
             ;; its check is the preflight from now on.
             (preflight (if (premises-gate? preflight)
                            (begin ((premises-gate-enter preflight)) (premises-gate-check preflight))
                            preflight)))
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
              state s
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
                    ;; A NAMED OUTCOME CARRIES THE RECORD (F100b item 5).
                    ;; TRIPWIRE, NOT A MEASUREMENT: replay creates nothing
                    ;; before its barrier (the F100b probe's PR-15 reading),
                    ;; so the record here is empty today and the answer is
                    ;; byte-identical; the call is here for when it is not.
                    (list (guard (e (#t (with-written
                                          (list 'error 'unknown
                                                (list 'replay-barrier-failed
                                                      (failure-text e)))
                                          (mutation-record))))
                            (request-answer s store verdict)))
                    (commit-then s
                      (lambda ()
                        (let ((bad (and (not (and verdict (eq? (car verdict) 'complete)))
                                        (or (and preflight (preflight state))
                                            (datum-src-refusal state intents)
                                            (begin (announce-count! s intents)
                                                   (and req (cursor-unreachable store s req)))))))
                          ;; THE WRITING BRANCH, as one procedure of the reduction and
                          ;; the session it writes through. It runs once for real;
                          ;; when a rule or a typed declaration is live it runs first
                          ;; against a copy of the reduction and a dry session, and
                          ;; what that rehearsal produced is judged before anything
                          ;; is written (rehearsal-refusal).
                          (define (branch state s)
                           (cond
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
                             (write-batch! s state req intents))))
                          (cond
                            (bad (list bad))
                            ((rehearsal-refusal state s branch receipt) => list)
                            (else (branch state s))))))))))))
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
                     ((reserved-relation raw)
                      => (lambda (rel) (list 'error 'reserved-relation (list 'relation rel))))
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
                      (let ((ev (let ((e (assq 'events (cdr answer)))) (and e (cadr e)))))
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
                       ;; EITHER SPELLING OF THE TEXT IS CHECKED. A plan
                       ;; for a text-mode block declares its bytes (F119);
                       ;; checking strings only let such a plan complete
                       ;; with a version that named other bytes (F119 r1
                       ;; review).
                       (bytes (cond ((string? text) (string->utf8 text))
                                    ((bytevector? text) text)
                                    (else #f)))
                       (item (and block (assoc block items))))
                  (if (and bytes item
                           (not (equal? (cadr item)
                                        (draft-version bytes
                                                       (caddr item) (cadddr item)))))
                      block
                      (loop (cdr es))))))))))

  ;; ---- completing a plan -------------------------------------------------
  ;;
  ;; A RETRY FINISHES A PERSISTED PLAN FROM THE PLAN, and the frozen entries
  ;; are not a licence to write over what others wrote since: the
  ;; judgement (the plan's markers, the stale targets, the checks after each
  ;; member) is the completion library's, (theourgia completion). It is
  ;; entered here, when a completion is reached, and not imported: the
  ;; store is loaded at every start of the command line, and a command that
  ;; never completes a plan should not pay for loading the judgement.
  ;;
  ;; The frozen text is checked against the version its consumes item names
  ;; first, here, as before; the library is given the run, which stays the
  ;; store's.
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
          ((eval 'completion-run (environment '(theourgia completion)))
           state plan-event entries missing indices consumes
           (lambda (run after-each)
             (session-pending-count-set! s (length missing))
             (run-intents! s state
                           (lambda (n) (request-actor req (list-ref indices n) plan-event))
                           run after-each))))))

  ;; NEVER: THE PLAN DECLARES WHAT ITS RECORDS WILL CARRY (F119), for a set of
  ;; an existing text-mode block's src -- the case canonical-intent settles;
  ;; its comment names the two it does not (a mode set earlier in the same
  ;; plan, and an `expect` wrapper with a fourth element). The reducer
  ;; holds each record of a plan to the plan's declared entry, verbatim for
  ;; `set` (request.sc, intent-produced?). resolve writes a text-mode
  ;; block's src as bytes, so a plan that declared the caller's string --
  ;; a commit's draft, from working.sc entry-intent -- met a record that
  ;; carried bytes: plan-mismatch, and the commit answered ok while its
  ;; record waited in pending for ever. The intents are made canonical
  ;; here, once, and both the plan and the run below use them.
  (define (write-plan-then! s state req intents . rest)
    (let* ((intents (map (lambda (i) (canonical-intent state i)) intents))
           (entries (plan-entries intents))
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
            ;; A NAMED OUTCOME CARRIES THE RECORD (F100b item 5, NO3): the
            ;; partial write's own entry is in it.
            (if (not (eq? (car outcome) 'committed))
                (with-written (write-outcome->answer outcome seq) (mutation-record))
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

  ;; ONLY A WELL-FORMED REFERENCE BECOMES A MARKER. A malformed one -- `(from)`,
  ;; `(from 0 extra)`, `(from -1)` -- is kept as written, so the plan holds
  ;; what the run refuses (resolve-from), and a completion cannot bind a
  ;; reference the first run never accepted.
  (define (as-marker x)
    (if (and (pair? x) (eq? (car x) 'from) (pair? (cdr x)) (null? (cddr x))
             (integer? (cadr x)) (exact? (cadr x)) (>= (cadr x) 0))
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
  ;;
  ;; AND EITHER END OF AN EDGE, so a write can create a block and link it
  ;; in one request: a rule about the new block can then see its edges.
  ;;
  ;; NEVER: ONE TABLE SAYS WHERE A REFERENCE MAY STAND, by intent kind and
  ;; position (0 is the verb), and every reader of references reads it:
  ;; the resolver below, the plan's declared intents (declared-intent), and
  ;; the completion's binding of a plan's markers (completion.sc). A
  ;; position known to one of them and not the others is a reference one
  ;; route resolves and another hands on unbound.
  (define intent-ref-positions
    '((insert 1 2) (move 2 3) (link 1 3) (unlink 1 3)))

  ;; A position the intent is too short to have reads as #f, so a plan is
  ;; still written for it and its run answers too-few-arguments, as any
  ;; run does.
  (define (intent-refs intent)
    (let* ((i (unwrap intent)) (e (and (pair? i) (assq (car i) intent-ref-positions))))
      (if e
          (map (lambda (k) (and (list? i) (< k (length i)) (list-ref i k))) (cdr e))
          '())))

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
           (e (assq (car i) intent-ref-positions))
           ;; THE INTENT IS REBUILT AT ITS FOUR PARTS, as the insert and move
           ;; arms always rebuilt it: a part past the fourth is not carried
           ;; into the resolved intent or the plan's declaration.
           (rewritten
             (if e
                 (let loop ((l i) (k 0) (vs (list parent after)))
                   (cond ((or (null? l) (= k 4)) '())
                         ((and (pair? vs) (memv k (cdr e))) (cons (car vs) (loop (cdr l) (+ k 1) (cdr vs))))
                         (else (cons (car l) (loop (cdr l) (+ k 1) vs)))))
                 i)))
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
      (link . 4) (unlink . 4) (tag . 2) (relation . 3) (rule . 3)))

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
    '((set . (2)) (link . (2)) (unlink . (2)) (relation . (1)) (rule . (1))))

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
  ;; BACK-REFERENCES ARE RESOLVED ONLY WHERE `intent-ref-positions` SAYS --
  ;; an insert's parent and predecessor, a move's, and either end of a link
  ;; or an unlink. A `(from n)` anywhere else is never substituted and
  ;; reaches the reader as a list.
  (define (plain-id? x) (string? x))
  (define (end-id? x) (or (string? x) (and (pair? x) (eq? (car x) 'from))))
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
          (cons 'link (list (cons 1 end-id?) (cons 3 end-id?)))
          (cons 'unlink (list (cons 1 end-id?) (cons 3 end-id?)))))

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

  ;; THE RELATION NAMES AN EDITOR'S FACTS ANSWER UNDER ARE NOT WRITTEN AS
  ;; EDGES. `refs` joins supplied `calls` and computes `ref`; `uses` and
  ;; `guards` are kept for the same kind of fact. An edge written under one
  ;; of these names would answer beside the supplied one and be read as
  ;; it, so link and unlink refuse them here, where a request's intent is
  ;; checked -- NOT in the reducer's payload check: a record already in a
  ;; log still applies, and `check` names it (reserved-relations). The
  ;; names are the reducer's list, reserved-relation-names.
  ;; -> the relation, or #f.
  (define (reserved-relation raw)
    (and (not (malformed-intent? raw))
         (let ((u (unwrap raw)))
           (and (memq (car u) '(link unlink)) (memq (caddr u) reserved-relation-names) (caddr u)))))

  ;; AFTER-EACH, when given, is called after every intent that answered ok,
  ;; with its index and its answer; #f goes on, anything else is the answer
  ;; that ends the run, after the ok. The table of blocks made stays here.
  (define (run-intents! s state actor-at intents . rest)
    (define after-each (and (pair? rest) (car rest)))
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
                           ((reserved-relation raw)
                            => (lambda (rel) (list 'error 'reserved-relation (list 'relation rel))))
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
            (cond
              ((eq? (car answer) 'error)
               (reverse (cons answer out)))
              ((and after-each (after-each n answer))
               => (lambda (stop) (reverse (cons stop (cons answer out)))))
              (else
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
                      (let ((ev (let ((e (assq 'events (cdr answer)))) (and e (cadr e)))))
                        (if (and (eq? 'insert (car (unwrap fixed))) (pair? ev))
                            (cons (cons n (block-id (car (car ev)) (cdr (car ev)))) made)
                            made))
                      (cons answer out))))))))

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
                      ;; LOCAL ADMISSION: a declaration whose whole value is
                      ;; the one in force -- or a retirement of a name with
                      ;; none in force -- writes nothing. Not for a tracked
                      ;; request: a plan's sub-operations, and a tracked
                      ;; batch's items, were declared before they ran, and
                      ;; each writes the record its receipt counts on. The
                      ;; record of the same value is harmless: agreement is
                      ;; by value.
                      ((and (memq (car payload) '(relation rule)) (not (request-actor? actor))
                            (equal? (caddr payload)
                                    (let ((d (if (eq? (car payload) 'rule)
                                                 (state-rule state (cadr payload))
                                                 (state-declaration state (cadr payload)))))
                                      (if (and d (eq? (car d) 'in-force)) (cadr d)
                                          (and (or (not d) (eq? (car d) 'retired)) 'retired)))))
                       '(ok (unchanged)))
                      (else
                        (let* ((deps (deps-for-payload state writer payload))
                               ;; COUNTED BEFORE THE RECORD IS APPLIED: what the
                               ;; writer's link or unlink found, not what it left.
                               (matched (and (memq (car payload) '(link unlink))
                                             (edges-matched state payload)))
                               (frame (make-frame (view-revision v) (view-epoch v)
                                                  writer seq actor deps payload))
                               (outcome (session-append! s frame)))
                          ;; NOT EVERY FAILURE MEANS NOTHING HAPPENED.
                          ;; `refused-before-reserve` is the only outcome
                          ;; that says the log is untouched; the record
                          ;; does not exist and the answers given so far
                          ;; are the exact committed prefix.
                          ;; `partial-write` leaves bytes that a later
                          ;; replay may well read back as a committed
                          ;; record. (`written-fsync-failed` was named
                          ;; here too; no append path produces it on
                          ;; this tree -- the F100b probe's PR-15
                          ;; reading -- so only partial-write reaches
                          ;; this branch today.) Answering
                          ;; those as a plain error claims a prefix that
                          ;; replay then contradicts -- the caller is
                          ;; told intent k failed and finds it applied.
                          ;; They get their own answer, and it says the
                          ;; outcome is not known rather than known to
                          ;; be nothing.
                          ;; A NAMED OUTCOME CARRIES THE RECORD (F100b
                          ;; item 5, NO3).
                          (if (not (eq? (car outcome) 'committed))
                              (with-written (write-outcome->answer outcome seq) (mutation-record))
                              (begin
                                ;; THE ACTOR GOES IN HERE TOO -- see the
                                ;; note in `append-payload!`.
                                (reduce-apply! state writer seq deps payload actor)
                                (session-applied! s (session-epoch s)
                                                  (reduce-applied-cut state))
                                ;; NEVER: A link OR unlink SAYS WHAT IT MATCHED.
                                ;; An unlink that names no edge is still a
                                ;; record (the set semantics: an edge the
                                ;; writer has not seen survives it), and
                                ;; `(matched 0)` is how the caller learns it
                                ;; changed nothing here.
                                (append
                                  (list 'ok
                                        (list 'events (list (cons writer seq)))
                                        (state-section state payload writer seq)
                                        (list 'cursor (cons writer seq))
                                        (list 'replay #f))
                                  (if matched (list (list 'matched matched)) '()))))))))))))))

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
           (and (not (eq? (entry-type path) 'absent)) (car ws)))
         (car ws))
        (else (loop (cdr ws))))))

  ;; THE PATTERNS A STORE'S .gitignore HOLDS, anchored at the store: the
  ;; instance identity, the evidence checkpoint, snapshots, the tables of
  ;; facts an editor supplied (derived/, rebuilt by the next supply), each
  ;; writer's working area and draft lock, and retained atomic-write temporaries
  ;; anywhere below the store -- EXCEPT a writer's incoming/ files, which
  ;; the negation brings back: a staged candidate kept there after a failure
  ;; is evidence the request index scans and a replay answers from, so a
  ;; checkout without it could answer a request differently.
  (define store-gitignore
    (string-append
      "/instance.sexp\n"
      "/request-index.sexp\n"
      "/snap/\n"
      "/derived/\n"
      "/writers/*/working/\n"
      "/writers/*/draft.lock\n"
      "*.tmp-*\n"
      "!/writers/*/incoming/*\n"))

  (define (store-init! store)
    (cond
      ((not (eq? (entry-type (string-append store "/meta.sexp")) 'absent))
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
         ;; overwrite-entry!, the door's truncating open (F100a): created
         ;; empty, or emptied in place with its inode kept, as before.
         (overwrite-entry! (string-append store "/lock") (make-bytevector 0))
         ;; A STORE KEPT IN GIT: what this INSTANCE owns, or can rebuild,
         ;; stays out of a checkout -- its identity, the evidence
         ;; checkpoint, snapshots, the working areas and their locks, and
         ;; the temporaries an atomic write keeps after a failure. The log
         ;; itself, and everything else, travels with `git add <store>`.
         ;; It is written BEFORE meta.sexp, so a store whose init failed
         ;; part-way already keeps a retained temporary out of a commit.
         (atomic-write! (string-append store "/.gitignore")
                        (string->utf8 store-gitignore)
                        'registry)
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
         (overwrite-entry! (string-append store "/writers/" writer "/" (segment-file-name 1))
                           (make-bytevector 0))
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
;; NEVER: A DRAFT IS TEXT, AND A DATUM BLOCK'S CODE IS ITS BODY. A draft
  ;; committed to a block whose mode is datum landed as a src beside the
  ;; body: the editor showed the text, while export-code --datum, whereis
  ;; and eval read the body, so an edit reported saved never took effect.
  ;; `write` and `commit` refuse such a block by this one predicate and this
  ;; one refusal, and `check` names a datum block that already has a src.
  (define (datum-mode-block? state id)
    (let* ((b (state-read state id))
           (fs (and b (assq (quote fields) b)))
           (m (and fs (assq (quote mode) (cdr fs)))))
      (and m (eq? (cdr m) (quote datum)))))
  (define (draft-on-datum-refusal id)
    (list (quote error) (quote bad-request) (quote draft-on-datum-unsupported)
          (list (quote block) id) (list (quote use) (quote def))))
  ;; `set <id> src <text>` IS THE OTHER ROUTE TO A SRC, and a `batch` holding
  ;; one: refused the same way before the first record, so a batch is
  ;; refused whole. `set <id> src` with no value still runs, since it takes
  ;; away a src written before the rule.
  ;;
  ;; The block a `set` of src with a value would give a src, when it is a
  ;; datum block; #f otherwise. A malformed intent is #f here and is left
  ;; for the validator that refuses it.
  (define (datum-src-target state x)
    (let ((i (if (and (list? x) (= 3 (length x)) (eq? (car x) 'expect)) (caddr x) x)))
      (and (list? i) (= 4 (length i)) (eq? (car i) 'set) (eq? (caddr i) 'src)
           (string? (cadr i)) (datum-mode-block? state (cadr i))
           (cadr i))))
  ;; The refusal for the first intent that would give a datum block a src,
  ;; or #f.
  (define (datum-src-refusal state intents)
    (let ((hit (and (list? intents) (find (lambda (x) (datum-src-target state x)) intents))))
      (and hit (draft-on-datum-refusal (datum-src-target state hit)))))
  ;; The live datum blocks that carry a src, in the store's order.
  (define (datum-blocks-with-src state)
    (filter (lambda (id)
              (let ((b (state-read state id)))
                (and b (not (cdr (assq (quote deleted) b)))
                     (datum-mode-block? state id)
                     (assq (quote src) (cdr (assq (quote fields) b)))
                     #t)))
            (map cadr (state-datum state))))

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
           ;; A WRITER THAT COULD NOT BE READ HAS NO END TO REPORT. Its entry
           ;; keeps the usual shape and says `unreadable` where the numbers
           ;; would be; its note, in integrity, names the path and the reason.
           ;; It is left out of the snapshot coverage rather than counted as 0,
           ;; which would be read as a writer that published nothing.
           (unreadable? (lambda (p) (and p (eq? (discovery-origin p) 'unreadable))))
           (coverage (map (lambda (e)
                            (cons (car e) (if (cdr e) (discovery-end-seq (cdr e)) 0)))
                          (filter (lambda (e) (not (unreadable? (cdr e)))) prefixes)))
           (per-writer
             (map (lambda (e)
                    (let ((p (cdr e)))
                      (list (car e)
                            (list 'end (cond ((unreadable? p) 'unreadable)
                                             (p (discovery-end-seq p))
                                             (else 0)))
                            (list 'torn (cond ((unreadable? p) 'unreadable)
                                              (else (and p (discovery-torn p) #t))))
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
           (state (open-and-reduce store))
           (notes (reduce-noted state))
           ;; A PATH TWO LIBRARIES, OR TWO FILES, HOLD IS REPORTED, AND IT IS
           ;; NOT DAMAGE: the log is whole and every record applies; what the
           ;; store holds is two identities for one path, which an export
           ;; refuses. The clause is there only when there is such a path, as
           ;; local-writer is there only when there is a writer, and the
           ;; verdict is then `duplicates` unless the store is damaged.
           (duplicated (state-duplicated-paths state))
           ;; AN OLD RECORD UNDER A RESERVED RELATION NAME IS REPORTED, AND
           ;; IT IS NOT DAMAGE: it was a valid edge when written and still
           ;; applies; `refs` would now show it beside supplied facts under
           ;; the same name. Present only when there is one; the verdict is
           ;; unchanged by it.
           ;; NEVER: NOT IN THE ORDER THE STATE HOLDS THEM. That is delivery
           ;; order, and delivery order is the route's: a reduction seeded
           ;; from a snapshot or kept by the daemon delivers what came after
           ;; it, a fresh replay delivers writer by writer, so one store
           ;; listed the same records in two orders. Sorted by event, writer
           ;; then seq, the clause is the same on every route.
           (reserved (list-sort
                       (lambda (x y)
                         (let ((a (cdr (list-ref x 3))) (b (cdr (list-ref y 3))))
                           (or (string<? (car a) (car b))
                               (and (string=? (car a) (car b)) (< (cadr a) (cadr b))))))
                       (state-reserved-relation-records state)))
           ;; A DATUM BLOCK THAT CARRIES A SRC is reported, and it is not
           ;; damage: its src was committed as a draft before commit refused
           ;; one, and nothing that runs or exports the block reads it.
           ;; Present only when there is one; the verdict is unchanged.
           (datum-src (datum-blocks-with-src state))
           ;; THE RULES AND TYPED ENDPOINTS IN FORCE, AUDITED (theourgia rules,
           ;; state-audit): each state rule over every live block of a kind it
           ;; lists, a write rule listed once as skipped (no write is in
           ;; hand), every typed edge whose end its selector does not hold.
           ;; Reported, and not damage: the verdict is unchanged. A store with
           ;; neither asks nothing.
           (audit (and (rehearsal-live? state)
                       (call-with-values (lambda () ((eval 'state-audit (environment '(theourgia rules))) state))
                                         list)))
           (rules (if audit (car audit) '()))
           (endpoints (if audit (cadr audit) '()))
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
            (list 'notes notes))
        (if (pair? duplicated) (list (list 'paths duplicated)) '())
        (if (pair? reserved) (list (list 'reserved-relations reserved)) '())
        (if (pair? datum-src) (list (list 'datum-with-src datum-src)) '())
        (if (pair? rules) (list (cons 'rules rules)) '())
        (if (pair? endpoints) (list (cons 'relation-endpoints endpoints)) '())
        ;; THE VERDICT SAYS IT TOO: `damaged` first, then `duplicates`, then
        ;; `ok`. A health verb that answered ok, and exited 0, on a store an
        ;; export refuses said nothing; any verdict but ok exits 1.
        (list
            (list 'verdict (cond ((or damaged? (pair? notes) (registry-inside-store?)) 'damaged)
                                 ((pair? duplicated) 'duplicates)
                                 (else 'ok)))))))

  ;; A SNAPSHOT THAT CANNOT BE USED IS NOT DAMAGE TO THE STORE -- the log
  ;; still loads and the state is still right, it just has to be rebuilt
  ;; from further back. So these are reported and do not decide the
  ;; verdict; the writers' integrity does.
  (define (check-snapshots store coverage)
    (let ((dir (string-append store "/snap")))
      (if (eq? (entry-type dir) 'absent)
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
