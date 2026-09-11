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
  (export open-and-reduce with-store-write store-init! nearest-ids)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs io ports) (rnrs files)
          (rnrs arithmetic fixnums) (rnrs unicode) (rnrs bytevectors)
          (only (theourgia log)
                log-open load-deliver! load-commit!
                load-snapshot-cut load-snapshot-rows
                log-begin log-end! session-view session-append! session-applied!
                session-epoch make-frame atomic-write! segment-file-name
                instance-install! owner-install! writer-directory store-writers
                store-register!
                view-revision view-epoch view-writer view-expect-seq)
          (only (theourgia ffi) mkdir-p! wall-clock-ms process-id)
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

  (define (nearest-ids state id)
    (define (shared a b)
      (let loop ((i 0))
        (if (or (>= i (string-length a)) (>= i (string-length b))
                (not (char=? (string-ref a i) (string-ref b i))))
            i
            (loop (+ i 1)))))
    (let ((ids (map cadr (state-datum state))))
      (let loop ((xs (list-sort (lambda (a b)
                                  (let ((sa (shared id a)) (sb (shared id b)))
                                    (if (= sa sb) (string<? a b) (> sa sb))))
                                ids))
                 (n 0) (out '()))
        (if (or (null? xs) (= n 3))
            (reverse out)
            (loop (cdr xs) (+ n 1) (cons (car xs) out))))))

  (define (known? state id) (and (state-read state id) #t))

  (define (deleted? state id)
    (let ((b (state-read state id)))
      (and b (cdr (assq 'deleted b)))))

  ;; THE DEPS ARE THE APPLIED CUT WITHOUT THIS WRITER. The cut is
  ;; causally closed by construction -- it is what the reduction has
  ;; applied -- and a writer's own previous event is a premise whether it
  ;; is named or not, so naming it would be the one entry that says
  ;; nothing. With only a local writer this is the empty list; with a
  ;; mirrored writer it carries that writer's last applied sequence.
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
      ((integrity registry-ahead) 'adopt)
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
          (list 'error 'no-view)
          (let ((writer (view-writer v))
                (seq (view-expect-seq v)))
            (let ((bad (check-expectation state intent)))
              (if bad
                  bad
                  (let ((payload (resolve state writer seq intent)))
                    (if (eq? (car payload) 'error)
                        payload
                        (let* ((deps (deps-for state writer))
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
)
