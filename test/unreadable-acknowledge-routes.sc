;; Copyright 2018 - 2026 The Theourgia Authors
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

;; F77c's ROUTES: how a reduction missing a writer is obtained, refused,
;; named and never published, beyond the acceptance cells in
;; unreadable-acknowledge.sc.
;;
;; A: the census of every call of a load primitive, each with its
;;    disposition, derived from the tree's source by a scan.
;; B: the library, in process: open-load's one exit handler, the abort
;;    cause, the overlay's notes, the seal, the declaration's extent.
;; C: a program held between discovery and delivery (THEOURGIA_HOLD) while a
;;    segment changes under it.
;; D: the undeclared verbs refuse, and write nothing.
;; E: a daemon: the probe's markers, the publication gate, the refresh
;;    guards, two requests of different classes side by side.
;;
;; Each row that makes an entry unreadable restores it before anything is
;; compared.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch rpc-ok?)
        (only (theourgia store) obtain-state open-and-reduce)
        (only (theourgia working) working-state)
        (only (theourgia log) log-open load-deliver! load-commit! log-publish! segment-sha store-id-of
              writer-directory segment-file-name load-listener-add! load-listener-remove!
              load-refused? load-refused-condition load-declaration-set! load-declaration-of)
        (only (theourgia incomplete) incomplete-accepted incomplete-reduction?
              incomplete-reduction-notes)
        (only (theourgia ffi) unreadable-entry? unreadable-entry-path current-lock-release)
        (only (theourgia client) call! envelope-version socket-path serve-log-path)
        (only (theourgia render) render-wire)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia crc32) crc32-string-hex)
        (only (theourgia reduce) block-id reduce-applied-cut state-read))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/unreadable-acknowledge-routes-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'unreadable-acknowledge-routes "scratch directory already exists" root))
(system (string-append "mkdir -p " root))
(define sock-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define (sh . xs) (system (apply string-append xs)))
(define (chmod! mode path) (sh "chmod " mode " '" path "'"))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (text-of-file p)
  (if (file-exists? p)
      (let ((t (call-with-input-file p get-string-all))) (if (eof-object? t) "" t))
      ""))
(define (lines-of-text t)
  (let loop ((cs (string->list t)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (has-substring? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (prefix? p s) (and (<= (string-length p) (string-length s)) (string=? p (substring s 0 (string-length p)))))
(define (first-datum-of p)
  (guard (e (#t 'UNREADABLE)) (let ((d (call-with-input-file p read))) (if (eof-object? d) 'NO-ANSWER d))))
(define (wait-until-file p limit-ms)
  (let loop ((k 0))
    (cond ((file-exists? p) #t)
          ((>= (* k 100) limit-ms) #f)
          (else (sh "sleep 0.1") (loop (+ k 1))))))
(define (wait-until-text p limit-ms)
  (let loop ((k 0))
    (cond ((> (string-length (text-of-file p)) 0) #t)
          ((>= (* k 100) limit-ms) #f)
          (else (sh "sleep 0.1") (loop (+ k 1))))))
(define (touch! p) (call-with-output-file p (lambda (o) (write 'go o)) 'truncate))

;; An answer's clause of the given head, or #f (an answer's second element is
;; its kind, a symbol, so the clauses are searched and not assq'd).
(define (clause-of answer head)
  (and (pair? answer) (list? answer)
       (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr answer))))
;; The paths an answer's incomplete clause names, or no-clause.
(define (incomplete-paths answer)
  (let ((c (clause-of answer 'incomplete)))
    (if (not c)
        'no-clause
        (map (lambda (u) (let ((p (and (pair? u) (assq 'path (cdr u))))) (and p (cadr p))))
             (cdr c)))))
;; How many incomplete clauses an answer has (one is the rule).
(define (incomplete-clause-count a)
  (and (pair? a) (list? a)
       (length (filter (lambda (x) (and (pair? x) (eq? (car x) 'incomplete))) (cdr a)))))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))

;; ---- the fixture ------------------------------------------------------------
;;
;; `(store writer B)`: a local writer with a block B and a draft of it, and a
;; mirror M published in TWO segments, so that an OLDER segment exists. The
;; current segment's bytes are kept by discovery and not read again at
;; delivery; an older one is read at delivery.
;; THE MIRROR SORTS LAST. A load delivers writer by writer in name order
;; (store-writers sorts), so a mirror whose delivery aborts before the local
;; writer's leaves the session no view: D-snapshot-readable answered
;; no-local-writer in F77c r1f and not-delivered in r1e, by whichever side of
;; "mirrorz9" the random local writer id fell. "zzzzzzzz" is the last writer
;; id there is.
(define M "zzzzzzzz")
(define n 0)
(define (ask store . req) (rpc-dispatch store req "test"))
(define (new-id answer)
  (let ((ev (and (pair? answer) (assq 'events (cdr answer)))))
    (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define (fresh-store!)
  (fresh-store-publishing!
    (list (encode-record 1 1757300000001 "peer" '() '(set "t" x "x"))
          (encode-record 2 1757300000002 "peer" '() '(set "t" y "y")))))
;; The same store with the mirror's records given: record k published as
;; the mirror's segment k.
(define (fresh-store-publishing! mirror-records)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n)))
         (st (string-append d "/store")))
    (sh "mkdir -p " st " " d "/home " d "/ext")
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let* ((init (ask st 'init))
           (w (cadr (assq 'writer (cdr init))))
           (B (new-id (ask st 'insert "--under" "root" "--title" "Beta" "--text" "a needle")))
           (_ (ask st 'write B "draft text" "--writer" w)))
      (let loop ((rs mirror-records) (k 1))
        (unless (null? rs)
          (log-publish! st M k (car rs) (segment-sha (car rs)))
          (loop (cdr rs) (+ k 1))))
      (list st w B (string-append d "/ext")))))
(define (mirror-dir st) (writer-directory st M))
(define (older-segment st) (string-append (mirror-dir st) "/" (segment-file-name 1)))
(define (current-segment st) (string-append (mirror-dir st) "/" (segment-file-name 2)))
;; READABLE DAMAGE: one byte of the older segment changed, the size kept, so
;; the file reads and its checksum does not verify.
(define (damage! path)
  (let* ((bv (call-with-port (open-file-input-port path) get-bytevector-all))
         (i (quotient (bytevector-length bv) 2)))
    (bytevector-u8-set! bv i (fxlogxor (bytevector-u8-ref bv i) 1))
    (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv)))
    bv))
(define (undamage! path bv)
  (let ((i (quotient (bytevector-length bv) 2)))
    (bytevector-u8-set! bv i (fxlogxor (bytevector-u8-ref bv i) 1))
    (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv)))))

;; =============================================================================
(printf "== A: the census of the load primitives ==\n")
;; EVERY CALL OF A LOAD PRIMITIVE IN THE TREE'S SOURCE, BY SCAN (F77c, plan
;; amendment A1 and its acceptance): a call that is not listed below with its
;; disposition is a red row, and so is a listed one that is gone. The key is
;; (file, the nearest enclosing `(define (<name>`, primitive) with a count --
;; never a line number, which drifts.
;; DISPOSITIONS:
;;   request   the load takes the request's declaration (A2): declared for a
;;             verb that carries the clause, undeclared for export/import/
;;             def/snapshot, undeclared outside any request
;;   builder   the load primitive's own definition site in log.sc / store.sc
;;   unsealer  obtain-state: a supplied state is judged where it is consumed
;;   daemon    the daemon's own fold, explicitly declared: it holds the state
;;   published the one reader of the daemon's publication (answer-published)
;;   notes     rows->state building a new value: from a base it keeps the
;;             base's notes (overlay-drafts), or it is the replay's own seed
(define census
  '(("code-project.sc" "check-id!" "open-and-reduce" 1 request)
    ("code-project.sc" "export-code" "open-and-reduce" 1 request)
    ("core.sc" "eval-view" "open-and-reduce" 1 request)
    ("daemon.sc" "answer-published" "published-state" 1 published)
    ("daemon.sc" "store-loop" "obtain-state" 2 daemon)
    ("datum-project.sc" "current" "open-and-reduce" 1 request)
    ("datum-project.sc" "def-datum" "open-and-reduce" 1 request)
    ("datum-project.sc" "export-datum" "open-and-reduce" 1 request)
    ("datum-project.sc" "field-changes!" "open-and-reduce" 1 request)
    ("eval-runner.sc" "project-and-run" "open-and-reduce" 1 request)
    ("eval-worker.sc" "answer" "open-and-reduce" 2 request)
    ("log.sc" "log-begin" "open-load" 1 builder)
    ("log.sc" "log-open" "open-load" 1 builder)
    ("log.sc" "log-open-in-session" "open-load" 1 builder)
    ("log.sc" "reload!" "open-load" 1 builder)
    ("project.sc" "export-md" "open-and-reduce" 1 request)
    ("rpc.sc" "reduction-for" "obtain-state" 1 unsealer)
    ("store.sc" "replay" "log-open" 1 builder)
    ("store.sc" "replay" "rows->state" 1 notes)
    ("store.sc" "store-check" "log-open" 1 request)
    ("store.sc" "store-check-with" "open-and-reduce" 1 request)
    ("store.sc" "store-conflicts" "open-and-reduce" 1 request)
    ("store.sc" "store-diff" "open-and-reduce" 1 request)
    ("store.sc" "store-evidence" "open-and-reduce" 1 request)
    ("store.sc" "store-grep" "open-and-reduce" 1 request)
    ("store.sc" "store-log" "log-open" 1 request)
    ("store.sc" "store-refs" "open-and-reduce" 1 request)
    ("store.sc" "store-search" "open-and-reduce" 1 request)
    ("store.sc" "store-snapshot!" "log-begin" 1 request)
    ("store.sc" "store-tags" "open-and-reduce" 1 request)
    ("store.sc" "with-store-write" "log-begin" 1 request)
    ("working.sc" "committed-parent" "open-and-reduce" 1 request)
    ("working.sc" "overlay-drafts-rows" "rows->state" 1 notes)
    ("working.sc" "working-baseline" "obtain-state" 1 unsealer)
    ("working.sc" "working-commit!" "open-and-reduce" 1 request)
    ("working.sc" "working-list" "obtain-state" 1 unsealer)
    ("working.sc" "working-read" "obtain-state" 1 unsealer)
    ("working.sc" "working-restore!" "obtain-state" 1 unsealer)
    ("working.sc" "working-restore!" "open-and-reduce" 1 request)
    ("working.sc" "working-snapshot" "obtain-state" 1 unsealer)
    ("working.sc" "working-state" "obtain-state" 1 unsealer)
    ("working.sc" "working-state" "open-and-reduce" 2 request)
    ("working.sc" "working-write!" "obtain-state" 1 unsealer)
    ("working.sc" "working-write!" "open-and-reduce" 1 request)))
(define primitives '("open-load" "log-open" "log-open-in-session" "log-begin"
                     "rows->state" "open-and-reduce" "obtain-state" "published-state"))
;; A line's code: the text before its first `;` outside a string.
(define (code-of line)
  (let loop ((i 0) (in-string #f))
    (cond ((= i (string-length line)) line)
          ((and (char=? (string-ref line i) #\\) in-string) (loop (+ i 2) in-string))
          ((char=? (string-ref line i) #\") (loop (+ i 1) (not in-string)))
          ((and (char=? (string-ref line i) #\;) (not in-string)) (substring line 0 i))
          (else (loop (+ i 1) in-string)))))
(define (delimiter? s i) (or (= i (string-length s)) (memv (string-ref s i) '(#\space #\tab #\) #\newline))))
;; The calls of `name` in one code line: "(name" followed by a delimiter.
(define (calls-in code name)
  (let ((open (string-append "(" name)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i (string-length open)) (string-length code)) k)
            ((and (string=? (substring code i (+ i (string-length open))) open)
                  (delimiter? code (+ i (string-length open))))
             (loop (+ i 1) (+ k 1)))
            (else (loop (+ i 1) k))))))
(define (definer code)
  (let* ((s (let skip ((i 0)) (if (and (< i (string-length code)) (char=? (string-ref code i) #\space)) (skip (+ i 1)) i)))
         (t (substring code s (string-length code))))
    (and (prefix? "(define (" t)
         (let loop ((i 9))
           (if (or (= i (string-length t)) (memv (string-ref t i) '(#\space #\))))
               (substring t 9 i)
               (loop (+ i 1)))))))
(define (scan-file file)
  (let loop ((ls (lines-of-text (text-of-file (string-append "../" file)))) (cur #f) (acc '()))
    (if (null? ls)
        acc
        (let* ((code (code-of (car ls)))
               (d (definer code))
               (cur (or d cur)))
          (loop (cdr ls) cur
                (fold-left
                  (lambda (acc name)
                    (let ((k (- (calls-in code name)
                                (if (and d (string=? d name)) 1 0))))
                      (if (> k 0)
                          (let ((key (list file cur name)))
                            (let ((e (assoc key acc)))
                              (if e
                                  (cons (cons key (+ (cdr e) k)) (remp (lambda (x) (equal? (car x) key)) acc))
                                  (cons (cons key k) acc))))
                          acc)))
                  acc primitives))))))
(define source-files
  (append (filter (lambda (f) (let ((l (string-length f))) (and (> l 3) (string=? ".sc" (substring f (- l 3) l)))))
                  (directory-list ".."))
          (map (lambda (f) (string-append "mcp/" f))
               (filter (lambda (f) (let ((l (string-length f))) (and (> l 3) (string=? ".sc" (substring f (- l 3) l)))))
                       (directory-list "../mcp")))))
(define scanned (apply append (map scan-file source-files)))
(want "A CONTROL the scan sees the replay's log-open in store.sc, and it read more than twenty files"
      (list (assoc '("store.sc" "replay" "log-open") scanned) (> (length source-files) 20))
      (list (cons '("store.sc" "replay" "log-open") 1) #t))
(want "A every call of a load primitive in the tree is listed with its count and a disposition, and nothing listed is gone"
      (list (filter (lambda (s) (not (member (list (car (car s)) (cadr (car s)) (caddr (car s)) (cdr s))
                                             (map (lambda (c) (list-head c 4)) census))))
                    scanned)
            (filter (lambda (c) (not (member (cons (list-head c 3) (cadddr c)) scanned))) census))
      '(() ()))

;; =============================================================================
(printf "== B: the library, in process ==\n")
;; A counting release, installed for the extent of a thunk.
;; An optional `at-release` thunk is called at each release and its values
;; kept, so a row can ask what was true WHEN the lock was released.
(define at-release-seen '())
(define (counting-release thunk . opts)
  (let ((orig (current-lock-release)) (k 0)
        (raise-after? (and (pair? opts) (car opts)))
        (at-release (and (pair? opts) (pair? (cdr opts)) (cadr opts))))
    (set! at-release-seen '())
    (let ((v (dynamic-wind
               (lambda ()
                 (current-lock-release
                   (lambda (l)
                     (set! k (+ k 1))
                     (when at-release (set! at-release-seen (cons (at-release) at-release-seen)))
                     (orig l)
                     (when raise-after?
                       (raise (make-message-condition "injected release failure"))))))
               thunk
               (lambda () (current-lock-release orig)))))
      (list v k))))
(define (kind-of-raise thunk)
  (guard (e ((incomplete-reduction? e) 'incomplete-reduction)
            ((unreadable-entry? e) 'unreadable-entry)
            ((assertion-violation? e) 'assertion)
            ((message-condition? e) (list 'message (condition-message e)))
            (#t 'other))
    (thunk)
    'returned))

;; D-commit (design review r5, finding 5): OUTSIDE any listener, a load
;; aborted by an entry it could not read is refused by that entry at
;; load-commit!, not by check-terminal!'s assertion (MR-22).
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)))
  (let ((ls (log-open st)))
    (chmod! "000" seg)
    (kind-of-raise (lambda () (load-deliver! ls '() (lambda args 'applied))))
    (let ((committed (kind-of-raise (lambda () (load-commit! ls)))))
      (chmod! "600" seg)
      (want "D-commit a load aborted by an older segment it could not read is refused at load-commit! by the unreadable entry"
            committed
            'unreadable-entry))))

;; D-release (design reviews r5b and r6): the refused load's lock is released
;; exactly once, counted by CALLS (a second release is a silent no-op, so
;; trace lines could not tell).
(let* ((c (fresh-store!)) (st (car c)))
  (want "D-release CONTROL a healthy load that commits calls the release once"
        (counting-release
          (lambda ()
            (let ((ls (log-open st)))
              (load-deliver! ls '() (lambda args 'applied))
              (load-commit! ls)
              'committed)))
        '(committed 1))
  (chmod! "000" (mirror-dir st))
  (let ((r (counting-release (lambda () (kind-of-raise (lambda () (log-open st)))))))
    (chmod! "700" (mirror-dir st))
    (want "D-release an undeclared load of a store missing a writer is refused, and its lock released by exactly one call"
          r '(incomplete-reduction 1))))

;; D-release-failure (design review r6, finding 1): a release that fails
;; neither skips the notification nor replaces the refusal (MR-28).
(let* ((c (fresh-store!)) (st (car c)) (key (string-copy st)) (told #f))
  (load-listener-add! key (lambda (event) (when (load-refused? event) (set! told (load-refused-condition event)))))
  (chmod! "000" (mirror-dir st))
  (let ((r (counting-release (lambda () (kind-of-raise (lambda () (log-open key)))) #t
                             (lambda () (and told #t)))))
    (chmod! "700" (mirror-dir st))
    (load-listener-remove! key)
    ;; THE ORDER IS READ AT THE RELEASE (review r1): the listener had
    ;; already been told when the lock was released.
    (want "D-release-failure a refusal whose release fails leaves as the refusal, the listener already told at the release, one release call"
          (list (car r) (and told (incomplete-reduction? told)) at-release-seen (cadr r))
          '(incomplete-reduction #t (#t) 1))))

;; D-meta and D-open (design reviews r5b, finding 2, and r5, finding 1): an
;; entry that cannot be read while the load is being OPENED is told to the
;; listener as a refusal naming it (MR-27, MR-24).
(define (refusal-told-for st path mode)
  (let ((key (string-copy st)) (told #f))
    (load-listener-add! key (lambda (event) (when (load-refused? event) (set! told (load-refused-condition event)))))
    (chmod! "000" path)
    (let ((k (kind-of-raise (lambda () (log-open key)))))
      (chmod! mode path)
      (load-listener-remove! key)
      (list k (and told (unreadable-entry? told) (unreadable-entry-path told))))))
(let* ((c (fresh-store!)) (st (car c)) (meta (string-append st "/meta.sexp")))
  (want "D-meta a meta.sexp that cannot be read is told to the listener as the refusal naming it"
        (refusal-told-for st meta "644")
        (list 'unreadable-entry meta)))
(let* ((c (fresh-store!)) (st (car c)) (w (string-append st "/writers")))
  (want "D-open a writers/ that cannot be listed is told to the listener as the refusal naming it"
        (refusal-told-for st w "700")
        (list 'unreadable-entry w)))

;; Overlay notes (design review r2, finding 7; MR-13): a DECLARED working
;; view of a store missing a writer is a new value built from the base's
;; rows; judged as a supplied state by an undeclared consumer, it is refused
;; with the base's notes.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (key (string-copy st)))
  (load-declaration-set! key incomplete-accepted)
  (chmod! "000" (mirror-dir st))
  (let* ((view (caught (working-state key #f w)))
         (overlay (and (pair? view) (eq? (car view) 'ok) (caddr view)))
         (judged (guard (e ((incomplete-reduction? e) (list 'refused (length (incomplete-reduction-notes e)))))
                   (obtain-state st overlay #f)
                   'handed-over)))
    (chmod! "700" (mirror-dir st))
    (load-declaration-set! key #f)
    (want "Overlay a declared working view of a store missing a writer, judged undeclared, is refused with the base's one note"
          (list (and overlay #t) judged)
          '(#t (refused 1)))))

;; No-reduction verbs with a supplied incomplete state (design review r2,
;; finding 5; MR-17): holding a state is not consuming it. Each verb is asked
;; with a declared incomplete state on a store that is readable again, and
;; answers as the same verb on a healthy store, with no clause.
(define (no-reduction-invocations st w B x)
  (list
    (list 'describe (lambda (state) (rpc-dispatch st '(describe) "test" state)))
    (list 'discard (lambda (state) (rpc-dispatch st (list 'discard B "--writer" w) "test" state)))
    (list 'adopt (lambda (state) (rpc-dispatch st '(adopt) "test" state)))
    (list 'publish (lambda (state)
                     (let ((file (string-append x "/mirror.bin"))
                           (r (encode-record 1 1789000000001 "peer" '() '(set "t" z "z"))))
                       (call-with-port (open-file-output-port file (file-options no-fail))
                         (lambda (p) (put-bytevector p r)))
                       (rpc-dispatch st (list 'publish "mirrorzz" "1" file) "test" state))))
    (list 'split-suggest (lambda (state)
                           (let ((in (string-append x "/split-in.sc")))
                             (call-with-port (open-file-output-port in (file-options no-fail))
                               (lambda (p) (put-bytevector p (string->utf8 "(define (f) 1)\n\n(define (g) 2)\n"))))
                             (rpc-dispatch st (list 'split-suggest in "--output" (string-append x "/split-out.txt")) "test" state))))))
(let ((offenders
        (let loop ((names '(describe discard adopt publish split-suggest)) (out '()))
          (if (null? names)
              (reverse out)
              (let* ((h (fresh-store!))
                     (healthy (caught ((cadr (assq (car names) (apply no-reduction-invocations h))) #f)))
                     (c (fresh-store!)) (st (car c)) (key (string-copy st)))
                (load-declaration-set! key incomplete-accepted)
                (chmod! "000" (mirror-dir st))
                (let ((state (caught (open-and-reduce key))))
                  (chmod! "700" (mirror-dir st))
                  (load-declaration-set! key #f)
                  (let ((a (caught ((cadr (assq (car names) (apply no-reduction-invocations c))) state))))
                    (loop (cdr names)
                          (if (and (equal? (head-of a) (head-of healthy)) (not (clause-of a 'incomplete)))
                              out
                              (cons (list (car names) (head-of a) (head-of healthy)) out))))))))))
  (want "No-reduction a verb that builds no reduction, handed an incomplete state, answers as on a healthy store with no clause"
        offenders '()))

;; D-nested (A2's condition 2; MR-30): a nested verb of another class leaves
;; the outer declaration in place on a normal return AND on an escape. The
;; outer request is this row: a listener and a declaration on its own store
;; object make the dispatch below a nested one, sharing that object.
(let* ((c (fresh-store!)) (st (car c)) (x (cadddr c)) (key (string-copy st)))
  (load-declaration-set! key incomplete-accepted)
  (chmod! "000" (mirror-dir st))
  (let* ((escaping #f)
         (_ (load-listener-add! key (lambda (event) (when (and escaping (load-refused? event)) (escaping 'escaped)))))
         (normal (begin (rpc-dispatch key (list 'export-code (string-append x "/n1")) "test")
                        (eq? (load-declaration-of key) incomplete-accepted)))
         (escaped (call/cc (lambda (k)
                             (set! escaping k)
                             (rpc-dispatch key (list 'export-code (string-append x "/n2")) "test")
                             'not-escaped)))
         (after-escape (eq? (load-declaration-of key) incomplete-accepted)))
    (set! escaping #f)
    (load-listener-remove! key)
    (load-declaration-set! key #f)
    (chmod! "700" (mirror-dir st))
    (want "D-nested an undeclared nested verb restores the outer declaration on return and on an escape"
          (list normal escaped after-escape)
          '(#t escaped #t))))

;; =============================================================================
(printf "== C: a program held between discovery and delivery ==\n")
;; -> (held? rc datum stderr-text stdout-text): the program run in the background with a
;; hold at `stage`, `change!` applied once the hold is reached, then released.
(define c-n 0)
(define (held-run stage env program args change!)
  (set! c-n (+ c-n 1))
  (let* ((tag (string-append root "/c" (number->string c-n)))
         (rel (string-append tag ".release"))
         (out (string-append tag ".out"))
         (err (string-append tag ".err"))
         (rcf (string-append tag ".rc")))
    (sh "( " env " THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_HOLD=" stage ":" rel
        " perl -e 'alarm 60; exec @ARGV' scheme --script " program " "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "> " out " 2> " err " < /dev/null; echo $? > " rcf " ) &")
    (let ((held (wait-until-file (string-append rel ".held") 20000)))
      (change!)
      (touch! rel)
      (wait-until-text rcf 30000)
      (list held
            (string->number (let ((t (text-of-file rcf))) (substring t 0 (max 0 (- (string-length t) 1)))))
            (first-datum-of out)
            (text-of-file err)
            (text-of-file out)))))
(define (unreadable-answer path)
  (list 'error 'unreadable (list 'path path) '(reason "Permission denied") '(errno EACCES)))
(define local "THEOURGIA_LOCAL=1")

;; D-barrier: held after discovery, an older segment made unreadable: the
;; delivery barrier meets it, and the answer names it, not internal.
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)))
  (let ((r (held-run "after-discovery" local "../core.sc" (list "outline" "--store" st)
                     (lambda () (chmod! "000" seg)))))
    (chmod! "600" seg)
    (want "D-barrier an older segment made unreadable after discovery: the answer names it (rc 1), the hold was reached"
          (list (car r) (cadr r) (caddr r))
          (list #t 1 (unreadable-answer seg)))))
;; D-deliver: held after the barrier: delivery itself meets it (MR-19).
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)))
  (let ((r (held-run "after-barrier" local "../core.sc" (list "outline" "--store" st)
                     (lambda () (chmod! "000" seg)))))
    (chmod! "600" seg)
    (want "D-deliver an older segment made unreadable after the barrier: the answer names it (rc 1), not internal"
          (list (car r) (cadr r) (caddr r))
          (list #t 1 (unreadable-answer seg)))))
;; D-healthy: the same holds with nothing changed answer as without them.
(let* ((c (fresh-store!)) (st (car c))
       (plain-rc #f)
       (plain (let ((out (string-append root "/plain.out")))
                (set! plain-rc (sh local " scheme --script ../core.sc outline --store " (quoted st) " > " out " 2> /dev/null < /dev/null"))
                (text-of-file out)))
       (a (held-run "after-discovery" local "../core.sc" (list "outline" "--store" st) (lambda () #f)))
       (b (held-run "after-barrier" local "../core.sc" (list "outline" "--store" st) (lambda () #f))))
  (want "D-healthy CONTROL each hold with nothing changed answers as the unheld run, and was reached"
        ;; EACH RUN ANSWERED (review r1): rc 0 three times, and the plain
        ;; answer is a real one -- the human rendering of an ok outline
        ;; begins with the symbol `-` -- not NO-ANSWER or an error.
        ;; THE WHOLE OUTPUT (code review r2): each held run printed exactly
        ;; what the plain run printed, and that is the fixture's outline.
        (list (car a) (equal? (list-ref a 4) plain) (car b) (equal? (list-ref b 4) plain)
              (list plain-rc (cadr a) (cadr b)) (has-substring? plain "Beta"))
        '(#t #t #t #t (0 0 0) #t)))
;; N, readable damage (ruling Q-r5-1): a CRC-damaged older segment after the
;; barrier keeps the base's answer, internal. F125 will name it.
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)) (saved #f))
  (let ((r (held-run "after-barrier" local "../core.sc" (list "outline" "--store" st)
                     (lambda () (set! saved (damage! seg))))))
    (undamage! seg saved)
    (want "N readable damage after the barrier keeps the base's answer: internal (F125 will name it)"
          (list (car r) (cadr r) (head-of (caddr r)))
          '(#t 1 (error internal)))))
;; D-snapshot (design review r5, finding 3; MR-25): a snapshot whose session
;; delivery aborts installs nothing, and its answer names the entry.
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)) (snap (string-append st "/snap")))
  (let* ((names-before (if (file-directory? snap) (list-sort string<? (directory-list snap)) '()))
         (r (held-run "after-barrier" local "../core.sc" (list "snapshot" "--store" st)
                      (lambda () (chmod! "000" seg))))
         (names-after (if (file-directory? snap) (list-sort string<? (directory-list snap)) '()))
         (installs (length (filter (lambda (l) (and (has-substring? l "/snap/")
                                                    (exists (lambda (op) (prefix? (string-append "(trace " op " ") l))
                                                            '("create" "write" "rename" "link"))))
                                   (lines-of-text (cadddr r))))))
    (chmod! "600" seg)
    (want "D-snapshot a snapshot whose delivery meets an unreadable older segment installs nothing and names it"
          (list (car r) (caddr r) installs (equal? names-before names-after))
          (list #t (unreadable-answer seg) 0 #t))))
;; D-snapshot, readable (plan amendment A3; MR-25): a damaged older segment
;; lets the session go on (F125's), so its snapshot reaches session-snapshot!,
;; whose gate refuses the installation: (error not-delivered), nothing
;; installed.
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)) (snap (string-append st "/snap")) (saved #f))
  (let* ((names-before (if (file-directory? snap) (list-sort string<? (directory-list snap)) '()))
         (r (held-run "after-barrier" local "../core.sc" (list "snapshot" "--store" st)
                      (lambda () (set! saved (damage! seg)))))
         (names-after (if (file-directory? snap) (list-sort string<? (directory-list snap)) '()))
         (installs (length (filter (lambda (l) (and (has-substring? l "/snap/")
                                                    (exists (lambda (op) (prefix? (string-append "(trace " op " ") l))
                                                            '("create" "write" "rename" "link"))))
                                   (lines-of-text (cadddr r))))))
    (undamage! seg saved)
    (want "D-snapshot-readable a snapshot whose delivery meets a damaged older segment is refused not-delivered and installs nothing"
          (list (car r) (head-of (caddr r)) installs (equal? names-before names-after))
          (list #t '(error not-delivered) 0 #t))))
;; Eval, the parent side, N (code review r1): eval --working whose baseline
;; load meets an unreadable older segment answers AS THE BASE -- working.sc's
;; `problem` names the entry (working-unavailable with its path and reason)
;; and eval adds (during cut). eval declares, so its own scope names nothing.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (seg (older-segment st)))
  (let ((r (held-run "after-discovery" "" "../core.sc" (list "eval" "--store" st "--working" "--writer" w "1")
                     (lambda () (chmod! "000" seg)))))
    (chmod! "600" seg)
    (want "Eval parent side N: an unreadable older segment in the baseline load answers as the base, working-unavailable naming it, (during cut), rc 1"
          (list (car r) (cadr r) (caddr r))
          (list #t 1 (list 'error 'working-unavailable (list 'path seg) '(reason "Permission denied") '(during cut))))))
;; Eval, the parent side, declared (plan section 9; MR-18; code review r1's S):
;; eval --working on a store with the mirror unreadable is DECLARED -- the
;; parent's baseline and view loads are not refused -- and the answer carries
;; ONE incomplete clause naming the mirror: the worker's notes and the
;; parent's merged, never two clauses.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (out (string-append root "/eval-declared.out")))
  (chmod! "000" (mirror-dir st))
  (let* ((rc (sh "THEOURGIA_LOCAL=1 scheme --script ../core.sc eval --store " (quoted st)
                 " --working --writer " (quoted w) " 1 --wire > " out " 2> /dev/null < /dev/null"))
         (a (first-datum-of out)))
    (chmod! "700" (mirror-dir st))
    (want "Eval parent side declared: eval --working with the mirror unreadable answers ok with one clause naming the mirror, rc 0"
          (list rc (head-of a) (incomplete-paths a)
                (and (pair? a) (list? a)
                     (length (filter (lambda (x) (and (pair? x) (eq? (car x) 'incomplete))) (cdr a)))))
          (list 0 'ok (list (mirror-dir st)) 1))))
;; Eval N, round 1's EXACT case (code review r2): every writers/ entry
;; readable, the store's meta.sexp not. The answer is the base's: working.sc's
;; `problem` names the entry and eval adds (during cut).
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (meta (string-append st "/meta.sexp"))
       (out (string-append root "/eval-meta.out")))
  (chmod! "000" meta)
  (let* ((rc (sh "THEOURGIA_LOCAL=1 scheme --script ../core.sc eval --store " (quoted st)
                 " --working --writer " (quoted w) " 1 --wire > " out " 2> /dev/null < /dev/null"))
         (a (first-datum-of out)))
    (chmod! "644" meta)
    (want "Eval parent side N meta: an unreadable meta.sexp, writers/ readable, answers as the base, working-unavailable naming it, (during cut), rc 1"
          (list rc a)
          (list 1 (list 'error 'working-unavailable (list 'path meta) '(reason "Permission denied") '(during cut))))))
;; Eval, the PARENT's notes alone (code review r2; MR-33): the mirror is
;; unreadable for the parent's first load (the cut's baseline) and readable
;; again, during a hold after that load's discovery, for everything after --
;; the view and the worker hear nothing. The clause can only come from what
;; the parent heard, merged into the answer.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)))
  (chmod! "000" (mirror-dir st))
  (let ((r (held-run "after-discovery" "" "../core.sc"
                     (list "eval" "--store" st "--working" "--writer" w "1" "--wire")
                     (lambda () (chmod! "700" (mirror-dir st))))))
    (chmod! "700" (mirror-dir st))
    (want "Eval parent notes: a mirror unreadable only for the parent's baseline load is named by the eval's one clause"
          (list (car r) (cadr r) (head-of (caddr r)) (incomplete-paths (caddr r)) (incomplete-clause-count (caddr r)))
          (list #t 0 'ok (list (mirror-dir st)) 1))))
;; Eval, the FAILURE exit carries the parent's notes (code review r2's S;
;; MR-34). The mirror M is unreadable throughout, so the parent's first load
;; (the cut's baseline; eval declares) succeeds and hears it. The view's load
;; must then ABORT: an entry unreadable at discovery or at the barrier is
;; only one more note to a declared load, so a second mirror's older segment
;; is made unreadable after the view's barrier, where delivery meets it. The
;; hold seam stops at the first arrival of a stage and re-arms while its
;; release file is absent, so two stages are alternated: each release file
;; is removed while the process waits at the other stage, and the second
;; arrival at after-barrier is the view's. The raise leaves the view for the
;; scheduler's guard -- the exit this row is about. The answer names the
;; segment AND carries the clause naming M.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (M2 "mirror02")
       (r1 (encode-record 1 1757300000011 "peer2" '() '(set "u" x "x")))
       (r2 (encode-record 2 1757300000012 "peer2" '() '(set "u" y "y")))
       (_ (log-publish! st M2 1 r1 (segment-sha r1)))
       (_ (log-publish! st M2 2 r2 (segment-sha r2)))
       (seg2 (string-append (writer-directory st M2) "/" (segment-file-name 1)))
       (tag (string-append root "/eval-exit"))
       (ra (string-append tag ".a")) (rb (string-append tag ".b"))
       (out (string-append tag ".out")) (rcf (string-append tag ".rc"))
       (arrived (lambda (rel) (wait-until-file (string-append rel ".held") 20000)))
       (re-arm! (lambda (rel) (sh "rm -f '" rel "' '" rel ".held'"))))
  (chmod! "000" (mirror-dir st))
  (sh "( THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_HOLD='after-discovery:" ra ";after-barrier:" rb "'"
      " perl -e 'alarm 60; exec @ARGV' scheme --script ../core.sc eval --store " (quoted st)
      " --working --writer " (quoted w) " 1 --wire > " out " 2> /dev/null < /dev/null; echo $? > " rcf " ) &")
  (let* ((baseline-discovered (arrived ra))
         (_ (touch! ra))
         (baseline-barrier (arrived rb))
         (_ (re-arm! ra))
         (_ (touch! rb))
         (view-discovered (arrived ra))
         (_ (re-arm! rb))
         (_ (touch! ra))
         (view-barrier (arrived rb))
         (_ (chmod! "000" seg2))
         (_ (touch! rb))
         (_ (wait-until-text rcf 30000))
         (rc (string->number (let ((t (text-of-file rcf))) (substring t 0 (max 0 (- (string-length t) 1))))))
         (a (first-datum-of out)))
    (chmod! "600" seg2)
    (chmod! "700" (mirror-dir st))
    (want "Eval failure exit: a view whose load is refused by an unreadable segment answers that AND the clause naming the mirror the baseline heard"
          (list (list baseline-discovered baseline-barrier view-discovered view-barrier)
                rc (head-of a) (clause-of a 'path) (incomplete-paths a))
          (list '(#t #t #t #t) 1 '(error unreadable) (list 'path seg2) (list (mirror-dir st))))))
;; Eval, the EARLY exits (code review r3's S; MR-35, MR-36). An oversized
;; source is answered after the cut's baseline load, which heard M, so it
;; carries the parent's notes. Bad limits are answered BEFORE any load
;; since the evaluation admission put the pre-admission refusals first, so
;; with M unreadable it carries no clause. Each is run twice: with M
;; unreadable and on the healthy store (the base answer exactly, no clause).
(define (eval-early st w args in)
  (let ((out (string-append root "/eval-early.out")))
    (let ((rc (sh "THEOURGIA_LOCAL=1 scheme --script ../core.sc eval --store " (quoted st)
                  " --working --writer " (quoted w) " "
                  (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                  "--wire > " out " 2> /dev/null < " (if in (quoted in) "/dev/null"))))
      (list rc (first-datum-of out)))))
;; THE BASE ANSWER'S USAGE FORM, read out of rpc.sc as data (code review
;; r4): the healthy bad-limits answer is compared whole, and its usage clause
;; is whatever `eval-usage` says -- a form F77c does not change. It is found
;; at the top level or in the body of a `(library ...)` form: rpc.sc is one
;; library form, and eval's usage form moved there from core.sc when the
;; catalogue began to publish it.
(define (defined-datum file name)
  (define (found x)
    (and (pair? x) (eq? (car x) 'define) (pair? (cdr x)) (eq? (cadr x) name) (pair? (cddr x))
         (let ((v (caddr x)))
           (if (and (pair? v) (eq? (car v) 'quote)) (cadr v) v))))
  (call-with-input-file file
    (lambda (p)
      (let loop ()
        (let ((x (read p)))
          (cond ((eof-object? x) #f)
                ((found x) => (lambda (v) v))
                ((and (pair? x) (eq? (car x) 'library) (list? x))
                 (or (exists found (cdr x)) (loop)))
                (else (loop))))))))
(define eval-usage-form (defined-datum "../rpc.sc" 'eval-usage))
(define (without-incomplete a)
  (if (and (pair? a) (list? a))
      (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'incomplete)))) a)
      a))
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c))
       (healthy (eval-early st w '("--timeout-ms" "0" "1") #f))
       (_ (chmod! "000" (mirror-dir st)))
       (missing (eval-early st w '("--timeout-ms" "0" "1") #f)))
  (chmod! "700" (mirror-dir st))
  (want "Eval bad limits: answered before any load, so with M unreadable the refusal carries no clause and is the healthy answer exactly"
        (list (car missing) (head-of (cadr missing)) (incomplete-paths (cadr missing))
              (incomplete-clause-count (cadr missing))
              (equal? (without-incomplete (cadr missing)) (cadr healthy))
              (and (pair? eval-usage-form) (eq? (car eval-usage-form) 'eval))
              (car healthy) (cadr healthy))
        (list 1 '(error bad-request) '() 0 #t
              #t
              1 (list 'error 'bad-request '(reason eval-arguments) (list 'usage eval-usage-form)))))
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c))
       (big (string-append root "/big-source.ss"))
       (_ (call-with-output-file big (lambda (o) (put-string o (make-string 1048577 #\space))) 'truncate))
       (healthy (eval-early st w '() big))
       (_ (chmod! "000" (mirror-dir st)))
       (missing (eval-early st w '() big)))
  (chmod! "700" (mirror-dir st))
  (want "Eval oversized source: answered after the baseline, with M unreadable the refusal carries one clause naming M; healthy it is the base answer"
        (list (car missing) (head-of (cadr missing)) (incomplete-paths (cadr missing))
              (incomplete-clause-count (cadr missing))
              (equal? (without-incomplete (cadr missing)) (cadr healthy))
              (car healthy) (cadr healthy))
        (list 1 '(error bad-source) (list (mirror-dir st)) 1 #t
              1 '(error bad-source (reason input-limit)))))
;; An unreadable abort RENAMED by a catch-all is named by the side channel
;; (MR-20): import-code of files carrying a projection header loads its
;; baseline inside code-project's catch-all, which calls any failure
;; unavailable-cut; held after discovery, an older segment made unreadable
;; aborts that load, finish-abort! tells the listener, and the answer names
;; the entry.
(let* ((c (fresh-store!)) (st (car c)) (x (cadddr c)) (seg (older-segment st)))
  (sh "mkdir -p " x "/code-in")
  (call-with-output-file (string-append x "/code-in/m.py") (lambda (p) (put-string p "def marmoset():\n  pass\n")))
  (ask st 'import-code (string-append x "/code-in"))
  (let* ((exported (ask st 'export-code (string-append x "/code-out")))
         ;; THE SETUP IS ASSERTED (code review r2): the export wrote a file
         ;; carrying the projection header -- the `@file <hex>` marker line
         ;; code-markers.sc writes, the store id hex-encoded inside it --
         ;; which is what sends the import to its baseline load inside
         ;; code-project's catch-all.
         (header-ok (has-substring? (text-of-file (string-append x "/code-out/m.py")) "@file "))
         (r (held-run "after-discovery" local "../core.sc" (list "import-code" (string-append x "/code-out") "--store" st)
                      (lambda () (chmod! "000" seg)))))
    (chmod! "600" seg)
    (want "D-import-baseline an abort inside code-project's catch-all is named unreadable through the listener, not unavailable-cut (rc 1)"
          (list (head-of exported) header-ok (car r) (cadr r) (caddr r))
          (list 'ok #t #t 1 (unreadable-answer seg)))))

;; =============================================================================
(printf "== D: the undeclared verbs refuse and write nothing ==\n")
;; A store's log, as bytes per file, to show nothing was written.
(define (log-bytes st)
  (let ((ls (lines-of-text (let* ((p (process (string-append "cd " (quoted st) " && find writers -type f | sort | xargs cat | cksum")))
                                  (t (get-string-all (car p))))
                             (close-port (car p)) (close-port (cadr p))
                             (if (eof-object? t) "" t)))))
    (and (pair? ls) (car ls))))
(define (refused-writing-nothing label st thunk)
  (chmod! "000" (mirror-dir st))
  (let* ((before (begin (chmod! "700" (mirror-dir st)) (let ((b (log-bytes st))) (chmod! "000" (mirror-dir st)) b)))
         (a (caught (thunk))))
    (chmod! "700" (mirror-dir st))
    (want label
          (list (head-of a) (equal? before (log-bytes st)))
          '((error incomplete-reduction) #t))))
(let* ((c (fresh-store!)) (st (car c)) (x (cadddr c)))
  (sh "mkdir -p " x "/md-in")
  (call-with-output-file (string-append x "/md-in/page.md") (lambda (p) (put-string p "# Page\n\ntext\n")))
  (refused-writing-nothing "D import-md on a store missing a writer refuses by name and writes nothing (Q3, an L)"
                           st (lambda () (ask st 'import-md (string-append x "/md-in")))))
(let* ((c (fresh-store!)) (st (car c)) (x (cadddr c)))
  (sh "mkdir -p " x "/code-in")
  (call-with-output-file (string-append x "/code-in/m.py") (lambda (p) (put-string p "def marmoset():\n  pass\n")))
  (refused-writing-nothing "D import-code on a store missing a writer refuses by name and writes nothing (Q3, an L)"
                           st (lambda () (ask st 'import-code (string-append x "/code-in")))))
;; import-code of files carrying a projection header: the baseline load sits
;; inside code-project's catch-all, which would call it unavailable-cut.
(let* ((c (fresh-store!)) (st (car c)) (x (cadddr c)))
  (sh "mkdir -p " x "/code-in")
  (call-with-output-file (string-append x "/code-in/m.py") (lambda (p) (put-string p "def marmoset():\n  pass\n")))
  (ask st 'import-code (string-append x "/code-in"))
  (ask st 'export-code (string-append x "/code-out"))
  (refused-writing-nothing "D import-code of exported files (header baseline, code-project's catch-all) refuses by name and writes nothing"
                           st (lambda () (ask st 'import-code (string-append x "/code-out")))))
(let* ((c (fresh-store!)) (st (car c)))
  (refused-writing-nothing "D def on a store missing a writer refuses by name and writes nothing (Q3, an L)"
                           st (lambda () (ask st 'def "ackg" "(define (ackg) 2)"))))
(define (files-under dir)
  (if (file-directory? dir) (length (directory-list dir)) 0))
;; --working exports (ruling 5): no route declares for an export.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (x (cadddr c)) (out (string-append x "/cw")))
  (chmod! "000" (mirror-dir st))
  (let ((a (caught (ask st 'export-code out "--working" "--writer" w))))
    (chmod! "700" (mirror-dir st))
    (want "D export-code --working on a store missing a writer refuses by name and writes no file"
          (list (head-of a) (files-under out)) '((error incomplete-reduction) 0))))
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (x (cadddr c)) (out (string-append x "/mw")))
  (sh "mkdir -p " out)
  (chmod! "000" (mirror-dir st))
  (let ((a (caught (ask st 'export-md out "--working" "--writer" w))))
    (chmod! "700" (mirror-dir st))
    (want "D export-md --working on a store missing a writer refuses by name and writes no file"
          (list (head-of a) (files-under out)) '((error incomplete-reduction) 0))))

;; =============================================================================
(printf "== E: a daemon ==\n")
;; Daemons are found by their exact argv line and stopped at the end of each
;; row, and on a raise.
(define run-root (string-append sock-base "/f77r-" (number->string (get-process-id))))
(sh "mkdir -p " run-root)
(putenv "THEOURGIA_RUN" (string-append run-root "/run"))
(define (lines-of-command cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (lines-of-text (if (eof-object? t) "" t))))
(define (daemon-pids store)
  (let ((needle (string-append "theourgiad.sc serve " store)))
    (map (lambda (l) (string->number (car (filter (lambda (w) (> (string-length w) 0))
                                                   (let split ((cs (string->list l)) (cur '()) (acc '()))
                                                     (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
                                                           ((char=? (car cs) #\space) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                           (else (split (cdr cs) (cons (car cs) cur) acc))))))))
         (filter (lambda (l) (has-substring? l needle)) (lines-of-command "ps -axo pid=,command=")))))
(define e-stores '())
(define (stop-daemon! store)
  (for-each (lambda (pid) (sh "kill -TERM " (number->string pid) " 2>/dev/null")) (daemon-pids store))
  (let loop ((k 0))
    (when (and (< k 50) (pair? (daemon-pids store))) (sh "sleep 0.1") (loop (+ k 1))))
  (for-each (lambda (pid) (sh "kill -KILL " (number->string pid) " 2>/dev/null")) (daemon-pids store)))
(let ((previous (base-exception-handler)))
  (base-exception-handler
    (lambda (e) (for-each stop-daemon! e-stores) (previous e))))
;; A thin-client command; -> (rc datum).
(define e-n 0)
;; ALWAYS --wire: an ok answer in the human rendering is text, and the rows
;; read the answer as a datum.
(define (client env . args)
  (set! e-n (+ e-n 1))
  (let ((out (string-append root "/e" (number->string e-n) ".out")))
    (let ((rc (sh env " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc "
                  (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                  "--wire > " out " 2> " out ".err < /dev/null")))
      (list rc (first-datum-of out)))))
;; A background command; its standard input is the file `in`, or /dev/null.
(define (client-bg-in! env out in . args)
  (sh "( " env " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc "
      (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
      "--wire > " out " 2> " out ".err < " (if in (quoted in) "/dev/null") " ) &"))
(define (client-bg! env out . args) (apply client-bg-in! env out #f args))
(define (daemon-store!)
  (let ((c (fresh-store!)))
    (set! e-stores (cons (car c) e-stores))
    c))
(define (serve-log st) (text-of-file (serve-log-path st)))
(define (trace-lines-with st op)
  (filter (lambda (l) (prefix? (string-append "(trace " op) l)) (lines-of-text (serve-log st))))
;; An outline naming block title "Outside" or not: whether the answer was
;; built from a state that has the outside commit.
(define (mentions-outside? a) (has-substring? (format "~s" a) "Outside"))
(define (outside-commit! st) (ask st 'insert "--under" "root" "--title" "Outside"))
;; Whether an outline answer holds the mirror's own record.
(define (mentions-mirror-record? a) (has-substring? (format "~s" a) "unplaced"))
;; WHERE THE ANSWER'S NOTES CAME FROM (code review r3, the closing round of
;; that class): a request that ignored its supplied state and loaded afresh
;; would open the log, and every fresh load does so through log-open
;; (store.sc replay), which traces `log-open`. So the count of those lines is
;; read before and after the answering request, and must not move.
(define (log-opens st) (length (trace-lines-with st "log-open ")))

;; U4b-trigger (ruling Q-r2-1 (b); MR-14): after an outside commit and a
;; mirror directory made unreadable, ONE outline at the connection answers
;; from the previous publication (it does not have the outside commit) and
;; says, from the probe's markers, that the mirror cannot be read.
(define (u4b-trigger! label env chmod-target clause-path verb-args)
  (let* ((c (daemon-store!)) (st (car c)))
    (client env "outline" "--store" st)
    (outside-commit! st)
    (chmod! "000" (chmod-target st))
    (let* ((opens-before (log-opens st))
           (r (apply client env (append verb-args (list "--store" st))))
           (opens-after (log-opens st)))
      (chmod! (if (string=? (chmod-target st) (mirror-dir st)) "700" "600") (chmod-target st))
      (stop-daemon! st)
      ;; THE PREVIOUS PUBLICATION ANSWERED, AND ONLY THE PROBE COULD NAME THE
      ;; MIRROR (code review r2): reload-raise keeps every reload from
      ;; publishing -- the daemon published once, at its start -- and outline
      ;; answers from the supplied state without a load of its own, so a
      ;; clause here comes from the probe's markers and nowhere else. The
      ;; request opened no log (code review r3), and the answer holds the
      ;; mirror's own record, which the previous publication has.
      (want label
            (list (car r) (mentions-outside? (cadr r)) (incomplete-paths (cadr r))
                  (length (trace-lines-with st "published "))
                  (- opens-after opens-before) (mentions-mirror-record? (cadr r)))
            (list 0 #f (list (clause-path st)) 1 0 #t)))))
;; The trigger rows arm reload-raise (see above) and trace.
(define no-reload "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_FAULT=reload-raise@conn")
(u4b-trigger! "U4b-trigger the read that triggers the probe answers the previous publication with the clause naming the mirror"
              no-reload mirror-dir mirror-dir '("outline"))
;; U4b-trigger-file (MR-15): only the mirror's CURRENT segment unreadable: a
;; marker nested in the writer's entry.
(u4b-trigger! "U4b-trigger-file a current segment made unreadable is named by the read that triggers the probe"
              no-reload current-segment current-segment '("outline"))
;; U4b-trigger-writer (design review r2, finding 3; MR-16): a writer-local
;; verb is answered from the publication too, and probes first.
(let* ((c (daemon-store!)) (st (car c)) (w (cadr c)))
  (client no-reload "outline" "--store" st)
  (outside-commit! st)
  (chmod! "000" (mirror-dir st))
  (let* ((opens-before (log-opens st))
         (r (client no-reload "drafts" "--writer" w "--store" st))
         (opens-after (log-opens st)))
    (chmod! "700" (mirror-dir st))
    (stop-daemon! st)
    ;; As the trigger rows: no reload publishes (reload-raise), and drafts
    ;; answers from the supplied state -- it opened no log (code review r3)
    ;; -- so the clause is the probe's.
    (want "U4b-trigger-writer a writer-local verb after the outside change carries the clause naming the mirror, from the probe alone"
          (list (car r) (incomplete-paths (cadr r)) (length (trace-lines-with st "published "))
                (- opens-after opens-before))
          (list 0 (list (mirror-dir st)) 1 0))))
;; U4b-older-segment and U4b-after: an OLDER segment is not fingerprinted, so
;; the read that triggers the probe (the outside commit changed the local
;; writer) is one behind and says nothing; once the reload has published,
;; the state's own notes name it. WHICH STATE ANSWERED is read from the
;; mirror's record ("t ... unplaced" in the outline): the previous
;; publication has it, the reloaded one does not (the mirror stops before
;; its unreadable older segment). The outside insert is no witness here: in
;; the F77c r1d diagnostic it was absent after the reload, and the reason read
;; in the code is that it depends on the mirror's records and waits once they
;; are unavailable (F77a available-through).
(let* ((c (daemon-store!)) (st (car c)) (seg (older-segment st)) (traced "THEOURGIA_INJECT=on THEOURGIA_TRACE=1"))
  (client traced "outline" "--store" st)
  (outside-commit! st)
  (chmod! "000" seg)
  (let* ((first (client traced "outline" "--store" st))
         ;; THE LATER READ WAITS FOR THE RELOAD'S PUBLICATION (code review r2),
         ;; so what it reads is the reloaded state -- outline loads nothing of
         ;; its own.
         (published (let loop ((k 0))
                      (cond ((>= (length (trace-lines-with st "published ")) 2) #t)
                            ((>= k 50) #f)
                            (else (sh "sleep 0.1") (loop (+ k 1))))))
         ;; AND IT OPENS NO LOG ITSELF (code review r3): the notes it carries
         ;; are the reloaded state's.
         (opens-before (log-opens st))
         (later (client traced "outline" "--store" st))
         (opens-after (log-opens st)))
    (chmod! "600" seg)
    (stop-daemon! st)
    (want "U4b-older-segment the triggering read is one behind with no clause; after the reload the state's own notes name the segment"
          (list (mentions-mirror-record? (cadr first)) (incomplete-paths (cadr first))
                published (mentions-mirror-record? (cadr later)) (incomplete-paths (cadr later))
                (- opens-after opens-before))
          (list #t 'no-clause #t #f (list seg) 0))))
;; The refresh guards (ruling 4; MR-6, MR-7): a failed fold keeps the
;; previous publication and says reload-failed; a failed probe says
;; probe-failed. The injected branch is shown to have run.
(let* ((c (daemon-store!)) (st (car c)) (env "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_FAULT=reload-raise@conn"))
  (client env "outline" "--store" st)
  (outside-commit! st)
  (client env "outline" "--store" st)
  (sh "sleep 1")
  (let ((r (client env "outline" "--store" st)))
    (stop-daemon! st)
    (want "Guard reload-raise: the failed fold is traced reload-failed and the previous publication is still served"
          ;; THE INJECTED BRANCH RAN (review r1): the traced line carries the
          ;; injected condition's own text.
          ;; THE PREVIOUS PUBLICATION'S CONTENT (code review r3): the fixture's
          ;; block Beta and the mirror's record are still in the answer.
          (list (exists (lambda (l) (has-substring? l "injected reload raise")) (trace-lines-with st "reload-failed"))
                (head-of (cadr r)) (mentions-outside? (cadr r))
                (has-substring? (format "~s" (cadr r)) "Beta") (mentions-mirror-record? (cadr r)))
          '(#t ok #f #t #t))))
(let* ((c (daemon-store!)) (st (car c)) (env "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_FAULT=probe-raise@conn"))
  (client env "outline" "--store" st)
  (outside-commit! st)
  (let ((r (client env "outline" "--store" st)))
    (stop-daemon! st)
    (want "Guard probe-raise: the failed probe is traced probe-failed and the read is answered"
          (list (exists (lambda (l) (has-substring? l "injected probe raise")) (trace-lines-with st "probe-failed"))
                (car r) (head-of (cadr r)))
          '(#t 0 ok))))
;; D-publish and D-publish-readable (design reviews r4 and r5; plan amendment
;; A3; MR-31, MR-21, MR-23): a batch whose session's INITIAL load is held
;; after the delivery barrier while an older segment of the mirror is made
;; unreadable -- or damaged. The daemon's own start-up fold passes the hold
;; first; it is released, and the hold re-armed by removing the release (a
;; hold waits while its release file does not exist).
;; - unreadable: the session refuses before any intent (A3): the answer names
;;   the segment, the log is unchanged, nothing is published.
;; - damaged (readable, F125's): the session goes on and writes, and the
;;   publication is withheld by the gate (the accepted L).
;; Each run its own files: a release, a marker or an answer left by an
;; earlier run would satisfy a wait at once (measured in r1e, where the
;; second run read the first run's answer).
(define pub-n 0)
(define (publish-run change! undo!)
  (set! pub-n (+ pub-n 1))
  (let* ((c (daemon-store!)) (st (car c)) (B (caddr c))
         (tag (string-append root "/pub-" (number->string pub-n)))
         (rel (string-append tag ".release"))
         (out (string-append tag ".out"))
         (env (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_HOLD=after-barrier:" rel)))
    (client-bg! env (string-append tag "-start.out") "outline" "--store" st)
    (let* ((start-held (wait-until-file (string-append rel ".held") 20000))
           (_ (begin (sh "rm -f " (quoted (string-append rel ".held"))) (touch! rel)
                     (wait-until-text (string-append tag "-start.out") 30000)
                     (sh "rm -f " (quoted rel))))
           (log-before (log-bytes st))
           (published-before (length (trace-lines-with st "published"))))
      ;; THE INTENTS ARRIVE ON STANDARD INPUT: batch reads its one argument
      ;; from there when none is given (test/cli1.sc does the same).
      (call-with-output-file (string-append out ".in")
        (lambda (o) (put-string o (string-append "((set \"" B "\" title \"one\") (set \"" B "\" title \"two\"))")))
        'truncate)
      (client-bg-in! env out (string-append out ".in") "batch" "--store" st)
      (let* ((held (wait-until-file (string-append rel ".held") 20000))
             (saved (change! st)))
        (touch! rel)
        (wait-until-text out 30000)
        (sh "sleep 0.5")
        (undo! st saved)
        (let* ((a (first-datum-of out))
               (withheld (length (trace-lines-with st "publish-withheld")))
               (published-after (length (trace-lines-with st "published")))
               (log-after (log-bytes st))
               ;; THE PUBLISH EVENTS IN ORDER: the start-up's `published`,
               ;; then the session's own -- which the gate must have
               ;; withheld -- before any `published` from the reload it asks
               ;; for (review r1). Every later pass through the hold traces
               ;; `hold` again, so the order is read from the publish lines.
               (publishes (filter (lambda (l) (prefix? "(trace publish" l)) (lines-of-text (serve-log st))))
               (first-publish (and (pair? publishes) (pair? (cdr publishes)) (cadr publishes)))
               ;; AFTER THE WITHHOLDING, EVERY PUBLICATION IS A FRESH FOLD'S
               ;; (code review r2): a session writes no log-open line, a
               ;; fold does, so no more publications than log-opens follow.
               (after-withheld (let loop ((ls (lines-of-text (serve-log st))))
                                 (cond ((null? ls) '())
                                       ((prefix? "(trace publish-withheld" (car ls)) (cdr ls))
                                       (else (loop (cdr ls))))))
               ;; IN ORDER (code review r3): each `published` after the
               ;; withholding has a log-open since the publish event before
               ;; it -- counts alone let the session publish ahead of a fold.
               (folds-cover (let loop ((ls after-withheld) (opened #f))
                              (cond ((null? ls) #t)
                                    ((prefix? "(trace log-open " (car ls)) (loop (cdr ls) #t))
                                    ((prefix? "(trace published " (car ls)) (and opened (loop (cdr ls) #f)))
                                    ((prefix? "(trace publish" (car ls)) (loop (cdr ls) #f))
                                    ;; A FAILED FOLD PUBLISHES NOTHING (code review r4): the
                                    ;; log-open before it covers no later publication.
                                    ((prefix? "(trace reload-failed" (car ls)) (loop (cdr ls) #f))
                                    (else (loop (cdr ls) opened))))))
          (stop-daemon! st)
          (list (and start-held held) a withheld (- published-after published-before)
                (equal? log-before log-after) st
                (and first-publish (prefix? "(trace publish-withheld" first-publish) #t)
                folds-cover))))))
(let* ((r (publish-run (lambda (st) (chmod! "000" (older-segment st)) #f)
                       (lambda (st saved) (chmod! "600" (older-segment st)))))
       (st (list-ref r 5)))
  (want "D-publish a session whose initial load meets an unreadable segment refuses by name, writes nothing, publishes nothing (A3)"
        (list (car r) (cadr r) (list-ref r 4) (cadddr r))
        (list #t (unreadable-answer (older-segment st)) #t 0)))
(let* ((r (publish-run (lambda (st) (damage! (older-segment st)))
                       (lambda (st saved) (undamage! (older-segment st) saved)))))
  (want "D-publish-readable a session whose initial load meets a damaged segment writes, and its publication is withheld before any publication (the accepted L)"
        (list (car r) (> (caddr r) 0) (not (list-ref r 4)) (list-ref r 6) (list-ref r 7))
        '(#t #t #t #t #t)))
;; D-interleave (A2's condition 1; MR-29): an undeclared export and a declared
;; search on one store missing a writer, both parked after discovery at the
;; same time in one daemon, each judged by its own class.
(let* ((c (daemon-store!)) (st (car c)) (x (cadddr c))
       (rel (string-append root "/il.release"))
       (env (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=after-discovery:" rel))
       (eout (string-append root "/il-export.out"))
       (sout (string-append root "/il-search.out")))
  ;; The daemon's own first fold passes the hold first; it is released, and
  ;; the hold is re-armed by removing the release (a hold waits while the
  ;; release file does not exist).
  (client-bg! env (string-append root "/il-start.out") "outline" "--store" st)
  (let* ((start-held (wait-until-file (string-append rel ".held") 20000))
         (_ (begin (sh "rm -f " (quoted (string-append rel ".held"))) (touch! rel)
                   (wait-until-text (string-append root "/il-start.out") 30000)
                   (sh "rm -f " (quoted rel))))
         (_ (chmod! "000" (mirror-dir st)))
         (_ (client-bg! env eout "export-code" (string-append x "/il-out") "--store" st))
         (export-held (wait-until-file (string-append rel ".held") 20000))
         (_ (sh "rm -f " (quoted (string-append rel ".held"))))
         (_ (client-bg! env sout "search" "needle" "--store" st))
         (search-held (wait-until-file (string-append rel ".held") 20000)))
    (touch! rel)
    (wait-until-text eout 30000)
    (wait-until-text sout 30000)
    (chmod! "700" (mirror-dir st))
    (stop-daemon! st)
    (let ((e (first-datum-of eout)) (s (first-datum-of sout)))
      (want "D-interleave an undeclared export and a declared search held together are judged each by its own class"
            (list start-held export-held search-held (head-of e) (head-of s) (incomplete-paths s))
            (list #t #t #t '(error incomplete-reduction) 'ok (list (mirror-dir st)))))))

;; =============================================================================
(printf "== K: damage that cuts a writer's history is said ==\n")
;; Readable damage stops a writer's discovery at the error that decided its
;; extent; the answer says so with a `cut` note -- the writer, the path, the
;; kind and after, the last sequence kept -- where it used to say nothing.
;; Every damaged fixture keeps valid records before the cut and asserts the
;; writer's applied sequence (what is kept) beside the note.
(define (k-bytes p) (call-with-port (open-file-input-port p) get-bytevector-all))
(define (k-put! p bv) (call-with-port (open-file-output-port p (file-options no-fail)) (lambda (o) (put-bytevector o bv))))
(define (k-cat . bs) (call-with-bytevector-output-port (lambda (o) (for-each (lambda (b) (put-bytevector o b)) bs))))
;; The writer's applied sequence in a reduction built under the declaration
;; that accepts an incomplete one, 0 when the writer contributed nothing.
(define (k-applied st writer)
  (dynamic-wind
    (lambda () (load-declaration-set! st incomplete-accepted))
    (lambda () (let ((c (assoc writer (reduce-applied-cut (open-and-reduce st))))) (if c (cdr c) 0)))
    (lambda () (load-declaration-set! st #f))))
(define (k-notes a) (let ((c (clause-of a 'incomplete))) (if c (cdr c) 'no-clause)))
(define (k-field c k) (let ((f (and (pair? c) (list? c) (assq k (cdr c))))) (and f (cadr f))))
(define (k-cut w path kind after)
  (list 'cut (list 'writer w) (list 'path path) (list 'reason (symbol->string kind)) (list 'kind kind) (list 'after after)))
;; Which of the writer's records 1..n made their block, in the same
;; reduction: the content kept, beside the extent k-applied reads.
(define (k-kept st writer seqs)
  (dynamic-wind
    (lambda () (load-declaration-set! st incomplete-accepted))
    (lambda () (let ((r (open-and-reduce st))) (filter (lambda (k) (and (state-read r (block-id writer k)) #t)) seqs)))
    (lambda () (load-declaration-set! st #f))))
(define (k-has-block? st id)
  (dynamic-wind
    (lambda () (load-declaration-set! st incomplete-accepted))
    (lambda () (and (state-read (open-and-reduce st) id) #t))
    (lambda () (load-declaration-set! st #f))))
;; Which of the writer's records 1..n an outline names by block id. The
;; outline is text, one "- <id>  <title>" line per block, so the id is
;; looked for between those delimiters, not as a quoted string.
(define (k-listed a writer seqs)
  (filter (lambda (k) (has-substring? (format "~a" a) (string-append "- " (block-id writer k) "  "))) seqs))
;; The integrity kinds check reports for a writer, in its order, or NO-REPORT.
(define (k-integrity-kinds ck writer)
  (let* ((report (k-writer-report ck writer))
         (i (and (list? report) (assq 'integrity (cdr report)))))
    (if (and i (list? (cadr i))) (map car (cadr i)) 'NO-REPORT)))
;; #t when kind a comes before kind b in a list of kinds.
(define (k-before? kinds a b)
  (and (list? kinds)
       (let ((ta (memq a kinds))) (and ta (memq b (cdr ta)) #t))))
;; A RECORD THAT MAKES A BLOCK: a section under the root, so its presence in
;; a reduction, an outline or an evaluation is the record's presence. Every
;; record of a damaged fixture below is one of these, so the extent is
;; measured over records that exist, not over records the reducer set
;; aside as malformed.
(define (k-rec who n)
  (encode-record n (+ 1757300000000 n) who '()
                 (list 'put (list (cons 'kind 'section) (cons 'title (string-append "k." (number->string n)))
                                  '(parent . root) (cons 'ord n)))))
(define (k-fresh-store!) (fresh-store-publishing! (list (k-rec "peer" 1) (k-rec "peer" 2))))
(define (three-segment-store!) (fresh-store-publishing! (list (k-rec "peer" 1) (k-rec "peer" 2) (k-rec "peer" 3))))
;; The writer's report in a check answer: (<writer> (end n) (torn b) (integrity (...))).
(define (k-writer-report ck writer)
  (let find ((x ck))
    (cond ((and (pair? x) (eq? (car x) 'writers) (list? x) (= 2 (length x)) (list? (cadr x))) (assoc writer (cadr x)))
          ((pair? x) (or (find (car x)) (find (cdr x))))
          (else #f))))
(define (mirror-seg st k) (string-append (mirror-dir st) "/" (segment-file-name k)))
(define (manifest-of st) (string-append (mirror-dir st) "/published.sexp"))
;; The first hex digit of a line's CRC changed to another hex digit, so the
;; line still frames and its check fails.
(define (crc-flipped bv start)
  (let ((c (bytevector-copy bv)))
    (bytevector-u8-set! c start (if (= (bytevector-u8-ref c start) 48) 49 48))
    c))
(define (last-line-start bv)
  (let loop ((i (- (bytevector-length bv) 2)))
    (cond ((< i 0) 0) ((= (bytevector-u8-ref bv i) 10) (+ i 1)) (else (loop (- i 1))))))

;; A STORE BUILT BY HAND around one owned writer, for the damage only the
;; local writer's own scan meets (no manifest is read for it): the bytes of
;; every segment are written here, so each `after` is known exactly.
(define HB "k3m9x2qa")
(define (hb-rec n) (k-rec "agent" n))
(define hb-n 0)
(define (hb-store!)
  (set! hb-n (+ hb-n 1))
  (let* ((d (string-append root "/hb" (number->string hb-n))) (st (string-append d "/store")))
    (sh "mkdir -p " (quoted (string-append st "/writers/" HB)) " " (quoted (string-append d "/home")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (k-put! (string-append st "/meta.sexp") (string->utf8 "((format 1) (store-id \"hb\"))\n"))
    (k-put! (string-append st "/lock") (make-bytevector 0))
    (k-put! (string-append st "/writers/" HB "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
    st))
(define (hb-seg st n) (string-append st "/writers/" HB "/" (segment-file-name n)))
(define (hb-seg! st n . bvs) (k-put! (hb-seg st n) (apply k-cat bvs)))
(define (hb-file! st name text) (k-put! (string-append st "/writers/" HB "/" name) (string->utf8 text)))
(define hb-R (bytevector-length (hb-rec 1)))

;; K1: a mirror's older segment with one byte changed -- manifest-hash --
;; read in process, through a local eval, and through the daemon.
;; The evaluation reads the three blocks itself: which of them this
;; reduction holds, as the evaluated code sees them.
(define k1-source
  (string-append "(map (lambda (i) (and (block i) #t)) (list "
                 (format "~s ~s ~s" (block-id M 1) (block-id M 2) (block-id M 3)) "))"))
(let* ((c (three-segment-store!)) (st (car c)) (seg2 (mirror-seg st 2)))
  (damage! seg2)
  (let* ((a (ask st 'outline))
         (ev (let ((out (string-append root "/k1-eval.out")))
               (sh local " scheme --script ../core.sc eval --store " (quoted st) " " (quoted k1-source) " --wire > " out " 2> /dev/null < /dev/null")
               (first-datum-of out))))
    (want "K1 a mirror's older segment damaged: an in-process outline is ok with ONE clause, ONE cut note (the mirror, the segment, manifest-hash, after 1); record 1 kept, 2 and 3 not -- in the reduction and in the outline"
          (list (head-of a) (incomplete-clause-count a) (k-notes a) (k-applied st M) (k-kept st M '(1 2 3)) (k-listed a M '(1 2 3)))
          (list 'ok 1 (list (k-cut M seg2 'manifest-hash 1)) 1 '(1) '(1)))
    (want "K1 a local eval of that store answers ok with the same ONE cut note, and the evaluated code sees record 1's block and not 2's or 3's"
          (list (head-of ev) (incomplete-clause-count ev) (k-notes ev) (clause-of ev 'values))
          (list 'ok 1 (list (k-cut M seg2 'manifest-hash 1)) '(values ((#t #f #f)))))))
;; THE DAEMON ROUTE, NOT THE LOCAL ONE: THEOURGIA_LOCAL sends every verb to
;; the in-process server, so it is removed from the client's environment,
;; and the row asserts a daemon for this store was running and wrote its
;; serve log -- the local route starts neither.
(let* ((c (three-segment-store!)) (st (car c)))
  (set! e-stores (cons st e-stores))
  (damage! (mirror-seg st 2))
  (let* ((r (client "env -u THEOURGIA_LOCAL" "outline" "--store" st))
         (served (list (pair? (daemon-pids st)) (file-exists? (serve-log-path st)))))
    (stop-daemon! st)
    (want "K1 through the daemon: a daemon served it, ok with the same ONE cut note, and the outline names record 1's block and not 2's or 3's"
          (list served (head-of (cadr r)) (incomplete-clause-count (cadr r)) (k-notes (cadr r)) (k-listed (cadr r) M '(1 2 3)))
          (list '(#t #t) 'ok 1 (list (k-cut M (mirror-seg st 2) 'manifest-hash 1)) '(1)))))

;; K1b: the ACTIVE, NON-RETIRED local writer's own older segment (a second
;; segment written after it, so it is sealed; no manifest is read for it),
;; its last record's CRC changed: a crc cut after the record before it. A
;; write on that store is refused as today, integrity.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (beta (caddr c))
       (gamma (new-id (ask st 'insert "--under" "root" "--title" "Gamma"))))
  (let* ((last (k-applied st w))
         (seg1 (string-append (writer-directory st w) "/" (segment-file-name 1)))
         (bv (k-bytes seg1)))
    (k-put! (string-append (writer-directory st w) "/" (segment-file-name 2))
            (encode-record (+ last 1) 1757300000009 "peer" '() '(set "t" q "q")))
    (k-put! seg1 (crc-flipped bv (last-line-start bv)))
    (let* ((a (ask st 'outline)) (kept (k-applied st w))
           (blocks (list (and beta (k-has-block? st beta)) (and gamma (k-has-block? st gamma))))
           (wr (ask st 'insert "--under" "root" "--title" "Refused")))
      (want "K1b the local writer's older segment, its last record's CRC changed: ONE cut note, kind crc, after = the record before it, which is kept (Beta's block present, Gamma's -- the damaged record's -- absent); a write is refused integrity"
            (list (head-of a) (k-notes a) kept blocks (head-of wr) (and (pair? wr) (> (length wr) 2) (caddr wr)))
            (list 'ok (list (k-cut w seg1 'crc (- last 1))) (- last 1) '(#t #f) '(error refused) 'integrity)))))

;; K1c: a mirror's LISTED segment file removed; and its manifest made
;; malformed. Both paths are the manifest's.
(let* ((c (three-segment-store!)) (st (car c)))
  (sh "rm -f " (quoted (mirror-seg st 2)))
  (let ((a (ask st 'outline)))
    (want "K1c a mirror's listed segment removed: ONE cut note, kind manifest-missing-segment, the manifest's path, after 1; record 1 kept, 2 and 3 not"
          (list (head-of a) (k-notes a) (k-applied st M) (k-kept st M '(1 2 3)))
          (list 'ok (list (k-cut M (manifest-of st) 'manifest-missing-segment 1)) 1 '(1)))))
(let* ((c (k-fresh-store!)) (st (car c)))
  (k-put! (manifest-of st) (string->utf8 "(\n"))
  (let ((a (ask st 'outline)))
    (want "K1c a mirror's manifest made malformed: ONE cut note, kind manifest, the manifest's path, after 0; nothing of the mirror kept"
          (list (head-of a) (k-notes a) (k-applied st M) (k-kept st M '(1 2)))
          (list 'ok (list (k-cut M (manifest-of st) 'manifest 0)) 0 '()))))

;; K1e: the kind matrix. On the local writer's older segment (a second one
;; follows it): frame, seq, torn-in-sealed. On a mirror: manifest-range (the
;; hash intact, the declared range of the last segment widened) and
;; retired-malformed.
(let ((st (hb-store!)))
  (hb-seg! st 1 (hb-rec 1) (hb-rec 2) (string->utf8 (string-append (crc32-string-hex "(") " (\n")))
  (hb-seg! st 2 (hb-rec 4))
  (let ((a (ask st 'outline)))
    (want "K1e frame (a checked line whose text does not parse): ONE cut note, kind frame, after 2"
          (list (head-of a) (k-notes a) (k-applied st HB) (k-kept st HB '(1 2 4)))
          (list 'ok (list (k-cut HB (hb-seg st 1) 'frame 2)) 2 '(1 2)))))
(let ((st (hb-store!)))
  (hb-seg! st 1 (hb-rec 1) (hb-rec 2) (hb-rec 4))
  (hb-seg! st 2 (hb-rec 5))
  (let ((a (ask st 'outline)))
    (want "K1e seq (a record whose sequence skips): ONE cut note, kind seq, after 2"
          (list (head-of a) (k-notes a) (k-applied st HB) (k-kept st HB '(1 2 4 5)))
          (list 'ok (list (k-cut HB (hb-seg st 1) 'seq 2)) 2 '(1 2)))))
(let ((st (hb-store!)))
  (hb-seg! st 1 (hb-rec 1) (hb-rec 2) (string->utf8 "abc"))
  (hb-seg! st 2 (hb-rec 3))
  (let ((a (ask st 'outline)))
    (want "K1e torn-in-sealed (an older segment ending without a newline): ONE cut note, kind torn-in-sealed, after 2"
          (list (head-of a) (k-notes a) (k-applied st HB) (k-kept st HB '(1 2 3)))
          (list 'ok (list (k-cut HB (hb-seg st 1) 'torn-in-sealed 2)) 2 '(1 2)))))
(let* ((c (three-segment-store!)) (st (car c))
       (h (lambda (k) (segment-sha (k-bytes (mirror-seg st k))))))
  (k-put! (manifest-of st) (string->utf8 (format "((1 ~s 1 1) (2 ~s 2 2) (3 ~s 3 4))\n" (h 1) (h 2) (h 3))))
  (let ((a (ask st 'outline)))
    (want "K1e manifest-range (segment 3's declared range widened, its hash intact): ONE cut note, kind manifest-range, segment 3, after 2"
          (list (head-of a) (k-notes a) (k-applied st M) (k-kept st M '(1 2 3)))
          (list 'ok (list (k-cut M (mirror-seg st 3) 'manifest-range 2)) 2 '(1 2)))))
(let* ((c (k-fresh-store!)) (st (car c)) (retired (string-append (mirror-dir st) "/retired.sexp")))
  (k-put! retired (string->utf8 "(\n"))
  (let ((a (ask st 'outline)))
    (want "K1e retired-malformed on a mirror: ONE cut note, kind retired-malformed, retired.sexp's path, after 0"
          (list (head-of a) (k-notes a) (k-applied st M) (k-kept st M '(1 2)))
          (list 'ok (list (k-cut M retired 'retired-malformed 0)) 0 '()))))

;; K1d NEGATIVE CONTROL: a retired-mismatch -- the declared offset ends
;; record 3 while the declared seq is 2, the segment published -- is a
;; diagnostic that leaves the extent (3) alone: no clause. That the
;; diagnostic was produced at all is read from check's integrity report, so
;; the row does not pass on a store where it never was.
(let ((st (hb-store!)))
  (hb-seg! st 1 (hb-rec 1) (hb-rec 2) (hb-rec 3))
  (hb-file! st "retired.sexp" (string-append "((prefix 1 " (number->string (* 3 hb-R)) " 2) (tx \"t1\"))\n"))
  (hb-file! st "published.sexp" (format "((1 ~s 1 3))\n" (segment-sha (k-bytes (hb-seg st 1)))))
  (let* ((a (ask st 'outline))
         (report (k-writer-report (ask st 'check) HB))
         (kinds (let ((i (and (list? report) (assq 'integrity (cdr report)))))
                  (if (and i (list? (cadr i))) (map car (cadr i)) 'NO-REPORT))))
    (want "K1d a retired-mismatch is diagnosed (check's report names it), leaves the extent and the content alone, and carries no clause"
          (list (head-of a) (k-notes a) (k-applied st HB) (k-kept st HB '(1 2 3)) (and (list? kinds) (memq 'retired-mismatch kinds) #t))
          (list 'ok 'no-clause 3 '(1 2 3) #t))))

;; K1g PROVENANCE: a retired local segment, unpublished, holding record 1, a
;; record 2 whose CRC fails and record 3, retirement declared at 3 with an
;; offset beyond the file: retired-beyond-file is recorded BEFORE the scan,
;; and the cut is the crc that stopped it, after 1.
(let ((st (hb-store!)))
  (let ((r2 (hb-rec 2)))
    (hb-seg! st 1 (hb-rec 1) (crc-flipped r2 0) (hb-rec 3)))
  (hb-file! st "retired.sexp" (string-append "((prefix 1 " (number->string (* 10 hb-R)) " 3) (tx \"t1\"))\n"))
  (let ((a (ask st 'outline))
        (kinds (k-integrity-kinds (ask st 'check) HB)))
    (want "K1g the cut is the error that decided the extent (crc, after 1), not the first one recorded (retired-beyond-file, which check reports before the crc): ONE cut note; record 1 kept"
          (list (head-of a) (k-notes a) (k-applied st HB) (k-kept st HB '(1 2 3)) (k-before? kinds 'retired-beyond-file 'crc))
          (list 'ok (list (k-cut HB (hb-seg st 1) 'crc 1)) 1 '(1) #t))))

;; K1f STOP PRECEDENCE: a quarantine with fork 2, record 1 valid, damage at
;; record 3: the scan stops at 1 before it reaches the damage, and there is
;; no cut (a pin).
(let ((st (hb-store!)))
  (let ((r3 (hb-rec 3)))
    (hb-seg! st 1 (hb-rec 1) (hb-rec 2) (crc-flipped r3 0)))
  (hb-file! st "quarantine.sexp" "((format 1) (fork 2))\n")
  (let ((a (ask st 'outline)))
    (want "K1f a quarantine that stops the scan before the damage: extent 1, no clause"
          (list (head-of a) (k-notes a) (k-applied st HB) (k-kept st HB '(1 2 3)))
          (list 'ok 'no-clause 1 '(1)))))

;; K2 export-code refuses, naming the cut; K3 conflicts and check.
(let* ((c (three-segment-store!)) (st (car c)) (x (cadddr c)) (seg2 (mirror-seg st 2)) (out (string-append x "/k2-out"))
       (sha2 (segment-sha (k-bytes seg2))))
  (damage! seg2)
  (let* ((a (ask st 'export-code out))
         (cf (ask st 'conflicts))
         (ck (ask st 'check)))
    (want "K2 export-code refuses incomplete-reduction whose notes hold the cut (kind manifest-hash) and writes no file"
          (list (head-of a)
                (let ((ns (and (pair? a) (list? a) (> (length a) 2) (caddr a)))) (and (pair? ns) (eq? (car ns) 'notes) (cdr ns)))
                (file-exists? out))
          (list '(error incomplete-reduction) (list (k-cut M seg2 'manifest-hash 1)) #f))
    ;; CHECK'S REPORT IS TODAY'S, FIELD BY FIELD: the writer's end, torn,
    ;; and the one integrity error with its kind, segment, offset and the
    ;; hash the manifest expected (the segment's before the damage).
    (want "K3 conflicts lists (cut <writer> <path> <kind> <after>); check reports the mirror as today, field by field (end 1, not torn, the manifest-hash error on segment 2 at offset 0 expecting the published hash), and its answer carries the cut clause"
          (list (has-substring? (format "~s" cf) (format "~s" (list 'cut M seg2 'manifest-hash 1)))
                (k-writer-report (without-incomplete ck) M)
                (k-notes ck))
          (list #t
                (list M '(end 1) '(torn #f) (list 'integrity (list (list 'manifest-hash '(segment 2) '(offset 0) (list 'expected sha2)))))
                (list (k-cut M seg2 'manifest-hash 1))))))

;; K4: the local writer's CURRENT segment ending in a residual without a
;; newline, as a crash leaves it: recovery, not damage -- no clause.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c)) (beta (caddr c))
       (delta (new-id (ask st 'insert "--under" "root" "--title" "Delta")))
       (seg1 (string-append (writer-directory st w) "/" (segment-file-name 1)))
       (last (k-applied st w)))
  (k-put! seg1 (k-cat (k-bytes seg1) (string->utf8 "abc")))
  (let ((a (ask st 'outline)))
    (want "K4 a torn end of the local writer's current segment is recovery: no clause, nothing lost (the extent, Beta's block and Delta's -- the last record's -- present)"
          (list (head-of a) (k-notes a) (k-applied st w)
                (and beta (k-has-block? st beta)) (and delta (k-has-block? st delta)))
          (list 'ok 'no-clause last #t #t))))

;; K5: two damaged mirrors and one unreadable mirror -> ONE clause, three
;; notes; one mirror with two damaged older segments -> one note, the
;; earlier cut.
;; Each damaged mirror keeps its first record: the damage is in its SECOND
;; segment, so each cut has a prefix to keep and an exact `after`.
(let* ((c (k-fresh-store!)) (st (car c)) (My "zzzzzzzy") (Mx "zzzzzzzx")
       (My-seg2 (string-append (writer-directory st My) "/" (segment-file-name 2))))
  (log-publish! st My 1 (k-rec "peer" 1) (segment-sha (k-rec "peer" 1)))
  (log-publish! st My 2 (k-rec "peer" 2) (segment-sha (k-rec "peer" 2)))
  (log-publish! st Mx 1 (k-rec "peer" 1) (segment-sha (k-rec "peer" 1)))
  (damage! (mirror-seg st 2))
  (damage! My-seg2)
  (chmod! "000" (writer-directory st Mx))
  (let ((a (ask st 'outline)))
    (chmod! "700" (writer-directory st Mx))
    (let ((kept (list (k-kept st M '(1 2)) (k-kept st My '(1 2)))))
      (want "K5 two damaged mirrors and one unreadable mirror: ONE clause, three notes -- the unreadable one naming its writer, the two cuts whole (writer, segment 2, manifest-hash, after 1) -- and each damaged mirror's record 1 kept, record 2 not"
            (list (head-of a) (incomplete-clause-count a)
                  (and (list? (k-notes a))
                       (map (lambda (n) (if (eq? (car n) 'cut) n (list (car n) (k-field n 'writer)))) (k-notes a)))
                  kept)
            (list 'ok 1
                  (list (list 'unreadable Mx) (k-cut My My-seg2 'manifest-hash 1) (k-cut M (mirror-seg st 2) 'manifest-hash 1))
                  '((1) (1)))))))
(let* ((c (three-segment-store!)) (st (car c)))
  (damage! (mirror-seg st 2))
  (damage! (mirror-seg st 3))
  (let ((a (ask st 'outline)))
    (want "K5 one mirror with two damaged segments: ONE note, the earlier cut (segment 2, after 1), and record 1 kept, 2 and 3 not"
          (list (head-of a) (k-notes a) (k-kept st M '(1 2 3)))
          (list 'ok (list (k-cut M (mirror-seg st 2) 'manifest-hash 1)) '(1)))))

;; K6: eval --working on K1's store -- the parent's load and the worker's
;; both hear the cut, and the parent's merge keeps its kind and after.
(let* ((c (three-segment-store!)) (st (car c)) (w (cadr c)) (seg2 (mirror-seg st 2)))
  (damage! seg2)
  (let ((r (eval-early st w '("1") #f)))
    (want "K6 eval --working: ONE clause whose single note is the cut, kind and after intact through the parent's merge"
          (list (head-of (cadr r)) (incomplete-clause-count (cadr r)) (k-notes (cadr r)))
          (list 'ok 1 (list (k-cut M seg2 'manifest-hash 1))))))

;; K7: an unreadable older segment is still spelled `unreadable`: the tag,
;; the path and the reason asserted explicitly.
(let* ((c (fresh-store!)) (st (car c)) (seg (older-segment st)))
  (chmod! "000" seg)
  (let ((a (ask st 'outline)))
    (chmod! "600" seg)
    (want "K7 an unreadable older segment keeps its unreadable note (today's path and reason, no kind, no after)"
          (list (head-of a) (k-notes a))
          (list 'ok (list (list 'unreadable (list 'writer M) (list 'path seg) (list 'reason "Permission denied")))))))

;; K8: the healthy local writer stays writable beside a cut mirror.
(let* ((c (three-segment-store!)) (st (car c)) (seg2 (mirror-seg st 2)))
  (damage! seg2)
  (let* ((a (ask st 'insert "--under" "root" "--title" "Kept"))
         (id (new-id a)))
    (want "K8 a declared insert by the healthy local writer beside the cut mirror: ok, its record appended (its block read back from a new reduction), and the mirror's cut carried"
          (list (head-of a) (and id (k-has-block? st id)) (k-notes a))
          (list 'ok #t (list (k-cut M seg2 'manifest-hash 1))))))

;; K9: an unreadable snapshot beside a cut mirror. The snapshot is taken
;; while the store is whole, then the mirror's second segment is damaged and
;; the snapshot made unreadable. The load finds the cut in discovery and
;; tells the request before it reads the snapshot, so the answer is the
;; snapshot's own refusal and still carries the cut.
(let* ((c (three-segment-store!)) (st (car c)) (seg2 (mirror-seg st 2))
       (snap (begin (ask st 'snapshot)
                    (let ((fs (guard (e (#t '())) (directory-list (string-append st "/snap")))))
                      (and (= 1 (length fs)) (string-append st "/snap/" (car fs)))))))
  (damage! seg2)
  (when snap (chmod! "000" snap))
  (let ((a (ask st 'outline)))
    (when snap (chmod! "600" snap))
    (want "K9 an unreadable snapshot beside a cut mirror: the answer is the snapshot's refusal, naming it, and carries the cut"
          (list (and snap #t) a)
          (list #t (append (unreadable-answer snap) (list (list 'incomplete (k-cut M seg2 'manifest-hash 1))))))))

(for-each stop-daemon! e-stores)
(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(sh "chmod -R u+rwX " root " 2>/dev/null")
(printf "unreadable-acknowledge-routes complete\n")
(exit (if (= bad 0) 0 1))
