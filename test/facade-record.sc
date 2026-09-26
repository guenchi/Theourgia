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

;; THE MUTATION RECORD IS KEYED BY THE ACTOR (F100 D1', amendment 8). A
;; parameter cell belongs to whichever process wrote it last and does not
;; survive a yield, so the record is looked up by `self`. Three rows, each
;; under the scheduler:
;;   (a) two actors, both scopes open before either writes, writing in
;;       turn: each record holds its own entries only;
;;   (b) an actor with no scope notes nothing, and `mutation-record` outside
;;       a scope answers the empty list;
;;   (c) a scope that spawns another actor to do the work establishes the
;;       scope inside that actor, handing it the entries so far
;;       (`with-mutation-record`'s optional second argument), and reads the
;;       combined record back.
(import (chezscheme) (theourgia ffi) (theourgia sched) (theourgia answers)
        ;; F100b M2a's two helper rows (H5, H7).
        (only (theourgia daemon) write-report-line!)
        (only (theourgia client) next-attempt-token))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define dir (string-append scratch-base "/facade-record-" (number->string (get-process-id))))
(mkdir-p! dir)
(define (under name) (string-append dir "/" name))

;; ONE FILE, CREATED AND WRITTEN THROUGH THE DOOR: its record is exactly
;; ((create p) (write p)); fsync and close note nothing.
;; THE TREE BY CONSTRUCTION (rulings J and K, RR3): make-one! reads back
;; what it wrote, and ANY failure -- the open, the write, the close, the
;; read-back, or bytes that differ -- raises its one marker,
;; (make-one!-failed path reason). Nothing in this file swallows it: an
;; actor answers it through raised-as, the main actor's rows through `want`.
;; So every row of this file asserts the tree through the marker; rows (a)
;; and (c) also project the bytes. A row that needs a write without the
;; read-back says so; none does today.
(define (bytes-of path) (utf8->string (entry-bytes path)))
(define (make-one! path)
  (let ((got (guard (e (#t (list 'raised (if (and (condition? e) (message-condition? e))
                                             (condition-message e)
                                             e))))
               (let ((fd (fd-open path '(write create))))
                 (write-all! fd (string->utf8 "r\n"))
                 (fd-close fd))
               (bytes-of path))))
    (unless (equal? got "r\n")
      (raise (list 'make-one!-failed path got)))))
;; AN ACTOR THAT RAISES ANSWERS WITH THE RAISE, so the row reads a wrong
;; answer rather than waiting out a timeout for a reply that never comes.
(define (raised-as e)
  (list 'raised (if (and (condition? e) (message-condition? e)) (condition-message e) e)))

;; ---- F100b H1, H2: the table and the aggregation rule (pure) -----------------
;;
;; (theourgia answers)' two helpers on fixed conditions and records, one row
;; per cell of the brief. The durable-error is ffi's vector
;; #(durable-error op (path . errno)); the unreadable-entry is ffi's own
;; constructor. Errnos: names for unreadable-entry, the numbers through ffi's
;; exported constants for durable-error (ruling D1).
(define (unreadable-at p errno-name reason) (make-unreadable-entry p reason errno-name))
(define (durable op p errno) (vector 'durable-error op (cons p errno)))
(define h-record '((mkdir "/a") (create "/a/b")))
(define no-such "No such file or directory")
(define denied "Permission denied")
(want "H1 unreadable-entry ENOENT, empty record -> absent"
      (classify-failure (unreadable-at "/p" 'ENOENT no-such) '())
      (list 'error 'absent '(path "/p") (list 'reason no-such) '(errno ENOENT)))
(want "H1 unreadable-entry ENOTDIR, empty record -> absent with ENOTDIR"
      (classify-failure (unreadable-at "/p" 'ENOTDIR "Not a directory") '())
      '(error absent (path "/p") (reason "Not a directory") (errno ENOTDIR)))
(want "H1 unreadable-entry EACCES, empty record -> unreadable"
      (classify-failure (unreadable-at "/p" 'EACCES denied) '())
      (list 'error 'unreadable '(path "/p") (list 'reason denied) '(errno EACCES)))
(want "H1 durable-error mkdir 13, empty record -> unwritable with op, the reason strerror's"
      (classify-failure (durable 'mkdir "/p" EACCES) '())
      (list 'error 'unwritable '(op mkdir) '(path "/p") (list 'reason denied) (list 'errno EACCES)))
(want "H1 durable-error dir-fsync 5, empty record -> unwritable (op dir-fsync)"
      (classify-failure (durable 'dir-fsync "/d" EIO) '())
      (list 'error 'unwritable '(op dir-fsync) '(path "/d") (list 'reason "Input/output error") (list 'errno EIO)))
(for-each
  (lambda (c)
    (let ((failure (car c)) (clauses (cdr c)))
      (want (string-append "H1 " (car clauses) " with a record -> incomplete, its clauses under failed, the record as written")
            (classify-failure failure h-record)
            (list 'error 'incomplete (cons 'failed (cdr clauses)) (list 'written h-record)))))
  (list (list (unreadable-at "/p" 'ENOENT no-such) "ENOENT" '(path "/p") (list 'reason no-such) '(errno ENOENT))
        (list (unreadable-at "/p" 'ENOTDIR "Not a directory") "ENOTDIR" '(path "/p") '(reason "Not a directory") '(errno ENOTDIR))
        (list (unreadable-at "/p" 'EACCES denied) "EACCES" '(path "/p") (list 'reason denied) '(errno EACCES))
        (list (durable 'mkdir "/p" EACCES) "durable mkdir" '(op mkdir) '(path "/p") (list 'reason denied) (list 'errno EACCES))
        (list (durable 'dir-fsync "/d" EIO) "durable dir-fsync" '(op dir-fsync) '(path "/d") '(reason "Input/output error") (list 'errno EIO))))
;; A durable-error with no errno is a short write (M2a review r1, F3):
;; the table answers it with a reason of its own instead of raising.
(want "H1 durable write with errno #f (a short write), empty record -> unwritable, reason \"short write\", errno #f"
      (classify-failure (durable 'write "/p" #f) '())
      '(error unwritable (op write) (path "/p") (reason "short write") (errno #f)))
(want "H1 durable create with errno #f (a Chez condition with no mapped errno), empty record -> unwritable, reason \"no errno\" (M2a review r2, F3)"
      (classify-failure (durable 'create "/p" #f) '())
      '(error unwritable (op create) (path "/p") (reason "no errno") (errno #f)))
(want "H1 what the table does not classify answers #f (the caller keeps its own answer)"
      (list (classify-failure (make-message-condition "x") '()) (classify-failure 'boom h-record))
      '(#f #f))
(define h-entries '((create "/l")))
;; Each input is the table's own shape for its kind: unwritable carries
;; its op, so a promotion that dropped a clause would show (F100b M1
;; review r1, F2).
(for-each
  (lambda (answer)
    (want (string-append "H2 " (symbol->string (cadr answer)) " with an empty record plus entries -> incomplete, every clause under failed")
          (combine-report answer h-entries)
          (list 'error 'incomplete (cons 'failed (cddr answer)) (list 'written h-entries))))
  (list '(error absent (path "/p") (reason "No such file or directory") (errno ENOENT))
        '(error unreadable (path "/p") (reason "r") (errno EACCES))
        (list 'error 'unwritable '(op mkdir) '(path "/p") (list 'reason denied) (list 'errno EACCES))))
(want "H2 incomplete with written W plus entries E -> written W then E"
      (combine-report '(error incomplete (failed (path "/p")) (written ((mkdir "/a")))) h-entries)
      '(error incomplete (failed (path "/p")) (written ((mkdir "/a") (create "/l")))))
(want "H2 any answer plus no entries is unchanged, and its written text identical"
      (let* ((a '(error unreadable (path "/p") (reason "r") (errno EACCES)))
             (b (combine-report a '())))
        (list (equal? a b) (string=? (format "~s" a) (format "~s" b))))
      '(#t #t))
;; A named outcome may carry atoms after its kind: store.sc answers
;; (error not-written reserved-not-written (sequence n)) (F100b M1 review
;; r1, F1: a lookup by assq raised on it).
(want "H2 a named outcome carrying an atom after its kind gains (written E) and keeps every clause"
      (list (with-written '(error not-written reserved-not-written (sequence 2)) h-entries)
            (combine-report '(error not-written reserved-not-written (sequence 2)) h-entries))
      (let ((a (list 'error 'not-written 'reserved-not-written '(sequence 2) (list 'written h-entries))))
        (list a a)))
(want "H2 a named outcome gains (written E) appended and keeps its name"
      (combine-report '(error eval-exception (kind raised) (message "m")) h-entries)
      (list 'error 'eval-exception '(kind raised) '(message "m") (list 'written h-entries)))
(want "H2 an answer already carrying written, plus no entries, is unchanged"
      (combine-report '(error working-unavailable (message "m") (written ((create "/w")))) '())
      '(error working-unavailable (message "m") (written ((create "/w")))))
(want "H2 a named outcome already carrying (written W) plus non-empty E -> ONE written clause holding W then E"
      (combine-report '(error working-unavailable (message "m") (written ((create "/w")))) h-entries)
      '(error working-unavailable (message "m") (written ((create "/w") (create "/l")))))
(want "H2 a success answer is never given a written clause"
      (combine-report '(ok (values (3))) h-entries)
      '(ok (values (3))))

;; ---- F100b M2a: the startup report's one write, and the attempt token ----
;;
;; H5 (brief item 3, E4): write-report-line! writes newline, the datum,
;; newline, as ONE write, and the caller's record holds exactly one entry
;; for it. The descriptor is opened outside the scope, so the scope holds
;; the report's write alone.
;; STATED LIMIT (M2a review r1, F6, ruled): "one call to write-once, read at
;; write-one!'s body, not measured". A write-one! that split a successful
;; write into two syscalls while keeping one note and one trace would leave
;; the file, the record and the fault as they are; no row here counts
;; syscalls (that would be a measurement of the kernel, not of the door).
(let* ((p (under "h5.log"))
       (fd (fd-open p '(write append create)))
       (record (with-mutation-record
                 (lambda ()
                   (write-report-line! fd '(error x (path "/p") (attempt "0123456789abcdef")))
                   (mutation-record)))))
  (fd-close fd)
  (want "H5 write-report-line! writes \\n + the datum + \\n exactly, and the record holds one write entry naming the file"
        (list (bytes-of p) record)
        (list "\n(error x (path \"/p\") (attempt \"0123456789abcdef\"))\n" (list (list 'write p)))))
;; H5b: under short-write@report the call raises durable-error (op write,
;; errno #f) and the file holds no complete report line -- only the seven
;; bytes the short write offered, never a second syscall's. Run as a child
;; process, since the fault is read from the environment at start.
(let* ((p (under "h5b.log"))
       (script (under "h5b.ss"))
       (out (under "h5b.out")))
  (call-with-output-file script
    (lambda (o)
      (write '(import (chezscheme) (theourgia ffi) (only (theourgia daemon) write-report-line!)) o)
      ;; Inside a record scope (M2a review r1, F8): the short write that
      ;; really ran is noted once, like a whole one.
      (write `(let ((fd (fd-open ,p '(write append create))))
                (write (with-mutation-record
                         (lambda ()
                           (list (guard (e ((fs-error? e) (list 'raised (fs-error-op e) (fs-error-errno e))))
                                   (parameterize ((theourgia-stage 'report))
                                     (write-report-line! fd '(error x (attempt "0123456789abcdef"))))
                                   'returned)
                                 (mutation-record)))))
                (fd-close fd))
             o)))
  (let ((rc (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=short-write@report scheme --script "
                                   script " > " out " 2> /dev/null < /dev/null"))))
    (want "H5b under short-write@report write-report-line! raises durable-error (op write, errno #f), notes the one write, and leaves no complete line: the seven offered bytes only"
          (list rc (guard (e (#t 'unread)) (call-with-input-file out read)) (bytes-of p))
          (list 0 (list '(raised write #f) (list (list 'write p))) "\n(error"))))
;; H5d (M2a review r1, F7): the port's buffered bytes are flushed before the
;; report, so they land before it: a child prints "pre" through the port,
;; writes a report to fd 1, prints "post", and its stdout reads in that
;; order.
(let* ((script (under "h5d.ss")) (out (under "h5d.out")))
  (call-with-output-file script
    (lambda (o)
      (write '(import (chezscheme) (only (theourgia daemon) write-report-line!)) o)
      (write '(begin (display "pre") (write-report-line! 1 '(error x (attempt #f))) (display "post")) o)))
  (let ((rc (system (string-append "scheme --script " script " > " out " 2> /dev/null < /dev/null"))))
    (want "H5d buffered port output lands before the report: stdout is pre, the framed report, post"
          (list rc (bytes-of out))
          (list 0 "pre\n(error x (attempt #f))\npost"))))
;; H7 (D11): two tokens from one process are distinct, each 16 lowercase hex.
(let* ((a (next-attempt-token)) (b (next-attempt-token))
       (hex16? (lambda (t) (and (string? t) (= 16 (string-length t))
                                (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))
                                         (string->list t))))))
  ;; The first eight characters are this process's pid (M2a review r1, F13).
  (want "H7 next-attempt-token twice in one process: two distinct strings of 16 lowercase hex, the first eight this pid"
        (list (hex16? a) (hex16? b) (string=? a b)
              (string->number (substring a 0 8) 16) (string->number (substring b 0 8) 16))
        (list #t #t #f (get-process-id) (get-process-id))))

(define scope-exits '(normal raise escape))
(define scope-enclosings '(none outer))
(define (cell-path tag part) (under (string-append tag "-" part)))
(define (scope-cell exit enclosing tag)
  (define (inner)
    (case exit
      ((normal)
       (with-mutation-record (lambda () (make-one! (cell-path tag "inner")))))
      ((raise)
       (guard (e ((eq? e 'cell-boom) 'caught))
         (with-mutation-record
           (lambda () (make-one! (cell-path tag "inner")) (raise 'cell-boom)))))
      ((escape)
       (call/cc
         (lambda (k)
           (with-mutation-record
             (lambda () (make-one! (cell-path tag "inner")) (k 'out))))))))
  (case enclosing
    ((none)
     (let ((pre (mutation-record)))
       (inner)
       (given-back pre (mutation-record))))
    ((outer)
     ;; THE OUTER RECORD, AND WHAT IS LEFT AFTER THE OUTER SCOPE HAS GONE
     ;; (ruling K; review r5): read inside, the outer record cannot see an
     ;; outer scope whose own exit leaves it installed.
     (let* ((pre (mutation-record))
            (rec (with-mutation-record
                   (lambda ()
                     (make-one! (cell-path tag "before"))
                     (inner)
                     (make-one! (cell-path tag "after"))
                     (mutation-record)))))
       (list rec (given-back pre (mutation-record)))))))
;; A FRESH ACTOR FINDS NO RECORD: each cell runs in an actor of its own, so
;; unlike facade-ffi's process-wide key, a record found before the scope is
;; itself a failure (one key shared between actors), and after the scope
;; the record must be () again.
(define (given-back pre post)
  (cond ((not (null? pre)) (list 'found-before pre))
        ((equal? post pre) '())
        (else (list 'left-behind post))))
(define (scope-cell-expect enclosing tag)
  (case enclosing
    ((none) '())
    ((outer)
     (list (list (list 'create (cell-path tag "before")) (list 'write (cell-path tag "before"))
                 (list 'create (cell-path tag "after")) (list 'write (cell-path tag "after")))
           '()))))

(start-scheduler
  (lambda ()
    (let ((main self))

      ;; ---- (a) two actors, disjoint records -----------------------------
      ;; BOTH SCOPES ARE OPEN BEFORE EITHER WRITES (F100a review r1). A
      ;; opens its scope and parks; B opens its scope and parks; then A
      ;; writes, then B writes, then each reports. With the record keyed by
      ;; the actor each holds only its own entries. With one key for both,
      ;; B's scope, installed last, would take A's write as well -- the
      ;; earlier shape, where A wrote before B's scope opened, stayed green
      ;; under that mistake.
      (let* ((a (spawn (lambda ()
                         (guard (e (#t (send main (list 'a-record (raised-as e)))))
                         (with-mutation-record
                           (lambda ()
                             (send main '(a-open))
                             (receive (after 5000 'no-go) (`(write) 'go))
                             (make-one! (under "rec-a"))
                             (send main '(a-wrote))
                             (receive (after 5000 'no-go) (`(report) 'go))
                             (send main (list 'a-record (mutation-record)))))))))
             (b (spawn (lambda ()
                         (guard (e (#t (send main (list 'b-record (raised-as e)))))
                         (with-mutation-record
                           (lambda ()
                             (send main '(b-open))
                             (receive (after 5000 'no-go) (`(write) 'go))
                             (make-one! (under "rec-b"))
                             (send main '(b-wrote))
                             (receive (after 5000 'no-go) (`(report) 'go))
                             (send main (list 'b-record (mutation-record))))))))))
        (receive (after 5000 'no-a-open) (`(a-open) 'ok))
        (receive (after 5000 'no-b-open) (`(b-open) 'ok))
        (send a '(write))
        (receive (after 5000 'no-a-wrote) (`(a-wrote) 'ok))
        (send b '(write))
        (receive (after 5000 'no-b-wrote) (`(b-wrote) 'ok))
        (send a '(report))
        (let ((a-rec (receive (after 5000 'no-a-record) (`(a-record ,r) r))))
          (send b '(report))
          (let ((b-rec (receive (after 5000 'no-b-record) (`(b-record ,r) r))))
            (want "F100-A record (a) two actors with both scopes open, writing in turn, each record only their own file"
                  (list a-rec b-rec (bytes-of (under "rec-a")) (bytes-of (under "rec-b")))
                  (list (list (list 'create (under "rec-a")) (list 'write (under "rec-a")))
                        (list (list 'create (under "rec-b")) (list 'write (under "rec-b")))
                        "r\n" "r\n")))))

      ;; ---- (b) no scope, no record ----------------------------------------
      (spawn (lambda ()
               (send main (list 'none-record
                                (guard (e (#t (raised-as e)))
                                  (make-one! (under "rec-none"))
                                  (mutation-record))))))
      (want "F100-A record (b) an actor with no scope notes nothing; mutation-record outside a scope is ()"
            (receive (after 5000 'no-none-record) (`(none-record ,r) r))
            '())

      ;; ---- (c) a scope hands its entries to the actor it spawns -----------
      ;; The spawning scope creates one file, spawns a worker with the entries
      ;; so far, the worker's scope creates a second, and the spawner reads
      ;; the combined record back from the worker.
      (want "F100-A record (c) a scope that spawns its work hands the entries so far and reads the combined record back"
            (list
             (with-mutation-record
              (lambda ()
                (make-one! (under "rec-c1"))
                (let ((so-far (mutation-record)))
                  (spawn (lambda ()
                           (guard (e (#t (send main (list 'c-record (raised-as e)))))
                             (with-mutation-record
                               (lambda ()
                                 (make-one! (under "rec-c2"))
                                 (send main (list 'c-record (mutation-record))))
                               so-far))))
                  (receive (after 5000 'no-c-record) (`(c-record ,r) r)))))
             (bytes-of (under "rec-c1"))
             (bytes-of (under "rec-c2")))
            (list (list (list 'create (under "rec-c1")) (list 'write (under "rec-c1"))
                        (list 'create (under "rec-c2")) (list 'write (under "rec-c2")))
                  "r\n" "r\n"))

      ;; ---- the exit matrix, each cell in its own actor --------------------
      ;; RR4 and RR5 (ruling J): the same six cells as facade-ffi's matrix --
      ;; a normal return, a raise caught outside, a continuation escape,
      ;; each with no scope around it or inside an outer one -- run under
      ;; the scheduler, one fresh actor per cell, so the key is the actor's
      ;; and a cell starts with no record. The actor answers from a guard.
      (for-each
        (lambda (exit)
          (for-each
            (lambda (enclosing)
              (let ((tag (string-append "actor-" (symbol->string exit) "-" (symbol->string enclosing))))
                (spawn (lambda ()
                         (send main (list 'cell (guard (e (#t (raised-as e)))
                                                  (scope-cell exit enclosing tag))))))
                (want (string-append "F100a exit matrix in an actor [" (symbol->string exit) " x "
                                     (symbol->string enclosing) "]: the scope gives back what was there before")
                      (receive (after 5000 'no-cell) (`(cell ,v) v))
                      (scope-cell-expect enclosing tag))))
            scope-enclosings))
        scope-exits)

      (system (string-append "rm -rf " dir))
      (printf "rows: ~a\n~a failures\nfacade-record complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
