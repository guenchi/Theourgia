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
(import (chezscheme) (theourgia ffi) (theourgia sched))

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
