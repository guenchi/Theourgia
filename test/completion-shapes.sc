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

;; EVERY ENTRY SHAPE A COMPLETION CAN BE HANDED, AND WHERE ITS REFERENCES ARE.
;;
;; A completion binds the markers of a plan's missing members, and refuses,
;; before anything is written, a marker that does not name an earlier
;; insert of the plan. Both depend on finding the references in an entry.
;; This fixture generates the entries that can reach a completion and
;; checks, through the library's one export, that the references are found
;; in every one of them.
;;
;; KEY: THE BOUND -- WHICH ENTRIES CAN REACH A COMPLETION.
;;
;;   (a) What the store writes: an insert or a move of exactly four parts,
;;       (insert <parent> <sibling> <fields>) and (move <id> <parent>
;;       <sibling>), bare or inside a three-part wrapper (expect <hash>
;;       <intent>).
;;
;;   (b) What the reducer admits beyond that. A plan record is admitted by
;;       request.sc's plan?, which asks of each entry only that it be a
;;       proper, nonempty list whose head is a symbol. So:
;;       - a bare insert or move may have any number of parts after its
;;         head, 0 or more, and must end properly;
;;       - a wrapper (expect ...) may have any number of parts, and its
;;         parts are not examined: the intent inside it may be any datum --
;;         shorter or longer than four parts, ending in a dotted tail, a
;;         wrapper itself, or not a form at all;
;;       - any other symbol may head an entry.
;;       Nothing else is admitted: an entry that is not a proper list, or
;;       whose head is not a symbol, does not make a plan.
;;
;;   THE REFERENCE POSITIONS ARE FIXED: an insert's elements 1 (parent) and
;;   2 (sibling), a move's elements 2 (parent) and 3 (sibling), each
;;   counted only when the entry reaches it. The intent is read through ONE
;;   wrapper, as the store reads it: an entry (expect <a> <b> ...) has the
;;   intent <b>; an intent that is itself a wrapper is not an insert or a
;;   move, and has no references.
;;
;; THE GENERATOR covers that bound: kind (insert, move) x parts after the
;; head (0 to 5) x wrapper (none; (expect h I); (expect h I extra);
;; (expect h I) with I dotted; (expect h (expect h I))) x a marker at each
;; position the entry has, or nowhere x the marker valid (names an earlier
;; insert), naming no member, or naming an earlier member that is not an
;; insert. And a few admitted shapes outside the two kinds.
;;
;; THE PROPERTY, for each generated entry as the third member of a plan
;; whose first is a missing insert and whose second a missing set:
;;   - a bad marker at a reference position: the answer is no-such-intent
;;     naming the marker, and the runner is never called;
;;   - a valid marker at a reference position: the runner receives the
;;     entry with that position bound to (from 0), every other part as
;;     generated;
;;   - no marker at a reference position: the runner receives the entry as
;;     generated;
;;   - nothing raises.
;; NEVER: THE EXPECTED COLUMN IS NOT THE LIBRARY'S READER. Whether a place
;; is a reference is decided from how the entry was generated (its kind,
;; its wrapper, the place the marker was put), by the few lines of
;; `reference-place?` below -- a second implementation of the positions
;; rule, sharing no code with the library's.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia request) (theourgia ffi) (theourgia wire) (theourgia working)
        (theourgia log))

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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/completion-shapes-" (number->string (get-process-id))))
(when (file-exists? root) (error 'completion-shapes "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; ---- one store holding one applied plan ------------------------------------
;;
;; completion-run reads the plan's cut and its applied members from the
;; state, and takes the entries it judges as an argument. So one forged
;; plan, applied and with no member applied, serves every generated entry.

(define store (string-append root "/store"))
(define writer (cadr (assq 'writer (cdr (rpc-dispatch store '(init) "test")))))
(define (state) (open-and-reduce store))
(define M
  (let ((ev (car (cadr (assq 'events (cdr (rpc-dispatch store '(insert "--title" "M" "--text" "old") "test")))))))
    (block-id (car ev) (cdr ev))))
(define section '((kind . section) (title . "N")))
(define first-entries
  (list (cons 0 (list 'insert 'root #f section)) (cons 1 (list 'set M 'src "x"))))
(define plan-event
  (let* ((after (or (assoc writer (reduce-applied-cut (state))) (cons writer 0)))
         (fp (request-fingerprint "test" 'commit (list writer M) after))
         (plan (list 'plan "SHAPES" fp after
                     (append first-entries (list (cons 2 (list 'insert 'root #f section))))))
         (actor (list "test" (cons writer "SHAPES") 'plan fp #f after))
         (frame (encode-record 1 1789000000005 actor (reduce-applied-cut (state)) (storable-encode plan))))
    (log-publish! store "forged00" 1 frame (segment-sha frame))
    (cons "forged00" 1)))
(define plan-state (state))
(define completion-run (eval 'completion-run (environment '(theourgia completion))))

;; -> (calls answer raised): the entry as member 2, every member missing.
(define (complete entry)
  (let* ((entries (append first-entries (list (cons 2 entry))))
         (calls '())
         (answer (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
                   (completion-run plan-state plan-event entries entries '(0 1 2) #f
                                   (lambda (run after-each) (set! calls (cons run calls)) 'ran)))))
    (list (reverse calls) answer (and (pair? answer) (eq? (car answer) 'RAISED)))))

;; ---- the generator ---------------------------------------------------------

(define valid '("#%new" 0))
(define markers (list valid '("#%new" 9) '("#%new" 1)))
(define (filler kind p)
  (if (eq? kind 'insert)
      (case p ((1) 'root) ((2) #f) ((3) section) (else 'extra))
      (case p ((1) M) ((2) 'root) ((3) #f) (else 'extra))))
;; A PLACE is (intent p), (wrapper q) for the outer wrapper's element q, or
;; (inner 1) for the subject of a wrapper inside the wrapper; none is #f.
(define (intent kind n dotted place value)
  (let loop ((p n) (tail (if dotted 'tail '())))
    (if (= p 0)
        (cons kind tail)
        (loop (- p 1) (cons (if (equal? place (list 'intent p)) value (filler kind p)) tail)))))
(define (entry kind n wrapper place value)
  (let ((at (lambda (where other) (if (equal? place where) value other))))
    (case wrapper
      ((none) (intent kind n #f place value))
      ((w3) (list 'expect (at '(wrapper 1) "0000") (intent kind n #f place value)))
      ((w4) (list 'expect (at '(wrapper 1) "0000") (intent kind n #f place value) (at '(wrapper 3) 'more)))
      ((wd) (list 'expect (at '(wrapper 1) "0000") (intent kind n #t place value)))
      ((wn) (list 'expect (at '(wrapper 1) "0000")
                  (list 'expect (at '(inner 1) "0000") (intent kind n #f place value)))))))
(define (places n wrapper)
  (append (let loop ((p n) (out '())) (if (= p 0) out (loop (- p 1) (cons (list 'intent p) out))))
          (case wrapper
            ((none) '())
            ((w3 wd) '((wrapper 1)))
            ((w4) '((wrapper 1) (wrapper 3)))
            ((wn) '((wrapper 1) (inner 1))))))
(define wrappers '(none w3 w4 wd wn))

;; THE POSITIONS RULE, from how the entry was made.
(define (reference-place? kind wrapper place)
  (and place (not (eq? wrapper 'wn)) (eq? (car place) 'intent)
       (let ((start (if (eq? kind 'insert) 1 2)))
         (<= start (cadr place) (+ start 1)))))

;; -> ((class entry expected-answer-or-run-entry) ...)
(define cases
  (let ((out '()))
    (define (add! c) (set! out (cons c out)))
    (for-each
      (lambda (kind)
        (do ((n 0 (+ n 1))) ((> n 5))
          (for-each
            (lambda (wrapper)
              (add! (list 'no-marker-at-reference (entry kind n wrapper #f #f) (entry kind n wrapper #f #f)))
              (for-each
                (lambda (place)
                  (for-each
                    (lambda (m)
                      (let ((e (entry kind n wrapper place m)))
                        (cond ((not (reference-place? kind wrapper place))
                               (add! (list 'no-marker-at-reference e e)))
                              ((equal? m valid)
                               (add! (list 'valid-at-reference e (entry kind n wrapper place '(from 0)))))
                              (else
                               (add! (list 'bad-at-reference e (list (list 'error 'no-such-intent (cadr m)))))))))
                    markers))
                (places n wrapper)))
            wrappers)))
      '(insert move))
    ;; Admitted shapes outside the two kinds: no references at all.
    (for-each
      (lambda (m)
        (for-each (lambda (e) (add! (list 'no-marker-at-reference e e)))
                  (list (list 'expect m) (list 'expect "0000" m) (list 'expect "0000" (list "insert" m))
                        (list 'expect "0000" 'insert m) (list 'frob m))))
      markers)
    (reverse out)))

;; THE SIZE OF THE PRODUCT, counted by arithmetic rather than by the
;; generator's loops: per kind and n, (1 + 3 places) entries for each
;; wrapper, places being n, n+1, n+2, n+1, n+2; and 5 x 3 others.
(define product
  (+ (* 2 (let sum ((n 0) (acc 0))
            (if (> n 5) acc
                (sum (+ n 1) (+ acc (+ 1 (* 3 n)) (* 2 (+ 1 (* 3 (+ n 1)))) (* 2 (+ 1 (* 3 (+ n 2)))))))))
     (* 5 3)))

;; ---- the property ----------------------------------------------------------

(define results (map (lambda (c) (cons c (complete (cadr c)))) cases))
(define (of class) (filter (lambda (r) (eq? (car (car r)) class)) results))
(define (holds? r)
  (let* ((c (car r)) (calls (cadr r)) (answer (caddr r)) (raised (cadddr r)))
    (and (not raised)
         (case (car c)
           ((bad-at-reference) (and (null? calls) (equal? answer (caddr c))))
           (else (and (= 1 (length calls)) (equal? (list-ref (car calls) 2) (caddr c))))))))
(define (failing class) (filter (lambda (r) (not (holds? r))) (of class)))
(define (shown rs) (list (length rs) (map (lambda (r) (list (cadr (car r)) (caddr r) (and (pair? (cadr r)) (list-ref (car (cadr r)) 2)))) (if (> (length rs) 3) (list-head rs 3) rs))))

(printf "entries ~a of ~a; bad-at-reference ~a, valid-at-reference ~a, no-marker-at-reference ~a\n"
        (length cases) product (length (of 'bad-at-reference)) (length (of 'valid-at-reference))
        (length (of 'no-marker-at-reference)))
(unless (= (length cases) product)
  (printf "NOT A READING: the generator made ~a entries, the product of its axes is ~a\n" (length cases) product))

(want "G the generator made every entry of the product" (length cases) product)
(want "G every class has entries"
      (map (lambda (k) (> (length (of k)) 0)) '(bad-at-reference valid-at-reference no-marker-at-reference))
      '(#t #t #t))
(want "G a bad marker at a reference: no-such-intent, the runner never called (failing, first three)"
      (shown (failing 'bad-at-reference)) '(0 ()))
(want "G a valid marker at a reference: bound to (from 0), the rest as generated (failing, first three)"
      (shown (failing 'valid-at-reference)) '(0 ()))
(want "G no marker at a reference: the runner receives the entry as generated (failing, first three)"
      (shown (failing 'no-marker-at-reference)) '(0 ()))
(want "G nothing raises" (length (filter cadddr results)) 0)

(printf "rows: ~a\n~a failures\ncompletion-shapes complete\n" rows bad)
