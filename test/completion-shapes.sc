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
;;   (a) What the store writes: an insert, a move, a link or an unlink of
;;       exactly four parts, (insert <parent> <sibling> <fields>), (move
;;       <id> <parent> <sibling>), (link <from> <rel> <to>) and (unlink
;;       <from> <rel> <to>), bare or inside a three-part wrapper (expect
;;       <hash> <intent>).
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
;;   2 (sibling), a move's elements 2 (parent) and 3 (sibling), a link's
;;   and an unlink's elements 1 (from) and 3 (to), each counted only when
;;   the entry reaches it. This file keeps its own copy of the positions
;;   (ref-positions below), so it is a reading of the store's table and
;;   not the table read back. The intent is read through ONE
;;   wrapper, as the store reads it: an entry (expect <a> <b> ...) has the
;;   intent <b>; an intent that is itself a wrapper is not an insert or a
;;   move, and has no references.
;;
;; THE GENERATOR covers that bound: kind (insert, move, link, unlink) x parts after the
;; head (0 to 5) x wrapper (none; (expect h I); (expect h I extra);
;; (expect h (expect h I))) x the intent ending properly or in a dotted tail
;; (under a wrapper; a bare entry must end properly) x the markers: one at
;; each place the entry has, or none, or one at each of the two reference
;; positions together x each marker valid (names an earlier insert), naming
;; no member, or naming an earlier member that is not an insert. And a few
;; admitted shapes outside the two kinds, (expect) among them.
;;
;; WHY A FINITE GENERATOR COVERS AN UNBOUNDED DOMAIN. Whether a place is a
;; reference depends only on: the kind; whether the intent is reached
;; through exactly one wrapper; and the place's index against the two
;; reference positions, start and start + 1 (start 1 for an insert, 2 for
;; a move). Every part after start + 1, and every wrapper part other than
;; the intent, is a non-reference whatever its index, and is read by
;; neither rule. So the relations that matter are an entry ending before
;; start, at start, at start + 1, and past it; 0 to 5 parts gives each of
;; them for both kinds, with parts past start + 1 present (a move of 5
;; parts has two). One extra wrapper part stands for any number, and a
;; dotted tail is the same whether it follows the parent or a later part.
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
  (case kind
    ((insert) (case p ((1) 'root) ((2) #f) ((3) section) (else 'extra)))
    ((move) (case p ((1) M) ((2) 'root) ((3) #f) (else 'extra)))
    (else (case p ((1) M) ((2) 'relates) ((3) M) (else 'extra)))))
;; A PLACE is (intent p), (wrapper q) for the outer wrapper's element q, or
;; (inner 1) for the subject of a wrapper inside the wrapper. MARKS is a
;; list of (place . value).
(define (at marks where other)
  (let ((m (assoc where marks))) (if m (cdr m) other)))
(define (intent kind n dotted marks)
  (let loop ((p n) (tail (if dotted 'tail '())))
    (if (= p 0)
        (cons kind tail)
        (loop (- p 1) (cons (at marks (list 'intent p) (filler kind p)) tail)))))
(define (entry kind n wrapper dotted marks)
  (let ((i (intent kind n dotted marks)))
    (case wrapper
      ((none) i)
      ((w3) (list 'expect (at marks '(wrapper 1) "0000") i))
      ((w4) (list 'expect (at marks '(wrapper 1) "0000") i (at marks '(wrapper 3) 'more)))
      ((wn) (list 'expect (at marks '(wrapper 1) "0000") (list 'expect (at marks '(inner 1) "0000") i))))))
(define (places n wrapper)
  (append (let loop ((p n) (out '())) (if (= p 0) out (loop (- p 1) (cons (list 'intent p) out))))
          (case wrapper
            ((none) '())
            ((w3) '((wrapper 1)))
            ((w4) '((wrapper 1) (wrapper 3)))
            ((wn) '((wrapper 1) (inner 1))))))
;; -> ((wrapper . dotted) ...): a bare entry ends properly.
(define shapes '((none . #f) (w3 . #f) (w3 . #t) (w4 . #f) (w4 . #t) (wn . #f) (wn . #t)))

;; THE POSITIONS RULE, from how the entry was made.
(define (ref-positions kind)
  (case kind ((insert) '(1 2)) ((move) '(2 3)) (else '(1 3))))
(define (reference-place? kind wrapper place)
  (and (not (eq? wrapper 'wn)) (eq? (car place) 'intent)
       (memv (cadr place) (ref-positions kind)) #t))

;; -> (class entry expected): the class from the reference places among
;; MARKS. A bad marker at a reference refuses, the parent's first; valid
;; ones become (from 0); markers elsewhere leave the entry as it is.
(define (judged kind n wrapper dotted marks)
  (let* ((e (entry kind n wrapper dotted marks))
         (refs (filter (lambda (m) (reference-place? kind wrapper (car m)))
                       (list-sort (lambda (a b) (< (cadr (car a)) (cadr (car b))))
                                  (filter (lambda (m) (eq? (car (car m)) 'intent)) marks))))
         (bad (find (lambda (m) (not (equal? (cdr m) valid))) refs)))
    (cond ((null? refs) (list 'no-marker-at-reference e e))
          (bad (list 'bad-at-reference e (list (list 'error 'no-such-intent (cadr (cdr bad))))))
          (else (list 'valid-at-reference e
                      (entry kind n wrapper dotted
                             (map (lambda (m) (if (memq m refs) (cons (car m) '(from 0)) m)) marks)))))))

(define cases
  (let ((out '()))
    (define (add! c) (set! out (cons c out)))
    (for-each
      (lambda (kind)
        (do ((n 0 (+ n 1))) ((> n 5))
          (for-each
            (lambda (shape)
              (let ((wrapper (car shape)) (dotted (cdr shape))
                    (s (car (ref-positions kind))) (s2 (cadr (ref-positions kind))))
                (add! (judged kind n wrapper dotted '()))
                (for-each
                  (lambda (place)
                    (for-each (lambda (m) (add! (judged kind n wrapper dotted (list (cons place m))))) markers))
                  (places n wrapper))
                ;; BOTH REFERENCE POSITIONS MARKED, when the entry has both.
                (when (>= n s2)
                  (for-each
                    (lambda (m1)
                      (for-each
                        (lambda (m2)
                          (add! (judged kind n wrapper dotted
                                        (list (cons (list 'intent s) m1) (cons (list 'intent s2) m2)))))
                        markers))
                    markers))))
            shapes)))
      '(insert move link unlink))
    ;; Admitted shapes outside the four kinds: no references at all.
    (add! (list 'no-marker-at-reference '(expect) '(expect)))
    (for-each
      (lambda (m)
        (for-each (lambda (e) (add! (list 'no-marker-at-reference e e)))
                  (list (list 'expect m) (list 'expect "0000" m) (list 'expect "0000" (list "insert" m))
                        (list 'expect "0000" 'insert m) (list 'frob m))))
      markers)
    (reverse out)))

;; THE SIZE OF THE PRODUCT, counted by arithmetic rather than by the
;; generator's loops. Per kind and n, each shape gives 1 + 3 places entries,
;; places being n for none, n + 1 for w3 (twice: proper and dotted), n + 2
;; for w4 and wn (twice each); so 7 + 3 (7n + 10) = 37 + 21n. Each shape
;; whose entry has both reference positions (n >= start + 1) gives 9 more:
;; 7 shapes x 9 x (n from 2 to 5 for an insert, 3 to 5 for a move, 4 + 3
;; values). And 1 + 5 x 3 others.
;; For each kind and each 0..5 parts: 37 + 21n entries over the seven
;; shapes, and 63 more when both reference positions are reached (an
;; insert from 2 parts, a move, a link and an unlink from 3).
(define product
  (+ (* 4 (let sum ((n 0) (acc 0)) (if (> n 5) acc (sum (+ n 1) (+ acc 37 (* 21 n))))))
     (* 7 9 (+ 4 3 3 3))
     1 (* 5 3)))

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
