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

;; THE DERIVED FIELDS STILL ARRIVE, BY THE ROUTE A CALLER USES.
;;
;; `view-fields.sc` asks `view-read` directly. That is the layer the
;; split created, and a tree in which every caller had been left on
;; `state-read` would pass every row of it: the derivation would work
;; perfectly and nobody would see it.
;;
;; SO THESE ROWS GO THROUGH `rpc-dispatch`, and through the three verbs
;; that show a name to a person -- `outline`, `read`, and `read
;; --recursive`. NEVER: Not through `code-field`, which is a fourth caller
;; and would only say that ONE of them was rewired.
;;
;; AND EACH NAME IS READ TWICE, BEFORE AND AFTER THE SOURCE CHANGES. A
;; row that only checked that `alpha` appears is also satisfied by a
;; stored label that happens to say `alpha`; the rows that matter are
;; the ones where the source is edited and the label follows, because
;; only a derivation can do that.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia rpc) (theourgia ffi))

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/view-rpc-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define input (string-append root "/input"))
(mkdir-p! input)
(rpc-dispatch store '(init) "t")

(define (write! path text)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 text)))))

(write! (string-append input "/m.py") "# module doc\ndef alpha(x):\n    return x\n")
(want "VR-00 the Python file imports" (car (rpc-dispatch store (list 'import-code input) "t")) 'ok)

(define (ids) (map cadr (state-datum (open-and-reduce store))))
(define (kind-of id)
  (let ((b (state-read (open-and-reduce store) id)))
    (and b (cdr (assq 'kind (cdr (assq 'fields b)))))))
(define file-id (car (filter (lambda (id) (eq? 'file (kind-of id))) (ids))))
(define code-id (car (filter (lambda (id) (eq? 'code (kind-of id))) (ids))))

;; THE ANSWER IS ASKED FOR ONE FIELD, so a row names the field it is
;; about rather than comparing a whole record that also carries the
;; block's bytes, its position and its edges.
(define (rpc-field id key . args)
  (let ((answer (apply rpc-dispatch store (append (list 'read id) args) (list "t"))))
    (and (pair? answer) (eq? 'ok (car answer))
         (let* ((record (cadr answer))
                (fs (assq 'fields record))
                (p (and fs (assq key (cdr fs)))))
           (and p (cdr p))))))

(define (outline-text)
  (let ((answer (rpc-dispatch store '(outline) "t")))
    (and (pair? answer) (eq? 'ok (car answer)) (cadr (cadr answer)))))

(define (contains? text needle)
  (let ((n (string-length text)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? needle (substring text i (+ i m))) #t)
            (else (loop (+ i 1)))))))

(want "VR-01 read gives the name derived from the source"
      (rpc-field code-id 'name) "alpha")
(want "VR-02 outline shows that name too"
      (contains? (outline-text) "alpha") #t)

;; `read --recursive` ON THE FILE BLOCK reaches the child, which is a
;; different assembly path from `read` on the child itself.
(define (recursive-text)
  (let ((answer (rpc-dispatch store (list 'read file-id "--recursive") "t")))
    (call-with-string-output-port (lambda (p) (write answer p)))))

(want "VR-03 read --recursive carries the child's derived name"
      (contains? (recursive-text) "alpha") #t)

;; NOW THE SOURCE CHANGES AND EVERY LABEL HAS TO FOLLOW.
(want "VR-04 the source edit is accepted"
      (car (rpc-dispatch store (list 'set code-id "src" "# module doc\ndef beta(x):\n    return x\n") "t"))
      'ok)

(want "VR-05 read follows the source"
      (rpc-field code-id 'name) "beta")
(want "VR-05 outline follows the source"
      (list (contains? (outline-text) "beta") (contains? (outline-text) "alpha"))
      '(#t #f))
(want "VR-05 read --recursive follows the source"
      (list (contains? (recursive-text) "beta") (contains? (recursive-text) "alpha"))
      '(#t #f))

;; THE DATUM SIDE, WHICH DERIVES ITS NAME FROM A BODY RATHER THAN FROM
;; SOURCE TEXT -- a different branch of `view-read`.
(with-store-write store
  (lambda (s v)
    (list (list 'insert 'root #f
                (list '(kind . code) '(mode . datum) '(body define gamma 1)))))
  "t")
(define datum-id
  (car (filter (lambda (id) (and (not (equal? id file-id)) (not (equal? id code-id))))
               (ids))))

(want "VR-06 read gives a datum block's derived name"
      (rpc-field datum-id 'name) 'gamma)
(want "VR-06 and the whole list of names it binds"
      (rpc-field datum-id 'names) '(gamma))
(want "VR-07 outline shows the datum name"
      (contains? (outline-text) "gamma") #t)

(want "VR-08 the body edit is accepted"
      (caar (with-store-write store
              (lambda (s v) (list (list 'set datum-id 'body (list 'define 'delta 1))))
              "t"))
      'ok)
(want "VR-09 read follows the body"
      (rpc-field datum-id 'name) 'delta)
(want "VR-09 outline follows the body"
      (list (contains? (outline-text) "delta") (contains? (outline-text) "gamma"))
      '(#t #f))

;; ---- the version a read hands back ------------------------------------
;;
;; NEVER: THE VERSION IS THE TOKEN `--if-unchanged` COMPARES, not a hash of
;; what the read printed. So the rows compare it with `block-hash` of the
;; state and then spend it: a write carrying the version a read gave is
;; accepted, and one carrying the version from before that write is refused.
(define (plain-read id)
  (rpc-dispatch store (list 'read id) "t"))
;; The version clause of an answer, or #f. Rows that build on a version
;; read it through this, so a tree that gives none reads red row by row
;; instead of stopping the file.
(define (version-of a)
  (and (list? a) (>= (length a) 3) (pair? (caddr a)) (eq? 'version (car (caddr a)))
       (pair? (cdr (caddr a))) (cadr (caddr a))))

(let ((a (plain-read code-id)))
  (want "VR-10 a plain read answers the record, then (version <block-hash>) as the third element, then the receipt's (cut ...) and (versions ...)"
        (list (length a) (cdr (assq 'id (cadr a))) (car (caddr a))
              (equal? (cadr (caddr a)) (block-hash (open-and-reduce store) code-id))
              (map car (cdddr a)))
        (list 5 code-id 'version #t '(cut versions)))
  (want "VR-10 the version is a string of 64 hex digits"
        (let ((v (cadr (caddr a))))
          (and (string? v) (= 64 (string-length v))
               (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list v))))
        #t))

(let* ((before (or (version-of (plain-read code-id)) "none"))
       (set-a (rpc-dispatch store (list 'set code-id "src" "# module doc\ndef beta(x):\n    return x + 0\n"
                                        "--if-unchanged" before) "t"))
       (after (or (version-of (plain-read code-id)) "none"))
       (after-is-current (equal? after (block-hash (open-and-reduce store) code-id)))
       (stale (rpc-dispatch store (list 'set code-id "src" "# module doc\ndef beta(x):\n    return x + 1\n"
                                        "--if-unchanged" before) "t"))
       (fresh (rpc-dispatch store (list 'set code-id "src" "# module doc\ndef beta(x):\n    return x + 2\n"
                                        "--if-unchanged" after) "t")))
  (want "VR-11 the version a read gave is accepted by --if-unchanged, and changes after that write"
        (list (car set-a) (equal? before after) after-is-current)
        '(ok #f #t))
  (want "VR-11 the version from before the write is refused changed, and the one after it is accepted"
        (list (and (pair? stale) (car stale)) (and (pair? stale) (pair? (cdr stale)) (cadr stale)) (car fresh))
        '(error changed ok)))

(let ((a (rpc-dispatch store (list 'read file-id "--recursive") "t")))
  (want "VR-12 read --recursive keeps its items, each the record a plain read of that block gives, then its (versions ...) clause, then the receipt's (cut ...)"
        (list (length a) (car (cadr a)) (car (caddr a))
              (for-all (lambda (r) (equal? r (cadr (plain-read (cdr (assq 'id r)))))) (cdr (cadr a)))
              (car (cadddr a)))
        '(4 items versions #t cut))
  (want "VR-12 one pair per item, in item order, each the version a plain read of that block gives"
        (let ((pairs (cadr (caddr a)))
              (item-ids (map (lambda (r) (cdr (assq 'id r))) (cdr (cadr a)))))
          (list (equal? (map car pairs) item-ids) (> (length pairs) 1)
                (for-all (lambda (p) (equal? (cdr p) (version-of (plain-read (car p))))) pairs)))
        '(#t #t #t)))

(want "VR-13 del is accepted" (car (rpc-dispatch store (list 'del datum-id) "t")) 'ok)
(let ((a (plain-read datum-id)))
  (want "VR-13 a deleted block is read as before: the record, with no version, then the receipt, whose versions leave it out"
        (list (car a) (length a) (cdr (assq 'deleted (cadr a))) (map car (cddr a)) (cadr (cadddr a)))
        '(ok 4 #t (cut versions) ())))
(let ((a (plain-read "nosuch.1")))
  (want "VR-14 an absent id is still refused, with no version clause"
        (list (car a) (and (list? a) (assq 'version (filter pair? (cdr a)))))
        '(error #f)))

;; THE BAND WHERE A BLOCK IS WRITTEN AND CANNOT BE HASHED (nesting-depth.sc
;; ND-02: 59 levels). It was read before there was a version, and it still
;; is; the version says it is unavailable and why, as a write's state
;; section does, rather than the whole answer becoming an internal error.
(define (nested-vector n)
  (let loop ((i 0) (v 0)) (if (= i n) v (loop (+ i 1) (vector v)))))
(define deep-id
  (guard (e (#t #f))
   (let ((before (ids)))
    (with-store-write store
      (lambda (s v)
        (list (list 'insert 'root #f (list '(kind . section) (cons 'depth-probe (nested-vector 59))))))
      "t")
    (let ((new (filter (lambda (id) (not (member id before))) (ids))))
      (and (= 1 (length new)) (car new))))))
(let ((a (plain-read (or deep-id "nosuch.2"))))
  (want "VR-15 a block too deep to hash is read as before, with (version unavailable (reason ...))"
        (list (car a) (length a) (cdr (assq 'id (cadr a))) (car (caddr a)) (cadr (caddr a))
              (car (caddr (caddr a))) (map car (cdddr a)))
        (list 'ok 5 deep-id 'version 'unavailable 'reason '(cut versions))))

(printf "rows: ~a\n~a failures\nview-rpc complete\n" rows bad)
