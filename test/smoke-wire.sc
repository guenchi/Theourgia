#!chezscheme
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
(import (chezscheme) (theourgia wire) (theourgia crc32))

(define fails 0)
(define (check label got want)
  (unless (equal? got want)
    (set! fails (+ fails 1))
    (printf "MISMATCH ~a\n  got  ~s\n  want ~s\n" label got want)))

(printf "== storage encoding round trip ==\n")
(define cases
  (list
    'plain-symbol
    "a string"
    42 -7 3/4 #t #f '()
    #\a #\newline #\x1F600
    (string->symbol "12")
    (string->symbol "weird sym")
    (string->symbol "#%char")
    (string->symbol "")
    '(1 2 . 3)
    '(a "b" #\c (d . #\e))
    '(d . #\e)
    '(d "#%char" 101)
    '(1 2 . #\a)
    (cons #\a #\b)
    (cons 'x (string->symbol "weird sym"))
    '((kind . section) (ord . 1))
    (list "#%dot" '(d) '("#%char" 101))
    (list "#%char" 97)
    (list "#%sym" "x")
    (list "#%quote" 5)
    (list "#%quote" (list "#%quote" 5))
    (list "#%unknown" 1)
    (cons "#%char" 5)
    (vector 1 #\b 'c (list "#%char" 1))
    (list (vector #\z) (list (list "#%quote" #\y)))
    '(define (opt alist key default) (if (assq key alist) (cdr (assq key alist)) default))))

(for-each
 (lambda (x)
   (let ((e (storable-encode x)))
     (check (list 'roundtrip x) (storable-decode e) x)
     ;; whatever came out must actually be writable
     (guard (exn (#t (set! fails (+ fails 1))
                     (printf "NOT WRITABLE ~s -> ~s\n" x e)))
       (sexpr->string-extended e))))
 cases)
(printf "encoded forms:\n")
(for-each (lambda (x) (printf "  ~s\n    -> ~s\n" x (storable-encode x)))
          (list #\a (string->symbol "weird sym") (list "#%char" 97)
                (list "#%quote" 5) (cons "#%char" 5)))

(printf "== determinism ==\n")
(check 'twice
       (encode-record 412 1757300000123 "agent:claude" '() '(put "k.a7f2" ((kind . section))))
       (encode-record 412 1757300000123 "agent:claude" '() '(put "k.a7f2" ((kind . section)))))

(printf "== record line shape ==\n")
(define line (encode-record 412 1757300000123 "agent:claude" '(("c9xq01mz" . 88))
                            '(put "k3m9x2qa.a7f2" ((kind . section) (ord . 1)))))
(printf "~s\n" (utf8->string line))
(check 'decode (decode-line line)
       '(ok 412 1757300000123 "agent:claude" (("c9xq01mz" . 88))
            (put "k3m9x2qa.a7f2" ((kind . section) (ord . 1)))))

(printf "== the four outcomes ==\n")
(define (bytes->list bv) (bytevector->u8-list bv))
(define (list->bytes l) (u8-list->bytevector l))
(define (drop-last bv) (list->bytes (reverse (cdr (reverse (bytes->list bv))))))
(define (with-byte bv i b)
  (let ((c (bytevector-copy bv))) (bytevector-u8-set! c i b) c))
(define (insert-before-newline bv s)
  (let ((l (bytes->list bv)))
    (list->bytes (append (reverse (cdr (reverse l)))
                         (bytevector->u8-list (string->utf8 s))
                         (list 10)))))

(check 'torn-no-newline (decode-line (drop-last line)) '(torn))
(check 'torn-empty (decode-line (make-bytevector 0)) '(torn))
;; flip one byte of the payload text, keep the old CRC
(check 'bad-crc-text (decode-line (with-byte line 30 (if (= 65 (bytevector-u8-ref line 30)) 66 65)))
       '(bad-crc))
;; flip one hex digit of the CRC field
(check 'bad-crc-field
       (decode-line (with-byte line 0 (if (= 97 (bytevector-u8-ref line 0)) 98 97)))
       '(bad-crc))
;; uppercase hex is not what the writer emits
(check 'bad-crc-uppercase
       (decode-line (with-byte line 0 65))
       '(bad-crc))
;; datum then a space, CRC recomputed over the new text
(define padded
  (let* ((l (bytes->list line))
         (text (list->bytes (list-tail l 9)))
         (t (utf8->string (drop-last text)))
         (t2 (string-append t " ")))
    (string->utf8 (string-append (crc32-string-hex t2) " " t2 "\n"))))
(check 'frame-padding (decode-line padded) '(frame-error padding))
;; two datums on one line, CRC recomputed
(define two
  (let* ((t2 "(1 2 \"a\" () (put \"x\" ())) (9)"))
    (string->utf8 (string-append (crc32-string-hex t2) " " t2 "\n"))))
(check 'frame-two-datums (decode-line two) '(frame-error parse))
;; four elements instead of five
(define four
  (let ((t2 "(1 2 \"a\" ())"))
    (string->utf8 (string-append (crc32-string-hex t2) " " t2 "\n"))))
(check 'frame-shape (decode-line four) '(frame-error shape))

(printf "== newline inside a string survives the frame ==\n")
(define nl-line (encode-record 1 2 "who" '() (list 'set "id" 'title "a\nb")))
(printf "bytes: ~s\n" (utf8->string nl-line))
(check 'one-newline
       (length (filter (lambda (b) (= b 10)) (bytes->list nl-line))) 1)
(check 'nl-roundtrip (decode-line nl-line) '(ok 1 2 "who" () (set "id" title "a\nb")))

(printf "== actor as the five element list ==\n")
(define al (encode-record 5 6 (list "agent:claude" "req-1" 0 "fp" '("w" . 3)) '() '(del "x")))
(check 'actor-list (decode-line al)
       '(ok 5 6 ("agent:claude" "req-1" 0 "fp" ("w" . 3)) () (del "x")))

(printf "== payload with an encoded character survives the line ==\n")
(define enc (storable-encode (list 'set "id" 'ch #\x1F600)))
(define cl (encode-record 7 8 "w" '() enc))
(check 'char-payload
       (let ((r (decode-line cl)))
         (and (eq? (car r) 'ok) (storable-decode (list-ref r 5))))
       (list 'set "id" 'ch #\x1F600))

(printf "== validation refuses malformed records ==\n")
(define (refuses? thunk)
  (guard (e (#t #t)) (thunk) #f))
(check 'bad-seq (refuses? (lambda () (encode-record -1 2 "w" '() '(a)))) #t)
(check 'bad-actor (refuses? (lambda () (encode-record 1 2 '(1 2) '() '(a)))) #t)
(check 'bad-deps (refuses? (lambda () (encode-record 1 2 "w" '(("a" . -1)) '(a)))) #t)
(check 'good-still-works (bytevector? (encode-record 1 2 "w" '() '(a))) #t)

(printf "\n~a mismatches\n" fails)

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "smoke-wire complete\n")
