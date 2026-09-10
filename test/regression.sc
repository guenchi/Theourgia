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
;; The defects the review named, each with a case that FAILED before the
;; fix and a control that must keep passing.
(import (chezscheme) (theourgia wire) (theourgia ffi) (theourgia crc32))
(define bad 0)
(define (want label got expect)
  (unless (equal? got expect)
    (set! bad (+ bad 1))
    (printf "FAIL ~a: got ~s want ~s\n" label got expect))
  (printf "~a ~a -> ~s\n" (if (equal? got expect) "ok  " "FAIL") label got))
(define (raises? thunk) (guard (e (#t #t)) (thunk) #f))

(printf "== ffi: duplicate flags must not become another flag ==\n")
(define dir "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/regwork")
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define log (string-append dir "/l"))
(let ((fd (fd-open log '(write create append)))) (write-all! fd (string->utf8 "AAA")) (fd-close fd))
(let ((fd (fd-open log '(write append append)))) (write-all! fd (string->utf8 "BBB")) (fd-close fd))
(want "append twice still appends (was: overwrote at offset 0)" (file-size log) 6)

(printf "== ffi: an exception from the body keeps its own type, and frees the lock ==\n")
;; A re-entry guard was tried here and removed: R6RS guard re-raises in
;; the dynamic environment of the original raise, so a clause that
;; DECLINES re-enters every dynamic-wind it unwound -- and the guard
;; then replaced the real error with an assertion violation. This row is
;; what that removal has to keep true.
(define lock (string-append dir "/lock"))
(file-ensure! lock)
(want "a declining inner guard does not change the error"
      (guard (e (#t (and (vector? e) (vector-ref e 0))))
        (guard (e ((and (vector? e) (eq? (vector-ref e 0) 'never-matches)) 'inner))
          (with-exclusive-lock lock
            (lambda (fd) (raise (vector 'durable-error 'write (cons "/x" 5)))))))
      'durable-error)
(want "the lock is free afterwards"
      (with-exclusive-lock lock (lambda (fd) 'fine)) 'fine)

(printf "== wire: a line is one line ==\n")
;; The CRC is COMPUTED over the injected bytes, never typed in: a
;; hand-written checksum would make every one of these read (bad-crc)
;; and the row would pass for the wrong reason.
(define (framed raw)
  (let* ((hex (crc32-hex raw))
         (out (make-bytevector (+ 9 (bytevector-length raw) 1))))
    (bytevector-copy! (string->utf8 hex) 0 out 0 8)
    (bytevector-u8-set! out 8 32)
    (bytevector-copy! raw 0 out 9 (bytevector-length raw))
    (bytevector-u8-set! out (+ 9 (bytevector-length raw)) 10)
    out))
(want "an interior newline is a frame error"
      (decode-line (framed (string->utf8 "(0 0 \"a\" () ())\n(1 2 \"b\" () ())")))
      '(frame-error embedded-newline))
(want "invalid UTF-8 with a correct CRC is a frame error"
      (decode-line (framed (u8-list->bytevector
        (append (bytevector->u8-list (string->utf8 "(0 0 \"")) (list 255)
                (bytevector->u8-list (string->utf8 "\" () ())"))))))
      '(frame-error encoding))
(want "a leading byte-order mark is a frame error"
      (decode-line (framed (u8-list->bytevector
        (append (list 239 187 191)
                (bytevector->u8-list (string->utf8 "(0 0 \"a\" () ())"))))))
      '(frame-error encoding))
(want "a good line still decodes"
      (decode-line (encode-record 1 2 "w" '() '(a))) '(ok 1 2 "w" () (a)))

(printf "== wire: an uninterned symbol is refused, an interned one is not ==\n")
(want "gensym refused" (raises? (lambda () (storable-encode (gensym "foo")))) #t)
(want "interned symbol fine" (storable-decode (storable-encode 'foo)) 'foo)
(want "unsafe interned symbol still encodes"
      (storable-decode (storable-encode (string->symbol "weird sym")))
      (string->symbol "weird sym"))

(printf "== wire: a circular spine raises instead of hanging ==\n")
(want "circular list refused"
      (raises? (lambda () (storable-encode (let ((x (list 'a 'b))) (set-cdr! (cdr x) x) x))))
      #t)
(want "long proper list still fine"
      (length (storable-decode (storable-encode (make-list 5000 #\a)))) 5000)

(printf "== crc32: a stray third argument is refused ==\n")
(want "three range arguments refused"
      (raises? (lambda () (crc32-hex (string->utf8 "a") 0 1 'oops)))
      #t)

(printf "\n~a failures\n" bad)

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "regression complete\n")
