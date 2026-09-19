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

;;; (theourgia markers) -- the marker grammar, and the byte work under it.
;;;
;;; THIS LIBRARY CANNOT REACH THE LANGUAGE TABLE, AND THAT IS ITS POINT.
;;;
;;; It is not dependency-free: `projection-control` reads a header, and a
;;; header is a stored datum, so `(theourgia wire)` comes with it. What
;;; matters is the other direction -- nothing here, and nothing this
;;; imports, can see `(theourgia languages)`. The gate asserts that over
;;; the transitive closure rather than over this file's import line,
;;; because an import line is a claim and a closure is the fact.
;;;
;;; The reduction must not know about languages: every hash and every
;;; `--based-on` is taken over it, so a rule that consults a table which
;;; is edited at runtime would make a stored block's acceptance depend on
;;; what the table said this minute. v129 moved the derived views out to
;;; `(theourgia view)` for that reason.
;;;
;;; That left the payload check. `datum-doc-marker?` has to recognise an
;;; `@file` / `@block` marker inside a doc string, and the grammar for
;;; that lived in `(theourgia code-markers)`, which imports
;;; `(theourgia languages)`. So the reduction still REACHED the table
;;; through two hops -- and although it no longer consulted it, that is
;;; a fact about today's code rather than a property of the arrangement:
;;; the next person to add a lookup inside those two hops would restore
;;; the dependency with nothing to notice.
;;;
;;; KEY: SO THE GRAMMAR MOVED DOWN HERE INSTEAD OF BEING FENCED OFF ABOVE.
;;; A gate that said "the reduction does not import the language table"
;;; while the table sat two hops away would be saying more than it can
;;; prove. With these definitions in a library that CANNOT reach it, the
;;; gate over the transitive closure says exactly what it proves.
;;;
;;; NOTHING WAS COPIED TO GET HERE. Each definition moved out of the file
;;; that held it -- the byte helpers from `text-code.sc`, the alist
;;; accessor from `languages.sc`, the grammar from `code-markers.sc` --
;;; and those three now import them back and re-export them under the
;;; same names, so not one of their callers changed.
;;;
;;; `language-property` IS NOT A TABLE LOOKUP. It reads a field out of an
;;; entry it is handed. Which entry that is -- one from the table, or the
;;; fixed doc wrapping in `datum-metadata.sc` -- is the caller's business,
;;; and that is why it can live below the table rather than beside it.

(library (theourgia markers)
  ;; THE CLOSURE MOVED, NOT A SELECTION FROM IT. `projection-control`
  ;; reads a header, which reads hex and checks an id, which raise a
  ;; projection failure -- taking only the five names the ruling listed
  ;; left `header-read` and `block-read` behind and the library would not
  ;; load. A move is finished when the thing moved has nothing left
  ;; pointing back.
  (export byte-slice bytes-append byte-lines safe-utf8 string-prefix-at?
          language-property projection-failure hex-decode safe-id?
          header-read block-read
          wrapping marker-line family projection-header-wrapper? projection-control)
  (import (rnrs) (only (theourgia wire) storable-decode string->sexpr-extended))

  (define (byte-slice b from to)
    (let ((out (make-bytevector (- to from)))) (bytevector-copy! b from out 0 (- to from)) out))
  (define (bytes-append . parts)
    (let ((out (make-bytevector (apply + (map bytevector-length parts)))))
      (let loop ((xs parts) (offset 0))
        (if (null? xs) out
            (let ((n (bytevector-length (car xs))))
              (bytevector-copy! (car xs) 0 out offset n) (loop (cdr xs) (+ offset n)))))))
  (define (byte-lines b)
    (let ((n (bytevector-length b)))
      (let loop ((start 0) (i 0) (out '()))
        (cond ((= i n) (reverse (if (< start n) (cons (list start n n) out) out)))
              ((= 10 (bytevector-u8-ref b i))
               (loop (+ i 1) (+ i 1) (cons (list start (if (and (> i start) (= 13 (bytevector-u8-ref b (- i 1)))) (- i 1) i) (+ i 1)) out)))
              (else (loop start (+ i 1) out))))))
  (define (safe-utf8 b)
    (guard (e (#t #f)) (let ((s (utf8->string b))) (and (equal? (string->utf8 s) b) s))))
  (define (string-prefix-at? text prefix start)
    (and (<= (+ start (string-length prefix)) (string-length text))
         (string=? prefix (substring text start (+ start (string-length prefix))))))
  (define (language-property entry name default)
    (let ([p (and entry (assq name entry))])
      (if p (cadr p) default)))
  (define (wrapping entry)
    (let ((line (language-property entry 'line-comment #f))
          (block (language-property entry 'block-comment #f)))
      (cond (line (cons (string-append line " ") ""))
            (block (cons (string-append (car block) " ") (string-append " " (cadr block))))
            (else (cons "# " "")))))
  (define (marker-line entry body)
    (let ((w (wrapping entry))) (string->utf8 (string-append (car w) body (cdr w) "\n"))))
  ;; Family recognition is exact at both ends. All positive @ counts escape.
  ;; The payload grammar is checked only for a single-@ control line.
  (define (family entry bytes)
    (let* ((s (safe-utf8 bytes)) (w (wrapping entry))
           (start (string-length (car w))) (suffix (string-length (cdr w))))
      (and s (string-prefix-at? s (car w) 0)
           (>= (string-length s) (+ start suffix))
           (string-prefix-at? s (cdr w) (- (string-length s) suffix))
           (let ((end (- (string-length s) suffix)))
             (let loop ((i start))
               (if (and (< i end) (char=? (string-ref s i) #\@)) (loop (+ i 1))
                   (and (> i start)
                        (or (string-prefix-at? s "block " i) (string-prefix-at? s "file " i))
                        (list (- i start) (substring s i end) start))))))))
  (define (projection-header-wrapper? entry bytes)
    (exists (lambda (row)
              (let ((f (family entry (byte-slice bytes (car row) (cadr row)))))
                (and f (= (car f) 1) (string-prefix-at? (cadr f) "file " 0))))
            (byte-lines bytes)))
  (define (projection-control entry content)
    (let ((f (family entry content)))
      (and f (= (car f) 1)
           (if (string-prefix-at? (cadr f) "file " 0)
               (list 'file (header-read (substring (cadr f) 5 (string-length (cadr f)))))
               (cons 'block (block-read (substring (cadr f) 6 (string-length (cadr f)))))))))
  (define (projection-failure reason . details)
    (raise (append (list 'error 'projection-invalid (list 'reason reason)) details)))
  (define (hex-decode s)
    (unless (and (<= (string-length s) 65536) (even? (string-length s)))
      (projection-failure 'invalid-header))
    (let ((b (make-bytevector (div (string-length s) 2))))
      (define (digit c)
        (cond ((char<=? #\0 c #\9) (- (char->integer c) 48))
              ((char<=? #\a c #\f) (+ 10 (- (char->integer c) 97)))
              (else (projection-failure 'invalid-header))))
      (do ((i 0 (+ i 2))) ((= i (string-length s)) b)
        (bytevector-u8-set! b (div i 2) (+ (* 16 (digit (string-ref s i))) (digit (string-ref s (+ i 1))))))))
  (define (safe-id? s)
    (and (> (string-length s) 0) (<= (string-length s) 256)
         (for-all (lambda (c) (or (char<=? #\a c #\z) (char<=? #\0 c #\9)
                                  (memv c '(#\. #\- #\_)))) (string->list s))
         (not (member s '("." "..")))))
  (define (header-read s)
    (guard (e (#t (projection-failure 'invalid-header)))
      (let ((h (storable-decode (string->sexpr-extended (utf8->string (hex-decode s))))))
        (unless (and (list? h) (= 7 (length h)) (eq? (car h) 'code-projection)
                     (equal? (cadr h) 1) (string? (caddr h)) (string? (cadddr h))
                     (list? (list-ref h 4)) (memq (list-ref h 5) '(text datum))
                     (memv (list-ref h 6) '(0 1))
                     (for-all (lambda (p) (and (pair? p) (string? (car p))
                                              (integer? (cdr p)) (exact? (cdr p)) (>= (cdr p) 0))) (list-ref h 4)))
          (projection-failure 'invalid-header)) h)))
  (define (block-read s)
    (let* ((n (string-length s))
           (space (let loop ((i 0)) (and (< i n) (if (char=? (string-ref s i) #\space) i (loop (+ i 1))))))
           (id (if space (substring s 0 space) s))
           (tail (and space (substring s space n))))
      (unless (and (safe-id? id) (or (not tail) (member tail '(" pad 0" " pad 1"))))
        (projection-failure 'invalid-marker))
      (list id (if (equal? tail " pad 1") 1 0))))
)
