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
(library (theourgia code-markers)
  (export projection-encode projection-decode marker-line projection-failure projection-header-wrapper?
          projection-control projection-header-line)
  ;; The marker grammar moved to (theourgia markers), which
  ;; CANNOT REACH THIS LIBRARY -- it imports (rnrs) and two names from
  ;; (theourgia wire), and nothing else. That, not "it imports nothing",
  ;; is the property the move was made for, and `test/closures.sc` reads
  ;; it off the import graph. The names come back here unchanged, so no
  ;; caller of this library changed. See markers.sc for why.
  (import (only (theourgia markers) wrapping marker-line family projection-header-wrapper?
                projection-control projection-failure hex-decode safe-id? header-read block-read)
          (rnrs) (theourgia text-code) (theourgia languages) (theourgia wire)
          (only (theourgia digest) bytevector->hex))






  (define (adjust-line entry b delta)
    (let ((f (family entry b)))
      (if (not f) b
          (let ((at (list-ref f 2)))
            (if (= delta 1)
                (bytes-append (byte-slice b 0 at) #vu8(64) (byte-slice b at (bytevector-length b)))
                (bytes-append (byte-slice b 0 at) (byte-slice b (+ at 1) (bytevector-length b))))))))
  (define (escape-body entry b)
    (apply bytes-append
      (map (lambda (r)
             (bytes-append (adjust-line entry (byte-slice b (car r) (cadr r)) 1)
                           (byte-slice b (cadr r) (caddr r)))) (byte-lines b))))
  (define (ends-lf? b)
    (and (> (bytevector-length b) 0) (= 10 (bytevector-u8-ref b (- (bytevector-length b) 1)))))
  (define (padding b) (if (or (zero? (bytevector-length b)) (ends-lf? b)) 0 1))
  (define (unpad b pad)
    (case pad ((0) b)
      ((1) (unless (ends-lf? b) (projection-failure 'invalid-padding))
           (byte-slice b 0 (- (bytevector-length b) 1)))
      (else (projection-failure 'invalid-padding))))






  (define (projection-header-line entry header mode pad)
    (let ((hex (bytevector->hex (string->utf8 (sexpr->string-extended
                    (storable-encode (append '(code-projection 1) header (list mode pad))))))))
      (when (> (string-length hex) 65536) (projection-failure 'header-limit))
      (marker-line entry (string-append "@file " hex))))

  ;; Header = (store file cut), entries = ((id-or-new raw-bytes) ...).
  ;; Framing LF belongs to the following marker and is never source text.
  (define (projection-encode entry header entries)
    (let* ((first (if (pair? entries) (cadar entries) #vu8()))
           (prefix-size (source-prefix-size first))
           (prefix (byte-slice first 0 prefix-size))
           (padded (if (= 1 (padding prefix)) (bytes-append prefix #vu8(10)) prefix))
           (head (and header (append '(code-projection 1) header (list 'text (padding prefix)))))
           (initial (if head
                        (bytes-append padded (marker-line entry
                          (string-append "@file " (bytevector->hex (string->utf8
                            (sexpr->string-extended (storable-encode head)))))))
                        padded)))
      (let loop ((xs entries) (first? #t) (previous initial) (out '()))
        (if (null? xs) (apply bytes-append (reverse (cons previous out)))
            (let* ((pad (if (and first? (not header)) (padding prefix) (padding previous)))
                   (body (cadar xs))
                   (body (if first? (byte-slice body prefix-size (bytevector-length body)) body))
                   (mark (marker-line entry (string-append "@block " (caar xs) " pad " (number->string pad))))
                   (part (if (and (= pad 1) (not first?)) (bytes-append previous #vu8(10)) previous)))
              (loop (cdr xs) #f (escape-body entry body) (cons mark (cons part out))))))))

  (define (projection-decode entry bytes)
    (let* ((rows (byte-lines bytes))
           (controlled? (exists (lambda (r) (let ((f (family entry (byte-slice bytes (car r) (cadr r)))))
                                              (and f (= 1 (car f))))) rows)))
      (if (not controlled?) (list #f (list (list "new" bytes)))
          (let loop ((rs rows) (header #f) (current #f) (parts '()) (out '()) (seen? #f))
            (if (null? rs)
                (let ((body (apply bytes-append (reverse parts))))
                  (list header (reverse (if current (cons (list current body) out)
                                            (if (> (bytevector-length body) 0) (cons (list "new" body) out) out)))))
                (let* ((r (car rs)) (content (byte-slice bytes (car r) (cadr r)))
                       (eol (byte-slice bytes (cadr r) (caddr r)))
                       (raw (byte-slice bytes (car r) (caddr r))) (f (family entry content)))
                  (cond
                    ((not f) (loop (cdr rs) header current (cons raw parts) out seen?))
                    ((> (car f) 1) (loop (cdr rs) header current
                                        (cons (bytes-append (adjust-line entry content -1) eol) parts) out seen?))
                    ((string-prefix-at? (cadr f) "file " 0)
                     (when (or header current seen?) (projection-failure 'duplicate-header))
                     (let* ((h (header-read (substring (cadr f) 5 (string-length (cadr f)))))
                            (prefix (unpad (apply bytes-append (reverse parts)) (list-ref h 6))))
                       (unless (= (bytevector-length prefix) (source-prefix-size prefix))
                         (projection-failure 'misplaced-header))
                       (loop (cdr rs) h #f (list prefix) out #t)))
                    (else
                     (let* ((mark (block-read (substring (cadr f) 6 (string-length (cadr f)))))
                            (body (unpad (apply bytes-append (reverse parts)) (cadr mark)))
                            (prefix? (and (not current) (or header (= (bytevector-length body) (source-prefix-size body))))))
                       (loop (cdr rs) header (car mark) (if prefix? (list body) '())
                             (if current (cons (list current body) out)
                                 (if (and (not prefix?) (> (bytevector-length body) 0)) (cons (list "new" body) out) out)) #t))))))))))
)
