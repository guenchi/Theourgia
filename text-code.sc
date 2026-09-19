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
(library (theourgia text-code)
  (export byte-lines byte-slice bytes-append text-properties definition-name
          safe-utf8 string-prefix-at? trim-left comment-prefixes source-prefix-size)
  ;; The byte helpers moved to (theourgia markers), which
  ;; CANNOT REACH THIS LIBRARY -- it imports (rnrs) and two names from
  ;; (theourgia wire), and nothing else. That, not "it imports nothing",
  ;; is the property the move was made for, and `test/closures.ss` reads
  ;; it off the import graph. The names come back here unchanged, so no
  ;; caller of this library changed. See markers.ss for why.
  (import (only (theourgia markers) byte-slice bytes-append byte-lines safe-utf8 string-prefix-at?)
          (rnrs) (theourgia languages) (theourgia regex))
  (define patterns (make-hashtable string-hash string=?))





  (define (trim-left s)
    (let loop ((i 0))
      (if (and (< i (string-length s)) (memv (string-ref s i) '(#\space #\tab #\page)))
          (loop (+ i 1)) (substring s i (string-length s)))))
  (define (comment-prefixes entry)
    (language-property entry 'comment-prefixes
      (let ((p (language-property entry 'line-comment #f))) (if p (list p) '()))))
  (define (definition-name entry text)
    (and entry
      (let loop ((ps (language-property entry 'def-heads '())))
        (and (pair? ps)
          (or (guard (e (#t #f))
                (let* ((pattern (or (hashtable-ref patterns (car ps) #f)
                                    (let ((ast (regex-compile (car ps))))
                                      (hashtable-set! patterns (car ps) ast) ast)))
                       (m (regex-match pattern text 'spans))
                       (span (and m (assv (language-property entry 'name-capture 1) m))))
                  (and span (> (cddr span) (cadr span))
                       ;; An ASCII pattern must not report the ASCII prefix of
                       ;; a longer Unicode identifier as a complete name.
                       (or (= (cddr span) (string-length text))
                           (not (let ((next (string-ref text (cddr span))))
                                  (and (> (char->integer next) 127)
                                       (or (char-alphabetic? next) (char-numeric? next))))))
                       (substring text (cadr span) (cddr span)))))
              (loop (cdr ps)))))))

  ;; A BOM and position-sensitive first/second lines stay before the header.
  ;; Detection is textual and never invokes a Scheme reader.
  (define (source-prefix-size b)
    (let* ((bom (if (and (>= (bytevector-length b) 3)
                         (equal? (byte-slice b 0 3) #vu8(239 187 191))) 3 0))
           (body (byte-slice b bom (bytevector-length b)))
           (lines (byte-lines body)))
      (let loop ((ls lines) (line 0) (end bom))
        (if (or (null? ls) (= line 2)) end
            (let* ((row (car ls)) (s (safe-utf8 (byte-slice body (car row) (cadr row))))
                   (directive (and s (= line 0) (string-prefix-at? s "#!" 0)))
                   (cookie (and s (guard (e (#t #f))
                                    (regex-match (regex-compile "^[ \\t]*#.*coding[:=][ \\t]*[-_.A-Za-z0-9]+") s)))))
              (if (or directive cookie)
                  (loop (cdr ls) (+ line 1) (+ bom (caddr row)))
                  ;; A second-line cookie may follow an ordinary comment.
                  (if (and (= line 0) (pair? (cdr ls)) s (string-prefix-at? (trim-left s) "#" 0))
                      (loop (cdr ls) 1 end) end)))))))

  ;; Name and doc are functions of src and language, with no independent state.
  (define (text-properties entry bytes)
    (let* ((b (if (string? bytes) (string->utf8 bytes) bytes))
           (start (source-prefix-size b))
           (lines (byte-lines (byte-slice b start (bytevector-length b))))
           (block (language-property entry 'block-comment #f)))
      (let loop ((ls lines) (doc '()) (inside? #f))
        (if (null? ls) (list #f (apply bytes-append (reverse doc)))
            (let* ((r (car ls))
                   (raw (byte-slice b (+ start (car r)) (+ start (caddr r))))
                   (line (safe-utf8 (byte-slice b (+ start (car r)) (+ start (cadr r))))))
              (if (not line) (list #f #vu8())
                  (let* ((line (trim-left line))
                         (comment? (or inside? (exists (lambda (p) (string-prefix-at? line p 0)) (comment-prefixes entry))
                                       (and block (string-prefix-at? line (car block) 0))))
                         (block-open? (and block (or inside? (string-prefix-at? line (car block) 0))))
                         (block-closed? (and block-open?
                                            (let search ((i 0))
                                              (and (< i (string-length line))
                                                   (or (string-prefix-at? line (cadr block) i) (search (+ i 1))))))))
                    (cond
                      (comment? (loop (cdr ls) (cons raw doc) (and block-open? (not block-closed?))))
                      ((string=? line "") (loop (cdr ls) '() #f))
                      (else (list (definition-name entry line) (apply bytes-append (reverse doc))))))))))))
)
