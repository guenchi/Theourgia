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
(library (theourgia datum-metadata)
  (export datum-doc-marker? datum-doc-format?)
  (import (rnrs) (theourgia code-markers) (theourgia text-code) (theourgia languages))
  ;; A doc is a consecutive, terminated comment block. Plain source or a
  ;; trailing blank line would change the set of forms or lose attachment
  ;; when generated code is read again.
  (define (datum-doc-format? doc)
    (and (string? doc)
         (let ((n (string-length doc)))
           (let lines ((start 0))
             (or (= start n)
                 (let indent ((i start))
                   (and (< i n)
                        (if (memv (string-ref doc i) '(#\space #\tab)) (indent (+ i 1))
                            (and (char=? (string-ref doc i) #\;)
                                 (let end ((j (+ i 1)))
                                   (and (< j n)
                                        (cond ((char=? (string-ref doc j) #\newline) (lines (+ j 1)))
                                              ((char=? (string-ref doc j) #\return)
                                               (lines (if (and (< (+ j 1) n) (char=? (string-ref doc (+ j 1)) #\newline)) (+ j 2) (+ j 1))))
                                              (else (end (+ j 1)))))))))))))))
  (define (datum-doc-marker? doc)
    (and (string? doc)
         (let ((bytes (string->utf8 doc)) (entry (language-for-name 'scheme)))
           (exists
             (lambda (row)
               (let ((line (byte-slice bytes (car row) (cadr row))))
                 (or (projection-header-wrapper? entry line)
                     (guard (e (#t #f)) (and (projection-control entry line) #t)))))
             (byte-lines bytes)))))
)
