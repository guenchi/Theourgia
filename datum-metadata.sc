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
  ;; STRAIGHT TO (theourgia markers), NOT THROUGH code-markers.
  ;; Everything used here -- the marker grammar and two byte helpers --
  ;; now lives in a library that cannot reach the language table. Going
  ;; through `code-markers` would work and would put `languages` back in
  ;; this library's closure, and therefore in the reduction's, which is
  ;; the whole thing the move was for.
  (import (rnrs)
          (only (theourgia markers)
                wrapping projection-header-wrapper? projection-control
                byte-lines byte-slice))
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
  ;; THE DOC GRAMMAR IS THE DOC'S, NOT A LANGUAGE'S.
  ;;
  ;; This asked `(language-for-name 'scheme)` for the wrapping to look
  ;; for, which made a rule about PAYLOAD LEGALITY depend on the language
  ;; table: editing Scheme's `line-comment` from ";;" to anything else
  ;; changed whether `;; @file x` counted as a marker, and therefore
  ;; whether a record was accepted. Measured -- with the prefix changed,
  ;; the same doc string stops being `marker-in-doc` and a payload the
  ;; store used to refuse goes in.
  ;;
  ;; A datum doc is not written in a language. `datum-doc-format?` above
  ;; already requires every one of its lines to begin with `;`, whatever
  ;; the block's own language is, so the wrapping this looks for is fixed
  ;; here beside that rule. It is the same pair the Scheme entry happens
  ;; to carry today, which is why nothing about which docs are accepted
  ;; changes -- only what the answer depends on.
  (define doc-wrapping '((line-comment ";;")))
  (define (datum-doc-marker? doc)
    (and (string? doc)
         (let ((bytes (string->utf8 doc)) (entry doc-wrapping))
           (exists
             (lambda (row)
               (let ((line (byte-slice bytes (car row) (cadr row))))
                 (or (projection-header-wrapper? entry line)
                     (guard (e (#t #f)) (and (projection-control entry line) #t)))))
             (byte-lines bytes)))))
)
