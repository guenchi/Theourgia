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
(import (chezscheme) (theourgia rpc) (theourgia render) (theourgia datum-code)
        (theourgia text-code) (theourgia digest) (only (theourgia store) store-resident-cache!))
(store-resident-cache! #t)
(define args (cdr (command-line)))
(define store (car args))
(define actor (cadr args))
(define (request-answer request actor)
  (if (equal? request '(transport-verbs))
      (list 'ok (list 'text (apply string-append
        (map (lambda (verb) (string-append (bytevector->hex (string->utf8 (symbol->string verb))) "\n")) (rpc-verbs)))))
      (rpc-dispatch store request actor)))
(define (frame-answer bytes)
  (guard (e (#t (render-wire '(error bad-request (reason framing)))))
    (unless (safe-utf8 bytes) (raise 'invalid-utf8))
    (let* ((forms (datum-source-read bytes)) (request (and (= (length forms) 1) (caar forms))))
      (unless (= (length forms) 1) (raise 'framing))
      (if (and (list? request) (pair? request) (eq? (car request) 'transport-v1))
          (begin
            (unless (and (= (length request) 5) (for-all string? (list-head (cdr request) 3))
                         (member (list-ref request 3) '("wire" "human"))) (raise 'framing))
            (let* ((answer (if (string=? (cadr request) store)
                               (request-answer (list-ref request 4) (caddr request))
                               '(error transport-store-mismatch)))
                   (text ((if (string=? (list-ref request 3) "human") render-human render-wire) answer)))
              (render-wire (list 'transport-answer (if (rpc-ok? answer) 0 1) (bytevector->hex (string->utf8 text))))))
          (render-wire (rpc-dispatch store request actor))))))
;; The outer socket layer admits bounded byte lines. This pipe has the same
;; limit so one-shot local fallback cannot bypass admission.
(let loop ()
  (let ((line (get-line (current-input-port))))
    (unless (eof-object? line)
      (let* ((bytes (string->utf8 line))
             (answer (if (> (bytevector-length bytes) 1048576)
                         (render-wire '(error bad-request (reason frame-limit))) (frame-answer bytes))))
        (put-string (current-output-port) answer)
        (flush-output-port (current-output-port)))
      (unless (member "--once" args) (loop)))))
