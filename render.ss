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
(library (theourgia render)
  (export render-wire render-human)
  (import (chezscheme))
  (define (render-wire value)
    (parameterize ((print-graph #f) (print-length #f) (print-level #f)
                   (print-radix 10) (print-unicode #f) (print-gensym #f))
      (call-with-string-output-port (lambda (p) (write value p) (newline p)))))
  (define (render-human answer)
    (cond
      ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
            (eq? (caadr answer) 'text)) (cadadr answer))
      ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
            (eq? (caadr answer) 'items)) (apply string-append (map render-wire (cdadr answer))))
      (else (render-wire answer))))
)
