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

(import (chezscheme) (theourgia wire))
(define (raises? t) (guard (e (#t #t)) (t) #f))
(printf "gensym actor refused:            ~a\n"
        (raises? (lambda () (encode-record 0 0 (list (gensym "a") 0 0 0 0) '() '()))))
(printf "gensym nested in plan-event-id:  ~a\n"
        (raises? (lambda () (encode-record 0 0 (list "w" "r" 0 "f" (cons (gensym "a") 3)) '() '()))))
(printf "character in the actor refused:  ~a\n"
        (raises? (lambda () (encode-record 0 0 (list "w" "r" #\a "f" #f) '() '()))))
(printf "CONTROL string actor accepted:   ~a\n"
        (not (raises? (lambda () (encode-record 0 0 "agent:claude" '() '())))))
(printf "CONTROL five-element accepted:   ~s\n"
        (decode-line (encode-record 0 0 (list "agent:claude" "req-1" 0 "fp" (cons "w" 3)) '() '(a))))
(printf "CONTROL #%-prefixed actor accepted: ~s\n"
        (decode-line (encode-record 0 0 (list "#%alice" "b" "c" "d" "e") '() '(a))))
(printf "cyclic vector in actor refused:  ~a\n"
        (raises? (lambda ()
          (let ((v (make-vector 1)))
            (vector-set! v 0 v)
            (encode-record 0 0 (list v 0 0 0 0) '() '())))))
(printf "shared (acyclic) subtree accepted: ~a\n"
        (not (raises? (lambda ()
          (let ((shared (list "s")))
            (encode-record 0 0 (list shared "b" 0 "d" shared) '() '(a)))))))
(printf "CONTROL 'single accepted:        ~s\n"
        (decode-line (encode-record 0 0 (list "agent:claude" "req-1" 'single "fp" #f) '() '(a))))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "actor-check complete\n")
