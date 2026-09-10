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
;; Does R6RS guard re-enter the dynamic extent when a clause DECLINES?
;; If it does, the re-entry check I added to call-with-lock fires during
;; ordinary exception propagation and replaces the real error.
(import (chezscheme) (theourgia ffi))
(define dir "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/gwork")
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define lock (string-append dir "/lock"))
(printf "outer guard sees: ~s\n"
  (guard (e (#t (list 'outer (if (and (vector? e) (> (vector-length e) 0)) (vector-ref e 0) 'not-a-vector))))
    (guard (e ((and (vector? e) (eq? (vector-ref e 0) 'never-matches)) 'inner))
      (with-exclusive-lock lock
        (lambda (fd) (raise (vector 'durable-error 'write (cons "/x" 5))))))))
(printf "plain dynamic-wind control: ~s\n"
  (let ((log '()))
    (guard (e (#t (reverse (cons 'caught log))))
      (guard (e ((eq? e 'never) 'inner))
        (dynamic-wind
          (lambda () (set! log (cons 'in log)))
          (lambda () (raise 'boom))
          (lambda () (set! log (cons 'out log))))))))

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "verify-guard complete\n")
