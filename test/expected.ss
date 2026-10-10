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

;; A ROW'S EXPECTED VALUE, EVALUATED UNDER A GUARD.
;;
;; A row is (want <label> <got> <expected>). The value it computes is caught
;; (`caught`), so a row that raises is a FAIL line and the rows after it run.
;; The expected value needs the same, and it did not have it: left bare, one
;; that raised ended the file; wrapped in `caught` as well, a row whose two
;; sides raised the same message compared equal and read green.
;;
;; (with-expected <label> <expected> (<x>) <body>) evaluates <expected>; when
;; it returns, <body> runs with <x> bound to its value. When it raises, the row
;; is reported through the including fixture's own want-1 -- its counters, its
;; format -- with the raise's message as the value got, against a fresh
;; procedure as the value wanted, which nothing a row computes can equal; and
;; <body> does not run.
;;
;; THE INCLUDING FIXTURE DEFINES want-1 (label got expected) before its first
;; row runs, and includes this before it defines `want`. The runner requires
;; both of every fixture that defines `want` (run-fixtures.sh).
(define-syntax with-expected
  (syntax-rules ()
    ((_ label expected (x) body)
     (call-with-current-continuation
       (lambda (k)
         (let ((x (guard (e (#t (k (want-1 label
                                          (list 'expected-raised
                                                (if (and (condition? e) (message-condition? e)) (condition-message e) e))
                                          (lambda () 'expected-raised)))))
                    expected)))
           body))))))
