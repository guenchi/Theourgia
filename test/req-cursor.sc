;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; THE CURSOR TAKES PART IN WHAT MAKES TWO REQUESTS THE SAME (README,
;; Requests and replay). A request's identity is its id and the writer its
;; cursor names (request.sc request-identity); its fingerprint holds the whole
;; cursor (request-fingerprint). Read through the dispatch:
;;
;;   RC-1  the same --req and the same cursor again: the replay of the first
;;   RC-2  the same --req, a cursor on the same writer at another sequence:
;;         req-mismatch, and nothing written
;;   RC-3  the same --req, a cursor on another writer (an adopted copy's
;;         successor): another request, which runs
(import (chezscheme) (theourgia rpc) (theourgia log))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/req-cursor-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define store (string-append root "/store"))
(define W (cadr (assq 'writer (cdr (rpc-dispatch store '(init) "test")))))
(rpc-dispatch store '(insert "--title" "a") "test")
(rpc-dispatch store '(insert "--title" "b") "test")
(define (end-seq st w) (discovery-end-seq (discover-prefix st w 'shared)))
(define (cursor w seq) (format "~a:~a" w seq))
(define (insert! st req at) (rpc-dispatch st (list 'insert "--title" "tracked" "--req" req "--cursor" at) "test"))
;; an answer's shape: (ok replay) (ok new) or (error <kind>)
(define (shape a)
  (cond ((not (pair? a)) (list 'not-an-answer a))
        ((eq? (car a) 'ok)
         (let ((r (assq 'replay (cdr a)))) (list 'ok (if (and r (eq? (cadr r) #t)) 'replay 'new))))
        (else (list (car a) (and (pair? (cdr a)) (cadr a))))))

(define e0 (end-seq store W))
(define first (insert! store "R" (cursor W e0)))
(define e1 (end-seq store W))
(printf "the first: ~s at ~a, the writer's end now ~a\n" (shape first) (cursor W e0) e1)
(want "RC-1 the same --req with the same cursor again is the same request: answered as a replay, nothing written"
      (list (shape first) (shape (insert! store "R" (cursor W e0))) (= (end-seq store W) e1))
      '((ok new) (ok replay) #t))
(want "RC-2 the same --req with a cursor on the same writer at another sequence is the same identity with another fingerprint: req-mismatch, nothing written"
      (list (shape (insert! store "R" (cursor W (- e0 1)))) (= (end-seq store W) e1))
      '((error req-mismatch) #t))

;; ANOTHER WRITER: a copy, adopted, retires W and writes as its successor
(define copy (string-append root "/copy"))
(system (string-append "cp -R '" store "' '" copy "'"))
(define adopted (rpc-dispatch copy '(adopt) "test"))
(define W2 (let* ((ls (log-open copy)) (w (local-writer-of copy ls))) (load-commit! ls) w))
(define s0 (end-seq copy W2))
(define other (insert! copy "R" (cursor W2 s0)))
(printf "the copy: ~s; its writer ~a; R on it ~s\n" (car adopted) W2 other)
(want "RC-3 the same --req with a cursor on another writer is another request: it runs, and its record is written on that writer"
      (list (car adopted) (and (string? W2) (not (equal? W2 W))) (shape other) (= (end-seq copy W2) (+ s0 1))
            (shape (insert! copy "R" (cursor W e0))))
      '(ok #t (ok new) #t (ok replay)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nreq-cursor complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
