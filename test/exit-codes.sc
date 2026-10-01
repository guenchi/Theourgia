;; Copyright 2018 - 2026 The Theourgia Authors
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

;; One refusal, one exit code (F97): whatever the verb and the route, an
;; answer headed `error` leaves core.sc with exit 1 and `ok` with exit 0.
;; Measured before this: `insert` refused by the instance check exited 0
;; while `set` with the same answer exited 1.
(import (chezscheme) (only (theourgia rpc) rpc-verbs register-verbs!)
        (only (theourgia extensions) extension-verbs))

;; THE VERBS REGISTERED FROM OUTSIDE THE CORE TABLE, as core.sc registers them:
;; the programs this spawns answer them, so the population includes them.
(register-verbs! extension-verbs)

(define bad 0)
(define rows 0)
(define (want-1 label got expect)
  (set! rows (+ rows 1))
  (if (equal? got expect)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a -> ~s   WANT ~s\n" label got expect))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/exitcodes-" pid-text))
(when (file-exists? here)
  (assertion-violation 'exit-codes "scratch directory already exists" here))
(define store (string-append here "/store"))
(define home-a (string-append here "/home-a"))
(define home-b (string-append here "/home-b"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(system (string-append "mkdir -p " store " " home-a " " home-b))
(putenv "THEOURGIA_RUN" (string-append here "/run"))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))
;; Runs core.sc in process (THEOURGIA_LOCAL=1) under the given home; answers
;; (rc . <last datum on stdout>), the machine-home line skipped by reading
;; the last datum.
(define (run home argv)
  (let ((out (string-append here "/out.txt")) (rc (string-append here "/rc.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 THEOURGIA_HOME=" home " "
              "scheme --script ../core.sc " argv " --store " store " --wire < /dev/null > " out " 2>/dev/null; echo $? > " rc))
    (let ((data (guard (e (#t '()))
                  (call-with-port (open-input-file out)
                    (lambda (p)
                      (let loop ((acc '()))
                        (let ((d (read p)))
                          (if (eof-object? d) (reverse acc) (loop (cons d acc))))))))))
      (cons (string->number (let ((t (file-text rc))) (substring t 0 (max 0 (- (string-length t) 1)))))
            (if (null? data) 'no-answer (car (reverse data)))))))
(define (head a) (and (pair? a) (car a)))

(run home-a "init")
;; THE BLOCK'S ID COMES FROM THE INSERT ANSWER: (events (("<writer>" . <seq>)))
;; names the writer and the sequence the block was written at.
(define the-id
  (let* ((a (cdr (run home-a "insert --title A --text one")))
         (ev (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'events))) (cdr a))))
         (e (and ev (pair? (cdr ev)) (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (string? (car e)) (integer? (cdr e))
         (string-append (car e) "." (number->string (cdr e))))))

(printf "== EC-1: every verb, refused for want of arguments, exits 1 ==\n")
;; A verb given no argument answers usage or bad-request; the head is error
;; and the exit must be 1. The row prints the verbs whose exit disagreed
;; with their head.
(define (disagreeing runs)
  (filter (lambda (r) (let ((rc (cadr r)) (h (caddr r)))
                        (or (and (eq? h 'error) (not (= rc 1)))
                            (and (eq? h 'ok) (not (= rc 0))))))
          runs))
(want "CONTROL EC-1 the verb list is not empty and the store has a block"
      (list (> (length (rpc-verbs)) 10) (string? the-id))
      '(#t #t))
(want "EC-1 no verb answers error with exit 0, nor ok with a non-zero exit, when called with no argument"
      (disagreeing
        (map (lambda (v)
               (let ((r (run home-a (symbol->string v))))
                 (list v (car r) (head (cdr r)))))
             (rpc-verbs)))
      '())

(printf "== EC-2: the instance refusal exits 1 from every write verb ==\n")
;; Under a second home every write is refused by the instance check with the
;; same answer; the exit must be the same too.
(want "EC-2 insert, set, move, del, write, commit and discard refused under another home all exit 1"
      (disagreeing
        (map (lambda (argv)
               (let ((r (run home-b argv)))
                 (list (substring argv 0 (let find ((i 0)) (if (or (= i (string-length argv)) (char=? (string-ref argv i) #\space)) i (find (+ i 1)))))
                       (car r) (head (cdr r)))))
             (list "insert --title B --text x"
                   (string-append "set " the-id " src two")
                   (string-append "move " the-id " root")
                   (string-append "del " the-id)
                   (string-append "write " the-id " bytes --writer w")
                   "commit --writer w"
                   "discard --writer w")))
      '())

(system (string-append "rm -rf " here))
(printf "\n~a failures\nrows: ~a\nexit-codes complete\n" bad rows)
