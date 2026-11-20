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

;; A COMMIT'S `behind` NAMES WHAT LANDED AFTER ITS DRAFTS WERE TAKEN, NOT ITS
;; OWN RECORDS (README, commit). A commit's plan and sets are appended by the
;; store's log writer, whose name is not the draft writer's, and a lone
;; writer's commit was named behind its own records.
;;
;;   CB-1  a lone writer writes a draft and commits it: no behind clause
;;   CB-2  a set landed on another block between the draft and the commit:
;;         behind names it, one record, and not the commit's own two
;;   CB-3  a commit that is its log writer's FIRST record (an adopted copy's
;;         successor): no behind clause, its writer read at 0 has not moved
(import (chezscheme))
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
                   "/commit-behind-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(define home (string-append root "/home"))
(define cli "../core.sc")

;; -> the first datum a command answered, with TEXT on its standard input
(define (run store text . args)
  (call-with-output-file (string-append root "/in") (lambda (p) (display text p)) 'replace)
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER THEOURGIA_HOME=" home
            " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " --wire < " root "/in > " root "/out 2> " root "/err"))
  (let ((x (call-with-input-file (string-append root "/out") read)))
    (if (eof-object? x) (list 'no-answer) x)))
(define n-store 0)
;; -> (<store> <block id> <cursor text>): a store holding one block, and the
;; cursor after it
(define (store-with-a-block!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))))
    (run d "" "init")
    (let* ((a (run d "" "insert" "--under" "root" "--title" "A"))
           (w (cadr (assq 'cursor (cdr a))))
           (id (car (car (cadr (assq 'state (cdr a)))))))
      (list d id (string-append (car w) ":" (number->string (cdr w)))))))
(define (end-of d)
  (let* ((c (run d "" "check")) (w (cadr (assq 'local-writer (cdr c)))))
    (cadr (assq 'end (cdr (assoc w (cadr (assq 'writers (cdr c)))))))))
;; -> the behind and behind-records clauses of a commit's answer
(define (behind-of a) (list (and (list? a) (assq 'behind (filter pair? (cdr a))))
                            (and (list? a) (assq 'behind-records (filter pair? (cdr a))))))

(want "CB-1 a lone writer's commit of its own draft answers no behind clause: nothing landed after the draft but the commit itself"
      (let* ((s (store-with-a-block!)) (d (car s)) (c (cadr s)))
        (run d "" "write" c "new text" "--writer" "me")
        (let ((a (run d "" "commit" c "--writer" "me")))
          (list (car a) (behind-of a))))
      '(ok (#f #f)))

(define s2 (store-with-a-block!))
(define d2 (car s2))
(define c2 (cadr s2))
(define other (car (car (cadr (assq 'state (cdr (run d2 "" "insert" "--under" "root" "--title" "D")))))))
(run d2 "" "write" c2 "new text" "--writer" "me")
;; the set's record, (<log writer> . <seq>): the one record that landed after the draft
(define landed (cadr (assq 'cursor (cdr (run d2 "" "set" other "note" "after the draft")))))
(printf "the set after the draft: ~s\n" landed)
(want "CB-2 a set landed on another block between the draft and the commit: behind names that one record, not the commit's own"
      (let ((a (run d2 "" "commit" c2 "--writer" "me")))
        (list (car a) (behind-of a)))
      (list 'ok (list (list 'behind (list landed)) (list 'behind-records (list (cons (car landed) 1))))))

(define s3 (store-with-a-block!))
(define copy3 (string-append (car s3) "-copy"))
(system (string-append "cp -R '" (car s3) "' '" copy3 "'"))
(define adopted (run copy3 "" "adopt"))
(run copy3 "" "write" (cadr s3) "the copy text" "--writer" "me")
(define first-commit (run copy3 "" "commit" (cadr s3) "--writer" "me"))
(printf "the copy's adopt: ~s\n  its first commit: ~s\n" adopted first-commit)
(want "CB-3 a commit that is its log writer's first record (an adopted copy's successor) answers no behind clause"
      (list (car adopted) (car first-commit) (behind-of first-commit))
      '(ok ok (#f #f)))

;; CB-4, A COMMIT COMPLETED ON RETRY: a tracked commit killed at its second
;; append (after its plan record, crash-at.ss), then the same request again.
;; The plan is this commit's own record, written by the log writer before the
;; retry began; the retry must not name it behind.
(include "crash-at.ss")
(define s4 (store-with-a-block!))
(define d4 (car s4))
(define c4 (cadr s4))
(run d4 "" "write" c4 "the crashed commit text" "--writer" "me")
(define v4 (list-ref (assq 'projection (cdr (run d4 "" "read" c4 "--working-info" "--writer" "me"))) 4))
(define w4 (let ((t (caddr s4))) (substring t 0 (let loop ((i 0)) (if (char=? (string-ref t i) #\:) i (loop (+ i 1)))))))
(define cur4 (string-append w4 ":" (number->string (end-of d4))))
(define crashed4
  (car (crash-at 2 root home cli
                 (list "commit" c4 "--req" "req-cb4" "--cursor" cur4 "--working-version" v4 "--writer" "me" "--store" d4))))
(define retry4 (run d4 "" "commit" c4 "--req" "req-cb4" "--cursor" cur4 "--working-version" v4 "--writer" "me"))
(printf "CB-4 the crash stopped at barrier ~s; the retry: ~s\n" crashed4 retry4)
(want "CB-4 a commit killed after its plan record and completed by the retry answers no behind clause: the plan is its own"
      (list crashed4 (car retry4) (behind-of retry4))
      '(2 ok (#f #f)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ncommit-behind complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
