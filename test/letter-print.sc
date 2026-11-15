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

;; A LETTER'S RESULT BLOCKS PRINT A TEXT-MODE CODE BLOCK'S SRC AS READ PRINTS IT
;; (render.sc reader-form). review-results answers each block the reviewer
;; made with its settled fields, ((<field> <value> <actor>) ...); a code block
;; imported or written as text keeps its src as bytes, and the letter printed
;; them as #vu8(...) where read --wire prints the text. The answer as a value
;; keeps the bytes; only the printed answer changes. In process, as
;; review-channel.sc runs the channel, the answers printed by render-wire.
;;
;;   LP-1  the printed review-results gives the code block's src as the text the
;;         printed read gives
;;   LP-2  the answer as a value still holds the bytes
;;   LP-3  CONTROL: a section block's fields print as they are
(import (chezscheme) (theourgia rpc) (only (theourgia reduce) block-id)
        (only (theourgia render) render-wire)
        (only (theourgia arguments) parse-arguments argument-positionals))

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
                   "/letter-print-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define M (string-append root "/main"))
(define X (string-append root "/scoped"))
(define (run store actor . args) (rpc-dispatch store args actor))
(define (scope-run store actor . args)
  (let ((nodes (parse-arguments 'scope args)))
    ((eval 'scope-verb (environment '(theourgia channel))) store actor (argument-positionals nodes) nodes #f)))
(define (new-id a) (let* ((ev (assq 'events (cdr a))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define (one a k) (let ((c (assq k (filter pair? (cdr a))))) (and c (cadr c))))
(define (insert-as store actor parent fields)
  (new-id (car (cadr (run store actor 'batch (format "~s" (list (list 'insert parent #f fields))))))))
;; an answer as it is printed, read back as data
(define (printed a) (read (open-string-input-port (render-wire a))))
;; the (block <id> ...) of a results answer
(define (result-block a id)
  (let ((r (and (pair? a) (assq 'results (filter pair? (cdr a))))))
    (and r (find (lambda (b) (and (pair? b) (equal? (cadr b) id))) (cdr (assq 'blocks (cdr r)))))))
(define (triple b name)
  (let ((f (and b (assq 'fields (cddr b))))) (and f (assq name (cadr f)))))

(run M "author" 'init)
(define R1 (new-id (run M "author" 'insert "--title" "Root one" "--text" "r1")))
(run M "author" 'tag "t1")
(define S (scope-run M "author" X "--cut" "t1" "--roots" R1 "--for" "codex"))
(define LC (one S 'letter-copy))
(define V (insert-as X "codex" LC '((kind . section) (title . "Verdict"))))
(define C (insert-as X "codex" V (list '(kind . code) '(mode . text) '(lang . scheme)
                                       (cons 'src (string->utf8 "(define (f x) x)\n")))))
(define results (run X "codex" 'review-results))
(define read-src (cdr (assq 'src (cdr (assq 'fields (cadr (printed (run X "codex" 'read C))))))))
(printf "read prints the code block's src as ~s\n" read-src)

(want "LP-1 printed, review-results gives the code block's src as the text the printed read gives, with its actor"
      (triple (result-block (printed results) C) 'src)
      (list 'src read-src "codex"))
(want "LP-2 the answer as a value still holds the src as bytes"
      (let ((t (triple (result-block results C) 'src))) (and t (bytevector? (cadr t))))
      #t)
(want "LP-3 CONTROL: a section block's fields print as they are"
      (equal? (result-block (printed results) V) (result-block results V))
      #t)

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nletter-print complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
