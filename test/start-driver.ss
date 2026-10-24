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

;; THE CLIENT LIBRARY'S START, DRIVEN AS A PROGRAM: the instrument for the
;; rows that test it.
;;
;; `ensure-daemon!` in (theourgia client) starts a daemon and reports why a
;; start failed (serve-start-failed with the report's kind, the attempt's
;; token, the exit status, the client's own written entries). The thin client
;; no longer reaches it -- a plain command never starts a daemon -- and the
;; MCP shell does; the rows that pin what it answers keep their readings by
;; running this program in the thin client's place. It is NOT how a fixture
;; gets a daemon it merely needs: that is serve-for.ss, the explicit serve.
;;
;; (start-driver! dir) -> the path of a program taking the thin client's
;; command line (`<verb> <arg>... [--store <s>] [--socket <p>] [--actor <a>]
;; [--wire]`, those four read wherever they stand, as the thin client scans
;; them). It sends the request to the store's socket; when nobody is listening
;; it calls ensure-daemon! with the command line a client gives theourgiad.sc
;; and, once the daemon is ready, sends the request once more. An answer is
;; written as the thin client writes one (stdout, stderr, the exit code it
;; carries); a refusal is written as one datum with exit 75, as the thin client
;; refused before this part.
(define (start-driver! dir)
  (let ((program (string-append dir "/start-driver.sc"))
        (tree (let* ((d (current-directory)) (n (string-length d)))
                (let loop ((i (- n 1))) (if (char=? (string-ref d i) #\/) (substring d 0 i) (loop (- i 1)))))))
    (unless (file-exists? program)
      (call-with-output-file program
        (lambda (p)
          (put-string p
            (string-append
              "(import (chezscheme)\n"
              "        (only (theourgia client) socket-path serve-log-path request-frame call! ensure-daemon!\n"
              "              answer-field readable-shape? exit-code? program-launch)\n"
              "        (only (theourgia ffi) with-mutation-record mutation-record)\n"
              "        (only (theourgia answers) classify-failure))\n"
              "(define tree " (call-with-string-output-port (lambda (o) (write tree o))) ")\n"
              "(define (refuse a) (put-string (current-output-port) (string-append (format \"~s\" a) \"\\n\")) (exit 75))\n"
              "(define (env-value n) (let ((v (getenv n))) (and v (> (string-length v) 0) v)))\n"
              "(define (scan xs verb args store socket actor wire?)\n"
              "  (cond ((null? xs) (list verb (reverse args) store socket actor wire?))\n"
              "        ((and (member (car xs) '(\"--store\" \"--socket\" \"--actor\")) (pair? (cdr xs)))\n"
              "         (let ((v (cadr xs)))\n"
              "           (scan (cddr xs) verb args (if (string=? (car xs) \"--store\") v store)\n"
              "                 (if (string=? (car xs) \"--socket\") v socket) (if (string=? (car xs) \"--actor\") v actor) wire?)))\n"
              "        ((string=? (car xs) \"--wire\") (scan (cdr xs) verb args store socket actor #t))\n"
              "        ((not verb) (scan (cdr xs) (string->symbol (car xs)) args store socket actor wire?))\n"
              "        (else (scan (cdr xs) verb (cons (car xs) args) store socket actor wire?))))\n"
              "(define (deliver bytes)\n"
              "  (let* ((text (utf8->string bytes))\n"
              "         (e (guard (x (#t #f)) (and (readable-shape? text) (read (open-string-input-port text)))))\n"
              "         (out (and e (answer-field e 'stdout string?))) (err (and e (answer-field e 'stderr string?)))\n"
              "         (code (and e (answer-field e 'exit exit-code?))))\n"
              "    (if (and out err code)\n"
              "        (begin (put-string (current-output-port) out)\n"
              "               (unless (string=? err \"\") (put-string (current-error-port) err))\n"
              "               (flush-output-port (current-output-port)) (exit code))\n"
              "        (refuse '(error transport-unknown (reason unreadable-answer))))))\n"
              "(define (settle o start)\n"
              "  (case (car o)\n"
              "    ((answer) (deliver (cadr o)))\n"
              "    ((not-sent) (refuse (cadr o)))\n"
              "    ((no-daemon) (if start (start) (refuse (list 'error 'connect-failed (list 'errno (cadr o)) '(carried-out no)))))\n"
              "    (else (refuse (list 'error 'transport-unknown (list 'reason (cadr o)))))))\n"
              "(with-mutation-record (lambda ()\n"
              "  (guard (e ((classify-failure e (mutation-record)) => refuse))\n"
              "    (let* ((s (scan (cdr (command-line)) #f '() #f #f #f #f))\n"
              "           (verb (list-ref s 0)) (args (list-ref s 1))\n"
              "           (store (or (list-ref s 2) (env-value \"THEOURGIA_STORE\") \".\"))\n"
              "           (socket (or (list-ref s 3) (socket-path store)))\n"
              "           (actor (or (list-ref s 4) (env-value \"THEOURGIA_ACTOR\") (env-value \"USER\") \"cli\"))\n"
              "           (frame (request-frame store verb args (list (cons 'actor actor) (cons 'writer (env-value \"THEOURGIA_WRITER\"))\n"
              "                                                       (cons 'stdin #f) (cons 'cwd (current-directory))\n"
              "                                                       (cons 'mode (if (list-ref s 5) 'wire 'human)))))\n"
              "           ;; THE START'S COMMAND LINE IS MADE ONLY FOR A START, as the client made it:\n"
              "           ;; its log path resolves the store, which may not be readable when a\n"
              "           ;; daemon that is already there would answer.\n"
              "           (argv (lambda () (append (list (or (getenv \"THEOURGIA_SCHEME\") \"scheme\"))\n"
              "                         (program-launch (string-append tree \"/theourgiad.sc\")) (list \"serve\")\n"
              "                         (if (and (> (string-length store) 0) (char=? (string-ref store 0) #\\-))\n"
              "                             (list \"--store\" store) (list store))\n"
              "                         (list \"--socket\" socket \"--detach\" \"--log\" (serve-log-path store))))))\n"
              "      (settle (call! socket frame 30000)\n"
              "              (lambda ()\n"
              "                (let ((started (ensure-daemon! (argv) store socket)))\n"
              "                  (if (eq? started 'ready) (settle (call! socket frame 30000) #f) (refuse started)))))))))\n")))))
    program))
