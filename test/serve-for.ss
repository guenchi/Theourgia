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

;; A FIXTURE THAT NEEDS A DAEMON STARTS ONE: IT IS THE OPENER.
;;
;; A plain command never starts a daemon; with none listening it answers in
;; a core.sc child of its own. A row that is about a daemon -- a stream, a
;; publication, a connection's routing -- therefore asks for one first, as an
;; opener does, by the start the README documents:
;;
;;   theourgiad.sc serve <store> --socket <socket> --detach --log <run-root>/<key>/serve.log
;;
;; (a store starting with `-` as `--store <store>`), which is also the command
;; line a fixture already looks for. The start is in this one place, so a
;; change to its spelling is a change here.
;;
;; (serve-for! dir store socket env) starts it and waits, bounded, for the
;; socket to accept a connection. It is (serve-await! (serve-spawn! ...)): a
;; row whose daemon meets a hold in its own first fold spawns, releases the
;; hold, and then awaits. -> the socket path when it does; a socket
;; that already accepts one is answered without a start. Otherwise the start's
;; refusal: the first `(error ...)` the daemon wrote to its log after this
;; start, else the last datum it wrote there (the log is read only past the
;; length it had before; the daemon's exit line follows its refusal), or
;; `(serve-for no-answer <what>)` when the process ended saying nothing, or
;; `(serve-for timeout)`.
;; DIR is a scratch directory; SOCKET #f is the store's own path, whose run
;; directory is made here (as the clients make it); ENV is environment text put
;; before the command, for a daemon that must start with a fault, a hold or a
;; buffer armed. It runs with the fixture's library variables, from the test
;; directory, as every fixture does.
(define serve-for-count 0)
(define (serve-for-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (serve-for-run cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? t) "" t)))
(define (serve-for-connects? socket)
  (zero? (system (string-append "perl -MIO::Socket::UNIX -e 'exit(IO::Socket::UNIX->new(Peer => $ARGV[0]) ? 0 : 1)' "
                                "'" socket "' 2>/dev/null"))))
(define (serve-for-refusal text)
  (let loop ((port (open-input-string text)) (last #f))
    (let ((d (guard (e (#t (eof-object))) (read port))))
      (cond ((eof-object? d) last)
            ((and (pair? d) (eq? (car d) 'error)) d)
            (else (loop port d))))))
(define (serve-spawn! dir store socket env)
  (set! serve-for-count (+ serve-for-count 1))
  (let* ((n (number->string serve-for-count))
         (program (string-append dir "/serve-for-" n ".sc"))
         (q (lambda (s) (call-with-string-output-port (lambda (p) (write s p))))))
    (when (file-exists? program) (delete-file program))
    ;; THE PATHS ARE THE PRODUCT'S, from (theourgia client)'s path rules; the
    ;; program prints them and the command line, one datum.
    (call-with-output-file program
      (lambda (p)
        (put-string p
          (string-append
            "(import (chezscheme) (only (theourgia client) socket-path serve-log-path program-launch))\n"
            "(define store " (q store) ")\n"
            "(define socket (or " (q (or socket #f)) " (socket-path store)))\n"
            "(define tree (let* ((d (current-directory)) (n (string-length d)))\n"
            "  (let loop ((i (- n 1))) (if (char=? (string-ref d i) #\\/) (substring d 0 i) (loop (- i 1))))))\n"
            "(define argv (append (list (or (getenv \"THEOURGIA_SCHEME\") \"scheme\"))\n"
            "                     (program-launch (string-append tree \"/theourgiad.sc\")) (list \"serve\")\n"
            "                     (if (and (> (string-length store) 0) (char=? (string-ref store 0) #\\-))\n"
            "                         (list \"--store\" store) (list store))\n"
            "                     (list \"--socket\" socket \"--detach\" \"--log\" (serve-log-path store))))\n"
            "(write (list socket (serve-log-path store) (socket-path store) argv)) (newline)\n"))))
    ;; THE PLAN IS THE LAST LIST HEADED BY A STRING: a process announces its
    ;; machine home ahead of anything else when THEOURGIA_HOME is set.
    ;; THE PLAN IS THE LAST LIST HEADED BY A STRING: a process announces its
    ;; machine home ahead of anything else when THEOURGIA_HOME is set.
    (let* ((plan (let loop ((port (open-input-string
                                    (serve-for-run (string-append env " scheme --script " program " 2>/dev/null < /dev/null"))))
                            (found #f))
                   (let ((d (guard (e (#t (eof-object))) (read port))))
                     (cond ((eof-object? d) found)
                           ((and (list? d) (= (length d) 4) (string? (car d))) (loop port d))
                           (else (loop port found))))))
           (sock (and (list? plan) (car plan)))
           (log (and (list? plan) (cadr plan)))
           (own (and (list? plan) (string=? sock (caddr plan))))
           (argv (and (list? plan) (cadddr plan))))
      (cond
        ((not (list? plan)) '(serve-for no-answer plan))
        ((serve-for-connects? sock) (list 'serving sock))
        (else
         (when own (system (string-append "mkdir -p \"$(dirname '" sock "')\"")))
         (system (string-append "mkdir -p \"$(dirname '" log "')\""))
         (let* ((before (string-length (serve-for-text log)))
                (cmd (apply string-append (map (lambda (a) (string-append " '" a "'")) argv)))
                (pid (let ((t (serve-for-run (string-append env cmd " > /dev/null 2>&1 < /dev/null & echo $!"))))
                       (string->number (let trim ((s t)) (if (and (> (string-length s) 0)
                                                                    (char-whitespace? (string-ref s (- (string-length s) 1))))
                                                               (trim (substring s 0 (- (string-length s) 1))) s))))))
           (list 'started sock log before pid)))))))

;; THE WAIT, bounded at 20 s: -> the socket path once it accepts a connection,
;; else the start's refusal as serve-for! describes it.
(define (serve-await! handle)
  (cond
    ((and (pair? handle) (eq? (car handle) 'serving)) (cadr handle))
    ((not (and (pair? handle) (eq? (car handle) 'started))) handle)
    (else
     (let* ((sock (list-ref handle 1)) (log (list-ref handle 2)) (before (list-ref handle 3)) (pid (list-ref handle 4))
            (alive? (lambda () (and pid (zero? (system (string-append "kill -0 " (number->string pid) " 2>/dev/null")))))))
       (let wait ((k 0))
         (cond
           ((serve-for-connects? sock) sock)
           ((not (alive?))
            (let* ((text (serve-for-text log))
                   (new (if (> (string-length text) before) (substring text before (string-length text)) "")))
              (or (serve-for-refusal new) '(serve-for no-answer exited))))
           ((> k 400) '(serve-for timeout))
           (else (system "sleep 0.05") (wait (+ k 1)))))))))

(define (serve-for! dir store socket env)
  (serve-await! (serve-spawn! dir store socket env)))
