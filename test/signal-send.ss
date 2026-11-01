;; Copyright 2018 - 2026 guenchi
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

;; A SIGNAL ONLY TO A PROCESS THIS RUN REGISTERED, AND ONLY WHILE IT IS STILL
;; THAT PROCESS. One definition, included by every fixture that signals a pid
;; it read from a pid file or from $! (daemon-idle, daemon-namespace, log15,
;; launch-self, runner-self, cli2, f0-ondemand).
;;
;; NEVER: A SIGNAL TO A PID THAT WAS NOT VERIFIED. A pid is reused once its
;; process is gone, so a send to a process that already exited can reach an
;; unrelated one. signal! sends only when the pid was registered by started!,
;; answers kill -0 AND its start instant (ps lstart) is the one recorded when
;; it was registered; otherwise it sends nothing and prints
;; "NOTE <sig> to <pid> not sent: already gone" on the output. A pid never
;; registered, or gone before it was registered (no recorded instant), is
;; never signalled.
;;
;; REGISTER WHERE THE PROCESS STARTS: started! right after the call that
;; spawned it returns, or at the first read of a pid file the process wrote
;; itself. A registration just before the send would record whatever holds
;; the pid at that moment, and the check would then pass for it.
;;
;; IT DEPENDS ON NOTHING THE INCLUDING FILE DEFINES: only system, process and
;; format, because the fixtures' own shell helpers differ in shape ((sh cmd)
;; in some, (sh . parts) in others). The including file defines none of
;; started, identities, pid-identity, started!, alive?, signal! or
;; pid-of-file itself; it may set! identities, as the CONTROL rows on the
;; guard do.
;;
;; A pid is an integer above 1: kill -0 of 0 or -1 asks about a whole group or
;; every process, so anything else is neither registered nor alive.
;;
;; STATED LIMIT: lstart counts whole seconds, so a pid reused within the
;; second its first process started reads as the same process.
(define started '())
(define identities '())
(define (pid-identity pid)
  (let* ((p (process (format "ps -o lstart= -p ~a 2>/dev/null" pid)))
         (t (get-string-all (car p))))
    (close-port (car p))
    (close-port (cadr p))
    (and (string? t) (> (string-length t) 0) t)))
(define (alive? pid)
  (and (integer? pid) (> pid 1) (zero? (system (format "kill -0 ~a 2>/dev/null" pid)))))
(define (started! pid)
  (when (and (integer? pid) (> pid 1))
    (set! started (cons pid started))
    (set! identities (cons (cons pid (pid-identity pid)) identities)))
  pid)
(define (signal! pid sig)
  (let ((known (and pid (assv pid identities))))
    (cond
      ((and known (cdr known) (alive? pid) (equal? (cdr known) (pid-identity pid)))
       (system (format "kill -~a ~a 2>/dev/null" sig pid))
       'sent)
      (else
       (printf "NOTE ~a to ~a not sent: already gone\n" sig pid)
       'already-gone))))

;; (pid-of-file path) -> the integer a pid file holds, or #f when the file is
;; absent, empty or holds something else.
(define (pid-of-file path)
  (guard (e (#t #f))
    (let ((d (call-with-input-file path read)))
      (and (integer? d) d))))
