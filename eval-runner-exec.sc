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

;; The runner's launcher: the first of the two stages of `eval --lang`.
;;
;;     scheme --script eval-runner-exec.sc <cpu-seconds> [--env NAME=VALUE]... -- <interpreter argv...>
;;
;; started by the supervisor with its cwd in the projection's tree/ and an
;; environment of PATH, HOME, LANG and the Chez library path, nothing else.
;;
;; NEVER: THE ORDER IS THE WORKER'S, AND IT IS THE SAFETY ARGUMENT:
;;
;;   1. the interpreter is resolved and found executable, or this process
;;      exits 3 having printed nothing. Before ready the supervisor reads
;;      only the exit status, so status 3 there means "no interpreter", and
;;      no interpreter has run: it cannot be forged by the evaluated code;
;;   2. a session of its own and the CPU ceiling (isolate-evaluation!);
;;   3. `(ready <pgid>)`;
;;   4. `go`, read from stdin -- nothing runs before it;
;;   5. the environment is replaced by PATH, HOME and LANG alone, then the
;;      runner's --env pairs are added to it, and the interpreter replaces
;;      this process. If exec returns anyway (the file
;;      changed between the check and the exec), this exits 127 with no
;;      output, which the supervisor reports as the runner's own exit data:
;;      the one answer the evaluated side could also produce.

(import (chezscheme)
        (only (theourgia ffi) isolate-evaluation! path-executable? exec-argv-env!))

(define args (cdr (command-line)))

;; <cpu-seconds> [--env NAME=VALUE]... -- <interpreter> <arg>...
;;
;; NEVER: EVERYTHING AFTER THE FIRST STANDALONE "--" IS THE INTERPRETER'S,
;; VERBATIM. The pairs are read up to it and not beyond, so an interpreter
;; argument spelled "--env" or "--" means nothing here. The pairs are the
;; runner's environment; they are applied after the clear below and never
;; to this program's own startup, whose library path is the calling
;; process's (eval-runner.sc).
(define (usage-exit) (exit 2))
(unless (pair? args) (usage-exit))
(define cpu-seconds (string->number (car args)))
(unless (and cpu-seconds (exact? cpu-seconds) (integer? cpu-seconds) (> cpu-seconds 0)) (usage-exit))
(define-values (env-pairs interpreter-argv)
  (let loop ((rest (cdr args)) (pairs '()))
    (cond ((null? rest) (usage-exit))
          ((string=? (car rest) "--") (values (reverse pairs) (cdr rest)))
          ((and (string=? (car rest) "--env") (pair? (cdr rest))) (loop (cddr rest) (cons (cadr rest) pairs)))
          (else (usage-exit)))))
(when (null? interpreter-argv) (usage-exit))

;; A PAIR IS NAME=VALUE, split at its first "=", with a name that is not
;; empty. One that is not exits 3 before ready: the supervisor never sends
;; one (the runner was checked before it spawned this), and before ready
;; nothing the evaluated side controls has run, so the status is still
;; unforgeable.
(define (pair-valid? p)
  (let loop ((i 0))
    (cond ((= i (string-length p)) #f)
          ((char=? (string-ref p i) #\=) (> i 0))
          (else (loop (+ i 1))))))
(unless (for-all pair-valid? env-pairs) (exit 3))
(define interpreter (car interpreter-argv))

(define (split-colons s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length s)) (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\:) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          (else (loop (+ i 1) start acc)))))

(define (has-slash? s) (memv #\/ (string->list s)))

;; A NAME WITH A SLASH IS A PATH, AS GIVEN; any other name is looked up in
;; each PATH component in order, an empty component meaning the current
;; directory, as the shell does.
(define resolved
  (if (has-slash? interpreter)
      (and (path-executable? interpreter) interpreter)
      (let ((path (getenv "PATH")))
        (let loop ((dirs (if path (split-colons path) '())))
          (cond ((null? dirs) #f)
                (else
                 (let ((candidate (string-append (if (string=? (car dirs) "") "." (car dirs)) "/" interpreter)))
                   (if (path-executable? candidate) candidate (loop (cdr dirs))))))))))

(unless resolved (exit 3))

(define-values (pgid cpu-ceiling cpu-status) (isolate-evaluation! cpu-seconds))

(define protocol (standard-output-port))
(put-bytevector protocol
                (string->utf8 (string-append "(ready " (number->string pgid) ")\n")))
(flush-output-port protocol)

;; NEVER: NOTHING BELOW THIS LINE RUNS UNTIL THE SUPERVISOR SAYS SO.
(define go (read (current-input-port)))
(unless (eq? go 'go) (exit 1))

(define kept-environment
  (let loop ((names '("PATH" "HOME" "LANG")) (acc '()))
    (cond ((null? names) (reverse acc))
          ((getenv (car names))
           => (lambda (v) (loop (cdr names) (cons (string-append (car names) "=" v) acc))))
          (else (loop (cdr names) acc)))))

(guard (e (#t (exit 127)))
  (exec-argv-env! resolved interpreter-argv (append kept-environment env-pairs)))
(exit 127)
