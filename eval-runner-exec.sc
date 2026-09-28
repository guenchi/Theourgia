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
;;     scheme --script eval-runner-exec.sc <cpu-seconds> -- <interpreter argv...>
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
;;   5. the environment is replaced by PATH, HOME and LANG alone, and the
;;      interpreter replaces this process. If exec returns anyway (the file
;;      changed between the check and the exec), this exits 127 with no
;;      output, which the supervisor reports as the runner's own exit data:
;;      the one answer the evaluated side could also produce.

(import (chezscheme)
        (only (theourgia ffi) isolate-evaluation! path-executable? exec-argv-env!))

(define args (cdr (command-line)))

;; <cpu-seconds> -- <interpreter> <arg>...
(define (usage-exit) (exit 2))
(unless (and (>= (length args) 3) (string=? (cadr args) "--")) (usage-exit))
(define cpu-seconds (string->number (car args)))
(unless (and cpu-seconds (exact? cpu-seconds) (integer? cpu-seconds) (> cpu-seconds 0)) (usage-exit))
(define interpreter-argv (cddr args))
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
  (exec-argv-env! resolved interpreter-argv kept-environment))
(exit 127)
