#!r6rs
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

;; A CHILD RECEIVES ITS PARENT'S ENVIRONMENT (F191).
;;
;; spawn-detached! (the daemon's start) and spawn-captured! (an MCP child
;; verb) pass envp explicitly, and the C library's `environ` they used to read
;; is not reachable from Chez on FreeBSD 15. They pass process-environment:
;; the names `exec /usr/bin/env -0` lists through /bin/sh, each with this
;; process's getenv value. These rows read what a child actually receives.
;;
;; A NAME THE PLATFORM'S /bin/sh REMOVES does not reach a child. Measured
;; 2026-10-12 under `env -i` with a known set: none on FreeBSD 15, OLDPWD on
;; macOS (bash as sh). E-5 holds that list as a reading, per platform, and
;; compares it with the C library's environ where that is readable.

(import (chezscheme)
        (only (theourgia ffi) process-environment spawn-captured! spawn-detached! waitpid-status))

(define bad 0)
(define rows 0)
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)
                               (if (and (condition? x) (irritants-condition? x)) (condition-irritants x) '()))))
             e))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/spawn-environment-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "'"))

;; ---- reading a NUL-separated environment -----------------------------------------------------------

(define (bytes-of path)
  (if (file-exists? path)
      (call-with-port (open-file-input-port path)
        (lambda (i) (let ((b (get-bytevector-all i))) (if (eof-object? b) (make-bytevector 0) b))))
      (make-bytevector 0)))
(define (entries-of b)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (bytevector-length b))
           (reverse (if (< start i) (cons (utf8->string (subbytes b start i)) out) out)))
          ((= 0 (bytevector-u8-ref b i)) (loop (+ i 1) (+ i 1) (cons (utf8->string (subbytes b start i)) out)))
          (else (loop (+ i 1) start out)))))
(define (subbytes b from to) (let ((out (make-bytevector (- to from)))) (bytevector-copy! b from out 0 (- to from)) out))
(define (name-of e) (let loop ((i 0)) (if (or (= i (string-length e)) (char=? (string-ref e i) #\=)) (substring e 0 i) (loop (+ i 1)))))
(define (value-of e) (let ((n (string-length (name-of e)))) (if (< n (string-length e)) (substring e (+ n 1) (string-length e)) #f)))
(define (sorted l) (list-sort string<? l))
(define (without-names l names) (filter (lambda (e) (not (member (name-of e) names))) l))
;; A child's exit, waited for.
(define (exited pid)
  (let loop ((i 0))
    (let ((s (waitpid-status pid)))
      (cond (s s) ((> i 1000) 'no-exit) (else (sleep (make-time 'time-duration 10000000 0)) (loop (+ i 1)))))))

;; A value set after the process started, with a space and a newline in it.
(define probe-name "THEOURGIA_F191_PROBE")
(define probe-value "a b\nc")
(putenv probe-name probe-value)
;; Two more, not among E-3's bindings: one plain, one whose value holds =.
(putenv "THEOURGIA_F191_A" "1")
(putenv "THEOURGIA_F191_EQ" "x=y")
(define env (process-environment))

(want "E-1 process-environment: every entry is NAME=VALUE with this process's getenv value, each name once, PATH among them"
      (list (for-all (lambda (e) (equal? (getenv (name-of e)) (value-of e))) env)
            (= (length env) (length (fold-left (lambda (acc e) (if (member (name-of e) acc) acc (cons (name-of e) acc))) '() env)))
            (and (member "PATH" (map name-of env)) #t))
      '(#t #t #t))
(want "E-2 a value set after the start, holding a space and a newline, is there exactly"
      (and (member (string-append probe-name "=" probe-value) env) #t)
      #t)

;; ---- spawn-captured!: an MCP child verb's path -----------------------------------------------------

(define captured-out (string-append root "/captured.env"))
(define bindings (list (cons "THEOURGIA_F191_BOUND" "bound") (cons probe-name "overridden")))
(want "E-3 spawn-captured! of /usr/bin/env -0: the child's environment is the parent's, the bindings replacing their names"
      (let-values (((pid err) (spawn-captured! (list "/usr/bin/env" "-0") bindings #f captured-out (string-append root "/captured.err"))))
        (list (and pid (exited pid))
              (equal? (sorted (entries-of (bytes-of captured-out)))
                      (sorted (append (without-names (process-environment) (map car bindings))
                                      (map (lambda (b) (string-append (car b) "=" (cdr b))) bindings))))))
      '((exit 0) #t))

;; ---- spawn-detached!: the daemon's start -------------------------------------------------------------

(define detached-out (string-append root "/detached.env"))
(want "E-4 spawn-detached! of a shell that execs /usr/bin/env -0: the child's environment is the parent's"
      (let ((pid (spawn-detached! (list "/bin/sh" "-c" (string-append "exec /usr/bin/env -0 > '" detached-out "'")))))
        (list (exited pid)
              (equal? (sorted (entries-of (bytes-of detached-out))) (sorted (process-environment)))))
      '((exit 0) #t))

;; ---- E-6: an oracle that is not process-environment ------------------------------------------
;;
;; E-3 and E-4 compare a child with process-environment itself, so an entry
;; it dropped would be dropped on both sides. These names are checked against
;; this process's getenv directly, in both children's environments.
(define independent '("PATH" "HOME" "THEOURGIA_TEST_ROOT" "THEOURGIA_F191_A" "THEOURGIA_F191_EQ"))
(want "E-6 PATH, HOME, the test root and two values set after the start reach both children as this process's getenv gives them"
      (let ((expected (map (lambda (n) (string-append n "=" (getenv n))) (filter getenv independent))))
        (list (length expected)
              (for-all (lambda (e) (and (member e (entries-of (bytes-of captured-out))) #t)) expected)
              (for-all (lambda (e) (and (member e (entries-of (bytes-of detached-out))) #t)) expected)))
      (list (length (filter getenv independent)) #t #t))
(want "E-6 PREMISE: the two values set after the start, and PATH, are set here"
      (map (lambda (n) (and (getenv n) #t)) '("THEOURGIA_F191_A" "THEOURGIA_F191_EQ" "PATH"))
      '(#t #t #t))

;; ---- E-5: the reading against the C library's environ --------------------------------------------

(define platform
  (let ((m (symbol->string (machine-type))))
    (cond ((let loop ((i 0)) (and (<= (+ i 3) (string-length m)) (or (string=? (substring m i (+ i 3)) "osx") (loop (+ i 1))))) 'darwin)
          ((let loop ((i 0)) (and (<= (+ i 2) (string-length m)) (or (string=? (substring m i (+ i 2)) "fb") (loop (+ i 1))))) 'freebsd)
          (else 'other))))
;; THE NAMES EACH PLATFORM'S /bin/sh REMOVES, as measured (header).
(define removed-by-sh (case platform ((darwin) '("OLDPWD")) ((freebsd) '()) (else #f)))
(define c-environ
  (guard (e (#t #f))
    (and (begin (for-each (lambda (l) (guard (x (#t #f)) (load-shared-object l))) '("libc.dylib" "libc.so.7" "libc.so.6")) #t)
         (foreign-entry? "environ")
         (let ((at (foreign-ref 'void* (foreign-entry "environ") 0)) (width (foreign-sizeof 'void*)))
           (let loop ((i 0) (out '()))
             (let ((p (foreign-ref 'void* at (* i width))))
               (if (= p 0) (reverse out)
                   (loop (+ i 1)
                         (cons (let c ((k 0) (bs '()))
                                 (let ((b (foreign-ref 'unsigned-8 p k)))
                                   (if (= b 0) (utf8->string (u8-list->bytevector (reverse bs))) (c (+ k 1) (cons b bs)))))
                               out)))))))))
(want "E-5 READING: where environ is readable, process-environment is within it and lacks only names this platform's sh removes; FreeBSD has no environ symbol"
      (case platform
        ((freebsd) (if c-environ 'environ-readable 'no-environ-symbol))
        ((darwin) (and c-environ
                       (let ((pe (process-environment)))
                         (list (for-all (lambda (e) (and (member e c-environ) #t)) pe)
                               (for-all (lambda (n) (and (member n removed-by-sh) #t))
                                        (map name-of (filter (lambda (e) (not (member e pe))) c-environ)))))))
        (else 'not-measured))
      (case platform ((freebsd) 'no-environ-symbol) ((darwin) '(#t #t)) (else 'not-measured)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nspawn-environment complete\n" bad rows)
(exit (if (= bad 0) 0 1))
