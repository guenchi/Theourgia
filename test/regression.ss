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

;; The defects the review named, each with a case that FAILED before the
;; fix and a control that must keep passing.
(import (chezscheme) (theourgia wire) (theourgia ffi) (theourgia crc32))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

(define bad 0)
(define (want label got expect)
  (unless (equal? got expect)
    (set! bad (+ bad 1))
    (printf "FAIL ~a: got ~s want ~s\n" label got expect))
  (printf "~a ~a -> ~s\n" (if (equal? got expect) "ok  " "FAIL") label got))
(define (raises? thunk) (guard (e (#t #t)) (thunk) #f))

(printf "== ffi: duplicate flags must not become another flag ==\n")
(define dir (test-dir "regwork"))
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define log (string-append dir "/l"))
(let ((fd (fd-open log '(write create append)))) (write-all! fd (string->utf8 "AAA")) (fd-close fd))
(let ((fd (fd-open log '(write append append)))) (write-all! fd (string->utf8 "BBB")) (fd-close fd))
(want "append twice still appends (was: overwrote at offset 0)" (file-size log) 6)

(printf "== ffi: an exception from the body keeps its own type, and frees the lock ==\n")
;; A re-entry guard was tried here and removed: R6RS guard re-raises in
;; the dynamic environment of the original raise, so a clause that
;; DECLINES re-enters every dynamic-wind it unwound -- and the guard
;; then replaced the real error with an assertion violation. This row is
;; what that removal has to keep true.
(define lock (string-append dir "/lock"))
(file-ensure! lock)
(want "a declining inner guard does not change the error"
      (guard (e (#t (and (vector? e) (vector-ref e 0))))
        (guard (e ((and (vector? e) (eq? (vector-ref e 0) 'never-matches)) 'inner))
          (with-exclusive-lock lock
            (lambda (fd) (raise (vector 'durable-error 'write (cons "/x" 5)))))))
      'durable-error)
(want "the lock is free afterwards"
      (with-exclusive-lock lock (lambda (fd) 'fine)) 'fine)

(printf "== wire: a line is one line ==\n")
;; The CRC is COMPUTED over the injected bytes, never typed in: a
;; hand-written checksum would make every one of these read (bad-crc)
;; and the row would pass for the wrong reason.
(define (framed raw)
  (let* ((hex (crc32-hex raw))
         (out (make-bytevector (+ 9 (bytevector-length raw) 1))))
    (bytevector-copy! (string->utf8 hex) 0 out 0 8)
    (bytevector-u8-set! out 8 32)
    (bytevector-copy! raw 0 out 9 (bytevector-length raw))
    (bytevector-u8-set! out (+ 9 (bytevector-length raw)) 10)
    out))
(want "an interior newline is a frame error"
      (decode-line (framed (string->utf8 "(0 0 \"a\" () ())\n(1 2 \"b\" () ())")))
      '(frame-error embedded-newline))
(want "invalid UTF-8 with a correct CRC is a frame error"
      (decode-line (framed (u8-list->bytevector
        (append (bytevector->u8-list (string->utf8 "(0 0 \"")) (list 255)
                (bytevector->u8-list (string->utf8 "\" () ())"))))))
      '(frame-error encoding))
(want "a leading byte-order mark is a frame error"
      (decode-line (framed (u8-list->bytevector
        (append (list 239 187 191)
                (bytevector->u8-list (string->utf8 "(0 0 \"a\" () ())"))))))
      '(frame-error encoding))
(want "a good line still decodes"
      (decode-line (encode-record 1 2 "w" '() '(a))) '(ok 1 2 "w" () (a)))

(printf "== wire: an uninterned symbol is refused, an interned one is not ==\n")
(want "gensym refused" (raises? (lambda () (storable-encode (gensym "foo")))) #t)
(want "interned symbol fine" (storable-decode (storable-encode 'foo)) 'foo)
(want "unsafe interned symbol still encodes"
      (storable-decode (storable-encode (string->symbol "weird sym")))
      (string->symbol "weird sym"))

(printf "== wire: a circular spine raises instead of hanging ==\n")
(want "circular list refused"
      (raises? (lambda () (storable-encode (let ((x (list 'a 'b))) (set-cdr! (cdr x) x) x))))
      #t)
(want "long proper list still fine"
      (length (storable-decode (storable-encode (make-list 5000 #\a)))) 5000)

(printf "== crc32: a stray third argument is refused ==\n")
(want "three range arguments refused"
      (raises? (lambda () (crc32-hex (string->utf8 "a") 0 1 'oops)))
      #t)

(printf "\n~a failures\n" bad)

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "regression complete\n")
