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

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce) (theourgia ffi))
(define bad 0)
(define (want label got expected)
  (if (equal? got expected) (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/code-text-" (number->string (get-process-id))))
(when (file-exists? root) (error 'code-text "Use a fresh root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(rpc-dispatch store '(init) "test")
(define input (string-append root "/input"))
(define output (string-append root "/output"))
(mkdir-p! input)
(define fixtures
  '((scheme "ss") (javascript "js") (typescript "ts") (python "py") (go "go")
    (rust "rs") (c "c") (java "java") (shell "sh") (markdown "md")))
(define (slurp path)
  (guard (e (#t 'missing))
    (call-with-port (open-file-input-port path)
      (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) #vu8() b))))))
(define (write-bytes path b)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p b))))
;; THE VECTORS TRAVEL WITH THE SUITE. They were read from
;; `../theourgos/` -- another repository, and a closed one -- so this
;; fixture could only run on a machine that had it checked out beside
;; this one, and a delivery carrying these files carried no way to run
;; them. They are copied into `test/vectors/` and found from this
;; script's own path, like every other thing a fixture needs.
;;
;; AND AN ABSENT VECTOR IS NAMED. `slurp` answers `missing` for anything
;; it cannot open, so a wrong directory made every comparison below run
;; against the same non-bytevector on both sides.
(define vectors-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) ".")))
    (string-append dir "/vectors")))
(define (vector-bytes name)
  (let* ((path (string-append vectors-dir "/text-" name ".txt"))
         (b (slurp path)))
    (if (bytevector? b) b
        (assertion-violation 'code-text "the language vector is missing" path))))
(define originals
  (map (lambda (e)
         (let* ((name (symbol->string (car e)))
                (b (vector-bytes name)))
           (write-bytes (string-append input "/" name "." (cadr e)) b)
           (cons (car e) b))) fixtures))
(define imported (rpc-dispatch store (list 'import-code input) "test"))
(want "CT-01 all language files import successfully" (rpc-ok? imported) #t)
(define (field b name)
  (let* ((fields (and b (assq 'fields b))) (p (and fields (assq name (cdr fields))))) (and p (cdr p))))
(define (blocks)
  (let ((state (open-and-reduce store)))
    (map (lambda (row) (cons (cadr row) (state-read state (cadr row)))) (state-datum state))))
(define (code-blocks) (filter (lambda (b) (eq? 'code (field (cdr b) 'kind))) (blocks)))
(for-each
  (lambda (e)
    (let ((found (filter (lambda (b) (eq? (car e) (field (cdr b) 'lang))) (code-blocks))))
      (want (string-append "CT-01 " (symbol->string (car e)) " source bytes are exact")
            (map (lambda (b) (field (cdr b) 'src)) found) (list (cdr (assq (car e) originals)))))) fixtures)
(want "CT-07 import does not split definitions heuristically" (length (code-blocks)) 10)
(define before (reduce-applied-cut (open-and-reduce store)))
(want "CT-01 recovery export succeeds" (rpc-ok? (rpc-dispatch store (list 'export-code output) "test")) #t)
(want "CT-01 recovery import succeeds" (rpc-ok? (rpc-dispatch store (list 'import-code output) "test")) #t)
(want "CT-01 unchanged import writes zero business events" (reduce-applied-cut (open-and-reduce store)) before)
(want "CT-01 recovery import preserves IDs" (length (code-blocks)) 10)
(define raw (string-append root "/raw"))
(want "CT-01 raw export succeeds" (rpc-ok? (rpc-dispatch store (list 'export-code raw "--raw") "test")) #t)
(for-each (lambda (e)
            (want (string-append "CT-01 " (symbol->string (car e)) " raw export matches original")
                  (slurp (string-append raw "/" (symbol->string (car e)) "." (cadr e)))
                  (cdr (assq (car e) originals)))) fixtures)
(printf "~a failures\ncode-text complete\n" bad)
(exit (if (zero? bad) 0 1))
