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

;; Every branch this tree can expand into, loaded once.
;;
;; `ffi.ss` has one `meta-cond`, on `THEOURGIA_INJECT`, so the library has
;; two forms and only one of them is expanded by any given run. A name
;; defined in the injected branch and again at the top level does not
;; collide until injection is on -- and then the library does not load at
;; all.
;;
;; THAT IS EXACTLY WHAT HAPPENED. A copy brought a `string-contains?` to
;; the top of `ffi.ss` while the injected branch already had one. Every
;; ordinary run stayed green; fourteen fault-injection fixtures failed
;; together, at the end of a full suite, and the first guess at the cause
;; was timing. The fixtures that eventually caught it are real guards, but
;; they are slow ones: they need the whole suite, and they report a
;; symptom several steps from the cause.
;;
;; SO THE BRANCHES ARE LOADED DIRECTLY. This says nothing about whether
;; the code is correct -- only that each form of it can be built at all,
;; which is the question a name collision answers no to, and which nothing
;; else here asks until much later.
(import (chezscheme))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    (system (string-append "rm -rf " path "; mkdir -p " path))
    path))

(define scratch (test-dir "expansion-branches"))
(define bad 0)
(define rows-run 0)

(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (want-1 label (caught got) (caught expect))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))

(define (text-of path)
  (call-with-port (open-file-input-port path) 
    (lambda (p) (utf8->string (get-bytevector-all p)))))

;; THE CHILD IS A SEPARATE PROCESS, because expansion happens once per
;; process and this fixture has already expanded the library its own way.
(define (loads-under? inject)
  (let ((src (string-append scratch "/probe.ss"))
        (out (string-append scratch "/probe.out")))
    (call-with-port (open-file-output-port src (file-options no-fail))
      (lambda (o)
        (put-bytevector o (string->utf8
          (string-append
            "(import (chezscheme) (theourgia ffi) (theourgia log) (theourgia store)\n"
            "        (theourgia wire) (theourgia digest))\n"
            "(display \"LOADED\")(newline)\n")))))
    ;; ⛔ SAY WHICH VARIABLE IS MISSING. Built straight into the command,
    ;; an unset one reaches `string-append` as #f and the row reports
    ;; "~s is not a string" -- which reads as "an expansion branch of this
    ;; tree does not build", a defect in the tree, when the truth is that
    ;; the caller did not source env.sh. Measured twice, once here and
    ;; once by the main session. The diagnostic is part of the check.
    (let ((dirs (getenv "CHEZSCHEMELIBDIRS"))
          (exts (getenv "CHEZSCHEMELIBEXTS")))
      (unless (and (string? dirs) (string? exts))
        (assertion-violation 'expansion-branches
          (string-append "this fixture needs the library path in the environment: "
                         (if (string? dirs) "" "CHEZSCHEMELIBDIRS ")
                         (if (string? exts) "" "CHEZSCHEMELIBEXTS ")
                         "is unset -- source test/env.sh first")
          (list 'CHEZSCHEMELIBDIRS dirs 'CHEZSCHEMELIBEXTS exts)))
      (system (string-append
                (if inject "THEOURGIA_INJECT=on " "env -u THEOURGIA_INJECT ")
                "CHEZSCHEMELIBDIRS=" dirs
                " CHEZSCHEMELIBEXTS='" exts "'"
                " scheme --script " src " > " out " 2>&1")))
    (let ((t (text-of out)))
      (let loop ((i 0))
        (cond ((> (+ i 6) (string-length t)) (list 'did-not-load t))
              ((string=? (substring t i (+ i 6)) "LOADED") #t)
              (else (loop (+ i 1))))))))

(printf "== every expansion branch builds ==\n")
(want "the libraries load with THEOURGIA_INJECT unset"
      (loads-under? #f)
      #t)
(want "the libraries load with THEOURGIA_INJECT=on"
      (loads-under? #t)
      #t)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "expansion-branches complete\n")
