#!chezscheme
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

;; Piece 1: atomic-write!, segment naming/enumeration, the manifest.
;; Every row has a control -- an assertion that only fires when the
;; thing under test is wrong, paired with one that must keep passing.
(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace))
;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   want ~s" expect)))))
(define (raises? t) (guard (e (#t #t)) (t) #f))
(define d "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/log1work")
(system (string-append "rm -rf " d "; mkdir -p " d "/writers/k3m9x2qa " d "/writers/c9xq01mz"))

(printf "== segment names ==\n")
(want "1 -> 000001.sexp" (segment-file-name 1) "000001.sexp")
(want "42 -> 000042.sexp" (segment-file-name 42) "000042.sexp")
(want "parse back" (segment-file-number "000042.sexp") 42)
(want "the sibling files are NOT segments"
      (map segment-file-number '("owner.sexp" "retired.sexp" "published.sexp" "quarantine.sexp"))
      '(#f #f #f #f))
(want "wrong digit count rejected"
      (map segment-file-number '("00001.sexp" "0000001.sexp" "00000a.sexp" "000000.sexp"))
      '(#f #f #f #f))
(want "zero and negative refused"
      (list (raises? (lambda () (segment-file-name 0)))
            (raises? (lambda () (segment-file-name -1)))
            (raises? (lambda () (segment-file-name 1234567))))
      '(#t #t #t))

(printf "== atomic-write! ==\n")
(define target (string-append d "/meta.sexp"))
(atomic-write! target (string->utf8 "(format 1)\n") 'snapshot)
(want "file has the content"
      (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "(format 1)\n")
(atomic-write! target (string->utf8 "(format 2)\n") 'snapshot)
(want "replacement replaces"
      (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "(format 2)\n")
(want "no temporary left behind on success"
      (filter (lambda (f) (and (> (string-length f) 4) (string=? (substring f 0 4) "meta")))
              (directory-list d))
      '("meta.sexp"))
;; THE DIRECTORY FLUSH IS INVISIBLE TO EVERY FUNCTIONAL ASSERTION -- the
;; file reads back correctly whether or not it happened. The trace is
;; the only evidence, so it is asserted rather than printed.
(define trace-lines '())
(define (ops-of text)
  (let loop ((i 0) (acc '()))
    (cond
      ((>= i (string-length text)) (reverse acc))
      ((char=? (string-ref text i) #\()
       (let ((j (let scan ((j (+ i 7))) (if (or (>= j (string-length text))
                                                (char=? (string-ref text j) #\space)) j (scan (+ j 1))))))
         (loop j (cons (substring text (+ i 7) j) acc))))
      (else (loop (+ i 1) acc)))))
(let ((p (open-output-string)))
  (parameterize ((theourgia-trace? #t) (current-error-port p))
    (atomic-write! (string-append d "/probe.sexp") (string->utf8 "x\n") 'snapshot))
  (set! trace-lines (get-output-string p)))
(want "atomic-write! is write, fsync file, fsync directory -- in that order"
      (ops-of trace-lines) '("write" "fsync" "fsync"))
(want "and the last fsync is the DIRECTORY, not the file"
      (let* ((ls (let loop ((i 0) (start 0) (acc '()))
                   (cond ((>= i (string-length trace-lines)) (reverse acc))
                         ((char=? (string-ref trace-lines i) #\newline)
                          (loop (+ i 1) (+ i 1) (cons (substring trace-lines start i) acc)))
                         (else (loop (+ i 1) start acc)))))
             (last (car (reverse ls))))
        (and (> (string-length last) 0)
             (not (let scan ((k 0)) (cond ((> (+ k 4) (string-length last)) #f)
                                          ((string=? (substring last k (+ k 4)) ".tmp") #t)
                                          (else (scan (+ k 1))))))))
      #t)

(printf "== enumeration ==\n")
(define wa (string-append d "/writers/k3m9x2qa"))
(for-each (lambda (n)
            (call-with-port (open-file-output-port (string-append wa "/" (segment-file-name n))
                                                   (file-options no-fail))
              (lambda (p) (put-bytevector p (string->utf8 "x\n")))))
          '(3 1 2))
;; sibling files that must not be counted
(for-each (lambda (f)
            (call-with-port (open-file-output-port (string-append wa "/" f) (file-options no-fail))
              (lambda (p) (put-bytevector p (string->utf8 "()\n")))))
          '("owner.sexp" "quarantine.sexp"))
(want "ascending, and only the segments" (enumerate-segment-files d "k3m9x2qa") '(1 2 3))
(want "writers are found" (store-writers d) '("c9xq01mz" "k3m9x2qa"))
(want "a store with no writers dir" (store-writers "/nonexistent") '())

(printf "== manifest ==\n")
(want "missing manifest is #f, not ()" (read-manifest d "k3m9x2qa") #f)
(write-manifest! d "c9xq01mz" '((1 . "aa") (2 . "bb")))
(want "round trip" (read-manifest d "c9xq01mz") '((1 . "aa") (2 . "bb")))
(want "segments of it" (manifest-segments (read-manifest d "c9xq01mz")) '(1 2))
(want "empty manifest is () and not #f"
      (begin (write-manifest! d "c9xq01mz" '()) (read-manifest d "c9xq01mz")) '())
(want "descending or duplicate entries refused"
      (list (raises? (lambda () (write-manifest! d "c9xq01mz" '((2 . "b") (1 . "a")))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 . "a") (1 . "b")))))
            (raises? (lambda () (write-manifest! d "c9xq01mz" '((1 . 5))))))
      '(#t #t #t))

(printf "== which segments participate ==\n")
;; THE SELECTION RULE IS NOT TESTED HERE ANY MORE. loadable-segments and
;; current-segment-number answered "which segments count" and "where does
;; an append go" beside discover-prefix, which owns both -- and they
;; answered differently: with segment 2 listed but absent, loadable-
;; segments returned (1 3) while discovery stops the extent at 1. Nothing
;; called them, so they were a bypass waiting for a caller rather than a
;; second opinion anyone had acted on. They are gone, and each row that
;; stood here names its successor:
;;   "an unlisted segment file is IGNORED"  -> log6, L4'(a), through the
;;      extent, with the control that listing it makes it count
;;   "the local writer needs no manifest"   -> log6's local-writer extent
;;   "current is the highest"               -> log9 C7-1, which asserts
;;      physical-current, the field that actually names the append target
;;   listed-but-missing                     -> log9 C8-1
(write-manifest! d "c9xq01mz" '((1 . "aa")))
(for-each (lambda (n)
            (call-with-port (open-file-output-port
                              (string-append d "/writers/c9xq01mz/" (segment-file-name n))
                              (file-options no-fail))
              (lambda (p) (put-bytevector p (string->utf8 "x\n")))))
          '(1 2))
(want "CONTROL: both files really exist"
      (enumerate-segment-files d "c9xq01mz") '(1 2))
(want "a corrupt manifest raises a log-error, not a parse error"
      (begin
        (call-with-port (open-file-output-port (string-append d "/writers/c9xq01mz/published.sexp")
                                               (file-options no-fail no-truncate))
          (lambda (p) (put-bytevector p (string->utf8 "(oops"))))
        (guard (e ((log-error? e) (list 'log-error (log-error-kind e) (log-error-writer e))))
          (read-manifest d "c9xq01mz") 'no-error))
      '(log-error manifest "c9xq01mz"))
(printf "\n~a failures\n" bad)
;; A run that did not reach here is not a pass; the runner greps for it.
(printf "log1 complete\n")
