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

;; WHERE A WRITE REQUEST'S TIME GOES, read off the write path's timing seam
;; (ffi.sc, THEOURGIA_TIMING=1; store.sc prints the lines).
;;
;; An import is ONE write request (code-project.sc's import-code calls
;; with-store-write once with every intent), so its cost is read in two
;; parts: the request's phases once each, `(timing request <phase> <us>)`,
;; and each window of 100 intents' phases summed, `(timing intents <n>
;; ((<phase> <us>) ...))`. A cost that grows with the request shows as a
;; rising series.
;;
;; ALWAYS, AND CHEAP: with the variable unset a small import prints no
;; timing line (TC-0); set, it prints every request phase (TC-1).
;;
;; AN OPT-IN FOR THE CURVE: with THEOURGIA_INSERT_COST=1, one synthetic import
;; of 3500 one-line files -- 7000 blocks, a file block and its code block
;; each -- prints the series at 1000, 3000 and 7000 intents and the ratio of
;; the last window to the tenth per phase. The rows assert only the shape;
;; the readings are the deliverable.

(import (chezscheme) (theourgia rpc))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)))) e))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/insert-cost-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define (file-text path)
  (if (file-exists? path) (let ((t (call-with-input-file path get-string-all))) (if (string? t) t "")) ""))
(define (lines-of text)
  (let loop ((p (open-input-string text)) (acc '()))
    (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc))))))
;; The (timing ...) lines of a child's stderr, as data.
(define (timing-lines path)
  (filter (lambda (d) (and (pair? d) (eq? (car d) 'timing)))
          (map (lambda (l) (guard (e (#t #f)) (read (open-input-string l)))) (lines-of (file-text path)))))
(define (write-files! dir n)
  (system (string-append "mkdir -p '" dir "'"))
  (let loop ((k 0))
    (when (< k n)
      (call-with-output-file (string-append dir "/f" (number->string k) ".js")
        (lambda (p) (put-string p (format "function f~a() { return ~a }\n" k k))) 'replace)
      (loop (+ k 1)))))
;; IMPORT IN A CHILD, so the switch, read once when the library loads, is the
;; child's: TIMING is "1" or #f (unset).
(define (import-in-child! tag dir timing)
  (let ((store (string-append root "/" tag "-store")) (err (string-append root "/" tag ".err")))
    (rpc-dispatch store '(init) "test")
    (system (string-append (if timing "THEOURGIA_TIMING=1 " "env -u THEOURGIA_TIMING ")
                           "THEOURGIA_LOCAL=1 scheme --script ../core.sc import-code '" dir "' --store '" store "' --wire"
                           " > '" root "/" tag ".out' 2> '" err "' < /dev/null"))
    (list (file-text (string-append root "/" tag ".out")) (timing-lines err) (file-text err))))
(define (contains? s sub)
  (let ((n (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length s)) #f)
            ((string=? (substring s i (+ i n)) sub) #t)
            (else (loop (+ i 1)))))))
(define request-phases
  '(load-replay replayed-records intents verdict premises datum-src cursor rules
    plan-canonical plan-entries plan-append apply log-end total))
(define (request-value lines phase)
  (let ((l (find (lambda (d) (and (>= (length d) 4) (eq? (cadr d) 'request) (eq? (caddr d) phase))) lines)))
    (and l (cadddr l))))

(define small (string-append root "/small"))
(write-files! small 3)

(let ((r (import-in-child! "unset" small #f)))
  (want "TC-0 with THEOURGIA_TIMING unset the import answers ok and its stderr holds no timing line"
        (list (contains? (car r) "(ok") (contains? (caddr r) "(timing"))
        '(#t #f)))

(let* ((r (import-in-child! "set" small "1"))
       (lines (cadr r)))
  (want "TC-1 with THEOURGIA_TIMING=1 the import answers ok and prints every request phase once, each a count of microseconds"
        (list (contains? (car r) "(ok")
              (map (lambda (ph) (let ((v (request-value lines ph))) (and (integer? v) (>= v 0)))) request-phases))
        (list #t (map (lambda (ph) #t) request-phases))))

(if (not (equal? (getenv "THEOURGIA_INSERT_COST") "1"))
    (printf "SKIP the curve: THEOURGIA_INSERT_COST is not 1 (one import of 7000 blocks; set it to read the series)~%")
    (let* ((big (string-append root "/big"))
           (files 3500))
      (write-files! big files)
      (let* ((r (import-in-child! "curve" big "1"))
             (lines (cadr r))
             (series (filter (lambda (d) (and (>= (length d) 4) (eq? (cadr d) 'intents))) lines))
             (window (lambda (n) (let ((w (find (lambda (d) (= (caddr d) n)) series))) (and w (cadddr w)))))
             (phase-us (lambda (n ph) (let* ((w (window n)) (e (and w (assq ph w)))) (and e (cadr e))))))
        (printf "~%request phases (us):~%")
        (for-each (lambda (ph) (printf "  ~a ~a~%" ph (request-value lines ph))) request-phases)
        (printf "~%per window of 100 intents (us): phase  at 1000  at 3000  at 7000  ratio 7000/1000~%")
        (for-each (lambda (ph)
                    (let ((a (phase-us 1000 ph)) (b (phase-us 3000 ph)) (c (phase-us 7000 ph)))
                      (printf "  ~a ~a ~a ~a ~a~%" ph a b c
                              (if (and (integer? a) (integer? c) (> a 0)) (exact->inexact (/ c a)) 'n/a))))
                  '(refs check resolve deps append reduce applied))
        (want "TC-2 the curve: the import answers ok, and the series has one window per 100 of its 7000 intents, every phase in every window a count of microseconds"
              (list (contains? (car r) "(ok") (length series)
                    (for-all (lambda (d) (for-all (lambda (e) (and (integer? (cadr e)) (>= (cadr e) 0))) (cadddr d))) series))
              (list #t 70 #t)))))

(system (string-append "rm -rf '" root "'"))
(printf "~%~a failures~%rows: ~a~%insert-cost complete~%" bad rows)
(exit (if (= bad 0) 0 1))
