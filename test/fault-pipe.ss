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
;; 4 MB through a fifo with a draining reader. Unfaulted, macOS takes it
;; in one write; with short-write armed the first call is offered seven
;; bytes, so the continuation branch -- the one that copies the tail and
;; re-enters -- runs for real, and out.dat says whether it copied the
;; right bytes.
(import (chezscheme) (theourgia ffi))
;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
(define dir (cadr (command-line)))
(system (string-append "rm -rf " dir "; mkdir -p " dir "; mkfifo " dir "/f"))
(system (string-append "cat " dir "/f > " dir "/out.dat &"))
(define n 4000000)
(define payload
  (let ((bv (make-bytevector n)))
    (let loop ((i 0) (x 987654321))
      (if (= i n) bv
          (let ((x (modulo (+ (* x 1103515245) 12345) 2147483648)))
            (bytevector-u8-set! bv i (modulo (quotient x 65536) 256))
            (loop (+ i 1) x))))))
(define fd (fd-open (string-append dir "/f") '(write)))
(parameterize ((theourgia-trace? #t))
  (printf "returned ~a of ~a\n" (write-all! fd payload) n))
(fd-close fd)
(sleep (make-time 'time-duration 0 1))
(let ((p (open-file-input-port (string-append dir "/out.dat"))))
  (let ((got (get-bytevector-all p)))
    (close-port p)
    (printf "out.dat ~a bytes, identical ~a\n"
            (bytevector-length got) (equal? got payload))))
