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

;; ONE STAT, FIVE FIELDS, BY PATH AND BY DESCRIPTOR.
;;
;; ffi.sc's path-stat (stat) and fd-stat (fstat) answer (device inode size
;; mtime ctime), each time an exact pair (seconds . nanoseconds) read at the
;; platform row's measured offsets. PS-1: on one file the two agree, and
;; agree with the readers already there (path-device-inode, path-version).
;; PS-2: the times are exact pairs, nanoseconds within a second. PS-3: after
;; another file is renamed over the path, the path's stat names the new file
;; while the open descriptor's still names the one it opened. PS-4: an absent
;; path is unreadable-entry ENOENT.
;;
;; The two are looked up by eval, so a tree without them loads this file and
;; its rows read red with the reason.

(import (chezscheme) (theourgia ffi))

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

(define (path-stat* . args) (apply (eval 'path-stat (environment '(theourgia ffi))) args))
(define (fd-stat* . args) (apply (eval 'fd-stat (environment '(theourgia ffi))) args))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/path-fd-stat-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "'"))
(define (under name) (string-append root "/" name))
(define (write-text! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (class-of thunk)
  (guard (e ((unreadable-entry? e) (list 'unreadable (unreadable-entry-errno e)))
            (#t (list 'other (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
    (thunk) 'returned))
;; (device inode msec mnsec csec cnsec size): path-version's shape, from a stat
(define (as-version st)
  (list (list-ref st 0) (list-ref st 1)
        (car (list-ref st 3)) (cdr (list-ref st 3))
        (car (list-ref st 4)) (cdr (list-ref st 4))
        (list-ref st 2)))

(define one (under "one"))
(write-text! one "twelve bytes")

(want "PS-1 one file: the path's stat and the descriptor's agree, the size is the bytes written, device and inode are path-device-inode's, and the times and size are path-version's"
      (let* ((fd (fd-open one '(read)))
             (by-path (path-stat* one))
             (by-fd (dynamic-wind (lambda () #f) (lambda () (fd-stat* fd)) (lambda () (fd-close fd)))))
        (let-values (((dev ino) (path-device-inode one)))
          (list (equal? by-path by-fd)
                (list-ref by-path 2)
                (equal? (list (list-ref by-path 0) (list-ref by-path 1)) (list dev ino))
                (equal? (as-version by-path) (path-version one)))))
      '(#t 12 #t #t))

(want "PS-2 the times are exact pairs, seconds and nanoseconds, the nanoseconds within one second"
      (let ((st (path-stat* one)))
        (map (lambda (t)
               (and (pair? t) (integer? (car t)) (exact? (car t)) (integer? (cdr t)) (exact? (cdr t))
                    (<= 0 (cdr t) 999999999)))
             (list (list-ref st 3) (list-ref st 4))))
      '(#t #t))

(define a (under "a"))
(define b (under "b"))
(write-text! a "old")
(write-text! b "newer!")
(want "PS-3 a file renamed over the path: the path's stat names the new file (another inode, its size), the open descriptor's still the one it opened"
      (let* ((fd (fd-open a '(read))))
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (let ((opened (list-ref (fd-stat* fd) 1)))
              (rename-over! b a)
              (let ((by-path (path-stat* a)) (by-fd (fd-stat* fd)))
                (list (= (list-ref by-fd 1) opened)
                      (= (list-ref by-path 1) opened)
                      (list-ref by-path 2)
                      (list-ref by-fd 2)))))
          (lambda () (fd-close fd))))
      '(#t #f 6 3))

(want "PS-4 an absent path: unreadable-entry ENOENT"
      (class-of (lambda () (path-stat* (under "nothing"))))
      '(unreadable ENOENT))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\npath-fd-stat complete\n" bad rows)
(exit (if (= bad 0) 0 1))
