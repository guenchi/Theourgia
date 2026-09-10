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
(import (chezscheme) (theourgia ffi))
(define dir "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/ffiwork")
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define log (string-append dir "/000001.sexp"))
(define lock (string-append dir "/lock"))

(printf "-- ensure/open/write/fsync/size --\n")
(define fd (fd-open log '(read-write create append)))
(printf "fd=~a\n" (> fd 0))
(printf "write-all -> ~a\n" (write-all! fd (string->utf8 "abcdef\n") "log"))
(fsync! fd "log")
(fd-close fd)
(printf "file-size -> ~a\n" (file-size log))

(printf "-- append flag really appends --\n")
(let ((fd (fd-open log '(read-write append))))
  (write-all! fd (string->utf8 "ghij\n") "log")
  (fsync! fd "log")
  (fd-close fd))
(printf "file-size -> ~a\n" (file-size log))
(printf "contents ~s\n" (let ((p (open-file-input-port log)))
                          (let ((b (get-bytevector-all p))) (close-port p) (utf8->string b))))

(printf "-- ftruncate to last newline --\n")
(let ((fd (fd-open log '(read-write))))
  (ftruncate! fd 7 "log")
  (fsync! fd "log")
  (printf "fd-size -> ~a\n" (fd-size fd "log"))
  (fd-close fd))
(printf "file-size -> ~a\n" (file-size log))

(printf "-- fsync-dir --\n")
(fsync-dir! dir)
(printf "ok\n")

(printf "-- lock held across body, released on exception --\n")
;; The lock file is created once, at store init -- the lock helpers
;; deliberately do not create it, so that a reader cannot.
(file-ensure! lock)
(with-exclusive-lock lock (lambda (lfd) (printf "in exclusive, lfd int? ~a\n" (integer? lfd))))
(guard (e (#t (printf "escaped with: ~a\n" (condition? e))))
  (with-exclusive-lock lock (lambda (lfd) (error 'test "boom"))))
(with-exclusive-lock lock (lambda (lfd) (printf "re-acquired after exception: yes\n")))
(with-shared-lock lock (lambda (lfd) (printf "shared ok\n")))

(printf "-- errors carry op and errno --\n")
(guard (e ((fs-error? e)
           (printf "op=~a target=~a errno=~a\n"
                   (fs-error-op e) (fs-error-target e) (fs-error-errno e))))
  (fd-open (string-append dir "/no-such-file") '(read)))
(guard (e ((fs-error? e)
           (printf "bad fd: op=~a errno=~a\n" (fs-error-op e) (fs-error-errno e))))
  (fsync! 9999 "nope"))

(printf "-- igropyr durable-error? accepts our shape --\n")
(printf "~a\n"
        (guard (e (#t ((let () (import (only (igropyr durable) durable-error?)) durable-error?) e)))
          (fd-open (string-append dir "/nope") '(read))))

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "smoke-ffi complete\n")
