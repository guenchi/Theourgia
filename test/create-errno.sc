;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; A CREATE THE OS REFUSES CARRIES THE OS'S ERRNO (ffi.sc condition-errno).
;; Chez makes the create (file-create-exclusive!, file-ensure!) and raises most
;; refusals as a bare filename error whose only trace of the errno is the OS's
;; text among its irritants; condition-errno maps that text back through this
;; process's own strerror over the platform's errnos.
;;
;;   CE-0  the map itself: a filename condition carrying strerror(EROFS)'s text
;;         answers EROFS; one carrying a text no errno has answers #f
;;   CE-1  a create under a regular file: ENOTDIR (the same on every platform)
;;   CE-2  a create at the root: EROFS where the root is read-only (macOS);
;;         elsewhere EACCES for a user and created for root
;;   CE-3  CONTROL: a create in a directory made unwritable: EACCES, mapped by
;;         Chez's protection class before the text was ever read; SKIPPED, with
;;         its reason printed, when run as root (uid 0 writes there)
(import (chezscheme)
        (only (theourgia ffi) file-create-exclusive! fs-error? fs-error-errno errno-name errno-text
              condition-errno unlink!))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/create-errno-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))

;; -> the errno's name a create of PATH was refused with, (no-errno) when the
;; refusal carried none, or created when it was not refused (the file is
;; then removed)
(define (create-refusal path)
  (guard (e ((fs-error? e) (let ((n (fs-error-errno e))) (if n (errno-name n) '(no-errno))))
            (#t (list 'other (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
    (and (file-create-exclusive! path)
         (begin (unlink! path) 'created))))
;; a filename condition as Chez raises one, its irritants (path text)
(define (filename-condition text)
  (condition (make-i/o-filename-error "/p")
             (make-message-condition "failed for ~a: ~(~a~)")
             (make-irritants-condition (list "/p" text))))

(define EROFS-text (errno-text (let loop ((n 1)) (if (and (< n 256) (not (eq? (errno-name n) 'EROFS))) (loop (+ n 1)) n))))
(printf "the platform's EROFS text: ~s\n" EROFS-text)
(want "CE-0 a filename condition carrying the OS's text for EROFS answers EROFS; one carrying a text no errno has answers no errno"
      (list (errno-name (condition-errno (filename-condition EROFS-text)))
            (condition-errno (filename-condition "no errno is called this")))
      '(EROFS #f))

(define plain (string-append root "/plain"))
(call-with-output-file plain (lambda (p) (display "x" p)))
(want "CE-1 a create under a regular file is refused with ENOTDIR"
      (create-refusal (string-append plain "/x"))
      'ENOTDIR)

(define at-root (string-append "/.create-errno-" (number->string (get-process-id))))
(define root-reading (create-refusal at-root))
(define macos? (zero? (system "test \"$(uname -s)\" = Darwin")))
;; uid 0 writes a directory whose mode is 555 and the root alike
(define root-user? (zero? (system "test \"$(id -u)\" = 0")))
(printf "CE-2 a create at the root reads ~s (~a)\n" root-reading (if macos? "macOS, a read-only system volume" "not macOS"))
(want "CE-2 a create at the root: EROFS on macOS's read-only root; elsewhere EACCES for a user, created when run as root"
      root-reading
      (cond (macos? 'EROFS) (root-user? 'created) (else 'EACCES)))

(if root-user?
    (printf "SKIP CE-3: run as root (uid 0 writes a directory whose mode is 555, so no refusal is made); run as another user to check it\n")
    (let ((closed (string-append root "/closed")))
      (system (string-append "mkdir -p '" closed "' && chmod 555 '" closed "'"))
      (let ((closed-reading (create-refusal (string-append closed "/x"))))
        (system (string-append "chmod 755 '" closed "'"))
        (want "CE-3 CONTROL: a create in a directory made unwritable is refused with EACCES (Chez's protection class)"
              closed-reading 'EACCES))))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ncreate-errno complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
