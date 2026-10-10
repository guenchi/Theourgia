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

;; What the daemon will and will not remove from its socket path.
;;
;; NEVER: IT UNLINKS ONLY A SOCKET, and the test for that is `stat`'s type
;; bits. It used to be "a regular file or a directory is somebody else's,
;; everything else may go" -- and those are not complements. A fifo is
;; neither, measured, so it fell through and was removed.
;;
;; NOTE: AND THE OFFSET OF `st_mode` IS PER PLATFORM, not derivable at run
;; time. `file-is-socket?` carries three of them. The first section here
;; is what catches a wrong one: four kinds of thing on one path, each
;; asked about. On a platform whose offset is wrong these rows fail,
;; instead of the daemon silently deleting something it does not own.

(import (chezscheme)
        (only (theourgia ffi) file-is-socket? file-is-regular? file-is-directory?))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/dsock-" (number->string (get-process-id))))
(define sock-here (string-append socket-base "/dsock-" (number->string (get-process-id))))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(system (string-append "rm -rf " here " " sock-here "; mkdir -p " here "/store " sock-here))

;; ---- the predicate, against all four kinds ------------------------------------
;;
;; NEVER: ALL FOUR, ON ONE PATH, IN TURN. Asking only about a socket would
;; pass for a predicate that answers #t to everything.
(define (kinds-of name make)
  (let ((p (string-append sock-here "/" name)))
    (system (string-append "rm -rf " p))
    (system (make p))
    (list (file-is-socket? p) (file-is-regular? p) (file-is-directory? p))))

(want "DS-1 a socket is a socket"
      (kinds-of "s" (lambda (p) (string-append
                                  "python3 -c \"import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1])\" " p)))
      '(#t #f #f))

;; KEY: THE ROW THE CHANGE IS FOR. A fifo answers #f to BOTH of the
;; questions the old guard asked, so it was treated as removable.
(want "DS-1 a fifo is none of the three, which is why the old test let it through"
      (kinds-of "f" (lambda (p) (string-append "mkfifo " p)))
      '(#f #f #f))

(want "DS-1 a regular file is a regular file"
      (kinds-of "r" (lambda (p) (string-append "touch " p)))
      '(#f #t #f))

(want "DS-1 a directory is a directory"
      (kinds-of "d" (lambda (p) (string-append "mkdir -p " p)))
      '(#f #f #t))

;; ---- and what the daemon does with each ---------------------------------------
;;
;; NOTE: A SHORT PATH: `sun_path` holds 104 bytes, and this suite's usual
;; scratch directory is longer -- a daemon there reports `listener-down`
;; and looks broken for a reason that has nothing to do with this row.
(define (serve-onto path)
  (let ((out (string-append here "/serve.txt")))
    (system (string-append "rm -f " out))
    ;; NOTE: THE ASSIGNMENTS GO INSIDE THE SUBSHELL. `VAR=x ( cmd & )` is a
    ;; syntax error, and the shell reports it on stderr while `system`
    ;; returns as though something ran -- the rows then read "the daemon
    ;; said nothing", which looks like a daemon that failed to start.
    (system (string-append
              "( CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
              "scheme --script ../theourgiad.sc serve " here "/store --socket " path
              " > " out " 2>&1 & echo $! > " here "/pid )"))
    (system "sleep 4")
    (let ((said (file-text out)))
      (system (string-append "kill $(cat " here "/pid) 2>/dev/null; sleep 1"))
      said)))

(system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                       "scheme --script ../core.sc init --store " here "/store --wire > /dev/null 2>&1"))

;; NEVER: A FIFO ON THE PATH IS NOT THIS DAEMON'S TO REMOVE. Before the
;; change it was unlinked and the daemon started on top of it.
(define fifo (string-append socket-base "/dsf-" (number->string (get-process-id))))
(system (string-append "rm -f " fifo "; mkfifo " fifo))
(define fifo-said (serve-onto fifo))

(want "DS-2 a fifo on the socket path is refused, and survives"
      (list (if (contains? fifo-said "serve-path-occupied") 'refused (list 'said fifo-said))
            (if (file-exists? fifo) 'and-still-there 'DELETED-IT))
      '(refused and-still-there))

(system (string-append "rm -f " fifo))

;; NEVER: TWIN: AND A STALE SOCKET IS STILL CLEARED. Without this row the one
;; above is satisfied by a daemon that refuses every path it finds
;; occupied -- which would mean a crashed daemon's leftover socket stopped
;; the next one for ever.
(define stale (string-append socket-base "/dss-" (number->string (get-process-id))))
(system (string-append "rm -f " stale))
(system (string-append "python3 -c \"import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1])\" " stale))

(want "DS-2 TWIN: a leftover socket nobody holds is cleared and served"
      (let ((said (serve-onto stale)))
        (if (contains? said "(serving ") 'took-it-over (list 'said said)))
      'took-it-over)

(system (string-append "rm -f " stale))

;; NEVER: AND A REGULAR FILE IS STILL REFUSED, which is the case the original
;; guard was written for and must not be lost.
(define plain (string-append socket-base "/dsp-" (number->string (get-process-id))))
(system (string-append "rm -f " plain "; touch " plain))

(want "DS-2 a regular file on the socket path is refused, and survives"
      (let ((said (serve-onto plain)))
        (list (if (contains? said "serve-path-occupied") 'refused (list 'said said))
              (if (file-exists? plain) 'and-still-there 'DELETED-IT)))
      '(refused and-still-there))

;; ---- DS-3 a daemon that cannot bind never said it was serving ------------
;;
;; KEY: THE ABSENCE IS THE ASSERTION. `(serving ...)` used to be reported
;; when the listener PROCESS was spawned, which is before `listen!` is
;; called -- so a daemon that could not bind printed
;;
;;   (serving (store "...") (socket "..."))
;;   (exiting (reason listener-down))
;;
;; in that order, and the first line is the one a caller waits for and
;; believes. It is now reported on the `bound` message the listener sends
;; after `listen!` returns.
;;
;; NEVER: A ROW THAT ONLY LOOKED FOR THE `exiting` LINE WOULD STILL PASS with
;; the old order: both lines were there. What has to be true is that the
;; first one is NOT.
;;
;; NOTE: AND THE PATH HAS TO BE ONE THAT FAILS AT `bind`, NOT ONE THAT IS
;; REFUSED BEFORE IT. Written first against a REGULAR FILE, this row
;; stayed green under the old order -- an occupied path is refused before
;; any listener is spawned, so `serving` was never printed either way and
;; the row measured nothing. The seed is what said so. A path LONGER THAN
;; `sun_path` passes every check the daemon makes and then fails in the
;; kernel, which is the shape this row needs.
(define too-long
  (string-append sock-here "/"
                 (let build ((n 120) (out "")) (if (zero? n) out (build (- n 1) (string-append out "d"))))))

(want "DS-3 a start that cannot bind never announces that it is serving"
      (let ((said (serve-onto too-long)))
        (list (if (contains? said "(serving") 'ANNOUNCED-IT-ANYWAY 'never-said-it)
              (if (contains? said "listener-down") 'and-said-why (list 'said said))))
      '(never-said-it and-said-why))

;; TWIN: without it, "no serving line" is also true of a daemon that
;; printed nothing at all, and of a check reading the wrong file.
(want "DS-3 TWIN: a start that does bind announces it"
      (let ((said (serve-onto (string-append sock-here "/announced.sock"))))
        (if (contains? said "(serving") 'said-it (list 'said said)))
      'said-it)

(system (string-append "rm -f " plain))
(system (string-append "rm -rf " here " " sock-here))
(printf "rows: ~a\n~a failures\ndaemon-socket complete\n" rows bad)
(exit (if (zero? bad) 0 1))
