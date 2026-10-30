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

;; A PROBE DURING A PUBLICATION IN PROGRESS ASKS FOR NO RELOAD (daemon.sc,
;; probe-for-outside-change! and store-loop's busy!).
;;
;; The store process is the only publisher. While it handles a message -- a
;; request whose commit has written its records and not yet published, a
;; reload or a timer's probe that has folded and not yet published -- the
;; disk is ahead of the publication by the daemon's own work. A
;; connection's or a writer's read probes, finds that difference, and used
;; to send (reload); after the publication the store process folded the
;; same state again: a redundant fold under the lock and a revision with no
;; change. Now the store process marks itself busy around every clause of
;; its loop, and a probe that finds it busy sends nothing and traces
;; reload-deferred.
;;
;; THE WINDOW IS MADE BY A HOLD, NOT BY TIMING. The store process is held
;; between a fold and its publication (reload-before-publish) or between a
;; commit's records and its publication (commit-before-publish), and a
;; conn-local read is asked during the hold. The hold sleeps through
;; igropyr's sleep-ms, so the connection runs; without any yield in the
;; fold the window exists only while held.
;;
;; THE COUNTS ARE THE DAEMON'S TRACE LINES from just before the event that
;; starts a fold to the quiet point: no new `(trace published` line for
;; quiet-ms, bounded by quiet-limit-ms. A redundant reload follows its
;; predecessor at once (it is already in the mailbox), well inside quiet-ms.
;;
;; - YA-10, the reload path: an outside commit, a read (it asks for the
;;   reload), the fold held, a read during the hold, release. Then a second
;;   outside commit and a read: the busy mark was cleared, so it reloads.
;; - YA-10a, the commit path: a commit through the daemon held after its
;;   records, a read during the hold, release.
;; - YA-10b, the timer path: a subscriber is live, so the prober ticks; an
;;   outside commit with no read; the tick folds and is held; a read during
;;   the hold; release.
;; - YA-10c, nothing is lost: a second outside commit while the fold is
;;   held (after it sampled and folded). The publication about to be
;;   installed predates it, so the first read after the release asks again
;;   and the next publication has it; with a subscriber live the stream's
;;   next frame carries it with no read at all.
;;
;; THE COMMIT SEAM IS NEW. A tree whose ffi.sc does not know
;; commit-before-publish refuses THEOURGIA_HOLD at load, so YA-10a's daemon
;; never starts there; the row then prints the daemon's refusal as the
;; reason and reads red, which is the seam missing, not a reading of the
;; defect.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia wire) encode-record storable-encode))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a -> ~s\n" label got)
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
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((text (call-with-input-file path get-string-all)))
        (if (string? text) text ""))))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (count-of text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) m) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i n) (+ k 1)))
            (else (loop (+ i 1) k))))))

;; ---- daemons ------------------------------------------------------------------
;;
;; Each daemon is this program's child, started from a runner file that
;; initialises its store and serves it, traced, with injection on and the
;; given environment. THEOURGIA_PROBE=off unless the row is about the timer:
;; a tick would fold an outside change on its own schedule and race the
;; row's read.
(define daemon-count 0)
(define all-runners '())
(define (start-daemon! tag env) (start-daemon* tag (string-append "THEOURGIA_PROBE=off " env)))
(define (start-timer-daemon! tag env) (start-daemon* tag (string-append "env -u THEOURGIA_PROBE " env)))
;; -> the daemon, or (no-daemon <log text>) when its socket never appears.
(define (start-daemon* tag env)
  (set! daemon-count (+ daemon-count 1))
  (let* ((t (string-append pid-text "-" (number->string daemon-count) "-" tag))
         (st (string-append scratch-base "/pp-" t))
         (sk (string-append socket-base "/pp-" t ".sock"))
         (rn (string-append scratch-base "/pp-" t ".sc"))
         (lg (string-append scratch-base "/pp-" t ".log")))
    (system (string-append "rm -rf " st " " sk "; mkdir -p " st))
    (set! all-runners (cons rn all-runners))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
                (string-append "(rpc-dispatch \"" st "\" '(init) \"tester\")")
                (string-append "(serve \"" st "\" \"" sk "\")"))))
      'truncate)
    (system (string-append "env THEOURGIA_TRACE=1 THEOURGIA_INJECT=on " env " "
                           "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " sh -c 'scheme --script " rn " > " lg " 2>&1' &"))
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg))
            ((> k 400) (list 'no-daemon (file-text lg)))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (daemon? d) (and (pair? d) (string? (car d))))
(define (d-store d) (list-ref d 0))
(define (d-socket d) (list-ref d 1))
(define (d-log d) (file-text (list-ref d 3)))
(define (stop-daemon! d)
  (when (daemon? d) (system (string-append "pkill -f " (list-ref d 2) " 2>/dev/null"))))
(define (not-a-reading! d)
  (printf "NOT A READING: a daemon never created its socket in 20 s~%~a~%" (cadr d))
  (for-each (lambda (rn) (system (string-append "pkill -f " rn " 2>/dev/null"))) all-runners)
  (printf "publication-pending complete~%")
  (exit 2))

(define (line-complete? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))
(define (envelope store verb args)
  (string-append "(request 1 " (format "~s" store) " \"tester\" #f wire #f #f " (symbol->string verb)
                 (apply string-append (map (lambda (a) (string-append " " (format "~s" a))) args))
                 ")\n"))
;; The answer's stdout as a datum, or (transport ...) when none came.
(define (unwrap-answer text)
  (let ((datum (guard (e (#t #f)) (read (open-string-input-port text)))))
    (if (and (pair? datum) (eq? 'answer (car datum)))
        (let ((hit (assq 'stdout (cdr datum))))
          (if (and (pair? hit) (string? (cadr hit)))
              (let ((inner (guard (e (#t #f)) (read (open-string-input-port (cadr hit))))))
                (or inner (cadr hit)))
              datum))
        (list 'not-an-answer text))))
(define (ask d verb . args)
  (let ((r (exchange (d-socket d) (string->utf8 (envelope (d-store d) verb args)) line-complete? 15000)))
    (if (and (pair? r) (eq? 'answer (car r)))
        (unwrap-answer (utf8->string (cadr r)))
        (list 'transport r))))
(define (head-of a) (if (pair? a) (car a) a))
;; A conn-local read: the connection answers it from the publication after
;; its probe.
(define (poke! d) (ask d 'outline))
;; Whether an answer names TITLE.
(define (names? a title) (contains? (format "~s" a) title))

;; An outside commit: another writer's record published into the store, a
;; section titled TITLE.
(define (outside! d writer title)
  (let ((bytes (encode-record 1 1789000000001 "peer" '()
                              (storable-encode (list 'put (list '(kind . section) (cons 'title title) '(parent . root) '(ord . 9)))))))
    (log-publish! (d-store d) writer 1 bytes (segment-sha bytes))))

;; ---- holds and counts ----------------------------------------------------------

(define (hold-file tag) (string-append scratch-base "/pp-" pid-text "-" tag ".release"))
(define (await-file path ms)
  (let loop ((k 0))
    (cond ((file-exists? path) #t)
          ((> (* k 50) ms) #f)
          (else (sleep-ms 50) (loop (+ k 1))))))
(define (release! rel) (call-with-output-file rel (lambda (p) (write 'go p)) 'truncate))
(define (rearm! rel) (system (string-append "rm -f " rel " " rel ".held")))
(define (published-count d) (count-of (d-log d) "(trace published "))
(define (deferred-count d) (count-of (d-log d) "(trace reload-deferred"))
(define quiet-ms 1500)
(define quiet-limit-ms 15000)
;; Waits until no new published line has come for quiet-ms; -> #t, or #f
;; when the publications did not settle within quiet-limit-ms.
(define (await-quiet d)
  (let loop ((last (published-count d)) (still 0) (waited 0))
    (cond ((>= still quiet-ms) #t)
          ((>= waited quiet-limit-ms) #f)
          (else
            (sleep-ms 100)
            (let ((now (published-count d)))
              (if (= now last)
                  (loop now (+ still 100) (+ waited 100))
                  (loop now 0 (+ waited 100))))))))
;; THUNK's effect on the counts: (published deferred settled?).
(define (counted d thunk)
  (let ((p0 (published-count d)) (r0 (deferred-count d)))
    (thunk)
    (let ((settled (await-quiet d)))
      (list (- (published-count d) p0) (- (deferred-count d) r0) settled))))

;; ---- subscribers --------------------------------------------------------------
;;
;; A process that connects, subscribes, and keeps every line it reads; it
;; answers (lines <from>) with them.
(define (spawn-subscriber! d)
  (spawn
    (lambda ()
      (connect! (d-socket d))
      (receive
        (after 4000 (let idle () (receive (`(lines ,from) (send from (list 'lines '(no-connect))) (idle)))))
        (`(connected ,p ,ref)
         (conn-read-start! ref)
         (conn-write! ref (string->utf8 (envelope (d-store d) 'subscribe '("changes" "0"))) 'subscribe)
         (let loop ((acc "") (done '()))
           (receive
             (`(written ,r ,t ,st) (loop acc done))
             (`(data ,r ,bv)
              (let split ((s (string-append acc (utf8->string bv))) (done done))
                (let ((nl (let find ((i 0)) (cond ((>= i (string-length s)) #f)
                                                  ((char=? (string-ref s i) #\newline) i)
                                                  (else (find (+ i 1)))))))
                  (if nl
                      (split (substring s (+ nl 1) (string-length s)) (cons (substring s 0 nl) done))
                      (loop s done)))))
             (`(eof ,r) (loop acc done))
             (`#(DOWN ,w ,why) (loop acc done))
             (`(lines ,from) (send from (list 'lines (reverse done))) (loop acc done)))))))))
(define (sub-lines sub)
  (send sub (list 'lines self))
  (receive (after 3000 '()) (`(lines ,ls) ls)))
;; Waits until a line after the acceptance satisfies OK?, or MS pass.
(define (await-line sub ok? ms)
  (let loop ((k 0))
    (let ((ls (sub-lines sub)))
      (cond ((and (pair? ls) (exists ok? (cdr ls))) #t)
            ((> (* k 100) ms) #f)
            (else (sleep-ms 100) (loop (+ k 1)))))))

(start-scheduler
  (lambda ()
    ;; ---- YA-10 the reload path -----------------------------------------------
    (let* ((rel (hold-file "reload"))
           (_ (rearm! rel))
           (d (start-daemon! "r" (string-append "THEOURGIA_HOLD='reload-before-publish:" rel "'"))))
      (unless (daemon? d) (not-a-reading! d))
      (poke! d)
      (let* ((held #f) (during #f)
             (c (counted d (lambda ()
                             (outside! d "mirrory1" "first-outside")
                             (poke! d)
                             (set! held (await-file (string-append rel ".held") 10000))
                             (set! during (poke! d))
                             (release! rel)))))
        (want "YA-10 PRE the fold is held at reload-before-publish, and a read during the hold answers ok from the old publication"
              (list held (head-of during) (names? during "first-outside")) '(#t ok #f))
        (want "YA-10 a read during a held reload asks for no second reload: one publication and one reload-deferred"
              c '(1 1 #t))
        ;; THE MARK IS CLEARED: a later outside commit and a read get their
        ;; reload (the release file stays, so the hold passes at once).
        (let* ((c2 (counted d (lambda () (outside! d "mirrory2" "second-outside") (poke! d))))
               (after (poke! d)))
          (want "YA-10 AFTER the release a new outside commit and a read reload once, and the read after it has the block"
                (list c2 (names? after "second-outside")) '((1 0 #t) #t))))
      (stop-daemon! d))

    ;; ---- YA-10b the timer path --------------------------------------------------
    (let* ((rel (hold-file "timer"))
           (_ (rearm! rel))
           (d (start-timer-daemon! "t" (string-append "THEOURGIA_HOLD='reload-before-publish:" rel "'"))))
      (unless (daemon? d) (not-a-reading! d))
      (let* ((sub (spawn-subscriber! d))
             (_ (let wait ((k 0)) (unless (or (pair? (sub-lines sub)) (> k 50)) (sleep-ms 100) (wait (+ k 1)))))
             (held #f) (during #f)
             (c (counted d (lambda ()
                             (outside! d "mirrort1" "timer-outside")
                             (set! held (await-file (string-append rel ".held") 10000))
                             (set! during (poke! d))
                             (release! rel)))))
        (want "YA-10b PRE with a subscriber live, the prober's tick folds the outside commit and is held; a read during the hold answers ok from the old publication"
              (list (pair? (sub-lines sub)) held (head-of during) (names? during "timer-outside")) '(#t #t ok #f))
        (want "YA-10b a read during a fold held in the timer's probe asks for no second reload: one publication and one reload-deferred"
              c '(1 1 #t)))
      (stop-daemon! d))

    ;; ---- YA-10c nothing is lost ---------------------------------------------------
    ;; The read path: no subscriber, so only a read asks again.
    (let* ((rel (hold-file "lost"))
           (_ (rearm! rel))
           (d (start-daemon! "c" (string-append "THEOURGIA_HOLD='reload-before-publish:" rel "'"))))
      (unless (daemon? d) (not-a-reading! d))
      (poke! d)
      (outside! d "mirrorc1" "lost-first")
      (poke! d)
      (let* ((held (await-file (string-append rel ".held") 10000))
             (_ (outside! d "mirrorc2" "lost-second"))
             (during (poke! d))
             (_ (release! rel))
             (_ (await-quiet d))
             (_ (poke! d))
             (_ (await-quiet d))
             (after (poke! d)))
        (want "YA-10c a commit made while a fold is held is read after the release: the first read asks again and the next publication has it"
              (list held (names? during "lost-second") (names? after "lost-first") (names? after "lost-second"))
              '(#t #f #t #t)))
      (stop-daemon! d))
    ;; The stream path: a subscriber, and no read after the release.
    (let* ((rel (hold-file "lost-stream"))
           (_ (rearm! rel))
           (d (start-timer-daemon! "s" (string-append "THEOURGIA_HOLD='reload-before-publish:" rel "'"))))
      (unless (daemon? d) (not-a-reading! d))
      (let* ((sub (spawn-subscriber! d))
             (_ (let wait ((k 0)) (unless (or (pair? (sub-lines sub)) (> k 50)) (sleep-ms 100) (wait (+ k 1)))))
             (_ (outside! d "mirrors1" "stream-first"))
             (held (await-file (string-append rel ".held") 10000))
             (_ (outside! d "mirrors2" "stream-second"))
             (_ (poke! d))
             (_ (release! rel))
             (framed (await-line sub (lambda (l) (contains? l "mirrors2")) 10000)))
        (want "YA-10c STREAM with a subscriber live, the commit made while the fold was held reaches the stream with no read after the release"
              (list held framed) '(#t #t)))
      (stop-daemon! d))

    ;; ---- YA-10a the commit path ---------------------------------------------------
    ;; The store's init commits in the runner before it serves, and passes
    ;; the hold there: the release exists at the start and is removed before
    ;; the row's commit.
    (let* ((rel (hold-file "commit"))
           (_ (begin (rearm! rel) (release! rel)))
           (d (start-daemon! "a" (string-append "THEOURGIA_HOLD='commit-before-publish:" rel "'"))))
      (if (not (daemon? d))
          (begin
            (printf "YA-10a NO DAEMON: this tree refused THEOURGIA_HOLD=commit-before-publish (the seam is missing here; the row's red below is that, not a reading of the defect):~%~a~%" (cadr d))
            (want "YA-10a a read during a held commit asks for no reload: one publication and one reload-deferred"
                  'no-daemon '(1 1 #t)))
          (begin
            (poke! d)
            (rearm! rel)
            (let* ((me self) (held #f) (during #f)
                   (c (counted d (lambda ()
                                   (spawn (lambda () (send me (list 'committed (ask d 'insert "--title" "held-commit")))))
                                   (set! held (await-file (string-append rel ".held") 10000))
                                   (set! during (poke! d))
                                   (release! rel))))
                   (committed (receive (after 15000 'no-answer) (`(committed ,a) a))))
              (want "YA-10a PRE the commit is held after its records, a read during the hold answers ok from the old publication, and the commit then answers ok"
                    (list held (head-of during) (names? during "held-commit") (head-of committed)) '(#t ok #f ok))
              (want "YA-10a a read during a held commit asks for no reload: one publication and one reload-deferred"
                    c '(1 1 #t)))
            (stop-daemon! d))))

    (printf "rows: ~a~%~a failures~%publication-pending complete~%" rows bad)
    (exit (if (= bad 0) 0 1))))
