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

;; THE CHANGE STREAM AT THE COMMAND LINE: `theourgia subscribe changes <rev>
;; [<token>]` keeps its connection, prints the acceptance and then every
;; frame as one line each as they arrive, and exits when the stream ends:
;;   0  the stream ended with (error draining) and EOF followed;
;;   1  the acceptance was a refusal (printed);
;;   1  a terminal frame lagging or transport-unknown arrived (printed);
;;   1  EOF came without a terminal frame, or in mid-frame:
;;      (error transport-error lost-stream) is printed.
;;
;; THE READER'S BOUNDARIES ARE OBSERVED, NOT ASSUMED. A stream line can be
;; cut anywhere by the socket, and two lines can arrive in one read. With
;; THEOURGIA_READ_CHUNK every read is bounded, and the client traces each
;; read as (trace read-chunk <bytes> <complete-lines> <tail?>); a row that
;; needs a read holding two lines and a tail looks for that line in the
;; trace and does not count itself green until it has seen one.
;;
;; THE APPLICATION RULE IS BY REVISION. A consumer subscribes first, then
;; reads with --rev; it holds a revision per object it read, applies a
;; frame to an object only when the frame's revision is past the one it
;; holds, rereads on a gap, and starts again when a read's daemon token is
;; not the subscription's. The rows here model that consumer and check that
;; what the daemon answers is enough for it.

(import (chezscheme)
        (only (theourgia client) socket-path serve-log-path))

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
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/csc-" (number->string (get-process-id))))
(define sock-base (or (getenv "THEOURGIA_TEST_SOCK") "/tmp"))
(define (sh . parts) (system (apply string-append parts)))
(sh "rm -rf " root "; mkdir -p " root)
(define run-root (string-append sock-base "/csc-" (number->string (get-process-id))))
(sh "rm -rf " run-root "; mkdir -p " run-root)
(putenv "THEOURGIA_RUN" (string-append run-root "/run"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define (quoted s) (string-append "'" s "'"))
(define (text-of-file p) (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (lines-of-text t)
  (let loop ((i 0) (from 0) (out '()))
    (cond ((= i (string-length t)) (reverse (if (> i from) (cons (substring t from i) out) out)))
          ((char=? (string-ref t i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring t from i) out)))
          (else (loop (+ i 1) from out)))))
(define (has-substring? t n)
  (let ((k (string-length n)) (m (string-length t)))
    (let loop ((i 0)) (cond ((> (+ i k) m) #f) ((string=? (substring t i (+ i k)) n) #t) (else (loop (+ i 1)))))))
(define (datum-of-line l) (guard (e (#t (list 'unreadable l))) (read (open-string-input-port l))))
;; A CLAUSE IS A LIST AFTER THE TAG; an error's second element is a symbol,
;; so the lookup passes over what is not a pair.
(define (clause name datum)
  (let ((c (and (pair? datum) (list? datum)
                (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr datum)))))
    (and c (cdr c))))
(define (wait-until thunk ms)
  (let loop ((k 0)) (cond ((thunk) #t) ((> k (div ms 50)) #f) (else (sh "sleep 0.05") (loop (+ k 1))))))

;; ---- stores, daemons, the thin client --------------------------------------------
(define n 0)
(define (fresh-store!)
  (set! n (+ n 1))
  (let ((st (string-append root "/s" (number->string n))))
    (sh "mkdir -p " st)
    (client "" "init" "--store" st)
    st))
(define (lines-of-command cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (lines-of-text (if (eof-object? t) "" t))))
(define (daemon-pids store)
  (let ((needle (string-append "theourgiad.sc serve " store)))
    (map (lambda (l) (string->number (car (filter (lambda (w) (> (string-length w) 0))
                                                   (let split ((cs (string->list l)) (cur '()) (acc '()))
                                                     (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
                                                           ((char=? (car cs) #\space) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                           (else (split (cdr cs) (cons (car cs) cur) acc))))))))
         (filter (lambda (l) (has-substring? l needle)) (lines-of-command "ps -ax -o pid= -o command=")))))
(define (signal-daemon! store sig)
  (for-each (lambda (pid) (sh "kill -" sig " " (number->string pid) " 2>/dev/null")) (daemon-pids store)))
(define (stop-daemon! store)
  (signal-daemon! store "TERM")
  (wait-until (lambda () (null? (daemon-pids store))) 6000)
  (signal-daemon! store "KILL"))
(define c-n 0)
;; A thin-client command, --wire; -> (rc datum).
(define (client env . args)
  (set! c-n (+ c-n 1))
  (let ((out (string-append root "/c" (number->string c-n) ".out")))
    (let ((rc (sh env " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc "
                  (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                  "--wire > " out " 2> " out ".err < /dev/null")))
      (list rc (let ((ls (lines-of-text (text-of-file out)))) (if (pair? ls) (datum-of-line (car ls)) 'nothing))))))
;; The streaming client in the background; -> (out-path rc-path err-path).
(define (subscribe-bg! env st . args)
  (set! c-n (+ c-n 1))
  (let* ((base (string-append root "/sub" (number->string c-n)))
         (out (string-append base ".out")) (rcf (string-append base ".rc")) (err (string-append base ".err")))
    (sh "( " env " THEOURGIA_TRACE=1 perl -e 'alarm 120; exec @ARGV' scheme --script ../theourgia.sc subscribe changes "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted st) " > " out " 2> " err " < /dev/null; echo $? > " rcf " ) &")
    (list out rcf err)))
;; ONLY COMPLETE LINES: a tail with no newline yet is a datum still being
;; written, and a row must not parse it.
(define (complete-lines-of-text t)
  (let ((ls (lines-of-text t)))
    (if (and (pair? ls) (> (string-length t) 0) (not (char=? (string-ref t (- (string-length t) 1)) #\newline)))
        (reverse (cdr (reverse ls)))
        ls)))
(define (sub-lines s) (complete-lines-of-text (text-of-file (car s))))
(define (sub-trace s) (map datum-of-line (lines-of-text (text-of-file (caddr s)))))
(define (sub-rc s) (let ((t (text-of-file (cadr s)))) (and (> (string-length t) 0) (string->number (car (lines-of-text t))))))
(define (await-sub-lines s k ms) (wait-until (lambda () (>= (length (sub-lines s)) k)) ms) (sub-lines s))
(define (await-sub-exit s ms) (wait-until (lambda () (sub-rc s)) ms) (sub-rc s))
(define (insert! st title) (client "" "insert" "--title" title "--store" st))
(define (new-id r)
  (let* ((a (cadr r)) (ev (and (pair? a) (list? a) (assq 'events (cdr a)))))
    (and ev (let ((e (car (cadr ev)))) (string-append (car e) "." (number->string (cdr e)))))))
(define (rev-of a) (let ((r (clause 'rev a))) (and r (car r))))
(define (token-of a) (let ((r (clause 'rev a))) (and r (pair? (cdr r)) (cadr (cadr r)))))

;; ---- the consumer: the application rule by revision --------------------------
;; held: alist object -> revision. -> (decision . held'), decision one of
;; apply, skip, reread.
(define (consume held object frame-rev)
  (let ((h (cdr (or (assoc object held) (cons object #f)))))
    (cond ((not h) (cons 'reread held))
          ((<= frame-rev h) (cons 'skip held))
          ((= frame-rev (+ h 1)) (cons 'apply (cons (cons object frame-rev) (remp (lambda (e) (equal? (car e) object)) held))))
          (else (cons 'reread held)))))

;; THE FRAMES ARE WHOLE: each line between the acceptance and the terminal is
;; a (changes ...) datum, their revisions consecutive from the acceptance's
;; current, each naming exactly the block its insert made. -> #t, or what
;; differed.
(define (frames-whole? ls ids)
  (let* ((acc (datum-of-line (car ls)))
         (current (let ((c (clause 'current acc))) (and c (car c))))
         (fs (map datum-of-line (cdr ls)))
         (changes (filter (lambda (f) (and (pair? f) (eq? (car f) 'changes))) fs)))
    (or (and current
             (= (length changes) (length ids))
             (let loop ((fs changes) (ids ids) (k (+ current 1)))
               (or (null? fs)
                   (and (equal? (clause 'rev (car fs)) (list k))
                        (equal? (clause 'items (car fs)) (list (list 'added (car ids))))
                        (loop (cdr fs) (cdr ids) (+ k 1))))))
        (list 'frames current (map (lambda (f) (list (clause 'rev f) (clause 'items f))) changes) 'ids ids))))

;; =============================================================================
(printf "== F10-7: the client verb ==~%")
(let* ((st (fresh-store!))
       (_ (insert! st "warm the daemon"))
       (s (subscribe-bg! "" st "0")))
  (await-sub-lines s 1 10000)
  (insert! st "one")
  (insert! st "two")
  (await-sub-lines s 3 10000)
  (signal-daemon! st "TERM")
  (let* ((rc (await-sub-exit s 15000)) (ls (sub-lines s)))
    (want "F10-7 the client prints the acceptance, two frames and the draining terminal as four lines, and exits 0"
          (list rc (length ls)
                (let ((a (datum-of-line (car ls)))) (and (pair? a) (car a)))
                (map (lambda (l) (car (datum-of-line l))) (cdr ls))
                (datum-of-line (list-ref ls 3)))
          (list 0 4 'ok '(changes changes error) '(error draining)))))

;; THE SAME FOUR LINES with every byte its own read.
(let* ((st (fresh-store!))
       (_ (insert! st "warm"))
       (s (subscribe-bg! "THEOURGIA_INJECT=on THEOURGIA_READ_CHUNK=1" st "0")))
  (await-sub-lines s 1 10000)
  (let* ((one (new-id (insert! st "one"))) (two (new-id (insert! st "two"))))
    (await-sub-lines s 3 10000)
    (signal-daemon! st "TERM")
    (let* ((rc (await-sub-exit s 15000)) (ls (sub-lines s))
           (chunks (filter (lambda (d) (and (pair? d) (list? d) (>= (length d) 3) (eq? (car d) 'trace) (eq? (cadr d) 'read-chunk)))
                           (sub-trace s))))
      (want "F10-7 with THEOURGIA_READ_CHUNK=1 the same four lines, the frames whole, exit 0"
            (list rc (length ls) (and (= (length ls) 4) (frames-whole? (reverse (cdr (reverse ls))) (list one two)))
                  (and (= (length ls) 4) (datum-of-line (list-ref ls 3))))
            (list 0 4 #t '(error draining)))
      (want "F10-7 and the seam ran: the client's reads were traced, every one of at most one byte"
            (list (> (length chunks) 4) (for-all (lambda (d) (<= (caddr d) 1)) chunks))
            '(#t #t)))))

(define coalesced-ids '())
;; TWO LINES AND A TAIL IN ONE READ: the daemon writes two complete frames
;; and the first half of a third in one write; the client's trace must show
;; one read holding two complete lines and a tail. Retried up to five
;; times; a run that never observes it is NOT a reading.
(let loop ((try 1))
  (let* ((st (fresh-store!))
         (_ (client "THEOURGIA_INJECT=on THEOURGIA_FAULT=stream-coalesce-cut@conn" "insert" "--title" "warm" "--store" st))
         (s (subscribe-bg! "THEOURGIA_INJECT=on THEOURGIA_FAULT=stream-coalesce-cut@conn THEOURGIA_READ_CHUNK=65536" st "0")))
    (await-sub-lines s 1 10000)
    (set! coalesced-ids (map (lambda (t) (new-id (insert! st t))) '("one" "two" "three")))
    (await-sub-lines s 4 10000)
    (signal-daemon! st "TERM")
    (let* ((rc (await-sub-exit s 15000)) (ls (sub-lines s))
           (observed (exists (lambda (l) (let ((d (datum-of-line l)))
                                           (and (pair? d) (list? d) (= (length d) 5) (eq? (car d) 'trace) (eq? (cadr d) 'read-chunk)
                                                (>= (list-ref d 3) 2) (eq? (list-ref d 4) #t))))
                             (lines-of-text (text-of-file (caddr s))))))
      (cond
        (observed
         (want "F10-7 under stream-coalesce-cut a read held two complete lines and a tail, and the frames are whole"
               (list rc (length ls) (and (= (length ls) 5) (frames-whole? (reverse (cdr (reverse ls))) coalesced-ids))
                     (and (= (length ls) 5) (datum-of-line (list-ref ls 4))))
               (list 0 5 #t '(error draining))))
        ((< try 5) (loop (+ try 1)))
        (else (want "F10-7 under stream-coalesce-cut a read held two complete lines and a tail" 'NO-READING 'observed))))))

;; ---- the application rows ------------------------------------------------------
(let* ((st (fresh-store!))
       (x (new-id (insert! st "x")))
       (s (subscribe-bg! "" st "0")))
  (await-sub-lines s 1 10000)
  (insert! st "r")
  (await-sub-lines s 2 10000)
  (let* ((read (cadr (client "" "read" x "--rev" "--store" st)))
         (held (list (cons x (rev-of read)))))
    (client "" "set" x "src" "after the read" "--store" st)
    (await-sub-lines s 3 10000)
    (let* ((frames (map datum-of-line (cdr (sub-lines s))))
           (revs (map (lambda (f) (car (clause 'rev f))) frames))
           (d1 (consume held x (car revs)))
           (d2 (consume (cdr d1) x (cadr revs))))
      (want "F10-7 a consumer that read after subscribing skips the frame at its rev and applies the next"
            (list (car d1) (car d2)) '(skip apply))))
  (stop-daemon! st))

;; TWO OBJECTS READ AT DIFFERENT REVISIONS are two baselines.
(let* ((st (fresh-store!))
       (a (new-id (insert! st "doc A")))
       (b (new-id (insert! st "doc B")))
       (s (subscribe-bg! "" st "0")))
  (await-sub-lines s 1 10000)
  (let* ((ra (rev-of (cadr (client "" "read" a "--rev" "--store" st))))
         (_ (client "" "set" a "src" "changed" "--store" st))
         (_ (await-sub-lines s 2 10000))
         (rb (rev-of (cadr (client "" "read" b "--rev" "--store" st))))
         (f (datum-of-line (cadr (sub-lines s))))
         (fr (car (clause 'rev f))))
    (want "F10-7 frame n+1 is applied to A read at n and not to B read at n+1"
          (list (car (consume (list (cons a ra)) a fr)) (car (consume (list (cons b rb)) b fr)))
          '(apply skip)))
  (stop-daemon! st))

;; A RESTART: a frame of the old incarnation is buffered; a read answered by
;; the new one carries another token, so the consumer discards the buffered
;; frame, subscribes afresh, and applies the new stream's next frame.
(let* ((st (fresh-store!))
       (x (new-id (insert! st "x")))
       (s (subscribe-bg! "" st "0")))
  (let* ((ls (await-sub-lines s 1 10000))
         (acc (datum-of-line (car ls)))
         (sub-token (let ((c (clause 'daemon acc))) (and c (car c)))))
    (client "" "set" x "src" "before the restart" "--store" st)
    (let* ((buffered (datum-of-line (cadr (await-sub-lines s 2 10000)))))
      (stop-daemon! st)
      (let* ((later (cadr (client "" "read" x "--rev" "--store" st)))
             (restarted? (and (string? sub-token) (string? (token-of later)) (not (equal? sub-token (token-of later)))))
             ;; the consumer's state after the read: the buffered frame is
             ;; the old incarnation's and is not offered to the rule at all
             (held (if restarted? (list (cons x (rev-of later))) (list (cons x (car (clause 'rev buffered))))))
             (s2 (subscribe-bg! "" st "0"))
             (acc2 (datum-of-line (car (await-sub-lines s2 1 10000)))))
        (client "" "set" x "src" "after the restart" "--store" st)
        (let* ((f (datum-of-line (cadr (await-sub-lines s2 2 10000)))))
          (want "F10-7 a read answered by a restarted daemon carries another token; the fresh subscription's token is the read's, and its next frame is applied"
                (list restarted? (equal? (clause 'daemon acc2) (list (token-of later)))
                      (equal? (clause 'daemon f) (list (token-of later)))
                      (car (consume held x (car (clause 'rev f)))))
                (list #t #t #t 'apply))))))
  (stop-daemon! st))

;; A GAP: an object read BEFORE subscribing, with two publications between
;; the read and the subscription; the first frame is two past the held rev,
;; and the consumer reads again.
(let* ((st (fresh-store!))
       (x (new-id (insert! st "x")))
       (held (list (cons x (rev-of (cadr (client "" "read" x "--rev" "--store" st)))))))
  (insert! st "between one") (insert! st "between two")
  (let ((s (subscribe-bg! "" st "0")))
    (await-sub-lines s 1 10000)
    (insert! st "the first frame")
    (let ((f (datum-of-line (cadr (await-sub-lines s 2 10000)))))
      (want "F10-7 a frame more than one past the held rev is a gap: the consumer reads again"
            (car (consume held x (car (clause 'rev f)))) 'reread)))
  (stop-daemon! st))

;; ---- the client's other ends -----------------------------------------------------
(let* ((st (fresh-store!))
       (s (subscribe-bg! "" st "x")))
  (want "F10-7 an invalid revision: exit 1, and the refusal printed"
        (list (await-sub-exit s 15000) (let ((ls (sub-lines s))) (and (pair? ls) (let ((d (datum-of-line (car ls)))) (and (pair? d) (list (car d) (cadr d) (caddr d)))))))
        (list 1 '(error bad-request invalid-revision)))
  (stop-daemon! st))
(let* ((st (fresh-store!))
       (_ (insert! st "warm"))
       (s (subscribe-bg! "" st "0")))
  (await-sub-lines s 1 10000)
  (signal-daemon! st "KILL")
  (want "F10-7 the daemon killed mid-stream: exit 1, and lost-stream printed"
        (list (await-sub-exit s 15000) (datum-of-line (car (reverse (sub-lines s)))))
        (list 1 '(error transport-error lost-stream))))
;; OVERFLOW: the client stopped (SIGSTOP) while the daemon's queue fills
;; past its limit; continued, it prints the lagging terminal and exits 1.
(let* ((st (fresh-store!))
       (_ (client "THEOURGIA_TRACE=1 THEOURGIA_INJECT=on THEOURGIA_SEND_BUFFER=16384" "insert" "--title" "warm" "--store" st))
       (s (subscribe-bg! "" st "0")))
  (await-sub-lines s 1 10000)
  (let ((pids (map (lambda (l) (string->number (car (filter (lambda (w) (> (string-length w) 0))
                                                             (let split ((cs (string->list l)) (cur '()) (acc '()))
                                                               (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
                                                                     ((char=? (car cs) #\space) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                                     (else (split (cdr cs) (cons (car cs) cur) acc))))))))
                   (filter (lambda (l) (and (has-substring? l "theourgia.sc subscribe changes") (has-substring? l st)))
                           (lines-of-command "ps -ax -o pid= -o command=")))))
    (for-each (lambda (p) (sh "kill -STOP " (number->string p))) pids)
    (let loop ((k 0))
      (when (and (< k 300) (not (has-substring? (text-of-file (serve-log-path st)) "(trace stream-closed lagging")))
        (insert! st (string-append "q" (number->string k)))
        (loop (+ k 1))))
    (for-each (lambda (p) (sh "kill -CONT " (number->string p))) pids))
  (let* ((rc (await-sub-exit s 20000)) (last (datum-of-line (car (reverse (sub-lines s))))))
    (want "F10-7 on overflow the client prints the lagging terminal and exits 1"
          (list rc (and (pair? last) (car last)) (clause 'reason last))
          (list 1 'error '(lagging))))
  (stop-daemon! st))
(let* ((st (fresh-store!))
       (hit (new-id (client "" "insert" "--title" "carries --rev in its title" "--store" st)))
       (miss (new-id (insert! st "plain"))))
  (want "F10-7 search --rev on a daemon still searches the text \"--rev\", as on the base: it finds the block that has it and not the other"
        (let* ((r (cadr (client "" "search" "--rev" "--store" st))) (text (format "~s" r)))
          (list (and (pair? r) (car r)) (and hit (has-substring? text hit)) (and miss (has-substring? text miss))))
        '(ok #t #f))
  (stop-daemon! st))

;; ---- the client's receive, against a scripted daemon -----------------------------
;;
;; A FAKE DAEMON on the store's socket: it accepts one connection, reads the
;; request line, answers the acceptance in the envelope, and then plays a
;; script -- lines, pauses, bytes with no newline, frames of a given size --
;; so a row decides exactly when each byte arrives. It is Perl
;; (change-stream-fake-daemon.pl, its language stated there), as `alarm`
;; above already needs Perl.
(define fake-daemon "change-stream-fake-daemon.pl")
(unless (file-exists? fake-daemon)
  (printf "NOT A READING: ~a is missing (the rows run from test/)~%" fake-daemon)
  (exit 2))
;; -> the subscriber's (out rc err), the fake daemon serving SCRIPT (a list
;; of lines of the little language above).
(define fake-n 0)
(define (subscribe-to-fake! st script)
  (set! fake-n (+ fake-n 1))
  (let ((sf (string-append root "/fake" (number->string fake-n) ".script"))
        (sk (socket-path st)))
    (call-with-output-file sf (lambda (p) (for-each (lambda (l) (display l p) (newline p)) script)))
    (sh "mkdir -p \"$(dirname " (quoted sk) ")\"; perl " (quoted fake-daemon) " " (quoted sk) " " (quoted sf)
        " > " sf ".log 2>&1 &")
    (wait-until (lambda () (file-exists? sk)) 5000)
    (subscribe-bg! "" st "0")))
(define (last-datum s) (let ((ls (sub-lines s))) (and (pair? ls) (datum-of-line (car (reverse ls))))))
(define (ms-now) (let ((t (current-time))) (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))

;; A TAIL THAT STALLS: one whole frame, then part of one, then nothing for
;; fifteen seconds. The partial-frame budget (5 s) ends the stream long
;; before the daemon would: exit 1 and lost-stream, the tail not printed.
(let* ((st (fresh-store!))
       (t0 (ms-now))
       (s (subscribe-to-fake! st '("f 1 200" "p 40" "s 15000" "c")))
       (rc (await-sub-exit s 14000))
       (elapsed (- (ms-now) t0)))
  (want "F10-7 a partial frame that stalls past the budget: exit 1 and lost-stream within the budget, the frame before it printed, the tail not"
        (list rc (< elapsed 12000) (length (sub-lines s)) (last-datum s))
        (list 1 #t 3 '(error transport-error lost-stream))))
;; NO IDLE DEADLINE: nothing for longer than an ordinary request's receive
;; timeout (30 s), then a frame and the drain.
(let* ((st (fresh-store!))
       (s (subscribe-to-fake! st '("s 33000" "f 1 200" "w (error draining)" "c")))
       (rc (await-sub-exit s 60000)))
  (want "F10-7 a stream idle past the ordinary receive timeout is not ended: the frame and the drain arrive, exit 0"
        (list rc (length (sub-lines s)) (last-datum s))
        (list 0 3 '(error draining))))
;; THE ANSWER LIMIT IS PER FRAME: thirty-four frames of one MiB each, more
;; than the 32 MiB limit together, each well under it.
(let* ((st (fresh-store!))
       (s (subscribe-to-fake! st '("f 34 1048576" "w (error draining)" "c")))
       (rc (await-sub-exit s 60000)))
  (want "F10-7 frames that together pass the answer limit, each under it, are all printed, exit 0"
        (list rc (length (sub-lines s)) (last-datum s))
        (list 0 36 '(error draining))))
;; ONE FRAME OVER THE LIMIT: 33 MiB with no newline.
(let* ((st (fresh-store!))
       (s (subscribe-to-fake! st '("p 34603008" "s 3000" "c")))
       (rc (await-sub-exit s 60000)))
  (want "F10-7 one frame past the answer limit ends the stream: exit 1 and answer-too-large"
        (list rc (last-datum s))
        (list 1 '(error transport-error answer-too-large))))
;; EOF IN MID-FRAME.
(let* ((st (fresh-store!))
       (s (subscribe-to-fake! st '("f 1 200" "p 30" "c")))
       (rc (await-sub-exit s 15000)))
  (want "F10-7 EOF in mid-frame: exit 1 and lost-stream, the partial frame not printed"
        (list rc (length (sub-lines s)) (last-datum s))
        (list 1 3 '(error transport-error lost-stream))))
;; THE STORE'S DEATH: the transport-unknown terminal.
(let* ((st (fresh-store!))
       (s (subscribe-to-fake! st '("f 1 200" "w (error transport-unknown (reason store-actor-down))" "c")))
       (rc (await-sub-exit s 15000)))
  (want "F10-7 a transport-unknown terminal: printed, exit 1"
        (list rc (last-datum s))
        (list 1 '(error transport-unknown (reason store-actor-down)))))

;; A FAILED TERMINAL ENDS THE RUN WHEN IT ARRIVES: the lagging line,
;; and then a peer that stays open fifteen seconds.
(let* ((st (fresh-store!))
       (t0 (ms-now))
       (s (subscribe-to-fake! st '("f 1 200" "w (error changes-unavailable (reason lagging) (current 9) (daemon \"1-2\"))" "s 15000" "c")))
       (rc (await-sub-exit s 14000))
       (elapsed (- (ms-now) t0)))
  (want "F10-7 a lagging terminal with the peer still open: printed, exit 1 at once, not at the peer's close"
        (list rc (< elapsed 10000) (and (last-datum s) (car (last-datum s))) (clause 'reason (last-datum s)))
        (list 1 #t 'error '(lagging))))

;; ---- core.sc forwards it like any verb (F10-14) -----------------------------------
(let* ((st (fresh-store!))
       (_ (insert! st "warm"))
       (out (string-append root "/core-subscribe.out"))
       (rc (sh "perl -e 'alarm 60; exec @ARGV' scheme --script ../core.sc subscribe changes 0 --wire --store " (quoted st)
               " > " out " 2> " out ".err < /dev/null"))
       (ls (lines-of-text (text-of-file out)))
       (a (and (pair? ls) (datum-of-line (car ls)))))
  (want "F10-14 core.sc subscribe changes 0 with a daemon present prints the acceptance and exits 0"
        (list rc (length ls) (and (pair? a) (car a)) (and (pair? a) (clause 'subscribed a)) (and (pair? a) (string? (car (or (clause 'daemon a) '(#f))))))
        (list 0 1 'ok '(0) #t))
  (stop-daemon! st))

(sh "rm -rf " run-root)
(printf "rows: ~a~%~a failures~%change-stream-client complete~%" rows bad)
(exit (if (= bad 0) 0 1))
