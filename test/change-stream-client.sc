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
(define (clause name datum)
  (let ((c (and (pair? datum) (list? datum) (assq name (cdr datum))))) (and c (cdr c))))
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
(define (sub-lines s) (lines-of-text (text-of-file (car s))))
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
  (let ((rc (await-sub-exit s 15000)) (ls (sub-lines s)))
    (want "F10-7 the client prints the acceptance, two frames and the draining terminal as four lines, and exits 0"
          (list rc (length ls)
                (let ((a (datum-of-line (car ls)))) (and (pair? a) (car a)))
                (map (lambda (l) (car (datum-of-line l))) (cdr ls))
                (datum-of-line (list-ref ls 3)))
          (list 0 4 'ok '(changes changes error) '(error draining)))))

;; THE SAME FOUR LINES with every byte its own read.
(let* ((st (fresh-store!))
       (_ (insert! st "warm"))
       (s (subscribe-bg! "THEOURGIA_READ_CHUNK=1" st "0")))
  (await-sub-lines s 1 10000)
  (insert! st "one") (insert! st "two")
  (await-sub-lines s 3 10000)
  (signal-daemon! st "TERM")
  (let ((rc (await-sub-exit s 15000)) (ls (sub-lines s)))
    (want "F10-7 with THEOURGIA_READ_CHUNK=1 the same four lines and exit 0"
          (list rc (length ls) (and (= (length ls) 4) (datum-of-line (list-ref ls 3))))
          (list 0 4 '(error draining)))))

;; TWO LINES AND A TAIL IN ONE READ: the daemon writes two complete frames
;; and the first half of a third in one write; the client's trace must show
;; one read holding two complete lines and a tail. Retried up to five
;; times; a run that never observes it is NOT a reading.
(let loop ((try 1))
  (let* ((st (fresh-store!))
         (_ (client "THEOURGIA_INJECT=on THEOURGIA_FAULT=stream-coalesce-cut@conn" "insert" "--title" "warm" "--store" st))
         (s (subscribe-bg! "THEOURGIA_INJECT=on THEOURGIA_FAULT=stream-coalesce-cut@conn THEOURGIA_READ_CHUNK=65536" st "0")))
    (await-sub-lines s 1 10000)
    (insert! st "one") (insert! st "two") (insert! st "three")
    (await-sub-lines s 4 10000)
    (signal-daemon! st "TERM")
    (let* ((rc (await-sub-exit s 15000)) (ls (sub-lines s))
           (observed (exists (lambda (l) (let ((d (datum-of-line l)))
                                           (and (pair? d) (list? d) (= (length d) 5) (eq? (car d) 'trace) (eq? (cadr d) 'read-chunk)
                                                (>= (list-ref d 3) 2) (eq? (list-ref d 4) #t))))
                             (lines-of-text (text-of-file (caddr s))))))
      (cond
        (observed
         (want "F10-7 under stream-coalesce-cut a read held two complete lines and a tail, and the lines are whole"
               (list rc (length ls) (map (lambda (l) (car (datum-of-line l))) (cdr ls)))
               (list 0 5 '(changes changes changes error))))
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

;; A RESTART: a read answered by another incarnation carries another token.
(let* ((st (fresh-store!))
       (x (new-id (insert! st "x")))
       (s (subscribe-bg! "" st "0")))
  (let* ((ls (await-sub-lines s 1 10000))
         (acc (datum-of-line (car ls)))
         (sub-token (let ((c (clause 'daemon acc))) (and c (car c)))))
    (stop-daemon! st)
    (let ((later (cadr (client "" "read" x "--rev" "--store" st))))
      (want "F10-7 a read answered by a restarted daemon carries another token: the consumer subscribes afresh"
            (and (string? sub-token) (string? (token-of later)) (not (equal? sub-token (token-of later))))
            #t)))
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
       (_ (client "THEOURGIA_TRACE=1 THEOURGIA_SEND_BUFFER=16384" "insert" "--title" "warm" "--store" st))
       (s (subscribe-bg! "THEOURGIA_SEND_BUFFER=16384" st "0")))
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
      (when (and (< k 300) (not (has-substring? (text-of-file (serve-log-path st)) "(trace lagging")))
        (insert! st (string-append "q" (number->string k)))
        (loop (+ k 1))))
    (for-each (lambda (p) (sh "kill -CONT " (number->string p))) pids))
  (let* ((rc (await-sub-exit s 20000)) (last (datum-of-line (car (reverse (sub-lines s))))))
    (want "F10-7 on overflow the client prints the lagging terminal and exits 1"
          (list rc (and (pair? last) (car last)) (clause 'reason last))
          (list 1 'error '(lagging))))
  (stop-daemon! st))
(let* ((st (fresh-store!))
       (_ (insert! st "warm")))
  (want "F10-7 search --rev on a daemon still searches the text \"--rev\", as on the base"
        (let ((r (cadr (client "" "search" "--rev" "--store" st)))) (and (pair? r) (car r)))
        'ok)
  (stop-daemon! st))

(sh "rm -rf " run-root)
(printf "rows: ~a~%~a failures~%change-stream-client complete~%" rows bad)
(exit (if (= bad 0) 0 1))
