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

;; THE STORE PROCESS KEEPS A MEMORY. A daemon's store process hands every load
;; it runs one memory, made at its start (daemon.sc, store-loop), so a load
;; decodes only the bytes no earlier load of that process remembered. These
;; rows hold it to answering as a daemon with an empty memory answers -- a
;; daemon just started -- on ONE store: at each point a row judges, the warm
;; daemon (its memory filled by its earlier loads) answers, it is stopped, and
;; a daemon started then on the same store answers the same requests.
;;
;;   SM-4  the second of two writes through the warm daemon decodes only the
;;         bytes appended since the store process's last load, and the cold
;;         daemon reads the store as the warm one published it
;;   SM-5  a disturbance between two loads of the warm daemon (a sealed
;;         segment replaced at its length, a sealed segment truncated, the
;;         highest segment appended to by another process) is answered as the
;;         cold daemon answers it
;;   SM-6  with --memory-limit below the log, every segment is scanned at
;;         every load, and the answers are the cold daemon's
;;   SM-7  a writer-process load (write --expect) leaves the store process's
;;         memory as it was, and answers as the cold daemon does
;;   SM-T  every load says once what its memory did: one load-memory line per
;;         log-open, with its two counts
;;
;; A WRITE'S DECISION IS JUDGED TWICE. Before each warm write a cold reader --
;; this process, loading with a memory of its own -- reads the versions the
;; write's premises name; the warm daemon must accept exactly when every one
;; holds and refuse naming the one that does not. After, the cold daemon is
;; sent the identical request (the same --req, --cursor and premises): an
;; accepted one must answer as a replay naming the same event, a refused one
;; must be refused the same way. The warm daemon runs under THEOURGIA_DEGENERACY
;; =only:replay-scan,verdict-discoveries, so every replay inside its write
;; sessions is compared with a fresh scan of the same bytes.
;;
;; THE TWO-COPY FORM IS NOT BUILT HERE: two byte-equal copies of a store, one
;; behind each daemon, written in parallel, would compare two histories each
;; daemon wrote itself. A copy is refused until adopted ((error refused
;; (instance inode))), adopting mints a new writer, and every record carries
;; its wall-clock time, so the two copies' bytes and their answers' ids would
;; differ by construction.
;;
;; The counts are read from the daemon's log, where it writes its trace
;; (THEOURGIA_TRACE=1): one `parse` per line decoded, one `load-memory` per
;; load.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia reduce)
        (only (theourgia store) open-and-reduce)
        (only (theourgia client) socket-path serve-log-path))

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

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define pid-text (number->string (get-process-id)))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/store-memory-" pid-text))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/smem" pid-text))
(define store (string-append root "/store"))
(define home (string-append root "/home"))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" home "' '" sock-root "/run'"))
(putenv "THEOURGIA_HOME" home)
(putenv "THEOURGIA_RUN" (string-append sock-root "/run"))

(define (file-text path)
  (if (file-exists? path) (let ((t (call-with-input-file path get-string-all))) (if (string? t) t "")) ""))
(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))
(define (has-prefix? s p)
  (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))
(define (read-or-false text)
  (guard (e (#t #f)) (let ((x (read (open-string-input-port text)))) (if (eof-object? x) #f x))))

;; ---- the store: writer W, 8 records in three segments (1-3, 4-6, 7-8) ------------

(define W "wwwm4k2a")
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (append! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail no-truncate append))
    (lambda (p) (put-bytevector p bv))))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (iota* from to) (if (> from to) '() (cons from (iota* (+ from 1) to))))
(define (record seq title)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '()
                 (storable-encode (list 'put (list (cons 'kind 'section) (cons 'title title))))))
(define (records from to letter)
  (apply cat (map (lambda (k) (record k (format "~a-~a" letter k))) (iota* from to))))
(define (segment-path n) (string-append store "/writers/" W "/" (segment-file-name n)))
(define (segment-bytes n) (call-with-port (open-file-input-port (segment-path n)) get-bytevector-all))
;; each row starts from this store, built afresh (no daemon is serving it then)
(define (build-store!)
  (system (string-append "rm -rf '" store "'; mkdir -p '" store "/writers/" W "' '" store "/snap'"))
  (put! (string-append store "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (put! (string-append store "/lock") (make-bytevector 0))
  (put! (string-append store "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (let ((n (instance-install! store))) (owner-install! store W n))
  (put! (segment-path 1) (records 1 3 "w"))
  (put! (segment-path 2) (records 4 6 "w"))
  (put! (segment-path 3) (records 7 8 "w")))
(define (bytevector-prefix b n) (let ((o (make-bytevector n))) (bytevector-copy! b 0 o 0 n) o))
(define (highest-segment) (let loop ((k 1)) (if (file-exists? (segment-path (+ k 1))) (loop (+ k 1)) k)))

;; ---- the daemons and the client ------------------------------------------------

(define (env extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" home
                 " THEOURGIA_RUN=" sock-root "/run THEOURGIA_TRACE=1 " extra " "))
(define warm-switch "THEOURGIA_DEGENERACY=only:replay-scan,verdict-discoveries")
(include "serve-for.ss")
(define log-path (serve-log-path store))
(define (log-length) (string-length (file-text log-path)))
(define (daemon-pids)
  (let* ((p (process (string-append "pgrep -f 'theourgiad.sc serve " store "'")))
         (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? t) '() (filter (lambda (x) x) (map string->number (lines-of t))))))
(define (stop-daemon!)
  (for-each (lambda (pid) (system (string-append "kill " (number->string pid) " 2>/dev/null"))) (daemon-pids))
  (let wait ((k 0))
    (cond ((and (null? (daemon-pids)) (not (serve-for-connects? (socket-path store)))) #t)
          ((> k 200) (for-each (lambda (pid) (system (string-append "kill -KILL " (number->string pid) " 2>/dev/null")))
                               (daemon-pids))
                     #f)
          (else (system "sleep 0.05") (wait (+ k 1))))))
;; -> the socket, or the start's refusal
(define (start-daemon! switch . extra)
  (stop-daemon!)
  (serve-await! (serve-spawn-with! root store #f (env switch) extra)))
;; ONE REQUEST THROUGH THE DAEMON, and what its log gained meanwhile:
;; -> (<answer datum> <parse lines> <load-memory counts, one per load> <log-open lines>)
(define (through . args)
  (let* ((before (log-length))
         (out (string-append root "/client.out"))
         (_ (system (string-append (env "") "scheme --script ../theourgia.sc "
                                   (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                                   "--store '" store "' --wire > '" out "' 2>/dev/null < /dev/null")))
         (answer (let loop ((port (open-string-input-port (file-text out))) (last #f))
                   (let ((d (guard (e (#t (eof-object))) (read port))))
                     (cond ((eof-object? d) last)
                           ((and (pair? d) (memq (car d) '(ok error usage))) d)
                           (else (loop port last))))))
         (text (let ((t (file-text log-path))) (substring t (min before (string-length t)) (string-length t))))
         (ls (lines-of text)))
    (list answer
          (length (filter (lambda (l) (has-prefix? l "(trace parse")) ls))
          (map (lambda (l) (let ((d (read-or-false l))) (and d (caddr d))))
               (filter (lambda (l) (has-prefix? l "(trace load-memory")) ls))
          (length (filter (lambda (l) (has-prefix? l "(trace log-open")) ls)))))
(define (answer-of r) (car r))
(define (parses-of r) (cadr r))
(define (counts-of r) (caddr r))
(define (opens-of r) (cadddr r))

;; THE COLD READER: this process, loading with a memory of its own
(define (cold-state) (open-and-reduce store))
(define (end-seq) (cdr (assoc W (reduce-applied-cut (cold-state)))))
(define (cursor) (string-append W ":" (number->string (end-seq))))
(define (hash-of id) (block-hash (cold-state) id))
(define (ids-now) (sort string<? (map cadr (state-datum (cold-state)))))
;; WHAT A DAEMON ANSWERS FROM A LOAD: check and log are the store process's
;; requests, each a load (outline and read are answered from the published
;; value, without one)
(define (loaded-readings)
  (list (answer-of (through "check")) (answer-of (through "log"))))
;; and what a reader sees of the store besides
(define (readings)
  (append (loaded-readings)
          (list (answer-of (through "outline"))
                (map (lambda (id) (answer-of (through "read" id))) (ids-now)))))
;; the event a write answer names: a fresh write's (events (<event>)), a
;; replay's (event <event>)
(define (event-of answer)
  (let ((many (and (pair? answer) (assq 'events (cdr answer))))
        (one (and (pair? answer) (assq 'event (cdr answer)))))
    (cond ((and many (pair? (cadr many)) (null? (cdr (cadr many)))) (car (cadr many)))
          (one (cadr one))
          (else #f))))
(define (replay-of answer) (let ((e (and (pair? answer) (assq 'replay (cdr answer))))) (and e (cadr e))))
;; an answer with what differs between two daemons' runs left out: a refusal's
;; time and instance fields
(define (decision answer)
  (and (pair? answer)
       (if (eq? (car answer) 'ok)
           (list 'ok (event-of answer))
           (list (car answer) (cadr answer)
                 (filter (lambda (c) (not (and (pair? c) (memq (car c) '(at time instance machine))))) (cddr answer))))))

;; ---- SM-4: two writes through the warm daemon ----------------------------------

(build-store!)
(define started (start-daemon! warm-switch))
(printf "SM-4 the warm daemon (~a): ~s\n" warm-switch started)
(define t1 (car (ids-now)))
;; WRITE 1: its premise read by the cold reader first
(define p1 (format "((premise ~s ~s))" t1 (hash-of t1)))
(define c1 (cursor))
(define h0 (highest-segment))
(define w1 (through "set" t1 "title" "first" "--premises" p1 "--req" "Q1" "--cursor" c1))
(define c2 (cursor))
(define h1 (highest-segment))
(define p2 (format "((premise ~s ~s))" t1 (hash-of t1)))
(define w2 (through "set" t1 "title" "second" "--premises" p2 "--req" "Q2" "--cursor" c2))
;; A REFUSED WRITE: its premise names the version the first write replaced,
;; which the cold reader no longer reads
(define stale (format "((premise ~s ~s))" t1 "0000000000000000000000000000000000000000000000000000000000000000"))
(define w3 (through "set" t1 "title" "third" "--premises" stale "--req" "Q3" "--cursor" (cursor)))
(printf "SM-4 write 1: ~s parses ~a loads ~s\n" (decision (answer-of w1)) (parses-of w1) (counts-of w1))
(printf "SM-4 write 2: ~s parses ~a loads ~s\n" (decision (answer-of w2)) (parses-of w2) (counts-of w2))
(printf "SM-4 write 3: ~s parses ~a loads ~s\n" (decision (answer-of w3)) (parses-of w3) (counts-of w3))
(want "SM-4 the cold reader's premises held for writes 1 and 2 and not for 3: the warm daemon accepted 1 and 2 and refused 3"
      (map (lambda (w) (and (pair? (answer-of w)) (car (answer-of w)))) (list w1 w2 w3))
      '(ok ok error))
;; THE SEGMENTS A LOAD SCANS ARE THE ONES NO EARLIER LOAD OF THE STORE PROCESS
;; SAW: the first write's load meets the three its start-up load read; the
;; second's meets those and the one the first write created. (A memory-less
;; load scans every segment, at both phases.)
(want "SM-4 each write's load scanned only the segments created since the store process's previous load: none, then the one the first write made"
      (map (lambda (w) (map (lambda (c) (and (list? c) (list-ref c 3))) (counts-of w))) (list w1 w2))
      (list (list (- h0 3)) (list (- h1 h0))))
(define warm-readings (readings))
(stop-daemon!)
(define cold-started (start-daemon! ""))
(define cold-readings (readings))
(want "SM-4 a daemon started after the writes reads the store as the warm one did: outline, every block, log"
      (equal? warm-readings cold-readings)
      #t)
;; THE DECISIONS RE-ASKED of the cold daemon, identical requests
(define r1 (through "set" t1 "title" "first" "--premises" p1 "--req" "Q1" "--cursor" c1))
(define r2 (through "set" t1 "title" "second" "--premises" p2 "--req" "Q2" "--cursor" c2))
(printf "SM-4 warm answers ~s ~s; cold answers ~s ~s\n" (answer-of w1) (answer-of w2) (answer-of r1) (answer-of r2))
(want "SM-4 the cold daemon answers the two accepted writes as replays naming the same events"
      (list (map (lambda (r) (replay-of (answer-of r))) (list r1 r2))
            (equal? (map (lambda (r) (event-of (answer-of r))) (list r1 r2))
                    (map (lambda (w) (event-of (answer-of w))) (list w1 w2))))
      '((#t #t) #t))
(define r3 (through "set" t1 "title" "third" "--premises" stale "--req" "Q3" "--cursor" (cursor)))
(want "SM-4 and refuses the refused one the same way"
      (equal? (decision (answer-of r3)) (decision (answer-of w3)))
      #t)

;; ---- SM-T: one load-memory line per load ----------------------------------------

(want "SM-T every load the warm daemon ran for a write said once what its memory did"
      (map (lambda (w) (list (length (counts-of w)) (opens-of w))) (list w1 w2))
      (map (lambda (w) (list (opens-of w) (opens-of w))) (list w1 w2)))

;; ---- SM-5: disturbances between two loads of the warm daemon ---------------------

(define (disturbed label disturb!)
  (stop-daemon!)
  (build-store!)
  (start-daemon! warm-switch)
  (through "check")
  (disturb!)
  (let ((warm (loaded-readings)))
    (start-daemon! "")
    (let ((cold (loaded-readings)))
      (unless (equal? warm cold) (printf "SM-5 ~a: warm ~s\n   cold ~s\n" label warm cold))
      (want (string-append "SM-5 " label ": the warm daemon answers as a daemon started after it")
            (equal? warm cold)
            #t))))
(disturbed "a sealed segment replaced at its length"
           (lambda () (put! (segment-path 1) (records 1 3 "x"))))
(disturbed "a sealed segment truncated"
           (lambda () (let ((b (segment-bytes 2))) (put! (segment-path 2) (bytevector-prefix b (- (bytevector-length b) 7))))))
(disturbed "the highest segment appended to by another process, without the lock"
           (lambda () (append! (segment-path (highest-segment)) (record (+ 1 (end-seq)) "lockless"))))

;; ---- SM-6: a limit below the log ------------------------------------------------

(stop-daemon!)
(build-store!)
(define limited-start (start-daemon! warm-switch "--memory-limit" "1"))
(printf "SM-6 the daemon with --memory-limit 1: ~s\n" limited-start)
(define l1 (through "check"))
(define l2 (through "check"))
(printf "SM-6 limit 1: check ~s ~s\n" (counts-of l1) (counts-of l2))
;; check runs two loads (its own and the one it nests); with nothing kept,
;; each scans at every ask, validation and delivery alike
(want "SM-6 with --memory-limit 1 no load replays a segment: the second check's loads scan as the first's did"
      (list (pair? (counts-of l2))
            (for-all (lambda (c) (and (list? c) (eqv? 0 (list-ref c 1)))) (append (counts-of l1) (counts-of l2)))
            (equal? (counts-of l1) (counts-of l2)))
      '(#t #t #t))
(define limited (loaded-readings))
(start-daemon! "")
(want "SM-6 and its answers are a daemon's with no limit, just started"
      (equal? limited (loaded-readings))
      #t)

;; ---- SM-7: a writer-process load ------------------------------------------------

(stop-daemon!)
(build-store!)
(start-daemon! warm-switch)
(define o1 (through "check"))
(define t7 (car (ids-now)))
(define x1 (through "write" t7 "(draft text)" "--writer" "d1" "--expect" (hash-of t7)))
(define o2 (through "check"))
(printf "SM-7 check ~s; write --expect ~s loads ~s; check ~s\n" (counts-of o1) (answer-of x1) (counts-of x1) (counts-of o2))
;; A MEMORY OF ITS OWN, STARTING EMPTY: validation scans each of the three
;; segments and delivery replays what validation scanned; the store process's
;; memory, which holds all three, would have replayed all six asks
(want "SM-7 the writer process's load is handed a memory of its own (it scans every segment once, delivery replaying them), and the store process's next load replays every one, as before it"
      (list (counts-of x1) (equal? (counts-of o2) (counts-of o1)))
      (list (list (list 'replayed-or-extended (highest-segment) 'scanned (highest-segment))) #t))
(define warm-write (answer-of x1))
(start-daemon! "")
(want "SM-7 and the same write --expect through a daemon just started answers alike"
      (decision (answer-of (through "write" t7 "(draft text)" "--writer" "d1" "--expect" (hash-of t7))))
      (decision warm-write))

(stop-daemon!)
(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\nstore-memory complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
