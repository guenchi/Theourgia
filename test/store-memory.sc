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
;;   SM-4x a segment the warm daemon holds, grown since, is extended, not
;;         scanned, and its load decodes only the appended lines (no switch)
;;   SM-5  a disturbance between two loads of the warm daemon (a sealed
;;         segment replaced at its length, a sealed segment truncated, the
;;         highest segment appended to by another process) is answered as the
;;         cold daemon answers it, the warm daemon proved to have served both
;;         requests after it (its exact load counts, its pid still alive)
;;   SM-6  with --memory-limit below the log, every segment is scanned at
;;         every load, and the answers are the cold daemon's
;;   SM-7  a writer-process load (write --expect) reads the publication's copy
;;         (every ask replayed), leaves the store process's memory as it was,
;;         and answers as the cold daemon does
;;   SM-9  a session no caller handed a memory (log-begin's, on the command
;;         line) keeps none: its reload scans every segment again
;;   SM-8  a write session that reloads (a writer's metadata changed while it
;;         was parked before its append) reloads with its memory: the reload's
;;         load replays what the session's load scanned
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
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia reduce) (theourgia trace)
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
;; A RAISE OUTSIDE A ROW STILL STOPS THE DAEMONS and removes the scratch: the
;; base handler does that first, then reports as it would have
(define cleaned? #f)
(let ((default (base-exception-handler)))
  (base-exception-handler
    (lambda (c)
      (unless cleaned?
        (set! cleaned? #t)
        (guard (e (#t #f)) (stop-daemon!))
        (system (string-append "rm -rf '" root "' '" sock-root "'")))
      (default c))))
(define (log-length) (string-length (file-text log-path)))
;; the bracket keeps pgrep's own shell, whose command line holds the pattern,
;; from matching it
(define (daemon-pids)
  (let* ((p (process (string-append "pgrep -f '[t]heourgiad.sc serve " store "'")))
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
;; THE LOG SETTLES before it is read: a line the daemon writes just after it
;; answers belongs to this request, not the next
(define (settled-log-length)
  (let loop ((n (log-length)) (k 0))
    (system "sleep 0.1")
    (let ((m (log-length)))
      (if (or (= m n) (> k 50)) m (loop m (+ k 1))))))
;; ONE REQUEST THROUGH THE DAEMON, and what its log gained meanwhile:
;; -> (<answer datum> <parse lines> <load-memory counts, one per load> <log-open lines>)
(define (through . args)
  (let* ((before (settled-log-length))
         (out (string-append root "/client.out"))
         (_ (system (string-append (env "") "scheme --script ../theourgia.sc "
                                   (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                                   "--store '" store "' --wire > '" out "' 2>/dev/null < /dev/null")))
         (answer (let loop ((port (open-string-input-port (file-text out))) (last #f))
                   (let ((d (guard (e (#t (eof-object))) (read port))))
                     (cond ((eof-object? d) last)
                           ((and (pair? d) (memq (car d) '(ok error usage))) d)
                           (else (loop port last))))))
         (text (let ((t (begin (settled-log-length) (file-text log-path))))
                 (substring t (min before (string-length t)) (string-length t))))
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
(define (served-readings)
  (let* ((c (through "check")) (l (through "log")))
    (list (list (answer-of c) (answer-of l)) (list (counts-of c) (counts-of l)))))
(define (loaded-readings) (car (served-readings)))
;; THE DAEMON THAT ANSWERED: a client with no daemon answers through a child of
;; its own, so an answer alone does not say a daemon served it; a row reads the
;; load-memory counts of the requests (one line per load the daemon ran) and
;; that the daemon it started is still the one alive
(define (daemon-pid) (let ((ps (daemon-pids))) (and (= 1 (length ps)) (car ps))))
(define (alive-pid? pid) (and pid (zero? (system (string-append "kill -0 " (number->string pid) " 2>/dev/null"))) #t))
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
(want "SM-4 each write's load scanned only the segments created since the store process's previous load: none, then the one the first write made (three segments before it, four after)"
      (list h0 h1 (map (lambda (w) (map (lambda (c) (and (list? c) (list-ref c 3))) (counts-of w))) (list w1 w2)))
      '(3 4 ((0) (1))))
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

;; ---- SM-4x: a segment the warm daemon holds, grown, is extended -------------------
;;
;; WITHOUT THE SWITCH, so the parse lines are the product's own (under it the
;; witness scans each replayed segment again): the daemon is started with the
;; variable empty, whatever this process runs under, since a whole-suite gate
;; sets it. Three writes through the warm daemon on a fresh store. The first
;; makes a new segment, the second grows it, and the third's load meets that
;; segment grown since the store process last held it: extended, not scanned,
;; and the lines it decodes are the ones the second write appended.
(stop-daemon!)
(build-store!)
(start-daemon! "THEOURGIA_DEGENERACY=")
(define x-id (car (ids-now)))
(define (x-write! title req)
  (let* ((c (cursor)) (p (format "((premise ~s ~s))" x-id (hash-of x-id))))
    (through "set" x-id "title" title "--premises" p "--req" req "--cursor" c)))
(define xw1 (x-write! "x1" "X1"))
(define xs1 (highest-segment))
(define xb1 (bytevector-length (segment-bytes xs1)))
(define xw2 (x-write! "x2" "X2"))
(define xs2 (highest-segment))
(define xb2 (bytevector-length (segment-bytes xs2)))
(define xw3-before (settled-log-length))
(define xw3 (x-write! "x3" "X3"))
(define xw3-text (let ((t (file-text log-path))) (substring t xw3-before (string-length t))))
;; THE LOAD'S OWN DECODES: the parse lines between the load's log-open and its
;; load-memory line (after it, the evidence index decodes on its own account)
(define (load-parses text)
  (let loop ((ls (lines-of text)) (in? #f) (n 0))
    (cond ((null? ls) n)
          ((has-prefix? (car ls) "(trace log-open") (loop (cdr ls) #t n))
          ((has-prefix? (car ls) "(trace load-memory") (if in? n (loop (cdr ls) #f n)))
          ((and in? (has-prefix? (car ls) "(trace parse")) (loop (cdr ls) #t (+ n 1)))
          (else (loop (cdr ls) in? n)))))
;; the lines the second write appended to the segment
(define (lines-between bv from to)
  (let loop ((i from) (n 0)) (if (>= i to) n (loop (+ i 1) (if (= 10 (bytevector-u8-ref bv i)) (+ n 1) n)))))
(define (extended-events text seg)
  (length (filter (lambda (l) (and (has-prefix? l "(trace memory (")
                                   (let ((d (read-or-false l))) (and d (list? (caddr d)) (= 4 (length (caddr d)))
                                                                     (eqv? (caddr (caddr d)) seg) (eq? (cadddr (caddr d)) 'extended)))))
                  (lines-of text))))
(define x-appended (lines-between (segment-bytes xs2) xb1 xb2))
(printf "SM-4x writes: ~s ~s ~s; segment ~a grew ~a -> ~a bytes (~a lines); the third write's loads ~s, its load's parses ~a (the request's in all ~a), extended events ~a\n"
        (decision (answer-of xw1)) (decision (answer-of xw2)) (decision (answer-of xw3)) xs2 xb1 xb2 x-appended
        (counts-of xw3) (load-parses xw3-text) (parses-of xw3) (extended-events xw3-text xs2))
(want "SM-4x the second write grew the segment the first made; the third write's load extended it (at least one extended ask, none scanned) and decoded only the lines the second write appended"
      (list (= xs1 xs2) (> xb2 xb1)
            (and (pair? (counts-of xw3)) (>= (list-ref (car (counts-of xw3)) 1) 1) (list-ref (car (counts-of xw3)) 3))
            (> (extended-events xw3-text xs2) 0)
            (= (load-parses xw3-text) x-appended))
      (list #t #t 0 #t #t))

;; ---- SM-T: one load-memory line per load ----------------------------------------

(want "SM-T every load the warm daemon ran for a write said once what its memory did"
      (map (lambda (w) (list (length (counts-of w)) (opens-of w))) (list w1 w2))
      (map (lambda (w) (list (opens-of w) (opens-of w))) (list w1 w2)))

;; A LOAD REFUSED AFTER IT WAS MADE says so too: a segment that cannot be read
;; stops its writer, and a load that did not declare it accepts an incomplete
;; store is refused, in this process, with a memory handed in
(stop-daemon!)
(build-store!)
(system (string-append "chmod 000 '" (segment-path 2) "'"))
(define refused-trace
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t #f)) (log-open-with-memory store (make-load-memory)))
      (trace-enable! #f))
    (lines-of (get-output-string p))))
(system (string-append "chmod 644 '" (segment-path 2) "'"))
(want "SM-T a load refused after it was made (an unreadable segment, no declaration) still says once what its memory did"
      (list (length (filter (lambda (l) (has-prefix? l "(trace log-open")) refused-trace))
            (length (filter (lambda (l) (has-prefix? l "(trace load-memory")) refused-trace)))
      '(1 1))

;; ---- SM-5: disturbances between two loads of the warm daemon ---------------------

;; EXPECTED is the warm daemon's load-memory counts for the check and the log
;; after the disturbance: the disturbed segment scanned (or extended), the
;; others replayed
(define (disturbed label expected disturb!)
  (stop-daemon!)
  (build-store!)
  (start-daemon! warm-switch)
  (let ((pid (daemon-pid)))
    (through "check")
    (disturb!)
    (let* ((warm (served-readings)) (alive (alive-pid? pid)))
      (printf "SM-5 ~a: the warm daemon ~a (alive after: ~a) counted ~s\n" label pid alive (cadr warm))
      (start-daemon! "")
      (let ((cold (loaded-readings)))
        (unless (equal? (car warm) cold) (printf "SM-5 ~a: warm ~s\n   cold ~s\n" label (car warm) cold))
        (want (string-append "SM-5 " label ": the warm daemon, still the one alive, served both requests after the disturbance with these loads, and answers as a daemon started after it")
              (list (equal? (car warm) cold) (cadr warm) alive)
              (list #t expected #t))))))
;; check runs two loads, its own (validation only: three asks) and the one it
;; nests (validation and delivery: six), the nested one ending first; log one
;; (six). The first load to see a disturbed segment scans it once; after it the
;; memory holds the new bytes and every ask replays.
(disturbed "a sealed segment replaced at its length"
           '(((replayed-or-extended 6 scanned 0) (replayed-or-extended 2 scanned 1)) ((replayed-or-extended 6 scanned 0)))
           (lambda () (put! (segment-path 1) (records 1 3 "x"))))
;; truncated, segment 2 ends the writer's readable prefix, so segment 3 is not asked
(disturbed "a sealed segment truncated"
           '(((replayed-or-extended 4 scanned 0) (replayed-or-extended 1 scanned 1)) ((replayed-or-extended 4 scanned 0)))
           (lambda () (let ((b (segment-bytes 2))) (put! (segment-path 2) (bytevector-prefix b (- (bytevector-length b) 7))))))
;; appended, the highest segment is extended (counted with the replays), none scanned
(disturbed "the highest segment appended to by another process, without the lock"
           '(((replayed-or-extended 6 scanned 0) (replayed-or-extended 3 scanned 0)) ((replayed-or-extended 6 scanned 0)))
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
(define limited-pid (daemon-pid))
(define limited* (served-readings))
(define limited-alive (alive-pid? limited-pid))
(printf "SM-6 the limited daemon ~a (alive after: ~a) counted ~s\n" limited-pid limited-alive (cadr limited*))
(start-daemon! "")
(want "SM-6 and its answers, the limited daemon still the one alive and its loads replaying nothing, are a daemon's with no limit, just started"
      (list (equal? (car limited*) (loaded-readings))
            (for-all (lambda (cs) (and (pair? cs) (for-all (lambda (c) (and (list? c) (eqv? 0 (list-ref c 1)))) cs)))
                     (cadr limited*))
            limited-alive)
      '(#t #t #t))

;; ---- SM-8: a write session that reloads -----------------------------------------
;;
;; A WRITE PARKED BEFORE ITS APPEND (THEOURGIA_BARRIER=before-append, an
;; injection build), the local writer's owner.sexp given a trailing newline
;; meanwhile (the same datum, other bytes): its file version, a hash of its
;; bytes, moves, so the session reloads before it
;; appends (log.sc, versions-changed?, reload!), and the reload is a second
;; load inside the same write. Handed the session's memory it replays every
;; segment the first load scanned.

(stop-daemon!)
(build-store!)
(define fifo (string-append root "/before-append.fifo"))
(system (string-append "rm -f '" fifo "'; mkfifo '" fifo "'"))
(start-daemon! (string-append warm-switch " THEOURGIA_INJECT=on THEOURGIA_BARRIER=before-append:" fifo))
(through "check")
(define t8 (car (ids-now)))
(define c8 (cursor))
(define p8 (format "((premise ~s ~s))" t8 (hash-of t8)))
(define before8 (settled-log-length))
;; THE PARKED CLIENT RUNS FROM A SCRIPT FILE, so its arguments (a premise
;; datum holds double quotes) are written once, each quoted for sh
(define out8 (string-append root "/client8.out"))
(define (sh-quoted s)
  (let loop ((cs (string->list s)) (acc '(#\')))
    (cond ((null? cs) (list->string (reverse (cons #\' acc))))
          ((char=? (car cs) #\') (loop (cdr cs) (append (reverse (string->list "'\\''")) acc)))
          (else (loop (cdr cs) (cons (car cs) acc))))))
(call-with-output-file (string-append root "/client8.sh")
  (lambda (p)
    (put-string p (string-append (env "") "scheme --script ../theourgia.sc set " (sh-quoted t8) " title parked --premises "
                                 (sh-quoted p8) " --req Q8 --cursor " (sh-quoted c8) " --store " (sh-quoted store)
                                 " --wire > " (sh-quoted out8) " 2>/dev/null < /dev/null; touch " (sh-quoted (string-append out8 ".done")) "\n")))
  'replace)
(system (string-append "sh '" root "/client8.sh' &"))
(define parked
  (let wait ((k 0))
    (let ((t (file-text log-path)))
      (cond ((let find ((i before8))
               (and (< (+ i 30) (string-length t))
                    (or (string=? (substring t i (+ i 30)) "(trace barrier before-append #")
                        (find (+ i 1)))))
             #t)
            ((> k 300) #f)
            (else (system "sleep 0.05") (wait (+ k 1)))))))
;; RELEASED ONLY WHEN PARKED: a write to the fifo blocks until a reader opens
;; it, and a write that never reached the barrier has no reader
(when parked
  (let ((owner (string-append store "/writers/" W "/owner.sexp")))
    (append! owner (string->utf8 "\n")))
  (call-with-output-file fifo (lambda (p) (display "x" p)) 'append))
(let wait ((k 0)) (unless (or (file-exists? (string-append out8 ".done")) (> k 600)) (system "sleep 0.05") (wait (+ k 1))))
(define after8 (begin (settled-log-length) (file-text log-path)))
(define counts8
  (map (lambda (l) (let ((d (read-or-false l))) (and d (caddr d))))
       (filter (lambda (l) (has-prefix? l "(trace load-memory"))
               (lines-of (substring after8 (min before8 (string-length after8)) (string-length after8))))))
(define answer8 (read-or-false (file-text out8)))
(printf "SM-8 parked ~a; the write ~s; its loads ~s\n" parked answer8 counts8)
;; THE WRITE IS REFUSED (refused epoch): a session that reloads before its
;; append does not append on the moved state; the request is the client's to
;; send again. That answer is the product's whatever the memory (the base, with
;; a memory per load, answers it too); what this row judges is the reload's load.
(want "SM-8 the write parked before its append reloaded after the metadata moved: two loads, the reload's replaying every segment the session's load had (none scanned), and the write refused as an epoch moved"
      (list parked (and (pair? answer8) (list (car answer8) (cadr answer8) (caddr answer8))) (length counts8)
            (and (= 2 (length counts8)) (list? (cadr counts8)) (list-ref (cadr counts8) 3)))
      '(#t (error refused epoch) 2 0))

;; ---- SM-9: a session that no caller handed a memory keeps none ------------------
;;
;; A WRITE ON THE COMMAND LINE (THEOURGIA_LOCAL=1: log-begin, no daemon, no
;; provider), parked before its append while owner.sexp's bytes change, as SM-8:
;; its reload is a second load in the same session, and with no memory kept by
;; the session it scans every segment again.
(stop-daemon!)
(build-store!)
(define fifo9 (string-append root "/before-append-9.fifo"))
(system (string-append "rm -f '" fifo9 "'; mkfifo '" fifo9 "'"))
(define t9 (car (ids-now)))
(define out9 (string-append root "/client9.out"))
(define err9 (string-append root "/client9.err"))
(call-with-output-file (string-append root "/client9.sh")
  (lambda (p)
    (put-string p (string-append (env (string-append "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_BARRIER=before-append:" fifo9))
                                 "scheme --script ../core.sc set " (sh-quoted t9) " title parked9 --store " (sh-quoted store)
                                 " --wire > " (sh-quoted out9) " 2> " (sh-quoted err9) " < /dev/null; touch " (sh-quoted (string-append out9 ".done")) "\n")))
  'replace)
(system (string-append "sh '" root "/client9.sh' &"))
(define parked9
  (let wait ((k 0))
    (cond ((let find ((t (file-text err9)) (i 0))
             (and (< (+ i 30) (string-length t))
                  (or (string=? (substring t i (+ i 30)) "(trace barrier before-append #") (find t (+ i 1)))))
           #t)
          ((> k 600) #f)
          (else (system "sleep 0.05") (wait (+ k 1))))))
(when parked9
  (append! (string-append store "/writers/" W "/owner.sexp") (string->utf8 "\n"))
  (call-with-output-file fifo9 (lambda (p) (display "x" p)) 'append))
(let wait ((k 0)) (unless (or (file-exists? (string-append out9 ".done")) (> k 600)) (system "sleep 0.05") (wait (+ k 1))))
(define counts9
  (map (lambda (l) (let ((d (read-or-false l))) (and d (caddr d))))
       (filter (lambda (l) (has-prefix? l "(trace load-memory")) (lines-of (file-text err9)))))
(printf "SM-9 parked ~a; its loads ~s\n" parked9 counts9)
(want "SM-9 a command-line write parked before its append reloaded after the metadata moved, and the reload, handed no kept memory, scanned every segment again"
      (list parked9
            (and (pair? counts9) (list? (car (last-pair counts9))) (list-ref (car (last-pair counts9)) 3)))
      (list #t (highest-segment)))

;; ---- SM-7: a writer-process load ------------------------------------------------

(stop-daemon!)
(build-store!)
(start-daemon! warm-switch)
(define o1 (through "check"))
(define t7 (car (ids-now)))
(define x1 (through "write" t7 "(draft text)" "--writer" "d1" "--expect" (hash-of t7)))
(define o2 (through "check"))
(printf "SM-7 check ~s; write --expect ~s loads ~s; check ~s\n" (counts-of o1) (answer-of x1) (counts-of x1) (counts-of o2))
;; THE PUBLICATION'S COPY (F437 S3's second step): the writer process's load
;; reads the copy the store process made when it published, which holds all
;; three segments, so all six asks replay and none scans. Before that step it
;; loaded with a memory of its own (three scanned, three replayed).
;; WHAT THIS JUDGES: that the load reads the copy. The counts cannot tell the
;; copy from the store process's memory itself (both would replay all six);
;; that the writer process is not handed the live memory, and stores nothing
;; into it, is test/store-copy.sc's SB-3. The next check's equal counts are not,
;; on their own, a witness of it either (equal bytes would leave a shared
;; memory's entries as they were).
(want "SM-7 the writer process's load reads the publication's copy of the store process's memory, not the memory itself (since F437 S3's second step: all six asks replayed, none scanned); the store process's next check counts as before it"
      (list (counts-of x1) (equal? (counts-of o2) (counts-of o1)))
      (list (list (list 'replayed-or-extended (* 2 (highest-segment)) 'scanned 0)) #t))
(define warm-write (answer-of x1))
(start-daemon! "")
(define cold-pid (daemon-pid))
(define x2 (through "write" t7 "(draft text)" "--writer" "d1" "--expect" (hash-of t7)))
(want "SM-7 and the same write --expect through a daemon just started answers alike, that daemon serving it (one load line, its pid alive after)"
      (list (decision (answer-of x2)) (length (counts-of x2)) (alive-pid? cold-pid))
      (list (decision warm-write) 1 #t))

(stop-daemon!)
(system (string-append "rm -rf '" root "' '" sock-root "'"))
(set! cleaned? #t)
(printf "\n~a failures\nrows: ~a\nstore-memory complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
