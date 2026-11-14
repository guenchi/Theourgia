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

;; A WRITER PROCESS'S LOADS READ THE PUBLICATION'S COPY OF THE STORE PROCESS'S
;; MEMORY (F437 S3, second step): publish! makes the copy in the store process
;; (daemon.sc), the cell carries it, answer-published seals it with the state,
;; working.sc's folds hand it to open-and-reduce-reading (store.sc), and the
;; load reads it through copy-reading-memory (log.sc), which never stores.
;;
;;   SB-1  a --based-on save through a warm daemon: routed to a writer process,
;;         its two loads read the copy (one load-on-copy line each) and scan
;;         nothing; and the same under only:replay-scan, the witness comparing
;;         every replay of the copy with a scan, silent
;;   SB-2  a segment no publication has seen (made on disk while the probe that
;;         would ask for a fold fails): each load of two saves scans it once
;;         (delivery replays it from the load's overlay), so no miss was stored
;;         into the copy and no overlay outlived its load
;;   SB-3  then the store process's own next load still scans it: the writer
;;         process's loads stored nothing into the store process's memory
;;   SB-4  in this process: a copy taken while a mirror's segment was hashed and
;;         not scanned (its manifest hash wrong) holds that entry as it was after
;;         a later load completed it in the memory, and after a load replaced
;;         another entry
;;   SB-5  the copy and the state a save uses are one publication's: a commit
;;         between two saves, the second save's loads name the newer publication
;;
;; The daemon harness is store-memory.sc's (a hand-built store: writer W, eight
;; records in three segments).
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia reduce) (theourgia trace)
        (only (theourgia store) open-and-reduce store-memory-provider! store-memory-copy)
        (only (theourgia digest) sha256 bytevector->hex)
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
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/store-copy-" pid-text))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/scopy" pid-text))
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

;; ---- what a request's log slice says --------------------------------------------
;; ONE REQUEST THROUGH THE DAEMON: -> (<answer datum> . <the log lines it added>)
(define (through* . args)
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
                 (substring t (min before (string-length t)) (string-length t)))))
    (cons answer (lines-of text))))
(define (lines-with ls prefix) (filter (lambda (l) (has-prefix? l prefix)) ls))
;; each load's (replayed-or-extended n scanned m)
(define (load-counts ls) (map (lambda (l) (let ((d (read-or-false l))) (and d (caddr d)))) (lines-with ls "(trace load-memory")))
(define (scanned-of ls) (map (lambda (c) (and (list? c) (list-ref c 3))) (load-counts ls)))
;; the publication each load-on-copy line names
(define (copy-names ls) (map (lambda (l) (let ((d (read-or-false l))) (and d (caddr d)))) (lines-with ls "(trace load-on-copy")))
(define (routed-writer? ls) (pair? (lines-with ls "(trace routed write writer)")))
;; how the memory answered segment SEG of W, in order, per memory line
(define (memory-answers ls seg)
  (filter values
          (map (lambda (l) (let ((d (read-or-false l)))
                             (and d (list? (caddr d)) (= 4 (length (caddr d)))
                                  (equal? (format "~a" (cadr (caddr d))) W) (eqv? (caddr (caddr d)) seg)
                                  (cadddr (caddr d)))))
               (lines-with ls "(trace memory ("))))
(define (published-seqs ls)
  (map (lambda (l) (let ((d (read-or-false l))) (and d (caddr d)))) (lines-with ls "(trace published")))

;; A SAVE AS THE EXTENSION SENDS IT: the block's version and the cut read from
;; the daemon, then write --based-on <version> --working-cut <cut>
(define save-writer "sbw")
(define (field-of answer key) (let ((x (and (pair? answer) (assq key (cdr answer))))) (and x (cadr x))))
(define (save! id text)
  (let* ((r (car (through* "read" id)))
         (version (field-of r 'version))
         (cut (field-of r 'cut)))
    (through* "write" id text "--based-on" version "--working-cut" (format "~s" cut) "--writer" save-writer)))

;; ---- SB-1: a save through a warm daemon reads the copy ---------------------------
(build-store!)
(printf "SB-1 the warm daemon: ~s\n" (start-daemon! ""))
(define id1 (car (ids-now)))
(define sb1 (save! id1 "sb-1"))
(printf "SB-1 answer ~s; loads ~s; on copy ~s\n" (car sb1) (load-counts (cdr sb1)) (copy-names (cdr sb1)))
(want "SB-1 a --based-on save through a warm daemon: a writer process ran it, its two loads read the copy (one load-on-copy line each) and scanned no segment"
      (list (and (pair? (car sb1)) (car (car sb1))) (routed-writer? (cdr sb1)) (scanned-of (cdr sb1)) (length (copy-names (cdr sb1))))
      '(ok #t (0 0) 2))
;; UNDER only:replay-scan the witness scans every replayed segment again and
;; compares: its parse lines appear inside the loads, and no degeneracy line
(printf "SB-1 under only:replay-scan: ~s\n" (start-daemon! "THEOURGIA_DEGENERACY=only:replay-scan"))
(define sb1w (save! id1 "sb-1 witness"))
(printf "SB-1 witness answer ~s; loads ~s; parse lines ~a\n" (car sb1w) (load-counts (cdr sb1w))
        (length (lines-with (cdr sb1w) "(trace parse")))
(want "SB-1 under only:replay-scan: the same save, every replay of the copy compared with a scan (the scan's parse lines there) and no difference named"
      (list (and (pair? (car sb1w)) (car (car sb1w))) (scanned-of (cdr sb1w)) (length (copy-names (cdr sb1w)))
            (> (length (lines-with (cdr sb1w) "(trace parse")) 0)
            (length (lines-with (cdr sb1w) "degeneracy:")))
      '(ok (0 0) 2 #t 0))
(stop-daemon!)

;; ---- SB-2 and SB-3: a segment no publication has seen ----------------------------
;; THE PROBE FAILS (THEOURGIA_FAULT=probe-raise), so a change on disk asks the
;; store process for no fold and no publication: the copy the cell holds is
;; the start-up one. Segment 4 (records 9 and 10) is then written beside the
;; daemon: for the copy it is a miss.
(build-store!)
(printf "SB-2 the daemon with the probe failing: ~s\n" (start-daemon! "THEOURGIA_INJECT=on THEOURGIA_FAULT=probe-raise@conn"))
(define id2 (car (ids-now)))
(put! (segment-path 4) (records 9 10 "w"))
(define sb2a (save! id2 "sb-2 a"))
(define sb2b (save! id2 "sb-2 b"))
(printf "SB-2 first save ~s loads ~s seg 4 ~s; second ~s loads ~s seg 4 ~s\n"
        (car sb2a) (load-counts (cdr sb2a)) (memory-answers (cdr sb2a) 4)
        (car sb2b) (load-counts (cdr sb2b)) (memory-answers (cdr sb2b) 4))
;; A MISS IS DECODED ONCE PER LOAD: validation scans segment 4 and delivery
;; replays it from the load's own overlay; the next load, and the next save,
;; meet the copy as it was and scan it again
(want "SB-2 two saves after segment 4 appeared: each load reads the copy and scans segment 4 once (validation; delivery replays it from the load's overlay) and nothing else, the second save as the first (the miss was not stored into the copy, and no overlay outlived its load)"
      (list (map (lambda (s) (and (pair? (car s)) (car (car s)))) (list sb2a sb2b))
            (scanned-of (cdr sb2a)) (scanned-of (cdr sb2b))
            (memory-answers (cdr sb2a) 4) (memory-answers (cdr sb2b) 4)
            (length (copy-names (cdr sb2a))) (length (copy-names (cdr sb2b))))
      '((ok ok) (1 1) (1 1) (scanned replayed scanned replayed) (scanned replayed scanned replayed) 2 2))
;; THE STORE PROCESS'S NEXT LOAD (check is its request, a load) meets segment 4
;; for the first time: it scans it
(define sb3 (through* "check"))

(printf "SB-3 check ~s loads ~s seg 4 ~s\n" (car sb3) (load-counts (cdr sb3)) (memory-answers (cdr sb3) 4))
(want "SB-3 then the store process's own load scans segment 4: nothing the writer process's loads read was stored into the store process's memory"
      (list (length (copy-names (cdr sb3))) (and (pair? (memory-answers (cdr sb3) 4)) (car (memory-answers (cdr sb3) 4))))
      '(0 scanned))
(stop-daemon!)

;; ---- SB-4: a copy is never changed by the memory it was taken from ----------------
;; In this process, the memory handed to every load (store-memory-provider!),
;; as the store process's is. A MIRROR, M2, whose manifest names its segment
;; with a WRONG hash: a load hashes the segment (the entry made, its digest
;; held) and cuts it there, never scanning it. The copy is taken then. The
;; manifest is corrected and the next load scans the segment: the memory
;; completes that same entry in place (found, transcript). Then segment 3 of W
;; is replaced and a load replaces its entry. The copy's entries are compared,
;; field by field (the bytes and transcripts by eq?), with what they were.
(build-store!)
(define M2 "mmmm2222")
(define m2-dir (string-append store "/writers/" M2))
(define m2-bytes (records 1 2 "m"))
(system (string-append "mkdir -p '" m2-dir "'"))
(put! (string-append m2-dir "/" (segment-file-name 1)) m2-bytes)
(define (m2-manifest! sha)
  (put! (string-append m2-dir "/published.sexp") (string->utf8 (format "((1 ~s 1 2))\n" sha))))
(m2-manifest! (make-string 64 #\0))
(define sb4-memory (make-load-memory))
(store-memory-provider! (lambda () sb4-memory))
(caught (open-and-reduce store))
(define sb4-copy (store-memory-copy))
(define (entry-view entries)
  (map (lambda (e) (list (car e) (list-ref e 3) (list-ref e 4) (list-ref e 6))) entries))
(define sb4-before (memory-copy-entries sb4-copy))
(printf "SB-4 the copy as taken: ~s\n" (entry-view sb4-before))
(m2-manifest! (bytevector->hex (sha256 m2-bytes)))
(caught (open-and-reduce store))
(put! (segment-path 3) (records 7 8 "x"))
(caught (open-and-reduce store))
(define sb4-after (memory-copy-entries sb4-copy))
(define sb4-live (memory-copy-entries (store-memory-copy)))
(store-memory-provider! (lambda () #f))
(define (same-entries? a b)
  (and (= (length a) (length b))
       (for-all (lambda (x y)
                  (and (equal? (car x) (car y)) (eq? (list-ref x 1) (list-ref y 1)) (eq? (list-ref x 2) (list-ref y 2))
                       (equal? (list-tail x 3) (list-tail y 3))))
                a b)))
(define (entry-at entries key) (find (lambda (e) (equal? (car e) key)) entries))
(printf "SB-4 the memory now: ~s\n" (entry-view sb4-live))
(want "SB-4 the copy holds M2's segment as it was when taken (hashed, not scanned), though the memory has since completed that entry, and W's segment 3 as it was, though the memory has replaced it"
      (list (same-entries? sb4-before sb4-after)
            (let ((e (entry-at sb4-after (cons M2 1)))) (and e (list-ref e 3)))
            (let ((e (entry-at sb4-live (cons M2 1)))) (and e (list-ref e 3)))
            (let ((a (entry-at sb4-after (cons W 3))) (b (entry-at sb4-live (cons W 3))))
              (and a b (not (eq? (list-ref a 1) (list-ref b 1))))))
      '(#t new remembered #t))

;; ---- SB-5: the copy and the state are one publication's ---------------------------
(build-store!)
(printf "SB-5 the daemon: ~s\n" (start-daemon! ""))
(define id5 (car (ids-now)))
(define sb5a (save! id5 "sb-5 a"))
(define sb5c (through* "insert" "--title" "between"))
(define sb5b (save! id5 "sb-5 b"))
(printf "SB-5 first save on ~s; the commit ~s published ~s; second save on ~s\n"
        (copy-names (cdr sb5a)) (car sb5c) (published-seqs (cdr sb5c)) (copy-names (cdr sb5b)))
(want "SB-5 a commit between two saves: the second save's loads name the publication the commit made, the first's the one before it"
      (let ((a (copy-names (cdr sb5a))) (b (copy-names (cdr sb5b))) (p (published-seqs (cdr sb5c))))
        (list (and (pair? (car sb5c)) (car (car sb5c)))
              (and (= 2 (length a)) (equal? (car a) (cadr a)))
              (and (= 2 (length b)) (equal? (car b) (cadr b)))
              (and (pair? b) (pair? p) (eqv? (car (car b)) (car (reverse p))))
              (and (pair? a) (pair? b) (< (car (car a)) (car (car b))))))
      '(ok #t #t #t #t))
(stop-daemon!)

(set! cleaned? #t)
(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\nstore-copy complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
