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

;; A CLIENT GIVES UP ON SILENCE, NOT ON A CLOCK (client.sc call!, daemon.sc
;; await-terminal, the ffi's socket primitives and progress!).
;;
;; A socket client waits while bytes move: the answer's, or the spaces a
;; version-2 connection's daemon writes every K while the request's work
;; reports progress. With nothing for W it answers
;; (error transport-unknown (reason silent) (window W) (elapsed ms)).
;;
;; THE CLOCKS ARE SHORTENED FOR THE ROWS (THEOURGIA_SILENCE_MS 2000,
;; THEOURGIA_KEEPALIVE_MS 400, the injection build only), except LV-1, which
;; runs at the real W, and LV-15's row without injection, which reads the
;; real window. Every silent row asserts the window clause and the elapsed
;; time, and the wall time the client took.
;; EVERY CLIENT IS A CHILD PROCESS with its own environment, because the
;; clocks are read when the library loads; the command line, a program
;; calling call!, and the MCP shell.
;; THE DAEMONS RUN WITH IDLE EXIT OFF, as every fixture's do: under a short
;; test window a request crossing an idle exit or a rebind could go silent
;; for a reason other than the one a row tests (B11).
;; THE PEERS THAT ARE NOT DAEMONS are perl stubs: an old daemon that
;; refuses version 2, one that never answers, one that never reads, one that
;; answers like an origin-core refusal.
;; LV-6 compares this tree's daemon with a base tree's (THEOURGIA_BASE_LIBDIR,
;; as change-stream-guard.sc does); without it that row prints SKIP and why.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (theourgia log) log-publish! segment-sha atomic-write!)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia client) request-frame)
        (only (theourgia render) render-wire)
        (only (theourgia ffi) progress-sink-set!)
        (only (theourgia store) open-and-reduce)
        (only (theourgia json) string->json))

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
(define scratch
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/lv-" pid-text))
(define sockets
  (string-append (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/lv-" pid-text))
(system (string-append "rm -rf " scratch " " sockets "; mkdir -p " scratch " " sockets))
(define lib-env (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                               " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"))
(define test-clocks "THEOURGIA_SILENCE_MS=2000 THEOURGIA_KEEPALIVE_MS=400")
(define W 2000)
(define K 400)
;; LV-6b's stub writes a refusal's trailing byte this long after the refusal's line,
;; so the client has read the line whole first: a timing assumption.
(define trailing-byte-ms 300)

(define (sh . xs) (system (apply string-append xs)))
(define (q s)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
(define (file-text p)
  (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
;; Every index at which NEEDLE starts in TEXT, in order.
(define (positions text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i n) m) (reverse out))
            ((string=? needle (substring text i (+ i n))) (loop (+ i 1) (cons i out)))
            (else (loop (+ i 1) out))))))
(define (count-of text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) m) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i n) (+ k 1)))
            (else (loop (+ i 1) k))))))
(define (first-datum p)
  (guard (e (#t 'unreadable)) (let ((d (call-with-input-file p read))) (if (eof-object? d) 'no-answer d))))
(define (await-file p ms)
  (let loop ((k 0)) (cond ((file-exists? p) #t) ((> (* k 50) ms) #f) (else (sleep-ms 50) (loop (+ k 1))))))
(define (touch! p) (call-with-output-file p (lambda (o) (write 'go o)) 'truncate))
(define (between? lo x hi) (and (number? x) (<= lo x) (< x hi)))
;; THE SILENT ANSWER'S PARTS: (window elapsed), or what came instead.
(define (silent-parts a)
  (if (and (pair? a) (eq? 'error (car a)) (pair? (cdr a)) (eq? 'transport-unknown (cadr a))
           (equal? (caddr a) '(reason silent)))
      (let ((w (assq 'window (cdddr a))) (e (assq 'elapsed (cdddr a))))
        (list (and w (cadr w)) (and e (cadr e))))
      (list 'not-silent a)))

;; ---- stores ---------------------------------------------------------------------

(define (cli-local . words)
  (string-append lib-env " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc"
                 (apply string-append (map (lambda (w) (string-append " " (q w))) words))
                 " > /dev/null 2>&1 < /dev/null"))
(define store-n 0)
;; A store holding a tree of N small files, imported.
(define (fresh-store! n)
  (set! store-n (+ store-n 1))
  (let* ((d (string-append scratch "/s" (number->string store-n)))
         (st (string-append d "/store")) (tree (string-append d "/tree")))
    (sh "mkdir -p " st " " tree)
    (do ((i 0 (+ i 1))) ((= i n))
      (call-with-output-file (string-append tree "/f" (number->string i) ".sc")
        (lambda (p) (fprintf p "(define (f~a x) (+ x ~a))\n" i i))))
    (sh (cli-local "init" "--store" st))
    (when (> n 0) (sh (cli-local "import-code" tree "--store" st)))
    st))
;; ANOTHER WRITER'S N SEGMENTS, published into ST, for a fold with N segments to validate.
(define (mirror-segments! st writer n)
  (do ((k 1 (+ k 1))) ((> k n))
    (let ((bytes (encode-record k (+ 1789000000000 k) "peer" '()
                                (storable-encode (list 'put (list '(kind . section)
                                                                  (cons 'title (string-append "m" (number->string k)))
                                                                  '(parent . root) (cons 'ord k)))))))
      (log-publish! st writer k bytes (segment-sha bytes)))))

;; HOW LONG ONE FILE OF AN EXPORT TAKES HERE (an atomic write), so a row can
;; size a natural export to a duration: the flushes decide it, not the CPU.
(define per-file-ms
  (let ((dir (string-append scratch "/calibrate")) (t0 (real-time)))
    (sh "mkdir -p " dir)
    (do ((i 0 (+ i 1))) ((= i 40))
      (atomic-write! (string-append dir "/c" (number->string i)) (make-bytevector 64 65) 'working))
    (max 1 (/ (- (real-time) t0) 40.0))))
(define (files-for ms) (max 4 (exact (ceiling (/ ms per-file-ms)))))
;; The same for an export each of whose files a fault pauses 100 ms more.
(define (paused-files-for ms) (max 4 (exact (ceiling (/ ms (+ 100 per-file-ms))))))
(printf "liveness sizing: ~a ms per file written here~%" per-file-ms)

;; ---- daemons --------------------------------------------------------------------

(define daemon-n 0)
(define runners '())
;; -> (st sk runner log pidfile), or (no-daemon <log text>).
(define (start-daemon! st env . lib)
  (set! daemon-n (+ daemon-n 1))
  (let* ((t (string-append pid-text "-" (number->string daemon-n)))
         (sk (string-append sockets "/d" (number->string daemon-n) ".sock"))
         (rn (string-append scratch "/d" t ".sc"))
         (lg (string-append scratch "/d" t ".log"))
         (pf (string-append scratch "/d" t ".pid"))
         (libs (if (pair? lib) (car lib) (getenv "CHEZSCHEMELIBDIRS"))))
    (sh "rm -f " sk " " pf)
    (set! runners (cons rn runners))
    (call-with-output-file rn
      (lambda (p)
        (for-each (lambda (l) (display l p) (newline p))
          (list "(import (chezscheme) (theourgia daemon))"
                (format "(call-with-output-file ~s (lambda (p) (write (get-process-id) p)) 'truncate)" pf)
                (format "(serve ~s ~s)" st sk))))
      'truncate)
    (sh "env THEOURGIA_TRACE=1 THEOURGIA_INJECT=on THEOURGIA_PROBE=off " env " "
        "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
        " sh -c 'scheme --script " rn " > " lg " 2>&1' &")
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg pf))
            ((or (> k 600) (contains? (file-text lg) "Exception")) (list 'no-daemon (file-text lg)))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (daemon? d) (and (pair? d) (string? (car d))))
(define (d-sock d) (list-ref d 1))
(define (d-log d) (file-text (list-ref d 3)))
;; THE TRACE LINES OF A LOG, each read as a datum: (trace <op> <subject> <bytes>).
(define (trace-data log)
  (let loop ((ls (let split ((cs (string->list log)) (line '()) (out '()))
                   (cond ((null? cs) (reverse (if (null? line) out (cons (list->string (reverse line)) out))))
                         ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse line)) out)))
                         (else (split (cdr cs) (cons (car cs) line) out)))))
             (out '()))
    (cond ((null? ls) (reverse out))
          ((and (>= (string-length (car ls)) 7) (string=? (substring (car ls) 0 7) "(trace "))
           (let ((d (guard (e (#t #f)) (read (open-string-input-port (car ls))))))
             (loop (cdr ls) (if (and (list? d) (= (length d) 4)) (cons d out) out))))
          (else (loop (cdr ls) out)))))
(define (d-pid d) (let ((t (file-text (list-ref d 4)))) (and (> (string-length t) 0) (string->number t))))
(define (stop-daemon! d) (when (daemon? d) (sh "pkill -f " (list-ref d 2) " 2>/dev/null")))
(define (not-a-reading! what d)
  (if (string? d)
      (printf "NOT A READING: ~a: ~a~%" what d)
      (printf "NOT A READING: ~a: a daemon never created its socket~%~a~%" what (if (pair? d) (cadr d) d)))
  (for-each (lambda (rn) (sh "pkill -f " rn " 2>/dev/null")) runners)
  (stop-stubs!)
  (printf "liveness complete~%")
  (exit 2))

;; ---- clients, each a child process ----------------------------------------------

(define out-n 0)
(define (next-out) (set! out-n (+ out-n 1)) (string-append scratch "/o" (number->string out-n)))
;; THE COMMAND LINE: -> (datum stderr-text wall-ms rc).
(define (cli env . args)
  (let ((o (next-out)) (t0 (real-time)))
    (let ((rc (sh "env THEOURGIA_INJECT=on THEOURGIA_TRACE=1 " env " " lib-env
                  " perl -e 'alarm 120; exec @ARGV' scheme --script ../theourgia.sc"
                  (apply string-append (map (lambda (a) (string-append " " (q a))) args))
                  " --wire > " o " 2> " o ".err < /dev/null")))
      (list (first-datum o) (file-text (string-append o ".err")) (- (real-time) t0) rc))))
;; The same in the background; (cli-result o) reads it once it has answered.
(define (cli-bg! env . args)
  (let ((o (next-out)))
    (sh "( env THEOURGIA_INJECT=on THEOURGIA_TRACE=1 " env " " lib-env
        " perl -e 'alarm 120; exec @ARGV' scheme --script ../theourgia.sc"
        (apply string-append (map (lambda (a) (string-append " " (q a))) args))
        " --wire > " o ".tmp 2> " o ".err < /dev/null; mv " o ".tmp " o " ) &")
    o))
(define (cli-result o ms)
  (if (await-file o ms) (list (first-datum o) (file-text (string-append o ".err"))) (list 'no-answer "")))
;; A PROGRAM: TEXT, a sequence of forms, run with ENV; -> (stdout datum, stderr text, wall ms).
(define (program env text)
  (let ((o (next-out)) (t0 (real-time)))
    (call-with-output-file (string-append o ".sc") (lambda (p) (put-string p text)) 'truncate)
    (sh "env THEOURGIA_INJECT=on THEOURGIA_TRACE=1 " env " " lib-env
        " perl -e 'alarm 120; exec @ARGV' scheme --script " o ".sc > " o " 2> " o ".err < /dev/null")
    (list (first-datum o) (file-text (string-append o ".err")) (- (real-time) t0))))
;; A frame this tree builds, as a string for a program's source.
(define (frame-literal st verb args version)
  (format "~s" (utf8->string (request-frame st verb args (list (cons 'actor "tester") (cons 'version version))))))

;; ---- raw bytes on one connection, in this process ----------------------------------
;;
;; -> (bytes how): every byte the daemon wrote for FRAME, until it closed
;; (eof), or QUIET ms passed after a newline (quiet), or LIMIT ms in all (limit).
(define (has-newline? bv)
  (let loop ((i 0)) (cond ((>= i (bytevector-length bv)) #f) ((= 10 (bytevector-u8-ref bv i)) #t) (else (loop (+ i 1))))))
(define (join bvs)
  (let* ((n (apply + (map bytevector-length bvs))) (out (make-bytevector n)))
    (let loop ((bs bvs) (at 0))
      (if (null? bs) out
          (begin (bytevector-copy! (car bs) 0 out at (bytevector-length (car bs)))
                 (loop (cdr bs) (+ at (bytevector-length (car bs)))))))))
(define (raw-collect sk frame quiet limit)
  (let ((me self))
    (spawn
      (lambda ()
        (connect! sk)
        (receive
          (after 4000 (send me '(raw #vu8() no-connect)))
          (`(connected ,p ,ref)
           (conn-read-start! ref)
           (conn-write! ref frame 'raw)
           (let ((t0 (real-time)))
             (let loop ((acc '()) (last #f))
               (receive
                 (after (if last
                            (max 0 (- (+ last quiet) (real-time)))
                            (max 0 (- (+ t0 limit) (real-time))))
                   (send me (list 'raw (join (reverse acc)) (if last 'quiet 'limit))))
                 (`(written ,r ,tok ,st) (loop acc last))
                 (`(data ,r ,bv)
                  (let ((acc (cons bv acc)))
                    (loop acc (if (or last (has-newline? bv)) (real-time) #f))))
                 (`(eof ,r) (send me (list 'raw (join (reverse acc)) 'eof)))
                 (`#(DOWN ,w ,why) (loop acc last)))))))))
    (receive (after (+ limit 10000) (list #vu8() 'collector-silent))
             (`(raw ,b ,how) (list b how)))))
(define (leading-spaces bv)
  (let loop ((i 0)) (if (and (< i (bytevector-length bv)) (= 32 (bytevector-u8-ref bv i))) (loop (+ i 1)) i)))
;; THE BYTES' SHAPE: (spaces <n> lines <newlines> after <bytes after the first newline>)
(define (shape bv)
  (let* ((n (leading-spaces bv))
         (nl (let loop ((i n)) (cond ((>= i (bytevector-length bv)) #f) ((= 10 (bytevector-u8-ref bv i)) i) (else (loop (+ i 1)))))))
    (list 'spaces n
          'lines (let loop ((i 0) (k 0)) (if (>= i (bytevector-length bv)) k (loop (+ i 1) (if (= 10 (bytevector-u8-ref bv i)) (+ k 1) k))))
          'after (if nl (- (bytevector-length bv) nl 1) 'no-newline))))
(define (v2-frame st verb args) (request-frame st verb args (list (cons 'actor "tester") (cons 'version 2))))

;; ---- perl stubs: peers that are not this daemon -------------------------------------

(define stub-script (string-append scratch "/stub.pl"))
(call-with-output-file stub-script
  (lambda (p)
    (put-string p "use strict; use IO::Socket::UNIX; use Socket;
my ($path, $mode, $dir, $pause) = @ARGV;
sub slurp { my $f = shift; open(my $h, '<', $f) or return ''; local $/; my $t = <$h>; close $h; return $t }
unlink $path;
$SIG{PIPE} = 'IGNORE';
my $srv = IO::Socket::UNIX->new(Type => SOCK_STREAM, Local => $path, Listen => 8) or die \"listen: $!\";
open(my $pf, '>', \"$path.pid\"); print $pf $$; close $pf;
while (my $c = $srv->accept) {
  $c->autoflush(1);
  if ($mode eq 'noread') { sleep 120; next }
  my $line = <$c>;
  if ($mode eq 'old') {
    open(my $lg, '>>', \"$path.frames\"); print $lg $line; close $lg;
    if (defined $line && $line =~ /^\\(request 2 /) { print $c slurp(\"$dir/refusal\"); close $c; next }
    print $c slurp(\"$dir/ok\"); close $c; next }
  if ($mode eq 'silent') { sleep 120; next }
  if ($mode eq 'lookalike') { print $c slurp(\"$dir/lookalike\"); close $c; next }
  if ($mode eq 'cut') { my $t = slurp(\"$dir/refusal\"); chomp $t; print $c $t; close $c; next }
  if ($mode eq 'after-one') { print $c slurp(\"$dir/refusal\") . 'x'; close $c; next }
  if ($mode eq 'after') { print $c slurp(\"$dir/refusal\"); select(undef, undef, undef, $pause / 1000); print $c 'x'; close $c; next }
}
"))
  'truncate)
(define replies (string-append scratch "/replies"))
(sh "mkdir -p " replies)
(define (reply! name datum) (call-with-output-file (string-append replies "/" name) (lambda (p) (put-string p (render-wire datum))) 'truncate))
;; TODAY'S REFUSAL OF A VERSION IT DOES NOT KNOW, as an old daemon renders it.
(reply! "refusal" (list 'answer (list 'stdout (render-wire '(error bad-request (reason malformed-envelope))))
                        '(stderr "") '(exit 1) '(origin transport)))
(reply! "ok" (list 'answer (list 'stdout (render-wire '(ok (old)))) '(stderr "") '(exit 0) '(origin core)))
(reply! "lookalike" (list 'answer (list 'stdout (render-wire '(error bad-request (reason malformed-envelope))))
                          '(stderr "") '(exit 1) '(origin core)))
(define stubs '())
;; A mode's stub is started once; a later row asking for it shares it.
(define (start-stub! mode)
  (let ((sk (string-append sockets "/stub-" mode ".sock")))
    (if (member sk stubs) sk (start-stub-at! sk mode))))
(define (start-stub-at! sk mode)
  (sh "rm -f " sk " " sk ".pid " sk ".frames; perl " stub-script " " sk " " mode " " replies " " (number->string trailing-byte-ms) " > /dev/null 2>&1 &")
  (set! stubs (cons sk stubs))
  (let up ((k 0)) (cond ((file-exists? sk) sk) ((> k 100) #f) (else (sleep-ms 50) (up (+ k 1))))))
(define (stop-stubs!)
  (for-each (lambda (sk) (let ((t (file-text (string-append sk ".pid"))))
                           (when (> (string-length t) 0) (sh "kill " t " 2>/dev/null"))))
            stubs))

(start-scheduler
  (lambda ()

    ;; ---- LV-15 invalid settings are refused at load ------------------------------
    (let ((zero (program "THEOURGIA_SILENCE_MS=0" "(import (chezscheme) (theourgia client)) (write silence-window-ms)"))
          (word (program "THEOURGIA_SILENCE_MS=abc" "(import (chezscheme) (theourgia client)) (write silence-window-ms)"))
          (set (program "THEOURGIA_SILENCE_MS=1234" "(import (chezscheme) (theourgia client)) (write silence-window-ms)"))
          (plain (program "THEOURGIA_INJECT=off THEOURGIA_SILENCE_MS=1234" "(import (chezscheme) (theourgia client)) (write silence-window-ms)")))
      (want "LV-15 THEOURGIA_SILENCE_MS: 0 and a word are refused at load, 1234 is the window under injection, and without injection it is not read (30000)"
            (list (car zero) (contains? (cadr zero) "THEOURGIA_SILENCE_MS")
                  (car word) (contains? (cadr word) "THEOURGIA_SILENCE_MS")
                  (car set) (car plain))
            '(no-answer #t no-answer #t 1234 30000)))
    (let* ((st (fresh-store! 0))
           (d (start-daemon! st "THEOURGIA_SILENCE_MS=2000 THEOURGIA_KEEPALIVE_MS=2000")))
      (want "LV-15 a daemon whose keepalive is not below the window refuses to load, and says so"
            (list (daemon? d) (contains? (if (daemon? d) (d-log d) (cadr d)) "THEOURGIA_KEEPALIVE_MS must be below"))
            '(#f #t))
      (stop-daemon! d))

    ;; ---- LV-2 a worker held alive, making no progress -----------------------------
    (let* ((st (fresh-store! 0))
           (rel (string-append scratch "/hold-2.release"))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_HOLD_MS=60000 THEOURGIA_HOLD=executor-before-verb:" rel))))
      (unless (daemon? d) (not-a-reading! "LV-2" d))
      (let* ((r (cli test-clocks "insert" "--under" "root" "--title" "held" "--store" st "--socket" (d-sock d)))
             (parts (silent-parts (car r))))
        (want "LV-2 a request whose worker is held without progress is silent at W: window 2000, elapsed in [W, W+K+1000), wall in [W, W+K+2000), and no keepalive arrived"
              (list (car parts) (between? W (cadr parts) (+ W K 1000)) (between? W (caddr r) (+ W K 2000))
                    (count-of (cadr r) "(trace keepalive-stripped"))
              (list W #t #t 0)))
      ;; LV-15 without injection: the names are not read, the window is the real one
      (let* ((o (next-out)) (t0 (real-time))
             (rc (sh "env THEOURGIA_INJECT=off THEOURGIA_SILENCE_MS=2000 " lib-env
                     " perl -e 'alarm 120; exec @ARGV' scheme --script ../theourgia.sc insert --under root --title held2"
                     " --store " (q st) " --socket " (q (d-sock d)) " --wire > " o " 2> " o ".err < /dev/null"))
             (wall (- (real-time) t0))
             (parts (silent-parts (first-datum o))))
        (want "LV-15 a client built without injection ignores THEOURGIA_SILENCE_MS: the silent answer's window is 30000 and it came after 30 s"
              (list (car parts) (between? 30000 (cadr parts) 32000) (>= wall 30000))
              '(30000 #t #t)))
      (touch! rel)
      (stop-daemon! d))

    ;; ---- LV-3 the whole process blocked -------------------------------------------
    (let* ((st (fresh-store! 0))
           (rel (string-append scratch "/hold-3.release"))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_HOLD_MS=8000 THEOURGIA_HOLD=executor-block-before-verb:" rel))))
      (unless (daemon? d) (not-a-reading! "LV-3" d))
      (let* ((r (cli test-clocks "insert" "--under" "root" "--title" "blocked" "--store" st "--socket" (d-sock d)))
             (parts (silent-parts (car r))))
        (want "LV-3 a request whose daemon is blocked is silent at about W: window 2000, elapsed in [W, W+1000)"
              (list (car parts) (between? W (cadr parts) (+ W 1000)))
              (list W #t)))
      (touch! rel)
      (stop-daemon! d))

    ;; ---- LV-1 a request that progresses past the real W ----------------------------
    (let* ((st (fresh-store! 400))
           (d (start-daemon! st "THEOURGIA_FAULT=export-slow@conn")))
      (unless (daemon? d) (not-a-reading! "LV-1" d))
      (let* ((out (string-append scratch "/lv1-export"))
             (r (cli "" "export-code" out "--store" st "--socket" (d-sock d))))
        (want "LV-1 at the real W, an export that progresses for about 42 s answers ok with every file, after more than 30 s, keepalives stripped on the way"
              (list (car r) (> (caddr r) 31000) (> (count-of (cadr r) "(trace keepalive-stripped") 0))
              '((ok (files 400)) #t #t)))
      (stop-daemon! d))

    ;; ---- LV-2b a request queued behind one that progresses ----------------------------
    (let* ((st (fresh-store! 40))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=export-slow@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-2b" d))
      (let* ((first (cli-bg! test-clocks "export-code" (string-append scratch "/lv2b-export") "--store" st "--socket" (d-sock d)))
             (_ (sleep-ms 500))
             (second (cli test-clocks "insert" "--under" "root" "--title" "queued" "--store" st "--socket" (d-sock d)))
             (one (cli-result first 30000)))
        (want "LV-2b behind an export that progresses past W, a queued request borrows nothing and goes silent at W; the export answers ok"
              (list (car (silent-parts (car second))) (car one))
              (list W '(ok (files 40)))))
      (stop-daemon! d))

    ;; ---- LV-2c the refusal branch clears the association --------------------------------
    (let* ((st (fresh-store! 40))
           (d (start-daemon! st "THEOURGIA_FAULT=export-slow@conn")))
      (unless (daemon? d) (not-a-reading! "LV-2c" d))
      (let* ((first (cli-bg! "" "export-code" (string-append scratch "/lv2c-export") "--store" st "--socket" (d-sock d)))
             (_ (sleep-ms 500))
             (second (cli-bg! "" "insert" "--under" "root" "--title" "refused" "--store" st "--socket" (d-sock d)))
             (_ (sleep-ms 500))
             (_ (sh "kill -TERM " (number->string (d-pid d))))
             (two (cli-result second 30000))
             (log (d-log d)))
        ;; NOTE: EACH TICKET'S OWN association-cleared, the executed export's and
        ;; the refused insert's, read from the line that names it: a count of
        ;; such lines would be satisfied by two for one ticket.
        (want "LV-2c a request refused at may-execute? (the daemon draining) is answered draining, its executor traces executing and executing-done like an executed one's, and each of the two tickets' association-cleared says #t"
              (let* ((data (trace-data log))
                     (tickets (map caddr (filter (lambda (d) (eq? (cadr d) 'executing)) data))))
                (list (car two) (length tickets) (count-of log "(trace executing-done ")
                      (map (lambda (t)
                             (let ((c (filter (lambda (d) (and (eq? (cadr d) 'association-cleared) (equal? (caddr d) t))) data)))
                               (if (= (length c) 1) (cadddr (car c)) (list 'lines (length c)))))
                           tickets)))
              '((error draining) 2 2 (#t #t))))
      (stop-daemon! d))

    ;; ---- LV-4 a keepalive pending when the answer arrives ----------------------------
    (let* ((n (paused-files-for (+ K 700)))
           (st (fresh-store! n)))
      (define (run fault)
        (let ((d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=" fault "@conn"))))
          (unless (daemon? d) (not-a-reading! "LV-4" d))
          (let* ((out (string-append scratch "/lv4-" fault))
                 (r (raw-collect (d-sock d) (v2-frame st 'export-code (list out)) 1500 20000))
                 (log (d-log d)))
            (stop-daemon! d)
            (list (shape (car r)) (cadr r) (contains? log (string-append "(trace fault " fault))
                  (length (directory-list out)) log))))
      ;; NOTE: THE OVERLAP IS READ FROM THE DAEMON'S TRACE, NOT ASSUMED FROM THE
      ;; SIZING: the withheld keepalive's line, then terminal-pending (the
      ;; answer arrived with it pending), then keepalive-completed for the same
      ;; ticket and keepalive (its completion released the answer), in that
      ;; order. keepalive-late's completion is sent only from inside that wait.
      (let* ((late (run "keepalive-late"))
             (data (trace-data (list-ref late 4)))
             (index-of (lambda (p) (let loop ((ds data) (i 0)) (cond ((null? ds) #f) ((p (car ds)) i) (else (loop (cdr ds) (+ i 1)))))))
             (issued (index-of (lambda (d) (and (eq? (cadr d) 'keepalive) (eqv? (caddr d) 1)))))
             (pending (index-of (lambda (d) (eq? (cadr d) 'terminal-pending))))
             (completed (index-of (lambda (d) (eq? (cadr d) 'keepalive-completed))))
             (same (and pending completed (equal? (cddr (list-ref data pending)) (cddr (list-ref data completed)))
                        (eqv? 1 (cadddr (list-ref data pending))))))
        (want "LV-4 a keepalive's completion held while the answer arrives: spaces, then exactly one answer line, nothing after; the fault fired"
              (list (> (list-ref (car late) 1) 0) (list-ref (car late) 3) (list-ref (car late) 5) (caddr late))
              '(#t 1 0 #t))
        (want "LV-4 and the trace orders it: keepalive 1, then terminal-pending, then keepalive-completed, the last two for the same ticket and keepalive 1"
              (list (and issued pending (< issued pending)) (and pending completed (< pending completed)) same)
              '(#t #t #t)))
      (let ((never (run "keepalive-never")))
        (want "LV-4 a keepalive that never completes: the connection closes after the bound with no answer, and the worker's files are all written"
              (list (> (list-ref (car never) 1) 0) (list-ref (car never) 3) (cadr never) (cadddr never))
              (list #t 0 'eof n)))
      (let ((stale (run "keepalive-stale")))
        (want "LV-4 stale completions (another connection's, an older token's) are ignored: spaces, then one answer"
              (list (> (list-ref (car stale) 1) 0) (list-ref (car stale) 3) (list-ref (car stale) 5) (caddr stale))
              '(#t 1 0 #t))))

    ;; ---- LV-5b a killed connection's count is reclaimed -------------------------------------
    ;; A KILLED ACTOR RUNS NO UNWINDS, so the count its wind would drop is
    ;; main's to reclaim when it sees the death. conn-killed-pending has the
    ;; daemon kill this connection at its first keepalive tick whose count
    ;; moved: that count is the executor's own bump for this ticket, so the
    ;; export is under way when the connection dies (a kill at the wait's
    ;; start could come before the executor began). The export pauses per
    ;; file and goes on; its files on disk say it finished, whatever the
    ;; executing-done line, which an unwinding executor writes too.
    (let* ((n (paused-files-for 2000))
           (st (fresh-store! n))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=conn-killed-pending@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-5b" d))
      (let* ((out (string-append scratch "/lv5b-export"))
             (r (raw-collect (d-sock d) (v2-frame st 'export-code (list out)) 1500 6000))
             (datum-at (lambda (log needle)
                         (let ((ps (positions log needle)))
                           (and (pair? ps)
                                (guard (e (#t #f)) (read (open-string-input-port (substring log (car ps) (string-length log)))))))))
             (log (let wait ((k 0))
                    (let* ((t (d-log d)) (rec (datum-at t "(trace progress-reclaimed ")))
                      (if (or (> k 400)
                              (and rec (exists (lambda (p) (> p (car (positions t "(trace progress-reclaimed ")))) (positions t "(trace executing-done "))))
                          t
                          (begin (sleep-ms 50) (wait (+ k 1)))))))
             (executing (datum-at log "(trace executing "))
             (reclaimed (datum-at log "(trace progress-reclaimed "))
             (ticket (and (pair? executing) (>= (length executing) 3) (caddr executing)))
             (done-after (let ((rp (positions log "(trace progress-reclaimed ")))
                           (and (pair? rp)
                                (exists (lambda (p)
                                          (let ((dd (guard (e (#t #f)) (read (open-string-input-port (substring log p (string-length log)))))))
                                            (and (> p (car rp)) (pair? dd) (equal? (caddr dd) ticket))))
                                        (positions log "(trace executing-done ")))))
             (in-order (let ((dp (positions log "(trace daemon-dispatch export-code"))
                             (ep (positions log "(trace executing "))
                             (fp (positions log "(trace fault conn-killed-pending"))
                             (rp (positions log "(trace progress-reclaimed ")))
                         (and (pair? dp) (pair? ep) (pair? fp) (pair? rp) (< (car dp) (car ep) (car fp) (car rp)))))
             (files (if (file-directory? out) (length (directory-list out)) 0))
             (later (cli test-clocks "outline" "--store" st "--socket" (d-sock d))))
        (want "LV-5b a connection killed while its version-2 export was under way: dispatch, the export's executing, the kill, main's reclaim of that ticket (progress-reclaimed), that ticket's executing-done; every export file on disk; and a later request on a new connection is answered"
              (list in-order
                    (and ticket (pair? reclaimed) (equal? (caddr reclaimed) ticket))
                    done-after
                    files
                    (and (pair? (car later)) (car (car later))))
              (list #t #t #t n 'ok)))
      (stop-daemon! d))

    ;; ---- LV-5 a connection-local read sends no keepalive --------------------------------
    (let* ((st (fresh-store! 2))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=conn-hold@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-5" d))
      (let ((r (raw-collect (d-sock d) (v2-frame st 'outline '()) 500 10000)))
        (want "LV-5 a version-2 read the connection answers itself, held 2.5 s, carries no keepalive: its first byte is the answer's"
              (list (list-ref (shape (car r)) 1) (list-ref (shape (car r)) 3))
              '(0 1)))
      (stop-daemon! d))

;; ---- LV-6 a version-1 request gets no keepalive, however long it progresses --------------
    (let* ((n (files-for (* 4 K)))
           (st (fresh-store! n))
           (d (start-daemon! st test-clocks)))
      (unless (daemon? d) (not-a-reading! "LV-6 v1" d))
      (let* ((out (string-append scratch "/lv6-v1-export"))
             (r (raw-collect (d-sock d) (request-frame st 'export-code (list out) '((actor . "tester"))) 1500 20000)))
        (want "LV-6 a version-1 export that progresses past K several times gets no keepalive: its first byte is the answer's, one line"
              (list (list-ref (shape (car r)) 1) (list-ref (shape (car r)) 3))
              '(0 1)))
      (stop-daemon! d))

    ;; ---- LV-8 a keepalive that fails stops the keepalives, and only them -------------------
    (let* ((n (paused-files-for (* 4 K)))
           (st (fresh-store! n)))
      (for-each
        (lambda (fault)
          (let ((d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=" fault "@conn"))))
            (unless (daemon? d) (not-a-reading! "LV-8" d))
            (let* ((out (string-append scratch "/lv8-" fault))
                   (r (cli test-clocks "export-code" out "--store" st "--socket" (d-sock d)))
                   (log (d-log d)))
              (stop-daemon! d)
              (want (string-append "LV-8 " fault ": one keepalive-failed line, no keepalive after the failed one, and the export runs to its end")
                    (list (count-of log "(trace keepalive-failed") (count-of log "(trace keepalive ")
                          (length (directory-list out)))
                    (list 1 (if (string=? fault "keepalive-raise") 0 1) n)))))
        '("keepalive-raise" "keepalive-error")))

    ;; ---- LV-13 a gathering pass and a validation pass that outlast W ---------------------
    (let* ((st (fresh-store! 30))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=gather-slow@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-13" d))
      (let ((r (cli test-clocks "export-code" (string-append scratch "/lv13-export") "--store" st "--socket" (d-sock d))))
        (want "LV-13 an export whose gathering alone takes 3 s, each file 100 ms, answers ok: the gathering reports progress before the first output"
              (car r) '(ok (files 30))))
      (stop-daemon! d))
    (let* ((st (fresh-store! 0))
           (_ (mirror-segments! st "zzzzmirr" 30))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=validate-slow@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-13 fold" d))
      (let ((r (cli test-clocks "insert" "--under" "root" "--title" "after-fold" "--store" st "--socket" (d-sock d))))
        (want "LV-13 a write whose fold validates 30 segments at 100 ms each answers ok: validation reports progress"
              (and (pair? (car r)) (car (car r))) 'ok))
      (stop-daemon! d))
    ;; A SEGMENT IS COUNTED ONCE JUDGED: one bump when its validation has
    ;; established its range, one when its delivery reached the boundary, and
    ;; none for a segment that cannot be read. In process, a sink counts
    ;; progress! over one fold of each of two stores alike but for one mirror
    ;; segment, readable in one and mode 000 in the other: the second counts
    ;; exactly two fewer. (A bump made before the judgement counts the
    ;; unreadable segment's validation: one fewer.)
    (let* ((count-fold (lambda (unreadable?)
                         (let* ((st (fresh-store! 0))
                                (_ (mirror-segments! st "zzzzmirr" 1))
                                (seg (string-append st "/writers/zzzzmirr"))
                                (files (filter (lambda (f) (and (> (string-length f) 5)
                                                                (string=? ".sexp" (substring f (- (string-length f) 5) (string-length f)))
                                                                (char-numeric? (string-ref f 0))))
                                               (directory-list seg)))
                                (n 0))
                           (when unreadable? (for-each (lambda (f) (sh "chmod 000 " (q (string-append seg "/" f)))) files))
                           (progress-sink-set! (lambda () (set! n (+ n 1))))
                           (guard (e (#t #f)) (open-and-reduce st))
                           (progress-sink-set! (lambda () #f))
                           (when unreadable? (for-each (lambda (f) (sh "chmod 644 " (q (string-append seg "/" f)))) files))
                           (list n (length files)))))
           (none (let* ((st (fresh-store! 0)) (n 0))
                   (progress-sink-set! (lambda () (set! n (+ n 1))))
                   (guard (e (#t #f)) (open-and-reduce st))
                   (progress-sink-set! (lambda () #f))
                   n))
           (good (count-fold #f))
           (bad (count-fold #t)))
      (want "LV-13 a segment is counted once judged: against the same store with no mirror, a readable segment adds two bumps (its validation and its delivery) and an unreadable one none"
            (list (cadr good) (cadr bad) (- (car good) none) (- (car bad) none))
            '(1 1 2 0)))
    ;; A SHORT DELIVERY IS NOT PROGRESS EITHER: the delivery's bump comes once
    ;; the segment reached the boundary its validation promised. A child
    ;; process counts progress! over one fold held after the delivery barrier
    ;; (as D-deliver); while held, the older of two mirror segments loses its
    ;; second half, so delivery reads readable bytes short of that extent and
    ;; the fold fails. Against a store with no mirror: healthy, the two
    ;; segments add four bumps (two validations, two deliveries); short, only
    ;; the two validations (none for the delivery that fell short, and the
    ;; mirror's delivery stops there).
    (let* ((child (lambda (st change)
                    (let ((o (next-out)) (rel (string-append scratch "/barrier-" (number->string out-n) ".release")))
                      (call-with-output-file (string-append o ".sc")
                        (lambda (p)
                          (put-string p (format "(import (chezscheme) (theourgia ffi) (theourgia store))
(define n 0)
(progress-sink-set! (lambda () (set! n (+ n 1))))
(define r (guard (e (#t 'failed)) (open-and-reduce ~s) 'ok))
(write (list n r))" st)))
                        'truncate)
                      (sh "rm -f " (q rel) " " (q rel) ".held; ( env THEOURGIA_INJECT=on THEOURGIA_HOLD=after-barrier:" (q rel) " " lib-env
                          " perl -e 'alarm 60; exec @ARGV' scheme --script " o ".sc > " o " 2> " o ".err < /dev/null; touch " o ".done ) &")
                      (await-file (string-append rel ".held") 20000)
                      (change)
                      (touch! rel)
                      (await-file (string-append o ".done") 30000)
                      (first-datum o))))
           (mirrored (lambda () (let ((st (fresh-store! 0))) (mirror-segments! st "zzzzmirr" 2) st)))
           (older (lambda (st) (let* ((dir (string-append st "/writers/zzzzmirr"))
                                      (fs (list-sort string<? (filter (lambda (f) (and (> (string-length f) 5) (char-numeric? (string-ref f 0))
                                                                                       (string=? ".sexp" (substring f (- (string-length f) 5) (string-length f)))))
                                                                      (directory-list dir)))))
                                 (and (pair? fs) (string-append dir "/" (car fs))))))
           (none (child (fresh-store! 0) (lambda () #f)))
           (healthy (child (mirrored) (lambda () #f)))
           (short-st (mirrored))
           (short (child short-st (lambda () (sh "perl -e 'truncate($ARGV[0], int((-s $ARGV[0]) / 2))' " (q (older short-st)))))))
      (want "LV-13 a delivery that falls short of its validated extent is not progress: healthy, two mirror segments add four bumps; the older cut short after the barrier, the fold fails and only the two validations count"
            (list (and (pair? healthy) (cadr healthy)) (and (pair? short) (cadr short))
                  (and (pair? healthy) (pair? none) (- (car healthy) (car none)))
                  (and (pair? short) (pair? none) (- (car short) (car none))))
            '(ok failed 4 2)))

    ;; ---- LV-14 an unrelated cached writer's death is not this request's -----------------
    (let* ((n (files-for 8000))
           (st (fresh-store! n))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=writer-raise-second@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-14" d))
      (let ((me self))
        (spawn
          (lambda ()
            (connect! (d-sock d))
            (receive
              (after 4000 (send me '(lv14 no-connect)))
              (`(connected ,p ,ref)
               (conn-read-start! ref)
               (conn-write! ref (v2-frame st 'drafts '("--writer" "w1")) 'one)
               (let loop ((acc '()) (phase 1))
                 (receive
                   (after 30000 (send me (list 'lv14 'timeout phase)))
                   (`(written ,r ,t ,s) (loop acc phase))
                   (`(data ,r ,bv)
                    (let ((acc (cons bv acc)))
                      (if (has-newline? bv)
                          (if (= phase 1)
                              (begin
                                (send me '(lv14 cached))
                                (conn-write! ref (v2-frame st 'export-code (list (string-append scratch "/lv14-export"))) 'two)
                                (loop '() 2))
                              (send me (list 'lv14 'answer (utf8->string (join (reverse acc))) (real-time))))
                          (loop acc phase))))
                   (`(eof ,r) (send me (list 'lv14 'eof phase)))))))))
        (receive (after 10000 #f) (`(lv14 cached) #t))
        (sleep-ms 300)
        ;; the cached writer's SECOND request kills it, from another connection
        (cli "" "drafts" "--writer" "w1" "--store" st "--socket" (d-sock d))
        ;; THE ORDER IS THE DAEMON'S OWN: this process is blocked while the
        ;; command line runs, so a time this side takes says nothing. In the
        ;; daemon's trace the export's connection records the cached writer's
        ;; DOWN, by its name, after the export's dispatch, and a keepalive
        ;; follows it before the export's executor is done. Only the pending
        ;; request's wait writes keepalives, so the DOWN reached that wait,
        ;; not write-answer's: executing-done is traced after the store loop
        ;; has sent the answer, so the interval alone does not say which.
        ;; The export's is the one pending version-2 request here.
        (let* ((a (receive (after 40000 '(lv14 nothing))
                    (`(lv14 ,tag . ,rest) (if (eq? tag 'answer) rest (cons tag rest)))))
               (r (and (pair? a) (string? (car a)) (car a)))
               ;; the export's executor finishes on its own clock, whenever the
               ;; answer came: wait for its executing-done before reading
               (log (let wait ((k 0))
                      (let* ((t (d-log d))
                             (ds (positions t "(trace daemon-dispatch export-code"))
                             (es (positions t "(trace executing-done ")))
                        (if (or (> k 400) (and (pair? ds) (exists (lambda (e) (> e (car ds))) es)))
                            t
                            (begin (sleep-ms 50) (wait (+ k 1)))))))
               (dispatched (positions log "(trace daemon-dispatch export-code"))
               (down (positions log "(trace writer-down-unrelated w1 "))
               (done (positions log "(trace executing-done "))
               (kept (positions log "(trace keepalive "))
               (window (and (pair? dispatched) (pair? down) (pair? done)
                            (< (car dispatched) (car down) (car (reverse done)))))
               (went-on (and window
                             (let ((after-down (filter (lambda (k) (> k (car down))) kept)))
                               (and (pair? after-down) (< (car after-down) (car (reverse done))))))))
          (want "LV-14 the cached writer w1's DOWN is traced by the export's connection inside the export's window (after its dispatch, before its executing-done)"
                window #t)
          (want "LV-14 and the export's wait went on after it: a keepalive follows the DOWN before the executing-done"
                went-on #t)
          (want "LV-14 a cached writer that dies during this connection's export is not its terminal: the export answers ok"
                (and (string? r) (contains? r "(ok (files")) #t)))
      (stop-daemon! d))

    ;; ---- LV-6b the retry at version 1 -------------------------------------------------
    (let* ((st (fresh-store! 0))
           (old (start-stub! "old"))
           (r (cli test-clocks "outline" "--store" st "--socket" old)))
      (want "LV-6b against a daemon that predates version 2: refused, resent once at version 1 on a new connection, answered as it answers"
            (list (car r) (count-of (cadr r) "(trace version-retry")
                  (count-of (file-text (string-append old ".frames")) "(request 2 ")
                  (count-of (file-text (string-append old ".frames")) "(request 1 "))
            '((ok (old)) 1 1 1)))
    (let* ((st (fresh-store! 0))
           (silent (start-stub! "silent"))
           (r (cli test-clocks "outline" "--store" st "--socket" silent)))
      (want "LV-6b NOT retried on silence: a silent peer is silent, once"
            (list (car (silent-parts (car r))) (count-of (cadr r) "(trace version-retry"))
            (list W 0)))
    (let* ((st (fresh-store! 0))
           (look (start-stub! "lookalike"))
           (r (cli test-clocks "outline" "--store" st "--socket" look)))
      (want "LV-6b NOT retried on an origin-core lookalike of the refusal"
            (list (car r) (count-of (cadr r) "(trace version-retry"))
            '((error bad-request (reason malformed-envelope)) 0)))
    (let* ((st (fresh-store! 0))
           (cut (start-stub! "cut"))
           (r (cli test-clocks "outline" "--store" st "--socket" cut)))
      (want "LV-6b NOT retried on the refusal cut short by the close: end of file before the terminator is lost-answer, once"
            (list (car r) (count-of (cadr r) "(trace version-retry"))
            '((error transport-unknown (reason lost-answer)) 0)))
    ;; A SOCKET KEEPS NO WRITE BOUNDARIES, so both shapes reach a client: the
    ;; refusal and its trailing byte in one read, and the byte read after the
    ;; line. Neither is the exact refusal; neither is retried.
    (let* ((st (fresh-store! 0))
           (one (start-stub! "after-one"))
           (r (cli test-clocks "outline" "--store" st "--socket" one)))
      (want "LV-6b NOT retried on the refusal with a byte after it in the same write: the line never ends, lost-answer, once"
            (list (car r) (count-of (cadr r) "(trace version-retry"))
            '((error transport-unknown (reason lost-answer)) 0)))
    ;; The byte comes trailing-byte-ms after the line. A red here under load
    ;; is that delay: the two writes reached the client as one read.
    (let* ((st (fresh-store! 0))
           (after (start-stub! "after"))
           (r (cli test-clocks "outline" "--store" st "--socket" after)))
      (want "LV-6b NOT retried on the refusal with a byte written after its line: answered as it is, once"
            (list (car r) (count-of (cadr r) "(trace version-retry"))
            '((error bad-request (reason malformed-envelope)) 0)))
    (let* ((st (fresh-store! 0))
           (d (start-daemon! st test-clocks)))
      (unless (daemon? d) (not-a-reading! "LV-6b new daemon" d))
      (let* ((good (utf8->string (v2-frame st 'outline '())))
             ;; a bad field: the mode a symbol no envelope has
             (bad-field (let ((d2 (read (open-string-input-port good))))
                          (render-wire (append (list-head d2 5) (list 'sideways) (list-tail d2 6)))))
             (trailing (string-append (substring good 0 (- (string-length good) 1)) " (x)\n"))
             (ask (lambda (text)
                    (program test-clocks
                             (format "(import (chezscheme) (theourgia client)) (let ((o (call! ~s (string->utf8 ~s)))) (write (if (eq? (car o) 'answer) (utf8->string (cadr o)) o)))"
                                     (d-sock d) text))))
             (a (ask bad-field))
             (b (ask trailing)))
        (want "LV-6b NOT retried on this daemon's refusal of a version-2 frame: a bad field and a trailing datum each answer malformed-envelope with (version 2), and no retry"
              (list (and (string? (car a)) (contains? (car a) "(version 2)")) (count-of (cadr a) "(trace version-retry")
                    (and (string? (car b)) (contains? (car b) "(version 2)")) (count-of (cadr b) "(trace version-retry"))
              '(#t 0 #t 0)))
      (stop-daemon! d))

    ;; ---- LV-7 the send side ---------------------------------------------------------------
    (let* ((st (fresh-store! 0))
           (sink (start-stub! "noread"))
           (big (make-string (* 1536 1024) #\a))
           (r (program test-clocks
                       (format "(import (chezscheme) (theourgia client)) (write (call! ~s (request-frame ~s 'write (list \"root\" ~s) (list (cons 'version 2)))))"
                               sink st big)))
           (z (program (string-append test-clocks " THEOURGIA_FAULT=send-stall@client")
                       (format "(import (chezscheme) (theourgia client)) (write (call! ~s (request-frame ~s 'outline '() (list (cons 'version 2)))))"
                               sink st))))
      (want "LV-7 a peer that never reads: the partly sent request is silent W after the last byte written (window 2000, elapsed at least W)"
            (let ((o (car r)))
              (list (and (pair? o) (car o)) (and (pair? o) (pair? (cdr o)) (cadr o))
                    (let ((w (and (pair? o) (assq 'window (cddr o))))) (and w (cadr w)))
                    (let ((e (and (pair? o) (assq 'elapsed (cddr o))))) (and e (>= (cadr e) W)))))
            (list 'transport-error 'silent W #t))
      (want "LV-7 a send that expires with nothing written is not-sent"
            (let ((o (car z))) (list (and (pair? o) (car o)) (and (pair? o) (pair? (cdr o)) (pair? (cadr o)) (cadr (cadr o)))))
            '(not-sent write-failed)))

    ;; ---- LV-7b a signal during the wait does not extend it ----------------------------
    ;; The client process takes SIGUSR1 every 500 ms, twelve times (6 s, three W),
    ;; while it waits on a peer that never answers and on one that never reads.
    ;; A blocking call that a signal interrupts returns EINTR (no SA_RESTART;
    ;; measured, landing2/r1). A retry that took the whole window again would end
    ;; W after the last signal, about 8 s; the client gives up at about W. The
    ;; handler's count says the signals arrived.
    (let* ((st (fresh-store! 0))
           (signo (let ((o (next-out)))
                    (sh "kill -l USR1 > " o " 2>/dev/null")
                    (first-datum o)))
           (signalled
            (lambda (sk frame-form)
              (program test-clocks
                       (format "(import (chezscheme) (theourgia client))
(define hits 0)
(register-signal-handler ~a (lambda (s) (set! hits (+ hits 1))))
(system (format \"( i=0; while [ $i -lt 12 ] && kill -USR1 ~~a 2>/dev/null; do sleep 0.5; i=$((i+1)); done ) > /dev/null 2>&1 &\" (get-process-id)))
(let ((o (call! ~s ~a))) (write (list o hits)))"
                               signo sk frame-form))))
           (read-row
            (lambda (r)
              (let* ((v (car r)) (o (and (pair? v) (car v))))
                (list (and (pair? o) (car o)) (and (pair? o) (pair? (cdr o)) (cadr o))
                      (let ((w (and (pair? o) (assq 'window (cddr o))))) (and w (cadr w)))
                      (let ((e (and (pair? o) (assq 'elapsed (cddr o))))) (and e (between? W (cadr e) (* 2 W))))
                      (and (pair? v) (pair? (cdr v)) (number? (cadr v)) (>= (cadr v) 2))
                      (< (caddr r) (* 3 W)))))))
      (unless (fixnum? signo)
        (not-a-reading! "LV-7b" (format "kill -l USR1 printed ~s, not a signal number" signo)))
      (let* ((quiet (start-stub! "silent"))
             (sink (start-stub! "noread"))
             (r (signalled quiet (format "(request-frame ~s 'outline '() (list (cons 'version 2)))" st)))
             (s (signalled sink (format "(request-frame ~s 'write (list \"root\" (make-string (* 1536 1024) #\\a)) (list (cons 'version 2)))" st))))
        (want "LV-7b a silent receive under a signal every 500 ms gives up at about W: silent, window W, elapsed in [W, 2W), the handler ran, wall under 3W"
              (read-row r)
              (list 'transport-error 'silent W #t #t #t))
        (want "LV-7b a blocked send under a signal every 500 ms gives up at about W: silent, window W, elapsed in [W, 2W), the handler ran, wall under 3W"
              (read-row s)
              (list 'transport-error 'silent W #t #t #t))))

    ;; ---- LV-9 silent at the surfaces ---------------------------------------------------
    (let* ((st (fresh-store! 0))
           (rel (string-append scratch "/hold-9.release"))
           (_ (touch! rel))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_HOLD_MS=30000 THEOURGIA_HOLD=executor-before-verb:" rel))))
      (unless (daemon? d) (not-a-reading! "LV-9" d))
      (let-values (((to from errs pid)
                    (open-process-ports (string-append "env THEOURGIA_INJECT=on " test-clocks " " lib-env
                                                       " THEOURGIA_RUN=" sockets "/run scheme --script ../mcp/server.sc --store " (q st)
                                                       " --socket " (q (d-sock d)) " 2>" scratch "/mcp.err")
                                        'line (native-transcoder))))
        (define (say! text) (put-string to text) (put-string to "\n") (flush-output-port to))
        (define (answer-to id)
          (let loop ()
            (let ((line (get-line from)))
              (cond ((eof-object? line) 'eof)
                    ((contains? line (string-append "\"id\":" (number->string id))) line)
                    (else (loop))))))
        (say! "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}")
        (answer-to 1)
        (say! "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
        (say! "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}")
        (answer-to 2)
        ;; the catalogue passed the hold; from here an executor holds
        (sh "rm -f " rel " " rel ".held")
        (say! "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_insert\",\"arguments\":{\"argv\":[\"--under\",\"root\",\"--title\",\"m\"]}}}")
        (let ((line (answer-to 3)))
          (close-port to)
          (want "LV-9 the MCP shell reports a silent daemon as -32603 with its data: reason silent, window 2000, elapsed"
                (list (and (string? line) (contains? line "\"code\":-32603"))
                      (and (string? line) (contains? line "\"reason\":\"silent\""))
                      (and (string? line) (contains? line "\"window\":2000"))
                      (and (string? line) (contains? line "\"elapsed\":")))
                '(#t #t #t #t))))
      (let ((r (cli test-clocks "insert" "--under" "root" "--title" "cli" "--store" st "--socket" (d-sock d))))
        (want "LV-9 the command line's wire answer for a silent daemon is (error transport-unknown (reason silent) (window 2000) (elapsed <ms>))"
              (car (silent-parts (car r))) W))
      (touch! rel)
      (stop-daemon! d))

    ;; THE CATALOGUE'S REQUEST GOES SILENT TOO: tools/list asks the daemon for
    ;; describe, and a silent one is answered with the same data as a tool's
    ;; own request (codex landing2a-r1a).
    (let* ((st (fresh-store! 0))
           (quiet (start-stub! "silent")))
      (let-values (((to from errs pid)
                    (open-process-ports (string-append "env THEOURGIA_INJECT=on " test-clocks " " lib-env
                                                       " THEOURGIA_RUN=" sockets "/run scheme --script ../mcp/server.sc --store " (q st)
                                                       " --socket " (q quiet) " 2>" scratch "/mcp-list.err")
                                        'line (native-transcoder))))
        (define (say! text) (put-string to text) (put-string to "\n") (flush-output-port to))
        (define (answer-to id)
          (let loop ()
            (let ((line (get-line from)))
              (cond ((eof-object? line) 'eof)
                    ((contains? line (string-append "\"id\":" (number->string id))) line)
                    (else (loop))))))
        (say! "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}")
        (answer-to 1)
        (say! "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
        (say! "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}")
        (let ((line (answer-to 2)))
          (close-port to)
          (want "LV-9 the MCP shell's tools/list against a silent daemon is -32603 with its data: reason silent, window 2000, elapsed"
                (list (and (string? line) (contains? line "\"code\":-32603"))
                      (and (string? line) (contains? line "\"reason\":\"silent\""))
                      (and (string? line) (contains? line "\"window\":2000"))
                      (and (string? line) (contains? line "\"elapsed\":")))
                '(#t #t #t #t)))))

    ;; ---- LV-6 version 1 is answered byte for byte as before ---------------------------------
    (let ((base (getenv "THEOURGIA_BASE_LIBDIR")))
      (if (not (and (string? base) (file-directory? (string-append base "/theourgia"))))
          (printf "SKIP LV-6 byte identity: THEOURGIA_BASE_LIBDIR names no library root holding the base tree's theourgia/ (set it as for change-stream-guard.sc to compare this daemon with the base's)~%")
          ;; NOTE: THE BASE TREE'S OWN CLIENTS, EACH A CHILD RUN FROM THE BASE'S
          ;; LIBRARY: its CLI, its MCP server (mcp/server.sc over stdio:
          ;; initialize, tools/list, a tools/call of outline) and its review
          ;; channel (review-results on a scope this tree made), each against
          ;; this daemon and then the base's. Equal bytes alone could be two
          ;; equal failures, so each output is also required to be the answer
          ;; it should be, on both sides. Core forwarding's comparison is
          ;; Landing 2b's.
          (let* ((st (fresh-store! 3))
                 (base-env (string-append "env CHEZSCHEMELIBDIRS=" base " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                                          " THEOURGIA_RUN=" (q (string-append scratch "/lv6-run"))))
                 (frames (list (request-frame st 'outline '() '((actor . "tester")))
                               (request-frame st 'describe '() '((actor . "tester")))
                               (request-frame st 'read '("no-such-block") '((actor . "tester")))
                               (request-frame st 'read '("--cut") '((actor . "tester")))))
                 (scoped (string-append scratch "/lv6-scope"))
                 (root-id (let ((o (next-out)))
                            (sh lib-env " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc outline --store " (q st) " --wire > " o " 2>/dev/null < /dev/null")
                            (let* ((t (file-text o)) (at (positions t "- ")))
                              (and (pair? at)
                                   (let loop ((i (+ (car at) 2)))
                                     (if (or (>= i (string-length t)) (char=? (string-ref t i) #\space))
                                         (substring t (+ (car at) 2) i)
                                         (loop (+ i 1))))))))
                 (_ (sh (cli-local "tag" "lv6" "--store" st)))
                 (_ (sh lib-env " THEOURGIA_LOCAL=1 scheme --script ../core.sc scope " (q scoped) " --cut lv6 --roots " (q (or root-id "none"))
                        " --for codex --store " (q st) " --wire > /dev/null 2>&1 < /dev/null"))
                 (mcp-in (let ((p (string-append scratch "/lv6-mcp.in")))
                           (call-with-output-file p
                             (lambda (o)
                               (for-each (lambda (l) (put-string o l) (newline o))
                                 (list (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                                                      "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                                                      "\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}")
                                       "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
                                       "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
                                       "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_outline\",\"arguments\":{\"argv\":[]}}}")))
                             'truncate)
                           p))
                 (answers (lambda (libs)
                            (let ((d (apply start-daemon! st "" (if libs (list libs) '())))
                                  (ds (apply start-daemon! scoped "" (if libs (list libs) '()))))
                              (unless (daemon? d) (not-a-reading! "LV-6" d))
                              (unless (daemon? ds) (not-a-reading! "LV-6 scope" ds))
                              (let ((got (map (lambda (f) (car (raw-collect (d-sock d) f 300 10000))) frames))
                                    (cli-out (let ((o (next-out)))
                                               (sh base-env " perl -e 'alarm 60; exec @ARGV' scheme --script " base "/theourgia/theourgia.sc outline --store " (q st)
                                                   " --socket " (q (d-sock d)) " --wire > " o " 2>/dev/null < /dev/null")
                                               (file-text o)))
                                    (mcp-out (let ((o (next-out)))
                                               ;; the shell names its session's writer from its own pid unless
                                               ;; told one; two runs would differ there and nowhere the daemon decides
                                               (sh base-env " THEOURGIA_WRITER=lv6-mcp perl -e 'alarm 60; exec @ARGV' scheme --script " base "/theourgia/mcp/server.sc --store " (q st)
                                                   " --socket " (q (d-sock d)) " > " o " 2>/dev/null < " (q mcp-in))
                                               (file-text o)))
                                    (channel-out (let ((o (next-out)))
                                                   (sh base-env " perl -e 'alarm 60; exec @ARGV' scheme --script " base "/theourgia/theourgia.sc review-results --store " (q scoped)
                                                       " --socket " (q (d-sock ds)) " --wire > " o " 2>/dev/null < /dev/null")
                                                   (file-text o))))
                                (stop-daemon! d)
                                (stop-daemon! ds)
                                (append (list cli-out mcp-out channel-out) got)))))
                 (new (answers #f))
                 (old (answers base))
                 ;; #t, or where the two first differ and 80 characters of each from there
                 (same-or-where (lambda (a b)
                                  (or (string=? a b)
                                      (let loop ((i 0))
                                        (if (and (< i (string-length a)) (< i (string-length b)) (char=? (string-ref a i) (string-ref b i)))
                                            (loop (+ i 1))
                                            (list 'differ-at i
                                                  (substring a i (min (string-length a) (+ i 80)))
                                                  (substring b i (min (string-length b) (+ i 80)))))))))
                 ;; NOTE: THE ANSWER IS READ AS MESSAGES, NOT AS SUBSTRINGS (codex
                 ;; landing2a-r1c-2): each line through the tree's own JSON reader;
                 ;; ids 1, 2 and 3 each a result and none an error; 2's tools name
                 ;; theourgia_outline; 3's result not isError true, its text naming
                 ;; f0.sc. Four substrings would also be found in an isError answer.
                 (four-strings? (lambda (t) (and (contains? t "\"id\":2") (contains? t "theourgia_outline") (contains? t "\"id\":3") (contains? t "f0.sc"))))
                 (mcp-ok? (lambda (t)
                            (let* ((msgs (let split ((cs (string->list t)) (line '()) (out '()))
                                           (define (flush) (if (null? line) out (cons (guard (e (#t #f)) (string->json (list->string (reverse line)))) out)))
                                           (cond ((null? cs) (reverse (flush)))
                                                 ((char=? (car cs) #\newline) (split (cdr cs) '() (flush)))
                                                 (else (split (cdr cs) (cons (car cs) line) out)))))
                                   (ref (lambda (o k) (and (pair? o) (let ((m (assoc k o))) (and m (cdr m))))))
                                   (by-id (lambda (n) (find (lambda (m) (let ((i (ref m "id"))) (and (number? i) (= i n)))) msgs)))
                                   (result-of (lambda (m) (and (pair? m) (not (assoc "error" m)) (assoc "result" m) (ref m "result"))))
                                   (r1 (result-of (by-id 1))) (r2 (result-of (by-id 2))) (r3 (result-of (by-id 3)))
                                   (tools (ref r2 "tools"))
                                   (content (ref r3 "content"))
                                   (text (and (vector? content) (> (vector-length content) 0) (ref (vector-ref content 0) "text"))))
                              (and r1 r2 r3 (vector? tools)
                                   (exists (lambda (k) (equal? (ref (vector-ref tools k) "name") "theourgia_outline")) (iota (vector-length tools)))
                                   (not (eq? #t (ref r3 "isError")))
                                   (string? text)
                                   ;; the outline's own answer: a core refusal is a successful
                                   ;; MCP result too (MC-03), and an unknown-id refusal can name
                                   ;; f0.sc as well
                                   (let* ((d (guard (e (#t #f)) (read (open-string-input-port text))))
                                          (tx (and (pair? d) (eq? (car d) 'ok) (list? d) (assq 'text (cdr d)))))
                                     (and tx (pair? (cdr tx)) (string? (cadr tx)) (contains? (cadr tx) "f0.sc")))
                                   #t))))
                 ;; the negatives: the same answer with id 3 replaced by an isError result
                 ;; naming f0.sc, and by a successful result carrying a core refusal that
                 ;; names it (the shape MC-03 says a refusal takes)
                 (forged-with (lambda (t line)
                           (let loop ((ls (let split ((cs (string->list t)) (cur '()) (out '()))
                                            (cond ((null? cs) (reverse (if (null? cur) out (cons (list->string (reverse cur)) out))))
                                                  ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse cur)) out)))
                                                  (else (split (cdr cs) (cons (car cs) cur) out)))))
                                      (acc ""))
                             (cond ((null? ls) acc)
                                   ((contains? (car ls) "\"id\":3")
                                    (loop (cdr ls) (string-append acc line "\n")))
                                   (else (loop (cdr ls) (string-append acc (car ls) "\n")))))))
                 (forged (lambda (t)
                           (forged-with t "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"failed to read f0.sc\"}],\"isError\":true}}")))
                 (refused (lambda (t)
                            (forged-with t "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"(error unknown-id \\\"f0.sc\\\")\"}],\"isError\":false}}")))
                 (channel-ok? (lambda (t) (and (> (string-length t) 0) (contains? t "(ok (results (letter ") (contains? t "(to \"codex\")")))))
            (want "LV-6 version-1 frames get byte-identical answers from this daemon and the base's (outline, describe, a missing block, a refusal), and the base tree's own CLI prints the same against both"
                  (map (lambda (a b) (equal? a b)) (cons (car new) (cdddr new)) (cons (car old) (cdddr old)))
                  '(#t #t #t #t #t))
            (want "LV-6 the base tree's MCP server (initialize, tools/list, a tools/call of outline) prints the same against this daemon and the base's, and each is that answer read as messages, id 3 the outline's own (ok (text ...)); the same with id 3 an isError result holding the four strings is not, nor with id 3 a successful result carrying an unknown-id refusal of f0.sc"
                  (list (same-or-where (cadr new) (cadr old)) (mcp-ok? (cadr new)) (mcp-ok? (cadr old))
                        (four-strings? (forged (cadr new))) (mcp-ok? (forged (cadr new))) (mcp-ok? (refused (cadr new))))
                  '(#t #t #t #t #f #f))
            (want "LV-6 the base tree's review channel: review-results on a scope prints the same against this daemon and the base's, and each is that answer"
                  (list (and root-id #t) (same-or-where (caddr new) (caddr old)) (channel-ok? (caddr new)) (channel-ok? (caddr old)))
                  '(#t #t #t #t)))))

    (stop-stubs!)
    (for-each (lambda (rn) (sh "pkill -f " rn " 2>/dev/null")) runners)
    (sh "rm -rf " scratch " " sockets)
    (printf "rows: ~a~%~a failures~%liveness complete~%" rows bad)
    (exit (if (= bad 0) 0 1))))
