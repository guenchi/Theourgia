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

;; A STREAM'S SILENCE BEFORE ITS ACCEPTANCE HAS THE CLIENT'S WINDOW, AND
;; NOTHING ELSE CHANGES (stream-client.sc stream-lines, daemon.sc
;; subscribe-stream).
;;
;; A version-2 subscribe gives up when nothing has arrived for W before its
;; acceptance: (error transport-unknown (reason silent) (window W) (elapsed
;; ms)). No daemon sends a keepalive on a subscription, before acceptance or
;; after; a space before the acceptance is stripped as a request client strips
;; one. After acceptance a stream may idle for ever, and a partial line still
;; has its 5 s.
;;
;; W IS SHORTENED (THEOURGIA_SILENCE_MS 2000, THEOURGIA_KEEPALIVE_MS 400, the
;; injection build only); the clients are the command line's `subscribe`,
;; each a child process. Daemons run with idle exit off.
;; THE PEERS THAT ARE NOT DAEMONS are perl stubs: one that never answers, one
;; that sends an acceptance and half a frame, one that sends the acceptance
;; and a frame in one write, one that sends spaces in pieces before the
;; acceptance.
;; LV-6's stream case runs the base tree's own stream client
;; (THEOURGIA_BASE_LIBDIR); without it that row prints SKIP and why.

(import (chezscheme) (theourgia sched)
        (only (theourgia render) render-wire))

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
                 "/lvs-" pid-text))
(define sockets
  (string-append (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/lvs-" pid-text))
(system (string-append "rm -rf " scratch " " sockets "; mkdir -p " scratch " " sockets))
(define lib-env (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                               " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"))
(define test-clocks "THEOURGIA_SILENCE_MS=2000 THEOURGIA_KEEPALIVE_MS=400")
(define W 2000)

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
(define (count-of text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) m) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i n) (+ k 1)))
            (else (loop (+ i 1) k))))))
(define (lines-of t)
  (let loop ((cs (string->list t)) (cur '()) (out '()))
    (cond ((null? cs) (reverse (if (null? cur) out (cons (list->string (reverse cur)) out))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) out)))
          (else (loop (cdr cs) (cons (car cs) cur) out)))))
(define (datum-of line) (guard (e (#t 'unreadable)) (read (open-string-input-port line))))
(define (between? lo x hi) (and (number? x) (<= lo x) (< x hi)))
(define (silent-parts a)
  (if (and (pair? a) (eq? 'error (car a)) (pair? (cdr a)) (eq? 'transport-unknown (cadr a))
           (equal? (caddr a) '(reason silent)))
      (let ((w (assq 'window (cdddr a))) (e (assq 'elapsed (cdddr a))))
        (list (and w (cadr w)) (and e (cadr e))))
      (list 'not-silent a)))
(define (await-file p ms)
  (let loop ((k 0)) (cond ((file-exists? p) #t) ((> (* k 50) ms) #f) (else (sleep-ms 50) (loop (+ k 1))))))

;; ---- stores and daemons -------------------------------------------------------------

(define (cli-local . words)
  (string-append lib-env " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc"
                 (apply string-append (map (lambda (w) (string-append " " (q w))) words))
                 " > /dev/null 2>&1 < /dev/null"))
(define store-n 0)
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
(define daemon-n 0)
(define runners '())
(define (start-daemon! st env)
  (set! daemon-n (+ daemon-n 1))
  (let* ((t (string-append pid-text "-" (number->string daemon-n)))
         (sk (string-append sockets "/d" (number->string daemon-n) ".sock"))
         (rn (string-append scratch "/d" t ".sc"))
         (lg (string-append scratch "/d" t ".log")))
    (sh "rm -f " sk)
    (set! runners (cons rn runners))
    (call-with-output-file rn
      (lambda (p)
        (for-each (lambda (l) (display l p) (newline p))
          (list "(import (chezscheme) (theourgia daemon))" (format "(serve ~s ~s)" st sk))))
      'truncate)
    (sh "env THEOURGIA_TRACE=1 THEOURGIA_INJECT=on THEOURGIA_PROBE=off " env " " lib-env
        " sh -c 'scheme --script " rn " > " lg " 2>&1' &")
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg))
            ((or (> k 600) (contains? (file-text lg) "Exception")) (list 'no-daemon (file-text lg)))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (daemon? d) (and (pair? d) (string? (car d))))
(define (d-sock d) (list-ref d 1))
(define (stop-daemon! d) (when (daemon? d) (sh "pkill -f " (list-ref d 2) " 2>/dev/null")))
(define (not-a-reading! what d)
  (if (string? d)
      (printf "NOT A READING: ~a: ~a~%" what d)
      (printf "NOT A READING: ~a: a daemon never created its socket~%~a~%" what (if (pair? d) (cadr d) d)))
  (for-each (lambda (rn) (sh "pkill -f " rn " 2>/dev/null")) runners)
  (stop-stubs!)
  (printf "liveness-streams complete~%")
  (exit 2))

;; ---- the stream client, a child --------------------------------------------------------

(define out-n 0)
(define (next-out) (set! out-n (+ out-n 1)) (string-append scratch "/o" (number->string out-n)))
;; THE STREAM IN THE BACKGROUND: -> its output path; its lines are read as
;; they arrive, its exit code when it ends (<out>.rc).
(define (subscribe-bg! env libs st sock)
  (let ((o (next-out)))
    (sh "( env THEOURGIA_INJECT=on THEOURGIA_TRACE=1 " env " CHEZSCHEMELIBDIRS=" libs
        " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
        " perl -e 'alarm 120; exec @ARGV' scheme --script "
        (if (string=? libs (getenv "CHEZSCHEMELIBDIRS")) "../theourgia.sc" (string-append libs "/theourgia/theourgia.sc"))
        " subscribe changes 0 --store " (q st) " --socket " (q sock) " --wire > " o " 2> " o ".err < /dev/null; echo $? > " o ".rc ) &")
    o))
(define (stream-ended o ms)
  (if (await-file (string-append o ".rc") ms)
      (list (lines-of (file-text o)) (file-text (string-append o ".err")) (string->number (car (lines-of (file-text (string-append o ".rc"))))))
      (list (lines-of (file-text o)) (file-text (string-append o ".err")) 'running)))
(define (kill-stream! o) (sh "pkill -f " (q (string-append o ".err")) " 2>/dev/null; pkill -f 'subscribe changes 0 --store' 2>/dev/null"))

;; ---- perl stubs ---------------------------------------------------------------------------

(define stub-script (string-append scratch "/stub.pl"))
(call-with-output-file stub-script
  (lambda (p)
    (put-string p "use strict; use IO::Socket::UNIX; use Socket;
my ($path, $mode, $dir) = @ARGV;
sub slurp { my $f = shift; open(my $h, '<', $f) or return ''; local $/; my $t = <$h>; close $h; return $t }
sub pause { select(undef, undef, undef, $_[0]) }
unlink $path;
$SIG{PIPE} = 'IGNORE';
my $srv = IO::Socket::UNIX->new(Type => SOCK_STREAM, Local => $path, Listen => 8) or die \"listen: $!\";
open(my $pf, '>', \"$path.pid\"); print $pf $$; close $pf;
while (my $c = $srv->accept) {
  $c->autoflush(1);
  if ($mode eq 'noread') { sleep 120; next }
  my $line = <$c>;
  if ($mode eq 'silent') { sleep 120; next }
  if ($mode eq 'old') {
    open(my $lg, '>>', \"$path.frames\"); print $lg $line; close $lg;
    if (defined $line && $line =~ /^\\(request 2 /) { print $c slurp(\"$dir/refusal\"); close $c; next }
    print $c slurp(\"$dir/accept\") . slurp(\"$dir/frame\"); pause(0.5); close $c; next }
  if ($mode eq 'partial') { print $c slurp(\"$dir/accept\"); print $c '(changes (rev 2'; sleep 120; next }
  if ($mode eq 'coalesce') { print $c slurp(\"$dir/accept\") . slurp(\"$dir/frame\"); pause(0.5); close $c; next }
  if ($mode eq 'old-space') {
    open(my $lg, '>>', \"$path.frames\"); print $lg $line; close $lg;
    print $c slurp(\"$dir/refusal\"); pause(0.3); print $c ' '; pause(0.3); close $c; next }
  if ($mode eq 'slow-accept') {
    my $a = slurp(\"$dir/accept\"); my $n = length($a); my $k = int($n / 7) + 1;
    for (my $i = 0; $i < $n; $i += $k) { print $c substr($a, $i, $k); pause(1) }
    print $c slurp(\"$dir/frame\"); pause(0.5); close $c; next }
  if ($mode eq 'half-accept') { my $a = slurp(\"$dir/accept\"); print $c substr($a, 0, int(length($a) / 2)); sleep 120; next }
  if ($mode eq 'prefix') { print $c '  '; pause(0.3); print $c ' '; pause(0.3); print $c slurp(\"$dir/accept\"); pause(0.5); close $c; next }
}
"))
  'truncate)
(define replies (string-append scratch "/replies"))
(sh "mkdir -p " replies)
(define (reply! name text) (call-with-output-file (string-append replies "/" name) (lambda (p) (put-string p text)) 'truncate))
(reply! "accept" (render-wire (list 'answer (list 'stdout (render-wire '(ok (subscribed 0) (daemon "stub-1") (current 1))))
                                    '(stderr "") '(exit 0) '(origin core))))
(reply! "frame" (render-wire '(changes (rev 2) (daemon "stub-1") (from-cut ()) (cut ()) (items))))
;; TODAY'S REFUSAL OF A VERSION IT DOES NOT KNOW, as an old daemon renders it.
(reply! "refusal" (render-wire (list 'answer (list 'stdout (render-wire '(error bad-request (reason malformed-envelope))))
                                     '(stderr "") '(exit 1) '(origin transport))))
(define stubs '())
;; EACH START ITS OWN SOCKET, so its own pid file: two starts of one mode
;; under one name would leave stop-stubs! the second's pid twice.
(define stub-n 0)
(define (start-stub! mode)
  (set! stub-n (+ stub-n 1))
  (let ((sk (string-append sockets "/stub-" mode "-" (number->string stub-n) ".sock")))
    (sh "rm -f " sk " " sk ".pid; perl " stub-script " " sk " " mode " " replies " > /dev/null 2>&1 &")
    (set! stubs (cons sk stubs))
    (let up ((k 0)) (cond ((file-exists? sk) sk) ((> k 100) #f) (else (sleep-ms 50) (up (+ k 1)))))))
(define (stop-stubs!)
  (for-each (lambda (sk) (let ((t (file-text (string-append sk ".pid"))))
                           (when (> (string-length t) 0) (sh "kill " t " 2>/dev/null"))))
            stubs))
(define libs (getenv "CHEZSCHEMELIBDIRS"))

(start-scheduler
  (lambda ()

    ;; ---- LV-10 a silent peer before acceptance ---------------------------------------
    (let* ((st (fresh-store! 0))
           (silent (start-stub! "silent"))
           (t0 (real-time))
           (o (subscribe-bg! test-clocks libs st silent))
           (r (stream-ended o 10000))
           (wall (- (real-time) t0))
           (parts (silent-parts (and (pair? (car r)) (datum-of (car (car r)))))))
      (want "LV-10 pre-acceptance silence of W: the subscribe answers silent (window 2000, elapsed in [W, W+1000)), exit 75 as every transport outcome the command line refuses"
            (list (car parts) (between? W (cadr parts) (+ W 1000)) (caddr r) (< wall 8000))
            (list W #t 75 #t))
      (kill-stream! o))

    ;; ---- LV-10 a subscribe queued behind a request that outlasts W ----------------------
    (let* ((st (fresh-store! 40))
           (d (start-daemon! st (string-append test-clocks " THEOURGIA_FAULT=export-slow@conn"))))
      (unless (daemon? d) (not-a-reading! "LV-10 queued" d))
      (let* ((ex (next-out))
             (_ (sh "( env THEOURGIA_INJECT=on " test-clocks " " lib-env " scheme --script ../theourgia.sc export-code "
                    (q (string-append scratch "/lv10-export")) " --store " (q st) " --socket " (q (d-sock d))
                    " --wire > " ex " 2>&1 < /dev/null ) &"))
             (_ (sleep-ms 500))
             (o (subscribe-bg! test-clocks libs st (d-sock d)))
             (r (stream-ended o 10000))
             (parts (silent-parts (and (pair? (car r)) (datum-of (car (car r)))))))
        (want "LV-10 a version-2 subscribe queued behind an export that outlasts W goes silent at W, and no keepalive byte came before it; exit 75"
              (list (car parts) (count-of (cadr r) "(trace keepalive-stripped") (caddr r))
              (list W 0 75))
        (kill-stream! o))
      (stop-daemon! d))

    ;; ---- LV-10 an accepted stream may idle past W ---------------------------------------
    ;; NOTE: THE IDLE INTERVAL IS COUNTED FROM THE ACCEPTANCE, NOT FROM THE
    ;; LAUNCH. The child starts from source, and its start-up and the
    ;; acceptance take their share of any interval counted from launch: a
    ;; client that kept W after acceptance could live through what was left
    ;; (codex landing2a-r1c). The row waits for the acceptance line, at most
    ;; 10 s, prints how long it took, and only then idles.
    (let* ((st (fresh-store! 0))
           (d (start-daemon! st test-clocks)))
      (unless (daemon? d) (not-a-reading! "LV-10 idle" d))
      (let* ((o (subscribe-bg! test-clocks libs st (d-sock d)))
             (t0 (real-time))
             (accepted?
               (lambda ()
                 (let ((ls (lines-of (file-text o))))
                   (and (pair? ls)
                        (let ((a (datum-of (car ls))))
                          (and (pair? a) (eq? 'ok (car a)) (assq 'subscribed (cdr a)) #t))))))
             (waited (let wait ()
                       (cond ((accepted?) (- (real-time) t0))
                             ((> (- (real-time) t0) 10000) #f)
                             (else (sleep-ms 50) (wait)))))
             (_ (unless waited
                  (kill-stream! o)
                  (not-a-reading! "LV-10 idle: no acceptance within 10 s of the launch" (file-text o))))
             (_ (printf "LV-10 idle: the acceptance came ~a ms after the launch~%" waited))
             (_ (sleep-ms (+ (* 2 W) 1000)))
             (_ (sh "env THEOURGIA_INJECT=on " test-clocks " " lib-env " scheme --script ../theourgia.sc insert --under root --title late"
                    " --store " (q st) " --socket " (q (d-sock d)) " --wire > /dev/null 2>&1 < /dev/null"))
             (_ (sleep-ms 1500))
             (r (stream-ended o 0)))
        (want "LV-10 an accepted stream idle for 2 W + 1 s after its acceptance is still open, and a commit after that arrives as a frame"
              (list (caddr r) (length (car r)) (and (>= (length (car r)) 2) (contains? (cadr (car r)) "(changes")))
              '(running 2 #t))
        (kill-stream! o))
      (stop-daemon! d))

    ;; ---- LV-10 the partial-frame rule is unchanged ----------------------------------------
    (let* ((st (fresh-store! 0))
           (partial (start-stub! "partial"))
           (t0 (real-time))
           (o (subscribe-bg! test-clocks libs st partial))
           (r (stream-ended o 12000))
           (wall (- (real-time) t0)))
      (want "LV-10 after acceptance, half a frame with nothing more ends lost-stream after about 5 s, not after W"
            (list (and (pair? (car r)) (datum-of (car (car r))))
                  (and (>= (length (car r)) 2) (datum-of (cadr (car r)))) (caddr r) (between? 5000 wall 9000))
            (list '(ok (subscribed 0) (daemon "stub-1") (current 1)) '(error transport-error lost-stream) 1 #t))
      (kill-stream! o))

    ;; ---- LV-10 the acceptance and the first frame in one read ---------------------------------
    (let* ((st (fresh-store! 0))
           (both (start-stub! "coalesce"))
           (o (subscribe-bg! test-clocks libs st both))
           (r (stream-ended o 8000)))
      (want "LV-10 the acceptance and a frame arriving in one read are both delivered, in order"
            (list (and (pair? (car r)) (datum-of (car (car r))))
                  (and (>= (length (car r)) 2) (datum-of (cadr (car r)))))
            (list '(ok (subscribed 0) (daemon "stub-1") (current 1))
                  '(changes (rev 2) (daemon "stub-1") (from-cut ()) (cut ()) (items))))
      (kill-stream! o))

    ;; ---- LV-10 a space prefix in pieces before the acceptance -----------------------------------
    (let* ((st (fresh-store! 0))
           (prefix (start-stub! "prefix"))
           (o (subscribe-bg! test-clocks libs st prefix))
           (r (stream-ended o 8000)))
      (want "LV-10 spaces before the acceptance, split across reads, are stripped: the acceptance is read whole, and the trace counts them"
            (list (and (pair? (car r)) (datum-of (car (car r)))) (>= (count-of (cadr r) "(trace keepalive-stripped") 2))
            (list '(ok (subscribed 0) (daemon "stub-1") (current 1)) #t))
      (kill-stream! o))

;; ---- LV-6b a version-2 subscribe against a daemon that predates it ---------------------------
    (let* ((st (fresh-store! 0))
           (old (start-stub! "old"))
           (o (subscribe-bg! test-clocks libs st old))
           (r (stream-ended o 8000)))
      (want "LV-6b a version-2 subscribe refused by an old daemon is sent once more at version 1 and streams as today: acceptance, then the frame"
            (list (and (pair? (car r)) (datum-of (car (car r))))
                  (and (>= (length (car r)) 2) (datum-of (cadr (car r))))
                  (count-of (cadr r) "(trace version-retry")
                  (count-of (file-text (string-append old ".frames")) "(request 2 ")
                  (count-of (file-text (string-append old ".frames")) "(request 1 "))
            (list '(ok (subscribed 0) (daemon "stub-1") (current 1))
                  '(changes (rev 2) (daemon "stub-1") (from-cut ()) (cut ()) (items))
                  1 1 1))
      (kill-stream! o))
    (let* ((st (fresh-store! 0))
           (d (start-daemon! st test-clocks)))
      (unless (daemon? d) (not-a-reading! "LV-6b stream, new daemon" d))
      (let* ((o (next-out))
             (frame (string-append "(request 2 " (format "~s" st) " \"tester\" #f wire #f #f subscribe \"changes\" \"0\") (x)\n"))
             (_ (call-with-output-file (string-append o ".sc")
                  (lambda (p)
                    (put-string p (format "(import (chezscheme) (theourgia stream-client)) (let ((o (stream! ~s (string->utf8 ~s) (lambda (l) (write (utf8->string l)) (newline))))) (write o))"
                                          (d-sock d) frame)))
                  'truncate))
             (_ (sh "env THEOURGIA_INJECT=on THEOURGIA_TRACE=1 " test-clocks " " lib-env
                    " perl -e 'alarm 60; exec @ARGV' scheme --script " o ".sc > " o " 2> " o ".err < /dev/null"))
             (lines (lines-of (file-text o))))
        (want "LV-6b NOT retried: this daemon's refusal of a version-2 subscribe with a trailing datum names (version 2) and is handed on as it came, once"
              (list (and (pair? lines) (contains? (car lines) "(version 2)"))
                    (count-of (file-text (string-append o ".err")) "(trace version-retry"))
              '(#t 0)))
      (stop-daemon! d))

;; ---- the stream's window holds until the acceptance line is in (codex landing2a-r1a) -------------
    ;; A REFUSAL FOLLOWED BY ANY BYTE IS NOT THE EXACT REFUSAL, a space
    ;; included: it is handed on, on the one connection, and not retried.
    (let* ((st (fresh-store! 0))
           (old (start-stub! "old-space"))
           (o (subscribe-bg! test-clocks libs st old))
           (r (stream-ended o 8000)))
      (want "LV-6b NOT retried: an old daemon's refusal of a version-2 subscribe followed by a space, then the end: handed on (exit 1), one connection, no retry"
            (list (count-of (cadr r) "(trace version-retry")
                  (count-of (file-text (string-append old ".frames")) "(request 2 ")
                  (count-of (file-text (string-append old ".frames")) "(request 1 ")
                  (caddr r))
            '(0 1 0 1))
      (kill-stream! o))
    ;; AN ACCEPTANCE THAT IS STILL ARRIVING keeps the stream waiting: its
    ;; pieces come a second apart, for longer than the 5 s partial-frame
    ;; budget, each well inside W; that budget starts after the acceptance.
    (let* ((st (fresh-store! 0))
           (slow (start-stub! "slow-accept"))
           (t0 (real-time))
           (o (subscribe-bg! test-clocks libs st slow))
           (r (stream-ended o 15000))
           (wall (- (real-time) t0)))
      (want "LV-10 an acceptance arriving in seven pieces a second apart (over 5 s, each gap under W) is read whole, and the frame after it"
            (list (and (pair? (car r)) (datum-of (car (car r))))
                  (and (>= (length (car r)) 2) (datum-of (cadr (car r))))
                  (> wall 5000))
            (list '(ok (subscribed 0) (daemon "stub-1") (current 1))
                  '(changes (rev 2) (daemon "stub-1") (from-cut ()) (cut ()) (items))
                  #t))
      (kill-stream! o))
    ;; HALF AN ACCEPTANCE AND THEN NOTHING is the silence the window bounds.
    (let* ((st (fresh-store! 0))
           (half (start-stub! "half-accept"))
           (o (subscribe-bg! test-clocks libs st half))
           (r (stream-ended o 10000))
           (parts (silent-parts (and (pair? (car r)) (datum-of (car (car r)))))))
      (want "LV-10 half an acceptance, then silence: silent at W (window 2000, elapsed in [W, W+1000)), exit 75, not lost-stream after 5 s"
            (list (car parts) (between? W (cadr parts) (+ W 1000)) (caddr r))
            (list W #t 75))
      (kill-stream! o))
    ;; THE STREAM UNDER A SIGNAL, as LV-7b for a request: SIGUSR1 every
    ;; 500 ms, twelve times, while the subscribe waits on half an acceptance
    ;; and while its send is blocked on a peer that never reads; and a send
    ;; that writes nothing at all is not-sent.
    (let* ((st (fresh-store! 0))
           (half (start-stub! "half-accept"))
           (sink (start-stub! "noread"))
           (signo (let ((o (next-out))) (sh "kill -l USR1 > " o " 2>/dev/null") (datum-of (file-text o))))
           (subscribe (lambda (big) (string-append "(request 2 " (format "~s" st) " \"tester\" #f wire #f #f subscribe \"changes\" \"0\""
                                                   (if big (string-append " \"" (make-string (* 1536 1024) #\a) "\"") "") ")\n")))
           (run (lambda (env sk frame signals?)
                  (let ((o (next-out)) (t0 (real-time)))
                    (call-with-output-file (string-append o ".sc")
                      (lambda (p)
                        (put-string p (string-append
                          "(import (chezscheme) (theourgia stream-client))\n(define hits 0)\n"
                          (if signals?
                              (format "(register-signal-handler ~a (lambda (s) (set! hits (+ hits 1))))\n(system (format \"( i=0; while [ $i -lt 12 ] && kill -USR1 ~~a 2>/dev/null; do sleep 0.5; i=$((i+1)); done ) > /dev/null 2>&1 &\" (get-process-id)))\n" signo)
                              "")
                          (format "(let ((o (stream! ~s (call-with-port (open-file-input-port ~s) get-bytevector-all) (lambda (l) #f)))) (write (list o hits)))\n" sk (string-append o ".frame")))))
                      'truncate)
                    (call-with-port (open-file-output-port (string-append o ".frame") (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 frame))))
                    (sh "env THEOURGIA_INJECT=on THEOURGIA_TRACE=1 " env " " lib-env
                        " perl -e 'alarm 60; exec @ARGV' scheme --script " o ".sc > " o " 2> " o ".err < /dev/null")
                    (list (datum-of (file-text o)) (- (real-time) t0)))))
           (silent-reading (lambda (r)
                             (let* ((v (car r)) (out (and (pair? v) (car v))))
                               (list (and (pair? out) (car out)) (and (pair? out) (pair? (cdr out)) (cadr out))
                                     (let ((w (and (pair? out) (assq 'window (cddr out))))) (and w (cadr w)))
                                     (let ((e (and (pair? out) (assq 'elapsed (cddr out))))) (and e (between? W (cadr e) (* 2 W))))
                                     (and (pair? v) (pair? (cdr v)) (number? (cadr v)) (>= (cadr v) 2))
                                     (< (cadr r) (* 3 W)))))))
      (unless (fixnum? signo)
        (not-a-reading! "LV-7b stream" (format "kill -l USR1 printed ~s, not a signal number" signo)))
      (want "LV-7b a stream's receive of half an acceptance under SIGUSR1 every 500 ms gives up at about W: silent, window W, elapsed in [W, 2W), the handler ran, wall under 3W"
            (silent-reading (run test-clocks half (subscribe #f) #t))
            (list 'transport-error 'silent W #t #t #t))
      (want "LV-7b a stream's blocked send under SIGUSR1 every 500 ms gives up at about W: silent, window W, elapsed in [W, 2W), the handler ran, wall under 3W"
            (silent-reading (run test-clocks sink (subscribe #t) #t))
            (list 'transport-error 'silent W #t #t #t))
      (want "LV-7 a stream's subscribe that expires with nothing written is not-sent"
            (let* ((v (car (run (string-append test-clocks " THEOURGIA_FAULT=send-stall@client") sink (subscribe #f) #f)))
                   (out (and (pair? v) (car v))))
              (list (and (pair? out) (car out)) (and (pair? out) (pair? (cdr out)) (pair? (cadr out)) (cadr (cadr out)))))
            '(not-sent write-failed)))

    ;; ---- LV-6 the base tree's stream client against this daemon -----------------------------------
    (let ((base (getenv "THEOURGIA_BASE_LIBDIR")))
      (if (not (and (string? base) (file-directory? (string-append base "/theourgia"))))
          (printf "SKIP LV-6 stream case: THEOURGIA_BASE_LIBDIR names no library root holding the base tree's theourgia/ (set it as for change-stream-guard.sc)~%")
          (let* ((st (fresh-store! 70))
                 (d (start-daemon! st "THEOURGIA_FAULT=export-slow@conn")))
            (unless (daemon? d) (not-a-reading! "LV-6 stream" d))
            (sh "( env THEOURGIA_INJECT=on " lib-env " scheme --script ../theourgia.sc export-code "
                (q (string-append scratch "/lv6-export")) " --store " (q st) " --socket " (q (d-sock d))
                " --wire > /dev/null 2>&1 < /dev/null ) &")
            (sleep-ms 500)
            (let* ((t0 (real-time))
                   (o (subscribe-bg! "" base st (d-sock d)))
                   (accepted (let loop ((k 0))
                               (cond ((> (length (lines-of (file-text o))) 0) (- (real-time) t0))
                                     ((> k 400) #f)
                                     (else (sleep-ms 50) (loop (+ k 1)))))))
              (want "LV-6 the base tree's stream client, a version-1 subscribe whose acceptance comes after more than 5 s, is accepted by this daemon"
                    (list (and accepted (> accepted 5000))
                          (let ((ls (lines-of (file-text o)))) (and (pair? ls) (contains? (car ls) "(ok (subscribed"))))
                    '(#t #t))
              (kill-stream! o))
            (stop-daemon! d))))

    (stop-stubs!)
    ;; NEVER: NO STUB OUTLIVES THE RUN. Each start has its own socket and pid
    ;; file, and stop-stubs! kills every one; this row reads the process table
    ;; for a perl running this run's stub script, up to two seconds after.
    ;; (Two starts of one mode used to share one socket and one pid file, and
    ;; the first stub was left blocked in accept; a gate's LEFT IN GROUP line
    ;; showed it.)
    ;; NOTE: THE TABLE IS COLLECTED, NOT PIPED (codex landing2a-r1c-2): ps's
    ;; output to a file and its exit status to another; a failed or empty
    ;; collection is NOT A READING, never a count of none. The command lines
    ;; are compared with a literal prefix, "perl <this run's stub script> ",
    ;; so no character of the path is a pattern. Three controls run the same
    ;; two procedures: a table line under a path holding "[1]" counts once; a
    ;; collector that prints a line and then fails reads #f, so the status is
    ;; read on its own (a command that exits the collecting shell would leave
    ;; no status and no table at once, and test neither: codex
    ;; landing2a-r1c-3); and a collector that succeeds with an empty table
    ;; reads #f too.
    (let* ((collect-with
             (lambda (command)
               (let ((tbl (string-append scratch "/ps-table")) (rcf (string-append scratch "/ps-rc")))
                 (sh "rm -f " (q tbl) " " (q rcf) "; " command " > " (q tbl) " 2>/dev/null; echo $? > " (q rcf))
                 (let ((rc (string->number (car (append (lines-of (file-text rcf)) '("x")))))
                       (ls (lines-of (file-text tbl))))
                   (and (eqv? rc 0) (pair? ls) ls)))))
           (count-prefixed
             (lambda (ls prefix)
               (length (filter (lambda (l) (and (>= (string-length l) (string-length prefix))
                                                (string=? (substring l 0 (string-length prefix)) prefix)))
                               ls))))
           (prefix (string-append "perl " stub-script " "))
           (alive (lambda ()
                    (let ((ls (collect-with "ps -A -o command=")))
                      (unless ls (not-a-reading! "LV-10 the stub census" "ps -A -o command= failed or listed nothing"))
                      (count-prefixed ls prefix)))))
      (let wait ((k 0))
        (unless (or (eqv? (alive) 0) (> k 20)) (sleep-ms 100) (wait (+ k 1))))
      (want "LV-10 no stub of this run is alive after stop-stubs!: every start has its own pid file; read from a collected table by a literal prefix, a bracketed path counted, a collector that prints a line and fails not read, nor an empty table"
            (list (alive)
                  (count-prefixed (list "perl /nowhere/review[1]/lvs-9/stub.pl /nowhere/lvs-9/stub-half-accept-2.sock half-accept /nowhere/review[1]/lvs-9/replies"
                                        "perl /nowhere/review1/lvs-9/stub.pl /nowhere/lvs-9/stub-silent-1.sock silent /nowhere/review1/lvs-9/replies"
                                        "sh -c ps -A -o command=")
                                  "perl /nowhere/review[1]/lvs-9/stub.pl ")
                  (collect-with "( echo 'perl /nowhere/x/stub.pl /nowhere/x/stub-silent-1.sock silent /nowhere/x/replies'; exit 3 )")
                  (collect-with "true"))
            '(0 1 #f #f)))
    (for-each (lambda (rn) (sh "pkill -f " rn " 2>/dev/null")) runners)
    (sh "rm -rf " scratch " " sockets)
    (printf "rows: ~a~%~a failures~%liveness-streams complete~%" rows bad)
    (exit (if (= bad 0) 0 1))))
