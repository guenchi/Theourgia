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

;; A DAEMON THAT NEVER HAD A SUBSCRIBER ANSWERS AS IT DID BEFORE THE STREAM.
;;
;; The guard is differential. One store is prepared, copied twice, and each
;; copy is served by a daemon (A COPY IS NOT THE STORE: its instance identity
;; names the original's directory, and a daemon refuses it. Each copy's
;; instance.sexp gets its own device and inode, and each daemon its own copy
;; of the prepared machine home -- the same machine, its own registry -- so
;; the two run as two equal stores without either learning of the other): one built from the tree before the change
;; stream (THEOURGIA_BASE_LIBDIR, a library root holding that theourgia and
;; igropyr), one from this tree. The same script of requests is asked of
;; both and the answers compared byte for byte, a timestamp (a run of twelve
;; or more digits) read as one token. Then the trace of one read and one
;; commit is compared the same way, and the time of commits is measured by
;; forty alternating rounds (the bound is stated where it is judged).
;;
;; THE ANSWERS A CHANGE MEANS TO ALTER ARE DATA, in
;; change-stream-guard-expect.sexp: each scripted answer this tree's change
;; alters against its base, with why. Empty when the tree changes no answer.
;; Every observed difference must be listed there, and every listed one
;; observed, so an item that changes an answer says so in the same commit,
;; and an entry that no longer differs (its change is in the base now) is
;; red until it is removed.
;;
;; EACH DAEMON IS THE ONE THAT SHIPS: its tree's own theourgiad.sc serve,
;; which registers the verbs from outside the core table (extensions.sc)
;; before it serves. A daemon started from (theourgia daemon) alone serves
;; without them -- it answered describe with 49 verbs where the program
;; answers 56, and context with unknown-verb -- so a guard built that way
;; could not see a change to an extension verb.
;;
;; THE TWO DAEMONS' OWN PATHS differ by construction (two copies, two
;; sockets); each answer and trace has its daemon's store and socket read
;; as one token before comparing.
;;
;; IT NEEDS A BASE TREE, AND THAT IS AN OPT-IN. With THEOURGIA_BASE_LIBDIR
;; unset the guard says why it did not run and is green, as a named opt-in
;; (the gate sets it to its own fresh base); set to a directory that is not
;; a base library root, it is NOT A READING, and says so.

(import (chezscheme) (theourgia sched) (theourgia net))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define base-lib (getenv "THEOURGIA_BASE_LIBDIR"))
(unless (and (string? base-lib) (> (string-length base-lib) 0))
  (printf "SKIP: THEOURGIA_BASE_LIBDIR is unset: set it to a library root holding the base tree's theourgia/ and igropyr/ to compare this tree's daemon with the base's (an opt-in, not a failure)~%")
  (printf "rows: 0~%0 failures~%change-stream-guard complete~%")
  (exit 0))
(unless (and (file-exists? (string-append base-lib "/theourgia/daemon.sc"))
             (file-exists? (string-append base-lib "/theourgia/theourgiad.sc")))
  (printf "NOT A READING: THEOURGIA_BASE_LIBDIR must name a library root holding the base tree's theourgia/ (with daemon.sc and theourgiad.sc) and igropyr/ (it is ~s)~%" base-lib)
  (printf "change-stream-guard complete~%")
  (exit 2))
(define this-lib (getenv "CHEZSCHEMELIBDIRS"))

(define pid-text (number->string (get-process-id)))
(define scratch (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/csg-" pid-text))
(define sockets (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/csg-" pid-text))
(system (string-append "rm -rf " scratch " " sockets "; mkdir -p " scratch " " sockets))
(define prepared-home (string-append scratch "/home"))
(putenv "THEOURGIA_HOME" prepared-home)

(define (file-text p) (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (replace-all text from to)
  (let ((n (string-length from)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i n) (string-length text)) (apply string-append (reverse (cons (substring text i (string-length text)) out))))
            ((string=? (substring text i (+ i n)) from) (loop (+ i n) (cons to out)))
            (else (loop (+ i 1) (cons (string (string-ref text i)) out)))))))
(define (contains? text needle) (not (equal? (replace-all text needle "") text)))
;; D's store and socket become STORE and SOCKET, a writer session's process
;; id (the digits after "w-", which each daemon's own pid makes) becomes "#",
;; then every run of twelve or more digits becomes "#". An atomic write's
;; temporary is masked in traces only (mask-temporaries), never in answers.
(define (digits-end text j)
  (let run ((j j)) (if (and (< j (string-length text)) (char-numeric? (string-ref text j))) (run (+ j 1)) j)))
(define (session-pids text)
  (let loop ((i 0) (out '()))
    (define (at? prefix)
      (let ((n (string-length prefix)))
        (and (<= (+ i n 1) (string-length text)) (string=? (substring text i (+ i n)) prefix)
             (char-numeric? (string-ref text (+ i n))))))
    (cond ((>= i (string-length text)) (list->string (reverse out)))
          ((at? "w-")
           (loop (digits-end text (+ i 2)) (append (list #\# #\- #\w) out)))
          (else (loop (+ i 1) (cons (string-ref text i) out))))))
;; A TRACE's atomic-write temporaries, <path>.tmp-<pid>-<n> (log.sc's
;; names), have the daemon's pid in them, and two daemons cannot share it:
;; that pid becomes "#". Only in a path the normalizer has already made the
;; store's or its home's (a token starting STORE), only that form (the
;; digits between ".tmp-" and "-<digits>", the counter ending the token),
;; and only in a trace: answers are compared unmasked, so the same text in
;; a payload still differs (GUARD-0c). The token is found textually, back
;; to the last space, paren, quote or newline before ".tmp-", and must
;; start STORE there: it is not a tokenizer. The store's own temporary
;; paths in these traces hold no such character, so each is recognized
;; whole; other text is judged only by that rule (a "(STORE/" inside a
;; longer string is recognized at its paren, a path holding a delimiter
;; before ".tmp-" is not).
(define (mask-temporaries text)
  (define (in-store-path? i)
    (let back ((k i))
      (cond ((< k 0) #f)
            ((memv (string-ref text k) '(#\space #\( #\" #\newline))
             (and (<= (+ k 6) (string-length text)) (string=? (substring text (+ k 1) (+ k 6)) "STORE")))
            (else (back (- k 1))))))
  (let loop ((i 0) (out '()))
    (cond ((>= i (string-length text)) (list->string (reverse out)))
          ((and (<= (+ i 6) (string-length text)) (string=? (substring text i (+ i 5)) ".tmp-")
                (char-numeric? (string-ref text (+ i 5)))
                (let ((j (digits-end text (+ i 5))))
                  (and (< (+ j 1) (string-length text)) (char=? (string-ref text j) #\-)
                       (char-numeric? (string-ref text (+ j 1)))
                       (let ((k (digits-end text (+ j 1))))
                         (or (= k (string-length text))
                             (memv (string-ref text k) '(#\space #\) #\" #\newline))))))
                (in-store-path? (- i 1)))
           (loop (digits-end text (+ i 5)) (append (reverse (string->list ".tmp-#")) out)))
          (else (loop (+ i 1) (cons (string-ref text i) out))))))
;; WHETHER A DAEMON'S ANSWER IS AN ok: the text is exactly ONE datum, the
;; envelope (answer (stdout "<datum>") (stderr ..) (exit <n>) ...); it has
;; an exit, as every envelope the product accepts does (core.sc,
;; answer-envelope?), and the exit is 0; and its stdout holds exactly one
;; datum, (ok ...). A row that drives a write asserts it with this, so a
;; refusal -- or an ok followed by a failure -- cannot be measured as the
;; thing it was meant to do.
(define (answered-ok? text)
  (define (all-data s)
    (let ((in (open-string-input-port s)))
      (let next ((acc '()))
        (let ((d (read in))) (if (eof-object? d) (reverse acc) (next (cons d acc)))))))
  (guard (e (#t #f))
    (let* ((outer (all-data text))
           (a (and (= 1 (length outer)) (car outer)))
           (out (and (pair? a) (eq? (car a) 'answer) (list? a) (assq 'stdout (cdr a))))
           (exit (and out (assq 'exit (cdr a))))
           (data (and out (string? (cadr out)) (all-data (cadr out)))))
      (and data exit (eqv? (cadr exit) 0)
           (= 1 (length data)) (pair? (car data)) (eq? (car (car data)) 'ok)))))
;; TEXT without its (trace store-check ...) lines, each with its newline.
;; PART B'S EXECUTOR LINES, (trace executing <ticket> #f), (trace
;; executing-done <ticket> #f) and (trace association-cleared <ticket> #t|#f),
;; one set around every request an executor runs, whatever its frame's
;; version: the base has none. -> (<the text
;; without them> . <those lines, in order>), so the guard can compare the
;; rest byte for byte and still check the pairing of what it left out.
(define (without-executor-lines text)
  (define (starts? l p) (and (>= (string-length l) (string-length p)) (string=? (substring l 0 (string-length p)) p)))
  (define (executor? l) (or (starts? l "(trace executing ") (starts? l "(trace executing-done ") (starts? l "(trace association-cleared ")))
  (let loop ((cs (string->list text)) (line '()) (out '()) (left '()))
    (cond ((null? cs)
           (let ((l (list->string (reverse line))))
             (cons (apply string-append (reverse (if (or (null? line) (executor? l)) out (cons l out))))
                   (reverse (if (and (pair? line) (executor? l)) (cons l left) left)))))
          ((char=? (car cs) #\newline)
           (let ((l (list->string (reverse line))))
             (if (executor? l)
                 (loop (cdr cs) '() out (cons l left))
                 (loop (cdr cs) '() (cons "\n" (cons l out)) left))))
          (else (loop (cdr cs) (cons (car cs) line) out left)))))
;; THE PAIRING: every ticket has exactly one executing, after it exactly one
;; executing-done, and after that exactly one association-cleared that says
;; #t; and there are as many tickets as the trace's requests routed to the
;; store or a writer (a connection-local read has none).
(define (executor-pairs-hold? lines rest)
  (let* ((data (map (lambda (l) (read (open-string-input-port l))) lines))
         (tickets (let loop ((ds data) (seen '()))
                    (cond ((null? ds) (reverse seen))
                          ((member (caddr (car ds)) seen) (loop (cdr ds) seen))
                          (else (loop (cdr ds) (cons (caddr (car ds)) seen))))))
         (routed (let loop ((ls (let split ((cs (string->list rest)) (line '()) (out '()))
                                  (cond ((null? cs) (reverse (if (null? line) out (cons (list->string (reverse line)) out))))
                                        ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse line)) out)))
                                        (else (split (cdr cs) (cons (car cs) line) out)))))
                           (n 0))
                   (cond ((null? ls) n)
                         ((let ((l (car ls)))
                            (and (>= (string-length l) 14) (string=? (substring l 0 14) "(trace routed ")
                                 (not (string=? (substring l (- (string-length l) 11) (string-length l)) "connection)"))))
                          (loop (cdr ls) (+ n 1)))
                         (else (loop (cdr ls) n))))))
    (and (= (length tickets) routed)
         (for-all (lambda (t)
                    (let ((mine (filter (lambda (d) (equal? (caddr d) t)) data)))
                      (and (equal? (map cadr mine) '(executing executing-done association-cleared))
                           (eq? #t (cadddr (caddr mine))))))
                  tickets))))
(define (without-store-checks text)
  (define (check? l) (and (>= (string-length l) 19) (string=? (substring l 0 19) "(trace store-check ")))
  (let loop ((cs (string->list text)) (line '()) (out '()))
    (cond ((null? cs)
           (let ((l (list->string (reverse line))))
             (apply string-append (reverse (if (or (null? line) (check? l)) out (cons l out))))))
          ((char=? (car cs) #\newline)
           (let ((l (list->string (reverse line))))
             (loop (cdr cs) '() (if (check? l) out (cons "\n" (cons l out))))))
          (else (loop (cdr cs) (cons (car cs) line) out)))))
;; check's (instance (machine ...) (device <n>) (inode <n>) ...) clause names
;; the store's identity, and its device and inode differ between any two
;; store directories by construction: the base and the tree run on two
;; copies. run-script hands check's raw answer here, before any text
;; normalisation. It is read as a datum, its stdout read as the check
;; datum, and in the instance clause directly under check each (device
;; <integer>) and (inode <integer>) gets 0 for its integer, as the process
;; id gets "#" in the machine and the nonce. 0 is an integer, so a value
;; whose type changes still compares. Strings are data and are never
;; looked into; every other field compares as it was.
;; NEVER: NO OTHER BYTE IS REWRITTEN. The answer is written back only when
;; its text is exactly what writing its datum gives -- the answer's own
;; text up to its ending, and stdout's text as the check datum written and
;; one newline -- so a change of spacing, of quoting or of the ending
;; leaves the text as it is and the guard compares it whole. An answer
;; that does not read, whose stdout is not exactly one check datum, or that
;; has no such field, is likewise left byte for byte.
(define (store-identity answer)
  (define (read-all text)
    (let ((p (open-string-input-port text)))
      (let loop ((out '()))
        (let ((d (read p))) (if (eof-object? d) (reverse out) (loop (cons d out)))))))
  (define (mask-field c)
    (if (and (list? c) (= (length c) 2) (memq (car c) '(device inode)) (integer? (cadr c)) (exact? (cadr c)))
        (list (car c) 0)
        c))
  (define (mask-check d)
    (cons 'check (map (lambda (c) (if (and (list? c) (pair? c) (eq? (car c) 'instance))
                                      (cons 'instance (map mask-field (cdr c)))
                                      c))
                      (cdr d))))
  (define (text-of d) (call-with-string-output-port (lambda (o) (write d o))))
  (define (prefix? p t) (and (<= (string-length p) (string-length t)) (string=? p (substring t 0 (string-length p)))))
  (guard (e (#t answer))
    (let ((as (read-all answer)))
      (if (not (and (= (length as) 1) (list? (car as)) (pair? (car as))))
          answer
          (let* ((a (car as))
                 (written (text-of a))
                 (ending (and (prefix? written answer) (substring answer (string-length written) (string-length answer))))
                 (so (assq 'stdout (cdr a)))
                 (ds (and so (list? so) (= (length so) 2) (string? (cadr so)) (read-all (cadr so)))))
            (if (not (and ending (member ending '("" "\n"))
                          ds (= (length ds) 1) (list? (car ds)) (pair? (car ds)) (eq? (car (car ds)) 'check)
                          (string=? (cadr so) (string-append (text-of (car ds)) "\n"))
                          (not (equal? (mask-check (car ds)) (car ds)))))
                answer
                (string-append
                  (text-of (map (lambda (c) (if (eq? c so) (list 'stdout (string-append (text-of (mask-check (car ds))) "\n")) c))
                                a))
                  ending)))))))
(define (check-answer text)
  (call-with-string-output-port
    (lambda (o) (write (list 'answer (list 'stdout (string-append text "\n")) '(stderr "") '(exit 0) '(origin core)) o))))
(want "GUARD-0 the device and inode of check's instance clause are masked, and nothing else is"
      (list
        ;; the clause's two numbers become "#"; (end 11) and (inodes 3) stay
        (store-identity (check-answer "(check (notes ()) (instance (machine \"m-1-#\") (device 16777234) (inode 122480277) (nonce \"n-1-#\")) (end 11) (inodes 3))"))
        ;; a nonce whose text holds "(inode 7)" still differs from one holding "(inode 8)"
        (equal? (store-identity (check-answer "(check (instance (machine \"m\") (device 1) (inode 2) (nonce \"(inode 7)\")))"))
                (store-identity (check-answer "(check (instance (machine \"m\") (device 1) (inode 2) (nonce \"(inode 8)\")))")))
        ;; a parenthesis in the machine string does not carry the clause into a sibling
        (equal? (store-identity (check-answer "(check (instance (machine \"(\") (device 1) (inode 2)) (device 7))"))
                (store-identity (check-answer "(check (instance (machine \"(\") (device 1) (inode 2)) (device 8))")))
        ;; an instance clause in another verb's answer, a payload, a value that is
        ;; not an integer, and an answer that does not read are left as they are
        (let ((xs (list (check-answer "(ok (instance (machine \"m\") (device 1) (inode 2)))")
                        (check-answer "(ok (text \"(check (instance (machine \\\"m\\\") (device 1)))\"))")
                        (check-answer "(check (instance (machine \"m\") (device 12x) (inode 2.5)))")
                        "(answer (stdout \"(check (instance (device 1)\"))")))
          (equal? (map store-identity xs) xs)))
      (list (check-answer "(check (notes ()) (instance (machine \"m-1-#\") (device 0) (inode 0) (nonce \"n-1-#\")) (end 11) (inodes 3))")
            #f #f #t))
(want "GUARD-0b a device that turned into a string, a clause that lost its inode, and a check printed with other spacing still differ from the base's; another inode does not"
      (let ((base (store-identity (check-answer "(check (instance (machine \"m\") (device 5) (inode 7)))"))))
        (list (equal? base (store-identity (check-answer "(check (instance (machine \"m\") (device \"#\") (inode 9)))")))
              (equal? base (store-identity (check-answer "(check (instance (machine \"m\") (device 5)))")))
              (equal? base (store-identity (string-append "(answer (stdout \"(check  (instance (machine \\\"m\\\") (device 5) (inode 9)))\\n\") "
                                                          "(stderr \"\") (exit 0) (origin core))")))
              (equal? base (store-identity (check-answer "(check (instance (machine \"m\") (device 6) (inode 9)))")))))
      '(#f #f #f #t))
;; GUARD-0c A TRACE'S TEMPORARY HAS ITS PID MASKED IN ITS ONE FORM AND NOWHERE
;; ELSE. Every control is a trace line whose path starts STORE, so each one
;; reaches the matcher: two daemons' pids read alike AND the mask is seen to
;; fire (".tmp-#-26"); the same form outside a store path, a ".tmp-" with no
;; "-<n>", a counter, and text running on past the counter are left as they
;; are; and answers (session-pids) are not masked at all.
(define (tr path) (string-append "(trace create " path " #f)"))
(want "GUARD-0c only a STORE path's .tmp-<pid>-<n> has its pid masked: two pids read alike and the mask fires; a payload's, .tmp-12/.tmp-13, the counter, a longer name, and an answer still differ"
      (list (string=? (mask-temporaries (tr "STORE-home/instances.sexp.tmp-3336-26"))
                      (mask-temporaries (tr "STORE-home/instances.sexp.tmp-3348-26")))
            (mask-temporaries (tr "STORE-home/instances.sexp.tmp-3336-26"))
            (string=? (mask-temporaries "(trace create \"x.tmp-3336-26\" #f)") (mask-temporaries "(trace create \"x.tmp-3348-26\" #f)"))
            (string=? (mask-temporaries (tr "STORE/x.tmp-12")) (mask-temporaries (tr "STORE/x.tmp-13")))
            (string=? (mask-temporaries (tr "STORE/x.tmp-12-3")) (mask-temporaries (tr "STORE/x.tmp-12-4")))
            (string=? (mask-temporaries (tr "STORE/x.tmp-12-3.bak")) (mask-temporaries (tr "STORE/x.tmp-13-3.bak")))
            (string=? (session-pids (tr "STORE/x.tmp-3336-26")) (session-pids (tr "STORE/x.tmp-3348-26"))))
      (list #t "(trace create STORE-home/instances.sexp.tmp-#-26 #f)" #f #f #f #f #f))
;; answered-ok? reads the whole envelope: an ok alone with exit 0 is an ok; an
;; ok followed by a second datum, an ok with a non-zero exit, an ok with no
;; exit, an envelope followed by more text, a refusal, and something that
;; is not an answer are not.
(want "GUARD-0d answered-ok? is true only for one envelope with exit 0 whose stdout is one ok datum"
      (map answered-ok?
           (list "(answer (stdout \"(ok (x 1))\\n\") (stderr \"\") (exit 0) (origin core))"
                 "(answer (stdout \"(ok (x 1))\\n(error late)\\n\") (stderr \"\") (exit 0) (origin core))"
                 "(answer (stdout \"(ok (x 1))\\n\") (stderr \"\") (exit 1) (origin core))"
                 "(answer (stdout \"(ok (x 1))\\n\") (stderr \"\") (origin core))"
                 "(answer (stdout \"(ok (x 1))\\n\") (stderr \"\") (exit 0) (origin core)) (answer (stdout \"(error late)\") (exit 1))"
                 "(answer (stdout \"(error unknown-id \\\"x\\\")\\n\") (stderr \"\") (exit 1) (origin core))"
                 "(not-an-answer)"))
      '(#t #f #f #f #f #f #f))
(define (normalize-for d text)
  (normalize (session-pids (replace-all (replace-all (replace-all text (cadr d) "SOCKET") (car d) "STORE") (list-ref d 4) "HOME"))))
(define (normalize text)
  (let loop ((i 0) (out '()))
    (cond ((>= i (string-length text)) (list->string (reverse out)))
          ((char-numeric? (string-ref text i))
           (let run ((j i)) (if (and (< j (string-length text)) (char-numeric? (string-ref text j))) (run (+ j 1))
                                (if (>= (- j i) 12) (loop j (cons #\# out))
                                    (loop j (append (reverse (string->list (substring text i j))) out))))))
          (else (loop (+ i 1) (cons (string-ref text i) out))))))

;; ---- one prepared store, two copies, two daemons -------------------------------
(define prepared (string-append scratch "/prepared"))
(system (string-append "mkdir -p " prepared))
(define (lib-env lib) (string-append "CHEZSCHEMELIBDIRS=" lib " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"))
(system (string-append (lib-env this-lib) " scheme --script ../theourgia.sc init --store " prepared " > /dev/null 2>&1 < /dev/null"))
(system (string-append (lib-env this-lib) " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc insert --title Seed --text seed --store " prepared " > /dev/null 2>&1 < /dev/null"))
;; THE DAEMON PROGRAM, relative to a library root. The spawn in start! names
;; it by this define, so a reading of this file's source (entry-split's
;; SPAWNS) sees which program the spawn starts.
(define daemon-file "theourgia/theourgiad.sc")
;; THE ROOT OF A LIBRARY PATH that holds daemon-file: the first of its roots
;; that does.
(define (daemon-root lib)
  (let loop ((roots (let split ((i 0) (start 0) (out '()))
                      (cond ((= i (string-length lib)) (reverse (cons (substring lib start i) out)))
                            ((char=? (string-ref lib i) #\:) (split (+ i 1) (+ i 1) (cons (substring lib start i) out)))
                            (else (split (+ i 1) start out))))))
    (cond ((null? roots)
           (printf "NOT A READING: no ~a under ~s~%" daemon-file lib) (exit 2))
          ((file-exists? (string-append (car roots) "/" daemon-file)) (car roots))
          (else (loop (cdr roots))))))
(define (start! name lib)
  (let* ((st (string-append scratch "/" name)) (sk (string-append sockets "/" name ".sock"))
         (hm (string-append scratch "/" name "-home"))
         (rt (daemon-root lib)) (rn (string-append rt "/" daemon-file)) (lg (string-append scratch "/" name ".log")))
    (system (string-append "rm -rf " st " " hm "; cp -R " prepared " " st "; cp -R " prepared-home " " hm))
    ;; the copy's own device and inode, in the identity it carries
    (system (string-append "perl -e 'my ($f, $d) = @ARGV; my @s = stat $d; open my $h, q(<), $f or die; local $/; my $t = <$h>; "
                           "$t =~ s/\\(device \\d+\\)/(device $s[0])/; $t =~ s/\\(inode \\d+\\)/(inode $s[1])/; "
                           "open $h, q(>), $f or die; print $h $t' " st "/instance.sexp " st))
    ;; THE PROGRAM IS daemon-file under RT, the first root of LIB that holds
    ;; it (RN, the same path); the socket path, unique to this daemon, is what
    ;; stop! matches.
    (system (string-append "THEOURGIA_TRACE=1 THEOURGIA_HOME=" hm " " (lib-env lib) " sh -c 'scheme --script " rt "/" daemon-file
                           " serve " st " --socket " sk " > " lg " 2>&1' &"))
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg hm))
            ((> k 400) (printf "NOT A READING: the ~a daemon never created its socket~%~a~%" name (file-text lg)) (exit 2))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (stop! d) (system (string-append "pkill -f " (cadr d) " 2>/dev/null")))
(define (line-complete? bv)
  (let loop ((i 0)) (cond ((>= i (bytevector-length bv)) #f) ((= (bytevector-u8-ref bv i) 10) #t) (else (loop (+ i 1))))))
(define (ask-raw d verb args)
  (let* ((text (string-append "(request 1 " (format "~s" (car d)) " \"tester\" #f wire #f #f " (symbol->string verb)
                              (apply string-append (map (lambda (a) (string-append " " (format "~s" a))) args)) ")\n"))
         (r (exchange (cadr d) (string->utf8 text) line-complete? 15000)))
    (if (and (pair? r) (eq? 'answer (car r))) (utf8->string (cadr r)) (format "~s" r))))

;; THE SCRIPT: requests over the verbs a client uses, in an order where each
;; can depend on the last. Ids are the seed's and the ones made here, the
;; same on both copies (one writer, one sequence).
(define script
  '((outline) (insert "--title" "A" "--text" "alpha") (insert "--title" "B" "--text" "beta")
    (read "SEED") (read "SEED" "--md") (read "SEED" "--recursive") (outline "--depth" "3")
    (set "A" "src" "alpha two") (set "A" "title" "A2") (move "B" "A") (link "A" "relates" "B")
    (refs "A") (unlink "A" "relates" "B") (search "alpha") (grep "beta") (whereis "A2")
    (conflicts) (log) (write "A" "a draft" "--writer" "w1") (read "A" "--working" "--writer" "w1") (drafts "--writer" "w1")
    (discard "A" "--writer" "w1")
    (write "A" "a second draft" "--writer" "w2") (diagnostics "--writer" "w2") (commit "--writer" "w2")
    (restore "no-such-version" "--writer" "w2") (reach "A") (tag "named") (tag) (diff "named" "named")
    (batch "()") (snapshot) (check) (adopt) (log "A")
    (del "B") (read "B") (outline)
    (query "--relations") (context "--for" "SEED" "--budget" "2000") (names "SEED")
    (describe) (no-such-verb) (read)))
;; (label reason) entries; label is the script's verb, or read-usage for read
;; with no arguments.
(define expected-differences
  (let ((f (string-append script-dir "/change-stream-guard-expect.sexp")))
    (if (file-exists? f) (call-with-input-file f read) 'NO-EXPECTATION-FILE)))
(define (resolve d args ids)
  (map (lambda (a) (let ((e (assoc a ids))) (if e (cdr e) a))) args))
(define (run-script d)
  ;; -> list of (label . normalized answer)
  (let loop ((ss script) (ids '()) (out '()))
    (if (null? ss) (reverse out)
        (let* ((req (car ss)) (verb (car req)) (args (resolve d (cdr req) ids))
               (answer (ask-raw d verb args))
               (label (cond ((and (eq? verb 'read) (null? args)) 'read-usage) (else verb)))
               (ids (cond ((and (eq? verb 'insert) (pair? (cdr req)))
                           ;; an answer of another shape (a refusal) names no id
                           (let ((id (guard (e (#t #f))
                                       (let* ((a (read (open-string-input-port answer)))
                                              (inner (and (pair? a) (assq 'stdout (cdr a)) (read (open-string-input-port (cadr (assq 'stdout (cdr a)))))))
                                              (ev (and (pair? inner) (assq 'events (cdr inner)))))
                                         (and ev (let ((e (car (cadr ev)))) (string-append (car e) "." (number->string (cdr e)))))))))
                             (if id (cons (cons (caddr req) id) ids) ids)))
                          (else ids))))
          (loop (cdr ss) ids (cons (cons label (normalize-for d (if (eq? verb 'check) (store-identity answer) answer)))
                                   out))))))

(start-scheduler
  (lambda ()
    (let* ((seed (let ((a (system (string-append (lib-env this-lib) " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc outline --wire --store " prepared " > " scratch "/seed.out 2>&1 < /dev/null"))))
                   ;; THE SEED'S ID, read from outline's answer: (ok (text "- <id>  Seed\n") ...),
                   ;; the text's first line after "- " up to its first space. The first quoted
                   ;; string of the output is the whole line, not the id, and every request
                   ;; naming SEED was then refused unknown-id on both daemons alike.
                   (let* ((in (open-string-input-port (file-text (string-append scratch "/seed.out"))))
                          (ok (let next ((d (read in)))
                                (cond ((eof-object? d) #f)
                                      ((and (pair? d) (eq? (car d) 'ok) (assq 'text (cdr d))) d)
                                      (else (next (read in))))))
                          (line (and ok (cadr (assq 'text (cdr ok))))))
                     (and (string? line) (>= (string-length line) 3) (string=? (substring line 0 2) "- ")
                          (let stop ((j 2))
                            (cond ((= j (string-length line)) (substring line 2 j))
                                  ((char=? (string-ref line j) #\space) (substring line 2 j))
                                  (else (stop (+ j 1)))))))))
           (_ (set! script (map (lambda (r) (map (lambda (a) (if (equal? a "SEED") seed a)) r)) script)))
           (b (start! "base" base-lib))
           (n (start! "new" this-lib))
           (rb (run-script b))
           (rn (run-script n)))
      (let* ((differing (let loop ((xs rb) (ys rn) (out '()))
                          (cond ((or (null? xs) (null? ys)) (reverse out))
                                ((and (not (equal? (cdar xs) (cdar ys))) (not (memq (caar xs) out)))
                                 (loop (cdr xs) (cdr ys) (cons (caar xs) out)))
                                (else (loop (cdr xs) (cdr ys) out)))))
             (listed (if (list? expected-differences) (map car expected-differences) '())))
        (want "GUARD-1 the expectation file is read, and every entry gives its reason"
              (and (list? expected-differences)
                   (for-all (lambda (e) (and (pair? e) (symbol? (car e)) (pair? (cdr e)) (string? (cadr e)))) expected-differences))
              #t)
        (want "GUARD-2 every answer of the script that differs from the base's is listed, and every listed one differs"
              (list (filter (lambda (l) (not (memq l listed))) differing)
                    (filter (lambda (l) (not (memq l differing))) listed))
              '(() ()))
        (for-each (lambda (l)
                    (unless (memq l listed)
                      (printf "   unlisted difference ~s:~%   base ~a~%   tree ~a~%" l (cdr (assq l rb)) (cdr (assq l rn)))))
                  differing))
      ;; GUARD-7 THE SEED IS AN ID THE STORE HAS: every request naming SEED
      ;; names it, so a read of it answers ok on both daemons. While the seed
      ;; was the outline's whole line, each of those requests was refused
      ;; unknown-id on both sides alike, and GUARD-3 and GUARD-4 compared
      ;; refusals, not a read and a commit.
      (want "GUARD-7 the seed is an id the store has: read <seed> answers ok on both daemons"
            (map (lambda (d)
                   (guard (e (#t #f))
                     (let* ((a (read (open-string-input-port (ask-raw d 'read (list seed)))))
                            (o (and (pair? a) (eq? (car a) 'answer) (assq 'stdout (cdr a))
                                    (read (open-string-input-port (cadr (assq 'stdout (cdr a))))))))
                       (and (pair? o) (eq? (car o) 'ok)))))
                 (list b n))
            '(#t #t))
      ;; GUARD-5, GUARD-6: EACH DAEMON KNOWS THE VERBS ITS PROGRAM SHIPS.
      ;; describe through the daemon lists the verbs describe lists from the
      ;; same tree's command line, the extension verbs among them, and each
      ;; extension request of the script is ANSWERED by both daemons: an ok
      ;; datum, the context one naming the block it was asked for. A refusal
      ;; of any kind (usage, unknown-verb, unknown-id) is not an answer here,
      ;; so GUARD-2 cannot be comparing two equal refusals.
      (let* ((unwrap (lambda (text)
                       (guard (e (#t #f))
                         (let ((a (read (open-string-input-port text))))
                           (if (and (pair? a) (eq? (car a) 'answer) (assq 'stdout (cdr a)))
                               (read (open-string-input-port (cadr (assq 'stdout (cdr a)))))
                               a)))))
             (names-of (lambda (text)
                         (let ((d (unwrap text)))
                           (and (pair? d) (eq? (car d) 'ok) (assq 'verbs (cdr d))
                                (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
                                           (map car (cdr (assq 'verbs (cdr d)))))))))
             (cli-describe (lambda (lib program)
                             (system (string-append (lib-env lib) " THEOURGIA_LOCAL=1 scheme --script " program
                                                    " describe --wire --store " prepared " > " scratch "/cli-describe.out 2>/dev/null < /dev/null"))
                             (let ((t (file-text (string-append scratch "/cli-describe.out"))))
                               ;; the CLI's line may follow a machine-home banner
                               (let ((i (let find ((k 0)) (cond ((>= (+ k 3) (string-length t)) #f)
                                                                ((string=? (substring t k (+ k 4)) "(ok ") k)
                                                                (else (find (+ k 1)))))))
                                 (if i (substring t i (string-length t)) t)))))
             (new-cli (names-of (cli-describe this-lib "../theourgia.sc")))
             (base-cli (names-of (cli-describe base-lib (string-append base-lib "/theourgia/theourgia.sc"))))
             (new-d (names-of (ask-raw n 'describe '())))
             (base-d (names-of (ask-raw b 'describe '()))))
        (want "GUARD-5 each daemon's describe lists the verbs its tree's command line lists, context and query among them"
              (list (and new-d (equal? new-d new-cli)) (and base-d (equal? base-d base-cli))
                    (and new-d (memq 'context new-d) (memq 'query new-d) #t))
              '(#t #t #t))
        (want "GUARD-6 each extension request of the script is answered ok by both daemons, context for the block asked"
              (map (lambda (d)
                     (let ((c (unwrap (ask-raw d 'context (list "--for" seed "--budget" "2000"))))
                           (q (unwrap (ask-raw d 'query (list "--relations"))))
                           (m (unwrap (ask-raw d 'names (list seed)))))
                       (list (and (pair? c) (eq? (car c) 'ok)
                                  (let ((f (assq 'for (cdr c))))
                                    (and f (pair? (cdr f)) (pair? (cadr f)) (equal? (car (cadr f)) seed))))
                             (and (pair? q) (eq? (car q) 'ok))
                             (and (pair? m) (eq? (car m) 'ok)))))
                   (list b n))
              '((#t #t #t) (#t #t #t))))
      ;; THE TRACE of one read and one commit, each daemon's lines between two markers.
      ;; NOTE: THE STORE CHECK'S LINES ARE LEFT OUT OF BOTH: the signal watcher
      ;; samples the store about once a second, on its own clock and not because
      ;; of the requests, so whether one falls in this window is chance, and the
      ;; base has none.
      ;; -> (<read and set both answered ok> . <trace>): a refused set has no
      ;; commit to trace, so the row asserts both answers before comparing.
      ;; NOTE: SO ARE PART B'S EXECUTOR LINES (without-executor-lines): the
      ;; rest is compared byte for byte, and what was left out is checked for
      ;; its pairing, one executing, one executing-done and one
      ;; association-cleared #t per request an executor ran; the base writes none.
      ;; -> (<read and set both answered ok> <trace> <executor lines>)
      (let ((trace-of (lambda (d)
                        (let* ((before (string-length (file-text (cadddr d))))
                               (r (answered-ok? (ask-raw d 'read (list seed))))
                               (w (answered-ok? (ask-raw d 'set (list seed "src" "traced")))))
                          (sleep-ms 300)
                          (let ((split (without-executor-lines
                                         (without-store-checks
                                           (substring (file-text (cadddr d)) before (string-length (file-text (cadddr d))))))))
                            (list (and r w) (mask-temporaries (normalize-for d (car split))) (cdr split)))))))
        (want "GUARD-3 a read and a commit are answered ok on both daemons, and the trace of them, Part B's executor lines left out, is byte for byte the base's; the base has none, and the new daemon's set (executing, executing-done, association-cleared #t) once per executed request"
              (let ((tb (trace-of b)) (tn (trace-of n)))
                (list (car tb) (car tn)
                      (if (equal? (cadr tb) (cadr tn)) 'equal (list (cadr tb) (cadr tn)))
                      (length (caddr tb))
                      (and (pair? (caddr tn)) (executor-pairs-hold? (caddr tn) (cadr tn)))))
              '(#t #t equal 0 #t)))
      ;; THE TIME OF COMMITS, BY ALTERNATING PAIRS. Forty rounds; each round
      ;; times ten commits on each daemon, the order flipped every round, so
      ;; a drift of the machine falls on both arms alike. Each arm is read by
      ;; its 10th percentile, the low edge a quiet machine reaches, not by a
      ;; median that moves with the load.
      ;; THE BOUND: new p10 <= base p10 x (1 + m), m = max(0.15, 2e), where e
      ;; is the base's own run-to-run deviation, |p10 of its first twenty
      ;; rounds / p10 of its last twenty - 1|. It is the start-up
      ;; instrument's construction of K (a floor for quiet noise, widened by
      ;; twice what this run shows the base itself moving), computed from
      ;; this run, since the base and the tree run side by side here. Every
      ;; number is printed. A BASE THAT MOVED BY MORE THAN HALF (e > 0.5) WAS
      ;; NOT A QUIET RUN: the row then says NOT A READING and is red, rather
      ;; than passing on a bound that widened to fit the noise.
      ;; EVERY TIMED SET IS COUNTED BY ITS ANSWER (not-ok, per daemon): the time
      ;; of refusals is not the time of commits, so GUARD-4b asserts there
      ;; were none before GUARD-4's bound is believed.
      (let* ((not-ok (list (cons 'base 0) (cons 'new 0)))
             (time-10 (lambda (d k)
                        (let ((t0 (real-time)) (arm (assq (if (eq? d b) 'base 'new) not-ok)))
                          (let loop ((i 0))
                            (when (< i 10)
                              (unless (answered-ok? (ask-raw d 'set (list seed "title" (string-append "t" (number->string k) "-" (number->string i)))))
                                (set-cdr! arm (+ (cdr arm) 1)))
                              (loop (+ i 1))))
                          (- (real-time) t0))))
             (rounds (let loop ((k 0) (out '()))
                       (if (= k 40) (reverse out)
                           (loop (+ k 1)
                                 (cons (if (even? k)
                                           (let* ((x (time-10 b k)) (y (time-10 n k))) (list x y))
                                           (let* ((y (time-10 n k)) (x (time-10 b k))) (list x y)))
                                       out)))))
             (base-ms (map car rounds)) (new-ms (map cadr rounds))
             (p10 (lambda (xs) (list-ref (list-sort < xs) (div (length xs) 10))))
             (e (abs (- (/ (p10 (list-head base-ms 20)) (max 1 (p10 (list-tail base-ms 20)))) 1)))
             (m (max 15/100 (* 2 e)))
             (bound (* (p10 base-ms) (+ 1 m))))
        (printf "   10 commits x 40 alternating rounds, ms: base p10 ~s new p10 ~s; e ~s m ~s bound ~s~%   base ~s~%   new  ~s~%"
                (p10 base-ms) (p10 new-ms) (inexact e) (inexact m) (inexact bound) base-ms new-ms)
        (when (> e 1/2)
          (printf "NOT A READING: the base's p10 moved by ~s between its halves (more than 0.5); the machine was not quiet~%" (inexact e)))
        (want "GUARD-4b every one of the 400 timed sets on each daemon was answered ok: commits, not refusals, were timed"
              not-ok
              '((base . 0) (new . 0)))
        (want "GUARD-4 the commits' p10 over forty alternating rounds is within the base's p10 x (1 + max(0.15, 2e)), on a quiet run (e <= 0.5)"
              (if (> e 1/2) 'not-a-reading (<= (p10 new-ms) bound))
              #t))
      (stop! b) (stop! n)
      (system (string-append "rm -rf " sockets))
      (printf "rows: ~a~%~a failures~%change-stream-guard complete~%" rows bad)
      (exit (if (= bad 0) 0 1)))))
