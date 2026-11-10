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

;; WHERE THE FIXTURES SEND SIGNALS, read as text, and the one send that checks
;; its pid, run.
;;
;; A pid is reused once its process is gone, so a signal to a pid that is no
;; longer the process it was read for can reach an unrelated one; and a pkill
;; pattern that ends right after this run's pid digits also matches a run
;; whose pid begins with them. The census of every send in test/*.sc, test/*.pl
;; and test/run-fixtures.sh found both. These rows hold the fixed ones fixed:
;;
;; SA-0  the files scanned are counted, and a count below the census's (255)
;;       is refused, so a scan that found nothing reads red, not green.
;; SA-1  no `pkill -f` pattern ends right after a name that ends in the run's
;;       pid: in the source, the name is spliced in and the pattern's closing
;;       quote follows it at once (`" here "'`).
;; SA-2  no pkill pattern is a literal with nothing of the run spliced in.
;; SA-3  in each file whose sends were moved onto signal! (or, in generated
;;       shell text, onto the same check made inline), the bare sends left are
;;       exactly the ones named below, each with its census row; and the
;;       checked sends, and the lookups by ps that feed them, are there.
;; SA-4  signal! itself (test/signal-send.ss): a registered process that has
;;       ended is not signalled, a live one is, an unregistered one is not,
;;       and one whose recorded start differs is not.
;; SA-5  no removal glob has `*` right after a splice of the run's pid.
;; SA-6  the lookup by store (test/signal-send.ss): the word after `serve`,
;;       or after `serve --store`, is the store, so a longer store that
;;       begins with it is not found; and on a real process, the lookup finds
;;       it with the instant ps prints for it, and signal-processes! ends it.
;; SA-7  the self-signalling loop (signal-loop-text): a program whose start
;;       instant ps cannot read is never signalled; with the real ps it is.
;;
;; NEVER: A TEXT RULE IS NOT A PROOF. SA-1..3 read the files line by line with
;; named patterns; they catch the shapes the census found and say nothing
;; about a send written some other way. The comments of a file (lines whose
;; first character is ; or #) are not read.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list (quote RAISED)))) e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))
(include "signal-send.ss")

;; ---- reading the files -----------------------------------------------------------

(define (file-lines path)
  (let ((t (guard (e (#t "")) (call-with-input-file path get-string-all))))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((or (eof-object? t) (= i (string-length t)))
             (reverse (if (and (string? t) (> i start)) (cons (substring t start i) acc) acc)))
            ((char=? (string-ref t i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring t start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (index-of text sub from)
  (let ((n (string-length sub)) (m (string-length text)))
    (let loop ((i from))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) sub) i)
            (else (loop (+ i 1)))))))
(define (has? text sub) (and (index-of text sub 0) #t))
(define (ends-with? text sub)
  (let ((n (string-length sub)) (m (string-length text)))
    (and (>= m n) (string=? (substring text (- m n) m) sub))))
(define (starts-with? text sub)
  (let ((n (string-length sub)))
    (and (>= (string-length text) n) (string=? (substring text 0 n) sub))))
(define (trim text)
  (let loop ((a 0) (b (string-length text)))
    (cond ((and (< a b) (char-whitespace? (string-ref text a))) (loop (+ a 1) b))
          ((and (< a b) (char-whitespace? (string-ref text (- b 1)))) (loop a (- b 1)))
          (else (substring text a b)))))
(define (comment-line? line)
  (let ((t (trim line)))
    (and (> (string-length t) 0) (memv (string-ref t 0) '(#\; #\#)) #t)))

;; -> ((line-number . text) ...) of the lines that are not comments.
(define (code-lines path)
  (let loop ((ls (file-lines path)) (n 1) (acc '()))
    (cond ((null? ls) (reverse acc))
          ((comment-line? (car ls)) (loop (cdr ls) (+ n 1) acc))
          (else (loop (cdr ls) (+ n 1) (cons (cons n (car ls)) acc))))))

(define (scope)
  (let ((names (list-sort string<? (directory-list "."))))
    (append (filter (lambda (n) (or (ends-with? n ".sc") (ends-with? n ".pl"))) names)
            (if (file-exists? "run-fixtures.sh") '("run-fixtures.sh") '()))))

;; ---- SA-0: the scan saw the census's files ---------------------------------------
(define census-files 255)
(define scanned (scope))
(printf "SA-0 scanned ~a files (test/*.sc, test/*.pl, run-fixtures.sh)\n" (length scanned))
(want "SA-0 the scan covers at least the census's 255 files"
      (>= (length scanned) census-files)
      #t)

;; ---- SA-1: a pkill pattern ends where this run's path does ----------------------
;;
;; THE NAMES THAT END IN THE RUN'S PID, per file: pid-text, and every name a
;; one-line `(define <name> (string-append ...))` makes whose last argument is
;; (number->string (get-process-id)) or a name already known to end in the
;; pid. Three passes reach a name defined from a name defined from the pid.
(define pid-expr "(number->string (get-process-id))")
(define (pid-ending-names lines)
  (let pass ((k 0) (names '("pid-text")))
    (if (= k 3)
        names
        (pass (+ k 1)
              (fold-left
                (lambda (acc l)
                  (let ((t (trim (cdr l))))
                    (if (and (starts-with? t "(define ") (has? t " (string-append "))
                        (let* ((i (string-length "(define "))
                               (j (index-of t " " i))
                               (name (and j (substring t i j))))
                          (if (and name (not (member name acc))
                                   (or (ends-with? t (string-append " " pid-expr "))"))
                                       (exists (lambda (n) (ends-with? t (string-append " " n "))"))) acc)))
                              (cons name acc)
                              acc))
                        acc)))
                names lines)))))

;; -> the names spliced into LINE as `" <name> "'`, the pattern closed at once.
(define (spliced-before-quote line)
  (let loop ((i 0) (acc '()))
    (let ((s (index-of line "\" " i)))
      (if (not s)
          (reverse acc)
          (let ((e (index-of line " \"'" (+ s 2))))
            (if (and e (let ((name (substring line (+ s 2) e)))
                         (and (> (string-length name) 0)
                              (or (string=? name pid-expr)
                                  (for-all (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\-)))
                                           (string->list name))))))
                (loop (+ s 1) (cons (substring line (+ s 2) e) acc))
                (loop (+ s 1) acc)))))))

(define sa1-hits
  (apply append
    (map (lambda (f)
           (let* ((ls (code-lines f)) (names (pid-ending-names ls)))
             (fold-right
               (lambda (l acc)
                 (if (and (has? (cdr l) "pkill")
                          (exists (lambda (n) (or (string=? n pid-expr) (member n names)))
                                  (spliced-before-quote (cdr l))))
                     (cons (list f (car l)) acc)
                     acc))
               '() ls)))
         scanned)))
(for-each (lambda (h) (printf "SA-1 hit ~a:~a\n" (car h) (cadr h))) sa1-hits)
(want "SA-1 no pkill -f pattern ends right after a name that ends in the run's pid"
      sa1-hits
      '())

;; ---- SA-2: a pkill pattern carries something of the run --------------------------
;;
;; A pattern written whole inside one string literal -- `-f '` and the closing
;; quote with no `"` between them -- has nothing of the run spliced into it,
;; so it matches every run's processes and every user's. A splice is taken as
;; the run's token; whether it ends where the path does is SA-1's question.
(define (literal-pattern? line)
  (let ((p (index-of line "pkill" 0)))
    (and p
         (let ((s (index-of line "-f '" p)))
           (and s
                (let ((e (index-of line "'" (+ s 4))))
                  (and e (not (has? (substring line (+ s 4) e) "\"")))))))))
(define sa2-hits
  (apply append
    (map (lambda (f)
           (fold-right (lambda (l acc) (if (literal-pattern? (cdr l)) (cons (list f (car l)) acc) acc))
                       '() (code-lines f)))
         scanned)))
(for-each (lambda (h) (printf "SA-2 hit ~a:~a\n" (car h) (cadr h))) sa2-hits)
(want "SA-2 no pkill pattern is a literal with nothing of the run in it"
      sa2-hits
      '())

;; ---- SA-3: the files whose sends were moved onto the check -----------------------
;;
;; A BARE SEND is `kill` (not after a letter, digit, `_`, `.` or `-`, so not
;; pkill and not os.killpg), an optional signal other than -0, spaces, and
;; then a pid, a `$` or a splice; or `kill -` followed at once by `"`, a
;; signal spliced in (`(string-append "kill -" name " " pid)`), whatever comes
;; after it. A line holding `ps -o lstart=` is the inline check of generated
;; shell text, not a bare send.
;;
;; PERL'S SENDS ARE READ TOO: `kill '<NAME>',`, `kill "<NAME>",` and `kill
;; <NAME> =>`, each followed by a target, `-$x`, `$x` or a number, and each
;; also inside parentheses, `kill('<NAME>', ...)` or `kill ('<NAME>', ...)`.
;; None is the shell's shape, so launch.pl's four sends went unread until
;; this rule. NOT READ, STATED: a signal given by number or by a variable
;; (`kill 9, $pid`, `kill $sig, $pid`); no file of the scan sends that way.
;;
;; NEVER: A SPLICED SIGNAL IS A SEND. The rule first read only a signal
;; written out, so daemon-e1's `"kill -" name` passed every row unseen while
;; its file's entry was being written (found 11-01 by the base reading of the
;; widened rule, which names that one line and nothing else on the base).
;;
;; KEY: THE EXEMPTION IS BY LINE, so the inline check's send and its lstart
;; comparison stay on one source line (retire-race's kill9-if-same-sh: `&& [
;; "$(ps -o lstart= ...)" = ... ] && kill -9 $1`). Split across two lines, the
;; send is read as bare and its file's row goes red; it cannot pass unseen.
(define (word-char? c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\_ #\. #\-))))
(define (perl-target-at? line j)
  (let* ((len (string-length line))
         (j (let scan ((j j)) (if (and (< j len) (char=? (string-ref line j) #\space)) (scan (+ j 1)) j)))
         (j (if (and (< j len) (char=? (string-ref line j) #\-)) (+ j 1) j)))
    (and (< j len)
         (let ((c (string-ref line j)))
           (or (char=? c #\$) (char-numeric? c))))))
(define (signal-name-end line j)
  (let ((len (string-length line)))
    (let scan ((k j))
      (if (and (< k len) (char-upper-case? (string-ref line k))) (scan (+ k 1)) k))))
(define (perl-send-at? line after)
  (let* ((len (string-length line))
         (skip (lambda (j) (let scan ((j j)) (if (and (< j len) (char=? (string-ref line j) #\space)) (scan (+ j 1)) j))))
         (s0 (skip after))
         (paren (and (< s0 len) (char=? (string-ref line s0) #\()))
         (s (if paren (skip (+ s0 1)) s0)))
    (and (> s after)
         (< s len)
         (let ((c (string-ref line s)))
           (cond
             ((memv c '(#\' #\"))
              (let ((e (signal-name-end line (+ s 1))))
                (and (> e (+ s 1))
                     (< (+ e 1) len)
                     (char=? (string-ref line e) c)
                     (char=? (string-ref line (+ e 1)) #\,)
                     (perl-target-at? line (+ e 2)))))
             ((char-upper-case? c)
              (let ((e (signal-name-end line s)))
                (and (<= (+ e 3) len)
                     (string=? (substring line e (+ e 3)) " =>")
                     (perl-target-at? line (+ e 3)))))
             (else #f))))))
(define (bare-sends-in line)
  (let loop ((i 0) (n 0))
    (let ((k (index-of line "kill" i)))
      (if (not k)
          n
          (let* ((after (+ k 4))
                 (len (string-length line))
                 (flag-end (if (and (< (+ after 1) len) (char=? (string-ref line after) #\space)
                                    (char=? (string-ref line (+ after 1)) #\-))
                               (let scan ((j (+ after 2)))
                                 (if (and (< j len) (or (char-upper-case? (string-ref line j))
                                                        (char-numeric? (string-ref line j))))
                                     (scan (+ j 1))
                                     j))
                               after))
                 (spliced (and (< (+ after 2) len) (char=? (string-ref line after) #\space)
                               (char=? (string-ref line (+ after 1)) #\-)
                               (char=? (string-ref line (+ after 2)) #\")))
                 (flag (substring line after flag-end))
                 (spaces-end (let scan ((j flag-end))
                               (if (and (< j len) (char=? (string-ref line j) #\space)) (scan (+ j 1)) j))))
            (loop after
                  (if (and (or (= k 0) (not (word-char? (string-ref line (- k 1)))))
                           (or spliced
                               (perl-send-at? line after)
                               (and (not (string=? flag " -0"))
                                    (> spaces-end flag-end)
                                    (< spaces-end len)
                                    (let ((c (string-ref line spaces-end)))
                                      (or (memv c '(#\" #\$)) (char-numeric? c) (char-lower-case? c))))))
                      (+ n 1)
                      n)))))))

;; CONTROL: made-up lines in three spellings, a signal written out, a signal
;; spliced in, and `kill -l` (lists signal names, sends nothing). Red on a
;; rule that reads only a written-out signal: the spliced line reads 0.
(want "SA-3 CONTROL the rule reads a written-out signal and a spliced one as bare sends, and kill -l as none"
      (map bare-sends-in
           (list "(sh \"kill -USR1 $pid 2>/dev/null\")"
                 "(system (string-append \"kill -\" name \" \" (number->string pid)))"
                 "(sh \"kill -l USR1 > \" out \" 2>/dev/null\")"))
      '(1 1 0))

;; CONTROL, perl: the two spellings perl sends with, each read once, and the
;; same send on a comment line, which code-lines drops. Red on a rule that
;; reads only the shell's spelling (0), or only the first of perl's (1).
(want "SA-3 CONTROL perl's `kill 'NAME', target` and `kill NAME => target` are bare sends, a comment line is none"
      (apply + (map bare-sends-in
                    (filter (lambda (l) (not (comment-line? l)))
                            '("    kill 'TERM', -$g;" "  kill TERM => $pid;" "  # kill 'KILL', -$g;"))))
      2)
;; CONTROL, perl's other spellings: the signal's name double-quoted, and the
;; call in parentheses with either quote. Red on a rule that reads only the
;; single-quoted name after a space (1: the shell's rule reads the `"`).
(want "SA-3 CONTROL perl's `kill \"NAME\", target` and `kill('NAME', target)` are bare sends"
      (map bare-sends-in '("    kill \"TERM\", -$g;" "  kill('KILL', $pid);" "  kill( \"INT\", -$g );" "  kill ('TERM', $pid);"))
      '(1 1 1 1))

;; THE SENDS LEFT, NAMED. Each: census row, why it stays, the text that marks
;; its line, and how many lines carry that text. A send added to one of
;; these files, or one of these moved onto the check without its entry
;; being struck here, reds the file's row. Keyed by text, not line number,
;; so a line may move.
(define remainders
  '(("log15.sc")
    ("launch-self.sc"
     (95 "SELF: a command signalling itself" "sh -c 'kill -TERM $$'" 1))
    ("runner-self.sc")
    ("retire-race.sc")
    ("cli2.sc")
    ("f0-ondemand.sc")
    ("change-stream-client.sc")
    ("entry-split.sc")
    ("read-at-cut.sc")
    ("unreadable-acknowledge-routes.sc")
    ("unreadable-routes.sc")
    ("append-prefix.sc")
    ("change-stream.sc")
    ("cli1.sc")
    ("client-socket.sc")
    ("daemon-e1.sc")
    ("daemon-socket.sc")
    ("detach.sc")
    ("eval-lang.sc"
     (73 "SELF: the evaluated shell program signalling itself" "\"kill -TERM $$\\n\"" 1))
    ("f3-wire-utf8.sc")
    ("facade-proc.sc"
     (85 "children: child-send!, end-and-wait!'s TERM to the children listed while their parent is held stopped, confirmed through its sender (row 85)" "(string-append \"kill -\" sig" 1))
    ("fold-failure.sc")
    ("liveness.sc")
    ("liveness-streams.sc")
    ("mcp-shell.sc")
    ("opener-route.sc")
    ("ownership.sc")
    ("smoke-shortwrite-nb.sc")
    ("startup-ratio.sc")
    ("store-in-git.sc")
    ("launch.pl"
     (99 "own child, not reaped: the pid is held (census_run's ps child, rows 99 and 100)" "kill 'KILL', -$pid" 2)
     (101 "G's leader held: sent before the reap (stop_group's TERM)" "kill 'TERM', -$g" 1)
     (102 "G's leader held: sent before the reap (stop_group's KILL)" "kill 'KILL', -$g" 1))
    ("run-fixtures.sh")))

;; The checked sends that must be there, per file, and the lookups by ps that
;; feed them: each the text of the form and the least number of lines that
;; carry it. In generated shell text the check is inline: cli1's holder
;; instant (started-text) compared on the kill's line (`ps -o lstart=`), and
;; the self-signalling loops' text from signal-loop-text (test/signal-send.ss).
(define converted
  '(("log15.sc" ("(signal! " 4))
    ("launch-self.sc" ("(signal! " 2) ("(started! " 4))
    ("runner-self.sc" ("(signal! " 7) ("(pid-registered! " 24))
    ("retire-race.sc" ("trap 'kill9_if_same " 2))
    ("cli2.sc" ("(signal! " 1))
    ("f0-ondemand.sc" ("(signal! " 3) ("(command-processes " 2) ("(started! " 2))
    ("change-stream-client.sc" ("(serve-processes " 1) ("(command-processes" 1) ("(signal! " 2) ("(started! " 1))
    ("entry-split.sc" ("(serve-processes " 3) ("(signal! " 2) ("(signal-processes! " 1))
    ("read-at-cut.sc" ("(serve-processes " 2) ("(signal-processes! " 2))
    ("unreadable-acknowledge-routes.sc" ("(serve-processes " 2) ("(signal-processes! " 2))
    ("unreadable-routes.sc" ("(serve-processes " 5) ("(signal-processes! " 5) ("(signal! " 2) ("(started! " 2))
    ("append-prefix.sc" ("(signal! " 2) ("(started! " 2))
    ("change-stream.sc" ("(signal! " 2) ("(started! " 1))
    ("cli1.sc" ("ps -o lstart=" 3) ("(started-text " 5))
    ("client-socket.sc" ("(signal! " 1) ("(started! " 1))
    ("daemon-e1.sc" ("(signal! " 1) ("(started! " 1))
    ("daemon-socket.sc" ("(signal! " 2) ("(started! " 2))
    ("detach.sc" ("(signal! " 2) ("(started! " 2))
    ("eval-lang.sc" ("(signal! " 2) ("(started! " 2))
    ("f3-wire-utf8.sc" ("(signal! " 1) ("(started! " 1))
    ("facade-proc.sc" ("(signal! " 1) ("(started! " 1) ("(end-and-wait! (list pid) checked-send!)" 1)
                      ("(worker-kill! " 3))
    ("fold-failure.sc" ("(signal! " 2) ("(started! " 1))
    ("liveness.sc" ("(signal-loop-text " 1) ("(signal! " 2) ("(started! " 2))
    ("liveness-streams.sc" ("(signal-loop-text " 1) ("(signal! " 1) ("(started! " 1))
    ("mcp-shell.sc" ("(signal! " 1) ("(started! " 2))
    ("opener-route.sc" ("(signal! " 1) ("(started! " 1))
    ("ownership.sc" ("(signal! " 1) ("(started! " 1))
    ("smoke-shortwrite-nb.sc" ("(signal! " 1) ("(started! " 1))
    ("startup-ratio.sc" ("(signal! " 1) ("(for-each started! " 1))
    ("store-in-git.sc" ("(serve-processes " 2) ("(signal-processes! " 2))
    ("launch.pl")
    ("run-fixtures.sh" ("kill_if_same " 7) ("bps -o lstart=" 3))))

(printf "SA-3 covers ~a files\n" (length remainders))

(define (bare-lines f)
  (fold-right (lambda (l acc)
                (let ((n (if (has? (cdr l) "ps -o lstart=") 0 (bare-sends-in (cdr l)))))
                  (if (> n 0) (cons (list (car l) n (trim (cdr l))) acc) acc)))
              '() (code-lines f)))

;; -> (unnamed . miscounted): the bare lines no entry names, and each entry
;; whose text is on a number of bare lines other than its own.
(define (remainder-verdict f)
  (let ((hits (bare-lines f)) (entries (cdr (assoc f remainders))))
    (for-each (lambda (h)
                (let ((e (find (lambda (e) (has? (caddr h) (caddr e))) entries)))
                  (printf "SA-3 hit ~a:~a ~a\n" f (car h)
                          (if e (format "census row ~a, ~a" (car e) (cadr e)) "NOT NAMED"))))
              hits)
    (cons (map car (filter (lambda (h) (not (exists (lambda (e) (has? (caddr h) (caddr e))) entries))) hits))
          (fold-right (lambda (e acc)
                        (let ((got (length (filter (lambda (h) (has? (caddr h) (caddr e))) hits))))
                          (if (= got (cadddr e)) acc (cons (list (car e) got (cadddr e)) acc))))
                      '() entries))))

(for-each
  (lambda (entry)
    (let ((f (car entry)))
      (want (format "SA-3 ~a: its bare sends are exactly the named ones (~a)" f (length (cdr entry)))
            (remainder-verdict f)
            '(()))
      (let ((forms (cdr (assoc f converted))))
        (want (format "SA-3 ~a: the checked sends are there (~a)" f
                      (fold-left (lambda (acc c)
                                   (string-append acc (if (string=? acc "") "" " and ")
                                                  (format "~s on at least ~a lines" (car c) (cadr c))))
                                 "" forms))
              (for-all (lambda (c) (>= (length (filter (lambda (l) (has? (cdr l) (car c))) (code-lines f))) (cadr c)))
                       forms)
              #t))))
  remainders)

;; ---- SA-5: a removal glob ends where this run's name does ------------------------
;;
;; A GLOB WITH `*` RIGHT AFTER THE RUN'S PID (`" pid-text "*`) matches every
;; name the pid's digits begin, so it also removes the files of a run whose
;; pid begins with them, and with a `*` before the pid as well, of a run whose
;; pid holds them anywhere. The names that end in the pid are SA-1's, with
;; (number->string pid) beside them. Every code line of test/*.sc is read, not
;; only one holding the `rm`, since a removal's operands run on over lines.
(define (glob-after-pid? name names)
  (or (string=? name pid-expr) (string=? name "(number->string pid)") (and (member name names) #t)))
(define (spliced-before-glob line)
  (let loop ((i 0) (acc '()))
    (let ((e (index-of line " \"*" i)))
      (if (not e)
          (reverse acc)
          (let ((s (let back ((j (- e 1)))
                     (cond ((< j 1) #f)
                           ((and (char=? (string-ref line j) #\space) (char=? (string-ref line (- j 1)) #\")) (+ j 1))
                           (else (back (- j 1)))))))
            (loop (+ e 1) (if s (cons (substring line s e) acc) acc)))))))
(define sa5-hits
  (apply append
    (map (lambda (f)
           (let* ((ls (code-lines f)) (names (pid-ending-names ls)))
             (fold-right
               (lambda (l acc)
                 (append (map (lambda (n) (list f (car l)))
                              (filter (lambda (n) (glob-after-pid? n names)) (spliced-before-glob (cdr l))))
                         acc))
               '() ls)))
         (filter (lambda (f) (ends-with? f ".sc")) scanned))))
(for-each (lambda (h) (printf "SA-5 hit ~a:~a\n" (car h) (cadr h))) sa5-hits)
(want "SA-5 no removal glob has `*` right after a splice of the run's pid"
      sa5-hits
      '())

;; ---- SA-4: signal! itself ----------------------------------------------------------
;;
;; Each process is started in the background by a shell that then exits, so
;; it is no child of this file's: once it ends nothing here holds it as a
;; zombie that would still answer kill -0.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/sigaudit-" (number->string (get-process-id))))
(system (string-append "rm -rf " here "; mkdir -p " here))
(define spawn-n 0)
(define (spawn-sleep secs)
  (set! spawn-n (+ spawn-n 1))
  (let ((pidf (string-append here "/s" (number->string spawn-n) ".pid")))
    (system (format "sleep ~a < /dev/null > /dev/null 2>&1 & echo $! > ~a" secs pidf))
    (pid-of-file pidf)))
;; (spawn-held go) -> the pid of a shell that ends once the file GO exists.
;; NEVER: A CHILD THAT ENDS ON A TIMER. The ended-process row must register
;; the process while it runs; a child that sleeps a fraction of a second can
;; end under load before ps has read its start, and the row would then read a
;; correct helper as red.
(define (spawn-held go)
  (set! spawn-n (+ spawn-n 1))
  (let ((pidf (string-append here "/s" (number->string spawn-n) ".pid")))
    (system (format "sh -c 'while [ ! -e ~a ]; do sleep 0.05; done' < /dev/null > /dev/null 2>&1 & echo $! > ~a"
                    go pidf))
    (pid-of-file pidf)))
(define (gone-within? pid seconds)
  (let loop ((i 0))
    (cond ((not (alive? pid)) #t)
          ((>= i (* seconds 20)) #f)
          (else (system "sleep 0.05") (loop (+ i 1))))))

(let* ((go (string-append here "/go"))
       (pid (started! (spawn-held go)))
       (recorded (let ((k (assv pid identities))) (and k (string? (cdr k)))))
       (ended (begin (system (string-append "touch " go)) (gone-within? pid 5))))
  (want "SA-4 a registered process that has ended is not signalled: already-gone"
        (list recorded ended (signal! pid "TERM"))
        '(#t #t already-gone)))

(let* ((pid (started! (spawn-sleep "30")))
       (sent (signal! pid "TERM")))
  (want "SA-4 a registered process still running is signalled, and ends"
        (list sent (gone-within? pid 5))
        '(sent #t)))

(let* ((pid (spawn-sleep "30"))
       (unregistered (signal! pid "TERM"))
       (still (alive? pid)))
  (started! pid)
  (signal! pid "TERM")
  (want "SA-4 a pid never registered is not signalled, and its process is still there"
        (list unregistered still (gone-within? pid 5))
        '(already-gone #t #t)))

;; THE IDENTITY COMPARISON HAS A ROW OF ITS OWN: while the instant recorded
;; for a live process differs from the one ps reads, nothing is sent and the
;; process is still there; under its true instant it is signalled and goes.
;; Red on a signal! that checks only kill -0.
(let* ((pid (started! (spawn-sleep "30")))
       (true-id (let ((k (assv pid identities))) (and k (cdr k)))))
  (set! identities (cons (cons pid (string-append (or true-id "") " (altered)")) identities))
  (let* ((wrong (signal! pid "TERM"))
         (still (alive? pid)))
    (set! identities (cdr identities))
    (let* ((right (signal! pid "TERM"))
           (gone (gone-within? pid 5)))
      (want "SA-4 a pid whose recorded start differs is not signalled; under its true start it is"
            (list (string? true-id) wrong still right gone)
            '(#t already-gone #t sent #t)))))

;; ---- SA-6: the lookup by store -------------------------------------------------------
;;
;; serves-store? takes the word after `serve`, or after `serve --store`, as
;; the store, so a store whose path has the asked one's as a prefix is not
;; matched. Red on a lookup that asks whether the line holds `serve <store>`.
(want "SA-6 serves-store? reads exactly the store after serve or serve --store"
      (map (lambda (line) (serves-store? line "/x/s"))
           '("123 scheme --script /p/theourgiad.sc serve /x/s"
             "123 scheme --script /p/theourgiad.sc serve /x/s --wire"
             "123 scheme --script /p/theourgiad.sc serve --store /x/s --socket /r/d.sock"
             "123 scheme --program /p/theourgiad.so serve /x/s --socket /r/d.sock"
             "123 scheme --script /p/theourgiad.sc serve /x/s1"
             "123 scheme --script /p/theourgiad.sc serve /x/s/"
             "123 scheme --script /p/theourgiad.sc serve --store /x/s1"
             "123 scheme --script /p/core.sc outline --store /x/s"))
      '(#t #t #t #t #f #f #f #f))

;; A REAL PROCESS WHOSE COMMAND LINE CARRIES `serve <here>/s1`, started by a
;; shell that then exits (perl: macOS sleep refuses operands it does not
;; read): asked for <here>/s, the lookup finds nothing; asked for <here>/s1,
;; it finds that pid, with the instant ps -o lstart= -p prints for it, and
;; signal-processes! TERM ends it.
(let* ((s1 (string-append here "/s1"))
       (pidf (string-append here "/serve-s1.pid"))
       (pid (begin (system (format "perl -e 'sleep 30' serve ~a < /dev/null > /dev/null 2>&1 & echo $! > ~a" s1 pidf))
                   (pid-of-file pidf)))
       (shorter (serve-processes (string-append here "/s")))
       (found (serve-processes s1))
       (instant (pid-identity pid))
       (sent (signal-processes! found "TERM")))
  (want "SA-6 a live serve of <here>/s1 is not found for <here>/s; it is for <here>/s1, with its instant, and TERM ends it"
        (list shorter (map car found) (and (pair? found) (equal? (cdar found) instant)) sent (gone-within? pid 5))
        (list '() (list pid) #t '(sent) #t)))

;; ---- SA-7: the self-signalling loop, with no identity read -------------------------
;;
;; A program counts USR1 and runs signal-loop-text's form (four rounds, 0.2 s
;; apart), once with PATH shimmed so that `ps` prints nothing and once with the
;; real ps; the shim is the only difference. With no instant captured the loop
;; must send nothing: on a loop that accepted an empty capture, the shimmed ps
;; reads empty twice, the two compare equal, and the program is signalled.
;; THE ROW IS ABOUT THE EMPTY CAPTURE ONLY: an empty reading in a later round,
;; after a real capture, already differs from it and ends the loop.
(define (command-text cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p))
    (close-port (cadr p))
    (if (eof-object? t) "" t)))
(define usr1 (string->number (signal-send-trim (command-text "kill -l USR1"))))
(define (loop-hits shim?)
  (let ((prog (string-append here "/loop-prog.ss"))
        (out (string-append here (if shim? "/loop-shim.out" "/loop-real.out")))
        (shim (string-append here "/shim")))
    (system (string-append "mkdir -p " shim))
    (call-with-output-file (string-append shim "/ps")
      (lambda (p) (put-string p "#!/bin/sh\nexit 0\n")) 'truncate)
    (system (string-append "chmod +x " shim "/ps"))
    (call-with-output-file prog
      (lambda (p)
        (put-string p (string-append
          "(import (chezscheme))\n(define hits 0)\n"
          (format "(register-signal-handler ~a (lambda (s) (set! hits (+ hits 1))))\n" usr1)
          (signal-loop-text "USR1" 4 0.2) "\n"
          "(let loop ((k 0)) (when (< k 20) (sleep (make-time 'time-duration 100000000 0)) (loop (+ k 1))))\n"
          "(write hits)\n")))
      'truncate)
    (system (string-append (if shim? (string-append "PATH=" shim ":$PATH ") "")
                           "scheme --script " prog " > " out " 2>&1 < /dev/null"))
    (guard (e (#t (list 'unreadable (command-text (string-append "cat " out)))))
      (call-with-input-file out read))))
(let ((shimmed (loop-hits #t)) (real (loop-hits #f)))
  (printf "SA-7 USR1 is ~a; hits with no instant ~s, with the real ps ~s\n" usr1 shimmed real)
  (want "SA-7 the self-signalling loop sends nothing when ps reads no start instant, and signals the program with the real ps"
        (list shimmed (and (integer? real) (>= real 1)))
        '(0 #t)))

;; ---- SA-8: the runner's own sends, run ------------------------------------------
;;
;; The runner's functions are cut from run-fixtures.sh between its `SA-8 CUT`
;; marker lines, as SA-4 includes signal-send.ss, and run in sh against real
;; processes, so what is read is the runner's own text. A region that is not
;; there reads #f, and every row that needs it is red.
(define runner-text (call-with-input-file "run-fixtures.sh" get-string-all))
(define (runner-cut name)
  (let* ((b (string-append "# ---- SA-8 CUT " name " BEGIN"))
         (e (string-append "# ---- SA-8 CUT " name " END ----"))
         (i (index-of runner-text b 0))
         (j (and i (index-of runner-text e i))))
    (and i j (substring runner-text i j))))
(define (text-lines text)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length text)) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
          (else (loop (+ i 1) start acc)))))
(define cut-helpers (runner-cut "helpers"))
(define cut-stop-probe (runner-cut "stop_probe"))
(define cut-on-signal (runner-cut "on_signal"))
(define cut-count-leaks (runner-cut "count_leaks"))
(define cut-probe (runner-cut "probe"))
(printf "SA-8 cuts: helpers ~a, stop_probe ~a, on_signal ~a, count_leaks ~a, probe ~a\n"
        (and cut-helpers #t) (and cut-stop-probe #t) (and cut-on-signal #t)
        (and cut-count-leaks #t) (and cut-probe #t))
;; (sh-script name text ...) writes the script; (sh-out name) reads what it
;; printed, stdout and stderr together.
(define (sh-script name . parts)
  (call-with-output-file (string-append here "/" name ".sh")
    (lambda (p) (for-each (lambda (s) (put-string p s) (newline p)) parts))
    'truncate))
(define (sh-run-all names)
  (system (apply string-append
                 (append (map (lambda (n) (format "sh ~a/~a.sh > ~a/~a.out 2>&1 < /dev/null & " here n here n)) names)
                         '("wait")))))
(define (sh-out name) (command-text (string-append "cat " here "/" name ".out")))
(define (result-of text)
  (let ((i (index-of text "RESULT " 0)))
    (and i (guard (e (#t #f)) (read (open-input-string (substring text (+ i 7) (string-length text))))))))
;; Every pid a script names on a `PID <n> <instant>` line is ended here if it
;; is still that process, through signal!: a row whose send was refused
;; leaves its sleeper. THE INSTANT IS THE SCRIPT'S, read where it acquired
;; the pid (signal-send.ss: register where the process starts), and the pair
;; is registered with started-at!; a pid read here, minutes later, and
;; registered now would pass signal!'s check for whatever holds it. A line
;; with no instant registers nothing, and signal! sends nothing.
(define (end-named-pids text)
  (for-each
    (lambda (line)
      (when (and (>= (string-length line) 4) (string=? (substring line 0 4) "PID "))
        (let* ((in (open-input-string (substring line 4 (string-length line))))
               (n (guard (e (#t #f)) (read in)))
               (instant (and (integer? n) (signal-send-trim (get-string-all-or-empty in)))))
          (when (integer? n)
            (started-at! n instant)
            (when (alive? n) (signal! n "KILL"))))))
    (text-lines text)))
(define (get-string-all-or-empty in)
  (let ((t (get-string-all in))) (if (eof-object? t) "" t)))

;; SA-8a kill_if_same, on a real `sleep 30`: a wrong instant (the recorded
;; text's first character altered), a wrong command (its last word altered)
;; and an empty identity are not signalled; a child that exited before its
;; record has an empty identity and is not signalled; the recorded identity
;; is, and the sleeper goes. Red on the base: no function.
(when cut-helpers
  (sh-script "sa8a" cut-helpers
    "sleep 30 > /dev/null 2>&1 &"
    "p=$!"
    "echo \"PID $p $(ps -o lstart= -p \"$p\")\""
    "id=$(bps -o lstart= -o command= -p \"$p\")"
    "st() { if alive \"$1\"; then echo alive; else echo gone; fi; }"
    "kill_if_same \"$p\" \"X${id#?}\" TERM; sleep 0.3; r1=$(st \"$p\")"
    "kill_if_same \"$p\" \"${id%30}31\" TERM; sleep 0.3; r2=$(st \"$p\")"
    "kill_if_same \"$p\" \"\" TERM; sleep 0.3; r3=$(st \"$p\")"
    "true & q=$!; wait \"$q\"; qid=$(bps -o lstart= -o command= -p \"$q\"); kill_if_same \"$q\" \"$qid\" TERM; r5=$?"
    "kill_if_same \"$p\" \"$id\" TERM; s4=$?"
    "n=0; while alive \"$p\" && [ \"$n\" -lt 100 ]; do sleep 0.05; n=$((n + 1)); done; r4=$(st \"$p\")"
    "case \"$id\" in *'sleep 30') i=read ;; *) i=unread ;; esac"
    "case \"$qid\" in '') qi=empty ;; *) qi=read ;; esac"
    "echo \"RESULT ($i $r1 $r2 $r3 $qi $r5 $s4 $r4)\""))
;; SA-8b the first record validated: a record whose command lacks the marker
;; is dropped and said, and nothing is sent; with the marker it is kept, and
;; the send goes.
(when cut-helpers
  (sh-script "sa8b" cut-helpers
    "sleep 30 > /dev/null 2>&1 &"
    "p=$!"
    "echo \"PID $p $(ps -o lstart= -p \"$p\")\""
    "record_identity bad \"$p\" \"NOT-ITS-COMMAND\" \"the sleeper\""
    "kill_if_same \"$p\" \"$bad\" TERM; sleep 0.3"
    "if alive \"$p\"; then a1=alive; else a1=gone; fi"
    "record_identity good \"$p\" \"sleep 30\" \"the sleeper\""
    "kill_if_same \"$p\" \"$good\" TERM"
    "n=0; while alive \"$p\" && [ \"$n\" -lt 100 ]; do sleep 0.05; n=$((n + 1)); done"
    "if alive \"$p\"; then a2=alive; else a2=gone; fi"
    "case \"$bad\" in '') b=empty ;; *) b=set ;; esac"
    "case \"$good\" in '') g=empty ;; *) g=set ;; esac"
    "echo \"RESULT ($b $a1 $g $a2)\""))

;; SA-8c THE PROBE HANDOFF: the subshell and stop_probe, cut, run with a bps
;; that sleeps 3 s on every call, and a child that ignores TERM, so only
;; stop_probe's KILL ends it. The child is the cut's own `scheme --script
;; <run_root>/.probe.ss &` with `scheme` a function that execs a perl ignoring
;; TERM with those arguments, so its command carries the marker the record
;; validates (a sh that execs sleep would show `sleep 30`, without it).
;; TRAP: TERM to the parent; its trap sends one TERM and exits, so the parent
;; is gone within 7 s of its TERM (the TERM goes after one bps, 3 s: within
;; 10 s of stop_probe's call), and stop_probe then finishes the child. KILLED:
;; the parent is KILLed once .probe-id exists, so the trap never runs, and
;; stop_probe finishes the child by its identity. UNPUBLISHED: .probe-id is
;; removed before stop_probe, which answers 1 and says the child was not owned.
(define (probe-script name before-stop)
  (sh-script name cut-helpers cut-stop-probe
    "bps() { sleep 3; perl -e 'alarm shift; exec { \"ps\" } \"ps\", @ARGV; exit 127' 5 \"$@\"; }"
    "scheme() { exec perl -e '$SIG{TERM} = \"IGNORE\"; sleep 30' -- \"$@\"; }"
    (format "run_root=~a/~a.root; mkdir -p \"$run_root\"" here name)
    "runner_command=$(ps -o command= -p $$)"
    cut-probe
    "n=0; while [ ! -s \"$run_root/.probe-id\" ] && [ \"$n\" -lt 400 ]; do sleep 0.05; n=$((n + 1)); done"
    "if [ -s \"$run_root/.probe-id\" ]; then idw=written; else idw=missing; fi"
    "child=$(cat \"$run_root/.probe-pid\" 2>/dev/null); echo \"PID $child $(ps -o lstart= -p \"$child\")\""
    "pp=$probe_parent"
    "gone_ps() { st=$(ps -o stat= -p \"$1\" 2>/dev/null); case \"$st\" in ''|Z*) return 0 ;; esac; return 1; }"
    (format "( s=$(mono); while ! gone_ps \"$pp\"; do sleep 0.05; done; echo $(($(mono) - s)) > \"$run_root/parent-ms\" ) &")
    "poller=$!"
    before-stop
    "s=$(mono); stop_probe; rc=$?; took=$(($(mono) - s))"
    "wait \"$poller\""
    "if gone_ps \"$child\"; then cg=gone; else cg=alive; fi"
    "echo \"RESULT ($idw $rc $(cat \"$run_root/parent-ms\") $cg $took)\""))
;; SA-8e THE CLEANUP SENDS ONLY TO THE PROCESS A SCRIPT ACQUIRED: a script
;; prints a PID line with a wrong instant for a live sleeper of its own. The
;; cleanup must send nothing to it (alive after); the script then ends it
;; itself, by its own record, once the row's go file exists. The sleeper and
;; the script's wait are 300 s: the cleanup runs after the slowest script, and
;; a sleeper that ended by itself before it would read as a send.
(when cut-helpers
  (sh-script "sa8e" cut-helpers
    "sleep 300 > /dev/null 2>&1 &"
    "p=$!"
    "id=$(bps -o lstart= -o command= -p \"$p\")"
    "echo \"PID $p Mon Jan  1 00:00:00 2001\""
    "echo \"SLEEPER $p\""
    (format "( n=0; while [ ! -e ~a/sa8e.go ] && [ \"$n\" -lt 6000 ]; do sleep 0.05; n=$((n + 1)); done; kill_if_same \"$p\" \"$id\" TERM ) > /dev/null 2>&1 < /dev/null &" here)
    "echo \"RESULT (started)\""))
;; SA-8f record_identity's 1 s IS A DEADLINE: under a ps on PATH that never
;; answers (each call execs a 30 s sleep), the record is dropped and said within
;; 1 s plus one read. Red on a loop that counts reads under bps's 5 s alarm
;; (20 reads, about 100 s).
(when cut-helpers
  (sh-script "sa8f" cut-helpers
    "sleep 30 > /dev/null 2>&1 &"
    "p=$!"
    "echo \"PID $p $(ps -o lstart= -p \"$p\")\""
    "id=$(bps -o lstart= -o command= -p \"$p\")"
    (format "mkdir -p ~a/sa8f-shim; printf '#!/bin/sh\\nexec sleep 30\\n' > ~a/sa8f-shim/ps; chmod +x ~a/sa8f-shim/ps" here here here)
    "s=$(mono)"
    (format "said=$(PATH=~a/sa8f-shim:$PATH; record_identity rec \"$p\" \"sleep 30\" \"the sleeper\"; printf 'REC[%s]' \"$rec\")" here)
    "took=$(($(mono) - s))"
    "echo \"$said\""
    "kill_if_same \"$p\" \"$id\" TERM"
    "echo \"RESULT ($took)\""))
(define probe-cuts (and cut-helpers cut-stop-probe cut-probe #t))
(when probe-cuts
  (probe-script "sa8c-trap" ":")
  (probe-script "sa8c-killed" "kill -KILL \"$pp\"")
  (probe-script "sa8c-unpublished" "rm -f \"$run_root/.probe-id\""))

;; SA-8d count_leaks, cut, WITH THE ACTUAL POST-STOP FLAGS. snapshot and
;; carrying_token are shims: one that finds this script's own pid as a
;; token carrier, one that finds none, one whose snapshot fails. UNOWNED:
;; probe_done=1, probe_unowned=1, env_visible=0, no baseline. NO-BASELINE:
;; probe_done=1, probe_unowned=0, env_visible=0, no baseline, called
;; directly and through on_signal (its stop_probe a stub that answers 0).
;; BEFORE (the v1.8 note): probe_done=0, as on an early refusal: the count by
;; token stands, with no verdict. Each run is asserted to print its
;; evidence, the root count, its verdict and leaked=1 (BEFORE: no verdict,
;; leaked=0), and never the by-name line or a shell numeric diagnostic.
(define (leak-script name snap flags via-signal?)
  (apply sh-script name
    (append
      (list cut-helpers cut-count-leaks)
      (if via-signal? (list cut-on-signal "stop_probe() { return 0; }") '())
      (list
        "count_run_root() { echo 0; }"
        "run_root_before=0; real_run_root=/nonexistent; token=TOKEN; snap_dir=/nonexistent"
        "echo \"SELF $$\""
        (case snap
          ((found) "snapshot() { return 0; }; carrying_token() { echo \"$$\"; }")
          ((none) "snapshot() { return 0; }; carrying_token() { return 0; }")
          (else "snapshot() { return 1; }; carrying_token() { return 0; }"))
        flags
        "env_visible=0; unset schemes_before")
      (if via-signal?
          (list "tracked_w=''; probe_parent=1; last_ran=x"
                "trap 'echo \"leaked=$leaked\"' EXIT"
                "on_signal 143")
          (list "count_leaks" "echo \"leaked=$leaked\"")))))
(define leak-cases
  ;; (name snapshot flags via-signal? verdict-text leaked)
  `(("sa8d-unowned-found" found "probe_done=1; probe_unowned=1" #f "LEAK COUNT NOT TAKEN: the probe child was not owned at its end" "leaked=1")
    ("sa8d-unowned-none" none "probe_done=1; probe_unowned=1" #f "LEAK COUNT NOT TAKEN: the probe child was not owned at its end" "leaked=1")
    ("sa8d-nobase-found" found "probe_done=1; probe_unowned=0" #f "LEAK COUNT NOT TAKEN: the by-name count has no baseline" "leaked=1")
    ("sa8d-nobase-none" none "probe_done=1; probe_unowned=0" #f "LEAK COUNT NOT TAKEN: the by-name count has no baseline" "leaked=1")
    ("sa8d-nobase-failed" failed "probe_done=1; probe_unowned=0" #f "LEAK COUNT NOT TAKEN: the by-name count has no baseline" "leaked=1")
    ("sa8d-signal-found" found "probe_done=0; probe_unowned=0" #t "LEAK COUNT NOT TAKEN: the by-name count has no baseline" "leaked=1")
    ("sa8d-signal-none" none "probe_done=0; probe_unowned=0" #t "LEAK COUNT NOT TAKEN: the by-name count has no baseline" "leaked=1")
    ("sa8d-signal-failed" failed "probe_done=0; probe_unowned=0" #t "LEAK COUNT NOT TAKEN: the by-name count has no baseline" "leaked=1")
    ("sa8d-before-none" none "probe_done=0; probe_unowned=0" #f #f "leaked=0")))
(define leak-cuts (and cut-helpers cut-count-leaks cut-on-signal #t))
(when leak-cuts
  (for-each (lambda (c) (leak-script (car c) (cadr c) (caddr c) (cadddr c))) leak-cases))

(sh-run-all (append (if cut-helpers '("sa8a" "sa8b" "sa8e" "sa8f") '())
                    (if probe-cuts '("sa8c-trap" "sa8c-killed" "sa8c-unpublished") '())
                    (if leak-cuts (map car leak-cases) '())))
(for-each (lambda (n) (end-named-pids (sh-out n)))
          '("sa8a" "sa8b" "sa8c-trap" "sa8c-killed" "sa8c-unpublished" "sa8e" "sa8f"))
(let* ((out (if cut-helpers (sh-out "sa8e") ""))
       (k (index-of out "SLEEPER " 0))
       (p (and k (guard (e (#t #f)) (read (open-input-string (substring out (+ k 8) (string-length out)))))))
       (alive-after (and (integer? p) (alive? p)))
       (gone (and (integer? p)
                  (begin (system (string-append "touch " here "/sa8e.go")) (gone-within? p 5)))))
  (printf "SA-8e sleeper ~a alive after the cleanup ~a, gone after go ~a\n" p alive-after gone)
  (want "SA-8e the cleanup sends nothing to a pid whose printed instant is not its own; the script's own send ends it"
        (list (integer? p) alive-after gone)
        '(#t #t #t)))
(let* ((out (if cut-helpers (sh-out "sa8f") ""))
       (took (result-of out)))
  (printf "SA-8f said ~s\n" out)
  (want "SA-8f under a ps that never answers, record_identity drops the record and says so within 1 s plus one read"
        (list (has? out "REC[]") (has? out "the sleeper: start identity not read")
              (and (pair? took) (integer? (car took)) (< (car took) 2500)))
        '(#t #t #t)))

(let ((a (and cut-helpers (result-of (sh-out "sa8a")))))
  (printf "SA-8a read ~s\n" a)
  (want "SA-8a kill_if_same: a wrong instant, a wrong command, an empty identity and an exited child are not signalled; the recorded identity is"
        a
        '(read alive alive alive empty 1 0 gone)))
(let ((send-lines (if cut-helpers
                      (filter (lambda (l) (has? l "kill \"-$3\"")) (text-lines cut-helpers))
                      '())))
  (want "SA-8a the send and the identity comparison are one line"
        (list (length send-lines) (for-all (lambda (l) (has? l "bps -o lstart=")) send-lines))
        '(1 #t)))
(let* ((out (if cut-helpers (sh-out "sa8b") ""))
       (b (result-of out)))
  (printf "SA-8b read ~s\n" b)
  (want "SA-8b a first record without the marker is dropped and said, and not signalled; with it, kept and signalled"
        (list b (has? out "the sleeper: start identity not read (pid ") (has? out "shows another command), not signalled"))
        '((empty alive set gone) #t #t)))
(let* ((outs (map (lambda (n) (if probe-cuts (sh-out n) "")) '("sa8c-trap" "sa8c-killed" "sa8c-unpublished")))
       (rs (map result-of outs)))
  (printf "SA-8c read ~s\n" rs)
  (want "SA-8c TRAP the parent is gone within 7 s of its TERM, the child after, .probe-id written before the wait"
        (let ((r (car rs)))
          (and (list? r) (= (length r) 5)
               (list (car r) (cadr r) (and (integer? (caddr r)) (< (caddr r) 10000)) (cadddr r))))
        '(written 0 #t gone))
  (want "SA-8c KILLED the parent KILLed before its trap ran, stop_probe finishes the child by its identity"
        (let ((r (cadr rs)))
          (and (list? r) (= (length r) 5) (list (car r) (cadr r) (cadddr r))))
        '(written 0 gone))
  (want "SA-8c UNPUBLISHED no .probe-id: stop_probe answers 1 and says the child was not owned"
        (let ((r (caddr rs)))
          (and (list? r) (= (length r) 5)
               (list (car r) (cadr r) (has? (caddr outs) "probe child ") (has? (caddr outs) ": not owned at the end, left to end by itself"))))
        '(written 1 #t #t)))
(for-each
  (lambda (c)
    (let* ((name (car c)) (snap (cadr c)) (verdict (list-ref c 4)) (leaked (list-ref c 5))
           (out (if leak-cuts (sh-out name) ""))
           (self (let ((k (index-of out "SELF " 0)))
                   (and k (guard (e (#t #f)) (read (open-input-string (substring out (+ k 5) (string-length out))))))))
           (evidence (case snap
                       ((found) (and self (has? out (format "LEAKED-PROCESSES: ~a -- " self))
                                     (has? out (format "\n  ~a " self))))
                       ((none) (has? out "processes: none carrying this run's token TOKEN is left running"))
                       (else (has? out "LEAK COUNT NOT TAKEN: the process snapshots under")))))
      (printf "SA-8d ~a said ~s\n" name out)
      (want (format "SA-8d ~a: the evidence, the root count, ~a, ~a, no by-name line, no numeric diagnostic"
                    name (if verdict "the verdict" "no verdict") leaked)
            (list evidence
                  (has? out "real run root: 0 before, 0 after")
                  (if verdict (has? out verdict) (not (has? out "LEAK COUNT NOT TAKEN")))
                  (has? out leaked)
                  (has? out "scheme processes before")
                  (has? out "integer expression expected"))
            '(#t #t #t #t #f #f))))
  leak-cases)

(system (string-append "rm -rf " here))

(printf "rows: ~a\n~a failures\nsignal-audit complete\n" rows bad)
(exit (if (zero? bad) 0 1))
