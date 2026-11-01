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
;;       checked sends are there.
;; SA-4  signal! itself (test/signal-send.ss): a registered process that has
;;       ended is not signalled, a live one is, an unregistered one is not,
;;       and one whose recorded start differs is not.
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
;; then a pid, a `$` or a splice. A line holding `ps -o lstart=` is the inline
;; check of generated shell text, not a bare send.
;;
;; KEY: THE EXEMPTION IS BY LINE, so the inline check's send and its lstart
;; comparison stay on one source line (retire-race's kill9-if-same-sh: `&& [
;; "$(ps -o lstart= ...)" = ... ] && kill -9 $1`). Split across two lines, the
;; send is read as bare and its file's row goes red; it cannot pass unseen.
(define (word-char? c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\_ #\. #\-))))
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
                 (flag (substring line after flag-end))
                 (spaces-end (let scan ((j flag-end))
                               (if (and (< j len) (char=? (string-ref line j) #\space)) (scan (+ j 1)) j))))
            (loop after
                  (if (and (or (= k 0) (not (word-char? (string-ref line (- k 1)))))
                           (not (string=? flag " -0"))
                           (> spaces-end flag-end)
                           (< spaces-end len)
                           (let ((c (string-ref line spaces-end)))
                             (or (memv c '(#\" #\$)) (char-numeric? c) (char-lower-case? c))))
                      (+ n 1)
                      n)))))))

;; THE SENDS LEFT, NAMED. Each: census row, why it stays, the text that marks
;; its line, and how many lines carry that text. A send added to one of
;; these files, or one of these moved onto the check without its entry
;; being struck here, reds the file's row. Keyed by text, not line number,
;; so a line may move.
(define remainders
  '(("log15.sc")
    ("launch-self.sc"
     (95 "SELF: a command signalling itself" "sh -c 'kill -TERM $$'" 1)
     (97 "slice B: W's pid, read from w.pid" "(sh \"kill -TERM \" (number->string w))" 1))
    ("runner-self.sc"
     (165 "slice B: the inner runner's pid, TERM (rows 165-169)" "(sh \"kill -TERM \" (number->string runner))" 5)
     (170 "slice B: the inner runner's pid, KILL" "(sh \"kill -9 \" (number->string runner))" 1))
    ("retire-race.sc")
    ("cli2.sc")
    ("f0-ondemand.sc"
     (76 "slice B: so-serve.pid" "\"kill $(cat \" scratch \"/so-serve.pid)" 1)
     (77 "slice B: pids looked up by ps" "\"kill \" (number->string pid)" 1)
     (78 "slice B: pids looked up by ps" "xargs kill" 1))))

;; The checked sends that must be there, per file: the text of the form and
;; the least number of lines that carry it.
(define converted
  '(("log15.sc" "(signal! " 4)
    ("launch-self.sc" "(signal! " 1)
    ("runner-self.sc" "(signal! " 1)
    ("retire-race.sc" "trap 'kill9_if_same " 2)
    ("cli2.sc" "(signal! " 1)
    ("f0-ondemand.sc" "(signal! " 1)))

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
      (let ((c (assoc f converted)))
        (want (format "SA-3 ~a: the checked sends are there (~s on at least ~a lines)" f (cadr c) (caddr c))
              (let ((n (length (filter (lambda (l) (has? (cdr l) (cadr c))) (code-lines f)))))
                (>= n (caddr c)))
              #t))))
  remainders)

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

(system (string-append "rm -rf " here))

(printf "rows: ~a\n~a failures\nsignal-audit complete\n" rows bad)
(exit (if (zero? bad) 0 1))
