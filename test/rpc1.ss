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

;; The dispatcher: one request, one answer.
;;
;; The command line and a caller over a socket ask the same questions of
;; the same store. Before this library they answered them separately, and
;; the duplication was already visible -- `nearest-ids` was built twice
;; inside cli.ss, once for `read` and once for `read --md`. What these
;; rows are for is the property that replaced it: THERE IS ONLY ONE PLACE
;; THAT ANSWERS, so the two cannot disagree.
;;
;; The strongest row here is therefore not about any one verb. It runs a
;; question BOTH WAYS -- through the command and through the dispatcher
;; -- and requires the same answer. An implementation that kept a second
;; opinion anywhere fails it without anyone having to guess where.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia ffi) (theourgia wire)
        (only (igropyr crypto) sha256 bytevector->hex))

;; THE RANGE A SEGMENT HOLDS, READ OUT OF THE SEGMENT. A manifest entry
;; declares first and last sequence beside the hash. A fixture that
;; declared them from memory would be asserting its own arithmetic
;; rather than what it actually wrote, and the product's own check for a
;; manifest that contradicts its bytes would then be measuring the
;; fixture.
(define (segment-seqs bytes)
  (let ((text (utf8->string bytes)))
    (let loop ((i 0) (start 0) (seqs '()))
      (cond
        ((>= i (string-length text)) (reverse seqs))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (if (and (pair? r) (eq? (car r) 'ok)) (cons (cadr r) seqs) seqs))))
        (else (loop (+ i 1) start seqs))))))

(define (manifest-entry n hash bytes)
  (let ((seqs (segment-seqs bytes)))
    (if (null? seqs)
        (list n hash 1 1)
        (list n hash (apply min seqs) (apply max seqs)))))

(define (manifest-entry-text n hash bytes)
  (let ((e (manifest-entry n hash bytes)))
    (string-append "(" (number->string n) " \"" hash "\" "
                   (number->string (caddr e)) " " (number->string (cadddr e)) ")")))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
(define cli
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/cli.ss"))
         (above (string-append dir "/../cli.ss")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'rpc1
              "cli.ss is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.ss sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "rpc1 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))




(define scratch (test-dir "rpc1"))
(define home (string-append scratch "/home"))
(define out-path (string-append scratch "/out.txt"))
(define err-path (string-append scratch "/err.txt"))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (slurp path))) (if b (utf8->string b) "")))

(define n-store 0)
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    (putenv "THEOURGIA_HOME" home)
    d))

;; THE COMMAND, RUN THE WAY A SHELL RUNS IT.
(define (cli-run store . args)
  (let* ((cmd (string-append
                "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                "THEOURGIA_HOME=" home " "
                "scheme --script " cli " "
                (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                "--store " store " > " out-path " 2> " err-path))
         (code (system cmd)))
    (list code (text-of out-path))))

;; THE DISPATCHER, ASKED THE SAME QUESTION IN THIS PROCESS.
(define (ask store . request)
  (putenv "THEOURGIA_HOME" home)
  (rpc-dispatch store (map (lambda (x) (if (symbol? x) x x)) request) "agent:claude"))

(printf "== S1: a request that is not a request is answered, not raised ==\n")
;; Whoever sent it is on the other side of a socket and cannot catch a
;; condition raised in here.
(define d1 (fresh-store!))
(ask d1 'init)
(want "a request that is not a list" (rpc-dispatch d1 'hello "a") '(error bad-request not-a-list))
(want "an empty request" (rpc-dispatch d1 '() "a") '(error bad-request empty))
(want "a tag that is not a symbol" (rpc-dispatch d1 (list "init") "a")
      '(error bad-request tag-not-a-symbol))
(want "an argument that is not a string" (rpc-dispatch d1 (list 'read 'x) "a")
      '(error bad-request arguments-not-strings))
(want "TWIN: a well-formed request is not a bad request"
      (car (rpc-dispatch d1 '(check) "a"))
      'check)

(printf "\n== S2: the verb list is the whole of what exists ==\n")
;; `eval` is not removed from the table by a second rule -- it was never
;; in it -- so a verb that does not exist and one deliberately absent are
;; the same answer, and there is nowhere to forget.
(want "eval is not a verb" (memq 'eval (rpc-verbs)) #f)
;; The same answer APART FROM THE TAG THEY NAME -- comparing the two
;; whole answers would compare the tags and be trivially false, which is
;; a row that passes while saying nothing.
(define (without-tag answer)
  (list (car answer) (cadr answer) (cadddr answer)))
(want "an unknown tag and a deliberately absent one answer identically"
      (equal? (without-tag (rpc-dispatch d1 '(eval "(+ 1 2)") "a"))
              (without-tag (rpc-dispatch d1 '(frobnicate "x") "a")))
      #t)
(want "and both name the verbs that do exist"
      (list (car (rpc-dispatch d1 '(eval "x") "a"))
            (cadr (rpc-dispatch d1 '(eval "x") "a"))
            (car (cadddr (rpc-dispatch d1 '(eval "x") "a"))))
      (list 'error 'unknown-verb 'verbs))

(printf "\n== S3: the three kinds of answer, and which is a success ==\n")
;; An answer says which of three things it is, so that whoever renders it
;; never has to guess from the shape. Guessing -- "a list of pairs is
;; probably items" -- would classify a block's field alist as items the
;; first time someone read a block with two fields.
(define d2 (fresh-store!))
(ask d2 'init)
(ask d2 'insert "--under" "root" "--title" "A")
(want "an outline is text" (car (cadr (ask d2 'outline))) 'text)
(want "a search is items" (car (cadr (ask d2 'search "A"))) 'items)
(want "a read is a single datum"
      (let ((a (ask d2 'read "nosuch.1"))) (car a))
      'error)
(want "check reports what it found and says separately whether it is sound"
      (list (car (ask d2 'check)) (rpc-ok? (ask d2 'check)))
      (list 'check #t))
(want "and a damaged store is the same shape with the other verdict"
      (let* ((d (fresh-store!)))
        (ask d 'init)
        (ask d 'insert "--under" "root" "--title" "x")
        (let* ((w (car (cadr (assq 'writers (cdr (ask d 'check))))))
               (seg (string-append d "/writers/" (car w) "/" (segment-file-name 1)))
               (bytes (slurp seg)))
          (put! seg (let ((o (bytevector-copy bytes)))
                      (bytevector-u8-set! o (- (bytevector-length o) 20)
                                          (if (= 98 (bytevector-u8-ref o (- (bytevector-length o) 20))) 99 98))
                      o))
          (list (car (ask d 'check)) (rpc-ok? (ask d 'check)))))
      (list 'check #f))

(printf "\n== S4: the command and the dispatcher answer the same question ==\n")
;; THE ROW THIS BATCH EXISTS FOR. Before the dispatcher the two answered
;; separately, and the duplication was already in the tree: `nearest-ids`
;; was built twice inside cli.ss. An implementation that kept a second
;; opinion anywhere fails here without anyone having to guess where.
;;
;; The comparison is on what the CLI PRINTS against what the dispatcher
;; ANSWERS, rendered by the same rule the CLI uses -- so a difference in
;; either the answer or the rendering shows up.
(define (rendered answer)
  (let-values (((out get) (open-string-output-port)))
    (cond
      ((and (pair? answer) (eq? (car answer) 'ok)
            (pair? (cdr answer)) (pair? (cadr answer))
            (eq? (car (cadr answer)) 'text))
       (put-string out (cadr (cadr answer))))
      ((and (pair? answer) (eq? (car answer) 'ok)
            (pair? (cdr answer)) (pair? (cadr answer))
            (eq? (car (cadr answer)) 'items))
       (for-each (lambda (i) (write i out) (newline out)) (cdr (cadr answer))))
      (else (write answer out) (newline out)))
    (get)))

(define d3 (fresh-store!))
(ask d3 'init)
(ask d3 'insert "--under" "root" "--title" "alpha" "--text" "a body")
(ask d3 'insert "--under" "root" "--title" "beta")
(ask d3 'tag "v1")
(define (both-ways store . request)
  (let ((printed (cadr (apply cli-run store (map (lambda (x) (if (symbol? x) (symbol->string x) x))
                                                 request))))
        (answered (rendered (apply ask store request))))
    (list printed answered)))
(define (agree? pair) (string=? (car pair) (cadr pair)))

(want "outline agrees" (agree? (both-ways d3 'outline)) #t)
(want "search agrees" (agree? (both-ways d3 'search "alpha")) #t)
(want "log agrees" (agree? (both-ways d3 'log)) #t)
(want "tag agrees" (agree? (both-ways d3 'tag)) #t)
(want "conflicts agrees" (agree? (both-ways d3 'conflicts)) #t)
(want "check agrees" (agree? (both-ways d3 'check)) #t)
;; AND THE ONE THAT WAS WRITTEN TWICE. Both spellings of a read went
;; through their own copy of the unknown-id refusal; now there is one.
(want "read of an unknown id agrees" (agree? (both-ways d3 'read "nosuch.9")) #t)
(want "read --md of an unknown id agrees"
      (agree? (both-ways d3 'read "nosuch.9" "--md")) #t)
;; A CONTROL, so the row above cannot pass by both sides being empty.
(want "CONTROL: these answers are not empty"
      (for-all (lambda (r) (> (string-length (car r)) 0))
               (list (both-ways d3 'outline) (both-ways d3 'log)
                     (both-ways d3 'read "nosuch.9")))
      #t)

(printf "\n== S5: a store that is not there, and the one verb that may make it ==\n")
;; `init` is the only verb that runs where there is no store yet. Every
;; other one is asking about a store that has to be there, and "there is
;; no store here" is the fact the caller can act on -- not the kind of
;; the first file the load layer happened to miss.
(define d4 (fresh-store!))
(want "every other verb answers no-store before it opens anything"
      (list (ask d4 'outline) (ask d4 'check) (ask d4 'log))
      (list (list 'error 'no-store d4) (list 'error 'no-store d4) (list 'error 'no-store d4)))
(want "init is allowed, and then the others are"
      (list (car (ask d4 'init)) (car (ask d4 'check)))
      (list 'ok 'check))
(want "TWIN: a second init is refused, and it is not a bad request"
      (list (car (ask d4 'init)) (cadr (ask d4 'init)))
      (list 'error 'already-initialised))

(printf "\n~a failures\n" bad)
(printf "rpc1 complete\n")
