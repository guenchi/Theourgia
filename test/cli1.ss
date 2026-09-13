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

;; P1, P3's rendering, P6, P7 and P8 through real processes. Every row
;; here runs `scheme --script cli.ss ...` and judges stdout, stderr and
;; the exit code -- the three things an agent or a shell actually sees.
;; The library-level half of the same criteria is store1.ss.
(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia ffi) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex))

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
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE. Rows read an
;; answer apart, and a seeded defect that changes the answer's SHAPE
;; makes the accessor raise while the row's value is being computed --
;; outside anything that was catching. The file then ends where it
;; stood, every row below it goes unrun, and the runner sees no `FAIL`
;; at all: the round scored three such defects as crashes with no
;; failures, for answers the store had in fact got right and said
;; plainly.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded `got`.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; still outside it, and a raise there still ends the file.
;; BOTH HALVES, BECAUSE EITHER CAN RAISE. The first version of this
;; guarded `got` only, and a row whose EXPECTATION is derived from the
;; program's own answer -- `(cadr (cadr (datum-of init-run)))`, the store
;; id that the registry is then required to agree with -- raised while
;; the expectation was being built and ended the file just the same. Two
;; sides of one comparison, and only one of them was being asked whether
;; it could be computed.
;; HOW MANY ROWS ACTUALLY RAN. A file that ends early still
;; reports the failures it had already found, so a seeded defect
;; that kills the file after a few rows is scored as caught while
;; the rows below it never ran. The count is the only thing that
;; tells those apart, and it has to be compared against the same
;; file's count on unmutated code -- there is no static number to
;; compare it with, because rows are written inside loops and case
;; tables as well as one at a time.
(define rows-run 0)

(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (want-1 label (caught got) (caught expect))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

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
      (else (assertion-violation 'cli1
              "cli.ss is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.ss sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "cli1 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))

(define (write-file! path text)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p (string->utf8 text)))))

(define (write-file! path text)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p (string->utf8 text)))))

(define (slurp path)
  (guard (e (#t ""))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) "" (utf8->string b)))))

;; STDOUT, STDERR AND THE EXIT CODE ARE THREE SEPARATE READINGS. Folding
;; them together is how a diagnostic on stderr comes to be read as the
;; answer, and how a non-zero code goes unnoticed because the text
;; looked right.
;;
;; THE CHILD INHERITS THE WHOLE SUITE'S ENVIRONMENT, not the one this
;; file set up. The suite runner exports THEOURGIA_INJECT=on for the
;; fault cases, every `scheme --script cli.ss` started here inherits it,
;; and the expansion-time banner then puts a SECOND line on stderr --
;; so the stderr rows below passed when run by hand and failed under
;; run-all. `env -u` removes it for the ordinary runs; a row that wants
;; injection sets it on its own command line, which is where wanting it
;; should be visible. Every child started from this file has to think
;; about this: the environment it gets is the suite's, not this file's.
(define scratch (test-dir "cli1"))
(define (run store args . stdin)
  (let ((out (string-append scratch "/out.txt"))
        (err (string-append scratch "/err.txt")))
    (let* ((cmd (string-append
                  (if (null? stdin)
                      ""
                      (string-append "printf '%s' " (car stdin) " | "))
                  "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                  "scheme --script " cli " " args
                  (if store (string-append " --store " store) "")
                  " > " out " 2> " err))
           (code (system cmd)))
      (list code (slurp out) (slurp err)))))

(define (code-of r) (car r))
(define (out-of r) (cadr r))
;; THE ID THE STORE SAYS IT MADE, not the one a count predicts. Every
;; record advances the sequence -- a move and a delete as much as an
;; insert -- so a fixture that counts its inserts names a block that
;; does not exist as soon as it has done anything else.
(define (id-of r)
  (let ((a (datum-of r)))
    (car (car (cadr (assq 'state (cdr a)))))))
(define (substring-at? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (lines-of text)
  (let loop ((cs (string->list text)) (cur '()) (out '()))
    (cond ((null? cs) (reverse (if (null? cur) out (cons (list->string (reverse cur)) out))))
          ((char=? (car cs) #\newline)
           (loop (cdr cs) '() (cons (list->string (reverse cur)) out)))
          (else (loop (cdr cs) (cons (car cs) cur) out)))))
(define (err-of r) (caddr r))
;; A CHILD THAT NEVER RAN IS A READING, NOT AN EXCEPTION. An empty
;; stdout used to reach `read` as end-of-file and every row that took a
;; cadr of the result died there -- so a fixture whose child could not
;; start ended with no failure count at all, which reads like a run that
;; was never made. The exit code and the first line of stderr come back
;; instead, because those are what say why.
(define (datum-of r)
  (let ((text (out-of r)))
    (if (= 0 (string-length text))
        (list 'child-failed (code-of r)
              (let loop ((i 0))
                (cond ((>= i (string-length (err-of r))) (err-of r))
                      ((char=? (string-ref (err-of r) i) #\newline)
                       (substring (err-of r) 0 i))
                      (else (loop (+ i 1))))))
        (guard (e (#t (list 'unreadable text)))
          (let* ((p (open-string-input-port text))
                 (x (read p)))
            (if (eof-object? x) (list 'unreadable text) x))))))
;; THE DIAGNOSTIC LINE THE FFI PRINTS ON EVERY RUN IS NOT AN ERROR. P6
;; allows stderr to hold one line; what it must not hold is a second
;; one, and what stdout must never hold is a diagnostic.
(define (err-lines r)
  (let loop ((i 0) (n 0))
    (cond ((>= i (string-length (err-of r))) n)
          ((char=? (string-ref (err-of r) i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))

;; THE WRITER'S NAME IS READ OFF THE STORE, not remembered from the init
;; answer: every row that builds a block id needs it, and taking it from
;; the directory is the same thing a second process would have to do.
;; THE MARKER HAS TO BE THE TYPE THE CALLERS USE. Every caller does
;; `(string-append (writer-of d) ".1")` to build a block id, so a symbol
;; here raised inside the caller instead of reaching a row -- and the
;; file ended. A string that cannot be a writer's name travels the same
;; path, reaches the comparison, and makes the row say what it wanted.
(define (writer-of dir)
  (let ((ws (store-writers dir)))
    (if (null? ws) "no-writer-in-this-store" (car ws))))

(define (files-under dir)
  (let ((listing (string-append scratch "/files.txt")))
    (system (string-append "cd " dir " && find . -type f | sort > " listing))
    (slurp listing)))
(define (tree-digest dir)
  (let ((listing (string-append scratch "/digest.txt")))
    (system (string-append "cd " dir " && find . -type f | sort | xargs md5 -q 2>/dev/null | sort > "
                           listing))
    (string-append (files-under dir) "|" (slurp listing))))

(printf "== P1: init ==\n")
(define d1 (test-dir "cli1init"))
;; THE MACHINE REGISTRY LIVES OUTSIDE THE STORE. Pointing
;; THEOURGIA_HOME inside it puts the registry's own files into the
;; store's listing, and the layout row then fails for a layout that
;; is correct.
(define home1 (string-append scratch "/home1"))
(putenv "THEOURGIA_HOME" home1)
(define init-run (run d1 "init"))
(want "init answers with the store and the writer, and exits 0"
      (list (code-of init-run)
            (car (datum-of init-run))
            (car (cadr (datum-of init-run)))
            (car (caddr (datum-of init-run))))
      (list 0 'ok 'store 'writer))
;; A TOP-LEVEL BINDING TAKEN FROM THE PROGRAM'S ANSWER MUST NOT RAISE.
;; Rows are guarded; this is not, and it runs at the file's top level --
;; so when `init` answered `(usage (init))` instead, `caddr` raised here
;; and ended the file, leaving every row below unrun. The guarded row
;; above had already reported the real failure; this line then threw the
;; rest of the evidence away.
;;
;; THE SUBSTITUTE MUST BE ONE NOTHING MATCHES. Rows downstream build
;; paths from it and compare them, so a name that cannot be a writer
;; makes each of them fail and say what it wanted -- which is the report
;; the file exists to produce.
(define the-writer
  (let ((d (datum-of init-run)))
    (if (and (pair? d) (eq? (car d) 'ok) (= 3 (length d))
             (pair? (caddr d)) (pair? (cdr (caddr d))))
        (cadr (caddr d))
        "NO-WRITER-IN-THE-ANSWER")))
;; THE LAYOUT IS SECTION 4.1 EXACTLY, named file by file. A row that
;; only checked "the directory is not empty" would pass for a store
;; missing the lock, which is the one file that must never be replaced.
(want "the directory holds exactly the files section 4.1 names"
      (files-under d1)
      (string-append "./instance.sexp\n./lock\n./meta.sexp\n"
                     "./writers/" the-writer "/000001.sexp\n"
                     "./writers/" the-writer "/owner.sexp\n"))
(want "and the current segment is empty, not absent"
      (string-length (slurp (string-append d1 "/writers/" the-writer "/000001.sexp")))
      0)
;; THE REGISTRY FILE IS NAMED DIRECTLY, not asked of registry-path.
;; That procedure resolves the home when the library is first invoked,
;; and this fixture changes THEOURGIA_HOME per store -- so it answers
;; for whichever home happened to be current at load time, which is a
;; reading about this process rather than about the store just made.
;; THE ENTRY IS KEYED BY STORE ID, NOT BY PATH -- a store that is moved
;; or mounted somewhere else is still the same store to the registry.
;; The water mark starts at zero: init has reserved nothing.
(want "the machine registry carries this store's id, writer and a zero water mark"
      (let* ((text (slurp (string-append home1 "/instances.sexp")))
             (data (guard (e (#t 'unreadable))
                     (read (open-string-input-port text))))
             (sid (cadr (cadr (datum-of init-run)))))
        (if (not (list? data))
            (list 'unreadable text)
            (let ((e (assoc sid data)))
              (if (not e)
                  (list 'no-entry-for sid data)
                  (list (car e) (caddr e) (cadddr e) (car (cddddr e)))))))
      (list (cadr (cadr (datum-of init-run))) the-writer 0 'active))
(define before-second (tree-digest d1))
(define second-init (run d1 "init"))
(want "a second init is refused, with a non-zero code"
      (list (> (code-of second-init) 0) (car (datum-of second-init)) (cadr (datum-of second-init)))
      (list #t 'error 'already-initialised))
(want "and it changed nothing on disk"
      (equal? before-second (tree-digest d1))
      #t)
;; A DIRECTORY CARRYING SOMEONE ELSE'S WRITER IS NOT "ALREADY
;; INITIALISED" -- it is a store that arrived from elsewhere, and
;; minting a second writer beside the first would give two writers the
;; same history to own.
(define d1f (test-dir "cli1foreign"))
(system (string-append "mkdir -p " d1f "/writers/aaaaaaaa"))
(system (string-append "printf '((machine \"elsewhere\") (instance \"n-1-1\"))\\n' > "
                       d1f "/writers/aaaaaaaa/owner.sexp"))
(define foreign-init (run d1f "init"))
(want "init on a store carrying a foreign writer points at adopt"
      (list (> (code-of foreign-init) 0)
            (car (datum-of foreign-init))
            (cadr (datum-of foreign-init))
            (cadddr (datum-of foreign-init)))
      (list #t 'error 'foreign-writer (list 'remedy 'adopt)))

(printf "== P3: what outline prints ==\n")
(define d3 (test-dir "cli1outline"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home3"))
(run d3 "init")
(run d3 "insert --under root --title Alpha")
(run d3 "insert --under root --title Beta")
(define A (string-append (writer-of d3) ".1"))
(define Bb (string-append (writer-of d3) ".2"))
(run d3 (string-append "insert --under " A " --title Child"))
(define C (string-append (writer-of d3) ".3"))
;; THE EXPECTED TEXT IS WRITTEN OUT HERE, not derived from the same code
;; that produces it. A render compared against its own generator agrees
;; with itself however wrong it is.
(want "the outline is a tree, indented by depth"
      (out-of (run d3 "outline"))
      (string-append "- " A "  Alpha\n"
                     "  - " C "  Child\n"
                     "- " Bb "  Beta\n"))
(want "a deleted block leaves the outline and its child is an orphan"
      (begin (run d3 (string-append "del " A))
             (out-of (run d3 "outline")))
      (string-append "- " Bb "  Beta\n"
                     "orphans:\n"
                     "- " C "  Child\n"))

;; AN ORPHAN IS A ROOT AND ITS SUBTREE IS DRAWN UNDER IT. The row above
;; has one orphan with nothing beneath it, so it passes equally well on a
;; printer that lists orphans and stops -- and that printer drops every
;; grandchild of a deleted block from the only view the command line
;; offers, while `read` goes on answering for them. A block that is in
;; the store and absent from the listing is the worst of the three
;; possible states, because nothing tells the operator to look.
(define d3b (test-dir "cli1orphans"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home3b"))
(run d3b "init")
(run d3b "insert --under root --title A")
(define oA (string-append (writer-of d3b) ".1"))
(run d3b (string-append "insert --under " oA " --title B"))
(define oB (string-append (writer-of d3b) ".2"))
(run d3b (string-append "insert --under " oB " --title C"))
(define oC (string-append (writer-of d3b) ".3"))
(want "CONTROL: three generations, indented by depth"
      (out-of (run d3b "outline"))
      (string-append "- " oA "  A\n"
                     "  - " oB "  B\n"
                     "    - " oC "  C\n"))
(want "deleting the top leaves the orphan AND everything under it"
      (begin (run d3b (string-append "del " oA))
             (out-of (run d3b "outline")))
      (string-append "orphans:\n"
                     "- " oB "  B\n"
                     "  - " oC "  C\n"))
;; AND THE GRANDCHILD WAS THERE ALL ALONG, which is what makes its
;; absence a reporting defect rather than a deletion.
(want "and the grandchild is still a block the store answers for"
      (let ((a (datum-of (run d3b (string-append "read " oC)))))
        (and (pair? a) (car a)))
      'ok)
;; TWIN: THE DEPTH LIMIT STILL STOPS WHERE IT SAYS. The orphan is the row
;; at depth 0, so a limit of one prints it and nothing under it -- the
;; same rule the top of the tree follows. Without this row the fix above
;; is also passed by a printer that ignores the limit under orphans.
(want "TWIN: --depth 1 prints the orphan and not its children"
      (out-of (run d3b "outline --depth 1"))
      (string-append "orphans:\n"
                     "- " oB "  B\n"))
;; AND ONE NUMBER MEANS ONE THING IN BOTH SECTIONS. An orphan is a root,
;; so it is a row at depth 0, and `--depth 0` asks for no rows at all.
;; The orphan section used to print its roots straight through the limit
;; while the tree above printed none of its own.
(want "--depth 0 prints nothing, in both sections"
      (out-of (run d3b "outline --depth 0"))
      "")
;; A BLOCK THE TREE ALREADY DREW IS NOT DRAWN AGAIN. A parent chain that
;; closes on itself through a deleted block makes a block both a
;; structural conflict and an orphan; `state-outline` relocates a cyclic
;; row to the top, so the tree has already printed it -- and with it, now,
;; its whole subtree. Listing it again under `orphans:` said the same
;; block was in two places, and the walk added for the row above turned
;; one duplicated row into a duplicated subtree.
(define d3c (test-dir "cli1cycle"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home3c"))
(run d3c "init")
(run d3c "insert --under root --title A")
(define cA (string-append (writer-of d3c) ".1"))
(run d3c (string-append "insert --under " cA " --title B"))
(define cB (string-append (writer-of d3c) ".2"))
(run d3c (string-append "insert --under " cB " --title C"))
(define cC (string-append (writer-of d3c) ".3"))
(run d3c (string-append "move " cA " " cB))
(run d3c (string-append "del " cA))
(want "a block the tree drew as a conflict is not repeated under orphans"
      (out-of (run d3c "outline"))
      (string-append "- " cB "  B  conflict  orphan\n"
                     "  - " cC "  C\n"))
;; AND THE ROW CARRIES BOTH FACTS, because it is the only row left to
;; carry the second. A block whose parent chain closes on itself is a
;; conflict; a block whose chain closes through a DELETED ancestor is a
;; conflict and unreachable from any root. Drawing it once was right;
;; dropping the second word made those two stores print the same
;; listing, and only one of them has lost a block from the tree.
(want "a conflict that is not also an orphan says only the one word"
      (begin
        ;; A CYCLE WITH NOTHING DELETED, and the ids are read from the
        ;; answers rather than counted. This store has already taken a
        ;; move and a delete, which are records too, so guessing `.4` and
        ;; `.5` from the insert count named two blocks that do not exist
        ;; -- and the row then measured a store where nothing had
        ;; happened.
        (let ((x (id-of (run d3c "insert --under root --title X"))))
          (let ((y (id-of (run d3c (string-append "insert --under " x " --title Y")))))
            (run d3c (string-append "move " x " " y))
            (let loop ((ls (lines-of (out-of (run d3c "outline")))) (found #f))
              (cond ((null? ls) found)
                    ((and (substring-at? (car ls) x) (substring-at? (car ls) "conflict"))
                     (loop (cdr ls) (not (substring-at? (car ls) "orphan"))))
                    (else (loop (cdr ls) found)))))))
      #t)

(printf "== an option's value is not scanned for options ==\n")
;; A TOKEN THAT SPELLS AN OPTION IS STILL A VALUE WHERE A VALUE BELONGS.
;; The reader this replaced pulled one option at a time, each pass
;; searching the whole list, so a value that looked like an option was
;; found again where it sat. Measured before the fix, on this exact
;; command:
;;
;;     insert --under root --title "--actor" --text body
;;       answered  ok
;;       title     "body"
;;       actor     "--text"
;;
;; THE ACTOR IS THE SERIOUS ONE. A record's actor is evidence of who
;; wrote it; a reader that can be made to take it from a neighbouring
;; token forges that evidence on a command that reports success. So this
;; row asserts all three -- the answer, what the block says, and what the
;; record says -- because any one of them alone passes on some wrong
;; implementation.
(define d7 (test-dir "cli1options"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home7"))
(run d7 "init")
(want "a title that spells another option is a title, and nothing else moves"
      (let* ((a (datum-of (run d7 "insert --under root --title '--actor' --text body")))
             (id (string-append (writer-of d7) ".1")))
        (list (and (pair? a) (car a))
              (out-of (run d7 "outline"))
              (let ((text (out-of (run d7 "log"))))
                (and (string? text)
                     (let loop ((i 0))
                       (cond ((>= (+ i 7) (string-length text)) 'no-actor)
                             ((string=? (substring text i (+ i 7)) "(actor ")
                              (let loop2 ((j (+ i 8)) (out '()))
                                (if (char=? #\" (string-ref text j))
                                    (list->string (reverse out))
                                    (loop2 (+ j 1) (cons (string-ref text j) out)))))
                             (else (loop (+ i 1)))))))))
      (list 'ok
            (string-append "- " (writer-of d7) ".1  --actor\n")
            (or (let ((e (getenv "THEOURGIA_ACTOR"))) (and e (> (string-length e) 0) e))
                (let ((u (getenv "USER"))) (and u (> (string-length u) 0) u))
                "cli")))
;; TWIN: AND AN ORDINARY TITLE STILL WORKS. Without this the row above is
;; also passed by a reader that stopped recognising `--actor` at all.
(want "TWIN: --actor is still an option when it is in an option's place"
      (begin (run d7 "insert --under root --title Plain --actor someone")
             (let ((text (out-of (run d7 "log"))))
               (and (string? text)
                    (let loop ((i 0))
                      (cond ((> (+ i 7) (string-length text)) #f)
                            ((string=? (substring text i (+ i 7)) "someone") #t)
                            (else (loop (+ i 1))))))))
      #t)
;; AN OPTION GIVEN TWICE IS A DUPLICATE, not the first or the last one
;; silently winning. Two spellings of one command must not become one
;; request, and picking either quietly is how they would.
(want "the same option twice is refused by name"
      (datum-of (run d7 "insert --under root --title A --title B"))
      '(error bad-request duplicate-option "--title"))
;; AND AN OPTION WITH NOTHING AFTER IT IS MISSING ITS VALUE, which is a
;; different complaint from "the form is wrong": the caller wrote the
;; option, so telling them the shape is unrecognised sends them to check
;; the wrong thing.
(want "an option at the end of the line is missing its value, by name"
      (datum-of (run #f (string-append "insert --store " d7 " --under root --title")))
      '(error bad-request missing-option-value "--title"))
;; AND THE TOKEN AFTER AN OPTION IS ITS VALUE WHATEVER IT SPELLS, which
;; is the other half of the same rule and pulls the opposite way. The row
;; above needs the option to be LAST; put anything after it -- another
;; option included -- and that thing is the value. Both halves are needed
;; and they are one decision: what a token means is decided by where it
;; is, once, and never revised by a later pass looking for a name.
(want "a following option is taken as the value, not as an option"
      (datum-of (run #f (string-append "insert --title --store --under root")))
      '(error no-store "."))

(printf "== P6: errors an agent can act on ==\n")
(define d6 (test-dir "cli1errors"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home6"))
(run d6 "init")
(run d6 "insert --under root --title Only")
(define only-id (string-append (writer-of d6) ".1"))
(define short-set (run d6 "set"))
(want "a verb given the wrong shape prints the right shape"
      (list (> (code-of short-set) 0) (datum-of short-set) (out-of short-set))
      (list #t '(usage (set <id> <field> <value>))
            "(usage (set <id> <field> <value>))\n"))
(define unknown (run d6 "set nosuch.9 title x"))
(want "an unknown id names the ids that do exist"
      (list (> (code-of unknown) 0)
            (car (datum-of unknown)) (cadr (datum-of unknown)) (caddr (datum-of unknown))
            (cadddr (datum-of unknown)))
      (list #t 'error 'unknown-id "nosuch.9" (list 'nearest (list only-id))))
(want "and neither of them wrote more than one line to stderr"
      (list (err-lines short-set) (err-lines unknown))
      (list 1 1))

(printf "== P6: a store that cannot be opened is still an answer ==\n")
;; AN UNCAUGHT EXCEPTION IS NOT AN ANSWER. The agent reading stdout gets
;; nothing, the backtrace goes somewhere it is not looking, and "the
;; store is not there" is indistinguishable from "the tool broke". Every
;; one of these used to print a Chez condition and leave stdout empty.
(define missing (run "/nonexistent/nowhere" "outline"))
(want "outline on a path with no store answers, and says which path"
      (list (> (code-of missing) 0) (datum-of missing) (err-lines missing))
      (list #t '(error no-store "/nonexistent/nowhere") 1))
(define d6b (test-dir "cli1nostore"))
(want "so does a directory that exists but was never initialised"
      (let ((r (run d6b "insert --under root --title x")))
        (list (> (code-of r) 0) (datum-of r)))
      (list #t (list 'error 'no-store d6b)))
;; A STORE WHOSE meta.sexp IS THERE BUT UNREADABLE IS A DIFFERENT ANSWER
;; from one that is absent: the caller can create the second and must
;; not create over the first.
(define d6c (test-dir "cli1badmeta"))
(system (string-append "printf '(oops' > " d6c "/meta.sexp"))
(define bad-meta (run d6c "outline"))
(want "a corrupt meta is named as a meta problem, not as a missing store"
      (list (> (code-of bad-meta) 0)
            (car (datum-of bad-meta))
            (cadr (datum-of bad-meta))
            (err-lines bad-meta))
      (list #t 'error 'meta 1))
(define d6d (test-dir "cli1oldformat"))
(system (string-append "printf '((format 9))\\n' > " d6d "/meta.sexp"))
(want "and so is a format this build does not support"
      (let ((r (run d6d "outline")))
        (list (> (code-of r) 0) (car (datum-of r)) (cadr (datum-of r))
              (assq 'supported (cddr (datum-of r)))))
      (list #t 'error 'meta '(supported 1)))
;; CONTROL: THE SAME VERB ON A GOOD STORE STILL WORKS. A translation
;; layer that turned every outcome into an error would pass every row
;; above.
(want "CONTROL: outline on a real store still answers normally"
      (let ((r (run d3 "outline")))
        (list (code-of r) (> (string-length (out-of r)) 0)))
      (list 0 #t))

(printf "== P7: a batch is one lock and one answer per item ==\n")
(define d7 (test-dir "cli1batch"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home7"))
(run d7 "init")
(run d7 "insert --under root --title Head")
(define head (string-append (writer-of d7) ".1"))
(define good-batch
  (run d7 "batch"
       (string-append "'((insert root #f ((kind . section) (title . \"from-batch\")))"
                      " (set \"" head "\" note \"n\")"
                      " (link \"" head "\" explains \"" head "\"))'")))
(want "three items, three answers, exit 0"
      (list (code-of good-batch)
            (car (datum-of good-batch))
            (map car (cadr (datum-of good-batch))))
      (list 0 'batch '(ok ok ok)))
(define mixed-batch
  (run d7 "batch"
       (string-append "'((set \"" head "\" a \"1\")"
                      " (set \"nosuch.9\" b \"2\")"
                      " (set \"" head "\" c \"3\"))'")))
;; THE ITEM AFTER THE FAILURE IS NOT ATTEMPTED, and the ones before it
;; stay committed. Two answers for three items is the shape that says so.
(want "a failing item stops the rest and the answer is shorter than the input"
      (list (> (code-of mixed-batch) 0)
            (map car (cadr (datum-of mixed-batch))))
      (list #t '(ok error)))
(want "what came before the failure is on disk, what came after is not"
      (let* ((state (open-and-reduce d7))
             (b (state-read state head))
             (fs (cdr (assq 'fields b))))
        (list (and (assq 'a fs) #t) (and (assq 'c fs) #t)))
      (list #t #f))

(printf "== P4: two processes writing the same store ==\n")
;; ONE WRITER, TWO PROCESSES. Both open sessions against the same store
;; and the same local writer, so the sequence numbers they take have to
;; form one run with no gap and no repeat -- the exclusive lock is the
;; only thing making that true, and nothing about the answers each
;; process gets would look different if it were missing.
;; THE WHOLE LIST IS REDIRECTED, NOT ITS LAST COMMAND. A shell binds
;; `> file` to the simple command it follows, so `a; b; echo x > f`
;; captures only the echo -- and every line before it goes to the
;; terminal, where it reads as the fixture printing progress rather than
;; as a report that was supposed to be parsed.
(define (sh cmd)
  (let ((out (string-append scratch "/sh.txt")))
    (system (string-append "{ " cmd "; } > " out " 2>&1"))
    (slurp out)))

(define d4 (test-dir "cli1race"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home4"))
(run d4 "init")
(define many-path (string-append scratch "/many.ss"))
(write-file! many-path
  (string-append
    "#!r6rs\n(import (chezscheme) (theourgia store))\n"
    "(define a (cdr (command-line)))\n"
    "(let loop ((i 1))\n"
    "  (when (<= i (string->number (caddr a)))\n"
    "    (with-store-write (car a)\n"
    "      (lambda (st v)\n"
    "        (list (list 'insert 'root #f\n"
    "                    (list (cons 'kind 'section)\n"
    "                          (cons 'title (string-append (cadr a) \"-\"\n"
    "                                        (number->string i))))))))\n"
    "    (loop (+ i 1))))\n"))
(sh (string-append
      "cd " scratch " && ( env -u THEOURGIA_INJECT scheme --script " many-path
      " " d4 " P 50 & env -u THEOURGIA_INJECT scheme --script " many-path
      " " d4 " Q 50 & wait )"))

;; EVERY RECORD IS DECODED, not counted by lines: a torn or duplicated
;; record still occupies a line, and the property being asserted is
;; about the sequence numbers inside them.
(define (seqs-of dir)
  (let* ((w (writer-of dir))
         (text (slurp (string-append dir "/writers/" w "/000001.sexp")))
         (n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((>= i n) (reverse acc))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (cons (if (and (pair? r) (eq? (car r) 'ok)) (cadr r) r) acc))))
        (else (loop (+ i 1) start acc))))))

(want "a hundred records, numbered one to a hundred with no gap and no repeat"
      (let ((seqs (seqs-of d4)))
        (list (length seqs)
              (equal? seqs (let build ((k 100) (out '()))
                             (if (= k 0) out (build (- k 1) (cons k out)))))))
      (list 100 #t))
(want "and the outline lists all hundred, in the order they were committed"
      (let* ((text (out-of (run d4 "outline")))
             (w (writer-of d4))
             (ids (let loop ((i 0) (start 0) (acc '()))
                    (cond
                      ((>= i (string-length text)) (reverse acc))
                      ((char=? (string-ref text i) #\newline)
                       (let ((line (substring text start i)))
                         (loop (+ i 1) (+ i 1)
                               (cons (substring line 2
                                                (let scan ((j 2))
                                                  (if (or (>= j (string-length line))
                                                          (char=? (string-ref line j) #\space))
                                                      j (scan (+ j 1)))))
                                     acc))))
                      (else (loop (+ i 1) start acc))))))
        (let ((expected (let build ((k 1) (out '()))
                          (if (> k 100)
                              (reverse out)
                              (build (+ k 1)
;; CHEZ'S number->string GIVES UPPERCASE DIGITS ABOVE
                                     ;; NINE -- 10 comes out "A". The product
                                     ;; spells its ids with its own lowercase
                                     ;; table, so an expectation built from
                                     ;; number->string disagrees from the tenth
                                     ;; id onward and says nothing about order.
                                     (cons (string-append
                                             w "." (string-downcase (number->string k 36)))
                                           out))))))
          (list (length ids)
                (if (equal? ids expected)
                    #t
                    ;; A BARE #f SAYS NOTHING ABOUT WHERE. The first
                    ;; disagreeing pair is what tells "the order is
                    ;; wrong" apart from "the ids are built wrong".
                    (let find ((a ids) (b expected) (k 0))
                      (cond ((or (null? a) (null? b)) (list 'length k))
                            ((equal? (car a) (car b)) (find (cdr a) (cdr b) (+ k 1)))
                            (else (list 'row k 'got (car a) 'want (car b)))))))))
      (list 100 #t))

(printf "== P4: a reader waits for the writer's lock ==\n")
;; THE WRITER PARKS INSIDE THE CRITICAL SECTION and the reader is caught
;; waiting. What the reader must NOT do is read the half-written record
;; the writer is in the middle of appending -- so the assertion is both
;; that it waited and that what it finally read is whole.
;; THE CONTROLLER RELEASES ON EITHER SIGNAL. Waiting only for lock-wait
;; would hang for any build that does not take the lock at all, which is
;; precisely the build this is meant to catch.
(define d4b (test-dir "cli1barrier"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home4b"))
(run d4b "init")
(run d4b "insert --under root --title Seed")
(define hold-path (string-append scratch "/holder.ss"))
(write-file! hold-path
  (string-append
    "#!r6rs\n(import (chezscheme) (theourgia store))\n"
    "(with-store-write (cadr (command-line))\n"
    "  (lambda (st v)\n"
    "    '((insert root #f ((kind . section) (title . \"held\"))))))\n"))
(define barrier-report
  (let ((gate (string-append scratch "/gate"))
        (ht (string-append scratch "/holder.trace"))
        (qt (string-append scratch "/reader.trace"))
        (qo (string-append scratch "/reader.out")))
    (sh (string-append
          "rm -f " gate " " ht " " qt " " qo "; mkfifo " gate "; "
;; NOT WRAPPED IN A SUBSHELL. `( cmd & )` puts the job in a
          ;; child shell's table, so the outer `wait` has nothing to
          ;; wait for and the report is read while the processes are
          ;; still running -- which is how the reader's output came out
          ;; empty.
          "THEOURGIA_INJECT=on THEOURGIA_BARRIER=before-append:" gate " THEOURGIA_TRACE=1 "
          "  scheme --script " hold-path " " d4b " > /dev/null 2> " ht " & "
          "i=0; while [ $i -lt 4000 ] && ! grep -q barrier " ht " 2>/dev/null; do i=$((i+1)); done; "
          "env -u THEOURGIA_INJECT THEOURGIA_TRACE=1 scheme --script " cli
          "  outline --store " d4b " > " qo " 2> " qt " & "
          "j=0; while [ $j -lt 4000 ] && ! grep -qE 'lock-wait|enter-critical' " qt
          "  2>/dev/null; do j=$((j+1)); done; "
          ;; THE RELEASE IS BOUNDED, BECAUSE THE WAIT ABOVE IS. Both spins
          ;; give up after a while; this write did not, and a write into a
          ;; fifo blocks until somebody opens the other end. When a seeded
          ;; defect stopped the holder from ever reaching its barrier, the
          ;; spin expired, this line took over, and the shell waited
          ;; forever -- holding the pipe it had inherited, so the round
          ;; that started it could not move on either. Measured at one
          ;; hour fifty minutes.
          ;;
          ;; A READER FIRST, SO THE WRITE HAS SOMEWHERE TO GO, and a bound
          ;; on the whole release so that a fixture which cannot be
          ;; released still ENDS and reports. Giving up is a row that
          ;; fails; hanging is a row nobody ever reads.
          "( head -c 1 " gate " > /dev/null 2>&1 & rp=$!; "
          "  ( sleep 20; kill $rp 2>/dev/null ) 2>/dev/null & tp=$!; "
          "  printf x > " gate " 2>/dev/null; "
          "  wait $rp 2>/dev/null; kill $tp 2>/dev/null ) ; "
          "wait; "
          "echo READER-WAITED $(grep -c lock-wait " qt "); "
          "echo READER-ENTERED-EARLY $(grep -c enter-critical " qt "); "
          "echo LINES $(wc -l < " qo ")"))))
(define (report-field name)
  (let loop ((i 0) (start 0))
    (cond
      ((>= i (string-length barrier-report)) 'not-found)
      ((char=? (string-ref barrier-report i) #\newline)
       (let ((line (substring barrier-report start i)))
         (if (and (>= (string-length line) (string-length name))
                  (string=? (substring line 0 (string-length name)) name))
             (string->number (substring line (+ 1 (string-length name))
                                        (string-length line)))
             (loop (+ i 1) (+ i 1)))))
      (else (loop (+ i 1) start)))))
(want "the reader was observed waiting for the lock, and never entered early"
      (list (report-field "READER-WAITED") (report-field "READER-ENTERED-EARLY"))
      (list 1 0))
(want "and what it read once released is two whole records"
      (report-field "LINES")
      2)
;; TWO WRITERS, AND THE SECOND IS CAUGHT AT THE DOOR. `enter-critical`
;; is announced by the log layer INSIDE the critical section, so it is
;; the event that says "this process got in" -- and a reader never
;; reaches it, which is why this pair uses a second writer rather than
;; the reader above.
;; THE COUNTS ARE READ BEFORE THE BARRIER IS RELEASED. Afterwards the
;; second writer gets in legitimately and its enter-critical appears; a
;; reading taken then cannot tell a lock from no lock.
(define d4c (test-dir "cli1twowriters"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home4c"))
(run d4c "init")
(run d4c "insert --under root --title Seed")
(define (two-writers extra)
  (let ((gate (string-append scratch "/gate-" extra))
        (ht (string-append scratch "/p-" extra ".trace"))
        (qt (string-append scratch "/q-" extra ".trace"))
        (env (if (string=? extra "noflock") "THEOURGIA_NOFLOCK=1 " "")))
    (sh (string-append
          "rm -f " gate " " ht " " qt "; mkfifo " gate "; "
          "THEOURGIA_INJECT=on " env
          "THEOURGIA_BARRIER=before-append:" gate " THEOURGIA_TRACE=1 "
          "  scheme --script " hold-path " " d4c " > /dev/null 2> " ht " & "
          "i=0; while [ $i -lt 4000 ] && ! grep -q barrier " ht " 2>/dev/null; do i=$((i+1)); done; "
          "THEOURGIA_INJECT=on " env "THEOURGIA_TRACE=1 scheme --script " hold-path
          "  " d4c " > /dev/null 2> " qt " & "
          "j=0; while [ $j -lt 4000 ] && ! grep -qE 'lock-wait|enter-critical' " qt
          "  2>/dev/null; do j=$((j+1)); done; "
          "echo EARLY $(grep -c enter-critical " qt "); "
          "echo WAITED $(grep -c lock-wait " qt "); "
          "printf x > " gate "; wait"))))
(define (field-in report name)
  (let loop ((i 0) (start 0))
    (cond
      ((>= i (string-length report)) 'not-found)
      ((char=? (string-ref report i) #\newline)
       (let ((line (substring report start i)))
         (if (and (>= (string-length line) (string-length name))
                  (string=? (substring line 0 (string-length name)) name))
             (string->number (substring line (+ 1 (string-length name)) (string-length line)))
             (loop (+ i 1) (+ i 1)))))
      (else (loop (+ i 1) start)))))
(define locked-report (two-writers "locked"))
(want "the second writer waits at the door while the first is inside"
      (list (field-in locked-report "WAITED") (field-in locked-report "EARLY"))
      (list 1 0))
;; THE REVERSE. A lock leaves no trace in any answer, so the row above
;; is green for a build that never takes one. This is the run that says
;; it is not: with the product's lock removed and the barrier kept, the
;; second writer walks in while the first is still parked inside.
(define noflock-report (two-writers "noflock"))
(want "with the lock removed the second writer gets in early and never waits"
      (list (field-in noflock-report "EARLY") (field-in noflock-report "WAITED"))
      (list 1 0))

(printf "== P8: a mirrored writer, through the command line ==\n")
;; THE fx2w LAYOUT: the local writer plus a second writer's segment with
;; a published manifest beside it. The mirror is read-only here -- this
;; machine never appends to it -- and the point is that its blocks are
;; ordinary blocks to every reading verb.
(define d8 (test-dir "cli1mirror"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home8"))
(run d8 "init")
(run d8 "insert --under root --title Local")
(define w8 (writer-of d8))
(define M "mirrorz9")
(define (mirror-bytes records)
  (let* ((bs (map (lambda (e)
                    (encode-record (car e) (+ 1757300000000 (car e))
                                   "agent:claude" (cadr e) (storable-encode (caddr e))))
                  records))
         (n (apply + (map bytevector-length bs)))
         (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs)
          o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(system (string-append "mkdir -p " d8 "/writers/" M))
(define seg1
  (mirror-bytes (list (list 1 '() (list 'put (list (cons 'kind 'section)
                                                   (cons 'title "FromMirror"))))
                      (list 2 '() (list 'set (string-append M ".1") 'note "m2")))))
(call-with-port (open-file-output-port (string-append d8 "/writers/" M "/000001.sexp")
                                       (file-options no-fail))
  (lambda (p) (put-bytevector p seg1)))
(write-manifest! d8 M (list (manifest-entry 1 (bytevector->hex (sha256 seg1)) seg1)))
;; BOTH BLOCKS SIT AT ROOT WITH ORD 0 -- the local insert took the
;; first place in an empty list, and the mirror's put declared no
;; position at all -- so the tie is broken by block id, and the local
;; writer's id is GENERATED AT INIT. Writing the order out by hand makes
;; this row a coin flip that passes on about half the runs: it did pass,
;; then failed on the next run for a store whose writer happened to sort
;; the other way. The order is derived from the same rule the product
;; uses instead.
(want "the outline lists the mirror's block beside the local one, tie broken by id"
      (out-of (run d8 "outline"))
      (let* ((local (string-append w8 ".1"))
             (mirror (string-append M ".1"))
             (first (if (string<? local mirror) local mirror))
             (second (if (string<? local mirror) mirror local))
             (label (lambda (id) (if (string=? id local) "Local" "FromMirror"))))
        (string-append "- " first "  " (label first) "\n"
                       "- " second "  " (label second) "\n")))
(want "and read answers for a mirrored block like any other"
      (let ((r (run d8 (string-append "read " M ".1"))))
        (list (code-of r)
              (cdr (assq 'title (cdr (assq 'fields (cadr (datum-of r))))))
              (cdr (assq 'note (cdr (assq 'fields (cadr (datum-of r))))))))
      (list 0 "FromMirror" "m2"))
;; A SEGMENT THE MANIFEST DOES NOT LIST IS NOT PART OF THE HISTORY. It
;; is on disk and readable, and it is ignored -- publication is what
;; makes a mirrored segment count, not the file being there.
(define seg2
  (mirror-bytes (list (list 3 '() (list 'set (string-append M ".1") 'title "Unpublished")))))
(call-with-port (open-file-output-port (string-append d8 "/writers/" M "/000002.sexp")
                                       (file-options no-fail))
  (lambda (p) (put-bytevector p seg2)))
(want "an unlisted segment is ignored, and the title is still the published one"
      (let ((r (run d8 (string-append "read " M ".1"))))
        (list (code-of r)
              (cdr (assq 'title (cdr (assq 'fields (cadr (datum-of r))))))))
      (list 0 "FromMirror"))
(want "CONTROL: listing it makes it count"
      (begin
        (write-manifest! d8 M (list (manifest-entry 1 (bytevector->hex (sha256 seg1)) seg1)
                                    (manifest-entry 2 (bytevector->hex (sha256 seg2)) seg2)))
        (let ((r (run d8 (string-append "read " M ".1"))))
          (list (code-of r)
                (cdr (assq 'title (cdr (assq 'fields (cadr (datum-of r)))))))))
      (list 0 "Unpublished"))
;; AND A LOCAL WRITE UNDER A MIRRORED BLOCK DECLARES WHAT IT HAD SEEN OF
;; THAT WRITER. Without the dep a reader cannot tell whether this block
;; was placed knowing the mirror's history or in ignorance of it.
(want "an insert under a mirrored block succeeds and names the mirror's last sequence"
      (let ((r (run d8 (string-append "insert --under " M ".1 --title Under"))))
        (list (code-of r) (car (datum-of r))))
      (list 0 'ok))
(want "and the record on disk carries that dependency"
      (let* ((text (slurp (string-append d8 "/writers/" w8 "/000001.sexp")))
             (n (string-length text))
             (second (let loop ((i 0) (start 0) (k 0))
                       (cond ((>= i n) 'no-record)
                             ((char=? (string-ref text i) #\newline)
                              (if (= k 1)
                                  (decode-line (string->utf8 (substring text start (+ i 1))))
                                  (loop (+ i 1) (+ i 1) (+ k 1))))
                             (else (loop (+ i 1) start k))))))
        (if (and (pair? second) (eq? (car second) 'ok)) (list-ref second 4) second))
      (list (cons M 3)))

(printf "== P9: the lock is released on the failing path too ==\n")
;; A WRITE THAT FAILS STILL HELD THE LOCK WHILE IT RAN. If the failure
;; path returns without releasing, the store stays locked for as long as
;; that process lives -- and a process that exits immediately after
;; hides it, because closing the file descriptor releases the lock
;; anyway. So the child fails a write and then STAYS ALIVE, parked on a
;; fifo, while a second process tries to write.
(define d9 (test-dir "cli1lockrelease"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home9"))
(run d9 "init")
(run d9 "insert --under root --title Seed")
(define fail-path (string-append scratch "/failer.ss"))
(write-file! fail-path
  (string-append
    "#!r6rs\n(import (chezscheme) (theourgia store))\n"
    "(define a (cdr (command-line)))\n"
    "(define answer\n"
    "  (with-store-write (car a)\n"
    "    (lambda (st v) '((insert \"nosuch.9\" #f ((kind . section) (title . \"x\")))))))\n"
    "(display (car (car answer))) (newline)\n"
    ";; still alive, and still holding whatever it did not release\n"
    "(let ((p (open-file-input-port (cadr a)))) (get-u8 p) (close-port p))\n"))
(define release-report
  (let ((gate (string-append scratch "/gate9"))
        (fo (string-append scratch "/failer.out"))
        (so (string-append scratch "/second.out")))
    (sh (string-append
          "rm -f " gate " " fo " " so "; mkfifo " gate "; "
          "env -u THEOURGIA_INJECT scheme --script " fail-path " " d9 " " gate " > " fo " 2>/dev/null & "
          "i=0; while [ $i -lt 8000 ] && [ ! -s " fo " ]; do i=$((i+1)); done; "
          "env -u THEOURGIA_INJECT scheme --script " cli
          "  insert --under root --title Second --store " d9 " > " so " 2>/dev/null; "
          "echo SECOND $?; "
          "echo FAILER $(head -1 " fo "); "
          "printf x > " gate "; wait"))))
(define (release-field name)
  (let loop ((i 0) (start 0))
    (cond
      ((>= i (string-length release-report)) 'not-found)
      ((char=? (string-ref release-report i) #\newline)
       (let ((line (substring release-report start i)))
         (if (and (>= (string-length line) (string-length name))
                  (string=? (substring line 0 (string-length name)) name))
             (substring line (+ 1 (string-length name)) (string-length line))
             (loop (+ i 1) (+ i 1)))))
      (else (loop (+ i 1) start)))))
(want "CONTROL: the first process really did fail its write and is still running"
      (release-field "FAILER")
      "error")
(want "a second process can write while the failed one is still alive"
      (release-field "SECOND")
      "0")
(want "and the second write is on disk"
      (let ((r (run d9 "outline")))
        (list (code-of r)
              (let count ((i 0) (n 0))
                (cond ((>= i (string-length (out-of r))) n)
                      ((char=? (string-ref (out-of r) i) #\newline) (count (+ i 1) (+ n 1)))
                      (else (count (+ i 1) n))))))
      (list 0 2))

(printf "== P9: a damaged local writer refuses to be written to ==\n")
;; A BAD LINE FOLLOWED BY A GOOD ONE. A torn tail at the very end is an
;; ordinary crash and is truncated; damage with valid records AFTER it
;; is not something this writer can reason about, so it stops writing
;; and says what the remedy is rather than appending past a hole.
(define d9b (test-dir "cli1damaged"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home9b"))
(run d9b "init")
(run d9b "insert --under root --title One")
(run d9b "insert --under root --title Two")
(define w9 (writer-of d9b))
(define seg-path (string-append d9b "/writers/" w9 "/000001.sexp"))
(define before-damage-registry (slurp (string-append scratch "/home9b/instances.sexp")))
(system (string-append "printf 'deadbeef (9 1 \"a\" () (put ()))\\n' >> " seg-path))
(define good-tail
  (mirror-bytes (list (list 3 '() (list 'set (string-append w9 ".1") 'title "after-hole")))))
(system (string-append "cat >> " seg-path " <<'EOF'\n" (utf8->string good-tail) "EOF\n"))
(define damaged-bytes (slurp seg-path))
(define damaged-write (run d9b "insert --under root --title Three"))
(want "a write into a damaged writer is refused and names adopt as the remedy"
      (list (> (code-of damaged-write) 0)
            (car (datum-of damaged-write))
            (cadr (datum-of damaged-write))
            (caddr (datum-of damaged-write))
            (cadddr (datum-of damaged-write)))
      (list #t 'error 'refused 'integrity '(remedy adopt)))
(want "and neither the log nor the registry moved"
      (list (equal? damaged-bytes (slurp seg-path))
            (equal? before-damage-registry
                    (slurp (string-append scratch "/home9b/instances.sexp"))))
      (list #t #t))

(printf "== who the record says wrote it ==\n")
;; A PUBLISHED LIBRARY MUST NOT PUT ANYBODY'S NAME IN ITS DEFAULT. The
;; actor was a constant in the source, so every record every user ever
;; wrote carried it. It comes from the caller now, in an order the
;; caller can override, and the fallback is a program's name rather than
;; a person's.
(define da (test-dir "cli1actor"))
(putenv "THEOURGIA_HOME" (string-append scratch "/homea"))
(run da "init")
(define (actor-run title extra)
  (let ((out (string-append scratch "/a.out"))
        (err (string-append scratch "/a.err")))
    (system (string-append "{ " extra " env -u THEOURGIA_INJECT scheme --script " cli
                           " insert --under root --title " title
                           " --store " da " ; } > " out " 2> " err))
    (slurp out)))
(define (actors-on-disk)
  (let* ((w (writer-of da))
         (text (slurp (string-append da "/writers/" w "/000001.sexp"))))
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i (string-length text)) (reverse out))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (cons (if (and (pair? r) (eq? (car r) 'ok)) (list-ref r 3) r) out))))
        (else (loop (+ i 1) start out))))))
(actor-run "A" "")
(actor-run "B" "THEOURGIA_ACTOR=from-env")
(want "an explicit actor beats the environment, which beats the account"
      (begin
        (system (string-append "{ env -u THEOURGIA_INJECT THEOURGIA_ACTOR=loses scheme --script "
                               cli " insert --under root --title C --actor wins --store " da
                               " ; } > /dev/null 2>&1"))
        (actors-on-disk))
      (list (or (getenv "USER") "cli") "from-env" "wins"))
;; AND WITH NOTHING AT ALL TO GO ON it writes a program's name. The row
;; above cannot show this: the account is almost always set.
(want "with no flag, no variable and no account, the actor is the program"
      (begin
        (system (string-append "{ env -u THEOURGIA_INJECT -u THEOURGIA_ACTOR -u USER scheme --script "
                               cli " insert --under root --title D --store " da
                               " ; } > /dev/null 2>&1"))
        (car (reverse (actors-on-disk))))
      "cli")
;; AND NO NAME OF ANY TOOL APPEARS IN ANY OF THEM.
(define (holds? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))
(want "CONTROL: nothing on disk carries a hard-coded agent name"
      (filter (lambda (a) (and (string? a) (holds? a "agent:"))) (actors-on-disk))
      '())

(printf "== A4: the outline is exact, and A5: a section reads back whole ==\n")
;; FULL TITLES, NOT ABBREVIATED ONES. An outline that truncated would
;; still list the right blocks in the right order -- so the titles are
;; compared in full, and they are CJK because a width-based truncation
;; would cut them at a different place than an ASCII one.
(define d10 (test-dir "cli1outline2"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home10"))
(run d10 "init")
(write-file! (string-append scratch "/corpus.md")
  (string-append
    "# \x7B2C;\x4E00;\x7AE0; \x5E8F;\x8BBA;\nlead paragraph\n"
    "## 1.1 \x80CC;\x666F;\nbackground body\n"
    "### 1.1.1 \x7EC6;\x8282;\ndetail body\n"
    "# \x7B2C;\x4E8C;\x7AE0;\nsecond chapter body\n"))
(system (string-append "mkdir -p " scratch "/corpus && cp " scratch "/corpus.md "
                       scratch "/corpus/a.md"))
(run d10 (string-append "import-md " scratch "/corpus"))
(define w10 (writer-of d10))
(want "the full outline lists every block, indented, with whole titles"
      (out-of (run d10 "outline"))
      (string-append
        "- " w10 ".1  a.md\n"
        "  - " w10 ".2  \x7B2C;\x4E00;\x7AE0; \x5E8F;\x8BBA;\n"
        "    - " w10 ".3  1.1 \x80CC;\x666F;\n"
        "      - " w10 ".4  1.1.1 \x7EC6;\x8282;\n"
        "  - " w10 ".5  \x7B2C;\x4E8C;\x7AE0;\n"))
;; A DEPTH LIMIT STOPS THE WALK. The deeper blocks are still there; the
;; listing simply does not go down to them.
(want "depth two stops before the grandchildren"
      (out-of (run d10 "outline --depth 2"))
      (string-append
        "- " w10 ".1  a.md\n"
        "  - " w10 ".2  \x7B2C;\x4E00;\x7AE0; \x5E8F;\x8BBA;\n"
        "  - " w10 ".5  \x7B2C;\x4E8C;\x7AE0;\n"))
(want "CONTROL: the blocks the limit hid are still in the store"
      (list (code-of (run d10 (string-append "read " w10 ".4")))
            (code-of (run d10 (string-append "read " w10 ".3"))))
      (list 0 0))
;; A5: EACH SECTION READS BACK AS ITS OWN BYTES, and the neighbour's do
;; not bleed in. A reader that returned the whole file, or a summary,
;; would pass a test that only checked the section was found.
(want "each section reads back exactly, with nothing of its neighbour"
      (list (out-of (run d10 (string-append "read " w10 ".3 --md")))
            (out-of (run d10 (string-append "read " w10 ".4 --md")))
            (out-of (run d10 (string-append "read " w10 ".5 --md"))))
      (list "## 1.1 \x80CC;\x666F;\nbackground body\n"
            "### 1.1.1 \x7EC6;\x8282;\ndetail body\n"
            "# \x7B2C;\x4E8C;\x7AE0;\nsecond chapter body\n"))
(want "and the S-expression read gives the fields, not a summary"
      (let* ((r (run d10 (string-append "read " w10 ".5")))
             (b (cadr (datum-of r)))
             (fs (cdr (assq 'fields b))))
        (list (code-of r) (cdr (assq 'title fs)) (cdr (assq 'kind fs))))
      (list 0 "\x7B2C;\x4E8C;\x7AE0;" 'section))

(printf "== the snapshot verb ==\n")
(define d11 (test-dir "cli1snapshot"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home11"))
(run d11 "init")
(system (string-append "mkdir -p " scratch "/snapsrc"))
(write-file! (string-append scratch "/snapsrc/a.md") "# A\nbody\n## B\nmore\n")
(run d11 (string-append "import-md " scratch "/snapsrc"))
(define snap-run (run d11 "snapshot"))
(want "it answers with the file it wrote and the cut it froze"
      (list (code-of snap-run)
            (car (datum-of snap-run))
            (car (cadr (datum-of snap-run)))
            (car (caddr (datum-of snap-run))))
      (list 0 'ok 'snapshot 'cut))
(want "the cut is the one the store's own reduction reached"
      (cadr (caddr (datum-of snap-run)))
      (reduce-applied-cut (open-and-reduce d11)))
;; A SECOND SNAPSHOT DOES NOT REPLACE THE FIRST. Selection falls back to
;; older ones when the newest turns out to be unusable, so overwriting
;; would throw away the fallback that makes the fallback possible.
(want "a second one is a new file beside it"
      (begin (run d11 "snapshot")
             (list-sort string<? (directory-list (string-append d11 "/snap"))))
      '("000001.sexp" "000002.sexp"))
;; AND THE STORE READS THE SAME AFTERWARDS. A snapshot is an
;; optimisation; if taking one changed an answer it would be a write.
(want "the outline is unchanged by having taken one"
      (out-of (run d11 "outline"))
      (let ((w (writer-of d11)))
        (string-append "- " w ".1  a.md\n"
                       "  - " w ".2  A\n"
                       "    - " w ".3  B\n")))
;; THE SNAPSHOT IS DOING THE WORK. Without this the rows above are green
;; for a store that writes snapshot files and then ignores them.
(want "reopening replays nothing, because the snapshot covers it all"
      (length (reduce-trace (open-and-reduce d11)))
      0)
(want "a store that was never initialised cannot be snapshotted"
      (let ((r (run (test-dir "cli1snapnostore") "snapshot")))
        (list (> (code-of r) 0) (car (datum-of r)) (cadr (datum-of r))))
      (list #t 'error 'no-store))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "cli1 complete\n")
