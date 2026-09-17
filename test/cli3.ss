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

;; The read verbs: refs and search.
;;
;; These ask the store questions and change nothing, so what there is to
;; get wrong is what counts as an answer. Two mistakes are easy and both
;; are quiet: listing a block's own out-edges as references TO it, and
;; scoring every hit the same so that the order comes from wherever the
;; blocks happened to be stored.
;;
;; A REFERENCE LIVES IN TWO PLACES AND THEY ARE NOT THE SAME PLACE. A
;; link record is an edge someone wrote and `unlink` removes it; a
;; reference in the text is a sentence, and nothing removes it but
;; editing the sentence. Each line says which it came from. That is why
;; the unlink row below still expects one line rather than none.
;;
;; EMPTY IS AN ANSWER. No references and no hits both print nothing and
;; exit zero; an error would say the question could not be asked, which
;; is a different fact.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia ffi) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia wire) string->sexpr-extended))

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
;; answer apart -- `(assq 'cursor (cdr a))`, `(caddr ...)` -- and a
;; mutant that changes the answer's SHAPE makes the accessor raise while
;; the row's value is being computed, outside anything that was
;; catching. The file then ends where it stood.
;;
;; WHAT THAT COSTS IS NOT ONE ROW. Every row below the raise goes unrun,
;; and the runner sees no `FAIL` at all: the seeded-defect round reported
;; `0 FAIL, sentinel=False` and scored it as a crash rather than a kill,
;; for three separate defects that the store had in fact answered
;; correctly and visibly. The rows that would have caught them were
;; further down the file.
;;
;; SO THE GUARD GOES WHERE EVERY ROW PASSES THROUGH, and `got` is
;; evaluated inside it. This is a macro rather than a procedure for that
;; one reason: an argument is evaluated before the call, so a procedure
;; could not have guarded it.
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
      (else (assertion-violation 'cli3
              "cli.ss is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.ss sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "cli3 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))



(define scratch (test-dir "cli3"))
(define home (string-append scratch "/home"))

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

(define out-path (string-append scratch "/out.txt"))
(define err-path (string-append scratch "/err.txt"))

;; ONE PLACE THAT RUNS THE PROGRAM, and it answers with the exit code and
;; the LINES it printed. A verb that prints one item per line is judged
;; line by line; collapsing the output into one datum would let a row
;; pass for output in a different order.
(define (run store . args)
  (let* ((cmd (string-append
                "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                "THEOURGIA_HOME=" home " "
                "scheme --script " cli " "
                (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                "--store " store " > " out-path " 2> " err-path))
         (code (system cmd))
         (text (text-of out-path)))
    (list code
          (let loop ((i 0) (start 0) (out '()))
            (cond
              ((>= i (string-length text)) (reverse out))
              ((char=? (string-ref text i) #\newline)
               (let ((line (substring text start i)))
                 (loop (+ i 1) (+ i 1)
                       (if (= 0 (string-length line))
                           out
                           (cons (guard (e (#t (list 'unreadable line)))
                                   (read (open-string-input-port line)))
                                 out)))))
              (else (loop (+ i 1) start out)))))))

(define (code-of r) (car r))
(define (lines-of r) (cadr r))

(define (init! d) (run d "init"))
(define (writer-of d)
  ;; the id of the first block tells us the writer name the store minted
  (let ((ls (lines-of (run d "outline"))))
    (if (null? ls) 'no-blocks 'see-ids)))

;; THE ID COMES BACK FROM THE INSERT, never from counting inserts. A
;; block id is <writer>.<sequence> and the sequence counts RECORDS, so a
;; `set` between two inserts moves the next block's id -- ids are not
;; consecutive and an expectation that assumes they are is pinned to
;; something the product never promised.
(define (insert! d . args)
  (apply run d "insert" "--under" "root" args)
  (let* ((datum (read (open-string-input-port (text-of out-path))))
         (state (cadr (assq 'state (cdr datum)))))
    (car (car state))))

(printf "== N1: refs lists what points at a block, and says from where ==\n")
(define d1 (fresh-store!))
(init! d1)
(define t1 (insert! d1 "--title" "target"))
(define t2 (insert! d1 "--title" "source one"))
(define t3 (insert! d1 "--title" "source two"))
(run d1 "set" t2 "src" (string-append "see [[" t1 "]] for more"))
(run d1 "link" t3 "mentions" t1)
;; AND AN OUT-EDGE OF THE TARGET ITSELF, which must not be reported as a
;; reference TO it. An implementation listing every edge that mentions
;; the id would pass every other row here and fail only this one.
(run d1 "link" t1 "mentions" t3)
;; The nearest-id row needs an id this writer does not have; the writer
;; is whatever minted t1, and the sequence is past every record written.
(define missing (string-append (let loop ((i 0))
                                 (cond ((>= i (string-length t1)) t1)
                                       ((char=? (string-ref t1 i) #\.) (substring t1 0 i))
                                       (else (loop (+ i 1)))))
                               ".99"))

(want "both kinds of reference are listed, each saying where it came from"
      (run d1 "refs" t1)
      (list 0 (list (list 'ref (list 'from t2) (list 'rel 'ref) (list 'via 'md))
                    (list 'ref (list 'from t3) (list 'rel 'mentions) (list 'via 'link)))))
(want "a block's own out-edge is not a reference to it"
      (lines-of (run d1 "refs" t3))
      (list (list 'ref (list 'from t1) (list 'rel 'mentions) (list 'via 'link))))
;; THE EDGE GOES, THE SENTENCE STAYS. One line is the whole point of the
;; row: an implementation that merged the two sources would print none.
(want "after unlink the written edge is gone and the sentence is not"
      (begin (run d1 "unlink" t3 "mentions" t1)
             (run d1 "refs" t1))
      (list 0 (list (list 'ref (list 'from t2) (list 'rel 'ref) (list 'via 'md)))))
(want "TWIN: a block nothing points at answers with nothing, and succeeds"
      (run d1 "refs" t2)
      (list 0 '()))
;; THE NEAREST IDS ARE COMPUTED FROM THE RULE, not read back: the store
;; holds .1 .2 .3, the request is .9, so the distances are 8, 7, 6 and
;; the order is .3 .2 .1.
(want "an unknown id is an error that names the nearest ids"
      (run d1 "refs" missing)
      (list 1 (list (list 'error 'unknown-id missing
                          (list 'nearest (list t3 t2 t1))))))

(printf "\n== N2: search scores by field and orders totally ==\n")
(define d2 (fresh-store!))
(init! d2)
;; FOUR BLOCKS: one hit in the title only, one in the src only, one in
;; both, and one that ties with another on score while sorting before it
;; by id -- the tie is what an implementation with a constant score, or
;; with hash-table order, gets wrong. Every id is taken from the answer
;; that created it.
(define e1 (insert! d2 "--title" "cat first inserted"))
(define e2 (insert! d2 "--title" "Concatenate strings"))
(define e3 (insert! d2 "--title" "plain heading"))
(define e4 (insert! d2 "--title" "cat and dog"))
(run d2 "set" e1 "src" "nothing to find here")
(run d2 "set" e3 "src" "we concatenate in the body")
(run d2 "set" e4 "src" "the cat sat on the mat")
;; CONTROL: e1 and e2 both score 2 for `cat`, and e1 was inserted first,
;; so it also sorts first by id -- which would hide an ordering defect.
;; The row below therefore also asserts that e2 follows e1, and this
;; control says the two facts are independent: the tie is real.
(want "CONTROL: the two blocks that tie were created in id order"
      (string<? e1 e2)
      #t)

(want "title hits score 2, src hits 1, both 3, and equal scores go by id"
      (lines-of (run d2 "search" "cat"))
      (list (list 'hit e4 3 "cat and dog")
            (list 'hit e1 2 "cat first inserted")
            (list 'hit e2 2 "Concatenate strings")
            (list 'hit e3 1 "we concatenate in the body")))
(want "a hit is a case-insensitive substring, so cat finds concatenate"
      (lines-of (run d2 "search" "CAT"))
      (lines-of (run d2 "search" "cat")))
(want "every token must hit: one that matches nothing empties the answer"
      (run d2 "search" "cat zzzzz")
      (list 0 '()))
(want "TWIN: two tokens that both hit the same block keep it"
      (lines-of (run d2 "search" "cat mat"))
      (list (list 'hit e4 3 "cat and dog")))
;; A QUERY IS TEXT. This one would be an enormous exact integer if any
;; part of the path handed it to a numeric parser, and the row would not
;; return rather than returning empty.
(want "a query that looks like a number is matched as text, and returns"
      (run d2 "search" "#e1e99999999")
      (list 0 '()))
(want "no hits is an answer, not an error"
      (run d2 "search" "zzzzzzzz")
      (list 0 '()))


(printf "\n== N3: log lists what was applied, in delivery order ==\n")
;; ONLY THE RECORDS THE INTENT NAMES. Two near misses are the point of
;; this section: a record whose DEPS name a block has not touched it, and
;; neither has one whose TEXT mentions it. Both would make `log <id>`
;; list records that never changed the block, and both are easy to write
;; by accident -- the first by walking deps, the second by grepping src.
(define d3 (fresh-store!))
(init! d3)
(define g1 (insert! d3 "--title" "first"))
(define g2 (insert! d3 "--title" "second"))
(define (verb-of line) (cadr (assq 'verb (cdr line))))
(define (event-of line) (cdr (assq 'event (cdr line))))
(run d3 "set" g1 "title" "first renamed")
(run d3 "link" g1 "mentions" g2)
;; A SET ON g2 WHOSE TEXT NAMES g1. It touches g2 and not g1.
(run d3 "set" g2 "src" (string-append "a mention of " g1 " in text only"))

(want "every applied record is listed once, oldest first"
      (map verb-of (lines-of (run d3 "log")))
      '(put put set link set))
(want "with an id, only the records that named that block"
      (map verb-of (lines-of (run d3 "log" g1)))
      '(put set link))
;; THE EXCLUSION, STATED AS ITS OWN ROW: the last record mentions g1 in
;; its text and is not listed for g1, while it IS listed for g2.
(want "a record whose text merely mentions the id is not a record about it"
      (list (length (lines-of (run d3 "log" g1)))
            (map verb-of (lines-of (run d3 "log" g2))))
      (list 3 '(put link set)))
(want "an unknown id is refused, not answered with an empty log"
      (code-of (run d3 "log" missing))
      1)
;; TWIN: the answer is the same on a second run, so nothing here depends
;; on a traversal order that could differ between processes.
(want "TWIN: two runs of the same store give the same log"
      (lines-of (run d3 "log"))
      (lines-of (run d3 "log")))

;; THE OTHER NEAR MISS NEEDS A SECOND WRITER. A lone local writer's
;; records carry no deps -- the deps are the applied cut without itself --
;; so the "deps name it but the intent does not" case cannot arise in the
;; store above. A published mirror segment supplies one: every local
;; record written afterwards names the mirror's sequence as a premise,
;; while touching only its own block.
(define d4 (fresh-store!))
(init! d4)
(define mirror "mirrorz9")
(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define mirror-bytes (rec 1 '(put ((kind . section) (title . "from the mirror")))))
(define mirror-block (string-append mirror ".1"))
(define cand (string-append scratch "/mirror.bin"))
(put! cand mirror-bytes)
(want "CONTROL: the mirror's segment publishes"
      (car (lines-of (run d4 "publish" mirror "1" cand)))
      '(ok (published 1)))
(define h1 (insert! d4 "--title" "local block"))
(run d4 "set" h1 "title" "renamed after the mirror arrived")
(want "CONTROL: the local record really does name the mirror as a premise"
      (let* ((ls (log-open d4))
             (deps (let ((found (vector '())))
                     (load-deliver! ls '()
                       (lambda (w seg off seq ts actor deps payload)
                         (when (and (not (string=? w mirror)) (pair? deps))
                           (vector-set! found 0 (cons deps (vector-ref found 0))))
                         'applied))
                     (load-commit! ls)
                     (vector-ref found 0))))
        (and (pair? deps) (equal? (car (car (car deps))) mirror)))
      #t)
(want "a record whose deps name the block is not a record about it"
      (map verb-of (lines-of (run d4 "log" mirror-block)))
      '(put))

(printf "\n== N4: a tag names the cut this store has applied ==\n")
;; THE APPLIED CUT, NOT THE DISCOVERED FRONTIER. A name pointing at
;; records the reducer has not applied would name a state no reader of
;; this store could produce.
;;
;; AND THE BINDING IS HISTORY. The cut is taken before the tag record
;; exists, so a tag is never inside the cut it binds, and an edit written
;; afterwards does not move it -- an implementation that recomputed the
;; cut when listing would pass every other row and fail these two.
(define d5 (fresh-store!))
(init! d5)
(define k1 (insert! d5 "--title" "before the tag"))
(define (cut-of line) (cadr (assq 'cut (cdr line))))
(define (name-of line) (cadr (assq 'name (cdr line))))
(define (writer-of-id id)
  (let loop ((i 0))
    (cond ((>= i (string-length id)) id)
          ((char=? (string-ref id i) #\.) (substring id 0 i))
          (else (loop (+ i 1))))))
(run d5 "tag" "v1")
(define cut-at-tag (cut-of (car (lines-of (run d5 "tag")))))
(want "the tag names the sequence written before it, not its own"
      (equal? cut-at-tag (list (cons (writer-of-id k1) 1)))
      #t)
(want "an edit written afterwards does not move the tag"
      (begin (insert! d5 "--title" "after the tag")
             (cut-of (car (lines-of (run d5 "tag")))))
      cut-at-tag)
;; LATER WINS, CAUSALLY. The second tag was written by a session that had
;; seen the first, so it supersedes it -- and both records stay in the
;; log, because a tag is not a uniqueness constraint.
(want "the same name written again, having seen the first, supersedes it"
      (begin (run d5 "tag" "v1")
             (let ((ls (lines-of (run d5 "tag"))))
               (list (length ls) (equal? (cut-of (car ls)) cut-at-tag))))
      (list 1 #f))
(want "and both tag records are still in the log"
      (length (filter (lambda (l) (eq? 'tag (verb-of l))) (lines-of (run d5 "log"))))
      2)
(want "names are listed in name order"
      (begin (run d5 "tag" "a-first")
             (map name-of (lines-of (run d5 "tag"))))
      (list "a-first" "v1"))

(printf "\n== N5: diff compares two states, each read to its own cut ==\n")
;; NOT THE CURRENT STATE WITH SOMETHING SUBTRACTED. A cut names what had
;; been applied at a moment, and the only way to know what the store said
;; then is to read it then -- the edit made after t1 below is the row
;; that catches an implementation which diffs against now.
;;
;; THE ENDPOINTS ARE NOT SYMMETRIC, and the reverse direction swaps added
;; for removed while leaving `changed` alone.
(define d6 (fresh-store!))
(init! d6)
(define p1 (insert! d6 "--title" "A"))
(define p2 (insert! d6 "--title" "B"))
(run d6 "tag" "t0")
(run d6 "set" p1 "title" "A renamed")
(run d6 "del" p2)
(define p3 (insert! d6 "--title" "C"))
(run d6 "tag" "t1")
;; written after t1, and so outside both cuts
(run d6 "set" p1 "src" "changed after t1")

(want "a retitle, a delete and an insert, and nothing from after the cut"
      (lines-of (run d6 "diff" "t0" "t1"))
      (list (list 'changed p1 'title) (list 'removed p2) (list 'added p3)))
(want "the reverse direction swaps added and removed"
      (lines-of (run d6 "diff" "t1" "t0"))
      (list (list 'changed p1 'title) (list 'added p2) (list 'removed p3)))
(want "TWIN: a cut against itself has no differences, and succeeds"
      (run d6 "diff" "t0" "t0")
      (list 0 '()))
;; AN EDGE IS A CHANGE. An implementation comparing only the scalar
;; fields calls these two states identical.
(want "adding one edge and nothing else is a change to that block"
      (begin (run d6 "tag" "t2")
             (run d6 "link" p1 "mentions" p3)
             (run d6 "tag" "t3")
             (lines-of (run d6 "diff" "t2" "t3")))
      (list (list 'changed p1 'links)))
(want "a move shows as both of the coordinates it moves"
      (begin (run d6 "tag" "t4")
             (run d6 "move" p3 p1)
             (run d6 "tag" "t5")
             (lines-of (run d6 "diff" "t4" "t5")))
      (list (list 'changed p3 'parent) (list 'changed p3 'ord)))
;; A CUT THIS STORE CANNOT REACH IS REFUSED BY NAME. Answering with the
;; part it could reach would be a diff against a moment that never was.
(want "an unreachable cut is refused, not silently truncated"
      (run d6 "diff" "t0" "((\"nobody\" . 99))")
      (list 1 (list (list 'error 'cut-unavailable (list 'cut 'to)
                          (list 'reason 'not-received)))))
(want "an unknown tag says which side it was on"
      (lines-of (run d6 "diff" "nosuch" "t0"))
      (list (list 'error 'unknown-tag "nosuch" (list 'cut 'from))))
;; THE LITERAL IS PARSED BY SHAPE. Handed to `read` this argument would
;; ask for an exact integer of ten billion digits and the row would not
;; return at all.
(want "a cut literal that asks for an enormous number is refused at once"
      (let* ((t0 (current-time 'time-monotonic))
             (r (run d6 "diff" "t0" "((\"w\" . #e1e99999999))"))
             (t1 (current-time 'time-monotonic)))
        (list (car r) (< (- (time-second t1) (time-second t0)) 20)))
      (list 1 #t))
(want "TWIN: a well-formed cut literal is accepted"
      (code-of (run d6 "diff" "t0" (string-append "((\"" (writer-of-id p1) "\" . 2))")))
      0)

(printf "\n== N6: what the store holds and cannot show ==\n")
;; THREE SECTIONS, AND A SECTION WITH NOTHING IN IT PRINTS NOTHING. An
;; empty answer therefore means an empty store rather than a verb that
;; declined to look, which is why the first row is a clean store.
(define d7 (fresh-store!))
(init! d7)
(define q1 (insert! d7 "--title" "parent"))
(define q2 (begin (run d7 "insert" "--under" q1 "--title" "child")
                  (let* ((datum (read (open-string-input-port (text-of out-path))))
                         (state (cadr (assq 'state (cdr datum)))))
                    (car (car state)))))
(want "TWIN: a store with nothing wrong answers with nothing, and succeeds"
      (run d7 "conflicts")
      (list 0 '()))
;; A DELETE DOES NOT CASCADE: the child is still readable, it has just
;; lost a place to be shown.
(want "a block whose parent was deleted is an orphan"
      (begin (run d7 "del" q1) (lines-of (run d7 "conflicts")))
      (list (list 'orphan q2)))

;; EVERY MISSING PREMISE, NOT THE FIRST. An implementation that stopped
;; at the first would send an operator to fetch one record and leave them
;; where they started.
(define d8 (fresh-store!))
(init! d8)
(define waiting
  (encode-record 1 1757300001000 "agent:claude"
                 '(("aaaaaaaa" . 2) ("bbbbbbbb" . 3))
                 (storable-encode '(put ((kind . section) (title . "waiting"))))))
(define waiting-file (string-append scratch "/waiting.bin"))
(put! waiting-file waiting)
(want "CONTROL: the record publishes, so what follows is about applying it"
      (car (lines-of (run d8 "publish" "mirrorzz" "1" waiting-file)))
      '(ok (published 1)))
(want "a record waiting on two premises is listed once for each"
      (lines-of (run d8 "conflicts"))
      (list (list 'pending (list 'event "mirrorzz" 1) (list 'missing "aaaaaaaa" 2))
            (list 'pending (list 'event "mirrorzz" 1) (list 'missing "bbbbbbbb" 3))))

;; A POSITION THAT NEVER SETTLED, built by hand rather than read back
;; from the outline: two moves of one block that neither saw the other
;; leave two candidates, and the block cannot be placed under either.
(define d9 (fresh-store!))
(init! d9)
(define r1 (insert! d9 "--title" "root block"))
(define r2 (insert! d9 "--title" "moved by two"))
(define w9 (writer-of-id r1))
;; a mirrored move that names only the creation as its premise, so it is
;; concurrent with the local move written next
(define rival
  (encode-record 1 1757300002000 "agent:claude"
                 (list (cons w9 2))
                 (storable-encode (list 'move r2 r1 5))))
(define rival-file (string-append scratch "/rival.bin"))
(put! rival-file rival)
;; THE LOCAL MOVE IS WRITTEN FIRST and the rival published after it:
;; written the other way round the local move would have the rival in its
;; past and would supersede it, leaving one candidate and no conflict --
;; which is correct behaviour and the wrong construction for this row.
(want "CONTROL: the local move commits and the rival publishes after it"
      (list (code-of (run d9 "move" r2 r1))
            (car (lines-of (run d9 "publish" "mirrorzz" "1" rival-file))))
      (list 0 '(ok (published 1))))
(want "a block moved concurrently by two writers is reported unplaced"
      (filter (lambda (l) (eq? 'conflict (car l))) (lines-of (run d9 "conflicts")))
      (list (list 'conflict r2 'unplaced)))
;; AND THE OUTLINE AGREES. The verb reads the structure from the same
;; place the outline does, so the two cannot come to different answers
;; about which blocks are in a conflict.
(want "and the outline marks the same block"
      (let* ((state (open-and-reduce d9))
             (row (let loop ((rows (state-outline state)))
                    (cond ((null? rows) #f)
                          ((equal? (caddr (car rows)) r2) (car rows))
                          (else (loop (cdr rows)))))))
        (and row (= 4 (length row)) (cadddr row)))
      'unplaced)

(printf "\n== N7: the outline says which blocks are in a conflict ==\n")
;; THE REDUCTION MARKS THESE ROWS AND THE PRINTER USED TO DROP THE MARK,
;; so a block whose position never settled rendered exactly like an
;; ordinary top-level one: the library knew and the command line did not.
;; An ordinary row still has three columns, which is what keeps this from
;; being a mark nobody can distinguish.
(define (outline-lines d)
  (let ((text (begin (run d "outline") (text-of out-path))))
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i (string-length text)) (reverse out))
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ i 1)
               (if (= start i) out (cons (substring text start i) out))))
        (else (loop (+ i 1) start out))))))
(define (ends-with? line suffix)
  (let ((n (string-length line)) (m (string-length suffix)))
    (and (>= n m) (string=? (substring line (- n m) n) suffix))))
(want "the block moved by two writers is marked, and the ordinary one is not"
      (let ((ls (outline-lines d9)))
        (list (length (filter (lambda (l) (ends-with? l "unplaced")) ls))
              (length ls)))
      (list 1 2))

(printf "\n== N8: a snapshot standing ahead of what can be read ==\n")
;; The other half of the inherited-baseline question, and it needs no new
;; code: a snapshot whose cut names records this store can no longer read
;; is refused as unusable, so its baseline is never inherited at all, and
;; the write is refused for integrity rather than proceeding from a
;; state nothing can rebuild.
(define d10 (fresh-store!))
(init! d10)
(define s1 (insert! d10 "--title" "one"))
(insert! d10 "--title" "two")
(insert! d10 "--title" "three")
;; THE SNAPSHOT IS TAKEN OVER ALL THREE, so its cut names the last
;; record -- and the damage below lands INSIDE that cut. Damaging a
;; record written after the snapshot would leave the snapshot perfectly
;; usable, which is correct behaviour and the wrong setup for this row.
(want "CONTROL: the snapshot covers every record written so far"
      (let ((line (car (lines-of (run d10 "snapshot")))))
        (equal? (cadr (assq 'cut (cdr line))) (list (cons (writer-of-id s1) 3))))
      #t)
(define (damage-last! d w)
  (let* ((seg (string-append d "/writers/" w "/" (segment-file-name 1)))
         (bytes (slurp seg))
         (at (- (bytevector-length bytes) 20))
         (o (bytevector-copy bytes)))
    (bytevector-u8-set! o at (if (= 98 (bytevector-u8-ref o at)) 99 98))
    (put! seg o)))
(want "CONTROL: the damage puts the readable end behind the snapshot's cut"
      (begin (damage-last! d10 (writer-of-id s1))
             (let* ((report (text-of (begin (run d10 "check") out-path)))
                    (has (lambda (needle)
                           (let loop ((i 0))
                             (cond ((> (+ i (string-length needle)) (string-length report)) #f)
                                   ((string=? (substring report i (+ i (string-length needle))) needle) #t)
                                   (else (loop (+ i 1))))))))
               (list (has "(end 2)") (has "(unusable "))))
      (list #t #t))
(want "and writing is refused rather than proceeding from a state nothing can rebuild"
      (car (lines-of (run d10 "insert" "--under" "root" "--title" "four")))
      '(error refused integrity (remedy adopt)))

(printf "\n== the batch verb's request identity survives the command line ==\n")
;; STDIN IS ONE ARGUMENT ARRIVING ANOTHER WAY, and the command used to
;; treat it as ALL of them: reading stdin replaced the whole argument
;; list, so `--req` and `--after` were gone before the dispatcher could
;; see them. The verb with the most to gain from a retry was the one verb
;; with no protection, and it was invisible -- the batch ran, the answer
;; said `ok`, and only sending it twice showed anything.
(define (run-piped store text . args)
  (let ((in-path (string-append scratch "/stdin.txt")))
    ;; WRITTEN THROUGH A PORT, NOT THROUGH THE SHELL. The intents contain
    ;; quotes, and a `printf` carrying them would be one escaping mistake
    ;; away from feeding the child something other than what this row
    ;; says it feeds it.
    (system (string-append "rm -f " in-path))
    (call-with-port (open-file-output-port in-path (file-options no-fail)
                                           'block (native-transcoder))
      (lambda (o) (put-string o text)))
    (let* ((cmd (string-append
                  "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                  "THEOURGIA_HOME=" home " "
                  "scheme --script " cli " "
                  (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                  "--store " store " < " in-path " > " out-path " 2> " err-path))
           (code (system cmd))
           (text (text-of out-path)))
      (list code (guard (e (#t (list 'unreadable text)))
                   (read (open-string-input-port text)))))))
(define one-intent "((insert root #f ((kind . section) (title \"B1\"))))")
(define dB (fresh-store!))
(init! dB)
(define WB
  (let ((a (cadr (run-piped dB "" "insert" "--under" "root" "--title" "Zero"))))
    (cadr (assq 'cursor (cdr a)))))
(define cursor-WB (string-append (car WB) ":" (number->string (cdr WB))))
(define (blocks-of d) (length (lines-of (run d "outline"))))
(want "CONTROL: the store has a writer and a cursor to write against"
      (list (string? (car WB)) (integer? (cdr WB)) (> (blocks-of dB) 0))
      '(#t #t #t))
;; THE ROW THE DEFECT HID BEHIND. Executing twice and answering `ok`
;; twice is what an untracked batch does, and it looks exactly like
;; success unless the second answer is examined.
(want "the same batch request twice is executed once"
      (let* ((first (cadr (run-piped dB one-intent "batch" "--req" "r-b" "--cursor" cursor-WB)))
             (n1 (blocks-of dB))
             (second (cadr (run-piped dB one-intent "batch" "--req" "r-b" "--cursor" cursor-WB)))
             (n2 (blocks-of dB)))
        ;; AND THE SECOND ANSWER MUST SAY SO. An unchanged block count
        ;; alone is also what a batch that failed for some other reason
        ;; produces; the word `replay` is the store stating that it
        ;; recognised the request.
        ;;
        ;; THE ANSWER IS NOT TAKEN APART BEFORE ITS SHAPE IS KNOWN. This
        ;; read `(cadr (car (cadr second)))` unconditionally, so an answer
        ;; of `(error bad-request ...)` raised INSIDE the row -- ending
        ;; the file and leaving every later row unrun, which reads as a
        ;; broken fixture rather than as a failed expectation.
        (list (car first)
              (if (and (pair? second) (eq? (car second) 'batch)
                       (pair? (cadr second)) (pair? (car (cadr second))))
                  (cadr (car (cadr second)))
                  (list 'not-a-batch second))
              (= n1 n2)))
      (list 'batch '(replay #t) #t))
;; AND THE PAIRING RULE REACHES THIS PATH TOO. It was enforced for every
;; other verb and silently skipped here, which is the worse half of the
;; same defect: a caller passing only `--req` was told nothing and given
;; nothing.
(want "a batch with an id and no cursor is refused, as everywhere else"
      (cadr (run-piped dB one-intent "batch" "--req" "r-lonely"))
      '(error bad-request req-without-cursor))
;; TWO SOURCES FOR THE INTENTS IS AN ERROR, not a silent choice between
;; them. Appending stdin to the arguments is what makes the options
;; survive, and it is also what makes this case reachable at all.
(want "intents given both as an argument and on stdin is a usage error"
      (cadr (run-piped dB one-intent "batch" one-intent))
      '(usage (batch <intents>)))
;; AND A VERB THAT WOULD IGNORE THE IDENTITY SAYS SO. Accepting it and
;; dropping it is the shape of a fault that arms and never fires: the
;; caller is told nothing and believes it is protected.
(for-each
  (lambda (verb)
    (want (string-append "`" verb "` refuses a request identity it does not track")
          (cadr (run-piped dB "" verb "--req" "r-n" "--cursor" cursor-WB))
          (list 'error 'bad-request 'req-not-tracked (string->symbol verb))))
  (list "snapshot" "adopt" "log" "outline"))
;; AN INTENT THAT IS NOT A FORM IS ANSWERED, NOT RAISED. Anything that
;; reads an intent starts by asking for its head, which raises on a value
;; that is not a pair -- and a raise from inside the library reaches the
;; caller as `(error internal ...)`, which names the condition and says
;; nothing about the input. The text "()" is the shortest way to send
;; one: it is not the empty batch (that is the empty text) but a batch of
;; ONE empty intent, and the two spellings looked alike enough that only
;; one of them was ever tried.
(for-each
  (lambda (case)
    (let ((text (car case)) (want-it (cdr case)))
      (want (string-append "batch intents " text " are answered, not raised")
            (let ((a (cadr (run-piped dB text "batch"))))
              (if (and (pair? a) (eq? (car a) 'batch)) (car (cadr a)) a))
            want-it)))
  ;; AND THE ANSWER NAMES THE RULE. It used to carry only the offending
  ;; intent, which made every rule in the boundary invisible: deleting
  ;; any one of them changed nothing an assertion could see.
  ;; THE REASON DESCRIBES THE INTENT; IT NO LONGER IS THE INTENT. These
  ;; rows expected the offending form back verbatim, which is what made
  ;; a refusal about an unwritable datum unwritable itself. A spelling
  ;; is a string, so it says the same thing and survives the wire.
  (list (cons "()" '(error malformed-intent (intent-not-a-form (spelling "()"))))
        (cons "(())" '(error malformed-intent (intent-not-a-form (spelling "()"))))
        (cons "(1 2)" '(error malformed-intent (verb-not-a-symbol (spelling "1"))))
        (cons "(\"x\")"
              '(error malformed-intent (verb-not-a-symbol (spelling "\"x\""))))))
;; AND A SHORT INTENT IS THE SAME DEFECT ONE ARGUMENT FURTHER IN. Each
;; verb's arm reaches straight for `(cadr i)` or `(cadddr i)`, so
;; `(insert root)` raised where `()` did -- a guard that answered for one
;; arity and raised for the next would have had a comment wider than its
;; check.
(for-each
  (lambda (text)
    (want (string-append "a short intent " text " is answered, not raised")
          (let ((a (cadr (run-piped dB text "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch)) (car (car (cadr a))) a))
          'error))
  (list "((insert))" "((insert root))" "((insert root #f))"
        "((move))" "((move a))" "((del))" "((set a))"
        "((link a))" "((unlink a b))" "((tag))"
        "((expect))" "((expect 1))"))
;; TWIN: AND EVERY VERB AT ITS REAL LENGTH STILL RUNS. The arity table is
;; a list of numbers written beside the arms that read them, and a number
;; one too high would refuse work the product is supposed to do -- which
;; the rows above cannot see, because refusing everything passes them
;; all.
(want "TWIN: a set, a move, a link, an unlink and a del all still execute"
      (let* ((mk (lambda (title)
                   (let ((a (cadr (run-piped dB "" "insert" "--under" "root"
                                             "--title" title))))
                     (car (car (cadr (assq 'state (cdr a))))))))
             (one (mk "T1")) (two (mk "T2"))
             ;; A `set` IS `(set <id> <field-symbol> <value>)` and a link
             ;; is `(link <from> <rel-symbol> <to>)`. Written with an
             ;; alist in the field position instead, this row failed --
             ;; and the failure was the fixture's, which is why it is
             ;; spelled out here rather than left to be rediscovered.
             (batch-text
               (string-append
                 "((set \"" one "\" title \"T1b\")"
                 " (move \"" two "\" root #f)"
                 " (link \"" one "\" rel \"" two "\")"
                 " (unlink \"" one "\" rel \"" two "\")"
                 " (del \"" two "\"))"))
             (a (cadr (run-piped dB batch-text "batch"))))
        ;; AND THE ANSWER IS NOT DEREFERENCED BEFORE IT IS KNOWN TO BE
        ;; ONE. `(map car (cadr a))` on an error answer raises inside the
        ;; row, which ends the fixture and reads as a broken file rather
        ;; than as a failed expectation.
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (list (car a) (map (lambda (x) (and (pair? x) (car x))) (cadr a)))
            (list 'not-a-batch a)))
      '(batch (ok ok ok ok ok)))

;; A POSITION WHOSE TYPE IS FIXED IS PART OF THE SHAPE TOO. Arity alone
;; lets `(set <id> ((title . "x")))` through -- right length, wrong thing
;; in the field position -- and the store raised where it wanted a
;; symbol. A field name and a relation name are the two places a caller
;; writing an intent by hand naturally puts something else, because both
;; read like values rather than like names.
(let* ((mk (lambda (title)
             (let ((a (cadr (run-piped dB "" "insert" "--under" "root"
                                       "--title" title))))
               (car (car (cadr (assq 'state (cdr a))))))))
       (p (mk "P1")) (q (mk "P2")))
  (for-each
    (lambda (text)
      (want (string-append "a fixed-type position holding the wrong type is answered: "
                           (substring text 0 (min 24 (string-length text))))
            (let ((a (cadr (run-piped dB text "batch"))))
              (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                  (car (car (cadr a)))
                  a))
            'error))
    (list (string-append "((set \"" p "\" ((title . \"x\"))))")
          (string-append "((set \"" p "\" \"title\" \"x\"))")
          (string-append "((link \"" p "\" \"notasym\" \"" q "\"))")
          (string-append "((unlink \"" p "\" 7 \"" q "\"))")))
  ;; TWIN: and the same positions holding a symbol still work, so the
  ;; rows above are about the type and not about refusing these verbs.
  (want "TWIN: the same verbs with symbols in those positions execute"
        (let ((a (cadr (run-piped dB
                         (string-append "((set \"" p "\" title \"x\")"
                                        " (link \"" p "\" rel \"" q "\"))")
                         "batch"))))
          (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
              (map (lambda (x) (and (pair? x) (car x))) (cadr a))
              a))
        '(ok ok)))

;; TWIN: THE EMPTY TEXT IS A DIFFERENT THING and still means a batch of
;; no intents at all, which writes nothing and is not an error. Without
;; this row the rows above are also passed by a store that refuses every
;; batch whose text it does not like the look of.
(want "TWIN: empty text is a batch of nothing, not a malformed intent"
      (cadr (run-piped dB "" "batch"))
      '(batch ()))
;; TWIN: and a well-formed intent still runs, so the guard did not become
;; a filter on shapes the product is supposed to accept.
(want "TWIN: a well-formed intent is still executed"
      (let ((a (cadr (run-piped dB one-intent "batch"))))
        (list (car a) (car (car (cadr a)))))
      '(batch ok))

;; TWIN: and the verbs that DO track still take it, so the row above is
;; about which verbs carry an identity rather than about refusing the
;; option everywhere.
(want "TWIN: a tracked verb still accepts the same options"
      (car (cadr (run-piped dB "" "insert" "--under" "root" "--title" "Tracked"
                            "--req" "r-t" "--cursor" cursor-WB)))
      'ok)

;; A TRACKED BATCH OF SEVERAL ITEMS TAKES A DIFFERENT PATH, and every
;; malformed row above is UNTRACKED -- so all of them go through the
;; single-record loop and none of them reaches the one that runs after a
;; receipt has been written. The check was added to both loops and only
;; one of them had a cell; the mutation round is what said so.
;;
;; THE RECEIPT IS ALREADY COMMITTED when this item is read, so a raise
;; here does not merely produce a worse message: it discards the answers
;; of the items that already succeeded and skips the commit that ends
;; the request.
(want "a malformed item in a TRACKED batch of several is answered, not raised"
      (let* ((a (cadr (run-piped dB
                        (string-append "(() (insert root #f ((kind . section) (title \"TB\"))))")
                        "batch" "--req" "r-tb" "--cursor" cursor-WB))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (car (car (cadr a)))
            (list 'not-a-batch (car a) (cadr a))))
      'error)
;; TWIN: and a malformed item AFTER a good one keeps the good one's
;; answer, which is the part a raise used to take away.
(want "TWIN: a good item before a malformed one keeps its answer"
      (let* ((a (cadr (run-piped dB
                        (string-append "((insert root #f ((kind . section) (title \"TC\")))"
                                       " (insert root))")
                        "batch" "--req" "r-tc" "--cursor" cursor-WB))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (map (lambda (x) (and (pair? x) (car x))) (cadr a))
            (list 'not-a-batch a)))
      '(ok error))
;; TWIN: and a tracked batch of several well-formed items still runs.
(want "TWIN: a tracked batch of two good items still executes both"
      (let* ((a (cadr (run-piped dB
                        (string-append "((insert root #f ((kind . section) (title \"TD\")))"
                                       " (insert root #f ((kind . section) (title \"TE\"))))")
                        "batch" "--req" "r-td" "--cursor" cursor-WB))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (map (lambda (x) (and (pair? x) (car x))) (cadr a))
            (list 'not-a-batch a)))
      '(ok ok))

;; A BACK-REFERENCE IS INSIDE AN ARGUMENT THE ARITY CHECK ALREADY
;; COUNTED, so `(insert (from) #f ())` has the right length and still
;; raised one level further in. The rows above cannot reach it: they are
;; about the intent's own shape, and this is about the shape of something
;; the intent carries.
(for-each
  (lambda (text)
    (want (string-append "a malformed back-reference is answered: "
                         (substring text 0 (min 26 (string-length text))))
          (let ((a (cadr (run-piped dB text "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                (car (car (cadr a)))
                a))
          'error))
  (list "((insert (from) #f ()))"
        "((insert (from -1) #f ()))"
        "((insert (from x) #f ()))"
        "((insert (from 0 1) #f ()))"))
;; AND THE ROWS ABOVE KEEP ONLY THE ANSWER'S HEAD, so a store that
;; answered `no-such-intent` for every malformed reference would pass all
;; four AND the absent-item twin below -- and the distinction those two
;; rows exist to draw would be guarded by nothing. The kind is asserted
;; here.
(for-each
  (lambda (text)
    (want (string-append "and it is malformed-intent, not no-such-intent: "
                         (substring text 0 (min 22 (string-length text))))
          (let ((a (cadr (run-piped dB text "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                (cadr (car (cadr a)))
                a))
          'malformed-intent))
  (list "((insert (from) #f ()))"
        "((insert (from x) #f ()))"))
;; TWIN: A WELL-FORMED BACK-REFERENCE NAMING NOTHING is a different
;; answer -- `no-such-intent`, not `malformed-intent` -- because the
;; caller wrote a reference correctly and pointed it at an item that is
;; not there. Collapsing the two would tell an author with a typo in
;; their index the same thing as an author with a typo in their syntax.
(want "TWIN: a well-formed reference to an absent item says so, differently"
      (let ((a (cadr (run-piped dB "((insert (from 9) #f ((kind . section))))" "batch"))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (list (car (car (cadr a))) (cadr (car (cadr a))))
            a))
      '(error no-such-intent))
;; TWIN: and a real back-reference still builds the tree it describes.
;; TWIN: and the reference resolves to the RIGHT block. `(ok ok)` alone
;; is also what a store that resolved `(from 0)` to `root` produces --
;; two successful inserts and the wrong tree. The row therefore reads the
;; second block back and compares its parent with the first block's id.
(want "TWIN: a back-reference resolves to the item it names, not to root"
      (let* ((a (cadr (run-piped dB
                        (string-append "((insert root #f ((kind . section) (title \"F1\")))"
                                       " (insert (from 0) #f ((kind . section) (title \"F2\"))))")
                        "batch")))
             (heads (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                        (map (lambda (x) (and (pair? x) (car x))) (cadr a))
                        (list a)))
             (ids (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                      (map (lambda (one)
                             (car (car (cadr (assq 'state (cdr one))))))
                           (cadr a))
                      '()))
             (second (and (= 2 (length ids)) (cadr ids)))
             ;; THE PARENT IS IN `position`, NOT IN A `parent` KEY.
             ;; `read` answers `(ok ((id . <id>) ... (position <parent>
             ;; . <ord>) (edges)))`, so looking for `parent` found
             ;; nothing and the row failed against a product that was
             ;; placing the block correctly -- the fixture was reading
             ;; for a field the answer does not have.
             (parent (and second
                          (let* ((ans (car (lines-of (run dB "read" second))))
                                 (alist (and (pair? ans) (eq? (car ans) 'ok) (cadr ans)))
                                 (pos (and alist (assq 'position alist))))
                            (if pos (cadr pos) 'no-position)))))
        (list heads (and (= 2 (length ids)) (equal? parent (car ids)))))
      (list '(ok ok) #t))

(printf "\n== a value that could not be an id is refused before the diagnosis ==\n")
;; THE REPLY TO A BAD ID WAS ITSELF A STRING OPERATION. `resolve` answers
;; an unknown id by asking `nearest-ids` for suggestions, and that takes
;; `string-length` of what it was given -- so `(del 7)` raised on the way
;; to being refused. A refusal that cannot be phrased is not a refusal.
;;
;; THE CHECK IS WIDER THAN ANY ONE POSITION NEEDS -- string, `root`, `#f`
;; or a `(from n)` reference are all accepted everywhere an id may go --
;; because which of those belongs in which position is already decided
;; further in, and a second copy of that judgement here would be a second
;; place to keep it right.
(let* ((mk (lambda (title)
             (let ((a (cadr (run-piped dB "" "insert" "--under" "root" "--title" title))))
               (car (car (cadr (assq 'state (cdr a))))))))
       (idA (mk "I1")))
  (for-each
    (lambda (text)
      (want (string-append "a non-id in an id position is refused: "
                           (substring text 0 (min 24 (string-length text))))
            (let ((a (cadr (run-piped dB text "batch"))))
              (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                  (cadr (car (cadr a)))
                  a))
            'malformed-intent))
    (list "((del 7))"
          "((set 7 title \"x\"))"
          "((move 7 root #f))"
          (string-append "((link 7 rel \"" idA "\"))")
          (string-append "((unlink \"" idA "\" rel 7))")
          "((insert 7 #f ((kind . section))))"
          "((insert root 7 ((kind . section))))"))
  ;; TWIN: and every shape that IS an id still works -- a string, `root`
  ;; as a parent, `#f` as a predecessor, and a back-reference. Without
  ;; this the rows above are also passed by a guard that refuses every id
  ;; it is shown.
  (want "TWIN: string, root, #f and a back-reference are all still ids"
        (let ((a (cadr (run-piped dB
                         (string-append "((insert root #f ((kind . section) (title \"J1\")))"
                                        " (insert (from 0) #f ((kind . section) (title \"J2\")))"
                                        " (del \"" idA "\"))")
                         "batch"))))
          (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
              (map (lambda (x) (and (pair? x) (car x))) (cadr a))
              a))
        '(ok ok ok)))

(printf "\n== an expectation that is not a hash is a refusal, not a switch ==\n")
;; `expectation` ANSWERS THE VALUE IT FINDS, so a wrapper holding #f was
;; indistinguishable from no wrapper at all and the check SILENTLY DID
;; NOT RUN: `(expect #f (set <id> title "Two"))` wrote. A caller that asks
;; for a premise to be verified and gets no answer either way is worse
;; off than one that never asked, because it will not look again.
(let* ((a (cadr (run-piped dB "" "insert" "--under" "root" "--title" "K1")))
       (pair (car (cadr (assq 'state (cdr a)))))
       (idK (car pair)) (hashK (cdr pair)))
  (for-each
    (lambda (bad)
      (want (string-append "an expectation spelled " bad " is refused")
            (let ((r (cadr (run-piped dB
                             (string-append "((expect " bad " (set \"" idK "\" title \"T\")))")
                             "batch"))))
              (if (and (pair? r) (eq? (car r) 'batch) (list? (cadr r)))
                  (cadr (car (cadr r)))
                  r))
            'malformed-intent))
    (list "#f" "7" "(a b)"))
  ;; TWIN: a STALE STRING is a different answer -- the check ran and said
  ;; no. Collapsing the two would tell a caller whose premise was refused
  ;; the same thing as a caller whose wrapper was unreadable.
  (want "TWIN: a stale hash is `changed`, because the check ran"
        (let ((r (cadr (run-piped dB
                         (string-append "((expect \"stale\" (set \"" idK "\" title \"T\")))")
                         "batch"))))
          (if (and (pair? r) (eq? (car r) 'batch) (list? (cadr r)))
              (cadr (car (cadr r)))
              r))
        'changed)
  ;; TWIN: and the real hash still lets the work through.
  (want "TWIN: the block's own hash still passes"
        (let ((r (cadr (run-piped dB
                         (string-append "((expect \"" hashK "\" (set \"" idK "\" title \"T\")))")
                         "batch"))))
          (if (and (pair? r) (eq? (car r) 'batch) (list? (cadr r)))
              (car (car (cadr r)))
              r))
        'ok))

(printf "\n== a record the reducer cannot apply is never appended ==\n")
;; THE WORST THING AN APPEND-ONLY STORE CAN HOLD is a record that is
;; validly framed and cannot be reduced: nothing downstream can refuse it
;; any more. `(insert root #f (7))` was such a record -- it passed the
;; item's shape check, encoded cleanly, was APPENDED, and then the
;; reducer raised on it. Measured before the fix: the store answered an
;; internal error to `outline` and to every later `insert`, permanently.
;; Twelve characters destroyed a knowledge base.
;;
;; THE WRITE PATH APPENDS BEFORE IT REDUCES, which is why "the reducer
;; will raise" is not a refusal -- by the time it raises, the record is
;; durable.
(define dM (fresh-store!))
(init! dM)
(run-piped dM "" "insert" "--under" "root" "--title" "Good")
(define (block-count d) (length (lines-of (run d "outline"))))
;; RECORDS, NOT BLOCKS. "No record was appended" was measured by counting
;; outline lines -- and an implementation that appended the malformed
;; record, answered an error, and let the reducer skip it produces the
;; same count. It would have passed this section entirely. The log's own
;; entries are the thing the claim is about.
;;
;; AND A ONE-LINE ERROR SATISFIES A COUNT OF ONE, which is the other half
;; of the same weakness: `outline` answering `(error ...)` is one line.
(define (record-count d) (length (lines-of (run d "log"))))
(define records-before (record-count dM))
(want "CONTROL: the store has a block, reads, and its log has entries"
      (list (block-count dM) (> records-before 0))
      (list 1 #t))
(want "a field collection the reducer cannot walk is refused"
      (let ((a (cadr (run-piped dM "((insert root #f (7)))" "batch"))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (car (car (cadr a)))
            a))
      'error)
;; AND NOTHING WAS WRITTEN. The answer alone does not say this: a store
;; that appended the record and then answered an error would give the
;; same first reading.
(want "and no record was appended"
      (record-count dM)
      records-before)
;; AND THE STORE IS STILL A STORE. This is the row the defect actually
;; broke: reading and writing after the refusal, not the refusal itself.
;; AND `outline` ANSWERING AN ERROR IS ONE LINE TOO, so this row reads
;; the line itself rather than counting it.
(want "and the store still reads and still writes"
      (let* ((wrote (car (cadr (run-piped dM "" "insert" "--under" "root" "--title" "After"))))
             (ls (lines-of (run dM "outline"))))
        (list wrote (length ls)
              (and (pair? (car ls)) (eq? (car (car ls)) 'error))))
      (list 'ok 2 #f))
;; TWIN: a good item BEFORE the bad one keeps its answer and its record,
;; because the receipt and the earlier items are already committed.
(want "TWIN: a good item before a bad one is kept, and the bad one refused"
      (let* ((before (block-count dM))
             (a (cadr (run-piped dM
                        "((insert root #f ((kind . section) (title \"Keep\"))) (insert root #f (7)))"
                        "batch")))
             (heads (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                        (map (lambda (x) (and (pair? x) (car x))) (cadr a))
                        (list a))))
        (list heads (- (block-count dM) before)))
      (list '(ok error) 1))
;; TWIN: and a well-formed field collection is still written, so the
;; guard did not become a refusal of ordinary work.
(want "TWIN: a well-formed field collection is still written"
      (let* ((before (block-count dM))
             (a (cadr (run-piped dM
                        "((insert root #f ((kind . section) (title \"Fine\"))))"
                        "batch"))))
        (list (car (car (cadr a))) (- (block-count dM) before)))
      (list 'ok 1))

(printf "\n== which layer refused, and what it said ==\n")
;; THE BOUNDARY IS THREE LAYERS DEEP and they overlap: the intent check
;; judges what the caller wrote, the payload check judges what `resolve`
;; produced, and the reducer judges what it is asked to apply. Remove any
;; one and the others still refuse the same inputs -- so a row that only
;; asks "was it refused" cannot tell whether a layer is doing anything,
;; and every one of them survived being deleted.
;;
;; WHAT DIFFERS IS THE ANSWER. The intent layer answers
;; `(malformed-intent <the intent>)`; the payload layer answers
;; `(malformed-intent (<reason> <the payload>))`. So the SHAPE of the
;; answer names the layer, and the REASON names the rule -- two checks
;; that catch the same input are not interchangeable if they send an
;; operator to two different places.
(define (refusal-of text)
  (let ((a (cadr (run-piped dB text "batch"))))
    (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a))
             (pair? (car (cadr a))) (eq? (car (car (cadr a))) 'error))
        (let ((why (caddr (car (cadr a)))))
          (list (cadr (car (cadr a)))
                (if (and (pair? why) (symbol? (car why))) (car why) why)))
        (list 'not-refused a))))
(for-each
  (lambda (case)
    (want (string-append "refused by the right layer, with the right reason: "
                         (substring (car case) 0 (min 22 (string-length (car case)))))
          (refusal-of (car case))
          (cdr case)))
  (list
    ;; the caller wrote a name the write path is about to compute: only
    ;; the caller's-fields rule knows this, and it is the reason an
    ;; operator needs -- the duplicate-key rule catches the same input
    ;; and tells them something true but useless.
    (cons "((insert root #f ((ord . \"x\"))))" '(malformed-intent field-name-reserved))
    (cons "((insert root #f ((parent . \"x\"))))" '(malformed-intent field-name-reserved))
    ;; a key the caller repeated, which the reserved rule says nothing about
    (cons "((insert root #f ((title . \"a\") (title . \"b\"))))"
          '(malformed-intent field-name-repeated))
    ;; the collection itself
    (cons "((insert root #f (7)))" '(malformed-intent field-entry-not-a-pair))
    (cons "((insert root #f 7))" '(malformed-intent fields-not-a-list))
    ;; an id position: caught at the INTENT layer, so the answer carries
    ;; the intent rather than a reason and a payload
    (cons "((del 7))" '(malformed-intent not-an-id))
    (cons "((del root))" '(malformed-intent not-an-id))
    ;; a payload only `resolve` can produce: the intent is well formed
    ;; and the thing about to be appended is not
    (cons "((tag 7))" '(malformed-intent tag-name-not-a-string))
    ;; A FIXED-TYPE POSITION IS CAUGHT AT THE INTENT LAYER, and the
    ;; payload layer would catch the same input one step later with its
    ;; own reason -- so without asserting WHICH reason comes back, the
    ;; intent-layer rule could be deleted and nothing would change. The
    ;; two answers point at different things: `name-not-a-symbol` is
    ;; about the intent the caller wrote, `field-name-not-a-symbol` is
    ;; about the record that was going to be appended.
    (cons "((set \"a.1\" \"title\" \"x\"))" '(malformed-intent name-not-a-symbol))
    (cons "((link \"a.1\" \"rel\" \"a.2\"))" '(malformed-intent name-not-a-symbol))
    (cons "((unlink \"a.1\" 7 \"a.2\"))" '(malformed-intent name-not-a-symbol))
    ;; A LEVEL IS READ AS A NUMBER when a heading is rendered, so a
    ;; stringy one applies cleanly and then breaks every Markdown read of
    ;; that block. The command line passes every field value as text,
    ;; which is exactly how one is produced.
    (cons "((insert root #f ((level . \"2\"))))" '(malformed-intent level-not-a-heading-level))
    (cons "((insert root #f ((level . 9))))" '(malformed-intent level-not-a-heading-level))
    ;; AND THE RESERVED NAME SPEAKS BEFORE THE DUPLICATE. Both are true of
    ;; `((ord . 1) (ord . 2))`, and "you wrote that key twice" sends the
    ;; caller to remove one -- which leaves a field they were never
    ;; allowed to write.
    (cons "((insert root #f ((ord . 1) (ord . 2))))" '(malformed-intent field-name-reserved))
    ;; AN IMPROPER LIST IS NOT A SHORT ONE. `(set <id> title "x" . junk)`
    ;; has every argument it needs and is still not a form; answering
    ;; `too-few-arguments` sends the caller to add an argument, which
    ;; cannot help. The payload validator already separated these, and
    ;; two validators must not describe one defect differently.
    ;;
    ;; IT IS SPELLED AS A TOP-LEVEL FORM, not bracketed. Wrapped, the
    ;; improper form becomes the HEAD of a well-formed item and is
    ;; answered `verb-not-a-symbol` -- a true answer about a different
    ;; shape, which would have made this row pass without reaching the
    ;; rule it is about.
    (cons "(set \"a.1\" title \"x\" . junk)" '(malformed-intent intent-not-a-proper-list))
    (cons "(expect \"h\" (set \"a.1\" title \"x\") . junk)"
          '(malformed-intent intent-not-a-proper-list))
    ;; TWIN: and a genuinely short one still says so.
    (cons "((set \"a.1\"))" '(malformed-intent too-few-arguments))
    (cons "((insert root))" '(malformed-intent too-few-arguments))))

;; AND A BLOCK CANNOT BECOME A NESTED DOCUMENT BY BEING RELABELLED.
;; `insert` and `move` both ask; `set kind doc` reached the same shape
;; without passing either, so the store held a nested document while the
;; README said the write path refuses to create one. A rule enforced at
;; two of its three entrances is not enforced.
(let* ((dD (fresh-store!)))
  (init! dD)
  ;; THE IDS COME FROM THE ANSWERS, not from the outline. `lines-of`
  ;; reads ONE datum per line and an outline line begins with `-`, so
  ;; taking its third element reads a structure that is not there.
  (let* ((a (cadr (run-piped dD "((insert root #f ((kind . doc) (title . \"Outer\"))) (insert (from 0) #f ((kind . section) (title . \"Inner\"))))" "batch")))
         (ids (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                  (map (lambda (one) (car (car (cadr (assq 'state (cdr one)))))) (cadr a))
                  '()))
         (outer (and (= 2 (length ids)) (car ids)))
         (inner (and (= 2 (length ids)) (cadr ids))))
    (want "CONTROL: the document has a section under it"
          (length ids)
          2)
    (want "relabelling a nested block as a document is refused"
          (let ((a (cadr (run-piped dD (string-append "((set \"" inner "\" kind doc))") "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                (cadr (car (cadr a)))
                a))
          'doc-must-be-top-level)
    ;; AND TWO LEVELS DOWN, WHERE THE IMMEDIATE PARENT IS NOT A DOCUMENT.
    ;; The first version of this rule asked only whether the parent was
    ;; itself a document, so `document -> section -> section` could still
    ;; be relabelled -- a third route to the shape the other two entrances
    ;; refuse. `insert` and `move` reject a document at ANY non-root
    ;; parent, and the row above cannot tell the two rules apart because
    ;; its block's parent IS the document.
    (want "relabelling a block two levels down is refused as well"
          (let* ((made (cadr (run-piped dD
                               (string-append "((insert \"" inner "\" #f ((kind . section) (title . \"Deep\"))))")
                               "batch")))
                 (deep (and (pair? made) (eq? (car made) 'batch)
                            (car (car (cadr (assq 'state (cdr (car (cadr made)))))))))
                 (a (cadr (run-piped dD (string-append "((set \"" deep "\" kind doc))") "batch"))))
            (list (and deep #t)
                  (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                      (cadr (car (cadr a)))
                      a)))
          (list #t 'doc-must-be-top-level))
    ;; TWIN: a TOP-LEVEL block may still be relabelled, so the row above
    ;; is about the nesting and not about refusing `set kind`.
    ;;
    ;; IT STARTS AS A SECTION AND ITS KIND IS READ BACK. This used
    ;; `outer`, which the fixture had already created with `(kind . doc)`
    ;; -- so an implementation that allowed only kinds that were not
    ;; changing would have passed, and the twin would have shown nothing.
    (want "TWIN: a top-level section may still become a document"
          (let* ((made (cadr (run-piped dD "" "insert" "--under" "root" "--title" "Plain")))
                 (fresh (car (car (cadr (assq 'state (cdr made))))))
                 (a (cadr (run-piped dD (string-append "((set \"" fresh "\" kind doc))") "batch")))
                 (head (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                           (car (car (cadr a)))
                           a))
                 (rd (car (lines-of (run dD "read" fresh))))
                 (kind (let ((fs (and (pair? rd) (eq? (car rd) 'ok)
                                      (cdr (assq 'fields (cadr rd))))))
                         (and fs (let ((e (assq 'kind fs))) (and e (cdr e)))))))
            (list head kind))
          (list 'ok 'doc))
    ;; TWIN: and nothing reports a nested document afterwards.
    (want "TWIN: and the store reports no nested document"
          (length (lines-of (run dD "conflicts")))
          0)
    ;; AND A LEVEL SET ON A REAL BLOCK is refused for its type, not for
    ;; the block being unknown -- which is what a made-up id would have
    ;; tested instead.
    (want "a level that is not a heading level is refused on an existing block"
          (let ((a (cadr (run-piped dD (string-append "((set \"" outer "\" level \"2\"))") "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                (cadr (car (cadr a)))
                a))
          'malformed-intent)
    (want "TWIN: a real heading level is accepted on the same block"
          (let ((a (cadr (run-piped dD (string-append "((set \"" outer "\" level 3))") "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                (car (car (cadr a)))
                a))
          'ok)))

(printf "\n== a title is one line, and the listing stays one row per block ==\n")
;; `outline` PRINTS ONE ROW PER BLOCK AS TEXT, so a title carrying a line
;; terminator produced an EXTRA ROW -- one that looked exactly like a
;; real one and carried a well-formed id no block has. Measured before
;; the fix: two records, two blocks, THREE rows. Anything reading the
;; listing as text believed the third.
(define dL (fresh-store!))
(init! dL)
(run-piped dL "" "insert" "--under" "root" "--title" "Real")
(define (log-lines d) (length (lines-of (run d "log"))))
(define (outline-lines d) (length (lines-of (run d "outline"))))
(define lines-before (outline-lines dL))
(define records-before (log-lines dL))
(for-each
  (lambda (case)
    (want (string-append "a title carrying " (car case) " is refused")
          (let ((a (cadr (run-piped dL (cdr case) "batch"))))
            (list (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                      (cadr (car (cadr a)))
                      a)
                  (outline-lines dL)
                  (log-lines dL)))
          (list 'malformed-intent lines-before records-before)))
  (list (cons "a newline"
              "((insert root #f ((kind . section) (title . \"Fake\\n- zzzzzzzz.9  Phantom\"))))")
        (cons "a carriage return"
              "((insert root #f ((kind . section) (title . \"Fake\\r- zzzzzzzz.9  Phantom\"))))")
        (cons "U+0085"
              "((insert root #f ((kind . section) (title . \"Fake\\x85;more\"))))")
        (cons "U+2028"
              "((insert root #f ((kind . section) (title . \"Fake\\x2028;more\"))))")
        (cons "U+2029"
              "((insert root #f ((kind . section) (title . \"Fake\\x2029;more\"))))")
        (cons "a bell"
              "((insert root #f ((kind . section) (title . \"Fake\\x7;more\"))))")))
;; TWIN: TAB AND EMOJI ARE ORDINARY TEXT IN A TITLE. Without these the
;; rows above are also passed by a rule that refuses any title it finds
;; unusual, which would be a worse defect than the one being fixed.
(for-each
  (lambda (case)
    (want (string-append "TWIN: a title carrying " (car case) " is accepted")
          (let ((a (cadr (run-piped dL (cdr case) "batch"))))
            (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                (car (car (cadr a)))
                a))
          'ok))
  (list (cons "a tab" "((insert root #f ((kind . section) (title . \"a\\tb\"))))")
        (cons "an emoji" "((insert root #f ((kind . section) (title . \"a \\x1F600; b\"))))")
        (cons "ordinary text" "((insert root #f ((kind . section) (title . \"Ordinary\"))))")))

(printf "\n== a symbol this store writes is one the wire writer will emit ==\n")
;; A RELATION OR FIELD NAME THAT IS NOT WIRE-SAFE WAS WRITTEN, AND THEN
;; THE ANSWER COULD NOT BE BUILT. The record encodes fine -- a non-bare
;; symbol becomes `("#%sym" ...)` -- but the state datum carrying the raw
;; symbol cannot be serialised, and that raise happened while assembling
;; the reply, AFTER the barrier. Measured before the fix: the caller was
;; told `(error internal (condition "unexpected failure"))` and the log
;; had grown by one.
;;
;; SO EVERY ROW HERE ASSERTS THE LOG LENGTH. The answer alone cannot
;; distinguish "refused" from "written and then reported as a failure",
;; which is the whole defect.
(define dU (fresh-store!))
(init! dU)
(define uA
  (car (car (cadr (assq 'state (cdr (cadr (run-piped dU "" "insert" "--under" "root" "--title" "A"))))))))
(define uB
  (car (car (cadr (assq 'state (cdr (cadr (run-piped dU "" "insert" "--under" "root" "--title" "B"))))))))
(define (u-log-lines) (length (lines-of (run dU "log"))))
(for-each
  (lambda (case)
    (let ((before (u-log-lines)))
      (want (string-append "a non-wire-safe symbol in " (car case) " is refused, and nothing is written")
            ;; THE REASON IS NESTED ONE LEVEL IN. The answer is
            ;; `(error malformed-intent (<reason> <payload>))`, so the
            ;; second element is the KIND and the reason lives inside the
            ;; third -- taking the second reads `malformed-intent` for
            ;; every rule alike, which is the distinction these rows
            ;; exist to make.
            (let ((a (cadr (run-piped dU (cdr case) "batch"))))
              (list (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a))
                             (pair? (car (cadr a))) (pair? (cddr (car (cadr a))))
                             (pair? (caddr (car (cadr a)))))
                        (car (caddr (car (cadr a))))
                        a)
                    (u-log-lines)))
            (list 'symbol-not-wire-safe before))))
  (list (cons "a link's relation"
              (string-append "((link \"" uA "\" |has part| \"" uB "\"))"))
        (cons "an unlink's relation"
              (string-append "((unlink \"" uA "\" |a(b| \"" uB "\"))"))
        (cons "a set's field name"
              (string-append "((set \"" uA "\" |field name| \"x\"))"))
        (cons "an insert's field name"
              "((insert root #f ((kind . section) (|odd name| . \"x\"))))")
        (cons "an insert's kind value"
              "((insert root #f ((kind . |not bare|))))")
        ;; A NAME THAT COMES BACK AS A NUMBER. These are the shapes the
        ;; round trip loses rather than mangles: the writer emits `1`
        ;; bare and the reader hands back the integer 1, so the symbol
        ;; the caller asked for does not exist on the other side. Measured
        ;; downstream before this closed: `link a 1 b` answered an
        ;; internal error AND left the record on disk, `read` printed
        ;; `(edges (\x31; . ...))`, and a reader one library away refused
        ;; the block outright -- so the block could not be opened at all.
        ;;
        ;; SPACES AND BRACKETS ARE NOT THE WHOLE FAMILY, which is why
        ;; these have their own rows: the cases above are names the
        ;; writer would have to quote, and these are names it would emit
        ;; unquoted into a different type.
        (cons "a link's relation that reads back as an integer"
              (string-append "((link \"" uA "\" |1| \"" uB "\"))"))
        (cons "a link's relation that reads back as a larger integer"
              (string-append "((link \"" uA "\" |42| \"" uB "\"))"))
        (cons "a link's relation that reads back as a negative integer"
              (string-append "((link \"" uA "\" |-1| \"" uB "\"))"))
        (cons "a field name that reads back as a decimal"
              (string-append "((set \"" uA "\" |1.5| \"x\"))"))))
;; TWIN: AND THE ORDINARY NAMES STILL WORK. Without this the rows above
;; are also passed by a rule that refuses every symbol.
(want "TWIN: ordinary relation and field names are still written"
      ;; `let*`, BECAUSE ONE OF THESE HAS TO HAPPEN FIRST. `let` does not
      ;; say which initialiser runs first, and this row needs the count
      ;; taken before the batch writes. It read green for months on the
      ;; order the compiler happened to pick, and changed to `((ok ok) 0)`
      ;; the day an unrelated edit changed the surrounding code enough to
      ;; pick the other one. A row whose answer depends on that was never
      ;; measuring what it says.
      (let* ((before (u-log-lines))
             (a (cadr (run-piped dU
                        (string-append "((link \"" uA "\" has-part \"" uB "\")"
                                       " (set \"" uA "\" note \"x\"))")
                        "batch"))))
        (list (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                  (map (lambda (x) (and (pair? x) (car x))) (cadr a))
                  a)
              (- (u-log-lines) before)))
      (list '(ok ok) 2))

;; TWIN: AND THE NAMES THAT ONLY LOOK NUMERIC ARE STILL WRITTEN. `+x`
;; and `a-b` start with a character the rows above refuse in front of a
;; digit, and both read back as the symbols they are -- so a rule that
;; refused anything beginning with a sign, or anything containing one,
;; would pass every row above and take these with it.
(want "TWIN: a name that begins with a sign, and one that contains one, are written"
      (let* ((before (u-log-lines))
             (a (cadr (run-piped dU
                        (string-append "((link \"" uA "\" |+x| \"" uB "\")"
                                       " (set \"" uA "\" |a-b| \"x\"))")
                        "batch"))))
        (list (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
                  (map (lambda (x) (and (pair? x) (car x))) (cadr a))
                  a)
              (- (u-log-lines) before)))
      (list '(ok ok) 2))

(printf "\n== committing and describing are two acts ==\n")
;; SECTION 7.3 SAYS `ok` MEANS THE WORK IS DURABLE. Read the other way,
;; an error that is not `unknown` has to mean NO RECORD -- otherwise a
;; client retries a write that already happened. Building the answer runs
;; AFTER the barrier, and any failure there used to turn a durable write
;; into `(error internal ...)`: measured, the log had grown by one while
;; the caller was told the write failed.
;;
;; THE ONLY WAY TO REACH THAT STEP ON PURPOSE IS TO ARM IT, which is why
;; `report-fail` exists. Its stage is compared statically, because
;; assembling an answer is not a durability call and runs inside no
;; stage; the spec still carries `@report` and is refused at startup
;; without it.
(define (run-armed store fault text . args)
  (let ((in-path (string-append scratch "/stdin-armed.txt")))
    (system (string-append "rm -f " in-path))
    (call-with-port (open-file-output-port in-path (file-options no-fail)
                                           'block (native-transcoder))
      (lambda (o) (put-string o text)))
    (let* ((cmd (string-append
                  (if fault
                      (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ")
                      "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT ")
                  "THEOURGIA_HOME=" home " "
                  "scheme --script " cli " "
                  (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                  "--store " store " < " in-path " > " out-path " 2> " err-path))
           (code (system cmd))
           (text (text-of out-path)))
      (list code (guard (e (#t (list 'unreadable text)))
                   (read (open-string-input-port text)))))))
(define dR (fresh-store!))
(init! dR)
(define (r-log-lines) (length (lines-of (run dR "log"))))
(want "a write whose report cannot be built is still ok, and says the report is missing"
      (let* ((before (r-log-lines))
             (a (cadr (run-armed dR "report-fail@report" ""
                                 "insert" "--under" "root" "--title" "R1")))
             (state-part (and (pair? a) (eq? (car a) 'ok) (assq 'state (cdr a)))))
        (list (and (pair? a) (car a))
              (and state-part (cadr state-part))
              ;; THE SHAPE IS ESTABLISHED BEFORE THE ANSWER IS TAKEN
              ;; APART, here as well as in the three components beside
              ;; it. This one was missing the test and `assq` raised on
              ;; `(unknown (interrupted ...))` -- ending the file, so the
              ;; rows below this point did not run and the round reported
              ;; the mutant as a crash with no failures rather than as a
              ;; kill. The other three already asked; only this one did
              ;; not, and the omission is invisible whenever the answer
              ;; comes back `ok`.
              (and (pair? a) (eq? (car a) 'ok) (assq 'cursor (cdr a)) #t)
              (- (r-log-lines) before)))
      (list 'ok 'unavailable #t 1))
;; TWIN: UNARMED, THE SAME WRITE REPORTS ITS HASHES. Without this the row
;; above is also passed by a store that never reports them.
(want "TWIN: unarmed, the same write carries its state hashes"
      (let* ((a (cadr (run-armed dR #f "" "insert" "--under" "root" "--title" "R2")))
             (state-part (and (pair? a) (eq? (car a) 'ok) (assq 'state (cdr a)))))
        (list (and (pair? a) (car a))
              (and state-part (pair? (cadr state-part)) #t)))
      (list 'ok #t))
;; AND THE WRITE REALLY DID HAPPEN, which is the claim `ok` makes: the
;; same request sent again is a replay rather than a second record.
(want "a request whose report failed is still a request that ran"
      (let* ((cur (cadr (assq 'cursor (cdr (cadr (run-armed dR #f "" "insert" "--under" "root" "--title" "R3"))))))
             (after (string-append (car cur) ":" (number->string (cdr cur))))
             (first (cadr (run-armed dR "report-fail@report" ""
                                     "insert" "--under" "root" "--title" "R4"
                                     "--req" "r-rep" "--cursor" after)))
             (n1 (r-log-lines))
             (again (cadr (run-armed dR #f "" "insert" "--under" "root" "--title" "R4"
                                     "--req" "r-rep" "--cursor" after)))
             (n2 (r-log-lines)))
        (list (and (pair? first) (car first))
              (and (pair? again) (car again))
              (and (assq 'replay (cdr again)) (cadr (assq 'replay (cdr again))))
              (= n1 n2)))
      (list 'ok 'ok #t #t))

;; AND A FAILED REPORT MUST NOT DELETE A BLOCK. The rows above are about
;; one record; this is about a request of several, where a later intent
;; names an earlier one. Which block an insert made is decided by the
;; record's own coordinates and is knowable the moment it commits -- but
;; it used to be read out of the hashes reported beside it, and those are
;; a description built afterwards which is allowed to be missing. So a
;; failure to DESCRIBE the first write made the back-reference resolve to
;; nothing, and the caller was told `no-such-intent` about an intent it
;; had just written: one block short, and the reason blamed the caller.
;;
;; MEASURED BEFORE THE FIX: armed, the second intent answered
;; `(error (no-such-intent 0))` and the document held one block instead
;; of two. Without this row the fix has no case at all -- it was verified
;; with a probe, and a probe is not a case.
(define dF (fresh-store!))
(init! dF)
(define (two-under-one armed)
  (cadr (run-armed dF armed
                   (string-append "((insert root #f ((kind . section) (title . \"Parent\")))"
                                  " (insert (from 0) #f ((kind . section) (title . \"Child\"))))")
                   "batch")))
(define (both-answers armed)
  (let ((a (two-under-one armed)))
    (and (pair? a) (eq? (car a) 'batch) (list? (cadr a))
         (map (lambda (x) (and (pair? x) (car x))) (cadr a)))))
;; THE COUNT IS A GROWTH, NOT A TOTAL. Both rows write into the same
;; store, so a total would be an assertion about how many rows ran before
;; this one -- which is the sort of expectation that goes wrong the next
;; time somebody adds a row above.
(want "CONTROL: unarmed, a back-reference finds the block the first intent made"
      (let ((before (length (lines-of (run dF "outline")))))
        (list (both-answers #f)
              (- (length (lines-of (run dF "outline"))) before)))
      (list '(ok ok) 2))
(want "a report that cannot be built does not take the second block with it"
      (let ((before (length (lines-of (run dF "outline")))))
        (list (both-answers "report-fail@report")
              (- (length (lines-of (run dF "outline"))) before)))
      (list '(ok ok) 2))

(printf "\n== tag is two requests under one name ==\n")
;; `tag <name>` WRITES A RECORD AND `tag` LISTS THEM. Only the first has
;; anything to replay, so trackability here is a property of the REQUEST
;; and not of the verb -- which a list of verb names cannot express. The
;; list got it wrong in both directions at once: leaving `tag` out
;; refused an identity the write path was already using, and putting it
;; in would accept one silently on the listing form.
(define dT (fresh-store!))
(init! dT)
(define WT
  (let ((a (cadr (run-piped dT "" "insert" "--under" "root" "--title" "Zero"))))
    (cadr (assq 'cursor (cdr a)))))
(define cursor-WT (string-append (car WT) ":" (number->string (cdr WT))))
(want "CONTROL: naming a tag under an identity is executed"
      (car (cadr (run-piped dT "" "tag" "v1" "--req" "t-1" "--cursor" cursor-WT)))
      'ok)
(want "the same tag request again is a replay, not a second record"
      (let ((a (cadr (run-piped dT "" "tag" "v1" "--req" "t-1" "--cursor" cursor-WT))))
        (list (car a) (cadr a)))
      '(ok (replay #t)))
;; TWIN: THE LISTING FORM HAS NOTHING TO REPLAY and refuses an identity,
;; so the row above is about which request carries one rather than about
;; the name `tag`.
(want "TWIN: tag with no name refuses an identity"
      (cadr (run-piped dT "" "tag" "--req" "t-2" "--cursor" cursor-WT))
      '(error bad-request req-not-tracked tag))
(want "TWIN: and listing still works with no identity given"
      (let ((a (cadr (run-piped dT "" "tag"))))
        (and (pair? a) (car a)))
      'tag)

(printf "\n== an expectation is checked for every verb that takes one ==\n")
;; `expect` IS OPTIMISTIC CONCURRENCY: the caller says what it believes
;; the block's hash to be, and the store refuses if the block has moved
;; on. It was checked for `set` and silently NOT checked for `move` --
;; because the step that rewrites an intent's references unwrapped the
;; intent and handed back the bare one, and that step runs for exactly
;; the two verbs that have references, insert and move.
;;
;; SILENTLY NOT CHECKED IS WORSE THAN NOT OFFERED. A caller told the
;; premise is verified for one verb writes its code as though it were
;; verified for both.
(define dE (fresh-store!))
(init! dE)
(define (insert-with-hash! title)
  (let* ((a (cadr (run-piped dE "" "insert" "--under" "root" "--title" title)))
         (pair (car (cadr (assq 'state (cdr a))))))
    (cons (car pair) (cdr pair))))
(define blockE (insert-with-hash! "E1"))
(define otherE (insert-with-hash! "E2"))
(define (batch-heads text)
  (let ((a (cadr (run-piped dE text "batch"))))
    (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
        (map (lambda (x) (and (pair? x) (car x))) (cadr a))
        (list 'not-a-batch a))))
(want "CONTROL: a stale expectation stops a set"
      (batch-heads (string-append "((expect \"deadbeef\" (set \"" (car blockE)
                                  "\" title \"E1b\")))"))
      '(error))
(want "a stale expectation stops a move as well"
      (batch-heads (string-append "((expect \"deadbeef\" (move \"" (car blockE)
                                  "\" root #f)))"))
      '(error))
;; TWIN: THE CURRENT HASH LETS BOTH THROUGH. Without this the rows above
;; are also passed by a store that refuses every intent carrying an
;; expectation, which would be the same defect facing the other way.
(want "TWIN: the block's actual hash lets the move through"
      (batch-heads (string-append "((expect \"" (cdr blockE) "\" (move \""
                                  (car blockE) "\" root #f)))"))
      '(ok))
(want "TWIN: and the actual hash lets a set through"
      (batch-heads (string-append "((expect \"" (cdr otherE) "\" (set \""
                                  (car otherE) "\" title \"E2b\")))"))
      '(ok))

(printf "\n== a thrown vector is read for its reason, not called unexpected ==\n")
;; THE SEXPR LAYER RAISES A VECTOR, NOT A CONDITION. `#(sexpr-error
;; <message> <position>)` is what (igropyr sexpr) throws, so
;; `message-condition?` is false for it and the last-resort arm answered
;; "unexpected failure" while the thrown object was carrying the reason
;; the whole way. Nothing here fixes the failure -- it reads it.
;;
;; A VALUE NESTED DEEPER THAN THE CODEC WILL GO, which is what makes
;; this reachable from the command line at all: `get-datum` parses it,
;; the record layer frames it, and the emitter that builds a snapshot's
;; rows refuses it -- a value this store can hold and cannot describe.
;;
;; ⚠️ IT USED TO BE A CHARACTER, `#\a`, AND THAT STOPPED BEING ONE.
;; Block hashing now goes through `storable-encode`, which writes a
;; character as the list ("#%char" 97) -- so a character is describable
;; after all, the `state` section below succeeded, and this row was red
;; in the delivery review of 2026-09-17 before anything in this batch
;; touched it. It was red on the pinned tree too; that was measured,
;; not assumed.
;;
;; THE SUCCESSOR HAD TO BE A VALUE STILL REFUSED BY BOTH READERS, and
;; depth is the one left: 58 levels of vector hash and snapshot
;; normally, 59 are written but cannot be described, and at 61 the
;; record codec refuses the write itself. `nesting-depth.ss` pins those
;; three edges; this block only needs one value from the middle band.
(define dF (fresh-store!))
(init! dF)
(define deep-title
  (let loop ((i 0) (out "0"))
    (if (= i 59) out (loop (+ i 1) (string-append "#(" out ")")))))
(define odd-intent
  (string-append "((insert root #f ((kind . section) (title . " deep-title "))))"))
(define plain-intent
  "((insert root #f ((kind . section) (title \"plain\")))) ")
(define (outline-count d) (length (lines-of (run d "outline"))))
;; CONTROL: THE RECORD IS DURABLE. Every row below is about a block that
;; is on the disk. Against a store that refused the intent they would all
;; still read the same, and would be measuring a rejection instead.
(define odd-before (outline-count dF))
;; ONE RUN, READ TWICE. Sending the intent again for the second row would
;; be a second record with a second answer, and the row would no longer
;; be about the block the first row says is on the disk.
(define odd-answer (cadr (run-piped dF odd-intent "batch")))
(define odd-one
  (and (pair? odd-answer) (eq? (car odd-answer) 'batch)
       (pair? (cadr odd-answer)) (pair? (car (cadr odd-answer)))
       (car (cadr odd-answer))))
(want "CONTROL: the block whose field cannot be described is written anyway"
      (list (and (pair? odd-answer) (car odd-answer))
            (and odd-one (car odd-one))
            (- (outline-count dF) odd-before))
      '(batch ok 1))
;; AND THE SAME MESSAGE ALREADY TRAVELS ON THE ANSWER'S OWN FALLBACK.
;; `state-section` catches this raise where the answer is assembled and
;; reports the part it could not build. The two readers are separate
;; code; this row says the reason survives the first of them.
(want "the answer says which part it could not build, and why"
      (and odd-one (assq 'state (cdr odd-one))
           (cdr (assq 'state (cdr odd-one))))
      '(unavailable (reason "nesting too deep (cyclic data?)")))
;; TWIN: A DESCRIBABLE WRITE STILL GETS ITS HASHES. `unavailable` has to
;; be what this particular value provoked, not what the section always
;; says.
(want "TWIN: a write this store can describe reports its state"
      (let ((a (cadr (run-piped dF plain-intent "batch"))))
        (and (pair? a) (eq? (car a) 'batch) (pair? (cadr a))
             (pair? (car (cadr a)))
             (let ((one (car (cadr a))))
               (and (assq 'state (cdr one))
                    (not (eq? 'unavailable (cadr (assq 'state (cdr one)))))))))
      #t)
;; V8: THE CLI'S OWN LAST RESORT, which is a different reader in a
;; different file. `snapshot` writes every row it can reach, so the
;; refusal escapes past every inner guard and arrives at `guarded` -- the
;; one place that has to decide what a thrown object of unknown shape is
;; called.
(want "the CLI's last-resort answer carries the thrown vector's message"
      (car (lines-of (run dF "snapshot")))
      '(error internal (condition "nesting too deep (cyclic data?)")))
;; TWIN: A SNAPSHOT THAT CAN BE BUILT IS BUILT. Without this row a
;; command that answered `(error internal ...)` for every snapshot would
;; pass, and so would one whose emitter refused everything.
(define dG (fresh-store!))
(init! dG)
(want "TWIN: a store holding only describable values snapshots"
      (begin
        (run-piped dG plain-intent "batch")
        (let ((a (car (lines-of (run dG "snapshot")))))
          (and (pair? a) (car a))))
      'ok)

(printf "\n== a malformed item is refused by name inside a tracked batch too ==\n")
;; THE CHECK EXISTS TWICE BECAUSE THE PATHS ARE TWO, and only one of
;; them was measured. An untracked batch runs its intents through
;; `run-intents!`, a tracked one through `run-items!`, and each asks
;; whether the intent is well formed before trying it. A row on the
;; first says nothing about the second: with the second check gone, the
;; item raises instead, the per-item handler catches it, and a request
;; that provably wrote nothing is answered `unknown` -- "send it again
;; and I will tell you whether it ran" -- with a format string for a
;; reason.
;;
;; THE ROW ABOVE REACHES THIS AND CANNOT SEE IT. "a good item before a
;; malformed one keeps its answer" sends the same two intents down the
;; same path, and keeps only the heads: `(ok error)`. With the check
;; gone the head is still `error`, so that row passes -- measured. Its
;; question is whether the GOOD item's answer survives, and the reason
;; belonging to the other one is computed and then dropped before the
;; comparison. This row is the one that looks at it.
(define dH (fresh-store!))
(init! dH)
(define WH
  (let ((a (cadr (run-piped dH "" "insert" "--under" "root" "--title" "Zero"))))
    (cadr (assq 'cursor (cdr a)))))
(define cursor-WH (string-append (car WH) ":" (number->string (cdr WH))))
(define two-intents
  "((insert root #f ((kind . section) (title \"one\"))) (insert root))")
(define tracked-answers
  (let ((a (cadr (run-piped dH two-intents "batch" "--req" "r-k2"
                            "--cursor" cursor-WH))))
    (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
        (cadr a)
        (list (list 'not-a-batch a)))))
;; CONTROL: THE FIRST ITEM RAN. Without it the row below also passes
;; against a batch that was refused whole, where no item was ever
;; reached and the second one's answer is about something else.
(want "CONTROL: the well-formed item of a tracked batch is executed"
      (and (pair? tracked-answers) (pair? (car tracked-answers))
           (car (car tracked-answers)))
      'ok)
(want "and the malformed item is refused by name, not called uncertain"
      (and (= 2 (length tracked-answers))
           (pair? (cadr tracked-answers))
           (list (car (cadr tracked-answers)) (cadr (cadr tracked-answers))))
      '(error malformed-intent))


(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "cli3 complete\n")
