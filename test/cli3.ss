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

(printf "\n~a failures\n" bad)
(printf "cli3 complete\n")
