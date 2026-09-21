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
        (theourgia ffi) (theourgia wire) (theourgia crc32)
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

;; THE RENDERER IS SHARED, NOT COPIED. This file read a raised condition
;; with `condition-message` alone, which threw the irritants away: the
;; reading `(RAISED "variable ~:s is not bound")` names a variable it does
;; not contain, and that is where the defect was first seen. What replaces
;; it, and what it is measured by, are in `condition-render.ss` and
;; `condition-render.sc` beside this file.
(include "condition-render.ss")
(include "forge-record.ss")

(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list 'RAISED (condition->text e)))) e0))))

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.sc sit side by side; in the
;; repository the fixtures are under test/ and cli.sc is at the root.
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
         (beside (string-append dir "/cli.sc"))
         (above (string-append dir "/../cli.sc")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'cli3
              "cli.sc is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.sc sitting beside this fixture was
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

(define (holds? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

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
    (list code (lines-of-text text))))

;; The splitter, named so a row can ask it directly. It is the harness, and a
;; harness that nothing measures is where a blind spot lives longest.
(define (lines-of-text text)
  (let ()
          ;; NEVER: AND THE LAST LINE COUNTS EVEN WITHOUT ITS NEWLINE. This
          ;; returned at end of text without taking what was left since the
          ;; previous newline, so anything printed WITHOUT a trailing newline
          ;; was invisible to every row that reads these lines -- and the
          ;; answer that arrives last is very often the error. Found by a
          ;; reviewer, who showed that a row asserting an EMPTY answer passed
          ;; while the program had printed `(error internal)`: no newline, no
          ;; line, `(0 ())`, green.
          ;;
          ;; This is the fixture harness, so the change reaches every row in
          ;; the file. The row baseline is what says it reached them harmlessly
          ;; -- every fixture's counts are recorded, and a line appearing
          ;; anywhere it had not before shows up there as drift.
          (let ((take (lambda (line out)
                        (if (= 0 (string-length line))
                            out
                            (cons (guard (e (#t (list 'unreadable line)))
                                    (read (open-string-input-port line)))
                                  out)))))
            (let loop ((i 0) (start 0) (out '()))
              (cond
                ((>= i (string-length text))
                 (reverse (take (substring text start i) out)))
                ((char=? (string-ref text i) #\newline)
                 (loop (+ i 1) (+ i 1) (take (substring text start i) out)))
                (else (loop (+ i 1) start out)))))))

;; NEVER: AND `batch` TAKES ITS INTENTS ON STDIN, NOT IN ARGV. Three attempts
;; to drive it through `run` were answered `(usage (batch <intents>))`, which
;; reads like a quoting problem and is not one: the verb takes NO command-line
;; argument and reads the intents from standard input. `cli1` has always called
;; it that way -- its `run` takes a stdin string -- and this file's `run` has no
;; such parameter, so the shape was simply unavailable here.
;;
;; This is a separate helper rather than a new parameter on `run`, because
;; `run` is called by every row in this file and the row baseline is what says
;; a harness change reached them harmlessly. An addition nothing else calls
;; cannot.
(define (run-with-stdin store input . args)
  (let* ((cmd (string-append
                "printf '%s' '" input "' | "
                "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                "THEOURGIA_HOME=" home " "
                "scheme --script " cli " "
                (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                "--store " store " > " out-path " 2> " err-path))
         (code (system cmd)))
    (list code (lines-of-text (text-of out-path)))))

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
;; NEVER: THE TIE HAD TO BE REBUILT WHEN SCORING GAINED TIERS. e1 and e2
;; used to tie at 2 for `cat`, and this control said so. With a word-boundary
;; hit worth more than a mid-word one they no longer tie -- e1 begins a word
;; and scores 3, e2 is inside `Concatenate` and scores 2 -- so the row that
;; pins "equal scores go by id" would have kept passing WITH NO TIE LEFT TO
;; ORDER. A pair that still ties is made below, both hits mid-word, and the
;; control moves onto that pair.
(define e5 (insert! d2 "--title" "Muscat grapes"))

(want "CONTROL: the two blocks that tie do so mid-word, and were created in id order"
      (list (string<? e2 e5) 'and-both-are-inside-a-longer-word)
      (list #t 'and-both-are-inside-a-longer-word))

;; A hit that begins a word outranks one buried in a longer word, and each
;; field has both tiers: title 3/2, src 2/1. The lower tier is what the field
;; scored before tiers existed, so a block whose hits are all mid-word keeps
;; the score it had -- e2 and e3 below are unchanged from the reading taken
;; before this round.
(want "a word-boundary hit outranks a mid-word one, field by field, and ties go by id"
      (lines-of (run d2 "search" "cat"))
      (list (list 'hit e4 5 "cat and dog")
            (list 'hit e1 3 "cat first inserted")
            (list 'hit e2 2 "Concatenate strings")
            (list 'hit e5 2 "Muscat grapes")
            (list 'hit e3 1 "we concatenate in the body")))
(want "a hit is a case-insensitive substring, so cat finds concatenate"
      (lines-of (run d2 "search" "CAT"))
      (lines-of (run d2 "search" "cat")))
(want "every token must hit: one that matches nothing empties the answer"
      (run d2 "search" "cat zzzzz")
      (list 0 '()))
(want "TWIN: two tokens that both hit the same block keep it"
      (lines-of (run d2 "search" "cat mat"))
      (list (list 'hit e4 5 "cat and dog")))
;; A QUERY IS TEXT. This one would be an enormous exact integer if any
;; part of the path handed it to a numeric parser, and the row would not
;; return rather than returning empty.
(want "a query that looks like a number is matched as text, and returns"
      (run d2 "search" "#e1e99999999")
      (list 0 '()))
(want "no hits is an answer, not an error"
      (run d2 "search" "zzzzzzzz")
      (list 0 '()))


(printf "\n== N2b: one word, several spellings -- and the tiers of a CJK phrase ==\n")
;; NEVER: EQUALITY BETWEEN TWO ANSWERS IS GREEN WHEN BOTH ARE EMPTY. The
;; claim is that two spellings of one query find the SAME BLOCK, so the row
;; pins the block and the score as well as the equality. A row that only
;; compared the two answers would pass on a build where neither spelling
;; found anything at all -- which is precisely the build this exists to catch,
;; since before this round the first spelling found nothing.
(define d2b (fresh-store!))
(init! d2b)
(define w1 (insert! d2b "--title" "\x6062;\x590d;\x65e7; reaper \x7684;\x505a;\x6cd5;"))
;; both bigrams of the query are here and the phrase itself is not
(define w2 (insert! d2b "--title" "\x6062;\x590d;\x4e86;\x4e00;\x534a;\xff0c;\x590d;\x65e7;\x7684;\x90e8;\x5206;\x6ca1;\x52a8;"))
(define w3 (insert! d2b "--title" "parseBlock and friends"))
(define w4 (insert! d2b "--title" "\xff26;\xff35;\xff2c;\xff2c;\xff37;\xff29;\xff24;\xff34;\xff28; letters"))

(want "N2b a CJK phrase written against a latin word is the same query as one with a space"
      (list (lines-of (run d2b "search" "\x6062;\x590d;\x65e7;reaper"))
            (lines-of (run d2b "search" "\x6062;\x590d;\x65e7; reaper")))
      (list (list (list 'hit w1 3 "\x6062;\x590d;\x65e7; reaper \x7684;\x505a;\x6cd5;"))
            (list (list 'hit w1 3 "\x6062;\x590d;\x65e7; reaper \x7684;\x505a;\x6cd5;"))))

;; NEVER: AND THE PHRASE OUTRANKS ITS SCATTERED HALVES. A CJK token of two
;; characters or more is matched by its bigrams -- wider than a substring, so
;; no hit is lost -- and the tier says which kind of hit it was. Measured on
;; the first version of this: every bigram hit answered the lower tier while a
;; single character answered the higher one, so `\x590d;\x65e7;` ranked below
;; `\x65e7;`; the more specific query ranked lower.
(want "N2b the block holding the phrase outranks the one whose bigrams are merely both present"
      (lines-of (run d2b "search" "\x6062;\x590d;\x65e7;"))
      (list (list 'hit w1 3 "\x6062;\x590d;\x65e7; reaper \x7684;\x505a;\x6cd5;")
            (list 'hit w2 2 "\x6062;\x590d;\x4e86;\x4e00;\x534a;\xff0c;\x590d;\x65e7;\x7684;\x90e8;\x5206;\x6ca1;\x52a8;")))

(want "N2b a camelCase seam is a word boundary, and the query's own case does not matter"
      (list (lines-of (run d2b "search" "block")) (lines-of (run d2b "search" "Block")))
      (list (list (list 'hit w3 3 "parseBlock and friends"))
            (list (list 'hit w3 3 "parseBlock and friends"))))

(want "N2b a fullwidth spelling and an ascii one are one word"
      (lines-of (run d2b "search" "fullwidth"))
      (list (list 'hit w4 3 "\xff26;\xff35;\xff2c;\xff2c;\xff37;\xff29;\xff24;\xff34;\xff28; letters")))

(printf "\n== N2d: a fold that changes the length must not move the positions ==\n")
;; NEVER: THE POSITION OF A MATCH IS A POSITION IN A PARTICULAR STRING.
;; `string-foldcase` is not length-preserving -- German sharp s folds to two
;; letters -- and the match position found in the folded text was used to
;; index the UNFOLDED one. Two consequences, both measured before the fix:
;;
;;   `Stra<sharp-s>e cat`  the boundary was judged on the `a` of `cat` rather
;;                         than its first letter, so the hit scored a tier low
;;   four sharp s and `cat` the folded index ran past the end of the unfolded
;;                         string and `string-ref` RAISED -- the whole search
;;                         answered nothing at all
;;
;; The second is why this row asks for a score rather than only for an answer:
;; a build that raises has no answer to compare, and a build that merely
;; mis-scores does.
(define d2d (fresh-store!))
(init! d2d)
(define sharp1 (insert! d2d "--title" "Stra\xdf;e cat"))
(define sharp2 (insert! d2d "--title" "\xdf;\xdf;\xdf;\xdf;cat"))

(want "N2d a word after a letter that folds to two is still found, at the tier it begins"
      (lines-of (run d2d "search" "cat"))
      (list (list 'hit sharp1 3 "Stra\xdf;e cat")
            (list 'hit sharp2 3 "\xdf;\xdf;\xdf;\xdf;cat")))

(printf "\n== N2e: normalising may not take a hit away, and the parts need not be adjacent ==\n")
;; NEVER: NORMALISING COMPOSES, AND COMPOSING CAN REMOVE A LETTER. A title
;; written as the letter `e` followed by a combining acute is two characters;
;; NFKC makes it one, which no longer contains `e` at all. Measured on the
;; first version of this segment: that block scored 2 for the query `e` before
;; the segment and NOTHING after it -- a hit lost, which is the one thing the
;; whole design promised could not happen.
;;
;; The raw text is asked when the prepared text has no match, at the LOWER
;; tier, which is the score that field had before tiers existed.
(define d2e (fresh-store!))
(init! d2e)
(define acc (insert! d2e "--title" "e\x301;"))

;; NEVER: AND THE EXPECTATION IS THE STRING THAT IS STORED, NOT THE ONE THAT
;; LOOKS THE SAME. The snippet is the title as written -- `e` followed by a
;; combining acute -- and the composed character is a DIFFERENT string that
;; prints identically. The first version of this row expected the composed
;; one and failed with an actual and an expected that could not be told apart
;; by eye, which is the whole subject of this segment arriving in its own cell.
(want "N2e a letter written with a combining mark is still found by the plain letter"
      (lines-of (run d2e "search" "e"))
      (list (list 'hit acc 2 "e\x301;")))

;; NEVER: AND THE SEAM IS WRITTEN BEFORE THE QUERY IS CUT UP. `tokens-of`
;; splits on whitespace and `prepare` WRITES whitespace at a CJK/Latin seam,
;; so preparing after splitting left the seam inside one token, which then had
;; to be found as a single contiguous run. Measured: against a title where the
;; two words are NOT adjacent, the compact query found nothing while the
;; spaced one found the block -- so the equivalence held only when the text
;; happened to be spelled the way the query was.
(define apart (insert! d2e "--title" "\x6062;\x590d;\x65e7; then reaper"))

(want "N2e the two spellings agree even when the words are apart in the text"
      (list (map cadr (lines-of (run d2e "search" "\x6062;\x590d;\x65e7;reaper")))
            (map cadr (lines-of (run d2e "search" "\x6062;\x590d;\x65e7; reaper"))))
      (list (list apart) (list apart)))

;; NEVER: AND THE SNIPPET READS THE UNIT THE SCORE READ. The score reads a
;; field whole; the snippet reads it line by line, so a query whose two
;; bigrams sit on different lines scored a hit and showed the reader nothing.
;; A line is still preferred -- that is what a reader wants -- and the whole
;; field is the fallback rather than a blank.
(define split-lines (insert! d2e "--title" "plain heading"))
(run d2e "set" split-lines "src" "\x6062;\x590d;\n\x590d;\x65e7;")

(want "N2e a hit whose parts are on two lines still shows the reader something"
      (let ((hit (car (reverse (lines-of (run d2e "search" "\x6062;\x590d;\x65e7;"))))))
        (list (cadr hit) (> (string-length (cadddr hit)) 0)))
      (list split-lines #t))

(printf "\n== N2c: the human rendering of a search is hit lines and nothing else ==\n")
;; NEVER: THIS IS A CONTRACT WITH ANOTHER REPOSITORY, AND IT BREAKS ALL AT
;; ONCE. The VS Code plugin runs `search` WITHOUT `--wire` and parses stdout
;; as data (`client.ts readData` -> `parseAnswers`), then `search.ts hitsOf`
;; walks the items: any item that is not a `(hit ...)` makes it `return null`
;; for the WHOLE answer, and the user is told the store did not answer the
;; search. So a helpful extra line -- "no hits; scanned 12 blocks" -- does not
;; get ignored by that reader; it blanks the result.
;;
;; The mechanism is `render.sc:52-58`: for an `items` answer `render-human`
;; writes each item on its own line and drops every other clause. That is why
;; `cut`, `scanned` and `coverage` can only be asked for with `--wire`, and
;; why zero hits has to mean zero output.
(want "N2c zero hits print nothing at all"
      (run d2b "search" "zzzzzzzzzz")
      (list 0 (quote ())))

(want "N2c and every line of a search that does hit reads back as a hit of four parts"
      (let ((items (lines-of (run d2b "search" "reaper"))))
        (list (> (length items) 0)
              (for-all (lambda (item)
                         (and (pair? item)
                              (eq? (car item) 'hit)
                              (= (length item) 4)
                              (string? (cadr item))
                              (integer? (caddr item))
                              (string? (cadddr item))))
                       items)))
      (list #t #t))

(define (whereis-in store q) (lines-of (run store "whereis" q)))

;; How many blocks a store lists, asked of the product rather than counted by
;; hand, so a row can bound something in terms of the store's SIZE instead of a
;; number that happens to be right today.
;; NEVER: AND IT REFUSES TO COUNT FROM A COMMAND THAT FAILED. This ran
;; `outline` and counted newlines in its stdout while IGNORING the exit status
;; -- the binding was even called `ignored`. The first store it was used on is
;; one where `outline` FAILS, which a row in that section asserts two lines
;; further down, so it counted the lines of a one-line error message and
;; answered 1. A bound computed from that cannot bind.
;;
;; A count of blocks has to come from a verb that SUCCEEDED on this store. If
;; none does, the caller must state the number and pin it with a control that
;; uses a verb which does work -- saying so out loud rather than taking a
;; number from whatever happened to print.
(define (blocks-in store)
  (let* ((r (run store "outline" "--depth" "9"))
         (text (text-of out-path)))
    (if (not (= 0 (car r)))
        'outline-failed-so-this-store-cannot-be-counted
        (let loop ((i 0) (n 0))
          (cond ((>= i (string-length text)) n)
                ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
                (else (loop (+ i 1) n)))))))

(printf "\n== N4: where a name is, and what a name match is worth ==\n")
;; A SMALL LIBRARY WITH THREE KINDS OF NAME IN IT: one this library defines
;; and exports, one it defines and keeps to itself, and one it re-exports
;; without defining -- which is what three of this tree's own libraries do,
;; and what makes an index built only from definitions answer `unknown` about
;; a name written plainly in an export list.
(define d4 (fresh-store!))
(init! d4)
(define src4 (string-append scratch "/src4"))
(system (string-append "rm -rf " src4 "; mkdir -p " src4))
(put! (string-append src4 "/own.sc")
      (string->utf8
        (string-append
          "(library (probe own)\n"
          "  (export reaper-start reaper-stop)\n"
          "  (import (rnrs))\n"
          "  (define (reaper-start x) x)\n"
          "  (define (reaper-stop x) x)\n"
          "  (define (helper-only y) y))\n")))
(put! (string-append src4 "/shim.sc")
      (string->utf8
        (string-append
          "(library (probe shim)\n"
          "  (export borrowed-name)\n"
          "  (import (only (elsewhere thing) borrowed-name)))\n")))
(run d4 "import-code" src4 "--datum")

(define (whereis-of q) (lines-of (run d4 "whereis" q)))

(want "N4 a name this library defines is reported as a definition, with its library and kind"
      ;; NEVER: AND THE RECORD NAMES A BLOCK. Every row here used to read the
      ;; tag, the library, the name and the kind, and none of them read the
      ;; second element -- the block id, which is the one part a reader uses to
      ;; go and look. An index answering with the right shape about the wrong
      ;; block passed all of them.
      (let* ((r (whereis-of "helper-only"))
             (id (cadr (car r)))
             (read-back (run d4 "read" id)))
        (list (length r) (car (car r)) (cadr (assq 'library (cddr (car r))))
              (cadr (assq 'name (cddr (car r)))) (cadr (assq 'kind (cddr (car r))))
              (string? id) (= 0 (car read-back))))
      (list 1 'def '(probe own) 'helper-only 'code #t #t))

;; NEVER: A NAME A LIBRARY CARRIES WITHOUT DEFINING IS STILL AN ANSWER. The
;; shim above defines nothing; an index built from definitions alone would say
;; `unknown-name` about a name its one export line spells out. The record says
;; which it is, so the reader knows the definition is somewhere else.
(want "N4 a name only re-exported comes back as an export record, and there is no definition"
      (let ((r (whereis-of "borrowed-name")))
        (list (length r) (car (car r)) (cadr (assq 'library (cddr (car r))))))
      (list 1 'export '(probe shim)))

(want "N4 a name both defined and exported gives the definition first"
      (let ((r (whereis-of "reaper-start")))
        (list (map car r) (cadr (assq 'library (cddr (car r))))))
      (list '(def export) '(probe own)))

(want "N4 a name that is nowhere is refused, and the refusal points at the nearest ones"
      ;; `run` answers (exit-code lines); a refusal is one line, and that line
      ;; is the datum -- so the datum is one `car` further in than the
      ;; item-shaped answers above.
      (let* ((r (run d4 "whereis" "reaper-star"))
             (d (car (cadr r))))
        (list (> (car r) 0) (car d) (cadr d)
              (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                         ;; (error unknown-name <name> (nearest ...)) -- the
                         ;; clauses begin after the name, not after the verb.
                         (cdr (assq 'nearest (cdddr d))))))
      (list #t 'error 'unknown-name '(reaper-start reaper-stop)))

;; NEVER: AND THE INDEX IS NOT A FILE. It is derived from the reduction when
;; it is wanted; an index on disk would be a second copy of the truth with its
;; own staleness. The row counts what is in the store directory before and
;; after asking.
(define (store-files d)
  (let ((out (string-append scratch "/files.txt")))
    (system (string-append "find " d " -type f | sort > " out))
    (text-of out)))

(want "N4 asking where a name is writes nothing into the store"
      (let* ((before (store-files d4))
             (ignored (whereis-of "reaper-start"))
             (after (store-files d4)))
        (list (string=? before after) (> (string-length before) 0)))
      (list #t #t))

;; NEVER: AND IT FOLLOWS THE STORE. Derived after the reduction means a name
;; committed a moment ago is found a moment later, with nothing to rebuild.
(define src4b (string-append scratch "/src4b"))
(system (string-append "rm -rf " src4b "; mkdir -p " src4b))
(put! (string-append src4b "/later.sc")
      (string->utf8 "(library (probe later)\n  (export arrived-late)\n  (import (rnrs))\n  (define (arrived-late z) z))\n"))

(want "N4 CONTROL: the new name is unknown before the import"
      (car (car (cadr (run d4 "whereis" "arrived-late"))))
      'error)

(want "N4 and known immediately after it, with no index to rebuild"
      (begin (run d4 "import-code" src4b "--datum")
             (let ((r (whereis-of "arrived-late")))
               (list (map car r) (cadr (assq 'library (cddr (car r)))))))
      (list '(def export) '(probe later)))

(printf "\n== N4c: what the index refuses to carry, and what it must not drop ==\n")
;; NEVER: A DELETED BLOCK IS NOT AN ANSWER. `state-datum` lists tombstones and
;; `state-outline` does not; the index read the first and checked neither, so
;; a deleted block kept answering for the name it used to define -- measured,
;; `(def "deleted" (library #f) (name ghost) (kind code))`, which sends a
;; reader to a block that is not there.
(define d4c (fresh-store!))
(init! d4c)
(define src4c (string-append scratch "/src4c"))
(system (string-append "rm -rf " src4c "; mkdir -p " src4c))
(put! (string-append src4c "/ghosts.sc")
      (string->utf8
        (string-append
          "(library (probe ghosts)\n"
          "  (export kept)\n"
          "  (import (rnrs))\n"
          "  (define (kept x) x)\n"
          "  (define (ghost y) y))\n")))
(run d4c "import-code" src4c "--datum")

(want "N4c CONTROL: both names are there before anything is deleted"
      (list (car (car (whereis-in d4c "kept"))) (car (car (whereis-in d4c "ghost"))))
      (list 'def 'def))

(want "N4c a deleted block stops answering for the name it defined"
      (let* ((victim (cadr (car (whereis-in d4c "ghost"))))
             (gone (run d4c "del" victim))
             (after (run d4c "whereis" "ghost")))
        (list (car gone)
              (car (car (cadr after)))
              (car (car (whereis-in d4c "kept")))))
      (list 0 'error 'def))

;; NEVER: AND WHAT IS LEFT OF THIS ROW IS ONLY WHAT IT CAN ACTUALLY ASK. It
;; used to read `defs-index-skipped-count` either side of this call and assert
;; `(>= after before)`. Both readings were always 0 -- `whereis-in` runs the
;; product in a `(system ...)` SUBPROCESS and the counter lives in this one --
;; and `(>= 0 0)` holds anyway, so the row said nothing about the skipping it
;; was named for. A mutation that deleted the guard survived and is how that
;; was found. The counting moved to N4g, where the index is built in this
;; process and the numbers mean something; what stays here is the half this
;; store can answer: after one block is deleted, the other still answers.
(want "N4c deleting one block leaves the other one answering"
      (car (car (whereis-in d4c "kept")))
      'def)

(printf "\n== N4g: a block that cannot be read, and the rest of the store ==\n")
;; NEVER: A GUARD NEEDS A CASE BEHIND IT, AND THIS ONE HAD NONE. The per-block
;; guard in the index was written for a real failure: a datum body whose clause
;; was an improper list made the extractor raise, and the condition travelled
;; all the way out to the verb, which answered `(error internal ...)` about a
;; store where every other block was fine.
;;
;; The row written for it measured nothing, and a mutation that DELETED THE
;; GUARD survived to prove it. Three faults, any one of which empties the row:
;;
;;   1. the store it used held no unreadable block at all -- the malformed body
;;      was in the COMMENT, and the fixture built two ordinary definitions;
;;   2. the assertion was `(>= after before)`, which holds when the two sides
;;      are equal, so it could not tell a skip from no skip;
;;   3. and the counters it read live in THIS process while the work happens in
;;      a `(system ...)` SUBPROCESS, so both readings were always 0.
;;
;; The third is the one that made it hollow rather than merely weak, and it
;; came from copying a row in `rpc1` that works -- because `rpc1` reaches the
;; product in process. The same line is sound there and empty here.
;;
;; So this store is used by NOTHING ELSE, which is what lets the build counter
;; below mean "the index was built inside this row".
(define d4g (fresh-store!))
(init! d4g)
(define src4g (string-append scratch "/src4g"))
(system (string-append "rm -rf " src4g "; mkdir -p " src4g))
;; The export list holds a rename clause whose tail is improper. `names-in`
;; takes the rename branch and maps over `(cdr n)`, which is
;; `((inner outer) . oops)` -- and `map` raises on that. It is the smallest
;; thing that reaches the guard, and it reaches it through the field a
;; `library` block really carries.
(put! (string-append src4g "/broken.sc")
      (string->utf8
        (string-append
          "(library (probe broken)\n"
          "  (export kept (rename (inner outer) . oops))\n"
          "  (import (rnrs))\n"
          "  (define (kept x) x)\n"
          "  (define (inner y) y))\n")))
(run d4g "import-code" src4g "--datum")

;; The counting half runs IN THIS PROCESS, because that is where the counters
;; are. The build counter is part of the reading, not decoration: without it a
;; delta of zero skips could mean "nothing was skipped" or "the index was
;; already built and was not rebuilt", and those are different answers.
(want "N4g a block that cannot be read is skipped, counted once, and the others are indexed"
      (let* ((st (open-and-reduce d4g))
             (s0 (defs-index-skipped-count))
             (b0 (defs-index-build-count))
             (ix (defs-index st))
             (s1 (defs-index-skipped-count))
             (b1 (defs-index-build-count)))
        (list (- s1 s0)
              (- b1 b0)
              (if (pair? (hashtable-ref ix "kept" (quote ()))) 'kept-is-indexed 'MISSING)))
      (list 1 1 'kept-is-indexed))

;; And the product half runs through the command line, because what a caller
;; sees is a different question from what a counter says.
(want "N4g and the verb answers about the rest of the store rather than refusing"
      (let ((a (whereis-in d4g "kept")))
        (list (car (car a)) (eq? (car (car a)) 'error)))
      (list 'def #f))

(printf "\n== N4d: the two verbs answer about the same names ==\n")
;; NEVER: ONE STORE, TWO VERBS, ONE ANSWER. `whereis` learned to report a
;; library that CARRIES a name without defining it; `search` did not, so the
;; same store answered "here it is" to one question and "nothing" to the
;; other. They now read one index of names, so a disagreement is a red row
;; rather than something a user discovers.
;;
;; The rename clause is here for the same reason: an export list may hold
;; `(rename (local public))`, where the usable name is the SECOND element, and
;; a guard that wanted a bare symbol threw the whole clause away.
(define d4d (fresh-store!))
(init! d4d)
(define src4d (string-append scratch "/src4d"))
(system (string-append "rm -rf " src4d "; mkdir -p " src4d))
(put! (string-append src4d "/own.sc")
      (string->utf8
        (string-append
          "(library (probe carried)\n"
          "  (export direct (rename (inner outer-alias)))\n"
          "  (import (rnrs))\n"
          "  (define (direct x) x)\n"
          "  (define (inner y) y))\n")))
(put! (string-append src4d "/shim.sc")
      (string->utf8
        (string-append
          "(library (probe carried-shim)\n"
          "  (export only-borrowed)\n"
          "  (import (only (elsewhere thing) only-borrowed)))\n")))
(run d4d "import-code" src4d "--datum")

(want "N4d a name a library only carries is found by BOTH verbs, not one of them"
      (list (car (car (whereis-in d4d "only-borrowed")))
            (> (length (lines-of (run d4d "search" "only-borrowed"))) 0))
      (list 'export #t))

(want "N4d a renamed export is reachable by the name a caller would write"
      (list (car (car (whereis-in d4d "outer-alias")))
            (car (car (whereis-in d4d "inner"))))
      (list 'export 'def))

;; NEVER: AND A REFUSAL ABOUT A NAME IS NOT A REFUSAL ABOUT THE VERB. Both
;; answer `(error ...)`, so a row that checked only the first symbol passed
;; against a build where `whereis` did not exist at all.
(want "N4d the refusal names the missing NAME, not a missing verb"
      (let ((d (car (cadr (run d4d "whereis" "no-such-name-at-all")))))
        (list (car d) (cadr d) (eq? (cadr d) 'unknown-verb)))
      (list 'error 'unknown-name #f))

;; NEVER: TWO SPELLINGS THAT DIFFER BY CASE ARE TWO NAMES. These are Scheme
;; identifiers and the reader keeps their case, so folding here would make the
;; verb answer about a name the store does not hold. The README said the
;; opposite for two rounds -- "must match whole, ignoring case" -- while the
;; index has always been keyed on the exact spelling; nothing measured it, so
;; the sentence and the code were free to disagree.
;;
;; The second half is what makes the refusal usable rather than merely
;; correct: a caller who typed the wrong case is one edit away, so the name
;; they wanted comes back under `nearest`. Exactness without that is just a
;; verb saying no.
(want "N4d a name given in the wrong case is not that name, and nearest says so"
      (let* ((right (car (car (whereis-in d4d "direct"))))
             (d (car (cadr (run d4d "whereis" "Direct"))))
             (near (if (and (pair? d) (eq? (car d) 'error) (= 4 (length d)))
                       (cdr (cadddr d))
                       'NO-NEAREST-CLAUSE)))
        (list right (cadr d) (if (memq 'direct near) 'offers-the-right-spelling near)))
      (list 'def 'unknown-name 'offers-the-right-spelling))

(printf "\n== N4e: the store a caller gets when they type the short command ==\n")
;; NEVER: EVERY CELL IN THIS SEGMENT TOOK THE OPT-IN PATH. `import-code`
;; writes TEXT-mode blocks; `--datum` is the flag, so datum mode is what a
;; caller has to ask for -- and every fixture above asks for it. So the whole
;; segment was measured on the path a caller does NOT take by default, and the
;; default path was broken the entire time in a way nothing could see.
;;
;; A text-mode block's name is derived by its language and comes back as a
;; STRING, where a datum block gives symbols. Measured before the repair:
;;
;;     whereis helper-fn -> (error unknown-name helper-fn (nearest))
;;     search  helper-fn -> no hits
;;
;; about a block the outline lists under exactly that name. Both verbs, one
;; cause, and neither of the two repairs written for this round -- the one
;; that made the verbs agree, and the one that added the singular `name` as a
;; fallback -- did anything at all on this path.
(define d4e (fresh-store!))
(init! d4e)
(define src4e (string-append scratch "/src4e"))
(system (string-append "rm -rf " src4e "; mkdir -p " src4e))
(put! (string-append src4e "/plain.sc")
      (string->utf8
        (string-append
          ";; wombat everywhere in the documentation line\n"
          "(define (helper-fn x)\n"
          "  (let ((note \"wombat appears in the source too\"))\n"
          "    x))\n")))
;; NO `--datum` HERE, AND THAT IS THE POINT OF THE SECTION.
(run d4e "import-code" src4e)

;; CONTROL: the name really is derived from the source rather than stored,
;; which is what makes this a different path and not just a different fixture.
(want "N4e CONTROL: the block is listed under a name nobody wrote into a field"
      (let* ((ignored (run d4e "outline" "--depth" "2"))
             (joined (text-of out-path)))
        (if (holds? joined "helper-fn") 'derived-from-the-source 'MISSING))
      'derived-from-the-source)

(want "N4e a block imported the DEFAULT way answers to the name it defines"
      (list (car (car (whereis-in d4e "helper-fn")))
            (if (> (length (lines-of (run d4e "search" "helper-fn"))) 0)
                'search-finds-it-too
                'SEARCH-DOES-NOT))
      (list 'def 'search-finds-it-too))

;; NEVER: BYTES ARE NOT TEXT. A text block keeps its source -- and the doc
;; derived from it -- as a bytevector, and the fallback that turned a field
;; into a string printed it: the doc entered the search as
;; `#vu8(59 59 32 119 ...)`. That is not a gap, it is a WRONG ANSWER -- the
;; hit came back with a snippet of byte numbers, and a caller searching for a
;; number matched a byte VALUE. Not finding words in that source is the gap;
;; answering with its bytes was the defect.
(want "N4e neither the printed vector nor a byte value inside it is a hit"
      (list (length (lines-of (run d4e "search" "vu8")))
            (length (lines-of (run d4e "search" "119"))))
      (list 0 0))

;; THE GAP IS SHUT, AND THIS ROW IS WHAT SAID SO.
;;
;; It used to read "KNOWN GAP (red when B2 closes it): words in a text
;; block's source are not searched", expecting zero hits, and it was written
;; to go red on the day the gap closed. It did exactly that, on the first run
;; after the source began to be decoded -- which is the whole reason a gap
;; gets a row instead of a sentence in a note.
;;
;; WHAT CLOSED IT WAS NOT A SEARCH CHANGE. A text-mode source is stored as a
;; bytevector, and `field-strings` had no branch for one, so the value was
;; dropped one step before any query saw it. Nothing had ever declined to
;; search it. The repair is in that function, and it is the same `else`
;; branch that was throwing symbol-valued fields away.
;;
;; The row is now positive, and it asks for the block by id rather than for
;; a count: a count of 1 would also be satisfied by some other block
;; matching, and the claim is about THIS source.
;; The word is inside a string literal in the source, so a hit can only come
;; from the source having been read as text. The snippet is asked for too:
;; a count alone would be satisfied by any block matching for any reason.
(want "N4e a word in a text block's source is found, and the hit shows it in context"
      (let ((r (run d4e "search" "appears")))
        (list (car r)
              (> (length (lines-of r)) 0)
              (holds? (text-of out-path) "wombat appears in the source")))
      (list 0 #t #t))

(printf "\n== N4f: search and the index answer about the same blocks ==\n")
;; NEVER: THE SET OF BLOCKS THAT EXIST IS THE OUTLINE, AND SEARCH LEARNED IT
;; SECOND. The index stopped answering about deleted blocks when N4c was
;; written; `store-search` walks the same `state-datum` list and had not, so
;; the two verbs disagreed about a block that is not there. A guard's comment
;; is a map of the entrance nobody guarded.
;;
;; NEVER: AND ONLY SOME BLOCKS HAVE NAMES. `names` is a field, so anything can
;; carry one; a NAME is something a `code` block defines or a `library` block
;; carries. The index applied that rule and search applied none, so a page with
;; a `names` field scored at the top of the name scale while `whereis` said the
;; name did not exist -- reachable with two ordinary commands, no batch and no
;; fixture trickery.
(define d4f (fresh-store!))
(init! d4f)
(define src4f (string-append scratch "/src4f"))
(system (string-append "rm -rf " src4f "; mkdir -p " src4f))
(put! (string-append src4f "/lib.sc")
      (string->utf8
        (string-append
          "(library (probe kept)\n"
          "  (export gremlin-real)\n"
          "  (import (rnrs))\n"
          "  (define (gremlin-real x) x))\n")))
(run d4f "import-code" src4f "--datum")
(define page4f (insert! d4f "--title" "a page about gremlins"))
(run d4f "set" page4f "names" "gremlin")

;; CONTROL: the page IS found, and IS in the outline, before anything is said
;; about how it is scored or what happens when it goes away.
(want "N4f CONTROL: the page is in the outline and search finds it"
      (let* ((ignored (run d4f "outline"))
             (listed (holds? (text-of out-path) "a page about gremlins")))
        (list listed (> (length (lines-of (run d4f "search" "gremlins"))) 0)))
      (list #t #t))

;; The score is the whole point: 3 for the title and nothing for the `names`
;; field, where an exact name match would be 12. Reading the number rather
;; than "is it below the definition" is what keeps this row about the rule
;; instead of about the two blocks that happen to be in this store.
(want "N4f a names field on a page is not a name: prose score only, and both verbs agree"
      (let* ((hits (lines-of (run d4f "search" "gremlin")))
             (page (assoc page4f (map (lambda (h) (cons (cadr h) (caddr h))) hits)))
             (w (car (cadr (run d4f "whereis" "gremlin")))))
        (list (if page (cdr page) 'NOT-A-HIT-AT-ALL) (car w) (cadr w)))
      (list 3 'error 'unknown-name))

(want "N4f CONTROL: the library's real name still answers, at the name tier"
      (let ((hits (lines-of (run d4f "search" "gremlin-real"))))
        (list (car (car (whereis-in d4f "gremlin-real")))
              (> (caddr (car hits)) 3)))
      (list 'def #t))

;; And the deleted block. Both sides are controlled: the outline lists it
;; before, does not list it after, and a SIBLING stays findable -- so a row
;; that went green because search stopped working entirely would still be red.
(want "N4f a deleted block stops being a search result, and its neighbours do not"
      (let* ((before (> (length (lines-of (run d4f "search" "gremlins"))) 0))
             (ignored (run d4f "del" page4f))
             (outlined (begin (run d4f "outline")
                              (holds? (text-of out-path) "a page about gremlins")))
             (after (length (lines-of (run d4f "search" "gremlins"))))
             (sibling (length (lines-of (run d4f "search" "gremlin-real")))))
        (list before outlined after (> sibling 0)))
      (list #t #f 0 #t))

(printf "\n== N4h: the same unreadable block, asked of the OTHER verb ==\n")
;; NEVER: A GUARD PROTECTS THE DOOR THAT HOLDS IT. N4g above proves the index
;; survives a block it cannot read. `search` reached the same parser by a
;; second route that had no guard, and one such block took down EVERY query in
;; that store -- measured, on this very fixture:
;;
;;     whereis kept -> (def "..." (library (probe broken)) (name kept) ...)
;;     search  kept -> (error internal (condition "~s is not a proper list"))
;;     search  x    -> (error internal (condition "~s is not a proper list"))
;;
;; The fixture for the guard was already here; nothing had ever pointed the
;; other verb at it. So this row exists to point it, and both verbs now come
;; through one function that owns the rule and the guard together.
;;
;; The second row needs a query that matches NOTHING, because a store where
;; every query fails looks the same as a store with no matches unless a row
;; asks for the difference between an empty answer and an error.
;;
;; It first used `x` -- which matches, because `x` is the parameter in
;; `(define (kept x) x)` and the printed body is searched. The row went red and
;; the product was right: a one-character query is a substring of almost
;; anything. A CONTROL now states the premise rather than assuming it, so a
;; token that starts matching something cannot quietly turn this row into a
;; test of nothing.
(want "N4h search survives the same block, counts the skip, and still scores the name"
      (let* ((s0 (defs-index-skipped-count))
             (hits (store-search d4g "kept"))
             (s1 (defs-index-skipped-count)))
        (list (- s1 s0)
              (if (pair? hits) 'found-it 'NO-HIT)
              (if (and (pair? hits) (> (cadr (car hits)) 10)) 'at-the-name-tier 'PROSE-ONLY)
              ;; the id is not spelled here -- it is asked of the OTHER verb,
              ;; so the row is about the two agreeing rather than about a
              ;; string this fixture happens to know.
              (if (and (pair? hits)
                       (equal? (car (car hits)) (cadr (car (whereis-in d4g "kept")))))
                  'the-same-block-whereis-names
                  'A-DIFFERENT-BLOCK)))
      (list 1 'found-it 'at-the-name-tier 'the-same-block-whereis-names))

;; The premise is checked against the BYTES this store was built from, which
;; is the whole of what is in it -- the fixture writes that one file and
;; imports it and does nothing else to this store. It is a weaker check than
;; asking the store itself, and it is written down as such: what it rules out
;; is the token quietly becoming present because somebody edited the source
;; above, which is exactly how the previous version of this row rotted.
(want "N4h CONTROL: the token below is absent from the text this store was built from"
      (let ((src (utf8->string (slurp (string-append src4g "/broken.sc")))))
        (list (if (holds? src "qzwxjv") 'THE-TOKEN-IS-PRESENT 'absent)
              (if (holds? src "kept") 'and-the-present-one-is-present 'CONTROL-IS-BROKEN)))
      (list 'absent 'and-the-present-one-is-present))

(want "N4h and a query that matches nothing is an empty answer, not an error"
      (let ((r (run d4g "search" "qzwxjv")))
        (list (car r) (lines-of r)))
      (list 0 '()))

(printf "\n== N4i: a library's own name is not a name it defines ==\n")
;; NEVER: AND A LIBRARY'S `name` IS A LIST OF SYMBOLS. `(probe broken)` is the
;; library's own name, not something it defines, and search put it through the
;; same extractor as a definition -- so each COMPONENT scored at the top of the
;; name scale: measured, `search probe` answered `(hit "..." 12 "probe")` about
;; a library that defines nothing of the sort, while `whereis probe` correctly
;; said the name was unknown. The two verbs read one function now, and its
;; library branch reads `exports` and nothing else.
;; NEVER: AND THIS ROW HAS TO BE ASKED OF A LIBRARY THAT CAN BE READ. It first
;; used `d4g`, whose library is the BROKEN one -- its export list raises, the
;; guard turns the whole block into no names at all, and so the library branch
;; never produces a name there whatever it is told to read. The row was green
;; because nothing in that store matched, not because the repair held: a
;; mutation that made the library branch read `name` as well SURVIVED it.
;;
;; So: a clean library, and a CONTROL that the name component really is
;; reachable in that store -- otherwise `(< top 10)` is true for the same
;; empty reason all over again.
(define d4i (fresh-store!))
(init! d4i)
(define src4i (string-append scratch "/src4i"))
(system (string-append "rm -rf " src4i "; mkdir -p " src4i))
(put! (string-append src4i "/clean.sc")
      (string->utf8
        (string-append
          "(library (marmoset clean)\n"
          "  (export thing)\n"
          "  (import (rnrs))\n"
          "  (define (thing x) x))\n")))
(run d4i "import-code" src4i "--datum")

(want "N4i CONTROL: this library is readable, and the word is in its name"
      (list (car (car (whereis-in d4i "thing")))
            (if (holds? (utf8->string (slurp (string-append src4i "/clean.sc"))) "marmoset")
                'the-component-is-there
                'CONTROL-IS-BROKEN))
      (list 'def 'the-component-is-there))

(want "N4i a library name component scores no name tier, and both verbs agree"
      (let* ((hits (store-search d4i "marmoset"))
             (top (if (pair? hits) (apply max (map cadr hits)) 0))
             (w (car (cadr (run d4i "whereis" "marmoset")))))
        (list (< top 10) (car w) (cadr w)))
      (list #t 'error 'unknown-name))

(printf "\n== N4j: a library that was deleted is not a library to name ==\n")
;; NEVER: AND AN ANCESTOR THAT IS GONE IS NOT THE ANSWER EITHER. Deleting a
;; library block left its name being reported by every definition under it,
;; because the ancestor walk never asked whether each step still exists --
;; measured, after deleting only the library:
;;
;;     (def "cntj2d0l.2" (library (probe gone)) (name kept2) (kind code))
;;
;; The `export` record went, correctly, because that record IS the library.
;; The `def` record went on naming one that is not there.
(define d4j (fresh-store!))
(init! d4j)
(define src4j (string-append scratch "/src4j"))
(system (string-append "rm -rf " src4j "; mkdir -p " src4j))
(put! (string-append src4j "/gone.sc")
      (string->utf8
        (string-append
          "(library (probe gone)\n"
          "  (export kept2)\n"
          "  (import (rnrs))\n"
          "  (define (kept2 x) x))\n")))
(run d4j "import-code" src4j "--datum")
(define lib4j
  (let ((r (whereis-in d4j "kept2")))
    (let loop ((l r))
      (cond ((null? l) 'no-export)
            ((eq? 'export (car (car l))) (cadr (car l)))
            (else (loop (cdr l)))))))

(want "N4j CONTROL: before the delete, both records are there and both name the library"
      (let ((r (whereis-in d4j "kept2")))
        (list (map car r)
              ;; a record is (def <id> (library X) (name n) (kind code)) --
              ;; a list whose second element is a STRING, so it is read by
              ;; position rather than with assq.
              (cadr (caddr (car r)))))
      (list '(def export) '(probe gone)))

(want "N4j after deleting the library, the definition stops naming it and the export is gone"
      (let* ((gone (run d4j "del" lib4j))
             (r (whereis-in d4j "kept2")))
        (list (car gone)
              (map car r)
              (cadr (caddr (car r)))))
      (list 0 '(def) #f))

(printf "\n== N4z: the harness itself, pinned ==\n")
;; NEVER: A ROW THAT READS NOTHING CANNOT TELL "NO OUTPUT" FROM "OUTPUT WITHOUT
;; A NEWLINE". `run` used to stop at end of text without taking what followed
;; the last newline, so a program printing an answer with no trailing newline
;; produced an EMPTY line list. Every row in this file reads those lines, and
;; the answer that arrives last is very often the error -- so the blind spot
;; sat exactly where the bad news does. A reviewer found it by showing a row
;; asserting an empty answer staying green while the program printed
;; `(error internal)`.
;;
;; This row does not go through the product at all: it writes the two shapes
;; itself and asks the harness what it sees, because the question is about the
;; harness. The pair is the reading -- with and without the newline -- since a
;; splitter that dropped BOTH would pass a row that only checked one.
(want "N4z the harness reads a final line whether or not it ends in a newline"
      (let ((with-nl (string-append scratch "/nl-yes.txt"))
            (without (string-append scratch "/nl-no.txt")))
        (put! with-nl (string->utf8 "(one)\n(two)\n"))
        (put! without (string->utf8 "(one)\n(two)"))
        (list (lines-of-text (text-of with-nl))
              (lines-of-text (text-of without))))
      (list '((one) (two)) '((one) (two))))

(printf "\n== N4m: a field whose value is not the shape its branch assumes ==\n")
;; NEVER: A SHAPE IS TESTED, NOT CAUGHT. `field-strings` reads a field in
;; conflict as `(conflict ((<value> <writer> <seq>) ...))` and took the second
;; element and mapped `car` over it. `set <id> title (conflict)` is accepted by
;; the write path, and then that expression raises -- and `store-search` reads
;; titles, srcs and keywords through it, on a block from `state-read`, which
;; the guarded read does not cover because it never goes through the view.
;; One such field took down every query in the store.
;;
;; It is repaired by testing the shape rather than wrapping it in a `guard`.
;; A raise there would say "we wrote the branch wrong" exactly as loudly as
;; "the data is odd", and catching it would file both under the count that
;; means "a block could not be read" -- so a real mistake of ours would hide
;; inside a number. A field whose shape does not fit contributes nothing, and
;; is NOT counted.
(define d4m (fresh-store!))
(init! d4m)
(define d4m-odd (insert! d4m "--title" "zibbet the title word"))
(define d4m-plain (insert! d4m "--title" "plain neighbour"))
;; NEVER: AND THE ROUTE HAS TO BE THE ONE SUCH A VALUE REALLY ARRIVES BY.
;; This used to write `(conflict)` through `batch`, which the write path
;; accepted at the time. It does not any more: a text field's value must be
;; something a reader can turn into text, and `(conflict)` is not. The value
;; is still worth asking about -- more so, because the caller path now being
;; shut means a record is the ONLY way it can reach a field, which is exactly
;; what a foreign or damaged record is.
;;
;; So it is appended to the writer's segment and found by replay. Replay
;; judges nothing, by design: a store already holding such a value has to
;; stay readable, or the rule that refuses the write would cost its owner
;; the library. `forge-record.ss` says how, and why the checksum it writes
;; was measured rather than assumed.
(forge-record! d4m (string-append "(set \"" d4m-odd "\" title (conflict))"))

;; NEVER: AND THE ROUTE DECIDES THE VALUE'S TYPE. The keyword below is written
;; as a STRING because `batch` carries the intent as data -- `keywords
;; odd-keyword` would store the SYMBOL, and `field-strings` reads only strings,
;; so the block would have no searchable keywords and the row below would fail
;; for a reason that has nothing to do with what it is asking. The command line
;; would have converted it; this route does not. That difference is the same
;; one this batch already found in `set <id> kind`, where the command line
;; stored a string and `batch` stored a symbol.
;; NEVER: AND THE CONTROL HAS TO SAY THE ODD VALUE IS REALLY THERE. This row
;; used to read "the store accepted a title that is not the shape the branch
;; assumes", which was true when `batch` would write one and is not true now
;; -- the caller path refuses it and the value arrives by record instead. A
;; title that goes on claiming the old fact would be green on a run where the
;; forged record never landed, and every row under it asks about a block that
;; would then be perfectly ordinary.
;;
;; So it asks two things: the odd value IS in the block, and an ordinary
;; write still lands on that same block.
(want "N4m CONTROL: the odd value is in the block, and ordinary writes still land on it"
      (list (let ((r (run d4m "read" d4m-odd)))
              ;; `run` answers with the PARSED lines, so the raw text is read
              ;; from the output file rather than from the answer.
              (if (and (= 0 (car r)) (holds? (text-of out-path) "(title conflict)"))
                  'the-odd-value-is-there
                  'NOT-THERE))
            (car (run-with-stdin d4m (string-append "((set \"" d4m-odd "\" keywords \"quorvan\"))") "batch"))
            (if (holds? (text-of out-path) "ok") 'the-write-landed 'REFUSED))
      (list 'the-odd-value-is-there 0 'the-write-landed))

(want "N4m search still answers about the other blocks, and exits cleanly"
      (let ((r (run d4m "search" "neighbour")))
        (list (car r) (> (length (lines-of r)) 0)))
      (list 0 #t))

;; NEVER: AND THE TWO TOKENS MUST NOT OVERLAP. The first version searched for
;; `odd` against a keyword `odd-keyword`, and `odd` is a SUBSTRING of it -- so
;; the row that was meant to show the broken title matching nothing found the
;; block through its keyword instead and read 1. Two words with nothing in
;; common now, so each half of the row can only be answered by the field it is
;; about.
(want "N4m and the odd block does not match on that field, while its other fields still do"
      (let* ((by-title (run d4m "search" "zibbet"))
             (by-keyword (run d4m "search" "quorvan")))
        (list (car by-title)
              (length (lines-of by-title))
              (car by-keyword)
              (> (length (lines-of by-keyword)) 0)))
      (list 0 0 0 #t))

;; TWIN: AND A REAL CONFLICT IS STILL READ. Narrowing a test is the easy way to
;; break the path it was protecting, and nothing would say so -- the rows above
;; pass just as well if `field-strings` stopped reading conflicts altogether.
;; A genuine conflict is what `reduce.sc` builds when two writers set one
;; field, so this one is written in that shape by hand.
(define d4m-real (insert! d4m "--title" "placeholder"))
(forge-record! d4m
  (string-append "(set \"" d4m-real
                 "\" title (conflict ((\"winning words\" \"w1\" 1) (\"other words\" \"w2\" 1))))"))

;; NEVER: AND EVERY CLAUSE OF THE SHAPE TEST NEEDS A CASE. The test has four
;; parts -- a pair, the tag, a second element, and a candidate list whose
;; members are pairs -- and the fixture above exercises exactly one of them:
;; `(conflict)` is rejected because there is no second element. A mutation that
;; deleted either of the last two would have survived, not because those checks
;; are wrong but because nothing here asked them anything.
;;
;; So both remaining shapes, each of which raises in a different expression if
;; its clause is removed: `(conflict 5)` reaches `for-all` with a non-list, and
;; `(conflict (1 2))` reaches `(map car ...)` with members that are not pairs.
(define d4m-notlist (insert! d4m "--title" "havrel the second"))
(define d4m-notpairs (insert! d4m "--title" "yompus the third"))
(forge-record! d4m (string-append "(set \"" d4m-notlist "\" title (conflict 5))"))
(forge-record! d4m (string-append "(set \"" d4m-notpairs "\" title (conflict (1 2)))"))

(want "N4m every shape the test rejects leaves search working, and none of them matches"
      (let ((a (run d4m "search" "havrel"))
            (b (run d4m "search" "yompus"))
            (c (run d4m "search" "neighbour")))
        (list (car a) (length (lines-of a))
              (car b) (length (lines-of b))
              (car c) (> (length (lines-of c)) 0)))
      (list 0 0 0 0 0 #t))

;; NEVER: AND THE TAG ITSELF NEEDS A CASE. The three malformed values above all
;; BEGIN with `conflict`, so deleting the tag test changes nothing any of them
;; reads -- a reviewer showed it: with that clause removed those three and the
;; twin are unchanged, while `(not-conflict (("sentinel" "w1" 1)))` goes from
;; contributing nothing to contributing `("sentinel")`.
;;
;; The rule this breaks is one this batch wrote down a round earlier: count the
;; clauses of a guard, then count the shapes the fixture feeds it. It was
;; applied here to "the clauses after the first", and the first was skipped
;; because it looked obviously right. **Obviously right is exactly the kind of
;; reason that leaves a clause with no case.** The tag test is the one most
;; likely to be skipped, because it reads as a definition of what the value IS
;; rather than as a judgement about it.
(define d4m-wrongtag (insert! d4m "--title" "murken the fourth"))
(run-with-stdin d4m
                (string-append "((set \"" d4m-wrongtag
                               "\" title (not-conflict ((\"murken-candidate\" \"w1\" 1)))))")
                "batch")

(want "N4m NEGATIVE TWIN: a value with another tag is not read as a conflict"
      (let ((a (run d4m "search" "murken-candidate"))
            (b (run d4m "search" "neighbour")))
        (list (car a) (length (lines-of a))
              (car b) (> (length (lines-of b)) 0)))
      (list 0 0 0 #t))

(want "N4m TWIN: a field in a real conflict is still searched, on every candidate"
      (let ((a (run d4m "search" "winning"))
            (b (run d4m "search" "other")))
        (list (> (length (lines-of a)) 0) (> (length (lines-of b)) 0)))
      (list #t #t))

(printf "\n== N4n: a word written as a symbol is in the store and cannot be found ==\n")
;; KNOWN OPEN, PINNED SO THAT FIXING IT IS LOUD. `batch` carries an intent as
;; DATA, so `(set <id> title tinvor)` stores the SYMBOL; the command line would
;; have converted it to a string. `field-strings` reads only strings. So a word
;; written by that route is IN the store, the write answers `ok` with exit 0,
;; and no query will ever find it.
;;
;; This is the same family as the naming defect this batch repaired -- a
;; text-mode block's name is a string and a datum block's is a symbol, and the
;; extractor took only one of them. That was the reading side for NAMES; this
;; is the reading side for TEXT, and it is not repaired here: the type rule for
;; field values belongs with the work that rebuilds field reading, and it needs
;; its own twin saying which fields should NOT accept a symbol.
;;
;; What this row is for: a note in a delivery has nobody to shout for it. This
;; goes red on the day the behaviour changes, and red is the reminder. It is
;; the third time this batch has used that shape -- after the text-source gap
;; and the three unnamed refusals -- and the most deserving of the three,
;; because what it pins is SILENT DATA LOSS: the word is there, the write
;; succeeded, and the user will report it as "search is broken".
;;
;; The six tokens are pairwise non-substring on purpose. An earlier row in this
;; file searched `odd` while a keyword read `odd-keyword`, and the half meant to
;; show a broken title matching nothing was answered by the keyword instead.
(define d4n (fresh-store!))
(init! d4n)
(define d4n-ids
  (map (lambda (t) (insert! d4n "--title" t))
       (list "carrier one" "carrier two" "carrier three"
             "carrier four" "carrier five" "carrier six")))
(define (d4n-set! i field value)
  (run-with-stdin d4n
                  (string-append "((set \"" (list-ref d4n-ids i) "\" " field " " value "))")
                  "batch"))
;; NEVER: AND THE CONTROL HAS TO ASSERT THE WRITES, NOT THAT THE BLOCKS EXIST.
;; It used to check that `read` on the six blocks exits 0 -- which is true
;; whether or not the symbol writes were ACCEPTED, because the blocks were made
;; by `insert!` before any of this. So if the type rule is one day settled by
;; REFUSING a symbol-valued text field, those writes would start failing, the
;; searches would still read `(0 1 0 1 0 1)`, and this row would stay green
;; through the very change it exists to announce.
;;
;; A reviewer found it, and the general form is worth more than the fix: **a
;; row that pins a known-open behaviour has to be red for EVERY way the thing
;; could be settled, not for the one the author had in mind.** The fixed action
;; is to list the ways it could be fixed before writing the row, and if some
;; are not covered, to say which.
;;
;; The two ways here: the reader learns to take symbols (the searches change),
;; or the writer refuses them (these responses change). Both are now watched.
(define d4n-writes
  (list (d4n-set! 0 "title"    "tinvor")
        (d4n-set! 1 "title"    "\"pesgul\"")
        (d4n-set! 2 "src"      "raxmid")
        (d4n-set! 3 "src"      "\"qolnev\"")
        (d4n-set! 4 "keywords" "hufpar")
        (d4n-set! 5 "keywords" "\"dewzin\"")))

;; THE RULE IS SETTLED, AND IT WAS SETTLED THE SECOND WAY: the writer
;; refuses a symbol in a text field. The rule itself lives in one place,
;; `text-field-types` in `reduce.sc`, and `field-types.sc` is where it is
;; measured field by field. What stays here is the half this section was
;; always about -- the ROUTE. `batch` carries an intent as data, so it is
;; the only route that can offer a symbol at all, and these rows are what
;; say the refusal is on that route and not only at the command line.
;;
;; NEVER: AND THE ROW THAT PINNED THE OPEN QUESTION COULD NOT SEE ITS OWN
;; ANSWER. It read the six searches and expected `(0 1 0 1 0 1)` -- a symbol
;; is unfindable, a string is not. Under this settlement it still reads
;; `(0 1 0 1 0 1)`, BYTE FOR BYTE: the symbol words never enter the store,
;; so searching for them still finds nothing, for the opposite reason, and
;; the row cannot tell the two reasons apart. That was measured before the
;; change, with a mutation that made the writer refuse -- the row stayed
;; green through the very settlement it existed to announce, and the
;; CONTROL below is what saw it.
(want "N4n the three symbol writes are refused, each by name, and the three string writes land"
      (list (map car d4n-writes)
            (map (lambda (r)
                   (let ((ls (cadr r)))
                     (and (pair? ls) (pair? (car ls)) (eq? 'batch (car (car ls)))
                          (let ((items (cadr (car ls))))
                            (and (pair? items) (pair? (car items))
                                 (car (car items)))))))
                 d4n-writes))
      (list '(1 0 1 0 1 0)
            '(error ok error ok error ok)))

(want "N4n CONTROL: the six blocks are still readable, so the searches below ask about something"
      (let ((codes (map (lambda (i) (car (run d4n "read" (list-ref d4n-ids i)))) '(0 1 2 3 4 5))))
        (list (apply max codes) (length codes)))
      (list 0 6))

;; AND THE SEARCHES READ THE SAME SIX NUMBERS AS BEFORE, which is why they
;; are no longer the row that carries the claim. They are kept because they
;; say the three words that WERE written are findable, and the row above is
;; what says why the other three are not.
(want "N4n the three words that were written are findable, and nothing else is"
      (map (lambda (w) (length (lines-of (run d4n "search" w))))
           '("tinvor" "pesgul" "raxmid" "qolnev" "hufpar" "dewzin"))
      '(0 1 0 1 0 1))

(printf "\n== N4k: the other way a block can be unreadable ==\n")
;; NEVER: A BLOCK IS NOT UNREADABLE IN ONE WAY, AND N4g MODELLED ONLY ONE OF
;; THEM. Its fixture raises while the NAMES are parsed, which the guard used to
;; catch because the guard was around name parsing. A block can also raise
;; while its FIELDS are derived -- before any verb asks for a name -- and that
;; path had no guard at all: the index read `kind` through its own reader
;; BEFORE calling for names, and search read `doc` and `body` AFTER.
;;
;; The shapes are not interchangeable and the difference is one level of
;; nesting. `(fields . broken)` is caught by a `list?` test at the outer level
;; and yields nothing; `(fields (mutable x . broken))` reaches an arity test
;; written as `(> (length field) 3)` behind a `pair?` check, and `length` on an
;; improper list raises. The comment that documented the guard gave the FIRST
;; shape as its example, the fixture was written from the comment, and so the
;; round that added the guard never exercised the case the guard was for.
(define d4k (fresh-store!))
(init! d4k)
;; NEVER: AND THE ROUTE MATTERS, BECAUSE ONE WRITER REFUSES THIS SHAPE. The
;; first version of this fixture built the block with `import-code --datum`,
;; which answers `(error bad-source (reason reader-rejected))` -- the source
;; reader will not read a file containing it, so that route cannot make one and
;; the fixture silently built an EMPTY store. Every row below would have been
;; asking questions of nothing, and passing.
;;
;; `batch` does accept it, and THAT IS CORRECT -- do not go and stop it. A
;; record reaches a store two ways: a caller writes one, or replay brings one
;; from another build. `import-code` is the external-input side and it already
;; refuses this, at the reader. Replay is the other side, and refusing to
;; replay a record is data loss -- which this tree decided against where replay
;; is defined, on the grounds that a store written by a LATER build must still
;; load here. `batch` is the intent layer both arrive through, so it is exactly
;; the wrong place to put that refusal.
;;
;; So the shape is reachable by an ordinary documented verb, the guard is not
;; theoretical, and the next reader who notices that `batch` will store a
;; broken body should read this paragraph before plugging it.
(define d4k-id (insert! d4k "--title" "holder"))
;; The neighbour is what "the rest of the store" MEANS here, so it has to be a
;; block the two verbs can answer about -- a plain inserted block has no name
;; and neither verb would say anything about it, which would make the rows
;; below green for the wrong reason.
(define d4k-neighbour (insert! d4k "--title" "neighbour"))
(run-with-stdin d4k (string-append "((set \"" d4k-neighbour "\" kind code))") "batch")
(run-with-stdin d4k (string-append "((set \"" d4k-neighbour "\" mode datum))") "batch")
(run-with-stdin d4k (string-append "((set \"" d4k-neighbour "\" body (define (ok-name x) x)))") "batch")
(run-with-stdin d4k (string-append "((set \"" d4k-id "\" kind code))") "batch")
(run-with-stdin d4k (string-append "((set \"" d4k-id "\" mode datum))") "batch")
(define d4k-write
  (run-with-stdin d4k
                  (string-append "((set \"" d4k-id
                                 "\" body (define-record-type thing (fields (mutable x . broken)))))")
                  "batch"))

;; CONTROL: the write LANDED. It cannot be confirmed by reading the block back,
;; because `read` is one of the verbs this body takes down -- so the control
;; asks the writer instead, which is the last point at which anything can still
;; answer about it.
(want "N4k CONTROL: the store accepted the body that cannot be read"
      (list (car d4k-write)
            (if (holds? (text-of out-path) "ok") 'the-write-landed 'REFUSED))
      (list 0 'the-write-landed))

;; NEVER: AND THE COUNTER IS IN THIS PROCESS WHILE THE VERB IS NOT. The first
;; version of this row read `defs-index-skipped-count` either side of
;; `whereis-in`, which runs the product in a `(system ...)` SUBPROCESS -- so
;; the counter never moved and the row read `(def #f #t)`. That is the third
;; time in this batch that a call was copied from elsewhere without asking
;; which channel it goes through; the first cost a round, because the row it
;; broke was green instead of red.
;;
;; So the halves are split by channel, as in N4g: the counting runs IN THIS
;; PROCESS where the counter lives, and what a caller sees is asked of the
;; command line.
;; NEVER: AND THE UPPER BOUND WAS COMPUTED FROM A COMMAND THAT FAILS. It used
;; `(blocks-in d4k)` -- and `outline` is one of the three verbs this very store
;; takes down, which the KNOWN OPEN row below asserts. So the count was of a
;; one-line error message, the bound became `d <= 3`, and the limit that was
;; added to stop the counting degenerating unnoticed could never have bound
;; anything. The factor was wrong as well: the INDEX path reads a block at most
;; TWICE -- `fields-of` and `block-names` -- and it is SEARCH that reads three
;; times, with `doc` and `body`. This row measures the index.
;;
;; No listing verb works on this store, so the size is STATED here, and the
;; control above pins it with the verb that does work.
(want "N4k CONTROL: no listing verb can count this store, so the size below is stated"
      (blocks-in d4k)
      'outline-failed-so-this-store-cannot-be-counted)

(want "N4k the index skips the unreadable block, counts it, and still indexes the rest"
      (let* ((n 2)
             (st (open-and-reduce d4k))
             (s0 (defs-index-skipped-count))
             (ix (defs-index st))
             (s1 (defs-index-skipped-count))
             (d (- s1 s0)))
        (list (if (pair? (hashtable-ref ix "ok-name" (quote ()))) 'the-rest-is-indexed 'MISSING)
              (> d 0)
              (<= d (* 2 n))))
      (list 'the-rest-is-indexed #t #t))

(want "N4k and the verb answers about the rest of the store"
      (car (car (whereis-in d4k "ok-name")))
      'def)

(want "N4k search answers about the rest of the store"
      (let ((hits (store-search d4k "ok-name")))
        (list (if (pair? hits) 'found-it 'NO-HIT)
              (if (and (pair? hits) (> (cadr (car hits)) 10)) 'at-the-name-tier 'PROSE-ONLY)))
      (list 'found-it 'at-the-name-tier))

(want "N4k and a search that matches nothing is still an empty answer"
      (let ((r (run d4k "search" "qzwxjv")))
        (list (car r) (lines-of r)))
      (list 0 '()))

;; KNOWN OPEN, MEASURED HERE RATHER THAN LEFT TO BE DISCOVERED. Three verbs
;; read a block without the guard, and on this store all three refuse:
;;
;;     outline      -> (error internal (condition "~s is not a proper list"))
;;     read <id>    -> the same
;;     export-code  -> the same
;;
;; They fail LOUDLY, which is the better half of the news -- a projection that
;; wrote one file fewer and exited 0 would be worse. What is wrong is that the
;; refusal does not say WHICH block: `(error internal (condition ...))` is the
;; same sentence on every path, and it drops the one useful fact at the point
;; where it was still known.
;;
;; NEVER: AND THIS ROW PINS TODAY'S ANSWER, NOT THE RIGHT ONE. It is expected
;; to go red, and the two ways it can go red mean opposite things:
;;
;;   somebody fixed it       -- the refusals name the block, or a verb learns
;;                              to fall back. Change this row, deliberately.
;;   somebody quietly added  -- a guard appears on one of these paths and the
;;   a guard                    verb starts SKIPPING. That would nail "silently
;;                              incomplete" down as the design.
;;
;; The row asserts both halves separately -- that all three refuse, AND that
;; none of them names the block -- so the reading says which half moved. A row
;; that asserted only "it refuses" could not tell the fix from the regression.
;; NEVER: AND THE TITLE OF THIS ROW IS THE PROMISE IT MAKES. What was measured
;; is that an unreadable block is found BEFORE anything is written: the verb
;; refuses and the target directory is empty, whether the broken block is in
;; the middle of the store or the last one in it -- the second case is what
;; tells "all or nothing" apart from "it failed before it got that far".
;;
;; It does NOT say the projection is atomic. A failure during WRITING -- the
;; disk filling on the third file, a target that cannot be written, the process
;; being killed -- would still leave a half-written tree, and this row would be
;; green through all of it. Whether the projection should write to a temporary
;; directory and rename is open, and belongs with the rest of the unreadable-
;; block question rather than here.
(want "N4k an unreadable block is found before anything is written: the verb refuses and the directory is empty"
      (let* ((out (string-append scratch "/n4k-empty"))
             (ignored (system (string-append "rm -rf " out "; mkdir -p " out)))
             (r (run d4k "export-code" out))
             (left (length (filter (lambda (f) (not (member f '("." ".."))))
                                   (directory-list out)))))
        (list (> (car r) 0) left))
      (list #t 0))

;; NEVER: AND EACH VERB'S OUTPUT IS READ BEFORE THE NEXT ONE OVERWRITES IT.
;; This ran all three and THEN read `(text-of out-path)` -- but `run`
;; redirects every call to the same file, so the naming half measured only
;; `export-code`, and it never looked at stderr at all. The consequence is not
;; a wrong answer today: it is that the row would stay GREEN on the day
;; somebody makes `read` name the block, which is the only day it exists for.
;;
;; The fixture already contained the idiom that avoids this, in two other rows
;; -- `(begin (run ...) (text-of out-path))`, each run paired with its own read
;; immediately. A sweep of the file found three rows that call `run` more than
;; once before reading the shared output; those two were safe for exactly that
;; reason, and this one was not.
;;
;; Both streams are read, because a refusal in this tree has appeared on each.
(want "N4k KNOWN OPEN: the three unguarded readers all refuse, and none of them names the block"
      (let* ((names? (lambda (r)
                       (list (> (car r) 0)
                             (if (or (holds? (text-of out-path) d4k-id)
                                     (holds? (text-of err-path) d4k-id))
                                 'names-the-block
                                 'no))))
             (o (names? (run d4k "outline")))
             (r (names? (run d4k "read" d4k-id)))
             (e (names? (run d4k "export-code" (string-append scratch "/n4k-export")))))
        (append o r e))
      (list #t 'no #t 'no #t 'no))

(printf "\n== N4b: the outline calls a code block by the name it defines ==\n")
;; NEVER: AND THIS ONE WAS ALREADY TRUE. D10 asked for it and the outline has
;; been doing it; the row exists because nothing said so, and a listing that
;; quietly went back to `chez:7` would be a worse tree with a green suite.
;; The names below are DERIVED from the source, not stored, so this also
;; pins that the listing reads the view rather than the stored fields.
(want "N4b a code block is listed under the name it defines, not a positional label"
      ;; The outline is TEXT, not items: `run` splits stdout into lines and the
      ;; row wants the characters, so it reads the captured stdout the way the
      ;; other outline rows in this fixture do (line 971) rather than the
      ;; split-up form.
      (let* ((ignored (run d4 "outline" "--depth" "2"))
             (joined (text-of out-path)))
        (list (if (holds? joined "reaper-start") 'names-the-definition 'POSITIONAL-LABEL)
              (if (holds? joined "helper-only") 'and-the-unexported-one-too 'MISSING)))
      (list 'names-the-definition 'and-the-unexported-one-too))

(printf "\n== N5: a name match outranks a mention of the same word ==\n")
;; NEVER: THE BLOCKS COMPETE, AND THE PROSE COMPETITOR IS FULLY LOADED. Four
;; blocks carry the token `reaper-start`: one DEFINES it, one is the library
;; that exports it, one defines a name that BEGINS with it, and one is prose
;; carrying the query in ALL THREE of title, keywords and source.
;;
;; That last one is the row's whole point. The first version of this cell gave
;; prose only a title, and under the scores of the day -- an exact name worth
;; 5 -- it passed while a fully-loaded prose block scored 4 + 3 + 2 = 9 and
;; BEAT the definition. The claim this segment is named for was false and the
;; cell could not see it, because its competitor was too weak to reach the
;; number that breaks it.
(define d5 (fresh-store!))
(init! d5)
(define src5 (string-append scratch "/src5"))
(system (string-append "rm -rf " src5 "; mkdir -p " src5))
(put! (string-append src5 "/defs.sc")
      (string->utf8
        (string-append
          "(library (probe defs)\n"
          "  (export reaper-start reaper-start-all)\n"
          "  (import (rnrs))\n"
          "  (define (reaper-start x) x)\n"
          "  (define (reaper-start-all xs) xs))\n")))
(run d5 "import-code" src5 "--datum")
(define prose5 (insert! d5 "--title" "all about reaper-start"
                        "--keywords" "reaper-start, notes"
                        "--text" "reaper-start appears in the body as well"))

(want "N5 a definition outranks prose that carries the query in every field it has"
      (let* ((r (lines-of (run d5 "search" "reaper-start")))
             (scored (map (lambda (h) (cons (cadr h) (caddr h))) r)))
        (list (map cdr scored)
              (> (cdr (car scored)) (cdr (assoc prose5 scored)))))
      ;; EVERY NUMBER HERE IS DERIVED FROM THE DOCUMENTED RULE, NOT COPIED
      ;; FROM A RUN. Exact name 12, prefix 10, doc and printed body 1 each,
      ;; keywords 4 / title 3 / src 2 at the upper tier:
      ;;
      ;;   the definition        12 (exact name) + 1 (its own body)  = 13
      ;;   the library exporting 12 (exact name), no body of its own = 12
      ;;   the prefix definition 10 (prefix)     + 1 (its own body)  = 11
      ;;   the prose               4 + 3 + 2, and no name at all     =  9
      ;;
      ;; The gap between 11 and 9 is not the point and will close: when a
      ;; text-mode block's source and doc are read, prose reaches 10 and ties
      ;; the prefix match. That tie is the intended answer and was chosen
      ;; against, which is why exact is 12 rather than 10. What the row is for
      ;; is the top of the list.
      (list '(13 12 11 9) #t))

(printf "\n== N5b: how high a block with no name at all can get ==\n")
;; NEVER: THE NAME SCORE IS CHOSEN AGAINST A CEILING, AND NOTHING MEASURED THE
;; CEILING. The first numbers -- exact 5 -- were chosen against a prose block
;; with a title. The second -- exact 10 -- against 4 + 3 + 2 = 9, the best
;; THIS TREE happened to produce that day. Both times the number that mattered
;; was "the most a block can score without a name", and both times it was
;; arrived at by thinking rather than by building the block and reading the
;; number. A ceiling nothing measures moves quietly: when a text block's
;; source and doc are read -- which is what segment B2 does -- prose gains a
;; point, and under the second set of numbers that was a tie with the weakest
;; name match, arriving in a later segment with nothing red to say so.
;;
;; So: build the block, ask for its number, and let the row hold the number
;; down. Every field that can carry a match at once, and no name:
;; keywords 4 + title 3 + src 2 + the printed body 1.
;;
;; `doc` is NOT in that sum and it is worth saying why, because the reason is
;; an accident rather than a rule: a caller cannot set it -- the write path
;; answers `(error malformed-intent (invalid-doc))` -- the datum importer
;; leaves it empty, and on a text-mode block it is a bytevector and is
;; skipped. If any of those three change, this row goes red, and that is the
;; row doing its job.
(define d5b (fresh-store!))
(init! d5b)
(define src5b (string-append scratch "/src5b"))
(system (string-append "rm -rf " src5b "; mkdir -p " src5b))
(put! (string-append src5b "/zeb.sc")
      (string->utf8
        (string-append
          "(library (probe zeb)\n"
          "  (export other zebra)\n"
          "  (import (rnrs))\n"
          "  (define (other q) (list (quote zebra) q))\n"
          "  (define (zebra q) q))\n")))
(run d5b "import-code" src5b "--datum")
;; The loaded block is the one that does NOT define the query: `other`, whose
;; body mentions `zebra` without being called it. Its id comes from asking the
;; product where `other` is defined -- not from a position in the outline,
;; whose row order is not promised, and not from counting inserts.
(define loaded5b
  (let ((r (whereis-in d5b "other")))
    (if (and (pair? r) (pair? (car r)) (eq? 'def (car (car r))))
        (cadr (car r))
        'no-block)))
(run d5b "set" loaded5b "title" "zebra topic")
(run d5b "set" loaded5b "keywords" "zebra, more")
(run d5b "set" loaded5b "src" "zebra in a src field")

;; NEVER: A CONTROL THAT TRIED ONE INVALID VALUE AND CONCLUDED SOMETHING ABOUT
;; ALL OF THEM. This row used to set `doc` to "a zebra doc", read the refusal
;; `(error malformed-intent (invalid-doc))`, and call that "a caller cannot add
;; a doc" -- which is how the ceiling came to be recorded as 10. But `doc` has
;; a GRAMMAR, not a ban: `datum-doc-format?` accepts a string of `;` comment
;; lines each ending in a newline, so the refusal was about the value, not
;; about the field. The review of the previous round is what said so.
;;
;; Both values are here now, because the pair is the reading: the invalid one
;; is refused, the valid one is taken, and the ceiling below is measured with
;; the doc IN PLACE.
(want "N5b CONTROL: doc has a grammar, not a ban -- one value is refused and one is taken"
      (let* ((bad (run d5b "set" loaded5b "doc" "a zebra doc"))
             (refused (or (holds? (text-of err-path) "invalid-doc")
                          (holds? (text-of out-path) "invalid-doc")))
             (good (run d5b "set" loaded5b "doc" ";; zebra\n")))
        (list (> (car bad) 0)
              (if refused 'refused-as-invalid-doc 'ACCEPTED-THE-BAD-ONE)
              (car good)))
      (list #t 'refused-as-invalid-doc 0))

;; Both numbers are derived from the documented rule, not copied from a run,
;; and with the doc now set the sum has every term it can have:
;;
;;   the loaded block, no name   keywords 4 + title 3 + src 2 + doc 1 + body 1 = 11
;;   the definition `zebra`      exact name 12 + its own printed body 1        = 13
;;
;; and the row asks for the ORDER as well, so a change that moved both by the
;; same amount would still have to explain itself. Eleven is the number that
;; matters: it is what an exact name match has to beat, and it is why exact is
;; 12 rather than the 10 it was for part of this batch.
;; NEVER: AND A CEILING MEASURED WITH ONE WORD IS A CEILING FOR ONE WORD. The
;; row below asks a single-token query. A reviewer showed that a per-token doc
;; score -- `(if doc-tier (length tokens) 0)` instead of `(if doc-tier 1 0)` --
;; leaves every number in that row unchanged while a two-word query lifts the
;; same no-name block to 12, above an exact name match. The claim is about a
;; BLOCK's maximum, so it has to be asked with more than one token too.
;;
;; The expectation is derived, not read off a run: every field counts once
;; however many tokens hit it, so two tokens that both land in this block's
;; fields score exactly what one does. The definition keeps its lead.
(want "N5b and the ceiling is the same for a query of two words"
      (let* ((hits (lines-of (run d5b "search" "zebra topic")))
             (scored (map (lambda (h) (cons (cadr h) (caddr h))) hits))
             (mine (assoc loaded5b scored)))
        (if mine (cdr mine) 'NOT-A-HIT))
      11)

(want "N5b the most a block with no name match scores is 11, and a definition is above it"
      (let* ((hits (lines-of (run d5b "search" "zebra")))
             (scored (map (lambda (h) (cons (cadr h) (caddr h))) hits))
             (mine (assoc loaded5b scored))
             (top (apply max (map cdr scored))))
        (list (if mine (cdr mine) 'NOT-A-HIT) top (> top (if mine (cdr mine) 0))))
      (list 11 13 #t))

;; NEVER: AND THE CEILING WAS DERIVED WHEN ONE OF ITS TERMS COULD NOT BE
;; REACHED. `src 2` has been in the sum above since it was written, but a
;; text-mode block stores its source as a BYTEVECTOR, and that value was
;; dropped before scoring -- so for those blocks the term was always zero
;; and no run could have shown it. Now that bytes are decoded, the term is
;; reachable by a second route, and the ceiling has to be re-derived rather
;; than re-asserted.
;;
;; IT IS UNCHANGED, AND THIS IS WHY: decoding did not add a term, it made an
;; existing one reachable. A field counts once, at its own tier, whatever it
;; is stored as -- so the question is whether bytes and text score the SAME,
;; and that is what this pair asks. If they ever differ, the sum above is
;; wrong for one kind of block and the number 11 is about the other one.
(define d5c (fresh-store!))
(init! d5c)
(define d5c-text (insert! d5c "--title" "a carrier"))
(define d5c-bytes (insert! d5c "--title" "a carrier"))
(run-with-stdin d5c
                (string-append "((set \"" d5c-text "\" src \"quoxal at the start\"))")
                "batch")
;; The same nineteen characters, as bytes.
(run-with-stdin d5c
                (string-append "((set \"" d5c-bytes "\" src #vu8("
                               (let loop ((l (bytevector->u8-list (string->utf8 "quoxal at the start")))
                                          (out ""))
                                 (if (null? l)
                                     out
                                     (loop (cdr l)
                                           (string-append out (if (string=? out "") "" " ")
                                                          (number->string (car l))))))
                               ")))")
                "batch")

(want "N5c a source stored as bytes scores exactly what the same source stored as text scores"
      (let* ((hits (lines-of (run d5c "search" "quoxal")))
             (scored (map (lambda (h) (cons (cadr h) (caddr h))) hits))
             (a (assoc d5c-text scored))
             (b (assoc d5c-bytes scored)))
        (list (if a (cdr a) 'TEXT-NOT-A-HIT)
              (if b (cdr b) 'BYTES-NOT-A-HIT)
              (equal? (and a (cdr a)) (and b (cdr b)))))
      (list 2 2 #t))

;; TWIN: AND NEITHER OF THEM REACHES THE NAME TIER. A source that scored as
;; a name would put the ceiling above 11 without any field being added, and
;; the row above would not notice because both halves would move together.
(want "N5c TWIN: a word in a source is a source hit, not a name hit"
      (let* ((hits (lines-of (run d5c "search" "quoxal")))
             (top (apply max (map caddr hits))))
        (list top (< top 10)))
      (list 2 #t))

;; ---- N6: every token must hit, and the two questions are different ------
(printf "\n== N6: a hit needs every token, but not all in one field ==\n")
;; SCORING AND SELECTION ARE TWO PROJECTIONS OF ONE TABLE, and they collapse
;; different axes of it. A field's score is the BEST tier any token reached
;; in that field, which throws away WHICH token. Selection asks whether every
;; token was taken by SOME field, which throws away which field was best.
;;
;; NEVER DERIVE ONE FROM THE OTHER. Asking selection of the collapsed
;; per-field values would look for one field that every token reached, and
;; that is a stricter rule than the one this store has: two tokens landing in
;; two different fields is a hit. A single-token query cannot tell the two
;; rules apart -- with one token, "some field took it" and "one field took
;; them all" are the same sentence -- so the row below uses two tokens that
;; deliberately land apart.
(define d6t (fresh-store!))
(init! d6t)
(define split-block (insert! d6t "--title" "vundrel heading"))
(run-with-stdin d6t
                (string-append "((set \"" split-block "\" src \"a source mentioning glarpis once\"))")
                "batch")
(define other6t (insert! d6t "--title" "an unrelated neighbour"))

(want "N6 CONTROL: each word alone finds the block, and they are in different fields"
      (let ((a (run d6t "search" "vundrel"))
            (b (run d6t "search" "glarpis")))
        (list (length (lines-of a)) (length (lines-of b))))
      (list 1 1))

;; THE ROW THE MATRIX EXISTS FOR. `vundrel` is only in the title and
;; `glarpis` is only in the source; no single field holds both.
(want "N6 two tokens that land in two different fields are a hit"
      (let ((r (run d6t "search" "vundrel glarpis")))
        (list (car r) (length (lines-of r))
              (if (> (length (lines-of r)) 0) (cadr (car (lines-of r))) 'NO-HIT)))
      (list 0 1 split-block))

;; THE OTHER DIRECTION, so that a rule which simply said yes would not pass.
(want "N6 TWIN: a token that no field takes empties the answer, however well the others hit"
      (let ((r (run d6t "search" "vundrel glarpis wexlom")))
        (list (car r) (length (lines-of r))))
      (list 0 0))

;; AND THE SCORE IS STILL THE PER-FIELD BEST, which is the other projection.
;; Title boundary is 3 and source boundary is 2, and a block reached by both
;; scores the sum -- so this row would also notice a selection rule that had
;; quietly started deciding the score.
(want "N6 and the score is the sum of what each field gave, not a count of tokens"
      (let ((r (run d6t "search" "vundrel glarpis")))
        (if (> (length (lines-of r)) 0) (caddr (car (lines-of r))) 'NO-HIT))
      5)

;; ---- N6b: the normalised-text table lives for one search ----------------
(printf "\n== N6b: the text table is built per search, and not kept ==\n")
;; WHY THIS IS PINNED. `store-search` normalises each field's text once and
;; reuses it for every query token; that table is dropped when the search
;; ends. Keeping it across searches is the obvious next thought, and it was
;; tried: with the table keyed by the string OBJECT it never hit once --
;; `state-read` copies every field value, so the key is a different object on
;; every read -- while it went on admitting each fresh copy as a new entry.
;; Measured at 254 MB still held after five queries and a full collection,
;; against 58 MB when the table is dropped per search.
;;
;; So this row is not about speed. It is a tripwire on the shape: without it,
;; somebody moves that table to module scope to "cache across queries", every
;; reading still looks right, and the memory comes back in silence. A table
;; that could be reused across searches needs a key that survives a copy --
;; (block id, field name) rather than the string -- and that is written up as
;; an input to the work on search candidates, not done here.
;;
;; THE ROWS RUN IN THIS PROCESS, not through the command line: the counter is
;; in the library, and a subprocess would take its own copy of it away.
(define d6b (fresh-store!))
(init! d6b)
(define d6b-block (insert! d6b "--title" "brindle the searchable heading"))

(want "N6b CONTROL: the store answers the query at all, so the rows below are about a real search"
      (length (store-search d6b "brindle"))
      1)

;; THE TRIPWIRE ASKS ABOUT IDENTITY, AND IT USED TO ASK ABOUT A COUNT.
;;
;; The count was evadable, and the review said so with a change small enough
;; to apply: `prepared-begin!` is two independent assignments, so deleting
;; the one that replaces the table and keeping the one that increments the
;; counter leaves a module-level table surviving every search -- and all four
;; rows here stayed green while the retention came back. The counter and
;; "is this a different table" were two facts, and only one of them was
;; being asked about.
;;
;; A token minted with the table cannot be separated from it by that edit.
;; `prepared-token-now` exists for this row and for nothing else, which is
;; written at its definition.
(want "N6b two searches in one process do not share a table"
      (let* ((ignored-a (store-search d6b "brindle"))
             (first (prepared-token-now))
             (ignored-b (store-search d6b "brindle"))
             (second (prepared-token-now)))
        (list (eq? first second) (and first #t) (and second #t)))
      (list #f #t #t))

;; The count is kept beside it, no longer carrying the claim. On its own it
;; is satisfied by a build that never replaces the table at all; what it adds
;; is that a table was begun once per search rather than, say, once per
;; block.
(want "N6b and the table is begun exactly once per search"
      (let* ((before (prepared-generation-count))
             (ignored-a (store-search d6b "brindle"))
             (ignored-b (store-search d6b "brindle")))
        (- (prepared-generation-count) before))
      2)

;; NEVER: AND THIS ROW DOES NOT SAY THE TABLE WAS DROPPED, although the
;; first version of its title said exactly that. Measured with a mutation
;; that makes the table survive across searches: this row stayed GREEN and
;; only the build count above went red. The reason is the same copy that
;; runs through this whole note -- a surviving table is keyed by string
;; objects that are new on every read, so the second search misses just as
;; often whether the table was kept or not.
;;
;; What it does establish is that each search does the normalising work
;; itself, and does the same amount of it, which is what makes the build
;; count meaningful rather than a number about an empty table. The row that
;; can tell a kept table from a dropped one is the one above.
(want "N6b each search normalises the text itself, and by the same amount"
      (let* ((h0 (prepared-miss-count))
             (ignored-a (store-search d6b "brindle"))
             (first (- (prepared-miss-count) h0))
             (h1 (prepared-miss-count))
             (ignored-b (store-search d6b "brindle"))
             (second (- (prepared-miss-count) h1)))
        (list (> first 0) (= first second)))
      (list #t #t))

;; TWIN: WITHIN ONE SEARCH THE TABLE IS USED. Without this, a table that was
;; built and never consulted would satisfy both rows above.
(want "N6b TWIN: within one search the table is hit, so it is not merely being built"
      (let* ((h0 (prepared-hit-count))
             (ignored (store-search d6b "brindle heading"))
             (hits (- (prepared-hit-count) h0)))
        (> hits 0))
      #t)

;; ---- N7: grep finds lines, where search finds blocks ---------------------
(printf "\n== N7: grep answers with lines ==\n")
;; `search` ranks blocks; `grep` lists lines. The two answer different
;; questions and carry different item tags, and `facade-gate.sc` keeps the
;; rule that no two verbs share a tag -- because a `(match id n text)` and a
;; `(hit id score snippet)` have the same arity and the same types in the
;; same places, so a reader that checks shape cannot tell them apart.
(define d7 (fresh-store!))
(init! d7)
(define src7 (string-append scratch "/src7"))
(system (string-append "rm -rf " src7 "; mkdir -p " src7))
(put! (string-append src7 "/a.sc")
      (string->utf8
        (string-append
          ";; a file about quenchel\n"
          "(define (one x) x)\n"
          "(define (two y) y)\n"
          ";; QUENCHEL again, shouting\n"
          "(define (three z) z)\n"
          ";; literal marker a.c here\n"
          ";; and a regex would also take abc here\n")))
(run d7 "import-code" src7)

(want "N7 CONTROL: the store has the file, and search finds the block"
      (let ((r (run d7 "search" "quenchel")))
        (list (car r) (> (length (lines-of r)) 0)))
      (list 0 #t))

;; THE LINE NUMBER IS WITHIN THE BLOCK, and the text is the line as written.
(want "N7 a match names the block, the line number and the line"
      (let* ((r (run d7 "grep" "quenchel"))
             (ls (lines-of r)))
        (list (car r)
              (length ls)
              (map car ls)
              (map caddr ls)))
      (list 0 2 '(match match) '(1 4)))

;; CASE IS IGNORED, which is why both lines came back above -- one says
;; `quenchel` and the other `QUENCHEL`.
(want "N7 TWIN: the two lines differ in case, so the match is case-insensitive"
      (let ((ls (lines-of (run d7 "grep" "quenchel"))))
        (map (lambda (l) (holds? (cadddr l) "QUENCHEL")) ls))
      '(#f #t))

;; NEVER: THE PATTERN IS LITERAL, AND THIS IS THE ROW THAT SAYS SO. A `.`
;; is a dot. If a regular expression ever arrives it will be a second
;; language inside this one, and this row is what makes that a decision
;; rather than a drift.
;; The file holds `a.c` on one line and `abc` on another. A literal pattern
;; takes the first and not the second; a regular expression would take both,
;; and that difference is the whole of what this row is for.
;;
;; The first version of this row grepped for `.` alone and asked that every
;; line returned contained a dot -- and the file it ran against had no dots
;; in it at all, so the answer was empty and the second half of the row was
;; vacuously true. It reported `(#f #t)`: the half that failed was the one
;; asking whether anything came back.
(want "N7 a dot matches a dot, not any character"
      (let* ((ls (lines-of (run d7 "grep" "a.c")))
             (texts (map cadddr ls)))
        (list (length ls)
              (for-all (lambda (t) (holds? t "a.c")) texts)
              (exists (lambda (t) (holds? t "would also take abc")) texts)))
      (list 1 #t #f))

(want "N7 TWIN: and a pattern that is in no line answers with nothing"
      (let ((r (run d7 "grep" "zzznotpresent")))
        (list (car r) (length (lines-of r))))
      (list 0 0))

;; THE CROSS-REPOSITORY CONTRACT, for grep as for search: a person who greps
;; for something absent gets no output at all, because the plugin parses
;; this same text and discards the whole answer on any item it cannot read.
(want "N7 zero matches print nothing at all"
      (begin (run d7 "grep" "zzznotpresent") (string-length (text-of out-path)))
      0)

(printf "\n== N7b: the two caps, and what the answer says about them ==\n")
;; One block with more than twenty matching lines, and enough blocks that the
;; total cap is reached as well.
(define d7b (fresh-store!))
(init! d7b)
(define src7b (string-append scratch "/src7b"))
(system (string-append "rm -rf " src7b "; mkdir -p " src7b))
(let loop ((f 0))
  (when (< f 12)
    (put! (string-append src7b "/f" (number->string f) ".sc")
          (string->utf8
            (let build ((i 0) (out ";; frobwick header\n"))
              (if (= i 30)
                  out
                  (build (+ i 1) (string-append out ";; frobwick line " (number->string i) "\n"))))))
    (loop (+ f 1))))
(run d7b "import-code" src7b)

(want "N7b CONTROL: without the caps there are far more than 200 matching lines"
      (let ((n (length (lines-of (run d7b "grep" "frobwick" "--all")))))
        (list (> n 200) n))
      (list #t 372))

;; TWENTY FROM ANY ONE BLOCK, AND TWO HUNDRED IN ALL. The per-block cap is
;; what stops the largest block spending the whole budget: without it, one
;; file of 418 matching lines would fill the answer and the other 38 that
;; matched would not appear, while the answer said only that lines were
;; dropped.
(want "N7b the answer holds 200 lines, and no block contributes more than 20"
      (let* ((ls (lines-of (run d7b "grep" "frobwick")))
             (per (let count ((l ls) (acc '()))
                    (cond ((null? l) acc)
                          (else
                            (let* ((id (cadr (car l)))
                                   (e (assoc id acc)))
                              (count (cdr l)
                                     (if e
                                         (map (lambda (p) (if (equal? (car p) id) (cons id (+ 1 (cdr p))) p)) acc)
                                         (cons (cons id 1) acc)))))))))
        (list (length ls) (apply max (map cdr per)) (length per)))
      (list 200 20 10))

;; AND THE TRUNCATION CARRIES BOTH DIMENSIONS. A count of lines alone cannot
;; say that two blocks matched and showed nothing at all.
(want "N7b the truncated clause names the lines omitted and the blocks unseen"
      (let* ((r (run d7b "grep" "frobwick" "--wire"))
             (answer (car (lines-of r)))
             (t (assq 'truncated (cdr answer))))
        (list (car r) t))
      (list 0 '(truncated (lines 172) (blocks 2))))

(want "N7b TWIN: --all removes both caps and the clause with them"
      (let* ((r (run d7b "grep" "frobwick" "--all" "--wire"))
             (answer (car (lines-of r))))
        (list (car r) (and (assq 'truncated (cdr answer)) #t)))
      (list 0 #f))

(printf "\n== N7c: what --wire adds, and what it does not ==\n")
;; NEVER: `coverage` IS THE STATE OF AN INDEX THE ANSWER CONSULTED, so grep
;; does not get one. It reads the text of every live block and consults no
;; index; `(defs absent)` would be true as English and wrong as a reading --
;; a constant clause that points a caller at something playing no part in
;; the answer. `scanned` is grep's answer to the same question.
;;
;; Somebody will add it back for the sake of one shape for all three verbs.
;; This pair is what says no, and the comment above says why.
(want "N7c grep's empty answer carries cut and scanned, and NO coverage"
      (let* ((r (run d7 "grep" "zzznotpresent" "--wire"))
             (answer (car (lines-of r)))
             (has (lambda (k) (and (assq k (cdr answer)) #t))))
        (list (has 'items) (has 'cut) (has 'scanned) (has 'coverage)))
      (list #t #t #t #f))

(want "N7c TWIN: search's empty answer DOES carry coverage, which is the contrast"
      (let* ((r (run d7 "search" "zzznotpresent" "--wire"))
             (answer (car (lines-of r)))
             (has (lambda (k) (and (assq k (cdr answer)) #t))))
        (list (has 'items) (has 'cut) (has 'scanned) (has 'coverage)))
      (list #t #t #t #t))

;; AND THE CLAUSES ARE EXACT, not merely present.
(want "N7c the scanned clause says how many blocks and which fields"
      (let* ((r (run d7 "grep" "zzznotpresent" "--wire"))
             (answer (car (lines-of r))))
        (assq 'scanned (cdr answer)))
      '(scanned (blocks 2) (fields (src doc body))))

;; NEVER: AND NONE OF THIS REACHES A PERSON. The human rendering of an items
;; answer writes the items and drops every other clause, which is what keeps
;; the zero-output contract true while the machine reading gains clauses.
(want "N7c TWIN: none of those clauses appear without --wire"
      (begin (run d7 "grep" "quenchel")
             (list (holds? (text-of out-path) "scanned")
                   (holds? (text-of out-path) "cut")))
      (list #f #f))

(printf "\n== N7e: the three verbs count the same blocks ==\n")
;; NEVER: ONE CLAUSE NAME, ONE DEFINITION -- AND FOR A WHILE IT HAD THREE.
;;
;; `scanned (blocks n)` was written three times. `grep` counted the blocks it
;; walked, taken from the outline; `search` and `whereis` reported
;; `(length (state-datum state))`, which counts TOMBSTONES -- so a store
;; whose only block had been deleted answered `(scanned (blocks 1))` with no
;; live block left in it.
;;
;; A reader of any ONE of those answers sees nothing wrong. The number is
;; plausible, it is near the size of the store, and nothing in that answer
;; says which of the two things it means. It takes two verbs side by side,
;; over one store, to see it -- which is why this row asks all three at once
;; and why it could not have been a row about `search`.
;;
;; The blocks a verb looked at are the blocks that exist. `whereis` reads an
;; index, and that index is itself built from `state-outline`, so the blocks
;; it covers are the live ones too.
(define d7e (fresh-store!))
(init! d7e)
(define src7e (string-append scratch "/src7e"))
(system (string-append "rm -rf " src7e "; mkdir -p " src7e))
(put! (string-append src7e "/p.sc")
      (string->utf8 ";; a heading\n(define (gnarlwick x)\n  x)\n"))
(run d7e "import-code" src7e)
(define dead7e (insert! d7e "--title" "a block that will be deleted"))
(run d7e "del" dead7e)

(define (scanned-blocks-of . args)
  (let* ((r (apply run d7e (append args '("--wire"))))
         (answer (car (lines-of r)))
         (c (and (pair? answer) (assq 'scanned (cdr answer)))))
    (if c (cadr (assq 'blocks (cdr c))) (list 'NO-SCANNED-CLAUSE answer))))

(define live7e (length (lines-of (run d7e "outline"))))

;; CONTROL: THE STORE REALLY HOLDS A TOMBSTONE. Without one, a count of
;; records and a count of live blocks are the same number and the row below
;; cannot tell the two apart.
;;
;; The first version of this control asked that `read` on the deleted block
;; EXIT NON-ZERO, and it does not: reading a deleted block succeeds and
;; answers `(deleted . #t)`. That is the better reading anyway -- it says the
;; record is still there, which is exactly what a tombstone is, where a
;; refusal would only have said the block is gone.
(want "N7e CONTROL: the store holds a tombstone, so records and live blocks differ"
      (let ((r (run d7e "read" dead7e)))
        (list (> live7e 0)
              (car r)
              (holds? (text-of out-path) "(deleted . #t)")
              (holds? (text-of out-path) "(deleted . #f)")))
      (list #t 0 #t #f))

(want "N7e all three verbs report the same number of blocks, and it is the live count"
      (let ((s (scanned-blocks-of "search" "zzznotpresent"))
            (g (scanned-blocks-of "grep" "zzznotpresent"))
            (w (scanned-blocks-of "whereis" "gnarlwick")))
        (list s g w (and (equal? s g) (equal? g w) (equal? s live7e))))
      (list live7e live7e live7e #t))

;; TWIN: AND THE NUMBER IS NOT SIMPLY ZERO OR CONSTANT. A build that reported
;; 0 everywhere, or the same figure whatever the store, would satisfy the row
;; above perfectly.
(want "N7e TWIN: the number follows the store, and is not a constant"
      (let* ((before (scanned-blocks-of "grep" "zzznotpresent"))
             (extra (insert! d7e "--title" "one more block"))
             (after (scanned-blocks-of "grep" "zzznotpresent")))
        (list (> before 0) (= after (+ before 1))))
      (list #t #t))

(printf "\n== N7d: --under limits the answer to a subtree ==\n")
(want "N7d CONTROL: the file block has a child, and the whole store has both"
      (let ((rows (lines-of (run d7 "outline"))))
        (> (length rows) 1))
      #t)

(want "N7d --under answers only about the block given and what is under it"
      (let* ((all-ids (map cadr (lines-of (run d7 "grep" "quenchel"))))
             (one (car all-ids))
             (under (map cadr (lines-of (run d7 "grep" "quenchel" "--under" one)))))
        (list (> (length all-ids) 0)
              (for-all (lambda (id) (string=? id one)) under)))
      (list #t #t))

;; NEVER: AND THE OPTION HAS TO BE GIVEABLE. `--under` was declared in the
;; usage form and nowhere else, and the parser reads a token as an option
;; only if it appears in its own tables -- so `grep <pattern> --under <id>`
;; arrived as three positionals and was refused for its arity. The option
;; was advertised and could not be given. `describe.sc`'s DS-3b now asks
;; that of every option of every verb; this row is the one that noticed.
(want "N7d TWIN: giving --under is not a usage error"
      (let ((r (run d7 "grep" "quenchel" "--under" "nosuch.1")))
        (list (car r) (length (lines-of r))))
      (list 0 0))

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
(define one-intent "((insert root #f ((kind . section) (title . \"B1\"))))")
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
      '(batch () (done 0)))
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
                        (string-append "(() (insert root #f ((kind . section) (title . \"TB\"))))")
                        "batch" "--req" "r-tb" "--cursor" cursor-WB))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (car (car (cadr a)))
            (list 'not-a-batch (car a) (cadr a))))
      'error)
;; TWIN: and a malformed item AFTER a good one keeps the good one's
;; answer, which is the part a raise used to take away.
(want "TWIN: a good item before a malformed one keeps its answer"
      (let* ((a (cadr (run-piped dB
                        (string-append "((insert root #f ((kind . section) (title . \"TC\")))"
                                       " (insert root))")
                        "batch" "--req" "r-tc" "--cursor" cursor-WB))))
        (if (and (pair? a) (eq? (car a) 'batch) (list? (cadr a)))
            (map (lambda (x) (and (pair? x) (car x))) (cadr a))
            (list 'not-a-batch a)))
      '(ok error))
;; TWIN: and a tracked batch of several well-formed items still runs.
(want "TWIN: a tracked batch of two good items still executes both"
      (let* ((a (cadr (run-piped dB
                        (string-append "((insert root #f ((kind . section) (title . \"TD\")))"
                                       " (insert root #f ((kind . section) (title . \"TE\"))))")
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
                        (string-append "((insert root #f ((kind . section) (title . \"F1\")))"
                                       " (insert (from 0) #f ((kind . section) (title . \"F2\"))))")
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
                         (string-append "((insert root #f ((kind . section) (title . \"J1\")))"
                                        " (insert (from 0) #f ((kind . section) (title . \"J2\")))"
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
                        "((insert root #f ((kind . section) (title . \"Keep\"))) (insert root #f (7)))"
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
                        "((insert root #f ((kind . section) (title . \"Fine\"))))"
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
;; NOTE: IT USED TO BE A CHARACTER, `#\a`, AND THAT STOPPED BEING ONE.
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
;; record codec refuses the write itself. `nesting-depth.sc` pins those
;; three edges; this block only needs one value from the middle band.
(define dF (fresh-store!))
(init! dF)
(define deep-title
  (let loop ((i 0) (out "0"))
    (if (= i 59) out (loop (+ i 1) (string-append "#(" out ")")))))
;; NEVER: AND THE FIELD IT SITS IN IS NOT A TEXT FIELD. This value used to
;; be written as a `title`, which no longer reaches the store: a text
;; field's value must be something a reader can turn into text, and a
;; 59-deep vector is not. The field name was never the subject here -- what
;; these rows ask about is a value the DESCRIBE layer cannot hash -- so it
;; moves to a field the text rule does not cover, and the rows go on asking
;; the question they were written for.
(define odd-intent
  (string-append "((insert root #f ((kind . section) (depth-probe . " deep-title "))))"))
(define plain-intent
  "((insert root #f ((kind . section) (title . \"plain\")))) ")
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
  "((insert root #f ((kind . section) (title . \"one\"))) (insert root))")
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
