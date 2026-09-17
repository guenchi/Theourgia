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

;; The rules that decide whether a request already ran.
;;
;; These are pure functions of a set of records, and the rows below feed
;; them sets built by hand. That is the point: the decision must be the
;; same for the same records however they arrived, so a case that is
;; awkward to produce on disk -- a duplicate history imported twice, a
;; record claiming an index its plan never declared -- is still a case
;; the rules can be asked about directly.
;;
;; THE ORDER OF THE RULES IS THE DESIGN. Asking about a fingerprint
;; before asking whether the evidence can be read at all would answer
;; `req-mismatch` for a store that cannot say what it holds -- and a
;; client would then "fix" its request id and execute the thing a second
;; time. Every row here is as much about which rule answered as about
;; what it said.

(import (chezscheme) (theourgia request) (theourgia store) (theourgia reduce) (theourgia log)
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
;; makes the accessor raise while the row is being computed -- outside
;; anything that was catching. The file then ends where it stood, every
;; row below goes unrun, and the runner sees no `FAIL` at all: a round
;; scored three such defects as crashes with no failures, for answers
;; the store had in fact got right and said plainly.
;;
;; BOTH SIDES, BECAUSE EITHER CAN RAISE. A row whose EXPECTATION is
;; derived from the program's own answer raises while the expectation
;; is built, and ends the file just the same.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded either side.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; outside it, and a raise there still ends the file.
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



(define WHO "agent:claude")
(define AFTER (cons "w3kxxxxx" 12))
(define ID (request-identity AFTER "req-1"))
(define FP (request-fingerprint WHO 'set (list "b" "t" "x") AFTER))
(define PLAN (cons "w3kxxxxx" 13))

;; A record as the request layer sees it: where it was found and whether
;; it has been delivered are part of the evidence, because two of the
;; three sets are defined by exactly those.
(define (ev seq sub payload . opts)
  (let ((placement (if (pair? opts) (car opts) 'valid-history))
        (delivered (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) #t))
        (marks (if (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts))) (caddr opts) '())))
    (make-evidence (cons "w3kxxxxx" seq)
                   (list WHO ID sub FP (if (eq? sub 'single) #f PLAN) AFTER)
                   '() payload placement delivered marks)))
(define (plan-ev entries)
  (make-evidence PLAN (list WHO ID 'plan FP #f AFTER) '()
                 (list 'plan "req-1" FP AFTER entries) 'valid-history #t '()))
(define (decide evidence n) (request-decision ID FP WHO AFTER evidence '() n '()))
(define two (list (cons 0 '(set "b" "t" "x")) (cons 1 '(set "c" "t" "y"))))

;; ⚠️ A `complete` VERDICT CARRIES THE PLAN NOW. It used to be
;; `(complete <present-indices>)`, and the only thing a caller could do
;; with that was answer `incomplete-request`. §7.5.11 makes the caller
;; FINISH the request from the plan's own frozen declaration, so the
;; verdict hands out the plan event the remaining members hang from and
;; the declared intents themselves. The rows below still assert the
;; indices that are present, which is what they were written for.
;; The fifth element is the plan's `consumes` list, or #f when the plan
;; carries none -- these hand-built plans are five-element ones.
(define (completed present entries) (list 'complete present PLAN entries #f))
(define three (append two (list (cons 2 '(set "d" "t" "z")))))

(printf "== U1: no evidence at all is the only time a range is asked ==\n")
;; Rule 2. The range test answers "could this have run where I cannot
;; see?" -- a question that only makes sense when nothing was found. With
;; evidence in hand, unreadable evidence is `unknown` and readable
;; evidence answers for itself.
(want "nothing found and nothing uncertain: run it"
      (decide '() #f)
      '(execute))
(want "nothing found, but the cursor points into an uncertain stretch"
      (request-decision ID FP WHO AFTER '() (list (list "w3kxxxxx" 10 20)) #f '())
      '(unknown (range-overlaps ("w3kxxxxx" 10 20))))
;; A CLOSED INTERVAL BELOW THE CURSOR CANNOT HOLD THIS REQUEST. Its
;; possible positions start after the cursor it was written against.
(want "TWIN: an uncertain stretch that ends before the cursor is not in the way"
      (request-decision ID FP WHO AFTER '() (list (list "w3kxxxxx" 1 5)) #f '())
      '(execute))
(want "and an uncertain stretch on an unrelated writer is not in the way either"
      (request-decision ID FP WHO AFTER '() (list (list "otherwrt" 1 999)) #f '())
      '(execute))
;; A SUCCESSOR'S WHOLE HISTORY IS IN RANGE, not the part above some
;; cursor. When a writer is retired and another takes over, the request
;; never held a cursor on the successor at all -- so every position over
;; there is a position it could have landed at, and a stretch anywhere in
;; it touches. Without this the test answers "clear" for every stretch on
;; every generation after the one the client last spoke to, which is the
;; generation a retry is most likely to be looking at.
(want "the same stretch on a SUCCESSOR of this writer is in the way"
      (request-decision ID FP WHO AFTER '() (list (list "otherwrt" 1 999)) #f
                        (list "otherwrt"))
      '(unknown (range-overlaps ("otherwrt" 1 999))))
;; AND ITS POSITION IN THAT WRITER MAKES NO DIFFERENCE, which is the
;; whole point: a stretch low in the successor would read as "below the
;; cursor" to a test that compared numbers across writers.
(want "wherever in the successor it sits"
      (request-decision ID FP WHO AFTER '() (list (list "otherwrt" 0 1)) #f
                        (list "otherwrt"))
      '(unknown (range-overlaps ("otherwrt" 0 1))))

(printf "\n== U2: unreadable evidence is unknown, never a range ==\n")
;; Rule 1, and it comes first for a reason: a store that cannot say what
;; it holds must not answer a question about what the client did.
(want "a quarantined record"
      (decide (list (ev 14 'single '(set "b" "t" "x") 'quarantined)) #f)
      '(unknown (quarantined ("w3kxxxxx" . 14))))
(want "a torn record"
      (decide (list (ev 14 'single '(set "b" "t" "x") 'torn)) #f)
      '(unknown (torn ("w3kxxxxx" . 14))))
(want "a record readable but outside verifiable history"
      (list (decide (list (ev 14 'single '(set "b" "t" "x") 'incoming)) #f)
            (decide (list (ev 14 'single '(set "b" "t" "x") 'damaged)) #f)
            (decide (list (ev 14 'single '(set "b" "t" "x") 'unlisted)) #f))
      (list '(unknown (unverifiable incoming ("w3kxxxxx" . 14)))
            '(unknown (unverifiable damaged ("w3kxxxxx" . 14)))
            '(unknown (unverifiable unlisted ("w3kxxxxx" . 14)))))
;; THE COMBINATION ROW. With BOTH a different fingerprint and unreadable
;; evidence, the answer is `unknown` -- an implementation that asked
;; about the fingerprint first would answer `req-mismatch` and send the
;; client to change its id.
(want "unreadable evidence outranks a mismatched fingerprint"
      (request-decision ID FP WHO AFTER
                        (list (make-evidence (cons "w3kxxxxx" 14)
                                             (list WHO ID 'single "other-fp" #f AFTER)
                                             '() '(set "b" "t" "x") 'quarantined #t '()))
                        '() #f '())
      '(unknown (quarantined ("w3kxxxxx" . 14))))
(want "TWIN: with the evidence readable, the fingerprint is what answers"
      (request-decision ID FP WHO AFTER
                        (list (make-evidence (cons "w3kxxxxx" 14)
                                             (list WHO ID 'single "other-fp" #f AFTER)
                                             '() '(set "b" "t" "x") 'valid-history #t '()))
                        '() #f '())
      '(req-mismatch ("w3kxxxxx" . 14)))

(printf "\n== U3: membership has three states and the middle one is not a verdict ==\n")
;; A record whose inputs have not arrived is UNDETERMINED: recomputed on
;; every delivery, never marked. Marking it would make the answer depend
;; on the order records happened to arrive in, which is the one thing a
;; distributed log cannot promise.
(want "a member whose plan has not arrived is waiting, not wrong"
      (membership (ev 14 0 '(set "b" "t" "x")) (list (ev 14 0 '(set "b" "t" "x"))))
      'undetermined)
(want "with the plan present and the payload as declared, it is a member"
      (let ((es (list (plan-ev two) (ev 14 0 '(set "b" "t" "x")))))
        (membership (cadr es) es))
      'valid)
;; THE PLAN ARRIVED AND SAID NO. That is not "an input has not arrived":
;; the input came and disagreed. Reading the two as one answer leaves a
;; record claiming a slot its plan never had waiting for ever.
(want "a payload that differs from what the plan declared is wrong, not waiting"
      (let ((es (list (plan-ev two) (ev 14 0 '(set "b" "t" "CHANGED")))))
        (membership (cadr es) es))
      'invalid)
(want "and so is a claim on an index the plan never declared"
      (let ((es (list (plan-ev two) (ev 16 2 '(set "d" "t" "z")))))
        (membership (cadr es) es))
      'invalid)
;; A PLAN DECLARES AN INTENT, NOT A RESULT. "Persist the plan before
;; executing" means the plan says what was MEANT to happen; what it will
;; look like is not knowable before the earlier sub-operations have
;; happened -- a second insert under the same parent gets an `ord` that
;; depends on the first one already being there. So the comparison is not
;; payload against payload: it is "did this record carry out that
;; intent", and which payload fields come from the intent is a table, one
;; row per verb.
;;
;; `("#%new" k)` NAMES THE BLOCK SUB-OPERATION k CREATED, bound from the
;; record that created it -- never from the plan. The id comes from where
;; that record sits: writer and sequence in base36, the same derivation
;; the store uses, so sub-operation 0 at sequence 14 makes `w3kxxxxx.e`.
(define (ins parent . fields)
  (list 'insert parent #f (if (null? fields) '((kind . section)) (car fields))))
(define (put-rec parent ord . fields)
  (list 'put (append (if (null? fields) '((kind . section)) (car fields))
                     (list (cons 'parent parent) (cons 'ord ord)))))
(define new-entries
  (list (cons 0 (ins "root"))
        (cons 1 (ins (list "#%new" 0)))))
(want "with the earlier sub-operation in, the marker binds and the record agrees"
      (let ((es (list (plan-ev new-entries)
                      (ev 14 0 (put-rec "root" '(0 . 1)))
                      (ev 15 1 (put-rec "w3kxxxxx.e" '(0 . 2))))))
        (membership (caddr es) es))
      'valid)
;; TWIN: until that record is here there is nothing to bind the marker
;; to, and the comparison cannot be made -- undetermined, not a verdict.
(want "TWIN: without it there is nothing to bind, and nothing to compare"
      (let ((es (list (plan-ev new-entries)
                      (ev 15 1 (put-rec "w3kxxxxx.e" '(0 . 2))))))
        (membership (cadr es) es))
      'undetermined)
;; TWIN: and a record naming some other block is a real disagreement, not
;; an unbound marker -- otherwise binding would turn every mismatch into
;; "wait and see".
(want "TWIN: bound, and naming a different parent, is invalid"
      (let ((es (list (plan-ev new-entries)
                      (ev 14 0 (put-rec "root" '(0 . 1)))
                      (ev 15 1 (put-rec "w3kxxxxx.zz" '(0 . 2))))))
        (membership (caddr es) es))
      'invalid)
;; AND TWO RECORDS AT ONE INDEX BIND NOTHING. They are a contested slot,
;; reported separately; choosing either would make every payload that
;; mentions the index depend on which was found first.
(want "TWIN: a contested index binds nothing"
      (let ((es (list (plan-ev new-entries)
                      (ev 14 0 (put-rec "root" '(0 . 1)))
                      (ev 17 0 (put-rec "root" '(0 . 9)))
                      (ev 15 1 (put-rec "w3kxxxxx.e" '(0 . 2))))))
        (membership (cadddr es) es))
      'undetermined)

(printf "\n== U3c: which payload fields come from the intent ==\n")
;; `ord` IS DELIBERATELY OUTSIDE THE TABLE. It is decided by the state at
;; the time -- which siblings were there -- and a completion months later
;; must compute it from the siblings it finds, not replay a number from a
;; state nobody has any more. A record whose `ord` differs from what the
;; plan would have produced is still that intent carried out.
(want "a different ord is still the same intent carried out"
      (intent-produced? (ins "root") (put-rec "root" '(9 . 99)))
      #t)
;; TWIN: a field the intent DID name is compared, or the table would be
;; excusing everything rather than excusing one thing.
(want "TWIN: a field the intent named, changed, is not"
      (intent-produced? (ins "root" '((kind . section) (title . "A")))
                        (put-rec "root" '(0 . 1) '((kind . section) (title . "B"))))
      #f)
;; A FIELD THE RECORD CARRIES THAT THE INTENT DID NOT NAME IS NOT THAT
;; INTENT. Membership asks whether the instantiated payload equals what
;; the plan declared; a record carrying something nobody asked for is
;; not what was declared, and calling it so would let a replay hand back
;; a block with content the caller never requested and say "this is your
;; request, already done".
;;
;; THIS ROW USED TO ASSERT THE OPPOSITE, on the grounds that "the record
;; always carries `parent` and `ord`, and comparing whole alists would
;; call every correct execution a mismatch". That reason is true and it
;; is answered by exempting exactly those two -- the row was wider than
;; its own argument, and the width was the defect.
(want "a field the record adds that the intent never named is not that intent"
      (intent-produced? (ins "root" '((kind . section)))
                        (put-rec "root" '(0 . 1) '((kind . section) (title . "A"))))
      #f)
;; AND THE EXEMPTION IS THE WRITE PATH'S OWN LIST, not a second copy of
;; it. Whatever the store supplies for every insert must be exempt here,
;; or the day a third name is added the lagging copy starts calling
;; correct executions mismatches -- and that failure reports a request
;; as never having run, which is the one answer that duplicates work.
;;
;; THE ROW ITERATES THE LIST rather than naming its members, so adding a
;; name to the table extends this row without anybody remembering to.
;;
;; IT ASKS THE REDUCER, NOT THE MEMBERSHIP RULE. Both read the list, and
;; only one of them reads it alone: `parent` carries a rule of its own --
;; it must equal the parent the intent named -- so a record built with a
;; wrong `parent` is refused for that reason and says nothing about the
;; exemption. What the reducer does with these names has no second rule
;; on top: a name in the table is the block's position and never one of
;; its fields. So that is the side the row reads, and the two sides are
;; tied together by sharing the definition rather than by this row
;; checking both.
(want "no field the store supplies for itself is stored as a field"
      (map (lambda (name)
             (let ((r (reduce-empty)))
               (reduce-apply!
                 r "wwwlocl0" 1 '()
                 (list 'put (append '((kind . section) (parent . root) (ord . 0))
                                    (if (memq name '(parent ord))
                                        '()
                                        (list (cons name 7)))))
                 "t")
               ;; A READING, NOT AN EXCEPTION. A build that treated `kind`
               ;; as position would leave no block to look at, and dying
               ;; here would end the file with no failure count -- which
               ;; reads exactly like a run nobody made.
               (let ((bs (state-datum r)))
                 (if (null? bs)
                     'no-block
                     (let ((fields (cadr (assq 'fields (cddr (car bs))))))
                       (and (assq 'kind fields) (not (assq name fields)) #t))))))
           store-supplied-fields)
      (map (lambda (name) #t) store-supplied-fields))
;; THE OTHER ROWS OF THE TABLE. `move` keeps its id and parent and drops
;; its ord; `tag` keeps its name and drops the cut, which is the moment
;; rather than the intent; the rest are carried out verbatim.
(want "move compares id and parent, not ord"
      (list (intent-produced? '(move "b" "p" #f) '(move "b" "p" (3 . 7)))
            (intent-produced? '(move "b" "p" #f) '(move "b" "q" (3 . 7))))
      '(#t #f))
(want "tag compares the name, not the cut it bound"
      (list (intent-produced? '(tag "v1") '(tag "v1" (("w" . 3))))
            (intent-produced? '(tag "v1") '(tag "v2" (("w" . 3)))))
      '(#t #f))
(want "set, del, link and unlink are carried out verbatim"
      (list (intent-produced? '(set "b" title "A") '(set "b" title "A"))
            (intent-produced? '(set "b" title "A") '(set "b" title "B"))
            (intent-produced? '(del "b") '(del "b"))
            (intent-produced? '(link "a" rel "b") '(link "a" rel "b")))
      '(#t #f #t #t))
;; A VERB THE TABLE DOES NOT NAME IS NOT COMPARED LENIENTLY. This build
;; cannot say which of the payload came from the intent, so it cannot say
;; the record carried the intent out.
(want "a verb the table does not name is never called a match"
      (intent-produced? '(frobnicate "b") '(frobnicate "b"))
      #f)

(printf "\n== U4: duplicates are found before order is asked ==\n")
;; Filtering by order first would silently keep whichever copy sat where
;; the order test wanted and apply it -- a duplicate history imported
;; twice would be applied once, quietly, with nothing recording the
;; choice.
(want "two records claiming one slot make the request unknown"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x")) (ev 17 0 '(set "b" "t" "x"))) 2)
      '(unknown (plan-conflict)))
(want "and both of them are named, so neither can be applied"
      (let ((es (list (plan-ev two) (ev 14 0 '(set "b" "t" "x")) (ev 17 0 '(set "b" "t" "x")))))
        (list-sort < (map (lambda (e) (cdr (ev-event e))) (plan-conflicts es))))
      '(14 17))
(want "TWIN: one record per slot is no conflict"
      (let ((es (list (plan-ev two) (ev 14 0 '(set "b" "t" "x")) (ev 15 1 '(set "c" "t" "y")))))
        (plan-conflicts es))
      '())

(printf "\n== U5: a hole in a plan is not a completion ==\n")
;; Sub-operations are written in order, so `{0,2}` is a set no correct
;; execution produces. Appending 1 after 2 would put the history in an
;; order its author never had.
(want "a prefix is completed from where it stopped"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))) 2)
      (completed '(0) two))
(want "a hole is refused, and the present set is named"
      (decide (list (plan-ev three) (ev 14 0 '(set "b" "t" "x")) (ev 16 2 '(set "d" "t" "z"))) 3)
      '(unknown (plan-order (0 2))))
;; AND IT NAMES THE LAST SUB-OPERATION -- the furthest point the promise
;; has to reach, and the record a caller must make durable before
;; repeating the word.
(want "the whole plan present is a replay, named at its last sub-operation"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x")) (ev 15 1 '(set "c" "t" "y"))) 2)
      '(replay ("w3kxxxxx" . 15)))
;; A REQUEST OF EXACTLY ONE SUB-OPERATION HAS NO PLAN, and an empty plan
;; is itself the whole evidence: both are complete the moment they are
;; present.
;; An empty plan is complete the moment it is present, and there is no
;; single record to name -- so that one answers bare.
(want "a single record, and an empty plan, are each complete alone"
      (list (decide (list (ev 14 'single '(set "b" "t" "x"))) #f)
            (decide (list (plan-ev '())) 0))
      (list '(replay ("w3kxxxxx" . 14)) '(replay ("w3kxxxxx" . 13))))
;; A RECORD A RESOLUTION SET ASIDE DID NOT RUN. When a resolution names
;; the real execution among contested candidates, the others are marked
;; `superseded` -- that mark is the store saying "not this one". Counting
;; such a record towards the sub-operations that were applied would let a
;; record known not to have run complete the request, and the completion
;; would then skip the index it stands at. Note where the mark has to be
;; read: this record has a plan above it and agrees with it, so
;; `membership` answers `valid`, and the mark is the only thing that
;; says otherwise.
(want "a superseded record is not one of the sub-operations that ran"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (ev 15 1 '(set "c" "t" "y") 'valid-history #t '(superseded)))
              2)
      (completed '(0) two))
(want "TWIN: the same record without the mark completes the plan"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (ev 15 1 '(set "c" "t" "y")))
              2)
      '(replay ("w3kxxxxx" . 15)))
(want "CONTROL: prefix? tells the two shapes apart"
      (list (prefix? '()) (prefix? '(0)) (prefix? '(0 1 2)) (prefix? '(0 2)) (prefix? '(1)))
      (list #t #t #t #f #f))

(printf "\n== U6: the scan looks everywhere a record can be ==\n")
;; Looking only where delivery looks would answer "never ran" for a
;; request whose record is lying in plain sight one directory away -- and
;; "never ran" means run it again. So the scan reads the segments the
;; manifest lists, the ones it does not, damaged/ and incoming/, and
;; carries WHERE each was found, because that is what `unknown` has to be
;; able to name.
;;
;; THE IDENTITY SELECTS, NOT THE WRITER. A record naming this request is
;; evidence wherever it sits -- which is what makes the answer survive an
;; adopt, where the records are carried by a successor.
(define scratch2 (test-dir "q2store"))
(define home2 (string-append scratch2 "/home"))
(define WW "wwwlocl0")
(define MM "mirrorzz")
(define (put-file! p b)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o b))))
(define S-AFTER (cons WW 0))
(define S-ID (request-identity S-AFTER "req-1"))
(define S-FP (request-fingerprint WHO 'set (list "b" "t" "x") S-AFTER))
(define (areq sub) (list WHO S-ID sub S-FP #f S-AFTER))
(define (rq seq sub)
  (encode-record seq (+ 1757300000000 seq) (areq sub) '()
                 (storable-encode '(put ((kind . section))))))
(define store2
  (let ((d (string-append scratch2 "/store")))
    (system (string-append "rm -rf " d " " home2 "; mkdir -p " d "/writers/" WW " "
                           d "/writers/" MM "/damaged " d "/writers/" MM "/incoming "
                           d "/snap " home2))
    (put-file! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"ev\"))\n"))
    (call-with-port (open-file-output-port (string-append d "/lock") (file-options no-fail))
      (lambda (p) #f))
    (put-file! (string-append d "/writers/" WW "/owner.sexp")
               (string->utf8 "((machine \"m\"))\n"))
    (putenv "THEOURGIA_HOME" home2)
    (let ((k (instance-install! d))) (owner-install! d WW k))
    (put-file! (string-append d "/writers/" WW "/" (segment-file-name 1)) (make-bytevector 0))
    (let ((b (rq 1 'single)))
      (log-publish! d MM 1 b (bytevector->hex (sha256 b))))
    (put-file! (string-append d "/writers/" MM "/damaged/000009.sexp.1.0") (rq 9 0))
    (put-file! (string-append d "/writers/" MM "/incoming/000008.sexp.aa.seg") (rq 8 1))
    ;; a record of a DIFFERENT request, which must not be collected
    (put-file! (string-append d "/writers/" MM "/incoming/000007.sexp.bb.seg")
               (encode-record 7 1757300007000
                              (list WHO (cons WW "other-req") 'single S-FP #f S-AFTER) '()
                              (storable-encode '(put ((kind . section))))))
    d))
(define found (store-evidence store2 S-ID))

(want "every record naming the identity is found, wherever it sits"
      (list-sort < (map (lambda (e) (cdr (ev-event e))) found))
      (list 1 8 9))
(want "three places, and the place travels with each"
      (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                 (map (lambda (e) (cons (ev-placement e) (cdr (ev-event e)))) found))
      (list (cons 'damaged 9) (cons 'incoming 8) (cons 'valid-history 1)))
;; A RECORD OF ANOTHER REQUEST SITTING IN THE SAME DIRECTORY IS NOT
;; EVIDENCE. The identity is what selects.
(want "and a record of a different request in the same directory is not collected"
      (length found)
      3)
(want "only the one in the valid history counts as delivered"
      (map (lambda (e) (cons (cdr (ev-event e)) (ev-delivered? e)))
           (list-sort (lambda (a b) (< (cdr (ev-event a)) (cdr (ev-event b)))) found))
      (list (cons 1 #t) (cons 8 #f) (cons 9 #f)))
;; AND THE DECISION IS `unknown`, NOT "NEVER RAN". A copy in damaged/ is
;; a record this store can read and cannot verify -- the one case where
;; answering "no" would run the request twice.
(want "a readable record outside verifiable history makes the answer unknown"
      (request-decision S-ID S-FP WHO S-AFTER found '() #f '())
      '(unknown (unverifiable damaged ("mirrorzz" . 9))))
(want "TWIN: with only the published record, the same request is a replay"
      (request-decision S-ID S-FP WHO S-AFTER
                        (filter (lambda (e) (eq? (ev-placement e) 'valid-history)) found)
                        '() #f '())
      '(replay ("mirrorzz" . 1)))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q2 complete\n")
