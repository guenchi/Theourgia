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

;; Deterministic reduction: candidates, tombstones, edges, order
;; (plan R1 R2 R4 R5 R7 R11, design 9.1 through 9.5).
;;
;; A WRITE DOES NOT OVERWRITE. It removes the candidates it could have
;; seen and leaves the rest, so two writers who did not see each other
;; leave two candidates and that is a conflict -- not a race whose winner
;; depends on which segment was enumerated first. Every row here is some
;; consequence of that.
(import (chezscheme) (theourgia reduce)
        (only (theourgia wire) sexpr->string-extended))

;; The work directory is decided at run time; see the note in the log
;; fixtures.
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

;; The fixtures name events the way the plan does -- a.1, b.2 -- and the
;; block ids follow from them by the derivation rule.
(define (feed! r . events)
  (for-each (lambda (e) (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))) events)
  r)
(define (field-of r id name)
  (let* ((b (state-read r id)) (fs (and b (cdr (assq 'fields b)))))
    (and fs (let ((e (assq name fs))) (and e (cdr e))))))

(define (member? lst x y)
  (let loop ((l lst) (seen-x #f))
    (cond ((null? l) #f)
          ((equal? (car l) y) seen-x)
          ((equal? (car l) x) (loop (cdr l) #t))
          (else (loop (cdr l) seen-x)))))

(printf "== R1: field candidates ==\n")
;; (a) one writer, three events in sequence: each supersedes the last,
;; because a writer's own predecessors are always in its past.
(want "sequential writes leave exactly one candidate"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("a" 3 () (set "a.1" title "q")))
        (field-of r "a.1" 'title))
      "q")
;; (b) two writers who did not see each other: two candidates, and the
;; read says conflict rather than picking one.
(want "concurrent writes leave both candidates, in event order"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("b" 1 (("a" . 1)) (set "a.1" title "r")))
        (field-of r "a.1" 'title))
      '(conflict (("p" "a" 2) ("r" "b" 1))))
;; (c) a write that saw both resolves it -- and resolution is an ordinary
;; write, not a special operation.
(want "a write whose past covers both candidates settles it"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("a" 3 (("b" . 1)) (set "a.1" title "s")))
        (field-of r "a.1" 'title))
      "s")
;; (d) three concurrent writers: three candidates. Two is not a special
;; case of the rule.
(want "three concurrent writers leave three candidates"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("c" 1 (("a" . 1)) (set "a.1" title "t")))
        (field-of r "a.1" 'title))
      '(conflict (("p" "a" 2) ("r" "b" 1) ("t" "c" 1))))
;; (f) a different field is not a conflict -- supersession is per field.
(want "CONTROL: concurrent writes to different fields do not conflict"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("b" 1 (("a" . 1)) (set "a.1" summary "r")))
        (list (field-of r "a.1" 'title) (field-of r "a.1" 'summary)))
      (list "p" "r"))

;; (e) TRANSITIVITY. c saw b, and b saw a -- so c's past contains a even
;; though c never names it. Without the transitive closure a's candidate
;; survives and the field reads as a conflict nobody is in.
;; THE INTERVENING WRITES TOUCH A DIFFERENT FIELD, which is what makes
;; the closure matter at all. If they had written `title` they would have
;; superseded the old candidate themselves and c would only need to see
;; the nearest one -- so a version that never walks past its direct deps
;; would pass. Here the only thing that can remove a.1's title is c's
;; past reaching a.1 through b and a.2.
(want "a past reaches through the events it saw, not only the ones it names"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (set "a.1" summary "p"))
               '("b" 1 (("a" . 2)) (set "a.1" summary "r"))
               '("c" 1 (("b" . 1)) (set "a.1" title "t")))
        (field-of r "a.1" 'title))
      "t")
(want "CONTROL: a writer that never saw the block's creation conflicts with it"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (set "a.1" summary "p"))
               '("b" 1 (("a" . 2)) (set "a.1" summary "r"))
               '("c" 1 () (set "a.1" title "t")))
        (field-of r "a.1" 'title))
      '(conflict (("x" "a" 1) ("t" "c" 1))))

;; (e) A WRITE THAT SAW ONLY ONE OF THE TWO SETTLES ONLY THAT ONE. c
;; names a.2 and nothing else, so a.2's candidate goes and b.1's stays.
;; An implementation that treats any later write as a resolution -- or
;; that resolves whenever the writer's past is non-empty -- leaves one
;; candidate here and passes (c) above, which is why (c) is not enough.
(want "a write settles only the candidates its own past covers"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("c" 1 (("a" . 2)) (set "a.1" title "t")))
        (field-of r "a.1" 'title))
      '(conflict (("r" "b" 1) ("t" "c" 1))))

;; (g) A WRITER INHERITS THROUGH ITS OWN PREDECESSOR. a.3 declares no
;; deps at all, but a.2 saw b.1, and a.2 is a.3's premise whether it
;; says so or not -- so a.3's past contains b.1 and its title wins
;; outright. An implementation that unions the explicit deps with the
;; writer's own earlier event-ids, and stops there, has a.3's past as
;; {a.1, a.2} and reports a conflict with b.1.
(want "an empty deps list still inherits what the writer's last event saw"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("a" 2 (("b" . 1)) (set "a.1" summary "p"))
               '("a" 3 () (set "a.1" title "s")))
        (field-of r "a.1" 'title))
      "s")
(want "CONTROL: without the intervening event the same a.3 conflicts"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("a" 2 () (set "a.1" summary "p"))
               '("a" 3 () (set "a.1" title "s")))
        (field-of r "a.1" 'title))
      '(conflict (("s" "a" 3) ("r" "b" 1))))

(printf "== R11: the two-argument set ==\n")
;; ABSENT IS A VALUE. Removing the candidates instead would make the
;; deletion invisible to a concurrent write, which would then look like
;; the only candidate rather than one of two.
;; A DELETED FIELD IS NOT REPORTED BY read -- it is gone, and a reader
;; asking what the block says should not be told about it. It is still in
;; the canonical state, because a concurrent write has to be able to
;; conflict with the deletion.
;; The marker is a tagged list, matching the codec's convention for
;; values that need a name the printer can carry.
(define absent-marker (list "#%absent"))
(want "a two-argument set removes the field from what a read reports"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (foo . 1))))
               '("a" 2 () (set "a.1" foo)))
        (list (field-of r "a.1" 'foo)
              (cadr (assq 'fields (cddr (car (state-datum r)))))))
      (list #f (list (list 'foo (list (cons absent-marker (cons "a" 2))))
                     (list 'kind (list (cons 'section (cons "a" 1)))))))
;; AND IT IS NOT THE SAME AS ANY ORDINARY VALUE. Assigning the symbol
;; #%absent by hand is the one case that could collide, so it is here
;; beside the others.
(want "deleting, false, the empty list and the literal symbol are four states"
      (let ((mk (lambda (payload)
                  (let ((r (reduce-empty)))
                    (feed! r
                           '("a" 1 () (put ((kind . section) (foo . 1))))
                           (list "a" 2 '() payload))
                    (block-hash r "a.1")))))
        (let ((a (mk '(set "a.1" foo)))
              (b (mk '(set "a.1" foo #f)))
              (c (mk '(set "a.1" foo ())))
              (d (mk (list 'set "a.1" 'foo (list "#%absent")))))
          (list (equal? a b) (equal? a c) (equal? b c) (equal? a d))))
      (list #f #f #f #t))
(want "a two-argument set concurrent with a write conflicts like any other"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (foo . 1))))
               '("a" 2 () (set "a.1" foo))
               '("b" 1 (("a" . 1)) (set "a.1" foo 1)))
        (field-of r "a.1" 'foo))
      (list 'conflict (list (list absent-marker "a" 2) (list 1 "b" 1))))

(printf "== R4: tombstones ==\n")
;; PERMANENT, NOT CASCADING, NOT REVIVABLE -- and it does not clear the
;; fields: a later write is still recorded as evidence of what someone
;; believed, even though the block is gone.
;; The later write SAW the put -- its deps name a.1 -- so it supersedes
;; that title; what the row establishes is that it was recorded at all,
;; against a block that is already dead. The tombstone does not clear
;; fields and does not stop evidence accumulating.
(want "a deleted block is still readable, marked, and later writes still land"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (del "a.1"))
               '("b" 1 (("a" . 1)) (set "a.1" title "y")))
        (let ((b (state-read r "a.1")))
          (list (cdr (assq 'deleted b)) (field-of r "a.1" 'title))))
      (list #t "y"))
;; AND THE FIELDS THE DELETE DID NOT TOUCH ARE STILL THERE. Clearing them
;; would lose what the block was, which is exactly what someone reading a
;; deleted block wants to know.
(want "a field written before the delete survives it"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (del "a.1")))
        (list (field-of r "a.1" 'kind) (field-of r "a.1" 'title)))
      (list 'section "x"))
(want "and it is not listed in the outline"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (del "a.1")))
        (state-outline r))
      '())
(want "CONTROL: an undeleted block is listed"
      (let ((r (reduce-empty)))
        (feed! r '("a" 1 () (put ((kind . section)))))
        (state-outline r))
      '((root 0 "a.1")))

(printf "== R5: the edge set ==\n")
(want "link then unlink by the same writer leaves no edge"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (link "S" explains "T"))
               '("a" 2 () (unlink "S" explains "T")))
        (cdr (assq 'links (state-dump r))))
      '())
;; THE UNLINK REMOVES WHAT IT SAW. A link it never saw survives, which is
;; what stops one writer's removal from silently undoing another's
;; addition.
(want "an unlink does not remove a concurrent link it never saw"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (link "S" explains "T"))
               '("b" 1 () (link "S" explains "T"))
               '("a" 2 () (unlink "S" explains "T")))
        (map (lambda (l) (list (car l) (cadddr l)))
             (cdr (assq 'links (state-dump r)))))
      '(("S" ("b" . 1))))
(want "removing an edge that is not there is not an error"
      (let ((r (reduce-empty)))
        (feed! r '("a" 1 () (unlink "S" explains "T")))
        (cdr (assq 'links (state-dump r))))
      '())

(printf "== R7: tags ==\n")
;; THE WHOLE CANDIDATE SET, values and event ids. A count of one is
;; satisfied by keeping the WRONG one, which is the failure a resolution
;; rule can actually have.
(define (tags-of r name)
  (let ((e (assoc name (cdr (assq 'tags (state-dump r))))))
    (and e (map (lambda (c) (list (car c) (car (cdr c)) (cdr (cdr c)))) (cdr e)))))
(want "concurrent tags of one name leave both candidates"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (tag "t" (("a" . 1))))
               '("b" 1 () (tag "t" (("b" . 1)))))
        (tags-of r "t"))
      ;; Sorted by event id, not by arrival: the dump is a function of
      ;; the state, so two libraries holding these records print the same
      ;; thing however the records reached them.
      (list (list '(("a" . 1)) "a" 1) (list '(("b" . 1)) "b" 1)))
(want "and a write covering both leaves exactly the new one"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (tag "t" (("a" . 1))))
               '("b" 1 () (tag "t" (("b" . 1))))
               '("a" 2 (("b" . 1)) (tag "t" (("a" . 2)))))
        (tags-of r "t"))
      (list (list '(("a" . 2)) "a" 2)))

(printf "== R2: the total order, and the bucket ==\n")
;; Stern-Brocot rather than the midpoint: repeated midpoints double the
;; denominator each time and the number stops fitting anything.
;; NOT THE MEDIANT OF THE ENDPOINTS. That is the same answer on (0,1)
;; and wrong as soon as they are not neighbours in the tree: between 1/5
;; and 1/2 the mediant is 2/7, while 1/3 is simpler and sits in the same
;; gap. Repeated, that is exactly the denominator growth the rule exists
;; to avoid.
(want "the simplest rational in the gap, not the mediant of its ends"
      (list (ord-between 0 1) (ord-between 0 1/2) (ord-between 0 1/3)
            (ord-between 1/5 1/2) (ord-between 1/4 1/2)
            (ord-between -1/2 -1/5) (ord-between -1/2 1/2))
      (list 1/2 1/3 1/4 1/3 1/3 -1/3 0))
(want "and a gap that straddles zero has zero in it, however narrow"
      (let ((tiny (/ 1 (expt 2 128))))
        (ord-between (- tiny) tiny))
      0)
;; AN OPEN END TAKES AN INTEGER, and that IS the smallest denominator:
;; between 3 and infinity the simplest rational is 4, and below 1 it is
;; 0 -- descending the tree would have answered 1/2, whose denominator is
;; larger than an integer that fits just as well.
(want "an open end takes an integer, at either end"
      (list (ord-between 3 #f) (ord-between #f 1) (ord-between #f #f)
            (ord-between 5/2 #f))
      (list 4 0 0 3))
(want "a gap that needs a denominator past the limit is refused"
      (let ((tiny (/ 1 (expt 2 128))))
        (ord-between 0 tiny))
      '(refused too-deep))
;; A BUCKET IS NOT AN ERROR. Two blocks share an ord because they were
;; inserted concurrently into one gap; v1 declines to order within it and
;; names the two places that ARE available.
(want "asking to insert inside a bucket names the two places that exist"
      (list (ord-between 1 1) (ord-between 3/2 3/2))
      (list '(refused bucket (before . 1) (after . 1))
            '(refused bucket (before . 3/2) (after . 3/2))))
(want "CONTROL: one step short of the limit still answers"
      (let ((nearly (/ 1 (- (expt 2 128) 2))))
        (let ((o (ord-between 0 nearly)))
          (list (and (not (pair? o)) #t) (< (denominator o) (expt 2 128)))))
      (list #t #t))
;; TIES ARE BROKEN BY THE ID, and both halves matter: concurrent inserts
;; into one gap get the same ord, which is a bucket rather than an error.
(want "blocks with equal ord are ordered by their ids"
      (let ((r (reduce-empty)))
        (feed! r
               '("b" 1 () (put ((kind . section) (ord . 1))))
               '("a" 1 () (put ((kind . section) (ord . 1))))
               '("a" 2 () (put ((kind . section) (ord . 1)))))
        (map caddr (state-outline r)))
      '("a.1" "a.2" "b.1"))

(printf "== the reduction is a pure function of the record set ==\n")
;; SAME RECORDS, ANY ARRIVAL ORDER, SAME STATE. A record whose premises
;; have not arrived waits rather than being applied against a state that
;; does not contain them.
(define the-records
  '(("a" 1 () (put ((kind . section) (title . "x"))))
    ("a" 2 () (set "a.1" title "p"))
    ("b" 1 (("a" . 1)) (set "a.1" summary "r"))
    ("b" 2 (("a" . 2)) (set "a.1" title "q"))))
(define (state-of order)
  (let ((r (reduce-empty)))
    (for-each (lambda (i) (let ((e (list-ref the-records i)))
                            (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))))
              order)
    (state-hash r)))
(want "the same records in four arrival orders give one state"
      (let ((h (state-of '(0 1 2 3))))
        (list (equal? h (state-of '(3 2 1 0)))
              (equal? h (state-of '(2 0 3 1)))
              (equal? h (state-of '(1 3 0 2)))))
      (list #t #t #t))
(want "a record whose premise has not arrived is pending, not applied"
      (let ((r (reduce-empty)))
        (reduce-apply! r "b" 1 '(("a" . 1)) '(set "a.1" summary "r"))
        (let ((mid (list (length (reduce-pending r)) (state-read r "a.1"))))
          (reduce-apply! r "a" 1 '() '(put ((kind . section))))
          (list mid (length (reduce-pending r))
                (and (state-read r "a.1") #t))))
      (list (list 1 #f) 0 #t))

;; THE TOKEN TALKS ABOUT WHAT HAS BEEN APPLIED. A record that is waiting
;; on a premise has not changed what anyone read, so it must not change
;; the token -- otherwise `--if-unchanged` would refuse a write because
;; of an event nobody has seen the effect of.
(want "pending records do not enter the state hash"
      (let ((a (reduce-empty)) (b (reduce-empty)))
        (for-each (lambda (r)
                    (reduce-apply! r "a" 1 '() '(put ((kind . section))))
                    (reduce-apply! r "a" 2 '() '(set "a.1" title "p")))
                  (list a b))
        ;; b additionally holds a record whose premise never arrives
        (reduce-apply! b "z" 1 '(("q" . 9)) '(set "a.1" title "never"))
        (list (length (reduce-pending a)) (length (reduce-pending b))
              (equal? (state-hash a) (state-hash b))))
      (list 0 1 #t))

(printf "== R3: every legal order of the whole vocabulary gives one state ==\n")
;; NINE RECORDS COVERING put/set/move/del/link/unlink/tag WITH CROSS-
;; WRITER DEPS, fed in every order the dependency graph permits. Four
;; hand-picked orders, which is what stood here before, sample the
;; orderings; they cannot distinguish "order does not matter" from
;; "these four happen to agree". The graph's edges are the declared deps
;; PLUS each writer's own predecessor -- a writer's earlier events are
;; in its past whether or not it names them, so a.1 before a.2 is not a
;; choice the scheduler has.
(define r3-records
  '(("z" 1 () (put ((kind . section) (title . "z"))))
    ("z" 2 () (put ((kind . section) (title . "w"))))
    ("a" 1 (("z" . 1)) (set "z.1" title "a"))
    ("a" 2 (("z" . 2)) (move "z.1" "z.2" 1))
    ("b" 1 (("a" . 1)) (link "z.1" explains "z.2"))
    ("b" 2 () (tag "t" (("z" . 1))))
    ("c" 1 (("z" . 2)) (link "z.1" explains "z.2"))
    ("c" 2 (("b" . 1)) (unlink "z.1" explains "z.2"))
    ("d" 1 (("a" . 2)) (del "z.2"))))
;; index -> the indices that must precede it
(define r3-preds
  '#(() (0) (0) (1 2) (2) (4) (1) (4 6) (3)))
(define (all-topo-orders n preds)
  (let go ((done '()))
    (if (= (length done) n)
        (list (reverse done))
        (let inner ((v 0) (out '()))
          (if (= v n)
              out
              (inner (+ v 1)
                     (if (and (not (memv v done))
                              (for-all (lambda (p) (memv p done)) (vector-ref preds v)))
                         (append out (go (cons v done)))
                         out)))))))
(define r3-orders (all-topo-orders 9 r3-preds))
(define (r3-feed order)
  (let ((r (reduce-empty)))
    (for-each (lambda (i)
                (let ((e (list-ref r3-records i)))
                  (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))))
              order)
    r))
(define (r3-event-ids order)
  (map (lambda (i) (let ((e (list-ref r3-records i))) (cons (car e) (cadr e)))) order))
;; THE COUNT IS PART OF THE ASSERTION. It pins the graph: drop the
;; same-writer edges and the enumeration admits orders that are not
;; legal, and the count moves before any state comparison is reached.
(want "the graph admits exactly this many orders" (length r3-orders) 206)
(define r3-reference (r3-feed (car r3-orders)))
(want "every legal order gives the same state, the same hash and no leftovers"
      (let loop ((os (cdr r3-orders)) (bad-state 0) (bad-hash 0) (bad-pending 0))
        (if (null? os)
            (list bad-state bad-hash bad-pending)
            (let ((r (r3-feed (car os))))
              (loop (cdr os)
                    (+ bad-state (if (equal? (state-datum r) (state-datum r3-reference)) 0 1))
                    (+ bad-hash (if (equal? (state-hash r) (state-hash r3-reference)) 0 1))
                    (+ bad-pending (if (null? (reduce-pending r)) 0 1))))))
      '(0 0 0))
;; AND THE REDUCER DOES NOT RE-SORT WHAT IT IS GIVEN. Fed a legal order,
;; every record is applicable the moment it arrives, so the applied
;; trace must be that order exactly. An implementation that buffered and
;; applied by its own rule would still reach the same state -- the row
;; above would stay green -- while reporting a history nobody fed it.
(want "the applied trace is the order it was fed, in every legal order"
      (let loop ((os r3-orders) (bad 0))
        (if (null? os)
            bad
            (loop (cdr os)
                  (+ bad (if (equal? (reduce-trace (r3-feed (car os)))
                                     (r3-event-ids (car os)))
                             0 1)))))
      0)

;; THREE INCREMENTAL BATCHINGS, one of them arriving against the
;; dependency order. The state after the last batch is the same state;
;; the dump between batches is where a record that cannot yet be applied
;; has to be visible as pending rather than silently dropped.
(define (r3-batched batches)
  ;; NOT map: the batches share one reduction, and Chez does not specify
  ;; the order in which map applies its procedure. Read in a scrambled
  ;; order the readings describe batches that never happened.
  (let ((r (reduce-empty)))
    (let loop ((bs batches) (acc '()))
      (if (null? bs)
          (reverse acc)
          (begin
            (for-each (lambda (i)
                        (let ((e (list-ref r3-records i)))
                          (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))))
                      (car bs))
            (loop (cdr bs)
                  (cons (list (length (reduce-pending r)) (length (reduce-trace r)))
                        acc)))))))
(want "batched arrival reaches the same state, three ways"
      (let ((ends
              (map (lambda (bs)
                     (let ((r (reduce-empty)))
                       (for-each
                         (lambda (b)
                           (for-each (lambda (i)
                                       (let ((e (list-ref r3-records i)))
                                         (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))))
                                     b))
                         bs)
                       (state-datum r)))
                   '(((0 1 2) (3 4 5) (6 7 8))
                     ((0) (1 2 6) (3 4) (5 7 8))
                     ((8 7 5 4) (3 6) (2) (1) (0))))))
        (list (equal? (car ends) (state-datum r3-reference))
              (equal? (cadr ends) (state-datum r3-reference))
              (equal? (caddr ends) (state-datum r3-reference))))
      '(#t #t #t))
;; THE REVERSE-DEPENDENCY BATCH, read between the batches. Four records
;; arrive before anything they depend on: all four wait, nothing is
;; applied, and they come out in dependency order once the premises land.
(want "records arriving before their premises wait, then apply in order"
      (r3-batched '((8 7 5 4) (3 6) (2) (1) (0)))
      '((4 0) (6 0) (7 0) (8 0) (0 9)))
(want "and the reverse batch applies them in a dependency-respecting order"
      (let ((r (reduce-empty)))
        (for-each (lambda (i)
                    (let ((e (list-ref r3-records i)))
                      (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))))
                  '(8 7 5 4 3 6 2 1 0))
        (let ((tr (reduce-trace r)))
          (list (length tr)
                (member? tr '("z" . 1) '("a" . 1))
                (member? tr '("a" . 1) '("b" . 1))
                (member? tr '("a" . 2) '("d" . 1))
                (member? tr '("b" . 1) '("c" . 2)))))
      '(9 #t #t #t #t))

(printf "== R3: the scheduler reconsiders what it has just unblocked ==\n")
;; Applying one record can make another applicable, and that one may sort
;; ahead of records already waiting. A pass that took the whole ready set
;; and applied it before looking again would run them in an order no
;; scheduler that reconsiders would produce.
(want "a record unblocked mid-pass is taken before one that was already waiting"
      (let ((r (reduce-empty)))
        ;; b.1 and d.1 wait on z.1; a.1 waits on b.1. Nothing can run.
        (reduce-apply! r "b" 1 '(("z" . 1)) '(set "z.1" title "b"))
        (reduce-apply! r "d" 1 '(("z" . 1)) '(set "z.1" summary "d"))
        (reduce-apply! r "a" 1 '(("b" . 1)) '(set "z.1" note "a"))
        (let ((before (length (reduce-pending r))))
          (reduce-apply! r "z" 1 '() '(put ((kind . section))))
          (list before (reduce-trace r))))
      (list 3 '(("z" . 1) ("b" . 1) ("a" . 1) ("d" . 1))))
(want "a writer's own predecessor is a premise too, whichever arrives first"
      (let ((r (reduce-empty)))
        (reduce-apply! r "a" 2 '() '(set "a.1" title "second"))
        (let ((mid (length (reduce-pending r))))
          (reduce-apply! r "a" 1 '() '(put ((kind . section))))
          (list mid (reduce-trace r))))
      (list 1 '(("a" . 1) ("a" . 2))))
(want "and a record waiting on one that is itself waiting stays waiting"
      (let ((r (reduce-empty)))
        (reduce-apply! r "b" 1 '(("a" . 1)) '(set "a.1" title "b"))
        (reduce-apply! r "c" 1 '(("b" . 1)) '(set "a.1" summary "c"))
        (list (length (reduce-pending r)) (reduce-trace r)))
      (list 2 '()))

(printf "== R2: ids are derived, and they sort as strings ==\n")
;; base36 keeps an id short without introducing a separator that could
;; occur in a writer id -- and the tie-break is byte order, so 36 sorts
;; before 35 once it is written down.
(want "the id comes from the event that created the block"
      (list (block-id "a" 1) (block-id "a" 35) (block-id "a" 36)
            (string<? (block-id "a" 36) (block-id "a" 35)))
      (list "a.1" "a.z" "a.10" #t))
;; A writer cannot start at sequence 35 -- its own predecessor is a
;; premise and never arrives -- so the ordering of those two ids is
;; asserted above on the derivation itself, and the bucket ordering has
;; its own row with sequences a fixture can actually produce.

(printf "== R12: what a read returns cannot be changed under it ==\n")
;; AND THE CALLER CANNOT REACH BACK IN. Comparing two reads only shows
;; they agree; what matters is that changing what one of them handed you
;; does not change the state -- a stored vector returned directly is a
;; way to move the token with no event at all.
(want "two independent reads are equal, and mutating one changes nothing"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               (list "a" 2 '() (list 'set "a.1" 'body (vector 1 2 3))))
        (let* ((before (block-hash r "a.1"))
               (one (state-read r "a.1"))
               (two (state-read r "a.1"))
               ;; compared BEFORE the mutation: afterwards they differ
               ;; from each other, which is itself the point -- each read
               ;; got its own copy.
               (agree (equal? one two))
               (v (cdr (assq 'body (cdr (assq 'fields one))))))
          (vector-set! v 0 99)
          (list agree
                (equal? before (block-hash r "a.1"))
                (vector-ref (cdr (assq 'body (cdr (assq 'fields (state-read r "a.1"))))) 0))))
      (list #t #t 1))

(printf "== R10: the hash is computed from the shape the design pins ==\n")
;; A HASH WHOSE INPUT SHAPE LIVED ONLY IN THIS FILE would agree with
;; nothing. The datum is checked against section 9.2's spelling, and the
;; digest against an independent computation over it.
(define sample
  (let ((r (reduce-empty)))
    (feed! r
           '("a" 1 () (put ((kind . section) (title . "x"))))
           '("b" 1 () (set "a.1" title "y"))
           '("a" 2 () (link "a.1" explains "b.1")))
    r))
;; Written out from section 9.2's spelling rather than from the output:
;; a candidate is (<value> . <event-id>), a position candidate is
;; ((<parent> . <ord>) . <event-id>), fields sort by name, candidates by
;; event id, and edges carry no event id at all.
(want "the datum has the pinned shape, sorted as the design says"
      (state-datum sample)
      (list (list 'block "a.1"
                  (list 'fields
                        (list (list 'kind (list (cons 'section (cons "a" 1))))
                              (list 'title (list (cons "x" (cons "a" 1))
                                                 (cons "y" (cons "b" 1))))))
                  (list 'position (list (cons (cons 'root 0) (cons "a" 1))))
                  (list 'deleted #f)
                  (list 'edges (list (cons 'explains "b.1"))))))
;; R10(c): THE TOKEN IS ABOUT ONE BLOCK. A change to a different block
;; must leave it valid -- that is what makes it usable as an
;; --if-unchanged guard on a write to this block. A digest over the whole
;; state cannot do it, since every block is in it.
(want "a change to an unrelated block leaves this block's token alone"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (for-each (lambda (r)
                    (feed! r '("a" 1 () (put ((kind . section) (title . "x"))))))
                  (list one two))
        (feed! two '("b" 1 () (put ((kind . section) (title . "elsewhere")))))
        (list (equal? (block-hash one "a.1") (block-hash two "a.1"))
              ;; CONTROL: the whole-state comparison DOES see it, which is
              ;; why the two questions have two answers.
              (equal? (state-hash one) (state-hash two))))
      (list #t #f))
(want "and a change to this block does move it"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (for-each (lambda (r)
                    (feed! r '("a" 1 () (put ((kind . section) (title . "x"))))))
                  (list one two))
        (feed! two '("b" 1 (("a" . 1)) (set "a.1" title "y")))
        (equal? (block-hash one "a.1") (block-hash two "a.1")))
      #f)
(want "same values, different event ids, different hash"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (feed! one '("a" 1 () (put ((kind . section) (title . "x")))))
        (feed! two '("b" 1 () (put ((kind . section) (title . "x")))))
        (equal? (state-hash one) (state-hash two)))
      #f)
(want "the edge set is part of it: unlinking one edge changes the hash"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (for-each (lambda (r)
                    (feed! r
                           '("a" 1 () (put ((kind . section))))
                           '("a" 2 () (link "a.1" explains "b.1"))))
                  (list one two))
        (feed! two '("a" 3 () (unlink "a.1" explains "b.1")))
        (equal? (state-hash one) (state-hash two)))
      #f)
(want "but the number of link events behind one logical edge is not"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (feed! one
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (link "a.1" explains "b.1")))
        (feed! two
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (link "a.1" explains "b.1"))
               '("c" 1 () (link "a.1" explains "b.1")))
        (equal? (state-hash one) (state-hash two)))
      #t)

;; THE POSITION CANDIDATE SET IS IN THE TOKEN, not just the resolved
;; position. Two states where the block hangs in the same place, one
;; because that is the only candidate and one because a concurrent move
;; is standing beside it, are not the same state: the next move
;; supersedes different things.
(want "a block with two position candidates hashes differently from one with one"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (for-each (lambda (r)
                    (feed! r
                           '("z" 1 () (put ((kind . section))))
                           '("z" 2 () (put ((kind . section))))
                           '("z" 3 () (put ((kind . section))))
                           '("a" 1 (("z" . 3)) (move "z.1" "z.2" 1))))
                  (list one two))
        (feed! two '("b" 1 (("z" . 3)) (move "z.1" "z.3" 1)))
        (list (equal? (state-hash one) (state-hash two))
              (length (cadr (assq 'position (cddr (car (state-datum one))))))
              (length (cadr (assq 'position (cddr (car (state-datum two))))))))
      (list #f 1 2))

;; THE TOMBSTONE IS IN THE TOKEN. A refusal that says "deleted" proves
;; the writer looked at the tombstone, not that the token did -- the
;; token is what a later writer compares against, and two states that
;; differ only in whether a block is deleted must not share one.
(want "deleting a block changes the token"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (for-each (lambda (r)
                    (feed! r
                           '("z" 1 () (put ((kind . section))))
                           '("z" 2 () (put ((kind . section))))))
                  (list one two))
        (feed! two '("a" 1 (("z" . 2)) (del "z.2")))
        (list (equal? (state-hash one) (state-hash two))
              (cadr (assq 'deleted (cddr (cadr (state-datum one)))))
              (cadr (assq 'deleted (cddr (cadr (state-datum two)))))))
      (list #f #f #t))

;; AN EDGE TO A TOMBSTONED BLOCK IS STILL AN EDGE. It is dangling, not
;; gone -- `links --dangling` lists it -- so keeping it and unlinking it
;; are two different states. An implementation that drops edges whose
;; target is deleted before hashing reports one token for both, and a
;; writer that unlinked one would be told nothing had changed.
(want "unlinking a dangling edge changes the token"
      (let ((kept (reduce-empty)) (cut (reduce-empty)))
        (for-each (lambda (r)
                    (feed! r
                           '("z" 1 () (put ((kind . section))))
                           '("z" 2 () (put ((kind . section))))
                           '("a" 1 (("z" . 2)) (link "z.1" explains "z.2"))
                           '("a" 2 () (del "z.2"))))
                  (list kept cut))
        (feed! cut '("a" 3 () (unlink "z.1" explains "z.2")))
        (list (equal? (state-hash kept) (state-hash cut))
              (cadr (assq 'edges (cddr (car (state-datum kept)))))
              (cadr (assq 'edges (cddr (car (state-datum cut)))))))
      (list #f (list (cons 'explains "z.2")) '()))

;; (d) THE TOKEN SURVIVES A SNAPSHOT. Resuming from rows and replaying
;; from the start must give one token, or `--if-unchanged` means
;; something different to a process that restarted.
(want "a token taken after a resume equals the one taken after a full replay"
      (let* ((build (lambda ()
                      (feed! (reduce-empty)
                             '("z" 1 () (put ((kind . section) (title . "x"))))
                             '("z" 2 () (put ((kind . section))))
                             '("a" 1 (("z" . 1)) (set "z.1" title "a"))
                             '("b" 1 (("z" . 1)) (set "z.1" title "b"))
                             '("a" 2 (("z" . 2)) (move "z.1" "z.2" 1))
                             '("a" 3 () (link "z.1" explains "z.2")))))
             (whole (build))
             (resumed (rows->state (state->rows (build)))))
        (list (equal? (state-hash whole) (state-hash resumed))
              (equal? (block-hash whole "z.1") (block-hash resumed "z.1"))))
      (list #t #t))
(want "and the order records arrived in is not"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (feed! one
               '("a" 1 () (put ((kind . section))))
               '("b" 1 () (set "a.1" title "y")))
        (feed! two
               '("b" 1 () (set "a.1" title "y"))
               '("a" 1 () (put ((kind . section)))))
        (equal? (state-hash one) (state-hash two)))
      #t)

;; AND CHECKED AGAINST AN IMPLEMENTATION THAT IS NOT THIS ONE. The
;; script beside this file reads an UNORDERED dump of the reduction and
;; does the sorting, the datum shape, the printer's spacing and the
;; digest itself, in another language. Hashing the text the product
;; already printed would only have established that sha256 is sha256;
;; what is worth cross-checking is whether two readings of section 9.2
;; arrive at the same bytes -- and the first attempt did not, because the
;; writer is a string and the printer collapses nested pairs.
(define (hex-of str)
  (let* ((bv (string->utf8 str))
         (n (bytevector-length bv))
         (digits "0123456789abcdef")
         (out (make-string (* 2 n))))
    (let loop ((i 0))
      (if (= i n)
          out
          (let ((b (bytevector-u8-ref bv i)))
            (string-set! out (* 2 i) (string-ref digits (div b 16)))
            (string-set! out (+ 1 (* 2 i)) (string-ref digits (mod b 16)))
            (loop (+ i 1)))))))

(define (dump-for-script r)
  (let ((out (open-output-string)))
    ;; A TYPED TERM, NOT A RENDERED ONE. The script used to receive each
    ;; value already printed by this library's own writer and paste it
    ;; into its answer -- so a defect in that writer showed up
    ;; identically on both sides and the two agreed. What crosses now is
    ;; the value itself, and the script does its own printing.
    (define (term x)
      (cond
        ((string? x) (string-append "str:" (hex-of x)))
        ((symbol? x) (string-append "sym:" (hex-of (symbol->string x))))
        ((null? x) "nil:")
        ((and (number? x) (exact? x) (integer? x))
         (string-append "int:" (number->string x)))
        ((and (number? x) (exact? x) (rational? x))
         (string-append "rat:" (number->string (numerator x))
                        "/" (number->string (denominator x))))
        ((pair? x) (string-append "pair: " (term (car x)) " " (term (cdr x))))
        (else (assertion-violation 'dump-for-script "no term for this value" x))))
    (for-each
      (lambda (bd)
        (let ((id (cadr bd)))
          ;; HEX, NOT A SEPARATOR THE DATA CAN CONTAIN. A title holding a
          ;; tab or a newline broke the transport, and a broken transport
          ;; that still prints a digest is worse than one that stops.
          ;; THE VARIABLE-LENGTH TERM GOES LAST on every line, so the
          ;; fixed fields can be read off the front without counting.
          (fprintf out "B ~a ~a\n" (term id)
                   (if (cadr (assq 'deleted (cddr bd))) 1 0))
          (for-each (lambda (f)
                      (for-each (lambda (c)
                                  (fprintf out "F ~a ~a ~a ~a ~a\n"
                                           (term id) (term (car (cdr c))) (cdr (cdr c))
                                           (term (car f)) (term (car c))))
                                (reverse (cadr f))))
                    (reverse (cadr (assq 'fields (cddr bd)))))
          (for-each (lambda (c)
                      (fprintf out "P ~a ~a ~a ~a\n"
                               (term id) (term (car (cdr c))) (cdr (cdr c))
                               (term (car c))))
                    (cadr (assq 'position (cddr bd))))
          (for-each (lambda (e)
                      (fprintf out "E ~a ~a ~a\n" (term id) (term (car e)) (term (cdr e))))
                    (cadr (assq 'edges (cddr bd))))))
      ;; reversed, so the script cannot inherit this file's ordering
      (reverse (state-datum r)))
    (get-output-string out)))
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
;; AND A VALUE THAT CONTAINS A SEPARATOR. A title holding a tab used to
;; crash the script and one holding a newline used to produce a
;; different record silently -- a separator the data can contain is not
;; a separator. Every field is hex now, and this is the row that says so.
(define awkward
  (let ((r (reduce-empty)))
    (feed! r
           (list "a" 1 '() (list 'put (list (cons 'kind 'section)
                                            (cons 'title "a\ttab and a\nnewline")))))
    r))
(want "a value containing a tab and a newline survives the transport"
      (let* ((dir (test-dir "reduce1"))
             (in (string-append dir "/awkward.tsv"))
             (out (string-append dir "/awkward-hash.txt")))
        (call-with-port (open-file-output-port in (file-options no-fail))
          (lambda (p) (put-bytevector p (string->utf8 (dump-for-script awkward)))))
        (system (string-append "python3 " script-dir "/reduce-hash-check.py < " in " > " out))
        (let ((got (let ((b (call-with-port (open-file-input-port out) get-bytevector-all)))
                     (if (eof-object? b) "" (utf8->string b)))))
          (equal? (state-hash awkward)
                  (if (> (string-length got) 0)
                      (substring got 0 (- (string-length got) 1))
                      ""))))
      #t)

(want "an independent implementation of section 9.2 computes the same token"
      (let* ((dir (test-dir "reduce1"))
             (in (string-append dir "/dump.tsv"))
             (out (string-append dir "/hash.txt")))
        (call-with-port (open-file-output-port in (file-options no-fail))
          (lambda (p) (put-bytevector p (string->utf8 (dump-for-script sample)))))
        (system (string-append "python3 " script-dir "/reduce-hash-check.py < " in " > " out))
        (let ((got (let ((b (call-with-port (open-file-input-port out) get-bytevector-all)))
                     (if (eof-object? b) "" (utf8->string b)))))
          (list (equal? (state-hash sample)
                        (substring got 0 (- (string-length got) 1)))
                ;; and the digest is not trivially empty
                (= 64 (string-length (state-hash sample))))))
      (list #t #t))

(printf "== R2: inserting before the first block ==\n")
;; The mirror of appending: below 1 the simplest rational is 0, below 0
;; it is -1. Same rule, other end.
(want "a front insert takes ceiling(first) - 1"
      (list (ord-between #f 1) (ord-between #f 0) (ord-between #f 5/2)
            (ord-between #f -3))
      (list 0 -1 2 -4))
(want "a denominator of exactly the limit is accepted, one past it is not"
      (let* ((limit (expt 2 128))
             (ok (ord-between 0 (/ 1 (- limit 1))))
             (no (ord-between 0 (/ 1 limit))))
        (list (and (not (pair? ok)) (= (denominator ok) limit)) no))
      (list #t '(refused too-deep)))

(printf "== R6: the effective parent graph ==\n")
;; ONLY SETTLED POSITIONS PARTICIPATE. A block whose position is in
;; conflict has no one parent, so it is shown under root and marked
;; rather than guessed at -- and it cannot be part of a cycle either.
(want "two writers who move each other under the other make a cycle"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section))))
               '("a" 3 () (move "a.1" "a.2" 1))
               '("b" 1 (("a" . 3)) (move "a.2" "a.1" 1)))
        (cdr (assq 'conflicts (state-structure r))))
      '("a.1" "a.2"))
(want "CONTROL: without the second move there is no cycle"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section))))
               '("a" 3 () (move "a.1" "a.2" 1)))
        (cdr (assq 'conflicts (state-structure r))))
      '())
;; THE CYCLE-FORMING CANDIDATE IS THE MOST RECENT ONE, deliberately: an
;; implementation that reads "the position" as "the first candidate"
;; would then find the cycle and report it. The block has TWO positions
;; and therefore no position, so it takes part in no parent graph at all
;; -- it is unplaced, and unplaced is not the same as cyclic.
(want "a block whose position is in conflict is unplaced, not on a cycle"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section))))
               '("a" 3 () (move "a.1" root 1))
               '("b" 1 (("a" . 3)) (move "a.2" "a.1" 1))
               '("c" 1 (("a" . 2)) (move "a.1" "a.2" 1)))
        (list (cdr (assq 'conflicts (state-structure r)))
              (cdr (assq 'unplaced (state-structure r)))))
      (list '() '("a.1")))

(printf "== R4: deletion does not cascade ==\n")
;; Three generations: deleting the middle one orphans the child, and the
;; grandchild stays under the child -- the block is gone, its subtree is
;; not.
(want "deleting a parent orphans its child and leaves the grandchild in place"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section) (parent . "a.1"))))
               '("a" 3 () (put ((kind . section) (parent . "a.2"))))
               '("a" 4 () (del "a.1")))
        (list (cdr (assq 'orphans (state-structure r)))
              (map (lambda (row) (list (car row) (caddr row))) (state-outline r))))
      (list '("a.2") '(("a.1" "a.2") ("a.2" "a.3"))))
(want "and moving the orphan to a live parent clears the mark"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section) (parent . "a.1"))))
               '("a" 3 () (del "a.1"))
               '("a" 4 () (move "a.2" root 1)))
        (cdr (assq 'orphans (state-structure r))))
      '())
;; A move that arrives causally after the delete is still recorded --
;; evidence of what someone believed, even about a block that is gone.
(want "a move after the delete is recorded as a candidate"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (del "a.1"))
               '("a" 3 () (move "a.1" root 7)))
        (let ((b (state-read r "a.1")))
          (list (cdr (assq 'deleted b)) (cdr (assq 'position b)))))
      (list #t '(root . 7)))

(printf "== R5: one logical edge, however many events ==\n")
(want "two link events for one edge are one edge, and the unlink sees both"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (link "a.1" explains "T"))
               '("a" 3 () (link "a.1" explains "T")))
        (let ((before (cadr (assq 'edges (cddr (car (state-datum r)))))))
          (feed! r '("a" 4 () (unlink "a.1" explains "T")))
          (list before (cadr (assq 'edges (cddr (car (state-datum r))))))))
      (list '((explains . "T")) '()))

;; A DUMP IS A FUNCTION OF THE STATE, not of how the records arrived.
;; Two libraries holding the same records must print the same thing.
(want "the dump does not depend on arrival order"
      (let ((one (reduce-empty)) (two (reduce-empty)))
        (feed! one
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("b" 1 () (put ((kind . section) (title . "y"))))
               '("a" 2 () (tag "t" (("a" . 1))))
               '("b" 2 () (tag "s" (("b" . 1)))))
        (feed! two
               '("b" 2 () (tag "s" (("b" . 1))))
               '("b" 1 () (put ((kind . section) (title . "y"))))
               '("a" 2 () (tag "t" (("a" . 1))))
               '("a" 1 () (put ((kind . section) (title . "x")))))
        (equal? (state-dump one) (state-dump two)))
      #t)

;; A BLOCK WITH NO ONE PLACE IS STILL SHOWN. Leaving it out of the
;; outline loses it: the facts are in the candidates, and a reader who
;; cannot see the block cannot act on them.
(want "a block whose position is in conflict appears under root, marked"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section))))
               '("a" 3 () (move "a.1" root 1))
               '("c" 1 (("a" . 2)) (move "a.1" "a.2" 1)))
        (state-outline r))
      ;; Sorted by parent, then ord, then id -- the mark does not change
      ;; where the row sits, only what it says about it.
      '((root 0 "a.1" unplaced) (root 0 "a.2")))
(want "and blocks on a cycle appear there too, marked differently"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section))))
               '("a" 3 () (move "a.1" "a.2" 1))
               '("b" 1 (("a" . 3)) (move "a.2" "a.1" 1)))
        (state-outline r))
      '((root 0 "a.1" conflict) (root 0 "a.2" conflict)))
(want "CONTROL: an ordinary row carries no mark"
      (let ((r (reduce-empty)))
        (feed! r '("a" 1 () (put ((kind . section)))))
        (state-outline r))
      '((root 0 "a.1")))

(printf "== R9: a cut that is not causally closed gives no state ==\n")
(want "a cut naming a record that was never received is unusable"
      (let ((r (reduce-empty)))
        (feed! r '("a" 1 () (put ((kind . section)))))
        (list (cut-usable? r '(("a" . 1)))
              (cut-usable? r '(("b" . 5)))))
      (list 'usable '(unusable not-received)))
(want "a cut that omits a premise of what it contains is unusable"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section))))
               '("a" 2 () (put ((kind . section))))
               '("b" 1 (("a" . 2)) (set "a.1" title "x")))
        (list (cut-usable? r '(("a" . 2) ("b" . 1)))
              (cut-usable? r '(("a" . 1) ("b" . 1)))))
      (list 'usable '(unusable not-closed)))
(want "and a cut naming one writer twice is malformed"
      (let ((r (reduce-empty)))
        (feed! r '("a" 1 () (put ((kind . section)))))
        (cut-usable? r '(("a" . 1) ("a" . 1))))
      '(unusable duplicate-writer))
(want "the canonical identity does not depend on the order it was written"
      (list (equal? (cut-id '(("b" . 2) ("a" . 1))) (cut-id '(("a" . 1) ("b" . 2))))
            (equal? (cut-id '(("a" . 1))) (cut-id '(("a" . 2))))
            (string-length (cut-id '())))
      (list #t #f 64))

(printf "== rows round-trip ==\n")
;; A SNAPSHOT STORES CANDIDATES, NOT RESOLUTIONS. Storing what a field
;; resolved to would not be equivalent to replaying: the next write's
;; supersession depends on WHICH events the candidates came from.
;; ROUND-TRIPPING THE STATE IS THE EASY HALF. The half that matters is
;; whether the resumed reduction can carry on: a snapshot exists to be
;; replayed FROM, and one that forgets what has been applied leaves the
;; next record waiting on premises it already has.
(want "state to rows and back is the same state"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (set "a.1" title "p"))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("a" 3 () (link "a.1" explains "b.1")))
        (list (equal? (state-hash r) (state-hash (rows->state (state->rows r))))
              (equal? (state-dump r) (state-dump (rows->state (state->rows r))))))
      (list #t #t))
(want "and the resumed state carries on where the original left off"
      (let* ((r (reduce-empty))
             (_ (feed! r
                       '("a" 1 () (put ((kind . section) (title . "x"))))
                       '("a" 2 () (set "a.1" title "p"))))
             (resumed (rows->state (state->rows r))))
        (reduce-apply! resumed "a" 3 '() '(set "a.1" title "q"))
        (list (length (reduce-pending resumed))
              (field-of resumed "a.1" 'title)))
      (list 0 "q"))
;; THREE WAYS TO THE SAME STATE: replay it all, or resume from rows and
;; feed the rest. A snapshot that is not equivalent to replaying is a
;; snapshot that quietly answers differently.
(want "replaying everything and resuming from rows agree, candidates and all"
      (let* ((whole (reduce-empty))
             (part (reduce-empty)))
        (feed! whole
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (set "a.1" summary "p"))
               '("b" 1 (("a" . 1)) (set "a.1" title "r"))
               '("a" 3 (("b" . 1)) (set "a.1" title "s")))
        (feed! part
               '("a" 1 () (put ((kind . section) (title . "x"))))
               '("a" 2 () (set "a.1" summary "p")))
        (let ((resumed (rows->state (state->rows part))))
          (reduce-apply! resumed "b" 1 '(("a" . 1)) '(set "a.1" title "r"))
          (reduce-apply! resumed "a" 3 '(("b" . 1)) '(set "a.1" title "s"))
          (list (equal? (state-datum whole) (state-datum resumed))
                (field-of whole "a.1" 'title)
                (field-of resumed "a.1" 'title))))
      (list #t "s" "s"))

;; A PREMISE CAN OUTLIVE EVERY CANDIDATE IT WROTE. a.1 sees z.1 and
;; writes one field; a.2 then supersedes that one candidate, so a.1
;; stands in the snapshot as nothing at all -- and b.1 still names it as
;; its only dep. Whether b.1 supersedes z.1's title is decided by what
;; a.1 had seen, which is reachable only through a.1's past. While the
;; clocks travelled attached to candidates, a.1 had nowhere to put one:
;; the resumed reduction kept BOTH title candidates where the full
;; replay kept one, and every row above stayed green because in all of
;; them the dep still had a candidate standing.
(define vanished-dep
  '(("z" 1 () (put ((kind . section) (title . "z-wrote"))))
    ("a" 1 (("z" . 1)) (set "z.1" summary "a1"))
    ("a" 2 () (set "z.1" summary "a2"))))
(define (title-candidates r)
  (let* ((b (let loop ((bs (state-datum r)))
              (cond ((null? bs) #f)
                    ((equal? (cadr (car bs)) "z.1") (car bs))
                    (else (loop (cdr bs))))))
         (fs (and b (cadr (assq 'fields (cddr b)))))
         (e (and fs (assq 'title fs))))
    (if e (length (cadr e)) 'no-such-field)))
;; CONTROL: the premise really did leave nothing behind. Without this
;; the row above could be green for a reduction in which a.1's own
;; candidate is doing the superseding.
(want "CONTROL: the dep's own candidate is gone from the snapshot"
      (let* ((r (apply feed! (reduce-empty) vanished-dep))
             (rows (state->rows r)))
        (let loop ((rows rows))
          (cond ((null? rows) 'not-found)
                ((and (eq? (car (car rows)) 'block) (equal? (cadr (car rows)) "z.1"))
                 (map (lambda (c) (cdr c))
                      (cdr (assq 'summary (cdr (assq 'fields (caddr (car rows))))))))
                (else (loop (cdr rows))))))
      '(("a" . 2)))
(want "a resumed reduction inherits the past of a dep that left no candidate"
      (let ((whole (apply feed! (reduce-empty) vanished-dep))
            (resumed (rows->state (state->rows (apply feed! (reduce-empty) vanished-dep)))))
        (for-each (lambda (r) (reduce-apply! r "b" 1 '(("a" . 1)) '(set "z.1" title "b1")))
                  (list whole resumed))
        (list (title-candidates whole)
              (title-candidates resumed)
              (equal? (state-datum whole) (state-datum resumed))))
      '(1 1 #t))

;; THE SNAPSHOT DOES NOT GROW WITH HISTORY. A clock per applied event is
;; correct and unbounded; the stored form keeps only the events where a
;; writer's clock jumps -- which is where it declared a dep that told it
;; something it did not already know. Everything else is the previous
;; clock with the writer's own slot raised, and is recomputed.
;; WITHOUT THIS ROW "compressed" is a claim in a comment: a state->rows
;; that stored every clock passes every correctness row above.
(define (stored-seqs r)
  (let loop ((rows (state->rows r)))
    (cond ((null? rows) 'no-pasts-row)
          ((eq? (car (car rows)) 'pasts)
           (list-sort (lambda (x y) (string<? (car x) (car y)))
                      (map (lambda (we) (list (car we) (map car (cadr we))))
                           (cadr (car rows)))))
          (else (loop (cdr rows))))))
(want "a clock is stored only where the writer's clock jumps"
      (stored-seqs
        (feed! (reduce-empty)
               '("a" 1 () (put ((kind . section))))
               '("b" 1 () (put ((kind . section))))
               '("a" 2 () (set "a.1" title "p"))
               '("a" 3 (("b" . 1)) (set "a.1" title "q"))
               '("a" 4 () (set "a.1" title "r"))))
      '(("a" (3)) ("b" ())))
;; A LONGER RUN OF THE SAME WRITER: twenty events, one dep, one clock.
(want "twenty events with one dep between them store one clock"
      (stored-seqs
        (let ((r (reduce-empty)))
          (feed! r '("b" 1 () (put ((kind . section)))))
          (feed! r '("a" 1 () (put ((kind . section)))))
          (let loop ((n 2))
            (when (<= n 20)
              (reduce-apply! r "a" n (if (= n 11) '(("b" . 1)) '())
                             (list 'set "a.1" 'title (number->string n)))
              (loop (+ n 1))))
          r))
      '(("a" (11)) ("b" ())))
;; AND THE RECONSTRUCTION REACHES BACK PAST THE GAP. a's clock jumps at
;; a.3, which is where it saw b.1; a.4 through a.9 declare nothing and
;; are not stored. c.1 then names a.9 as its only premise, and whether
;; it supersedes b.1's title turns on a.9's clock being rebuilt from
;; a.3's and not from a.1's -- or from nothing. Nine events separate the
;; stored clock from the one that is asked for.
(define (long-gap)
  (let ((r (reduce-empty)))
    (feed! r
           '("z" 1 () (put ((kind . section) (title . "x"))))
           '("b" 1 (("z" . 1)) (set "z.1" title "b"))
           '("a" 1 (("z" . 1)) (set "z.1" note "a1"))
           '("a" 2 () (set "z.1" note "a2"))
           '("a" 3 (("b" . 1)) (set "z.1" note "a3")))
    (let loop ((n 4))
      (when (<= n 9)
        (reduce-apply! r "a" n '() (list 'set "z.1" 'note (number->string n)))
        (loop (+ n 1))))
    r))
(want "CONTROL: the clock asked for is nine events past the stored one"
      (stored-seqs (long-gap))
      '(("a" (1 3)) ("b" (1)) ("z" ())))
(want "a clock rebuilt across a long gap still carries what the jump saw"
      (let ((whole (long-gap))
            (resumed (rows->state (state->rows (long-gap)))))
        (for-each (lambda (r) (reduce-apply! r "c" 1 '(("a" . 9)) '(set "z.1" title "c")))
                  (list whole resumed))
        (list (field-of whole "z.1" 'title) (field-of resumed "z.1" 'title)))
      (list "c" "c"))

;; AND A SNAPSHOT OF A RESUMED STATE IS NO BIGGER THAN THE FIRST ONE.
;; The rows above measure the first snapshot only. A rebuild that gets
;; every clock behaviourally right but stores a slightly different value
;; in the writer's own slot is invisible to every candidate assertion in
;; this file -- and the next compression then finds no clock matching
;; its prediction and stores all of them, so the snapshot goes linear in
;; history one generation later than anyone is looking.
;; Three generations, because the first resume is where the table stops
;; being the one replay built.
(want "compression survives being snapshotted, resumed and snapshotted again"
      (let* ((g1 (long-gap))
             (g2 (rows->state (state->rows g1)))
             (g3 (rows->state (state->rows g2))))
        (list (stored-seqs g1) (stored-seqs g2) (stored-seqs g3)))
      (list '(("a" (1 3)) ("b" (1)) ("z" ()))
            '(("a" (1 3)) ("b" (1)) ("z" ()))
            '(("a" (1 3)) ("b" (1)) ("z" ()))))
(want "and the twenty-event run stays at one stored clock across generations"
      (let* ((build (lambda ()
                      (let ((r (reduce-empty)))
                        (feed! r '("b" 1 () (put ((kind . section)))))
                        (feed! r '("a" 1 () (put ((kind . section)))))
                        (let loop ((n 2))
                          (when (<= n 20)
                            (reduce-apply! r "a" n (if (= n 11) '(("b" . 1)) '())
                                           (list 'set "a.1" 'title (number->string n)))
                            (loop (+ n 1))))
                        r)))
             (g1 (build))
             (g2 (rows->state (state->rows g1))))
        (list (stored-seqs g1) (stored-seqs g2)))
      (list '(("a" (11)) ("b" ()))
            '(("a" (11)) ("b" ()))))

(printf "== an event is applied once ==\n")
;; Delivering a record twice used to regress the applied cursor and
;; bring a superseded candidate back as a conflict.
(want "a second delivery of an applied record is refused and changes nothing"
      (let ((r (reduce-empty)))
        (feed! r
               '("a" 1 () (put ((kind . section) (title . "old"))))
               '("a" 2 () (set "a.1" title "new")))
        (let ((before (block-hash r "a.1"))
              (answer (reduce-apply! r "a" 1 '() '(put ((kind . section) (title . "old"))))))
          (list answer
                (equal? before (block-hash r "a.1"))
                (field-of r "a.1" 'title)
                (reduce-applied-cut r))))
      (list '(refused already-applied) #t "new" '(("a" . 2))))
(want "CONTROL: the next unseen record is accepted"
      (let ((r (reduce-empty)))
        (feed! r '("a" 1 () (put ((kind . section)))))
        (reduce-apply! r "a" 2 '() '(set "a.1" title "next")))
      'accepted)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "reduce1 complete\n")
