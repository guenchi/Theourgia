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
;; A payload still holding `("#%new" k)` names a block an earlier
;; sub-operation created, so it cannot be compared until that one is in.
(want "a declared payload still naming a block to be created is undetermined"
      (let* ((entries (list (cons 0 (list 'insert (list "#%new" 0) "t"))))
             (es (list (plan-ev entries) (ev 14 0 '(insert "w3kxxxxx.14" "t")))))
        (membership (cadr es) es))
      'undetermined)
(want "either way it reaches the answer as unknown, marked or not"
      (list (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "CHANGED"))) 2)
            (decide (list (plan-ev two)
                          (ev 14 0 '(set "b" "t" "x") 'valid-history #t '(plan-mismatch)))
                    2))
      (list '(unknown (plan-mismatch ("w3kxxxxx" . 14)))
            '(unknown (plan-mismatch ("w3kxxxxx" . 14)))))

(printf "\n== U3b: a record that is there and was never applied ==\n")
;; IT IS NEITHER ANSWER. Not "no evidence" -- the bytes are in verifiable
;; history, so the request reached the store. And not a replay either: a
;; replay tells a client the work is DONE, and nothing here says this
;; record was ever applied to the state the client will read. Delivery
;; can stop short of a readable record for reasons that have nothing to
;; do with this request -- a premise that has not arrived, a writer the
;; load stopped at.
(want "a readable record that was never delivered"
      (decide (list (ev 14 'single '(set "b" "t" "x") 'valid-history #f)) #f)
      '(unknown (undelivered ("w3kxxxxx" . 14))))
;; AND THE ANSWER NAMES THE RECORD IT IS ABOUT. A caller that has to
;; make something durable before repeating the word needs to know which
;; record the word is about -- it may sit in an earlier segment, or on a
;; generation since retired -- and deriving it a second time from the
;; same evidence would be a second supplier of the one fact this rule
;; established.
(want "TWIN: the same record, delivered, and the answer names it"
      (decide (list (ev 14 'single '(set "b" "t" "x"))) #f)
      '(replay ("w3kxxxxx" . 14)))
;; AND "NO PLAN WAS DECLARED" DOES NOT MAKE EVIDENCE A COMPLETED SINGLE.
;; A request with no plan is one sub-operation, so a replay needs exactly
;; that: an applied record whose actor calls itself `single`. Evidence
;; that is only a plan record passes every rule above and establishes
;; nothing about a single execution.
(want "evidence that is only a plan, asked as a single request"
      (decide (list (plan-ev two)) #f)
      '(unknown (no-applied-record)))
(want "TWIN: the same evidence with the single record beside it"
      (decide (list (plan-ev two) (ev 14 'single '(set "b" "t" "x"))) #f)
      '(replay ("w3kxxxxx" . 14)))

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
      '(complete (0)))
(want "a hole is refused, and the present set is named"
      (decide (list (plan-ev three) (ev 14 0 '(set "b" "t" "x")) (ev 16 2 '(set "d" "t" "z"))) 3)
      '(unknown (plan-order (0 2))))
(want "the whole plan present is a replay"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x")) (ev 15 1 '(set "c" "t" "y"))) 2)
      '(replay))
;; A REQUEST OF EXACTLY ONE SUB-OPERATION HAS NO PLAN, and an empty plan
;; is itself the whole evidence: both are complete the moment they are
;; present.
;; An empty plan is complete the moment it is present, and there is no
;; single record to name -- so that one answers bare.
(want "a single record, and an empty plan, are each complete alone"
      (list (decide (list (ev 14 'single '(set "b" "t" "x"))) #f)
            (decide (list (plan-ev '())) 0))
      (list '(replay ("w3kxxxxx" . 14)) '(replay)))
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
      '(complete (0)))
(want "TWIN: the same record without the mark completes the plan"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (ev 15 1 '(set "c" "t" "y")))
              2)
      '(replay))
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
(printf "q2 complete\n")
