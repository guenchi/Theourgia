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

;; A request that may have been sent once already, at the store's own
;; write path.
;;
;; Every row here is about the same thing said twice. A client sends a
;; request, hears nothing, and sends it again -- and the store has to
;; tell the two apart from a client that meant to do the thing twice.
;; The only difference between those two clients is the request id they
;; chose, so the id is what decides, and the content is what is checked
;; AGAINST the id rather than what identifies it.
;;
;; THE IDENTITY HAS TO BE IN THE RECORD. A record carrying only an actor
;; name is a record the scan cannot attribute, so a store that wrote one
;; would answer "no evidence" to the retry and do the work again. Half of
;; these rows are really about that: the write path writes the six-element
;; actor, and the read path finds it.

(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace)
        (theourgia reduce) (theourgia store) (theourgia wire) (theourgia request))

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

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) x))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


(define scratch (test-dir "q7"))
(define case-n 0)
;; EACH STORE REMEMBERS ITS OWN MACHINE HOME, and every case switches
;; back to it before touching that store. The machine identity is a
;; nonce kept IN the machine home, so pointing THEOURGIA_HOME somewhere
;; else makes this a different machine -- and every store created under
;; the old home then fails its identity check for a reason that has
;; nothing to do with what the row is testing.
(define homes '())
(define (home-of d) (cdr (assoc d homes)))
(define (use-store! d) (putenv "THEOURGIA_HOME" (home-of d)))

(define (fresh-store! records)
  (set! case-n (+ case-n 1))
  (let ((d (string-append scratch "/s" (number->string case-n))))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    ;; THE MACHINE REGISTRY LIVES OUTSIDE THE STORE. That is not a
    ;; convenience: the registry is the one witness to a rollback that
    ;; does not travel with a backup of the store. Putting the home
    ;; under the store made restore-in-place! roll the registry back
    ;; too, so the generation record vanished along with the generation
    ;; and every rollback row passed the store as healthy -- the fixture
    ;; was testing a layout the design forbids.
    (system (string-append "mkdir -p " scratch "/home" (number->string case-n)))
    (putenv "THEOURGIA_HOME" (string-append scratch "/home" (number->string case-n)))
    (set! homes (cons (cons d (string-append scratch "/home" (number->string case-n))) homes))
    (store-init! d)
    (for-each (lambda (title)
                (with-store-write d
                  (lambda (st v) (list (list 'insert 'root #f
                                             (list (cons 'kind 'section)
                                                   (cons 'title title)))))
                  "t"))
              records)
    d))

(define (writer-of d) (car (list-sort string<? (store-writers d))))
(define (local-of d)
  ;; the writer this machine may still extend: local origin, not retired
  (let loop ((ws (list-sort string<? (store-writers d))))
    (cond ((null? ws) #f)
          ((and (file-exists? (string-append d "/writers/" (car ws) "/owner.sexp"))
                (not (file-exists? (string-append d "/writers/" (car ws) "/retired.sexp"))))
           (car ws))
          (else (loop (cdr ws))))))
(define (slurp p)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
      (if (eof-object? b) "" (utf8->string b)))))
(define (append-line! p text)
  (call-with-port (open-file-output-port p (file-options no-fail no-truncate)
                                         'block (native-transcoder))
    (lambda (o)
      (set-port-position! o (bytevector-length (string->utf8 (slurp p))))
      (put-string o text))))
(define (records-of d) (length (reduce-trace (open-and-reduce d))))
;; SORTED, BECAUSE state-datum ORDERS BY BLOCK ID and a block id starts
;; with its writer's generated name. After an adopt the new writer's
;; name may sort either side of the old one's, so an expectation written
;; in document order passes or fails by the coin flip of two random
;; strings. What these rows claim is which blocks survived, not where
;; they land in a listing.
(define (titles-of d)
  (list-sort string<?
             (map (lambda (b)
                    (let ((e (assq 'title (cadr (assq 'fields (cddr b))))))
                      (if e (car (car (cadr e))) "none")))
                  (state-datum d))))
;; A WRITER WITH NO SEGMENT IS A READING, NOT AN EXCEPTION. A new
;; generation that never got its first segment file has nothing to read,
;; and dying here ends the file with no failure count -- which reads
;; exactly like a run nobody made.
(define (deps-of d writer n)
  (let* ((raw (slurp (string-append d "/writers/" writer "/000001.sexp")))
         (text (if (string? raw) raw "")))
    (if (not (string? raw))
        'no-segment
    (let loop ((i 0) (start 0) (k 0))
      (cond ((>= i (string-length text)) 'no-record)
            ((char=? (string-ref text i) #\newline)
             (if (= k n)
                 (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
                   (if (and (pair? r) (eq? (car r) 'ok)) (list-ref r 4) r))
                 (loop (+ i 1) (+ i 1) (+ k 1))))
            (else (loop (+ i 1) start k)))))))


(printf "== the same request twice ==\n")
(define d1 (fresh-store! '()))
(define W1 (local-of d1))
(define AFTER1 (cons W1 0))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))

(define (insert-intent title)
  (list (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title title)))))
(define (send! store req title)
  (car (with-store-write store (lambda (st v) (insert-intent title)) "agent:claude" req)))
(define (req-of id after title)
  (make-write-request "agent:claude" 'insert (list "root" title) id after))

(want "CONTROL: a request the store has never seen is executed"
      (car (send! d1 (req-of "r-1" AFTER1 "One") "One"))
      'ok)
(want "and the block is there"
      (titles-of (open-and-reduce d1))
      '("One"))
;; THE SECOND SENDING IS THE WHOLE POINT. Same who, same verb, same
;; arguments, same cursor, same id -- so it is the same request, and the
;; store says the work is done rather than doing it again.
(want "the same request again is a replay, not a second block (section-13 L21)"
      (list (let ((a (send! d1 (req-of "r-1" AFTER1 "One") "One")))
              (list (car a) (cadr a)))
            (titles-of (open-and-reduce d1)))
      (list '(ok (replay #t)) '("One")))
;; A DIFFERENT ID IS A DIFFERENT REQUEST, whatever the content says. A
;; client that means to insert a second section with the same title is
;; entitled to, and nothing about the two records distinguishes them
;; except the ids their authors chose.
(want "TWIN: the same content under a different id is a second request"
      (list (car (send! d1 (req-of "r-2" AFTER1 "One") "One"))
            (titles-of (open-and-reduce d1)))
      (list 'ok '("One" "One")))
;; AND THE SAME ID OVER DIFFERENT CONTENT IS THE ERROR THAT MATTERS. The
;; client reused a name; executing would append something its author
;; never asked for under an id that already names something else.
(want "the same id over different content is refused (section-13 L21)"
      (let ((a (send! d1 (req-of "r-1" AFTER1 "Two") "Two")))
        (list (car a) (cadr a)))
      '(error req-mismatch))
(want "and nothing was written"
      (titles-of (open-and-reduce d1))
      '("One" "One"))

(printf "\n== a request whose positions are in doubt ==\n")
(define d2 (fresh-store! '()))
(define W2 (local-of d2))
;; A STRETCH THE STORE IS UNSURE OF, SITTING WHERE THIS REQUEST WOULD
;; HAVE LANDED. There is no evidence of the request -- and that is
;; exactly the case where absence proves nothing, because the stretch is
;; where the evidence would be.
(uncertain-write! d2 W2 (list (list W2 0 40)) 'commit)
(want "no evidence, and the positions it could occupy are in doubt (section-13 L21)"
      (let ((a (send! d2 (req-of "r-1" (cons W2 0) "One") "One")))
        (list (car a) (cadr a) (caddr a)))
      (list 'error 'unknown (list 'range-overlaps (list W2 0 40))))
(want "and nothing was written"
      (titles-of (open-and-reduce d2))
      '())
;; TWIN: the same store with the stretch ending at the cursor. The
;; request's positions start just above it, so a stretch that stops there
;; is behind it.
(want "TWIN: a stretch the request could not have reached is not in the way"
      (begin (uncertain-write! d2 W2 (list (list W2 0 0)) 'commit)
             (list (car (send! d2 (req-of "r-1" (cons W2 0) "One") "One"))
                   (titles-of (open-and-reduce d2))))
      (list 'ok '("One")))

(printf "\n== a cursor the record could not have been written at ==\n")
;; THE CURSOR IS NOT DECORATION; it is the bottom of the stretch that
;; protects the request. A client that declares a position far above
;; where its record will actually land gets an interval, after a
;; rollback, that ends far below what it declared -- so the retry's range
;; test finds nothing in its way and the work runs a second time. The
;; sequence is: declare (W . 100) on an empty writer, land at W:1, lose
;; the record to a rollback whose mark is 1, adopt, retry. Refusing the
;; first of those is what closes it.
(define d8 (fresh-store! '()))
(define W8 (local-of d8))
(want "a cursor above the position the record would take is refused"
      (let ((a (send! d8 (req-of "r-1" (cons W8 100) "One") "One")))
        (list (car a) (cadr a) (caddr a)))
      (list 'error 'cursor-unreachable (list 'after (cons W8 100))))
(want "and nothing was written"
      (titles-of (open-and-reduce d8))
      '())
;; TWIN: the cursor the writer is actually standing at is accepted, and
;; so is every cursor below it -- an earlier attempt could have been
;; written against any of those.
(want "TWIN: a cursor at or below the writer's own position is accepted"
      (list (car (send! d8 (req-of "r-2" (cons W8 0) "One") "One"))
            (car (send! d8 (req-of "r-3" (cons W8 1) "Two") "Two")))
      '(ok ok))

(printf "\n== what the write path does without a request ==\n")
;; THE OLD CALLERS ARE UNCHANGED. A write with no request is not a
;; request with no id: nothing is looked up, nothing is refused, and the
;; record carries the plain actor name it always did.
(define d3 (fresh-store! '("One")))
(want "a write with no request executes and is not tracked"
      (list (car (car (with-store-write d3
                        (lambda (st v) (insert-intent "Two")) "t")))
            (titles-of (open-and-reduce d3)))
      (list 'ok '("One" "Two")))

(printf "\n== the replay passes the barrier ==\n")
;; `replay` is the only answer here that promises anything: it tells the
;; client the work is done and will still be there after a power cut. So
;; it goes through the same table a publish goes through -- and if the
;; table cannot be completed, the answer is not given. Taking the
;; segment away is the cheapest way to make the barrier fail: the row it
;; is required to cover is not there, and the store says so instead of
;; saying the work is safe.
(define d5 (fresh-store! '()))
(define W5 (local-of d5))
(want "CONTROL: the request runs, and repeating it is a replay"
      (list (car (send! d5 (req-of "r-1" (cons W5 0) "One") "One"))
            (let ((a (send! d5 (req-of "r-1" (cons W5 0) "One") "One")))
              (list (car a) (cadr a))))
      (list 'ok '(ok (replay #t))))
;; THE READING IS THE PRODUCT'S OWN TRACE. With THEOURGIA_TRACE on, every
;; fsync reports its subject, so what the replay flushed can be read
;; rather than inferred -- and the subjects are the barrier's table, not
;; some flush the write path would have done anyway, because a replay
;; writes nothing at all.
;;
;; The subject is written rather than printed as a string, so `read`
;; gives it back as a symbol; a filter asking `string?` here would
;; collect nothing, which reads exactly like a replay that flushed
;; nothing.
(define (fsync-subjects-of thunk)
  (let* ((port (open-output-string))
         (subjects
           (parameterize ((current-error-port port))
             (trace-enable! #t)
             (let ((r (guard (e (#t (trace-enable! #f) (raise e))) (thunk))))
               (trace-enable! #f)
               r)
             (get-output-string port))))
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i (string-length subjects)) (reverse out))
        ((char=? (string-ref subjects i) #\newline)
         (let ((datum (guard (e (#t #f))
                        (read (open-string-input-port (substring subjects start i))))))
           (loop (+ i 1) (+ i 1)
                 (if (and (pair? datum) (eq? (car datum) 'trace)
                          (eq? (cadr datum) 'fsync) (symbol? (caddr datum)))
                     (cons (symbol->string (caddr datum)) out)
                     out))))
        (else (loop (+ i 1) start out))))))

;; WHICH OF THE FOUR ACTUALLY DISCRIMINATES: only `writers`. The other
;; three are flushed by the ordinary write path as well, so they
;; document the table rather than test it -- measured by removing the
;; barrier, which flips that one element and no other. The twin below
;; asks about that element alone.
(want "the replay flushes the record's file, the directory naming it, and the store"
      (let ((subjects (fsync-subjects-of
                        (lambda () (send! d5 (req-of "r-1" (cons W5 0) "One") "One")))))
        (map (lambda (p) (and (member p subjects) #t))
             (list (string-append d5 "/writers/" W5 "/000001.sexp")
                   (string-append d5 "/writers/" W5)
                   (string-append d5 "/writers")
                   d5)))
      '(#t #t #t #t))
;; A FIRST EXECUTION MAKES THE SAME PROMISE, at the end rather than
;; instead: what changed with per-request commit is WHEN, not whether.
(want "and so does a first execution, at the end of its request"
      (let ((subjects (fsync-subjects-of
                        (lambda () (send! d5 (req-of "r-fresh" (cons W5 0) "Two") "Two")))))
        (and (member (string-append d5 "/writers") subjects) #t))
      #t)
;; TWIN: A REQUEST THAT WROTE NOTHING DOES NOT. Nothing it describes is
;; claimed to survive a crash, and putting the store's whole recovery
;; closure on the way out of every refusal would charge refusals for a
;; promise they do not make.
(want "TWIN: a request that wrote nothing does not make it"
      (let ((subjects (fsync-subjects-of
                        (lambda ()
                          (with-store-write d5
                            (lambda (st v)
                              (list (list 'set "nosuchblock" 'title "x")))
                            "agent:claude"
                            (req-of "r-nothing" (cons W5 0) "x"))))))
        (and (member (string-append d5 "/writers") subjects) #t))
      #f)

;; AND IT GOES WHERE THE RECORD IS, not where the writer happens to be
;; now. After an adopt the record a replay promises sits on a generation
;; this store has retired, and flushing the CURRENT writer's current
;; segment says nothing about it. The answer carries the record the
;; decision found, and the barrier follows it.
(define d7 (fresh-store! '()))
(define old7 (local-of d7))
(want "CONTROL: the request runs on the generation that is current then"
      (car (send! d7 (req-of "r-1" (cons old7 0) "One") "One"))
      'ok)
(append-line! (string-append d7 "/writers/" old7 "/000001.sexp")
              "deadbeef (9 1757300000009 \"t\" () (put ((kind . section))))\n")
(store-adopt! d7)
(define new7 (local-of d7))
(want "CONTROL: the store now writes on a different generation"
      (not (string=? old7 new7))
      #t)
;; THE GUARD IS PART OF THE ROW. A barrier aimed at the wrong writer
;; finds no segment there and refuses -- correctly -- and an unguarded
;; row would end the file on that condition with no failure count, which
;; reads exactly like a run nobody made.
(want "the replay flushes the retired generation's segment, not the current one"
      (guard (e (#t (list 'raised)))
        (let ((subjects (fsync-subjects-of
                          (lambda () (send! d7 (req-of "r-1" (cons old7 0) "One") "One")))))
          (list (and (member (string-append d7 "/writers/" old7 "/000001.sexp") subjects) #t)
                (and (member (string-append d7 "/writers/" new7 "/000001.sexp") subjects) #t))))
      '(#t #f))

(printf "\n== a cursor on a writer that has since been succeeded ==\n")
;; A request written against a retired writer's cursor could have landed
;; anywhere on any writer that succeeded it -- it never held a cursor
;; over there at all. So a stretch in doubt on the SUCCESSOR is in this
;; request's way, and a store that tested only the writer the cursor
;; names would answer "clear" for the generation a retry is most likely
;; to be looking at.
(define d6 (fresh-store! '("One")))
(define old6 (local-of d6))
;; A DAMAGED TAIL IS WHAT MAKES AN ADOPT NECESSARY. A healthy store
;; refuses to adopt, and a fixture that asked anyway would go on with
;; one writer and call it two.
(append-line! (string-append d6 "/writers/" old6 "/000001.sexp")
              "deadbeef (9 1757300000009 \"t\" () (put ((kind . section))))\n")
(store-adopt! d6)
(define new6 (local-of d6))
(want "CONTROL: the adopt produced a successor, and the chain is followed"
      (list (not (string=? old6 new6)) (store-successors d6 old6))
      (list #t (list (list new6) #f)))
(want "a stretch in doubt on the successor is in the old cursor's way"
      (begin
        (uncertain-write! d6 new6 (list (list new6 0 40)) 'commit)
        (let ((a (send! d6 (req-of "r-9" (cons old6 0) "Two") "Two")))
          (list (car a) (cadr a) (caddr a))))
      (list 'error 'unknown (list 'range-overlaps (list new6 0 40))))
;; TWIN: the same stretch on a writer that is NOT a successor is not in
;; the way, which is what makes the row above about the chain rather
;; than about "any stretch anywhere stops everything".
(want "TWIN: the same stretch on a writer nothing succeeded is not"
      (begin
        (uncertain-write! d6 new6 '() 'commit)
        (uncertain-write! d6 old6 (list (list "unrelat0" 0 40)) 'commit)
        (car (send! d6 (req-of "r-10" (cons old6 0) "Two") "Two")))
      'ok)

(printf "\n== an operator's determination reaches the request ==\n")
;; A RESOLUTION IS A RECORD ABOUT AN IDENTITY, NOT ONE OF ITS RECORDS: it
;; is written by an operator, under the operator's own actor and cursor,
;; and it names the request it resolves in its PAYLOAD. A scan that
;; matched only on the actor would leave every resolution unreachable
;; from the request it resolves -- so a determination that the work
;; already ran would be ignored and the store would run it again.
(define d9 (fresh-store! '()))
(define W9 (local-of d9))
(define AFTER9 (cons W9 0))
(define ID9 (request-identity AFTER9 "r-res"))
(define FP9 (request-fingerprint "agent:claude" 'insert (list "root" "One") AFTER9))
(define (ops-actor seq)
  (list "ops:carol" (cons W9 "fix-1") 'single "opsfp" #f (cons W9 (- seq 1))))
(define (append-record! seq payload)
  (let ((p (string-append d9 "/writers/" W9 "/000001.sexp")))
    (call-with-port (open-file-output-port p (file-options no-fail no-truncate))
      (lambda (o)
        (set-port-position! o (bytevector-length (call-with-port (open-file-input-port p)
                                                   get-bytevector-all)))
        (put-bytevector o (encode-record seq (+ 1757300000000 seq) (ops-actor seq)
                                         '() (storable-encode payload)))))))
(want "CONTROL: with no resolution the request executes"
      (car (send! d9 (make-write-request "agent:claude" 'insert (list "root" "Zero")
                                         "r-zero" AFTER9)
                  "Zero"))
      'ok)
(want "a confirmation written into the log settles the request"
      (begin
        (append-record! 2 (list 'resolve ID9 FP9 'all 'executed (list W9 0 40)
                                '(supersedes)))
        (let ((a (send! d9 (make-write-request "agent:claude" 'insert (list "root" "One")
                                               "r-res" AFTER9)
                        "One")))
          (list (car a) (cadr a))))
      '(error resolved-executed))
(want "and nothing was written"
      (titles-of (open-and-reduce d9))
      '("Zero"))

(printf "\n== metadata this store cannot read ==\n")
;; TWO OF THE FIVE KINDS OF UNCERTAIN STRETCH EXIST ONLY IN THE FILE --
;; the rollback interval and the identity-mismatch interval, whose
;; coordinates existed only at the moment an adopt computed them. So a
;; file that cannot be read takes those away, and a retried request that
;; fell in one would be told to run again. The report cannot be a warning
;; that execution proceeds past.
(define d10 (fresh-store! '()))
(define W10 (local-of d10))
(want "an unreadable uncertain cache stops the request"
      (begin
        (put! (string-append d10 "/writers/" W10 "/uncertain.sexp")
              (string->utf8 "((not an"))
        (let ((a (send! d10 (req-of "r-1" (cons W10 0) "One") "One")))
          (list (car a) (cadr a) (caddr a))))
      (list 'error 'unknown (list 'uncertain-cache 'unreadable)))
(want "TWIN: a readable one does not"
      (begin
        (uncertain-write! d10 W10 '() 'commit)
        (car (send! d10 (req-of "r-1" (cons W10 0) "One") "One")))
      'ok)
;; AND A RETIREMENT RECORD THAT CANNOT BE READ IS NOT "NEVER RETIRED".
;; The two lead to opposite answers: a chain that stops early leaves a
;; generation's uncertain stretches out of the test, so a request that
;; could have landed there is told to run.
(want "an unreadable retirement record stops the request"
      (begin
        (put! (string-append d10 "/writers/" W10 "/retired.sexp")
              (string->utf8 "((format 1) (tx"))
        (let ((a (send! d10 (req-of "r-2" (cons W10 0) "Two") "Two")))
          (list (car a) (cadr a) (caddr a))))
      (list 'error 'unknown (list 'retirement-unreadable W10)))

(printf "\n== the count of flushes does not grow with the batch ==\n")
;; THIS IS THE ROW THE WHOLE BATCH EXISTS FOR. On the real corpus one
;; `insert` produced ten fsyncs, and on macOS each is fsync plus
;; F_FULLFSYNC at 5 to 20 ms -- 1318 records took 57 s, about 43 ms each,
;; independent of record size. The unit was wrong, not merely the cost:
;; what the store promises is that a REQUEST is durable when it is
;; answered.
;;
;; Without this row "one barrier per request" is a claim about code
;; rather than a reading of behaviour. It is read from the product's own
;; trace, the same way the barrier rows are.
(define d12 (fresh-store! '()))
(define W12 (local-of d12))
(define (insert-n n)
  (let loop ((k 0) (out '()))
    (if (= k n)
        (reverse out)
        (loop (+ k 1)
              (cons (list 'insert 'root #f
                          (list (cons 'kind 'section)
                                (cons 'title (string-append "b" (number->string k)))))
                    out)))))
(define (flushes-for n id)
  (length (fsync-subjects-of
            (lambda ()
              (with-store-write d12 (lambda (st v) (insert-n n)) "agent:claude"
                                (make-write-request "agent:claude" 'batch (list id) id
                                                    (cons W12 0)))))))
(define one-item (flushes-for 1 "n-1"))
(define five-items (flushes-for 5 "n-5"))
(define nine-items (flushes-for 9 "n-9"))
;; THE READING: a batch of five and a batch of nine cost the SAME number
;; of flushes. That is the claim -- the cost is per request, not per
;; record. (A single-intent request costs fewer still: it has no receipt,
;; so it runs one barrier where a batch runs two.)
;; WHERE THE REMAINING GROWTH IS, MEASURED RATHER THAN ASSERTED AWAY.
;; The log's own flush is gone from the per-record path: it is the
;; request's barrier now. What still grows is the REGISTRY -- read,
;; raised and rewritten once per record, and each rewrite is an atomic
;; write, so two flushes. Five items to nine is four more records and
;; exactly eight more flushes.
;;
;; The one-shot reservation that would remove it is written and not
;; reached: reserving a whole request's range leaves the water mark above
;; the last record on disk whenever the request writes fewer records than
;; it asked for, and that is what the rollback gate reads as a rollback.
;; This row is what will change when that is settled, and it says the
;; number rather than hiding it.
;; THE READING, AND IT IS THE ONE THE WHOLE BATCH EXISTS FOR: a batch of
;; nine records costs exactly what a batch of five costs. The cost is per
;; REQUEST now, not per record.
;;
;; It used to grow twice over. The log fsynced once per record -- ten
;; fsyncs per `insert` on the real corpus, 43 ms each on this machine,
;; 1318 records in 57 s -- and the registry was read, raised and
;; rewritten once per record on top of that. Removing only the first left
;; (16, 38, 46): still two flushes a record, all of them the registry's.
;; Both are gone: one barrier and one reservation per request.
;;
;; THE CONSTANT WENT UP BY TWO, and that is the two-fact registry entry
;; being paid for: the barrier writes `written` back once per request, so
;; the store can tell "a reservation nobody used" from "history that went
;; missing". Two flushes a request, not two a record.
(want "a batch of nine costs exactly what a batch of five costs"
      (= five-items nine-items)
      #t)
;; TWIN: and the number is not zero -- a measurement that read nothing
;; would satisfy the row above just as well.
(want "TWIN: and both of them cost something"
      (> five-items 0)
      #t)
(want "and a single-intent request costs less than a batch, not more"
      (< one-item five-items)
      #t)
;; TWIN: and every record really was written, so the rows above are not
;; measuring a batch that did nothing. 1 + 5 + 9 blocks.
(want "TWIN: and all fifteen blocks are there"
      (length (titles-of (open-and-reduce d12)))
      15)

(printf "\n== a replay reconciles the registry, because it acknowledges ==\n")
;; `written` IS RAISED IN A SECOND MACHINE-LOCK SECTION AFTER THE
;; BARRIER, so a crash between the two leaves a record durable and the
;; registry counting fewer. A retry then finds the record, flushes it and
;; answers `ok` -- and if it did not also reconcile, that ACKNOWLEDGED
;; record could afterwards be restored away with the rollback gate
;; silent, because the gate compares `written` with the log's end and
;; both would be low.
(define d13 (fresh-store! '()))
(define W13 (local-of d13))
;; THE HELPERS NAME THE WRITER THEY MEAN. An earlier version took the
;; first six-element entry and rewrote every six-element entry, which is
;; the same thing while one store has one writer -- and silently the
;; wrong thing the moment an adopt puts a second entry in the file. A
;; reader that cannot say WHICH entry it read cannot tell "this
;; generation was reconciled" from "some generation was".
;;
;; AN ENTRY IS `(store-id instance writer authorised state written)`,
;; six elements, one shape; the writer is the third.
;; AN ENTRY IS KEYED BY THREE FIELDS, NOT ONE. The helpers below took the
;; writer alone, which is the same thing while one machine holds one
;; store -- and silently the wrong thing as soon as two entries share a
;; writer name. The product matches on all three (`registry-update`), so
;; a fixture matching on one cannot tell "it found the right entry" from
;; "it found an entry".
(define registry-entry-store-id car)
(define registry-entry-instance cadr)
(define registry-entry-writer caddr)
(define (registry-entry-key e)
  (list (registry-entry-store-id e) (registry-entry-instance e)
        (registry-entry-writer e)))
(define (read-registry)
  (let* ((text (let ((b (slurp (registry-path)))) (if (string? b) b "")))
         (reg (guard (e (#t '())) (read (open-string-input-port text)))))
    (if (list? reg) reg '())))
;; TAKES THE WRITER, AND REFUSES TO ANSWER IF TWO ENTRIES CLAIM IT.
;; "The entry for W" is only a well-formed question while exactly one
;; entry has that writer; answering the first of several is how a row
;; comes to be about an entry nobody chose.
(define (registry-entry-for writer)
  (let ((matches (filter (lambda (e)
                           (and (list? e) (= 6 (length e))
                                (equal? (registry-entry-writer e) writer)))
                         (read-registry))))
    (cond ((null? matches) #f)
          ((null? (cdr matches)) (car matches))
          (else (list 'ambiguous (length matches))))))
;; ANSWERS A VALUE FOR AN ENTRY THAT IS NOT THERE. A reconciliation that
;; matched on the writer alone leaves this key gone, and `list-ref` on #f
;; raises -- which ends the fixture and reads as a broken file rather
;; than as a product that reconciled the wrong entry.
(define (written-at key)
  (let ((e (registry-entry-at key)))
    (if e (list-ref e 5) 'no-entry)))
(define (registry-entry-at key)
  (let loop ((es (read-registry)))
    (cond ((null? es) #f)
          ((and (list? (car es)) (= 6 (length (car es)))
                (equal? (registry-entry-key (car es)) key))
           (car es))
          (else (loop (cdr es))))))
;; AND THE ROWS SAY HOW MANY ENTRIES THEY EXPECT TO FIND. A helper that
;; answers about one writer is only trustworthy beside a count: "the
;; entry for W says 1" and "there is exactly one entry" are different
;; claims, and the second is the one an adopt breaks.
(define (registry-writers)
  (map registry-entry-writer
       (filter (lambda (e) (and (list? e) (= 6 (length e)))) (read-registry))))
(define (written-of writer)
  (let ((e (registry-entry-for writer)))
    (and e (list-ref e 5))))
(define (write-registry-text! reg)
  (call-with-port (open-file-output-port (registry-path) (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 (format "~s\n" reg))))))
(define (set-written-at! key to)
  (write-registry-text!
    (map (lambda (e)
           (if (and (list? e) (= 6 (length e)) (equal? (registry-entry-key e) key))
               (append (list-head e 5) (list to))
               e))
         (read-registry))))
(define (set-written! writer to)
  (let ((e (registry-entry-for writer)))
    (if (or (not e) (eq? (car e) 'ambiguous))
        (assertion-violation 'set-written!
                             "the writer does not name exactly one entry" writer)
        (set-written-at! (registry-entry-key e) to))))
(want "CONTROL: a request runs and the registry counts its record"
      (begin (send! d13 (req-of "r-1" (cons W13 0) "One") "One")
             (list (written-of W13) (registry-writers)))
      (list 1 (list W13)))
;; The crash between the barrier and the reconciliation, staged directly:
;; the record is on disk and durable, and the registry has not caught up.
(want "a replay of a record the registry does not count raises written to it"
      (begin
        (set-written! W13 0)
        (let ((a (send! d13 (req-of "r-1" (cons W13 0) "One") "One")))
          (list (car a) (cadr a) (written-of W13))))
      (list 'ok '(replay #t) 1))
;; TWIN: and it only ever raises -- a replay of an old record cannot pull
;; a frontier a later request established back down.
(want "TWIN: a replay never lowers what a later request established"
      (begin
        (send! d13 (req-of "r-2" (cons W13 1) "Two") "Two")
        (send! d13 (req-of "r-1" (cons W13 0) "One") "One")
        (written-of W13))
      2)

;; AND THE GENERATION IT RECONCILES IS THE GENERATION THE RECORD IS ON.
;; The rows above use one writer, so they cannot tell "the replay
;; reconciles" from "the replay reconciles the writer this machine is
;; currently extending". Those are different statements, and the
;; difference is exactly where the defect lives: the record a replay
;; acknowledges is very often on a RETIRED generation -- that is what a
;; retry after a crash looks like -- and it is the retired generation's
;; entry that the rollback gate will read when someone restores that
;; writer's directory from a backup.
;;
;; An implementation that skipped retired generations would leave this
;; store acknowledging a record while the only witness that could refuse
;; its disappearance stays low.
(define d15 (fresh-store! '()))
(define old15 (local-of d15))
;; TWO RECORDS ON THE OLD GENERATION, BEFORE THE ADOPT. A retired writer
;; cannot be extended -- a later send lands on whatever generation is
;; current, however its cursor is written -- so the only moment the old
;; generation can be given a second record is now. It needs one because
;; the twin below has to watch a mark that is HIGHER than the current
;; generation's, and a generation holding one record cannot provide it.
(send! d15 (req-of "r-old" (cons old15 0) "One") "One")
(send! d15 (req-of "r-old2" (cons old15 1) "Three") "Three")
;; A DAMAGED TAIL IS WHAT MAKES AN ADOPT NECESSARY, as in the successor
;; rows above: a healthy store refuses to adopt, and a fixture that
;; asked anyway would go on with one writer and call it two.
(append-line! (string-append d15 "/writers/" old15 "/000001.sexp")
              "deadbeef (9 1757300000009 \"t\" () (put ((kind . section))))\n")
(store-adopt! d15)
(define new15 (local-of d15))
;; THE NEW GENERATION HAS TO WRITE BEFORE IT HAS AN ENTRY. A reservation
;; is taken at the first append, not at the adopt, so a store that has
;; just adopted has exactly one entry -- and a twin asking about the new
;; generation's mark would be comparing #f with #f and passing whatever
;; the product did. The row below is here so that the twin further down
;; has two entries to tell apart.
(send! d15 (req-of "r-new" (cons new15 0) "Two") "Two")
(want "CONTROL: the adopt retired the old writer, and both generations are counted"
      (list (and old15 new15 (not (string=? old15 new15)))
            (length (registry-writers))
            (and (member old15 (registry-writers)) #t)
            (and (member new15 (registry-writers)) #t)
            (written-of old15))
      (list #t 2 #t #t 2))
;; ONLY THE RETIRED ENTRY IS LOWERED, so whichever entry comes back up
;; is the entry the product chose -- not one the fixture arranged to be
;; the only candidate.
;; THE RETIRED GENERATION STANDS ABOVE THE CURRENT ONE, and that ordering
;; is the whole of what the twin below can see. With both marks at 1 a
;; reconciliation applied to EVERY generation is indistinguishable from
;; one applied to the right generation: raising the current mark to 1
;; when it is already 1 changes nothing, so the twin passes either way.
;; Replaying a record at sequence 2 while the current generation is still
;; at 1 gives an indiscriminate implementation somewhere to show itself.
(define new15-before (written-of new15))
(want "CONTROL: the retired generation now stands above the current one"
      (list (written-of old15) new15-before)
      (list 2 1))
(want "a replay of a record on a retired generation reconciles THAT generation"
      (begin
        (set-written! old15 0)
        (let ((a (send! d15 (req-of "r-old2" (cons old15 1) "Three") "Three")))
          (list (car a) (cadr a) (written-of old15))))
      (list 'ok '(replay #t) 2))
;; TWIN: and it did not reach across to the current generation's entry,
;; which is what makes the row above about the record's generation
;; rather than about "the replay raises every mark it can find". The
;; replayed sequence is 2 and the current generation is at 1, so a
;; reconciliation that walked every writer would leave 2 here.
(want "TWIN: and the current generation's mark is untouched"
      (list (written-of new15) new15-before)
      (list 1 1))

;; AND THE ENTRY IT RAISES IS THE ONE WITH THIS STORE'S id AND instance,
;; not merely the one with this writer's name. Every row above uses a
;; store whose registry holds one entry per writer, so a reconciliation
;; that matched on the writer alone passes all of them -- and would raise
;; some OTHER store's frontier past records that store never wrote, which
;; is a rollback gate that has been told the wrong history.
;;
;; TWO DECOYS, seeded directly: same writer, different store-id and
;; different instance. They are not other live stores -- one machine
;; running one store is the ordinary case -- they are what the registry
;; looks like when a store has been copied, re-instantiated, or restored
;; under a new identity, which is exactly when the gate matters.
(define d17 (fresh-store! '()))
(define W17 (local-of d17))
(send! d17 (req-of "r-k" (cons W17 0) "One") "One")
(define real-key (registry-entry-key (registry-entry-for W17)))
(define decoy-store (list "other-store" (cadr real-key) W17))
(define decoy-instance (list (car real-key) '((machine "elsewhere")) W17))
;; THE DECOYS GO FIRST, AND THEY HOLD DIFFERENT NUMBERS. Appended after
;; the real entry they only test the UPDATE side: a lookup that matched
;; on the writer alone and took the first match would still land on the
;; real entry, so the row passed a reader that cannot tell three entries
;; apart. Put in front, that reader reaches a decoy instead.
;;
;; AND EACH DECOY CARRIES ITS OWN FRONTIER, so the reading says WHICH
;; entry moved rather than only that something did: three zeroes would
;; make any wrong answer look like the right one.
(write-registry-text!
  (append (list (list (car decoy-store) (cadr decoy-store) (caddr decoy-store)
                      9 'active 5)
                (list (car decoy-instance) (cadr decoy-instance) (caddr decoy-instance)
                      9 'active 7))
          (read-registry)))
;; THE REAL ENTRY IS LOWERED BY KEY, not by writer: `set-written!` now
;; refuses a writer that names more than one entry, which is the point of
;; this section -- so the row that sets up the section has to say which
;; of the three it means.
(set-written-at! real-key 0)
(want "CONTROL: three entries share this writer, and the real one is lowered"
      (list (length (registry-writers))
            (written-at real-key)
            (written-at decoy-store)
            (written-at decoy-instance))
      (list 3 0 5 7))
(want "a replay raises only the entry with this store's id and instance"
      (begin
        (send! d17 (req-of "r-k" (cons W17 0) "One") "One")
        (list (written-at real-key)
              (written-at decoy-store)
              (written-at decoy-instance)))
      (list 1 5 7))
;; TWIN: and no other field of either decoy moved, so the row above is
;; about which entry was chosen rather than about one number.
(want "TWIN: the decoys are untouched in every field"
      (list (registry-entry-at decoy-store) (registry-entry-at decoy-instance))
      (list (list (car decoy-store) (cadr decoy-store) (caddr decoy-store) 9 'active 5)
            (list (car decoy-instance) (cadr decoy-instance) (caddr decoy-instance)
                  9 'active 7)))

(printf "\n== a batch is one receipt and a request per item ==\n")
;; A BATCH IS NOT ONE REQUEST WITH SEVERAL SUB-OPERATIONS. Each item is
;; its own request, under its own identity `(batch <req> k)`, and what
;; holds them together is the receipt. One batch-level identity would
;; leave a retry able to say only how far the whole batch got -- never
;; which item -- and the per-item cursors the range test needs would have
;; nowhere to live.
;;
;; THE RECEIPT IS FIRST, and it is what lets a retry say anything at all:
;; without it a crash between two items leaves a set of records and no
;; statement of what the set was supposed to be, so "item 3 is missing"
;; and "there were only three items" are the same picture.
(define d11 (fresh-store! '()))
(define W11 (local-of d11))
(define (records-of-writer d w)
  (let* ((raw (slurp (string-append d "/writers/" w "/000001.sexp")))
         (text (if (string? raw) raw "")))
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i (string-length text)) (reverse out))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1) (if (and (pair? r) (eq? (car r) 'ok)) (cons r out) out))))
        (else (loop (+ i 1) start out))))))
(define two-intents
  (list (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title "One")))
        (list 'insert (list 'from 0) #f (list (cons 'kind 'section) (cons 'title "Two")))))
(define (send-batch! id)
  (with-store-write d11 (lambda (st v) two-intents) "agent:claude"
                    (make-write-request "agent:claude" 'batch (list "two") id
                                        (cons W11 0))))
(define two-answer (send-batch! "r-1"))
(want "both items are written"
      (map car two-answer)
      '(ok ok))
(want "and the log holds the receipt and the two, in that order"
      (map (lambda (r) (car (storable-decode (list-ref r 5)))) (records-of-writer d11 W11))
      '(batch put put))
;; EACH ITEM CARRIES ITS OWN IDENTITY. `(batch "r-1" k)` is the request
;; id of item k, and the record calls itself `single` because one item of
;; one intent is one sub-operation belonging to no plan.
(want "each item's actor names its own item of the batch"
      (map (lambda (r) (let ((a (list-ref r 3)))
                         (list (cdr (list-ref a 1)) (list-ref a 2) (list-ref a 4))))
           (cdr (records-of-writer d11 W11)))
      (list (list (list 'batch "r-1" 0) 'single #f)
            (list (list 'batch "r-1" 1) 'single #f)))
;; THE ENTRIES ARE THE CURSORS THE ITEMS WERE ACTUALLY WRITTEN AGAINST --
;; computed before anything was written, from the position the session
;; was standing at, and checked against each record's real position
;; before that record existed. An entry computed afterwards from what
;; happened would agree with anything.
(want "the receipt declares each item's own cursor, one below its record"
      (let* ((rs (records-of-writer d11 W11))
             (entries (list-ref (storable-decode (list-ref (car rs) 5)) 4)))
        (list entries
              (map (lambda (r) (cons W11 (cadr r))) (cdr rs))))
      (list (list (cons 0 (cons W11 1)) (cons 1 (cons W11 2)))
            (list (cons W11 2) (cons W11 3))))
;; AND THE WHOLE THING IS ONE BATCH. Sent again under the same id it is
;; not executed a second time.
;; AND IT NAMES THE LAST ITEM -- the furthest point the promise has to
;; reach, and the record the barrier before the word has to cover.
(want "the same batch again is a replay of its last item, and writes nothing"
      (let* ((before (length (records-of-writer d11 W11)))
             (last-at (cons W11 (cadr (list-ref (records-of-writer d11 W11) 2))))
             (a (car (send-batch! "r-1"))))
        (list (car a) (cadr a) (caddr a)
              (= before (length (records-of-writer d11 W11)))
              (equal? (cadr (caddr a)) last-at)))
      (list 'ok '(replay #t) (list 'event (cons W11 3)) #t #t))
;; TWIN: a different id is a different batch, and it does run.
(want "TWIN: a different id is a different batch"
      (let ((before (length (records-of-writer d11 W11))))
        (send-batch! "r-2")
        (- (length (records-of-writer d11 W11)) before))
      3)
(want "and the tree has both pairs, each second block under its first"
      (titles-of (open-and-reduce d11))
      '("One" "One" "Two" "Two"))


(printf "\n== what is written and what is read back are the same value ==\n")
;; STORAGE ESCAPES SOME VALUES AND THE READER MUST UNDO IT EXACTLY ONCE.
;; A field value that is a list headed by a reserved marker -- here
;; `("#%char" 97)` -- is written as `("#%quote" ("#%char" 97))` so that
;; reading it back cannot be confused with the marker's own meaning.
;;
;; THE ENCODING WAS ONE-WAY. It was applied on append and never undone on
;; delivery, so the same record reduced to two different things: the
;; write session saw the value the caller wrote, and every later read saw
;; the escaped form. Both reduce without complaint. The hashes differ --
;; so a write that answered `ok` returned a block hash that reopening the
;; store immediately contradicted, and nothing said so.
;;
;; THE STIMULUS HAS TO CARRY THE ESCAPE, AND IT TOOK FOUR TRIES. A bare
;; `"#%char"` is a string and is never escaped. `(note "#%char" 97)` is
;; an entry whose spine head is `note`, so the encoder sees no marker
;; there either. A value of `("#%char" 97)` is the SAME shape once it is
;; the cdr of an entry -- the spine is still `(note "#%char" 97)`. Only a
;; value of `(("#%char" 97))`, a marker-headed list INSIDE the value,
;; is escaped.
;;
;; ALL THREE WRONG VERSIONS READ IDENTICALLY WITH AND WITHOUT THE FIX,
;; and each time the reading said the defect was not there. The value
;; below is checked against the record on disk by the row that follows.
(define d18 (fresh-store! '()))
(define W18 (local-of d18))
(define escaped-answer
  (with-store-write d18
    (lambda (st v)
      (list (list 'insert 'root #f
                  (list (cons 'kind 'section)
                        (cons 'title "Escaped")
                        (cons 'note (list (list "#%char" 97)))))))
    "agent:claude" #f))
(define escaped-pair
  (let ((a (car escaped-answer)))
    (car (cadr (assq 'state (cdr a))))))
;; CONTROL: THE RECORD ON DISK REALLY IS ESCAPED. Without this the rows
;; below are about a value the encoder left alone, which is how the first
;; three attempts passed while the defect was still present.
(want "CONTROL: the record was written, and the bytes on disk carry the escape"
      (let* ((raw (slurp (string-append d18 "/writers/" W18 "/000001.sexp")))
             (text (if (string? raw) raw "")))
        (list (car (car escaped-answer))
              (string? (cdr escaped-pair))
              (and (> (string-length text) 0)
                   (let loop ((i 0))
                     (cond ((> (+ i 8) (string-length text)) #f)
                           ((string=? (substring text i (+ i 8)) "#%quote\"") #t)
                           (else (loop (+ i 1))))))))
      (list 'ok #t #t))
;; THE ROW: the hash the write answered and the hash a reopened store
;; computes for the same block are the same string.
(want "the hash a write answers is the hash a reopened store computes"
      (equal? (cdr escaped-pair) (block-hash (open-and-reduce d18) (car escaped-pair)))
      #t)
;; TWIN: and the value itself comes back as it went in -- not as the
;; escaped form. Comparing hashes alone would also pass a store that
;; escaped on both paths and returned the wrong value consistently.
(want "TWIN: and the field reads back as the value that was written"
      (let* ((st (open-and-reduce d18))
             (b (state-read st (car escaped-pair)))
             (fields (cdr (assq 'fields b))))
        (cdr (assq 'note fields)))
      (list (list "#%char" 97)))

(printf "\n== the frontier is raised on a real entry, or the answer says so ==\n")
;; A REPLAY IS AN ACKNOWLEDGEMENT, so it tells the registry how far
;; records reached -- and that update is keyed by the store's identity
;; and the writer. The merge that raises the number takes no action when
;; no entry matches, so a key that finds nothing left the registry
;; untouched while the replay was answered `ok`: the number never moves,
;; and a store later restored to an earlier sequence walks past the
;; rollback check this number exists to fail. The acknowledged request
;; then runs a second time, which is the outcome the whole mechanism is
;; for. Silence there reads exactly like success.
;;
;; A RESEND IS WHAT REACHES IT. A fresh write reserves first, and the
;; reservation re-creates the entry before reconciliation ever looks --
;; so the state is unreachable that way, and a case built on a fresh
;; write measures a different refusal. The replay path takes no
;; reservation: it finds the record, flushes it, and reconciles.
(define d20 (fresh-store! '()))
(define W20 (local-of d20))
(want "CONTROL: the request runs and the registry records how far it reached"
      (list (car (send! d20 (req-of "r-1" (cons W20 0) "One") "One"))
            (written-of W20))
      (list 'ok 1))
(define registry-with-entry (let ((b (slurp (registry-path)))) (if (string? b) b "")))
(want "a replay whose registry entry is gone says so rather than acknowledging"
      (begin
        (write-registry-text! '())
        (let ((a (send! d20 (req-of "r-1" (cons W20 0) "One") "One")))
          (list (car a) (cadr a) (car (caddr a)))))
      (list 'error 'unknown 'replay-barrier-failed))
;; TWIN: with the entry back the same resend is answered as the replay it
;; is. Without this the row above is also passed by a build that has
;; stopped answering replays at all.
(want "TWIN: with the entry restored the same resend is a replay"
      (begin
        (call-with-port (open-file-output-port (registry-path) (file-options no-fail))
          (lambda (o) (put-bytevector o (string->utf8 registry-with-entry))))
        (let ((a (send! d20 (req-of "r-1" (cons W20 0) "One") "One")))
          (list (car a) (cadr (assq 'replay (cdr a))))))
      (list 'ok #t))

(printf "\n== a request of nothing ==\n")
;; SIX ROWS HERE WERE RETIRED ON 2026-09-17, BECAUSE THE RULE THEY
;; STATED WAS REPLACED. They said "a request that produces no
;; sub-operation writes no record", and that is no longer what the store
;; does.
;;
;; THE SOURCE OF THE CHANGE, so this is a retirement and not a fixture
;; that gave up: brief `q3-crash-consistency.md`, section 4, on the empty
;; plan. It fixes two answers: a request with no sub-operations writes an
;; empty plan and a resend of it replays; a plan record deselected by the
;; manifest and then resent answers `unknown`. A request of nothing now
;; writes exactly one record, an
;; empty plan, and that record is what makes the resend a replay rather
;; than a second execution. Under the old rule a resend of an empty
;; request had nothing to find and did the work again.
;;
;; WHO ASKS THE RETIRED QUESTIONS NOW: `empty-plan.sc`, rows QE-01 (the
;; first answer names the durable empty plan; the retry replays it and
;; appends nothing), QE-06 (zero operations write one empty plan, and one
;; operation still uses `single` with no plan) and QE-03 (a pending empty
;; plan is `unknown`; a delivered one is complete).
;;
;; AND TWO OF THE SIX ASKED SOMETHING THOSE ROWS DO NOT. They are kept
;; here with new expectations rather than retired, because a question
;; that survives a rule change loses its expectation, not its row:
;;
;;   * the store does not BURN a sequence number. The old rows checked
;;     this by requiring the next append to land where nothing had been
;;     skipped. The number changes -- the empty plan is itself a record
;;     now -- and the invariant does not: the sequences a writer holds
;;     are still consecutive from 1, with no gap where the empty request
;;     stood. A batch of zero that reserved an empty range and then
;;     appended one past it would leave exactly such a gap.
;;   * emptiness is decided by WHAT THE WORK PRODUCED, not by what the
;;     request asked for. A request carrying real arguments whose
;;     evaluation yields no intent is the ordinary case -- a conditional
;;     edit whose condition is already satisfied -- and a guard reading
;;     the request rather than the intents passes every row above and
;;     still mishandles it.
(define d14 (fresh-store! '()))
(define W14 (local-of d14))
(define (seqs-of d w)
  (map cadr (records-of-writer d w)))
;; AND WHAT THE RECORDS SAY, not only how many there are. A store that
;; appended one ordinary record for each empty request -- rather than
;; the empty plan the rule names -- satisfies every sequence count in
;; this section and still fails the contract that makes the resend a
;; replay. The verb and the plan's own fields are what tell them apart.
(define (payloads-of d w)
  (map (lambda (r) (storable-decode (list-ref r 5))) (records-of-writer d w)))
(with-store-write d14 (lambda (st v)
                        (list (list 'insert 'root #f
                                    (list (cons 'kind 'section) (cons 'title "One")))))
                  "agent:claude" #f)
;; THE GUARD IS PART OF THE ROW. If the empty request raises instead of
;; answering, an uncaught raise would end the fixture here -- the reading
;; would be an abort, which is what a broken fixture also looks like.
;; Caught, it is a failed row beside its twin.
(define empty-answer
  (guard (e (#t (list 'raised)))
    (with-store-write d14 (lambda (st v) '()) "agent:claude"
                      (make-write-request "agent:claude" 'batch '() "r-empty"
                                          (cons W14 1)))))
(want "a request that produces no sub-operation writes one empty plan"
      (list (map car empty-answer) (seqs-of d14 W14) (map car (payloads-of d14 W14)))
      (list '(ok) (list 1 2) '(put plan)))
(want "and the plan it wrote is empty and carries the request id"
      (let ((plan (cadr (payloads-of d14 W14))))
        (list (cadr plan) (list-ref plan 4)))
      (list "r-empty" '()))
(want "and the next append lands at the next sequence, with no gap"
      (begin
        (with-store-write d14 (lambda (st v)
                                (list (list 'insert 'root #f
                                            (list (cons 'kind 'section) (cons 'title "Two")))))
                          "agent:claude" #f)
        (seqs-of d14 W14))
      (list 1 2 3))
;; TWIN: one sub-operation under the same shape of request is executed.
;; Without it the row above is also passed by a store that refuses every
;; request carrying an id.
(want "TWIN: a request of one sub-operation under an id does write"
      (let ((a (with-store-write d14 (lambda (st v)
                                       (list (list 'insert 'root #f
                                                   (list (cons 'kind 'section)
                                                         (cons 'title "Three")))))
                                 "agent:claude"
                                 (make-write-request "agent:claude" 'batch '() "r-one"
                                                     (cons W14 3)))))
        (list (map car a) (seqs-of d14 W14)))
      (list '(ok) (list 1 2 3 4)))

;; A NEW STORE RE-POINTS THE MACHINE HOME, so this one is created after
;; the rows above have finished with theirs. `fresh-store!` sets
;; THEOURGIA_HOME, and a store created in the middle of a section leaves
;; the section's earlier store without the registry its next append has
;; to find -- which reads as the product refusing a request, not as a
;; fixture that moved the furniture.
;; AND THE SAME AT THE BOUNDARY WHERE THE STORE HAS WRITTEN NOTHING. A
;; store's first request is exactly where an empty one is most likely to
;; arrive -- a client that starts up, has nothing to say yet, and says it
;; anyway -- and the empty plan it writes has to be the writer's first
;; record rather than its second.
(define d16 (fresh-store! '()))
(define W16 (local-of d16))
(want "on a store that has written nothing, a request of nothing writes sequence 1"
      (let ((a (guard (e (#t (list 'raised)))
                 (with-store-write d16 (lambda (st v) '()) "agent:claude"
                                   (make-write-request "agent:claude" 'batch '() "r-empty0"
                                                       (cons W16 0))))))
        (list (map car a) (seqs-of d16 W16)))
      (list '(ok) (list 1)))
(want "and the store's first real record follows it at sequence 2"
      (begin
        (with-store-write d16 (lambda (st v)
                                (list (list 'insert 'root #f
                                            (list (cons 'kind 'section)
                                                  (cons 'title "First")))))
                          "agent:claude" #f)
        (seqs-of d16 W16))
      (list 1 2))
(want "a request with arguments whose work produces nothing writes the same empty plan"
      (let ((a (guard (e (#t (list 'raised)))
                 (with-store-write d16 (lambda (st v) '()) "agent:claude"
                                   (make-write-request "agent:claude" 'batch
                                                       (list "root" "Nothing to do")
                                                       "r-empty-args"
                                                       (cons W16 2))))))
        (list (map car a) (seqs-of d16 W16) (map car (payloads-of d16 W16))))
      (list '(ok) (list 1 2 3) '(plan put plan)))
(want "and that plan is empty too, under its own request id"
      (let ((plan (caddr (payloads-of d16 W16))))
        (list (cadr plan) (list-ref plan 4)))
      (list "r-empty-args" '()))
(want "on a store that has written nothing the first record is an empty plan"
      (let ((plan (car (payloads-of d16 W16))))
        (list (car plan) (cadr plan) (list-ref plan 4)))
      (list 'plan "r-empty0" '()))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q7 complete\n")
