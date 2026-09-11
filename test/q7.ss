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
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

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
(want "the same request again is a replay, not a second block"
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
(want "the same id over different content is refused"
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
(want "no evidence, and the positions it could occupy are in doubt"
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
;; TWIN: a request the store has never seen flushes none of that on its
;; way to being executed -- the barrier belongs to the answer that makes
;; a promise, not to every call that reaches the write path.
(want "TWIN: a first execution does not make that promise on the way in"
      (let ((subjects (fsync-subjects-of
                        (lambda () (send! d5 (req-of "r-fresh" (cons W5 0) "Two") "Two")))))
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

(printf "\n== a request of more than one sub-operation ==\n")
;; The plan record and the receipt that have to precede several
;; sub-operations are not written from here yet. Writing the records
;; anyway would give each an actor whose plan-event is #f -- a
;; sub-operation belonging to no plan -- and a retry would read a set of
;; unrelated single requests that happen to share an id. Refusing names
;; what is missing; the wrong actor would not.
(define d4 (fresh-store! '()))
(define W4 (local-of d4))
(want "it is refused by name rather than written with the wrong actor"
      (let ((a (car (with-store-write d4
                      (lambda (st v)
                        (append (insert-intent "One") (insert-intent "Two")))
                      "agent:claude"
                      (req-of "r-1" (cons W4 0) "One")))))
        (list (car a) (cadr a) (caddr a)))
      (list 'error 'request-not-single '(intents 2)))
(want "and nothing was written"
      (titles-of (open-and-reduce d4))
      '())

(printf "\n~a failures\n" bad)
(printf "q7 complete\n")
