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

;; The recovery barrier: one table, two callers, one cell per artefact.
;;
;; A durable answer is a promise that what it describes will still be
;; there after a power cut. `published` says it about a segment that was
;; just installed; `idempotent` says it about one that was installed by
;; some earlier call, possibly by a process that died before flushing
;; anything. The two make the SAME promise, so they have to flush the
;; same things -- and the way to make that true is one table that both
;; go through, not two lists that agree today.
;;
;; THE REPLAY IS THE SHARP MEASUREMENT. A first publish writes: it
;; stages, renames, appends to the manifest, and each of those has
;; durability of its own, so a fault armed anywhere near it can be
;; caught by a step that is not the barrier. A replay writes NOTHING --
;; the bytes and the manifest entry are already exactly right. Anything
;; at all that fails during a replay is the barrier, because the barrier
;; is the only thing a replay does. Every artefact therefore gets its
;; row measured on the replay, where the reading cannot be explained by
;; something else.
;;
;; EACH ROW IS A CHILD PROCESS. `THEOURGIA_INJECT` is read when the
;; library is expanded, so a fault cannot be armed and disarmed inside
;; one run; the fixture writes a small program, runs it with the fault
;; in its environment, and reads back what it printed. A child that
;; could not start prints nothing, and that is reported as
;; `(child-failed ...)` rather than as an absence that could be read as
;; a pass -- the whole hazard of an armed fault is that "nothing
;; happened" and "the guard worked" look alike.

(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Paths
    ;; go into generated scripts and shell commands unquoted, so a root
    ;; with a space or a bracket in it makes the child read nothing at
    ;; all -- which reads exactly like a child that ran and found
    ;; nothing wrong.
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
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim
    ;; it at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps and the scratch root outlives the run, so
    ;; a directory left by an earlier run holding the same pid is handed
    ;; to this one already populated. Both commands are checked, because
    ;; a removal that failed leaves `mkdir -p` succeeding on exactly the
    ;; populated directory this is here to prevent.
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
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


(define scratch (test-dir "q3"))
(define W "wwwlocl0")
(define M "mirrorz9")
(define d (string-append scratch "/store"))
(define home (string-append scratch "/home"))
(define child (string-append scratch "/child.sc"))
(define child-out (string-append scratch "/child.out"))
(define cand (string-append scratch "/cand.bin"))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define bytes (cat (rec 1 '(put ((kind . section))))
                   (rec 2 '(put ((kind . section))))))
(define sha (bytevector->hex (sha256 bytes)))

;; A STORE WITH EVERY ARTEFACT PRESENT. The optional rows -- a retirement
;; record, an uncertain-interval cache, a machine registry -- are absent
;; from an ordinary fresh store, and a row that is absent is a row no
;; fault can be armed at. Each is put there deliberately so that the
;; table below is the whole table and not the part of it this fixture
;; happened to create.
;;
;; The retirement record goes under the LOCAL writer and the uncertain
;; cache under the published-to writer, which is where each belongs; the
;; publish path reads neither on the way to a replay, so their presence
;; changes what the barrier covers and nothing else.
(define (build!)
  (let ((status (system (string-append "rm -rf " d " " home "; mkdir -p "
                                       d "/writers/" W " " d "/writers/" M " "
                                       d "/snap " home))))
    (unless (eqv? 0 status)
      (assertion-violation 'build! "could not build the store" status)))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"q3\"))\n"))
  (put! (string-append d "/lock") (make-bytevector 0))
  (putenv "THEOURGIA_HOME" home)
  (let ((k (instance-install! d))) (owner-install! d W k))
  (store-register! d)
  (put! (string-append d "/writers/" W "/retired.sexp") (string->utf8 "(1 0 0)\n"))
  (put! (string-append d "/writers/" M "/uncertain.sexp") (string->utf8 "()\n")))

(define (first-publish!) (log-publish! d M 1 bytes sha))

;; THE CHILD PUBLISHES ONE CANDIDATE AND SAYS WHAT HAPPENED. The head of
;; the answer, or the marker for a condition that escaped, then whether
;; the segment is on disk and whether the manifest lists it. Printed as
;; one datum, so a half-written line cannot be read as a whole one.
(define (write-child! segment)
  (put! child
        (string->utf8
          (string-append
            "#!chezscheme\n"
            "(import (chezscheme) (theourgia log) (theourgia ffi)\n"
            "        (only (theourgia digest) sha256 bytevector->hex))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(define bytes (call-with-port (open-file-input-port \"" cand "\")\n"
            "                get-bytevector-all))\n"
            "(define answer\n"
            "  (guard (e ((log-error? e) (list 'log-error (log-error-kind e)))\n"
            "            (#t (list 'raised)))\n"
            "    (log-publish! \"" d "\" \"" M "\" " (number->string segment) " bytes\n"
            "                  (bytevector->hex (sha256 bytes)))))\n"
            "(define listed\n"
            "  (let ((m (guard (e (#t 'unreadable)) (read-manifest \"" d "\" \"" M "\"))))\n"
            "    (cond ((eq? m 'unreadable) 'unreadable)\n"
            "          ((not m) 'no-manifest)\n"
            "          ((assv " (number->string segment) " m) 'listed)\n"
            "          (else 'not-listed))))\n"
            "(printf \"~s\\n\" (list (car answer)\n"
            "                      (if (file-exists? \"" d "/writers/" M "/"
                                        (segment-file-name segment) "\") 'present 'absent)\n"
            "                      listed))\n"))))

(define child-err (string-append scratch "/child.err"))

(define (child-says fault segment)
  (put! cand bytes)
  (write-child! segment)
  (system (string-append
            (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
            "scheme --script " child " > " child-out " 2> " child-err))
  (let ((text (let ((b (slurp child-out))) (if b (utf8->string b) ""))))
    (if (= 0 (string-length text))
        (list 'child-failed fault)
        (guard (e (#t (list 'unreadable text)))
          (read (open-string-input-port text))))))

;; A REPLAY: the store already holds the segment, published and listed,
;; and the child publishes exactly the same bytes again.
(define (replay-says fault)
  (build!)
  (first-publish!)
  (child-says fault 1))

;; A FIRST EXECUTION: nothing is published yet and the child does it.
(define (execution-says fault)
  (build!)
  (child-says fault 1))

(printf "== Q10: what the table names ==\n")
(build!)
(first-publish!)
;; The kinds, not the paths: the paths carry a pid and a scratch root,
;; and asserting those would be asserting the fixture's own arithmetic.
(want "eight artefacts, in the order recovery depends on them"
      (map car (barrier-artefacts d M 1))
      '(log-file writer-directory manifest owner retired instance uncertain registry))
;; AND THE PATHS ARE THE ONES THOSE NAMES MEAN. A table of the right
;; length whose `manifest` row pointed at the segment would flush the
;; right number of things and promise nothing about the manifest.
(want "and each row is the file that name means"
      (map (lambda (e)
             (let ((p (cdr e)))
               (substring p (string-length scratch) (string-length p))))
           (barrier-artefacts d M 1))
      (list (string-append "/store/writers/" M "/000001.sexp")
            (string-append "/store/writers/" M)
            (string-append "/store/writers/" M "/published.sexp")
            (string-append "/store/writers/" W "/owner.sexp")
            (string-append "/store/writers/" W "/retired.sexp")
            "/store/instance.sexp"
            (string-append "/store/writers/" M "/uncertain.sexp")
            "/home/instances.sexp"))
;; A ROW THAT IS NOT THERE IS NOT A ROW. flush-file! is silent about a
;; path that does not exist, so a table listing absent files would hand
;; a case eight rows of which three can never fail.
(want "an artefact that does not exist is not listed"
      (begin
        (delete-file (string-append d "/writers/" M "/uncertain.sexp"))
        (delete-file (string-append d "/writers/" W "/retired.sexp"))
        (map car (barrier-artefacts d M 1)))
      '(log-file writer-directory manifest owner instance registry))
;; TWIN: and the rows the answer depends on are not optional in the same
;; way. Each of the three is a premise of every one of the four durable
;; answers -- the segment, the entry naming it, and the manifest listing
;; it -- so with any of them gone the barrier has nothing to promise and
;; says so, rather than flushing what is left and returning.
(define (barrier-without path)
  (let ((saved (slurp path)))
    (delete-file path)
    (let ((r (guard (e (#t 'refused)) (begin (run-barrier! d M 1 'publish 'publish) 'ran))))
      (put! path saved)
      r)))
;; WHICH ROWS ARE REQUIRED DEPENDS ON WHAT THE ANSWER CLAIMS. A publish
;; answer says the segment is listed in the manifest; a commit answer
;; says a record is in this writer's own directory, where nothing lists
;; anything -- a local writer has no manifest at all, and requiring one
;; would refuse every write the store makes on its own behalf.
(want "the required rows are the ones the claim asserts"
      (list (barrier-required 'publish) (barrier-required 'commit))
      '((log-file writer-directory manifest) (log-file writer-directory)))
(want "and a claim the table does not name is refused"
      (guard (e (#t 'refused)) (barrier-required 'whatever))
      'refused)
(want "and each of them stops the barrier when it is missing"
      (list (barrier-without (string-append d "/writers/" M "/000001.sexp"))
            (barrier-without (string-append d "/writers/" M "/published.sexp")))
      '(refused refused))
;; TWIN: an optional row missing is not a refusal, or the rule above
;; would be "the barrier refuses whenever anything is absent".
(want "TWIN: an optional row missing runs the barrier as usual"
      (barrier-without (string-append d "/instance.sexp"))
      'ran)

(printf "\n== Q10: every ancestor directory the names hang from ==\n")
;; `directory-entry-durable!` flushes a file's IMMEDIATE parent, so the
;; table taken literally never mentions `writers/` -- the directory whose
;; entry gives `writers/<w>` its name. That entry is flushed once, by the
;; call that creates the writer directory, and only by the call that
;; CREATES it: a first publish whose mkdir succeeded and whose flush
;; failed leaves the name not durable and every retry skipping the flush
;; because the directory is already there. Nothing else would repair it,
;; so the barrier's directory row makes its own entry durable too.
;;
;; This is read from the trace rather than by arming a fault, because no
;; substring of `<store>/writers` fails to be a substring of
;; `<store>/writers/<w>` -- a fault armed at the parent would fire on the
;; child and the row would pass without the parent ever being touched.
(define (fsync-subjects)
  (let loop ((i 0) (start 0) (out '()) (text (let ((b (slurp child-err)))
                                               (if b (utf8->string b) ""))))
    (cond
      ((>= i (string-length text)) (reverse out))
      ((char=? (string-ref text i) #\newline)
       (let* ((line (substring text start i))
              (datum (guard (e (#t #f)) (read (open-string-input-port line)))))
         (loop (+ i 1) (+ i 1)
               ;; THE SUBJECT IS WRITTEN, NOT PRINTED AS A STRING, so a
               ;; path comes back from `read` as a symbol and a test
               ;; asking `string?` here would collect nothing at all --
               ;; which reads exactly like a run in which nothing was
               ;; flushed.
               (if (and (pair? datum) (eq? (car datum) 'trace)
                        (eq? (cadr datum) 'fsync) (symbol? (caddr datum)))
                   (cons (symbol->string (caddr datum)) out)
                   out)
               text)))
      (else (loop (+ i 1) start out text)))))

(build!)
(first-publish!)
(put! cand bytes)
(write-child! 1)
(system (string-append "THEOURGIA_TRACE=1 scheme --script " child
                       " > " child-out " 2> " child-err))
(want "the replay answered, so what follows is the barrier's own trace"
      (guard (e (#t 'unreadable))
        (read (open-string-input-port (let ((b (slurp child-out)))
                                        (if b (utf8->string b) "")))))
      '(idempotent present listed))
(want "and it flushed the writer directory, its parent, and the store"
      (let ((subjects (fsync-subjects)))
        (map (lambda (p) (and (member p subjects) #t))
             (list (string-append d "/writers/" M)
                   (string-append d "/writers")
                   d
                   home
                   (string-append d "/writers/" W))))
      '(#t #t #t #t #t))

(printf "\n== Q10: a first execution passes the barrier ==\n")
(want "CONTROL: with nothing armed the candidate is published and listed"
      (execution-says #f)
      '(published present listed))

(printf "\n== Q10': the replay passes the same barrier ==\n")
;; The control has to come first and has to be exact: every row below
;; reads "not published", and "not published" is also what a broken
;; fixture says.
(want "CONTROL: republishing the same bytes answers idempotent"
      (replay-says #f)
      '(idempotent present listed))
;; ONE ROW PER ARTEFACT. The replay writes nothing, so each of these
;; says: the barrier reached this file, and its failure escaped rather
;; than being folded into an answer. `raised` and not `(child-failed
;; ...)` is half the assertion -- a child that never started would
;; otherwise pass every row here.
(want "the segment's own file"
      (replay-says "fsync-fail@publish:file=000001.sexp")
      '(raised present listed))
(want "the directory entry naming it"
      (replay-says (string-append "fsync-fail@publish:dir=writers/" M))
      '(raised present listed))
(want "the manifest that admits it to the history"
      (replay-says "fsync-fail@publish:file=published.sexp")
      '(raised present listed))
(want "the owner record of the generation on the path to it"
      (replay-says "fsync-fail@publish:file=owner.sexp")
      '(raised present listed))
(want "the retirement record"
      (replay-says "fsync-fail@publish:file=retired.sexp")
      '(raised present listed))
(want "the store's instance identity"
      (replay-says "fsync-fail@publish:file=instance.sexp")
      '(raised present listed))
(want "the uncertain-interval cache"
      (replay-says "fsync-fail@publish:file=uncertain.sexp")
      '(raised present listed))
(want "the machine registry"
      (replay-says "fsync-fail@publish:file=instances.sexp")
      '(raised present listed))

(printf "\n== Q10': a fault outside the barrier's stage is not the barrier ==\n")
;; THE STAGE IS PART OF THE CLAIM. If the rows above passed with any
;; stage at all they would be measuring "an fsync happens somewhere in
;; this process", which is true of a great many programs. A fault armed
;; at the same file under a stage the publish never enters must leave
;; the replay answering.
(want "the manifest, armed at a stage publish does not enter"
      (replay-says "fsync-fail@snapshot:file=published.sexp")
      '(idempotent present listed))
(want "and a file the barrier does not name"
      (replay-says "fsync-fail@publish:file=meta.sexp")
      '(idempotent present listed))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q3 complete\n")
