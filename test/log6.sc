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

;; The rewritten reader path: one validated-prefix discovery.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace) (theourgia crc32)
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

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
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
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define d (test-dir "log6work"))
(define A "k3m9x2qa") (define B "c9xq01mz")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
    (if (eof-object? b) (make-bytevector 0) b)))
(define (hash-of path)
  (bytevector->hex (sha256 (call-with-port (open-file-input-port path) get-bytevector-all))))
(define (build!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" A " " d "/writers/" B " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (put! (string-append d "/writers/" A "/000001.sexp")
        (cat (rec 1 '() '(put "a.1" ())) (rec 2 '() '(put "a.2" ()))))
  (put! (string-append d "/writers/" B "/000001.sexp") (rec 1 '() '(put "b.1" ())))
  (put! (string-append d "/writers/" B "/published.sexp")
        (string->utf8 (string-append "(" (manifest-entry-text
                                           1
                                           (hash-of (string-append d "/writers/" B "/000001.sexp"))
                                           (slurp (string-append d "/writers/" B "/000001.sexp")))
                                     ")\n"))))

(printf "== one discovery answers the question ==\n")
(build!)
(define ls (log-open d))
(define pa (load-prefix ls A))
(define pb (load-prefix ls B))
(want "A is local, B is mirrored"
      (list (discovery-origin pa) (discovery-origin pb)) '(local mirrored))
(want "A's validated extent ends at seq 2" (discovery-end-seq pa) 2)
(want "B's at 1" (discovery-end-seq pb) 1)
(want "no integrity anywhere" (load-integrity ls) '())
(want "segment ranges are recorded" (discovery-segment-ranges pa) '((1 1 2)))

(printf "== L10: the extent and the append target are different facts ==\n")
(put! (string-append d "/writers/" A "/000002.sexp") (make-bytevector 0))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the extent still ends at 2 despite an empty rotated segment"
        (discovery-end-seq p) 2)
  (want "and the append target is the new empty segment"
        (car (discovery-physical-current p)) 2)
  (load-commit! ls))

(printf "== physical-current says 'no append target' where there is none ==\n")
(let* ((ls (log-open d)) (p (load-prefix ls B)))
  (want "a mirrored writer has no append target"
        (discovery-physical-current p) 'no-append-target)
  (load-commit! ls))

(printf "== an interrupted first publication is ignored entirely ==\n")
(build!)
(system (string-append "mkdir -p " d "/writers/zzzzzzzz"))
(put! (string-append d "/writers/zzzzzzzz/000001.sexp") (rec 1 '() '(put "z.1" ())))
(let* ((ls (log-open d)) (p (load-prefix ls "zzzzzzzz")))
  (want "classified as an interrupted publication"
        (discovery-origin p) 'incomplete-publication)
  (want "and it contributes no extent" (discovery-end-seq p) 0)
  (load-commit! ls))

(printf "== a malformed retirement marker is an integrity error, not 'never retired' ==\n")
(build!)
(put! (string-append d "/writers/" A "/retired.sexp") (string->utf8 "(oops"))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the marker is reported"
        (map log-error-kind (discovery-integrity p)) '(retired-malformed))
  (want "and no history is exposed past it" (discovery-end-seq p) 0)
  (load-commit! ls))

(printf "== a retirement offset past the end of its file is reported ==\n")
(build!)
(put! (string-append d "/writers/" A "/retired.sexp")
      (string->utf8 "((format 1) (tx \"t1\") (prefix 1 999999 100))\n"))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "reported rather than silently clipped"
        (map log-error-kind (discovery-integrity p)) '(retired-beyond-file))
  (load-commit! ls))

(printf "== a swapped published segment is caught by the hash ==\n")
(build!)
(put! (string-append d "/writers/" B "/000001.sexp") (rec 1 '() '(put "SWAPPED" ())))
(let* ((ls (log-open d)) (p (load-prefix ls B)))
  (want "manifest-hash, and nothing of B is in the extent"
        (list (map log-error-kind (discovery-integrity p)) (discovery-end-seq p))
        '((manifest-hash) 0))
  (want "CONTROL: A is untouched by B's damage"
        (discovery-end-seq (load-prefix ls A)) 2)
  (load-commit! ls))

(printf "== the quarantined suffix leaves the extent, not just snapshot eligibility ==\n")
(build!)
(put! (string-append d "/writers/" A "/quarantine.sexp")
      (string->utf8 "((format 1) (fork 2))\n"))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the extent stops before the fork" (discovery-end-seq p) 1)
  (want "and the fork is reported" (cadr (discovery-quarantine p)) 2)
  (load-commit! ls))

(printf "== the load holds one shared lock and releases it on commit ==\n")
(build!)
(let ((p (open-output-string)))
  (parameterize ((current-error-port p))
    (trace-enable! #t)
    (let ((ls (log-open d))) (load-commit! ls))
    (trace-enable! #f))
  (let ((s (get-output-string p)))
    (define (count-of needle)
      (let ((n (string-length s)) (m (string-length needle)))
        (let loop ((i 0) (k 0))
          (cond ((> (+ i m) n) k)
                ((string=? (substring s i (+ i m)) needle) (loop (+ i 1) (+ k 1)))
                (else (loop (+ i 1) k))))))
    (want "exactly one shared acquisition for the whole load" (count-of "(trace flock") 1)
    (want "and exactly one release" (count-of "(trace unlock") 1)))

(printf "== delivery: from the cut's segment, bounded by the extent ==\n")
(build!)
(put! (string-append d "/writers/" A "/000002.sexp") (cat (rec 3 '() '(put "a.3" ()))))
(define (deliver-with cut)
  (let* ((ls (log-open d)) (seen '()))
    (let ((r (load-deliver! ls cut
               (lambda (w seg off seq ts actor deps payload)
                 (set! seen (cons (list w seq) seen))))))
      (when (eq? r 'delivered) (load-commit! ls))
      (list r (reverse seen)))))
(want "with no cut, everything in the extent arrives"
      (deliver-with '())
      (list 'delivered (list (list B 1) (list A 1) (list A 2) (list A 3))))
(want "a cut at A.2 suppresses only what it covers"
      (deliver-with (list (cons A 2)))
      (list 'delivered (list (list B 1) (list A 3))))
(want "a cut at A.3 leaves A with nothing to deliver"
      (deliver-with (list (cons A 3)))
      (list 'delivered (list (list B 1))))

(printf "== the extent bounds delivery, not a scan-until-failure ==\n")
(build!)
(put! (string-append d "/writers/" A "/quarantine.sexp")
      (string->utf8 "((format 1) (fork 2))\n"))
(want "the quarantined suffix is not delivered even though its bytes are fine"
      (deliver-with '()) (list 'delivered (list (list B 1) (list A 1))))

(printf "== a segment that cannot be flushed is never delivered ==\n")
;; A SEGMENT DISCOVERY READ AND THE BARRIER CANNOT. The bytes were read
;; and cached before the file became unreadable, so delivery could hand
;; them out -- from a cache, with no flush behind them. A power cut would
;; then take records a reader had already promised.
;;
;; THE FAILURE MUST BE LATE, or an implementation that simply stopped at
;; the first unreadable file would pass; and the failing segment must be
;; a SEALED one, because the highest segment is the retained current
;; buffer and is not re-read at all. (The first version of this case made
;; the highest one unreadable and read as "the abort path does not
;; work".)
;;
;; NOTHING IS DELIVERED AT ALL. This used to deliver some records and
;; then fail on the read, reporting `delivery-failed`; the barrier now
;; refuses before the first callback. That is what "delivery implies
;; durability" requires -- a reader may not hand out a record it could
;; not flush -- and it is the stronger of the two: the earlier version
;; had already promised some of them.
(build!)
(put! (string-append d "/writers/" A "/000002.sexp") (cat (rec 3 '() '(put "a.3" ()))))
(put! (string-append d "/writers/" A "/000003.sexp") (cat (rec 4 '() '(put "a.4" ()))))
(let* ((ls (log-open d)) (seen '()))
  ;; unreadable only after discovery has run, and sealed rather than current
  (system (string-append "chmod 000 " d "/writers/" A "/000002.sexp"))
  (let ((r (guard (e (#t 'raised))
             (load-deliver! ls '()
               (lambda (w seg off seq ts actor deps payload)
                 (set! seen (cons (list w seq) seen)))))))
    (system (string-append "chmod 644 " d "/writers/" A "/000002.sexp"))
    (want "the load fails rather than delivering what it cannot flush"
          r 'raised)
    (want "and not one record was handed out"
          (length seen) 0)
    (want "and the load is aborted, not committed"
          (let ((o (load-outcome ls))) (if (pair? o) (car o) o)) 'aborted)))


;; AND THE CASE ONLY THE BARRIER CAN SEE. The row above cannot tell
;; "the barrier refused" from "the read failed": a sealed segment that
;; will not open will not be read either, so both stop the load and both
;; deliver nothing.
;;
;; THE HIGHEST SEGMENT IS DIFFERENT. Discovery keeps its bytes as the
;; retained current buffer, so delivery serves them from memory and never
;; opens the file -- the read SUCCEEDS whatever the mode bits say. Only
;; the barrier opens it. With the open skipped, this store hands out
;; records with no flush behind them and says nothing; a power cut then
;; takes records a reader had promised.
;;
;; (The first version of the row above made this segment unreadable and
;; read as "the abort path does not work". It was the right stimulus for
;; the wrong question.)
;;
;; THE OUTCOME IS TAGGED. `'raised` as a bare answer cannot be told from
;; a delivery that happened to return the symbol `raised`, and it says
;; nothing about WHAT was raised -- any unrelated failure inside the
;; guarded expression satisfies it just as well. `(returned <v>)` and
;; `(raised <who>)` are two shapes that cannot be confused, and the
;; second carries the condition's `who` so the row can say the failure
;; came from the durability path rather than from anywhere.
(define (delivery-outcome damage)
  (let* ((ls (log-open d))
         (seen '()))
    (damage)
    (guard (e (#t (list 'raised
                        (cond ((unreadable-entry? e) 'unreadable-entry)
                              ((and (condition? e) (who-condition? e)) (condition-who e))
                              ((and (vector? e) (> (vector-length e) 0)) (vector-ref e 0))
                              (else 'unknown))
                        (reverse seen))))
      (let ((r (load-deliver! ls '()
                 (lambda (w seg off seq ts actor deps payload)
                   (set! seen (cons (list w seq) seen))))))
        (list 'returned (if (pair? r) (car r) r) (reverse seen))))))
;; THE STORE THIS ROW USES, built once and described here so the two
;; damaged rows and the healthy twin are all talking about the same
;; three records: A holds a.1 and a.2 in segment 1 and a.3 in segment 2,
;; and B holds b.1.
(define (build-with-second-segment!)
  (build!)
  (put! (string-append d "/writers/" A "/000002.sexp")
        (cat (rec 3 '() '(put "a.3" ())))))
;; TWIN FIRST, so the expectation the damaged rows are measured against
;; is a reading and not an assumption: undamaged, this store delivers
;; these exact records. An earlier version asked only for
;; `(> (length seen) 0)` while its label said "every record" -- which is
;; passed by a store that delivers one of the four.
(build-with-second-segment!)
(define healthy-delivery (delivery-outcome (lambda () (if #f #f))))
(want "TWIN: undamaged, the store delivers these exact records"
      healthy-delivery
      (list 'returned 'delivered
            (list (list B 1) (list A 1) (list A 2) (list A 3))))
;; THE SEGMENT DELIVERY READS FROM CACHE IS STILL FLUSHED, OR REFUSED --
;; and the row is asked TWICE, under two different reasons for the open
;; to fail. One error class is one branch: a barrier that propagated
;; permission errors and quietly skipped everything else would pass a
;; single-class row while leaving the defect exactly where it was.
;; THE OPEN'S FAILURE IS unreadable-entry SINCE F100a (D1): an open without
;; create is a non-mutating primitive. The delivery stops as before; these
;; rows, the errno rows and both witnesses below read that class, and an
;; errno by the name ffi gives it where it knows one (the number otherwise).
(build-with-second-segment!)
(want "a cached segment whose file cannot be opened stops the delivery (permission)"
      (delivery-outcome
        (lambda () (system (string-append "chmod 000 " d "/writers/" A "/000002.sexp"))))
      (list 'raised 'unreadable-entry '()))
(system (string-append "chmod 644 " d "/writers/" A "/000002.sexp"))
;; THE SECOND CLASS IS FREE AND IT IS A DIFFERENT ONE: the file is gone
;; rather than forbidden. Discovery has already read it, so delivery
;; would still serve every record from the retained buffer -- which is
;; what makes this the same question as the row above and not a test
;; that the reader notices a missing file.
(build-with-second-segment!)
(want "a cached segment whose file has been removed stops the delivery (absent)"
      (delivery-outcome
        (lambda () (system (string-append "rm -f " d "/writers/" A "/000002.sexp"))))
      (list 'raised 'unreadable-entry '()))

;; AND THE CLASS OF FAILURE IS NOT ALLOWED TO MATTER. The two rows above
;; reach the barrier's open by removing the file and by forbidding it --
;; ENOENT and EACCES. Both are things a fixture can arrange with rm and
;; chmod, and that is exactly their limitation: an implementation that
;; propagated those two and quietly skipped anything else would pass both
;; rows while leaving the defect where it was. Descriptor exhaustion is
;; the obvious third class, it is the one a busy process actually meets,
;; and no amount of rm and chmod produces it.
;;
;; SO THE ERRNO IS INJECTED. `open-fail@<stage>:file=<sub>:errno=<name>`
;; fails one open, in one stage, on one file, with a named errno, raising
;; the same condition a real failure raises. The rows then ask the
;; question the fixture could not: does this barrier refuse for a reason
;; it has never seen before?
;;
;; A CHILD PROCESS, because THEOURGIA_INJECT is an expansion-time gate
;; and a fault armed in this file would apply to every open in it.
(define child6 (string-append d "/child.sc"))
(define child6-out (string-append d "/child.out"))
(define (write-child6!)
  (put! child6
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(define seen 0)\n"
            "(define res\n"
            "  (guard (e (#t (list 'raised (cond ((fs-error? e) (fs-error-errno e))\n"
            "                                     ((unreadable-entry? e) (unreadable-entry-errno e))\n"
            "                                     (else 'other)))))\n"
            "    (let* ((ls (log-open \"" d "\"))\n"
            "           (r (load-deliver! ls '()\n"
            "                (lambda (w seg off seq ts actor deps payload)\n"
            "                  (set! seen (+ seen 1))))))\n"
            "      (list 'returned (if (pair? r) (car r) r)))))\n"
            "(printf \"~s ~s ~s\\n\" (car res) (cadr res) seen)\n"))))
;; THE UNARMED RUN CLEARS THE VARIABLES RATHER THAN OMITTING THEM. This
;; fixture inherits whatever environment it was started in, and the suite
;; runner exports the injection switch -- so a control that merely
;; declines to SET the fault would run under whatever the parent had, and
;; a control that is quietly armed reads as a product that ignored the
;; fault.
;;
;; AND THE CHILD'S EXIT STATUS IS PART OF THE READING. A child that died
;; before printing leaves an empty file, and an empty file parsed for
;; three datums gives the same `unreadable` answer as a child that
;; printed nonsense. The status tells them apart.
(define (child6-says fault)
  (build-with-second-segment!)
  (write-child6!)
  (let* ((prefix (if fault
                     (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ")
                     "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "))
         (rc (system (string-append prefix "scheme --script " child6
                                    " > " child6-out " 2>/dev/null")))
         (text (let ((b (slurp child6-out)))
                 (if (bytevector? b) (utf8->string b) (if (string? b) b "")))))
    (if (not (eqv? rc 0))
        (list 'child-failed rc text)
        (guard (e (#t (list 'unreadable text)))
          (let ((p (open-string-input-port text)))
            (let* ((a (read p)) (b (read p)) (c (read p)))
              ;; AND NOTHING AFTER THE THREE. Trailing output would mean
              ;; the child ran further than this row believes it did.
              (if (eof-object? (read p))
                  (list a b c)
                  (list 'trailing-output text))))))))
;; THE UNARMED TWIN FIRST. It is what says the child runs at all -- an
;; armed row reading `raised` is also what a child that cannot start
;; produces, and the two are indistinguishable from the parent.
(want "CONTROL: with nothing armed the child delivers every record"
      (child6-says #f)
      (list 'returned 'delivered 4))
;; THE CLASSES, ONE ROW EACH. Same aim, same stage, same file -- only the
;; reason differs, so a barrier that answered differently for one of them
;; would be saying that some failures to make a promise are acceptable.
;;
;; THREE NAMES ARE NOT THE SAME CLAIM AS "ANY REASON". A list of names is
;; a list, and an implementation that propagated exactly the names on it
;; and skipped everything else would pass every row here while leaving
;; the property untested by construction. That is why the qualifier also
;; takes a NUMBER: the last two rows name classes nothing in this suite
;; has any other way to reach -- the system-wide descriptor limit, and a
;; device error -- and they are here to make the list stop being the
;; thing under test.
(for-each
  (lambda (case)
    (let ((name (car case)) (code (cdr case)))
      (want (string-append "an open that fails with " name
                           " refuses the delivery, it does not skip it")
            (child6-says (string-append "open-fail@deliver-barrier:file=000002.sexp:errno=" name))
            (list 'raised code 0))))
  (list (cons "EMFILE" 'EMFILE)
        (cons "EACCES" 'EACCES)
        (cons "ENOENT" 'ENOENT)
        (cons "23" 23)
        (cons "5" 'EIO)))
;; AND THE SEAM ITSELF FIRES. An injection point that is never reached
;; reads, in every one of the rows above, exactly like a product that
;; refuses correctly -- both give `raised`. So the witness runs the same
;; child against an aim that CANNOT match: the same stage and errno, a
;; file substring no path contains. If the seam were inert this row would
;; read the same as the three above; it must read like the control.
(want "WITNESS: aimed at a file that does not exist in this store, nothing is injected"
      (child6-says "open-fail@deliver-barrier:file=zzzzzz.sexp:errno=EMFILE")
      (list 'returned 'delivered 4))
;; AND THE STAGE QUALIFIER IS LOAD-BEARING TOO: the same aim in a stage
;; this path never enters injects nothing.
(want "WITNESS: the same aim in another stage injects nothing"
      (child6-says "open-fail@snapshot:file=000002.sexp:errno=EMFILE")
      (list 'returned 'delivered 4))
;; AND THE CONDITION SAYS WHICH OPERATION AND WHICH FILE. Every row above
;; reads the errno and the callback count, so all of them are satisfied by
;; a failure that carries the right number and the wrong subject -- a
;; fault that fired on some other file's open in the same stage, say,
;; which is precisely the mistake a path qualifier exists to prevent and
;; the one an armed run is least able to notice. Asking for the operation
;; and the target makes the aim part of the reading.
(define aimed (string-append d "/aimed.sc"))
(define aimed-out (string-append d "/aimed.out"))
(build-with-second-segment!)
(put! aimed
      (string->utf8
        (string-append
          "#!chezscheme\n(import (chezscheme) (theourgia ffi))\n"
          "(define p \"" d "/writers/" A "/000002.sexp\")\n"
          "(define (try-full)\n"
          "  (guard (e (#t (cond ((fs-error? e)\n"
          "                       (list 'raised (fs-error-op e) (fs-error-target e) (fs-error-errno e)))\n"
          "                      ((unreadable-entry? e)\n"
          "                       (list 'raised 'unreadable (unreadable-entry-path e) (unreadable-entry-errno e)))\n"
          "                      (else (list 'raised 'not-an-fs-error)))))\n"
          "    (parameterize ((theourgia-stage 'deliver-barrier))\n"
          "      (let ((fd (fd-open p '(read)))) (fd-close fd) 'opened))))\n"
          "(printf \"~s\\n\" (try-full))\n")))
(want "WITNESS: the injected condition names the open, the file aimed at, and the errno"
      (begin
        (system (string-append "THEOURGIA_INJECT=on "
                               "THEOURGIA_FAULT=open-fail@deliver-barrier:file=000002.sexp:errno=EMFILE "
                               "scheme --script " aimed " > " aimed-out " 2>/dev/null"))
        (let ((text (let ((b (slurp aimed-out)))
                      (if (bytevector? b) (utf8->string b) (if (string? b) b "")))))
          (guard (e (#t (list 'unreadable text)))
            (read (open-string-input-port text)))))
      (list 'raised 'unreadable (string-append d "/writers/" A "/000002.sexp") 'EMFILE))

;; AND THE INJECTION IS SPENT WHEN IT FIRES. Every row above stops at the
;; first exception, so none of them can tell a one-shot fault from one
;; that fires on every matching open -- and a persistent one would take
;; out whatever the process opened next, including a fixture's own read
;; of the result. The witness opens the same file TWICE in one armed
;; process: the first open must fail with the errno asked for, and the
;; second must succeed.
(define oneshot (string-append d "/oneshot.sc"))
(define oneshot-out (string-append d "/oneshot.out"))
(build-with-second-segment!)
(put! oneshot
      (string->utf8
        (string-append
          "#!chezscheme\n(import (chezscheme) (theourgia ffi))\n"
          "(define p \"" d "/writers/" A "/000002.sexp\")\n"
          "(define (try)\n"
          "  (guard (e (#t (list 'raised (cond ((fs-error? e) (fs-error-errno e))\n"
          "                                     ((unreadable-entry? e) (unreadable-entry-errno e))\n"
          "                                     (else 'other)))))\n"
          "    (parameterize ((theourgia-stage 'deliver-barrier))\n"
          "      (let ((fd (fd-open p '(read)))) (fd-close fd) 'opened))))\n"
          ;; SEQUENCED WITH let*, NOT PASSED AS TWO ARGUMENTS. Chez
          ;; evaluates arguments right to left, so `(printf "~s ~s" (try)
          ;; (try))` runs the SECOND call first -- and this row would
          ;; then read the spent fault as the first open and the fired
          ;; one as the second, which is exactly the picture it exists to
          ;; rule out.
          "(let* ((first (try)) (second (try)))\n"
          "  (printf \"~s ~s\\n\" first second))\n")))
(want "WITNESS: an armed open-fault fires once and is then spent"
      (begin
        (system (string-append "THEOURGIA_INJECT=on "
                               "THEOURGIA_FAULT=open-fail@deliver-barrier:file=000002.sexp:errno=EMFILE "
                               "scheme --script " oneshot " > " oneshot-out " 2>/dev/null"))
        (let ((text (let ((b (slurp oneshot-out)))
                      (if (bytevector? b) (utf8->string b) (if (string? b) b "")))))
          (guard (e (#t (list 'unreadable text)))
            (let ((p (open-string-input-port text))) (list (read p) (read p))))))
      (list (list 'raised 'EMFILE) 'opened))

(printf "== L4: a reader changes nothing ==\n")
(build!)
(define (listing)
  (let loop ((dirs (list d)) (acc '()))
    (if (null? dirs) (list-sort string<? acc)
        (let* ((cur (car dirs)) (es (directory-list cur)))
          (loop (append (cdr dirs)
                        (map (lambda (e) (string-append cur "/" e))
                             (filter (lambda (e) (file-directory? (string-append cur "/" e))) es)))
                (append acc (map (lambda (e)
                                   (let ((p (string-append cur "/" e)))
                                     (if (file-directory? p) (string-append p "/")
                                         (string-append p ":"
                                           (crc32-hex (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
                                                        (if (eof-object? b) (make-bytevector 0) b)))))))
                                 es)))))))
(define before (listing))
(let ((ls (log-open d)))
  (load-deliver! ls '() (lambda args (if #f #f)))
  (load-commit! ls))
(want "the recursive listing and every hash are identical"
      (if (equal? (listing) before) (list 'identical (length before)) (listing))
      (list 'identical (length before)))

(printf "== L4'(a): an unlisted segment file is ignored ==\n")
(build!)
(put! (string-append d "/writers/" B "/000002.sexp") (rec 2 '() '(put "ghost" ())))
(want "the ghost is not in B's extent"
      (discovery-end-seq (load-prefix (log-open d) B)) 1)
(want "CONTROL: listing it makes it count"
      (begin (put! (string-append d "/writers/" B "/published.sexp")
                   (string->utf8
                     (string-append "("
                       (manifest-entry-text 1 (hash-of (string-append d "/writers/" B "/000001.sexp"))
                                            (slurp (string-append d "/writers/" B "/000001.sexp")))
                       " "
                       (manifest-entry-text 2 (hash-of (string-append d "/writers/" B "/000002.sexp"))
                                            (slurp (string-append d "/writers/" B "/000002.sexp")))
                       ")\n")))
             (discovery-end-seq (load-prefix (log-open d) B)))
      2)

(printf "== L1 through the reader: a torn tail on the local current segment ==\n")
(build!)
(let* ((p (string-append d "/writers/" A "/000001.sexp"))
       (whole (call-with-port (open-file-input-port p) get-bytevector-all))
       (cut (make-bytevector (- (bytevector-length whole) 9))))
  (bytevector-copy! whole 0 cut 0 (bytevector-length cut))
  (put! p cut))
(let* ((ls (log-open d)) (pa (load-prefix ls A)))
  (want "the first record survives and the residual is a torn tail"
        (list (discovery-end-seq pa) (car (discovery-torn pa))) '(1 1))
  (want "and it is not an integrity error" (discovery-integrity pa) '())
  (load-commit! ls))


(printf "== L7: snapshot selection inside the load ==\n")
(define (snap! n cut rows) (snapshot-write! (string-append d "/snap/" (segment-file-name n)) cut rows))
(define (adopted)
  (let ((ls (log-open d)))
    (let ((r (list (load-snapshot-cut ls) (load-snapshot-rows ls) (load-snapshot-reason ls))))
      (load-commit! ls) r)))
(build!)
(snap! 1 (list (cons A 2)) '((block "x" ())))
(want "a supported snapshot is adopted" (adopted) (list (list (cons A 2)) '((block "x" ())) #f))
;; THE REJECTED SNAPSHOT MUST BE THE ONLY ONE, or the row cannot tell
;; "rejected" from "fell back to the older one" -- both leave a valid
;; adoption behind. The fallback gets its own row below.
(want "a cut beyond the log, as the only snapshot, is refused with its reason"
      (begin (system (string-append "rm -f " d "/snap/000001.sexp"))
             (snap! 2 (list (cons A 99)) '((block "ghost" ())))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'unsupported-cut))
(want "CONTROL: with a supported older one present, it falls back to that"
      (begin (snap! 1 (list (cons A 2)) '((block "x" ()))) (car (adopted)))
      (list (cons A 2)))
(want "a void frame falls back to the older valid one"
      (begin (put! (string-append d "/snap/000003.sexp")
                   (string->utf8 "(snapshot 1 ((\"k3m9x2qa\" . 2)))\n"))
             (car (adopted)))
      (list (cons A 2)))
(want "CONTROL: the void one alone leaves nothing adopted"
      (begin (system (string-append "rm -f " d "/snap/000001.sexp " d "/snap/000002.sexp"))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'no-end))
(want "a cut naming one writer twice is malformed, not first-entry-wins"
      (begin (system (string-append "rm -f " d "/snap/000003.sexp"))
             (snap! 4 (list (cons A 1) (cons A 2)) '((block "d" ())))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'duplicate-writer))
(want "a cut reaching into quarantined history is refused"
      (begin (system (string-append "rm -f " d "/snap/000004.sexp"))
             (system (string-append "rm -f " d "/snap/000003.sexp " d "/snap/000004.sexp"))
             (snap! 5 (list (cons A 2)) '((block "q" ())))
             (put! (string-append d "/writers/" A "/quarantine.sexp")
                   (string->utf8 "((format 1) (fork 2))\n"))
             (list (car (adopted)) (caddr (adopted))))
      (list #f 'quarantined))

(printf "== the adopted cut is what delivery honours ==\n")
(build!)
(snap! 1 (list (cons A 1)) '((block "x" ())))
(let* ((ls (log-open d)) (seen '()))
  (load-deliver! ls (load-snapshot-cut ls)
    (lambda (w seg off seq ts actor deps payload) (set! seen (cons (list w seq) seen))))
  (load-commit! ls)
  (want "A.1 is covered by the snapshot and not re-delivered"
        (reverse seen) (list (list B 1) (list A 2))))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log6 complete\n")
