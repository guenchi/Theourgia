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

;; log-publish! under a fault at each durability point.
;;
;; The rows in log18 ask what publish DECIDES. These ask what survives
;; when a step of the installation does not complete, and each one names
;; a single fsync: the staged candidate, the evidence copy taken before a
;; replacement, the directory entry that makes an installed name durable,
;; and the manifest. One blanket "an fsync happened somewhere" would pass
;; with any of the four missing, and each of the four protects a
;; different thing -- so each gets a row, armed at that file alone.
;;
;; EACH ROW IS A CHILD PROCESS. `THEOURGIA_INJECT` is read when the
;; library is expanded, so a fault cannot be armed and disarmed inside
;; one run; the fixture writes a small program, runs it with the fault in
;; its environment, and reads back what it printed. A child that could
;; not start prints nothing, and that is reported as `(child-failed ...)`
;; rather than crashing a row that then reads like a failed assertion.
;;
;; AND EVERY ARMED ROW CARRIES ITS UNARMED TWIN, because a row that
;; refuses under a fault proves nothing unless the same store, the same
;; candidate and the same code publish it when nothing is armed.

(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (only (igropyr crypto) sha256 bytevector->hex))

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
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))


(define scratch (test-dir "log19"))
(define W "wwwlocl0")
(define M "mirrorz9")
(define child (string-append scratch "/child.ss"))
(define child-out (string-append scratch "/child.out"))

(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (recs lo hi)
  (apply cat (let loop ((i lo))
               (if (> i hi) '() (cons (rec i (list 'put (list (cons 'kind 'section)))) (loop (+ i 1)))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (sha-of b) (bytevector->hex (sha256 b)))
(define d (string-append scratch "/store"))
(define home (string-append scratch "/home"))
(define (mpath n) (string-append d "/writers/" M "/" (segment-file-name n)))
(define (build!)
  (system (string-append "rm -rf " d " " home "; mkdir -p "
                         d "/writers/" W " " d "/writers/" M " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"q1\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" home)
  (let ((k (instance-install! d))) (owner-install! d W k))
  (put! (string-append d "/writers/" W "/" (segment-file-name 1)) (make-bytevector 0)))

;; THE CHILD PUBLISHES ONE CANDIDATE AND SAYS WHAT HAPPENED TO IT: the
;; answer or the condition, then whether the segment file is there and
;; whether the manifest lists it. Those three are the whole reading, and
;; they are printed as one datum so a partial line cannot be read as a
;; whole one.
(define (write-child! segment bytes)
  (put! child
        (string->utf8
          (string-append
            "#!chezscheme\n"
            "(import (chezscheme) (theourgia log) (theourgia ffi)\n"
            "        (only (igropyr crypto) sha256 bytevector->hex))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(define bytes (call-with-port (open-file-input-port \"" scratch "/cand.bin\")\n"
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
            "(printf \"~s\\n\" (list (if (pair? answer) (car answer) answer)\n"
            "                      (if (file-exists? \"" (mpath segment) "\") 'present 'absent)\n"
            "                      listed))\n"))))

(define (child-says fault segment bytes)
  (put! (string-append scratch "/cand.bin") bytes)
  (write-child! segment bytes)
  (system (string-append
            (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
            "scheme --script " child " > " child-out " 2>/dev/null"))
  ;; A CHILD THAT NEVER RAN IS A READING, NOT AN EXCEPTION.
  (let ((text (let ((b (slurp child-out))) (if b (utf8->string b) ""))))
    (if (= 0 (string-length text))
        (list 'child-failed fault)
        (guard (e (#t (list 'unreadable text)))
          (read (open-string-input-port text))))))

(printf "== Q1: the staged candidate ==\n")
;; The candidate is written to a temp name and flushed before anything is
;; linked into place. If that flush fails nothing may appear at the
;; segment's name at all: a linked file whose bytes are not durable is a
;; published segment that a power cut can empty.
(want "CONTROL: with nothing armed the candidate is published"
      (begin (build!) (child-says #f 1 (recs 1 3)))
      '(published present listed))
(want "the staged candidate's flush fails, and nothing takes the segment's name"
      (begin (build!) (child-says "fsync-fail@publish:file=publish." 1 (recs 1 3)))
      '(raised absent no-manifest))

(printf "\n== Q2: the manifest ==\n")
;; The bytes are linked into place first and listed second, so a fault
;; here leaves a segment present and unlisted -- which is precisely the
;; state the "carried through to the manifest" row in log18 answers. The
;; two rows are the same sequence seen from either side.
(want "the manifest's write fails, leaving the segment present and unlisted"
      (begin (build!) (child-says "fsync-fail@publish:file=published.sexp" 1 (recs 1 3)))
      '(raised present no-manifest))
(want "and a retry with nothing armed carries it through"
      (child-says #f 1 (recs 1 3))
      '(published present listed))

(printf "\n== Q3: the evidence taken before a replacement ==\n")
;; A repair replaces bytes that are already here, so the originals are
;; copied aside first. Until that copy is durable -- its bytes AND its
;; name -- nothing may be replaced, because the only other copy of the
;; damaged segment is the one about to be overwritten.
(define (damaged-store!)
  (build!)
  (let* ((whole (recs 1 3))
         (o (bytevector-copy whole)))
    (bytevector-u8-set! o 80 (if (= 97 (bytevector-u8-ref o 80)) 98 97))
    (put! (mpath 1) o)
    o))
(define damaged-bytes #f)
(want "CONTROL: with nothing armed the damaged segment is repaired"
      (begin (set! damaged-bytes (damaged-store!)) (child-says #f 1 (recs 1 3)))
      '(repaired present listed))
(want "the evidence copy's flush fails, and the damaged original is still here"
      (let ((o (damaged-store!)))
        (let ((answer (child-says "fsync-fail@publish:file=damaged" 1 (recs 1 3))))
          (list answer (equal? (slurp (mpath 1)) o))))
      (list '(raised present no-manifest) #t))

(printf "\n== Q4: the directory entries ==\n")
;; A name is not durable because the file under it is. Each directory
;; that gains an entry -- the writer's own, and the one holding the
;; evidence -- is flushed in its turn, and a fault at either must leave
;; the manifest unwritten.
(want "the writer directory's entry fails to persist, and nothing is listed"
      (begin (build!) (child-says (string-append "fsync-fail@publish:dir=writers/" M) 1 (recs 1 3)))
      '(raised present no-manifest))
(want "CONTROL: the same store and candidate with nothing armed"
      (begin (build!) (child-says #f 1 (recs 1 3)))
      '(published present listed))

(printf "\n== Q5: the candidate that is kept rather than installed ==\n")
;; A REFUSAL STILL WRITES SOMETHING DURABLE. The bytes are kept as
;; evidence under a content-addressed name, and the marker beside them
;; says the bytes passed their own checks -- so the keep has the same
;; obligation as an install, and the same fault must stop it rather than
;; let a refusal be reported over a file that is not there yet.
(want "CONTROL: a candidate that leaves a hole is refused and kept"
      (begin (build!)
             (child-says #f 1 (recs 1 3))
             (child-says #f 5 (recs 20 21)))
      '((segment-layout-conflict (gap-before-candidate (history-ends 3) (candidate-starts 20)))
        absent not-listed))
(want "the kept candidate's flush fails, and the refusal is not reported over it"
      (begin (build!)
             (child-says #f 1 (recs 1 3))
             (child-says "fsync-fail@publish:file=incoming" 5 (recs 20 21)))
      '(raised absent not-listed))

(printf "\n== Q6: the adopter opens for the first time after the publisher is gone ==\n")
;; L18(b). The process that published these bytes has exited; nothing it
;; did in memory survives it, and the reader that arrives next has never
;; opened this store. So the flushing that makes a mirror's records
;; history -- the manifest and the directory that names it -- has to be
;; done by the arriving process, at its own barrier, before it delivers
;; anything that depends on them. A reader that leaned on the publisher
;; having flushed would be right on every machine where the publisher
;; happened to survive, and wrong exactly when it did not.
;;
;; The publisher here really is a separate process that has exited by the
;; time the row reads anything, and the adopter really is opening for the
;; first time: both are fresh children of this fixture.
(define publisher (string-append scratch "/publisher.ss"))
(define adopter (string-append scratch "/adopter.ss"))
(define adopter-out (string-append scratch "/adopter.out"))

(define (write-publisher! segment bytes)
  (put! (string-append scratch "/pub.bin") bytes)
  (put! publisher
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi)\n"
            "        (only (igropyr crypto) sha256 bytevector->hex))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(let ((b (call-with-port (open-file-input-port \"" scratch "/pub.bin\")\n"
            "           get-bytevector-all)))\n"
            "  (log-publish! \"" d "\" \"" M "\" " (number->string segment) " b\n"
            "                (bytevector->hex (sha256 b))))\n"))))

(define (write-adopter!)
  (put! adopter
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(define n 0)\n"
            "(define outcome\n"
            "  (guard (e (#t 'raised))\n"
            "    (let ((s (log-begin \"" d "\"\n"
            "               (lambda (w seg off seq . rest)\n"
            "                 (when (string=? w \"" M "\") (set! n (+ n 1)))\n"
            "                 'applied))))\n"
            "      (log-end! s)\n"
            "      'opened)))\n"
            "(printf \"~s\\n\" (list outcome n))\n"))))

(define (publish-then-adopt fault)
  (build!)
  (write-publisher! 1 (recs 1 2))
  (write-adopter!)
  ;; the publisher is a child, and it is gone before the adopter starts
  (system (string-append "scheme --script " publisher " > /dev/null 2>&1"))
  (system (string-append
            (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
            "scheme --script " adopter " > " adopter-out " 2>/dev/null"))
  (let ((text (let ((b (slurp adopter-out))) (if b (utf8->string b) ""))))
    (if (= 0 (string-length text))
        (list 'child-failed fault)
        (guard (e (#t (list 'unreadable text)))
          (read (open-string-input-port text))))))

(want "CONTROL: a first open after the publisher exits delivers its records"
      (publish-then-adopt #f)
      '(opened 2))
(want "with the adopter's own manifest flush failing, it delivers nothing"
      (publish-then-adopt "fsync-fail@deliver-barrier:file=published.sexp")
      '(raised 0))

(printf "\n== Q7: a kept candidate is checked by its content, not by its name ==\n")
;; The kept file is named by the hash of the bytes it should hold, so an
;; interrupted keep leaves exactly the right name over the wrong bytes.
;; Answering `kept' for that points the sender at evidence that is not
;; its candidate.
(define (kept-path segment sha)
  (string-append d "/writers/" M "/incoming/" (segment-file-name segment) "." sha ".seg"))
(define (gap-keep!)
  (build!)
  (child-says #f 1 (recs 1 3))
  (let* ((b (recs 20 21)) (sha (sha-of b)))
    (child-says #f 5 b)
    (list b sha (kept-path 5 sha))))
(want "CONTROL: a refused candidate is kept whole"
      (let* ((k (gap-keep!)) (b (car k)) (path (caddr k)))
        (list (file-exists? path) (equal? (slurp path) b)))
      (list #t #t))
(want "a kept file truncated under the right name is made whole again"
      (let* ((k (gap-keep!)) (b (car k)) (sha (cadr k)) (path (caddr k)))
        (put! path (make-bytevector 3))
        (child-says #f 5 b)
        (list (equal? (slurp path) b) (bytevector-length (slurp path))))
      (list #t (bytevector-length (recs 20 21))))

(printf "\n== Q8: a writer this store has never seen ==\n")
;; Until the directory is made here, the first thing to touch a new
;; writer was an open with `create` -- which creates a file and not the
;; directory above it, so the first publish for a new writer failed on an
;; open instead of answering.
(define (publish-to-new-writer fault)
  (build!)
  (system (string-append "rm -rf " d "/writers/neverseen"))
  (put! (string-append scratch "/cand.bin") (recs 1 2))
  (put! child
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi)\n"
            "        (only (igropyr crypto) sha256 bytevector->hex))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(define b (call-with-port (open-file-input-port \"" scratch "/cand.bin\")\n"
            "            get-bytevector-all))\n"
            "(printf \"~s\\n\"\n"
            "  (guard (e (#t (list 'raised)))\n"
            "    (log-publish! \"" d "\" \"neverseen\" 1 b\n"
            "                  (bytevector->hex (sha256 b)))))\n")))
  (system (string-append
            (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 ") "")
            "scheme --script " child " > " child-out " 2> " (string-append scratch "/trace.txt")))
  (let ((text (let ((b (slurp child-out))) (if b (utf8->string b) ""))))
    (if (= 0 (string-length text))
        (list 'child-failed fault)
        (guard (e (#t (list 'unreadable text)))
          (read (open-string-input-port text))))))
(define (trace-has? pattern)
  (let* ((text (let ((b (slurp (string-append scratch "/trace.txt")))) (if b (utf8->string b) "")))
         (n (string-length text)) (m (string-length pattern)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring text i (+ i m)) pattern) #t)
            (else (loop (+ i 1)))))))
(want "the first candidate for an unseen writer is published, not an open failure"
      (publish-to-new-writer #t)
      '(published 1))
;; AND THE NAME IS DURABLE. A segment whose directory entry is not
;; flushed is a segment a crash can strand where no reader looks.
(want "and the entry for it in writers/ was made durable"
      (trace-has? (string-append "(trace fsync " d "/writers #f)"))
      #t)

(printf "\n== Q9: idempotent means listed durably ==\n")
;; A sync client deletes its own copy on the strength of this word, and
;; the entry it names may have been written by a process that died before
;; flushing it.
(want "CONTROL: with nothing armed the second offer is idempotent"
      (begin (build!)
             (child-says #f 1 (recs 1 3))
             (child-says #f 1 (recs 1 3)))
      '(idempotent present listed))
(want "with the manifest's flush failing, idempotent does not get said"
      (begin (build!)
             (child-says #f 1 (recs 1 3))
             (child-says "fsync-fail@publish:file=published.sexp" 1 (recs 1 3)))
      '(raised present listed))

(printf "\n== Q10: a manifest that cannot be read ==\n")
;; Unreadable and unparseable are one fact to every caller: this store
;; cannot say what this writer published. Either way the answer is an
;; integrity error naming the manifest, and the writer stops before it --
;; not an implementation's own i/o condition reaching a caller as a
;; broken tool.
(define (with-manifest-text text thunk)
  (build!)
  (child-says #f 1 (recs 1 3))
  (put! (string-append d "/writers/" M "/published.sexp") (string->utf8 text))
  (thunk))
(want "CONTROL: the manifest as published reads, and the writer has its records"
      (begin (build!) (child-says #f 1 (recs 1 3))
             (let* ((ls (log-open d)) (p (load-prefix ls M)))
               (let ((r (if p (discovery-end-seq p) 'no-prefix)))
                 (load-abort! ls 'probe) r)))
      3)
(want "a truncated manifest is an integrity error, and the writer stops before it"
      (with-manifest-text "((1 \"aa\" 1"
        (lambda ()
          (list (guard (e ((log-error? e) (log-error-kind e)) (#t 'raised))
                  (read-manifest d M) 'no-error)
                (let* ((ls (log-open d)) (p (load-prefix ls M)))
                  (let ((r (if p (discovery-end-seq p) 'no-prefix)))
                    (load-abort! ls 'probe) r)))))
      (list 'manifest 0))
(want "and an entry of the wrong shape is the same integrity error"
      (with-manifest-text "((1 . \"aa\"))\n"
        (lambda ()
          (guard (e ((log-error? e) (log-error-kind e)) (#t 'raised))
            (read-manifest d M) 'no-error)))
      'manifest)

(printf "\n== Q11: a metadata file that will not open ==\n")
;; WHAT A STORE CANNOT READ IT CANNOT PROMISE. On the reading side that
;; means the writer stops before its records and every other writer is
;; unaffected -- the open itself succeeds, because one writer's
;; unreadable metadata is a fact about that writer and not a broken
;; tool. On the writing side it means the next append is refused. The
;; reason carries the path and what the operating system said, which is
;; the one thing an integrity note cannot reconstruct afterwards.
(define (unreadable-manifest! w)
  (system (string-append "chmod 000 " d "/writers/" w "/published.sexp")))
(define (readable-manifest! w)
  (system (string-append "chmod 644 " d "/writers/" w "/published.sexp")))

(define (open-and-report)
  (let* ((ls (log-open d))
         (p (load-prefix ls M))
         (ends (if p (discovery-end-seq p) 'no-prefix))
         (kinds (map (lambda (e) (log-error-kind (cdr e))) (load-integrity ls))))
    (load-abort! ls 'probe)
    (list ends kinds)))

(want "CONTROL: with the manifest readable the writer has its records"
      (begin (build!) (child-says #f 1 (recs 1 3)) (open-and-report))
      (list 3 '()))
(want "an unreadable manifest stops that writer, and the open still succeeds"
      (begin (build!) (child-says #f 1 (recs 1 3))
             (unreadable-manifest! M)
             (let ((r (open-and-report))) (readable-manifest! M) r))
      (list 0 '(metadata-unreadable)))
(want "TWIN: made readable again, the records are delivered once more"
      (open-and-report)
      (list 3 '()))
;; AND THE REASON IS IN THE REPORT, path and operating-system text both:
;; an operator reading `check` is told which file and why.
(want "the note names the file and what the system said about it"
      (begin (build!) (child-says #f 1 (recs 1 3))
             (unreadable-manifest! M)
             (let* ((ls (log-open d))
                    (detail (let ((es (load-integrity ls)))
                              (if (null? es) 'none (log-error-detail (cdr (car es)))))))
               (load-abort! ls 'probe)
               (readable-manifest! M)
               (list (and (assq 'path detail) #t)
                     (cdr (assq 'reason detail)))))
      (list #t "Permission denied"))

(printf "\n== Q12: and the writing side refuses rather than promising ==\n")
;; The reading side stops that writer before its records; the writing
;; side stops before its next one. A local writer whose own metadata will
;; not open cannot be flushed, cannot be compared against the versions
;; the session remembers, and cannot be promised durable -- so the append
;; is refused before anything is reserved, and the log does not grow.
(define (append-once)
  (guard (e (#t (list 'raised)))
    (let* ((s (log-begin d (lambda args 'applied)))
           (v (session-view s))
           (r (if v
                  (session-append! s (make-frame (view-revision v) (view-epoch v)
                                                 (view-writer v) (view-expect-seq v)
                                                 "agent:claude" '() '(put "w.1" ())))
                  'no-view)))
      (log-end! s)
      (if (pair? r) (list (car r) (cadr r)) r))))
(define (local-log-size)
  (let ((p (string-append d "/writers/" W "/" (segment-file-name 1))))
    (if (file-exists? p) (file-size p) 0)))

(want "CONTROL: with its metadata readable the local writer appends"
      (begin (build!) (append-once))
      '(committed 1))
(want "with its own metadata unreadable the append is refused, and nothing is written"
      (begin (build!)
             (put! (string-append d "/writers/" W "/published.sexp") (string->utf8 "()\n"))
             (append-once)
             (let ((before (local-log-size)))
               (system (string-append "chmod 000 " d "/writers/" W "/published.sexp"))
               (let ((answer (append-once)))
                 (system (string-append "chmod 644 " d "/writers/" W "/published.sexp"))
                 (list answer (= before (local-log-size))))))
      (list '(refused-before-reserve metadata-unreadable) #t))
(want "TWIN: readable again, the next append commits"
      (append-once)
      '(committed 2))

(printf "\n~a failures\n" bad)
(printf "log19 complete\n")
