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

(printf "\n~a failures\n" bad)
(printf "log19 complete\n")
