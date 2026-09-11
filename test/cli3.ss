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

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
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
         (beside (string-append dir "/cli.ss"))
         (above (string-append dir "/../cli.ss")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'cli3
              "cli.ss is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.ss sitting beside this fixture was
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
    (list code
          (let loop ((i 0) (start 0) (out '()))
            (cond
              ((>= i (string-length text)) (reverse out))
              ((char=? (string-ref text i) #\newline)
               (let ((line (substring text start i)))
                 (loop (+ i 1) (+ i 1)
                       (if (= 0 (string-length line))
                           out
                           (cons (guard (e (#t (list 'unreadable line)))
                                   (read (open-string-input-port line)))
                                 out)))))
              (else (loop (+ i 1) start out)))))))

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
;; CONTROL: e1 and e2 both score 2 for `cat`, and e1 was inserted first,
;; so it also sorts first by id -- which would hide an ordering defect.
;; The row below therefore also asserts that e2 follows e1, and this
;; control says the two facts are independent: the tie is real.
(want "CONTROL: the two blocks that tie were created in id order"
      (string<? e1 e2)
      #t)

(want "title hits score 2, src hits 1, both 3, and equal scores go by id"
      (lines-of (run d2 "search" "cat"))
      (list (list 'hit e4 3 "cat and dog")
            (list 'hit e1 2 "cat first inserted")
            (list 'hit e2 2 "Concatenate strings")
            (list 'hit e3 1 "we concatenate in the body")))
(want "a hit is a case-insensitive substring, so cat finds concatenate"
      (lines-of (run d2 "search" "CAT"))
      (lines-of (run d2 "search" "cat")))
(want "every token must hit: one that matches nothing empties the answer"
      (run d2 "search" "cat zzzzz")
      (list 0 '()))
(want "TWIN: two tokens that both hit the same block keep it"
      (lines-of (run d2 "search" "cat mat"))
      (list (list 'hit e4 3 "cat and dog")))
;; A QUERY IS TEXT. This one would be an enormous exact integer if any
;; part of the path handed it to a numeric parser, and the row would not
;; return rather than returning empty.
(want "a query that looks like a number is matched as text, and returns"
      (run d2 "search" "#e1e99999999")
      (list 0 '()))
(want "no hits is an answer, not an error"
      (run d2 "search" "zzzzzzzz")
      (list 0 '()))

(printf "\n~a failures\n" bad)
(printf "cli3 complete\n")
