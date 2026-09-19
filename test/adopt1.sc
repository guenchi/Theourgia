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

;; J1: adopt and generations (design 4.1, 5.2 step 7; plan L20, L19(b),
;; and the rollback input from the C-batch review).
(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace)
        (theourgia reduce) (theourgia store) (theourgia wire))

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


(define scratch (test-dir "adopt1"))
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
(define (put! p bv)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o bv))))
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

(printf "== adopt refuses on a store with nothing wrong ==\n")
;; IT IS THE WAY OUT OF A NAMED CONDITION. A voluntary generation change
;; splits the history for nothing, so the refusal lists what was looked
;; for rather than just saying no.
(define d1 (fresh-store! '("One" "Two")))
(want "a healthy store is refused, and the refusal names the conditions"
      (store-adopt! d1)
      (list 'refused 'not-needed
            (list 'checked '(identity retired missing-generation registry-ahead damage))))
(want "CONTROL: and nothing was created"
      (length (store-writers d1))
      1)

(printf "== L20: adopt across a damaged record ==\n")
;; THE LAST LINE IS COMPLETE AND ITS DATUM PARSES; only the checksum is
;; wrong. That is the case where a writer cannot tell where its history
;; ends by looking at the bytes alone, and the one adopt exists for.
(define d2 (fresh-store! '("One" "Two")))
(define old2 (writer-of d2))
;; THE PREFIX LENGTH IS MEASURED, NOT WRITTEN DOWN. A record's length
;; depends on the actor string among other things, so a byte count
;; copied from another run is a number about that run. This is the file
;; length before the damaged line is appended -- which is exactly what
;; the retirement offset has to come back as.
(define prefix-bytes
  (string-length (slurp (string-append d2 "/writers/" old2 "/000001.sexp"))))
(append-line! (string-append d2 "/writers/" old2 "/000001.sexp")
              "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
(want "CONTROL: the valid prefix stops at two, and the store knows it is damaged"
      (list (records-of d2) (car (adopt-needed? d2)))
      (list 2 'damage))
(define adopt2 (store-adopt! d2))
(want "adopt reports where it came from, where it went, and why"
      (list (car adopt2)
            (car (cadr adopt2)) (car (caddr adopt2))
            (cadr (assq 'reason (cdr adopt2))))
      (list 'adopted 'from 'to 'damage))
(define new2 (cadr (assq 'to (cdr adopt2))))
;; THE RETIREMENT OFFSET IS WHERE THE PREFIX ENDS, not where the file
;; ends. Declaring the file length points the marker at the very bytes
;; that caused the adopt, and the next load reports the marker and the
;; scan disagreeing about where sequence two finished.
(want "the retirement marker records the prefix, not the file length"
      (let* ((text (slurp (string-append d2 "/writers/" old2 "/retired.sexp")))
             (d (guard (e (#t 'unreadable)) (read (open-string-input-port text)))))
        (list (cadr (assq 'prefix d))
              (caddr (assq 'prefix d))
              (cadddr (assq 'prefix d))
              (and (assq 'successor d) (cadr (assq 'successor d)))))
      (list 1 prefix-bytes 2 new2))
(want "and the store is clean afterwards: the damage is past the boundary"
      (cadr (assq 'verdict (cdr (store-check d2))))
      'ok)
;; THE NEW WRITER'S FIRST RECORD COVERS THE OLD PREFIX. It falls out of
;; the ordinary deps rule -- the applied cut minus this writer -- but
;; only if the old prefix was applied, so it is asserted rather than
;; trusted.
(want "a write on the new writer names the old writer's last sequence"
      (begin
        (with-store-write d2
          (lambda (st v) '((insert root #f ((kind . section) (title . "Three"))))) "t")
        (deps-of d2 new2 0))
      (list (cons old2 2)))
(want "the old prefix's blocks are still there, and the new one is too"
      (titles-of (open-and-reduce d2))
      '("One" "Three" "Two"))
(want "and again after a restart, and after another"
      (list (titles-of (open-and-reduce d2)) (titles-of (open-and-reduce d2)))
      (list '("One" "Three" "Two") '("One" "Three" "Two")))

(printf "== the rollback input, refused two ways ==\n")
;; BACK UP, ADOPT, WRITE ON THE NEW GENERATION, THEN RESTORE THE BACKUP
;; IN PLACE. The store is now exactly as it was before the adopt --
;; same identity, same owner, same inode, water mark 100 -- and the old
;; writer looks perfectly writable. What says otherwise is not in the
;; store at all: it is the registry, which does not live in the store
;; and so does not travel with the backup.
(define d3 (fresh-store! '("One" "Two")))
(define old3 (writer-of d3))
(define backup3 (string-append scratch "/backup3"))
(system (string-append "rm -rf " backup3 "; cp -R " d3 " " backup3))
;; force an adopt, then commit on the new generation
(append-line! (string-append d3 "/writers/" old3 "/000001.sexp")
              "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
(define adopt3 (store-adopt! d3))
(define new3 (cadr (assq 'to (cdr adopt3))))
(with-store-write d3
  (lambda (st v) '((insert root #f ((kind . section) (title . "OnTheNew"))))) "t")
(want "CONTROL: the new generation committed, and the registry has it active"
      (list (car adopt3)
            (> (string-length (slurp (string-append d3 "/writers/" new3 "/000001.sexp"))) 0))
      (list 'adopted #t))
;; RESTORED IN PLACE: the directory itself is never removed, so its
;; inode is the one recorded in instance.sexp. Deleting and recreating
;; it changes the inode, the identity check fires first, and the row
;; then measures the identity path instead of the generation chain --
;; green, and about something else. A real restore-from-backup over a
;; live directory keeps the inode, which is exactly why the registry has
;; to be the thing that notices.
(define (restore-in-place! backup target)
  (system (string-append "find " target " -mindepth 1 -delete && cp -R "
                         backup "/. " target "/")))
(restore-in-place! backup3 d3)
(want "CONTROL: identity still verifies -- so nothing but the chain can object"
      (list (verify-instance d3)
            (list-sort string<? (store-writers d3))
            (file-exists? (string-append d3 "/writers/" old3 "/retired.sexp")))
      (list 'ok (list old3) #f))
;; (1) A FRESHLY OPENED HANDLE. The registry says a generation is active
;; that this store no longer contains, so the chain does not agree.
(want "a new handle is refused, because a generation it should have is gone"
      (adopt-needed? d3)
      '(missing-generation))
(want "and the write itself is refused rather than merely warned about"
      (let ((a (with-store-write d3
                 (lambda (st v) '((insert root #f ((kind . section) (title . "W101")))))
                 "t")))
        (list (car (car a)) (cadr (car a)) (caddr (car a)) (cadddr (car a))))
      (list 'error 'refused 'missing-generation '(remedy adopt)))
;; (2) A HANDLE OPENED BEFORE THE ADOPT AND KEPT ALIVE ACROSS IT. This
;; is the one a check at open cannot see: everything the handle looked
;; at when it opened is still true. The refusal has to come from the
;; re-check inside the reservation, not from anything decided earlier.
(define d4 (fresh-store! '("One" "Two")))
(define old4 (writer-of d4))
(define backup4 (string-append scratch "/backup4"))
(system (string-append "rm -rf " backup4 "; cp -R " d4 " " backup4))
(define held-answer
  (let ((held (log-begin d4 (lambda args 'applied))))
    ;; the handle is open and holds a view; now the world changes underneath it
    (let ((v (session-view held)))
      (log-end! held)
      ;; another party adopts and commits
      (append-line! (string-append d4 "/writers/" old4 "/000001.sexp")
                    "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
      (store-adopt! d4)
      (with-store-write d4
        (lambda (st v) '((insert root #f ((kind . section) (title . "OnTheNew"))))) "t")
      ;; and the store is rolled back to before all of it
      (restore-in-place! backup4 d4)
      ;; the pre-adopt view still says sequence 3 is this writer's next
      (list (view-writer v) (view-expect-seq v)))))
(want "CONTROL: the held view still names the old writer and its next sequence"
      held-answer
      (list old4 3))
(want "a write authorised by that view is refused too"
      (let ((a (with-store-write d4
                 (lambda (st v) '((insert root #f ((kind . section) (title . "W101")))))
                 "t")))
        (list (car (car a)) (cadr (car a)) (caddr (car a)) (cadddr (car a))))
      (list 'error 'refused 'missing-generation '(remedy adopt)))
(want "and adopt is what it asks for"
      (car (adopt-needed? d4))
      'missing-generation)

(printf "== the registry may not live inside the store it watches ==\n")
;; IT IS THE ONE WITNESS TO A ROLLBACK THAT DOES NOT TRAVEL WITH A
;; BACKUP. Put it under the store and restoring the store restores it
;; too: the generation record vanishes along with the generation, the
;; chain agrees, and the rolled-back store writes on. It fails silently
;; -- the store opens, writes, and reports itself healthy -- which is
;; how this fixture passed the rollback rows before the layout was
;; fixed.
(define d5 (fresh-store! '("One")))
(define home-outside (string-append scratch "/home" (number->string case-n)))
(want "CONTROL: with the registry outside, writes work and check is clean"
      (list (car (car (with-store-write d5
                        (lambda (st v) '((insert root #f ((kind . section) (title . "Two")))))
                        "t")))
            (cadr (assq 'registry (cdr (store-check d5))))
            (cadr (assq 'verdict (cdr (store-check d5)))))
      (list 'ok 'outside-store 'ok))
;; THE WHOLE HOME IS COPIED, dot files included: the machine identity
;; lives in it, and a home missing machine.sexp mints a new one -- the
;; write would then be refused for a changed machine identity instead,
;; and this row would be green about the wrong refusal.
;; THIS IS THE ONE PLACE A HOME INSIDE THE STORE IS DELIBERATE.
(define (parts-of a n)
  (let loop ((x a) (k n) (out '()))
    (cond ((= k 0) (reverse out))
          ((not (pair? x)) (loop '() (- k 1) (cons 'missing out)))
          (else (loop (cdr x) (- k 1) (cons (car x) out))))))
(want "CONTROL: the copied-in home keeps the machine identity"
      (begin
        (system (string-append "mkdir -p " d5 "/home && cp -R " home-outside "/. " d5 "/home/"))
        (putenv "THEOURGIA_HOME" (string-append d5 "/home"))
        (verify-instance d5))
      'ok)
(want "a registry under the store refuses every write, and names the remedy"
      (parts-of (car (with-store-write d5
                       (lambda (st v) '((insert root #f ((kind . section) (title . "Three")))))
                       "t"))
                4)
      (list 'error 'refused 'registry-inside-store
            '(remedy move-the-registry-outside-the-store)))
(want "and check says where the registry is, not just that something is wrong"
      (let ((r (store-check d5)))
        (list (cadr (assq 'registry (cdr r))) (cadr (assq 'verdict (cdr r)))))
      (list 'inside-store 'damaged))
;; A SYMLINK IS NOT A WAY ROUND IT. Ancestry is walked by (device,
;; inode), so a home whose path shares no prefix with the store is still
;; found to be inside it.
(want "a home reached through a symlink into the store is refused too"
      (begin
        (system (string-append "ln -s " d5 "/home " scratch "/aliased" (number->string case-n)))
        (putenv "THEOURGIA_HOME" (string-append scratch "/aliased" (number->string case-n)))
        (registry-inside-store?))
      #t)
(want "CONTROL: and pointing it back outside makes writes work again"
      (begin
        (putenv "THEOURGIA_HOME" home-outside)
        (list (registry-inside-store?)
              (car (car (with-store-write d5
                          (lambda (st v) '((insert root #f ((kind . section) (title . "Four")))))
                          "t")))))
      (list #f 'ok))

(printf "== L19(b): the registry ahead of the log ==\n")
;; KILLED BETWEEN RESERVING AND WRITING. The water mark is raised and
;; the record never lands, so the store's last sequence is below what
;; the registry vouches for. That is indistinguishable from a restored
;; backup, and is treated the same way.
(define d6 (fresh-store! '("One" "Two")))
(define old6 (writer-of d6))
;; THE ENTRY SAYS TWO THINGS AND THIS RAISES ONE OF THEM. `authorised`
;; is leave to write at these positions; `written` is how far records
;; actually reached the disk. A helper that raised both at once could not
;; tell the two rollback stories apart, and they have opposite answers.
(define (raise-field! d writer which to)
  (let* ((path (registry-path))
         (text (slurp path))
         (reg (read (open-string-input-port text)))
         (bumped (map (lambda (e)
                        (if (and (list? e) (= 6 (length e)) (equal? (caddr e) writer))
                            (if (eq? which 'authorised)
                                (list (car e) (cadr e) (caddr e) to
                                      (list-ref e 4) (list-ref e 5))
                                (list (car e) (cadr e) (caddr e)
                                      (max to (list-ref e 3)) (list-ref e 4) to))
                            e))
                      reg)))
    (call-with-port (open-file-output-port path (file-options no-fail))
      (lambda (o) (put-bytevector o (string->utf8 (format "~s\n" bumped)))))))
(define (raise-mark! d writer to) (raise-field! d writer 'written to))
;; THE REGISTRY WITHOUT ITS GENERATIONS, which is what a crash between
;; the retirement marker and the generation reservation leaves.
(define (write-registry-no-gen! d)
  (let* ((path (registry-path))
         (reg (read (open-string-input-port (slurp path)))))
    (call-with-port (open-file-output-port path (file-options no-fail))
      (lambda (o)
        (put-bytevector o (string->utf8
                            (format "~s\n"
                                    (remp (lambda (e) (and (list? e) (eq? (car e) 'gen)))
                                          reg))))))))
;; THE END OF THE VALID PREFIX, READ BEFORE ANY OF THIS HAPPENS. It is
;; the left end of the stretch the rollback loses, and taking it from the
;; adopt's own answer afterwards would be asking the thing under test to
;; supply the expectation.
(define end6 (discovery-end-seq (discover-prefix d6 old6 'held-exclusive)))
;; A RESERVATION NOBODY USED IS NOT A ROLLBACK. `authorised` above the
;; log's end is what every refusal leaves behind, and what every request
;; that reserves its range and uses part of it leaves behind. The gate
;; reads `written`, so a store in that state is healthy and writes on.
;;
;; ON ITS OWN STORE, because the rows below count records in d6 and a
;; write made here would move every one of their numbers.
(define d6a (fresh-store! '("One" "Two")))
(define old6a (writer-of d6a))
(raise-field! d6a old6a 'authorised 9)
(want "authorised ahead of the log is not a rollback, and the store writes on"
      (list (adopt-needed? d6a)
            (car (car (with-store-write d6a
                        (lambda (st v)
                          '((insert root #f ((kind . section) (title . "Ok")))))
                        "t"))))
      (list #f 'ok))
;; TWIN: `written` ahead of the log IS one -- records the registry says
;; reached the disk and the log no longer holds.
(use-store! d6)
(raise-mark! d6 old6 9)
(want "the store is refused, and the reason is that the registry is ahead"
      (let ((a (car (with-store-write d6
                      (lambda (st v) '((insert root #f ((kind . section) (title . "Three")))))
                      "t"))))
        (list (car a) (cadr a) (caddr a) (cadddr a)))
      (list 'error 'refused 'registry-ahead '(remedy adopt)))
(want "adopt is what it asks for, and afterwards the store writes again"
      (list (car (adopt-needed? d6))
            (car (store-adopt! d6))
            (car (car (with-store-write d6
                        (lambda (st v) '((insert root #f ((kind . section) (title . "Three")))))
                        "t"))))
      (list 'registry-ahead 'adopted 'ok))
(want "and the old prefix survived the adopt"
      (titles-of (open-and-reduce d6))
      '("One" "Three" "Two"))
;; THE STRETCH THE ROLLBACK LOST IS RECORDED BEFORE THE SUCCESSOR EXISTS.
;; The registry authorised records up to 9 and the log holds fewer, so
;; everything between the log's end and the mark was written, promised,
;; and is now unrecoverable -- and a request whose positions fall in
;; there cannot be told apart from one that ran. The entry is closed,
;; because the mark is a real top.
;;
;; The left end is measured from the store BEFORE the adopt, which is
;; what "the end of the valid prefix" means; the right end is 9, which
;; this fixture set. Neither is read out of the adopt's own answer.
(want "a rollback adopt leaves the lost stretch behind it"
      (uncertain-load d6 old6)
      (list (list (list old6 end6 9)) #f))
;; TWIN: and an adopt that lost nothing leaves nothing. Without this the
;; row above would pass for an implementation that recorded a stretch
;; after every adopt.
(want "TWIN: the earlier adopt, which was not a rollback, recorded no stretch"
      (uncertain-load d2 old2)
      '(() #f))
;; RESUMING MUST NOT SKIP THE ENTRY. A crash can land on either side of
;; it, so recovery asks the same question again rather than assuming.
;; Here the entry and the successor's owner are both taken away and the
;; generation is put back to pending -- the state a crash between step 1
;; and step 3 leaves -- and recovery has to produce the entry again.
(define new6 (car (list-sort string<? (remp (lambda (w) (string=? w old6)) (store-writers d6)))))
(define (set-gen-pending!)
  (let ((reg (map (lambda (e)
                    (if (and (list? e) (eq? (car e) 'gen))
                        (append (list-head e 8) (list 'pending))
                        e))
                  (read (open-string-input-port (slurp (registry-path)))))))
    (call-with-port (open-file-output-port (registry-path) (file-options no-fail))
      (lambda (o) (put-bytevector o (string->utf8 (format "~s\n" reg)))))))
(want "recovery writes the entry when the crash came before it"
      (begin
        (delete-file (string-append d6 "/writers/" old6 "/uncertain.sexp"))
        (delete-file (string-append d6 "/writers/" new6 "/owner.sexp"))
        (set-gen-pending!)
        (continue-adopt! d6)
        (uncertain-load d6 old6))
      (list (list (list old6 end6 9)) #f))
;; AND WRITING ONE THAT IS ALREADY THERE IS HARMLESS, which is the
;; property that makes a resume safe to run as often as a crash demands.
(want "and running it again does not write the entry twice"
      (begin
        (delete-file (string-append d6 "/writers/" new6 "/owner.sexp"))
        (set-gen-pending!)
        (continue-adopt! d6)
        (uncertain-load d6 old6))
      (list (list (list old6 end6 9)) #f))
;; AND IT ADDS RATHER THAN REPLACES. An earlier adopt's entry is still
;; true -- the records it covers are still unrecoverable -- so a second
;; adopt writing only its own would take away a stretch nothing else
;; would ever put back. Note what makes this row discriminate and the one
;; above not: with one entry in the file, "added" and "replaced" produce
;; the same file.
(want "a second adopt keeps the entries that were already there"
      (begin
        (uncertain-write! d6 old6 (list (list old6 100 200) (list old6 end6 9)) 'commit)
        (delete-file (string-append d6 "/writers/" new6 "/owner.sexp"))
        (set-gen-pending!)
        (continue-adopt! d6)
        (uncertain-load d6 old6))
      (list (list (list old6 100 200) (list old6 end6 9)) #f))

(printf "== a terminal transaction is never resumed ==\n")
;; THE SIX STEPS THE REVIEW ASKED FOR. Back up; write the retirement
;; marker for T; back up again; reserve T in the registry; restore the
;; first backup, so the store has no marker while the registry has T
;; pending; recovery cancels T; restore the second backup, so the marker
;; for T is back while the registry calls T cancelled.
;; NOW THE STORE MATCHES ROW ONE WORD FOR WORD -- a marker bearing T,
;; and (as far as that row looks) nothing in the registry contradicting
;; it -- and row one's action is to resume. A terminal state has to win
;; over the shape of the store.
(define d7 (fresh-store! '("One" "Two")))
(define old7 (writer-of d7))
(define home7 (string-append scratch "/home" (number->string case-n)))
(define tx7 "tx-restored-cancelled-0001")
(define (registry-now) (read (open-string-input-port (slurp (registry-path)))))
(define (write-registry-text! reg)
  (call-with-port (open-file-output-port (registry-path) (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 (format "~s\n" reg))))))
(define b0 (string-append scratch "/b0"))
(define b1 (string-append scratch "/b1"))
(system (string-append "rm -rf " b0 "; cp -R " d7 " " b0))
;; step 1 by hand: the retirement marker, no successor
(call-with-port (open-file-output-port (string-append d7 "/writers/" old7 "/retired.sexp")
                                       (file-options no-fail))
  (lambda (o) (put-bytevector o (string->utf8
                                  (string-append "((format 1) (tx \"" tx7 "\")"
                                                 " (prefix 1 214 2))\n")))))
(system (string-append "rm -rf " b1 "; cp -R " d7 " " b1))
;; step 2 by hand: the generation is reserved, pending
(write-registry-text!
  (append (registry-now)
          (list (list 'gen (store-id-of d7)
                      (read (open-string-input-port
                              (string-append "\"" (cadr (assq 'instance
                                (read (open-string-input-port
                                        (slurp (string-append d7 "/writers/" old7
                                                              "/owner.sexp")))))) "\"")))
                      tx7 old7 "wouldbenew" #f #f 'pending))))
(want "CONTROL: the store now has the marker and the registry has T pending"
      (list (and (file-exists? (string-append d7 "/writers/" old7 "/retired.sexp")) #t)
            (let loop ((r (registry-now)))
              (cond ((null? r) 'no-gen)
                    ((and (list? (car r)) (eq? (car (car r)) 'gen)) (list-ref (car r) 8))
                    (else (loop (cdr r))))))
      (list #t 'pending))
;; restore B0: no marker, no new owner -- the generation never got far
(restore-in-place! b0 d7)
(want "recovery cancels a generation that left nothing behind"
      (begin
        (continue-adopt! d7)
        (let loop ((r (registry-now)))
          (cond ((null? r) 'no-gen)
                ((and (list? (car r)) (eq? (car (car r)) 'gen)) (list-ref (car r) 8))
                (else (loop (cdr r))))))
      'cancelled)
;; restore B1: the marker for T is back, and the registry calls T cancelled
(restore-in-place! b1 d7)
(want "the restored marker does not resurrect the cancelled transaction"
      (begin
        (continue-adopt! d7)
        (let loop ((r (registry-now)))
          (cond ((null? r) 'no-gen)
                ((and (list? (car r)) (eq? (car (car r)) 'gen)) (list-ref (car r) 8))
                (else (loop (cdr r))))))
      'cancelled)
(want "and the old writer's prefix is still readable"
      (titles-of (open-and-reduce d7))
      '("One" "Two"))

(printf "== a completed chain that belongs to somebody else ==\n")
;; A COPIED STORE PRESENTS "a marker bearing T, and no T in MY registry"
;; -- row one word for word, whose action is to resume. Resuming it
;; would finish a transaction whose new owner names another instance's
;; nonce, which proves nothing about this one's authority. The nonce
;; test is a gate on the table, not a row in it.
(define d8 (fresh-store! '("One" "Two")))
(define old8 (writer-of d8))
(append-line! (string-append d8 "/writers/" old8 "/000001.sexp")
              "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
(define adopt8 (store-adopt! d8))
(define new8 (cadr (assq 'to (cdr adopt8))))
(define copy8 (string-append scratch "/copy8"))
(system (string-append "rm -rf " copy8 "; cp -R " d8 " " copy8))
(want "CONTROL: the copy carries a completed chain and its owner's nonce"
      (list (car adopt8)
            (and (file-exists? (string-append copy8 "/writers/" new8 "/owner.sexp")) #t))
      (list 'adopted #t))
;; the copy is a different directory, so its identity does not verify
;; A COPY STILL CARRIES THE ORIGINAL'S instance.sexp, so its owners'
;; nonces match trivially and the gate cannot fire yet. Identity is
;; what notices first, and the design's order is why: an identity
;; mismatch mints a new nonce BEFORE the registry is consulted, and
;; only then is "whose transaction is this" a question that can be
;; asked at all.
(want "the copy is an identity mismatch, so nothing is resumed"
      (begin
        (putenv "THEOURGIA_HOME" (string-append scratch "/home-copy8"))
        (system (string-append "mkdir -p " scratch "/home-copy8"))
        (list (car (verify-instance copy8))
              (continue-adopt! copy8)))
      (list 'mismatch 'identity-mismatch))
;; ONCE IT ADOPTS, IT MINTS A NONCE -- and from then on the chain it
;; inherited names somebody else, which is what the gate is for.
;; MEASURED BEFORE THE ADOPT, for the same reason as the rollback row.
(define end8 (discovery-end-seq (discover-prefix copy8 new8 'held-exclusive)))
(want "adopting the copy mints a nonce and branches from the head, not the retired writer"
      (let ((a (store-adopt! copy8)))
        (list (car a)
              (cadr (assq 'from (cdr a)))
              (cadr (assq 'reason (cdr a)))))
      (list 'adopted new8 'identity))
;; AN IDENTITY MISMATCH LEAVES AN OPEN STRETCH, and the openness is the
;; point. A rollback knows where the lost records stop -- the water mark
;; is a real top. Here the store has found that it is not who its own
;; records say it is, so nothing above the prefix can be attributed to
;; it at all and there is no top to name. That is also why no operator
;; may later write a `not-executed` over it: only a closed interval is
;; an observation, and this one is not.
(want "an identity-mismatch adopt leaves an open stretch behind it"
      (uncertain-load copy8 new8)
      (list (list (list new8 end8 #f)) #f))
(want "and afterwards the inherited chain is recognised as imported"
      (begin (putenv "THEOURGIA_HOME" (string-append scratch "/home-copy8"))
             (continue-adopt! copy8))
      'imported)
(want "CONTROL: and the original, whose nonce does match, is not called imported"
      (begin (use-store! d8) (continue-adopt! d8))
      'nothing-to-do)

;; AND AN ADOPT THAT IS TRIED AGAIN DOES NOT ERASE WHAT THE FIRST ONE
;; RECORDED. Step 1 writes the whole record, so it has to carry what the
;; record already said. An adopt that gets as far as that write and no
;; further leaves a retirement with no successor -- and `adopt-needed?`
;; answers `retired` for exactly that shape, so the next adopt comes back
;; through step 1 with a reason of its own that records nothing. Writing
;; only the new interval replaced the recorded stretch with `()`: the one
;; place those coordinates still existed, overwritten by the retry of the
;; adopt that computed them.
(define d19 (fresh-store! '("One" "Two")))
(define old19 (writer-of d19))
(define end19 (discovery-end-seq (discover-prefix d19 old19 'held-exclusive)))
(raise-mark! d19 old19 9)
(store-adopt! d19)
(want "CONTROL: the first adopt recorded the stretch"
      (car (uncertain-load d19 old19))
      (list (list old19 end19 9)))
(want "a retry after a crash between the steps keeps it"
      (begin
        ;; the state a crash after step 1 leaves: a marker, no generation,
        ;; no successor
        (let ((new19 (car (list-sort string<?
                            (remp (lambda (w) (string=? w old19)) (store-writers d19))))))
          (system (string-append "rm -rf " d19 "/writers/" new19)))
        (put! (string-append d19 "/writers/" old19 "/retired.sexp")
              (string->utf8
                (string-append "((format 2) (tx \"tx-crashed-19\") (prefix 1 0 "
                               (number->string end19) ") (uncertain ((\"" old19 "\" "
                               (number->string end19) " 9))))\n")))
        (delete-file (string-append d19 "/writers/" old19 "/uncertain.sexp"))
        (write-registry-no-gen! d19)
        (list (car (adopt-needed? d19))
              (car (store-adopt! d19))
              (car (uncertain-load d19 old19))))
      (list 'retired 'adopted (list (list old19 end19 9))))

;; AND AN ADOPT THAT CANNOT READ WHAT IT IS ABOUT TO REPLACE REFUSES.
;; Step 1 rewrites the retirement record, and the step after it rewrites
;; the cache -- so an adopt that guessed at either would replace the
;; history with its guess, and no later successful read could take that
;; back. The cache is the dangerous one: a stretch that a repair has
;; since made underivable lives there and nowhere else, so reading an
;; unreadable cache as empty would take both copies in one adopt.
(define d20 (fresh-store! '("One" "Two")))
(define old20 (writer-of d20))
(raise-mark! d20 old20 9)
(want "an unreadable cache stops the adopt rather than replacing it"
      (begin
        (put! (string-append d20 "/writers/" old20 "/uncertain.sexp")
              (string->utf8 "((not a"))
        (guard (e (#t 'refused)) (store-adopt! d20) 'adopted))
      'refused)
(want "TWIN: with the cache readable the same adopt goes through"
      (begin
        (uncertain-write! d20 old20 '() 'commit)
        (car (store-adopt! d20)))
      'adopted)

(printf "\n== the cache is a cache, and the record is the authority ==\n")
;; `uncertain.sexp` IS DOCUMENTED AS A CACHE -- written so a reader need
;; not re-derive the set on every call, read for nothing that is not
;; also derivable, and therefore safe to lose. Three of the five kinds
;; live up to that: a divergence, a torn tail and an unreadable
;; beginning are all visible in a writer's own files whenever anyone
;; cares to look. These two did not. Their coordinates existed at the
;; moment an adopt held them and nowhere else, so the cache was the only
;; copy, and a store whose cache was deleted reported itself HEALTHY
;; about a stretch it could not vouch for -- no interval and no
;; integrity kind, which is the pair of readings a clean store gives.
;; A retried request whose positions fell in that stretch was then told
;; to run a second time.
;;
;; SO THE RECORD CARRIES THEM, written into the retirement marker before
;; the successor's owner exists. The rows below are the three ways the
;; cache can be lost, and they are three rows rather than one because
;; they arrive at three different arms of the reconciliation.
(define d16 (fresh-store! '("One" "Two")))
(define old16 (writer-of d16))
;; MEASURED BEFORE THE ADOPT, as everywhere else in this file: the left
;; end is where the valid prefix ends, and reading it out of the adopt's
;; own answer afterwards would ask the thing under test for the
;; expectation.
(define end16 (discovery-end-seq (discover-prefix d16 old16 'held-exclusive)))
(raise-mark! d16 old16 9)
(define u16 (string-append d16 "/writers/" old16 "/uncertain.sexp"))
(want "CONTROL: the rollback adopt records the stretch, and the store is clean"
      (list (car (store-adopt! d16)) (uncertain-load d16 old16))
      (list 'adopted (list (list (list old16 end16 9)) #f)))
;; AND THE RECORD SAYS SO IN ITS OWN RIGHT. Without this row the three
;; below could all pass on an implementation that kept a second cache
;; somewhere else; this is the row that names where the fact lives.
(want "the retirement record carries the stretch"
      (let ((text (slurp (string-append d16 "/writers/" old16 "/retired.sexp"))))
        (and (string? text)
             (let loop ((xs (read (open-string-input-port text))))
               (cond ((null? xs) 'no-uncertain-clause)
                     ((and (pair? (car xs)) (eq? (caar xs) 'uncertain)) (cadr (car xs)))
                     (else (loop (cdr xs)))))))
      (list (list old16 end16 9)))
;; AND IT SURVIVES THE STEP THAT REWRITES THE RECORD. The successor is
;; backfilled into this same file after the owner is installed, and the
;; rewrite that did it was built out of the fields somebody remembered
;; to pass -- so a clause it did not name was dropped, silently, while
;; every step reported success. The successor being there at all is what
;; makes this row mean something.
(want "and the successor was backfilled into the same record without displacing it"
      (and (retired-successor d16 old16) #t)
      #t)
;; ONE: DELETED. The absent arm reports `absent` only when there is
;; something to be absent FROM, so a store that has forgotten the
;; stretch reads exactly like a healthy writer that never had one.
(want "a deleted cache loses neither the stretch nor the report"
      (begin (delete-file u16) (uncertain-load d16 old16))
      (list (list (list old16 end16 9)) (list 'uncertain-cache 'absent)))
;; TWO: TRUNCATED. An empty file is not an empty list; it does not parse
;; at all, and the store has to say so rather than read it as a claim.
(want "a truncated cache loses neither the stretch nor the report"
      (begin (put! u16 (string->utf8 "")) (uncertain-load d16 old16))
      (list (list (list old16 end16 9)) (list 'uncertain-cache 'unreadable)))
;; THREE: REWRITTEN AS `()`. The dangerous one, because it is the only
;; one of the three that is a well-formed cache: it is a positive claim
;; that nothing about this writer is uncertain, made in the file whose
;; job is to answer that question.
(want "a cache rewritten as an empty claim loses neither the stretch nor the report"
      (begin (put! u16 (string->utf8 "()\n")) (uncertain-load d16 old16))
      (list (list (list old16 end16 9))
            (list 'uncertain-cache 'stale (list (list old16 end16 9)))))
;; AND THE ANSWER A CLIENT GETS, which is what the whole section is for.
;; A reading of the interval table is not a reading of the write path:
;; the table could be right and the request still run. The request
;; declares a cursor inside the lost stretch, so the only correct answer
;; is that nobody can tell whether it ran.
(want "and a request whose positions fall in the stretch is refused, not executed"
      (begin
        (delete-file u16)
        (let ((a (car (with-store-write d16
                        (lambda (st v)
                          (list (list 'insert 'root #f
                                      (list (cons 'kind 'section) (cons 'title "Dup")))))
                        "t"
                        (make-write-request "t" 'insert (list "root" "Dup")
                                            "r-lost" (cons old16 end16))))))
          (list (car a) (cadr a))))
      (list 'error 'unknown))
(want "and nothing was written"
      (titles-of (open-and-reduce d16))
      '("One" "Two"))
;; TWIN: THE SAME DELETION ON A STORE THAT NEVER ADOPTED. Without this
;; the rows above would pass for an implementation that answered
;; `unknown` whenever the cache was missing, which would stop every
;; healthy store from taking its first write.
(define d17 (fresh-store! '()))
(define old17 (writer-of d17))
(want "TWIN: a healthy store has no cache to lose and writes anyway"
      (list (uncertain-load d17 old17)
            (file-exists? (string-append d17 "/writers/" old17 "/uncertain.sexp"))
            (car (car (with-store-write d17
                        (lambda (st v)
                          (list (list 'insert 'root #f
                                      (list (cons 'kind 'section) (cons 'title "First")))))
                        "t"
                        (make-write-request "t" 'insert (list "root" "First")
                                            "h-1" (cons old17 0))))))
      (list '(() #f) #f 'ok))
;; AND THE OPEN KIND TOO. The identity stretch is the one that cannot be
;; worked out again from anything: a rollback leaves the registry's mark
;; and the retired prefix on disk, so its coordinates can be asked for a
;; second time, while an identity mismatch is a question about a nonce
;; the store has already replaced. If the record did not carry it, no
;; recovery could put it back.
;; AND A RECORD WRITTEN BEFORE THE CLAUSE EXISTED. Every retirement
;; record on disk when this shipped is format 1 and carries no
;; `uncertain` clause, and nothing can now say whether the adopt that
;; wrote it lost a stretch: the registry mark it was measured against is
;; long gone. Reading the absence as "nothing was lost" is the defect
;; this batch closes, one version later; so the absence is read as the
;; widest thing it could mean, and the version number is what makes the
;; two absences tellable apart.
(define d18 (fresh-store! '("One" "Two")))
(define old18 (writer-of d18))
(define end18 (discovery-end-seq (discover-prefix d18 old18 'held-exclusive)))
(raise-mark! d18 old18 9)
(store-adopt! d18)
(define r18 (string-append d18 "/writers/" old18 "/retired.sexp"))
(want "CONTROL: the record this version writes says so in its version"
      (car (read (open-string-input-port (slurp r18))))
      '(format 2))
(want "a format 1 record with no clause puts everything above its prefix in doubt"
      (begin
        ;; the record the previous version wrote, byte for byte: version 1,
        ;; no clause at all
        (put! r18 (string->utf8
                    (string-append "((format 1) (tx \"t18\") (prefix 1 0 "
                                   (number->string end18) "))\n")))
        (delete-file (string-append d18 "/writers/" old18 "/uncertain.sexp"))
        (car (uncertain-load d18 old18)))
      (list (list old18 end18 #f)))
;; TWIN: THE SAME RECORD AT THE NEW VERSION, SAYING THE ADOPT LOST
;; NOTHING. Without this the row above is also passed by an
;; implementation that puts every retired writer in doubt for ever,
;; which would refuse every request on every store that has adopted.
(want "TWIN: a format 2 record with an empty clause is a claim, not a silence"
      (begin
        (put! r18 (string->utf8
                    (string-append "((format 2) (tx \"t18\") (prefix 1 0 "
                                   (number->string end18) ") (uncertain ()))\n")))
        (car (uncertain-load d18 old18)))
      '())
(want "the identity stretch survives the same deletion"
      (begin
        (delete-file (string-append copy8 "/writers/" new8 "/uncertain.sexp"))
        (uncertain-load copy8 new8))
      (list (list (list new8 end8 #f)) (list 'uncertain-cache 'absent)))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "adopt1 complete\n")
