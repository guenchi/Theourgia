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

;; W6-retire-race: RETIREMENT AND A NEW DRAFT, IN ONE WRITER.
;;
;; Retiring a draft is read-then-unlink: read the envelope, decide its
;; version was consumed, delete the file. A second process of the SAME
;; writer may rename a new draft into that name in between -- and then
;; the delete removes work nobody committed, silently.
;;
;; ⭐ THE SCHEDULE HAS TO OBSERVE THE BLOCKING, and that is the whole
;; reason this fixture is shaped the way it is. "Install v2 first, then
;; retire" would also leave v2 standing -- on a build with no lock at
;; all, because the compare would simply fail. The row that discriminates
;; is the one where the retiring process is INSIDE the lock, past its
;; comparison, and the writing process is seen to WAIT.
;;
;; ⚠️ EVERY WAIT IS BOUNDED. A fifo with no reader blocks forever, and
;; this suite has lost fifteen minutes a fixture to exactly that. The
;; dance runs under one alarm and kills its children on any exit.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia ffi) (only (theourgia log) writer-directory))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define cli (string-append script-dir "/../cli.ss"))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/retire-race-" (number->string (get-process-id))))
(when (file-exists? root) (error 'retire-race "Use a fresh test root" root))
(mkdir-p! root)
(define home (string-append root "/home"))
(mkdir-p! home)
(putenv "THEOURGIA_HOME" home)
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))
(define (call . args) (rpc-dispatch store args "test"))
(define A
  (let ((a (call 'insert "--title" "A" "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(call 'write A "v1 text")
(define v1 (list-ref (assq 'projection (cdr (call 'read A "--working-info"))) 4))
(define cursor
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (open-and-reduce store)))))))

(define fifo (string-append root "/gate"))
(define trace (string-append root "/retire.trace"))
(define wtrace (string-append root "/write.trace"))
(define report (string-append root "/report.txt"))
(define runner (string-append root "/run.sh"))

;; THE DANCE.
;;
;;   1. a child commits, which retires the draft; it is armed to stop
;;      INSIDE the lock, after it has decided the version is consumed;
;;   2. a second child writes a NEW draft into the same name; it must
;;      block on the lock, and the report records whether it had
;;      finished before the first was released;
;;   3. the first is released; both finish.
(call-with-port (open-file-output-port runner (file-options no-fail) 'block (native-transcoder))
  (lambda (p)
    (put-string p
      (string-append
        "#!/bin/sh\n"
        "rm -f '" fifo "' '" trace "' '" wtrace "'\n"
        "mkfifo '" fifo "'\n"
        "a= ; b=\n"
        "trap 'test -n \"$a\" && kill -9 $a 2>/dev/null; test -n \"$b\" && kill -9 $b 2>/dev/null' EXIT\n"
        "THEOURGIA_HOME='" home "' THEOURGIA_INJECT=on "
        "THEOURGIA_BARRIER=retire-locked:'" fifo "' THEOURGIA_TRACE=1 "
        "scheme --script '" cli "' commit '" A "' --req R1 --cursor '" cursor "' "
        "--working-version '" v1 "' --store '" store "' > /dev/null 2> '" trace "' &\n"
        "a=$!\n"
        "i=0\n"
        "while [ $i -lt 400000 ] && ! grep -q retire-locked '" trace "' 2>/dev/null; do i=$((i+1)); done\n"
        "grep -q retire-locked '" trace "' 2>/dev/null && echo 'retirer-inside yes' >> '" report "' || echo 'retirer-inside no' >> '" report "'\n"
        "THEOURGIA_HOME='" home "' THEOURGIA_TRACE=1 "
        "scheme --script '" cli "' write '" A "' 'v2 text' "
        "--store '" store "' > /dev/null 2> '" wtrace "' &\n"
        "b=$!\n"
        "j=0\n"
        "while [ $j -lt 300000 ]; do j=$((j+1)); done\n"
        "if grep -q lock-wait '" wtrace "' 2>/dev/null; then "
        "echo 'writer-waited-on-lock yes' >> '" report "'; "
        "else echo 'writer-waited-on-lock no' >> '" report "'; fi\n"
        "printf x > '" fifo "'\n"
        "wait $a 2>/dev/null; a=\n"
        "wait $b 2>/dev/null; b=\n"
        "echo done >> '" report "'\n"))))
(system (string-append "chmod +x '" runner "'"))
(system (string-append "perl -e 'alarm 180; exec @ARGV' sh '" runner "' > /dev/null 2>&1"))

(define lines
  (guard (e (#t '()))
    (call-with-input-file report
      (lambda (p)
        (let loop ((out '()))
          (let ((l (get-line p)))
            (if (eof-object? l) (reverse out) (loop (cons l out)))))))))

(want "W6-retire-race the dance ran to the end" (member "done" lines) '("done"))
(want "W6-retire-race the retiring process stopped inside the lock"
      (and (member "retirer-inside yes" lines) #t) #t)

;; ⭐ THE DISCRIMINATING ROW. A build with no lock lets the write finish
;; while the retirer sits at the barrier, and this reads `no`.
;; ⭐ THE READING IS THE LOCK'S OWN EVENT, NOT THE PROCESS'S LIVENESS.
;;
;; ⚠️ IT USED TO BE `kill -0`. A process that is alive may be waiting on
;; the lock, or doing anything else -- compiling, opening files, sleeping
;; on a slow disk. With the lock removed and the writer merely delayed
;; before installing, every row here could pass. `ffi.ss` emits
;; `lock-wait` when, and only when, a `flock` did not succeed
;; immediately; that event is the whole discriminating power of this
;; fixture.
(want "W6-retire-race TWIN: and the writing process waited ON THE LOCK"
      (and (member "writer-waited-on-lock yes" lines) #t) #t)

;; ---- and after both finish ------------------------------------------------

(want "W6-retire-race the commit went through"
      (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce store) A)))))
      "v1 text")
(want "W6-retire-race the new draft survives" (call 'read A "--working") '(ok (text "v2 text")))
(want "W6-retire-race and it is listed as a draft"
      (let ((a (call 'drafts))) (and (rpc-ok? a) (length (cdr (assq 'items (cdr a)))))) 1)

;; ---- the lock is not a draft, and cannot be deleted as one ---------------
;;
;; ⛔ IT USED TO BE `working/.lock`, AND `discard` TOOK IT. `safe-id?`
;; admits `.lock`, so a client could delete the file the two steps above
;; serialise on -- after which the next processes create and lock
;; DIFFERENT inodes and the race comes back with nothing to show for it.
;;
;; The file moved to the writer's own directory, where no block id can
;; name it, and `discard` now asks `block-name?` rather than `safe-id?`.

(want "the lock is not among the drafts"
      (file-exists? (string-append (writer-directory store writer) "/working/.lock")) #f)
(want "it is in the writer's directory"
      (file-exists? (string-append (writer-directory store writer) "/draft.lock")) #t)
(want "and discard refuses a name no block can have"
      (let ((a (call 'discard ".lock"))) (list (car a) (cadr a) (caddr a)))
      '(error bad-request invalid-block-id))
(want "TWIN: the lock file is still there afterwards"
      (file-exists? (string-append (writer-directory store writer) "/draft.lock")) #t)

;; ---- W6b THE COMPARISON, WHICH THE LOCK HIDES -----------------------------
;;
;; ⭐ THE DANCE ABOVE NEVER EXERCISES THE ENVELOPE COMPARISON. It arms
;; `retire-locked`, which fires INSIDE the lock and AFTER the compare has
;; already agreed, so the replacement draft cannot exist yet when the
;; compare runs: the writer is still waiting on the lock. Delete the
;; comparison and every row above stays green.
;;
;; The window the comparison is for opens earlier: `entries` is read at
;; the top of the commit, and the draft lock is taken at the very end --
;; after the store's write session has been released. A replacement that
;; lands in that gap is a draft the retirer never read. `before-retire`
;; stops the commit exactly there.
;;
;; ⚠️ IT CANNOT BE STOPPED ANY EARLIER THAN THAT, and the first attempt
;; here tried: armed at `before-append`, the commit still holds the
;; store's write session, so the second process blocked trying to read
;; the store and the two waited on each other until the alarm. The gap
;; that matters is the one where no store lock is held, which is also
;; the only gap where a replacement can appear.
;;
;; The feeder offers more bytes than the one stop needs and is killed
;; with the rest on exit; a fifo writer with no reader would otherwise
;; hold the script open past the alarm.

(define B
  (let ((a (call 'insert "--title" "B" "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(call 'write B "b1 text")
(define bfifo (string-append root "/gate2"))
(define btrace (string-append root "/retire2.trace"))
(define breport (string-append root "/report2.txt"))
(define brunner (string-append root "/run2.sh"))
(define bdraft (string-append (writer-directory store writer) "/working/" B))

(define (file-bytes path)
  (guard (e (#t #f))
    (and (file-exists? path)
         (call-with-port (open-file-input-port path) get-bytevector-all))))

(call-with-port (open-file-output-port brunner (file-options no-fail) 'block (native-transcoder))
  (lambda (p)
    (put-string p
      (string-append
        "#!/bin/sh\n"
        "rm -f '" bfifo "' '" btrace "'\n"
        "mkfifo '" bfifo "'\n"
        "a= ; b= ; f=\n"
        "trap 'for pid in $a $b $f; do kill -9 $pid 2>/dev/null; done' EXIT\n"
        "THEOURGIA_HOME='" home "' THEOURGIA_INJECT=on "
        "THEOURGIA_BARRIER=before-retire:'" bfifo "' THEOURGIA_TRACE=1 "
        "scheme --script '" cli "' commit '" B "' "
        "--store '" store "' > /dev/null 2> '" btrace "' &\n"
        "a=$!\n"
        "i=0\n"
        "while [ $i -lt 400000 ] && ! grep -q 'barrier before-retire' '" btrace "' 2>/dev/null; do i=$((i+1)); done\n"
        "grep -q 'barrier before-retire' '" btrace "' 2>/dev/null && echo 'retirer-paused yes' >> '" breport "' || echo 'retirer-paused no' >> '" breport "'\n"
        ;; THE REPLACEMENT, WHICH MUST FINISH WHILE THE OTHER IS STOPPED.
        ;; ⚠️ UNDER ITS OWN ALARM. It is the one step that can block --
        ;; on the draft lock, if the build being measured takes the lock
        ;; earlier than this one does -- and the controller's alarm
        ;; reaches the controller, not its children. Without this a
        ;; stuck replacement outlives the fixture holding a lock.
        "perl -e 'alarm 60; exec @ARGV' env THEOURGIA_HOME='" home "' "
        "scheme --script '" cli "' write '" B "' 'b2 text' "
        "--store '" store "' > /dev/null 2>&1\n"
        "echo \"replacement-rc $?\" >> '" breport "'\n"
        "md5 -q '" bdraft "' >> '" breport "' 2>/dev/null || echo no-draft >> '" breport "'\n"
        "sh -c 'for k in 1 2 3 4; do printf x > \"" bfifo "\"; done' &\n"
        "f=$!\n"
        ;; ⛔ AND THE RETIRER'S EXIT STATUS IS PART OF THE REPORT. A
        ;; child that DIES at the barrier leaves the commit durable, the
        ;; barrier line in the trace and the replacement standing -- all
        ;; of which read exactly like the comparison having worked. The
        ;; status separates the two, so it is written down rather than
        ;; discarded by `wait`.
        "wait $a 2>/dev/null; rc=$?; a=\n"
        "echo \"retirer-rc $rc\" >> '" breport "'\n"
        "echo done >> '" breport "'\n"))))
(system (string-append "chmod +x '" brunner "'"))
(system (string-append "perl -e 'alarm 180; exec @ARGV' sh '" brunner "' > /dev/null 2>&1"))

(define blines
  (guard (e (#t '()))
    (call-with-input-file breport
      (lambda (p)
        (let loop ((out '()))
          (let ((l (get-line p)))
            (if (eof-object? l) (reverse out) (loop (cons l out)))))))))

(want "W6b the dance ran to the end" (member "done" blines) '("done"))
(want "W6b the retiring process stopped before it took the draft lock"
      (and (member "retirer-paused yes" blines) #t) #t)
(want "W6b and the replacement was written while it was stopped"
      (and (member "replacement-rc 0" blines) #t) #t)
;; ⛔ AND THE RETIRER RAN TO THE END. Every row below is about what it
;; did NOT delete, and a process that died at the barrier deletes
;; nothing either.
(want "W6b TWIN: and the retiring process finished, rather than dying parked"
      (and (member "retirer-rc 0" blines) #t) #t)

;; ⭐ THE ROW THE COMPARISON OWNS. The retirer read `b1 text`; what is
;; in the slot is `b2 text`, written by a process it never saw. Without
;; the envelope comparison the unlink takes the replacement, and this
;; reads `#f`.
(want "W6b the replacement is still there afterwards" (file-exists? bdraft) #t)
;; ⚠️ THE DIGEST IS TAKEN AGAIN, NOT MERELY LOOKED FOR. An earlier
;; version of this row checked that a 32-character line was in the
;; report and that the file could still be read, which is true of any
;; file at all -- including one the retirer had deleted and the writer
;; had put back differently.
(system (string-append "md5 -q '" bdraft "' > '" root "/md5-after' 2>/dev/null"))
(define md5-after
  (guard (e (#t #f))
    (call-with-input-file (string-append root "/md5-after") get-line)))
(define md5-before (find (lambda (l) (= 32 (string-length l))) blines))
;; ⚠️ BOTH SIDES ARE REQUIRED TO BE DIGESTS INSIDE THE COMPARISON. A
;; version of this row compared `(list md5-after recorded)` against
;; `(list md5-after md5-after)`: with no digest in the report and none
;; readable now, both sides are `(#f #f)` and the row passes about two
;; absences.
(want "W6b and it is byte for byte the file the other process wrote"
      (list (and (string? md5-before) (string-length md5-before))
            (and (string? md5-after) (string-length md5-after))
            (equal? md5-before md5-after))
      '(32 32 #t))
(want "W6b reading it gives the replacement text"
      (call 'read B "--working") '(ok (text "b2 text")))
(want "W6b and the commit it raced still went through"
      (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce store) B)))))
      "b1 text")

(printf "rows: ~a\n~a failures\nretire-race complete\n" rows bad)
