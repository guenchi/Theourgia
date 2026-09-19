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

(import (chezscheme) (theourgia rpc) (theourgia working)
        (theourgia store) (theourgia reduce) (theourgia ffi))
(define failures 0)
(define (want name actual expected)
  (if (equal? actual expected) (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/working-fault-" (number->string (get-process-id))))
;; A ROOT WITH A LINE BREAK IN IT IS REFUSED, and only that.
;;
;; THE REASON IS SPECIFIC, so the rule is too. This fixture reads the
;; child's trace by splitting it on newlines and recovers the
;; temporary's path from one of those lines; the trace prints paths
;; unescaped, so a root containing a newline makes that path
;; unrecoverable and the run dies saying the child created no temporary,
;; which is a sentence about the store. Everything else a path can hold
;; survives: the shell arguments here are quoted, and the trace is
;; compared as text rather than read back as Scheme -- spaces, quotes,
;; colons and parentheses were all measured round-tripping intact.
;; (q7.ss refuses much more, for a different reason: it builds shell
;; commands that are NOT quoted.)
(let loop ((i 0))
  (when (< i (string-length root))
    (when (memv (string-ref root i) '(#\newline #\return))
      (assertion-violation 'working2
        "THEOURGIA_TEST_ROOT may not contain a line break: the child's trace is read by lines"
        root))
    (loop (+ i 1))))
(when (file-exists? root) (error 'working2 "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))

;; ⭐ THE WRITER IS NAMED HERE BECAUSE IT IS NO LONGER GUESSED. A draft
;; verb that was not told which writer it speaks for used to fall back to
;; this store's own local log writer, so two agents that never passed
;; `--writer` shared one draft space without either being told. The core
;; now refuses that call instead; naming the same writer the old fallback
;; would have chosen keeps every row below asking what it asked before.
;;
;; ⛔ AND ONLY WHERE IT WAS MISSING: a call that already names a writer is
;; naming it to make a point, and must keep the one it names.
(define draft-verbs '(write restore drafts discard commit))

(define (wants-writer? verb args)
  (or (memq verb draft-verbs)
      (and (eq? verb 'read)
           (or (member "--working" args) (member "--working-info" args)))))

(define (call . args)
  (rpc-dispatch store
                (if (and (wants-writer? (car args) (cdr args))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define made (call 'insert "--title" "A" "--text" "old"))
(define ev (car (cadr (assq 'events (cdr made)))))
(define id (block-id (car ev) (cdr ev)))
(define dir (string-append store "/writers/" writer "/working"))
(define file (string-append dir "/" id))
(define (read-bytes p) (call-with-port (open-file-input-port p) get-bytevector-all))
(define binary (bytevector 255 0 10 128))
;; ⭐ THE LIBRARY IS CALLED DIRECTLY HERE, SO THE WRITER IS PASSED
;; DIRECTLY. `working-write!` takes it third, where this used to pass #f
;; and be given the store's local writer; it now refuses an unnamed
;; writer, and these two rows are about BYTES, not about identity.
(want "WS-15 arbitrary bytes are durable in one envelope"
      (rpc-ok? (working-write! store #f writer id binary #f)) #t)
(want "WS-15 binary bytes survive reopening"
      (working-read store #f writer id)
      (list 'ok (list 'bytes binary)))
(want "WS-15 a non-text draft is not silently converted on commit"
      (car (call 'commit id)) 'error)
(call 'write id "saved")
(define original (read-bytes file))
(define (draft-blocks answer)
  (if (and (rpc-ok? answer) (pair? (cdr answer)) (pair? (cadr answer)))
      (map (lambda (d) (cadr (assq 'block (cdr d)))) (cdr (cadr answer)))
      answer))
(define (draft-versions answer)
  (if (and (rpc-ok? answer) (pair? (cdr answer)) (pair? (cadr answer)))
      (map (lambda (d) (cadr (assq 'version (cdr d)))) (cdr (cadr answer)))
      answer))
(define versions-before (draft-versions (call 'drafts)))
(define (shell-quote s)
  (string-append "'" (apply string-append
    (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
;; THE FAULT IS SET FOR THE CHILD, AND SO IS THE GATE THAT COMPILES IT
;; IN. `THEOURGIA_INJECT` is read when `(theourgia ffi)` is EXPANDED, so
;; a fault named in the environment of a process that was expanded
;; without it is a no-op. Measured, on a store of this shape: with
;; `THEOURGIA_INJECT` unset the child answers `(ok (saved ...))` and
;; exits 1; with it set to `on` the child prints
;; `fault-injection-armed`, answers `(error working-unavailable ...)`
;; and exits 0. So without the gate the first row below read
;; `child-status` = 1 and the second saw the envelope replaced -- two
;; reds whose sentence is "the product replaced the envelope after a
;; failed fsync", about a fault that never happened. Both variables go
;; to the child, in its command, the way the other fault fixtures here
;; write it.
;;
;; AND THE PARENT'S OWN ENVIRONMENT IS LEFT ALONE. `putenv` here reached
;; a process whose libraries were already expanded, so it changed
;; nothing; naming the fault where it is read is the whole fix.
(define fault (string-append "fsync-fail@working:file=" id))
(define child-script
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (p (string-append dir "/working-fault-child.ss")))
    (if (file-exists? p) p
        (assertion-violation 'working2
          "working-fault-child.ss is not beside this fixture" p))))
;; AND THE RUN IS READ, NOT ONLY ITS EXIT STATUS. `working-unavailable`
;; is what `working.ss` answers for EVERY exception on that path,
;; including one raised before a temporary is ever created -- and such a
;; failure also leaves the old envelope alone and leaves one draft
;; standing, so the three rows after it agree with it. The exit status
;; therefore cannot say that the write reached the flush this fixture is
;; about. The trace can: with the fault aimed at this block the child
;; creates the temporary, writes it, and stops -- no rename. With the
;; fault aimed at a block that is never written, the same child renames
;; the temporary into place and answers ok. Both readings are taken
;; below, so the rows are about the path and not only about the answer.
(define trace-path (string-append root "/child-trace.txt"))
(define (run-child-on! block spec)
  (let ((status
          (system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_FAULT="
                                (shell-quote spec) " "
                                "scheme --script " (shell-quote child-script) " "
                                (shell-quote store) " " (shell-quote block)
                                " " (shell-quote writer)
                                " > " (shell-quote trace-path) " 2>&1"))))
    (cons status (utf8->string (read-bytes trace-path)))))
(define (run-child! spec) (run-child-on! id spec))

;; THE TRACE IS READ AS WHOLE LINES ABOUT ONE NAMED FILE. Two earlier
;; versions matched text instead of events, and every loosening was
;; reachable: a substring search for `file=w.1` matched an announcement
;; for `file=w.10`; a prefix ending `/w.1.tmp-` matched
;; `/w.1.tmp-123-1/child`; and a rename test anchored only on the event
;; type matched a line whose SOURCE path happened to contain
;; `" . <target>)"`. So the temporary's exact name is taken from the one
;; event that introduces it, and every other row compares a whole line.
(define (lines-of text)
  (let loop ((i 0) (start 0) (out '()))
    (cond
      ((>= i (string-length text))
       (reverse (if (> i start) (cons (substring text start i) out) out)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
      (else (loop (+ i 1) start out)))))
(define (starts-with? line prefix)
  (and (>= (string-length line) (string-length prefix))
       (string=? prefix (substring line 0 (string-length prefix)))))
(define (has-line? lines exact) (exists (lambda (l) (string=? l exact)) lines))
(define working-dir (string-append store "/writers/" writer "/working"))
(define tmp-prefix (string-append working-dir "/" id ".tmp-"))
;; THE NAME COMES OUT OF THE `create` EVENT, whose line is
;; `(trace create <path> #f)` -- so the path is everything between the
;; fixed head and the fixed tail, and no guessing is involved.
(define (temporary-in* lines block)
  (let* ((head "(trace create ") (tail " #f)")
         (tmp-prefix (string-append working-dir "/" block ".tmp-"))
         (hit (exists (lambda (l)
                        (and (starts-with? l (string-append head tmp-prefix))
                             (let ((n (string-length l)))
                               (and (> n (+ (string-length head) (string-length tail)))
                                    (string=? tail (substring l (- n (string-length tail)) n))
                                    (substring l (string-length head)
                                               (- n (string-length tail)))))))
                      lines)))
    (or hit
        (assertion-violation 'working2
          "the child created no temporary for this block" tmp-prefix))))
(define (temporary-in lines) (temporary-in* lines id))
(define child-run (run-child! fault))
(define child-status (car child-run))
(define child-lines (lines-of (cdr child-run)))
(define tmp (temporary-in child-lines))
(want "WS-15 failed fsync never answers saved" child-status 0)
(want "WS-15 the injected failure was armed for this block"
      (has-line? child-lines
                 (string-append "(theourgia fault-injection-armed " fault ")")) #t)
;; FOUR EVENTS ABOUT ONE FILE, AND THE SET OF THEM IS THE STATEMENT.
;; `working.ss` answers `working-unavailable` for every exception on this
;; path, so the answer cannot say which step raised. The trace can, but
;; only taken together -- measured, one fault per run, on a store of this
;; shape:
;;
;;   write-eio-first          create, unlink            no write
;;   write-eio-after-partial  create, write 7, unlink   no fsync
;;   fsync-fail               create, write 170         no fsync, no
;;                                                      unlink, no rename
;;   no fault                 create, write, fsync, rename
;;
;; The `fsync` line is emitted only after the flush RETURNS, so its
;; absence is what separates a failure IN the flush from one after it --
;; a failing close or rename would leave it present. The absent unlink
;; separates it from a failing write, which cleans the temporary up.
(define (write-count lines path)
  ;; THE WHOLE LINE, AND THE COUNT OUT OF IT. A prefix test accepted
  ;; `(trace write <tmp> extra-path 7)`, which names another file, and
  ;; said nothing about how many bytes were written.
  ;;
  ;; AND ALL OF THEM ARE ADDED UP, because one write is not one event: a
  ;; short write is retried for the remainder and each successful round
  ;; is traced. Taking the first count read 7 where the file held 170,
  ;; which would have failed a healthy run that happened to be chunked.
  (let ((head (string-append "(trace write " path " ")))
    (let loop ((ls lines) (total #f))
      (cond
        ((null? ls) total)
        ((starts-with? (car ls) head)
         (let* ((l (car ls)) (n (string-length l)))
           (if (and (char=? #\) (string-ref l (- n 1)))
                    (let ((digits (substring l (string-length head) (- n 1))))
                      (and (> (string-length digits) 0)
                           (for-all char-numeric? (string->list digits)))))
               (loop (cdr ls)
                     (+ (or total 0)
                        (string->number (substring l (string-length head) (- n 1)))))
               (loop (cdr ls) total))))
        (else (loop (cdr ls) total))))))
(want "WS-15 the temporary was written, and the trace says how much"
      (and (write-count child-lines tmp) #t) #t)
;; AND THE TRACE AND THE DISK AGREE ABOUT IT. The count in the trace is
;; what the product says it wrote; the file's size is what is there.
(want "WS-15 and what the trace counted is what is on disk"
      (let ((n (write-count child-lines tmp)))
        (and n (= n (call-with-port (open-file-input-port tmp)
                      (lambda (port) (bytevector-length (get-bytevector-all port)))))))
      #t)
(want "WS-15 and the flush never returned, so no fsync was recorded for it"
      (has-line? child-lines (string-append "(trace fsync " tmp " #f)")) #f)
(want "WS-15 and the temporary was not cleaned up, as a failed write would be"
      (has-line? child-lines (string-append "(trace unlink " tmp " #f)")) #f)
(want "WS-15 and it was never renamed into place"
      (has-line? child-lines
                 (string-append "(trace rename (" tmp " . " working-dir "/" id ") #f)")) #f)
;; ⚠️ WHAT THIS STILL DOES NOT SEPARATE, written down rather than left
;; to be discovered: a partial write whose CLEANUP ALSO FAILED leaves the
;; same four readings, because `log.ss` swallows an unlink error and the
;; trace records an unlink only after the file is really gone. That takes
;; two faults at once, and this fixture arms one; with one fault the
;; readings above are measured to separate all three shapes. A row for
;; the two-fault case would need a fault device that can arm two points.
;;
;; AND THE TEMPORARY IS STILL THERE, which is a fact about the disk
;; rather than about what was printed.
(want "WS-15 and it is still on disk" (file-exists? tmp) #t)
(want "WS-15 failed temporary cannot replace the old envelope" (read-bytes file) original)
;; AND IT SAYS HOW MANY DRAFTS THERE ARE, not merely that the answer was
;; `ok`. The row's name is about an EXTRA draft appearing, and `(car
;; answer)` cannot see one: an answer listing both the surviving
;; envelope and the failed temporary is `ok` too, and printed `ok` for
;; this row when it was tried.
(want "WS-26 a failed temporary is not another draft"
      (let ((answer (call 'drafts))) (list (car answer) (draft-blocks answer)))
      (list 'ok (list id)))
;; AND IT IS THE SURVIVOR, not merely one entry with the right block id.
;; Counting and naming the block cannot tell the old envelope from a
;; failed temporary that took its place: both would list this block
;; once. The version is what separates them, and it was read before the
;; child ran.
(want "WS-26 and the draft standing is the one that was there before"
      (draft-versions (call 'drafts)) versions-before)
(want "WS-15 reopening reads the old complete value" (call 'read id "--working") '(ok (text "saved")))

;; TWIN: THE TWO TRACE ROWS ABOVE CAN BE FALSE. The same child, with the
;; same gate, and a fault aimed at a block this run never writes, goes
;; all the way through: it renames the temporary into place and answers
;; `ok`. Without this the two rows are also passed by a child that never
;; got as far as writing anything.
;; IT RUNS ON A SECOND BLOCK, so the subject of the rows above is left
;; exactly as they found it. A twin that wrote this block would leave a
;; new envelope -- same text, new version -- and the later row comparing
;; the envelope with `original` would fail for a reason that has nothing
;; to do with what it asserts.
(define other (let* ((made (call 'insert "--title" "B" "--text" "old"))
                     (ev (car (cadr (assq 'events (cdr made)))))) 
                (block-id (car ev) (cdr ev))))
(define elsewhere-run (run-child-on! other (string-append "fsync-fail@working:file=" id)))
(want "WS-15 TWIN: unaimed, the same child completes the rename"
      (let* ((ls (lines-of (cdr elsewhere-run))) (t (temporary-in* ls other)))
        (list (car elsewhere-run)
              (has-line? ls (string-append "(trace fsync " t " #f)"))
              (has-line? ls (string-append "(trace rename (" t " . " working-dir "/" other ") #f)"))))
      (list 1 #t #t))
(want "WS-15 TWIN: and it answers saved rather than unavailable"
      (exists (lambda (l) (starts-with? l "(ok (saved ")) (lines-of (cdr elsewhere-run))) #t)
;; THE TWIN'S OWN DRAFT IS REMOVED, AND THE REMOVAL IS CHECKED. `discard`
;; can answer `working-unavailable` like anything else on that path, and
;; a draft left standing here contaminates four later rows that expect
;; the draft list to be empty -- none of which mentions this block, so
;; the reading would look like a defect in what they are about.
(want "WS-15 TWIN: and its draft is removed again"
      (list (rpc-ok? (call 'discard other)) (draft-blocks (call 'drafts)))
      (list #t (list id)))

(define chmod (foreign-procedure "chmod" (string int) int))
(define getuid (foreign-procedure "getuid" () int))
(when (= 0 (getuid)) (error 'working2 "Permission evidence requires an ordinary user"))
(want "WS-16 deny actual retirement permission" (chmod dir #o555) 0)
(define committed (call 'commit id))
(want "WS-16 log commit succeeds when retirement fails" (rpc-ok? committed) #t)
(want "WS-16 the actual old envelope remains on disk" (read-bytes file) original)
(want "WS-16 leftover is not an active draft after reopen" (call 'drafts) '(ok (items)))
(call 'set id "src" "later-commit")
(want "WS-18 consumption is proved even after a later commit" (call 'drafts) '(ok (items)))
(want "WS-16 an unqualified repeat has nothing to append" (call 'commit) '(ok (items)))
(want "WS-16 restore writable test directory" (chmod dir #o755) 0)
(call 'write id "saved")
(want "WS-17 a later same-byte draft is still a draft"
      (let ((answer (call 'drafts)))
        (if (and (rpc-ok? answer) (pair? (cadr answer)))
            (length (cdr (cadr answer))) answer)) 1)
(want "WS-17 a new version is not the consumed envelope" (equal? (read-bytes file) original) #f)
(call-with-port (open-file-output-port file (file-options no-fail))
  (lambda (p) (put-bytevector p (string->utf8 "(broken"))))
(want "WS-26 corrupt storage is not absence" (cadr (call 'read id "--working")) 'working-unavailable)
(want "WS-26 corrupt storage is visible in the draft list" (cadr (call 'drafts)) 'working-unavailable)
;; ---- WS-27: the refusal belongs to the library, at every door ---------
;;
;; ⭐ ONE RULE, ONE PLACE. `rpc.ss` keeps no copy of this check: a caller
;; that reaches `(theourgia working)` directly -- the evaluator does --
;; must be refused by the same rule and told the same thing, or the
;; library is a second entry point with no guard on it (§7.6.50 v249).
;;
;; ⛔ ONE ROW WOULD NOT HAVE CAUGHT THIS. Eight entry points take a
;; writer and two of them, `working-snapshot` and `working-baseline`,
;; tested only `(not writer)`. `unbound` is a symbol and a symbol is
;; true, so both walked past the guard and used it AS a writer id: the
;; answer was `working-unavailable` carrying an unformatted
;; "~s is not a string". That is not a near miss --
;; `working-fault-child.ss` decides whether an injected durability fault
;; fired by looking for exactly `working-unavailable`, so an unnamed
;; writer and a failed flush were the same answer. Both are reachable
;; from `cli.ss` (`eval --working` with no `--writer`).
;;
;; So there is a row per door, and they are not redundant: each one is
;; the only row that would go red if its own door lost the branch.

;; ⚠️ ON A STORE OF ITS OWN. These rows sit at the end of the file, and
;; by here WS-26 has deliberately corrupted the store it shares -- which
;; the eight refusals above survive (the writer is judged before the
;; drafts are touched, §7.6.50 v249), but the positive twin cannot: it
;; asks whether a NAMED writer still gets through, and against a corrupt
;; store nothing does. A fresh store makes the block independent of where
;; in the file it sits.
(define ws27-store (string-append root "/ws27"))
(define ws27-init (rpc-dispatch ws27-store '(init) "test"))
(define ws27-writer (cadr (assq 'writer (cdr ws27-init))))
(define ws27-id
  (let* ((a (rpc-dispatch ws27-store '(insert "--title" "A" "--text" "old") "test"))
         (ev (car (cadr (assq 'events (cdr a))))))
    (block-id (car ev) (cdr ev))))

(define (refusal thunk)
  (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                  (condition-message e) e))))
    (let ((a (thunk))) (if (pair? a) (list (car a) (cadr a)) a))))

(define w-version
  (let ((a (working-snapshot ws27-store #f ws27-writer)))
    (if (and (pair? a) (eq? 'ok (car a)) (pair? (caddr a)) (pair? (car (caddr a))))
        (cadr (car (caddr a)))
        "no-such-version")))

(want "WS-27 working-write! refuses an unnamed writer"
      (refusal (lambda () (working-write! ws27-store #f #f ws27-id (string->utf8 "x") #f)))
      '(error writer-required))
(want "WS-27 working-read refuses an unnamed writer"
      (refusal (lambda () (working-read ws27-store #f #f ws27-id)))
      '(error writer-required))
(want "WS-27 working-restore! refuses an unnamed writer"
      (refusal (lambda () (working-restore! ws27-store #f #f w-version)))
      '(error writer-required))
(want "WS-27 working-discard! refuses an unnamed writer"
      (refusal (lambda () (working-discard! ws27-store #f ws27-id)))
      '(error writer-required))
(want "WS-27 working-list refuses an unnamed writer"
      (refusal (lambda () (working-list ws27-store #f #f)))
      '(error writer-required))
(want "WS-27 working-commit! refuses an unnamed writer"
      (refusal (lambda () (working-commit! ws27-store #f (list ws27-id) "test" #f)))
      '(error writer-required))
;; ⭐ THE TWO THAT WERE WRONG. Named apart from the six above because
;; they are the reason the other six are written out one by one.
(want "WS-27 working-snapshot refuses an unnamed writer"
      (refusal (lambda () (working-snapshot ws27-store #f #f)))
      '(error writer-required))
(want "WS-27 working-baseline refuses an unnamed writer"
      (refusal (lambda () (working-baseline ws27-store #f #f)))
      '(error writer-required))

;; ⛔ AND THE REFUSAL IS NOT MERELY "SOME ERROR". `working-unavailable`
;; is what these two used to answer, and it is the value a durability
;; fault reports; a row that accepted any refusal would have passed
;; against the defect it exists for.
(want "WS-27 TWIN: an unnamed writer is not reported as a storage failure"
      (let ((a (working-snapshot ws27-store #f #f)))
        (if (and (pair? a) (eq? (cadr a) 'working-unavailable)) 'CONFUSED-WITH-A-FAULT 'distinct))
      'distinct)

;; TWIN: the same doors, given a writer, still work. Without this the
;; eight rows above would pass against a library that refused everything.
(want "WS-27 TWIN: and a named writer still gets through"
      (list (rpc-ok? (working-list ws27-store #f ws27-writer))
            (rpc-ok? (working-snapshot ws27-store #f ws27-writer))
            (rpc-ok? (working-baseline ws27-store #f ws27-writer)))
      '(#t #t #t))

;; ---- WS-28: the writer is judged before the store is touched -------------
;;
;; ⛔ THE ORDER CHANGES THE ANSWER, not just the cost. The entry points
;; bound `writer` and `state` in one `let`, so the store was opened and
;; folded first and the missing writer noticed afterwards. On a store that
;; cannot be opened, an unnamed writer was therefore told
;; `working-unavailable` -- the value a durability fault reports, and the
;; one `working-fault-child.ss` keys its exit status on. A caller-fixable
;; mistake was reported as a storage failure.
;;
;; ⚠️ THE TWIN IS WHAT KEEPS THE FIX HONEST: a NAMED writer on the same
;; unopenable store must still be told the storage failed. Without it,
;; answering `writer-required` for everything would pass.
(define ws28-store (string-append root "/ws28"))
(rpc-dispatch ws28-store '(init) "test")
(system (string-append "chmod 000 " ws28-store "/writers"))

(want "WS-28 an unnamed writer is refused even when the store will not open"
      (let ((a (working-list ws28-store #f #f)))
        (if (pair? a) (list (car a) (cadr a)) a))
      '(error writer-required))

(want "WS-28 TWIN: a named writer on the same store still reports the storage failure"
      (let ((a (working-list ws28-store #f "w1")))
        (if (pair? a) (list (car a) (cadr a)) a))
      '(error working-unavailable))

;; ⛔ AND THE SAME FOR ARGUMENT SHAPES. A block id is well formed or it
;; is not, and the store has nothing to say about it -- but the check sat
;; after `open-and-reduce`, so on a store that cannot be opened a
;; malformed id was answered `working-unavailable` too. The caller's
;; mistake, reported as a storage failure.
(want "WS-28 a malformed block id is refused even when the store will not open"
      (let ((a (working-write! ws28-store #f "w1" "not a valid id!!" (string->utf8 "x") #f)))
        (if (pair? a) (list (car a) (cadr a)) a))
      '(error bad-request))

;; ⚠️ `read` IS NOT IN THIS ROW AND THAT IS DELIBERATE. Its answer for an
;; unknown id is `unknown-id`, which is a fact about the store's contents
;; and cannot be known without opening it -- so `working-unavailable`
;; there is the true answer, not a misreport. Named so the difference is
;; a decision rather than an oversight.

(system (string-append "chmod 755 " ws28-store "/writers"))

(printf "~a failures\nworking2 complete\n" failures)
(exit (if (= failures 0) 0 1))
