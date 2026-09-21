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

;; What the command line loads before it knows what it was asked to do.
;;
;; KEY: EVERY `read` USED TO LOAD THE ACTOR SYSTEM, for the benefit of
;; three verbs that were not being run. `cli.sc` imported
;; `(theourgia sched)`, `(theourgia net)`, `(theourgia daemon)` and
;; `(theourgia eval-supervise)` unconditionally; measured against the
;; same libraries, that was 652ms to start against 452ms without them,
;; and the reader this program documents itself for is an agent, which
;; makes one call after another.
;;
;; NOTE: THE STRUCTURAL ROW AND THE TIMING ROW ARE BOTH NEEDED, and neither
;; implies the other. A build could import them again and still be quick
;; on a fast morning; a build could keep them out and be slow for an
;; unrelated reason. One says what is loaded, the other says what it
;; costs.

(import (chezscheme))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define script-dir
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring argv0 0 cut) ".")))
(load (string-append script-dir "/import-walk.sc"))

(define root (string-append script-dir "/.."))
(define (source-of name) (string-append root "/" (symbol->string name) ".sc"))

;; ---- F0-1: what the static closure holds --------------------------------------
;;
;; NEVER: THE SAME WALKER `daemon-link-gate.sc` USES, and transitive for the
;; same reason: a library two imports away is loaded just as surely as
;; one written at the top of the file.
;; NEVER: AN IMPORT THIS WALK CANNOT RESOLVE IS REPORTED, NOT DROPPED. The
;; filter here used to be `(filter file-exists? ...)`, which silently
;; discarded any import whose source file it could not find -- so a
;; library that was renamed or moved would simply leave the closure, and
;; the row below asserting the closure excludes four names would go green
;; because it excludes everything. A walker that quietly stops matching
;; produces exactly the clean result the row is looking for.
(define unresolved-imports '())

(define (closure-of path)
  (let walk ((todo (list path)) (seen '()))
    (cond
      ((null? todo) seen)
      ((member (car todo) seen) (walk (cdr todo) seen))
      (else
       (let* ((here (car todo))
              (libs (imports-of-file 'theourgia here))
              (paths (map (lambda (l) (source-of (cadr l))) libs))
              (next (filter file-exists? paths)))
         (for-each (lambda (p)
                     (unless (file-exists? p)
                       (set! unresolved-imports (cons (cons here p) unresolved-imports))))
                   paths)
         (walk (append next (cdr todo)) (cons here seen)))))))

(define (basename path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) path)
          ((char=? (string-ref path i) #\/) (substring path (+ i 1) (string-length path)))
          (else (loop (- i 1))))))

(define heavy '("sched.sc" "net.sc" "daemon.sc" "eval-supervise.sc"))

(define cli-closure (map basename (closure-of (string-append root "/cli.sc"))))

(want "F0-1 the command line's static closure holds none of the four heavy libraries"
      (filter (lambda (h) (member h cli-closure)) heavy)
      '())

;; NEVER: AND THE WALKER CAN SEE THEM WHEN THEY ARE THERE. Without this row
;; the one above is satisfied by a walker that finds nothing at all --
;; which is also what a changed import spelling would produce.
(define daemon-closure (map basename (closure-of (string-append root "/daemon.sc"))))

(want "F0-1 every import in both closures resolved to a file"
      unresolved-imports
      '())

(want "F0-1 CONTROL: the same walk finds those libraries in the daemon's own closure"
      (list (and (member "sched.sc" daemon-closure) #t)
            (and (member "net.sc" daemon-closure) #t)
            (> (length cli-closure) 5))
      '(#t #t #t))

;; ---- F0-2: what it costs ------------------------------------------------------
;;
;; NOTE: THE READING, THE MACHINE AND THE VERSION, because a duration is
;; not a fact about a program on its own.
;;
;;   measured 2026-09-18, Darwin 25.3.0 arm64, Chez 10.1, this tree:
;;     theourgia 583d9ba (before the heavy imports)   465 ms/call
;;     theourgia 3017e45 (with them)                  691 ms/call
;;     this tree (loading them on demand)             454 ms/call
;;
;; NEVER: MEASURED WITH A VERB THAT DOES NOT EXIST, so no store is opened and
;; nothing but the program's own loading is timed.
;;
;; ---- the budget was raised from 495 to 540, and here is the reading -------
;;
;; It went red on two deliveries in a row. The question was whether segment B1
;; had made startup slower -- `store.sc` grew by about five hundred lines and
;; it is in the command line's static closure -- so that was measured instead
;; of argued. Two runs back to back on the same quiet machine, 2026-09-21:
;;
;;   the tree BEFORE segment B1 (6c5c881, git archive)   median 497   RED
;;     all (497 502 492 505 497 493 497 500 496 515)
;;   this tree, 502 lines longer                         median 491   green
;;
;; **The older tree misses the same budget, and the longer one is faster.**
;; Segment B1 did not raise this cost.
;;
;; And four readings of the SAME code say what this row actually measures:
;;
;;   the tree before B1, quiet machine                   497   red
;;   this tree, quiet machine                            491   green
;;   this tree, inside a full suite run                  505   red
;;   this tree, in another session's isolated copy       511   red   (load 14)
;;
;; This row could not tell the two trees apart. It told two MINUTES apart. The
;; margin was smaller than the spread, so its colour was decided by what else
;; the machine happened to be doing.
;;
;; WHAT 540 STILL CATCHES, AND WHAT IT NO LONGER DOES. A quiet reading of this
;; tree is about 491 ms, so 540 leaves roughly ten percent of headroom: **a
;; regression that makes startup less than about nine percent slower will not
;; be seen here any more.** What it still catches is the shape this row was
;; built for -- the heavy libraries being pulled back into the static closure,
;; which cost 691 against 465 in the reading above, half as much again. A
;; number this row cannot see is not a number nobody should care about; it is
;; one this row is the wrong instrument for.
;;
;; THE RIGHT INSTRUMENT IS RELATIVE, and it is queued rather than built here:
;; F54 measures two arms in ONE run -- the command line against the tree
;; before the heavy imports -- and asserts a ratio, not milliseconds. An
;; absolute constant on a development machine that other work perturbs will
;; drift for ever, and a row that flickers is not a row: when it goes red
;; nobody believes it.
;;
;; Raised by the main session, 2026-09-21. The number was changed because
;; somebody looked and decided, which is the only way a budget is allowed to
;; move; the alternative on the table -- moving new code out of the static
;; closure -- was rejected because the measurement says there is nothing there
;; to move.
(define budget-ms 540)

(define (median xs)
  (let ((v (list-sort < xs)))
    (list-ref v (div (length v) 2))))

(define (one-call-ms)
  (let ((start (real-time)))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
              " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
              "scheme --script " root "/cli.sc no-such-verb --wire > /dev/null 2>&1"))
    (- (real-time) start)))

(define timings (let loop ((n 10) (out '())) (if (zero? n) out (loop (- n 1) (cons (one-call-ms) out)))))
(define observed (median timings))

(want "F0-2 starting the command line costs no more than the tree before the heavy imports"
      (if (<= observed budget-ms)
          'within-budget
          (list 'median observed 'budget budget-ms 'all timings))
      'within-budget)

(printf "   (F0-2 median ~a ms over 10 runs, budget ~a ms)\n" observed budget-ms)

;; ---- F0-3: and the three paths that DO need them still work -------------------
;;
;; NEVER: AN IMPLEMENTATION THAT SIMPLY DELETED THE IMPORTS WOULD PASS BOTH
;; ROWS ABOVE. These are the rows it fails.
(define scratch (string-append "/tmp/f0-" (number->string (get-process-id))))
(define store (string-append scratch "/store"))
;; NOTE: SHORT ON PURPOSE. `sun_path` holds 104 bytes; a socket under this
;; suite's usual scratch directory is longer than that, and the daemon
;; then reports `listener-down` and exits -- which reads exactly like a
;; daemon that cannot start. Measured while writing this file, against
;; an UNMODIFIED core, which is how it was told apart from a defect.
(define socket (string-append "/tmp/f0s-" (number->string (get-process-id)) ".sock"))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (sh cmd) (system cmd))
(define (cli-out args out-file . env)
  (sh (string-append
        (if (pair? env) (car env) "")
        " CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
        " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
        "scheme --script " root "/cli.sc " args " > " out-file " 2>&1"))
  (let ((t (call-with-input-file out-file get-string-all))) (if (string? t) t "")))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(sh (string-append "rm -rf " scratch "; mkdir -p " store "; rm -f " socket))
(cli-out (string-append "init --store " store " --wire") (string-append scratch "/init.txt")
         "THEOURGIA_LOCAL=1")

(want "F0-3 eval still answers, so its supervisor was loaded when it was needed"
      (if (contains? (cli-out (string-append "eval '(+ 1 2)' --store " store " --wire")
                              (string-append scratch "/eval.txt") "THEOURGIA_LOCAL=1")
                     "(values (3))")
          'answered 'NOT-LOADED)
      'answered)

;; The daemon is started with tracing on, because the row below has to
;; say the answer came from IT and not from the local path -- which
;; produces the same bytes, deliberately.
(sh (string-append "( THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                   " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
                   "scheme --script " root "/cli.sc serve " store " --socket " socket
                   " > " scratch "/serve.txt 2>&1 & echo $! > " scratch "/serve.pid )"))
(sh "sleep 5")

(define serve-text (call-with-input-file (string-append scratch "/serve.txt") get-string-all))

(want "F0-3 serve still comes up, so the daemon was loaded when it was needed"
      (list (if (contains? (if (string? serve-text) serve-text "") "(serving ") 'serving 'NOT-LOADED)
            (if (file-exists? socket) 'socket-there 'NO-SOCKET))
      '(serving socket-there))

(define forwarded
  (cli-out (string-append "insert --title F0 --text hi --store " store
                          " --socket " socket " --wire")
           (string-append scratch "/fwd.txt")))

(sh "sleep 1")
(define serve-after (call-with-input-file (string-append scratch "/serve.txt") get-string-all))

;; NEVER: THE ANSWER ALONE PROVES NOTHING. A forwarded call and a local one
;; answer with the same bytes on purpose, so a row reading only the
;; answer passes when the socket was ignored and the work was done here.
;; NOTE: Measured while writing this: with a socket path too long to bind,
;; these same calls answered normally, having quietly run locally.
(want "F0-3 a call with a daemon present is answered BY the daemon, so net was loaded"
      (list (if (contains? forwarded "(ok (events") 'answered (list 'said forwarded))
            (if (contains? (if (string? serve-after) serve-after "") "daemon-dispatch")
                'the-daemon-dispatched-it
                'IT-RAN-LOCALLY))
      '(answered the-daemon-dispatched-it))

;; ---- F0-4: and the same thing in the form it ships in --------------------------
;;
;; KEY: EVERYTHING ABOVE WAS MEASURED FROM SOURCE, AND THAT IS NOT WHAT A
;; USER RUNS. Development runs `--script` against `.sc`; a user gets
;; compiled objects. The difference matters twice over.
;;
;; **First, lazy loading has to still work.** `cli.sc` resolves
;; `(theourgia daemon)` at RUN time, and whether that works depends on
;; the library being findable -- which it is as a `.so` on the library
;; path. NEVER: It would NOT be inside a whole-program package that dropped
;; it for being statically unreferenced; that form is F13 and has no
;; reading here. These rows cover the `.so` form only, and say so.
;;
;; **Second, the saving is much smaller there.** Same machine, same
;; objects, only `cli.sc` differing:
;;
;;     source form   static imports 623 ms   on demand 435 ms   -188 ms
;;     .so form      static imports  51 ms   on demand  42 ms   -9 ms
;;
;; NOTE: SO THE 200 ms WAS MOSTLY EXPANSION, which compiled objects do not
;; pay. In the form a user runs this saves about 9 ms a call, not 200 --
;; still 18% of startup, still worth having, and the structural point
;; stands on its own: a `read` has no business loading the daemon. But a
;; note claiming 200 ms for users would be false, and F0-2's budget is a
;; SOURCE-form budget.
;;
;; NOTE: Also worth knowing: compiled objects start about twelve times
;; faster than source either way. Most of what F0 attacks is a cost only
;; developers pay.

(define objects (string-append scratch "/objects"))

(define build-said
  (begin
    (sh (string-append "mkdir -p " objects))
    (sh (string-append
          "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
          " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
          "scheme --script " root "/build.ss " (getenv "CHEZSCHEMELIBDIRS") " " objects
          " > " scratch "/build.txt 2>&1"))
    (file-text (string-append scratch "/build.txt"))))

(want "F0-4 the tree compiles to objects at all"
      (if (contains? build-said "build complete") 'built (list 'said build-said))
      'built)

;; NEVER: `.so` ONLY ON THE PATH, so nothing can fall back to source and make
;; these rows describe the form they were meant to leave behind.
(define (so-cli args out-file . env)
  (sh (string-append
        (if (pair? env) (car env) "")
        " CHEZSCHEMELIBDIRS=" objects " CHEZSCHEMELIBEXTS='.so' "
        "scheme --script " root "/cli.sc " args " > " out-file " 2>&1"))
  (file-text out-file))

(define so-store (string-append scratch "/so-store"))
(sh (string-append "mkdir -p " so-store))

(want "F0-4 a verb runs from the objects, with no source reachable"
      (list (if (contains? (so-cli (string-append "init --store " so-store " --wire")
                                   (string-append scratch "/so-init.txt") "THEOURGIA_LOCAL=1")
                           "(ok (store")
                'initialised 'NO)
            (if (contains? (so-cli "no-such-verb --wire" (string-append scratch "/so-verb.txt"))
                           "unknown-verb")
                'and-refuses-what-it-should 'NO))
      '(initialised and-refuses-what-it-should))

;; KEY: THE ROW THIS SECTION EXISTS FOR. `eval` reaches its supervisor
;; through `(environment '(theourgia eval-supervise))` at run time; if
;; that cannot be resolved in this form the answer is an exception rather
;; than a value, and nothing above would have noticed.
(want "F0-4 eval loads its supervisor on demand from the objects"
      (if (contains? (so-cli (string-append "eval '(+ 1 2)' --store " so-store " --wire")
                             (string-append scratch "/so-eval.txt") "THEOURGIA_LOCAL=1")
                     "(values (3))")
          'answered (list 'said (file-text (string-append scratch "/so-eval.txt"))))
      'answered)

(define so-socket (string-append "/tmp/f0so-" (number->string (get-process-id)) ".sock"))
(sh (string-append "rm -f " so-socket))
(sh (string-append "( THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" objects " CHEZSCHEMELIBEXTS='.so' "
                   "scheme --script " root "/cli.sc serve " so-store " --socket " so-socket
                   " > " scratch "/so-serve.txt 2>&1 & echo $! > " scratch "/so-serve.pid )"))
(sh "sleep 5")

(want "F0-4 serve loads the daemon on demand from the objects"
      (list (if (contains? (file-text (string-append scratch "/so-serve.txt")) "(serving ") 'serving 'NOT-LOADED)
            (if (file-exists? so-socket) 'socket-there 'NO-SOCKET))
      '(serving socket-there))

(define so-forwarded
  (so-cli (string-append "insert --title F04 --text hi --store " so-store
                         " --socket " so-socket " --wire")
          (string-append scratch "/so-fwd.txt")))

(sh "sleep 1")

(want "F0-4 forwarding loads the transport on demand, and the daemon answers"
      (list (if (contains? so-forwarded "(ok (events") 'answered (list 'said so-forwarded))
            (if (contains? (file-text (string-append scratch "/so-serve.txt")) "daemon-dispatch")
                'the-daemon-dispatched-it 'IT-RAN-LOCALLY))
      '(answered the-daemon-dispatched-it))

(sh (string-append "kill $(cat " scratch "/so-serve.pid) 2>/dev/null; rm -f " so-socket))

;; ---- F18: the output directory has to be usable BY ITSELF -----------------
;;
;; KEY: EVERY ROW ABOVE RUNS THE PROGRAM FROM THE SOURCE TREE. They put only
;; objects on the library path, which is what proves the LIBRARIES resolve
;; from objects -- but the thing they invoke is `root/cli.sc`, a source
;; file. So none of them can say whether the output directory would work
;; on a machine that has no source tree, which is the only machine a user
;; has.
;;
;; Measured before `build.ss` copied the programs, with the output alone on
;; the path: `Exception in load: failed for <out>/theourgia/theourgia.sc: no
;; such file or directory`, rc=255. Putting only the thin client there moved
;; the same failure one step along to `cli.sc`. The programs are found BESIDE
;; THE PROGRAM, by path, so no quantity of `.so` substitutes for them.

(define (product-file rel) (string-append objects "/theourgia/" rel))

(want "F18 the build left every program in the output directory"
      (map (lambda (rel) (cons rel (if (file-exists? (product-file rel)) 'there 'MISSING)))
           '("theourgia.sc" "cli.sc" "eval-worker.sc" "mcp/server.sc"))
      '(("theourgia.sc" . there) ("cli.sc" . there)
        ("eval-worker.sc" . there) ("mcp/server.sc" . there)))

;; NEVER: THE LIBRARY PATH HOLDS THE OUTPUT AND NOTHING ELSE. With the source
;; tree also reachable this row would resolve the client from there and pass
;; whatever the output directory contained, which is the one thing it exists
;; to rule out.
(define (product-client args out-file)
  (sh (string-append
        "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" scratch "/f18home"
        " THEOURGIA_RUN=" scratch "/f18home/run"
        " CHEZSCHEMELIBDIRS=" objects " CHEZSCHEMELIBEXTS='.so' "
        "scheme --script " (product-file "theourgia.sc") " " args
        " > " out-file " 2>&1"))
  (file-text out-file))

(sh (string-append "mkdir -p " scratch "/f18home/run"))

(want "F18 the thin client runs from the output directory alone and answers the verb table"
      (if (contains? (product-client "describe --wire" (string-append scratch "/f18-desc.txt"))
                     "(ok (verbs (init ")
          'answered
          (list 'said (file-text (string-append scratch "/f18-desc.txt"))))
      'answered)

;; NEVER: AND THE ROW ABOVE HAS TO BE ABOUT THE COPIED PROGRAMS. Moving one
;; away and putting it back is the only thing here that shows the row is
;; reading them rather than something else that happens to be true.
(define f18-hidden (string-append scratch "/cli.sc.hidden"))
(sh (string-append "mv " (product-file "cli.sc") " " f18-hidden))
(define f18-without (product-client "describe --wire" (string-append scratch "/f18-without.txt")))
(sh (string-append "mv " f18-hidden " " (product-file "cli.sc")))

(want "F18 TWIN: with one program taken out of the output, the same call fails"
      (list (if (contains? f18-without "(ok (verbs (init ") 'STILL-ANSWERED 'refused)
            (if (contains? f18-without "cli.sc") 'and-names-the-missing-file 'SAID-SOMETHING-ELSE))
      '(refused and-names-the-missing-file))

(want "F18 TWIN: and putting it back restores the answer"
      (if (contains? (product-client "describe --wire" (string-append scratch "/f18-again.txt"))
                     "(ok (verbs (init ")
          'answered
          (list 'said (file-text (string-append scratch "/f18-again.txt"))))
      'answered)

(sh (string-append "kill $(cat " scratch "/serve.pid) 2>/dev/null; sleep 1; rm -rf " scratch "; rm -f " socket))

(printf "rows: ~a\n~a failures\nf0-ondemand complete\n" rows bad)
(exit (if (zero? bad) 0 1))
