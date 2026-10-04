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
;; three verbs that were not being run. `core.sc` imported
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

(define cli-closure (map basename (closure-of (string-append root "/core.sc"))))

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
;;
;; ---- the budget was raised from 540 to 590 (F100a), and here is the reading
;;
;; F100a moved every filesystem access into ffi.sc (the door) and added the
;; mutation record there, and ffi.sc is in the command line's static
;; closure. Measured, two passes alternated back to back on one machine,
;; 2026-09-25, the same layout on both sides:
;;
;;   F0-2 median          1f7f9d9 (before F100a)   the F100a tree
;;     pass 1             521 (load 2.11)          541 (load 4.46)
;;     pass 2             518 (load 3.97)          538 (load 3.54)
;;
;; +20 ms in both passes, against a base spread of 3: real, not load. Where
;; it comes from, each library loaded alone (median of ten starts):
;;
;;                        1f7f9d9     tree     tree, the three new C
;;                                              bindings stubbed
;;     (theourgia ffi)    61 / 60     72.5 / 72     72 / 72
;;     (theourgia log)    212.5/207.5  229 / 230.5    --
;;
;; The new bindings cost nothing measurable; the rest is the door itself,
;; compiled from source at every start, and it cannot move out of ffi.sc:
;; the record has to load with the primitives that note into it. So the
;; budget moved, by this reading, to 590: today's tree read 540, and 590 is
;; 540 x 1.09, the headroom 540 had over the 491 quiet reading above.
;; NOTE: TODAY'S BASE READ 518 TO 521 AGAINST THE 491 RECORDED ABOVE, AND
;; IT IS THE BATCHES BETWEEN, NOT THE MACHINE. Measured the same evening, both
;; trees on igropyr 56ca0db (git archive), alternated:
;;
;;   F0-2 median          6c5c881 (the 491 tree)    1f7f9d9
;;     pass 1             490 (load 2.90)           520 (load 2.54)
;;     pass 2             484 (load 2.07)           513 (load 3.76)
;;
;; The 6c5c881 tree reads its recorded 491 today, so the ~30 ms between it
;; and 1f7f9d9 came in with the batches between the two (F46 onwards), not
;; from this machine; F54's first row takes this reading.
;; The relative instrument, F54, is the next item after F100a. Moved by the
;; main session (ruling F, 2026-09-25), the only way this budget may move.
;;
;; ---- moved again, from 590 to 636, for the facts an editor supplies ---------
;;
;; The supply interface adds verbs, a table check and the projection's map
;; and key to what every start compiles. Its facts' library loads on first
;; use, and what stays static is what the design keeps there; the user
;; accepted the measured cost, 2026-09-29. Measured on one machine, the
;; same command as this row, base 9f806bb against the tree, ten rounds of
;; ten starts alternated:
;;
;;   median of round medians   base 572   tree 583   +11 ms (1.9 %)
;;   per library, alone        rpc 559 -> 567, store 433 -> 436,
;;                             code-project 440 -> 442
;;
;; The record is archive/theourgia-f127-startup-2026-09-29/READINGS.md in
;; the workspace root repository. The budget follows this row's own rule:
;; today's tree reading times 1.09, 583 x 1.09 = 635.5, so 636.
;;
;; ---- moved again, from 636 to 637, for the platform numbers ---------------
;;
;; Every platform number ffi.sc uses now comes from a measured row, one per
;; platform, instead of a literal: the rows are compiled from source at
;; every start, and about sixty numbers that were constants are run-time
;; values, so ffi.sc's own compiled code grows (nothing folds, the fixnum
;; compares no longer inline). The user accepted the measured cost,
;; 2026-09-29; a compiled build does not pay it. Measured on one machine,
;; ten rounds alternated, base 64d92e0 against the tree:
;;
;;   this row's command        min 562 -> 576, median 571.5 -> 584
;;   (theourgia rpc) alone     min 555 -> 567, median 558.5 -> 573.5
;;   (theourgia ffi) alone     min 85 -> 96, the rows about 4 of it
;;
;; The record is archive/theourgia-f181-2026-09-29/measure-r1/ in the
;; workspace root repository. The budget follows this row's own rule:
;; today's tree reading times 1.09, 584 x 1.09 = 636.6, so 637.
(define budget-ms 637)

(define (median xs)
  (let ((v (list-sort < xs)))
    (list-ref v (div (length v) 2))))

(define (one-call-ms)
  (let ((start (real-time)))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
              " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
              "scheme --script " root "/core.sc no-such-verb --wire > /dev/null 2>&1"))
    (- (real-time) start)))

(define timings (let loop ((n 10) (out '())) (if (zero? n) out (loop (- n 1) (cons (one-call-ms) out)))))
(define observed (median timings))

(want "F0-2 starting the command line costs no more than the tree before the heavy imports"
      (if (<= observed budget-ms)
          'within-budget
          (list 'median observed 'budget budget-ms 'all timings))
      'within-budget)

(printf "   (F0-2 median ~a ms over 10 runs, budget ~a ms)\n" observed budget-ms)

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

;; ---- F0-3: and the three paths that DO need them still work -------------------
;;
;; NEVER: AN IMPLEMENTATION THAT SIMPLY DELETED THE IMPORTS WOULD PASS BOTH
;; ROWS ABOVE. These are the rows it fails.
(define scratch (string-append scratch-base "/f0-" (number->string (get-process-id))))
(define store (string-append scratch "/store"))
;; NOTE: SHORT ON PURPOSE. `sun_path` holds 104 bytes; a socket under this
;; suite's usual scratch directory is longer than that, and the daemon
;; then reports `listener-down` and exits -- which reads exactly like a
;; daemon that cannot start. Measured while writing this file, against
;; an UNMODIFIED core, which is how it was told apart from a defect.
(define socket (string-append socket-base "/f0s-" (number->string (get-process-id)) ".sock"))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (sh cmd) (system cmd))
(define (cli-out args out-file . env)
  (sh (string-append
        (if (pair? env) (car env) "")
        " CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
        " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
        "scheme --script " root "/core.sc " args " > " out-file " 2>&1"))
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
                   "scheme --script " root "/theourgiad.sc serve " store " --socket " socket
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
;; **First, lazy loading has to still work.** `theourgiad.sc` resolves
;; `(theourgia daemon)` at RUN time, once its arguments are checked, and
;; `core.sc` resolves `(theourgia eval-supervise)`, `(theourgia sched)` and
;; `(theourgia net)` the same way; whether that works depends on the
;; library being findable -- which it is as a `.so` on the library
;; path. NEVER: It would NOT be inside a whole-program package that dropped
;; it for being statically unreferenced; that form is F13 and has no
;; reading here. These rows cover the `.so` form only, and say so.
;;
;; **Second, the saving is much smaller there.** Same machine, same
;; objects, only `core.sc` differing:
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
        "scheme --script " root "/core.sc " args " > " out-file " 2>&1"))
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

(define so-socket (string-append socket-base "/f0so-" (number->string (get-process-id)) ".sock"))
(sh (string-append "rm -f " so-socket))
(sh (string-append "( THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" objects " CHEZSCHEMELIBEXTS='.so' "
                   "scheme --script " root "/theourgiad.sc serve " so-store " --socket " so-socket
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

;; ---- F0-5: every library entered on demand loads -----------------------------
;;
;; NEVER: THE PREFLIGHT DOES NOT LOAD THEM. expansion-branches.sc imports
;; the static libraries in a child under each value of THEOURGIA_INJECT;
;; a library entered on demand is reached by none of those imports, so an
;; unbound identifier in it passes the preflight and is met only when its
;; verb is dispatched. These rows load each one, in a child, in both
;; branches.
;;
;; THE LIST IS READ FROM THE PROGRAM, NOT WRITTEN HERE: every library a
;; shipped source hands, quoted, to `environment` or to the programs'
;; `later`, and the library of every extension verb's handler. The files
;; are listed and read by source-files and forms-of, import-walk.sc's,
;; which this file loads at its top for F0-1's walk; they are not copied.
(define (quoted-theourgia-lib x)
  (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x))
       (pair? (cadr x)) (eq? (car (cadr x)) 'theourgia)
       (cadr x)))
;; `environment` takes any number of import specs, so every quoted one is
;; read; the programs' `later` takes one, and then a name.
(define (entered-libs x)
  (cond ((not (pair? x)) '())
        ((and (eq? (car x) 'environment) (list? (cdr x)) (exists quoted-theourgia-lib (cdr x)))
         (append (filter values (map quoted-theourgia-lib (cdr x)))
                 (apply append (map entered-libs (cdr x)))))
        ((and (eq? (car x) 'later) (pair? (cdr x)) (quoted-theourgia-lib (cadr x)))
         => (lambda (lib) (cons lib (entered-libs (cddr x)))))
        (else (append (entered-libs (car x)) (entered-libs (cdr x))))))
(define (sources-in dir)
  (map (lambda (n) (string-append dir "/" n)) (source-files dir)))
(define on-demand
  (let loop ((ls (append (apply append (map (lambda (path) (apply append (map entered-libs (forms-of path))))
                                            (append (sources-in root) (sources-in (string-append root "/mcp")))))
                         (map (lambda (e) (car (list-ref e 7)))
                              (eval 'extension-verbs (environment '(theourgia extensions))))))
             (out '()))
    (cond ((null? ls) (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) out))
          ((member (car ls) out) (loop (cdr ls) out))
          (else (loop (cdr ls) (cons (car ls) out))))))

;; One child loads each library in LIBS, each under its own guard, and
;; prints one datum a library; -> the libraries that did not load, each
;; with what the child said, or (did-not-finish <text>). A library the
;; child never reported loaded is one that did not load, (<lib>
;; not-reported), so a transcript that holds only its last line is not
;; read as clean.
(define (unloaded libs inject extra-dir tag)
  (let ((src (string-append scratch "/ondemand-" tag ".sc"))
        (out (string-append scratch "/ondemand-" tag ".out")))
    (call-with-output-file src
      (lambda (p)
        (write '(import (chezscheme)) p)
        (write `(for-each
                  (lambda (lib)
                    (guard (e (#t (write (list lib 'failed
                                               (call-with-string-output-port
                                                 (lambda (q) (display-condition e q)))))
                                  (newline)))
                      (environment lib)
                      (write (list lib 'loaded))
                      (newline)))
                  ',libs) p)
        (write '(begin (write '(probe done)) (newline)) p))
      'truncate)
    (sh (string-append (if inject "THEOURGIA_INJECT=on " "env -u THEOURGIA_INJECT ")
                       "CHEZSCHEMELIBDIRS=" (if extra-dir (string-append extra-dir ":") "")
                       (getenv "CHEZSCHEMELIBDIRS")
                       " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                       " scheme --script " src " > " out " 2>&1"))
    (let ((said (guard (e (#t #f))
                  (call-with-input-file out
                    (lambda (p) (let loop ((acc '()))
                                  (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))))
      (if (and said (member '(probe done) said))
          (append (filter (lambda (x) (and (pair? x) (pair? (cdr x)) (eq? (cadr x) 'failed))) said)
                  (map (lambda (lib) (list lib 'not-reported))
                       (filter (lambda (lib)
                                 (not (exists (lambda (x) (and (pair? x) (equal? (car x) lib) (pair? (cdr x))))
                                              said)))
                               libs)))
          (list 'did-not-finish (file-text out))))))

(want "F0-5 CONTROL: the walk finds the libraries the programs and the dispatcher enter at run time"
      (map (lambda (l) (and (member l on-demand) #t))
           '((theourgia daemon) (theourgia eval-supervise) (theourgia sched) (theourgia net)
             (theourgia stream-client) (theourgia derived) (theourgia premises) (theourgia query)))
      '(#t #t #t #t #t #t #t #t))
(want "F0-5 every library entered on demand loads in a child, THEOURGIA_INJECT unset"
      (unloaded on-demand #f #f "plain")
      '())
(want "F0-5 every library entered on demand loads in a child, THEOURGIA_INJECT=on"
      (unloaded on-demand #t #f "inject")
      '())
;; TWIN: the child says when a library does not load. A library of this
;; scratch directory's own that names an unbound identifier is reported
;; failed, and the one beside it loaded, by the same probe: only the
;; broken one is in the answer, which also holds every library the child
;; did not report.
(define twin-dir (string-append scratch "/ondemand-twin"))
(sh (string-append "mkdir -p " twin-dir "/f0probe"))
(call-with-output-file (string-append twin-dir "/f0probe/broken.sc")
  (lambda (p) (write '(library (f0probe broken) (export f) (import (rnrs)) (define (f) (not-bound-anywhere))) p))
  'truncate)
(want "F0-5 TWIN: a library naming an unbound identifier is reported, by name, as not loaded"
      (let ((r (unloaded '((f0probe broken) (theourgia extensions)) #f twin-dir "twin")))
        (and (list? r)
             (list (map car r)
                   (and (pair? r) (contains? (caddr (car r)) "not-bound-anywhere")))))
      '(((f0probe broken)) #t))

;; ---- F18: the output directory has to be usable BY ITSELF -----------------
;;
;; KEY: EVERY ROW ABOVE RUNS THE PROGRAM FROM THE SOURCE TREE. They put only
;; objects on the library path, which is what proves the LIBRARIES resolve
;; from objects -- but the thing they invoke is `root/core.sc`, a source
;; file. So none of them can say whether the output directory would work
;; on a machine that has no source tree, which is the only machine a user
;; has.
;;
;; Measured before `build.ss` copied the programs, with the output alone on
;; the path: `Exception in load: failed for <out>/theourgia/theourgia.sc: no
;; such file or directory`, rc=255. Putting only the thin client there moved
;; the same failure one step along to the program it starts (then `core.sc`;
;; since F46 also `theourgiad.sc`). The programs are found BESIDE
;; THE PROGRAM, by path, so no quantity of `.so` substitutes for them.

(define (product-file rel) (string-append objects "/theourgia/" rel))

(want "F18 the build left every program in the output directory"
      (map (lambda (rel) (cons rel (if (file-exists? (product-file rel)) 'there 'MISSING)))
           '("theourgia.sc" "core.sc" "theourgiad.sc" "eval-worker.sc" "mcp/server.sc"))
      '(("theourgia.sc" . there) ("core.sc" . there) ("theourgiad.sc" . there)
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
(define f18-hidden (string-append scratch "/core.sc.hidden"))
(sh (string-append "mv " (product-file "core.sc") " " f18-hidden))
(define f18-without (product-client "describe --wire" (string-append scratch "/f18-without.txt")))
(sh (string-append "mv " f18-hidden " " (product-file "core.sc")))

(want "F18 TWIN: with one program taken out of the output, the same call fails"
      (list (if (contains? f18-without "(ok (verbs (init ") 'STILL-ANSWERED 'refused)
            (if (contains? f18-without "core.sc") 'and-names-the-missing-file 'SAID-SOMETHING-ELSE))
      '(refused and-names-the-missing-file))

(want "F18 TWIN: and putting it back restores the answer"
      (if (contains? (product-client "describe --wire" (string-append scratch "/f18-again.txt"))
                     "(ok (verbs (init ")
          'answered
          (list 'said (file-text (string-append scratch "/f18-again.txt"))))
      'answered)

;; ---- F46: the daemon program is copied, and the thin client starts it ------
;;
;; NEVER: THE OLD NAME IS GONE FROM THE OUTPUT, not merely joined by the new
;; ones. A build that copied `theourgiad.sc` and `core.sc` and left a stale
;; copy of the program they replaced beside them would pass the row above.
;; The old name is assembled, not spelled: entry-split.sc's F46-6 greps the
;; tree for it.
(let ((old (string-append "cli" ".sc")))
  (want "F18 the output directory holds theourgiad.sc and core.sc and not the program they replaced"
        (map (lambda (rel) (cons rel (if (file-exists? (product-file rel)) 'there 'absent)))
             (list "theourgiad.sc" "core.sc" old))
        (list '("theourgiad.sc" . there) '("core.sc" . there) (cons old 'absent))))

;; A REQUEST THAT FORWARDS starts the daemon program from beside the thin
;; client, which is the measurement F46-5's build.ss tripwire stands in for.
;; `init` is answered locally; `outline` then finds no daemon, and the thin
;; client starts `<output>/theourgiad.sc serve ...` and asks again. The run
;; root is under the socket base, short enough for a socket path.
(define f18-run (string-append socket-base "/f18r-" (number->string (get-process-id))))
(define f18-store (string-append scratch "/f18store"))
(define (product-client-forwarding args out-file)
  (sh (string-append
        "THEOURGIA_HOME=" scratch "/f18home THEOURGIA_RUN=" f18-run
        " CHEZSCHEMELIBDIRS=" objects " CHEZSCHEMELIBEXTS='.so' "
        "scheme --script " (product-file "theourgia.sc") " " args
        " < /dev/null > " out-file " 2>&1"))
  (file-text out-file))
(sh (string-append "mkdir -p " f18-run " " f18-store))
(product-client-forwarding (string-append "init --store " f18-store " --wire")
                           (string-append scratch "/f18-init.txt"))
(define f18-outline
  (product-client-forwarding (string-append "outline --store " f18-store " --wire")
                             (string-append scratch "/f18-outline.txt")))
;; The daemon's pid, found by the output directory's own path, which carries
;; this process's pid, so no other run's daemon can match it.
(define f18-daemon-pids
  (let* ((out (string-append scratch "/f18-ps.txt")))
    (sh (string-append "ps -ax -o pid= -o command= | grep -F '" (product-file "theourgiad.sc") " serve '"
                       " | grep -v grep | awk '{print $1}' > " out))
    (let ((t (file-text out)))
      (let loop ((p (open-string-input-port t)) (acc '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse acc) (loop p (if (integer? x) (cons x acc) acc))))))))

(want "F18 a forwarding call from the output directory starts theourgiad.sc from beside the thin client, and is answered"
      (list (if (contains? f18-outline "(ok ") 'answered (list 'said f18-outline))
            (length f18-daemon-pids))
      '(answered 1))

;; Stopped by the pid read above, and waited for.
(for-each (lambda (pid) (sh (string-append "kill " (number->string pid) " 2>/dev/null"))) f18-daemon-pids)
(sh "sleep 1")

(sh (string-append "kill $(cat " scratch "/serve.pid) 2>/dev/null; sleep 1; rm -rf " scratch " " f18-run "; rm -f " socket))

(printf "rows: ~a\n~a failures\nf0-ondemand complete\n" rows bad)
(exit (if (zero? bad) 0 1))
