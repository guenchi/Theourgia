#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; The registry of verbs added from outside the core table.
;;
;; WHAT IS ASKED IS THAT A REGISTERED VERB IS A VERB LIKE THE OTHERS: it
;; answers, describe lists it, the MCP shell offers it as a tool, the parser
;; knows its options -- and that registering costs nothing to a start that
;; answers another verb, and changes nothing about the built-in ones.
;;
;; THE BUILT-IN DESCRIBE ENTRIES ARE COMPARED WITH A READING OF THE BASE,
;; not with this tree's own literal: a row comparing the catalogue with
;; itself would pass whatever the registry did to it. The reading is
;; verb-registry-base.sexp, taken on the base commit it names.
(import (chezscheme) (theourgia rpc)
        (only (theourgia arguments) parse-arguments)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia client) socket-path))

(define bad 0)
(define rows 0)
;; THE VALUES A ROW READS ARE TAKEN IN THE ORDER THEY ARE WRITTEN.
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define (raised-message thunk)
  (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)
                      (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
    (thunk)
    'NOT-RAISED))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (guard (e (#t (list 'RAISED e))) got) expected))))

(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/verb-registry-" (number->string (get-process-id))))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/vr" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" root "/home' '" root "/lib/fixture' '" sock-root "/run'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(putenv "THEOURGIA_RUN" (string-append sock-root "/run"))
(define store (string-append root "/store"))
(define (env extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run " extra " "))
(define (sh-out name command)
  (let ((out (string-append root "/" name ".out")))
;; A command that names its own input keeps it: a second `<` would win.
    (system (string-append command " > '" out "' 2> '" root "/" name ".err'"
                           (if (string-contains? command " < ") "" " < /dev/null")))
    (file-text out)))
(define (write-file! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'replace))

;; ---- before registering: what the base has ---------------------------------------

(define base (call-with-input-file "verb-registry-base.sexp" read))
(define (base-of key) (cadr (assq key (cdr base))))

(rpc-dispatch store '(init) "test")
(define (run . args) (rpc-dispatch store args "test"))
(define representative '((conflicts) (search "no such text anywhere") (describe-verbs)))
(define (answer-of form)
  (if (equal? form '(describe-verbs))
      (map car (cdr (assq 'verbs (cdr (run 'describe)))))
      (apply run form)))
(define unregistered
  (list (car (run 'commitments)) (and (memq 'commitments (rpc-verbs)) #t)
        (and (assq 'commitments (cdr (assq 'verbs (cdr (run 'describe))))) #t)))

(want "V1 with nothing registered, commitments is an unknown verb and describe does not list it"
      (list (car unregistered) (cadr unregistered) (caddr unregistered)
            (let ((a (run 'commitments))) (and (pair? a) (pair? (cdr a)) (cadr a))))
      '(error #f #f unknown-verb))

;; ---- registering --------------------------------------------------------------------

;; A SECOND CLIENT OF THE REGISTRY, from a library this fixture writes: a
;; registry that worked for the one entry it was written for would pass
;; every row about `commitments`.
(write-file! (string-append root "/lib/fixture/probe.sc")
  "(library (fixture probe) (export probe-verb) (import (rnrs))
     (define (probe-verb store actor args req options state writer cwd)
       (list 'ok (list 'items (list 'probe (if (assoc \"--loud\" (map cdr (filter pair? options))) 'loud 'quiet) args)))))")
(library-directories (cons (string-append root "/lib") (library-directories)))
(define probe-entry
  (list 'probe '(probe ["--loud"]) "Answer for the registry fixture." #f 'daemon '() '("--loud")
        '((fixture probe) . probe-verb)))

(register-verbs! extension-verbs)
(register-verbs! (list probe-entry))
(define verbs-registered (rpc-verbs))

(want "V1 registered, commitments answers ok and describe lists it with its catalogue fields"
      (list (car (run 'commitments))
            (let ((e (assq 'commitments (cdr (assq 'verbs (cdr (run 'describe)))))))
              (and e (list (cadr (assq 'protocol (cdr e))) (cadr (assq 'route (cdr e)))))))
      '(ok (#f daemon)))
(want "V1 a second registered verb answers too, its flag parsed as a flag"
      (list (run 'probe) (run 'probe "--loud"))
      '((ok (items (probe quiet ()))) (ok (items (probe loud ())))))
(want "V1 every built-in verb's describe entry is the base's, byte for byte"
      (let ((now (filter (lambda (e) (not (memq (car e) '(commitments probe))))
                         (cdr (assq 'verbs (cdr (run 'describe)))))))
        (if (equal? now (base-of 'describe-entries)) 'identical
            (list 'differs (filter (lambda (e) (not (member e (base-of 'describe-entries)))) now))))
      'identical)
(want "V1 three representative built-ins answer as on the base"
      (map (lambda (form) (equal? (answer-of form)
                                  (if (equal? form '(describe-verbs))
                                      (append (base-of 'describe-verbs) '(commitments probe))
                                      (cadr (assoc form (base-of 'answers))))))
           representative)
      '(#t #t #t))

;; ---- V2 and the route: a refused batch leaves the registry as it was --------------

;; Each synthetic entry has a value option of its own, so that a batch
;; published in part would show in the parser as well as in the verb list.
(define (entry name route)
  (list name (list name '["--fresh" <x>]) "x" #f route '("--fresh") '() '((fixture probe) . probe-verb)))
(define (catalogue-now) (cdr (assq 'verbs (cdr (run 'describe)))))
(define catalogue-registered (catalogue-now))
(define (fresh-parse) (parse-arguments 'fresh-one '("--fresh" "x")))
;; THE THREE PROJECTIONS A BATCH PUBLISHES -- the verb list (dispatch), the
;; catalogue (describe) and the option tables (the parser) -- each compared
;; with what it was before the refused batch.
(define (refusal batch)
  (let ((r (raised-message (lambda () (register-verbs! batch)))))
    (list r
          (and (equal? (rpc-verbs) verbs-registered)
               (equal? (catalogue-now) catalogue-registered)
               (equal? (fresh-parse) '((pos "--fresh") (pos "x"))))
          (car (run 'fresh-one)))))

(want "V2 a batch naming a built-in verb is refused by name, its valid entry not installed"
      (refusal (list (entry 'fresh-one 'daemon) (entry 'read 'daemon)))
      '((RAISED "the name is a built-in verb" (read)) #t error))
(want "V2 one name twice within a batch is refused"
      (refusal (list (entry 'fresh-one 'daemon) (entry 'fresh-one 'daemon)))
      '((RAISED "the name is given twice in the batch" (fresh-one)) #t error))
(want "V2 a name an earlier batch registered is refused"
      (refusal (list (entry 'fresh-one 'daemon) (entry 'commitments 'daemon)))
      '((RAISED "the name is already registered" (commitments)) #t error))
(want "V2 a registered verb's route is daemon: local, child and another route are each refused, by name"
      (map (lambda (route) (let ((r (refusal (list (entry 'fresh-one route)))))
                             (list (car (car r)) (cadr (car r)) (equal? (caddr (car r)) (list (entry 'fresh-one route)))
                                   (cadr r) (caddr r))))
           '(local child remote))
      '((RAISED "a registered verb's route is daemon" #t #t error)
        (RAISED "a registered verb's route is daemon" #t #t error)
        (RAISED "a registered verb's route is daemon" #t #t error)))

(want "V2 CONTROL: a valid batch with the same entry does change all three"
      (begin (register-verbs! (list (entry 'fresh-one 'daemon)))
             (list (and (memq 'fresh-one (rpc-verbs)) #t)
                   (and (assq 'fresh-one (catalogue-now)) #t)
                   (fresh-parse)
                   (car (run 'fresh-one))))
      '(#t #t ((option "--fresh" "x")) ok))

;; ---- V3: the options, by node type ------------------------------------------------

(want "V3 --since and --under are value options, --open --all --drifted are flags"
      (let ((nodes (parse-arguments 'commitments '("--since" "((\"a\" . 1))" "--under" "x.1" "--open" "--all" "--drifted"))))
        (map (lambda (name) (let ((n (find (lambda (n) (and (pair? n) (pair? (cdr n)) (equal? (cadr n) name))) nodes)))
                              (and n (car n))))
             '("--since" "--under" "--open" "--all" "--drifted")))
      '(option option flag flag flag))
(want "V3 CONTROL: an option no table knows is a positional"
      (parse-arguments 'commitments '("--sideways"))
      '((pos "--sideways")))

;; ---- D1: the two sentences, in each place that carries the protocol ---------------

(define sentences
  '("Record a decision as a block of kind decision"
    "Open a session with commitments --open"))
(want "D1 the protocol constant carries both sentences"
      (map (lambda (s) (string-contains? write-protocol s)) sentences)
      '(#t #t))
(want "D1 README's section for agents carries both"
      (let ((t (file-text "../README.md"))) (map (lambda (s) (string-contains? t s)) sentences))
      '(#t #t))

;; ---- the MCP shell's tools, from a daemon that registered and one that did not -----

(define (tools-list name)
  (let ((in (string-append root "/" name "-in.jsonl")))
    (write-file! in (string-append
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                      "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}\n"
                      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                      "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}\n"))
    (sh-out name (string-append (env "") "scheme --script ../mcp/server.sc --store '" store "' < '" in "'"))))
(define (description-in listing tool)
  (let* ((key (string-append "\"name\":\"" tool "\",\"description\":\""))
         (n (string-length key))
         (at (let find ((i 0)) (cond ((> (+ i n) (string-length listing)) #f)
                                     ((string=? (substring listing i (+ i n)) key) (+ i n))
                                     (else (find (+ i 1)))))))
    (and at (let loop ((i at) (acc '()))
              (cond ((>= i (string-length listing)) #f)
                    ((char=? (string-ref listing i) #\\) (loop (+ i 2) (cons (string-ref listing (+ i 1)) acc)))
                    ((char=? (string-ref listing i) #\") (list->string (reverse acc)))
                    (else (loop (+ i 1) (cons (string-ref listing i) acc))))))))

(define with-registration (tools-list "tools"))
(system (string-append "pkill -f 'serve " store "' 2>/dev/null; sleep 1"))
(want "V1 the MCP shell offers commitments as a tool"
      (and (description-in with-registration "theourgia_commitments") #t)
      #t)
(want "D1 the MCP descriptions of the two writing tools carry both sentences"
      (map (lambda (tool)
             (let ((d (description-in with-registration tool)))
               (and d (map (lambda (s) (string-contains? d s)) sentences))))
           '("theourgia_insert" "theourgia_write"))
      '((#t #t) (#t #t)))

;; THE SAME SHELL AGAINST A DAEMON WHOSE REGISTRATION LINE IS GONE: a copy
;; of theourgiad.sc with that one line deleted, started on the socket the
;; shell will look for, so the shell finds it rather than starting its own.
(define mutant (string-append root "/mutant/theourgiad.sc"))
(system (string-append "mkdir -p '" root "/mutant'; grep -v \"'register-verbs!) extension-verbs\" ../theourgiad.sc > '" mutant "'"))
(define mutant-lines-removed
  (let ((f (string-append root "/count.txt")))
    (system (string-append "echo $(( $(wc -l < ../theourgiad.sc) - $(wc -l < '" mutant "') )) > '" f "'"))
    (string->number (let ((t (file-text f))) (substring t 0 (- (string-length t) 1))))))
(define sock (socket-path store))
(system (string-append "mkdir -p \"$(dirname '" sock "')\"; (" (env "") "scheme --script '" mutant "' serve '" store
                       "' --socket '" sock "' > '" root "/mutant.log' 2>&1 &)"))
(let wait ((k 0)) (unless (or (file-exists? sock) (> k 200)) (system "sleep 0.05") (wait (+ k 1))))
(define without-registration (tools-list "tools-unregistered"))
(system (string-append "pkill -f '" mutant "' 2>/dev/null; sleep 1"))
(want "V1 CONTROL: the copy differs from theourgiad.sc by exactly the registration line"
      mutant-lines-removed
      1)
(want "V1 without the registration line the shell offers no commitments tool, and still offers read"
      (list (and (description-in without-registration "theourgia_commitments") #t)
            (and (description-in without-registration "theourgia_read") #t))
      '(#f #t))

;; ---- A1: registering loads nothing ----------------------------------------------------
;;
;; A child process registers, answers one verb, and says whether the
;; commitments library is loaded; then answers `commitments` and says
;; again. The second reading is what shows the first could have said yes.
(define child (string-append root "/ondemand.sc"))
(write-file! child
  (string-append
    "(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))\n"
    "(register-verbs! extension-verbs)\n"
    "(define (loaded?) (and (member '(theourgia commitments) (library-list)) #t))\n"
    "(define store \"" store "\")\n"
    "(rpc-dispatch store '(outline) \"test\")\n"
    "(define after-outline (loaded?))\n"
    "(rpc-dispatch store '(commitments) \"test\")\n"
    "(write (list after-outline (loaded?)))\n"))
(want "A1 a start that registers and answers another verb has not loaded commitments; one that answers it has"
      (let ((t (sh-out "ondemand" (string-append (env "") "scheme --script '" child "'"))))
        (guard (e (#t (list 'UNREADABLE t))) (read (open-string-input-port t))))
      '(#f #t))

;; THE REAL ENTRY POINTS, read statically: no library either program imports,
;; directly or through another, is (theourgia commitments).
(define (import-specs form)
  (cond ((and (pair? form) (eq? (car form) 'import)) (cdr form))
        ((and (pair? form) (eq? (car form) 'library))
         (let ((i (find (lambda (x) (and (pair? x) (eq? (car x) 'import))) (cddr form))))
           (if i (cdr i) '())))
        (else '())))
(define (spec-library spec)
  (cond ((and (pair? spec) (memq (car spec) '(only except prefix rename)) (pair? (cdr spec))) (spec-library (cadr spec)))
        ((and (pair? spec) (eq? (car spec) 'theourgia) (pair? (cdr spec))) (cadr spec))
        (else #f)))
(define (theourgia-imports path)
  (guard (e (#t '()))
    (call-with-input-file path
      (lambda (p)
        (let loop ((acc '()))
          (let ((x (read p)))
            (if (eof-object? x) acc
                (loop (append acc (filter values (map spec-library (import-specs x))))))))))))
(define (closure-of path)
  (let walk ((todo (list path)) (seen '()))
    (cond ((null? todo) seen)
          ((member (car todo) seen) (walk (cdr todo) seen))
          (else (walk (append (cdr todo)
                              (map (lambda (n) (string-append "../" (symbol->string n) ".sc"))
                                   (theourgia-imports (car todo))))
                      (cons (car todo) seen))))))
(define core-closure (closure-of "../core.sc"))
(define daemon-closure (closure-of "../theourgiad.sc"))
(define client-closure (closure-of "../theourgia.sc"))
(want "A1 no program's static closure holds the commitments library; the two that register hold the registry's data"
      (map (lambda (c) (list (and (member "../commitments.sc" c) #t) (and (member "../extensions.sc" c) #t)
                             (> (length c) 3)))
           (list core-closure daemon-closure client-closure))
      '((#f #t #t) (#f #t #t) (#f #f #t)))

;; AND AT RUN TIME, through the programs themselves: a copy of the library
;; that writes a file when its body runs, first on the library path. An
;; import alone does not run a library's body; a handler entered on dispatch
;; does, and so would a registration that entered it.
(define marklib (string-append root "/marklib"))
(define marker (string-append root "/marker"))
(system (string-append "mkdir -p '" marklib "/theourgia'"))
(let* ((text (file-text "../commitments.sc"))
       (anchor "  (define (commitments-verb ")
       (at (let find ((i 0)) (cond ((> (+ i (string-length anchor)) (string-length text)) #f)
                                   ((string=? (substring text i (+ i (string-length anchor))) anchor) i)
                                   (else (find (+ i 1)))))))
  (write-file! (string-append marklib "/theourgia/commitments.sc")
               (string-append (substring text 0 at)
                              "  (define marker-written (let ((p (open-file-output-port \"" marker
                              "\" (file-options no-fail)))) (close-port p) #t))\n"
                              (substring text at (string-length text)))))
(define (env-marked extra)
  (string-append "CHEZSCHEMELIBDIRS=" marklib ":" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run " extra " "))
;; No daemon left from an earlier section may answer the thin client: one
;; started with the unmarked library path would make the control row lie.
(system (string-append "pkill -f 'serve " store "' 2>/dev/null; sleep 1"))
;; -> (answer-head marker-written?): an answer that was not ok would say nothing
;; about what answering the verb loads.
(define (marked? program verb extra)
  (system (string-append "rm -f '" marker "'"))
  (let ((t (sh-out "marked" (string-append (env-marked extra) "scheme --script ../" program " " verb " --store '" store "' --wire"))))
    (list (guard (e (#t 'UNREADABLE)) (car (read (open-string-input-port t))))
          (file-exists? marker))))
(want "A1 core.sc and the thin client answering outline do not run the commitments library; answering commitments does"
      (in-order (marked? "core.sc" "outline" "THEOURGIA_LOCAL=1")
                (marked? "core.sc" "commitments" "THEOURGIA_LOCAL=1")
                (marked? "theourgia.sc" "outline" "")
                (marked? "theourgia.sc" "commitments" ""))
      '((ok #f) (ok #t) (ok #f) (ok #t)))

(system (string-append "pkill -f 'serve " store "' 2>/dev/null"))
(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\nverb-registry complete\n" bad rows)
(exit (if (= bad 0) 0 1))
