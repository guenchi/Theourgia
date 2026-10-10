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

;; WHAT THE REDUCER IS ALLOWED TO REACH.
;;
;; The block hash is a fact about what was stored. The derived view --
;; a title read out of a document, the language a block is written in,
;; the name a definition carries -- is a fact about how this version of
;; this core reads it. While the reducer could reach the language table,
;; registering a language could change a hash, and a store written by
;; one build would stop agreeing with the same store read by the next.
;;
;; THE RULE IS ABOUT REACHING, NOT ABOUT CALLING, so nothing local can
;; witness it. `reduce.sc` never named `languages`; it named
;; `text-code`, which names `languages`. A reviewer reading `reduce.sc`
;; sees a clean file. The property is a property of the CLOSURE, and the
;; closure is what this reads.
;;
;; NEVER: NOT A GREP OVER THE FILE. The walk is `import-walk.sc`, shared
;; with `facade-gate.sc`; why it reads the forms as data rather than the
;; text is written there.
;;
;; THE FOURTH ROW IS THE TWIN, AND IT IS THE POINT OF THE OTHER THREE.
;; Three closures that do not contain a name are also what a walker that
;; returns nothing produces, and a walker that returns nothing is the
;; likelier of the two. `rpc` reaches all five of the forbidden names --
;; it is the layer that is SUPPOSED to project a view -- so the same
;; question, asked about it, has to come back non-empty. Without that
;; row these cells would be green on a tree with no import graph at all.

(import (chezscheme))

(define failures 0)
(define rows 0)
(define (want-1 name actual expected)
  (set! rows (+ rows 1))
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
;; BOTH SIDES OF A ROW ARE GUARDED. A `want` written as a procedure
;; evaluates its arguments before the call, so an argument that raises
;; kills the fixture: the row never prints, and what the suite sees is a
;; missing sentinel rather than a red row naming the question that could
;; not be answered. This directory's `run-fixtures.sh` counts fixtures
;; whose `want` is a bare procedure for exactly that reason.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/core.sc")) up script-dir)))

(load (string-append script-dir "/import-walk.sc"))

;; THE GRAPH IS BUILT FROM WHAT EACH FILE DECLARES ITSELF TO BE, not
;; from its filename. They agree in this tree, and a graph keyed on
;; filenames would go on agreeing with itself after they stopped.
(define graph
  (let loop ((names (source-files root)) (out '()))
    (if (null? names)
        out
        (let* ((path (string-append root "/" (car names)))
               (declared (declared-library-name path)))
          (loop (cdr names)
                (if (and (pair? declared) (eq? 'theourgia (car declared)) (pair? (cdr declared)))
                    (cons (cons (cadr declared)
                                (map cadr (filter (lambda (r) (pair? (cdr r)))
                                                  (imports-of-file 'theourgia path))))
                          out)
                    out))))))

;; THE CLOSURE, WITH A SEEN SET. Chez refuses an import cycle outright
;; -- measured, on two libraries importing each other: `Exception:
;; cyclic dependency involving import of library (cyclic a)` -- so this
;; tree cannot hold one. The seen set is here anyway, because a walker
;; that assumes its input is acyclic answers a question about the
;; walker: it would spin rather than say so.
(define (closure start)
  (let loop ((todo (list start)) (seen '()))
    (cond
      ((null? todo) (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
      ((memq (car todo) seen) (loop (cdr todo) seen))
      (else
       (let ((edges (cond ((assq (car todo) graph) => cdr) (else '()))))
         (loop (append edges (cdr todo)) (cons (car todo) seen)))))))

;; THE NAMES THAT PROJECT A VIEW. A block's stored bytes do not depend
;; on any of them; what a reader shows for that block does.
(define derived '(languages text-code datum-code code-project datum-project))

(define (reaches start)
  (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
             (filter (lambda (n) (memq n derived)) (closure start))))

(define (report start)
  (printf "closure ~a (~a): ~s\n" start (length (closure start)) (closure start)))

;; THE INSTRUMENT'S OWN FIRST READING, BEFORE ANY OF THE ROWS BELOW.
;; An empty graph makes every "does not reach" row green.
(want "CL-00 the graph holds this core's libraries"
      (> (length graph) 20) #t)
(want "CL-00 and every name the rows below ask about is in it"
      (filter (lambda (n) (not (assq n graph)))
              '(reduce markers datum-metadata rpc view))
      '())

(for-each report '(reduce markers datum-metadata view rpc))

(want "CL-01 the reducer cannot reach a derived view" (reaches 'reduce) '())
(want "CL-02 the marker grammar cannot reach a derived view" (reaches 'markers) '())
(want "CL-03 datum metadata cannot reach a derived view" (reaches 'datum-metadata) '())

;; THE TWIN. Same question, a layer that is supposed to answer yes.
(want "CL-04 rpc does reach all five, which is how we know the walk walks"
      (reaches 'rpc)
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) derived))

;; AND THE PROJECTION LIVES SOMEWHERE. `view-read` was moved out of the
;; reducer, not deleted; if it had been deleted the three rows above
;; would read exactly the same.
;;
;; THE THREE NAMES ARE THE THREE IMPORTS `reduce.sc` LOST, taken from
;; the change rather than from a reading of the answer: an expectation
;; copied from what the instrument prints cannot disagree with it. The
;; other two derived names are NOT here and must not be -- `code-project`
;; and `datum-project` are built ON the view, so a closure of `view`
;; that contained them would be a cycle, not a success.
(want "CL-05 the view layer reaches the three projections the reducer gave up"
      (reaches 'view)
      '(datum-code languages text-code))

;; ---- C-1 the two thin things ----------------------------------------------
;;
;; KEY: A CLIENT MAY NOT LOAD THE SERVER IT IS TRYING TO TALK TO. That is
;; the whole reason the command line and the MCP shell were split off:
;; every call used to pay to load the dispatcher, the store, the actor
;; system and libuv, and then use none of them. These rows are what say
;; it is still true.
;;
;; NEVER: NAMED, NOT COUNTED. "The closure is small" would stay true of a
;; closure that had swapped one of these libraries for another.

(define server-side '(rpc sched net daemon store reduce log working))

(define (server-parts-of start)
  (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
             (filter (lambda (n) (memq n server-side)) (closure start))))

(want "C-1 the client library reaches none of the server"
      (server-parts-of 'client)
      '())

;; (theourgia answers) is in it since F100b item 6: the client's own
;; failures are answered by the table (classify-failure).
;; (theourgia platform-numbers) is in it because ffi.sc imports it: every
;; platform number the FFI uses comes from the running platform's row.
(want "C-1 and its closure is exactly what it should be"
      (closure 'client)
      '(answers client digest ffi incomplete platform-numbers render trace))

;; KEY: THE CONTROL ROW. Without it every row above is also passed by a
;; walker that found no edges at all -- which is the state this file
;; would be in if the graph were built from the wrong directory.
(want "C-1 CONTROL: the server side does reach all of it"
      (let ((missing (filter (lambda (n) (not (memq n (closure 'rpc)))) server-side)))
        ;; `rpc` is itself in the list and reaches the rest; `sched`,
        ;; `net` and `daemon` are above it and are not expected here.
        (filter (lambda (n) (memq n '(store reduce log working))) missing))
      '())

;; NOTE: THE SHELL IS A PROGRAM, not a library, so its own import list is
;; the seed. A row keyed on a library name would have silently measured
;; nothing at all.
(define shell-imports
  (map cadr (filter (lambda (r) (pair? (cdr r)))
                    (imports-of-file 'theourgia (string-append root "/mcp/server.sc")))))

(define (closure-of-all starts)
  (let loop ((todo starts) (seen '()))
    (cond
      ((null? todo)
       (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
      ((memq (car todo) seen) (loop (cdr todo) seen))
      (else
       (let ((edges (cond ((assq (car todo) graph) => cdr) (else '()))))
         (loop (append edges (cdr todo)) (cons (car todo) seen)))))))

(want "C-1 the MCP shell imports something at all"
      (if (null? shell-imports) 'NOTHING-WAS-READ 'read-its-imports)
      'read-its-imports)

(want "C-1 the MCP shell reaches none of the server"
      (filter (lambda (n) (memq n server-side)) (closure-of-all shell-imports))
      '())

;; (theourgia answers) is in it since F100b item 6: the client's own
;; failures are answered by the table (classify-failure).
;; (theourgia refusal) is in it since F100b M3a: the shell renders every
;; refusal through one object (item 8; ruling Q-M3-1).
;; (theourgia platform-numbers) is in it because ffi.sc imports it: every
;; platform number the FFI uses comes from the running platform's row.
(want "C-1 and the shell's closure is exactly what it should be"
      (closure-of-all shell-imports)
      '(answers arguments client digest ffi incomplete json platform-numbers refusal render trace))


;; ---- C-2 the client PROGRAM's own closure --------------------------------
;;
;; NOTE: A PROGRAM'S IMPORTS ARE ITS OWN, and the rows above are about the
;; `client` LIBRARY. `theourgia.sc` is what a person actually runs, and
;; what it reaches is a separate fact.
;;
;; KEY: `arguments` IS ALLOWED HERE, AND THE REASON IS THE POINT. The
;; envelope carries what the caller piped in, and only the verb's own
;; table knows WHETHER a verb reads standard input -- `write <id> -` does
;; and `write <id> text` does not. The program asks that table rather than
;; keeping a second copy of it, and still forwards the arguments exactly
;; as they were written. `(theourgia arguments)` imports nothing but
;; `(rnrs base)` and `(rnrs lists)`: it reaches neither the dispatcher,
;; nor the scheduler, nor the networking library, which is what this file
;; exists to keep true.
;;
;; KEY: `answers` IS ALLOWED HERE TOO. F100b point 4: the program's main
;; turns a filesystem failure into the one table answer that every other
;; point gives, rather than keeping a second copy of the table.
;; `(theourgia answers)` imports `(chezscheme)` and names from
;; `(theourgia ffi)`, which the closure already holds, and does no
;; filesystem work of its own: it adds no edge to the server.
(define program-imports
  (map cadr (filter (lambda (r) (pair? (cdr r)))
                    (imports-of-file 'theourgia (string-append root "/theourgia.sc")))))

(want "C-2 the client program imports something at all"
      (if (null? program-imports) 'NOTHING-WAS-READ 'read-its-imports)
      'read-its-imports)

(want "C-2 the client program reaches none of the server"
      (filter (lambda (n) (memq n server-side)) (closure-of-all program-imports))
      '())

;; (theourgia platform-numbers) is in it because ffi.sc imports it: every
;; platform number the FFI uses comes from the running platform's row.
(want "C-2 and the client program's closure is exactly what it should be"
      (closure-of-all program-imports)
      '(answers arguments client digest ffi incomplete platform-numbers render trace))

;; ---- C-3 the change stream's two libraries are entered on demand -----------
;;
;; KEY: A START THAT FOLLOWS NOTHING DOES NOT LOAD THEM. (theourgia
;; stream-frames) is the frame computation, entered by the daemon's store
;; process when a subscription is accepted; (theourgia stream-client) is the
;; streaming reader, entered by the command line for a stream verb. Neither
;; is imported by any program or library: these rows say so from the
;; imports, and change-stream.sc's on-demand rows say so from a running
;; process (library-list) and a daemon's trace.
(define stream-libraries '(stream-frames stream-client))
(define (program-imports-of rel)
  (map cadr (filter (lambda (r) (pair? (cdr r)))
                    (imports-of-file 'theourgia (string-append root "/" rel)))))
(want "C-3 CONTROL: both libraries are in the graph"
      (filter (lambda (n) (not (assq n graph))) stream-libraries)
      '())
(want "C-3 no program reaches either: core.sc, theourgia.sc, theourgiad.sc, the MCP shell"
      (map (lambda (rel)
             (list rel (filter (lambda (n) (memq n stream-libraries))
                               (closure-of-all (program-imports-of rel)))))
           '("core.sc" "theourgia.sc" "theourgiad.sc" "mcp/server.sc"))
      '(("core.sc" ()) ("theourgia.sc" ()) ("theourgiad.sc" ()) ("mcp/server.sc" ())))
(want "C-3 and no library imports either"
      (filter (lambda (entry) (exists (lambda (n) (memq n stream-libraries)) (cdr entry))) graph)
      '())
(want "C-3 the frame library reaches the reducer, through its one exported reading"
      (and (memq 'reduce (closure 'stream-frames)) #t)
      #t)
(want "C-3 the streaming reader's closure is the client library's, and none of the server"
      (list (closure 'stream-client) (server-parts-of 'stream-client))
      (list '(answers client digest ffi incomplete platform-numbers render stream-client trace) '()))

(printf "rows: ~a\n~a failures\nclosures complete\n" rows failures)
(exit (if (zero? failures) 0 1))
