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

;; `describe`: what the verbs are, for something that has to ask.
;;
;; KEY: THE CATALOGUE IS A SECOND PLACE THAT KNOWS THE VERBS, and the first
;; row here is the only thing holding it honest. A verb added to the
;; dispatcher and not to the catalogue is callable and undocumented; one
;; added to the catalogue and not the dispatcher is advertised and
;; missing, which is worse -- a tool list is read and believed.
;;
;; NEVER: IT IS COMPARED IN BOTH DIRECTIONS. "Every catalogue entry is a real
;; verb" is satisfied by a catalogue with one entry in it.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch rpc-ok? rpc-verbs verb-catalogue write-protocol register-verbs!)
        ;; DS-3b asks the parser itself rather than a copy of its tables.
        (only (theourgia arguments) parse-arguments)
        (only (theourgia extensions) extension-verbs))

;; THE VERBS REGISTERED FROM OUTSIDE THE CORE TABLE, as core.sc and the daemon
;; register them: this census reads the registry itself, not a copy of it.
(register-verbs! extension-verbs)

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/describe-" (number->string (get-process-id))))
(define root-dir "..")
(system (string-append "rm -rf " here "; mkdir -p " here "/store " here "/home"))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(define store (string-append here "/store"))

(rpc-dispatch store '(init) "test")

(define answer (rpc-dispatch store '(describe) "test"))
(define entries (cdr (assq 'verbs (cdr answer))))
(define (entry-field entry name) (cadr (assq name (cdr entry))))
(define described (map car entries))

(define (missing-from xs ys)
  (let loop ((xs xs) (out '()))
    (cond ((null? xs) (reverse out))
          ((memq (car xs) ys) (loop (cdr xs) out))
          (else (loop (cdr xs) (cons (car xs) out))))))

;; ---- DS-1 the two lists ----------------------------------------------------
;;
;; NOTE: WITH ONE NAMED EXCEPTION. An entry whose route is `child` is carried
;; out by a program the caller runs (`eval`, by `core.sc`), never by the
;; dispatcher, so it is described and not dispatched. The exception is read
;; from the entries' own route, and the rows below pin that it is `eval`
;; alone.
(define child-only
  (map car (filter (lambda (e) (eq? 'child (entry-field e 'route))) entries)))

(want "DS-1 every verb the dispatcher has is described"
      (missing-from (rpc-verbs) described)
      '())

(want "DS-1 TWIN: and nothing is described that the dispatcher has not got, except a child-route entry"
      (missing-from (missing-from described (rpc-verbs)) child-only)
      '())

;; NEVER: AND THE COUNTS AGREE, which the two rows above do not by themselves
;; guarantee: a catalogue that listed a verb twice passes both. The entries
;; are the dispatcher's verbs plus the child-route ones, and no name twice.
(want "DS-1 and there are as many entries as verbs plus child-route entries, no name twice"
      (list (length described)
            (let loop ((xs described) (seen '()) (twice '()))
              (cond ((null? xs) (reverse twice))
                    ((memq (car xs) seen) (loop (cdr xs) seen (cons (car xs) twice)))
                    (else (loop (cdr xs) (cons (car xs) seen) twice)))))
      (list (+ (length (rpc-verbs)) (length child-only)) '()))

;; ---- DS-2 asking costs nothing ---------------------------------------------
;;
;; KEY: A CALLER ASKING WHAT THE VERBS ARE OFTEN HAS NO STORE. The MCP
;; shell builds its tool list before anyone has said which store they
;; mean, so a `describe` that needed one would make the shell's first
;; act a refusal.
(want "DS-2 describe answers without a store"
      (rpc-ok? (rpc-dispatch (string-append here "/no-such-store") '(describe) "test"))
      #t)

;; NEVER: AND IT DID NOT CREATE ONE ON THE WAY. "It answered" would also be
;; true of an implementation that quietly made the store first.
(want "DS-2 TWIN: and it did not create one"
      (if (file-exists? (string-append here "/no-such-store")) 'CREATED-IT 'left-it-alone)
      'left-it-alone)

(want "DS-2 extra arguments are refused"
      (car (rpc-dispatch store '(describe "something") "test"))
      'usage)

;; ---- DS-2b it touches nothing ----------------------------------------------
;;
;; NEVER: "IT ANSWERED WITHOUT A STORE" IS NOT THE SAME CLAIM AS "IT TOUCHED
;; NOTHING". A `describe` that opened an existing store, took its lock and
;; wrote a snapshot would pass every row above -- the rows above only ever
;; gave it a store that was not there.
;;
;; NOTE: MEASURED AS EVERY FILE'S NAME *AND CONTENT*, before and after.
;; NEVER: Not as "did it raise", which a read would not trip. NEVER: And not as a
;; listing of NAMES, which was the first attempt: `insert` appends to a
;; log file that already exists, so a name listing compared equal across
;; a write and the twin below said so -- the instrument could not see the
;; thing it was there to see.
(define (tree-listing dir)
  (let ((out (string-append here "/listing.txt")))
    (system (string-append "find " dir " -type f -exec md5 -r {} \\; | sort > " out))
    (call-with-input-file out get-string-all)))

(define before-describe (tree-listing store))
(define quiet-answer (rpc-dispatch store '(describe) "test"))
(define after-describe (tree-listing store))

(want "DS-2b describe against a real store leaves it byte for byte as it was"
      (if (string=? before-describe after-describe)
          'untouched
          (list 'changed))
      'untouched)

;; NEVER: AND THE LISTING REALLY WOULD HAVE NOTICED. Without this the row above
;; is passed by a listing that is empty, or by one taken of the wrong
;; directory -- both of which compare equal to themselves.
(want "DS-2b TWIN: and a verb that does write is seen by the same listing"
      (let ((before (tree-listing store)))
        (rpc-dispatch store '(insert "--title" "DS2B") "test")
        (if (string=? before (tree-listing store)) 'SAW-NOTHING 'it-notices))
      'it-notices)


;; ---- DS-2c what a byte-for-byte listing cannot see -------------------------
;;
;; NEVER: THE ROWS ABOVE COMPARE NAMES AND CONTENTS, and three of the things
;; `describe` promises not to do leave both unchanged: opening a file and
;; reading it, taking a lock and releasing it, making a temporary file and
;; removing it again. The promise is "it does not open the store, take a
;; lock or write a byte" -- and only one third of that is measurable by
;; looking at the store afterwards.
;;
;; NOTE: SO THE INSTRUMENT IS THE TRACE, which reports the acts themselves:
;; `log-open`, `flock`, `unlock`, `fsync`. Measured for `outline` on this
;; same store: flock 1, log-open 1, fsync 6, unlock 1. For `describe`:
;; none at all.
(define (trace-kinds-of verb)
  (let ((out (string-append here "/" verb ".trace")))
    (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
                           "THEOURGIA_HOME=" here "/home THEOURGIA_TRACE=1 "
                           "scheme --script ../core.sc " verb " --store " store
                           " --wire > /dev/null 2> " out))
    (let* ((text (call-with-input-file out get-string-all))
           (n (string-length text)))
      (let count ((i 0) (seen 0))
        (cond ((> (+ i 7) n) seen)
              ((string=? (substring text i (+ i 7)) "(trace ") (count (+ i 7) (+ seen 1)))
              (else (count (+ i 1) seen)))))))

(want "DS-2c describe does not open the store, lock it, or flush anything"
      (let ((n (trace-kinds-of "describe")))
        (if (zero? n) 'did-nothing-to-the-store (list 'events n)))
      'did-nothing-to-the-store)

;; NEVER: AND THE INSTRUMENT REALLY SEES THOSE ACTS. Without this row the one
;; above is passed by a build with tracing switched off, by a trace
;; written somewhere else, and by a verb that failed before it started.
(want "DS-2c TWIN: and a verb that does touch the store is seen doing it"
      (let ((n (trace-kinds-of "outline")))
        (if (> n 0) 'it-notices (list 'BLIND n)))
      'it-notices)

;; ---- DS-3 what each entry carries ------------------------------------------

(want "DS-3 every entry has a usage form, a description and a protocol flag"
      (let loop ((es entries) (bad '()))
        (cond
          ((null? es) (reverse bad))
          ((and (pair? (entry-field (car es) 'usage))
                (string? (entry-field (car es) 'description))
                (> (string-length (entry-field (car es) 'description)) 0)
                (boolean? (entry-field (car es) 'protocol)))
           (loop (cdr es) bad))
          (else (loop (cdr es) (cons (car (car es)) bad)))))
      '())

;; NOTE: THE USAGE FORM NAMES THE VERB IT BELONGS TO. A catalogue whose
;; entries had drifted -- `read`'s usage filed under `refs` -- would pass
;; every row above.
(want "DS-3 each usage form begins with its own verb"
      (let loop ((es entries) (bad '()))
        (cond
          ((null? es) (reverse bad))
          ((eq? (car (entry-field (car es) 'usage)) (car (car es)))
           (loop (cdr es) bad))
          (else (loop (cdr es) (cons (car (car es)) bad)))))
      '())

;; ---- DS-3b the options a usage form declares can actually be given --------
;;
;; NEVER: DECLARING AN OPTION IN A USAGE FORM IS NOT DECLARING IT. The
;; parser reads a token as an option only if it appears in `value-options`
;; or `flag-options` inside `arguments.sc`; anything else becomes a
;; positional. So a verb can advertise an option in the text a person is
;; shown, and answer a usage error when they give it.
;;
;; That is not hypothetical. `grep` was written with `["--under" <id>]` in
;; its usage form and nowhere else, and `grep <pattern> --under <id>`
;; arrived as three positionals and was refused for its arity. It was found
;; by RUNNING the verb, not by reading either list -- which is the point:
;; the usage form and the parser are two lists, and only one of them is
;; consulted when a command is read.
;;
;; THE ROW ASKS THE PARSER, NOT A SECOND COPY OF THE TABLE. `value-options`
;; and `flag-options` are not exported -- the file that holds them warns in
;; its own comment that they are "a second place that knows the command
;; line" -- so a row that restated them would be a third. `parse-arguments`
;; is exported and is the thing that DECIDES, so the row hands it a command
;; and looks at what came back.
;;
;; That choice is the whole design of this row, and it is written down
;; because the other way looks more direct: a row either asks the thing that
;; decides, or it is adding another copy of what that thing knows. Comparing
;; the usage forms against those two tables would read as a tighter check
;; and would in fact be a third list to keep in step -- and the first of the
;; three to drift would be the one nothing runs.
;;
;; THE TECHNIQUE WAS ALREADY HERE, in another fixture, and this row did not
;; invent it: `docs-check.sc`'s DOC-2 asks the same question of the README's
;; advertised options with the same `parse-arguments` call. What is new is
;; the SOURCE list -- the usage forms the catalogue hands out, which is what
;; `describe` shows and what the MCP tool descriptions are built from.
;;
;; The two do not make each other redundant, and they would have failed
;; differently on the defect that prompted this row: `--under` was in the
;; usage form and not yet in the README, so DOC-2 had nothing to compare and
;; stayed green. An option that is in both is covered twice, which is fine;
;; an option in either alone is covered once, which is the point.
;;
;; NEVER: AND THIS IS ONE DIRECTION OF THREE. The gate above asks that the
;; catalogue and the dispatcher agree; this one asks that what a usage form
;; declares, the parser accepts. The third -- that every option a HANDLER
;; reads is one the parser accepts -- is still missing, and it needs a
;; different scan, over the handler bodies rather than over the catalogue.
;; It is recorded in the queue as B3 and is not done here. Saying so is the
;; difference between a gap and the impression that this was dealt with.
(define (usage-options form)
  ;; `["--md"]` reads as ("--md"), `["--writer" <name>]` as ("--writer" <name>)
  (let loop ((xs (cdr form)) (out '()))
    (cond
      ((not (pair? xs)) (reverse out))
      ((and (pair? (car xs)) (string? (car (car xs)))
            (> (string-length (car (car xs))) 2)
            (string=? "--" (substring (car (car xs)) 0 2)))
       (loop (cdr xs) (cons (car xs) out)))
      (else (loop (cdr xs) out)))))

;; -> #f when the parser took it as an option, else a word saying what it did.
(define (parser-takes? verb decl)
  (let* ((name (car decl))
         (valued? (> (length decl) 1))
         (argv (if valued? (list "subject" name "a-value") (list "subject" name)))
         (nodes (parse-arguments verb argv)))
    (cond
      ((not (list? nodes)) 'refused)
      ((exists (lambda (n) (and (pair? n) (memq (car n) '(option flag))
                                (string? (cadr n)) (string=? (cadr n) name)))
               nodes)
       #f)
      (else 'read-as-a-positional))))

(want "DS-3b every option a usage form declares is one the parser will accept"
      (let loop ((es entries) (bad '()))
        (cond
          ((null? es) (reverse bad))
          (else
            (let* ((verb (car (car es)))
                   (form (entry-field (car es) 'usage))
                   (missed (filter (lambda (d) (parser-takes? verb d))
                                   (usage-options form))))
              (loop (cdr es)
                    (if (null? missed) bad (cons (cons verb (map car missed)) bad)))))))
      '())

;; CONTROL: the row is looking at something. A run where no usage form
;; declared any option at all would satisfy it perfectly.
(want "DS-3b CONTROL: usage forms do declare options, and this many of them"
      (> (apply + (map (lambda (e) (length (usage-options (entry-field e 'usage)))) entries)) 5)
      #t)

;; ---- DS-4 the protocol is the one constant ---------------------------------
;;
;; NEVER: BYTE FOR BYTE, not "contains something about writing". The MCP tool
;; descriptions are built from this same text; if `describe` handed out a
;; paraphrase, the shell and the README would be documenting two
;; different protocols and both would look right.
(want "DS-4 the protocol describe hands out is the constant itself"
      (if (string=? (cadr (assq 'protocol (cdr answer))) write-protocol)
          'byte-for-byte
          'DIFFERENT)
      'byte-for-byte)

;; ---- DS-5 which verbs the protocol is about --------------------------------
;;
;; NOTE: NAMED, NOT COUNTED. "Two verbs are marked" would stay true if the
;; mark moved to two other verbs.
(want "DS-5 the protocol flag is set on exactly insert and write"
      (let loop ((es entries) (out '()))
        (cond ((null? es) (reverse out))
              ((entry-field (car es) 'protocol) (loop (cdr es) (cons (car (car es)) out)))
              (else (loop (cdr es) out))))
      '(insert write))

;; ---- DS-6 the route, and the client's bootstrap list ----------------------
;;
;; KEY: EVERY ENTRY SAYS WHO CARRIES IT OUT. `daemon` means a client sends
;; it over the socket; `local` means the client runs the server itself,
;; because there is nothing to send it to yet.
;;
;; NEVER: THE DEFECT THIS EXISTS FOR: the MCP shell has no local route at all,
;; so it offered `theourgia_init` as a tool, sent it to a daemon for a
;; store that did not exist, and the daemon could not start. The shell
;; could never create a store, and an agent reading the tool list was told
;; otherwise.
;;
;; NOTE: THE CLIENT PROGRAM KEEPS ITS OWN LIST and must: it needs to know
;; before it can ask anybody. So the two are compared, in both directions.
;; NEVER: The list is READ OUT OF THE PROGRAM'S SOURCE rather than written
;; again here -- a copy in this file would agree with itself forever.

(define client-source
  (let ((p (string-append root-dir "/theourgia.sc")))
    (if (file-exists? p) (call-with-input-file p get-string-all) "")))

(define (local-verbs-of-client text)
  (let* ((key "(define local-verbs '(")
         (at (let loop ((i 0))
               (cond ((> (+ i (string-length key)) (string-length text)) #f)
                     ((string=? (substring text i (+ i (string-length key))) key) i)
                     (else (loop (+ i 1)))))))
    (and at
         (let loop ((i (+ at (string-length key))) (acc '()) (word '()))
           (cond
             ((>= i (string-length text)) (reverse acc))
             ((char=? (string-ref text i) #\))
              (reverse (if (null? word) acc (cons (list->string (reverse word)) acc))))
             ((char=? (string-ref text i) #\space)
              (loop (+ i 1) (if (null? word) acc (cons (list->string (reverse word)) acc)) '()))
             (else (loop (+ i 1) acc (cons (string-ref text i) word))))))))

(define client-local (or (local-verbs-of-client client-source) '()))

(want "DS-6 the client program's local list was read, and is not empty"
      (if (null? client-local) 'READ-NOTHING 'read-it)
      'read-it)

(define catalogue-local
  (map (lambda (e) (symbol->string (car e)))
       (let loop ((es entries) (out '()))
         (cond ((null? es) (reverse out))
               ((eq? 'local (entry-field (car es) 'route)) (loop (cdr es) (cons (car es) out)))
               (else (loop (cdr es) out))))))

(want "DS-6 every verb the catalogue routes locally is in the client's list"
      (let loop ((xs catalogue-local) (bad '()))
        (cond ((null? xs) (reverse bad))
              ((member (car xs) client-local) (loop (cdr xs) bad))
              (else (loop (cdr xs) (cons (car xs) bad)))))
      '())


;; NEVER: AND EVERY ENTRY'S ROUTE IS A ROUTE. The comparisons above select the
;; entries whose route is exactly `local`, so a verb whose route is
;; MISSPELLED -- `deamon` -- silently leaves that set and matches
;; everything they ask. The shell then drops it from the tool list, which
;; is a verb quietly disappearing for an agent, and no row here moves.
;; A closed set is checked against the whole table instead.
(want "DS-6 every entry in the catalogue carries a route that exists"
      (let loop ((es entries) (bad '()))
        (cond ((null? es) (reverse bad))
              ((memq (entry-field (car es) 'route) '(local daemon child stream))
               (loop (cdr es) bad))
              (else (loop (cdr es)
                          (cons (list (car (car es)) (entry-field (car es) 'route)) bad)))))
      '())

;; THE STREAM ROUTE (F10-14). `subscribe` keeps its connection open, so it is
;; carried out by a client that streams: the route is `stream`, which the
;; shell does not offer and the thin client names in a list of its own.
(want "DS-6s subscribe is in the catalogue, routed stream"
      (let ((e (assq 'subscribe entries))) (and e (entry-field e 'route)))
      'stream)
;; THE CLIENT'S LIST, READ AS DATA: theourgia.sc's top-level forms are read
;; and the one `(define stream-verbs '(...))` among them is taken, so its
;; layout and any mention in a comment do not matter. #f when there is none.
(define (client-list-of path name)
  (and (file-exists? path)
       (let ((p (open-input-file path)))
         (let loop ()
           (let ((x (guard (e (#t (eof-object))) (read p))))
             (cond ((eof-object? x) (close-port p) #f)
                   ((and (list? x) (= (length x) 3) (eq? (car x) 'define) (eq? (cadr x) name)
                         (pair? (caddr x)) (eq? (car (caddr x)) 'quote) (list? (cadr (caddr x))))
                    (close-port p)
                    (map symbol->string (cadr (caddr x))))
                   (else (loop))))))))
(define client-stream
  (or (client-list-of (string-append root-dir "/theourgia.sc") 'stream-verbs) '()))
(define catalogue-stream
  (map (lambda (e) (symbol->string (car e)))
       (filter (lambda (e) (eq? 'stream (entry-field e 'route))) entries)))
(want "DS-6s the client's stream list is not empty and equals the catalogue's stream entries, both ways"
      (list (pair? client-stream)
            (list-sort string<? client-stream)
            (equal? (list-sort string<? client-stream) (list-sort string<? catalogue-stream)))
      (list #t (list-sort string<? catalogue-stream) #t))

;; NOTE: THE OTHER DIRECTION HAS AN EXEMPTION, AND IT IS NAMED. `serve` is a
;; command of the daemon PROGRAM, not a core verb, so it is not in the
;; catalogue at all and cannot have a route. A client local-verb that IS in
;; the catalogue has to be marked `local` or `child` there: both are carried
;; out by a process the client runs, and `child` is the one an MCP shell can
;; carry out as well.
(define not-core-verbs '("serve"))

(define catalogue-child
  (map (lambda (e) (symbol->string (car e)))
       (filter (lambda (e) (eq? 'child (entry-field e 'route))) entries)))

(want "DS-6 every core verb the client runs locally is marked local or child"
      (let loop ((xs client-local) (bad '()))
        (cond
          ((null? xs) (reverse bad))
          ((member (car xs) not-core-verbs) (loop (cdr xs) bad))
          ((not (member (car xs) (map (lambda (e) (symbol->string (car e))) entries)))
           (loop (cdr xs) (cons (list (car xs) 'not-a-core-verb) bad)))
          ((member (car xs) catalogue-local) (loop (cdr xs) bad))
          ((member (car xs) catalogue-child) (loop (cdr xs) bad))
          (else (loop (cdr xs) (cons (list (car xs) 'routed-to-the-daemon) bad)))))
      '())

;; NEVER: `child` IS THREE VERBS' ROUTE, AND NONE OF THEM IS A DISPATCHER
;; VERB. The exception DS-1 makes is these entries and nothing else: eval,
;; and the review channel's two programs, scope and collect, which send
;; stores their own requests and so must never run inside a daemon. A
;; fourth child entry, or any of the three appearing in the dispatcher's
;; table, turns this red and somebody decides.
(want "DS-6 eval, scope and collect are the catalogue's child entries, and none is a dispatcher verb"
      (list (list-sort string<? catalogue-child)
            (filter (lambda (v) (memq v (rpc-verbs))) '(eval scope collect)))
      (list '("collect" "eval" "scope") '()))

;; NEVER: AND A DAEMON STILL DOES NOT CARRY IT OUT. eval is advertised; the
;; dispatcher answers a request naming it as it answers any verb it does
;; not have. A control: green before eval was catalogued, and green after.
(want "DS-6 eval sent to the dispatcher is still an unknown verb"
      (let ((a (rpc-dispatch store '(eval "(+ 1 2)") "test")))
        (and (pair? a) (list (car a) (cadr a))))
      '(error unknown-verb))
(want "DS-6 scope and collect sent to the dispatcher are unknown verbs too"
      (map (lambda (req) (let ((a (rpc-dispatch store req "test"))) (and (pair? a) (list (car a) (cadr a)))))
           '((scope "x" "--cut" "()" "--roots" "a" "--for" "b") (collect "x")))
      '((error unknown-verb) (error unknown-verb)))

;; NEVER: AND THE EXEMPTION IS NOT A HOLE: the name it covers must really be
;; absent from the catalogue. If `serve` ever became a core verb this row
;; goes red and somebody decides, rather than the exemption quietly
;; covering a verb that now has a route.
(want "DS-6 the exempted names are genuinely not core verbs"
      (let loop ((xs not-core-verbs) (bad '()))
        (cond ((null? xs) (reverse bad))
              ((member (car xs) (map (lambda (e) (symbol->string (car e))) entries))
               (loop (cdr xs) (cons (car xs) bad)))
              (else (loop (cdr xs) bad))))
      '())

;; ---- DS-7 a refusal carries the form the catalogue publishes ---------------
;;
;; NEVER: A VERB'S REFUSAL USED TO SAY NOTHING ABOUT WHAT THE VERB ACCEPTS.
;; `commit` takes any number of block ids, including none, so there is no
;; argument count that can be wrong; a caller who misspelled an option was
;; told what was wrong with the value and never what the verb takes. The
;; form is appended to whatever refusal came back, which leaves the
;; refusal's own classification alone.
;;
;; NEVER: AND NOTHING ASKED WHETHER IT ARRIVED. That form had a definition,
;; a catalogue entry and three comments written about it, and not one row:
;; deleting the append left every cell in the tree green. A behaviour with
;; no instrument is not the same failure as an instrument that overstates
;; itself -- the second misleads a reader, the first loses the feature in
;; silence and nothing says so.
;;
;; Both answers below are RUN. This file reads no source: `describe`
;; publishes the catalogue's spelling, a refused `commit` carries the
;; refusal's, and the two are compared.
;;
;; NOTE: WHAT THIS PAIR CAN AND CANNOT SAY. The two sites reference one
;; binding, so their equality holds by construction, and this is NOT a
;; check that two spellings agree. What it checks is that the site is
;; still there and still emits this form. Whether a form is WRITTEN in more
;; than one place is a different question and belongs to
;; `test/options-gate.sc`; where a form is written and how it reaches a
;; caller are not the same question.
;;
;; NOTE: THE REFUSAL IS CHOSEN TO REACH THE HANDLER. The store-missing
;; refusal would have been the wrong choice: it is answered before the
;; handler runs, so it carries no usage clause and would have proved
;; something else.
;;
;; NEVER: BUT IT IS NOT UNRELATED TO THIS ROW, WHICH IS WHAT THIS SAID.
;; The control below asks for `(error bad-request duplicate-block)` exactly,
;; so renaming that refusal, or accepting repeated ids, or adding an earlier
;; check that answers this request first, turns the control red while the
;; usage form it is really about has not moved. The control is coupled to
;; that rule and the coupling is written here rather than denied: if it goes
;; red, read the row below it before concluding anything about commit's
;; usage form.
(define commit-entry (assq 'commit entries))
(define commit-published-usage
  (and (pair? commit-entry) (entry-field commit-entry 'usage)))

(define commit-refusal (rpc-dispatch store '(commit "b1" "b1" "--writer" "w") "test"))

(define (usage-clause-of answer)
  (and (pair? answer)
       (let look ((xs (cdr answer)))
         (cond
           ((not (pair? xs)) #f)
           ((and (pair? (car xs)) (eq? (car (car xs)) 'usage) (pair? (cdr (car xs))))
            (cadr (car xs)))
           (else (look (cdr xs)))))))

;; CONTROL: the catalogue really published a form for this verb. Without
;; this, a missing entry would make the comparison below compare #f with #f
;; on the day the refusal also stopped carrying one.
(want "DS-7 CONTROL: describe publishes a commit form, and it is a commit form"
      (list (and (pair? commit-published-usage) #t)
            (and (pair? commit-published-usage) (car commit-published-usage)))
      (list #t 'commit))

;; CONTROL: and the call really was refused, by the rule this row picked.
;; If this goes red the premise is wrong, and the reading says which
;; refusal answered instead -- which is what a control is for.
(want "DS-7 CONTROL: the call was refused, for the reason this row chose"
      (list (and (pair? commit-refusal) (car commit-refusal))
            (and (pair? commit-refusal) (pair? (cdr commit-refusal)) (cadr commit-refusal))
            (and (pair? commit-refusal) (pair? (cdr commit-refusal))
                 (pair? (cddr commit-refusal)) (caddr commit-refusal)))
      (list 'error 'bad-request 'duplicate-block))

(want "DS-7 a refused commit carries the usage form, compared whole"
      (list (and (usage-clause-of commit-refusal) #t)
            (equal? (usage-clause-of commit-refusal) commit-published-usage))
      (list #t #t))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\ndescribe complete\n" rows bad)
(exit (if (zero? bad) 0 1))
