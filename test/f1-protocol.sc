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

;; The write protocol, in one place and reaching three readers.
;;
;; KEY: THE POINT IS THAT THERE IS ONE COPY. `write-protocol` is exported by
;; `(theourgia rpc)`; the README's `## Writing for agents` section is that
;; string; the MCP shell's `insert` and `write` descriptions begin with
;; it. Three statements of a protocol are three protocols as soon as one
;; is edited, and the reader follows whichever they happened to meet.

(import (chezscheme)
        (only (theourgia rpc) write-protocol verb-catalogue register-verbs!)
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
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

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

(define here (string-append scratch-base "/f1-" (number->string (get-process-id))))
(define sock-here (string-append socket-base "/f1-" (number->string (get-process-id))))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (index-of text needle from)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i from))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))

(define (contains? text needle) (and (index-of text needle 0) #t))

;; ---- DOC-P1: the README section IS the constant -------------------------------
;;
;; NEVER: READ FROM THE FILE ON DISK, NOT REGENERATED. A check that wrote the
;; section from the constant and then compared them would agree with
;; itself no matter what the committed README says, which is the one
;; thing this row exists to notice.
(define readme (file-text "../README.md"))

;; NOTE: THE BLANK LINE BEFORE THE NEXT HEADING BELONGS TO THE DOCUMENT,
;; not to the protocol: Markdown needs it and the constant does not have
;; it. Taking it made the two differ by one byte -- a real difference,
;; and not the one this row is about.
(define section-body
  (let ((start (index-of readme "## Writing for agents\n\n" 0)))
    (and start
         (let* ((from (+ start (string-length "## Writing for agents\n\n")))
                (end (index-of readme "\n\n## " from)))
           (and end (substring readme from (+ end 1)))))))

(want "DOC-P1 the README's section is the core's protocol string, byte for byte"
      (if (equal? section-body write-protocol)
          'identical
          (list 'readme section-body 'constant write-protocol))
      'identical)

;; ---- DOC-P2: and the protocol still says the seven things ---------------------
;;
;; NEVER: DOC-P1 CANNOT SEE A RULE GO MISSING. It says the two copies agree;
;; delete a rule from the constant and regenerate the README and it stays
;; green. NOTE: This list is the seventh copy on purpose -- it is a list of
;; what the protocol is FOR, kept outside the string so that dropping a
;; rule has to be done twice and deliberately.
(define seven-rules
  '(("one block answers one question" "answer one question on its own")
    ("a size bound, with a number"     "800 tokens")
    ("and the same bound in bytes"     "3000 bytes")
    ("a one-sentence title"            "one-sentence title")
    ("3 to 8 comma-separated keywords" "3 to 8 keywords, comma separated")
    ("the option that carries them"    "--keywords")
    ("placement under a parent"        "--under")
    ("the source bytes are not edited" "must not edit its prose")
    ("changes go through a draft"      "write and then commit")))

(want "DOC-P2 the protocol still states every rule it is for"
      (map car (filter (lambda (r) (not (contains? write-protocol (cadr r)))) seven-rules))
      '())

;; ---- MC-P1: the two writing tools carry it ------------------------------------
(define shell "../mcp/server.sc")

(define init
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                 "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                 "\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}"))
(define ready "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
(define listing "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}")

(system (string-append "rm -rf " here " " sock-here "; mkdir -p " here "/store " sock-here))
(system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                       "scheme --script ../core.sc init --store " here "/store --wire > /dev/null 2>&1"))

;; NOTE: THE REAL HANDSHAKE. `tools/list` before `notifications/initialized`
;; is refused by the shell, deliberately -- so a row that skipped it
;; would be reading an error and finding no tools in it.
(define tools-line
  (begin
    (call-with-output-file (string-append here "/in.jsonl")
      (lambda (p) (put-string p (string-append init "\n" ready "\n" listing "\n"))))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "THEOURGIA_RUN=" sock-here "/run "
                           "scheme --script " shell " --store " here "/store < " here "/in.jsonl > "
                           here "/out.jsonl 2>" here "/err.txt"))
    ;; NEVER: BY THE REQUEST ID, NOT BY THE WORD `tools`. The initialize
    ;; result announces `"capabilities":{"tools":{}}` on the FIRST line,
    ;; so a search for that word finds the handshake and reports that
    ;; the shell offers no tools at all.
    (let* ((all (file-text (string-append here "/out.jsonl")))
           (at (index-of all "\"id\":2" 0)))
      (and at
           (let ((start (let loop ((i at)) (if (or (zero? i) (char=? (string-ref all (- i 1)) #\newline))
                                               i (loop (- i 1)))))
                 (end (or (index-of all "\n" at) (string-length all))))
             (substring all start end))))))

;; The description of one tool, taken out of the listing by name.
(define (description-of name)
  (let* ((key (string-append "\"name\":\"" name "\",\"description\":\""))
         (at (and tools-line (index-of tools-line key 0))))
    (and at
         (let loop ((i (+ at (string-length key))) (acc '()))
           (cond
             ((>= i (string-length tools-line)) #f)
             ((char=? (string-ref tools-line i) #\\)
              (loop (+ i 2) (cons (let ((c (string-ref tools-line (+ i 1))))
                                    (cond ((char=? c #\n) #\newline)
                                          ((char=? c #\t) #\tab)
                                          (else c)))
                                  acc)))
             ((char=? (string-ref tools-line i) #\") (list->string (reverse acc)))
             (else (loop (+ i 1) (cons (string-ref tools-line i) acc))))))))

(want "MC-P1 the listing was read at all"
      (if (and tools-line (contains? tools-line "theourgia_insert")) 'listed
          (list 'said (if tools-line (substring tools-line 0 (min 120 (string-length tools-line))) #f)))
      'listed)

;; EVERY VERB IS OFFERED, EXCEPT THE ONE THAT CANNOT BE, and the list of
;; verbs comes from the catalogue rather than being written here. A row that
;; named one new verb would have to be edited for the next one, and the verb
;; after that would be offered by nobody's row -- which is how a verb could
;; arrive with a catalogue entry, a README section and no tool.
;;
;; `init` is the exception and it is named rather than filtered out of sight.
;; It is what CREATES a store, so there is no daemon for the shell to send it
;; to; the shell once offered it, sent it to a daemon for a store that did
;; not exist, and the daemon could not start. The route field in the
;; catalogue says `local` for it, and the shell has no local route.
;;
;; Written as a list of one rather than as a `route` test on purpose: if a
;; second verb ever cannot be offered, somebody has to come here and say why.
(define not-offered-by-the-shell '(init))

(want "MC-P1 every verb in the catalogue is offered as a tool, except init"
      (let loop ((es (verb-catalogue)) (missing '()))
        (cond
          ((null? es) (reverse missing))
          ((memq (car (car es)) not-offered-by-the-shell) (loop (cdr es) missing))
          ((and tools-line
                (contains? tools-line
                           (string-append "theourgia_" (symbol->string (car (car es))))))
           (loop (cdr es) missing))
          (else (loop (cdr es) (cons (car (car es)) missing)))))
      '())

;; CONTROL: the exception is a real one. If `init` were offered after all,
;; the list above would be excusing something that does not need excusing.
(want "MC-P1 CONTROL: and init really is absent from the listing"
      (and tools-line (contains? tools-line "theourgia_init"))
      #f)

;; NEVER: THE VERB'S SENTENCE COMES FROM THE SERVER, NOT FROM A LITERAL HERE.
;; This used to pin the string "Execute the core insert command" -- the
;; generic sentence the shell built for every verb, which said nothing
;; about the verb and was what §7.6.45 asked to be replaced. A row
;; holding that literal pins the thing the batch set out to remove.
;;
;; NOTE: SO THE EXPECTATION IS TAKEN FROM THE CATALOGUE, which is the one
;; supplier: the shell asks `describe` for it. What this row asserts is
;; that the shell did NOT write a sentence of its own -- a shell that
;; invented one would not match what the catalogue holds. That the
;; catalogue's sentences are present and non-empty is asserted separately,
;; in `describe.sc`, so this row is not the only thing standing behind
;; them.
(define (catalogue-description verb)
  (let look ((es (verb-catalogue)))
    (cond ((null? es) #f)
          ((eq? (car (car es)) verb) (caddr (car es)))
          (else (look (cdr es))))))

(for-each
  (lambda (pair)
    (let ((d (description-of (car pair)))
          (sentence (catalogue-description (cdr pair))))
      (want (string-append "MC-P1 " (car pair) " carries the protocol, then its own sentence")
            (list (if (and d (>= (string-length d) (string-length write-protocol))
                           (string=? (substring d 0 (string-length write-protocol)) write-protocol))
                      'protocol-first
                      (list 'said (and d (substring d 0 (min 60 (string-length d))))))
                  (cond
                    ((not sentence) 'NO-SUCH-VERB-IN-THE-CATALOGUE)
                    ((and d (contains? d sentence)) 'then-the-verb)
                    (else (list 'NO-VERB-SENTENCE 'wanted sentence))))
            '(protocol-first then-the-verb))))
  (list (cons "theourgia_insert" 'insert)
        (cons "theourgia_write" 'write)))

;; NEVER: AND A TOOL THAT DOES NOT WRITE DOES NOT CARRY IT. Without this row
;; the two above are satisfied by a shell that prefixes every description
;; -- which would put the rules for writing a block in front of `read`,
;; `search` and everything else an agent is choosing between.
(want "MC-P1 TWIN: a reading tool keeps its own sentence and not the protocol"
      (let ((d (description-of "theourgia_read"))
            (sentence (catalogue-description 'read)))
        (list (if (and d (not (contains? d "A block is the unit"))) 'no-protocol (list 'said d))
              (cond
                ((not sentence) 'NO-SUCH-VERB-IN-THE-CATALOGUE)
                ((and d (contains? d sentence)) 'its-own-sentence)
                (else (list 'MISSING 'wanted sentence)))))
      '(no-protocol its-own-sentence))

;; ---- and the shell's own README says where it comes from ----------------------
(want "MC-P1 the MCP README points at the core's string rather than restating it"
      (let ((t (file-text "../mcp/README.md")))
        (list (if (contains? t "write-protocol") 'names-the-constant 'MISSING)
              (if (contains? t "A block is the unit of writing") 'RESTATES-IT 'does-not-restate)))
      '(names-the-constant does-not-restate))

;; ---- MC-P3 the descriptions came from the server, this time -------------
;;
;; NEVER: EVERY ROW ABOVE COMPARES THE LISTING WITH THIS PROCESS'S OWN COPY of
;; `verb-catalogue` and `write-protocol` -- the same values the shell
;; would hold if it carried a hardcoded table, or a cached one from some
;; earlier run. They agree in this tree, and they would go on agreeing
;; after the shell stopped asking anyone.
;;
;; NOTE: SO THE PEER'S ANSWER IS MADE DIFFERENT FROM ANYTHING THIS TREE
;; CONTAINS. A marker that appears in no source file cannot be served
;; from a copy: if the shell shows it, it asked, and it used what came
;; back.
(let* ((psock (string-append sock-here "/p3.sock"))
       (ppeer (string-append here "/p3.sc"))
       (marker "MARKER-ONLY-THE-PEER-KNOWS")
       (pout (string-append here "/p3.out")))
  (call-with-output-file ppeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              (string-append
                "(define reply (string->utf8 "
                "\"(answer (stdout \\\"(ok (verbs (outline (usage (outline)) "
                "(description \\\\\\\"" marker "\\\\\\\") (protocol #t) "
                "(route daemon))) "
                "(protocol \\\\\\\"" marker "-PROTOCOL\\\\\\\"))\\n\\\") "
                "(stderr \\\"\\\") (exit 0) (origin core))\n\"))")
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" psock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
              "               (`(data ,r ,bv) (conn-write! r reply 'last) (serve))"
              "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))"))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " ppeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? psock) 'up)
          ((> k 300) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "THEOURGIA_RUN=" sock-here "/run "
                         "scheme --script " shell " --store " here "/store --socket " psock
                         " < " here "/in.jsonl > " pout " 2>/dev/null"))
  (let ((text (file-text pout)))
    (want "MC-P3 the tool list is what the server answered, not a copy of this tree's"
          (if (contains? text marker) 'came-from-the-server (list 'said-instead
                                                                  (substring text 0 (min 120 (string-length text)))))
          'came-from-the-server)
    ;; NEVER: AND THE PROTOCOL TEXT TOO, which is the part an agent reads
    ;; before it writes anything.
    (want "MC-P3 and so is the protocol text the descriptions carry"
          (if (contains? text (string-append marker "-PROTOCOL")) 'from-the-server 'A-LOCAL-COPY)
          'from-the-server))
  (system (string-append "pkill -f " ppeer " 2>/dev/null")))

(system (string-append "rm -rf " here " " sock-here))
;; NEVER: THE SHELL STARTS A DAEMON NOW, so this fixture must say where its
;; run root is and must take down what it started. Before the shell was
;; rewritten it dispatched in its own process and started nothing, which
;; is why neither line was here. Measured without them: sockets and logs
;; under the user's real `$HOME/.theourgia/run`, and daemons still alive
;; minutes later.

(system (string-append "pkill -f 'serve " here "' 2>/dev/null"))
(system "sleep 1")

(printf "rows: ~a\n~a failures\nf1-protocol complete\n" rows bad)
(exit (if (zero? bad) 0 1))
