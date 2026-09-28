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

;; The MCP shell, driven the way a client drives it: a real process, JSON
;; lines in on stdin, JSON lines out on stdout.
;;
;; KEY: EVERY ROW HERE TALKS TO A CHILD PROCESS. A row that called the
;; shell's procedures would be testing the library and would say nothing
;; about framing, about stdio, or about what a client actually receives.

(import (chezscheme) (theourgia json)
        ;; NOTE: THE PRODUCT'S OWN SPAWN, used by MC-11's control to make a
        ;; child that nothing waits for -- the only way this fixture can
        ;; produce one, measured.
        (only (theourgia ffi) spawn-detached! reap-children!)
        ;; F100b M3a's P7 rows: the key directory the client derives.
        (only (theourgia client) socket-path)
        ;; The shell's own option table, probed as data.
        (only (theourgia arguments) parse-arguments))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; NEVER: A RAISE INSIDE A ROW IS THAT ROW FAILING, not the file ending: a
;; fixture whose sixth row raises reports five passes and no failures.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define pid-text (number->string (get-process-id)))

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

(define here (string-append scratch-base "/mcpshell-" pid-text))
(define sock-here (string-append socket-base "/mcpshell-" pid-text))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define shell "../mcp/server.sc")

;; The canonical path of a directory, asked of the shell rather than of
;; the library whose answer this file is checking.
(define (resolved-by-the-shell dir)
  (let ((out (string-append scratch-base "/mcpshell-resolve-" (number->string (get-process-id)) ".txt")))
    (system (string-append "cd " dir " && pwd -P > " out))
    (let ((t (call-with-input-file out get-string-all)))
      (if (and (string? t) (> (string-length t) 0))
          (substring t 0 (- (string-length t) 1))
          dir))))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; ---- driving the shell -------------------------------------------------------
;;
;; NOTE: THE CHILD IS KEPT OPEN. Several rows need to send one frame, look at
;; what came back, and only then decide what to send next -- a helper that
;; wrote everything and read everything could not ask those questions.
(define (start-shell . options)
  (start-shell* (if (pair? options) (car options) (string-append here "/store"))
                (and (pair? options) (pair? (cdr options)) (cadr options))
                "" ""))

;; NOTE: `launch` goes before `scheme` (an `env ...` prefix: which
;; THEOURGIA_WRITER the shell sees), `extra` after the options the fixture
;; always passes. The shell is EXEC'd, so the pid the port answers is the
;; shell's own, which a derived writer carries.
(define (start-shell* store socket launch extra)
  (let* ((store store)
         ;; NEVER: THE RUN ROOT IS THE FIXTURE'S, NOT THE USER'S. The shell
         ;; now starts a daemon when it cannot reach one, and a daemon
         ;; puts its socket and its log under the run root -- which
         ;; defaults to `$HOME/.theourgia/run`. Measured before this
         ;; line existed: five runs of this file left THIRTY-THREE
         ;; directories in the real one, and fifteen daemons alive.
         ;; A fixture may not write there, and must not leave anything
         ;; running when it is done.
         (command (string-append
                    "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                    "THEOURGIA_RUN=" sock-here "/run exec " launch
                    "scheme --script " shell " --store " store
                    (if socket (string-append " --socket " socket) "")
                    extra
                    " 2>>" here "/shell.err")))
    (let-values (((to from errs pid) (open-process-ports command 'line (native-transcoder))))
      (list to from pid))))

(define (shell-in s) (car s))
(define (shell-pid s) (caddr s))
(define (shell-out s) (cadr s))

(define (send-frame! s text)
  (put-string (shell-in s) text)
  (put-string (shell-in s) "\n")
  (flush-output-port (shell-in s)))

(define (read-frame s) (get-line (shell-out s)))

(define (close-input! s) (close-port (shell-in s)))

;; One conversation: write these frames, close stdin, read every line.
(define (talk frames . options)
  (drain (apply start-shell options) frames))

(define (talk* frames store socket launch extra)
  (drain (start-shell* store socket launch extra) frames))

(define (drain s frames)
  (for-each (lambda (f) (send-frame! s f)) frames)
  (close-input! s)
  (let loop ((out '()))
    (let ((line (read-frame s)))
      (if (eof-object? line)
          (reverse out)
          (loop (cons line out))))))

(define hello
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                 "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                 "\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}"))
(define ready "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")

(define (parse line) (guard (e (#t 'unparseable)) (string->json line)))

;; What is left of an answer after the first occurrence of `marker`; the
;; part before it carries a store-specific writer name.
(define (tail-after text marker)
  (let ((n (string-length marker)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) text)
            ((string=? (substring text i (+ i n)) marker) (substring text (+ i n) m))
            (else (loop (+ i 1)))))))

(define (starts-with-text? text prefix)
  (and (string? text)
       (let ((n (string-length prefix)))
         (and (>= (string-length text) n) (string=? (substring text 0 n) prefix)))))

(define (field line . path)
  (let loop ((v (parse line)) (ks path))
    (cond ((eq? v 'unparseable) 'unparseable)
          ((null? ks) v)
          ((not v) #f)
          (else (loop (json-ref* v (car ks)) (cdr ks))))))

(define (code-of line) (field line "error" "code"))
;; isError AS WRITTEN, or `absent` (F100b M3a review r1, F1): `field` answers #f
;; both for `"isError":false` and for no isError at all, so a row expecting
;; false would pass a result that dropped the member.
(define (is-error-of line)
  (let ((r (field line "result")))
    (if (and (pair? r) (assoc "isError" r)) (cdr (assoc "isError" r)) 'absent)))
(define (id-of line) (field line "id"))

(system (string-append "rm -rf " here " " sock-here "; mkdir -p " here "/store " sock-here))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
          "scheme --script ../core.sc init --store " here "/store > /dev/null 2>&1"))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
          "THEOURGIA_LOCAL=1 scheme --script ../core.sc insert --title MC-CANARY --store "
          here "/store > /dev/null 2>&1"))

;; ---- MC-lifecycle ------------------------------------------------------------
;;
;; NEVER: NOTHING IS SERVED BEFORE THE HANDSHAKE IS FINISHED, and the
;; handshake has two steps, not one. A shell that answered `tools/list`
;; after `initialize` but before the client's `notifications/initialized`
;; would be answering a client that has not said it is ready.
(let ((out (talk (list "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/list\"}"))))
  (want "MC-lifecycle tools/list before initialize is refused"
        (list (length out) (code-of (car out)))
        '(1 -32600)))

(let ((out (talk (list hello "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/list\"}"))))
  (want "MC-lifecycle and still refused between initialize and the ready notification"
        (list (length out) (code-of (cadr out)))
        '(2 -32600)))

(let ((out (talk (list hello))))
  (want "MC-lifecycle initialize pins the protocol version and offers tools"
        (list (field (car out) "result" "protocolVersion")
              (if (json-ref* (field (car out) "result" "capabilities") "tools") 'offers-tools 'no-tools))
        '("2025-11-25" offers-tools)))

(let ((out (talk (list hello ready "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/list\"}"))))
  (want "MC-lifecycle after the notification the catalogue is served"
        (list (length out)
              (if (vector? (field (cadr out) "result" "tools")) 'a-list-of-tools 'no-tools))
        '(2 a-list-of-tools)))

;; NEVER: AND THE NOTIFICATION ITSELF IS NEVER ANSWERED. Two frames in, two
;; answers out -- the middle one produced nothing, which is what the row
;; above counts.
(let ((out (talk (list hello ready hello))))
  (want "MC-lifecycle a second initialize is refused"
        (list (length out) (code-of (cadr out)))
        '(2 -32602)))

;; ---- MC-04 the envelope ------------------------------------------------------
;;
;; NEVER: SIX WAYS TO BE MALFORMED, AND NONE OF THEM DISPATCHES ANYTHING. Each
;; is refused by the shell, in the JSON-RPC error channel, before the
;; core is asked anything at all.
(define (call-with-arguments text)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":"
                 "\"theourgia_outline\",\"arguments\":" text "}}"))

(let ((out (talk (list hello ready
                       (call-with-arguments "{}")
                       (call-with-arguments "{\"argv\":\"x\"}")
                       (call-with-arguments "{\"argv\":[],\"extra\":1}")
                       (call-with-arguments "{\"argv\":[4]}")
                       "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":4}"
                       "{\"jsonrpc\":\"2.0\",\"id\":true,\"method\":\"tools/list\"}"))))
  (want "MC-04 six malformed envelopes are each refused by the shell"
        (map code-of out)
        '(#f -32602 -32602 -32602 -32602 -32602 -32600)))

;; NEVER: AN UNKNOWN METHOD IS -32601, which is a different fact from a
;; malformed one and has to stay different.
(let ((out (talk (list hello ready "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"no/such\"}"))))
  (want "MC-04 an unknown method is method-not-found, not invalid-params"
        (code-of (cadr out)) -32601))

;; NEVER: A BROKEN LINE IS A PARSE ERROR, and the shell keeps going.
(let ((out (talk (list hello ready "{not json" "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"ping\"}"))))
  (want "MC-04 a line that is not JSON is a parse error and the session survives"
        (list (code-of (cadr out)) (id-of (caddr out)))
        '(-32700 8)))

;; NOTE: AN ID COMES BACK AS IT WENT OUT, in its own type. A client matches
;; replies by it, and a string that came back as a number would match
;; nothing.
(let ((out (talk (list hello ready
                       "{\"jsonrpc\":\"2.0\",\"id\":\"s-1\",\"method\":\"ping\"}"
                       "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"ping\"}"))))
  (want "MC-04 ids are echoed in the type they arrived in"
        (list (id-of (cadr out)) (id-of (caddr out)))
        '("s-1" 42)))

;; ---- MC-03 a core refusal is a successful result -----------------------------
;;
;; NEVER: THE COMMAND RAN AND WAS REFUSED, which is an answer. A shell that
;; turned it into a JSON-RPC error would be telling the client its
;; command could not be run.
;; The text a tool returned, dug out of the MCP result envelope.
(define (text-of line)
  (let ((content (field line "result" "content")))
    (if (and (vector? content) (> (vector-length content) 0))
        (let ((first (vector-ref content 0)))
          (or (json-ref* first "text") 'no-text))
        'no-content)))

(define (call-tool name argv)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":"
                 (json->string name) ",\"arguments\":{\"argv\":" (json->string (list->vector argv)) "}}}"))

(let ((out (talk (list hello ready (call-tool "theourgia_read" '("nope.1"))))))
  (want "MC-03 a core refusal comes back as a successful text result"
        (list (is-error-of (cadr out))
              (if (contains? (text-of (cadr out)) "unknown-id") 'the-core-refusal
                  (list 'said (text-of (cadr out)))))
        '(#f the-core-refusal)))

;; ---- MC-03b the third route answers with the same items ---------------------
;;
;; NEVER: AND NOT BYTE FOR BYTE, BECAUSE IT CANNOT BE. The command line and
;; the thin client hand back the same bytes and a row elsewhere says so; the
;; shell wraps the same text in a JSON envelope, so the comparison that can
;; be made here is that the ITEMS are the ones the core produced -- an id and
;; a line number in a `match`. Claiming byte-for-byte across an envelope
;; would be a row that passed by measuring the envelope.
;;
;; The payload is required to be non-empty first. Three routes agreeing that
;; nothing was found is not agreement about anything.
(let* ((made (talk (list hello ready
                         (call-tool "theourgia_insert"
                                    (list "--title" "MCPSEEK" "--text" "a line holding mcpseek")))))
       (found (talk (list hello ready (call-tool "theourgia_grep" '("mcpseek"))))))
  (want "MC-03b CONTROL: the block was written through the shell"
        (if (contains? (text-of (cadr made)) "(ok") 'written
            (list 'said (text-of (cadr made))))
        'written)

  (want "MC-03b the shell answers grep with the core's items, id and line number"
        (let ((t (text-of (cadr found))))
          (list (contains? t "(match ")
                (contains? t "mcpseek")
                (contains? t "(items)")))
        (list #t #t #f)))

;; NEVER: AND IT BRANCHES ON THE TRANSPORT'S TAG, NOT ON THE TEXT. A core
;; answer whose text happens to READ like a transport failure is still an
;; answer -- this one is produced on purpose, and a shell that matched on
;; the words would turn it into a JSON-RPC error.
(let ((out (talk (list hello ready
                       (call-tool "theourgia_insert"
                                  '("--title" "(error transport-unknown (reason lost-answer))"))))))
  (want "MC-03 TWIN: a payload that reads like a transport failure is still a result"
        (list (is-error-of (cadr out))
              (if (starts-with-text? (text-of (cadr out)) "(ok") 'still-a-result
                  (list 'said (text-of (cadr out)))))
        '(#f still-a-result)))

;; ---- MC-argv-verbatim --------------------------------------------------------
;;
;; NEVER: EVERY BYTE OF EVERY ARGUMENT ARRIVES AS IT WAS SENT: leading and
;; trailing spaces, an empty string, a newline inside an argument, shell
;; metacharacters -- NEVER: nothing trimmed, normalised, split or expanded,
;; and NEVER: nothing handed to a shell on the way.
;;
;; NOTE: THE ORACLE IS THE COMMAND LINE, NEVER: NOT MY IDEA OF THE ANSWER.
;; Measured by getting it wrong first: this row compared the argument
;; against the listing and failed, because the core renders non-ASCII
;; with its own `\x…;` escapes -- the argument HAD arrived intact and the
;; expectation was a guess about somebody else's printer. The CLI runs
;; the same dispatcher, so "the same bytes as the CLI" is a question the
;; tree can answer.
(define (cli-answer-on store socket argv)
  (let ((out (string-append here "/cli-out2.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
              "scheme --script ../core.sc " (car argv)
              (apply string-append (map (lambda (a) (string-append " " (shell-quote a))) (cdr argv)))
              " --store " store " --socket " socket " --wire > " out " 2>&1"))
    (file-text out)))

(define (cli-answer store argv)
  (let ((out (string-append here "/cli-out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc " (car argv)
              (apply string-append
                     (map (lambda (a) (string-append " " (shell-quote a))) (cdr argv)))
              " --store " store " --wire > " out " 2>&1"))
    (file-text out)))

;; NOTE: SINGLE QUOTES, WITH THE ONE ESCAPE THAT WORKS INSIDE THEM. This is
;; the fixture handing bytes to a shell, NEVER: which is exactly what the
;; shell under test must never do -- the row would be worthless if the
;; comparison side mangled them.
(define (shell-quote a)
  (string-append "'"
                 (apply string-append
                        (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                             (string->list a)))
                 "'"))

(let* ((odd "  \x6C49;\x5B57;\x1F600;  ")
       (meta "${MC_SENTINEL} $(printf changed) ; \"quoted\"")
       (newlined "one\ntwo")
       (cases (list odd meta newlined ""))
       (store2 (string-append here "/store2"))
       (store3 (string-append here "/store3")))
  (for-each
    (lambda (st)
      (system (string-append "rm -rf " st "; mkdir -p " st))
      (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                             "scheme --script ../core.sc init --store " st " > /dev/null 2>&1")))
    (list store2 store3))
  ;; KEY: `read <arg>` ON A FRESH STORE ECHOES THE ARGUMENT AND NOTHING
  ;; ELSE: `(error unknown-id "<arg>" (nearest ()))`. No writer name, no
  ;; hash, no sequence number -- so two routes into two different stores
  ;; are comparable, which is what an answer carrying ids is not.
  ;; Measured by trying the other way first: comparing `insert` answers
  ;; across two stores fails on the hashes, for a reason that has nothing
  ;; to do with argv.
  (let* ((through-mcp
           (let ((out (talk (cons hello
                                  (cons ready
                                        (map (lambda (a) (call-tool "theourgia_read" (list a)))
                                             cases)))
                            store2)))
             (map text-of (cdr out))))
         (through-cli (map (lambda (a) (cli-answer store3 (list "read" a))) cases)))
    (want "MC-argv-verbatim four awkward arguments reach the core as the CLI's do"
          (map (lambda (m c) (if (equal? m c) 'same-as-the-cli (list 'differ m c)))
               through-mcp through-cli)
          '(same-as-the-cli same-as-the-cli same-as-the-cli same-as-the-cli))
    ;; NEVER: AND ONE OF THEM IS PINNED LITERALLY. Two routes that agree can
    ;; agree about something wrong -- they share a dispatcher. The
    ;; expectation here comes from outside both: it is what the argument
    ;; is, spelled the way the core spells it back.
    ;; NOTE: THE SPELLING CHANGED WITH F3, AND THIS ROW IS THE RECORD OF IT.
    ;; It used to expect `\x6C49;\x5B57;\x1F600;`, because that is how the
    ;; core spelled a non-ASCII character back. Answers now carry the
    ;; characters themselves -- so this expectation is, more directly
    ;; than before, "what the argument is". A user-visible change, and
    ;; the one cell in this suite that states it literally.
    (want "MC-argv-verbatim and the echoed argument is exactly what was sent"
          (car through-mcp)
          "(error unknown-id \"  \x6c49;\x5b57;\x1f600;  \" (nearest ()))\n")
    ;; NEVER: NOBODY LET A SHELL SEE IT. `${MC_SENTINEL}` has a value in this
    ;; fixture's environment that would be visible if any layer had.
    (want "MC-argv-verbatim TWIN: no layer let a shell expand the argument"
          (if (exists (lambda (t) (and (string? t) (contains? t "EXPANDED-BY-A-SHELL")))
                      through-mcp)
              'A-SHELL-SAW-IT
              'nobody-expanded-it)
          'nobody-expanded-it)))

;; ---- MC-05 eval is not a tool ------------------------------------------------
;;
;; NEVER: `eval` IS ABSENT FROM THE CATALOGUE AND INDISTINGUISHABLE FROM A
;; VERB THAT DOES NOT EXIST. A shell that refused it with a message of
;; its own would be telling a client that the capability is there and
;; withheld, which is a different fact and a worse one.
(let ((out (talk (list hello ready
                       "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/list\"}"
                       (call-tool "theourgia_eval" '("(+ 1 1)"))
                       (call-tool "theourgia_no_such_verb_at_all" '())))))
  (want "MC-05 eval is not listed, and calling it is the same as calling nothing"
        (let ((names (let ((tools (field (cadr out) "result" "tools")))
                       (if (vector? tools)
                           (map (lambda (i) (json-ref* (vector-ref tools i) "name"))
                                (let loop ((i 0) (acc '()))
                                  (if (= i (vector-length tools)) (reverse acc)
                                      (loop (+ i 1) (cons i acc)))))
                           '()))))
          (list (if (member "theourgia_eval" names) 'LISTED 'absent)
                (code-of (caddr out))
                (code-of (cadddr out))
                (if (equal? (field (caddr out) "error" "message")
                            (field (cadddr out) "error" "message"))
                    'one-shape-for-both
                    (list (field (caddr out) "error" "message")
                          (field (cadddr out) "error" "message")))))
        '(absent -32602 -32602 one-shape-for-both)))

;; ---- MC-06 which route served it ---------------------------------------------
;;
;; NEVER: THREE WAYS FOR A SOCKET NOT TO BE THERE, AND THE SHELL STARTS A
;; DAEMON RATHER THAN SERVING THEM ITSELF. NOTE: THIS ROW USED TO SAY THE
;; OPPOSITE -- "served locally, three ways" -- and it was right about the
;; shell that then existed: with no daemon it dispatched in its own
;; process, which meant every shell loaded the whole core to answer its
;; first call. That is the cost the split exists to avoid, so the shell
;; now starts a daemon the way the command line does (§7.6.50).
;;
;; NOTE: Each answer must still be the real one, so a shell that refused
;; instead of starting one fails on the content rather than on a tag. The
;; path held by a REGULAR FILE is the one that cannot be served at all --
;; a daemon cannot bind there -- so it is the one that must come back
;; unavailable, and its file must survive.
(let* ((nothing (string-append sock-here "/no-socket-here"))
       (regular (string-append sock-here "/a-regular-file"))
       (folder  (string-append sock-here "/a-directory")))
  (system (string-append "printf keep > " regular "; mkdir -p " folder))
  (let ((answers
          (map (lambda (path)
                 (let ((out (talk (list hello ready (call-tool "theourgia_outline" '()))
                                  (string-append here "/store") path)))
                   (text-of (cadr out))))
               (list nothing))))
    (want "MC-06 an empty socket path gets a daemon, and the answer is the real one"
          (map (lambda (t) (if (starts-with-text? t "(ok (text") 'answered (list 'said t)))
               answers)
          '(answered))
    ;; NEVER: AND A PATH A DAEMON CANNOT TAKE IS REPORTED, NOT WORKED AROUND.
    ;; Neither a regular file nor a directory can be bound; the old shell
    ;; answered anyway by running the verb itself, which is exactly the
    ;; fallback that is gone. What must NOT happen is a silent success.
    ;;
    ;; NOTE: BOTH KINDS, because they fail at different places -- the file
    ;; is refused by the daemon's own check on what is already at the
    ;; path, the directory by `bind` itself -- and a build that handled
    ;; one and not the other would pass a row that named only one.
;; ---- MC-07 what the shell says when no server could be started -----------
    ;;
    ;; NEVER: "EXECUTION MAY BE UNKNOWN" IS FALSE HERE, and it is what the
    ;; shell used to say. A server that would not start means the frame
    ;; never went out, so nothing ran -- and the only useful fact anyone
    ;; had, the server's own reason, was thrown away. The command-line
    ;; client relays that reason verbatim; the shell said something untrue
    ;; instead.
    ;;
    ;; NOTE: IT IS `tools/list` THAT SHOWS THIS, not a tool call: the shell
    ;; asks the server for its catalogue before it can turn a tool name
    ;; into a verb, so a server that will not start is met at that step.
    ;; F100b M3a (L): the shape is item 8's ERROR -32000 "core did not
    ;; start", and the server's own reason is the refusal object's kind.
    (want "MC-07 a server that would not start is reported in its own words"
          (let* ((out (talk (list hello ready
                                  "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/list\"}")
                            (string-append here "/store") regular))
                 (line (cadr out)))
            (list (field line "error" "code") (field line "error" "message")
                  (field line "error" "data" "kind")))
          '(-32000 "core did not start" "serve-path-occupied"))

    ;; NEVER: AND THE TWIN: the sentence about an unknown outcome still exists,
    ;; for the case where it is true. Without this row the one above is
    ;; passed by a shell that simply stopped saying "unknown" -- and a
    ;; request that WAS sent and then lost must keep saying so.
    ;;
    ;; NOTE: THE PEER HERE READS THE FRAME AND CLOSES, which is "sent, then
    ;; lost" with nothing else in it. Written first with the fault
    ;; injector that parks a request, this row failed for a reason of the
    ;; injector's own and told me nothing about the shell.
    (let ((lostsock (string-append sock-here "/lost.sock"))
          (lostpeer (string-append here "/lost.sc")))
      (call-with-output-file lostpeer
        (lambda (port)
          (for-each (lambda (l) (display l port) (newline port))
            (list "(import (chezscheme) (theourgia sched) (theourgia net))"
                  "(start-scheduler"
                  "  (lambda ()"
                  (string-append "    (listen! \"" lostsock "\" 16)")
                  "    (let serve ()"
                  "      (receive (after 20000 (exit 0))"
                  "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
                  ;; NEVER: THE BYTES ARE READ AND THEN THE CONNECTION CLOSES.
                  ;; Reading first is what makes this "the request
                  ;; arrived", not "the dial failed".
                  "               (`(data ,r ,bv) (conn-close! r) (serve))"
                  "               (`(eof ,r) (serve))"
                  "               (`#(DOWN ,w ,y) (serve))))))"))))
      (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                             "scheme --script " lostpeer " > /dev/null 2>&1 &"))
      (let up ((k 0))
        (cond ((file-exists? lostsock) 'up)
              ((> k 300) 'never)
              (else (system "sleep 0.05") (up (+ k 1)))))
      (let* ((out (talk (list hello ready
                              "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/list\"}")
                        (string-append here "/store") lostsock))
             (message (field (cadr out) "error" "message")))
        (system (string-append "pkill -f " lostpeer " 2>/dev/null"))
        (want "MC-07 TWIN: an answer lost after the request went out is still unknown"
              (if (and (string? message) (contains? message "may be unknown"))
                  'still-unknown
                  (list 'said message))
              'still-unknown)))

    (want "MC-06 a socket path that a daemon cannot take is not served"
          (map (lambda (path)
                 (let ((out (talk (list hello ready (call-tool "theourgia_outline" '()))
                                  (string-append here "/store") path)))
                   (if (starts-with-text? (text-of (cadr out)) "(ok (text")
                       (list 'SERVED-ANYWAY (text-of (cadr out)))
                       'not-served)))
               (list regular folder))
          '(not-served not-served))
    ;; NEVER: AND THE REGULAR FILE IS STILL THERE. Falling back must not mean
    ;; tidying up something that is not ours.
    (want "MC-06 TWIN: the regular file on the socket path was left alone"
          (file-text regular) "keep")))

;; ---- MC-daemon-reach ---------------------------------------------------------
;;
;; KEY: THIS IS THE ROW THE DAEMON DELIVERIES CARRIED AS A RISK. The Python
;; shell spoke an envelope the Scheme daemon does not answer, so with a
;; daemon running MCP was unavailable -- measured then as
;; `(error transport-invalid-answer)`. This row is what closes it.
;;
;; NEVER: THREE WITNESSES, because the answer alone proves nothing: the same
;; verb answers the same way locally, which is the entire point of having
;; two routes. So the row also reads the DAEMON's own record that it
;; dispatched, and the SHELL's own trace for the absence of a local
;; store open.
(let* ((dstore (string-append here "/dstore"))
       (dsock (string-append sock-here "/d.sock"))
       (runner (string-append here "/serve.sc"))
       (dlog (string-append here "/daemon.log"))
       (shell-err (string-append here "/shell.err")))
  (system (string-append "rm -rf " dstore " " dsock "; mkdir -p " dstore))
  (call-with-output-file runner
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
              (string-append "(rpc-dispatch \"" dstore "\" '(init) \"tester\")")
              (string-append "(rpc-dispatch \"" dstore "\" '(insert \"--title\" \"ONLY-IN-THE-DAEMON\") \"tester\")")
              (string-append "(serve \"" dstore "\" \"" dsock "\")")))))
  (system (string-append "THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" libs
                         " CHEZSCHEMELIBEXTS='" exts "' scheme --script " runner
                         " > " dlog " 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? dsock) 'up)
          ((> k 200) 'never)
          (else (system "sleep 0.05") (up (+ k 1)))))
  (system (string-append "rm -f " shell-err))
  (let* ((out (talk (list hello ready (call-tool "theourgia_outline" '())) dstore dsock))
         (text (text-of (cadr out))))
    (system (string-append "pkill -f " runner " 2>/dev/null"))
    (want "MC-daemon-reach the shell reaches a Scheme daemon, and three things say so"
          (list (if (contains? text "ONLY-IN-THE-DAEMON") 'the-daemons-answer (list 'said text))
                (if (contains? (file-text dlog) "daemon-dispatch") 'the-daemon-dispatched-it 'NO-DISPATCH)
                (if (contains? (file-text shell-err) "log-open") 'ALSO-RAN-LOCALLY 'no-local-open))
          '(the-daemons-answer the-daemon-dispatched-it no-local-open))))

;; ---- MC-frame-limit ----------------------------------------------------------
;;
;; NEVER: THE LIMIT IS IN BYTES AND IT COUNTS THE TERMINATOR. A frame of
;; exactly a mebibyte is legal; one byte more is refused -- and refused
;; without being parsed, so nothing is dispatched.
(let* ((big (string-append here "/big.json"))
       (out (string-append here "/big.out")))
  (system (string-append
            "python3 - <<'EOF'\n"
            "pad = 'x' * (1024*1024 - len('{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"pad\":\"\"}') - 1 + 1)\n"
            "open('" big "','w').write('{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"pad\":\"' + pad + '\"}\\n')\n"
            "EOF\n"))
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " shell " --store " here "/store < " big
                         " > " out " 2>/dev/null"))
  (want "MC-frame-limit a frame past the limit is refused, not parsed"
        (let ((answer (file-text out)))
          (list (if (contains? answer "-32600") 'refused (list 'said answer))
                (if (contains? answer "result") 'AND-ALSO-ANSWERED 'nothing-else)))
        '(refused nothing-else)))

;; ---- a daemon whose first writer request parks ---------------------------
;;
;; NOTE: THE PAUSE HAS TO YIELD, NEVER: not block. A probe that slept on the
;; scheduler's thread would stop everything, and then a shell that
;; answered calls concurrently would look serial too -- the row would
;; pass against the implementation it exists to refuse. The daemon's
;; `writer-hold` seam parks inside a lock with `sleep-ms`, which yields.
(define (start-parking-daemon! tag)
  (let* ((dstore (string-append here "/" tag "-store"))
         (dsock (string-append sock-here "/" tag ".sock"))
         (runner (string-append here "/" tag ".sc"))
         (dlog (string-append here "/" tag ".log")))
    (system (string-append "rm -rf " dstore " " dsock "; mkdir -p " dstore))
    (call-with-output-file runner
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
                (string-append "(rpc-dispatch \"" dstore "\" '(init) \"tester\")")
                (string-append "(serve \"" dstore "\" \"" dsock "\")")))))
    (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=writer-hold@conn "
                           "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "scheme --script " runner " > " dlog " 2>&1 &"))
    (let up ((k 0))
      (cond ((file-exists? dsock) 'up)
            ((> k 200) 'never)
            (else (system "sleep 0.05") (up (+ k 1)))))
    (list dstore dsock runner dlog)))

(define (now-ms) (real-time))

;; ---- MC-serial ---------------------------------------------------------------
;;
;; NEVER: ONE CALL AT A TIME. Both frames are written before either is
;; answered, and the slow one was sent first: a shell that served them
;; concurrently would answer the fast one first, because the slow one is
;; parked for a second and a half.
(let* ((d (start-parking-daemon! "serial"))
       (s (start-shell (car d) (cadr d))))
  (send-frame! s hello)
  (send-frame! s ready)
  (read-frame s)
  (send-frame! s (call-tool "theourgia_drafts" '("--writer" "w1")))
  (send-frame! s (call-tool "theourgia_outline" '()))
  (let* ((t0 (now-ms))
         (first (read-frame s))
         (t1 (now-ms))
         (second (read-frame s)))
    (close-input! s)
    ;; NEVER: AND SOMEBODY ELSE COULD GET ON WITH IT. While the first call was
    ;; parked, an independent caller reached the same daemon -- so "the
    ;; second answer waited" is a fact about this shell's serialisation,
    ;; NEVER: not about a daemon that had stopped.
    (let ((elsewhere (cli-answer-on (car d) (cadr d) (list "outline"))))
      (system (string-append "pkill -f " (caddr d) " 2>/dev/null"))
      (want "MC-serial the slow call is answered before the fast one is started"
            (list (if (> (- t1 t0) 900) 'the-first-took-its-time (list 'at (- t1 t0)))
                  (if (starts-with-text? (text-of first) "(ok (items") 'the-slow-one-first
                      (list 'first-said (text-of first)))
                  (if (starts-with-text? (text-of second) "(ok (text") 'then-the-fast-one
                      (list 'second-said (text-of second)))
                  (if (starts-with-text? elsewhere "(ok") 'and-the-daemon-served-others
                      (list 'elsewhere elsewhere)))
            '(the-first-took-its-time the-slow-one-first then-the-fast-one
              and-the-daemon-served-others)))))

;; ---- MC-eof-drain ------------------------------------------------------------
;;
;; NEVER: A CALL THAT HAS BEEN ACCEPTED IS ANSWERED, even though the client
;; has gone. NOTE: The row closes stdin while the call is parked -- after the
;; frame is in, before the answer is out -- which is the only window in
;; which "drain" means anything.
(let* ((d (start-parking-daemon! "eofdrain"))
       (s (start-shell (car d) (cadr d))))
  (send-frame! s hello)
  (send-frame! s ready)
  (read-frame s)
  (send-frame! s (call-tool "theourgia_drafts" '("--writer" "w1")))
  (system "sleep 0.3")
  (close-input! s)
  (let* ((answer (read-frame s))
         (after (read-frame s)))
    (system (string-append "pkill -f " (caddr d) " 2>/dev/null"))
    (want "MC-eof-drain a call in flight is answered after stdin closes, then the shell ends"
          (list (if (starts-with-text? (text-of answer) "(ok (items")
                    'the-parked-call-was-answered
                    (list 'said (text-of answer)))
                (if (eof-object? after) 'and-then-it-ended (list 'said-more after)))
          '(the-parked-call-was-answered and-then-it-ended))))

;; ---- MC-envelope-shared ------------------------------------------------------
;;
;; NEVER: THE SHELL AND THE COMMAND LINE SEND THE SAME ENVELOPE, and the row
;; reads the BYTES rather than trusting that both call the same
;; procedure. A controlled listener records what arrives; the two routes
;; are driven at it in turn and what they sent is compared.
;;
;; NOTE: THE CAPTURE IS WHY THIS IS A ROW AND NOT A GREP. "Both call
;; `request-frame`" is a fact about today's source; "both sent these
;; bytes" is a fact about the programs. The seed that proves it: give
;; `request-frame` an extra field, and both captures must change.
(let* ((csock (string-append sock-here "/capture.sock"))
       (cstore (string-append here "/store"))
       (peer (string-append here "/capture.sc"))
       (seen-cli (string-append here "/seen-cli.txt"))
       (seen-mcp (string-append here "/seen-mcp.txt")))
  (define (start-capture! into)
    (system (string-append "rm -f " csock " " into))
    (call-with-output-file peer
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          ;; NEVER: THE PEER ANSWERS `describe` BEFORE IT CAPTURES ANYTHING.
          ;; The shell asks for the catalogue before it can turn a tool
          ;; name into a verb, so the FIRST frame it sends is always
          ;; `describe` -- and a peer that recorded the first frame and
          ;; hung up recorded that, then never saw the tool request at
          ;; all. Measured: this row compared the CLI's `read` envelope
          ;; against the shell's `describe` envelope and reported that
          ;; the two routes disagreed, which was true and about nothing.
          ;;
          ;; NOTE: The catalogue it answers with is the smallest one that
          ;; contains the tool this row calls. It is a STAND-IN for the
          ;; server, and the rows about what the real catalogue holds are
          ;; in `describe.sc`; what is being measured here is only the
          ;; bytes of the request that follows.
          (list "(import (chezscheme) (theourgia sched) (theourgia net))"
                (string-append
                  ;; NOTE: THE STUB CARRIES EVERY FIELD THE SHELL READS, and
                  ;; `route` was added to that list after this was written.
                  ;; Without it the shell correctly dropped the verb --
                  ;; a tool it cannot carry out is not offered -- so no
                  ;; tool call followed and the capture came back empty.
                  ;; A stand-in stops standing in the moment the thing it
                  ;; models grows a field.
                  "(define catalogue-reply (string->utf8 "
                  "\"(answer (stdout \\\"(ok (verbs (read (usage (read <id>)) "
                  "(description \\\\\\\"Read a block.\\\\\\\") (protocol #f) "
                  "(route daemon))) "
                  "(protocol \\\\\\\"P\\\\\\\"))\\n\\\") (stderr \\\"\\\") "
                  "(exit 0))\n\"))")
                ;; NEVER: WHICH FRAME TO ANSWER IS DECIDED BY WHAT IT IS, not
                ;; by whether it is the first. Written as "answer the
                ;; first, record the second" this swallowed the COMMAND
                ;; LINE's request: the CLI never asks for a catalogue, so
                ;; its first frame is the one that was supposed to be
                ;; recorded, and the capture came back empty while the
                ;; shell's side looked fine.
                "(define (asks-to-describe? bv)"
                "  (let* ((t (utf8->string bv)) (n (string-length t)))"
                "    (let loop ((i 0))"
                "      (cond ((> (+ i 8) n) #f)"
                "            ((string=? (substring t i (+ i 8)) \"describe\") #t)"
                "            (else (loop (+ i 1)))))))"
                "(start-scheduler"
                "  (lambda ()"
                (string-append "    (listen! \"" csock "\" 16)")
                "    (let serve ((answered #f))"
                "      (receive (after 20000 (exit 0))"
                "               (`(accepted ,ref) (conn-read-start! ref) (serve answered))"
                (string-append
                  "               (`(data ,r ,bv)"
                  "                (if (asks-to-describe? bv)"
                  "                    (begin (conn-write! r catalogue-reply 'last)"
                  "                           (serve #t))"
                  "                    (begin (call-with-output-file \"" into
                  "\" (lambda (p) (put-string p (utf8->string bv))) 'truncate)"
                  "                           (conn-close! r) (serve answered))))")
                "               (`(written ,r ,t ,st) (conn-close! r) (serve answered))"
                "               (`(eof ,r) (serve answered))"
                "               (`#(DOWN ,w ,y) (serve answered))))))")))
      'truncate)
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "scheme --script " peer " > /dev/null 2>&1 &"))
    (let up ((k 0))
      (cond ((file-exists? csock) 'up)
            ((> k 200) 'never)
            (else (system "sleep 0.05") (up (+ k 1))))))

  ;; NOTE: BOTH ROUTES NAME THE WRITER `w`, EACH THE WAY IT TAKES ONE. The
  ;; shell never sends writer #f: without a writer it derives one for its
  ;; session, which the command line does not, and the two envelopes would
  ;; differ in that one field for a reason this row is not about. The
  ;; command line's envelope carries only its `--writer` option (it does not
  ;; read THEOURGIA_WRITER), and that option stays in its arguments; so the
  ;; command line is given `--writer w`, and the shell is started with
  ;; `--writer w` and called with the same arguments.
  (start-capture! seen-cli)
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script ../core.sc read x.1 --writer w --wire --store " cstore
                         " --socket " csock " > /dev/null 2>&1"))
  (system (string-append "pkill -f " peer " 2>/dev/null"))
  (start-capture! seen-mcp)
  (talk* (list hello ready (call-tool "theourgia_read" '("x.1" "--writer" "w"))) cstore csock "env -u THEOURGIA_WRITER " " --writer w")
  (system (string-append "pkill -f " peer " 2>/dev/null"))

;; NOTE: THE COMMAND LINE IS DRIVEN WITH `--wire` HERE, and that is not a
  ;; convenience. The envelope now carries the MODE the caller wants its
  ;; answer rendered in, and the shell always wants `wire` because it
  ;; parses what comes back. A CLI run without `--wire` therefore sends a
  ;; genuinely different envelope -- measured, the two differed in that
  ;; one field and in nothing else -- and comparing them would be asking
  ;; two callers who want different things to say the same thing.
  (want "MC-envelope-shared the shell and the command line put the same bytes on the wire"
        (let ((a (file-text seen-cli)) (b (file-text seen-mcp)))
          (list (if (> (string-length a) 0) 'the-cli-sent-something (list 'cli a))
                (if (string=? a b) 'and-the-shell-sent-the-same (list 'differ a b))))
        '(the-cli-sent-something and-the-shell-sent-the-same))

  ;; NEVER: AND IT IS THE ENVELOPE THE DAEMON PARSES, spelled out here once so
  ;; that "both sent the same thing" cannot be satisfied by both sending
  ;; the same wrong thing.
  ;; NOTE: THE FIELDS ARE SPELLED OUT, INCLUDING THE ONE THAT IS #f.
  ;; `stdin` is absent here as a VALUE and not by being left out: an
  ;; envelope whose length varied with what the caller happened to
  ;; have would be one the reader had to guess about. The writer is the one
  ;; both routes were given, `w`, and a build that started omitting the
  ;; field would fail here rather than at the far end of a parse.
  ;; NOTE: THE STORE TRAVELS BY ITS RESOLVED NAME, so the expectation is the
  ;; resolved one -- and it is resolved by the SHELL, not by the library
  ;; under test. Asking `client.sc` what it would produce would compare
  ;; this file's copy of the rule with the rule itself and agree with any
  ;; answer. (`/tmp` is a symlink on this platform, which is what makes
  ;; the two spellings differ at all.)
  ;; NOTE: THE DIRECTORY IS IN THE ENVELOPE NOW, and it is this fixture's own,
  ;; resolved by the shell rather than by the library under test.
  (want "MC-envelope-shared and the bytes are the request envelope, terminator and all"
        (file-text seen-mcp)
        (string-append "(request 1 \"" (resolved-by-the-shell cstore) "\" \""
                       (or (getenv "THEOURGIA_ACTOR") (getenv "USER") "cli")
                       "\" \"w\" wire \"" (resolved-by-the-shell ".") "\" #f read \"x.1\" \"--writer\" \"w\")\n")))

;; ---- MC-08 two ways a well-formed request was mishandled --------------------
;;
;; NEVER: "PRESENT AND false" IS NOT "ABSENT". The parser answered #f for both
;; a missing `params` key and one whose value is `false`, so
;; `params: false` -- which the protocol does not allow -- was read as no
;; params and the call SUCCEEDED.
(let ((out (talk (list hello ready
                       "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/list\",\"params\":false}"))))
  (want "MC-08 params given as false is refused, not read as absent"
        (code-of (cadr out))
        -32602))

;; NEVER: AND THE TWIN: no params at all is still fine, which is how every
;; ordinary listing arrives.
(let ((out (talk (list hello ready
                       "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/list\"}"))))
  (want "MC-08 TWIN: a listing with no params is still served"
        (if (field (cadr out) "result" "tools") 'served (list 'said (cadr out)))
        'served))


;; ---- MC-11 collecting the children a start leaves behind -----------------
;;
;; NEVER: WHAT THIS ROW CAN ESTABLISH, AND WHAT IT CANNOT. `spawn-detached!`
;; answers a pid and nothing waits for it, so a started process that exits
;; stays in the table until someone collects it -- measured by the review
;; on the primitive itself, with `/usr/bin/true` and `waitpid`. The shell
;; now collects before every call.
;;
;; NEVER: THE SHELL'S OWN ACCUMULATION COULD NOT BE PRODUCED FROM OUTSIDE, and
;; a row claiming it would be green for a reason that is not the claim.
;; Measured twice while writing this, sampling every two seconds across a
;; whole call: the shell had NO children at all. A start happens only when
;; the connect fails with an errno that means "nobody is listening"; a
;; socket path holding a regular file answers ENOTSOCK, which is `not-sent`
;; and starts nothing, and a path the client cannot use is refused before
;; spawning. When a start does happen the daemon binds and stays alive,
;; so it is a child and not a corpse. The case that leaves corpses is the
;; lock race -- two starts, the loser exiting at once -- which this fixture
;; cannot hold still.
;;
;; NOTE: SO THE ROW BELOW IS ABOUT THE COUNTING, NOT ABOUT THE SHELL. It
;; proves the instrument can see an uncollected child; the claim that the
;; shell does not accumulate them rests on the review's measurement of the
;; primitive and on reading `ask`, and is written up as such in the
;; delivery notes rather than dressed as a measurement here.
(begin
  ;; NOTE: THE INSTRUMENT IS `reap-children!` ITSELF, not a `ps` line. Asked
  ;; as `ps -o stat=,ppid=`, the listing covers only the processes of the
  ;; asking terminal, so in a suite with no terminal it answered zero while
  ;; a child was demonstrably there -- measured, this row read BLIND in the
  ;; same run in which collecting took one away. A reading that depends on
  ;; where the suite was started from is not a reading.
  (want "MC-11 a child that nothing waited for is collected, and counted"
        (begin
          (spawn-detached! (list "/usr/bin/true"))
          (system "sleep 1")
          (let* ((took (reap-children!))
                 (again (reap-children!)))
            (list (if (> took 0) 'collected-it (list 'took took))
                  (if (= again 0) 'and-nothing-was-left (list 'still-there again)))))
        '(collected-it and-nothing-was-left))

  ;; NEVER: AND IT ANSWERS ZERO WHEN THERE IS NOTHING, which is what makes the
  ;; count above evidence rather than a number that is always positive.
  (want "MC-11 CONTROL: with no child of its own it collects nothing"
        (reap-children!)
        0))


;; ---- MC-09 a tool call that could not be sent says whose id it answers ----
;;
;; NEVER: MEASURED DEFECT. `catalogue` carries `start-failed` and `not-sent`
;; out of itself deliberately -- the comment where it does says the
;; reason would otherwise be lost one layer before the place that reports
;; it -- and `tools/call` checked only for `unavailable`. The other two
;; fell through to `assoc` on an error datum, which raised, and the guard
;; around the request answered `-32603 "Core transport unavailable"` with
;; `"id": null`. So the client could not match the failure to the request
;; that caused it, and the reason the catalogue had preserved was thrown
;; away exactly where it was meant to be used.
;;
;; NOTE: MC-06 CANNOT SAY THIS: it asks only that no outline result came
;; back, which is true of a null-id internal error as well.
(let* ((badsock (string-append sock-here "/not-a-socket-file"))
       (out (begin
              (system (string-append "printf keep > " badsock))
              (talk (list hello ready
                          "{\"jsonrpc\":\"2.0\",\"id\":41,\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_outline\",\"arguments\":{\"argv\":[]}}}")
                    (string-append here "/store") badsock)))
       (reply (cadr out))
       (message (field reply "error" "message")))
  (want "MC-09 a tool call that could not be sent is answered under its own id"
        (if (contains? reply "\"id\":41") 'echoed (list 'said reply))
        'echoed)
  ;; NEVER: AND IT SAYS THE TOOL DID NOT RUN. "Something went wrong" leaves
  ;; the caller to decide whether to try again, which is the one thing it
  ;; must not have to guess about.
  ;; F100b M3a (L): the shape is item 8's tools/call RESULT with isError, and
  ;; "it did not run" is its refusal: a start that failed, in the datum.
  (want "MC-09 and it says the request was not carried out: a RESULT with isError whose refusal is a failed start"
        (list (field reply "result" "isError")
              (let ((d (field reply "result" "_meta" "refusal" "datum")))
                (and (string? d) (starts-with-text? d "(error serve-start-failed"))))
        '(#t #t))
  ;; NEVER: THE TWIN: tools/list, the same failure, the same shape. The two
  ;; branched differently for a whole release -- one of them handled all
  ;; three outcomes and the other did not -- so the rows have to compare
  ;; them rather than check each alone.
  (let* ((lout (talk (list hello ready
                           "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"tools/list\"}")
                     (string-append here "/store") badsock))
         (lmessage (field (cadr lout) "error" "message")))
    ;; F100b M3a (L): tools/list's ERROR -32000 names the same kind as the
    ;; tool call's refusal, so the two still say the same thing.
    (want "MC-09 TWIN: tools/list fails the same way for the same reason"
          (list (if (contains? (cadr lout) "\"id\":42") 'echoed (list 'said (cadr lout)))
                (field (cadr lout) "error" "code") lmessage
                (let ((k (field (cadr lout) "error" "data" "kind")))
                  (and (string? k) (equal? k (field reply "result" "_meta" "refusal" "kind")))))
          '(echoed -32000 "core did not start" #t))))

;; ---- MC-10 who refused: the core, or the daemon carrying the request -----
;;
;; NEVER: §7.6.4 SAYS A CORE `(error ...)` IS A SUCCESSFUL TOOL CALL whose text
;; is a refusal -- the agent asked, and the answer is no. A daemon that is
;; draining, or that lost the process serving the request, refuses in the
;; SAME envelope with the SAME shape, and those are not answers to the
;; question at all. Read as tool results they came back `isError: false`
;; with the refusal as their text, so a caller was told its request had
;; been carried out and answered when it had not been carried out at all.
;;
;; NOTE: THE ENVELOPE NOW CARRIES `origin`, and these two rows are the two
;; sides of it. A stand-in peer answers, because a real daemon cannot be
;; made to produce both on demand.
(let* ((osock (string-append sock-here "/origin.sock"))
       (opeer (string-append here "/origin.sc"))
       (ostore (string-append here "/store")))
  (define (peer-answering body)
    (call-with-output-file opeer
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia sched) (theourgia net))"
                "(define catalogue-reply (string->utf8 "
                "\"(answer (stdout \\\"(ok (verbs (outline (usage (outline)) "
                "(description \\\\\\\"Outline.\\\\\\\") (protocol #f) "
                "(route daemon))) "
                "(protocol \\\\\\\"P\\\\\\\"))\\n\\\") (stderr \\\"\\\") "
                "(exit 0) (origin core))\n\"))"
                (string-append "(define tool-reply (string->utf8 \"" body "\n\"))")
                "(define (asks-to-describe? bv)"
                "  (let* ((t (utf8->string bv)) (n (string-length t)))"
                "    (let loop ((i 0))"
                "      (cond ((> (+ i 8) n) #f)"
                "            ((string=? (substring t i (+ i 8)) \"describe\") #t)"
                "            (else (loop (+ i 1)))))))"
                "(start-scheduler"
                "  (lambda ()"
                (string-append "    (listen! \"" osock "\" 16)")
                "    (let serve ()"
                "      (receive (after 20000 (exit 0))"
                "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
                "               (`(data ,r ,bv)"
                "                 (conn-write! r (if (asks-to-describe? bv) catalogue-reply tool-reply) 'last)"
                "                 (serve))"
                "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
                "               (`(eof ,r) (serve))"
                "               (`#(DOWN ,w ,y) (serve))))))")))
      'truncate)
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "scheme --script " opeer " > /dev/null 2>&1 &"))
    (let up ((k 0))
      (cond ((file-exists? osock) 'up)
            ((> k 300) 'never)
            (else (system "sleep 0.05") (up (+ k 1))))))

  (define (call-through) 
    (cadr (talk (list hello ready (call-tool "theourgia_outline" '())) ostore osock)))

  (peer-answering "(answer (stdout \\\"(error draining)\\\") (stderr \\\"\\\") (exit 1) (origin transport))")
  (let ((reply (call-through)))
    (system (string-append "pkill -f " opeer " 2>/dev/null"))
    (system (string-append "rm -f " osock))
    ;; F100b M3a (L): the daemon's refusal is item 8's tools/call RESULT with
    ;; isError, and origin transport says who refused. It is never a result
    ;; the tool gave: isError is true and the refusal is the daemon's.
    (want "MC-10 a daemon that declined to carry the request is a RESULT with isError, its refusal the daemon's (origin transport)"
          (list (field reply "result" "isError") (field reply "result" "_meta" "refusal" "kind")
                (field reply "result" "_meta" "refusal" "origin"))
          '(#t "draining" "transport")))

  ;; NEVER: THE TWIN, AND IT IS THE RULE THAT MUST NOT BREAK. A core refusal
  ;; is a SUCCESSFUL call: the tool ran and its answer is no. A build that
  ;; turned every non-zero exit into an error would pass the row above and
  ;; fail this one -- which is exactly what §7.6.4 forbids.
  (peer-answering "(answer (stdout \\\"(error unknown-id \\\\\\\"x.1\\\\\\\")\\\") (stderr \\\"\\\") (exit 1) (origin core))")
  (let ((reply (call-through)))
    (system (string-append "pkill -f " opeer " 2>/dev/null"))
    (system (string-append "rm -f " osock))
    ;; isError false TOO (F100b M3a): the daemon's refusal is now a RESULT
    ;; as well, so "not a JSON-RPC error" no longer tells the two apart.
    (want "MC-10 TWIN: a core refusal is still a successful tool call (isError false)"
          (list (if (field reply "error" "message") 'AN-ERROR 'a-result)
                ;; IN THE RESULT'S TEXT (M3a review r2, F2), not anywhere in
                ;; the frame: a reply with no content could carry the words in
                ;; another member.
                (let ((t (text-of reply))) (if (and (string? t) (contains? t "unknown-id")) 'carries-the-refusal (list 'said reply)))
                (is-error-of reply))
          '(a-result carries-the-refusal #f)))

  ;; NEVER: AND THE FIELD IS WHAT DECIDES, NOT THE WORDS. The two rows above
  ;; use a transport refusal that SAYS `draining` and a core refusal that
  ;; says `unknown-id`, so a shell that read the text and recognised names
  ;; would pass both. These two send the SAME words under the two
  ;; provenances: a build that classifies by reading them gets one wrong.
  (peer-answering "(answer (stdout \\\"(error draining)\\\") (stderr \\\"\\\") (exit 1) (origin core))")
  (let ((reply (call-through)))
    (system (string-append "pkill -f " opeer " 2>/dev/null"))
    (system (string-append "rm -f " osock))
    (want "MC-10 the same words marked as the core's are a successful call (isError false), carrying the core's words"
          (list (if (field reply "error" "message") 'AN-ERROR 'a-result) (is-error-of reply)
                (let ((t (text-of reply))) (if (and (string? t) (contains? t "(error draining)")) 'carries-the-words (list 'said reply))))
          '(a-result #f carries-the-words)))

  (peer-answering "(answer (stdout \\\"(error draining)\\\") (stderr \\\"\\\") (exit 1) (origin transport))")
  (let ((reply (call-through)))
    (system (string-append "pkill -f " opeer " 2>/dev/null"))
    (system (string-append "rm -f " osock))
    (want "MC-10 and the same words marked as the transport's are a refusal the daemon made (isError, origin transport)"
          (list (field reply "result" "isError") (field reply "result" "_meta" "refusal" "origin"))
          '(#t "transport"))))

;; NEVER: AN INTEGER-VALUED ID IS AN ID, HOWEVER IT WAS SPELLED. Requiring an
;; exact integer rejected `2.0` -- ordinary JSON for the number two -- and
;; answered with `id: null`, so a client matching replies by id could not
;; match its own.
(let ((out (talk (list hello ready "{\"jsonrpc\":\"2.0\",\"id\":2.0,\"method\":\"ping\"}"))))
  (want "MC-08 an id written 2.0 is answered, not refused"
        (if (contains? (cadr out) "\"error\"") (list 'refused (cadr out)) 'answered)
        'answered))

;; NOTE: AND THE ID COMES BACK AS IT WAS WRITTEN.
;;
;; NEVER: "NOT NULL" WAS NOT ENOUGH, and this row used to ask only that. A
;; reply carrying `3`, or `"2.0"`, or an id of a different type passed it
;; -- every answer except the one failure it was named for. What a client
;; matches on is the id's exact text, so that is what is compared.
;;
;; NEVER: AND ONE OF THESE CANNOT BE PRINTED BACK FROM THE PARSED NUMBER.
;; `9007199254740993` is not representable as a double: parsing rounds it
;; to ...992, so a reply built by printing the parsed value answers a
;; DIFFERENT id (measured: `9.007199254740992e15`) and the client that
;; sent it cannot match its own reply. `2e0` is the same failure in a
;; smaller form -- it came back as `2.0`, which is the same number
;; spelled differently, and a client comparing text would miss it.
(define (echoes-id? spelling)
  (let ((out (talk (list hello ready
                         (string-append "{\"jsonrpc\":\"2.0\",\"id\":" spelling
                                        ",\"method\":\"ping\"}")))))
    (if (and (pair? out) (pair? (cdr out))
             (contains? (cadr out) (string-append "\"id\":" spelling ",")))
        'echoed
        (list spelling 'came-back-as (and (pair? out) (pair? (cdr out)) (cadr out))))))

(want "MC-08 and the id it sent is the id it gets back, as written"
      (map echoes-id? '("2.0" "2e0" "9007199254740993.0"))
      '(echoed echoed echoed))

;; NEVER: THE TWIN: an ordinary integer and a string id are unchanged by all
;; of this. Without them, "echo the token you were sent" is satisfied by a
;; build that has stopped parsing ids at all.
(want "MC-08 TWIN: an ordinary integer id and a string id still echo"
      (list (echoes-id? "7")
            (let ((out (talk (list hello ready
                                   "{\"jsonrpc\":\"2.0\",\"id\":\"abc\",\"method\":\"ping\"}"))))
              (if (contains? (cadr out) "\"id\":\"abc\"") 'echoed (list 'said (cadr out)))))
      '(echoed echoed))


;; ---- MC-12 a key is what it means, and a reply is always JSON ------------
;;
;; NEVER: `"id"` IS JSON FOR `id`. The scan that finds the id's own token
;; compared the key's SPELLING against `"id"`, so a host that escaped a
;; character in the key -- which JSON allows anywhere -- was not
;; recognised, and the reply fell back to printing the parsed value: the
;; one path the token echo exists to avoid.
;;
;; KEY: AND THE FALLBACK COULD WRITE SOMETHING THAT WAS NOT JSON. Chez
;; prints a flonum with fewer significant bits than a full mantissa as
;; `5e-324|1`, and that bar is not JSON, so the whole line stopped being
;; parseable -- measured on the previous build:
;;
;;   {"jsonrpc":"2.0","id":5e-324|1,"result":{}}
;;
;; NOTE: SO THE ROW ASKS THE STRONGEST QUESTION AVAILABLE: not "does the text
;; look right" but "does this line parse at all". A client that cannot
;; read the frame has lost the session, not one answer.
(define (ping-keyed key id)
  (let ((out (talk (list hello ready
                         (string-append "{\"jsonrpc\":\"2.0\",\"" key "\":" id
                                        ",\"method\":\"ping\"}")))))
    (and (pair? out) (pair? (cdr out)) (cadr out))))

(want "MC-12 an id whose key is spelled with an escape is still that request's id"
      (let ((line (ping-keyed "i\\u0064" "5e-324")))
        (list (if (eq? 'unparseable (parse line)) 'NOT-JSON 'parses)
              (if (and (string? line) (contains? line "\"id\":5e-324,")) 'echoed
                  (list 'said line))))
      '(parses echoed))

;; NEVER: AND THE SAME VALUE WITH THE ORDINARY KEY, which reaches the printer
;; by a different road: here the echo succeeds, so this row is about the
;; value surviving at all. Both spellings must answer the same line.
(want "MC-12 and the ordinary spelling of the key answers the same line"
      (let ((a (ping-keyed "id" "5e-324"))
            (b (ping-keyed "i\\u0064" "5e-324")))
        (list (if (eq? 'unparseable (parse a)) 'NOT-JSON 'parses)
              (if (and (string? a) (string? b) (string=? a b)) 'same-answer
                  (list 'differ a b))))
      '(parses same-answer))

;; NEVER: TWIN: A KEY THAT MEANS SOMETHING ELSE IS STILL SOMETHING ELSE.
;; Decoding the key must not turn the comparison into one that matches
;; anything: `"ie"` is `ie`, not `id`, and a request with no id is a
;; notification, which is answered with nothing at all.
(want "MC-12 TWIN: a key that decodes to a different name is not the id"
      (let ((out (talk (list hello ready
                             "{\"jsonrpc\":\"2.0\",\"i\\u0065\":5,\"method\":\"ping\"}"))))
        (if (and (pair? out) (pair? (cdr out)))
            (list 'answered-anyway (cadr out))
            'treated-as-a-notification))
      'treated-as-a-notification)

;; ---- F100b M3a: P7, the shell's refusals (item 8, point 7) -------------------
;;
;; Brief v7's P7 rows, WITHOUT the transport-refusal row, which is dropped as
;; unreachable (ruling, brief :876-881): H6 measures `origin` from a
;; constructed refusal. Each store lives under this run's directory, so the
;; teardown below takes down any daemon a row started.
(printf "== F100b M3a: P7 ==\n")
(putenv "THEOURGIA_RUN" (string-append sock-here "/run"))
(system (string-append "mkdir -p " sock-here "/run"))
(define p7-n 0)
(define (p7-store!)
  (set! p7-n (+ p7-n 1))
  (let* ((d (string-append here "/p7-" (number->string p7-n)))
         (s (string-append d "/parent/store")))
    (system (string-append "mkdir -p " s))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "scheme --script ../core.sc init --store " s " > /dev/null 2>&1"))
    s))
(define (p7-parent s) (substring s 0 (- (string-length s) (string-length "/store"))))
(define (p7-key-dir s) (let ((p (socket-path s))) (substring p 0 (- (string-length p) (string-length "/socket")))))
;; The P6 setup: meta.sexp at 000; with pre-create? the key directory and
;; its lock are made first, so neither the client nor main records one.
(define (p7-unreadable! pre-create?)
  (let ((s (p7-store!)))
    (when pre-create?
      (system (string-append "mkdir -p " (p7-key-dir s) " && touch " (p7-key-dir s) "/.socket.lock")))
    (system (string-append "chmod 000 " s "/meta.sexp"))
    s))
(define (p7-restore! s) (system (string-append "chmod 644 " s "/meta.sexp")))
(define (call-frame id) (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id)
                                       ",\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_read\",\"arguments\":{\"argv\":[\"root\"]}}}"))
(define (list-frame id) (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id) ",\"method\":\"tools/list\"}"))
(define (refusal-of line) (field line "result" "_meta" "refusal"))
(define (member-of* obj key) (and (pair? obj) (let ((m (assoc key obj))) (and m (cdr m)))))
(define (hex16? t) (and (string? t) (= 16 (string-length t))
                        (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list t))))
;; After the handshake the store's parent is made 000, then one request.
(define (after-handshake-000 s frame)
  (let ((sh (start-shell s)))
    (send-frame! sh hello) (read-frame sh)
    (send-frame! sh ready)
    (system (string-append "chmod 000 " (p7-parent s)))
    (send-frame! sh frame)
    (let ((line (read-frame sh)))
      (system (string-append "chmod 755 " (p7-parent s)))
      (close-input! sh)
      (let drain () (unless (eof-object? (read-frame sh)) (drain)))
      line)))

;; local, tools/call: no daemon, the parent at 000; the server's argv cannot
;; be derived, and the table's answer is a tool RESULT (point 7, E11).
(let* ((s (p7-store!)) (line (after-handshake-000 s (call-frame 51))) (r (refusal-of line)))
  (want "P7 local tools/call: a RESULT with isError and _meta.refusal {kind unreadable, path <store>, reason, errno EACCES}, id 51"
        (list (field line "id") (field line "result" "isError")
              (member-of* r "kind") (member-of* r "path") (member-of* r "reason") (member-of* r "errno"))
        (list 51 #t "unreadable" s "Permission denied" "EACCES")))
;; local, tools/list (E11): the same setup answers the tools/list ERROR.
(let* ((s (p7-store!)) (line (after-handshake-000 s (list-frame 52))))
  (want "P7 local tools/list: ERROR -32000 \"core did not start\" with data.kind unreadable and data.path <store>, id 52"
        (list (field line "id") (field line "error" "code") (field line "error" "message")
              (field line "error" "data" "kind") (field line "error" "data" "path"))
        (list 52 -32000 "core did not start" "unreadable" s)))
;; tools/list start-failed: the P6-unreadable store.
(let* ((s (p7-unreadable! #t))
       (out (talk (list hello ready (list-frame 53)) s))
       (line (and (pair? (cdr out)) (cadr out)))
       (data (and line (field line "error" "data"))))
  (p7-restore! s)
  (want "P7 tools/list start-failed: ERROR -32000 with data {kind unreadable, path meta.sexp, reason, errno EACCES, a 16-hex attempt, exit 75}"
        (list (field line "error" "code") (field line "error" "message")
              (member-of* data "kind") (member-of* data "path") (member-of* data "reason")
              (member-of* data "errno") (hex16? (member-of* data "attempt")) (member-of* data "exit"))
        (list -32000 "core did not start" "unreadable" (string-append s "/meta.sexp") "Permission denied" "EACCES" #t 75)))
;; tools/call, the catalogue site: the same store; the catalogue's own start fails.
(let* ((s (p7-unreadable! #t))
       (out (talk (list hello ready (call-frame 54)) s))
       (line (and (pair? (cdr out)) (cadr out)))
       (r (and line (refusal-of line))))
  (p7-restore! s)
  (want "P7 tools/call at the catalogue site: a RESULT, isError, _meta.refusal.kind unreadable with a 16-hex attempt"
        (list (field line "result" "isError") (member-of* r "kind") (hex16? (member-of* r "attempt")))
        (list #t "unreadable" #t)))
;; two starts, one shell (E9): two tools/call on the P6-unreadable store.
(let* ((s (p7-unreadable! #t))
       (out (talk (list hello ready (call-frame 55) (call-frame 56)) s))
       (r1 (and (>= (length out) 3) (refusal-of (cadr out))))
       (r2 (and (>= (length out) 3) (refusal-of (caddr out)))))
  (p7-restore! s)
  (want "P7 two starts in one shell: two RESULT refusals whose attempts are 16-hex and differ"
        (list (hex16? (member-of* r1 "attempt")) (hex16? (member-of* r2 "attempt"))
              (and (string? (member-of* r1 "attempt")) (not (equal? (member-of* r1 "attempt") (member-of* r2 "attempt")))))
        '(#t #t #t)))
;; client-written rendered: the P6-incomplete store (the key directory
;; absent, the run root present), so the client makes the key directory and
;; main makes the lock.
(let* ((s (p7-unreadable! #f)) (kd (p7-key-dir s))
       (out (talk (list hello ready (call-frame 57)) s))
       (r (and (pair? (cdr out)) (refusal-of (cadr out)))))
  (p7-restore! s)
  (want "P7 client-written rendered: _meta.refusal client-written [[mkdir <key>]] and written [[create <key>/.socket.lock]]"
        (list (member-of* r "kind") (member-of* r "client-written") (member-of* r "written"))
        (list "incomplete" (vector (vector "mkdir" kd)) (vector (vector "create" (string-append kd "/.socket.lock"))))))

;; ---- a stub daemon that answers `describe` once --------------------------
;;
;; It listens on the store's socket path, reads one request frame (up to its
;; newline), and answers with an envelope whose stdout is the given text. With
;; unlink-first (the J2 tool-site row) it closes its listener and unlinks the
;; socket BEFORE writing the answer, so no second connection can be accepted
;; and lost. It appends one line to its log per answer and exits.
(define stub-py (string-append here "/stub.py"))
(call-with-output-file stub-py
  (lambda (o)
    (put-string o (string-append
      "import os, socket, sys\n"
      "path, text_file, log, unlink_first = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4] == '1'\n"
      "text = open(text_file).read()\n"
      "esc = text.replace('\\\\', '\\\\\\\\').replace('\"', '\\\\\"')\n"
      "env = '(answer (stdout \"' + esc + '\") (stderr \"\") (exit 0) (origin core))\\n'\n"
      "s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)\n"
      "s.bind(path); s.listen(1)\n"
      "open(log, 'a').write('listening\\n')\n"
      "c, _ = s.accept()\n"
      "buf = b''\n"
      "while not buf.endswith(b'\\n'):\n"
      "    chunk = c.recv(65536)\n"
      "    if not chunk: break\n"
      "    buf += chunk\n"
      "if unlink_first:\n"
      "    s.close(); os.unlink(path)\n"
      "c.sendall(env.encode()); c.close()\n"
      "if not unlink_first:\n"
      "    s.close(); os.unlink(path)\n"
      "open(log, 'a').write('answered\\n')\n")))
  'truncate)
(define (start-stub! s name text unlink-first?)
  (let ((tf (string-append here "/" name ".stdout")) (log (string-append here "/" name ".log")))
    (call-with-output-file tf (lambda (o) (put-string o text)) 'truncate)
    ;; THE KEY DIRECTORY FIRST: a store without the P6 setup has none, and a
    ;; bind there fails, so the shell would start a real daemon and the stub
    ;; would never be asked (the base reading showed it: 0 answers).
    (system (string-append "mkdir -p " (p7-key-dir s)))
    (system (string-append "python3 " stub-py " " (socket-path s) " " tf " " log " " (if unlink-first? "1" "0") " &"))
    (let up ((k 0))
      (cond ((file-exists? (socket-path s)) #t)
            ((> k 100) #f)
            (else (system "sleep 0.05") (up (+ k 1)))))
    log))
(define (log-lines log) (let ((t (file-text log))) (length (filter (lambda (l) (string=? l "answered")) (let split ((cs (string->list t)) (cur '()) (acc '()))
                                                                                                        (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
                                                                                                              ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                                                                              (else (split (cdr cs) (cons (car cs) cur) acc))))))))

;; tools/call, the tool site (D20, E1, J2): the stub answers the catalogue
;; once and is gone; the tool's own request finds no socket and starts one
;; on the P6-unreadable store, which fails. The stub's log shows ONE answer,
;; so the refusal came from the tool request, not the catalogue.
(let* ((s (p7-unreadable! #t))
       (log (start-stub! s "j2" "(ok (verbs (read (route daemon) (description \"r\"))) (protocol \"p\"))" #t))
       (out (talk (list hello ready (call-frame 58)) s))
       (line (and (pair? (cdr out)) (cadr out)))
       (r (and line (refusal-of line))))
  (p7-restore! s)
  (want "P7 tools/call at the tool site: the stub answered the catalogue once, and the tool's start failure is a RESULT with kind unreadable and an attempt"
        (list (log-lines log) (field line "result" "isError") (member-of* r "kind") (hex16? (member-of* r "attempt")))
        (list 1 #t "unreadable" #t)))
;; broken catalogue (PR-12 cat): exactly the unavailable object.
(let* ((s (p7-store!))
       (log (start-stub! s "cat" "(ok (verbs (read (usage (read <id>))" #f))
       (out (talk (list hello ready (list-frame 59)) s))
       (line (and (pair? (cdr out)) (cadr out))))
  (want "P7 broken catalogue: tools/list ERROR -32000 whose data is exactly {kind unavailable, datum (unavailable)}"
        (list (log-lines log) (field line "error" "code")
              (let ((d (field line "error" "data")))
                (and (pair? d) (list-sort (lambda (a b) (string<? (car a) (car b))) d))))
        (list 1 -32000 '(("datum" . "(unavailable)") ("kind" . "unavailable")))))
;; a PARSED core error (E12): describe answered with (error describe-refused
;; (path "/x")) is refused with its own kind, not flattened to unavailable.
(let* ((s (p7-store!))
       (log (start-stub! s "perr" "(error describe-refused (path \"/x\"))" #f))
       (out (talk (list hello ready (list-frame 60)) s))
       (line (and (pair? (cdr out)) (cadr out))))
  (want "P7 a parsed core error from describe: tools/list ERROR -32000 with data.kind describe-refused and data.path /x"
        (list (log-lines log) (field line "error" "code") (field line "error" "data" "kind") (field line "error" "data" "path"))
        (list 1 -32000 "describe-refused" "/x")))

;; ---- F100b M3b: F for the shell (F110, point 7) -----------------------------
;;
;; On success inputs the shell's answers carry no record: no written, no
;; client-written, and no _meta -- in the JSON text (the keys, quoted), and in
;; the parsed result object, which has no _meta member and no isError set
;; true (ruling Q-M3b-2), so a shell that moved a refusal out of the text into
;; a member cannot pass.
;; THE WHOLE PARSED RESULT IS WALKED (M3b review r2, F1): member names are
;; compared AFTER the parser has decoded them, so an escaped spelling such as
;; "\u0077ritten" is the name it spells, at any depth. The substring
;; checks stay as a second, textual reading.
(define (record-member-anywhere? v)
  (cond
    ((and (pair? v) (list? v) (for-all (lambda (m) (and (pair? m) (string? (car m)))) v))
     (exists (lambda (m) (or (member (car m) '("written" "client-written" "_meta"))
                             (record-member-anywhere? (cdr m))))
             v))
    ((vector? v) (exists record-member-anywhere? (vector->list v)))
    (else #f)))
(define (no-record-member? line)
  (let ((r (field line "result")))
    (and (string? line)
         (not (contains? line "\"written\""))
         (not (contains? line "\"client-written\""))
         (not (contains? line "\"_meta\""))
         (pair? r)
         (not (record-member-anywhere? r))
         (not (eq? #t (and (assoc "isError" r) (cdr (assoc "isError" r))))))))
;; AND IN THE ANSWER ITSELF (M3b review r1, F1): a tool result's text is the
;; core's Scheme answer, and a record there is a clause of that datum, not a
;; JSON key.
;; EVERY content item's text is decoded (M3b review r2, F2), not only the
;; first, and none may carry a written or client-written clause.
(define (text-carries-record? line)
  (let ((content (field line "result" "content")))
    (and (vector? content)
         (exists (lambda (item)
                   (let* ((t (and (pair? item) (json-ref* item "text")))
                          (d (and (string? t) (guard (e (#t #f)) (read (open-string-input-port t))))))
                     (and (list? d)
                          (exists (lambda (c) (and (pair? c) (memq (car c) '(written client-written)) #t)) (cdr d)))))
                 (vector->list content)))))
(let* ((out (talk (list hello ready (list-frame 61) (call-tool "theourgia_outline" '()))))
       (listing (and (>= (length out) 3) (cadr out)))
       (called (and (>= (length out) 3) (caddr out))))
  (want "F the MCP shell on success: tools/list and a tools/call answer carry no written, client-written or _meta, no isError true, and the tool's answer datum no record clause"
        (list (and listing (vector? (field listing "result" "tools")) (no-record-member? listing))
              (and called (starts-with-text? (text-of called) "(ok (text") (no-record-member? called)
                   (not (text-carries-record? called))))
        '(#t #t)))

;; ---- MW: whose drafts a session writes -----------------------------------------
;;
;; NEVER: THE SHELL NEVER SENDS writer #f. Without THEOURGIA_WRITER a session
;; could not write drafts unless each call carried `--writer`, and the one
;; remedy on offer gave every session of a host one writer. The writer is
;; decided once at start -- the variable, else `--writer`, else one derived
;; from the actor, the start instant and the pid -- said in `instructions`,
;; carried on every frame, and checked at start.

;; The three sentences the shell has always answered with, byte for byte.
(define base-instructions
  (string-append
    "Tools return the unmodified core command answer as S-expression text. "
    "Core refusals are successful transport results. "
    "Eval is available only in the local CLI."))
(define writer-sentence-head " This session's writer is ")
(define writer-sentence-tail
  "; drafts left by an earlier session are read with `drafts --writer <that session's writer>`.")

;; The writer the initialize answer names, or #f when the instructions are
;; not the three sentences followed by exactly the writer sentence.
(define (announced line)
  (let ((t (field line "result" "instructions"))
        (head (string-append base-instructions writer-sentence-head)))
    (and (string? t) (starts-with-text? t head)
         (let* ((rest (substring t (string-length head) (string-length t)))
                (n (string-length rest)) (m (string-length writer-sentence-tail)))
           (and (> n m)
                (string=? (substring rest (- n m) n) writer-sentence-tail)
                (substring rest 0 (- n m)))))))

;; `<actor>-<base 36>-<pid>` split at its last two "-", or #f.
(define (name-parts name)
  (let* ((cs (string->list name))
         (dashes (let loop ((i 0) (cs cs) (out '()))
                   (cond ((null? cs) out)
                         ((char=? (car cs) #\-) (loop (+ i 1) (cdr cs) (cons i out)))
                         (else (loop (+ i 1) (cdr cs) out))))))
    (and (>= (length dashes) 2)
         (let ((last (car dashes)) (before (cadr dashes)))
           (list (substring name 0 before)
                 (substring name (+ before 1) last)
                 (substring name (+ last 1) (string-length name)))))))

(define (epoch-ms)
  (let ((t (current-time 'time-utc)))
    (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))

;; working.sc's rule for a writer id, COPIED: a row that asked the product's
;; own predicate would agree with any change to it.
(define (writer-id-copy? x)
  (and (string? x) (> (string-length x) 0) (<= (string-length x) 128)
       (for-all (lambda (c) (or (char<=? #\a c #\z) (char<=? #\0 c #\9)
                                (memv c '(#\- #\_ #\.)))) (string->list x))
       (not (member x '("." "..")))))

(define no-writer "env -u THEOURGIA_WRITER ")

;; A store of its own, with one block to write drafts on.
(define wstore (string-append here "/wstore"))
(system (string-append "mkdir -p " wstore))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
          "scheme --script ../core.sc init --store " wstore " > /dev/null 2>&1"))
(cli-answer wstore '("insert" "--title" "MW-BLOCK" "--text" "mwblock line"))
(cli-answer wstore '("insert" "--title" "MW-SECOND" "--text" "mwsecond line"))
(define (block-by-grep word)
  (let ((answer (guard (e (#t #f)) (read (open-input-string (cli-answer wstore (list "grep" word)))))))
    (let find ((x answer))
      (cond ((and (pair? x) (eq? (car x) 'match) (pair? (cdr x)) (string? (cadr x))) (cadr x))
            ((pair? x) (or (find (car x)) (find (cdr x))))
            (else #f)))))
(define wblock (block-by-grep "mwblock"))
(define wblock2 (block-by-grep "mwsecond"))

;; A PEER THAT ANSWERS EVERY FRAME AND RECORDS EVERY FRAME, describe
;; included, and counts the connections it accepted. The envelope rows'
;; capture stub answers describe without recording it and answers nothing
;; else, so a session could not go on past its first call.
(define wsock (string-append sock-here "/mw.sock"))
(define wpeer (string-append here "/mw-peer.sc"))
(define wframes (string-append here "/mw-peer.frames"))
(define wconns (string-append here "/mw-peer.conns"))
(define (reply-text stdout)
  (string-append (format "~s" (list 'answer (list 'stdout stdout) (list 'stderr "") (list 'exit 0))) "\n"))
(define wcatalogue
  (reply-text
    (string-append
      "(ok (verbs "
      "(read (usage (read <id>)) (description \"Read a block.\") (protocol #f) (route daemon)) "
      "(write (usage (write <id> <text>)) (description \"Write a draft.\") (protocol #f) (route daemon)) "
      "(drafts (usage (drafts)) (description \"List drafts.\") (protocol #f) (route daemon))) "
      "(protocol \"P\"))\n")))
(define (start-wpeer!)
  (system (string-append "pkill -f " wpeer " 2>/dev/null; rm -f " wsock " " wframes " " wconns))
  (call-with-output-file wpeer
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia sched) (theourgia net))"
              (string-append "(define catalogue-reply (string->utf8 " (format "~s" wcatalogue) "))")
              (string-append "(define ok-reply (string->utf8 " (format "~s" (reply-text "(ok)\n")) "))")
              "(define (asks-to-describe? bv)"
              "  (let* ((t (utf8->string bv)) (n (string-length t)))"
              "    (let loop ((i 0))"
              "      (cond ((> (+ i 8) n) #f)"
              "            ((string=? (substring t i (+ i 8)) \"describe\") #t)"
              "            (else (loop (+ i 1)))))))"
              "(start-scheduler"
              "  (lambda ()"
              (string-append "    (listen! \"" wsock "\" 16)")
              "    (let serve ()"
              "      (receive (after 20000 (exit 0))"
              (string-append
                "               (`(accepted ,ref)"
                " (call-with-output-file \"" wconns "\" (lambda (p) (put-string p \"c\\n\")) 'append)"
                " (conn-read-start! ref) (serve))")
              (string-append
                "               (`(data ,r ,bv)"
                " (call-with-output-file \"" wframes "\" (lambda (p) (put-string p (utf8->string bv))) 'append)"
                " (conn-write! r (if (asks-to-describe? bv) catalogue-reply ok-reply) 'last)"
                " (serve))")
              "               (`(written ,r ,t ,st) (conn-close! r) (serve))"
              "               (`(eof ,r) (serve))"
              "               (`#(DOWN ,w ,y) (serve))))))")))
    'truncate)
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script " wpeer " > /dev/null 2>&1 &"))
  (let up ((k 0))
    (cond ((file-exists? wsock) 'up)
          ((> k 200) 'never)
          (else (system "sleep 0.05") (up (+ k 1))))))
(define (stop-wpeer!) (system (string-append "pkill -f " wpeer " 2>/dev/null")))
;; Every frame the peer recorded, as data: (request <n> <store> <actor>
;; <writer> <mode> <cwd> <stdin> <verb> <arg> ...).
(define (recorded-frames)
  (let ((p (open-input-string (file-text wframes))))
    (let loop ((out '()))
      (let ((d (guard (e (#t (eof-object))) (read p))))
        (if (eof-object? d) (reverse out) (loop (cons d out)))))))
(define (frame-writer f) (and (list? f) (> (length f) 8) (list-ref f 4)))
(define (frame-verb f) (and (list? f) (> (length f) 8) (list-ref f 8)))
(define (line-count path)
  (let ((t (file-text path)))
    (let loop ((cs (string->list t)) (n 0))
      (cond ((null? cs) n) ((char=? (car cs) #\newline) (loop (cdr cs) (+ n 1))) (else (loop (cdr cs) n))))))

;; W1 (a): the derived writer is named and carried on every frame.
(start-wpeer!)
(let* ((t0 (epoch-ms))
       (s (start-shell* wstore wsock no-writer " --actor alpha")))
  (send-frame! s hello)
  (let ((init (read-frame s)))
    (send-frame! s ready)
    (send-frame! s (call-tool "theourgia_write" '("x.1" "t")))
    (read-frame s)
    (system "sleep 0.01")
    (send-frame! s (call-tool "theourgia_drafts" '()))
    (read-frame s)
    (close-input! s)
    (let loop () (unless (eof-object? (read-frame s)) (loop)))
    (let* ((t1 (epoch-ms))
           (name (announced init))
           (parts (and name (name-parts name)))
           (frames (recorded-frames)))
      (stop-wpeer!)
      (want "W1 (a) with no THEOURGIA_WRITER and no --writer the shell names alpha-<start in base 36>-<its pid> after the three sentences, and describe, write and drafts all carry it"
            (list (and parts (car parts))
                  (and parts (equal? (caddr parts) (number->string (shell-pid s))))
                  (and parts (let ((ms (string->number (cadr parts) 36))) (and ms (<= t0 ms t1))))
                  (map frame-verb frames)
                  (and name (for-all (lambda (f) (equal? (frame-writer f) name)) frames)))
            (list "alpha" #t #t '(describe write describe drafts) #t)))))

;; W1 (b): the draft round trip against the real daemon.
(let* ((out (talk* (list hello ready
                         (call-tool "theourgia_write" (list wblock "mw1b bytes"))
                         (call-tool "theourgia_drafts" '()))
                   wstore #f no-writer " --actor alpha")))
  (want "W1 (b) the derived session writes a draft and its drafts list it"
        (list (and (>= (length out) 3) (starts-with-text? (text-of (cadr out)) "(ok"))
              (and (>= (length out) 3) (string? wblock) (contains? (text-of (caddr out)) wblock)))
        '(#t #t)))

;; The `(version ...)` clause a write answered with, or #f.
(define (answered-version text)
  (let ((d (and (string? text) (guard (e (#t #f)) (read (open-input-string text))))))
    (let find ((x d))
      (cond ((and (pair? x) (eq? (car x) 'version) (pair? (cdr x))) (cadr x))
            ((pair? x) (or (find (car x)) (find (cdr x))))
            (else #f)))))

;; W2: two sessions, one actor, alive at once.
(let* ((a (start-shell* wstore #f no-writer " --actor alpha")))
  (send-frame! a hello)
  (let* ((a-init (read-frame a))
         (crossed (let ((t (epoch-ms))) (let wait () (if (= (epoch-ms) t) (wait) 'crossed))))
         (b (start-shell* wstore #f no-writer " --actor alpha")))
    (send-frame! b hello)
    (let* ((b-init (read-frame b))
           (na (announced a-init)) (nb (announced b-init))
           (pa (and na (name-parts na))) (pb (and nb (name-parts nb))))
      (send-frame! a ready) (send-frame! b ready)
      (send-frame! a (call-tool "theourgia_write" (list wblock "mw2 bytes of A")))
      (let ((wa (read-frame a)))
        (send-frame! b (call-tool "theourgia_write" (list wblock "mw2 bytes of B")))
        (let ((wb (read-frame b)))
          (send-frame! a (call-tool "theourgia_read" (list wblock "--working")))
          (let ((ra (read-frame a)))
            (send-frame! b (call-tool "theourgia_read" (list wblock "--working")))
            (let ((rb (read-frame b)))
              (close-input! a) (close-input! b)
              (let loop () (unless (eof-object? (read-frame a)) (loop)))
              (let loop () (unless (eof-object? (read-frame b)) (loop)))
              (want "W2 two sessions of one actor get writers that differ in instant and in pid, and each writes and reads back its own draft of one block"
                    (list (and pa pb (string=? (car pa) "alpha") (string=? (car pb) "alpha"))
                          (and pa pb (not (string=? (cadr pa) (cadr pb))))
                          (and pa pb (not (string=? (caddr pa) (caddr pb))))
                          (starts-with-text? (text-of wa) "(ok")
                          (starts-with-text? (text-of wb) "(ok")
                          (let ((va (answered-version (text-of wa))) (vb (answered-version (text-of wb))))
                            (and va vb (not (equal? va vb))))
                          (contains? (text-of ra) "mw2 bytes of A")
                          (contains? (text-of rb) "mw2 bytes of B"))
                    '(#t #t #t #t #t #t #t #t)))))))))

;; W3: precedence, read from the frames.
(define (session-with launch extra)
  (start-wpeer!)
  (let* ((out (talk* (list hello ready (call-tool "theourgia_write" '("x.1" "t"))) wstore wsock launch extra))
         (frames (recorded-frames)))
    (stop-wpeer!)
    (list (and (pair? out) (announced (car out)))
          (map frame-writer frames))))
(want "W3 THEOURGIA_WRITER wins over --writer: the sentence and every frame name w1"
      (session-with "env THEOURGIA_WRITER=w1 " " --writer w2 --actor alpha")
      '("w1" ("w1" "w1")))
(want "W3 --writer alone is the session's writer: w2"
      (session-with no-writer " --writer w2 --actor alpha")
      '("w2" ("w2" "w2")))
(want "W3 with neither, the derived writer, and the frames carry it"
      (let* ((r (session-with no-writer " --actor alpha")) (name (car r)) (parts (and name (name-parts name))))
        (list (and parts (car parts)) (and name (equal? (cadr r) (list name name)))))
      '("alpha" #t))

;; W4: the derived writer is a writer id, whatever the actor.
(for-each
  (lambda (actor prefix)
    (let* ((out (talk* (list hello ready (call-tool "theourgia_write" (list wblock "mw4")))
                       wstore #f no-writer (string-append " --actor " (shell-quote actor))))
           (name (and (pair? out) (announced (car out)))))
      (want (string-append "W4 the actor " (format "~s" (if (> (string-length actor) 20) (substring actor 0 20) actor))
                           " gives a writer id of at most 128 characters starting " prefix ", and a write under it succeeds")
            (list (writer-id-copy? name)
                  (and name (<= (string-length name) 128))
                  (and name (starts-with-text? name prefix))
                  (and (>= (length out) 2) (starts-with-text? (text-of (cadr out)) "(ok")))
            '(#t #t #t #t))))
  (list "Louis de Guenchy!" "" "!!!" ".." (make-string 200 #\a))
  (list "louis-de-guenchy-" "agent-" "agent-" "agent-" (string-append (make-string 40 #\a) "-")))

;; W5: an ended session's drafts, from another session.
(let* ((a-out (talk* (list hello ready (call-tool "theourgia_write" (list wblock "mw5 from A")))
                     wstore #f no-writer " --actor alpha"))
       (a-name (and (pair? a-out) (announced (car a-out))))
       (b-out (talk* (list hello ready
                           (call-tool "theourgia_drafts" (list "--writer" (or a-name "none")))
                           (call-tool "theourgia_write" (list (or wblock2 "none") "mw5 from B"))
                           (call-tool "theourgia_drafts" '()))
                     wstore #f no-writer " --actor alpha"))
       (b-name (and (pair? b-out) (announced (car b-out))))
       (c-out (talk* (list hello ready (call-tool "theourgia_drafts" '()))
                     wstore #f (string-append "env THEOURGIA_WRITER=" (or a-name "none") " ") " --actor gamma")))
  (want "W5 a later session reads an ended one's drafts with drafts --writer, keeps its own writer after that call (its own draft listed, not the other's), and a session started as that writer lists them"
        (list (and a-name b-name (not (string=? a-name b-name)))
              (and (>= (length b-out) 4) (string? wblock) (contains? (text-of (cadr b-out)) wblock))
              (and (>= (length b-out) 4) (string? wblock) (string? wblock2)
                   (starts-with-text? (text-of (caddr b-out)) "(ok")
                   (starts-with-text? (text-of (cadddr b-out)) "(ok")
                   (contains? (text-of (cadddr b-out)) wblock2)
                   (not (contains? (text-of (cadddr b-out)) wblock)))
              (and (pair? c-out) (equal? (announced (car c-out)) a-name))
              (and (>= (length c-out) 2) (string? wblock) (contains? (text-of (cadr c-out)) wblock)))
        '(#t #t #t #t #t)))

;; W6: a writer that cannot be one is refused at start, before any frame.
;; -> (exit-status stdout stderr connections)
(define (run-shell-once launch extra)
  (let ((in (string-append here "/mw6.in")) (out (string-append here "/mw6.out"))
        (err (string-append here "/mw6.err")) (rc (string-append here "/mw6.rc")))
    (start-wpeer!)
    (call-with-output-file in
      (lambda (p) (for-each (lambda (f) (put-string p f) (newline p))
                            (list hello ready (call-tool "theourgia_read" '("x.1")))))
      'truncate)
    (system (string-append "rm -f " out " " err " " rc "; "
                           "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "THEOURGIA_RUN=" sock-here "/run " launch
                           "scheme --script " shell " --store " wstore " --socket " wsock extra
                           " < " in " > " out " 2> " err "; echo $? > " rc))
    (stop-wpeer!)
    (list (let ((t (file-text rc))) (if (> (string-length t) 0) (substring t 0 (- (string-length t) 1)) t))
          (file-text out) (file-text err) (line-count wconns))))
(define (refused-at-start r)
  (list (car r) (cadr r) (contains? (caddr r) "usage: theourgia-mcp") (cadddr r)))
(want "W6 --writer \"Bad Id\" is refused at start: exit 2, the usage line, no frame, no connection"
      (refused-at-start (run-shell-once no-writer " --writer 'Bad Id'"))
      '("2" "" #t 0))
(want "W6 --writer with no value is refused at start the same way"
      (refused-at-start (run-shell-once no-writer " --writer"))
      '("2" "" #t 0))
(want "W6 THEOURGIA_WRITER=\"Bad Id\" with no option is refused at start the same way"
      (refused-at-start (run-shell-once "env 'THEOURGIA_WRITER=Bad Id' " ""))
      '("2" "" #t 0))
(want "W6 a bad --writer is refused even when THEOURGIA_WRITER would have won"
      (refused-at-start (run-shell-once "env THEOURGIA_WRITER=w1 " " --writer 'Bad Id'"))
      '("2" "" #t 0))
(want "W6 an empty THEOURGIA_WRITER is unset, not bad: the shell starts, derives, and reaches its socket"
      (let* ((r (run-shell-once "env THEOURGIA_WRITER= " " --actor alpha"))
             (first-line (let ((t (cadr r))) (let loop ((i 0)) (cond ((>= i (string-length t)) t) ((char=? (string-ref t i) #\newline) (substring t 0 i)) (else (loop (+ i 1)))))))
             (name (announced first-line)))
        (list (car r) (and name (name-parts name) (car (name-parts name))) (> (cadddr r) 0)))
      '("0" "alpha" #t))
(want "W6 a valid --writer starts normally, is named in the instructions (which the base does not do), and reaches its socket"
      (let* ((r (run-shell-once no-writer " --writer w6"))
             (first-line (let ((t (cadr r))) (let loop ((i 0)) (cond ((>= i (string-length t)) t) ((char=? (string-ref t i) #\newline) (substring t 0 i)) (else (loop (+ i 1))))))))
        (list (car r) (announced first-line) (> (cadddr r) 0)))
      '("0" "w6" #t))

;; W6 PINS: every other parse error answers as it always did -- the -32600
;; frame on stdout, exit 2 -- and an option the daemon's table declares is
;; still parsed as one: the shell's table declares everything serve's did.
(define (answered-frame r)
  (list (car r) (contains? (cadr r) "\"code\":-32600") (contains? (caddr r) "usage: theourgia-mcp") (cadddr r)))
(want "W6 PIN --actor without a value answers the -32600 frame, not the usage line, and exits 2"
      (answered-frame (run-shell-once no-writer " --actor"))
      '("2" #t #f 0))
(want "W6 PIN --detach given twice is still a duplicate option: the -32600 frame, exit 2"
      (answered-frame (run-shell-once no-writer " --detach --detach"))
      '("2" #t #f 0))

;; W6: the writer is checked before the store is looked at. Resolving the
;; default socket stats the store, and a store under a directory that cannot
;; be searched makes that raise; the bad writer is still answered with the
;; usage line.
(want "W6 a bad --writer with no --socket and a store under an unsearchable directory is still refused at start with the usage line, exit 2"
      (let* ((denied (string-append here "/mw6-denied")) (store (string-append denied "/child"))
             (err (string-append here "/mw6d.err")) (rc (string-append here "/mw6d.rc")))
        (system (string-append "mkdir -p " store "; chmod 000 " denied))
        (system (string-append "rm -f " err " " rc "; "
                               "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                               "THEOURGIA_RUN=" sock-here "/run " no-writer
                               "scheme --script " shell " --store " store " --writer 'Bad Id'"
                               " < /dev/null > /dev/null 2> " err "; echo $? > " rc))
        (system (string-append "chmod 755 " denied))
        (list (let ((t (file-text rc))) (if (> (string-length t) 0) (substring t 0 (- (string-length t) 1)) t))
              (contains? (file-text err) "usage: theourgia-mcp")))
      '("2" #t))

;; The shell's parser, asked through the library's environment: a name the
;; base does not export would stop this whole file from loading there, and a
;; base run has to read row by row.
(define (parse-shell-arguments args)
  ((eval 'parse-shell-arguments (environment '(theourgia arguments))) args))

;; W7: the shell's own parser, beside the verb tables, not in them. The
;; command line and the RPC route parse a request's options before they ask
;; whether its verb exists, so a key in the verb tables would answer a request
;; for the unknown verb `mcp` from the shell's options.
(want "W7 PIN a request for the unknown verb mcp, with --log or with --writer, is answered unknown-verb as on the base"
      (list (contains? (cli-answer wstore '("mcp" "--log")) "(error unknown-verb")
            (contains? (cli-answer wstore '("mcp" "--writer")) "(error unknown-verb"))
      '(#t #t))
(want "W7 --writer is an option of the shell's parser, a positional of serve's table and of the verb mcp, and the daemon's usage is unchanged"
      (list (parse-shell-arguments '("--writer" "w"))
            (parse-arguments 'serve '("--writer" "w"))
            (parse-arguments 'mcp '("--writer" "w"))
            (call-with-input-file "../theourgiad.sc"
              (lambda (p)
                (let loop ()
                  (let ((d (read p)))
                    (cond ((eof-object? d) 'no-serve-usage)
                          ((and (pair? d) (eq? (car d) 'define) (pair? (cdr d)) (eq? (cadr d) 'serve-usage))
                           (cadr (caddr d)))
                          (else (loop))))))))
      (list '((option "--writer" "w"))
            '((pos "--writer") (pos "w"))
            '((pos "--writer") (pos "w"))
            '(serve [<store>] ["--socket" <path>]
                    ["--detach" "--log" <path> (started-by-a-client-not-by-hand)]
                    ["--attempt" <token> (started-by-a-client-not-by-hand)])))

;; ---- teardown ---------------------------------------------------------------
;;
;; NEVER: EVERY DAEMON THIS FILE CAUSED TO EXIST IS TAKEN DOWN. The shell
;; starts one when it cannot reach one, so a run of this file leaves
;; daemons behind that no row mentions -- measured, fifteen of them after
;; five runs, each holding a socket and a log.
;;
;; NOTE: THE PATTERN IS THIS RUN'S OWN DIRECTORY, which carries this
;; process's pid. A pattern like `serve` or `theourgiad.sc` would also match the
;; daemons of a suite running beside this one, and of another session
;; entirely.
(system (string-append "pkill -f 'serve " here "' 2>/dev/null"))
(system "sleep 1")
(let ((left (string-append here "/left.txt")))
  (system (string-append "pgrep -f 'serve " here "' | wc -l | tr -d ' ' > " left))
  (let ((n (let ((t (file-text left)))
             (if (> (string-length t) 0) (substring t 0 (- (string-length t) 1)) "?"))))
    (want "MC-teardown no daemon this run started is still alive" n "0")))

(printf "rows: ~a\n~a failures\nmcp-shell complete\n" rows bad)
(exit (if (zero? bad) 0 1))
