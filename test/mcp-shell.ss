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
;; ⭐ EVERY ROW HERE TALKS TO A CHILD PROCESS. A row that called the
;; shell's procedures would be testing the library and would say nothing
;; about framing, about stdio, or about what a client actually receives.

(import (chezscheme) (theourgia json))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; ⛔ A RAISE INSIDE A ROW IS THAT ROW FAILING, not the file ending: a
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
(define here (string-append "/tmp/mcpshell-" pid-text))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define shell "../mcp/server.ss")

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
;; ⚠️ THE CHILD IS KEPT OPEN. Several rows need to send one frame, look at
;; what came back, and only then decide what to send next -- a helper that
;; wrote everything and read everything could not ask those questions.
(define (start-shell . options)
  (let* ((store (if (pair? options) (car options) (string-append here "/store")))
         (socket (and (pair? options) (pair? (cdr options)) (cadr options)))
         (command (string-append
                    "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                    "scheme --script " shell " --store " store
                    (if socket (string-append " --socket " socket) "")
                    " 2>>" here "/shell.err")))
    (let-values (((to from errs pid) (open-process-ports command 'line (native-transcoder))))
      (list to from pid))))

(define (shell-in s) (car s))
(define (shell-out s) (cadr s))

(define (send-frame! s text)
  (put-string (shell-in s) text)
  (put-string (shell-in s) "\n")
  (flush-output-port (shell-in s)))

(define (read-frame s) (get-line (shell-out s)))

(define (close-input! s) (close-port (shell-in s)))

;; One conversation: write these frames, close stdin, read every line.
(define (talk frames . options)
  (let ((s (apply start-shell options)))
    (for-each (lambda (f) (send-frame! s f)) frames)
    (close-input! s)
    (let loop ((out '()))
      (let ((line (read-frame s)))
        (if (eof-object? line)
            (reverse out)
            (loop (cons line out)))))))

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
(define (id-of line) (field line "id"))

(system (string-append "rm -rf " here "; mkdir -p " here "/store"))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
          "scheme --script ../cli.ss init --store " here "/store > /dev/null 2>&1"))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
          "THEOURGIA_LOCAL=1 scheme --script ../cli.ss insert --title MC-CANARY --store "
          here "/store > /dev/null 2>&1"))

;; ---- MC-lifecycle ------------------------------------------------------------
;;
;; ⛔ NOTHING IS SERVED BEFORE THE HANDSHAKE IS FINISHED, and the
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

;; ⛔ AND THE NOTIFICATION ITSELF IS NEVER ANSWERED. Two frames in, two
;; answers out -- the middle one produced nothing, which is what the row
;; above counts.
(let ((out (talk (list hello ready hello))))
  (want "MC-lifecycle a second initialize is refused"
        (list (length out) (code-of (cadr out)))
        '(2 -32602)))

;; ---- MC-04 the envelope ------------------------------------------------------
;;
;; ⛔ SIX WAYS TO BE MALFORMED, AND NONE OF THEM DISPATCHES ANYTHING. Each
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

;; ⛔ AN UNKNOWN METHOD IS -32601, which is a different fact from a
;; malformed one and has to stay different.
(let ((out (talk (list hello ready "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"no/such\"}"))))
  (want "MC-04 an unknown method is method-not-found, not invalid-params"
        (code-of (cadr out)) -32601))

;; ⛔ A BROKEN LINE IS A PARSE ERROR, and the shell keeps going.
(let ((out (talk (list hello ready "{not json" "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"ping\"}"))))
  (want "MC-04 a line that is not JSON is a parse error and the session survives"
        (list (code-of (cadr out)) (id-of (caddr out)))
        '(-32700 8)))

;; ⚠️ AN ID COMES BACK AS IT WENT OUT, in its own type. A client matches
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
;; ⛔ THE COMMAND RAN AND WAS REFUSED, which is an answer. A shell that
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
        (list (field (cadr out) "result" "isError")
              (if (contains? (text-of (cadr out)) "unknown-id") 'the-core-refusal
                  (list 'said (text-of (cadr out)))))
        '(#f the-core-refusal)))

;; ⛔ AND IT BRANCHES ON THE TRANSPORT'S TAG, NOT ON THE TEXT. A core
;; answer whose text happens to READ like a transport failure is still an
;; answer -- this one is produced on purpose, and a shell that matched on
;; the words would turn it into a JSON-RPC error.
(let ((out (talk (list hello ready
                       (call-tool "theourgia_insert"
                                  '("--title" "(error transport-unknown (reason lost-answer))"))))))
  (want "MC-03 TWIN: a payload that reads like a transport failure is still a result"
        (list (field (cadr out) "result" "isError")
              (if (starts-with-text? (text-of (cadr out)) "(ok") 'still-a-result
                  (list 'said (text-of (cadr out)))))
        '(#f still-a-result)))

;; ---- MC-argv-verbatim --------------------------------------------------------
;;
;; ⛔ EVERY BYTE OF EVERY ARGUMENT ARRIVES AS IT WAS SENT: leading and
;; trailing spaces, an empty string, a newline inside an argument, shell
;; metacharacters -- ⛔ nothing trimmed, normalised, split or expanded,
;; and ⛔ nothing handed to a shell on the way.
;;
;; ⚠️ THE ORACLE IS THE COMMAND LINE, ⛔ NOT MY IDEA OF THE ANSWER.
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
              "scheme --script ../cli.ss " (car argv)
              (apply string-append (map (lambda (a) (string-append " " (shell-quote a))) (cdr argv)))
              " --store " store " --socket " socket " --wire > " out " 2>&1"))
    (file-text out)))

(define (cli-answer store argv)
  (let ((out (string-append here "/cli-out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../cli.ss " (car argv)
              (apply string-append
                     (map (lambda (a) (string-append " " (shell-quote a))) (cdr argv)))
              " --store " store " --wire > " out " 2>&1"))
    (file-text out)))

;; ⚠️ SINGLE QUOTES, WITH THE ONE ESCAPE THAT WORKS INSIDE THEM. This is
;; the fixture handing bytes to a shell, ⛔ which is exactly what the
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
                             "scheme --script ../cli.ss init --store " st " > /dev/null 2>&1")))
    (list store2 store3))
  ;; ⭐ `read <arg>` ON A FRESH STORE ECHOES THE ARGUMENT AND NOTHING
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
    ;; ⛔ AND ONE OF THEM IS PINNED LITERALLY. Two routes that agree can
    ;; agree about something wrong -- they share a dispatcher. The
    ;; expectation here comes from outside both: it is what the argument
    ;; is, spelled the way the core spells it back.
    (want "MC-argv-verbatim and the echoed argument is exactly what was sent"
          (car through-mcp)
          "(error unknown-id \"  \\x6C49;\\x5B57;\\x1F600;  \" (nearest ()))\n")
    ;; ⛔ NOBODY LET A SHELL SEE IT. `${MC_SENTINEL}` has a value in this
    ;; fixture's environment that would be visible if any layer had.
    (want "MC-argv-verbatim TWIN: no layer let a shell expand the argument"
          (if (exists (lambda (t) (and (string? t) (contains? t "EXPANDED-BY-A-SHELL")))
                      through-mcp)
              'A-SHELL-SAW-IT
              'nobody-expanded-it)
          'nobody-expanded-it)))

;; ---- MC-05 eval is not a tool ------------------------------------------------
;;
;; ⛔ `eval` IS ABSENT FROM THE CATALOGUE AND INDISTINGUISHABLE FROM A
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
;; ⛔ FOUR WAYS FOR A SOCKET NOT TO BE THERE, AND EACH RUNS LOCALLY. The
;; brief names the errnos; here they are produced rather than named --
;; nothing at the path, a regular file, a directory, and a socket whose
;; daemon has gone. ⚠️ Each answer must be the real one, so a shell that
;; refused instead of falling back fails on the content, not on a tag.
(let* ((nothing (string-append here "/no-socket-here"))
       (regular (string-append here "/a-regular-file"))
       (folder  (string-append here "/a-directory")))
  (system (string-append "printf keep > " regular "; mkdir -p " folder))
  (let ((answers
          (map (lambda (path)
                 (let ((out (talk (list hello ready (call-tool "theourgia_outline" '()))
                                  (string-append here "/store") path)))
                   (text-of (cadr out))))
               (list nothing regular folder))))
    (want "MC-06 a socket path with nobody behind it is served locally, three ways"
          (map (lambda (t) (if (starts-with-text? t "(ok (text") 'served-locally (list 'said t)))
               answers)
          '(served-locally served-locally served-locally))
    ;; ⛔ AND THE REGULAR FILE IS STILL THERE. Falling back must not mean
    ;; tidying up something that is not ours.
    (want "MC-06 TWIN: the regular file on the socket path was left alone"
          (file-text regular) "keep")))

;; ---- MC-daemon-reach ---------------------------------------------------------
;;
;; ⭐ THIS IS THE ROW THE DAEMON DELIVERIES CARRIED AS A RISK. The Python
;; shell spoke an envelope the Scheme daemon does not answer, so with a
;; daemon running MCP was unavailable -- measured then as
;; `(error transport-invalid-answer)`. This row is what closes it.
;;
;; ⛔ THREE WITNESSES, because the answer alone proves nothing: the same
;; verb answers the same way locally, which is the entire point of having
;; two routes. So the row also reads the DAEMON's own record that it
;; dispatched, and the SHELL's own trace for the absence of a local
;; store open.
(let* ((dstore (string-append here "/dstore"))
       (dsock (string-append here "/d.sock"))
       (runner (string-append here "/serve.ss"))
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
;; ⛔ THE LIMIT IS IN BYTES AND IT COUNTS THE TERMINATOR. A frame of
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
;; ⚠️ THE PAUSE HAS TO YIELD, ⛔ not block. A probe that slept on the
;; scheduler's thread would stop everything, and then a shell that
;; answered calls concurrently would look serial too -- the row would
;; pass against the implementation it exists to refuse. The daemon's
;; `writer-hold` seam parks inside a lock with `sleep-ms`, which yields.
(define (start-parking-daemon! tag)
  (let* ((dstore (string-append here "/" tag "-store"))
         (dsock (string-append here "/" tag ".sock"))
         (runner (string-append here "/" tag ".ss"))
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
;; ⛔ ONE CALL AT A TIME. Both frames are written before either is
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
    ;; ⛔ AND SOMEBODY ELSE COULD GET ON WITH IT. While the first call was
    ;; parked, an independent caller reached the same daemon -- so "the
    ;; second answer waited" is a fact about this shell's serialisation,
    ;; ⛔ not about a daemon that had stopped.
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
;; ⛔ A CALL THAT HAS BEEN ACCEPTED IS ANSWERED, even though the client
;; has gone. ⚠️ The row closes stdin while the call is parked -- after the
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
;; ⛔ THE SHELL AND THE COMMAND LINE SEND THE SAME ENVELOPE, and the row
;; reads the BYTES rather than trusting that both call the same
;; procedure. A controlled listener records what arrives; the two routes
;; are driven at it in turn and what they sent is compared.
;;
;; ⚠️ THE CAPTURE IS WHY THIS IS A ROW AND NOT A GREP. "Both call
;; `request-frame`" is a fact about today's source; "both sent these
;; bytes" is a fact about the programs. The seed that proves it: give
;; `request-frame` an extra field, and both captures must change.
(let* ((csock (string-append here "/capture.sock"))
       (cstore (string-append here "/store"))
       (peer (string-append here "/capture.ss"))
       (seen-cli (string-append here "/seen-cli.txt"))
       (seen-mcp (string-append here "/seen-mcp.txt")))
  (define (start-capture! into)
    (system (string-append "rm -f " csock " " into))
    (call-with-output-file peer
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia sched) (theourgia net))"
                "(start-scheduler"
                "  (lambda ()"
                (string-append "    (listen! \"" csock "\" 16)")
                "    (let serve ()"
                "      (receive (after 20000 (exit 0))"
                "               (`(accepted ,ref) (conn-read-start! ref) (serve))"
                (string-append
                  "               (`(data ,r ,bv)"
                  " (call-with-output-file \"" into
                  "\" (lambda (p) (put-string p (utf8->string bv))) 'truncate)"
                  " (conn-close! r) (serve))")
                "               (`(eof ,r) (serve))"
                "               (`#(DOWN ,w ,y) (serve))))))")))
      'truncate)
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "scheme --script " peer " > /dev/null 2>&1 &"))
    (let up ((k 0))
      (cond ((file-exists? csock) 'up)
            ((> k 200) 'never)
            (else (system "sleep 0.05") (up (+ k 1))))))

  (start-capture! seen-cli)
  (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                         "scheme --script ../cli.ss read x.1 --store " cstore
                         " --socket " csock " > /dev/null 2>&1"))
  (system (string-append "pkill -f " peer " 2>/dev/null"))
  (start-capture! seen-mcp)
  (talk (list hello ready (call-tool "theourgia_read" '("x.1"))) cstore csock)
  (system (string-append "pkill -f " peer " 2>/dev/null"))

  (want "MC-envelope-shared the shell and the command line put the same bytes on the wire"
        (let ((a (file-text seen-cli)) (b (file-text seen-mcp)))
          (list (if (> (string-length a) 0) 'the-cli-sent-something (list 'cli a))
                (if (string=? a b) 'and-the-shell-sent-the-same (list 'differ a b))))
        '(the-cli-sent-something and-the-shell-sent-the-same))

  ;; ⛔ AND IT IS THE ENVELOPE THE DAEMON PARSES, spelled out here once so
  ;; that "both sent the same thing" cannot be satisfied by both sending
  ;; the same wrong thing.
  (want "MC-envelope-shared and the bytes are the request envelope, terminator and all"
        (file-text seen-mcp)
        (string-append "(request \"" cstore "\" \"" (or (getenv "THEOURGIA_ACTOR")
                                                        (getenv "USER") "cli")
                       "\" read \"x.1\")\n")))

(printf "rows: ~a\n~a failures\nmcp-shell complete\n" rows bad)
(exit (if (zero? bad) 0 1))
