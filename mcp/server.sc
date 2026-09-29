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

;; The MCP shell: JSON-RPC 2025-11-25 over stdio, in front of the same
;; dispatcher everything else uses.
;;
;; NEVER: THE JSON IS THE ENVELOPE AND NOTHING ELSE. What a tool returns is
;; the core's answer as S-expression TEXT, byte for byte -- this shell
;; does not read it, reformat it, or decide anything from it (§7.6.4,
;; §7.6.37). The one question it asks about an answer is whether the
;; TRANSPORT produced one.
;;
;; NEVER: A CORE REFUSAL IS A SUCCESSFUL RESULT. `(error unknown-id …)` is the
;; answer to the question that was asked, so it comes back as text with
;; `isError: false`. Only this shell's own failures -- a broken frame, a
;; daemon that took the request and then lost the answer -- use the
;; JSON-RPC error channel. Translating core refusals into MCP errors
;; would tell a client that its command could not be run when it ran and
;; was refused.
;;
;; NEVER: AND IT BRANCHES ON THE TRANSPORT'S TAG, NEVER ON THE ANSWER'S TEXT.
;; `exchange` answers `(answer <bytes>)` or `(transport-error <what>)`;
;; a core answer whose text happens to read `(error transport-unknown …)`
;; is still an answer. Reading the text to decide would make a payload
;; able to impersonate a transport failure.

;; NEVER: THE SHELL IS AS THIN AS THE COMMAND LINE. It does not import the
;; core, the scheduler or the socket machinery: it speaks to a daemon
;; through `(theourgia client)` and knows nothing about what any verb
;; means -- except eval's `--timeout-ms`, read only to decide how long to
;; wait for the child it runs for eval (the child budget). NOTE: It used to dispatch in this process when there was no
;; daemon, which meant every shell loaded the whole core to serve the
;; first call -- and that is the cost the split exists to avoid. It now
;; starts a daemon instead, the same way the command line does.
(import (chezscheme)
        (only (theourgia render) answer-printing! render-wire)
        (only (theourgia client)
              socket-path serve-log-path request-frame call! ensure-daemon!
              answer-field readable-shape?)
        ;; NOTE: THESE NAMES WIDEN NOTHING: `(theourgia ffi)` is already in
        ;; this shell's closure through `(theourgia client)`, which is what
        ;; opens the socket and starts the daemon. The child route uses the
        ;; rest: the spawn, the pid wait and signal, the call directory.
        (only (theourgia platform-numbers) platform-number)
        (only (theourgia ffi) reap-children! spawn-captured! waitpid-status signal-pid!
              mkdir-exclusive! mkdir-p! real-path entry-type unlink! rmdir! file-size
              read-entry read-entry-range theourgia-stage hold-point! trace-event!
              fs-error? fs-error-errno fd-open write-all! fd-close)
        (only (theourgia client) run-root store-key)
        ;; F100b item 8: a refusal is answered through ONE renderer, and a
        ;; filesystem condition raised inside a request is answered by the
        ;; table (point 7).
        (only (theourgia refusal) refusal-result-json refusal-error-json)
        (only (theourgia answers) classify-failure)
        (theourgia json)
        (theourgia arguments))

;; ---- what this shell is pinned to -------------------------------------------

(define protocol-version "2025-11-25")
(define frame-limit (* 1024 1024))
(define answer-deadline-ms 30000)

(define instructions
  (string-append
    "Tools return the unmodified core command answer as S-expression text. "
    "Core refusals are successful transport results. "
    "Eval runs on the store's machine as a child of this shell; a language other than Scheme runs only where the operator has set THEOURGIA_RUNNERS=on for it."))

;; THE SESSION'S WRITER IS SAID TO THE CLIENT: the three sentences above,
;; and one more naming the writer this run chose and how an earlier
;; session's drafts are reached. The third said "Eval is available only in
;; the local CLI" until the shell began to carry eval out as its child.
(define (session-instructions)
  (string-append instructions
                 " This session's writer is " (shell-writer)
                 "; drafts left by an earlier session are read with"
                 " `drafts --writer <that session's writer>`."))

;; ---- naming a tool ----------------------------------------------------------
;;
;; NEVER: A VERB IS NOT ALWAYS A LEGAL TOOL NAME, and the ones that are not
;; must still be reachable. A verb of plain characters keeps its
;; spelling; anything else -- and anything already starting `x_`, so the
;; escape cannot collide with a real verb -- is carried as hex. NOTE: The
;; `x_` exclusion is what makes the mapping injective: without it a verb
;; literally called `x_612f62` and the escaped form of `a/b` would be one
;; name.
(define (plain-name? text)
  (let loop ((i 0))
    (or (= i (string-length text))
        (let ((c (string-ref text i)))
          (and (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9)
                   (char=? c #\_) (char=? c #\.) (char=? c #\-))
               (loop (+ i 1)))))))

(define (starts-with? text prefix)
  (let ((n (string-length prefix)))
    (and (>= (string-length text) n) (string=? (substring text 0 n) prefix))))

(define (hex-of text)
  (let ((bytes (string->utf8 text)))
    (call-with-string-output-port
      (lambda (port)
        (let loop ((i 0))
          (when (< i (bytevector-length bytes))
            (let ((b (bytevector-u8-ref bytes i)))
              (put-string port (string (string-ref "0123456789abcdef" (div b 16))
                                       (string-ref "0123456789abcdef" (mod b 16)))))
            (loop (+ i 1))))))))

(define (tool-name verb)
  (let* ((text (symbol->string verb))
         (suffix (if (and (> (string-length text) 0)
                          (plain-name? text)
                          (not (starts-with? text "x_")))
                     text
                     (string-append "x_" (hex-of text))))
         (name (string-append "theourgia_" suffix)))
    (and (<= (string-length name) 128) name)))

;; NEVER: ASKED AGAIN EVERY TIME, NEVER: never cached, and NEVER: NOT BUILT HERE. The
;; list of verbs, what each one is for, and the writing protocol all come
;; from the server's `describe`. A shell that kept its own copy would be
;; a second description of the same verbs, and a tool description that
;; has drifted from the verb it describes is worse than none -- it is
;; read and believed.
;;
;; NOTE: AND IT IS ASKED OF THE DAEMON, which is what makes `tools/list` the
;; call that starts one when there is none.
;;
;; Answers `(ok <entries>)`, `(unavailable)` for a catalogue that does not
;; read, `(refused <datum> <origin>)` for a refusal of `describe` (origin
;; `transport` or #f, F100b E12), or a start-failed / not-sent /
;; transport-lost outcome as itself. NEVER: NOT an empty list on
;; failure: an empty tool list is a valid answer meaning "this server has
;; no tools", and a client that got one would stop asking.
(define (catalogue store actor socket)
  (let ((answer (ask store actor socket 'describe '())))
    (if (not (eq? 'text (car answer)))
        ;; NOTE: A SERVER THAT WOULD NOT START IS CARRIED THROUGH AS ITSELF.
        ;; Flattened to `unavailable` here, its reason would be lost one
        ;; layer before the place that reports it.
        ;; A REFUSAL THE TRANSPORT MADE IS CARRIED THROUGH TOO (F100b E12),
        ;; as `(refused <datum> transport)`.
        (cond
          ((memq (car answer) '(start-failed not-sent)) answer)
          ;; NEVER: A LOST ANSWER IS NOT AN UNREADABLE CATALOGUE. The request
          ;; went out and its answer did not come back: whether it ran is
          ;; unknown. Folded into `unavailable` it was answered as "core did
          ;; not start" (F100b M3a r1, MC-07 TWIN), which says something the
          ;; shell does not know.
          ((eq? 'transport-lost (car answer)) answer)
          ((and (eq? 'transport-refusal (car answer)) (refusal-datum (cadr answer)))
           => (lambda (d) (list 'refused d 'transport)))
          (else (list 'unavailable)))
        (let ((datum (guard (e (#t #f))
                       ;; NEVER: THE GUARD BELONGS TO THE READER, NOT TO THE
                       ;; ENVELOPE. The shape check ran over the envelope,
                       ;; where this text sat INSIDE a string and was never
                       ;; looked at -- so a catalogue whose own datum
                       ;; carries a cycle passed the outer check and hung
                       ;; the walk that follows. Every text a peer wrote is
                       ;; asked before it is read, wherever it was carried.
                       (let ((inner (cadr answer)))
                         (and (readable-shape? inner)
                              (read (open-string-input-port inner)))))))
          (cond
            ;; A PARSED `(error ...)` IS THE CORE'S REFUSAL OF `describe`, NOT
            ;; AN UNREADABLE CATALOGUE (F100b E12): it is carried as
            ;; `(refused <datum> #f)` and answered with its own kind.
            ((refusal-datum? datum) (list 'refused datum #f))
            ((not (and (pair? datum) (eq? 'ok (car datum))))
              (list 'unavailable))
            (else
              (let ((verbs (assq 'verbs (cdr datum)))
                    (protocol (assq 'protocol (cdr datum))))
                (if (not (and verbs protocol (string? (cadr protocol))))
                    (list 'unavailable)
                    (list 'ok (tools-from (cdr verbs) (cadr protocol)))))))))))

;; `(error <symbol> ...)`, the shape of every refusal datum.
(define (refusal-datum? d)
  (and (list? d) (>= (length d) 2) (eq? 'error (car d)) (symbol? (cadr d))))

;; The refusal datum a transport refusal's text carries, or #f. The text is
;; peer text: it is asked before it is read.
(define (refusal-datum text)
  (and (string? text) (readable-shape? text)
       (let ((d (guard (e (#t #f)) (read (open-string-input-port text)))))
         (and (refusal-datum? d) d))))

(define (tools-from entries protocol)
  (let loop ((es entries) (out '()))
    (cond
      ((null? es) (reverse out))
      (else
       (let* ((entry (car es))
              (verb (car entry))
              (name (tool-name verb)))
         (cond
           ;; A verb whose name will not fit is left out rather than
           ;; breaking the whole listing.
           ((not name) (loop (cdr es) out))
           ;; NEVER: AND A VERB THIS SHELL CANNOT CARRY OUT IS NOT OFFERED.
           ;; It carries out two routes: `daemon`, sent to the store's
           ;; server, and `child`, run as this shell's own child. `init` is
           ;; `local` -- what CREATES a store, so there is no daemon to send
           ;; it to -- and offered as a tool it could never succeed; an agent
           ;; told the capability exists would keep trying. A host creates
           ;; the store once before starting this.
           ((not (memq (entry-field entry 'route) '(daemon child))) (loop (cdr es) out))
           (else
            (loop (cdr es)
                  (cons (list name verb (description-of entry protocol) (entry-field entry 'route))
                        out)))))))))

(define (entry-field entry name)
  (let ((hit (assq name (cdr entry))))
    (and (pair? hit) (pair? (cdr hit)) (cadr hit))))

;; KEY: THE TWO TOOLS THAT WRITE CARRY THE PROTOCOL, and they carry the
;; server's copy of it rather than a sentence written here. An agent
;; choosing a tool from `tools/list` reads the description and nothing
;; else; if the rules for writing live in a README it will not open, they
;; are rules it will not follow.
;;
;; NOTE: WHICH TOOLS THOSE ARE IS THE SERVER'S ANSWER TOO -- the `protocol`
;; flag on each entry -- and not a list kept here. A list kept here would
;; be a third place that knows which tool descriptions carry the
;; protocol.
(define (description-of entry protocol)
  (let ((plain (or (entry-field entry 'description) "")))
    (if (entry-field entry 'protocol)
        (string-append protocol "\n" plain)
        plain)))

;; ---- the JSON-RPC envelope ---------------------------------------------------

(define (jstring text) text)

(define (error-frame identity code message)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (id-json identity)
                 ",\"error\":{\"code\":" (json-number code)
                 ",\"message\":" (json->string message) "}}"))

(define (error-object-frame identity error-json)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (id-json identity)
                 ",\"error\":" error-json "}"))

(define (result-frame identity body)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (id-json identity)
                 ",\"result\":" body "}"))

;; NOTE: AN ID COMES BACK AS IT CAME. A string id must not turn into a
;; number, and a number must not turn into a string: a client matches
;; replies by it.
;; NEVER: AN ID IS ECHOED AS THE CLIENT WROTE IT, AND A NUMBER CANNOT BE
;; RE-PRINTED FAITHFULLY. Parsing turns the id into a Scheme number and
;; printing that number turns it back into text, and the two are not
;; inverses: `9007199254740993.0` came back as `9.007199254740992e15` --
;; a DIFFERENT number -- so a client matching replies by id could not
;; match its own. The rounding happens inside the parser, so no amount of
;; care at printing time can undo it.
;;
;; NOTE: SO THE TOKEN IS KEPT, and this is the only place in this core that
;; looks at JSON text without the parser. That is a real cost -- two
;; things that know the syntax -- so it is paid under a check: the text
;; taken from the frame is parsed again, and it is used ONLY if it yields
;; the very number the parser produced. A scan that drifts from the
;; parser cannot echo a wrong id; it can only fall back to printing.
(define id-text #f)

;; NOTE: THE KEPT TEXT IS USED ONLY IF IT IS THE SAME ID. It is parsed again
;; and compared with what the request's own parse produced: a number by
;; value, a string by content. A scan that drifted from the parser cannot
;; echo somebody else's token; it can only fall back to printing.
(define (faithful-id-text value)
  (and (string? id-text)
       (let ((again (guard (e (#t 'no)) (string->json id-text))))
         (and (cond ((string? value) (and (string? again) (string=? again value)))
                    ((number? value) (and (number? again) (= again value)))
                    (else #f))
              id-text))))

;; NEVER: EVERY ID IS ECHOED AS ITS OWN TOKEN, strings included. A string id
;; was re-serialised from the parsed value, so an id the client wrote as
;; `"a\u0062c"` came back as `"abc"` -- the same string to a reader that
;; compares values, a different one to a client matching the text it sent.
;; And the integer test was applied AFTER the parser had rounded, so
;; `1.0000000000000001` passed as an integer while `9007199254740993`
;; came back as a different number.
;;
;; The token that arrived is written back. What must be true of it is only
;; that the protocol allows it: a string or a number.
;; NEVER: SCHEME'S NUMBER PRINTER IS NOT A JSON PRINTER. Chez writes a flonum
;; that has fewer bits than a full mantissa with its precision after a
;; bar -- `5e-324` comes back as `5e-324|1` -- and that is not a JSON
;; number, so the whole response line stops being JSON. Measured: a ping
;; whose id was `5e-324` was answered `{"jsonrpc":"2.0","id":5e-324|1,...`,
;; which a conforming host cannot parse at all.
;;
;; NOTE: THE DIGITS BEFORE THE BAR ARE THE WHOLE VALUE. The suffix says how
;; many bits are significant; it does not add any. Reading what is left
;; gives back the same double, so dropping it loses nothing.
;;
;; KEY: AND IT IS HERE, NOT AT THE ONE CALLER THAT WAS SEEN TO NEED IT.
;; "What this process writes is JSON" is this layer's promise, and the
;; fallback below is only one of the ways a number reaches the line.
(define (json-number x)
  (let* ((text (number->string x))
         (bar (let look ((i 0))
                (cond ((>= i (string-length text)) #f)
                      ((char=? (string-ref text i) #\|) i)
                      (else (look (+ i 1)))))))
    (if bar (substring text 0 bar) text)))

(define (id-json identity)
  (cond ((or (string? identity) (number? identity))
         (or (faithful-id-text identity)
             (if (string? identity) (json->string identity) (json-number identity))))
        (else "null")))

;; NEVER: A KEY IS COMPARED BY WHAT IT MEANS, NOT BY HOW IT IS SPELLED.
;; `"i\u0064"` is JSON for `id`, and a literal comparison against `"id"`
;; misses it -- so the id's own token was never found and the answer fell
;; back to reprinting the parsed value. Measured: a ping keyed
;; `"i\u0064"` with the id `5e-324` was answered with a line that was not
;; JSON, by the printer this scan exists to avoid using.
;;
;; NOTE: DECODED BY THE PARSER THIS FILE ALREADY HAS, not by an unescaper
;; written here. A second reader of JSON's string syntax would be a second
;; thing to keep in step with the first, and the drift would show up as
;; exactly this kind of miss.
(define (same-member-name? key name)
  (let ((decoded (guard (e (#t #f)) (string->json key))))
    (and (string? decoded) (string=? decoded name))))

;; The raw text of one member of the outermost JSON object. Values are
;; skipped by shape -- strings by their escapes, objects and arrays by
;; depth -- so a member of the same name nested inside `params` is not
;; mistaken for this one.
(define (top-level-member-text text name)
  (let ((n (string-length text)))
    (define (skip-ws i)
      (if (and (< i n) (char-whitespace? (string-ref text i))) (skip-ws (+ i 1)) i))
    (define (end-of-string i)
      (let loop ((i (+ i 1)))
        (cond ((>= i n) #f)
              ((char=? (string-ref text i) #\\) (loop (+ i 2)))
              ((char=? (string-ref text i) #\") (+ i 1))
              (else (loop (+ i 1))))))
    (define (end-of-nested i)
      (let loop ((i i) (depth 0))
        (cond ((>= i n) #f)
              ((char=? (string-ref text i) #\")
               (let ((e (end-of-string i))) (and e (loop e depth))))
              ((or (char=? (string-ref text i) #\{) (char=? (string-ref text i) #\[))
               (loop (+ i 1) (+ depth 1)))
              ((or (char=? (string-ref text i) #\}) (char=? (string-ref text i) #\]))
               (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1))))
              (else (loop (+ i 1) depth)))))
    (define (end-of-bare i)
      (let loop ((i i))
        (cond ((>= i n) n)
              ((let ((c (string-ref text i)))
                 (or (char-whitespace? c) (char=? c #\,) (char=? c #\}) (char=? c #\])))
               i)
              (else (loop (+ i 1))))))
    (define (end-of-value i)
      (cond ((>= i n) #f)
            ((char=? (string-ref text i) #\") (end-of-string i))
            ((or (char=? (string-ref text i) #\{) (char=? (string-ref text i) #\[))
             (end-of-nested i))
            (else (end-of-bare i))))
    (let ((start (skip-ws 0)))
      (and (< start n)
           (char=? (string-ref text start) #\{)
           (let member ((i (skip-ws (+ start 1))))
             (and (< i n)
                  (char=? (string-ref text i) #\")
                  (let ((key-end (end-of-string i)))
                    (and key-end
                         (let* ((key (substring text i key-end))
                                (colon (skip-ws key-end)))
                           (and (< colon n)
                                (char=? (string-ref text colon) #\:)
                                (let* ((vstart (skip-ws (+ colon 1)))
                                       (vend (end-of-value vstart)))
                                  (and vend
                                       (if (same-member-name? key name)
                                           (substring text vstart vend)
                                           (let ((next (skip-ws vend)))
                                             (and (< next n)
                                                  (char=? (string-ref text next) #\,)
                                                  (member (skip-ws (+ next 1))))))))))))))))))

;; NOTE: `stdin` IS OPTIONAL AND ADDITIVE. A host that never sends it sees
;; the tools it saw before; a host that does can call the verbs whose input
;; comes from standard input -- `batch`, and `write <id> -` -- which on this
;; route could not be called at all, because the shell had no way to carry
;; what would have been piped in.
(define (schema-json)
  (string-append "{\"type\":\"object\",\"properties\":{\"argv\":{\"type\":\"array\","
                 "\"items\":{\"type\":\"string\"}},"
                 "\"stdin\":{\"type\":\"string\"}},\"required\":[\"argv\"],"
                 "\"additionalProperties\":false}"))

;; KEY: THE TWO TOOLS THAT WRITE CARRY THE PROTOCOL, and they carry the
;; core's copy of it rather than a sentence written here. An agent
;; choosing a tool from `tools/list` reads the description and nothing
;; else; if the rules for writing live in a README it will not open,
;; they are rules it will not follow.
;;
;; NOTE: EVERY OTHER TOOL KEEPS THE PLAIN SENTENCE. The protocol is about
;; writing a block, and putting it on `read` or `search` would be noise
;; in the place an agent is choosing from.
(define writing-verbs '(insert write))

(define (description-for verb)
  (let ((plain (string-append "Execute the core " (symbol->string verb)
                              " command and return its exact S-expression answer.")))
    (if (memq verb writing-verbs)
        (string-append write-protocol "\n" plain)
        plain)))

(define (tools-json entries)
  (string-append
    "{\"tools\":["
    (let loop ((es entries) (out ""))
      (cond
        ((null? es) out)
        (else
         (loop (cdr es)
               (string-append
                 out (if (string=? out "") "" ",")
                 "{\"name\":" (json->string (car (car es)))
                 ",\"description\":" (json->string (caddr (car es)))
                 ",\"inputSchema\":" (schema-json) "}")))))
    "]}"))

(define (text-result-json text)
  (string-append "{\"content\":[{\"type\":\"text\",\"text\":" (json->string text)
                 "}],\"isError\":false}"))

;; ---- reaching the daemon -----------------------------------------------------
;;
;; NEVER: ONE ROUTE TO A DAEMON, AND A CHILD ROUTE. There is a daemon or there
;; is about to be one: the shell asks, and if nothing is listening it starts
;; one and asks again. A verb the catalogue routes `child` -- eval -- is not
;; sent at all: it is run as this shell's child (below). NOTE: It
;; used to fall back to dispatching in this process, which is why this
;; program imported the core -- and a shell that loads the core to serve
;; its first call has paid for the server it exists not to be.
;;
;; NEVER: AND A LOST ANSWER IS NOT AN EMPTY ONE. Once the frame has gone out
;; the request may have been carried out, so it is never re-sent and
;; never reported as not having happened.
;;
;; NEVER: A STORE THAT STARTS WITH "-" GOES AS `--store <store>`. The daemon
;; refuses a token spelled like an option that it does not read (F94), and a
;; store is passed verbatim, so a store named `--x` given as a positional was
;; refused where the client meant a store; the value after `--store` is taken
;; whatever its spelling. Every other store keeps the positional, so its
;; daemon's command line is the one it always was (`serve <store> ...`),
;; which is what anything that finds a daemon by its command line matches.
(define (server-argv store socket)
  (append (list (or (getenv "THEOURGIA_SCHEME") "scheme")
                "--script" (beside-this-program "theourgiad.sc") "serve")
          (if (and (> (string-length store) 0) (char=? (string-ref store 0) #\-))
              (list "--store" store)
              (list store))
          (list "--socket" socket
                "--detach" "--log" (serve-log-path store))))

;; NOTE: `theourgiad.sc`, THE DAEMON THIS SHELL STARTS (F46), SITS ONE LEVEL
;; UP: this program lives in `mcp/`.
(define (beside-this-program name)
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring argv0 0 cut) ".")))
    (string-append dir "/../" name)))

(define (ask store actor socket verb args . piped)
  ;; NEVER: THE SHELL OUTLIVES THE DAEMONS IT STARTS. Every start races for
  ;; the store's lock and the loser exits immediately, and nothing waited
  ;; for it: a one-shot client exits and the entries go with it, but this
  ;; process serves a whole session, so they accumulate for as long as it
  ;; runs. Collected here because this is the one place the shell speaks
  ;; to a daemon, so "before every call" is a single line rather than a
  ;; rule every caller has to remember.
  (reap-children!)
  (let* ((frame (request-frame store verb args
                               ;; NOTE: THE SHELL IS BOUND TO ONE IDENTITY FOR
                               ;; ITS WHOLE RUN, so the writer is the
                               ;; shell's, not something read out of each
                               ;; tool call. `wire` because this answer is
                               ;; parsed, not shown to anyone.
                               ;; NEVER: WHAT THE CALLER PIPED IN AND WHERE THIS
                               ;; PROCESS IS. Neither used to travel from
                               ;; this shell, so a tool whose input comes
                               ;; from standard input could not be called,
                               ;; and a relative path was read wherever the
                               ;; daemon happened to be started.
                               (list (cons 'actor actor)
                                     (cons 'writer (shell-writer))
                                     (cons 'stdin (and (pair? piped)
                                                       (string? (car piped))
                                                       (car piped)))
                                     (cons 'cwd (current-directory))
                                     (cons 'mode 'wire))))
         (first (call! socket frame answer-deadline-ms)))
    (settle first
            (lambda ()
              (let ((started (ensure-daemon! (server-argv store socket) store socket)))
                (if (eq? 'ready started)
                    (settle (call! socket frame answer-deadline-ms) #f)
                    (list 'start-failed started)))))))

;; KEY: ONE CLASSIFICATION, WHICHEVER SEND PRODUCED THE OUTCOME. The second
;; send used to fold every non-answer into `transport-lost` -- "execution
;; may be unknown" -- including outcomes that establish the opposite. A
;; connect that fails after the daemon has announced itself ready sent
;; nothing, exactly as it sent nothing the first time.
;;
;; `start` is what to do about nobody listening; #f is the second time,
;; where nobody listening is again proof that nothing was sent, NEVER: and
;; never grounds for starting another one.
(define (settle outcome start)
  (cond
      ((eq? 'answer (car outcome)) (unpack outcome))
      ;; NEVER: NOT THE SAME SENTENCE AS A SERVER THAT WOULD NOT START. The
      ;; library worked this out itself -- a socket path that cannot fit
      ;; in `sun_path` is known before anything is opened -- so saying
      ;; "the server said" would attribute to the server a thing it never
      ;; said and was never asked.
      ((eq? 'not-sent (car outcome)) (list 'not-sent (cadr outcome)))
      ;; Nothing is listening: this request reached nobody, so starting a
      ;; daemon and asking once does the work once.
      ;;
      ;; NEVER: AND A SERVER THAT WOULD NOT START IS NOT "MAY HAVE EXECUTED".
      ;; This used to answer `transport-lost` there, which the caller was
      ;; told meant "execution may be unknown" -- a sentence that is
      ;; simply false: the frame never went out, so nothing ran. It also
      ;; discarded the only useful thing anyone had, the server's own
      ;; reason for not starting. The command-line client relays that
      ;; verbatim; this said something untrue instead.
      ((eq? 'no-daemon (car outcome))
       (if start
           (start)
           (list 'not-sent (list 'error 'connect-failed
                                 (list 'errno (cadr outcome))
                                 '(carried-out no)))))
      (else (list 'transport-lost))))

;; NEVER: THE SHELL TAKES `stdout` AND NOTHING ELSE from the envelope. The
;; text it returns to its caller is the core's answer, and that is what
;; `stdout` holds. `exit` is the server's verdict on the same answer, and
;; §7.6.4 fixed that a core `(error ...)` is a SUCCESSFUL tool call whose
;; text happens to be a refusal -- turning a non-zero exit into a
;; JSON-RPC error here would undo exactly that.
;; NEVER: WHO REFUSED IS READ FROM THE ENVELOPE, NOT FROM THE WORDS. A core
;; `(error unknown-id ...)` is a successful tool call whose answer is no
;; (§7.6.4). A daemon that is draining, or that lost the process serving
;; the request, refuses in the same envelope with the same shape -- and
;; those are not answers to the question asked. The daemon now says which
;; it is; before it did, both arrived as ordinary tool results and the
;; caller was told its request had been carried out.
;;
;; NOTE: AN ENVELOPE WITH NO `origin` IS TREATED AS THE CORE'S, which is
;; what every envelope was before the field existed.
(define (unpack outcome)
  (let ((envelope (guard (e (#t 'unreadable))
                    (let ((text (utf8->string (cadr outcome))))
                      ;; NEVER: ASKED BEFORE THE READER IS HANDED IT: a datum
                      ;; label makes a cycle that every later walk follows
                      ;; forever, and this process serves a whole session.
                      (if (readable-shape? text)
                          (read (open-string-input-port text))
                          'unreadable)))))
    (let ((text (answer-stdout envelope))
          (origin (answer-origin envelope)))
      (cond
        ((not text) (list 'transport-lost))
        ((eq? origin 'transport) (list 'transport-refusal text))
        (else (list 'text text))))))

(define (answer-origin envelope)
  (answer-field envelope 'origin (lambda (x) #t)))

;; ---- carrying out eval: the child route ------------------------------------
;;
;; KEY: THE CHILD IS THE COMMAND LINE'S OWN EVAL INVOCATION. `theourgia eval
;; <argv>` execs `core.sc eval <argv>`; this shell runs the same program with
;; the same argv as its CHILD and reads the answer, so it can answer the next
;; call afterwards. The daemon never executes user code, and this process
;; loads no core: the evaluation, its supervisor and its limits are core.sc's.
;;
;; NEVER: THE CHILD'S argv IS THE CALLER'S, VERBATIM AND ALONE. The store,
;; the wire mode and this session's writer reach it through its
;; environment -- THEOURGIA_STORE, THEOURGIA_WIRE=1, THEOURGIA_WRITER --
;; each REPLACING any binding of that name. Nothing is prepended to the
;; argv, so the child's parse of it is the caller-only parse, and every
;; refusal of that argv is the one the command line gives for it.
;;
;; NEVER: ITS STREAMS ARE THIS CALL'S FILES. fd 0 is the tool's stdin written
;; to a file, or /dev/null; fd 1 and fd 2 are the call's answer and
;; diagnostics files. The child can never read this shell's standard input,
;; which is the host's pipe, nor write into its answers.
;;
;; OUTCOMES: `(text <s>)` when the child exited 0 or 1 with a non-empty
;; answer that is UTF-8 -- whatever the datum says, as for every tool; a
;; result datum for a refusal this shell makes before anything runs;
;; `(not-sent <datum>)` when nothing ran; `(child-lost <reason> <diag>)`
;; otherwise, and that one says execution may be unknown.
(define (child-route store verb argv stdin)
  (let ((nodes (parse-arguments verb argv)))
    (cond
      ;; NEVER: C STRINGS END AT NUL, so an argument holding one would reach the
      ;; child cut short. Refused before anything is made or run.
      ((or (exists (lambda (a) (string-has-nul? a)) argv)
           (and (string? stdin) (string-has-nul? stdin)))
       (list 'result-datum '(error bad-request nul-in-argument)))
      ;; A CALLER'S argv MAY NOT NAME THE TRANSPORT, on this route as on the
      ;; daemon's (transport-option-in-rpc, the same list): the child would
      ;; take a caller's `--store` over the store this shell serves. Only an
      ;; option NODE counts, so positional text that merely spells one, or
      ;; anything after `--`, is not refused. A parse ERROR is not this
      ;; shell's to answer: the child answers it, as the command line would.
      ((and (not (parse-error? nodes))
            (exists (lambda (o) (argument-option nodes o)) transport-options))
       (list 'result-datum '(error bad-request transport-option-in-rpc)))
      (else
       (let ((core (beside-this-program "core.sc")))
         (if (not (eq? (entry-type core) 'regular))
             (list 'not-sent (list 'error 'core-missing (list 'path core)))
             (let ((dir (claim-call-directory store)))
               (if (not dir)
                   (list 'not-sent '(error scratch-unavailable))
                   (run-child-in dir core argv stdin (child-budget-ms nodes))))))))))

(define (parse-error? nodes) (and (pair? nodes) (eq? (car nodes) 'error)))

(define (string-has-nul? s)
  (let loop ((i 0))
    (and (< i (string-length s))
         (or (char=? (string-ref s i) #\nul) (loop (+ i 1))))))

;; THE BUDGET, IN ONE PLACE. T is the caller's `--timeout-ms` when it is an
;; exact integer in 1..60000 -- the bound core.sc applies -- and 3000
;; otherwise: an invalid value is refused by the child at once, so the
;; default is enough for that. Twice T covers a wait for an evaluation slot
;; (at most T) and the evaluation (at most T); preparation is on top.
;;
;; NOTE: THIS IS THE ONE PLACE THE SHELL READS A VERB'S OPTION, and it reads
;; it only to decide how long to wait for a child, never to decide anything
;; about the request.
(define (child-budget-ms nodes)
  (let* ((given (and (not (parse-error? nodes)) (argument-option nodes "--timeout-ms")))
         (n (and (string? given) (string->number given 10)))
         (t (if (and n (exact? n) (integer? n) (<= 1 n 60000)) n 3000)))
    (+ (* 2 t) preparation-ms)))

;; A CHOSEN ALLOWANCE, NOT A MEASURED BOUND: ready 5 s, the group wait and
;; drain 3 s, and 62 s for projection, cleanup and everything else a child
;; does before and after its evaluation. THEOURGIA_MCP_PREPARATION_MS
;; replaces it, a positive integer, checked at start; it is a test seam.
(define preparation-ms 70000)

(define (preparation-from-environment)
  (let ((v (getenv "THEOURGIA_MCP_PREPARATION_MS")))
    (cond ((not v) 70000)
          ((let ((n (string->number v 10))) (and n (exact? n) (integer? n) (> n 0) n)) => (lambda (n) n))
          (else #f))))

;; THE CALL'S DIRECTORY IS THIS SHELL'S, CLAIMED EXCLUSIVELY. Under the run
;; root resolved once by real-path, in the daemon's own `<store-key>`
;; directory (made if absent), `mcp-<pid>-<counter>` with mode 0700; a name
;; already taken -- a file, a directory, a link -- is left alone and the
;; next counter is tried, up to 8 names. -> the directory, or #f when the
;; root cannot be resolved or every name was taken. A filesystem condition
;; raised here is left to raise: the request's table answers it as a
;; result, as it answers any other before the spawn.
(define call-counter 0)

(define (claim-call-directory store)
  (let ((root (real-path (run-root))))
    (and root
         (let ((parent (string-append root "/" (store-key store))))
           (mkdir-p! parent)
           (let loop ((tries 0))
             (and (< tries 8)
                  (let ((dir (string-append parent "/mcp-" (number->string (get-process-id))
                                            "-" (number->string call-counter))))
                    (set! call-counter (+ call-counter 1))
                    (if (eq? 'created (mkdir-exclusive! dir #o700))
                        dir
                        (loop (+ tries 1))))))))))

(define (scheme-binary)
  (or (getenv "THEOURGIA_SCHEME") "scheme"))

(define (run-child-in dir core argv stdin budget)
  (let ((stdin-path (and (string? stdin) (string-append dir "/stdin")))
        (answer-path (string-append dir "/answer"))
        (diag-path (string-append dir "/diag")))
    ;; THE STDIN FILE IS WRITTEN THROUGH THE DOOR, whose failures are the
    ;; conditions the request's table classifies; a native port's I/O
    ;; condition is not one, and was answered as an unknown transport
    ;; failure. And whatever this step raises, the call's directory is
    ;; removed first: it was claimed, and nothing else would remove it.
    (when stdin-path
      (guard (e (#t (remove-call-files! dir (list stdin-path answer-path diag-path))
                    (raise e)))
        (let ((fd (fd-open stdin-path '(write create))))
          ;; A write that fails leaves the descriptor open; it is closed
          ;; before the failure goes on, or a session would keep it.
          (guard (e (#t (guard (x (#t #f)) (fd-close fd)) (raise e)))
            (write-all! fd (string->utf8 stdin) stdin-path))
          (fd-close fd))))
    (let-values (((pid spawn-errno)
                  (spawn-captured! (append (list (scheme-binary) "--script" core "eval") argv)
                                   (list (cons "THEOURGIA_STORE" serving-store)
                                         (cons "THEOURGIA_WIRE" "1")
                                         (cons "THEOURGIA_WRITER" (shell-writer)))
                                   stdin-path answer-path diag-path)))
      (if (not pid)
          (begin (remove-call-files! dir (list stdin-path answer-path diag-path))
                 (list 'not-sent (list 'error 'spawn-failed (list 'errno spawn-errno))))
          (let ((ending (await-child pid budget)))
            (let ((outcome (outcome-of ending answer-path diag-path)))
              (remove-call-files! dir (list stdin-path answer-path diag-path))
              outcome))))))

;; THE STORE THIS SHELL SERVES, as the child is told it.
(define serving-store #f)

(define (now-ms)
  (let ((t (current-time 'time-monotonic)))
    (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))

(define (pause-ms ms) (sleep (make-time 'time-duration (* ms 1000000) 0)))

;; NEVER: THE STATUS IS ASKED OF THAT PID, NOT OF ANY CHILD. While a child is
;; owned here nothing calls reap-children!, which collects any exited child
;; and would take this one's status; `ask` runs before or after, never
;; during. A waitpid that raises, on any poll, is caught here: its execution
;; is unknown, and left to the request's table it would be answered as a
;; filesystem RESULT.
;;
;; -> (status <(exit n)|(signal s)>), (wait-failed <errno>),
;;    (deadline <ms>), (deadline-unreaped <ms>) or (signal-failed <errno>).
(define (await-child pid budget)
  ;; THE WATCHDOG STARTS WHEN THE SPAWN RETURNS, before anything else: a hold
  ;; here is time the child's budget pays for, not time added to it.
  (let ((deadline (+ (now-ms) budget)))
    ;; THE HOLD SEAM EXISTS ONLY IN AN INJECTION BUILD, and a marker it cannot
    ;; make must not end the wait: the child has started, and leaving here
    ;; would report a filesystem result for a request that ran.
    (guard (e (#t (void))) (hold-point! 'mcp-child-wait))
    (let poll ()
      (let ((status (poll-child pid)))
        (cond
          ((and (pair? status) (eq? (car status) 'wait-failed)) status)
          (status (list 'status status))
          ((< (now-ms) deadline) (pause-ms 25) (poll))
          (else (kill-at-deadline pid budget)))))))

;; -> #f while it runs, its status once it ended, or (wait-failed <errno>).
(define (poll-child pid)
  (guard (e (#t (list 'wait-failed (if (fs-error? e) (fs-error-errno e) 'unknown))))
    (parameterize ((theourgia-stage 'mcp-wait)) (waitpid-status pid))))

;; WHAT A KILL LEAVES. SIGKILL ends the supervisor without running its
;; exits, so its worker's or runner's process group is NOT signalled and its
;; scratch is not removed: a CPU-bound survivor ends at its CPU ceiling, if
;; the kernel accepted one; a sleeping or blocked one has no finite bound
;; from here; and a descendant that left the group had none before either.
;; The status is then polled for up to 2 s.
(define (kill-at-deadline pid budget)
  (let ((r (parameterize ((theourgia-stage 'mcp-signal)) (signal-pid! pid (platform-number 'SIGKILL)))))
    (if (not (eqv? r 0))
        (list 'signal-failed r)
        (let ((grace (+ (now-ms) 2000)))
          (let poll ()
            (let ((status (poll-child pid)))
              (cond
                ((and (pair? status) (eq? (car status) 'wait-failed)) status)
                (status (list 'deadline budget))
                ((< (now-ms) grace) (pause-ms 25) (poll))
                (else (list 'deadline-unreaped budget)))))))))

(define (outcome-of ending answer-path diag-path)
  (case (car ending)
    ((status)
     (let ((status (cadr ending)))
       (cond
         ((and (eq? (car status) 'exit) (memv (cadr status) '(0 1)))
          (let ((bytes (guard (e (#t 'unreadable)) (read-entry answer-path))))
            (cond
              ((not (bytevector? bytes)) (lost "answer-unreadable" diag-path))
              ((= 0 (bytevector-length bytes)) (lost "empty-answer" diag-path))
              ((not (valid-utf8? bytes)) (lost "not-utf8" diag-path))
              (else (list 'text (utf8-text bytes))))))
         ((eq? (car status) 'exit)
          (lost (string-append "exit " (number->string (cadr status))) diag-path))
         (else
          (lost (string-append "signal " (number->string (cadr status))) diag-path)))))
    ((wait-failed) (lost (string-append "wait-failed " (errno-word (cadr ending))) diag-path))
    ((signal-failed) (lost (string-append "signal-failed " (errno-word (cadr ending))) diag-path))
    ((deadline) (lost (string-append "deadline " (number->string (cadr ending))) diag-path))
    (else (lost (string-append "deadline-unreaped " (number->string (cadr ending))) diag-path))))

(define (errno-word e) (if (number? e) (number->string e) "unknown"))

(define (lost reason diag-path)
  (list 'child-lost reason (diag-tail diag-path)))

;; THE LAST 4096 BYTES OF THE DIAGNOSTICS, AND NO EARLIER BYTE: a child may
;; write any amount there, and reading all of it to keep the end would cost
;; its whole size. The range from max(0, size-4096) to the end is read, in
;; one or more reads bounded to it, and the trace records that range as
;; (offset length). -> a string, or #f when the file is not a regular file,
;; cannot be read, or its tail is not UTF-8 -- a tail cut inside a multibyte
;; character included.
(define (diag-tail path)
  (guard (e (#t #f))
    (and (eq? 'regular (entry-type path))
    (let* ((size (file-size path))
           (offset (max 0 (- size 4096))))
      (trace-event! 'diag-read path (list offset (- size offset)))
      (let ((bytes (read-entry-range path offset (- size offset))))
        (and (valid-utf8? bytes) (utf8-text bytes)))))))

;; THE CALL'S FILES ARE REMOVED ON EVERY OUTCOME, each whether it is a file or
;; an empty directory, then the directory. A removal that fails is written to
;; this shell's own stderr -- which is not a channel to the host -- and the
;; directory is left; the call's outcome is unchanged by it.
(define (remove-call-files! dir paths)
  (guard (e (#t (let ((p (current-error-port)))
                  (put-string p "(theourgia-mcp call-directory-left \"")
                  (put-string p dir)
                  (put-string p "\")\n")
                  (flush-output-port p))))
    (for-each (lambda (path)
                (when path
                  (case (entry-type path)
                    ((absent) (void))
                    ((directory) (rmdir! path))
                    (else (unlink! path)))))
              paths)
    (rmdir! dir)))

;; STRICT UTF-8: overlong forms, surrogates and code points above U+10FFFF
;; are refused, as is a sequence cut short at the end.
(define (valid-utf8? b)
  (let ((n (bytevector-length b)))
    (define (byte i) (bytevector-u8-ref b i))
    (define (cont? i) (and (< i n) (= #x80 (fxand (byte i) #xC0))))
    (define (in? i lo hi) (and (< i n) (<= lo (byte i) hi)))
    (let loop ((i 0))
      (if (= i n)
          #t
          (let ((c (byte i)))
            (cond
              ((< c #x80) (loop (+ i 1)))
              ((<= #xC2 c #xDF) (and (cont? (+ i 1)) (loop (+ i 2))))
              ((= c #xE0) (and (in? (+ i 1) #xA0 #xBF) (cont? (+ i 2)) (loop (+ i 3))))
              ((or (<= #xE1 c #xEC) (<= #xEE c #xEF))
               (and (cont? (+ i 1)) (cont? (+ i 2)) (loop (+ i 3))))
              ((= c #xED) (and (in? (+ i 1) #x80 #x9F) (cont? (+ i 2)) (loop (+ i 3))))
              ((= c #xF0) (and (in? (+ i 1) #x90 #xBF) (cont? (+ i 2)) (cont? (+ i 3)) (loop (+ i 4))))
              ((<= #xF1 c #xF3) (and (cont? (+ i 1)) (cont? (+ i 2)) (cont? (+ i 3)) (loop (+ i 4))))
              ((= c #xF4) (and (in? (+ i 1) #x80 #x8F) (cont? (+ i 2)) (cont? (+ i 3)) (loop (+ i 4))))
              (else #f)))))))

;; THE TEXT OF BYTES ALREADY VALIDATED AS STRICT UTF-8, EVERY CHARACTER KEPT.
;; NEVER: NO BYTE-ORDER-MARK HANDLING. utf8->string drops a leading U+FEFF,
;; and putting one back handles only the first of several; the answer is the
;; bytes as read, so they are decoded here code point by code point and a
;; U+FEFF is a character like any other, wherever it is.
(define (utf8-text b)
  (let ((n (bytevector-length b)))
    (define (byte i) (bytevector-u8-ref b i))
    (define (tail i) (fxand (byte i) #x3F))
    (let loop ((i 0) (out '()))
      (if (= i n)
          (list->string (reverse out))
          (let ((c (byte i)))
            (cond
              ((< c #x80) (loop (+ i 1) (cons (integer->char c) out)))
              ((< c #xE0) (loop (+ i 2) (cons (integer->char (fxior (fxsll (fxand c #x1F) 6) (tail (+ i 1)))) out)))
              ((< c #xF0) (loop (+ i 3) (cons (integer->char (fxior (fxsll (fxand c #x0F) 12) (fxsll (tail (+ i 1)) 6)
                                                                    (tail (+ i 2)))) out)))
              (else (loop (+ i 4) (cons (integer->char (fxior (fxsll (fxand c #x07) 18) (fxsll (tail (+ i 1)) 12)
                                                              (fxsll (tail (+ i 2)) 6) (tail (+ i 3)))) out)))))))))

;; THE TRANSPORT ERROR A LOST CHILD IS ANSWERED WITH: the one this shell uses
;; when a request's fate is unknown, with what is known in its data. Never
;; re-sent.
(define (child-lost-error-json reason diag)
  (string-append "{\"code\":-32603,\"message\":\"Core answer unavailable; execution may be unknown\""
                 ",\"data\":{\"reason\":" (json->string reason)
                 ",\"diag\":" (if diag (json->string diag) "null") "}}"))

;; NEVER: THE SERVER'S OWN WORDS, RENDERED AS TEXT. What `ensure-daemon!`
;; hands back is the refusal the server wrote to its log --
;; `(error serve-path-occupied (path ...))` and the like -- and that is
;; the whole value of this path: "it did not start" is the one sentence a
;; client can always produce and the one that helps least.
;;
;; NOTE: IT SAYS THE REQUEST DID NOT RUN, explicitly. The other -32603 in
;; this file says the opposite, and a reader has to be able to tell the
;; two apart without guessing which one they are looking at.
(define (start-failure-message answer)
  (string-append "The store's server could not be started, so the request was "
                 "not carried out. The server said: "
                 (rendered answer)))

;; NOTE: A DIFFERENT SENTENCE FOR A DIFFERENT FACT. Both mean "this did not
;; happen", and only one of them has a server's words behind it.
(define (not-sent-message answer)
  (string-append "The request was not sent and was not carried out: "
                 (rendered answer)))

(define (rendered answer)
  (call-with-string-output-port (lambda (port) (write answer port))))

;; NEVER: THE ENVELOPE CAME FROM A PEER, SO ITS SHAPE IS NOT A GIVEN. `assq`
;; demands a proper list of pairs and raises on anything else, so
;; `(answer . broken)` -- a datum that reads perfectly well -- raised out
;; of here, past the refusal this is part of, and was answered as an
;; internal error with a null id. NEVER: THE READER IS `answer-field`, IN THE
;; LIBRARY, shared with the two other programs that read this envelope.
(define (answer-stdout envelope) (answer-field envelope 'stdout string?))

;; ---- one request -------------------------------------------------------------

(define session-initialized #f)
(define session-ready #f)

;; NOTE: A JSON ARRAY IS A VECTOR HERE, and an object is an alist of
;; `(key . value)`; `null` is the symbol `null`, an empty object is `()`
;; and an empty array is `#()`. Measured, because the two empties are
;; exactly where a hand-written guess goes wrong: they are
;; distinguishable, and a check that accepted `()` for an array would
;; accept an object where a list of arguments belongs.
(define (string-vector? x)
  (and (vector? x)
       (let loop ((i 0))
         (or (= i (vector-length x))
             (and (string? (vector-ref x i)) (loop (+ i 1)))))))

(define (vector->list* x) (let loop ((i (- (vector-length x) 1)) (out '()))
                            (if (< i 0) out (loop (- i 1) (cons (vector-ref x i) out)))))

(define (object? x) (and (list? x) (let loop ((xs x)) (or (null? xs) (and (pair? (car xs)) (string? (caar xs)) (loop (cdr xs)))))))

(define (member-of obj key) (and (object? obj) (json-ref* obj key)))

(define (keys-of obj) (if (object? obj) (map car obj) '()))

(define (handle store actor socket request)
  (cond
    ((not (object? request)) (list 'error 'null -32600 "Invalid Request"))
    ((not (equal? "2.0" (member-of request "jsonrpc"))) (list 'error 'null -32600 "Invalid Request"))
    ((not (string? (member-of request "method"))) (list 'error 'null -32600 "Invalid Request"))
    (else
     (let* ((has-id (memp (lambda (e) (string=? (car e) "id")) request))
            (identity (and has-id (member-of request "id")))
            (method (member-of request "method"))
            ;; NEVER: "PRESENT AND false" IS NOT "ABSENT". `member-of`
            ;; answers #f for both, so `params:false` -- which the
            ;; protocol does not allow, params must be an object or an
            ;; array -- was read as no params at all and the call
            ;; SUCCEEDED. The key's presence is asked for separately, the
            ;; same way the id's already is.
            (has-params (memp (lambda (e) (string=? (car e) "params")) request))
            (params (if has-params (member-of request "params") '())))
       (cond
         ;; NEVER: A BOOLEAN OR AN OBJECT IS NOT AN ID. The protocol allows a
         ;; string or a number; anything else cannot be echoed back
         ;; faithfully and is refused before anything is dispatched.
         ;; NEVER: AN INTEGER-VALUED ID IS AN ID, however it was spelled.
         ;; Requiring an EXACT integer rejected `2.0` and `2e0` -- both
         ;; perfectly ordinary JSON for the number two -- and answered
         ;; with `id: null`, so a client matching replies by id could not
         ;; match its own. What must be faithful is the VALUE echoed back,
         ;; and `id-json` prints whichever form arrived.
         ((and has-id (not (or (string? identity) (number? identity))))
          (list 'error 'null -32600 "Invalid Request"))
         ;; NEVER: A NOTIFICATION IS NEVER ANSWERED -- including a
         ;; `tools/call` without an id, which is therefore never run.
         ((not has-id)
          (when (and (string=? method "notifications/initialized") session-initialized)
            (set! session-ready #t))
          (list 'silent))
         ((and has-params (not (or (object? params) (null? params) (vector? params))))
          (list 'error identity -32602 "Invalid params"))
         ((string=? method "initialize") (do-initialize identity params))
         ((string=? method "ping") (list 'result identity "{}"))
         ((not session-ready) (list 'error identity -32600 "Session is not initialized"))
         ((string=? method "tools/list")
          (if (not (null? params))
              (list 'error identity -32602 "Pagination is not available")
              ;; F100b item 8: a refusal is an ERROR -32000 "core did not
              ;; start" whose data is the refusal object; not-sent keeps its
              ;; -32603 sentence (M3 ruling Q-M3-2).
              (let ((listing (catalogue store actor socket)))
                (cond
                  ((eq? 'start-failed (car listing))
                   (list 'error-object identity (refusal-error-json (cadr listing))))
                  ((eq? 'refused (car listing))
                   (list 'error-object identity (refusal-error-json (cadr listing) '() (caddr listing))))
                  ((eq? 'not-sent (car listing))
                   (list 'error identity -32603 (not-sent-message (cadr listing))))
                  ;; The answer was lost: execution may be unknown, as before.
                  ((eq? 'transport-lost (car listing))
                   (list 'error identity -32603
                         "Core answer unavailable; execution may be unknown"))
                  ((eq? 'unavailable (car listing))
                   (list 'error-object identity (refusal-error-json '(unavailable))))
                  (else (list 'result identity (tools-json (cadr listing))))))))
         ((string=? method "tools/call") (do-call store actor socket identity params))
         (else (list 'error identity -32601 "Method not found")))))))

(define (do-initialize identity params)
  (let ((version (member-of params "protocolVersion"))
        (capabilities (member-of params "capabilities"))
        (client (member-of params "clientInfo")))
    (if (or session-initialized
            (not (string? version))
            (not (object? capabilities))
            (not (object? client))
            (not (string? (member-of client "name")))
            (not (string? (member-of client "version"))))
        (list 'error identity -32602 "Invalid initialization")
        (begin
          (set! session-initialized #t)
          (list 'result identity
                (string-append
                  "{\"protocolVersion\":" (json->string protocol-version)
                  ",\"capabilities\":{\"tools\":{}}"
                  ",\"serverInfo\":{\"name\":\"theourgia\",\"version\":\"1\"}"
                  ",\"instructions\":" (json->string (session-instructions)) "}"))))))

(define (do-call store actor socket identity params)
  (let* ((name (member-of params "name"))
         (arguments (member-of params "arguments")))
    (cond
      ((not (string? name)) (list 'error identity -32602 "Invalid tool arguments envelope"))
      ((not (object? arguments)) (list 'error identity -32602 "Invalid tool arguments envelope"))
      ;; NEVER: EXACTLY `argv`, AND EVERY ITEM A STRING. An extra property is
      ;; refused rather than ignored: a client that sent one meant
      ;; something by it.
      ;; NEVER: `argv`, AND OPTIONALLY `stdin`. Anything else is refused rather
      ;; than ignored: a client that sent one meant something by it.
      ((not (let ((ks (keys-of arguments)))
              (or (equal? '("argv") ks)
                  (equal? '("argv" "stdin") ks)
                  (equal? '("stdin" "argv") ks))))
       (list 'error identity -32602 "Invalid tool arguments envelope"))
      ((not (string-vector? (member-of arguments "argv")))
       (list 'error identity -32602 "Invalid tool arguments envelope"))
      ((and (memp (lambda (e) (string=? (car e) "stdin")) arguments)
            (not (string? (member-of arguments "stdin"))))
       (list 'error identity -32602 "Invalid tool arguments envelope"))
      (else
       (let ((listing (catalogue store actor socket)))
         (cond
           ;; NEVER: THE CATALOGUE'S OWN FAILURES, IN ITS OWN WORDS. `catalogue`
           ;; carries `start-failed` and `not-sent` out deliberately -- the
           ;; comment where it does says the reason would otherwise be lost
           ;; one layer before the place that reports it -- and this was
           ;; that place, checking only for `unavailable`. The other two
           ;; fell through to `assoc` on an error datum, which raised, and
           ;; the guard around the request answered with a null id and a
           ;; sentence naming nothing. `tools/list` has always branched on
           ;; all three; this is the same branching.
           ;; F100b item 8: THE CALL REACHED THE SHELL, so a store condition
           ;; is a tool RESULT with isError and the refusal object, never a
           ;; protocol error. not-sent keeps its -32603 sentence (M3 rulings
           ;; Q-M3-2, Q-M3-6).
           ((eq? 'start-failed (car listing))
            (list 'result identity (refusal-result-json (cadr listing))))
           ((eq? 'refused (car listing))
            (list 'result identity (refusal-result-json (cadr listing) '() (caddr listing))))
           ((eq? 'not-sent (car listing))
            (list 'error identity -32603 (not-sent-message (cadr listing))))
           ;; The catalogue's answer was lost: the TOOL was certainly not sent,
           ;; and that is what this says, as before.
           ((eq? 'transport-lost (car listing))
            (list 'error identity -32603
                  (string-append "The tool catalogue could not be read, so \""
                                 name "\" was not sent and was not carried out")))
           ;; NEVER: AND THE TOOL WAS NOT SENT. Reading the catalogue is a step
           ;; BEFORE the tool's own request, which happens further down: if
           ;; this step fails the tool never went out at all, so "execution
           ;; may be unknown" -- the sentence that used to be here -- says
           ;; the one thing that is certainly untrue. What may be unknown
           ;; is the fate of `describe`, and nobody asked for that.
           ;; The tool was still not sent: the unavailable object says the
           ;; catalogue could not be read, and its datum is `(unavailable)`.
           ((eq? 'unavailable (car listing))
            (list 'result identity (refusal-result-json '(unavailable))))
           (else
             (let ((entry (assoc name (cadr listing))))
               (if (not entry)
                   (list 'error identity -32602 "Unknown tool")
                   ;; THE SHELL DISPATCHES ON THE ROUTE THE CATALOGUE GIVES, not
                   ;; on the verb's name: `child` is run here as this shell's
                   ;; child, `daemon` is sent.
                   (let ((outcome (if (eq? 'child (cadddr entry))
                                      (child-route store (cadr entry)
                                                   (vector->list* (member-of arguments "argv"))
                                                   (member-of arguments "stdin"))
                                      (ask store actor socket (cadr entry)
                                           (vector->list* (member-of arguments "argv"))
                                           (member-of arguments "stdin")))))
                     (cond
                       ((eq? 'text (car outcome))
                        (list 'result identity (text-result-json (cadr outcome))))
                       ;; A refusal this shell made before anything ran, as the
                       ;; core would have rendered it: an ordinary result.
                       ((eq? 'result-datum (car outcome))
                        (list 'result identity (text-result-json (render-wire (cadr outcome)))))
                       ;; A child that ran and whose answer did not come back
                       ;; whole: execution may be unknown, with the reason and
                       ;; the end of its diagnostics.
                       ((eq? 'child-lost (car outcome))
                        (list 'error-object identity
                              (child-lost-error-json (cadr outcome) (caddr outcome))))
                       ;; NEVER: THE DAEMON DECLINING IS NOT THE TOOL ANSWERING.
                       ;; A draining daemon, or one that lost the process
                       ;; serving this request, refuses in the same envelope
                       ;; a core refusal arrives in. Read as a tool result
                       ;; it became `isError: false` with the refusal as its
                       ;; text -- a caller told its request had been carried
                       ;; out and answered. The envelope now says which side
                       ;; spoke, and only the core's answers are results.
                       ;; REACHED WHEN THE DAEMON DECLINES AFTER ANSWERING
                       ;; `describe`: a daemon that starts draining between
                       ;; the two refuses the tool with origin transport, and
                       ;; MC-10 measures it (M3a r1 found this reachable; the
                       ;; brief's ruling had taken it as a race). Rendered
                       ;; through the same object, with origin transport.
                       ((and (eq? 'transport-refusal (car outcome)) (refusal-datum (cadr outcome)))
                        => (lambda (d) (list 'result identity (refusal-result-json d '() 'transport))))
                       ((eq? 'transport-refusal (car outcome))
                        (list 'error identity -32603
                              (string-append "The store's server did not carry out the request: "
                                             (cadr outcome))))
                       ((eq? 'start-failed (car outcome))
                        (list 'result identity (refusal-result-json (cadr outcome))))
                       ((eq? 'not-sent (car outcome))
                        (list 'error identity -32603 (not-sent-message (cadr outcome))))
                       (else
                        (list 'error identity -32603
                              "Core answer unavailable; execution may be unknown")))))))))))))

;; ---- frames on stdio ---------------------------------------------------------
;;
;; NEVER: BYTES, NOT CHARACTERS. The limit is a byte limit, a request may
;; arrive split anywhere -- including in the middle of a UTF-8 sequence
;; -- and two requests may arrive in one write. A reader that decoded as
;; it went would break on the first split sequence.

(define in (standard-input-port))
(define out (standard-output-port))

(define (say text)
  (put-bytevector out (string->utf8 (string-append text "\n")))
  (flush-output-port out))

(define (newline-at bv from)
  (let loop ((i from))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) i)
          (else (loop (+ i 1))))))

(define (subbytes bv from to)
  (let ((o (make-bytevector (- to from))))
    (bytevector-copy! bv from o 0 (- to from))
    o))

(define (append-bytes a b)
  (let* ((na (bytevector-length a)) (nb (bytevector-length b))
         (o (make-bytevector (+ na nb))))
    (bytevector-copy! a 0 o 0 na)
    (bytevector-copy! b 0 o na nb)
    o))

(define (serve store actor socket)
  (let loop ((buffered (make-bytevector 0)))
    (let ((cut (newline-at buffered 0)))
      (cond
        (cut
         ;; NOTE: THE LIMIT COUNTS THE TERMINATOR, so a frame of exactly the
         ;; limit is legal and one byte more is not.
         (if (> (+ cut 1) frame-limit)
             (begin (say (error-frame 'null -32600 "Frame limit or incomplete frame"))
                    (exit 1))
             (let ((line (subbytes buffered 0 cut))
                   (rest (subbytes buffered (+ cut 1) (bytevector-length buffered))))
               (answer-one store actor socket line)
               (loop rest))))
        ((> (bytevector-length buffered) frame-limit)
         (say (error-frame 'null -32600 "Frame limit or incomplete frame"))
         (exit 1))
        (else
         (let ((chunk (get-bytevector-some in)))
           (if (eof-object? chunk)
               ;; NEVER: A PARTIAL FRAME AT EOF IS NOT RUN. It is a request
               ;; nobody finished sending.
               (if (> (bytevector-length buffered) 0)
                   (begin (say (error-frame 'null -32600 "Frame limit or incomplete frame"))
                          (exit 1))
                   (exit 0))
               (loop (append-bytes buffered chunk)))))))))

;; POINT 7 (F100b): A FILESYSTEM CONDITION RAISED INSIDE A REQUEST -- the
;; server's argv derived from an unreadable store, say -- is answered by the
;; table, in the shape of the request's method: the tools/call RESULT for
;; tools/call, the tools/list ERROR otherwise (E11). The record is the MCP
;; process's own, empty (D22), combined through the same renderer. A raise
;; the table does not classify keeps -32603.
(define (classified-raise e parsed)
  (let ((d (classify-failure e '())))
    (and d
         (let* ((id (and (object? parsed) (member-of parsed "id")))
                (identity (if (or (string? id) (number? id)) id 'null))
                (method (and (object? parsed) (member-of parsed "method"))))
           (if (equal? method "tools/call")
               (list 'result identity (refusal-result-json d))
               (list 'error-object identity (refusal-error-json d)))))))

(define (answer-one store actor socket line)
  (let* ((text (guard (inner (#t 'bad)) (utf8->string line)))
         (parsed (if (eq? text 'bad)
                     'bad
                     (guard (e (#t 'bad)) (string->json text)))))
    (set! id-text (and (string? text)
                       (guard (e (#t #f)) (top-level-member-text text "id"))))
    (if (eq? parsed 'bad)
        (say (error-frame 'null -32700 "Parse error"))
        (let ((answer (guard (e (#t (or (classified-raise e parsed)
                                        (list 'error 'null -32603 "Core transport unavailable"))))
                        (handle store actor socket parsed))))
          (case (car answer)
            ((silent) (if #f #f))
            ((result) (say (result-frame (cadr answer) (caddr answer))))
            ((error-object) (say (error-object-frame (cadr answer) (caddr answer))))
            (else (say (error-frame (cadr answer) (caddr answer) (cadddr answer)))))))))

;; ---- argv --------------------------------------------------------------------

;; THE SHELL NEVER SENDS writer #f. A session without a writer could not
;; write drafts unless each call carried `--writer`, and the remedy on offer -- one configured name per
;; host entry -- gave every session of that host the same writer, the case
;; in which a later draft replaces an earlier one without a word. The
;; writer is decided once, at start: THEOURGIA_WRITER when it is set and
;; not empty; else `--writer`; else one derived for this run.
;;
;; NEVER: NOT THE ACTOR ALONE. Two sessions given the same actor would share
;; a draft space, the failure a writer exists to prevent. The derived name
;; is the actor, reduced to what a writer id may hold, then this run's
;; start instant in milliseconds in base 36 and its pid: two shells alive
;; at once in one pid namespace differ by pid, and a name repeats only if a
;; pid is reused in the same millisecond of a clock that went back.
;;
;; NEVER: EVERY SUPPLIED WRITER IS CHECKED AT START, the one not chosen too.
;; An unusable name used to be refused per write, after the session had
;; started and the client had been told nothing; a bad value in either
;; place is a mistake in how the shell was started, answered where it was
;; made. An empty THEOURGIA_WRITER is unset, not bad.
(define (environment-writer)
  (let ((e (getenv "THEOURGIA_WRITER"))) (and e (> (string-length e) 0) e)))

(define session-writer #f)

(define (shell-writer) session-writer)

;; The actor as the start of a writer id: lower case, every character a
;; writer id may not hold made "-", runs of "-" made one, "-" and "." taken
;; off both ends, "agent" if nothing is left, at most 40 characters.
(define (actor-part actor)
  (let* ((mapped (map (lambda (c)
                        (let ((c (char-downcase c)))
                          (if (or (char<=? #\a c #\z) (char<=? #\0 c #\9) (memv c '(#\. #\_ #\-)))
                              c
                              #\-)))
                      (string->list actor)))
         (collapsed (let loop ((cs mapped) (out '()))
                      (cond ((null? cs) (reverse out))
                            ((and (char=? (car cs) #\-) (pair? out) (char=? (car out) #\-))
                             (loop (cdr cs) out))
                            (else (loop (cdr cs) (cons (car cs) out))))))
         (trim (lambda (cs)
                 (let loop ((cs cs))
                   (if (and (pair? cs) (memv (car cs) '(#\- #\.))) (loop (cdr cs)) cs))))
         (trimmed (reverse (trim (reverse (trim collapsed)))))
         (text (if (null? trimmed) "agent" (list->string trimmed))))
    (if (> (string-length text) 40) (substring text 0 40) text)))

(define (derived-writer actor)
  (let* ((t (current-time 'time-utc))
         (ms (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))
    (string-append (actor-part actor)
                   "-" (string-downcase (number->string ms 36))
                   "-" (number->string (get-process-id)))))

(define (environment-actor)
  (or (let ((e (getenv "THEOURGIA_ACTOR"))) (and e (> (string-length e) 0) e))
      (let ((u (getenv "USER"))) (and u (> (string-length u) 0) u))
      "cli"))

(define (usage-exit)
  (display "usage: theourgia-mcp --store <path> [--socket P] [--actor A] [--writer W]\n"
           (current-error-port))
  (exit 2))

(define (main argv)
  ;; NEVER: ONCE, BEFORE THE FIRST FRAME IS ANSWERED. The shell returns the
  ;; core's answer as text; how that text spells a non-ASCII character is
  ;; decided here, in the same place and the same way as in the CLI and
  ;; the daemon.
  (answer-printing!)
  (let ((nodes (parse-shell-arguments argv)))
    (cond
      ;; `--writer` given without its value is a mistake in how the shell was
      ;; started: the usage line, before any frame. Every other parse error
      ;; answers the frame below, as it always did.
      ((equal? nodes '(error bad-request missing-option-value "--writer"))
       (usage-exit))
      ((and (pair? nodes) (eq? (car nodes) 'error))
       (say (error-frame 'null -32600 "Invalid arguments")) (exit 2))
      (else
        (let ((store (argument-option nodes "--store"))
              (actor (or (argument-option nodes "--actor") (environment-actor)))
              (env (environment-writer))
              (opt (argument-option nodes "--writer")))
          ;; THE SUPPLIED WRITERS ARE CHECKED BEFORE ANYTHING IS LOOKED AT ON
          ;; DISK. Resolving the default socket stats the store, which can
          ;; raise; a bad writer is a mistake in how the shell was started,
          ;; and its answer is the usage line whatever the store is.
          (cond
            ((not store) (usage-exit))
            ((and env (not (working-id? env))) (usage-exit))
            ((and opt (not (working-id? opt))) (usage-exit))
            ;; The test seam for the child budget is checked where the
            ;; writers are: a value that is not a positive integer is a
            ;; mistake in how the shell was started.
            ((not (preparation-from-environment)) (usage-exit))
            (else
             (let ((socket (or (argument-option nodes "--socket")
                               ;; NEVER: THE SHARED RULE. This defaulted to
                               ;; `<store>/socket`, a third spelling of a
                               ;; path that has one function for it.
                               (socket-path store))))
               (set! session-writer (or env opt (derived-writer actor)))
               (set! serving-store store)
               (set! preparation-ms (preparation-from-environment))
               ;; NOTE: NO SCHEDULER. Reaching a daemon used to need one,
               ;; because the socket went through the actor system; the
               ;; client's calls are plain blocking reads and writes, so
               ;; the loop runs here.
               (serve store actor socket)))))))))

(main (cdr (command-line)))
