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
;; ⛔ THE JSON IS THE ENVELOPE AND NOTHING ELSE. What a tool returns is
;; the core's answer as S-expression TEXT, byte for byte -- this shell
;; does not read it, reformat it, or decide anything from it (§7.6.4,
;; §7.6.37). The one question it asks about an answer is whether the
;; TRANSPORT produced one.
;;
;; ⛔ A CORE REFUSAL IS A SUCCESSFUL RESULT. `(error unknown-id …)` is the
;; answer to the question that was asked, so it comes back as text with
;; `isError: false`. Only this shell's own failures -- a broken frame, a
;; daemon that took the request and then lost the answer -- use the
;; JSON-RPC error channel. Translating core refusals into MCP errors
;; would tell a client that its command could not be run when it ran and
;; was refused.
;;
;; ⛔ AND IT BRANCHES ON THE TRANSPORT'S TAG, NEVER ON THE ANSWER'S TEXT.
;; `exchange` answers `(answer <bytes>)` or `(transport-error <what>)`;
;; a core answer whose text happens to read `(error transport-unknown …)`
;; is still an answer. Reading the text to decide would make a payload
;; able to impersonate a transport failure.

(import (chezscheme)
        (theourgia rpc)
        (only (theourgia render) render-wire answer-printing!)
        (theourgia json)
        (theourgia arguments)
        (only (theourgia sched) start-scheduler)
        (theourgia net))

;; ---- what this shell is pinned to -------------------------------------------

(define protocol-version "2025-11-25")
(define frame-limit (* 1024 1024))
(define answer-deadline-ms 30000)

(define instructions
  (string-append
    "Tools return the unmodified core command answer as S-expression text. "
    "Core refusals are successful transport results. "
    "Eval is available only in the local CLI."))

;; ---- naming a tool ----------------------------------------------------------
;;
;; ⛔ A VERB IS NOT ALWAYS A LEGAL TOOL NAME, and the ones that are not
;; must still be reachable. A verb of plain characters keeps its
;; spelling; anything else -- and anything already starting `x_`, so the
;; escape cannot collide with a real verb -- is carried as hex. ⚠️ The
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

;; ⛔ ASKED AGAIN EVERY TIME, ⛔ never cached. The catalogue is `rpc-verbs`,
;; and a verb added to the core is a tool the shell can see -- a snapshot
;; taken at start-up, or refreshed only on `tools/list`, would answer
;; "unknown tool" for a verb that exists.
(define (catalogue)
  (let loop ((verbs (rpc-verbs)) (out '()))
    (cond
      ((null? verbs) (reverse out))
      ((tool-name (car verbs))
       => (lambda (name) (loop (cdr verbs) (cons (cons name (car verbs)) out))))
      ;; A verb whose name will not fit is left out of the catalogue
      ;; rather than breaking the whole listing.
      (else (loop (cdr verbs) out)))))

;; ---- the JSON-RPC envelope ---------------------------------------------------

(define (jstring text) text)

(define (error-frame identity code message)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (id-json identity)
                 ",\"error\":{\"code\":" (number->string code)
                 ",\"message\":" (json->string message) "}}"))

(define (result-frame identity body)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (id-json identity)
                 ",\"result\":" body "}"))

;; ⚠️ AN ID COMES BACK AS IT CAME. A string id must not turn into a
;; number, and a number must not turn into a string: a client matches
;; replies by it.
(define (id-json identity)
  (cond ((string? identity) (json->string identity))
        ((and (number? identity) (exact? identity) (integer? identity))
         (number->string identity))
        (else "null")))

(define (schema-json)
  (string-append "{\"type\":\"object\",\"properties\":{\"argv\":{\"type\":\"array\","
                 "\"items\":{\"type\":\"string\"}}},\"required\":[\"argv\"],"
                 "\"additionalProperties\":false}"))

;; ⭐ THE TWO TOOLS THAT WRITE CARRY THE PROTOCOL, and they carry the
;; core's copy of it rather than a sentence written here. An agent
;; choosing a tool from `tools/list` reads the description and nothing
;; else; if the rules for writing live in a README it will not open,
;; they are rules it will not follow.
;;
;; ⚠️ EVERY OTHER TOOL KEEPS THE PLAIN SENTENCE. The protocol is about
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
                 "{\"name\":" (json->string (caar es))
                 ",\"description\":" (json->string (description-for (cdar es)))
                 ",\"inputSchema\":" (schema-json) "}")))))
    "]}"))

(define (text-result-json text)
  (string-append "{\"content\":[{\"type\":\"text\",\"text\":" (json->string text)
                 "}],\"isError\":false}"))

;; ---- reaching the core -------------------------------------------------------
;;
;; ⛔ TWO ROUTES, ONE ANSWER. With a daemon the request goes over the
;; socket in the envelope `request-frame` packs -- the same one the
;; command line sends, from the same procedure. Without one it is
;; dispatched in this process. ⛔ No child process on either path.
(define (answer-through store actor socket verb args)
  (if (and socket (file-exists? socket))
      (let ((outcome (exchange socket
                               (request-frame store actor verb args)
                               datum-line?
                               answer-deadline-ms)))
        (cond
          ((and (pair? outcome) (eq? 'answer (car outcome)))
           (list 'text (utf8->string (cadr outcome))))
          ;; Reached nobody: this request was never run, so running it
          ;; here does the work once.
          ((transport-unreachable? outcome) (list 'text (local-answer store actor verb args)))
          ;; ⛔ CONNECTED AND THEN LOST: the request MAY have been carried
          ;; out. ⛔ It is not re-run, and the client is told the shell
          ;; could not obtain the answer -- which is what "ask me again"
          ;; means here.
          (else (list 'transport-lost))))
      (list 'text (local-answer store actor verb args))))

(define (local-answer store actor verb args)
  (render-answer (rpc-dispatch store (cons verb args) actor)))

;; ⛔ THE CORE'S PRINTER, NOT A COPY OF IT. This was a copy, with the
;; same six settings written out again -- and a copy of a printer is a
;; second printer the day one of them is edited.
(define (render-answer answer) (render-wire answer))

(define (datum-line? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))

;; ---- one request -------------------------------------------------------------

(define session-initialized #f)
(define session-ready #f)

;; ⚠️ A JSON ARRAY IS A VECTOR HERE, and an object is an alist of
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
            (params (or (member-of request "params") '())))
       (cond
         ;; ⛔ A BOOLEAN OR AN OBJECT IS NOT AN ID. The protocol allows a
         ;; string or a number; anything else cannot be echoed back
         ;; faithfully and is refused before anything is dispatched.
         ((and has-id (not (or (string? identity)
                               (and (number? identity) (exact? identity) (integer? identity)))))
          (list 'error 'null -32600 "Invalid Request"))
         ;; ⛔ A NOTIFICATION IS NEVER ANSWERED -- including a
         ;; `tools/call` without an id, which is therefore never run.
         ((not has-id)
          (when (and (string=? method "notifications/initialized") session-initialized)
            (set! session-ready #t))
          (list 'silent))
         ((not (or (object? params) (null? params)))
          (list 'error identity -32602 "Invalid params"))
         ((string=? method "initialize") (do-initialize identity params))
         ((string=? method "ping") (list 'result identity "{}"))
         ((not session-ready) (list 'error identity -32600 "Session is not initialized"))
         ((string=? method "tools/list")
          (if (not (null? params))
              (list 'error identity -32602 "Pagination is not available")
              (list 'result identity (tools-json (catalogue)))))
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
                  ",\"instructions\":" (json->string instructions) "}"))))))

(define (do-call store actor socket identity params)
  (let* ((name (member-of params "name"))
         (arguments (member-of params "arguments")))
    (cond
      ((not (string? name)) (list 'error identity -32602 "Invalid tool arguments envelope"))
      ((not (object? arguments)) (list 'error identity -32602 "Invalid tool arguments envelope"))
      ;; ⛔ EXACTLY `argv`, AND EVERY ITEM A STRING. An extra property is
      ;; refused rather than ignored: a client that sent one meant
      ;; something by it.
      ((not (equal? '("argv") (keys-of arguments)))
       (list 'error identity -32602 "Invalid tool arguments envelope"))
      ((not (string-vector? (member-of arguments "argv")))
       (list 'error identity -32602 "Invalid tool arguments envelope"))
      (else
       (let ((entry (assoc name (catalogue))))
         (if (not entry)
             (list 'error identity -32602 "Unknown tool")
             (let ((outcome (answer-through store actor socket (cdr entry)
                                            (vector->list* (member-of arguments "argv")))))
               (if (eq? 'text (car outcome))
                   (list 'result identity (text-result-json (cadr outcome)))
                   (list 'error identity -32603
                         "Core answer unavailable; execution may be unknown")))))))))

;; ---- frames on stdio ---------------------------------------------------------
;;
;; ⛔ BYTES, NOT CHARACTERS. The limit is a byte limit, a request may
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
         ;; ⚠️ THE LIMIT COUNTS THE TERMINATOR, so a frame of exactly the
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
               ;; ⛔ A PARTIAL FRAME AT EOF IS NOT RUN. It is a request
               ;; nobody finished sending.
               (if (> (bytevector-length buffered) 0)
                   (begin (say (error-frame 'null -32600 "Frame limit or incomplete frame"))
                          (exit 1))
                   (exit 0))
               (loop (append-bytes buffered chunk)))))))))

(define (answer-one store actor socket line)
  (let ((parsed (guard (e (#t 'bad))
                  (let ((text (guard (inner (#t 'bad)) (utf8->string line))))
                    (if (eq? text 'bad) 'bad (string->json text))))))
    (if (eq? parsed 'bad)
        (say (error-frame 'null -32700 "Parse error"))
        (let ((answer (guard (e (#t (list 'error 'null -32603 "Core transport unavailable")))
                        (handle store actor socket parsed))))
          (case (car answer)
            ((silent) (if #f #f))
            ((result) (say (result-frame (cadr answer) (caddr answer))))
            (else (say (error-frame (cadr answer) (caddr answer) (cadddr answer)))))))))

;; ---- argv --------------------------------------------------------------------

(define (environment-actor)
  (or (let ((e (getenv "THEOURGIA_ACTOR"))) (and e (> (string-length e) 0) e))
      (let ((u (getenv "USER"))) (and u (> (string-length u) 0) u))
      "cli"))

(define (main argv)
  ;; ⛔ ONCE, BEFORE THE FIRST FRAME IS ANSWERED. The shell returns the
  ;; core's answer as text; how that text spells a non-ASCII character is
  ;; decided here, in the same place and the same way as in the CLI and
  ;; the daemon.
  (answer-printing!)
  (let ((nodes (parse-arguments 'serve argv)))
    (if (and (pair? nodes) (eq? (car nodes) 'error))
        (begin (say (error-frame 'null -32600 "Invalid arguments")) (exit 2))
        (let* ((store (argument-option nodes "--store"))
               (actor (or (argument-option nodes "--actor") (environment-actor)))
               (socket (or (argument-option nodes "--socket")
                           (and store (string-append store "/socket")))))
          (if (not store)
              (begin (display "usage: theourgia-mcp --store <path> [--socket P] [--actor A]\n"
                              (current-error-port))
                     (exit 2))
              ;; ⚠️ EVERYTHING RUNS INSIDE THE SCHEDULER, because reaching
              ;; a daemon needs it -- and `start-scheduler` never returns,
              ;; so there is no "afterwards" to run the loop in.
              (start-scheduler (lambda () (serve store actor socket))))))))

(main (cdr (command-line)))
