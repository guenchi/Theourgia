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

;; The request envelope: what a client says about itself, and what the
;; daemon refuses to read.
;;
;;   (request <version> <store> <actor> <writer> <mode> <cwd> <stdin>
;;            <verb> <args> ...)
;;
;; KEY: THE WRITER IS THE POINT OF THE VERSION BUMP. Before it, a draft
;; verb with no `--writer` fell back to the store's own local log writer,
;; so two agents that never passed one shared a draft space and neither
;; was told. The envelope carries the identity the CLIENT PROCESS is
;; bound to; a `--writer` on a single call still wins over it, which is
;; how one agent copies another's draft into its own space.
;;
;; NEVER: FIVE OF THE FIELDS ARE STRINGS NEXT TO EACH OTHER -- store, actor,
;; writer, cwd, stdin -- so the rows here are written to tell them apart
;; by their EFFECT and not by their position. A frame built with two of
;; them swapped parses and dispatches; what it does is somebody else's
;; work under somebody else's name.

(import (chezscheme)
        (theourgia client)
        (theourgia rpc)
        (theourgia ffi))

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

(define here (string-append scratch-base "/envelope-" (number->string (get-process-id))))
(define sock-here (string-append socket-base "/envelope-" (number->string (get-process-id))))
(system (string-append "rm -rf " here " " sock-here "; mkdir -p " here "/store " here "/home " sock-here))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(define store (string-append here "/store"))
(define sock (string-append sock-here "/s.sock"))
(define cli "../cli.sc")

(rpc-dispatch store '(init) "test")
(define block
  (let* ((a (rpc-dispatch store '(insert "--title" "B" "--text" "old") "test"))
         (ev (car (cadr (assq 'events (cdr a))))))
    (string-append (car ev) "." (number->string (cdr ev)))))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                       "THEOURGIA_HOME=" here "/home "
                       "scheme --script " cli " serve " store " --socket " sock
                       " > " here "/serve.log 2>&1 &"))
(let wait ((i 0))
  (cond ((file-exists? sock) 'up)
        ((> i 300) 'never)
        (else (system "sleep 0.05") (wait (+ i 1)))))

;; NEVER: TWO LAYERS, AND BOTH ARE READ HERE. What comes back is the ANSWER
;; ENVELOPE; the core's answer is the text inside its `stdout`. A helper
;; that read only the outer datum would make every row below about the
;; envelope's shape and none of them about the answer.
(define (raw-answer frame)
  (let ((r (call! sock frame 5000)))
    (if (and (pair? r) (eq? 'answer (car r)))
        (guard (e (#t 'unreadable))
          (read (open-string-input-port (utf8->string (cadr r)))))
        r)))

(define (envelope-part envelope name)
  (let ((hit (and (pair? envelope) (eq? 'answer (car envelope))
                  (assq name (cdr envelope)))))
    (and (pair? hit) (pair? (cdr hit)) (cadr hit))))

(define (ask frame)
  (let* ((envelope (raw-answer frame))
         (out (envelope-part envelope 'stdout)))
    (if (string? out)
        (guard (e (#t 'unreadable)) (read (open-string-input-port out)))
        (list 'NOT-AN-ENVELOPE envelope))))

(define (send-verb verb args fields) (ask (request-frame store verb args fields)))

;; NEVER: A HAND-BUILT FRAME, so the rows about malformed envelopes can
;; actually build one. `request-frame` refuses to make these, which is
;; correct of it and useless here.
(define (raw-frame text) (string->utf8 (string-append text "\n")))

;; NEVER: SOME ROWS NEED BYTES THAT ARE NOT TEXT AT ALL, so the frame is built
;; from bytevectors rather than from a string: a string cannot hold an
;; invalid UTF-8 sequence, which is exactly what one row has to send.
(define (string-append-bytes . parts)
  (let* ((n (apply + (map bytevector-length parts)))
         (out (make-bytevector n)))
    (let place ((ps parts) (at 0))
      (if (null? ps)
          out
          (let ((k (bytevector-length (car ps))))
            (bytevector-copy! (car ps) 0 out at k)
            (place (cdr ps) (+ at k)))))))

(define (kind answer)
  (if (and (pair? answer) (pair? (cdr answer))) (list (car answer) (cadr answer)) answer))

;; ---- EV-1 the writer the envelope carries ---------------------------------

(want "EV-1 a draft verb with no writer anywhere is refused"
      (kind (send-verb 'write (list block "draft-a") '()))
      '(error writer-required))

(want "EV-1 the envelope's writer is used when the call names none"
      (let ((a (send-verb 'write (list block "draft-a") (list (cons 'writer "w1")))))
        (and (pair? a) (eq? 'ok (car a)) (cadr (assq 'writer (cdr a)))))
      "w1")

;; KEY: THE SCOPING IS THE PROPERTY, not the acceptance. An implementation
;; that took the envelope's writer and then wrote everything into one
;; space passes the row above and fails these two.
(want "EV-1 and the draft is in that writer's space"
      (let ((a (send-verb 'drafts '() (list (cons 'writer "w1")))))
        (if (and (pair? a) (eq? 'ok (car a))) (length (cdr (cadr a))) a))
      1)

(want "EV-1 TWIN: and not in another writer's"
      (let ((a (send-verb 'drafts '() (list (cons 'writer "w2")))))
        (if (and (pair? a) (eq? 'ok (car a))) (length (cdr (cadr a))) a))
      0)

;; NEVER: AN EXPLICIT --writer STILL WINS, which is what makes two agents
;; able to work on one block at once: the second copies the first's
;; version into its own space by naming it.
(want "EV-1 an explicit --writer overrides the envelope's"
      (let ((a (send-verb 'write (list block "draft-b" "--writer" "w2")
                          (list (cons 'writer "w1")))))
        (and (pair? a) (eq? 'ok (car a)) (cadr (assq 'writer (cdr a)))))
      "w2")

(want "EV-1 and it landed in the writer it named"
      (let ((a (send-verb 'drafts '() (list (cons 'writer "w2")))))
        (if (and (pair? a) (eq? 'ok (car a))) (length (cdr (cadr a))) a))
      1)

;; ---- EV-5 binding a writer changes nothing for a verb that has none -------
;;
;; KEY: GENERATED FROM THE CATALOGUE, so a verb added later is in this cell
;; without anyone remembering to add it. The list is the product's own.
;;
;; NEVER: THE DEFECT THIS EXISTS FOR WAS A P1 AND EVERY ROW IN THIS FILE WAS
;; GREEN THROUGH IT. The envelope's writer was injected into EVERY
;; request; for a verb whose grammar has no `--writer` the two tokens
;; became positionals, the arity check failed, and the answer was that
;; verb's usage line. So binding an identity broke `outline`, `read` and
;; `describe` -- and the MCP shell asks `describe` for its tool list
;; before it can do anything, so the shell died at step one. EV-1 missed
;; it by only ever sending a bound writer with DRAFT verbs, which are
;; exactly the verbs that accept the option: the half of the axis where
;; it works.
;;
;; NEVER: THE RULE HERE DOES NOT CONSULT THE OPTION TABLE, because the fix
;; does. A cell that asked the same table would be checking the
;; implementation against its own source. It asks behaviour instead:
;;
;;   unbound answer refuses for want of a writer -> bound must not
;;   otherwise                                   -> the two are identical
;;
;; NOTE: WHAT THIS DOES *NOT* COVER. Sent with no arguments, `write`,
;; `restore`, `discard` and `read` fail their arity check BEFORE the
;; writer is looked at, so for them this loop proves only that the
;; injection is harmless -- not that the writer arrives. EV-1 covers that
;; for those verbs, with arguments. Said here because "32 verbs checked"
;; would otherwise read as more than it is.

(define (answer-for-verb verb args w)
  (let ((r (call! sock (request-frame store verb args (list (cons 'writer w))) 5000)))
    (if (and (pair? r) (eq? 'answer (car r)))
        (utf8->string (cadr r))
        (list 'transport r))))

(define (mentions? text needle)
  (and (string? text)
       (let ((n (string-length needle)) (m (string-length text)))
         (let loop ((i 0))
           (cond ((> (+ i n) m) #f)
                 ((string=? (substring text i (+ i n)) needle) #t)
                 (else (loop (+ i 1))))))))

;; NOTE: ONE VERB IS NOT IDEMPOTENT AND IS NAMED RATHER THAN DROPPED.
;; `snapshot` writes a snapshot, so a second identical call answers with a
;; different path -- a difference that has nothing to do with the writer.
;; It is sent WITH an argument instead, which makes it a deterministic
;; refusal and still exercises the parse the defect lived in.
(define non-idempotent '(snapshot))

(define (probe-args verb) (if (memq verb non-idempotent) '("x") '()))

(define writer-axis
  (map (lambda (entry)
         (let* ((verb (car entry))
                (args (probe-args verb))
                (unbound (answer-for-verb verb args #f))
                (bound (answer-for-verb verb args "w1")))
           (list verb unbound bound)))
       (verb-catalogue)))

(want "EV-5 the catalogue was walked, and it is not empty"
      (if (> (length writer-axis) 20) 'walked (list 'verbs (length writer-axis)))
      'walked)

(want "EV-5 binding a writer changes no answer for a verb that does not take one"
      (let loop ((xs writer-axis) (bad '()))
        (cond
          ((null? xs) (reverse bad))
          (else
           (let* ((row (car xs)) (verb (car row)) (unbound (cadr row)) (bound (caddr row)))
             (cond
               ;; a draft verb that got as far as its writer check
               ((mentions? unbound "writer-required") (loop (cdr xs) bad))
               ((equal? unbound bound) (loop (cdr xs) bad))
               (else (loop (cdr xs) (cons (list verb 'unbound unbound 'bound bound) bad))))))))
      '())

;; NEVER: AND THE OTHER HALF: a verb that refused for want of a writer must
;; stop refusing once one is bound. Without this row the one above is
;; passed by a build that ignores the envelope's writer entirely.
(want "EV-5 a verb that refused for want of a writer accepts the bound one"
      (let loop ((xs writer-axis) (seen 0) (bad '()))
        (cond
          ((null? xs)
           (if (zero? seen) (list 'NO-DRAFT-VERB-REACHED-ITS-WRITER-CHECK) (reverse bad)))
          (else
           (let* ((row (car xs)) (verb (car row)) (unbound (cadr row)) (bound (caddr row)))
             (if (not (mentions? unbound "writer-required"))
                 (loop (cdr xs) seen bad)
                 ;; NEVER: "NO LONGER writer-required" IS NOT "ACCEPTED". A
                 ;; build where binding a writer merely changed one
                 ;; refusal into another would pass that. The answer has
                 ;; to stop being an error at all.
                 (loop (cdr xs) (+ seen 1)
                       (cond
                         ((mentions? bound "writer-required")
                          (cons (list verb 'still-refused bound) bad))
                         ((mentions? bound "(error ")
                          (cons (list verb 'refused-for-another-reason bound) bad))
                         (else bad))))))))
      '())

;; ---- EV-9 the request's own text is not searched for options --------------
;;
;; NEVER: MEASURED DEFECT. The envelope's writer used to be spliced into the
;; argument list unless a string search over unparsed argv found
;; `--writer` already present. The search could not tell an option from a
;; caller's own words, so a request that merely CONTAINED the text
;; `--writer` -- here, as the bytes being written -- suppressed its own
;; default and was refused `writer-required`. A legitimate write became
;; one that could not be made at all.
;;
;; KEY: THE BYTES ARE THE OPTION'S OWN SPELLING, after `--`, which is the
;; one place a caller can write anything at all. A build that goes back
;; to searching argv fails the first row; a build that searches and then
;; injects anyway fails the second, because the stored text would carry
;; the two extra tokens.
(want "EV-9 a write whose bytes are the literal --writer is served"
      (car (send-verb 'write (list block "--" "--writer") '((writer . "w1"))))
      'ok)

(want "EV-9 and the bytes stored are exactly what was sent"
      (send-verb 'read (list block "--working") '((writer . "w1")))
      '(ok (text "--writer")))

;; NEVER: THE OTHER HALF: an explicit option still wins over the envelope.
;; Without this row the two above are passed by a build that has stopped
;; reading `--writer` from the arguments at all.
(want "EV-9 TWIN: an explicit --writer still names the writer"
      (car (send-verb 'drafts '("--writer" "w2") '((writer . "w1"))))
      'ok)

;; ---- EV-2 what the daemon will not read -----------------------------------
;;
;; KEY: EACH OF THESE IS A FRAME THAT WOULD HAVE BEEN EXECUTED by a reader
;; that checked position and not content. They answer a refusal rather
;; than doing the work, and `bad-request` rather than a verb's own
;; complaint -- the difference between "I did not understand you" and "I
;; understood you and the answer is no".

(want "EV-2 an envelope from a later version is not read as this one"
      (kind (ask (raw-frame (string-append
                              "(request 2 \"" store "\" \"tester\" #f wire #f #f outline)"))))
      '(error bad-request))

(want "EV-2 TWIN: and version 1 of the same frame is served"
      (car (ask (raw-frame (string-append
                             "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)"))))
      'ok)

(want "EV-2 a mode that is neither wire nor human is refused"
      (kind (ask (raw-frame (string-append
                              "(request 1 \"" store "\" \"tester\" #f shouty #f #f outline)"))))
      '(error bad-request))

;; NOTE: THE FIELD IS A STRING OR #f AND NOTHING ELSE. A number there would
;; have become a writer id on the way in.
(want "EV-2 a writer that is not a string is refused"
      (kind (ask (raw-frame (string-append
                              "(request 1 \"" store "\" \"tester\" 7 wire #f #f outline)"))))
      '(error bad-request))

(want "EV-2 an envelope with fields missing is refused"
      (kind (ask (raw-frame (string-append "(request 1 \"" store "\" \"tester\" outline)"))))
      '(error bad-request))

;; NEVER: A SECOND DATUM ON THE LINE IS NOT A SECOND REQUEST. Executing the
;; first and discarding the rest is the worst of the options: the caller
;; is answered, and never learns that half of what it sent was dropped.
(want "EV-2 anything after the frame refuses the whole frame"
      (kind (ask (raw-frame (string-append
                              "(request 1 \"" store "\" \"tester\" #f wire #f #f outline)"
                              " (request 1 \"" store "\" \"tester\" #f wire #f #f log)"))))
      '(error bad-request))

;; ---- EV-6 what the refusal is called ---------------------------------------
;;
;; NEVER: A REFUSAL'S NAME HAS TO SAY SOMETHING TRUE. Both of these used to
;; answer `not-a-datum`, and for a frame that IS a datum -- one whose
;; fields are simply not an envelope -- that is untrue, and it sends the
;; reader looking at their encoding when the problem is their fields.

;; NEVER: AN ANSWER IS NOT ALWAYS THE ANSWER THIS ROW EXPECTED, and `assq`
;; raises on anything that is not a proper list of pairs. Asked of
;; `(error bad-request duplicate-option "--writer")` -- a perfectly
;; ordinary refusal whose tail is not an alist -- it took the fixture down
;; and reported the FIXTURE's failure where the product's answer belonged.
(define (reason-of answer)
  (let look ((xs (and (pair? answer) (pair? (cdr answer)) (cddr answer))))
    (cond ((not (pair? xs)) #f)
          ((and (pair? (car xs)) (eq? 'reason (caar xs)) (pair? (cdar xs)))
           (cadr (car xs)))
          (else (look (cdr xs))))))

;; The refusal's own kind, for answers that name it directly rather than
;; through a `reason` field.
(define (kind-of answer)
  (and (pair? answer) (eq? 'error (car answer))
       (pair? (cdr answer)) (pair? (cddr answer))
       (caddr answer)))

(want "EV-6 a datum whose fields are not an envelope says so"
      (reason-of (ask (raw-frame (string-append "(request 1 \"" store "\" \"t\" #f wire #f #f read 42)"))))
      'malformed-envelope)

(want "EV-6 and so does a frame with something after it"
      (reason-of (ask (raw-frame (string-append
                                   "(request 1 \"" store "\" \"t\" #f wire #f #f outline) (x)"))))
      'malformed-envelope)

;; NEVER: THE TWIN, or the row above is satisfied by calling everything
;; `malformed-envelope`. Bytes that genuinely will not read as a datum
;; keep the older name, because for them it is the true one.
(want "EV-6 TWIN: bytes that are not a datum keep the name that fits them"
      (reason-of (ask (raw-frame "(((( not a datum")))
      'not-a-datum)

;; ---- EV-13 the exit code for answers that are neither ok nor error --------
;;
;; NEVER: THE SERVER DECIDES THE EXIT CODE, and these two answers are why it
;; must. A `batch` reports one result per intent, and its outer symbol is
;; `batch` whichever way it went: whether the request succeeded is a fact
;; about the items INSIDE it, which `rpc-ok?` knows and a client reading
;; the outer symbol cannot. The other rows here use an ordinary `(error
;; ...)`, which any first-symbol rule gets right.
;;
;; NOTE: THE ANSWER IS READ, NOT SEARCHED. Written as "the text mentions
;; `(batch`", this passed on `(usage (batch <intents>))` -- a failure
;; whose usage form contains the same characters -- so both rows were
;; green for the wrong reason until the twin disagreed with them. What is
;; asked now is the datum's own head, and the envelope's exit field.
(define (batch-outcome intents)
  (let* ((envelope (raw-answer (request-frame store 'batch '()
                                              (list (cons 'actor "t")
                                                    (cons 'stdin intents)))))
         (out (envelope-part envelope 'stdout))
         (datum (and (string? out)
                     (guard (e (#t #f)) (read (open-string-input-port out))))))
    (list (envelope-part envelope 'exit)
          (if (and (pair? datum) (eq? 'batch (car datum)))
              'a-batch-answer
              (list 'head (and (pair? datum) (car datum)))))))

(want "EV-13 a batch with one failed item exits non-zero"
      (batch-outcome "((insert root #f ((kind . section) (title . \"EV13-A\"))) (del \"nosuch.1\"))")
      '(1 a-batch-answer))

;; NEVER: THE TWIN, and it is what makes the row above about the ITEMS. A
;; batch whose every item succeeded has the same outer shape and must
;; exit 0 -- so "exit 1 whenever the answer is a batch" fails here, and
;; so does a build that cannot run a batch at all.
(want "EV-13 TWIN: a batch whose items all succeeded exits zero"
      (batch-outcome "((insert root #f ((kind . section) (title . \"EV13-B\"))))")
      '(0 a-batch-answer))

;; ---- EV-12 a suffix that used to be read as "nothing follows" -------------
;;
;; NEVER: THE TRAILING-DATA CHECK ASKS THE PORT, NOT THE READ, and the case
;; that forced that is this one: asked as "did the next read return an
;; end-of-file object", a frame whose suffix begins with the literal
;; token `#!eof` answered yes -- because a datum CAN BE an eof object --
;; so the first form was executed and the rest discarded. The existing
;; row sends an ordinary datum as the suffix, which BOTH implementations
;; refuse; it cannot tell them apart.
;;
;; NOTE: AND THE FIRST FORM IS A WRITE, so "it was not executed" is a fact
;; about the store rather than about the wording of the answer. A row
;; that only read the refusal would pass on a build that refused AND ran
;; it.
(let* ((canary "EV12-CANARY")
       (frame (string-append "(request 1 \"" store "\" \"t\" \"w1\" wire #f #f write \""
                             block "\" \"" canary "\") #!eof (x)"))
       (answer (ask (raw-frame frame))))
  (want "EV-12 a frame with #!eof after it is refused"
        (reason-of answer)
        'not-a-datum)

  (want "EV-12 and the write in front of it did not happen"
        (let ((back (send-verb 'read (list block "--working") '((writer . "w1")))))
          (if (and (pair? back) (eq? 'ok (car back))
                   (mentions? (call-with-string-output-port
                                (lambda (port) (write back port)))
                              canary))
              'IT-RAN
              'did-not-run))
        'did-not-run)

  ;; NEVER: THE TWIN: the same write, with nothing after it, must succeed --
  ;; or the row above passes on a build that cannot write at all.
  (want "EV-12 TWIN: the same write on its own is served"
        (car (send-verb 'write (list block canary) '((writer . "w1"))))
        'ok))

;; ---- EV-16 well-formed is judged before whose store it is ----------------
;;
;; NEVER: FOUR LAYERS, IN ORDER (§7.6.50): well-formed, then whose store, then
;; premises, then execution. Judged the other way round, a request whose
;; arguments do not parse and which names another store was answered
;; `transport-store-mismatch` -- which sends the caller off to find the
;; daemon that serves that store, for a request no daemon will accept.
;;
;; NOTE: THE REQUEST IS MALFORMED IN A WAY THE PARSER OWNS (a repeated
;; `--writer`, which `drafts` does not take twice) AND names a store this
;; daemon does not serve. Only the order decides which answer comes back.
(want "EV-16 a request that does not parse is told so, whatever store it names"
      (kind-of (ask (raw-frame (string-append
                                 "(request 1 \"" store "-elsewhere\" \"t\" #f wire #f #f "
                                 "drafts \"--writer\" \"w1\" \"--writer\" \"w2\")"))))
      'duplicate-option)

;; NEVER: THE TWIN: a request that DOES parse and names another store is still
;; refused for the store. Without it the row above passes on a daemon that
;; has stopped checking which store it serves.
(want "EV-16 TWIN: a well-formed request for another store is refused for that"
      (let ((a (ask (raw-frame (string-append
                                 "(request 1 \"" store "-elsewhere\" \"t\" #f wire #f #f outline)")))))
        (if (and (pair? a) (eq? 'error (car a))) (cadr a) (list 'said a)))
      'transport-store-mismatch)

;; ---- EV-14 the two lexical shapes the reader must not be handed ----------
;;
;; NEVER: ONE RULE, BOTH DIRECTIONS. A request and a reply go through the same
;; `read`, so what it must not be handed is one fact: the check lives in
;; the client library beside the packer, and the daemon asks it of a
;; request exactly as the three clients ask it of a reply.
;;
;; `|...|` is refused because inside it EVERY character is literal --
;; including the quote this scan uses to know where a string ends -- so a
;; scan that tried to carry on through one would read the rest of the text
;; in the wrong mode and could be walked past the guard entirely.
(want "EV-14 a request carrying a bar-quoted symbol is not a datum"
      (reason-of (ask (raw-frame (string-append
                                   "(request 1 \"" store "\" \"t\" #f wire #f #f |a b|)"))))
      'not-a-datum)

(want "EV-14 and a bar inside a string is a caller's text, not a shape"
      (car (send-verb 'write (list block "a|b") '((writer . "w1"))))
      'ok)

(want "EV-14 TWIN: and those bytes are stored exactly"
      (send-verb 'read (list block "--working") '((writer . "w1")))
      '(ok (text "a|b")))

;; ---- EV-15 a verb that reads standard input and was given none -----------
;;
;; NEVER: MEASURED DEFECT, AND IT WAS SILENT. `write <id> -` takes its bytes
;; from standard input. With none carried in the message the placeholder
;; stayed in the arguments, so the literal "-" was stored as the block's
;; text and the answer was `(ok (saved ...))`. The caller's bytes were
;; discarded and nothing anywhere said so.
(want "EV-15 a write whose bytes were not sent is refused, not invented"
      (let ((a (send-verb 'write (list block "-") '((writer . "w1")))))
        (list (car a) (cadr a) (caddr a)))
      '(error bad-request stdin-required))

(want "EV-15 TWIN: and the block still holds what it held"
      (send-verb 'read (list block "--working") '((writer . "w1")))
      '(ok (text "a|b")))

;; NEVER: THE OTHER HALF: with the bytes carried, the same call writes them.
(want "EV-15 TWIN: the same write with its bytes carried is served"
      (car (send-verb 'write (list block "-")
                      '((writer . "w1") (stdin . "BYTES-FROM-THE-ENVELOPE"))))
      'ok)

(want "EV-15 TWIN: and what it stored is what was carried"
      (send-verb 'read (list block "--working") '((writer . "w1")))
      '(ok (text "BYTES-FROM-THE-ENVELOPE")))


;; NEVER: AND A VERB THAT DOES NOT READ INPUT IGNORES INPUT THAT CAME ANYWAY.
;; A caller can put bytes in the envelope for any verb; our own client
;; only does it when the argument form asks, but a wrapper that forwards
;; whatever it was given is an ordinary way to write a client, and every
;; verb it sent would fail if unread input were refused.
;;
;; KEY: PINNED AS A CHOICE, NOT DISCOVERED AS A BEHAVIOUR. The refusal for
;; input that was ASKED for and did not come is EV-15 above; this row is
;; the other side of that line, and it is written down so that the next
;; change has to mean to cross it.
;;
;; NOTE: AND THE READING IS BYTE IDENTITY, not "it did not fail". An answer
;; that merely started with `ok` would be produced by a build that had
;; quietly let the input change what was read.
(want "EV-15 input a verb does not read changes nothing about its answer"
      (let ((with (send-verb 'read (list block "--working")
                             '((writer . "w1") (stdin . "INPUT-NOBODY-READS"))))
            (without (send-verb 'read (list block "--working") '((writer . "w1")))))
        (list (if (and (pair? with) (eq? 'ok (car with))) 'served (list 'said with))
              (if (equal? with without) 'the-same-answer (list 'differ with without))))
      '(served the-same-answer))

;; NEVER: AND THE PLACEHOLDER IS THE BYTES, NOT THE NAME. Written as "every
;; positional spelled `-`", a block whose id is literally `-` had its NAME
;; replaced by what was piped in, so the write went somewhere nobody asked
;; for -- and that is neither what was asked nor an error.
(want "EV-15 a block named `-` keeps its name when the bytes come from stdin"
      (let ((a (send-verb 'write (list "-" "-")
                          '((writer . "w1") (stdin . "x")))))
        (if (and (pair? a) (eq? 'error (car a))) (cadr a) (list 'said a)))
      'bad-request)

;; ---- EV-11 the limit's own boundary ---------------------------------------
;;
;; NEVER: EV-8 SENDS ONE FRAME FAR OVER AND ONE ORDINARY FRAME, which is a
;; pair that any threshold anywhere between them satisfies -- including
;; one off by a megabyte, and including one that measures the wrong
;; thing. The boundary is where a limit is either right or wrong, so it
;; is the boundary that is sent: exactly the limit, and one byte past it.
;;
;; NOTE: THE PADDING GOES IN THE ACTOR, which is an arbitrary string the
;; daemon carries and does not interpret, so the frame stays a perfectly
;; ordinary request whose only remarkable property is its length.
(define frame-ceiling (* 1024 1024))

(define (frame-of-length n)
  (let* ((head (string-append "(request 1 \"" store "\" \""))
         (tail "\" #f wire #f #f outline)")
         (pad (- n (string-length head) (string-length tail))))
    (and (> pad 0)
         (string-append head (make-string pad #\a) tail))))

(want "EV-11 a frame of exactly the limit is served"
      (car (ask (raw-frame (frame-of-length frame-ceiling))))
      'ok)

(want "EV-11 and one byte more is refused"
      (reason-of (ask (raw-frame (frame-of-length (+ frame-ceiling 1)))))
      'frame-limit)


;; ---- EV-10 what the reader is asked to build -------------------------------
;;
;; NEVER: MEASURED: the frame limit counts BYTES, and that is not this
;; question. `#e1e100000` is eleven characters asking `read` for a
;; 332193-bit integer, and it is built before any check can look at the
;; result -- a 50-byte frame, well inside every limit, spending the
;; daemon's memory and time on its way to being called malformed. The
;; exponent is the attacker's to choose.
;;
;; KEY: THE BOUND COMES FROM A MEASUREMENT, not from a guess: building this
;; one takes 756 ms in this reader (1e5 -> 3.3 ms, 1e6 -> 69 ms,
;; 5e6 -> 756 ms), so a build that still reads it cannot answer inside
;; the budget below, and one that refuses by looking at characters
;; answers in the time of a round trip.
(define (ms-for thunk)
  (let* ((a (current-time 'time-monotonic))
         (ignored (thunk))
         (b (current-time 'time-monotonic)))
    (+ (* 1000 (- (time-second b) (time-second a)))
       (/ (- (time-nanosecond b) (time-nanosecond a)) 1000000.0))))

(define allocating-frame
  (raw-frame (string-append "(request 1 \"" store "\" \"t\" #f wire #f #f outline #e1e5000000)")))

(define allocating-ms (ms-for (lambda () (ask allocating-frame))))

(want "EV-10 a literal that would be built before it is judged is not a datum"
      (reason-of (ask allocating-frame))
      'not-a-datum)

(want "EV-10 and it is refused without building it"
      (if (< allocating-ms 500) 'prompt (list 'ms allocating-ms))
      'prompt)

(want "EV-10 an empty frame is not a datum"
      (reason-of (ask (raw-frame "")))
      'not-a-datum)

(want "EV-10 a frame of nothing but spaces is not a datum"
      (reason-of (ask (raw-frame "   ")))
      'not-a-datum)

;; NEVER: THE TWIN, AND IT IS THE WHOLE POINT OF SCANNING RATHER THAN
;; FORBIDDING. A `#` inside a string is a caller's own text and must be
;; served: this writes the very characters the row above refuses. Without
;; it, "refuse every #" passes everything above.
;;
;; NOTE: The other half -- that `#f` outside a string still reads -- is
;; asserted by every other row in this file: an envelope carries `#f` in
;; three of its fields, so a build that refused them would fail all of
;; them.
(want "EV-10 TWIN: the same characters inside a string are a caller's text"
      (car (send-verb 'write (list block "#e1e5000000") '((writer . "w1"))))
      'ok)

(want "EV-10 TWIN: and they are stored exactly"
      (send-verb 'read (list block "--working") '((writer . "w1")))
      '(ok (text "#e1e5000000")))

;; ---- EV-7 arguments, and bytes that are not text ---------------------------
;;
;; KEY: THE READER IS SYMMETRIC WITH THE PACKER. `request-frame` refuses to
;; build a frame whose arguments are not all strings; the reader accepted
;; one, so EV-3's claim held in one direction only.
;;
;; NOTE: IT WAS NOT REACHING ANYTHING DANGEROUS -- measured, the dispatcher
;; refuses a non-string argument itself and the daemon goes on serving.
;; This row asserts the answer that actually comes back, NEVER: not the
;; predicted crash: a row written to a symptom that does not occur is red
;; for a reason nobody can find.
(want "EV-7 an argument that is not a string is refused by the reader"
      (kind (ask (raw-frame (string-append "(request 1 \"" store "\" \"t\" #f wire #f #f read 42)"))))
      '(error bad-request))

(want "EV-7 TWIN: and the daemon is still serving afterwards"
      (car (ask (request-frame store 'outline '() '())))
      'ok)

;; NEVER: BYTES THAT ARE NOT UTF-8 ARE REFUSED, NOT REPAIRED. `utf8->string`
;; substitutes U+FFFD, so a frame whose actor field held invalid bytes was
;; decoded into a DIFFERENT actor and the request then ran under it.
;; Measured before the fix: the verb executed and answered `(ok ...)`.
(define not-utf8-frame
  (string-append-bytes
    (string->utf8 (string-append "(request 1 \"" store "\" \"a"))
    (bytevector 255 254)
    (string->utf8 "b\" #f wire #f #f outline)\n")))

(want "EV-7 a frame that is not valid UTF-8 is refused, not repaired"
      (kind (ask not-utf8-frame))
      '(error bad-request))

;; NEVER: AND IT IS `not-a-datum`, THE SAME REASON AS SYNTAX THAT WILL NOT
;; READ. The grouping is deliberate and is written down at the refusal
;; (`daemon.sc`, above `parse-frame`): the vocabulary separates "no datum
;; came out of these bytes" from "a datum came out and its fields are not
;; an envelope", which is the distinction a caller can act on. Bytes that
;; will not decode and bytes that will not parse are the same fact on
;; that axis, and the caller does the same thing about both.
;;
;; NOTE: THE ROW EXISTS BECAUSE THE CHOICE WAS ONLY A COMMENT. A reviewer
;; asked why the two are not told apart, and nothing in the tree answered
;; -- a decision that is only prose is one the next change can reverse
;; without anyone noticing. NEVER: It is pinned here as a choice, not
;; discovered here as a behaviour.
(want "EV-7 and undecodable bytes are grouped with unreadable syntax, deliberately"
      (list (reason-of (ask not-utf8-frame))
            (reason-of (ask (raw-frame (string-append "(request 1 \"" store "\" \"t\" #f wire #f #f")))))
      '(not-a-datum not-a-datum))

;; NEVER: AND THE TWIN THAT SAYS NON-ASCII STILL WORKS. Without it the row
;; above is passed by a build that refuses every byte over 127.
(want "EV-7 TWIN: a frame with valid non-ASCII is served"
      (car (ask (raw-frame (string-append "(request 1 \"" store "\" \"\x4f5c;\x8005;\" #f wire #f #f outline)"))))
      'ok)

;; ---- EV-8 the frame limit ---------------------------------------------------
;;
;; NEVER: A LIMIT THAT ONLY CATCHES CLIENTS WHO DO NOT FINISH THEIR FRAMES IS
;; NOT A LIMIT. The "a whole frame is here" branch came before the size
;; check, so the megabyte ceiling fired only on bytes that had NOT been
;; terminated. Measured: a request a megabyte over the limit with a
;; newline on the end was dispatched and answered `(ok (items))`; the same
;; bytes without the newline were refused.
;;
;; NOTE: WHAT IS MEASURED IS THE FRAME, NOT THE BUFFER. Several small frames
;; can arrive in one read and their total says nothing about any of them;
;; the twin below is an ordinary frame, which must still be served.
(define over-limit
  (let build ((n (+ (* 1024 1024) 1000)) (out '()))
    (if (zero? n) (list->string out) (build (- n 1) (cons #\x out)))))

(want "EV-8 a frame over the limit is refused even though it is terminated"
      (reason-of (ask (raw-frame (string-append
                                   "(request 1 \"" store "\" \"t\" #f wire #f #f search \""
                                   over-limit "\")"))))
      'frame-limit)

(want "EV-8 TWIN: an ordinary frame is still served"
      (car (ask (request-frame store 'outline '() '())))
      'ok)

;; ---- EV-3 the packer will not build what the reader will not read ---------
;;
;; The two sides are separate libraries because they are separate
;; processes; these rows are what says they still agree about the shape.

(want "EV-3 the packer refuses a mode the reader would refuse"
      (guard (e (#t 'refused))
        (request-frame store 'outline '() (list (cons 'mode 'shouty))))
      'refused)

(want "EV-3 the packer refuses a writer the reader would refuse"
      (guard (e (#t 'refused))
        (request-frame store 'outline '() (list (cons 'writer 7))))
      'refused)

(want "EV-3 the version the packer writes is the one the reader wants"
      (let* ((frame (utf8->string (request-frame store 'outline '() '())))
             (datum (read (open-string-input-port frame))))
        (list-ref datum 1))
      (envelope-version))

;; ---- EV-4 the answer envelope ---------------------------------------------
;;
;;   (answer (stdout "<bytes>") (stderr "<bytes>") (exit <n>) (origin <who>))
;;
;; KEY: THE SERVER DECIDES THE EXIT CODE, and that is the whole reason this
;; envelope exists. Whether an answer counts as a success is knowledge
;; about what a verb MEANS -- `check` turns on its verdict, `batch` on
;; every one of its items -- and it lives in `rpc-ok?`, in the core. A
;; client cannot import the core, so a client that worked the code out
;; would need a copy of that rule, and the copy would be the one that got
;; it wrong.

(want "EV-4 every answer arrives in an envelope"
      (let ((e (raw-answer (request-frame store 'outline '() '()))))
        (list (and (pair? e) (car e))
              (string? (envelope-part e 'stdout))
              (string? (envelope-part e 'stderr))
              (integer? (envelope-part e 'exit))))
      '(answer #t #t #t))

(want "EV-4 a verb that succeeded exits 0"
      (envelope-part (raw-answer (request-frame store 'outline '() '())) 'exit)
      0)

;; NEVER: NOT EVERY REFUSAL IS AN `(error ...)`, which is why the code comes
;; from the server: a caller matching on the answer's first symbol would
;; get this one right and `check` wrong.
(want "EV-4 a verb that was refused exits non-zero"
      (let ((n (envelope-part
                 (raw-answer (request-frame store 'read '("nosuch.1") '()))
                 'exit)))
        (if (and (integer? n) (> n 0)) 'non-zero (list 'exit n)))
      'non-zero)

;; NOTE: A REFUSAL FROM BEFORE THE ENVELOPE WAS READ still wears one. This
;; is the case that happens when something is already wrong, and it is
;; the one a client would be least able to cope with if it arrived bare.
(want "EV-4 a frame that could not be parsed is refused inside an envelope too"
      (let ((e (raw-answer (raw-frame "(request 99 \"x\" \"y\")"))))
        (list (and (pair? e) (car e)) (string? (envelope-part e 'stdout))))
      '(answer #t))

;; NOTE: TODAY'S FACT, PINNED SO THAT IT CANNOT CHANGE QUIETLY. The core
;; answers with one datum and has nothing to say on a second stream. The
;; field exists because the client also runs the server locally, where a
;; real stderr exists. The day something starts writing there, this row
;; goes red and somebody decides what the client should do with it --
;; rather than the message being dropped without a word.
(want "EV-4 stderr is empty today, every time"
      (list (envelope-part (raw-answer (request-frame store 'outline '() '())) 'stderr)
            (envelope-part (raw-answer (request-frame store 'read '("nosuch.1") '())) 'stderr))
      '("" ""))

;; NEVER: THE MODE CHOOSES THE RENDERING OF THE ANSWER, NOT OF THE ENVELOPE.
;; The envelope is read by a program either way; rendering IT in human
;; form would hand the client something it cannot parse.
(want "EV-4 human mode renders the answer, and still wears a wire envelope"
      (let* ((e (raw-answer (request-frame store 'outline '()
                                           (list (cons 'mode 'human)))))
             (out (envelope-part e 'stdout)))
        (list (and (pair? e) (car e))
              (if (and (string? out) (> (string-length out) 0)
                       (not (char=? #\( (string-ref out 0))))
                  'rendered-for-a-person
                  (list 'said out))))
      '(answer rendered-for-a-person))

(want "EV-4 TWIN: and wire mode renders the datum"
      (let* ((e (raw-answer (request-frame store 'outline '()
                                           (list (cons 'mode 'wire)))))
             (out (envelope-part e 'stdout)))
        (if (and (string? out) (> (string-length out) 0) (char=? #\( (string-ref out 0)))
            'rendered-for-a-program
            (list 'said out)))
      'rendered-for-a-program)


;; ---- EV-17 what may be handed to the reader --------------------------
;;
;; NEVER: THE GUARD'S MODEL OF THE LANGUAGE IS SMALLER THAN THE LANGUAGE, and
;; that is deliberate: it knows where a string ends and refuses, by
;; character class, everything else that can contain a quote. The rows
;; below are the two halves of that bargain -- what it must refuse, and
;; what it must still let through, since a guard that refused everything
;; would pass the first half and make the program useless.
;;
;; NOTE: MEASURED, AND IT DEFEATED THE GUARD. A reply beginning with a line
;; comment that contains a quote flipped the scan into "inside a string",
;; so a datum label after it passed unexamined, read as a cycle and hung
;; the reader at the eight second alarm -- the very failure the guard was
;; written for, walked past by a line of prose.
(want "EV-17 a line comment containing a quote is refused, and what follows it is never reached"
      (readable-shape? "; a comment with a quote \"\n#0=(answer (stdout \"x\") . #0#)\n")
      #f)

;; NEVER: TWIN: THE SAME CHARACTER INSIDE A STRING IS A CALLER'S OWN TEXT.
;; A semicolon in a title or a commit message is ordinary content, and a
;; guard that refused it would refuse the answers people actually get.
(want "EV-17 TWIN: a semicolon inside a string is content, and passes"
      (readable-shape? "(answer (stdout \"one; two\") (exit 0))")
      #t)

;; The other five constructs that can hold a quote, by the same rule.
;; NOTE: `#\"` IS THE ONE THAT READS AS A QUOTE AND IS NOT ONE: a character
;; literal holding the delimiter itself, which is how a peer would end a
;; string the scanner believes is still open.
(want "EV-17 a character literal holding a quote is refused"
      (readable-shape? "(answer (stdout #\\\") (exit 0))")
      #f)

(want "EV-17 a block comment is refused"
      (readable-shape? "#| a \" |# (answer (exit 0))")
      #f)

(want "EV-17 a datum comment is refused"
      (readable-shape? "#;(answer (stdout \"x\")) (answer (exit 0))")
      #f)

(want "EV-17 a bar-quoted symbol is refused"
      (readable-shape? "(answer (stdout |a \" b|) (exit 0))")
      #f)

;; NEVER: AND THE CONTROL: AN ORDINARY ANSWER STILL PASSES. Every row above is
;; satisfied by a guard that answers #f to everything; this one is the
;; only reason to believe the program can read its own replies at all.
(want "EV-17 a backslash outside a string is refused"
      (readable-shape? "(answer \\\" . #0=((x 1) . #0#))")
      #f)

(want "EV-17 and so is a backslash before an allocating literal"
      (readable-shape? "(\\\" #e1e100000)")
      #f)

(want "EV-17 a quote prefix is refused"
      (readable-shape? "'(answer (stdout \"x\"))")
      #f)

(want "EV-17 a symbol character outside the alphabet is refused"
      (readable-shape? "(answer (stdout \"x\") (\x00e9; 1))")
      #f)

(want "EV-17 a bracket is refused"
      (readable-shape? "[answer (exit 0)]")
      #f)

;; NEVER: AND A THIRD-PARTY CLIENT THAT WRITES THE FRAME ITSELF MEETS THE
;; GUARD. Our own command line refuses `show me` before printing it, in
;; the part both routes share -- but nothing stops somebody writing
;; `(request ... |show me| ...)` to the socket by hand, and a symbol
;; between bars can hold anything at all, a quote included.
;;
;; NOTE: THE TWO LAYERS ANSWER DIFFERENT QUESTIONS, and this row is the
;; second one: not "no daemon would accept this" but "these bytes are not
;; something I will hand to `read`".
(want "EV-17 a frame whose verb is written between bars is not a datum"
      (reason-of (ask (raw-frame (string-append "(request 1 \"" store "\" \"t\" #f wire #f #f |show me|)"))))
      'not-a-datum)

(want "EV-17 TWIN: the same daemon still serves an ordinary frame afterwards"
      (car (ask (request-frame store 'outline '() '())))
      'ok)


(want "EV-17 CONTROL: an ordinary answer passes, booleans and all"
      (list (readable-shape? "(answer (stdout \"x\") (stderr \"\") (exit 0) (origin core))")
            (readable-shape? "(request 1 \"/s\" \"a\" #f wire #f #f outline)")
            (readable-shape? "(answer (working #t) (exit 0))"))
      '(#t #t #t))

(system (string-append "pkill -f 'serve " store "' 2>/dev/null"))
(system (string-append "sleep 1; rm -rf " here " " sock-here))
(printf "rows: ~a\n~a failures\nenvelope complete\n" rows bad)
(exit (if (zero? bad) 0 1))
