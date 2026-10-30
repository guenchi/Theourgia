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

;;; (theourgia frame) -- one request frame, read and answered in one place.
;;;
;;; KEY: TWO READERS OF A FRAME, ONE RULE. A daemon's connection reads a
;;; frame off its socket; core.sc under `--frame` reads one off its standard
;;; input, written there by a thin client that found no daemon listening.
;;; Both judge it with what is here -- the frame's limit, its reader, the
;;; shapes refused before a verb is looked at, the dispatch with its guard,
;;; and the answer's envelope -- so the answer is the same on both routes
;;; because it is made by the same code, not because two copies agree.
;;;
;;; NEVER: NO SCHEDULER AND NO NETWORK. core.sc loads this to answer in its
;;; own process, and the actor system is the cost its imports are arranged
;;; to avoid; the thin client does not load this at all.

(library (theourgia frame)
  (export frame-limit parse-frame frame-refusal frame-field request-shape-error answer-for answer-envelope)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          (rnrs io ports) (rnrs unicode) (rnrs exceptions)
          (only (chezscheme) read)
          (only (theourgia rpc) rpc-dispatch rpc-ok?)
          (only (theourgia client) envelope-version readable-shape?)
          (only (theourgia render) render-wire render-human)
          (only (theourgia arguments) parse-arguments)
          (only (theourgia log) unreadable-entry? unreadable-entry-path unreadable-entry-reason)
          (only (theourgia answers) condition-text))

  ;; One request per line, one answer per line. NEVER: THE LIMIT IS CHECKED
  ;; BEFORE ANYTHING IS PARSED: an over-long frame is refused and the
  ;; connection closed, never dispatched.
  (define frame-limit (* 1024 1024))

  ;; NOTE: A RAISED `(error ...)` IS AN ANSWER THAT TOOK THE SHORT WAY OUT.
  ;; The locking strategy raises `(error store-busy ...)` from deep
  ;; inside the store, where no return value could carry it; folding it
  ;; into `internal` would tell the client we broke when the truth is
  ;; that somebody else is holding the lock.
  ;; NEVER: WHAT THE CALLER PIPED IN AND WHERE THE CALLER WAS travel with the
  ;; request. `parse-frame` has always carried both out of the envelope
  ;; and nothing read them: `batch`, whose intents ARE its standard
  ;; input, answered its usage line on this route, and `write <id> -`
  ;; stored the literal "-" and reported `(ok (saved ...))` -- the
  ;; caller's bytes discarded without a word. The rule that says which
  ;; verbs read standard input is `argument-stdin`, in the parser, and it
  ;; is applied by the dispatcher rather than restated here.
  (define (answer-for store parsed actor state writer piped cwd)
    (guard (e ((and (pair? e) (eq? 'error (car e))) e)
              ;; As rpc's guarded: an unreadable entry is named, never
              ;; answered as internal (K1).
              ((unreadable-entry? e)
               (list 'error 'unreadable
                     (list 'path (unreadable-entry-path e))
                     (list 'reason (unreadable-entry-reason e))))
              (#t (list 'error 'internal (list 'reason (condition-text e)))))
      (rpc-dispatch store parsed actor state writer
                    (and (string? piped) (lambda () piped))
                    cwd)))

  ;; The shapes `rpc-dispatch` refuses before it looks at a verb are
  ;; refused here in the same words, so that judging them does not depend
  ;; on the daemon being willing to serve.
  (define (request-shape-error request)
    (guard (e (#t #f))
      (and (pair? request)
           (symbol? (car request))
           (for-all string? (cdr request))
           (let ((nodes (parse-arguments (car request) (cdr request))))
             (and (pair? nodes) (eq? 'error (car nodes)) nodes)))))

  ;; NEVER: THE ENVELOPE NAMES THE STORE IT IS FOR:
  ;; `(request <store> <actor> <verb> <arg> ...)`.
  ;;
  ;; A client reaches a daemon by deriving the socket path from its
  ;; store, and `--socket P` lets it reach one directly -- so a request
  ;; meant for store B can arrive at a daemon serving store A. Without
  ;; the store in the envelope that request would be EXECUTED against A,
  ;; which is a write to the wrong library that nothing would ever
  ;; report. It is refused instead, and NEVER: not dispatched.
  ;; ---- the envelope, unpacked ---------------------------------------------
  ;;
  ;; NEVER: THE SHAPE IS PACKED IN `(theourgia client)` AND READ HERE, which
  ;; is two libraries because it is two processes. The ORDER is written
  ;; down in one of them; this side names the fields as it takes them
  ;; apart so that a change there shows up as a changed name here rather
  ;; than as a silently shifted meaning.
  ;;
  ;;   (request <version> <store> <actor> <writer> <mode> <cwd> <stdin>
  ;;            <verb> <args> ...)
  ;;
  ;; NOTE: THE VERSION IS CHECKED, NOT SKIPPED. A client built against a
  ;; later envelope must be told its request was not understood, rather
  ;; than have its fields read as though they were these fields -- which
  ;; is what a parser that only looked at positions would do, and would
  ;; do quietly.
  ;;
  ;; NOTE: `writer`, `cwd` AND `stdin` ARE EACH A STRING OR #f. #f is
  ;; "there is none", which is a thing the client says rather than a
  ;; field it leaves out: a frame whose length varies is one this reader
  ;; would have to guess about.
  (define (frame-field name parsed) (cdr (assq name parsed)))

  ;; NOTE: WHITESPACE AFTER THE FRAME IS NOT DATA. A trailing newline or a
  ;; space is how a frame ordinarily ends; only a further TOKEN is a
  ;; second thing on the line.
  (define (nothing-but-space-left? port)
    (let look ()
      (let ((c (lookahead-char port)))
        (cond
          ((eof-object? c) #t)
          ((char-whitespace? c) (get-char port) (look))
          (else #f)))))

  ;; NEVER: BYTES THAT ARE NOT UTF-8 ARE REFUSED, NOT REPAIRED. `utf8->string`
  ;; substitutes U+FFFD for a malformed sequence, so a frame whose actor
  ;; field held invalid bytes was decoded into a DIFFERENT actor and the
  ;; request then ran under it -- measured: an actor containing \xff\xfe
  ;; was accepted and the verb executed. What arrived is not what the
  ;; caller sent, and the caller is never told.
  ;;
  ;; NOTE: THE DECODER IS THE LIBRARY'S OWN, set to raise rather than
  ;; substitute. Writing a UTF-8 validator here would be a second decoder
  ;; to keep in step with the first.
  (define (frame-text bv)
    (bytevector->string bv (make-transcoder (utf-8-codec)
                                            (eol-style none)
                                            (error-handling-mode raise))))

  ;; NEVER: THE TWO WAYS A FRAME IS UNREADABLE ARE NOT THE SAME FACT, and the
  ;; refusal says which. `not-a-datum` is for bytes that will not decode
  ;; or will not read as a datum at all; `malformed-envelope` is for a
  ;; perfectly good datum whose fields are not an envelope, or a frame
  ;; with something after it. Both used to answer `not-a-datum`, which for
  ;; the second is simply untrue -- it IS a datum -- and sends the reader
  ;; looking at their encoding when the problem is their fields.
  (define (parse-frame bv)
    (guard (e (#t 'not-a-datum))
      (let ((text (frame-text bv)))
       (if (not (readable-shape? text))
           'not-a-datum
      (let* ((port (open-string-input-port text))
             (datum (read port))
             ;; NEVER: NOTHING MAY FOLLOW THE FRAME. A second datum on the
             ;; line is not a second request -- requests are separated by
             ;; the newline this reader was handed one of -- so it is
             ;; either a client that packed two things or someone
             ;; appending to a frame. Either way, executing the first and
             ;; discarding the rest is the worst of the options.
             ;;
             ;; NEVER: AND THE QUESTION IS PUT TO THE PORT, NOT TO THE READ.
             ;; Asked as "did the next read return an end-of-file object",
             ;; a frame containing the literal token `#!eof` answered yes
             ;; -- because a datum CAN BE an eof object. Measured: a frame
             ;; ending `... outline) #!eof (x)` executed `outline` and
             ;; discarded the rest, which is exactly what this check
             ;; exists to prevent, defeated by a literal.
             (clean (nothing-but-space-left? port)))
        (if (not (and clean (well-formed-envelope? datum)))
            ;; A VERSION-2 FRAME THAT CANNOT BE USED SAYS SO WITH ITS VERSION
            ;; (a bad field, something after it). The version is the one the
            ;; reader read, in its place, never a byte pattern. A client that
            ;; predates version 2 is answered today's bytes by a daemon that
            ;; predates it, and resends at version 1 on exactly those bytes; a
            ;; refusal naming the version is never them, so this daemon's own
            ;; refusal of a version-2 frame is never resent. Every other
            ;; version, and a frame whose version cannot be read, gets today's
            ;; bytes.
            (if (and (pair? datum) (eq? 'request (car datum))
                     (pair? (cdr datum)) (eqv? 2 (cadr datum)))
                (vector 'refused 'malformed-envelope 2)
                'malformed-envelope)
            (list (cons 'version (list-ref datum 1))
                  (cons 'store (list-ref datum 2))
                  (cons 'actor (list-ref datum 3))
                  (cons 'writer (list-ref datum 4))
                  (cons 'mode (list-ref datum 5))
                  (cons 'cwd (list-ref datum 6))
                  (cons 'stdin (list-ref datum 7))
                  (cons 'request (list-tail datum 8)))))))))

  ;; NEVER: THE ENVELOPE'S WRITER IS NOT SPLICED INTO THE ARGUMENTS. It used
  ;; to be: two tokens pushed to the front of the request unless a string
  ;; search over unparsed argv found `--writer` already there. That search
  ;; could not tell an option from a caller's own text, so a request whose
  ;; arguments contained the literal `--writer` -- after a `--`, or as the
  ;; bytes being written -- suppressed its own default and was refused
  ;; `writer-required`. The accepted cost of a string search turned out to
  ;; be a legitimate write that could not be made at all.
  ;;
  ;; The writer now travels beside the actor, as the identity it is, and
  ;; `rpc-dispatch` applies it in one place to the verbs that take one.
  ;; The arguments reach the parser exactly as the caller wrote them.

  (define (string-or-false? x) (or (not x) (string? x)))

  ;; A FRAME parse-frame REFUSED, AS THE ANSWER'S DATUM, or #f for one it
  ;; parsed: (error bad-request (reason <why>)), with (version 2) after the
  ;; reason when the frame was a version-2 frame.
  (define (frame-refusal parsed)
    (cond
      ((symbol? parsed) (list 'error 'bad-request (list 'reason parsed)))
      ((and (vector? parsed) (= 3 (vector-length parsed)) (eq? 'refused (vector-ref parsed 0)))
       (list 'error 'bad-request (list 'reason (vector-ref parsed 1)) (list 'version (vector-ref parsed 2))))
      (else #f)))

  (define (well-formed-envelope? datum)
    (and (list? datum)
         (>= (length datum) 9)
         (eq? 'request (car datum))
         ;; VERSION 1, THE ENVELOPE OF envelope-version, OR 2, THE SAME
         ;; ENVELOPE FROM A CLIENT THAT READS KEEPALIVES. A connection
         ;; reads the version from the parse and keeps it alive only on 2.
         (let ((v (list-ref datum 1))) (or (eqv? v (envelope-version)) (eqv? v 2)))
         (string? (list-ref datum 2))
         (string? (list-ref datum 3))
         (string-or-false? (list-ref datum 4))
         (memq (list-ref datum 5) '(wire human))
         (string-or-false? (list-ref datum 6))
         (string-or-false? (list-ref datum 7))
         (symbol? (list-ref datum 8))
         ;; NOTE: AND THE ARGUMENTS, which this did not check. The packer
         ;; refuses to build a frame whose arguments are not all strings;
         ;; the reader accepted one. `EV-3` asserts the packer will not
         ;; make what the reader will not read, and the reverse had no
         ;; guarantee at all.
         ;;
         ;; NEVER: IT WAS NOT REACHING ANYTHING DANGEROUS -- measured, the
         ;; dispatcher refuses a non-string argument itself
         ;; (`rpc.sc`, `arguments-not-strings`) and the daemon goes on
         ;; serving. This is the reader being symmetric with the packer,
         ;; not a repair of a crash, and the row that covers it asserts
         ;; the answer that actually comes back rather than a predicted
         ;; one.
         (all-strings? (list-tail datum 9))
         #t))

  (define (all-strings? xs)
    (or (null? xs)
        (and (string? (car xs)) (all-strings? (cdr xs)))))

  ;; ---- the answer envelope ------------------------------------------------
  ;;
  ;;   (answer (stdout "<bytes>") (stderr "<bytes>") (exit <n>) (origin <who>))
  ;;
  ;; KEY: THE SERVER RENDERS AND THE SERVER DECIDES THE EXIT CODE. Both are
  ;; knowledge about what a verb's answer MEANS -- `check` is a failure
  ;; when its verdict is not ok, `batch` when any one of its items is not
  ;; -- and that knowledge lives in `rpc-ok?`, in the core. A client that
  ;; worked it out would need the core, which is the cost the whole split
  ;; exists to avoid, and a client that carried a copy of the rule would
  ;; be a second place for it to be wrong.
  ;;
  ;; So the client unpacks this one shape, writes two byte strings to two
  ;; streams and exits with a number. That is the same work for every
  ;; verb, and is not knowledge about any of them.
  ;;
  ;; NOTE: `stderr` IS EMPTY TODAY, EVERY TIME. The core answers with one
  ;; datum and has nothing to say on a second stream; the field is here
  ;; because the client also runs the server locally for `init`, `serve`
  ;; and `eval`, where a real stderr exists and has to arrive somewhere.
  ;; NEVER: That is a description of today, not a promise -- a row asserts it
  ;; is empty, so the day something starts writing there it is a red row
  ;; and not a silently dropped message.
  ;; NEVER: WHO IS REFUSING IS NOT SOMETHING THE TEXT CAN BE ASKED. A core
  ;; `(error unknown-id ...)` is a successful call whose answer is no; a
  ;; `(error draining ...)` or `(error transport-unknown ...)` is this
  ;; daemon declining to carry the request, or admitting it does not know
  ;; whether it did. They arrive in the same envelope, and a shell that
  ;; had to tell them apart could only inspect the words inside -- or
  ;; treat every non-zero exit as a failure, which would make every core
  ;; refusal look like a broken call.
  ;;
  ;; NOTE: IT CANNOT BE DERIVED FROM THE TAG EITHER: `bad-request` is
  ;; answered both by this file, about a frame, and by the core, about a
  ;; verb's arguments. What separates them is WHERE the answer was made,
  ;; so that is what is recorded -- `transport` by the paths that refuse
  ;; without dispatching, `core` by the two that carry a dispatched
  ;; result.
  ;;
  ;; NOTE: It is a new field at the END of the envelope, and every reader
  ;; here takes fields by name, so a client of the previous shape reads
  ;; this one unchanged.
  (define (answer-envelope result mode origin)
    (list 'answer
          (list 'stdout ((if (eq? mode 'human) render-human render-wire) result))
          (list 'stderr "")
          (list 'exit (if (rpc-ok? result) 0 1))
          (list 'origin origin)))

)
