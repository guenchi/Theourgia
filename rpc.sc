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

;; One request, one answer. This is what a verb MEANS, in the one place
;; that says so.
;;
;; THE COMMAND LINE AND THE NETWORK ASK THE SAME QUESTIONS, and they used
;; to answer them separately: the shape of a refusal, which outcomes
;; count as success, what "no store here" is called, how an id that does
;; not exist is reported. Two answers to one question is the defect this
;; library exists to remove -- and it was already present, with
;; `nearest-ids` built twice inside the command line, once for `read` and
;; once for `read --md`.
;;
;; ARGUMENTS ARE STRINGS, exactly as they arrive on a command line:
;; `(insert "--under" "root" "--title" "x")`. A typed request shape would
;; read better over a socket and would be a SECOND shape, and then the
;; numeric gate and the cut parser each have two callers that can drift.
;; One parser is a fact only while there is one shape to parse.
;;
;; NOTHING HERE EXITS, PRINTS OR RAISES. Every failure this library can
;; reach becomes an answer with a name in it; deciding what to do with an
;; answer belongs to whoever asked.
(library (theourgia rpc)
  (export rpc-dispatch rpc-dispatch-parsed rpc-ok? rpc-verbs register-verbs! dispatch-helper
          request-frame transport-unreachable?
          count-argument outline-text write-protocol verb-catalogue
          describe-log-error eval-usage subscribe-shape-error)
  (import (only (theourgia view) view-read)
          (only (theourgia render) render-wire)
          (only (theourgia client) request-frame verb-spelling-error no-daemon-errno?)
          (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs conditions) (rnrs io ports) (rnrs files)
          (rnrs unicode) (rnrs arithmetic fixnums) (rnrs bytevectors)
          (rnrs hashtables)
          (theourgia store) (theourgia reduce) (theourgia log)
          (theourgia working) (theourgia baseline) (theourgia code-project) (theourgia code-suggest)
          (theourgia datum-project)
          (only (theourgia datum-code) datum-source-read)
          (only (theourgia template-read) store-template template-roots template-relations)
          (only (theourgia ffi) read-entry entry-type directory-entries fs-error? with-mutation-record mutation-record)
          (only (theourgia answers) classify-failure)
          (only (theourgia incomplete) incomplete-accepted incomplete-refused)
          (only (theourgia request) req-id-ok?)
          (rnrs eval)
          (theourgia arguments) (theourgia project) (theourgia md))

  ;; ---- answers --------------------------------------------------------------

  ;; NOTHING RAISED REACHES A CALLER. An agent reads an S-expression; a
  ;; backtrace is neither an answer nor readable, so a caller would get
  ;; nothing and no way to tell "the store does not exist" from "the tool
  ;; broke". Every failure reachable here is translated into a form with
  ;; a name in it.
  (define (describe-log-error e)
    (let ((detail (log-error-detail e)))
      (append (list 'error (log-error-kind e))
              (if (log-error-writer e) (list (list 'writer (log-error-writer e))) '())
              (if (log-error-segment e) (list (list 'segment (log-error-segment e))) '())
              (if (and (pair? detail) (pair? (car detail)))
                  (map (lambda (p) (list (car p) (cdr p))) detail)
                  '()))))

;; ---- exporting a writer's working view (F17) -------------------------------
  ;;
  ;; NEVER: THE VIEW IS working.sc's `working-state`, the one reduction a
  ;; writer's drafts make over its baseline; no export rebuilds it.
  ;;
  ;; NOTE: THE VIEW IS TAKEN WHEN THE EXPORTER ASKS FOR IT, after its own
  ;; checks, so a directory that is not one is answered as the plain
  ;; export answers it. A refusal from the view (no writer, drafts that
  ;; cannot be read) is raised there and is the export's answer.
  ;;
  ;; NOTE: THE CLAUSES GO AFTER THE EXISTING ONES, and only on this route:
  ;; `(cut <c>)` is the committed cut the view stands on, `(working #t)`
  ;; says the files are a view and not the committed state. A plain export
  ;; answers byte for byte as before.
  (define (working-export store state writer export)
    (let* ((view #f)
           (a (export (lambda ()
                        (let ((w (working-state store state writer)))
                          (unless (and (pair? w) (eq? 'ok (car w))) (raise w))
                          (set! view (caddr w))
                          view)))))
      (if (and view (pair? a) (eq? 'ok (car a)))
          (append a (list (list 'cut (reduce-applied-cut view)) '(working #t)))
          a)))

  (define (guarded thunk)
    (guard (e ((and (list? e) (pair? e) (eq? (car e) 'error)) e)
              ;; A FILESYSTEM FAILURE IS NOT ANSWERED HERE (F100b, D7): it
              ;; goes on to the whole dispatch's table in
              ;; rpc-dispatch-parsed, which answers it with the dispatch's
              ;; mutation record -- what this verb had already changed.
              ;; Answered here it lost that record, and a durable-error
              ;; (a vector, not a condition) fell to `internal` below.
              ((unreadable-entry? e) (raise e))
              ((fs-error? e) (raise e))
              ((log-error? e) (describe-log-error e))
              ;; A THROWN VALUE NEED NOT BE A CONDITION. The sexpr layer
              ;; raises `#(sexpr-error <message> <position>)`, a vector,
              ;; so `message-condition?` was false and the answer said
              ;; "unexpected failure" while the thrown object was
              ;; carrying the reason all along. This reads it.
              ;;
              ;; IT IS A DIAGNOSTIC IMPROVEMENT, NOT A FIX: the next
              ;; thrown value of some other shape falls back the same
              ;; way. What stops a caller being told a durable write
              ;; failed is the split between committing and reporting.
              (#t (list 'error 'internal
                        (list 'condition
                              (cond
                                ((and (vector? e) (= 3 (vector-length e))
                                      (eq? (vector-ref e 0) 'sexpr-error))
                                 (vector-ref e 1))
                                ((message-condition? e) (condition-message e))
                                (else "unexpected failure"))))))
      (thunk)))

  ;; A RAISE AFTER A WRITE WAS ENTERED, before its premises were checked, is
  ;; answered as this dispatcher answers any raise (store.sc, premises-
  ;; preflight's RUN): a filesystem failure with the dispatch's record, and
  ;; anything else as guarded answers it. Installed once, when this library
  ;; is loaded, so every route that dispatches has it.
  (define raise-answer-installed
    (begin
      (store-raise-answer-hook!
        (lambda (e) (or (classify-failure e (mutation-record)) (guarded (lambda () (raise e))))))
      #t))

  ;; HOW TO WRITE INTO A STORE, IN ONE PLACE.
  ;;
  ;; KEY: THIS STRING IS THE ONLY COPY. The README's `## Writing for
  ;; agents` section is this text, and the MCP shell's `insert` and
  ;; `write` tool descriptions begin with it. A protocol stated in three
  ;; places is three protocols the moment one of them is edited, and the
  ;; one a reader happens to meet is the one they will follow.
  ;;
  ;; NOTE: ENGLISH, like every other user-facing string in this library.
  ;;
  ;; NEVER: EDITING THIS IS A USER-VISIBLE CHANGE. `f1-protocol.sc` compares
  ;; it to the README byte for byte (DOC-P1), so the README cannot drift from
  ;; it, and DOC-P2 checks that the phrases in ITS OWN LIST are still here.
  ;;
  ;; NEVER: AND THAT LIST IS NOT EVERY RULE. This said DOC-P2 checks each
  ;; rule. It checks the nine it names, and the writer-holding rule is not
  ;; among them -- that one is asked separately, by `docs-check.sc`'s DOC-5,
  ;; which looks for the phrase "one agent at a time" in this string and in
  ;; the README. NOTE: A PHRASE, NOT A RULE. Change what holding means, or
  ;; drop the requirement while keeping the words, and DOC-5 stays green. Saying "each rule" was replacing one
  ;; sentence that claimed more than its file does with another that claimed
  ;; more than its row does.
  ;;
  ;; NEVER: AND IT NAMES THE FILE THAT ACTUALLY ASKS. This said `docs-check.sc`
  ;; for a round, which does neither of those two things: its DOC-5 asks a
  ;; narrower question, whether the writer-holding rule is in the README and in
  ;; this constant. A sentence about which file guarantees what is the worst
  ;; kind to get wrong, because a reader stops looking once they believe a gate
  ;; is there.
  ;;
  ;; NEVER: AND IT IS THE FILE ON DISK, NOT THE COMMITTED ONE. This said
  ;; COMMITTED. The reader is `(file-text "../README.md")`, so what is compared
  ;; is the working tree -- which is the right thing to compare and the wrong
  ;; thing to call it.
  (define write-protocol
    (string-append
      "A block is the unit of writing: one block should answer one question on its own.\n"
      "Keep a block under about 800 tokens (roughly 3000 bytes); split a longer one.\n"
      "Give every block a one-sentence title.\n"
      "Give every block 3 to 8 keywords, comma separated, with --keywords.\n"
      "Place a block under the parent its source or subject puts it under, with --under.\n"
      "Do not rewrite the source bytes: splitting a document must not edit its prose.\n"
      "Change a block with write and then commit, through a draft, rather than replacing it.\n"
      "Hold a writer id from one agent at a time: a later session may bind the same id and carry on with its drafts, but two agents writing one draft at once overwrite each other silently.\n"
      "Record a decision as a block of kind decision, and link each block that implements it with link <block> implements <decision>.\n"
      "Open a session with commitments --open, which lists the decisions not yet implemented, done or dropped.\n"
      "Set class with set <id> class inference for what you concluded and did not verify, and with set <id> class external for material from outside; neither is ever treated as a ruling.\n"
      "To replace a ruling, write the new one and link <new> supersedes <old>; to record that something is wrong, link <evidence> refutes <claim>.\n"
      "Say what your work rests on with link <work> depends-on <premise>: when the premise changes, the work is marked for review.\n"
      "After checking that an implementation still carries out its decision, link <impl> implements <decision> again: linking again is how you say you checked.\n"))

  (define (usage form) (list 'usage form))

  ;; AN ANSWER SAYS WHICH OF THREE THINGS IT IS, so that whoever renders
  ;; it never has to guess from the shape. `(ok (items …))` is a list to
  ;; be shown one per line, `(ok (text …))` is bytes to be shown as they
  ;; are, and anything else is a single datum. Guessing -- "a list of
  ;; pairs is probably items" -- would classify a block's field alist as
  ;; a list of items the first time someone read a block with two fields.
  (define (items xs) (list 'ok (cons 'items xs)))

  ;; THE VERSION A READ HANDS BACK IS THE TOKEN `--if-unchanged` COMPARES:
  ;; `block-hash` of the block in the state read, the same string a write
  ;; is checked against. A deleted block has no version to offer, and its
  ;; answer is the one it always was; an absent id never reaches here.
  ;;
  ;; NEVER: A BLOCK THAT CANNOT BE HASHED IS STILL READ. A value nested
  ;; deeply enough is written and readable while its hash cannot be
  ;; encoded (nesting-depth.sc's middle band), and the read answered it
  ;; before there was a version. So the version is a part of the answer
  ;; that may be missing, as a write's state section is: the reason says
  ;; why, and the record comes back as it always did.
  (define (read-version state id)
    (let ((b (state-read state id)))
      (and b (not (cdr (assq 'deleted b)))
           (guard (e (#t (list 'unavailable (list 'reason (failure-text e)))))
             (block-hash state id)))))

  ;; NEVER: WHAT A READER WAS GIVEN IS SAID IN THE ANSWER, BY ONE PROCEDURE. A
  ;; receipt is `(cut <alist>)`, the applied cut of the state the answer was
  ;; read from, and `(versions ((<id> . <hash>) ...))`, one pair for every
  ;; block whose content the answer shows or whose id it lists as a result,
  ;; in order of first appearance and each id once -- the token
  ;; `--if-unchanged` compares, by `read-version`, so a block that cannot be
  ;; hashed is `(<id> unavailable (reason ...))` and a deleted one is left
  ;; out, as `read --recursive` already answered. A handler appends what it
  ;; asks for AFTER its own clauses and inserts nothing before or between
  ;; them; `parts` names the clauses wanted, both by default, so a verb that
  ;; already says its cut asks for the versions only. With `ids` #f there
  ;; are no versions: the verb lists no block as a result. A second part is
  ;; an alist of versions the handler already read, so a block is hashed
  ;; once per answer.
  (define (receipt state ids . parts)
    (let ((want (if (pair? parts) (car parts) '(cut versions)))
          (known (if (and (pair? parts) (pair? (cdr parts))) (cadr parts) '())))
      (append
        (if (memq 'cut want) (list (list 'cut (reduce-applied-cut state))) '())
        (if (and ids (memq 'versions want))
            (list (list 'versions (receipt-versions state ids known)))
            '()))))

  (define (receipt-versions state ids known)
    (let ((seen (make-hashtable string-hash string=?)))
      (let loop ((ids ids) (out '()))
        (cond
          ((null? ids) (reverse out))
          ((or (not (string? (car ids))) (hashtable-ref seen (car ids) #f)) (loop (cdr ids) out))
          (else
           (hashtable-set! seen (car ids) #t)
           (let ((v (let ((k (assoc (car ids) known))) (if k (cdr k) (read-version state (car ids))))))
             (loop (cdr ids) (if v (cons (cons (car ids) v) out) out))))))))

  ;; A CLAUSE THE DISPATCHER ADDS AFTER THE HANDLER HAS ANSWERED -- today
  ;; `(incomplete ...)` -- FOLLOWS THE RECEIPT: the order is the handler's
  ;; clauses, the receipt, the dispatcher's clauses. The handler cannot append
  ;; after what is added once it has returned, and nothing reorders them; an
  ;; answer with the receipt removed is still the answer it was.

  ;; THE CLAUSES A MACHINE READS, AND WHY THE CORE DOES NOT GATE THEM.
  ;;
  ;; These appear only under `--wire`, and that is the RENDERER's doing, not
  ;; a decision taken here. `render-human` for an items answer writes the
  ;; items and ignores every other clause, so a clause added after `items`
  ;; is invisible to a person and present for a machine, and the core does
  ;; not have to know which one is asking.
  ;;
  ;; The first version asked `(argument-option options "--wire")` here and
  ;; emitted nothing: `--wire` is the CLIENT's flag, consumed where the
  ;; answer is rendered, and a handler never sees it. That reading -- no
  ;; clauses at all, under either mode -- is what pointed at the renderer.
  ;;
  ;; THE ZERO-HIT CONTRACT STILL HOLDS, and it is why this matters: a person
  ;; who greps for something absent must get no output at all, because the
  ;; plugin parses that same human text and discards the whole answer on any
  ;; item it does not recognise. NOTHING ON THAT PATH IS AN ITEM, so nothing
  ;; here can reach it; the contract is kept by `render-human` and pinned by
  ;; `cli3.sc`'s row that greps for an absent word and asks for no output.
  ;;
  ;; AND THE CORE DOES NOT ASK A SECOND TIME. It would be easy to have the
  ;; handler check for `--wire` as well, as a belt on top of the braces.
  ;; That would be a SECOND place answering "is this answer for a machine or
  ;; for a person", and two places that answer one question are what this
  ;; batch has spent its length taking apart. The renderer answers it.
  ;;
  ;; `cut` says which state was read, `scanned` says how much was looked at
  ;; and where, and `coverage` -- only when nothing was found -- says
  ;; whether the absence is a fact about the store or about an index that
  ;; was not there to be consulted.
  (define (scan-clauses report empty?)
    (let ((field (lambda (k) (let ((e (assq k report))) (and e (cdr e))))))
      (append
        (list (list 'cut (field 'cut))
              (append
                (list 'scanned
                      (list 'blocks (field 'scanned-blocks))
                      (list 'fields (field 'fields)))
                ;; `unreadable-blocks` SAYS HOW MANY OF THOSE BLOCKS HELD
                ;; TEXT THIS SCAN COULD NOT READ, counted once per block.
                ;;
                ;; THE NAME CARRIES THE UNIT, and that is the reason for it
                ;; rather than a nicety. `(unreadable 1)` reads as "one
                ;; what": lines, fields, bytes and blocks are all plausible
                ;; here, and the clause beside it counts BLOCKS while the
                ;; process counter this is derived from counts ATTEMPTS. It
                ;; is the same rule `(truncated (hits n))` is written under
                ;; -- a number after a clause name means whatever the verb
                ;; decided it meant -- and it would apply even if the word
                ;; were free.
                ;;
                ;; IT IS ALSO TAKEN, and by this project's own client.
                ;; Read in `theourgia-vsc/src/client.ts` on the `vscode`
                ;; branch: `TransportError('unreadable', ...)` is raised
                ;; there when an answer's SHAPE cannot be read -- two item
                ;; lists where one was expected, and the like. That is a
                ;; client-side refusal and never travels on the wire, so the
                ;; two never meet in one place. They would still be one word
                ;; meaning two things on the two sides of one boundary, and
                ;; the cheapest moment to avoid that is before the first
                ;; reader.
                ;;
                ;; A VERB THAT READ NO BLOCK'S TEXT DOES NOT GET IT, by the
                ;; same rule that keeps `coverage` off grep's answers: a
                ;; clause that could only ever be `(unreadable-blocks 0)`
                ;; is true as English and wrong as a reading: it points a
                ;; worried caller at something that plays no part in the
                ;; answer. `search` and `grep` read block text and carry the
                ;; count; `whereis` reads the definitions index, whose
                ;; unreadable blocks are a different fact with a counter of
                ;; its own.
                ;;
                ;; Presence in the report decides, so the rule lives in the
                ;; verb that knows whether it read any text -- not in a list
                ;; of verb names here, which would be a second place
                ;; answering the same question.
                (if (assq 'unreadable-blocks report)
                    (list (list 'unreadable-blocks (field 'unreadable-blocks)))
                    '())))
        ;; COVERAGE IS THE STATE OF THE INDEX THIS ANSWER CONSULTED, and a
        ;; verb that consults none does not get the clause.
        ;;
        ;; For `search` it is the whole point: a caller deciding whether to
        ;; trust an empty answer needs to know whether nothing in this store
        ;; answers to that, or whether there was nothing to consult. The
        ;; NUMBER of names is what says which; the paragraph below says why
        ;; it is a number and not a word.
        ;;
        ;; `grep` answers that same question with `scanned`: `(blocks 0)` is
        ;; its "there was nothing to look at". A `coverage` clause here would
        ;; be a constant -- `(defs (names 0))` on every answer, for ever --
        ;; and a clause that never varies is noise that gets read as
        ;; information. It is true as English and wrong as a reading: it
        ;; points a worried caller at something that plays no part in the
        ;; answer it is worried about.
        ;;
        ;; `grep` reads the text of every live block and nothing else. Giving
        ;; it `(defs absent)` on a zero-hit answer would be true as English
        ;; and wrong as a reading: it says an index was not there, when no
        ;; index was wanted, and a caller deciding whether to trust an empty
        ;; answer would be told to worry about something that plays no part
        ;; in it. `scanned` already says what grep looked at.
        ;;
        ;; The verb that DOES consult the definitions index says so here.
        ;; HOW MANY NAMES, NOT WHETHER THERE IS AN INDEX.
        ;;
        ;; NEVER: `(defs absent)` WAS A WORD NOTHING COULD PRODUCE. The
        ;; index is always built -- it guards per block and answers with its
        ;; table whatever it found -- so the old clause said `built` for
        ;; every store there is, including an empty one. It existed to
        ;; separate "nothing in this store answers to that" from "there was
        ;; nothing to consult" and could only say the first.
        ;;
        ;; `(names 0)` is the second, and it is the ordinary case rather than
        ;; a corner: a 3564-block markdown corpus holds zero names, the same
        ;; as an empty store -- but now it also says 3564 blocks beside it,
        ;; and the two readings together are what a caller needs. What holds
        ;; no names is a store with nothing NAMEABLE in it; the reason once
        ;; written here, that only a `--datum` import writes name-bearing
        ;; blocks, is wrong and the fixture `d4e` in `test/cli3.sc` -- a
        ;; library imported without that flag, answering `(names 1)` -- is
        ;; what overturned it. `store.sc` carries the same correction beside
        ;; the field this clause reads.
        ;;
        ;; NEVER: AND ZERO HAS A SECOND SOURCE THIS ANSWER CANNOT SEPARATE.
        ;; A store whose nameable blocks could not be READ also answers
        ;; `(names 0)`: the index guards each block and counts the failures
        ;; elsewhere. "None" and "we could not tell" are one number, which is
        ;; the shape this clause replaced `(defs built)` to fix. Naming the
        ;; second is scheduled; until then a reader of `(names 0)` does not
        ;; know which of the two it has, and this comment is the only place
        ;; that says so.
        (if (and empty? (assq 'defs-names report))
            (list (list 'coverage
                        (list 'defs (list 'names (field 'defs-names)))
                        (list 'names-from '(datum lexical))))
            '()))))
  (define (text t) (list 'ok (list 'text t)))

  ;; A STORE THAT IS NOT THERE IS ITS OWN ANSWER, decided before anything
  ;; opens it. Letting the load layer discover it works, but it answers
  ;; with the kind of the file that was missing rather than with the fact
  ;; the caller can act on: there is no store here.
  ;; THROUGH THE DOOR (F100a): absent is no-store, as before; a meta.sexp
  ;; whose directory cannot be searched raises instead of reading as no
  ;; store (F100b translates what escapes).
  ;; A VERB TAKES `--premises` EXACTLY WHEN ITS USAGE FORM NAMES IT: the
  ;; catalogue is the one place a verb's options are written down.
  (define (takes-premises? verb)
    (let ((e (assq verb (verb-catalogue))))
      (and e (let walk ((x (cadr e)))
               (cond ((equal? x "--premises") #t)
                     ((pair? x) (or (walk (car x)) (walk (cdr x))))
                     (else #f))))))

  ;; -> the refusal of a premise set the store cannot check, or #f. Reading
  ;; the set reads nothing of the store (store.sc, premises-preflight).
  (define (premises-refusal store verb options)
    (let ((text (argument-option options "--premises")))
      (and text (takes-premises? verb)
           (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
             (premises-preflight store text #f)
             #f))))

  (define (no-store? store)
    (and (eq? (entry-type (string-append store "/meta.sexp")) 'absent)
         (list 'error 'no-store store)))

  ;; WHETHER AN ANSWER IS A SUCCESS IS A RULE, not a shape anyone may
  ;; guess at. It lives here because both callers need it and neither may
  ;; have its own opinion: a shell reads an exit code and a sync client
  ;; reads a status, and they must agree.
  (define (rpc-ok? answer)
    (cond
      ((not (pair? answer)) #f)
      ((eq? (car answer) 'ok) #t)
      ((eq? (car answer) 'error) #f)
      ((eq? (car answer) 'usage) #f)
      ;; `check` reports whatever it found and says separately whether
      ;; the store is sound.
      ((eq? (car answer) 'check)
       (let ((v (assq 'verdict (cdr answer))))
         (and v (eq? (cadr v) 'ok))))
      ;; a list of per-intent answers is a success when every one is
      ((memq (car answer) '(batch import))
       (for-all (lambda (a) (eq? (car a) 'ok)) (cadr answer)))
      (else #f)))

  ;; ---- argument shapes ------------------------------------------------------

  ;; A NUMBER FROM A REQUEST IS CHECKED BY SHAPE BEFORE IT IS CONVERTED.
  ;; `string->number` implements the whole of Scheme's numeric syntax, and
  ;; `#e1e99999999` is twelve characters requesting an exact integer of a
  ;; hundred million decimal digits -- about 40 MiB of value bits, built by a
  ;; conversion whose cost bears no relation to the caller's twelve bytes. It
  ;; does not refuse, and no check placed after it runs until it is done. A shape test first -- plain ASCII digits, and few
  ;; enough of them to name something a store could hold -- makes the
  ;; conversion bounded work.
  (define count-digit-limit 18)

  (define (count-argument s)
    (and (string? s)
         (> (string-length s) 0)
         (<= (string-length s) count-digit-limit)
         (let loop ((i 0))
           (cond
             ((= i (string-length s)) (string->number s 10))
             ((char<=? #\0 (string-ref s i) #\9) (loop (+ i 1)))
             (else #f)))))

  ;; ---- rendering ------------------------------------------------------------

  ;; ONE BLOCK, ONE ROW -- WHATEVER THE TITLE CONTAINS. The write path
  ;; refuses a title carrying a line terminator, but a record from another
  ;; machine or an older build can hold one, and this listing put it
  ;; through unchanged: the output then had MORE ROWS THAN BLOCKS, and
  ;; the extra row looked exactly like a real one. A title reading
  ;; `"Fake<newline>- zzzzzzzz.9  Phantom"` produced a row for a block
  ;; that does not exist, and anything reading this output as text
  ;; believed it. Measured: two records, two blocks, three rows.
  ;;
  ;; THE ESCAPE BELONGS TO THIS LISTING AND NOWHERE ELSE. `read` answers
  ;; the field as it is stored, because a caller asking for the value
  ;; wants the value; only a line-per-block rendering has a reason to
  ;; rewrite it. It is applied HERE, at the one procedure both rows go
  ;; through, rather than at each `put-string` -- there are two of those
  ;; and the second is the one a later edit forgets.
  (define (escape-one-line s)
    (let-values (((out get) (open-string-output-port)))
      (let loop ((i 0))
        (if (= i (string-length s))
            (get)
            (let* ((c (string-ref s i)) (n (char->integer c)))
              (cond
                ((= n 10) (put-string out "\\n"))
                ((= n 13) (put-string out "\\r"))
                ((or (= n 133) (= n 8232) (= n 8233)
                     (and (not (= n 9))
                          (or (< n 32) (and (>= n 127) (< n 160)))))
                 (put-string out "\\x")
                 (put-string out (number->string n 16))
                 (put-string out ";"))
                (else (put-char out c)))
              (loop (+ i 1)))))))

  ;; NEVER: A DERIVATION THAT RAISES IS NOT A REFUSAL WITH NO BLOCK. A
  ;; block's derived fields are read through the language table, and a
  ;; raise there was answered as guarded answers any raise, which named
  ;; no block, so a reader of a subtree could not tell which one. The
  ;; refusal is guarded's, byte for byte, with (id <id>) after it; a
  ;; filesystem failure goes on as guarded sends it. Every reading of the
  ;; view in this file comes through here.
  (define (view-of state id)
    (guard (e (#t (raise (append (guarded (lambda () (raise e))) (list (list 'id id))))))
      (view-read state id)))

  ;; A TITLE IS SOMETHING A PERSON READS, so it comes from the view: for
  ;; a code block the name is derived from its source, and `state-read`
  ;; no longer derives anything.
  (define (title-of state id)
    (let* ((b (view-of state id))
           (fs (and b (cdr (assq 'fields b)))))
      (define (get k)
        (let ((e (and fs (assq k fs))))
          (and e (or (and (string? (cdr e)) (cdr e))
                     (and (eq? k 'name) (datum-spelling (cdr e)))))))
      (define (fallback)
        (if (not (and fs (equal? (assq 'kind fs) '(kind . code)))) ""
            (let* ((lang (assq 'lang fs))
                   (parent (cadr (assq 'position b)))
                   (siblings (map caddr (filter (lambda (r) (equal? (car r) parent)) (state-outline state)))))
              (string-append
                (if (and lang (symbol? (cdr lang))) (symbol->string (cdr lang)) "unknown") ":"
                (number->string
                  (let loop ((xs siblings) (n 1))
                    (if (or (null? xs) (equal? (car xs) id)) n (loop (cdr xs) (+ n 1)))))))))
      (escape-one-line (or (get 'title) (get 'path) (get 'name) (fallback)))))

  ;; THE OUTLINE IS TEXT, and it is rendered here rather than by whoever
  ;; asked. A caller that received the rows and drew them itself would be
  ;; a second opinion about what an outline looks like -- and the two
  ;; would differ first at exactly the rows that are hardest to draw, the
  ;; ones in a structural conflict.
  ;; NOTE: THE SECOND REST ARGUMENT IS OPTIONAL SO EVERY EXISTING CALLER IS
  ;; UNCHANGED, and `--with-keywords` is off unless it is asked for. A
  ;; listing that grew a suffix by default would change the bytes of
  ;; every outline anybody has ever scripted against.
  (define (outline-text state . limit)
    (let*-values (((out get) (open-string-output-port)))
      (let* ((depth-limit (if (pair? limit) (car limit) #f))
             (with-keywords? (and (pair? limit) (pair? (cdr limit)) (cadr limit)))
             ;; id -> the signature an editor supplied, or #f; absent, no column
             (signature-of (if (and (pair? limit) (pair? (cdr limit)) (pair? (cddr limit)))
                               (caddr limit)
                               (lambda (id) #f)))
             (put-signature
               (lambda (id)
                 (let ((sig (signature-of id)))
                   (when sig
                     (put-string out "  :: ")
                     (put-string out sig)))))
             (keywords-of
               (lambda (id)
                 (let* ((block (state-read state id))
                        (fields (and block (assq 'fields block)))
                        (e (and fields (assq 'keywords (cdr fields)))))
                   (and e (string? (cdr e)) (cdr e)))))
             (put-keywords
               (lambda (id)
                 (when with-keywords?
                   (let ((k (keywords-of id)))
                     ;; NEVER: A BLOCK WITHOUT THE FIELD PRINTS NO BRACKETS.
                     ;; Empty brackets would say the writer chose no
                     ;; keywords, which is a different thing from a store
                     ;; written before the field existed.
                     (when k
                       (put-string out "  [")
                       (put-string out k)
                       (put-string out "]"))))))
             (rows (state-outline state))
             (structure (state-structure state))
             (orphans (cdr (assq 'orphans structure))))
        (define (children-of parent)
          (filter (lambda (row) (equal? (car row) parent)) rows))
        ;; WHAT THE TREE ALREADY DREW. A block can be both a structural
        ;; conflict and an orphan -- a parent chain that closes on itself
        ;; through a deleted block is one -- and `state-outline` relocates
        ;; a cyclic row to the top, so the tree above has already printed
        ;; it. Printing it again under `orphans:` listed it twice, and
        ;; walking it there listed its whole subtree twice. A listing says
        ;; where each block is, once.
        (define drawn '())
        ;; A DEPTH LIMIT STOPS THE WALK, it does not filter the output: a
        ;; filtered listing still pays to render everything, and on a
        ;; corpus that is the difference between an outline and a dump.
        (define (walk parent depth)
          (for-each
            (lambda (row)
              (let ((id (caddr row))
                    ;; THE FOURTH ELEMENT NAMES A STRUCTURAL CONFLICT, and
                    ;; a row that has one has to say so. The reduction
                    ;; marks these rows and this printer used to drop the
                    ;; mark, so a block whose position never settled
                    ;; rendered exactly like an ordinary one at the top
                    ;; level -- the library knew and the command line did
                    ;; not, which is the same failure as saying half of
                    ;; something.
                    (mark (and (= 4 (length row)) (cadddr row))))
                (when (or (not depth-limit) (< depth depth-limit))
                  (set! drawn (cons id drawn))
                  (put-string out (make-string (* 2 depth) #\space))
                  (put-string out "- ")
                  (put-string out id)
                  (put-string out "  ")
                  (put-string out (title-of state id))
                  (when mark
                    (put-string out "  ")
                    (put-string out (symbol->string mark)))
                  ;; AND IT SAYS SO IF IT IS ALSO AN ORPHAN. A block whose
                  ;; parent chain closes on itself through a deleted block
                  ;; is both, and the tree draws it once -- so the row the
                  ;; tree draws is the only place left to say the second
                  ;; thing. Without this, a self-cycle and a cycle through
                  ;; a deleted ancestor print identically, and only the
                  ;; second leaves the block unreachable from any root.
                  ;; `conflicts` still reports both facts; this keeps the
                  ;; listing from being the one view that does not.
                  (when (member id orphans)
                    (put-string out "  orphan"))
                  (put-keywords id)
                  (put-signature id)
                  (put-string out "\n")
                  (walk id (+ depth 1)))))
            (children-of parent)))
        (walk 'root 0)
        ;; AN ORPHAN IS A ROOT, AND A ROOT IS WALKED. Listing the
        ;; orphan and stopping there drops everything hanging under it:
        ;; delete a block and its grandchildren leave the outline
        ;; altogether, while staying in the store and in `read`. They are
        ;; not deleted and not unreachable -- they are exactly as
        ;; reachable as their parent, which this section is printing --
        ;; so a listing that omits them tells an operator the store has
        ;; lost blocks it still holds.
        ;;
        ;; DEPTH 1, so a limit of one behaves here as it does at the top:
        ;; the orphan itself is the row at depth 0.
        ;; AND THE ORPHAN ROW OBEYS THE LIMIT THE TREE OBEYS. An orphan
        ;; is a root, so it is a row at depth 0 -- and `--depth 0` asks
        ;; for no rows at all. This section used to print its roots
        ;; through the limit while the tree above printed none of its
        ;; own, so one number meant two things in one listing.
        (let ((left (if (and depth-limit (not (< 0 depth-limit)))
                        '()
                        (filter (lambda (id) (not (member id drawn))) orphans))))
          (unless (null? left)
            (put-string out "orphans:\n")
            (for-each (lambda (id)
                        (set! drawn (cons id drawn))
                        (put-string out "- ")
                        (put-string out id)
                        (put-string out "  ")
                        (put-string out (title-of state id))
                        (put-keywords id)
                        (put-signature id)
                        (put-string out "\n")
                        (walk id 1))
                      left)))
        (get))))

  ;; ---- intents --------------------------------------------------------------

  ;; OPTION NAMES TRAVEL AS STRINGS IN A USAGE FORM. "--under" is not a
  ;; symbol an R6RS reader will accept, and the point of answering with
  ;; the shape is that an agent can read it back -- a form this library
  ;; could not itself read would be a poor thing to hand out.
  (define commit-usage
    '(commit [<block> ...] ["--writer" <name>] ["--working-version" <block>=<version>] ["--premises" <datum>]))

  (define outline-usage
    '(outline ["--depth" <n>] ["--with-keywords"] ["--with-signatures"]))

  ;; ONE SPELLING OF subscribe's FORM, reached by name from its catalogue
  ;; entry and from the refusal below. The daemon carries the verb and asks
  ;; subscribe-shape-error for the same refusal, so the form is written once.
  ;; <stream> names what is followed; `changes` is the one stream there is,
  ;; and the refusal below holds a request to it.
  (define subscribe-usage '(subscribe <stream> <rev> [<token>]))
  (define (subscribe-shape-error args)
    (and (not (and (<= 2 (length args) 3) (equal? (car args) "changes")))
         (usage subscribe-usage)))

  (define read-usage-form
    '(read <id> ["--md"] ["--recursive"] ["--writer" <name>]
           ["--working"] ["--working-info"] ["--signature"] ["--cut" <cut>] ["--rev"]))

  ;; <file> BEFORE THE OPTIONAL GROUPS: the positional slots are read off
  ;; the form up to its first group, and the daemon resolves a <file> slot
  ;; against the caller's directory only when it is one of them.
  (define supply-usage
    '(supply <kind> <file> ["--for" <writer>] ["--clear"]))

  ;; IMPORT-CODE'S FORM, written once: the catalogue and the handler's
  ;; usage answer use it.
  (define import-code-usage
    '(import-code <dir> ["--allow-delete"] ["--datum"] ["--symbols" <symbols-file>] ["--premises" <datum>]))

  ;; --under IS OPTIONAL, and its absence is root (parse-insert); the usage
  ;; says so by bracketing it like every other optional part.
  ;; LINK'S AND UNLINK'S FORMS, written once: the catalogue and `parse-edge`,
  ;; which answers both, use these.
  (define link-usage '(link <from> <rel> <to> ["--premises" <datum>]))
  (define unlink-usage '(unlink <from> <rel> <to> ["--premises" <datum>]))
  ;; THE REVIEW CHANNEL'S FORMS. scope and collect are core.sc's programs and
  ;; answer these through the catalogue; review-results and collect-into
  ;; through channel-answer below. A required option is written bare, as
  ;; insert's --title is.
  (define scope-usage '(scope <dir> "--cut" <cut> "--roots" <id> "--for" <actor>))
  (define collect-usage '(collect <dir>))
  (define review-results-usage '(review-results))
  (define collect-into-usage '(collect-into <letter-id> "--results" <datum>))

  (define relation-usage
    '(relation <name> ["--as" <kind>] ["--from" <selector>] ["--to" <selector>] ["--retire"] ["--premises" <datum>]))

  (define rule-usage
    '(rule <name> ["--on" <kind>] ["--where" <goal>] ["--must" <goal>] ["--must-not" <goal>]
           ["--builtin" <name>] ["--retire"] ["--premises" <datum>]))

  (define insert-usage
    '(insert ("--under" <id>) ("--after" <id>) "--title" <text> ("--text" <text>)
             ("--keywords" <text>) ("--premises" <datum>)))

  (define (unknown-id state id)
    (list 'error 'unknown-id id (list 'nearest (nearest-ids state id))))

  ;; ---- the facts an editor supplied: loaded on first use -----------------------
  ;;
  ;; NEVER: THIS LIBRARY DOES NOT IMPORT (theourgia derived). Every start of
  ;; the command line compiles what this library's closure holds, and the
  ;; facts' tables, freshness and answers cost about 23 ms of it from source,
  ;; measured, for verbs most starts never use. So the library is entered
  ;; here, on first use, as core.sc enters the actor system (`later`), and a
  ;; reader enters only when a table file of the kind it reads exists -- a
  ;; name in <store>/derived/, read without loading anything. A store with
  ;; no tables answers read, outline, search, refs and drafts as it did
  ;; before tables existed, and loads nothing for them.
  (define (derived name) (eval name (environment '(theourgia derived))))

  ;; ---- validity: the lifecycle provider, loaded on use --------------------------
  ;;
  ;; NEVER: THIS LIBRARY DOES NOT IMPORT (theourgia lifecycle), for the reason
  ;; above, and holds none of its code: what a read or a search says about
  ;; validity is built there. Whether a state links any relation with an
  ;; effect is the reducer's to answer, without the provider; a store that
  ;; links none reads, searches and greps as it did before, by the byte, and
  ;; loads nothing for it.
  (define (lifecycle-entry name) (eval name (environment '(theourgia lifecycle))))
  (define (lifecycle-of st) (and (state-effect-relation? st) ((lifecycle-entry 'lifecycle) st)))
  ;; The clauses the provider named builds, or none without a provider.
  (define (validity-clauses L name . args)
    (if L (apply (lifecycle-entry name) L args) '()))
  ;; THE FILTER THE SEARCH VERBS HAND THE STORE LIBRARY, a procedure of the
  ;; state it folds, so a hit and its validity come from one fold; `keep` is
  ;; handed the provider for the answer's clauses.
  (define (validity-hook keep all?)
    (lambda (st)
      (let ((L (lifecycle-of st)))
        (keep L)
        (and L ((lifecycle-entry 'search-filter) L all?)))))

  ;; NEVER: APPLYING A TEMPLATE IS ENTERED ON USE, as (theourgia derived) is
  ;; above: init answers every start that names no template without it.
  (define (template-entry name) (eval name (environment '(theourgia template))))

  ;; NEVER: THE REVIEW CHANNEL IS ENTERED ON USE, for the same reason: only
  ;; its verbs need it. Two of them are here, review-results and
  ;; collect-into; scope and collect are programs core.sc runs (their route
  ;; is `child`), never this dispatcher: each sends a store its own requests,
  ;; and one of those stores may be served by the daemon that would be
  ;; running it.
  (define (channel-entry name) (eval name (environment '(theourgia channel))))

  ;; THE USAGE FORM IS THE CATALOGUE'S, written there once: the channel
  ;; answers the bare word `usage` for a request of the wrong shape.
  (define (channel-answer name thunk)
    (guarded
      (lambda ()
        (let ((a (thunk)))
          (if (eq? a 'usage) (list 'usage (usage-form-of name)) a)))))

  ;; A new store, then the template applied to it in this process. The
  ;; answer is init's with what was applied and what it made; plain init's
  ;; answer is not touched. NEVER: AN APPLY THAT FAILS AFTER THE STORE WAS
  ;; MADE SAYS SO: the store stays, and the error carries (store-created).
  (define (init-with-template store actor req init-answer datum named)
    (let ((r ((template-entry 'template-apply!) store actor req datum)))
      (if (eq? (car r) 'ok)
          (append init-answer (list (list 'template named)) (cdr r))
          (append r (list '(store-created))))))
  (define (derived-tables? store kind)
    (let ((dir (string-append store "/derived")) (prefix (string-append (symbol->string kind) "-")))
      (and (eq? (entry-type dir) 'directory)
           (exists (lambda (n) (and (> (string-length n) (string-length prefix))
                                    (string=? prefix (substring n 0 (string-length prefix)))))
                   (directory-entries dir))
           #t)))

  ;; AN EDITOR'S KEYWORDS, as `search` consults them: from the committed
  ;; store's table, judged against the state searched, asked only when the
  ;; query has a word; #f when no table exists. One procedure, so the query
  ;; relation `score` gets exactly the hits `search` does.
  (define (search-keyword-hook store)
    (lambda (st)
      (and (derived-tables? store 'signatures)
           ((derived 'keyword-hook) store st))))

  ;; IMPORT-MD'S ANSWER, with the request's premises. Its files are read
  ;; inside the write session, before the check: a raise there is answered
  ;; by RUN (store.sc, premises-preflight) and says so.
  (define (import-md-answer store dir actor options)
    (let-values (((check finish run) (premises-preflight store (argument-option options "--premises") #f)))
      (run (lambda ()
             (let ((r (import-md-report store dir actor (argument-option options "--allow-delete") check)))
               (finish (append (list 'import (car r))
                               (filter (lambda (c) c) (cdr r)))))))))

  ;; THE DISPATCHER'S OWN HELPERS, handed to the facts' library by name, so
  ;; its verbs use these definitions rather than copies of them.
  (define (dispatch-helper name)
    (case name
      ((guarded) guarded) ((items) items) ((unknown-id) unknown-id)
      ((reduction-for) reduction-for) ((count-argument) count-argument)
      ((usage) usage) ((receipt) receipt) ((keyword-hook) search-keyword-hook)
      ((read) (cdr (assq 'read (verb-table))))
      (else (assertion-violation 'dispatch-helper "no such helper" name))))

  ;; ONE INTENT, ONE WRITE SESSION. OPTIONS are the handler's, for the
  ;; premises the request carries (store.sc, premises-preflight); CHECK is the
  ;; verb's own preflight, asked after them.
  (define (one-write store actor intent req options . check)
    (let-values (((preflight finish run) (premises-preflight store (argument-option options "--premises")
                                                             (and (pair? check) (car check)))))
      (run (lambda () (finish (car (with-store-write store (lambda (state view) (list intent)) actor req preflight)))))))

  (define (parse-insert store actor args req options)
    (let ((under (argument-option options "--under"))
          (after (argument-option options "--after"))
          (title (argument-option options "--title"))
          (text (argument-option options "--text"))
          (keywords (argument-option options "--keywords")) (rest args))
      (cond
        ((not (null? rest)) (usage insert-usage))
        ((not title) (usage insert-usage))
        (else
         ;; `--under` IS OPTIONAL AND ITS ABSENCE MEANS THE ROOT, which is
         ;; also what the word "root" means. The caller that omitted it
         ;; and the caller that spelled it get the same parent.
         (one-write store actor
                    (list 'insert
                          (if (or (not under) (string=? under "root")) 'root under)
                          after
                          ;; NEVER: STORED AS THE CALLER WROTE IT. `read` gives
                          ;; back the same bytes -- spacing, commas and
                          ;; all -- because a field that came back
                          ;; normalised would be a different string from
                          ;; the one that was sent.
                          (append (list (cons 'kind 'section) (cons 'title title))
                                  (if text (list (cons 'src text)) '())
                                  (if keywords (list (cons 'keywords keywords)) '())))
                    req options)))))

  (define (parse-set store actor args req options state)
    (let ((expect (argument-option options "--if-unchanged")) (rest args))
      (let ((intent
              (cond
                ((= 3 (length rest))
                 (list 'set (car rest) (string->symbol (cadr rest))
                       (cond
                         ;; NEVER: `kind` IS A SYMBOL FIELD AND THE COMMAND LINE
                         ;; HANDS OVER STRINGS. Everything the tree compares a
                         ;; kind against is a symbol, so `set <id> kind doc`
                         ;; stored the string "doc" and nothing ever matched it
                         ;; -- silently: the block simply never behaved like a
                         ;; doc. The conversion happens here, and an unknown
                         ;; spelling is refused BY NAME rather than stored.
                         ;; `class` IS THE SAME KIND OF FIELD (a word from a
                         ;; fixed list) AND IS CONVERTED AND REFUSED BY THE
                         ;; SAME RULE, over the reducer's one table of them.
                         ((assq (string->symbol (cadr rest)) vocabulary-fields)
                          (let ((field (string->symbol (cadr rest)))
                                (k (string->symbol (caddr rest))))
                            (if (vocabulary-known? field k)
                                k
                                ;; NEVER: THE SAME CONDITION GETS THE SAME NAME
                                ;; ON BOTH ROUTES. This said `unknown-kind`
                                ;; while the intent layer said
                                ;; `kind-not-known` -- two names I coined for
                                ;; one thing inside one change, which is how a
                                ;; reader ends up believing they are two
                                ;; things. The envelope differs because the
                                ;; layers differ; the condition does not.
                                (raise (list 'error 'bad-request
                                             (string->symbol (string-append (cadr rest) "-not-known"))
                                             (list field (caddr rest))
                                             (list 'known (cdr (assq field vocabulary-fields))))))))
                         ((and (string=? (cadr rest) "body")
                               (eq? (code-field (reduction-for store state) (car rest) 'mode) 'datum))
                          (let ((forms (datum-source-read (string->utf8 (caddr rest)))))
                            (if (= (length forms) 1) (caar forms)
                                (raise '(error bad-source (reason expected-one-form))))))
                         (else (caddr rest)))))
                ((= 2 (length rest))
                 (list 'set (car rest) (string->symbol (cadr rest))))
                (else #f))))
        (if (not intent)
            (usage '(set <id> <field> <value> ["--if-unchanged" <version>] ["--based-on" <version>] ["--premises" <datum>]))
            (one-write store actor (if expect (list 'expect expect intent) intent) req options
              (let ((h (argument-option options "--based-on")))
                (and h (lambda (state) (baseline-refusal state (car rest) h)))))))))

  (define (parse-move store actor args req options)
    (let ((after (argument-option options "--after")) (rest args))
      (if (not (= 2 (length rest)))
          (usage '(move <id> <parent> ["--after" <id>] ["--premises" <datum>]))
          (one-write store actor
                     (list 'move (car rest)
                           (if (string=? (cadr rest) "root") 'root (cadr rest))
                           after)
                     req options))))

  (define (parse-edge store actor verb args req options)
    (if (not (= 3 (length args)))
        (usage (if (eq? verb 'link) link-usage unlink-usage))
        (one-write store actor
                   (list verb (car args) (string->symbol (cadr args)) (caddr args))
                   req options)))

  ;; `relation <name> --as <kind> [--from <selector>] [--to <selector>]`, or
  ;; `relation <name> --retire`: one declaration record, whose value is the
  ;; whole declaration (reduce.sc, declared relations). A selector is
  ;; written as its clauses and read as data, `(kind doc)` or `(kind doc)
  ;; (field slot "result")`; an end not given is any block.
  (define (parse-relation store actor args req options)
    (let ((as (argument-option options "--as"))
          (from (argument-option options "--from"))
          (to (argument-option options "--to"))
          (retire (argument-option options "--retire")))
      (define (selector text)
        (if (not text)
            '()
            (guard (e (#t 'unreadable))
              (let ((in (open-string-input-port text)))
                (let loop ((out '()))
                  (let ((d (get-datum in)))
                    (if (eof-object? d) (reverse out) (loop (cons d out)))))))))
      (cond
        ((not (= 1 (length args))) (usage relation-usage))
        ((if retire (or as from to) (not as)) (usage relation-usage))
        (retire (one-write store actor (list 'relation (string->symbol (car args)) 'retired) req options))
        ((not (memq (string->symbol as) declaration-kinds))
         (list 'error 'bad-request 'kind-not-known (list 'kind as) (list 'known declaration-kinds)))
        (else
         (let ((f (selector from)) (t (selector to)))
           (if (or (eq? f 'unreadable) (eq? t 'unreadable))
               '(error malformed-intent (selector-malformed))
               (one-write store actor
                          (list 'relation (string->symbol (car args)) (list (string->symbol as) f t))
                          req options)))))))

  ;; A RULE OF THE STORE: `rule <name> --on <kind> ... [--where <goal>] --must
  ;; <goal> | --must-not <goal>`, `rule <name> --builtin <name>`, or `rule
  ;; <name> --retire`. The value is put in its one form, its class computed
  ;; from its goals, by the query library (rule-value-check), which the
  ;; store asks again when it writes; a goal that reads outside the log, an
  ;; unknown relation, a wrong arity or an unknown kind is refused here and
  ;; nothing is written. In this build a rule is stored and listed, and no
  ;; write is judged by it.
  (define (parse-rule store actor args req options)
    (let ((on (argument-option-list options "--on"))
          (where (argument-option options "--where"))
          (must (argument-option options "--must"))
          (must-not (argument-option options "--must-not"))
          (builtin (argument-option options "--builtin"))
          (retire (argument-option options "--retire")))
;; UNREADABLE IS NO DATUM a goal can read as: a goal that reads as the
      ;; symbol unreadable is checked as any goal is.
      (define unreadable (list 'unreadable))
      (define (goal text)
        (guard (e (#t unreadable))
          (let* ((in (open-string-input-port text)) (d (get-datum in)))
            (if (and (not (eof-object? d)) (eof-object? (get-datum in))) d unreadable))))
      (define (write-rule value)
        (let ((v ((eval 'rule-value-check (environment '(theourgia query))) value)))
          (if (and (pair? v) (eq? (car v) 'error))
              v
              (one-write store actor (list 'rule (string->symbol (car args)) v) req options))))
      (cond
        ((not (= 1 (length args))) (usage rule-usage))
        (retire
         (if (or (pair? on) where must must-not builtin)
             (usage rule-usage)
             (one-write store actor (list 'rule (string->symbol (car args)) 'retired) req options)))
        (builtin
         (if (or (pair? on) where must must-not)
             (usage rule-usage)
             (write-rule (list (list 'builtin (string->symbol builtin))))))
        ((or (null? on) (and must must-not) (not (or must must-not))) (usage rule-usage))
        (else
         ;; A given --where is checked whatever it reads as, #f and the
         ;; symbol absent among them; only an absent one adds no clause.
         (let ((w (and where (goal where))) (g (goal (or must must-not))))
           (if (or (eq? w unreadable) (eq? g unreadable))
               '(error bad-request (reason goal-unreadable))
               (write-rule (append (list (cons 'on (map string->symbol on)))
                                   (if where (list (list 'where w)) '())
                                   (list (list (if must 'must 'must-not) g))))))))))

  ;; A BATCH IS ONE WRITE SESSION. Read as data by whoever holds the
  ;; bytes, handed here as a list of intents; running them one at a time
  ;; through separate sessions would let another writer interleave, and
  ;; the back-references `(from n)` would then point into a history the
  ;; author did not have.
  (define (run-batch store actor items req options)
    ;; the caller knows whether this request had any sub-operations at all;
    ;; the answer cannot be told from its shape
    (let-values (((preflight finish run) (premises-preflight store (argument-option options "--premises") #f)))
      (run (lambda ()
             (finish (batch-answer (with-store-write store (lambda (state view) items) actor req preflight)
                                   (null? items)))))))

  (define (parse-batch store actor args req options)
    (if (not (= 1 (length args)))
        (usage '(batch <intents> ["--premises" <datum>]))
        (let ((data (guard (e (#t 'unreadable))
                      (let ((in (open-string-input-port (car args))))
                        (let loop ((out '()))
                          (let ((d (get-datum in)))
                            (if (eof-object? d) (reverse out) (loop (cons d out)))))))))
          (if (eq? data 'unreadable)
              (list 'error 'bad-request 'unreadable-intents)
              ;; ONE WRAPPING LIST OR SEVERAL TOP-LEVEL FORMS, both
              ;; accepted: an author writing a batch by hand brackets it,
              ;; and a program emitting one form per line does not. The
              ;; two are told apart by whether the single datum's first
              ;; element is itself a list, which an intent never is.
              (run-batch store actor
                         (if (and (= 1 (length data)) (list? (car data))
                                  (pair? (car data)) (list? (car (car data))))
                             (car data)
                             data)
                         req options)))))

  ;; ---- the verbs ------------------------------------------------------------

  ;; THE TABLE IS THE LIST OF VERBS. `eval` is not in it -- it is not
  ;; removed from it by a second rule somewhere, which is how a verb
  ;; comes back by accident -- so an unknown tag and a deliberately
  ;; absent one are the same answer, and there is nowhere to forget.
  ;; NEVER: THE SPELLING IS ADVERTISED WHERE IT IS REFUSED, AND IT IS WRITTEN
  ;; ONCE. `eval` is not a dispatcher verb, so no handler here refuses it;
  ;; its refusals are `core.sc`'s, which imports this form, and the
  ;; catalogue publishes the same form so that a caller who has only
  ;; `describe` can see what eval takes. `options-gate.sc` reads it as data
  ;; and checks every option in it against `parse-arguments`, in both
  ;; directions.
  ;;
  ;; NOTE: ONLY VERB-SPECIFIC OPTIONS BELONG HERE. `--store`, `--wire`,
  ;; `--actor`, `--req`, `--cursor` and `--socket` are accepted for every
  ;; verb by the common part of the table, and listing them in one usage
  ;; form would suggest they are special to it.
  (define eval-usage
    '(eval ["--lang" <language>] ["--cut" <cut>] ["--under" <library-id>] ["--working"] ["--latest"]
           ["--writer" <name>] ["--timeout-ms" <n>] ["--memory-bytes" <n>]
           ["--output-bytes" <n>] <source>))

  ;; ---- the catalogue -------------------------------------------------------
  ;;
  ;; KEY: WHAT THE VERBS ARE, FOR SOMETHING THAT HAS TO ASK. The MCP shell
  ;; builds its tool list from this, and a caller can read it with
  ;; `describe`; both get it from here rather than each keeping a copy,
  ;; because a tool description that has drifted from the verb it
  ;; describes is worse than none -- it is read and believed.
  ;;
  ;; NEVER: IT IS DATA AND IT RUNS NOTHING. Asking what the verbs are may not
  ;; open the store, take a lock or write a byte.
  ;;
  ;; NOTE: AND IT IS A SECOND PLACE THAT KNOWS THE VERBS. The dispatcher is
  ;; the first. A verb added there and not here would be callable and
  ;; undocumented; one here and not there would be advertised and
  ;; missing. Nothing in the language stops either, so `describe.sc` has
  ;; a row that compares the two lists in both directions -- that row is
  ;; the only thing holding this table honest. The one exception is an
  ;; entry whose route is `child`: it is carried out by a program the
  ;; caller runs, never by the dispatcher, so it is here and not there.
  ;;
  ;; KEY: THE FIFTH FIELD SAYS WHO CARRIES THE VERB OUT. `daemon` means a
  ;; client sends it over the socket; `local` means the client runs the
  ;; server in its own process instead, because there is nothing to send
  ;; it to yet or because the work has to be this process's child.
  ;; `child` means an MCP shell runs `core.sc` as its own child and reads
  ;; the answer, and the command line execs it, as for `local`: the work
  ;; has to be a process's child, and the shell is a process that can have
  ;; one and still answer the next call.
  ;; `stream` means a client that keeps the connection open carries it out:
  ;; the thin client sends it to the daemon and prints what arrives until
  ;; the stream ends. The MCP shell, which is request and response, does not
  ;; offer it.
  ;;
  ;; NEVER: IT EXISTS BECAUSE A LIST THAT ONLY THE CLIENT KNEW WAS WRONG FOR
  ;; THE SHELL. `init` is what CREATES a store, so there is no daemon for
  ;; it to reach; the MCP shell has no local route at all, so it offered
  ;; `theourgia_init` as a tool, sent it to a daemon for a store that did
  ;; not exist, and the daemon could not start. Measured: the shell can
  ;; never create a store. The shell now lists only what it can actually
  ;; carry out, and a host creates the store once before starting it.
  ;;
    ;; NOTE: THE FOURTH FIELD IS CALLED `protocol`, AND THE NAME IS THE
  ;; POINT. It says "this verb's description carries the writing protocol
  ;; text", which is a fact about what agents are told. It was first
  ;; called `writes`, meaning "changes the store" -- and under that name
  ;; `set` belongs in it, because `set <id> src <text>` certainly does
  ;; change the store. The name invited a verb in that the specification
  ;; had not put there. "Does it change the store" is a real question and
  ;; this table is not the place that answers it: nothing consumes such
  ;; an answer.
  (define (verb-catalogue) (append (built-in-catalogue) (extension-catalogue)))

  (define (built-in-catalogue)
    (list
      (list 'init '(init ["--template" <name>] ["--template-file" <template-file>])
            "Create a store in this directory; with --template (a built-in such as project) or --template-file, then apply that template to it." #f 'local)
      (list 'eval eval-usage
            "Evaluate source against the store, or against a writer's working view with --working: Scheme by default, another language with --lang, whose runner runs only where the operator has set THEOURGIA_RUNNERS=on. It runs as a child process of the caller, never in the store's server." #f 'child)
      (list 'insert insert-usage
            "Add a block under a parent, with a title and optional text." #t 'daemon)
      ;; NOTE: NOT MARKED, ALTHOUGH `set <id> src <text>` DOES PUT PROSE IN.
      ;; §7.6.45 names the two verbs the protocol is attached to, and
      ;; this is not one of them. It is left as the specification has it
      ;; rather than widened here, because the mark is what the MCP tool
      ;; descriptions are built from and widening it silently would
      ;; change what agents are told without anyone deciding to.
      (list 'set '(set <id> <field> <value> ["--if-unchanged" <version>] ["--based-on" <version>] ["--premises" <datum>])
            "Replace one field of one block." #f 'daemon)
      (list 'move '(move <id> <parent> ["--after" <id>] ["--premises" <datum>])
            "Move a block to another parent, optionally after a sibling." #f 'daemon)
      (list 'del '(del <id> ["--premises" <datum>])
            "Retire a block. Its history stays." #f 'daemon)
      (list 'link link-usage
            "Record a named relation between two blocks." #f 'daemon)
      (list 'unlink unlink-usage
            "Remove a named relation between two blocks." #f 'daemon)
      (list 'relation relation-usage
            "Declare what a relation name does: --as one of the six relations with an effect (supersedes, refutes, depends-on, implements, verifies, conflicts-with), whose rules its edges then follow, or nothing, a listed edge with no effect. --from and --to name the blocks each end is for, stored and listed, not enforced. With --retire the name is a plain edge again. The same declaration again answers (ok (unchanged)) and writes nothing."
            #f 'daemon)
      (list 'rule rule-usage
            "Declare a rule of the store: on writes to blocks of the --on kinds (repeated for several), selected by --where when given, the --must goal must have a row for the block, or the --must-not goal none; ?w stands for the block. --builtin names a built-in rule (citation-coverage); --retire ends a rule. A goal is a query goal over the stored facts; one that reads outside the log is refused. In this build a rule is stored, listed by check and conflicts, and judges no write. The same rule again answers (ok (unchanged)) and writes nothing."
            #f 'daemon)
      ;; NEVER: AND THIS ENTRY WAS THREE OPTIONS SHORT OF THE HANDLER'S OWN
      ;; SPELLING. It named `--writer`, `--based-on` and `--rebase` while the
      ;; handler's refusal named `--working-cut`, `--working-parent-writer`
      ;; and `--working-parent` as well -- all three accepted by the parser.
      ;; NEVER: AND THE REASON THE OPTIONS GATE DID NOT SEE IT IS NOT THAT
      ;; IT UNIONED THE TWO SPELLINGS -- which is what this comment said
      ;; when it was written. The gate's `usage-forms` is the `(usage ...)`
      ;; call sites appended to the `<verb>-usage` definitions, and the verb
      ;; table is in neither: it never read the catalogue at all. The union
      ;; is real, and it is a union of call sites with named definitions.
      ;;
      ;; The difference decides where the repair goes. "I read two places
      ;; and merged them" asks for a better comparison; "there is a place I
      ;; never read" asks for a wider census. The first story was the one
      ;; acted on, and it produced a comparison of the right shape whose
      ;; reach was still short.
      ;;
      ;; It was not only a documentation gap: this entry is what `describe`
      ;; publishes, and `usage-form-of` reads it for `under-cwd-nodes`, which
      ;; rewrites the arguments a form calls paths against the caller's
      ;; directory. None of the three is a path today, so nothing was
      ;; computed wrongly -- but the next path-valued option written into a
      ;; handler and left out of here would be rewritten by nothing, in
      ;; silence.
      (list 'write '(write <block> <bytes> ["--writer" <name>] ["--based-on" <version>]
                           ["--working-cut" <cut>] ["--working-parent-writer" <name>]
                           ["--working-parent" <version>] ["--rebase"])
            "Save a draft of a block in a writer's own space, without committing it." #t 'daemon)
      (list 'restore '(restore <version> ["--writer" <name>])
            "Take an earlier version of a draft back into a writer's space." #f 'daemon)
      (list 'commit commit-usage
            "Install a writer's drafts into the store as one change." #f 'daemon)
      (list 'drafts '(drafts ["--writer" <name>])
            "List the drafts a writer is holding." #f 'daemon)
      (list 'diagnostics '(diagnostics ["--writer" <name>])
            "List the diagnostics an editor supplied for a writer's working view, by block and then by start, each at a byte range of its block's own src; one whose range cannot be mapped onto a single block's src is listed with (at unmappable)." #f 'daemon)
      (list 'discard '(discard <block> ["--writer" <name>])
            "Throw away a writer's draft of a block." #f 'daemon)
      (list 'batch '(batch <intents> ["--premises" <datum>])
            "Carry out several changes as one request." #f 'daemon)
      (list 'split-suggest '(split-suggest <file> ["--output" <review-file>] ["--symbols" <symbols-file>])
            "Propose where a long file could be divided into blocks. With --symbols, the cuts come from an editor's list of the file's top-level symbols instead of the language's definition patterns; the answer's cuts-from says which." #f 'daemon)
      (list 'import-code import-code-usage
            "Read a directory of source into the store. With --datum, the whole-line ; comments directly above a form become its doc; a ; comment inside a form is dropped, and the answer warns with its line and column. A #| |# block comment, and any comment inside a datum discarded with #;, is dropped with neither. With --datum, only Scheme files are read: those the language table gives to Scheme by extension (ss, sc, scm, sls, matched exactly); every other file the directory walk returns (it does not enter a name that starts with a dot) is listed, in the order it was walked, in the answer's skipped clause, which is there only when something was skipped. A file the reader refuses is named in the refusal's path clause. Text mode, without --datum, skips a file that is not UTF-8 text or that holds a NUL byte, and lists it in the same skipped clause; it is decided by the bytes, not the name, so a source file in a legacy 8-bit encoding or in UTF-16 is skipped and listed, not imported, unless its bytes happen to be valid UTF-8 with no NUL. With --symbols, a file an editor's symbols file names is split on its first import at those symbols' ranges, every start and every end a cut, as a marked import of the same cuts would split it; a file that already carries markers follows them, and the answer lists it as symbols-ignored. A file whose cuts the scanner does not see at the top level is not imported, and the answer lists it with its refusal as symbols-refused."
            #f 'daemon)
      (list 'export-code '(export-code <dir> ["--raw"] ["--datum"] ["--working"] ["--writer" <name>])
            "Write the store's source back out to a directory." #f 'daemon)
      (list 'def '(def <name> ["--under" <library>] <source> ["--premises" <datum>])
            "Define or replace one named definition." #f 'daemon)
      (list 'import-md '(import-md <dir> ["--allow-delete"] ["--premises" <datum>])
            "Read a directory of markdown into the store." #f 'daemon)
      (list 'export-md '(export-md <dir> ["--with-ids"] ["--working"] ["--writer" <name>])
            "Write the store out as markdown." #f 'daemon)
      (list 'supply supply-usage
            "Keep facts an editor computed from an export-code projection -- signatures, calls, diagnostics -- in the store's directory under derived/, outside the event log. The header carries the digests of the projected files it was computed from; every file it lists is checked against the store's own re-projection, of the committed state or, with --for, of that writer's working view. A fact is used only while the blocks it depends on still project as they did. With --clear, the table the header names is removed."
            #f 'daemon)
      (list 'adopt '(adopt)
            "Take in records that are on disk but not yet in the log." #f 'daemon)
      (list 'check '(check)
            "Read the whole store and report whether it is sound." #f 'daemon)
      (list 'snapshot '(snapshot)
            "Record the current state as a reduction that can be reopened quickly." #f 'daemon)
      (list 'publish '(publish <writer> <segment> <file> [<sha256>])
            "Publish a segment of a writer's log." #f 'daemon)
      (list 'outline outline-usage
            "List the blocks as a tree of titles." #f 'daemon)
      (list 'read read-usage-form
            "Read one block: its fields, or its text. With --signature, the signature an editor supplied for it, of the committed store or, with --working, of the writer's working view. With --cut, the block as it was at a causal cut (a literal or a tag name): the same answer over that cut's state, or unknown-id where the block did not exist yet." #f 'daemon)
      (list 'refs '(refs <id>)
            "List the relations a block takes part in." #f 'daemon)
      (list 'reach '(reach <id> ["--rel" <rel>] ["--depth" <n>])
            "List the blocks a block reaches over the edges an editor supplied (calls, by default), outward, up to --depth hops (1 by default); the block itself is at depth 0." #f 'daemon)
      (list 'search '(search <query> ["--all"] ["--all-validity"])
            "Find blocks whose title, keywords or text match every word given." #f 'daemon)
      (list 'grep '(grep <pattern> ["--under" <id>] ["--all"] ["--all-validity"])
            "List the lines that contain a pattern, literally." #f 'daemon)
      (list 'whereis '(whereis <name> ["--all-validity"])
            "Say where a name is defined, and which libraries carry it." #f 'daemon)
      (list 'log '(log [<id>])
            "Show the changes recorded, for the store or for one block." #f 'daemon)
      (list 'tag '(tag [<name>] ["--premises" <datum>])
            "Name the current cut, or list the names already given." #f 'daemon)
      (list 'diff '(diff <cut> <cut>)
            "Report what changed between two cuts." #f 'daemon)
      (list 'conflicts '(conflicts)
            "List blocks whose writers disagree." #f 'daemon)
      (list 'subscribe subscribe-usage
            "Follow the store's publications: <stream> is changes, and every publication after <rev> arrives as a line naming what changed, until the stream ends. 0 starts from now; a resume names the daemon token its acceptance gave." #f 'stream)
      (list 'scope scope-usage
            "Make a new store at <dir> holding a letter to <actor> and a copy of each block under the roots as it was at the cut, each copy carrying origin (the block's id here) and origin-cut; the edges between copies are copied, an edge leaving the roots is dropped and listed, and a contested field is listed and not copied. The letter copy's baseline is the new store's cut after the copies. --roots is given once per root. One record is written here: the letter, at the top level, with to, status, cut, roots and scope. An existing <dir> is refused before anything is written, and so is a block or edge the write would refuse. It runs in the caller's own program -- the command line's, or the MCP shell's child -- never in a store's server." #f 'child)
      (list 'collect collect-usage
            "Bring a scoped store's review results back under its letter here: the blocks the letter's reviewer created in <dir>, with their settled fields, under the reviewer's name, and their about edges, to the origin of a copy or to the collected block. A second collect writes nothing new. It runs in the caller's own program -- the command line's, or the MCP shell's child -- never in a store's server." #f 'child)
      (list 'review-results review-results-usage
            "In a scoped store, list the review results as one datum: each live block the letter's reviewer created that is not a copy, with its parent, settled fields and the actor of each, and its about targets; a finding with no about and a verdict missing a condition field are listed." #f 'daemon)
      (list 'collect-into collect-into-usage
            "Write a review-results datum under a letter: the blocks not collected before, then the about edges not present, as the reviewer's records. Answers how many were inserted, linked and already linked, and every field another actor set." #f 'daemon)
      (list 'describe '(describe)
            "List the verbs, what each is for, and the writing protocol." #f 'daemon)))

  ;; ---- verbs registered from outside the core ---------------------------------
  ;;
  ;; KEY: A VERB THE CORE DOES NOT KNOW IS ADDED AS DATA, not by editing the
  ;; tables above. An entry is (verb usage description protocol? route
  ;; value-options flag-options (library . name)): the first five project
  ;; into the catalogue, so describe and the MCP shell's tools see it as they
  ;; see a built-in; the options go to the parser (set-extension-options!);
  ;; and the verb dispatches to the handler named, with the same
  ;; eight-argument contract as every built-in handler.
  ;;
  ;; NEVER: REGISTERING DOES NOT LOAD THE HANDLER'S LIBRARY. The handler is a
  ;; name, entered on dispatch as (theourgia derived) is above, so a start
  ;; that answers another verb compiles nothing of it.
  ;;
  ;; NEVER: A BATCH IS CHECKED WHOLE BEFORE ANYTHING IS PUBLISHED. A name
  ;; that is a built-in verb, a name given twice in the batch or already
  ;; registered, a route other than daemon, or an entry of another shape
  ;; refuses the whole batch, by name, and leaves the registry as it was: a
  ;; half-registered batch would answer for some of its verbs and not others.
  ;;
  ;; NEVER: DAEMON ONLY, AND NOT BECAUSE THE CATALOGUE HAS NO OTHERS. The
  ;; carriers of the other two do not read this registry: the client decides
  ;; what it runs locally from a list of its own, and the MCP shell's child
  ;; route always runs `eval`. A registered local or child verb would be sent
  ;; where it cannot be answered, so it is refused until a carrier reads the
  ;; registry.
  (define extensions '())

  (define (extension-catalogue)
    (map (lambda (e) (list (list-ref e 0) (list-ref e 1) (list-ref e 2) (list-ref e 3) (list-ref e 4)))
         extensions))

  (define extension-routes '(daemon))

  (define (option-names? x)
    (and (list? x)
         (for-all (lambda (o) (and (string? o) (> (string-length o) 2)
                                   (char=? #\- (string-ref o 0)) (char=? #\- (string-ref o 1))))
                  x)))

  (define (entry-problem e)
    (cond ((not (and (list? e) (memv (length e) '(8 9)))) "an entry is (verb usage description protocol? route value-options flag-options (library . name) [declaration])")
          ((not (symbol? (list-ref e 0))) "the verb is not a symbol")
          ((not (pair? (list-ref e 1))) "the usage is not a form")
          ((not (string? (list-ref e 2))) "the description is not a string")
          ((not (boolean? (list-ref e 3))) "the protocol mark is not a boolean")
          ((not (memq (list-ref e 4) extension-routes)) "a registered verb's route is daemon")
          ((not (option-names? (list-ref e 5))) "the value options are not option names")
          ((not (option-names? (list-ref e 6))) "the flag options are not option names")
          ((not (let ((h (list-ref e 7))) (and (pair? h) (list? (car h)) (pair? (car h)) (symbol? (cdr h)))))
           "the handler is not (library . name)")
          ((and (= 9 (length e)) (not (declaration? (list-ref e 8))))
           "the declaration is not accept, refuse, or (refuse <action> ...)")
          (else #f)))

  ;; A REGISTERED VERB'S DECLARATION, the ninth field and optional: `accept`
  ;; (the default, what an entry without it means) answers from a load that
  ;; could not read a writer and says so; `refuse` refuses such a load, as the
  ;; built-in undeclared verbs do; `(refuse <action> ...)` refuses it only when
  ;; the first argument is one of those actions, for a verb whose actions
  ;; differ in whether they write from the reduction.
  (define (declaration? d)
    (or (memq d '(accept refuse))
        (and (list? d) (pair? d) (eq? (car d) 'refuse) (pair? (cdr d)) (for-all string? (cdr d)))))

  (define (register-verbs! batch)
    (unless (list? batch)
      (assertion-violation 'register-verbs! "a batch is a list of entries" batch))
    (for-each (lambda (e)
                (let ((problem (entry-problem e)))
                  (when problem (assertion-violation 'register-verbs! problem e))))
              batch)
    (let ((built-in (append (map car verbs) (map car (built-in-catalogue))))
          (earlier (map car extensions)))
      (let loop ((es batch) (seen '()))
        (unless (null? es)
          (let ((name (car (car es))))
            (cond ((memq name built-in)
                   (assertion-violation 'register-verbs! "the name is a built-in verb" name))
                  ((memq name earlier)
                   (assertion-violation 'register-verbs! "the name is already registered" name))
                  ((memq name seen)
                   (assertion-violation 'register-verbs! "the name is given twice in the batch" name))
                  (else (loop (cdr es) (cons name seen))))))))
    (set! extensions (append extensions batch))
    (set-extension-options!
      (map (lambda (e) (list (list-ref e 0) (list-ref e 5) (list-ref e 6))) extensions)))

  ;; (verb . handler) for a registered verb, or #f; the handler's library is
  ;; entered here, on the dispatch that needs it.
  (define (extension-dispatch-entry verb)
    (let ((e (find (lambda (e) (eq? (car e) verb)) extensions)))
      (and e
           (let ((h (list-ref e 7)))
             (cons verb
                   (lambda arguments
                     (apply (eval (cdr h) (environment (car h))) arguments)))))))

  ;; THE STORE'S TEMPLATE, AS DESCRIBE ADDS IT: its roots and its relations,
  ;; or #f.
  ;;
  ;; NEVER: DESCRIBE DOES NOT OPEN THE STORE, take a lock or write a byte, on any
  ;; route (test/describe.sc DS-2c). So the clause comes only from a state the
  ;; verb is HANDED -- the daemon's published reduction, which the daemon
  ;; already holds -- and with none there is no clause: the in-process route
  ;; does not load a store to describe it. A handed state that could not read
  ;; every writer adds nothing either: a template read from part of a store is
  ;; not the store's template.
  ;; A SEALED STATE'S NOTES ARE ALL OF ITS NOTES -- the reduction's own and
  ;; what the daemon's probe found since -- so a sealed state is asked for
  ;; those, and a bare one for its own.
;; THE DECLARED TABLE, from the same state and under the same rule as the
  ;; template above: a state handed in, and complete; never a store opened.
  ;; -> (declared-relations (<name> <kind> [(from <selector>)] [(to
  ;; <selector>)]) ...), a selector only when it is given and a contested
  ;; name as (<name> (contested)); #f when the table is empty, so describe
  ;; reads as it did for a store with no declaration.
  (define (describe-declared state)
    (let* ((view (cond ((sealed-state? state) (sealed-state-state state))
                       (else state)))
           (complete (cond ((sealed-state? state) (null? (sealed-state-notes state)))
                           (view (null? (unreadable-behind view)))
                           (else #f)))
           (table (if (and view complete (reduction? view)) (state-declared-relations view) '())))
      (and (pair? table)
           (cons 'declared-relations
                 (map (lambda (d)
                        (if (symbol? (cadr d))
                            (append (list (car d) (cadr d))
                                    (filter (lambda (c) (pair? (cadr c))) (cddr d)))
                            d))
                      table)))))

;; THE RULES IN FORCE, under the same rule as the declared table: ->
  ;; (declared-rules (<name> <value>) ...), or #f when there is none, so
  ;; describe reads as it did for a store with no rule.
  (define (describe-rules state)
    (let* ((view (cond ((sealed-state? state) (sealed-state-state state))
                       (else state)))
           (complete (cond ((sealed-state? state) (null? (sealed-state-notes state)))
                           (view (null? (unreadable-behind view)))
                           (else #f)))
           (table (if (and view complete (reduction? view)) (state-declared-rules view) '())))
      (and (pair? table) (cons 'declared-rules table))))

  (define (describe-template state)
    (let* ((view (cond ((sealed-state? state) (sealed-state-state state))
                       (else state)))
           (complete (cond ((sealed-state? state) (null? (sealed-state-notes state)))
                           (view (null? (unreadable-behind view)))
                           (else #f)))
           (t (and view complete (reduction? view) (store-template view))))
      (and t (list 'template (cons 'roots (template-roots t)) (cons 'relations (template-relations t))))))

  ;; The catalogue as an answer. NEVER: The protocol is carried ONCE, beside
  ;; the verbs, rather than repeated into each entry that needs it: the
  ;; entries say WHETHER it applies to them, and a reader that wants the
  ;; text reads it from the one place it is written.
  (define (describe-answer)
    (list 'ok
          (cons 'verbs
                (map (lambda (entry)
                       (list (car entry)
                             (list 'usage (cadr entry))
                             (list 'description (caddr entry))
                             (list 'protocol (cadddr entry))
                             (list 'route (list-ref entry 4))))
                     (verb-catalogue)))
          (list 'protocol write-protocol)))

  ;; The ordinary edit distance, bounded so a long name cannot be "near"
  ;; everything: anything further than a few edits is not a typo.
  (define (edit-distance a b)
    (let* ((n (string-length a)) (m (string-length b)))
      (if (> (abs (- n m)) 2)
          99
          (let ((prev (make-vector (+ m 1) 0)) (cur (make-vector (+ m 1) 0)))
            (let init ((j 0)) (when (<= j m) (vector-set! prev j j) (init (+ j 1))))
            (let rows ((i 1))
              (if (> i n)
                  (vector-ref prev m)
                  (begin
                    (vector-set! cur 0 i)
                    (let cols ((j 1))
                      (when (<= j m)
                        (vector-set! cur j
                                     (min (+ 1 (vector-ref cur (- j 1)))
                                          (+ 1 (vector-ref prev j))
                                          (+ (vector-ref prev (- j 1))
                                             (if (char=? (string-ref a (- i 1))
                                                         (string-ref b (- j 1)))
                                                 0 1))))
                        (cols (+ j 1))))
                    (let copy ((j 0))
                      (when (<= j m)
                        (vector-set! prev j (vector-ref cur j))
                        (copy (+ j 1))))
                    (rows (+ i 1)))))))))

  ;; NEVER: AND A REFUSAL POINTS SOMEWHERE. A name that is not there is the
  ;; common case -- a typo, a name from another version, a name the reader
  ;; half remembers -- so the refusal carries the closest names it knows, at
  ;; most five.
  ;;
  ;; THREE KINDS OF CLOSE, IN THIS ORDER: a shared prefix, which is what a
  ;; half-remembered name looks like; then plain containment; then a small
  ;; edit distance, which is what a TYPO looks like. Measured while writing
  ;; the cell for this: asking for `reaper-star` in a store holding
  ;; `reaper-start` and `reaper-stop` offered only the first, because the
  ;; second shares no prefix and is not contained -- and it is exactly the
  ;; name a reader who typed `reaper-star` might have meant.
  (define (nearest-names index wanted)
    (let* ((all (vector->list (hashtable-keys index)))
           (prefix (filter (lambda (k)
                             (and (>= (string-length k) (string-length wanted))
                                  (string=? (substring k 0 (string-length wanted)) wanted)))
                           all))
           (inside (filter (lambda (k)
                             (and (not (member k prefix))
                                  (let ((n (string-length k)) (m (string-length wanted)))
                                    (and (<= m n)
                                         (let loop ((i 0))
                                           (cond ((> (+ i m) n) #f)
                                                 ((string=? (substring k i (+ i m)) wanted) #t)
                                                 (else (loop (+ i 1)))))))))
                           all))
           (near (filter (lambda (k)
                           (and (not (member k prefix))
                                (not (member k inside))
                                (<= (edit-distance k wanted) 2)))
                         all))
           (ranked (append (list-sort string<? prefix)
                           (list-sort string<? inside)
                           (list-sort string<? near))))
      (let take ((l ranked) (n 0) (out '()))
        (if (or (null? l) (= n 5))
            (map string->symbol (reverse out))
            (take (cdr l) (+ n 1) (cons (car l) out))))))

  (define (verb-table)
    (list
      (cons 'describe
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(describe))
                  (let ((t (describe-template state)) (d (describe-declared state)) (r (describe-rules state)))
                    (append (describe-answer) (if t (list t) '()) (if d (list d) '()) (if r (list r) '()))))))
      (cons 'init
            (lambda (store actor args req options state writer cwd)
              (let ((name (argument-option options "--template"))
                    (file (argument-option options "--template-file")))
                (if (or (not (null? args)) (and name file))
                    (usage '(init ["--template" <name>] ["--template-file" <template-file>]))
                    (guarded (lambda ()
                               ;; THE TEMPLATE IS READ BEFORE THE STORE IS MADE, so a
                               ;; name or file that cannot be used refuses without
                               ;; leaving a new store behind.
                               (let ((d (and (or name file) ((template-entry 'template-datum-for) name file))))
                                 (if (and d (not (eq? (car d) 'ok)))
                                     d
                                     (let ((a (store-init! store)))
                                       (cond
                                         ((not (eq? (car a) 'ok)) (cons 'error (cdr a)))
                                         ((not d) a)
                                         (else (init-with-template store actor req a (cadr d)
                                                                   (if file (list 'file file) name)))))))))))))
      (cons 'insert (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-insert store actor args req options)))))
      (cons 'set (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-set store actor args req options state)))))
      (cons 'move (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-move store actor args req options)))))
      (cons 'del
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(del <id> ["--premises" <datum>]))
                  (guarded (lambda () (one-write store actor (list 'del (car args)) req options))))))
      (cons 'link (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-edge store actor 'link args req options)))))
      (cons 'unlink (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-edge store actor 'unlink args req options)))))
      (cons 'relation (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-relation store actor args req options)))))
      (cons 'rule (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-rule store actor args req options)))))
      (cons 'write
            (lambda (store actor args req options state writer cwd)
              (if (= 2 (length args))
                  (working-write! store state writer
                                  (car args) (cadr args) (argument-option options "--rebase")
                                  (argument-option options "--based-on") (argument-option options "--working-cut")
                                  (argument-option options "--working-parent-writer") (argument-option options "--working-parent"))
                  (usage '(write <block> <bytes> ["--writer" <name>] ["--based-on" <version>]
                     ["--working-cut" <cut>] ["--working-parent-writer" <name>]
                     ["--working-parent" <version>] ["--rebase"])))))
      ;; `write --restore <version>` IS ITS OWN SHAPE, not `write` with a
      ;; flag: it takes a version and no bytes, and the bytes come from
      ;; the log. Folding it into `write` would make the two-argument
      ;; check above answer for a call that has one.
      (cons 'restore
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (working-restore! store state writer (car args))
                  (usage '(restore <version> ["--writer" <name>])))))
      ;; NEVER: COMMIT ADVERTISES ADDITIVELY, BECAUSE IT HAS NO SHAPE TO GET
      ;; WRONG. Every other verb answers a usage form when its arity or
      ;; its positionals are wrong; `commit` accepts any number of block
      ;; ids, including none, so there is no such moment -- and until
      ;; this batch a caller who misspelled `--working-version` was told
      ;; what was wrong with the value and never what the verb accepts.
      ;; The form is therefore appended to whatever refusal came back,
      ;; which leaves the refusal's own classification alone: a reader
      ;; testing `(car answer)` or its first three fields sees exactly
      ;; what it saw before.
      (cons 'commit
            (lambda (store actor args req options state writer cwd)
              (let ((answer (working-commit! store writer
                                             args actor req
                                             (argument-option-list options "--working-version")
                                             (argument-option options "--premises"))))
                (if (and (pair? answer) (eq? (car answer) 'error))
                    (append answer (list (list 'usage commit-usage)))
                    answer))))
      (cons 'drafts
            (lambda (store actor args req options state writer cwd)
              (if (null? args)
                  (let ((a (working-list store state writer)))
                    ;; the answer's own refusals, and the writer "-", before
                    ;; the derived directory is so much as listed
                    (if (and (pair? a) (eq? (car a) 'ok) (string? writer) (not (equal? writer "-"))
                             (derived-tables? store 'diagnostics))
                        ((derived 'drafts-verb) dispatch-helper store state writer a)
                        a))
                  (usage '(drafts ["--writer" <name>])))))
      (cons 'diagnostics
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(diagnostics ["--writer" <name>]))
                  ((derived 'diagnostics-verb) dispatch-helper store args options state writer))))
      (cons 'discard
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (working-discard! store writer (car args))
                  (usage '(discard <block> ["--writer" <name>])))))
      (cons 'batch (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-batch store actor args req options)))))
      (cons 'split-suggest
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (guarded (lambda () (split-suggest (car args)
                                                     (argument-option options "--output")
                                                     (argument-option options "--symbols"))))
                  (usage '(split-suggest <file> ["--output" <review-file>] ["--symbols" <symbols-file>])))))
      ;; --symbols SPLITS A TEXT IMPORT, so with --datum, which reads forms and
      ;; has no text to cut, it is refused before anything is read.
      (cons 'import-code
            (lambda (store actor args req options state writer cwd)
              (cond
                ((not (= 1 (length args))) (usage import-code-usage))
                ((and (argument-option options "--datum") (argument-option options "--symbols"))
                 '(error bad-request incompatible-import-options))
                (else
                 (guarded (lambda ()
                   (let-values (((check finish run) (premises-preflight store (argument-option options "--premises") #f)))
                     (if (argument-option options "--datum") (import-datum store (car args) actor req (list check finish run))
                         (import-code store (car args) actor req
                                      (argument-option options "--allow-delete") (list check finish run)
                                      (let ((path (argument-option options "--symbols")))
                                        (and path (cons (lambda () (read-import-symbols path)) import-symbol-cuts))))))))))))
      (cons 'export-code
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (guarded (lambda ()
                    (cond ((and (argument-option options "--datum") (argument-option options "--raw"))
                           '(error bad-request incompatible-projection-options))
                          ((argument-option options "--working")
                           (working-export store state writer
                             (lambda (view)
                               (if (argument-option options "--datum")
                                   (export-datum-view store (car args) view)
                                   (export-code-view store (car args) (argument-option options "--raw") view)))))
                          ((argument-option options "--datum") (export-datum store (car args)))
                          (else (export-code store (car args) (argument-option options "--raw"))))))
                  (usage '(export-code <dir> ["--raw"] ["--datum"] ["--working"] ["--writer" <name>])))))
      ;; THE TABLE'S WRITER IS --for, AND THE VIEW IS THAT WRITER'S: the
      ;; committed state for "-" (no --for), the writer's working view
      ;; otherwise -- the view the plugin's export-code projected.
      (cons 'supply
            (lambda (store actor args req options state writer cwd)
              (if (not (= 2 (length args)))
                  (usage supply-usage)
                  ;; #f from the facts' library: the first argument names no kind
                  (or ((derived 'supply-verb) dispatch-helper store args options state writer)
                      (usage supply-usage)))))
      (cons 'def
            (lambda (store actor args req options state writer cwd)
              (if (= (length args) 2)
                  (guarded (lambda ()
                    (let-values (((check finish run) (premises-preflight store (argument-option options "--premises") #f)))
                      (def-datum store (car args) (argument-option options "--under") (cadr args) actor req (list check finish run)))))
                  (usage '(def <name> ["--under" <library>] <source> ["--premises" <datum>])))))
      (cons 'import-md
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(import-md <dir> ["--allow-delete"] ["--premises" <datum>]))
                  ;; THE RESULTS STAY THE SECOND ELEMENT, judged one by one
                  ;; as before; what the directory lacked, or what was
                  ;; deleted, is a clause beside them, there only when
                  ;; there is something to say.
                  (guarded (lambda () (import-md-answer store (car args) actor options))))))
      (cons 'export-md
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(export-md <dir> ["--with-ids"] ["--working"] ["--writer" <name>]))
                  (guarded (lambda ()
                    (if (argument-option options "--working")
                        (working-export store state writer
                          (lambda (view) (export-md-view (car args) view (argument-option options "--with-ids"))))
                        (export-md store (car args) (argument-option options "--with-ids"))))))))
      (cons 'adopt
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(adopt))
                  (guarded (lambda ()
                             (let ((a (store-adopt! store)))
                               (if (eq? (car a) 'adopted)
                                   (cons 'ok (cdr a))
                                   (cons 'error (cdr a)))))))))
      (cons 'check
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(check))
                  (guarded (lambda () (store-check store))))))
      (cons 'snapshot
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(snapshot))
                  (guarded (lambda ()
                             (let ((a (store-snapshot! store actor)))
                               (if (eq? (car a) 'written)
                                   (list 'ok (list 'snapshot (cadr a)) (list 'cut (caddr a)))
                                   (cons 'error (cdr a)))))))))
      (cons 'publish
            (lambda (store actor args req options state writer cwd)
              (let ((form '(publish <writer> <segment> <file> [<sha256>])))
                (if (not (or (= 3 (length args)) (= 4 (length args))))
                    (usage form)
                    (let ((segment (count-argument (cadr args))))
                      (if (not (and segment (> segment 0)))
                          (usage form)
                          (guarded
                            (lambda ()
                              ;; NEVER: A CANDIDATE THAT CANNOT BE READ IS NOT A
                              ;; CANDIDATE THAT IS NOT THERE (U9, F77b). R1's read:
                              ;; ENOENT and ENOTDIR are no-candidate as before,
                              ;; and any other failure is candidate-unreadable
                              ;; with the path and the system's reason.
                              (let* ((path (caddr args))
                                     (bytes (guard (e ((unreadable-entry? e)
                                                       (list 'unreadable (unreadable-entry-path e)
                                                             (unreadable-entry-reason e))))
                                              (let ((b (read-entry path)))
                                                (and (not (eq? b 'absent)) b)))))
                                (cond
                                  ((not bytes)
                                   (list 'error 'no-candidate (list 'path path)))
                                  ((pair? bytes)
                                   (list 'error 'candidate-unreadable
                                         (list 'path (cadr bytes)) (list 'reason (caddr bytes))))
                                  (else
                                    (let* ((sha (if (= 4 (length args))
                                                    (cadddr args)
                                                    (segment-sha bytes)))
                                           (a (log-publish! store (car args) segment bytes sha)))
                                      (if (publish-durable? a)
                                          (cons 'ok (list a))
                                          (cons 'error
                                                (if (eq? (car a) 'error) (cdr a) (list a))))))))))))))))
      (cons 'outline
            (lambda (store actor args req options state writer cwd)
              (let ((depth (argument-option options "--depth")) (rest args))
                (cond
                  ((not (null? rest)) (usage outline-usage))
                  ((and depth (not (count-argument depth))) (usage outline-usage))
                  (else
                   (let ((with-keywords (argument-option options "--with-keywords"))
                         (with-signatures (argument-option options "--with-signatures")))
                     (guarded (lambda ()
                                (let ((st (reduction-for store state)))
                                  (if (not (and with-signatures (derived-tables? store 'signatures)))
                                      (append (text (outline-text st (and depth (count-argument depth)) with-keywords))
                                              (receipt st #f))
                                      (let-values (((signature-of finish) ((derived 'outline-signatures) store st)))
                                        (let ((listing (outline-text st (and depth (count-argument depth))
                                                                     with-keywords signature-of)))
                                          (append (text listing) (finish) (receipt st #f))))))))))))))
      ;; A FILE-LEVEL BLOCK HOLDS ALMOST NOTHING. Its own `src` is the
      ;; front matter and whatever sits above the first heading, which is
      ;; usually empty -- everything a reader wants is in the sections
      ;; under it. So `read` of a document answered with an empty body
      ;; and was, for that one shape, useless; `--recursive` is what asks
      ;; for the subtree.
      ;;
      ;; THE OPTIONS ARE INDEPENDENT AND BOTH ARE STRIPPED FIRST, so
      ;; `--md --recursive` and `--recursive --md` are the same request.
      ;; An order-sensitive option list is a component whose meaning
      ;; depends on where it appears.
      ;;
      ;; A READ AT A CAUSAL CUT (`--cut`) IS THE SAME READ OVER ANOTHER STATE:
      ;; the cut is resolved and judged against the state this request holds
      ;; (store-state-at-cut), the state at the cut is one restricted replay,
      ;; and the branches below answer from it exactly as they answer from
      ;; the present. The options that ask about a writer's draft or an
      ;; editor's present-day facts have no meaning at a past cut and are
      ;; refused together, before any state is read.
      (cons 'read
            (lambda (store actor args req options state writer cwd)
              ;; THE OPTIONS THIS HANDLER JUDGES ARE READ FIRST, before the read
              ;; itself is defined: an option scanner that assigns what it finds
              ;; to the verb it last passed reads them as `read`'s.
              (let* ((at (argument-option options "--cut"))
                     (incompatible (and at (or (argument-option options "--working")
                                               (argument-option options "--working-info")
                                               (argument-option options "--writer")
                                               (argument-option options "--signature"))))
                     (rev? (argument-option options "--rev"))
                     (rev-incompatible (and rev? (or (argument-option options "--working-info")
                                                     (argument-option options "--signature")))))
                ;; THE READ ITSELF, over whatever state it is given: the present (the
                ;; request's own, or a fresh fold) or the state at a causal cut. It
                ;; is defined here, inside the handler, so the facade gate's walk of
                ;; `(items ...)` sites still finds this verb's.
                (define (read-from store args options state writer)
                  (let ((md? (argument-option options "--md"))
                        (deep? (argument-option options "--recursive")) (rest args))
                    (cond
                      ((argument-option options "--signature")
                       ((derived 'read-signature-verb) dispatch-helper store (car rest) options state writer))
                      ((or (argument-option options "--working") (argument-option options "--working-info"))
                       (if (or md? deep?) '(error bad-request incompatible-working-options)
                           (working-read store state writer (car rest) (argument-option options "--working-info"))))
                      (md?
                       (guarded
                         (lambda ()
                           ;; `state-read`, NOT `view-read`, AND THE COMMENT
                           ;; BELOW IS THE REASON. This branch answers with
                           ;; the block's own bytes -- `front`, `heading-src`
                           ;; and `src` -- every one of which is stored. It
                           ;; reads no derived field, so asking the view
                           ;; layer for it would say that it does.
                           ;;
                           ;; IT WAS `view-read` UNTIL A MUTATION SURVIVED
                           ;; HERE. Swapping this one call back left every
                           ;; cell green, which is what a call with no
                           ;; consumer looks like; the other three
                           ;; readings of the view in this file, each
                           ;; through `view-of`, kill a row.
                           (let* ((state (reduction-for store state))
                                  (b (state-read state (car rest))))
                             (cond
                               ((not b) (unknown-id state (car rest)))
                               (deep? (append (text (block-text state (car rest) #f))
                                              (receipt state (subtree-ids state (car rest)))))
                               (else
                                ;; ONE BLOCK'S OWN BYTES, which is not the
                                ;; same question and is answered from the
                                ;; block rather than from the renderer: a
                                ;; reader asking for this block is asking
                                ;; what IT says, not what its heading would
                                ;; look like if the title had since changed.
                                ;; A FIELD THAT IS NOT TEXT IS NOT TEXT. These
                                ;; three go straight into `string-append`, and
                                ;; a value that is not a string raised -- the
                                ;; caller got `(error internal ...)` for a
                                ;; record the store had applied happily. A
                                ;; field can also be UNSETTLED: two concurrent
                                ;; writes leave a conflict representation
                                ;; rather than a value, which is ordinary,
                                ;; correct data and is likewise not a string.
                                ;; So the reader asks what it has rather than
                                ;; assuming, in both cases.
                                ;;
                                ;; AND A DOCUMENT'S FRONT MATTER IS PART OF
                                ;; ITS OWN BYTES. The README says a file-level
                                ;; block's body is the front matter and
                                ;; whatever sits above the first heading; this
                                ;; read answered only the latter.
                                (let* ((fs (cdr (assq 'fields b)))
                                       (str (lambda (k)
                                              (let ((e (assq k fs)))
                                                (if (and e (string? (cdr e))) (cdr e) ""))))
                                       (front (str 'front))
                                       (head (str 'heading-src))
                                       (body (str 'src)))
                                  (append (text (string-append front head body))
                                          (receipt state (list (car rest)))))))))))
                      (deep?
                       (guarded
                         (lambda ()
                           (let* ((state (reduction-for store state))
                                  (ids (subtree-ids state (car rest))))
                             (if (not ids)
                                 (unknown-id state (car rest))
                                 ;; The items are the records a plain read gives,
                                 ;; unchanged; the versions are one clause after
                                 ;; them, in item order, so a reader of the items
                                 ;; reads what it read before.
                                 (append (items (map (lambda (id) (view-of state id)) ids))
                                         (list (list 'versions
                                                     (filter cdr (map (lambda (id) (cons id (read-version state id)))
                                                                      ids))))
                                         (validity-clauses (lifecycle-of state) 'listed-validity-clause ids)
                                         (receipt state #f '(cut))))))))
                      (else
                       (guarded
                         (lambda ()
                           (let* ((state (reduction-for store state))
                                  (b (view-of state (car rest)))
                                  (v (and b (read-version state (car rest)))))
                             (cond ((not b) (unknown-id state (car rest)))
                                   (v (append (list 'ok b (cons 'version (if (string? v) (list v) v)))
                                              (validity-clauses (lifecycle-of state) 'read-validity-clause (car rest))
                                              (receipt state (list (car rest)) '(cut versions)
                                                       (list (cons (car rest) v)))))
                                   (else (append (list 'ok b)
                                                 (validity-clauses (lifecycle-of state) 'read-validity-clause (car rest))
                                                 (receipt state (list (car rest)))))))))))))
                ;; --rev NAMES THE PUBLICATION THE ANSWER WAS BUILT FROM: the
                ;; name the daemon sealed with the state it handed here. It is
                ;; appended to an ok answer and to nothing else; a state with
                ;; no name -- the local route, a bare reduction -- has no
                ;; publication to name.
                (define (with-rev answer)
                  (let ((name (and (sealed-state? state) (sealed-state-name state))))
                    (if (and (pair? answer) (eq? (car answer) 'ok))
                        (append answer (list (list 'rev (car name) (list 'daemon (cadr name)))))
                        answer)))
                (cond
                  ((not (= 1 (length args))) (usage read-usage-form))
                  ((and rev? at) '(error bad-request incompatible-cut-options))
                  (rev-incompatible '(error bad-request incompatible-rev-options))
                  ((and rev? (not (and (sealed-state? state) (sealed-state-name state))))
                   '(error bad-request (reason no-published-state)))
                  (rev? (with-rev (read-from store args options state writer)))
                  ((not at) (read-from store args options state writer))
                  (incompatible '(error bad-request incompatible-cut-options))
                  (else
                   (guarded
                     (lambda ()
                       (let ((past (store-state-at-cut store state at)))
                         (if (reduction? past)
                             (read-from store args options past writer)
                             past)))))))))
      (cons 'refs
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(refs <id>))
                  (guarded
                    (lambda ()
                      ;; ONE STATE FOR THE ROWS, THE SUPPLIED EDGES AND THE
                      ;; RECEIPT: `store-refs` used to fold its own, so the
                      ;; cut the answer says would not have been the one its
                      ;; rows were read from.
                      (let* ((st (reduction-for store state))
                             (a (store-refs store (car args) st)))
                        (if (eq? (car a) 'ok)
                            ;; AN EDITOR'S CALLS EDGES INTO THIS BLOCK FOLLOW
                            ;; THE STORE'S OWN ROWS, their via the provenance
                            ;; that supplied them rather than a symbol.
                            (let-values (((rows clauses)
                                          (if (derived-tables? store 'calls)
                                              ((derived 'refs-supplied) store st (car args))
                                              (values '() '()))))
                              (append
                                (items (map (lambda (r)
                                              (list 'ref (list 'from (car r))
                                                    (list 'rel (cadr r))
                                                    (list 'via (caddr r))))
                                            (append (cadr a) rows)))
                                clauses
                                (receipt st (cons (car args) (map car (append (cadr a) rows))))))
                            a)))))))
      ;; OVER THE EDGES AN EDITOR SUPPLIED ONLY: an edge a writer linked by
      ;; hand is `refs`' to show, and a relation no supply produces is
      ;; refused by name rather than answered as the block alone. Once a
      ;; table was read the stale count is there even when 0, so a walk that
      ;; reached nothing says whether it had edges it could not use.
      (cons 'reach
            (lambda (store actor args req options state writer cwd)
              (let ((rel (argument-option options "--rel")) (depth (argument-option options "--depth")))
                (if (or (not (= 1 (length args))) (and depth (not (count-argument depth)))
                        (and rel (= 0 (string-length rel))))
                    (usage '(reach <id> ["--rel" <rel>] ["--depth" <n>]))
                    ((derived 'reach-verb) dispatch-helper store args options state writer)))))
      ;; NEVER: A NAME CAN BE IN A LIBRARY WITHOUT BEING DEFINED THERE, AND
      ;; THE ANSWER SAYS WHICH IT IS. Three of this tree's own libraries
      ;; define nothing -- they re-export what igropyr defines -- so an answer
      ;; built only from definitions would say `unknown-name` about a name
      ;; written plainly in a one-line export list. Definitions come first
      ;; because that is what a reader usually wants; the export records say
      ;; which library carries the name, wherever it is defined.
      (cons 'whereis
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(whereis <name> ["--all-validity"]))
                  (guarded
                    (lambda ()
                      (let* ((wanted (car args))
                             ;; The handler is handed whatever the daemon had;
                             ;; `reduction-for` is how every other read verb
                             ;; turns that into a reduction to read.
                             (state (reduction-for store state))
                             (index (defs-index state))
                             (all-found (hashtable-ref index wanted '()))
                             ;; THE FILTER IS APPLIED TO THE RECORDS AFTER THE
                             ;; NAME WAS FOUND: a name whose every record is
                             ;; left out answers no items and says so, not
                             ;; unknown-name. whereis has no cap.
                             (L (lifecycle-of state))
                             (found (if L
                                        ((lifecycle-entry 'whereis-split) L all-found
                                                                          (argument-option options "--all-validity"))
                                        all-found))
                             (defs (filter (lambda (r) (eq? (car r) 'def)) found))
                             (exports (filter (lambda (r) (eq? (car r) 'export)) found)))
                        ;; NEVER: `whereis` HAS NO EMPTY ANSWER, so there is
                        ;; nowhere for `coverage` to go.
                        ;;
                        ;; A name it cannot place is an ERROR carrying the
                        ;; nearest names, not an `items` answer with nothing
                        ;; in it -- so the clause that would say whether the
                        ;; index was there has no answer to be a clause of.
                        ;; Appending one to the error would change the shape
                        ;; of a refusal that callers already match on, to
                        ;; carry a fact the refusal itself implies: the index
                        ;; was consulted, because `nearest` came out of it.
                        ;;
                        ;; So the found answer carries `cut` and `scanned`
                        ;; like the others, and the refusal is left alone.
                        ;; This is written down because "all three verbs get
                        ;; the same three clauses" is the obvious reading of
                        ;; the rule, and it is not what the verbs can do.
                        (if (null? all-found)
                            (list 'error 'unknown-name (string->symbol wanted)
                                  (cons 'nearest (nearest-names index wanted)))
                            (append
                              (items (append defs exports))
                              (scan-clauses
                                (list (cons 'cut (reduce-applied-cut state))
                                      ;; LIVE BLOCKS, for the same reason as
                                      ;; `search`: `state-datum` counts
                                      ;; tombstones, and the index this verb
                                      ;; reads is itself built from
                                      ;; `state-outline`, so the blocks it
                                      ;; covers are the live ones.
                                      (cons 'scanned-blocks (length (state-outline state)))
                                      (cons 'fields '(names exports)))
                                #f)
                              (validity-clauses L 'search-clauses (map cadr (append defs exports)))
                              (receipt state (map cadr (append defs exports)) '(versions))))))))))
      (cons 'search
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(search <query> ["--all"] ["--all-validity"]))
                  (guarded (lambda ()
                             ;; THE CAP IS NAMED AT THE CALL, NOT CHOSEN BY
                             ;; THE CALLEE. `#f` is every hit; the number is
                             ;; `search-hit-limit`, defined beside the
                             ;; reasoning for it in `store.sc` so that the
                             ;; verb names one value rather than a second
                             ;; copy of it.
                             (let* ((held #f)
                                    (all-validity (argument-option options "--all-validity"))
                                    (r (store-search-report
                                         store (car args)
                                         (if (argument-option options "--all")
                                             #f
                                             search-hit-limit)
                                         (search-keyword-hook store)
                                         (validity-hook (lambda (L) (set! held L)) all-validity)))
                                    (hits (cdr (assq 'items r)))
                                    (omitted (cdr (assq 'omitted-hits r))))
                               (append (items (map (lambda (hit) (cons 'hit hit)) hits))
                                       ;; NAMED, NOT A BARE INTEGER. `grep`
                                       ;; answers `(truncated (lines n)
                                       ;; (blocks m))` and this answers
                                       ;; `(truncated (hits n))`: a number
                                       ;; alone after a clause name means
                                       ;; whatever the verb decided it
                                       ;; meant, and a reader takes clauses
                                       ;; by name without knowing which verb
                                       ;; replied.
                                       (if (> omitted 0)
                                           (list (list 'truncated (list 'hits omitted)))
                                           '())
                                       (scan-clauses r (null? hits))
                                       (let ((tables (assq 'derived-tables r)) (via (assq 'derived-via r))
                                             (stale (assq 'derived-stale r)))
                                         (if tables ((derived 'derived-clauses) (cdr tables) (cdr via) (cdr stale)) '()))
                                       (validity-clauses held 'search-clauses (map car hits))
                                       ;; THE VERSIONS FROM THE STATE THE HITS CAME
                                       ;; FROM, which the report carries.
                                       (receipt (cdr (assq 'state r)) (map car hits) '(versions)))))))))
      ;; NEVER: THE ITEMS ARE `match`, NOT `hit`, AND THE TAG IS THE ONLY
      ;; THING THAT SAYS SO. A grep line and a search hit have the same
      ;; arity and the same types in the same places -- an id, an integer, a
      ;; string -- so a reader that checks shape cannot tell them apart. The
      ;; plugin's `hitsOf` keys on the tag `hit` and checks only that the
      ;; entry has at least four elements with a string second and an
      ;; integer third, which a grep line satisfies exactly; under one tag it
      ;; would read line numbers as scores and say nothing was wrong.
      ;;
      ;; The defence is here, in what this file names things, and not in the
      ;; reader: a consumer's accepting shape is usually wider than the shape
      ;; it was written against, so two answers a protocol reader would never
      ;; confuse can still be indistinguishable to code. `facade-gate.sc`
      ;; keeps that rule over the tag sites it collects, with a written list
      ;; of sites it accepts as exceptions; a collision involving one of
      ;; those is outside what it detects, and it says so where the list is.
      ;;
      ;; NEVER: THIS SAID THE RULE HELD FOR ALL VERBS, full stop. A gate with
      ;; a stated exception list is not the same claim as a gate without one,
      ;; and the difference is exactly where a defect would sit.
      (cons 'grep
            (lambda (store actor args req options state writer cwd)
              (let ((under (argument-option options "--under"))
                    (all? (and (argument-option options "--all") #t))
                    (held #f))
                (if (not (= 1 (length args)))
                    (usage '(grep <pattern> ["--under" <id>] ["--all"] ["--all-validity"]))
                    (guarded
                      (lambda ()
                        (let* ((r (store-grep store (car args) under all?
                                              (validity-hook (lambda (L) (set! held L))
                                                             (argument-option options "--all-validity"))))
                               (field (lambda (k) (cdr (assq k r))))
                               (lines (field 'items))
                               (omitted (field 'omitted-lines))
                               (unseen (field 'unseen-blocks))
                               ;; THE TAG IS BUILT WHERE THE ITEMS CLAUSE IS.
                               ;; It was built in a binding above this line at
                               ;; first, and the facade gate's census -- which
                               ;; reads the tag of each item constructed under
                               ;; an `items` form -- could not see it, so the
                               ;; verb was missing from the table of which
                               ;; verb answers with which tag. One call, and
                               ;; the tag is in it.
                               (answer (items (map (lambda (m) (cons 'match m)) lines)))
                               (validity (validity-clauses held 'search-clauses (map car lines))))
                          (if (and (= omitted 0) (= unseen 0))
                              (append answer (scan-clauses r (null? lines))
                                      validity
                                      (receipt (field 'state) (map car lines) '(versions)))
                              ;; THE TRUNCATION CARRIES BOTH DIMENSIONS, and
                              ;; each is named. A bare integer after a clause
                              ;; name means whatever the verb decides it
                              ;; means, and a consumer reads clauses by name:
                              ;; `(lines n)` and `(blocks m)` can be asked for
                              ;; without knowing which verb answered. `blocks`
                              ;; is the count of blocks that matched and show
                              ;; no line at all, which a count of lines cannot
                              ;; express.
                              (append answer
                                      (list (list 'truncated
                                                  (list 'lines omitted)
                                                  (list 'blocks unseen)))
                                      (scan-clauses r (null? lines))
                                      validity
                                      (receipt (field 'state) (map car lines) '(versions)))))))))))
      (cons 'log
            (lambda (store actor args req options state writer cwd)
              (if (not (or (null? args) (= 1 (length args))))
                  (usage '(log [<id>]))
                  (guarded
                    (lambda ()
                      (let ((a (store-log store (if (null? args) #f (car args)))))
                        (if (eq? (car a) 'ok)
                            ;; Each entry is (record cut past) from store-log:
                            ;; the causal cut right after the event and the one
                            ;; right before it, as `read --cut` and `diff` take them.
                            (items (map (lambda (ec)
                                          (let ((e (car ec)))
                                            (list 'entry
                                                  (list 'event (car e) (cadr e))
                                                  (list 'ts (caddr e))
                                                  (list 'actor (cadddr e))
                                                  (list 'verb (let ((p (car (cddddr e))))
                                                                (if (pair? p) (car p) 'unknown)))
                                                  (list 'cut (cadr ec))
                                                  (list 'past (caddr ec)))))
                                        (cadr a)))
                            a)))))))
      (cons 'tag
            (lambda (store actor args req options state writer cwd)
              (cond
                ((null? args)
                 (guarded
                   (lambda ()
                     (items (apply append
                                        (map (lambda (t)
                                               (if (eq? (car t) 'settled)
                                                   (list (list 'tag (list 'name (cadr t))
                                                               (list 'cut (caddr t))))
                                                   (map (lambda (c)
                                                          (list 'tag (list 'name (cadr t))
                                                                (list 'cut (car c))
                                                                (list 'event (cadr c) (caddr c))
                                                                (list 'unsettled #t)))
                                                        (caddr t))))
                                             (store-tags store)))))))
                ((= 1 (length args))
                 (guarded (lambda () (one-write store actor (list 'tag (car args)) req options))))
                (else (usage '(tag [<name>] ["--premises" <datum>]))))))
      (cons 'diff
            (lambda (store actor args req options state writer cwd)
              (if (not (= 2 (length args)))
                  (usage '(diff <cut> <cut>))
                  (guarded
                    (lambda ()
                      (let ((a (store-diff store (car args) (cadr args))))
                        (if (eq? (car a) 'ok) (items (cadr a)) a)))))))
      (cons 'conflicts
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(conflicts))
                  (guarded (lambda () (items (store-conflicts store)))))))
      ;; A STREAM NEEDS A DAEMON: the daemon carries a subscription on its
      ;; connection and never hands it here, so whoever reaches this handler
      ;; -- the local route, a nested dispatch -- has no stream to give.
      (cons 'subscribe
            (lambda (store actor args req options state writer cwd)
              (or (subscribe-shape-error args)
                  '(error bad-request (reason needs-daemon)))))
      (cons 'review-results
            (lambda (store actor args req options state writer cwd)
              (channel-answer 'review-results
                (lambda () ((channel-entry 'review-results-verb) (reduction-for store state) args)))))
      (cons 'collect-into
            (lambda (store actor args req options state writer cwd)
              (channel-answer 'collect-into
                (lambda () ((channel-entry 'collect-into-verb) store args options)))))))

  (define verbs (verb-table))

  (define (rpc-verbs) (append (map car verbs) (map car extensions)))

  ;; ---- dispatch -------------------------------------------------------------

  ;; A REQUEST THAT IS NOT A REQUEST IS ANSWERED, NOT RAISED. Whoever
  ;; sent it is on the other side of a socket or a pipe and cannot catch
  ;; a condition raised in here; a malformed request is as much a fact
  ;; about the exchange as an unknown verb is.
  ;;
  ;; ONE DISPATCH TAKES ONE LOCK, by construction rather than by
  ;; agreement: every verb below calls the same store entry point the
  ;; command line called, and those take the lock once each.
  ;; WHICH REQUESTS CARRY AN IDENTITY INTO THE WRITE PATH. It is decided
  ;; here rather than by each handler for the same reason the verb table
  ;; is a list: a handler that had to declare its own trackability is a
  ;; handler that can forget to, and forgetting looks exactly like a verb
  ;; that is not tracked.
  ;;
  ;; AND IT IS A PROCEDURE, NOT A LIST, BECAUSE `tag` IS TWO REQUESTS
  ;; UNDER ONE NAME: `tag <name>` writes a record and `tag` lists what is
  ;; there. Only the first has anything to replay. A list of verbs got
  ;; this wrong in both directions at once -- leaving `tag` out refused
  ;; an identity the write path was already using, and putting it in
  ;; would go back to accepting one silently on the listing form.
  ;; NEVER: A RELATIVE PATH IS RELATIVE TO THE CALLER, NOT TO THE SERVER. On
  ;; the daemon route the process that reads the path is somewhere else
  ;; entirely -- it was started from whatever directory happened to be
  ;; current then -- so `import-code src` meant one directory to the
  ;; person typing it and another to the process acting on it. The
  ;; envelope carries the caller's directory; where there is none (the
  ;; command line, which is already in it) the path is used as it came.
  ;; NEVER: WHICH ARGUMENTS ARE PATHS IS READ FROM THE USAGE FORM, not from a
  ;; list kept beside it. Six verbs take a path and only three had been
  ;; given the caller's directory, because the three were the ones in front
  ;; of me when the rule was written -- a list of names does not shout when
  ;; a name is missing from it. The usage forms already say which arguments
  ;; are paths, in the placeholders a reader of `--help` relies on, so they
  ;; are what decides.
  ;;
  ;; A placeholder names a path when it is `<dir>`, `<file>`, or ends in
  ;; `-file`. NOTE: `<writer>`, `<segment>` and `<sha256>` are not paths and
  ;; must not be rewritten -- `publish` takes all four kinds in one line.
  ;; NOTE: THE ANGLE BRACKETS ARE PART OF THE SYMBOL. `<dir>` reads as a
  ;; symbol whose name is `"<dir>"`, brackets and all -- a rule written
  ;; against `"dir"` matched nothing at all, and the three verbs that had
  ;; been resolving paths by hand stopped resolving them. Measured: a
  ;; relative `import-code src` answered `not-a-directory` where it had
  ;; worked the moment before.
  (define (path-placeholder? x)
    (and (symbol? x)
         (let* ((raw (symbol->string x))
                (k (string-length raw))
                (n (if (and (>= k 2)
                            (char=? (string-ref raw 0) #\<)
                            (char=? (string-ref raw (- k 1)) #\>))
                       (substring raw 1 (- k 1))
                       raw))
                (m (string-length n)))
           (or (string=? n "dir")
               (string=? n "file")
               (and (> m 5) (string=? (substring n (- m 5) m) "-file"))))))

  ;; The positional slots of a usage form, in order, as placeholders: the
  ;; symbols before any optional `[...]` group.
  (define (usage-positionals form)
    (if (not (pair? form))
        '()
        (let loop ((xs (cdr form)) (out '()))
          (cond ((not (pair? xs)) (reverse out))
                ((pair? (car xs)) (reverse out))
                (else (loop (cdr xs) (cons (car xs) out)))))))

  ;; The options whose VALUE is a path, as spellings: `["--output" <review-file>]`.
  (define (usage-path-options form)
    (if (not (pair? form))
        '()
        (let loop ((xs (cdr form)) (out '()))
          (cond ((not (pair? xs)) (reverse out))
                ((and (pair? (car xs)) (= 2 (length (car xs)))
                      (string? (car (car xs))) (path-placeholder? (cadr (car xs))))
                 (loop (cdr xs) (cons (car (car xs)) out)))
                (else (loop (cdr xs) out))))))

  ;; The usage form a verb is published with, or #f.
  (define (usage-form-of verb)
    (let look ((es (verb-catalogue)))
      (cond ((null? es) #f)
            ((eq? verb (car (car es))) (cadr (car es)))
            (else (look (cdr es))))))

  ;; Every argument the usage form calls a path, rewritten against the
  ;; caller's directory. Positionals by their slot, option values by their
  ;; spelling; everything else is left exactly as it arrived.
  (define (under-cwd-nodes cwd verb nodes)
    (if (not (string? cwd))
        nodes
        (let* ((form (usage-form-of verb))
               (slots (usage-positionals form))
               (opts (usage-path-options form)))
          (if (not form)
              nodes
              (let loop ((ns nodes) (pos 0) (out '()))
                (cond
                  ((null? ns) (reverse out))
                  ((eq? 'pos (car (car ns)))
                   (let ((path? (and (< pos (length slots))
                                     (path-placeholder? (list-ref slots pos)))))
                     (loop (cdr ns) (+ pos 1)
                           (cons (if path?
                                     (list 'pos (under-cwd cwd (cadr (car ns))))
                                     (car ns))
                                 out))))
                  ((and (eq? 'option (car (car ns)))
                        (member (cadr (car ns)) opts))
                   (loop (cdr ns) pos
                         (cons (list 'option (cadr (car ns))
                                     (under-cwd cwd (caddr (car ns))))
                               out)))
                  (else (loop (cdr ns) pos (cons (car ns) out)))))))))

  (define (under-cwd cwd path)
    (if (and (string? cwd) (string? path) (> (string-length path) 0)
             (not (char=? (string-ref path 0) #\/)))
        (string-append cwd "/" path)
        path))

  (define (tracked-request? verb args)
    (case verb
      ((insert set move del link unlink relation rule batch commit import-code def) #t)
      ((tag) (= 1 (length args)))
      (else #f)))

  (define (rpc-dispatch store request . rest)
    (cond
      ((not (list? request)) '(error bad-request not-a-list))
      ((null? request) '(error bad-request empty))
      ((not (symbol? (car request))) '(error bad-request tag-not-a-symbol))
      ((not (for-all string? (cdr request))) '(error bad-request arguments-not-strings))
      (else
       (let ((nodes (parse-arguments (car request) (cdr request))))
         (if (and (pair? nodes) (eq? (car nodes) 'error)) nodes
             (rpc-dispatch-parsed store (car request) nodes
               (if (pair? rest) (car rest) "rpc")
               (and (pair? rest) (pair? (cdr rest)) (cadr rest))
               (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest))
                    (caddr rest))
               (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest))
                    (pair? (cdddr rest)) (cadddr rest))
               (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest))
                    (pair? (cdddr rest)) (pair? (cddddr rest)) (car (cddddr rest)))))))))

  ;; Parsed nodes are the CLI's internal handoff. All verb arguments and
  ;; their fingerprint are derived from the same tokenization.
  ;; NEVER: `state` IS A VALUE THE CALLER ALREADY HOLDS, AND HANDLERS MAY NOT
  ;; CHANGE IT. #f means "there is none, load one" -- which is what every
  ;; caller that reaches a store by its path passes. A daemon passes the
  ;; reduction it published: one fold, shared by every reader, never
  ;; mutated after it was published. A handler that modified it would be
  ;; modifying what every other reader sees, in another process, with
  ;; nothing to report it.
  ;;
  ;; NOTE: AND IT IS ONLY EVER THE WHOLE STORE. A verb asking for a
  ;; historical cut is asking for a DIFFERENT reduction and still opens
  ;; the log for it; passing this one there would answer a question about
  ;; the past with the present.
  ;;
  ;; F77c: THE STATE A HANDLER IS GIVEN IS SEALED, and this unseals it --
  ;; or loads -- through obtain-state, which judges it against the
  ;; request's declaration. A verb that never calls this never consumes
  ;; the state and is never refused for holding it.
  (define (reduction-for store state)
    (obtain-state store state #f))

  ;; ---- the envelope a request travels in ----------------------------------
  ;;
  ;; NEVER: ONE PLACE PACKS IT. Two callers reach a daemon -- the command line
  ;; and the MCP shell -- and if each wrote out `(request store actor verb
  ;; args…)` for itself, the day the envelope grew a field would be the
  ;; day one of them quietly kept sending the old one.
  ;;
  ;; NOTE: AND IT PRODUCES THE BYTES, terminator included, because the
  ;; terminator is part of the envelope. Measured on the version where it
  ;; was not: `render-wire` already ends with a newline and the caller
  ;; appended a second, so every forwarded request was followed by an
  ;; EMPTY FRAME. The daemon answered it -- `(ok …)` and then
  ;; `(error bad-request (reason not-a-datum))` -- and closed the
  ;; connection. Nobody saw it, because that caller exits after the first
  ;; answer.
  ;; NEVER: "NOBODY IS THERE" IS ONE QUESTION, ASKED IN ONE PLACE. `exchange`
  ;; reports a failed connection and a failure mid-answer with the SAME
  ;; tag, `(transport-error <status>)`, and only the status tells them
  ;; apart -- so the two callers that fall back to running locally have
  ;; to agree about which statuses mean "the request reached nobody".
  ;; Disagreeing would mean one of them re-running work that may already
  ;; have been done.
  ;;
  ;; KEY: THE STATUSES ARE libuv's, WHICH ON UNIX ARE THE libc ERRNO NEGATED,
  ;; so the question is the client's own: no-daemon-errno? of the status
  ;; negated -- ENOENT (nothing at that path), ENOTSOCK (something that is
  ;; not a socket), ECONNREFUSED (a socket file whose daemon has gone),
  ;; each the running platform's number from (theourgia platform-numbers).
  ;; A list of literals here once held macOS's -38 and -61 with Linux's
  ;; -111 added by hand, and missed Linux's -88: a file that is not a
  ;; socket at the socket path was not "nobody there" on Linux.
  ;;
  ;; NOTE: WHAT COVERS IT. `test/cli-forward.sc` ends a daemon with an
  ;; uncatchable signal, checks the socket it bound is still on disk, and
  ;; requires the next call to answer from the local store -- on this
  ;; platform that path arrives here as ECONNREFUSED. The other platforms'
  ;; numbers are asked of this predicate directly, under forced rows, by
  ;; `test/platform-rows.sc`'s PN-E rows, since no row can make libuv
  ;; report another platform's numbers. `test/client-start.sc`'s CS-9
  ;; rows separate "provably never went out" from "this may already have
  ;; happened" by the COUNT of bytes written, not by an errno.
  (define (transport-unreachable? outcome)
    (and (pair? outcome)
         (eq? 'transport-error (car outcome))
         (pair? (cdr outcome))
         (let ((status (cadr outcome)))
           (and (integer? status) (exact? status) (negative? status)
                (no-daemon-errno? (- status))))
         #t))

  ;; NEVER: `request-frame` IS NOT DEFINED HERE ANY MORE. It moved to
  ;; `(theourgia client)` and is re-exported from this library so that no
  ;; caller had to change. The reason it moved is the same one that moved
  ;; `socket-path` there: the CLIENT has to pack the envelope and the
  ;; client may not import this library -- loading the dispatcher is the
  ;; cost the split exists to avoid -- so a packer living here would have
  ;; had to be written a second time on the other side, and two spellings
  ;; of one envelope is the defect this arrangement exists to prevent.

  ;; KEY: THE WRITER IS AN IDENTITY THE CALLER CARRIES, NOT A WORD IN ITS
  ;; ARGUMENTS. The daemon used to splice `--writer <name>` into the
  ;; argument list before parsing it, which meant a request whose own
  ;; text contained `--writer` -- as a block's bytes, or as anything
  ;; after `--` -- suppressed the default and was then refused for
  ;; having no writer. The envelope's writer now arrives here, beside
  ;; the actor, and the arguments are passed through untouched.
  ;;
  ;; KEY: AN ANSWER BUILT FROM AN INCOMPLETE REDUCTION SAYS SO, AND IT SAYS
  ;; SO HERE, ONCE, NOT IN EACH VERB (K10). A writer this store could not
  ;; read is left out of every reduction, and a read that answered without
  ;; it would be answering "these records are not there" about records it
  ;; could not see -- the quiet absence F77 exists to remove. So every
  ;; answer, ok or refusal, whose loads found an unreadable writer ends in
  ;;   (incomplete (unreadable (writer w) (path p) (reason r)) ...)
  ;; and an answer from a healthy store is exactly what it was.
  ;;
  ;; What was found comes from two places: the loads this request opened,
  ;; which report to a listener registered under this request's own copy
  ;; of the store string (log.sc, load-listener-add!), and the reduction
  ;; the caller handed in, which remembers its load (log.sc,
  ;; unreadable-behind). A dispatch nested inside another reuses the
  ;; outer one's key, so its loads are heard by the answer that goes out.
  ;;
  ;; NOTE: AN ANSWER THAT LEAVES AS A RAISE HAS NO TAIL TO CARRY THE CLAUSE.
  ;; The daemon's answer-for turns such a raise into an answer after this
  ;; point; that answer does not carry it.
  ;;
  ;; THE TRANSLATION POINT (F100b point 1). The outermost dispatch -- the one
  ;; that owns the load listener, `own?` -- opens its own mutation-record
  ;; scope around the whole of dispatch-verb, and a filesystem condition
  ;; that leaves it is answered by (theourgia answers)' table with that
  ;; scope's record: `unwritable`/`unreadable`/`absent` when nothing had
  ;; been changed, `incomplete` with what had. A NESTED dispatch opens no
  ;; scope of its own: its changes are part of the outer one's record, and
  ;; a scope of its own would hide them from the outer answer (scopes do
  ;; not pass their entries outward). What the table does not classify
  ;; leaves as it did, to the caller's own backstop.
  (define (rpc-dispatch-parsed store verb nodes actor . rest)
    (let* ((own? (and (string? store) (not (load-listener-of store))))
           (key (if own? (string-copy store) store))
           (heard '())
           (refused #f)
           ;; TWO EVENTS REACH THE LISTENER (F77c): a load's notes, and a
           ;; load that was refused -- the first refusal is kept.
           (hear! (lambda (event)
                    (if (load-refused? event)
                        (unless refused (set! refused (load-refused-condition event)))
                        (set! heard (merge-unreadable heard event))))))
      (let ((answer (if own?
                        (with-mutation-record
                          (lambda ()
                            ;; A REFUSED LOAD NAMES THE ANSWER, whatever a
                            ;; catch-all between it and here made of the
                            ;; raise: the refusal's own answer, with this
                            ;; dispatch's record (F77c; design review r6).
                            ;; A consumer that wants to go on without a
                            ;; writer must declare; catching the refusal and
                            ;; carrying on is not a way round it.
                            (let ((a (guard (e ((and refused (classify-failure refused (mutation-record)))
                                                => (lambda (a) a))
                                               ((classify-failure e (mutation-record)) => (lambda (a) a)))
                                       (dynamic-wind
                                         (lambda () (load-listener-add! key hear!))
                                         (lambda () (apply dispatch-verb key verb nodes actor rest))
                                         (lambda () (load-listener-remove! key))))))
                              (if refused (classify-failure refused (mutation-record)) a))))
                        (apply dispatch-verb key verb nodes actor rest))))
        ;; The notes come from what the scope HEARD: loads, and supplied
        ;; states a consumer unsealed (obtain-state tells the listener).
        (with-incomplete-clause answer heard))))

  (define (with-incomplete-clause answer unreadable)
    (let ((clause (incomplete-clause unreadable)))
      (if (and clause (pair? answer) (list? answer))
          (append answer (list clause))
          answer)))

  ;; ---- the declaration (F77c) ---------------------------------------------
  ;;
  ;; A VERB EITHER CARRIES WHAT ITS REDUCTION IS MISSING INTO ITS ANSWER --
  ;; rpc-dispatch-parsed appends the clause to every answer built from a
  ;; load that could not read a writer -- or it writes a projection, a
  ;; snapshot or records from the reduction, and then it may not build on
  ;; one that is missing a writer. These are the second kind; every other
  ;; verb declares. A verb that builds no reduction declares too, and it
  ;; makes no difference: it never loads.
  (define undeclared-verbs '(export-code export-md import-md import-code def snapshot supply))
  ;; THE UNDECLARED VERBS ARE STRICT: a cut refuses them before they build
  ;; anything (the load raises), where every consumer outside a request
  ;; still loads a cut store as it always has.
  ;; ONE PLACE ANSWERS FOR EVERY VERB: the built-in list, then a registered
  ;; verb's own declaration (rpc.sc, entry-problem), read with the arguments
  ;; the verb was given.
  (define (verb-declaration verb args)
    (cond ((memq verb undeclared-verbs) incomplete-refused)
          ((find (lambda (e) (eq? (car e) verb)) extensions)
           => (lambda (e)
                (let ((d (if (= 9 (length e)) (list-ref e 8) 'accept)))
                  (if (or (eq? d 'refuse)
                          (and (pair? d) (pair? args) (member (car args) (cdr d))))
                      incomplete-refused
                      incomplete-accepted))))
          (else incomplete-accepted)))

  ;; THE DECLARATION RIDES ON THE REQUEST'S STORE OBJECT for the extent of
  ;; the verb (plan amendment A2), and the outer one is put back on the way
  ;; out -- a normal return AND an escape -- so a nested verb of another
  ;; class is judged by its own class and leaves its caller's intact.
  (define (with-verb-declaration store verb args thunk)
    (let ((outer (load-declaration-of store)))
      (dynamic-wind
        (lambda () (load-declaration-set! store (verb-declaration verb args)))
        thunk
        (lambda () (load-declaration-set! store outer)))))

  (define (dispatch-verb store verb nodes actor . rest)
    (let* ((state (let ((s (and (pair? rest) (car rest))))
                    ;; SEALED: see reduction-for.
                    (and s (if (sealed-state? s) s (seal-state s '())))))
           (default-writer (and (pair? rest) (pair? (cdr rest)) (cadr rest)))
           ;; NEVER: WHAT THE CALLER PIPED IN, AND WHERE THE CALLER WAS. Both
           ;; are facts about the process that made the request, and on
           ;; the daemon route that process is somewhere else entirely.
           ;; `argument-stdin` is the one rule that says which verbs read
           ;; standard input and where it goes in their arguments; it is
           ;; applied here when a caller supplies a reader, and the
           ;; command line -- which parses its own arguments and has
           ;; already applied it -- supplies none.
           (stdin-reader (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest))
                              (caddr rest)))
           (cwd (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest))
                     (pair? (cdddr rest)) (cadddr rest)))
           ;; NEVER: A VERB THAT READS STANDARD INPUT AND WAS GIVEN NONE IS
           ;; TOLD SO. It used to run with the placeholder still in its
           ;; arguments, so `write <id> -` stored the literal "-" and
           ;; answered `(ok (saved ...))` -- the caller's bytes discarded
           ;; with no error anywhere. Asked of the same table that decides
           ;; where the input goes.
           (wants-stdin (argument-wants-stdin? verb nodes))
           (nodes (if (and wants-stdin (procedure? stdin-reader))
                      (argument-stdin verb nodes stdin-reader)
                      nodes))
           (nodes (under-cwd-nodes cwd verb nodes))
           (entry (or (assq verb verbs) (extension-dispatch-entry verb)))
           (id (argument-option nodes "--req"))
           (cursor (argument-option nodes "--cursor"))
           (after (and cursor (parse-after cursor)))
           (options (argument-remove nodes '("--req" "--cursor")))
           (args (argument-positionals options))
           ;; ONE RULE, ONE PLACE: an explicit option wins, the envelope's
           ;; writer is the default, and every handler is told the answer
           ;; rather than working it out again.
           (writer (or (argument-option options "--writer") default-writer)))
      (cond
        ;; NEVER: A VERB THAT CANNOT BE PRINTED IS REFUSED BEFORE IT IS
        ;; PRINTED, and this is the well-formedness layer, so it comes
        ;; first. A caller's verb can be any symbol; one they got wrong is
        ;; exactly the kind the wire writer will not emit -- `show me`
        ;; went out as `show\x20;me` and the reader that had asked the
        ;; question could not parse the answer to it.
        ;;
        ;; KEY: AND IT IS JUDGED HERE, IN THE PART BOTH ROUTES SHARE. The
        ;; reader's guard (`readable-shape?`) would refuse such a frame as
        ;; `not-a-datum` at a daemon, and a call made in this process
        ;; would never meet that guard at all -- so the same verb would
        ;; have got two different answers depending on whether a daemon
        ;; happened to be running. `daemon.sc` promises the two routes are
        ;; byte for byte the same answer; this is what keeps that true.
        ;;
        ;; NOTE: THE TWO LAYERS ANSWER DIFFERENT QUESTIONS AND BOTH STAY.
        ;; This one says "no daemon would accept this, so it is not sent";
        ;; the reader's guard says "these bytes are not something I will
        ;; hand to `read`", and it still answers a third-party client that
        ;; writes `(request |show me| ...)` to the socket itself.
        ((verb-spelling-error (datum-spelling verb)) => (lambda (e) e))
        ;; THE VERB IS SPELLED, NOT SENT BACK: the spelling is a STRING
        ;; here, which survives the wire whatever it holds -- and after
        ;; the clause above, what it holds is within the alphabet, so the
        ;; string and the symbol now agree character for character.
        ((not entry)
         (list 'error 'unknown-verb (list 'spelling (datum-spelling verb))
               (cons 'verbs (rpc-verbs))))
        ((transport-option-refusal options) => (lambda (e) e))
        ;; NEVER: ONLY THE SHAPE THAT USED TO BE SILENT. A verb whose input is
        ;; simply absent answers its own usage line and always has; the one
        ;; that needed saying is an unfilled `-`, which was stored as the
        ;; text with `(ok (saved ...))` on top of it.
        ;;
        ;; KEY: AND STDIN A VERB DOES NOT READ IS IGNORED, NOT REFUSED. This
        ;; is a choice, and the reason is that the only caller who can get
        ;; here is one that put input in the envelope for a verb whose
        ;; argument form does not take any -- and a wrapper that forwards
        ;; whatever it was given, unconditionally, is an ordinary and
        ;; legitimate way to write a client. Refusing it would fail every
        ;; verb for such a caller, which is the same shape as a runner
        ;; inheriting a terminal and reading input nobody meant to send.
        ;; A process ignores standard input it does not read; so does this.
        ;; NEVER: AN UNFILLED `-` IS THE REFUSAL, and it is the branch below:
        ;; there the caller ASKED for input and none came.
        ((and (not (procedure? stdin-reader))
              (argument-stdin-placeholder? verb nodes))
         '(error bad-request stdin-required))
        ;; NEVER: TWO VERBS DO NOT NEED A STORE AND MUST NOT BE REFUSED FOR
        ;; NOT HAVING ONE. `init` is what creates it. `describe` answers
        ;; out of a table: asking what the verbs are may not open a
        ;; store, take a lock or write a byte, and a caller asking that
        ;; question is quite often one that has not got a store yet --
        ;; the MCP shell builds its tool list this way, before anyone has
        ;; said which store they mean.
        ;; THE PREMISE SET IS READ HERE, ONCE, before the store is asked
        ;; whether it exists and before any verb's own checks: a set the
        ;; store cannot check is the answer. Both routes dispatch through
        ;; this point; what comes before it is the request arriving.
        ((premises-refusal store verb options) => (lambda (a) a))
        ((and (not (memq verb '(init describe))) (no-store? store)) => (lambda (a) a))
        ((and id (not after)) '(error bad-request req-without-cursor))
        ((and id (not (tracked-request? verb args)))
         (list 'error 'bad-request 'req-not-tracked verb))
        ((and after (not id)) '(error bad-request cursor-without-req))
        ((and id (not (req-id-ok? id))) '(error bad-request malformed-req-id))
        ((eq? after 'malformed) '(error bad-request malformed-cursor))
        (else (with-verb-declaration store verb args
                (lambda ()
                  ((cdr entry) store actor args
                   ;; A REQUEST'S IDENTITY NEVER INCLUDES ITS PREMISES: the same
                   ;; request retried with a new receipt is the same request.
                   ;; The handler still receives the option.
                   (and id (make-write-request actor verb (argument-strings (argument-remove options '("--premises"))) id after))
                   options state writer cwd)))))))

  ;; `<writer>:<seq>`, BY SHAPE AND NEVER THROUGH `read`. The reader
  ;; implements the whole of Scheme's numeric syntax, and `#e1e99999999`
  ;; is twelve characters asking it to build an integer of a hundred million
  ;; digits: it does not refuse, it allocates and computes for as long as that
  ;; takes, and no check placed after it runs until it is done.
  ;;
  ;; NOTE: THE SIZE IS LARGE, NOT UNBOUNDED, and this used to say the machine
  ;; would be exhausted. A hundred million decimal digits is about 332 million
  ;; value bits, roughly 40 MiB before overhead: far more than the caller's
  ;; twelve bytes, which is the point, and not enough to justify the stronger
  ;; word.
  ;;
  ;; NEVER: BOTH NUMBERS IN THIS SENTENCE WERE WRONG. It said eleven
  ;; characters and ten billion digits; the literal is twelve characters and
  ;; the exponent asks for 99999999 + 1. The digit count was written in two
  ;; places, the character count in one -- and the correction first said both
  ;; were in both, which added a wrong number while fixing two. The argument
  ;; never depended on either figure, but the figures are the part a later
  ;; reader quotes.
  (define (parse-after text)
    (let loop ((i 0))
      (cond
        ((= i (string-length text)) 'malformed)
        ((char=? (string-ref text i) #\:)
         (let ((w (substring text 0 i))
               (n (count-argument (substring text (+ i 1) (string-length text)))))
           (if (and n (> (string-length w) 0)) (cons w n) 'malformed)))
        (else (loop (+ i 1))))))
)
