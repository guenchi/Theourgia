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
  (export rpc-dispatch rpc-dispatch-parsed rpc-ok? rpc-verbs
          request-frame transport-unreachable?
          count-argument outline-text write-protocol verb-catalogue
          describe-log-error)
  (import (only (theourgia view) view-read)
          (only (theourgia render) render-wire)
          (only (theourgia client) request-frame verb-spelling-error)
          (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs conditions) (rnrs io ports) (rnrs files)
          (rnrs unicode) (rnrs arithmetic fixnums) (rnrs bytevectors)
          (rnrs hashtables)
          (theourgia store) (theourgia reduce) (theourgia log)
          (theourgia working) (theourgia baseline) (theourgia code-project) (theourgia code-suggest)
          (theourgia datum-project)
          (only (theourgia datum-code) datum-source-read)
          (only (theourgia ffi) read-entry entry-type fs-error? with-mutation-record mutation-record)
          (only (theourgia answers) classify-failure)
          (only (theourgia incomplete) incomplete-accepted)
          (only (theourgia request) req-id-ok?)
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
      "Hold a writer id from one agent at a time: a later session may bind the same id and carry on with its drafts, but two agents writing one draft at once overwrite each other silently.\n"))

  (define (usage form) (list 'usage form))

  ;; AN ANSWER SAYS WHICH OF THREE THINGS IT IS, so that whoever renders
  ;; it never has to guess from the shape. `(ok (items …))` is a list to
  ;; be shown one per line, `(ok (text …))` is bytes to be shown as they
  ;; are, and anything else is a single datum. Guessing -- "a list of
  ;; pairs is probably items" -- would classify a block's field alist as
  ;; a list of items the first time someone read a block with two fields.
  (define (items xs) (list 'ok (cons 'items xs)))

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

  ;; A TITLE IS SOMETHING A PERSON READS, so it comes from the view: for
  ;; a code block the name is derived from its source, and `state-read`
  ;; no longer derives anything.
  (define (title-of state id)
    (let* ((b (view-read state id))
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
    '(commit [<block> ...] ["--writer" <name>] ["--working-version" <block>=<version>]))

  (define outline-usage
    '(outline ["--depth" <n>] ["--with-keywords"]))

  (define insert-usage
    '(insert "--under" <id> ("--after" <id>) "--title" <text> ("--text" <text>)
             ("--keywords" <text>)))

  (define (unknown-id state id)
    (list 'error 'unknown-id id (list 'nearest (nearest-ids state id))))

  (define (one-write store actor intent req . check)
    (let ((answers (with-store-write store (lambda (state view) (list intent))
                                     actor req (and (pair? check) (car check)))))
      (car answers)))

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
                    req)))))

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
                         ((string=? (cadr rest) "kind")
                          (let ((k (string->symbol (caddr rest))))
                            (if (kind-known? k)
                                k
                                ;; NEVER: THE SAME CONDITION GETS THE SAME NAME
                                ;; ON BOTH ROUTES. This said `unknown-kind`
                                ;; while the intent layer said
                                ;; `kind-not-known` -- two names I coined for
                                ;; one thing inside one change, which is how a
                                ;; reader ends up believing they are two
                                ;; things. The envelope differs because the
                                ;; layers differ; the condition does not.
                                (raise (list 'error 'bad-request 'kind-not-known
                                             (list 'kind (caddr rest))
                                             (list 'known known-kinds))))))
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
            (usage '(set <id> <field> <value> ["--if-unchanged" <version>] ["--based-on" <version>]))
            (one-write store actor (if expect (list 'expect expect intent) intent) req
              (let ((h (argument-option options "--based-on")))
                (and h (lambda (state) (baseline-refusal state (car rest) h)))))))))

  (define (parse-move store actor args req options)
    (let ((after (argument-option options "--after")) (rest args))
      (if (not (= 2 (length rest)))
          (usage '(move <id> <parent> ["--after" <id>]))
          (one-write store actor
                     (list 'move (car rest)
                           (if (string=? (cadr rest) "root") 'root (cadr rest))
                           after)
                     req))))

  (define (parse-edge store actor verb args req)
    (if (not (= 3 (length args)))
        (usage (list verb '<from> '<rel> '<to>))
        (one-write store actor
                   (list verb (car args) (string->symbol (cadr args)) (caddr args))
                   req)))

  ;; A BATCH IS ONE WRITE SESSION. Read as data by whoever holds the
  ;; bytes, handed here as a list of intents; running them one at a time
  ;; through separate sessions would let another writer interleave, and
  ;; the back-references `(from n)` would then point into a history the
  ;; author did not have.
  (define (run-batch store actor items req)
    ;; the caller knows whether this request had any sub-operations at all;
    ;; the answer cannot be told from its shape
    (batch-answer (with-store-write store (lambda (state view) items) actor req)
                  (null? items)))

  (define (parse-batch store actor args req)
    (if (not (= 1 (length args)))
        (usage '(batch <intents>))
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
                         req)))))

  ;; ---- the verbs ------------------------------------------------------------

  ;; THE TABLE IS THE LIST OF VERBS. `eval` is not in it -- it is not
  ;; removed from it by a second rule somewhere, which is how a verb
  ;; comes back by accident -- so an unknown tag and a deliberately
  ;; absent one are the same answer, and there is nowhere to forget.
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
  ;; the only thing holding this table honest.
  ;;
  ;; KEY: THE FIFTH FIELD SAYS WHO CARRIES THE VERB OUT. `daemon` means a
  ;; client sends it over the socket; `local` means the client runs the
  ;; server in its own process instead, because there is nothing to send
  ;; it to yet or because the work has to be this process's child.
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
  (define (verb-catalogue)
    (list
      (list 'init '(init)
            "Create a store in this directory." #f 'local)
      (list 'insert insert-usage
            "Add a block under a parent, with a title and optional text." #t 'daemon)
      ;; NOTE: NOT MARKED, ALTHOUGH `set <id> src <text>` DOES PUT PROSE IN.
      ;; §7.6.45 names the two verbs the protocol is attached to, and
      ;; this is not one of them. It is left as the specification has it
      ;; rather than widened here, because the mark is what the MCP tool
      ;; descriptions are built from and widening it silently would
      ;; change what agents are told without anyone deciding to.
      (list 'set '(set <id> <field> <value> ["--if-unchanged" <version>] ["--based-on" <version>])
            "Replace one field of one block." #f 'daemon)
      (list 'move '(move <id> <parent> ["--after" <id>])
            "Move a block to another parent, optionally after a sibling." #f 'daemon)
      (list 'del '(del <id>)
            "Retire a block. Its history stays." #f 'daemon)
      (list 'link '(link <from> <rel> <to>)
            "Record a named relation between two blocks." #f 'daemon)
      (list 'unlink '(unlink <from> <rel> <to>)
            "Remove a named relation between two blocks." #f 'daemon)
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
      (list 'discard '(discard <block> ["--writer" <name>])
            "Throw away a writer's draft of a block." #f 'daemon)
      (list 'batch '(batch <intents>)
            "Carry out several changes as one request." #f 'daemon)
      (list 'split-suggest '(split-suggest <file> ["--output" <review-file>])
            "Propose where a long file could be divided into blocks." #f 'daemon)
      (list 'import-code '(import-code <dir> ["--allow-delete"] ["--datum"])
            "Read a directory of source into the store. With --datum, the whole-line ; comments directly above a form become its doc; a ; comment inside a form is dropped, and the answer warns with its line and column. A #| |# block comment, and any comment inside a datum discarded with #;, is dropped with neither."
            #f 'daemon)
      (list 'export-code '(export-code <dir> ["--raw"] ["--datum"] ["--working"] ["--writer" <name>])
            "Write the store's source back out to a directory." #f 'daemon)
      (list 'def '(def <name> ["--under" <library>] <source>)
            "Define or replace one named definition." #f 'daemon)
      (list 'import-md '(import-md <dir> ["--allow-delete"])
            "Read a directory of markdown into the store." #f 'daemon)
      (list 'export-md '(export-md <dir> ["--with-ids"] ["--working"] ["--writer" <name>])
            "Write the store out as markdown." #f 'daemon)
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
      (list 'read '(read <id> ["--md"] ["--recursive"] ["--writer" <name>]
                         ["--working"] ["--working-info"])
            "Read one block: its fields, or its text." #f 'daemon)
      (list 'refs '(refs <id>)
            "List the relations a block takes part in." #f 'daemon)
      (list 'search '(search <query> ["--all"])
            "Find blocks whose title, keywords or text match every word given." #f 'daemon)
      (list 'grep '(grep <pattern> ["--under" <id>] ["--all"])
            "List the lines that contain a pattern, literally." #f 'daemon)
      (list 'whereis '(whereis <name>)
            "Say where a name is defined, and which libraries carry it." #f 'daemon)
      (list 'log '(log [<id>])
            "Show the changes recorded, for the store or for one block." #f 'daemon)
      (list 'tag '(tag [<name>])
            "Name the current cut, or list the names already given." #f 'daemon)
      (list 'diff '(diff <cut> <cut>)
            "Report what changed between two cuts." #f 'daemon)
      (list 'conflicts '(conflicts)
            "List blocks whose writers disagree." #f 'daemon)
      (list 'describe '(describe)
            "List the verbs, what each is for, and the writing protocol." #f 'daemon)))

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
                  (describe-answer))))
      (cons 'init
            (lambda (store actor args req options state writer cwd)
              (if (not (null? args))
                  (usage '(init))
                  (guarded (lambda ()
                             (let ((a (store-init! store)))
                               (if (eq? (car a) 'ok) a (cons 'error (cdr a)))))))))
      (cons 'insert (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-insert store actor args req options)))))
      (cons 'set (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-set store actor args req options state)))))
      (cons 'move (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-move store actor args req options)))))
      (cons 'del
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(del <id>))
                  (guarded (lambda () (one-write store actor (list 'del (car args)) req))))))
      (cons 'link (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-edge store actor 'link args req)))))
      (cons 'unlink (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-edge store actor 'unlink args req)))))
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
                                             (argument-option-list options "--working-version"))))
                (if (and (pair? answer) (eq? (car answer) 'error))
                    (append answer (list (list 'usage commit-usage)))
                    answer))))
      (cons 'drafts
            (lambda (store actor args req options state writer cwd)
              (if (null? args) (working-list store state writer)
                  (usage '(drafts ["--writer" <name>])))))
      (cons 'discard
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (working-discard! store writer (car args))
                  (usage '(discard <block> ["--writer" <name>])))))
      (cons 'batch (lambda (store actor args req options state writer cwd) (guarded (lambda () (parse-batch store actor args req)))))
      (cons 'split-suggest
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (guarded (lambda () (split-suggest (car args)
                                                     (argument-option options "--output"))))
                  (usage '(split-suggest <file> ["--output" <review-file>])))))
      (cons 'import-code
            (lambda (store actor args req options state writer cwd)
              (if (= 1 (length args))
                  (guarded (lambda ()
                    (if (argument-option options "--datum") (import-datum store (car args) actor req)
                        (import-code store (car args) actor req
                                     (argument-option options "--allow-delete")))))
                  (usage '(import-code <dir> ["--allow-delete"] ["--datum"])))))
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
      (cons 'def
            (lambda (store actor args req options state writer cwd)
              (if (= (length args) 2)
                  (guarded (lambda () (def-datum store (car args) (argument-option options "--under") (cadr args) actor req)))
                  (usage '(def <name> ["--under" <library>] <source>)))))
      (cons 'import-md
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(import-md <dir> ["--allow-delete"]))
                  (guarded (lambda ()
                             (list 'import
                                   (import-md store (car args) actor
                                              (argument-option options "--allow-delete"))))))))
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
                   (let ((with-keywords (argument-option options "--with-keywords")))
                     (guarded (lambda ()
                                (text (outline-text (reduction-for store state)
                                                    (and depth (count-argument depth))
                                                    with-keywords))))))))))
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
      (cons 'read
            (lambda (store actor args req options state writer cwd)
              (let ((md? (argument-option options "--md"))
                    (deep? (argument-option options "--recursive")) (rest args))
                (cond
                  ((not (= 1 (length rest))) (usage '(read <id> ["--md"] ["--recursive"] ["--writer" <name>]
                                 ["--working"] ["--working-info"])))
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
                       ;; `view-read`s in this file each kill a row.
                       (let* ((state (reduction-for store state))
                              (b (state-read state (car rest))))
                         (cond
                           ((not b) (unknown-id state (car rest)))
                           (deep? (text (block-text state (car rest) #f)))
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
                              (text (string-append front head body)))))))))
                  (deep?
                   (guarded
                     (lambda ()
                       (let* ((state (reduction-for store state))
                              (ids (subtree-ids state (car rest))))
                         (if (not ids)
                             (unknown-id state (car rest))
                             (items (map (lambda (id) (view-read state id)) ids)))))))
                  (else
                   (guarded
                     (lambda ()
                       (let* ((state (reduction-for store state))
                              (b (view-read state (car rest))))
                         (if b (cons 'ok (list b)) (unknown-id state (car rest)))))))))))
      (cons 'refs
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(refs <id>))
                  (guarded
                    (lambda ()
                      (let ((a (store-refs store (car args))))
                        (if (eq? (car a) 'ok)
                            (items (map (lambda (r)
                                          (list 'ref (list 'from (car r))
                                                (list 'rel (cadr r))
                                                (list 'via (caddr r))))
                                        (cadr a)))
                            a)))))))
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
                  (usage '(whereis <name>))
                  (guarded
                    (lambda ()
                      (let* ((wanted (car args))
                             ;; The handler is handed whatever the daemon had;
                             ;; `reduction-for` is how every other read verb
                             ;; turns that into a reduction to read.
                             (state (reduction-for store state))
                             (index (defs-index state))
                             (found (hashtable-ref index wanted '()))
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
                        (if (null? found)
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
                                #f)))))))))
      (cons 'search
            (lambda (store actor args req options state writer cwd)
              (if (not (= 1 (length args)))
                  (usage '(search <query> ["--all"]))
                  (guarded (lambda ()
                             ;; THE CAP IS NAMED AT THE CALL, NOT CHOSEN BY
                             ;; THE CALLEE. `#f` is every hit; the number is
                             ;; `search-hit-limit`, defined beside the
                             ;; reasoning for it in `store.sc` so that the
                             ;; verb names one value rather than a second
                             ;; copy of it.
                             (let* ((r (store-search-report
                                         store (car args)
                                         (if (argument-option options "--all")
                                             #f
                                             search-hit-limit)))
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
                                       (scan-clauses r (null? hits)))))))))
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
                    (all? (and (argument-option options "--all") #t)))
                (if (not (= 1 (length args)))
                    (usage '(grep <pattern> ["--under" <id>] ["--all"]))
                    (guarded
                      (lambda ()
                        (let* ((r (store-grep store (car args) under all?))
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
                               (answer (items (map (lambda (m) (cons 'match m)) lines))))
                          (if (and (= omitted 0) (= unseen 0))
                              (append answer (scan-clauses r (null? lines)))
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
                                      (scan-clauses r (null? lines)))))))))))
      (cons 'log
            (lambda (store actor args req options state writer cwd)
              (if (not (or (null? args) (= 1 (length args))))
                  (usage '(log [<id>]))
                  (guarded
                    (lambda ()
                      (let ((a (store-log store (if (null? args) #f (car args)))))
                        (if (eq? (car a) 'ok)
                            (items (map (lambda (e)
                                          (list 'entry
                                                (list 'event (car e) (cadr e))
                                                (list 'ts (caddr e))
                                                (list 'actor (cadddr e))
                                                (list 'verb (let ((p (car (cddddr e))))
                                                              (if (pair? p) (car p) 'unknown)))))
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
                 (guarded (lambda () (one-write store actor (list 'tag (car args)) req))))
                (else (usage '(tag [<name>]))))))
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
                  (guarded (lambda () (items (store-conflicts store)))))))))

  (define verbs (verb-table))

  (define (rpc-verbs) (map car verbs))

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
      ((insert set move del link unlink batch commit import-code def) #t)
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
  ;; -2 ENOENT (nothing at that path), -38 ENOTSOCK (something that is
  ;; not a socket), -61 ECONNREFUSED (a socket file whose daemon has
  ;; gone). KEY: All three measured 2026-09-18 on the development machine
  ;; (Darwin arm64) against a real daemon, the last by ending one with an
  ;; uncatchable signal so it could not unlink its own socket. -111 is the
  ;; same refusal on Linux, where libuv reports that errno instead; it is
  ;; listed by name rather than left to be found by a user whose CLI
  ;; stopped working.
  ;;
  ;; NOTE: THOSE ARE OBSERVATIONS ABOUT PLATFORMS, AS OF THAT DATE, not
  ;; properties of this program: a platform's mapping can change, and the
  ;; Linux figure was never measured here at all: `grep -rn '\-111' test/*.sc`
  ;; prints nothing and exits 1, there being no match.
  ;;
  ;; NEVER: THAT SENTENCE SAID THE COMMAND "ANSWERS 0", WHICH IT DOES NOT --
  ;; `grep -c` would. The rule this comment was written under is that a claim
  ;; about what nothing covers must carry the command supporting it, and the
  ;; command was quoted without being run. Quoting a command is the act that
  ;; makes a reader stop checking, so what belongs here is its output.
  ;;
  ;; NEVER: AND THE SENTENCE SAYING NOTHING WOULD GO RED WAS FALSE. It said
  ;; dropping a status from this list turns nothing red. `test/cli-forward.sc`
  ;; ends a daemon with an uncatchable signal, checks the socket it bound is
  ;; still on disk, and requires the next call to answer from the local store
  ;; -- and on this platform that path arrives here as ECONNREFUSED. Remove
  ;; `-61` and that row goes red. The claim was taken from a review that had
  ;; read four files and said so, and the qualifier was lost on the way into
  ;; this comment. A sentence telling the next reader that nothing covers
  ;; something tells them not to look.
  ;;
  ;; What IS uncovered, narrowly: `-2`, `-38` and `-111` have no row of their
  ;; own, and no row contrasts the four with a failure that lost an answer
  ;; mid-flight. That distinction is tethered elsewhere and by other means --
  ;; `test/client-start.sc`'s CS-9 rows separate "provably never went out"
  ;; from "this may already have happened", and they do it by the COUNT of
  ;; bytes written, not by an errno.
  (define (transport-unreachable? outcome)
    (and (pair? outcome)
         (eq? 'transport-error (car outcome))
         (memv (cadr outcome) '(-2 -38 -61 -111))
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
  (define undeclared-verbs '(export-code export-md import-md import-code def snapshot))
  (define (verb-declaration verb)
    (if (memq verb undeclared-verbs) #f incomplete-accepted))

  ;; THE DECLARATION RIDES ON THE REQUEST'S STORE OBJECT for the extent of
  ;; the verb (plan amendment A2), and the outer one is put back on the way
  ;; out -- a normal return AND an escape -- so a nested verb of another
  ;; class is judged by its own class and leaves its caller's intact.
  (define (with-verb-declaration store verb thunk)
    (let ((outer (load-declaration-of store)))
      (dynamic-wind
        (lambda () (load-declaration-set! store (verb-declaration verb)))
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
           (entry (assq verb verbs))
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
        ((or (argument-option options "--store") (argument-option options "--actor")
             (argument-option options "--wire") (argument-option options "--socket"))
         '(error bad-request transport-option-in-rpc))
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
        ((and (not (memq verb '(init describe))) (no-store? store)) => (lambda (a) a))
        ((and id (not after)) '(error bad-request req-without-cursor))
        ((and id (not (tracked-request? verb args)))
         (list 'error 'bad-request 'req-not-tracked verb))
        ((and after (not id)) '(error bad-request cursor-without-req))
        ((and id (not (req-id-ok? id))) '(error bad-request malformed-req-id))
        ((eq? after 'malformed) '(error bad-request malformed-cursor))
        (else (with-verb-declaration store verb
                (lambda ()
                  ((cdr entry) store actor args
                   (and id (make-write-request actor verb (argument-strings options) id after))
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
