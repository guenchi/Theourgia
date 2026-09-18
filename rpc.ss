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
          count-argument outline-text)
  (import (only (theourgia view) view-read)
          (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs conditions) (rnrs io ports) (rnrs files)
          (rnrs unicode) (rnrs arithmetic fixnums) (rnrs bytevectors)
          (theourgia store) (theourgia reduce) (theourgia log)
          (theourgia working) (theourgia baseline) (theourgia code-project) (theourgia code-suggest)
          (theourgia datum-project)
          (only (theourgia datum-code) datum-source-read)
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

  (define (guarded thunk)
    (guard (e ((and (list? e) (pair? e) (eq? (car e) 'error)) e)
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

  (define (usage form) (list 'usage form))

  ;; AN ANSWER SAYS WHICH OF THREE THINGS IT IS, so that whoever renders
  ;; it never has to guess from the shape. `(ok (items …))` is a list to
  ;; be shown one per line, `(ok (text …))` is bytes to be shown as they
  ;; are, and anything else is a single datum. Guessing -- "a list of
  ;; pairs is probably items" -- would classify a block's field alist as
  ;; a list of items the first time someone read a block with two fields.
  (define (items xs) (list 'ok (cons 'items xs)))
  (define (text t) (list 'ok (list 'text t)))

  ;; A STORE THAT IS NOT THERE IS ITS OWN ANSWER, decided before anything
  ;; opens it. Letting the load layer discover it works, but it answers
  ;; with the kind of the file that was missing rather than with the fact
  ;; the caller can act on: there is no store here.
  (define (no-store? store)
    (and (not (file-exists? (string-append store "/meta.sexp")))
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
  ;; `#e1e99999999` is a request to build an exact integer of ten billion
  ;; digits: it does not refuse, it allocates until the machine is
  ;; exhausted, and no check placed after it ever runs because the call
  ;; does not return. A shape test first -- plain ASCII digits, and few
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
  (define (outline-text state . limit)
    (let*-values (((out get) (open-string-output-port)))
      (let* ((depth-limit (if (pair? limit) (car limit) #f))
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
                        (put-string out "\n")
                        (walk id 1))
                      left)))
        (get))))

  ;; ---- intents --------------------------------------------------------------

  ;; OPTION NAMES TRAVEL AS STRINGS IN A USAGE FORM. "--under" is not a
  ;; symbol an R6RS reader will accept, and the point of answering with
  ;; the shape is that an agent can read it back -- a form this library
  ;; could not itself read would be a poor thing to hand out.
  (define insert-usage
    '(insert "--under" <id> ("--after" <id>) "--title" <text> ("--text" <text>)))

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
          (text (argument-option options "--text")) (rest args))
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
                          (append (list (cons 'kind 'section) (cons 'title title))
                                  (if text (list (cons 'src text)) '())))
                    req)))))

  (define (parse-set store actor args req options state)
    (let ((expect (argument-option options "--if-unchanged")) (rest args))
      (let ((intent
              (cond
                ((= 3 (length rest))
                 (list 'set (car rest) (string->symbol (cadr rest))
                       (if (and (string=? (cadr rest) "body")
                                (eq? (code-field (reduction-for store state) (car rest) 'mode) 'datum))
                           (let ((forms (datum-source-read (string->utf8 (caddr rest)))))
                             (if (= (length forms) 1) (caar forms)
                                 (raise '(error bad-source (reason expected-one-form)))))
                           (caddr rest))))
                ((= 2 (length rest))
                 (list 'set (car rest) (string->symbol (cadr rest))))
                (else #f))))
        (if (not intent)
            (usage '(set <id> <field> <value>))
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
    (list 'batch (with-store-write store (lambda (state view) items) actor req)))

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
  (define (verb-table)
    (list
      (cons 'init
            (lambda (store actor args req options state)
              (if (not (null? args))
                  (usage '(init))
                  (guarded (lambda ()
                             (let ((a (store-init! store)))
                               (if (eq? (car a) 'ok) a (cons 'error (cdr a)))))))))
      (cons 'insert (lambda (store actor args req options state) (guarded (lambda () (parse-insert store actor args req options)))))
      (cons 'set (lambda (store actor args req options state) (guarded (lambda () (parse-set store actor args req options state)))))
      (cons 'move (lambda (store actor args req options state) (guarded (lambda () (parse-move store actor args req options)))))
      (cons 'del
            (lambda (store actor args req options state)
              (if (not (= 1 (length args)))
                  (usage '(del <id>))
                  (guarded (lambda () (one-write store actor (list 'del (car args)) req))))))
      (cons 'link (lambda (store actor args req options state) (guarded (lambda () (parse-edge store actor 'link args req)))))
      (cons 'unlink (lambda (store actor args req options state) (guarded (lambda () (parse-edge store actor 'unlink args req)))))
      (cons 'write
            (lambda (store actor args req options state)
              (if (= 2 (length args))
                  (working-write! store state (argument-option options "--writer")
                                  (car args) (cadr args) (argument-option options "--rebase")
                                  (argument-option options "--based-on") (argument-option options "--working-cut")
                                  (argument-option options "--working-parent-writer") (argument-option options "--working-parent"))
                  (usage '(write <block> <bytes>)))))
      ;; `write --restore <version>` IS ITS OWN SHAPE, not `write` with a
      ;; flag: it takes a version and no bytes, and the bytes come from
      ;; the log. Folding it into `write` would make the two-argument
      ;; check above answer for a call that has one.
      (cons 'restore
            (lambda (store actor args req options state)
              (if (= 1 (length args))
                  (working-restore! store state (argument-option options "--writer") (car args))
                  (usage '(restore <version>)))))
      (cons 'commit
            (lambda (store actor args req options state)
              (working-commit! store (argument-option options "--writer") args actor req
                               (argument-option-list options "--working-version"))))
      (cons 'drafts
            (lambda (store actor args req options state)
              (if (null? args) (working-list store state (argument-option options "--writer"))
                  (usage '(drafts)))))
      (cons 'discard
            (lambda (store actor args req options state)
              (if (= 1 (length args))
                  (working-discard! store (argument-option options "--writer") (car args))
                  (usage '(discard <block>)))))
      (cons 'batch (lambda (store actor args req options state) (guarded (lambda () (parse-batch store actor args req)))))
      (cons 'split-suggest
            (lambda (store actor args req options state)
              (if (= 1 (length args))
                  (guarded (lambda () (split-suggest (car args) (argument-option options "--output"))))
                  (usage '(split-suggest <file> ["--output" <review-file>])))))
      (cons 'import-code
            (lambda (store actor args req options state)
              (if (= 1 (length args))
                  (guarded (lambda ()
                    (if (argument-option options "--datum") (import-datum store (car args) actor req)
                        (import-code store (car args) actor req (argument-option options "--allow-delete")))))
                  (usage '(import-code <dir> ["--allow-delete"])))))
      (cons 'export-code
            (lambda (store actor args req options state)
              (if (= 1 (length args))
                  (guarded (lambda ()
                    (cond ((and (argument-option options "--datum") (argument-option options "--raw"))
                           '(error bad-request incompatible-projection-options))
                          ((argument-option options "--datum") (export-datum store (car args)))
                          (else (export-code store (car args) (argument-option options "--raw"))))))
                  (usage '(export-code <dir> ["--raw"])))))
      (cons 'def
            (lambda (store actor args req options state)
              (if (= (length args) 2)
                  (guarded (lambda () (def-datum store (car args) (argument-option options "--under") (cadr args) actor req)))
                  (usage '(def <name> ["--under" <library>] <source>)))))
      (cons 'import-md
            (lambda (store actor args req options state)
              (if (not (= 1 (length args)))
                  (usage '(import-md <dir> ["--allow-delete"]))
                  (guarded (lambda ()
                             (list 'import
                                   (import-md store (car args) actor
                                              (argument-option options "--allow-delete"))))))))
      (cons 'export-md
            (lambda (store actor args req options state)
              (if (not (= 1 (length args)))
                  (usage '(export-md <dir> ["--with-ids"]))
                  (guarded (lambda () (export-md store (car args) (argument-option options "--with-ids")))))))
      (cons 'adopt
            (lambda (store actor args req options state)
              (if (not (null? args))
                  (usage '(adopt))
                  (guarded (lambda ()
                             (let ((a (store-adopt! store)))
                               (if (eq? (car a) 'adopted)
                                   (cons 'ok (cdr a))
                                   (cons 'error (cdr a)))))))))
      (cons 'check
            (lambda (store actor args req options state)
              (if (not (null? args))
                  (usage '(check))
                  (guarded (lambda () (store-check store))))))
      (cons 'snapshot
            (lambda (store actor args req options state)
              (if (not (null? args))
                  (usage '(snapshot))
                  (guarded (lambda ()
                             (let ((a (store-snapshot! store actor)))
                               (if (eq? (car a) 'written)
                                   (list 'ok (list 'snapshot (cadr a)) (list 'cut (caddr a)))
                                   (cons 'error (cdr a)))))))))
      (cons 'publish
            (lambda (store actor args req options state)
              (let ((form '(publish <writer> <segment> <file> [<sha256>])))
                (if (not (or (= 3 (length args)) (= 4 (length args))))
                    (usage form)
                    (let ((segment (count-argument (cadr args))))
                      (if (not (and segment (> segment 0)))
                          (usage form)
                          (guarded
                            (lambda ()
                              (let* ((path (caddr args))
                                     (bytes (and (file-exists? path)
                                                 (call-with-port (open-file-input-port path)
                                                   (lambda (in)
                                                     (let ((b (get-bytevector-all in)))
                                                       (if (eof-object? b)
                                                           (make-bytevector 0)
                                                           b)))))))
                                (if (not bytes)
                                    (list 'error 'no-candidate (list 'path path))
                                    (let* ((sha (if (= 4 (length args))
                                                    (cadddr args)
                                                    (segment-sha bytes)))
                                           (a (log-publish! store (car args) segment bytes sha)))
                                      (if (publish-durable? a)
                                          (cons 'ok (list a))
                                          (cons 'error
                                                (if (eq? (car a) 'error) (cdr a) (list a)))))))))))))))
      (cons 'outline
            (lambda (store actor args req options state)
              (let ((depth (argument-option options "--depth")) (rest args))
                (cond
                  ((not (null? rest)) (usage '(outline ["--depth" <n>])))
                  ((and depth (not (count-argument depth))) (usage '(outline ["--depth" <n>])))
                  (else
                   (guarded (lambda ()
                              (text (if depth
                                        (outline-text (reduction-for store state)
                                                      (count-argument depth))
                                        (outline-text (reduction-for store state)))))))))))
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
            (lambda (store actor args req options state)
              (let ((md? (argument-option options "--md"))
                    (deep? (argument-option options "--recursive")) (rest args))
                (cond
                  ((not (= 1 (length rest))) (usage '(read <id> ["--md"] ["--recursive"])))
                  ((or (argument-option options "--working") (argument-option options "--working-info"))
                   (if (or md? deep?) '(error bad-request incompatible-working-options)
                       (working-read store state (argument-option options "--writer") (car rest) (argument-option options "--working-info"))))
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
            (lambda (store actor args req options state)
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
      (cons 'search
            (lambda (store actor args req options state)
              (if (not (= 1 (length args)))
                  (usage '(search <query>))
                  (guarded (lambda ()
                             (items (map (lambda (hit) (cons 'hit hit))
                                         (store-search store (car args)))))))))
      (cons 'log
            (lambda (store actor args req options state)
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
            (lambda (store actor args req options state)
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
            (lambda (store actor args req options state)
              (if (not (= 2 (length args)))
                  (usage '(diff <cut> <cut>))
                  (guarded
                    (lambda ()
                      (let ((a (store-diff store (car args) (cadr args))))
                        (if (eq? (car a) 'ok) (items (cadr a)) a)))))))
      (cons 'conflicts
            (lambda (store actor args req options state)
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
               (and (pair? rest) (pair? (cdr rest)) (cadr rest))))))))

  ;; Parsed nodes are the CLI's internal handoff. All verb arguments and
  ;; their fingerprint are derived from the same tokenization.
  ;; ⛔ `state` IS A VALUE THE CALLER ALREADY HOLDS, AND HANDLERS MAY NOT
  ;; CHANGE IT. #f means "there is none, load one" -- which is what every
  ;; caller that reaches a store by its path passes. A daemon passes the
  ;; reduction it published: one fold, shared by every reader, never
  ;; mutated after it was published. A handler that modified it would be
  ;; modifying what every other reader sees, in another process, with
  ;; nothing to report it.
  ;;
  ;; ⚠️ AND IT IS ONLY EVER THE WHOLE STORE. A verb asking for a
  ;; historical cut is asking for a DIFFERENT reduction and still opens
  ;; the log for it; passing this one there would answer a question about
  ;; the past with the present.
  (define (reduction-for store state)
    (or state (open-and-reduce store)))

  (define (rpc-dispatch-parsed store verb nodes actor . rest)
    (let* ((state (and (pair? rest) (car rest)))
           (entry (assq verb verbs))
           (id (argument-option nodes "--req"))
           (cursor (argument-option nodes "--cursor"))
           (after (and cursor (parse-after cursor)))
           (options (argument-remove nodes '("--req" "--cursor")))
           (args (argument-positionals options)))
      (cond
        ;; THE VERB IS SPELLED, NOT SENT BACK. It came from a caller, so it
        ;; can be any symbol at all -- and a verb a caller got wrong is
        ;; exactly the kind that is not wire-safe. `show me` came back as
        ;; `show\x20;me` and the reader that had asked the question could
        ;; not parse the answer to it. A string carries the same fact and
        ;; survives the wire whatever it holds.
        ((not entry)
         (list 'error 'unknown-verb (list 'spelling (datum-spelling verb))
               (cons 'verbs (rpc-verbs))))
        ((or (argument-option options "--store") (argument-option options "--actor")
             (argument-option options "--wire") (argument-option options "--socket"))
         '(error bad-request transport-option-in-rpc))
        ((and (not (eq? verb 'init)) (no-store? store)) => (lambda (a) a))
        ((and id (not after)) '(error bad-request req-without-cursor))
        ((and id (not (tracked-request? verb args)))
         (list 'error 'bad-request 'req-not-tracked verb))
        ((and after (not id)) '(error bad-request cursor-without-req))
        ((and id (not (req-id-ok? id))) '(error bad-request malformed-req-id))
        ((eq? after 'malformed) '(error bad-request malformed-cursor))
        (else ((cdr entry) store actor args
               (and id (make-write-request actor verb (argument-strings options) id after))
               options state)))))

  ;; `<writer>:<seq>`, BY SHAPE AND NEVER THROUGH `read`. The reader
  ;; implements the whole of Scheme's numeric syntax, and `#e1e99999999`
  ;; is eleven characters asking it to build an integer of ten billion
  ;; digits: it does not refuse, it allocates until the machine is gone,
  ;; and no check placed after it ever runs.
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
