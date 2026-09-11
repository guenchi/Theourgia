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
  (export rpc-dispatch rpc-ok? rpc-verbs
          count-argument outline-text)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs exceptions) (rnrs conditions) (rnrs io ports) (rnrs files)
          (rnrs unicode) (rnrs arithmetic fixnums) (rnrs bytevectors)
          (theourgia store) (theourgia reduce) (theourgia log)
          (theourgia project) (theourgia md))

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
    (guard (e ((log-error? e) (describe-log-error e))
              (#t (list 'error 'internal
                        (list 'condition
                              (if (message-condition? e)
                                  (condition-message e)
                                  "unexpected failure")))))
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

  ;; OPTIONS ARE PULLED OUT FIRST AND THE REST STAY POSITIONAL, so that a
  ;; value beginning with a dash is still a value: `set x note --n` takes
  ;; --n as the note because the option scan has already consumed the
  ;; options it knows and stops claiming anything after them.
  (define (take-option args name)
    (let loop ((xs args) (kept '()) (found #f))
      (cond
        ((null? xs) (values found (reverse kept)))
        ((and (not found) (string=? (car xs) name) (pair? (cdr xs)))
         (loop (cddr xs) kept (cadr xs)))
        (else (loop (cdr xs) (cons (car xs) kept) found)))))

  ;; A FLAG TAKES NO VALUE, so it is removed wherever it sits rather
  ;; than consuming the word after it. `take-option` would take the id as
  ;; `--md`'s value and leave the option list looking empty.
  (define (take-flag args name)
    (let loop ((xs args) (kept '()) (found #f))
      (cond
        ((null? xs) (values found (reverse kept)))
        ((string=? (car xs) name) (loop (cdr xs) kept #t))
        (else (loop (cdr xs) (cons (car xs) kept) found)))))

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

  (define (title-of state id)
    (let* ((b (state-read state id))
           (fs (and b (cdr (assq 'fields b))))
           (get (lambda (k) (let ((e (and fs (assq k fs))))
                              (and e (string? (cdr e)) (cdr e))))))
      (or (get 'title) (get 'path) "")))

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
                  (put-string out (make-string (* 2 depth) #\space))
                  (put-string out "- ")
                  (put-string out id)
                  (put-string out "  ")
                  (put-string out (title-of state id))
                  (when mark
                    (put-string out "  ")
                    (put-string out (symbol->string mark)))
                  (put-string out "\n")
                  (walk id (+ depth 1)))))
            (children-of parent)))
        (walk 'root 0)
        (unless (null? orphans)
          (put-string out "orphans:\n")
          (for-each (lambda (id)
                      (put-string out "- ")
                      (put-string out id)
                      (put-string out "  ")
                      (put-string out (title-of state id))
                      (put-string out "\n"))
                    orphans))
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

  (define (one-write store actor intent)
    (let ((answers (with-store-write store (lambda (state view) (list intent)) actor)))
      (car answers)))

  (define (parse-insert store actor args)
    (let*-values (((under rest1) (take-option args "--under"))
                  ((after rest2) (take-option rest1 "--after"))
                  ((title rest3) (take-option rest2 "--title"))
                  ((text rest) (take-option rest3 "--text")))
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
                                  (if text (list (cons 'src text)) '()))))))))

  (define (parse-set store actor args)
    (let-values (((expect rest) (take-option args "--if-unchanged")))
      (let ((intent
              (cond
                ((= 3 (length rest))
                 (list 'set (car rest) (string->symbol (cadr rest)) (caddr rest)))
                ((= 2 (length rest))
                 (list 'set (car rest) (string->symbol (cadr rest))))
                (else #f))))
        (if (not intent)
            (usage '(set <id> <field> <value>))
            (one-write store actor (if expect (list 'expect expect intent) intent))))))

  (define (parse-move store actor args)
    (let-values (((after rest) (take-option args "--after")))
      (if (not (= 2 (length rest)))
          (usage '(move <id> <parent> ["--after" <id>]))
          (one-write store actor
                     (list 'move (car rest)
                           (if (string=? (cadr rest) "root") 'root (cadr rest))
                           after)))))

  (define (parse-edge store actor verb args)
    (if (not (= 3 (length args)))
        (usage (list verb '<from> '<rel> '<to>))
        (one-write store actor
                   (list verb (car args) (string->symbol (cadr args)) (caddr args)))))

  ;; A BATCH IS ONE WRITE SESSION. Read as data by whoever holds the
  ;; bytes, handed here as a list of intents; running them one at a time
  ;; through separate sessions would let another writer interleave, and
  ;; the back-references `(from n)` would then point into a history the
  ;; author did not have.
  (define (run-batch store actor items)
    (list 'batch (with-store-write store (lambda (state view) items) actor)))

  (define (parse-batch store actor args)
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
                             data))))))

  ;; ---- the verbs ------------------------------------------------------------

  ;; THE TABLE IS THE LIST OF VERBS. `eval` is not in it -- it is not
  ;; removed from it by a second rule somewhere, which is how a verb
  ;; comes back by accident -- so an unknown tag and a deliberately
  ;; absent one are the same answer, and there is nowhere to forget.
  (define (verb-table)
    (list
      (cons 'init
            (lambda (store actor args)
              (if (not (null? args))
                  (usage '(init))
                  (guarded (lambda ()
                             (let ((a (store-init! store)))
                               (if (eq? (car a) 'ok) a (cons 'error (cdr a)))))))))
      (cons 'insert (lambda (store actor args) (guarded (lambda () (parse-insert store actor args)))))
      (cons 'set (lambda (store actor args) (guarded (lambda () (parse-set store actor args)))))
      (cons 'move (lambda (store actor args) (guarded (lambda () (parse-move store actor args)))))
      (cons 'del
            (lambda (store actor args)
              (if (not (= 1 (length args)))
                  (usage '(del <id>))
                  (guarded (lambda () (one-write store actor (list 'del (car args))))))))
      (cons 'link (lambda (store actor args) (guarded (lambda () (parse-edge store actor 'link args)))))
      (cons 'unlink (lambda (store actor args) (guarded (lambda () (parse-edge store actor 'unlink args)))))
      (cons 'batch (lambda (store actor args) (guarded (lambda () (parse-batch store actor args)))))
      (cons 'import-md
            (lambda (store actor args)
              (if (not (or (= 1 (length args))
                           (and (= 2 (length args)) (string=? (cadr args) "--allow-delete"))))
                  (usage '(import-md <dir> ["--allow-delete"]))
                  (guarded (lambda ()
                             (list 'import
                                   (import-md store (car args) actor
                                              (= 2 (length args)))))))))
      (cons 'export-md
            (lambda (store actor args)
              (if (not (or (= 1 (length args))
                           (and (= 2 (length args)) (string=? (cadr args) "--with-ids"))))
                  (usage '(export-md <dir> ["--with-ids"]))
                  (guarded (lambda () (export-md store (car args) (= 2 (length args))))))))
      (cons 'adopt
            (lambda (store actor args)
              (if (not (null? args))
                  (usage '(adopt))
                  (guarded (lambda ()
                             (let ((a (store-adopt! store)))
                               (if (eq? (car a) 'adopted)
                                   (cons 'ok (cdr a))
                                   (cons 'error (cdr a)))))))))
      (cons 'check
            (lambda (store actor args)
              (if (not (null? args))
                  (usage '(check))
                  (guarded (lambda () (store-check store))))))
      (cons 'snapshot
            (lambda (store actor args)
              (if (not (null? args))
                  (usage '(snapshot))
                  (guarded (lambda ()
                             (let ((a (store-snapshot! store actor)))
                               (if (eq? (car a) 'written)
                                   (list 'ok (list 'snapshot (cadr a)) (list 'cut (caddr a)))
                                   (cons 'error (cdr a)))))))))
      (cons 'publish
            (lambda (store actor args)
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
            (lambda (store actor args)
              (let-values (((depth rest) (take-option args "--depth")))
                (cond
                  ((not (null? rest)) (usage '(outline ["--depth" <n>])))
                  ((and depth (not (count-argument depth))) (usage '(outline ["--depth" <n>])))
                  (else
                   (guarded (lambda ()
                              (text (if depth
                                        (outline-text (open-and-reduce store)
                                                      (count-argument depth))
                                        (outline-text (open-and-reduce store)))))))))))
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
            (lambda (store actor args)
              (let*-values (((md? rest1) (take-flag args "--md"))
                            ((deep? rest) (take-flag rest1 "--recursive")))
                (cond
                  ((not (= 1 (length rest))) (usage '(read <id> ["--md"] ["--recursive"])))
                  (md?
                   (guarded
                     (lambda ()
                       (let* ((state (open-and-reduce store))
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
                            (let* ((fs (cdr (assq 'fields b)))
                                   (head (let ((e (assq 'heading-src fs))) (if e (cdr e) "")))
                                   (body (let ((e (assq 'src fs))) (if e (cdr e) ""))))
                              (text (string-append head body)))))))))
                  (deep?
                   (guarded
                     (lambda ()
                       (let* ((state (open-and-reduce store))
                              (ids (subtree-ids state (car rest))))
                         (if (not ids)
                             (unknown-id state (car rest))
                             (items (map (lambda (id) (state-read state id)) ids)))))))
                  (else
                   (guarded
                     (lambda ()
                       (let* ((state (open-and-reduce store))
                              (b (state-read state (car rest))))
                         (if b (cons 'ok (list b)) (unknown-id state (car rest)))))))))))
      (cons 'refs
            (lambda (store actor args)
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
            (lambda (store actor args)
              (if (not (= 1 (length args)))
                  (usage '(search <query>))
                  (guarded (lambda ()
                             (items (map (lambda (hit) (cons 'hit hit))
                                         (store-search store (car args)))))))))
      (cons 'log
            (lambda (store actor args)
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
            (lambda (store actor args)
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
                 (guarded (lambda () (one-write store actor (list 'tag (car args))))))
                (else (usage '(tag [<name>]))))))
      (cons 'diff
            (lambda (store actor args)
              (if (not (= 2 (length args)))
                  (usage '(diff <cut> <cut>))
                  (guarded
                    (lambda ()
                      (let ((a (store-diff store (car args) (cadr args))))
                        (if (eq? (car a) 'ok) (items (cadr a)) a)))))))
      (cons 'conflicts
            (lambda (store actor args)
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
  (define (rpc-dispatch store request . rest)
    (let ((actor (if (pair? rest) (car rest) "rpc")))
      (cond
        ((not (list? request))
         (list 'error 'bad-request 'not-a-list))
        ((null? request)
         (list 'error 'bad-request 'empty))
        ((not (symbol? (car request)))
         (list 'error 'bad-request 'tag-not-a-symbol))
        ((not (for-all string? (cdr request)))
         (list 'error 'bad-request 'arguments-not-strings))
        (else
         (let ((entry (assq (car request) verbs)))
           (cond
             ((not entry)
              (list 'error 'unknown-verb (car request) (cons 'verbs (rpc-verbs))))
             ;; `init` is the one verb that runs where there is no store
             ;; yet; every other one is asking about a store that has to
             ;; be there.
             ((and (not (eq? (car request) 'init)) (no-store? store))
              => (lambda (answer) answer))
             (else ((cdr entry) store actor (cdr request)))))))))
)
