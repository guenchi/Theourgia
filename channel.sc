#!r6rs
;; Copyright 2018 - 2026 guenchi
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

;; THE REVIEW CHANNEL: A READER THAT SEES A CUT'S ROOTS AND NOTHING ELSE.
;;
;; `scope <dir> --cut <cut> --roots <id> ... --for <actor>` builds a NEW
;; store at <dir> out of the main store's blocks under the roots, as they
;; are at the cut, written as ordinary records of the new store; the
;; reviewer works there and never in the main store. `collect <dir>`
;; brings what the reviewer wrote back under a letter in the main store.
;; Four verbs:
;;
;;   scope           route child: a program over ordinary requests
;;   collect         route child: review-results, then collect-into
;;   review-results  route daemon: a read of the scoped store
;;   collect-into    route daemon: a write of the main store
;;
;; NEVER: NO DAEMON WRITES OR PUBLISHES ANOTHER STORE. scope and collect run
;; in core.sc -- the caller's process, or the MCP shell's child -- and send
;; each store its own requests through its own route: its daemon when one
;; serves it, else the local route. A daemon
;; publishes its own store's reduction and nothing else; a program that
;; asked one daemon to write a second store would make it publish a
;; reduction it does not hold.
;;
;; THIS IS ITS OWN LIBRARY because only these four verbs need it: core.sc
;; and the dispatcher enter it when one of them is asked, so every other
;; call starts without it.

(library (theourgia channel)
  (export scope-verb collect-verb review-results-verb collect-into-verb)
  (import (rnrs)
          (only (chezscheme) format getenv current-directory iota)
          (only (theourgia rpc) rpc-dispatch-parsed)
          (only (theourgia arguments) parse-arguments argument-option argument-option-list)
          (only (theourgia client) socket-path call! request-frame answer-field readable-shape? plain-datum)
          (only (theourgia ffi) entry-type)
          (only (theourgia store) with-store-write judgement-refusal?)
          (only (theourgia reduce) state-read state-outline outline-subtree state-datum
                caller-payload-reason caller-fields-reason reserved-relation-names
                block-creation event-actor state-put-events block-id state-event-past cut-covers?
                state-field-contested? state-block-ids))

  ;; A REQUEST OF THE WRONG SHAPE IS ANSWERED WITH THE WORD `usage`: the
  ;; form itself is the catalogue's (rpc.sc), which core.sc and the
  ;; dispatcher answer.

  ;; ---- one request to one store, through that store's own route -------------
  ;;
  ;; THE ROUTE IS THE ONE THE COMMAND LINE TAKES (core.sc): the store's daemon
  ;; when its socket is there, else this process; a socket with nobody behind
  ;; it is a daemon that has gone, and the request reached nobody, so it runs
  ;; here. Once bytes have gone out the request may have been carried out:
  ;; that is `transport-unknown`, never a second attempt. THEOURGIA_LOCAL=1
  ;; sends everything here, as it does for the command line.
  ;; SOCKET, when given, is the one the caller named for this store
  ;; (`--socket`), in place of the default path.
  (define (routed store verb args actor . socket-given)
    (define (here) (rpc-dispatch-parsed store verb (parse-arguments verb args) actor #f #f))
    (let ((socket (if (and (pair? socket-given) (string? (car socket-given))) (car socket-given) (socket-path store))))
      (if (or (equal? (getenv "THEOURGIA_LOCAL") "1") (eq? (entry-type socket) 'absent))
          (here)
          (let ((outcome (call! socket
                                (request-frame store verb args
                                               (list (cons 'actor actor) (cons 'mode 'wire)
                                                     (cons 'cwd (current-directory))))
                                30000)))
            (cond
              ((eq? (car outcome) 'answer) (envelope-answer (cadr outcome)))
              ((eq? (car outcome) 'no-daemon) (here))
              ((eq? (car outcome) 'not-sent) (cadr outcome))
              (else (list 'error 'transport-unknown (list 'reason (cadr outcome)))))))))

  ;; THE DAEMON'S ANSWER IS ITS RENDERED WIRE TEXT, in an envelope; it is read
  ;; back into the datum the local route would have answered.
  (define (envelope-answer bytes)
    (let* ((text (utf8->string bytes))
           (envelope (and (readable-shape? text)
                          (guard (e (#t #f)) (read (open-string-input-port text)))))
           (out (and envelope (answer-field envelope 'stdout string?))))
      (or (and out (plain-datum out))
          '(error transport-unknown (reason unreadable-answer)))))

  ;; plain-datum is (theourgia client)'s: every reader of a daemon's rendered
  ;; text reads with the one rule.

  (define (ok? a) (and (pair? a) (eq? (car a) 'ok)))
  (define (clause a key)
    (let ((p (and (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) key))) (cdr a)))))
      (and p (cdr p))))
  (define (field fs key) (let ((p (assq key fs))) (and p (cdr p))))
  (define (written x) (format "~s" x))
  (define (absolute dir cwd)
    (if (and (> (string-length dir) 0) (char=? (string-ref dir 0) #\/))
        dir
        (string-append (or cwd (current-directory)) "/" dir)))

  ;; ---- scope ----------------------------------------------------------------

  ;; A CODE BLOCK'S DERIVED FIELDS ARE THE VIEW'S, NOT THE BLOCK'S. `read`
  ;; answers with the view (view.sc), which puts a text block's `name` and
  ;; `doc`, and a datum block's `name` and `names`, in place of anything
  ;; stored under those keys; copied, they would become stored fields the
  ;; block never had. They are left out; the scoped store's own view derives
  ;; them again from the copied source.
  (define (derived-keys fs)
    (if (eq? (field fs 'kind) 'code)
        (case (field fs 'mode) ((text) '(name doc)) ((datum) '(name names)) (else '()))
        '()))

  ;; A FIELD CONTESTED AT THE CUT, as a read over the wire shows it: `(conflict
  ;; ((<value> <writer> <seq>) ...))` with two candidates or more. A value
  ;; written once can have that shape too (reduce.sc, state-field-contested?),
  ;; and a read cannot tell the two apart; this test admits only the exact
  ;; shape, so a value that merely starts with `conflict` is copied.
  (define (contested? v)
    (and (list? v) (= 2 (length v)) (eq? (car v) 'conflict) (list? (cadr v)) (>= (length (cadr v)) 2)
         (for-all (lambda (c) (and (list? c) (= 3 (length c)) (string? (cadr c))
                                   (integer? (caddr c)) (exact? (caddr c))))
                  (cadr v))))

  ;; THE LETTER'S OWN FIELDS, the main letter's and its copy's alike.
  (define (letter-fields for cut-text roots dir)
    (list (cons 'kind 'doc) (cons 'to for) (cons 'status "unread") (cons 'cut cut-text)
          (cons 'roots roots) (cons 'scope dir)))

  ;; THE WRITE'S OWN VALIDATOR, asked of every record scope will write before
  ;; anything is written: the same question the write asks, so the scope is
  ;; refused here rather than half-written there.
  (define (put-reason fields)
    (or (caller-fields-reason fields)
        (caller-payload-reason (list 'put (append fields '((parent . root) (ord . 1)))))))

  ;; SOCKET, when given, is the main store's daemon socket the caller named.
  (define (scope-verb store actor args options cwd . socket-given)
    (let ((socket (and (pair? socket-given) (car socket-given)))
          (cut (argument-option options "--cut"))
          (roots (argument-option-list options "--roots"))
          (for (argument-option options "--for")))
      (cond
        ((not (and (= 1 (length args)) (string? cut) (pair? roots) (string? for)))
         'usage)
        (else
         (let ((dir (absolute (car args) cwd)))
;; Through the store's one door to the filesystem: an entry that
           ;; cannot be read raises, rather than reading as absent.
           (if (not (eq? (entry-type dir) 'absent))
               (list 'error 'scope-exists (list 'dir dir))
               (scope-read store socket actor dir cut roots for)))))))

  ;; (1) THE MAIN STORE'S BLOCKS AT THE CUT. Each root is read through the
  ;; main store's route at the cut, so the answer is the cut's bytes or the
  ;; refusal (cut-moved, cut-unavailable, unknown-id), and a refusal is the
  ;; answer with nothing written anywhere. A tag is resolved once: the first
  ;; read's receipt names the literal, and every later read asks for it.
  (define (cut-text at) (if (string? at) at (written at)))
  (define (scope-read store socket actor dir cut roots for)
    (let loop ((rs roots) (at cut) (got '()))
      (if (null? rs)
          (scope-plan store socket actor dir (cut-text at) (reverse got) roots for)
          (let ((a (routed store 'read (list (car rs) "--cut" (cut-text at) "--recursive") actor socket)))
            (if (not (ok? a))
                a
                (let ((items (clause a 'items)) (c (clause a 'cut)))
                  (loop (cdr rs) (if c (car c) at) (cons (cons (car rs) items) got))))))))

  (define (scope-plan store socket actor dir cut-text per-root roots for)
    (let* ((all (apply append (map cdr per-root)))
           (ids (map (lambda (rec) (field rec 'id)) all))
           (overlap (let dup ((xs ids) (seen '()))
                      (cond ((null? xs) #f) ((member (car xs) seen) (car xs)) (else (dup (cdr xs) (cons (car xs) seen)))))))
      (cond
        (overlap (list 'error 'bad-request '(reason roots-overlap) (list 'block overlap)))
        ;; A ROOT THAT IS RETIRED AT THE CUT IS NOT A BLOCK TO REVIEW: a read
        ;; answers it, tombstone and all, and a copy would be a live block.
        ((find (lambda (rec) (field rec 'deleted)) all)
         => (lambda (rec) (list 'error 'bad-request '(reason root-deleted) (list 'block (field rec 'id)))))
        (else (scope-build store socket actor dir cut-text all ids roots for)))))

  ;; (2) WHAT WILL BE WRITTEN, and the preflight over all of it. A copy is the
  ;; block's settled fields as they are at the cut, less the view's derived
  ;; ones, with `origin` and `origin-cut`; a contested field is listed and not
  ;; copied. An edge is copied when both its ends are retained, else dropped
  ;; and listed.
  ;; A COPY GOES UNDER ITS PARENT'S COPY ONLY WHEN THAT COPY IS WRITTEN BEFORE
  ;; IT, which outline order gives for every block whose position is a tree.
  ;; Blocks whose positions make a cycle are each listed as a root, so one
  ;; names a parent copied after it; it is placed at the top level instead of
  ;; naming an insert that has not run.
  ;; A requested root is at the top level whatever its position says: two
  ;; roots in a cycle of positions are each read as a root.
  (define (placed-parents recs roots)
    (let loop ((rs recs) (seen '()) (out '()))
      (if (null? rs)
          (reverse out)
          (let* ((rec (car rs)) (pos (field rec 'position))
                 (parent (and (not (member (field rec 'id) roots))
                              (pair? pos) (string? (car pos)) (member (car pos) seen) (car pos))))
            (loop (cdr rs) (cons (field rec 'id) seen) (cons parent out))))))

  (define (scope-build store socket actor dir cut-text recs ids roots for)
    (let* ((contested '())
           (copies
             (map (lambda (rec parent)
                    (let* ((id (field rec 'id)) (fs (field rec 'fields)) (skip (derived-keys fs)))
                      (list id parent
                            (append
                              (filter (lambda (f)
                                        (cond ((memq (car f) skip) #f)
                                              ((contested? (cdr f))
                                               (set! contested (cons (list 'contested id (car f)) contested))
                                               #f)
                                              (else #t)))
                                      fs)
                              (list (cons 'origin id) (cons 'origin-cut cut-text))))))
                  recs (placed-parents recs roots)))
           (edges (apply append
                         (map (lambda (rec)
                                (map (lambda (e) (list (field rec 'id) (car e) (cdr e)))
                                     (or (field rec 'edges) '())))
                              recs)))
           (kept (filter (lambda (e) (member (caddr e) ids)) edges))
           (dropped (map (lambda (e) (cons 'dropped-edge e))
                         (filter (lambda (e) (not (member (caddr e) ids))) edges)))
           (letter (letter-fields for cut-text roots dir))
           (unwritable
             (append
               (let ((r (put-reason letter))) (if r (list (list "letter" 'fields r)) '()))
               (apply append
                      (map (lambda (c)
                             (let ((r (put-reason (caddr c)))) (if r (list (list (car c) 'fields r)) '())))
                           copies))
               ;; the placement the write checks: a doc only at the top level
               (apply append
                      (map (lambda (c)
                             (if (and (cadr c) (eq? (field (caddr c) 'kind) 'doc))
                                 (list (list (car c) 'kind 'doc-must-be-top-level))
                                 '()))
                           copies))
               (apply append
                      (map (lambda (e)
                             (if (memq (cadr e) reserved-relation-names)
                                 (list (list (car e) (cadr e) 'relation-name-reserved))
                                 (let ((r (caller-payload-reason (list 'link (car e) (cadr e) (caddr e)))))
                                   (if r (list (list (car e) (cadr e) r)) '()))))
                           kept)))))
      (if (pair? unwritable)
          (list 'error 'scope-unwritable (list 'items unwritable))
          (scope-write store socket actor dir cut-text letter copies kept dropped (reverse contested)))))

  (define (failed dir step answer . more)
    (append (list 'error 'scope-failed (list 'dir dir) (list 'step step)) more (list (list 'answer answer))))

  ;; THE ID A BATCH ITEM'S INSERT CREATED is named by its event (block-id),
  ;; as the store names it.
  (define (created-id item)
    (let* ((ev (and (ok? item) (clause item 'events)))
           (e (and ev (pair? (car ev)) (car (car ev)))))
      (and (pair? e) (string? (car e)) (integer? (cdr e)) (block-id (car e) (cdr e)))))

  (define (batch-items answer)
    (and (pair? answer) (eq? (car answer) 'batch) (pair? (cdr answer)) (list? (cadr answer)) (cadr answer)))

  (define (batch-ok? answer)
    (let ((items (batch-items answer))) (and items (for-all ok? items))))

  ;; (2), the writing: init, then the letter copy and the copies as one batch
  ;; (a parent named by its intent's place, `(from k)`), then the edges
  ;; between the copies, then the baseline. (3) The main letter last, then
  ;; its id on the copy. A failure after init leaves the directory as it is
  ;; and says which step it reached.
  (define (scope-write store socket actor dir cut-text letter copies kept dropped contested)
    (let ((init (routed dir 'init '() actor)))
      (if (not (ok? init))
          init
          (let* ((index (let loop ((cs copies) (k 1) (out '()))
                          (if (null? cs) out (loop (cdr cs) (+ k 1) (cons (cons (car (car cs)) k) out)))))
                 (intents
                   (cons (list 'insert 'root #f letter)
                         (map (lambda (c)
                                (let ((p (and (cadr c) (assoc (cadr c) index))))
                                  (list 'insert (if p (list 'from (cdr p)) 'root) #f (caddr c))))
                              copies)))
                 (made (routed dir 'batch (list (written intents)) actor)))
            (if (not (batch-ok? made))
                (failed dir 'import made)
                (let* ((ids (map created-id (batch-items made)))
                       (copy-of (map (lambda (c id) (cons (car c) id)) copies (cdr ids)))
                       (letter-copy (car ids))
                       (links (map (lambda (e) (list 'link (cdr (assoc (car e) copy-of)) (cadr e)
                                                     (cdr (assoc (caddr e) copy-of))))
                                   kept))
                       (linked (and (pair? links) (routed dir 'batch (list (written links)) actor))))
                  (if (and linked (not (batch-ok? linked)))
                      (failed dir 'import linked)
                      (let* ((read-back (routed dir 'read (list letter-copy) actor))
                             (base (and (ok? read-back) (clause read-back 'cut)))
                             (baseline (and base (written (car base))))
                             (set-base (and baseline (routed dir 'set (list letter-copy "baseline" baseline) actor))))
                        (if (not (and set-base (ok? set-base)))
                            (failed dir 'baseline (or set-base read-back))
                            (let* ((main (routed store 'batch (list (written (list (list 'insert 'root #f letter)))) actor socket))
                                   (main-id (and (batch-ok? main) (created-id (car (batch-items main))))))
                              (if (not main-id)
                                  (failed dir 'letter main '(main-letter none))
                                  (let ((set-letter (routed dir 'set (list letter-copy "letter" main-id) actor)))
                                    (if (not (ok? set-letter))
                                        (failed dir 'letter set-letter (list 'main-letter main-id))
                                        (append
                                          (list 'ok (list 'scope dir) (list 'cut cut-text) (list 'letter main-id)
                                                (list 'letter-copy letter-copy) (list 'baseline baseline)
                                                (list 'blocks (length copies)) (list 'edges (length kept))
                                                (list 'dropped-edges (length dropped))
                                                (list 'contested-fields (length contested)))
                                          dropped contested))))))))))))))

  ;; ---- review-results: the reviewer's blocks, as one datum ------------------
  ;;
  ;; THE LETTER COPY IS THE FIRST BLOCK THE SCOPED STORE MADE: scope writes it
  ;; first, in the first batch, before the store has another writer. FIRST
  ;; IS CAUSAL, NOT THE ORDER OF THE HISTORY: after an adoption the store has
  ;; a second writer, and where its records fall among the first writer's is
  ;; not a statement about which came first. So the letter copy is the
  ;; first put of some writer that every other writer's first put has in its
  ;; past. THE RESULTS are every live block that has no
  ;; `origin` field, is not the letter copy, and was created by the letter's
  ;; `to` actor -- the actor of the block's creating put, read through the
  ;; history (block-creation), not the log writer, which adoption changes.
  ;; A verdict is a result directly under the letter copy; a finding is a
  ;; result under a verdict. Neither class rule is enforced here (they are
  ;; write-time rules for later): a finding with no `about` is listed as
  ;; `(no-about <id>)`, a verdict without one of the four conditions as
  ;; `(missing <id> <field>)`, and nothing is refused.
  (define condition-fields '(model approval sandbox effort))

  (define (causally-first-put r)
    (let* ((firsts (let loop ((es (state-put-events r)) (seen '()) (out '()))
                     (cond ((null? es) (reverse out))
                           ((member (car (car es)) seen) (loop (cdr es) seen out))
                           (else (loop (cdr es) (cons (car (car es)) seen) (cons (car es) out)))))))
      (find (lambda (p)
              (for-all (lambda (q)
                         (or (equal? p q)
                             (let ((past (state-event-past r q))) (and past (cut-covers? past (list p))))))
                       firsts))
            firsts)))

  ;; EVERY BLOCK, IN OUTLINE ORDER FIRST. Deletion does not cascade: a
  ;; finding under a verdict that was deleted is alive, but the outline no
  ;; longer reaches it from the top. The blocks the outline does not reach
  ;; follow, in the store's own order; the caller keeps the live ones.
  (define (live-in-order r)
    (let* ((rows (state-outline r))
           (reached (apply append
                           (map (lambda (row) (outline-subtree rows (caddr row)))
                                (filter (lambda (row) (eq? (car row) 'root)) rows)))))
      (append reached (filter (lambda (id) (not (member id reached))) (state-block-ids r)))))

  (define (review-results-verb r args)
    (if (not (null? args))
        'usage
        (let* ((first (causally-first-put r))
               (letter-copy (and first (block-id (car first) (cdr first))))
               (lb (and letter-copy (state-read r letter-copy)))
               (lfs (and lb (field lb 'fields)))
               (to (and lfs (field lfs 'to)))
               (main-letter (and lfs (field lfs 'letter))))
          (cond
            ((not (and (string? to) (field lfs 'scope)))
             '(error bad-request (reason not-a-scoped-store)))
            ((not (string? main-letter))
             (list 'error 'bad-request '(reason letter-not-set) (list 'letter-copy letter-copy)))
            (else (review-results-of r letter-copy to main-letter))))))

;; A RESULT'S FIELDS ARE ITS SETTLED ONES, each with the actor of the
  ;; record that set it, so the author's own writes to a reviewer block show
  ;; under the author's name. A contested field is not a value and is left
  ;; out, as scope leaves it out of a copy.
  (define (settled-fields r events id)
    (let ((cands (hashtable-ref events id '())))
      (filter (lambda (x) x)
              (map (lambda (f)
                     (and (not (state-field-contested? r id (car f) (cdr f)))
                          (let ((c (assq (car f) cands)))
                            (list (car f) (cdr f)
                                  (and c (= 1 (length (cadr c)))
                                       (event-actor r (cdr (car (cadr c)))))))))
                   (field (state-read r id) 'fields)))))

  (define (review-results-of r letter-copy to main-letter)
    (let* ((events (let ((t (make-hashtable string-hash string=?)))
                     (for-each (lambda (b) (hashtable-set! t (cadr b) (cadr (assq 'fields (cddr b)))))
                               (state-datum r))
                     t))
           (result? (lambda (id)
                      (let* ((b (state-read r id)) (fs (field b 'fields)) (made (block-creation r id)))
                        (and b (not (equal? id letter-copy)) (not (field b 'deleted)) (not (assq 'origin fs))
                             made (equal? (car made) to)))))
           (ids (filter result? (live-in-order r)))
           (parent-of (lambda (id) (let ((p (field (state-read r id) 'position))) (and (pair? p) (car p)))))
           (verdict? (lambda (id) (equal? (parent-of id) letter-copy)))
           (finding? (lambda (id) (let ((p (parent-of id))) (and (member p ids) (verdict? p) #t))))
           (abouts (lambda (id) (filter (lambda (e) (eq? (car e) 'about)) (or (field (state-read r id) 'edges) '()))))
           (target (lambda (to-id)
                     (let* ((b (state-read r to-id)) (o (and b (field (field b 'fields) 'origin))))
                       (if (string? o) (list 'origin o) (list 'scoped to-id)))))
           (item
             (lambda (id)
               (let ((p (parent-of id)))
                 (list 'block id
                       (list 'parent (if (member p ids) p 'letter))
                       (list 'fields (settled-fields r events id))
                       (list 'about (map (lambda (e) (target (cdr e))) (abouts id)))))))
           (no-about (map (lambda (id) (list 'no-about id))
                          (filter (lambda (id) (and (finding? id) (null? (abouts id)))) ids)))
           (missing (apply append
                           (map (lambda (id)
                                  (let ((fs (field (state-read r id) 'fields)))
                                    (map (lambda (k) (list 'missing id k))
                                         (filter (lambda (k) (not (field fs k))) condition-fields))))
                                (filter verdict? ids)))))
      (append (list 'ok (list 'results (list 'letter main-letter) (list 'to to) (cons 'blocks (map item ids))))
              no-about missing)))

  ;; ---- collect-into: the results under the main letter ----------------------
  ;;
  ;; TWO WRITES, EACH PLANNED AT THE LOCKED POINT from the state it finds and
  ;; the datum: the copies that are not there yet, then the `about` edges
  ;; the logical edge set does not have. A copy's identity is the
  ;; `scoped-id` in its creating put's payload (block-creation), not its
  ;; current field or place, so an unset field or a moved copy still
  ;; matches, and two that match refuse. So a second collect inserts and
  ;; links nothing, a collect stopped between the two writes finishes the
  ;; links, and two collectors serialise on the store's lock.
  ;;
  ;; NEVER: NEITHER WRITE CARRIES THE REQUEST. Its identity is what makes a
  ;; retried request a replay; the second write under the same identity
  ;; would be read as the first one's replay and not run. What makes a
  ;; retry safe here is the plan above, which is a function of the state.
  ;; And the records are the reviewer's: their actor is the datum's `to`,
  ;; not the author who asked for the collect.
  (define (collect-into-verb store args options)
    (let ((text (argument-option options "--results")))
      (if (not (and (= 1 (length args)) (string? text)))
          'usage
          (let ((d (plain-datum text)))
            (if (not (results-datum? d))
                '(error bad-request (reason results-malformed))
                (collect-into-write store (car args) d))))))

  (define (results-part d key) (let ((p (assq key (cdr d)))) (and p (if (eq? key 'blocks) (cdr p) (cadr p)))))
  (define (block-part b key) (let ((p (assq key (cddr b)))) (and p (cadr p))))

  ;; THE DATUM IS THE CALLER'S, and is read as one: every part this verb
  ;; reads is checked for its shape before anything is planned.
  (define (results-datum? d)
    (and (list? d) (pair? d) (eq? (car d) 'results)
         (for-all (lambda (x) (and (list? x) (pair? x) (or (eq? (car x) 'blocks) (pair? (cdr x))))) (cdr d))
         (string? (results-part d 'to)) (string? (results-part d 'letter)) (list? (results-part d 'blocks))
         ;; one result one id: two blocks with one id would both be planned
         ;; as new, and the second write would find them ambiguous
         (let ids ((bs (results-part d 'blocks)) (seen '()))
           (cond ((null? bs) #t)
                 ((and (pair? (car bs)) (pair? (cdar bs)) (member (cadr (car bs)) seen)) #f)
                 (else (ids (cdr bs) (if (and (pair? (car bs)) (pair? (cdar bs))) (cons (cadr (car bs)) seen) seen)))))
         (for-all (lambda (b)
                    (and (list? b) (> (length b) 2) (eq? (car b) 'block) (string? (cadr b))
                         (for-all (lambda (x) (and (pair? x) (pair? (cdr x)))) (cddr b))
                         (let ((p (block-part b 'parent))) (or (string? p) (eq? p 'letter)))
                         (let ((fs (block-part b 'fields)))
                           (and (list? fs)
                                (for-all (lambda (f) (and (list? f) (= 3 (length f)) (symbol? (car f)))) fs)))
                         (let ((as (or (block-part b 'about) '())))
                           (and (list? as)
                                (for-all (lambda (a) (and (list? a) (= 2 (length a)) (memq (car a) '(origin scoped))
                                                          (string? (cadr a))))
                                         as)))))
                  (results-part d 'blocks))))

  ;; scoped id -> the main blocks whose creating put carries it
  (define (copies-by-scoped-id r)
    (let ((t (make-hashtable string-hash string=?)))
      (for-each
        (lambda (e)
          (let* ((id (block-id (car e) (cdr e))) (made (block-creation r id))
                 (fs (and made (pair? (cdr made)) (pair? (cddr made)) (cadr (cdr made))))
                 (sid (and (list? fs) (let ((p (assq 'scoped-id fs))) (and p (cdr p))))))
            (when (string? sid) (hashtable-update! t sid (lambda (xs) (cons id xs)) '()))))
        (state-put-events r))
      t))

  (define (all-ok answers) (for-all ok? answers))

;; THE NEW RESULTS, EACH AFTER ITS PARENT when the parent is new too. The
  ;; datum lists them in the scoped store's order, which for a subtree whose
  ;; top was deleted is not parent-first; an insert can only name an earlier
  ;; one. A result whose parent is never reached (a cycle) keeps its place,
  ;; and goes under the letter.
  (define (parent-first bs)
    (let loop ((pending bs) (out '()))
      (if (null? pending)
          (reverse out)
          (let* ((ids (map cadr pending))
                 (ready (filter (lambda (b) (let ((p (block-part b 'parent))) (not (and (string? p) (member p ids)))))
                                pending)))
            (if (null? ready)
                (append (reverse out) pending)
                (loop (filter (lambda (b) (not (memq b ready))) pending) (append (reverse ready) out)))))))

  (define (collect-into-write store letter d)
    (let* ((to (results-part d 'to)) (blocks (results-part d 'blocks))
           (refused #f) (inserted 0) (linked 0) (already 0) (unresolved '())
           (one (lambda (t sid)
                  (let ((xs (hashtable-ref t sid '())))
                    (cond ((null? xs) #f)
                          ((pair? (cdr xs))
                           (unless refused
                             (set! refused (list 'error 'collect-ambiguous (list 'scoped-id sid) (list 'copies xs))))
                           #f)
                          (else (car xs))))))
           (first
             (with-store-write store
               (lambda (state view)
                 (if (not (state-read state letter))
                     (begin (set! refused (list 'error 'unknown-id letter)) '())
                     (let* ((t (copies-by-scoped-id state))
                            (new (parent-first (filter (lambda (b) (not (one t (cadr b)))) blocks)))
                            (index (let loop ((bs new) (k 0) (out '()))
                                     (if (null? bs) out (loop (cdr bs) (+ k 1) (cons (cons (cadr (car bs)) k) out)))))
                            (intents
                              (map (lambda (b k)
                                     (let* ((p (block-part b 'parent))
                                            (parent (cond ((not (string? p)) letter)
                                                          ((one t p) => (lambda (id) id))
                                                          ((assoc p index)
                                                           => (lambda (e) (if (< (cdr e) k) (list 'from (cdr e)) letter)))
                                                          (else letter))))
                                       (list 'insert parent #f
                                             (append (map (lambda (f) (cons (car f) (cadr f)))
                                                          (filter (lambda (f) (not (eq? (car f) 'scoped-id)))
                                                                  (block-part b 'fields)))
                                                     (list (cons 'scoped-id (cadr b)))))))
                                   new (iota (length new)))))
                       (if refused '() (begin (set! inserted (length intents)) intents)))))
               to #f #f)))
      (cond
        (refused refused)
        ((not (all-ok first)) (if (and (= 1 (length first)) (judgement-refusal? (car first))) (car first) (cons 'batch (list first))))
        (else
         (let ((second
                 (with-store-write store
                   (lambda (state view)
                     ;; THE EDGES ARE A SET, AND SO IS THE PLAN: two targets
                     ;; that resolve to one block, or one written twice, are
                     ;; one edge, linked once or counted once as already there.
                     ;; A TARGET THAT RESOLVES TO NOTHING -- a reader block
                     ;; deleted before its first collection, so never copied --
                     ;; is not linked and is NAMED in the answer, `(unresolved
                     ;; <result> <target>)`, rather than dropped.
                     (let* ((t (copies-by-scoped-id state))
                            (pairs
                              (let loop ((ps (apply append
                                                    (map (lambda (b)
                                                           (let ((from (one t (cadr b))))
                                                             (map (lambda (target)
                                                                    (list from
                                                                          (if (eq? (car target) 'origin)
                                                                              (cadr target)
                                                                              (one t (cadr target)))
                                                                          (cadr b) target))
                                                                  (or (block-part b 'about) '()))))
                                                         blocks)))
                                         (out '()))
                                (cond ((null? ps) (reverse out))
                                      ((not (and (car (car ps)) (cadr (car ps))))
                                       (let ((u (list 'unresolved (caddr (car ps)) (cadddr (car ps)))))
                                         (unless (member u unresolved) (set! unresolved (cons u unresolved))))
                                       (loop (cdr ps) out))
                                      ((member (cons (car (car ps)) (cadr (car ps))) out) (loop (cdr ps) out))
                                      (else (loop (cdr ps) (cons (cons (car (car ps)) (cadr (car ps))) out))))))
                            (present? (lambda (p) (member (cons 'about (cdr p))
                                                          (or (field (state-read state (car p)) 'edges) '()))))
                            (intents (map (lambda (p) (list 'link (car p) 'about (cdr p)))
                                          (filter (lambda (p) (not (present? p))) pairs))))
                       (set! already (- (length pairs) (length intents)))
                       (if refused '() (begin (set! linked (length intents)) intents))))
                   to #f #f)))
           (cond
             (refused refused)
             ((not (all-ok second)) (if (and (= 1 (length second)) (judgement-refusal? (car second))) (car second) (cons 'batch (list second))))
             (else
              (append (list 'ok (list 'inserted inserted) (list 'linked linked) (list 'already already))
                      (reverse unresolved)
                      (apply append
                             (map (lambda (b)
                                    (map (lambda (f) (list 'foreign-field (cadr b) (car f) (caddr f)))
                                         (filter (lambda (f) (not (equal? (caddr f) to))) (block-part b 'fields))))
                                  blocks))))))))))

  ;; ---- collect: review-results, then collect-into --------------------------
  ;;
  ;; Each step is its store's own request, on its own route; collect writes
  ;; nothing in the scoped store. The lists review-results makes (no-about,
  ;; missing) are passed on beside collect-into's answer.
  (define (collect-verb store actor args cwd . socket-given)
    (if (not (= 1 (length args)))
        'usage
        (let* ((dir (absolute (car args) cwd))
               (results (routed dir 'review-results '() actor))
               (d (and (ok? results)
                       (find (lambda (x) (and (pair? x) (eq? (car x) 'results))) (cdr results)))))
          (cond
            ((not (ok? results)) results)
            ((not (and d (results-datum? d))) '(error transport-unknown (reason unreadable-answer)))
            (else
              (let* ((into (routed store 'collect-into (list (results-part d 'letter) "--results" (written d)) actor
                                   (and (pair? socket-given) (car socket-given)))))
                (if (ok? into)
                    (append into (filter (lambda (x) (and (pair? x) (memq (car x) '(no-about missing))))
                                         (cddr results)))
                    into)))))))
)
