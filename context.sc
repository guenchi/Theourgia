#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; (theourgia context) -- what to read before working on a block, within a
;;; budget, with the receipt a commit gives back.
;;;
;;; THE HARD MATERIAL IS THE ANSWER OF FIVE QUERIES and the records of the
;;; blocks the first one names, and nothing else. The five goals run in one
;;; session of the query library; `for`, the three hard sections and the
;;; notes are built from their rows and those records; the same five goals,
;;; with their digests, are the query premises of the receipt, beside a
;;; block premise for every hard block, shown or not. So whatever changes
;;; the hard material changes a row of one of the five or the record of a
;;; hard block, and the receipt is refused.
;;;
;;; EACH HARD BLOCK IS IN EXACTLY ONE PLACE, read from the top: the block
;;; asked for is `for`; one not in force (superseded or refuted) is
;;; `to-verify`; one in force and authoritative is `constraints`; any other
;;; is `evidence`.
;;;
;;; THE PREFERRED MATERIAL fills what room is left, step by step, in
;;; `counterexamples` (the blocks a constraint replaced) and `background`
;;; (everything else), never in a hard section: the definitions a hard code
;;; block uses (one step), the evidence of each constraint, the blocks a
;;; constraint replaced, the blocks that rest on a hard block, the blocks
;;; relevant by score, the ancestors of hard blocks. None of it is in the
;;; receipt, and none of it decides anything about the hard material: the
;;; hard blocks are entered and upgraded before the first preferred
;;; candidate is read.
;;;
;;; THE USERS OF THE UNIT'S NAMES are a soft clause of their own, after
;;; `background`: (users (listed <k>) (of <n>) [(skipped ...)] <user> ...),
;;; a user (user <id> (name <n>) (library <lib>|none) (mode datum|text)).
;;; For each name a block of T's unit defines, the live code blocks outside
;;; the unit that use it by the name-use table `uses` reads, ordered by
;;; name then id. The names are the def rows of the soft goal
;;; (and (hard T ?u unit _) (def ?u ?lib ?name)), and the unit set is that
;;; goal's (hard T ?u unit _), not the hard rows, which are joined with
;;; validity; the unit holds T itself (query.sc, (unit ?t ?t)), so T's own
;;; definitions are covered. Neither goal is in the receipt, and the notes
;;; stay the five queries'. The table is the view's own, the working view's
;;; with --working, syntactic as `uses` says, its skipped counts those of
;;; the whole view as `uses` counts them. A user is listed whatever else the
;;; answer shows of it, and is not in the versions: the clause names blocks
;;; it does not show. Users are admitted after the dependents and before
;;; the relevant blocks, so never ahead of a hard entry; the clause is
;;; always there, and `listed` against `of` says when the budget cut it or
;;; that there are none. It is reserved at the counters' digits whatever its
;;; counts are (listed, of and both skipped counts at 999999, and capped), so
;;; which hard blocks are shown, and at which level, never depends on a use
;;; or an unreadable block elsewhere; a count above the cap is shown as the
;;; excluded counters show theirs.
;;;
;;; THE BUDGET is tokens of four bytes over the answer as the wire renders
;;; it, the cut clause left out (it grows with any write anywhere). Nine
;;; tenths of it are usable, and an ok answer is never larger. The envelope
;;; is reserved at its largest before anything is admitted, from the request
;;; and the hard set alone: the whole receipt, the notes, the working clause
;;; with --working, the budget clause at the budget's own digits, the
;;; insufficient clause at twenty of the longest hard entries, the excluded
;;; counters at six digits, the users clause and its skipped counts reserved
;;; at the counters' digits, and with
;;; --names the asked clause at its largest. An entry is admitted at a level
;;; only when the answer with it fits. Greedy, one pass: stated, not
;;; optimal. `--budget min+<t>` asks for the minimum the request would
;;; report in budget-too-small plus t tokens, and the budget clause shows
;;; the number it resolved to.
;;;
;;; AMBIGUITY IS FOLDED: when a name a hard code block uses has more than two
;;; definitions left after the hard blocks and those already entered, the
;;; first two are entered and the fold is said in `folded`; the others stay
;;; free for a later step. A name the caller gives with --names is entered
;;; whole, ahead of everything else preferred, and never folded; `asked`
;;; says which given names are defined only in hard blocks and which have no
;;; definition at all.
;;;
;;; WITH --working THE VIEW IS THE REQUEST WRITER'S WORKING VIEW, the one
;;; `read --working` and `export-code --working` read: the committed state
;;; at the drafts' cut with the writer's drafts laid over it. Everything
;;; above is computed over that view, in a fresh query session, and the
;;; answer says so in `(working <writer>)`, the clause after `for`, which
;;; the budget counts as it counts every clause of the answer. A refusal
;;; of the view (no writer, a writer that is not one, a cut that cannot be
;;; opened, a draft that does not read) is the answer.
;;;
;;; It writes no record and runs no user code. Between requests it keeps
;;; nothing of its own; with --working, store.sc keeps the last working view
;;; asked about in each of two slots (the definitions index and the tables),
;;; so a process holds at most two views until another view asks. Without
;;; --working it reads no log and writes nothing; with it, the view is
;;; folded from the log at the drafts' cut, as the working view always is,
;;; under the writer's draft lock when the writer has a draft area; for a
;;; writer that has none it creates nothing.
(library (theourgia context)
  (export context-verb
          ;; users-clause and reserved-users are exported for the fixture's
          ;; rows on the counter cap (test/context-users.sc), not as an
          ;; interface.
          users-clause reserved-users)
  (import (rnrs) (rnrs mutable-pairs)
          (only (theourgia arguments) argument-option argument-option-list)
          (only (theourgia reduce) state-read block-hash reduce-applied-cut state-field-contested? relation-kind)
          (only (theourgia project) subtree-ids)
          (only (theourgia store) search-state distinct-query-text sealed-state? sealed-state-notes library-locator)
          (only (theourgia name-use) name-use-table live-kind)
          (only (theourgia markers) src-text)
          (only (theourgia log) unreadable-behind merge-unreadable incomplete-clause)
          (only (theourgia render) render-wire printed-entry)
          (only (theourgia query) make-query-session session-answer query-budget-default fresh-query-session session-note!)
          (prefix (theourgia lifecycle) lc:)
          (only (theourgia extensions) context-usage))

  ;; ---- the five queries --------------------------------------------------------------

  ;; THE HARD GOALS, with the block asked for written into each. Their order is
  ;; the receipt's.
  (define (hard-goals t)
    (list `(and (hard ,t ?h ?role ?about) (validity ?h ?v))
          `(and (hard ,t ?h _ _) (validity-reason ?h ?why ?by))
          `(and (unsettled-for ,t ?d) (decision-state ?d ?s))
          `(and (unsettled-for ,t ?d) (decision-state ?d review) (moved-kind ?i implements ?d ?end))
          `(and (scope-of ,t ?a) (contradicts ?a ?b))))

  ;; A refusal from the query library is the verb's answer, raised to the top.
  (define-record-type (refusal make-refusal refusal?) (fields answer))

  ;; -> (rows . digest), or raises the refusal.
  (define (ask S goal)
    (let ((a (session-answer S goal)))
      (if (eq? (car a) 'ok)
          (cons (cadr a) (cadddr a))
          (raise (make-refusal a)))))
  (define (rows-of S goal) (car (ask S goal)))

  ;; ---- small tools ----------------------------------------------------------------------

  ;; THE BYTES THE WIRE WRITES FOR A DATUM, by the printer the daemon and the
  ;; command line write every answer with, its newline left out.
  (define (wire-length x)
    (- (bytevector-length (string->utf8 (render-wire x))) 1))
  ;; AN ENTRY'S BYTES AS THE ANSWER PRINTS IT. The printer gives a text-mode
  ;; code block's src and doc as text where the block holds bytes (render.sc),
  ;; and an entry measured alone is not an answer the printer converts: charged
  ;; as its value, a block entered whole cost three to four times what the
  ;; reader is given, and the budget kept it at summary or identity.
  (define (entry-length e) (wire-length (printed-entry e)))

  (define (unique xs)
    (let ((seen (make-hashtable equal-hash equal?)))
      (let loop ((l xs) (out '()))
        (cond ((null? l) (reverse out))
              ((hashtable-ref seen (car l) #f) (loop (cdr l) out))
              (else (hashtable-set! seen (car l) #t) (loop (cdr l) (cons (car l) out)))))))

  (define (index-of x xs)
    (let loop ((l xs) (i 0))
      (cond ((null? l) #f) ((equal? (car l) x) i) (else (loop (cdr l) (+ i 1))))))

  (define (field-value row name)
    (let ((e (and row (assq name (cdr (assq 'fields row)))))) (and e (cdr e))))

  ;; A field as text to search with: a string, or a bytevector that is text
  ;; (a committed src that began with a byte-order mark), the mark left out
  ;; as search leaves it out; anything else is "".
  (define (text-field row name)
    (let ((v (field-value row name)))
      (cond ((string? v) v)
            ((and (bytevector? v) (src-text v))
             => (lambda (t) (if (and (> (string-length t) 0) (char=? (string-ref t 0) (integer->char #xFEFF)))
                                (substring t 1 (string-length t))
                                t)))
            (else ""))))

  ;; ---- entries --------------------------------------------------------------------------

  ;; A block's version, as a read hands it back; #f when it cannot be hashed.
  (define (version-of state id)
    (guard (e (#t #f)) (block-hash state id)))

  ;; THE SUMMARY: the first 240 bytes of the block's text, cut back to the end
  ;; of a line when one ends inside them, and never inside a character. A
  ;; text-mode code block keeps its text as bytes, and they are cut alike.
  (define summary-bytes 240)
  (define (summary-of row)
    (let* ((src (field-value row 'src))
           (bv (cond ((bytevector? src) src) ((string? src) (string->utf8 src)) (else (make-bytevector 0))))
           (n (bytevector-length bv)))
      (if (<= n summary-bytes)
          (utf8->string bv)
          (let* ((cut (let back ((i summary-bytes))
                        (if (and (> i 0) (= (bitwise-and (bytevector-u8-ref bv i) #xC0) #x80)) (back (- i 1)) i)))
                 (line (let find ((i (- cut 1)))
                         (cond ((< i 0) #f)
                               ((= (bytevector-u8-ref bv i) 10) i)
                               (else (find (- i 1))))))
                 (end (or line cut))
                 (out (make-bytevector end)))
            (bytevector-copy! bv 0 out 0 end)
            (utf8->string out)))))

  ;; The names a code block carries, as stored.
  (define (names-of row)
    (let ((n (field-value row 'name)))
      (cond ((not (eq? (field-value row 'kind) 'code)) '())
            ((or (symbol? n) (string? n)) (list (list 'names n)))
            ((and (list? n) (pair? n) (for-all (lambda (x) (or (symbol? x) (string? x))) n)) (list (cons 'names n)))
            (else '()))))

  ;; THE RECORD A PLAIN READ ANSWERS, asked of the read handler itself and kept
  ;; for the request: one place reads a block for a reader.
  (define (record-reader dispatch-helper store view)
    (let ((kept (make-hashtable string-hash string=?)))
      (lambda (id)
        (or (hashtable-ref kept id #f)
            (let* ((a ((dispatch-helper 'read) store "context" (list id) #f '() view #f #f))
                   (r (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a))) (cadr a) a)))
              (hashtable-set! kept id r)
              r)))))

  ;; ONE ENTRY AT A LEVEL. The identity clauses, the whys, the validity when it
  ;; is not valid, the names for code, then the level's own clause.
  (define (entry state records id version whys validity level)
    (let* ((row (state-read state id))
           (kind (let ((k (field-value row 'kind))) (if (symbol? k) k 'none)))
           (head (list id (list 'kind kind) (list 'title (text-field row 'title))
                       (list 'version version) (list 'level level))))
      (append head whys (if validity (list validity) '()) (names-of row)
              (case level
                ((summary) (list (list 'summary (summary-of row))))
                ((full) (list (list 'block (records id))))
                (else '())))))

  ;; ---- the verb -------------------------------------------------------------------------

  ;; THE BUDGET AS ASKED: tokens, or (min . <t>) for `min+<t>`, or #f.
  (define (budget-request dispatch-helper text)
    (and (string? text)
         (if (and (> (string-length text) 4) (string=? (substring text 0 4) "min+"))
             (let ((k ((dispatch-helper 'count-argument) (substring text 4 (string-length text)))))
               (and k (cons 'min k)))
             ((dispatch-helper 'count-argument) text))))

  (define (context-verb dispatch-helper store actor args req options state writer cwd)
    (let* ((t (argument-option options "--for"))
           (budget-text (argument-option options "--budget"))
           (budget (budget-request dispatch-helper budget-text))
           ;; THE NAMES THE EDIT WILL USE, each once, in the order given
           (names (unique (argument-option-list options "--names")))
           (all? (argument-option options "--all-validity"))
           (working? (argument-option options "--working")))
      (if (or (pair? args) (not (string? t)) (not budget) (exists (lambda (n) (= 0 (string-length n))) names))
          ((dispatch-helper 'usage) context-usage)
          ((dispatch-helper 'guarded)
           (lambda ()
             ;; THE VIEW: the committed one, or with --working the request
             ;; writer's working view, taken through the dispatcher as
             ;; export-code --working takes it; its refusal is the answer.
             (let ((w (and working? ((dispatch-helper 'working-state) store state writer))))
               (if (and w (not (eq? (car w) 'ok)))
                   w
                   (let* ((view (if w (caddr w) ((dispatch-helper 'reduction-for) store state)))
                          (marking (if w (list (list 'working (cadr w))) '()))
                          (row (state-read view t)))
                     ;; AN UNKNOWN OR DELETED BLOCK IS ANSWERED AS READ ANSWERS IT: one
                     ;; spelling per cause, so the read handler itself answers, over
                     ;; the same view: a block the working view does not hold is
                     ;; unknown there even when the committed state has it now. A
                     ;; working answer is one cut, the drafts' cut and the drafts;
                     ;; answered from the present it would mix two in one receipt.
                     (if (or (not row) (cdr (assq 'deleted row)))
                         ((dispatch-helper 'read) store actor (list t) req '() (if w view state) writer cwd)
                         (guard (e ((refusal? e) (refusal-answer e)))
                           (assemble dispatch-helper store view t budget names all? marking
                                     (appended-clause state view))))))))))))

  ;; WHAT THE DISPATCHER WILL APPEND after this answer, or #f: the clause that
  ;; says which writers the state could not read. It is part of the bytes a
  ;; caller receives, so the budget counts it.
  (define (appended-clause state view)
    (incomplete-clause (merge-unreadable (if (sealed-state? state) (sealed-state-notes state) '())
                                         (unreadable-behind view))))

  ;; ---- the hard material ------------------------------------------------------------------

  (define role-order '(unit member unsettled supersedes cause implementer stale))

  (define (assemble dispatch-helper store view t budget names all? marking appended)
    ;; THE SESSION IS THE DISPATCHER'S when it has one (the daemon's, kept
    ;; across requests), else a fresh one, as the query verb takes it
    (let* ((S (let ((p (dispatch-helper 'session)))
                (if p (p store view) (fresh-query-session view ((dispatch-helper 'keyword-hook) store)))))
           (goals (hard-goals t))
           (answers (map (lambda (g) (ask S g)) goals))
           (k1 (car (list-ref answers 0)))
           (k2 (car (list-ref answers 1)))
           (k3 (car (list-ref answers 2)))
           (k4 (car (list-ref answers 3)))
           (k5 (car (list-ref answers 4)))
           (roles (make-hashtable string-hash string=?))
           (validity (make-hashtable string-hash string=?))
           (reasons (make-hashtable string-hash string=?)))
      (for-each (lambda (r)
                  (let ((h (car r)))
                    (hashtable-set! roles h (append (hashtable-ref roles h '()) (list (list (cadr r) (caddr r)))))
                    (hashtable-set! validity h (cadddr r))))
                k1)
      (for-each (lambda (r) (hashtable-set! reasons (car r) (append (hashtable-ref reasons (car r) '()) (list (cdr r)))))
                k2)
      (let* ((hard (unique (map car k1)))
             (has-role? (lambda (h role) (exists (lambda (r) (eq? (car r) role)) (hashtable-ref roles h '()))))
             (order (hard-order view t hard has-role? roles))
             (whys (lambda (h) (hard-whys h (hashtable-ref roles h '()) k3 k4)))
             (validity-clause
               (lambda (h)
                 (let ((v (hashtable-ref validity h 'valid)))
                   (and (not (eq? v 'valid)) (cons* 'validity v (hashtable-ref reasons h '()))))))
             (place (lambda (h)
                      (let ((v (hashtable-ref validity h 'valid)))
                        (cond ((equal? h t) 'for)
                              ((memq v '(superseded refuted)) 'to-verify)
                              ((authoritative? view h) 'constraints)
                              (else 'evidence)))))
             ;; A STALE VERIFICATION IS SAID WHERE A READER LOOKS: beside the
             ;; nogoods, (stale-verification <verifier> verifies <target>), one for
             ;; each hard row of role stale. Its place stays evidence: the target
             ;; is in force, and to-verify is for what is not.
             (notes (append
                      (unique (map (lambda (r)
                                     (let ((a (car r)) (b (cadr r)))
                                       (if (string<? a b) (list 'nogood a b 'conflicts-with) (list 'nogood b a 'conflicts-with))))
                                   k5))
                      (unique (map (lambda (r) (list 'stale-verification (caddr r) 'verifies (car r)))
                                   (filter (lambda (r) (eq? (cadr r) 'stale)) k1)))))
             (versions (map (lambda (h) (cons h (version-of view h))) order))
             (unhashable (map car (filter (lambda (p) (not (cdr p))) versions)))
             (receipt (cons 'receipt
                            (append (map (lambda (p) (list 'premise (car p) (cdr p)))
                                         (filter cdr versions))
                                    (map (lambda (g a) (list 'premise (list 'query g) (cdr a))) goals answers)))))
        ;; A HARD BLOCK THAT CANNOT BE HASHED HAS NO PREMISE TO GIVE, and the
        ;; receipt covers every hard block: no answer, by name. A value nested
        ;; past what the encoder writes is stored and read while its hash
        ;; cannot be taken, so the state exists.
        (if (pair? unhashable)
            (list 'error 'unhashable-hard-block (cons 'id unhashable))
            (budgeted dispatch-helper store view (record-reader dispatch-helper store view) S t budget names all? order versions whys validity-clause place notes receipt
                      (lambda (h) (hashtable-ref validity h 'valid)) marking appended (unit-users S view t))))))

  ;; ---- the users of the unit's names ----------------------------------------------------------

  ;; THE USERS OF WHAT T'S UNIT DEFINES, from the name-use table `uses` reads,
  ;; over the request's own view. The names are the def rows of the unit's
  ;; blocks and the unit is the hard rule's (T and what is inside it), both
  ;; asked of the session as soft goals, outside the receipt. A block using
  ;; two of the names is listed under each. The table is built only when the
  ;; unit defines a name. -> #(<entries> <skipped clause or #f>), the
  ;; skipped clause counted over the whole view, as `uses` counts it.
  (define (unit-users S view t)
    (let ((names (list-sort string<? (unique (map caddr (rows-of S `(and (hard ,t ?u unit _) (def ?u ?lib ?name))))))))
      (if (null? names)
          (vector '() #f)
          (let* ((unit (map car (rows-of S `(hard ,t ?u unit _))))
                 (table (name-use-table view))
                 (library-of (library-locator view (live-kind view)))
                 (reasons (let-values (((ks vs) (hashtable-entries (vector-ref table 2)))) (vector->list vs)))
                 (count (lambda (r) (length (filter (lambda (x) (eq? x r)) reasons))))
                 (counts (filter (lambda (c) (> (cadr c) 0))
                                 (list (list 'no-language (count 'no-language))
                                       (list 'unreadable-code (count 'unreadable-code))))))
            (vector
              (apply append
                     (map (lambda (n)
                            (map (lambda (id)
                                   (list 'user id (list 'name n) (list 'library (or (library-of id) 'none))
                                         (list 'mode (hashtable-ref (vector-ref table 1) id 'text))))
                                 (list-sort string<? (filter (lambda (id) (not (member id unit)))
                                                             (unique (hashtable-ref (vector-ref table 0) n '()))))))
                          names))
              (and (pair? counts) (cons 'skipped counts)))))))

  ;; THE USERS CLAUSE: how many users it lists of how many there are, the
  ;; table's skipped counts when it has some, then the users listed.
  ;; A count above the cap is shown at it, and the clause then says (capped),
  ;; as the excluded clause does.
  (define (users-clause listed n skipped)
    (let* ((k (length listed))
           (counts (if skipped (map cadr (cdr skipped)) '()))
           (capped? (exists (lambda (x) (> x counter-cap)) (cons* k n counts))))
      (append (list 'users (list 'listed (min counter-cap k)) (list 'of (min counter-cap n)))
              (if skipped
                  (list (cons 'skipped (map (lambda (c) (list (car c) (min counter-cap (cadr c)))) (cdr skipped))))
                  '())
              (if capped? '((capped)) '())
              listed)))

  ;; AUTHORITY IS THE RECORD'S: its effective class, an absent one by kind.
  (define (authoritative? view h)
    (let* ((row (state-read view h))
           (c (field-value row 'class)))
      (lc:authoritative? (lc:effective-class row (state-field-contested? view h 'class c)))))

  ;; HARD ORDER: the block asked for; the rest of its unit in document order;
  ;; the other members by distance from the unit over depends edges, ties by
  ;; id; then the blocks that are not members, by their first role in the
  ;; order unsettled, supersedes, cause, implementer, by id within a role.
  (define (hard-order view t hard has-role? roles)
    (let* ((unit (filter (lambda (h) (has-role? h 'unit)) hard))
           (unit-order (let ((walk (or (subtree-ids view t) (list t))))
                         (append (filter (lambda (id) (member id unit)) walk)
                                 (list-sort string<? (filter (lambda (id) (not (member id walk))) unit)))))
           (members (filter (lambda (h) (and (has-role? h 'member) (not (member h unit)))) hard))
           (distance (member-distances view unit-order members))
           (member-order (list-sort (lambda (a b)
                                      (let ((da (hashtable-ref distance a #f)) (db (hashtable-ref distance b #f)))
                                        (cond ((and da db (not (= da db))) (< da db))
                                              ((and da (not db)) #t)
                                              ((and db (not da)) #f)
                                              (else (string<? a b)))))
                                    members))
           (others (filter (lambda (h) (not (or (member h unit) (member h members)))) hard))
           (first-role (lambda (h)
                         (apply min (map (lambda (r) (or (index-of (car r) role-order) 99))
                                         (hashtable-ref roles h '())))))
           (other-order (list-sort (lambda (a b)
                                     (let ((ra (first-role a)) (rb (first-role b)))
                                       (if (= ra rb) (string<? a b) (< ra rb))))
                                   others)))
      (cons t (append (filter (lambda (id) (not (equal? id t))) unit-order) member-order other-order))))

  ;; Breadth first from the unit over each record's edges of kind depends-on
  ;; and implements, under any name, to members only.
  (define (member-distances view unit members)
    (let ((d (make-hashtable string-hash string=?)))
      (let loop ((frontier unit) (k 1))
        (unless (null? frontier)
          (let ((next (unique
                        (apply append
                               (map (lambda (id)
                                      (let ((row (state-read view id)))
                                        (filter (lambda (to) (and (member to members) (not (hashtable-ref d to #f))))
                                                (map cdr (filter (lambda (e) (memq (relation-kind view (car e)) '(depends-on implements)))
                                                                 (cdr (assq 'edges row)))))))
                                    frontier)))))
            (for-each (lambda (id) (hashtable-set! d id k)) next)
            (loop (list-sort string<? next) (+ k 1)))))
      d))

  ;; EVERY WHY A HARD BLOCK HAS, from its rows: member is not said of a block
  ;; that is in the unit, which already says more.
  (define (hard-whys h roles k3 k4)
    (let ((unit? (exists (lambda (r) (eq? (car r) 'unit)) roles)))
      (unique
        (apply append
               (map (lambda (role)
                      (apply append
                             (map (lambda (r)
                                    (if (not (eq? (car r) role))
                                        '()
                                        (case role
                                          ((unit) '((why unit)))
                                          ((member) (if unit? '() '((why member))))
                                          ((unsettled) (map (lambda (s) (list 'why 'unsettled (cadr s)))
                                                            (filter (lambda (s) (equal? (car s) h)) k3)))
                                          ((supersedes) (list (list 'why 'supersedes (cadr r))))
                                          ((cause) (list (list 'why 'cause (cadr r))))
                                          ((stale) (list (list 'why 'stale-verification (cadr r))))
                                          ((implementer)
                                           (map (lambda (m) (list 'why 'implementer (car m) (caddr m)))
                                                (filter (lambda (m) (and (equal? (cadr m) h) (equal? (car m) (cadr r)))) k4)))
                                          (else '()))))
                                  (list-sort (lambda (a b) (string<? (format-about (cadr a)) (format-about (cadr b)))) roles))))
                    role-order)))))
  (define (format-about x) (if (string? x) x (call-with-string-output-port (lambda (p) (write x p)))))

  ;; ---- the budget ----------------------------------------------------------------------------

  (define section-names '(constraints evidence to-verify counterexamples background))
  (define excluded-names '(definitions evidence counterexamples dependents users relevant ancestors))
  (define counter-cap 999999)
  ;; THE USERS CLAUSE AS RESERVED, whatever its counts: every count at the
  ;; cap, both skipped counts, and capped.
  (define reserved-users
    (list 'users (list 'listed counter-cap) (list 'of counter-cap)
          (list 'skipped (list 'no-language counter-cap) (list 'unreadable-code counter-cap)) '(capped)))

  ;; An ok answer without its cut fits when ten times its bytes are at most
  ;; thirty-six times the budget: four bytes a token, nine tenths usable.
  (define (fits? bytes budget) (<= (* 10 bytes) (* 36 budget)))

  ;; The answer's bytes as the wire writes it, the cut left out: its datum and
  ;; the newline after it.
  (define (answer-bytes form) (+ 1 (wire-length form)))

  (define (budgeted dispatch-helper store view records S t budget names all? order versions whys validity-clause place notes receipt validity-of marking appended users)
    (let* ((version (lambda (h) (let ((p (assoc h versions))) (and p (cdr p)))))
           (identity (lambda (h) (entry view records h (version h) (whys h) (validity-clause h) 'identity)))
           (pair-of (lambda (h) (cons h (version h))))
           (cost (lambda (h) (+ (entry-length (identity h)) (wire-length (pair-of h)))))
           (others (cdr order))
           (n-others (length others))
           (costs (map cost others))
           (longest-id (fold-left (lambda (best h) (if (> (string-length h) (string-length best)) h best)) "" others))
           (largest (fold-left max 0 costs))
           (named (min 20 n-others))
           (reserved-insufficient
             (if (= n-others 0)
                 '()
                 (list (cons* 'insufficient
                              (list 'missing (let copies ((k named) (out '())) (if (= k 0) out (copies (- k 1) (cons (list longest-id largest) out)))))
                              (if (> n-others 20)
                                  (list (list 'more (- n-others 20) (list 'bytes (fold-left + 0 costs))))
                                  '())))))
           (reserved-excluded (append (list 'excluded) (map (lambda (n) (list n counter-cap)) excluded-names) '((capped))))
           ;; THE ASKED CLAUSE AT AN UPPER BOUND, from the request and the hard
           ;; set alone: every given name as having no definition, and as
           ;; defined in every hard block. What the clause does not use goes
           ;; back to the preferred material once the hard blocks are in.
           (reserved-asked
             (if (null? names)
                 '()
                 (list (append (list 'asked (cons 'names names))
                               (apply append (map (lambda (n) (map (lambda (h) (list 'in-hard n h)) order)) names))
                               (list (cons 'no-definition names))))))
           (base (lambda (tokens)
                   (append (list 'ok (list 'for (identity t)))
                           ;; the working clause, reserved as it is rendered
                           marking
                           (map (lambda (n) (list n '())) section-names)
                           ;; the users clause with none listed, at the counters' digits
                           (list reserved-users)
                           (list (list 'notes notes))
                           reserved-insufficient
                           (list reserved-excluded)
                           reserved-asked
                           (list (list 'budget (list 'tokens tokens) (list 'used tokens) (list 'reserve tokens))
                                 receipt
                                 (list 'versions (list (pair-of t)))
                                 '(name-use syntactic)))))
           (tail (if appended (+ 1 (wire-length appended)) 0))
           ;; `min+<t>`: the minimum this request reports, plus t
           (budget (if (pair? budget) (+ (minimum-budget base tail) (cdr budget)) budget))
           (base-size (+ tail (answer-bytes (base budget)))))
      (if (not (fits? base-size budget))
          (list 'error 'budget-too-small (list 'minimum (minimum-budget base tail)))
          (admit dispatch-helper store view records S t budget names all? order whys validity-clause place notes receipt validity-of
                 version identity pair-of cost base-size tail reserved-asked marking users))))

  ;; THE SMALLEST BUDGET THE RESERVATION AND THE BLOCK ASKED FOR FIT IN. The
  ;; reservation holds the budget at its own digits, so it is found digit
  ;; count by digit count.
  (define (minimum-budget base tail)
    (let loop ((digits 1))
      (let* ((low (if (= digits 1) 0 (expt 10 (- digits 1))))
             (size (+ tail (answer-bytes (base low))))
             (need (max low (div (+ (* 10 size) 35) 36))))
        (if (< need (expt 10 digits)) need (loop (+ digits 1))))))

  (define (admit dispatch-helper store view records S t budget names all? order whys validity-clause place notes receipt validity-of
                 version identity pair-of cost base-size tail reserved-asked marking users)
    (let ((size base-size)
          (levels (make-hashtable string-hash string=?))
          (counts (map (lambda (n) (cons n 0)) section-names))
          (sections (map (lambda (n) (cons n '())) section-names))
          (missing '())
          (entered '()))
      (define (count-of s) (cdr (assq s counts)))
      (define (enter! s id e)
        (set-cdr! (assq s counts) (+ 1 (count-of s)))
        (set-cdr! (assq s sections) (append (cdr (assq s sections)) (list (cons id e)))))
      (define (delta s e pair) (+ (entry-length e) (if (> (count-of s) 0) 1 0) (wire-length pair) 1))
      ;; B5: the hard blocks at identity, in hard order.
      (hashtable-set! levels t 'identity)
      (for-each (lambda (h)
                  (let* ((s (place h)) (e (identity h)) (d (delta s e (pair-of h))))
                    (if (fits? (+ size d) budget)
                        (begin (set! size (+ size d)) (hashtable-set! levels h 'identity)
                               (enter! s h #f) (set! entered (cons h entered)))
                        (set! missing (cons h missing)))))
                (cdr order))
      (set! missing (reverse missing))
      ;; B6: one pass of upgrades in hard order.
      (for-each (lambda (h)
                  (when (hashtable-ref levels h #f)
                    (let ((at-identity (entry-length (identity h))))
                      (let try ((ls '(full summary)))
                        (unless (null? ls)
                          (let ((d (- (entry-length (entry view records h (version h) (whys h) (validity-clause h) (car ls)))
                                      at-identity)))
                            (if (fits? (+ size d) budget)
                                (begin (set! size (+ size d)) (hashtable-set! levels h (car ls)))
                                (try (cdr ls)))))))))
                order)
      ;; B7: the preferred material in what room is left.
      (let* ((hard-entry (lambda (h) (entry view records h (version h) (whys h) (validity-clause h) (hashtable-ref levels h 'identity))))
             (L (lc:lifecycle view))
             (preferred (preferred-steps dispatch-helper store view S t names all? order place L))
             ;; THE USERS ARE A STEP OF THEIR OWN, after the dependents and
             ;; before the relevant blocks; each candidate is its entry.
             (steps (let split ((l (vector-ref preferred 0)))
                      (cond ((null? l) '())
                            ((eq? (list-ref (car l) 4) 'dependents)
                             (cons* (car l) (list 'users 'users '() (vector-ref users 0) 'users) (cdr l)))
                            (else (cons (car l) (split (cdr l)))))))
             (listed '())
             (groups (vector-ref preferred 2))
             (asked (vector-ref preferred 3))
             ;; each folded group's shown count, kept by the ids it entered
             (group-of (make-hashtable string-hash string=?))
             (fold-refused #f)
             (excluded (map (lambda (n) (cons n 0)) excluded-names))
             (exclude! (lambda (counter) (set-cdr! (assq counter excluded) (+ 1 (cdr (assq counter excluded))))))
             (tried '())
             (chosen '()))
        (for-each (lambda (g) (for-each (lambda (id) (hashtable-set! group-of id g)) (vector-ref g 4))) groups)
        ;; THE ASKED RESERVE GIVEN BACK: the clause as it is, against its
        ;; upper bound.
        (when asked
          (set! size (+ size (- (wire-length asked) (wire-length (car reserved-asked))))))
        (for-each
          (lambda (step)
            (let ((counter (car step)) (s (cadr step)) (allowed (caddr step)) (n 0))
              ;; THE FOLDED CLAUSE IS CHARGED BEFORE THE STEP IT REPORTS ON, at
              ;; its full size (a shown count is one digit whatever it is). When
              ;; it does not fit, no fold is applied unsaid: every candidate of
              ;; a folded group is excluded instead.
              (when (and (memq 'fold step) (pair? groups))
                (let ((d (+ 1 (wire-length (folded-clause groups)))))
                  (if (fits? (+ size d) budget)
                      (set! size (+ size d))
                      (set! fold-refused #t))))
              (for-each
                (lambda (c)
                  (set! n (+ n 1))
                  (if (eq? s 'users)
                      ;; A USER IS ENTERED WHOLE OR NOT AT ALL, at its own wire
                      ;; length; it has no level and no version.
                      (let ((d (+ 1 (wire-length c))))
                        (if (fits? (+ size d) budget)
                            (begin (set! size (+ size d)) (set! listed (cons c listed)))
                            (exclude! counter)))
                      (let* ((id (car c)) (cwhys (cdr c))
                             (g (and (memq 'fold step) (hashtable-ref group-of id #f)))
                             (v (lc:validity-of L id))
                             (vclause (and (not (eq? (car v) 'valid)) (cons* 'validity (car v) (lc:validity-reasons v))))
                             (ver (version-of view id))
                             (pair (cons id ver)))
                        (if (and g fold-refused)
                            (exclude! counter)
                            (let try ((ls allowed))
                              (if (null? ls)
                                  (exclude! counter)
                                  (let* ((e (entry view records id ver cwhys vclause (car ls))) (d (delta s e pair)))
                                    (if (fits? (+ size d) budget)
                                        (begin (set! size (+ size d)) (enter! s id e) (set! chosen (cons pair chosen))
                                               (when g (vector-set! g 3 (+ 1 (vector-ref g 3)))))
                                        (try (cdr ls))))))))))
                (cadddr step))
              (set! tried (cons (list (list-ref step 4) (length (cadddr step)) n) tried))))
          steps)
        ;; WHAT THE PREFERRED STEPS PRODUCED AND TRIED, for the session trace
        (session-note! S 'preferred-candidates (reverse tried))
        (finish view t budget order hard-entry place notes receipt missing cost sections excluded
                (reverse chosen) version (vector-ref preferred 1) tail
                (if (or fold-refused (null? groups)) #f (folded-clause groups)) asked marking (reverse listed) users))))

  ;; (folded (<name> <block> (candidates <n>) (shown <m>)) ...), a group a
  ;; vector #(<name> <block> <candidates> <shown> <entered ids>)
  (define (folded-clause groups)
    (cons 'folded (map (lambda (g) (list (vector-ref g 0) (vector-ref g 1)
                                         (list 'candidates (vector-ref g 2)) (list 'shown (vector-ref g 3))))
                       groups)))

  ;; ---- the preferred material ------------------------------------------------------------------

  ;; -> #(<steps> <name use consulted> <folded groups> <asked clause or #f>),
  ;; each step (<counter> <section> <levels allowed> <candidates> <label>
  ;; [fold]), a candidate (<id> <why> ...). A candidate is never a hard block
  ;; or the block asked for, and is in the earliest step that names it.
  (define (preferred-steps dispatch-helper store view S t names all? order place L)
    (let* ((seen (make-hashtable string-hash string=?))
           (rank (lambda (h) (or (index-of h order) 1000000)))
           (in-force? (lambda (id) (memq (car (lc:validity-of L id)) '(valid needs-review))))
           (admitted? (lambda (id) (or all? (in-force? id))))
           (constraints (filter (lambda (h) (eq? (place h) 'constraints)) order))
           (take (lambda (cands keep?)
                   (let loop ((l cands) (out '()))
                     (cond ((null? l) (reverse out))
                           ((or (member (car (car l)) order) (hashtable-ref seen (car (car l)) #f) (not (keep? (car (car l)))))
                            (loop (cdr l) out))
                           (else (hashtable-set! seen (car (car l)) #t) (loop (cdr l) (cons (car l) out)))))))
           (by (lambda (key<? xs) (list-sort key<? xs)))
           ;; 0: THE NAMES THE CALLER GAVE, each defined in T's scope
           ;; (def-for-name). EXISTENCE IS DECIDED BEFORE THE HARD EXCLUSION: a
           ;; name defined in a hard block, T itself included, is `in-hard`, and
           ;; only a name with no definition at all is `no-definition`. Its
           ;; entries are marked seen as any step's, so a given name's
           ;; definitions are never folded below.
           (named (map (lambda (n) (cons n (unique (map car (rows-of S `(def-for-name ,t ,n ?d)))))) names))
           (step0 (take (apply append
                               (map (lambda (p) (map (lambda (d) (list d (list 'why 'names (car p))))
                                                     (list-sort string<? (filter (lambda (d) (not (member d order))) (cdr p)))))
                                    named))
                        admitted?))
           (asked (and (pair? names)
                       (append (list 'asked (cons 'names names))
                               (apply append
                                      (map (lambda (p) (map (lambda (d) (list 'in-hard (car p) d))
                                                            (filter (lambda (d) (member d (cdr p))) order)))
                                           named))
                               (let ((none (map car (filter (lambda (p) (null? (cdr p))) named))))
                                 (if (null? none) '() (list (cons 'no-definition none)))))))
           ;; 1: the definitions of the names each hard code block in force uses.
           (code (filter (lambda (h) (and (eq? (field-value (state-read view h) 'kind) 'code) (in-force? h))) order))
           (uses (if (null? code) '()
                     (filter (lambda (r) (member (car r) code)) (rows-of S `(and (hard ,t ?b _ _) (uses-name ?b ?n))))))
           (defs (if (null? uses) '()
                     (filter (lambda (r) (member (car r) code)) (rows-of S `(and (hard ,t ?b _ _) (def-for ?b ?name ?d))))))
           ;; THE FOLD, per (using block, name), over what is left after the
           ;; hard blocks, the ids already seen and the validity filter: more
           ;; than two left, and the first two in this order are entered and
           ;; the group recorded; the rest are NOT marked seen, so a later step
           ;; may still enter them for its own reason.
           (fold-cap 2)
           (folded '())
           (step1 (let loop ((rs (by (lambda (a b) (or (< (rank (car a)) (rank (car b)))
                                                       (and (= (rank (car a)) (rank (car b)))
                                                            (or (string<? (format-about (cadr a)) (format-about (cadr b)))
                                                                (and (equal? (cadr a) (cadr b)) (string<? (caddr a) (caddr b)))))))
                                     defs))
                             (out '()))
                    (if (null? rs)
                        (reverse out)
                        (let* ((b (car (car rs))) (name (cadr (car rs)))
                               (group (let g ((l rs) (acc '()))
                                        (if (and (pair? l) (equal? (car (car l)) b) (equal? (cadr (car l)) name))
                                            (g (cdr l) (cons (car l) acc))
                                            (reverse acc))))
                               (left (filter (lambda (r) (let ((d (caddr r)))
                                                           (not (or (member d order) (hashtable-ref seen d #f) (not (admitted? d))))))
                                             group))
                               (over? (> (length left) fold-cap))
                               (kept (if over? (list-head left fold-cap) left))
                               (ambiguous (if (> (length group) 1) '((ambiguous)) '())))
                          (for-each (lambda (r) (hashtable-set! seen (caddr r) #t)) kept)
                          (when over?
                            (set! folded (cons (vector name b (length left) 0 (map caddr kept)) folded)))
                          (loop (list-tail rs (length group))
                                (append (reverse (map (lambda (r) (list (caddr r) (append (list 'why 'defines name b) ambiguous)))
                                                      kept))
                                        out))))))
           ;; 2: the evidence of each constraint: what verifies it, what implements it.
           (evidence (lambda (rows why)
                       (map (lambda (r) (list (cadr r) (list 'why why (car r))))
                            (filter (lambda (r) (member (car r) constraints)) rows))))
           (step2 (take (by (lambda (a b) (let ((ca (caddr (cadr a))) (cb (caddr (cadr b))))
                                            (or (< (rank ca) (rank cb)) (and (= (rank ca) (rank cb)) (string<? (car a) (car b))))))
                            (append (evidence (rows-of S `(and (hard ,t ?c _ _) (verified-by ?c ?v))) 'verifies)
                                    (evidence (map (lambda (r) (list (cadr r) (car r)))
                                                   (rows-of S `(and (hard ,t ?c _ _) (edge-kind ?v implements ?c))))
                                              'implements)))
                        admitted?))
           ;; 3: the blocks a constraint replaced, when they are not in force.
           (replaced (lambda (rel why)
                       (map (lambda (r) (list (cadr r) (list 'why why (car r))))
                            (filter (lambda (r) (and (member (car r) constraints)
                                                     (memq (car (lc:validity-of L (cadr r))) '(superseded refuted))))
                                    (rows-of S `(and (hard ,t ?c _ _) (edge-kind ?c ,rel ?o)))))))
           (step3 (take (by (lambda (a b) (let ((ca (caddr (cadr a))) (cb (caddr (cadr b))))
                                            (or (< (rank ca) (rank cb)) (and (= (rank ca) (rank cb)) (string<? (car a) (car b))))))
                            (append (replaced 'supersedes 'superseded-by) (replaced 'refutes 'refuted-by)))
                        (lambda (id) #t)))
           ;; 4: the blocks that rest on a hard block.
           (step4 (take (map (lambda (r) (list (cadr r) (list 'why 'rests-on (car r))))
                             (by (lambda (a b) (or (< (rank (car a)) (rank (car b)))
                                                   (and (= (rank (car a)) (rank (car b))) (string<? (cadr a) (cadr b)))))
                                 (rows-of S `(and (hard ,t ?h _ _) (affects ?h ?b)))))
                        admitted?))
           ;; 5: the blocks relevant to the block's title and text, by score.
           (step5 (take (map (lambda (h) (list (car h) (list 'why 'relevant (cdr h))))
                             (by (lambda (a b) (or (> (cdr a) (cdr b)) (and (= (cdr a) (cdr b)) (string<? (car a) (car b)))))
                                 (relevant dispatch-helper store view t all? L)))
                        admitted?))
           ;; 6: the ancestors of hard blocks, nearest first.
           (step6 (take (apply append
                               (map (lambda (h) (map (lambda (a) (list a (list 'why 'contains h))) (ancestors view h)))
                                    order))
                        admitted?)))
      (vector (list (list 'definitions 'background '(summary identity) step0 'names)
                    (list 'definitions 'background '(summary identity) step1 'definitions 'fold)
                    (list 'evidence 'background '(full summary identity) step2 'evidence)
                    (list 'counterexamples 'counterexamples '(full summary identity) step3 'counterexamples)
                    (list 'dependents 'background '(identity) step4 'dependents)
                    (list 'relevant 'background '(full summary identity) step5 'relevant)
                    (list 'ancestors 'background '(identity) step6 'ancestors))
              (pair? uses)
              (reverse folded)
              asked)))

  (define (list-head l k) (if (or (= k 0) (null? l)) '() (cons (car l) (list-head (cdr l) (- k 1)))))

  ;; THE HITS OF SEARCH for the block's title, and for its text when it has
  ;; one: the best score of each block. Without --all-validity, search's own
  ;; filter leaves out what is superseded or refuted, as `score` does.
  ;; The text is asked by its DISTINCT tokens: a source repeats its words,
  ;; and a repeated token changes no hit and no score, only the work.
  (define (relevant dispatch-helper store view t all? L)
    (let* ((row (state-read view t))
           (texts (filter (lambda (s) (> (string-length s) 0))
                          (list (text-field row 'title) (distinct-query-text (text-field row 'src)))))
           (best (make-hashtable string-hash string=?)))
      (for-each
        (lambda (text)
          (let ((r (search-state view text #f ((dispatch-helper 'keyword-hook) store)
                                 (lambda (st) (lc:search-filter L all?)))))
            (for-each (lambda (h)
                        (let ((id (car h)) (score (cadr h)))
                          (when (and (not (equal? id t)) (> score (hashtable-ref best id -1)))
                            (hashtable-set! best id score))))
                      (cdr (assq 'items r)))))
        texts)
      (let-values (((ks vs) (hashtable-entries best)))
        (map cons (vector->list ks) (vector->list vs)))))

  ;; A block's ancestors from its records, nearest first, the root left out. A
  ;; parent with no record in the state (its writer could not be read) ends
  ;; the walk and is never named.
  (define (ancestors view h)
    (let loop ((id h) (out '()) (k 0))
      (let* ((row (state-read view id))
             (p (and row (cdr (assq 'position row)))))
        (if (and (pair? p) (string? (car p)) (< k 10000) (not (member (car p) out))
                 (state-read view (car p)))
            (loop (car p) (append out (list (car p))) (+ k 1))
            out))))

  ;; ---- the answer ------------------------------------------------------------------------------

  (define (finish view t budget order hard-entry place notes receipt missing cost sections excluded chosen version consulted tail
                  folded asked marking listed users)
    (let* ((section-of
             (lambda (name)
               (append (map (lambda (h) (hard-entry h))
                            (filter (lambda (h) (and (not (equal? h t)) (eq? (place h) name)
                                                     (assoc h (cdr (assq name sections)))
                                                     (not (cdr (assoc h (cdr (assq name sections)))))))
                                    order))
                       (filter values (map cdr (cdr (assq name sections)))))))
           (shown (map (lambda (name) (list name (section-of name))) section-names))
           (ids (append (list t) (apply append (map (lambda (s) (map car (cadr s))) shown))))
           (versions (map (lambda (id) (cons id (or (version id) (let ((p (assoc id chosen))) (and p (cdr p)))))) ids))
           (insufficient
             (if (null? missing)
                 '()
                 (let* ((first (let take ((l missing) (k 0)) (if (or (null? l) (= k 20)) '() (cons (car l) (take (cdr l) (+ k 1))))))
                        (rest (list-tail missing (length first))))
                   (list (cons* 'insufficient
                                (list 'missing (map (lambda (h) (list h (cost h))) first))
                                (if (null? rest) '()
                                    (list (list 'more (length rest) (list 'bytes (fold-left + 0 (map cost rest)))))))))))
           (capped? (exists (lambda (e) (> (cdr e) counter-cap)) excluded))
           (excluded-clause (append (list 'excluded) (map (lambda (e) (list (car e) (min counter-cap (cdr e)))) excluded)
                                    (if capped? '((capped)) '())))
           (form (lambda (used)
                   (append (list 'ok (list 'for (hard-entry t)))
                           ;; `(working <writer>)` with --working, inside the
                           ;; form, so `used` and the size check count it
                           marking
                           shown
                           (list (users-clause listed (length (vector-ref users 0)) (vector-ref users 1)))
                           (list (list 'notes notes))
                           insufficient
                           (list excluded-clause)
                           (if folded (list folded) '())
                           (if asked (list asked) '())
                           (list (list 'budget (list 'tokens budget) (list 'used used) (list 'reserve (div budget 10)))
                                 receipt
                                 (list 'versions versions))
                           (if consulted '((name-use syntactic)) '()))))
           (used (let loop ((u 0))
                   (let ((u2 (div (+ tail (answer-bytes (form u)) 3) 4)))
                     (if (= u2 u) u (loop u2)))))
           (answer (form used)))
      (unless (fits? (+ tail (answer-bytes answer)) budget)
        (assertion-violation 'context "an answer larger than its budget" budget))
      ;; THE CUT GOES BEFORE THE VERSIONS, where every read says it.
      (let loop ((cs answer) (out '()))
        (cond ((null? cs) (reverse out))
              ((and (pair? (car cs)) (eq? (car (car cs)) 'versions))
               (loop (cdr cs) (cons (car cs) (cons (list 'cut (reduce-applied-cut view)) out))))
              (else (loop (cdr cs) (cons (car cs) out)))))))
)
