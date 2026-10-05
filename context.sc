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
;;; THE BUDGET is tokens of four bytes over the answer as the wire renders
;;; it, the cut clause left out (it grows with any write anywhere). Nine
;;; tenths of it are usable, and an ok answer is never larger. The envelope
;;; is reserved at its largest before anything is admitted, from the request
;;; and the hard set alone: the whole receipt, the notes, the budget clause
;;; at the budget's own digits, the insufficient clause at twenty of the
;;; longest hard entries, the excluded counters at six digits. An entry is
;;; admitted at a level only when the answer with it fits. Greedy, one
;;; pass: stated, not optimal.
;;;
;;; It writes nothing, reads no log, runs no user code and keeps nothing
;;; between requests.
(library (theourgia context)
  (export context-verb)
  (import (rnrs) (rnrs mutable-pairs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read block-hash reduce-applied-cut state-field-contested?)
          (only (theourgia project) subtree-ids)
          (only (theourgia store) search-state sealed-state? sealed-state-notes)
          (only (theourgia log) unreadable-behind merge-unreadable incomplete-clause)
          (only (theourgia render) render-wire)
          (only (theourgia query) make-query-session session-answer query-budget-default)
          (prefix (theourgia lifecycle) lc:)
          (only (theourgia extensions) context-usage))

  ;; ---- the five queries --------------------------------------------------------------

  ;; THE HARD GOALS, with the block asked for written into each. Their order is
  ;; the receipt's.
  (define (hard-goals t)
    (list `(and (hard ,t ?h ?role ?about) (validity ?h ?v))
          `(and (hard ,t ?h _ _) (validity-reason ?h ?why ?by))
          `(and (unsettled-for ,t ?d) (decision-state ?d ?s))
          `(and (unsettled-for ,t ?d) (decision-state ?d review) (moved ?i implements ?d ?end))
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

  (define (text-field row name)
    (let ((v (field-value row name))) (if (string? v) v "")))

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
  (define (record-reader store view)
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

  (define (context-verb store actor args req options state writer cwd)
    (let* ((t (argument-option options "--for"))
           (budget-text (argument-option options "--budget"))
           (budget (and (string? budget-text) ((dispatch-helper 'count-argument) budget-text)))
           (all? (argument-option options "--all-validity")))
      (if (or (pair? args) (not (string? t)) (not budget))
          ((dispatch-helper 'usage) context-usage)
          ((dispatch-helper 'guarded)
           (lambda ()
             (let* ((view ((dispatch-helper 'reduction-for) store state))
                    (row (state-read view t)))
               ;; AN UNKNOWN OR DELETED BLOCK IS ANSWERED AS READ ANSWERS IT: one
               ;; spelling per cause, so the read handler itself answers.
               (if (or (not row) (cdr (assq 'deleted row)))
                   ((dispatch-helper 'read) store actor (list t) req '() state writer cwd)
                   (guard (e ((refusal? e) (refusal-answer e)))
                     (assemble store view t budget all? (appended-clause state view))))))))))

  ;; WHAT THE DISPATCHER WILL APPEND after this answer, or #f: the clause that
  ;; says which writers the state could not read. It is part of the bytes a
  ;; caller receives, so the budget counts it.
  (define (appended-clause state view)
    (incomplete-clause (merge-unreadable (if (sealed-state? state) (sealed-state-notes state) '())
                                         (unreadable-behind view))))

  ;; ---- the hard material ------------------------------------------------------------------

  (define role-order '(unit member unsettled supersedes cause implementer))

  (define (assemble store view t budget all? appended)
    (let* ((S (make-query-session view '() query-budget-default ((dispatch-helper 'keyword-hook) store)))
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
             (notes (unique (map (lambda (r)
                                   (let ((a (car r)) (b (cadr r)))
                                     (if (string<? a b) (list 'nogood a b 'conflicts-with) (list 'nogood b a 'conflicts-with))))
                                 k5)))
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
            (budgeted store view (record-reader store view) S t budget all? order versions whys validity-clause place notes receipt
                      (lambda (h) (hashtable-ref validity h 'valid)) appended)))))

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

  ;; Breadth first from the unit over each record's depends-on and implements
  ;; edges, to members only.
  (define (member-distances view unit members)
    (let ((d (make-hashtable string-hash string=?)))
      (let loop ((frontier unit) (k 1))
        (unless (null? frontier)
          (let ((next (unique
                        (apply append
                               (map (lambda (id)
                                      (let ((row (state-read view id)))
                                        (filter (lambda (to) (and (member to members) (not (hashtable-ref d to #f))))
                                                (map cdr (filter (lambda (e) (memq (car e) '(depends-on implements)))
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
                                          ((implementer)
                                           (map (lambda (m) (list 'why 'implementer (car m) (caddr m)))
                                                (filter (lambda (m) (and (equal? (cadr m) h) (equal? (car m) (cadr r)))) k4)))
                                          (else '()))))
                                  (list-sort (lambda (a b) (string<? (format-about (cadr a)) (format-about (cadr b)))) roles))))
                    role-order)))))
  (define (format-about x) (if (string? x) x (call-with-string-output-port (lambda (p) (write x p)))))

  ;; ---- the budget ----------------------------------------------------------------------------

  (define section-names '(constraints evidence to-verify counterexamples background))
  (define excluded-names '(definitions evidence counterexamples dependents relevant ancestors))
  (define counter-cap 999999)

  ;; An ok answer without its cut fits when ten times its bytes are at most
  ;; thirty-six times the budget: four bytes a token, nine tenths usable.
  (define (fits? bytes budget) (<= (* 10 bytes) (* 36 budget)))

  ;; The answer's bytes as the wire writes it, the cut left out: its datum and
  ;; the newline after it.
  (define (answer-bytes form) (+ 1 (wire-length form)))

  (define (budgeted store view records S t budget all? order versions whys validity-clause place notes receipt validity-of appended)
    (let* ((version (lambda (h) (let ((p (assoc h versions))) (and p (cdr p)))))
           (identity (lambda (h) (entry view records h (version h) (whys h) (validity-clause h) 'identity)))
           (pair-of (lambda (h) (cons h (version h))))
           (cost (lambda (h) (+ (wire-length (identity h)) (wire-length (pair-of h)))))
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
           (base (lambda (tokens)
                   (append (list 'ok (list 'for (identity t)))
                           (map (lambda (n) (list n '())) section-names)
                           (list (list 'notes notes))
                           reserved-insufficient
                           (list reserved-excluded
                                 (list 'budget (list 'tokens tokens) (list 'used tokens) (list 'reserve tokens))
                                 receipt
                                 (list 'versions (list (pair-of t)))
                                 '(name-use syntactic)))))
           (tail (if appended (+ 1 (wire-length appended)) 0))
           (base-size (+ tail (answer-bytes (base budget)))))
      (if (not (fits? base-size budget))
          (list 'error 'budget-too-small (list 'minimum (minimum-budget base tail)))
          (admit store view records S t budget all? order whys validity-clause place notes receipt validity-of
                 version identity pair-of cost base-size tail))))

  ;; THE SMALLEST BUDGET THE RESERVATION AND THE BLOCK ASKED FOR FIT IN. The
  ;; reservation holds the budget at its own digits, so it is found digit
  ;; count by digit count.
  (define (minimum-budget base tail)
    (let loop ((digits 1))
      (let* ((low (if (= digits 1) 0 (expt 10 (- digits 1))))
             (size (+ tail (answer-bytes (base low))))
             (need (max low (div (+ (* 10 size) 35) 36))))
        (if (< need (expt 10 digits)) need (loop (+ digits 1))))))

  (define (admit store view records S t budget all? order whys validity-clause place notes receipt validity-of
                 version identity pair-of cost base-size tail)
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
      (define (delta s e pair) (+ (wire-length e) (if (> (count-of s) 0) 1 0) (wire-length pair) 1))
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
                    (let ((at-identity (wire-length (identity h))))
                      (let try ((ls '(full summary)))
                        (unless (null? ls)
                          (let ((d (- (wire-length (entry view records h (version h) (whys h) (validity-clause h) (car ls)))
                                      at-identity)))
                            (if (fits? (+ size d) budget)
                                (begin (set! size (+ size d)) (hashtable-set! levels h (car ls)))
                                (try (cdr ls)))))))))
                order)
      ;; B7: the preferred material in what room is left.
      (let* ((hard-entry (lambda (h) (entry view records h (version h) (whys h) (validity-clause h) (hashtable-ref levels h 'identity))))
             (L (lc:lifecycle view))
             (preferred (preferred-steps store view S t all? order place L))
             (excluded (map (lambda (n) (cons n 0)) excluded-names))
             (chosen '()))
        (for-each
          (lambda (step)
            (let ((counter (car step)) (s (cadr step)) (allowed (caddr step)))
              (for-each
                (lambda (c)
                  (let* ((id (car c)) (cwhys (cdr c))
                         (v (lc:validity-of L id))
                         (vclause (and (not (eq? (car v) 'valid)) (cons* 'validity (car v) (lc:validity-reasons v))))
                         (ver (version-of view id))
                         (pair (cons id ver)))
                    (let try ((ls allowed))
                      (if (null? ls)
                          (set-cdr! (assq counter excluded) (+ 1 (cdr (assq counter excluded))))
                          (let* ((e (entry view records id ver cwhys vclause (car ls))) (d (delta s e pair)))
                            (if (fits? (+ size d) budget)
                                (begin (set! size (+ size d)) (enter! s id e) (set! chosen (cons pair chosen)))
                                (try (cdr ls))))))))
                (cadddr step))))
          (car preferred))
        (finish view t budget order hard-entry place notes receipt missing cost sections excluded
                (reverse chosen) version (cdr preferred) tail))))

  ;; ---- the preferred material ------------------------------------------------------------------

  ;; -> (<steps> . <name use consulted>), each step (<counter> <section>
  ;; <levels allowed> <candidates>), a candidate (<id> <why> ...). A candidate
  ;; is never a hard block or the block asked for, and is in the earliest step
  ;; that names it.
  (define (preferred-steps store view S t all? order place L)
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
           ;; 1: the definitions of the names each hard code block in force uses.
           (code (filter (lambda (h) (and (eq? (field-value (state-read view h) 'kind) 'code) (in-force? h))) order))
           (uses (if (null? code) '()
                     (filter (lambda (r) (member (car r) code)) (rows-of S `(and (hard ,t ?b _ _) (uses-name ?b ?n))))))
           (defs (if (null? uses) '()
                     (filter (lambda (r) (member (car r) code)) (rows-of S `(and (hard ,t ?b _ _) (def-for ?b ?name ?d))))))
           (step1 (take (map (lambda (r)
                               (let ((others (filter (lambda (x) (and (equal? (car x) (car r)) (equal? (cadr x) (cadr r)))) defs)))
                                 (list (caddr r)
                                       (append (list 'why 'defines (cadr r) (car r))
                                               (if (> (length others) 1) '((ambiguous)) '())))))
                             (by (lambda (a b) (or (< (rank (car a)) (rank (car b)))
                                                   (and (= (rank (car a)) (rank (car b)))
                                                        (or (string<? (format-about (cadr a)) (format-about (cadr b)))
                                                            (and (equal? (cadr a) (cadr b)) (string<? (caddr a) (caddr b)))))))
                                 defs))
                         admitted?))
           ;; 2: the evidence of each constraint: what verifies it, what implements it.
           (evidence (lambda (rows why)
                       (map (lambda (r) (list (cadr r) (list 'why why (car r))))
                            (filter (lambda (r) (member (car r) constraints)) rows))))
           (step2 (take (by (lambda (a b) (let ((ca (caddr (cadr a))) (cb (caddr (cadr b))))
                                            (or (< (rank ca) (rank cb)) (and (= (rank ca) (rank cb)) (string<? (car a) (car b))))))
                            (append (evidence (rows-of S `(and (hard ,t ?c _ _) (verified-by ?c ?v))) 'verifies)
                                    (evidence (map (lambda (r) (list (cadr r) (car r)))
                                                   (rows-of S `(and (hard ,t ?c _ _) (edge ?v implements ?c))))
                                              'implements)))
                        admitted?))
           ;; 3: the blocks a constraint replaced, when they are not in force.
           (replaced (lambda (rel why)
                       (map (lambda (r) (list (cadr r) (list 'why why (car r))))
                            (filter (lambda (r) (and (member (car r) constraints)
                                                     (memq (car (lc:validity-of L (cadr r))) '(superseded refuted))))
                                    (rows-of S `(and (hard ,t ?c _ _) (edge ?c ,rel ?o)))))))
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
                                 (relevant store view t all? L)))
                        admitted?))
           ;; 6: the ancestors of hard blocks, nearest first.
           (step6 (take (apply append
                               (map (lambda (h) (map (lambda (a) (list a (list 'why 'contains h))) (ancestors view h)))
                                    order))
                        admitted?)))
      (cons (list (list 'definitions 'background '(summary identity) step1)
                  (list 'evidence 'background '(full summary identity) step2)
                  (list 'counterexamples 'counterexamples '(full summary identity) step3)
                  (list 'dependents 'background '(identity) step4)
                  (list 'relevant 'background '(full summary identity) step5)
                  (list 'ancestors 'background '(identity) step6))
            (pair? uses))))

  ;; THE HITS OF SEARCH for the block's title, and for its text when it has
  ;; one: the best score of each block. Without --all-validity, search's own
  ;; filter leaves out what is superseded or refuted, as `score` does.
  (define (relevant store view t all? L)
    (let* ((row (state-read view t))
           (texts (filter (lambda (s) (> (string-length s) 0)) (list (text-field row 'title) (text-field row 'src))))
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

  (define (finish view t budget order hard-entry place notes receipt missing cost sections excluded chosen version consulted tail)
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
                           shown
                           (list (list 'notes notes))
                           insufficient
                           (list excluded-clause
                                 (list 'budget (list 'tokens budget) (list 'used used) (list 'reserve (div budget 10)))
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
