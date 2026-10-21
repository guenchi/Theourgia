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
(library (theourgia working)
  (export working-write! working-read working-discard! working-list working-commit! latest-parent-cut
          working-restore! working-snapshot working-baseline working-state
          working-new! working-delete! working-structural provisional-id provisional-id?
          working-structural-outputs
          ;; NOTE: EXPORTED SO THAT NOBODY WRITES THIS PATH OUT A SECOND
          ;; TIME. The daemon has a seam that has to park inside exactly
          ;; the lock a write takes, and a seam holding a path spelled
          ;; independently would be holding a different file on the day
          ;; this one moved -- and would still look like it was working.
          (rename (lock-path draft-lock-path)))
  (import (rnrs) (only (rnrs eval) eval environment) (theourgia store) (theourgia reduce) (theourgia baseline)
          (theourgia wire) (theourgia digest) (only (theourgia markers) src-text)
          (only (theourgia log) store-writers writer-directory atomic-write!
                remember-unreadable-notes! unreadable-behind
                directory-entry-durable! present-or-unreadable-skip?)
          (only (theourgia answers) with-written)
          (only (theourgia arguments) working-id?)
          (only (theourgia ffi) mutation-record)
          (only (theourgia ffi) directory-entries file-is-directory? mkdir-p!
                process-id wall-clock-ms unlink! file-ensure! with-exclusive-lock barrier!
                hold-point!
                entry-type list-entries read-entry unreadable-entry? unreadable-entry-path
                unreadable-entry-reason fs-error? fs-error-op fs-error-errno)
          (only (theourgia datum-code) datum-source-read))

  ;; NEVER: A DRAFT OR A DIRECTORY THIS PROCESS CANNOT READ IS NOT ONE THAT IS
  ;; NOT THERE (U10). Presence is R1's question: absence (ENOENT,
  ;; ENOTDIR) is #f, and any other failure raises unreadable-entry naming
  ;; the path, which `problem` answers as working-unavailable with the path
  ;; and the system's reason. Chez's file-exists? answered #f for a draft
  ;; under an unreadable working/, so read --working gave the committed
  ;; text and commit gave no-draft.
  (define (present? p) (not (eq? (entry-type p) 'absent)))

  (define counter 0)
  (define (fresh-id)
    (set! counter (+ counter 1))
    (string-append "w-" (number->string (process-id)) "-"
                   (number->string (wall-clock-ms)) "-" (number->string counter)))
  (define (encode x)
    (string->utf8 (sexpr->string-extended (storable-encode x))))
  (define (digest x) (bytevector->hex (sha256 (encode x))))

  ;; ---- the version is the content's name -----------------------------------
  ;;
  ;; `version = sha256(bytes || based-on || cut)`. It is not a token and
  ;; not a counter: anyone can compute it, and two writes of the same
  ;; bytes on the same baseline get the same name, which is what lets a
  ;; retry say "this exact draft" without anybody having saved a
  ;; correspondence.
  ;;
  ;; NOTE: THE CONCATENATION IS UNAMBIGUOUS ONLY BECAUSE THE MIDDLE FIELD
  ;; HAS A FIXED WIDTH. `bytes` is arbitrary and comes first, so if
  ;; `based-on` could be any length then (bytes="ab", based-on="c") and
  ;; (bytes="a", based-on="bc") would hash alike -- and a collision here
  ;; answers "this is the draft you sent" for a draft nobody sent. A
  ;; block hash is 64 hex characters; this refuses to name anything else
  ;; rather than leave the assumption unwritten.
  ;; ONE DEFINITION, IN `reduce`. The completion path needs the same
  ;; rule -- it checks a plan's frozen text against the version the
  ;; record names -- and two copies of a hash rule are two answers
  ;; waiting to disagree.
  (define (content-version bytes based-on cut) (draft-version bytes based-on cut))

  ;; A DRAFT THAT DOES NOT HASH TO ITS OWN NAME IS NOT READ. The envelope
  ;; is a file on disk that nothing else guards; recomputing the name
  ;; from the three inputs it carries is the whole check, and it is made
  ;; wherever an envelope is read rather than only where it is written.
  (define (version-ok? entry)
    (equal? (list-ref entry 4)
            (content-version (list-ref entry 7) (list-ref entry 5) (list-ref entry 6))))
  ;; THE RULE IS arguments.sc's `working-id?`, shared with the MCP shell,
  ;; which checks its writer at start without loading this library.
  (define safe-id? working-id?)
  ;; R2g's SKIP, KEPT AS RULED, THROUGH THE DOOR: an owner.sexp
  ;; or retired.sexp that cannot be stat'ed reads as absent, by
  ;; present-or-unreadable-skip?, defined once in log.sc for both R2g sites.
  (define (writer-for store supplied)
    (cond
      (supplied (and (safe-id? supplied) supplied))
      (else (find (lambda (w)
                    (and (present-or-unreadable-skip? (string-append (writer-directory store w) "/owner.sexp"))
                         (not (present-or-unreadable-skip? (string-append (writer-directory store w) "/retired.sexp")))))
                  (store-writers store)))))
  ;; ---- whose drafts are these? ----------------------------------------------
  ;;
  ;; KEY: A REQUEST THAT NAMES NO WRITER IS REFUSED, and this is the one
  ;; place that decides it. `writer-for` above falls back to the store's
  ;; own log writer when nothing is supplied -- which is right for the
  ;; internal caller that wants "this store's writer", and catastrophic
  ;; for a request, because EVERY client that names no writer lands in the
  ;; same draft space.
  ;;
  ;; NEVER: MEASURED, AND IT IS NOT MERELY SHARING -- IT IS SILENT OVERWRITE.
  ;; Two clients with different actors, both without `--writer`, writing a
  ;; draft on one block:
  ;;
  ;;   agent-one write  -> (ok (saved …) (writer "esu85u1f") (version "783fde55…"))
  ;;   agent-two write  -> (ok (saved …) (writer "esu85u1f") (version "e98b10f5…"))
  ;;   agent-one drafts -> (draft … (writer "esu85u1f") (version "e98b10f5…"))
  ;;
  ;; The second write replaced the first, and the first client's own
  ;; `drafts` then reported the OTHER client's version as its own. NEVER:
  ;; Nothing anywhere said so: both writes answered `ok`, `drafts`
  ;; answered `ok`, and a version is a hash nobody checks against the one
  ;; they just wrote.
  ;;
  ;; NOTE: SO IT CANNOT BE LEFT TO CALLERS TO REMEMBER. A client that forgets
  ;; has no way to discover it from any answer it receives, which is why
  ;; the absence is refused rather than defaulted.
  ;;
  ;; NEVER: AND THERE IS NO STANDALONE EXEMPTION. The local path refuses on the
  ;; same terms as the forwarded one; a rule with a "but not when there is
  ;; no daemon" clause is two rules.
  (define (requested-writer store supplied)
    (if supplied (writer-for store supplied) 'unbound))

  (define (writer-required) '(error writer-required))

  ;; NEVER: AND IT IS REFUSED BEFORE THE STORE IS TOUCHED. The entry points
  ;; below bound `writer` and `state` in the same `let`, so opening and
  ;; folding the store happened first and only then was the missing
  ;; writer noticed. On a store that cannot be opened that turned a
  ;; caller-fixable mistake into a storage failure -- measured, an
  ;; unnamed writer answered `working-unavailable`, which is the value a
  ;; durability fault reports and the one `working-fault-child.sc` keys
  ;; its exit status on.
  ;;
  ;; NOTE: `requested-writer` ALREADY SHORT-CIRCUITS on an absent writer, so
  ;; this asks the same question one step earlier and nothing else
  ;; changes: the answer for a named writer is untouched.
  ;; NEVER: AND THIS IS THE ONLY PLACE THAT DECIDES IT. Each entry point used
  ;; to carry its own `(eq? writer 'unbound)` branch as well. Once the
  ;; guard moved out here those branches became unreachable -- one rule
  ;; with two suppliers, where the second can never fire and so can never
  ;; be found wrong. They are removed rather than left as reassurance.
  (define (needing-writer supplied thunk)
    (if supplied (thunk) (writer-required)))

  ;; NEVER: AND THE SAME FOR THE ARGUMENT SHAPES THAT DO NOT NEED THE STORE.
  ;; A block id is well formed or it is not, and the store has nothing to
  ;; say about it -- but the check sat after `open-and-reduce`, so on a
  ;; store that cannot be opened a malformed id was answered
  ;; `working-unavailable`. Measured:
  ;;
  ;;   healthy store,    bad id -> (error bad-request)
  ;;   unopenable store, bad id -> (error working-unavailable)
  ;;
  ;; which is the caller's mistake reported as a storage failure, and is
  ;; the same shape as the writer ordering above.
  (define (well-formed-first check thunk)
    (or (check) (thunk)))

  (define (block-name? name)
    (and (safe-id? name)
         (= 1 (length (filter (lambda (c) (char=? c #\.)) (string->list name))))
         (not (char=? #\. (string-ref name 0)))
         (not (char=? #\. (string-ref name (- (string-length name) 1))))))
  (define (area store writer name)
    (string-append (writer-directory store writer) "/" name))
  (define (ensure-area! store writer name)
    (let ((dir (area store writer name)))
      (mkdir-p! dir)
      (directory-entry-durable! (writer-directory store writer) 'working)
      (directory-entry-durable! dir 'working)
      dir))
  (define (path-for store writer id) (string-append (area store writer "working") "/" id))

  ;; ---- the one lock this library takes -------------------------------------
  ;;
  ;; TWO STEPS, AND ONLY TWO: the rename that installs a draft, and the
  ;; compare-then-unlink that retires one. Without it those two race
  ;; inside a SINGLE writer: retirement reads an envelope, decides its
  ;; version is consumed, and unlinks -- while a second process of the
  ;; same writer has renamed a new draft into that name in between. The
  ;; new draft is deleted and nothing says so.
  ;;
  ;; NEVER: IT IS NOT THE UNIT OF CONCURRENCY. Concurrency between writers is
  ;; provided by giving them separate draft spaces (7.5.3); this is one
  ;; writer's own consistency between two of its own processes, and the
  ;; lock is held across no wait but the I/O of those two steps.
  ;; NEVER: THE LOCK DOES NOT LIVE AMONG THE DRAFTS.
  ;;
  ;; It was `working/.lock`, and `discard` takes any `safe-id?` -- which
  ;; `.lock` is. A client could delete the lock file; the next two
  ;; processes then create and lock DIFFERENT inodes, and the retirement
  ;; that the lock exists to serialise runs beside an installation
  ;; again. A name-based exclusion would be one more list to keep in
  ;; step with `safe-id?`; putting the file where no block id can name
  ;; it removes the question.
  (define (lock-path store writer)
    (string-append (writer-directory store writer) "/draft.lock"))
  (define (with-draft-lock store writer thunk)
    (ensure-area! store writer "working")
    (let ((path (lock-path store writer)))
      (file-ensure! path)
      ;; INJECTION ONLY (item 7): held with the writer's scope open, its
      ;; area and lock file already recorded (A-record).
      (hold-point! 'write-after-create)
      (with-exclusive-lock path (lambda (fd) (thunk)))))
  (define (entry? x)
    (and (list? x) (= (length x) 8) (eq? (car x) 'working) (equal? (cadr x) 1)
         (safe-id? (list-ref x 2)) (safe-id? (list-ref x 3))
         (string? (list-ref x 4)) (string? (list-ref x 5))
         (list? (list-ref x 6)) (bytevector? (list-ref x 7))))
  ;; THE DRAFT'S BYTES ARE READ WITH R1's READ, then decoded (found in review):
  ;; a draft file this process cannot open raises unreadable-entry naming
  ;; it, where Chez's open-file-input-port raised an i/o condition that the
  ;; commit read as "no draft".
  (define (entry-at store writer id)
    (let* ((p (path-for store writer id))
           (bytes (read-entry p)))
      (and (not (eq? bytes 'absent))
           (let ((x (storable-decode (string->sexpr-extended (utf8->string bytes)))))
             (unless (and (entry? x) (equal? writer (list-ref x 2)) (equal? id (list-ref x 3)))
               (assertion-violation 'working "Corrupt working envelope" p))
             ;; NEVER: RAISED, NOT RETURNED, so that every reader of an
             ;; envelope gets the check whether or not it remembered to
             ;; ask for it. `problem` turns it into the answer the
             ;; caller sees; an assertion-violation here would read as a
             ;; defect in this core rather than as a damaged file.
             (unless (version-ok? x) (raise (list 'working-error 'version-mismatch id)))
             x))))
  (define (field state id name)
    (let* ((b (state-read state id)) (fs (and b (assq 'fields b)))
           (v (and fs (assq name (cdr fs)))))
      (and v (cdr v))))
  ;; A NAMED OUTCOME CARRIES THE RECORD (NO1): whatever the
  ;; enclosing point's scope had changed when the failure left is appended
  ;; as `(written ...)`; an empty record leaves the answer as it was.
  (define (problem thunk)
    (guard (e ((and (pair? e) (eq? 'working-error (car e)))
               (with-written (list 'error 'working-unavailable (list 'reason (cadr e)))
                             (mutation-record)))
              ((unreadable-entry? e) (with-written (unreadable-answer e) (mutation-record)))
              (#t (with-written
                    (list 'error 'working-unavailable
                          (list 'message (if (message-condition? e) (condition-message e) "Working storage failed")))
                    (mutation-record))))
      (thunk)))
  (define (invalid-writer) '(error bad-request invalid-working-writer))

  ;; ---- is this draft consumed -----------------------------------------------
  ;;
  ;; THE LOG SAYS SO, AND NOTHING ELSE DOES. Until this, each commit left
  ;; an immutable packet beside the drafts it named, and every read of a
  ;; draft asked every packet whether it had been replayed. That was two
  ;; stores to keep in step -- and a scan whose cost grew with the number
  ;; of commits the writer had ever made.
  ;;
  ;; A commit now names its drafts in its own plan record, and the
  ;; reduction keeps an index from (owner, version) to the plans that
  ;; named it. A draft is consumed once one of those plans has COMPLETED.
  (define (consumed? store state entry)
    (state-consumed? state (list-ref entry 2) (list-ref entry 4)))

  (define (unreadable-answer e)
    (list 'error 'working-unavailable
          (list 'path (unreadable-entry-path e))
          (list 'reason (unreadable-entry-reason e))))

  ;; R1's listing: no working/ (absent, or not a directory) is no drafts, as
  ;; before; one that cannot be listed raises unreadable-entry (U10).
  (define (active-entries store writer state)
    (let ((names (list-entries (area store writer "working"))))
      (if (eq? names 'absent) '()
          (filter (lambda (e) (not (consumed? store state e)))
            (map (lambda (id) (entry-at store writer id))
                 (filter block-name? (list-sort string<? names)))))))

  ;; ONE CUT COVERS ANOTHER when it has reached at least as far along
  ;; every writer the other names (`cut-covers?`, from the reduction, the
  ;; one definition). Two cuts that neither covers are CONCURRENT, which is
  ;; a fact about the store and not a tie to be broken.

  ;; A committed parent advances a sequential edit only to that
  ;; execution's causal cut. A later external edit is never adopted as
  ;; its baseline.
  ;;
  ;; THE CUT COMES FROM THE COMPLETION EVENT, not from the plan event.
  ;; A plan is written before its members, so its cut is earlier than the
  ;; state the commit produced -- taking it here reports the writer's own
  ;; next edit as stale against its own predecessor.
  ;;
  ;; SEVERAL REQUESTS MAY HAVE NAMED THE SAME VERSION. When their cuts
  ;; are comparable the latest is the parent; when they are not -- two
  ;; unsynchronised replicas each completed one -- there is no single
  ;; parent and the answer is #f, which sends the client to an explicit
  ;; `--rebase` rather than to a cut that is not after both.
  ;; THE PARENT AMONG SEVERAL COMMITTED CUTS: the one that covers every
  ;; other, or #f when there is none (or there are no cuts).
  ;; NEVER: NOT A FOLD THAT GIVES UP AT THE FIRST PAIR NOT ORDERED. That
  ;; answered #f for (A B C) and C for (C A B) when C covers A and B, which
  ;; are not ordered -- and the cuts come in the order the plans were
  ;; delivered, which is the route's. The best so far is replaced only by a
  ;; cut that covers it, and is the answer only if it covers every cut: the
  ;; cut that covers all, wherever it stands, becomes the best when it is
  ;; reached and stays it.
  (define (latest-parent-cut cuts)
    (and (pair? cuts)
         (let ((best (fold-left (lambda (best c) (if (cut-covers? c best) c best))
                                (car cuts) (cdr cuts))))
           (and (for-all (lambda (c) (cut-covers? best c)) cuts) best))))

  (define (committed-parent store state writer id version hash original-cut)
    (and writer version (safe-id? writer)
      ;; THE CUT IS THE JOIN OF THE PLAN'S MEMBER CUTS, which for every
      ;; plan this core writes is the last member's -- and for a plan
      ;; whose members came from two writers is the only cut that
      ;; contains both. `state-consumed-parent-cuts` computes it.
      (let ((latest (latest-parent-cut (filter values (state-consumed-parent-cuts state writer version)))))
        (and latest
          (let ((past (open-and-reduce store latest)))
            (and (reduction? past) past))))))

  (define (working-write! store state supplied id bytes rebase? . provenance)
    (needing-writer supplied (lambda ()
    (well-formed-first
      (lambda () (cond ((provisional-id? id) '(error bad-request provisional-target))
                       ((not (block-name? id)) '(error bad-request invalid-block-id))
                       (else #f)))
      (lambda ()
    (problem
      (lambda ()
        (let* ((writer (requested-writer store supplied)) (state (obtain-state store state #f))
               (hash (and (pair? provenance) (car provenance)))
               (cut-text (and (pair? provenance) (pair? (cdr provenance)) (cadr provenance)))
               (parent-writer (and (>= (length provenance) 4) (list-ref provenance 2)))
               (parent-version (and (>= (length provenance) 4) (list-ref provenance 3)))
               (historical (and hash cut-text
                 (guard (failure (#t #f))
                   (let ((parsed (parse-cut cut-text))) (and parsed (open-and-reduce store parsed))))))
               (parent (and (reduction? historical)
                            (committed-parent store state parent-writer id parent-version hash (reduce-applied-cut historical)))))
          (cond
            ((not writer) (invalid-writer))
            ((not (safe-id? id)) '(error bad-request invalid-block-id))
            ((not (state-read state id)) (list 'error 'unknown-id id))
            ;; a datum block's code is its body; a draft is text (store.sc)
            ((datum-mode-block? state id) (draft-on-datum-refusal id))
            ;; A PARENT NEEDS A BASELINE: an incomplete or unsafe parent, or one
            ;; named with no baseline at all, is refused here; one named with
            ;; HALF a baseline is answered below by the half that is missing.
            ((and (or parent-writer parent-version)
                  (not (and parent-writer parent-version (safe-id? parent-writer) (or hash cut-text))))
             '(error invalid-working-baseline))
            ;; NEVER: A REBASE NAMES THE VERSION IT MERGED ONTO.
            ;;
            ;; `--rebase` used to mean "take the block's hash as it is
            ;; this instant", and that is a claim the store cannot check
            ;; and the client did not make: the merge was done against
            ;; some version the client had READ, and between reading it
            ;; and saying `--rebase` somebody else may have committed.
            ;; Falling back to "now" records that later commit as the
            ;; baseline, and the next commit then overwrites it without
            ;; being refused.
            ((and rebase? (not (and hash cut-text)))
             '(error bad-request rebase-needs-baseline))
            ;; TWO WAYS A NAMED BASELINE CAN FAIL, AND THEY ARE NOT THE
            ;; SAME ANSWER. The cut may be unusable -- a conflict was
            ;; retracted and the state it names cannot be rebuilt -- in
            ;; which case nothing about the hash has been established and
            ;; the client's only move is an explicit rebase. Or the cut
            ;; is fine and the block simply never had that hash there,
            ;; which is a claim the client got wrong.
            ;; HALF A BASELINE IS NAMED BY WHAT IS MISSING. A version is
            ;; checked at the cut it was read at, so each needs the other;
            ;; one given alone used to be answered cut-unusable, which sent
            ;; a caller who had given no cut looking for what was wrong
            ;; with one.
            ((and hash (not cut-text))
             '(error invalid-working-baseline (reason cut-missing)))
            ((and cut-text (not hash))
             '(error invalid-working-baseline (reason based-on-missing)))
            ;; THE CUT CANNOT BE USED AT ALL -- it does not parse, or the
            ;; state it names cannot be rebuilt. Nothing about the hash
            ;; has been established, and the client's only move is an
            ;; explicit rebase.
            ((and (or hash cut-text) (not (reduction? historical)))
             '(error invalid-working-baseline (reason cut-unusable)))
            ;; THE CUT IS FINE AND THE BLOCK IS NOT IN IT. A different
            ;; answer from the one above, and from the one below: the
            ;; client named a moment before this block existed.
            ((and (or hash cut-text) (not (state-read historical id)))
             '(error invalid-working-baseline (reason block-not-at-cut)))
            ((and (or hash cut-text)
                  (not (equal? hash (block-hash historical id))))
             '(error invalid-working-baseline (reason hash-not-at-cut)))
            (else
             ;; THE BASELINE IS DECIDED FIRST AND THE NAME FOLLOWS FROM
             ;; IT. The version used to be a fresh counter, so two writes
             ;; of the same bytes on the same baseline had different
             ;; names and a retry could not say which draft it meant
             ;; without somebody having stored the answer.
             (let* ((old (entry-at store writer id))
                    (reuse (and old (not rebase?) (not (consumed? store state old))))
                    (body (if (string? bytes) (string->utf8 bytes) bytes))
                    ;; NEVER: A REBASE USES THE BASELINE IT NAMED. Both of
                    ;; these arms once required `(not rebase?)`, so an
                    ;; explicit rebase fell through to "now" -- it
                    ;; validated the version the client said it had
                    ;; merged onto and then recorded a different one,
                    ;; silently adopting whatever had been committed in
                    ;; between. That is the whole failure `--rebase
                    ;; --based-on` exists to prevent, performed by the
                    ;; code that implements it.
                    (based-on
                      (cond (reuse (list-ref old 5))
                            ((and parent (not rebase?)) (block-hash parent id))
                            (historical hash)
                            (else (block-hash state id))))
                    (cut
                      (cond (reuse (list-ref old 6))
                            ((and parent (not rebase?)) (reduce-applied-cut parent))
                            (historical (reduce-applied-cut historical))
                            (else (reduce-applied-cut state))))
                    (entry (list 'working 1 writer id
                                 (content-version body based-on cut)
                                 based-on cut body)))
               (unless (bytevector? (list-ref entry 7)) (assertion-violation 'write "Expected bytes" bytes))
               (with-draft-lock store writer
                 (lambda ()
                   (atomic-write! (path-for store writer id) (encode entry) 'working)))
               (list 'ok (list 'saved id) (list 'writer writer)
                     (list 'version (list-ref entry 4)) (list 'based-on (list-ref entry 5))))))))))))))

  ;; ---- putting a revoked draft back ----------------------------------------
  ;;
  ;; The file was retired by a commit that has since been taken back. Its
  ;; bytes are still in the log, and this is the verb that asks for them:
  ;; a CHANGED item's text is the one the plan froze, an UNCHANGED item's
  ;; text is the committed src at the cut the item names -- it had no
  ;; sub-operation, so the plan carries nothing for it.
  ;;
  ;; NEVER: THE VERSION IS RECOMPUTED BEFORE ANYTHING IS WRITTEN. The record
  ;; says `version`, `based-on` and `cut`; if the text those name does
  ;; not hash to that version, the record and the text disagree and this
  ;; refuses rather than standing a draft on bytes nobody sent.
  ;;
  ;; NEVER: AND THE BASELINE IS THE ONE THE RECORD HOLDS, not "now". A draft
  ;; restored onto the current hash would be a claim that it was edited
  ;; from the current text, and its next commit would overwrite whatever
  ;; happened in between without being refused.
  (define (working-restore! store state supplied version)
    (needing-writer supplied (lambda ()
    (problem
      (lambda ()
        (let* ((writer (requested-writer store supplied))
               (state (and writer (obtain-state store state #f)))
               (found (and state
                           (find (lambda (r) (equal? version (cadr (car r))))
                                 (state-revoked state writer)))))
          (cond
            ((not writer) (invalid-writer))
            ((not found) (list 'error 'unknown-version version))
            (else
             (let* ((item (car found))
                    (id (car item))
                    (based-on (caddr item))
                    (cut (cadddr item))
                    (declared (caddr found))
                    (text (or declared
                              (let ((past (guard (e (#t #f)) (open-and-reduce store cut))))
                                (and (reduction? past) (field past id 'src)))))
                    (body (and text (if (string? text) (string->utf8 text) text))))
               (cond
                 ;; NEVER: NO ROUTE MAKES A DRAFT ON A DATUM BLOCK (store.sc,
                 ;; datum-mode-block?), and restore is one: a revoked draft on
                 ;; one is refused by name and no draft file is written.
                 ((datum-mode-block? state id) (draft-on-datum-refusal id))
                 ((not body) (list 'error 'working-unavailable (list 'reason 'no-text-for-version)))
                 ((not (equal? version (content-version body based-on cut)))
                  (list 'error 'consumes-version-mismatch (list 'block id)))
                 (else
                  (let ((entry (list 'working 1 writer id version based-on cut body)))
                    (with-draft-lock store writer
                      (lambda ()
                        ;; THE SLOT IS READ BEFORE IT IS WRITTEN (found in review):
                        ;; a draft there that cannot be read raises
                        ;; unreadable-entry, and nothing replaces it unseen. A
                        ;; readable one is replaced, as before.
                        (read-entry (path-for store writer id))
                        (atomic-write! (path-for store writer id) (encode entry) 'working)))
                    (list 'ok (list 'restored id) (list 'writer writer)
                          (list 'version version) (list 'based-on based-on)))))))))))))) 

  (define (working-read store state supplied id . information)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          (else
            (let* ((e (and (safe-id? id) (entry-at store writer id)))
                   (active (and e (not (consumed? store state e))))
                   (body (if active (list-ref e 7) (or (field state id 'src) ""))))
              (cond
                ((not (state-read state id)) (list 'error 'unknown-id id))
                (else
                  (let* ((b (if (string? body) (string->utf8 body) body))
                         (text (src-text b)))
                    (if (and (pair? information) (car information))
                        (if text
                            (list 'ok (list 'projection (if active 'working 'committed) writer id
                              (and active (list-ref e 4))
                              (if active (list-ref e 5) (block-hash state id))
                              (if active (list-ref e 6) (reduce-applied-cut state)) text
                              (string-append (or (field state id 'front) "") (or (field state id 'heading-src) ""))))
                            '(error working-unavailable non-text-projection))
                        (if text (list 'ok (list 'text text)) (list 'ok (list 'bytes b))))))))))))))))

  ;; NEVER: NO `state` PARAMETER, AND THAT IS NOT AN OVERSIGHT. Every other
  ;; verb in this family takes one because it folds the log to answer;
  ;; this one removes a file from a writer's own directory and never
  ;; looks at a reduction at all. A parameter it ignored would say it
  ;; used the caller's value when it uses nothing.
  (define (working-discard! store supplied id)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)))
        (cond ((not writer) (invalid-writer))
              ;; A STRUCTURAL DRAFT IS NAMED BY ITS KEY: a new file's provisional
              ;; id, or delete- and the holder it deletes.
              ((structural-key? id)
               (with-draft-lock store writer
                 (lambda ()
                   (let ((p (structural-path store writer id)))
                     (unless (eq? (read-entry p) 'absent)
                       (unlink! p) (directory-entry-durable! p 'working))
                     (list 'ok (list 'discarded id))))))
              ;; A DRAFT IS NAMED BY A BLOCK, and `block-name?` is what
              ;; says so -- `safe-id?` admits names no block can have.
              ((not (block-name? id)) '(error bad-request invalid-block-id))
              ;; READ, NOT ONLY STATTED (found in review): a draft this process
              ;; cannot read raises unreadable-entry, and is not deleted unseen.
              (else (let ((p (path-for store writer id)))
                      (unless (eq? (read-entry p) 'absent)
                        (unlink! p) (directory-entry-durable! p 'working))
                      (list 'ok (list 'discarded id)))))))))))

;; ---- structural drafts: a file created, a file deleted -------------------
  ;;
  ;; A DRAFT THAT CREATES OR DELETES A FILE, beside the per-block drafts. They
  ;; live in the writer's `structural` area, apart from `working`, whose
  ;; readers take every name there for a block's envelope:
  ;;
  ;;   (working-new 1 <writer> <provisional id> <version> <path> <language> <cut> <bytes>)
  ;;     file <provisional id>: a file with these bytes at this path, split into
  ;;     blocks when committed, as the importer splits one (landing-files.sc);
  ;;   (working-delete 1 <writer> <holder> <version> <based-on> <cut>)
  ;;     file delete-<holder>: the file this holder carries, deleted, its
  ;;     based-on the file's baseline at the cut (reduce.sc, file-baseline).
  ;;
  ;; THE PROVISIONAL ID is reduce.sc's provisional-id of the path. THE VERSION is structural-version's (reduce.sc),
  ;; recomputed wherever an envelope is read. THE CUT is the draft's baseline,
  ;; joined into the working view's as a block draft's is; a draft written
  ;; again keeps its cut, as a block draft keeps its baseline.
  ;; NOTE: CODE-PROJECT IS ENTERED, NOT IMPORTED: this library loads at every
  ;; start, and only a structural draft needs a file's projection.
  (define (code-project-entry name) (eval name (environment '(theourgia code-project))))
  (define (hex-digest bytes) (bytevector->hex (sha256 bytes)))
  ;; a cut as the text a reader of a cut parses (parse-cut)
  (define (cut->text c) (call-with-string-output-port (lambda (p) (write c p))))
  (define (delete-key holder) (string-append "delete-" holder))
  (define (structural-key? x)
    (or (provisional-id? x)
        (and (string? x) (> (string-length x) 7) (string=? (substring x 0 7) "delete-")
             (block-name? (substring x 7 (string-length x))))))
  (define (structural-path store writer key) (string-append (area store writer "structural") "/" key))
  (define (new-entry? x)
    (and (list? x) (= (length x) 9) (eq? (car x) 'working-new) (equal? (cadr x) 1)
         (safe-id? (list-ref x 2)) (provisional-id? (list-ref x 3)) (string? (list-ref x 4))
         (string? (list-ref x 5)) (list? (list-ref x 6)) (list? (list-ref x 7)) (bytevector? (list-ref x 8))))
  (define (delete-entry? x)
    (and (list? x) (= (length x) 7) (eq? (car x) 'working-delete) (equal? (cadr x) 1)
         (safe-id? (list-ref x 2)) (block-name? (list-ref x 3)) (string? (list-ref x 4))
         (string? (list-ref x 5)) (list? (list-ref x 6))))
  (define (entry-key e) (if (eq? (car e) 'working-new) (list-ref e 3) (delete-key (list-ref e 3))))
  ;; -> the inputs of the version an envelope's own fields name:
  ;; (kind subject content cut)
  (define (structural-inputs e)
    (if (eq? (car e) 'working-new)
        (list 'new (list-ref e 5) (hex-digest (list-ref e 8)) (list-ref e 7))
        (list 'delete (list-ref e 3) (list-ref e 5) (list-ref e 6))))
  (define (structural-at store writer key)
    (let ((bytes (read-entry (structural-path store writer key))))
      (and (not (eq? bytes 'absent))
           (let ((x (storable-decode (string->sexpr-extended (utf8->string bytes)))))
             (unless (and (or (new-entry? x) (delete-entry? x)) (equal? writer (list-ref x 2))
                          (equal? key (entry-key x)))
               (assertion-violation 'working "Corrupt structural envelope" key))
             (let ((inputs (structural-inputs x)))
               (unless (and (apply structural-version? inputs) (equal? (list-ref x 4) (apply structural-version inputs)))
                 (raise (list 'working-error 'version-mismatch key))))
             x))))
  ;; A NAMED STRUCTURAL DRAFT IS READ AS A BLOCK DRAFT IS by a commit: a
  ;; failure is deferred, not raised, so a request carrying --req still
  ;; reaches the store, whose replay of it needs no draft at all; without
  ;; --req the failure is the answer, as for a block draft.
  ;; -> (<entry or #f> <deferred answer or #f> <unreadable?>)
  (define (deferred-read thunk)
    (guard (e ((unreadable-entry? e) (list #f (unreadable-answer e) #t))
              (#t (list #f
                        (if (and (pair? e) (eq? 'working-error (car e)))
                            (list 'error 'working-unavailable (list 'reason (cadr e)))
                            (list 'error 'working-unavailable
                                  (list 'message (if (message-condition? e) (condition-message e) "A draft could not be read"))))
                        #f)))
      (list (thunk) #f #f)))
  ;; -> the writer's live structural entries, by key
  (define (active-structural store writer state)
    (let ((names (list-entries (area store writer "structural"))))
      (if (eq? names 'absent) '()
          (filter (lambda (e) (not (consumed? store state e)))
                  (map (lambda (k) (structural-at store writer k))
                       (filter structural-key? (list-sort string<? names)))))))
  ;; -> (<bytes> <child ids>) of the live text file HOLDER carries in STATE, or #f
  (define (file-content state holder)
    (and (block-name? holder)
         (eq? 'text ((code-project-entry 'code-field) state holder 'mode))
         (equal? 'file ((code-project-entry 'code-field) state holder 'kind))
         (let ((file ((code-project-entry 'projected-file) state holder)))
           (and (pair? file)
                (let ((children (cadddr file)))
                  (list (apply-bytes (map (lambda (c) (let ((s (cadr c))) (if (string? s) (string->utf8 s) s)))
                                          children))
                        (map car children)))))))
  (define (apply-bytes parts)
    (let ((out (make-bytevector (fold-left + 0 (map bytevector-length parts)))))
      (let loop ((ps parts) (at 0))
        (if (null? ps) out
            (begin (bytevector-copy! (car ps) 0 out at (bytevector-length (car ps)))
                   (loop (cdr ps) (+ at (bytevector-length (car ps)))))))))
  ;; -> the live text holder of PATH in STATE, or #f
  (define (path-holder state path)
    (find (lambda (id) (and (eq? 'text ((code-project-entry 'code-field) state id 'mode))
                            (equal? path ((code-project-entry 'code-field) state id 'path))))
          ((code-project-entry 'code-files) state)))
  ;; -> the file baseline of HOLDER in STATE, or #f
  (define (holder-baseline state holder)
    (let ((c (file-content state holder))) (and c (file-baseline (car c) (cadr c)))))
  ;; -> the state at CUT-TEXT, or STATE when there is none; #f when unusable
  (define (state-at-cut-text store state cut-text)
    (if cut-text
        (guard (e (#t #f))
          (let ((parsed (parse-cut cut-text))) (and parsed (let ((s (open-and-reduce store parsed))) (and (reduction? s) s)))))
        state))
  ;; SAVE A STRUCTURAL ENTRY, or answer the one saved when it is already this
  ;; version: a retry writes nothing.
  (define (save-structural! store writer entry)
    (with-draft-lock store writer
      (lambda ()
        (let ((old (structural-at store writer (entry-key entry))))
          (unless (and old (equal? (list-ref old 4) (list-ref entry 4)))
            (ensure-area! store writer "structural")
            (atomic-write! (structural-path store writer (entry-key entry)) (encode entry) 'working))
          (list 'ok (list 'saved (entry-key entry)) (list 'writer writer) (list 'version (list-ref entry 4)))))))
  ;; A FILE CREATED: PATH with BYTES, LANGUAGE its language fields as the
  ;; importer gives them. CUT-TEXT is the cut it stands on (the export's), kept
  ;; from an earlier draft of the same path when there is one. Refused: the
  ;; path held by a live file at that cut (path-exists), bytes that are not
  ;; text (not-text), an unusable cut.
  (define (working-new! store state supplied path language bytes cut-text)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let* ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          ((not (and (string? path) (> (string-length path) 0) (bytevector? bytes) (list? language)))
           '(error bad-request invalid-new-file))
          ((not (src-text bytes)) (list 'error 'not-text (list 'path path)))
          (else
           (let* ((pid (provisional-id path))
                  (old (structural-at store writer pid))
                  (live-old (and old (not (consumed? store state old)) old))
                  (at (if live-old (state-at-cut-text store state (cut->text (list-ref live-old 7)))
                          (state-at-cut-text store state cut-text))))
             (cond
               ((not at) '(error invalid-working-baseline (reason cut-unusable)))
               ((path-holder at path) (list 'error 'path-exists (list 'path path)))
               (else
                (let ((cut (if live-old (list-ref live-old 7) (reduce-applied-cut at))))
                  (save-structural! store writer
                    (list 'working-new 1 writer pid
                          (structural-version 'new path (hex-digest bytes) cut)
                          path language cut bytes))))))))))))))
  ;; A FILE DELETED: the file HOLDER carries, at CUT-TEXT (kept from an earlier
  ;; draft as above). Refused: no live text file holder there (not-a-file).
  (define (working-delete! store state supplied holder cut-text)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let* ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          ((not (block-name? holder)) '(error bad-request invalid-block-id))
          (else
           (let* ((old (structural-at store writer (delete-key holder)))
                  (live-old (and old (not (consumed? store state old)) old))
                  (at (if live-old (state-at-cut-text store state (cut->text (list-ref live-old 6)))
                          (state-at-cut-text store state cut-text)))
                  (based-on (and at (holder-baseline at holder))))
             (cond
               ((not at) '(error invalid-working-baseline (reason cut-unusable)))
               ((not based-on) (list 'error 'not-a-file (list 'holder holder)))
               (else
                (let ((cut (if live-old (list-ref live-old 6) (reduce-applied-cut at))))
                  (save-structural! store writer
                    (list 'working-delete 1 writer holder
                          (structural-version 'delete holder based-on cut)
                          based-on cut))))))))))))))
  ;; -> (ok <writer> <items>): each live structural draft, (new (provisional
  ;; <id>) (path <p>) (version <v>) (cut <c>) (bytes <b>)) or (delete (holder
  ;; <h>) (version <v>) (based-on <b>) (cut <c>))
  (define (working-structural store state supplied)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (if (not writer)
            (invalid-writer)
            (list 'ok writer
                  (map structural-item
                       (with-draft-lock store writer (lambda () (active-structural store writer state))))))))))))
  (define (structural-item e)
    (if (eq? (car e) 'working-new)
        (list 'new (list 'provisional (list-ref e 3)) (list 'path (list-ref e 5)) (list 'version (list-ref e 4))
              (list 'cut (list-ref e 7)) (list 'bytes (list-ref e 8)))
        (list 'delete (list 'holder (list-ref e 3)) (list 'version (list-ref e 4))
              (list 'based-on (list-ref e 5)) (list 'cut (list-ref e 6)))))

;; THE WORKING TREE'S FILES: OUTPUTS, ((<path> <bytes>) ...) of VIEW, with
  ;; STRUCTURAL, the entries working-state answered beside VIEW, laid over
  ;; them: each new file added, each deleted holder's file left out
  ;; (landing-files.sc, structural-outputs).
  (define (working-structural-outputs view structural outputs)
    (if (null? structural)
        outputs
        ((eval 'structural-outputs (environment '(theourgia landing-files)))
         outputs
         (map (lambda (e) (list (list-ref e 5) (list-ref e 8)))
              (filter (lambda (e) (eq? (car e) 'working-new)) structural))
         (map (lambda (e) ((code-project-entry 'code-field) view (list-ref e 3) 'path))
              (filter (lambda (e) (eq? (car e) 'working-delete)) structural)))))

  ;; ---- committing structural drafts ---------------------------------------
  ;;
  ;; NAMED BY KEY AND VERSION: --working-new <provisional id>=<version> and
  ;; --working-delete <holder>=<version>. -> ((new|delete <key> <version>) ...),
  ;; or #f when one is malformed, named twice, or deletes a holder the request
  ;; also names as a block (one consumes item a block).
  (define (parse-structural new-specs delete-specs ids)
    (let ((parsed (append (map (lambda (s) (let ((p (split-at-equals s)))
                                             (and p (provisional-id? (car p)) (> (string-length (cdr p)) 0)
                                                  (list 'new (car p) (cdr p)))))
                               new-specs)
                          (map (lambda (s) (let ((p (split-at-equals s)))
                                             (and p (block-name? (car p)) (> (string-length (cdr p)) 0)
                                                  (not (member (car p) ids))
                                                  (list 'delete (car p) (cdr p)))))
                               delete-specs))))
      (and (for-all values parsed)
           (no-duplicate-blocks? (map (lambda (n) (cons (structural-file-key n) (caddr n))) parsed))
           parsed)))
  (define (structural-file-key n) (if (eq? (car n) 'new) (cadr n) (delete-key (cadr n))))
  (define (structural-pair n) (cons (structural-file-key n) (caddr n)))
  ;; THE PREMISES OF THE NAMED STRUCTURAL DRAFTS, against CURRENT, the state
  ;; the write lands on: each is there at the version named; a new file's path
  ;; is held by no live file; a deleted file is still the file its draft was
  ;; written on (bytes and children: its baseline). -> #f, or the refusal.
  (define (structural-preflight current structural)
    (let loop ((ps structural))
      (if (null? ps) #f
          (let ((n (car (car ps))) (e (cdr (car ps))))
            (cond
              ((not e) (list 'error 'no-draft (list 'blocks (structural-file-key n))))
              ((not (equal? (caddr n) (list-ref e 4)))
               (list 'error 'working-version-changed (list 'blocks (structural-file-key n))))
              ((and (eq? (car e) 'working-new) (path-holder current (list-ref e 5)))
               (list 'error 'path-exists (list 'path (list-ref e 5))))
              ((and (eq? (car e) 'working-delete)
                    (not (equal? (holder-baseline current (list-ref e 3)) (list-ref e 5))))
               (list 'error 'stale-delete (list 'holder (list-ref e 3))))
              (else (loop (cdr ps))))))))
  ;; THE INTENTS OF THE STRUCTURAL DRAFTS, appended to a batch whose first
  ;; FIRST intents come before them: each new file by the importer's own
  ;; construction, each deleted file's children and then its holder, read from
  ;; CURRENT. -> (values <intents> ((<provisional id> . <holder insert's index>) ...))
  (define (structural-intents current first entries)
    (if (null? entries)
        (values '() '())
        (let ((lf (lambda (name) (eval name (environment '(theourgia landing-files))))))
          (let loop ((es entries) (k first) (out '()) (at '()))
            (if (null? es)
                (values (apply append (reverse out)) (reverse at))
                (let* ((e (car es))
                       (is (if (eq? (car e) 'working-new)
                               ((lf 'file-new-intents) (list-ref e 5) (list-ref e 8) k)
                               (let ((c (file-content current (list-ref e 3))))
                                 ((lf 'file-delete-intents) (list-ref e 3) (if c (cadr c) '()))))))
                  (loop (cdr es) (+ k (length is)) (cons is out)
                        (if (eq? (car e) 'working-new) (cons (cons (list-ref e 3) k) at) at))))))))
  ;; A STRUCTURAL DRAFT IN A PLAN'S consumes: (<key id> <version> <content> <cut>),
  ;; the content the version is computed from (the new file's bytes' sha256,
  ;; or the deleted file's baseline)
  (define (structural-consumes-item e)
    (if (eq? (car e) 'working-new)
        (list (list-ref e 3) (list-ref e 4) (hex-digest (list-ref e 8)) (list-ref e 7))
        (list (list-ref e 3) (list-ref e 4) (list-ref e 5) (list-ref e 6))))
  ;; THE STRUCTURAL DRAFTS A COMMIT CONSUMED, removed when their envelope is
  ;; still the one consumed (a later write keeps its file). -> #f, or the
  ;; cleanup failure.
  (define (retire-structural! store writer entries)
    (and (pair? entries)
         (guard (failure (#t (cleanup-failed (lock-path store writer) failure)))
           (with-draft-lock store writer
             (lambda ()
               (for-each (lambda (e)
                           (let* ((k (entry-key e)) (now (structural-at store writer k)))
                             (when (and now (equal? (list-ref now 4) (list-ref e 4)))
                               (let ((p (structural-path store writer k)))
                                 (unlink! p) (directory-entry-durable! p 'working)))))
                         entries)
               #f)))))
  ;; A REPLAY'S MAPPING IS READ FROM THE RECORDS IT REPEATS: the drafts were
  ;; retired by the commit it replays, so no holder is known here, and its
  ;; answer is (ok (replay #t) (event <writer . seq>)) with no events to read.
  ;; The record at that event carries the request's identity in its actor, as
  ;; every record of the request does; read in the order they were applied,
  ;; each holder insert among them names its path, whose provisional id this
  ;; request names, and its own event is the id it made.
  (define (replayed-created-clause state answers pids)
    (let* ((ev (exists (lambda (a) (and (pair? a) (eq? (car a) 'ok)
                                        (let ((c (assq 'event (cdr a)))) (and c (pair? (cdr c)) (pair? (cadr c)) (cadr c)))))
                       answers))
           (identity-of (lambda (r) (let ((a (list-ref r 4))) (and (list? a) (> (length a) 2) (list-ref a 1)))))
           (history (let ((h (assq 'request-history (state->rows state)))) (if h (cadr h) '())))
           (at (and ev (find (lambda (r) (and (equal? (car r) (car ev)) (equal? (cadr r) (cdr ev)))) history)))
           (identity (and at (identity-of at)))
           (made (if identity
                     (filter values
                             (map (lambda (r)
                                    (let ((p (list-ref r 3)))
                                      (and (equal? (identity-of r) identity)
                                           (pair? p) (eq? (car p) 'put) (pair? (cdr p)) (list? (cadr p))
                                           (let ((kind (assq 'kind (cadr p))) (path (assq 'path (cadr p))))
                                             (and kind (eq? (cdr kind) 'file) path (string? (cdr path))
                                                  (let ((pid (provisional-id (cdr path))))
                                                    (and (member pid pids) (cons pid (block-id (car r) (cadr r))))))))))
                                  history))
                     '())))
      (if (null? made) '() (list (cons 'created made)))))
  ;; (created (<provisional id> . <holder id>) ...): each new file's committed
  ;; holder, read from its insert's answer; nothing when no file was created
  (define (created-clause holders answers)
    (define (made-id answer)
      (let ((ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer)))))
        (and ev (pair? (cadr ev)) (block-id (car (car (cadr ev))) (cdr (car (cadr ev)))))))
    (if (null? holders) '()
        (list (cons 'created (map (lambda (h) (cons (car h) (and (< (cdr h) (length answers))
                                                                 (made-id (list-ref answers (cdr h))))))
                                  holders)))))

  ;; "NOTHING TO COMMIT" IS READ, NOT STORED.
  ;;
  ;; A draft whose bytes are what the block already says is not a
  ;; change. That is a comparison anyone can make at any time, so the
  ;; envelope does not carry a flag for it: a stored flag is a second
  ;; copy of a fact, and it goes out of date the moment somebody else
  ;; commits.
  ;;
  ;; NEVER: STALE COMES FIRST. If the baseline is no longer the block's
  ;; current hash then the draft is stale, and what its bytes happen to
  ;; equal is not the question -- the committed text it would be
  ;; compared against is not the one it was written on. A build that
  ;; answered "unchanged" there would turn a conflict into a no-op.
  (define (unchanged? state entry fresh)
    (and fresh
         (let ((src (field state (list-ref entry 3) 'src)))
           (and (or (string? src) (bytevector? src))
                (equal? (list-ref entry 7)
                        (if (string? src) (string->utf8 src) src))))))

  ;; NEVER: THE WHOLE OF A WRITER'S LIVE DRAFTS, TAKEN AT ONE INSTANT, UNDER
  ;; THAT WRITER'S OWN LOCK. An evaluation runs against a view that must
  ;; not change under it: the committed side is pinned by the cut, which
  ;; loads deterministically, and the drafts are pinned by being copied
  ;; out here, before the child that will read them exists. A commit
  ;; landing mid-run changes neither.
  ;;
  ;; NOTE: THE LOCK IS HELD ONLY FOR THE COPY. Nothing holds it during the
  ;; evaluation -- section 7.6 is explicit that a run takes no store or
  ;; draft lock (its admission slot, under the run root, is neither) -- so
  ;; what this returns is a snapshot of that moment and says nothing about
  ;; the moment after it.
  ;;
  ;; NOTE: FOUR FIELDS, NOT ONE. The bytes are what the evaluation reads;
  ;; the version and the base are what the answer reports back, so a
  ;; caller can tell which draft it actually got.
  (define (working-snapshot store state supplied)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          (else
            (list 'ok writer
                  (with-draft-lock store writer
                    (lambda ()
                      (map entry-draft (active-entries store writer state)))))))))))))

  ;; The four fields of an entry a view carries: id, version, base, bytes.
  (define (entry-draft e)
    (list (list-ref e 3) (list-ref e 4) (list-ref e 5) (list-ref e 7)))

  ;; THE BASELINE OF A WRITER'S WORKING VIEW: the join of the cuts its
  ;; live drafts were written against.
  ;;
  ;; NEVER: THIS IS WHAT `--working` EVALUATES AT, AND THE REASON IS THE
  ;; USER'S RULING: a writer's working view stands on the state ITS OWN
  ;; drafts record, so another writer's commit made after those drafts
  ;; does not walk into it. Evaluating at the current committed state
  ;; instead -- which is what the tree did -- means a draft is read
  ;; against a store it was never written against, and two runs of the
  ;; same unchanged draft can answer differently because somebody else
  ;; committed in between.
  ;;
  ;; NOTE: A JOIN, NOT ONE DRAFT'S CUT. The drafts may have been written at
  ;; different times; the view has to contain all of them, and the join
  ;; is the only cut that does.
  ;;
  ;; NOTE: EMPTY WHEN THERE ARE NO DRAFTS, and the caller reads that as the
  ;; current committed state: a writer with nothing in progress has
  ;; nothing to be pinned to.
  ;;
  ;; NEVER: THE ENTRY LAYOUT STAYS IN THIS FILE. `behind-clauses` folds the same
  ;; field of the same entries; a caller doing it would be a second
  ;; place that knows an envelope's sixth element is its cut.
  (define (working-baseline store state supplied)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          (else
            (list 'ok writer
                  (with-draft-lock store writer
                    (lambda ()
                      (cut-join (entries-cut (active-entries store writer state))
                                (structural-cut (active-structural store writer state))))))))))))))

  (define (entries-cut entries)
    (fold-left (lambda (acc e) (cut-join acc (list-ref e 6))) '() entries))
  ;; the join of the structural entries' cuts (a new file's is its eighth
  ;; field, a delete's its seventh)
  (define (structural-cut entries)
    (fold-left (lambda (acc e) (cut-join acc (list-ref e (if (eq? (car e) 'working-new) 7 6)))) '() entries))

  ;; ---- the working view as a reduction -------------------------------------
  ;;
  ;; NEVER: ONE VIEW, THE ONE `eval --working` RUNS ON (design 7.5.21). The
  ;; committed side is the state at the writer's baseline -- the join of
  ;; its drafts' cuts, `working-baseline` -- and every block with a live
  ;; draft reads as that draft. A writer with no drafts has an empty
  ;; baseline and gets the current state, as eval does.
  ;;
  ;; NOTE: THE DRAFTS AND THE BASELINE ARE TAKEN UNDER ONE LOCK, so the cut
  ;; is the join of exactly the drafts that are overlaid. Taking them under
  ;; two locks, as the eval route does, lets a write land between the two.
  ;;
  ;; NEVER: THE STATE IS COPIED, NOT CHANGED. open-and-reduce may hand back a
  ;; state other readers hold; the copy goes through the snapshot rows, and
  ;; a drafted block gets a new field list, so nothing the original shares
  ;; is touched.
  ;;
  ;; NOTE: ONE STORED FIELD IS REPLACED PER DRAFT, the one the draft's bytes
  ;; stand for. A text block's `src` keeps the type it has in the committed
  ;; state: a bytevector stays bytes, a string reads the draft as UTF-8;
  ;; what is derived from it (a code block's name) comes from the view
  ;; layer, which derives it from the new src. A datum block stores no src:
  ;; its draft is read as its `body` (design 7.5.21, "datum blocks use the body in
  ;; the draft"), exactly one form, and a draft that does not read is
  ;; refused by name rather than left out of the view. A block with neither
  ;; is left as it is, the rule eval-context's `with-draft` applies.
  ;;
  ;; KEY: A DRAFT CANNOT INSERT A BLOCK -- working-write! refuses an id the
  ;; state does not hold -- so the view has the committed state's blocks.
  ;; NOTE: THE STRUCTURAL DRAFTS ARE THE ANSWER'S FIFTH ELEMENT, not part of
  ;; the view: a new file has no blocks until it is committed, so only the
  ;; file trees built from this answer (export-code --working, edit-export,
  ;; landing-freeze: landing-files.sc structural-outputs) show it, and a
  ;; deleted file is hidden there alone. outline, context and read --working
  ;; read the view and show neither (v1).
  (define (working-state store state supplied)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          (else
            (let* ((both (with-draft-lock store writer
                           (lambda () (cons (active-entries store writer state)
                                            (active-structural store writer state)))))
                   (entries (car both))
                   (structural (cdr both))
                   (cut (cut-join (entries-cut entries) (structural-cut structural)))
                   (drafts (map entry-draft entries))
                   (base (if (null? cut) (open-and-reduce store) (open-and-reduce store cut))))
              (if (not (reduction? base))
                  (list 'error 'working-unavailable (list 'reason 'unavailable-cut) (list 'cut cut))
                  (let ((fields (draft-fields base drafts)))
                    (if (eq? (car fields) 'error)
                        fields
                        (list 'ok writer (overlay-drafts base (cdr fields)) drafts structural)))))))))))))

  ;; `(ok (<id> <field> . <value>) ...)`, the stored field each draft
  ;; replaces, or the refusal of the first draft that does not read.
  (define (draft-fields base drafts)
    (let loop ((ds drafts) (acc '()))
      (if (null? ds)
          (cons 'ok (reverse acc))
          (let* ((id (car (car ds))) (bytes (cadddr (car ds)))
                 (b (state-read base id))
                 (fs (if (and (pair? b) (assq 'fields b)) (cdr (assq 'fields b)) '()))
                 (src (assq 'src fs)))
            (cond
              ((and (equal? (assq 'mode fs) '(mode . datum)) (assq 'body fs))
               (let ((body (draft-body id bytes)))
                 (if (eq? (car body) 'error)
                     body
                     (loop (cdr ds) (cons (cons id (cons 'body (cadr body))) acc)))))
              (src
               (loop (cdr ds)
                     (cons (cons id (cons 'src (if (bytevector? (cdr src)) bytes (utf8->string bytes))))
                           acc)))
              (else (loop (cdr ds) acc)))))))

  (define (draft-body id bytes)
    (define (refused reason)
      (list 'error 'working-draft-unreadable (list 'block id) reason))
;; NOTE: ONLY THE READER'S REFUSAL IS CAUGHT. datum-source-read answers
    ;; every failure of the text as an `(error bad-source ...)` list -- its
    ;; own guard turns any other condition into `(reason reader-rejected)` --
    ;; so that list is the one thing a draft that does not read can raise,
    ;; and a catch-all here would only hide what is not the draft's fault.
    (guard (e ((and (pair? e) (eq? (car e) 'error))
               (refused (or (assq 'reason (filter pair? e)) '(reason reader-rejected)))))
      (let ((forms (datum-source-read bytes)))
        (if (= (length forms) 1)
            (list 'body (car (car forms)))
            (refused '(reason expected-one-form))))))

;; NOTE: A STORED FIELD IS A REGISTER, NOT A VALUE: `(name (value writer .
  ;; seq) ...)`, one candidate per concurrent write (reduce.sc; measured,
  ;; `(src ("" "ka7a7y1s" . 1))`). The first version wrote `(src . bytes)`
  ;; into the rows and the rebuilt state raised "not a proper list". The
  ;; view's register holds ONE candidate: the draft's value, carrying the
  ;; provenance of the committed candidate it stands over -- the draft
  ;; settles the field for this writer, as a commit of it would.
  (define (drafted-register f value)
    (let ((candidates (cdr f)))
      (list (car f)
            (cons value (if (and (pair? candidates) (pair? (car candidates)))
                            (cdr (car candidates))
                            (cons "" 0))))))

  ;; THE OVERLAY KEEPS ITS BASE'S NOTES (found in design review): it is a
  ;; new value built from the base's rows, and whatever the base was
  ;; missing it is missing too.
  (define (overlay-drafts base replacements)
    (let ((overlay (overlay-drafts-rows base replacements)))
      (remember-unreadable-notes! overlay (unreadable-behind base))
      overlay))

  (define (overlay-drafts-rows base replacements)
    (rows->state
      (map (lambda (row)
             (let ((r (and (eq? (car row) 'block) (assoc (cadr row) replacements))))
               (if (not r) row
                   (list 'block (cadr row)
                         (map (lambda (part)
                                (if (eq? (car part) 'fields)
                                    (cons 'fields (map (lambda (f) (if (and (pair? f) (eq? (car f) (cadr r))) (drafted-register f (cddr r)) f))
                                                       (cdr part)))
                                    part))
                              (caddr row))))))
           (state->rows base))))

  (define (working-list store state supplied)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      (let ((writer (requested-writer store supplied)) (state (obtain-state store state #f)))
        (cond
          ((not writer) (invalid-writer))
          (else
            (list 'ok (cons 'items
              (append
              (map (lambda (e)
                     (let* ((id (list-ref e 3))
                            (now (block-hash state id))
                            (fresh (equal? (list-ref e 5) now)))
                       (list 'draft (list 'block id) (list 'writer writer)
                             (list 'version (list-ref e 4)) (list 'based-on (list-ref e 5))
                             (list 'now now)
                             (list 'fresh fresh)
                             (list 'unchanged (unchanged? state e fresh)))))
                   (active-entries store writer state))
              ;; NEVER: A REVOKED CONSUMPTION IS NOT A DRAFT, AND IT IS NOT
              ;; NOTHING. The file was retired by a commit that has since
              ;; been taken back; the bytes are in the plan record, and
              ;; `write --restore <version>` puts them back. Saying
              ;; nothing here is what makes the work look deleted.
              ;;
              ;; NEVER: AND THE FILE IS NOT RESURRECTED BEHIND THE PERSON'S
              ;; BACK. Whether to bring a draft back is theirs to decide;
              ;; a store that re-created files during a read would be
              ;; writing on a path nobody asked to write on.
              (map (lambda (r)
                     (let ((item (car r)))
                       (list 'revoked (list 'block (car item)) (list 'writer writer)
                             (list 'version (cadr item))
                             (list 'based-on (caddr item))
                             (list 'plan-event (cadr r)))))
                   (filter (lambda (r)
                             (not (present? (path-for store writer (car (car r))))))
                           (state-revoked state writer)))
              ;; THE STRUCTURAL DRAFTS, after the block drafts: (new ...) and
              ;; (delete ...) items, the new file's bytes left out of the
              ;; listing (working-structural reads them)
              (map (lambda (e)
                     (let ((item (structural-item e)))
                       (if (eq? (car item) 'new)
                           (append (list 'new (list 'writer writer))
                                   (filter (lambda (c) (not (eq? (car c) 'bytes))) (cdr item)))
                           (append (list 'delete (list 'writer writer)) (cdr item)))))
                   (active-structural store writer state)))))))))))))

  ;; THE (block . version) PAIRS THIS REQUEST IS ABOUT.
  ;;
  ;; A commit that carries `--req` must be GIVEN its versions: a retry
  ;; from a new process has no draft to read them from, and "infer once
  ;; and reuse" has nowhere to keep the inference. A commit without
  ;; `--req` makes no retry promise, so it reads them from the drafts it
  ;; is about to consume.
  ;; `<block>=<version>`, or a bare version when exactly one block is
  ;; named. The pairs a request declares are a SET: one per block, and
  ;; repeating the option is how a caller spells more than one.
  (define (split-at-equals s)
    (let loop ((i 0))
      (cond ((= i (string-length s)) #f)
            ((char=? #\= (string-ref s i))
             (cons (substring s 0 i) (substring s (+ i 1) (string-length s))))
            (else (loop (+ i 1))))))

  (define (parse-version-spec ids spec)
    (let ((split (split-at-equals spec)))
      (cond
        (split split)
        ((= 1 (length ids)) (cons (car ids) spec))
        (else #f))))

  ;; NEVER: ONE VERSION PER BLOCK. Two values naming the same block leave
  ;; "which version did this request consume" without an answer -- and
  ;; the answer matters twice: the identity is taken over the pairs, and
  ;; RETIREMENT deletes the drafts whose version is among them. Accepting
  ;; both `A=v1` and `A=v2` let a completion delete a replacement its
  ;; request never consumed.
  (define (no-duplicate-blocks? pairs)
    (let loop ((ps pairs) (seen '()))
      (or (null? ps)
          (and (not (member (car (car ps)) seen))
               (loop (cdr ps) (cons (car (car ps)) seen))))))

  (define (commit-versions ids entries selected)
    (cond
      ((pair? selected)
       (let ((parsed (map (lambda (spec) (parse-version-spec ids spec)) selected)))
         (and (for-all values parsed) (no-duplicate-blocks? parsed) parsed)))
      (else (map (lambda (e) (cons (list-ref e 3) (list-ref e 4))) entries))))

  ;; WHICH NAMED BLOCKS HAVE NO VERSION. A request that promises a retry
  ;; must name one for every block it commits; the refusal says which is
  ;; missing rather than only that something is.
  (define (versions-missing ids pairs)
    (filter (lambda (id) (not (assoc id pairs))) ids))

  ;; NEVER: EVERY REFUSAL HERE IS A PREMISE, AND PREMISES COME SECOND.
  ;;
  ;; §7.5.4 fixes the order: request identity, then premises, then
  ;; execution. These three -- the draft has gone, the draft moved under
  ;; the version the client named, the block moved under its baseline --
  ;; are all statements about the store as it is NOW, and a retry of a
  ;; request that already succeeded must be answered about its identity
  ;; instead. Measured, twice: answering `no-draft` first told a retry
  ;; about the drafts its own first attempt had retired, and answering
  ;; `working-version-changed` first told a retry about the new draft the
  ;; same person had written since.
  (define (preflight state entries missing ids selected)
    (let ((bad (filter values
                 (map (lambda (e) (baseline-refusal state (list-ref e 3) (list-ref e 5) (list-ref e 6))) entries))))
      (cond ((pair? missing) (list 'error 'no-draft (cons 'blocks missing)))
            ;; A DRAFT WOULD LAND AS A SRC BESIDE A DATUM BLOCK'S BODY, which
            ;; nothing that runs the block reads (store.sc): the first such
            ;; draft, in the order read, is named and nothing is written.
            ((find (lambda (e) (datum-mode-block? state (list-ref e 3))) entries)
             => (lambda (e) (draft-on-datum-refusal (list-ref e 3))))
            ;; EVERY NAMED VERSION MUST BE THE ONE ON DISK. A draft the
            ;; client did not name the current version of has moved
            ;; under them since they read it.
            ((and (pair? selected) (pair? entries)
                  (pair? (filter (lambda (e)
                                   (let ((p (assoc (list-ref e 3) selected)))
                                     (and p (not (equal? (cdr p) (list-ref e 4))))))
                                 entries)))
             (list 'error 'working-version-changed
                   (cons 'blocks (map (lambda (e) (list-ref e 3))
                                      (filter (lambda (e)
                                                (let ((p (assoc (list-ref e 3) selected)))
                                                  (and p (not (equal? (cdr p) (list-ref e 4))))))
                                              entries)))))
            (else (baseline-combine bad)))))
  ;; A MARKED SRC IS COMMITTED AS ITS BYTES, mark included, as import-code
  ;; stores a marked file; every other src as its text. src-text (markers.sc)
  ;; answers a marked src's text with U+FEFF in front, and only then.
  (define (entry-intent e)
    (let* ((bytes (list-ref e 7)) (text (src-text bytes)))
      (unless text
        (assertion-violation 'commit "The src field requires valid UTF-8" (list-ref e 3)))
      (list 'set (list-ref e 3) 'src
            (if (and (> (string-length text) 0) (char=? (string-ref text 0) (integer->char #xFEFF))) bytes text))))
  ;; NEVER: ONLY THE DRAFTS THIS REQUEST CONSUMED.
  ;;
  ;; The entries were read when the request was assembled, and on a
  ;; COMPLETION -- a retry finishing a plan a dead process had written --
  ;; what is in the slot now is a LATER draft the same person wrote in
  ;; the meantime. Retiring it would throw away work nobody committed.
  ;; Measured as W4": the completion carried out the plan's frozen text
  ;; correctly and then deleted the draft that was standing beside it.
  ;;
  ;; The versions the request named are the test, and the slot's own
  ;; envelope is checked as well: an envelope replaced between the read
  ;; and here is not the one that was consumed either.
  ;; ---- who moved under this commit -----------------------------------------
  ;;
  ;; A DRAFT IS WRITTEN AGAINST A BASELINE, and by the time its commit
  ;; lands other writers may have committed. The person who just
  ;; committed is the one who most wants to know, because they are about
  ;; to decide whether to test again -- and the fact is free here: both
  ;; cuts are already in hand, so this reads no file and takes no lock.
  ;;
  ;; NEVER: IT IS INFORMATION, NOT A VERDICT (§7.5.22, v161). It does not
  ;; refuse, does not hold the commit back, and triggers nothing. The
  ;; earlier proposals -- refuse when behind, dry-run before committing
  ;; -- were both ruled out.
  ;;
  ;; NOTE: AND THIS WRITER IS NOT AMONG THE NAMES. Its own later records
  ;; are its own work, not somebody moving underneath it; a commit that
  ;; told you that you are behind yourself would be noise in the one
  ;; place the answer is read. The case is reachable -- commit another
  ;; block between writing this draft and committing it -- so it is a
  ;; decision rather than an accident, and a row pins it.
  ;;
  ;; NOTE: ABSENT, NOT EMPTY, when nothing moved: `(behind ())` would be a
  ;; field that is always there, and a field that is always there says
  ;; nothing.
  ;; NEVER: NOTHING HERE MAY TURN A COMMIT THAT SUCCEEDED INTO A FAILURE.
  ;; `problem` wraps the whole verb and renders ANY raise as
  ;; `(error working-unavailable ...)`, so a cut this code cannot read --
  ;; a replacement envelope can carry `(bogus)` past `entry?`'s
  ;; list-only check and `cut-join` raises on its `(car e)` -- would
  ;; answer a failure for a commit whose records are already durable.
  ;; An informational field is not worth that, so every failure here is
  ;; the field's absence.
  ;;
  ;; NOTE: THE CUT IS PASSED IN, NOT TAKEN FROM A REDUCTION THIS FUNCTION
  ;; CHOOSES. Which reduction it is decides whether the answer is right,
  ;; and the two available ones differ; the caller is where that is
  ;; visible, so it is decided there.
  ;;
  ;; NOTE: AND IT IS NOT A COVERAGE TEST. A writer whose entry in the cut
  ;; is EARLIER than the baseline's -- reachable after a retraction
  ;; rebuilds the applied cut -- is not named, because it did not move
  ;; after the baseline; it moved back. The field answers "who landed
  ;; after you", not "is your baseline still comparable".
  ;;
  ;; NEVER: HOW FAR, IN RECORDS, IS SAID BY THE CORE, ONCE. `behind` gives each
  ;; moved writer's CURRENT seq, which is not a count: a client that showed
  ;; it as one reported 10 records where 2 had landed. The baseline is the
  ;; join of the consumed drafts' cuts, which only the core holds, so the
  ;; subtraction is here: `(behind-records ((<writer> . <n>) ...))`, n the
  ;; writer's current seq minus the seq the baseline holds for it (0 when it
  ;; holds none), beside `behind` and over the same writers. `behind` is as
  ;; it was, so a client that reads only it is unaffected.
  ;;
  ;; -> the clauses (behind ...) and (behind-records ...), or () when
  ;; nothing moved.
  (define (behind-clauses cut writer entries)
    (guard (e (#t '()))
      (let* ((baseline (fold-left (lambda (acc e) (cut-join acc (list-ref e 6)))
                                  '() entries))
             (moved (list-sort (lambda (a b) (string<? (car a) (car b)))
                               (filter (lambda (p)
                                         (and (not (string=? (car p) writer))
                                              (let ((mine (assoc (car p) baseline)))
                                                (or (not mine) (< (cdr mine) (cdr p))))))
                                       cut))))
        (if (null? moved)
            '()
            (list (list 'behind moved)
                  (list 'behind-records
                        (map (lambda (p)
                               (let ((mine (assoc (car p) baseline)))
                                 (cons (car p) (- (cdr p) (if mine (cdr mine) 0)))))
                             moved)))))))

  (define (retire! store writer entries versions landed?)
    ;; KEY: A PLACE TO STOP IN THE GAP THE COMPARISON BELOW IS FOR. The
    ;; envelopes were read at the top of the commit, and the store's
    ;; write session is already released by the time this runs -- so
    ;; another process belonging to the same writer can put a NEW draft
    ;; in the slot before the lock is taken here. That is the case the
    ;; `equal?` below refuses to delete.
    ;;
    ;; `retire-locked` cannot arm it: it fires after the comparison has
    ;; agreed, with the lock already held, so nothing can get in. Armed
    ;; from here, a build that deletes whatever is in the slot and a
    ;; build that compares first stop being indistinguishable.
    ;;
    ;; NEVER: A COMMIT THAT LANDED KEEPS ITS ANSWER (U10; found in review).
    ;; When something landed -- records, or an empty plan -- a cleanup that
    ;; fails does not replace the answer: this returns #f or the first
    ;; failure as (cleanup-failed (path p) (reason r)), and the commit adds
    ;; it. When nothing landed, retiring runs as on the base: a lock that
    ;; cannot be taken is the answer, and a draft that cannot be retired is
    ;; not reported.
    (barrier! 'before-retire)
    (if landed?
        (guard (failure (#t (cleanup-failed (lock-path store writer) failure)))
          (with-draft-lock store writer
            (lambda ()
              (let loop ((es entries) (first #f))
                (if (null? es)
                    first
                    (let ((failed (guard (failure (#t (cleanup-failed
                                                        (path-for store writer (list-ref (car es) 3))
                                                        failure)))
                                    (retire-one! store writer (car es) versions))))
                      (loop (cdr es) (or first failed))))))))
        (with-draft-lock store writer
          (lambda ()
            (for-each (lambda (e) (guard (failure (#t #f)) (retire-one! store writer e versions)))
                      entries)
            #f))))

  ;; -> #f, or (cleanup-failed ...) for a draft still there after its unlink.
  (define (retire-one! store writer e versions)
    (let ((p (path-for store writer (list-ref e 3))))
      (if (and (member (cons (list-ref e 3) (list-ref e 4)) versions)
               (equal? e (entry-at store writer (list-ref e 3))))
          (begin
            ;; A STEP NOTHING CAN ARM READS LIKE A STEP THAT PASSED. The whole
            ;; point of the lock is that a concurrent `write` WAITS here;
            ;; without a place to stop, a lockless build and this one are
            ;; indistinguishable from outside.
            (barrier! 'retire-locked)
            (if (present? p)
                (begin
                  (unlink! p)
                  ;; NEVER: ASKED AGAIN, NOT ASSUMED. unlink! was Chez's
                  ;; delete-file, which answered #f and raised nothing when
                  ;; it could not delete. Through the door it is unlink(2)
                  ;; and a failure raises durable-error, which retire!'s
                  ;; guard turns into cleanup-failed; this second question
                  ;; stays for a draft that is still there after an unlink
                  ;; that reported success.
                  (if (present? p)
                      (list 'cleanup-failed (list 'path p)
                            (list 'reason "the draft file is still there after it was unlinked"))
                      (begin (directory-entry-durable! p 'working) #f)))
                #f))
          #f)))

  (define (cleanup-failed path failure)
    (list 'cleanup-failed
          (list 'path (if (unreadable-entry? failure) (unreadable-entry-path failure) path))
          (list 'reason
                (cond ((unreadable-entry? failure) (unreadable-entry-reason failure))
                      ((fs-error? failure)
                       (let ((op (fs-error-op failure)) (n (fs-error-errno failure)))
                         (string-append (cond ((symbol? op) (symbol->string op))
                                              ((string? op) op)
                                              (else "an operation"))
                                        " failed, errno "
                                        (if (number? n) (number->string n) "unknown"))))
                      ((and (condition? failure) (message-condition? failure))
                       (condition-message failure))
                      (else "the cleanup failed")))))

  ;; An answer with the cleanup clause, when there is one, at its end.
  (define (with-cleanup answer failed)
    (if (and failed (pair? answer) (list? answer)) (append answer (list failed)) answer))

  ;; THE PREMISES OF A COMMIT, read once (store.sc, premises-preflight): ->
  ;; (check finish run), or the refusal of a set the store cannot check,
  ;; which is raised as an answer and returned as one.
  (define (commit-premises store text)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
      (call-with-values (lambda () (premises-preflight store text #f)) list)))

  ;; THE PATH THAT WRITES NOTHING ENTERS AND ASKS the premises itself: it is
  ;; the one place besides a write session where the check is called.
  (define (premises-asked gate state)
    (cond ((premises-gate? gate)
           ((premises-gate-enter gate))
           ((premises-gate-check gate) state))
          (gate (gate state))
          (else #f)))

  (define (working-commit! store supplied ids actor supplied-req . rest)
    (needing-writer supplied (lambda ()
    (problem (lambda ()
      ;; THE VERSIONS ARRIVE AS ONE LIST, and a rest argument wraps it
      ;; in another. Reading the wrapper as the list made every value a
      ;; list of strings, and the first refusal was a type error from
      ;; deep inside the parser rather than the answer this rule owes.
      ;; THE PREMISES THE COMMIT IS ACCEPTED ON (store.sc, premises-preflight),
      ;; compiled once. A set the store cannot check was refused at the
      ;; dispatcher's entry (rpc.sc, premises-refusal), before any of this;
      ;; called directly, the refusal is still the answer.
      ;; PREMISE-CHECK is asked before the commit's own check on both paths
      ;; below; FINISH is given each answer those paths make, and RUN holds
      ;; them, so a raise after either was entered says so.
      (let ((premises-given (commit-premises store (and (pair? rest) (pair? (cdr rest)) (cadr rest)))))
       (if (eq? (car premises-given) 'error)
           premises-given
      (let* ((selected-version (if (pair? rest) (or (car rest) '()) '()))
             (writer (requested-writer store supplied)) (state (open-and-reduce store))
             (premise-check (car premises-given))
             (finish (cadr premises-given))
             (run (caddr premises-given)))
       (run (lambda ()
        (cond
          ((not writer) (invalid-writer))
          ((exists provisional-id? ids) '(error bad-request provisional-target))
          ((not (for-all safe-id? ids)) '(error bad-request invalid-block-id))
          ((not (= (length ids) (length (list-sort string<? (let loop ((xs ids) (out '()))
                                      (if (null? xs) out (loop (cdr xs) (if (member (car xs) out) out (cons (car xs) out)))))))))
           '(error bad-request duplicate-block))
          (else
           ;; NEVER: NO PACKET. A commit used to write an immutable file
           ;; beside the drafts, holding the request it had accepted and
           ;; the exact envelopes it had read, so that a retry could be
           ;; answered from it. The log now holds both: the plan record
           ;; names the versions this request consumed, and the plan's
           ;; own payload freezes the text. A second store that has to
           ;; be kept in step with the first is a second supplier of the
           ;; same fact, and the two disagree exactly when one of the
           ;; two writes fails.
           (let* ((after (or (assoc (writer-for store #f) (reduce-applied-cut state))
                             (cons (writer-for store #f) 0)))
                  (external (or supplied-req (make-write-request actor 'commit ids (fresh-id) after))))
             ;; NEVER: A DRAFT THAT CANNOT BE READ IS A PREMISE, NOT AN
             ;; IDENTITY.
             ;;
             ;; A corrupt envelope, or one whose bytes are not text,
             ;; used to raise out of here and become the answer -- before
             ;; the request had been judged at all. A retry whose own
             ;; first attempt retired its drafts, and whose slot now
             ;; holds a damaged replacement, was told about the
             ;; replacement instead of being told it had already
             ;; succeeded.
             ;;
             ;; The failure is CAUGHT and carried to `preflight`, which
             ;; runs after the verdict. On a replay or a completion it is
             ;; never reached, which is the point: neither of those needs
             ;; a draft.
             ;; THE SELECTION IS DECIDED BEFORE ANY DRAFT IS OPENED.
             ;; It is what the caller named -- positionally, or by the
             ;; versions it gave -- and never what happens to be on disk.
             (let* ((named (and (pair? selected-version)
                                (let ((parsed (map (lambda (spec)
                                                     (parse-version-spec ids spec))
                                                   selected-version)))
                                  (and (for-all values parsed)
                                       (no-duplicate-blocks? parsed)
                                       parsed))))
                    (version-shape-ok (or (null? selected-version) (and named #t)))
                    ;; THE STRUCTURAL DRAFTS THIS REQUEST NAMES: ((new|delete <key>
                    ;; <version>) ...), or #f for a malformed one. A structural
                    ;; draft is committed only when named.
                    (structural-named (parse-structural (if (> (length rest) 2) (or (list-ref rest 2) '()) '())
                                                        (if (> (length rest) 3) (or (list-ref rest 3) '()) '())
                                                        ids))
                    (selection (cond ((pair? ids) ids)
                                     (named (map car named))
                                     (else '())))
                    (read-failure #f)
                    ;; A DRAFT THIS PROCESS COULD NOT READ, as opposed to one
                    ;; that read and is damaged (U10): without --req the
                    ;; first is answered before `no-draft`, because the draft
                    ;; may be there. A damaged draft keeps its answers.
                    (unreadable-failure #f)
                    (entries
                      (guard (e ((unreadable-entry? e)
                                 (set! read-failure (unreadable-answer e))
                                 (set! unreadable-failure #t)
                                 '())
                                (#t (set! read-failure
                                          (if (and (pair? e) (eq? 'working-error (car e)))
                                              (list 'error 'working-unavailable (list 'reason (cadr e)))
                                              (list 'error 'working-unavailable
                                                    (list 'message
                                                          (if (message-condition? e)
                                                              (condition-message e)
                                                              "A draft could not be read")))))
                                    '()))
                        (let ((all (active-entries store writer state)))
                          (cond ((pair? ids) (filter (lambda (e) (member (list-ref e 3) ids)) all))
                                ;; NAMING ONLY STRUCTURAL DRAFTS SELECTS NO BLOCK DRAFT
                                ((and (null? selected-version) (pair? structural-named)) '())
                                (else all)))))
                    ;; ((<named> . <entry or #f>) ...), each named structural draft
                    ;; and its live envelope
                    (structural
                      (if (list? structural-named)
                          (map (lambda (n)
                                 (cons n (let* ((r (deferred-read (lambda () (structural-at store writer (structural-file-key n)))))
                                                (x (car r)))
                                           (when (cadr r)
                                             (unless read-failure (set! read-failure (cadr r)))
                                             (when (caddr r) (set! unreadable-failure #t)))
                                           (and x (not (consumed? store state x)) x))))
                               structural-named)
                          '()))
                    (structural-entries (filter values (map cdr structural)))
                    (missing (append (filter (lambda (id) (not (exists (lambda (e) (equal? id (list-ref e 3))) entries))) selection)
                                     (map (lambda (p) (cadr (car p))) (filter (lambda (p) (not (cdr p))) structural))))
                    ;; KEY: AN UNCHANGED DRAFT HAS NO SUB-OPERATION.
                    ;;
                    ;; Its bytes are what the block already says, so a
                    ;; `set` carrying them is a record that changes
                    ;; nothing -- and §7.5.11 says what to write instead:
                    ;; nothing at all without `--req`, an empty plan with
                    ;; one. It is still CONSUMED, and still named in
                    ;; `consumes`: the draft is being retired, and a
                    ;; retry has to be able to say which one.
                    ;;
                    ;; NOTE: THE TEST IS THE SAME ONE `drafts` USES, and it
                    ;; is `fresh` AND equal -- a stale draft is not
                    ;; unchanged whatever its bytes equal, because the
                    ;; text it would be compared against is not the one
                    ;; it was written on.
                    (changed
                      (filter (lambda (e)
                                (not (unchanged? state e
                                                  (equal? (list-ref e 5)
                                                          (block-hash state (list-ref e 3))))))
                              entries))
                    (intents
                      (guard (e (#t (unless read-failure
                                      (set! read-failure
                                            (list 'error 'working-unavailable
                                                  (list 'message
                                                        (if (message-condition? e)
                                                            (condition-message e)
                                                            "A draft could not be read")))))
                                    '()))
                        (map entry-intent changed)))
                    ;; KEY: THE IDENTITY IS TAKEN OVER THE VERSIONS, NOT
                    ;; OVER THE DRAFTS' BYTES.
                    ;;
                    ;; It used to be the encoded envelopes. A retry
                    ;; arrives after those envelopes have been retired
                    ;; and, often, after the same person has written a
                    ;; new draft into the same slot -- so the bytes
                    ;; differ, the fingerprint differs with them, and a
                    ;; retry of a request that succeeded was answered
                    ;; `req-mismatch`. Measured, as W4' in
                    ;; `plan-completion.sc`.
                    ;;
                    ;; §7.5.9 puts who, the verb, the draft writer, the
                    ;; ORDERED (block . version) list and `after` in the
                    ;; fingerprint. Every one of those is something the
                    ;; client holds and can send again, which is the
                    ;; property the packet used to provide by storing
                    ;; them for it.
                    ;;
                    ;; SORTED BY BLOCK, because the blocks a commit names
                    ;; are a SET: two orderings of the same set are the
                    ;; same request, and a client that lists them the
                    ;; other way round on a retry is retrying, not
                    ;; sending something new.
                    ;; NEVER: AND `pairs` HAS TO BE A LIST BEFORE ANYTHING
                    ;; MAPS OVER IT. A duplicated or malformed
                    ;; `--working-version` leaves it #f, and building the
                    ;; effective request maps over it -- so the caller
                    ;; got a storage error from inside the request
                    ;; machinery instead of the bad-request this rule
                    ;; owes them. Measured.
                    (given (or named
                               (and (null? selected-version)
                                    (map (lambda (e) (cons (list-ref e 3) (list-ref e 4)))
                                         entries))))
                    (pairs (and given
                                (list-sort (lambda (a b) (string<? (car a) (car b))) given)))
                    ;; NEVER: THE CALLER'S ARGUMENT STRINGS ARE NOT IN IT.
                    ;;
                    ;; §7.5.9 fixes the fingerprint's inputs: who, the
                    ;; verb, the draft writer, the ordered (block .
                    ;; version) list, and `after`. Appending the sorted
                    ;; pairs to the raw arguments left the raw ones in --
                    ;; so `commit A B` and `commit B A`, the same request
                    ;; over the same set, fingerprinted differently, and
                    ;; a retry that listed them the other way round was a
                    ;; `req-mismatch`. Sorting the suffix does not
                    ;; canonicalise a list that still carries the
                    ;; original order in front of it.
                    (effective (and version-shape-ok pairs (list? structural-named)
                                    (make-write-request actor 'commit
                                      (cons writer
                                            (map (lambda (p) (string-append (car p) "\x0;" (cdr p)))
                                                 (list-sort (lambda (a b) (string<? (car a) (car b)))
                                                            (append pairs (map structural-pair structural-named)))))
                                      (list-ref external 4) (list-ref external 5)))))
                  (cond
                    ;; NEVER: NO `no-draft` HERE. §7.5.4 fixes the order:
                    ;; request identity first, premises second. A retry
                    ;; whose drafts were retired by the commit it is
                    ;; retrying must be told about its identity -- replay
                    ;; or mismatch -- and not that there is no draft.
                    ;; The check moved into the preflight, which runs
                    ;; after the verdict.
                    ;; KEY: A REQUEST THAT WILL CONSUME SOMETHING MUST SAY
                    ;; WHAT. Its identity is taken over the versions, and
                    ;; a retry from a new process has no draft left to
                    ;; read them from -- "infer once and reuse" has
                    ;; nowhere to keep the inference.
                    ;;
                    ;; NEVER: NOT FOR A COMMIT THAT CONSUMES NOTHING. Zero
                    ;; drafts is a legitimate request: it writes an empty
                    ;; plan, which is that request's whole durable
                    ;; evidence, and there is no version to name.
                    ;; Measured -- requiring one unconditionally refused
                    ;; every row in `empty-plan.sc` with `bad-request`
                    ;; where the answer should have been about identity.
                    ((not pairs) '(error bad-request malformed-working-version))
                    ((not (list? structural-named)) '(error bad-request malformed-working-version))
                    ;; KEY: THE VERSIONS NAME THE SELECTION, AND THE
                    ;; SELECTION IS THE IDENTITY.
                    ;;
                    ;; The fingerprint is taken over the pairs, so two
                    ;; requests with the same pairs are the same request.
                    ;; If the blocks a caller lists POSITIONALLY could
                    ;; differ from the blocks it named versions for, then
                    ;; `commit A --working-version A=.. B=..` and
                    ;; `commit B --working-version A=.. B=..` share an
                    ;; identity while selecting different drafts -- and
                    ;; the second, completing the first's plan, deleted a
                    ;; draft that plan never consumed. Measured.
                    ;; TWO WAYS THE TWO LISTS CAN DISAGREE, AND THEY
                    ;; DESERVE DIFFERENT WORDS: a block with no version
                    ;; is a version that is MISSING, and the refusal
                    ;; names it; a version for a block this request did
                    ;; not select is a version that does not belong here.
                    ((and (pair? selected-version) (pair? ids)
                          (pair? (versions-missing ids pairs)))
                     (list 'error 'bad-request 'req-needs-versions
                           (cons 'blocks (versions-missing ids pairs))))
                    ((and (pair? selected-version) (pair? ids)
                          (not (equal? (list-sort string<? ids)
                                       (list-sort string<? (map car pairs)))))
                     (list 'error 'bad-request 'working-version-mismatch
                           (cons 'blocks (filter (lambda (b) (not (member b ids)))
                                                 (map car pairs)))))
                    ;; NOTE: THE BLOCK SET OF A REQUEST THAT NAMES VERSIONS
                    ;; IS THE SET IT NAMED. Deriving it from the drafts
                    ;; on disk made a NEW draft, written after the first
                    ;; attempt, turn a retry into `req-needs-versions` --
                    ;; a refusal about the store's present state, handed
                    ;; to a request whose identity was never judged.
                    ((and supplied-req (null? selected-version)
                          (or (pair? ids) (pair? entries)))
                     (list 'error 'bad-request 'req-needs-versions
                           (cons 'blocks (if (null? ids)
                                             (map (lambda (e) (list-ref e 3)) entries)
                                             ids))))
                    ((and supplied-req (pair? ids) (pair? (versions-missing ids pairs)))
                     (list 'error 'bad-request 'req-needs-versions
                           (cons 'blocks (versions-missing ids pairs))))
                    ((and (not supplied-req) unreadable-failure) read-failure)
                    ((and (not supplied-req) (pair? missing))
                     (list 'error 'no-draft (cons 'blocks missing)))
                    ;; NOTHING TO DO AND NO IDENTITY TO REMEMBER IT BY.
                    ;; With `--req` this is a request like any other and
                    ;; writes its empty plan, which is its whole durable
                    ;; evidence; without one there is nothing to record.
                    ;; NEVER: NOT WHEN A DRAFT COULD NOT BE READ. "No
                    ;; sub-operations" and "the draft is damaged" are
                    ;; different answers, and this arm used to give the
                    ;; first for the second -- a non-text draft
                    ;; committed as a no-op and was retired.
                    ;; NOTHING TO WRITE, AND STILL SOMETHING TO CHECK.
                    ;;
                    ;; A commit with no request id and no sub-operations
                    ;; writes no record -- but "nothing to write" is not
                    ;; "nothing to check": the versions the caller named
                    ;; must still be the ones on disk, and the baselines
                    ;; must still be current. This arm answered
                    ;; `(ok (items))` without either, and retired the
                    ;; drafts anyway.
                    ;;
                    ;; NEVER: THE CHECK IS THE SAME FUNCTION, not a copy of
                    ;; its rules: `preflight` is asked here exactly as
                    ;; the write path asks it.
                    ;; THE PREMISES ARE ASKED HERE TOO (on the state this verb
                    ;; read, not under the store's lock: nothing is written on
                    ;; this path, so there is no later state to protect), and
                    ;; a refusal retires nothing.
                    ((and (not supplied-req) (null? intents) (null? structural) (not read-failure)
                          (or (premises-asked premise-check state)
                              (preflight state entries missing ids pairs)))
                     => (lambda (refusal) (finish refusal)))
                    ((and (not supplied-req) (null? intents) (null? structural) (not read-failure))
                     ;; NOTE: THIS ARM ANSWERS THE SAME QUESTION AND USED TO
                     ;; SKIP IT. Nothing is written here -- the request has
                     ;; no identity to record and no sub-operations -- but
                     ;; the drafts are still retired, so it IS a commit and
                     ;; the person is owed the same fact. Nothing was
                     ;; appended, so the cut this verb opened with is the
                     ;; current one.
                     (let* ((failed (retire! store writer entries pairs #f))
                            (behind (behind-clauses (reduce-applied-cut state) writer entries)))
                       (finish (with-cleanup (append (list 'ok '(items)) behind) failed))))
                    ((and (not supplied-req) read-failure) read-failure)
                    (else
                     ;; WHAT THIS COMMIT CONSUMES, SAID IN THE RECORD.
                     ;; Each item carries the version and the two other
                     ;; inputs the version is computed from, so a reader
                     ;; with the record alone can both check the name and
                     ;; rebuild the draft.
                     (finish
                     (let* ((live #f)
                            ;; ((<provisional id> . <index of its holder's insert>) ...)
                            (holders '())
                            (answers (with-store-write store
                                       (lambda (current view)
                                         (set! live current)
                                         (let-values (((more at) (structural-intents current (length intents) structural-entries)))
                                           (set! holders at)
                                           (append intents more)))
                                      actor effective
                                      (premises-also premise-check
                                        (lambda (current)
                                          (or read-failure (preflight current entries missing ids pairs)
                                              (structural-preflight current structural))))
                                      #t
                                      (and (or (pair? entries) (pair? structural-entries))
                                           (list 'consumes writer
                                                 (append
                                                   (map (lambda (e)
                                                          (list (list-ref e 3) (list-ref e 4)
                                                                (list-ref e 5) (list-ref e 6)))
                                                        entries)
                                                   (map structural-consumes-item structural-entries)))))))
                       (if (for-all (lambda (a) (and (pair? a) (eq? (car a) 'ok))) answers)
                           (begin
                             ;; NEVER: A REPLAY RETIRES NOTHING. The drafts
                             ;; this request consumed were retired by the
                             ;; execution it is a replay of; what is in
                             ;; the slot now is a LATER draft, written by
                             ;; the same person after that commit, and
                             ;; retiring it would throw away work nobody
                             ;; committed. Measured: W4' read back
                             ;; "v1 text" from a draft that said
                             ;; "v2 text" until the retry arrived.
                             (let ((replay? (exists (lambda (a) (equal? '(replay #t) (assq 'replay (cdr a))))
                                                    answers)))
                               ;; A DRAFT THAT COULD NOT BE READ COULD NOT BE RETIRED
                               ;; (found in review). Its read failure emptied `entries`,
                               ;; so retire! has nothing to report; when the commit
                               ;; landed -- a completion bypasses the preflight -- that
                               ;; failure is the cleanup clause, with its path and reason.
                               (let ((failed (and (not replay?)
                                                  (or (retire! store writer entries pairs #t)
                                                      (retire-structural! store writer structural-entries)
                                                      (and unreadable-failure
                                                           (cons 'cleanup-failed (cddr read-failure)))))))
                               ;; KEY: THE CUT IS THE ONE THE WRITE PRODUCED, not the
                               ;; one this verb opened with. They differ, and the
                               ;; difference is the whole answer: appending this
                               ;; request's records can RELEASE a foreign record
                               ;; that was waiting on them, and that writer has
                               ;; moved. Read from the outer state it is omitted,
                               ;; and the field is then wrong rather than stale.
                               ;; `with-store-write` builds its own reduction and
                               ;; hands it to the thunk above -- the same object it
                               ;; folds into -- so `live` is that cut for free.
                               ;;
                               ;; NEVER: AND A REPLAY GETS NO FIELD. Its drafts were
                               ;; retired by the execution it repeats, so `entries`
                               ;; is empty or holds a LATER draft: an empty
                               ;; baseline names every writer in the store, and a
                               ;; later one hides the movement that did happen.
                               ;; Neither is "who moved under this commit", and the
                               ;; commit it repeats already answered that question.
                               (let ((behind (if (and (not replay?) live)
                                                 (behind-clauses (reduce-applied-cut live)
                                                                 writer entries)
                                                 '())))
                                 (with-cleanup
                                   (append (list 'ok (cons 'items answers)) behind
                                           (if replay?
                                               (replayed-created-clause
                                                 state answers
                                                 (map cadr (filter (lambda (n) (eq? (car n) 'new))
                                                                   (if (list? structural-named) structural-named '()))))
                                               (created-clause holders answers)))
                                   failed)))))
                           (if (= (length answers) 1) (car answers) (batch-answer answers)))))))))))))))))))))
)
