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
          (only (chezscheme) format getenv current-directory iota list-head)
          (only (theourgia rpc) rpc-dispatch-parsed)
          (only (theourgia arguments) parse-arguments argument-option argument-option-list)
          (only (theourgia client) socket-path call! request-frame answer-field readable-shape? plain-datum)
          (only (theourgia ffi) entry-type read-entry)
          (only (theourgia languages) language-for-path)
          (only (theourgia code-project) lang-fields code-children)
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
  ;; A TEXT-MODE CODE BLOCK'S SRC IS STORED AS BYTES, and a daemon prints it
  ;; as the string those bytes encode when they are text (render.sc). A copy
  ;; read through a daemon therefore has the string; its bytes are the
  ;; string's UTF-8, exactly, and export-code requires them as bytes. A src
  ;; stored as a string -- an insert can leave one; `set` stores a string as
  ;; its bytes -- is copied as its bytes too: the same text, in the form
  ;; `set` stores and export-code reads. FS is the record's fields, which
  ;; say whether the block is a text-mode one.
  (define (stored-src f fs)
    (if (and (eq? (car f) 'src) (string? (cdr f))
             (eq? (field fs 'kind) 'code) (eq? (field fs 'mode) 'text))
        (cons 'src (string->utf8 (cdr f)))
        f))

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
  ;; A SCOPE OF A FROZEN CANDIDATE (`--freeze <dir>`, a landing freeze): the
  ;; cut is the freeze's pinned cut, a copy with a draft holds the draft's
  ;; bytes, the committed src is kept beside it in a `before` section, and no
  ;; main letter is written (collect-into --letter new makes it, after the
  ;; landing). `--packet <file>` writes sections under the letter copy.
  ;; `--roots all` names every top-level block of the store.
  (define (scope-verb store actor args options cwd . socket-given)
    (let ((socket (and (pair? socket-given) (car socket-given)))
          (cut (argument-option options "--cut"))
          (roots (argument-option-list options "--roots"))
          (for (argument-option options "--for"))
          (freeze (argument-option options "--freeze"))
          (packet (argument-option options "--packet")))
      (cond
        ((not (and (= 1 (length args)) (or (string? cut) (string? freeze)) (pair? roots) (string? for)))
         'usage)
        (else
         (let ((dir (absolute (car args) cwd)))
;; Through the store's one door to the filesystem: an entry that
           ;; cannot be read raises, rather than reading as absent.
           (if (not (eq? (entry-type dir) 'absent))
               (list 'error 'scope-exists (list 'dir dir))
               (let ((fz (and freeze (read-freeze (absolute freeze cwd))))
                     (pk (if packet (read-packet (absolute packet cwd)) '())))
                 (cond
                   ((and fz (pair? fz)) fz)
                   ((and (pair? pk) (eq? (car pk) 'error)) pk)
                   ;; compared as text: a --cut given with a freeze is the
                   ;; pinned cut's literal, and anything else is refused
                   ((and fz (string? cut) (not (equal? cut (written (freeze-cut fz)))))
                    (list 'error 'bad-request '(reason freeze-cut-mismatch) (list 'cut cut) (list 'pinned-cut (freeze-cut fz))))
                   (else
                    (let ((all (if (equal? roots '("all"))
                                   (top-level-ids store socket actor (if fz (written (freeze-cut fz)) cut) fz)
                                   roots)))
                      (if (and (pair? all) (eq? (car all) 'error))
                          all
                          (scope-read store socket actor dir (if fz (written (freeze-cut fz)) cut) all for fz pk))))))))))))

  ;; ---- the freeze, the packet, the whole tree -------------------------------
  ;;
  ;; THE FREEZE IS READ ONCE: DRAFTS.sexp's pinned cut, each draft's bytes by
  ;; block, the files the freeze adds (`(new (provisional <id>) (path <p>)
  ;; ... (bytes <n>.bytes) ...)`: the whole file) and the holders it deletes
  ;; (`(delete (holder <id>) ...)`), as tools/drafts-dump.sc writes them. A
  ;; freeze that cannot be read is refused before anything is written.
  ;; -> #(pinned-cut drafts news deletes dir), or the refusal
  (define (freeze-cut fz) (vector-ref fz 0))
  (define (freeze-drafts fz) (vector-ref fz 1))
  (define (freeze-news fz) (vector-ref fz 2))
  (define (freeze-deletes fz) (vector-ref fz 3))
  (define (freeze-dir fz) (vector-ref fz 4))
  (define (entry-part x key) (let ((p (and (list? x) (assq key (cdr x))))) (and p (pair? (cdr p)) (cadr p))))
  ;; THROUGH THE STORE'S ONE DOOR TO THE FILESYSTEM: an entry that cannot be
  ;; read raises unreadable-entry, and one that is not there is `absent`.
  ;; -> the bytes, or #f
  (define (file-bytes path) (let ((b (read-entry path))) (and (bytevector? b) b)))
  ;; -> the file's one plain datum, or #f when it is absent or holds no one datum
  (define (file-datum path)
    (let* ((b (file-bytes path)) (d (and b (plain-datum (utf8->string b)))))
      (and b (not (eof-object? d)) d)))
  (define (freeze-refusal fdir) (list 'error 'bad-request '(reason freeze-unreadable) (list 'dir fdir)))
  (define (read-freeze fdir)
    (let* ((d (file-datum (string-append fdir "/DRAFTS.sexp")))
           (entries (and (list? d) (pair? d) (eq? (car d) 'drafts) (for-all pair? (cdr d)) (cdr d)))
           (cut (and entries (entry-part d 'pinned-cut)))
           (bytes-of (lambda (x) (let ((n (entry-part x 'bytes))) (and (string? n) (file-bytes (string-append fdir "/" n))))))
           (of (lambda (kind) (if entries (filter (lambda (x) (eq? (car x) kind)) entries) '())))
           (drafts (map (lambda (x) (cons (entry-part x 'block) (bytes-of x))) (of 'draft)))
           (news (map (lambda (x) (list (entry-part x 'provisional) (entry-part x 'path) (bytes-of x))) (of 'new)))
           (deletes (map (lambda (x) (entry-part x 'holder)) (of 'delete))))
      (if (and (list? cut)
               (for-all (lambda (x) (and (string? (car x)) (bytevector? (cdr x)))) drafts)
               (for-all (lambda (x) (and (string? (car x)) (string? (cadr x)) (bytevector? (caddr x)))) news)
               (for-all string? deletes))
          (vector cut drafts news deletes fdir)
          (freeze-refusal fdir))))
  ;; THE PACKET: (packet <section> ...), a section (section (title <t>) (text
  ;; <x>) [(children <section> ...)]); sections, since a doc may not be nested.
  ;; -> the sections, or the refusal
  (define (read-packet path)
    (let ((d (file-datum path)))
      (define (section? x)
        (and (list? x) (pair? x) (eq? (car x) 'section) (for-all pair? (cdr x)) (string? (entry-part x 'title))
             (let ((t (entry-part x 'text))) (or (not t) (string? t)))
             (let ((c (assq 'children (cdr x)))) (or (not c) (and (list? c) (for-all section? (cdr c)))))))
      (if (and (list? d) (pair? d) (eq? (car d) 'packet) (for-all section? (cdr d)))
          (cdr d)
          (list 'error 'bad-request '(reason packet-malformed) (list 'file path)))))
  ;; THE WHOLE TREE: every block whose parent is the root, as `under` answers
  ;; it, and every file a freeze adds. A query answers at the store's cut,
  ;; and that must be the cut the scope reads at, compared as text: a store
  ;; that has moved since (or a cut named by a tag) is refused rather than
  ;; given another cut's tree.
  (define (top-level-ids store socket actor cut fz)
    (let* ((a (routed store 'query (list "(under ?b \"root\")") actor socket))
           (at (and (ok? a) (clause a 'cut))))
      (cond
        ((not (ok? a)) a)
        ((not (and (pair? at) (equal? (written (car at)) cut)))
         (list 'error 'bad-request '(reason roots-all-at-another-cut) (list 'cut cut) (list 'now (and (pair? at) (car at)))))
        (else
         (append (map cadr (filter (lambda (row) (and (pair? row) (eq? (car row) 'row))) (or (clause a 'items) '())))
                 (if fz (map car (freeze-news fz)) '()))))))

  ;; (1) THE MAIN STORE'S BLOCKS AT THE CUT. Each root is read through the
  ;; main store's route at the cut, so the answer is the cut's bytes or the
  ;; refusal (cut-moved, cut-unavailable, unknown-id), and a refusal is the
  ;; answer with nothing written anywhere. A tag is resolved once: the first
  ;; read's receipt names the literal, and every later read asks for it.
  (define (cut-text at) (if (string? at) at (written at)))
  ;; A FILE THE FREEZE ADDS is not in the main store: its copy is a holder
  ;; whose origin is the provisional id, with one code block holding the
  ;; whole file (the split into blocks is the landing's).
  ;; Its language is the path's, as import-code gives a file it adds.
  (define (new-records entry)
    (let* ((pid (car entry)) (lf (lang-fields (language-for-path (cadr entry)))))
      (list (list (cons 'id pid) (cons 'position (cons 'root 1))
                  (cons 'fields (append (list '(kind . file) '(mode . text) (cons 'path (cadr entry))) lf)))
            (list (cons 'id (string-append pid "/src")) (cons 'origin pid) (cons 'position (cons pid 1))
                  (cons 'fields (append (list '(kind . code) '(mode . text) (cons 'src (caddr entry))) lf))))))
  (define (scope-read store socket actor dir cut roots for . candidate)
    (let loop ((rs roots) (at cut) (got '()))
      (if (null? rs)
          (apply scope-plan store socket actor dir (cut-text at) (reverse got) roots for candidate)
          (let ((a (let ((n (and (pair? candidate) (car candidate) (assoc (car rs) (freeze-news (car candidate))))))
                     (if n
                         (list 'ok (cons 'items (new-records n)))
                         (routed store 'read (list (car rs) "--cut" (cut-text at) "--recursive") actor socket)))))
            (if (not (ok? a))
                a
                (let ((items (clause a 'items)) (c (clause a 'cut)))
                  (loop (cdr rs) (if c (car c) at) (cons (cons (car rs) items) got))))))))

  (define (scope-plan store socket actor dir cut-text per-root roots for . candidate)
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
        (else (apply scope-build store socket actor dir cut-text all ids roots for candidate)))))

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

  ;; THE CANDIDATE (a freeze): a copied block that has a draft holds the
  ;; draft's bytes, and the committed src it replaces is kept in a `before`
  ;; section under it when the two differ; a root the freeze deletes says so,
  ;; `(deleted . "proposed")`. A `before` has no origin: it is not a block
  ;; to review, and cannot be a verdict's source.
  (define (candidate-fields fz id root? fs)
    (let* ((d (and fz (assoc id (freeze-drafts fz))))
           (fs (if d
                   (map (lambda (f) (if (eq? (car f) 'src) (cons 'src (cdr d)) f))
                        (if (assq 'src fs) fs (append fs (list (cons 'src (cdr d))))))
                   fs)))
      (if (and fz root? (member id (freeze-deletes fz))) (append fs (list (cons 'deleted "proposed"))) fs)))
  (define (before-fields fz id fs)
    (let ((d (and fz (assoc id (freeze-drafts fz)))) (old (field fs 'src)))
      (and d (not (equal? old (cdr d)))
           (append (list '(kind . section) '(title . "before") (cons 'before-of id))
                   (if old (list (cons 'src old)) '())))))
  (define (scope-build store socket actor dir cut-text recs ids roots for . candidate)
    (let* ((fz (and (pair? candidate) (car candidate)))
           (pk (if (and (pair? candidate) (pair? (cdr candidate))) (cadr candidate) '()))
           (contested '())
           (copies
             (map (lambda (rec parent)
                    (let* ((id (field rec 'id)) (fs (field rec 'fields)) (skip (derived-keys fs))
                           (kept-fs (map (lambda (f) (stored-src f fs))
                                         (filter (lambda (f)
                                                   (cond ((memq (car f) skip) #f)
                                                         ((contested? (cdr f))
                                                          (set! contested (cons (list 'contested id (car f)) contested))
                                                          #f)
                                                         (else #t)))
                                                 fs))))
                      (list id parent
                            (append (candidate-fields fz id (member id roots) kept-fs)
                                    (list (cons 'origin (or (field rec 'origin) id)) (cons 'origin-cut cut-text)))
                            (before-fields fz id kept-fs))))
                  recs (placed-parents recs roots)))
           (edges (apply append
                         (map (lambda (rec)
                                (map (lambda (e) (list (field rec 'id) (car e) (cdr e)))
                                     (or (field rec 'edges) '())))
                              recs)))
           (kept (filter (lambda (e) (member (caddr e) ids)) edges))
           (dropped (map (lambda (e) (cons 'dropped-edge e))
                         (filter (lambda (e) (not (member (caddr e) ids))) edges)))
           (letter (if fz
                       (append (letter-fields for cut-text roots dir) (list (cons 'freeze (freeze-dir fz))))
                       (letter-fields for cut-text roots dir)))
           (befores (filter cadddr copies))
           (sections (packet-sections pk))
           (unwritable
             (append
               (let ((r (put-reason letter))) (if r (list (list "letter" 'fields r)) '()))
               (apply append
                      (map (lambda (c)
                             (let ((r (put-reason (caddr c)))) (if r (list (list (car c) 'fields r)) '())))
                           copies))
               (apply append
                      (map (lambda (c)
                             (let ((r (put-reason (cadddr c)))) (if r (list (list (car c) 'before r)) '())))
                           befores))
               (apply append
                      (map (lambda (x)
                             (let ((r (put-reason (cdr x)))) (if r (list (list "packet" 'fields r)) '())))
                           sections))
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
          (scope-write store socket actor dir cut-text letter copies kept dropped (reverse contested) fz sections))))

  ;; THE PACKET'S SECTIONS, parent first: each (<depth-first index of its
  ;; parent, or #f for the letter copy> . <fields>), marked `packet`: the
  ;; scope's own blocks, which a candidate's results leave out whoever made
  ;; them.
  (define (packet-sections pk)
    (let ((out '()) (n 0))
      (let walk ((xs pk) (parent #f))
        (for-each (lambda (x)
                    (let ((k n))
                      (set! n (+ n 1))
                      (set! out (cons (cons parent
                                            (append (list '(kind . section) (cons 'title (entry-part x 'title)) '(packet . #t))
                                                    (let ((t (entry-part x 'text))) (if t (list (cons 'text t)) '()))))
                                      out))
                      (walk (let ((c (assq 'children (cdr x)))) (if c (cdr c) '())) k)))
                  xs))
      (reverse out)))

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
  ;; A CANDIDATE'S `before` sections and the packet's sections follow the
  ;; copies in the same batch, each naming its parent's place; the baseline
  ;; is taken after them, so they are part of what the reviewer reads.
  (define (scope-write store socket actor dir cut-text letter copies kept dropped contested . candidate)
    (let ((init (routed dir 'init '() actor))
          (fz (and (pair? candidate) (car candidate)))
          (sections (if (and (pair? candidate) (pair? (cdr candidate))) (cadr candidate) '())))
      (if (not (ok? init))
          init
          (let* ((index (let loop ((cs copies) (k 1) (out '()))
                          (if (null? cs) out (loop (cdr cs) (+ k 1) (cons (cons (car (car cs)) k) out)))))
                 (befores (filter cadddr copies))
                 (first-section (+ 1 (length copies) (length befores)))
                 (intents
                   (cons (list 'insert 'root #f letter)
                         (append
                           (map (lambda (c)
                                  (let ((p (and (cadr c) (assoc (cadr c) index))))
                                    (list 'insert (if p (list 'from (cdr p)) 'root) #f (caddr c))))
                                copies)
                           (map (lambda (c) (list 'insert (list 'from (cdr (assoc (car c) index))) #f (cadddr c)))
                                befores)
                           (map (lambda (x) (list 'insert (list 'from (if (car x) (+ first-section (car x)) 0)) #f (cdr x)))
                                sections))))
                 (made (routed dir 'batch (list (written intents)) actor)))
            (if (not (batch-ok? made))
                (failed dir 'import made)
                (let* ((ids (map created-id (batch-items made)))
                       (copy-of (map (lambda (c id) (cons (car c) id)) copies (list-head (cdr ids) (length copies))))
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
                        (cond
                          ((not (and set-base (ok? set-base)))
                           (failed dir 'baseline (or set-base read-back)))
                          ;; A CANDIDATE'S LETTER IS NOT WRITTEN NOW: the main
                          ;; store's cut is the freeze's until the landing, and
                          ;; collect-into --letter new writes it after.
                          (fz
                           (append
                             (list 'ok (list 'scope dir) (list 'cut cut-text) (list 'letter 'new)
                                   (list 'letter-copy letter-copy) (list 'baseline baseline)
                                   (list 'blocks (length copies)) (list 'before (length befores))
                                   (list 'packet (length sections)) (list 'edges (length kept))
                                   (list 'dropped-edges (length dropped))
                                   (list 'contested-fields (length contested)))
                             dropped contested))
                          (else
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
                                          dropped contested)))))))))))))))

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
            ((field lfs 'freeze) (candidate-results r letter-copy lfs to))
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

  ;; ---- the results of a candidate's review ----------------------------------
  ;;
  ;; A SCOPE OF A FROZEN CANDIDATE (its letter copy has `freeze`) is answered
  ;; strictly: `(pending ...)` while no verdict is under the letter copy;
  ;; `(error results-incomplete (block <id>) (field <f>))` for the first piece
  ;; a verdict or a finding lacks (results-gap, which collect-into asks of the
  ;; same datum); `ok` only for complete results. The envelope carries the
  ;; letter's own fields, which the main letter is made from after the
  ;; landing, and the PROPOSALS: each copy whose src the reviewer changed.
  ;; A block of the reviewer's that is under neither the letter copy nor
  ;; another result has the parent `root`, so it is not taken for a verdict.
  ;; The scope's own blocks (a `before`, a packet section) are not results,
  ;; even when the scope's actor is the reader. A SOURCE is a CANDIDATE COPY:
  ;; live, and created with an `origin` -- a field set later does not make
  ;; one -- and the envelope lists them, `(candidates <id> ...)`, so
  ;; collect-into asks the same question of the datum.
  ;; A scope without `freeze` is answered as before (review-results-of).
  (define verdict-values '(no-objection findings could-not-review))
  (define finding-fields '(claim evidence confidence))
  (define (word-of v) (cond ((symbol? v) v) ((string? v) (string->symbol v)) (else #f)))
  ;; `sources` is a list of ids, or one string of ids separated by spaces,
  ;; which is what a `set` from the command line stores.
  (define (source-ids v)
    (cond ((and (list? v) (for-all string? v)) v)
          ((string? v)
           (let split ((cs (string->list v)) (cur '()) (out '()))
             (define (flush) (if (null? cur) out (cons (list->string (reverse cur)) out)))
             (cond ((null? cs) (reverse (flush)))
                   ((char-whitespace? (car cs)) (split (cdr cs) '() (flush)))
                   (else (split (cdr cs) (cons (car cs) cur) out)))))
          (else #f)))
  (define (result-value b k) (let ((f (assq k (or (block-part b 'fields) '())))) (and f (cadr f))))

  ;; -> #f when the results are complete, `none` when there is no verdict, or
  ;; (<block id> <field>) for the first gap, verdicts in order and each
  ;; verdict's findings after it. SOURCE? is asked of each source id.
  (define (results-gap d source?)
    (let* ((bs (results-part d 'blocks))
           (verdicts (filter (lambda (b) (eq? (block-part b 'parent) 'letter)) bs)))
      (if (null? verdicts)
          'none
          (let loop ((vs verdicts))
            (if (null? vs)
                #f
                (let* ((v (car vs)) (id (cadr v)) (srcs (source-ids (result-value v 'sources)))
                       (gap (cond ((not (memq (word-of (result-value v 'verdict)) verdict-values)) 'verdict)
                                  ((not (and (pair? srcs) (for-all source? srcs))) 'sources)
                                  ((find (lambda (k) (not (result-value v k))) condition-fields) => (lambda (k) k))
                                  (else #f))))
                  (if gap
                      (list id gap)
                      (let finding ((fs (filter (lambda (b) (equal? (block-part b 'parent) id)) bs)))
                        (if (null? fs)
                            (loop (cdr vs))
                            (let ((k (or (find (lambda (k) (not (result-value (car fs) k))) finding-fields)
                                         (and (null? (or (block-part (car fs) 'about) '())) 'about))))
                              (if k (list (cadr (car fs)) k) (finding (cdr fs)))))))))))))

  (define (created-fields r id)
    (let ((made (block-creation r id)))
      (and made (pair? (cdr made)) (pair? (cddr made)) (list? (cadr (cdr made))) (cadr (cdr made)))))
  (define (candidate-copy? r id)
    (let ((b (state-read r id)) (fs (created-fields r id)))
      (and b (not (field b 'deleted)) fs (string? (field fs 'origin)) (not (field fs 'before-of)) (not (field fs 'packet)))))
  (define (scope-own? r id)
    (let ((fs (created-fields r id))) (and fs (or (field fs 'before-of) (field fs 'packet)) #t)))
  (define (candidate-results r letter-copy lfs to)
    (let* ((plain (review-results-of r letter-copy to 'new))
           (d (find (lambda (x) (and (pair? x) (eq? (car x) 'results))) (cdr plain)))
           (blocks (filter (lambda (b) (not (scope-own? r (cadr b)))) (results-part d 'blocks)))
           (blocks (map (lambda (b)
                          (let ((p (field (state-read r (cadr b)) 'position)))
                            (if (and (eq? (block-part b 'parent) 'letter) (not (and (pair? p) (equal? (car p) letter-copy))))
                                (cons 'block (cons (cadr b) (map (lambda (x) (if (eq? (car x) 'parent) '(parent root) x)) (cddr b))))
                                b)))
                        blocks))
           (candidates (filter (lambda (id) (candidate-copy? r id)) (state-block-ids r)))
           (proposals (candidate-proposals r letter-copy to blocks))
           (envelope (list 'results (list 'letter 'new) (list 'to to) (list 'letter-copy letter-copy)
                           (list 'cut (field lfs 'cut)) (list 'roots (field lfs 'roots))
                           (list 'scope (field lfs 'scope)) (list 'status (field lfs 'status))
                           (list 'freeze (field lfs 'freeze)) (cons 'candidates candidates)
                           (cons 'blocks blocks) (cons 'proposals proposals)))
           (gap (results-gap envelope (lambda (id) (and (member id candidates) #t)))))
      (cond
        ((eq? gap 'none) (list 'pending (list 'letter-copy letter-copy) (list 'status (field lfs 'status))))
        (gap (list 'error 'results-incomplete (list 'block (car gap)) (list 'field (cadr gap))))
        (else (list 'ok envelope)))))

  ;; A PROPOSAL IS A COPY THE REVIEWER CHANGED: its src now, set by the
  ;; letter's `to`, is not the src it was copied with. It belongs to the
  ;; first FINDING about it (a result under a verdict), else to the first
  ;; verdict. An unchanged copy is not one, whatever else was written to it.
  (define (candidate-proposals r letter-copy to blocks)
    (let* ((verdicts (filter (lambda (b) (eq? (block-part b 'parent) 'letter)) blocks))
           (verdict-ids (map cadr verdicts))
           (events (let ((t (make-hashtable string-hash string=?)))
                     (for-each (lambda (b) (hashtable-set! t (cadr b) (cadr (assq 'fields (cddr b)))))
                               (state-datum r))
                     t))
           ;; an about names a copy by its origin (review-results-of's target)
           (on (lambda (origin)
                 (let ((f (find (lambda (b) (and (member (block-part b 'parent) verdict-ids)
                                                 (member (list 'origin origin) (or (block-part b 'about) '()))))
                                blocks)))
                   (cond (f (cadr f)) ((pair? verdicts) (cadr (car verdicts))) (else 'letter))))))
      (filter (lambda (x) x)
              (map (lambda (id)
                     (let* ((b (state-read r id)) (fs (and b (field b 'fields)))
                            (made (created-fields r id))
                            (was (and made (field made 'src)))
                            (now (and fs (field fs 'src)))
                            (origin (and fs (field fs 'origin)))
                            (setter (and now (assq 'src (settled-fields r events id)))))
                       (and (candidate-copy? r id) (string? origin) now (not (equal? now was))
                            setter (equal? (caddr setter) to)
                            (list 'proposal (list 'origin origin) (list 'scoped id) (list 'src now) (list 'on (on origin))))))
                   (live-in-order r)))))

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
  ;; A CANDIDATE'S RESULTS (the datum has `freeze`) are refused unless
  ;; complete, by the check review-results answers with (results-gap), and
  ;; may make their letter: `--letter new` in place of a letter id inserts
  ;; the main letter, with the letter copy's id as its `scoped-id`, in the
  ;; first write. `--map <file>`, `(mapping (<provisional id> <id>) ...)`,
  ;; is the landing's: every origin the datum names is read through it, and
  ;; an origin still provisional is named, not linked. `--to-writer <W>`
  ;; makes each proposal a draft for W.
  (define (collect-into-verb store args options)
    (let ((text (argument-option options "--results"))
          (new? (equal? (argument-option options "--letter") "new"))
          (map-file (argument-option options "--map"))
          (to-writer (argument-option options "--to-writer")))
      (if (not (and (string? text)
                    (if new? (null? args) (= 1 (length args)))
                    (or (not (argument-option options "--letter")) new?)))
          'usage
          (let ((d (plain-datum text)) (mapping (if map-file (read-mapping map-file) '())))
            (cond
              ((not (results-datum? d)) '(error bad-request (reason results-malformed)))
              ((and (pair? mapping) (eq? (car mapping) 'error)) mapping)
              ((and (candidate-datum? d) (results-gap d (lambda (id) (and (member id (results-part d 'candidates)) #t))))
               => (lambda (gap)
                    (if (eq? gap 'none)
                        (list 'error 'results-incomplete (list 'block (results-part d 'letter-copy)) '(field verdict))
                        (list 'error 'results-incomplete (list 'block (car gap)) (list 'field (cadr gap))))))
              ((and new? (not (candidate-datum? d)))
               '(error bad-request (reason letter-new-needs-a-candidate)))
              (else (collect-into-write store (if new? 'new (car args)) (mapped d mapping) to-writer)))))))

  (define (candidate-datum? d) (and (results-part d 'freeze) #t))
  ;; A PROVISIONAL ID is a landing's name for a block it has not made yet:
  ;; `new-` and 56 hex digits.
  (define (provisional? id)
    (and (string? id) (= (string-length id) 60) (string=? (substring id 0 4) "new-")
         (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list (substring id 4 60)))))
  ;; THE MAPPING IS THE LANDING'S ANSWER: the file holds what
  ;; tools/landing-commit.sc printed, `(ok ... (created ("<provisional>" .
  ;; "<id>") ...) ...)`; a landing that added no file has no `created`
  ;; clause, and maps nothing. -> ((<provisional> <id>) ...), or the refusal
  (define (read-mapping path)
    (let* ((m (file-datum path))
           (c (and (list? m) (pair? m) (eq? (car m) 'ok) (for-all pair? (cdr m)) (assq 'created (cdr m)))))
      (cond
        ((not (and (list? m) (pair? m) (eq? (car m) 'ok))) (list 'error 'bad-request '(reason map-unreadable) (list 'file path)))
        ((not c) '())
        ((and (list? c) (for-all (lambda (x) (and (pair? x) (string? (car x)) (string? (cdr x)))) (cdr c)))
         (map (lambda (x) (list (car x) (cdr x))) (cdr c)))
        (else (list 'error 'bad-request '(reason map-unreadable) (list 'file path))))))
  ;; THE DATUM WITH EVERY ORIGIN READ THROUGH THE MAPPING: the about targets
  ;; and the proposals' origins.
  (define (mapped d mapping)
    (define (through id) (let ((p (assoc id mapping))) (if p (cadr p) id)))
    (if (null? mapping)
        d
        (cons 'results
              (map (lambda (part)
                     (case (car part)
                       ((blocks)
                        (cons 'blocks
                              (map (lambda (b)
                                     (cons 'block
                                           (cons (cadr b)
                                                 (map (lambda (x)
                                                        (if (eq? (car x) 'about)
                                                            (list 'about (map (lambda (t) (if (eq? (car t) 'origin) (list 'origin (through (cadr t))) t))
                                                                              (cadr x)))
                                                            x))
                                                      (cddr b)))))
                                   (cdr part))))
                       ((proposals)
                        (cons 'proposals
                              (map (lambda (p) (map (lambda (x) (if (and (pair? x) (eq? (car x) 'origin)) (list 'origin (through (cadr x))) x)) p))
                                   (cdr part))))
                       (else part)))
                   (cdr d)))))

  (define (results-part d key) (let ((p (assq key (cdr d)))) (and p (if (memq key '(blocks proposals candidates)) (cdr p) (cadr p)))))
  (define (block-part b key) (let ((p (assq key (cddr b)))) (and p (cadr p))))
  (define (proposal-part p key) (let ((x (assq key (cdr p)))) (and x (cadr x))))

  ;; THE DATUM IS THE CALLER'S, and is read as one: every part this verb
  ;; reads is checked for its shape before anything is planned.
  (define (results-datum? d)
    (and (list? d) (pair? d) (eq? (car d) 'results)
         (for-all (lambda (x) (and (list? x) (pair? x) (or (memq (car x) '(blocks proposals candidates)) (pair? (cdr x))))) (cdr d))
         (string? (results-part d 'to)) (list? (results-part d 'blocks))
         (let ((l (results-part d 'letter))) (or (string? l) (and (eq? l 'new) (string? (results-part d 'letter-copy)))))
         ;; a candidate's: the letter's fields, and the proposals
         (or (not (results-part d 'freeze))
             (and (string? (results-part d 'freeze)) (string? (results-part d 'cut)) (string? (results-part d 'scope))
                  (let ((rs (results-part d 'roots))) (and (list? rs) (for-all string? rs)))
                  (let ((cs (results-part d 'candidates))) (and (list? cs) (for-all string? cs)))
                  (let ((ps (or (results-part d 'proposals) '()))
                        (ids (map (lambda (b) (and (pair? b) (pair? (cdr b)) (cadr b))) (or (results-part d 'blocks) '()))))
                    (for-all (lambda (p)
                               (and (list? p) (pair? p) (eq? (car p) 'proposal)
                                    (for-all (lambda (x) (and (list? x) (= 2 (length x)))) (cdr p))
                                    (string? (proposal-part p 'origin)) (string? (proposal-part p 'scoped))
                                    (let ((src (proposal-part p 'src))) (or (string? src) (bytevector? src)))
                                    ;; a proposal belongs to a result the datum carries, or to the letter
                                    (let ((on (proposal-part p 'on))) (or (and (string? on) (member on ids) #t) (eq? on 'letter)))))
                             ps))))
         ;; one result one id: two blocks with one id would both be planned
         ;; as new, and the second write would find them ambiguous
         (let ids ((bs (results-part d 'blocks)) (seen '()))
           (cond ((null? bs) #t)
                 ((and (pair? (car bs)) (pair? (cdar bs)) (member (cadr (car bs)) seen)) #f)
                 (else (ids (cdr bs) (if (and (pair? (car bs)) (pair? (cdar bs))) (cons (cadr (car bs)) seen) seen)))))
         (for-all (lambda (b)
                    (and (list? b) (> (length b) 2) (eq? (car b) 'block) (string? (cadr b))
                         (for-all (lambda (x) (and (pair? x) (pair? (cdr x)))) (cddr b))
                         (let ((p (block-part b 'parent))) (or (string? p) (memq p '(letter root))))
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

  ;; THE MAIN LETTER OF A CANDIDATE'S REVIEW, from the fields the datum
  ;; carries; its identity is the letter copy's id, as a copy's is its own.
  (define (main-letter-fields d)
    (list (cons 'kind 'doc) (cons 'to (results-part d 'to)) (cons 'status (or (results-part d 'status) "unread"))
          (cons 'cut (results-part d 'cut)) (cons 'roots (results-part d 'roots)) (cons 'scope (results-part d 'scope))
          (cons 'freeze (results-part d 'freeze)) (cons 'scoped-id (results-part d 'letter-copy))))

  ;; LETTER is the main letter's id, or `new`: then the first write finds the
  ;; letter it made before by its scoped-id, or inserts it first, at place 0,
  ;; and every result that goes under the letter names that place.
  (define (collect-into-write store letter d . to-writer-given)
    (let* ((to (results-part d 'to)) (blocks (results-part d 'blocks))
           (to-writer (and (pair? to-writer-given) (car to-writer-given)))
           (refused #f) (inserted 0) (linked 0) (already 0) (unresolved '()) (letter-id #f)
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
                 (let* ((t (copies-by-scoped-id state))
                        (found (and (eq? letter 'new) (one t (results-part d 'letter-copy))))
                        (fresh? (and (eq? letter 'new) (not found) (not refused)))
                        (letter (cond ((not (eq? letter 'new)) letter) (found found) (else (list 'from 0))))
                        (shift (if fresh? 1 0)))
                 (if (and (string? letter) (not (state-read state letter)))
                     (begin (set! refused (list 'error 'unknown-id letter)) '())
                     (let* ((new (parent-first (filter (lambda (b) (not (one t (cadr b)))) blocks)))
                            (index (let loop ((bs new) (k 0) (out '()))
                                     (if (null? bs) out (loop (cdr bs) (+ k 1) (cons (cons (cadr (car bs)) k) out)))))
                            (intents
                              (map (lambda (b k)
                                     (let* ((p (block-part b 'parent))
                                            (parent (cond ((not (string? p)) letter)
                                                          ((one t p) => (lambda (id) id))
                                                          ((assoc p index)
                                                           => (lambda (e) (if (< (cdr e) k) (list 'from (+ shift (cdr e))) letter)))
                                                          (else letter))))
                                       (list 'insert parent #f
                                             (append (map (lambda (f) (cons (car f) (cadr f)))
                                                          (filter (lambda (f) (not (eq? (car f) 'scoped-id)))
                                                                  (block-part b 'fields)))
                                                     (list (cons 'scoped-id (cadr b)))))))
                                   new (iota (length new))))
                            (all (if fresh? (cons (list 'insert 'root #f (main-letter-fields d)) intents) intents))
                            ;; A CANDIDATE'S RESULTS GO IN WHOLE OR NOT AT ALL: a
                            ;; batch answers each item, so every insert, the
                            ;; letter's included, is put to the write's own
                            ;; validator first, and one it would refuse refuses
                            ;; the collect with nothing written.
                            (unwritable (if (candidate-datum? d)
                                            (filter (lambda (x) x)
                                                    (map (lambda (i) (let ((r (put-reason (cadddr i)))) (and r (list 'fields r))))
                                                         all))
                                            '())))
                       (when (and (pair? unwritable) (not refused))
                         (set! refused (list 'error 'collect-unwritable (cons 'items unwritable))))
                       (if refused '() (begin (set! inserted (length intents)) all))))))
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
                            (noted (set! letter-id (if (eq? letter 'new) (one t (results-part d 'letter-copy)) letter)))
                            (pairs
                              (let loop ((ps (apply append
                                                    (map (lambda (b)
                                                           (let ((from (one t (cadr b))))
                                                             (map (lambda (target)
                                                                    (list from
                                                                          (if (eq? (car target) 'origin)
                                                                              (and (not (and (candidate-datum? d) (provisional? (cadr target)))) (cadr target))
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
              (let ((drafted (if to-writer (draft-proposals store d to to-writer) '())))
               (if (not (and (list? drafted)
                             (for-all (lambda (x) (and (pair? x) (memq (car x) '(draft unresolved unknown already held undraftable)))) drafted)))
                drafted
              (append (list 'ok (list 'inserted inserted) (list 'linked linked) (list 'already already))
                      (if (eq? letter 'new) (list (list 'letter letter-id)) '())
                      (if (candidate-datum? d)
                          (list (list 'proposals (length (or (results-part d 'proposals) '()))) (list 'drafted drafted))
                          '())
                      (reverse unresolved)
                      (apply append
                             (map (lambda (b)
                                    (map (lambda (f) (list 'foreign-field (cadr b) (car f) (caddr f)))
                                         (filter (lambda (f) (not (equal? (caddr f) to))) (block-part b 'fields))))
                                  blocks))))))))))))

  ;; A PROPOSAL BECOMES A DRAFT FOR THE RECEIVING WRITER, never a record of
  ;; the store: the store's own `write`, on the block the proposal's origin
  ;; names, as the reviewer, into the receiver's working area, where the
  ;; receiver keeps, rewrites or discards it. EACH IS PLANNED FIRST, from the
  ;; state at the locked point and the receiver's live drafts, and is not
  ;; written when:
  ;; - its origin is still provisional: `(unresolved <scoped> (origin <id>))`;
  ;; - the block is not there: `(unknown <scoped> <block>)`;
  ;; - its provenance is already recorded on the collected block it belongs
  ;;   to -- a retry, after the receiver kept, rewrote or discarded it:
  ;;   `(already <block>)`;
  ;; - the receiver holds a draft of that block already, which is the
  ;;   receiver's and is not overwritten: `(held <block> <version>)`.
  ;; A FILE THE LANDING ADDED is named by its holder; its proposal is drafted
  ;; on the holder's one code block, and a file split into more than one is
  ;; named, `(undraftable <scoped> <holder>)`, not drafted.
  ;; A draft written is `(draft <block> <version> <on>)`, and THE PROVENANCE
  ;; is written on the collected block it belongs to, `(proposed (<block>
  ;; <version>) ...)`, adding to what is there, in one more write planned at
  ;; the locked point, and not written when nothing is added.
  ;; -> those entries, or a refusal
  (define (draft-proposals store d to to-writer)
    (let* ((ps (or (results-part d 'proposals) '()))
           (main-of (lambda (t on)
                      (let ((xs (hashtable-ref t (if (eq? on 'letter) (results-part d 'letter-copy) on) '())))
                        (and (= 1 (length xs)) (car xs)))))
           (proposed-of (lambda (state id) (let ((v (and id (state-read state id)))) (or (and v (field (field v 'fields) 'proposed)) '()))))
           (live (let ((a (rpc-dispatch-parsed store 'drafts (parse-arguments 'drafts (list "--writer" to-writer)) to #f #f)))
                   (if (ok? a)
                       (map (lambda (x) (cons (entry-part x 'block) (entry-part x 'version)))
                            (filter (lambda (x) (and (pair? x) (eq? (car x) 'draft))) (or (clause a 'items) '())))
                       a)))
           (plan '()))
      (if (not (list? live))
          live
          (begin
            (with-store-write store
              (lambda (state view)
                (let ((t (copies-by-scoped-id state)))
                  (set! plan
                    (map (lambda (p)
                           (let* ((origin (proposal-part p 'origin)) (scoped (proposal-part p 'scoped)) (on (proposal-part p 'on))
                                  (b (and (not (provisional? origin)) (state-read state origin)))
                                  (holder? (and b (eq? (field (field b 'fields) 'kind) 'file)))
                                  (kids (if holder? (code-children state origin) '()))
                                  (target (cond ((not holder?) origin) ((= 1 (length kids)) (car kids)) (else #f))))
                             (cond
                               ((provisional? origin) (list 'unresolved scoped (list 'origin origin)))
                               ((or (not b) (field b 'deleted)) (list 'unknown scoped origin))
                               ((not target) (list 'undraftable scoped origin))
                               ((assoc target (proposed-of state (main-of t on))) (list 'already target))
                               ((assoc target live) => (lambda (h) (list 'held target (cdr h))))
                               (else (list 'write target (proposal-part p 'src) on)))))
                         ps))
                  '()))
              to #f #f)
            (let loop ((xs plan) (out '()))
              (if (pair? xs)
                  (if (not (eq? (car (car xs)) 'write))
                      (loop (cdr xs) (cons (car xs) out))
                      (let* ((target (cadr (car xs))) (src (caddr (car xs))) (on (cadddr (car xs)))
                             (a (rpc-dispatch-parsed store 'write
                                                     (parse-arguments 'write (list target (if (bytevector? src) (utf8->string src) src)
                                                                                   "--writer" to-writer))
                                                     to #f #f)))
                        (if (ok? a)
                            (loop (cdr xs) (cons (list 'draft target (car (clause a 'version)) on) out))
                            a)))
                  (let* ((done (reverse out))
                         (drafts (filter (lambda (x) (eq? (car x) 'draft)) done))
                         (written-at
                           (with-store-write store
                             (lambda (state view)
                               (let* ((t (copies-by-scoped-id state))
                                      (ons (let dedup ((xs (map cadddr drafts)) (seen '()))
                                             (cond ((null? xs) (reverse seen))
                                                   ((member (car xs) seen) (dedup (cdr xs) seen))
                                                   (else (dedup (cdr xs) (cons (car xs) seen)))))))
                                 (filter (lambda (x) x)
                                         (map (lambda (on)
                                                (let* ((id (main-of t on)) (had (proposed-of state id))
                                                       (adds (filter (lambda (e) (not (member e had)))
                                                                     (map (lambda (x) (list (cadr x) (caddr x)))
                                                                          (filter (lambda (x) (equal? (cadddr x) on)) drafts)))))
                                                  (and id (pair? adds) (list 'set id 'proposed (append had adds)))))
                                              ons))))
                             to #f #f)))
                    (cond
                      ((all-ok written-at) done)
                      ((and (= 1 (length written-at)) (judgement-refusal? (car written-at))) (car written-at))
                      (else (cons 'batch (list written-at)))))))))))

  ;; ---- collect: review-results, then collect-into --------------------------
  ;;
  ;; Each step is its store's own request, on its own route; collect writes
  ;; nothing in the scoped store. The lists review-results makes (no-about,
  ;; missing) are passed on beside collect-into's answer.
  ;; A CANDIDATE'S RESULTS are collected with `--letter new`: the letter is
  ;; made with them. `--map` (a path, so made absolute here: collect-into may
  ;; run in the store's server) and `--to-writer` are passed on. MORE is the
  ;; main store's socket, then the options.
  (define (collect-verb store actor args cwd . more)
    (let* ((socket (and (pair? more) (car more)))
           (options (if (and (pair? more) (pair? (cdr more))) (cadr more) '()))
           (map-file (argument-option options "--map"))
           (to-writer (argument-option options "--to-writer")))
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
              (let* ((into (routed store 'collect-into
                                   (append (if (candidate-datum? d)
                                               (list "--letter" "new")
                                               (list (results-part d 'letter)))
                                           (list "--results" (written d))
                                           (if map-file (list "--map" (absolute map-file cwd)) '())
                                           (if to-writer (list "--to-writer" to-writer) '()))
                                   actor socket)))
                (if (ok? into)
                    (append into (filter (lambda (x) (and (pair? x) (memq (car x) '(no-about missing))))
                                         (cddr results)))
                    into))))))))
)
