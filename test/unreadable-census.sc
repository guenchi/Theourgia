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

;; THREE CENSUSES OF THE PRODUCTION SOURCE, EACH PINNED TO A TABLE.
;;
;; A writer's file that cannot be read must not be answered as a file that
;; is not there. Two kinds of code have answered it that way: a presence
;; or type question asked before the read (`file-exists?` says #f for a
;; directory it may not search), and an exception handler that turns the
;; failed read into #f, '() or a default. Rounds of review kept finding
;; more of both, so the list is not written in a document: it is read from
;; the source here and compared with `unreadable-census.sexp`, which says
;; for each one why it is there.
;;
;;   (i)   every occurrence of a presence or type predicate, including the
;;         host spellings and any name an import gives them, counted per
;;         enclosing definition;
;;   (ii)  every exception handler, with what it catches, its category and
;;         its whole body, so a body turned from a re-raise into #f is a
;;         changed pin even though what it catches stays the same;
;;   (iii) the discovery record's raw coordinate accessors, which may
;;         appear only in the record and in the wrapper that guards each.
;;
;; A census that cannot go red is not a census, so each row is also run
;; against an in-memory copy of log.sc with one thing added or changed,
;; and the difference it reports is compared with the one expected.
;;
;; STATED LIMITS. This reads source, not its expansion, so it does not see:
;;   - a helper that is unchanged and gains a new caller under a writer's
;;     directory (no occurrence is added; that is why the generic helpers
;;     are converted whoever calls them);
;;   - a guard written where a macro puts it, as an argument rather than at
;;     the head of a form, e.g. (invoke guard (e (#t #f)) ...);
;;   - a handler reached through a value: with-exception-handler bound to
;;     another name by let or define, or base-exception-handler
;;     parameterized;
;;   - an identifier made during expansion, by datum->syntax or
;;     string->symbol, for a predicate or a raw accessor;
;;   - a file brought in by include from outside the walk;
;;   - an import inside a body, (let () (import ...) ...): imports are read
;;     at the top level and as a library's children only, so a prefix or
;;     rename made there hides the predicate or handler it names;
;;   - a raw coordinate read by reflection, (record-accessor (record-rtd d)
;;     k): raw accessors are found by name;
;;   - what the guard around a raw accessor does: row (iii) reads that each
;;     wrapper is (guarded-coordinate raw), not the body of guarded-
;;     coordinate itself, which a behaviour row (U7b) measures instead.
;; Each was shown by review to keep every row here green. A check for any of
;; these shapes would be one more partial reader measuring its own reach.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/log.sc")) up script-dir)))

;; ---- the scanner ------------------------------------------------------

(define presence-names
  '(file-exists? file-is-directory? file-is-regular? file-directory? file-regular?
    entry-name-type))
(define handler-heads '(guard with-exception-handler))
(define source-suffixes '(".sc" ".ss" ".sls" ".scm"))

(define (suffix? s suffix)
  (let ((n (string-length s)) (k (string-length suffix)))
    (and (>= n k) (string=? suffix (substring s (- n k) n)))))

;; EVERY SOURCE THE CORE SHIPS, AT ANY DEPTH. The root's `test/` is out of
;; scope: the fixtures are not the product. A directory called test
;; anywhere else is walked like any other. A symbolic link to a directory
;; is not followed, because the checkout carries `theourgia -> .` and a
;; walk that follows it never ends; a symbolic link to a source file is
;; read like the file. Paths are relative to the root and sorted.
(define (production-files)
  (define (walk rel out)
    (let ((dir (if (string=? rel "") root (string-append root "/" rel))))
      (fold-left
        (lambda (out name)
          (let* ((r (if (string=? rel "") name (string-append rel "/" name)))
                 (p (string-append root "/" r)))
            (cond
              ((member name '("." ".." ".git")) out)
              ((and (string=? rel "") (string=? name "test")) out)
              ((and (file-symbolic-link? p) (file-directory? p)) out)
              ((file-directory? p) (walk r out))
              ((exists (lambda (s) (suffix? name s)) source-suffixes) (cons r out))
              (else out))))
        out
        (directory-list dir))))
  (list-sort string<? (walk "" '())))

(define (text-of rel)
  (call-with-input-file (string-append root "/" rel) get-string-all))

(define (unwrap x) (if (annotation? x) (annotation-expression x) x))

;; THE NAME A FORM DEFINES, so each occurrence is placed in the chain of
;; definitions around it -- a place that does not move when a line is
;; added above it.
(define (defined-name d)
  (and (pair? d) (pair? (cdr d))
       (let ((h (cadr d)))
         (case (car d)
           ((define define-syntax)
            (cond ((symbol? h) h) ((and (pair? h) (symbol? (car h))) (car h)) (else #f)))
           ((define-record-type define-condition-type)
            (cond ((symbol? h) h) ((and (pair? h) (symbol? (car h))) (car h)) (else #f)))
           (else #f)))))

;; THE NAMES AN IMPORT GIVES A BASE NAME: an alist (local . canonical).
;; `(rename lib (file-exists? e?))` makes `e?` a presence question, and
;; `(prefix lib cs:)` makes `cs:file-exists?` one and `cs:guard` a handler;
;; none of them spells the base name at its use. The specs compose as the
;; language composes them -- a prefix of a prefix, a rename of an only --
;; and every library reference is taken to export every base name, which
;; can only add names to look for.
(define (import-aliases forms base)
  (define (visible s)
    (cond
      ((not (pair? s)) '())
      ((and (eq? (car s) 'only) (pair? (cdr s)))
       (filter (lambda (e) (memq (car e) (cddr s))) (visible (cadr s))))
      ((and (eq? (car s) 'except) (pair? (cdr s)))
       (filter (lambda (e) (not (memq (car e) (cddr s)))) (visible (cadr s))))
      ((and (eq? (car s) 'prefix) (pair? (cdr s)) (pair? (cddr s)) (symbol? (caddr s)))
       (map (lambda (e)
              (cons (string->symbol (string-append (symbol->string (caddr s)) (symbol->string (car e))))
                    (cdr e)))
            (visible (cadr s))))
      ((and (eq? (car s) 'rename) (pair? (cdr s)) (list? (cddr s)))
       (let ((pairs (filter (lambda (pr) (and (list? pr) (= 2 (length pr)))) (cddr s))))
         (map (lambda (e) (let ((pr (assq (car e) pairs))) (if pr (cons (cadr pr) (cdr e)) e)))
              (visible (cadr s)))))
      ((and (eq? (car s) 'for) (pair? (cdr s)))
       (visible (cadr s)))
      (else (map (lambda (b) (cons b b)) base))))
  (define (imports x)
    (if (and (pair? x) (eq? (car x) 'import) (list? x))
        (apply append (map visible (cdr x)))
        '()))
  (let loop ((all (apply append
                         (map (lambda (b) (cons b b)) base)
                         (map (lambda (f)
                                (append (imports f)
                                        (if (and (pair? f) (eq? (car f) 'library) (list? f))
                                            (apply append (map imports (cdr f)))
                                            '())))
                              forms)))
             (out '()))
    (cond ((null? all) (reverse out))
          ((assq (caar all) out) (loop (cdr all) out))
          (else (loop (cdr all) (cons (car all) out))))))

;; WHAT A HANDLER CATCHES: each guard clause's test, in order; `else` is
;; #t; a `with-exception-handler` catches everything.
(define (handler-catches kind d)
  (if (eq? kind 'with-exception-handler)
      'any
      (let ((spec (and (pair? (cdr d)) (cadr d))))
        (if (and (pair? spec) (list? (cdr spec)))
            (map (lambda (c) (cond ((not (pair? c)) c)
                                   ((eq? (car c) 'else) #t)
                                   (else (car c))))
                 (cdr spec))
            'unparsed))))

;; THE BODY A HANDLER IS PINNED BY: a guard's (variable clause ...), a
;; with-exception-handler's handler expression. The guarded expression is
;; not part of it -- that changes with every edit to the code it protects.
(define (handler-body kind d)
  (and (pair? (cdr d)) (cadr d)))

;; THE SPAN OF EACH GUARD CLAUSE'S BODY, as character offsets into the
;; text: what the negative test replaces with #f.
(define (clause-spans a)
  (let ((spec (and (pair? (cdr (unwrap a))) (cadr (unwrap a)))))
    (let ((clauses (and (pair? (unwrap spec)) (cdr (unwrap spec)))))
      (let loop ((cs (unwrap clauses)) (out '()))
        (let ((cs (unwrap cs)))
          (if (not (pair? cs))
              (reverse out)
              (let* ((c (unwrap (car cs)))
                     (body (and (pair? c) (unwrap (cdr c))))
                     (exprs (let flat ((b body) (acc '()))
                              (let ((b (unwrap b)))
                                (if (pair? b) (flat (cdr b) (cons (car b) acc)) (reverse acc))))))
                (loop (cdr cs)
                      (if (and (pair? exprs) (annotation? (car exprs))
                               (annotation? (car (last-pair exprs))))
                          (cons (cons (source-object-bfp (annotation-source (car exprs)))
                                      (source-object-efp
                                        (annotation-source (car (last-pair exprs)))))
                                out)
                          (cons #f out))))))))))

;; ONE FILE'S TEXT, READ AS DATA WITH CHEZ'S OWN READER: a comment is not a
;; datum and a string is not a symbol. Answers three lists:
;;   presence  (file defs name)                     one per occurrence
;;   handlers  (file defs ord kind catches body spans position)
;;   symbols   (file defs name)                     every symbol, for (iii)
(define (scan-text rel text)
  (let* ((sfd (make-source-file-descriptor rel (open-bytevector-input-port (string->utf8 text))))
         (p (open-string-input-port text))
         (annotated (let loop ((bfp 0) (out '()))
                      (let-values (((a nb) (get-datum/annotations p sfd bfp)))
                        (if (eof-object? a) (reverse out) (loop nb (cons a out))))))
         (stripped (map annotation-stripped annotated))
         (names (map car (import-aliases stripped presence-names)))
         (heads (import-aliases stripped handler-heads))
         (presence '()) (handlers '()) (symbols '())
         (ordinals (make-hashtable equal-hash equal?)))
    (define (ordinal! defs)
      (let ((k (hashtable-ref ordinals defs 0)))
        (hashtable-set! ordinals defs (+ k 1))
        (+ k 1)))
    (for-each
      (lambda (a)
        (let walk ((x a) (defs '()))
          (let ((d (unwrap x)))
            (cond
              ((symbol? d)
               (set! symbols (cons (list rel defs d) symbols))
               (when (memq d names)
                 (set! presence (cons (list rel defs d) presence))))
              ((pair? d)
               (let* ((sd (annotation-stripped x))
                      (nm (defined-name sd))
                      (defs2 (if nm (append defs (list nm)) defs))
                      (head (let ((h (assq (unwrap (car d)) heads))) (and h (cdr h)))))
                 (when (and head (annotation? x))
                   (set! handlers
                     (cons (list rel defs2 (ordinal! defs2) head
                                 (handler-catches head sd) (handler-body head sd)
                                 (if (eq? head 'guard) (clause-spans x) '())
                                 (source-object-bfp (annotation-source x)))
                           handlers)))
                 (let lp ((l d))
                   (let ((l (unwrap l)))
                     (cond ((pair? l) (walk (car l) defs2) (lp (cdr l)))
                           ((null? l) (void))
                           (else (walk l defs2)))))))
              ((vector? d) (vector-for-each (lambda (e) (walk e defs)) d))
              ((box? d) (walk (unbox d) defs))
              (else (void))))))
      annotated)
    (values (reverse presence) (reverse handlers) (reverse symbols))))

(define (scan-tree files texts)
  (let loop ((fs files) (ts texts) (ps '()) (hs '()) (ss '()))
    (if (null? fs)
        (values ps hs ss)
        (let-values (((p h s) (scan-text (car fs) (car ts))))
          (loop (cdr fs) (cdr ts) (append ps p) (append hs h) (append ss s))))))

;; ---- the comparisons --------------------------------------------------

;; OCCURRENCES WITH MULTIPLICITY: ((file defs name) . count), in first-seen
;; order.
(define (tally keys)
  (let ((h (make-hashtable equal-hash equal?)) (order '()))
    (for-each (lambda (k)
                (unless (hashtable-contains? h k) (set! order (cons k order)))
                (hashtable-update! h k (lambda (n) (+ n 1)) 0))
              keys)
    (map (lambda (k) (cons k (hashtable-ref h k 0))) (reverse order))))

;; ROW (i): what the source has and the pin does not, what the pin has and
;; the source does not, and each count that differs.
(define (presence-diff scanned pinned)
  (let ((have (tally scanned))
        (pin (map (lambda (e) (cons (list (car e) (cadr e) (caddr e)) (cadddr e))) pinned)))
    (append
      (map (lambda (k) (list 'unpinned (car k) (cdr k)))
           (filter (lambda (k) (not (assoc (car k) pin))) have))
      (map (lambda (k) (list 'not-in-source (car k) (cdr k)))
           (filter (lambda (k) (not (assoc (car k) have))) pin))
      (map (lambda (k) (list 'count (car k) (cdr (assoc (car k) pin)) (cdr k)))
           (filter (lambda (k) (let ((e (assoc (car k) pin))) (and e (not (= (cdr e) (cdr k))))))
                   have)))))

;; ROW (ii): handlers keyed by file, definitions and ordinal within them.
;; A pinned handler is (file defs ord kind catches category segment note
;; body).
(define (handler-key h) (list (car h) (cadr h) (caddr h)))
(define (handler-diff scanned pinned)
  (let ((pin (map (lambda (e) (cons (handler-key e) e)) pinned))
        (have (map (lambda (h) (cons (handler-key h) h)) scanned)))
    (append
      (map (lambda (k) (list 'unpinned (car k) (list-ref (cdr k) 4)))
           (filter (lambda (k) (not (assoc (car k) pin))) have))
      (map (lambda (k) (list 'not-in-source (car k)))
           (filter (lambda (k) (not (assoc (car k) have))) pin))
      (apply append
        (map (lambda (k)
               (let ((e (assoc (car k) pin)))
                 (if (not e)
                     '()
                     (let ((h (cdr k)) (p (cdr e)))
                       (append
                         (if (equal? (list-ref h 3) (list-ref p 3)) '()
                             (list (list 'kind (car k) (list-ref p 3) (list-ref h 3))))
                         (if (equal? (list-ref h 4) (list-ref p 4)) '()
                             (list (list 'catches (car k) (list-ref p 4) (list-ref h 4))))
                         (if (equal? (list-ref h 5) (list-ref p 8)) '()
                             (list (list 'body (car k)))))))))
             have)))))

;; `conservative` SINCE F100a: a handler that chooses the branch a ruling
;; names as the safe one (the R2g skip, F77b: log.sc's present-or-unreadable-skip?), stated per pin.
(define categories '(propagate fact refuse unrelated conservative))
(define (pin-shape-problems pin)
  (append
    (map (lambda (e) (list 'presence e))
         (filter (lambda (e)
                   (not (and (list? e) (= (length e) 7) (string? (car e)) (list? (cadr e))
                             (symbol? (caddr e)) (fixnum? (cadddr e))
                             (memq (list-ref e 4) '(keep binding convert out-of-scope))
                             (memq (list-ref e 5) '(a b c -))
                             (string? (list-ref e 6)) (> (string-length (list-ref e 6)) 0))))
                 (cdr (assq 'presence pin))))
    (map (lambda (e) (list 'handler (list-head e (min 3 (length e)))))
         (filter (lambda (e)
                   (not (and (list? e) (= (length e) 9) (string? (car e)) (list? (cadr e))
                             (fixnum? (caddr e))
                             (memq (list-ref e 5) categories)
                             (memq (list-ref e 6) '(a b c))
                             (string? (list-ref e 7)) (> (string-length (list-ref e 7)) 0))))
                 (cdr (assq 'handlers pin))))))

;; ROW (iii): every symbol that is one of the record's raw accessors, with
;; where it stands, against the places the pin allows: the record itself
;; and the one wrapper for each.
(define (record-fields forms-of-log record)
  (let ((f (find (lambda (x) (and (pair? x) (eq? (car x) 'define-record-type)
                                  (eq? (defined-name x) record)))
                 forms-of-log)))
    (and f
         (let ((fs (find (lambda (c) (and (pair? c) (eq? (car c) 'fields))) (cddr f))))
           (and fs (cdr fs))))))
(define (raw-accessors fields)
  (filter (lambda (s)
            (let ((t (symbol->string s)))
              (and (> (string-length t) 4) (string=? "raw-" (substring t 0 4)))))
          (map (lambda (f) (if (and (pair? f) (= (length f) 3)) (caddr f) f)) fields)))
(define (raw-diff symbols raws allowed)
  (let ((have (tally (map (lambda (s) (list (car s) (cadr s) (caddr s)))
                          (filter (lambda (s) (memq (caddr s) raws)) symbols)))))
    (append
      (map (lambda (k) (list 'outside-a-wrapper (car k) (cdr k)))
           (filter (lambda (k) (not (member (car k) allowed))) have))
      (map (lambda (k) (list 'count (car k) (cdr k)))
           (filter (lambda (k) (and (member (car k) allowed) (not (= (cdr k) 1)))) have))
      (map (lambda (a) (list 'missing a))
           (filter (lambda (a) (not (assoc a have))) allowed)))))

;; ---- the pin ----------------------------------------------------------

(define pin
  (call-with-input-file (string-append script-dir "/unreadable-census.sexp")
    (lambda (p) (let loop ((out '()))
                  (let ((x (read p))) (if (eof-object? x) (reverse out) (loop (cons x out))))))))
(define pinned-presence (cdr (assq 'presence pin)))
(define pinned-handlers (cdr (assq 'handlers pin)))
(define pinned-raw (cdr (assq 'raw-accessors pin)))
(define raw-record (cadr (assq 'record pinned-raw)))
(define raw-file (cadr (assq 'file pinned-raw)))
(define pinned-fields (cdr (assq 'fields pinned-raw)))
(define pinned-wrappers (cdr (assq 'wrappers pinned-raw)))

(define files (production-files))
(define texts (map text-of files))
(define-values (presence handlers symbols) (scan-tree files texts))

(define (forms-of-text rel text)
  (let ((p (open-string-input-port text)))
    (let loop ((out '()))
      (let ((x (read p)))
        (if (eof-object? x)
            (let ((fs (reverse out)))
              (apply append fs
                     (map (lambda (f) (if (and (pair? f) (eq? (car f) 'library) (list? f)) f '()))
                          fs)))
            (loop (cons x out)))))))

(define (allowed-raw-places)
  (apply append
    (map (lambda (w)
           (list (list raw-file (list raw-record) (car w))
                 (list raw-file (list (cadr w)) (car w))))
         pinned-wrappers)))

;; ---- the instrument's own first reading -------------------------------

(want "U11-00 the walk found the production sources, log.sc among them"
      (and (> (length files) 30) (member "log.sc" files) (member "mcp/server.sc" files) #t)
      #t)
(want "U11-00 the scan found predicates, handlers and the raw accessors at all"
      (list (> (length presence) 0) (> (length handlers) 100)
            (> (length (raw-accessors (or (record-fields
                                             (forms-of-text raw-file (text-of raw-file))
                                             raw-record)
                                           '())))
               0))
      '(#t #t #t))
(want "U11-00 every pinned entry has its fields, a known category and a reason"
      (pin-shape-problems pin) '())

;; ---- the three rows ----------------------------------------------------

(want "U11-i every presence or type predicate, per enclosing definition, is the pinned one"
      (presence-diff presence pinned-presence) '())

(want "U11-ii every exception handler, what it catches and its body, is the pinned one"
      (handler-diff handlers pinned-handlers) '())

(define log-forms (forms-of-text raw-file (text-of raw-file)))
(want "U11-iii the discovery record's fields are the pinned ones"
      (record-fields log-forms raw-record) pinned-fields)
(want "U11-iii every raw accessor has one pinned wrapper"
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 (raw-accessors (or (record-fields log-forms raw-record) '())))
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 (map car pinned-wrappers)))
(define (unguarded-wrappers forms)
  (filter (lambda (w)
            (not (member (list 'define (cadr w) (list 'guarded-coordinate (car w))) forms)))
          pinned-wrappers))
(want "U11-iii each wrapper is (guarded-coordinate raw)"
      (unguarded-wrappers log-forms)
      '())
(want "U11-iii the raw accessors appear only in the record and in their wrappers"
      (raw-diff symbols (map car pinned-wrappers) (allowed-raw-places)) '())

;; ---- the door (F100 D1) ------------------------------------------------
;;
;; KEY: EVERY FILESYSTEM ACCESS GOES THROUGH ffi.sc. This walks the same
;; production files as the three rows above, keeps the `.sc` ones other
;; than ffi.sc (the `.ss` build script is outside the door, and NOTES names
;; it), and is red on any occurrence of a native filesystem name as a
;; TOKEN: read as data, so strings and comments are not occurrences and a
;; name split across lines is one. A name an import gives a banned one
;; (prefix, rename) counts as the banned name, as row (i) counts them.
;; The list is the design's, exactly.
(define door-banned
  '(file-exists? delete-file rename-file directory-list mkdir file-directory?
    file-regular? file-modification-time file-change-time file-access-time
    open-file-input-port open-file-output-port open-file-input/output-port
    open-input-file open-output-file call-with-input-file call-with-output-file
    with-input-from-file with-output-to-file set-port-position! port-position
    get-mode chmod))
(define (door-file? rel) (and (suffix? rel ".sc") (not (string=? rel "ffi.sc"))))
(define (door-hits fs ts syms)
  (let ((aliases (map (lambda (f t)
                        (cons f (if (door-file? f)
                                    (map car (import-aliases (forms-of-text f t) door-banned))
                                    '())))
                      fs ts)))
    (filter (lambda (s) (let ((a (assoc (car s) aliases))) (and a (memq (caddr s) (cdr a)))))
            syms)))

(want "D1-door-00 the walk reads .sc files at any depth; ffi.sc and the .ss files it saw are outside the door"
      (list (and (member "mcp/server.sc" files) (door-file? "mcp/server.sc") #t)
            (and (member "ffi.sc" files) (door-file? "ffi.sc"))
            (filter (lambda (f) (suffix? f ".ss")) files))
      '(#t #f ("build.ss")))
(want "D1-door-01 no native filesystem name outside ffi.sc, as a token or by an imported name"
      (door-hits files texts symbols) '())

;; ---- the close census (F100a, ruling H1) --------------------------------
;;
;; KEY: EVERY CLOSE IN ffi.sc IS ACCOUNTED FOR. Review round 2 found a close
;; on the normal path swallowed in the one place round 1's fixes did not
;; reach (lock-try-acquire!'s contention answer), so the list is read, not
;; remembered: every call of c-close, c-closedir, close-quietly, the lock's
;; release through (current-lock-release), and the unlock (c-flock with
;; LOCK_UN), by enclosing definitions and ordinal, with its line for the
;; reader. Review round 3 found closes the list did not name, so it also
;; takes a Chez port's close (close-port, close-input-port,
;; close-output-port), the descriptor's own close (fd-close), and the forms
;; that close what they open on return: call-with-input-file,
;; call-with-output-file, with-input-from-file, with-output-to-file and
;; call-with-port (a form of one of those names is a site of its enclosing
;; definition; the last three names of the port family were added by
;; ruling J with no site in ffi.sc). Each is pinned in the .sexp's `close`
;; form with ONE of four categories:
;;   checked     the normal path's close, whose failure raises (read-closing,
;;               close-unwritten-port!, the written close, call-with-lock's
;;               checked release, lock-release!, the contention answer);
;;   escape      a quiet close that runs only when a failure is already on
;;               its way out (an unwind after-thunk, a guard that re-raises);
;;   contention  an answer path whose close is not checked -- none since the
;;               ruling made the contention close checked; kept so a pin can
;;               say so if one appears.
;;   out-of-scope  a close of a process, socket or stdio descriptor that
;;               D1's scope exclusion leaves as it is, or of something that
;;               is not a store entry (the injection FIFO, /proc); allowed
;;               only inside the definitions D1 puts outside the door (the
;;               row below pins that list, so a filesystem close cannot
;;               take it).
;; A site with no pin, or a pin with no site, is red; the counts are equal.
;; The census counts occurrences, not behaviour: a close whose normal-path
;; check is dropped keeps its site. The close-fail rows of facade-ffi are
;; the behaviour reading (F100a review r3, ruling I).
(define close-callees
  '(c-close c-closedir close-quietly current-lock-release close-port fd-close
    close-input-port close-output-port
    call-with-input-file call-with-output-file with-input-from-file with-output-to-file
    call-with-port))
(define close-categories '(checked escape contention out-of-scope))
(define close-out-of-scope-definitions
  '(above-stdio redirect-stdio! unix-socket-connect spawn-detached! barrier! rss-linux))
(define (close-sites rel text)
  (let* ((sfd (make-source-file-descriptor rel (open-bytevector-input-port (string->utf8 text))))
         (p (open-string-input-port text))
         (annotated (let loop ((bfp 0) (out '()))
                      (let-values (((a nb) (get-datum/annotations p sfd bfp)))
                        (if (eof-object? a) (reverse out) (loop nb (cons a out))))))
         (line-of (lambda (bfp)
                    (let loop ((i 0) (n 1))
                      (cond ((>= i bfp) n)
                            ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
                            (else (loop (+ i 1) n))))))
         (ordinals (make-hashtable equal-hash equal?))
         (out '()))
    (define (site! defs callee x)
      (let* ((k (list defs callee))
             (n (+ 1 (hashtable-ref ordinals k 0))))
        (hashtable-set! ordinals k n)
        (set! out (cons (list defs callee n
                              (if (annotation? x) (line-of (source-object-bfp (annotation-source x))) 0))
                        out))))
    ;; EVERY REFERENCE, NOT ONLY A CALL: `(for-each c-close held)` passes the
    ;; close as a value, and a walk of call heads did not see it. A
    ;; definition's own name -- (define (close-quietly fd) ...), (define
    ;; c-close ...) -- is not a site.
    (for-each
      (lambda (a)
        (let walk ((x a) (defs '()))
          (let ((d (unwrap x)))
            (cond
              ((symbol? d)
               (when (memq d close-callees) (site! defs d x)))
              ((and (pair? d) (memq (unwrap (car d)) '(export import)))
               ;; A DECLARATION, NOT A USE: the library's export and import
               ;; lists name a close without calling or passing it.
               (void))
              ((pair? d)
               ;; A PLAIN LIST (the parts of a definition left after its
               ;; header) has no definition name of its own; it is walked into.
               (let* ((sd (and (annotation? x) (annotation-stripped x)))
                      (nm (and sd (defined-name sd)))
                      (defs2 (if nm (append defs (list nm)) defs))
                      (head (unwrap (car d)))
                      (body (if (and sd (memq head '(define define-syntax)) (pair? (unwrap (cdr d))))
                                (let ((target (unwrap (car (unwrap (cdr d))))))
                                  (if (pair? target)
                                      (cons (cdr target) (cdr (unwrap (cdr d))))
                                      (cdr (unwrap (cdr d)))))
                                d)))
                 (when (and sd (eq? head 'c-flock) (list? sd) (memq 'LOCK_UN sd))
                   (site! defs2 'unlock x))
                 (let lp ((l body))
                   (let ((l (unwrap l)))
                     (cond ((pair? l) (walk (car l) defs2) (lp (cdr l)))
                           ((symbol? l) (walk l defs2))
                           (else (void)))))))
              (else (void))))))
      annotated)
    (reverse out)))
(define pinned-close (cdr (assq 'close pin)))
(define (close-diff sites pins)
  (let ((keys (map (lambda (s) (list-head s 3)) sites))
        (pkeys (map (lambda (p) (list-head p 3)) pins)))
    (append
      (map (lambda (s) (list 'unpinned s))
           (filter (lambda (s) (not (member (list-head s 3) pkeys))) sites))
      (map (lambda (p) (list 'no-site (list-head p 3)))
           (filter (lambda (p) (not (member p keys))) pkeys)))))
(define ffi-text (text-of "ffi.sc"))
(define ffi-close-sites (close-sites "ffi.sc" ffi-text))
(want "D1-close-00 the close census found close sites in ffi.sc: a reader's, the unlock, and a close passed as a value"
      (list (> (length ffi-close-sites) 5)
            (and (find (lambda (s) (equal? (list-head s 2) '((read-entry/errno) c-close))) ffi-close-sites)
                 (find (lambda (s) (equal? (list-head s 2) '((lock-release!) unlock))) ffi-close-sites)
                 (find (lambda (s) (equal? (list-head s 2) '((above-stdio) c-close))) ffi-close-sites)
                 #t))
      '(#t #t))
(want "D1-close-01 every close site in ffi.sc is pinned, every pin has its site, and the counts are equal"
      (list (close-diff ffi-close-sites pinned-close)
            (= (length ffi-close-sites) (length pinned-close)))
      '(() #t))
(want "D1-close-03 out-of-scope is taken only inside the definitions D1 puts outside the door"
      (filter (lambda (p) (and (eq? (list-ref p 3) 'out-of-scope)
                               (not (and (pair? (car p))
                                         (exists (lambda (d) (memq d close-out-of-scope-definitions)) (car p))))))
              pinned-close)
      '())
(want "D1-close-02 every close pin has one of the four categories and a reason"
      (filter (lambda (p) (not (and (= (length p) 5) (memq (list-ref p 3) close-categories)
                                    (string? (list-ref p 4)) (> (string-length (list-ref p 4)) 0))))
              pinned-close)
      '())

;; ---- each row, against a copy of log.sc with one thing changed ---------
;;
;; The copy is a string; nothing is written. The change goes before the
;; library form's closing parenthesis, so it is read as a definition of
;; the library like any other.

(define log-text (text-of raw-file))
(define (with-log-text new)
  (map (lambda (f t) (if (string=? f raw-file) new t)) files texts))
(define (insert-before-close text form)
  (let loop ((i (- (string-length text) 1)))
    (cond ((< i 0) (assertion-violation 'insert-before-close "no closing parenthesis"))
          ((char=? (string-ref text i) #\))
           (string-append (substring text 0 i) "\n  " form "\n" (substring text i (string-length text))))
          (else (loop (- i 1))))))

(define-values (p1 h1 s1)
  (scan-tree files (with-log-text
                     (insert-before-close log-text "(define (census-probe p) (file-exists? p))"))))
(want "U11-i NEGATIVE: a predicate use added to log.sc is the one difference"
      (presence-diff p1 pinned-presence)
      (list (list 'unpinned (list raw-file '(census-probe) 'file-exists?) 1)))

;; AN IMPORT CAN GIVE A PREDICATE ANOTHER NAME, and a use under that name
;; spells no predicate. The copy's library imports one under `rename` and
;; one under `prefix`, and uses each.
(define (with-extra-import text spec)
  (let ((at "(import (chezscheme)"))
    (let loop ((i 0))
      (cond ((> (+ i (string-length at)) (string-length text))
             (assertion-violation 'with-extra-import "no import clause" at))
            ((string=? at (substring text i (+ i (string-length at))))
             (string-append (substring text 0 (+ i (string-length at)))
                            " " spec
                            (substring text (+ i (string-length at)) (string-length text))))
            (else (loop (+ i 1)))))))

(define-values (p5 h5 s5)
  (scan-tree files (with-log-text
                     (insert-before-close
                       (with-extra-import log-text "(rename (chezscheme) (file-exists? present?))")
                       "(define (census-renamed p) (present? p))"))))
(want "U11-i NEGATIVE: a predicate imported under another name, and used by it, is the difference"
      (presence-diff p5 pinned-presence)
      (list (list 'unpinned (list raw-file '() 'file-exists?) 1)
            (list 'unpinned (list raw-file '() 'present?) 1)
            (list 'unpinned (list raw-file '(census-renamed) 'present?) 1)))

(define-values (p6 h6 s6)
  (scan-tree files (with-log-text
                     (insert-before-close
                       (with-extra-import log-text "(prefix (chezscheme) cs:)")
                       "(define (census-prefixed p) (cs:file-exists? p))"))))
(want "U11-i NEGATIVE: a predicate reached through a prefixed import is the one difference"
      (presence-diff p6 pinned-presence)
      (list (list 'unpinned (list raw-file '(census-prefixed) 'cs:file-exists?) 1)))

(define-values (p2 h2 s2)
  (scan-tree files (with-log-text
                     (insert-before-close
                       log-text
                       "(define (census-swallow p) (guard (e ((unreadable-entry? e) #f)) (read-entry p)))"))))
(want "U11-ii NEGATIVE: a handler that catches only unreadable-entry and answers #f is the one difference"
      (handler-diff h2 pinned-handlers)
      (list (list 'unpinned (list raw-file '(census-swallow) 1) '((unreadable-entry? e)))))

;; THE FLIP: the first pinned PROPAGATE guard in log.sc with one clause
;; whose body is not already #f and holds no handler of its own -- removing
;; a nested guard would renumber the handlers after it, and the row would
;; report that instead. Its body in the copy becomes #f; what it catches
;; does not change, and its category in the pin does not change.
(define (mentions? sym x)
  (cond ((eq? x sym) #t)
        ((pair? x) (or (mentions? sym (car x)) (mentions? sym (cdr x))))
        ((vector? x) (mentions? sym (vector->list x)))
        (else #f)))
(define flip-target
  (find (lambda (h)
          (let ((p (assoc (handler-key h) (map (lambda (e) (cons (handler-key e) e)) pinned-handlers))))
            (and (string=? (car h) raw-file) (eq? (list-ref h 3) 'guard)
                 p (eq? (list-ref (cdr p) 5) 'propagate)
                 (= (length (list-ref h 6)) 1) (car (list-ref h 6))
                 (not (equal? (cdr (cadr (list-ref h 5))) '(#f)))
                 (not (exists (lambda (k) (mentions? k (cdr (cadr (list-ref h 5)))))
                              handler-heads)))))
        handlers))
(want "U11-ii NEGATIVE: there is a PROPAGATE handler in log.sc to flip"
      (and flip-target #t) #t)
(when flip-target
  (let* ((span (car (list-ref flip-target 6)))
         (flipped (string-append (substring log-text 0 (car span)) "#f"
                                 (substring log-text (cdr span) (string-length log-text)))))
    (let-values (((p3 h3 s3) (scan-tree files (with-log-text flipped))))
      (printf "     flipped: ~s\n" (handler-key flip-target))
      (want "U11-ii NEGATIVE: a PROPAGATE handler whose body became #f is the one difference"
            (handler-diff h3 pinned-handlers)
            (list (list 'body (handler-key flip-target)))))))

(define-values (p4 h4 s4)
  (scan-tree files (with-log-text
                     (insert-before-close log-text "(define (census-raw d) (raw-end-seq d))"))))
(want "U11-iii NEGATIVE: a raw accessor used outside its wrapper is the one difference"
      (raw-diff s4 (map car pinned-wrappers) (allowed-raw-places))
      (list (list 'outside-a-wrapper (list raw-file '(census-raw) 'raw-end-seq) 1)))

;; ---- the negatives for what the first review found unexercised ------------
;;
;; Each is a copy of log.sc with one change, and the row asks for exactly
;; the difference that change makes.

(define (replace-once text from to)
  (let ((n (string-length from)))
    (let loop ((i 0) (hit #f))
      (cond
        ((> (+ i n) (string-length text))
         (unless hit (assertion-violation 'replace-once "text not found" from))
         (string-append (substring text 0 hit) to
                        (substring text (+ hit n) (string-length text))))
        ((string=? from (substring text i (+ i n)))
         (when hit (assertion-violation 'replace-once "text found twice" from))
         (loop (+ i 1) i))
        (else (loop (+ i 1) hit))))))

(define-values (p7 h7 s7)
  (scan-tree files (with-log-text
                     (insert-before-close
                       (with-extra-import log-text "(prefix (chezscheme) cs:)")
                       "(define (census-cs p) (cs:guard (e (#t #f)) (read-entry p)))"))))
(want "U11-ii NEGATIVE: a guard reached through a prefixed import is the one difference"
      (handler-diff h7 pinned-handlers)
      (list (list 'unpinned (list raw-file '(census-cs) 1) '(#t))))

(define-values (p8 h8 s8)
  (scan-tree files (with-log-text
                     (insert-before-close
                       (with-extra-import log-text "(prefix (prefix (chezscheme) inner:) outer:)")
                       "(define (census-nested p) (outer:inner:file-exists? p))"))))
(want "U11-i NEGATIVE: a predicate reached through a prefix of a prefix is the one difference"
      (presence-diff p8 pinned-presence)
      (list (list 'unpinned (list raw-file '(census-nested) 'outer:inner:file-exists?) 1)))

(define-values (p9 h9 s9)
  (scan-tree files (with-log-text
                     (insert-before-close log-text "#!chezscheme (define (census-box) (unbox '#&(guard (e (#t #f)) 1)))"))))
(want "U11-ii NEGATIVE: a guard inside a box datum is the one difference"
      (handler-diff h9 pinned-handlers)
      (list (list 'unpinned (list raw-file '(census-box) 1) '(#t))))

;; (The box needs the reader switched to Chez's own syntax, as a source file
;; may switch it; log.sc begins #!r6rs.)

;; A PINNED OCCURRENCE COUNTED TWICE. ensure-machine-home! asks
;; file-is-directory? of the machine home once; the copy asks it twice in the
;; same definition. (It was open-load's file-exists? of meta.sexp until F100a
;; moved every presence test through the door and left log.sc none.)
(define-values (p10 h10 s10)
  (scan-tree files (with-log-text
                     (replace-once log-text "(unless (file-is-directory? home)"
                                   "(unless (and (file-is-directory? home) (file-is-directory? home))"))))
(want "U11-i NEGATIVE: a pinned predicate use made twice is a changed count, the one difference"
      (presence-diff p10 pinned-presence)
      (let ((e (find (lambda (e) (equal? (list-head e 3) (list raw-file '(ensure-machine-home!) 'file-is-directory?)))
                     pinned-presence)))
        (list (list 'count (list raw-file '(ensure-machine-home!) 'file-is-directory?)
                    (cadddr e) (+ 1 (cadddr e))))))

(define-values (p11 h11 s11)
  (scan-tree files (with-log-text
                     (replace-once log-text "(define discovery-torn (guarded-coordinate raw-torn))"
                                   "(define discovery-torn (begin raw-torn (guarded-coordinate raw-torn)))"))))
(want "U11-iii NEGATIVE: a raw accessor used twice at an allowed place is a changed count, the one difference"
      (raw-diff s11 (map car pinned-wrappers) (allowed-raw-places))
      (list (list 'count (list raw-file '(discovery-torn) 'raw-torn) 2)))

(want "U11-iii NEGATIVE: a wrapper that stops guarding its accessor is the one unguarded wrapper"
      (unguarded-wrappers
        (forms-of-text raw-file (replace-once log-text "(define discovery-end-seq (guarded-coordinate raw-end-seq))"
                                              "(define discovery-end-seq raw-end-seq)")))
      '((raw-end-seq discovery-end-seq)))

(want "U11-iii NEGATIVE: a field added to the discovery record is the one field not pinned"
      (let ((fields (record-fields
                      (forms-of-text raw-file (replace-once log-text "(immutable torn raw-torn)"
                                                            "(immutable torn raw-torn) (immutable extra raw-extra)"))
                      raw-record)))
        (filter (lambda (f) (not (member f pinned-fields))) fields))
      '((immutable extra raw-extra)))

;; ---- the door row, against a copy of log.sc with one thing changed ----
(define (door-diff new-log-text)
  (let ((ts (with-log-text new-log-text)))
    (let-values (((p h s) (scan-tree files ts)))
      (door-hits files ts s))))
(want "D1-door NEGATIVE: a native name used in log.sc is the one hit"
      (door-diff (insert-before-close log-text "(define (door-probe p) (file-exists? p))"))
      (list (list raw-file '(door-probe) 'file-exists?)))
(want "D1-door NEGATIVE: a native name split across lines is one hit"
      (door-diff (insert-before-close log-text "(define (door-split p)\n  (call-with-input-file\n    p get-string-all))"))
      (list (list raw-file '(door-split) 'call-with-input-file)))
(want "D1-door NEGATIVE: a native name reached through a prefixed import is a hit where it is used"
      (and (member (list raw-file '(door-prefixed) 'host:delete-file)
                   (door-diff (insert-before-close
                                (with-extra-import log-text "(prefix (only (chezscheme) delete-file) host:)")
                                "(define (door-prefixed p) (host:delete-file p))")))
           #t)
      #t)
(want "D1-door CONTROL: a native name in a string or a comment is not a hit"
      (door-diff (insert-before-close log-text "(define (door-quiet) \"file-exists?\")\n  ;; delete-file"))
      '())

;; ---- the close census, against a copy of ffi.sc with one close added ---
(want "D1-close NEGATIVE: a close passed as a value is a site, and unpinned"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe-value fds) (for-each c-close fds))"))
                           pinned-close))
      '(unpinned))
(want "D1-close NEGATIVE: a quiet close added to ffi.sc is the one unpinned site"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe fd) (close-quietly fd))"))
                           pinned-close))
      '(unpinned))
(want "D1-close NEGATIVE: a bare Chez port close added to ffi.sc is the one unpinned site"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe-port p) (close-port p))"))
                           pinned-close))
      '(unpinned))
(want "D1-close NEGATIVE: a form that closes what it opens, added to ffi.sc, is the one unpinned site"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe-implicit p) (with-output-to-file p newline))"))
                           pinned-close))
      '(unpinned))
(want "D1-close NEGATIVE: an input port close added to ffi.sc is the one unpinned site"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe-close-input-port p) (close-input-port p))"))
                           pinned-close))
      '(unpinned))
(want "D1-close NEGATIVE: an output port close added to ffi.sc is the one unpinned site"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe-close-output-port p) (close-output-port p))"))
                           pinned-close))
      '(unpinned))
(want "D1-close NEGATIVE: a call-with-port added to ffi.sc is the one unpinned site"
      (map car (close-diff (close-sites "ffi.sc" (insert-before-close ffi-text "(define (close-probe-call-with-port p) (call-with-port p get-u8))"))
                           pinned-close))
      '(unpinned))

(printf "\n~a failures\nrows: ~a\nunreadable-census complete\n" bad rows)
