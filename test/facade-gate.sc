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

;; ONE SEAM TO IGROPYR, AND THIS COUNTS IT.
;;
;; The rule is that every use of igropyr goes through a facade of this
;; core's own, so that replacing the dependency -- or vendoring it again,
;; as Z did -- touches those files and no caller. A rule kept in
;; somebody's head is a rule until the first hurry; this reads the tree.
;;
;; NEVER: IT DOES NOT GREP, IT READS EACH FILE AS DATA -- and the walk that
;; does so lives in `import-walk.sc`, beside this file, because
;; `closures.sc` needs the same one. Why it cannot be a grep, and why it
;; is `load`ed rather than `include`d, are written there.
;;
;; WHAT IT WALKS INTO. An import may name a library directly or wrap it
;; in `only` / `except` / `rename` / `prefix`, and `meta-cond` puts whole
;; import lists behind a branch that this host may not take -- an import
;; in the branch that is NOT taken here is still a use of igropyr in this
;; source, so the walk goes into every branch rather than evaluating any.
;;
;; FOUR DIRECTIONS, WHICH IS WHAT MAKES THE SET OF RULES CLOSED:
;;
;;   a name in facades.sexp  -> that root file exists
;;   a name in facades.sexp  -> it really does import igropyr
;;   a root file imports it  -> its name is in facades.sexp
;;   a name in facades.sexp  -> no copied definition left in it  (facades.sc)
;;
;; The first three are here. Two sides -- the list and the tree -- and
;; both directions of each; there is no fifth way for them to disagree.

(import (chezscheme)
        ;; NEVER: THE GATE ASKS THE PRODUCT WHAT THE TABLE IS. Writing the
        ;; kinds out here would give the tree a second copy of the very set
        ;; whose uniqueness is the question, and this file would then agree
        ;; with itself for as long as nobody changed both.
        (only (theourgia reduce) known-kinds block-id state-datum reduce-applied-cut)
        (only (theourgia project) md-kinds)
        ;; and the third rule, whose shape this census cannot see: it is the
        ;; key set of a dispatch table, not a quoted list, so it is pinned by
        ;; value in its own row below.
        (only (theourgia store) name-bearing-kinds open-and-reduce store-evidence)
        ;; What the run-time census at the end of this file needs to make each
        ;; verb answer: the dispatcher, and the pieces restore's and publish's
        ;; own setups are built from.
        (only (theourgia rpc) rpc-dispatch rpc-ok? rpc-verbs)
        (only (theourgia ffi) mkdir-p!)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia request) ev-actor ev-payload actor-sub)
        (only (theourgia code-project) code-field))

(define failures 0)
(define rows 0)
(define (want-1 name actual expected)
  (set! rows (+ rows 1))
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
;; BOTH SIDES OF A ROW ARE GUARDED. A `want` written as a procedure
;; evaluates its arguments before the call, so an argument that raises
;; kills the fixture: the row never prints, and what the suite sees is a
;; missing sentinel rather than a red row naming the question that could
;; not be answered. This directory's `run-fixtures.sh` counts fixtures
;; whose `want` is a bare procedure for exactly that reason.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

;; THE ROOT IS FOUND FROM THIS SCRIPT, not from the current directory:
;; the runner starts every fixture from `test/`.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/core.sc")) up script-dir)))

(define facade-names
  (call-with-input-file (string-append script-dir "/facades.sexp") read))

(load (string-append script-dir "/import-walk.sc"))

(define (uses-igropyr? name)
  (pair? (imports-of-file 'igropyr (string-append root "/" name))))

;; ---- the scan reaches into subdirectories, and it has to ---------------
;;
;; NEVER: A NON-RECURSIVE SCAN HAS A DOOR IN IT. `source-files` lists one
;; directory; a root library that imports a nested helper, and lets THAT
;; file import igropyr, leaves the set of root importers unchanged. All
;; four directions stay satisfied and the seam is gone. Nothing in this
;; tree does that today -- which is exactly when a hole is cheap to
;; close.
;;
;; NOTE: `test/` IS OUT OF SCOPE, ON PURPOSE. Fixtures are not the library:
;; some of them import igropyr in order to MEASURE it (`q10`, `cli3`),
;; and requiring them to go through a facade would mean measuring the
;; facade instead of the thing. The rule is about what the core ships.
;; NOTE: THE RECURSION CARRIES ITS DIRECTORY. Written as a named `let` over
;; entries alone, the inner call went back into the loop with the
;; SUBDIRECTORY's entries and the OUTER directory's path -- every nested
;; name was joined to the wrong parent, and the run stopped answering.
;; Measured: it had to be killed. A walker that takes the directory as
;; an argument cannot make that mistake.
;; NOTE: THE SAME EXTENSIONS THE SHARED WALKER KNOWS. This listed `.ss`
;; alone while `import-walk.sc` recognises `.ss`, `.sls`, `.sc` and
;; `.scm` -- so a nested `helper.sls` importing igropyr passed the deep
;; scan untouched, which is the hole this scan was added to close. One
;; list, read from the walker's own definition.
(define (source-suffix? name)
  (exists (lambda (suffix)
            (let ((n (string-length name)) (k (string-length suffix)))
              (and (> n k) (string=? suffix (substring name (- n k) n)))))
          source-suffixes))

(define (sources-under dir)
  (let loop ((entries (directory-list dir)) (out '()))
    (cond
      ((null? entries) out)
      ((member (car entries) '("." ".." "test" ".git")) (loop (cdr entries) out))
      (else
       (let ((path (string-append dir "/" (car entries))))
         (cond
           ;; NEVER: SYMLINKS ARE NOT DESCENDED, AND THE REASON IS IN THIS
           ;; DIRECTORY. The checkout carries a self-link -- `theourgia`
           ;; pointing at `.`, so that `(theourgia x)` resolves from the
           ;; tree itself -- and a walk that follows it re-enters the
           ;; same directory for ever. Measured twice: the first version
           ;; had to be killed, the second reported 198 importers for a
           ;; tree with six, which is the same fault with a bound on it.
           ((and (file-directory? path) (not (file-symbolic-link? path)))
            (loop (cdr entries) (append (sources-under path) out)))
           ((source-suffix? (car entries)) (loop (cdr entries) (cons path out)))
           (else (loop (cdr entries) out))))))))

(define scanned (source-files root))
(define importers (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                             (map stem (filter uses-igropyr? scanned))))
(define declared (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                            facade-names))

;; THE INSTRUMENT'S OWN FIRST READING. A walk that found nothing would
;; make the comparison below vacuous, and "no file imports igropyr" is
;; exactly what a broken reader produces.
(want "FG-00 the scan reached the root sources"
      (> (length scanned) 20) #t)
(want "FG-00 and it found imports to compare at all"
      (> (length importers) 0) #t)

(want "FG-01 the files importing igropyr are exactly the declared facades"
      importers declared)

;; KEY: THE SAME RULE, ASKED OF EVERY SOURCE THE CORE SHIPS. Root files are
;; the population above; this one is "any file at all, however deep".
;; With the tree flat they agree -- and the day somebody adds a
;; subdirectory, this is the row that keeps the answer true.
(define deep-importers
  (list-sort
    string<?
    (map (lambda (p) (let* ((cut (let loop ((i (- (string-length p) 1)))
                                   (cond ((< i 0) -1)
                                         ((char=? (string-ref p i) #\/) i)
                                         (else (loop (- i 1))))))
                            (base (substring p (+ cut 1) (string-length p))))
                       base))
         (filter (lambda (p) (pair? (imports-of-file 'igropyr p)))
                 (sources-under root)))))

(want "FG-03 no source outside the declared facades imports igropyr, at any depth"
      (filter (lambda (b) (not (memq (string->symbol (substring b 0 (- (string-length b) 3)))
                                     facade-names)))
              deep-importers)
      '())

;; NOTE: AND THE DEEP SCAN REALLY LOOKED. A walker that found nothing would
;; satisfy the row above by finding no importers at all.
(want "FG-03 TWIN: the deep scan found the facades themselves"
      (length deep-importers) (length facade-names))

(want "FG-02 every declared facade exists as a root source"
      (filter (lambda (n) (not (file-exists? (string-append root "/" (symbol->string n) ".sc"))))
              facade-names)
      '())

;; ---- one supplier for the count in a partial answer ------------------------
;;
;; KEY: FOUR ROUTES CAN ANSWER `(batch <items> (done n))` -- the `batch` verb,
;; a plan completed from an earlier request, a commit over several blocks, and
;; the two imports. If each assembled the shape itself, `n` and the list would
;; be two suppliers of one fact and free to disagree. They all call
;; `batch-answer`, where the count is a FUNCTION of the list beside it.
;;
;; NEVER: THIS CENSUS ASKS ABOUT `done`, NOT ABOUT `batch`. The tag `batch`
;; means two different things in this tree -- the answer, and a request
;; identity `(batch <req> k)` written into a plan -- so a census on it counts
;; things that have nothing to do with each other. `done` has one meaning.
;;
;; NEVER: AND THE READING HAS TO BE ONE, NOT ZERO. A pattern that matches
;; nothing would satisfy "no second supplier" while proving only that the
;; census cannot see. The second row names where the one hit lives, so
;; renaming the constructor turns THIS row red as well.

(define (last-slash p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) -1)
          ((char=? (string-ref p i) #\/) i)
          (else (loop (- i 1))))))

(define (contains-text? text needle)
  (let ((n (string-length text)) (k (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i k) n) #f)
            ((string=? (substring text i (+ i k)) needle) #t)
            (else (loop (+ i 1)))))))

(define (read-forms path)
  (call-with-port (open-input-file path)
    (lambda (port)
      (let loop ((out (quote ())))
        (let ((d (read port)))
          (if (eof-object? d) (reverse out) (loop (cons d out))))))))

;; NEVER: THE CENSUS READS DATA, NOT TEXT. The text version needed a new
;; pattern every time somebody found another way to write the same thing --
;; `(list 'done n)`, `(cons 'done n)`, a backquoted `(done ,n)`, `(quote done)`,
;; a newline where a space was expected, and it counted the constructor when it
;; appeared inside a COMMENT. Each patch was a guard whose model of the
;; language was smaller than the language. `read` is the language: it discards
;; comments and whitespace and turns every quotation form into the same
;; structure, so there is one shape to recognise instead of a list of spellings.
;;
;; NOTE: WHICH IS ALSO WHY THIS IS NOT A COUNT OF THE SYMBOL `done`. `ffi.sc`
;; has `(set-box! fault-state 'done)`, and a census of the symbol would report
;; it. Neither shape below matches: its head is `set-box!`, and it is not a
;; list beginning with `done`.
(define (builds-done-clause? form quoted?)
  (and (pair? form)
       (or
         ;; (list 'done …) or (cons 'done …) -- after `read`, `'done` is
         ;; `(quote done)` however it was spelled
         ;; NOTE: AND WHAT THIS DOES NOT RECOGNISE IS WRITTEN DOWN. `list`,
         ;; `cons` and `list*` are the constructors this tree uses; a
         ;; `(cons `done …)` or a `` `(,'done ,1) `` would build the clause and
         ;; not be counted. THE CENSUS PINS THE SPELLINGS IT KNOWS, and is not
         ;; a proof that construction is unique -- the rows in `cli1.sc` are
         ;; what say the answers are right.
         (and (memq (car form) (quote (list cons list*)))
              (pair? (cdr form))
              (let ((x (cadr form)))
                (and (pair? x) (eq? (car x) (quote quote))
                     (pair? (cdr x)) (eq? (cadr x) (quote done)))))
         ;; (done …) as data -- but ONLY inside a quotation. A list whose head
         ;; is `done` is also what an ordinary local binding looks like after
         ;; `read`: measured, `(let ((done (box #f))) …)` appears in `ffi.sc`
         ;; twice, in `wire.sc` and in `log.sc`, and counting those took the
         ;; census from 1 to 5. Inside a quasiquote the same shape IS the
         ;; clause, so the flag is the whole difference.
         (and quoted? (eq? (car form) (quote done))))))

;; The name of the definition a hit sits inside, taken from the datum. The
;; text version searched the FILE for `(define (batch-answer`, which a comment
;; could satisfy.
;; THE TRAVERSAL TAKES THE QUESTION AS AN ARGUMENT. There are two censuses
;; in this file now and there is one walk: a second copy of this would be a
;; second instrument, free to drift from the first and to be right about a
;; different tree.
(define (hits-in pred form enclosing quoted?)
  (cond
    ((not (pair? form)) (quote ()))
    (else
     ;; BOTH SPELLINGS OF `define` NAME A DEFINITION. A table is bound
     ;; with `(define known-kinds '(…))`, whose cadr is the symbol
     ;; itself; reading only the procedure spelling reported `#f` for
     ;; it, and a twin row that says which definition a hit is inside
     ;; would then have been unable to say anything about exactly the
     ;; kind of definition a table is.
     (let* ((name (cond
                    ((not (eq? (car form) (quote define))) enclosing)
                    ((not (pair? (cdr form))) enclosing)
                    ((symbol? (cadr form)) (cadr form))
                    ((and (pair? (cadr form)) (symbol? (car (cadr form))))
                     (car (cadr form)))
                    (else enclosing)))
            ;; NEVER: AND `unquote` LEAVES THE QUOTATION. Inside a
            ;; quasiquote an unquoted expression is ordinary code again, so
            ;; `,(let ((done (box #f))) …)` would otherwise be counted -- the
            ;; same local-binding false positive the quotation flag exists to
            ;; prevent, reintroduced one level in.
            (inside? (cond
                       ((memq (car form) (quote (unquote unquote-splicing))) #f)
                       ((memq (car form) (quote (quasiquote quote))) #t)
                       (else quoted?)))
            (here (if (pred form quoted?) (list name) (quote ()))))
       (let loop ((xs form) (acc here))
         (cond
           ((pair? xs) (loop (cdr xs) (append acc (hits-in pred (car xs) name inside?))))
           ((null? xs) acc)
           (else (append acc (hits-in pred xs name inside?)))))))))

(define (census pred)
  (let loop ((fs (sources-under root)) (hits (quote ())))
    (if (null? fs)
        hits
        (let ((found (apply append
                            (map (lambda (f) (hits-in pred f #f #f))
                                 (read-forms (car fs))))))
          (loop (cdr fs)
                (if (null? found)
                    hits
                    (cons (cons (car fs) found) hits)))))))

;; NEVER: AND THE ONE STRUCTURAL GUARANTEE OF THIS BATCH GETS AN INSTRUMENT,
;; NOT A GREP. A block that cannot be read must take only itself down, and the
;; way that is now true is that `store.sc` reads a block in exactly ONE place,
;; under a guard. That property was established by grepping once, by hand --
;; which means the next round would have inherited a comment claiming it and
;; nothing able to notice a second reader appearing.
;;
;; The census walks FORMS, so this counts CALLS BY NAME: a form whose car is
;; the symbol `view-read`. The import, where it is a bare symbol in a list,
;; does not match.
;;
;; NEVER: AND "A SECOND CALL SITE ANYWHERE REDS THIS ROW" IS WIDER THAN THE
;; ROW. That is what this comment used to say. What the row sees is a call that
;; NAMES it. A reviewer supplied the shapes it does not see, and measured them:
;;
;;     (quasiquote #((unquote (view-read state id))))    one call, counted 0
;;     ((if #t view-read other) state id)                one call, counted 0
;;
;; and macro expansion, `include`, binding identity and where a guard sits are
;; all outside its view. **None of those shapes exists in this tree today** --
;; that was checked, and `store.sc`'s direct-call count is 1.
;;
;; The row is kept as it is rather than made cleverer. What it is for is the
;; likely accident -- somebody adds `(view-read ...)` to a function that needs
;; a field -- and it catches that. Saying so exactly is the difference between
;; an instrument and a comfort.
;;
;; The other files that read blocks are named rather than counted, so a NEW
;; file starting to do it is also visible here. The list was derived by reading
;; the sources, not recalled: a first draft of this row named two files from
;; memory and missed two, which is the failure this row exists to catch,
;; committed while writing it.
;;
;; NEVER: AND A DEFINITION'S HEAD IS NOT A CALL. `(define (view-read r id) ...)`
;; contains the form `(view-read r id)`, so a census that walks forms counts
;; the definition in `view.sc` as a use of itself. The first version of this
;; row listed `view.sc` and explained that in a comment -- which meant the row
;; was only correct for a reader who had read the comment, AND that a real call
;; appearing inside `view.sc` would have been absorbed by the entry already
;; there.
;;
;; So the definitions are counted separately and subtracted. `view.sc` then has
;; zero real calls and does not appear at all; if it ever calls its own
;; `view-read` from somewhere else, it appears, and this row goes red. Nothing
;; here needs a comment to be read correctly.
;;
;; What the three are:
;;   store.sc        the one guarded read, which is the property above
;;   rpc.sc          rendering -- the outline's label and the `read` verb
;;   code-project.sc the code projection, which needs the DERIVED name
;;                   because a code block's name is not in its stored fields
;;
;; NEITHER of the other two readers is guarded, and `code-project.sc`'s walks
;; every block in the store. That is recorded as a finding rather than fixed
;; here: whether a block that cannot be read should be labelled by its id in
;; the outline, or make `export-code` REFUSE rather than quietly write one file
;; fewer, are product decisions and not this row's business. What this row
;; guarantees is that nobody adds a fourth reader quietly.
(define (calls-view-read? form quoted?)
  (and (not quoted?) (pair? form) (eq? (car form) 'view-read)))

(define (defines-view-read? form quoted?)
  (and (not quoted?) (pair? form) (eq? (car form) 'define)
       (pair? (cdr form)) (pair? (cadr form))
       (eq? (car (cadr form)) 'view-read)))

(define (per-file hits)
  (map (lambda (h)
         (let ((q (car h)))
           (cons (substring q (+ 1 (last-slash q)) (string-length q))
                 (length (cdr h)))))
       hits))

(define view-read-hits
  (let ((calls (per-file (census calls-view-read?)))
        (defs (per-file (census defines-view-read?))))
    (filter (lambda (e) (> (cdr e) 0))
            (map (lambda (e)
                   (let ((d (assoc (car e) defs)))
                     (cons (car e) (- (cdr e) (if d (cdr d) 0)))))
                 calls))))

(want "S-B1 the store reads a block in exactly one place, and these are all the readers"
      (list-sort (lambda (a b) (string<? (car a) (car b))) view-read-hits)
      '(("code-project.sc" . 1) ("rpc.sc" . 3) ("store.sc" . 1)))

;; ---- S-B2b no two places construct the same item tag ----------------------
;;
;; WHY A TAG IS LOAD-BEARING. An answer's items carry a tag and the tag is
;; the only thing that says which question was asked. `search` answers
;; `(hit <id> <score> <snippet>)` and `grep` answers
;; `(match <id> <line> <text>)`: same arity, same types in the same places.
;; The plugin's reader keys on the tag and then checks only that an entry has
;; at least four elements with a string second and an integer third -- which
;; both satisfy. Under one tag it would read line numbers as scores and
;; report nothing wrong. It should not be asked to invent a rule about the
;; range of a score to tell them apart: a consumer's accepting shape is
;; usually wider than the shape it was written against, so the defence
;; belongs in what the core names things.
;;
;; THE DIMENSION IS THE PLACE THAT CONSTRUCTS THE TAG, NOT THE VERB THAT
;; ANSWERS WITH IT, and the title says so because the two are not the same.
;; A tag built in `store.sc` reaches an answer because some handler passes a
;; function's result to `items`, and that link is a data flow across two
;; files. This walk cannot follow it, and a census that guessed the
;; attribution would read as though it had checked it. So the pairs are
;; (where it is built . tag): a verb name for the handlers that build their
;; own, a function name for the rest.
;;
;; WHAT IT WALKS. Every `(items X)` in `rpc.sc`. X comes in three shapes: a
;; literal construction, a call to a named function, or a variable bound in
;; the same handler to such a call. The first gives the tag directly; the
;; other two are followed into `store.sc`. A site that fits none of them is
;; reported by name -- see the row below -- rather than passed over, because
;; a scope nobody measured is the thing this file exists to avoid.
;; Every function `store.sc` defines, so the walk follows only names it can
;; actually read the body of.
(define store-forms
  (let walk ((fs (read-forms (string-append root "/store.sc"))) (acc '()))
    (cond ((null? fs) acc)
          ((and (pair? (car fs)) (eq? (car (car fs)) 'library))
           (walk (cdr fs) (append acc (cdr (car fs)))))
          (else (walk (cdr fs) (append acc (list (car fs))))))))

(define store-defines
  (let loop ((fs store-forms) (out '()))
    (cond ((null? fs) out)
          ((and (pair? (car fs)) (eq? (car (car fs)) 'define)
                (pair? (cdr (car fs))) (pair? (cadr (car fs))))
           (loop (cdr fs) (cons (car (cadr (car fs))) out)))
          (else (loop (cdr fs) out)))))

(define (items-call? f) (and (pair? f) (eq? (car f) 'items)))

(define (literal-tag f)
  (and (pair? f) (memq (car f) '(cons list))
       (pair? (cdr f))
       (let ((a (cadr f)))
         (and (pair? a) (eq? (car a) 'quote) (pair? (cdr a)) (symbol? (cadr a)) (cadr a)))))

(define (verb-of f)
  (and (pair? f) (eq? (car f) 'cons) (pair? (cdr f)) (pair? (cddr f))
       (let ((a (cadr f)) (b (caddr f)))
         (and (pair? a) (eq? (car a) 'quote) (pair? (cdr a)) (symbol? (cadr a))
              (pair? b) (eq? (car b) 'lambda)
              (cadr a)))))

;; The outermost literal tags in a form, not descending past one that is found.
(define (outermost-tags f)
  (let walk ((x f) (out '()))
    (cond
      ((not (pair? x)) out)
      ((literal-tag x) => (lambda (t) (if (memq t out) out (cons t out))))
      (else (let loop ((l x) (acc out))
              (cond ((pair? l) (loop (cdr l) (walk (car l) acc)))
                    (else acc)))))))

;; Every (name (f ...)) binding anywhere in a form, so `(items (cadr a))`
;; can be followed back to the call that produced `a`.
(define (bindings-in f)
  (let walk ((x f) (out '()))
    (cond
      ((not (pair? x)) out)
      ((and (pair? (car x)) (symbol? (car (car x)))
            (pair? (cdr (car x))) (pair? (car (cdr (car x))))
            (symbol? (car (car (cdr (car x))))))
       (let ((acc (cons (cons (car (car x)) (car (car (cdr (car x))))) out)))
         (let loop ((l x) (a acc))
           (cond ((pair? l) (loop (cdr l) (walk (car l) a))) (else a)))))
      (else (let loop ((l x) (acc out))
              (cond ((pair? l) (loop (cdr l) (walk (car l) acc))) (else acc)))))))

(define (symbols-in f)
  (let walk ((x f) (out '()))
    (cond ((symbol? x) (if (memq x out) out (cons x out)))
          ((pair? x) (let loop ((l x) (acc out))
                       (cond ((pair? l) (loop (cdr l) (walk (car l) acc))) (else acc))))
          (else out))))

;; -> (list pairs unresolved), walking rpc.sc
(define items-scan
  (let ((pairs '()) (unresolved '()))
    (define (note! where tag)
      (let ((k (cons where tag)))
        (unless (member k pairs) (set! pairs (cons k pairs)))))
    ;; NEVER: A NAME IS ONLY FOLLOWED IF IT IS A FUNCTION THIS WALK CAN READ.
    ;;
    ;; The first version took the head symbol of whatever it found, so
    ;; `(items (cadr a))` resolved to `cadr` and `read`'s
    ;; `(items (map (lambda (id) (view-read ...)) ids))` resolved to
    ;; `view-read`. Both were then "followed" into `store.sc`, found nothing,
    ;; and contributed no tags -- a resolution that reads as success and
    ;; produces silence. Two whole verbs' tags went missing that way, and the
    ;; list looked complete.
    ;;
    ;; So a name is followed only when `store.sc` defines it. Anything else
    ;; is unresolved and is REPORTED, which is the honest answer: the tags are
    ;; built somewhere this walk does not go.
    (define (callee-of x binds)
      (cond
        ((and (pair? x) (symbol? (car x)) (memq (car x) store-defines)) (car x))
        ((symbol? x) (let ((e (assq x binds)))
                       (and e (memq (cdr e) store-defines) (cdr e))))
        ((pair? x) (let loop ((l (cdr x)))
                     (cond ((null? l) #f)
                           ((callee-of (car l) binds) => (lambda (r) r))
                           (else (loop (cdr l))))))
        (else #f)))
    (define (walk f verb binds)
      (when (pair? f)
        (let* ((v (or (verb-of f) verb))
               (b (append (bindings-in f) binds)))
          (when (and (items-call? f) v)
            (let ((tags (outermost-tags (cdr f))))
              (if (pair? tags)
                  (for-each (lambda (t) (note! v t)) tags)
                  ;; NOT FOLLOWED INTO ANOTHER FILE. Following the name and
                  ;; taking the outermost tags of its body takes the wrong
                  ;; thing: `store-diff` yielded `error`, `from`, `ok` and
                  ;; `to` -- its own answer envelope -- while the item tags
                  ;; it really builds are deeper, and `rpc.sc` takes the
                  ;; SECOND element of what it returns. Getting that right
                  ;; needs to know which part of a return value becomes the
                  ;; items, which is a data flow and not a shape.
                  ;;
                  ;; A gate that can raise a false alarm is worse than one
                  ;; with a narrower reach: a false alarm has to be
                  ;; disproved, a narrow reach is merely uncovered. So the
                  ;; site is recorded by name and the row below expects it.
                  (let ((callee (callee-of (cadr f) b)))
                    (set! unresolved
                          (cons (cons v (or callee (if (pair? (cadr f)) (car (cadr f)) (cadr f))))
                                unresolved))))))
          (let loop ((l f))
            (cond ((pair? l) (walk (car l) v b) (loop (cdr l))) (else #f))))))
    (for-each (lambda (form) (walk form #f '())) (read-forms (string-append root "/rpc.sc")))
    (list pairs unresolved)))

;; The tags each followed function constructs, taken from its definition in
;; store.sc.
(define tag-sites
  (list-sort (lambda (a b) (string<? (symbol->string (cdr a)) (symbol->string (cdr b))))
             (car items-scan)))

(want "S-B2b these are the item tags and the place each one is constructed in"
      tag-sites
      '((log . entry) (search . hit) (grep . match) (refs . ref) (tag . tag)))

;; THE PROPERTY, ASKED SEPARATELY FROM THE LIST. The row above notices any
;; change at all; this one is the rule.
;;
;; NEVER: AND THE RULE IT CHECKS IS NOT QUITE THE PROPERTY WE WANT. The rule
;; is written once, at `rpc.sc` above the `grep` handler: NO TWO VERBS ANSWER
;; WITH ONE TAG. Every other place that describes this gate points there
;; rather than restating it, and the paragraph above the last row of this
;; section says where the gate is stricter than the rule.
;;
;; WHAT THIS ROW MEASURES, EXACTLY. `note!` keys on `(verb . tag)`, so two
;; construction sites inside ONE verb collapse to one entry and this row
;; cannot see them; what it finds is a tag that appears under two DIFFERENT
;; verbs. The row's title used to say "in two different places", which is
;; not what the deduplication does.
(want "S-B2b TWIN: and no tag is constructed under two different verbs"
      (let loop ((ps tag-sites) (bad '()))
        (cond
          ((null? ps) (reverse bad))
          (else
            (let* ((tag (cdr (car ps)))
                   (places (map car (filter (lambda (q) (eq? (cdr q) tag)) tag-sites))))
              (loop (cdr ps)
                    (if (and (> (length places) 1) (not (assq tag bad)))
                        (cons (cons tag places) bad)
                        bad))))))
      '())

;; WHAT THE WALK COULD NOT FOLLOW, as a reading rather than as prose. An
;; `(items X)` whose X is neither a literal construction nor a call this walk
;; can name is listed here; its tags are built somewhere this gate does not
;; look, and saying which sites those are is the difference between a scope
;; that was measured and a comment that claims one.
;; THE SITES IT COULD NOT FOLLOW ARE AN EXPECTATION, NOT A PRINT.
;;
;; Printing them would say what is uncovered today and nothing at all on the
;; day a fifth appears. As a row, a new unfollowable site is a red one and
;; somebody comes to look.
;;
;; THE COST OF (A), STATED PLAINLY. The tags built behind these four sites
;; are not in the table above: `def` and `export` from the definitions index,
;; `added`, `removed` and `changed` from `store-diff`, and `conflict`,
;; `orphan`, `nested-document` and `pending` from `store-conflicts`. THOSE
;; NAMES ARE TAKEN AND THIS GATE WILL NOT SHOUT FOR THEM -- if a verb ever
;; answers with one of them as well, nothing here notices.
;;
;; THREE OF THEM ARE DISPATCHED ON BY THE PLUGIN, read in its own source
;; rather than taken on report: `theourgia-vsc/src/model.ts:716` holds
;; `const STRUCTURAL_HEADS = ['orphan', 'nested-document', 'conflict']`, and
;; `pending` reaches the same reader as an item that build does not know.
;;
;; BUT ITS READERS DISPATCH PER VERB, and that makes the real rule narrower
;; than the one this gate checks: `structuralMarks` issues
;; `request('conflicts', [])` and reads only that answer, and `hitsOf` is
;; handed a search answer. So a collision misleads a reader only when two
;; items are in ONE VERB'S answer. The safe boundary is a different verb,
;; not a different meaning.
;;
;; Three things follow, and the third is why they are written here:
;;
;;   1. The rule that matters is written at `rpc.sc`, above the `grep`
;;      handler: no two verbs answer with one tag. What a reader needs is
;;      that one verb's answer never carries two different kinds of item
;;      under one tag.
;;   2. This gate checks something STRICTER. What the TWIN row measures is
;;      that no tag appears under two different VERBS -- `note!` keys on
;;      `(verb . tag)`, so two construction sites inside one verb collapse
;;      to a single entry and are invisible to it. That is stricter than
;;      the rule, because a verb may legitimately answer with a kind of
;;      item another verb also answers with.
;;
;;      AN EARLIER VERSION OF THIS PARAGRAPH SAID the gate forbids a tag
;;      being constructed "in two places at all". It does not, and the
;;      difference is not academic: a comment that describes the wrong
;;      mechanism sends the next reader looking for a second construction
;;      site when what went red was a second VERB.
;;   3. SO A RED ROW HERE IS NOT NECESSARILY A DEFECT. It may be a
;;      legitimate reuse of a tag across two verbs. When it goes red, the
;;      thing to do is judge it -- NOT widen the expectation.
;;
;;      MEASURED, rather than argued: copy the `refs` handler to a
;;      `backlinks` verb and leave it answering the same `ref` items it
;;      answers today. Nothing is ambiguous -- both verbs return exactly
;;      one kind of item, and a reader of either answer knows what a `ref`
;;      is -- and the TWIN row goes from `()` to `((ref refs backlinks))`.
;;      That reading was taken by applying the change to the census
;;      expression in memory, not by adding the verb to this build.
;;
;; A gate that is stricter than the rule, and does not say where it is
;; stricter, teaches the next person to raise the expectation until it stops
;; complaining.
(want "S-B2b and these are the items sites the walk cannot follow"
      (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                 (cadr items-scan))
      '((conflicts . store-conflicts) (diff . store-diff) (read . map) (whereis . append)))

;; THE THREE THINGS IT STILL CANNOT SEE, named rather than described as a
;; class:
;;
;;   * a tag constructed in a binding ABOVE the `items` call. I made exactly
;;     that mistake in this batch -- `grep` built its tag in a `let*` above
;;     the call, and this census did not list grep at all until it moved.
;;   * a tag that is a variable, or taken from a table, rather than written
;;     as a literal symbol.
;;   * a construction reached through a function call. Those four sites are
;;     the row above, and what is built behind them is listed there.
;;
;; So what this gate sees is that somebody constructed an item literally in
;; one of the places it walks. It stops the most likely accident, not every
;; one.

(define done-hits (census builds-done-clause?))

(want "F32 exactly one place in the shipped sources builds the done clause"
      (list (length done-hits)
            (apply + (map (lambda (h) (length (cdr h))) done-hits))
            (map (lambda (h) (let ((p (car h)))
                               (substring p (+ 1 (last-slash p)) (string-length p))))
                 done-hits))
      (list 1 1 '("store.sc")))

;; NEVER: AND THE DEFINITION IS NAMED FROM THE DATUM. The text version asked
;; whether the FILE contained `(define (batch-answer`, which a comment
;; mentioning it would have satisfied -- this one reports the name of the
;; definition the hit is lexically inside, read as data.
(want "F32 TWIN: and the definition it is inside is batch-answer"
      (if (null? done-hits)
          'THE-CENSUS-SAW-NOTHING
          (cdr (car done-hits)))
      '(batch-answer))

;; F42: ONE TABLE OF KINDS, AND THE CENSUS IS HOW WE KNOW.
;;
;; A kind decides what a block is, and four routes write one: the command
;; line, an intent in a batch, replay, and the projection that exports. The
;; failure this counts is not "a bad kind got in" -- it is TWO ALLOWLISTS:
;; a second literal set somewhere, agreeing today, and one of them extended
;; next month. The rows in `cli1.sc` say the routes refuse; this says there
;; is one thing for them to ask.
;;
;; WHAT COUNTS AS A HIT: any list, anywhere in the shipped sources, of two or
;; more distinct symbols every one of which is a kind. That catches a quoted
;; `'(code doc)` handed to `memq`, and it also catches the bare clause head of
;; a `case`, which is not quoted and which a search for quotations would miss.
;; It does NOT catch a set built at run time, or one spelled with strings --
;; SO THIS IS NOT A PROOF OF UNIQUENESS, it is a tripwire on the spelling a
;; second allowlist would most likely have.
;; NEVER: A DRIFTING LIST IS EXACTLY THE ONE WITH AN EXTRA NAME IN IT. The
;; first version of this required EVERY element to be a known kind, so
;; `'(code doc)` was counted and `'(code doc alien)` was not -- and a second
;; allowlist that has drifted is far more likely to look like the second. The
;; test is now two or more DISTINCT known kinds among a list of symbols,
;; whatever else is in it.
;;
;; The cost is false positives: any list of symbols that happens to contain
;; two kind names -- a formals list `(code section)`, say -- is counted. That
;; is the right way round for a tripwire, and the expected count is a number
;; in the row below, so a false positive announces itself the day it appears
;; rather than hiding something.
(define (kind-set? form quoted?)
  (and (pair? form)
       (list? form)
       (for-all symbol? form)
       (let loop ((xs form) (seen (quote ())))
         (cond ((null? xs) (>= (length seen) 2))
               ((and (memq (car xs) known-kinds) (not (memq (car xs) seen)))
                (loop (cdr xs) (cons (car xs) seen)))
               (else (loop (cdr xs) seen))))))

(define kind-hits (census kind-set?))

;; NEVER: THE PLACES ARE NAMED, NOT COUNTED. There are two lists of kinds in
;; the shipped sources and they answer different questions: `known-kinds` says
;; which kinds EXIST -- every write route asks it, and it is the only thing
;; that decides legality -- while `md-kinds` says which of them the markdown
;; projection is responsible for, so that a `code` block is understood as
;; addressed elsewhere rather than reported as lost.
;;
;; Naming them is what keeps this a tripwire. A row that only counted would
;; have to be edited to a bigger number every time somebody added a list, and
;; a number is exactly the thing nobody argues with. A FOURTH list, wherever it
;; appears and whatever it is for, makes this row red and has to be defended.
;;
;; IT WENT TO THREE FOR ONE ROUND, AND THEN BACK TO TWO -- AND THE HISTORY IS
;; THE POINT. `store-search` began asking which kinds have names and spelled
;; its own set to do it, which this row caught: the count read 3 and the twin
;; below read `store-search`, a name that is a FUNCTION rather than a table,
;; which is exactly what an unnamed second allowlist looks like.
;;
;; The first repair gave that rule one definition, a set called
;; `name-bearing-kinds`, and the census went to three. Then a mutation deleted
;; that set and NOTHING WENT RED: a `cond` next to it already branched on the
;; same two kinds, so the set was a restatement of the control flow and no
;; reading could tell whether it was there.
;;
;; The second repair made the table the DISPATCH -- `name-readers`, a kind to
;; the procedure that reads its names -- so "which kinds have names" and "how
;; their names are read" cannot come apart. There is no literal set left for
;; this census to see, which is why it is back to two and why the row below
;; exists: a pattern that looks for quoted sets of kinds is blind to a rule
;; expressed as the keys of a dispatch table.
;;
;; Ruled by the main session, 2026-09-20.
(want "F42 two places in the shipped sources spell out a set of kinds, and these are they"
      (list (apply + (map (lambda (h) (length (cdr h))) kind-hits))
            (list-sort string<?
                       (map (lambda (h) (let ((p (car h)))
                                          (substring p (+ 1 (last-slash p)) (string-length p))))
                            kind-hits)))
      (list 2 '("project.sc" "reduce.sc")))

;; NEVER: AND THE ONE HIT IS THE DEFINITION ITSELF, NOT A USE OF IT. Reported
;; from the datum, so renaming the table to something else and leaving
;; `known-kinds` as an alias -- which keeps the count at one and keeps every
;; import working -- is what this row is here to be red about.
;; NEVER: AND EACH ONE IS THE DEFINITION IT IS SUPPOSED TO BE, NOT A USE OF
;; IT. Reported from the datum, so renaming a table and leaving the old name
;; as an alias -- which keeps the count right and every import working -- is
;; what this row is here to be red about.
(want "F42 TWIN: and the definitions they are inside are known-kinds and md-kinds"
      (if (null? kind-hits)
          'THE-CENSUS-SAW-NOTHING
          (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                     (apply append (map cdr kind-hits))))
      '(known-kinds md-kinds))

;; AND THE ONE THE CENSUS CANNOT SEE IS PINNED BY ITS VALUE INSTEAD. The kinds
;; that have names are the KEYS of a dispatch table, so no pattern looking for
;; a quoted set will ever find them -- and a rule no instrument can see is a
;; rule that can grow quietly. The expectation here is written from outside the
;; tree: adding a reader is a change somebody has to come and defend, which is
;; what the census does for the other two.
(want "F42 and the kinds that bear names are exactly these, read from the product"
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 name-bearing-kinds)
      '(code library))


;; NEVER: AND THE TWO TABLES STAND IN A FIXED RELATION. The census counts the
;; PLACES that spell out a set of kinds; it cannot see a change to what one of
;; them CONTAINS -- measured: putting `code` and `library` into `md-kinds`
;; left the census green, and only a behaviour row noticed. This is the other
;; half. `md-kinds` says which kinds this projection writes and `known-kinds`
;; says which kinds exist, so a kind that this projection claims and the
;; reducer has never heard of is a table that has drifted, whichever of the
;; two moved.
;;
;; Both sides come from the libraries' own exports, so neither is this file's
;; copy of anything.
(want "F42 every kind the markdown projection claims is a kind that exists"
      (list (length (filter (lambda (k) (not (memq k known-kinds))) md-kinds))
            (filter (lambda (k) (not (memq k known-kinds))) md-kinds)
            (if (null? md-kinds) 'THE-PROJECTION-CLAIMS-NOTHING 'and-it-claims-something))
      (list 0 '() 'and-it-claims-something))

;; ---- F58 the run-time census of item tags ----------------------------------
;;
;; KEY: THE STATIC ROWS ABOVE READ rpc.sc; THESE RUN IT. For every verb the
;; program answers to, one invocation is made and its answer is classified.
;; The census is of the answers exercised here, one invocation per verb; it
;; is not a proof of every tag a verb can produce, and a verb's class says
;; what ITS invocation answered.
;;
;; THE POPULATION IS (rpc-verbs), not a scanner and not what a run produced.
;; Success is `rpc-ok?`'s, the product's own rule, and nowhere else.
;;
;; Every invocation lands in exactly one class:
;;   tagged       an items answer with an item that is a symbol-headed pair:
;;                recorded as verb -> (sorted heads . count of other items)
;;   untagged     an items answer with items, none symbol-headed
;;   not-items    a success that is not an items answer: recorded with its head
;;   unexercised  anything else, with a structured reason:
;;                (no-invocation), (empty-items), (refused <head> <reason>),
;;                (raised <message>)
;; An items answer is `(ok (items x ...) clause ...)`; the trailing clauses
;; are siblings of the items list, never items.

;; A FRESH ROOT, REFUSED IF IT EXISTS. The root is named by process id, and a
;; recycled id with a directory left behind would hand this census a store
;; it did not build (F71).
(define census-root
  (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                 "/facade-gate-census-" (number->string (get-process-id))))
(when (file-exists? census-root)
  (error 'facade-gate "the census wants a fresh root" census-root))
(mkdir-p! census-root)

(define (census-write! path s)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p (if (string? s) (string->utf8 s) s)))))
(define (census-new-id answer)
  (let ((ev (and (pair? answer) (assq 'events (cdr answer)))))
    (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))

;; ISOLATION IS A FRESH STORE, NOT A COPY: a copied store keeps the
;; original's instance identity, and the machine registry lives in the
;; machine home. So each verb gets its own home, its own store and its own
;; directory for every file it reads or writes outside the store.
(define census-areas '())
(define (census-case! name)
  (let* ((area (string-append census-root "/" name))
         (home (string-append area "/home"))
         (store (string-append area "/store"))
         (ext (string-append area "/ext")))
    (set! census-areas (cons (list name home store ext (file-exists? area)) census-areas))
    (mkdir-p! ext)
    (putenv "THEOURGIA_HOME" home)
    (cons store ext)))
(define (census-ask store . req) (rpc-dispatch store req "census"))

;; THE COMMON SEED. Each step is here because some verb needs it:
;;   P, C, then P deleted     C is an orphan, for conflicts
;;   B with "needle" text     a hit for search and a match for grep
;;   F, then t0; then B's title, src, level, parent and ord (moved under
;;   F) and links (B explains C) all changed, D deleted, E inserted; then t1
;;                            added, removed, and changed in every field diff
;;                            compares (store.sc projected-fields), between two
;;                            cuts; and stored tags, for the argument-free tag
;;   B explains E             a link to E, for refs
;;   a datum-mode library     somewhere for def to define into, and a live
;;                            export for whereis
;;   a draft of B             written after every committed edit to B, and
;;                            differing from it, for drafts, commit, discard
;; Every step's answer is kept, and a CONTROL row below asks that each one
;; succeeded: a step that failed would change what the verbs answer.
(define census-seed-failures '())
(define (census-seed! store ext)
  (let ((steps '()))
    (define (step! label answer)
      (unless (rpc-ok? answer)
        (set! census-seed-failures (cons (list label answer) census-seed-failures)))
      answer)
    (let* ((init (step! 'init (census-ask store 'init)))
           (writer (cadr (assq 'writer (cdr init))))
           (P (census-new-id (step! 'insert-p (census-ask store 'insert "--under" "root" "--title" "Parent"))))
           (C (census-new-id (step! 'insert-c (census-ask store 'insert "--under" P "--title" "Child"))))
           (del-p (step! 'del-p (census-ask store 'del P)))
           ;; F comes before B at the root, so that moving B under F changes
           ;; its ord as well as its parent.
           (F (census-new-id (step! 'insert-f (census-ask store 'insert "--under" "root" "--title" "Foxtrot"))))
           (B (census-new-id (step! 'insert-b (census-ask store 'insert "--under" "root" "--title" "Beta"
                                                          "--text" "a needle in the beta block"))))
           (D (census-new-id (step! 'insert-d (census-ask store 'insert "--under" "root" "--title" "Delta"))))
           (t0 (step! 'tag-t0 (census-ask store 'tag "t0")))
           (retitle (step! 'retitle-b (census-ask store 'set B "title" "Beta renamed")))
           (resrc (step! 'resrc-b (census-ask store 'set B "src" "a needle in the changed beta block")))
           ;; level is an integer, and `set` hands its value on as text, which
           ;; the product refuses as a malformed intent; a batch intent is read
           ;; as data, so the level goes through batch.
           (relevel (step! 'relevel-b (census-ask store 'batch
                                                  (string-append "((set \"" B "\" level 3))"))))
           (move-b (step! 'move-b (census-ask store 'move B F)))
           (link-c (step! 'link-b-c (census-ask store 'link B "explains" C)))
           (del-d (step! 'del-d (census-ask store 'del D)))
           (E (census-new-id (step! 'insert-e (census-ask store 'insert "--under" "root" "--title" "Echo"))))
           (t1 (step! 'tag-t1 (census-ask store 'tag "t1")))
           (link (step! 'link (census-ask store 'link B "explains" E)))
           (lib-dir (string-append ext "/lib-in")))
      (mkdir-p! lib-dir)
      (census-write! (string-append lib-dir "/a.sc")
                     "(library (a) (export gnarlwick) (import (rnrs)) (define gnarlwick 1))\n")
      (step! 'import-lib (census-ask store 'import-code lib-dir "--datum"))
      (let* ((state (open-and-reduce store))
             (lib (find (lambda (id) (equal? (code-field state id 'name) '(a)))
                        (map cadr (state-datum state)))))
        (step! 'draft-b (census-ask store 'write B "draft text that differs" "--writer" writer))
        (list (cons 'writer writer) (cons 'B B) (cons 'C C) (cons 'E E) (cons 'lib lib))))))
(define (census-get seed key) (cdr (assq key seed)))
(define census-diff-block #f)
(define (seeded proc)
  (lambda (store ext) (proc store ext (census-seed! store ext))))

;; THE INVOCATION TABLE: one entry per verb. A verb with no entry is
;; (no-invocation), so a verb added to the dispatcher is named, not skipped.
(define census-table
  (list
    (cons 'describe (seeded (lambda (st x s) (census-ask st 'describe))))
    ;; init is given a directory nothing has initialised
    (cons 'init (lambda (st x) (census-ask st 'init)))
    (cons 'insert (seeded (lambda (st x s) (census-ask st 'insert "--under" "root" "--title" "Census"))))
    (cons 'set (seeded (lambda (st x s) (census-ask st 'set (census-get s 'E) "title" "Echo renamed"))))
    (cons 'move (seeded (lambda (st x s) (census-ask st 'move (census-get s 'E) (census-get s 'B)))))
    (cons 'del (seeded (lambda (st x s) (census-ask st 'del (census-get s 'E)))))
    (cons 'link (seeded (lambda (st x s) (census-ask st 'link (census-get s 'E) "explains" (census-get s 'B)))))
    (cons 'unlink (seeded (lambda (st x s) (census-ask st 'unlink (census-get s 'B) "explains" (census-get s 'E)))))
    (cons 'write (seeded (lambda (st x s)
                           (census-ask st 'write (census-get s 'E) "a draft of echo"
                                       "--writer" (census-get s 'writer)))))
    (cons 'drafts (seeded (lambda (st x s) (census-ask st 'drafts "--writer" (census-get s 'writer)))))
    (cons 'commit (seeded (lambda (st x s)
                            (census-ask st 'commit (census-get s 'B) "--writer" (census-get s 'writer)))))
    (cons 'discard (seeded (lambda (st x s)
                             (census-ask st 'discard (census-get s 'B) "--writer" (census-get s 'writer)))))
    (cons 'batch (seeded (lambda (st x s)
                           (census-ask st 'batch
                                       (string-append "((set \"" (census-get s 'E) "\" title \"batched\"))")))))
    (cons 'split-suggest (seeded (lambda (st x s)
                                   (let ((in (string-append x "/split-in.sc")))
                                     (census-write! in "(define (f) 1)\n\n(define (g) 2)\n")
                                     (census-ask st 'split-suggest in "--output" (string-append x "/split-out.txt"))))))
    (cons 'import-code (seeded (lambda (st x s)
                                 (let ((d (string-append x "/code-in")))
                                   (mkdir-p! d)
                                   (census-write! (string-append d "/m.py") "def marmoset():\n  pass\n")
                                   (census-ask st 'import-code d)))))
    (cons 'export-code (seeded (lambda (st x s) (census-ask st 'export-code (string-append x "/code-out")))))
    (cons 'def (seeded (lambda (st x s)
                         (census-ask st 'def "freshname" "--under" (census-get s 'lib) "(define freshname 1)"))))
    ;; THE DIRECTORY IS MADE FIRST: export-md into one that does not exist
    ;; yet writes nothing and calls every document's path unusable (F82).
    (cons 'export-md (seeded (lambda (st x s)
                               (let ((d (string-append x "/md-out")))
                                 (mkdir-p! d)
                                 (census-ask st 'export-md d)))))
    (cons 'adopt (seeded (lambda (st x s) (census-ask st 'adopt))))
    (cons 'check (seeded (lambda (st x s) (census-ask st 'check))))
    (cons 'snapshot (seeded (lambda (st x s) (census-ask st 'snapshot))))
    (cons 'outline (seeded (lambda (st x s) (census-ask st 'outline))))
    ;; read's items mode
    (cons 'read (seeded (lambda (st x s) (census-ask st 'read (census-get s 'B) "--recursive"))))
    (cons 'refs (seeded (lambda (st x s) (census-ask st 'refs (census-get s 'E)))))
    (cons 'search (seeded (lambda (st x s) (census-ask st 'search "needle"))))
    (cons 'grep (seeded (lambda (st x s) (census-ask st 'grep "needle"))))
    (cons 'whereis (seeded (lambda (st x s) (census-ask st 'whereis "gnarlwick"))))
    ;; with no arguments log lists every applied event, the seed's among them
    (cons 'log (seeded (lambda (st x s) (census-ask st 'log))))
    ;; tag's items mode is the argument-free listing
    (cons 'tag (seeded (lambda (st x s) (census-ask st 'tag))))
    (cons 'diff (seeded (lambda (st x s)
                          (set! census-diff-block (census-get s 'B))
                          (census-ask st 'diff "t0" "t1"))))
    (cons 'conflicts (seeded (lambda (st x s) (census-ask st 'conflicts))))
    ;; RESTORE'S OWN SETUP, never the common seed's: a committed version,
    ;; then a second record claiming its identity in another writer's
    ;; stream, which revokes it. Built as test/revoke-restore.sc builds it.
    (cons 'restore (seeded (lambda (st x s)
      (let* ((w (census-get s 'writer)) (A (census-get s 'E)))
        (census-ask st 'write A "the text I wrote" "--writer" w)
        (let* ((v1 (list-ref (assq 'projection (cdr (census-ask st 'read A "--working-info" "--writer" w))) 4))
               (cursor (string-append w ":" (number->string
                                              (cdr (assoc w (reduce-applied-cut (open-and-reduce st)))))))
               (committed (census-ask st 'commit A "--req" "R1" "--cursor" cursor
                                      "--working-version" v1 "--writer" w))
               (plan-ev (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e))))
                              (store-evidence st (cons w "R1"))))
               (rival (encode-record 1 1789000000001 (ev-actor plan-ev) '()
                                     (storable-encode (ev-payload plan-ev)))))
          (log-publish! st "rivalzzz" 1 rival (segment-sha rival))
          (census-ask st 'restore v1 "--writer" w))))))
    ;; PUBLISH'S OWN SETUP: one valid record for a mirror writer.
    (cons 'publish (seeded (lambda (st x s)
      (let ((file (string-append x "/mirror.bin")))
        (census-write! file (encode-record 1 1789000000001 "peer" '()
                                           (storable-encode '(put ((kind . section) (title . "mirrored"))))))
        (census-ask st 'publish "mirrorzz" "1" file)))))
    ;; IMPORT-MD'S OWN SETUP: a fresh store holding one document at seed.md
    ;; with two sections, exported and imported back. Not the common seed:
    ;; the import's answer here is the plain one, every document of the
    ;; store in the directory it reads, so nothing is reported absent.
    ;; The directory is made first (F82).
    (cons 'import-md (lambda (st x)
      (census-ask st 'init)
      (let ((doc (census-new-id (census-ask st 'insert "--under" "root" "--title" "Seed doc")))
            (d (string-append x "/md")))
        (census-ask st 'set doc "kind" "doc")
        (census-ask st 'set doc "path" "seed.md")
        (census-ask st 'insert "--under" doc "--title" "One" "--text" "first")
        (census-ask st 'insert "--under" doc "--title" "Two" "--text" "second")
        (mkdir-p! d)
        (census-ask st 'export-md d)
        (census-ask st 'import-md d))))))

(define (census-name<? a b) (string<? (symbol->string a) (symbol->string b)))
(define (census-dedup-strings xs)
  (let loop ((l xs) (acc '()))
    (if (null? l) acc (loop (cdr l) (if (member (car l) acc) acc (cons (car l) acc))))))
(define (census-dedup xs)
  (let loop ((l xs) (acc '()))
    (if (null? l) (reverse acc) (loop (cdr l) (if (memq (car l) acc) acc (cons (car l) acc))))))

;; -> (class detail), and the answer
(define (census-classify answer)
  (cond
    ((not (rpc-ok? answer))
     (list 'unexercised
           (list 'refused (car answer) (and (pair? (cdr answer)) (cadr answer)))))
    ((and (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
          (eq? (car (cadr answer)) 'items))
     (let* ((items (cdr (cadr answer)))
            (tagged? (lambda (x) (and (pair? x) (symbol? (car x)))))
            (heads (census-dedup (map car (filter tagged? items))))
            (others (length (filter (lambda (x) (not (tagged? x))) items))))
       (cond ((null? items) (list 'unexercised '(empty-items)))
             ((null? heads) (list 'untagged others))
             (else (list 'tagged (cons (list-sort census-name<? heads) others))))))
    (else (list 'not-items (car answer)))))

;; (verb class detail answer), one per population member
(define census
  (map (lambda (verb)
         (let ((entry (assq verb census-table)))
           (if (not entry)
               (list verb 'unexercised '(no-invocation) #f)
               (let* ((c (census-case! (symbol->string verb)))
                      (answer (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                                              (condition-message e) e))))
                                ((cdr entry) (car c) (cdr c)))))
                 (if (and (pair? answer) (eq? (car answer) 'RAISED))
                     (list verb 'unexercised (list 'raised (cadr answer)) answer)
                     (let ((k (census-classify answer)))
                       (list verb (car k) (cadr k) answer)))))))
       (rpc-verbs)))

(define (census-class name)
  (list-sort census-name<? (map car (filter (lambda (r) (eq? (cadr r) name)) census))))
(define (census-detail name)
  (list-sort (lambda (a b) (census-name<? (car a) (car b)))
             (map (lambda (r) (cons (car r) (caddr r)))
                  (filter (lambda (r) (eq? (cadr r) name)) census))))
(for-each (lambda (r) (printf "   census ~a: ~a ~s\n" (car r) (cadr r) (caddr r))) census)

;; ---- the rows ----

;; PRINTED AS WELL AS COMPARED: a row that is ok prints no value, and these
;; two values are readings a delivery cites.
(define (census-isolation)
  (let ((distinct (lambda (k) (length (census-dedup-strings (map k census-areas))))))
    (list (length census-areas) (distinct cadr) (distinct caddr) (distinct cadddr)
          (length (filter (lambda (a) (list-ref a 4)) census-areas))
          (length (filter (lambda (a) (not (and (file-exists? (cadr a))
                                                (pair? (directory-list (cadr a))))))
                          census-areas)))))
(printf "   census isolation (cases, homes, stores, outside dirs, pre-existing, empty homes): ~s\n"
        (census-isolation))

;; CONTROL: EACH VERB HAD ITS OWN HOME, STORE AND OUTSIDE DIRECTORY, none of
;; them existing before its case began, and each home was written to by its
;; own case (the machine registry lives there). Read as the number of cases,
;; the number of distinct homes, stores and outside directories, the number
;; whose area existed beforehand, and the number of homes left empty.
(want "F58 CONTROL each case ran in its own fresh home, store and outside directory"
      (census-isolation)
      (let ((n (length (rpc-verbs)))) (list n n n n 0 0)))

;; CONTROL: the seed's steps all succeeded in every case that used it. A
;; failed step would change what the verbs answer, and the rows below would
;; then be about a store this file did not mean to build.
(want "F58 CONTROL every seed step answered as a success, in every seeded case"
      (map car census-seed-failures)
      '())

;; CONTROL: THE SEED DOES WHAT SECTION 4 OF THE DESIGN ASKS OF IT FOR diff --
;; one surviving block changed in every field diff compares. Read from the
;; diff case's own answer: the fields it reports as changed for B.
(want "F58 CONTROL on the seed, diff reports B changed in title, src, level, parent, ord and links"
      (let* ((r (assq 'diff census))
             (a (and r (cadddr r)))
             (items (if (and (pair? a) (pair? (cdr a)) (pair? (cadr a))) (cdr (cadr a)) '())))
        (list-sort census-name<?
                   (map caddr (filter (lambda (i) (and (pair? i) (eq? (car i) 'changed)
                                                      (equal? (cadr i) census-diff-block)))
                                      items))))
      '(level links ord parent src title))

;; PRODUCT ROW: the raw population has no verb twice. rpc-verbs maps the
;; dispatch table directly (rpc.sc:1368), so a duplicate key there turns this
;; red.
(want "F58 the verb table names no verb twice"
      (let loop ((l (rpc-verbs)) (seen '()) (dup '()))
        (cond ((null? l) (reverse dup))
              ((memq (car l) seen) (loop (cdr l) seen (cons (car l) dup)))
              (else (loop (cdr l) (cons (car l) seen) dup))))
      '())

;; PRODUCT ROW although it reads this file's table: an entry for a verb the
;; dispatcher no longer has is red with no edit here.
(want "F58 every invocation in the table is for a verb the dispatcher has"
      (filter (lambda (v) (not (memq v (rpc-verbs)))) (map car census-table))
      '())

(want "F58 these verbs answer with tagged items"
      (census-class 'tagged)
      '(commit conflicts def diff drafts grep import-code log refs search tag whereis))
(want "F58 these verbs answer with items that carry no tag"
      (census-class 'untagged)
      '(read))
(want "F58 these verbs answer with a success that is not items"
      (census-class 'not-items)
      '(batch check del describe discard export-code export-md import-md init insert
        link move outline publish restore set snapshot split-suggest unlink write))
(want "F58 these verbs are unexercised"
      (census-class 'unexercised)
      '(adopt))

;; THE TAGGED MAP: verb -> (sorted heads . count of items that are not
;; symbol-headed pairs). A head may be `ok` (commit, import-code, def); none
;; is filtered out as not a real tag.
(want "F58 each tagged verb's heads, and how many of its items carry none"
      (census-detail 'tagged)
      '((commit (ok) . 0) (conflicts (orphan) . 0) (def (ok) . 0)
        (diff (added changed removed) . 0) (drafts (draft) . 0) (grep (match) . 0)
        (import-code (ok) . 0) (log (entry) . 0) (refs (ref) . 0) (search (hit) . 0)
        (tag (tag) . 0) (whereis (def export) . 0)))
(want "F58 each not-items verb's answer head"
      (census-detail 'not-items)
      '((batch . batch) (check . check) (del . ok) (describe . ok) (discard . ok)
        (export-code . ok) (export-md . ok) (import-md . import) (init . ok) (insert . ok)
        (link . ok) (move . ok) (outline . ok) (publish . ok) (restore . ok) (set . ok)
        (snapshot . ok) (split-suggest . ok) (unlink . ok) (write . ok)))

;; THE UNEXERCISED LIST IS AN ALLOW-LIST, AND EVERY ENTRY IS JUSTIFIED. A
;; refusal is accepted only with the line saying why it is the verb's correct
;; answer on this seed. Anything else unexercised -- a catch-all, a raise, a
;; verb with no invocation -- has no such line and is red.
(define census-allowed
  (list
    (list 'adopt '(refused error not-needed)
          "a healthy store needs no adoption (log.sc:4428)")))
(want "F58 CONTROL every accepted refusal says why it is correct"
      (filter (lambda (a) (not (and (string? (caddr a)) (> (string-length (caddr a)) 0))))
              census-allowed)
      '())
(want "F58 the unexercised verbs and their reasons are exactly the justified ones"
      (census-detail 'unexercised)
      (list-sort (lambda (a b) (census-name<? (car a) (car b)))
                 (map (lambda (a) (cons (car a) (cadr a))) census-allowed)))

;; TRIPWIRE, NOT A MEASUREMENT: an answer, at the top level or inside a
;; batch or import envelope, whose head and reason are one of the catch-alls
;; known on 2026-09-23. The list is today's and incomplete; the allow-list
;; above is what closes the gap.
(define census-catch-alls '((error internal) (error working-unavailable) (error unknown)))
(define (census-members answer)
  (if (and (pair? answer) (memq (car answer) '(batch import)) (pair? (cdr answer)) (list? (cadr answer)))
      (cons answer (cadr answer))
      (list answer)))
(define (census-tripwire-hits)
  (apply append
             (map (lambda (r)
                    (let ((hits (filter (lambda (a)
                                          (and (pair? a) (pair? (cdr a))
                                               (member (list (car a) (cadr a)) census-catch-alls)))
                                        (census-members (cadddr r)))))
                      (if (null? hits) '() (list (car r)))))
                  census)))
(printf "   census tripwire hits: ~s\n" (census-tripwire-hits))
(want "F58 TRIPWIRE no answer is one of the known catch-alls"
      (census-tripwire-hits)
      '())

;; CONTROL: one total classification per population member.
(want "F58 CONTROL the four classes together are the population, each verb once"
      (let ((all (append (census-class 'tagged) (census-class 'untagged)
                         (census-class 'not-items) (census-class 'unexercised))))
        (list (length all) (length (census-dedup all))
              (equal? (list-sort census-name<? all) (list-sort census-name<? (rpc-verbs)))))
      (list (length (rpc-verbs)) (length (rpc-verbs)) #t))

;; THE STATIC GATE BECOMES A CROSS-CHECK. Every (verb . tag) the static scan
;; reads from rpc.sc appears at run time; every verb it could not follow is
;; tagged or untagged at run time.
(define census-runtime-pairs
  (apply append (map (lambda (e) (map (lambda (h) (cons (car e) h)) (cadr e)))
                     (census-detail 'tagged))))
(want "F58 every tag the static scan reads is one the verb produces at run time"
      (filter (lambda (p) (not (member p census-runtime-pairs))) (car items-scan))
      '())
(want "F58 every verb the static scan cannot follow answers with items at run time"
      (filter (lambda (v) (not (or (memq v (census-class 'tagged)) (memq v (census-class 'untagged)))))
              (census-dedup (map car (cadr items-scan))))
      '())
;; PRINTED, NOT COMPARED: what the run shows that the static scan does not.
(printf "   run-time pairs the static scan does not read: ~s\n"
        (filter (lambda (p) (not (member p (car items-scan)))) census-runtime-pairs))

(system (string-append "rm -rf '" census-root "'"))

(printf "rows: ~a\n~a failures\nfacade-gate complete\n" rows failures)
(exit (if (zero? failures) 0 1))
