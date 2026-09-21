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
        (only (theourgia reduce) known-kinds)
        (only (theourgia project) md-kinds)
        ;; and the third rule, whose shape this census cannot see: it is the
        ;; key set of a dispatch table, not a quoted list, so it is pinned by
        ;; value in its own row below.
        (only (theourgia store) name-bearing-kinds))

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
    (if (file-exists? (string-append up "/cli.sc")) up script-dir)))

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
;; NEVER: AND THE RULE IT CHECKS IS NOT QUITE THE PROPERTY WE WANT. What we
;; want is that no two ANSWERS share a tag. What this checks is that no tag
;; is constructed in two PLACES. Those are the same only if each place serves
;; one answer -- and that premise is not checked here. It is written down so
;; that whoever relies on it does so knowingly, rather than inheriting it.
(want "S-B2b TWIN: and no tag is constructed in two different places"
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

(printf "rows: ~a\n~a failures\nfacade-gate complete\n" rows failures)
(exit (if (zero? failures) 0 1))
