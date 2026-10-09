#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; The receipt on a read: what a reader was given is said in the answer.
;;
;; `(cut <alist>)` is the applied cut of the state the answer was read from;
;; `(versions ((<id> . <hash>) ...))` holds one pair for every block whose
;; content the answer shows or whose id it lists as a result, in order of
;; first appearance, each once, each the block-hash `--if-unchanged`
;; compares. Both come after the answer's own clauses, from one helper.
;;
;; IDENTITY WITH THE PRODUCT BEFORE RECEIPTS is a reading, not a row here:
;; read-receipt-base.ss, beside this file, runs one seeded store through both
;; products and compares them.
;;
;; THE CENSUS IS A TABLE OF EVERY VERB THE CATALOGUE HAS: the cut and the
;; versions, the cut only, or none with the reason. A verb the table does
;; not name is red, so a verb added later is classified or the file fails.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-block-ids block-hash reduce-applied-cut block-id)
        (only (theourgia log) log-publish! segment-sha writer-directory segment-file-name)
        (only (theourgia wire) encode-record)
        (only (theourgia render) render-human))

(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (caught got) expected))))

(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (write-file! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/read-receipt-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/s"))
(define (run . args) (rpc-dispatch store args "test"))
(define (state) (open-and-reduce store))

;; The clause headed `name` among an answer's elements, or #f. Not assq: an
;; error answer holds its name, a symbol, beside its clauses.
(define (clause answer name)
  (and (pair? answer) (list? answer)
       (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr answer))))
(define (clause-value answer name) (let ((c (clause answer name))) (and c (cadr c))))
(define (id-by-title title)
  (let ((s (state)))
    (find (lambda (id) (let ((row (state-read s id)))
                         (and row (equal? (cdr (or (assq 'title (cdr (assq 'fields row))) '(#f . #f))) title))))
          (state-block-ids s))))
(define (insert! title text . under)
  (apply run 'insert "--title" title "--text" text (if (pair? under) (list "--under" (car under)) '()))
  (id-by-title title))

;; ---- a seeded store ----------------------------------------------------------------

(define init-answer (run 'init))
(define WR (cadr (assq 'writer (cdr init-answer))))
(define X (insert! "Xray block" "xray body alpha"))
(define cut-after-x (reduce-applied-cut (state)))
(define Y (insert! "Yankee block" "yankee body beta" X))
;; TWO LINES THAT MATCH: grep lists Z twice, and its versions hold it once.
(define Z (insert! "Zulu block" "zulu body gamma\nzulu body again"))
(run 'link Y "cites" X)
(define D (insert! "Decision one" "the ruling"))
(run 'set D "kind" "decision")
(define T (insert! "Task one" "task body"))
(run 'set T "kind" "task")
(run 'set T "status" "todo")
(run 'link T "implements" D)
(define src (string-append root "/lib"))
(system (string-append "mkdir -p '" src "'"))
(write-file! (string-append src "/rr.sc")
             "(library (rr) (export h g) (import (rnrs))\n(define (h) 1)\n(define (g) (h)))\n")
(run 'import-code src "--datum")
(define (definition-of name)
  (let ((s (state)))
    (find (lambda (id)
            (let ((b (cdr (or (assq 'body (cdr (assq 'fields (state-read s id)))) '(#f . #f)))))
              (and (pair? b) (pair? (cdr b)) (pair? (cadr b)) (eq? (car (cadr b)) (string->symbol name)))))
          (state-block-ids s))))
(define H (definition-of "h"))
;; The library block: whereis lists its export record, so it is listed too.
(define LIB (let ((s (state)))
              (find (lambda (id) (eq? 'library (cdr (or (assq 'kind (cdr (assq 'fields (state-read s id)))) '(#f . #f)))))
                    (state-block-ids s))))
(define G (definition-of "g"))
;; A BLOCK THAT CANNOT BE HASHED: a field nested past the depth the describe
;; layer can encode (nesting-depth.sc's band), with a title search finds.
(define (nest n) (if (= n 0) "x" (string-append "(" (nest (- n 1)) ")")))
(run 'batch (string-append "((insert root #f ((kind . section) (title . \"needle unhashable\") (depth-probe . "
                           (nest 59) "))))"))
(define U (id-by-title "needle unhashable"))

(want "SETUP every seeded block exists"
      (map string? (list X Y Z D T H G U LIB))
      '(#t #t #t #t #t #t #t #t #t))

;; ---- the census ---------------------------------------------------------------------

(define cut-and-versions '(read search grep whereis refs commitments tasks names uses))
(define cut-only '(outline reach))
;; Every other verb, with the reason it carries no receipt.
(define none
  '((init "creates a store") (eval "evaluates source; its working-view names the cut it used")
    (insert "a write") (set "a write") (move "a write") (del "a write") (link "a write")
    (unlink "a write") (relation "a write") (rule "a write") (batch "a write") (commit "a write") (def "a write") (tag "a write or a tag listing")
    (import-code "a write") (import-md "a write") (adopt "a store operation") (snapshot "a store operation")
    (publish "a store operation") (supply "writes a derived table") (template "applies or exports a template")
    (write "a draft") (restore "a draft") (discard "a draft") (drafts "a draft listing")
    (diagnostics "an editor's supplied facts") (split-suggest "reads a file, not the store")
    (export-code "an export") (export-md "an export") (log "the log, not a read of the state")
    (diff "two cuts, not one state") (conflicts "conflicts, not a read of blocks")
    (check "the store's health") (describe "the catalogue")
    (query "answers its own cut and a digest of its rows")
    (context "answers its own receipt: the premises of what it must show, its cut and its versions")
    (subscribe "a stream: its frames name their own cuts, and read --rev names the publication")
    (scope "a write: the letter here and a new store; its answer names the cut it copied")
    (collect "a write: the review results under the letter")
    (collect-into "a write")
    (review-results "a datum for collect-into, which plans from its own store at its own locked point")))
(define (census table-verbs)
  (let ((catalogue (map car (verb-catalogue))))
    (list (filter (lambda (v) (not (memq v table-verbs))) catalogue)
          (filter (lambda (v) (not (memq v catalogue))) table-verbs))))
(define table-verbs (append cut-and-versions cut-only (map car none)))
(want "C48a-4 the census table names every verb of the catalogue and nothing else"
      (census table-verbs)
      '(() ()))
(want "C48a-4 CONTROL: a verb removed from the table is named by the census"
      (census (filter (lambda (v) (not (eq? v 'refs))) table-verbs))
      '((refs) ()))
(define (verb-of name)
  (if (memq name '(read read-md read-md-recursive read-recursive)) 'read name))
(define (symbols-sorted l) (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) l))

;; ---- the answers ---------------------------------------------------------------------

;; Each answer: its name, the answer, the blocks it lists IN THE ORDER IT
;; LISTS THEM -- read from the answer itself, first appearance, each once --
;; the set the seeded store should give (so a reading cannot be empty), and
;; the call that made it.
(define (items-of a) (let ((c (clause a 'items))) (if c (cdr c) '())))
(define (text-of a) (let ((c (clause a 'text))) (if (and c (string? (cadr c))) (cadr c) "")))
(define (position text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))
(define (title-of id) (cdr (or (assq 'title (cdr (assq 'fields (state-read (state) id)))) '(#f . "\x0;"))))
;; The blocks among `ids` whose title a markdown answer shows, in the order
;; it shows them.
(define (shown-in a ids)
  (let ((t (text-of a)))
    (map cdr (list-sort (lambda (x y) (< (car x) (car y)))
                        (filter car (map (lambda (id) (cons (position t (title-of id)) id)) ids))))))
(define (src-of id) (let ((e (assq 'src (cdr (assq 'fields (state-read (state) id)))))) (if (and e (string? (cdr e))) (cdr e) "\x0;")))
(define (record-id a) (list (cdr (assq 'id (cadr a)))))
(define (item-record-ids a) (map (lambda (r) (cdr (assq 'id r))) (items-of a)))
(define (answer name args extract expected) (list name (apply run args) extract expected args))
(define (dedup ids) (let loop ((l ids) (out '())) (cond ((null? l) (reverse out)) ((member (car l) out) (loop (cdr l) out)) (else (loop (cdr l) (cons (car l) out))))))
(define (second-of-items a) (dedup (map cadr (items-of a))))
(define answers
  (list (answer 'read (list 'read X) record-id (list X))
        ;; A markdown read of one block answers that block's own bytes, which
        ;; need not hold its title: the block it shows is the one whose
        ;; text it is.
        (answer 'read-md (list 'read X "--md")
                (lambda (a) (filter (lambda (id) (string=? (text-of a) (src-of id))) (list X Y)))
                (list X))
        (answer 'read-md-recursive (list 'read X "--md" "--recursive") (lambda (a) (shown-in a (list Y X))) (list X Y))
        (answer 'read-recursive (list 'read X "--recursive") item-record-ids (list X Y))
        (answer 'outline (list 'outline) #f #f)
        (answer 'refs (list 'refs X)
                (lambda (a) (dedup (cons X (map (lambda (r) (cadr (cadr r))) (items-of a))))) (list X Y))
        (answer 'reach (list 'reach H) #f #f)
        (answer 'search (list 'search "xray") second-of-items (list X))
        (answer 'grep (list 'grep "body") second-of-items (list X Y Z T))
        (answer 'whereis (list 'whereis "h") second-of-items (list H LIB))
        (answer 'commitments (list 'commitments "--all")
                (lambda (a) (dedup (apply append
                                          (map (lambda (r) (if (eq? (car r) 'decision)
                                                               (cons (cadr r)
                                                                     (append (map car (cdr (assq 'implemented-by (cddr r))))
                                                                             (let ((c (assq 'concurrent-with (cddr r)))) (if c (cdr c) '()))))
                                                               '()))
                                               (items-of a)))))
                (list D T))
        (answer 'tasks (list 'tasks) second-of-items (list T))
        (answer 'names (list 'names G) (lambda (a) (list G)) (list G))
        (answer 'uses (list 'uses "h") second-of-items (list G))))
(define now (state))
(define (answer-of name) (cadr (assq name answers)))

;; THE CLASSES ARE WHAT THE ROWS READ: the verbs whose answers this file
;; checks are exactly the two classes that carry a receipt, so moving a verb
;; to `none` turns this red; which clauses each answer must carry is read
;; from the class of its verb (below), so moving a verb between the two
;; classes turns its rows red; and every verb of `none` is run at the end of
;; this file, or named there with the reason it cannot be.
(want "C48a-4 the answers checked below are of exactly the verbs the table says carry a receipt"
      (symbols-sorted (dedup (map (lambda (e) (verb-of (car e))) answers)))
      (symbols-sorted (append cut-and-versions cut-only)))
(define (versions-of a) (or (clause-value a 'versions) 'NO-VERSIONS))

;; C48a-1: the cut, on every verb that carries one.
(for-each
  (lambda (e)
    (want (format "C48a-1 ~a answers (cut ...) equal to the applied cut" (car e))
          (in-order (car (cadr e)) (equal? (clause-value (cadr e) 'cut) (reduce-applied-cut now)))
          '(ok #t)))
  answers)
(want "C48a-1 read --cut answers the cut it read at, not the present one"
      (let ((a (run 'read X "--cut" (format "~s" cut-after-x))))
        (in-order (car a) (equal? (clause-value a 'cut) cut-after-x) (equal? cut-after-x (reduce-applied-cut now))))
      '(ok #t #f))

;; C48a-2: the versions are exactly the blocks listed, each once, each its hash.
(define (sorted ids) (list-sort string<? ids))
(define (class-of name)
  (cond ((memq (verb-of name) cut-and-versions) 'cut-and-versions)
        ((memq (verb-of name) cut-only) 'cut-only)
        (else 'none)))
(for-each
  (lambda (e)
    (when (eq? (class-of (car e)) 'cut-and-versions)
      (want (format "C48a-2 ~a, classified cut and versions, answers (versions ...) of exactly the listed blocks, in the order listed, each once and each its block-hash; the listed set is the seeded one" (car e))
            (let ((vs (versions-of (cadr e))) (listed ((caddr e) (cadr e))))
              (in-order (equal? (if (list? vs) (map car vs) vs) listed)
                        (and (list? vs) (for-all (lambda (p) (equal? (cdr p) (block-hash now (car p)))) vs))
                        (equal? (sorted listed) (sorted (cadddr e)))))
            '(#t #t #t))))
  answers)
(for-each
  (lambda (e)
    (when (eq? (class-of (car e)) 'cut-only)
      (want (format "C48a-2 ~a, classified cut only, carries a cut and no versions: it answers structure" (car e))
            (in-order (and (clause (cadr e) 'cut) #t) (clause (cadr e) 'versions))
            '(#t #f))))
  answers)
(define needle (run 'search "needle"))
(want "C48a-2 a hit on a block that cannot be hashed answers unavailable, and the answer is otherwise whole"
      (let* ((vs (versions-of needle)) (pair (and (list? vs) (assoc U vs))))
        (in-order (car needle)
                  (and (clause needle 'items) (map cadr (cdr (clause needle 'items))))
                  (and pair (cadr pair))
                  (and pair (pair? (cddr pair)) (car (caddr pair)))
                  (and (clause needle 'cut) #t)))
      (list 'ok (list U) 'unavailable 'reason #t))

;; ---- C48a-3: appended, nothing else changed ---------------------------------------------

;; The clauses THIS item appends, per answer: the last elements of each.
(define appended
  '((read cut versions) (read-md cut versions) (read-md-recursive cut versions)
    (read-recursive cut) (outline cut) (refs cut versions) (reach cut)
    (search versions) (grep versions) (whereis versions)
    (commitments cut versions) (tasks cut versions) (names cut versions) (uses cut versions)))
(define (strip name a)
  (let* ((k (length (cdr (assq name appended)))) (n (length a)))
    (list-head a (- n k))))
(for-each
  (lambda (e)
    (let* ((a (cadr e)) (heads (cdr (assq (car e) appended)))
           (tail (list-tail a (- (length a) (length heads)))))
      (want (format "C48a-3 ~a: the receipt's clauses are the answer's last elements, and the rest has none of them that it did not have" (car e))
            (in-order (map car tail)
                      (length (filter (lambda (x) (and (pair? x) (memq (car x) heads))) (cdr (strip (car e) a)))))
            (list heads 0))))
  answers)
;; THE VERBS PRINTED WHOLE ARE EXACTLY plain read AND reach: an answer that is
;; neither items nor text is printed whole by the renderer, so its receipt is
;; in its human output. Every other answer's human output is unchanged. A new
;; verb printed whole turns this row red instead of changing a human output.
(define (printed-whole? a)
  (not (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (memq (car (cadr a)) '(items text)))))
(want "C48a-3 the verbs whose answer is printed whole are exactly read and reach"
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 (dedup (map (lambda (e) (verb-of (car e))) (filter (lambda (e) (printed-whole? (cadr e))) answers))))
      '(reach read))
(for-each
  (lambda (e)
    (unless (printed-whole? (cadr e))
      ;; WHAT THIS ROW PROVES: the receipt adds nothing to what a person sees.
      ;; That the rest of the answer is the older product's is the reading
      ;; read-receipt-base.ss makes, not a row here.
      (want (format "C48a-3 ~a: the human output is the same with and without the receipt" (car e))
            (string=? (render-human (cadr e)) (render-human (strip (car e) (cadr e))))
            #t)))
  answers)
(want "C48a-3 reach: printed whole, its human output shows the cut and no versions"
      (let ((h (render-human (answer-of 'reach))))
        (in-order (string-contains? h "(cut ") (string-contains? h "(versions ")))
      '(#t #f))
(want "C48a-3 plain read: the human output shows the two clauses (it prints the answer whole)"
      (let ((h (render-human (answer-of 'read))))
        (in-order (string-contains? h "(cut ") (string-contains? h "(versions ")
                  (string-contains? h (render-human (strip 'read (answer-of 'read))))))
      '(#t #t #f))

;; A TITLE CHANGED ON ONE BLOCK: an answer listing it differs only in that
;; block's record, its version and the cut; one not listing it only in the cut.
(define (without-cut a) (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'cut)))) a))
(define before-x (run 'read X))
(define before-z (run 'read Z))
(define before-grep (run 'grep "zulu"))
(run 'set X "title" "Xray renamed")
(define after-x (run 'read X))
(define after-z (run 'read Z))
(define after-grep (run 'grep "zulu"))
(want "C48a-3 after the title of X changed: read X differs in X's record, X's version and the cut"
      (in-order (equal? (cadr before-x) (cadr after-x))
                (equal? (clause-value before-x 'versions) (clause-value after-x 'versions))
                (equal? (clause-value before-x 'cut) (clause-value after-x 'cut))
                (equal? (map car (clause-value before-x 'versions)) (map car (clause-value after-x 'versions))))
      '(#f #f #f #t))
(want "C48a-3 and answers that do not list X differ only in the cut: read Z, grep zulu"
      (in-order (equal? (without-cut before-z) (without-cut after-z))
                (equal? (clause-value before-z 'cut) (clause-value after-z 'cut))
                (equal? (without-cut before-grep) (without-cut after-grep)))
      '(#t #f #t))

;; ---- one answer, one state ---------------------------------------------------------------
;;
;; A STATE GIVEN TO THE DISPATCH: `now` is the state every answer above was
;; read from. After a link to X, a new hit and X's title changed again, each
;; call given `now` answers what it answered then, byte for byte -- rows and
;; receipt alike -- so a receipt or a row taken from a fold of its own
;; differs. search and grep fold the store themselves whatever they are
;; given (unchanged by this item): their rows and their receipt both come
;; from that fold, which is the present one.
(run 'link Z "cites" X)
(insert! "Xray late" "xray body late")
(run 'set X "title" "Xray renamed again")
(define present (state))
(want "SETUP the present differs from `now`: the cut, X's hash"
      (in-order (equal? (reduce-applied-cut present) (reduce-applied-cut now))
                (equal? (block-hash present X) (block-hash now X)))
      '(#f #f))
(for-each
  (lambda (e)
    (let ((given (rpc-dispatch store (list-ref e 4) "test" now)))
      (if (memq (car e) '(search grep))
          (want (format "C48a-1 ~a given an earlier state: its rows and its receipt come from its own fold, the present one" (car e))
                (let ((fresh (apply run (list-ref e 4))))
                  (in-order (equal? given fresh)
                            (equal? (versions-of given) (versions-of (cadr e)))
                            (and (list? (versions-of given))
                                 (for-all (lambda (p) (equal? (cdr p) (block-hash present (car p)))) (versions-of given)))))
                '(#t #f #t))
          (want (format "C48a-1 ~a given an earlier state answers what it answered at that state, receipt included" (car e))
                (equal? given (cadr e))
                #t))))
  answers)

;; ---- C48a-4: every verb classified none, run --------------------------------------------------
;;
;; Each verb the table classifies none is run here on this store (writes on
;; a probe block of their own), and answers without an error and with no
;; (cut ...) and no (versions ...) clause. A verb that cannot be run here is
;; named below with the reason; the two lists together are the whole class.
(define probe (insert! "Probe block" "probe text"))
(define export-dir (string-append root "/export"))
(define md-dir (string-append root "/md"))
(system (string-append "mkdir -p '" export-dir "/md' '" export-dir "/code' '" md-dir "'"))
(write-file! (string-append md-dir "/note.md") "# A note\n\nSome text.\n")
;; eval is answered by the command line's program, not the dispatcher: it
;; is run as a caller runs it, its wire answer read back.
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (cli-wire . args)
  (let* ((out (string-append root "/cli.out"))
         (rc (system (string-append "scheme --script ../core.sc " (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                    "--store " (quoted store) " --wire > " (quoted out) " 2>/dev/null < /dev/null")))
         (a (guard (e (#t (list 'error 'unreadable (file-text out)))) (read (open-string-input-port (file-text out))))))
    ;; NO ANSWER IS NOT AN ANSWER: a failed run or an empty output is said
    ;; as such, so the row below cannot read it as a clean one.
    (cond ((not (= rc 0)) (list 'error 'command-failed rc (file-text out)))
          ((eof-object? a) (list 'error 'no-answer))
          (else a))))
(define none-calls
  (list (list 'init (lambda () (rpc-dispatch (string-append root "/s3") '(init) "test")))
        (list 'eval (lambda () (cli-wire "eval" "(+ 1 2)")))
        (list 'insert (lambda () (run 'insert "--title" "Probe two" "--text" "two")))
        (list 'set (lambda () (run 'set probe "title" "Probe block, set")))
        (list 'move (lambda () (run 'move probe X)))
        (list 'link (lambda () (run 'link probe "cites" Y)))
        (list 'unlink (lambda () (run 'unlink probe "cites" Y)))
        (list 'relation (lambda () (run 'relation "annotates" "--as" "nothing")))
        (list 'rule (lambda () (run 'rule "sections-titled" "--on" "section" "--must" "(title ?w ?t)")))
        (list 'batch (lambda () (run 'batch (format "((set ~s title \"Probe block, batch\"))" probe))))
        (list 'def (lambda () (run 'def "probe-fn" "--under" LIB "(define (probe-fn) 1)")))
        (list 'tag (lambda () (run 'tag "probe-tag")))
        (list 'write (lambda () (run 'write probe "draft two" "--writer" WR)))
        (list 'drafts (lambda () (run 'drafts "--writer" WR)))
        (list 'diagnostics (lambda () (run 'diagnostics "--writer" WR)))
        (list 'discard (lambda () (run 'discard probe "--writer" WR)))
        (list 'commit (lambda () (run 'write probe "draft three" "--writer" WR) (run 'commit probe "--writer" WR)))
        (list 'import-md (lambda () (run 'import-md md-dir)))
        (list 'import-code (lambda () (run 'import-code src "--datum")))
        (list 'export-md (lambda () (run 'export-md (string-append export-dir "/md"))))
        (list 'export-code (lambda () (run 'export-code (string-append export-dir "/code") "--datum")))
        (list 'split-suggest (lambda () (run 'split-suggest (string-append md-dir "/note.md"))))
        (list 'snapshot (lambda () (run 'snapshot)))
        (list 'log (lambda () (run 'log X)))
        (list 'diff (lambda () (run 'diff (format "~s" cut-after-x) (format "~s" (reduce-applied-cut (state))))))
        (list 'conflicts (lambda () (run 'conflicts)))
        (list 'check (lambda () (run 'check)))
        (list 'describe (lambda () (run 'describe)))
        (list 'template (lambda () (run 'template "apply" "memory")))
        (list 'query (lambda () (run 'query "(kind ?x section)")))
        (list 'context (lambda () (run 'context "--for" T "--budget" "100000")))
        ;; THE CHANNEL, in this order: the three after scope read or write
        ;; what it made. Nothing in the scoped store was written by the
        ;; letter's reader, so the results are empty. scope and collect are
        ;; core.sc's programs, as eval is, and run through it.
        (list 'scope (lambda () (cli-wire "scope" (string-append root "/scoped") "--cut" (format "~s" (reduce-applied-cut (state)))
                                          "--roots" X "--for" "codex")))
        (list 'review-results (lambda () (rpc-dispatch (string-append root "/scoped") '(review-results) "test")))
        (list 'collect-into (lambda () (run 'collect-into X "--results" "(results (letter \"x\") (to \"codex\") (blocks))")))
        (list 'collect (lambda () (cli-wire "collect" (string-append root "/scoped"))))
        (list 'del (lambda () (run 'del probe)))))
(define none-not-run
  '((adopt "binds a store written on another machine; this store has one writer, its own")
    (publish "takes another writer's segment file; this store has no other writer")
    (supply "takes an editor's derived table; this file supplies none")
    (restore "takes back a draft another writer's concurrent commit revoked; this store has one writer")
    (subscribe "a stream a daemon carries; in process it answers needs-daemon, which is no success")))
(want "C48a-4 the verbs run below and the verbs named as not run are together the class none, each once"
      (in-order (symbols-sorted (append (map car none-calls) (map car none-not-run)))
                (length (append (map car none-calls) (map car none-not-run))))
      (in-order (symbols-sorted (map car none)) (length none)))
;; A VERB WHOSE OWN ANSWER ALREADY SAID ITS CUT, before receipts existed:
;; snapshot names the cut it froze. That clause is the verb's, not a receipt.
(define own-cut '(snapshot query context scope))
;; AND ONE WHOSE OWN ANSWER SAYS ITS VERSIONS: context's receipt is its own,
;; the versions of what it shows beside it.
(define own-versions '(context))
(for-each
  (lambda (c)
    (want (format "C48a-4 ~a, classified none, answers a success (rpc-ok?) with no (versions ...) clause, and no (cut ...) unless the verb's own" (car c))
          (let ((a ((cadr c))))
            ;; SUCCESS IS THE TREE'S RULE, rpc-ok?: a usage answer, or a batch
            ;; or an import with one failing intent, is not one.
            (in-order (if (rpc-ok? a) #f (list 'NOT-A-SUCCESS a))
                      (and (clause a 'cut) #t) (and (clause a 'versions) #t)))
          (list #f (and (memq (car c) own-cut) #t) (and (memq (car c) own-versions) #t))))
  none-calls)

;; ---- the dispatcher's (incomplete ...) follows the receipt -------------------------------
;;
;; A store whose mirror's older segment has readable damage: a read answers
;; with an (incomplete ...) clause, which the dispatcher adds after the
;; handler answered. The order is the handler's clauses, the receipt, the
;; dispatcher's; with the receipt removed the answer is what it was, and a
;; person still sees the incomplete clause.
(define M "zzzzzzzz")
(define store2 (string-append root "/s2"))
(system (string-append "mkdir -p '" root "/home2'"))
(putenv "THEOURGIA_HOME" (string-append root "/home2"))
(rpc-dispatch store2 '(init) "test")
(define B2
  (let* ((a (rpc-dispatch store2 '(insert "--title" "Beta" "--text" "a needle") "test"))
         (e (car (cadr (assq 'events (cdr a))))))
    (block-id (car e) (cdr e))))
(let loop ((rs (list (encode-record 1 1757300000001 "peer" '() '(set "t" x "x"))
                     (encode-record 2 1757300000002 "peer" '() '(set "t" y "y"))))
           (k 1))
  (unless (null? rs)
    (log-publish! store2 M k (car rs) (segment-sha (car rs)))
    (loop (cdr rs) (+ k 1))))
(let* ((path (string-append (writer-directory store2 M) "/" (segment-file-name 1)))
       (bv (call-with-port (open-file-input-port path) get-bytevector-all))
       (i (quotient (bytevector-length bv) 2)))
  (bytevector-u8-set! bv i (fxlogxor (bytevector-u8-ref bv i) 1))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (tail-heads a k)
  (let ((hs (map (lambda (c) (and (pair? c) (car c))) (cdr a))))
    (list-tail hs (max 0 (- (length hs) k)))))
(define (heads a) (map (lambda (c) (and (pair? c) (car c))) (cdr a)))
(define (without-heads a hs) (filter (lambda (c) (not (and (pair? c) (memq (car c) hs)))) a))
(define inc-outline (rpc-dispatch store2 '(outline) "test"))
(define inc-search (rpc-dispatch store2 '(search "needle") "test"))
(define inc-read (rpc-dispatch store2 (list 'read B2) "test"))
(want "C48a-3 an incomplete store: the receipt, then the dispatcher's (incomplete ...): outline, search, read"
      (list (tail-heads inc-outline 2) (tail-heads inc-search 2) (tail-heads inc-read 3))
      '((cut incomplete) (versions incomplete) (cut versions incomplete)))
(want "C48a-3 an incomplete store: with the receipt removed, outline and plain read are their own clauses then incomplete"
      (list (heads (without-heads inc-outline '(cut)))
            (cdr (heads (without-heads inc-read '(cut versions)))))
      '((text incomplete) (version incomplete)))
(want "C48a-3 an incomplete store: a person still sees the incomplete clause of outline and of read"
      (in-order (string-contains? (render-human inc-outline) "incomplete")
                (string-contains? (render-human inc-read) "incomplete"))
      '(#t #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nread-receipt complete\n" bad rows)
(exit (if (= bad 0) 0 1))
