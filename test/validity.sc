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

;; Class and validity as a caller meets them: the `class` field at each
;; route, the clause a read adds for a block that is not valid, what search,
;; grep and whereis leave out and say, the caps, the human output, the
;; write protocol, and a store with no effect-bearing link, which answers as
;; before and loads neither library.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read reduce-applied-cut block-id known-classes)
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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/validity-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/s"))
(define (run . args) (rpc-dispatch store args "test"))
(define (state) (open-and-reduce store))

;; The clause headed `name` among an answer's elements, or #f.
(define (clause answer name)
  (and (pair? answer) (list? answer)
       (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr answer))))
(define (clause-value answer name) (let ((c (clause answer name))) (and c (cdr c))))
(define (items-of a) (let ((c (clause a 'items))) (if c (cdr c) '())))
(define (dedup l) (let loop ((l l) (out '())) (cond ((null? l) (reverse out)) ((member (car l) out) (loop (cdr l) out)) (else (loop (cdr l) (cons (car l) out))))))
(define (new-id answer)
  (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev))))
    (block-id (car e) (cdr e))))
(define (ins! title text . under)
  (new-id (apply run 'insert "--title" title "--text" text
                 (if (pair? under) (list "--under" (car under)) '()))))
(define (field-value id f)
  (let ((e (assq f (cdr (assq 'fields (state-read (state) id)))))) (and e (cdr e))))

(run 'init)

;; ---- C49-1: the class field at each route ------------------------------------------------

(define K (ins! "Class target" "plain"))
(want "C49-1 set <id> class inference stores the symbol"
      (in-order (car (run 'set K "class" "inference")) (field-value K 'class))
      '(ok inference))
(want "C49-1 through batch a symbol is stored"
      (in-order (rpc-ok? (run 'batch (format "((set ~s class external))" K))) (field-value K 'class))
      '(#t external))
;; THE SAME DETAIL, EACH ROUTE'S ENVELOPE: a class refusal is the kind
;; refusal on the same route with the field's name and list in place.
(define (swap x)
  (cond ((eq? x 'kind) 'class)
        ((eq? x 'kind-not-known) 'class-not-known)
        ((equal? x '(code section file doc library decision task template)) known-classes)
        ((pair? x) (cons (swap (car x)) (swap (cdr x))))
        (else x)))
(define (refusal-pair make)
  (let ((k (make "kind")) (c (make "class")))
    (in-order (rpc-ok? c) (equal? (swap k) c) (and (memq 'class-not-known (flatten c)) #t)
              (and (string-contains? (format "~s" c) "observation inference ruling verification external") #t))))
(define (flatten x) (cond ((pair? x) (append (flatten (car x)) (flatten (cdr x)))) ((null? x) '()) (else (list x))))
(want "C49-1 the command line refuses an unknown class by name, as it refuses an unknown kind"
      (refusal-pair (lambda (f) (run 'set K f "nonsense")))
      '(#f #t #t #t))
(want "C49-1 batch refuses an unknown symbol with the known list, as it refuses an unknown kind"
      (refusal-pair (lambda (f) (run 'batch (format "((set ~s ~a nonsense))" K f))))
      '(#f #t #t #t))
(want "C49-1 batch refuses a string with the known list, as it refuses a string kind"
      (refusal-pair (lambda (f) (run 'batch (format "((set ~s ~a \"inference\"))" K f))))
      '(#f #t #t #t))
(want "C49-1 the refused writes left the class as it was"
      (field-value K 'class)
      'external)

;; ---- the mode field: the command line's word is the symbol a mode is compared with ----------
;;
;; NEVER: `set <id> mode datum` FROM THE COMMAND LINE STORED THE STRING "datum".
;; Everything that reads a mode compares it with the symbols text and datum,
;; so the block never behaved as a datum block, and a second `set ... mode
;; datum` on a real datum block answered mode-mismatch. The blocks here hold
;; no src: mode datum on a block holding one is refused by another rule.
(define (bare! title) (new-id (run 'insert "--title" title)))
;; The tail of the first list anywhere in X that begins with HEAD, or #f.
(define (from-head x head)
  (cond ((and (pair? x) (eq? (car x) head)) x)
        ((pair? x) (or (from-head (car x) head) (from-head (cdr x) head)))
        (else #f)))
(define M (bare! "Mode target"))
(want "MODE-1 set <id> mode datum from the command line stores the symbol"
      (in-order (car (run 'set M "mode" "datum")) (field-value M 'mode))
      '(ok datum))
;; A DATUM BLOCK WHOSE MODE IS THE SYMBOL, made through batch (which hands
;; the symbol over), asked the command line's same set: the string would not
;; equal it and was answered mode-mismatch.
(define MD (bare! "Datum mode target"))
(want "MODE-1 the command line's set mode datum on a datum block is taken, not mode-mismatch"
      (in-order (rpc-ok? (run 'batch (format "((set ~s mode datum))" MD))) (field-value MD 'mode)
                (car (run 'set MD "mode" "datum")))
      '(#t datum ok))
(define MT (bare! "Text mode target"))
(want "MODE-1 TWIN: set <id> mode text stores the symbol"
      (in-order (car (run 'set MT "mode" "text")) (field-value MT 'mode))
      '(ok text))
(define MX (bare! "Unknown mode target"))
(define mode-cli (run 'set MX "mode" "nonsense"))
(define MY (bare! "Unknown mode target, batch"))
(define mode-batch (run 'batch (format "((set ~s mode nonsense))" MY)))
(want "MODE-2 the command line refuses an unknown mode by name, with the known list"
      (from-head mode-cli 'mode-not-known)
      '(mode-not-known (mode "nonsense") (known (text datum))))
(want "MODE-2 batch refuses it on a block of its own with the same detail"
      (in-order (rpc-ok? mode-batch) (from-head mode-batch 'mode-not-known))
      '(#f (mode-not-known (mode "nonsense") (known (text datum)))))
(want "MODE-2 the refused writes left both blocks without a mode"
      (list (field-value MX 'mode) (field-value MY 'mode))
      '(#f #f))

;; ---- C49-3: what a read says ----------------------------------------------------------------

(define G (ins! "Ground" "ground"))
(define P (ins! "Prior ruling" "prior" G))
(define C (ins! "A claim" "claim" G))
(define W (ins! "Some work" "work" G))
(define V (ins! "Valid one" "valid" G))
(define N (ins! "Newer ruling" "newer"))
(define E (ins! "Evidence" "evidence"))
(define Q (ins! "Premise" "premise"))
(define G2 (ins! "Quiet ground" "quiet"))
(define V2 (ins! "Quiet child" "quiet child" G2))
(define before-links (reduce-applied-cut (state)))
(run 'link N "supersedes" P)
(run 'link E "refutes" C)
(run 'link W "depends-on" Q)
(run 'set Q "title" "Premise, revised")
(want "C49-3 plain read of a superseded, a refuted and a needs-review block ends its own clauses with (validity ...)"
      (map (lambda (id) (clause-value (run 'read id) 'validity)) (list P C W))
      (list (list 'superseded (list 'superseded-by N))
            (list 'refuted (list 'refuted-by E))
            (list 'needs-review (list 'premise-moved Q))))
(want "C49-3 the clause comes after the version and before the receipt; a valid block has none"
      (in-order (map (lambda (c) (and (pair? c) (car c))) (cddr (run 'read P)))
                (map (lambda (c) (and (pair? c) (car c))) (cddr (run 'read V))))
      '((version validity cut versions) (version cut versions)))
(want "C49-3 read --recursive lists exactly the blocks that are not valid, in item order"
      (map car (car (clause-value (run 'read G "--recursive") 'validity)))
      (list P C W))
(want "C49-3 read --recursive with every block valid has no clause"
      (clause (run 'read G2 "--recursive") 'validity)
      #f)
(want "C49-3 read --cut before the superseding link has no clause"
      (clause (run 'read P "--cut" (format "~s" before-links)) 'validity)
      #f)

;; ---- C49-4: search, grep and whereis leave out and say ----------------------------------------

(define S1 (ins! "kestrel superseded" "kestrel superseded body"))
(define R1 (ins! "kestrel refuted" "kestrel refuted body"))
(define NR1 (ins! "kestrel review" "kestrel review body"))
(define OK1 (ins! "kestrel fine" "kestrel fine body"))
(define S1n (ins! "Successor" "nothing to see"))
(define R1e (ins! "Counter" "nothing to see"))
(define Q1 (ins! "Footing" "footing"))
(run 'link S1n "supersedes" S1)
(run 'link R1e "refutes" R1)
(run 'link NR1 "depends-on" Q1)
(run 'set Q1 "title" "Footing, revised")
(define (hit-ids a) (map cadr (items-of a)))
(define (sorted ids) (list-sort string<? ids))
(define (validity-ids a) (let ((v (clause-value a 'validity))) (if v (map (lambda (r) (list (car r) (cadr r))) (car v)) '())))
(define (excluded a) (let ((e (clause-value a 'excluded))) (and e (car e))))
(for-each
  (lambda (verb)
    (let ((a (run verb "kestrel")) (all (run verb "kestrel" "--all-validity")))
      (want (format "C49-4 ~a leaves out the superseded and the refuted block and counts them in blocks; the needs-review hit is returned and named" verb)
            (in-order (sorted (dedup (hit-ids a))) (excluded a) (validity-ids a))
            (list (sorted (list NR1 OK1)) '(blocks (superseded 1) (refuted 1)) (list (list NR1 'needs-review))))
      (want (format "C49-4 ~a --all-validity returns every hit and names the three that are not valid" verb)
            (in-order (sorted (dedup (hit-ids all))) (excluded all) (sorted (map car (validity-ids all))))
            (list (sorted (list S1 R1 NR1 OK1)) #f (sorted (list S1 R1 NR1))))))
  '(search grep))
(want "C49-4 the needs-review hit's reason names the premise"
      (car (clause-value (run 'search "kestrel") 'validity))
      (list (list NR1 'needs-review (list 'premise-moved Q1))))

;; whereis, over a datum library: wkx exported and defined, wkhidden only
;; defined, wkref defined and refuted.
(define src (string-append root "/lib"))
(system (string-append "mkdir -p '" src "'"))
(write-file! (string-append src "/wk.sc")
             "(library (wk) (export wkx) (import (rnrs))\n(define (wkx) 1)\n(define (wkhidden) 2)\n(define (wkref) 3))\n")
(run 'import-code src "--datum")
(define (def-of name) (let ((r (find (lambda (r) (eq? (car r) 'def)) (items-of (run 'whereis name))))) (and r (cadr r))))
(define WKH (def-of "wkhidden"))
(define WKR (def-of "wkref"))
(run 'link S1n "supersedes" WKH)
(run 'link R1e "refutes" WKR)
(want "C49-4c whereis of a name whose only definition is superseded answers ok, no items, and the excluded clause"
      (let ((a (run 'whereis "wkhidden")))
        (in-order (car a) (items-of a) (excluded a)))
      '(ok () (blocks (superseded 1) (refuted 0))))
(want "C49-4c whereis of a refuted definition likewise; of a name nothing defines, unknown-name as before"
      (in-order (excluded (run 'whereis "wkref")) (cadr (run 'whereis "wknothing")))
      '((blocks (superseded 0) (refuted 1)) unknown-name))
(want "C49-4c whereis --all-validity returns the definition and names it"
      (let ((a (run 'whereis "wkhidden" "--all-validity")))
        (in-order (map cadr (items-of a)) (excluded a) (validity-ids a)))
      (list (list WKH) #f (list (list WKH 'superseded))))

;; ---- C49-4b: the caps ---------------------------------------------------------------------------------

;; SEARCH: ten hits in force with the word in the title and the text, and one
;; superseded block -- ranked last (the word in its text only), or ranked
;; first (in its keywords, title and text).
(define (osprey-store! word first?)
  (let ((ids (map (lambda (k) (ins! (format "~a ~a" word k) (format "~a text ~a" word k))) (iota 10)))
        (sup (if first?
                 (new-id (run 'insert "--title" (format "~a top" word) "--text" (format "~a top" word) "--keywords" word))
                 (ins! "low one" (format "plain ~a" word)))))
    (run 'link S1n "supersedes" sup)
    ids))
(define low-ids (osprey-store! "osprey" #f))
(define top-ids (osprey-store! "merlin" #t))
(want "C49-4b search: the limit of hits in force, then one superseded hit: excluded 1, truncated absent"
      (let ((a (run 'search "osprey")))
        (in-order (sorted (hit-ids a)) (excluded a) (clause a 'truncated)))
      (list (sorted low-ids) '(blocks (superseded 1) (refuted 0)) #f))
(want "C49-4b search: the superseded hit ranked first: the limit is still filled with hits in force"
      (let ((a (run 'search "merlin")))
        (in-order (sorted (hit-ids a)) (excluded a) (clause a 'truncated)))
      (list (sorted top-ids) '(blocks (superseded 1) (refuted 0)) #f))
;; GREP: ten blocks of twenty matching lines in force and one superseded
;; block of twenty, under one parent each, the superseded block first in
;; one and last in the other.
(define (twenty word k) (apply string-append (map (lambda (i) (format "~a line ~a ~a\n" word k i)) (iota 20))))
(define (heron-ground! title sup-first?)
  (let* ((g (ins! title "ground text"))
         (sup-block (lambda () (let ((s (ins! (format "~a sup" title) (twenty "heron" "sup") g))) (run 'link S1n "supersedes" s) s)))
         (s0 (and sup-first? (sup-block)))
         (ids (map (lambda (k) (ins! (format "~a ~a" title k) (twenty "heron" k) g)) (iota 10)))
         (s1 (and (not sup-first?) (sup-block))))
    (list g ids)))
(define heron-first (heron-ground! "Heron first" #t))
(define heron-last (heron-ground! "Heron last" #f))
(for-each
  (lambda (h name)
    (want (format "C49-4b grep, the superseded block ~a: 200 lines, all from blocks in force, excluded 1 block, nothing truncated" name)
          (let ((a (run 'grep "heron" "--under" (car h))))
            (in-order (length (items-of a)) (sorted (dedup (hit-ids a))) (excluded a) (clause a 'truncated)))
          (list 200 (sorted (cadr h)) '(blocks (superseded 1) (refuted 0)) #f)))
  (list heron-first heron-last) '("first" "last"))

;; ---- C49-4d: the human output ---------------------------------------------------------------------------

(define (human-is-items? a) (string=? (render-human a) (render-human (list 'ok (cons 'items (items-of a))))))
(want "C49-4d search and grep print the hits in force, one per line, and nothing else; whereis, every hit left out, prints the excluded line; the wire form has the clauses"
      (let ((as (list (run 'search "kestrel") (run 'grep "kestrel") (run 'whereis "wkhidden"))))
        (in-order (map human-is-items? as)
                  (map (lambda (a) (length (filter (lambda (l) (> (string-length l) 0))
                                                   (let split ((cs (string->list (render-human a))) (cur '()) (out '()))
                                                     (cond ((null? cs) (reverse (cons (list->string (reverse cur)) out)))
                                                           ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse cur)) out)))
                                                           (else (split (cdr cs) (cons (car cs) cur) out)))))))
                       as)
                  (map (lambda (a) (and (or (clause a 'excluded) (clause a 'validity)) #t)) as)))
      '((#t #t #f) (2 2 1) (#t #t #t)))
;; Every hit left out is not no hit: with no item left the human output is
;; the excluded line, and a search that matched nothing prints nothing.
(define PL (ins! "plover" "plover body"))
(define PLn (ins! "Later" "nothing to see"))
(run 'link PLn "supersedes" PL)
(want "C49-4d a search or grep whose every hit was left out prints the excluded line, one that matched nothing prints nothing"
      (in-order (map (lambda (verb) (render-human (run verb "plover"))) '(search grep))
                (map (lambda (verb) (render-human (run verb "qqzzunmatched"))) '(search grep)))
      (list (list "(excluded (blocks (superseded 1) (refuted 0)))\n" "(excluded (blocks (superseded 1) (refuted 0)))\n")
            (list "" "")))
(want "C49-4d the wire forms are as they were: no items and the excluded clause, no items and none"
      (in-order (map (lambda (verb) (let ((a (run verb "plover"))) (list (items-of a) (excluded a)))) '(search grep))
                (map (lambda (verb) (let ((a (run verb "qqzzunmatched"))) (list (items-of a) (excluded a)))) '(search grep)))
      (list (list (list '() '(blocks (superseded 1) (refuted 0))) (list '() '(blocks (superseded 1) (refuted 0))))
            (list (list '() #f) (list '() #f))))

;; ---- C49-7: the write protocol --------------------------------------------------------------------------

(define sentences
  '("set <id> class inference for what you concluded and did not verify"
    "link <new> supersedes <old>"
    "link <work> depends-on <premise>"
    "linking again is how you say you checked"))
(want "C49-7 describe's protocol text, the constant and README carry the four sentences"
      (in-order (map (lambda (s) (string-contains? write-protocol s)) sentences)
                (let ((t (format "~s" (run 'describe)))) (map (lambda (s) (string-contains? t s)) sentences))
                (let ((t (file-text "../README.md"))) (map (lambda (s) (string-contains? t s)) sentences)))
      '((#t #t #t #t) (#t #t #t #t) (#t #t #t #t)))

;; ---- C49-8: two writers, seen through read --------------------------------------------------------------

;; A state with concurrent writers, handed to the dispatch: d = a.1 is a
;; decision, i = a.2 implements it; two writers each edit a part of i and
;; each link. No single witness has seen all of i: the reason names d.
(define (state-of . events)
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (apply reduce-apply! r e)) events)
    r))
(define split
  (state-of '("a" 1 () (put ((kind . decision) (title . "D"))))
            '("a" 2 () (put ((kind . code) (title . "impl"))))
            '("b" 1 (("a" . 2)) (set "a.2" title "by-b")) '("b" 2 () (link "a.2" implements "a.1"))
            '("c" 1 (("a" . 2)) (set "a.2" src "by-c")) '("c" 2 () (link "a.2" implements "a.1"))))
(define concurrent-link-and-edit
  (state-of '("a" 1 () (put ((kind . section) (title . "premise"))))
            '("a" 2 () (put ((kind . section) (title . "work"))))
            '("a" 3 () (link "a.2" depends-on "a.1"))
            '("b" 1 (("a" . 2)) (set "a.1" title "premise by b"))
            '("a" 4 () (set "a.2" title "work by a"))))
(want "C49-8 read through a two-writer state: the reasons name the edge's other end"
      (in-order (clause-value (rpc-dispatch store '(read "a.2") "test" split) 'validity)
                (clause-value (rpc-dispatch store '(read "a.2") "test" concurrent-link-and-edit) 'validity))
      '((needs-review (implementation-moved "a.1"))
        (needs-review (premise-moved "a.1"))))

;; ---- C49-5: a store with no effect-bearing link -----------------------------------------------------------

;; A CHILD PROCESS, because this file loads both libraries itself: on a store
;; linked only by relations with no effect (documents among them), read,
;; read --recursive, search, grep and whereis (of a name the store defines)
;; each answer a success carrying no clause this item adds -- no validity,
;; no excluded -- and neither library is loaded. The same child then reads a
;; store that has one, which shows the census could say yes. That the
;; answers are the older product's, byte for byte, is a reading:
;; validity-base.ss beside this file runs the same store through both.
(define quiet (string-append root "/quiet"))
(define child (string-append root "/census.sc"))
(define quiet-lib (string-append root "/quiet-lib"))
(rpc-dispatch quiet '(init) "test")
(system (string-append "mkdir -p '" quiet-lib "'"))
(write-file! (string-append quiet-lib "/qq.sc") "(library (qq) (export qx) (import (rnrs))\n(define (qx) 1))\n")
(rpc-dispatch quiet (list 'import-code quiet-lib "--datum") "test")
(let* ((a (new-id (rpc-dispatch quiet '(insert "--title" "alpha osprey" "--text" "alpha text") "test")))
       (b (new-id (rpc-dispatch quiet (list 'insert "--title" "beta osprey" "--text" "beta text" "--under" a) "test"))))
  (rpc-dispatch quiet (list 'link a "documents" b) "test")
  (rpc-dispatch quiet (list 'link b "cites" a) "test")
  (write-file! child
    (string-append
      "(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))\n"
      "(register-verbs! extension-verbs)\n"
      "(define (loaded) (list-sort (lambda (x y) (string<? (symbol->string (cadr x)) (symbol->string (cadr y)))) (filter (lambda (l) (member l '((theourgia lifecycle) (theourgia attest)))) (library-list))))\n"
      "(define (ask st . r) (rpc-dispatch st r \"test\"))\n"
      "(define (clean? a) (and (rpc-ok? a) (not (exists (lambda (c) (and (pair? c) (memq (car c) '(validity excluded)))) (cdr a)))))\n"
      (format "(define answers (list (ask ~s 'read ~s) (ask ~s 'read ~s \"--recursive\") (ask ~s 'search \"osprey\") (ask ~s 'grep \"text\") (ask ~s 'whereis \"qx\")))\n"
              quiet a quiet a quiet quiet quiet)
      "(define quiet-loaded (loaded))\n"
      (format "(ask ~s 'read ~s)\n" store P)
      "(write (list (map clean? answers) (map (lambda (a) (length (cdr a))) answers) quiet-loaded (loaded)))\n")))
(want "C49-5 on a store with no effect-bearing link each verb answers a success with no clause this item adds, and neither library is loaded; on one with, a read loads both"
      (let ((out (string-append root "/census.out")))
        (system (string-append "scheme --script '" child "' > '" out "' 2>/dev/null < /dev/null"))
        (let ((r (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-string-input-port (file-text out))))))
          (if (and (list? r) (= 4 (length r))) (list (car r) (caddr r) (cadddr r)) r)))
      '((#t #t #t #t #t) () ((theourgia attest) (theourgia lifecycle))))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nvalidity complete\n" rows bad)
(exit (if (= bad 0) 0 1))
