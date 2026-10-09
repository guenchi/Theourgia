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

;; A block of mode datum takes no src.
;;
;; Its text is its body, written by `def`; export-code --datum, whereis and
;; eval read the body. `write` and `commit` used to answer ok on such a
;; block and leave a src beside the body that nothing read: an edit
;; reported saved that never took effect. Every route that would put a src
;; there now refuses by name before anything is written, and `check` lists
;; a block that already holds one.
;;
;; WHAT A REFUSAL MUST NOT CHANGE is read from the disk: the bytes of the
;; writers' segments and the list of the store's files, before and after.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-block-ids block-hash reduce-applied-cut draft-version)
        (only (theourgia log) writer-directory)
        (only (theourgia wire) sexpr->string-extended storable-encode string->sexpr-extended)
        (only (theourgia crc32) crc32-hex))

(include "forge-record.ss")
(include "forge-draft.ss")

;; THE VALUES A ROW READS ARE TAKEN IN THE ORDER THEY ARE WRITTEN: R6RS
;; leaves the order of a call's arguments open.
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
    ((_ name got expected)
     ;; AN EXPECTED VALUE THAT RAISES IS A FAIL LINE, with no comparison: any
     ;; value standing for the raise could be equalled by a row, and with
     ;; `caught` on both sides two rows raising the same message would read
     ;; as agreeing.
     (call-with-current-continuation
       (lambda (k)
         (let ((x (guard (e (#t (expected-raised! name e) (k #f))) expected)))
           (want-1 name (caught got) x)))))))
(define (expected-raised! name e)
  (set! rows (+ rows 1))
  (set! bad (+ bad 1))
  (printf "FAIL ~a -> the expected value raised ~s\n" name
          (if (and (condition? e) (message-condition? e)) (condition-message e) e)))

(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (file-bytes path)
  (if (file-exists? path)
      (call-with-port (open-file-input-port path) get-bytevector-all)
      'ABSENT))
(define (write-file! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'replace))

(register-verbs! extension-verbs)

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/datum-draft-" (number->string (get-process-id))))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/dd" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" root "/home' '" sock-root "/run'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(putenv "THEOURGIA_RUN" (string-append sock-root "/run"))

(define counter 0)
(define (fresh-dir name)
  (set! counter (+ counter 1))
  (let ((d (string-append root "/" name (number->string counter))))
    (system (string-append "mkdir -p '" d "'"))
    d))
(define (run store . args) (rpc-dispatch store args "test"))
(define (state-of store) (open-and-reduce store))
(define (fields-of store id) (let ((row (state-read (state-of store) id))) (if row (cdr (assq 'fields row)) '())))
(define (field-of store id name) (let ((e (assq name (fields-of store id)))) (and e (cdr e))))
;; Not assq: an error answer holds symbols beside its clauses.
(define (clause-of answer name)
  (let ((c (and (pair? answer) (list? answer)
                (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr answer)))))
    (and c (cdr c))))
;; The first n elements of a list, or the list when it is shorter.
(define (head-of x n) (if (or (= n 0) (not (pair? x))) '() (cons (car x) (head-of (cdr x) (- n 1)))))

;; A READING OF THE DISK, OR WHY THERE IS NONE. A command that fails gives
;; (FAILED <name> <status>), never the empty text an empty store would.
(define (shell-reading name command)
  (let* ((f (string-append root "/" name ".reading"))
         (status (system (string-append "(" command ") > '" f "' 2>&1"))))
    (if (= status 0) (file-text f) (list 'FAILED name status))))
;; UNCHANGED MEANS TWO READINGS THAT BOTH SUCCEEDED AND AGREE: two failed
;; readings agree with each other and say nothing.
(define (same-reading? a b)
  (and (string? a) (string? b) (> (string-length a) 0) (string=? a b)))
;; The writers' segments by CONTENT (a checksum over them in a fixed order,
;; so an equal-length change shows) and the store's file list. There is at
;; least one segment, or the reading fails. NO PIPE: a pipeline's status is
;; its last command's, so each step writes a file and the next runs only
;; when it succeeded.
(define (segments-cmd store)
  (let ((t (lambda (n) (string-append "'" root "/" n "'"))))
    (string-append "cd '" store "' && find . -name '*.sexp' -path '*writers*' > " (t "seg.found")
                   " && LC_ALL=C sort " (t "seg.found") " > " (t "seg.list")
                   " && test -s " (t "seg.list") " && xargs cat < " (t "seg.list") " > " (t "seg.bytes")
                   " && cksum < " (t "seg.bytes"))))
(define (pins store)
  (shell-reading "pins" (string-append (segments-cmd store) " && cd '" store "' && find . > '" root "/all.found'"
                                       " && LC_ALL=C sort '" root "/all.found'")))

;; A store with a datum library defining h, and a text block beside it.
(define (datum-store!)
  (let ((store (fresh-dir "s")) (src (fresh-dir "lib")))
    (write-file! (string-append src "/dd.sc")
                 "(library (dd) (export h) (import (rnrs))\n(define (h) 'committed))\n")
    (run store 'init)
    (run store 'import-code src "--datum")
    (run store 'insert "--title" "Plain" "--text" "plain text")
    store))
(define (ids-where store pred)
  (let ((s (state-of store)))
    (filter (lambda (id) (pred (cdr (assq 'fields (state-read s id))))) (state-block-ids s))))
(define (the-one xs) (if (and (pair? xs) (null? (cdr xs))) (car xs) (list 'NOT-ONE xs)))
(define (h-of store)
  (the-one (ids-where store (lambda (fs) (and (equal? (assq 'kind fs) '(kind . code))
                                              (equal? (assq 'mode fs) '(mode . datum)))))))
(define (library-of store)
  (the-one (ids-where store (lambda (fs) (equal? (assq 'kind fs) '(kind . library))))))
(define (plain-of store)
  (the-one (ids-where store (lambda (fs) (equal? (assq 'title fs) '(title . "Plain"))))))
(define (refusal id) (list 'error 'bad-request 'draft-on-datum-unsupported (list 'block id) '(use def)))

(define s1 (datum-store!))
(define h (h-of s1))
(define lib (library-of s1))
(define plain (plain-of s1))
(want "SETUP one datum definition, its library and a plain block, each found once"
      (map string? (list h lib plain))
      '(#t #t #t))

;; ---- write -------------------------------------------------------------------------

(define before-write (pins s1))
(define write-h (run s1 'write h "(define (h) 'edited)" "--writer" "w1"))
(want "W1 write on a datum definition is refused by name, and nothing is written"
      (in-order write-h (same-reading? (pins s1) before-write) (file-exists? (string-append s1 "/writers/w1/working/" h)))
      (list (refusal h) #t #f))
(define write-lib (run s1 'write lib "(library (dd) (export h) (import (rnrs)))" "--writer" "w1"))
(want "W2 write on the datum library block is refused the same way: the rule is the mode"
      (in-order write-lib (same-reading? (pins s1) before-write))
      (list (refusal lib) #t))
(want "W3 the datum block holds its body and no src after both refusals"
      (in-order (and (field-of s1 h 'body) #t) (field-of s1 h 'src))
      '(#t #f))

;; ---- a draft made before the rule ----------------------------------------------------

(define planted-version (forge-fresh-draft! s1 "w5" h "(define (h) 'drafted-before)"))
(define draft-path (string-append s1 "/writers/w5/working/" h))
(define draft-bytes (file-bytes draft-path))
(define before-commit (pins s1))
(define commit-h (run s1 'commit h "--writer" "w5"))
(want "C1 commit of a draft on a datum block is refused by name, with commit's usage"
      (in-order (head-of commit-h 5) (and (clause-of commit-h 'usage) #t))
      (list (refusal h) #t))
(want "C2 and the draft is left as it is, and nothing is written"
      (in-order (equal? (file-bytes draft-path) draft-bytes) (same-reading? (pins s1) before-commit) (field-of s1 h 'src))
      '(#t #t #f))
(define commit-all (run s1 'commit "--writer" "w5"))
(want "C3 commit naming no block refuses the same way when that writer's drafts hold one"
      (in-order (head-of commit-all 5) (equal? (file-bytes draft-path) draft-bytes) (same-reading? (pins s1) before-commit))
      (list (refusal h) #t #t))

;; ---- set and batch -------------------------------------------------------------------

(define before-set (pins s1))
(want "S1 set <datum> src <text> is refused by name, and nothing is written"
      (in-order (run s1 'set h "src" "(define (h) 'set)") (same-reading? (pins s1) before-set))
      (list (refusal h) #t))
(define plain-src (field-of s1 plain 'src))
(define batch-text
  (string-append "((set \"" plain "\" src \"changed by the batch\") (set \"" h "\" src \"(define (h) 'batched)\"))"))
(define batch-answer (run s1 'batch batch-text))
(want "B1 a batch holding such a set is refused whole: the refusal, nothing done, nothing written"
      (in-order (and (pair? batch-answer) (car batch-answer))
                (and (pair? batch-answer) (pair? (cdr batch-answer)) (cadr batch-answer))
                (clause-of batch-answer 'done)
                (same-reading? (pins s1) before-set)
                (equal? (field-of s1 plain 'src) plain-src))
      (list 'batch (list (refusal h)) '(0) #t #t))
(define control-answer
  (run s1 'batch (string-append "((set \"" plain "\" src \"changed by the batch\"))")))
(want "B2 CONTROL: the same batch without the datum intent writes the plain block"
      (in-order (clause-of control-answer 'done) (equal? (field-of s1 plain 'src) plain-src))
      '((1) #f))

;; EVERY SPELLING AND EVERY ORDER. A batch is refused whole, with nothing
;; written, when an intent in it would leave a datum block holding a src:
;; wrapped in expect, with arguments past the value, a block made datum and
;; then given a src, a block holding a src made datum, and an insert
;; carrying both.
(define (batch-refused-whole? text expected)
  (let* ((before (pins s1))
         (answer (run s1 'batch text))
         (after (pins s1)))
    (list (and (pair? answer) (car answer))
          (and (pair? answer) (pair? (cdr answer)) (equal? (cadr answer) (list expected)))
          (clause-of answer 'done)
          (same-reading? before after))))
(define bare (begin (run s1 'insert "--title" "Bare") (the-one (ids-where s1 (lambda (fs) (equal? (assq 'title fs) '(title . "Bare")))))))
(want "B3 SETUP: the plain block holds a src and no mode; the bare one holds neither"
      (in-order (string? (field-of s1 plain 'src)) (field-of s1 plain 'mode) (string? bare) (field-of s1 bare 'src) (field-of s1 bare 'mode))
      '(#t #f #t #f #f))
(want "B3 a set wrapped in expect, at the block's current hash, is refused whole"
      (batch-refused-whole? (string-append "((expect \"" (block-hash (state-of s1) h) "\" (set \"" h "\" src \"(define (h) 'x)\")))")
                            (refusal h))
      '(batch #t (0) #t))
(want "B4 a set with an argument past its value is refused whole"
      (batch-refused-whole? (string-append "((set \"" h "\" src \"(define (h) 'x)\" extra))") (refusal h))
      '(batch #t (0) #t))
(want "B5 a block made datum and then given a src in the same batch is refused whole, the first intent unwritten"
      (append (batch-refused-whole? (string-append "((set \"" bare "\" mode datum) (set \"" bare "\" src \"x\"))") (refusal bare))
              (list (field-of s1 bare 'mode)))
      '(batch #t (0) #t #f))
(want "B6 a block holding a src made datum is refused whole"
      (append (batch-refused-whole? (string-append "((set \"" plain "\" mode datum))") (refusal plain))
              (list (field-of s1 plain 'mode)))
      '(batch #t (0) #t #f))
(want "B7 an insert carrying mode datum and a src is refused whole, with no block clause: it has no id yet"
      (batch-refused-whole? "((insert root #f ((kind . code) (mode . datum) (body . (define (x) 1)) (src . \"ignored\"))))"
                            '(error bad-request draft-on-datum-unsupported (use def)))
      '(batch #t (0) #t))
(define holder (begin (run s1 'insert "--title" "Holder" "--text" "held text")
                      (the-one (ids-where s1 (lambda (fs) (equal? (assq 'title fs) '(title . "Holder")))))))
(want "B9 SETUP: the holder block holds a src"
      (string? (field-of s1 holder 'src))
      #t)
;; THE RULE READS THE BATCH AS A SET, NOT IN ORDER: a src taken away and then
;; the block made datum is refused whole too, and takes two requests.
(want "B9 a src taken away and then the block made datum, in one batch, is refused whole: the rule reads the batch in any order"
      (append (batch-refused-whole? (string-append "((set \"" holder "\" src) (set \"" holder "\" mode datum))") (refusal holder))
              (list (string? (field-of s1 holder 'src))))
      '(batch #t (0) #t #t))
(want "B9 TWIN: as two requests it is taken -- the src taken away, then the block made datum"
      (in-order (car (run s1 'set holder "src")) (clause-of (run s1 'batch (string-append "((set \"" holder "\" mode datum))")) 'done)
                (field-of s1 holder 'mode) (field-of s1 holder 'src))
      '(ok (1) datum #f))
(want "B10 an insert that makes a datum block, then a src set on an id the store does not hold, is refused whole: the new block's id is the write's"
      (batch-refused-whole? (string-append "((insert root #f ((kind . code) (mode . datum) (body . (define (y) 2))))"
                                           " (set \"nosuch.1\" src \"x\"))")
                            (refusal "nosuch.1"))
      '(batch #t (0) #t))
(want "B11 an insert that makes a plain block, then mode datum and a src on an id the store does not hold, is refused whole"
      (batch-refused-whole? (string-append "((insert root #f ((title . \"N\")))"
                                           " (set \"nosuch.2\" mode datum) (set \"nosuch.2\" src \"x\"))")
                            (refusal "nosuch.2"))
      '(batch #t (0) #t))
;; A TEXT-MODE CODE BLOCK HOLDING A SRC, made datum: resolve would refuse it
;; (datum-src-set? comes before mode-mismatch), and the batch is refused whole,
;; before the insert beside it is written.
(define s5 (fresh-dir "s"))
(define s5-src (fresh-dir "js"))
(write-file! (string-append s5-src "/t.js") "function t() { return 1; }\n")
(run s5 'init)
(run s5 'import-code s5-src)
(define text-block
  (the-one (ids-where s5 (lambda (fs) (and (equal? (assq 'kind fs) '(kind . code)) (equal? (assq 'mode fs) '(mode . text)))))))
(want "B12 an insert, then mode datum on a text-mode code block holding a src, is refused whole: the insert is not written"
      (if (string? text-block)
          (let* ((before (pins s5))
                 (answer (run s5 'batch (string-append "((insert root #f ((title . \"N\"))) (set \"" text-block "\" mode datum))")))
                 (after (pins s5)))
            (list (and (pair? answer) (pair? (cdr answer)) (equal? (cadr answer) (list (refusal text-block))))
                  (clause-of answer 'done) (same-reading? before after)))
          (list 'NO-TEXT-BLOCK text-block))
      '(#t (0) #t))
(want "B8 CONTROL: mode datum on the bare block alone is taken"
      (in-order (clause-of (run s1 'batch (string-append "((set \"" bare "\" mode datum))")) 'done) (field-of s1 bare 'mode))
      '((1) datum))

;; ---- a text block beside it ----------------------------------------------------------

(define plain-write (run s1 'write plain "the draft of the plain block" "--writer" "w2"))
(define plain-commit (run s1 'commit plain "--writer" "w2"))
(want "T1 a plain block beside the datum block still takes a draft and its commit"
      (in-order (car plain-write) (car plain-commit) (field-of s1 plain 'src))
      '(ok ok "the draft of the plain block"))

;; ---- check ----------------------------------------------------------------------------

(define s2 (datum-store!))
(define h2 (h-of s2))
(define (with-src-clauses answer)
  (filter (lambda (c) (and (pair? c) (eq? (car c) 'datum-with-src))) (if (pair? answer) (cdr answer) '())))
(define clean-check (run s2 'check))
(want "K1 check on a clean store says nothing about datum blocks"
      (in-order (car clean-check) (with-src-clauses clean-check))
      '(check ()))
(define writer-of-s2
  (let ((ws (filter (lambda (w) (= 8 (string-length w))) (directory-list (string-append s2 "/writers")))))
    (the-one ws)))
(forge-record-as! s2 writer-of-s2 (string-append "(set \"" h2 "\" src \"(define (h) 'forged)\")"))
(define seeded-check (run s2 'check))
(want "K2 check lists a datum block that holds a src, once, and its verdict is the clean store's"
      (in-order (with-src-clauses seeded-check) (equal? (clause-of seeded-check 'verdict) (clause-of clean-check 'verdict)))
      (list (list (list 'datum-with-src (list h2))) #t))
;; AN UNCHANGED DRAFT ON A DATUM BLOCK: one whose text is the src a store
;; written before the rule holds. Its commit would write nothing and retire
;; the draft with ok; it is refused like any other, and the draft stays.
(forge-fresh-draft! s2 "w8" h2 "(define (h) 'forged)")
(define unchanged-path (string-append s2 "/writers/w8/working/" h2))
(define unchanged-bytes (file-bytes unchanged-path))
(define unchanged-commit (run s2 'commit h2 "--writer" "w8"))
(want "C4 commit of an UNCHANGED draft on a datum block is refused by name too, and the draft is left as it is"
      (in-order (head-of unchanged-commit 5) (equal? (file-bytes unchanged-path) unchanged-bytes))
      (list (refusal h2) #t))
(define unset (run s2 'set h2 "src"))
(want "K3 set <datum> src with no value takes the src away, and check is silent again"
      (in-order (car unset) (field-of s2 h2 'src) (with-src-clauses (run s2 'check)))
      '(ok #f ()))

;; ---- eval --working over a library whose view holds a draft on a datum block -----------
;;
;; `write` makes no such draft any more, so it is planted as working.sc writes
;; one, the shape a store written before that refusal still holds.

(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (core-eval store . args)
  (let ((out (string-append root "/eval.out")))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                           "THEOURGIA_HOME=" root "/home THEOURGIA_RUN=" sock-root "/run "
                           "scheme --script ../core.sc eval "
                           (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                           "--store '" store "' --wire > '" out "' 2> '" out ".err' < /dev/null"))
    (let ((t (file-text out)))
      (guard (e (#t (list 'UNREADABLE t)))
        (string->sexpr-extended
          (let loop ((i 0))
            (cond ((>= i (string-length t)) t)
                  ((char=? (string-ref t i) #\newline) (substring t 0 i))
                  (else (loop (+ i 1))))))))))

(define s4 (datum-store!))
(define h4 (h-of s4))
(define lib4 (library-of s4))
(forge-fresh-draft! s4 "w5" h4 "(define (h) 'drafted)")
(forge-fresh-draft! s4 "w7" lib4 "(library (dd) (export h) (import (rnrs)))")
(define e-drafted (core-eval s4 "--working" "--writer" "w5" "--under" lib4 "(h)"))
(define e-clean (core-eval s4 "--working" "--writer" "w6" "--under" lib4 "(h)"))
(define e-raise (core-eval s4 "--working" "--writer" "w6" "--under" lib4 "(car '())"))
(want "E1 eval --working over a library whose view holds a draft on a datum block is refused by name, naming the block"
      (head-of e-drafted 4)
      (list 'error 'eval-context '(reason draft-on-datum-unsupported) (list 'block h4)))
(want "E2 a writer with no such draft evaluates as before: the committed body"
      (in-order (and (pair? e-clean) (car e-clean)) (clause-of e-clean 'values))
      '(ok ((committed))))
(want "E3 the refusal is not the answer the source's own raise gets"
      (in-order (and (pair? e-drafted) (pair? (cdr e-drafted)) (cadr e-drafted))
                (and (pair? e-raise) (pair? (cdr e-raise)) (cadr e-raise)))
      '(eval-context eval-exception))
(want "E4 a draft on the library block itself is refused the same way, naming the library"
      (head-of (core-eval s4 "--working" "--writer" "w7" "--under" lib4 "(h)") 4)
      (list 'error 'eval-context '(reason draft-on-datum-unsupported) (list 'block lib4)))
(want "E5 without --under nothing is spliced from the library, and the drafted writer evaluates"
      (let ((a (core-eval s4 "--working" "--writer" "w5" "(+ 1 2)")))
        (in-order (and (pair? a) (car a)) (clause-of a 'values)))
      '(ok ((3))))

;; ---- def's catalogue sentence, against def ---------------------------------------------
;;
;; A second def of a name the library already defines is refused, and the
;; sentence describe gives for def names that refusal and does not offer to
;; replace.
(define s5 (datum-store!))
(define def-answer (run s5 'def "h" "(define (h) 'again)"))
(define def-sentence
  (let* ((a (run s5 'describe))
         (e (and (pair? a) (assq 'verbs (cdr a)) (assq 'def (cdr (assq 'verbs (cdr a))))))
         (d (and e (assq 'description (cdr e)))))
    (and d (cadr d))))
(want "DEF a def of a name the library defines is refused, and describe's def sentence names that refusal and offers no replacing"
      (in-order (and (pair? def-answer) (car def-answer))
                (and (pair? def-answer) (pair? (cdr def-answer)) (cadr def-answer))
                (and (string? def-sentence) (pair? def-answer) (pair? (cdr def-answer))
                     (string-contains? def-sentence (symbol->string (cadr def-answer))))
                (and (string? def-sentence) (string-contains? def-sentence "or replace")))
      '(error name-exists #t #f))

;; ---- the three routes ------------------------------------------------------------------

(define s3 (datum-store!))
(define h3 (h-of s3))
(define (env extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run " extra " "))
(define (sh-out name command)
  (let ((out (string-append root "/" name ".out")))
    (system (string-append command " > '" out "' 2> '" root "/" name ".err' < /dev/null"))
    (file-text out)))
(define (first-datum text)
  (guard (e (#t (list 'UNREADABLE text)))
    (string->sexpr-extended (let loop ((i 0))
                              (cond ((>= i (string-length text)) text)
                                    ((char=? (string-ref text i) #\newline) (substring text 0 i))
                                    (else (loop (+ i 1))))))))
(define (log-bytes store) (shell-reading "log-bytes" (segments-cmd store)))
(define log-before-routes (log-bytes s3))
(define local-route (run s3 'write h3 "(define (h) 'local)" "--writer" "w3"))
(define client-route
  (first-datum (sh-out "client" (string-append (env "") "scheme --script ../theourgia.sc write " h3
                                               " \"(define (h) 'client)\" --writer w3 --store '" s3 "' --wire"))))
(define mcp-out
  (let ((in (string-append root "/mcp-in.jsonl")))
    (write-file! in (string-append
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                      "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}\n"
                      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                      "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":"
                      "{\"name\":\"theourgia_write\",\"arguments\":{\"argv\":[\"" h3 "\",\"(define (h) 'mcp)\"]}}}\n"))
    (let ((out (string-append root "/mcp.out")))
      (system (string-append (env "") "scheme --script ../mcp/server.sc --store '" s3 "' < '" in "' > '" out "' 2> '" root "/mcp.err'"))
      (file-text out))))
;; The text of the id-3 answer's first content item, unescaped.
(define mcp-text
  (let* ((at (let find ((i 0))
               (cond ((> (+ i 6) (string-length mcp-out)) #f)
                     ((string=? (substring mcp-out i (+ i 6)) "\"id\":3") i)
                     (else (find (+ i 1))))))
         (key "\"text\":\"")
         (k (and at (let find ((i at))
                      (cond ((> (+ i (string-length key)) (string-length mcp-out)) #f)
                            ((string=? (substring mcp-out i (+ i (string-length key))) key) (+ i (string-length key)))
                            (else (find (+ i 1))))))))
    (and k (let loop ((i k) (acc '()))
             (cond ((>= i (string-length mcp-out)) #f)
                   ((char=? (string-ref mcp-out i) #\\)
                    (let ((c (string-ref mcp-out (+ i 1))))
                      (loop (+ i 2) (cons (cond ((char=? c #\n) #\newline) ((char=? c #\t) #\tab) (else c)) acc))))
                   ((char=? (string-ref mcp-out i) #\") (list->string (reverse acc)))
                   (else (loop (+ i 1) (cons (string-ref mcp-out i) acc))))))))
(want "R1 the three routes refuse write on a datum block with the same answer"
      (in-order local-route client-route (if mcp-text (first-datum mcp-text) (list 'NO-TEXT mcp-out)))
      (list (refusal h3) (refusal h3) (refusal h3)))
;; THE STORE'S FILE LIST IS NOT COMPARED HERE: a daemon and an MCP shell
;; were started on this store, and what they open for themselves is not a
;; write. The log's bytes and the drafts' slots are.
(define (drafts-of store id)
  (shell-reading "drafts" (string-append "cd '" store "' && find . -path '*/working/*' -name '" id "' > '" root "/drafts.found'"
                                         " && LC_ALL=C sort '" root "/drafts.found'")))
(want "R2 and none of them wrote: the log's bytes are as they were, and no writer holds a draft of the block"
      (in-order (same-reading? (log-bytes s3) log-before-routes) (drafts-of s3 h3))
      '(#t ""))

(system (string-append "pkill -f 'serve " s3 "' 2>/dev/null"))
(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\ndatum-draft complete\n" bad rows)
(exit (if (= bad 0) 0 1))
