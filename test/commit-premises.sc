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

;; Premises on a committed write: what a reader was given is what is checked
;; when its result is accepted.
;;
;; A request may carry `--premises <datum>`: block premises (an id and the
;; version the reader saw), query premises (a goal and the digest the reader
;; saw), and the clauses a read answers, given back as they were received.
;; The set is checked ONCE, at admission, under the store's lock, before the
;; first record of a fresh request; a refusal names every premise that
;; failed and writes nothing. A replay, or the completion of a plan already
;; durable, does not check it again, and its answer says so. A request's
;; identity never includes its premises.
;;
;; WRITTEN FROM THE DESIGN'S CELLS BEFORE THE PRODUCT, and run first against
;; the tree before it (where every row that needs the option is red).
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce store-evidence)
        (only (theourgia wire) storable-encode storable-decode string->sexpr-extended sexpr->string-extended)
        (only (theourgia reduce) state-read state-block-ids block-hash block-id reduce-applied-cut))

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
(define (write-file! path text) (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/commit-premises-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(define home (string-append root "/home"))
(putenv "THEOURGIA_HOME" home)
(define store (string-append root "/s"))
(define cli "../core.sc")
(define (run . args) (rpc-dispatch store args "test"))
(define (state) (open-and-reduce store))
(define (new-id answer)
  (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define (ins! title text . under)
  (new-id (apply run 'insert "--title" title "--text" text (if (pair? under) (list "--under" (car under)) '()))))
(define (ver id) (block-hash (state) id))
(define (clause answer name)
  (and (pair? answer) (list? answer) (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr answer))))
(define (head a) (and (pair? a) (if (eq? (car a) 'error) (list 'error (cadr a)) (car a))))
(define (string-split-dot id)
  (let loop ((i 0)) (if (char=? (string-ref id i) #\.) (list (substring id 0 i) (substring id (+ i 1) (string-length id))) (loop (+ i 1)))))
(define (contains? x needle) (or (equal? x needle) (and (pair? x) (or (contains? (car x) needle) (contains? (cdr x) needle)))))
;; A premise set, spelled as the option's value.
(define (premises . forms) (format "~s" forms))
(define (block-premise id h) (list 'premise id h))

;; THE LOG, BYTE FOR BYTE: every file under the store's writers.
(define (log-digest)
  (let ((out (string-append root "/digest.txt")))
    (system (string-append "cd " (quoted store) " && find writers -type f | LC_ALL=C sort | xargs cat 2>/dev/null | md5 > " (quoted out)))
    (file-text out)))

;; The writer this store's requests are from, and its cursor for a tracked request.
(define init-answer (run 'init))
(define W (cadr (assq 'writer (cdr init-answer))))
(define (cursor) (string-append W ":" (number->string (cdr (assoc W (reduce-applied-cut (state)))))))

;; ---- a seeded store -------------------------------------------------------------------

(define A (ins! "Premise block" "what the reader saw"))
(define B (ins! "Written block" "to be written"))
(define C (ins! "Moved block" "to be moved"))
(define A-old (ver A))
(run 'set A "title" "Premise block, changed")
(define A-new (ver A))
(define stale (premises (block-premise A A-old)))
(define current (lambda () (premises (block-premise A (ver A)))))
(define src (string-append root "/lib"))
(system (string-append "mkdir -p '" src "'"))
(write-file! (string-append src "/pp.sc") "(library (pp) (export pf) (import (rnrs))\n(define (pf) 1))\n")
;; TEXT CODE for import-code without --datum, which takes code-project's
;; path rather than datum-project's.
(define js (string-append root "/js"))
(system (string-append "mkdir -p '" js "'"))
(write-file! (string-append js "/a.js") "function ja() { return 1 }\n")
(define md (string-append root "/md"))
(system (string-append "mkdir -p '" md "'"))
(write-file! (string-append md "/note.md") "# A note\n\nSome text.\n")

(want "SETUP the premise block moved: its old version is stale"
      (in-order (string? A-old) (equal? A-old A-new))
      '(#t #f))

;; ---- C48b-1: a stale block premise refuses every committed-write verb ------------------------

;; Each verb: what it needs first, then the verb as a procedure of a premise
;; set. NEVER: THE PREPARATION RUNS BEFORE THE LOG IS MEASURED. Inside the
;; window, commit's draft and del's own block read as "the refused request
;; wrote", which it did not.
(define (no-preparation) #f)
(define del-target #f)
(define verbs
  (list (list 'set no-preparation (lambda (P) (run 'set B "title" "Written, set" "--premises" P)))
        (list 'insert no-preparation (lambda (P) (run 'insert "--title" "Inserted" "--premises" P)))
        (list 'move no-preparation (lambda (P) (run 'move C B "--premises" P)))
        (list 'link no-preparation (lambda (P) (run 'link B "cites" C "--premises" P)))
        (list 'batch no-preparation (lambda (P) (run 'batch (format "((set ~s title \"Written, batch\"))" B) "--premises" P)))
        (list 'commit (lambda () (run 'write B "draft text" "--writer" W))
              (lambda (P) (run 'commit B "--writer" W "--premises" P)))
        ;; the library gains a definition, so the import has something to write
        (list 'import-code (lambda () (write-file! (string-append src "/pp.sc")
                                                   "(library (pp) (export pf pf2) (import (rnrs))\n(define (pf) 1)\n(define (pf2) 2))\n"))
              (lambda (P) (run 'import-code src "--datum" "--premises" P)))
        (list 'import-code-text no-preparation (lambda (P) (run 'import-code js "--premises" P)))
        (list 'import-md no-preparation (lambda (P) (run 'import-md md "--premises" P)))
        (list 'def no-preparation (lambda (P) (run 'def "pg" "--under" (library-id) "(define (pg) 2)" "--premises" P)))
        (list 'template no-preparation (lambda (P) (run 'template "apply" "memory" "--premises" P)))
        (list 'del (lambda () (set! del-target (ins! "Deleted block" "deleted by its row")))
              (lambda (P) (run 'del del-target "--premises" P)))))
(define (library-id)
  (find (lambda (id) (let ((f (assq 'kind (cdr (assq 'fields (state-read (state) id)))))) (and f (eq? (cdr f) 'library))))
        (state-block-ids (state))))
;; The library def writes into exists first, from a plain import.
(run 'import-code src "--datum")
(for-each
  (lambda (v)
    (let* ((prepared ((cadr v)))
           (before (log-digest))
           (refused ((caddr v) stale))
           (after (log-digest)))
      (want (format "C48b-1 ~a with a stale block premise is refused premise-changed, expected and current named, and writes nothing" (car v))
            (in-order (head refused)
                      (let ((b (and (pair? refused) (memq 'premise-changed refused) (assq 'block (cdr (memq 'premise-changed refused))))))
                        (and b (list (cadr b) (cadr (assq 'expected (cddr b))) (cadr (assq 'current (cddr b))))))
                      (equal? before after))
            (list '(error premise-changed) (list A A-old (ver A)) #t))
      (want (format "C48b-1 ~a with the current version commits: it writes, and its answer carries no premises clause" (car v))
            (let* ((before-ok (log-digest)) (a ((caddr v) (current))) (after-ok (log-digest)))
              (in-order (rpc-ok? a) (clause a 'premises) (equal? before-ok after-ok)))
            '(#t #f #f))))
  verbs)

;; ---- C48b-2: several failing premises, in order; deleted, unknown; since ---------------------

(define D (ins! "Doomed block" "to be deleted"))
(define D-ver (ver D))
(run 'del D)
(define cut-before (reduce-applied-cut (state)))
(define A-at-cut (ver A))
(run 'set A "title" "Premise block, changed again")
(want "C48b-2 every failing premise is named, in the order given: stale, deleted, unknown"
      (let ((a (run 'set B "title" "x" "--premises"
                    (premises (block-premise A A-old) (block-premise D D-ver) (block-premise "zz.9" "0")))))
        (in-order (head a) (map (lambda (b) (list (cadr b) (cadr (assq 'current (cddr b)))))
                                (filter (lambda (x) (and (pair? x) (eq? (car x) 'block))) (cddr a)))))
      (list '(error premise-changed) (list (list A (ver A)) (list D 'deleted) (list "zz.9" 'unknown))))
(want "C48b-2 since: with a cut in the set, exactly the events after it; without one, no since"
      (let* ((with-cut (run 'set B "title" "y" "--premises" (format "~s" (list (block-premise A A-at-cut) (list 'cut cut-before)))))
             (without (run 'set B "title" "y" "--premises" (premises (block-premise A A-at-cut))))
             (since-of (lambda (a) (let ((b (find (lambda (x) (and (pair? x) (eq? (car x) 'block))) (cddr a))))
                                     (and b (let ((s (assq 'since (cddr b)))) (and s (cdr s))))))))
        (in-order (since-of with-cut) (since-of without)))
      (list (list (cons W (cdr (assoc W (reduce-applied-cut (state)))))) #f))

(define G (ins! "Since block" "read, changed, deleted"))
(define G-read (ver G))
(define G-cut (reduce-applied-cut (state)))
(run 'set G "title" "Since block, changed")
(define G-title-event (cons W (cdr (assoc W (reduce-applied-cut (state))))))
(run 'del G)
(want "C48b-2 since is given for a block deleted after the cut: current deleted, the title change named"
      (let* ((a (run 'set B "title" "y2" "--premises" (format "~s" (list (block-premise G G-read) (list 'cut G-cut)))))
             (b (find (lambda (x) (and (pair? x) (eq? (car x) 'block))) (if (pair? a) (cddr a) '())))
             (s (and b (assq 'since (cddr b)))))
        (in-order (head a) (and b (cadr (assq 'current (cddr b)))) (and s (member G-title-event (cdr s)) #t)))
      '((error premise-changed) deleted #t))

;; ---- C48b-3: the forms --------------------------------------------------------------------------

(want "C48b-3 a read's (versions ...) and (cut ...) given back verbatim are accepted and checked"
      (let* ((r (run 'read A "--recursive"))
             (set (format "~s" (list (clause r 'versions) (clause r 'cut)))))
        (rpc-ok? (run 'set B "title" "z" "--premises" set)))
      #t)
(want "C48b-3 a receipt holding block premises is accepted"
      (rpc-ok? (run 'set B "title" "z2" "--premises" (format "~s" (list (list 'receipt (block-premise A (ver A)))))))
      #t)
(want "C48b-3 the forms are checked, not only accepted: a read's versions and a receipt given back stale are refused"
      (let* ((r (run 'read A "--recursive"))
             (vs (clause r 'versions))
             (moved (run 'set A "title" "Premise block, after the read"))
             (by-versions (run 'set B "title" "z3" "--premises" (format "~s" (list vs))))
             (by-receipt (run 'set B "title" "z4" "--premises" (format "~s" (list (list 'receipt vs))))))
        (in-order (head by-versions) (head by-receipt)))
      '((error premise-changed) (error premise-changed)))
(want "C48b-3 an unknown form is refused by name, before anything is read"
      (let ((before (log-digest)) (a (run 'set B "title" "w" "--premises" "((frobnicate 1))")))
        (in-order (and (pair? a) (list (car a) (cadr a) (caddr a))) (equal? before (log-digest))))
      '((error bad-request premise-not-understood) #t))
;; BEFORE ANYTHING IS READ: on a store whose segment cannot be read, a set the
;; store cannot check is the answer -- commit, and set on a datum body (which
;; reads the block before it writes). A read of that store does not fail: it
;; goes past the segment and the dispatcher says so with (incomplete ...). So
;; the answer carries no incomplete clause exactly when nothing was read.
(define unreadable-store (string-append root "/u"))
(rpc-dispatch unreadable-store '(init) "test")
(define U1 (new-id (rpc-dispatch unreadable-store (list 'insert "--title" "unreadable") "test")))
(define (segments-of st) (string-append st "/writers/*/000001.sexp"))
(want "C48b-3 a set the store cannot check is refused before the store is read: commit and set"
      (begin
        (system (string-append "chmod 000 " (segments-of unreadable-store)))
        (let* ((c (rpc-dispatch unreadable-store (list 'commit "--writer" (car (string-split-dot U1)) "--premises" "((frobnicate 1))") "test"))
               (b (rpc-dispatch unreadable-store (list 'set U1 "body" "(x)" "--premises" "((frobnicate 1))") "test")))
          (system (string-append "chmod 644 " (segments-of unreadable-store)))
          (map (lambda (a) (and (pair? a) (list? a) (>= (length a) 3) (list (car a) (cadr a) (caddr a) (contains? a 'incomplete)))) (list c b))))
      '((error bad-request premise-not-understood #f) (error bad-request premise-not-understood #f)))
;; A VERB'S OWN CHECK WITHOUT A SET is the plain procedure it always was, and
;; with a set it is still asked after the premises: set's --based-on, stale.
(want "C48b-3 a verb's own check, without a set and after a current one: refused stale-baseline both ways, nothing written"
      (let* ((before (log-digest))
             (plain (run 'set A "body" "own check" "--based-on" A-old))
             (with (run 'set A "body" "own check" "--based-on" A-old "--premises" (current))))
        (in-order (head plain) (head with) (equal? before (log-digest))))
      '((error stale-baseline) (error stale-baseline) #t))
;; THE SET IS THE FIRST THING A VERB LOOKS AT, whatever else the request
;; holds: template apply's file is not read, and batch's intents are not
;; parsed, before a set the store cannot check is refused.
(define unreadable-template (string-append root "/unreadable-template.sexp"))
(write-file! unreadable-template "(template)")
(define (head3 a) (and (pair? a) (list? a) (>= (length a) 3) (list (car a) (cadr a) (caddr a))))
(want "C48b-3 the set is looked at first: template apply's unreadable file, batch's unparsable intents"
      (begin
        (system (string-append "chmod 000 " (quoted unreadable-template)))
        (let* ((t (run 'template "apply" "--file" unreadable-template "--premises" "((frobnicate 1))"))
               (b (run 'batch "(" "--premises" "((frobnicate 1))")))
          (system (string-append "chmod 644 " (quoted unreadable-template)))
          (map head3 (list t b))))
      '((error bad-request premise-not-understood) (error bad-request premise-not-understood)))
(want "C48b-3 two hashes for one id are refused premise-inconsistent"
      (let ((a (run 'set B "title" "w" "--premises" (premises (block-premise A (ver A)) (block-premise A A-old)))))
        (and (pair? a) (list (car a) (cadr a) (caddr a))))
      '(error bad-request premise-inconsistent))

;; ---- C48b-4: query premises -------------------------------------------------------------------------

(define goal "(kind ?x section)")
(define (digest-of g) (cadr (clause (run 'query g) 'digest)))
(define d-before (digest-of goal))
(run 'insert "--title" "Another section" "--text" "more")
(want "C48b-4 a query premise whose answer changed refuses with both digests"
      (let ((a (run 'set B "title" "q" "--premises" (format "~s" (list (list 'premise (list 'query (read (open-string-input-port goal))) d-before))))))
        (in-order (head a) (let ((qc (find (lambda (x) (and (pair? x) (eq? (car x) 'query))) (cddr a))))
                             (and qc (list (cadr (assq 'expected (cddr qc))) (equal? (cadr (assq 'current (cddr qc))) (digest-of goal)))))))
      (list '(error premise-changed) (list d-before #t)))
(define d-now (digest-of "(kind ?x decision)"))
(run 'set B "title" "the cut moves")
(want "C48b-4 a query premise whose answer is unchanged although the cut moved commits"
      (rpc-ok? (run 'set B "title" "q2" "--premises" (format "~s" (list (list 'premise '(query (kind ?x decision)) d-now)))))
      #t)
(want "C48b-4 a query premise that cannot be evaluated is premise-unevaluable"
      (let ((a (run 'set B "title" "q3" "--premises" (format "~s" (list (list 'premise '(query (nosuch ?x)) "0"))))))
        (and (pair? a) (list (car a) (cadr a))))
      '(error premise-unevaluable))

;; ---- C48b-5: admission ------------------------------------------------------------------------------

(want "C48b-5 a request refused for a premise leaves no evidence; the same request id with a current receipt executes"
      (let* ((cur (cursor))
             (refused (run 'set B "title" "tracked" "--req" "req-a" "--cursor" cur "--premises" stale))
             (evidence (store-evidence store (cons W "req-a")))
             (again (run 'set B "title" "tracked" "--req" "req-a" "--cursor" cur "--premises" (current))))
        (in-order (head refused) evidence (rpc-ok? again)))
      '((error premise-changed) () #t))
(want "C48b-5 a replay of an executed request answers its recorded answer, and says its premises were not checked"
      (let* ((cur (cursor))
             (first (run 'set B "title" "replayed" "--req" "req-b" "--cursor" cur "--premises" (current)))
             (second (run 'set B "title" "replayed" "--req" "req-b" "--cursor" cur "--premises" stale)))
        (in-order (rpc-ok? first) (rpc-ok? second) (clause first 'premises) (clause second 'premises)))
      '(#t #t #f (premises not-checked (reason replay))))
(want "C48b-5 import-code and def: refused for a premise, the same request id with a current receipt executes (the operation packet stays)"
      (let* ((cur (cursor))
             (refused (run 'def "ph" "--under" (library-id) "(define (ph) 3)" "--req" "req-c" "--cursor" cur "--premises" stale))
             (retried (run 'def "ph" "--under" (library-id) "(define (ph) 3)" "--req" "req-c" "--cursor" cur "--premises" (current))))
        (in-order (head refused) (rpc-ok? retried)))
      '((error premise-changed) #t))
(want "C48b-5 import-code: refused for a premise, the same request id with a current receipt executes (the operation packet stays)"
      (let* ((cur (cursor))
             (refused (run 'import-code js "--req" "req-h" "--cursor" cur "--premises" stale))
             (retried (run 'import-code js "--req" "req-h" "--cursor" cur "--premises" (current))))
        (in-order (head refused) (rpc-ok? retried)))
      '((error premise-changed) #t))
;; A PLAN INTERRUPTED AFTER ITS PLAN RECORD: a real crash at the second
;; append (crash-at.ss), then a retry. The premised block has moved since,
;; by another writer; the retry completes and says it did not check.
(include "crash-at.ss")
;; ONE REQUEST IN A CHILD BUILT WITH FAULT INJECTION, so a fault named by
;; THEOURGIA_FAULT happens inside the write: -> the answer the child printed.
(define child-count 0)
(define (child-dispatch fault on-store args . actor)
  (set! child-count (+ child-count 1))
  (let ((script (string-append root "/child" (number->string child-count) ".sc"))
        (out (string-append root "/child" (number->string child-count) ".out")))
    (write-file! script
      (string-append "(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))\n"
                     "(register-verbs! extension-verbs)\n"
;; THE CHILD WRITES ITS ANSWER AND ITS OWN COUNT OF CHECK CALLS: the
                     ;; parent's counter does not see a child's.
                     (format "(let ((a (rpc-dispatch ~s '~s ~s))) (write (list a ((eval 'premises-check-calls (environment '(theourgia premises)))))))\n"
                             on-store args (if (pair? actor) (car actor) "test"))))
    (system (string-append "THEOURGIA_HOME=" (quoted home) " THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault
                           " scheme --script " (quoted script) " > " (quoted out) " 2>/dev/null < /dev/null"))
    (guard (e (#t (list (list 'unreadable (file-text out)) #f))) (read (open-string-input-port (file-text out))))))
;; -> the child's answer; (child-calls r) its count.
(define (child-answer r) (car r))
(define (child-calls r) (cadr r))
;; A RETRY OF A REQUEST THE COMMAND LINE SENT IS FROM THE COMMAND LINE'S ACTOR
;; (core.sc: THEOURGIA_ACTOR, else USER, else "cli"): who sent it is part of
;; a commit's identity, and from "test" the retry is another request.
(define cli-actor (or (getenv "THEOURGIA_ACTOR") (getenv "USER") "cli"))
(define (run-as-cli . args) (rpc-dispatch store args cli-actor))
(define req-d-cursor #f)
;; A COMMIT THAT CARRIES --req NAMES THE VERSION OF EVERY BLOCK IT CONSUMES
;; (req-needs-versions); with one block, the bare version.
(define req-d-version #f)
(define req-d-premises #f)
(define crashed
  (begin
    (run 'write B "crash draft" "--writer" W)
    (set! req-d-premises (current))
    (set! req-d-version (list-ref (assq 'projection (cdr (run 'read B "--working-info" "--writer" W))) 4))
    (set! req-d-cursor (cursor))
    (car (crash-at 2 root home cli
                   (list "commit" B "--req" "req-d" "--cursor" req-d-cursor "--working-version" req-d-version
                         "--writer" W "--premises" req-d-premises "--store" store)))))
(want "C48b-5 the crash happened where it was meant to: after the plan record"
      crashed
      2)
(run 'set A "title" "moved after the plan")
(want "C48b-5b a completion whose write fails before a byte lands answers not-written, and says not-fresh"
      (let* ((r (child-dispatch "write-eio-first@commit" store
                               (list 'commit B "--req" "req-d" "--cursor" req-d-cursor "--working-version" req-d-version
                                     "--writer" W "--premises" req-d-premises)
                               cli-actor))
            (a (child-answer r)))
        (in-order (and (pair? a) (list (car a) (cadr a))) (clause a 'premises) (child-calls r)))
      '((error not-written) (premises not-checked (reason not-fresh)) 0))
(want "C48b-5 a plan interrupted after its plan record completes on retry with its premises, now stale (another writer's edit), and says not-fresh"
      (let ((r (run-as-cli 'commit B "--req" "req-d" "--cursor" req-d-cursor "--working-version" req-d-version "--writer" W "--premises" req-d-premises)))
        (in-order (equal? req-d-premises (current)) (rpc-ok? r) (clause r 'premises)))
      '(#f #t (premises not-checked (reason not-fresh))))

;; A COMMIT OF TWO BLOCKS whose premise is on the first: killed after its plan
;; and its first member, so the premised block has been moved by the
;; request's OWN member when the retry completes it.
(define (draft-version id) (list-ref (assq 'projection (cdr (run 'read id "--working-info" "--writer" W))) 4))
(define B2 (ins! "Own member one" "first"))
(define B3 (ins! "Own member two" "second"))
(run 'write B2 "own draft one" "--writer" W)
(run 'write B3 "own draft two" "--writer" W)
(define own-premises (premises (block-premise B2 (ver B2))))
(define own-cursor (cursor))
(define own-versions (list "--working-version" (string-append B2 "=" (draft-version B2))
                           "--working-version" (string-append B3 "=" (draft-version B3))))
(define own-crashed
  (car (crash-at 3 root home cli
                 (append (list "commit" B2 B3 "--req" "req-i" "--cursor" own-cursor) own-versions
                         (list "--writer" W "--premises" own-premises "--store" store)))))
(want "C48b-5 the two-block commit was killed after its plan and its first member"
      own-crashed
      3)
(want "C48b-5 a plan whose premised block its own member has moved completes on retry, and says not-fresh"
      (let ((r (apply run-as-cli (append (list 'commit B2 B3 "--req" "req-i" "--cursor" own-cursor) own-versions
                                         (list "--writer" W "--premises" own-premises)))))
        (in-order (equal? own-premises (premises (block-premise B2 (ver B2)))) (rpc-ok? r) (clause r 'premises)))
      '(#f #t (premises not-checked (reason not-fresh))))

;; A RETRY THAT ADDS A STALE PREMISE SET to a request first sent without one.
(define B4 (ins! "Sent without premises" "plain"))
(run 'write B4 "plain draft" "--writer" W)
(define add-cursor (cursor))
(define add-version (draft-version B4))
(define add-crashed
  (car (crash-at 2 root home cli
                 (list "commit" B4 "--req" "req-j" "--cursor" add-cursor "--working-version" add-version
                       "--writer" W "--store" store))))
(want "C48b-5 a retry that adds a stale premise set to a request first sent without one completes, and says not-fresh"
      (let ((r (run-as-cli 'commit B4 "--req" "req-j" "--cursor" add-cursor "--working-version" add-version
                           "--writer" W "--premises" stale)))
        (in-order add-crashed (rpc-ok? r) (clause r 'premises)))
      '(2 #t (premises not-checked (reason not-fresh))))

;; THE STATED LIMIT: two requests that each read what the other writes. The
;; first read X and was admitted to write Y, and stopped after its plan; the
;; second read the old Y and wrote X; the first completes Y. Each was checked
;; against what it read, both ends are written, and no serial order of the two
;; explains both. Pinned as it is.
(define X1 (ins! "Read by the first" "x"))
(define Y1 (ins! "Written by the first" "y"))
(run 'write Y1 "the first's draft" "--writer" W)
(define limit-cursor (cursor))
(define limit-version (draft-version Y1))
(define limit-premises (premises (block-premise X1 (ver X1))))
(define limit-crashed
  (car (crash-at 2 root home cli
                 (list "commit" Y1 "--req" "req-k" "--cursor" limit-cursor "--working-version" limit-version
                       "--writer" W "--premises" limit-premises "--store" store))))
(want "C48b-5 the stated limit: the second request, on the old Y, writes X; the first completes Y; both ends are written"
      (let* ((second (run 'set X1 "title" "written by the second" "--premises" (premises (block-premise Y1 (ver Y1)))))
             (first (run-as-cli 'commit Y1 "--req" "req-k" "--cursor" limit-cursor "--working-version" limit-version
                                "--writer" W "--premises" limit-premises))
             (x-title (cdr (assq 'title (cdr (assq 'fields (state-read (state) X1))))))
             (y-src (cdr (assq 'src (cdr (assq 'fields (state-read (state) Y1)))))))
        (in-order limit-crashed (rpc-ok? second) (rpc-ok? first) (clause first 'premises) x-title y-src))
      '(2 #t #t (premises not-checked (reason not-fresh)) "written by the second" "the first's draft"))

;; ---- C48b-6: commit's arm that writes nothing --------------------------------------------------------

(want "C48b-6 commit with unchanged drafts and a stale premise refuses and retires nothing; with a current one it retires"
      (let* ((txt (cdr (assq 'src (cdr (assq 'fields (state-read (state) C))))))
             (w (run 'write C txt "--writer" W))
             (refused (run 'commit C "--writer" W "--premises" stale))
             (drafts-after-refusal (length (cdr (cadr (run 'drafts "--writer" W)))))
             (ok (run 'commit C "--writer" W "--premises" (current)))
             (retired (not (contains? (run 'drafts "--writer" W) C))))
        (in-order (head refused) (> drafts-after-refusal 0) (rpc-ok? ok) retired))
      '((error premise-changed) #t #t #t))

;; ---- C48b-6b: the identity -----------------------------------------------------------------------------

(want "C48b-6b the same request id with and without premises, and with two sets, is one request: a second send is a replay, never req-mismatch"
      (let* ((cur (cursor))
             (a1 (run 'set B "title" "ident" "--req" "req-e" "--cursor" cur))
             (a2 (run 'set B "title" "ident" "--req" "req-e" "--cursor" cur "--premises" (current)))
             (a3 (run 'set B "title" "ident" "--req" "req-e" "--cursor" cur "--premises" stale)))
        (in-order (rpc-ok? a1) (head a2) (head a3) (clause a2 'premises) (clause a3 'premises)))
      '(#t ok ok (premises not-checked (reason replay)) (premises not-checked (reason replay))))

;; EVERY VERB THAT CARRIES A REQUEST ID: sent first without premises, then
;; with a current set and with a stale one under the same id, the second and
;; third sends are the first's replays. import-md is not among them: its write
;; carries no request id.
(define ident-commit-version #f)
(define ident-del-target #f)
(define ident-verbs
  (list (list 'set (lambda () #f) (lambda () (list 'set B "title" "ident-set")))
        (list 'insert (lambda () #f) (lambda () (list 'insert "--title" "ident insert")))
        (list 'move (lambda () #f) (lambda () (list 'move C B)))
        (list 'link (lambda () #f) (lambda () (list 'link B "cites" C)))
        (list 'unlink (lambda () #f) (lambda () (list 'unlink B "cites" C)))
        (list 'tag (lambda () #f) (lambda () (list 'tag "ident-tag")))
        (list 'batch (lambda () #f) (lambda () (list 'batch (format "((set ~s title \"ident-batch\"))" B))))
        (list 'del (lambda () (set! ident-del-target (ins! "Ident deleted" "x"))) (lambda () (list 'del ident-del-target)))
        (list 'commit (lambda () (run 'write B "ident draft" "--writer" W) (set! ident-commit-version (draft-version B)))
              (lambda () (list 'commit B "--working-version" ident-commit-version "--writer" W)))
        (list 'import-code (lambda () #f) (lambda () (list 'import-code js)))
        (list 'def (lambda () #f) (lambda () (list 'def "pid" "--under" (library-id) "(define (pid) 7)")))))
(for-each
  (lambda (v)
    (let* ((prepared ((cadr v)))
           (cur (cursor))
           (id (string-append "ident-" (symbol->string (car v))))
           (first (apply run (append ((caddr v)) (list "--req" id "--cursor" cur))))
           (second (apply run (append ((caddr v)) (list "--req" id "--cursor" cur "--premises" (current)))))
           (third (apply run (append ((caddr v)) (list "--req" id "--cursor" cur "--premises" stale)))))
      (want (format "C48b-6b ~a: the same request id without premises, with a current set and with a stale one is one request" (car v))
            (in-order (and (pair? first) (not (eq? (car first) 'error))) (clause second 'premises) (clause third 'premises))
            '(#t (premises not-checked (reason replay)) (premises not-checked (reason replay))))))
  ident-verbs)

;; NOT template apply: it takes no request id (req-not-tracked), so it has no
;; identity to keep premises out of. Pinned, so the exclusion is a fact and
;; not an omission.
(want "C48b-6b template apply takes no request id: refused req-not-tracked, so it has no identity"
      (let ((a (run 'template "apply" "memory" "--req" "ident-template" "--cursor" (cursor) "--premises" (current))))
        (and (pair? a) (list (car a) (cadr a) (caddr a))))
      '(error bad-request req-not-tracked))

;; ---- C48b-7: per-intent expects are unchanged ------------------------------------------------------------

(define (expect-batch)
  (let ((h (ver B)))
    (format "((expect ~s (set ~s title \"e1\")) (expect ~s (set ~s title \"e2\")))" h B h B)))
(want "C48b-7 a batch whose two items expect the initial hash applies the first and refuses the second, with and without a premise set"
      (let ((without (run 'batch (expect-batch)))
            (with (run 'batch (expect-batch) "--premises" (current))))
        (map (lambda (a) (and (pair? a) (eq? (car a) 'batch) (map car (cadr a)))) (list without with)))
      '((ok error) (ok error)))

;; ---- C48b-8: the call-site census ---------------------------------------------------------------------------

;; Every with-store-write call in the tree's sources -- every .sc, .ss and
;; .sls file, in any directory -- by file and the definition it sits in,
;; with `premises` or (exempt <why>). The calls are found by READING the
;; sources as data, so a comment or a string cannot be mistaken for one, and
;; store.sc is read like every other file.
;; ONE RULE EXEMPTS A WHOLE DIRECTORY: a call in a file under test/ is a
;; test's own, not a committed-write site. Those files are pinned by name
;; below and counted in the output; a product source the reader cannot read
;; is a failure, not a file with no calls.
(define census
  '((("code-project.sc" import-code) premises)
    (("datum-project.sc" execute) premises)
    (("project.sc" import-md-report) premises)
    (("rpc.sc" one-write) premises)
    (("rpc.sc" run-batch) premises)
    (("template.sc" template-apply!) premises)
    (("working.sc" working-commit!) premises)))
(define (source-files)
  (let ((out (string-append root "/sources.txt")))
    (system (string-append "cd .. && find . -type f \\( -name '*.sc' -o -name '*.ss' -o -name '*.sls' \\) -not -path './.git/*'"
                           " | sed 's#^\\./##' | LC_ALL=C sort > " (quoted out)))
    (let loop ((p (open-input-string (file-text out))) (acc '()))
      (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc)))))))
(define (read-source f)
  (call-with-input-file (string-append "../" f)
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (test-file? f) (and (>= (string-length f) 5) (string=? (substring f 0 5) "test/")))
(define test-call-files
  '("test/adopt1.sc" "test/code-import.sc" "test/commit-consumes.sc" "test/commit-premises.sc" "test/commit-text-mode.sc" "test/completion-stale.sc" "test/datum-import.sc" "test/datum-metadata1.sc" "test/doc-predicate.sc" "test/empty-plan.sc" "test/evidence-cli1-hang/holder.sc" "test/evidence-index1.sc" "test/md2.sc" "test/name-use-verbs.sc" "test/q7.sc" "test/q8.sc" "test/read1.sc" "test/resident.sc" "test/snap1.sc" "test/store1.sc" "test/unreadable-adopt.sc" "test/view-fields.sc" "test/view-rpc.sc"))
(define (calls-in form)
  (cond ((pair? form) (+ (if (eq? (car form) 'with-store-write) 1 0)
                         (let loop ((f form) (n 0)) (if (pair? f) (loop (cdr f) (+ n (calls-in (car f)))) n))))
        (else 0)))
;; EVERY TOP-LEVEL FORM of every library body is scanned, keyed by the name
;; it defines -- a procedure or a variable -- or <toplevel>; a definition's
;; head is not a call, so with-store-write's own definition counts only what
;; its body calls.
(define (form-name d)
  (and (pair? d) (eq? (car d) 'define) (pair? (cdr d))
       (if (pair? (cadr d)) (car (cadr d)) (cadr d))))
(define (scan-forms f forms)
  (apply append
    (map (lambda (top)
           (let ((body (if (and (pair? top) (eq? (car top) 'library)) (cdddr top) (list top))))
             (apply append
               (map (lambda (d)
                      (make-list (calls-in (if (form-name d) (cddr d) d))
                                 (list f (or (form-name d) '<toplevel>))))
                    body))))
         forms)))
(define (unique-sorted xs)
  (let loop ((l (list-sort string<? xs)) (out '()))
    (cond ((null? l) (reverse out))
          ((and (pair? out) (string=? (car l) (car out))) (loop (cdr l) out))
          (else (loop (cdr l) (cons (car l) out))))))
;; -> (product-calls test-call-files unreadable-product-files test-call-count
;;     unreadable-test-file-count)
(define (census-reading files reader)
  (let loop ((fs files) (calls '()) (bad '()))
    (if (null? fs)
        (let ((product (filter (lambda (c) (not (test-file? (car c)))) calls))
              (tests (filter (lambda (c) (test-file? (car c))) calls)))
          (list (list-sort key<? product) (unique-sorted (map car tests))
                (filter (lambda (f) (not (test-file? f))) (reverse bad)) (length tests)
                (length (filter test-file? bad))))
        (let ((forms (guard (e (#t #f)) (reader (car fs)))))
          (if forms
              (loop (cdr fs) (append calls (scan-forms (car fs) forms)) bad)
              (loop (cdr fs) calls (cons (car fs) bad)))))))
(define (scan-calls) (car (census-reading (source-files) read-source)))
(define (key<? a b) (string<? (format "~s" a) (format "~s" b)))
(define census-now (census-reading (source-files) read-source))
(printf "census: ~a calls under test/, in ~a files, exempt by the one rule; ~a test files the reader could not read\n"
        (cadddr census-now) (length (cadr census-now)) (list-ref census-now 4))
(want "C48b-8 the census names every with-store-write call in the tree, by file and definition, each labelled; test/ by its pinned files; no product source unread"
      (in-order (car census-now)
                (for-all (lambda (e) (or (eq? (cadr e) 'premises) (and (pair? (cadr e)) (eq? (car (cadr e)) 'exempt)))) census)
                (cadr census-now)
                (caddr census-now))
      (list (list-sort key<? (map car census)) #t test-call-files '()))
(want "C48b-8 CONTROL: a call in a file in a new directory is a product call, one under test/ is exempt, and an unreadable product source is named"
      (let ((r (census-reading '("newdir/x.sc" "test/y.sc" "bad.sc")
                               (lambda (f)
                                 (if (string=? f "bad.sc")
                                     (raise 'unreadable)
                                     '((library (x) (export) (import (rnrs))
                                         (define (w s) (with-store-write s (lambda (a b) '()))))))))))
        (list (car r) (cadr r) (caddr r) (equal? (car r) (list-sort key<? (map car census)))))
      '((("newdir/x.sc" w)) ("test/y.sc") ("bad.sc") #f))
(want "C48b-8 CONTROL: a call missing from the census, or one more than it holds, is named by the comparison"
      (in-order (equal? (list-sort key<? (scan-calls)) (list-sort key<? (map car (cdr census))))
                (equal? (list-sort key<? (scan-calls)) (list-sort key<? (cons '("store.sc" nowhere) (map car census)))))
      '(#f #f))
(want "C48b-8 CONTROL: the scan finds a call bound to a variable and one outside any definition, and not a definition's head"
      (scan-forms "x.sc" '((library (x) (export) (import (rnrs))
                             (define extra (lambda (s) (with-store-write s (lambda (st v) '()))))
                             (with-store-write "s" (lambda (st v) '()))
                             (define (with-store-write store proc) proc))))
      '(("x.sc" extra) ("x.sc" <toplevel>)))

;; ---- C48b-5b: every answer of a request that carried premises -------------------------------------------------
;;
;; Beside each answer, how many times a compiled check was asked: the clause
;; is there exactly when the check was not. A child process's calls are its
;; own and are not counted here; its rows read the clause alone.
(define (calls) ((eval 'premises-check-calls (environment '(theourgia premises)))))
(define (counted thunk)
  (let* ((n0 (calls)) (a (thunk)) (n1 (calls))) (cons a (- n1 n0))))
(want "C48b-5b a fresh write that succeeds: checked once, no clause"
      (let ((r (counted (lambda () (run 'set B "title" "fresh" "--premises" (current))))))
        (in-order (rpc-ok? (car r)) (clause (car r) 'premises) (cdr r)))
      '(#t #f 1))
(want "C48b-5b a fresh batch whose second item fails after the first was written: checked once, no clause"
      (let ((r (counted (lambda () (run 'batch (format "((set ~s title \"b1\") (expect \"0\" (set ~s title \"b2\")))" B B)
                                         "--premises" (current))))))
        (in-order (and (pair? (car r)) (eq? (car (car r)) 'batch) (map car (cadr (car r)))) (clause (car r) 'premises) (cdr r)))
      '((ok error) #f 1))
(define req-f-cursor (cursor))
(run 'set B "title" "seven" "--req" "req-f" "--cursor" req-f-cursor "--premises" (current))
(want "C48b-5b a replay: not checked, the clause says replay"
      (let ((r (counted (lambda () (run 'set B "title" "seven" "--req" "req-f" "--cursor" req-f-cursor "--premises" (current))))))
        (in-order (clause (car r) 'premises) (cdr r)))
      '((premises not-checked (reason replay)) 0))
(want "C48b-5b req-mismatch, a verdict that refuses before any write: not checked, the clause says not-fresh"
      (let ((r (counted (lambda () (run 'set B "title" "other" "--req" "req-f" "--cursor" req-f-cursor "--premises" (current))))))
        (in-order (head (car r)) (clause (car r) 'premises) (cdr r)))
      '((error req-mismatch) (premises not-checked (reason not-fresh)) 0))
(want "C48b-5b commit's arm that writes nothing: checked once, no clause"
      (let* ((txt (cdr (assq 'src (cdr (assq 'fields (state-read (state) C))))))
             (w (run 'write C txt "--writer" W))
             (r (counted (lambda () (run 'commit C "--writer" W "--premises" (current))))))
        (in-order (rpc-ok? (car r)) (clause (car r) 'premises) (cdr r)))
      '(#t #f 1))
(want "C48b-5b a refusal before the session (def's name-exists): not checked, no clause, nothing written"
      (let* ((before (log-digest))
             (r (counted (lambda () (run 'def "pf" "--under" (library-id) "(define (pf) 9)" "--premises" (current)))))
             (after (log-digest)))
        (in-order (head (car r)) (clause (car r) 'premises) (cdr r) (equal? before after)))
      '((error name-exists) #f 0 #t))
;; A FRESH WRITE THAT ENDS UNKNOWN AFTER BYTES WERE WRITTEN, on a store of its
;; own (the partial record it leaves would trouble the rows above): the check
;; ran before the first byte, so the answer carries no clause.
(define fault-store (string-append root "/f"))
(rpc-dispatch fault-store '(init) "test")
(define F (new-id (rpc-dispatch fault-store (list 'insert "--title" "faulted") "test")))
(define WF (car (string-split-dot F)))
(rpc-dispatch fault-store (list 'write F "faulted draft" "--writer" WF) "test")
(want "C48b-5b a fresh commit that ends unknown after bytes were written: the check ran once, no clause"
      (let* ((r (child-dispatch "write-eio-after-partial@commit" fault-store
                                (list 'commit F "--writer" WF "--premises"
                                      (premises (block-premise F (block-hash (open-and-reduce fault-store) F))))))
             (a (child-answer r)))
        (in-order (and (pair? a) (list (car a) (cadr a))) (clause a 'premises) (child-calls r)))
      '((error unknown) #f 1))
;; A WRITE THAT RAISES AFTER IT WAS ENTERED: import-md reads its files inside
;; the write session, before the check, and a file it cannot read raises
;; there. (A file that is not UTF-8 does not: it is read and imported.) The
;; request wrote nothing and its premises were never examined; the answer
;; says so.
(define bad-md (string-append root "/badmd"))
(system (string-append "mkdir -p '" bad-md "' && printf '# T\\n' > '" bad-md "/bad.md' && chmod 000 '" bad-md "/bad.md'"))
(want "C48b-5b a write that raises after entry, before the check (import-md, a file it cannot read): not checked, the clause says not-fresh"
      (let ((r (counted (lambda () (run 'import-md bad-md "--premises" (current))))))
        (in-order (and (pair? (car r)) (eq? (car (car r)) 'error)) (clause (car r) 'premises) (cdr r)))
      '(#t (premises not-checked (reason not-fresh)) 0))
;; A RAISE AFTER ENTRY AT EVERY SITE, not import-md's alone: a working packet
;; whose captured operation is not a list (operation-packet.sc checks only
;; its length) makes import-code --datum and def raise inside the write
;; session, before the check. Each request is first sent with a stale set,
;; so its packet is captured and nothing is written; the packet is then
;; damaged and the request sent again under its id.
(define packet-store (string-append root "/pk"))
(define PW (cadr (assq 'writer (cdr (rpc-dispatch packet-store '(init) "test")))))
(define (run-pk . args) (rpc-dispatch packet-store args "test"))
(define pk-src (string-append root "/pklib"))
(define pk-src2 (string-append root "/pklib2"))
(system (string-append "mkdir -p " (quoted pk-src) " " (quoted pk-src2)))
(write-file! (string-append pk-src "/pk.sc") "(library (pk) (export k) (import (rnrs))\n(define (k) 1))\n")
(write-file! (string-append pk-src2 "/pk2.sc") "(library (pk2) (export k2) (import (rnrs))\n(define (k2) 2))\n")
(run-pk 'import-code pk-src "--datum")
(define PK (new-id (run-pk 'insert "--title" "packet premise")))
(define PK-old (block-hash (open-and-reduce packet-store) PK))
(run-pk 'set PK "title" "packet premise, changed")
(define pk-stale (premises (block-premise PK PK-old)))
(define pk-lib
  (let ((st (open-and-reduce packet-store)))
    (find (lambda (id) (let ((f (assq 'kind (cdr (assq 'fields (state-read st id)))))) (and f (eq? (cdr f) 'library))))
          (state-block-ids st))))
(define pk-cursor (string-append PW ":" (number->string (cdr (assoc PW (reduce-applied-cut (open-and-reduce packet-store)))))))
(define (pk-import) (run-pk 'import-code pk-src2 "--datum" "--req" "pk-import" "--cursor" pk-cursor "--premises" pk-stale))
(define (pk-def) (run-pk 'def "pq" "--under" pk-lib "(define (pq) 3)" "--req" "pk-def" "--cursor" pk-cursor "--premises" pk-stale))
(want "C48b-5b the first sends were refused for their premise"
      (let* ((i (pk-import)) (d (pk-def))) (map head (list i d)))
      '((error premise-changed) (error premise-changed)))
(define (damage-packets! s)
  (let ((dir (string-append s "/operation-packets")))
    (fold-left (lambda (n f)
                 (if (string=? f "lock")
                     n
                     (let* ((p (string-append dir "/" f))
                            (saved (storable-decode (string->sexpr-extended (file-text p)))))
                       (write-file! p (sexpr->string-extended
                                        (storable-encode (list (list-ref saved 0) (list-ref saved 1) (list-ref saved 2)
                                                               (list-ref saved 3) 7))))
                       (+ n 1))))
               0 (directory-list dir))))
(want "C48b-5b both captured packets were damaged"
      (damage-packets! packet-store)
      2)
(want "C48b-5b a write that raises after entry at import-code --datum and def (a damaged packet): not checked, the clause says not-fresh"
      (map (lambda (send)
             (let ((r (counted send)))
               (in-order (and (pair? (car r)) (eq? (car (car r)) 'error)) (clause (car r) 'premises) (cdr r))))
           (list pk-import pk-def))
      '((#t (premises not-checked (reason not-fresh)) 0) (#t (premises not-checked (reason not-fresh)) 0)))
;; THE CLASSIFICATION HOOK ON BOTH ROUTES: a raise after entry is answered
;; not-fresh by the command line's local route (core.sc, THEOURGIA_LOCAL=1)
;; and by the daemon (theourgia.sc), each on a store of its own. A hook that
;; was never installed would read exactly as the defect it closes. Each row
;; also counts the daemons serving its store right after the send: none on
;; the local route, one on the daemon route, so the route is a reading.
(define route-run (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/cpr-" (number->string (get-process-id))))
(define (route-send env program st . args)
  (let ((out (string-append root "/route.out")))
    (system (string-append "THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted route-run) " " env
                           " perl -e 'alarm 120; exec @ARGV' scheme --script " program " "
                           (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                           "--store " (quoted st) " > " (quoted out) " 2>/dev/null < /dev/null"))
    (guard (e (#t (list 'unreadable (file-text out)))) (read (open-string-input-port (file-text out))))))
(define (daemons-on st)
  (length (filter (lambda (l) (> (string-length l) 0))
                  (let ((t (let ((p (process (string-append "pgrep -f " (quoted (string-append "[t]heourgiad.sc serve " st))))))
                             (let ((x (get-string-all (car p)))) (close-port (car p)) (close-port (cadr p)) x))))
                    (if (eof-object? t) '() (let split ((cs (string->list t)) (cur '()) (acc '()))
                                              (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
                                                    ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                    (else (split (cdr cs) (cons (car cs) cur) acc)))))))))
(define (stop-daemons! st)
  (let ((pattern (quoted (string-append "[t]heourgiad.sc serve " st))))
    (system (string-append "pkill -TERM -f " pattern " ; for i in 1 2 3 4 5 6 7 8 9 10; do pgrep -f " pattern
                           " > /dev/null || break; sleep 0.3; done; pkill -KILL -f " pattern " ; true"))))
(define (packet-fixture! name)
  (let* ((st (string-append root "/" name))
         (w (cadr (assq 'writer (cdr (rpc-dispatch st '(init) "test")))))
         (lib (string-append root "/" name "-lib")))
    (system (string-append "mkdir -p " (quoted lib)))
    (write-file! (string-append lib "/r.sc") "(library (r) (export r) (import (rnrs))\n(define (r) 1))\n")
    (let* ((p (new-id (rpc-dispatch st (list 'insert "--title" "route premise") "test")))
           (old (block-hash (open-and-reduce st) p)))
      (rpc-dispatch st (list 'set p "title" "route premise, changed") "test")
      (list st lib (premises (block-premise p old))
            (string-append w ":" (number->string (cdr (assoc w (reduce-applied-cut (open-and-reduce st))))))))))
(define (route-row env program name)
  (let* ((f (packet-fixture! name))
         (st (car f))
         (send (lambda () (route-send env program st "import-code" (cadr f) "--datum" "--req" (string-append name "-req")
                                      "--cursor" (cadddr f) "--premises" (caddr f)))))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let* ((first (send)) (damaged (damage-packets! st)) (second (send)) (serving (daemons-on st)))
          (list (head first) damaged (and (pair? second) (eq? (car second) 'error)) (clause second 'premises) serving)))
      (lambda () (stop-daemons! st)))))
(want "C48b-5b a raise after entry on the command line's local route: the clause says not-fresh"
      (route-row "THEOURGIA_LOCAL=1" cli "rl")
      '((error premise-changed) 1 #t (premises not-checked (reason not-fresh)) 0))
(want "C48b-5b a raise after entry on the daemon route: the clause says not-fresh, and no daemon is left"
      (let ((r (route-row "" "../theourgia.sc" "rd")))
        (list r (daemons-on (string-append root "/rd"))))
      '(((error premise-changed) 1 #t (premises not-checked (reason not-fresh)) 1) 0))
;; ONE POINT, BEFORE EVERYTHING ELSE: the dispatcher reads a request's set at
;; its entry, before it asks whether the store exists and before any verb's
;; own checks -- insert's usage, move's and link's arity, commit's writer.
(define malformed "((frobnicate 1))")
(want "C48b-3 the set is read at the dispatcher's entry: before a verb's usage, its arity, commit's writer and the store probe"
      (map head3 (list (run 'insert "--premises" malformed)
                       (run 'move B "--premises" malformed)
                       (run 'link B "--premises" malformed)
                       (run 'commit B "--premises" malformed)
                       (rpc-dispatch (string-append root "/no-such-store") (list 'set "x.1" "title" "v" "--premises" malformed) "test")))
      (make-list 5 '(error bad-request premise-not-understood)))
(define (dispatch-point-row env program name)
  (let ((st (string-append root "/" name)))
    (rpc-dispatch st '(init) "test")
    (dynamic-wind
      (lambda () #f)
      (lambda () (let ((a (route-send env program st "insert" "--premises" malformed))) (list (head3 a) (daemons-on st))))
      (lambda () (stop-daemons! st)))))
(want "C48b-3 the set is read at the dispatcher's entry on the command line's local route and the daemon route, and no daemon is left"
      (let* ((local (dispatch-point-row "THEOURGIA_LOCAL=1" cli "dl"))
             (daemon (dispatch-point-row "" "../theourgia.sc" "dd")))
        (list local daemon (daemons-on (string-append root "/dd"))))
      '(((error bad-request premise-not-understood) 0) ((error bad-request premise-not-understood) 1) 0))

;; A PARTIAL BATCH: a tracked batch killed after its receipt, then retried.
;; The batch's verdict refuses to re-run what ran, before any write: not
;; checked, and the clause says so.
(define req-g-cursor (cursor))
(define req-g-intents (string-append root "/req-g-intents"))
(write-file! req-g-intents (format "((set ~s title \"p1\") (set ~s title \"p2\"))" B B))
;; NEVER: ON THE COMMAND LINE A BATCH'S INTENTS ARE ITS STANDARD INPUT, and a
;; positional beside it is a second answer to one question (usage).
(define batch-crashed
  (car (crash-at 2 root home cli
                 (list "batch" "--req" "req-g" "--cursor" req-g-cursor "--premises" (current) "--store" store)
                 req-g-intents)))
(want "C48b-5b the batch crash happened after its receipt"
      batch-crashed
      2)
(want "C48b-5b a partial batch: not checked, the clause says not-fresh (and it is the batch's own verdict, not a req-mismatch)"
      (let ((r (counted (lambda () (run-as-cli 'batch (format "((set ~s title \"p1\") (set ~s title \"p2\"))" B B)
                                                "--req" "req-g" "--cursor" req-g-cursor "--premises" (current))))))
        (in-order (contains? (car r) 'req-mismatch) (clause (car r) 'premises) (cdr r)))
      '(#f (premises not-checked (reason not-fresh)) 0))
;; NOT YET WRITTEN, AND WHY: resolved-executed. That verdict comes only from a
;; resolution record (request.sc), which no verb writes; q5.sc builds one at
;; the request level, below the write path a premise is checked on. This path
;; has no input that reaches it today; a row is written when a verb writes a
;; resolution record.

;; ON DEMAND: the premises library is loaded by a request that carries a
;; set, and by nothing else -- not by a refused verb, not by a write
;; without the option (store.sc and rpc.sc test the gate and the option
;; without it).
(define on-demand (string-append root "/ondemand.sc"))
(write-file! on-demand
  (string-append
    "(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))\n"
    "(register-verbs! extension-verbs)\n"
    "(define (loaded?) (and (member '(theourgia premises) (library-list)) #t))\n"
    (format "(define store ~s)\n" store)
    "(rpc-dispatch store '(no-such-verb) \"test\")\n"
    "(define after-refused (loaded?))\n"
    "(rpc-dispatch store '(insert \"--title\" \"on demand\") \"test\")\n"
    "(define after-plain (loaded?))\n"
    "(rpc-dispatch store '(insert \"--title\" \"on demand two\" \"--premises\" \"()\") \"test\")\n"
    "(write (list after-refused after-plain (loaded?)))\n"))
(want "ON DEMAND the premises library is absent after a refused verb and a write without the option, present after one with a set"
      (let ((out (string-append root "/ondemand.out")))
        (system (string-append "scheme --script " (quoted on-demand) " > " (quoted out) " 2>/dev/null < /dev/null"))
        (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-string-input-port (file-text out)))))
      '(#f #f #t))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\ncommit-premises complete\n" rows bad)
(exit (if (= bad 0) 0 1))
