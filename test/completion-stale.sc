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

;; COMPLETING A PERSISTED PLAN DOES NOT OVERWRITE WHAT OTHERS WROTE SINCE.
;;
;; A request of several sub-operations writes its plan first and its
;; members after it. A process that dies in between leaves the plan on
;; disk with members missing, and a retry finishes it from the plan. What
;; it must not do is write a missing member over a block that another
;; request wrote after this one was admitted.
;;
;; THE RULE (the brief's C1 to C9): before anything is written, the
;; frozen text against its version, then the plan's markers, then every
;; missing member's target -- stale when an applied record that touches
;; it lies outside the plan's causal cut and is not one of the plan's own
;; members, or, for a commit member, when the block's hash is not the
;; based-on its consumes item froze. One stale target and nothing is
;; written. After each member written: the plan and that record must be
;; applied, and if anything else became applied because of the write the
;; members still missing are judged again.
;;
;; THE CRASHES ARE REAL: crash-at.ss runs the product's own entry as a
;; child and kills it at the n-th append. Each crash row first asserts
;; where the child stopped.
;;
;; Rows are named by the brief's cells, K1 to K20.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia request) (theourgia ffi) (theourgia wire) (theourgia working)
        (theourgia log) (theourgia baseline) (theourgia code-project)
        (theourgia code-markers) (theourgia languages) (theourgia datum-code))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
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

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define cli (string-append script-dir "/../core.sc"))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/completion-stale-" (number->string (get-process-id))))
(when (file-exists? root) (error 'completion-stale "Use a fresh test root" root))
(mkdir-p! root)
(define home (string-append root "/home"))
(mkdir-p! home)
(putenv "THEOURGIA_HOME" home)

(include "crash-at.ss")

;; ---- a store, its blocks, its drafts -----------------------------------------

(define counter 0)
(define (fresh-dir tag)
  (set! counter (+ counter 1))
  (let ((d (string-append root "/" tag (number->string counter))))
    (mkdir-p! d)
    d))
;; -> (store . writer)
(define (fresh-store tag)
  (let* ((d (string-append (fresh-dir tag) "/store"))
         (init (rpc-dispatch d '(init) "test")))
    (cons d (cadr (assq 'writer (cdr init))))))
(define draft-verbs '(write restore drafts discard commit))
(define (call st . args)
  (rpc-dispatch (car st)
                (if (and (or (memq (car args) draft-verbs)
                             (and (eq? (car args) 'read)
                                  (or (member "--working" (cdr args)) (member "--working-info" (cdr args)))))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" (cdr st)))
                    args)
                "test"))
(define (state st) (open-and-reduce (car st)))
(define (text v) (if (bytevector? v) (utf8->string v) v))
(define (src st id)
  (let* ((b (state-read (state st) id)) (p (and b (assq 'src (cdr (assq 'fields b))))))
    (and p (text (cdr p)))))
(define (tomb? st id)
  (let ((b (state-read (state st) id))) (and b (cdr (assq 'deleted b)) #t)))
(define (event-of answer) (car (cadr (assq 'events (cdr answer)))))
(define (new-block st title body)
  (let ((ev (event-of (call st 'insert "--title" title "--text" body))))
    (block-id (car ev) (cdr ev))))
(define (version st id)
  (list-ref (assq 'projection (cdr (call st 'read id "--working-info"))) 4))
(define (cursor-of st)
  (let ((p (assoc (cdr st) (reduce-applied-cut (state st)))))
    (string-append (cdr st) ":" (number->string (if p (cdr p) 0)))))
(define (last-seq st)
  (let ((p (assoc (cdr st) (reduce-applied-cut (state st))))) (if p (cdr p) 0)))

;; How many records the completing writer's segments hold: what was
;; APPENDED, which the applied cut is not (a record taken back leaves the
;; applied cut shorter while it stays on disk).
(define (record-count st)
  (apply + (map (lambda (f) (length (filter (lambda (b) (= b 10)) (bytevector->u8-list (cdr f))))) (log-bytes st))))
;; The bytes of the completing writer's segments: "writes nothing" is
;; these, before and after.
(define (log-bytes st)
  (let ((dir (writer-directory (car st) (cdr st))))
    (map (lambda (f) (cons f (call-with-port (open-file-input-port (string-append dir "/" f)) get-bytevector-all)))
         (list-sort string<? (filter (lambda (f) (and (> (string-length f) 5)
                                                      (string=? ".sexp" (substring f (- (string-length f) 5) (string-length f)))))
                                     (directory-list dir))))))

;; The command line of a commit of IDS under REQ.
(define (commit-args st ids req cursor)
  (append (list "commit") ids
          (list "--req" req "--cursor" cursor)
          (apply append (map (lambda (id) (list "--working-version" (string-append id "=" (version st id)))) ids))
          (list "--writer" (cdr st) "--store" (car st))))

;; Runs the product's entry with ARGS and reads the last datum it printed.
(define (cli-run args)
  (let ((out (string-append root "/cli" (number->string (begin (set! counter (+ counter 1)) counter)) ".txt")))
    (system (string-append "env THEOURGIA_HOME=" (crash-at-quote home) " scheme --script " (crash-at-quote cli)
                           (apply string-append (map (lambda (a) (string-append " " (crash-at-quote a))) args))
                           " > " (crash-at-quote out) " 2>&1 < /dev/null"))
    (call-with-input-file out
      (lambda (p)
        (let loop ((last #f))
          (let ((x (guard (e (#t 'unreadable)) (read p))))
            (cond ((eof-object? x) last)
                  ((eq? x 'unreadable) last)
                  (else (loop x)))))))))

(define (evidence st req) (store-evidence (car st) (cons (cdr st) req)))
(define (subs st req) (map (lambda (e) (actor-sub (ev-actor e))) (evidence st req)))
(define (plan-ev st req) (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) (evidence st req)))
;; The plan's entries, (index . intent), and its event.
(define (plan-entries st req) (list-ref (ev-payload (plan-ev st req)) 4))
(define (plan-event st req) (ev-event (plan-ev st req)))
(define (member-block st req k) (cadr (cdr (assv k (plan-entries st req)))))

(define (mentions? x id)
  (cond ((equal? x id) #t) ((pair? x) (or (mentions? (car x) id) (mentions? (cdr x) id))) (else #f)))
(define (clause name answer) (let ((p (and (pair? answer) (assq name (filter pair? answer))))) p))
(define (head2 a) (and (pair? a) (pair? (cdr a)) (list (car a) (cadr a))))
;; The answers of a batch, or the one answer.
(define (answers a) (if (and (pair? a) (eq? (car a) 'batch)) (cadr a) (list a)))
(define (last-answer a) (car (reverse (answers a))))
(define (since-events a) (map car (cdr (or (clause 'since a) '(since)))))

;; A crash of a commit of IDS at its N-th append. -> barriers.
(define (crash-commit! st ids req n)
  (let ((args (commit-args st ids req (cursor-of st))))
    (values (car (crash-at n root home cli args)) args)))

;; ---- K1 (control): a crash after member 0; a new draft; the retry ---------------

(let* ((st (fresh-store "k1"))
       (A (new-block st "A" "old A")) (B (new-block st "B" "old B")))
  (call st 'write A "frozen A") (call st 'write B "frozen B")
  (let-values (((barriers args) (crash-commit! st (list A B) "R1" 3)))
    (want "K1 the child stopped at the third append" barriers 3)
    (want "K1 the plan and member 0 are on disk" (subs st "R1") '(plan 0))
    (let ((second (member-block st "R1" 1)))
      (call st 'write second "a newer draft")
      (let* ((before (length (state->rows (state st))))
             (n0 (last-seq st))
             (retry (cli-run args)))
        (want "K1 the retry succeeds" (car retry) 'ok)
        (want "K1 it completes member 1" (subs st "R1") '(plan 0 1))
        (want "K1 exactly one record is written" (- (last-seq st) n0) 1)
        (want "K1 from the plan's frozen text" (src st second) (if (equal? second B) "frozen B" "frozen A"))
        (want "K1 the newer draft is still a draft" (call st 'read second "--working") '(ok (text "a newer draft")))))))

;; ---- K2: another request sets C after the plan ---------------------------------

(define (four st)
  (let ((A (new-block st "A" "old A")) (B (new-block st "B" "old B"))
        (C (new-block st "C" "old C")) (D (new-block st "D" "old D")))
    (for-each (lambda (id t) (call st 'write id t)) (list A B C D) '("new A" "new B" "new C" "new D"))
    (list A B C D)))

(define (new-text id ids) (list-ref '("new A" "new B" "new C" "new D") (let loop ((is ids) (k 0)) (if (equal? (car is) id) k (loop (cdr is) (+ k 1))))))
(define (old-text st id ids) (list-ref '("old A" "old B" "old C" "old D") (let loop ((is ids) (k 0)) (if (equal? (car is) id) k (loop (cdr is) (+ k 1))))))

(define (k2-like tag other!)
  (let* ((st (fresh-store tag)) (ids (four st)))
    (let-values (((barriers args) (crash-commit! st ids "R2" 2)))
      (let* ((e (plan-event st "R2"))
             (C (member-block st "R2" 2))
             (ev (other! st C ids))
             (before (log-bytes st))
             (retry (cli-run args)))
        (list st ids barriers args e C ev before retry)))))

(let* ((r (k2-like "k2" (lambda (st C ids) (event-of (call st 'set C "src" "theirs")))))
       (st (list-ref r 0)) (ids (list-ref r 1)) (e (list-ref r 4)) (C (list-ref r 5)) (ev (list-ref r 6))
       (retry (list-ref r 8)))
  (want "K2 the child stopped at the second append" (list-ref r 2) 2)
  (want "K2 the retry writes nothing" (log-bytes st) (list-ref r 7))
  (want "K2 it answers stale-baseline" (head2 retry) '(error stale-baseline))
  (want "K2 for C" (clause 'block retry) (list 'block C))
  (want "K2 with the block's hash now" (clause 'now retry) (list 'now (block-hash (state st) C)))
  (want "K2 the other's record first in since" (car (since-events retry)) ev)
  (want "K2 and the completion clause" (clause 'completion retry) (list 'completion (list 'plan e) '(present) '(of 4)))
  (want "K2 C reads the other's text" (src st C) "theirs")
  (want "K2 the others their old text"
        (map (lambda (id) (src st id)) (filter (lambda (id) (not (equal? id C))) ids))
        (map (lambda (id) (old-text st id ids)) (filter (lambda (id) (not (equal? id C))) ids)))
  ;; K7: a second retry is refused the same way; the drafts rebased and
  ;; committed under a NEW request id land whole.
  (let ((again (cli-run (list-ref r 3))))
    (want "K7 a second retry is refused the same way" again retry))
  (call st 'discard C)
  (call st 'write C (new-text C ids))
  (let ((fresh (apply call st 'commit
                      (append ids (list "--req" "R7" "--cursor" (cursor-of st))
                              (apply append (map (lambda (id) (list "--working-version" (string-append id "=" (version st id)))) ids))))))
    (want "K7 the rebased drafts land whole under a new request id" (car fresh) 'ok)
    (want "K7 every block has the new text" (map (lambda (id) (src st id)) ids) '("new A" "new B" "new C" "new D"))))

;; ---- K3: deleting C, moving C, linking from C ----------------------------------

(for-each
  (lambda (case-name other!)
    (let* ((r (k2-like "k3" other!)) (st (list-ref r 0)) (C (list-ref r 5)) (retry (list-ref r 8)))
      (want (string-append "K3 " case-name ": the retry writes nothing") (log-bytes st) (list-ref r 7))
      (want (string-append "K3 " case-name ": stale-baseline for C") (list (head2 retry) (clause 'block retry))
            (list '(error stale-baseline) (list 'block C)))
      (want (string-append "K3 " case-name ": the other's record in since") (car (since-events retry)) (list-ref r 6))
      (when (equal? case-name "delete")
        (want "K3 delete: C stays a tombstone" (tomb? st C) #t))))
  '("delete" "move" "link")
  (list (lambda (st C ids) (event-of (call st 'del C)))
        (lambda (st C ids) (event-of (call st 'move C (find (lambda (x) (not (equal? x C))) ids))))
        (lambda (st C ids) (event-of (call st 'link C "cites" (find (lambda (x) (not (equal? x C))) ids))))))

;; ---- K4: two blocks written by others ------------------------------------------

(let* ((st (fresh-store "k4")) (ids (four st)))
  (let-values (((barriers args) (crash-commit! st ids "R4" 2)))
    (let* ((C (member-block st "R4" 2)) (D (member-block st "R4" 3)) (e (plan-event st "R4")))
      (call st 'set C "src" "theirs C") (call st 'set D "src" "theirs D")
      (let* ((before (log-bytes st)) (retry (cli-run args)))
        (want "K4 the retry writes nothing" (log-bytes st) before)
        (want "K4 one answer naming both blocks"
              (list (head2 retry) (map (lambda (b) (cadr (assq 'block (filter pair? b)))) (cdr (clause 'blocks retry))))
              (list '(error stale-baseline) (list C D)))
        (want "K4 with the completion clause" (clause 'completion retry) (list 'completion (list 'plan e) '(present) '(of 4)))))))

;; ---- K5: a crash prefix --------------------------------------------------------

(let* ((st (fresh-store "k5")) (ids (four st)))
  (let-values (((barriers args) (crash-commit! st ids "R5" 3)))
    (want "K5 the child stopped at the third append" barriers 3)
    (let* ((C (member-block st "R5" 2)) (A (member-block st "R5" 0)) (e (plan-event st "R5")))
      (call st 'set C "src" "theirs")
      (let* ((before (log-bytes st)) (retry (cli-run args)))
        (want "K5 the retry writes nothing more" (log-bytes st) before)
        (want "K5 present is member 0" (clause 'completion retry) (list 'completion (list 'plan e) '(present 0) '(of 4)))
        (want "K5 member 0's block has the frozen text" (src st A)
              (list-ref '("new A" "new B" "new C" "new D") (let loop ((is ids) (k 0)) (if (equal? (car is) A) k (loop (cdr is) (+ k 1))))))))))

;; ---- K6 (control): writes the plan does not touch ------------------------------

(let* ((st (fresh-store "k6")) (ids (four st)) (E (new-block st "E" "old E")))
  (let-values (((barriers args) (crash-commit! st ids "R6" 2)))
    (call st 'set E "src" "theirs E")
    (call st 'insert "--title" "child" "--under" (member-block st "R6" 0))
    (let ((retry (cli-run args)))
      (want "K6 the retry completes whole" (list (car retry) (subs st "R6")) '(ok (plan 0 1 2 3))))))

;; ---- K8 (control): a record in the plan's past is taken back --------------------

(let* ((st (fresh-store "k8")) (ids (four st)))
  (call st 'insert "--title" "T" "--req" "R0" "--cursor" (cursor-of st))
  (let-values (((barriers args) (crash-commit! st ids "R8" 2)))
    (let* ((t (find (lambda (e) (eq? 'single (actor-sub (ev-actor e)))) (evidence st "R0")))
           (rival (encode-record 1 1789000000001 (ev-actor t) '() (storable-encode (ev-payload t)))))
      (log-publish! (car st) "rivalzzz" 1 rival (segment-sha rival))
      (let* ((before (log-bytes st)) (retry (cli-run args)))
        (want "K8 the retry answers unknown" (head2 retry) '(error unknown))
        (want "K8 and writes nothing" (log-bytes st) before)))))

;; ---- K11: two attempts ---------------------------------------------------------

(let* ((st (fresh-store "k11")) (ids (four st)))
  (let-values (((barriers args) (crash-commit! st ids "R11" 2)))
    (let ((second (car (crash-at 2 root home cli args))))
      (want "K11 the second attempt stopped after member 0" (list second (subs st "R11")) '(2 (plan 0)))
      (let ((C (member-block st "R11" 2)) (e (plan-event st "R11")))
        (call st 'set C "src" "theirs")
        (let* ((before (log-bytes st)) (retry (cli-run args)))
          (want "K11 the third attempt writes nothing" (log-bytes st) before)
          (want "K11 and names C" (list (head2 retry) (clause 'block retry)) (list '(error stale-baseline) (list 'block C)))
          (want "K11 present is member 0" (clause 'completion retry) (list 'completion (list 'plan e) '(present 0) '(of 4))))))))
(let* ((st (fresh-store "k11b")) (ids (four st)))
  (let-values (((barriers args) (crash-commit! st ids "R11" 2)))
    (call st 'set (member-block st "R11" 2) "src" "theirs")
    (let* ((before (log-bytes st)) (retry (cli-run args)))
      (want "K11 with the other's write before the second attempt, it writes nothing at all" (log-bytes st) before)
      (want "K11 and only the plan is on disk" (subs st "R11") '(plan)))))

;; ---- K9, K10, K12, K14: the imports ---------------------------------------------

(define py (language-for-name 'python))
(define scm (language-for-name 'scheme))
(define (write-file! path bytes)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bytes))))
(define (b s) (string->utf8 s))
;; An import case: a store with a.py of four entries imported, and an edit dir.
(define (import-case tag files)
  (let* ((area (fresh-dir tag)) (store (string-append area "/store"))
         (input (string-append area "/input")) (edit (string-append area "/edit")))
    (mkdir-p! input) (mkdir-p! edit)
    (let ((init (rpc-dispatch store '(init) "test")))
      (for-each (lambda (f) (write-file! (string-append input "/" (car f)) (cdr f))) files)
      (unless (rpc-ok? (rpc-dispatch store (list 'import-code input) "test")) (error 'fixture "initial import failed"))
      (list (cons store (cadr (assq 'writer (cdr init)))) edit))))
(define (file-id st path)
  (let ((s (state st))) (find (lambda (id) (equal? (code-field s id 'path) path)) (code-files s))))
(define (children st id) (code-children (state st) id))
(define (header st file) (list (store-id-of (car st)) file (reduce-applied-cut (state st))))
(define (import-args st edit req . extra)
  (append (list "import-code" edit) extra (list "--req" req "--cursor" (cursor-of st) "--store" (car st))))
(define (csrc st id) (text (code-field (state st) id 'src)))

(define e1 (b "def e1():\n  return 1\n"))
(define e2 (b "def e2():\n  return 2\n"))
(define e3 (b "def e3():\n  return 3\n"))
(define e4 (b "def e4():\n  return 4\n"))
(define (changed n) (b (string-append "def e" (number->string n) "():\n  return 10" (number->string n) "\n")))

;; K9: change three entries of an existing file and delete a fourth.
(define (k9 tag pick)
  (let* ((c (import-case tag (list (cons "a.py" (projection-encode py #f (list (list "new" e1) (list "new" e2) (list "new" e3) (list "new" e4)))))))
         (st (car c)) (edit (cadr c)) (a (file-id st "a.py")) (ids (children st a)))
    (write-file! (string-append edit "/a.py")
                 (projection-encode py (header st a) (list (list (list-ref ids 0) (changed 1)) (list (list-ref ids 1) (changed 2))
                                                           (list (list-ref ids 2) (changed 3)))))
    (let* ((args (import-args st edit "R9" "--allow-delete"))
           (barriers (car (crash-at 2 root home cli args)))
           (target (pick ids)))
      (call st 'set target "src" "def theirs():\n  pass\n")
      (let* ((before (log-bytes st)) (retry (cli-run args)))
        (want (string-append "K9 " tag ": the child stopped after the plan") barriers 2)
        (want (string-append "K9 " tag ": the retry writes nothing") (log-bytes st) before)
        (want (string-append "K9 " tag ": and names the block") (list (head2 retry) (clause 'block retry))
              (list '(error stale-baseline) (list 'block target)))))))
(k9 "k9set" (lambda (ids) (list-ref ids 1)))
(k9 "k9del" (lambda (ids) (list-ref ids 3)))

;; K9 for import-code --datum: a library of four definitions.
(define (datum-case tag text)
  (let* ((area (fresh-dir tag)) (store (string-append area "/store"))
         (input (string-append area "/input")) (edit (string-append area "/edit")))
    (mkdir-p! input) (mkdir-p! edit)
    (let ((init (rpc-dispatch store '(init) "test")))
      (write-file! (string-append input "/a.sc") (b text))
      (unless (rpc-ok? (rpc-dispatch store (list 'import-code input "--datum") "test")) (error 'fixture "initial datum import failed"))
      (list (cons store (cadr (assq 'writer (cdr init)))) edit))))
(define (library-id st name)
  (let ((s (state st))) (find (lambda (id) (equal? (code-field s id 'name) name)) (map cadr (state-datum s)))))
(define (render-datum st lib rows exports)
  (string-append
    (utf8->string (projection-header-line scm (header st lib) 'datum 0))
    "(library (a)\n" (datum-print (cons 'export exports)) (datum-print '(import (rnrs)))
    (apply string-append
      (map (lambda (row)
             (string-append (utf8->string (marker-line scm (string-append "@block " (car row)))) (datum-print (cadr row))))
           rows))
    ")\n"))
(let* ((c (datum-case "k9d" "(library (a) (export w x y z) (import (rnrs))\n(define w 1)\n(define x 2)\n(define y 3)\n(define z 4))\n"))
       (st (car c)) (edit (cadr c)) (lib (library-id st '(a))) (ids (children st lib)))
  (write-file! (string-append edit "/a.sc")
               (b (render-datum st lib (list (list (list-ref ids 0) '(define w 10)) (list (list-ref ids 1) '(define x 20))
                                             (list (list-ref ids 2) '(define y 30)))
                                '(w x y))))
  (let* ((args (append (list "import-code" edit "--datum" "--allow-delete") (list "--req" "R9d" "--cursor" (cursor-of st) "--store" (car st))))
         (barriers (car (crash-at 2 root home cli args)))
         (target (list-ref ids 1)))
    (with-store-write (car st) (lambda (s v) (list (list 'set target 'body '(define x 99)))) "test")
    (let* ((before (log-bytes st)) (retry (cli-run args)))
      (want "K9 datum: the child stopped after the plan" barriers 2)
      (want "K9 datum: the retry writes nothing" (log-bytes st) before)
      (want "K9 datum: and names the block" (list (head2 retry) (clause 'block retry)) (list '(error stale-baseline) (list 'block target))))))

;; K10: own progress is not foreign. An import that changes an entry AND
;; moves it (two members on one block), stopped after the first of them.
(let* ((c (import-case "k10" (list (cons "a.py" (projection-encode py #f (list (list "new" e1) (list "new" e2) (list "new" e3)))))))
       (st (car c)) (edit (cadr c)) (a (file-id st "a.py")) (ids (children st a)))
  (write-file! (string-append edit "/a.py")
               (projection-encode py (header st a) (list (list (list-ref ids 1) e2) (list (list-ref ids 2) e3) (list (list-ref ids 0) (changed 1)))))
  (let* ((args (import-args st edit "R10"))
         (barriers (car (crash-at 3 root home cli args)))
         (on-one (let ((targets (map (lambda (e) (cadr (cdr e))) (plan-entries st "R10"))))
                   (exists (lambda (t) (> (length (filter (lambda (u) (equal? u t)) targets)) 1)) targets))))
    (printf "observe K10 plan entries ~s\n" (plan-entries st "R10"))
    (want "K10 the plan has two members on one block" on-one #t)
    (want "K10 the child stopped after the first member" barriers 3)
    (let ((retry (cli-run args)))
      (want "K10 the retry completes whole" (car retry) 'ok)
      (want "K10 the entry has its new text, last" (list (csrc st (list-ref ids 0)) (car (reverse (children st a))))
            (list (utf8->string (changed 1)) (list-ref ids 0))))))

;; K14: a NEW file of two entries, interrupted three ways, against a twin.
(define (tree-of st)
  (let ((s (state st)))
    (map (lambda (f) (list (code-field s f 'path) (map (lambda (k) (text (code-field s k 'src))) (code-children s f))))
         (list-sort (lambda (x y) (string<? (code-field s x 'path) (code-field s y 'path))) (code-files s)))))
(define (new-file-case tag)
  (let* ((c (import-case tag (list (cons "a.py" (projection-encode py #f (list (list "new" e1)))))))
         (st (car c)) (edit (cadr c)))
    (write-file! (string-append edit "/c.py") (projection-encode py #f (list (list "new" e2) (list "new" e3))))
    (write-file! (string-append edit "/a.py") (projection-encode py (header st (file-id st "a.py"))
                                                                (list (list (car (children st (file-id st "a.py"))) e1))))
    c))
(define twin
  (let* ((c (new-file-case "k14twin")) (st (car c)))
    (rpc-dispatch (car st) (list 'import-code (cadr c)) "test")
    (tree-of st)))
(for-each
  (lambda (n)
    (let* ((c (new-file-case "k14")) (st (car c)) (args (import-args st (cadr c) "R14"))
           (barriers (car (crash-at n root home cli args))))
      (want (string-append "K14 code, stopped at append " (number->string n)) barriers n)
      (let ((retry (cli-run args)))
        (want (string-append "K14 code, stopped at append " (number->string n) ": the retry completes whole") (car retry) 'ok)
        (want (string-append "K14 code, stopped at append " (number->string n) ": the same tree as the twin") (tree-of st) twin))))
  '(2 3 4))

;; The same for import-code --datum: a new library of several forms.
(define (datum-tree st)
  (let ((s (state st)))
    (map (lambda (l) (list (code-field s l 'name) (map (lambda (k) (code-field s k 'body)) (code-children s l))))
         (list-sort (lambda (x y) (string<? (format "~s" (code-field s x 'name)) (format "~s" (code-field s y 'name))))
                    (map cadr (state-datum s))))))
(define (new-library-case tag)
  (let* ((c (datum-case tag "(library (a) (export w) (import (rnrs))\n(define w 1))\n")) (st (car c)) (edit (cadr c)))
    (write-file! (string-append edit "/n.sc") (b "(library (n) (export p q) (import (rnrs))\n(define p 1)\n(define q 2))\n"))
    (write-file! (string-append edit "/a.sc")
                 (b (render-datum st (library-id st '(a)) (list (list (car (children st (library-id st '(a)))) '(define w 1))) '(w))))
    c))
(define datum-twin
  (let* ((c (new-library-case "k14dtwin")) (st (car c)))
    (rpc-dispatch (car st) (list 'import-code (cadr c) "--datum") "test")
    (datum-tree st)))
(for-each
  (lambda (n)
    (let* ((c (new-library-case "k14d")) (st (car c))
           (args (append (list "import-code" (cadr c) "--datum") (list "--req" "R14d" "--cursor" (cursor-of st) "--store" (car st))))
           (barriers (car (crash-at n root home cli args))))
      (want (string-append "K14 datum, stopped at append " (number->string n)) barriers n)
      (let ((retry (cli-run args)))
        (want (string-append "K14 datum, stopped at append " (number->string n) ": the retry completes whole") (car retry) 'ok)
        (want (string-append "K14 datum, stopped at append " (number->string n) ": the same tree as the twin") (datum-tree st) datum-twin))))
  '(2 3 4))

;; K14's data row: the marker-shaped list inside a member's VALUE is data.
;; The uninterrupted import appends the same record; the reducer gates it
;; in both stores (ledger F219).
(define (data-row-case tag)
  (let* ((c (datum-case tag "(library (a) (export x) (import (rnrs))\n(define x 1))\n")) (st (car c)) (edit (cadr c))
         (lib (library-id st '(a))))
    (write-file! (string-append edit "/a.sc")
                 (b (render-datum st lib (list (list (car (children st lib)) '(define x (quote ("#%new" 0))))) '(x))))
    c))
(define data-twin
  (let* ((c (data-row-case "k14dt")) (st (car c))
         (a (rpc-dispatch (car st) (list 'import-code (cadr c) "--datum" "--req" "RD" "--cursor" (cursor-of st)) "test")))
    (list (car a) (cddr (ev-payload (find (lambda (e) (eqv? 0 (actor-sub (ev-actor e)))) (evidence st "RD")))))))
(let* ((c (data-row-case "k14d")) (st (car c))
       (args (append (list "import-code" (cadr c) "--datum") (list "--req" "RD" "--cursor" (cursor-of st) "--store" (car st))))
       (barriers (car (crash-at 2 root home cli args)))
       (e (plan-event st "RD"))
       (retry (cli-run args))
       (member (find (lambda (x) (eqv? 0 (actor-sub (ev-actor x)))) (evidence st "RD"))))
  (want "K14 data row: the child stopped after the plan" barriers 2)
  (want "K14 data row: the uninterrupted import answers ok" (car data-twin) 'ok)
  (want "K14 data row: the retry appends the same record, the list unchanged" (cddr (ev-payload member)) (cadr data-twin))
  (want "K14 data row: a batch of the member's ok and unknown, not applied"
        (list (car retry) (map car (answers retry)) (head2 (last-answer retry)) (clause 'not-applied (last-answer retry)))
        (list 'batch '(ok error) '(error unknown) (list 'not-applied (ev-event member))))
  (want "K14 data row: present is empty, of 1, done 1"
        (list (clause 'completion (last-answer retry)) (clause 'done retry))
        (list (list 'completion (list 'plan e) '(present) '(of 1)) '(done 1))))

;; K12 (limit 1, pinned as it is): the parent and the sibling a member
;; names are not judged.
(define (k12-case tag)
  ;; a.py with A, and p.py with one entry: P is its file block. (An empty
  ;; p.py is not an empty parent: its import makes one child, so an edit
  ;; that lists none deletes it and is refused.)
  (let* ((c (import-case tag (list (cons "a.py" (projection-encode py #f (list (list "new" e1) (list "new" e2))))
                                   (cons "p.py" (projection-encode py #f (list (list "new" e4)))))))
         (st (car c)) (edit (cadr c)) (a (file-id st "a.py")) (P (file-id st "p.py")) (ids (children st a)))
    (list st edit a P ids)))
(for-each
  (lambda (label other! expect)
    (let* ((k (k12-case "k12")) (st (car k)) (edit (cadr k)) (a (caddr k)) (P (cadddr k)) (ids (list-ref k 4)))
      ;; a set of A in a.py; an insertion into the empty P of p.py
      (write-file! (string-append edit "/a.py") (projection-encode py (header st a) (list (list (car ids) (changed 1)) (list (cadr ids) e2))))
      (write-file! (string-append edit "/p.py") (projection-encode py (header st P) (list (list (car (children st P)) e4) (list "new" e3))))
      (let* ((args (import-args st edit "R12")) (barriers (car (crash-at 2 root home cli args))))
        (printf "observe K12 ~a: P ~s, plan entries ~s\n" label P (plan-entries st "R12"))
        (other! st P)
        (let ((retry (cli-run args)))
          (printf "observe K12 ~a: retry ~s\n" label retry)
          (want (string-append "K12 " label) (expect retry P) #t)))))
  ;; THE PLAN IS a set of A, a move of P's existing child (the import
  ;; restates its order) and the insert under P: a projection cannot make
  ;; an empty parent. So with P deleted the set and the move are written
  ;; (a move under a deleted parent is, F-n) and the insert answers
  ;; deleted: done 2.
  '("another inserts a child under P: the retry completes whole"
    "another deletes P: the retry writes the members before the insert and answers deleted P, done 2")
  (list (lambda (st P) (call st 'insert "--title" "theirs" "--under" P))
        (lambda (st P) (call st 'del P)))
  (list (lambda (retry P) (eq? (car retry) 'ok))
        (lambda (retry P) (equal? (list (car retry) (map car (answers retry)) (last-answer retry) (clause 'done retry))
                                  (list 'batch '(ok ok error) (list 'error 'deleted P) '(done 2))))))
;; A MOVE UNDER A DELETED PARENT IS WRITTEN. An import cannot move an
;; entry into another file (that is refused as foreign-file), so the moves
;; here reorder a.py's entries, and another request deletes a.py's file
;; block, the parent the moves name.
(let* ((k (k12-case "k12m")) (st (car k)) (edit (cadr k)) (a (caddr k)) (ids (list-ref k 4)))
  (write-file! (string-append edit "/a.py") (projection-encode py (header st a) (list (list (cadr ids) e2) (list (car ids) e1))))
  (let* ((args (import-args st edit "R12m")) (barriers (car (crash-at 2 root home cli args))))
    (printf "observe K12 move: plan entries ~s\n" (plan-entries st "R12m"))
    (call st 'del a)
    (let ((retry (cli-run args)))
      (printf "observe K12 move: retry ~s\n" retry)
      (want "K12 a move under a deleted parent is written, as on the base" (car retry) 'ok))))
;; ---- forged plans: K14's fabricated plans, K19 ------------------------------------
;;
;; A PLAN NO PRODUCER WRITES, published under the identity a later
;; `working-commit!` computes, so the retry takes the completion path
;; (as w4-plan-crash's mismatch-store does). Its premises are the store's
;; applied cut, as a real plan's are.
(define (forged-store tag)
  (let* ((st (fresh-store tag))
         (M (new-block st "M" "old")))
    (call st 'write M "the text")
    (let* ((info (assq 'projection (cdr (call st 'read M "--working-info"))))
           (based-on (list-ref info 5)) (cut (list-ref info 6))
           (real (draft-version (string->utf8 "the text") based-on cut))
           (after (or (assoc (cdr st) (reduce-applied-cut (state st))) (cons (cdr st) 0))))
      (list st M based-on cut real after))))
(define (publish-plan! f entries consumes)
  (let* ((st (list-ref f 0)) (M (list-ref f 1)) (real (list-ref f 4)) (after (list-ref f 5)) (w (cdr st))
         (args (cons w (list (string-append M "\x0;" real))))
         (fp (request-fingerprint "test" 'commit args after))
         (plan (append (list 'plan "MM" fp after entries) (if consumes (list consumes) '())))
         (actor (list "test" (cons w "MM") 'plan fp #f after))
         (frame (encode-record 1 1789000000005 actor (reduce-applied-cut (state st)) (storable-encode plan))))
    (log-publish! (car st) "forged00" 1 frame (segment-sha frame))
    (cons "forged00" 1)))
(define (forged-retry f)
  (let ((st (list-ref f 0)))
    (working-commit! (car st) (cdr st) (list (list-ref f 1)) "test"
                     (make-write-request "test" 'commit (list (list-ref f 1)) "MM" (list-ref f 5))
                     (list (list-ref f 4)))))
(define section '((kind . section) (title . "N")))
(for-each
  (lambda (label entries expected other?)
    (let* ((f (forged-store "k14f")) (st (car f)) (M (cadr f)))
      (publish-plan! f (entries M) #f)
      (when other? (call st 'set M "src" "theirs"))
      (let* ((before (log-bytes st)) (a (forged-retry f)))
        (want (string-append "K14 fabricated: " label) (list (car a) (cadr a) (caddr a)) (list 'error 'no-such-intent expected))
        (want (string-append "K14 fabricated: " label ", and nothing is written") (log-bytes st) before))))
  '("a marker naming a set" "a marker naming no index" "a marker naming a later member" "a marker naming a set, the set's block written by another (C9)")
  (list (lambda (M) (list (cons 0 (list 'set M 'src "x")) (cons 1 (list 'insert '("#%new" 0) #f section))))
        (lambda (M) (list (cons 0 (list 'insert 'root #f section)) (cons 1 (list 'insert '("#%new" 5) #f section))))
        (lambda (M) (list (cons 0 (list 'insert '("#%new" 1) #f section)) (cons 1 (list 'insert 'root #f section))))
        (lambda (M) (list (cons 0 (list 'set M 'src "x")) (cons 1 (list 'insert '("#%new" 0) #f section)))))
  '(0 5 1 0)
  '(#f #f #f #t))

;; K12's sibling case. AN IMPORT CANNOT NAME A SIBLING WITHOUT TARGETING
;; IT: it restates every entry's order with a move, so the sibling is a
;; target, and its deletion by another request is a foreign record that
;; makes the completion stale (measured: the import's plan for "a new
;; entry after S" carries a move of S). The limit is about a member that
;; names S only as its sibling, so the plan is forged: a set, then an
;; insert after S.
(let* ((f (forged-store "k12s")) (st (car f)) (M (cadr f)) (S (new-block st "S" "s")))
  (set-car! (list-tail f 5) (or (assoc (cdr st) (reduce-applied-cut (state st))) (cons (cdr st) 0)))
  (publish-plan! f (list (cons 0 (list 'set M 'src "the text")) (cons 1 (list 'insert 'root S section))) #f)
  (call st 'del S)
  (let ((a (forged-retry f)))
    (want "K12 a member to follow a deleted sibling: the members before it, then unknown-sibling"
          (list (car a) (map car (answers a)) (last-answer a) (clause 'done a))
          (list 'batch '(ok error) (list 'error 'unknown-sibling S) '(done 1)))))

;; K19: the hash check, alone. The consumes item names a based-on that is
;; not the block's hash, and its version is computed from that based-on,
;; so consumes-mismatch passes; nobody else writes.
(let* ((f (forged-store "k19")) (st (car f)) (M (cadr f)) (cut (list-ref f 3))
       (fake (make-string 64 #\0))
       (named (draft-version (string->utf8 "the text") fake cut))
       (e (publish-plan! f (list (cons 0 (list 'set M 'src "the text"))) (list 'consumes (cdr st) (list (list M named fake cut))))))
  (let* ((before (log-bytes st)) (a (forged-retry f)))
    (want "K19 the retry answers stale-baseline" (head2 a) '(error stale-baseline))
    (want "K19 with an empty since and the reason"
          (list (clause 'block a) (clause 'based-on a) (clause 'since a) (clause 'reason a))
          (list (list 'block M) (list 'based-on fake) '(since) '(reason candidate-set-changed)))
    (want "K19 and the completion clause" (clause 'completion a) (list 'completion (list 'plan e) '(present) '(of 1)))
    (want "K19 and writes nothing" (log-bytes st) before)
    (want "K19 the block keeps its text" (src st M) "old")))

;; ---- mirrored records: K15, K16, K18, K20 -----------------------------------------
;;
;; A RECORD OF ANOTHER WRITER, on disk before its premise: written as
;; test/q1.sc and test/q8.sc write them, into that writer's segment.
(define (publish! st writer recs)
  ;; recs: (seq deps payload actor) ...
  (let ((bytes (string->utf8 (apply string-append
                 (map (lambda (r) (utf8->string (encode-record (car r) (+ 1789000000000 (car r)) (list-ref r 3) (cadr r)
                                                               (storable-encode (caddr r)))))
                      recs)))))
    (log-publish! (car st) writer 1 bytes (segment-sha bytes))))
(define (two st)
  (let ((A (new-block st "A" "old A")) (B (new-block st "B" "old B")))
    (call st 'write A "new A") (call st 'write B "new B")
    (list A B)))
(define (k15 tag arrange)
  (let* ((st (fresh-store tag)) (ids (two st)))
    (let-values (((barriers args) (crash-commit! st ids "R15" 2)))
      (let* ((e (plan-event st "R15")) (X (member-block st "R15" 0)) (Y (member-block st "R15" 1))
             (next (cons (cdr st) (+ (last-seq st) 1)))
             (mirrored (arrange st Y next))
             (retry (cli-run args)))
        (list st e X Y mirrored retry barriers ids)))))
(define (other-single who req)
  (let ((after (cons who 0)))
    (list "review" (cons who req) 'single (request-fingerprint "review" 'set '("x") after) #f after)))
(for-each
  (lambda (label arrange)
    (let* ((r (k15 "k15" arrange)) (st (list-ref r 0)) (e (list-ref r 1)) (X (list-ref r 2)) (Y (list-ref r 3))
           (mirrored (list-ref r 4)) (retry (list-ref r 5)))
      (want (string-append "K15 " label ": the child stopped after the plan") (list-ref r 6) 2)
      (want (string-append "K15 " label ": a batch of member 0's ok and stale-baseline for B")
            (list (car retry) (map car (answers retry)) (head2 (last-answer retry)) (clause 'block (last-answer retry)))
            (list 'batch '(ok error) '(error stale-baseline) (list 'block Y)))
      (want (string-append "K15 " label ": the mirrored record in since") (car (since-events (last-answer retry))) mirrored)
      (want (string-append "K15 " label ": present is member 0")
            (clause 'completion (last-answer retry)) (list 'completion (list 'plan e) '(present 0) '(of 2)))
      (want (string-append "K15 " label ": B reads q and A the commit's text") (list (src st Y) (src st X))
            (list "q" (if (equal? X (car (list-ref r 7))) "new A" "new B")))))
  '("untracked" "tracked" "transitive")
  (list (lambda (st Y next) (publish! st "mirrorab" (list (list 1 (list next) (list 'set Y 'src "q") "peer"))) (cons "mirrorab" 1))
        (lambda (st Y next) (publish! st "mirrorab" (list (list 1 (list next) (list 'set Y 'src "q") (other-single "mirrorab" "other")))) (cons "mirrorab" 1))
        (lambda (st Y next)
          (publish! st "mirrorcd" (list (list 1 (list next) '(put ((kind . section) (title . "U") (parent . root) (ord . 9))) "peer")))
          (publish! st "mirrorab" (list (list 1 (list (cons "mirrorcd" 1)) (list 'set Y 'src "q") (other-single "mirrorab" "other"))))
          (cons "mirrorab" 1))))

;; K16 (control): delivered unapplied records this completion does not
;; release do not hold it.
(let* ((st (fresh-store "k16")) (ids (two st)))
  (let-values (((barriers args) (crash-commit! st ids "R16" 2)))
    (let ((Y (member-block st "R16" 1)))
      ;; waits for a THIRD writer's record, which the completion does not write
      (publish! st "mirrorab" (list (list 1 (list (cons "thirdzzz" 1)) (list 'set Y 'src "q") "peer")))
      (let ((retry (cli-run args)))
        (want "K16 a record waiting on a third writer does not hold the retry" (list (car retry) (subs st "R16")) '(ok (plan 0 1))))
      ;; the third writer's record arrives, concurrent with the completion
      (publish! st "thirdzzz" (list (list 1 '() '(put ((kind . section) (title . "Third") (parent . root) (ord . 7))) "peer")))
      (want "K16 the mirrored set is then applied beside the commit: B has two candidates"
            (let ((v (src st Y))) (and (pair? v) (eq? (car v) 'conflict) (length (cadr v))))
            2))))
(let* ((st (fresh-store "k16b")) (ids (two st)))
  (let-values (((barriers args) (crash-commit! st ids "R16" 2)))
    (let* ((Y (member-block st "R16" 1))
           (after (cons "forgezzz" 0))
           (fp (request-fingerprint "review" 'set '("x") after))
           (actor (lambda (sub pe) (list "review" (cons "forgezzz" "other") sub fp pe after))))
      ;; another plan, and a member of it that is not what that plan
      ;; declares: plan-mismatch, gated
      (publish! st "forgezzz" (list (list 1 '() (list 'plan "other" fp after (list (cons 0 (list 'set Y 'src "declared")))) (actor 'plan #f))
                                    (list 2 '() (list 'set Y 'src "different") (actor 0 (cons "forgezzz" 1)))))
      (let ((retry (cli-run args)))
        (want "K16 a gated forged member of another plan does not hold the retry" (list (car retry) (subs st "R16")) '(ok (plan 0 1)))))))

;; K18, K20: a premise taken back by what the write released.
(define (contest tag tag-before-plan?)
  (let* ((st (fresh-store tag)) (ids (two st)))
    (let* ((tag-ev (and tag-before-plan? (event-of (call st 'insert "--title" "T" "--req" "R0" "--cursor" (cursor-of st))))))
      (let-values (((barriers args) (crash-commit! st ids "R18" 2)))
        (let ((tag-ev (or tag-ev (event-of (call st 'insert "--title" "T" "--req" "R0" "--cursor" (cursor-of st))))))
          (let* ((e (plan-event st "R18"))
                 (t (find (lambda (x) (eq? 'single (actor-sub (ev-actor x)))) (evidence st "R0")))
                 (next (cons (cdr st) (+ (last-seq st) 1))))
            ;; a mirrored record claiming the same single identity, whose
            ;; premise is the completing writer's next record
            (publish! st "rivalzzz" (list (list 1 (list next) (ev-payload t) (ev-actor t))))
            (let* ((n0 (record-count st)) (retry (cli-run args)))
              (list st e retry (- (record-count st) n0) barriers))))))))
(let* ((r (contest "k18" #t)) (st (car r)) (e (cadr r)) (retry (caddr r)))
  (want "K18 the completion stops with unknown, the plan not applied"
        (list (head2 (last-answer retry)) (clause 'not-applied (last-answer retry)))
        (list '(error unknown) (list 'not-applied e)))
  (want "K18 and its completion clause" (clause 'completion (last-answer retry)) (list 'completion (list 'plan e) '(present) '(of 2)))
  (want "K18 nothing is appended after member 0" (list-ref r 3) 1))
(let* ((r (contest "k20" #f)) (st (car r)) (e (cadr r)) (retry (caddr r)))
  (want "K20 a batch of member 0's ok and unknown for member 0's own record"
        (list (car retry) (map car (answers retry)) (head2 (last-answer retry)))
        (list 'batch '(ok error) '(error unknown)))
  (want "K20 naming member 0's event, and nothing applied"
        (list (clause 'not-applied (last-answer retry)) (clause 'completion (last-answer retry)) (clause 'done retry))
        (list (list 'not-applied (event-of (car (answers retry)))) (list 'completion (list 'plan e) '(present) '(of 2)) '(done 1)))
  (want "K20 one record appended" (list-ref r 3) 1))

;; ---- K17: baseline-refusal answers what it answered before -------------------------
;;
;; THE BASE'S PROCEDURE, copied whole as it stood before the split, is the
;; reference; the split procedure must answer the same on stale commits,
;; on a block with more than eight records, and on an empty since.
(define (reference-refusal state id wanted . rest)
  (define (eligible? r id cut)
    (let ((seen (assoc (car r) cut)))
      (and (let ((p (list-ref r 3)))
             (and (pair? p)
                  (case (car p)
                    ((put) (equal? id (block-id (car r) (cadr r))))
                    ((set move del link unlink) (and (pair? (cdr p)) (equal? id (cadr p))))
                    (else #f))))
           (or (not seen) (> (cadr r) (cdr seen))))))
  (define (weight r) (let ((c (state-event-cut state (cons (car r) (cadr r))))) (if c (apply + (map cdr c)) 0)))
  (define (later? a b)
    (let ((ra (list (weight a) (car a) (cadr a))) (rb (list (weight b) (car b) (cadr b))))
      (cond ((not (= (car ra) (car rb))) (> (car ra) (car rb)))
            ((not (string=? (cadr ra) (cadr rb))) (string>? (cadr ra) (cadr rb)))
            (else (> (caddr ra) (caddr rb))))))
  (define (entry-of r)
    (let* ((content (list-ref r 3))
           (body (and (pair? content) (eq? 'set (car content)) (= 4 (length content)) (eq? 'src (caddr content))
                      (string? (cadddr content)) (cadddr content)))
           (n (if body (bytevector-length (string->utf8 body))
                  (bytevector-length (string->utf8 (sexpr->string-extended (storable-encode content)))))))
      (list (cons (car r) (cadr r)) (list-ref r 4) (if (> n 1024) (list 'content 'elided n) content))))
  (let* ((now (block-hash state id)) (cut (if (pair? rest) (car rest) '()))
         (history (cadr (assq 'request-history (state->rows state)))))
    (and (not (equal? wanted now))
         (let* ((all (filter (lambda (r) (eligible? r id cut)) history))
                (ranked (list-sort later? all))
                (kept (if (> (length ranked) 8) (list-head ranked 8) ranked))
                (omitted? (> (length ranked) (length kept)))
                (ordered (if (null? kept) '() (cons (car kept) (list-sort (lambda (a b) (later? b a)) (cdr kept))))))
           (append (list 'error 'stale-baseline (list 'block id) (list 'based-on wanted) (list 'now now)
                         (cons 'since (map entry-of ordered)))
                   (if (null? ordered) (list '(reason candidate-set-changed) (list 'conflicts id)) '())
                   (if omitted? (list '(truncated #t) (list 'retrieve (list 'log id) (list 'read id))) '()))))))
(let* ((st (fresh-store "k17")) (A (new-block st "A" "a0")) (h0 (block-hash (state st) A))
       (cut0 (reduce-applied-cut (state st))))
  (let loop ((k 1))
    (when (<= k 10)
      (call st 'set A "src" (string-append "a" (number->string k)))
      (let ((s (state st)))
        (want (string-append "K17 after " (number->string k) " sets, the split answers as the base")
              (baseline-refusal s A h0 cut0) (reference-refusal s A h0 cut0)))
      (loop (+ k 1))))
  (let ((s (state st)))
    (want "K17 the hash equal: silent, as the base" (list (baseline-refusal s A (block-hash s A) cut0) (reference-refusal s A (block-hash s A) cut0)) '(#f #f))
    (want "K17 no record outside the cut: candidate-set-changed, as the base"
          (baseline-refusal s A "not-a-hash" (reduce-applied-cut s)) (reference-refusal s A "not-a-hash" (reduce-applied-cut s)))
    (want "K17 a long body is elided, as the base"
          (begin (call st 'set A "src" (make-string 2000 #\x))
                 (let ((s (state st))) (equal? (baseline-refusal s A h0 cut0) (reference-refusal s A h0 cut0))))
          #t)))

(printf "rows: ~a\n~a failures\ncompletion-stale complete\n" rows bad)
