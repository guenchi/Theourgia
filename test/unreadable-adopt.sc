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

;; Adopt, and its continuation, read everything they depend on before they
;; write anything; and a read that failed is not damage.
;;
;; Adopt is the way out of a damaged writer: it retires the writer at the
;; end of its readable prefix and starts a new one. A writer whose files
;; this process merely cannot read is not damaged -- its history is intact
;; and returns with the permission -- so retiring it throws away records
;; for good. And a refusal that comes after the first write has already
;; changed the store it refused.
;;
;; "No write" is read from the trace: every write, rename, link, unlink,
;; truncate, create and registry write the call made. Equal bytes
;; afterwards would not do; a file written back identically passes that.
;; The detector is shown to see the writes of an adopt that does run
;; before any row relies on it seeing none.

(import (chezscheme) (theourgia log) (theourgia store) (theourgia trace)
        (only (theourgia reduce) reduce-trace))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (with-expected label expected (x) (want-1 label (caught got) x))))))

;; The typed condition is looked up at run time, so this file loads on a
;; tree without it; there, the rows that need it are red and the rest read.
(define (lookup name) (guard (e (#t #f)) (eval name (environment '(theourgia log)))))
(define (lookup-in name lib) (guard (e (#t #f)) (eval name (environment lib))))
(define unreadable-entry? (lookup 'unreadable-entry?))
(define unreadable-entry-path (lookup 'unreadable-entry-path))

;; THE SCRATCH ROOT FOLLOWS THEOURGIA_TEST_ROOT, and a directory already
;; there is refused rather than reused (F71).
(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/unreadable-adopt-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'unreadable-adopt "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

;; EACH STORE REMEMBERS ITS OWN MACHINE HOME. The instance nonce lives in
;; the home, so switching homes is switching machines; a row that means to
;; do that says so with `use-home!`.
(define n 0)
(define homes '())
(define (use-store! d) (putenv "THEOURGIA_HOME" (cdr (assoc d homes))))
(define (use-home! h) (putenv "THEOURGIA_HOME" h))
(define (fresh-home!)
  (set! n (+ n 1))
  (let ((h (string-append root "/home" (number->string n))))
    (system (string-append "mkdir -p " h))
    h))
(define (fresh-store! titles)
  (let* ((h (fresh-home!))
         (d (string-append root "/s" (number->string n))))
    (system (string-append "mkdir -p " d))
    (use-home! h)
    (set! homes (cons (cons d h) homes))
    (store-init! d)
    (for-each (lambda (t)
                (with-store-write d
                  (lambda (st v) (list (list 'insert 'root #f
                                             (list (cons 'kind 'section) (cons 'title t)))))
                  "t"))
              titles)
    d))
(define (writer-of d) (car (list-sort string<? (store-writers d))))
(define (wfile d w f) (string-append d "/writers/" w "/" f))
(define (chmod! mode path) (system (string-append "chmod " mode " " path)))
(define (slurp p)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
      (if (eof-object? b) "" (utf8->string b)))))
(define (spit! p text)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 text)))))
(define (append-line! p text) (spit! p (string-append (slurp p) text)))
(define (records-of d) (length (reduce-trace (open-and-reduce d))))
(define (origin-and-kinds d w)
  (let ((p (discover-prefix d w #f)))
    (list (discovery-origin p) (map log-error-kind (discovery-integrity p)))))
(define (notes-name? d w file)
  (let ((p (discover-prefix d w #f)))
    (exists (lambda (e)
              (let ((a (assq 'path (log-error-detail e))))
                (and a (string? (cdr a)) (ends-with? (cdr a) file))))
            (discovery-integrity p))))
(define (ends-with? s tail)
  (and (>= (string-length s) (string-length tail))
       (string=? tail (substring s (- (string-length s) (string-length tail))
                                 (string-length s)))))

;; ---- the trace ------------------------------------------------------------

(define mutating '(write rename link unlink ftruncate create registry-write))
;; A TRACE LINE IS WRITTEN WITH DISPLAY, so a path reads back as a symbol.
(define (subject-path s)
  (let ((x (if (pair? s) (car s) s)))
    (if (symbol? x) (symbol->string x) x)))
;; `(answer events)`: the call's answer (or its raise, as a list headed
;; RAISED) and every trace line it produced, read back as data.
(define (traced d thunk) (append (traced-1 thunk) (list d)))
(define (traced-1 thunk)
  (let-values (((port get) (open-string-output-port)))
    (trace-enable! #t)
    (let ((r (guard (e (#t (list 'RAISED
                                 (cond ((and unreadable-entry? (unreadable-entry? e))
                                        (list 'unreadable-entry (unreadable-entry-path e)))
                                       ((and (condition? e) (message-condition? e))
                                        (condition-message e))
                                       (else e)))))
               (parameterize ((current-error-port port)) (thunk)))))
      (trace-enable! #f)
      (list r (let ((in (open-string-input-port (get))))
                (let loop ((acc '()))
                  (let ((x (guard (e (#t (eof-object))) (read in))))
                    (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))))
(define (answer-of t) (car t))
;; ONLY WRITES TO THE STORE AND TO THE MACHINE REGISTRY COUNT. A row that
;; switches to a fresh machine home makes that home's identity on first use,
;; which is a write the store under test has no part in; and a lock file is
;; created to be locked, not to hold anything.
(define (watched? store path)
  (and (string? path)
       (not (ends-with? path "/lock"))
       (or (and (> (string-length path) (string-length store))
                (string=? (string-append store "/")
                          (substring path 0 (+ 1 (string-length store)))))
           (string=? (strip-temp path) (registry-path)))))
(define (strip-temp p)
  (let loop ((i 0))
    (cond ((> (+ i 5) (string-length p)) p)
          ((string=? (substring p i (+ i 5)) ".tmp-") (substring p 0 i))
          (else (loop (+ i 1))))))
(define (writes-of t)
  (filter (lambda (e) (and (list? e) (= 4 (length e)) (eq? (car e) 'trace)
                           (memq (cadr e) mutating)
                           (watched? (caddr t) (subject-path (caddr e)))))
          (cadr t)))
;; What a write touched, by file name, so a failing row says which file.
(define (written-names t)
  (map (lambda (e)
         (let ((p (let ((x (subject-path (caddr e)))) (if (string? x) (strip-temp x) x))))
           (list (cadr e)
                 (if (string? p)
                     (let loop ((i (- (string-length p) 1)))
                       (cond ((< i 0) p)
                             ((char=? (string-ref p i) #\/) (substring p (+ i 1) (string-length p)))
                             (else (loop (- i 1)))))
                     p))))
       (writes-of t)))
(define (saw? t op) (exists (lambda (e) (and (list? e) (> (length e) 1) (eq? (cadr e) op))) (cadr t)))

;; A REFUSAL, FOR CONTINUE-ADOPT!, IS EITHER SHAPE: an answer headed
;; `refused`, or the typed condition raised. It answers bare symbols to an
;; internal caller and has no refusal family of its own to extend. ADOPT
;; does have one -- (refused not-needed ...) -- so its rows pin the kind,
;; and a raise does not satisfy them. Neither the assertion-violation these
;; paths raise today nor an answer that went ahead counts for either.
(define (refusal-of a)
  (cond
    ((and (pair? a) (eq? (car a) 'refused) (pair? (cdr a))) (list 'refused (cadr a)))
    ((and (pair? a) (eq? (car a) 'RAISED) (pair? (cadr a)) (eq? (car (cadr a)) 'unreadable-entry))
     (list 'refused 'raised-unreadable-entry))
    ((and (pair? a) (eq? (car a) 'RAISED)) (list 'raised (cadr a)))
    ((pair? a) (list 'answered (car a) (and (pair? (cdr a)) (cadr a))))
    (else (list 'answered a))))
(define (refusal-path a)
  (cond
    ((and (pair? a) (eq? (car a) 'refused))
     (let ((p (assq 'path (cddr a)))) (and p (cadr p))))
    ((and (pair? a) (eq? (car a) 'RAISED) (pair? (cadr a)) (eq? (car (cadr a)) 'unreadable-entry))
     (cadr (cadr a)))
    (else #f)))

;; ---- the registry and a continuation to resume -----------------------------

(define (registry-now) (read (open-string-input-port (slurp (registry-path)))))
(define (registry-put! reg) (spit! (registry-path) (format "~s\n" reg)))
;; `written` ahead of the log: the rollback adopt resolves by recording the
;; lost stretch as uncertain.
(define (raise-written! w to)
  (registry-put!
    (map (lambda (e)
           (if (and (list? e) (= 6 (length e)) (equal? (caddr e) w))
               (list (car e) (cadr e) (caddr e) to (list-ref e 4) to)
               e))
         (registry-now))))
;; The five-element water mark, which read-registry upgrades by WRITING the
;; registry back.
(define (registry-to-legacy!)
  (registry-put!
    (map (lambda (e)
           (if (and (list? e) (= 6 (length e)) (string? (car e))) (list-head e 5) e))
         (registry-now))))
(define (legacy-registry?)
  (exists (lambda (e) (and (list? e) (= 5 (length e)) (string? (car e)))) (registry-now)))
(define (set-gens-pending!)
  (registry-put!
    (map (lambda (e)
           (if (and (list? e) (eq? (car e) 'gen)) (append (list-head e 8) (list 'pending)) e))
         (registry-now))))
;; A retirement record that does not carry the stretch the registry still
;; vouches for: resume-uncertain! must add it back, which is a write.
(define (drop-retired-uncertain! d w)
  (let* ((p (wfile d w "retired.sexp"))
         (x (read (open-string-input-port (slurp p)))))
    (spit! p (format "~s\n" (map (lambda (c) (if (and (pair? c) (eq? (car c) 'uncertain))
                                                 (list 'uncertain '())
                                                 c))
                                 x)))))
;; A store adopted out of a rollback, left the way a crash between the
;; retirement and the successor's owner leaves it: generation pending, the
;; successor without owner.sexp, and the retirement without its stretch.
;; Answers (store old new).
(define (interrupted-adopt!)
  (let* ((d (fresh-store! '("One" "Two")))
         (old (writer-of d)))
    (raise-written! old 9)
    (let ((a (store-adopt! d)))
      (unless (and (pair? a) (eq? (car a) 'adopted))
        (assertion-violation 'interrupted-adopt! "the setup adopt did not run" a))
      (let ((new (cadr (assq 'to (cdr a)))))
        (delete-file (wfile d new "owner.sexp"))
        (set-gens-pending!)
        (drop-retired-uncertain! d old)
        (list d old new)))))
;; A pending generation whose transaction never started: its old writer is
;; not retired under it and its new writer has no owner, so continue-adopt!
;; cancels it -- a registry write. Put FIRST, so a continuation that
;; handled generations one at a time would write before reaching the next.
(define (prepend-cancellable-generation! d old-writer)
  (let* ((reg (registry-now))
         (g (find (lambda (e) (and (list? e) (eq? (car e) 'gen))) reg)))
    (registry-put!
      (cons (list 'gen (list-ref g 1) (list-ref g 2) "tx-cancellable-0001"
                  old-writer "zzcancel" #f #f 'pending)
            reg))))

;; ============================================================================
;; NEVER: A ROW'S READINGS ARE BOUND WITH let*, NOT let. Chez evaluates a
;; let's bindings in no promised order, and the first draft of this file
;; read K13's precondition AFTER the adopt it was the precondition of.

(printf "== the detector sees an adopt's writes ==\n")
(let* ((d (fresh-store! '("One" "Two")))
       (w (writer-of d)))
  (append-line! (wfile d w "000001.sexp")
                "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
  (let ((t (traced d (lambda () (adopt! d)))))
    (want "CONTROL an adopt across a damaged record runs, and the trace shows it writing retired.sexp"
          (list (car (answer-of t))
                (and (member '(rename "retired.sexp") (written-names t)) #t))
          '(adopted #t))))
(let ((d (fresh-store! '("One"))))
  (let ((t (traced d (lambda () (adopt! d)))))
    (want "CONTROL an adopt with nothing to do writes nothing, so the detector is not always red"
          (list (refusal-of (answer-of t)) (written-names t))
          '((refused not-needed) ()))))

(printf "== K13: unreadable is not damaged ==\n")
(let* ((d (fresh-store! '("One" "Two")))
       (w (writer-of d))
       (seg (wfile d w "000001.sexp")))
  (chmod! "000" seg)
  (let* ((pre (caught (origin-and-kinds d w)))
        (t (traced d (lambda () (adopt! d))))
        (retired? (file-exists? (wfile d w "retired.sexp"))))
    (chmod! "644" seg)
    (want "K13 precondition: discovery notes the unreadable segment"
          (and (pair? pre) (pair? (cdr pre)) (and (memq 'segment-unreadable (cadr pre)) #t))
          #t)
    (want "K13 adopt refuses a writer whose segment cannot be read, naming it"
          (list (refusal-of (answer-of t)) (refusal-path (answer-of t)))
          (list '(refused segment-unreadable) seg))
    (want "K13 and it wrote nothing: no retirement, no new writer"
          (list (written-names t) retired? (length (store-writers d)))
          '(() #f 1))
    (want "K13 TWIN with the permission back the history is whole and adopt has nothing to do"
          (list (records-of d) (refusal-of (adopt! d)))
          '(2 (refused not-needed)))))

(printf "== U8: owner.sexp cannot be read ==\n")
(let* ((d (fresh-store! '("One")))
       (w (writer-of d))
       (owner (wfile d w "owner.sexp")))
  (chmod! "000" owner)
  (let ((t (traced d (lambda () (adopt! d)))))
    (chmod! "600" owner)
    (want "U8 adopt stops with verify-instance's refusal, owner-unreadable"
          (refusal-of (answer-of t))
          '(refused owner-unreadable))
    (want "U8 and adopt-locked! changed nothing"
          (list (written-names t) (length (store-writers d)))
          '(() 1))))

(printf "== U8b: a readable owner beside an unreadable quarantine.sexp ==\n")
(let* ((d (fresh-store! '("One" "Two")))
       (w (writer-of d))
       (q (wfile d w "quarantine.sexp")))
  (spit! q "((format 1) (fork 99))\n")
  (want "CONTROL with quarantine.sexp readable (fork past the end) the writer reads as it did"
        (origin-and-kinds d w)
        '(local ()))
  (chmod! "000" q)
  (let* ((pre (caught (list (car (origin-and-kinds d w)) (notes-name? d w "quarantine.sexp"))))
        (t (traced d (lambda () (adopt! d)))))
    (chmod! "600" q)
    (want "U8b precondition: discovery answers unreadable, naming quarantine.sexp"
          pre
          '(unreadable #t))
    (want "U8b adopt refuses with that kind and path, not not-needed and not an adopt"
          (list (refusal-of (answer-of t)) (refusal-path (answer-of t)))
          (list '(refused metadata-unreadable) q))
    (want "U8b and wrote nothing -- no retirement, no rollback marker"
          (written-names t)
          '())))

(printf "== U8c: another machine's identity, and a read that fails ==\n")
;; CONTROL: identity mismatch alone is adopted -- the path does write, so
;; the two refusals below are about the unreadable file.
(let* ((d (fresh-store! '("One")))
       (w (writer-of d)))
  (use-home! (fresh-home!))
  (let ((t (traced d (lambda () (adopt! d)))))
    (want "CONTROL identity mismatch with everything readable is adopted, and installs an instance"
          (list (car (answer-of t))
                (and (exists (lambda (x) (equal? (cadr x) "instance.sexp")) (written-names t)) #t))
          '(adopted #t))))
(let* ((d (fresh-store! '("One")))
       (w (writer-of d))
       (q (wfile d w "quarantine.sexp")))
  (spit! q "((format 1) (fork 99))\n")
  (chmod! "000" q)
  (use-home! (fresh-home!))
  (let ((t (traced d (lambda () (adopt! d)))))
    (chmod! "600" q)
    (want "U8c identity mismatch with quarantine.sexp unreadable: refused, naming it"
          (list (refusal-of (answer-of t)) (refusal-path (answer-of t)))
          (list '(refused metadata-unreadable) q))
    (want "U8c and no instance-install! write, nor any other"
          (written-names t)
          '())))
(let* ((d (fresh-store! '("One")))
       (w (writer-of d))
       (u (wfile d w "uncertain.sexp")))
  (spit! u "()\n")
  (chmod! "000" u)
  (use-home! (fresh-home!))
  (let* ((pre (caught (origin-and-kinds d w)))
        (t (traced d (lambda () (adopt! d)))))
    (chmod! "600" u)
    (want "U8c precondition: with only uncertain.sexp unreadable, discovery is readable"
          (and (pair? pre) (car pre))
          'local)
    (want "U8c identity mismatch with only uncertain.sexp unreadable: refused, naming uncertain.sexp"
          (list (car (refusal-of (answer-of t))) (refusal-path (answer-of t)))
          (list 'refused u))
    (want "U8c and no instance-install! write, nor any other"
          (written-names t)
          '())))

(printf "== U7a: a readable discovery, an unreadable uncertainty cache ==\n")
(let* ((d (fresh-store! '("One" "Two")))
       (w (writer-of d))
       (u (wfile d w "uncertain.sexp")))
  (append-line! (wfile d w "000001.sexp")
                "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
  (spit! u "()\n")
  (chmod! "000" u)
  (let ((t (traced d (lambda () (adopt! d)))))
    (chmod! "600" u)
    (want "U7a adopt of a damaged writer reaches the cache, finds it unreadable, and refuses naming it"
          (list (car (refusal-of (answer-of t))) (refusal-path (answer-of t)))
          (list 'refused u))
    (want "U7a and persists nothing"
          (written-names t)
          '())))

(printf "== U8e: continue-adopt! reads everything before it writes ==\n")
;; CONTROL: the interrupted adopt resumes when its cache can be read, and
;; resuming writes the retirement's stretch back -- the write the rows below
;; must not see.
(let* ((x (interrupted-adopt!)) (d (car x)) (old (cadr x)))
  (let ((t (traced d (lambda () (continue-adopt! d)))))
    (want "CONTROL the interrupted adopt resumes, and resuming rewrites retired.sexp"
          (list (answer-of t)
                (and (member '(rename "retired.sexp") (written-names t)) #t))
          '(resumed #t))))
(let* ((x (interrupted-adopt!)) (d (car x)) (old (cadr x))
       (u (wfile d old "uncertain.sexp")))
  (chmod! "000" u)
  (let ((t (traced d (lambda () (continue-adopt! d)))))
    (chmod! "600" u)
    (want "U8e with the cache unreadable the continuation refuses, naming uncertain.sexp"
          (list (car (refusal-of (answer-of t))) (refusal-path (answer-of t)))
          (list 'refused u))
    (want "U8e and writes neither the retirement nor the registry"
          (written-names t)
          '())))
(let* ((x (interrupted-adopt!)) (d (car x)) (old (cadr x))
       (u (wfile d old "uncertain.sexp")))
  (registry-to-legacy!)
  (let ((legacy? (legacy-registry?)))
    (chmod! "000" u)
    (let ((t (traced d (lambda () (continue-adopt! d)))))
      (chmod! "600" u)
      (want "U8e LEGACY registry: precondition, the water marks are five-element"
            legacy?
            #t)
      (want "U8e LEGACY registry: refused, with no registry upgrade write and no other"
            (list (car (refusal-of (answer-of t))) (written-names t))
            '(refused ())))))
;; CONTROL for the two-generation row: the cancellable generation alone is
;; cancelled, which writes the registry.
(let* ((x (interrupted-adopt!)) (d (car x)) (old (cadr x)) (new (caddr x)))
  (prepend-cancellable-generation! d new)
  (let ((t (traced d (lambda () (continue-adopt! d)))))
    (want "CONTROL the cancellable generation is cancelled, a registry write"
          (saw? t 'registry-write)
          #t)))
(let* ((x (interrupted-adopt!)) (d (car x)) (old (cadr x)) (new (caddr x))
       (u (wfile d old "uncertain.sexp")))
  (prepend-cancellable-generation! d new)
  (chmod! "000" u)
  (let ((t (traced d (lambda () (continue-adopt! d)))))
    (chmod! "600" u)
    (want "U8e TWO generations, the later one's cache unreadable: refused, and zero writes in the whole call"
          (list (car (refusal-of (answer-of t))) (written-names t))
          '(refused ()))))

(printf "== U8f: adopt-needed? reads the registry before discovery ==\n")
(let* ((d (fresh-store! '("One" "Two")))
       (w (writer-of d))
       (q (wfile d w "quarantine.sexp")))
  (registry-to-legacy!)
  (spit! q "((format 1) (fork 99))\n")
  (chmod! "000" q)
  (let* ((legacy? (legacy-registry?))
         (t (traced d (lambda () (adopt! d)))))
    (chmod! "600" q)
    (want "U8f precondition: the registry is legacy before the call"
          legacy?
          #t)
    (want "U8f matching identity, legacy registry, unreadable quarantine: refused, no registry upgrade write"
          (list (car (refusal-of (answer-of t))) (written-names t))
          '(refused ()))))

;; ---- the whole-store inventory: a mirror's files are read too -------------
;; ADDED BY THE CODE SESSION (F77b review 1, the main session's ruling): adopt
;; reads every writer's owner, uncertainty and retirement files before its
;; first write, a mirror's included, and refuses naming one it cannot read.
(printf "== the inventory: a mirror's unreadable file refuses adopt ==\n")
(define encode-record* (lookup-in 'encode-record '(theourgia wire)))
(define (damaged-store-with-mirror!)
  (let* ((d (fresh-store! '("One" "Two")))
         (w (writer-of d))
         (bytes (encode-record* 1 1757300000001 "a" '() '(set "t" x "x"))))
    (log-publish! d "mirrorz9" 1 bytes (segment-sha bytes))
    (append-line! (wfile d w "000001.sexp")
                  "deadbeef (3 1757300000003 \"t\" () (put ((kind . section))))\n")
    (list d w)))
(let* ((x (damaged-store-with-mirror!)) (d (car x)))
  (let ((t (traced d (lambda () (adopt! d)))))
    (want "CONTROL a damaged local writer beside a readable mirror is adopted"
          (car (answer-of t))
          'adopted)))
(for-each
  (lambda (name)
    (let* ((x (damaged-store-with-mirror!)) (d (car x))
           (f (wfile d "mirrorz9" name)))
      (spit! f (if (string=? name "owner.sexp") "((instance \"x\"))\n" "()\n"))
      (chmod! "000" f)
      (let ((t (traced d (lambda () (adopt! d)))))
        (chmod! "600" f)
        (want (string-append "INVENTORY a mirror's " name " at 000 refuses adopt, naming it, and nothing is written")
              (list (car (refusal-of (answer-of t))) (refusal-path (answer-of t)) (written-names t))
              (list 'refused f '())))))
  '("owner.sexp" "uncertain.sexp" "retired.sexp"))

;; ---- review round 2: the continuation's inventory with no instance ---------
;; ADDED BY THE CODE SESSION (F77b review 2, the main session's request): the
;; inventory runs even when instance.sexp is absent, so a legacy registry is
;; not upgraded before the refusal.
(printf "== review 2: the continuation reads first, with instance.sexp absent ==\n")
(let* ((x (interrupted-adopt!)) (d (car x)) (old (cadr x))
       (r (wfile d old "retired.sexp")))
  (registry-to-legacy!)
  (delete-file (string-append d "/instance.sexp"))
  (chmod! "000" r)
  (let* ((legacy-before (legacy-registry?))
         (t (traced d (lambda () (continue-adopt! d))))
         (legacy-after (legacy-registry?)))
    (chmod! "600" r)
    (want "REVIEW2 with instance.sexp absent the continuation refuses the unreadable retirement, writes nothing, and the registry stays legacy"
          (list legacy-before (car (refusal-of (answer-of t))) (written-names t) legacy-after)
          '(#t refused () #t))))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "chmod -R u+rwX " root " 2>/dev/null"))
;; THE SENTINEL THE RUNNER READS: without it a fixture is red whatever its
;; rows say (run-fixtures.sh counts "<name> complete").
(printf "unreadable-adopt complete\n")
(exit (if (= bad 0) 0 1))
