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

;; Request identity in the record, and the verbs that keep books.
;;
;; A REQUEST'S IDENTITY TRAVELS IN THE RECORD IT WROTE. Who asked, which
;; request it was, which sub-operation this is, the fingerprint of what
;; was asked, the plan it belongs to and the cursor it was written
;; against are all in the actor -- so any one record answers "was this
;; request executed", and a reader never infers the origin from the
;; physical writer. That inference is wrong for every record a successor
;; writer carries after an adopt, which is exactly when the question is
;; being asked.
;;
;; AND THE SHAPE IS CHECKED, NOT THE LENGTH. Two slots changed meaning
;; when the actor grew from five to six: a bare request id became the
;; pair `(origin . req-id)`, and `plan-event-id` was inserted before
;; `after`. A length test accepts an actor whose fields have all shifted
;; by one -- right size, every field reading its neighbour.
;;
;; BOOKKEEPING VERBS ARE UNDERSTOOD AND CHANGE NOTHING. They advance the
;; cursor and carry premises; they touch no block. A verb from outside
;; the table is not silently nothing: until now an older build reading a
;; newer store applied such a record, counted it as applied, and reported
;; a state missing it without saying so.

(import (chezscheme) (theourgia request) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia ffi) (theourgia wire)
        (only (igropyr crypto) sha256 bytevector->hex))

;; THE RANGE A SEGMENT HOLDS, READ OUT OF THE SEGMENT. A manifest entry
;; declares first and last sequence beside the hash. A fixture that
;; declared them from memory would be asserting its own arithmetic
;; rather than what it actually wrote, and the product's own check for a
;; manifest that contradicts its bytes would then be measuring the
;; fixture.
(define (segment-seqs bytes)
  (let ((text (utf8->string bytes)))
    (let loop ((i 0) (start 0) (seqs '()))
      (cond
        ((>= i (string-length text)) (reverse seqs))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (if (and (pair? r) (eq? (car r) 'ok)) (cons (cadr r) seqs) seqs))))
        (else (loop (+ i 1) start seqs))))))

(define (manifest-entry n hash bytes)
  (let ((seqs (segment-seqs bytes)))
    (if (null? seqs)
        (list n hash 1 1)
        (list n hash (apply min seqs) (apply max seqs)))))

(define (manifest-entry-text n hash bytes)
  (let ((e (manifest-entry n hash bytes)))
    (string-append "(" (number->string n) " \"" hash "\" "
                   (number->string (caddr e)) " " (number->string (cadddr e)) ")")))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
(define cli
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/cli.ss"))
         (above (string-append dir "/../cli.ss")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'q1
              "cli.ss is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.ss sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "q1 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))




(define scratch (test-dir "q1"))
(define home (string-append scratch "/home"))
(define out-path (string-append scratch "/out.txt"))
(define err-path (string-append scratch "/err.txt"))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (slurp path))) (if b (utf8->string b) "")))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))

(define n-store 0)
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    (putenv "THEOURGIA_HOME" home)
    d))
(define (run store . args)
  (let* ((cmd (string-append
                "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                "THEOURGIA_HOME=" home " "
                "scheme --script " cli " "
                (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                "--store " store " > " out-path " 2> " err-path))
         (code (system cmd))
         (text (text-of out-path)))
    (list code
          (let loop ((i 0) (start 0) (out '()))
            (cond
              ((>= i (string-length text)) (reverse out))
              ((char=? (string-ref text i) #\newline)
               (let ((line (substring text start i)))
                 (loop (+ i 1) (+ i 1)
                       (if (= 0 (string-length line)) out
                           (cons (guard (e (#t (list 'unreadable line)))
                                   (read (open-string-input-port line)))
                                 out)))))
              (else (loop (+ i 1) start out)))))))
(define (code-of r) (car r))
(define (lines-of r) (cadr r))

(printf "== T1: the actor carries a whole identity, checked by shape ==\n")
(define (accepts? actor)
  (not (guard (e (#t #t)) (encode-record 1 1 actor '() (storable-encode '(a))) #f)))
(define good (list "agent:claude" (cons "w" "req-1") 0 "fp" (cons "w" 2) (cons "w" 3)))
(want "a six element request actor is accepted, and reads back whole"
      (let ((r (decode-line (encode-record 1 1 good '() (storable-encode '(a))))))
        (list (car r) (list-ref r 3)))
      (list 'ok good))
(want "a plain name is still an actor"
      (accepts? "agent:claude") #t)
;; THE ROW A LENGTH CHECK PASSES. This is the old five-slot actor with
;; one field appended: the right number of elements, every field after
;; the first reading the one beside it.
(want "six elements in the old order are refused"
      (accepts? (list "agent:claude" "req-1" 'single "fp" #f (cons "w" 3)))
      #f)
(want "and so is the five element actor it came from"
      (accepts? (list "agent:claude" "req-1" 0 "fp" (cons "w" 3)))
      #f)
(want "each slot is checked: identity, sub, fingerprint, plan, after"
      (list (accepts? (list "a" "req-1" 0 "fp" #f (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 'other "fp" #f (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 0 42 #f (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 0 "fp" "not-an-event" (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 0 "fp" #f #f)))
      (list #f #f #f #f #f))
(want "TWIN: the shapes a request really uses are all accepted"
      (list (accepts? good)
            (accepts? (list "a" (cons "w" "r") 'single "fp" #f (cons "w" 1)))
            (accepts? (list "a" (cons "w" "r") 'plan "fp" #f (cons "w" 1)))
            (accepts? (list "a" (cons "w" (list 'batch "b1" 2)) 2 "fp" (cons "w" 1) (cons "w" 3))))
      (list #t #t #t #t))

(printf "\n== T2: bookkeeping verbs, and verbs this build does not know ==\n")
;; A BOOKKEEPING RECORD ADVANCES THE CURSOR AND CHANGES NO BLOCK. The
;; state hash is taken over the blocks alone, so this holds by
;; construction -- and the row is here because it is the property the
;; rest of the request machinery will lean on, not because the code
;; looks like it.
(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define (mirror-file name . recs)
  (let ((p (string-append scratch "/" name)))
    (put! p (apply cat recs))
    p))
(define (state-of d) (open-and-reduce d))

(define d1 (fresh-store!))
(run d1 "init")
(run d1 "insert" "--under" "root" "--title" "A")
(define hash-before (state-hash (state-of d1)))
(define bookkeeping
  (mirror-file "book.bin"
               (rec 1 '(plan "r1" "fp" (("w" . 1)) ()))
               (rec 2 '(batch "b1" "fp" (("w" . 1)) ()))
               (rec 3 '(resolve ("w" . "r1") "fp" all executed (("w" . 1) ("w" . 2)) (supersedes)))))
(want "CONTROL: three bookkeeping records publish"
      (car (lines-of (run d1 "publish" "mirrorzz" "1" bookkeeping)))
      '(ok (published 1)))
(want "they advance that writer's cursor"
      (cdr (assoc "mirrorzz" (reduce-applied-cut (state-of d1))))
      3)
(want "and they change no block, so the state hash is what it was"
      (state-hash (state-of d1))
      hash-before)
(want "nor are they reported as verbs this build does not know"
      (lines-of (run d1 "conflicts"))
      '())

;; A VERB FROM OUTSIDE THE TABLE IS AN OBSERVATION, NOT A NO-OP. Applied
;; silently, it would leave an older build reporting a state missing
;; those records without saying anything was missing.
(define d2 (fresh-store!))
(run d2 "init")
(want "CONTROL: the record publishes and is delivered"
      (car (lines-of (run d2 "publish" "mirrorzz" "1"
                          (mirror-file "unknown.bin" (rec 1 '(frobnicate "x"))))))
      '(ok (published 1)))
(want "an unrecognised verb is reported, naming the record and the verb"
      (lines-of (run d2 "conflicts"))
      (list (list 'unknown-verb (list 'event "mirrorzz" 1) (list 'verb 'frobnicate))))
(want "and a payload that is not even a form is reported too"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1" (mirror-file "atom.bin" (rec 1 'hello)))
        (lines-of (run d "conflicts")))
      (list (list 'unknown-verb (list 'event "mirrorzz" 1) (list 'verb 'malformed))))

;; Q15': A BOOKKEEPING RECORD STILL RELEASES WHAT WAITED ON IT. An
;; implementation that suppressed every effect while ingesting one would
;; leave the operation that depended on it pending for ever, and the row
;; above -- where nothing changed -- would pass just the same.
(define d3 (fresh-store!))
(run d3 "init")
(define blocked
  (mirror-file "blocked.bin"
               (encode-record 1 1757300001000 "agent:claude" '(("mirrorzz" . 2))
                              (storable-encode '(put ((kind . section) (title . "waited")))))))
;; THE WRITER NAME IS EIGHT CHARACTERS OF [0-9a-z]. A shorter one is not
;; a writer this store will ever read, which is a trap worth naming
;; here: the publish below would answer `published` for a name like
;; "mirrora" and the records would never be delivered to anything.
(want "CONTROL: an operation whose premise has not arrived is pending"
      (begin (run d3 "publish" "mirrorab" "1" blocked)
             (map car (lines-of (run d3 "conflicts"))))
      '(pending))
(want "and delivering the bookkeeping record it waited on applies it"
      (begin (run d3 "publish" "mirrorzz" "1"
                  (mirror-file "release.bin"
                               (rec 1 '(plan "r1" "fp" (("w" . 1)) ()))
                               (rec 2 '(plan "r2" "fp" (("w" . 1)) ()))))
             (list (lines-of (run d3 "conflicts"))
                   (length (lines-of (run d3 "log")))))
      (list '() 3))

(printf "\n== T3: the fingerprint covers who asked, what, and of what ==\n")
;; A RETRY IS THE SAME IDENTITY AND THE SAME FINGERPRINT. The same
;; identity with a different fingerprint is a client reusing an id for a
;; different request -- a mistake to report, not a history to rebuild.
;;
;; THE DIGEST IS OVER A CANONICAL SERIALISATION, not over the fields
;; compared one at a time. A field-by-field comparison invites one that
;; forgets a field, and the field it forgets is the one nobody thought
;; of -- which is the same field the client changed.
;;
;; THE FIXED VECTOR BELOW WAS NOT COPIED OUT OF THIS IMPLEMENTATION. The
;; canonical string was printed, and an independent sha256 (python's
;; hashlib, outside this process and this language) was taken over the
;; same bytes; the two agreed. An expectation read back from the code it
;; checks is a statement that the code equals itself.
(define fixed-canonical
  "(\"agent:claude\" set \"b\" \"title\" \"x\" (\"w3k\" . 412))")
(want "the canonical form is the request's words in order"
      (sexpr->string-extended (list "agent:claude" 'set "b" "title" "x" (cons "w3k" 412)))
      fixed-canonical)
(want "and its fingerprint is this exact digest"
      (request-fingerprint "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 412))
      "9b16cdbd2325c04118e1d09f8288946fc3e81b84ec9fc9004a4241413df83521")
;; EACH PART OF THE REQUEST MOVES IT. A fingerprint that forgot the verb
;; would make `(set b "x")` and `(del b)` the same request.
(define (fp who verb args after) (request-fingerprint who verb args after))
(define base-fp (fp "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 412)))
(want "the argument, the verb, who asked and the cursor each change it"
      (list (equal? base-fp (fp "agent:claude" 'set (list "b" "title" "y") (cons "w3k" 412)))
            (equal? base-fp (fp "agent:claude" 'del (list "b" "title" "x") (cons "w3k" 412)))
            (equal? base-fp (fp "agent:other"  'set (list "b" "title" "x") (cons "w3k" 412)))
            (equal? base-fp (fp "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 413))))
      (list #f #f #f #f))
(want "TWIN: the same request asked twice has the same fingerprint"
      (equal? base-fp (fp "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 412)))
      #t)
;; THE ORIGIN IS READ FROM THE CURSOR THE REQUEST NAMED, never from
;; whichever writer holds the record now.
(want "the identity is the cursor's writer and the request id"
      (request-identity (cons "w3k" 412) "req-1")
      (cons "w3k" "req-1"))
(want "a request id is 1 to 64 of [A-Za-z0-9._-], or a batch item"
      (list (req-id-ok? "req-1") (req-id-ok? "a.b_c-D9")
            (req-id-ok? (list 'batch "b1" 0))
            (req-id-ok? "") (req-id-ok? "has space") (req-id-ok? "has/slash")
            (req-id-ok? (make-string 65 #\a)))
      (list #t #t #t #f #f #f #f))
(want "TWIN: sixty-four characters is still a request id"
      (req-id-ok? (make-string 64 #\a))
      #t)

(printf "\n~a failures\n" bad)
(printf "q1 complete\n")
