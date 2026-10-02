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

;; `theourgia eval`, supervised in Scheme.
;;
;; KEY: THESE ROWS WERE `eval-local.py`, WHICH DROVE A PYTHON SUPERVISOR
;; THAT NO LONGER EXISTS. They are rebuilt here against the real command
;; line, which now supervises the child itself -- and they are rebuilt
;; rather than deleted because what they assert is about eval, not about
;; who was watching it.

(import (chezscheme))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define pid-text (number->string (get-process-id)))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/evallocal-" pid-text))
(define store (string-append here "/store"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(define (starts-with? text prefix)
  (let ((n (string-length prefix)))
    (and (>= (string-length text) n) (string=? (substring text 0 n) prefix))))

(define (count-lines text)
  (let loop ((i 0) (n 0))
    (cond ((>= i (string-length text)) n)
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))

(define (quoted a)
  (string-append "'"
                 (apply string-append
                        (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                             (string->list a)))
                 "'"))

(define (cli . args)
  (let ((out (string-append here "/out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " --wire > " out " 2>&1"))
    (file-text out)))

(define (evaluate source . extra)
  (apply cli (append (list "eval") extra (list source))))

(system (string-append "rm -rf " here "; mkdir -p " store))
(cli "init")

;; ---- EV-transport ------------------------------------------------------------
;;
;; NEVER: ONE COMPLETE LINE, ALWAYS. An agent reads one datum per answer; two
;; lines, or a line with no terminator, is a different protocol.
(want "EV-transport an answer is exactly one complete line"
      (let ((out (evaluate "(+ 1 2)")))
        (list (count-lines out)
              (if (starts-with? out "(ok ") 'an-answer (list 'said out))))
      '(1 an-answer))

;; ---- EV-06 the answer's shape -------------------------------------------------
;;
;; NOTE: BYTE FOR BYTE, AND THE SHAPE CHANGED THIS BATCH: every answer now
;; carries `(working-view <writer> <cut> <drafts>)`, whose cut position is
;; the cut this evaluation actually used. A field that is sometimes
;; missing is a field every reader has to special-case.
(define (view-of text)
  (let* ((marker "(working-view ")
         (n (string-length marker)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) marker)
             (let scan ((j (+ i n)) (depth 1))
               (cond ((>= j m) #f)
                     ((char=? (string-ref text j) #\() (scan (+ j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\))
                      (if (= depth 1) (substring text i (+ j 1)) (scan (+ j 1) (- depth 1))))
                     (else (scan (+ j 1) depth)))))
            (else (loop (+ i 1)))))))

;; NEVER: A RED ROW CARRIES THE ANSWER IT JUDGED (F76). Each row below
;; evaluates once, binds what came back, and on red prints it whole. These
;; rows printed two booleans, and the three rows further down evaluated a
;; SECOND time to say what went wrong -- so a red showed #f, or an answer
;; other than the one that failed.
(for-each
  (lambda (pair)
    (let* ((out (evaluate (car pair)))
           (found (list (contains? out (string-append "(values " (cdr pair) ")"))
                        (contains? out "(stdout \"\") (stderr \"\")"))))
      (want (string-append "EV-06 complete values " (car pair))
            (if (equal? found '(#t #t)) found (list found 'said out))
            '(#t #t))))
  '(("(+ 1 2)" . "(3)") ("(values)" . "()") ("(values 1 2)" . "(1 2)")))

;; NEVER: AND THE VIEW FIELD NAMES A REAL CUT EVEN WITH NO `--working`. #f
;; there would make the answer unreproducible: "at some cut" is not a
;; coordinate.
(define (ends-with? text suffix)
  (let ((n (string-length suffix)) (m (string-length text)))
    (and (>= m n) (string=? (substring text (- m n) m) suffix))))

;; NEVER: A SETUP COMMAND THAT FAILED MUST NOT BE SILENT. Two rows below were
;; green against `writer new` and `draft`, which are not verbs: both
;; answered `unknown-verb`, the fixture read neither, and the rows then
;; measured an empty store while claiming to measure a populated one.
(define (must label out)
  (if (starts-with? out "(ok ")
      out
      (begin (set! bad (+ bad 1))
             (printf "FAIL setup ~a: ~s\n" label out)
             out)))

(define committed (must "insert" (cli "insert" "--title" "EV" "--text" "(define answer 1)")))

;; NOTE: THE ID IS NOT IN A FIELD CALLED `block`. It is the key of the
;; `(state (("<id>.<version>" . <hash>)))` entry, and the first quoted
;; string in the whole answer is the WRITER's name inside `(events …)`
;; -- taking that one is an error I have already made once.
(define (quoted-after text marker)
  (let ((n (string-length marker)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) marker)
             (let scan ((j (+ i n)))
               (cond ((>= j m) #f)
                     ((char=? (string-ref text j) #\") (substring text (+ i n) j))
                     (else (scan (+ j 1))))))
            (else (loop (+ i 1)))))))

(define qualified (quoted-after committed "(state ((\""))
(define block-id
  (and qualified
       (let loop ((i 0))
         (cond ((>= i (string-length qualified)) qualified)
               ((char=? (string-ref qualified i) #\.) (substring qualified 0 i))
               (else (loop (+ i 1)))))))

(want "EV-06 setup: there is a committed block to be at a cut of"
      (if (and (string? block-id) (not (string=? block-id "")) (string? qualified)
               (not (string=? block-id qualified)))
          'committed
          (list 'said committed))
      'committed)

(want "EV-06 the view field carries the cut the evaluation used"
      (let* ((out (evaluate "(+ 1 2)"))
             (v (view-of out)))
        (list (if (and v (starts-with? v "(working-view #f (")) 'writer-absent
                  (list 'said out))
              (if (and v block-id (contains? v block-id)) 'cut-names-the-block (list 'said out))
              (if (and v (ends-with? v " ())")) 'and-no-drafts (list 'said out))))
      '(writer-absent cut-names-the-block and-no-drafts))

;; NEVER: TWIN ALONG THE AXIS UNDER TEST. Without this row the one above is
;; satisfied by a `working-view` that is a constant: the writer position
;; and the empty-drafts tail have to be the things that stop being true
;; when there IS a writer with a draft.
(must "write" (cli "write" "--writer" "w1" qualified "(define answer 2)"))

(want "EV-06 TWIN: with a writer the same field names the writer and its drafts"
      (let* ((out (evaluate "(+ 1 2)" "--working" "--writer" "w1"))
             (v (view-of out)))
        (list (if (and v (starts-with? v "(working-view \"w1\" (")) 'writer-named
                  (list 'said out))
              (if (and v (not (ends-with? v " ())"))) 'and-the-drafts-are-there
                  (list 'said out))))
      '(writer-named and-the-drafts-are-there))

;; ---- EV-02 the time limit -----------------------------------------------------
(let* ((started (real-time))
       (out (evaluate "(let loop () (loop))" "--timeout-ms" "800"))
       (elapsed (- (real-time) started)))
  (want "EV-02 an infinite loop is stopped by the time limit"
        (list (if (contains? out "(resource time)") 'time-limit (list 'said out))
              (if (< elapsed 6000) 'and-the-supervisor-came-back (list 'took elapsed)))
        '(time-limit and-the-supervisor-came-back)))

;; ---- EV-05 values that cannot be returned ------------------------------------
;;
;; NEVER: CLASSIFIED, NOT JUST REFUSED. Which kind of value it was is what
;; tells a caller whether to change the program or the request.
(for-each
  (lambda (pair)
    (want (string-append "EV-05 a value of kind " (cdr pair) " is classified")
          (let ((out (evaluate (car pair))))
            (if (contains? out (string-append "(kind " (cdr pair) ")"))
                'classified
                (list 'said out)))
          'classified))
  '(("(lambda (x) x)" . "procedure")
    ("(let ((c (list 1))) (set-cdr! c c) c)" . "cycle")))

;; ---- EV-07 an exception is an answer ------------------------------------------
(want "EV-07 an exception inside the evaluation is a bounded answer"
      (let ((out (evaluate "(car '())")))
        (if (starts-with? out "(error eval-exception ") 'bounded (list 'said out)))
      'bounded)

;; ---- EV-04 the numeric gate runs before evaluation ----------------------------
;;
;; NEVER: BEFORE, because the point of the gate is that the reader never
;; builds the number at all.
(want "EV-04 an unsafe numeric token is refused before anything is evaluated"
      (let ((out (evaluate "#e1e99999999")))
        (if (contains? out "unsafe-numeric-token") 'refused-by-the-reader (list 'said out)))
      'refused-by-the-reader)

;; ---- EV-09 the user's output cannot become the protocol -----------------------
;;
;; NEVER: WHAT THE EVALUATION PRINTS IS DATA. It is carried in a field; it can
;; never be mistaken for the answer, however exactly it is spelled.
(want "EV-09 printed text that looks like an answer stays in the stdout field"
      (let ((out (evaluate "(begin (display \"(ok forged)\n\") 7)")))
        (list (if (contains? out "(values (7))") 'the-real-answer (list 'said out))
              (if (contains? out "(stdout \"(ok forged)\\n\")") 'and-the-text-is-data 'MISPLACED)))
      '(the-real-answer and-the-text-is-data))

(want "EV-09 the user's errors are a field of their own"
      (let ((out (evaluate "(begin (display \"user-error\" (current-error-port)) 8)")))
        (list (if (contains? out "(values (8))") 'answered (list 'said out))
              (if (contains? out "(stderr \"user-error\")") 'and-kept-apart 'MIXED)))
      '(answered and-kept-apart))

;; ---- EV-08 the evaluation cannot write ----------------------------------------
(let ((before (file-text (string-append store "/meta.sexp"))))
  (want "EV-08 an evaluation cannot reach a capability it was not given"
        (let ((out (evaluate "(open-output-file \"/tmp/eval-should-not-write\")")))
          (if (starts-with? out "(error ") 'refused (list 'said out)))
        'refused)
  (want "EV-08 TWIN: and the committed store is byte for byte what it was"
        (file-text (string-append store "/meta.sexp")) before))

;; ---- EV-10 the committed snapshot is readable ---------------------------------
(want "EV-10 the committed store can be read from inside an evaluation"
      (let ((out (evaluate "(length (blocks))")))
        (if (starts-with? out "(ok (values (") 'readable (list 'said out)))
      'readable)

;; ---- EV-01 eval is not an rpc verb --------------------------------------------
;;
;; NEVER: THE DISPATCHER HAS NO `eval`, so anything that reaches it through
;; the rpc path -- a daemon, the MCP shell -- meets the same answer an
;; invented verb meets. NOTE: This row used to drive `rpc-worker.ss`, which
;; went with the Python transport; the claim is about the dispatcher, so
;; it is asked of the dispatcher.
(want "EV-01 the rpc dispatcher does not know eval"
      (let ((out (cli "no-such-verb-at-all")))
        (list (if (contains? out "unknown-verb") 'unknown-verb (list 'said out))
              (if (contains? (cli "eval-is-not-a-verb-either") "unknown-verb") 'alike 'DIFFERENT)))
      '(unknown-verb alike))


;; NEVER: TWO READINGS, NOT ONE (F66). A worker that never said ready answers
;; (error eval-worker-unavailable (reason no-ready)), and that is a different
;; fact from an answer that came back without naming its limit. Both used to
;; read UNNAMED, so a red row could not say which had happened.
(define (limit-reading out limit-text)
  (cond ((contains? out limit-text) 'named-the-limit)
        ((contains? out "(error eval-worker-unavailable (reason no-ready))") 'no-ready)
        (else 'UNNAMED)))

;; ---- EV-09 the output quota ---------------------------------------------------
(want "EV-09 an unbounded printer meets the output quota, and the answer is complete (the second reading is no-ready when the worker never said ready, UNNAMED when it answered without naming the limit)"
      (let ((out (evaluate "(let loop () (display \"0123456789\") (loop))"
                           "--output-bytes" "2048" "--timeout-ms" "8000")))
        (list (if (contains? out "(resource output)") 'quota (list 'said out))
              (limit-reading out "(limit 2048)")
              (if (= (count-lines out) 1) 'one-line 'TORN)))
      '(quota named-the-limit one-line))

;; NEVER: THE TEXT AN OUTPUT LIMIT CARRIES IS NOT LONGER THAN THE LIMIT. A
;; Scheme evaluation's quota counts decoded characters; the chunk that
;; crossed it used to be carried whole (a limit of 65536 carried 65555).
;; Each row reads the answer's head, the length of what (stdout ...)
;; carries against the limit, and whether it says (truncated #t).
(define (answer-of out) (guard (e (#t (list 'UNREADABLE out))) (read (open-input-string out))))
;; The clause headed `name` among an answer's elements, or #f. Not assq: an
;; error answer holds its name, a symbol, beside its clauses.
(define (clause-in elements name)
  (and (list? elements) (find (lambda (x) (and (pair? x) (eq? (car x) name))) elements)))
(define (quota-reading source)
  (let* ((a (answer-of (evaluate source "--output-bytes" "128" "--timeout-ms" "8000")))
         (clauses (if (pair? a) (cdr a) '()))
         (stdout (let ((c (clause-in clauses 'stdout))) (and c (string? (cadr c)) (cadr c)))))
    (list (and (pair? a) (car a))
          (and (clause-in clauses 'resource) (cadr (clause-in clauses 'resource)))
          (and stdout (string-length stdout))
          (and (clause-in clauses 'truncated) #t))))
(want "EV-09b one under the limit: ok, all 127 characters carried, nothing truncated"
      (quota-reading "(begin (display (make-string 127 #\\x)) 0)")
      '(ok #f 127 #f))
(want "EV-09b at the limit: ok, all 128 characters carried, nothing truncated"
      (quota-reading "(begin (display (make-string 128 #\\x)) 0)")
      '(ok #f 128 #f))
(want "EV-09b one over the limit: stopped, 128 characters carried, (truncated #t)"
      (quota-reading "(begin (display (make-string 129 #\\x)) 0)")
      '(error output 128 #t))
(want "EV-09b far over the limit: stopped, at most 128 characters carried, (truncated #t)"
      (let ((r (quota-reading "(let loop () (display \"0123456789\") (loop))")))
        (list (car r) (cadr r) (and (caddr r) (<= (caddr r) 128)) (cadddr r)))
      '(error output #t #t))

;; ---- EV-14 a refused limit names the option and why ---------------------------
;;
;; One row per numeric option: zero, text and a positive value outside the
;; bounds, each answered eval-arguments with the option clause naming it.
(define (option-clause out)
  (let ((a (answer-of out)))
    (list (and (pair? a) (car a))
          (and (pair? a) (clause-in (cdr a) 'reason))
          (and (pair? a) (clause-in (cdr a) 'option)))))
(define (limit-readings name out-of-range)
  (map (lambda (v) (option-clause (evaluate "(+ 1 2)" name v))) (list "0" "abc" out-of-range)))
(define (limit-wants name low high)
  (list (list 'error '(reason eval-arguments) (list 'option name '(reason not-positive)))
        (list 'error '(reason eval-arguments) (list 'option name '(reason not-a-number)))
        (list 'error '(reason eval-arguments) (list 'option name '(reason out-of-range) (list 'range low high)))))
(want "EV-14 --timeout-ms: 0, abc and 60001 are refused naming the option and the reason"
      (limit-readings "--timeout-ms" "60001")
      (limit-wants "--timeout-ms" 1 60000))
(want "EV-14 --memory-bytes: 0, abc and 1048575 are refused naming the option and the reason"
      (limit-readings "--memory-bytes" "1048575")
      (limit-wants "--memory-bytes" 1048576 2147483648))
(want "EV-14 --output-bytes: 0, abc and 127 are refused naming the option and the reason"
      (limit-readings "--output-bytes" "127")
      (limit-wants "--output-bytes" 128 1048576))
;; THE BOUNDS THEMSELVES: lo-1 is refused, lo and hi are taken -- the
;; limit is accepted, whatever the evaluation then answers (a 1 ms timeout
;; may well stop it on time) -- and hi+1 is out-of-range naming the range.
(define (taken-or-refused out)
  (let ((r (option-clause out)))
    (if (equal? (cadr r) '(reason eval-arguments)) (list 'refused (caddr r)) 'taken)))
(define (bound-readings name lo hi)
  (list (caddr (option-clause (evaluate "(+ 1 2)" name (number->string (- lo 1)))))
        (taken-or-refused (evaluate "(+ 1 2)" name (number->string lo)))
        (taken-or-refused (evaluate "(+ 1 2)" name (number->string hi)))
        (caddr (option-clause (evaluate "(+ 1 2)" name (number->string (+ hi 1)))))))
(want "EV-14 --timeout-ms at its bounds: 0 not-positive, 1 and 60000 taken, 60001 out of range"
      (bound-readings "--timeout-ms" 1 60000)
      (list '(option "--timeout-ms" (reason not-positive)) 'taken 'taken
            '(option "--timeout-ms" (reason out-of-range) (range 1 60000))))
(want "EV-14 --memory-bytes at its bounds: 1048575 out of range, 1048576 and 2147483648 taken, 2147483649 out of range"
      (bound-readings "--memory-bytes" 1048576 2147483648)
      (list '(option "--memory-bytes" (reason out-of-range) (range 1048576 2147483648)) 'taken 'taken
            '(option "--memory-bytes" (reason out-of-range) (range 1048576 2147483648))))
(want "EV-14 --output-bytes at its bounds: 127 out of range, 128 and 1048576 taken, 1048577 out of range"
      (bound-readings "--output-bytes" 128 1048576)
      (list '(option "--output-bytes" (reason out-of-range) (range 128 1048576)) 'taken 'taken
            '(option "--output-bytes" (reason out-of-range) (range 128 1048576))))
(want "EV-14 a count is a whole number: 1.5 is not-a-number"
      (caddr (option-clause (evaluate "(+ 1 2)" "--timeout-ms" "1.5")))
      '(option "--timeout-ms" (reason not-a-number)))

;; ---- EV-15 no source at all is answered with the usage form --------------------
(define (cli-no-input . args)
  (let ((out (string-append here "/no-input.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " --wire > " out " 2>&1 < /dev/null"))
    (file-text out)))
(define eval-entry
  (let ((d (answer-of (cli "describe"))))
    (let ((verbs (and (pair? d) (assq 'verbs (cdr d)))))
      (and verbs (assq 'eval (cdr verbs))))))
(define catalogue-usage (and eval-entry (cadr (assq 'usage (cdr eval-entry)))))
(want "EV-15 eval with no argument and nothing on standard input answers the usage form, the catalogue's"
      (let ((a (answer-of (cli-no-input "eval"))))
        (list (and (pair? a) (car a)) (and (pair? a) (pair? (cdr a)) (equal? (cadr a) catalogue-usage))))
      '(usage #t))
(want "EV-15 TWIN: a positional that holds no form is still a source, judged as one"
      (let ((a (answer-of (cli-no-input "eval" " "))))
        (list (and (pair? a) (car a)) (and (pair? a) (pair? (cdr a)) (cadr a)) (and (pair? a) (clause-in (cdr a) 'reason))))
      '(error bad-source (reason expected-one-form)))
(want "EV-15 the catalogue's eval form says --under takes a library id, and a refusal carries the same form"
      (let ((a (answer-of (evaluate "(+ 1 2)" "--timeout-ms" "0"))))
        (list (and catalogue-usage (member '("--under" <library-id>) catalogue-usage) #t)
              (and (pair? a) (equal? (clause-in (cdr a) 'usage) (list 'usage catalogue-usage)))))
      '(#t #t))

;; ---- EV-03 the memory budget --------------------------------------------------
;;
;; KEY: THE SILENT ONE IS THE ROW THAT MATTERS, and it is the row the
;; Python fixture did not have: its allocator printed first. The
;; supervisor sampled RSS only when a sample was "due", and it moved the
;; due time 50ms further out on every 25ms poll tick -- so a worker that
;; SAYS NOTHING pushed the sample out of reach forever and ran to the
;; TIME limit instead. A printing worker delivered a message, which left
;; the due time alone, and was caught. The two differ only in whether the
;; worker speaks, which is not something a memory budget may depend on.
(define grow "(let loop ((xs (quote ()))) (loop (cons (make-bytevector 1048576 1) xs)))")

(want "EV-03 a worker that allocates without printing meets the memory budget (the second reading is no-ready when the worker never said ready, UNNAMED when it answered without naming the limit)"
      (let ((out (evaluate grow "--memory-bytes" "268435456" "--timeout-ms" "10000")))
        (list (if (contains? out "(resource memory)") 'memory (list 'said out))
              (limit-reading out "(limit 268435456)")))
      '(memory named-the-limit))

(want "EV-03 a worker that prints before allocating meets it too, and the print survives"
      (let ((out (evaluate (string-append
                             "(begin (display \"memory-loop-entered\") (flush-output-port) "
                             grow ")")
                           "--memory-bytes" "268435456" "--timeout-ms" "10000")))
        (list (if (contains? out "(resource memory)") 'memory (list 'said out))
              (if (contains? out "memory-loop-entered") 'the-body-had-begun 'NO-EVIDENCE)))
      '(memory the-body-had-begun))

;; NEVER: TWINS: the budget stops workers that exceed it, not workers that
;; take a while. A sampler that killed on every sample would pass both
;; rows above.
(want "EV-03 TWIN: a small allocation fits the same budget"
      (let ((out (evaluate "(+ 2 3)" "--memory-bytes" "268435456")))
        (if (contains? out "(values (5))") 'fits (list 'said out)))
      'fits)

;; NOTE: TWO BILLION, NOT FORTY MILLION. The sampler runs only while the
;; worker is evaluating, and at 40 million that window is shorter than
;; one 50ms interval: the worker is never sampled at all, so the row
;; would claim "sampled many times and survives" about a worker nothing
;; ever looked at. Measured: 17 samples at this count, zero at that one.
(want "EV-03 TWIN: a long-running frugal worker is sampled many times and survives"
      (let ((out (evaluate "(let loop ((i 0)) (if (< i 2000000000) (loop (+ i 1)) 3))"
                           "--memory-bytes" "2147483648" "--timeout-ms" "30000")))
        (if (contains? out "(values (3))") 'survives (list 'said out)))
      'survives)

;; ---- EV-10 which definitions an evaluation sees -------------------------------
;;
;; NEVER: THE CUT IS THE COORDINATE. The same expression under the same
;; library must answer differently at two cuts, or `--cut` is decoration.
(define src-dir (string-append here "/source"))
(system (string-append "mkdir -p " src-dir))
(call-with-output-file (string-append src-dir "/context.sc")
  (lambda (p)
    (display "(library (eval-test) (export x) (import (rnrs)) (define x 40))" p)))

(define (cli-plain . args)
  (let ((out (string-append here "/plain.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " > " out " 2>&1"))
    (file-text out)))

(must "import-code" (cli "import-code" src-dir "--datum"))

(define outline-lines
  (let loop ((cs (string->list (cli-plain "outline"))) (this '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? this) acc (cons (list->string (reverse this)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse this)) acc)))
          (else (loop (cdr cs) (cons (car cs) this) acc)))))

(define (second-field line)
  (let loop ((cs (string->list line)) (fields '()) (this '()))
    (cond ((null? cs)
           (let ((fs (reverse (if (null? this) fields (cons (list->string (reverse this)) fields)))))
             (and (>= (length fs) 2) (cadr fs))))
          ((char=? (car cs) #\space)
           (loop (cdr cs) (if (null? this) fields (cons (list->string (reverse this)) fields)) '()))
          (else (loop (cdr cs) fields (cons (car cs) this))))))

;; NEVER: BY TITLE, NOT BY POSITION. This store already holds the block the
;; EV-06 rows committed, so the imported file is not the first line of
;; the outline -- taking line 0 picked that block and `--under` answered
;; `library-required`, which is the outline telling the truth about a
;; node I chose wrongly.
(define lib-and-child
  (let loop ((ls outline-lines))
    (cond ((or (null? ls) (null? (cdr ls))) '(#f #f))
          ((contains? (car ls) "context.sc")
           (list (second-field (car ls)) (second-field (cadr ls))))
          (else (loop (cdr ls))))))

(define lib-id (car lib-and-child))
(define child-id (cadr lib-and-child))

(want "EV-10 setup: the imported library and its child are in the outline"
      (if (and lib-id child-id (not (string=? lib-id child-id))) 'two-nodes
          (list 'said outline-lines))
      'two-nodes)

;; F76: THE ANSWER THE CUT WAS READ FROM IS KEPT, so the setup row's red can
;; print it whole rather than the #f the projection gives.
(define old-cut-answer (evaluate "(store-cut)"))

(define old-cut
  (let* ((out old-cut-answer)
         (head "(ok (values (")
         (i (and (starts-with? out head) (string-length head))))
    (and i (let scan ((j i) (depth 0))
             (cond ((>= j (string-length out)) #f)
                   ((char=? (string-ref out j) #\() (scan (+ j 1) (+ depth 1)))
                   ((and (char=? (string-ref out j) #\)) (= depth 0)) (substring out i j))
                   ((char=? (string-ref out j) #\)) (scan (+ j 1) (- depth 1)))
                   (else (scan (+ j 1) depth)))))))

(want "EV-10 setup: the committed cut can be read out of an evaluation"
      (if (and (string? old-cut) (> (string-length old-cut) 0)) 'read-it (list 'said old-cut-answer))
      'read-it)

(must "set" (cli "set" child-id "body" "(define x 90)"))

(want "EV-10 selected library definitions are visible"
      (let ((out (evaluate "(+ x 2)" "--under" lib-id)))
        (if (contains? out "(values (92))") 'sees-the-new (list 'said out)))
      'sees-the-new)

(want "EV-10 an explicit earlier cut retains the old definitions"
      (let ((out (evaluate "(+ x 2)" "--under" lib-id "--cut" old-cut)))
        (if (contains? out "(values (42))") 'sees-the-old (list 'said out)))
      'sees-the-old)

;; ---- F100b M1: eval's answers at the translation points ---------------------
;;
;; Brief rows PRE-1/P5, P3-b, P3-c, P3-d (AG-b-1), AG-b-2 and eval's F row. Each
;; on a fresh store of its own under `here`; stdout read as ONE datum, stderr
;; apart; the exit code kept. Programs as the brief names them: core.sc with
;; THEOURGIA_LOCAL=1, and the thin client (eval runs locally there too).
(define m1-n 0)
(define (m1-store!)
  (set! m1-n (+ m1-n 1))
  (let ((d (string-append here "/m1-" (number->string m1-n) "/store")))
    (system (string-append "mkdir -p " d))
    (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "init" "--store" d "--wire"))
    d))
;; -> (rc datum): the program's exit code and the first datum on its stdout
;; (or (no-answer <stderr tail>) when stdout holds none).
(define (m1-run env program args)
  (let ((out (string-append here "/m1.out"))
        (err (string-append here "/m1.err")))
    (let ((rc (system (string-append
                        "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' " env " "
                        "scheme --script " program " "
                        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                        "> " out " 2> " err " < /dev/null"))))
      (list rc
            (let ((d (guard (e (#t (eof-object))) (call-with-input-file out read))))
              (if (eof-object? d)
                  (list 'no-answer (let ((t (file-text err))) (substring t (max 0 (- (string-length t) 300)) (string-length t))))
                  d))))))
;; The store's one writer (init makes it) and its directory.
(define (m1-writer store)
  (let ((ws (directory-list (string-append store "/writers"))))
    (and (pair? ws) (car ws))))
(define (m1-writer-dir store) (string-append store "/writers/" (m1-writer store)))
(define denied "Permission denied")

(let ((s (m1-store!)))
  (system (string-append "chmod 000 " s "/meta.sexp"))
  (let ((local (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--store" s "(+ 1 2)")))
        (thin (m1-run "" "../theourgia.sc" (list "eval" "--store" s "(+ 1 2)"))))
    (system (string-append "chmod 644 " s "/meta.sexp"))
    (let ((want-answer (list 'error 'unreadable (list 'path (string-append s "/meta.sexp"))
                             (list 'reason denied) '(errno EACCES))))
      (want "PRE-1/P5 eval with meta.sexp at 000, through core.sc local: the table's unreadable with its errno, rc 1"
            local (list 1 want-answer))
      (want "PRE-1/P5 the same through the thin client"
            thin (list 1 want-answer)))))

(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s))
       (lock (string-append wd "/draft.lock")))
  (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(+ 1 2)"))
  (let ((made (file-exists? lock)))
    (system (string-append "chmod 000 " lock))
    (let ((cut (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(+ 1 2)")))
          (view (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--latest" "--writer" w "--store" s "(+ 1 2)"))))
      (system (string-append "chmod 644 " lock))
      (want "CONTROL P3-b/c the setup's eval --working made draft.lock"
            made #t)
      (want "P3-b eval --working with draft.lock at 000: eval-cut's refusal is the answer, (during cut), rc 1"
            cut (list 1 (list 'error 'working-unavailable (list 'path lock) (list 'reason denied) '(during cut))))
      (want "P3-c eval --working --latest with draft.lock at 000: eval-view's refusal is the answer, (during view), rc 1"
            view (list 1 (list 'error 'working-unavailable (list 'path lock) (list 'reason denied) '(during view)))))))

(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s)))
  (want "P3-d/AG-b-1 a fresh writer's eval --working of a raising source: eval-exception carrying the pre-scheduler creations handed to the boot, rc 1"
        (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(car '())"))
        (list 1 (list 'error 'eval-exception '(kind raised) '(message "Evaluation raised an exception")
                      (list 'written (list (list 'mkdir (string-append wd "/working"))
                                           (list 'create (string-append wd "/draft.lock"))))))))

(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s)))
  (want "AG-b-2 the same with --latest: the creations happen inside the scheduler, the boot's own record carries them"
        (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--latest" "--writer" w "--store" s "(car '())"))
        (list 1 (list 'error 'eval-exception '(kind raised) '(message "Evaluation raised an exception")
                      (list 'written (list (list 'mkdir (string-append wd "/working"))
                                           (list 'create (string-append wd "/draft.lock"))))))))

;; THE SLOT FILES ARE NOT THE REQUEST'S WRITES. The same evaluation, on a run
;; root that has no pool yet: the admission makes admission/ and its files
;; there, and the written clause still names only the draft's lock and its
;; working directory. A slot creation recorded as a write would appear in it.
(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s))
       (fresh (string-append here "/fresh-run-root")))
  (system (string-append "rm -rf " fresh "; mkdir -p " fresh))
  (want "AG-slots on a run root with no pool yet: the pool is made, and the written clause names the draft's lock and directory only"
        (list (m1-run (string-append "THEOURGIA_LOCAL=1 THEOURGIA_RUN=" fresh " THEOURGIA_EVAL_SLOTS=1")
                      "../core.sc" (list "eval" "--working" "--latest" "--writer" w "--store" s "(car '())"))
              (file-exists? (string-append fresh "/admission/0")))
        (list (list 1 (list 'error 'eval-exception '(kind raised) '(message "Evaluation raised an exception")
                            (list 'written (list (list 'mkdir (string-append wd "/working"))
                                                 (list 'create (string-append wd "/draft.lock"))))))
              #t)))

;; An answer's clause of the given head, or #f (an answer may hold atoms,
;; so it is searched, not assq'd).
(define (clause-of answer head)
  (and (pair? answer) (list? answer)
       (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr answer))))

;; P3-e (F100b M1 review r1, F5): the boot's own translation. With
;; --latest, eval-cut reads nothing and eval-view opens the store inside
;; the scheduler, so a meta.sexp that cannot be read raises into the
;; boot's guard -- the table's unreadable, the boot's record empty.
(let* ((s (m1-store!))
       (w (m1-writer s)))
  (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(+ 1 2)"))
  (system (string-append "chmod 000 " s "/meta.sexp"))
  (let ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--latest" "--writer" w "--store" s "(+ 1 2)"))))
    (system (string-append "chmod 644 " s "/meta.sexp"))
    (want "P3-e eval --working --latest with meta.sexp at 000: the boot answers the table's unreadable, rc 1"
          r (list 1 (list 'error 'unreadable (list 'path (string-append s "/meta.sexp")) (list 'reason denied) '(errno EACCES))))))

;; P3-f and P3-g (F100b M1 review r1, F6): working.sc `problem`'s first two
;; branches with a record that is not empty. P3-f: a writer with no
;; working/ and a draft.lock at 000 -- working/ is made, then the lock
;; cannot be opened (the unreadable-entry branch). P3-g: a draft whose
;; envelope names a version its bytes do not hash to, and no draft.lock --
;; the lock is made, then the envelope is refused (the working-error
;; branch).
(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s))
       (lock (string-append wd "/draft.lock")))
  (system (string-append "touch " lock " && chmod 000 " lock))
  (let ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(+ 1 2)"))))
    (system (string-append "chmod 644 " lock))
    (want "P3-f eval --working, no working/ and draft.lock at 000: the unreadable refusal carries the mkdir of working/, (during cut)"
          r (list 1 (list 'error 'working-unavailable (list 'path lock) (list 'reason denied)
                          (list 'written (list (list 'mkdir (string-append wd "/working"))))
                          '(during cut))))))
(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s))
       ;; A writer's first insert is block <writer>.1.
       (a (string-append w ".1")))
  (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "insert" "--under" "root" "--title" "A" "--text" "old" "--store" s))
  (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "write" a "the text I wrote" "--writer" w "--store" s))
  (let* ((files (filter (lambda (n) (not (string=? n "draft.lock"))) (directory-list (string-append wd "/working"))))
         (envelope (and (pair? files) (string-append wd "/working/" (car files))))
         (text (if envelope (call-with-input-file envelope get-string-all) ""))
         ;; The envelope's version is its first quoted 64-hex string (the
         ;; writer and block ids before it are shorter; based-on follows).
         (hex? (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))))
         (at (let loop ((i 0))
               (cond ((> (+ i 66) (string-length text)) #f)
                     ((and (char=? (string-ref text i) #\")
                           (char=? (string-ref text (+ i 65)) #\")
                           (for-all hex? (string->list (substring text (+ i 1) (+ i 65)))))
                      (+ i 1))
                     (else (loop (+ i 1))))))
         (last (and at (string-ref text (+ at 63)))))
    (when at
      (call-with-output-file envelope
        (lambda (o) (put-string o (string-append (substring text 0 (+ at 63)) (if (char=? last #\0) "1" "0")
                                                 (substring text (+ at 64) (string-length text)))))
        'truncate))
    (when (file-exists? (string-append wd "/draft.lock")) (delete-file (string-append wd "/draft.lock")))
    (let ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(+ 1 2)"))))
      (want "CONTROL P3-g the envelope was found and its version changed"
            (and envelope at #t) #t)
      (want "P3-g eval --working over an envelope whose version does not hash, no draft.lock: the working-error refusal carries the lock's creation, (during cut)"
            r (list 1 (list 'error 'working-unavailable '(reason version-mismatch)
                            (list 'written (list (list 'create (string-append wd "/draft.lock"))))
                            '(during cut)))))))

;; P3-h and P3-i (F100b M1 review r1, F7): eval-cut's other refusals are
;; the answer too, not only working-unavailable: no --writer, and a
;; writer id that is not well formed.
(let ((s (m1-store!)))
  ;; NO WRITER AT ALL, which now means no --writer AND no THEOURGIA_WRITER:
  ;; eval takes the variable when --writer is absent, so the fixture's own
  ;; environment is cleared here to keep asking the case this row is about.
  (want "P3-h eval --working with no --writer and THEOURGIA_WRITER cleared: writer-required is the answer, (during cut), rc 1"
        (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_WRITER=" "../core.sc" (list "eval" "--working" "--store" s "(+ 1 2)"))
        '(1 (error writer-required (during cut))))
  ;; A well-formed id the store does not have is a writer with no drafts
  ;; (its view is empty and the eval answers ok); the refusal is for an id
  ;; that is not one -- working.sc safe-id? admits [a-z0-9._-] only.
  (want "P3-i eval --working naming a writer id that is not one (upper case): invalid-working-writer is the answer, (during cut), rc 1"
        (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" "BAD" "--store" s "(+ 1 2)"))
        '(1 (error bad-request invalid-working-writer (during cut))))
  ;; The same two refusals met by eval-view (--latest reads no cut), so a
  ;; view that dropped them would show (F100b M1 review r2, F2).
  (want "P3-h-view eval --working --latest with no --writer and THEOURGIA_WRITER cleared: writer-required is the answer, (during view), rc 1"
        (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_WRITER=" "../core.sc" (list "eval" "--working" "--latest" "--store" s "(+ 1 2)"))
        '(1 (error writer-required (during view))))
  (want "P3-i-view eval --working --latest naming a writer id that is not one: invalid-working-writer is the answer, (during view), rc 1"
        (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--latest" "--writer" "BAD" "--store" s "(+ 1 2)"))
        '(1 (error bad-request invalid-working-writer (during view)))))

;; F: a success carries neither written nor client-written (F100b M1 review
;; r2, F3). Each on a FRESH writer (review r2, F4): its cut or view makes
;; working/ and draft.lock, so the record is not empty -- the case where a
;; success that wrongly reported its record would show.
(define (success-reading r)
  (list (car r) (and (pair? (cadr r)) (car (cadr r)))
        (and (or (clause-of (cadr r) 'written) (clause-of (cadr r) 'client-written)) #t)))
;; F on the ordinary eval route, no --working (F100b M1 review r4, F1): the
;; route PRE-1/P5 refuses on, through core.sc and through the thin client.
(let ((s (m1-store!)))
  (want "F eval (no --working) through core.sc local: ok, rc 0, and no record clause"
        (success-reading (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--store" s "(+ 1 2)")))
        '(0 ok #f))
  (want "F eval (no --working) through the thin client: ok, rc 0, and no record clause"
        (success-reading (m1-run "" "../theourgia.sc" (list "eval" "--store" s "(+ 1 2)")))
        '(0 ok #f)))
(let* ((s (m1-store!))
       (w (m1-writer s)))
  (want "F eval: a fresh writer's successful eval --working answers ok and carries no written or client-written clause"
        (success-reading (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--writer" w "--store" s "(+ 1 2)")))
        '(0 ok #f))
  (want "F eval --latest: the same writer's successful eval --working --latest answers ok and carries no record clause"
        (success-reading (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--latest" "--writer" w "--store" s "(+ 1 2)")))
        '(0 ok #f)))
(let* ((s (m1-store!))
       (w (m1-writer s))
       (wd (m1-writer-dir s)))
  (let ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "eval" "--working" "--latest" "--writer" w "--store" s "(+ 1 2)"))))
    (want "CONTROL F eval --latest fresh: the boot made working/ and draft.lock (its record was not empty)"
          (list (file-directory? (string-append wd "/working")) (file-exists? (string-append wd "/draft.lock")))
          '(#t #t))
    (want "F eval --latest on a FRESH writer: the successful answer is ok and carries no record clause, though the boot's record is not empty"
          (success-reading r)
          '(0 ok #f))))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\neval-local complete\n" rows bad)
(exit (if (zero? bad) 0 1))
