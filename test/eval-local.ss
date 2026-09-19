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
(define here (string-append "/tmp/evallocal-" pid-text))
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
              "scheme --script ../cli.ss "
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

(for-each
  (lambda (pair)
    (let ((out (evaluate (car pair))))
      (want (string-append "EV-06 complete values " (car pair))
            (list (contains? out (string-append "(values " (cdr pair) ")"))
                  (contains? out "(stdout \"\") (stderr \"\")"))
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
      (let ((v (view-of (evaluate "(+ 1 2)"))))
        (list (if (and v (starts-with? v "(working-view #f (")) 'writer-absent
                  (list 'said v))
              (if (and v block-id (contains? v block-id)) 'cut-names-the-block (list 'said v))
              (if (and v (ends-with? v " ())")) 'and-no-drafts (list 'said v))))
      '(writer-absent cut-names-the-block and-no-drafts))

;; NEVER: TWIN ALONG THE AXIS UNDER TEST. Without this row the one above is
;; satisfied by a `working-view` that is a constant: the writer position
;; and the empty-drafts tail have to be the things that stop being true
;; when there IS a writer with a draft.
(must "write" (cli "write" "--writer" "w1" qualified "(define answer 2)"))

(want "EV-06 TWIN: with a writer the same field names the writer and its drafts"
      (let ((v (view-of (evaluate "(+ 1 2)" "--working" "--writer" "w1"))))
        (list (if (and v (starts-with? v "(working-view \"w1\" (")) 'writer-named
                  (list 'said v))
              (if (and v (not (ends-with? v " ())"))) 'and-the-drafts-are-there
                  (list 'said v))))
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
          (if (contains? (evaluate (car pair)) (string-append "(kind " (cdr pair) ")"))
              'classified
              (list 'said (evaluate (car pair))))
          'classified))
  '(("(lambda (x) x)" . "procedure")
    ("(let ((c (list 1))) (set-cdr! c c) c)" . "cycle")))

;; ---- EV-07 an exception is an answer ------------------------------------------
(want "EV-07 an exception inside the evaluation is a bounded answer"
      (if (starts-with? (evaluate "(car '())") "(error eval-exception ") 'bounded
          (list 'said (evaluate "(car '())")))
      'bounded)

;; ---- EV-04 the numeric gate runs before evaluation ----------------------------
;;
;; NEVER: BEFORE, because the point of the gate is that the reader never
;; builds the number at all.
(want "EV-04 an unsafe numeric token is refused before anything is evaluated"
      (if (contains? (evaluate "#e1e99999999") "unsafe-numeric-token") 'refused-by-the-reader
          (list 'said (evaluate "#e1e99999999")))
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
        (if (starts-with? (evaluate "(open-output-file \"/tmp/eval-should-not-write\")") "(error ")
            'refused (list 'said (evaluate "(open-output-file \"/tmp/eval-should-not-write\")")))
        'refused)
  (want "EV-08 TWIN: and the committed store is byte for byte what it was"
        (file-text (string-append store "/meta.sexp")) before))

;; ---- EV-10 the committed snapshot is readable ---------------------------------
(want "EV-10 the committed store can be read from inside an evaluation"
      (if (starts-with? (evaluate "(length (blocks))") "(ok (values (") 'readable
          (list 'said (evaluate "(length (blocks))")))
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


;; ---- EV-09 the output quota ---------------------------------------------------
(want "EV-09 an unbounded printer meets the output quota, and the answer is complete"
      (let ((out (evaluate "(let loop () (display \"0123456789\") (loop))"
                           "--output-bytes" "2048" "--timeout-ms" "8000")))
        (list (if (contains? out "(resource output)") 'quota (list 'said out))
              (if (contains? out "(limit 2048)") 'named-the-limit 'UNNAMED)
              (if (= (count-lines out) 1) 'one-line 'TORN)))
      '(quota named-the-limit one-line))

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

(want "EV-03 a worker that allocates without printing meets the memory budget"
      (let ((out (evaluate grow "--memory-bytes" "268435456" "--timeout-ms" "10000")))
        (list (if (contains? out "(resource memory)") 'memory (list 'said out))
              (if (contains? out "(limit 268435456)") 'named-the-limit 'UNNAMED)))
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
      (if (contains? (evaluate "(+ 2 3)" "--memory-bytes" "268435456") "(values (5))")
          'fits (list 'said (evaluate "(+ 2 3)" "--memory-bytes" "268435456")))
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
              "scheme --script ../cli.ss "
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

(define old-cut
  (let* ((out (evaluate "(store-cut)"))
         (head "(ok (values (")
         (i (and (starts-with? out head) (string-length head))))
    (and i (let scan ((j i) (depth 0))
             (cond ((>= j (string-length out)) #f)
                   ((char=? (string-ref out j) #\() (scan (+ j 1) (+ depth 1)))
                   ((and (char=? (string-ref out j) #\)) (= depth 0)) (substring out i j))
                   ((char=? (string-ref out j) #\)) (scan (+ j 1) (- depth 1)))
                   (else (scan (+ j 1) depth)))))))

(want "EV-10 setup: the committed cut can be read out of an evaluation"
      (if (and (string? old-cut) (> (string-length old-cut) 0)) 'read-it (list 'said old-cut))
      'read-it)

(must "set" (cli "set" child-id "body" "(define x 90)"))

(want "EV-10 selected library definitions are visible"
      (if (contains? (evaluate "(+ x 2)" "--under" lib-id) "(values (92))") 'sees-the-new
          (list 'said (evaluate "(+ x 2)" "--under" lib-id)))
      'sees-the-new)

(want "EV-10 an explicit earlier cut retains the old definitions"
      (if (contains? (evaluate "(+ x 2)" "--under" lib-id "--cut" old-cut) "(values (42))")
          'sees-the-old
          (list 'said (evaluate "(+ x 2)" "--under" lib-id "--cut" old-cut)))
      'sees-the-old)

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\neval-local complete\n" rows bad)
(exit (if (zero? bad) 0 1))
