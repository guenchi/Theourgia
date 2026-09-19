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

;; Evaluating against a writer's working view.
;;
;; KEY: EVERY ROW HERE RUNS THE REAL COMMAND LINE AS A PROCESS. The view is
;; built before a child exists and carried into it as bytes; a row that
;; called the libraries would be testing neither of those.

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
(define here (string-append "/tmp/evalworking-" pid-text))
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

;; NOTE: SINGLE QUOTES, so the fixture never hands a shell anything to
;; expand -- the programs under test must receive the bytes as written.
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

(define (field-of text name)
  (let* ((marker (string-append name " . \""))
         (n (string-length marker)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) marker)
             (let scan ((j (+ i n)))
               (cond ((>= j m) #f)
                     ((char=? (string-ref text j) #\") (substring text (+ i n) j))
                     (else (scan (+ j 1))))))
            (else (loop (+ i 1)))))))

(system (string-append "rm -rf " here "; mkdir -p " store))
(cli "init")
(define inserted (cli "insert" "--title" "W11" "--text" "(define answer 1)"))

;; NEVER: THE ID COMES FROM THE `state` FIELD, not from the first quoted thing
;; in the answer. Measured by getting it wrong: the first `("` in an
;; insert's answer is inside `(events (("i64pwro4" . 1)))` -- the WRITER's
;; name -- and every row downstream then asked about a block that does not
;; exist and read `(error unknown-id "i64pwro4")`.
;; The `(working-view ...)` field of an answer, as text, and the cut
;; position inside it.
;;
;; NEVER: BY BALANCED PARENTHESES, NOT BY THE FIRST `)`. The cut is itself a
;; list of pairs, so a scan stopping at the first closer would return
;; `(("id" . 1` and every comparison against it would be about a prefix.
(define (balanced text i)
  (let loop ((j i) (depth 0))
    (cond ((>= j (string-length text)) #f)
          ((char=? (string-ref text j) #\() (loop (+ j 1) (+ depth 1)))
          ((char=? (string-ref text j) #\))
           (if (= depth 1) (+ j 1) (loop (+ j 1) (- depth 1))))
          (else (loop (+ j 1) depth)))))

(define (index-of text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))

(define (view-of answer)
  (let ((i (index-of answer "(working-view ")))
    (and i (let ((e (balanced answer i))) (and e (substring answer i e))))))

;; The writer position is either `#f` or a quoted name; the cut is the
;; next parenthesised group after it.
(define (view-cut answer)
  (let ((v (view-of answer)))
    (and v (let ((i (index-of v " (")))
             (and i (let ((e (balanced v (+ i 1))))
                      (and e (substring v (+ i 1) e))))))))

(define (block-id-of text)
  (let* ((marker "(state ((\"")
         (n (string-length marker)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) marker)
             (let scan ((j (+ i n)))
               (cond ((>= j m) #f)
                     ((char=? (string-ref text j) #\") (substring text (+ i n) j))
                     (else (scan (+ j 1))))))
            (else (loop (+ i 1)))))))

(define block-id (block-id-of inserted))

(want "W11-00 the fixture has a block to work on"
      (if (and block-id (contains? block-id ".")) 'have-a-block (list 'said inserted))
      'have-a-block)

;; Two writers, two different drafts of the same block.
;; NOTE: COMMITTED BEFORE ANY DRAFT IS TAKEN, AND THAT ORDER IS NOW LOAD
;; BEARING. `W11-overlay-wins` reads this block under w1's view; since
;; the view is pinned to the baseline of w1's own drafts, a block
;; committed AFTER them is not in it -- which is what
;; `W11-pinned-by-default` asserts three sections down, deliberately.
;; Creating it after the drafts, as this fixture used to, made the
;; overlay row read `#f` and look like an overlay defect.
(define second (cli "insert" "--title" "W11-B" "--text" "(define other 9)"))
(define second-id (block-id-of second))

(cli "write" "--writer" "w1" block-id "(define answer 2)")
(cli "write" "--writer" "w2" block-id "(define answer 3)")

;; ---- W11-working-view --------------------------------------------------------
;;
;; NEVER: EACH WRITER SEES ITS OWN DRAFT AND NOBODY ELSE'S. Two writers with a
;; draft of the same block is the case where an overlay that read the
;; wrong directory still looks like it works -- both answers would be
;; drafts, just the wrong one.
(define committed (cli "eval" (string-append "(block \"" block-id "\")")))
(define as-w1 (cli "eval" "--working" "--writer" "w1" (string-append "(block \"" block-id "\")")))
(define as-w2 (cli "eval" "--working" "--writer" "w2" (string-append "(block \"" block-id "\")")))

(want "W11-working-view each writer evaluates against its own draft"
      (list (field-of committed "src") (field-of as-w1 "src") (field-of as-w2 "src"))
      '("(define answer 1)" "(define answer 2)" "(define answer 3)"))

;; ---- W11-report-agrees -------------------------------------------------------
;;
;; NEVER: THE ANSWER MUST NOT CONTRADICT ITSELF. `(working-view …)` lists the
;; drafts the evaluation was given; the value is what it actually read.
;; NOTE: Measured with the overlay wired only into `--under`: the report said
;; the draft was in the view and the value was the COMMITTED source --
;; both halves "succeeded" and nothing was red. A row that only looked at
;; one of them cannot see that.
(want "W11-report-agrees what the view claims is what the evaluation read"
      (list (if (contains? as-w1 "(working-view \"w1\"") 'reported-w1 (list 'said as-w1))
            (if (equal? (field-of as-w1 "src") "(define answer 2)")
                'and-read-w1s-draft
                (list 'read (field-of as-w1 "src"))))
      '(reported-w1 and-read-w1s-draft))

;; NEVER: TWIN: WITHOUT `--working` THE REPORT CLAIMS NOTHING AND THE VALUE IS
;; COMMITTED. Without it, "the two agree" is satisfied by an answer that
;; reports nothing and reads nothing.
;; NOTE: THE EXPECTATION FOLLOWS A RULING, NOT THE CODE. This row used to
;; require `(working-view #f #f ())`: writer absent, CUT ABSENT, no
;; drafts. v218 settled that the cut position always carries the cut the
;; evaluation actually used, because that is the coordinate the answer
;; is reproducible from -- "at some cut" is not one. So what says "no
;; view was claimed" is the writer position and the empty draft table,
;; and the cut is expected to be there and to name the committed state.
(want "W11-report-agrees TWIN: with no view claimed, the committed source is read"
      (list (if (contains? committed "(working-view #f (") 'claimed-no-writer
                (list 'said committed))
            (if (contains? committed " ())") 'and-no-drafts (list 'said committed))
            (field-of committed "src"))
      '(claimed-no-writer and-no-drafts "(define answer 1)"))

;; ---- W11-overlay-wins --------------------------------------------------------
;;
;; NEVER: A BLOCK WITH NO DRAFT STILL READS AS COMMITTED, in the same answer
;; as one that has a draft. An overlay that replaced the whole view
;; rather than the blocks it covers would lose the rest of the store.
(want "W11-overlay-wins a block without a draft still reads as committed under a view"
      (field-of (cli "eval" "--working" "--writer" "w1"
                     (string-append "(block \"" second-id "\")"))
                "src")
      "(define other 9)")

;; ---- W11-snapshot-not-live ---------------------------------------------------
;;
;; NOTE: RENAMED. This row was called `W11-pinned-by-default`, which is the
;; name of a DIFFERENT property -- the one three rows below, where the
;; view is pinned to the baseline of the writer's own drafts. What this
;; one is about is that each run carries its own COPY rather than reading
;; a live directory. Two senses of "pinned", and the old name made this
;; row read as coverage of the other.
;;
;; NEVER: THE VIEW IS FIXED WHEN THE EVALUATION STARTS. The draft is changed
;; while nothing is running and read again: a second run sees the new
;; one, which is what says the first run saw a SNAPSHOT rather than a
;; live directory. NOTE: This row does not need a race to mean something --
;; what it pins is that each run carries its own copy.
(cli "write" "--writer" "w1" block-id "(define answer 22)")
(want "W11-snapshot-not-live a later draft is read by a later run, not an earlier one"
      (list (field-of as-w1 "src")
            (field-of (cli "eval" "--working" "--writer" "w1"
                           (string-append "(block \"" block-id "\")"))
                      "src"))
      '("(define answer 2)" "(define answer 22)"))

;; ---- W11-no-lock-during-run --------------------------------------------------
;;
;; NEVER: NOTHING IS HELD WHILE THE EVALUATION RUNS. A writer's draft lock is
;; taken for the copy and released; if it were held for the run, this
;; write -- issued while a slow evaluation is in flight -- would block
;; until the evaluation ended.
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
          "scheme --script ../cli.ss eval --working --writer w1 --timeout-ms 20000 "
          (quoted "(let loop ((i 0)) (if (< i 2000000000) (loop (+ i 1)) 'done))")
          " --store " store " --wire > " here "/slow.txt 2>&1 &"))
(system "sleep 1")

;; NEVER: THE PREMISE IS CHECKED, NOT ASSUMED. Most of a run's wall time is
;; two Chez processes loading this library from source, and the
;; EVALUATION itself is only the part after that. Measured: a loop to
;; forty million -- what this row used to launch -- evaluates in under
;; 50ms and the whole command returns in about a second, so the write
;; below was issued at roughly the moment the evaluation ended. A row
;; that says "while an evaluation is running" has to establish that one
;; was, or it reads the same whether a lock is held or not.
(want "W11-no-lock-during-run PREMISE: the evaluation has not answered yet"
      (if (string=? "" (file-text (string-append here "/slow.txt")))
          'still-running
          (list 'already-answered (file-text (string-append here "/slow.txt"))))
      'still-running)

(define during (cli "write" "--writer" "w1" block-id "(define answer 222)"))
(want "W11-no-lock-during-run a draft can be written while an evaluation is running"
      (if (contains? during "(ok (saved") 'wrote-during-the-run (list 'said during))
      'wrote-during-the-run)
(system "sleep 20")

;; NEVER: AND THE EVALUATION IT RAN BESIDE HAS TO HAVE SUCCEEDED. If the
;; write had disturbed it -- or if the launch had failed outright and
;; left an empty file, which is also what "still running" looks like --
;; the row above would still be green.
(want "W11-no-lock-during-run TWIN: the evaluation it ran beside answered normally"
      (let ((out (file-text (string-append here "/slow.txt"))))
        (if (contains? out "(values (done))") 'answered (list 'said out)))
      'answered)

;; ---- W11-pinned-by-default, W11-latest, W11-cut-overrides --------------------
;;
;; NEVER: A WRITER'S WORKING VIEW STANDS ON THE STATE ITS OWN DRAFTS RECORD.
;; A third writer takes a draft, and only then is a new block committed.
;; Pinned, that block is not in the view; `--latest` asks for the current
;; state and it is. NOTE: Until this batch the tree always evaluated at the
;; current state -- the `--latest` behaviour, as the default -- and
;; `--latest` itself was accepted by the parser and read by nothing.
(cli "write" "--writer" "w3" block-id "(define answer 3)")
(define after-draft (cli "insert" "--title" "W11-C" "--text" "(define later 7)"))
(define after-id (block-id-of after-draft))

(want "W11-pinned-by-default a commit made after the drafts is not in the view"
      (let ((out (cli "eval" "--working" "--writer" "w3"
                      (string-append "(block \"" after-id "\")"))))
        (if (contains? out "(values (#f))") 'not-in-view (list 'said out)))
      'not-in-view)

(want "W11-latest the same block is in the view when the pin is released"
      (let ((out (cli "eval" "--working" "--latest" "--writer" "w3"
                      (string-append "(block \"" after-id "\")"))))
        (if (contains? out "(define later 7)") 'in-view (list 'said out)))
      'in-view)

;; NEVER: TWIN: PINNING IS NOT "SEES NOTHING". The writer's own draft is in
;; the pinned view -- otherwise the row above is satisfied by a view that
;; is simply empty, which is a different defect answering to the same
;; reading.
(want "W11-pinned-by-default TWIN: the writer's own draft is in the pinned view"
      (let ((out (cli "eval" "--working" "--writer" "w3"
                      (string-append "(block \"" block-id "\")"))))
        (if (contains? out "(define answer 3)") 'own-draft-visible (list 'said out)))
      'own-draft-visible)

;; NEVER: AND THE REPORTED CUT IS THE ONE IT USED, so the two runs above are
;; distinguishable from the answer alone and not only by what they read.
(want "W11-pinned-by-default the reported cut differs between pinned and latest"
      (let ((pinned (cli "eval" "--working" "--writer" "w3" "(+ 1 2)"))
            (latest (cli "eval" "--working" "--latest" "--writer" "w3" "(+ 1 2)")))
        (if (and (contains? pinned "(working-view \"w3\" (")
                 (contains? latest "(working-view \"w3\" (")
                 (not (string=? (view-of pinned) (view-of latest))))
            'two-coordinates
            (list (view-of pinned) (view-of latest))))
      'two-coordinates)

;; NEVER: AN EXPLICIT COORDINATE WINS OVER BOTH, and asking for two at once
;; is refused rather than resolved by a precedence rule -- a rule would
;; make one of the two spellings silently do nothing, which is the defect
;; this batch removed.
(want "W11-cut-overrides an explicit --cut is used, and --cut with --latest is refused"
      (let ((pinned-cut (view-cut (cli "eval" "--working" "--writer" "w3" "(+ 1 2)")))
            (both (cli "eval" "--working" "--latest" "--cut" "((\"x\" . 1))" "(+ 1 2)")))
        (list (if (contains? (cli "eval" "--working" "--writer" "w3"
                                  "--cut" pinned-cut "(+ 1 2)")
                             (string-append "(working-view \"w3\" " pinned-cut))
                  'cut-honoured
                  (list 'said pinned-cut))
              (if (contains? both "(reason cut-and-latest)") 'both-refused (list 'said both))))
      '(cut-honoured both-refused))

;; ---- what is NOT covered here ------------------------------------------------
;;
;; NEVER: `W11-export-working` AND `W11-cut-plus-working` BELONG TO VERBS THIS
;; FIXTURE DOES NOT DRIVE (`export-code`, and `eval --cut` combined with a
;; view beyond the row above). They are named here so their absence is a
;; decision on the record rather than an oversight.

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\neval-working complete\n" rows bad)
(exit (if (zero? bad) 0 1))
