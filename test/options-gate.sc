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

;; Every verb's options, said once and checked in both directions.
;;
;; KEY: THE BUG THIS EXISTS FOR: `eval` had no entry in the option table,
;; so `eval --timeout-ms 5000 <source>` parsed `--timeout-ms` as a
;; POSITIONAL -- the evaluation's source became the string
;; "--timeout-ms" and the run failed with an exception rather than
;; refusing a limit. Nothing was red. Every cell that passed options to
;; `eval` passed ones the table happened to contain.
;;
;; NEVER: A GATE THAT ONLY LISTED THE VERBS WOULD NOT HAVE CAUGHT IT. `eval`
;; is not in `rpc-verbs` -- it is the CLI's own verb, like `serve` -- so
;; a gate walking the dispatcher's verbs would have reported everything
;; well while the defect sat in the one verb it could not see. The verb
;; set here is therefore `rpc-verbs` UNION the CLI's own two, and the
;; second of those unions is the whole point.
;;
;; NOTE: THE TABLE IS NOT THE ORACLE, THE PARSER IS. Whether an option is
;; accepted is measured by calling `parse-arguments` and reading what it
;; produced, not by re-reading the table this gate is meant to check. A
;; cell that restates the table checks its own copy of it.

(import (rnrs)
        (only (chezscheme) with-input-from-file system get-process-id call-with-input-file
              get-string-all file-exists?)
        (only (theourgia arguments) parse-arguments)
        (only (theourgia rpc) rpc-verbs))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (display (string-append "ok " label "\n"))
      (begin (set! bad (+ bad 1))
             (display (string-append "FAIL " label ": "))
             (write got) (display " WANT ") (write expected) (newline))))

;; NEVER: `want` IS A MACRO AND `caught` IS WHY. A procedural `want`
;; evaluates both arguments before the call, so a row whose expression
;; raises ENDS THE FILE -- the rows after it never run, and the ones
;; before it have already printed `ok`. The suite names fixtures that
;; lack this pair, and a new fixture has no business joining that list.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define scratch (string-append "/tmp/optgate-" (number->string (get-process-id))))
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

(define (read-data path)
  (with-input-from-file path
    (lambda ()
      (let loop ((acc '()))
        (let ((x (read)))
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

;; NEVER: THE HEADER OF A `define` IS NOT A CALL. `(define (usage form) ...)`
;; has a subform whose car is the symbol `usage`, and a walker that did
;; not know this would report the definition of `usage` itself as a usage
;; site it could not resolve -- one permanent unexplained entry, which is
;; how a count-based guard gets taught to ignore itself.
(define (walk form visit)
  (visit form)
  (cond
    ((and (pair? form) (eq? (car form) 'define) (pair? (cdr form))
          (pair? (cadr form)))
     (for-each (lambda (f) (walk f visit)) (cddr form)))
    ((pair? form)
     (walk (car form) visit)
     (walk (cdr form) visit))
    (else #f)))

(define (dashed? x) (and (string? x) (>= (string-length x) 2)
                         (string=? (substring x 0 2) "--")))

(define (strings-in form)
  (let ((acc '()))
    (walk form (lambda (f) (when (dashed? f) (set! acc (cons f acc)))))
    acc))

(define (quoted-list? x)
  (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (pair? (cadr x))))

;; ---- the three sources --------------------------------------------------------

(system (string-append "rm -rf " scratch "; mkdir -p " scratch "/store"))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
          "scheme --script ../cli.sc init --store " scratch "/store > /dev/null 2>&1"))

(define arguments-data (read-data "../arguments.sc"))
(define rpc-data (read-data "../rpc.sc"))
(define cli-data (read-data "../cli.sc"))

;; EVERY OPTION SPELLING THE TABLE KNOWS, as data. This is the set the
;; probe sweeps; it is read from the table because the table is where a
;; spelling is introduced, and every answer about it is then taken from
;; the parser.
(define all-spellings
  (let loop ((xs (strings-in arguments-data)) (out '()))
    (cond ((null? xs) out)
          ((member (car xs) out) (loop (cdr xs) out))
          (else (loop (cdr xs) (cons (car xs) out))))))

;; ACCEPTED FOR EVERY VERB, so naming one in a single verb's usage form
;; would say it is special to that verb.
(define common-options '("--store" "--wire" "--actor" "--req" "--cursor" "--socket"))

;; NOTE: NOT "TOP LEVEL": every definition in these files sits inside a
;; `library` form, so a scan of the file's own top level finds nothing
;; and reports each such site as unreadable -- which is what this did,
;; and the two `insert` sites showed up as two more unresolvable ones.
;; A lookup that cannot find anything and a lookup that found nothing
;; report the same thing.
(define (find-quoted-define name data)
  (let ((found #f))
    (walk data
          (lambda (f)
            (when (and (not found) (pair? f) (pair? (cdr f)))
              (cond
                ;; `(define <name> '<form>)`
                ((and (eq? (car f) 'define) (eq? (cadr f) name)
                      (pair? (cddr f)) (quoted-list? (caddr f)))
                 (set! found (cadr (caddr f))))
                ;; NOTE: `(let ((<name> '<form>)) ...)` COUNTS TOO. `publish`
                ;; binds its usage form in a `let` so that its two
                ;; refusals cannot drift apart -- which is the reason
                ;; this gate exists, done right -- and a resolver that
                ;; only knew `define` called the better-written one
                ;; unreadable.
                ((and (memq (car f) '(let let*)) (list? (cadr f)))
                 (for-each
                   (lambda (b)
                     (when (and (pair? b) (eq? (car b) name)
                                (pair? (cdr b)) (quoted-list? (cadr b)))
                       (set! found (cadr (cadr b)))))
                   (cadr f)))
                (else #f)))))
    found))

;; Usage forms, resolved where they can be and COUNTED where they cannot.
(define unresolved 0)

(define (collect-usage data)
  (let ((found '()))
    (walk data
          (lambda (f)
            (when (and (pair? f) (eq? (car f) 'usage) (pair? (cdr f))
                       (null? (cddr f)))
              (let ((arg (cadr f)))
                (cond
                  ((quoted-list? arg) (set! found (cons (cadr arg) found)))
                  ((symbol? arg)
                   (let ((resolved (find-quoted-define arg data)))
                     (if resolved
                         (set! found (cons resolved found))
                         (set! unresolved (+ unresolved 1)))))
                  (else (set! unresolved (+ unresolved 1))))))))
    found))

(define rpc-usage (collect-usage rpc-data))

;; NOTE: A USAGE FORM NEED NOT BE PASSED TO `usage`. `commit` has no shape
;; to get wrong, so its form is appended to whatever refusal came back,
;; and `eval` and `serve` are the CLI's own verbs and never reach the
;; dispatcher at all. All three are found the same way: a definition
;; named `<verb>-usage`. Looking only for call sites would have reported
;; every one of them as a verb that advertises nothing.
(define named-usage-forms
  (let loop ((vs (append (rpc-verbs) '(eval serve))) (out '()))
    (if (null? vs)
        out
        (let* ((name (string->symbol (string-append (symbol->string (car vs)) "-usage")))
               (found (or (find-quoted-define name rpc-data)
                          (find-quoted-define name cli-data))))
          (loop (cdr vs) (if found (cons found out) out))))))

(define usage-forms (append rpc-usage named-usage-forms))

(define (usage-for verb)
  (let loop ((xs usage-forms) (acc '()))
    (cond ((null? xs) acc)
          ((and (pair? (car xs)) (eq? (caar xs) verb))
           (loop (cdr xs) (cons (car xs) acc)))
          (else (loop (cdr xs) acc)))))

;; NOTE: ONE USAGE SITE IS BUILT AT RUN TIME: `parse-edge` serves both
;; `link` and `unlink` and spells its form from the verb it was called
;; with. The number is pinned rather than ignored, because a SECOND one
;; has to be either resolvable or admitted here deliberately -- an
;; unresolvable site reads exactly like a verb with no usage form at all,
;; and this count is the only thing that tells the two apart.
(define expected-unresolved 1)

;; ---- the verb set -------------------------------------------------------------

(define cli-own-verbs '(eval serve))
(define verbs-to-cover (append (rpc-verbs) cli-own-verbs))

;; NOTE: NAMED, WITH THE REASON IN THE NAME OF THE LIST, AND EACH LIST IS
;; CHECKED FOR EQUALITY rather than membership: closing one of these
;; gaps turns its row red until the entry is deleted. A list that only
;; had to contain the gaps would let a fixed one sit in it for ever.

;; Their usage form is spelled from the verb at run time, so it cannot be
;; read as data -- but it does exist, and the row below reads it out of
;; the running program instead of trusting this list.
(define verbs-whose-usage-is-built-at-run-time '(link unlink))

;; NEVER: EMPTY, AND IT STAYS THAT WAY. `commit` was the one entry: it
;; answered no usage form at all, because its handler delegates and it
;; has no arity to get wrong. It now appends one to its refusals. A verb
;; added here again is a verb whose spelling the program will not say.
(define verbs-with-no-usage-form-yet '())

(define verbs-whose-spelling-is-the-verb
  (append verbs-whose-usage-is-built-at-run-time verbs-with-no-usage-form-yet))

;; ---- A: every verb the program answers to is advertised somewhere -------------

(define verbs-without-usage
  (let loop ((xs verbs-to-cover) (out '()))
    (cond ((null? xs) (reverse out))
          ((or (pair? (usage-for (car xs)))
               (memq (car xs) verbs-whose-spelling-is-the-verb))
           (loop (cdr xs) out))
          (else (loop (cdr xs) (cons (car xs) out))))))

(want "GATE-A every verb the program answers to has a usage form"
      verbs-without-usage '())

;; NEVER: THE EXEMPTION IS NOT TAKEN ON TRUST. `link` and `unlink` are
;; excused from the static check because their form is built at run
;; time; this row makes the running program produce it. Without it the
;; exemption would be indistinguishable from "this verb advertises
;; nothing at all", which is exactly what the list beside it records.
(define link-usage-answer
  (let ((out (string-append scratch "/link.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../cli.sc link a b c d --store " scratch "/store --wire > "
              out " 2>&1"))
    (file-text out)))

(want "GATE-A the run-time usage form exists, and names the verb it was called with"
      (list (contains? link-usage-answer "(usage (link ")
            (contains? link-usage-answer "<from>")
            (contains? link-usage-answer "<rel>")
            (contains? link-usage-answer "<to>"))
      '(#t #t #t #t))

(want "GATE-A the two CLI verbs are in the set, and are the reason it is a union"
      (list (and (memq 'eval verbs-to-cover) #t)
            (and (memq 'serve verbs-to-cover) #t)
            (and (memq 'eval (rpc-verbs)) #t))
      '(#t #t #f))

(want "GATE-A every usage site could be read, or is one of the pinned run-time ones"
      unresolved expected-unresolved)

;; ---- B direction 1: what a usage form advertises, the parser accepts ---------

(define (accepted? verb option)
  (let ((nodes (parse-arguments verb (list option "x"))))
    (and (pair? nodes) (pair? (car nodes))
         (memq (caar nodes) '(option flag)) #t)))

(define advertised-but-refused
  (let loop ((vs verbs-to-cover) (out '()))
    (if (null? vs)
        (reverse out)
        (let ((opts (apply append (map strings-in (usage-for (car vs))))))
          (loop (cdr vs)
                (append (reverse
                          (map (lambda (o) (list (car vs) o))
                               (filter (lambda (o) (not (accepted? (car vs) o))) opts)))
                        out))))))

(want "GATE-B1 every option a usage form advertises is accepted by the parser"
      advertised-but-refused '())

;; ---- B direction 2: what the parser accepts, a usage form advertises ---------

(define accepted-but-unadvertised
  (let loop ((vs verbs-to-cover) (out '()))
    (if (null? vs)
        (reverse out)
        (let* ((verb (car vs))
               (advertised (apply append (map strings-in (usage-for verb))))
               (missing (filter (lambda (o)
                                  (and (not (memq verb verbs-with-no-usage-form-yet))
                                       (accepted? verb o)
                                       (not (member o common-options))
                                       (not (member o advertised))))
                                all-spellings)))
          (loop (cdr vs) (append (reverse (map (lambda (o) (list verb o)) missing)) out))))))

;; NEVER: EMPTY, AND COMPARED FOR EQUALITY SO IT STAYS EMPTY. Seventeen
;; options were accepted by the parser and named in no usage form: a
;; caller who misspelled one was told what was wrong with the value and
;; never what the verb accepts. They are all advertised now. The list is
;; kept, empty, because the row has to say something when the next
;; option is added to the table and to no usage form -- and because a
;; row comparing against nothing reads the same as a row that found
;; nothing.
(define options-not-yet-advertised '())

(want "GATE-B2 no verb-specific option the parser accepts goes unadvertised"
      accepted-but-unadvertised options-not-yet-advertised)

;; ---- B3: what a handler READS, the parser accepts -----------------------------
;;
;; KEY: THE DIRECTION THE FIRST TWO DO NOT COVER, AND THE ONE THE DEFECT
;; THIS GATE EXISTS FOR WAS IN. `eval`'s handler read `--timeout-ms`
;; while the table had no `eval` entry at all; `restore`'s handler read
;; `--writer` while the table listed it for `read`, `drafts` and
;; `discard` only. Both were found by reading, not by a row.
;;
;; NEVER: B1 AND B2 WERE GREEN THROUGHOUT. It is not that the gate did not
;; run -- it is that the gate did not ask this question. A usage form
;; and an option table can agree with each other perfectly while the
;; handler reaches for a third thing neither of them mentions.
;;
;; NOTE: WHAT A MISMATCH DOES IS QUIET. The token is not refused; it is
;; parsed as a POSITIONAL. So the verb sees extra positionals and
;; answers a usage form, or -- worse, and this is what `eval` did --
;; takes the option's own spelling as its argument and runs with it.

(define (find-from text needle from)
  (let ((n (string-length needle)) (m (string-length text)))
    (let scan ((j from))
      (cond ((> (+ j n) m) #f)
            ((string=? (substring text j (+ j n)) needle) j)
            (else (scan (+ j 1)))))))

(define rpc-text (call-with-input-file "../rpc.sc" get-string-all))
(define cli-text (call-with-input-file "../cli.sc" get-string-all))

;; NEVER: SCOPED TO THE VERB TABLE, NOT TO THE FILE. A scan for `(cons '`
;; across the whole of `rpc.sc` also matches `(cons 'items ...)` and
;; `(cons 'src text)` in the helpers, and then attributes to those
;; imaginary verbs every option read after them -- which is exactly what
;; the first version of this row did, reporting seven failures for two
;; verbs that do not exist. The table runs from `(define (verb-table)` to
;; the definition after it.
(define table-from (find-from rpc-text "(define (verb-table)" 0))
(define table-to (find-from rpc-text "(define verbs " (or table-from 0)))

(define (verb-spans)
  (let loop ((i (or table-from 0)) (out '()))
    (let ((at (find-from rpc-text "(cons '" i)))
      (if (or (not at) (and table-to (>= at table-to)))
          (reverse out)
          (let* ((from (+ at (string-length "(cons '")))
                 (end (let scan ((j from))
                        (cond ((>= j (string-length rpc-text)) j)
                              ((or (char=? (string-ref rpc-text j) #\space)
                                   (char=? (string-ref rpc-text j) #\newline)) j)
                              (else (scan (+ j 1))))))
                 (name (string->symbol (substring rpc-text from end))))
            (loop end (cons (cons name from) out)))))))

;; Every `(argument-option options "--x")` between one verb's `(cons '`
;; and the next one's belongs to that verb.
;; NOTE: THREE READER NAMES, NOT ONE. `eval`'s limits are read through
;; `eval-number`, not `argument-option` -- so a scanner that knew only
;; the latter missed `--timeout-ms`, `--memory-bytes` and
;; `--output-bytes`, which is to say it missed the exact option the
;; defect this row exists for was about. It caught `eval` anyway,
;; through three other spellings, and a row that finds the right answer
;; for the wrong reason is one tree-shape away from finding nothing.
(define reader-names '("argument-option" "eval-number"))

(define (options-read-in text from to)
  (apply append
         (map (lambda (r) (options-read-via r text from to)) reader-names)))

(define (options-read-via marker text from to)
  (let loop ((i from) (out '()))
    (let ((at (find-from text marker i)))
      (if (or (not at) (>= at to))
          out
          (let ((q (let scan ((j at))
                     (cond ((>= j to) #f)
                           ((char=? (string-ref text j) #\") j)
                           ((char=? (string-ref text j) #\)) #f)
                           (else (scan (+ j 1)))))))
            (if (not q)
                (loop (+ at (string-length marker)) out)
                (let ((e (let scan ((j (+ q 1)))
                           (cond ((>= j to) #f)
                                 ((char=? (string-ref text j) #\") j)
                                 (else (scan (+ j 1)))))))
                  (if (not e)
                      (loop (+ at (string-length marker)) out)
                      (let ((o (substring text (+ q 1) e)))
                        (loop e (if (or (member o out) (not (dashed? o))) out (cons o out))))))))))))

(define handler-reads
  (let* ((spans (verb-spans)) (n (length spans)))
    (let loop ((i 0) (out '()))
      (if (>= i n)
          (reverse out)
          (let* ((this (list-ref spans i))
                 (to (if (< (+ i 1) n)
                         (cdr (list-ref spans (+ i 1)))
                         (or table-to (string-length rpc-text))))
                 (opts (options-read-in rpc-text (cdr this) to)))
            (loop (+ i 1)
                  (append (reverse (map (lambda (o) (list (car this) o)) opts)) out)))))))

;; NOTE: `eval` IS NOT IN THAT TABLE -- it is the CLI's own verb -- and it
;; is the reason this row exists: its handler read `--timeout-ms` while
;; the option table had no `eval` entry at all. `cli.sc` is scanned as
;; one handler, and only the spellings `eval` itself takes are attributed
;; to it, because that file also reads options on behalf of other verbs.
(define cli-reads
  (map (lambda (o) (list 'eval o))
       (filter (lambda (o)
                 (member o '("--cut" "--under" "--working" "--latest" "--writer"
                             "--timeout-ms" "--memory-bytes" "--output-bytes")))
               (options-read-in cli-text 0 (string-length cli-text)))))

(define read-but-refused
  (filter (lambda (p) (not (accepted? (car p) (cadr p))))
          (append handler-reads cli-reads)))

(want "GATE-B3 every option a handler reads is one the parser accepts"
      read-but-refused '())

;; NEVER: AND THE SWEEP HAS TO HAVE FOUND SOMETHING TO SWEEP. An empty
;; `handler-reads` satisfies the row above and says nothing at all.
;; NOTE: THE TWO NAMED WITNESSES USED TO BE `(write "--writer")` AND
;; `(restore "--writer")`, and they are gone on purpose: the writer is no
;; longer an option a handler reads. It arrives as a dispatch position and
;; the ONE place that reads `--writer` is the dispatcher, which is not a
;; handler and is not swept. A control row names examples so that an empty
;; sweep cannot pass, so it needs examples that exist -- and when the code
;; moves, the examples move with it rather than the row being deleted.
(want "GATE-B3 the sweep found handlers reading options"
      (list (> (length (append handler-reads cli-reads)) 20)
            (and (member '(read "--working-info") handler-reads) #t)
            (and (member '(import-code "--allow-delete") handler-reads) #t))
      '(#t #t #t))

;; ---- the gate's own instrument ------------------------------------------------
;;
;; NEVER: AN EMPTY SWEEP IS WHAT A BROKEN GATE LOOKS LIKE. All four rows
;; above are satisfied by reading nothing at all: no verbs, no usage
;; forms, no spellings. These say the instrument was loaded.
(want "GATE-0 the gate read a verb list, usage forms and option spellings"
      (list (> (length verbs-to-cover) 20)
            (> (length usage-forms) 20)
            (> (length all-spellings) 20))
      '(#t #t #t))

;; NEVER: AND THAT IT CAN SAY NO. `--title` belongs to `insert`; if the probe
;; called every option accepted for every verb, both directions above
;; would be vacuous.
(want "GATE-0 the probe distinguishes a verb's own option from another's"
      (list (accepted? 'insert "--title")
            (accepted? 'outline "--title")
            (accepted? 'eval "--timeout-ms")
            (accepted? 'outline "--timeout-ms"))
      '(#t #f #t #f))

(system (string-append "rm -rf " scratch))

(display (string-append "rows: " (number->string rows) "\n"
                        (number->string bad) " failures\n"
                        "options-gate complete\n"))
(exit (if (zero? bad) 0 1))
