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
              get-string-all file-exists? load)
        (only (theourgia arguments) parse-arguments)
        (only (theourgia rpc) rpc-verbs verb-catalogue register-verbs!)
        (only (theourgia extensions) extension-verbs))

;; THE VERBS REGISTERED FROM OUTSIDE THE CORE TABLE, as core.sc and the daemon
;; register them: this census reads the registry itself, not a copy of it.
(register-verbs! extension-verbs)

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

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define scratch (string-append scratch-base "/optgate-" (number->string (get-process-id))))
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
;; ---- the definition a form sits in, so a site has an identity -------------
;;
;; NEVER: AN UNREADABLE SITE USED TO BE KEYED BY ITS FILE AND ITS SHAPE ALONE.
;; Two sites of one shape in one file were then one reading: moving a
;; spelling out of an exempt place and into a defective one left the
;; exemption list comparing equal, because the key could not tell the two
;; apart. The identity now carries WHERE, and where is the name of the
;; definition the site sits in.
;;
;; NOTE: A COUNTER WOULD NOT DO. `fresh-inline-key` is a counter, which is
;; right for inline places where only distinctness is wanted. An exemption
;; has to NAME its site, and a counter's value moves whenever the scan
;; order moves. The rule a key has to satisfy is: stable under changes that
;; have nothing to do with the site, and different when the site is
;; different. A name satisfies the first; the row asserting the keys are
;; pairwise distinct catches the second failing.
;;
;; NOTE: AND A FILE'S TOP LEVEL IS NOT ITS DEFINITIONS' TOP LEVEL. A library
;; is ONE datum holding every definition it has, so reading only a file's
;; top level would give every site in `rpc.sc` the same label and the keys
;; would all collide. Measured: `rpc.sc` and `store.sc` have one top-level
;; datum each; `core.sc` has 29 and `mcp/server.sc` 58. This descends
;; through `library` and `begin` to reach the definitions.
(define (definition-name form)
  (and (pair? form)
       (eq? (car form) 'define)
       (pair? (cdr form))
       (let ((head (cadr form)))
         (cond
           ((symbol? head) head)
           ((and (pair? head) (symbol? (car head))) (car head))
           (else #f)))))

(define (labelled-forms data)
  (let loop ((xs data) (acc '()))
    (cond
      ((not (pair? xs)) (reverse acc))
      ((and (list? (car xs)) (pair? (car xs)) (eq? (car (car xs)) 'library)
            (>= (length (car xs)) 5))
       (loop (cdr xs) (append (reverse (labelled-forms (cddddr (car xs)))) acc)))
      ((and (list? (car xs)) (pair? (car xs)) (eq? (car (car xs)) 'begin))
       (loop (cdr xs) (append (reverse (labelled-forms (cdr (car xs)))) acc)))
      (else
        (loop (cdr xs)
              (cons (cons (or (definition-name (car xs)) 'file-level) (car xs)) acc))))))

;; The visitor is called with the label and the form, so every reader below
;; knows which definition it is standing in without `walk` being changed.
(define (walk-file file visit)
  (for-each (lambda (labelled)
              (walk (cdr labelled) (lambda (f) (visit (car labelled) f))))
            (labelled-forms (data-of file))))

(define (walk form visit)
  (visit form)
  (cond
    ((and (pair? form) (eq? (car form) 'define) (pair? (cdr form))
          (pair? (cadr form)))
     (for-each (lambda (f) (walk f visit)) (cddr form)))
    ((pair? form)
     (walk (car form) visit)
     (walk (cdr form) visit))
    ;; NEVER: AND IT GOES INTO VECTORS. A form written inside a vector
    ;; literal was invisible: the walk fell through to `else` and returned,
    ;; so a spelling there was neither compared nor recorded. Nothing in the
    ;; tree puts one there today, which is exactly why it went unnoticed --
    ;; the walk's blind spot cost nothing until it did.
    ((vector? form)
     (for-each (lambda (f) (walk f visit)) (vector->list form)))
    (else #f)))

(define (dashed? x) (and (string? x) (>= (string-length x) 2)
                         (string=? (substring x 0 2) "--")))

(define (strings-in form)
  (let ((acc '()))
    (walk form (lambda (f) (when (dashed? f) (set! acc (cons f acc)))))
    acc))

(define (quoted-list? x)
  (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (pair? (cadr x))))

(define (quoted-symbol? x)
  (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (symbol? (cadr x))))

;; ---- the three sources --------------------------------------------------------

(system (string-append "rm -rf " scratch "; mkdir -p " scratch "/store"))
(system (string-append
          "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
          "scheme --script ../core.sc init --store " scratch "/store > /dev/null 2>&1"))

(define arguments-data (read-data "../arguments.sc"))
(define rpc-data (read-data "../rpc.sc"))
(define cli-data (read-data "../core.sc"))
(define daemon-data (read-data "../theourgiad.sc"))
;; The verbs registered from outside the core table write their forms here.
(define extension-data (read-data "../extensions.sc"))

;; EVERY OPTION SPELLING THE TABLE KNOWS, as data. This is the set the
;; probe sweeps; it is read from the table because the table is where a
;; spelling is introduced, and every answer about it is then taken from
;; the parser.
;; NOTE: AND THE OPTIONS OF THE VERBS REGISTERED FROM OUTSIDE THE CORE
;; TABLE, which are introduced in extensions.sc and nowhere in arguments.sc.
(define all-spellings
  (let loop ((xs (append (strings-in arguments-data) (filter dashed? (strings-in extension-data)))) (out '()))
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
;; NEVER: AND THE CLI'S OWN VERBS ARE NAMED ONCE. This loop used to spell
;; `'(eval serve)` for itself while `verbs-to-cover` spelled it again forty
;; lines below -- two copies of one list, in the file whose subject is two
;; copies of one form.
;;
;; NEVER: AND THE LIST IS READ FROM THE PROGRAMS, NOT TYPED HERE (F64). It
;; was the literal `'(eval serve)`, while `rpc-verbs` was derived from the
;; dispatcher's own table; `core.sc` dispatched `eval` and `theourgiad.sc`
;; dispatched `serve` each with a `string=?`, so a third verb added to either
;; joined no list, was compared against nothing, and turned no row red. Each
;; program now dispatches from one `own-verbs` table, and `own-verbs.sc`
;; reads both, raising rather than answering empty -- the F64 rows below
;; hold it to that.
;;
;; NEVER: EACH VERB'S FORM IS LOOKED FOR IN THE PROGRAM THAT ANSWERS IT.
;; Since F46 `serve-usage` is `theourgiad.sc`'s, and with `core.sc` the only
;; program read here, `serve` read as a verb with no usage form: GATE-A and
;; GATE-B2 went red on the split's own tree. That is F64, and the paragraph at
;; the partition row says what the partition does and does not promise.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))

(load (string-append script-dir "/own-verbs.sc"))

(define cli-own-verbs
  (append (own-verbs-of "../core.sc") (own-verbs-of "../theourgiad.sc")))

(define named-usage-forms
  (let loop ((vs (append (rpc-verbs) cli-own-verbs)) (out '()))
    (if (null? vs)
        out
        (let* ((name (string->symbol (string-append (symbol->string (car vs)) "-usage")))
               (found (or (find-quoted-define name rpc-data)
                          (find-quoted-define name cli-data)
                          (find-quoted-define name daemon-data)
                          (find-quoted-define name extension-data))))
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
;;
;; AND THIS ROW IS FOUR `contains?` CALLS, which is enough to say the form
;; EXISTS and not enough to say it agrees with anything. An option the
;; run-time form grew and the catalogue entry did not would leave all four
;; substrings present and this row green. Section C compares the whole form
;; against the catalogue entry; this row stays as it is, saying the smaller
;; thing it can actually say.
(define link-usage-answer
  (let ((out (string-append scratch "/link.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc link a b c d --store " scratch "/store --wire > "
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

;; ---- F64: the programs' own verbs are read, and the reader can say no -----
;;
;; NEVER: THE CENSUS IS ONLY AS GOOD AS ITS READER. A reader that answered an
;; empty list when it could not find the table would make every row above
;; read "nothing to check" as green, so each way of not finding it is a
;; row here: no define, a table naming nothing, an entry of another shape.
;; The samples are forms, not files: the reader is the same procedure
;; either way, and a sample on disk would be one more thing to clean up.
(display "== F64: the verbs each program answers itself ==\n")

(want "F64 each program's own verbs are read from its dispatch table, core.sc then theourgiad.sc"
      (list (own-verbs-of "../core.sc") (own-verbs-of "../theourgiad.sc"))
      '((eval) (serve)))

(want "F64 a program with no own-verbs define is a raise naming the file, not an empty census"
      (guard (e (#t (list 'RAISED (condition-message e) (condition-irritants e))))
        (own-verbs-in '((import (rnrs)) (define (main argv) argv)) "sample-without"))
      '(RAISED "no (define own-verbs ...) at the top level" ("sample-without")))

(want "F64 a table that names no verb is a raise, not an empty census"
      (own-verbs-in '((define own-verbs (list))) "sample-empty")
      '(RAISED "the own-verbs table names no verb"))

(want "F64 an entry that is not (cons '<verb> <procedure>) is a raise, not a skipped verb"
      (own-verbs-in '((define own-verbs (list (cons 'eval f) (list 'serve g)))) "sample-shape")
      '(RAISED "an entry is not (cons '<verb> <procedure>)"))

(want "F64 a quote with more than one datum in an entry is a raise, not the first datum"
      (own-verbs-in '((define own-verbs (list (cons (quote eval ignored) f)))) "sample-quote")
      '(RAISED "an entry is not (cons '<verb> <procedure>)"))

(want "F64 a define with more than one value form is a raise, not its first value"
      (own-verbs-in '((define own-verbs (list (cons 'eval f)) extra)) "sample-define")
      '(RAISED "own-verbs is not a (list ...) form"))

(want "F64 CONTROL: a well-formed sample table reads as its verbs, in order"
      (own-verbs-in '((define own-verbs (list (cons 'b f) (cons 'a g)))) "sample-good")
      '(b a))

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
(define cli-text (call-with-input-file "../core.sc" get-string-all))

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
;; the option table had no `eval` entry at all. `core.sc` is scanned as
;; one handler, and only the spellings `eval` itself takes are attributed
;; to it, because that file also reads options on behalf of other verbs.
(define cli-reads
  (map (lambda (o) (list 'eval o))
       (filter (lambda (o)
                 (member o '("--cut" "--under" "--working" "--latest" "--writer"
                             "--timeout-ms" "--memory-bytes" "--output-bytes")))
               (options-read-in cli-text 0 (string-length cli-text)))))

;; THE HANDLERS OF THE VERBS REGISTERED FROM OUTSIDE THE CORE TABLE live in
;; a library named by the entry, and an option read inside the handler's
;; own definition belongs to that verb: from `(define (<handler> ` to the
;; library's next top-level definition. Not the whole file: one library
;; may hold the handlers of two verbs, and an option one of them reads is
;; not the other's.
(define extension-reads
  (apply append
    (map (lambda (e)
           (let* ((lib (car (list-ref e 7)))
                  (handler (cdr (list-ref e 7)))
                  (text (call-with-input-file
                          (string-append "../" (symbol->string (cadr lib)) ".sc") get-string-all))
                  (from (find-from text (string-append "(define (" (symbol->string handler) " ") 0))
                  (next (and from (find-from text "\n  (define " (+ from 1))))
                  (to (or next (string-length text))))
             (if from
                 (map (lambda (o) (list (car e) o)) (options-read-in text from to))
                 (list (list (car e) 'NO-HANDLER-DEFINITION)))))
         extension-verbs)))

(define read-but-refused
  (filter (lambda (p) (not (accepted? (car p) (cadr p))))
          (append handler-reads cli-reads extension-reads)))

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
(want "GATE-B3 the sweep of the registered verbs' handlers found commitments reading its options"
      (map (lambda (o) (and (member (list 'commitments o) extension-reads) #t))
           '("--open" "--all" "--drifted" "--since" "--under"))
      '(#t #t #t #t #t))

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

;; ---- C: one usage form, written in more than one place ------------------------
;;
;; KEY: THE DIRECTIONS ABOVE COMPARE A FORM WITH THE PARSER. This one
;; compares a form WITH ITSELF, because a verb's usage form is written in
;; more than one place and the places can drift apart.
;;
;; NEVER: AND THE REASON B1 AND B2 MISSED `write` IS NOT THAT THEY UNIONED
;; THE CATALOGUE WITH THE HANDLER. They never read the catalogue at all:
;; `usage-forms` is `rpc-usage` (the `(usage ...)` call sites) appended to
;; `named-usage-forms` (the `<verb>-usage` definitions), and the verb table
;; is in neither. An earlier version of this comment said the union hid the
;; mismatch; the union is real, but it is a union of call sites with named
;; definitions. What hid `write`'s short catalogue entry is that the entry
;; was outside every reader this file had.
;;
;; A PLACE IS A SPELLING, AND A SPELLING IS IDENTIFIED BY WHERE IT IS
;; WRITTEN -- not by what it says.
;;
;;   * a form written out at a site -- a quoted list in a catalogue entry or
;;     in a `(usage '(...))` call -- is a place of its own. TWO SUCH SITES
;;     ARE TWO PLACES EVEN IF THEY SAY THE SAME THING TODAY, because either
;;     can be edited alone tomorrow, and comparing them is what this section
;;     is for.
;;   * a reference to a named form -- `(list 'commit commit-usage ...)`,
;;     `(usage insert-usage)` -- is not a place. It is a use of the place
;;     where that name is DEFINED. Any number of references to one binding
;;     are one spelling: there is only one thing to change.
;;
;; NEVER: AND `equal?` IS THE WRONG WAY TO DEDUPLICATE, which an earlier
;; version of this did by accident -- it keyed an inline place on the FORM,
;; so two separately written spellings that agreed today collapsed into one.
;; That drops the verb out of the compared set and into "written in one
;; place", telling a reader there is one copy to maintain while there are
;; two. The detection would still work once they diverged; the CENSUS would
;; be lying while everything was fine.
;;
;; NEVER: AND THERE IS NO ROW ABOUT LAYOUT, BECAUSE LAYOUT REACHES NO READER
;; OF THIS PROGRAM -- not merely because this gate cannot see it.
;;
;; `read` -- the verb -- has two spellings differing by eight spaces of
;; continuation indent and by nothing else, and it would be easy to write a
;; row asserting that no two spellings differ only in layout. Measured, here
;; is what happens to a usage form on its way to every consumer:
;;
;;     rpc.sc  (define (usage form) (list 'usage form))
;;         wrapped in a list; the printer lays the answer out afresh
;;     rpc.sc  usage-form-of returns the catalogue entry's second element
;;         a LIST
;;     rpc.sc  under-cwd-nodes consumes it by slot and by option spelling
;;         as data
;;
;; NEVER: AND THE CLAIM IS NEGATIVE, BECAUSE THAT IS THE ONE THAT WAS
;; MEASURED. An earlier wording said "every reader receives the form as
;; DATA". That is false: `mcp/server.sc` receives a tagged TEXT answer and
;; parses it with `read`, and `core.sc` copies an envelope's stdout string
;; straight out. What is true, and what those two do not disturb, is that
;; the text they receive was produced by `render.sc` with `write`.
;;
;; So, with its subject written out: NO CONSUMER OF THIS PROGRAM RECEIVES THE
;; SOURCE TEXT OF A USAGE FORM. The consumers that receive text receive the
;; printer's output, and how the source was laid out is not observable by any
;; of them.
;;
;; NEVER: AND THE SUBJECT IS THE PART THAT KEPT BEING WRONG. The first
;; wording said "every reader receives the form as data" -- false, two
;; receive text. The second said "no reader receives the source text" --
;; false too, because THIS FIXTURE reads the source text, a few hundred
;; lines below, to list the files it scans. A fixture is not a consumer; it
;; is the instrument. Three wordings, and the thing that was wrong each time
;; was who the claim was about.
;;
;; "Every X is Y" and "no X is Z" are not the same claim, only the second was
;; measured -- and a negative universal has to say WHO, not only WHAT.
;;
;; THE ONE PLACE A LAYOUT COPY REALLY EXISTS is the README, whose code blocks
;; are text and can drift from the source. That already has a name: it is
;; F52's question, and giving it a second mechanism here is the trade this
;; batch has spent the night refusing.
;;
;; So the absence is written where the row would have been: THERE IS
;; DELIBERATELY NO LAYOUT ROW, and the place to ask about layout is F52.

(define inline-counter 0)
(define (fresh-inline-key)
  (set! inline-counter (+ inline-counter 1))
  inline-counter)

;; ---- what this scanner could not read, counted rather than dropped --------
;;
;; NEVER: THE `(else #f)` BRANCHES USED TO THROW SPELLINGS AWAY IN SILENCE.
;; A catalogue entry or a `(usage ...)` site whose shape this file does not
;; know was skipped, and the verb went on looking well covered. Three rounds
;; running, the defect found was another instance of exactly that -- a form
;; named by a variable, a form built at run time, and now `serve`'s second
;; spelling, which sits inside a quoted error datum where the argument to
;; `usage` is a plain list rather than a quotation or a name.
;;
;; Teaching the scanner one more shape each time is a race it cannot win:
;; the syntax is open and the list of shapes is not. So the unrecognised
;; ones are COUNTED and the count is pinned. Any spelling this gate cannot
;; read now moves a number, and a number that moves is a red row.
;;
;; "I found them all" and "there are some I could not read" stop giving the
;; same reading.
(define unreadable-sites '())
(define unreadable-noted 0)

;; `file` is which source, `where` is which definition inside it, `what` is
;; the shape and the reason. The count is kept beside the list because the
;; two are compared: a list longer than the count means something built an
;; entry without coming through here.
(define (note-unreadable! file where what)
  (set! unreadable-noted (+ unreadable-noted 1))
  (set! unreadable-sites (cons (list file where what) unreadable-sites)))

;; ---- one constructor for a place, and a count of how many it made ---------
;;
;; NEVER: A PLACE USED TO BE BUILT IN SIX SEPARATE EXPRESSIONS. Four of them
;; pushed onto an accumulator, one used `map` and one used `cons` -- so a
;; census that counted the accumulator idiom would have read FOUR while the
;; truth was SIX, and read it without looking wrong. An instrument that
;; recognises a writing habit rather than the thing itself has the same
;; shape as every defect this section has found.
;;
;; So the count is taken at RUN TIME, by the constructor, and compared with
;; the length of what actually arrived. That equality holds whatever idiom
;; a collector uses, and it fails in both directions: a place built without
;; this function makes the list longer than the count, and a place made and
;; then dropped makes the count larger than the list.
;;
;; NOTE: THE SYNTHETIC PLACES IN THE ROWS AT THE END ARE NOT MADE HERE, on
;; purpose. They are inputs to `places-of-in`, not members of `all-places`,
;; and routing them through this counter would make the census disagree
;; with itself for a reason that has nothing to do with the collectors.
(define places-made '())
(define (new-place! verb site form)
  (let ((place (list verb site form)))
    (set! places-made (cons place places-made))
    place))

;; ---- every candidate a collector judged has to end somewhere --------------
;;
;; NEVER: A GUARD BEFORE THE CLASSIFIER IS A SECOND SILENT EXIT. The rule is
;; that one `cond` classifies and its `else` records, but a `when` or a
;; `cond` clause placed BEFORE it can still drop a candidate without a
;; word, and twice it did. Counting the classifier's exits cannot see that,
;; because the candidate never reached the classifier.
;;
;; So the accounting is done from outside: take the two counters before the
;; candidate is judged and again after, and if neither moved, record THAT.
;; It needs no second recording function and no split between kinds of
;; note -- any outcome at all counts, including one recorded on the way
;; through by something else.
(define (judging file where thunk)
  ;; NOTE: `eq?` ON THE LIST, NOT ITS LENGTH. `places-made` is the list of what
  ;; the constructor built, so a place added during the candidate replaces the
  ;; head and the two are no longer the same object. It answers the question
  ;; -- did anything get made here -- without walking anything.
  ;;
  ;; NOTE: AND IT RESTS ON A PREMISE, WHICH IS WHY THE PREMISE IS WRITTEN HERE.
  ;; The premise is that `new-place!` is the only place `places-made` is
  ;; written, and that all it does there is `cons`. The two ways it could
  ;; break are not equally dangerous:
  ;;
  ;;   a path that adds a place WITHOUT changing the head -- this reports a
  ;;   candidate with no outcome that did have one. Noisy, and safe.
  ;;
  ;;   a path that changes the head WITHOUT adding a place -- rebinding it to
  ;;   a fresh list of the same contents, say -- this reads as an outcome
  ;;   where none happened, and a real silent drop is covered up. Quiet, and
  ;;   the dangerous direction.
  ;;
  ;; The row comparing `all-places` with `places-made` as SETS is the guard on
  ;; that premise: a head that moved without a place being made leaves the two
  ;; sets unequal and names the difference. So these two are not independent
  ;; checks -- this one is cheap because that one is watching its premise.
  (let ((places-before places-made) (notes-before unreadable-noted))
    (let ((answer (thunk)))
      (when (and (eq? places-before places-made) (= notes-before unreadable-noted))
        (note-unreadable! file where '(candidate-produced-no-outcome)))
      answer)))

;; ---- the files this gate reads -------------------------------------------
;;
;; NEVER: AND IT IS NOT JUST TWO OF THEM. A spelling in any shipped source
;; was entirely outside the comparison while this read `rpc.sc` and `core.sc`
;; alone. The list is taken from the directory rather than typed here, so a
;; new source file joins it by existing; the row below pins how many there
;; are, because a list taken from a directory can also SHRINK.
(define source-file-list
  (let ((out (string-append scratch "/sources.txt")))
    (system (string-append "ls ../*.sc ../mcp/*.sc 2>/dev/null | sort > " out))
    (let loop ((ls (with-input-from-file out
                     (lambda ()
                       (let gather ((acc '()))
                         (let ((l (get-line (current-input-port))))
                           (if (eof-object? l) (reverse acc) (gather (cons l acc))))))))
               (acc '()))
      (if (null? ls) (reverse acc) (loop (cdr ls) (cons (car ls) acc))))))

;; NEVER: A FILE THAT WILL NOT PARSE IS NOT A FILE WITH NOTHING IN IT. The
;; read was caught and the failure became an empty data set, which every
;; reader below then treated as a source containing no usage forms -- the
;; very equivalence this section exists to break, inside the machinery built
;; to break it.
(define source-data
  (map (lambda (f)
         (let ((d (caught (read-data f))))
           (cond
             ((and (pair? d) (eq? (car d) 'RAISED))
              (note-unreadable! f 'the-whole-file (list 'source-would-not-parse (cadr d)))
              (cons f '()))
             (else (cons f d)))))
       source-file-list))

(define (data-of file) (let ((e (assoc file source-data))) (and e (cdr e))))

;; ---- one classification point, and its `else` records --------------------
;;
;; NEVER: EVERY BRANCH THAT DID NOT UNDERSTAND SOMETHING USED TO DROP IT.
;; The collector added a round ago exists to stop exactly that, and it was
;; itself built out of shape guards that skipped BEFORE recording: an outer
;; `when` that did not match, a `cond` whose `else` returned `#f`, a read
;; that failed into an empty file. Six of a reviewer's nine findings were
;; that one sentence.
;;
;; So there is one place that decides what a spelling is, and it has exactly
;; three outcomes:
;;
;;     a symbol         -> a reference to the binding of that name
;;     a quoted list    -> a form written here
;;     anything else    -> RECORDED, with the file and the shape
;;
;; NEVER: AND THE ASYMMETRY IS THE POINT. Recognising an open set of good
;; shapes cannot be done -- the syntax keeps growing and the list of shapes
;; does not. Refusing everything but two is easy, and it is the same job.
;; A checker's accepted set must be NARROW: Postel's law turned around for
;; the thing whose business is to object.
;;
;; NEVER: AND THIS MAKES ONE THING THIS GATE SAYS WEAKER, ON PURPOSE.
;;
;; A round ago, a partial usage form written inside a quoted error datum was
;; INTERPRETED here, and the gate said "these two spellings of `serve`
;; disagree" -- a strong claim. Now the same text is recorded as "I do not
;; interpret this spelling" -- a weaker one. Both are red; the second says
;; less.
;;
;; It says less because it is the most this reader can truthfully say.
;; `(usage (serve ...))` sitting inside a quotation and
;; `(usage (list verb ...))` calling a constructor are THE SAME OBJECT after
;; `read`: a list whose car is a symbol. A scanner that interpreted the
;; first would be interpreting the second as well, and would be reporting a
;; form that is never anybody's usage form.
;;
;; So the strong claim moved rather than vanished, and the division is:
;;
;;     THE STATIC GATE SAYS "THERE IS A SPELLING HERE I CANNOT READ".
;;     A RUNNING-PROGRAM CELL SAYS "AND HERE IS WHAT IT ACTUALLY SAID".
;;
;; `test/detach.sc`'s D-2b rows are the second half: they run the refusal and
;; compare the form it carries with `serve-usage`, whole. What was one
;; instrument's strong assertion is now two instruments, each saying the part
;; it can establish.
(define (classify-spelling file where what)
  (cond
    ((symbol? what) (list 'binding what))
    ((quoted-list? what) (list 'inline (cadr what)))
    (else (note-unreadable! file where (list 'unrecognised-spelling what)) #f)))

;; A catalogue entry names its form or writes it. Anything else is recorded.
;; NEVER: THE OUTER GUARD USED TO SKIP BEFORE RECORDING. An entry that looked
;; like a catalogue row but did not match every part of this `and` vanished.
;; The shape is split: anything that reaches the classifier is TAKEN to be a
;; catalogue entry, and if the rest does not fit, that is recorded.
;;
;; NEVER: AND THE SHAPE WAS NEVER THE CANDIDATE SET. Measured, the first time
;; the description branch recorded instead of dropping: `(list '<symbol> ...)`
;; matched SEVENTY-FIVE forms in `rpc.sc` -- `(list 'ok ...)`, `(list 'error
;; ...)`, `(list 'usage ...)`, every ordinary construction in the file -- and
;; the description-string test was what threw them out. So that test was not
;; a guard in front of a classifier at all; it was the REAL candidate test,
;; wearing the wrong clothes. Recording what it rejected turned a two-entry
;; exemption list into a seventy-seven entry one.
;;
;; The candidate set is now what it always meant: the entries inside the
;; catalogue's own definition. The same argument the section already makes
;; about files -- a shape-based scan over the tree finds shapes, not
;; catalogues -- applies one level further in, and the labels this gate now
;; carries are what make it sayable.
;;
;; NOTE: NOTHING PINS THIS NAME BY ITSELF, and nothing needs to. Misspell it
;; and no catalogue entry is found at all, which the population row reads as
;; a collapse rather than as a quiet zero.
;; NOTE: TWO DEFINITIONS, ONE PER KIND OF ENTRY: the built-in literal in
;; rpc.sc, and the entries registered from outside the core table, written
;; as data in extensions.sc. `verb-catalogue` is their union at run time and
;; holds no literal of its own.
(define catalogue-definition 'built-in-catalogue)
(define extension-catalogue-file "../extensions.sc")
(define extension-catalogue-definition 'extension-verbs)

(define (catalogue-places file definition)
  (let ((found '()))
    (walk-file file
          (lambda (where f)
            (when (and (eq? where definition)
                       (pair? f) (eq? (car f) 'list)
                       (pair? (cdr f)) (quoted-symbol? (cadr f))
                       (pair? (cddr f)))
              (judging file where (lambda ()
              (let ((verb (cadr (cadr f))))
                (cond
                  ;; a catalogue row carries a description after the form.
                  ;;
                  ;; NEVER: THIS BRANCH USED TO RETURN `#f` AND SAY NOTHING,
                  ;; directly under a comment promising that anything which
                  ;; did not fit would be recorded rather than dropped. An
                  ;; entry whose description was built rather than written
                  ;; -- a `string-append`, or a name -- left no place and no
                  ;; unreadable entry, so the verb went on looking covered.
                  ;; The guard ran BEFORE the one classification point, which
                  ;; is how a second silent exit gets built beside the first.
                  ((not (and (pair? (cdddr f)) (string? (cadddr f))))
                   (note-unreadable! file where
                                     (list 'catalogue-row-without-a-written-description
                                           verb (cdddr f)))
                   #f)
                  (else
                    (let ((place (classify-spelling file where (caddr f))))
                      (cond
                        ((not place) #f)
                        ((eq? (car place) 'inline)
                         (set! found (cons (new-place! verb
                                                       (list 'inline file (fresh-inline-key))
                                                       (cadr place))
                                           found)))
                        (else
                          (let ((bp (binding-place file where (cadr place))))
                            (when bp
                              (set! found (cons (new-place! verb
                                                            (list 'binding (car bp) (cadr place))
                                                            (cdr bp))
                                                found)))))))))))))))
    found))

(define (usage-places file)
  (let ((found '()))
    (walk-file file
          (lambda (where f)
            (when (and (pair? f) (eq? (car f) 'usage) (pair? (cdr f)) (null? (cddr f)))
              (judging file where (lambda ()
              (let ((place (classify-spelling file where (cadr f))))
                (when place
                  (if (eq? (car place) 'inline)
                      (let ((form (cadr place)))
                        (if (and (pair? form) (symbol? (car form)))
                            (set! found (cons (new-place! (car form)
                                                          (list 'inline file (fresh-inline-key))
                                                          form)
                                              found))
                            (note-unreadable! file where
                                              (list 'usage-form-has-no-verb form))))
                      ;; NEVER: AND THIS BRANCH RECORDS WHAT ITS TWIN RECORDS.
                      ;; The inline branch above notes a form with no verb at
                      ;; its head; this one used a `when` and dropped the same
                      ;; shape without a word. Two paths through one function
                      ;; gave one kind of bad input two different answers, and
                      ;; only one of them was countable.
                      ;;
                      ;; `binding-place` has already recorded its own reason
                      ;; when it returns #f, so that case is not noted twice.
                      (let ((bp (binding-place file where (cadr place))))
                        (cond
                          ((not bp) #f)
                          ((and (pair? (cdr bp)) (symbol? (car (cdr bp))))
                           (set! found (cons (new-place! (car (cdr bp))
                                                         (list 'binding (car bp) (cadr place))
                                                         (cdr bp))
                                             found)))
                          (else
                            (note-unreadable! file where
                                              (list 'usage-form-has-no-verb
                                                    (cdr bp))))))))))))))
    found))

;; A `<verb>-usage` definition is a place whether or not anything references
;; it: the form is written there.
;;
;; NEVER: AND A BINDING IS IDENTIFIED BY ITS FILE, NOT ONLY BY ITS NAME.
;; Two definitions of one name in different files -- or in different scopes
;; of one file -- are two places, and keying on the name alone merged them.
;; The file is in the key. Two definitions of ONE name in ONE file cannot be
;; told apart by this reader, so that case is COUNTED instead of merged
;; quietly: `binding-place` records it.
(define (binding-count file name)
  (let ((data (data-of file)) (n 0))
    (when (pair? data)
      (walk data
            (lambda (f)
              (cond
                ((and (pair? f) (eq? (car f) 'define) (pair? (cdr f)) (eq? (cadr f) name)
                      (pair? (cddr f)) (quoted-list? (caddr f)))
                 (set! n (+ n 1)))
                ((and (pair? f) (memq (car f) '(let let*)) (list? (cadr f)))
                 (for-each (lambda (b)
                             (when (and (pair? b) (eq? (car b) name)
                                        (pair? (cdr b)) (quoted-list? (cadr b)))
                               (set! n (+ n 1))))
                           (cadr f)))
                (else #f)))))
    n))

;; NEVER: A BINDING IS IDENTIFIED BY WHERE IT IS DEFINED, NOT BY WHERE IT IS
;; USED. This used to look in the using file and then fall back to the first
;; other file that defined the name, while the KEY recorded the file of the
;; USE -- so one binding referenced from two files became two places, and two
;; files defining the same name silently became whichever came first in the
;; directory listing. Both directions were wrong, in opposite ways.
;;
;; The homes of a name are all the scanned files that define it. Exactly one
;; is a place; none or several are recorded.
(define (binding-homes name)
  (let loop ((fs source-file-list) (acc '()))
    (cond
      ((null? fs) (reverse acc))
      ((and (pair? (data-of (car fs))) (find-quoted-define name (data-of (car fs))))
       (loop (cdr fs) (cons (car fs) acc)))
      (else (loop (cdr fs) acc)))))

;; Every home of a name, and the one reading neither caller can do without:
;; that a single file defines it more than once.
;;
;; NEVER: THIS WAS WRITTEN OUT TWICE AND THE COUNT WAS ONLY IN ONE OF THEM.
;; `binding-place` walked the files, and `named-places` walked them again in
;; a verbatim copy of the same loop under a different loop name -- so the
;; promise a few lines below, that two definitions of one name in one file
;; are COUNTED rather than merged quietly, was true of references and false
;; of the named definitions. Third time in this file that one rule had two
;; copies.
;;
;; NOTE: AND THE SHARED PIECE IS EXTRACTED RATHER THAN ONE CALLER ROUTED
;; THROUGH THE OTHER. The two answer different questions -- `binding-place`
;; asks which definition a REFERENCE resolves to and must refuse when that
;; is ambiguous, `named-places` asks where a form is WRITTEN and keeps every
;; home. Routing the second through the first would have turned two homes
;; from two places into none, which is the defect an earlier round removed.
(define (homes-of name)
  (let ((homes (binding-homes name)))
    (for-each (lambda (home)
                (when (> (binding-count home name) 1)
                  (note-unreadable! home name '(binding-defined-more-than-once))))
              homes)
    homes))

;; -> (<home-file> . <form>), or #f with the reason recorded.
(define (binding-place file where name)
  (let ((homes (homes-of name)))
    (cond
      ((null? homes)
       (note-unreadable! file where (list 'binding-not-defined-in-any-scanned-file name))
       #f)
      ((not (null? (cdr homes)))
       (note-unreadable! file where (list 'binding-defined-in-several-files name homes))
       #f)
      (else
        (cons (car homes) (find-quoted-define name (data-of (car homes))))))))

;; NEVER: AND EVERY DEFINITION IS KEPT, NOT THE FIRST. This walked the files
;; and returned at the first one that defined `<verb>-usage`, so a second
;; definition of the same name in another file was neither compared nor
;; recorded: it simply was not there. That is the rule this section is built
;; on -- a place is where a form is written -- applied to everything except
;; itself.
;; NOTE: NO `judging` HERE, AND THE REASON BELONGS BESIDE THE ABSENCE. Every
;; home yields exactly one place, always, so there is no branch that could
;; end without an outcome. A verb with NO named definition is not a dropped
;; candidate either: that is an answer, and the row naming the verbs no
;; form was found for is where it is read.
(define (named-places)
  (let loop ((vs verbs-to-cover) (out '()))
    (if (null? vs)
        out
        (let* ((name (string->symbol (string-append (symbol->string (car vs)) "-usage")))
               (homes (homes-of name)))
          (loop (cdr vs)
                (append (map (lambda (home)
                               (new-place! (car vs) (list 'binding home name)
                                           (find-quoted-define name (data-of home))))
                             homes)
                        out))))))

;; A FORM BUILT AT RUN TIME IS STILL A PLACE, and this file already had it
;; in its hand.
;;
;; NEVER: "THERE IS NOTHING TO COMPARE A CATALOGUE ENTRY AGAINST" WAS FALSE.
;; `link` and `unlink` have catalogue entries written as LITERALS,
;; `parse-edge` builds a second spelling at run time from the verb it was
;; called with, and GATE-A above already runs the program to fetch that
;; second spelling. GATE-A asks whether that form EXISTS and whether four
;; substrings appear in it; four `contains?` calls cannot see an option the
;; run-time form grew and the catalogue did not. Comparing the whole form is
;; what this section is for, so the form joins the places here.
(define (runtime-usage-form verb)
  (let ((out (string-append scratch "/rt-" (symbol->string verb) ".txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc " (symbol->string verb)
              " a b c d --store " scratch "/store --wire > " out " 2>&1"))
    (let ((answer (caught (with-input-from-file out read))))
      (and (pair? answer)
           (cond
             ((and (eq? (car answer) 'usage) (pair? (cdr answer)) (pair? (cadr answer)))
              (cadr answer))
             (else
               (let look ((xs answer))
                 (cond
                   ((not (pair? xs)) #f)
                   ((and (pair? (car xs)) (eq? (car (car xs)) 'usage)
                         (pair? (cdr (car xs))) (pair? (cadr (car xs))))
                    (cadr (car xs)))
                   (else (look (cdr xs)))))))))))

;; NEVER: AND A RUN THAT ANSWERED NOTHING USED TO LEAVE NO TRACE. If the
;; program failed to start, or answered a shape this cannot read, the verb
;; simply lost its second spelling and fell back to one place -- which is
;; exactly the reading the exemption below relies on NOT happening. The
;; cover disappearing has to be louder than the cover working.
(define runtime-places
  (let loop ((vs '(link unlink)) (out '()))
    (if (null? vs)
        out
        (let ((form (runtime-usage-form (car vs))))
          (loop (cdr vs)
                (cond
                  ((pair? form)
                   (cons (new-place! (car vs) (list 'runtime (car vs)) form) out))
                  (else
                    (note-unreadable! '<run-time> (car vs)
                                      (list 'runtime-form-not-read (car vs)))
                    out)))))))

;; NEVER: THE CATALOGUE IS ONE TABLE IN ONE FILE, AND THE SHAPE IS NOT.
;; `(list '<x> <y> "<string>" ...)` is an ordinary construction: run over
;; every source it matched `(list 'result identity "...")` in the MCP server
;; and reported `identity` as a usage form it could not read. The `(usage
;; ...)` sites are scanned everywhere, because a call to `usage` really can
;; be anywhere; the catalogue scan is scoped to the file the catalogue is
;; in, because a shape-based scan over the whole tree finds shapes, not
;; catalogues.
(define catalogue-file "../rpc.sc")

;; NOTE: HELD IN A NAME BECAUSE IT IS READ TWICE. Calling the collector again
;; for the closure row below would build every catalogue place a second time
;; and the constructor's count would stop matching the list -- the census
;; would report a defect that the act of measuring had caused.
(define catalogue-found
  (append (catalogue-places catalogue-file catalogue-definition)
          (catalogue-places extension-catalogue-file extension-catalogue-definition)))

(define all-places
  (append catalogue-found
          (apply append (map (lambda (e) (usage-places (car e))) source-data))
          (named-places)
          runtime-places))

;; The distinct spellings of one verb's form. Inline sites each keep their
;; own identity; references collapse onto the binding they name.
;;
;; NEVER: AND THE ROWS THAT PIN THIS RULE CALL THIS FUNCTION, NOT A COPY OF
;; IT. The first version of those rows had their own structurally identical
;; deduplicator, so that they could be given made-up places. Point the real
;; one's key back at the form -- the defect this rule exists to prevent, and
;; one that was really there -- and the copy would not notice: both rows
;; stayed green. What needed to be made-up was the DATA, not the function.
(define (places-of-in ps verb)
  (let loop ((l ps) (out '()))
    (cond
      ((null? l) (reverse out))
      ((not (eq? (car (car l)) verb)) (loop (cdr l) out))
      (else
        (let* ((p (car l)) (key (cadr p)))
          (loop (cdr l)
                (if (exists (lambda (q) (equal? (cadr q) key)) out)
                    out
                    (cons p out))))))))

(define (places-of-verb verb) (places-of-in all-places verb))

(define places-of (map (lambda (v) (cons v (places-of-verb v))) verbs-to-cover))

(define (verbs-with n)
  (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
             (map car (filter (lambda (e) (n (length (cdr e)))) places-of))))

(define compared          (verbs-with (lambda (k) (> k 1))))
(define written-in-one    (verbs-with (lambda (k) (= k 1))))
(define no-form-found     (verbs-with (lambda (k) (= k 0))))

(define disagreeing
  (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
             (map car
                  (filter (lambda (e)
                            (and (> (length (cdr e)) 1)
                                 (let ((fs (map caddr (cdr e))))
                                   (not (for-all (lambda (f) (equal? f (car fs))) fs)))))
                          places-of))))

;; NEVER: THE THREE LISTS ARE A PARTITION, AND THE SUM IS PINNED. The
;; version before this published two lists built from two different
;; conditions, and a verb could satisfy neither: `insert` and `outline` sat
;; in a bucket that nothing named and nothing compared. The prose beside the
;; second list claimed that a verb leaving the compared set would turn a row
;; red, and for one way of leaving it that was true -- for the other it was
;; not.
;;
;; Counting every verb into exactly one of three lists and asserting the
;; total makes that bucket impossible by construction rather than by anyone
;; remembering to look for it.
;;
;; NEVER: AND HERE IS WHAT THE TOTAL DOES NOT PROMISE. It says the three
;; lists PARTITION `verbs-to-cover`. It does not say `verbs-to-cover` covers
;; the program. `rpc-verbs` is derived from the dispatcher's table, but the
;; CLI's own two are a hand-written literal, and `core.sc` dispatches them
;; with one `string=?` apiece and keeps no table: a third verb added there
;; would be in no list, compared against nothing, and the total would still
;; balance, because both sides of it are taken from the same list.
;;
;; So this row closes one way out and not the other. Deriving the CLI's
;; verbs from `core.sc` the way `catalogue-places` derives from `rpc.sc` is
;; F64. Written here because a row asserting a total reads like "every verb
;; has been accounted for", and this one accounts for every verb THIS GATE
;; KNOWS ABOUT.
(want "GATE-C every verb lands in exactly one of the three lists"
      (list (+ (length compared) (length written-in-one) (length no-form-found))
            (length verbs-to-cover))
      (list 44 44))

;; NEVER: AND THE NUMBER IS ABOUT THE SHIPPED SOURCES, NOT ABOUT THE WORLD.
;; `write`'s form is written in FOUR places: its catalogue entry, its
;; handler's refusal, the README's section heading, and the code block under
;; that heading -- and the heading was the one that was wrong, three options
;; short of the block one blank line below it. This gate reads no
;; documentation and will not notice when the README drifts.
;;
;; That sentence was in this file when the section was first written and was
;; lost when the section was rewritten a round later. Three rounds passed
;; and nobody noticed; the reviewer did. When a passage is rewritten, its
;; old claims have to be checked off against the new one: reading only the
;; new text cannot show what went missing.
;;
;; CONTROL: THE POPULATION, NOT ONLY THE DISAGREEMENTS. "No verb disagrees"
;; is also what a scanner that found nothing reports, and it is what happens
;; when a verb quietly LOSES one of its two spellings. The number below is
;; what says the question was asked of anybody.
;;
;; NEVER: AND IT CAUGHT EXACTLY THAT, ON THE FIRST RUN OF THIS SECTION. The
;; population read ZERO while the disagreement row below it read green --
;; the catalogue scanner asked `quoted-list?` of the verb, which is a quoted
;; SYMBOL, so it matched nothing and the disagreement list was empty because
;; there was nothing in it. Without this row the gate would have been green
;; from the moment it was written, and green for ever, over an empty set.
;; That run's output is kept beside this delivery as
;; `GATE-C-RED-population-zero.out`; its line 9 is the reading, `0 WANT 29`.
;;
;; NOTE: NAMING THE FILE IS THE WHOLE OF THIS SENTENCE'S WEIGHT. No row
;; opens it, so deleting it turns nothing red -- a reader who wants the
;; evidence has to be told where it is, and the previous wording said only
;; that it existed somewhere.
(want "GATE-C the verbs whose form is written in more than one place"
      (length compared)
      32)

(want "GATE-C no verb's two spellings of its usage form disagree"
      disagreeing
      '())

;; AND THE OTHER TWO LISTS ARE NAMED, not counted: a verb moving between
;; them is a red row rather than a number that happens to stay the same.
;;
;; NEVER: THE REASONS ARE POSITIVE, AND THEY NAME A PLACE. They used to be
;; of the form "there is no other spelling" -- a negative universal, which is
;; precisely the claim reading code cannot establish. Three turned out false
;; in three rounds: `link`, `unlink`, and then `serve`, whose second
;; spelling sat inside a quoted error datum. Each time the sentence was true
;; about what had been looked at and false about the world.
;;
;; So each entry names WHERE THE ONE PLACE THIS GATE FOUND IS, and the
;; completeness question -- is there another the gate cannot read -- belongs
;; to the row that counts what it could not read.
;;
;;   commit   rpc.sc:499  `commit-usage`
;;   outline  rpc.sc:502  `outline-usage`
;;   insert   rpc.sc:505  `insert-usage`
;;   eval     rpc.sc      `eval-usage`
;;   supply   rpc.sc:586  `supply-usage`
;;     Each is reached from the catalogue entry and from the `(usage <name>)`
;;     sites BY NAME, so several arrivals are one place to edit.
;;     `commit-usage` is never passed to `usage` at all -- `commit` appends
;;     it to whatever refusal came back -- which is why it has no call site,
;;     and is not why it has one place. `eval-usage` is core.sc's refusals'
;;     form too, imported from rpc.sc; eval's catalogue entry has route
;;     `child` and the dispatcher never answers it.
;;   serve    theourgiad.sc:55  `serve-usage`
;;     The daemon program's own verb; it never reaches the dispatcher's
;;     catalogue. Since F46 `serve` is the daemon program's, `theourgiad.sc`.
;;     `serve` was in this list for a round with the wrong reason: a second
;;     spelling in `core.sc` gave a partial form in which `--detach` was not
;;     bracketed, so it read as required rather than optional -- a false
;;     statement about the verb rather than a fragment of a true one. That
;;     refusal names `serve-usage` now.
;;
;; An earlier version of this list also held `link` and `unlink`, with the
;; reason "their form is built at run time, so there is nothing to compare a
;; catalogue entry against".
;;
;; NEVER: AND THE PREMISE OF THAT SENTENCE IS TRUE. `parse-edge` really does
;; build their form at run time. What is false is the CONCLUSION drawn from
;; it, and it is false for three independent reasons: they have catalogue
;; entries, written as literals; the run-time spelling is a second form that
;; can drift from those entries; and this file was already fetching that
;; second spelling, for a GATE-A row, while the sentence said there was
;; nothing to fetch. Any one of the three would be enough. They are compared
;; now.
;;
;; A retraction can overshoot as easily as the sentence it retracts, and is
;; less likely to be read again.
(want "GATE-C the verbs whose form is written in exactly one place, named"
      written-in-one
      '(commit commitments eval insert names outline read serve supply tasks template uses))

(want "GATE-C the verbs with no usage form this gate can find, named"
      no-form-found
      '())

;; ---- the shapes this gate could not read, pinned ---------------------------
;;
;; NEVER: THIS IS THE ROW THAT MAKES THE OTHERS MEAN SOMETHING. Every list
;; above is a statement about the spellings this file MANAGED TO READ. Three
;; rounds running, the defect was a spelling it could not read and dropped
;; without a word, and each time the lists above went on looking complete.
;;
;; A shape the scanner does not know now lands here instead of nowhere. The
;; expectation is the empty list, so the first unreadable spelling anyone
;; writes turns this red -- and the row says which file and what shape.
;; NAMED, WITH THE REASON BESIDE EACH, AND CHECKED FOR EQUALITY rather than
;; for membership -- the idiom this file already uses for its other
;; exemptions. Closing one of these turns the row red until its entry is
;; deleted, and a NEW shape the gate cannot read turns it red as well.
;;
;;   ../core.sc  (theourgia <verb> ...)
;;     The program's own top-level usage, printed when no verb was given.
;;     `theourgia` is the PROGRAM, not a verb, so it is not in the verb
;;     census and there is no second spelling of it to compare against.
;;
;;   ../rpc.sc  (list verb '<from> '<rel> '<to>)
;;     `parse-edge` building `link` and `unlink`'s form at run time from the
;;     verb it was called with.
;;
;;     NEVER: AND THE REASON SAYS WHERE IT IS READ INSTEAD, not merely that
;;     it cannot be read here. This spelling IS compared: `runtime-places`
;;     runs the program and takes the form that comes back, which is the
;;     form a caller actually sees. An exemption whose reason is "I cannot
;;     read this" stays true after the thing that DID read it has been
;;     deleted -- the exemption never expires and the coverage is gone. This
;;     one names its cover.
;;
;;     Measured: with the run-time fetch turned off, `link` and `unlink`
;;     fall back to one place each and two rows go red --
;;     `more than one place: 29 WANT 31` and the one-place list gaining
;;     `link` and `unlink`. So the cover is pinned, not asserted.
;; NOTE: AND EACH ENTRY NAMES ITS SITE, not only its file and its shape.
;; Two sites of one shape in one file used to be one reading, so a spelling
;; moved out of an exempt place and into a defective one left this list
;; comparing equal while the exemption had quietly changed what it covered.
;; The middle field is the definition the site sits in.
(define spellings-this-gate-does-not-interpret
  (list (list "../core.sc" 'main
              (list 'unrecognised-spelling '(theourgia <verb> ...)))
        (list "../rpc.sc" 'parse-edge
              (list 'unrecognised-spelling
                    (list 'list 'verb ''<from> ''<rel> ''<to>)))))

(want "GATE-C the only spellings this gate does not interpret are the two named ones"
      (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
                 unreadable-sites)
      (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
                 spellings-this-gate-does-not-interpret))

;; ---- what each row below is a function of, said before any of them ---------
;;
;; NEVER: SIX ROWS IN THIS FILE CANNOT BE TURNED RED BY ANY CHANGE TO THE
;; PROGRAM, and only one of them said so. A reader counting the rows here
;; counts them among the ones watching `rpc.sc` and `core.sc`, and five of them
;; are not: their subject is THIS FILE's own scanner, or nothing but this
;; file. They are not empty -- each was turned red by a variant applied to the
;; collectors -- but a row that can only be moved by editing the instrument
;; has to say that where it stands, or it is read as cover it does not give.
;;
;; NEVER: AND THE COUNT WAS FIVE HERE FOR A ROUND. The row it left out was the
;; unreadable-entry twin, which is the one row that had already been annotated
;; as a tripwire -- so the sentence introducing the annotations undercounted
;; by exactly the row that proved the annotations were needed.
;;
;; THE SIX ARE, by the title each carries: the constructor census, its
;; unreadable-entry twin, the no-outcome row, the two deduplication witnesses,
;; and the vector witness. Each of those carries a line naming what it is a
;; function of. The other rows in this file are moved by the program and do
;; not carry one.
;;
;;   this scanner    -- it changes when the collectors in this file change
;;   synthetic only  -- its input and its function are both written here
;;
;; ---- the census: everything in those lists came through one door ----------
;;
;; NEVER: "THERE IS ONE CLASSIFICATION POINT" WAS PROSE. The rule was written
;; down and then broken three times in the same file -- a guard before the
;; classifier, a branch whose twin recorded and it did not, and a second
;; copy of the homes loop that skipped the count. A sentence cannot notice
;; any of that. A number can.
;;
;; NOTE: AND THE NUMBER IS TAKEN AT RUN TIME. Counting the places built in
;; the SOURCE of this file would have read four, because two of the six
;; collectors build theirs with `map` and `cons` instead of pushing onto an
;; accumulator -- an instrument that recognises a writing habit rather than
;; the thing itself, which is the shape of every defect this section found.
;; The constructor counts what it made; the list says what arrived; the two
;; are compared. Any idiom at all is covered, and it fails both ways.
;; NEVER: THIS COMPARED TWO TOTALS, AND A TOTAL IS CONSERVED BY A PAIR OF
;; OPPOSITE ERRORS. Dropping one constructed place and adding one built
;; elsewhere leaves the two counts equal, and so does replacing a place with a
;; different one, and so does duplicating one while losing another. The row
;; would have said `agree` for all of them.
;;
;; It is the same shape that cost this round two attempts at the parentheses:
;; one closer too many in one procedure and one too few in the next, the total
;; conserved, the depth reading identical to the frozen tree's, and the
;; scanner silent twice. That lesson was written down in this round's delivery
;; note BEFORE this instrument was built with the same shape in it.
;;
;; So the constructor keeps WHAT it made, not how many, and the two are
;; compared as sets of written forms. A pair of opposite errors now changes
;; the contents even when it does not change the count.
;; A FUNCTION OF: this scanner. No change to the program moves it, because
;; every completed `new-place!` call is retained by its caller and reaches
;; `all-places`. It moves when a collector stops going through the
;; constructor, which is what it is for. Variant: build a place with `map`
;; in `named-places` and it reports the five that did not come through.
;; NEVER: WHETHER IT GOES RED AND WHAT IT THEN SAYS WERE TWO DIFFERENT
;; SEMANTICS. The decision compares two SORTED LISTS, which counts repeats,
;; so a place constructed twice and delivered once does turn this red. The
;; two lists it printed afterwards were filtered with `member`, which does
;; not: in exactly that case both came back EMPTY and the row's answer was
;; two empty lists beside two different counts. The row was right about
;; whether, and useless about who -- and those are two properties of one row,
;; each needing its own case.
;;
;; The difference is taken one occurrence at a time now, so a repeat that is
;; not matched by a repeat is named like anything else.
(want "GATE-C every place in the list came through the one constructor"
      (let* ((written (lambda (xs)
                        (list-sort string<? (map (lambda (x) (format "~s" x)) xs))))
             (without-one (lambda (x ys)
                            (let loop ((ys ys) (out '()))
                              (cond ((null? ys) (reverse out))
                                    ((string=? (car ys) x) (append (reverse out) (cdr ys)))
                                    (else (loop (cdr ys) (cons (car ys) out)))))))
             (minus (lambda (xs ys)
                      (let loop ((xs xs) (ys ys) (out '()))
                        (cond ((null? xs) (reverse out))
                              ((member (car xs) ys)
                               (loop (cdr xs) (without-one (car xs) ys) out))
                              (else (loop (cdr xs) ys (cons (car xs) out))))))))
        (let ((in-the-list (written all-places))
              (constructed (written places-made)))
          (if (equal? in-the-list constructed)
              'the-same
              (list 'only-in-the-list (minus in-the-list constructed)
                    'only-constructed (minus constructed in-the-list)
                    'counts (list (length in-the-list) (length constructed))))))
      'the-same)

;; NOTE: THIS TWIN IS A TRIPWIRE, NOT A MEASUREMENT, AND SAYING SO IS THE
;; POINT. One function both appends the entry and bumps the count, so the
;; two agree by construction today and this row cannot fail. It is here so
;; that the day somebody adds a second way to record, the equality is
;; already written down and stops holding. A row that can only fail in the
;; future is worth having as long as nobody reads it as evidence now.
;;
;; A FUNCTION OF: this scanner, and today not even that -- see below.
;;
;; NOTE: AND IT IS A COUNT, WHICH THE ROW ABOVE STOPPED BEING. That is a
;; weaker comparison for the reason stated there -- a total survives a pair
;; of opposite errors -- and it is left as a count deliberately, because a
;; second recorder would change the count. NEVER: THIS SAID A SECOND RECORDER
;; WAS THE ONLY THING THAT COULD BREAK IT, which is false in both directions:
;; deleting an entry, or changing this recorder's increment, breaks it without
;; a second recorder, and a second recorder that updated both fields would not
;; break it at all. It is not evidence that the entries are right; the row
;; pinning the exemption list by name is what asks that.
(want "GATE-C TWIN: and every unreadable entry came through the one that records"
      (if (= (length unreadable-sites) unreadable-noted)
          'agree
          (list 'entries (length unreadable-sites) 'noted unreadable-noted))
      'agree)

;; NEVER: A GUARD IN FRONT OF THE CLASSIFIER IS INVISIBLE TO A COUNT OF THE
;; CLASSIFIER'S EXITS, because the candidate never reaches it. `judging`
;; watches both counters across each candidate and records the candidates
;; that moved neither. This row is the reading: the answer is the list, so
;; a drop names its file and its definition instead of being a number that
;; went up.
;; A FUNCTION OF: this scanner. Every branch reachable inside the two
;; `judging` wrappers already produces an outcome today, so the program
;; cannot move this row. It moves when an edit here adds a branch that does
;; not, which is the failure this whole section keeps finding.
;;
;; NOTE: AND IT CANNOT SEE WHAT NEVER BECAME A CANDIDATE. The shape filters
;; at the head of both collectors run OUTSIDE the wrapper, so a catalogue row
;; or a usage site written with `apply`, `cons`, a macro, a quasiquotation or
;; a quoted table is not judged at all and does not appear here. That is the
;; remaining exit, it is named in the scope note at the end of this section,
;; and the answer to it is F65 rather than another shape.
(want "GATE-C no candidate was judged and then dropped without an outcome"
      (filter (lambda (e) (equal? (caddr e) '(candidate-produced-no-outcome)))
              unreadable-sites)
      '())

;; NEVER: AND THE KEYS HAVE TO BE TELLING APART WHAT THEY CLAIM TO. A name
;; is stable under changes that have nothing to do with a site, which is
;; what a counter was not; it is not enough on its own, because two
;; unreadable sites inside ONE definition would share it -- the same
;; collision one level finer. So the condition says so itself rather than
;; being guessed at: an ordinal is NOT pre-installed, because an ordinal
;; would also move when a site is added inside a definition, and that
;; movement is one that ought to be seen.
(want "GATE-C no two unreadable entries share a key"
      (let loop ((xs unreadable-sites) (seen '()) (clashes '()))
        (cond
          ((null? xs) (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
                                 clashes))
          (else
            (let ((key (list (car (car xs)) (cadr (car xs)))))
              (loop (cdr xs)
                    (cons key seen)
                    (if (member key seen) (cons key clashes) clashes))))))
      '())

;; AND WHICH FILES WERE READ, BY NAME. The list comes from a directory
;; listing, so it grows by itself when a source file is added -- and it
;; shrinks by itself too. A COUNT would catch a file going missing and would
;; not catch one being swapped for another: forty-four is forty-four either
;; way. That is the lesson this section learned about verbs an hour ago, and
;; it applies to the instrument as much as to the subject.
;;
;; THE EXPECTATION COMES FROM THE RULE, NOT FROM A RUN. The rule is: every
;; `.sc` beside `core.sc`, plus the MCP server's own directory -- the shipped
;; sources a usage form could be written in. A file appearing or vanishing
;; turns this red, and the answer is to decide whether the rule should have
;; included it, not to paste the new list in.
;;
;; NEVER: AND THE RULE IS TWO NON-RECURSIVE GLOBS. A source in some other
;; subdirectory is invisible to the scan AND to this row, which is the shape
;; of every reach problem in this section: the population is derived from
;; something hand-written rather than from the repository. It is the same
;; hole as the CLI's own verb list, and it has the same answer -- derive the
;; population from what the repository holds -- which is F64. Measured by the
;; main session: `git ls-files '*.sc'` is 191, of which 43 at the top level
;; and 1 under `mcp/` are the shipped sources and 147 are fixtures.
;; NEVER: NARROWING A CANDIDATE SET IS ITSELF A WAY OF GOING QUIET. The scan
;; is scoped to the catalogue's own definition, which is what makes the
;; recording above mean anything -- and it also means an entry written
;; ANYWHERE ELSE is now invisible to this gate, with nothing to say so. A
;; narrowed population needs a closure assertion taken from somewhere else,
;; or the narrowing is the next silent exit.
;;
;; `rpc-verbs` is the dispatcher's own table. It is not this file's rule and
;; not derived from this scan, so it answers independently: an entry moved
;; out of the definition drops its verb out of the scan and this row names
;; it, and a misspelt definition name finds nothing at all rather than
;; quietly finding less.
;;
;; NOTE: PLUS THE ENTRIES WHOSE ROUTE IS `child`. Such a verb is carried out
;; by a program the caller runs -- `eval`, by `core.sc` -- so it is in the
;; catalogue and not in the dispatcher's table, by design. It is taken from
;; the running catalogue's route field, not named here.
(define child-only-verbs
  (map car (filter (lambda (e) (eq? 'child (list-ref e 4))) (verb-catalogue))))

(want "GATE-C the catalogue scan found an entry for exactly the verbs the program has"
      (let ((by-name (lambda (xs)
                       (list-sort (lambda (a b) (string<? (symbol->string a)
                                                          (symbol->string b)))
                                  xs))))
        (let ((found (by-name (map car catalogue-found)))
              (theirs (by-name (append (rpc-verbs) child-only-verbs))))
          (if (equal? found theirs)
              'the-same
              (list 'scanned found 'dispatcher theirs))))
      'the-same)

;; ---- what this section does NOT reach, said as narrowly as it is true ------
;;
;; NEVER: EVERY ROUND OF THIS SECTION ENDED BY TEACHING THE SCANNER ONE MORE
;; SHAPE, and the syntax is open while the list of shapes is not. What follows
;; is the reach as it actually stands, so that the next reader starts from it
;; instead of rediscovering it. The answer to the first three is F65 -- one
;; named binding per form and every other site referring to it by name, which
;; turns those into "is this position a symbol" -- and not another shape added
;; here. The fourth is about which files are scanned at all, which F65 does
;; not touch; it is F64. NEVER: THIS SAID F65 WAS THE ANSWER TO ALL OF IT,
;; two lines above the sentence that assigns the fourth one elsewhere.
;;
;;   THE SHAPE FILTERS RUN BEFORE THE ACCOUNTING. A catalogue row that does
;;   not begin with `list` and a quoted symbol, or a usage site that is not a
;;   two-element form headed by `usage`, never becomes a candidate, so the
;;   no-outcome row cannot report it. Measured example: `rpc.sc`'s commit
;;   refusal writes `(list 'usage commit-usage)`, whose head is `list`;
;;   replacing its second argument with a separately written quoted form is
;;   invisible to all four collectors.
;;
;;   NEVER: AND THIS USED TO LIST `apply`, MACROS, QUASIQUOTATIONS AND QUOTED
;;   TABLES as things that escape. A review ran the actual collector against
;;   them: a quoted and a quasiquoted `(usage ...)` both produce unreadable
;;   entries, and a macro template containing one produces a place, because
;;   the walker descends through those containers without tracking quotation.
;;   The sentence had been written from what sounded like the same family
;;   rather than from what the walker does.
;;
;;   THE CLOSURE ROW COMPARES VERB NAMES, NOT CONSTRUCTIONS. It sorts the
;;   names the catalogue scan found and compares them with `(rpc-verbs)`,
;;   without removing repeats, so what it requires is that the two name lists
;;   match exactly. It says nothing about whether a scanned construction is
;;   the one the catalogue returns: the walker reads syntax and does not
;;   follow values.
;;
;;   BINDING RESOLUTION HAS NO SCOPES. `find-quoted-define` searches by
;;   spelling and returns what it meets first, so which definition a shadowed
;;   name resolves to follows traversal order rather than lexical scope. Two
;;   quoted definitions of one name are recorded as ambiguous, but only for
;;   names a collector actually asks `homes-of` to resolve; this is not a
;;   general duplicate-definition scan.
;;
;;   THE POPULATION COMES FROM TWO NON-RECURSIVE GLOBS AND A VERB LIST. A
;;   source in another subdirectory is outside the scan and outside the row
;;   that names the scanned files. That is F64, not F65.
;;
;; refusal.sc since F100b M3a (L); incomplete.sc since F77c (L). NOTE: a named list like this is what F64
;; replaced elsewhere; that this one should come from the tree is queued.
;; eval-runner.sc and eval-runner-exec.sc: the runner path of eval --lang (L).
;; completion.sc: the judgement of a plan's completion, entered by the store when one is reached (L).
(want "GATE-C the scanned source files, named"
      ;; Compared as sorted BASENAMES, so the expectation does not depend on
      ;; which directory a source sits in: `mcp/server.sc` sorts among the
      ;; paths by its directory and among the names by its name, and the
      ;; question this row asks is which files, not where they are.
      (list-sort string<?
                 (map (lambda (f)
                        (let loop ((i (- (string-length f) 1)))
                          (cond ((< i 0) f)
                                ((char=? (string-ref f i) #\/)
                                 (substring f (+ i 1) (string-length f)))
                                (else (loop (- i 1))))))
                      source-file-list))
      '("admission.sc" "answers.sc" "arguments.sc" "baseline.sc" "client.sc"
        "code-markers.sc" "code-project.sc" "code-suggest.sc" "commitments.sc" "completion.sc" "core.sc" "crc32.sc"
        "daemon.sc" "datum-code.sc" "datum-match.sc" "datum-metadata.sc"
        "datum-project.sc" "derived.sc" "digest.sc" "eval-admission.sc" "eval-context.sc" "eval-runner-exec.sc" "eval-runner.sc"
        "eval-supervise.sc"
        "eval-worker.sc" "evidence-index.sc" "extensions.sc" "ffi.sc" "field-reading.sc" "incomplete.sc" "json.sc"
        "languages.sc" "log.sc" "markers.sc" "md.sc" "name-use.sc" "net.sc"
        "operation-packet.sc" "platform-numbers.sc" "proc.sc" "project.sc" "reduce.sc" "refusal.sc" "regex.sc"
        "render.sc" "request.sc" "rpc.sc" "sched.sc" "server.sc"
        "source-lex.sc" "store.sc" "tasks.sc" "template-read.sc" "template.sc" "templates.sc" "text-code.sc" "theourgia.sc" "theourgiad.sc" "trace.sc"
        "view.sc" "wire.sc" "working.sc"))

;; ---- the two halves of "a place is where it is written", each with a case --
;;
;; The rule has two directions and each needs an input that could kill it.
;; Without them the rule is a paragraph: it would go on reading correctly
;; while the code did something else.
;;
;; NEVER: THE FIRST HALF IS WHERE AN EARLIER VERSION FAILED. It keyed an
;; inline place on the form itself, so two separately written spellings that
;; agree today counted as one -- the `equal?` deduplication this file
;; forbids, arrived at by accident rather than on purpose.
;;
;; The two rows below feed made-up places to `places-of-in`, which is the
;; function the gate itself runs on. A row that fed them to a second copy of
;; the rule would answer the same whether the rule was right or only its
;; copy was -- and this is the only place the source-identity rule is
;; pinned at all.
(define two-identical-literals
  (list (list 'probe (list 'inline 9001) '(probe <a>))
        (list 'probe (list 'inline 9002) '(probe <a>))))

(define one-binding-three-arrivals
  (list (list 'probe (list 'binding 'probe-usage) '(probe <a>))
        (list 'probe (list 'binding 'probe-usage) '(probe <a>))
        (list 'probe (list 'binding 'probe-usage) '(probe <a>))))

;; A FUNCTION OF: synthetic only. Both rows below feed made-up places to
;; `places-of-in`, which is defined in this file. No value derived from the
;; program takes part, so nothing in `rpc.sc` or `core.sc` can move them. They
;; are unit tests of this file's own deduplication rule, and they are here
;; because that rule is the one an earlier round got wrong.
(want "GATE-C two spellings written separately are two places even when identical today"
      (length (places-of-in two-identical-literals 'probe))
      2)

(want "GATE-C TWIN: one binding reached three times is one place"
      (length (places-of-in one-binding-three-arrivals 'probe))
      1)

;; NEVER: AND THE VECTOR BRANCH HAD NO CASE BEHIND IT AT ALL. `walk` descends
;; into vectors because a form written inside a vector literal used to be
;; invisible -- and the comment there says, correctly, that nothing in the
;; tree puts one there today. That is precisely the condition under which a
;; branch can be deleted with every row staying green: a guard whose case
;; does not exist is not protecting anything anyone can see.
;;
;; The two rows above cannot stand in for this one: they hand made-up
;; places straight to `places-of-in` and never call `walk` at all.
;;
;; NOTE: THE INPUT IS SYNTHETIC BECAUSE THE TREE HAS NONE, which is the
;; reason for the row rather than an excuse for it. The reading that makes
;; it mean something is the deletion: with the vector branch taken out this
;; answers 0.
;; A FUNCTION OF: synthetic only. The walker and the vector are both written
;; here. The program cannot move this row -- nothing in the tree puts a form
;; in a vector, which is exactly why the branch had no witness at all until
;; this round. It moves when the branch is removed from this file.
(want "GATE-C walk reaches a form written inside a vector"
      (let ((seen '()))
        (walk (vector '(usage '(probe <a>)) 'other)
              (lambda (f)
                (when (and (pair? f) (eq? (car f) 'usage))
                  (set! seen (cons f seen)))))
        (length seen))
      1)

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
