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

;; One scan over the argument list, and every token consumed once.
;;
;; WHAT THIS REPLACES, AND WHY IT HAD TO BE REPLACED. The old reader
;; pulled one option at a time, each pass searching the WHOLE list -- so
;; a token that spelled an option was found again wherever it sat,
;; including where it sat as another option's value. Measured on the
;; command line before this landed:
;;
;;     insert --under root --title "--actor" --text body
;;       answered   ok
;;       the title  "body"
;;       the actor  "--text"
;;
;; Three wrong things and no word to the caller. The last one is the
;; serious one: a record's actor is evidence of who wrote it, and a
;; reader that can be made to assign it from a neighbouring token
;; forges that evidence -- quietly, on a command that reports success.
;;
;; ONE SCAN IS THE WHOLE OF THE FIX. A value is taken as the option's
;; value the moment the option is recognised, and it is never looked at
;; again; a second `--title` is a duplicate rather than a silently
;; ignored one, and an option at the end of the list with nothing after
;; it is missing its value rather than absent.
;;
;; THE NODES ARE SHARED WITH THE RPC LAYER. Both entrances parse once
;; into the same nodes and read options from them; neither re-scans the
;; other's result. That is what stops the two from disagreeing about
;; which token was which -- an argument list whose meaning depends on
;; which layer looked at it is the shape this library exists to remove.
(library (theourgia arguments)
  (export argument-option-list parse-arguments argument-option argument-remove argument-positionals
          argument-strings argument-stdin argument-wants-stdin?
          argument-stdin-placeholder?)
  (import (rnrs base) (rnrs lists))

  ;; WARNING -- THESE TWO TABLES ARE A SECOND PLACE THAT KNOWS THE COMMAND LINE.
  ;; A verb that grows an option and is not added here does not fail --
  ;; it mis-parses: the new option becomes a positional and its value
  ;; becomes another positional, so the verb answers its usage line and
  ;; the caller is told the form is wrong when the form was right. The
  ;; usage forms in `rpc.ss` are the other place; they and these must
  ;; name the same options, and nothing enforces it but a case.
  ;;
  ;; The split is by arity and not by verb: an option that takes a value
  ;; must consume the token after it, and a flag must not -- reading a
  ;; flag as a value takes the id that follows it and leaves the list
  ;; looking empty, which is how `read <id> --md` used to lose its id.
  (define (value-options verb)
    (append '("--store" "--actor" "--req" "--cursor" "--socket")
      (case verb
        ;; NEVER: WHERE A DETACHED DAEMON'S OUTPUT GOES IS THE CLIENT'S
        ;; DECISION, so it is a value the client passes and not something
        ;; the daemon works out for itself. The client has to read that
        ;; file afterwards to report why a start failed; two sides
        ;; computing the path separately is the shape that already cost a
        ;; day here, when a socket path was derived twice and the two
        ;; derivations disagreed about a store that did not exist yet.
        ((serve) '("--log"))
        (else '()))
      (case verb
        ;; NOTE: `--keywords` IS A VALUE OPTION AND ITS VALUE IS TEXT. The
        ;; field holds what the caller typed, commas and all; the
        ;; splitting happens in `search`, which is the only reader that
        ;; needs tokens. Parsing it here would mean the store held a
        ;; normalised form and `read` could not give back what was sent.
        ((insert) '("--under" "--after" "--title" "--text" "--keywords"))
        ((set) '("--if-unchanged" "--based-on"))
        ((move) '("--after"))
        ((outline) '("--depth"))
        ((split-suggest) '("--output"))
        ((def) '("--under"))
        ((commit) '("--writer" "--working-version"))
        ((write) '("--writer" "--based-on" "--working-cut" "--working-parent-writer" "--working-parent"))
        ;; NOTE: `restore` IS IN THIS GROUP BECAUSE ITS HANDLER READS
        ;; `--writer`, and it was not. The token then parsed as a
        ;; POSITIONAL: `restore <version> --writer w1` arrived with three
        ;; positionals, failed the arity check and answered a usage form,
        ;; so a version could never be restored into a named writer's
        ;; slot from the command line. Same shape as the `eval` entry
        ;; below -- a handler reading an option this table does not list.
        ((read drafts discard restore) '("--writer"))
        ;; NOTE: ADDED WITH THE SCHEME SUPERVISOR, AND THIS TABLE IS WHY IT
        ;; HAD TO BE. Measured before it was: `eval --timeout-ms 999999`
        ;; parsed `--timeout-ms` as a POSITIONAL, so the source of the
        ;; evaluation became the string "--timeout-ms" and the run failed
        ;; with an exception instead of refusing an out-of-range limit.
        ;; The warning at the top of this file describes exactly that.
        ((eval) '("--cut" "--under" "--timeout-ms" "--memory-bytes"
                  "--output-bytes" "--writer"))
        (else '()))))

  ;; AN OPTION THAT MAY BE GIVEN MORE THAN ONCE.
  ;;
  ;; Every other option is refused on repetition, and that is right: two
  ;; `--title`s leave "which one" without an answer. A commit's versions
  ;; are different -- there is one per block it names, and they are a
  ;; SET, so repeating the option is how the set is spelled. Each value
  ;; is `<block>=<version>`; a commit naming exactly one block may give
  ;; the bare version, which is what the single-block callers already
  ;; wrote.
  (define (repeatable-options verb)
    (case verb ((commit) '("--working-version")) (else '())))

  (define (flag-options verb)
    (cons "--wire" (case verb
      ;; NOTE: `--detach` CHANGES WHAT THE PROCESS DOES BEFORE IT SERVES, not
      ;; how it answers: it leaves the caller's session, drops the
      ;; caller's stdio and then behaves exactly like the foreground
      ;; form. A `serve` started by hand must keep its terminal, so this
      ;; is a flag the CLIENT passes and a person does not.
      ((serve) '("--detach"))
      ((outline) '("--with-keywords"))
      ((read) '("--md" "--recursive" "--working" "--working-info"))
      ;; `--working` names the view and `--writer` names whose; `--latest`
      ;; releases the pin.
      ;;
      ;; NOTE: THIS COMMENT USED TO CLAIM `--latest` WAS "the same spelling
      ;; the read verbs already use". It was not: `latest` appeared
      ;; nowhere in the dispatcher, the flag was accepted by this table
      ;; and read by no code at all, and the DEFAULT was the floating
      ;; behaviour the flag was meant to ask for. A sentence taken from
      ;; a design and written down as a fact about the tree.
      ((eval) '("--working" "--latest"))
      ((write) '("--rebase"))
      ((import-md) '("--allow-delete"))
      ((import-code) '("--allow-delete" "--datum"))
      ((export-code) '("--raw" "--datum"))
      ((export-md) '("--with-ids"))
      (else '()))))

  ;; `--` ENDS THE OPTIONS AND NOTHING AFTER IT IS ONE. It is kept as a
  ;; node rather than dropped, because `argument-strings` has to hand
  ;; back what the caller wrote: the request fingerprint is taken over
  ;; those strings.
  ;; NOTE: AND THE CONSEQUENCE RUNS THE OTHER WAY FROM WHAT THIS COMMENT
  ;; USED TO SAY. It claimed that dropping the token would give two
  ;; spellings of one command two identities; dropping it does the
  ;; opposite -- `("b" "text")` and `("--" "b" "text")` would come back as
  ;; the same strings and so take the SAME fingerprint. Keeping the node
  ;; is what lets the two spellings stay distinguishable.
  (define (parse-arguments verb args)
    (let loop ((xs args) (seen '()) (out '()) (literal? #f))
      (cond
        ((null? xs) (reverse out))
        (literal? (loop (cdr xs) seen (cons (list 'pos (car xs)) out) #t))
        ((string=? (car xs) "--")
         (loop (cdr xs) seen (cons '(end) out) #t))
        ((or (member (car xs) (value-options verb))
             (member (car xs) (flag-options verb)))
         (cond
           ((and (member (car xs) seen)
                 (not (member (car xs) (repeatable-options verb))))
            (list 'error 'bad-request 'duplicate-option (car xs)))
           ((member (car xs) (flag-options verb))
            (loop (cdr xs) (cons (car xs) seen) (cons (list 'flag (car xs)) out) #f))
           ((null? (cdr xs))
            (list 'error 'bad-request 'missing-option-value (car xs)))
           (else (loop (cddr xs) (cons (car xs) seen)
                       (cons (list 'option (car xs) (cadr xs)) out) #f))))
        (else (loop (cdr xs) seen (cons (list 'pos (car xs)) out) #f)))))

  (define (argument-option nodes name)
    (let ((n (find (lambda (n) (and (memq (car n) '(option flag))
                                    (string=? (cadr n) name)))
                   nodes)))
      (and n (if (eq? (car n) 'flag) #t (caddr n)))))

  ;; EVERY VALUE GIVEN FOR ONE OPTION, in the order it was written.
  (define (argument-option-list nodes name)
    (map caddr
         (filter (lambda (n) (and (eq? 'option (car n)) (string=? (cadr n) name)))
                 nodes)))

  (define (argument-remove nodes names)
    (filter (lambda (n) (not (and (memq (car n) '(option flag))
                                  (member (cadr n) names))))
            nodes))

  (define (argument-positionals nodes)
    (map cadr (filter (lambda (n) (eq? (car n) 'pos)) nodes)))

  ;; THE SPELLING AND THE ORDER ARE PART OF THE REQUEST. A request's
  ;; fingerprint is taken over the argument strings, so this has to
  ;; reproduce what the caller wrote and not a normalised form of it --
  ;; two spellings that fingerprint alike would make two different
  ;; requests one, and a retry of either would be answered as a replay
  ;; of the other.
  (define (argument-strings nodes)
    (apply append
           (map (lambda (n)
                  (case (car n)
                    ((pos flag) (list (cadr n)))
                    ((option) (cdr n))
                    ((end) '("--"))))
                nodes)))

  ;; STDIN IS AN ARGUMENT THAT ARRIVES ANOTHER WAY. A request carries
  ;; strings, so the two places this command reads from stdin are
  ;; resolved into strings here -- the library never learns that a
  ;; terminal was involved, and a caller over a socket sends the same
  ;; request with the same bytes in it.
  ;;
  ;; THE OTHER ARGUMENTS SURVIVE. An earlier version replaced the whole
  ;; argument list, which meant `--req` and `--cursor` never reached the
  ;; dispatcher -- so the one verb whose retry is most worth protecting
  ;; was the one verb with no protection. Adding to the nodes leaves
  ;; every option in place.
  ;;
  ;; AND BOTH SOURCES AT ONCE IS AN ERROR, not a silent choice. With
  ;; positional text AND stdin the verb sees two positionals and answers
  ;; its usage line: two ways of saying what the batch is cannot both be
  ;; honoured, and picking one quietly would mean the text a caller
  ;; actually passed was discarded without a word.
  ;; KEY: ASKED OF THE RULE ITSELF, so there is no second list of which
  ;; verbs read standard input. The reader handed in records that it was
  ;; called and answers an empty string; whatever `argument-stdin` builds
  ;; with that is thrown away, and what is kept is whether it asked. A
  ;; verb added to the table below is answered for here without anyone
  ;; remembering to.
  (define (argument-wants-stdin? verb nodes)
    (let ((asked #f))
      (argument-stdin verb nodes (lambda () (set! asked #t) ""))
      asked))

  ;; NEVER: AN UNFILLED `-`, WHICH IS THE CASE THAT USED TO BE SILENT. A verb
  ;; that reads standard input and was sent none has two shapes. `batch`
  ;; with nothing on either side answers its own usage line, which names
  ;; what is missing and always has. `write <id> -` did not: the
  ;; placeholder stayed in the arguments and was stored AS the text, and
  ;; the answer said the write had succeeded.
  ;;
  ;; NOTE: ASKED OF `argument-stdin` ITSELF rather than from a second list of
  ;; verbs: the input is offered as a sentinel, and the question is whether
  ;; a `-` that was there has been replaced by it.
  (define (argument-stdin-placeholder? verb nodes)
    (let* ((sentinel "\x0;stdin-placeholder")
           (filled (argument-stdin verb nodes (lambda () sentinel))))
      (let loop ((a nodes) (b filled))
        (cond
          ((or (null? a) (null? b)) #f)
          ((and (eq? 'pos (car (car a))) (string=? (cadr (car a)) "-")
                (eq? 'pos (car (car b))) (string=? (cadr (car b)) sentinel))
           #t)
          ((and (eq? 'option (car (car a))) (= 3 (length (car a)))
                (string=? (caddr (car a)) "-")
                (eq? 'option (car (car b))) (= 3 (length (car b)))
                (string=? (caddr (car b)) sentinel))
           #t)
          (else (loop (cdr a) (cdr b)))))))

  (define (argument-stdin verb nodes read-text)
    (case verb
      ;; NEVER: APPENDED WHETHER OR NOT SOMETHING IS ALREADY THERE, and that is
      ;; the rule: intents given BOTH as an argument and on standard input
      ;; are two answers to one question, so the verb sees two positionals
      ;; and answers its usage line. Made conditional, the argument won and
      ;; what was piped in was dropped without a word -- `cli3` pins this,
      ;; and it was right to.
      ((batch) (append nodes (list (list 'pos (read-text)))))
      ((def) (if (= (length (argument-positionals nodes)) 1)
                 (append nodes (list (list 'pos (read-text)))) nodes))
      ;; NEVER: THE BYTES ARE THE SECOND POSITIONAL, AND ONLY THAT ONE.
      ;; Written as "every positional spelled `-`", a block whose id is
      ;; literally `-` had its NAME replaced by the caller's standard
      ;; input -- so the write went to a block named by whatever had been
      ;; piped in, which is neither what was asked nor an error.
      ((write)
       (let loop ((ns nodes) (pos 0) (out '()))
         (cond
           ((null? ns) (reverse out))
           ((eq? (car (car ns)) 'pos)
            (loop (cdr ns) (+ pos 1)
                  (cons (if (and (= pos 1) (string=? (cadr (car ns)) "-"))
                            (list 'pos (read-text))
                            (car ns))
                        out)))
           (else (loop (cdr ns) pos (cons (car ns) out))))))
      ((insert)
       (map (lambda (n)
              (if (and (eq? (car n) 'option) (string=? (cadr n) "--text")
                       (string=? (caddr n) "-"))
                  (list 'option "--text" (read-text))
                  n))
            nodes))
      (else nodes))))
