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
  (export parse-arguments argument-option argument-remove argument-positionals
          argument-strings argument-stdin)
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
    (append '("--store" "--actor" "--req" "--cursor")
      (case verb
        ((insert) '("--under" "--after" "--title" "--text"))
        ((set) '("--if-unchanged"))
        ((move) '("--after"))
        ((outline) '("--depth"))
        (else '()))))

  (define (flag-options verb)
    (case verb
      ((read) '("--md" "--recursive"))
      ((import-md) '("--allow-delete"))
      ((export-md) '("--with-ids"))
      (else '())))

  ;; `--` ENDS THE OPTIONS AND NOTHING AFTER IT IS ONE. It is kept as a
  ;; node rather than dropped, because `argument-strings` has to hand
  ;; back what the caller wrote: the request fingerprint is taken over
  ;; those strings, and a reader that silently removed a token would
  ;; give two spellings of one command two different identities.
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
           ((member (car xs) seen)
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
  (define (argument-stdin verb nodes read-text)
    (case verb
      ((batch) (append nodes (list (list 'pos (read-text)))))
      ((insert)
       (map (lambda (n)
              (if (and (eq? (car n) 'option) (string=? (cadr n) "--text")
                       (string=? (caddr n) "-"))
                  (list 'option "--text" (read-text))
                  n))
            nodes))
      (else nodes))))
