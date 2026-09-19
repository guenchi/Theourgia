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

;; `describe`: what the verbs are, for something that has to ask.
;;
;; KEY: THE CATALOGUE IS A SECOND PLACE THAT KNOWS THE VERBS, and the first
;; row here is the only thing holding it honest. A verb added to the
;; dispatcher and not to the catalogue is callable and undocumented; one
;; added to the catalogue and not the dispatcher is advertised and
;; missing, which is worse -- a tool list is read and believed.
;;
;; NEVER: IT IS COMPARED IN BOTH DIRECTIONS. "Every catalogue entry is a real
;; verb" is satisfied by a catalogue with one entry in it.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch rpc-ok? rpc-verbs verb-catalogue write-protocol))

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

(define here (string-append "/tmp/describe-" (number->string (get-process-id))))
(define root-dir "..")
(system (string-append "rm -rf " here "; mkdir -p " here "/store " here "/home"))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(define store (string-append here "/store"))

(rpc-dispatch store '(init) "test")

(define answer (rpc-dispatch store '(describe) "test"))
(define entries (cdr (assq 'verbs (cdr answer))))
(define (entry-field entry name) (cadr (assq name (cdr entry))))
(define described (map car entries))

(define (missing-from xs ys)
  (let loop ((xs xs) (out '()))
    (cond ((null? xs) (reverse out))
          ((memq (car xs) ys) (loop (cdr xs) out))
          (else (loop (cdr xs) (cons (car xs) out))))))

;; ---- DS-1 the two lists ----------------------------------------------------

(want "DS-1 every verb the dispatcher has is described"
      (missing-from (rpc-verbs) described)
      '())

(want "DS-1 TWIN: and nothing is described that the dispatcher has not got"
      (missing-from described (rpc-verbs))
      '())

;; NEVER: AND THE COUNTS AGREE, which the two rows above do not by themselves
;; guarantee: a catalogue that listed a verb twice passes both.
(want "DS-1 and there are as many entries as verbs"
      (list (length described) (length (rpc-verbs)))
      (list (length (rpc-verbs)) (length (rpc-verbs))))

;; ---- DS-2 asking costs nothing ---------------------------------------------
;;
;; KEY: A CALLER ASKING WHAT THE VERBS ARE OFTEN HAS NO STORE. The MCP
;; shell builds its tool list before anyone has said which store they
;; mean, so a `describe` that needed one would make the shell's first
;; act a refusal.
(want "DS-2 describe answers without a store"
      (rpc-ok? (rpc-dispatch (string-append here "/no-such-store") '(describe) "test"))
      #t)

;; NEVER: AND IT DID NOT CREATE ONE ON THE WAY. "It answered" would also be
;; true of an implementation that quietly made the store first.
(want "DS-2 TWIN: and it did not create one"
      (if (file-exists? (string-append here "/no-such-store")) 'CREATED-IT 'left-it-alone)
      'left-it-alone)

(want "DS-2 extra arguments are refused"
      (car (rpc-dispatch store '(describe "something") "test"))
      'usage)

;; ---- DS-2b it touches nothing ----------------------------------------------
;;
;; NEVER: "IT ANSWERED WITHOUT A STORE" IS NOT THE SAME CLAIM AS "IT TOUCHED
;; NOTHING". A `describe` that opened an existing store, took its lock and
;; wrote a snapshot would pass every row above -- the rows above only ever
;; gave it a store that was not there.
;;
;; NOTE: MEASURED AS EVERY FILE'S NAME *AND CONTENT*, before and after.
;; NEVER: Not as "did it raise", which a read would not trip. NEVER: And not as a
;; listing of NAMES, which was the first attempt: `insert` appends to a
;; log file that already exists, so a name listing compared equal across
;; a write and the twin below said so -- the instrument could not see the
;; thing it was there to see.
(define (tree-listing dir)
  (let ((out (string-append here "/listing.txt")))
    (system (string-append "find " dir " -type f -exec md5 -r {} \\; | sort > " out))
    (call-with-input-file out get-string-all)))

(define before-describe (tree-listing store))
(define quiet-answer (rpc-dispatch store '(describe) "test"))
(define after-describe (tree-listing store))

(want "DS-2b describe against a real store leaves it byte for byte as it was"
      (if (string=? before-describe after-describe)
          'untouched
          (list 'changed))
      'untouched)

;; NEVER: AND THE LISTING REALLY WOULD HAVE NOTICED. Without this the row above
;; is passed by a listing that is empty, or by one taken of the wrong
;; directory -- both of which compare equal to themselves.
(want "DS-2b TWIN: and a verb that does write is seen by the same listing"
      (let ((before (tree-listing store)))
        (rpc-dispatch store '(insert "--title" "DS2B") "test")
        (if (string=? before (tree-listing store)) 'SAW-NOTHING 'it-notices))
      'it-notices)


;; ---- DS-2c what a byte-for-byte listing cannot see -------------------------
;;
;; NEVER: THE ROWS ABOVE COMPARE NAMES AND CONTENTS, and three of the things
;; `describe` promises not to do leave both unchanged: opening a file and
;; reading it, taking a lock and releasing it, making a temporary file and
;; removing it again. The promise is "it does not open the store, take a
;; lock or write a byte" -- and only one third of that is measurable by
;; looking at the store afterwards.
;;
;; NOTE: SO THE INSTRUMENT IS THE TRACE, which reports the acts themselves:
;; `log-open`, `flock`, `unlock`, `fsync`. Measured for `outline` on this
;; same store: flock 1, log-open 1, fsync 6, unlock 1. For `describe`:
;; none at all.
(define (trace-kinds-of verb)
  (let ((out (string-append here "/" verb ".trace")))
    (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "' "
                           "THEOURGIA_HOME=" here "/home THEOURGIA_TRACE=1 "
                           "scheme --script ../cli.sc " verb " --store " store
                           " --wire > /dev/null 2> " out))
    (let* ((text (call-with-input-file out get-string-all))
           (n (string-length text)))
      (let count ((i 0) (seen 0))
        (cond ((> (+ i 7) n) seen)
              ((string=? (substring text i (+ i 7)) "(trace ") (count (+ i 7) (+ seen 1)))
              (else (count (+ i 1) seen)))))))

(want "DS-2c describe does not open the store, lock it, or flush anything"
      (let ((n (trace-kinds-of "describe")))
        (if (zero? n) 'did-nothing-to-the-store (list 'events n)))
      'did-nothing-to-the-store)

;; NEVER: AND THE INSTRUMENT REALLY SEES THOSE ACTS. Without this row the one
;; above is passed by a build with tracing switched off, by a trace
;; written somewhere else, and by a verb that failed before it started.
(want "DS-2c TWIN: and a verb that does touch the store is seen doing it"
      (let ((n (trace-kinds-of "outline")))
        (if (> n 0) 'it-notices (list 'BLIND n)))
      'it-notices)

;; ---- DS-3 what each entry carries ------------------------------------------

(want "DS-3 every entry has a usage form, a description and a protocol flag"
      (let loop ((es entries) (bad '()))
        (cond
          ((null? es) (reverse bad))
          ((and (pair? (entry-field (car es) 'usage))
                (string? (entry-field (car es) 'description))
                (> (string-length (entry-field (car es) 'description)) 0)
                (boolean? (entry-field (car es) 'protocol)))
           (loop (cdr es) bad))
          (else (loop (cdr es) (cons (car (car es)) bad)))))
      '())

;; NOTE: THE USAGE FORM NAMES THE VERB IT BELONGS TO. A catalogue whose
;; entries had drifted -- `read`'s usage filed under `refs` -- would pass
;; every row above.
(want "DS-3 each usage form begins with its own verb"
      (let loop ((es entries) (bad '()))
        (cond
          ((null? es) (reverse bad))
          ((eq? (car (entry-field (car es) 'usage)) (car (car es)))
           (loop (cdr es) bad))
          (else (loop (cdr es) (cons (car (car es)) bad)))))
      '())

;; ---- DS-4 the protocol is the one constant ---------------------------------
;;
;; NEVER: BYTE FOR BYTE, not "contains something about writing". The MCP tool
;; descriptions are built from this same text; if `describe` handed out a
;; paraphrase, the shell and the README would be documenting two
;; different protocols and both would look right.
(want "DS-4 the protocol describe hands out is the constant itself"
      (if (string=? (cadr (assq 'protocol (cdr answer))) write-protocol)
          'byte-for-byte
          'DIFFERENT)
      'byte-for-byte)

;; ---- DS-5 which verbs the protocol is about --------------------------------
;;
;; NOTE: NAMED, NOT COUNTED. "Two verbs are marked" would stay true if the
;; mark moved to two other verbs.
(want "DS-5 the protocol flag is set on exactly insert and write"
      (let loop ((es entries) (out '()))
        (cond ((null? es) (reverse out))
              ((entry-field (car es) 'protocol) (loop (cdr es) (cons (car (car es)) out)))
              (else (loop (cdr es) out))))
      '(insert write))

;; ---- DS-6 the route, and the client's bootstrap list ----------------------
;;
;; KEY: EVERY ENTRY SAYS WHO CARRIES IT OUT. `daemon` means a client sends
;; it over the socket; `local` means the client runs the server itself,
;; because there is nothing to send it to yet.
;;
;; NEVER: THE DEFECT THIS EXISTS FOR: the MCP shell has no local route at all,
;; so it offered `theourgia_init` as a tool, sent it to a daemon for a
;; store that did not exist, and the daemon could not start. The shell
;; could never create a store, and an agent reading the tool list was told
;; otherwise.
;;
;; NOTE: THE CLIENT PROGRAM KEEPS ITS OWN LIST and must: it needs to know
;; before it can ask anybody. So the two are compared, in both directions.
;; NEVER: The list is READ OUT OF THE PROGRAM'S SOURCE rather than written
;; again here -- a copy in this file would agree with itself forever.

(define client-source
  (let ((p (string-append root-dir "/theourgia.sc")))
    (if (file-exists? p) (call-with-input-file p get-string-all) "")))

(define (local-verbs-of-client text)
  (let* ((key "(define local-verbs '(")
         (at (let loop ((i 0))
               (cond ((> (+ i (string-length key)) (string-length text)) #f)
                     ((string=? (substring text i (+ i (string-length key))) key) i)
                     (else (loop (+ i 1)))))))
    (and at
         (let loop ((i (+ at (string-length key))) (acc '()) (word '()))
           (cond
             ((>= i (string-length text)) (reverse acc))
             ((char=? (string-ref text i) #\))
              (reverse (if (null? word) acc (cons (list->string (reverse word)) acc))))
             ((char=? (string-ref text i) #\space)
              (loop (+ i 1) (if (null? word) acc (cons (list->string (reverse word)) acc)) '()))
             (else (loop (+ i 1) acc (cons (string-ref text i) word))))))))

(define client-local (or (local-verbs-of-client client-source) '()))

(want "DS-6 the client program's local list was read, and is not empty"
      (if (null? client-local) 'READ-NOTHING 'read-it)
      'read-it)

(define catalogue-local
  (map (lambda (e) (symbol->string (car e)))
       (let loop ((es entries) (out '()))
         (cond ((null? es) (reverse out))
               ((eq? 'local (entry-field (car es) 'route)) (loop (cdr es) (cons (car es) out)))
               (else (loop (cdr es) out))))))

(want "DS-6 every verb the catalogue routes locally is in the client's list"
      (let loop ((xs catalogue-local) (bad '()))
        (cond ((null? xs) (reverse bad))
              ((member (car xs) client-local) (loop (cdr xs) bad))
              (else (loop (cdr xs) (cons (car xs) bad)))))
      '())


;; NEVER: AND EVERY ENTRY'S ROUTE IS A ROUTE. The comparisons above select the
;; entries whose route is exactly `local`, so a verb whose route is
;; MISSPELLED -- `deamon` -- silently leaves that set and matches
;; everything they ask. The shell then drops it from the tool list, which
;; is a verb quietly disappearing for an agent, and no row here moves.
;; A closed set is checked against the whole table instead.
(want "DS-6 every entry in the catalogue carries a route that exists"
      (let loop ((es entries) (bad '()))
        (cond ((null? es) (reverse bad))
              ((memq (entry-field (car es) 'route) '(local daemon))
               (loop (cdr es) bad))
              (else (loop (cdr es)
                          (cons (list (car (car es)) (entry-field (car es) 'route)) bad)))))
      '())

;; NOTE: THE OTHER DIRECTION HAS AN EXEMPTION, AND IT IS NAMED. `serve` and
;; `eval` are commands of the server PROGRAM, not core verbs, so they are
;; not in the catalogue at all and cannot have a route. Only a client
;; local-verb that IS a core verb has to be marked local.
(define not-core-verbs '("serve" "eval"))

(want "DS-6 every core verb the client runs locally is marked local"
      (let loop ((xs client-local) (bad '()))
        (cond
          ((null? xs) (reverse bad))
          ((member (car xs) not-core-verbs) (loop (cdr xs) bad))
          ((not (member (car xs) (map (lambda (e) (symbol->string (car e))) entries)))
           (loop (cdr xs) (cons (list (car xs) 'not-a-core-verb) bad)))
          ((member (car xs) catalogue-local) (loop (cdr xs) bad))
          (else (loop (cdr xs) (cons (list (car xs) 'routed-to-the-daemon) bad)))))
      '())

;; NEVER: AND THE EXEMPTION IS NOT A HOLE: the two names it covers must really
;; be absent from the catalogue. If `eval` ever became a core verb this
;; row goes red and somebody decides, rather than the exemption quietly
;; covering a verb that now has a route.
(want "DS-6 the exempted names are genuinely not core verbs"
      (let loop ((xs not-core-verbs) (bad '()))
        (cond ((null? xs) (reverse bad))
              ((member (car xs) (map (lambda (e) (symbol->string (car e))) entries))
               (loop (cdr xs) (cons (car xs) bad)))
              (else (loop (cdr xs) bad))))
      '())

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\ndescribe complete\n" rows bad)
(exit (if (zero? bad) 0 1))
