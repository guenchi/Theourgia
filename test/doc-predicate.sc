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

;; WHETHER A DOC CARRIES A RECOVERY MARKER IS NOT A QUESTION FOR THE
;; LANGUAGE TABLE.
;;
;; A `doc` that contains `@block <id>` is refused, because a recovery
;; marker inside a document would let a replay materialise a block that
;; was never written. That refusal decides whether a record is accepted
;; into the log, so it has to mean the same thing in every build of this
;; core -- and `register-language!` edits a table at runtime.
;;
;; IT USED TO ASK THE TABLE. `datum-metadata.sc` looked the language up
;; by name and took its comment prefix from the entry, so re-registering
;; Scheme with a different line comment changed the verdict: `;; @block
;; w.1` stopped being seen as a marker. A store that accepted that
;; record would disagree with a store that refused it, on the same
;; bytes.
;;
;; THE ROWS ARE THE SAME THREE PAYLOADS TWICE, and the assertion is that
;; the two readings are EQUAL rather than that they are any particular
;; value: what is being pinned is independence, not the verdicts. The
;; verdicts are named anyway, so that a version which refused everything
;; could not satisfy "equal before and after".
;;
;; DP-00 IS THE TWIN AND IT COMES FIRST. Every row below is also what a
;; `register-language!` that did nothing would produce, and the property
;; these rows are about is the very one the predicate used to read. So
;; the table is asked, directly, whether it changed.
;;
;; THE STRUCTURAL HALF IS IN `closures.sc`. Caching `language-for-name`
;; into a constant at load time would pass every row here and still put
;; the table in the answer; CL-02 and CL-03 assert that neither
;; `markers` nor `datum-metadata` can reach `languages` at all.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia rpc)
        (theourgia languages) (theourgia ffi))

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
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/doc-predicate-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define n 0)
;; A FRESH STORE PER PAYLOAD. A refused write leaves the store usable,
;; but an accepted one leaves a block in it, and the next row's answer
;; would then be about a store with a history.
(define (fresh!)
  (set! n (+ n 1))
  (let ((s (string-append root "/s" (number->string n))))
    (rpc-dispatch s '(init) "t")
    s))

;; THE ANSWER IS REDUCED TO ITS VERDICT. Two runs of the same payload
;; produce different writer names and different hashes, so comparing the
;; answers whole would compare the store's identity, not its decision.
(define (verdict doc)
  (let ((answer (with-store-write (fresh!)
                  (lambda (s v)
                    (list (list 'insert 'root #f
                                (list '(kind . code) '(mode . datum) '(body define x 1)
                                      (cons 'doc doc)))))
                  "t")))
    (if (and (pair? answer) (pair? (car answer)) (eq? 'error (caar answer)))
        (cdar answer)
        'accepted)))

(define marker-doc ";; @block w.1\n")
(define plain-doc ";; hello\n")
(define slash-doc "// @block w.1\n")

(define before (map verdict (list marker-doc plain-doc slash-doc)))

(define scheme-entry (language-for-name "scheme"))
(register-language!
  (map (lambda (p) (if (eq? (car p) 'line-comment) (list 'line-comment "//") p))
       scheme-entry))

(want "DP-00 TWIN: the table really was changed under the predicate"
      (list (language-property scheme-entry 'line-comment #f)
            (language-property (language-for-name "scheme") 'line-comment #f))
      '(";;" "//"))

(define after (map verdict (list marker-doc plain-doc slash-doc)))

(want "DP-01 the three verdicts do not change with the table" after before)

;; AND THEY ARE THESE THREE, so that "equal before and after" cannot be
;; satisfied by a build that refuses, or accepts, everything.
(want "DP-02 a doc carrying a recovery marker is refused"
      (list-ref after 0) '(malformed-intent (marker-in-doc)))
(want "DP-03 an ordinary comment doc is accepted"
      (list-ref after 1) 'accepted)
(want "DP-04 a doc that is not in the language's comment form is refused"
      (list-ref after 2) '(malformed-intent (invalid-doc)))

(printf "rows: ~a\n~a failures\ndoc-predicate complete\n" rows bad)
