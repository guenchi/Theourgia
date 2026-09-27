;; Copyright 2018 - 2026 The Theourgia Authors
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

;;; (theourgia refusal) -- a refusal datum as the MCP shell renders it.
;;;
;;; F100b item 8. The shell answers a refusal in one of two shapes, and both
;;; carry ONE object built here from the refusal datum D:
;;;
;;;   {"kind": K, "path": P?, "reason": R?, "errno": E?, "op": O?,
;;;    "written": [[..]..]?, "client-written": [[..]..]?, "attempt": T?,
;;;    "exit": n? | "signal": n?, "origin": "transport"?, "datum": "<D>"}
;;;
;;; - K is D's kind; for the thin client's serve-start-failed it is the
;;;   kind its `(kind K)` clause names (item 6).
;;; - An absent clause is omitted. For `incomplete`, the clauses of
;;;   `failed` are flattened to the top level.
;;; - `written` and `client-written` are arrays of arrays of strings.
;;; - `origin` is present only for a refusal the transport made.
;;; - `datum` is D written, the whole of it, clauses this object does not
;;;   name included.
;;;
;;; THE TWO SHAPES:
;;; - tools/call: a RESULT
;;;   {"content": [{"type": "text", "text": "<D>"}], "isError": true,
;;;    "_meta": {"refusal": <object>}};
;;; - tools/list: an ERROR {"code": -32000, "message": "core did not
;;;   start", "data": <object>}.
;;; `(unavailable)`, a catalogue that could not be read, is the one refusal
;;; that is not an `(error ...)` datum. Its object is exactly
;;; {"kind": "unavailable", "datum": "(unavailable)"} (J3).
;;;
;;; AGGREGATION (c) (item 2): the object is built from D combined with the
;;; MCP process's own record. TRIPWIRE: the shell creates nothing of its
;;; own (D22), so that record is empty on every route today and the
;;; combination changes nothing. H6 is its coverage.
;;;
;;; Pure: no filesystem work, like (theourgia answers).

(library (theourgia refusal)
  (export refusal-object refusal-result-json refusal-error-json)
  (import (chezscheme)
          (only (theourgia json) json->string)
          (only (theourgia answers) combine-report))

  (define refusal-error-code -32000)
  (define refusal-error-message "core did not start")

  (define (written-text d)
    (call-with-string-output-port (lambda (p) (write d p))))

  (define (text-of x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          (else (written-text x))))

  ;; An entry `(op path ...)` as an array of strings.
  (define (entry-json e)
    (list->vector (map text-of (if (list? e) e (list e)))))

  (define (entries-json entries)
    (list->vector (map entry-json (if (list? entries) entries '()))))

  ;; An errno is a name (a symbol), a number, or #f (a short write):
  ;; a JSON string, a number, or null.
  (define (errno-json e)
    (cond ((symbol? e) (symbol->string e))
          ((number? e) e)
          (else 'null)))

  (define (clause-of clauses head)
    (let loop ((cs clauses))
      (cond ((null? cs) #f)
            ((and (pair? (car cs)) (eq? (caar cs) head) (pair? (cdar cs))) (car cs))
            (else (loop (cdr cs))))))

  ;; The members item 8 names, in its order, each only when its clause is
  ;; there. `failed` (incomplete) contributes its own clauses at this level.
  (define (members-of clauses)
    (let* ((failed (clause-of clauses 'failed))
           (flat (if failed (append (cdr failed) clauses) clauses))
           (one (lambda (head key render)
                  (let ((c (clause-of flat head)))
                    (if c (list (cons key (render (cadr c)))) '())))))
      (append
        (one 'path "path" text-of)
        (one 'reason "reason" text-of)
        (one 'errno "errno" errno-json)
        (one 'op "op" text-of)
        (one 'written "written" entries-json)
        (one 'client-written "client-written" entries-json)
        (one 'attempt "attempt" (lambda (t) (if (string? t) t 'null)))
        (one 'exit "exit" (lambda (n) n))
        (one 'signal "signal" (lambda (n) n)))))

  ;; D's kind: serve-start-failed names it in its `(kind K)` clause.
  (define (kind-of d)
    (let ((k (and (pair? (cddr d)) (clause-of (cddr d) 'kind))))
      (if (and (eq? (cadr d) 'serve-start-failed) k) (cadr k) (cadr d))))

  ;; (refusal-object D [record [origin]]) -> an alist json->string writes as
  ;; the object above. D is `(error kind clause ...)` or `(unavailable)`.
  ;; record defaults to '(); origin is #f or 'transport.
  (define (refusal-object d . opt)
    (let ((record (if (pair? opt) (car opt) '()))
          (origin (and (pair? opt) (pair? (cdr opt)) (cadr opt))))
      (if (and (pair? d) (eq? (car d) 'unavailable))
          (list (cons "kind" "unavailable") (cons "datum" "(unavailable)"))
          (let* ((d (combine-report d record))
                 (clauses (cddr d)))
            (append
              (list (cons "kind" (text-of (kind-of d))))
              (members-of clauses)
              (if origin (list (cons "origin" (text-of origin))) '())
              (list (cons "datum" (written-text d))))))))

  ;; tools/call's RESULT body for D.
  (define (refusal-result-json d . opt)
    (let ((obj (apply refusal-object d opt)))
      (json->string
        (list (cons "content" (vector (list (cons "type" "text")
                                            (cons "text" (cdr (assoc "datum" obj))))))
              (cons "isError" #t)
              (cons "_meta" (list (cons "refusal" obj)))))))

  ;; tools/list's ERROR object for D (the `error` member of the frame).
  (define (refusal-error-json d . opt)
    (json->string
      (list (cons "code" refusal-error-code)
            (cons "message" refusal-error-message)
            (cons "data" (apply refusal-object d opt))))))
