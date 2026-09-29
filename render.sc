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
(library (theourgia render)
  (export render-wire render-human answer-printing!)
  (import (chezscheme))

  ;; KEY: ONE PRINTER FOR EVERY ANSWER THIS SYSTEM GIVES. There used to be
  ;; three: this one, a copy of it in `mcp/server.sc`, and a bare `write`
  ;; in `daemon.sc` that set nothing at all -- so the same answer could
  ;; leave by three routes in two spellings, and the daemon's was already
  ;; the odd one out.
  ;;
  ;; KEY: NON-ASCII GOES OUT AS UTF-8, NOT AS `\x6C49;`. Measured by the
  ;; worker: escaping made CJK cost 2.36 times the tokens, where the JSON
  ;; envelope around it costs 1.16. The reader of these answers is an
  ;; agent paying by the token.
  ;;
  ;; NEVER: SET ONCE PER PROCESS, NOT `parameterize`d AROUND THE WRITE. Chez
  ;; parameters are per OS THREAD, and every green thread on that thread
  ;; shares them (7.6.36, measured) -- so a printer that set them for the
  ;; duration of its own call would be setting them for whatever else was
  ;; running. Each entry point calls this once, before it answers
  ;; anything.
  ;;
  ;; NEVER: THIS DOES NOT TOUCH WHAT IS WRITTEN TO DISK. A record is
  ;; serialised by `sexpr->string-extended` in `wire.sc`, which writes
  ;; its own characters and has never consulted these parameters. `U4`
  ;; compares the bytes of a stored record before and after, and it is a
  ;; guard rather than a formality: the two paths are separate because
  ;; somebody kept them separate, and nothing but a reading says so.
  (define (answer-printing!)
    (print-graph #f)
    (print-length #f)
    (print-level #f)
    (print-radix 10)
    (print-unicode #t)
    (print-gensym #f))

  (define (render-wire value)
    (call-with-string-output-port (lambda (p) (write value p) (newline p))))
  ;; THE INCOMPLETE CLAUSE IS PRINTED EVEN WHERE THE REST OF THE ANSWER IS
  ;; UNWRAPPED. A text or items answer is shown as its body alone, and that
  ;; used to drop every clause after the body; the one saying the answer
  ;; is missing a writer's records is the one a reader must not lose (K10).
  ;; So are `stale` and `via` after a TEXT body: an answer built from facts
  ;; an editor supplied says how many it could not use and where the others
  ;; came from. NOT after ITEMS: a reader of an items answer takes every line
  ;; as an item (the VS Code plugin parses a search's lines and rejects the
  ;; whole answer at the first line that is not a hit), so there the two
  ;; travel only in --wire.
  (define kept-clauses '(incomplete via stale))
  (define kept-after-items '(incomplete))
  (define (render-human answer)
    (let* ((clauses (if (and (pair? answer) (list? answer))
                        (filter (lambda (x) (and (pair? x) (memq (car x) kept-clauses))) (cdr answer))
                        '()))
           (tail (apply string-append (map render-wire clauses)))
           (items-tail (apply string-append
                              (map render-wire (filter (lambda (x) (memq (car x) kept-after-items)) clauses)))))
      (cond
        ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
              (eq? (caadr answer) 'text))
         (let ((text (cadadr answer)))
           (cond ((null? clauses) text)
                 ((or (= 0 (string-length text))
                      (char=? #\newline (string-ref text (- (string-length text) 1))))
                  (string-append text tail))
                 (else (string-append text "\n" tail)))))
        ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
              (eq? (caadr answer) 'items))
         (string-append (apply string-append (map render-wire (cdadr answer))) items-tail))
        (else (render-wire answer)))))
)
