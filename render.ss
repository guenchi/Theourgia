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

  ;; ⭐ ONE PRINTER FOR EVERY ANSWER THIS SYSTEM GIVES. There used to be
  ;; three: this one, a copy of it in `mcp/server.ss`, and a bare `write`
  ;; in `daemon.ss` that set nothing at all -- so the same answer could
  ;; leave by three routes in two spellings, and the daemon's was already
  ;; the odd one out.
  ;;
  ;; ⭐ NON-ASCII GOES OUT AS UTF-8, NOT AS `\x6C49;`. Measured by the
  ;; worker: escaping made CJK cost 2.36 times the tokens, where the JSON
  ;; envelope around it costs 1.16. The reader of these answers is an
  ;; agent paying by the token.
  ;;
  ;; ⛔ SET ONCE PER PROCESS, NOT `parameterize`d AROUND THE WRITE. Chez
  ;; parameters are per OS THREAD, and every green thread on that thread
  ;; shares them (7.6.36, measured) -- so a printer that set them for the
  ;; duration of its own call would be setting them for whatever else was
  ;; running. Each entry point calls this once, before it answers
  ;; anything.
  ;;
  ;; ⛔ THIS DOES NOT TOUCH WHAT IS WRITTEN TO DISK. A record is
  ;; serialised by `sexpr->string-extended` in `wire.ss`, which writes
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
  (define (render-human answer)
    (cond
      ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
            (eq? (caadr answer) 'text)) (cadadr answer))
      ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
            (eq? (caadr answer) 'items)) (apply string-append (map render-wire (cdadr answer))))
      (else (render-wire answer))))
)
