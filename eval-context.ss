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
(library (theourgia eval-context)
  (export eval-context! store-cut blocks block
          display write newline open-string-input-port current-error-port flush-output-port)
  (import (except (chezscheme) display write newline)
          (prefix (only (chezscheme) display write newline) host:)
          (theourgia reduce) (theourgia code-project))
  ;; The worker owns this detached read view. No store handle or IO capability
  ;; is exported into evaluated code.

  ;; ⛔ EVERY WRITE THE SANDBOX MAKES IS FLUSHED BY THE WRAPPER, not by
  ;; the code that made it. The worker frames user output as `(out …)` /
  ;; `(err …)` on a custom port, and Chez BUFFERS before it ever calls
  ;; that port's `write!` -- measured: three separate calls,
  ;; `(display "hello ")`, `(display 42)` and a `write`, arrived as ONE
  ;; frame. A quota counted on frames is then a quota counted on how the
  ;; runtime happened to coalesce them, and `EV-output-limit-before-exit`
  ;; -- 65,537 bytes and then an infinite loop -- would be decided by the
  ;; buffer rather than by the limit.
  ;;
  ;; ⚠️ THE FLUSH CANNOT LIVE IN THE PORT. `make-custom-textual-output-port`
  ;; takes no buffer-mode in Chez 10.1 and `custom-port-buffer-size` may
  ;; not be zero, so "unbuffered" is not available there; and it must not
  ;; live in the user's hands, because a user who forgets is a user whose
  ;; output the supervisor cannot count in time. It lives here, in the
  ;; three procedures the sandbox is given.
  (define display
    (case-lambda
      ((x) (host:display x) (flush-output-port (current-output-port)))
      ((x port) (host:display x port) (flush-output-port port))))
  (define write
    (case-lambda
      ((x) (host:write x) (flush-output-port (current-output-port)))
      ((x port) (host:write x port) (flush-output-port port))))
  (define newline
    (case-lambda
      (() (host:newline) (flush-output-port (current-output-port)))
      ((port) (host:newline port) (flush-output-port port))))

  (define snapshot #f)
  (define overlay '())

  ;; ⛔ THE OVERLAY IS APPLIED WHERE THE SANDBOX READS, not only where a
  ;; library body is assembled. Measured with it only in the latter:
  ;; `eval --working --writer w1 '(block "…")'` answered the COMMITTED
  ;; src while the answer's own `(working-view …)` said the draft was in
  ;; the view -- the report and the value disagreeing about the same
  ;; request.
  ;;
  ;; ⚠️ IT IS `working-read`'s RULE, not a second one: a block with a live
  ;; draft reads as that draft, everything else as what is committed.
  (define (eval-context! state drafts)
    (set! snapshot state)
    (set! overlay drafts))

  (define (draft-src id)
    (let loop ((ds overlay))
      (cond ((null? ds) #f)
            ((equal? id (car (car ds)))
             (let ((body (cadddr (car ds))))
               (if (string? body) body (utf8->string body))))
            (else (loop (cdr ds))))))

  ;; The shape a block reads as is the reduction's; only the `src` field
  ;; is replaced, because that is the only thing a draft holds.
  (define (with-draft id record)
    (let ((src (draft-src id)))
      (if (or (not src) (not (pair? record)))
          record
          (map (lambda (part)
                 (if (and (pair? part) (eq? 'fields (car part)))
                     (cons 'fields
                           (map (lambda (f)
                                  (if (and (pair? f) (eq? 'src (car f))) (cons 'src src) f))
                                (cdr part)))
                     part))
               record))))

  (define (store-cut) (reduce-applied-cut snapshot))
  (define (blocks)
    (map (lambda (b)
           (let ((id (and (pair? b) (pair? (car b)) (eq? 'id (caar b)) (cdar b))))
             (if id (with-draft id b) b)))
         (state-datum snapshot)))
  (define (block id) (with-draft id (state-read snapshot id)))
)
