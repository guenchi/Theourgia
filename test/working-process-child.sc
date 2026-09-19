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

(import (chezscheme) (theourgia rpc) (theourgia reduce))
(define args (cdr (command-line)))
;; NO ARGUMENTS IS A USAGE LINE, NOT A CRASH. See working-fault-child.
(when (or (< (length args) 2)
          (and (not (string=? (car args) "setup")) (< (length args) 5)))
  (printf "usage: working-process-child setup <store> | working-process-child write <store> <writer> <block-id> <bytes>\n")
  (exit 0))
(define store (cadr args))
(if (string=? (car args) "setup")
    (begin
      (unless (rpc-ok? (rpc-dispatch store '(init) "test")) (exit 1))
      (let* ((a (rpc-dispatch store '(insert "--title" "Shared" "--text" "old") "test"))
             (ev (car (cadr (assq 'events (cdr a))))))
        (display (block-id (car ev) (cdr ev))) (newline)))
    (let ((writer (caddr args)) (id (cadddr args)) (bytes (list-ref args 4)))
      (display "ready") (newline) (flush-output-port (current-output-port))
      (when (eof-object? (read-char)) (exit 2))
      (let ((a (rpc-dispatch store (list 'write id bytes "--writer" writer) "test")))
        (unless (rpc-ok? a) (write a) (newline) (exit 1))
        (write (rpc-dispatch store (list 'read id "--working" "--writer" writer) "test"))
        (newline))))
