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

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia request)
        (theourgia ffi) (theourgia trace) (theourgia log) (theourgia wire)
        (theourgia digest))
(define bad 0)
(define (want label got expected)
  (if (equal? got expected) (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/evidence-index-" (number->string (get-process-id))))
(when (file-exists? root) (error 'evidence-index1 "Use a fresh root" root))
(mkdir-p! root)
(define store (string-append root "/store"))
(putenv "THEOURGIA_HOME" (string-append store "-home"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))
(with-store-write store (lambda (state view)
                         (map (lambda (n) (list 'insert 'root #f (list (cons 'title (number->string n))))) (iota 40))) "test")
(rpc-dispatch store (list 'insert "--title" "target" "--req" "indexed" "--cursor" (string-append writer ":40")) "test")
(define identity (cons writer "indexed"))
(define (query) (store-evidence store identity))
(want "QI-01 indexed identity is present" (length (query)) 1)
(define out (open-output-string))
(parameterize ((current-error-port out)) (trace-enable! #t) (query) (trace-enable! #f))
(define probes
  (let ((p (open-input-string (get-output-string out))))
    (let loop ((n 0)) (let ((x (read p)))
      (if (eof-object? x) n
          (loop (+ n (if (and (pair? x) (eq? 'trace (car x)) (eq? 'identity-source-decode (cadr x))) 1 0))))))))
(want "QI-07 hot disk query decodes only the matching frame" probes 1)
(define checkpoint (string-append store "/request-index.sexp"))
(want "QI-06 readonly evidence lookup does not write a cache" (file-exists? checkpoint) #f)
(define dir (writer-directory store writer))
(define segment (string-append dir "/000001.sexp"))
(define original (call-with-port (open-file-input-port segment) get-bytevector-all))
(define frame
  (let loop ((i (- (bytevector-length original) 2)))
    (if (or (< i 0) (= (bytevector-u8-ref original i) 10))
        (let ((b (make-bytevector (- (bytevector-length original) (+ i 1)))))
          (bytevector-copy! original (+ i 1) b 0 (bytevector-length b)) b)
        (loop (- i 1)))))
(define incoming (string-append dir "/incoming"))
(mkdir-p! incoming)
(define candidate (string-append incoming "/candidate"))
(call-with-port (open-file-output-port candidate) (lambda (p) (put-bytevector p frame)))
(want "QI-03 newly arrived incoming evidence invalidates a hot index"
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) (map ev-placement (query)))
      '(incoming valid-history))
(delete-file candidate)
(want "QI-03 clearing a duplicate location preserves the verified original"
      (map ev-placement (query)) '(valid-history))
(define manifest (string-append dir "/published.sexp"))
(call-with-output-file manifest (lambda (p) (write '() p)))
(want "QI-05 unchanged sequences with a new manifest become unlisted" (map ev-placement (query)) '(unlisted))
(delete-file manifest)
(want "QI-05 restored metadata restores verifiable placement" (map ev-placement (query)) '(valid-history))
(store-snapshot! store "test")
(want "QI-09 snapshot persists a derived identity checkpoint" (file-exists? checkpoint) #t)
;; THE CHILD IS FOUND BESIDE THIS SCRIPT, NOT UNDER THE CURRENT
;; DIRECTORY. The runner starts every fixture from `test/`, where
;; `test/evidence-index-child.sc` does not exist -- the shell then
;; reported a missing file, `code` was non-zero, and every row that used
;; the child read `(child-failed 1)`: a red about the caller's directory
;; rather than about the index.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define child-script
  (let ((p (string-append script-dir "/evidence-index-child.sc")))
    (if (file-exists? p) p
        (assertion-violation 'evidence-index1
          "evidence-index-child.sc is not beside this fixture" p))))
;; EVERY PATH IN THE COMMAND IS QUOTED. These are concatenated into a
;; shell command line, and one of them is now derived from the path this
;; script was started with -- so a space anywhere above the delivery
;; splits the script name in two and the row reads `(child-failed 1)`, a
;; sentence about the store produced by a directory name.
(define (shell-quote s)
  (string-append "'" (apply string-append
    (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
(define (child . req)
  (let ((report (string-append root "/child.sexp")))
    (when (file-exists? report) (delete-file report))
    (let ((code (system (string-append "scheme --script " (shell-quote child-script)
                                     " " (shell-quote store) " " (shell-quote writer)
                                     " " (shell-quote report)
                                     (if (null? req) "" (string-append " " (shell-quote (car req))))))))
      (if (and (= code 0) (file-exists? report)) (call-with-input-file report read) (list 'child-failed code)))))
(want "QI-06 a fresh process reconstructs the same identity" (child) '(valid-history))
(define checkpoint-bytes (and (file-exists? checkpoint) (call-with-port (open-file-input-port checkpoint) get-bytevector-all)))
(when checkpoint-bytes
  ;; A syntactically valid, checksummed cache still cannot invent log evidence.
  (let* ((x (storable-decode (string->sexpr-extended (utf8->string checkpoint-bytes))))
         (body (list-ref x 4)) (decoded (decode-line frame)) (a (cadddr decoded))
         (fake (encode-record (cadr decoded) (caddr decoded)
                              (cons (car a) (cons (cons writer "forged") (cddr a)))
                              (list-ref decoded 4) (list-ref decoded 5)))
         (entries (map (lambda (e)
                         (if (equal? (cadddr e) frame)
                             (list (car e) (cadr e) (caddr e) fake (list-ref e 4)) e)) (cadr body)))
         (new-body (list (car body) entries))
         (encoded (string->utf8 (sexpr->string-extended (storable-encode new-body))))
         (forged (list 'request-index 1 (caddr x) (bytevector->hex (sha256 encoded)) new-body)))
    (call-with-port (open-file-output-port checkpoint (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 (sexpr->string-extended (storable-encode forged))))))
    (want "QI-04 cached frame bytes cannot manufacture verified evidence"
          (child "forged") '(index-evidence-missing))
    (delete-file checkpoint)
    (want "QI-06 a forged cache key is not a permanent execution fence" (child "forged") '())))
(when checkpoint-bytes
  (call-with-output-file checkpoint (lambda (p) (display "(broken" p)) 'replace))
(want "QI-06 a corrupt checkpoint falls back to real records" (child) '(valid-history))
(when checkpoint-bytes
  (call-with-port (open-file-output-port checkpoint (file-options no-fail)) (lambda (p) (put-bytevector p checkpoint-bytes))))
;; The old checkpoint is a missing-evidence hint, never proof of execution.
(delete-file segment)
(want "QI-04 a checkpoint without the source refuses to forget a seen identity"
      (child) '(index-evidence-missing))
(printf "~a failures\nevidence-index1 complete\n" bad)
(exit (if (zero? bad) 0 1))
