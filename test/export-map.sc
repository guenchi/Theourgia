;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; EXPORT-CODE --MAP ANSWERS WHERE EACH BLOCK LIES IN EACH FILE (README,
;; export-code): beside `(files <n>)`, `(map ((<path> ((<id> <start> <end>)
;; ...)) ...))`, half-open byte offsets in file order covering the file, from
;; the layout the encoder wrote; a block's marker line and pad go with the
;; block they precede. A file whose ranges the export does not know (one the
;; working view's structural overlay added) is not in the map; `files` still
;; counts it. Without --map the answer is (ok (files <n>)) alone, as on the
;; base, for a machine reader and a person alike.
;;
;;   EM-1  two files, the second of two blocks: the map names both, in order, each
;;         covering its file, b.scm with two ranges
;;   EM-2  every range holds its block's @block marker line, and nothing else's
;;   EM-3  --raw: each range is that block's src, as stored, in order
;;   EM-4  CONTROL: without --map the answer is (ok (files 2)), read as data
;;         and printed for a person alike
;;   EM-5  --working with a file the overlay added: (files 3), the map without it
(import (chezscheme) (only (theourgia working) working-new!))
(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/export-map-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(define home (string-append root "/home"))
(define cli "../core.sc")

;; -> the first datum a command answered, with TEXT on its standard input
(define (run store text . args)
  (call-with-output-file (string-append root "/in") (lambda (p) (display text p)) 'replace)
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER THEOURGIA_HOME=" home
            " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " --wire < " root "/in > " root "/out 2> " root "/err"))
  (let ((x (call-with-input-file (string-append root "/out") read)))
    (if (eof-object? x) (list 'no-answer) x)))
(define n-store 0)
;; -> (<store> <block id> <cursor text>): a store holding one block, and the
;; cursor after it
(define (store-with-a-block!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append root "/s" (number->string n-store))))
    (run d "" "init")
    (let* ((a (run d "" "insert" "--under" "root" "--title" "A"))
           (w (cadr (assq 'cursor (cdr a))))
           (id (car (car (cadr (assq 'state (cdr a)))))))
      (list d id (string-append (car w) ":" (number->string (cdr w)))))))
(define (end-of d)
  (let* ((c (run d "" "check")) (w (cadr (assq 'local-writer (cdr c)))))
    (cadr (assq 'end (cdr (assoc w (cadr (assq 'writers (cdr c)))))))))
;; -> the text a command printed (no --wire)
(define (run-human store . args)
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER THEOURGIA_HOME=" home
            " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " < /dev/null > " root "/out 2> " root "/err"))
  (call-with-input-file (string-append root "/out") get-string-all))
(define (file-bytes p) (call-with-port (open-file-input-port p) get-bytevector-all))
(define (clause a k) (and (pair? a) (list? a) (assq k (filter pair? (cdr a)))))
;; the ranges cover [0, n) in order, end to start
(define (covers? ranges n)
  (let loop ((rs ranges) (at 0))
    (if (null? rs) (= at n)
        (and (= (cadr (car rs)) at) (<= (cadr (car rs)) (caddr (car rs))) (loop (cdr rs) (caddr (car rs)))))))
(define (contains? hay needle)
  (let ((n (string-length needle)) (m (string-length hay)))
    (let loop ((i 0)) (and (<= (+ i n) m) (or (string=? (substring hay i (+ i n)) needle) (loop (+ i 1)))))))
(define (slice bv s e) (let ((out (make-bytevector (- e s)))) (bytevector-copy! bv s out 0 (- e s)) out))

(define d (string-append root "/s"))
(run d "" "init")
(define src (string-append root "/src"))
(system (string-append "mkdir -p '" src "' && printf '(define (f x) x)\\n' > '" src "/a.scm' && printf '(define (g y) y)\\n(define (h z) z)\\n' > '" src "/b.scm'"))
;; b.scm gets a second block after the one its import made, so its file has
;; two ranges, in order
(define imported
  (map (lambda (i) (car (car (cadr (assq 'state (cdr i))))))
       (cdr (assq 'items (cdr (run d "" "import-code" src))))))
(define (fields-of id) (cdr (assq 'fields (cadr (run d "" "read" id)))))
(define (parent-of id) (cadr (assq 'position (cadr (run d "" "read" id)))))
(define b-file (find (lambda (id) (equal? (assq 'path (fields-of id)) '(path . "b.scm"))) imported))
(define b-child (find (lambda (id) (equal? (parent-of id) b-file)) imported))
(printf "b.scm's file block ~a, its block ~a; the second: ~s\n" b-file b-child
        (run d (format "((insert ~s ~s ((kind . code) (mode . text) (lang . scheme) (src . ~s))))"
                       b-file b-child (string->utf8 "(define (m w) w)\n"))
             "batch"))
(define out1 (string-append root "/out1"))
(define marked (run d "" "export-code" out1 "--map"))
(printf "export-code --map: ~s\n" marked)
(define the-map (let ((m (clause marked 'map))) (if m (cadr m) '())))

(want "EM-1 export-code --map of two files answers a map naming both, in the export's order, each file's ranges covering its bytes from 0 to its end"
      (list (cadr (clause marked 'files)) (map car the-map)
            (map (lambda (e) (covers? (cadr e) (bytevector-length (file-bytes (string-append out1 "/" (car e)))))) the-map)
            (map (lambda (e) (length (cadr e))) the-map))
      '(2 ("a.scm" "b.scm") (#t #t) (1 2)))

(want "EM-2 each range holds its own block's @block marker line and no other block's"
      (map (lambda (e)
             (let ((bytes (file-bytes (string-append out1 "/" (car e)))))
               (map (lambda (r)
                      (let ((text (utf8->string (slice bytes (cadr r) (caddr r)))))
                        (and (contains? text (string-append "@block " (car r) " "))
                             (for-all (lambda (o) (or (equal? (car o) (car r)) (not (contains? text (string-append "@block " (car o) " ")))))
                                      (cadr e)))))
                    (cadr e))))
           the-map)
      (map (lambda (e) (map (lambda (r) #t) (cadr e))) the-map))

(define out3 (string-append root "/out3"))
(define raw (run d "" "export-code" out3 "--raw" "--map"))
(want "EM-3 export-code --raw --map: each range is its block's src as stored, in order, covering the file"
      (let ((m (let ((c (clause raw 'map))) (if c (cadr c) '()))))
        (list (map car m)
              (map (lambda (e)
                     (let ((bytes (file-bytes (string-append out3 "/" (car e)))))
                       (and (covers? (cadr e) (bytevector-length bytes))
                            (for-all (lambda (r)
                                       (equal? (slice bytes (cadr r) (caddr r))
                                               (let ((s (cdr (assq 'src (cdr (assq 'fields (cadr (run d "" "read" (car r)))))))))
                                                 (if (string? s) (string->utf8 s) s))))
                                     (cadr e)))))
                   m)))
      '(("a.scm" "b.scm") (#t #t)))

(want "EM-4 CONTROL: without --map the answer is (ok (files 2)) alone, as data and as printed for a person"
      (list (run d "" "export-code" (string-append root "/out4"))
            (run-human d "export-code" (string-append root "/out4b")))
      '((ok (files 2)) "(ok (files 2))\n"))

(putenv "THEOURGIA_HOME" home)
(define added (working-new! d #f "me" "c.scm" '((lang . scheme)) (string->utf8 "(define (k) 1)\n") #f))
(printf "working-new!: ~s\n" added)
(define out5 (string-append root "/out5"))
(define working (run d "" "export-code" out5 "--working" "--writer" "me" "--map"))
(printf "export-code --working --map: ~s\n" working)
(want "EM-5 export-code --working --map with a file the overlay added: files counts it, the map leaves it out and covers the others"
      (let ((m (let ((c (clause working 'map))) (if c (cadr c) '()))))
        (list (car added) (cadr (clause working 'files)) (map car m)
              (map (lambda (e) (covers? (cadr e) (bytevector-length (file-bytes (string-append out5 "/" (car e)))))) m)
              (file-exists? (string-append out5 "/c.scm"))))
      '(ok 3 ("a.scm" "b.scm") (#t #t) #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nexport-map complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
