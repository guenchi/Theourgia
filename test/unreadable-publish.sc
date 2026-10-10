;; Copyright 2018 - 2026 guenchi
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

;; Publish never replaces what it could not read.
;;
;; A candidate publish keeps (incoming/<seg>.<sha>.seg) is checked by
;; reading it back; a read that failed was taken as "not kept yet" and the
;; file was written over. A local writer whose metadata cannot be read
;; cannot be shown to be inactive, so its current segment is not repaired.
;; And the local copy publish compares against is read with the host
;; reader, whose failure answered (error internal ...) with no path.
;;
;; "Not replaced" is read from the trace -- no rename onto the file -- as
;; well as from the bytes; an identical write passes a byte comparison.

(import (chezscheme) (theourgia log) (theourgia trace)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia wire) encode-record))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (with-expected label expected (x) (want-1 label (caught got) x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/unreadable-publish-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'unreadable-publish "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

(define n 0)
(define (fresh-store!)
  (set! n (+ n 1))
  (let ((d (string-append root "/s" (number->string n))))
    (system (string-append "mkdir -p " d "/store " d "/home"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((store (string-append d "/store")))
      (rpc-dispatch store '(init) "test")
      store)))
(define (ask store . req) (rpc-dispatch store req "test"))
(define (local-writer store)
  (cadr (assq 'local-writer (cdr (ask store 'check)))))
(define (chmod! mode path) (system (string-append "chmod " mode " " path)))
(define (slurp-bytes p)
  (guard (e (#t 'unreadable))
    (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
      (if (eof-object? b) (make-bytevector 0) b))))
(define (spit-bytes! p bv)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o bv))))
(define (contains? text word)
  (let loop ((i 0))
    (and (<= (+ i (string-length word)) (string-length text))
         (or (string=? (substring text i (+ i (string-length word))) word)
             (loop (+ i 1))))))
(define M "mirrorz9")
(define rec1 (encode-record 1 1757300000001 "a" '() '(set "t" x "x")))
(define rec2 (encode-record 2 1757300000002 "a" '() '(set "t" y "y")))
(define (both a b)
  (let ((o (make-bytevector (+ (bytevector-length a) (bytevector-length b)))))
    (bytevector-copy! a 0 o 0 (bytevector-length a))
    (bytevector-copy! b 0 o (bytevector-length a) (bytevector-length b))
    o))

;; `(answer events)`, the events read back as data; paths read back as
;; symbols, because the trace line is written with display.
(define (traced thunk)
  (let-values (((port get) (open-string-output-port)))
    (trace-enable! #t)
    (let ((r (caught (parameterize ((current-error-port port)) (thunk)))))
      (trace-enable! #f)
      (list r (let ((in (open-string-input-port (get))))
                (let loop ((acc '()))
                  (let ((x (guard (e (#t (eof-object))) (read in))))
                    (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))))
(define (subject e)
  (let ((x (caddr e))) (let ((y (if (pair? x) (car x) x))) (if (symbol? y) (symbol->string y) y))))
(define (strip-temp p)
  (let loop ((i 0))
    (cond ((> (+ i 5) (string-length p)) p)
          ((string=? (substring p i (+ i 5)) ".tmp-") (substring p 0 i))
          (else (loop (+ i 1))))))
;; Renames onto `path`: a temporary named after it, moved over it.
(define (renames-onto t path)
  (length (filter (lambda (e) (and (list? e) (= 4 (length e)) (eq? (cadr e) 'rename)
                                   (string? (subject e))
                                   (string=? (strip-temp (subject e)) path)))
                  (cadr t))))
(define (any-renames t)
  (length (filter (lambda (e) (and (list? e) (= 4 (length e)) (eq? (cadr e) 'rename))) (cadr t))))
(define (kept-path a)
  (let ((k (and (pair? a) (list? a) (assq 'kept (cdr a))))) (and k (cadr k))))

(printf "== U9: a retained candidate this store cannot read ==\n")
(let* ((s (fresh-store!))
       (_ (log-publish! s M 1 rec1 (segment-sha rec1)))
       (cand (both rec1 rec2))
       (sha (segment-sha cand))
       (first (log-publish! s M 1 cand sha))
       (kept (kept-path first)))
  (want "CONTROL a candidate that extends a mirror's segment is kept, and the answer names the file"
        (list (and (pair? first) (car first)) (and (string? kept) (file-exists? kept)))
        '(newer-history-unmergeable #t))
  (let ((again (traced (lambda () (log-publish! s M 1 cand sha)))))
    (want "CONTROL publishing it again finds it kept and renames nothing onto it"
          (list (car (car again)) (renames-onto again kept))
          '(newer-history-unmergeable 0)))
  (let ((before (slurp-bytes kept)))
    (chmod! "000" kept)
    (let ((t (traced (lambda () (log-publish! s M 1 cand sha)))))
      (chmod! "600" kept)
      (want "U9 with the retained candidate at 000 the publish is refused, naming it"
            (let ((a (car t)))
              (list (and (pair? a) (not (memq (car a) '(newer-history-unmergeable published idempotent))))
                    (contains? (format "~s" a) kept)))
            '(#t #t))
      (want "U9 and nothing was renamed onto it, and its bytes are the ones it held"
            (list (renames-onto t kept) (equal? before (slurp-bytes kept)))
            '(0 #t)))))

(printf "== U9: an active local writer whose quarantine.sexp cannot be read ==\n")
(let* ((s (fresh-store!))
       (_ (ask s 'insert "--under" "root" "--title" "A"))
       (w (local-writer s))
       (seg (string-append (writer-directory s w) "/000001.sexp"))
       (q (string-append (writer-directory s w) "/quarantine.sexp"))
       (held (slurp-bytes seg))
       (cand (both held (encode-record 2 1757300000002 "a" '() '(set "t" y "y")))))
  (spit-bytes! q (string->utf8 "((format 1) (fork 99))\n"))
  (chmod! "000" q)
  (let ((t (traced (lambda () (log-publish! s w 1 cand (segment-sha cand))))))
    (chmod! "600" q)
    (want "U9 a covering candidate for its current segment is refused"
          (let ((a (car t)))
            (and (pair? a) (not (memq (car a) '(published idempotent newer-history-unmergeable)))))
          #t)
    (want "U9 and no rename happened anywhere, and the segment holds its bytes"
          (list (any-renames t) (equal? held (slurp-bytes seg)))
          '(0 #t))))

(printf "== the local copy publish compares against ==\n")
(let* ((s (fresh-store!))
       (_ (ask s 'insert "--under" "root" "--title" "A"))
       (w (local-writer s))
       (seg (string-append (writer-directory s w) "/000001.sexp"))
       (held (slurp-bytes seg))
       (cand (both held (encode-record 2 1757300000002 "a" '() '(set "t" y "y"))))
       (file (string-append root "/candidate-" (number->string n))))
  (spit-bytes! file cand)
  (chmod! "000" seg)
  (let ((a (caught (ask s 'publish w "1" file))))
    (chmod! "644" seg)
    (want "a local segment publish cannot read is named, not answered internal"
          (let ((text (format "~s" a)))
            (list (and (pair? a) (car a))
                  (contains? text "internal")
                  (contains? text seg)))
          '(error #f #t))
    (want "and the segment holds its bytes"
          (equal? held (slurp-bytes seg))
          #t)))

;; ---- a readable directory given as the candidate --------------------------
;; ADDED BY THE CODE SESSION (F77b review 1, ruled L): the candidate is read
;; with R1's read, and a directory reads as EISDIR -- answered by name where
;; the base answered (error internal ...). Nothing under writers/ is unreadable.
(printf "== a readable directory as the candidate ==\n")
(let* ((s (fresh-store!))
       (dir (string-append root "/a-directory-" (number->string n))))
  (system (string-append "mkdir -p " dir))
  (let ((a (caught (ask s 'publish M "1" dir))))
    (want "a readable directory given as the candidate answers candidate-unreadable, naming it, not internal"
          (let ((text (format "~s" a)))
            (list (contains? text "candidate-unreadable") (contains? text "internal") (contains? text dir)))
          '(#t #f #t))))

;; ---- review round 2: a sibling segment, a fork marker, the .ok marker --------
;; ADDED BY THE CODE SESSION (F77b review 2, the main session's request).
(printf "== review 2: publish reads what it depends on by name ==\n")
(let* ((s (fresh-store!))
       (_ (log-publish! s M 1 rec1 (segment-sha rec1)))
       (_ (log-publish! s M 2 rec2 (segment-sha rec2)))
       (seg2 (string-append (writer-directory s M) "/000002.sexp"))
       (file (string-append root "/again-" (number->string n))))
  (spit-bytes! file rec1)
  (chmod! "000" seg2)
  (let ((a (caught (ask s 'publish M "1" file))))
    (chmod! "600" seg2)
    (want "REVIEW2 an unreadable sibling segment is named, not answered internal"
          (let ((text (format "~s" a))) (list (contains? text "internal") (contains? text seg2)))
          '(#f #t))))
(let* ((s (fresh-store!))
       (_ (log-publish! s M 1 rec1 (segment-sha rec1)))
       (q (string-append (writer-directory s M) "/quarantine.sexp"))
       (divergent (encode-record 1 1757300000001 "a" '() '(set "t" x "different")))
       (file (string-append root "/divergent-" (number->string n))))
  (spit-bytes! q (string->utf8 "((format 1) (fork 99))\n"))
  (spit-bytes! file divergent)
  (chmod! "000" q)
  (let ((a (caught (ask s 'publish M "1" file))))
    (chmod! "600" q)
    (want "REVIEW2 a divergent publish beside an unreadable quarantine.sexp is named, not answered internal"
          (let ((text (format "~s" a))) (list (contains? text "internal") (contains? text q)))
          '(#f #t))))
(let* ((s (fresh-store!))
       (_ (log-publish! s M 1 rec1 (segment-sha rec1)))
       (cand (both rec1 rec2))
       (sha (segment-sha cand))
       (first (log-publish! s M 1 cand sha))
       (kept (kept-path first))
       (marker (string-append (substring kept 0 (- (string-length kept) 4)) ".ok")))
  (system (string-append "rm -f " marker "; mkdir -p " marker))
  (let ((again (caught (log-publish! s M 1 cand sha))))
    (want "REVIEW2 a readable directory at the marker path is present, as the base read it: the keep answers as before"
          (and (pair? again) (car again))
          'newer-history-unmergeable)))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "chmod -R u+rwX " root " 2>/dev/null"))
;; THE SENTINEL THE RUNNER READS: without it a fixture is red whatever its
;; rows say (run-fixtures.sh counts "<name> complete").
(printf "unreadable-publish complete\n")
(exit (if (= bad 0) 0 1))
