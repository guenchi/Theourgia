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

;; THE APPEND KEEPS ITS OWN WRITER'S PREFIX (design v20). An append
;; used to read the writer's whole log again to learn its sequence and its
;; target; the session now keeps what it validated at log-begin, advances it
;; by its own writes, and stands it in for that read only while one stat and
;; one listing find the disk as the session left it.
;;
;; TWO KINDS OF ROW. ANSWER rows read only what an append answers and what is
;; on the disk: master reads the log in full on every append, so they hold
;; there exactly as here, and master is their oracle. PATH rows read which
;; path answered -- (trace catch-up-path <writer> consumed-prefix |
;; validated-in-full) -- and whether the session still keeps a prefix
;; (session-own-prefix, looked up at run time so this file loads on master).
;; Neither exists on master, so PATH rows are red there by construction.
;;
;; THE DEGENERACY ROW runs the in-process scenarios again in a child of this
;; file (named by its absolute path, so a runner can find it) expanded with
;; THEOURGIA_DEGENERACY=1: after every committed append
;; that leaves a kept prefix, the writer's log is validated in full and
;; compared with it field by field, and a difference raises. The child
;; prints its answers; the row asks that it finished and answered as this
;; process did.
;;
;; THE WINDOW ROWS park a child at a barrier (THEOURGIA_INJECT=on,
;; THEOURGIA_BARRIER=<name>:<fifo>) while this process changes the segment
;; under it: after-tail-check, between the tail check and the write (new on
;; this tree, so master reads never-parked), and before-rotate-write, before a
;; rotation leaves the current segment (master has it, and answers there with
;; no session-own-prefix to read). Each child appends a second time in the same
;; session and reports the path that append took.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi) (theourgia trace))

(define child? (equal? (getenv "APPEND_PREFIX_ROLE") "child"))

;; A fresh directory under THEOURGIA_TEST_ROOT, named by the process; the
;; same refusals as log12's (the path is used unquoted in shell commands and
;; is removed before use).
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    (unless (for-all (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\/ #\. #\- #\_))))
                     (string->list path))
      (assertion-violation 'test-dir "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _" root))
    (unless (char=? #\/ (string-ref path 0))
      (assertion-violation 'test-dir "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i)) (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    (for-each (lambda (command)
                (let ((status (system command)))
                  (unless (eqv? 0 status)
                    (assertion-violation 'test-dir "could not prepare the scratch directory" (list command status)))))
              (list (string-append "rm -rf " path " " path "-home") (string-append "mkdir -p " path)))
    path))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(include "expected.ss")
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define d (test-dir "append-prefix"))
(define W "wwwa7p2k")
(define (rec seq ts deps payload)
  (encode-record seq ts "agent:claude" deps (storable-encode payload)))
(define (r n) (rec n (+ 1757300000000 n) '() (list 'put (string-append "w." (number->string n)) '())))
(define R (bytevector-length (r 1)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) (make-bytevector 0) b))))
(define (len-of path)
  (let ((b (slurp path))) (if (bytevector? b) (bytevector-length b) b)))
(define (text-of path)
  (let ((b (slurp path))) (if (bytevector? b) (utf8->string b) "")))
(define (wpath n) (string-append d "/writers/" W "/" (segment-file-name n)))
(define (wfile name) (string-append d "/writers/" W "/" name))
(define owner-text "((machine \"m\"))\n")
(define (build! . segs)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" W " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (wfile "owner.sexp") (string->utf8 owner-text))
  (putenv "THEOURGIA_HOME" (string-append d "-home"))
  (let ((n (instance-install! d))) (owner-install! d W n))
  (put! (wpath 1) (if (null? segs) (cat (r 1) (r 2)) (car segs))))
;; Records 1 and 2, then the first 20 bytes of record 3: a torn tail.
(define (torn-store!)
  (build! (cat (r 1) (r 2) (let ((h (r 3)) (o (make-bytevector 20))) (bytevector-copy! h 0 o 0 20) o))))

(define (traced thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e))) (thunk))
      (trace-enable! #f))
    (get-output-string p)))
(define (has-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))
(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))
;; The catch-up-path events of TEXT, in order.
(define (paths-of text)
  (map (lambda (l)
         (cond ((has-substring? l "consumed-prefix") 'consumed-prefix)
               ((has-substring? l "validated-in-full") 'validated-in-full)
               (else 'other)))
       (filter (lambda (l) (has-substring? l "(trace catch-up-path ")) (lines-of text))))

;; session-own-prefix where the library exports it (this tree), else #f.
(define own-prefix-reader
  (guard (e (#t #f)) (eval 'session-own-prefix (environment '(theourgia log)))))
(define (kept? s) (if own-prefix-reader (and (own-prefix-reader s) #t) 'no-reader))

;; One append, as a reducer drives it: a reset handshake answered first when the
;; session withholds its view for one, and a committed record confirmed as
;; applied, so the next append has a view.
(define (append-one! s payload)
  (when (and (not (session-view s)) (session-reset-pending s))
    (session-reset-done! s (session-epoch s)))
  (let ((v (session-view s)))
    (if v
        (let ((res (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                                  (view-expect-seq v) "agent:claude" '() payload))))
          (when (and (pair? res) (eq? (car res) 'committed))
            (session-applied! s (session-epoch s) (list (cons W (cadr res)))))
          res)
        (list 'no-view #f))))
(define fixed-ts 1757300000003)
;; Past segment 1's age limit (its first record's time plus an hour): the
;; next append rotates.
(define age-ts (+ 1757300000001 3600000 1000))

;; BODY in one session under CLOCK, traced: (BODY's value . the trace).
(define (in-session clock body)
  (let* ((out #f)
         (text (traced
                 (lambda ()
                   (parameterize ((log-clock (lambda () clock)))
                     (let ((s (log-begin d (lambda args 'applied))))
                       (set! out (body s))
                       (guard (e (#t (if #f #f))) (session-commit! s))
                       (log-end! s)))))))
    (cons out text)))

;; ---- the in-process scenarios: each answers (answers paths kept) ----------

;; AP-2: the first append rotates (segment 1 is past its age), the next two
;; land in segment 2.
(define (ap-2)
  (build!)
  (let ((run (in-session age-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.3" ())))
                        (b (append-one! s '(put "w.4" ())))
                        (c (append-one! s '(put "w.5" ()))))
                   (list (list a b c) (kept? s)))))))
    (list (list (car (car run)) (len-of (wpath 1)) (len-of (wpath 2)))
          (paths-of (cdr run))
          (cadr (car run)))))

;; AP-3: a torn tail; the first append repairs it.
(define (ap-3)
  (torn-store!)
  (let ((run (in-session fixed-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.3" ())))
                        (b (append-one! s '(put "w.4" ())))
                        (c (append-one! s '(put "w.5" ()))))
                   (list (list a b c) (kept? s)))))))
    (list (list (car (car run)) (len-of (wpath 1)))
          (paths-of (cdr run))
          (cadr (car run)))))

;; AP-5: a newline added to the own writer's owner.sexp between two appends:
;; the same datum, another content, so its version changes (file-version hashes
;; the content) and the session reloads. The frame built
;; before the reload is refused for its epoch; the next, from a new view,
;; validates in full.
(define (ap-5)
  (build!)
  (let ((run (in-session fixed-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.3" ())))
                        (rewrite (put! (wfile "owner.sexp") (cat (slurp (wfile "owner.sexp")) (string->utf8 "\n"))))
                        (b (append-one! s '(put "w.4" ())))
                        (c (append-one! s '(put "w.4" ()))))
                   (list (list a b c) (kept? s)))))))
    (list (car (car run)) (paths-of (cdr run)) (cadr (car run)))))

;; AP-7(a): another frame appended to the current segment from outside,
;; between two appends of one session.
(define (ap-7a)
  (build!)
  (let ((run (in-session fixed-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.3" ())))
                        (outside (put! (wpath 1) (cat (slurp (wpath 1)) (r 4))))
                        (b (append-one! s '(put "w.4" ()))))
                   (list (list a b) (kept? s)))))))
    (list (list (car (car run)) (len-of (wpath 1))) (paths-of (cdr run)) (cadr (car run)))))

;; AP-8: the current segment truncated from outside, between two appends of
;; one session, back to records 1 and 2.
(define (ap-8)
  (build!)
  (let ((run (in-session fixed-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.3" ())))
                        (outside (put! (wpath 1) (cat (r 1) (r 2))))
                        (b (append-one! s '(put "w.4" ()))))
                   (list (list a b) (kept? s)))))))
    (list (list (car (car run)) (len-of (wpath 1))) (paths-of (cdr run)) (cadr (car run)))))

;; AP-11: an empty next segment created from outside between two appends: the
;; current segment's size and times do not move, the segment names do, and the
;; append validates in full and writes into the new segment.
(define (ap-11)
  (build!)
  (let ((run (in-session fixed-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.3" ())))
                        (outside (put! (wpath 2) (make-bytevector 0)))
                        (b (append-one! s '(put "w.4" ()))))
                   (list (list a b) (kept? s)))))))
    (list (list (car (car run)) (len-of (wpath 1)) (len-of (wpath 2))) (paths-of (cdr run)) (cadr (car run)))))

;; AP-12: an older segment replaced from outside by a directory of the same
;; name between two appends. The current segment does not move and the names
;; do not either; the regular files do, so the append validates in full, which
;; finds the history starting at record 3 and refuses for integrity.
(define (ap-12)
  (build!)
  (put! (wpath 2) (r 3))
  (let ((run (in-session fixed-ts
               (lambda (s)
                 (let* ((a (append-one! s '(put "w.4" ())))
                        (outside (system (string-append "rm " (wpath 1) " && mkdir " (wpath 1))))
                        (b (append-one! s '(put "w.5" ()))))
                   (list (list a b) (kept? s)))))))
    (list (list (car (car run)) (len-of (wpath 2))) (paths-of (cdr run)) (cadr (car run)))))

(define scenarios (list (cons 'ap-2 ap-2) (cons 'ap-3 ap-3) (cons 'ap-5 ap-5) (cons 'ap-7a ap-7a) (cons 'ap-8 ap-8)
                        (cons 'ap-11 ap-11) (cons 'ap-12 ap-12)))

;; THE CHILD: the scenarios' answers, one datum, then nothing else.
(when child?
  (let ((out (guard (e (#t (list 'append-prefix-child 'raised
                                 (if (and (condition? e) (message-condition? e)) (condition-message e) e)
                                 (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
               (list 'append-prefix-child 'ok
                     (map (lambda (sc) (cons (car sc) (car ((cdr sc))))) scenarios)))))
    (write out)
    (newline)
    (exit 0)))

(define results (map (lambda (sc) (cons (car sc) (caught ((cdr sc))))) scenarios))
(define (answers-of name) (let ((x (cdr (assq name results)))) (if (and (pair? x) (eq? (car x) 'RAISED)) x (car x))))
(define (paths-from name) (let ((x (cdr (assq name results)))) (if (and (pair? x) (eq? (car x) 'RAISED)) x (cadr x))))
(define (kept-after name) (let ((x (cdr (assq name results)))) (if (and (pair? x) (eq? (car x) 'RAISED)) x (caddr x))))

(printf "== AP-2: rotation ==\n")
(want "AP-2 ANSWER the first append rotates and all three commit in segment 2; segment 1 keeps records 1 and 2"
      (answers-of 'ap-2)
      (list '((committed 3 2) (committed 4 2) (committed 5 2)) (* 2 R) (* 3 R)))
(want "AP-2 PATH every append consumes the kept prefix (seeded at log-begin, advanced across the rotation), and it is kept after"
      (list (paths-from 'ap-2) (kept-after 'ap-2))
      '((consumed-prefix consumed-prefix consumed-prefix) #t))

(printf "== AP-3: a torn tail ==\n")
(want "AP-3 ANSWER the first append repairs the tail; all three commit in segment 1"
      (answers-of 'ap-3)
      (list '((committed 3 1) (committed 4 1) (committed 5 1)) (* 5 R)))
(want "AP-3 PATH nothing is kept from a torn discovery: in full (the repair), in full (the re-seed), then the kept prefix"
      (list (paths-from 'ap-3) (kept-after 'ap-3))
      '((validated-in-full validated-in-full consumed-prefix) #t))

(printf "== AP-5: a reload in one session ==\n")
(want "AP-5 ANSWER a frame built before the reload is refused for its epoch, a fresh one commits"
      (answers-of 'ap-5)
      '((committed 3 1) (refused-before-reserve epoch) (committed 4 1)))
(want "AP-5 PATH the refused frame records no catch-up path; the append after the reload validates in full"
      (list (paths-from 'ap-5) (kept-after 'ap-5))
      '((consumed-prefix validated-in-full) #t))

(printf "== AP-7(a): a frame appended from outside between two appends ==\n")
(want "AP-7a ANSWER the second append is refused for its sequence: the disk holds a record the session did not write"
      (answers-of 'ap-7a)
      (list '((committed 3 1) (refused-before-reserve expect-seq)) (* 4 R)))
(want "AP-7a PATH the tail check sees the size move and the append validates in full"
      (list (paths-from 'ap-7a) (kept-after 'ap-7a))
      '((consumed-prefix validated-in-full) #t))

(printf "== AP-8: the current segment truncated from outside between two appends ==\n")
(want "AP-8 ANSWER the second append is refused for its sequence: the disk ends at record 2"
      (answers-of 'ap-8)
      (list '((committed 3 1) (refused-before-reserve expect-seq)) (* 2 R)))
(want "AP-8 PATH the tail check sees the size move and the append validates in full"
      (list (paths-from 'ap-8) (kept-after 'ap-8))
      '((consumed-prefix validated-in-full) #t))

(printf "== AP-11: a segment created from outside between two appends ==\n")
(want "AP-11 ANSWER the second append lands in the new segment"
      (answers-of 'ap-11)
      (list '((committed 3 1) (committed 4 2)) (* 3 R) R))
(want "AP-11 PATH the segment names moved, so the append validates in full, and the prefix is kept after"
      (list (paths-from 'ap-11) (kept-after 'ap-11))
      '((consumed-prefix validated-in-full) #t))

(printf "== AP-12: an older segment replaced by a directory between two appends ==\n")
(want "AP-12 ANSWER the second append is refused for integrity: the history no longer starts at record 1"
      (answers-of 'ap-12)
      (list '((committed 4 2) (refused-before-reserve integrity)) (* 2 R)))
(want "AP-12 PATH the segment files moved, so the append validates in full, and nothing is kept from a discovery with an integrity error"
      (list (paths-from 'ap-12) (kept-after 'ap-12))
      '((consumed-prefix validated-in-full) #f))

(printf "== AP-6: the own writer quarantined mid-session ==\n")
(build!)
(define ap-6
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let* ((s (log-begin d (lambda args 'applied)))
           (a (append-one! s '(put "w.3" ())))
           (q (put! (wfile "quarantine.sexp") (string->utf8 "((format 1) (fork 3))\n")))
           (b #f)
           (text (traced (lambda () (set! b (caught (append-one! s '(put "w.4" ()))))))))
      (guard (e (#t (if #f #f))) (session-commit! s))
      (guard (e (#t (if #f #f))) (log-end! s))
      (list a b (paths-of text)))))
;; The quarantine's fork at 3 clips the history to record 2, below what the
;; reducer was told it applied, so the reload asks for a reset first.
(want "AP-6 the append in flight when the own writer is quarantined is refused for the reset it needs, recording no catch-up path"
      ap-6
      '((committed 3 1) (refused-before-reserve reset-pending) ()))

;; ---- the children ---------------------------------------------------------

(define answer-path (string-append d "-answer"))
(define child-path (string-append d "-child.sc"))
;; Two appends in one session in a child under CLOCK; it writes (<first
;; answer's head> <kept? after it> <the second append's catch-up paths>) to
;; answer-path, a head being raised when the append raised. A committed first
;; append is confirmed as applied, so the second has a view. The answer is
;; written beside the path and renamed onto it, so a reader that sees the path
;; reads the whole datum.
(define (write-child! clock)
  (put! child-path
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(import (theourgia trace))\n"
            "(define reader (guard (e (#t #f)) (eval 'session-own-prefix (environment '(theourgia log)))))\n"
            "(define (one! s payload)\n"
            "  (let ((v (session-view s)))\n"
            "    (if (not v) (list 'no-view)\n"
            "        (guard (e (#t (list 'raised)))\n"
            "          (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)\n"
            "                                         (view-expect-seq v) \"agent:claude\" '() payload))))))\n"
            "(define (paths text)\n"
            "  (let loop ((i 0) (acc '()))\n"
            "    (let ((j (let find ((k i)) (cond ((> (+ k 20) (string-length text)) #f)\n"
            "                                    ((string=? (substring text k (+ k 20)) \"(trace catch-up-path\") k)\n"
            "                                    (else (find (+ k 1)))))))\n"
            "      (if (not j) (reverse acc)\n"
            "          (let ((e (let find ((k j)) (if (char=? (string-ref text k) #\\newline) k (find (+ k 1))))))\n"
            "            (loop e (cons (let ((l (substring text j e)))\n"
            "                            (cond ((string=? (substring l (- (string-length l) 16) (string-length l)) \"consumed-prefix)\") 'consumed-prefix)\n"
            "                                  (else 'validated-in-full)))\n"
            "                          acc)))))))\n"
            "(define res\n"
            "  (guard (e (#t (list 'raised)))\n"
            "    (parameterize ((log-clock (lambda () " (number->string clock) ")))\n"
            "      (let* ((s (log-begin \"" d "\" (lambda args 'applied)))\n"
            "             (r (one! s '(put \"w.3\" ())))\n"
            "             (k (if reader (and (reader s) #t) 'no-reader))\n"
            "             (confirm (when (eq? (car r) 'committed)\n"
            "                        (session-applied! s (session-epoch s) (list (cons (session-writer s) (cadr r))))))\n"
            "             (port (open-output-string))\n"
            "             (second (parameterize ((current-error-port port))\n"
            "                       (trace-enable! #t)\n"
            "                       (let ((x (one! s '(put \"w.4\" ())))) (trace-enable! #f) x))))\n"
            "        (guard (e (#t #f)) (session-commit! s))\n"
            "        (guard (e (#t #f)) (log-end! s))\n"
            "        (list (car r) k (paths (get-output-string port)))))))\n"
            "(call-with-port (open-file-output-port \"" answer-path ".tmp\" (file-options no-fail))\n"
            "  (lambda (p) (put-bytevector p (string->utf8 (format \"~s\\n\" res)))))\n"
            "(rename-file \"" answer-path ".tmp\" \"" answer-path "\")\n"))))
(define (answer)
  (guard (e (#t (list 'unreadable (text-of answer-path))))
    (read (open-string-input-port (text-of answer-path)))))
(define (wait-for path test limit)
  (let loop ((n 0))
    (cond ((test path) #t)
          ((> n limit) #f)
          (else (system "sleep 0.05") (loop (+ n 1))))))

;; AP-4: a write that moves some bytes and then fails.
(define (child-with-fault fault)
  (build!)
  (system (string-append "rm -f " answer-path))
  (write-child! fixed-ts)
  (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault
                         " scheme --script " child-path " < /dev/null > /dev/null 2>&1"))
  (answer))
(printf "== AP-4: a partial write ==\n")
(define ap-4 (child-with-fault "write-eio-after-partial@commit:file=000001.sexp"))
(want "AP-4 ANSWER the append answers partial-write"
      (and (pair? ap-4) (car ap-4)) 'partial-write)
(want "AP-4 PATH the session keeps no prefix after a partial write, and its next append validates in full"
      (and (pair? ap-4) (cdr ap-4)) '(#f (validated-in-full)))

;; The child whose pid PIDF holds, stopped only while that pid still runs the
;; child's program: a child that has finished may have had its pid reused.
(define (stop-child! pidf)
  (system (string-append "p=$(cat " pidf " 2>/dev/null); [ -n \"$p\" ] && ps -p $p -o command= 2>/dev/null | grep -q -- "
                         child-path " && kill -9 $p 2>/dev/null")))

;; A child under CLOCK parked at BARRIER; ACTION runs while it waits; then it
;; is released and its answer read. never-parked when it never got there.
;; THE RELEASE IS A FEEDER, not one write: the barrier parks at every pass, and
;; the child's second append may pass it again. The feeder writes to the fifo
;; until the answer is there and is then stopped; printf is the shell's own,
;; so stopping the subshell ends a write still waiting for a reader. SIGPIPE
;; is ignored there: a reader that closed between the open and the write must
;; not end the feeder before the next pass.
(define (child-at-barrier clock barrier action)
  (let ((fifo (string-append d "-fifo")) (trace (string-append d "-trace")) (pidf (string-append d "-pid"))
        (feeder (string-append d "-feeder")))
    (system (string-append "rm -f " fifo " " trace " " pidf " " feeder " " answer-path))
    (system (string-append "mkfifo " fifo))
    (write-child! clock)
    (system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_BARRIER=" barrier ":" fifo
                           " scheme --script " child-path " < /dev/null > /dev/null 2> " trace
                           " & echo $! > " pidf))
    (if (not (wait-for trace (lambda (p) (has-substring? (text-of p) (string-append "(trace barrier " barrier " #f)"))) 1200))
        (begin (stop-child! pidf) 'never-parked)
        (begin
          (action)
          (system (string-append "( trap '' PIPE; while :; do printf x > " fifo "; done ) > /dev/null 2>&1 & echo $! > " feeder))
          (let ((got (if (wait-for answer-path file-exists? 1200) (answer) 'no-answer)))
            (system (string-append "kill -9 $(cat " feeder ") 2>/dev/null"))
            (when (eq? got 'no-answer) (stop-child! pidf))
            got)))))

(printf "== AP-10: a raise between the catch-up and the accounting ==\n")
(define ap-10 (child-with-fault "stat-fail@commit:file=000001.sexp"))
;; The fault is one-shot: the child's second append, in the same session,
;; writes the record the first did not.
(want "AP-10 ANSWER a failed size probe raises out of the append; the retry in the same session commits it"
      (list (and (pair? ap-10) (car ap-10)) (len-of (wpath 1)))
      (list 'raised (* 3 R)))
(want "AP-10 PATH the session keeps no prefix after the raise, and its next append validates in full"
      (and (pair? ap-10) (cdr ap-10)) '(#f (validated-in-full)))

(printf "== AP-9: a refusal after a rotation ==\n")
(build!)
;; The first append does not rotate; the instance file is then made
;; unreadable as data, and the second append, past segment 1's age, rotates
;; and is refused at the instance check that follows the rotation.
(define ap-9
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let* ((s (log-begin d (lambda args 'applied)))
           (a (append-one! s '(put "w.3" ())))
           (tamper (put! (string-append d "/instance.sexp") (string->utf8 "((oops\n")))
           (b (caught (parameterize ((log-clock (lambda () age-ts))) (append-one! s '(put "w.4" ())))))
           (k (kept? s))
           (text (traced (lambda () (caught (parameterize ((log-clock (lambda () age-ts)))
                                              (append-one! s '(put "w.4" ()))))))))
      (guard (e (#t (if #f #f))) (session-commit! s))
      (guard (e (#t (if #f #f))) (log-end! s))
      (list a b (len-of (wpath 2)) k (paths-of text)))))
(want "AP-9 ANSWER the rotating append is refused for the malformed instance; the new segment is there and empty"
      (list (car ap-9) (cadr ap-9) (caddr ap-9))
      '((committed 3 1) (refused-before-reserve instance-malformed) 0))
(want "AP-9 PATH the session keeps no prefix after a refusal that follows a rotation, and its next append validates in full"
      (list-tail ap-9 3) '(#f (validated-in-full)))

(printf "== AP-7(b): a frame appended from outside inside the window ==\n")
(build!)
(define ap-7b (child-at-barrier fixed-ts "after-tail-check"
                                (lambda () (put! (wpath 1) (cat (slurp (wpath 1)) (r 3))))))
(want "AP-7b PATH the write lands after the outside frame; the post-write size check keeps no prefix, and the next append validates in full"
      ap-7b '(committed #f (validated-in-full)))

(printf "== AP-2(c): the outgoing segment grows inside a rotation ==\n")
(build!)
(define ap-2c (child-at-barrier age-ts "before-rotate-write"
                                (lambda () (put! (wpath 1) (cat (slurp (wpath 1)) (r 3))))))
(want "AP-2c PATH the write lands in segment 2; the outgoing segment is not the size kept, so no prefix is kept, and the next append validates in full"
      (list ap-2c (len-of (wpath 1)))
      (list '(committed #f (validated-in-full)) (* 3 R)))

(printf "== the degeneracy child ==\n")
(define degeneracy-child
  (let ((out (string-append d "-degeneracy.out")) (err (string-append d "-degeneracy.err")))
    (system (string-append "THEOURGIA_DEGENERACY=1 APPEND_PREFIX_ROLE=child scheme --script '"
                           (let ((self (car (command-line))))
                             (if (and (> (string-length self) 0) (char=? #\/ (string-ref self 0)))
                                 self
                                 (string-append (current-directory) "/" self)))
                           "' < /dev/null > " out " 2> " err))
    (guard (e (#t (list 'unreadable (text-of out) (text-of err))))
      (read (open-string-input-port (text-of out))))))
(want "AP-D under the degeneracy switch the scenarios finish (every kept prefix equal to a full validate) and answer as here"
      (if (and (pair? degeneracy-child) (eq? (car degeneracy-child) 'append-prefix-child)
               (eq? (cadr degeneracy-child) 'ok))
          (list 'ok (equal? (caddr degeneracy-child) (map (lambda (sc) (cons (car sc) (answers-of (car sc)))) scenarios)))
          degeneracy-child)
      '(ok #t))

(system (string-append "rm -rf " d " " d "-home " d "-answer " d "-answer.tmp " d "-child.sc " d "-fifo " d "-trace " d "-pid " d "-feeder "
                       d "-degeneracy.out " d "-degeneracy.err"))
(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "append-prefix complete\n")
(exit (if (= bad 0) 0 1))
