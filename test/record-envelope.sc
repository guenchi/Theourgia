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

;; log-publish! and the record envelope: publish accepts exactly what the
;; reader reads.
;;
;; A segment the store answers `published` for is one the sender deletes
;; its own copy of. So the set of records publish accepts has to be the set
;; the reader will read past -- not wider, or a record nobody can read
;; becomes the only copy; and not narrower, or a peer's readable history is
;; refused for a rule the reader does not have. Each row below is one field
;; of the envelope, framed by hand with a correct CRC, so that the only
;; thing wrong with it is that field.

(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (only (theourgia crc32) crc32-string-hex)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia reduce) draft-version)
        (only (theourgia working) working-list))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

(define rows-run 0)

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) x))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define scratch (test-dir "record-envelope"))
(define W "wwwlocl0")
(define M "mirrorz9")
(define n-store 0)

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (sha-of b) (bytevector->hex (sha256 b)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))

(define (fresh!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d " " d "-home; mkdir -p "
                           d "/writers/" W " " d "/snap"))
    (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"p9\"))\n"))
    (file-ensure! (string-append d "/lock"))
    (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
    (putenv "THEOURGIA_HOME" (string-append d "-home"))
    (let ((k (instance-install! d))) (owner-install! d W k))
    (put! (string-append d "/writers/" W "/" (segment-file-name 1)) (make-bytevector 0))
    d))

;; A LINE FRAMED BY HAND. encode-record checks the envelope before it
;; frames anything, which is right for a writer and useless here: the
;; rows below are about bytes a peer sends, and a peer is not obliged to
;; have used encode-record.
(define (line text)
  (string->utf8 (string-append (crc32-string-hex text) " " text "\n")))
(define good-payload "(set \"t\" x \"x\")")
(define (text seq ts actor deps payload)
  (string-append "(" seq " " ts " " actor " " deps " " payload ")"))
(define (good seq) (line (text (number->string seq) "1757300000001" "\"a\"" "()" good-payload)))

(define (pub d bytes) (log-publish! d M 1 bytes (sha-of bytes)))
(define (mdir d) (string-append d "/writers/" M))
(define (left-behind d)
  (list (list 'segment (file-exists? (string-append (mdir d) "/" (segment-file-name 1))))
        (list 'listed (let ((m (and (file-exists? (mdir d)) (read-manifest d M))))
                        (and m (assv 1 m) #t)))
        (list 'incoming (let ((i (string-append (mdir d) "/incoming")))
                          (and (file-exists? i) (not (null? (directory-list i))))))))

;; THE READER'S VERDICT ON THE SAME BYTES, asked of the reader itself.
;; No expected sequence is passed: these rows are about the envelope, and a
;; record numbered 0 would otherwise be refused for its place in the
;; sequence rather than for its envelope.
(define (reader-reason bytes)
  (let ((r (scan-segment bytes M 1 #f #f (lambda args 'go))))
    (if (eq? (car r) 'integrity)
        (let ((e (cadr r)))
          (list (log-error-kind e)
                (let ((p (assq 'reason (log-error-detail e)))) (and p (cdr p)))
                (log-error-offset e)))
        (list 'reads (car r)))))

(define cases
  (list (list 'payload-not-a-form (text "1" "1757300000001" "\"a\"" "()" "x"))
        (list 'payload-not-a-form (text "1" "1757300000001" "\"a\"" "()" "42"))
        (list 'deps-malformed     (text "1" "1757300000001" "\"a\"" "(7)" good-payload))
        (list 'ts-not-a-number    (text "1" "oops" "\"a\"" "()" good-payload))
        (list 'actor-malformed    (text "1" "1757300000001" "42" "()" good-payload))
        (list 'seq-not-a-number   (text "oops" "1757300000001" "\"a\"" "()" good-payload))))

(printf "== E0: CONTROL -- each case is refused by the reader, for its own reason ==\n")
;; If one of these is not refused by the reader, the rows after it are
;; about nothing.
(for-each
  (lambda (c)
    (want (format "CONTROL the reader refuses ~a" (cadr c))
          (reader-reason (line (cadr c)))
          (list 'frame (car c) 0)))
  cases)
(want "CONTROL a well-formed record reads"
      (reader-reason (good 1))
      '(reads complete))

(printf "== E1: publish refuses each one, says why and where, and leaves nothing ==\n")
(for-each
  (lambda (c)
    (want (format "publish refuses ~a" (cadr c))
          (let* ((d (fresh!)) (a (pub d (line (cadr c)))))
            (list a (left-behind d)))
          (list (list 'error 'invalid-candidate (car c) (list 'offset 0))
                '((segment #f) (listed #f) (incoming #f)))))
  cases)

(printf "== E2: the offset is the refused record's, not the segment's ==\n")
(want "a bad second record is named at its own offset"
      (let ((first (good 1)))
        (pub (fresh!) (cat first (line (text "2" "1757300000002" "\"a\"" "()" "x")))))
      (list 'error 'invalid-candidate 'payload-not-a-form
            (list 'offset (bytevector-length (good 1)))))
(want "CONTROL the reader names the same offset"
      (let ((first (good 1)))
        (reader-reason (cat first (line (text "2" "1757300000002" "\"a\"" "()" "x")))))
      (list 'frame 'payload-not-a-form (bytevector-length (good 1))))

(printf "== E3: equal to the reader, not narrower ==\n")
;; The write side refuses both of these (ts below zero; an actor that is a
;; list but not a six-element request actor). The reader reads them. So
;; publish must too: a peer's readable history is not refused for a rule
;; the reader does not have.
(for-each
  (lambda (t)
    (want (format "publish accepts what the reader reads: ~a" t)
          (let ((bytes (line t)))
            (list (reader-reason bytes) (pub (fresh!) bytes)))
          '((reads complete) (published 1))))
  (list (text "1" "-1" "\"a\"" "()" good-payload)
        (text "1" "1757300000001" "(\"a\" \"b\")" "()" good-payload)))

(printf "== E4: publish and the reader give one answer per case ==\n")
;; The same question as E1, asked the other way round: for every case,
;; the reason publish gives is the reason the reader gives. A rule
;; written twice agrees until one copy changes.
(want "publish's reason is the reader's reason, case by case"
      (map (lambda (c)
             (let ((a (pub (fresh!) (line (cadr c)))))
               (and (pair? a) (eq? (car a) 'error) (pair? (cdr a)) (pair? (cddr a))
                    (eq? (caddr a) (cadr (reader-reason (line (cadr c))))))))
           cases)
      (map (lambda (c) #t) cases))

(printf "== E5: the writer does not produce what the reader refuses ==\n")
(want "encode-record refuses a payload that is not a form"
      (guard (e (#t 'refused))
        (encode-record 1 1757300000001 "a" '() 'x)
        'encoded)
      'refused)
(want "encode-record refuses a number as payload"
      (guard (e (#t 'refused))
        (encode-record 1 1757300000001 "a" '() 42)
        'encoded)
      'refused)
(want "CONTROL encode-record still encodes a form"
      (reader-reason (encode-record 1 1757300000001 "a" '() '(set "t" x "x")))
      '(reads complete))

(printf "== E0b: CONTROL -- the reader's order and its boundary acceptances ==\n")
;; E0 names one field per record. These pin what the shared predicate must
;; preserve and cannot be moved by it: which reason wins when two fields are
;; wrong, and the smallest values the reader accepts.
(want "CONTROL two bad fields: the reader names the actor before the deps"
      (reader-reason (line (text "1" "1757300000001" "42" "(7)" good-payload)))
      '(frame actor-malformed 0))
(want "CONTROL two bad fields: the reader names the seq before the payload"
      (reader-reason (line (text "oops" "1757300000001" "\"a\"" "()" "x")))
      '(frame seq-not-a-number 0))
(for-each
  (lambda (t)
    (want (format "CONTROL the reader accepts the boundary record ~a" t)
          (reader-reason (line t))
          '(reads complete)))
  (list (text "0" "1757300000001" "\"a\"" "()" good-payload)
        (text "1" "0" "\"a\"" "()" good-payload)
        (text "1" "1757300000001" "\"a\"" "((\"w\" . 0))" good-payload)
        (text "1" "1757300000001" "(\"agent\" \"a\" \"b\" \"c\" \"d\" \"e\")" "()" good-payload)
        (text "1" "1757300000001" "\"a\"" "()" "(x)")))

(printf "== E2b: the offset counts bytes, not characters ==\n")
;; The first record carries multibyte UTF-8, so a character count and a byte
;; count of it differ; the expected offset is measured from the bytes.
(define wide-first
  (line (text "1" "1757300000001" "\"a\"" "()" "(set \"t\" x \"\x3bb;\x3bb;\x3bb;\")")))
(want "CONTROL the first record is longer in bytes than in characters"
      (> (bytevector-length wide-first) (string-length (utf8->string wide-first)))
      #t)
(want "a bad second record after a multibyte first is named at its byte offset"
      (pub (fresh!) (cat wide-first (line (text "2" "1757300000002" "\"a\"" "()" "x"))))
      (list 'error 'invalid-candidate 'payload-not-a-form
            (list 'offset (bytevector-length wide-first))))
(want "a second record whose seq is not a number is refused, not raised"
      (pub (fresh!) (cat wide-first (line (text "oops" "1757300000002" "\"a\"" "()" good-payload))))
      (list 'error 'invalid-candidate 'seq-not-a-number
            (list 'offset (bytevector-length wide-first))))

(printf "== E5b: the writer refuses each reader refusal it can be handed ==\n")
(want "encode-record refuses deps (7)"
      (guard (e (#t 'refused)) (encode-record 1 1757300000001 "a" '(7) '(set "t" x "x")) 'encoded)
      'refused)
(want "encode-record refuses actor 42"
      (guard (e (#t 'refused)) (encode-record 1 1757300000001 42 '() '(set "t" x "x")) 'encoded)
      'refused)

(printf "== E6: which refusal wins ==\n")
(define poison (line (text "1" "1757300000001" "\"a\"" "()" "x")))
(want "an unreadable manifest is answered before the envelope"
      (let ((d (fresh!)))
        (system (string-append "mkdir -p " (mdir d)))
        (put! (string-append (mdir d) "/published.sexp") (string->utf8 "((format 1) ("))
        (let ((a (pub d poison))) (list (car a) (cadr a))))
      '(refused manifest-unreadable))
(want "the active local segment is answered before the envelope"
      (let ((d (fresh!)))
        (log-publish! d W 1 poison (sha-of poison)))
      '(refused active-writer-segment))
(want "a wrong hash is answered before the envelope"
      (log-publish! (fresh!) M 1 poison "deadbeef")
      '(error invalid-candidate sha-mismatch))
;; A record that does not decode is not looked at by the envelope row. The
;; candidate below is refused either way; what this pins is that the reason
;; is the envelope-bad record's, at its own offset.
(define crc-broken
  (let ((b (bytevector-copy (good 1))))
    (bytevector-u8-set! b 12 (if (= 55 (bytevector-u8-ref b 12)) 56 55))
    b))
(want "CONTROL the damaged first record does not decode"
      (car (decode-line crc-broken))
      'bad-crc)
(want "a bad-CRC record then an envelope-bad record: the envelope row answers, at the second"
      (pub (fresh!) (cat crc-broken (line (text "2" "1757300000002" "\"a\"" "()" "x"))))
      (list 'error 'invalid-candidate 'payload-not-a-form
            (list 'offset (bytevector-length crc-broken))))

(printf "== E7: a refusal stages nothing and runs no barrier, read from the trace ==\n")
;; The final state cannot show this: a candidate staged and then removed
;; leaves the same directory as one never staged. The product's own trace
;; says what it did.
(define (traced thunk)
  (let-values (((port take) (open-string-output-port)))
    (parameterize ((current-error-port port))
      (trace-enable! #t)
      (let ((a (guard (e (#t (trace-enable! #f) (list 'RAISED e))) (thunk))))
        (trace-enable! #f)
        (let ((text (take)))
          ;; A LINE THAT DOES NOT READ IS KEPT AS AN EVENT, NOT TAKEN AS THE
          ;; END. Stopping there would let whatever came after it -- a
          ;; staging, a flush -- vanish from the observation.
          (let loop ((ip (open-input-string text)) (out '()))
            (let ((x (guard (e (#t 'UNREADABLE)) (read ip))))
              (cond ((eof-object? x) (list a (reverse out)))
                    ((eq? x 'UNREADABLE) (list a (reverse (cons '(trace UNREADABLE unreadable #f) out))))
                    (else (loop ip (cons x out)))))))))))
;; EVERY OPERATION THAT CHANGES OR FLUSHES A FILE, and the writer's
;; directory itself as well as what is under it: a directory flush names the
;; directory, with no slash after it (ffi.sc fsync-dir!). The list is the
;; product's own trace vocabulary at 0ce3a02, less the events that only read.
;; rmdir joins it with the scratch directory `eval --lang` removes.
(define mutating-ops
  '(create write rename fsync unlink rmdir link ftruncate copy barrier published registry-write))
(define (touches? events d)
  (let ((under (mdir d)))
    (filter (lambda (ev)
              (and (list? ev) (= 4 (length ev)) (eq? 'trace (car ev))
                   (or (memq (cadr ev) '(barrier published UNREADABLE))
                       (and (memq (cadr ev) mutating-ops)
                            (let ((s (format "~a" (caddr ev))))
                              (let scan ((i 0))
                                (and (<= (+ i (string-length under)) (string-length s))
                                     (or (string=? (substring s i (+ i (string-length under))) under)
                                         (scan (+ i 1))))))))))
            events)))
(want "a refused publish changes and flushes nothing at or under the writer"
      (let ((d (fresh!)))
        (system (string-append "mkdir -p " (mdir d)))
        (let ((r (traced (lambda () (pub d poison)))))
          (list (car r) (map cadr (touches? (cadr r) d)))))
      (list (list 'error 'invalid-candidate 'payload-not-a-form (list 'offset 0)) '()))
;; The barrier is seen by its flushes, not by a `barrier` event: measured
;; on 0ce3a02, a publish emits no such event, and a twin asking for one was
;; red on a publish that certainly ran it. What a barrier does to this
;; writer's files is fsync them, and that is what the row above forbids.
(want "TWIN a valid publish of the same shape is seen staging and flushing under the writer"
      (let ((d (fresh!)))
        (system (string-append "mkdir -p " (mdir d)))
        (let* ((r (traced (lambda () (pub d (good 1)))))
               (ops (map cadr (touches? (cadr r) d))))
          (list (car r) (and (memq 'create ops) #t) (and (memq 'fsync ops) #t))))
      '((published 1) #t #t))

(printf "== E8: what a poison-carrying candidate loses, on purpose ==\n")
;; A mirror that already holds poison, planted directly: the segment and the
;; manifest entry naming it, as a publish of an earlier build would have
;; left them.
(define (plant! d bytes first last)
  (system (string-append "mkdir -p " (mdir d)))
  (put! (string-append (mdir d) "/" (segment-file-name 1)) bytes)
  (write-manifest! d M (list (list 1 (sha-of bytes) first last))))
(define held (cat (good 1) (line (text "2" "1757300000002" "\"a\"" "()" "x"))))
(want "CONTROL the planted mirror holds poison the reader refuses"
      (reader-reason held)
      (list 'frame 'payload-not-a-form (bytevector-length (good 1))))
(want "an identical re-send of held poison is refused, not idempotent"
      (let ((d (fresh!))) (plant! d held 1 2) (pub d held))
      (list 'error 'invalid-candidate 'payload-not-a-form
            (list 'offset (bytevector-length (good 1)))))
(want "TWIN an identical re-send of a clean held segment is idempotent"
      (let ((d (fresh!)) (clean (cat (good 1) (good 2)))) (plant! d clean 1 2) (pub d clean))
      '(idempotent 1))
(define torn-tail (let ((g (good 3))) (let ((o (make-bytevector 20))) (bytevector-copy! g 0 o 0 20) o)))
(want "a candidate that would repair a torn tail but carries the held poison is refused"
      (let ((d (fresh!)))
        (plant! d (cat held torn-tail) 1 2)
        (pub d (cat held (good 3))))
      (list 'error 'invalid-candidate 'payload-not-a-form
            (list 'offset (bytevector-length (good 1)))))
(want "TWIN the same repair over a clean local segment is still made"
      (let ((d (fresh!)) (clean (cat (good 1) (good 2))))
        (plant! d (cat clean torn-tail) 1 2)
        (let ((a (pub d (cat clean (good 3))))) (list (car a) (cadr a))))
      '(repaired 1))

(printf "== E0c: the reader's whole acceptance set, field by field ==\n")
;; E0 and E0b name examples. These are the edges of each rule: what the
;; reader takes and what it refuses, so that a shared predicate that moved a
;; boundary, or reordered two checks, is caught by the reader's own answer.
;; A number written as a decimal fraction (1.5, 1.0 as text) never reaches
;; the envelope: the codec refuses it while parsing, and the reader names
;; that `parse`. That is a fact about these spellings, not about inexact
;; numbers: the codec's own spelling of a flonum, #f8"...", and a ratio such
;; as 3/2 DO decode, and reach the envelope's exact-integer tests -- the
;; matrix (M) carries both. Measured on 0ce3a02.
(for-each
  (lambda (c)
    (want (format "CONTROL reader: ~a" (cadr c))
          (let ((r (reader-reason (line (cadr c)))))
            (if (eq? (car r) 'reads) 'reads (cadr r)))
          (car c)))
  (list
    (list 'ts-not-a-number    (text "1" "oops" "42" "()" good-payload))
    (list 'deps-malformed     (text "1" "1757300000001" "\"a\"" "(7)" "x"))
    (list 'seq-not-a-number   (text "-1" "1757300000001" "\"a\"" "()" good-payload))
    (list 'parse              (text "1.5" "1757300000001" "\"a\"" "()" good-payload))
    (list 'parse              (text "1.0" "1757300000001" "\"a\"" "()" good-payload))
    (list 'parse              (text "1" "1.5" "\"a\"" "()" good-payload))
    (list 'reads              (text "1" "-5" "\"a\"" "()" good-payload))
    (list 'reads              (text "1" "1757300000001" "(\"a\" . \"b\")" "()" good-payload))
    (list 'actor-malformed    (text "1" "1757300000001" "a" "()" good-payload))
    (list 'deps-malformed     (text "1" "1757300000001" "\"a\"" "((\"w\" . -1))" good-payload))
    (list 'parse              (text "1" "1757300000001" "\"a\"" "((\"w\" . 1.5))" good-payload))
    (list 'deps-malformed     (text "1" "1757300000001" "\"a\"" "((w . 1))" good-payload))
    (list 'deps-malformed     (text "1" "1757300000001" "\"a\"" "(\"w\" . 1)" good-payload))
    (list 'deps-malformed     (text "1" "1757300000001" "\"a\"" "((\"w\" . 1) . 2)" good-payload))
    (list 'reads              (text "1" "1757300000001" "\"a\"" "((\"w\" . 1) (\"v\" . 2))" good-payload))
    (list 'reads              (text "1" "1757300000001" "\"a\"" "()" "(x . y)"))
    (list 'payload-not-a-form (text "1" "1757300000001" "\"a\"" "()" "\"s\""))
    (list 'payload-not-a-form (text "1" "1757300000001" "\"a\"" "()" "()"))))

(printf "== E2c: the FIRST refused record, and offsets past the second ==\n")
(define bad-x (lambda (seq) (line (text (number->string seq) "1757300000001" "\"a\"" "()" "x"))))
(define bad-deps (lambda (seq) (line (text (number->string seq) "1757300000001" "\"a\"" "(7)" good-payload))))
(want "two bad records: the first one is named, with its own reason"
      (pub (fresh!) (cat (good 1) (bad-deps 2) (bad-x 3)))
      (list 'error 'invalid-candidate 'deps-malformed (list 'offset (bytevector-length (good 1)))))
(want "a bad third record is named at the sum of the first two lengths"
      (pub (fresh!) (cat wide-first (good 2) (bad-x 3)))
      (list 'error 'invalid-candidate 'payload-not-a-form
            (list 'offset (+ (bytevector-length wide-first) (bytevector-length (good 2))))))

(printf "== E4b: the predicate is exported, and answers as the reader does ==\n")
;; Looked up at run time so that this file still loads on a tree without it:
;; on such a tree this row is red and the rest still read.
(define shared
  (guard (e (#t #f))
    (eval 'record-envelope-refusal (environment '(theourgia wire)))))
(want "record-envelope-refusal is exported by (theourgia wire)"
      (procedure? shared)
      #t)
(printf "== E5c: the writer keeps its own stricter rules, and never hangs ==\n")
(define (encodes? seq ts actor deps payload)
  ;; ENCODED MEANS BYTES THAT READ BACK AS WHAT WAS GIVEN, not "did not
  ;; raise": an encoder returning #f, or bytes of something else, is not an
  ;; encoder that accepted the record.
  (let ((b (guard (e (#t 'refused)) (encode-record seq ts actor deps payload))))
    (cond ((eq? b 'refused) 'refused)
          ((not (bytevector? b)) (list 'not-bytes b))
          ((equal? (decode-line b) (list 'ok seq ts actor deps payload)) 'encoded)
          (else (list 'reads-back-as (decode-line b))))))
(define valid-request-actor
  (list "agent" (cons "id" "req-1") 'single "fp" #f (cons "w" 1)))
(want "the writer refuses a negative ts the reader accepts"
      (encodes? 1 -5 "a" '() '(set "t" x "x")) 'refused)
(want "the writer refuses a pair actor that is not a request actor"
      (encodes? 1 1757300000001 (cons "a" "b") '() '(set "t" x "x")) 'refused)
(want "the writer refuses a negative seq"
      (encodes? -1 1757300000001 "a" '() '(set "t" x "x")) 'refused)
(want "the writer refuses a seq that is not a number"
      (encodes? 'oops 1757300000001 "a" '() '(set "t" x "x")) 'refused)
(want "CONTROL the writer accepts a well-formed request actor"
      (encodes? 1 1757300000001 valid-request-actor '() '(set "t" x "x")) 'encoded)
(want "the writer refuses circular deps, and answers rather than hanging"
      (let ((cyc (list (cons "w" 0))))
        (set-cdr! cyc cyc)
        (let ((out 'hung))
          ((make-engine (lambda () (set! out (encodes? 1 1757300000001 "a" cyc '(set "t" x "x")))))
           1000000 (lambda (ticks v) v) (lambda (k) #f))
          out))
      'refused)

(printf "== E6b: the refusals before the envelope, in their own order ==\n")
(want "an unreadable manifest is answered before a wrong hash"
      (let ((d (fresh!)))
        (system (string-append "mkdir -p " (mdir d)))
        (put! (string-append (mdir d) "/published.sexp") (string->utf8 "((format 1) ("))
        (let ((a (log-publish! d M 1 poison "deadbeef"))) (list (car a) (cadr a))))
      '(refused manifest-unreadable))
(want "the active local segment is answered before a wrong hash"
      (log-publish! (fresh!) W 1 poison "deadbeef")
      '(refused active-writer-segment))
(want "an unreadable manifest is answered before the active local segment"
      (let ((d (fresh!)))
        (put! (string-append d "/writers/" W "/published.sexp") (string->utf8 "((format 1) ("))
        (let ((a (log-publish! d W 1 poison (sha-of poison)))) (list (car a) (cadr a))))
      '(refused manifest-unreadable))

(printf "== E6c: records that do not decode keep today's answers ==\n")
;; Each of these is refused today by a row the envelope row does not
;; replace. The answers are today's, read on 0ce3a02, and must not move.
(define crc-broken-poison
  (let ((b (bytevector-copy (bad-x 1))))
    (bytevector-u8-set! b 12 (if (= 55 (bytevector-u8-ref b 12)) 56 55))
    b))
(want "CONTROL a bad-CRC line whose text is envelope-bad does not decode"
      (car (decode-line crc-broken-poison))
      'bad-crc)
(want "a bad-CRC line whose own text is envelope-bad is refused as today, not by the envelope row"
      (pub (fresh!) (cat crc-broken-poison (good 2)))
      '(error invalid-candidate not-contiguous))
(want "a lone bad-CRC record is refused as today"
      (pub (fresh!) (cat (good 1) crc-broken))
      '(error invalid-candidate not-contiguous))
(want "an envelope-valid gap in the sequence is refused as today"
      (pub (fresh!) (cat (good 1) (good 3)))
      '(error invalid-candidate not-contiguous))

(printf "== E8b: a clean candidate over held poison, and the held bytes after a refusal ==\n")
(define (slurp path)
  (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
    (if (eof-object? b) (make-bytevector 0) b)))
(define (held-file d) (string-append (mdir d) "/" (segment-file-name 1)))
(want "a clean correction of held poison takes today's branch (a fork at the corrected record)"
      (let ((d (fresh!)))
        (plant! d held 1 2)
        (let ((a (pub d (cat (good 1) (good 2))))) (list (car a) (cadr a))))
      '(divergence (fork 2)))
(want "after a refused re-send, the held segment is byte for byte what was planted"
      (let ((d (fresh!)))
        (plant! d held 1 2)
        (let ((a (pub d held)))
          (list (and (pair? a) (car a))
                (equal? (slurp (held-file d)) held)
                (reader-reason (slurp (held-file d))))))
      (list 'error #t (list 'frame 'payload-not-a-form (bytevector-length (good 1)))))
(want "after a refused poison repair, the torn local segment is byte for byte what was planted"
      (let ((d (fresh!)) (planted (cat held torn-tail)))
        (plant! d planted 1 2)
        (pub d (cat held (good 3)))
        (equal? (slurp (held-file d)) planted))
      #t)

(printf "== M: the envelope rule as a matrix, against a written-out oracle ==\n")
;; THE EXAMPLES ABOVE NAME CASES; THIS NAMES THE RULE. Each field gets values
;; the rule accepts and values it refuses, several spellings of each; every
;; record with one field replaced, and every record with two fields replaced
;; (for which reason wins), is built, framed with a correct CRC, and put to
;; four parties: the reader, the exported predicate, publish, and the writer.
;; What each should answer comes from `oracle`, which is R1 of the design
;; written out -- a second statement of the rule, kept here on purpose, so
;; that the product's copy is checked against something that is not itself.
(define (oracle-deps? ds)
  (and (list? ds)
       (for-all (lambda (x) (and (pair? x) (string? (car x))
                                 (integer? (cdr x)) (exact? (cdr x)) (>= (cdr x) 0)))
                ds)))
(define (oracle d)
  (let ((seq (list-ref d 1)) (ts (list-ref d 2)) (actor (list-ref d 3))
        (deps (list-ref d 4)) (payload (list-ref d 5)))
    (cond ((not (and (integer? seq) (exact? seq) (>= seq 0))) 'seq-not-a-number)
          ((not (and (integer? ts) (exact? ts))) 'ts-not-a-number)
          ((not (or (string? actor) (pair? actor))) 'actor-malformed)
          ((not (oracle-deps? deps)) 'deps-malformed)
          ((not (pair? payload)) 'payload-not-a-form)
          (else #f))))
(define fields '(seq ts actor deps payload))
;; THE CODEC'S OWN SPELLING OF THE FLONUM 1.0. An integer-valued inexact
;; number is the one value that tells an exactness test from an integer
;; test, and the decimal text "1.0" does not decode at all -- so without
;; this spelling, deleting every `exact?` in the rule would leave the
;; matrix green (round-4 review, finding 1).
(define flonum-one "#f8\"AAAAAAAA8D8=\"")
(define base-values (list "1" "1757300000001" "\"a\"" "()" good-payload))
(define good-values
  (list (list "1" "0")
        (list "1757300000001" "0" "-5")
        (list "\"a\"" "(\"a\" . \"b\")" "(\"a\" \"b\")" "(a)")
        (list "()" "((\"w\" . 0))" "((\"w\" . 1) (\"v\" . 2))")
        (list good-payload "(x)" "(x . y)" "(42)" "(\"s\" x)")))
(define bad-values
  (list (list "oops" "-1" "3/2" "1.5" "1.0" "+inf.0" "\"1\"" "#t" "()" flonum-one)
        (list "oops" "3/2" "1.5" "+inf.0" "\"t\"" "#t" flonum-one)
        (list "42" "a" "#t" "()" "#(1 2)")
        (list "(7)" (string-append "((\"w\" . " flonum-one "))") "((\"w\" . -1))" "((\"w\" . 3/2))" "((w . 1))" "(\"w\" . 1)" "((\"w\" . 1) . 2)" "7" "\"d\"" "((\"w\" . 0) 7)")
        (list "x" "42" "\"s\"" "()" "#t" "#(1)")))
(define (with-field vals i v)
  (let loop ((k 0) (vs vals))
    (if (null? vs) '() (cons (if (= k i) v (car vs)) (loop (+ k 1) (cdr vs))))))
(define (record-text vals) (apply text vals))
(define matrix
  (append
    (apply append
      (map (lambda (i) (map (lambda (v) (list (list 'one (list-ref fields i) v) (with-field base-values i v)))
                            (append (list-ref good-values i) (list-ref bad-values i))))
           '(0 1 2 3 4)))
    (apply append
      (map (lambda (ij)
             (let ((i (car ij)) (j (cadr ij)))
               (apply append
                 (map (lambda (vi)
                        (map (lambda (vj)
                               (list (list 'two (list-ref fields i) vi (list-ref fields j) vj)
                                     (with-field (with-field base-values i vi) j vj)))
                             (let ((b (list-ref bad-values j))) (list (car b) (cadr b) (caddr b)))))
                      (let ((b (list-ref bad-values i))) (list (car b) (cadr b) (caddr b)))))))
           '((0 1) (0 2) (0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
    ;; AND A NON-DEFAULT ACCEPTED VALUE IN ONE FIELD WITH A REFUSED VALUE IN
    ;; ANOTHER, both ways round, so that a check skipped only when some
    ;; other field takes an unusual accepted value is seen.
    (apply append
      (map (lambda (ij)
             (let ((i (car ij)) (j (cadr ij)))
               (apply append
                 (map (lambda (vi)
                        (map (lambda (vj)
                               (list (list 'mixed (list-ref fields i) vi (list-ref fields j) vj)
                                     (with-field (with-field base-values i vi) j vj)))
                             (let ((b (list-ref bad-values j))) (list (car b) (cadr b) (caddr b)))))
                      (cdr (list-ref good-values i))))))
           '((0 1) (0 2) (0 3) (0 4) (1 0) (1 2) (1 3) (1 4) (2 0) (2 1) (2 3) (2 4)
             (3 0) (3 1) (3 2) (3 4) (4 0) (4 1) (4 2) (4 3))))
    ;; AND EVERY PAIR OF ACCEPTED VALUES, so that a rule refusing two values
    ;; only in combination (a negative ts WITH a pair actor) is seen.
    (apply append
      (map (lambda (ij)
             (let ((i (car ij)) (j (cadr ij)))
               (apply append
                 (map (lambda (vi)
                        (map (lambda (vj)
                               (list (list 'two-good (list-ref fields i) vi (list-ref fields j) vj)
                                     (with-field (with-field base-values i vi) j vj)))
                             (list-ref good-values j)))
                      (list-ref good-values i)))))
           '((0 1) (0 2) (0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))))
(define decoded
  (filter (lambda (m) (eq? 'ok (car (decode-line (line (record-text (cadr m)))))))
          matrix))
(define undecodable
  (filter (lambda (m) (not (eq? 'ok (car (decode-line (line (record-text (cadr m))))))))
          matrix))
(define (dec m) (decode-line (line (record-text (cadr m)))))

(want "CONTROL the matrix holds more than a hundred records"
      (> (length matrix) 100)
      #t)
(want "CONTROL each field has at least one decoded refused value"
      (for-all (lambda (f)
                 (exists (lambda (m) (and (eq? 'one (car (car m))) (eq? f (cadr (car m)))
                                          (eq? (oracle (dec m))
                                               (cdr (assq f '((seq . seq-not-a-number) (ts . ts-not-a-number)
                                                              (actor . actor-malformed) (deps . deps-malformed)
                                                              (payload . payload-not-a-form)))))))
                         decoded))
               fields)
      #t)
(want "CONTROL decoded integer-valued inexact numbers reach seq, ts and a dependency"
      (let ((ii? (lambda (x) (and (number? x) (inexact? x) (integer? x)))))
        (list (exists (lambda (m) (ii? (list-ref (dec m) 1))) decoded)
              (exists (lambda (m) (ii? (list-ref (dec m) 2))) decoded)
              (exists (lambda (m) (let ((ds (list-ref (dec m) 4)))
                                    (and (list? ds) (exists (lambda (x) (and (pair? x) (ii? (cdr x)))) ds))))
                      decoded)))
      '(#t #t #t))
;; WHICH RECORDS DO NOT DECODE IS PINNED, not derived: `decoded` and
;; `undecodable` are split by the decoder under test, so a decoder that
;; began refusing a readable spelling would move records out of M1-M4 and
;; into M5-M6 without any row saying so. This list is the reading on
;; 0ce3a02 (2026-09-23).
(want "CONTROL exactly these records do not decode"
      (map car undecodable)
      '((one seq "1.5") (one seq "1.0") (one ts "1.5") (two seq "oops" ts "1.5") (two seq "-1" ts "1.5") (two seq "3/2" ts "1.5") (two ts "1.5" actor "42") (two ts "1.5" actor "a") (two ts "1.5" actor "#t") (two ts "1.5" deps "(7)") (two ts "1.5" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") (two ts "1.5" deps "((\"w\" . -1))") (two ts "1.5" payload "x") (two ts "1.5" payload "42") (two ts "1.5" payload "\"s\"") (mixed seq "0" ts "1.5") (mixed actor "(\"a\" . \"b\")" ts "1.5") (mixed actor "(\"a\" \"b\")" ts "1.5") (mixed actor "(a)" ts "1.5") (mixed deps "((\"w\" . 0))" ts "1.5") (mixed deps "((\"w\" . 1) (\"v\" . 2))" ts "1.5") (mixed payload "(x)" ts "1.5") (mixed payload "(x . y)" ts "1.5") (mixed payload "(42)" ts "1.5") (mixed payload "(\"s\" x)" ts "1.5")))
(printf "   matrix: ~a records, ~a decode, ~a do not\n"
        (length matrix) (length decoded) (length undecodable))

(define (mismatches f)
  (filter (lambda (x) x) (map f decoded)))

(want "M1 the reader answers every decoded record as the oracle does (set and order)"
      (mismatches
        (lambda (m)
          ;; THE WHOLE RESULT, not its reason alone: a sequence error carries
          ;; no reason, so comparing reasons would read it as acceptance.
          (let ((r (reader-reason (line (record-text (cadr m))))) (o (oracle (dec m))))
            (and (not (equal? r (if o (list 'frame o 0) '(reads complete))))
                 (list (car m) 'reader r 'oracle o)))))
      '())
(want "M2 the exported predicate answers every decoded record as the oracle does"
      (if (procedure? shared)
          (mismatches
            (lambda (m)
              (let ((a (shared (dec m))) (o (oracle (dec m))))
                (and (not (equal? a o)) (list (car m) 'predicate a 'oracle o)))))
          'no-predicate)
      '())
(want "M3 publish refuses exactly the records the oracle refuses, with the oracle's reason"
      (mismatches
        (lambda (m)
          (let* ((d (dec m)) (o (oracle d)))
            (and (or o (eqv? (list-ref d 1) 1))
                 (let* ((d (fresh!))
                        (bytes (line (record-text (cadr m))))
                        (a (caught (pub d bytes)))
                        (want (if o (list 'error 'invalid-candidate o (list 'offset 0)) '(published 1)))
                        (installed (and (not o) (equal? a want)
                                        (let ((f (string-append (mdir d) "/" (segment-file-name 1))))
                                          (and (file-exists? f) (equal? (slurp f) bytes))))))
                   (cond ((not (equal? a want)) (list (car m) 'publish a 'want want))
                         ((and (not o) (not installed)) (list (car m) 'installed-bytes-differ))
                         (else #f)))))))
      '())
(want "M4 the writer refuses every decoded record the oracle refuses"
      (mismatches
        (lambda (m)
          (let ((d (dec m)))
            (and (oracle d)
                 (let ((w (encodes? (list-ref d 1) (list-ref d 2) (list-ref d 3) (list-ref d 4) (list-ref d 5))))
                   (and (not (eq? w 'refused)) (list (car m) 'writer w)))))))
      '())
(want "M5 CONTROL a record that does not decode is named by the reader as a frame error, never an envelope reason"
      (filter (lambda (x) x)
        (map (lambda (m)
               (let ((r (reader-reason (line (record-text (cadr m))))))
                 (and (or (not (eq? (car r) 'frame))
                          (memq (cadr r) '(seq-not-a-number ts-not-a-number actor-malformed
                                           deps-malformed payload-not-a-form)))
                      (list (car m) r))))
             undecodable))
      '())
(want "M6 publish answers a candidate that does not decode as today"
      (let ((as (map (lambda (m) (caught (pub (fresh!) (line (record-text (cadr m)))))) undecodable)))
        (filter (lambda (a) (not (equal? a '(error invalid-candidate not-contiguous)))) as))
      '())

(printf "== M7: the writer's own stricter rules, beyond the reader's ==\n")
(for-each
  (lambda (c)
    (want (format "the writer refuses the actor ~s" (car c))
          (encodes? 1 1757300000001 (car c) '() '(set "t" x "x"))
          'refused))
  (list (list (list "a" "b"))
        (list (list "a" "b" "c" "d" "e" "f"))
        (list (list "agent" (cons "id" "req-1") 'bogus "fp" #f (cons "w" 1)))
        (list (list "agent" (cons "id" "req-1") 'single "fp" #f 7))))

(printf "== M8: the scanner still checks the sequence after the envelope ==\n")
(want "CONTROL an envelope-valid record out of sequence is a seq error, not an envelope one"
      (let ((r (scan-segment (good 1) M 1 2 #f (lambda args 'go))))
        (list (car r) (log-error-kind (cadr r))))
      '(integrity seq))

(printf "== M9: no refusal touches the writer, traced, for every refused case ==\n")
(want "every E1 case, refused, changes and flushes nothing at or under the writer"
      (filter (lambda (x) x)
        (map (lambda (c)
               (let ((d (fresh!)))
                 (system (string-append "mkdir -p " (mdir d)))
                 (let* ((r (traced (lambda () (pub d (line (cadr c))))))
                        (t (map cadr (touches? (cadr r) d))))
                   (and (not (null? t)) (list (cadr c) t)))))
             cases))
      '())
(want "a refused re-send of held poison changes and flushes nothing, manifest included"
      (let ((d (fresh!)))
        (plant! d held 1 2)
        (let ((before (slurp (string-append (mdir d) "/published.sexp"))))
          (let* ((r (traced (lambda () (pub d held))))
                 (t (map cadr (touches? (cadr r) d))))
            (list t (equal? before (slurp (string-append (mdir d) "/published.sexp")))))))
      '(() #t))

(printf "== M10: the clean twins did what their answers say ==\n")
(want "TWIN a clean repair leaves the candidate's bytes as the segment"
      (let ((d (fresh!)) (clean (cat (good 1) (good 2))) (cand (cat (good 1) (good 2) (good 3))))
        (plant! d (cat clean torn-tail) 1 2)
        (let ((a (pub d cand)))
          (list (car a) (equal? (slurp (held-file d)) cand))))
      '(repaired #t))
(want "TWIN a clean correction of held poison writes the fork marker it reports"
      (let ((d (fresh!)))
        (plant! d held 1 2)
        (pub d (cat (good 1) (good 2)))
        (file-exists? (string-append (mdir d) "/quarantine.sexp")))
      #t)

(printf "== M4b: the writer ENCODES what the oracle accepts, within its own stricter rule ==\n")
;; M4 asks only that refusals are refused. This asks the other half: every
;; decoded record the oracle accepts, whose ts is not negative and whose
;; actor is a string (the writer's own rule is stricter there, M7), is
;; encoded -- so a writer that began refusing, say, every non-empty
;; dependency list is seen.
(want "M4b the writer encodes every oracle-accepted record inside its own rule"
      (mismatches
        (lambda (m)
          (let ((d (dec m)))
            (and (not (oracle d)) (>= (list-ref d 2) 0) (string? (list-ref d 3))
                 ;; ENCODED AND READ BACK UNCHANGED, not merely "did not raise".
                 (let ((b (guard (e (#t #f))
                            (encode-record (list-ref d 1) (list-ref d 2) (list-ref d 3) (list-ref d 4) (list-ref d 5)))))
                   (cond ((not b) (list (car m) 'writer 'refused))
                         ((not (equal? (decode-line b) d)) (list (car m) 'round-trip (decode-line b)))
                         (else #f)))))))
      '())
(want "CONTROL M4b is not vacuous: it saw non-empty dependency lists and pair payloads"
      (let ((seen (filter (lambda (m) (let ((d (dec m))) (and (not (oracle d)) (>= (list-ref d 2) 0) (string? (list-ref d 3)))))
                          decoded)))
        (list (exists (lambda (m) (pair? (list-ref (dec m) 4))) seen)
              (exists (lambda (m) (not (list? (list-ref (dec m) 5)))) seen)))
      '(#t #t))

(printf "== M7b: the writer's request-actor rule, field by field ==\n")
(want "the writer refuses a request actor whose plan-event is not an event id"
      (encodes? 1 1757300000001 (list "agent" (cons "id" "req-1") 'single "fp" 7 (cons "w" 1)) '() '(set "t" x "x"))
      'refused)

(printf "== M8b: the reader asks the envelope before the sequence ==\n")
(want "CONTROL an envelope-bad record that is also out of sequence is named for its envelope"
      (let ((r (scan-segment (line (text "1" "0" "42" "()" "(x)")) M 1 2 #f (lambda args 'go))))
        (list (car r) (log-error-kind (cadr r))
              (let ((p (assq 'reason (log-error-detail (cadr r))))) (and p (cdr p)))))
      '(integrity frame actor-malformed))

(printf "== E6d: a malformed seq does not jump the rows before the envelope ==\n")
(define seq-poison (line (text "oops" "1757300000001" "\"a\"" "()" good-payload)))
(want "an unreadable manifest is answered before a seq that is not a number"
      (let ((d (fresh!)))
        (system (string-append "mkdir -p " (mdir d)))
        (put! (string-append (mdir d) "/published.sexp") (string->utf8 "((format 1) ("))
        (let ((a (caught (pub d seq-poison)))) (if (pair? a) (list (car a) (cadr a)) a)))
      '(refused manifest-unreadable))
(want "the active local segment is answered before a seq that is not a number"
      (caught (log-publish! (fresh!) W 1 seq-poison (sha-of seq-poison)))
      '(refused active-writer-segment))
(want "a wrong hash is answered before a seq that is not a number"
      (caught (log-publish! (fresh!) M 1 seq-poison "deadbeef"))
      '(error invalid-candidate sha-mismatch))

(printf "== E6e: a torn candidate keeps today's answer ==\n")
(define torn-frag (let ((g (good 2))) (let ((o (make-bytevector 20))) (bytevector-copy! g 0 o 0 20) o)))
(want "a candidate ending in a torn record is refused as today"
      (caught (pub (fresh!) (cat (good 1) torn-frag)))
      '(error invalid-candidate not-contiguous))

(printf "== M9b: the refused poison repair touches nothing either ==\n")
(want "a refused poison-carrying repair creates, writes and flushes nothing at or under the writer"
      (let ((d (fresh!)))
        (plant! d (cat held torn-tail) 1 2)
        (let* ((r (traced (lambda () (pub d (cat held (good 3))))))
               (t (map cadr (touches? (cadr r) d))))
          t))
      '())

(printf "== M10b: the fork marker says what the answer said ==\n")
(want "TWIN the marker written for a clean correction of held poison names fork 2"
      (let ((d (fresh!)))
        (plant! d held 1 2)
        (let ((a (pub d (cat (good 1) (good 2))))
              (m (call-with-port (open-input-file (string-append (mdir d) "/quarantine.sexp")) read)))
          (list a (assq 'fork (cdr m)))))
      '((divergence (fork 2)) (fork 2)))

(printf "== R6: round-5 review, the rest ==\n")
(want "the first refused record is named even when a later one has an earlier reason"
      (pub (fresh!) (cat (good 1) (bad-x 2) (bad-deps 3)))
      (list 'error 'invalid-candidate 'payload-not-a-form (list 'offset (bytevector-length (good 1)))))
(define parse-fail (line (text "1.5" "1757300000001" "\"a\"" "()" good-payload)))
(want "CONTROL the parse-failing line does not decode"
      (car (decode-line parse-fail))
      'frame-error)
(want "a record that does not parse is passed over, and a later envelope-bad record is named"
      (pub (fresh!) (cat parse-fail (bad-x 2)))
      (list 'error 'invalid-candidate 'payload-not-a-form (list 'offset (bytevector-length parse-fail))))
(want "an envelope-bad record is named even when a torn record follows it"
      (pub (fresh!) (cat (good 1) (bad-x 2) torn-frag))
      (list 'error 'invalid-candidate 'payload-not-a-form (list 'offset (bytevector-length (good 1)))))
(want "CONTROL the scanner advances its expected sequence: a gap at the third record is found there"
      (let ((r (scan-segment (cat (good 1) (good 2) (good 4)) M 1 1 #f (lambda args 'go))))
        (list (car r) (log-error-kind (cadr r)) (log-error-offset (cadr r))))
      (list 'integrity 'seq (+ (bytevector-length (good 1)) (bytevector-length (good 2)))))
(want "CONTROL an out-of-sequence record with bad deps is named for its deps"
      (let ((r (scan-segment (cat (good 1) (line (text "3" "1757300000001" "\"a\"" "(7)" good-payload))) M 1 1 #f (lambda args 'go))))
        (list (car r) (log-error-kind (cadr r)) (let ((p (assq 'reason (log-error-detail (cadr r))))) (and p (cdr p)))))
      '(integrity frame deps-malformed))
(want "CONTROL an out-of-sequence record with a bad payload is named for its payload"
      (let ((r (scan-segment (cat (good 1) (bad-x 3)) M 1 1 #f (lambda args 'go))))
        (list (car r) (log-error-kind (cadr r)) (let ((p (assq 'reason (log-error-detail (cadr r))))) (and p (cdr p)))))
      '(integrity frame payload-not-a-form))
(for-each
  (lambda (actor)
    (want (format "the writer accepts the request actor ~s" actor)
          (encodes? 1 1757300000001 actor '() '(set "t" x "x"))
          'encoded))
  (list (list "agent" (cons "id" "req-1") 'single "fp" (cons "w" 2) (cons "w" 1))
        (list "agent" (cons "id" "req-1") 'plan "fp" #f (cons "w" 1))
        (list "agent" (cons "id" "req-1") 3 "fp" #f (cons "w" 1))
        (list "agent" (cons "id" (list 'batch "b" 0)) 'single "fp" #f (cons "w" 1))))
(want "a clean correction of held poison over a torn tail takes today's branch (a fork, not a repair)"
      (let ((d (fresh!)))
        (plant! d (cat held torn-tail) 1 2)
        (let ((a (pub d (cat (good 1) (good 2) (good 3))))) (list (car a) (cadr a))))
      '(divergence (fork 2)))

(printf "== R7: case review round 6 ==\n")
(want "the exported predicate itself answers on circular deps, within an engine budget"
      (if (procedure? shared)
          (let ((cyc (list (cons "w" 0))))
            (set-cdr! cyc cyc)
            (let ((out 'hung))
              ((make-engine (lambda () (set! out (shared (list 'ok 1 1757300000001 "a" cyc '(x))))))
               1000000 (lambda (ticks v) v) (lambda (k) #f))
              out))
          'no-predicate)
      'deps-malformed)
(want "a later-record refusal on a fresh mirror changes and flushes nothing at or under the writer"
      (let ((d (fresh!)))
        (system (string-append "mkdir -p " (mdir d)))
        (let* ((r (traced (lambda () (pub d (cat (good 1) (bad-x 2))))))
               (t (map cadr (touches? (cadr r) d))))
          (list (car r) t)))
      (list (list 'error 'invalid-candidate 'payload-not-a-form (list 'offset (bytevector-length (good 1)))) '()))
(want "the writer refuses a request actor whose fingerprint is not a string"
      (encodes? 1 1757300000001 (list "agent" (cons "id" "req-1") 'single 42 #f (cons "w" 1)) '() '(set "t" x "x"))
      'refused)
;; THE ORACLE'S VERDICT ON EVERY RECORD, PINNED. M1-M4 compute what each
;; record should get from the record AS DECODED; a decoder that began
;; turning a refused value into an accepted one (a vector payload into a
;; list) would move its own expectation along with it. This literal is the
;; reading on 0ce3a02 of (key . verdict) for every decoded record, so the
;; decoder is held to it too.
(define pinned-verdicts
  '(((one seq "1") . #f) ((one seq "0") . #f) ((one seq "oops") . seq-not-a-number) ((one seq "-1") . seq-not-a-number) ((one seq "3/2") . seq-not-a-number) ((one seq "+inf.0") . seq-not-a-number) ((one seq "\"1\"") . seq-not-a-number) ((one seq "#t") . seq-not-a-number) ((one seq "()") . seq-not-a-number) ((one seq "#f8\"AAAAAAAA8D8=\"") . seq-not-a-number) ((one ts "1757300000001") . #f) ((one ts "0") . #f) ((one ts "-5") . #f) ((one ts "oops") . ts-not-a-number) ((one ts "3/2") . ts-not-a-number) ((one ts "+inf.0") . ts-not-a-number) ((one ts "\"t\"") . ts-not-a-number) ((one ts "#t") . ts-not-a-number) ((one ts "#f8\"AAAAAAAA8D8=\"") . ts-not-a-number) ((one actor "\"a\"") . #f) ((one actor "(\"a\" . \"b\")") . #f) ((one actor "(\"a\" \"b\")") . #f) ((one actor "(a)") . #f) ((one actor "42") . actor-malformed) ((one actor "a") . actor-malformed) ((one actor "#t") . actor-malformed) ((one actor "()") . actor-malformed) ((one actor "#(1 2)") . actor-malformed) ((one deps "()") . #f) ((one deps "((\"w\" . 0))") . #f) ((one deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((one deps "(7)") . deps-malformed) ((one deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((one deps "((\"w\" . -1))") . deps-malformed) ((one deps "((\"w\" . 3/2))") . deps-malformed) ((one deps "((w . 1))") . deps-malformed) ((one deps "(\"w\" . 1)") . deps-malformed) ((one deps "((\"w\" . 1) . 2)") . deps-malformed) ((one deps "7") . deps-malformed) ((one deps "\"d\"") . deps-malformed) ((one deps "((\"w\" . 0) 7)") . deps-malformed) ((one payload "(set \"t\" x \"x\")") . #f) ((one payload "(x)") . #f) ((one payload "(x . y)") . #f) ((one payload "(42)") . #f) ((one payload "(\"s\" x)") . #f) ((one payload "x") . payload-not-a-form) ((one payload "42") . payload-not-a-form) ((one payload "\"s\"") . payload-not-a-form) ((one payload "()") . payload-not-a-form) ((one payload "#t") . payload-not-a-form) ((one payload "#(1)") . payload-not-a-form) ((two seq "oops" ts "oops") . seq-not-a-number) ((two seq "oops" ts "3/2") . seq-not-a-number) ((two seq "-1" ts "oops") . seq-not-a-number) ((two seq "-1" ts "3/2") . seq-not-a-number) ((two seq "3/2" ts "oops") . seq-not-a-number) ((two seq "3/2" ts "3/2") . seq-not-a-number) ((two seq "oops" actor "42") . seq-not-a-number) ((two seq "oops" actor "a") . seq-not-a-number) ((two seq "oops" actor "#t") . seq-not-a-number) ((two seq "-1" actor "42") . seq-not-a-number) ((two seq "-1" actor "a") . seq-not-a-number) ((two seq "-1" actor "#t") . seq-not-a-number) ((two seq "3/2" actor "42") . seq-not-a-number) ((two seq "3/2" actor "a") . seq-not-a-number) ((two seq "3/2" actor "#t") . seq-not-a-number) ((two seq "oops" deps "(7)") . seq-not-a-number) ((two seq "oops" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . seq-not-a-number) ((two seq "oops" deps "((\"w\" . -1))") . seq-not-a-number) ((two seq "-1" deps "(7)") . seq-not-a-number) ((two seq "-1" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . seq-not-a-number) ((two seq "-1" deps "((\"w\" . -1))") . seq-not-a-number) ((two seq "3/2" deps "(7)") . seq-not-a-number) ((two seq "3/2" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . seq-not-a-number) ((two seq "3/2" deps "((\"w\" . -1))") . seq-not-a-number) ((two seq "oops" payload "x") . seq-not-a-number) ((two seq "oops" payload "42") . seq-not-a-number) ((two seq "oops" payload "\"s\"") . seq-not-a-number) ((two seq "-1" payload "x") . seq-not-a-number) ((two seq "-1" payload "42") . seq-not-a-number) ((two seq "-1" payload "\"s\"") . seq-not-a-number) ((two seq "3/2" payload "x") . seq-not-a-number) ((two seq "3/2" payload "42") . seq-not-a-number) ((two seq "3/2" payload "\"s\"") . seq-not-a-number) ((two ts "oops" actor "42") . ts-not-a-number) ((two ts "oops" actor "a") . ts-not-a-number) ((two ts "oops" actor "#t") . ts-not-a-number) ((two ts "3/2" actor "42") . ts-not-a-number) ((two ts "3/2" actor "a") . ts-not-a-number) ((two ts "3/2" actor "#t") . ts-not-a-number) ((two ts "oops" deps "(7)") . ts-not-a-number) ((two ts "oops" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . ts-not-a-number) ((two ts "oops" deps "((\"w\" . -1))") . ts-not-a-number) ((two ts "3/2" deps "(7)") . ts-not-a-number) ((two ts "3/2" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . ts-not-a-number) ((two ts "3/2" deps "((\"w\" . -1))") . ts-not-a-number) ((two ts "oops" payload "x") . ts-not-a-number) ((two ts "oops" payload "42") . ts-not-a-number) ((two ts "oops" payload "\"s\"") . ts-not-a-number) ((two ts "3/2" payload "x") . ts-not-a-number) ((two ts "3/2" payload "42") . ts-not-a-number) ((two ts "3/2" payload "\"s\"") . ts-not-a-number) ((two actor "42" deps "(7)") . actor-malformed) ((two actor "42" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . actor-malformed) ((two actor "42" deps "((\"w\" . -1))") . actor-malformed) ((two actor "a" deps "(7)") . actor-malformed) ((two actor "a" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . actor-malformed) ((two actor "a" deps "((\"w\" . -1))") . actor-malformed) ((two actor "#t" deps "(7)") . actor-malformed) ((two actor "#t" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . actor-malformed) ((two actor "#t" deps "((\"w\" . -1))") . actor-malformed) ((two actor "42" payload "x") . actor-malformed) ((two actor "42" payload "42") . actor-malformed) ((two actor "42" payload "\"s\"") . actor-malformed) ((two actor "a" payload "x") . actor-malformed) ((two actor "a" payload "42") . actor-malformed) ((two actor "a" payload "\"s\"") . actor-malformed) ((two actor "#t" payload "x") . actor-malformed) ((two actor "#t" payload "42") . actor-malformed) ((two actor "#t" payload "\"s\"") . actor-malformed) ((two deps "(7)" payload "x") . deps-malformed) ((two deps "(7)" payload "42") . deps-malformed) ((two deps "(7)" payload "\"s\"") . deps-malformed) ((two deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))" payload "x") . deps-malformed) ((two deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))" payload "42") . deps-malformed) ((two deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))" payload "\"s\"") . deps-malformed) ((two deps "((\"w\" . -1))" payload "x") . deps-malformed) ((two deps "((\"w\" . -1))" payload "42") . deps-malformed) ((two deps "((\"w\" . -1))" payload "\"s\"") . deps-malformed) ((mixed seq "0" ts "oops") . ts-not-a-number) ((mixed seq "0" ts "3/2") . ts-not-a-number) ((mixed seq "0" actor "42") . actor-malformed) ((mixed seq "0" actor "a") . actor-malformed) ((mixed seq "0" actor "#t") . actor-malformed) ((mixed seq "0" deps "(7)") . deps-malformed) ((mixed seq "0" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed seq "0" deps "((\"w\" . -1))") . deps-malformed) ((mixed seq "0" payload "x") . payload-not-a-form) ((mixed seq "0" payload "42") . payload-not-a-form) ((mixed seq "0" payload "\"s\"") . payload-not-a-form) ((mixed ts "0" seq "oops") . seq-not-a-number) ((mixed ts "0" seq "-1") . seq-not-a-number) ((mixed ts "0" seq "3/2") . seq-not-a-number) ((mixed ts "-5" seq "oops") . seq-not-a-number) ((mixed ts "-5" seq "-1") . seq-not-a-number) ((mixed ts "-5" seq "3/2") . seq-not-a-number) ((mixed ts "0" actor "42") . actor-malformed) ((mixed ts "0" actor "a") . actor-malformed) ((mixed ts "0" actor "#t") . actor-malformed) ((mixed ts "-5" actor "42") . actor-malformed) ((mixed ts "-5" actor "a") . actor-malformed) ((mixed ts "-5" actor "#t") . actor-malformed) ((mixed ts "0" deps "(7)") . deps-malformed) ((mixed ts "0" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed ts "0" deps "((\"w\" . -1))") . deps-malformed) ((mixed ts "-5" deps "(7)") . deps-malformed) ((mixed ts "-5" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed ts "-5" deps "((\"w\" . -1))") . deps-malformed) ((mixed ts "0" payload "x") . payload-not-a-form) ((mixed ts "0" payload "42") . payload-not-a-form) ((mixed ts "0" payload "\"s\"") . payload-not-a-form) ((mixed ts "-5" payload "x") . payload-not-a-form) ((mixed ts "-5" payload "42") . payload-not-a-form) ((mixed ts "-5" payload "\"s\"") . payload-not-a-form) ((mixed actor "(\"a\" . \"b\")" seq "oops") . seq-not-a-number) ((mixed actor "(\"a\" . \"b\")" seq "-1") . seq-not-a-number) ((mixed actor "(\"a\" . \"b\")" seq "3/2") . seq-not-a-number) ((mixed actor "(\"a\" \"b\")" seq "oops") . seq-not-a-number) ((mixed actor "(\"a\" \"b\")" seq "-1") . seq-not-a-number) ((mixed actor "(\"a\" \"b\")" seq "3/2") . seq-not-a-number) ((mixed actor "(a)" seq "oops") . seq-not-a-number) ((mixed actor "(a)" seq "-1") . seq-not-a-number) ((mixed actor "(a)" seq "3/2") . seq-not-a-number) ((mixed actor "(\"a\" . \"b\")" ts "oops") . ts-not-a-number) ((mixed actor "(\"a\" . \"b\")" ts "3/2") . ts-not-a-number) ((mixed actor "(\"a\" \"b\")" ts "oops") . ts-not-a-number) ((mixed actor "(\"a\" \"b\")" ts "3/2") . ts-not-a-number) ((mixed actor "(a)" ts "oops") . ts-not-a-number) ((mixed actor "(a)" ts "3/2") . ts-not-a-number) ((mixed actor "(\"a\" . \"b\")" deps "(7)") . deps-malformed) ((mixed actor "(\"a\" . \"b\")" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed actor "(\"a\" . \"b\")" deps "((\"w\" . -1))") . deps-malformed) ((mixed actor "(\"a\" \"b\")" deps "(7)") . deps-malformed) ((mixed actor "(\"a\" \"b\")" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed actor "(\"a\" \"b\")" deps "((\"w\" . -1))") . deps-malformed) ((mixed actor "(a)" deps "(7)") . deps-malformed) ((mixed actor "(a)" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed actor "(a)" deps "((\"w\" . -1))") . deps-malformed) ((mixed actor "(\"a\" . \"b\")" payload "x") . payload-not-a-form) ((mixed actor "(\"a\" . \"b\")" payload "42") . payload-not-a-form) ((mixed actor "(\"a\" . \"b\")" payload "\"s\"") . payload-not-a-form) ((mixed actor "(\"a\" \"b\")" payload "x") . payload-not-a-form) ((mixed actor "(\"a\" \"b\")" payload "42") . payload-not-a-form) ((mixed actor "(\"a\" \"b\")" payload "\"s\"") . payload-not-a-form) ((mixed actor "(a)" payload "x") . payload-not-a-form) ((mixed actor "(a)" payload "42") . payload-not-a-form) ((mixed actor "(a)" payload "\"s\"") . payload-not-a-form) ((mixed deps "((\"w\" . 0))" seq "oops") . seq-not-a-number) ((mixed deps "((\"w\" . 0))" seq "-1") . seq-not-a-number) ((mixed deps "((\"w\" . 0))" seq "3/2") . seq-not-a-number) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" seq "oops") . seq-not-a-number) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" seq "-1") . seq-not-a-number) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" seq "3/2") . seq-not-a-number) ((mixed deps "((\"w\" . 0))" ts "oops") . ts-not-a-number) ((mixed deps "((\"w\" . 0))" ts "3/2") . ts-not-a-number) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" ts "oops") . ts-not-a-number) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" ts "3/2") . ts-not-a-number) ((mixed deps "((\"w\" . 0))" actor "42") . actor-malformed) ((mixed deps "((\"w\" . 0))" actor "a") . actor-malformed) ((mixed deps "((\"w\" . 0))" actor "#t") . actor-malformed) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" actor "42") . actor-malformed) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" actor "a") . actor-malformed) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" actor "#t") . actor-malformed) ((mixed deps "((\"w\" . 0))" payload "x") . payload-not-a-form) ((mixed deps "((\"w\" . 0))" payload "42") . payload-not-a-form) ((mixed deps "((\"w\" . 0))" payload "\"s\"") . payload-not-a-form) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" payload "x") . payload-not-a-form) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" payload "42") . payload-not-a-form) ((mixed deps "((\"w\" . 1) (\"v\" . 2))" payload "\"s\"") . payload-not-a-form) ((mixed payload "(x)" seq "oops") . seq-not-a-number) ((mixed payload "(x)" seq "-1") . seq-not-a-number) ((mixed payload "(x)" seq "3/2") . seq-not-a-number) ((mixed payload "(x . y)" seq "oops") . seq-not-a-number) ((mixed payload "(x . y)" seq "-1") . seq-not-a-number) ((mixed payload "(x . y)" seq "3/2") . seq-not-a-number) ((mixed payload "(42)" seq "oops") . seq-not-a-number) ((mixed payload "(42)" seq "-1") . seq-not-a-number) ((mixed payload "(42)" seq "3/2") . seq-not-a-number) ((mixed payload "(\"s\" x)" seq "oops") . seq-not-a-number) ((mixed payload "(\"s\" x)" seq "-1") . seq-not-a-number) ((mixed payload "(\"s\" x)" seq "3/2") . seq-not-a-number) ((mixed payload "(x)" ts "oops") . ts-not-a-number) ((mixed payload "(x)" ts "3/2") . ts-not-a-number) ((mixed payload "(x . y)" ts "oops") . ts-not-a-number) ((mixed payload "(x . y)" ts "3/2") . ts-not-a-number) ((mixed payload "(42)" ts "oops") . ts-not-a-number) ((mixed payload "(42)" ts "3/2") . ts-not-a-number) ((mixed payload "(\"s\" x)" ts "oops") . ts-not-a-number) ((mixed payload "(\"s\" x)" ts "3/2") . ts-not-a-number) ((mixed payload "(x)" actor "42") . actor-malformed) ((mixed payload "(x)" actor "a") . actor-malformed) ((mixed payload "(x)" actor "#t") . actor-malformed) ((mixed payload "(x . y)" actor "42") . actor-malformed) ((mixed payload "(x . y)" actor "a") . actor-malformed) ((mixed payload "(x . y)" actor "#t") . actor-malformed) ((mixed payload "(42)" actor "42") . actor-malformed) ((mixed payload "(42)" actor "a") . actor-malformed) ((mixed payload "(42)" actor "#t") . actor-malformed) ((mixed payload "(\"s\" x)" actor "42") . actor-malformed) ((mixed payload "(\"s\" x)" actor "a") . actor-malformed) ((mixed payload "(\"s\" x)" actor "#t") . actor-malformed) ((mixed payload "(x)" deps "(7)") . deps-malformed) ((mixed payload "(x)" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed payload "(x)" deps "((\"w\" . -1))") . deps-malformed) ((mixed payload "(x . y)" deps "(7)") . deps-malformed) ((mixed payload "(x . y)" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed payload "(x . y)" deps "((\"w\" . -1))") . deps-malformed) ((mixed payload "(42)" deps "(7)") . deps-malformed) ((mixed payload "(42)" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed payload "(42)" deps "((\"w\" . -1))") . deps-malformed) ((mixed payload "(\"s\" x)" deps "(7)") . deps-malformed) ((mixed payload "(\"s\" x)" deps "((\"w\" . #f8\"AAAAAAAA8D8=\"))") . deps-malformed) ((mixed payload "(\"s\" x)" deps "((\"w\" . -1))") . deps-malformed) ((two-good seq "1" ts "1757300000001") . #f) ((two-good seq "1" ts "0") . #f) ((two-good seq "1" ts "-5") . #f) ((two-good seq "0" ts "1757300000001") . #f) ((two-good seq "0" ts "0") . #f) ((two-good seq "0" ts "-5") . #f) ((two-good seq "1" actor "\"a\"") . #f) ((two-good seq "1" actor "(\"a\" . \"b\")") . #f) ((two-good seq "1" actor "(\"a\" \"b\")") . #f) ((two-good seq "1" actor "(a)") . #f) ((two-good seq "0" actor "\"a\"") . #f) ((two-good seq "0" actor "(\"a\" . \"b\")") . #f) ((two-good seq "0" actor "(\"a\" \"b\")") . #f) ((two-good seq "0" actor "(a)") . #f) ((two-good seq "1" deps "()") . #f) ((two-good seq "1" deps "((\"w\" . 0))") . #f) ((two-good seq "1" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good seq "0" deps "()") . #f) ((two-good seq "0" deps "((\"w\" . 0))") . #f) ((two-good seq "0" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good seq "1" payload "(set \"t\" x \"x\")") . #f) ((two-good seq "1" payload "(x)") . #f) ((two-good seq "1" payload "(x . y)") . #f) ((two-good seq "1" payload "(42)") . #f) ((two-good seq "1" payload "(\"s\" x)") . #f) ((two-good seq "0" payload "(set \"t\" x \"x\")") . #f) ((two-good seq "0" payload "(x)") . #f) ((two-good seq "0" payload "(x . y)") . #f) ((two-good seq "0" payload "(42)") . #f) ((two-good seq "0" payload "(\"s\" x)") . #f) ((two-good ts "1757300000001" actor "\"a\"") . #f) ((two-good ts "1757300000001" actor "(\"a\" . \"b\")") . #f) ((two-good ts "1757300000001" actor "(\"a\" \"b\")") . #f) ((two-good ts "1757300000001" actor "(a)") . #f) ((two-good ts "0" actor "\"a\"") . #f) ((two-good ts "0" actor "(\"a\" . \"b\")") . #f) ((two-good ts "0" actor "(\"a\" \"b\")") . #f) ((two-good ts "0" actor "(a)") . #f) ((two-good ts "-5" actor "\"a\"") . #f) ((two-good ts "-5" actor "(\"a\" . \"b\")") . #f) ((two-good ts "-5" actor "(\"a\" \"b\")") . #f) ((two-good ts "-5" actor "(a)") . #f) ((two-good ts "1757300000001" deps "()") . #f) ((two-good ts "1757300000001" deps "((\"w\" . 0))") . #f) ((two-good ts "1757300000001" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good ts "0" deps "()") . #f) ((two-good ts "0" deps "((\"w\" . 0))") . #f) ((two-good ts "0" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good ts "-5" deps "()") . #f) ((two-good ts "-5" deps "((\"w\" . 0))") . #f) ((two-good ts "-5" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good ts "1757300000001" payload "(set \"t\" x \"x\")") . #f) ((two-good ts "1757300000001" payload "(x)") . #f) ((two-good ts "1757300000001" payload "(x . y)") . #f) ((two-good ts "1757300000001" payload "(42)") . #f) ((two-good ts "1757300000001" payload "(\"s\" x)") . #f) ((two-good ts "0" payload "(set \"t\" x \"x\")") . #f) ((two-good ts "0" payload "(x)") . #f) ((two-good ts "0" payload "(x . y)") . #f) ((two-good ts "0" payload "(42)") . #f) ((two-good ts "0" payload "(\"s\" x)") . #f) ((two-good ts "-5" payload "(set \"t\" x \"x\")") . #f) ((two-good ts "-5" payload "(x)") . #f) ((two-good ts "-5" payload "(x . y)") . #f) ((two-good ts "-5" payload "(42)") . #f) ((two-good ts "-5" payload "(\"s\" x)") . #f) ((two-good actor "\"a\"" deps "()") . #f) ((two-good actor "\"a\"" deps "((\"w\" . 0))") . #f) ((two-good actor "\"a\"" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good actor "(\"a\" . \"b\")" deps "()") . #f) ((two-good actor "(\"a\" . \"b\")" deps "((\"w\" . 0))") . #f) ((two-good actor "(\"a\" . \"b\")" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good actor "(\"a\" \"b\")" deps "()") . #f) ((two-good actor "(\"a\" \"b\")" deps "((\"w\" . 0))") . #f) ((two-good actor "(\"a\" \"b\")" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good actor "(a)" deps "()") . #f) ((two-good actor "(a)" deps "((\"w\" . 0))") . #f) ((two-good actor "(a)" deps "((\"w\" . 1) (\"v\" . 2))") . #f) ((two-good actor "\"a\"" payload "(set \"t\" x \"x\")") . #f) ((two-good actor "\"a\"" payload "(x)") . #f) ((two-good actor "\"a\"" payload "(x . y)") . #f) ((two-good actor "\"a\"" payload "(42)") . #f) ((two-good actor "\"a\"" payload "(\"s\" x)") . #f) ((two-good actor "(\"a\" . \"b\")" payload "(set \"t\" x \"x\")") . #f) ((two-good actor "(\"a\" . \"b\")" payload "(x)") . #f) ((two-good actor "(\"a\" . \"b\")" payload "(x . y)") . #f) ((two-good actor "(\"a\" . \"b\")" payload "(42)") . #f) ((two-good actor "(\"a\" . \"b\")" payload "(\"s\" x)") . #f) ((two-good actor "(\"a\" \"b\")" payload "(set \"t\" x \"x\")") . #f) ((two-good actor "(\"a\" \"b\")" payload "(x)") . #f) ((two-good actor "(\"a\" \"b\")" payload "(x . y)") . #f) ((two-good actor "(\"a\" \"b\")" payload "(42)") . #f) ((two-good actor "(\"a\" \"b\")" payload "(\"s\" x)") . #f) ((two-good actor "(a)" payload "(set \"t\" x \"x\")") . #f) ((two-good actor "(a)" payload "(x)") . #f) ((two-good actor "(a)" payload "(x . y)") . #f) ((two-good actor "(a)" payload "(42)") . #f) ((two-good actor "(a)" payload "(\"s\" x)") . #f) ((two-good deps "()" payload "(set \"t\" x \"x\")") . #f) ((two-good deps "()" payload "(x)") . #f) ((two-good deps "()" payload "(x . y)") . #f) ((two-good deps "()" payload "(42)") . #f) ((two-good deps "()" payload "(\"s\" x)") . #f) ((two-good deps "((\"w\" . 0))" payload "(set \"t\" x \"x\")") . #f) ((two-good deps "((\"w\" . 0))" payload "(x)") . #f) ((two-good deps "((\"w\" . 0))" payload "(x . y)") . #f) ((two-good deps "((\"w\" . 0))" payload "(42)") . #f) ((two-good deps "((\"w\" . 0))" payload "(\"s\" x)") . #f) ((two-good deps "((\"w\" . 1) (\"v\" . 2))" payload "(set \"t\" x \"x\")") . #f) ((two-good deps "((\"w\" . 1) (\"v\" . 2))" payload "(x)") . #f) ((two-good deps "((\"w\" . 1) (\"v\" . 2))" payload "(x . y)") . #f) ((two-good deps "((\"w\" . 1) (\"v\" . 2))" payload "(42)") . #f) ((two-good deps "((\"w\" . 1) (\"v\" . 2))" payload "(\"s\" x)") . #f)))
(want "CONTROL every decoded record gets the verdict read on 0ce3a02"
      (map (lambda (m) (cons (car m) (oracle (dec m)))) decoded)
      pinned-verdicts)

(printf "== R8: case review round 7 ==\n")
(want "the reader delivers no refused record: every matrix record the oracle refuses reaches no consumer"
      (filter (lambda (x) x)
        (map (lambda (m)
               (let* ((delivered '())
                      (r (scan-segment (line (record-text (cadr m))) M 1 #f #f
                                       (lambda (off seq ts actor deps payload)
                                         (set! delivered (cons seq delivered)) 'go))))
                 (and (oracle (dec m)) (not (null? delivered)) (list (car m) 'delivered delivered))))
             decoded))
      '())
(want "CONTROL the same callback does receive an accepted record"
      (let ((delivered '()))
        (scan-segment (good 1) M 1 #f #f (lambda (off seq . rest) (set! delivered (cons seq delivered)) 'go))
        delivered)
      '(1))
;; R3 PERMITS ONE THING HERE: creating the writer's directory, which
;; `mkdir-one!` traces as `(create <that directory>)`. That single event is
;; let through by name; anything else at or under the directory is not.
(want "a refusal for a writer whose directory publish must create stages and flushes nothing under it"
      (let ((d (fresh!)))
        (let* ((r (traced (lambda () (pub d poison))))
               (t (filter (lambda (ev) (not (and (eq? (cadr ev) 'create)
                                                 (equal? (format "~a" (caddr ev)) (mdir d)))))
                          (touches? (cadr r) d))))
          (list (car r) (map cadr t))))
      (list (list 'error 'invalid-candidate 'payload-not-a-form (list 'offset 0)) '()))
(want "the writer refuses a request actor whose who is not a string"
      (encodes? 1 1757300000001 (list 42 (cons "id" "req-1") 'single "fp" #f (cons "w" 1)) '() '(set "t" x "x"))
      'refused)
(want "the writer refuses a request actor whose request id is not a request id"
      (encodes? 1 1757300000001 (list "agent" (cons "id" 42) 'single "fp" #f (cons "w" 1)) '() '(set "t" x "x"))
      'refused)
;; THE WRITER'S BYTES AGAINST THE TEXT THIS FILE WROTE, not against the
;; decoder's reading of it: for every accepted record inside the writer's
;; own rule, encoding the decoded values must give back exactly the line
;; the matrix framed. A decoder that normalised a value would then show as
;; a byte difference here.
(want "M4c the writer reproduces each accepted record's framed line byte for byte"
      (mismatches
        (lambda (m)
          (let ((d (dec m)))
            (and (not (oracle d)) (>= (list-ref d 2) 0) (string? (list-ref d 3))
                 (let ((b (guard (e (#t #f)) (encode-record (list-ref d 1) (list-ref d 2) (list-ref d 3) (list-ref d 4) (list-ref d 5)))))
                   (and (not (equal? b (line (record-text (cadr m)))))
                        (list (car m) 'writer (and b (utf8->string b)))))))))
      '())

(printf "== R9: case review round 8 ==\n")
(want "the writer refuses a request actor whose identity origin is not a string"
      (encodes? 1 1757300000001 (list "agent" (cons 42 "req-1") 'single "fp" #f (cons "w" 1)) '() '(set "t" x "x"))
      'refused)

;; ---- the trace rows' reach, drafts under publish, a mirror already poisoned ---

(printf "== what the trace rows see, drafts under publish, held poison ==\n")

;; NEVER: THE TRACE ROWS SEE EVERY MUTATING DOOR. E7 and M9 decide "nothing
;; was changed" from trace events whose op is in `mutating-ops`. A door that
;; records a mutation (ffi.sc's `note!`) but traces it under an op this list
;; does not name would be invisible to them. The list of such ops is read
;; from ffi.sc as data, at run time: for every definition whose body calls
;; `note!`, the ops of the `trace-event!` calls in that body. A new door with
;; a new op turns this row red rather than passing through E7 unseen.
;;
;; ITS REACH IS SYNTACTIC: it sees a `(note! ...)` written inside a
;; `(define (name ...) ...)` form and a trace op written as a quoted symbol.
;; A door that notes through a helper, computes its op, or is defined as
;; `(define name (lambda ...))` contributes no op, and the control of five
;; still passes; such a door is outside what this row reads.
;;
;; NOTE: A FAILED WRITE IS NOTED, AND ITS FAILING CHUNK IS NOT TRACED
;; (write-all!): each chunk that succeeds is traced as `write`, and a later
;; chunk can then fail after earlier ones reached the file, which is traced
;; only as far as those chunks. That is a reading about the failure path;
;; this row is about op names.
(define (forms-in x)
  (cond ((pair? x) (cons x (append (forms-in (car x)) (forms-in (cdr x)))))
        (else '())))
(define (headed? f name) (and (pair? f) (eq? (car f) name)))
(define (quoted-sym x) (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (symbol? (cadr x)) (cadr x)))
(define ffi-door-ops
  (let* ((lib (call-with-input-file "../ffi.sc"
                (lambda (p)
                  (let loop ()
                    (let ((x (read p)))
                      (if (and (pair? x) (eq? (car x) 'library)) x (if (eof-object? x) #f (loop))))))))
         (defines (filter (lambda (f) (and (headed? f 'define) (pair? (cdr f)) (pair? (cadr f))))
                          (if lib (forms-in lib) '()))))
    (fold-left
      (lambda (acc d)
        (let ((fs (forms-in d)))
          (if (exists (lambda (f) (headed? f 'note!)) fs)
              (fold-left (lambda (acc f)
                           (let ((op (and (headed? f 'trace-event!) (pair? (cdr f)) (quoted-sym (cadr f)))))
                             (if (and op (not (memq op acc))) (append acc (list op)) acc)))
                         acc fs)
              acc)))
      '() defines)))
(printf "   (reading: ops traced by ffi doors that record a mutation: ~s)\n" ffi-door-ops)
(want "every op an ffi door that records a mutation traces is one E7/M9 count as mutating"
      (list (>= (length ffi-door-ops) 5)
            (filter (lambda (op) (not (memq op mutating-ops))) ffi-door-ops))
      '(#t ()))

;; NEVER: PUBLISH DOES NOT CONSUME A WRITER'S DRAFTS. A publish for writer M
;; lands M's received history. The drafts under writers/<writer>/working --
;; the view `--working` reads -- are not its to touch: neither M's, in the
;; directory the publish writes into, nor the local writer's.
;; Each writer gets one draft, written as working.sc writes one (the
;; envelope `(working 1 <writer> <block> <version> <based-on> <cut> <bytes>)`
;; under the block's name, its version from reduce.sc's draft-version), so
;; the product lists it. The row reads the product's own list
;; (working-list) and the directory's bytes before and after the publish.
;; The first value is the CONTROL that the product saw the drafts at all.
(define (listing-with-bytes dir)
  (if (file-directory? dir)
      (map (lambda (f) (cons f (call-with-port (open-file-input-port (string-append dir "/" f)) get-bytevector-all)))
           (list-sort string<? (directory-list dir)))
      'NO-DIRECTORY))
(define (plant-draft! d writer block)
  (let* ((wd (string-append d "/writers/" writer "/working"))
         (bytes (bytevector 1 2))
         (based (make-string 64 #\a))
         (cut '())
         (entry (list 'working 1 writer block (draft-version bytes based cut) based cut bytes)))
    (system (string-append "mkdir -p " wd))
    (put! (string-append wd "/" block) (string->utf8 (sexpr->string-extended (storable-encode entry))))
    wd))
(define (drafts-listed d writer)
  (let ((a (working-list d #f writer)))
    (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (car (cadr a)) 'items))
        (map (lambda (it) (cadr (assq 'block (cdr it))))
             (filter (lambda (it) (and (pair? it) (eq? (car it) 'draft))) (cdr (cadr a))))
        a)))
(want "a publish for a writer leaves the drafts the product lists, for that writer and the local one, and their bytes, as they were"
      (let* ((d (fresh!))
             (mwd (plant-draft! d M "b.1"))
             (wwd (plant-draft! d W "c.1")))
        (let* ((listed-before (list (drafts-listed d M) (drafts-listed d W)))
               (bytes-before (list (listing-with-bytes mwd) (listing-with-bytes wwd)))
               (answer (pub d (good 1)))
               (bytes-after (list (listing-with-bytes mwd) (listing-with-bytes wwd)))
               (listed-after (list (drafts-listed d M) (drafts-listed d W))))
          (list listed-before answer (equal? bytes-before bytes-after) (equal? listed-before listed-after))))
      '((("b.1") ("c.1")) (published 1) #t #t))

;; KNOWN OPEN, STATED: A MIRROR THAT ALREADY HOLDS POISON. Publish refuses to
;; ACCEPT a record its reader refuses; it does not repair one a store
;; accepted before that rule. The row pins what such a mirror answers
;; today, so a change to that answer is a decision somebody makes, not a
;; drift. Today: the reader stops at the held segment's second record, a
;; clean segment 2 published after it answers `(published 2)`, and the held
;; segment still stops the reader afterwards -- the later segment is
;; accepted and does not repair the one before it. The first two values
;; were read on the row's first run and pinned from that reading; the third
;; is the row's claim that nothing was repaired.
(want "KNOWN OPEN a mirror holding poison stays as it was: the reader stops at the poison, a clean later segment is published, and the poison is still there after it"
      (let ((d (fresh!)))
        (plant! d held 1 2)
        (let* ((before (reader-reason (call-with-port (open-file-input-port (held-file d)) get-bytevector-all)))
               (answer (log-publish! d M 2 (good 3) (sha-of (good 3))))
               (after (reader-reason (call-with-port (open-file-input-port (held-file d)) get-bytevector-all))))
          (list before answer after)))
      '((frame payload-not-a-form 50) (published 2) (frame payload-not-a-form 50)))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "record-envelope complete\n")
