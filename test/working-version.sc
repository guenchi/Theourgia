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

;; A DRAFT'S VERSION IS THE NAME OF ITS CONTENT.
;;
;; `version = sha256(bytes || based-on || cut)`. It used to be a fresh
;; counter, which meant two writes of the same bytes on the same
;; baseline had different names and a retry could only say which draft
;; it meant if somebody had stored the correspondence.
;;
;; KEY: THE ADDRESS IS COMPUTED HERE, INDEPENDENTLY. The concatenation
;; order and the encoding of each part are written into this file, not
;; taken from the core -- a row that asked the core for the version and
;; compared it with the core's version would be green for a build that
;; hashed the three inputs in any order at all.
;;
;; THREE INPUTS, THREE ROWS. Each is varied alone: the bytes, the
;; baseline hash, and the cut. A build that forgot one of them passes
;; every row but the one that moves it, and the one it fails is the one
;; that says what a version means.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia digest) (theourgia wire) (theourgia ffi)
        (theourgia request)
        (only (theourgia log) writer-directory)
        (only (theourgia store) store-evidence)
        (only (theourgia working) latest-parent-cut))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/working-version-" (number->string (get-process-id))))
(when (file-exists? root) (error 'working-version "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))

;; KEY: THE WRITER IS NAMED HERE BECAUSE IT IS NO LONGER GUESSED. A draft
;; verb that was not told which writer it speaks for used to fall back to
;; this store's own local log writer, so two agents that never passed
;; `--writer` shared one draft space without either being told. The core
;; now refuses that call instead; naming the same writer the old fallback
;; would have chosen keeps every row below asking what it asked before.
;;
;; NEVER: AND ONLY WHERE IT WAS MISSING: a call that already names a writer is
;; naming it to make a point, and must keep the one it names.
(define draft-verbs '(write restore drafts discard commit))

(define (wants-writer? verb args)
  (or (memq verb draft-verbs)
      (and (eq? verb 'read)
           (or (member "--working" args) (member "--working-info" args)))))

(define (call . args)
  (rpc-dispatch store
                (if (and (wants-writer? (car args) (cdr args))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define (insert title)
  (let ((a (call 'insert "--title" title "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define A (insert "A"))
(define B (insert "B"))
(define (hash id) (block-hash (open-and-reduce store) id))
(define (cut) (reduce-applied-cut (open-and-reduce store)))

;; THE ENVELOPE IS READ AS DATA, from the file the core wrote. A row
;; that asked the core to tell it what it had written would be asking
;; the same build twice.
(define (envelope id)
  (let ((path (string-append (writer-directory store writer) "/working/" id)))
    (and (file-exists? path)
         (call-with-port (open-file-input-port path)
           (lambda (p) (storable-decode
                         (string->sexpr-extended (utf8->string (get-bytevector-all p)))))))))
(define (env-version e) (list-ref e 4))
(define (env-based-on e) (list-ref e 5))
(define (env-cut e) (list-ref e 6))
(define (env-bytes e) (list-ref e 7))

;; KEY: THE ADDRESS FUNCTION, WRITTEN OUT. bytes first, then the baseline
;; hash, then the cut in the store's own encoding, all as one byte
;; string, sha256, hex.
(define (address bytes based-on cut)
  (let* ((tail (string->utf8 (string-append based-on
                                            (sexpr->string-extended (storable-encode cut)))))
         (out (make-bytevector (+ (bytevector-length bytes) (bytevector-length tail)))))
    (bytevector-copy! bytes 0 out 0 (bytevector-length bytes))
    (bytevector-copy! tail 0 out (bytevector-length bytes) (bytevector-length tail))
    (bytevector->hex (sha256 out))))

;; ---- WV-00 / WV-01 the address ------------------------------------------

(want "WV-00 a draft is saved" (rpc-ok? (call 'write A "draft one")) #t)
(define e1 (envelope A))
(want "WV-00 and its envelope is on disk" (and e1 #t) #t)

(want "WV-01 the stored version is sha256(bytes || based-on || cut)"
      (env-version e1)
      (address (env-bytes e1) (env-based-on e1) (env-cut e1)))
(want "WV-01 TWIN: and that function can disagree -- a byte later is a different name"
      (equal? (env-version e1)
              (address (string->utf8 "draft onf") (env-based-on e1) (env-cut e1)))
      #f)
(want "WV-01 the baseline it recorded is the block's hash at the time"
      (env-based-on e1) (hash A))

;; ---- WV-02 same content, same name ---------------------------------------

(call 'write A "something else")
(call 'write A "draft one")
(want "WV-02 the same bytes on the same baseline get the same version"
      (env-version (envelope A)) (env-version e1))

;; ---- WV-03 the bytes ------------------------------------------------------

(call 'write A "draft onf")
(want "WV-03 one byte different is a different version"
      (equal? (env-version (envelope A)) (env-version e1)) #f)
(call 'write A "draft one")

;; ---- WV-04 the cut --------------------------------------------------------
;;
;; T and H are held still and only C moves: a commit to an UNRELATED
;; block advances the store's cut without touching A's hash. A build
;; that left the cut out of the address answers the same name here.

(want "WV-04 the baseline hash is unchanged by an unrelated commit"
      (let ((before (hash A)))
        (call 'set B "src" "B moved on")
        (equal? (hash A) before))
      #t)
(want "WV-04 but the cut is not"
      (equal? (cut) (env-cut e1)) #f)
(call 'discard A)
(call 'write A "draft one")
(want "WV-04 so the same bytes on the same baseline at a later cut are a different version"
      (equal? (env-version (envelope A)) (env-version e1)) #f)

;; ---- WV-05 the baseline ---------------------------------------------------
;;
;; Same bytes, same cut, two blocks whose hashes differ. The block id is
;; NOT an input to the address, so the only thing that differs is H.

(call 'discard A)
(call 'discard B)
(call 'write A "identical text")
(call 'write B "identical text")
(define ea (envelope A))
(define eb (envelope B))
(want "WV-05 the two drafts were written at one cut" (equal? (env-cut ea) (env-cut eb)) #t)
(want "WV-05 with the same bytes" (equal? (env-bytes ea) (env-bytes eb)) #t)
(want "WV-05 TWIN: and different baselines" (equal? (env-based-on ea) (env-based-on eb)) #f)
(want "WV-05 so their versions differ" (equal? (env-version ea) (env-version eb)) #f)

;; ---- WV-06 what the envelope does not carry ------------------------------

(want "WV-06 an envelope is eight fields and none of them is `unchanged`"
      (list (length ea) (car ea) (exists (lambda (x) (eq? x 'unchanged)) ea))
      '(8 working #f))

;; ---- WV-07 a tampered envelope is refused --------------------------------

(define (write-envelope! id e)
  (let ((path (string-append (writer-directory store writer) "/working/" id)))
    (call-with-port (open-file-output-port path (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 (sexpr->string-extended (storable-encode e))))))))

(write-envelope! A (list 'working 1 writer A (env-version ea) (env-based-on ea) (env-cut ea)
                         (string->utf8 "tampered")))
(want "WV-07 an envelope whose bytes do not hash to its version is refused"
      (call 'read A "--working")
      '(error working-unavailable (reason version-mismatch)))
(want "WV-07 and so is listing the drafts"
      (call 'drafts)
      '(error working-unavailable (reason version-mismatch)))
(call 'discard A)

;; ---- WV-08 rewriting what was committed ----------------------------------

(call 'discard B)
(define C (insert "C"))
(call 'write C "fresh text")
(define before-commit (env-version (envelope C)))
(want "WV-08 the draft commits" (rpc-ok? (call 'commit C)) #t)
(call 'write C "fresh text")
(want "WV-08 writing the same bytes again is a NEW version"
      (equal? (env-version (envelope C)) before-commit) #f)
(define (draft-of id)
  (let ((a (call 'drafts)))
    (and (rpc-ok? a)
         (find (lambda (d) (equal? (list 'block id) (assq 'block (cdr d))))
               (cdr (assq 'items (cdr a)))))))
(define (flag d key) (let ((p (assq key (cdr d)))) (and p (cadr p))))
(want "WV-08 and drafts calls it unchanged" (flag (draft-of C) 'unchanged) #t)
(want "WV-08 TWIN: a draft that does differ is not"
      (begin (call 'write C "fresh text!") (flag (draft-of C) 'unchanged)) #f)

;; ---- WV-09 unchanged loses to stale --------------------------------------
;;
;; REGRESSION GUARD, not evidence of a new mechanism: the draft's bytes
;; still equal what it was written on, but the block has moved, so the
;; text it would be compared against is not the one it was written on.

(call 'discard C)
(call 'write C "fresh text")
(want "WV-09 the rewritten draft starts out unchanged" (flag (draft-of C) 'unchanged) #t)
(call 'set C "src" "somebody else committed")
(want "WV-09 after another commit it is no longer unchanged"
      (flag (draft-of C) 'unchanged) #f)
(want "WV-09 and it is not fresh either" (flag (draft-of C) 'fresh) #f)

;; KEY: WV-09b ISOLATES THE BASELINE, WHICH WV-09 DOES NOT. Above, the
;; committed text changed too, so comparing bytes alone already answers
;; "not unchanged" -- a build that never looked at the baseline passes
;; it. Measured: seeding `unchanged?` to ignore `fresh` left every row
;; above green.
;;
;; Here the committed text is set to the SAME bytes the draft holds. A
;; block's hash covers the identity of its candidate set, not only its
;; content, so re-setting the same text moves the hash and leaves the
;; text alone. The draft's bytes now equal the committed src AND its
;; baseline is stale -- and stale is the answer.
(call 'discard C)
(call 'set C "src" "same text")
(call 'write C "same text")
(want "WV-09b the draft matches the committed text" (flag (draft-of C) 'unchanged) #t)
(define h-before (hash C))
(call 'set C "src" "same text")
(want "WV-09b TWIN: re-setting the same text moves the hash"
      (equal? (hash C) h-before) #f)
(want "WV-09b TWIN: and leaves the committed text alone"
      (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce store) C)))))
      "same text")
(want "WV-09b so the draft is stale, not unchanged"
      (list (flag (draft-of C) 'fresh) (flag (draft-of C) 'unchanged))
      '(#f #f))

;; ---- WV-10 committing a draft that changes nothing ------------------------
;;
;; NOTE: THE RULE WAS DERIVED FOR `drafts` AND NOT WIRED INTO `commit`.
;; An unchanged draft still produced a `set` carrying bytes the block
;; already had -- a record that changes nothing, written into the log
;; forever. §7.5.11: no record without `--req`, an empty plan with one.

(define D (insert "D"))
(call 'write D "settled")
(want "WV-10 the draft commits" (rpc-ok? (call 'commit D)) #t)
(define log-path (string-append (writer-directory store writer) "/000001.sexp"))
(define (log-bytes)
  (call-with-port (open-file-input-port log-path) get-bytevector-all))

;; The same bytes again: a new version, and nothing to do.
(call 'write D "settled")
(want "WV-10 TWIN: and drafts calls it unchanged" (flag (draft-of D) 'unchanged) #t)
(define before-noop (log-bytes))
(want "WV-10 committing it answers ok with no items" (call 'commit D) '(ok (items)))
(want "WV-10 and writes nothing at all" (log-bytes) before-noop)
(want "WV-10 TWIN: the draft is retired all the same"
      (rpc-ok? (call 'read D "--working")) #t)
(want "WV-10 TWIN: and it is no longer listed" (draft-of D) #f)

;; With a request id it is a request like any other: an empty plan is its
;; whole durable evidence, and a retry of it replays.
(call 'write D "settled")
(define vD (list-ref (assq 'projection (cdr (call 'read D "--working-info"))) 4))
(define cD (string-append writer ":"
                          (number->string (cdr (assoc writer (reduce-applied-cut (open-and-reduce store)))))))
(define before-empty (log-bytes))
(want "WV-10 with --req it is accepted"
      (rpc-ok? (call 'commit D "--req" "N1" "--cursor" cD "--working-version" vD)) #t)
(want "WV-10 and it DID write, because a request needs its evidence"
      (equal? (log-bytes) before-empty) #f)
(want "WV-10 what it wrote is an empty plan"
      (map (lambda (e) (list (actor-sub (ev-actor e)) (list-ref (ev-payload e) 4)))
           (store-evidence store (cons writer "N1")))
      '((plan ())))

;; ---- WV-P: the parent among several committed cuts -----------------------------
;;
;; Two requests can each complete a plan consuming the same version (two
;; replicas). Their cuts name the parent of the next sequential edit only when
;; one covers the other; when neither does there is no single parent and the
;; answer is #f, which sends the client to an explicit --rebase. Covering is
;; the reduction's cut-covers?, read per writer and never as one number.
(want "WV-P the covering cut is the parent, whichever order the cuts come in"
      (list (latest-parent-cut '((("a" . 2) ("b" . 1)) (("a" . 1))))
            (latest-parent-cut '((("a" . 1)) (("a" . 2) ("b" . 1))))
            (latest-parent-cut '((("a" . 1)) (("a" . 2) ("b" . 1)) (("b" . 1)))))
      '((("a" . 2) ("b" . 1)) (("a" . 2) ("b" . 1)) (("a" . 2) ("b" . 1))))
(want "WV-P two cuts neither covers have no parent, nor has an empty list; one cut is its own"
      (list (latest-parent-cut '((("a" . 5)) (("b" . 1))))
            (latest-parent-cut '((("a" . 1) ("b" . 9)) (("a" . 3) ("b" . 2))))
            (latest-parent-cut '())
            (latest-parent-cut '((("a" . 1)))))
      '(#f #f #f (("a" . 1))))

(printf "rows: ~a\n~a failures\nworking-version complete\n" rows bad)
