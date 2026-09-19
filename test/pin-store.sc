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

;; A STORE WRITTEN BY AN EARLIER BUILD, AND THE HASHES IT HAD THEN.
;;
;; `hash-view.sc` shows that the language table is not in the hash. That
;; is a statement about two readings taken by the SAME build, minutes
;; apart, and it stays true of a build whose hashing has drifted as a
;; whole: both readings drift together.
;;
;; THIS IS THE READING THAT CANNOT DRIFT WITH US. `pin-store/` holds a
;; store written on 2026-09-17, and its two block hashes and its state
;; hash were computed BY THE LIBRARY AT `d1655fe` -- the commit at which
;; the core still carried its own copies of the three vendored
;; libraries, before `digest.sc`, `wire.sc` and `ffi.sc` became forwards
;; onto igropyr and before the derived fields left the reduction.
;;
;; KEY: THE EXPECTATIONS COME FROM OUTSIDE THE TREE. All three were
;; computed by `mkpin.ss` against the library at d1655fe and written
;; into the brief for this batch; the store's own bytes are pinned
;; beside them in `pin-store.md5`. Nothing this suite runs produces
;; either file.
;;
;; NEVER: SO THEY ARE NOT EDITED TO MATCH A READING. If the code changes
;; what a stored block hashes to, the honest outcomes are a red row and
;; a ruling. An earlier draft of this fixture asserted sixty-four hex
;; digits of which it had been told eight and had invented fifty-six;
;; the prefixes matched and the tails did not, which is how that was
;; caught. The digits below are the recorded ones.

;; THE SUBJECT IS CHECKED BEFORE THE READING. `pin-store.md5` is the
;; recorded digest of every file in the store, and it is compared first:
;; a hash taken over a store that is no longer the pinned one would be a
;; confident number about the wrong thing.

;; NEVER: THE STORE IS COPIED BEFORE IT IS OPENED. Opening takes a lock and
;; may write a snapshot; a fixture that did that to `pin-store/` would
;; leave the pin different from the pin the next run measures, and the
;; drift would be ours.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia ffi))

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

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))

;; THE STORE SITS UNDER `vectors/`, with the other data this suite reads
;; rather than generates.
(define vectors-dir (string-append script-dir "/vectors"))
(define pin (string-append vectors-dir "/pin-store"))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/pin-store-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define work (string-append root "/store"))
(system (string-append "cp -R '" pin "' '" work "'"))

(want "PS-00 the pinned store is carried with this delivery"
      (file-exists? (string-append pin "/writers/cjk33185/000001.sexp")) #t)
(want "PS-00 and the copy this fixture opens is not it"
      (string=? work pin) #f)

;; THE SUBJECT, FIRST. `md5 -c` is not portable; the recorded file is
;; read and each line checked with the platform's own `md5 -q`.
(define (recorded-md5s)
  (call-with-input-file (string-append vectors-dir "/pin-store.md5")
    (lambda (p)
      (let loop ((out '()))
        (let ((line (get-line p)))
          (if (eof-object? line)
              (reverse out)
              (let ((sp (let scan ((i 0))
                          (cond ((>= i (string-length line)) #f)
                                ((char=? #\space (string-ref line i)) i)
                                (else (scan (+ i 1)))))))
                (loop (if sp
                          (cons (cons (substring line 0 sp)
                                      (substring line (+ sp 1) (string-length line)))
                                out)
                          out)))))))))

(define (md5-of path)
  (let* ((tmp (string-append root "/md5.txt")))
    (system (string-append "md5 -q '" path "' > '" tmp "'"))
    (call-with-input-file tmp get-line)))

(want "PS-00 every file of the carried store matches the recorded digest"
      (filter (lambda (pair)
                (not (equal? (md5-of (string-append vectors-dir "/" (cdr pair)))
                             (car pair))))
              (recorded-md5s))
      '())

;; AND A WRONG SUBJECT ENDS THE FIXTURE HERE, with its counter and its
;; sentinel printed. Left to continue, the tampered store raised inside
;; `open-and-reduce` and the run ended with no sentinel at all -- which
;; the suite reports as a fixture that died, not as the one row that had
;; already said exactly what was wrong.
(when (> bad 0)
  (printf "rows: ~a\n~a failures\npin-store complete\n" rows bad)
  (exit 1))

;; NEVER: AND IT IS CHECKED BEFORE THE STORE IS OPENED, WHICH IS WHERE THE
;; FIRST VERSION HAD IT WRONG. The row sat after `open-and-reduce`, and
;; a byte appended to `meta.sexp` made the open RAISE -- so the fixture
;; died two rows in, printed no sentinel, and the guard written to name
;; a tampered store never ran. A check placed after the thing it is
;; meant to survive is not a check.

(define state (open-and-reduce work))
(define ids (map cadr (state-datum state)))

(want "PS-00 the pinned store still reduces to the two blocks it held"
      ids '("cjk33185.1" "cjk33185.2"))

;; THE THREE DIGESTS, as recorded.
(printf "measured cjk33185.1 ~a\n" (block-hash state "cjk33185.1"))
(printf "measured cjk33185.2 ~a\n" (block-hash state "cjk33185.2"))
(printf "measured state      ~a\n" (state-hash state))

(want "PS-01 the first block hashes as it did before the forwards"
      (block-hash state "cjk33185.1")
      "d2c7fdcb8ad1285bf9d4c5d285415a0cb8be25641fadcbc58b7319f3c0acf6c7")
(want "PS-01 the second block hashes as it did before the forwards"
      (block-hash state "cjk33185.2")
      "6a7791c72da314539f40408a70a90fc33efe1913f7568f3c29a4c320096a0ca7")
(want "PS-02 and so does the state as a whole"
      (state-hash state)
      "0f84adb1d090dda0fb2ae1bd1ca01c7fcd5f8e0176234700584d0796bbfd34c3")

;; TWIN: THE COMPARISON CAN FAIL. Two blocks of the one store, read the
;; same way, must not answer alike -- otherwise every row above would be
;; green against a `block-hash` that returned a constant.
(want "PS-03 TWIN: two blocks of the pinned store do not share a hash"
      (equal? (block-hash state "cjk33185.1") (block-hash state "cjk33185.2"))
      #f)

(printf "rows: ~a\n~a failures\npin-store complete\n" rows bad)
