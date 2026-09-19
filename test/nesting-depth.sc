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

;; THREE EDGES, AND A BAND BETWEEN TWO OF THEM.
;;
;; A value can be too deeply nested for this store's codec. What happens
;; then is not one thing, it is three, and they happen at three
;; different depths:
;;
;;   up to 58   the block is written, hashed and described normally
;;   59 and 60  the block IS WRITTEN and cannot be described: the answer
;;              carries `(state unavailable (reason ...))` and a
;;              `snapshot` of that store answers `(error internal ...)`
;;   61 and up  the RECORD codec refuses the write; nothing is stored
;;
;; KEY: THE MIDDLE BAND IS THE INTERESTING ONE, and it is the reason this
;; fixture exists rather than a single row. A store that refused
;; everything from 59 upwards would pass any row written only about
;; "deep values fail"; so would one that wrote everything and described
;; nothing. The band says the two limits are DIFFERENT limits, two
;; levels apart, and that a block can be durable and indescribable at
;; the same time -- which is the property `cli3` relies on.
;;
;; WHERE THE NUMBERS COME FROM. Two measurements, six apart: the bare
;; codec's last accepted depth is 64 (ND-06 finds the first refusal at
;; 65), and a block's is 58. The six is the wrapper a field value sits
;; inside --
;;
;;     (block <id> (fields ((<name> ((<value> <writer> . <seq>))))))
;;
;; -- so 58 and 59 are a consequence of 64 and 65 rather than a second,
;; unrelated limit. ND-06 is in this file so that both ends of that
;; subtraction are measured by the same run.
;;
;; NOTE: THEY ARE MEASURED, NOT DERIVED, and they are properties of the
;; codec this core now forwards to. They moved once already: the values
;; here were re-measured after `wire.sc` became a forward onto igropyr,
;; and came back the same.
;;
;; THE CLI IS DRIVEN THROUGH A SUBPROCESS, in the shape `cli1`, `cli2`
;; and `cli3` each use. A shared helper would have to live in a library
;; this directory can import, and the fixtures here are run with the
;; library path pointing at the CORE, not at the fixtures.

(import (chezscheme) (theourgia wire))

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
(define cli (string-append script-dir "/../cli.sc"))

(define scratch (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                               "/nesting-depth-" (number->string (get-process-id))))
(system (string-append "rm -rf " scratch "; mkdir -p " scratch "/home"))
(define home (string-append scratch "/home"))
(define out-path (string-append scratch "/out.txt"))
(define in-path (string-append scratch "/stdin.txt"))

(define (text-of path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((out ""))
        (let ((line (get-line p)))
          (if (eof-object? line) out (loop (string-append out line "\n"))))))))

(define (last-datum path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((last #f))
        (let ((x (guard (e (#t 'unreadable)) (read p))))
          (cond ((eof-object? x) last)
                ((eq? x 'unreadable) last)
                (else (loop x))))))))

(define n-store 0)
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    d))

(define (run store . args)
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
            "THEOURGIA_HOME=" home " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " > " out-path " 2>&1"))
  (last-datum out-path))

(define (run-piped store text . args)
  (system (string-append "rm -f " in-path))
  (call-with-port (open-file-output-port in-path (file-options no-fail)
                                         'block (native-transcoder))
    (lambda (o) (put-string o text)))
  (system (string-append
            "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
            "THEOURGIA_HOME=" home " scheme --script " cli " "
            (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
            "--store " store " < " in-path " > " out-path " 2>&1"))
  (last-datum out-path))

(define (lines-of text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((>= i (string-length text))
           (reverse (if (> i start) (cons (substring text start i) out) out)))
          ((char=? #\newline (string-ref text i))
           (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))

;; THE OUTLINE IS READ AS LINES, NOT AS A DATUM. It prints `- <id>` per
;; block, which is not a datum at all; `last-datum` on that output
;; returned the symbol from the machine-home banner, and the row that
;; asked whether a block was on the disk answered `theourgia`.
(define (outline-ids store)
  (run store "outline")
  (filter (lambda (l) (and (> (string-length l) 2) (string=? "- " (substring l 0 2))))
          (lines-of (text-of out-path))))

(define (nest n)
  (let loop ((i 0) (out "0"))
    (if (= i n) out (loop (+ i 1) (string-append "#(" out ")")))))

(define (intent n)
  (string-append "((insert root #f ((kind . section) (title . " (nest n) "))))"))

;; ONE STORE PER DEPTH. Sending two intents to one store would make the
;; second row read a state the first row's block is also in, and
;; `unavailable` is a property of the WHOLE state section.
(define (at-depth n)
  (let ((d (fresh-store!)))
    (run d "init")
    (let ((answer (run-piped d (intent n) "batch" "--actor" "t")))
      (list answer (length (outline-ids d))))))

(define (state-of answer)
  (and (pair? answer) (eq? 'batch (car answer))
       (pair? (cadr answer)) (pair? (car (cadr answer)))
       (let ((one (car (cadr answer))))
         (and (eq? 'ok (car one))
              (let ((p (assq 'state (cdr one)))) (and p (cdr p)))))))

(define (refusal-of answer)
  (and (pair? answer) (eq? 'batch (car answer))
       (pair? (cadr answer)) (pair? (car (cadr answer)))
       (let ((one (car (cadr answer))))
         (and (eq? 'error (car one)) (cdr one)))))

;; ND-00. THE INSTRUMENT, AT A DEPTH NOTHING REFUSES.
(define shallow (at-depth 3))
(want "ND-00 a shallow value is written and described"
      (let ((st (state-of (car shallow))))
        (and (pair? st) (pair? (car st)) (pair? (caar st)) #t))
      #t)

;; ND-01. THE LAST DEPTH THAT IS DESCRIBED.
(define d58 (at-depth 58))
(want "ND-01 58 levels still produce a state hash"
      (let ((st (state-of (car d58))))
        (and (pair? st) (pair? (car st)) (pair? (caar st)) #t))
      #t)

;; ND-02, ND-03. THE BAND: WRITTEN, NOT DESCRIBED. Two depths, because
;; one would not show that this is a band rather than a single value the
;; codec happens to dislike.
(define d59 (at-depth 59))
(define d60 (at-depth 60))
(want "ND-02 59 levels are written but cannot be described"
      (state-of (car d59))
      '(unavailable (reason "nesting too deep (cyclic data?)")))
(want "ND-02 and the block is on the disk all the same"
      (cadr d59) 1)
(want "ND-03 60 levels behave the same way"
      (state-of (car d60))
      '(unavailable (reason "nesting too deep (cyclic data?)")))

;; ND-04. THE SECOND EDGE: THE RECORD CODEC ITSELF.
(define d61 (at-depth 61))
(want "ND-04 61 levels are refused by the record codec"
      (refusal-of (car d61)) '(refused unframable))
(want "ND-04 TWIN: and nothing was written"
      (cadr d61) 0)

;; ND-05. THE OTHER READER. `snapshot` builds every row it can reach, so
;; the refusal escapes to the CLI's last resort rather than to the
;; answer's own fallback -- a different code path for the same fact.
(define snap-store (fresh-store!))
(want "ND-05 a store holding a 59-level value cannot be snapshotted"
      (begin (run snap-store "init")
             (run-piped snap-store (intent 59) "batch" "--actor" "t")
             (run snap-store "snapshot"))
      '(error internal (condition "nesting too deep (cyclic data?)")))

;; ND-06. WHERE THE LIMIT ACTUALLY LIVES. The block wrapper is six
;; levels; this is the same limit read without it, which is what makes
;; 58/59 a consequence rather than a coincidence.
(define (codec-edge)
  (let loop ((n 60))
    (cond ((> n 70) 'no-edge)
          ((guard (e (#t #f))
             (begin (sexpr->string-extended
                      (storable-encode
                        (let build ((i 0) (v 0)) (if (= i n) v (build (+ i 1) (vector v))))))
                    #t))
           (loop (+ n 1)))
          (else n))))

(want "ND-06 the bare codec refuses at 65 levels" (codec-edge) 65)

(printf "rows: ~a\n~a failures\nnesting-depth complete\n" rows bad)
