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

;; THE SEGMENTS A SCAN IS TRIED ON, IN ONE PLACE. The shapes come from two
;; fixtures that each build their own: log2.sc (three records with
;; multibyte text, truncation inside the last one, a flipped payload byte
;; and CRC digit, sequence gaps, a zeroed page under a record that spans
;; four) and record-envelope.sc (each envelope refusal as a line framed by
;; hand with a correct CRC, and the records the reader accepts that the
;; writer would not write: sequence zero, a negative or zero timestamp, a
;; list actor, non-empty dependencies). Here every damage, and a validly
;; checksummed witness of every frame reason the scanner reaches
;; (crc-field, encoding, padding, parse, shape), is placed at the first, a
;; middle and the last record, and each segment says what a scan of it
;; answers, so a fixture can check that the corpus is damaged as it claims
;; before it reasons over it.
;;
;; Included, not imported: the including file imports (chezscheme),
;; (theourgia wire) and (theourgia crc32).
;;
;;   segment-corpus   ((name bytes class) ...), where class is what
;;                    scan-segment with no expected sequence and a
;;                    recoverable tail answers: complete, torn, or the
;;                    kind of the integrity error (crc, frame, seq)

(define (corpus-cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs)
          o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))

(define (corpus-take bv k)
  (let ((o (make-bytevector k))) (bytevector-copy! bv 0 o 0 k) o))

(define (corpus-flip bv i)
  (let ((c (bytevector-copy bv)))
    (bytevector-u8-set! c i (if (= 65 (bytevector-u8-ref c i)) 66 65))
    c))

(define (corpus-zero-page bv p)
  (let ((c (bytevector-copy bv)))
    (do ((i (* p 4096) (+ i 1)))
        ((or (>= i (* (+ p 1) 4096)) (>= i (bytevector-length c))) c)
      (bytevector-u8-set! c i 0))))

;; A record as the writer frames it (log2.sc's `rec`).
(define (corpus-rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))

;; A line framed by hand (record-envelope.sc's `line`): a correct CRC over
;; whatever text it is given, so the only thing wrong is the text.
(define (corpus-line text)
  (string->utf8 (string-append (crc32-string-hex text) " " text "\n")))
(define (corpus-line-raw raw)
  (corpus-cat (string->utf8 (string-append (crc32-hex raw) " ")) raw (u8-list->bytevector '(10))))
(define corpus-payload "(set \"t\" x \"x\")")
(define (corpus-text seq ts actor deps payload)
  (string-append "(" seq " " ts " " actor " " deps " " payload ")"))
(define (corpus-good seq)
  (corpus-line (corpus-text (number->string seq) "1757300000001" "\"a\"" "()" corpus-payload)))
;; Three records numbered from `first`, each with these fields.
(define (corpus-three-of first ts actor deps)
  (apply corpus-cat
         (map (lambda (i)
                (corpus-line (corpus-text (number->string (+ first i)) ts actor deps corpus-payload)))
              '(0 1 2))))

;; record-envelope.sc's `cases`: one text per refusal.
(define corpus-refusals
  (list (list 'payload-not-a-form (corpus-text "1" "1757300000001" "\"a\"" "()" "x"))
        (list 'payload-not-a-form-number (corpus-text "1" "1757300000001" "\"a\"" "()" "42"))
        (list 'deps-malformed (corpus-text "1" "1757300000001" "\"a\"" "(7)" corpus-payload))
        (list 'ts-not-a-number (corpus-text "1" "oops" "\"a\"" "()" corpus-payload))
        (list 'actor-malformed (corpus-text "1" "1757300000001" "42" "()" corpus-payload))
        (list 'seq-not-a-number (corpus-text "oops" "1757300000001" "\"a\"" "()" corpus-payload))))

;; Three good lines numbered 1 to 3, the one at `k` (1, 2 or 3) replaced.
(define (corpus-three-with k bad)
  (apply corpus-cat (map (lambda (i) (if (= i k) bad (corpus-good i))) '(1 2 3))))

(define corpus-r1 (corpus-rec 1 '(put "k3m9x2qa.1" ((title . "a")))))
(define corpus-r2 (corpus-rec 2 '(put "k3m9x2qa.2" ((title . "b")))))
(define corpus-r3
  (corpus-rec 3 (list 'set "k3m9x2qa.1" 'title "a2\x00b7;\x65e5;\x672c;\x8a9e;\x1f600;")))
(define corpus-fx3 (corpus-cat corpus-r1 corpus-r2 corpus-r3))
(define corpus-o3 (+ (bytevector-length corpus-r1) (bytevector-length corpus-r2)))
(define corpus-l3 (bytevector-length corpus-r3))

(define (corpus-run . seqs)
  (apply corpus-cat (map (lambda (s) (corpus-rec s (list 'put "b" '()))) seqs)))

(define corpus-pages
  (corpus-cat (corpus-rec 1 (list 'put "k3m9x2qa.1" (list (cons 'body (make-string 12500 #\x)))))
              (corpus-rec 2 '(put "k3m9x2qa.2" ()))))

(define corpus-positions '((first . 1) (middle . 2) (last . 3)))

(define (corpus-at-each name class bad-for)
  (map (lambda (p)
         (list (string-append name "-" (symbol->string (car p)))
               (corpus-three-with (cdr p) (bad-for (cdr p)))
               class))
       corpus-positions))

(define segment-corpus
  (append
    (list
      (list "clean-three-multibyte" corpus-fx3 'complete)
      (list "empty" (make-bytevector 0) 'complete)
      (list "clean-one" (corpus-run 1) 'complete)
      (list "clean-six" (corpus-run 1 2 3 4 5 6) 'complete)
      (list "clean-starts-at-six" (corpus-run 6 7 8) 'complete)
      (list "clean-hand-framed" (corpus-three-with 0 #f) 'complete)
      (list "residual-one-byte" (corpus-take corpus-fx3 (+ corpus-o3 1)) 'torn)
      (list "residual-inside-multibyte" (corpus-take corpus-fx3 (+ corpus-o3 corpus-l3 -2)) 'torn)
      (list "residual-many" (corpus-take corpus-fx3 (+ corpus-o3 corpus-l3 -1)) 'torn)
      (list "residual-only" (corpus-take corpus-r1 5) 'torn)
      (list "seq-gap-1-2-4" (corpus-run 1 2 4) 'seq)
      (list "seq-repeat-1-2-2" (corpus-run 1 2 2) 'seq)
      (list "seq-reversal-1-2-1" (corpus-run 1 2 1) 'seq)
      (list "seq-gap-middle-then-more" (corpus-run 1 3 4 5) 'seq)
      (list "reader-sequence-zero" (corpus-three-of 0 "1757300000001" "\"a\"" "()") 'complete)
      (list "reader-negative-timestamp" (corpus-three-of 1 "-1" "\"a\"" "()") 'complete)
      (list "reader-zero-timestamp" (corpus-three-of 1 "0" "\"a\"" "()") 'complete)
      (list "reader-pair-actor" (corpus-three-of 1 "1757300000001" "(\"a\" \"b\")" "()") 'complete)
      (list "reader-request-actor"
            (corpus-three-of 1 "1757300000001" "(\"agent\" \"a\" \"b\" \"c\" \"d\" \"e\")" "()") 'complete)
      (list "reader-deps"
            (corpus-three-of 1 "1757300000001" "\"a\"" "((\"w\" . 0) (\"v\" . 3))") 'complete)
      (list "page-model-clean" corpus-pages 'complete)
      (list "page-model-zeroed-page" (corpus-zero-page corpus-pages 1) 'crc))
    (corpus-at-each "crc-payload-byte" 'crc (lambda (k) (corpus-flip (corpus-good k) 30)))
    (corpus-at-each "crc-digit" 'crc (lambda (k) (corpus-flip (corpus-good k) 1)))
    (corpus-at-each "frame-crc-field" 'frame (lambda (k) (string->utf8 "short\n")))
    (corpus-at-each "frame-encoding" 'frame
                    (lambda (k)
                      (corpus-line-raw
                        (corpus-cat (string->utf8 (string-append "(" (number->string k)
                                                                 " 1757300000001 \"a\" () (set \"t\" x \""))
                                    (u8-list->bytevector '(255))
                                    (string->utf8 "\"))")))))
    (corpus-at-each "frame-padding" 'frame
                    (lambda (k)
                      (corpus-line (string-append " " (corpus-text (number->string k) "1757300000001"
                                                                   "\"a\"" "()" corpus-payload)))))
    (corpus-at-each "frame-shape" 'frame
                    (lambda (k) (corpus-line (string-append "(" (number->string k) " 3)"))))
    (corpus-at-each "frame-parse" 'frame
                    (lambda (k) (corpus-line (string-append "(" (number->string k)
                                                            " 1757300000001 \"a\" () (set \"t\" x \"x\")"))))
    (corpus-at-each "seq-numbered-wrong" 'seq (lambda (k) (corpus-good (+ k 5))))
    (apply append
           (map (lambda (r)
                  (corpus-at-each (string-append "envelope-" (symbol->string (car r))) 'frame
                                  (lambda (k) (corpus-line (cadr r)))))
                corpus-refusals))))
