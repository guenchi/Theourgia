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
(library (theourgia source-lex)
  (export source-ledger source-coordinate unsafe-numeric-token?)
  (import (rnrs) (theourgia text-code))

  (define (source-error reason offset)
    (raise (list 'error 'bad-source (list 'reason reason) (list 'character-offset offset))))
  (define (decimal? c) (char<=? #\0 c #\9))
  (define (unsafe-numeric-token? token)
    (let* ((s (string-downcase token)) (n (string-length s)))
      (define (at? p i) (string-prefix-at? s p i))
      (define (numeric-start? i)
        (and (< i n)
             (or (decimal? (string-ref s i))
                 (and (memv (string-ref s i) '(#\+ #\- #\.))
                      (< (+ i 1) n)
                      (or (decimal? (string-ref s (+ i 1)))
                          (and (memv (string-ref s i) '(#\+ #\-))
                               (or (at? "." (+ i 1)) (at? "inf.0" (+ i 1)) (at? "nan.0" (+ i 1)))))))))
      (let prefix ((i 0) (radix 10) (forced? #f))
        (cond
          ((and (< (+ i 1) n) (char=? (string-ref s i) #\#)
                (memv (string-ref s (+ i 1)) '(#\e #\i #\b #\o #\d #\x)))
           (prefix (+ i 2) (case (string-ref s (+ i 1)) ((#\b) 2) ((#\o) 8) ((#\d) 10) ((#\x) 16) (else radix)) #t))
          ((not (or forced? (numeric-start? i))) #f)
          ((> n 256) #t)
          ((not (= radix 10)) #f)
          (else
           ;; Scientific exponents are accumulated only while <=1000. No
           ;; string->number runs before this whole-file admission pass.
           (let scan ((j i))
             (and (< j n)
                  (or
                    (and (memv (string-ref s j) '(#\e #\s #\f #\d #\l))
                         (let* ((begin (+ j 1))
                                (begin (if (and (< begin n) (memv (string-ref s begin) '(#\+ #\-))) (+ begin 1) begin)))
                           (let digits ((k begin) (value 0) (count 0))
                             (cond ((> value 1000) #t)
                                   ((and (< k n) (decimal? (string-ref s k)))
                                    (or (> count 4) (digits (+ k 1) (+ (* value 10) (- (char->integer (string-ref s k)) 48)) (+ count 1))))
                                   (else #f)))))
                    (scan (+ j 1))))))))))

  (define (source-coordinate text offset)
    (let loop ((i 0) (line 1) (column 1) (bytes 0))
      (if (= i offset) (list bytes line column)
          (let* ((c (string-ref text i)) (width (bytevector-length (string->utf8 (string c)))))
            (cond ((char=? c #\newline) (loop (+ i 1) (+ line 1) 1 (+ bytes width)))
                  ((and (char=? c #\return) (or (= (+ i 1) (string-length text)) (not (char=? (string-ref text (+ i 1)) #\newline))))
                   (loop (+ i 1) (+ line 1) 1 (+ bytes width)))
                  (else (loop (+ i 1) line (+ column 1) (+ bytes width))))))))

  ;; The ledger records lexical positions. Chez remains the datum reader.
  ;; #; contents are scanned for unsafe tokens even though they are discarded.
  (define (source-ledger bytes)
    (unless (and (bytevector? bytes) (<= (bytevector-length bytes) 1048576)) (source-error 'input-limit 0))
    (let ((s (safe-utf8 bytes)) (comments '()) (discards '()) (prefixes 0) (run-start #f))
      (unless s (source-error 'invalid-utf8 0))
      (let ((n (string-length s)))
        (define (at? p i) (string-prefix-at? s p i))
        ;; NEVER: THE LIMIT IS ON A RUN OF CONSECUTIVE PREFIXES, NOT ON THE FILE'S
        ;; TOTAL (F51). What a prefix can cost -- a quotation nested inside a
        ;; quotation, a discard of a discard -- grows with the run; a total
        ;; counted every quote in a thousand-line library, and log.sc was
        ;; refused at 43 percent of its length. The run is reset by any token
        ;; that is not a prefix (`end-run!`); whitespace and comments between
        ;; two prefixes do not end it. The refusal names the run's first prefix.
        (define (prefix! i)
          (unless run-start (set! run-start i))
          (set! prefixes (+ prefixes 1))
          (when (> prefixes 256) (source-error 'prefix-limit run-start)))
        (define (end-run!)
          (set! prefixes 0)
          (set! run-start #f))
        (define (delimiter? c) (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\|))))
        (define (quoted-end start endchar)
          (let loop ((i (+ start 1)))
            (cond ((>= i n) (source-error 'unclosed-quote start))
                  ((char=? (string-ref s i) #\\) (loop (+ i 2)))
                  ((char=? (string-ref s i) endchar) (+ i 1))
                  (else (loop (+ i 1))))))
        (define (block-end start)
          (let loop ((i (+ start 2)) (depth 1))
            (cond ((> depth 256) (source-error 'depth-limit start))
                  ((>= i n) (source-error 'unclosed-comment start))
                  ((at? "#|" i) (loop (+ i 2) (+ depth 1)))
                  ((at? "|#" i) (if (= depth 1) (+ i 2) (loop (+ i 2) (- depth 1))))
                  (else (loop (+ i 1) depth)))))
        (define (atom-end start)
          (let loop ((i start))
            (cond ((or (= i n) (delimiter? (string-ref s i))) i)
                  ((char=? (string-ref s i) #\\)
                   (if (and (< (+ i 1) n) (char-ci=? (string-ref s (+ i 1)) #\x))
                       (let hex ((j (+ i 2)))
                         (cond ((>= j n) (source-error 'invalid-escape i))
                               ((char=? (string-ref s j) #\;) (loop (+ j 1)))
                               (else (hex (+ j 1)))))
                       (loop (min n (+ i 2)))))
                  (else (loop (+ i 1))))))
        ;; The end of a character literal's name: the literal's own character
        ;; is taken whatever it is, and a name (`#\space`, `#\x41`) runs to the
        ;; next delimiter. NEVER: NO ESCAPE RULE HERE (F51). The name was scanned
        ;; with atom-end, whose `\` skips the character after it, so `#\\)`
        ;; swallowed its `)` and the ledger lost count of the depth for the rest
        ;; of the file -- measured as `unbalanced` at the last byte of client,
        ;; code-project, mcp/server, md, regex and this file.
        (define (char-name-end start)
          (let loop ((i start))
            (if (or (= i n) (delimiter? (string-ref s i))) i (loop (+ i 1)))))
        (let loop ((i 0) (depth 0))
          (cond
            ((= i n)
             (unless (= depth 0) (source-error 'unbalanced i))
             (list s (reverse comments) (reverse discards)))
            ((> depth 256) (source-error 'depth-limit i))
            ((char-whitespace? (string-ref s i)) (loop (+ i 1) depth))
            ((at? "#|" i) (loop (block-end i) depth))
            ((at? "#;" i) (prefix! i) (set! discards (cons i discards)) (loop (+ i 2) depth))
            ((at? "#\\" i)
             (when (= (+ i 2) n) (source-error 'invalid-character i))
             (end-run!)
             (let ((end (if (delimiter? (string-ref s (+ i 2))) (+ i 3) (char-name-end (+ i 3)))))
               (when (> (- end i) 256) (source-error 'token-limit i))
               (loop end depth)))
            ((char=? (string-ref s i) #\;)
             (let line ((j (+ i 1)))
               (if (or (= j n) (memv (string-ref s j) '(#\newline #\return)))
                   (begin (set! comments (cons (list i j) comments)) (loop j depth)) (line (+ j 1)))))
            ((memv (string-ref s i) '(#\" #\|))
             (end-run!)
             (loop (quoted-end i (string-ref s i)) depth))
            ((memv (string-ref s i) '(#\( #\[ #\{)) (end-run!) (loop (+ i 1) (+ depth 1)))
            ((memv (string-ref s i) '(#\) #\] #\}))
             (when (= depth 0) (source-error 'unbalanced i))
             (end-run!)
             (loop (+ i 1) (- depth 1)))
            ;; `,@` IS ONE PREFIX (r1 review): read as `,` and then an atom
            ;; `@`, the atom ended the run, and 257 of them passed the limit.
            ((and (char=? (string-ref s i) #\,) (< (+ i 1) n) (char=? (string-ref s (+ i 1)) #\@))
             (prefix! i) (loop (+ i 2) depth))
            ((memv (string-ref s i) '(#\' #\` #\,)) (prefix! i) (loop (+ i 1) depth))
            (else
             (end-run!)
             (let* ((end (atom-end i)) (token (substring s i end)))
               (when (= i end) (source-error 'unsupported-token i))
               (when (> (- end i) 4096) (source-error 'token-limit i))
               ;; Length-prefixed vectors and datum labels can allocate or
               ;; construct cycles independently of numeric atom conversion.
               (when (and (> (string-length token) 1) (char=? (string-ref token 0) #\#) (decimal? (string-ref token 1)))
                 (source-error 'unsupported-reader-label i))
               (when (and (> (string-length token) 0) (char=? (string-ref token 0) #\#)
                          (not (or
                            (member (string-downcase token) '("#t" "#f" "#true" "#false" "#vu8" "#!chezscheme" "#!r6rs" "#!fold-case" "#!no-fold-case"))
                            (and (string=? token "#") (< end n) (memv (string-ref s end) '(#\( #\' #\` #\,)))
                            (and (> (string-length token) 1) (memv (char-downcase (string-ref token 1)) '(#\e #\i #\b #\o #\d #\x))))))
                 (source-error 'unsupported-reader-dispatch i))
               (when (unsafe-numeric-token? token) (source-error 'unsafe-numeric-token i))
               (loop end depth))))))))
)
