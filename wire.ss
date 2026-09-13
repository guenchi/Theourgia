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

;;; (theourgia wire) -- the storage encoding and the record line.
;;;
;;; TWO LAYERS THAT DO NOT KNOW ABOUT EACH OTHER, and keeping them apart
;;; is the point of the file:
;;;
;;;   storable-encode / storable-decode   arbitrary Scheme data <-> the
;;;                                       subset that can be written
;;;                                       (three tags from the format
;;;                                       spec, plus #%dot -- see the
;;;                                       note on it below)
;;;   encode-record   / decode-line       a record <-> one line of bytes
;;;
;;; The writer composes them -- encode the payload, then frame it -- and
;;; encode-record DOES NOT apply storable-encode to what it is given.
;;; That is deliberate and it is the one thing to get right when calling
;;; this library:
;;;
;;;   * storable-encode is not idempotent. Encoding an already-encoded
;;;     ("#%char" 97) wraps it in #%quote, which is correct for a caller
;;;     who meant that list and wrong for one who meant the character.
;;;     A layer that applied it implicitly could not tell those apart.
;;;   * decode-line's payload is then EXACTLY what is on disk, which is
;;;     what a test comparing against injected bytes needs to see.
;;;
;;; So: encode-record and decode-line work at the byte-and-frame level,
;;; storable-encode and storable-decode at the datum level, and the log
;;; layer names both in the order section 5.2 gives -- validate, storage
;;; encode, serialise, CRC.
;;;
;;; THE CODEC IS RE-EXPORTED RATHER THAN WRAPPED. sexpr->string-extended
;;; and string->sexpr-extended come straight from (igropyr sexpr) and
;;; leave here under their own names. On a host that runs the browser
;;; half there is a byte-compatible implementation of the same two
;;; procedures, and this is the file that would name it instead: one
;;; import line, no abstraction interface, no second set of names for
;;; the same thing.
;;;
;;; NEWLINES INSIDE STRINGS ARE ESCAPED HERE AND NOT BY THE CODEC. The
;;; record format's only frame boundary is the newline at the end of the
;;; line, so a literal newline anywhere else destroys the frame --
;;; and (igropyr sexpr)'s writer escapes only " and \, passing a
;;; newline inside a string through raw. Its READER accepts \n, so the
;;; escape applied to the serialised text round-trips through the parser
;;; untouched. The substitution is safe over the whole text rather than
;;; only inside string literals because that writer emits a newline
;;; nowhere else: every other byte it produces is a token, a delimiter,
;;; a space, or base64.
;;;
;;; WHAT decode-line CHECKS, AND IN WHICH ORDER. The order is the
;;; specification, not an optimisation:
;;;
;;;   1. a terminating newline -- without one the line is TORN, which is
;;;      the only damage a crashed write can leave and the only one that
;;;      is repaired rather than reported;
;;;   2. the CRC, over the RAW TEXT, before anything parses it. A line
;;;      whose checksum fails never reaches the parser at all, and the
;;;      trace is what shows that: no parse event is emitted for it;
;;;   3. the frame -- no padding around the datum, one datum, five
;;;      elements.
;;;
;;; TWO THINGS THE ENCODING DOES NOT PROMISE, both measured rather than
;;; assumed. First, encoding CONSUMES NESTING DEPTH: a #%quote or a
;;; #%dot adds levels, so a datum sitting just under the codec's depth
;;; limit can be writable before encoding and refused after it. The
;;; refusal is loud, which is why it is documented rather than guarded.
;;; Second, storable-decode is MANY-TO-ONE on input it did not write:
;;; a hand-made ("#%quote" 5) decodes to 5 and ("#%dot" (a) ()) to (a),
;;; neither of which re-encodes to what was read. Nothing this library
;;; produces takes those shapes, so decode(encode(x)) = x still holds --
;;; but a consumer must not assume an accepted encoding is canonical.
;;;
;;; CANONICAL FORM IS THE WRITER'S GUARANTEE, NOT THE READER'S CHECK.
;;; decode-line rejects bytes before or after the datum, because section 4.2
;;; forbids them outright; it does not re-serialise and compare, so a
;;; hand-made line with unusual interior spacing and a matching CRC is
;;; accepted. Enforcing canonical form on read would be a stronger
;;; claim than the format makes, and it would reject nothing this
;;; library can produce.

(library (theourgia wire)
  (export storable-encode storable-decode
          encode-record decode-line
          wire-safe-symbol? escape-newlines
          sexpr->string-extended string->sexpr-extended)
  (import (chezscheme)
          (theourgia trace)
          (only (theourgia crc32) crc32-hex crc32-string-hex))

  ;; BEGIN COPIED FROM IGROPYR -- the wire codec, and the base64 it needs
  ;;
  ;;   repository  https://github.com/guenchi/Igropyr
  ;;   commit      7196bfe
  ;;   files       igropyr/sexpr.sc, igropyr/crypto.sc
  ;;   licence     Apache-2.0, same author as this file
  ;;   extracted   b64-chars, base64-encode, b64-value, base64-decode
  ;;               default-max-depth, default-max-token, sfail
  ;;               string->sexpr, string->sexpr-extended, $parse
  ;;               sexpr->string, sexpr->string-extended, emit
  ;;               put-numeral, wire-symbol?
  ;;
  ;; No digest is claimed here; the digests are in the table under test/,
  ;; outside this file, so that editing the copy does not edit the claim.
  ;;
  ;; WHY BASE64 CAME WITH IT: the extended forms are base64 -- a bytevector
  ;; is `#vu8"..."`, a flonum its eight IEEE bytes as `#f8"..."`. The codec
  ;; cannot be taken without it. `base64url` was NOT taken: nothing here
  ;; calls it, and code with no caller is a second place to maintain that
  ;; no reading covers.
  ;;
  ;; THE ALPHABET AND ITS INVERSE ARE DEFINED ONCE. If sha256 is copied in
  ;; later it must not bring its own `b64-chars`/`b64-value` -- that would
  ;; be a second copy made in the same work that exists to remove copies.

  (define b64-chars
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define (base64-encode bv)
    (let ((n (bytevector-length bv)))
      (call-with-string-output-port
        (lambda (p)
          (define (out i) (write-char (string-ref b64-chars i) p))
          (let loop ((i 0))
            (let ((left (- n i)))
              (cond
                ((>= left 3)
                 (let ((b0 (bytevector-u8-ref bv i))
                       (b1 (bytevector-u8-ref bv (+ i 1)))
                       (b2 (bytevector-u8-ref bv (+ i 2))))
                   (out (fxsrl b0 2))
                   (out (fxior (fxsll (fxand b0 3) 4) (fxsrl b1 4)))
                   (out (fxior (fxsll (fxand b1 15) 2) (fxsrl b2 6)))
                   (out (fxand b2 63))
                   (loop (+ i 3))))
                ((= left 2)
                 (let ((b0 (bytevector-u8-ref bv i))
                       (b1 (bytevector-u8-ref bv (+ i 1))))
                   (out (fxsrl b0 2))
                   (out (fxior (fxsll (fxand b0 3) 4) (fxsrl b1 4)))
                   (out (fxsll (fxand b1 15) 2))
                   (write-char #\= p)))
                ((= left 1)
                 (let ((b0 (bytevector-u8-ref bv i)))
                   (out (fxsrl b0 2))
                   (out (fxsll (fxand b0 3) 4))
                   (write-char #\= p)
                   (write-char #\= p)))
                (else (void)))))))))

  (define (b64-value ch)
    (cond
      ((and (char<=? #\A ch) (char<=? ch #\Z)) (- (char->integer ch) 65))
      ((and (char<=? #\a ch) (char<=? ch #\z)) (+ 26 (- (char->integer ch) 97)))
      ((and (char<=? #\0 ch) (char<=? ch #\9)) (+ 52 (- (char->integer ch) 48)))
      ((char=? ch #\+) 62)
      ((char=? ch #\/) 63)
      (else #f)))

  (define (base64-decode s)
    (let-values (((p get) (open-bytevector-output-port)))
      (let loop ((i 0) (acc 0) (bits 0))
        (if (= i (string-length s))
            (begin
              (unless (zero? acc)
                (assertion-violation 'base64-decode
                  "non-canonical base64: unused trailing bits are set" s))
              (get))
            (let ((v (b64-value (string-ref s i))))
              (if (not v)
                  (loop (+ i 1) acc bits)   ; skip '=', newlines, etc.
                  (let ((acc (+ (* acc 64) v)) (bits (+ bits 6)))
                    (if (>= bits 8)
                        (let ((keep (- bits 8)))
                          (put-u8 p (fxand (bitwise-arithmetic-shift-right acc keep) #xFF))
                          (loop (+ i 1) (fxand acc (- (fxsll 1 keep) 1)) keep))
                        (loop (+ i 1) acc bits)))))))))


  (define default-max-depth 64)
  ;; Cap on a single atom token (symbol or number). Bounds two costs an
  ;; untrusted sender could otherwise inflate without limit: interning a
  ;; multi-megabyte symbol into the global symbol table, and the
  ;; superlinear bignum build of an enormous digit run. 64 KiB is far
  ;; above any real wire tag, name, or number; strings are not tokens
  ;; and stay bounded by the caller's frame/body size limit instead.
  (define default-max-token 65536)

  (define (sfail msg pos)
    (raise (vector 'sexpr-error msg pos)))

  ;; ---- parser -----------------------------------------------------------

  (define (string->sexpr s . opts)
    ($parse s (if (pair? opts) (car opts) default-max-depth) #f))

  (define (string->sexpr-extended s . opts)
    ($parse s (if (pair? opts) (car opts) default-max-depth) #t))

  (define ($parse s max-depth ext?)
    (let ((n (string-length s)))
      (define (ws? c) (memv c '(#\space #\tab #\newline #\return)))
      (define (skip i)
        (if (and (< i n) (ws? (string-ref s i))) (skip (+ i 1)) i))
      (define (delim? c)
        (or (ws? c) (char=? c #\() (char=? c #\)) (char=? c #\")))
      (define (parse-value i depth)
        (when (> depth max-depth) (sfail "nesting too deep" i))
        (let ((i (skip i)))
          (when (>= i n) (sfail "unexpected end of input" i))
          (let ((c (string-ref s i)))
            (cond
              ((char=? c #\() (parse-list (+ i 1) depth))
              ((char=? c #\)) (sfail "unexpected )" i))
              ((char=? c #\") (parse-string (+ i 1)))
              ((char=? c #\#) (parse-hash (+ i 1) depth))
              (else (parse-atom i))))))
      (define (parse-list i depth)
        (let loop ((i i) (acc '()))
          (let ((i (skip i)))
            (when (>= i n) (sfail "unterminated list" i))
            (cond
              ((char=? (string-ref s i) #\))
               (values (reverse acc) (+ i 1)))
              ;; a lone dot: dotted tail, then the close paren
              ((and (char=? (string-ref s i) #\.)
                    (or (>= (+ i 1) n) (delim? (string-ref s (+ i 1))))
                    (pair? acc))
               (let-values (((tail j) (parse-value (+ i 1) (+ depth 1))))
                 (let ((j (skip j)))
                   (unless (and (< j n) (char=? (string-ref s j) #\)))
                     (sfail "expected ) after dotted tail" j))
                   (values (append (reverse (cdr acc))
                                   (cons (car acc) tail))
                           (+ j 1)))))
              (else
               (let-values (((v j) (parse-value i (+ depth 1))))
                 (loop j (cons v acc))))))))
      (define (parse-string i)
        (let loop ((i i) (acc '()))
          (when (>= i n) (sfail "unterminated string" i))
          (let ((c (string-ref s i)))
            (cond
              ((char=? c #\")
               (values (list->string (reverse acc)) (+ i 1)))
              ((char=? c #\\)
               (when (>= (+ i 1) n) (sfail "dangling escape" i))
               (let ((e (string-ref s (+ i 1))))
                 (loop (+ i 2)
                       (cons (case e
                               ((#\n) #\newline) ((#\t) #\tab)
                               ((#\r) #\return)
                               ((#\" #\\) e)
                               (else (sfail "bad string escape" i)))
                             acc))))
              (else (loop (+ i 1) (cons c acc)))))))
      (define (parse-hash i depth)
        (when (>= i n) (sfail "dangling #" i))
        (let ((c (string-ref s i)))
          (case c
            ((#\t #\f)
             ;; extended #f8"..." (a flonum) must be told apart from the
             ;; #f boolean: the 8-and-quote lookahead decides
             (if (and ext? (char=? c #\f)
                      (< (+ i 2) n)
                      (char=? (string-ref s (+ i 1)) #\8)
                      (char=? (string-ref s (+ i 2)) #\"))
                 (parse-flonum-b64 (+ i 3))
                 (begin
                   (unless (or (>= (+ i 1) n) (delim? (string-ref s (+ i 1))))
                     (sfail "bad # literal" i))
                   (values (char=? c #\t) (+ i 1)))))
            ((#\()                               ; extended: vector
             (unless ext? (sfail "bad # literal" i))
             (parse-vector (+ i 1) depth))
            ((#\v)                               ; extended: #vu8"<base64>"
             (unless (and ext?
                          (< (+ i 3) n)
                          (char=? (string-ref s (+ i 1)) #\u)
                          (char=? (string-ref s (+ i 2)) #\8)
                          (char=? (string-ref s (+ i 3)) #\"))
               (sfail "bad # literal" i))
             (parse-bytevector-b64 (+ i 4)))
            (else (sfail "bad # literal" i)))))
      ;; extended: like a list body, but a dotted tail is illegal
      (define (parse-vector i depth)
        (let loop ((i i) (acc '()))
          (let ((i (skip i)))
            (when (>= i n) (sfail "unterminated vector" i))
            (cond
              ((char=? (string-ref s i) #\))
               (values (list->vector (reverse acc)) (+ i 1)))
              ((and (char=? (string-ref s i) #\.)
                    (or (>= (+ i 1) n) (delim? (string-ref s (+ i 1)))))
               (sfail "dot not allowed in vector" i))
              (else
               (let-values (((v j) (parse-value i (+ depth 1))))
                 (loop j (cons v acc))))))))
      ;; Scan a base64 payload to its closing quote and decode it in one
      ;; pass -- no per-element list, so no O(n) allocation blowup. The
      ;; base64 alphabet has no " or \, so there is nothing to escape; a
      ;; stray char fails loudly. -> (values bytes next-i)
      (define (parse-b64 start what)
        (let loop ((j start))
          (cond
            ((>= j n) (sfail (string-append "unterminated " what) start))
            ((char=? (string-ref s j) #\")
             ;; base64-decode raises &assertion on a non-canonical tail (the
             ;; unused bits of the last character must be zero), and this
             ;; input is a peer's. Keep it inside the documented
             ;; #(sexpr-error ...) contract rather than letting a raw
             ;; assertion escape into a node's distribution process.
             (values (guard (e (#t (sfail (string-append "bad base64 in " what)
                                          start)))
                       (base64-decode (substring s start j)))
                     (+ j 1)))
            ((let ((c (string-ref s j)))
               (or (char<=? #\A c #\Z) (char<=? #\a c #\z) (char<=? #\0 c #\9)
                   (char=? c #\+) (char=? c #\/) (char=? c #\=)))
             (loop (+ j 1)))
            (else (sfail (string-append "bad base64 in " what) j)))))
      (define (parse-bytevector-b64 start)
        (parse-b64 start "bytevector"))
      ;; extended: the 8 IEEE-754 bytes of a double, little-endian
      (define (parse-flonum-b64 start)
        (let-values (((bv j) (parse-b64 start "flonum")))
          (unless (= (bytevector-length bv) 8)
            (sfail "flonum wants exactly 8 bytes" start))
          (values (bytevector-ieee-double-ref bv 0 (endianness little)) j)))
      (define (digits? str a b)
        (and (< a b)
             (let lp ((i a))
               (or (= i b)
                   (and (char<=? #\0 (string-ref str i) #\9)
                        (lp (+ i 1)))))))
      (define (token->number tok)
        ;; [-]digits or [-]digits/digits, nothing else
        (let* ((m (string-length tok))
               (a (if (and (> m 0) (char=? (string-ref tok 0) #\-)) 1 0))
               (slash (let lp ((i a))
                        (cond ((= i m) #f)
                              ((char=? (string-ref tok i) #\/) i)
                              (else (lp (+ i 1)))))))
          (cond
            ((and slash (digits? tok a slash) (digits? tok (+ slash 1) m))
             (let ((d (string->number (substring tok (+ slash 1) m) 10)))
               (and d (not (zero? d))
                    (/ (let ((v (string->number (substring tok a slash) 10)))
                         (if (= a 1) (- v) v))
                       d))))
            ((digits? tok a m)
             (let ((v (string->number (substring tok a m) 10)))
               (and v (if (= a 1) (- v) v))))
            (else #f))))
      (define (symbol-char? c)
        (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9)
            (memv c '(#\- #\+ #\* #\/ #\< #\> #\= #\? #\! #\. #\_
                      #\% #\& #\^ #\~ #\: #\@))))
      (define (valid-symbol? tok)
        (let ((m (string-length tok)))
          (and (> m 0)
               (let lp ((i 0))
                 (or (= i m)
                     (and (symbol-char? (string-ref tok i)) (lp (+ i 1))))))))
      (define (numeric-shape? tok)
        ;; starts like a number: it must BE a whitelisted number, so
        ;; 1.5 or 1e9 can't slip through as symbols
        (let ((m (string-length tok)))
          (and (> m 0)
               (let ((c (string-ref tok 0)))
                 (or (char<=? #\0 c #\9)
                     (and (char=? c #\-) (> m 1)
                          (char<=? #\0 (string-ref tok 1) #\9)))))))
      ;; No decimal flonum text on the wire in EITHER mode: a flonum
      ;; crosses only as #f8"<base64>" (extended), so a numeric-shaped
      ;; token carrying '.' or an exponent is always a bad number.
      (define (parse-atom i)
        (let ((j (let lp ((j i))
                   (if (or (>= j n) (delim? (string-ref s j))) j (lp (+ j 1))))))
          (when (> (- j i) default-max-token) (sfail "token too long" i))
          (let ((tok (substring s i j)))
            (cond
              ((token->number tok) => (lambda (v) (values v j)))
              ((numeric-shape? tok) (sfail "bad number" i))
              ((valid-symbol? tok) (values (string->symbol tok) j))
              (else (sfail "bad token" i))))))
      (let-values (((v i) (parse-value 0 0)))
        (unless (= (skip i) n) (sfail "trailing data after datum" i))
        v)))

  ;; ---- writer -----------------------------------------------------------

  (define (sexpr->string x)
    (call-with-string-output-port
     (lambda (p) (emit x p 0 #f))))

  (define (sexpr->string-extended x)
    (call-with-string-output-port
     (lambda (p) (emit x p 0 #t))))

  (define (emit x p depth ext?)
    (when (> depth default-max-depth)
      (sfail "nesting too deep (cyclic data?)" 0))
    (cond
      ((null? x) (put-string p "()"))
      ((pair? x)
       (put-char p #\()
       (emit (car x) p (+ depth 1) ext?)
       ;; the spine is bounded too: a cycle along cdr never nests, so
       ;; the depth counter alone would spin forever
       (let tail ((x (cdr x)) (k 0))
         (when (> k 1000000) (sfail "list too long (cyclic data?)" 0))
         (cond
           ((null? x) (put-char p #\)))
           ((pair? x)
            (put-char p #\space)
            (emit (car x) p (+ depth 1) ext?)
            (tail (cdr x) (+ k 1)))
           (else
            (put-string p " . ")
            (emit x p (+ depth 1) ext?)
            (put-char p #\))))))
      ((symbol? x)
       (let ((s (symbol->string x)))
         (unless (wire-symbol? s)
           (sfail "symbol not wire-safe" 0))
         (put-string p s)))
      ((string? x)
       (put-char p #\")
       (string-for-each
        (lambda (c)
          (when (or (char=? c #\") (char=? c #\\)) (put-char p #\\))
          (put-char p c))
        x)
       (put-char p #\"))
      ((eq? x #t) (put-string p "#t"))
      ((eq? x #f) (put-string p "#f"))
      ;; MEASURED ON THE NUMERAL, ONCE, AND WITH ITS SIGN. The reader
      ;; caps a token at default-max-token characters, so a numeral
      ;; longer than that is one this library can write and cannot read
      ;; back -- the failure landing at the far end, after the sender saw
      ;; success. Refusing here puts the error on the end that can still
      ;; do something about it.
      ;;
      ;; The cap counts the whole token, sign included: -(10^65535) has
      ;; 65536 digits and 65537 characters, and a check that counted
      ;; digits would pass exactly the value the reader rejects. The
      ;; string produced here is the one measured and the one written,
      ;; so no numeral is rendered twice.
      ((and (integer? x) (exact? x)) (put-numeral p (number->string x)))
      ((and (rational? x) (exact? x)) (put-numeral p (number->string x)))
      ;; extended whitelist; in strict mode these fall through to the
      ;; refusal below, exactly as before
      ((and ext? (vector? x))
       (put-string p "#(")
       (let ((m (vector-length x)))
         (do ((i 0 (+ i 1))) ((= i m))
           (when (> i 0) (put-char p #\space))
           (emit (vector-ref x i) p (+ depth 1) ext?)))
       (put-char p #\)))
      ((and ext? (bytevector? x))
       (put-string p "#vu8\"")
       (put-string p (base64-encode x))
       (put-char p #\"))
      ;; the 8 IEEE bytes, little-endian: bit-exact for every double,
      ;; inf and nan included -- decimal printing never touches the wire
      ((and ext? (flonum? x))
       (put-string p "#f8\"")
       (let ((bv (make-bytevector 8)))
         (bytevector-ieee-double-set! bv 0 x (endianness little))
         (put-string p (base64-encode bv)))
       (put-char p #\"))
      (else (sfail "datum not in the wire whitelist" 0))))

  ;; Exact integers and ratios reach the wire as their printed numeral,
  ;; and the reader will not accept one past the token cap. Same limit,
  ;; same constant -- there is one supplier of it in this file and both
  ;; ends read it, which is a mechanism rather than an obligation.
  (define (put-numeral p str)
    (when (> (string-length str) default-max-token)
      ;; The same failure shape the symbol refusal already uses, because
      ;; a caller catching one has to catch the other: both are "this
      ;; datum cannot go on the wire", raised by the writer.
      (sfail "token too long for the wire -- carry a value this large as a bytevector, which needs the extended mode (sexpr->string-extended)" 0))
    (put-string p str))

  ;; A bare token is re-read by parse-atom, which tries a number first
  ;; and treats "." as the improper-list marker. So a symbol whose name
  ;; reads back as a number (|12|, |1.5|) or as the dot would return
  ;; from the wire as a DIFFERENT datum -- an integer instead of a
  ;; symbol, or an improper pair instead of a 3-element list. There is
  ;; no escaped symbol form in this grammar, so such symbols are
  ;; refused by the writer (the whole point of the whitelist) rather
  ;; than silently corrupted in transit.
  ;;
  ;; THE QUESTION IS WHAT THE READER WILL DO WITH THE NAME, NOT WHAT
  ;; CHEZ THINKS OF IT. string->number alone was the wrong judge, and
  ;; the two are not even nested: parse-atom commits to reading a number
  ;; the moment a token STARTS like one (a digit, or '-' then a digit)
  ;; and then fails if the rest is not one. So |0x10| passed
  ;; string->number's test -- Chez does not read it as a number -- went
  ;; out on the wire bare, and came back as "bad number" at the far end:
  ;; a datum this library wrote and could not read, which is the one
  ;; failure a whitelist exists to prevent. |12abc|, |1/0| and |-1x| are
  ;; the same shape.
  ;;
  ;; The reader's own test is mirrored here -- COPIED, not shared, which
  ;; is a maintenance obligation and not a guarantee: whoever widens
  ;; numeric-shape? in the reader has to widen this one in the same
  ;; commit, or the same class of defect comes straight back. Keeping
  ;; them textually identical is what makes that check a glance.
  ;; string->number stays as well, and the overlap is not redundant: it
  ;; refuses names the reader would accept, such as |+i|, which is an
  ;; over-refusal rather than a corruption and costs a caller nothing
  ;; but a rename.
  (define (wire-symbol? s)
    (let ((m (string-length s)))
      (and (> m 0)
           ;; Before the character walk, because it is one comparison and
           ;; the walk is not: a name past the reader's token cap comes
           ;; back as "token too long" at the far end, so it is refused
           ;; here for the same reason the numeric shapes are.
           (<= m default-max-token)
           (not (string->number s))
           ;; the reader's numeric-shape?, kept identical to it
           (not (let ((c (string-ref s 0)))
                  (or (char<=? #\0 c #\9)
                      (and (char=? c #\-) (> m 1)
                           (char<=? #\0 (string-ref s 1) #\9)))))
           (not (string=? s "."))
           (let lp ((i 0))
             (or (= i m)
                 (and (let ((c (string-ref s i)))
                        (or (char<=? #\a c #\z) (char<=? #\A c #\Z)
                            (char<=? #\0 c #\9)
                            (memv c '(#\- #\+ #\* #\/ #\< #\> #\= #\? #\! #\.
                                      #\_ #\% #\& #\^ #\~ #\: #\@))))
                      (lp (+ i 1))))))))

  ;; END COPIED FROM IGROPYR

  ;; ---- which symbols survive the wire -----------------------------------

  ;; ASKED OF THE CODEC, NOT RE-DERIVED FROM IT. (igropyr sexpr) does not
  ;; export its wire-symbol?, and a copy of that predicate here would be
  ;; a second supplier of one decision: whoever widened one would have to
  ;; find the other, and the failure of not doing so is silent -- a
  ;; symbol stored bare that reads back as a number.
  ;;
  ;; The question asked is the one that matters, and it is stronger than
  ;; the predicate: does this symbol come back from the codec AS ITSELF.
  ;; Writing it and reading it answers that with the actual mechanism.
  ;;
  ;; Memoised in an ORDINARY eq table, because R6RS has no weak one and
  ;; this file is portable. The cost, stated rather than discovered: an
  ;; entry keeps its symbol alive for the life of the process. That is
  ;; nearly free here -- storable-encode is only ever asked about
  ;; symbols that are already interned, and interning already keeps them
  ;; alive -- but on a host where the reader can produce collectable
  ;; symbols this table would retain them.
  (define symbol-cache (make-eq-hashtable))

  ;; AN UNINTERNED SYMBOL CANNOT BE STORED, and it has to be refused
  ;; rather than approximated. A gensym's identity is not its name: two
  ;; separately made (gensym "foo") are distinct objects, both would go
  ;; to disk as ("#%sym" "foo"), and both would come back as the interned
  ;; foo -- one encoding for two data, and a round trip that returns
  ;; something plausible instead of raising. The test is the mechanism
  ;; rather than a predicate name: an interned symbol is the one its own
  ;; name interns to.
  (define (interned-symbol? s)
    (eq? s (string->symbol (symbol->string s))))

  (define (wire-safe-symbol? s)
    (unless (symbol? s)
      (assertion-violation 'wire-safe-symbol? "not a symbol" s))
    (let ((hit (hashtable-ref symbol-cache s 'miss)))
      (if (eq? hit 'miss)
          (let ((answer
                  (guard (e (#t #f))
                    (eq? s (string->sexpr-extended (sexpr->string-extended s))))))
            (hashtable-set! symbol-cache s answer)
            answer)
          hit)))

  ;; ---- storage encoding -------------------------------------------------

  (define char-tag  "#%char")
  (define sym-tag   "#%sym")
  (define quote-tag "#%quote")
  (define dot-tag   "#%dot")

  ;; The tags are STRINGS because a symbol cannot hold a '#': the codec's
  ;; symbol grammar has no such character, so there is no symbol a user
  ;; could supply that collides with a tag. A string can collide, which
  ;; is what #%quote exists for.
  (define (tag-string? x)
    (and (string? x)
         (>= (string-length x) 2)
         (char=? (string-ref x 0) #\#)
         (char=? (string-ref x 1) #\%)))

  (define (list2? x)
    (and (pair? x) (pair? (cdr x)) (null? (cddr x))))

  (define (list3? x)
    (and (pair? x) (pair? (cdr x)) (pair? (cddr x)) (null? (cdddr x))))

  (define (tagged? x tag)
    (and (pair? x) (string? (car x)) (string=? (car x) tag)))

  ;; A list with its final cdr held separately, so the two can be
  ;; encoded and then rebuilt -- or, when rebuilding them would lose the
  ;; distinction, named instead.
  ;; WALKED WITH A TORTOISE, because a circular spine is reachable: the
  ;; Chez reader builds one from #0=(a . #0#), so it can arrive from a
  ;; source file rather than only from a program bug. Without this the
  ;; walk does not fail, it does not return -- and a hang is the failure
  ;; a test suite reports worst.
  ;;
  ;; A CYCLE THROUGH A car IS NOT CAUGHT HERE. #0=(#0#) recurses through
  ;; storable-encode instead of along a spine, and runs until the stack
  ;; is gone. Saying so is the honest position: closing it needs a seen
  ;; set threaded through the whole encoder, and the layer that accepts
  ;; data from outside is the right place to refuse cyclic data outright.
  (define (circular-spine? x)
    (let loop ((slow x) (fast x))
      (cond
        ((not (pair? fast)) #f)
        ((not (pair? (cdr fast))) #f)
        ((eq? (cdr fast) slow) #t)
        ((eq? (cddr fast) (cdr slow)) #t)
        (else (loop (cdr slow) (cddr fast))))))

  (define (spine-elements x)
    (if (pair? x) (cons (car x) (spine-elements (cdr x))) '()))

  (define (spine-tail x)
    (if (pair? x) (spine-tail (cdr x)) x))

  (define (rebuild elements tail)
    (if (null? elements)
        tail
        (cons (car elements) (rebuild (cdr elements) tail))))

  ;; ANYTHING NOT MENTIONED PASSES THROUGH UNTOUCHED, and is refused
  ;; later by the codec if it does not belong on the wire. There is one
  ;; supplier of the whitelist verdict and it is (igropyr sexpr); a
  ;; second rejection here would be a second list to keep in step, and
  ;; the two disagreeing is how a value becomes writable by one layer
  ;; and unwritable by the next.
  ;;
  ;; #%dot EXISTS BECAUSE A DOTTED TAIL CAN STOP BEING AN ATOM. (a . #\b)
  ;; has a character where a list would end; the character encodes to
  ;; ("#%char" 98), which is a PAIR, and consing a pair onto the tail of
  ;; (a) produces the proper list (a "#%char" 98) -- the very same object
  ;; the proper list (a "#%char" 98) encodes to. Two different data with
  ;; one encoding is the failure a storage format exists to prevent, and
  ;; it is silent: the round trip returns a plausible list. So when, and
  ;; only when, the encoded tail is a pair and the original tail was not
  ;; the empty list, the improper shape is named rather than built:
  ;; ("#%dot" <encoded elements> <encoded tail>). Alists are untouched --
  ;; (kind . section) has a symbol tail, which encodes to a symbol.
  ;;
  ;; The (not (null? tail)) half of that guard is REDUNDANT as the code
  ;; stands -- spine-tail never returns a pair, so an encoded tail can
  ;; only be a pair when the tail was a character or an unsafe symbol,
  ;; and neither of those is (). It is kept because it states the
  ;; condition the rule actually means; a reader should not go looking
  ;; for the case it covers, because there is not one today.
  (define (storable-encode x)
    (cond
      ((char? x) (list char-tag (char->integer x)))
      ((symbol? x)
       (unless (interned-symbol? x)
         (assertion-violation 'storable-encode
           "an uninterned symbol has an identity no text can carry" x))
       (if (wire-safe-symbol? x) x (list sym-tag (symbol->string x))))
      ((pair? x)
       (when (circular-spine? x)
         (assertion-violation 'storable-encode "circular list" x))
       (let* ((elements (spine-elements x))
              (tail (spine-tail x))
              (enc-elements (map storable-encode elements))
              (enc-tail (storable-encode tail)))
         (if (and (not (null? tail)) (pair? enc-tail))
             (list dot-tag enc-elements enc-tail)
             (let ((body (rebuild enc-elements enc-tail)))
               ;; The test is on the ORIGINAL car, not the encoded one: a
               ;; car that was a character or an unsafe symbol has just
               ;; become a list, and a list is not a tag.
               (if (tag-string? (car x)) (list quote-tag body) body)))))
      ((vector? x)
       (let* ((n (vector-length x)) (v (make-vector n)))
         (do ((i 0 (+ i 1))) ((= i n) v)
           (vector-set! v i (storable-encode (vector-ref x i))))))
      (else x)))

  (define (code-point? n)
    (and (integer? n) (exact? n) (<= 0 n #x10FFFF)
         (not (<= #xD800 n #xDFFF))))

  ;; THE INVERSE, AND EXACTLY THE INVERSE. #%quote's payload is decoded
  ;; SPINE-WISE rather than through storable-decode, and that is the
  ;; whole of why quoting works: the payload is the encoding of a list
  ;; whose own car was a tag, so re-testing that car would strip the
  ;; quote a second time. ("#%quote" ("#%quote" 5)) is the encoding of
  ;; ("#%quote" 5) and must decode back to it, not to 5.
  ;;
  ;; A tag that is present but malformed -- ("#%char" "x"), a three
  ;; element ("#%sym" ...) -- is read as ordinary data rather than
  ;; refused. Nothing this library writes takes that shape, so the case
  ;; only arises for bytes that came from somewhere else, and returning
  ;; them as what they literally are loses less than raising does.
  (define (storable-decode y)
    (cond
      ((pair? y)
       (cond
         ((and (tagged? y char-tag) (list2? y) (code-point? (cadr y)))
          (integer->char (cadr y)))
         ((and (tagged? y sym-tag) (list2? y) (string? (cadr y)))
          (string->symbol (cadr y)))
         ((and (tagged? y quote-tag) (list2? y))
          (decode-spine (cadr y)))
         ((and (tagged? y dot-tag) (list3? y)
               (list? (cadr y)) (pair? (cadr y)))
          (rebuild (map storable-decode (cadr y))
                   (storable-decode (caddr y))))
         (else (decode-spine y))))
      ((vector? y)
       (let* ((n (vector-length y)) (v (make-vector n)))
         (do ((i 0 (+ i 1))) ((= i n) v)
           (vector-set! v i (storable-decode (vector-ref y i))))))
      (else y)))

  (define (decode-spine y)
    (if (pair? y)
        (cons (storable-decode (car y)) (decode-spine (cdr y)))
        (storable-decode y)))

  ;; ---- the record line --------------------------------------------------

  (define (escape-newlines s)
    (if (not (let loop ((i 0))
               (and (< i (string-length s))
                    (or (char=? (string-ref s i) #\newline) (loop (+ i 1))))))
        s
        (call-with-string-output-port
          (lambda (p)
            (string-for-each
              (lambda (c)
                (if (char=? c #\newline) (put-string p "\\n") (put-char p c)))
              s)))))

  ;; VALIDATION IS IN THE BODY, NOT IN A CONTRACT. Contracts are a
  ;; development-time aid and are compiled out of a release, so anything
  ;; that must hold in production is written here where it cannot be
  ;; switched off. What is checked is the shape the format fixes; what
  ;; the values MEAN -- that the seq follows the last one, that the deps
  ;; are reduced -- is the log layer's to know and is deliberately not
  ;; checked here.
  ;;
  ;; A DEP'S WRITER IS WRITTEN AS A STRING AND READ AS EITHER. Section
  ;; 1.1 makes ids strings because a base36 writer id can begin with a
  ;; digit and the codec refuses such a symbol -- so a writer that
  ;; emitted symbols would work until the day it generated an id
  ;; starting with a digit, which is the worst possible schedule for
  ;; finding out. The write side therefore takes strings only. The read
  ;; side does not check deps at all, so a hand-made log carrying bare
  ;; symbols still loads; nothing has to be relaxed for that.
  ;; Refuses an uninterned symbol anywhere in a structure, and refuses
  ;; to walk forever on a circular one.
  ;;
  ;; TWO SETS, NOT ONE, AND THE SECOND ONE IS NOT AN OPTIMISATION. The
  ;; ancestor set is what detects a cycle: a node reached while it is
  ;; still on the path from the root is a cycle, one merely reached
  ;; twice is sharing, and only the first is an error. But tracking
  ;; ancestors ALONE re-walks every shared subtree, and a structure can
  ;; share exponentially: 65 nested (vector x x) hold 65 distinct
  ;; vectors and 2^65 paths, so the walk stops returning on a value the
  ;; codec used to refuse in microseconds. Both reviews of this file
  ;; found that independently, on a version of this walk that had only
  ;; the path. The completed set makes the cost linear in NODES rather
  ;; than in paths, which is the difference between a check and a hang.
  ;;
  ;; A hashtable rather than a list for the same reason: a list scan
  ;; made a 20,000 element actor quadratic, measured at 207 ms against
  ;; 0.4 ms to serialise it.
  (define (assert-interned-symbols! who x)
    (let ((done (make-eq-hashtable))
          (on-path (make-eq-hashtable)))
      (let walk ((x x))
        (cond
          ((symbol? x)
           (unless (interned-symbol? x)
             (assertion-violation who
               "an uninterned symbol has an identity no text can carry" x)))
          ((or (pair? x) (vector? x))
           (unless (hashtable-ref done x #f)
             (when (hashtable-ref on-path x #f)
               (assertion-violation who "circular structure" x))
             (hashtable-set! on-path x #t)
             (if (pair? x)
                 (begin (walk (car x)) (walk (cdr x)))
                 (let ((n (vector-length x)))
                   (do ((i 0 (+ i 1))) ((= i n))
                     (walk (vector-ref x i)))))
             (hashtable-delete! on-path x)
             (hashtable-set! done x #t)))
          (else (void))))))

  ;; AN ACTOR IS EITHER A NAME OR A WHOLE REQUEST IDENTITY. The second
  ;; form carries who asked, which request it was, which sub-operation of
  ;; that request this record is, the fingerprint of what was asked, the
  ;; plan this record belongs to, and the cursor the request was written
  ;; against:
  ;;
  ;;     (who (origin . req-id) sub fingerprint plan-event-id after)
  ;;
  ;; THE SHAPE IS CHECKED, NOT THE LENGTH. Two of the six slots changed
  ;; meaning when this grew from five -- what was a bare req-id became
  ;; the pair `(origin . req-id)`, and `plan-event-id` was inserted
  ;; before `after` -- so a length test would accept an actor of the
  ;; right size whose fields had all shifted by one, which is the hardest
  ;; kind of wrong to see. It would also accept every five element actor
  ;; a mistake in new code still produced.
  ;;
  ;; The identity has to be readable from any one record: a reader that
  ;; inferred the origin from the physical writer would be wrong for
  ;; every record a successor writer carries after an adopt.
  (define (event-id? x)
    (and (pair? x) (string? (car x)) (integer? (cdr x)) (exact? (cdr x)) (>= (cdr x) 0)))

  (define (req-id? x)
    (or (string? x)
        ;; a batch item names the batch and its index within it
        (and (list? x) (= 3 (length x)) (eq? (car x) 'batch)
             (string? (cadr x)) (integer? (caddr x)) (exact? (caddr x)) (>= (caddr x) 0))))

  (define (request-actor? x)
    (and (list? x) (= 6 (length x))
         (let ((who (list-ref x 0))
               (identity (list-ref x 1))
               (sub (list-ref x 2))
               (fingerprint (list-ref x 3))
               (plan-event (list-ref x 4))
               (after (list-ref x 5)))
           (and (string? who)
                (pair? identity) (string? (car identity)) (req-id? (cdr identity))
                (or (memq sub '(single plan))
                    (and (integer? sub) (exact? sub) (>= sub 0)))
                (string? fingerprint)
                (or (not plan-event) (event-id? plan-event))
                (event-id? after)))))

  (define (check-record! who seq ts actor deps)
    (unless (and (integer? seq) (exact? seq) (>= seq 0))
      (assertion-violation who "seq must be a non-negative exact integer" seq))
    (unless (and (integer? ts) (exact? ts) (>= ts 0))
      (assertion-violation who "ts must be a non-negative exact integer" ts))
    (unless (or (string? actor) (request-actor? actor))
      (assertion-violation who
        "actor must be a string or a six element request actor" actor))
    ;; THE ACTOR IS THIS FUNCTION'S RESPONSIBILITY IN A WAY THE PAYLOAD
    ;; IS NOT: the format defines its shape, so it must also be storable
    ;; here rather than by arrangement with the caller. An uninterned
    ;; symbol anywhere inside it would go to disk as its name and come
    ;; back as the INTERNED symbol of that name -- a record attributed
    ;; to a different actor, silently.
    ;;
    ;; CHECKED FOR THE ONE THING THAT FAILS SILENTLY, and not for
    ;; membership of the storable subset. An earlier version asked
    ;; whether the actor was a fixed point of storable-encode, which was
    ;; wrong twice over: it REJECTED a perfectly legal actor whose first
    ;; element is a string beginning "#%" (the encoder quotes it, so it
    ;; is not its own encoding, but the codec writes it unharmed), and
    ;; it HUNG on an actor containing a self-referential vector, where
    ;; the codec's own depth limit had previously refused it. Everything
    ;; else outside the subset is refused loudly by the codec a moment
    ;; later; the uninterned symbol is the only value that would go to
    ;; disk meaning something other than what it is.
    (assert-interned-symbols! who actor)
    (unless (list? deps)
      (assertion-violation who "deps must be a proper list" deps))
    (for-each
      (lambda (d)
        (unless (and (pair? d)
                     (string? (car d))
                     (integer? (cdr d)) (exact? (cdr d)) (>= (cdr d) 0))
          (assertion-violation who "dep must be (writer-string . seq)" d)))
      deps))

  ;; The record datum is the five elements of section 4.2 and the CRC covers all
  ;; of them -- seq, timestamp, actor and deps included, not just the
  ;; payload. A checksum over the payload alone would leave the fields
  ;; that decide ordering and causality unprotected, which is the half
  ;; of the record a corruption would be hardest to notice in.
  (define (record-text seq ts actor deps payload)
    (escape-newlines
      (sexpr->string-extended (list seq ts actor deps payload))))

  ;; THE CONDITION ON THE PAYLOAD, stated because it cannot be checked
  ;; here cheaply: it must already be in the storable subset, which is
  ;; what storable-encode produces. The one value that would fail
  ;; SILENTLY is an uninterned symbol -- the codec writes its name and
  ;; the reader hands back the interned symbol of that name, so the
  ;; record on disk means something else. storable-encode refuses those,
  ;; so the composed path is safe and only a caller who skipped it is
  ;; exposed. Everything else outside the subset (a character, a record
  ;; instance, a procedure) is refused loudly by the codec.
  (define (encode-record seq ts actor deps payload)
    (check-record! 'encode-record seq ts actor deps)
    (let* ((text (record-text seq ts actor deps payload))
           (hex (crc32-string-hex text)))
      (string->utf8 (string-append hex " " text "\n"))))

  (define newline-byte 10)
  (define space-byte 32)
  (define crc-hex-length 8)
  (define text-start 9)

  (define (lower-hex-byte? b)
    (or (<= 48 b 57) (<= 97 b 102)))

  (define (hex-field bv)
    (let loop ((i 0) (acc '()))
      (if (= i crc-hex-length)
          (list->string (reverse acc))
          (let ((b (bytevector-u8-ref bv i)))
            (and (lower-hex-byte? b)
                 (loop (+ i 1) (cons (integer->char b) acc)))))))

  (define (whitespace-char? c)
    (or (char=? c #\space) (char=? c #\tab)
        (char=? c #\newline) (char=? c #\return)))

  ;; A well formed datum's last character is ')' or '"' or an atom
  ;; character, never a space -- so a trailing space IS the "bytes after
  ;; the datum" section 4.2 forbids, and finding it needs no scanner of its
  ;; own. The parser catches a SECOND datum by itself, because it
  ;; refuses non-whitespace after the first; what it does not catch is
  ;; whitespace, since it skips that before deciding it is at the end.
  (define (padded? text)
    (let ((n (string-length text)))
      (and (> n 0)
           (or (whitespace-char? (string-ref text 0))
               (whitespace-char? (string-ref text (- n 1)))))))

  (define (interior-newline? bv)
    (let ((n (bytevector-length bv)))
      (let loop ((i 0))
        (and (< i n)
             (or (= (bytevector-u8-ref bv i) newline-byte)
                 (loop (+ i 1)))))))

  (define parse-failed (list 'parse-failed))

  (define (decode-line input)
    (let ((bv (cond ((bytevector? input) input)
                    ((string? input) (string->utf8 input))
                    (else (assertion-violation
                            'decode-line "not a bytevector" input)))))
      (let ((n (bytevector-length bv)))
        (cond
          ;; 1. the frame boundary
          ((or (= n 0) (not (= (bytevector-u8-ref bv (- n 1)) newline-byte)))
           (list 'torn))
          (else
           (let ((end (- n 1)))
             (cond
               ((or (< end text-start)
                    (not (= (bytevector-u8-ref bv crc-hex-length) space-byte)))
                (list 'frame-error 'crc-field))
               (else
                (let ((claimed (hex-field bv)))
                  (cond
                    ;; 2. the checksum, on the raw bytes, before any parse
                    ((not claimed) (list 'bad-crc))
                    ((not (string=? claimed (crc32-hex bv text-start end)))
                     (list 'bad-crc))
                    (else
                     (let* ((raw (let ((s (make-bytevector (- end text-start))))
                                   (bytevector-copy! bv text-start s 0
                                                     (- end text-start))
                                   s))
                            (text (utf8->string raw)))
                       (cond
                         ;; 3. the frame
                         ;;
                         ;; ONE LINE MEANS ONE NEWLINE, AND IT IS THE
                         ;; LAST BYTE. Checking only the last byte would
                         ;; leave decode-line depending on its caller
                         ;; having split the file at every newline: hand
                         ;; it a buffer spanning two records with a CRC
                         ;; computed over both and it would return the
                         ;; first record's parse of a two-record string.
                         ;; The format says the newline is the only frame
                         ;; boundary, so the check belongs here rather
                         ;; than in an invariant somebody has to
                         ;; remember.
                         ((interior-newline? raw)
                          (list 'frame-error 'embedded-newline))
                         ;; THE BYTES MUST BE UTF-8, AND THE CHECKSUM
                         ;; DOES NOT SAY SO. Chez's utf8->string
                         ;; SUBSTITUTES U+FFFD for a malformed sequence
                         ;; rather than raising, so a hand-made line
                         ;; carrying byte 0xFF inside a string has a
                         ;; perfectly correct CRC and decodes to a
                         ;; DIFFERENT string than its bytes -- a datum
                         ;; that was never written. The same
                         ;; preprocessing silently swallows a leading
                         ;; byte-order mark, which would otherwise slip
                         ;; bytes in front of the datum past the padding
                         ;; check.
                         ;;
                         ;; Re-encoding and comparing needs no UTF-8
                         ;; scanner of its own: well-formed UTF-8 is what
                         ;; string->utf8 produces, so it round-trips byte
                         ;; for byte, and no malformed sequence can
                         ;; survive equality with well-formed output.
                         ;;
                         ;; WHAT IT ACCEPTS IS NOT EXACTLY "WELL-FORMED
                         ;; UTF-8", and the difference is one code point:
                         ;; EF BB BF is well-formed -- it encodes U+FEFF
                         ;; -- and is rejected when it LEADS, because the
                         ;; reader strips it there. Every other scalar
                         ;; value survives, U+FEFF included when it sits
                         ;; inside a string rather than in front of the
                         ;; datum. That is the right rule for this format
                         ;; (a record begins with a list, never with a
                         ;; mark) but it is a narrower claim than
                         ;; well-formedness, and stating the wider one
                         ;; would send the next reader looking for a
                         ;; check that is not here.
                         ((not (equal? raw (string->utf8 text)))
                          (list 'frame-error 'encoding))
                         ((padded? text) (list 'frame-error 'padding))
                         (else
                          (trace-event! 'parse #f (- end text-start))
                          (let ((d (guard (e (#t parse-failed))
                                     (string->sexpr-extended text))))
                            (cond
                              ((eq? d parse-failed) (list 'frame-error 'parse))
                              ((not (and (list? d) (= 5 (length d))))
                               (list 'frame-error 'shape))
                              (else (cons 'ok d))))))))))))))))))
)