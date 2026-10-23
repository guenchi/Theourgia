#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;;; (theourgia reader) -- a reader of Scheme forms, ours, small and bounded:
;;; the forms a text-mode block's source holds, and a text file's header.
;;;
;;; KEY: THE SYSTEM READER IS NEVER CALLED. client.sc records two rounds of
;;; guarding Chez's `read` against what it does before any validation (a
;;; datum label builds a cycle, `#e1e100000` allocates a number of a hundred
;;; thousand digits); this reader accepts a short list of shapes and refuses
;;; the rest, so nothing it is given can make it do more than the limits say.
;;;
;;; ACCEPTED: lists in the three bracket pairs, improper ones with a dot;
;;; symbols; strings with their escapes; characters; booleans; quote,
;;; quasiquote, unquote and unquote-splicing; line comments; `#|...|#`
;;; block comments, nested; `#;` datum comments, whose datum is read under
;;; the same rules and limits and dropped; `#!` directives, skipped to the
;;; end of the line; a leading U+FEFF, the byte-order mark the projection
;;; keeps as the first character of a file's first piece.
;;;
;;; NUMBERS ARE NOT CONVERTED. A token of at most 18 decimal digits with an
;;; optional sign is an exact integer, read digit by digit; every other
;;; numeric token is a NUMBER-TEXT record holding the token as written,
;;; which is neither a symbol nor a pair, so the name walk passes over it.
;;;
;;; REFUSED, each a failure naming its reason: a datum label (`#n=`, `#n#`)
;;; is `label`; any other `#` syntax, `|`, or a dot where a list cannot have
;;; one is `unsupported`; a bracket left open, a close with nothing open or
;;; one that does not match its open is `unbalanced`. ONE EXCEPTION, asked
;;; for by the caller (read-library-piece): the last piece of a library
;;; file ends with the library form's own closer, so there closing brackets
;;; with nothing open are taken as that closer when only whitespace and
;;; comments are around them up to the end; anything after them is still
;;; `unbalanced`.
;;;
;;; LIMITS, each a failure naming itself: lists nested deeper than 1000 is
;;; `depth` (a quote mark or a `#;` waiting for its datum counts as one level);
;;; more than 200000 nodes (atoms and lists, the lists quote marks build
;;; included, counted as read, inside a `#;` datum too) is `nodes`; a token or a string longer than
;;; 65536 bytes is `token`. The reader keeps its own stack, so no limit
;;; leans on the host's.
;;;
;;; It is a reader of forms, not of Scheme: it builds what a form's shape
;;; shows and evaluates nothing.
(library (theourgia reader)
  (export read-forms read-library-piece read-datum reader-failure? reader-failure-reason
          number-text? number-text-text
          read-file-header header-byte-limit library-name-proper
          reader-depth-limit reader-node-limit reader-token-limit)
  (import (rnrs))

  (define reader-depth-limit 1000)
  (define reader-node-limit 200000)
  (define reader-token-limit 65536)

  (define-record-type (reader-failure make-reader-failure reader-failure?) (fields reason))
  (define-record-type (number-text make-number-text number-text?) (fields text))

  ;; ---- characters --------------------------------------------------------------

  (define (delimiter? c)
    (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\;))))
  (define (closer-of c) (case c ((#\() #\)) ((#\[) #\]) (else #\})))
  (define (utf8-width c)
    (let ((k (char->integer c)))
      (cond ((< k #x80) 1) ((< k #x800) 2) ((< k #x10000) 3) (else 4))))
  ;; the bytes of TEXT from I to J, as UTF-8 would write them
  (define (span-bytes text i j)
    (let loop ((k i) (n 0))
      (if (>= k j) n (loop (+ k 1) (+ n (utf8-width (string-ref text k)))))))
  (define (hex-value c)
    (cond ((char<=? #\0 c #\9) (- (char->integer c) 48))
          ((char<=? #\a c #\f) (- (char->integer c) 87))
          ((char<=? #\A c #\F) (- (char->integer c) 55))
          (else #f)))
  ;; -> the scalar value of HEX (1 to 6 hex digits), or #f
  (define (hex-scalar hex)
    (and (<= 1 (string-length hex) 6)
         (let loop ((k 0) (v 0))
           (if (= k (string-length hex))
               (and (or (< v #xD800) (< #xDFFF v #x110000)) v)
               (let ((d (hex-value (string-ref hex k))))
                 (and d (loop (+ k 1) (+ (* v 16) d))))))))
  (define named-characters
    (list (cons "nul" (integer->char 0)) (cons "null" (integer->char 0)) (cons "alarm" (integer->char 7))
          (cons "backspace" (integer->char 8)) (cons "tab" (integer->char 9)) (cons "linefeed" (integer->char 10))
          (cons "newline" (integer->char 10)) (cons "vtab" (integer->char 11)) (cons "page" (integer->char 12))
          (cons "return" (integer->char 13)) (cons "esc" (integer->char 27)) (cons "altmode" (integer->char 27))
          (cons "space" (integer->char 32)) (cons "delete" (integer->char 127)) (cons "rubout" (integer->char 127))))

  ;; ---- atoms --------------------------------------------------------------------

  (define (digits? s from)
    (and (< from (string-length s))
         (let loop ((k from))
           (or (= k (string-length s)) (and (char<=? #\0 (string-ref s k) #\9) (loop (+ k 1)))))))
  ;; A TOKEN: an exact integer when it is at most 18 decimal digits after an
  ;; optional sign, read digit by digit; a number-text when it looks numeric
  ;; in any other way; else a symbol. Chez's `1+`, `1-` and `-1+` are
  ;; symbols, as Chez reads them.
  (define (atom tok)
    (let* ((n (string-length tok))
           (c0 (string-ref tok 0))
           (signed (memv c0 '(#\+ #\-)))
           (from (if signed 1 0)))
      (cond
        ((member tok '("1+" "1-" "-1+")) (string->symbol tok))
        ((and (digits? tok from) (<= (- n from) 18))
         (let loop ((k from) (v 0))
           (if (= k n)
               (if (char=? c0 #\-) (- v) v)
               (loop (+ k 1) (+ (* v 10) (- (char->integer (string-ref tok k)) 48))))))
        ((or (char<=? #\0 c0 #\9)
             (and (memv c0 '(#\+ #\- #\.)) (> n 1)
                  (let ((c1 (string-ref tok 1)))
                    (or (char<=? #\0 c1 #\9)
                        (and (char=? c1 #\.) signed (> n 2) (char<=? #\0 (string-ref tok 2) #\9)))))
             (member tok '("+inf.0" "-inf.0" "+nan.0" "-nan.0")))
         (make-number-text tok))
        (else (string->symbol tok)))))

  ;; ---- the reader ----------------------------------------------------------------
  ;;
  ;; THE STACK: each frame a vector. #(list <closer> <elements, reversed>
  ;; <tail> <dot>), dot 0 before a dot, 1 after it, 2 once the tail is read;
  ;; #(prefix <symbol>), a quote-family mark waiting for its datum;
  ;; #(comment), a `#;` waiting for the datum it drops.
  ;;
  ;; (scan TEXT STOP?) -> (read <forms>), (stopped <value>) or a failure.
  ;; STOP? is asked after every datum delivered to the top level or to a
  ;; list, with the datum and the stack, and a value other than #f ends the
  ;; read with it.
  (define (scan text stop? . options)
    (let ((n (string-length text)) (library-tail? (and (pair? options) (car options))) (tail? #f))
      (call-with-current-continuation
        (lambda (return)
          (let ((frames '()) (forms '()) (depth 0) (nodes 0))
            (define (fail reason) (return (make-reader-failure reason)))
            (define (node!)
              (set! nodes (+ nodes 1))
              (when (> nodes reader-node-limit) (fail 'nodes)))
            ;; A QUOTE-FAMILY MARK NESTS AS A LIST DOES: it is counted in the
            ;; depth while it waits, so `''''x` cannot build a tree deeper
            ;; than the limit.
            (define (push-prefix! sym)
              (set! depth (+ depth 1))
              (when (> depth reader-depth-limit) (fail 'depth))
              (set! frames (cons (vector 'prefix sym) frames)))
            (define (deliver d)
              (let loop ((d d))
                (cond
                  ((null? frames)
                   (set! forms (cons d forms))
                   (let ((v (stop? d frames))) (when v (return (list 'stopped v)))))
                  (else
                   (let ((f (car frames)))
                     (case (vector-ref f 0)
                       ;; THE LIST A MARK BUILDS IS A NODE, charged before it is
                       ;; built, so marks cannot multiply the node limit
                       ((prefix) (set! frames (cdr frames)) (set! depth (- depth 1)) (node!) (loop (list (vector-ref f 1) d)))
                       ((comment) (set! frames (cdr frames)) (set! depth (- depth 1)))
                       (else
                        (case (vector-ref f 4)
                          ((0) (vector-set! f 2 (cons d (vector-ref f 2))))
                          ((1) (vector-set! f 3 d) (vector-set! f 4 2))
                          (else (fail 'unsupported)))
                        (let ((v (stop? d frames))) (when v (return (list 'stopped v)))))))))))
            ;; -> the index after the token that starts at I
            (define (token-end i)
              (let loop ((j i))
                (if (or (= j n) (delimiter? (string-ref text j))) j (loop (+ j 1)))))
            (define (checked-token i j)
              (when (> (span-bytes text i j) reader-token-limit) (fail 'token))
              (substring text i j))
            (define (skip-line i)
              (let loop ((j i)) (if (or (= j n) (char=? (string-ref text j) #\newline)) j (loop (+ j 1)))))
            (define (skip-block-comment i)
              (let loop ((j i) (level 1))
                (cond ((= level 0) j)
                      ((>= (+ j 1) n) (fail 'unbalanced))
                      ((and (char=? (string-ref text j) #\|) (char=? (string-ref text (+ j 1)) #\#)) (loop (+ j 2) (- level 1)))
                      ((and (char=? (string-ref text j) #\#) (char=? (string-ref text (+ j 1)) #\|)) (loop (+ j 2) (+ level 1)))
                      (else (loop (+ j 1) level)))))
            ;; a string from the quote at I: delivered, -> the index after it
            (define (read-string i)
              (let find ((j (+ i 1)))
                (cond ((>= j n) (fail 'unbalanced))
                      ((char=? (string-ref text j) #\\) (find (+ j 2)))
                      ((char=? (string-ref text j) #\")
                       (when (> (span-bytes text i (+ j 1)) reader-token-limit) (fail 'token))
                       (node!)
                       (deliver (string-body (+ i 1) j))
                       (+ j 1))
                      (else (find (+ j 1))))))
            (define (string-body from to)
              (let-values (((port extract) (open-string-output-port)))
                (let loop ((k from))
                  (if (>= k to)
                      (extract)
                      (let ((c (string-ref text k)))
                        (if (not (char=? c #\\))
                            (begin (put-char port c) (loop (+ k 1)))
                            (let ((e (string-ref text (+ k 1))))
                              (case e
                                ((#\n) (put-char port #\newline) (loop (+ k 2)))
                                ((#\t) (put-char port #\tab) (loop (+ k 2)))
                                ((#\r) (put-char port (integer->char 13)) (loop (+ k 2)))
                                ((#\a) (put-char port (integer->char 7)) (loop (+ k 2)))
                                ((#\b) (put-char port (integer->char 8)) (loop (+ k 2)))
                                ((#\0) (put-char port (integer->char 0)) (loop (+ k 2)))
                                ((#\x #\X)
                                 (let semi ((m (+ k 2)))
                                   (cond ((>= m to) (fail 'unsupported))
                                         ((char=? (string-ref text m) #\;)
                                          (let ((v (hex-scalar (substring text (+ k 2) m))))
                                            (unless v (fail 'unsupported))
                                            (put-char port (integer->char v))
                                            (loop (+ m 1))))
                                         (else (semi (+ m 1))))))
                                (else
                                 (if (char-whitespace? e)
                                     ;; a line continuation: the escape, the
                                     ;; blanks and one line end are nothing
                                     (let skip ((m (+ k 1)) (seen-newline #f))
                                       (cond ((and (< m to) (char=? (string-ref text m) #\newline) (not seen-newline))
                                              (skip (+ m 1) #t))
                                             ((and (< m to) (char-whitespace? (string-ref text m)) (not (char=? (string-ref text m) #\newline)))
                                              (skip (+ m 1) seen-newline))
                                             (else (loop m))))
                                     (begin (put-char port e) (loop (+ k 2)))))))))))))
            ;; the `#` syntax at I: -> the index after it
            (define (read-hash i)
              (when (>= (+ i 1) n) (fail 'unsupported))
              (let ((c (string-ref text (+ i 1))))
                (cond
                  ((char=? c #\|) (skip-block-comment (+ i 2)))
                  ((char=? c #\;)
                   (set! depth (+ depth 1))
                   (when (> depth reader-depth-limit) (fail 'depth))
                   (set! frames (cons (vector 'comment) frames))
                   (+ i 2))
                  ((char=? c #\!) (skip-line (+ i 2)))
                  ((memv c '(#\t #\f))
                   (let* ((j (token-end (+ i 1))) (tok (checked-token i j)))
                     (cond ((member tok '("#t" "#true")) (node!) (deliver #t) j)
                           ((member tok '("#f" "#false")) (node!) (deliver #f) j)
                           (else (fail 'unsupported)))))
                  ((char=? c #\\)
                   (when (>= (+ i 2) n) (fail 'unbalanced))
                   (let* ((j (token-end (+ i 3))) (name (checked-token (+ i 2) j)))
                     (node!)
                     (cond
                       ((= (string-length name) 1) (deliver (string-ref name 0)) j)
                       ((assoc name named-characters) => (lambda (p) (deliver (cdr p)) j))
                       ((and (memv (string-ref name 0) '(#\x #\X)) (hex-scalar (substring name 1 (string-length name))))
                        => (lambda (v) (deliver (integer->char v)) j))
                       (else (fail 'unsupported)))))
                  ((char<=? #\0 c #\9)
                   (let loop ((j (+ i 1)))
                     (cond ((and (< j n) (char<=? #\0 (string-ref text j) #\9)) (loop (+ j 1)))
                           ((and (< j n) (memv (string-ref text j) '(#\= #\#))) (fail 'label))
                           (else (fail 'unsupported)))))
                  ((memv c '(#\e #\E #\i #\I #\x #\X #\b #\B #\o #\O #\d #\D))
                   (let* ((j (token-end i)) (tok (checked-token i j)))
                     (node!)
                     (deliver (make-number-text tok))
                     j))
                  (else (fail 'unsupported)))))
            (define (close! c)
              (when (and (null? frames) (not library-tail?)) (fail 'unbalanced))
              (if (null? frames) (set! tail? #t) (close-frame! c)))
            (define (close-frame! c)
              (let ((f (car frames)))
                (unless (and (eq? (vector-ref f 0) 'list) (char=? (vector-ref f 1) c)) (fail 'unbalanced))
                (when (= (vector-ref f 4) 1) (fail 'unsupported))
                (set! frames (cdr frames))
                (set! depth (- depth 1))
                (node!)
                (deliver (fold-left (lambda (tail x) (cons x tail)) (vector-ref f 3) (vector-ref f 2)))))
            (let loop ((i (if (and (> n 0) (char=? (string-ref text 0) (integer->char #xFEFF))) 1 0)))
              (if (= i n)
                  (if (null? frames) (list 'read (reverse forms)) (fail 'unbalanced))
                  (let ((c (string-ref text i)))
                    (cond
                      ((char-whitespace? c) (loop (+ i 1)))
                      ((char=? c #\;) (loop (skip-line i)))
                      ;; PAST THE LIBRARY'S CLOSER: more closers, block
                      ;; comments, and nothing else
                      ((and tail? (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\|))
                       (loop (skip-block-comment (+ i 2))))
                      ((and tail? (not (memv c '(#\) #\] #\})))) (fail 'unbalanced))
                      ((memv c '(#\( #\[ #\{))
                       (set! depth (+ depth 1))
                       (when (> depth reader-depth-limit) (fail 'depth))
                       (set! frames (cons (vector 'list (closer-of c) '() '() 0) frames))
                       (loop (+ i 1)))
                      ((memv c '(#\) #\] #\})) (close! c) (loop (+ i 1)))
                      ((char=? c #\") (loop (read-string i)))
                      ((char=? c #\') (push-prefix! 'quote) (loop (+ i 1)))
                      ((char=? c #\`) (push-prefix! 'quasiquote) (loop (+ i 1)))
                      ((char=? c #\,)
                       (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\@))
                           (begin (push-prefix! 'unquote-splicing) (loop (+ i 2)))
                           (begin (push-prefix! 'unquote) (loop (+ i 1)))))
                      ((char=? c #\#) (loop (read-hash i)))
                      ((char=? c #\|) (fail 'unsupported))
                      (else
                       (let* ((j (token-end i)) (tok (checked-token i j)))
                         (if (string=? tok ".")
                             (let ((f (and (pair? frames) (car frames))))
                               (unless (and f (eq? (vector-ref f 0) 'list) (pair? (vector-ref f 2)) (= (vector-ref f 4) 0))
                                 (fail 'unsupported))
                               (vector-set! f 4 1)
                               (loop j))
                             (begin (node!) (deliver (atom tok)) (loop j))))))))))))))

  ;; -> the forms of TEXT, in order, or a failure.
  (define (read-forms text)
    (let ((r (scan text (lambda (d frames) #f))))
      (if (reader-failure? r) r (cadr r))))
  ;; -> the same for a piece of a file whose header is a library form:
  ;; closers with nothing open at the end are the library's.
  (define (read-library-piece text)
    (let ((r (scan text (lambda (d frames) #f) #t)))
      (if (reader-failure? r) r (cadr r))))

  ;; -> the first form of TEXT, a failure, or the end-of-file object when
  ;; TEXT holds no form.
  (define (read-datum text)
    (let ((r (scan text (lambda (d frames) (and (null? frames) (list d))))))
      (cond ((reader-failure? r) r)
            ((eq? (car r) 'stopped) (car (cadr r)))
            (else (eof-object)))))

  ;; ---- a library's name ----------------------------------------------------------
  ;;
  ;; -> the parts of a library name, its trailing version reference
  ;; dropped, or #f when NAME is not one. The one spelling store and name
  ;; use both read.
  (define (library-name-proper name)
    (and (pair? name) (list? name)
         (let* ((last (list-ref name (- (length name) 1)))
                (parts (if (and (list? last) (> (length name) 1)) (reverse (cdr (reverse name))) name)))
           (and (pair? parts) (for-all symbol? parts) parts))))

  ;; ---- a text file's header ------------------------------------------------------
  ;;
  ;; THE HEADER IS READ FROM A PREFIX, its first 65536 bytes at most: the
  ;; caller joins the file's pieces in order, since a comment-only first
  ;; piece, a marker line or a cut may fall inside the header. Forms are
  ;; read until the header is complete, and nothing after it is read:
  ;;   - a first form `(library <name> ...` whose `(import ...)` form has
  ;;     closed: (library <name parts> <import specs>). The library form
  ;;     itself need not close, so a file whose definitions are cut inside
  ;;     its one library form still has its header. A library form that
  ;;     closes with no import form gives no imports.
  ;;   - a first form `(import ...)`, a script: (script <import specs>).
  ;;   - anything else, a read failure, or a prefix that ends first: #f.
  (define header-byte-limit 65536)
  (define (import-form? d) (and (pair? d) (eq? (car d) 'import) (list? d)))
  (define (library-header form)
    (let ((name (and (pair? (cdr form)) (library-name-proper (cadr form)))))
      (and name
           (let ((imports (find import-form? (cddr form))))
             (list 'library name (if imports (cdr imports) '()))))))
  (define (read-file-header text)
    (let ((r (scan text
                   (lambda (d frames)
                     (cond
                       ((null? frames)
                        (cond ((import-form? d) (list 'script (cdr d)))
                              ((and (pair? d) (eq? (car d) 'library) (list? d)) (or (library-header d) 'none))
                              (else 'none)))
                       ((and (import-form? d) (null? (cdr frames)) (eq? (vector-ref (car frames) 0) 'list)
                             (= (vector-ref (car frames) 4) 0))
                        (let ((before (reverse (vector-ref (car frames) 2))))
                          (if (eq? (car before) 'library)
                              (or (library-header before) 'none)
                              'none)))
                       ((and (null? (cdr frames)) (eq? (vector-ref (car frames) 0) 'list)
                             (let ((es (vector-ref (car frames) 2)))
                               (and (pair? es) (null? (cdr es)) (not (memq (car es) '(library import))))))
                        'none)
                       (else #f))))))
      (and (pair? r) (eq? (car r) 'stopped) (not (eq? (cadr r) 'none)) (cadr r))))
)
