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

;; Markdown, in three independent pieces.
;;
;; THEY ARE SEPARATE BECAUSE THEIR EVIDENCE IS SEPARATE. Splitting a
;; file into byte ranges is judged by comparing bytes; turning a range
;; into an AST is judged by generating markdown back from an AST that
;; never had any bytes; finding references is judged against a list
;; someone wrote by hand. Folded into one pass, a defect in the second
;; would be hidden by the first -- the original bytes are replayed on
;; export, so a parser that understood nothing at all would still round
;; trip every file perfectly.
;;
;;   md-split / md-join    the byte ranges of section 2.4
;;   md->blocks / blocks->md   the document AST of section 2.1
;;   md-ref-nodes          the references the parser found, in document order
(library (theourgia md)
  (export md-split md-join
          doc-front doc-src doc-sections
          section-level section-title section-heading-src section-src
          make-section section?
          md->blocks blocks->md md-ref-nodes)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          (rnrs unicode) (rnrs records syntactic) (rnrs io ports))

  (define-record-type doc
    (fields front src sections))

  (define-record-type section
    (fields level title heading-src src))

  ;; ---- lines ---------------------------------------------------------------

  ;; A LINE IS (start content-end next-start). content-end excludes the
  ;; terminator, next-start includes it -- so heading-src can be "up to
  ;; and including the newline" and the body can start exactly after it
  ;; without either side computing the terminator's width. CRLF and a
  ;; final line with no terminator both fall out of the same two numbers.
  (define (line-at text i)
    (let ((n (string-length text)))
      (let loop ((j i))
        (cond
          ((>= j n) (list i n n))
          ((char=? (string-ref text j) #\newline) (list i j (+ j 1)))
          ((and (char=? (string-ref text j) #\return)
                (< (+ j 1) n)
                (char=? (string-ref text (+ j 1)) #\newline))
           (list i j (+ j 2)))
          (else (loop (+ j 1)))))))

  (define (blank-line? text start end)
    (let loop ((i start))
      (cond ((>= i end) #t)
            ((memv (string-ref text i) '(#\space #\tab)) (loop (+ i 1)))
            (else #f))))

  ;; ---- fences --------------------------------------------------------------

  ;; A HASH INSIDE A CODE FENCE IS NOT A HEADING. Without this the
  ;; splitter cuts a new section in the middle of a code block, and the
  ;; two halves are then exported around a heading line that the file
  ;; never contained.
  (define (fence-at text start end)
    (let loop ((i start) (indent 0))
      (cond
        ((and (< i end) (char=? (string-ref text i) #\space) (< indent 3))
         (loop (+ i 1) (+ indent 1)))
        ((>= i end) #f)
        ((or (char=? (string-ref text i) #\`) (char=? (string-ref text i) #\~))
         (let* ((c (string-ref text i))
                (run (let count ((j i) (k 0))
                       (if (and (< j end) (char=? (string-ref text j) c))
                           (count (+ j 1) (+ k 1))
                           k))))
           (and (>= run 3) (cons c run))))
        (else #f))))

  ;; ---- headings ------------------------------------------------------------

  ;; ATX ONLY, AT THE LINE START, WITH AT MOST THREE SPACES OF INDENT --
  ;; four spaces makes it an indented code block. Returns (level . title)
  ;; or #f. Closing hashes are dropped from the title; `#` with no space
  ;; after it is not a heading.
  (define (heading-at text start end)
    (let loop ((i start) (indent 0))
      (cond
        ((and (< i end) (char=? (string-ref text i) #\space) (< indent 3))
         (loop (+ i 1) (+ indent 1)))
        ((>= i end) #f)
        ((char=? (string-ref text i) #\#)
         (let* ((level (let count ((j i) (k 0))
                         (if (and (< j end) (char=? (string-ref text j) #\#))
                             (count (+ j 1) (+ k 1))
                             k)))
                (after (+ i level)))
           (and (<= 1 level 6)
                (or (= after end)
                    (memv (string-ref text after) '(#\space #\tab)))
                (cons level (heading-title text after end)))))
        (else #f))))

  (define (heading-title text after end)
    (let* ((from (let skip ((i after))
                   (if (and (< i end) (memv (string-ref text i) '(#\space #\tab)))
                       (skip (+ i 1))
                       i)))
           ;; TRAILING HASHES ARE A CLOSING SEQUENCE ONLY WHEN SPACE-
           ;; SEPARATED. "## A ##" has the title "A"; "## A#" has the
           ;; title "A#", because the hashes are part of the word.
           (to (let back ((i end))
                 (if (and (> i from) (memv (string-ref text (- i 1)) '(#\space #\tab)))
                     (back (- i 1))
                     i)))
           (no-hash (let back ((i to))
                      (if (and (> i from) (char=? (string-ref text (- i 1)) #\#))
                          (back (- i 1))
                          i))))
      (if (and (< no-hash to)
               (or (= no-hash from)
                   (memv (string-ref text (- no-hash 1)) '(#\space #\tab))))
          (let ((cut (let back ((i no-hash))
                       (if (and (> i from) (memv (string-ref text (- i 1)) '(#\space #\tab)))
                           (back (- i 1))
                           i))))
            (substring text from cut))
          (substring text from to))))

  ;; ---- front matter --------------------------------------------------------

  ;; ONLY AT THE VERY START OF THE FILE, and only closed by a line that
  ;; is exactly the same marker. An unterminated opener is ordinary body
  ;; text -- treating it as front matter would swallow the whole file.
  (define (front-end text)
    (let ((n (string-length text)))
      (if (= n 0)
          0
          (let ((first (line-at text 0)))
            (if (not (marker-line? text (car first) (cadr first)))
                0
                (let loop ((i (caddr first)))
                  (if (>= i n)
                      0
                      (let ((l (line-at text i)))
                        (if (marker-line? text (car l) (cadr l))
                            (caddr l)
                            (loop (caddr l)))))))))))

  (define (marker-line? text start end)
    (and (= (- end start) 3)
         (char=? (string-ref text start) #\-)
         (char=? (string-ref text (+ start 1)) #\-)
         (char=? (string-ref text (+ start 2)) #\-)))

  ;; ---- split ---------------------------------------------------------------

  (define (md-split text)
    (let* ((n (string-length text))
           (front-to (front-end text))
           (heads (heading-starts text front-to))
           (body-to (if (null? heads) n (car (car heads)))))
      (make-doc (substring text 0 front-to)
                (substring text front-to body-to)
                (let loop ((hs heads) (out '()))
                  (if (null? hs)
                      (reverse out)
                      (let* ((h (car hs))
                             (start (car h))
                             (content-end (cadr h))
                             (next-start (caddr h))
                             (level (car (cadddr h)))
                             (title (cdr (cadddr h)))
                             (src-to (if (null? (cdr hs)) n (car (car (cdr hs))))))
                        (loop (cdr hs)
                              (cons (make-section level title
                                                  (substring text start next-start)
                                                  (substring text next-start src-to))
                                    out))))))))

  ;; Each entry is (start content-end next-start (level . title)).
  (define (heading-starts text from)
    (let ((n (string-length text)))
      (let loop ((i from) (fence #f) (out '()))
        (if (>= i n)
            (reverse out)
            (let* ((l (line-at text i))
                   (start (car l)) (content-end (cadr l)) (next (caddr l))
                   (f (fence-at text start content-end)))
              (cond
                ;; inside a fence: only a matching closer ends it
                (fence
                 (loop next
                       (if (and f (char=? (car f) (car fence)) (>= (cdr f) (cdr fence)))
                           #f
                           fence)
                       out))
                (f (loop next f out))
                (else
                 (let ((h (heading-at text start content-end)))
                   (loop next #f
                         (if h (cons (list start content-end next h) out) out)))))))))) 

  ;; ---- join ----------------------------------------------------------------

  ;; THE SEPARATOR RULE OF SECTION 2.4. Between two non-empty pieces, if
  ;; the earlier one does not end in a newline, one is added and it
  ;; belongs to the LATER piece. In a file nobody reordered, the only
  ;; piece that can lack a final newline is the last one, so an
  ;; unmodified export never adds anything -- which is what makes the
  ;; rule safe to apply unconditionally.
  (define (md-join front src sections)
    (let ((pieces (append (list front src)
                          (apply append
                                 (map (lambda (s)
                                        (list (section-heading-src s) (section-src s)))
                                      sections)))))
      (let loop ((ps (filter (lambda (p) (> (string-length p) 0)) pieces))
                 (out '()))
        (cond
          ((null? ps) (apply string-append (reverse out)))
          ((null? (cdr ps)) (loop '() (cons (car ps) out)))
          (else
           (let ((p (car ps)))
             (loop (cdr ps)
                   (cons (if (ends-with-newline? p) p (string-append p "\n"))
                         out))))))))

  (define (ends-with-newline? s)
    (let ((n (string-length s)))
      (and (> n 0) (char=? (string-ref s (- n 1)) #\newline))))

  ;; ---- the document AST (section 2.1) --------------------------------------

  ;; WHAT IS NOT RECOGNISED BECOMES raw-md, WHOLE. Tables, footnotes,
  ;; images, reference links and HTML are all v1 raw-md by design -- so
  ;; "I do not know this" is a legal answer rather than a failure, and
  ;; the parser can be small on purpose.
  ;; THAT IS ALSO WHY THIS FILE'S OWN EVIDENCE CANNOT BE A ROUND TRIP:
  ;; a parser that answered raw-md for everything would round trip every
  ;; file byte for byte, because export replays the stored bytes.

  (define (lines-of text)
    (let ((n (string-length text)))
      (let loop ((i 0) (out '()))
        (if (>= i n)
            (reverse out)
            (let ((l (line-at text i)))
              (loop (caddr l) (cons l out)))))))

  (define (line-text text l) (substring text (car l) (cadr l)))

  (define (indent-of s)
    (let loop ((i 0) (k 0))
      (cond ((>= i (string-length s)) k)
            ((char=? (string-ref s i) #\space) (loop (+ i 1) (+ k 1)))
            ((char=? (string-ref s i) #\tab) (loop (+ i 1) (+ k 4)))
            (else k))))

  (define (trim-left s)
    (let loop ((i 0))
      (if (and (< i (string-length s)) (memv (string-ref s i) '(#\space #\tab)))
          (loop (+ i 1))
          (substring s i (string-length s)))))

  (define (trim-right s)
    (let loop ((i (string-length s)))
      (if (and (> i 0) (memv (string-ref s (- i 1)) '(#\space #\tab #\return)))
          (loop (- i 1))
          (substring s 0 i))))

  (define (blank? s) (= 0 (string-length (trim-left (trim-right s)))))

  (define (starts-with? s p)
    (and (>= (string-length s) (string-length p))
         (string=? (substring s 0 (string-length p)) p)))

  ;; A THEMATIC BREAK: three or more of - * _ with only spaces between.
  (define (hr-line? s)
    (let ((t (trim-right (trim-left s))))
      (and (>= (string-length t) 3)
           (let ((c (string-ref t 0)))
             (and (memv c '(#\- #\* #\_))
                  (let loop ((i 0) (k 0))
                    (cond ((>= i (string-length t)) (>= k 3))
                          ((char=? (string-ref t i) c) (loop (+ i 1) (+ k 1)))
                          ((char=? (string-ref t i) #\space) (loop (+ i 1) k))
                          (else #f))))))))

  ;; A BULLET OR AN ORDERED MARKER, returning (ordered? start rest-index)
  ;; or #f. The marker must be followed by a space: "-word" is a word.
  (define (list-marker s)
    (let* ((t (trim-left s))
           (drop (- (string-length s) (string-length t))))
      (cond
        ((= 0 (string-length t)) #f)
        ((and (memv (string-ref t 0) '(#\- #\* #\+))
              (> (string-length t) 1)
              (char=? (string-ref t 1) #\space))
         (list #f #f (+ drop 2)))
        ((char-numeric? (string-ref t 0))
         (let loop ((i 0))
           (cond
             ((and (< i (string-length t)) (char-numeric? (string-ref t i)))
              (loop (+ i 1)))
             ((and (< (+ i 1) (string-length t))
                   (memv (string-ref t i) '(#\. #\)))
                   (char=? (string-ref t (+ i 1)) #\space))
              (list #t (string->number (substring t 0 i)) (+ drop i 2)))
             (else #f))))
        (else #f))))

  ;; SPANS ARE BYTE OFFSETS INTO THE SRC THE TOP CALL WAS GIVEN. Every block
  ;; node and every reference carries (span <start> <end>), half-open, in the
  ;; UTF-8 encoding of that src, after the node's scalar elements and before
  ;; its children. A block's span runs from the start of its first line to
  ;; the end of its last line's terminator, container prefixes and interior
  ;; lines included; a reference's from the first byte of `[[` to the byte
  ;; after `]]`, read from the cursor before escapes and entities are
  ;; decoded.
  ;;
  ;; THE ORIGIN MAP. Each line of the string a taker walks has a PLACE, a
  ;; vector (start next content): the start and next-start of the top src's
  ;; line it came from, and the top byte offset of its own first character.
  ;; A quote or a list recurses on a joined child string, as it always has,
  ;; and hands the recursion the places of the lines it kept, each content
  ;; offset moved past the prefix it stripped; so a quote in a list in a
  ;; quote maps to the top src, never to the string it was handed.
  (define (char-width c)
    (let ((n (char->integer c)))
      (cond ((< n #x80) 1) ((< n #x800) 2) ((< n #x10000) 3) (else 4))))

  (define (utf8-width text from to)
    (let loop ((i from) (k 0))
      (if (>= i to) k (loop (+ i 1) (+ k (char-width (string-ref text i)))))))

  (define (top-places text ls)
    (let ((out (make-vector (vector-length ls))))
      (let loop ((i 0) (b 0))
        (if (= i (vector-length ls))
            out
            (let* ((l (vector-ref ls i))
                   (next (+ b (utf8-width text (car l) (caddr l)))))
              (vector-set! out i (vector b next b))
              (loop (+ i 1) next))))))

  ;; KEPT is a list of (place . stripped-bytes), one per line handed down.
  (define (child-places kept)
    (list->vector
      (map (lambda (k)
             (let ((p (car k)))
               (vector (vector-ref p 0) (vector-ref p 1) (+ (vector-ref p 2) (cdr k)))))
           kept)))

  (define (lines-span places i j)
    (list 'span (vector-ref (vector-ref places i) 0) (vector-ref (vector-ref places j) 1)))

  (define (md->blocks text)
    (let ((ls (list->vector (lines-of text))))
      (blocks-of text ls (top-places text ls) (string->utf8 text))))

  ;; SRC is the top src's bytes, which a raw-md node's text is sliced from.
  (define (blocks-of text ls places src)
    (let loop ((i 0) (out '()))
      (if (>= i (vector-length ls))
          (reverse out)
          (let* ((l (vector-ref ls i))
                 (s (line-text text l)))
            (cond
              ((blank? s) (loop (+ i 1) out))
              ((fence-at text (car l) (cadr l))
               (let-values (((node next) (take-fence text ls places i)))
                 (loop next (cons node out))))
              ((hr-line? s) (loop (+ i 1) (cons (list 'hr (lines-span places i i)) out)))
              ((starts-with? (trim-left s) ">")
               (let-values (((node next) (take-quote text ls places src i)))
                 (loop next (cons node out))))
              ((list-marker s)
               (let-values (((node next) (take-list text ls places src i)))
                 (loop next (cons node out))))
              ;; FOUR SPACES IS AN INDENTED CODE BLOCK, and v1 keeps it
              ;; whole rather than pretending to understand it. It is code,
              ;; so its refs clause is empty.
              ((>= (indent-of s) 4)
               (let-values (((node next) (take-raw text ls places src i #f)))
                 (loop next (cons node out))))
              ;; A TABLE, AN HTML BLOCK OR A FOOTNOTE: recognised only
              ;; well enough to be kept whole, its references found.
              ((or (starts-with? (trim-left s) "|")
                   (starts-with? (trim-left s) "<")
                   (starts-with? (trim-left s) "[^"))
               (let-values (((node next) (take-raw text ls places src i #t)))
                 (loop next (cons node out))))
              (else
               (let-values (((node next) (take-para text ls places i)))
                 (loop next (cons node out)))))))))

  ;; AN UNCLOSED FENCE RUNS TO THE END: its span ends at the last line's.
  (define (take-fence text ls places i)
    (let* ((l (vector-ref ls i))
           (open (fence-at text (car l) (cadr l)))
           (head (trim-right (trim-left (line-text text l))))
           (lang (let ((rest (substring head (cdr open) (string-length head))))
                   (trim-right (trim-left rest))))
           (fence (make-string (cdr open) (car open))))
      (let loop ((j (+ i 1)) (body '()))
        (cond
          ((>= j (vector-length ls))
           (values (list 'code fence lang (join-lines (reverse body)) (lines-span places i (- j 1))) j))
          (else
           (let* ((m (vector-ref ls j))
                  (f (fence-at text (car m) (cadr m))))
             (if (and f (char=? (car f) (car open)) (>= (cdr f) (cdr open)))
                 (values (list 'code fence lang (join-lines (reverse body)) (lines-span places i j)) (+ j 1))
                 (loop (+ j 1) (cons (line-text text m) body)))))))))

;; NEWLINES BETWEEN THE LINES, NOT AFTER THE LAST ONE. Appending one
  ;; after the last line put the terminator inside the block's own text,
  ;; so a paragraph read back from generated markdown carried a trailing
  ;; "\n" that the AST it came from did not have -- every round trip
  ;; differed by exactly that character.
  (define (join-lines ls)
    (if (null? ls)
        ""
        (let loop ((xs (cdr ls)) (out (list (car ls))))
          (if (null? xs)
              (apply string-append (reverse out))
              (loop (cdr xs) (cons (car xs) (cons "\n" out)))))))

  ;; The child string is the lines with `>` and one space taken off; each
  ;; kept line's place moves past the bytes taken.
  (define (take-quote text ls places src i)
    (let loop ((j i) (body '()) (kept '()))
      (if (or (>= j (vector-length ls))
              (not (starts-with? (trim-left (line-text text (vector-ref ls j))) ">")))
          (let* ((child (join-lines (reverse body)))
                 (cls (list->vector (lines-of child))))
            (values (cons* 'quote (lines-span places i (- j 1))
                           (blocks-of child cls (child-places (reverse kept)) src))
                    j))
          (let* ((l (vector-ref ls j))
                 (whole (line-text text l))
                 (s (trim-left whole))
                 (rest (substring s 1 (string-length s)))
                 (spaced (and (> (string-length rest) 0) (char=? (string-ref rest 0) #\space)))
                 (taken (+ (- (string-length whole) (string-length s)) 1 (if spaced 1 0))))
            (loop (+ j 1)
                  (cons (if spaced (substring rest 1 (string-length rest)) rest) body)
                  (cons (cons (vector-ref places j) (utf8-width text (car l) (+ (car l) taken))) kept))))))

  ;; ONE LEVEL OF LIST, ITEMS SPLIT AT THE MARKERS. Continuation lines
  ;; belong to the item above them; a blank line ends the list unless the
  ;; next line is another marker. An item is a list of (text taken index):
  ;; the rest of the line it keeps, the bytes of the marker or indentation
  ;; it took off the front, and the line's index here. A blank line is in no
  ;; item, and the blank this consumes after the last item is in no node:
  ;; the list's span ends at the last line an item kept.
  (define (take-list text ls places src i)
    (let* ((first (list-marker (line-text text (vector-ref ls i))))
           (ordered? (car first))
           (start (cadr first)))
      (define (entry j chars)
        (let ((l (vector-ref ls j)))
          (list (substring text (+ (car l) chars) (cadr l)) (utf8-width text (car l) (+ (car l) chars)) j)))
      (let loop ((j i) (items '()) (current '()))
        (define (flush)
          (if (null? current) items (cons (reverse current) items)))
        (define (done next)
          (values (make-list-node ordered? start (reverse (flush)) places src) next))
        (cond
          ((>= j (vector-length ls)) (done j))
          (else
           (let* ((s (line-text text (vector-ref ls j)))
                  (m (list-marker s)))
             (cond
               ((and m (or (null? current) #t) (not (= j i)) (not (null? current))
                     (eq? (car m) ordered?))
                (loop (+ j 1) (flush) (list (entry j (caddr m)))))
               (m (loop (+ j 1) items (cons (entry j (caddr m)) current)))
               ((blank? s)
                (if (and (< (+ j 1) (vector-length ls))
                         (list-marker (line-text text (vector-ref ls (+ j 1)))))
                    (loop (+ j 1) items current)
                    (done (+ j 1))))
               ((>= (indent-of s) 2)
                (loop (+ j 1) items (cons (entry j (- (string-length s) (string-length (trim-left s)))) current)))
               (else (done j)))))))))

  (define (make-list-node ordered? start items places src)
    (define (item-node lines)
      (let* ((child (join-lines (map car lines)))
             (cls (list->vector (lines-of child))))
        (cons* 'item (lines-span places (caddr (car lines)) (caddr (car (reverse lines))))
               (blocks-of child cls
                          (child-places (map (lambda (e) (cons (vector-ref places (caddr e)) (cadr e))) lines))
                          src))))
    (append (list 'list ordered? (or start 1)
                  (lines-span places (caddr (car (car items))) (caddr (car (reverse (car (reverse items)))))))
            (map item-node items)))

  ;; A PARAGRAPH'S CHARACTER MAP: one top-src byte offset per character of
  ;; the joined lines, the LF between two lines given the byte after the
  ;; first line's content. parse-inline reads a reference's ends from it.
  (define (inline-map text ls places i j)
    (let loop ((k i) (out '()))
      (if (= k j)
          (list->vector (reverse out))
          (let ((l (vector-ref ls k)))
            (let chars ((c (car l)) (b (vector-ref (vector-ref places k) 2)) (out out))
              (if (< c (cadr l))
                  (chars (+ c 1) (+ b (char-width (string-ref text c))) (cons b out))
                  (loop (+ k 1) (if (< (+ k 1) j) (cons b out) out))))))))

  (define (take-para text ls places i)
    (let loop ((j i) (body '()))
      (if (or (>= j (vector-length ls))
              (let ((s (line-text text (vector-ref ls j))))
                (or (blank? s)
                    (and (> j i)
                         (or (fence-at text (car (vector-ref ls j)) (cadr (vector-ref ls j)))
                             (hr-line? s)
                             (list-marker s)
                             (starts-with? (trim-left s) ">")
                             (starts-with? (trim-left s) "|"))))))
          (values (cons* 'para (lines-span places i (- j 1))
                         (parse-inline (join-lines (reverse body)) (inline-map text ls places i j) 0))
                  j)
          (loop (+ j 1) (cons (line-text text (vector-ref ls j)) body)))))

  ;; RAW-MD IS VERBATIM: its text is the top src's bytes between its span's
  ;; ends, so CRLF, a container's prefixes and the first line's prefix are
  ;; all kept, and a last line with no terminator keeps none. Its references
  ;; are what parse-inline finds over its lines, unless it is code.
  (define (take-raw text ls places src i refs?)
    (let loop ((j i))
      (if (or (>= j (vector-length ls))
              (blank? (line-text text (vector-ref ls j))))
          (let* ((sp (lines-span places i (- j 1)))
                 (bytes (let ((o (make-bytevector (- (caddr sp) (cadr sp)))))
                          (bytevector-copy! src (cadr sp) o 0 (- (caddr sp) (cadr sp)))
                          o)))
            (values (list 'raw-md (utf8->string bytes) sp
                          (cons 'refs
                                (if refs?
                                    (inline-refs
                                      (parse-inline (join-lines (let lines ((k i) (out '()))
                                                                  (if (= k j) (reverse out)
                                                                      (lines (+ k 1) (cons (line-text text (vector-ref ls k)) out)))))
                                                    (inline-map text ls places i j) 0))
                                    '())))
                    j))
          (loop (+ j 1)))))

  ;; ---- inline --------------------------------------------------------------

  ;; ONE LEFT-TO-RIGHT SCAN. Emphasis is matched by looking ahead for the
  ;; closing run; an opener with no closer stays literal text, which is
  ;; what a reader sees too. BYTES is the paragraph's character map and BASE
  ;; the index in it of this string's first character: a sub-parse of
  ;; strong, em or a link's text is handed the same map and its own base, so
  ;; a reference inside it is spanned in the top src.
  (define (parse-inline text bytes base)
    (let ((n (string-length text)))
      (let loop ((i 0) (plain '()) (out '()))
        (define (flush)
          (if (null? plain)
              out
              (cons (list->string (reverse plain)) out)))
        (cond
          ((>= i n) (reverse (flush)))
          ;; AN ESCAPE IS THE CHARACTER ITSELF. The renderer writes "\\*"
          ;; for a literal asterisk, so the reader has to give back "*"
          ;; -- otherwise the backslash accumulates on every round trip.
          ((and (char=? (string-ref text i) #\\) (< (+ i 1) n))
           (loop (+ i 2) (cons (string-ref text (+ i 1)) plain) out))
          ((entity-at text i)
           => (lambda (pair)
                (loop (cdr pair) (cons (car pair) plain) out)))
          ;; a reference: [[...]], its ends the bytes of `[[` and `]]`
          ((and (char=? (string-ref text i) #\[)
                (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\[)
                (find-close text (+ i 2) "]]"))
           => (lambda (close)
                (let* ((inner (substring text (+ i 2) close))
                       (lexeme (substring text i (+ close 2))))
                  (loop (+ close 2) '()
                        (cons (list 'ref lexeme (ref-key inner)
                                    (list 'span (vector-ref bytes (+ base i))
                                          (+ (vector-ref bytes (+ base close 1)) 1)))
                              (flush))))))
          ;; inline code: a backtick run closed by an equal run
          ((char=? (string-ref text i) #\`)
           (let* ((run (let count ((j i) (k 0))
                         (if (and (< j n) (char=? (string-ref text j) #\`))
                             (count (+ j 1) (+ k 1))
                             k)))
                  (close (find-run text (+ i run) #\` run)))
             (if close
                 (loop (+ close run) '()
                       (cons (list 'code (substring text (+ i run) close)) (flush)))
                 (loop (+ i 1) (cons (string-ref text i) plain) out))))
          ;; strong before emphasis: ** and __ are two characters
          ((and (delim-at? text i 2) (find-delim text (+ i 2) (string-ref text i) 2))
           => (lambda (close)
                (loop (+ close 2) '()
                      (cons (cons 'strong (parse-inline (substring text (+ i 2) close) bytes (+ base i 2)))
                            (flush)))))
          ((and (delim-at? text i 1) (find-delim text (+ i 1) (string-ref text i) 1))
           => (lambda (close)
                (loop (+ close 1) '()
                      (cons (cons 'em (parse-inline (substring text (+ i 1) close) bytes (+ base i 1)))
                            (flush)))))
          ;; a link: [text](href)
          ((and (char=? (string-ref text i) #\[)
                (link-at text i))
           => (lambda (parts)
                (loop (caddr parts) '()
                      (cons (cons 'link (cons (cadr parts) (parse-inline (car parts) bytes (+ base i 1))))
                            (flush)))))
          ;; a hard break: two spaces before a newline
          ((and (char=? (string-ref text i) #\newline)
                (>= i 2)
                (char=? (string-ref text (- i 1)) #\space)
                (char=? (string-ref text (- i 2)) #\space))
           (loop (+ i 1) '()
                 (cons '(br)
                       (let ((o (flush)))
                         (if (null? o)
                             o
                             (cons (trim-right (car o)) (cdr o)))))))
          (else (loop (+ i 1) (cons (string-ref text i) plain) out))))))

  ;; Returns (char . next-index) for the entities the renderer emits. `&#13;`
  ;; is a CR: the printer spells a CR that an LF follows with it, since the
  ;; two bytes would read back as one line end.
  (define (entity-at text i)
    (let ((n (string-length text)))
      (define (try name c)
        (and (<= (+ i (string-length name)) n)
             (string=? (substring text i (+ i (string-length name))) name)
             (cons c (+ i (string-length name)))))
      (and (< i n)
           (char=? (string-ref text i) #\&)
           (or (try "&lt;" #\<) (try "&gt;" #\>) (try "&amp;" #\&) (try "&#13;" #\return)))))

  (define (delim-at? text i k)
    (let ((n (string-length text)))
      (and (< (+ i k) n)
           (memv (string-ref text i) '(#\* #\_))
           (let same ((j i) (m 0))
             (if (and (< j n) (char=? (string-ref text j) (string-ref text i)))
                 (same (+ j 1) (+ m 1))
                 (>= m k)))
           (not (memv (string-ref text (+ i k)) '(#\space #\newline))))))

  (define (find-delim text from c k)
    (let ((n (string-length text)))
      (let loop ((i from))
        (cond
          ((>= (+ i k) (+ n 1)) #f)
          ((>= i n) #f)
          ((and (char=? (string-ref text i) c)
                (let same ((j i) (m 0))
                  (if (and (< j n) (char=? (string-ref text j) c))
                      (same (+ j 1) (+ m 1))
                      (>= m k)))
                (> i from)
                (not (memv (string-ref text (- i 1)) '(#\space #\newline))))
           i)
          (else (loop (+ i 1)))))))

  (define (find-close text from close)
    (let ((n (string-length text)) (m (string-length close)))
      (let loop ((i from))
        (cond ((> (+ i m) n) #f)
              ((string=? (substring text i (+ i m)) close) i)
              (else (loop (+ i 1)))))))

  (define (find-run text from c k)
    (let ((n (string-length text)))
      (let loop ((i from))
        (cond
          ((>= i n) #f)
          ((char=? (string-ref text i) c)
           (let ((run (let count ((j i) (m 0))
                        (if (and (< j n) (char=? (string-ref text j) c))
                            (count (+ j 1) (+ m 1))
                            m))))
             (if (= run k) i (loop (+ i run)))))
          (else (loop (+ i 1)))))))

  (define (link-at text i)
    (let* ((n (string-length text))
           (close (find-close text (+ i 1) "]")))
      (and close
           (< (+ close 1) n)
           (char=? (string-ref text (+ close 1)) #\()
           (let ((rparen (find-close text (+ close 2) ")")))
             (and rparen
                  (list (substring text (+ i 1) close)
                        (substring text (+ close 2) rparen)
                        (+ rparen 1)))))))

  ;; [[foo.md]] AND [[foo]] ARE THE SAME KEY, and the lexeme each was
  ;; written with is kept beside it. A display form after a bar is part
  ;; of the lexeme, not of the key.
  (define (ref-key inner)
    (let* ((bar (let loop ((i 0))
                  (cond ((>= i (string-length inner)) #f)
                        ((char=? (string-ref inner i) #\|) i)
                        (else (loop (+ i 1))))))
           (target (trim-right (trim-left (if bar (substring inner 0 bar) inner))))
           (dot (let loop ((i (string-length target)))
                  (cond ((<= i 0) #f)
                        ((char=? (string-ref target (- i 1)) #\.) (- i 1))
                        (else (loop (- i 1)))))))
      (if (and dot (string=? (substring target dot (string-length target)) ".md"))
          (substring target 0 dot)
          target)))

  ;; ---- rendering (A7) ------------------------------------------------------

  ;; AN AST WITH NO src HAS TO BE PRINTED, and printed so that reading it
  ;; back gives the same AST. That is what the escaping is for: a
  ;; paragraph whose text begins "# " must not come out as a heading, and
  ;; a literal asterisk must not come out as emphasis.
  ;;
  ;; A PRINTED BLOCK IS A LIST OF PIECES: (line . "<text>"), written with an
  ;; LF after it and prefixed or indented by every container around it, or
  ;; (raw . "<bytes>"), a raw-md node's text, written as it is. A raw-md's
  ;; bytes already carry the prefixes and the marker the source had, so no
  ;; ancestor touches them; a hand-built raw-md inside a hand-built quote
  ;; carries its own `> ` for the same reason. Spans are not printed.
  (define (blocks->md blocks)
    (apply string-append
           (map (lambda (p) (if (eq? (car p) 'line) (string-append (cdr p) "\n") (cdr p)))
                (blocks-pieces blocks #t))))

  ;; Blocks of a document or a quote are separated by a blank line; an
  ;; item's are not, since a blank line inside an item would end the list.
  ;; TWO LISTS OF THE SAME KIND IN A ROW are separated by two blank lines:
  ;; one would read back as a blank between two items of one list. An item
  ;; cannot hold two such lists, its lines holding no blank line.
  (define (blocks-pieces blocks blank-between?)
    (define (same-kind-lists? a b)
      (and (eq? (car a) 'list) (eq? (car b) 'list) (eq? (cadr a) (cadr b))))
    (let loop ((bs (no-spans blocks)) (prev #f) (out '()))
      (if (null? bs)
          (apply append (reverse out))
          (loop (cdr bs) (car bs)
                (cons (block-pieces (car bs))
                      (cond ((not (and blank-between? prev)) out)
                            ((same-kind-lists? prev (car bs)) (cons (list (cons 'line "") (cons 'line "")) out))
                            (else (cons (list (cons 'line "")) out))))))))

  (define (no-spans xs)
    (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'span)))) xs))

  (define (block-pieces b)
    (case (car b)
      ((para) (map (lambda (s) (cons 'line s)) (split-at-lf (para->md (no-spans (cdr b))))))
      ((hr) (list (cons 'line "---")))
      ((raw-md) (let ((t (cadr b)))
                  (list (cons 'raw (if (ends-with-newline? t) t (string-append t "\n"))))))
      ;; A NON-EMPTY BODY IS FOLLOWED BY EXACTLY ONE LF before the closing
      ;; fence, so a body whose last line is blank keeps it.
      ((code)
       (let ((fence (cadr b)) (lang (caddr b)) (body (cadddr b)))
         (append (list (cons 'line (string-append fence lang)))
                 (if (= 0 (string-length body))
                     '()
                     (map (lambda (s) (cons 'line s))
                          (let ((ls (split-at-lf (string-append body "\n")))) (reverse (cdr (reverse ls))))))
                 (list (cons 'line fence)))))
      ((quote) (prefix-lines (blocks-pieces (cdr b) #t) "> "))
      ((list)
       (let ((ordered? (cadr b)) (start (caddr b)))
         (let loop ((items (no-spans (cdddr b))) (k start) (out '()))
           (if (null? items)
               (apply append (reverse out))
               (let ((marker (if ordered?
                                 (string-append (number->string k) ". ")
                                 "- ")))
                 (loop (cdr items) (+ k 1)
                       (cons (indent-continuation marker (blocks-pieces (cdr (car items)) #f)) out)))))))
      (else (list (cons 'line (string-append ";; unknown block " (symbol->string (car b))))))))

  ;; Every LF-separated segment, the last one included.
  (define (split-at-lf s)
    (let loop ((i 0) (from 0) (out '()))
      (cond ((= i (string-length s)) (reverse (cons (substring s from i) out)))
            ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s from i) out)))
            (else (loop (+ i 1) from out)))))

  ;; AN EMPTY LINE TAKES THE PREFIX TRIMMED, `>`; a line of spaces keeps the
  ;; whole prefix and its bytes, which a reader strips back to the spaces.
  (define (prefix-lines pieces prefix)
    (map (lambda (p)
           (cond ((eq? (car p) 'raw) p)
                 ((= 0 (string-length (cdr p))) (cons 'line (trim-right prefix)))
                 (else (cons 'line (string-append prefix (cdr p))))))
         pieces))

  ;; The first line carries the marker; the rest are indented under it so
  ;; that reading the result back puts them in the same item. A raw piece
  ;; first carries its own marker.
  (define (indent-continuation marker pieces)
    (let ((pad (make-string (string-length marker) #\space)))
      (let loop ((ps pieces) (first #t) (out '()))
        (if (null? ps)
            (reverse out)
            (let ((p (car ps)))
              (loop (cdr ps) #f
                    (cons (if (eq? (car p) 'raw) p (cons 'line (string-append (if first marker pad) (cdr p))))
                          out)))))))

  ;; A PARAGRAPH IS SPELLED AS CHARACTERS, EACH MARKED BY WHETHER A TEXT
  ;; STRING EMITTED IT, so that the rules below touch text and nothing a
  ;; delimiter, a code span, a reference's lexeme or a hard break wrote. The
  ;; four spellings, in this order: a CR whose next emitted byte is an LF is
  ;; `&#13;`; on a line an LF of the text ends, the last space of a trailing
  ;; run of two or more is escaped; then each line is judged as the parser
  ;; reads a line, on the bytes now emitted: one that is blank, or indented
  ;; four or more with a first character a text wrote, takes a backslash at
  ;; index 0, and otherwise a marker at its first character after spaces and
  ;; tabs is escaped. One line-start escape per line, and never a backslash
  ;; before the entity's `&`. An indented line a code span or a lexeme
  ;; begins is left alone: the parser keeps such a line in the paragraph, and
  ;; a backslash there would be read back as part of the code or the lexeme.
  (define (para->md xs)
    (list->string (map car (escape-line-starts (spell-hard-breaks (spell-crs (inline-chars xs)))))))

  (define (chars-of s text?) (map (lambda (c) (cons c text?)) (string->list s)))

  (define (inline-chars xs) (apply append (map one-inline-chars xs)))

  (define (one-inline-chars x)
    (cond
      ((string? x) (chars-of (escape-text x) #t))
      ((eq? (car x) 'em) (append (chars-of "*" #f) (inline-chars (cdr x)) (chars-of "*" #f)))
      ((eq? (car x) 'strong) (append (chars-of "**" #f) (inline-chars (cdr x)) (chars-of "**" #f)))
      ((eq? (car x) 'code) (chars-of (fence-inline (cadr x)) #f))
      ((eq? (car x) 'link)
       (append (chars-of "[" #f) (inline-chars (cddr x)) (chars-of (string-append "](" (cadr x) ")") #f)))
      ((eq? (car x) 'ref) (chars-of (cadr x) #f))
      ((eq? (car x) 'br) (chars-of "  \n" #f))
      (else '())))

  ;; A text CR that an LF follows, or that ends the paragraph (the printer's
  ;; LF follows it), would read back as a CRLF line end.
  (define (spell-crs cs)
    (let loop ((cs cs) (out '()))
      (cond ((null? cs) (reverse out))
            ((and (char=? (caar cs) #\return) (cdar cs)
                  (or (null? (cdr cs)) (char=? (car (cadr cs)) #\newline)))
             (loop (cdr cs) (append (reverse (chars-of "&#13;" #t)) out)))
            (else (loop (cdr cs) (cons (car cs) out))))))

  ;; Two spaces before an LF read as a hard break, so a text LF after a run
  ;; of two or more spaces gets the run's last space escaped.
  (define (spell-hard-breaks cs)
    (let loop ((cs cs) (out '()))
      (cond ((null? cs) (reverse out))
            ((and (char=? (caar cs) #\newline) (cdar cs)
                  (pair? out) (char=? (caar out) #\space) (cdar out)
                  (pair? (cdr out)) (char=? (car (cadr out)) #\space))
             (loop (cdr cs) (cons (car cs) (cons (car out) (cons (cons #\\ #t) (cdr out))))))
            (else (loop (cdr cs) (cons (car cs) out))))))

  (define (escape-line-starts cs)
    (let loop ((cs cs) (line '()) (out '()))
      (cond ((null? cs) (append (reverse out) (escape-line (reverse line))))
            ((char=? (caar cs) #\newline)
             (loop (cdr cs) '() (append (cons (car cs) (reverse (escape-line (reverse line)))) out)))
            (else (loop (cdr cs) (cons (car cs) line) out)))))

  (define (escape-line line)
    (let* ((s (list->string (map car line)))
           (p (let skip ((i 0)) (if (and (< i (string-length s)) (memv (string-ref s i) '(#\space #\tab))) (skip (+ i 1)) i)))
           (at (lambda (k) (append (take line k) (list (cons #\\ #t)) (list-tail line k)))))
      (cond
        ((blank? s) (at 0))
        ((>= (indent-of s) 4) (if (cdar line) (at 0) line))
        ((and (< p (string-length s)) (cdr (list-ref line p)) (marker-at s p)) => at)
        (else line))))

  (define (take xs k) (if (= k 0) '() (cons (car xs) (take (cdr xs) (- k 1)))))

  ;; Where a line read from P would start a block of another kind, the index
  ;; to escape, or #f: a bullet, an ordered marker (its `.` or `)`), a quote,
  ;; a tilde fence, an hr, a table.
  (define (marker-at s p)
    (let* ((n (string-length s)) (c (string-ref s p)))
      (cond
        ((and (memv c '(#\- #\+)) (< (+ p 1) n) (char=? (string-ref s (+ p 1)) #\space)) p)
        ((char-numeric? c)
         (let digits ((i p))
           (cond ((and (< i n) (char-numeric? (string-ref s i))) (digits (+ i 1)))
                 ((and (< i n) (memv (string-ref s i) '(#\. #\)))
                       (or (= (+ i 1) n) (char=? (string-ref s (+ i 1)) #\space)))
                  i)
                 (else #f))))
        ((char=? c #\>) p)
        ((char=? c #\|) p)
        ((and (char=? c #\~) (<= (+ p 3) n) (string=? (substring s p (+ p 3)) "~~~")) p)
        ((and (memv c '(#\- #\_ #\*)) (hr-line? s)) p)
        (else #f))))

  ;; A CODE SPAN IS FENCED BY A RUN LONGER THAN ANY RUN INSIDE IT, so a
  ;; span containing backticks still comes back as one span.
  (define (fence-inline body)
    (let* ((longest (let loop ((i 0) (run 0) (best 0))
                      (cond
                        ((>= i (string-length body)) (max run best))
                        ((char=? (string-ref body i) #\`) (loop (+ i 1) (+ run 1) best))
                        (else (loop (+ i 1) 0 (max run best))))))
           (fence (make-string (+ longest 1) #\`))
           (pad (if (or (= 0 (string-length body))
                        (char=? (string-ref body 0) #\`)
                        (char=? (string-ref body (- (string-length body) 1)) #\`))
                    " "
                    "")))
      (string-append fence pad body pad fence)))

  ;; THE ESCAPES ARE THE ONES THAT CHANGE THE PARSE. A backslash before
  ;; the character is CommonMark's own escape and reads back as the
  ;; character itself; the leading "# " of a string is escaped for the
  ;; same reason -- unescaped it becomes a heading, which would move the
  ;; text out of the block it belongs to. The line-start escapes are the
  ;; paragraph's (para->md), since a string may start mid-line.
  (define (escape-text s)
    (let ()
      (let-values (((port get) (open-string-output-port)))
        (let loop ((i 0))
          (if (>= i (string-length s))
              (get)
              (let ((c (string-ref s i)))
                (cond
                  ((memv c '(#\* #\_ #\` #\[ #\] #\\))
                   (put-char port #\\) (put-char port c) (loop (+ i 1)))
                  ((char=? c #\<) (put-string port "&lt;") (loop (+ i 1)))
                  ((char=? c #\&) (put-string port "&amp;") (loop (+ i 1)))
                  ((and (= i 0) (char=? c #\#))
                   (put-char port #\\) (put-char port c) (loop (+ i 1)))
                  ((and (= i 0) (memv c '(#\- #\+ #\>)))
                   (put-char port #\\) (put-char port c) (loop (+ i 1)))
                  (else (put-char port c) (loop (+ i 1))))))))))

  ;; ---- the references (A2) -------------------------------------------------

  ;; THE PARSER IS THE ONE SUPPLIER OF REFERENCES. A raw-md node carries the
  ;; references parse-inline finds over its lines, so the parser reaches
  ;; what it does not take apart, and code is decided once, by the parser:
  ;; a fence (a quoted one too), indented code, a code span (one that spans
  ;; lines too) and an escaped `\[[` hold none. The answer is every
  ;; reference node of md->blocks of TEXT, `(ref <lexeme> <key> (span s e))`,
  ;; in document order.
  (define (md-ref-nodes text) (tree-refs (md->blocks text)))

  ;; Through para inlines (em, strong, a link's text), quote and item
  ;; children, and raw-md's refs clause.
  (define (tree-refs blocks)
    (apply append
           (map (lambda (b)
                  (case (car b)
                    ((para) (inline-refs (cdr b)))
                    ((quote) (tree-refs (no-spans (cdr b))))
                    ((list) (apply append (map (lambda (it) (tree-refs (no-spans (cdr it)))) (no-spans (cdddr b)))))
                    ((raw-md) (let ((r (find (lambda (x) (and (pair? x) (eq? (car x) 'refs))) (cddr b))))
                                (if r (cdr r) '())))
                    (else '())))
                blocks)))

  (define (inline-refs xs)
    (apply append
           (map (lambda (x)
                  (cond ((not (pair? x)) '())
                        ((eq? (car x) 'ref) (list x))
                        ((memq (car x) '(em strong)) (inline-refs (cdr x)))
                        ((eq? (car x) 'link) (inline-refs (cddr x)))
                        (else '())))
                xs)))
)
