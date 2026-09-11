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
;;   md-refs               the reference lexer
(library (theourgia md)
  (export md-split md-join
          doc-front doc-src doc-sections
          section-level section-title section-heading-src section-src
          make-section section?
          md->blocks blocks->md md-refs)
  (import (rnrs base) (rnrs control) (rnrs lists)
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

  (define (md->blocks text)
    (let ((ls (list->vector (lines-of text))))
      (let loop ((i 0) (out '()))
        (if (>= i (vector-length ls))
            (reverse out)
            (let* ((l (vector-ref ls i))
                   (s (line-text text l)))
              (cond
                ((blank? s) (loop (+ i 1) out))
                ((fence-at text (car l) (cadr l))
                 (let-values (((node next) (take-fence text ls i)))
                   (loop next (cons node out))))
                ((hr-line? s) (loop (+ i 1) (cons '(hr) out)))
                ((starts-with? (trim-left s) ">")
                 (let-values (((node next) (take-quote text ls i)))
                   (loop next (cons node out))))
                ((list-marker s)
                 (let-values (((node next) (take-list text ls i)))
                   (loop next (cons node out))))
                ;; FOUR SPACES IS AN INDENTED CODE BLOCK, and v1 keeps it
                ;; whole rather than pretending to understand it.
                ((>= (indent-of s) 4)
                 (let-values (((node next) (take-raw text ls i)))
                   (loop next (cons node out))))
                ;; A TABLE, AN HTML BLOCK OR A FOOTNOTE: recognised only
                ;; well enough to be kept whole.
                ((or (starts-with? (trim-left s) "|")
                     (starts-with? (trim-left s) "<")
                     (starts-with? (trim-left s) "[^"))
                 (let-values (((node next) (take-raw text ls i)))
                   (loop next (cons node out))))
                (else
                 (let-values (((node next) (take-para text ls i)))
                   (loop next (cons node out))))))))))

  (define (take-fence text ls i)
    (let* ((l (vector-ref ls i))
           (open (fence-at text (car l) (cadr l)))
           (head (trim-right (trim-left (line-text text l))))
           (lang (let ((rest (substring head (cdr open) (string-length head))))
                   (trim-right (trim-left rest))))
           (fence (make-string (cdr open) (car open))))
      (let loop ((j (+ i 1)) (body '()))
        (cond
          ((>= j (vector-length ls))
           (values (list 'code fence lang (join-lines (reverse body))) j))
          (else
           (let* ((m (vector-ref ls j))
                  (f (fence-at text (car m) (cadr m))))
             (if (and f (char=? (car f) (car open)) (>= (cdr f) (cdr open)))
                 (values (list 'code fence lang (join-lines (reverse body))) (+ j 1))
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

  (define (take-quote text ls i)
    (let loop ((j i) (body '()))
      (if (or (>= j (vector-length ls))
              (not (starts-with? (trim-left (line-text text (vector-ref ls j))) ">")))
          (values (cons 'quote (md->blocks (join-lines (reverse body)))) j)
          (let* ((s (trim-left (line-text text (vector-ref ls j))))
                 (rest (substring s 1 (string-length s))))
            (loop (+ j 1)
                  (cons (if (and (> (string-length rest) 0)
                                 (char=? (string-ref rest 0) #\space))
                            (substring rest 1 (string-length rest))
                            rest)
                        body))))))

  ;; ONE LEVEL OF LIST, ITEMS SPLIT AT THE MARKERS. Continuation lines
  ;; belong to the item above them; a blank line ends the list unless the
  ;; next line is another marker.
  (define (take-list text ls i)
    (let* ((first (list-marker (line-text text (vector-ref ls i))))
           (ordered? (car first))
           (start (cadr first)))
      (let loop ((j i) (items '()) (current '()))
        (define (flush)
          (if (null? current) items (cons (reverse current) items)))
        (cond
          ((>= j (vector-length ls))
           (values (make-list-node ordered? start (reverse (flush))) j))
          (else
           (let* ((s (line-text text (vector-ref ls j)))
                  (m (list-marker s)))
             (cond
               ((and m (or (null? current) #t) (not (= j i)) (not (null? current))
                     (eq? (car m) ordered?))
                (loop (+ j 1) (flush) (list (substring s (caddr m) (string-length s)))))
               (m (loop (+ j 1) items
                        (cons (substring s (caddr m) (string-length s)) current)))
               ((blank? s)
                (if (and (< (+ j 1) (vector-length ls))
                         (list-marker (line-text text (vector-ref ls (+ j 1)))))
                    (loop (+ j 1) items current)
                    (values (make-list-node ordered? start (reverse (flush))) (+ j 1))))
               ((>= (indent-of s) 2)
                (loop (+ j 1) items (cons (trim-left s) current)))
               (else
                (values (make-list-node ordered? start (reverse (flush))) j)))))))))

  (define (make-list-node ordered? start items)
    (append (list 'list ordered? (or start 1))
            (map (lambda (lines) (cons 'item (md->blocks (join-lines lines)))) items)))

  (define (take-para text ls i)
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
          (values (cons 'para (parse-inline (join-lines (reverse body)))) j)
          (loop (+ j 1) (cons (line-text text (vector-ref ls j)) body)))))

  (define (take-raw text ls i)
    (let loop ((j i) (body '()))
      (if (or (>= j (vector-length ls))
              (blank? (line-text text (vector-ref ls j))))
          (values (list 'raw-md (join-lines (reverse body))) j)
          (loop (+ j 1) (cons (line-text text (vector-ref ls j)) body)))))

  ;; ---- inline --------------------------------------------------------------

  ;; ONE LEFT-TO-RIGHT SCAN. Emphasis is matched by looking ahead for the
  ;; closing run; an opener with no closer stays literal text, which is
  ;; what a reader sees too.
  (define (parse-inline text)
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
          ;; a reference: [[...]]
          ((and (char=? (string-ref text i) #\[)
                (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\[)
                (find-close text (+ i 2) "]]"))
           => (lambda (close)
                (let* ((inner (substring text (+ i 2) close))
                       (lexeme (substring text i (+ close 2))))
                  (loop (+ close 2) '()
                        (cons (list 'ref lexeme (ref-key inner)) (flush))))))
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
                      (cons (cons 'strong (parse-inline (substring text (+ i 2) close)))
                            (flush)))))
          ((and (delim-at? text i 1) (find-delim text (+ i 1) (string-ref text i) 1))
           => (lambda (close)
                (loop (+ close 1) '()
                      (cons (cons 'em (parse-inline (substring text (+ i 1) close)))
                            (flush)))))
          ;; a link: [text](href)
          ((and (char=? (string-ref text i) #\[)
                (link-at text i))
           => (lambda (parts)
                (loop (caddr parts) '()
                      (cons (cons 'link (cons (cadr parts) (parse-inline (car parts))))
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

  ;; Returns (char . next-index) for the entities the renderer emits.
  (define (entity-at text i)
    (let ((n (string-length text)))
      (define (try name c)
        (and (<= (+ i (string-length name)) n)
             (string=? (substring text i (+ i (string-length name))) name)
             (cons c (+ i (string-length name)))))
      (and (< i n)
           (char=? (string-ref text i) #\&)
           (or (try "&lt;" #\<) (try "&gt;" #\>) (try "&amp;" #\&)))))

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
  (define (blocks->md blocks)
    (let loop ((bs blocks) (out '()))
      (if (null? bs)
          (apply string-append (reverse out))
          (loop (cdr bs)
                (cons (block->md (car bs))
                      (if (null? out) out (cons "\n" out)))))))

  (define (block->md b)
    (case (car b)
      ((para) (string-append (inline->md (cdr b)) "\n"))
      ((hr) "---\n")
      ((raw-md) (let ((t (cadr b)))
                  (if (ends-with-newline? t) t (string-append t "\n"))))
      ((code)
       (let ((fence (cadr b)) (lang (caddr b)) (body (cadddr b)))
         (string-append fence lang "\n"
                        (if (or (= 0 (string-length body)) (ends-with-newline? body))
                            body
                            (string-append body "\n"))
                        fence "\n")))
      ((quote)
       (prefix-lines (blocks->md (cdr b)) "> "))
      ((list)
       (let ((ordered? (cadr b)) (start (caddr b)))
         (let loop ((items (cdddr b)) (k start) (out '()))
           (if (null? items)
               (apply string-append (reverse out))
               (let* ((marker (if ordered?
                                  (string-append (number->string k) ". ")
                                  "- "))
                      (body (blocks->md (cdr (car items)))))
                 (loop (cdr items) (+ k 1)
                       (cons (indent-continuation marker body) out)))))))
      (else (string-append ";; unknown block " (symbol->string (car b)) "\n"))))

  (define (prefix-lines text prefix)
    (let loop ((ls (lines-of text)) (out '()))
      (if (null? ls)
          (apply string-append (reverse out))
          (let ((s (line-text text (car ls))))
            (loop (cdr ls)
                  (cons (if (blank? s)
                            (string-append (trim-right prefix) "\n")
                            (string-append prefix s "\n"))
                        out))))))

  ;; The first line carries the marker; the rest are indented under it so
  ;; that reading the result back puts them in the same item.
  (define (indent-continuation marker body)
    (let ((pad (make-string (string-length marker) #\space)))
      (let loop ((ls (lines-of body)) (first #t) (out '()))
        (if (null? ls)
            (apply string-append (reverse out))
            (let ((s (line-text body (car ls))))
              (loop (cdr ls) #f
                    (cons (string-append (if first marker pad) s "\n") out)))))))

  (define (inline->md xs)
    (let loop ((xs xs) (out '()))
      (if (null? xs)
          (apply string-append (reverse out))
          (loop (cdr xs) (cons (one-inline->md (car xs)) out)))))

  (define (one-inline->md x)
    (cond
      ((string? x) (escape-text x))
      ((eq? (car x) 'em) (string-append "*" (inline->md (cdr x)) "*"))
      ((eq? (car x) 'strong) (string-append "**" (inline->md (cdr x)) "**"))
      ((eq? (car x) 'code) (fence-inline (cadr x)))
      ((eq? (car x) 'link)
       (string-append "[" (inline->md (cddr x)) "](" (cadr x) ")"))
      ((eq? (car x) 'ref) (cadr x))
      ((eq? (car x) 'br) "  \n")
      (else "")))

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
  ;; character itself; the leading "# " of a paragraph is escaped for the
  ;; same reason -- unescaped it becomes a heading, which would move the
  ;; text out of the block it belongs to.
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

  ;; ---- the reference lexer (A2) --------------------------------------------

  ;; REFERENCES ARE FOUND BY LEXING, NOT BY WALKING THE AST, because they
  ;; also have to be found inside raw-md -- the nodes the parser did not
  ;; understand. Code is where they must NOT be found: fenced blocks,
  ;; indented blocks and inline spans all quote their contents.
  (define (md-refs text)
    (let* ((ls (lines-of text))
           (n (string-length text)))
      (let loop ((ls ls) (fence #f) (out '()))
        (if (null? ls)
            (reverse out)
            (let* ((l (car ls))
                   (start (car l)) (content-end (cadr l))
                   (s (line-text text l))
                   (f (fence-at text start content-end)))
              (cond
                (fence
                 (loop (cdr ls)
                       (if (and f (char=? (car f) (car fence)) (>= (cdr f) (cdr fence)))
                           #f
                           fence)
                       out))
                (f (loop (cdr ls) f out))
                ((>= (indent-of s) 4) (loop (cdr ls) #f out))
                (else (loop (cdr ls) #f (append (reverse (line-refs text start content-end))
                                                out)))))))))

  ;; Within one line: skip inline code spans, collect [[...]].
  (define (line-refs text start end)
    (let loop ((i start) (out '()))
      (cond
        ((>= i end) (reverse out))
        ((char=? (string-ref text i) #\`)
         (let* ((run (let count ((j i) (k 0))
                       (if (and (< j end) (char=? (string-ref text j) #\`))
                           (count (+ j 1) (+ k 1))
                           k)))
                (close (let scan ((j (+ i run)))
                         (cond
                           ((>= j end) #f)
                           ((char=? (string-ref text j) #\`)
                            (let ((r (let count ((k j) (m 0))
                                       (if (and (< k end) (char=? (string-ref text k) #\`))
                                           (count (+ k 1) (+ m 1))
                                           m))))
                              (if (= r run) j (scan (+ j r)))))
                           (else (scan (+ j 1)))))))
           (if close (loop (+ close run) out) (loop (+ i run) out))))
        ((and (char=? (string-ref text i) #\[)
              (< (+ i 1) end)
              (char=? (string-ref text (+ i 1)) #\[))
         (let ((close (let scan ((j (+ i 2)))
                        (cond ((>= (+ j 1) end) #f)
                              ((and (char=? (string-ref text j) #\])
                                    (char=? (string-ref text (+ j 1)) #\])) j)
                              (else (scan (+ j 1)))))))
           (if close
               (loop (+ close 2)
                     (cons (list i (substring text i (+ close 2))
                                 (ref-key (substring text (+ i 2) close)))
                           out))
               (loop (+ i 1) out))))
        (else (loop (+ i 1) out)))))
)
