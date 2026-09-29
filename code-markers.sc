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
(library (theourgia code-markers)
  (export projection-encode projection-encode-map projection-wrapping
          projection-decode marker-line projection-failure projection-header-wrapper?
          projection-control projection-header-line)
  ;; The marker grammar moved to (theourgia markers), which
  ;; CANNOT REACH THIS LIBRARY -- it imports (rnrs) and two names from
  ;; (theourgia wire), and nothing else. That, not "it imports nothing",
  ;; is the property the move was made for, and `test/closures.sc` reads
  ;; it off the import graph. The names come back here unchanged, so no
  ;; caller of this library changed. See markers.sc for why.
  (import (only (theourgia markers) wrapping marker-line family projection-header-wrapper?
                projection-control projection-failure hex-decode safe-id? header-read block-read)
          (rnrs) (theourgia text-code) (theourgia languages) (theourgia wire)
          (only (theourgia digest) bytevector->hex))






  (define (adjust-line entry b delta)
    (let ((f (family entry b)))
      (if (not f) b
          (let ((at (list-ref f 2)))
            (if (= delta 1)
                (bytes-append (byte-slice b 0 at) #vu8(64) (byte-slice b at (bytevector-length b)))
                (bytes-append (byte-slice b 0 at) (byte-slice b (+ at 1) (bytevector-length b))))))))
  ;; -> (values escaped inserted): the body with one "@" inserted into every
  ;; marker-like line, and the offsets IN THE ESCAPED BYTES where those "@"
  ;; bytes sit. One walk makes both, so the offsets are the ones written.
  (define (escape-body-map entry b)
    (let loop ((rows (byte-lines b)) (parts '()) (inserted '()) (at 0))
      (if (null? rows)
          (values (apply bytes-append (reverse parts)) (reverse inserted))
          (let* ((r (car rows))
                 (content (byte-slice b (car r) (cadr r)))
                 (eol (byte-slice b (cadr r) (caddr r)))
                 (f (family entry content))
                 (line (adjust-line entry content 1)))
            (loop (cdr rows) (cons eol (cons line parts))
                  (if f (cons (+ at (list-ref f 2)) inserted) inserted)
                  (+ at (bytevector-length line) (bytevector-length eol)))))))
  (define (ends-lf? b)
    (and (> (bytevector-length b) 0) (= 10 (bytevector-u8-ref b (- (bytevector-length b) 1)))))
  (define (padding b) (if (or (zero? (bytevector-length b)) (ends-lf? b)) 0 1))
  (define (unpad b pad)
    (case pad ((0) b)
      ((1) (unless (ends-lf? b) (projection-failure 'invalid-padding))
           (byte-slice b 0 (- (bytevector-length b) 1)))
      (else (projection-failure 'invalid-padding))))






  (define (projection-header-line entry header mode pad)
    (let ((hex (bytevector->hex (string->utf8 (sexpr->string-extended
                    (storable-encode (append '(code-projection 1) header (list mode pad))))))))
      (when (> (string-length hex) 65536) (projection-failure 'header-limit))
      (marker-line entry (string-append "@file " hex))))

  ;; Header = (store file cut), entries = ((id-or-new raw-bytes) ...).
  ;; Framing LF belongs to the following marker and is never source text.
  (define (projection-encode entry header entries)
    (let-values (((bytes pieces) (projection-encode-map entry header entries))) bytes))

  ;; THE PROJECTION AND WHERE EVERY BYTE OF IT CAME FROM, written in one pass
  ;; so the map is the layout that was written and not a second description
  ;; of it. -> (values bytes pieces), pieces in file order:
  ;;   (source <id> <start> <end> <src-start> (<inserted> ...))
  ;;   (control <id> <start> <end> <src-start>)
  ;; `start` and `end` are projected byte offsets, half-open. A source piece
  ;; holds the block's own bytes beginning at `src-start` of its src, with an
  ;; escape "@" at each projected offset in `inserted`. A control -- a pad LF
  ;; or a marker line -- holds none, and names the block and the src offset
  ;; of the source piece it precedes (#f and 0 when no block follows).
  ;;
  ;; KEY: THE FIRST BLOCK'S PREFIX IS ITS SOURCE, NOT A CONTROL. The BOM and
  ;; the position-sensitive lines stay at the top of the file, before the
  ;; @file line, and they are the first block's own bytes; so the first
  ;; block has two source pieces (the prefix at src 0, then its body at
  ;; src prefix-size) and every other block one. A piece of zero length is
  ;; kept (an empty block still owns its place in the file); an empty
  ;; prefix is not a piece.
  (define (projection-encode-map entry header entries)
    (let* ((first (if (pair? entries) (cadar entries) #vu8()))
           (first-id (and (pair? entries) (caar entries)))
           (prefix-size (source-prefix-size first))
           (prefix (byte-slice first 0 prefix-size))
           (head (and header (append '(code-projection 1) header (list 'text (padding prefix)))))
           (head-line (if head
                          (marker-line entry
                            (string-append "@file " (bytevector->hex (string->utf8
                              (sexpr->string-extended (storable-encode head))))))
                          #vu8()))
           (opening (bytes-append (if (= 1 (padding prefix)) #vu8(10) #vu8()) head-line)))
      ;; parts: (kind bytes id src-start inserted), inserted relative to the
      ;; part, NEWEST FIRST (reversed when they are placed).
      (let loop ((xs entries) (first? #t) (previous-body prefix)
                 (parts (cons (list 'control opening first-id prefix-size '())
                              (if (> prefix-size 0) (list (list 'source prefix first-id 0 '())) '()))))
        (if (null? xs)
            (let place ((ps (reverse parts)) (at 0) (bytes '()) (pieces '()))
              (if (null? ps)
                  (values (apply bytes-append (reverse bytes)) (reverse pieces))
                  (let* ((p (car ps)) (n (bytevector-length (cadr p))) (end (+ at n)))
                    (place (cdr ps) end (cons (cadr p) bytes)
                           ;; A CONTROL OF ZERO BYTES IS NO PIECE: it holds no
                           ;; offset and precedes nothing on its own.
                           (if (and (eq? (car p) 'control) (= n 0))
                               pieces
                               (cons (if (eq? (car p) 'source)
                                         (list 'source (caddr p) at end (cadddr p)
                                               (map (lambda (i) (+ at i)) (list-ref p 4)))
                                         (list 'control (caddr p) at end (cadddr p)))
                                     pieces))))))
            ;; THE PAD IS THE ORIGINAL'S: for the first block it is taken over
            ;; everything written before its marker (the prefix, its pad LF and
            ;; the @file line), which is the prefix alone only without a header.
            (let* ((id (caar xs))
                   (pad (cond ((not first?) (padding previous-body))
                              ((not header) (padding prefix))
                              (else (padding (bytes-append prefix opening)))))
                   (body (cadar xs))
                   (body (if first? (byte-slice body prefix-size (bytevector-length body)) body))
                   (src-start (if first? prefix-size 0))
                   (mark (marker-line entry (string-append "@block " id " pad " (number->string pad))))
                   (framing (if (and (= pad 1) (not first?)) (bytes-append #vu8(10) mark) mark)))
              (let-values (((escaped inserted) (escape-body-map entry body)))
                (loop (cdr xs) #f escaped
                      (cons (list 'source escaped id src-start inserted)
                            (cons (list 'control framing id src-start '()) parts)))))))))

  ;; The comment wrapping a projection writes its marker lines in, the one
  ;; property of a language entry the projected bytes depend on: the marker
  ;; lines and the escape family are both read through it.
  (define (projection-wrapping entry) (wrapping entry))

  (define (projection-decode entry bytes)
    (let* ((rows (byte-lines bytes))
           (controlled? (exists (lambda (r) (let ((f (family entry (byte-slice bytes (car r) (cadr r)))))
                                              (and f (= 1 (car f))))) rows)))
      (if (not controlled?) (list #f (list (list "new" bytes)))
          (let loop ((rs rows) (header #f) (current #f) (parts '()) (out '()) (seen? #f))
            (if (null? rs)
                (let ((body (apply bytes-append (reverse parts))))
                  (list header (reverse (if current (cons (list current body) out)
                                            (if (> (bytevector-length body) 0) (cons (list "new" body) out) out)))))
                (let* ((r (car rs)) (content (byte-slice bytes (car r) (cadr r)))
                       (eol (byte-slice bytes (cadr r) (caddr r)))
                       (raw (byte-slice bytes (car r) (caddr r))) (f (family entry content)))
                  (cond
                    ((not f) (loop (cdr rs) header current (cons raw parts) out seen?))
                    ((> (car f) 1) (loop (cdr rs) header current
                                        (cons (bytes-append (adjust-line entry content -1) eol) parts) out seen?))
                    ((string-prefix-at? (cadr f) "file " 0)
                     (when (or header current seen?) (projection-failure 'duplicate-header))
                     (let* ((h (header-read (substring (cadr f) 5 (string-length (cadr f)))))
                            (prefix (unpad (apply bytes-append (reverse parts)) (list-ref h 6))))
                       (unless (= (bytevector-length prefix) (source-prefix-size prefix))
                         (projection-failure 'misplaced-header))
                       (loop (cdr rs) h #f (list prefix) out #t)))
                    (else
                     (let* ((mark (block-read (substring (cadr f) 6 (string-length (cadr f)))))
                            (body (unpad (apply bytes-append (reverse parts)) (cadr mark)))
                            (prefix? (and (not current) (or header (= (bytevector-length body) (source-prefix-size body))))))
                       (loop (cdr rs) header (car mark) (if prefix? (list body) '())
                             (if current (cons (list current body) out)
                                 (if (and (not prefix?) (> (bytevector-length body) 0)) (cons (list "new" body) out) out)) #t))))))))))
)
