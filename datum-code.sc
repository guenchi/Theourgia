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
(library (theourgia datum-code)
  (export datum-source-read datum-names datum-print record-definition-shape?)
  (import (rnrs) (theourgia source-lex) (theourgia text-code)
          (only (theourgia ffi) source-reader-open source-reader-next source-reader-at source-datum-print))

  ;; (define-record-type <name spec> <clause> ...) as R6RS shapes it: the
  ;; name spec a symbol or three symbols; each clause one of R6RS's, each
  ;; kind at most once, parent and parent-rtd not both:
  ;;   (fields <field spec> ...)   a symbol, (immutable n [accessor]) or
  ;;                               (mutable n [accessor mutator])
  ;;   (parent <name>)  (protocol <expression>)  (sealed <boolean>)
  ;;   (opaque <boolean>)  (nongenerative [<uid>])  (parent-rtd <e> <e>)
  ;; F is any datum; anything else answers #f. This is the one statement of
  ;; the shape: the name walk asks it of a record definition before its rule
  ;; fires, and datum-names asks it before it reads a field.
  (define (record-definition-shape? f)
    (define (distinct? xs)
      (or (null? xs) (and (not (memq (car xs) (cdr xs))) (distinct? (cdr xs)))))
    (define (field-spec? s)
      (or (symbol? s)
          (and (pair? s) (list? s) (for-all symbol? s)
               (case (car s)
                 ((immutable) (<= 2 (length s) 3))
                 ((mutable) (memv (length s) '(2 4)))
                 (else #f)))))
    (define (clause? c)
      (and (pair? c) (list? c)
           (case (car c)
             ((fields) (for-all field-spec? (cdr c)))
             ((parent) (and (= (length c) 2) (symbol? (cadr c))))
             ((protocol) (= (length c) 2))
             ((sealed opaque) (and (= (length c) 2) (boolean? (cadr c))))
             ((nongenerative) (or (= (length c) 1) (and (= (length c) 2) (symbol? (cadr c)))))
             ((parent-rtd) (= (length c) 3))
             (else #f))))
    (and (list? f) (>= (length f) 2) (eq? (car f) 'define-record-type)
         (let ((spec (cadr f)) (clauses (cddr f)))
           (and (or (symbol? spec)
                    (and (list? spec) (= (length spec) 3) (for-all symbol? spec)))
                (for-all clause? clauses)
                (distinct? (map car clauses))
                (not (and (assq 'parent clauses) (assq 'parent-rtd clauses)))))))

  ;; The names a definition binds. TOTAL BY CONSTRUCTION: a stored body can
  ;; be any datum, and every shape is decided at the entry -- a define by its
  ;; head, a record definition by record-definition-shape? -- before a name
  ;; is read from it. A record definition that does not fit its shape binds
  ;; only its type's own names, when its name spec is one R6RS allows, and
  ;; otherwise none.
  (define (datum-names body)
    (define (suffix name ending) (string->symbol (string-append (symbol->string name) ending)))
    (define (type-names head)
      (cond ((symbol? head)
             (list head (string->symbol (string-append "make-" (symbol->string head))) (suffix head "?")))
            ((and (list? head) (= (length head) 3) (for-all symbol? head)) head)
            (else '())))
    (define (field-names type field)
      (let* ((name (if (symbol? field) field (cadr field)))
             (default (string->symbol (string-append (symbol->string type) "-" (symbol->string name))))
             (access (if (and (pair? field) (> (length field) 2)) (caddr field) default)))
        (if (and (pair? field) (eq? (car field) 'mutable))
            (list access (if (= (length field) 4) (cadddr field) (suffix default "-set!")))
            (list access))))
    (cond
      ((not (and (list? body) (>= (length body) 2))) '())
      ((memq (car body) '(define define-syntax))
       (let ((head (cadr body)))
         (cond ((symbol? head) (list head))
               ((and (eq? (car body) 'define) (pair? head) (symbol? (car head))) (list (car head)))
               (else '()))))
      ((eq? (car body) 'define-record-type)
       (let ((head (cadr body)))
         (if (not (record-definition-shape? body))
             (type-names head)
             (let ((type (if (symbol? head) head (car head)))
                   (fields (assq 'fields (cddr body))))
               (append (type-names head)
                       (apply append (map (lambda (field) (field-names type field))
                                          (if fields (cdr fields) '()))))))))
      (else '())))

  (define (datum-print datum) (source-datum-print datum))
  (define (char-lines text)
    (let ((n (string-length text)))
      (let loop ((start 0) (i 0) (out '()))
        (cond ((= i n) (reverse (if (< start n) (cons (list start n n) out) out)))
              ((char=? (string-ref text i) #\return)
               (let ((next (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\newline)) (+ i 2) (+ i 1))))
                 (loop next next (cons (list start i next) out))))
              ((char=? (string-ref text i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (list start (if (and (> i start) (char=? (string-ref text (- i 1)) #\return)) (- i 1) i) (+ i 1)) out)))
              (else (loop start (+ i 1) out))))))
  (define (blank? s) (for-all char-whitespace? (string->list s)))
  (define (leading-doc text start comments lines)
    (let loop ((rows lines) (previous '()))
      (cond
        ((null? rows) "")
        ((and (<= (caar rows) start) (< start (caddar rows)))
         (if (not (blank? (substring text (caar rows) start))) ""
             (let collect ((rs previous) (out '()))
               (if (null? rs) (apply string-append out)
                   (let* ((row (car rs))
                          (comment (find (lambda (c) (and (<= (car row) (car c)) (< (car c) (caddr row)))) comments)))
                     (if (and comment (blank? (substring text (car row) (car comment))))
                         (collect (cdr rs) (cons (substring text (car row) (caddr row)) out))
                         (apply string-append out)))))))
        (else (loop (cdr rows) (cons (car rows) previous))))))
  ;; Read the datum that follows a discard at offset. Copying the rest of the
  ;; text for every discard made a file of many discards quadratic, so the
  ;; datum is read from a window that doubles until the datum ends strictly
  ;; before the window's edge. A datum that ends before the edge reads the
  ;; same from the window as from the whole text: the reader has seen every
  ;; character it would see, including the delimiter after an atom. An eof,
  ;; a lexical error or an end at the edge may be the window's doing, so the
  ;; window grows; the last try is the whole rest of the text, whose answer
  ;; (datum, eof or error) is final.
  ;;
  ;; NOTE: ONLY LEXICAL VIOLATIONS ARE CAUGHT. Chez's reader raises a datum
  ;; cut short (a list, string, block comment, vector, number or character
  ;; left open) as one condition that is both a lexical violation and an
  ;; i/o read error, "unexpected end-of-file reading"; that is the
  ;; truncation, and it means grow. The filter is by condition type, not by
  ;; origin: a lexical violation raised by anything else inside the read (a
  ;; reader observer, say) also grows the window, and the last try raises it
  ;; as the whole text would. Any other condition -- the observers today
  ;; raise a plain symbol -- goes on at once. The window is a string, so no
  ;; filesystem condition can arise here.
  (define (discarded-datum text offset)
    (let ((n (string-length text)))
      (let grow ((size 64))
        (let ((edge (+ offset size)))
          (if (>= edge n)
              (source-reader-at text offset)
              (let ((a (guard (e ((lexical-violation? e) #f)) (source-reader-next (source-reader-open (substring text offset edge))))))
                (if (and a (not (eof-object? a)) (< (caddr a) size))
                    a
                    (grow (* size 2)))))))))
  (define (datum-source-read bytes . context)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) (raise e))
              (#t (raise '(error bad-source (reason reader-rejected)))))
      (let* ((ledger (source-ledger bytes)) (text (car ledger)) (comments (cadr ledger))
             (reader (source-reader-open text)) (lines (char-lines text))
             (discarded
               (map (lambda (start)
                      (let ((a (discarded-datum text (+ start 2))))
                        (when (eof-object? a) (raise '(error bad-source (reason missing-discarded-datum))))
                        (list start (+ start 2 (caddr a))))) (caddr ledger))))
        (define (inside-discard? offset)
          (exists (lambda (r) (<= (car r) offset (- (cadr r) 1))) discarded))
        (define (convert a)
          (let* ((body (car a)) (start (cadr a)) (end (caddr a))
                 (actual (filter (lambda (c) (not (inside-discard? (car c)))) comments))
                 (warnings (map (lambda (c)
                                  (let ((where (source-coordinate text (car c))))
                                    (list 'warning 'internal-comment (list 'byte-offset (car where))
                                          (list 'line (cadr where)) (list 'column (caddr where)))))
                                (filter (lambda (c) (< start (car c) end)) actual))))
            (list body (leading-doc text start actual lines) (datum-names body) start end warnings
                  (map convert (list-ref a 3)))))
        (let loop ((forms '()) (count 0))
          (when (> count 4096) (raise '(error bad-source (reason form-limit))))
          (let ((a (source-reader-next reader)))
            (if (eof-object? a)
                (if (pair? context)
                    (list (reverse forms) (filter (lambda (c) (not (inside-discard? (car c)))) comments) discarded text)
                    (reverse forms))
                (loop (cons (convert a) forms) (+ count 1))))))))
)
