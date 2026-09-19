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
  (export datum-source-read datum-names datum-print)
  (import (rnrs) (theourgia source-lex) (theourgia text-code)
          (only (theourgia ffi) source-reader-open source-reader-next source-reader-at source-datum-print))

  (define (datum-names body)
    (define (suffix name ending) (string->symbol (string-append (symbol->string name) ending)))
    (cond
      ((not (and (list? body) (>= (length body) 2))) '())
      ((memq (car body) '(define define-syntax))
       (let ((head (cadr body)))
         (cond ((symbol? head) (list head))
               ((and (eq? (car body) 'define) (pair? head) (symbol? (car head))) (list (car head)))
               (else '()))))
      ((eq? (car body) 'define-record-type)
       (let* ((head (cadr body))
              (type (cond ((symbol? head) head) ((and (list? head) (= (length head) 3) (symbol? (car head))) (car head)) (else #f)))
              (base (cond ((not type) '()) ((symbol? head) (list type (string->symbol (string-append "make-" (symbol->string type))) (suffix type "?")))
                          (else (filter symbol? head))))
              (fields (find (lambda (x) (and (pair? x) (eq? (car x) 'fields))) (cddr body))))
         (if (not type) '()
             (append base
               (apply append
                 (map (lambda (field)
                        (let* ((name (if (symbol? field) field (and (list? field) (> (length field) 1) (cadr field))))
                               (mutable? (and (pair? field) (eq? (car field) 'mutable)))
                               (default (and (symbol? name) (string->symbol (string-append (symbol->string type) "-" (symbol->string name)))))
                               (access (if (and (list? field) (> (length field) 2)) (caddr field) default))
                               (mutator (and mutable? (if (> (length field) 3) (cadddr field) (and default (suffix default "-set!"))))))
                          (filter symbol? (if mutable? (list access mutator) (list access)))))
                      (if fields (cdr fields) '())))))))
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
  (define (datum-source-read bytes . context)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) (raise e))
              (#t (raise '(error bad-source (reason reader-rejected)))))
      (let* ((ledger (source-ledger bytes)) (text (car ledger)) (comments (cadr ledger))
             (reader (source-reader-open text)) (lines (char-lines text))
             (discarded
               (map (lambda (start)
                      (let ((a (source-reader-at text (+ start 2))))
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
