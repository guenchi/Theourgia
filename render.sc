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
(library (theourgia render)
  (export render-wire render-human answer-printing!)
  (import (chezscheme))

  ;; KEY: ONE PRINTER FOR EVERY ANSWER THIS SYSTEM GIVES. There used to be
  ;; three: this one, a copy of it in `mcp/server.sc`, and a bare `write`
  ;; in `daemon.sc` that set nothing at all -- so the same answer could
  ;; leave by three routes in two spellings, and the daemon's was already
  ;; the odd one out.
  ;;
  ;; KEY: NON-ASCII GOES OUT AS UTF-8, NOT AS `\x6C49;`. Measured by the
  ;; worker: escaping made CJK cost 2.36 times the tokens, where the JSON
  ;; envelope around it costs 1.16. The reader of these answers is an
  ;; agent paying by the token.
  ;;
  ;; NEVER: SET ONCE PER PROCESS, NOT `parameterize`d AROUND THE WRITE. Chez
  ;; parameters are per OS THREAD, and every green thread on that thread
  ;; shares them (7.6.36, measured) -- so a printer that set them for the
  ;; duration of its own call would be setting them for whatever else was
  ;; running. Each entry point calls this once, before it answers
  ;; anything.
  ;;
  ;; NEVER: THIS DOES NOT TOUCH WHAT IS WRITTEN TO DISK. A record is
  ;; serialised by `sexpr->string-extended` in `wire.sc`, which writes
  ;; its own characters and has never consulted these parameters. `U4`
  ;; compares the bytes of a stored record before and after, and it is a
  ;; guard rather than a formality: the two paths are separate because
  ;; somebody kept them separate, and nothing but a reading says so.
  (define (answer-printing!)
    (print-graph #f)
    (print-length #f)
    (print-level #f)
    (print-radix 10)
    (print-unicode #t)
    (print-gensym #f))

  (define (render-wire value)
    (call-with-string-output-port (lambda (p) (write (reader-form value) p) (newline p))))

  ;; A TEXT-MODE CODE BLOCK'S `src` AND `doc` ARE PRINTED AS TEXT WHEN THEY
  ;; ARE TEXT. The store keeps them as bytes, and a block record printed with
  ;; them as `#vu8(...)` cost three to four times the text's bytes in every
  ;; answer carrying it -- read, context's (block ...) entries, the daemon's,
  ;; the MCP shell's. Here, where every answer is printed, a record whose
  ;; fields say `(kind . code)` and `(mode . text)` has each of the two given
  ;; as a string when its bytes are exactly that string's encoding; other
  ;; bytes stay bytes. ONLY THE PRINTED ANSWER: the answer as a value is
  ;; unchanged, so a caller inside the process -- a scope copying blocks, an
  ;; export -- still has the bytes as stored.
  ;;
  ;; EXACTLY: the string encodes back to the same bytes. `utf8->string` drops
  ;; one leading byte-order mark, so a string that comes back three bytes
  ;; short is tried again with U+FEFF in front; whichever encodes back to the
  ;; bytes is the text, and none is no text.
  (define (exact-text bv)
    (let* ((t (utf8->string bv))
           (marked (string-append (string #\xFEFF) t)))
      (cond ((bytevector=? (string->utf8 t) bv) t)
            ((bytevector=? (string->utf8 marked) bv) marked)
            (else #f))))
  ;; X is a proper list here. A BLOCK RECORD IS KNOWN BY ITS HEAD, as
  ;; state-read builds it: (id . "<id>"), (deleted . #t/#f), then
  ;; (fields ...); -> that fields element, else #f.
  (define (record-fields x)
    (and (>= (length x) 3)
         (pair? (car x)) (eq? (caar x) 'id) (string? (cdar x))
         (pair? (cadr x)) (eq? (caadr x) 'deleted) (boolean? (cdadr x))
         (let ((e (caddr x)))
           (and (pair? e) (eq? (car e) 'fields) (list? (cdr e)) (for-all pair? (cdr e)) e))))
  (define (text-mode? e)
    (and (equal? (assq 'kind (cdr e)) '(kind . code))
         (equal? (assq 'mode (cdr e)) '(mode . text))))
  ;; ONCE OVER THE ANSWER: a proper list is asked once whether it is a record
  ;; and otherwise mapped, so an answer of n items costs n, not n squared as
  ;; walking every tail would. NOTHING STORED IS ENTERED: a block record is
  ;; given back whole (a text-mode one with its src and doc as text), and a
  ;; query's (row ...) is a row of stored values, so a field holding a value
  ;; shaped like a record is printed as it is stored.
  (define (reader-form x)
    (cond
      ((not (pair? x)) x)
      ((not (list? x))
       (let walk ((p x))
         (if (pair? p) (cons (reader-form (car p)) (walk (cdr p))) (reader-form p))))
      ((eq? (car x) 'row) x)
      ((record-fields x)
       => (lambda (e)
            (if (not (text-mode? e))
                x
                (map (lambda (y)
                       (if (eq? y e)
                           (cons 'fields
                                 (map (lambda (f)
                                        (if (and (memq (car f) '(src doc)) (bytevector? (cdr f)))
                                            (let ((t (exact-text (cdr f)))) (if t (cons (car f) t) f))
                                            f))
                                      (cdr e)))
                           y))
                     x))))
      (else (map reader-form x))))
  ;; THE INCOMPLETE CLAUSE IS PRINTED EVEN WHERE THE REST OF THE ANSWER IS
  ;; UNWRAPPED. A text or items answer is shown as its body alone, and that
  ;; used to drop every clause after the body; the one saying the answer
  ;; is missing a writer's records is the one a reader must not lose (K10).
  ;; So are `stale` and `via` after a TEXT body: an answer built from facts
  ;; an editor supplied says how many it could not use and where the others
  ;; came from. NOT after ITEMS: a reader of an items answer takes every line
  ;; as an item (the VS Code plugin parses a search's lines and rejects the
  ;; whole answer at the first line that is not a hit), so there the two
  ;; travel only in --wire.
  (define kept-clauses '(incomplete via stale))
  (define kept-after-items '(incomplete))
  ;; NEVER: EVERY HIT LEFT OUT IS NOT NO HIT. A search, grep or whereis
  ;; whose every hit was in a superseded or refuted block printed nothing,
  ;; as one that matched nothing does. With no item left, the excluded
  ;; clause is printed, so the two differ. With an item left it is not:
  ;; then every line is a hit, as the reader above needs.
  (define kept-after-no-items '(excluded))
  ;; A QUERY'S ROW IS ITS VALUES, separated by a space, one row a line; every
  ;; other item is printed whole.
  (define (render-item item)
    (if (and (pair? item) (eq? (car item) 'row) (list? item))
        (call-with-string-output-port
          (lambda (p)
            (let loop ((vs (cdr item)) (first #t))
              (unless (null? vs)
                (unless first (put-char p #\space))
                (write (car vs) p)
                (loop (cdr vs) #f)))
            (newline p)))
        (render-wire item)))
  ;; AN ANSWER THAT CARRIES A REVISION IS PRINTED WHOLE, as --wire prints it:
  ;; only a program asks a read for its rev, and unwrapping the body would
  ;; drop the clause it asked for.
  (define (carries-rev? answer)
    (and (pair? answer) (list? answer)
         (exists (lambda (x) (and (pair? x) (eq? (car x) 'rev))) (cdr answer))))
  ;; A CONTEXT ANSWER IS PRINTED BY SECTION, for a person: each section's
  ;; name, then one line per entry, its id, level and title; the notes one
  ;; per line; the insufficient, excluded and budget clauses as the wire
  ;; writes them; then THE RECEIPT ON ONE LINE, the clause exactly as the wire
  ;; writes it, so a person who reads the human form can hand it to commit
  ;; --premises as it stands. The cut and the versions are for a program,
  ;; which asks for the wire form. A kept clause (incomplete) follows the
  ;; receipt, as it does on the wire.
  (define context-sections '(for constraints evidence to-verify counterexamples background notes))
  (define context-tail '(insufficient excluded budget))
  (define (context-answer? answer)
    (and (pair? answer) (list? answer) (eq? (car answer) 'ok) (pair? (cdr answer))
         (pair? (cadr answer)) (eq? (car (cadr answer)) 'for)))
  (define (context-entry-line e)
    (let ((part (lambda (name)
                  (let ((c (and (list? e) (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr e)))))
                    (and c (pair? (cdr c)) (cadr c))))))
      (call-with-string-output-port
        (lambda (p)
          (put-string p "  ")
          (display (car e) p)
          (put-char p #\space)
          (display (or (part 'level) "") p)
          (put-char p #\space)
          (display (or (part 'title) "") p)
          (newline p)))))
  (define (render-context answer kept)
    (apply string-append
           (append
             (map (lambda (c)
                    (cond
                      ((not (and (pair? c) (pair? (cdr c)))) "")
                      ((eq? (car c) 'for)
                       (string-append "for\n" (context-entry-line (cadr c))))
                      ((eq? (car c) 'notes)
                       (apply string-append "notes\n"
                              (map (lambda (n) (string-append "  " (render-wire n))) (cadr c))))
                      ((memq (car c) context-sections)
                       (apply string-append (symbol->string (car c)) "\n"
                              (map context-entry-line (cadr c))))
                      ((memq (car c) context-tail) (render-wire c))
                      (else "")))
                  (cdr answer))
             (map render-wire (filter (lambda (c) (and (pair? c) (eq? (car c) 'receipt))) (cdr answer)))
             (list kept))))
  (define (render-human answer)
    (let* ((clauses (if (and (pair? answer) (list? answer))
                        (filter (lambda (x) (and (pair? x) (memq (car x) kept-clauses))) (cdr answer))
                        '()))
           (tail (apply string-append (map render-wire clauses)))
           (items-tail
             (lambda (items)
               (apply string-append
                      (map render-wire
                           (filter (lambda (x)
                                     (and (pair? x)
                                          (or (memq (car x) kept-after-items)
                                              (and (null? items) (memq (car x) kept-after-no-items)))))
                                   (if (list? answer) (cdr answer) '())))))))
      (cond
        ((carries-rev? answer) (render-wire answer))
        ((context-answer? answer) (render-context answer tail))
        ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
              (eq? (caadr answer) 'text))
         (let ((text (cadadr answer)))
           (cond ((null? clauses) text)
                 ((or (= 0 (string-length text))
                      (char=? #\newline (string-ref text (- (string-length text) 1))))
                  (string-append text tail))
                 (else (string-append text "\n" tail)))))
        ((and (pair? answer) (eq? (car answer) 'ok) (pair? (cdr answer)) (pair? (cadr answer))
              (eq? (caadr answer) 'items))
         (string-append (apply string-append (map render-item (cdadr answer))) (items-tail (cdadr answer))))
        (else (render-wire answer)))))
)
