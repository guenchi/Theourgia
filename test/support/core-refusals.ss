#!r6rs
(import (chezscheme))
;; Read trusted implementation forms; comments and string contents are not code.
;;
;; NOTE: THIS READS THE CONSTRUCTOR ROUTES PRESENT AT THE PINNED CORE, BY NAME,
;; AND CANNOT SHOW ITSELF COMPLETE. Each clause below is a way the core has been
;; read to build an `(error <kind> ...)` head; a route it does not know is found
;; by review, not by this census. Three reviews each found one it missed, so the
;; core is to declare its refusal kinds in one place, through one constructor
;; family, with a machine-readable table; this census is then a second reading
;; compared against that table, not the authority.
(define (literal-symbol form)
  (and (pair? form) (eq? (car form) 'quote) (pair? (cdr form))
       (null? (cddr form)) (symbol? (cadr form)) (cadr form)))
;; Every quoted symbol inside an expression, in order.
(define (quoted-symbols form)
  (cond
    ((literal-symbol form) (list (literal-symbol form)))
    ((pair? form) (append (quoted-symbols (car form)) (quoted-symbols (cdr form))))
    (else '())))
;; The functions whose `(list 'refused <kind> ...)` is adopt's answer, turned
;; into `(error <kind> ...)` by rpc.sc's `adopt` arm (`(cons 'error (cdr a))`).
;; The same list shape is used by other protocols (session, reduce, net), and
;; some of theirs are turned into answer heads by other arms too; this census
;; reads adopt's only, and the others are for the core's own table.
(define adopt-refusers
  '(verify-instance adopt-decided! adopt-preflight unreadable-refusal read-failure-refusal))
(for-each
 (lambda (file)
   (let ((found '()))
     (define (remember kind)
       (unless (memq kind found)
         (set! found (cons kind found))
         (printf "~a\t~a\n" file kind)))
     ;; NOTE: A FORM IS MATCHED ONLY WHERE IT STANDS AS A LIST, AT ITS HEAD.
     ;; The walk used to visit every pair, list tails included, so a tail
     ;; such as `(make-log-error log-error?)` inside an export list or a
     ;; binding matched as if it were a call. `in` is the name of the
     ;; innermost enclosing `(define (<name> ...) ...)`.
     (define (walk form in)
       (when (pair? form)
         (let ((in (if (and (eq? (car form) 'define) (pair? (cdr form)) (pair? (cadr form))
                            (symbol? (caadr form)))
                       (caadr form)
                       in)))
           (cond
             ;; NOTE: A TABLE OF KINDS THE CORE MAKES THROUGH A VARIABLE (f5ebd58,
             ;; answers.sc: `(define table-kinds '(absent unreadable unwritable))`,
             ;; answered as `(cons* 'error (failure-kind c) ...)`). No literal
             ;; `(error <kind> ...)` names them, so the census read none of them
             ;; until the re-pin found two reaching a save; the table itself is
             ;; read here, so a kind added to it is counted.
             ((and (eq? (car form) 'define) (pair? (cdr form)) (eq? (cadr form) 'table-kinds)
                   (pair? (cddr form)) (pair? (caddr form)) (eq? (car (caddr form)) 'quote)
                   (pair? (cdr (caddr form))) (list? (cadr (caddr form))))
              (for-each (lambda (kind) (when (symbol? kind) (remember kind))) (cadr (caddr form))))
             ;; NOTE: THE SYMBOLS FILE'S REFUSALS ARE MADE THROUGH A NAMED
             ;; CONSTRUCTOR (code-suggest.sc: `(symbols-refusal 'symbols-stale ...)`,
             ;; which raises `(error <kind> ...)` from a variable). No literal
             ;; `(error <kind> ...)` names them, so the constructor's calls are
             ;; read; the name is this one constructor's, not a pattern.
             ((and (eq? (car form) 'symbols-refusal) (pair? (cdr form)) (literal-symbol (cadr form)))
              (remember (literal-symbol (cadr form))))
             ;; NOTE: THE REQUEST LEDGER'S VERDICTS ARE CONSED (store.sc,
             ;; `request-answer`: `(cons 'error (cons 'incomplete-request ...))`).
             ((and (eq? (car form) 'cons) (pair? (cdr form)) (eq? (literal-symbol (cadr form)) 'error)
                   (pair? (cddr form)) (pair? (caddr form)) (eq? (car (caddr form)) 'cons)
                   (pair? (cdr (caddr form))) (literal-symbol (cadr (caddr form))))
              (remember (literal-symbol (cadr (caddr form)))))
             ;; NOTE: A LOG CONDITION RAISED IS AN ERROR HEAD (rpc.sc,
             ;; `describe-log-error`). A `make-log-error` that is the argument of
             ;; `raise` is printed as a `#raised` row as well, so that a kind
             ;; excused as "returned, never raised" can be checked against it.
             ((and (eq? (car form) 'raise) (pair? (cdr form)) (pair? (cadr form))
                   (eq? (car (cadr form)) 'make-log-error) (pair? (cdr (cadr form)))
                   (literal-symbol (cadr (cadr form))))
              (printf "#raised\t~a\t~a\n" (literal-symbol (cadr (cadr form))) file))
             ;; NOTE: `make-log-error` is read, and so are `validate`'s `note!` and
             ;; `cut!` (log.sc), which make one from the kind they are given, when
             ;; that kind is a literal. A `make-log-error` whose kind is not a
             ;; literal is printed as a `#variable` row: the census cannot read its
             ;; kind, and the cell that counts those rows notices a new one.
             ;; `note!` and `cut!` print no such row: ffi.sc has an unrelated
             ;; `note!` that is never given a kind, and validate's own
             ;; non-literal calls pass on a kind read elsewhere.
             ((and (memq (car form) '(note! cut!)) (pair? (cdr form)) (literal-symbol (cadr form)))
              (remember (literal-symbol (cadr form))))
             ((and (eq? (car form) 'make-log-error) (pair? (cdr form)))
              (if (literal-symbol (cadr form))
                  (remember (literal-symbol (cadr form)))
                  (printf "#variable\t~a\t~a\n" (car form) file)))
             ;; NOTE: ADOPT'S REFUSALS (see `adopt-refusers`). A kind that is not a
             ;; literal is printed as a `#variable` row, as `make-log-error`'s is.
             ((and (eq? (car form) 'list) (pair? (cdr form)) (eq? (literal-symbol (cadr form)) 'refused)
                   (pair? (cddr form)) (memq in adopt-refusers))
              (if (literal-symbol (caddr form))
                  (remember (literal-symbol (caddr form)))
                  (printf "#variable\tlist-refused\t~a\n" file)))
             ;; NOTE: `unreadable-refusal` IS GIVEN ITS KIND BY ITS CALLER
             ;; (log.sc, `adopt-preflight`: `(if ... 'registry-unreadable
             ;; 'metadata-unreadable)`), so the quoted symbols of that argument are
             ;; read.
             ((and (eq? (car form) 'unreadable-refusal) (pair? (cdr form)))
              (for-each remember (quoted-symbols (cadr form))))
             ((and (eq? (car form) 'list) (pair? (cdr form)) (pair? (cddr form))
                   (eq? (literal-symbol (cadr form)) 'error) (literal-symbol (caddr form)))
              (remember (literal-symbol (caddr form))))
             ;; NOTE: A QUASIQUOTED HEAD IS READ AS A QUOTED ONE (daemon.sc:
             ;; `` `(error serve-busy (path ,socket)) ``).
             ((and (memq (car form) '(quote quasiquote)) (pair? (cdr form))
                   (pair? (cadr form)) (eq? (caadr form) 'error)
                   (pair? (cdadr form)) (symbol? (cadadr form)))
              (remember (cadadr form))))
           (let loop ((xs form))
             (when (pair? xs)
               (walk (car xs) in)
               (loop (cdr xs)))))))
     (call-with-input-file file
       (lambda (port)
         (let loop ((form (read port)))
           (unless (eof-object? form) (walk form #f) (loop (read port))))))))
 (cdr (command-line)))
