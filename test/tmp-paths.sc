;; Copyright 2018 - 2026 guenchi
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

;; NO FIXTURE WRITES INTO /tmp BY NAME (F71).
;;
;; The runner makes two directories for a run and removes both at its end:
;; THEOURGIA_TEST_ROOT for files and directories, THEOURGIA_TEST_SOCK for
;; sockets. A fixture that names a path under /tmp instead escapes that
;; removal -- /tmp held eight fixture prefixes with 130 to 214 leftovers
;; each -- and can meet an old run's directory with the same process id,
;; which is how two fixtures went red in a gate. So every string literal
;; in a file the suite runs or loads is read, and one that leads into /tmp
;; is allowed only as the fallback a fixture uses when it runs alone:
;;
;;   (or (getenv "THEOURGIA_TEST_ROOT") "/tmp...")          -- or _SOCK
;;   (let ((v (getenv "THEOURGIA_TEST_ROOT")))
;;     (if <test> v "/tmp..."))
;;
;; -- read as code: the v in scope at the if is the one the let binds, by
;; the scoping rules of the binding forms around it; the shape inside a
;; quote is data; and a getenv, or, if, let or let* the file or a form
;; around the literal rebinds gets no exemption --
;; or by an entry in the allow-list below, each with its reason, all
;; printed. The rule names no fixture, so a new one is covered without
;; being named.
;;
;; WHAT LEADS INTO /tmp: a literal equal to "/tmp", or holding "/tmp/" at
;; its start or right after a character that cannot continue a path name
;; (space, quote, =, parenthesis, colon) -- so "rm -rf /tmp/dmn-*" is read
;; and "/tmp-replacement", a file name, is not.
;;
;; READ AS WHAT: .sc and .ss files with Chez's own reader, so a comment is
;; not a literal and a string is exactly the string; .py files with
;; Python's tokenize, for the same reason, with the syntax tree giving
;; every string constant inside an f-string, its expressions included,
;; and the exact extent of each documentation string -- the first
;; statement of a module, class or function -- measured in the text as
;; decoded from the file's own encoding, which is left out as prose and
;; named in the output when it names /tmp; and the shell files the suite runs or sources as text,
;; whole-line comments left out, a word that is /tmp itself included, and
;; launch.pl the same way; a file of any other kind is named as not read. A
;; file that cannot be opened or read is a red row, not a skip, and a
;; directory the walk does not enter (__pycache__, a symbolic link) is
;; named.
;;
;; STATED LIMITS: a path spelled "/private/tmp/..." or built from pieces
;; ("/" "tmp") is not seen; nor is one made at run time -- a format string
;; ("~a/tmp/..."), text read from a port or a file, a symbol turned into a
;; string -- since the rule is about string literals; a getenv, or, if,
;; let or let* given another meaning by an import is not seen; the
;; reading of scope is known to be incomplete in both directions -- a
;; define spliced in by begin, a let-syntax, a macro template, are not
;; seen, and a let-values or do initialiser, an internal define, a
;; variable named quote, are read too strictly -- and is to be replaced
;; by F88, one helper file as the only place that names /tmp; the shell
;; files are read as text, not parsed; the shell files the suite never
;; runs are not read, and are named; and this file is not read, because
;; its specimens and its patterns name /tmp on purpose.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))

(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

;; ---- the pattern ------------------------------------------------------

(define (suffix? s suffix)
  (let ((n (string-length s)) (k (string-length suffix)))
    (and (>= n k) (string=? suffix (substring s (- n k) n)))))
(define (prefix? s p)
  (and (>= (string-length s) (string-length p)) (string=? p (substring s 0 (string-length p)))))

;; AMENDMENT 5 (2026-09-23): `{`, `[` and `<` are boundaries too, for every
;; reader. None of them continues a path name in any source here, and Perl's
;; q{/tmp/...} otherwise escaped the text reading (codex r7 C3).
(define boundary-chars '(#\space #\" #\' #\= #\( #\) #\: #\tab #\newline #\{ #\[ #\<))
(define (leads-into-tmp? s)
  (or (string=? s "/tmp")
      (prefix? s "/tmp/")
      (let loop ((i 1))
        (cond ((> (+ i 5) (string-length s)) #f)
              ((and (string=? "/tmp/" (substring s i (+ i 5)))
                    (memv (string-ref s (- i 1)) boundary-chars))
               #t)
              (else (loop (+ i 1)))))))

;; ---- the files --------------------------------------------------------

;; EVERY FILE UNDER test/, AT ANY DEPTH, and the directories not entered:
;; __pycache__ holds what Python compiled, not source, and a symbolic link
;; to a directory is not followed. Each one skipped is returned, to be
;; named as not read. -> (files skipped), skipped as (path reason).
(define (files-under dir)
  (let walk ((rel "") (acc (list '() '())))
    (let ((path (if (string=? rel "") dir (string-append dir "/" rel))))
      (fold-left
        (lambda (acc name)
          (let* ((r (if (string=? rel "") name (string-append rel "/" name)))
                 (p (string-append dir "/" r)))
            (cond ((member name '("." "..")) acc)
                  ((and (string=? name "__pycache__") (file-directory? p))
                   (list (car acc) (cons (list r "compiled python, not source; not entered") (cadr acc))))
                  ((and (file-symbolic-link? p) (file-directory? p))
                   (list (car acc) (cons (list r "a symbolic link to a directory; not followed") (cadr acc))))
                  ((file-directory? p) (walk r acc))
                  (else (list (cons r (car acc)) (cadr acc))))))
        acc
        (directory-list path)))))

;; THE SHELL FILES THE SUITE RUNS OR SOURCES. The runner, the check it
;; runs, and the environment every caller sources; any other shell file
;; here is a tool the suite never runs, and it is named as not read.
(define suite-shell-files '("run-fixtures.sh" "row-baseline-check.sh" "env.sh"))
;; AND THE SUITE'S OTHER PROGRAMS THAT ARE NOT SCHEME, PYTHON OR SHELL, read
;; the same way, as text with whole-line comments left out: launch.pl, which
;; starts every fixture. A file of any other kind under test/ is named as
;; not read, never skipped in silence.
(define suite-text-files '("launch.pl"))
(define self-name "tmp-paths.sc")

;; ---- the allow-list ----------------------------------------------------
;;
;; (file literal-or-line-text reason). A Scheme or Python literal is
;; matched whole; a shell line is matched when it contains the text. An
;; entry that matches nothing is a red row: an allowance for something
;; that is gone is an allowance for whatever takes its place.
(define allow-list
  '(("daemon-e1.sc" "(request 1 \"/tmp/some-other-store\" \"tester\" #f wire #f #f outline)\n"
     "test data: a request naming another store, which the daemon must refuse; no such path is made")
    ("runner-self.sc" "/tmp"
     "the cells list /tmp to read what a run left there")
    ("runner-self.sc" "/tmp/ths."
     "the cells read that the socket root is where the runner makes it")
    ("runner-self.sc" "/tmp/run-"
     "the cells read that a run with no caller base makes its scratch root under /tmp")
    ("eval-local.sc" "(open-output-file \"/tmp/eval-should-not-write\")"
     "test data: source an evaluation must be refused to run; nothing is written")
    ("runner-self.sc" "RS-6 the scratch root is <base>/run-<token> and the socket root /tmp/ths.<token>, at most 20 bytes, each marked by the probe"
     "a row label naming where the runner makes its roots")
    ("runner-self.sc" "RS-6 with THEOURGIA_TEST_ROOT unset the scratch root is /tmp/run-<token>, and it is gone afterwards"
     "a row label naming where the runner makes its roots")
    ("runner-self.sc" "GUARD RS-4 after a run refused on its library path, the base is empty and no new /tmp/ths.* remains"
     "a row label naming where the runner makes its roots")
    ("runner-self.sc" "(when (and (> (string-length d) 9) (string=? (substring d 0 9) \"/tmp/ths.\"))\n"
     "RS-11c's fixture checks it was handed a socket root under /tmp/ths. before locking it; it writes nothing there")
    ("paths.py" "/tmp"
     "solo fallback after both variables, for a socket-capable directory")
    ("run-fixtures.sh" "mktemp -d /tmp/ths.XXXXXX"
     "the runner makes the socket root")))

;; ---- reading one Scheme file -------------------------------------------

(define (line-of text pos)
  (let loop ((i 0) (n 1))
    (cond ((>= i pos) n)
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))
(define (unwrap x) (if (annotation? x) (annotation-expression x) x))
(define test-variables '("THEOURGIA_TEST_ROOT" "THEOURGIA_TEST_SOCK"))
(define (getenv-of-test-variable? x)
  (let ((d (and x (annotation-stripped* x))))
    (and (list? d) (= (length d) 2) (eq? (car d) 'getenv) (member (cadr d) test-variables) #t)))
(define (annotation-stripped* x) (if (annotation? x) (annotation-stripped x) x))

;; IS THIS LITERAL A FALLBACK? `path` runs from the literal outward: the
;; literal, its parent, and so on to the top-level form. Shape one: the
;; parent is (or (getenv V) literal), the literal the second operand.
;; Shape two: the parent is (if test x literal), the literal the
;; else-operand, and the binding of x that is in scope there -- the
;; innermost one, by the scoping rules of the forms that bind -- is a let
;; or let* binding whose initialiser is (getenv V).
;;
;; NEVER: A SPELLING IS NOT A MEANING. The shape is read only where it is
;; code: inside quote, quasiquote, syntax or quasisyntax it is data and
;; nothing is a fallback. And getenv, or, if, let and let* must mean what
;; they say: a file that defines one of them at top level, or a binding
;; form around the literal that binds one of them, gets no exemption.
;; Anything this reading cannot place counts as a violation, not as a
;; fallback.
(define exemption-names '(getenv or if let let*))
(define (stripped-list a)
  (let ((d (annotation-stripped* a))) (and (list? d) d)))
(define (formals-names f)
  (let loop ((f f) (out '()))
    (cond ((symbol? f) (cons f out))
          ((pair? f) (loop (cdr f) (if (symbol? (car f)) (cons (car f) out) out)))
          (else out))))
;; The names a body defines with define or define-syntax, which are in
;; scope for everything in that body.
(define (defined-names forms)
  (fold-left
    (lambda (out f)
      (if (and (pair? f) (memq (car f) '(define define-syntax)) (pair? (cdr f)))
          (let ((t (cadr f)))
            (cond ((symbol? t) (cons t out))
                  ((and (pair? t) (symbol? (car t))) (cons (car t) out))
                  (else out)))
          out))
    '()
    forms))
(define (index-of x l)
  (let loop ((l l) (i 0))
    (cond ((null? l) #f) ((eq? (car l) x) i) (else (loop (cdr l) (+ i 1))))))
(define (binding-pairs raw-bindings)
  (map (lambda (b) (unwrap-list b)) (unwrap-list raw-bindings)))
(define (pair-binds? bl name) (and (pair? bl) (eq? (unwrap (car bl)) name)))
(define (answer-for bl)
  (if (and (= (length bl) 2) (getenv-of-test-variable? (cadr bl))) 'test 'other))
;; What ancestor `a` binds `name` to, as seen from its child `c` (and
;; grandchild `g`, for let*): none, test (a let or let* binding to
;; (getenv V)), or other.
(define (binding-of a c g name)
  (let ((d (stripped-list a)) (raw (unwrap-list (unwrap a))))
    (cond
      ((not (and d (pair? d))) 'none)
      ((memq name (defined-names (cdr d))) 'other)
      ((and (eq? (car d) 'let) (>= (length raw) 3) (symbol? (cadr d)))
       (cond ((eq? c (caddr raw)) 'none)
             ((eq? name (cadr d)) 'other)
             (else (let ((hit (find (lambda (bl) (pair-binds? bl name)) (binding-pairs (caddr raw)))))
                     (if hit (answer-for hit) 'none)))))
      ((and (eq? (car d) 'let) (>= (length raw) 2))
       (if (eq? c (cadr raw))
           'none
           (let ((hit (find (lambda (bl) (pair-binds? bl name)) (binding-pairs (cadr raw)))))
             (if hit (answer-for hit) 'none))))
      ((and (eq? (car d) 'let*) (>= (length raw) 2))
       (let* ((bs (unwrap-list (cadr raw)))
              (visible (if (eq? c (cadr raw))
                           (let ((j (index-of g bs))) (if j (list-head bs j) '()))
                           bs))
              (hit (find (lambda (bl) (pair-binds? bl name))
                         (reverse (map unwrap-list visible)))))
         (if hit (answer-for hit) 'none)))
      ((and (memq (car d) '(letrec letrec*)) (>= (length d) 2) (list? (cadr d)))
       (if (exists (lambda (b) (and (pair? b) (eq? (car b) name))) (cadr d)) 'other 'none))
      ((and (memq (car d) '(let-values let*-values)) (>= (length d) 2) (list? (cadr d)))
       (if (exists (lambda (b) (and (pair? b) (memq name (formals-names (car b))))) (cadr d)) 'other 'none))
      ((and (eq? (car d) 'do) (>= (length d) 2) (list? (cadr d)))
       (if (exists (lambda (b) (and (pair? b) (eq? (car b) name))) (cadr d)) 'other 'none))
      ((and (eq? (car d) 'lambda) (>= (length raw) 2))
       (if (and (not (eq? c (cadr raw))) (memq name (formals-names (cadr d)))) 'other 'none))
      ((eq? (car d) 'case-lambda)
       (let ((clause (and c (stripped-list c))))
         (if (and clause (pair? clause) (memq name (formals-names (car clause)))) 'other 'none)))
      ((and (eq? (car d) 'define) (>= (length raw) 2) (pair? (cadr d)))
       (if (and (not (eq? c (cadr raw))) (memq name (formals-names (cdr (cadr d))))) 'other 'none))
      (else 'none))))
;; The binding of `name` in scope at path element `k`, looking outward:
;; none, test or other; `file-defs` are the names the file defines at
;; top level.
(define (binding-at path k name file-defs)
  (let loop ((i (+ k 1)))
    (if (>= i (vector-length path))
        (if (memq name file-defs) 'other 'none)
        (let ((r (binding-of (vector-ref path i) (vector-ref path (- i 1))
                             (and (>= i 2) (vector-ref path (- i 2))) name)))
          (if (eq? r 'none) (loop (+ i 1)) r)))))
(define (quoted-context? ancestors)
  (exists (lambda (a)
            (let ((d (annotation-stripped* a)))
              (and (pair? d) (memq (car d) '(quote quasiquote syntax quasisyntax)))))
          ancestors))
(define (fallback? lit ancestors file-defs)
  (and (pair? ancestors)
       (not (quoted-context? ancestors))
       (let ((path (list->vector (cons lit ancestors))))
         (and (for-all (lambda (n) (eq? (binding-at path 0 n file-defs) 'none)) exemption-names)
              (let ((p (annotation-stripped* (car ancestors))))
                (or
                  (and (list? p) (= (length p) 3) (eq? (car p) 'or)
                       (let ((raw (unwrap-list (unwrap (car ancestors)))))
                         (and (eq? (caddr raw) lit)
                              (getenv-of-test-variable? (cadr raw)))))
                  (and (list? p) (= (length p) 4) (eq? (car p) 'if) (symbol? (caddr p))
                       (let ((raw (unwrap-list (unwrap (car ancestors)))))
                         (and (eq? (cadddr raw) lit)
                              (eq? (binding-at path 1 (caddr p) file-defs) 'test))))))))))
(define (unwrap-list x)
  (let loop ((l (unwrap x)) (out '()))
    (let ((l (unwrap l)))
      (if (pair? l) (loop (cdr l) (cons (car l) out)) (reverse out)))))

;; -> (values literals unreadable): literals are (file line text fallback?)
(define (scheme-literals rel text)
  (guard (e (#t (values '() (list rel (if (and (condition? e) (message-condition? e))
                                           (condition-message e) 'unreadable)))))
    (let* ((sfd (make-source-file-descriptor rel (open-bytevector-input-port (string->utf8 text))))
           (p (open-string-input-port text))
           (forms (let loop ((bfp 0) (acc '()))
                    (let-values (((a nb) (get-datum/annotations p sfd bfp)))
                      (if (eof-object? a) (reverse acc) (loop nb (cons a acc))))))
           (file-defs (defined-names (map annotation-stripped* forms)))
           (out '()))
      (for-each
        (lambda (a)
            (let walk ((x a) (ancestors '()))
              (let ((d (unwrap x)))
                (cond
                  ((string? d)
                   (when (leads-into-tmp? d)
                     (set! out (cons (list rel
                                           (if (annotation? x)
                                               (line-of text (source-object-bfp (annotation-source x)))
                                               0)
                                           d
                                           (fallback? x ancestors file-defs))
                                     out))))
                  ((pair? d)
                   (let lp ((l d))
                     (let ((l (unwrap l)))
                       (cond ((pair? l) (walk (car l) (cons x ancestors)) (lp (cdr l)))
                             ((null? l) (void))
                             (else (walk l (cons x ancestors)))))))
                  ((vector? d) (vector-for-each (lambda (e) (walk e (cons x ancestors))) d))
                  ((box? d) (walk (unbox d) (cons x ancestors)))
                  (else (void))))))
        forms)
      (values (reverse out) #f))))

;; ---- reading Python files, with Python's tokenize ------------------------
;;
;; One python3 process reads every file named on its command line and
;; prints, per string token, `file<TAB>line<TAB>value` with the value
;; written as a Scheme string; an unreadable file prints `UNREADABLE`.
(define python-program
  "import sys, tokenize, ast, io, re
def q(s):
    s = s.encode('utf-8', 'backslashreplace').decode('utf-8')
    return '\"' + s.replace('\\\\', '\\\\\\\\').replace('\"', '\\\\\"').replace('\\n', '\\\\n').replace('\\t', '\\\\t').replace('\\r', '\\\\r').replace('\\x85', '\\\\x85;').replace('\\u2028', '\\\\x2028;').replace('\\u2029', '\\\\x2029;') + '\"'
for path in sys.argv[1:]:
    try:
        src = open(path, 'rb').read()
        tree = ast.parse(src)
        enc = tokenize.detect_encoding(io.BytesIO(src).readline)[0]
        text = src.decode(enc)
        lines = re.split('\\r\\n|\\r|\\n', text)
        def at(line, byte_col):
            return (line, len(lines[line - 1].encode('utf-8')[:byte_col].decode('utf-8', 'replace')))
        spans = []
        for n in ast.walk(tree):
            if isinstance(n, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)) and n.body:
                b = n.body[0]
                if isinstance(b, ast.Expr) and isinstance(b.value, ast.Constant) and isinstance(b.value.value, str):
                    v = b.value
                    spans.append((at(v.lineno, v.col_offset), at(v.end_lineno, v.end_col_offset)))
        seen = set()
        jspans = []
        for n in ast.walk(tree):
            if isinstance(n, ast.JoinedStr):
                jspans.append((at(n.lineno, n.col_offset), at(n.end_lineno, n.end_col_offset)))
                for m in ast.walk(n):
                    if isinstance(m, ast.Constant) and isinstance(m.value, (str, bytes)) and id(m) not in seen:
                        seen.add(id(m))
                        v = m.value.decode('latin-1') if isinstance(m.value, bytes) else m.value
                        print(path + '\\t' + str(m.lineno) + '\\t' + q(v))
        depth = 0
        fstart = getattr(tokenize, 'FSTRING_START', None)
        fend = getattr(tokenize, 'FSTRING_END', None)
        for t in tokenize.generate_tokens(io.StringIO(text, newline=None).readline):
            if fstart is not None and t.type == fstart:
                depth += 1
                continue
            if fend is not None and t.type == fend:
                depth -= 1
                continue
            if t.type != tokenize.STRING or depth > 0:
                continue
            if any(a <= t.start < e for a, e in jspans):
                continue
            prefix = t.string[:len(t.string) - len(t.string.lstrip('rRbBuUfF'))]
            if 'f' in prefix.lower():
                continue
            try: v = ast.literal_eval(t.string)
            except Exception: v = t.string
            if isinstance(v, bytes): v = v.decode('latin-1')
            if any(a <= t.start < e for a, e in spans):
                print('DOC\\t' + path + '\\t' + str(t.start[0]) + '\\t' + q(str(v)))
            else:
                print(path + '\\t' + str(t.start[0]) + '\\t' + q(str(v)))
    except Exception as e:
        print(path + '\\tUNREADABLE\\t' + q(str(e)))
")
(define (split-tabs line)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length line)) (reverse (cons (substring line start i) out)))
          ((char=? (string-ref line i) #\tab) (loop (+ i 1) (+ i 1) (cons (substring line start i) out)))
          (else (loop (+ i 1) start out)))))
(define (python-literals dir rels)
  (if (null? rels)
      (values '() '() '())
      (let ((prog (string-append scratch-base "/tmp-paths-" (number->string (get-process-id)) ".py"))
            (out (string-append scratch-base "/tmp-paths-" (number->string (get-process-id)) ".out")))
        (call-with-output-file prog (lambda (p) (display python-program p)) 'replace)
        (system (string-append "cd " dir " && python3 " prog
                               (apply string-append (map (lambda (r) (string-append " '" r "'")) rels))
                               " > " out " 2>&1"))
        (let ((lines (call-with-input-file out
                       (lambda (p) (let loop ((acc '()))
                                     (let ((l (get-line p)))
                                       (if (eof-object? l) (reverse acc) (loop (cons l acc)))))))))
          (delete-file prog) (delete-file out)
          (let loop ((ls lines) (lits '()) (unread '()) (docs '()))
            (if (null? ls)
                (values (reverse lits) (reverse unread) (reverse docs))
                (let ((f (split-tabs (car ls))))
                  (cond
                    ((and (= (length f) 4) (string=? (car f) "DOC"))
                     (let ((v (read (open-string-input-port (cadddr f)))))
                       (loop (cdr ls) lits unread
                             (if (and (string? v) (leads-into-tmp? v))
                                 (cons (list (cadr f) (string->number (caddr f))) docs)
                                 docs))))
                    ((and (= (length f) 3) (string=? (cadr f) "UNREADABLE"))
                     (loop (cdr ls) lits (cons (list (car f) (read (open-string-input-port (caddr f)))) unread) docs))
                    ((= (length f) 3)
                     (let ((v (read (open-string-input-port (caddr f)))))
                       (loop (cdr ls)
                             (if (and (string? v) (leads-into-tmp? v))
                                 (cons (list (car f) (string->number (cadr f)) v #f) lits)
                                 lits)
                             unread docs)))
                    (else (loop (cdr ls) lits (cons (list 'python-output (car ls)) unread) docs))))))))))

;; ---- reading shell files, as text ---------------------------------------

;; The words of a shell line, split where a path name cannot continue, so
;; that `X=/tmp` or `cd /tmp;` yields the word /tmp itself.
;; With amendment 5's openers, their closers: a bare q{/tmp} or q[/tmp] is
;; the word /tmp once the braces break it (codex r8 C3).
(define shell-breaks '(#\space #\tab #\" #\' #\= #\( #\) #\: #\; #\| #\& #\< #\> #\` #\{ #\[ #\} #\]))
(define (shell-words l)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length l))
           (reverse (if (> i start) (cons (substring l start i) out) out)))
          ((memv (string-ref l i) shell-breaks)
           (loop (+ i 1) (+ i 1) (if (> i start) (cons (substring l start i) out) out)))
          (else (loop (+ i 1) start out)))))

(define (shell-literals rel text)
  (let loop ((ls (let split ((i 0) (start 0) (acc '()))
                   (cond ((= i (string-length text)) (reverse (cons (substring text start i) acc)))
                         ((char=? (string-ref text i) #\newline)
                          (split (+ i 1) (+ i 1) (cons (substring text start i) acc)))
                         (else (split (+ i 1) start acc)))))
             (n 1) (out '()))
    (if (null? ls)
        (reverse out)
        (let* ((l (car ls))
               (trimmed (let skip ((i 0)) (if (and (< i (string-length l)) (char-whitespace? (string-ref l i)))
                                              (skip (+ i 1)) (substring l i (string-length l))))))
          (loop (cdr ls) (+ n 1)
                (if (and (not (prefix? trimmed "#"))
                         (or (leads-into-tmp? (string-append " " l)) (member "/tmp" (shell-words l))))
                    (cons (list rel n l #f) out)
                    out))))))

;; ---- one reading of a set of files --------------------------------------

(define (text-of dir rel) (call-with-input-file (string-append dir "/" rel) get-string-all))

;; (literals unreadable not-read) for the files given; `texts` may supply
;; the text of a file in place of reading it, for the negatives.
(define (read-all dir rels texts)
  (define copied-docs '())
  (let loop ((rs rels) (lits '()) (unread '()) (not-read '()) (pys '()))
    (if (null? rs)
        (let-values (((plits punread pdocs) (python-literals dir (reverse pys))))
          (list (append lits plits) (append unread punread) (reverse not-read)
                (append copied-docs pdocs)))
        (let* ((r (car rs))
               (text (lambda () (let ((t (assoc r texts))) (if t (cdr t) (text-of dir r)))))
               (unreadable (lambda (e) (list r (if (and (condition? e) (message-condition? e))
                                                   (condition-message e) 'unreadable)))))
          (cond
            ((string=? r self-name)
             (loop (cdr rs) lits unread (cons (list r "the checker itself: its specimens and patterns name /tmp on purpose") not-read) pys))
            ((or (suffix? r ".sc") (suffix? r ".ss"))
             (let ((t (guard (e (#t (cons 'unreadable (unreadable e)))) (cons 'text (text)))))
               (if (eq? (car t) 'text)
                   (let-values (((ls u) (scheme-literals r (cdr t))))
                     (loop (cdr rs) (append lits ls) (if u (cons u unread) unread) not-read pys))
                   (loop (cdr rs) lits (cons (cdr t) unread) not-read pys))))
            ((suffix? r ".py")
             (if (assoc r texts)
                 (let ((copy (string-append scratch-base "/tmp-paths-copy-" (number->string (get-process-id)) ".py")))
                   (call-with-output-file copy (lambda (p) (display (cdr (assoc r texts)) p)) 'replace)
                   (let-values (((plits punread pdocs) (python-literals scratch-base
                                                   (list (string-append "tmp-paths-copy-" (number->string (get-process-id)) ".py")))))
                     (delete-file copy)
                     (set! copied-docs (append copied-docs (map (lambda (d) (cons r (cdr d))) pdocs)))
                     (loop (cdr rs)
                           (append lits (map (lambda (l) (cons r (cdr l))) plits))
                           (append unread punread) not-read pys)))
                 (loop (cdr rs) lits unread not-read (cons r pys))))
            ((suffix? r ".sh")
             (if (member r suite-shell-files)
                 (let ((t (guard (e (#t (cons 'unreadable (unreadable e)))) (cons 'text (text)))))
                   (if (eq? (car t) 'text)
                       (loop (cdr rs) (append lits (shell-literals r (cdr t))) unread not-read pys)
                       (loop (cdr rs) lits (cons (cdr t) unread) not-read pys)))
                 (loop (cdr rs) lits unread (cons (list r "a shell tool the suite never runs or sources") not-read) pys)))
            ((member r suite-text-files)
             (let ((t (guard (e (#t (cons 'unreadable (unreadable e)))) (cons 'text (text)))))
               (if (eq? (car t) 'text)
                   (loop (cdr rs) (append lits (shell-literals r (cdr t))) unread not-read pys)
                   (loop (cdr rs) lits (cons (cdr t) unread) not-read pys))))
            (else (loop (cdr rs) lits unread
                        (cons (list r "not a kind of file the suite runs or loads") not-read) pys)))))))

;; A SHELL LINE IS ALLOWED ONLY FOR WHAT ITS ENTRY NAMES: the entry's text
;; is taken out of the line, every time it occurs, and what remains must
;; not lead into /tmp. An entry that merely appears somewhere on a line --
;; in a trailing comment, say -- does not cover another path beside it.
(define (without line t)
  (let loop ((i 0) (out '()))
    (cond ((> (+ i (string-length t)) (string-length line))
           (apply string-append (reverse (cons (substring line i (string-length line)) out))))
          ((string=? t (substring line i (+ i (string-length t))))
           (loop (+ i (string-length t)) (cons " " out)))
          (else (loop (+ i 1) (cons (string (string-ref line i)) out))))))
(define (allowed-by lit)
  (find (lambda (e)
          (and (string=? (car e) (car lit))
               (if (or (suffix? (car lit) ".sh") (member (car lit) suite-text-files))
                   (let* ((line (caddr lit)) (rest (without line (cadr e))))
                     (and (not (string=? rest line))
                          (not (leads-into-tmp? (string-append " " rest)))
                          (not (member "/tmp" (shell-words rest)))))
                   (string=? (cadr e) (caddr lit)))))
        allow-list))

;; The literals that are neither a fallback nor allowed, as "file:line text".
(define (violations lits)
  (map (lambda (l) (format "~a:~a ~s" (car l) (cadr l) (caddr l)))
       (filter (lambda (l) (not (or (cadddr l) (allowed-by l)))) lits)))
(define (unused-allowances lits)
  (filter (lambda (e) (not (exists (lambda (l) (and (not (cadddr l)) (eq? (allowed-by l) e))) lits)))
          allow-list))

;; ---- the reading --------------------------------------------------------

(define walked (files-under script-dir))
(define files (list-sort string<? (car walked)))
(define skipped (list-sort (lambda (a b) (string<? (car a) (car b))) (cadr walked)))
(define reading (read-all script-dir files '()))
(define lits (car reading))

(printf "allow-list (~a entries):\n" (length allow-list))
(for-each (lambda (e)
            (printf "  ~a ~s -- ~a (~a use(s))\n" (car e) (cadr e) (caddr e)
                    (length (filter (lambda (l) (and (not (cadddr l)) (eq? (allowed-by l) e))) lits))))
          allow-list)
(printf "python documentation strings left out as prose, the first statement of a module, class or function, that name /tmp (~a):\n"
        (length (cadddr reading)))
(for-each (lambda (d) (printf "  ~a:~a\n" (car d) (cadr d))) (cadddr reading))
(printf "not read (~a):\n" (+ (length (caddr reading)) (length skipped)))
(for-each (lambda (n) (printf "  ~a -- ~a\n" (car n) (cadr n))) (append (caddr reading) skipped))

(want "TP-00 the walk found the fixtures, the python and the suite's shell files"
      (list (> (length (filter (lambda (f) (suffix? f ".sc")) files)) 100)
            (and (member "paths.py" files) #t)
            (and (for-all (lambda (s) (member s files)) (append suite-shell-files suite-text-files)) #t)
            (> (length (filter cadddr lits)) 50))
      '(#t #t #t #t))
(want "TP-01 every file in scope could be read"
      (cadr reading)
      '())
(want "TP-02 every literal that leads into /tmp is a solo fallback of one of the two variables, or allowed"
      (violations lits)
      '())
(want "TP-03 every allow-list entry allows something"
      (map (lambda (e) (list (car e) (cadr e))) (unused-allowances lits))
      '())

;; ---- the negatives --------------------------------------------------------
;;
;; Each reads a copy of one file with one thing changed and asks for the
;; violations of that copy alone.
(define specimen "q1.sc")
(define specimen-text (text-of script-dir specimen))
(define (with-appended form) (string-append specimen-text "\n" form "\n"))
;; A NEGATIVE READS ITS INPUT OR SAYS IT COULD NOT. An input that failed to
;; read yields no literals, and "no violations" would then pass a control
;; for the wrong reason; so a reading with anything unread raises, and the
;; row that asked goes red.
(define (checked-reading rel text)
  (let ((r (read-all script-dir (list rel) (list (cons rel text)))))
    (unless (null? (cadr r))
      (assertion-violation 'tmp-paths "the negative's input could not be read" (cadr r)))
    r))
(define (violations-of rel text)
  (violations (car (checked-reading rel text))))

(want "TP-N1 a scratch path built on /tmp is the one violation"
      (violations-of specimen (with-appended "(define census-x (string-append \"/tmp/x-\" (number->string (get-process-id))))"))
      (list (format "~a:~a ~s" specimen (+ 2 (length (filter (lambda (c) (char=? c #\newline)) (string->list specimen-text)))) "/tmp/x-")))
(want "TP-N2 the same literal as the fallback of THEOURGIA_TEST_ROOT is no violation"
      (violations-of specimen (with-appended "(define census-x (or (getenv \"THEOURGIA_TEST_ROOT\") \"/tmp/x-\"))"))
      '())
(want "TP-N2 and as the fallback in the let-and-if shape, of THEOURGIA_TEST_SOCK"
      (violations-of specimen (with-appended "(define census-x (let ((w (getenv \"THEOURGIA_TEST_SOCK\"))) (if (string? w) w \"/tmp\")))"))
      '())
(want "TP-N3 the fallback of another variable is a violation"
      (length (violations-of specimen (with-appended "(define census-x (or (getenv \"HOME\") \"/tmp\"))")))
      1)
(want "TP-N4 the first operand of the or, not the fallback, is a violation"
      (length (violations-of specimen (with-appended "(define census-x (or \"/tmp\" (getenv \"THEOURGIA_TEST_ROOT\")))")))
      1)
(want "TP-N5 the same text in a comment is no literal"
      (violations-of specimen (with-appended ";; (string-append \"/tmp/x-\" (number->string (get-process-id)))"))
      '())
(want "TP-N6 /tmp/ inside a command string, after a space, is a violation"
      (length (violations-of specimen (with-appended "(define census-x (system \"rm -rf /tmp/dmn-x\"))")))
      1)
(want "TP-N6 CONTROL: a file name that merely starts with /tmp- is not"
      (violations-of specimen (with-appended "(define census-x (string-append \"d\" \"/tmp-replacement\"))"))
      '())
(want "TP-N7 a literal in a .py file is a violation"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py") "\nX = '/tmp/y'\n")))
      1)
(want "TP-N7 a bare string statement that is not a docstring is read, and is a violation"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py")
                                                       "\ndef census_f():\n    x = 1\n    '/tmp/z'\n")))
      1)
(want "TP-N7 CONTROL: the same string in docstring position is prose, and named"
      (let ((r (checked-reading "paths.py"
                                (string-append (text-of script-dir "paths.py")
                                               "\ndef census_g():\n    '/tmp/z'\n"))))
        (list (length (violations (car r))) (length (cadddr r))))
      '(0 2))
(want "TP-N8 a /tmp path in a suite shell file is a violation"
      (length (violations-of "env.sh" (string-append (text-of script-dir "env.sh") "\nexport X=/tmp/y\n")))
      1)
(want "TP-N9 a Scheme file that does not parse is named, not skipped"
      (map car (cadr (read-all script-dir (list specimen) (list (cons specimen "(define x \"unclosed")))))
      (list specimen))

;; A FILE THAT CANNOT BE OPENED, of each kind read: a Scheme file, a suite
;; shell file and a python file, each mode 000 in a directory of their
;; own. The row first shows that none of them can be opened here, so a
;; run as a user who can read them anyway says so instead of passing.
(define unread-dir (string-append scratch-base "/tmp-paths-unread-" (number->string (get-process-id))))
(system (string-append "rm -rf '" unread-dir "'; mkdir -p '" unread-dir "'"))
(for-each (lambda (n) (call-with-output-file (string-append unread-dir "/" n)
                        (lambda (p) (display "x = 1\n" p)) 'replace))
          '("x.sc" "env.sh" "paths.py"))
(system (string-append "chmod 000 '" unread-dir "'/*"))
(want "TP-N9 a file that cannot be opened is named, for each of the three readers"
      (list (for-all (lambda (n) (guard (e (#t #t)) (call-with-input-file (string-append unread-dir "/" n) get-string-all) #f))
                     '("x.sc" "env.sh" "paths.py"))
            (list-sort string<? (map car (cadr (read-all unread-dir '("env.sh" "paths.py" "x.sc") '())))))
      '(#t ("env.sh" "paths.py" "x.sc")))
(system (string-append "chmod -R u+rwx '" unread-dir "'; rm -rf '" unread-dir "'"))

;; THE FALLBACK IS READ WITH ITS SCOPE, not by its spelling.
(want "TP-N10 an inner let that rebinds the variable to something else is a violation"
      (length (violations-of specimen (with-appended "(define census-x (let ((x (getenv \"THEOURGIA_TEST_ROOT\"))) (let ((x #f)) (if x x \"/tmp/shadow\"))))")))
      1)
(want "TP-N10 CONTROL: in let*, the later binding of the same name is the one in scope"
      (violations-of specimen (with-appended "(define census-x (let* ((x #f) (x (getenv \"THEOURGIA_TEST_ROOT\"))) (if x x \"/tmp/ok\")))"))
      '())
(want "TP-N11 a let binding is not in scope in its sibling's initialiser"
      (length (violations-of specimen (with-appended "(define census-x (let ((x (getenv \"THEOURGIA_TEST_ROOT\")) (y (if #f x \"/tmp/parallel\"))) y))")))
      1)
(want "TP-N12 a getenv bound around the or is not the environment's"
      (length (violations-of specimen (with-appended "(define census-x (let ((getenv (lambda (v) #f))) (or (getenv \"THEOURGIA_TEST_ROOT\") \"/tmp/fake-getenv\")))")))
      1)
(want "TP-N12 a file that defines getenv at top level gets no exemption, its existing fallback included"
      (length (violations-of specimen (with-appended "(define (getenv v) #f)\n(define census-x (or (getenv \"THEOURGIA_TEST_ROOT\") \"/tmp/fake2\"))")))
      2)
(want "TP-N13 the shape inside quote, and inside quasiquote, is data and a violation"
      (list (length (violations-of specimen (with-appended "(define census-x (caddr '(or (getenv \"THEOURGIA_TEST_ROOT\") \"/tmp/quoted\")))")))
            (length (violations-of specimen (with-appended "(define census-x (caddr `(or (getenv \"THEOURGIA_TEST_ROOT\") \"/tmp/quoted\")))"))))
      '(1 1))

;; A SHELL LINE AND A PYTHON STRING, READ FOR WHAT THEY HOLD.
(want "TP-N14 an allowed shell text elsewhere on a line does not cover another path on it"
      (length (violations-of "run-fixtures.sh" (string-append (text-of script-dir "run-fixtures.sh")
                                                              "\nexport CENSUS_PATH=/tmp/escape # mktemp -d /tmp/ths.XXXXXX\n")))
      1)
(want "TP-N15 a shell word that is /tmp itself is a violation"
      (length (violations-of "env.sh" (string-append (text-of script-dir "env.sh") "\nexport CENSUS_PATH=/tmp\n")))
      1)
(want "TP-N16 a string after a docstring on the same line is code, and a violation"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py")
                                                       "\ndef census_h(): 'documentation'; return '/tmp/doc-leak'\n")))
      1)
(want "TP-N16 CONTROL: a docstring written as two strings on two lines is prose throughout, and named"
      (let ((r (checked-reading "paths.py"
                                (string-append (text-of script-dir "paths.py")
                                               "\ndef census_doc():\n    ('doc '\n     '/tmp/doc')\n"))))
        (list (length (violations (car r))) (length (cadddr r))))
      '(0 2))
(want "TP-N17 an f-string is read decoded, escapes and all"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py") "\nX = f\"\\x2ftmp/{42}\"\n")))
      1)

(want "TP-N25 a /tmp literal in launch.pl, read as text, is a violation"
      (length (violations-of "launch.pl" (string-append (text-of script-dir "launch.pl") "\nmy $x = '/tmp/y';\n")))
      1)
(want "TP-N27 a Perl q{} literal leading into /tmp in launch.pl is a violation"
      (length (violations-of "launch.pl" (string-append (text-of script-dir "launch.pl") "\nmy $x = q{/tmp/y};\n")))
      1)
(want "TP-N27 a bare /tmp in Perl q{} and in q[] in launch.pl is a violation, each"
      (list (length (violations-of "launch.pl" (string-append (text-of script-dir "launch.pl") "\nmy $x = q{/tmp};\n")))
            (length (violations-of "launch.pl" (string-append (text-of script-dir "launch.pl") "\nmy $x = q[/tmp];\n"))))
      '(1 1))
(want "TP-N27 and a Scheme string with /tmp/ after [ is a violation"
      (length (violations-of specimen (with-appended "(define census-x \"x[/tmp/y\")")))
      1)
(want "TP-N27 and a python string with /tmp/ after < is a violation"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py") "\nX = 'x</tmp/y'\n")))
      1)
(want "TP-N26 a file of a kind this check does not read is named as not read"
      (map car (caddr (read-all script-dir '("zz-not-read.txt") '())))
      '("zz-not-read.txt"))

;; THE DIRECTORIES THE WALK DOES NOT ENTER ARE NAMED.
(define walk-dir (string-append scratch-base "/tmp-paths-walk-" (number->string (get-process-id))))
(system (string-append "rm -rf '" walk-dir "'; mkdir -p '" walk-dir "/__pycache__' '" walk-dir "/real'"
                       "; echo '(define p 1)' > '" walk-dir "/__pycache__/leak.ss'"
                       "; ln -s real '" walk-dir "/linked'"))
(want "TP-N18 __pycache__ and a directory reached by a symbolic link are named as not entered"
      (list-sort string<? (map car (cadr (files-under walk-dir))))
      '("__pycache__" "linked"))
(system (string-append "rm -rf '" walk-dir "'"))
;; THE SMALLER READINGS: getenv's arity, f-string contents, the source
;; encoding, a control's own input, and a tab in a value.
(want "TP-N19 a getenv call with an extra operand is not the environment reader's shape"
      (length (violations-of specimen (with-appended "(define census-x (or (getenv \"THEOURGIA_TEST_ROOT\" #f) \"/tmp/x\"))")))
      1)
(want "TP-N20 a string inside an f-string's expression is read, once"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py") "\nX = f\"{'/tmp/inside'}\"\n")))
      1)
;; A LATIN-1 SOURCE: the docstring's extent is measured in the decoded
;; text, so the string after it on the same line is code. The row first
;; shows the file really is Latin-1 -- byte 233 for each e-acute, none of
;; UTF-8's 195.
(define latin-dir (string-append scratch-base "/tmp-paths-latin-" (number->string (get-process-id))))
(system (string-append "rm -rf '" latin-dir "'; mkdir -p '" latin-dir "'"))
(call-with-port
  (open-file-output-port (string-append latin-dir "/latin.py") (file-options no-fail) (buffer-mode block)
                         (make-transcoder (latin-1-codec)))
  (lambda (p) (display (string-append "# coding: latin-1\ndef f(): '" (make-string 30 (integer->char 233))
                                      "'; return '/tmp/hidden'\n") p)))
(want "TP-N21 in a Latin-1 source a docstring does not cover the code string after it"
      (let ((bytes (bytevector->u8-list (call-with-port (open-file-input-port (string-append latin-dir "/latin.py"))
                                                        get-bytevector-all))))
        (let-values (((lits unread docs) (python-literals latin-dir '("latin.py"))))
          (list (and (memv 233 bytes) (not (memv 195 bytes)) #t)
                (map caddr lits) unread docs)))
      '(#t ("/tmp/hidden") () ()))
;; AND THE COLUMN IS COUNTED IN DECODED TEXT BEFORE THE DOCSTRING TOO:
;; ten e-acute in a function name ahead of it on the same line, which a
;; reading that decoded Latin-1 as UTF-8 would count differently.
(call-with-port
  (open-file-output-port (string-append latin-dir "/latin2.py") (file-options no-fail) (buffer-mode block)
                         (make-transcoder (latin-1-codec)))
  (lambda (p) (display (string-append "# coding: latin-1\ndef " (make-string 10 (integer->char 233))
                                      "(): '/tmp/doc'\n") p)))
(want "TP-N21 in a Latin-1 source a docstring after non-ASCII text on its line is still prose"
      (let-values (((lits unread docs) (python-literals latin-dir '("latin2.py"))))
        (list (map caddr lits) unread (map cadr docs)))
      '(() () (2)))
(system (string-append "rm -rf '" latin-dir "'"))
(want "TP-N22 a negative whose input cannot be read raises instead of reporting no violations"
      (guard (e (#t 'raised))
        (violations-of specimen (string-append specimen-text "\n(\n(define census-x (or (getenv \"THEOURGIA_TEST_ROOT\") \"/tmp/x-\"))\n")))
      'raised)
(want "TP-N20 an ordinary string joined to an f-string without an operator is read once"
      (length (violations-of "paths.py" (string-append (text-of script-dir "paths.py") "\nX = f\"{42}\" '/tmp/tail'\n")))
      1)
(want "TP-N23 a lone surrogate in a python string is a value, not an unreadable file"
      (let ((r (read-all script-dir (list "paths.py")
                         (list (cons "paths.py" (string-append (text-of script-dir "paths.py") "\nX = '\\ud800'\n"))))))
        (list (violations (car r)) (cadr r)))
      '(() ()))
(want "TP-N23 a value with a tab, a quote, a backslash and a carriage return comes back exactly"
      (filter (lambda (v) (prefix? v "/tmp/q"))
              (map caddr (car (checked-reading "paths.py"
                                               (string-append (text-of script-dir "paths.py") "\nX = '/tmp/q\\t\"\\\\\\r'\n")))))
      (list (string-append "/tmp/q" (string #\tab #\" #\\ #\return))))
(want "TP-N23 NEL and the two Unicode line separators in a value come back as themselves, not as newlines"
      (filter (lambda (v) (prefix? v "/tmp/n"))
              (map caddr (car (checked-reading "paths.py"
                                               (string-append (text-of script-dir "paths.py") "\nX = '/tmp/n\\u0085\\u2028\\u2029'\n")))))
      (list (string-append "/tmp/n" (string (integer->char #x85) (integer->char #x2028) (integer->char #x2029)))))
;; A SOURCE WITH CR-ONLY LINE ENDS, written as bytes: the tokenizer and the
;; syntax tree count its lines alike, so the joined string is read once
;; and the docstring on line 3 is prose.
(define cr-dir (string-append scratch-base "/tmp-paths-cr-" (number->string (get-process-id))))
(system (string-append "rm -rf '" cr-dir "'; mkdir -p '" cr-dir "'"))
(call-with-port (open-file-output-port (string-append cr-dir "/cr.py") (file-options no-fail))
  (lambda (p) (put-bytevector p (string->utf8 (string-append "pass" (string #\return)
                                                             "X = f\"{42}\" '/tmp/tail'" (string #\return)
                                                             "def f(): '/tmp/doc'" (string #\return))))))
(want "TP-N24 in a source with CR line ends a joined string is read once and a docstring is still prose"
      (let ((bytes (bytevector->u8-list (call-with-port (open-file-input-port (string-append cr-dir "/cr.py"))
                                                        get-bytevector-all))))
        (let-values (((lits unread docs) (python-literals cr-dir '("cr.py"))))
          (list (and (memv 13 bytes) (not (memv 10 bytes)) #t) (map caddr lits) unread (map cadr docs))))
      '(#t ("/tmp/tail") () (3)))
(system (string-append "rm -rf '" cr-dir "'"))
(want "TP-N23 a tab inside a python string is a value, not an unreadable file"
      (let ((r (read-all script-dir (list "paths.py")
                         (list (cons "paths.py" (string-append (text-of script-dir "paths.py") "\nX = '\\t'\n"))))))
        (list (violations (car r)) (cadr r)))
      '(() ()))

(printf "\n~a failures\nrows: ~a\ntmp-paths complete\n" bad rows)
