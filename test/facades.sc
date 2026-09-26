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

;; THE COPIES ARE GONE, AND THIS IS WHAT SAYS SO.
;;
;; Z vendored three pieces of igropyr into this tree; v126 put them back
;; behind imports. What a fixture can check afterwards is not "the code
;; is identical to upstream" -- there is no copy left to compare -- but
;; that NONE OF THE COPIED DEFINITIONS IS STILL DEFINED HERE, and that
;; each file really does import the library it now depends on.
;;
;; TWO KINDS OF FACADE, BECAUSE THEY ARE NOT THE SAME SHAPE:
;;
;;   `digest.sc` is a PURE forward -- a library header, an import and an
;;   export, and nothing else. For it the check is the strong one: zero
;;   top-level definitions. That catches the private helpers (`mask32`,
;;   `rotr32`, `sha256-k` ...) without naming them.
;;
;;   `wire.sc` and `ffi.sc` are this core's OWN libraries that happen to
;;   be the single seam for one area of igropyr each. `encode-record`,
;;   `exec-argv!` and a hundred and eighty others live in them and
;;   always did, so "zero definitions" would fail a correct file. For
;;   them the check is the name list below.
;;
;; NOTE: WHERE THE NAME LIST COMES FROM, because this matters more than the
;; list: it was MEASURED -- every `(define ...)` between the two
;; `COPIED FROM IGROPYR` markers as they stood at bc5547a. It was NOT
;; copied from those markers' own `extracted` headers, which are wrong:
;; `wire.sc`'s header named fifteen definitions and the region defined
;; THIRTY-THREE. The eighteen it never mentioned -- `parse-atom`,
;; `token->number`, `ws?`, `parse-list` and the rest -- would have been
;; free to come back with nothing to notice.
;;
;; That is the shape to remember when the next thing is vendored: a
;; hand-written list of what a region contains goes stale the first time
;; the region grows, and it goes stale silently. Recompute the list from
;; the region; do not transcribe it from a comment about the region.

(import (chezscheme))

(define failures 0)
(define rows 0)
(define (want-1 name actual expected)
  (set! rows (+ rows 1))
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
;; BOTH SIDES OF A ROW ARE GUARDED. A `want` written as a procedure
;; evaluates its arguments before the call, so an argument that raises
;; kills the fixture: the row never prints, and what the suite sees is a
;; missing sentinel rather than a red row naming the question that could
;; not be answered. This directory's `run-fixtures.sh` counts fixtures
;; whose `want` is a bare procedure for exactly that reason.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/core.sc")) up script-dir)))

;; Measured at bc5547a, between the two markers. See the note above on
;; why this is not the markers' own list.
(define copied-definitions
  '((digest
      add32 bytevector->hex hex-digits mask32 rotr32 sha256 sha256-k 
      shr32)
    (wire
      $parse b64-chars b64-value base64-decode base64-encode 
      default-max-depth default-max-token delim? digits? emit 
      numeric-shape? out parse-atom parse-b64 parse-bytevector-b64 
      parse-flonum-b64 parse-hash parse-list parse-string parse-value 
      parse-vector put-numeral sexpr->string sexpr->string-extended 
      sfail skip string->sexpr string->sexpr-extended symbol-char? 
      token->number valid-symbol? wire-symbol? ws?)
    (ffi
      ensure-supported-platform! load-first-shared-object! machine-name 
      platform-arch platform-os string-contains? string-search 
      string-suffix?)))

(define (forms-of path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((out '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse out) (loop (cons x out))))))))

;; Top-level definitions of a library: the `define` forms directly in the
;; library body, not the ones nested inside them.
(define (library-body form)
  (if (and (pair? form) (eq? 'library (car form))) (cddddr form) '()))
;; NOTE: IT WALKS INTO `begin` AND COUNTS `define-syntax`. Reading only the
;; immediate library body, a definition wrapped in `(begin ...)` is
;; invisible and so is a macro -- so "sched defines nothing of its own"
;; was a rule anybody could step around by adding one pair of
;; parentheses. Reported by codex; the row it protects is the one that
;; says a forwarding library forwards and nothing else.
(define (defined-names path)
  (let walk ((forms (library-body (car (forms-of path)))) (out '()))
    (cond
      ((null? forms) out)
      ((not (pair? (car forms))) (walk (cdr forms) out))
      ((and (memq (caar forms) '(define define-syntax)) (pair? (cdar forms)))
       (walk (cdr forms)
             (cons (let ((head (cadr (car forms))))
                     (if (pair? head) (car head) head))
                   out)))
      ((eq? (caar forms) 'begin) (walk (append (cdar forms) (cdr forms)) out))
      (else (walk (cdr forms) out)))))

(define (igropyr-in x)
  (cond ((and (pair? x) (eq? 'igropyr (car x))) (list x))
        ((pair? x) (append (igropyr-in (car x)) (igropyr-in (cdr x))))
        (else '())))
(define (imports-of form)
  (cond ((not (pair? form)) '())
        ((eq? 'import (car form)) (igropyr-in (cdr form)))
        (else (append (imports-of (car form)) (imports-of (cdr form))))))

(define (path-of name) (string-append root "/" (symbol->string name) ".sc"))

;; -- the pure forward -------------------------------------------------
(want "D1-01 digest.sc defines nothing of its own"
      (defined-names (path-of 'digest)) '())
(want "D1-01 and it takes sha256 and bytevector->hex from igropyr"
      (map car (apply append (map imports-of (forms-of (path-of 'digest)))))
      '(igropyr))

;; -- the mixed libraries ----------------------------------------------
(for-each
  (lambda (entry)
    (let* ((name (car entry))
           (gone (cadr entry))
           (here (defined-names (path-of name)))
           (left (filter (lambda (n) (memq n here)) gone)))
      (want (string-append "D1-02 no copied definition is left in "
                           (symbol->string name) ".sc")
            left '())
      (want (string-append "D1-02 and " (symbol->string name)
                           ".sc imports igropyr")
            (pair? (apply append (map imports-of (forms-of (path-of name))))) #t)))
  (map (lambda (e) (list (car e) (cdr e))) copied-definitions))

;; ---- every facade, not only the three that were copies ----------------
;;
;; NOTE: THE LIST ABOVE IS A HISTORY, NOT A POLICY. It names definitions
;; that WERE copied out of igropyr and must not come back, so it says
;; nothing about a facade that never held a copy -- and `sched`, `net`
;; and `proc` never did. A facade could therefore import igropyr, export
;; the right names, and privately redefine what it claims to forward,
;; with every row above green.
;;
;; So the rule is stated for all six, read from `facades.sexp`:
;;
;;   * `sched` FORWARDS AND NOTHING ELSE. It re-exports a scheduler; a
;;     definition in it would be a second scheduler in the seam.
;;   * `net` and `proc` MAY DEFINE ADAPTERS -- they own connections and
;;     children so that igropyr's vector shapes never reach a caller, and
;;     that needs code. What they may not do is define a transport or a
;;     process primitive of their own: the seam exists so the dependency
;;     can be replaced, and a facade that reimplemented half of it would
;;     make the replacement a lie.
;;   * `digest`, `wire`, `ffi` keep the historical rule above.

(define facade-names
  (call-with-input-file (string-append script-dir "/facades.sexp") read))

(want "D1-05 the facade list is the seven this tree declares"
      facade-names '(digest wire ffi sched net proc json))

;; KEY: ONE DEFINITION IN sched, BY NAME (F100a, ruling A2). Measured rather
;; than asserted in prose: the file is read as data and every `define` in
;; it counted. Its own start-scheduler exists because Chez runs a
;; library's body only when a variable the library defines is referenced:
;; it hands ffi the mutation record's key, (lambda () self), and then
;; starts igropyr's scheduler. A second definition is red here.
(want "D1-05 sched.sc defines exactly (start-scheduler)"
      (defined-names (path-of 'sched)) '(start-scheduler))

;; AND THAT DEFINITION DOES WHAT THE ROW ABOVE SAYS IT IS FOR: read as
;; data, its body names ffi's setter and igropyr's start-scheduler,
;; imported renamed. A start-scheduler that forgot the setter, or called
;; itself, would still satisfy the name row.
(define (symbols-in x)
  (cond ((symbol? x) (list x))
        ((pair? x) (append (symbols-in (car x)) (symbols-in (cdr x))))
        (else '())))
(want "D1-05 TWIN: sched's start-scheduler calls mutation-set-self! and igropyr's start-scheduler, imported renamed"
      (let* ((lib (car (forms-of (path-of 'sched))))
             (def (find (lambda (f) (and (pair? f) (eq? (car f) 'define)
                                         (pair? (cadr f)) (eq? (caadr f) 'start-scheduler)))
                        (library-body lib)))
             (syms (if def (symbols-in (cddr def)) '()))
             (import-specs (cdr (list-ref lib 3))))
        (list (and (memq 'mutation-set-self! syms) #t)
              (and (memq 'igropyr-start-scheduler syms) #t)
              (and (member '(rename (only (igropyr actor) start-scheduler)
                                    (start-scheduler igropyr-start-scheduler))
                           import-specs)
                   #t)))
      '(#t #t #t))

;; NOTE: AND THE OTHER TWO DO DEFINE THINGS, which is what makes the row
;; above a statement about `sched` rather than about the reader. An
;; empty answer everywhere would satisfy it for the wrong reason.
(want "D1-05 TWIN: net and proc do define their adapters"
      (map (lambda (n) (> (length (defined-names (path-of n))) 0)) '(net proc))
      '(#t #t))

;; NEVER: AND NONE OF THE SIX REDEFINES A NAME IGROPYR OWNS. A facade whose
;; own definition shadows the thing it forwards is the failure this
;; whole arrangement is for: callers would be depending on this tree's
;; copy of a primitive while the gate reported one clean seam.
(define igropyr-primitive-names
  '(spawn spawn&link send receive self monitor link start-scheduler sleep-ms
    process-alive? tcp-listen! tcp-connect! tcp-write! tcp-close! tcp-read-start!
    tcp-read-stop! tcp-stop-listen! pipe-listen! pipe-connect! conn-set-owner!
    conn-on-close! proc-spawn! proc-write! proc-kill! proc-close! proc-stdin-close!
    proc-read-start! proc-read-stop! sha256 bytevector->hex))

;; ONE STATED EXCEPTION (F100a, ruling A2): sched's own start-scheduler,
;; which D1-05 pins and whose body its TWIN reads. It is not a copy of
;; igropyr's: it forwards to it by its renamed import.
(define (allowed-shadow? facade n)
  (and (eq? facade 'sched) (eq? n 'start-scheduler)))
(for-each
  (lambda (name)
    (let* ((here (defined-names (path-of name)))
           (clash (filter (lambda (n) (and (memq n igropyr-primitive-names)
                                           (not (allowed-shadow? name n))))
                          here)))
      (want (string-append "D1-06 " (symbol->string name)
                           ".sc defines no name igropyr owns")
            clash '())))
  facade-names)

;; -- the twin ---------------------------------------------------------
;; `crc32.sc` was never a copy: it is this tree's own digest, and it must
;; still carry its definitions. Without this row the two checks above are
;; also satisfied by a tree in which every library has been emptied.
(want "D1-03 TWIN: crc32.sc still defines its own algorithm"
      (> (length (defined-names (path-of 'crc32))) 3) #t)
(want "D1-03 TWIN: and it imports no igropyr"
      (apply append (map imports-of (forms-of (path-of 'crc32)))) '())

;; -- the export sets, pinned to c91f033 -------------------------------
;; A facade that re-exported its whole upstream library would satisfy
;; everything above and quietly widen what a replacement has to provide.
(define (exports-of path)
  (let ((form (car (forms-of path))))
    (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
               (cdr (caddr form)))))
(want "D1-04 digest.sc exports exactly what the core uses"
      (exports-of (path-of 'digest)) '(bytevector->hex sha256))
;; record-envelope-refusal is the one addition since: the envelope rule the
;; reader, publish and the writer all ask, exported so that there is one of
;; it rather than a copy per caller.
(want "D1-04 wire.sc exports what it exported before the change, and the envelope rule"
      (exports-of (path-of 'wire))
      '(decode-line encode-record escape-newlines record-envelope-refusal
        sexpr->string-extended storable-decode storable-encode
        string->sexpr-extended wire-safe-symbol?))

(printf "rows: ~a\n~a failures\nfacades complete\n" rows failures)
(exit (if (zero? failures) 0 1))
