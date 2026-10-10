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

;; NOTHING IN THE DAEMON MAY LINK.
;;
;; NEVER: A LINKED PROCESS THAT EXITS ABNORMALLY TAKES ITS PEER WITH IT, and
;; it does so without asking: igropyr's `@kill` kills a linked process
;; that is not trapping exits, and that process has no say and no chance
;; to finish what it was writing. The daemon's whole shape is the
;; opposite -- one connection dying costs that connection, a writer dying
;; costs that writer, and only the store and the listener are fatal. One
;; `link` anywhere in the tree the daemon loads would make that a
;; statement about the paths that happen to be tested rather than about
;; the program.
;;
;; NEVER: SO THE RULE IS ABOUT WHAT IS IN SCOPE, NOT ABOUT WHAT IS CALLED.
;; "No call to `link`" is a question about every path; "the name `link`
;; is not imported here" is a question about the text, it is closed, and
;; it cannot be satisfied by a call that only runs on Tuesdays. What a
;; file has not imported it cannot write.
;;
;; NEVER: AND IT CANNOT BE A GREP FOR `link`. Measured on this tree: the
;; symbol `link` appears as data in EIGHT files of the daemon's import
;; closure -- `reduce.sc` 5, `store.sc` 8, `rpc.sc` 3, `md.sc` 2,
;; `ffi.sc` 2, `request.sc`, `baseline.sc` -- and every one of those is
;; either the document verb `link`/`unlink` or a filesystem link. A grep
;; gate would have been permanently red about seven files with nothing
;; wrong with them, which is the same as having no gate.
;;
;; THE FACADES ARE EXEMPT, BY THE SAME LIST THE OTHER RULES USE. `sched`
;; is where `link` and `spawn&link` are re-exported from igropyr; that is
;; its job, and it is the one place the name may appear.

(import (chezscheme))

(define failures 0)
(define rows 0)
(define (want-1 name actual expected)
  (set! rows (+ rows 1))
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
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
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring argv0 0 cut) ".")))
(load (string-append script-dir "/import-walk.sc"))

(define root
  (let ((up (string-append script-dir "/..")))
    up))

(define forbidden '(link spawn&link))

(define facades
  (let ((names (call-with-input-file (string-append script-dir "/facades.sexp") read)))
    names))

(define (source-of name)
  (string-append root "/" (symbol->string name) ".sc"))

;; ---- the closure ----------------------------------------------------------
;;
;; NOTE: TRANSITIVE, because the rule is about what the daemon LOADS. A
;; `link` two libraries away is in the same VM and links the same
;; processes as one written here.
(define (closure-of path)
  (let walk ((todo (list path)) (seen '()))
    (cond
      ((null? todo) seen)
      ((member (car todo) seen) (walk (cdr todo) seen))
      (else
       (let* ((here (car todo))
              (libs (imports-of-file 'theourgia here))
              (next (filter file-exists?
                            (map (lambda (l) (source-of (cadr l))) libs))))
         (walk (append next (cdr todo)) (cons here seen)))))))

(define daemon-closure (closure-of (string-append root "/daemon.sc")))

(define (basename path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) path)
          ((char=? (string-ref path i) #\/) (substring path (+ i 1) (string-length path)))
          (else (loop (- i 1))))))

(define (facade? path)
  (exists (lambda (f) (string=? (basename path) (string-append (symbol->string f) ".sc")))
          facades))

;; ---- the question ---------------------------------------------------------
;;
;; An import of the scheduler facade names what it brings in, and the
;; answer is the list of names. A WHOLESALE import brings in everything
;; the facade exports, `link` among them, so it answers #f -- which is
;; the shape this refuses.
(define (sched-names form)
  (cond
    ((not (pair? form)) '())
    ((eq? 'import (car form))
     (let collect ((xs (cdr form)) (out '()))
       (cond
         ((not (pair? xs)) out)
         ((and (pair? (car xs)) (eq? 'only (caar xs))
               (pair? (cdar xs)) (equal? '(theourgia sched) (cadr (car xs))))
          (collect (cdr xs) (cons (cddr (car xs)) out)))
         ((equal? '(theourgia sched) (car xs))
          (collect (cdr xs) (cons #f out)))
         (else (collect (cdr xs) out)))))
    (else (append (sched-names (car form)) (sched-names (cdr form))))))

(define (offences path)
  (let loop ((fs (forms-of path)) (out '()))
    (cond
      ((null? fs) out)
      (else
       (loop (cdr fs)
             (append out
               (apply append
                 (map (lambda (names)
                        (cond
                          ((not names) (list 'imports-the-whole-scheduler))
                          (else (filter (lambda (n) (memq n forbidden)) names))))
                      (sched-names (car fs))))))))))

(define checked (filter (lambda (p) (not (facade? p))) daemon-closure))

(want "DL-01 the daemon's import closure was found"
      (if (> (length daemon-closure) 10) 'walked (list 'only (length daemon-closure)))
      'walked)

(want "DL-02 daemon.sc itself is in it"
      (if (exists (lambda (p) (string=? (basename p) "daemon.sc")) daemon-closure)
          'present 'MISSING)
      'present)

;; NEVER: THE GATE PROVES IT CAN STILL SEE THE THING IT IS LOOKING FOR. A
;; walker that quietly stopped matching would print this same clean
;; reading for ever, and the tree would go on looking compliant.
(want "DL-03 a wholesale import of the scheduler is recognised"
      (sched-names '(import (rnrs) (theourgia sched) (theourgia digest)))
      '(#f))
(want "DL-04 a named import of link is recognised"
      (sched-names '(import (only (theourgia sched) spawn link send)))
      '((spawn link send)))
(want "DL-05 and a named import without it is not an offence"
      (filter (lambda (n) (memq n forbidden))
              (car (sched-names '(import (only (theourgia sched) spawn send self)))))
      '())

(for-each
  (lambda (path)
    (let ((bad (offences path)))
      (want (string-append "DL-06 " (basename path) " does not bring `link` into scope")
            (if (null? bad) 'clean (cons 'brings bad))
            'clean)))
  (list-sort string<? checked))

(printf "rows: ~a\n~a failures\ndaemon-link-gate complete\n" rows failures)
(exit (if (zero? failures) 0 1))
