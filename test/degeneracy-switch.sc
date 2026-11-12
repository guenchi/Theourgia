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

;; THE DEGENERACY SWITCH NAMES ITS CHECKS (degeneracy.sc).
;;
;;   DS-1  the names the library knows are exactly the names its uses carry,
;;         read from the tree's source: a use cannot carry a name the switch
;;         does not know, nor the switch keep a name no use has
;;   DS-2  each check a whole-suite gate leaves out for its cost has a
;;         witness: a fixture whose own child sets THEOURGIA_DEGENERACY=1 and
;;         whose named row judges it
;;   DS-3  the variable's three forms, each read in a child that expands the
;;         library: unset none, 1 every check, only: those, all-but: the rest
;;   DS-4  an unknown name in the variable, an empty one, any other value of
;;         the variable, and an unknown name at a use each refuse at
;;         expansion, naming what they refuse
;;   DS-5  one check on and another off: degeneracy-active and
;;         degeneracy-value answer by name
;;
;; The children run from this tree's library path, as this process does; each
;; writes its answer on stdout, and a refusal is read from stderr.
(import (chezscheme) (theourgia degeneracy))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define tree-dir (string-append script-dir "/.."))
(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/degeneracy-switch-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))

(define (file-text p)
  (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (contains? text needle)
  (let ((n (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (symbol-list<? a b) (string<? (symbol->string a) (symbol->string b)))

;; ---- DS-1, the census ----------------------------------------------------
;; -> the names carried by every (degeneracy-check <name> and
;; (degeneracy-value <name> in TEXT, in order, repeats kept
(define (names-used text)
  (let loop ((i 0) (out '()))
    (let next ((heads '("(degeneracy-check " "(degeneracy-value ")))
      (cond
        ((>= i (string-length text)) (reverse out))
        ((null? heads) (loop (+ i 1) out))
        ((let ((h (car heads)))
           (and (<= (+ i (string-length h)) (string-length text))
                (string=? (substring text i (+ i (string-length h))) h)))
         (let* ((s (+ i (string-length (car heads))))
                (e (let end ((j s))
                     (if (or (= j (string-length text))
                             (memv (string-ref text j) '(#\space #\newline #\( #\))))
                         j
                         (end (+ j 1))))))
           (loop e (cons (string->symbol (substring text s e)) out))))
        (else (next (cdr heads)))))))
;; the tree's library files: every .sc at its root but this switch's own
(define product-files
  (filter (lambda (f) (and (> (string-length f) 3)
                           (string=? (substring f (- (string-length f) 3) (string-length f)) ".sc")
                           (not (string=? f "degeneracy.sc"))))
          (list-sort string<? (directory-list tree-dir))))
(define uses
  (apply append (map (lambda (f) (map (lambda (n) (cons f n)) (names-used (file-text (string-append tree-dir "/" f)))))
                     product-files)))
(printf "DS-1 the uses: ~s\n" uses)
(want "DS-1 the names the switch knows are exactly the names its uses carry, read from the tree's source, and there are eleven uses"
      (list (list-sort symbol-list<? (let dedupe ((ns (map cdr uses)) (out '()))
                                       (cond ((null? ns) out)
                                             ((memq (car ns) out) (dedupe (cdr ns) out))
                                             (else (dedupe (cdr ns) (cons (car ns) out))))))
            (length uses))
      (list (list-sort symbol-list<? (degeneracy-known-checks)) 11))

;; ---- DS-2, the two checks a gate leaves out, and their witnesses ----------
(define witnesses
  '((structure-per-record "structure-graph.sc" "SG-1")
    (kept-prefix "append-prefix.sc" "AP-D")))
(want "DS-2 each check a whole-suite gate leaves out is a known name, and its witness fixture sets THEOURGIA_DEGENERACY=1 for its own child and holds the row that judges it"
      (map (lambda (w)
             (let ((text (file-text (string-append script-dir "/" (cadr w)))))
               (list (car w)
                     (and (memq (car w) (degeneracy-known-checks)) #t)
                     (contains? text "THEOURGIA_DEGENERACY=1 ")
                     (contains? text (string-append "(want \"" (caddr w) " ")))))
           witnesses)
      '((structure-per-record #t #t #t) (kept-prefix #t #t #t)))

;; ---- the children ----------------------------------------------------------
;; -> (stdout-datum-or-#f . stderr text) of a child that expands PROGRAM with
;; THEOURGIA_DEGENERACY set to SETTING (#f: unset)
(define (child tag setting program)
  (let ((script (string-append root "/" tag ".ss"))
        (out (string-append root "/" tag ".out"))
        (err (string-append root "/" tag ".err")))
    (call-with-output-file script (lambda (p) (for-each (lambda (f) (write f p) (newline p)) program)) 'replace)
    (system (string-append (if setting
                               (string-append "THEOURGIA_DEGENERACY='" setting "' ")
                               "env -u THEOURGIA_DEGENERACY ")
                           "scheme --script '" script "' > '" out "' 2> '" err "' < /dev/null"))
    (cons (guard (e (#t #f))
            (let ((x (read (open-string-input-port (file-text out))))) (if (eof-object? x) #f x)))
          (file-text err))))
(define checks-on-program
  '((import (chezscheme) (theourgia degeneracy)) (write (degeneracy-checks-on))))
(define (checks-on tag setting) (car (child tag setting checks-on-program)))

;; ---- DS-3, the three forms --------------------------------------------------
(want "DS-3 unset, no check is on"
      (checks-on "unset" #f)
      '())
(want "DS-3 1, every check, in the known order"
      (checks-on "all" "1")
      (degeneracy-known-checks))
(want "DS-3 only:, those checks and no other, in the known order"
      (checks-on "only" "only:kept-prefix,replay-scan")
      '(replay-scan kept-prefix))
(want "DS-3 all-but:, every check but those: the whole-suite gate's form"
      (checks-on "all-but" "all-but:structure-per-record,kept-prefix")
      (filter (lambda (n) (not (memq n '(structure-per-record kept-prefix)))) (degeneracy-known-checks)))

;; ---- DS-4, the refusals -----------------------------------------------------
;; -> (<the child answered nothing> <its error names the refused text>)
(define (refused tag setting program needle)
  (let ((r (child tag setting program)))
    (list (not (car r)) (contains? (cdr r) needle))))
(want "DS-4 an unknown name in only: refuses at expansion, naming it"
      (refused "unknown" "only:replay-scna" checks-on-program "replay-scna")
      '(#t #t))
(want "DS-4 an unknown name in all-but: refuses at expansion, naming it"
      (refused "unknown-but" "all-but:kept-prefx" checks-on-program "kept-prefx")
      '(#t #t))
(want "DS-4 an empty name refuses, and so does any other value of the variable"
      (list (refused "empty" "only:" checks-on-program "empty check")
            (refused "other" "yes" checks-on-program "only:<names>"))
      '((#t #t) (#t #t)))
(want "DS-4 a use carrying a name the library does not know refuses at expansion, naming it"
      (refused "use" "1" '((import (chezscheme) (theourgia degeneracy))
                           (write (list (degeneracy-check no-such-check (display "ran")))))
               "no-such-check")
      '(#t #t))

;; ---- DS-5, by name ----------------------------------------------------------
(want "DS-5 with only replay-scan on, the checks on are that one"
      (checks-on "by-name" "only:replay-scan")
      '(replay-scan))
(want "DS-5 degeneracy-active and degeneracy-value answer by name"
      (car (child "by-name-forms" "only:replay-scan"
                  '((import (chezscheme) (theourgia degeneracy))
                    (write (list (degeneracy-active) (degeneracy-active replay-scan) (degeneracy-active kept-prefix)
                                 (degeneracy-value replay-scan 'on 'off) (degeneracy-value kept-prefix 'on 'off)
                                 (let ((ran #f)) (degeneracy-check kept-prefix (set! ran #t)) ran)
                                 (let ((ran #f)) (degeneracy-check replay-scan (set! ran #t)) ran))))))
      '(#t #t #f on off #f #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ndegeneracy-switch complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
