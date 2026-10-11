#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; THE LANDING GATE OF A STORE: the place gate readings are kept and the rule a
;; landing is judged by. An operator script, not a fixture.
;;
;;   scheme --script test/tools/landing-gates.sc <store>
;;   scheme --script test/tools/landing-gates.sc --self-test
;;
;; It declares, in the store it is given (not in a template):
;;
;;   - one top-level document `gates` (kind doc, slot "gates"), under which each
;;     gate's reading is written once, as a section of class observation with
;;     the fields slot "gate", cut (the cut literal, as text, in the one spelling
;;     `tag` answers), result ("green" or the list of what was not) and
;;     alternation;
;;   - one note under it on the two slots (a section, slot "gates-note");
;;   - the rule landing-gate: a decision with slot "landing" must carry a cut
;;     and be verified by a gate reading of the same cut, of class observation
;;     and slot "gate", whose result is green. A landing is written in ONE batch
;;     with its verifies edge, so the rule judges the state the whole batch
;;     produces.
;;
;; IT CAN BE RUN AGAIN. The document is found by its kind, slot and place (top
;; level); two such documents are refused, not chosen between. The note is
;; found by its slot under the document and its text set again (restored when
;; it was edited); two notes are refused. The rule
;; declared again answers (ok (unchanged)). It prints, and exits 0 only when the
;; document, the note and the rule are all in place:
;;
;;   (landing-gates (gates <id> made|found) (note <id> made|found <set answer>) (rule <answer>))
;;   (error <reason> ...)
;;
;; --self-test runs it on a scratch store and writes landings the rule must
;; take and refuse, printing one row each.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) block-id))

(register-verbs! extension-verbs)

(define rule-where "(field ?w \"slot\" \"landing\")")
(define rule-must
  "(and (field ?w \"cut\" ?c) (edge ?g verifies ?w) (class ?g observation) (field ?g \"slot\" \"gate\") (field ?g \"cut\" ?c) (field ?g \"result\" \"green\"))")
(define note-text
  (string-append
    "Two slots carry the landing protocol. A gate reading is a section under this document,"
    " class observation, slot \"gate\", with the fields cut (the cut literal the gate ran on),"
    " result (\"green\" or what was not) and alternation; it is written once and never edited,"
    " since an edit to it does not judge the landing again. A landing is a decision with slot"
    " \"landing\" and the field cut, written in one batch with the edge <gate> verifies <landing>"
    " and premises on the gate's version; the rule landing-gate refuses it unless that gate ran"
    " on the same cut and is green.\n"))

(define (quoted s)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))

(define (new-ids answer)
  (let ((ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer)))))
    (if ev (map (lambda (e) (block-id (car e) (cdr e))) (cadr ev)) '())))
;; The id the batch's first intent made, or the refusal.
(define (batch-made store intents)
  (let ((a (rpc-dispatch store (list 'batch (format "~s" intents)) "landing-gates")))
    (if (and (pair? a) (eq? (car a) 'batch) (pair? (cadr a)) (pair? (new-ids (car (cadr a)))))
        (car (new-ids (car (cadr a))))
        a)))
;; The first column of a query's rows, or (error query-failed <answer>).
(define (query-ids store goal)
  (let* ((a (rpc-dispatch store (list 'query goal) "landing-gates"))
         (items (and (pair? a) (eq? (car a) 'ok) (find (lambda (c) (and (pair? c) (eq? (car c) 'items))) (cdr a)))))
    (if items
        (map cadr (filter (lambda (r) (and (pair? r) (eq? (car r) 'row))) (cdr items)))
        (list 'error 'query-failed a))))
(define (failed? x) (and (pair? x) (eq? (car x) 'error)))

(define (setup! store)
  (call/cc
    (lambda (return)
      (let ((found-gates (query-ids store "(and (field ?b \"slot\" \"gates\") (kind ?b doc) (under ?b \"root\"))")))
        (when (failed? found-gates) (return found-gates))
        (when (> (length found-gates) 1) (return (list 'error 'several-gates-documents (list 'ids found-gates))))
        (let ((gates (if (pair? found-gates)
                         (car found-gates)
                         (batch-made store '((insert root #f ((kind . doc) (title . "gates") (slot . "gates"))))))))
          (unless (string? gates) (return (list 'error 'gates-not-made gates)))
          (let ((found-note (query-ids store (format "(and (field ?b \"slot\" \"gates-note\") (under ?b ~s))" gates))))
            (when (failed? found-note) (return found-note))
            (when (> (length found-note) 1) (return (list 'error 'several-notes (list 'ids found-note))))
            (let* ((note (if (pair? found-note)
                             (car found-note)
                             (batch-made store (list (list 'insert gates #f
                                                           (list '(kind . section) '(title . "the two slots")
                                                                 '(slot . "gates-note") (cons 'src note-text)))))))
                   (_ (unless (string? note) (return (list 'error 'note-not-made note))))
                   (set (if (pair? found-note)
                            (rpc-dispatch store (list 'set note "src" note-text) "landing-gates")
                            '(ok made)))
                   (_ (unless (and (pair? set) (eq? (car set) 'ok)) (return (list 'error 'note-not-set set))))
                   (rule (rpc-dispatch store (list 'rule "landing-gate" "--on" "decision" "--where" rule-where "--must" rule-must)
                                       "landing-gates")))
              (unless (and (pair? rule) (eq? (car rule) 'ok)) (return (list 'error 'rule-not-declared rule)))
              (list 'landing-gates
                    (list 'gates gates (if (pair? found-gates) 'found 'made))
                    (list 'note note (if (pair? found-note) 'found 'made) set)
                    (list 'rule rule)))))))))

;; ---- the self-test -------------------------------------------------------------------------------

(define (self-test)
  (define bad 0)
  (define rows 0)
  (define (row name got expected)
    (set! rows (+ rows 1))
    (if (equal? got expected)
        (printf "ok   ~a\n" name)
        (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
  (define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/landing-gates-" (number->string (get-process-id))))
  (define store (string-append root "/store"))
  (define (run . args) (rpc-dispatch store args "self-test"))
  (define (guarded thunk) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) (thunk)))
  (define (part r k) (and (pair? r) (list? r) (assq k (cdr r))))
  (define (contains? text needle)
    (let ((n (string-length needle)) (m (string-length text)))
      (let loop ((i 0)) (cond ((> (+ i n) m) #f) ((string=? (substring text i (+ i n)) needle) #t) (else (loop (+ i 1)))))))
  ;; Whether a read of the note shows the note's text, as it is written in an answer.
  (define (note-has-text? id) (contains? (format "~s" (run 'read id)) (format "~s" note-text)))
  (system (string-append "rm -rf " (quoted root) "; mkdir -p " (quoted (string-append root "/home"))))
  (putenv "THEOURGIA_HOME" (string-append root "/home"))
  (run 'init)
  (let* ((first (guarded (lambda () (setup! store))))
         (gates (and (part first 'gates) (cadr (part first 'gates))))
         (note (and (part first 'note) (cadr (part first 'note))))
         (second (guarded (lambda () (setup! store))))
         (second-text (note-has-text? note))
         (_ (run 'set note "src" "an edited note\n"))
         (edited-text (note-has-text? note))
         (third (guarded (lambda () (setup! store))))
         (third-text (note-has-text? note)))
    (row "S-1 a fresh store: the gates document and the note are made, the rule declared"
         (list (and (part first 'gates) (caddr (part first 'gates))) (string? gates)
               (and (part first 'note) (caddr (part first 'note))) (string? note)
               (and (part first 'rule) (pair? (cadr (part first 'rule))) (car (cadr (part first 'rule)))))
         '(made #t made #t ok))
    (row "S-2 run again: the same document and note found, the note holding its text, the rule unchanged"
         (list (and (part second 'gates) (list (cadr (part second 'gates)) (caddr (part second 'gates))))
               (and (part second 'note) (list (cadr (part second 'note)) (caddr (part second 'note))))
               second-text
               (and (part second 'rule) (cadr (part second 'rule))))
         (list (list gates 'found) (list note 'found) #t '(ok (unchanged))))
    (row "S-3 the note edited (its text gone), then a run: the same note, its text restored"
         (list edited-text (and (part third 'note) (cadr (part third 'note))) third-text)
         (list #f note #t))
    (let* (;; A gate reading under gates. -> its id, or the refusal.
           (gate (lambda (cut result class slot)
                   (batch-made store (list (list 'insert gates #f
                                                 (list '(kind . section) '(title . "gate") (cons 'class class)
                                                       (cons 'slot slot) (cons 'cut cut) (cons 'result result)))))))
           ;; A landing on CUT in one batch with the verifies edge from GATE when
           ;; there is one. -> (ok ok), or (refused <the rules named>).
           (land (lambda (cut gate)
                   (let ((a (run 'batch (format "~s" (append (list (list 'insert 'root #f
                                                                         (list '(kind . decision) '(title . "landing")
                                                                               '(slot . "landing") (cons 'cut cut))))
                                                             (if gate (list (list 'link gate 'verifies '(from 0))) '()))))))
                     (cond ((and (pair? a) (eq? (car a) 'batch)) (map car (cadr a)))
                           ((and (list? a) (>= (length a) 4) (equal? (list-head a 3) '(error refused rule-violation)))
                            (list 'refused (map (lambda (f) (cadr (assq 'rule f))) (cdr (list-ref a 3)))))
                           (else a)))))
           (cut "((\"w\" . 1))")
           (other "((\"w\" . 2))")
           (g-green (gate cut "green" 'observation "gate"))
           (g-other (gate other "green" 'observation "gate"))
           (g-red (gate cut "(red some-fixture)" 'observation "gate"))
           (g-inference (gate cut "green" 'inference "gate"))
           (g-slot (gate cut "green" 'observation "note")))
      (row "S-4 PREMISE: every gate reading the rows below use was written"
           (map string? (list g-green g-other g-red g-inference g-slot))
           '(#t #t #t #t #t))
      (row "S-5 a landing verified by a green gate of the same cut is written"
           (land cut g-green)
           '(ok ok))
      (row "S-6 refused by landing-gate: a gate of another cut, a gate not green, no gate, a gate of class inference, a gate whose slot is not gate"
           (list (land cut g-other) (land cut g-red) (land cut #f) (land cut g-inference) (land cut g-slot))
           (make-list 5 '(refused (landing-gate)))))
    ;; Two gates documents are refused, not chosen between.
    (run 'batch "((insert root #f ((kind . doc) (title . \"gates\") (slot . \"gates\"))))")
    (row "S-7 a second gates document: the run is refused several-gates-documents"
         (let ((r (guarded (lambda () (setup! store))))) (and (pair? r) (list? r) (list-head r (min 2 (length r)))))
         '(error several-gates-documents)))
  (system (string-append "rm -rf " (quoted root)))
  (printf "\n~a failures\nrows: ~a\nlanding-gates self-test complete\n" bad rows)
  (exit (if (= bad 0) 0 1)))

;; ---- the command line ----------------------------------------------------------------------------

(define args (cdr (command-line)))
(cond
  ((and (pair? args) (string=? (car args) "--self-test")) (self-test))
  ((and (pair? args) (null? (cdr args)))
   (let ((r (setup! (car args))))
     (write r) (newline)
     (exit (if (and (pair? r) (eq? (car r) 'landing-gates)) 0 1))))
  (else
   (write '(usage "scheme --script test/tools/landing-gates.sc <store> | --self-test")) (newline)
   (exit 2)))
