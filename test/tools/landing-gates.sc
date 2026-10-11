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
;; landing is judged by.
;;
;;   scheme --script test/tools/landing-gates.sc <store>
;;   scheme --script test/tools/landing-gates.sc --self-test
;;
;; It declares, in the store it is given (not in a template):
;;
;;   - a top-level document `gates` (slot "gates"), under which each gate's
;;     reading is written once, as a section of class observation with the
;;     fields slot "gate", cut (the cut literal, as text, in the one spelling
;;     `tag` answers), result ("green" or the list of what was not) and
;;     alternation;
;;   - a note under it on the two slots;
;;   - the rule landing-gate: a decision with slot "landing" must carry a cut
;;     and be verified by a gate reading of the same cut whose result is green.
;;     A landing is written in ONE batch with its verifies edge, so the rule
;;     judges the state the whole batch produces.
;;
;; IT CAN BE RUN AGAIN: the document and the note are found by their slots and
;; not made twice, and the rule declared again answers (ok (unchanged)). It
;; prints what it found or made:
;;
;;   (landing-gates (gates <id> made|found) (note <id> made|found) (rule <answer>))
;;
;; --self-test runs it twice on a scratch store and then writes landings the
;; rule must take and refuse, printing one row each.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) block-id))

(register-verbs! extension-verbs)

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

(define (new-ids answer)
  (let ((ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer)))))
    (if ev (map (lambda (e) (block-id (car e) (cdr e))) (cadr ev)) '())))
(define (batch-new-id store intents)
  (let ((a (rpc-dispatch store (list 'batch (format "~s" intents)) "landing-gates")))
    (and (pair? a) (eq? (car a) 'batch) (pair? (cadr a))
         (let ((ids (new-ids (car (cadr a))))) (and (pair? ids) (car ids))))))
;; The live blocks whose field slot is SLOT, as ids.
(define (with-slot store slot)
  (let* ((a (rpc-dispatch store (list 'query (format "(field ?b \"slot\" ~s)" slot)) "landing-gates"))
         (items (and (pair? a) (eq? (car a) 'ok) (find (lambda (c) (and (pair? c) (eq? (car c) 'items))) (cdr a)))))
    (if items (map cadr (filter (lambda (r) (and (pair? r) (eq? (car r) 'row))) (cdr items))) '())))

(define (setup! store)
  (let* ((found-gates (with-slot store "gates"))
         (gates (if (pair? found-gates)
                    (car found-gates)
                    (batch-new-id store '((insert root #f ((kind . doc) (title . "gates") (slot . "gates")))))))
         (found-note (with-slot store "gates-note"))
         (note (cond ((pair? found-note) (car found-note))
                     (gates (batch-new-id store (list (list 'insert gates #f
                                                            (list '(kind . section) '(title . "the two slots")
                                                                  '(slot . "gates-note") (cons 'src note-text))))))
                     (else #f)))
         (rule (rpc-dispatch store (list 'rule "landing-gate" "--on" "decision"
                                         "--where" "(field ?w \"slot\" \"landing\")" "--must" rule-must)
                             "landing-gates")))
    (list 'landing-gates
          (list 'gates gates (if (pair? found-gates) 'found 'made))
          (list 'note note (if (pair? found-note) 'found 'made))
          (list 'rule rule))))

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
  (system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
  (putenv "THEOURGIA_HOME" (string-append root "/home"))
  (run 'init)
  (let* ((first (guard (e (#t (list 'RAISED e))) (setup! store)))
         (second (guard (e (#t (list 'RAISED e))) (setup! store)))
         (part (lambda (r k) (and (pair? r) (assq k (cdr r))))))
    (row "S-1 a fresh store: the gates document and the note are made, the rule declared"
         (list (and (part first 'gates) (caddr (part first 'gates))) (and (part first 'note) (caddr (part first 'note)))
               (and (part first 'rule) (pair? (cadr (part first 'rule))) (car (cadr (part first 'rule)))))
         '(made made ok))
    (row "S-2 run again: the same gates document and note are found, the rule unchanged"
         (list (and (part second 'gates) (equal? (cadr (part second 'gates)) (cadr (part first 'gates))))
               (and (part second 'gates) (caddr (part second 'gates)))
               (and (part second 'note) (caddr (part second 'note)))
               (and (part second 'rule) (cadr (part second 'rule))))
         '(#t found found (ok (unchanged))))
    (let* ((gates (cadr (part first 'gates)))
           ;; A gate reading under gates, on CUT, with RESULT.
           (gate (lambda (cut result)
                   (batch-new-id store (list (list 'insert gates #f
                                                   (list '(kind . section) '(title . "gate") '(class . observation)
                                                         '(slot . "gate") (cons 'cut cut) (cons 'result result)))))))
           ;; A landing on CUT, in one batch with the verifies edge from GATE
           ;; when there is one. -> the head of the answer.
           (land (lambda (cut gate)
                   (let ((a (run 'batch (format "~s" (append (list (list 'insert 'root #f
                                                                         (list '(kind . decision) '(title . "landing")
                                                                               '(slot . "landing") (cons 'cut cut))))
                                                             (if gate (list (list 'link gate 'verifies '(from 0))) '()))))))
                     (cond ((and (pair? a) (eq? (car a) 'batch)) (map car (cadr a)))
                           ((and (pair? a) (list? a) (>= (length a) 3)) (list-head a 3))
                           (else a)))))
           (cut "((\"w\" . 1))"))
      (row "S-3 a landing verified by a green gate of the same cut is written"
           (land cut (gate cut "green"))
           '(ok ok))
      (row "S-4 a landing whose gate ran on another cut is refused"
           (land cut (gate "((\"w\" . 2))" "green"))
           '(error refused rule-violation))
      (row "S-5 a landing whose gate is not green is refused"
           (land cut (gate cut "(red some-fixture)"))
           '(error refused rule-violation))
      (row "S-6 a landing with no gate verifying it is refused"
           (land cut #f)
           '(error refused rule-violation))))
  (system (string-append "rm -rf '" root "'"))
  (printf "\n~a failures\nrows: ~a\nlanding-gates self-test complete\n" bad rows)
  (exit (if (= bad 0) 0 1)))

;; ---- the command line ----------------------------------------------------------------------------

(define args (cdr (command-line)))
(cond
  ((and (pair? args) (string=? (car args) "--self-test")) (self-test))
  ((and (pair? args) (null? (cdr args)))
   (let ((r (setup! (car args))))
     (write r) (newline)
     (exit (if (let ((rule (assq 'rule (cdr r)))) (and rule (pair? (cadr rule)) (eq? (car (cadr rule)) 'ok))) 0 1))))
  (else
   (write '(usage "scheme --script test/tools/landing-gates.sc <store> | --self-test")) (newline)
   (exit 2)))
