#!r6rs
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

;; TWO AGENTS, ONE STORE: requests, results and reviews, kept honest by the
;; store's typed relations, its rules and the receipts its readers carry.
;;
;; A daemon serves a project-template store. Agent a asks (a request is a
;; decision under design) and answers (a result is a top-level doc, slot
;; "result", `for` naming the request, linked answers to it); agent b waits
;; for results on the change stream, reads one with context, and reviews it
;; (a top-level doc, slot "review", citing the result) carrying the
;; receipt. The store declares: answers (a result to a decision) and cites
;; (a review to a result), typed; review-cites, result-answers (state
;; rules); reviews-carry-receipt (a write rule); citation coverage, which the
;; project template declares. The rows walk it: a result made in two writes
;; is refused; b's subscription sees a result arrive; b's review is written
;; with its receipt; a refutes the result b read; b's revision on the old
;; receipt is refused; b reads again, finds the result to verify, and writes
;; with the new receipt; a citation b never read is refused; a review
;; answering a request is refused at its end.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (theourgia wire) sexpr->string-extended))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((text (call-with-input-file path get-string-all)))
        (if (string? text) text ""))))

;; ---- the daemon ---------------------------------------------------------------
;;
;; A DAEMON THAT DOES NOT COME UP IS NOT A READING: its socket is waited for,
;; and the file stops, saying so, when it never appears.
(define store (string-append scratch-base "/ps-" pid-text))
(define sock (string-append socket-base "/ps-" pid-text ".sock"))
(define runner (string-append scratch-base "/ps-" pid-text ".sc"))
(define daemon-log (string-append scratch-base "/ps-" pid-text ".log"))
(define (start-daemon!)
  (system (string-append "rm -rf '" store "' '" sock "'; mkdir -p '" store "'"))
  (call-with-output-file runner
    (lambda (port)
      (for-each (lambda (l) (display l port) (newline port))
        (list "(import (chezscheme) (theourgia daemon) (theourgia rpc))"
              (string-append "(rpc-dispatch \"" store "\" '(init \"--template\" \"project\") \"agent-a\")")
              (string-append "(serve \"" store "\" \"" sock "\")"))))
    'truncate)
  (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                         " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                         " sh -c 'scheme --script " runner " > " daemon-log " 2>&1' &"))
  (let up ((k 0))
    (cond ((file-exists? sock) #t)
          ((> k 400)
           (printf "NOT A READING: the daemon never created its socket in 20 s~%~a~%" (file-text daemon-log))
           (exit 2))
          (else (sleep-ms 50) (up (+ k 1))))))
(define (stop-daemon!) (system (string-append "pkill -f " runner " 2>/dev/null")))

;; ---- asking, as an agent ------------------------------------------------------

(define (line-complete? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))
(define (envelope actor verb args)
  (string-append "(request 1 " (format "~s" store) " " (format "~s" actor) " #f wire #f #f " (symbol->string verb)
                 (apply string-append (map (lambda (a) (string-append " " (format "~s" a))) args))
                 ")\n"))
(define (unwrap-answer text)
  (let ((datum (guard (e (#t #f)) (read (open-string-input-port text)))))
    (if (and (pair? datum) (eq? 'answer (car datum)))
        (let ((hit (assq 'stdout (cdr datum))))
          (if (and (pair? hit) (string? (cadr hit)))
              (let ((inner (guard (e (#t #f)) (read (open-string-input-port (cadr hit))))))
                (or inner (cadr hit)))
              datum))
        (list 'not-an-answer text))))
(define (ask actor verb . args)
  (let ((r (exchange sock (string->utf8 (envelope actor verb args)) line-complete? 30000)))
    (if (and (pair? r) (eq? 'answer (car r)))
        (unwrap-answer (utf8->string (cadr r)))
        (list 'transport r))))
(define (a . args) (apply ask "agent-a" args))
(define (b . args) (apply ask "agent-b" args))
(define (new-id answer)
  (let ((ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer)))))
    (and ev (pair? (cadr ev)) (let ((e (car (cadr ev)))) (string-append (car e) "." (number->string (cdr e)))))))
(define (item-ids answer) (if (and (pair? answer) (eq? (car answer) 'batch)) (map new-id (cadr answer)) '()))
(define (batch-of who intents . options) (apply who 'batch (format "~s" intents) options))
(define (head x n) (if (and (list? x) (>= (length x) n)) (list-head x n) x))
(define (clause datum name)
  (and (pair? datum) (list? datum) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr datum))))
(define (section answer name) (let ((c (clause answer name))) (and c (cadr c))))
(define (ids-in answer name) (let ((es (section answer name))) (if (list? es) (map car es) 'no-section)))
;; THE RECEIPT IS GIVEN BACK AS THE ANSWER PRINTS IT: the clause itself.
(define (receipt-text answer) (let ((r (clause answer 'receipt))) (if r (sexpr->string-extended r) "()")))

;; ---- b's subscription -----------------------------------------------------------
;;
;; A process holding its own connection: it sends `subscribe changes <rev>`
;; and keeps every line it reads; (lines <from>) asks it for them.
(define (spawn-subscriber! rev)
  (let ((text (envelope "agent-b" 'subscribe (list "changes" rev))))
    (spawn
      (lambda ()
        (connect! sock)
        (receive
          (after 4000 (let idle () (receive (`(lines ,from) (send from (list 'lines '(no-connect))) (idle)))))
          (`(connected ,p ,ref)
           (conn-read-start! ref)
           (conn-write! ref (string->utf8 text) 'subscribe)
           (let loop ((acc "") (done '()))
             (receive
               (`(written ,r ,t ,st) (loop acc done))
               (`(data ,r ,bv)
                (let split ((s (string-append acc (utf8->string bv))) (done done))
                  (let ((nl (let find ((i 0)) (cond ((>= i (string-length s)) #f)
                                                    ((char=? (string-ref s i) #\newline) i)
                                                    (else (find (+ i 1)))))))
                    (if nl
                        (split (substring s (+ nl 1) (string-length s)) (cons (substring s 0 nl) done))
                        (loop s done)))))
               (`(eof ,r) (loop acc done))
               (`#(DOWN ,w ,why) (loop acc done))
               (`(lines ,from) (send from (list 'lines (reverse done))) (loop acc done))
               (`(close) (conn-close! ref))))))))))
(define (sub-lines sub)
  (send sub (list 'lines self))
  (receive (after 3000 '()) (`(lines ,ls) ls)))
;; The frames after the acceptance that hold ITEM, waiting up to MS.
(define (await-item sub item ms)
  (let wait ((k 0))
    (let* ((ls (sub-lines sub))
           (frames (map (lambda (l) (guard (e (#t #f)) (read (open-string-input-port l)))) (if (pair? ls) (cdr ls) '())))
           (hit (exists (lambda (f) (let ((items (clause f 'items))) (and items (member item (cdr items)) #t))) frames)))
      (cond (hit #t)
            ((> k (div ms 50)) (list 'not-seen (length frames)))
            (else (sleep-ms 50) (wait (+ k 1)))))))

;; THE SCENARIO'S NAMES, assigned in order as it runs inside the scheduler.
(define init-ok #f)
(define design #f)
(define reqs #f)
(define R1 #f)
(define R2 #f)
(define sub #f)
(define made-1 #f)
(define S1 #f)
(define made-2 #f)
(define S2 #f)
(define read-1 #f)
(define review #f)
(define W #f)
(define read-2 #f)
(define read-3 #f)

(start-scheduler
  (lambda ()
    (start-daemon!)

    ;; ==== the store's declarations ====
    (set! init-ok
      (map car
           (list (a 'relation "answers" "--as" "implements" "--from" "(kind doc) (field slot \"result\")" "--to" "(kind decision)")
                 (a 'relation "cites" "--as" "depends-on" "--from" "(kind doc) (field slot \"review\")" "--to" "(kind doc) (field slot \"result\")")
                 (a 'rule "review-cites" "--on" "doc" "--where" "(field ?w \"slot\" \"review\")" "--must" "(edge ?w cites ?s)")
                 (a 'rule "result-answers" "--on" "doc" "--where" "(field ?w \"slot\" \"result\")"
                    "--must" "(and (edge ?w answers ?r) (field ?w \"for\" ?r))")
                 (a 'rule "reviews-carry-receipt" "--on" "doc" "--where" "(field+ ?w \"slot\" \"review\")" "--must" "(receipt-carried)"))))
    (want "P the store declares answers and cites, typed, and the three rules; coverage is the template's"
          (list init-ok (map car (cdr (or (clause (a 'describe) 'declared-rules) '(declared-rules)))))
          (list '(ok ok ok ok ok) '(cover result-answers review-cites reviews-carry-receipt)))

    ;; ==== a's requests ====
    ;; The template's design root, by its slug: requests are decisions under it.
    (set! design
      (let ((items (clause (a 'query "(field ?d \"slug\" \"design\")") 'items)))
        (and items (pair? (cdr items)) (cadr (cadr items)))))
    (set! reqs (item-ids (batch-of a (list (list 'insert design #f '((kind . decision) (title . "R1")))
                                             (list 'insert design #f '((kind . decision) (title . "R2")))))))
    (set! R1 (car reqs))
    (set! R2 (cadr reqs))

    ;; ==== b subscribes ====
    (set! sub (spawn-subscriber! "0"))
    (want "P b's subscription is accepted"
          (let wait ((k 0))
            (let ((ls (sub-lines sub)))
              (cond ((pair? ls) (car (unwrap-answer (car ls))))
                    ((> k 100) 'no-acceptance)
                    (else (sleep-ms 50) (wait (+ k 1))))))
          'ok)

    ;; ==== a's results ====
    (want "P a result made alone, its answers edge to follow, is refused: a result answers the request it is for"
          (head (batch-of a (list (list 'insert 'root #f (list '(kind . doc) '(title . "S0") '(slot . "result") (cons 'for R1)))))
                3)
          '(error refused rule-violation))
    (set! made-1 (batch-of a (list (list 'insert 'root #f (list '(kind . doc) '(title . "S1") '(slot . "result") (cons 'for R1)))
                                     (list 'link '(from 0) 'answers R1))))
    (set! S1 (car (item-ids made-1)))
    (set! made-2 (batch-of a (list (list 'insert 'root #f (list '(kind . doc) '(title . "S2") '(slot . "result") (cons 'for R2)))
                                     (list 'link '(from 0) 'answers R2))))
    (set! S2 (car (item-ids made-2)))
    (want "P a result made with its answers edge in one write is written"
          (list (map car (cadr made-1)) (map car (cadr made-2)))
          '((ok ok) (ok ok)))
    (want "P b's subscription sees the result arrive"
          (await-item sub (list 'added S1) 15000)
          #t)

    ;; ==== b reads and reviews ====
    (set! read-1 (b 'context "--for" S1 "--budget" "4000"))
    (want "P b reads the result with context: it is the answer's for, and a receipt comes with it"
          (list (car read-1) (car (section read-1 'for)) (and (clause read-1 'receipt) #t))
          (list 'ok S1 #t))
    (set! review (batch-of b (list (list 'insert 'root #f (list '(kind . doc) '(title . "W") '(slot . "review")))
                                     (list 'link '(from 0) 'cites S1))
                             "--premises" (receipt-text read-1)))
    (set! W (car (item-ids review)))
    (want "P b's review, made with its cites edge and the receipt in one write, is written"
          (map car (cadr review))
          '(ok ok))
    (want "P a review answering a request is refused at its end: answers runs from a result"
          (head (b 'link W "answers" R1) 3)
          '(error bad-request relation-endpoint))

    ;; ==== a refutes; b's old receipt no longer holds ====
    (want "P a refutes the result b read"
          (map car (cadr (batch-of a (list (list 'insert 'root #f '((kind . doc) (title . "Rebuttal")))
                                           (list 'link '(from 0) 'refutes S1)))))
          '(ok ok))
    (b 'write W "the review, revised" "--writer" "agent-b")
    (want "P b's revision on the old receipt is refused: what b read has changed"
          (head (b 'commit W "--writer" "agent-b" "--premises" (receipt-text read-1)) 2)
          '(error premise-changed))
    (set! read-2 (b 'context "--for" W "--budget" "4000"))
    (want "P b reads again: the result it cites is to verify"
          (and (member S1 (let ((ids (ids-in read-2 'to-verify))) (if (list? ids) ids '()))) #t)
          #t)
    (want "P b's revision on the new receipt is written"
          (car (b 'commit W "--writer" "agent-b" "--premises" (receipt-text read-2)))
          'ok)

    ;; ==== a citation b never read ====
    ;; Read again after the commit: the commit moved W's own version.
    (set! read-3 (b 'context "--for" W "--budget" "4000"))
    (want "P a citation of a result b's receipt does not hold is refused citation-not-read"
          (let ((r (b 'link W "cites" S2 "--premises" (receipt-text read-3))))
            (list (head r 3)
                  (let ((f (and (list? r) (>= (length r) 4) (cadr (list-ref r 3)))))
                    (and f (assq 'witness (cdr f))))))
          (list '(error refused rule-violation)
                (list 'witness (list 'citation-not-read (list 'block W) (list 'cites S2)))))

    ;; ==== check ====
    (want "P check: the state rules hold over the store, the write rules are skipped, the verdict ok"
          (let ((c (a 'check)))
            (list (map (lambda (r) (list (car r) (cadr r))) (cdr (or (clause c 'rules) '(rules))))
                  (cadr (or (clause c 'verdict) '(verdict none)))))
          (list '((rule-skipped (rule cover)) (rule-skipped (rule reviews-carry-receipt)))
                'ok))

    (send sub '(close))
    (stop-daemon!)
    (system (string-append "rm -rf '" store "' '" sock "' '" runner "' '" daemon-log "'"))
    (printf "rows: ~a~%~a failures~%paper-scenario complete~%" rows bad)
    (exit (if (= bad 0) 0 1))))
