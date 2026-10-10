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

;; (theourgia sched) -- THE TIMEOUT CLAUSE IS NOT A CONSOLATION PRIZE.
;;
;; NEVER: "`(after 0 ...)` RETURNS IMMEDIATELY" IS NOT A TEST. An
;; implementation that took the timeout branch every time would pass it,
;; and so would one that dropped the mailbox on the floor. igropyr scans
;; the queued messages BEFORE it considers the timeout, and that order is
;; what these rows are about:
;;
;;   * a message already waiting beats `after 0`;
;;   * a message that matches no clause STAYS in the mailbox;
;;   * a message that arrives late still beats a timeout that is longer.
;;
;; This matters here because the facade re-exports `receive` as syntax.
;; A facade that wrapped it in a procedure would take the clause away
;; entirely -- there would be no timeout at all, and no row anywhere else
;; would notice.

(import (chezscheme) (theourgia sched))

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
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(start-scheduler
  (lambda ()
    (let ((main self))

      ;; ---- SC-01 a queued message beats `after 0` ----------------------
      (send main (list 'queued 'first))
      (want "SC-01 a message already in the mailbox wins over a zero timeout"
            (receive (after 0 'took-the-timeout)
                     (`(queued ,what) (list 'matched what)))
            '(matched first))

      ;; ---- SC-02 an unmatched message is kept, not consumed ------------
      ;;
      ;; KEY: THE ROW THAT CATCHES A MAILBOX BEING DRAINED. A `receive` that
      ;; threw away what it could not match would pass every row that only
      ;; asks whether the RIGHT message arrives.
      (send main (list 'unmatched 'keep-me))
      (send main (list 'wanted 'take-me))
      (want "SC-02 the wanted message is taken, past the one that does not match"
            (receive (after 500 'timeout)
                     (`(wanted ,what) (list 'matched what)))
            '(matched take-me))
      (want "SC-02 TWIN: and the unmatched one is still there afterwards"
            (receive (after 500 'gone)
                     (`(unmatched ,what) (list 'still-here what)))
            '(still-here keep-me))

      ;; ---- SC-03 a late message beats a longer timeout -----------------
      ;;
      ;; A second process sends after a delay; the timeout is longer than
      ;; the delay, so the message must win. An implementation that fired
      ;; the timeout regardless would answer 'timeout here and nowhere
      ;; else.
      (spawn (lambda () (sleep-ms 150) (send main (list 'late 'arrived))))
      (want "SC-03 a message that arrives during the wait beats the timeout"
            (receive (after 3000 'timeout)
                     (`(late ,what) (list 'matched what)))
            '(matched arrived))

      ;; ---- SC-04 and the timeout does fire when nothing comes ----------
      ;;
      ;; NEVER: WITHOUT THIS the three rows above are all satisfied by a
      ;; `receive` that never times out at all -- it would simply block,
      ;; and blocking looks like success until the day nothing arrives.
      (want "SC-04 TWIN: with an empty mailbox the timeout is what answers"
            (receive (after 100 'timed-out)
                     (`(nothing-sends-this ,x) 'wrong))
            'timed-out)

      ;; ---- SC-05 monitor: every ending is reported, with a name --------
      ;;
      ;; NEVER: `monitor` AND `link` ARE NOT THE SAME PROMISE, and both are
      ;; re-exported here with no row until now. A monitor reports EVERY
      ;; ending, normal or not, and the report carries WHICH process
      ;; ended -- a watcher of two children that could not tell them
      ;; apart would act on the wrong one.
      (let* ((quiet (spawn (lambda () (sleep-ms 400))))
             (quick (spawn (lambda () (sleep-ms 50)))))
        (monitor quiet)
        (monitor quick)
        (want "SC-05 a normal ending is reported, and says which process ended"
              (receive (after 3000 'no-report)
                       (`#(DOWN ,who ,why) (list (eq? who quick) (eq? who quiet))))
              '(#t #f))
        (want "SC-05 TWIN: and the other one is reported when ITS turn comes"
              (receive (after 3000 'no-report)
                       (`#(DOWN ,who ,why) (list (eq? who quiet) (eq? who quick))))
              '(#t #f)))

      ;; ---- SC-06 link: an abnormal death takes its partner with it -----
      ;;
      ;; The linked pair is built inside a process of its own, watched
      ;; from here, so that the cascade can be observed without this
      ;; process being the thing that dies.
      ;; NEVER: AND THE CHILD MUST BE A SECOND PROCESS, RUNNING. Measured by
      ;; the reviewer: with `spawn&link` replaced by
      ;; `(lambda (thunk) (thunk) (spawn (lambda () (if #f #f))))` --
      ;; which runs the body in the CALLER and links nothing -- every row
      ;; in this file still passed, because the raise then happened
      ;; inside the partner and the partner still went down. Both
      ;; processes announce themselves before anything is asked to die,
      ;; and the row requires them to be distinct and both alive.
      (let* ((partner
               (spawn (lambda ()
                        (let ((doomed (spawn&link (lambda ()
                                                    (send main (list 'kid-up self))
                                                    (receive
                                                      (after 5000 'nobody-said)
                                                      (`(raise-now) (raise 'on-purpose)))))))
                          (send main (list 'parent-up self))
                          (sleep-ms 5000)))))
             (ignored (monitor partner)))
        (let ((pair (let wait ((kid #f) (parent #f))
                      (if (and kid parent)
                          (cons kid parent)
                          (receive (after 4000 (cons kid parent))
                                   (`(kid-up ,k) (wait k parent))
                                   (`(parent-up ,p) (wait kid p)))))))
          (want "SC-06 spawn&link starts a SECOND process, and both are running"
                (let ((kid (car pair)) (parent (cdr pair)))
                  (and kid parent
                       (list (not (eq? kid parent))
                             (process-alive? kid)
                             (process-alive? parent)
                             (eq? parent partner))))
                '(#t #t #t #t))
          (when (car pair) (send (car pair) (list 'raise-now)))
          (want "SC-06 TWIN: and a partner that dies abnormally takes the linked one with it"
                (receive (after 4000 'still-alive)
                         (`#(DOWN ,who ,why) (if (eq? who partner) 'went-too 'someone-else)))
                'went-too)))

      ;; ---- SC-07 TWIN: a normal ending does NOT cascade ----------------
      ;;
      ;; NEVER: WITHOUT THIS TWIN, SC-06 is satisfied by a `link` that kills
      ;; its partner whenever it ends -- which would tear down half a
      ;; system every time a worker finished its work normally. The two
      ;; rows differ in ONE thing: how the linked process ends.
      (let* ((survivor
               (spawn (lambda ()
                        (let ((finisher (spawn&link (lambda ()
                                                      (send main (list 'finisher-up self))
                                                      (sleep-ms 100)))))
                          (let live ()
                            (receive (after 5000 'done)
                                     (`(ping ,from) (send from (list 'pong)) (live))))))))
             (ignored2 (monitor survivor)))
        ;; NOTE: THE SAME REQUIREMENT AS SC-06: the linked process has to be
        ;; a different one that really ran, or "ended normally" is only a
        ;; statement about the caller.
        (want "SC-07 the linked process is a second one, which ran"
              (receive (after 4000 'no-announcement)
                       (`(finisher-up ,f) (and (not (eq? f survivor)) 'a-second-process)))
              'a-second-process)
        (sleep-ms 600)
        (send survivor (list 'ping main))
        (want "SC-07 TWIN: a partner that ends normally leaves it alone"
              (receive (after 3000 'no-answer)
                       (`(pong) 'still-running)
                       (`#(DOWN ,who ,why) (if (eq? who survivor) 'died-too 'someone-else)))
              'still-running)
        ;; NOTE: AND `process-alive?` IS ASKED THE SAME QUESTION SEPARATELY,
        ;; because the answer above could also come from a pong sent by a
        ;; process that died immediately afterwards.
        (want "SC-07 and process-alive? agrees about the one that survived"
              (list (process-alive? survivor) (process-alive? main))
              '(#t #t)))

      ;; ---- SC-08 `link` itself, not `spawn&link` ------------------------
      ;;
      ;; NEVER: SC-06 AND SC-07 BOTH BUILD THEIR PAIR WITH `spawn&link`, which
      ;; igropyr implements through its own internal linking -- so
      ;; replacing this facade's `link` export with something that does
      ;; nothing would leave both of them green. This row links two
      ;; processes that already exist, which is the only way to reach the
      ;; exported name.
      ;; NEVER: AND THE CASCADE ALONE DOES NOT SAY THE LINK WORKED. Measured
      ;; by the reviewer, not by me: with `link` replaced by a definition
      ;; that raises, every row in this file still passed -- because a
      ;; partner that dies INSIDE `link` is also a partner that goes
      ;; down, and the DOWN says nothing about which of the two happened.
      ;; The first row below is what separates them: the partner
      ;; acknowledges after linking and is still running before anything
      ;; is asked to die.
      (let* ((partner (spawn (lambda ()
                               (receive
                                 (after 5000 'nobody-said)
                                 (`(link-to ,other ,ack)
                                  (link other)
                                  (send ack (list 'linked))
                                  (sleep-ms 5000))))))
             (doomed (spawn (lambda ()
                              (receive
                                (after 5000 'nobody-said)
                                (`(go) (raise 'on-purpose))))))
             (watched (monitor partner)))
        (send partner (list 'link-to doomed main))
        (want "SC-08 linking two live processes ends neither of them"
              (receive (after 3000 'no-acknowledgement)
                       (`(linked) (list 'linked (process-alive? partner)))
                       (`#(DOWN ,who ,why) (list 'went-down-while-linking (eq? who partner))))
              '(linked #t))
        ;; NEVER: AND A LINK THAT KILLS ON A NORMAL ENDING WOULD PASS BOTH
        ;; ROWS. SC-07 checks that only for `spawn&link`, which igropyr
        ;; implements by another route; this asks it of the exported
        ;; `link`, on a partner that ends the ordinary way.
        (let* ((quiet-partner
                 (spawn (lambda ()
                          (receive
                            (after 5000 'nobody-said)
                            (`(link-to ,other ,ack)
                             (link other)
                             (send ack (list 'linked-quietly))
                             (let live ()
                               (receive (after 5000 'done)
                                        (`(ping ,from) (send from (list 'pong)) (live)))))))))
               (finisher (spawn (lambda ()
                                  (receive (after 5000 'nobody-said)
                                           (`(go) (if #f #f))))))
               (watched3 (monitor quiet-partner)))
          (send quiet-partner (list 'link-to finisher main))
          (receive (after 3000 'no-acknowledgement) (`(linked-quietly) 'linked))
          (send finisher (list 'go))
          (sleep-ms 400)
          (send quiet-partner (list 'ping main))
          (want "SC-08 TWIN: a linked partner that ends normally leaves the other alone"
                (receive (after 3000 'no-answer)
                         (`(pong) 'still-running)
                         (`#(DOWN ,who ,why)
                          (if (eq? who quiet-partner) 'died-too 'someone-else)))
                'still-running))
        (send doomed (list 'go))
        (want "SC-08 TWIN: and then the linked partner goes down with it"
              (receive (after 4000 'still-alive)
                       (`#(DOWN ,who ,why) (if (eq? who partner) 'went-too 'someone-else)))
              'went-too))

      ;; ---- SC-09 TWIN: process-alive? about one that is NOT alive -------
      ;;
      ;; NEVER: SC-07 ASKS IT ONLY ABOUT PROCESSES THAT ARE RUNNING, and
      ;; `(define (process-alive? p) #t)` answers those correctly. The
      ;; question is only worth asking if the answer can be no.
      (let* ((brief (spawn (lambda () (if #f #f))))
             (watched2 (monitor brief)))
        (receive (after 3000 'no-report) (`#(DOWN ,who ,why) 'ended))
        (want "SC-09 TWIN: a process that has ended is not alive, and this one is"
              (list (process-alive? brief) (process-alive? main))
              '(#f #t)))

      (printf "rows: ~a\n~a failures\nfacade-sched complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
