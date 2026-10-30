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

;; THE STREAMING CLIENT: ONE REQUEST, THEN LINES UNTIL THE CONNECTION ENDS.
;;
;; THIS IS ITS OWN LIBRARY because only a stream verb needs it: the command
;; line enters it for `subscribe` and for nothing else, so every other call
;; starts without it. It is (theourgia client)'s transport kept open: the
;; connect, the frame and the outcomes are that library's, asked here.

(library (theourgia stream-client)
  (export stream!)
  (import (rnrs)
          (only (theourgia client)
                socket-path-fits? path-too-long no-daemon-errno? close-noting-failure
                answer-limit join-bytes silence-window-ms old-version-refusal-of? frame-at-version-1)
          (only (theourgia ffi)
                unix-socket-connect fd-read socket-receive-timeout!
                theourgia-read-chunk fs-error? fs-error-errno wall-clock-ms trace-event!
                monotonic-ms socket-send-within! socket-read-within socket-expired?)
          (only (theourgia platform-numbers) platform-number))

  (define EINVAL (platform-number 'EINVAL))

  ;; ---- a stream: one request, then lines until the connection ends ----------
  ;;
  ;; THE ONE CALL THAT KEEPS ITS CONNECTION. The frame is sent as call! sends
  ;; it, under the silence window; then every line that arrives is handed to
  ;; `line!` (its bytes, no newline) as soon as it is whole, and the answer is
  ;; how the stream ended:
  ;;   (no-daemon <errno>) or (not-sent <answer>)  as call! says them;
  ;;   (transport-error silent (window W) (elapsed ms))
  ;;                             nothing moved for W while the frame was going
  ;;                             out or before the acceptance line was whole;
  ;;   (ended eof)               the connection closed at a line's end;
  ;;   (ended lost-stream)       it closed in mid-line, a read failed, or a
  ;;                             partial line after the acceptance waited
  ;;                             past its budget;
  ;;   (ended answer-too-large)  one line passed the answer limit.
  ;; UNTIL THE ACCEPTANCE LINE IS WHOLE, THE WINDOW: every read, a partial
  ;; line's included, is bounded by what is left of W since the last byte.
  ;; AFTER IT, NO IDLE DEADLINE: a stream idles legitimately. A PARTIAL LINE
  ;; AFTER THE ACCEPTANCE HAS A BUDGET, the daemon's frame budget: once bytes
  ;; of a line have arrived, the rest must arrive within it, and each read is
  ;; bounded by what is left of it (a receive timeout, which the platform
  ;; table measures). The answer limit applies to each line, never to the
  ;; stream.
  ;;
  ;; THEOURGIA_READ_CHUNK (the injection build only) bounds each read, so a
  ;; row can cut lines where it likes; every read is traced as
  ;; (trace read-chunk <bytes> <whole lines> <tail?>).
  (define stream-frame-ms 5000)
  ;; A DAEMON THAT PREDATES VERSION 2 refuses a version-2 subscribe with
  ;; exactly today's bytes and closes; then the subscribe is sent once more
  ;; at version 1 on a new connection, nothing else changed, and the stream is
  ;; today's. The recogniser is call!'s (old-version-refusal-of?); this
  ;; transport adds that no space came before the line and that the
  ;; connection ended after it, under the window. A refusal held back that
  ;; way and followed by anything is handed on as the first line after all.
  (define (stream! path frame line!)
    (let ((first (stream-on path frame line! (lambda (bytes) (old-version-refusal-of? bytes frame)))))
      (if (equal? first '(old-version-refused))
          (begin
            (trace-event! 'version-retry 1 #f)
            (stream-on path (frame-at-version-1 frame) line! #f))
          first)))
  (define (stream-on path frame line! refused?)
    (if (not (socket-path-fits? path))
        (list 'not-sent (path-too-long path))
        (let ((fd (guard (e ((fs-error? e)
                             (let ((code (fs-error-errno e)))
                               (if (no-daemon-errno? code)
                                   (list 'no-daemon code)
                                   (list 'not-sent
                                         (list 'error 'connect-failed
                                               (list 'path path)
                                               (list 'errno code)))))))
                    (unix-socket-connect path silence-window-ms))))
          (if (pair? fd)
              fd
              ;; THE SUBSCRIBE IS SENT AS A REQUEST IS (client.sc exchange-on):
              ;; under the window, moved by every byte written; nothing written
              ;; when it runs out is not-sent, part of the frame written is silent.
              (let ((outcome (guard (e ((fs-error? e) (list 'ended 'lost-stream)))
                               (let* ((start (monotonic-ms))
                                      (last start)
                                      (sent 0)
                                      (left (lambda () (- (+ last silence-window-ms) (monotonic-ms))))
                                      (unsent (guard (e ((and (fs-error? e) (= sent 0))
                                                         (list 'not-sent
                                                               (list 'error 'write-failed
                                                                     (list 'path path)
                                                                     (list 'errno (fs-error-errno e)))))
                                                        ((socket-expired? e) (silent-since start)))
                                                (socket-send-within! fd frame left
                                                                     (lambda (n) (set! sent n) (set! last (monotonic-ms))))
                                                #f)))
                                 (or unsent (stream-lines fd line! start last refused?))))))
                (close-noting-failure fd)
                outcome)))))

  ;; ONE FAILURE OF THE RECEIVE BOUND IS NOT AN END. Measured on macOS:
  ;; setsockopt on a socket whose peer has already closed answers EINVAL, and
  ;; raising there ended a stream as lost-stream with its terminal line
  ;; already received and unread. A peer that has closed cannot leave a read
  ;; blocked -- what it sent is read, then EOF -- so that failure is passed
  ;; over. ANY OTHER FAILURE RAISES, and the stream ends lost-stream: a bound
  ;; that could not be set on a live peer would leave a read unbounded, and
  ;; one that could not be cleared would end an idle stream later anyway.
  (define (bound-receive! fd ms)
    (guard (e ((and (fs-error? e) (eqv? (fs-error-errno e) EINVAL)) #f))
      (socket-receive-timeout! fd ms)))

  ;; THE PARTIAL LINE IS KEPT AS ITS PARTS, newest first, and joined once,
  ;; when its newline arrives: re-joining it on every read made a line of
  ;; tens of megabytes cost the square of its size, and the budget ran out
  ;; before the limit was ever judged (a 33 MiB line answered lost-stream,
  ;; not answer-too-large).
  ;; BEFORE THE ACCEPTANCE, THE WINDOW. Until the first line (the
  ;; acceptance) is complete, every read, a partial line's included, is under
  ;; the client's silence window, counted from the frame's last byte sent and
  ;; moved by every byte; a daemon silent that long answers
  ;; (transport-error silent (window W) (elapsed ms)), as a request's would.
  ;; Byte 32 before the acceptance's first other byte is stripped, as a
  ;; request client strips keepalives: no daemon sends them on a
  ;; subscription, and a stray one is tolerated; nothing is stripped once a
  ;; line is held back as the old refusal. From the first byte after the
  ;; acceptance on: the partial-line budget, and no idle deadline.
  ;; -> (transport-error silent (window W) (elapsed ms)), ELAPSED from START,
  ;; the moment the subscribe's first byte was offered.
  (define (silent-since start)
    (list 'transport-error 'silent (list 'window silence-window-ms)
          (list 'elapsed (- (monotonic-ms) start))))
  (define (stream-lines fd line! start sent-at refused?)
    (let ((want (or (theourgia-read-chunk) 65536)) (accepted #f) (last sent-at)
          (spaced #f) (withheld #f))
      ;; a line held back as the old refusal goes on to LINE! before anything else does
      (define (flush!) (when withheld (let ((w withheld)) (set! withheld #f) (line! w))))
      (define (ending outcome) (flush!) outcome)
      (bound-receive! fd 0)
      (let loop ((parts '()) (held 0) (deadline #f))
        (when deadline
          (bound-receive! fd (max 1 (- deadline (wall-clock-ms)))))
        (let ((chunk (cond
                       ((and deadline (>= (wall-clock-ms) deadline)) 'expired)
                       ((not accepted)
                        (guard (e ((socket-expired? e) 'silent) ((fs-error? e) 'failed))
                          (let ((c (socket-read-within fd want (lambda () (- (+ last silence-window-ms) (monotonic-ms))))))
                            (bound-receive! fd 0)
                            (set! last (monotonic-ms))
                            ;; nothing is stripped once a line is held back as the
                            ;; old refusal: a byte after it, a space included, hands
                            ;; the refusal on and ends the retry
                            (let ((skip (if (and (null? parts) (zero? held) (not withheld)) (leading-spaces c) 0)))
                              (cond
                                ((zero? skip) c)
                                (else
                                 (set! spaced #t)
                                 (trace-event! 'keepalive-stripped skip #f)
                                 (if (= skip (bytevector-length c))
                                     'spaces
                                     (sub-bytes c skip (bytevector-length c)))))))))
                       (else (guard (e ((fs-error? e) 'failed)) (fd-read fd want))))))
          (cond
            ((eq? chunk 'spaces) (loop parts held deadline))
            ((eq? chunk 'silent) (ending (silent-since start)))
            ((symbol? chunk) (ending (list 'ended 'lost-stream)))
            ((and (zero? (bytevector-length chunk)) withheld (zero? held))
             '(old-version-refused))
            ((zero? (bytevector-length chunk))
             (ending (list 'ended (if (zero? held) 'eof 'lost-stream))))
            (else
             (let split ((from 0) (parts parts) (held held) (whole 0))
               (let ((nl (newline-from chunk from)))
                 (cond
                   ((and nl (> (+ held (- nl from)) (answer-limit)))
                    (ending (list 'ended 'answer-too-large)))
                   (nl
                    (let ((line (join-bytes (reverse (cons (sub-bytes chunk from nl) parts)) (+ held (- nl from)))))
                      (if (and refused? (not accepted) (not withheld) (not spaced)
                               (refused? (join-bytes (list line (make-bytevector 1 10)) (+ 1 (bytevector-length line)))))
                          (set! withheld line)
                          (begin
                            (set! accepted #t)
                            (flush!)
                            (line! line))))
                    (split (+ nl 1) '() 0 (+ whole 1)))
                   (else
                    (let* ((rest (- (bytevector-length chunk) from))
                           (parts (if (zero? rest) parts (cons (sub-bytes chunk from (bytevector-length chunk)) parts)))
                           (now-held (+ held rest)))
                      (trace-event! 'read-chunk (bytevector-length chunk)
                                    (string-append (number->string whole) " "
                                                   (if (zero? now-held) "#f" "#t")))
                      (cond
                        ((> now-held (answer-limit)) (ending (list 'ended 'answer-too-large)))
                        ((zero? now-held)
                         (when deadline (bound-receive! fd 0))
                         (loop '() 0 #f))
                        ;; BEFORE THE ACCEPTANCE LINE IS IN, a partial line is
                        ;; read on under the window, not the partial-frame budget
                        ((not accepted) (loop parts now-held #f))
                        (else
                         ;; the budget runs from the first byte of the line
                         ;; still partial: kept while the same line goes on,
                         ;; started afresh when this read began a new one
                         (loop parts now-held
                               (if (and deadline (zero? whole))
                                   deadline
                                   (+ (wall-clock-ms) stream-frame-ms))))))))))))))))

  (define (leading-spaces bv)
    (let loop ((i 0))
      (if (and (< i (bytevector-length bv)) (= 32 (bytevector-u8-ref bv i))) (loop (+ i 1)) i)))
  (define (newline-from bv from)
    (let loop ((i from))
      (cond ((>= i (bytevector-length bv)) #f)
            ((= (bytevector-u8-ref bv i) 10) i)
            (else (loop (+ i 1))))))
  (define (sub-bytes bv from to)
    (let ((out (make-bytevector (- to from))))
      (bytevector-copy! bv from out 0 (- to from))
      out))
)
