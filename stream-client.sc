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
                answer-limit join-bytes)
          (only (theourgia ffi)
                unix-socket-connect fd-read write-all! socket-receive-timeout!
                theourgia-read-chunk fs-error? fs-error-errno wall-clock-ms trace-event!)
          (only (theourgia platform-numbers) platform-number))

  (define EINVAL (platform-number 'EINVAL))

  ;; ---- a stream: one request, then lines until the connection ends ----------
  ;;
  ;; THE ONE CALL THAT KEEPS ITS CONNECTION. The frame is sent as call! sends
  ;; it; then every line that arrives is handed to `line!` (its bytes, no
  ;; newline) as soon as it is whole, and the answer is how the stream
  ;; ended:
  ;;   (no-daemon <errno>) or (not-sent <answer>)  as call! says them;
  ;;   (ended eof)               the connection closed at a line's end;
  ;;   (ended lost-stream)       it closed in mid-line, a read failed, or a
  ;;                             partial line waited past its budget;
  ;;   (ended answer-too-large)  one line passed the answer limit.
  ;; NO IDLE DEADLINE: a stream idles legitimately, so the receive timeout
  ;; that connecting installs is taken off once the frame is sent. A PARTIAL
  ;; LINE HAS A BUDGET, the daemon's frame budget: once bytes of a line have
  ;; arrived, the rest must arrive within it, and each read is bounded by
  ;; what is left of it (a receive timeout, which the platform table
  ;; measures). The answer limit applies to each line, never to the stream.
  ;;
  ;; THEOURGIA_READ_CHUNK (the injection build only) bounds each read, so a
  ;; row can cut lines where it likes; every read is traced as
  ;; (trace read-chunk <bytes> <whole lines> <tail?>).
  (define stream-frame-ms 5000)
  (define (stream! path frame line!)
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
                    (unix-socket-connect path 30000))))
          (if (pair? fd)
              fd
              (let ((outcome (guard (e ((fs-error? e) (list 'ended 'lost-stream)))
                               (let* ((sent 0)
                                      (unsent (guard (e ((and (fs-error? e) (= sent 0))
                                                         (list 'not-sent
                                                               (list 'error 'write-failed
                                                                     (list 'path path)
                                                                     (list 'errno (fs-error-errno e))))))
                                                (write-all! fd frame path (lambda (n) (set! sent n)))
                                                #f)))
                                 (or unsent (stream-lines fd line!))))))
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
  (define (stream-lines fd line!)
    (let ((want (or (theourgia-read-chunk) 65536)))
      (bound-receive! fd 0)
      (let loop ((parts '()) (held 0) (deadline #f))
        (when deadline
          (bound-receive! fd (max 1 (- deadline (wall-clock-ms)))))
        (let ((chunk (if (and deadline (>= (wall-clock-ms) deadline))
                         'expired
                         (guard (e ((fs-error? e) 'failed)) (fd-read fd want)))))
          (cond
            ((symbol? chunk) (list 'ended 'lost-stream))
            ((zero? (bytevector-length chunk))
             (list 'ended (if (zero? held) 'eof 'lost-stream)))
            (else
             (let split ((from 0) (parts parts) (held held) (whole 0))
               (let ((nl (newline-from chunk from)))
                 (cond
                   ((and nl (> (+ held (- nl from)) (answer-limit)))
                    (list 'ended 'answer-too-large))
                   (nl
                    (line! (join-bytes (reverse (cons (sub-bytes chunk from nl) parts)) (+ held (- nl from))))
                    (split (+ nl 1) '() 0 (+ whole 1)))
                   (else
                    (let* ((rest (- (bytevector-length chunk) from))
                           (parts (if (zero? rest) parts (cons (sub-bytes chunk from (bytevector-length chunk)) parts)))
                           (now-held (+ held rest)))
                      (trace-event! 'read-chunk (bytevector-length chunk)
                                    (string-append (number->string whole) " "
                                                   (if (zero? now-held) "#f" "#t")))
                      (cond
                        ((> now-held (answer-limit)) (list 'ended 'answer-too-large))
                        ((zero? now-held)
                         (when deadline (bound-receive! fd 0))
                         (loop '() 0 #f))
                        (else
                         ;; the budget runs from the first byte of the line
                         ;; still partial: kept while the same line goes on,
                         ;; started afresh when this read began a new one
                         (loop parts now-held
                               (if (and deadline (zero? whole))
                                   deadline
                                   (+ (wall-clock-ms) stream-frame-ms))))))))))))))))

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
