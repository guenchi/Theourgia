#!chezscheme
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

;; The publication point and the delivery boundary. Every row here exists
;; because a review found something the forty rows of log6 could not see:
;; they observe what delivery YIELDS, and these observe when it must
;; REFUSE, who may end the transaction, and what happens to the lock when
;; a callback does not return normally.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace) (theourgia crc32))
(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/log7work")
(define A "k3m9x2qa")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (damage-crc! bv)
  ;; The seq field is inside the checksummed body, so flipping a digit
  ;; there makes the record's own CRC wrong while the frame stays intact.
  (bytevector-u8-set! bv 3 (if (= 48 (bytevector-u8-ref bv 3)) 49 48))
  bv)
(define (build!)
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" A " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n")))
(define (seg! n . rs) (put! (string-append d "/writers/" A "/00000" (number->string n) ".sexp") (apply cat rs)))
(define (collect ls cut)
  (let ((seen '()))
    (let ((r (load-deliver! ls cut
               (lambda (w seg off seq ts actor deps payload)
                 (set! seen (cons seq seen))))))
      (list r (reverse seen)))))

(printf "== an error inside a segment stops the extent AT the last good record ==\n")
;; The torn branch was fixed for exactly this and its sibling was left
;; standing: record 1 is CRC-valid and contiguous, and reporting extent 0
;; threw it away. Pre-fix reading: end-seq 0 with ranges ().
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())) (damage-crc! (rec 2 '() '(put "a.2" ())))
        (rec 3 '() '(put "a.3" ())))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the record before the damage is still history" (discovery-end-seq p) 1)
  (want "and its segment range says so" (discovery-segment-ranges p) '((1 1 1)))
  (want "the error is retained, not swallowed"
        (map log-error-kind (discovery-integrity p)) '(crc))
  (want "delivery yields that record and the load stands"
        (list (collect ls '()) (load-outcome ls))
        (list (list 'delivered '(1)) 'open))
  (load-commit! ls))

(printf "== delivery must REACH the discovered boundary, not merely not raise ==\n")
;; The whole point of calling the scanner is its verdict. Damaged bytes in
;; a sealed segment, arriving after discovery, complete without raising
;; and deliver less than validation promised.
;; Pre-fix reading: (delivered (1)) and outcome committed -- record 2 was
;; skipped in silence and the load reported success.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(seg! 2 (rec 2 '() '(put "a.2" ())))
(seg! 3 (rec 3 '() '(put "a.3" ())))
(let ((ls (log-open d)))
  (want "discovery saw all three" (discovery-end-seq (load-prefix ls A)) 3)
  ;; sealed, and NOT the current segment, which is retained in memory
  (seg! 2 (damage-crc! (rec 2 '() '(put "a.2" ()))))
  (want "delivery refuses to call a short delivery a success"
        (collect ls '()) (list (list 'delivery-failed A) '(1)))
  (want "and the load is aborted"
        (let ((o (load-outcome ls))) (if (pair? o) (car o) o)) 'aborted))

(printf "== a reducer's return value may not truncate delivery ==\n")
;; The scanner stops on 'stop. A reducer that happens to return that
;; symbol would end the segment early -- and the scan COMPLETES, so it
;; reads as success. Pre-fix reading: (delivered (1)).
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())) (rec 2 '() '(put "a.2" ()))
        (rec 3 '() '(put "a.3" ())))
(let* ((ls (log-open d)) (seen '()))
  (let ((r (load-deliver! ls '()
             (lambda (w seg off seq ts actor deps payload)
               (set! seen (cons seq seen))
               'stop))))
    (want "every record in the extent still arrives" (list r (reverse seen))
          (list 'delivered '(1 2 3))))
  (load-commit! ls))

(printf "== the transaction boundary is enforced, not assumed ==\n")
;; Pre-fix reading: the commit SUCCEEDED and returned the staging state,
;; releasing the load's shared lock while the remaining segments were
;; still being read against files a writer was then free to replace.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(seg! 2 (rec 2 '() '(put "a.2" ())))
(let* ((ls (log-open d)) (inner 'never-ran))
  (guard (e (#t (want "committing from inside a delivery callback is refused"
                      (condition-violation? e) #t)))
    (load-deliver! ls '()
      (lambda (w seg off seq ts actor deps payload)
        (set! inner (guard (e (#t 'refused)) (load-commit! ls) 'committed))))
    (want "the inner commit did not take" inner 'refused))
  ;; A REFUSED INNER COMMIT DOES NOT KILL THE LOAD. My first version of
  ;; this row expected 'aborted and read 'open. The refusal is raised at
  ;; the callback, which caught it and returned normally, so delivery
  ;; finished -- and it is right that a rejected attempt to end the
  ;; transaction early leaves the transaction where it was. What the row
  ;; has to establish is that the publication point still belongs to the
  ;; owner afterwards.
  (want "the load is left open, not ended by the attempt"
        (load-outcome ls) 'open)
  (want "and its owner can still commit it"
        (begin (load-commit! ls) (load-outcome ls)) 'committed))

;; Pre-fix reading: 'committed -- a later commit overwrote the abort,
;; turning a discarded load into a published one.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(let ((ls (log-open d)))
  (load-abort! ls 'because)
  (want "a commit cannot overwrite an abort"
        (guard (e (#t 'refused)) (load-commit! ls) 'committed) 'refused)
  (want "and the outcome is still the abort"
        (load-outcome ls) '(aborted because)))

(printf "== an escape out of delivery aborts and releases the lock ==\n")
;; Pre-fix reading: outcome 'open with 0 unlock events -- the reader had
;; given up while an exclusive writer would still wait on its lock.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(seg! 2 (rec 2 '() '(put "a.2" ())))
(let ((p (open-output-string)) (ls #f))
  (parameterize ((current-error-port p))
    (trace-enable! #t)
    (set! ls (log-open d))
    (guard (e (#t (if #f #f)))
      (load-deliver! ls '()
        (lambda (w seg off seq ts actor deps payload)
          (raise 'the-reducer-gave-up))))
    (trace-enable! #f))
  (let ((s (get-output-string p)))
    (define (count-of needle)
      (let ((n (string-length s)) (m (string-length needle)))
        (let loop ((i 0) (k 0))
          (cond ((> (+ i m) n) k)
                ((string=? (substring s i (+ i m)) needle) (loop (+ i 1) (+ k 1)))
                (else (loop (+ i 1) k))))))
    (want "the lock is released on the way out" (count-of "(trace unlock") 1))
  (want "and the load is aborted, naming the escape"
        (load-outcome ls) '(aborted delivery-escaped)))

(printf "\n~a failures\n" bad)
(printf "log7 complete\n")
