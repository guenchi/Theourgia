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

;; THE COST OF VALIDITY OVER MANY EDGES, MEASURED. For N = 200, 400, 800: a
;; chain of N blocks, each depending on the next, the last edited after its
;; link, so every block needs review through the chain; the time to answer
;; every block's validity from one provider. A cost quadratic in the edges
;; shows as about x4 per doubling of N, a linear one as about x2. Run it with
;; CHEZSCHEMELIBDIRS naming the tree to measure, from any directory:
;;
;;     scheme --script test/probe/edge-cost.ss
(import (chezscheme)
        (only (theourgia reduce) reduce-empty reduce-apply! block-id)
        (prefix (theourgia lifecycle) lc:))
(define (chain n)
  (let ((r (reduce-empty)))
    (do ((i 1 (+ i 1))) ((> i n))
      (reduce-apply! r "a" i '() `(put ((kind . section) (title . ,(format "b~a" i))))))
    (do ((i 1 (+ i 1))) ((= i n))
      (reduce-apply! r "a" (+ n i) '() `(link ,(block-id "a" i) depends-on ,(block-id "a" (+ i 1)))))
    (reduce-apply! r "a" (* 2 n) '() `(set ,(block-id "a" n) title "edited"))
    r))
(for-each
  (lambda (n)
    (let* ((s (chain n)) (t0 (current-time 'time-monotonic))
           (L (lc:lifecycle s))
           (vs (map (lambda (i) (lc:validity-of L (block-id "a" (+ i 1)))) (iota n)))
           (d (time-difference (current-time 'time-monotonic) t0)))
      (printf "edges ~a: ~a ms; needs-review ~a\n" (- n 1)
              (+ (* 1000 (time-second d)) (quotient (time-nanosecond d) 1000000))
              (length (filter (lambda (v) (eq? (car v) 'needs-review)) vs)))))
  '(200 400 800))
