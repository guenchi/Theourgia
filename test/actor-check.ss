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

(import (chezscheme) (theourgia wire))
(define (raises? t) (guard (e (#t #t)) (t) #f))
(printf "gensym actor refused:            ~a\n"
        (raises? (lambda () (encode-record 0 0 (list (gensym "a") 0 0 0 0) '() '()))))
;; THE ACTOR IS NOW TOO CONSTRAINED TO CARRY MOST OF WHAT THESE ROWS
;; USED TO OFFER IT. Every slot of the request actor has a shape -- a
;; name, a pair of writer and request id, an index or `single`/`plan`, a
;; fingerprint, an event id or #f, an event id -- so a gensym, a
;; character or a cyclic vector cannot be placed anywhere inside one
;; that would otherwise be accepted. They are still refused, and the
;; rows below say so; but they are now refused by SHAPE, and the reason
;; matters, because a row that passes for a different reason than its
;; name says is a row nobody is reading correctly.
;;
;; The protection those rows were written for has not gone anywhere: an
;; uninterned symbol or a cycle in the PAYLOAD is still refused, by
;; storable-encode, and that is where they are checked now.
(define (six . slots) slots)
(define ok-actor (six "agent:claude" (cons "w" "req-1") 0 "fp" (cons "w" 2) (cons "w" 3)))

(printf "CONTROL six-element actor accepted: ~s
"
        (decode-line (encode-record 0 0 ok-actor '() '(a))))
(printf "CONTROL 'single and a #f plan:   ~s
"
        (decode-line (encode-record 0 0 (six "agent:claude" (cons "w" "req-1") 'single "fp" #f (cons "w" 3))
                                   '() '(a))))
(printf "CONTROL batch item identity:     ~s
"
        (decode-line (encode-record 0 0 (six "a" (cons "w" (list 'batch "b1" 2)) 2 "fp" (cons "w" 1) (cons "w" 3))
                                   '() '(a))))
(printf "CONTROL #%-prefixed name accepted: ~s
"
        (decode-line (encode-record 0 0 (six "#%alice" (cons "w" "r") 0 "f" #f (cons "w" 1)) '() '(a))))

(printf "old five-element actor refused:  ~a
"
        (raises? (lambda () (encode-record 0 0 (list "agent:claude" "req-1" 0 "fp" (cons "w" 3)) '() '(a)))))
;; SIX SLOTS IN THE WRONG ORDER. A length check accepts this; it is the
;; shape of the actor as it was before `plan-event-id` was inserted, with
;; one field added at the end, and every field after the first is
;; therefore reading the one beside it.
(printf "six elements shifted by one refused: ~a
"
        (raises? (lambda () (encode-record 0 0 (list "agent:claude" "req-1" 'single "fp" #f (cons "w" 3)) '() '(a)))))
(printf "identity that is not a pair refused: ~a
"
        (raises? (lambda () (encode-record 0 0 (six "a" "req-1" 0 "f" #f (cons "w" 1)) '() '(a)))))
(printf "sub outside index/single/plan refused: ~a
"
        (raises? (lambda () (encode-record 0 0 (six "a" (cons "w" "r") 'other "f" #f (cons "w" 1)) '() '(a)))))
(printf "character in a slot refused:     ~a
"
        (raises? (lambda () (encode-record 0 0 (six "a" (cons "w" "r") #\a "f" #f (cons "w" 1)) '() '(a)))))
(printf "gensym in a slot refused:        ~a
"
        (raises? (lambda () (encode-record 0 0 (six "a" (cons "w" "r") 0 "f" (cons (gensym "g") 3) (cons "w" 1)) '() '(a)))))

;; WHERE THOSE PROTECTIONS STILL BITE: the payload, which is not shaped
;; by the format and so can be handed anything at all.
(printf "gensym in the payload refused:   ~a
"
        (raises? (lambda () (storable-encode (list 'put (gensym "g"))))))
(printf "cyclic payload refused:          ~a
"
        (raises? (lambda ()
          (let ((x (list 'a 'b))) (set-cdr! (cdr x) x) (storable-encode x)))))
(printf "shared acyclic payload accepted: ~a
"
        (not (raises? (lambda ()
          (let ((shared (list "s"))) (storable-encode (list shared shared)))))))
(printf "CONTROL string actor accepted:   ~a
"
        (not (raises? (lambda () (encode-record 0 0 "agent:claude" '() '(a))))))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "actor-check complete\n")
