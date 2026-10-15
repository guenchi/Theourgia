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

;;; (theourgia trace) -- the one switch behind THEOURGIA_TRACE.
;;;
;;; WHY THIS IS ITS OWN LIBRARY AND NOT PART OF THE FFI. Both the
;;; syscall layer and the record codec report events, and the tests
;;; assert their interleaving -- so there has to be exactly one switch,
;;; or a run can have half a trace and read like a run with no trace at
;;; all. Putting it in (theourgia ffi) would have made the codec import
;;; the FFI, and the codec is the layer that gets retargeted: on a host
;;; where the records are read by compiled wasm there is no libc to
;;; load, and a dependency taken for one procedure would have to be
;;; unpicked there. Thirty lines with no dependencies is the cheaper
;;; shape.
;;;
;;; ONE LINE PER EVENT, ON STDERR:
;;;
;;;   (trace <op> <subject> <bytes>)
;;;
;;; <bytes> is #f for the operations that move none. A zero would read
;;; as "moved nothing", which is a different fact from "this operation
;;; does not move bytes", and a test asserting a byte total should not
;;; have to know which of the two it is looking at.

(library (theourgia trace)
  (export trace-enabled? trace-enable! trace-event! trace-line-sink trace-line-sink-set!)
  (import (rnrs base) (rnrs control) (rnrs io ports)
          (rnrs io simple) (rnrs exceptions) (rnrs unicode))

  ;; THE SWITCH IS INJECTED, NOT READ. Neither getenv nor a parameter is
  ;; R6RS, and this library is the one place the whole tree's tracing
  ;; passes through -- so it takes neither. The platform layer reads the
  ;; environment once at load and calls trace-enable!; a test calls it
  ;; directly. That keeps this file portable to a host with no
  ;; environment at all, which is the point of separating it from the
  ;; FFI in the first place.
  ;; A one-slot VECTOR, not a pair: R6RS pairs are immutable and
  ;; set-car! lives in (rnrs mutable-pairs), which is exactly the kind of
  ;; extra dependency this file exists to avoid. Vectors are mutable in
  ;; plain R6RS.
  (define enabled (vector #f))

  (define (trace-enabled?) (vector-ref enabled 0))

  (define (trace-enable! on?) (vector-set! enabled 0 (and on? #t)))

  ;; FLUSHED ON EVERY LINE, because the sequence is the thing being
  ;; asserted and two processes writing an unflushed stderr interleave
  ;; by buffer boundary rather than by event order.
  ;;
  ;; A FAILURE TO REPORT IS SWALLOWED. Tracing must never be able to
  ;; turn a write that worked into a write that raised: that would make
  ;; the traced run and the untraced run different programs, and the
  ;; traced one is the one being believed.
  ;; DISPLAY, NOT WRITE. R6RS has no fprintf, so the line is assembled
  ;; by hand -- and the first version reached for `write`, which quotes
  ;; strings: every path in the trace gained a pair of quotes and every
  ;; assertion that matched a bare path stopped matching. The format is
  ;; part of this library's interface because tests assert it, so the
  ;; replacement has to reproduce it exactly, not merely produce
  ;; something reasonable.
  (define (write-field p x) (display x p))

  ;; ONE LINE IS ONE STRING, HANDED OVER WHOLE (F298). The line used to be
  ;; written in seven pieces -- four put-strings and three displays -- with a
  ;; flush after the last, and every actor of
  ;; a process writes to the same port: an actor preempted between two
  ;; pieces -- or inside one, a display of 76 was seen split between its
  ;; digits -- had its half-line pushed out by the next actor's flush, and
  ;; a reader of the log then took the rest of the log as part of one
  ;; datum. So the line is assembled first, on a string port, and the sink
  ;; is given the port and the whole string.
  ;;
  ;; THE SINK IS INJECTED, LIKE THE SWITCH. Writing the string so that no
  ;; preemption can fall inside it takes the host's scheduler, which this
  ;; library does not know: the platform layer installs a sink that writes
  ;; and flushes with interrupts disabled. The default, for a host that
  ;; installs none, writes the string in one put-string and flushes.
  ;; A one-slot vector, for the reason `enabled` is one.
  (define (default-line-sink p line)
    (put-string p line)
    (flush-output-port p))
  (define line-sink (vector default-line-sink))
  (define (trace-line-sink) (vector-ref line-sink 0))
  (define (trace-line-sink-set! sink) (vector-set! line-sink 0 sink))

  ;; THE PORT IS LOOKED UP ON EVERY EVENT, as it always was: a test that
  ;; rebinds the current error port reads the trace it made.
  (define (trace-event! op subject bytes)
    (when (trace-enabled?)
      (guard (e (#t (if #f #f)))
        (let ((line (call-with-string-output-port
                      (lambda (s)
                        (put-string s "(trace ")
                        (write-field s op)
                        (put-string s " ")
                        (write-field s subject)
                        (put-string s " ")
                        (write-field s bytes)
                        (put-string s ")\n")))))
          ((vector-ref line-sink 0) (current-error-port) line))))))
