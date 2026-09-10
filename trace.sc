#!chezscheme
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
  (export theourgia-trace? trace-event!)
  (import (chezscheme))

  ;; Read once, when the library is initialised. A process either was
  ;; started to be traced or it was not, and re-reading the environment
  ;; on every syscall would put a getenv on the write path to buy a
  ;; capability nothing wants. The parameter is exported so an
  ;; in-process test can turn it on without spawning a fresh process.
  (define theourgia-trace?
    (make-parameter (equal? (getenv "THEOURGIA_TRACE") "1")))

  ;; FLUSHED ON EVERY LINE, because the sequence is the thing being
  ;; asserted and two processes writing an unflushed stderr interleave
  ;; by buffer boundary rather than by event order.
  ;;
  ;; A FAILURE TO REPORT IS SWALLOWED. Tracing must never be able to
  ;; turn a write that worked into a write that raised: that would make
  ;; the traced run and the untraced run different programs, and the
  ;; traced one is the one being believed.
  (define (trace-event! op subject bytes)
    (when (theourgia-trace?)
      (guard (e (#t (void)))
        (let ((p (current-error-port)))
          (fprintf p "(trace ~a ~a ~a)\n" op subject bytes)
          (flush-output-port p))))))
