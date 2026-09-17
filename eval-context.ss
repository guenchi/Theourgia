#!r6rs
(library (theourgia eval-context)
  (export eval-context! store-cut blocks block
          display write newline open-string-input-port current-error-port flush-output-port)
  (import (chezscheme) (theourgia reduce) (theourgia code-project))
  ;; The worker owns this detached read view. No store handle or IO capability
  ;; is exported into evaluated code.
  (define snapshot #f)
  (define (eval-context! state) (set! snapshot state))
  (define (store-cut) (reduce-applied-cut snapshot))
  (define (blocks) (state-datum snapshot))
  (define (block id) (state-read snapshot id))
)
