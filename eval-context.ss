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
