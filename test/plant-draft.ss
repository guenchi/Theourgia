;; Copyright 2018 - 2026 The Theourgia Authors
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

;; A DRAFT WRITTEN AS working.sc WRITES ONE, WITHOUT ASKING `write`: the
;; envelope (working 1 <writer> <block> <version> <based-on> <cut> <bytes>)
;; under writers/<writer>/working/<block>, its version from reduce.sc's
;; draft-version, on the block's committed hash and the store's applied
;; cut. It makes the drafts `write` refuses -- one on a block of mode
;; datum -- which a store written before that refusal can still hold.
;;
;; The including program imports:
;;   (only (theourgia store) open-and-reduce)
;;   (only (theourgia reduce) block-hash reduce-applied-cut draft-version)
;;   (only (theourgia log) writer-directory)
;;   (only (theourgia wire) sexpr->string-extended storable-encode)
(define (plant-draft! store writer block text)
  (let* ((state (open-and-reduce store))
         (bytes (string->utf8 text))
         (based (block-hash state block))
         (cut (reduce-applied-cut state))
         (dir (string-append (writer-directory store writer) "/working"))
         (entry (list 'working 1 writer block (draft-version bytes based cut) based cut bytes)))
    (system (string-append "mkdir -p '" dir "'"))
    (call-with-port (open-file-output-port (string-append dir "/" block) (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 (sexpr->string-extended (storable-encode entry))))))
    (list-ref entry 4)))
