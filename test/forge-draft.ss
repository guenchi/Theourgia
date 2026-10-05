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

;; A DRAFT AN EARLIER BUILD WROTE.
;;
;; `write` refuses a block whose mode is datum, at any baseline, so this
;; build cannot make a draft on one. A build before that refusal could,
;; and its drafts are still on disk: a slot in a writer's working area
;; holding (working 1 <writer> <id> <version> <based-on> <cut> <bytes>),
;; based on the block as it then was. What reads such a draft -- commit's
;; check, the working view -- can only be asked about one if a fixture
;; writes it the way that build did.
;;
;; So this writes the envelope `write` writes (working.sc, working-write!),
;; at the block's baseline NOW: its hash and the applied cut of the store
;; as it stands, the version computed from those and the bytes by the one
;; rule (reduce.sc, draft-version). A draft at a fresh baseline is the one
;; that passes every check before the one being asked about.
;;
;; THE INCLUDING FIXTURE IMPORTS what this uses: open-and-reduce, from
;; (theourgia store); block-hash, reduce-applied-cut and draft-version, from
;; (theourgia reduce); storable-encode and sexpr->string-extended, from
;; (theourgia wire); and writer-directory, from (theourgia log).
;; -> the draft's version.
(define (forge-fresh-draft! store writer id text)
  (let* ((state (open-and-reduce store))
         (hash (block-hash state id))
         (cut (reduce-applied-cut state))
         (body (string->utf8 text))
         (version (draft-version body hash cut))
         (dir (string-append (writer-directory store writer) "/working")))
    (system (string-append "mkdir -p '" dir "'"))
    (call-with-port (open-file-output-port (string-append dir "/" id) (file-options no-fail))
      (lambda (o)
        (put-bytevector o (string->utf8 (sexpr->string-extended
                                          (storable-encode (list 'working 1 writer id version hash cut body)))))))
    version))
