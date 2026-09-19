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
(library (theourgia operation-packet)
  (export frozen-operation)
  (import (rnrs) (theourgia store) (theourgia wire) (theourgia digest)
          (only (theourgia log) atomic-write! directory-entry-durable!)
          (only (theourgia ffi) mkdir-p! file-ensure! with-exclusive-lock))
  (define (encode x) (string->utf8 (sexpr->string-extended (storable-encode x))))
  ;; Capture and install under a separate, stable lock. The store lock is
  ;; acquired only inside capture or execution, never in the reverse order.
  ;; A retry uses exactly the first captured operation, including its baselines.
  (define (frozen-operation store req capture)
    (if (not req) (list #f (capture))
        (let* ((dir (string-append store "/operation-packets"))
               (key (bytevector->hex (sha256 (encode (list (car (list-ref req 5)) (list-ref req 4))))))
               (path (string-append dir "/" key ".sexp"))
               (lock (string-append dir "/lock")))
          (mkdir-p! dir)
          (directory-entry-durable! dir 'working)
          (file-ensure! lock)
          (with-exclusive-lock lock
            (lambda (fd)
              (if (file-exists? path)
                  (let ((saved (call-with-port (open-file-input-port path)
                                 (lambda (p) (storable-decode (string->sexpr-extended (utf8->string (get-bytevector-all p))))))))
                    (unless (and (list? saved) (= 5 (length saved)) (eq? (car saved) 'operation-packet)
                                 (equal? (cadr saved) 1))
                      (raise '(error operation-packet-unavailable)))
                    (unless (equal? req (caddr saved)) (raise '(error req-mismatch)))
                    (list (cadddr saved) (list-ref saved 4)))
                  (let* ((captured (capture))
                         (effective (make-write-request (list-ref req 1) (list-ref req 2)
                                      (append (list-ref req 3) (list (utf8->string (encode captured))))
                                      (list-ref req 4) (list-ref req 5))))
                    (atomic-write! path (encode (list 'operation-packet 1 req effective captured)) 'working)
                    (list effective captured))))))))
)
