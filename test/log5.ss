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

;; The defects the three review letters named, each with a case that
;; would have failed before the fix and a control that must keep passing.
(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace))
(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define (raises? t) (guard (e (#t #t)) (t) #f))
(define d "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/log5work")
(system (string-append "rm -rf " d "; mkdir -p " d))

(printf "== a stale temporary must never be reused ==\n")
;; Before: the temp name was opened with create-if-absent and no
;; truncation, so a leftover from a crashed run with the same pid and
;; counter was written INTO, leaving new bytes followed by old ones.
(define target (string-append d "/t.sexp"))
(atomic-write! target (string->utf8 "AAAAAAAAAAAAAAAAAAAAAAAA\n") 'snapshot)
(want "first write" (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "AAAAAAAAAAAAAAAAAAAAAAAA\n")
;; plant a leftover under every name the next call could pick
(let loop ((i 1))
  (when (< i 8)
    (call-with-port (open-file-output-port
                      (string-append target ".tmp-" (number->string (get-process-id))
                                     "-" (number->string i))
                      (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 "STALESTALESTALESTALESTALE\n"))))
    (loop (+ i 1))))
(atomic-write! target (string->utf8 "BB\n") 'snapshot)
(want "a short replacement is exactly itself, with no stale tail"
      (utf8->string (call-with-port (open-file-input-port target) get-bytevector-all))
      "BB\n")

(printf "== a trailing slash is refused before anything is written ==\n")
(want "refused" (raises? (lambda () (atomic-write! (string-append d "/sub/") (string->utf8 "x") 'snapshot))) #t)
;; The first version of this row compared one expression with ITSELF,
;; which is true however the code behaves. The listing is captured
;; before the refused call and compared with the listing after it.
(define before-slash (sort string<? (directory-list d)))
(want "refused again, with the directory captured before and after"
      (begin (raises? (lambda () (atomic-write! (string-append d "/sub2/") (string->utf8 "x") 'snapshot)))
             (sort string<? (directory-list d)))
      before-slash)

(printf "== the install declares a stage, so a fault can be aimed at it ==\n")
;; Without a declared stage every fault selecting `snapshot` was inert
;; and the run looked exactly like one where the injection had fired and
;; found nothing to break.
(want "a snapshot-stage fsync fault reaches this install"
      (guard (e ((fs-error? e) (list 'raised (fs-error-op e) (fs-error-errno e))))
        (atomic-write! (string-append d "/aimed.sexp") (string->utf8 "x\n") 'snapshot)
        'not-reached)
      (if (theourgia-fault) '(raised fsync 5) 'not-reached))
(printf "  (fault selected: ~s)\n" (theourgia-fault))

(printf "== meta must carry a supported format version ==\n")
(define (store-with meta)
  (let ((s (string-append d "/store")))
    (system (string-append "rm -rf " s "; mkdir -p " s "/writers"))
    (call-with-port (open-file-output-port (string-append s "/meta.sexp") (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 meta))))
    ;; A load now holds the store's shared lock for its whole duration,
    ;; so a store without a lock file cannot be opened at all -- and the
    ;; lock helpers deliberately do not create one. init writes it; this
    ;; fixture stands in for init.
    (file-ensure! (string-append s "/lock"))
    s))
(want "format 999 is refused"
      (guard (e ((log-error? e) (log-error-kind e))) (log-open (store-with "((format 999))\n")) 'opened)
      'meta)
(want "a datum with no format is refused"
      (guard (e ((log-error? e) (log-error-kind e))) (log-open (store-with "((store-id \"x\"))\n")) 'opened)
      'meta)
(want "CONTROL: format 1 opens"
      (guard (e ((log-error? e) (log-error-kind e)))
        (begin (log-open (store-with "((format 1) (store-id \"x\"))\n")) 'opened))
      'opened)

(printf "== a fifo named like a segment is not a segment ==\n")
;; Opening a fifo for reading blocks until a writer appears -- and the
;; reader does that INSIDE the shared lock, so a name check alone can
;; hold the store's lock forever. Nothing raises, so no handler helps.
(define fdir (string-append d "/writers/aaaaaaaa"))
(system (string-append "mkdir -p " fdir "; mkfifo " fdir "/000002.sexp"))
(call-with-port (open-file-output-port (string-append fdir "/000001.sexp")
                                       (file-options no-fail))
  (lambda (p) (put-bytevector p (string->utf8 "x\n"))))
(want "the fifo is not enumerated as a segment"
      (enumerate-segment-files d "aaaaaaaa") '(1))
(want "CONTROL: a regular file with the same name IS enumerated"
      (begin (system (string-append "rm -f " fdir "/000002.sexp"))
             (call-with-port (open-file-output-port (string-append fdir "/000002.sexp")
                                                    (file-options no-fail))
               (lambda (p) (put-bytevector p (string->utf8 "y\n"))))
             (enumerate-segment-files d "aaaaaaaa"))
      '(1 2))

(printf "== a creation failure that is not a name clash is re-raised ==\n")
;; Catching everything turned an unwritable directory into 65 retries
;; and then a generic error naming neither the cause nor the candidate.
(define ro (string-append d "/readonly"))
(system (string-append "rm -rf " ro "; mkdir -p " ro "; chmod 500 " ro))
(want "the real cause survives, rather than becoming a generic temp error"
      (guard (e ((log-error? e) (list 'generic (log-error-kind e)))
                (#t 'raised-something-specific))
        (atomic-write! (string-append ro "/x.sexp") (string->utf8 "x") 'snapshot)
        'wrote)
      'raised-something-specific)
(system (string-append "chmod 700 " ro))

(printf "\n~a failures\n" bad)
(printf "log5 complete\n")
