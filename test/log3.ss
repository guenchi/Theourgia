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

;; Piece 3: the snapshot frame, against L7's four void variants and the
;; cut-support entry point.
(import (chezscheme) (theourgia log) (theourgia trace) (theourgia crc32))
;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/log3work")
(system (string-append "rm -rf " d "; mkdir -p " d))
(define snap (string-append d "/snap.sexp"))
(define cut '(("k3m9x2qa" . 10) ("c9xq01mz" . 3)))
(define rows (list '(block "k3m9x2qa.1" ((title ("a" ("k3m9x2qa" . 1)))))
                   '(edge "k3m9x2qa.1" explains "k3m9x2qa.2" ("k3m9x2qa" . 2))
                   '(tag "v1" ((("k3m9x2qa" . 10))))))
(define (read2 p) (call-with-values (lambda () (snapshot-read p)) (lambda (a b) (list a b))))
;; get-bytevector-all answers #!eof for an empty file, not an empty
;; bytevector -- and this helper is used by the injection guard, so a
;; case that writes an empty file would crash the guard rather than the
;; row it guards.
(define (text p)
  (if (not (file-exists? p))
      ""
      (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
        (if (eof-object? b) "" (utf8->string b)))))
;; EVERY INJECTION IS CHECKED TO HAVE CHANGED THE BYTES (plan section
;; 0). The first version of the CRC case replaced #\a with #\b in an end
;; line that happened to contain no #\a: the file was untouched, the
;; reader correctly accepted it, and the row printed FAIL as though the
;; code were broken. A no-op injection produces a reading that looks
;; exactly like a real defect.
(define (retext! p s)
  (let ((before (text p)))
    (call-with-port (open-file-output-port p (file-options no-fail))
      (lambda (o) (put-bytevector o (string->utf8 s))))
    (when (string=? before (text p))
      (set! bad (+ bad 1))
      (printf "FAIL injection changed nothing at ~a -- the row below proves nothing\n" p))))

(printf "== a whole snapshot ==\n")
(snapshot-write! snap cut rows)
(printf "on disk:\n~a" (text snap))
(want "round trip" (read2 snap) (list cut rows))
(want "the last line is the end line"
      (let ((ls (let loop ((i 0) (st 0) (acc '()))
                  (cond ((>= i (string-length (text snap))) (reverse acc))
                        ((char=? (string-ref (text snap) i) #\newline)
                         (loop (+ i 1) (+ i 1) (cons (substring (text snap) st i) acc)))
                        (else (loop (+ i 1) st acc))))))
        (substring (car (reverse ls)) 0 5))
      "(end ")

(printf "== L7 variant: end missing ==\n")
(define whole (text snap))
(define no-end
  (let loop ((i (- (string-length whole) 2)))
    (if (char=? (string-ref whole i) #\newline) (substring whole 0 (+ i 1)) (loop (- i 1)))))
(retext! snap no-end)
(want "voided, reason no-end" (read2 snap) '(#f no-end))

(printf "== L7 variant: end CRC wrong ==\n")
(snapshot-write! snap cut rows)
(define w2 (text snap))
;; A checksum that is definitely not the right one, rather than a
;; character substitution that may find nothing to substitute.
(retext! snap (let ((i (let loop ((i (- (string-length w2) 2)))
                         (if (char=? (string-ref w2 i) #\newline) (+ i 1) (loop (- i 1))))))
                (string-append (substring w2 0 i) "(end \"deadbeef\")\n")))
(want "voided, reason crc" (read2 snap) '(#f crc))

(printf "== L7 variant: a row altered, checksum therefore wrong ==\n")
(snapshot-write! snap cut rows)
(define w3 (text snap))
(retext! snap (list->string (map (lambda (c) (if (char=? c #\x) #\y c)) (string->list w3))))
(want "voided, reason crc (never partly adopted)" (read2 snap) '(#f crc))

(printf "== other void shapes ==\n")
(retext! snap "")
(want "empty file" (read2 snap) '(#f empty))
(retext! snap "(end \"00000000\")\n")
(want "end but no header" (read2 snap) '(#f no-header))
(want "absent file" (read2 (string-append d "/nope.sexp")) '(#f absent))
(snapshot-write! snap cut rows)
(retext! snap (string-append (text snap) "(block \"ghost\" ())\n"))
;; The end line has to be the LAST line, so trailing bytes mean the last
;; line is not an end line. no-end is the accurate category here; my
;; first expectation of crc assumed a reader that hunts for an end line
;; among the rows, which would mean parsing before checksumming.
(want "bytes after the end line void it" (read2 snap) '(#f no-end))

;; ASSERTED, NOT EYEBALLED. The first version compared the trace length
;; against zero, which is true of every string -- a row that passes
;; whatever the code does.
(define (has? hay needle)
  (let ((n (string-length hay)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring hay i (+ i m)) needle) #t)
            (else (loop (+ i 1)))))))
(printf "== rows whose strings contain newlines and other traps ==\n")
;; The codec's writer emits a newline inside a string RAW, and this
;; frame is line-oriented. Records escape them; snapshots did not, so a
;; row with an embedded newline produced a file that checksummed
;; correctly and then failed to parse.
(define hard-rows
  (list '(block "x" ((body . "a\nb")))
        '(block "y" ((body . "quote \" and backslash \\")))
        (list 'block "z" (list (cons 'body "tab\there")))
        '(block "w" ((body . "\x65e5;\x672c;\x8a9e;")))))
(snapshot-write! snap cut hard-rows)
(want "rows with newlines, quotes, backslashes, tabs and multibyte round trip"
      (read2 snap) (list cut hard-rows))
(want "and the file is still one line per row plus a header and an end"
      (let loop ((i 0) (n 0))
        (if (>= i (string-length (text snap))) n
            (loop (+ i 1) (if (char=? (string-ref (text snap) i) #\newline) (+ n 1) n))))
      (+ 1 (length hard-rows) 1))

(printf "== a header that is PRESENT but wrong ==\n")
;; The mutation "header not validated" survived the first version of
;; this smoke: every no-header row it had was really exercising the
;; empty-rows check, so nothing ever handed the reader a malformed
;; header. These build a frame with a CORRECT checksum so that the
;; header check is the only thing that can refuse it.
(define (raw-snapshot! path body-lines)
  (let* ((body (apply string-append (map (lambda (l) (string-append l "\n")) body-lines)))
         (crc (crc32-string-hex body)))
    (retext! path (string-append body "(end \"" crc "\")\n"))))
(raw-snapshot! snap (list "(snapshot 2 ((\"w\" . 1)))"))
(want "a wrong format version is refused" (read2 snap) '(#f no-header))
(raw-snapshot! snap (list "(snapshot 1 \"not-a-cut\")"))
(want "a cut that is not an alist is refused" (read2 snap) '(#f no-header))
(raw-snapshot! snap (list "(snapshot 1 ((w . 1)))"))
(want "a cut whose writer is a symbol is refused" (read2 snap) '(#f no-header))
(raw-snapshot! snap (list "(block \"x\" ())"))
(want "a first line that is not a header at all is refused" (read2 snap) '(#f no-header))
(raw-snapshot! snap (list "(snapshot 1 ((\"w\" . 1)))" "(block \"x\" ())"))
(want "CONTROL: a correct header with the same machinery is ACCEPTED"
      (read2 snap) '((("w" . 1)) ((block "x" ()))))

(printf "== the trace says which category ==\n")
(snapshot-write! snap cut rows)
(let ((p (open-output-string)))
  (parameterize ((theourgia-trace? #t) (current-error-port p))
    (read2 snap)
    (retext! snap "")
    (read2 snap))
  ;; DRAINED ONCE. get-output-string empties the port, so calling it to
  ;; print and again to assert leaves the assertion looking at "". The
  ;; weak version of these rows (length >= 0) passed anyway, which is
  ;; how the bug survived into a green run.
  (let ((s (get-output-string p)))
    (printf "~a" s)
    (want "the adoption is traced with the byte count"
          (has? s "snap.sexp 213") #t)
    (want "the rejection is traced and carries the reason in its subject"
          (has? s ". empty)") #t)))

(printf "== L7 variant: cut not supported by the log ==\n")
(want "supported when coverage reaches it"
      (snapshot-cut-supported? cut '(("k3m9x2qa" . 10) ("c9xq01mz" . 3))) #t)
(want "supported when coverage exceeds it"
      (snapshot-cut-supported? cut '(("k3m9x2qa" . 12) ("c9xq01mz" . 3))) #t)
(want "NOT supported when the log is one short"
      (snapshot-cut-supported? cut '(("k3m9x2qa" . 9) ("c9xq01mz" . 3))) #f)
(want "NOT supported when a named writer is absent entirely"
      (snapshot-cut-supported? cut '(("k3m9x2qa" . 10))) #f)
(want "an absent writer is not a zero"
      (snapshot-cut-supported? '(("w" . 0)) '()) #f)
(want "CONTROL: a zero-seq writer present in coverage IS supported"
      (snapshot-cut-supported? '(("w" . 0)) '(("w" . 0))) #t)
(want "empty cut is trivially supported" (snapshot-cut-supported? '() '()) #t)

(printf "\n~a failures\n" bad)
(printf "log3 complete\n")
