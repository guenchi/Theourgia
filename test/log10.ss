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

;; Retirement extension, read side (design v38 section 5.1, steps 2, 3, 5).
;;
;; A retirement marker is a LOWER bound on history, not an upper one. The
;; prefix it declares is where the writer stopped writing; a mirror that
;; later supplies the segments it never published can push the readable
;; boundary past it. The manifest is the sole authority for that: bytes
;; sitting in the file are not history merely because they parse.
;;
;; So the same trailing bytes mean two different things depending on one
;; fact that is not in them -- whether published.sexp lists their segment
;; and its hash agrees:
;;   not listed  -> the extent stops at the declared prefix, and the rest
;;                  is retired-tail: informational, not an error, because
;;                  a torn tail is the ordinary way a writer retires
;;   listed, hash agrees   -> history, and the extent runs to end of file
;;   listed, hash disagrees -> salvage: the whole segment leaves history,
;;                  taking the retired prefix inside it, because "the
;;                  manifest says X and the file is not X" can only come
;;                  from a rollback, and discovery does not guess for it
;;
;; The fixtures place segments and manifests by hand: publish, extend and
;; adopt do not exist yet, so L20 end to end is not what this covers.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (only (igropyr crypto) sha256 bytevector->hex))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    (system (string-append "mkdir -p " path))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d (test-dir "log10work"))
(define A "k3m9x2qa")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (r n) (rec n '() (list 'put (string-append "a." (number->string n)) '())))
(define R (bytevector-length (r 1)))
;; A record of a different length, for building the file extend would
;; have left behind if it had not preserved the prefix bytes.
(define (long n)
  (rec n '() (list 'put (string-append "a." (number->string n)) '("padding" "padding"))))
(define L (bytevector-length (long 1)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (call-with-port (open-file-input-port path) get-bytevector-all))
(define (wpath n) (string-append d "/writers/" A "/" (segment-file-name n)))
(define (seg! n . rs) (put! (wpath n) (apply cat rs)))
(define (build!)
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" A " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n")))
(define (retire! seg off seq)
  (put! (string-append d "/writers/" A "/retired.sexp")
        (string->utf8 (string-append "((prefix " (number->string seg) " " (number->string off)
                                     " " (number->string seq) ") (tx \"t1\"))\n"))))
(define (unretire!) (system (string-append "rm -f " d "/writers/" A "/retired.sexp")))
(define (quar! f)
  (put! (string-append d "/writers/" A "/quarantine.sexp")
        (string->utf8 (string-append "((format 1) (fork " (number->string f) "))\n"))))
;; The manifest is the only thing that authorises extension, so every row
;; states exactly which segments are in it and whether each hash is the
;; file's own. `bad-hash' lists segments whose entry deliberately does not
;; match the bytes on disk.
(define (publish! listed bad-hash)
  (put! (string-append d "/writers/" A "/published.sexp")
        (string->utf8
          (string-append "("
            (apply string-append
              (map (lambda (n)
                     (string-append "(" (number->string n) " . \""
                                    (if (or (memv n bad-hash)
                                            (not (file-exists? (wpath n))))
                                        "00000000000000000000000000000000000000000000000000000000000000ff"
                                        (bytevector->hex (sha256 (slurp (wpath n)))))
                                    "\")"))
                   listed))
            ")\n"))))
(define (survey)
  (let* ((ls (log-open d)) (p (load-prefix ls A)) (seen '()))
    (load-deliver! ls '() (lambda (w seg off seq . rest) (set! seen (cons seq seen))))
    (let ((o (load-outcome ls)) (rt (discovery-retired p)))
      (list (list (discovery-end-segment p) (discovery-end-offset p) (discovery-end-seq p))
            (discovery-segment-ranges p)
            (map log-error-kind (discovery-integrity p))
            (reverse seen)
            (if (pair? o) (car o) o)
            (and rt (not (eq? (car rt) 'malformed)) (list-head rt 3))
            (discovery-retired-tail p)))))

(printf "== the prefix segment is not in the manifest: the tail is not history ==\n")
;; The ordinary shape of a retired writer. The records past the declared
;; prefix parse and check out, and they are still not history: nothing has
;; published them.
(build!)
(seg! 1 (r 1) (r 2) (r 3))
(retire! 1 (* 2 R) 2)
(want "valid records past the declared prefix are a retired tail, not an error"
      (survey)
      (list (list 1 (* 2 R) 2) '((1 1 2)) '() '(1 2) 'open (list 1 (* 2 R) 2)
            (list 1 (* 2 R))))

;; The same position reached by the other route: a torn line rather than a
;; whole record. Retirement is often exactly this, which is why it is not
;; an integrity error.
(build!)
(put! (wpath 1) (cat (r 1) (r 2) (string->utf8 "{ this line never finished")))
(retire! 1 (* 2 R) 2)
(want "a torn residual past the declared prefix is a retired tail either way"
      (survey)
      (list (list 1 (* 2 R) 2) '((1 1 2)) '() '(1 2) 'open (list 1 (* 2 R) 2)
            (list 1 (* 2 R))))

;; And with nothing after the prefix there is no tail to report.
(build!)
(seg! 1 (r 1) (r 2))
(retire! 1 (* 2 R) 2)
(want "CONTROL: a prefix that ends at the file's end has no tail"
      (survey)
      (list (list 1 (* 2 R) 2) '((1 1 2)) '() '(1 2) 'open (list 1 (* 2 R) 2) #f))

(printf "== the manifest is what turns those same bytes into history ==\n")
;; Byte-for-byte the first fixture, with one line added to published.sexp.
(build!)
(seg! 1 (r 1) (r 2) (r 3))
(retire! 1 (* 2 R) 2)
(publish! '(1) '())
(want "a listed prefix segment with a matching hash extends to the end of its file"
      (survey)
      (list (list 1 (* 3 R) 3) '((1 1 3)) '() '(1 2 3) 'open (list 1 (* 2 R) 2) #f))

(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 4) (r 5))
(retire! 1 (* 2 R) 2)
(publish! '(1 2) '())
(want "a following listed segment whose first seq joins on extends further"
      (survey)
      (list (list 2 (* 2 R) 5) '((1 1 3) (2 4 5)) '() '(1 2 3 4 5) 'open
            (list 1 (* 2 R) 2) #f))

(printf "== extension stops at the first thing the manifest does not vouch for ==\n")
(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 3 (r 4) (r 5))
(retire! 1 (* 2 R) 2)
(publish! '(1 2 3) '())
(want "a listed segment that is missing stops the extension, and is named"
      (survey)
      (list (list 1 (* 3 R) 3) '((1 1 3)) '(manifest-missing-segment) '(1 2 3) 'open
            (list 1 (* 2 R) 2) #f))

(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 4) (r 5))
(retire! 1 (* 2 R) 2)
(publish! '(1) '())
(want "a segment that is simply not listed ends the extension quietly"
      (survey)
      (list (list 1 (* 3 R) 3) '((1 1 3)) '() '(1 2 3) 'open (list 1 (* 2 R) 2) #f))

(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 7) (r 8))
(retire! 1 (* 2 R) 2)
(publish! '(1 2) '())
(want "a listed segment whose first seq does not join on stops before it"
      (survey)
      (list (list 1 (* 3 R) 3) '((1 1 3)) '(seq) '(1 2 3) 'open (list 1 (* 2 R) 2) #f))

(printf "== a listed segment whose hash disagrees leaves history entirely ==\n")
;; The salvage rule, and it reaches INSIDE the retired prefix: the records
;; of segment 1 are individually valid and they are still excluded,
;; because the manifest says this file is something else.
(build!)
(seg! 1 (r 1) (r 2) (r 3))
(retire! 1 (* 2 R) 2)
(publish! '(1) '(1))
(want "the prefix segment itself is excluded, and nothing is left below it"
      (survey)
      (list (list #f #f 0) '() '(manifest-hash) '() 'open (list 1 (* 2 R) 2) #f))

(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 4) (r 5))
(retire! 1 (* 2 R) 2)
(publish! '(1 2) '(2))
(want "a later bad hash stops the extent before it and keeps the prefix"
      (survey)
      (list (list 1 (* 3 R) 3) '((1 1 3)) '(manifest-hash) '(1 2 3) 'open
            (list 1 (* 2 R) 2) #f))

(printf "== a fork inside the extension still wins ==\n")
(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 4) (r 5))
(retire! 1 (* 2 R) 2)
(publish! '(1 2) '())
(quar! 4)
(want "the extent is the minimum of the extension and the fork"
      (survey)
      (list (list 1 (* 3 R) 3) '((1 1 3)) '() '(1 2 3) 'open (list 1 (* 2 R) 2) #f))

;; A MUTATION SURVIVED THE ROWS ABOVE AND THIS IS WHY IT EXISTS.
;; Dropping the "only when nothing published these bytes" guard from the
;; tail changed no reading here: wherever the manifest vouches for the
;; segment the scan runs to the end of the file, so there is never
;; anything above the boundary to report. The one place the two differ is
;; when something ELSE stops the scan inside a published segment -- a
;; fork. Those bytes above the fork are published history being withheld,
;; not the residue of retiring, and calling them a retired tail would put
;; the wrong name on quarantined history.
(build!)
(seg! 1 (r 1) (r 2) (r 3))
(retire! 1 (* 2 R) 2)
(publish! '(1) '())
(quar! 3)
(want "a fork inside a published prefix segment leaves no retired tail"
      (survey)
      (list (list 1 (* 2 R) 2) '((1 1 2)) '() '(1 2) 'open (list 1 (* 2 R) 2) #f))

(printf "== extension does not exempt the marker from being checked ==\n")
;; EXTEND PRESERVES THE PREFIX BYTES, so on a correctly extended file the
;; marker's offset still ends the record it declares. When it does not,
;; the manifest and retired.sexp are two authorities contradicting each
;; other about the same bytes, and discovery says so rather than picking
;; one. The extent still follows the manifest: the disagreement is
;; recorded, it does not lower the boundary.
;;
;; Built the way it would really arise -- the prefix records replaced by
;; records of a different length, then listed -- so that the declared
;; offset no longer falls on any record boundary at all.
(build!)
(seg! 1 (long 1) (long 2) (long 3))
(retire! 1 (* 2 R) 2)
(publish! (list 1) (list))
(want "an extended file whose declared offset no longer ends that record is flagged"
      (survey)
      (list (list 1 (* 3 L) 3) (list (list 1 1 3)) '(retired-mismatch) '(1 2 3) 'open
            (list 1 (* 2 R) 2) #f))

;; The offset lands on a boundary, but on the wrong record's.
(build!)
(seg! 1 (r 1) (r 2) (r 3))
(retire! 1 (* 3 R) 2)
(publish! (list 1) (list))
(want "an offset that ends the wrong record is flagged just the same"
      (survey)
      (list (list 1 (* 3 R) 3) (list (list 1 1 3)) '(retired-mismatch) '(1 2 3) 'open
            (list 1 (* 3 R) 2) #f))

;; GREEN TWIN: the ordinary extended file, where the offset still ends the
;; declared record. This is the row above under "the manifest is what
;; turns those same bytes into history"; repeated here because the two
;; readings differ in exactly one field and a check that fired on every
;; extension would pass every row in that section.
(build!)
(seg! 1 (r 1) (r 2) (r 3))
(retire! 1 (* 2 R) 2)
(publish! (list 1) (list))
(want "CONTROL: a faithfully extended file is still checked, and agrees"
      (survey)
      (list (list 1 (* 3 R) 3) (list (list 1 1 3)) '() '(1 2 3) 'open
            (list 1 (* 2 R) 2) #f))

(printf "== none of this reaches a writer that has not retired ==\n")
;; The same files and the same manifest, with retired.sexp removed. A
;; live local writer owns its whole current segment and needs no manifest
;; at all; if the extension rules leaked here they would either truncate
;; it or start demanding published entries.
(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 4) (r 5))
(publish! '(1) '())
(want "CONTROL: a live writer reads the same layout by the local rules"
      (survey)
      (list (list 2 (* 2 R) 5) '((1 1 3) (2 4 5)) '() '(1 2 3 4 5) 'open #f #f))

(printf "== the manifest is the authority, and it does not edit the marker ==\n")
(build!)
(seg! 1 (r 1) (r 2) (r 3)) (seg! 2 (r 4) (r 5))
(retire! 1 (* 2 R) 2)
(publish! '(1 2) '())
(let ((before (slurp (string-append d "/writers/" A "/retired.sexp"))))
  (survey)
  (want "retired.sexp is byte-identical after a load that extended past it"
        (slurp (string-append d "/writers/" A "/retired.sexp")) before))

(printf "\n~a failures\n" bad)
(printf "log10 complete\n")
