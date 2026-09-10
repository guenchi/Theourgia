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

;; Rejection, snapshots across a fork, and the metadata version barrier
;; (plan L25 d f g, and L22).
;;
;; A REJECTION IS THE REDUCER'S, NOT THE DISK'S. A record it cannot use
;; -- an unknown verb, a binding that will not check -- stops that writer
;; where it stands, and nothing about the bytes is wrong: the history is
;; untouched and no integrity error is recorded. Confusing the two would
;; put a store into "run adopt" for a reason adopt cannot fix.
;;
;; A VERSION MUST BE DURABLE BEFORE ANYTHING DEPENDS ON IT. A record
;; written because a manifest says a mirror's segment is history is a
;; claim that outlives the manifest if the manifest was never flushed.
;; Directory durability is not content durability, and a segment barrier
;; cannot stand in: a fork at a writer's first event means no record of
;; that writer is ever read.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace)
        (only (igropyr crypto) sha256 bytevector->hex))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
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
(define d (test-dir "log17work"))
(define home (string-append d "/home"))
(define W "wwwg7q2a")
(define M "mmmg7q2a")
(define fixed-ts 1757300000003)
(define (rec seq ts deps payload)
  (encode-record seq ts "agent:claude" deps (storable-encode payload)))
(define (r tag n) (rec n (+ 1757300000000 n) '()
                       (list 'put (string-append tag "." (number->string n)) '())))
(define R (bytevector-length (r "w" 1)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) (make-bytevector 0) b))))
(define (wpath w n) (string-append d "/writers/" w "/" (segment-file-name n)))
(define (qpath w) (string-append d "/writers/" w "/quarantine.sexp"))
(define (publish! . ns)
  (put! (string-append d "/writers/" M "/published.sexp")
        (string->utf8
          (string-append "("
            (apply string-append
              (map (lambda (n)
                     (string-append "(" (number->string n) " . \""
                                    (bytevector->hex (sha256 (slurp (wpath M n)))) "\")"))
                   ns))
            ")\n"))))
(define (build!)
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" W " " d "/writers/" M
                         " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"g7\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (putenv "THEOURGIA_HOME" home)
  (put! (wpath W 1) (cat (r "w" 1) (r "w" 2)))
  (put! (wpath M 1) (cat (r "m" 1) (r "m" 2) (r "m" 3)))
  (publish! 1)
  (let ((n (instance-install! d))) (owner-install! d W n)))
(define (quarantine! w fork)
  (put! (qpath w)
        (string->utf8 (string-append "((format 1) (fork " (number->string fork) "))\n"))))
(define (tree-bytes)
  (map (lambda (w)
         (cons w (map (lambda (n) (cons n (slurp (wpath w n))))
                      (enumerate-segment-files d w))))
       (store-writers d)))

(printf "== L25(d): a rejection is the reducer's, not the disk's ==\n")
(build!)
(define before-reject (tree-bytes))
(want "rejecting a pending record leaves the history and the diagnostics alone"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (let* ((res (session-reject! s (session-epoch s) (cons M 2) 'no-such-verb))
                 (integrity (map (lambda (e) (log-error-kind (cdr e)))
                                 (load-integrity (session-load s))))
                 (extent (cdr (assq 'contiguous
                                    (cdr (assoc M (session-frontiers s))))))
                 (same (equal? (tree-bytes) before-reject)))
            (log-end! s)
            (list (car res) integrity extent same))))
      (list 'rejected '() 3 #t))
;; A REJECTION IS NOT AN INTEGRITY ERROR, and the difference matters:
;; integrity means the bytes are wrong and the remedy is adopt, which
;; would do nothing whatever about a verb the reducer does not know.
(want "and the writer is not poisoned -- adopt is not the remedy for this"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (session-reject! s (session-epoch s) (cons M 2) 'no-such-verb)
          ;; The LOCAL writer is untouched by a rejection of the mirror.
          ;; GUARDED: an implementation that poisons the session on a
          ;; rejection raises here, and an exception would end the file
          ;; with no FAIL line -- which reads exactly like a clean run.
          (let ((res (guard (e ((log-error? e) (list (log-error-kind e))) (#t '(other)))
                       (let ((v (session-view s)))
                         (if v
                             (session-append! s (make-frame (view-revision v) (view-epoch v)
                                                            (view-writer v) (view-expect-seq v)
                                                            "agent:claude" '() '(put "w.3" ())))
                             '(no-view))))))
            (log-end! s)
            (car res))))
      'committed)
(want "an already-applied event cannot be taken back quietly"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (let ((res (session-reject! s (session-epoch s) (cons M 1) 'too-late)))
            (log-end! s)
            (list (car res) (cadr res)))))
      (list 'refused 'already-applied))
(want "and a stale epoch is refused"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (let ((res (guard (e ((log-error? e) (log-error-kind e)) (#t 'other))
                       (session-reject! s 99 (cons M 2) 'no-such-verb)
                       'accepted)))
            (log-end! s)
            res)))
      'stale-epoch)

(printf "== L25(f): a snapshot whose cut crosses the fork ==\n")
;; The snapshot's support is measured from the discovered extent, and a
;; fork lowers it -- so a cut that was supported before the quarantine
;; is not supported after it, and the state is rebuilt from unquarantined
;; history instead.
(build!)
(define (snap! n cut rows)
  (snapshot-write! (string-append d "/snap/" (segment-file-name n)) cut rows))
(snap! 1 (list (cons M 3)) '((block "x" ())))
(want "CONTROL: with no fork the snapshot is adopted"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let* ((ls (log-open d))
               (out (list (load-snapshot-cut ls) (load-snapshot-reason ls))))
          (load-commit! ls)
          out))
      (list (list (cons M 3)) #f))
(quarantine! M 2)
(want "with the fork installed, the same snapshot is refused, and says why"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let* ((ls (log-open d))
               (out (list (load-snapshot-cut ls) (load-snapshot-reason ls))))
          (load-commit! ls)
          out))
      (list #f 'quarantined))
;; And within a session: the reload must reach the same conclusion, not
;; keep the snapshot it adopted before the fork existed.
(build!)
(snap! 1 (list (cons M 3)) '((block "x" ())))
(want "a reload re-decides the snapshot rather than keeping the old decision"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (let ((before (load-snapshot-cut (session-load s))))
            (quarantine! M 2)
            (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
            (let ((after (list (load-snapshot-cut (session-load s))
                               (load-snapshot-reason (session-load s)))))
              (log-end! s)
              (list before after)))))
      (list (list (cons M 3)) (list #f 'quarantined)))

(printf "== L22: a version is made durable before anything depends on it ==\n")
;; The barrier is invisible to every functional assertion -- the store
;; reads back identically whether or not anything was flushed -- so the
;; trace is the only evidence, and it is asserted rather than printed.
(define (traced thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e))) (thunk))
      (trace-enable! #f))
    (get-output-string p)))
(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))
(define (has-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))
(define (events text) (filter (lambda (l) (has-substring? l "(trace ")) (lines-of text)))
;; PARSED, NOT COUNTED. Slicing at a hand-counted offset dropped the
;; leading slash, which a substring test tolerates and an exact one does
;; not -- so the loose assertions passed while the path was wrong.
(define (flushed-paths text)
  (let loop ((ls (events text)) (acc '()))
    (cond
      ((null? ls) (reverse acc))
      (else
       (let ((d* (guard (e (#t #f)) (read (open-string-input-port (car ls))))))
         (if (and (list? d*) (= 4 (length d*)) (eq? (car d*) 'trace)
                  (eq? (cadr d*) 'fsync))
             (loop (cdr ls) (cons (let ((x (caddr d*)))
                                    (if (symbol? x) (symbol->string x) x))
                                  acc))
             (loop (cdr ls) acc)))))))
(define (flushed? text needle)
  (let loop ((ps (flushed-paths text)))
    (cond ((null? ps) #f)
          ((has-substring? (car ps) needle) #t)
          (else (loop (cdr ps))))))
;; THE DIRECTORY ITSELF, not any path under it. "writers/M" is a
;; substring of "writers/M/quarantine.sexp", so a barrier that flushed
;; only the file satisfied the directory assertion too -- and the file
;; and the entry are two different obligations.
(define (flushed-exactly? text path)
  (let loop ((ps (flushed-paths text)))
    (cond ((null? ps) #f)
          ((string=? (car ps) path) #t)
          (else (loop (cdr ps))))))

;; A version that arrives MID-SESSION. The load-time barrier cannot have
;; covered it, which is exactly the case a segment barrier misses.
(build!)
(define l22-trace "")
(want "a quarantine installed mid-session is flushed, with its directory, before the append"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (quarantine! M 2)
          (set! l22-trace
                (traced (lambda ()
                          (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                         '(put "w.3" ()))))))
          (let ((out (list (flushed-exactly? l22-trace (qpath M))
                           (flushed-exactly? l22-trace
                                             (string-append d "/writers/" M)))))
            (log-end! s)
            out)))
      (list #t #t))
;; THE POSITIVE CONTROL IS A REAL DEPENDENT WRITE. "The flush came first"
;; is satisfied by an implementation that flushes and then refuses
;; everything, or that flushes and then ignores the metadata entirely.
(build!)
(want "and the write that depended on it actually succeeded, under the new boundary"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (quarantine! M 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (let* ((v (session-view s))
                 (res (session-append! s (make-frame (view-revision v) (view-epoch v)
                                                     (view-writer v) (view-expect-seq v)
                                                     "agent:claude" '() '(put "w.3" ()))))
                 (have (cdr (assq 'contiguous (cdr (assoc M (session-frontiers s)))))))
            (log-end! s)
            (list (car res) have))))
      (list 'committed 1))
;; A version present at load and never changed afterwards is still a
;; version the session depends on.
(build!)
(quarantine! M 2)
(define l22-load-trace "")
(want "a version already present at load is flushed too, before any dependent write"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s #f))
          (set! l22-load-trace
                (traced (lambda ()
                          (set! s (log-begin d (lambda args 'applied))))))
          (let ((out (list (flushed-exactly? l22-load-trace (qpath M))
                           (flushed-exactly? l22-load-trace
                                             (string-append d "/writers/" M)))))
            (log-end! s)
            out)))
      (list #t #t))

;; A VERSION THAT CHANGES is a different version, and the confirmation
;; is per version rather than per file: flushing "this file, once" would
;; leave the session depending on contents it never made durable.
(build!)
(quarantine! M 3)
(define l22-change-trace "")
(want "a version that changes mid-session is flushed again"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 3)) 'pending 'applied)))))
          ;; The first version was flushed at load. Now it changes.
          (quarantine! M 2)
          (set! l22-change-trace
                (traced (lambda ()
                          (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                         '(put "w.3" ()))))))
          (let ((out (flushed-exactly? l22-change-trace (qpath M))))
            (log-end! s)
            out)))
      #t)

(printf "== a version that cannot be made durable refuses the write ==\n")
;; In a child, because THEOURGIA_INJECT is an expansion-time gate and
;; fsync-fail is persistent within its stage: arming it here would fail
;; every barrier in the file.
(define child-path (string-append d "/l22-child.ss"))
(define child-out (string-append d "/l22-child.out"))
(define (write-child!)
  (put! child-path
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            ;; THE CHILD INSTALLS THE VERSION ITSELF, after log-begin. If
            ;; the quarantine were already there the LOAD-time barrier
            ;; would meet the fault first and the session would never
            ;; start -- correct, but it masks the append-time refusal
            ;; this row exists for.
            "(define res\n"
            "  (guard (e (#t (list 'raised)))\n"
            "    (parameterize ((log-clock (lambda () " (number->string fixed-ts) ")))\n"
            ;; The mirror's later records stay pending, so the fork does
            ;; not put the reducer's cursor past the new boundary and the
            ;; session reloads without needing a reset -- a reset would
            ;; withhold the view and this row would learn nothing.
            "      (let ((s (log-begin \"" d "\"\n"
            "                 (lambda (w seg off seq . rest)\n"
            "                   (if (and (string=? w \"" M "\") (>= seq 2)) 'pending 'applied)))))\n"
            "        (call-with-port (open-file-output-port \"" (qpath M) "\"\n"
            "                                              (file-options no-fail))\n"
            "          (lambda (p) (put-bytevector p (string->utf8 \"((format 1) (fork 2))\\n\"))))\n"
            ;; The first append absorbs the reload the new version
            ;; causes -- it is refused on the epoch, which is correct and
            ;; not what this row is about. The second is built from a
            ;; fresh view, so its outcome is about the barrier alone.
            "        (session-append! s (make-frame 0 0 \"" W "\" 3 \"agent:claude\" '()\n"
            "                                       '(put \"w.3\" ())))\n"
            "        (let* ((v (session-view s))\n"
            "               (r (if v\n"
            "                      (session-append! s (make-frame (view-revision v) (view-epoch v)\n"
            "                                                     (view-writer v) (view-expect-seq v)\n"
            "                                                     \"agent:claude\" '()\n"
            "                                                     '(put \"w.3\" ())))\n"
            "                      (list 'no-view))))\n"
            "          (log-end! s) r)))))\n"
            "(printf \"~s ~s\\n\" (car res) (file-size \"" (wpath W 1) "\"))\n"))))
(define (child-says fault)
  (build!)
  (write-child!)
  (system (string-append (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
                         "scheme --script " child-path " > " child-out " 2>/dev/null"))
  (let ((text (let ((b (slurp child-out))) (if (bytevector? b) (utf8->string b) ""))))
    (guard (e (#t (list 'unreadable text)))
      (let ((p (open-string-input-port text))) (list (read p) (read p))))))
(want "CONTROL: with nothing armed the dependent write commits"
      (child-says #f) (list 'committed (* 3 R)))
(want "a version whose flush fails refuses the write, and the log is untouched"
      (child-says "fsync-fail@deliver-barrier:file=quarantine.sexp")
      (list 'refused-before-reserve (* 2 R)))

(printf "== a version reinstalled is a new installation ==\n")
;; A certificate proves that those bytes were once made durable. It does
;; not prove that the rename which put them BACK has been -- and a crash
;; in that gap restores the version in between.
(build!)
(quarantine! M 3)
(want "A then B then A is flushed all three times, not twice"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 3)) 'pending 'applied))))
              (n 0))
          (define (append-and-count!)
            (let ((t (traced (lambda ()
                               (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                              '(put "w.3" ())))))))
              (if (flushed-exactly? t (qpath M)) 1 0)))
          (let* ((a (begin (quarantine! M 2) (append-and-count!)))
                 (b (begin (quarantine! M 3) (append-and-count!)))
                 (c (begin (quarantine! M 2) (append-and-count!)))
                 (again (append-and-count!)))
            (log-end! s)
            ;; The fourth append changes nothing, so it flushes nothing:
            ;; the confirmation is per installation, not per call.
            (list a b c again))))
      (list 1 1 1 0))

(printf "== a rejection is a boundary, not a blanket ==\n")
;; Suppressing the writer entirely threw away the valid prefix BELOW the
;; rejected record, so a replay rebuilt state without history that was
;; never in question.
(build!)
(define replayed '())
(want "after a reset, the records below the rejected one are replayed"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (set! replayed (cons (list w seq) replayed))
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (session-reject! s (session-epoch s) (cons M 2) 'no-such-verb)
          (session-applied! s 0 (list (cons W 2)))
          (set! replayed '())
          ;; A fork in the LOCAL writer's history forces a reset.
          (quarantine! W 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          ;; The reset has to be acknowledged for the replay to happen at
          ;; all -- without this the row was asserting the contents of a
          ;; replay that never ran.
          (session-reset-done! s (session-epoch s))
          (let ((out (reverse replayed)))
            (log-end! s)
            (list (filter (lambda (e) (string=? (car e) M)) out)))))
      (list (list (list M 1))))

(printf "== a rejection the reducer returned lasts too ==\n")
(build!)
(define seen-again '())
(want "a writer the callback rejected is not delivered again after a reload"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (set! seen-again (cons (list w seq) seen-again))
                                (cond ((and (string=? w M) (= seq 2))
                                       (list 'rejected 'no-such-verb))
                                      (else 'applied))))))
          (session-applied! s 0 (list (cons W 2)))
          (set! seen-again '())
          (put! (wpath M 2) (cat (r "m" 4) (r "m" 5)))
          (publish! 1 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (let ((out (reverse seen-again)))
            (log-end! s)
            (length (filter (lambda (e) (string=? (car e) M)) out)))))
      0)

(printf "== what was delivered is a fact about an epoch ==\n")
(build!)
(want "a record delivered before a reload cannot be rejected under the new epoch"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (session-applied! s 0 (list (cons W 2)))
          ;; The fork removes m.2 and m.3, so neither is delivered in the
          ;; new epoch. m.2 WOULD be re-delivered by a fork above it --
          ;; and rejecting it then is perfectly proper, which is why the
          ;; record chosen here is one the new boundary has taken away.
          (quarantine! M 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (let ((res (session-reject! s (session-epoch s) (cons M 3) 'no-such-verb)))
            (log-end! s)
            (list (car res) (cadr res)))))
      (list 'refused 'not-delivered))

(printf "== the barrier precedes the delivery a reload performs ==\n")
;; A reload delivers the records the new metadata ADMITS, and delivery
;; implies durability -- so a barrier that runs afterwards has already
;; let the reducer see records whose manifest may not survive. The fault
;; is aimed at the manifest, and the child reports how many mirrored
;; records it was handed.
;;
;; THE CHILD COMPUTES THE MANIFEST ITSELF rather than being handed its
;; text: embedding a string full of quotes into a generated script was a
;; escaping problem with no upside.
(define reload-child (string-append d "/reload-child.ss"))
(define reload-out (string-append d "/reload-child.out"))
(define (write-reload-child!)
  (put! reload-child
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi)\n"
            "        (only (igropyr crypto) sha256 bytevector->hex))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(define (slurp p)\n"
            "  (call-with-port (open-file-input-port p)\n"
            "    (lambda (q) (let ((b (get-bytevector-all q)))\n"
            "                  (if (eof-object? b) (make-bytevector 0) b)))))\n"
            "(define n 0)\n"
            "(define res\n"
            "  (guard (e (#t (list 'raised)))\n"
            "    (parameterize ((log-clock (lambda () " (number->string fixed-ts) ")))\n"
            "      (let ((s (log-begin \"" d "\"\n"
            "                 (lambda (w seg off seq . rest)\n"
            "                   (when (string=? w \"" M "\") (set! n (+ n 1)))\n"
            "                   'applied))))\n"
            "        (set! n 0)\n"
            "        (call-with-port (open-file-output-port\n"
            "                          \"" (string-append d "/writers/" M "/published.sexp") "\"\n"
            "                          (file-options no-fail))\n"
            "          (lambda (p)\n"
            "            (put-bytevector p (string->utf8\n"
            "              (string-append \"((1 . \\\"\"\n"
            "                (bytevector->hex (sha256 (slurp \"" (wpath M 1) "\")))\n"
            "                \"\\\") (2 . \\\"\"\n"
            "                (bytevector->hex (sha256 (slurp \"" (wpath M 2) "\")))\n"
            "                \"\\\"))\\n\")))))\n"
            "        (let ((r (session-append! s (make-frame 0 0 \"" W "\" 3 \"agent:claude\" '()\n"
            "                                                '(put \"w.3\" ())))))\n"
            "          (log-end! s) r)))))\n"
            "(printf \"~s ~s\\n\" (car res) n)\n"))))
;; NO MANIFEST AT LOAD. With one present the load-time barrier meets the
;; fault first and the session never starts -- correct, but it masks the
;; mid-session case this row is for. Without it the mirror's segments are
;; simply ignored until the child installs the manifest.
(define (reload-child-says fault)
  (build!)
  (system (string-append "rm -f " d "/writers/" M "/published.sexp"))
  (put! (wpath M 2) (cat (r "m" 4) (r "m" 5)))
  (write-reload-child!)
  (system (string-append (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
                         "scheme --script " reload-child " > " reload-out " 2>/dev/null"))
  (let ((t (let ((b (slurp reload-out))) (if (bytevector? b) (utf8->string b) ""))))
    (guard (e (#t (list 'unreadable t)))
      (let ((p (open-string-input-port t))) (list (read p) (read p))))))
;; THE APPEND IS REFUSED IN BOTH CASES and for different reasons -- on
;; the epoch when the reload succeeds, on the barrier when it does not --
;; so what this row reads is the DELIVERY COUNT. That is the thing the
;; ordering decides.
(want "CONTROL: with nothing armed the newly admitted records are delivered"
      (reload-child-says #f) (list 'refused-before-reserve 5))
(want "with the manifest's flush failing, nothing is delivered at all"
      (reload-child-says "fsync-fail@deliver-barrier:file=published.sexp")
      (list 'refused-before-reserve 0))

(printf "== the reset acknowledgement re-establishes the barrier ==\n")
;; The replay is a delivery too, so a version that changed between the
;; reset and its acknowledgement must be made durable before the records
;; it admits are handed over.
(build!)
(define reset-trace "")
(want "a version changed during reset-pending is flushed by the acknowledgement"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (session-applied! s 0 (list (cons W 2) (cons M 3)))
          (quarantine! M 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          ;; still reset-pending here; the marker changes again
          (quarantine! M 1)
          (set! reset-trace
                (traced (lambda () (session-reset-done! s (session-epoch s)))))
          (let ((out (flushed-exactly? reset-trace (qpath M))))
            (log-end! s)
            out)))
      #t)

(printf "\n~a failures\n" bad)
(printf "log17 complete\n")
