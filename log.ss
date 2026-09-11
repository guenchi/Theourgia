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

;;; (theourgia log) -- the log layer: segments, manifests, replay,
;;; append, publication and the snapshot frame.
;;;
;;; THIS FILE IS BEING BUILT IN PIECES, and this is the first: whole-file
;;; atomic replacement, segment enumeration, and the manifest. They come
;;; first because everything above them is stated in terms of them --
;;; which segments participate in loading is a manifest question, and
;;; every small file this library replaces goes through one primitive.
;;;
;;; ONE ATOMIC WRITE, AND IT IS OURS. Section 4.5 names a single
;;; sequence for every whole-file replacement (meta, instance, owner,
;;; retired, published, quarantine, the registry, snapshots):
;;;
;;;   tmp -> write-all! -> fsync file -> rename over -> fsync directory
;;;
;;; built on this library's own FFI rather than on (igropyr durable),
;;; and the reason is not preference. Fault injection has to be able to
;;; reach these writes -- L18 and L19 exist to break exactly them -- and
;;; that helper has no injection seam. One implementation, one place to
;;; inject.
;;;
;;; WHICH SEGMENTS PARTICIPATE IS NOT THE SAME QUESTION AS WHICH FILES
;;; EXIST, and conflating them is how an unfinished publication becomes
;;; history. For another writer, a segment counts only if `published.sexp`
;;; lists it (section 9.6): a file that exists but is unlisted is a
;;; publication that did not finish, and loading ignores it so that the
;;; next synchronisation can redo it. For the local writer there is no
;;; manifest -- it owns its own directory and every well-named segment
;;; in it counts. A retired local writer is read by the prefix its
;;; `retired.sexp` records, extended later by published segments that
;;; verify against it.
;;;
;;; THE CURRENT SEGMENT IS THE HIGHEST-NUMBERED ONE. No renaming, no
;;; mode bits, no marker file: rotation creates N+1 and that is the
;;; whole of the state change, so recovery has only two observable
;;; states and both are legal (section 4.4).

(library (theourgia log)
  (export directory-entry-durable!
          discover-prefix
          discovery? discovery-origin discovery-end-segment discovery-end-offset discovery-end-seq
          discovery-segment-ranges discovery-physical-current discovery-current-buffer
          discovery-torn discovery-integrity discovery-quarantine discovery-retired
          discovery-versions discovery-retired-tail discovery-clean?
          log-clock registry-path machine-lock-path instance-install!
          store-register!
          session-retired? owner-install!
          session-reset-done! session-reject! session-reset-pending
          log-open log-open-in-session load-prefix load-writers load-integrity
          log-begin log-end! session? session-store session-epoch session-writer
          session-append!
          session-frontiers session-view session-applied! session-load
          make-frame frame? frame-view-id frame-epoch frame-writer
          frame-expect-seq frame-actor frame-deps frame-payload
          view? view-revision view-epoch view-writer view-expect-seq
          view-applied-cut
          with-store-operation store-operation-active?
          load-commit! load-abort! load-outcome load-deliver!
          load-snapshot-cut load-snapshot-rows load-snapshot-reason
          snapshot-write! snapshot-read snapshot-cut-supported?
          session-snapshot!
          scan-segment
          atomic-write!
          segment-file-name segment-file-number
          store-writers writer-directory
          enumerate-segment-files
          read-manifest write-manifest! manifest-segments
          log-error? log-error-kind log-error-writer log-error-segment
          log-error-offset log-error-detail make-log-error)
  (import (chezscheme)
          (theourgia ffi)
          (theourgia trace)
          (only (theourgia crc32) crc32-hex)
          (only (igropyr crypto) sha256 bytevector->hex)
          (only (theourgia wire)
                sexpr->string-extended string->sexpr-extended decode-line
                escape-newlines encode-record storable-encode))

  ;; ---- errors -------------------------------------------------------------

  ;; A RECORD, NOT A TAGGED VECTOR, and deliberately unlike the rest of
  ;; this tree. (igropyr durable)'s own comment records a sibling vector
  ;; that grew from two fields to three, after which callers matching on
  ;; vector-length failed silently against the new one. This one has six
  ;; things to say on the day it is written and will grow; a record
  ;; cannot be mis-destructured, and adding a field to it does not
  ;; quietly change what an existing reader sees.
  ;;
  ;; Filesystem failures still arrive as (theourgia ffi)'s
  ;; #(durable-error op (target . errno)). Two shapes, because they are
  ;; two kinds of fact: one is "this log is not what it should be", the
  ;; other is "the world refused". A caller wanting both catches both.
  ;; R6RS derives make-log-error, log-error? and the five accessors from
  ;; this form; naming them again by hand would be a second declaration
  ;; of the same thing.
  (define-record-type log-error
    (fields kind writer segment offset detail))

  ;; ---- atomic whole-file replacement --------------------------------------

  ;; THE TEMPORARY NAME IS NOT SHARED. Two processes replacing the same
  ;; target must not meet in one temporary file and interleave into it,
  ;; because the loser's bytes are the ones the rename would install.
  ;; The process id separates processes and the counter separates
  ;; replacements within one.
  (define temp-counter (box 0))

  (define (temp-name-for path)
    (set-box! temp-counter (+ 1 (unbox temp-counter)))
    (string-append path ".tmp-"
                   (number->string (process-id)) "-"
                   (number->string (unbox temp-counter))))

  (define (parent-directory path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) ".")
        ((char=? (string-ref path i) #\/)
         (if (= i 0) "/" (substring path 0 i)))
        (else (loop (- i 1))))))

  ;; THE DIRECTORY FLUSH IS THE STEP THAT GETS LEFT OUT, and leaving it
  ;; out passes every test that writes and reads back on a machine that
  ;; does not lose power. A rename is a change to a directory, and a
  ;; directory is a file that also has to be flushed; without it the
  ;; rename lives in the kernel's memory and a crash can leave neither
  ;; the old contents nor the new.
  ;;
  ;; THE TEMPORARY IS LEFT BEHIND WHEN A LATER STEP FAILS. Its contents
  ;; are complete once the write returns, and the target still holds
  ;; whatever it held before; deleting it would destroy the only copy of
  ;; what the caller handed over. A stray file an operator can find
  ;; beats bytes nobody can. It IS removed when the write itself fails,
  ;; because then it holds nothing worth keeping.
  ;; THE TEMPORARY IS CREATED EXCLUSIVELY, and that is not tidiness. The
  ;; first version created it with "open if present, create if not", so
  ;; a temporary left behind by an earlier failure could be REUSED: the
  ;; operating system reissues process ids, the per-process counter
  ;; starts again at one, and the name collides. Opening it without
  ;; truncation and writing something shorter leaves the new contents
  ;; followed by the old tail -- and then the fsync and the rename
  ;; succeed, installing a file that is part new and part stale. That is
  ;; the one outcome an atomic replacement exists to prevent, and the
  ;; policy of keeping failed temporaries for recovery is what made it
  ;; reachable.
  (define (create-temp! path)
    (let loop ((tries 0))
      (when (> tries 64)
        (raise (make-log-error 'temp #f #f #f (list (cons 'path path)))))
      (let ((tmp (temp-name-for path)))
        (if (file-exists? tmp)
            (loop (+ tries 1))
            ;; ONLY "IT ALREADY EXISTS" IS A COLLISION. Catching every
            ;; exception here turned an unwritable directory, a read-only
            ;; filesystem or an exhausted descriptor table into 65
            ;; pointless retries and then a generic error that named
            ;; neither the cause nor the candidate. Anything that is not
            ;; a name clash is the caller's answer and is re-raised
            ;; unchanged.
            (if (file-create-exclusive! tmp)
                tmp
                (loop (+ tries 1)))))))

  ;; THE STAGE IS AN ARGUMENT, NOT A DEFAULT. Fault targeting is
  ;; <fault>@<stage>[:<path>] and a call site that declares no stage
  ;; cannot be aimed at -- so with no stage here, every fault selecting
  ;; `snapshot` or `publish` was silently inert and the run looked like
  ;; the injection had not fired. Hard-coding one stage inside would be
  ;; worse: this one sequence serves the snapshot, the manifest, the
  ;; registry and the repair, and they are different phases.
  (define (atomic-write! path bytes stage)
    (unless (string? path)
      (assertion-violation 'atomic-write! "path must be a string" path))
    (unless (symbol? stage)
      (assertion-violation 'atomic-write! "stage must be a symbol" stage))
    ;; A trailing slash names a directory; renaming a file over one
    ;; fails after the temporary has been written, which leaves a stray
    ;; file and reports the failure at the wrong step.
    (when (and (> (string-length path) 0)
               (char=? (string-ref path (- (string-length path) 1)) #\/))
      (assertion-violation 'atomic-write! "target must not end in a slash" path))
    (unless (bytevector? bytes)
      (assertion-violation 'atomic-write! "contents must be a bytevector" bytes))
    (parameterize ((theourgia-stage stage))
    (let ((tmp (create-temp! path))
          (dir (parent-directory path)))
      (let ((fd (fd-open tmp '(write)))
            (open? (box #t)))
        ;; dynamic-wind, not a guard: a guard does not run when the body
        ;; is left through a continuation, and this descriptor must not
        ;; outlive the call by any exit.
        (dynamic-wind
          void
          (lambda ()
            ;; ONLY THE WRITE IS GUARDED. The stated policy is that an
            ;; incomplete temporary is removed and a complete one is
            ;; kept -- and the first version guarded the flush as well,
            ;; so a failed fsync DELETED a temporary whose contents were
            ;; complete. The comment said one thing and the code did the
            ;; other.
            (guard (e (#t
                       (when (unbox open?)
                         (set-box! open? #f)
                         (guard (e2 (#t (void))) (fd-close fd)))
                       (guard (e2 (#t (void))) (unlink! tmp))
                       (raise e)))
              (write-all! fd bytes tmp))
            (fsync! fd tmp)
            ;; CLOSED HERE, WHERE ITS FAILURE CAN STILL STOP THE INSTALL.
            ;; Leaving the close to the unwind alone was a regression:
            ;; that path swallows errors so that it cannot replace an
            ;; exception already on its way out, and a close failing with
            ;; EIO after a successful write and flush then vanished --
            ;; the rename went ahead and this returned success. The flag
            ;; makes the two closes exclusive: this one on the way
            ;; through, the quiet one only on the way out.
            (set-box! open? #f)
            (fd-close fd))
          (lambda ()
            (when (unbox open?)
              (set-box! open? #f)
              (guard (e (#t (void))) (fd-close fd))))))
      (rename-over! tmp path)
      (fsync-dir! dir)
      path)))

  ;; A NEWLY CREATED DIRECTORY'S OWN ENTRY IS NOT MADE DURABLE BY
  ;; FLUSHING WHAT IS INSIDE IT. atomic-write! flushes the target's
  ;; immediate parent, so creating writers/U/ and then durably writing
  ;; writers/U/owner.sexp flushes U/ and never writers/ -- and a power
  ;; loss can then take U away while the successor and the registry
  ;; still name it. Exposed now, before the installs that need it are
  ;; written: adopt must confirm U's entry in writers/ before completing
  ;; owner installation, repair must confirm damaged/'s entry before
  ;; replacing an original, and an existence check is not enough after
  ;; an interrupted earlier attempt.
  (define (directory-entry-durable! path stage)
    (unless (symbol? stage)
      (assertion-violation 'directory-entry-durable! "stage must be a symbol" stage))
    (parameterize ((theourgia-stage stage))
      (fsync-dir! (parent-directory path))))

  ;; ---- segment names ------------------------------------------------------

  (define segment-digits 6)
  (define segment-suffix ".sexp")

  (define (segment-file-name n)
    (unless (and (integer? n) (exact? n) (> n 0))
      (assertion-violation 'segment-file-name "segment number must be positive" n))
    (let ((s (number->string n)))
      (when (> (string-length s) segment-digits)
        (assertion-violation 'segment-file-name "segment number too large" n))
      (string-append
        (make-string (- segment-digits (string-length s)) #\0)
        s segment-suffix)))

  ;; STRICT, AND THAT STRICTNESS IS WHAT KEEPS THE OTHER FILES OUT.
  ;; owner.sexp, retired.sexp, published.sexp and quarantine.sexp all sit
  ;; in the same directory and all end in .sexp; a loose match would
  ;; enumerate them as segments. Exactly six digits and nothing else.
  ;; Returns #f rather than raising: a directory may legitimately hold
  ;; files this layer does not know about, and a name it does not
  ;; recognise is not an error, it is not a segment.
  (define (segment-file-number name)
    (and (string? name)
         (= (string-length name) (+ segment-digits (string-length segment-suffix)))
         (let loop ((i 0))
           (cond
             ((= i segment-digits)
              (and (string=? (substring name segment-digits (string-length name))
                             segment-suffix)
                   (let ((n (string->number (substring name 0 segment-digits) 10)))
                     (and n (exact? n) (integer? n) (> n 0) n))))
             ((char<=? #\0 (string-ref name i) #\9) (loop (+ i 1)))
             (else #f)))))

  ;; ---- directories --------------------------------------------------------

  (define (writer-directory store writer)
    (string-append store "/writers/" writer))

  ;; Every subdirectory of writers/ is a writer id. A writer id is 8
  ;; base36 characters (section 1.1); anything else in there is not one,
  ;; and is skipped rather than refused for the same reason a stray file
  ;; is skipped above.
  (define (writer-id? name)
    (and (string? name)
         (= (string-length name) 8)
         (let loop ((i 0))
           (or (= i 8)
               (let ((c (string-ref name i)))
                 (and (or (char<=? #\0 c #\9) (char<=? #\a c #\z))
                      (loop (+ i 1))))))))

  (define (store-writers store)
    (let ((dir (string-append store "/writers")))
      (if (not (file-is-directory? dir))
          '()
          (list-sort string<? (filter writer-id? (directory-entries dir))))))

  ;; A SEGMENT MUST BE A REGULAR FILE. The name check alone accepts a
  ;; fifo called 000002.sexp, and opening one for reading blocks until a
  ;; writer appears -- inside the shared lock, which would then be held
  ;; forever and block every exclusive operation on the store. A symlink
  ;; to an endless byte source is the same shape. Neither can be
  ;; recovered from by an exception handler, because nothing raises.
  (define (enumerate-segment-files store writer)
    (let ((dir (writer-directory store writer)))
      (if (not (file-is-directory? dir))
          '()
          (list-sort < (filter
                    (lambda (n) n)
                    (map (lambda (name)
                           (let ((n (segment-file-number name)))
                             (and n
                                  (file-is-regular? (string-append dir "/" name))
                                  n)))
                         (directory-entries dir)))))))

  ;; ---- the manifest -------------------------------------------------------

  ;; ((<segment number> . "<hex sha256>") ...), ascending, written whole
  ;; through atomic-write!. Section 9.6 fixes the content -- segment
  ;; numbers and hashes -- and this fixes the shape.
  ;;
  ;; A MISSING MANIFEST IS NOT AN EMPTY ONE. #f means this writer has no
  ;; manifest at all, which is the ordinary state of the local writer and
  ;; of a retired prefix; '() means a manifest exists and lists nothing,
  ;; which is a mirrored writer whose publications have all been
  ;; unfinished. The two lead to different decisions, so they are
  ;; different answers.
  (define (manifest-path store writer)
    (string-append (writer-directory store writer) "/published.sexp"))

  (define (read-manifest store writer)
    (let ((path (manifest-path store writer)))
      (and (file-exists? path)
           (let ((text (utf8->string (read-whole path))))
             (let ((datum (guard (e (#t 'bad)) (string->sexpr-extended text))))
               (if (valid-manifest? datum)
                   datum
                   (raise (make-log-error 'manifest writer #f #f
                                          (list (cons 'path path))))))))))

  (define (valid-manifest? d)
    (and (list? d)
         (let loop ((xs d) (last 0))
           (or (null? xs)
               (let ((e (car xs)))
                 (and (pair? e)
                      (integer? (car e)) (exact? (car e)) (> (car e) last)
                      (string? (cdr e))
                      (loop (cdr xs) (car e))))))))

  (define (manifest-segments manifest)
    (if manifest (map car manifest) '()))

  (define (manifest-hash manifest n)
    (let ((e (and manifest (assv n manifest)))) (and e (cdr e))))

  (define (write-manifest! store writer entries)
    (unless (valid-manifest? entries)
      (assertion-violation 'write-manifest!
                           "entries must be ascending (number . hash) pairs" entries))
    (atomic-write! (manifest-path store writer)
                   (string->utf8 (string-append (sexpr->string-extended entries) "\n"))
                   'publish))

  ;; ---- scanning one segment ------------------------------------------------

  ;; ONE RECOVERABLE SHAPE, AND IT IS THE ONLY ONE. A crash can interrupt
  ;; write-all! part way through a line, and the visible result is a
  ;; final line with no terminating newline. That is the whole of what
  ;; automatic recovery covers (section 4.4-prime). Everything else --
  ;; a line that HAS its newline and fails its CRC, a sequence that
  ;; jumps, repeats or goes backwards, anything wrong in a sealed
  ;; segment -- is an integrity error that stops the writer, because it
  ;; is indistinguishable from bytes that were acknowledged and then
  ;; damaged.
  ;;
  ;; A TORN TAIL IN A SEALED SEGMENT IS NOT A TORN TAIL. The shape is
  ;; identical; what differs is whether anything is allowed to be
  ;; writing there. Only the local writer's current segment can hold an
  ;; interrupted write, so `recoverable-tail?` is the caller's statement
  ;; about which file this is, and it is not inferred here -- inferring
  ;; it would mean this function deciding, from bytes alone, a question
  ;; the bytes cannot answer.
  ;;
  ;; A BROKEN LINE FOLLOWED BY MORE BYTES FALLS OUT CORRECTLY without a
  ;; rule of its own: splitting at newlines makes the interrupted bytes
  ;; and whatever followed them into one line that ends with a newline,
  ;; so it is judged as a complete line and fails its CRC or its frame.
  ;; That is L4-prime variant c, and it wants an integrity error rather
  ;; than a truncation.
  ;;
  ;; THE OFFSET REPORTED IS THE START OF THE OFFENDING RECORD, not the
  ;; position of the byte that gave it away. L3 asserts the reported
  ;; (segment, offset, expected seq, actual seq) is exactly the
  ;; injection point, and a scanner that reported where it noticed --
  ;; the end of a line, or the parser's index -- would be off by the
  ;; length of the record in a way that still looks plausible.
  ;;
  ;; Returns one of:
  ;;   (complete <last-seq> <end-offset>)
  ;;   (torn <offset of the residual> <last valid seq>)
  ;;   (integrity <log-error>)
  ;; `deliver` is called per record as (deliver offset seq ts actor deps
  ;; payload) and may return the symbol stop to end the scan early, in
  ;; which case the outcome describes what had been consumed.
  (define (scan-segment bv writer segment expected-seq recoverable-tail? deliver)
    (unless (bytevector? bv)
      (assertion-violation 'scan-segment "not a bytevector" bv))
    (let ((n (bytevector-length bv)))
      (let loop ((start 0) (expect expected-seq) (last-seq #f))
        (cond
          ((>= start n) (list 'complete last-seq n))
          (else
           (let ((nl (find-newline bv start n)))
             (cond
               ;; no newline to the end: the residual
               ((not nl)
                (if recoverable-tail?
                    (list 'torn start last-seq)
                    (list 'integrity
                          (make-log-error 'torn-in-sealed writer segment start
                                          (list (cons 'bytes (- n start)))) last-seq start)))
               (else
                (let* ((end (+ nl 1))
                       (line (subbytes bv start end))
                       (r (decode-line line)))
                  (case (car r)
                    ((ok)
                     (let ((seq (cadr r)))
                       (cond
                         ;; THE ENVELOPE'S TYPES ARE CHECKED BEFORE ITS
                         ;; VALUES ARE COMPARED. decode-line establishes
                         ;; that a line is one datum of five elements; it
                         ;; does not establish that the first is a
                         ;; number. A crafted record such as
                         ;; (oops 1 "a" () (put "x" ())) has the right
                         ;; shape, and comparing its seq raised an
                         ;; ordinary exception that escaped replay
                         ;; entirely -- taking every other writer's
                         ;; delivery with it, which is exactly what
                         ;; per-writer scoping exists to prevent.
                         ((not (and (integer? seq) (exact? seq) (>= seq 0)))
                          (list 'integrity
                                (make-log-error 'frame writer segment start
                                                (list (cons 'reason 'seq-not-a-number))) last-seq start))
                         ((not (and (integer? (caddr r)) (exact? (caddr r))))
                          (list 'integrity
                                (make-log-error 'frame writer segment start
                                                (list (cons 'reason 'ts-not-a-number))) last-seq start))
                         ((and expect (not (= seq expect)))
                          (list 'integrity
                                (make-log-error 'seq writer segment start
                                                (list (cons 'expected expect)
                                                      (cons 'actual seq))) last-seq start))
                         (else
                          (let ((v (deliver start seq (caddr r) (cadddr r)
                                            (list-ref r 4) (list-ref r 5))))
                            (if (eq? v 'stop)
                                (list 'complete seq end)
                                (loop end (+ seq 1) seq)))))))
                    ((bad-crc)
                     (list 'integrity
                           (make-log-error 'crc writer segment start
                                           (list (cons 'bytes (- end start)))) last-seq start))
                    ((frame-error)
                     (list 'integrity
                           (make-log-error 'frame writer segment start
                                           (list (cons 'reason (cadr r)))) last-seq start))
                    ;; decode-line answers torn only without a trailing
                    ;; newline, and this branch has one.
                    (else
                     (list 'integrity
                           (make-log-error 'frame writer segment start
                                           (list (cons 'reason (car r)))) last-seq start)))))))))))) 

  (define (find-newline bv start end)
    (let loop ((i start))
      (cond
        ((>= i end) #f)
        ((= (bytevector-u8-ref bv i) 10) i)
        (else (loop (+ i 1))))))

  (define (subbytes bv start end)
    (let ((out (make-bytevector (- end start))))
      (bytevector-copy! bv start out 0 (- end start))
      out))

  ;; ---- the snapshot frame --------------------------------------------------

  ;; ONE DATUM PER LINE, AND ONE CHECKSUM FOR THE WHOLE FILE (section
  ;; 4.5-prime):
  ;;
  ;;   (snapshot 1 <cut>)
  ;;   ... rows, opaque to this layer ...
  ;;   (end "<crc32 hex of every byte above>")
  ;;
  ;; Records carry a checksum each because they are appended one at a
  ;; time and a torn one has to be told from a damaged one. A snapshot is
  ;; replaced whole, so it has one checksum and one verdict.
  ;;
  ;; NEVER PARTLY ADOPTED. A missing end line, a checksum that does not
  ;; match, a header that is not a header, a line that will not parse,
  ;; anything after the end line -- every one of them voids the WHOLE
  ;; file. There is no state in which some rows are used: the rows and
  ;; the cut are one frozen result, and half of a frozen result describes
  ;; a moment that never existed. The caller falls back to an older
  ;; snapshot or replays from empty.
  ;;
  ;; THE ROWS ARE DATA HERE. What a block, edge, tag or req row MEANS
  ;; belongs to the reducer; this layer owns the frame, the checksum and
  ;; the verdict.

  (define (snapshot-header? d)
    (and (list? d) (= 3 (length d))
         (eq? (car d) 'snapshot)
         (eqv? (cadr d) 1)
         (valid-cut? (caddr d))))

  ;; ((writer . seq) ...): writers are strings (section 1.1 -- a base36
  ;; id can begin with a digit, and the codec refuses such a symbol).
  (define (valid-cut? c)
    (and (list? c)
         (let loop ((xs c))
           (or (null? xs)
               (let ((e (car xs)))
                 (and (pair? e) (string? (car e))
                      (integer? (cdr e)) (exact? (cdr e)) (>= (cdr e) 0)
                      (loop (cdr xs))))))))

  (define (snapshot-write! path cut rows)
    (unless (valid-cut? cut)
      (assertion-violation 'snapshot-write! "cut must be ((writer . seq) ...)" cut))
    (unless (list? rows)
      (assertion-violation 'snapshot-write! "rows must be a list" rows))
    ;; NEWLINES INSIDE STRINGS ARE ESCAPED HERE FOR THE SAME REASON AS
    ;; IN A RECORD, and forgetting it here was a real defect rather than
    ;; a theoretical one: this frame is line-oriented, the codec's
    ;; writer emits a newline inside a string RAW, and a row carrying
    ;; (block "x" ((body . "a\nb"))) therefore wrote a file that was
    ;; checksummed correctly and then refused as unparseable when read
    ;; back. wire's escape is reused rather than repeated -- one supplier
    ;; of the rule, and its inverse is already in the reader that parses
    ;; these lines.
    (let* ((body (call-with-string-output-port
                   (lambda (p)
                     (put-string p (escape-newlines
                                     (sexpr->string-extended (list 'snapshot 1 cut))))
                     (put-char p #\newline)
                     (for-each (lambda (r)
                                 (put-string p (escape-newlines
                                                 (sexpr->string-extended r)))
                                 (put-char p #\newline))
                               rows))))
           (bytes (string->utf8 body))
           (whole (string-append body
                                 (sexpr->string-extended
                                   (list 'end (crc32-hex bytes)))
                                 "\n")))
      (atomic-write! path (string->utf8 whole) 'snapshot)))

  ;; Returns (values cut rows) when the file is whole, and
  ;; (values #f reason) when it is not. The reason is returned rather
  ;; than only traced because the caller has to report which category of
  ;; rejection happened (L7), and a caller that had to parse it back out
  ;; of a trace line would be reading its own debugging output as data.
  (define (snapshot-read path)
    (define (reject reason)
      (trace-event! 'snapshot-read (cons path reason) #f)
      (values #f reason))
    (if (not (file-exists? path))
        (values #f 'absent)
        (let* ((bytes (read-whole path))
               (n (bytevector-length bytes)))
          (cond
            ((= n 0) (reject 'empty))
            ((not (= (bytevector-u8-ref bytes (- n 1)) 10)) (reject 'no-end))
            (else
             (let ((lines (split-lines bytes)))
               (cond
                 ;; NO SPECIAL CASE FOR A SHORT FILE. A file holding only
                 ;; an end line has a valid checksum of nothing, and the
                 ;; header check is what should refuse it -- reporting
                 ;; "no end" for a file whose only line IS the end line
                 ;; names the wrong category, and the category is what
                 ;; L7 asserts.
                 (else
                  ;; The end line is the last one, and the checksum
                  ;; covers every byte before it -- which is why the
                  ;; split has to be on BYTES: recomputing from a
                  ;; re-serialised datum would checksum what this code
                  ;; can produce rather than what the file holds.
                  ;;
                  ;; BYTES AFTER THE END LINE THEREFORE READ AS no-end,
                  ;; not as a checksum failure: the end line has to be
                  ;; the last one, so anything following it means the
                  ;; last line is not an end line. That is the honest
                  ;; category and it needs no scan of its own -- looking
                  ;; for a stray end line among the rows would mean
                  ;; parsing before checking the checksum, which is the
                  ;; order records are careful not to use.
                  (let* ((last (car (reverse lines)))
                         (above (- n (+ (bytevector-length (cdr last)) 1)))
                         (end-datum (guard (e (#t 'bad))
                                      (string->sexpr-extended
                                        (utf8->string (cdr last))))))
                    (cond
                      ((not (and (list? end-datum) (= 2 (length end-datum))
                                 (eq? (car end-datum) 'end)
                                 (string? (cadr end-datum))))
                       (reject 'no-end))
                      ((not (string=? (cadr end-datum) (crc32-hex bytes 0 above)))
                       (reject 'crc))
                      (else
                       (let ((data (guard (e (#t 'bad))
                                     (map (lambda (l)
                                            (string->sexpr-extended
                                              (utf8->string (cdr l))))
                                          (reverse (cdr (reverse lines)))))))
                         (cond
                           ((eq? data 'bad) (reject 'parse))
                           ((null? data) (reject 'no-header))
                           ((not (snapshot-header? (car data))) (reject 'no-header))
                           (else
                            (trace-event! 'snapshot-read path n)
                            (values (caddr (car data)) (cdr data))))))))))))))))

  ;; (offset . bytes) per line, the newline removed. Bytes, not
  ;; characters, because the checksum is over bytes.
  (define (split-lines bv)
    (let ((n (bytevector-length bv)))
      (let loop ((start 0) (acc (list)))
        (if (>= start n)
            (reverse acc)
            (let ((nl (find-newline bv start n)))
              (if (not nl)
                  (reverse (cons (cons start (subbytes bv start n)) acc))
                  (loop (+ nl 1)
                        (cons (cons start (subbytes bv start nl)) acc))))))))

  ;; THE CUT MUST BE SUPPORTED BY WHAT IS ACTUALLY THERE. A snapshot
  ;; naming w.11 while the log holds w.10 is not a snapshot of anything
  ;; this store can reproduce, and adopting it would make replay start
  ;; after a record that does not exist (L7). `coverage` is what the
  ;; caller established by enumerating and scanning: ((writer . last
  ;; supported seq) ...). A writer named by the cut and absent from
  ;; coverage is unsupported -- absence is not zero, it is "this store
  ;; cannot speak for that writer at all".
  (define (snapshot-cut-supported? cut coverage)
    (unless (valid-cut? cut)
      (assertion-violation 'snapshot-cut-supported? "bad cut" cut))
    (let loop ((xs cut))
      (or (null? xs)
          (let* ((e (car xs))
                 (have (assoc (car e) coverage)))
            (and have (>= (cdr have) (cdr e)) (loop (cdr xs)))))))

  ;; ---- validated prefix discovery ------------------------------------------

  ;; THE SINGLE SUPPLIER. "How far does this writer's valid history
  ;; reach?" is answered here and nowhere else. It had two approximate
  ;; implementations once -- one scanning a single segment for coverage,
  ;; one scanning all of them for replay -- and they disagreed; three
  ;; defects came out of one round of patching them. Coverage, replay
  ;; and snapshot selection now consume one result.
  ;;
  ;; WHAT MAKES DELIVERY ABLE TO FULFIL VALIDATION IS OWNERSHIP, NOT
  ;; IMMUTABILITY. An earlier draft argued that sealed segments cannot
  ;; change, so a second read returns the same bytes. Section 9.7.8
  ;; explicitly permits atomic replacement by repair and by retirement
  ;; extension, and no acceptance row schedules one between the two
  ;; passes -- so that draft could have stayed green over a wrong split.
  ;; The lock is held across BOTH passes instead: a session already owns
  ;; the exclusive lock throughout, and a standalone load holds the
  ;; shared lock for the whole load. Version hashes cannot substitute:
  ;; in the repair and extension transactions the segment replacement
  ;; precedes the manifest installation, so equal versions do not mean
  ;; equal bytes.
  ;;
  ;; TWO PASSES, ONE SEGMENT OF MEMORY. Validation reads each segment,
  ;; verifies its manifest hash, frames and checksums every record,
  ;; advances the sequence counter and records that segment's seq range;
  ;; it keeps only the locked copy of the current segment. Delivery then
  ;; opens the segment CONTAINING THE CUT directly -- that is what the
  ;; ranges are for -- and suppresses only within it. It does not rescan
  ;; from the beginning suppressing everything below the cut: that is a
  ;; different promise and a whole extra traversal.

  (define-record-type discovery
    (fields origin end-segment end-offset end-seq segment-ranges
            physical-current current-buffer torn integrity
            quarantine retired versions retired-tail))

  ;; RETIRED-TAIL IS INFORMATION, NOT A DIAGNOSTIC. It is #f, or
  ;; (segment offset): the position at which a retired writer's file goes
  ;; on past the boundary the manifest vouches for. Those bytes are the
  ;; ordinary residue of retiring -- a half-written record, or records
  ;; that were never published -- so reporting them as integrity would
  ;; make the normal case look like corruption and would put a retired
  ;; writer's history in doubt. It sits outside `integrity` for that
  ;; reason: nothing decides load outcome from it, and discovery-clean?
  ;; does not consult it.

  (define (discovery-clean? p) (null? (discovery-integrity p)))

  ;; ORIGIN IS DECIDED BY owner.sexp, WHICH ONLY THIS STORE WRITES. A
  ;; mirror's first publication can link a segment and crash before
  ;; creating published.sexp, leaving a directory with neither file:
  ;; that is an interrupted publication, and every segment in it is
  ;; ignored -- not delivered, not read as a torn tail -- so the next
  ;; synchronisation can finish what it started.
  (define (origin-of store writer)
    (cond
      ((file-exists? (writer-file store writer "owner.sexp")) 'local)
      ((file-exists? (writer-file store writer "published.sexp")) 'mirrored)
      (else 'incomplete-publication)))

  (define (writer-file store writer name)
    (string-append (writer-directory store writer) "/" name))

  ;; call-with-port closes on a NORMAL return only, so an I/O error part
  ;; way through a read would leak the descriptor and a caller that
  ;; retries would leak one per attempt.
  (define (read-whole path)
    (let ((port (open-file-input-port path))
          (open? (vector #t)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let ((b (get-bytevector-all port)))
            (if (eof-object? b) (make-bytevector 0) b)))
        (lambda ()
          (when (vector-ref open? 0)
            (vector-set! open? 0 #f)
            (guard (e (#t (if #f #f))) (close-port port)))))))

  (define (quarantine-of store writer)
    (let ((p (writer-file store writer "quarantine.sexp")))
      (and (file-exists? p)
           (let* ((bytes (read-whole p))
                  (version (crc32-hex bytes))
                  (d (guard (e (#t #f))
                       (string->sexpr-extended (utf8->string bytes)))))
             (list version (and (list? d) (pair? d)
                                (let loop ((xs d))
                                  (cond
                                    ((null? xs) #f)
                                    ((and (list? (car xs)) (= 2 (length (car xs)))
                                          (eq? (caar xs) 'fork))
                                     (cadr (car xs)))
                                    (else (loop (cdr xs)))))))))))

  ;; THE READ IS NOT GUARDED, ONLY THE PARSE. Swallowing an I/O failure
  ;; here answers "this writer was never retired", which makes a retired
  ;; writer look active and exposes the evidence bytes past its
  ;; retirement boundary as history -- the most dangerous available
  ;; wrong answer. A malformed marker is an integrity error, not a
  ;; writer that may be replayed freely.
  (define (retired-of store writer)
    (let ((p (writer-file store writer "retired.sexp")))
      (and (file-exists? p)
           (let* ((bytes (read-whole p))
                  (version (crc32-hex bytes))
                  (d (guard (e (#t 'malformed))
                       (string->sexpr-extended (utf8->string bytes)))))
             (if (eq? d 'malformed)
                 (list 'malformed version)
                 (let loop ((xs (if (list? d) d '())))
                   (cond
                     ((null? xs) (list 'malformed version))
                     ((and (list? (car xs)) (= 4 (length (car xs)))
                           (eq? (caar xs) 'prefix)
                           (for-all (lambda (v) (and (integer? v) (exact? v)))
                                    (cdr (car xs))))
                      (append (cdr (car xs)) (list (transaction-of d) version)))
                     (else (loop (cdr xs))))))))))

  (define (transaction-of d)
    (let loop ((xs (if (list? d) d '())))
      (cond
        ((null? xs) #f)
        ((and (list? (car xs)) (= 2 (length (car xs))) (eq? (caar xs) 'tx))
         (cadr (car xs)))
        (else (loop (cdr xs))))))

  ;; THE VALIDATE PASS. Everything about this writer that goes wrong is
  ;; recorded in its own integrity list; nothing about this writer
  ;; escapes as an exception. A store- or runtime-scoped fault -- the
  ;; store lock, enumerating writers/, meta.sexp -- is a different kind
  ;; of fact and is raised, because recording a missing store lock as
  ;; the first-visited writer's damage would make L4's refusal pass for
  ;; the wrong reason. And a failed READ is never reported as an ABSENT
  ;; file.
  (define (discover-prefix store writer lock-context)
    (let ((origin (origin-of store writer)))
      (if (eq? origin 'incomplete-publication)
          (make-discovery origin #f #f 0 '() #f #f #f '() #f #f '() #f)
          (validate store writer origin lock-context))))

  (define (validate store writer origin lock-context)
    (let* ((quarantine (quarantine-of store writer))
           (retired (retired-of store writer))
           ;; A RETIRED WRITER READS ITS MANIFEST EVEN WHEN IT IS LOCAL.
           ;; The manifest is the sole authority for extension, so a
           ;; local writer that has retired is no longer the only voice
           ;; about its own segments: a mirror can publish the ones it
           ;; never got to.
           (manifest (if (and (eq? origin 'local) (not retired))
                         #f
                         (read-manifest-safely store writer)))
           (versions (list (cons 'manifest (manifest-version store writer))
                           (cons 'retired (and retired (car (reverse retired))))
                           (cons 'quarantine (and quarantine (car quarantine)))))
           (errs (vector '()))
           (note! (lambda (kind seg off detail)
                    (vector-set! errs 0 (cons (make-log-error kind writer seg off detail)
                                              (vector-ref errs 0))))))
      (cond
        ((eq? manifest 'malformed)
         (note! 'manifest #f #f '())
         (finish origin 0 '() #f #f errs quarantine retired versions))
        ((and retired (eq? (car retired) 'malformed))
         (note! 'retired-malformed #f #f '())
         (finish origin 0 '() #f #f errs quarantine retired versions))
        (else
         (let* ((present (enumerate-segment-files store writer))
                ;; ONE CEILING, APPLIED WHILE SCANNING. Both markers name
                ;; a boundary in the same coordinate -- a seq -- and both
                ;; used to be applied after the fact: retirement by
                ;; clipping bytes, quarantine by lowering end-seq alone.
                ;; Lowering only end-seq left end-segment, end-offset and
                ;; the ranges describing history the fork excludes, and
                ;; delivery walked those ranges straight into it. Stopping
                ;; the scan is what makes every coordinate agree, because
                ;; there is then only one place the extent is decided.
                (fork-ceiling (and quarantine (cadr quarantine)
                                   (- (cadr quarantine) 1)))
                (listed (manifest-segments manifest))
                ;; THE PREFIX IS READ BY LOCAL RULES, THE EXTENSION IS
                ;; NOT. Segments up to and including the one the marker
                ;; names are this store's own history and need no
                ;; manifest entry -- requiring one would lose the history
                ;; of every writer that retired before anything was
                ;; published. Everything above it exists only because
                ;; some mirror published it, so it must be listed.
                (rseg (and retired (not (eq? (car retired) 'malformed))
                           (car retired)))
                (roff (and rseg (cadr retired)))
                ;; WHERE THE MARKER'S OFFSET LANDS, observed while the
                ;; scan goes past it rather than inferred from the
                ;; endpoint afterwards. Slot 0 says the named segment was
                ;; actually scanned; slot 1 is the seq of the record that
                ;; ends exactly at the declared offset, or #f if no record
                ;; boundary is there at all. The endpoint cannot answer
                ;; this once extension is in play: it is then the end of
                ;; the file, not the end of the declared prefix.
                ;;
                ;; The exits that skip the scan -- an unreadable segment,
                ;; a hash the manifest disagrees with -- do not run the
                ;; marker check at all, so there is deliberately no
                ;; "was it scanned" slot here: a guard no reachable input
                ;; exercises is an excuse nobody can test.
                (probe (vector #f))
                ;; THE MARKER STOPS THE SCAN ONLY WHILE NOTHING HAS
                ;; PUBLISHED PAST IT. Once the prefix segment is in the
                ;; manifest, the declared seq is a lower bound that has
                ;; been superseded, and holding the scan there would
                ;; discard exactly the history extension exists to
                ;; recover. If that listing turns out to disagree with
                ;; the bytes, the salvage branch below removes the whole
                ;; segment, so no boundary is taken on trust either way.
                (retired-seq (and rseg (not (memv rseg listed))
                                  (caddr retired)))
                (ceiling (cond
                           ((and fork-ceiling retired-seq)
                            (min fork-ceiling retired-seq))
                           (fork-ceiling fork-ceiling)
                           (else retired-seq)))
                (segs (cond
                        (rseg (filter (lambda (n)
                                        (or (<= n rseg) (memv n listed)))
                                      present))
                        ((eq? origin 'local) present)
                        (else (filter (lambda (n) (memv n listed)) present))))
                (missing (cond
                           (rseg (filter (lambda (n)
                                           (and (> n rseg) (not (memv n present))))
                                         listed))
                           ((eq? origin 'local) '())
                           (else (filter (lambda (n) (not (memv n present))) listed))))
                (stop-before (and (pair? missing) (car (list-sort < missing))))
                (segs (if stop-before
                          (filter (lambda (n) (< n stop-before)) segs)
                          segs))
                (highest (and (pair? segs) (car (reverse segs))))
                (noted? (lambda (kind)
                          (let loop ((es (vector-ref errs 0)))
                            (cond ((null? es) #f)
                                  ((eq? (log-error-kind (car es)) kind) #t)
                                  (else (loop (cdr es)))))))
                ;; THE MARKER IS CHECKED AGAINST WHERE THE SCAN ACTUALLY
                ;; ARRIVED. A disagreement is recorded and nothing more:
                ;; the extent is already min(declared, verified), and a
                ;; claim shown to be wrong about this file is no safer a
                ;; bound than the bytes are.
                (verify-retired!
                  (lambda (end)
                    (when (and retired (not (eq? (car retired) 'malformed)))
                      (let ((rseg (car retired))
                            (roff (cadr retired))
                            (rseq (caddr retired))
                            (eseq (if end (caddr end) 0)))
                        (cond
                          ;; a lower fork stopped the scan before the
                          ;; claim could be reached, so there is nothing
                          ;; to check it against and nothing wrong with it
                          ((and fork-ceiling (< eseq rseq)) (if #f #f))
                          ((not (memv rseg present))
                           (note! 'retired-missing-segment rseg #f
                                  (list (cons 'declared rseq))))
                          ((noted? 'retired-beyond-file) (if #f #f))
                          ;; EXTENSION DOES NOT EXEMPT THE MARKER. extend
                          ;; preserves the prefix bytes, so on a faithfully
                          ;; extended file the declared offset still ends
                          ;; the declared record -- the file grew past it,
                          ;; the record boundary did not move. When it does
                          ;; not, the manifest and retired.sexp are two
                          ;; authorities contradicting each other about the
                          ;; same bytes; that is recorded, and the extent
                          ;; still follows the manifest.
                          ((eqv? (vector-ref probe 0) rseq) (if #f #f))
                          (else
                           (note! 'retired-mismatch rseg roff
                                  (list (cons 'declared rseq)
                                        (cons 'at-offset (vector-ref probe 0))))))))))
                (tail (and (eq? origin 'local) highest (not retired)
                           (capture-tail store writer highest lock-context))))
           ;; A ceiling below the first sequence admits nothing, and the
           ;; scanner has no way to decline a record it has already read:
           ;; returning 'stop stops AFTER the record, so this case has to
           ;; be answered before any segment is opened.
           (if (and ceiling (< ceiling 1))
             (finish-with origin #f '()
                          (physical-of store writer origin retired highest)
                          #f #f errs quarantine retired versions #f)
           ;; THE TAIL EXTENDS THE SEGMENT LIST, IT DOES NOT REPLACE IT.
           ;; Walking only the captured tail dropped every sealed segment
           ;; before it: a store whose segment 1 held records 1-2 and
           ;; whose freshly rotated segment 2 was empty reported an
           ;; extent of 0, because segment 1 was never visited. That is
           ;; L10's case and it is why the extent and the append target
           ;; are separate facts in the first place.
           (let loop ((ss (if tail
                              (append (filter (lambda (n) (< n (caar tail))) segs)
                                      (map car tail))
                              segs))
                      (expect 1) (ranges '()) (end #f) (torn #f) (buffer #f))
             (cond
               ((null? ss)
                (when stop-before
                  (note! 'manifest-missing-segment stop-before #f '()))
                (verify-retired! end)
                (finish-with origin end ranges
                             (physical-of store writer origin retired
                                          (if tail (car (reverse (map car tail))) highest))
                             buffer torn errs quarantine retired versions #f))
               (else
                (let* ((seg (car ss))
                       (from-tail (and tail (assv seg tail)))
                       (current? (and from-tail (eqv? seg (car (reverse (map car tail))))))
                       (bytes (if from-tail (cdr from-tail) (read-segment store writer seg)))
                       (want (manifest-hash manifest seg)))
                  (cond
                    ((eq? bytes 'unreadable)
                     (note! 'segment-unreadable seg #f '())
                     (finish-with origin end ranges
                                  (physical-of store writer origin retired highest)
                                  buffer torn errs quarantine retired versions #f))
                    ;; VERIFIED WHILE THE BYTES ARE IN HAND. Without it a
                    ;; published segment could be replaced by different
                    ;; content whose records each carry a correct CRC.
                    ;; Records inside a hash-mismatched file are repair
                    ;; EVIDENCE only: not in end-*, never delivered.
                    ((and want (not (string=? want (bytevector->hex (sha256 bytes)))))
                     (note! 'manifest-hash seg 0 (list (cons 'expected want)))
                     (finish-with origin end ranges
                                  (physical-of store writer origin retired highest)
                                  buffer torn errs quarantine retired versions #f))
                    (else
                     (let* ((clipped (begin
                                       (check-retirement-offset! bytes seg retired note!)
                                       bytes))
                            (outcome
                              (scan-segment clipped writer seg expect current?
                                            (lambda (off seq ts actor deps payload)
                                              ;; a record STARTING at the
                                              ;; declared offset means the
                                              ;; one before it ended there
                                              (when (and roff (eqv? seg rseg)
                                                         (= off roff))
                                                (vector-set! probe 0 (- seq 1)))
                                              ;; the scanner stops AFTER the
                                              ;; record it is told to stop on,
                                              ;; which is what makes the
                                              ;; ceiling inclusive
                                              (if (and ceiling (>= seq ceiling))
                                                  'stop
                                                  (if #f #f)))))
                            (probe-noted
                              (when (and roff (eqv? seg rseg))
                                ;; the other way the offset can be a
                                ;; boundary: it is where this segment's
                                ;; scan came to rest -- the file's end, or
                                ;; wherever the ceiling stopped it
                                (let ((endoff (case (car outcome)
                                                ((complete) (caddr outcome))
                                                ((torn) (cadr outcome))
                                                (else (cadddr outcome))))
                                      (lastseq (if (eq? (car outcome) 'complete)
                                                   (cadr outcome)
                                                   (caddr outcome))))
                                  (when (and endoff lastseq (= endoff roff))
                                    (vector-set! probe 0 lastseq))))))
                       (case (car outcome)
                         ((complete)
                          (let* ((last (cadr outcome))
                                 (ranges (add-range ranges seg (or expect 1)
                                                    (or last (- expect 1))))
                                 (end (if last
                                          (list seg (caddr outcome) last)
                                          end))
                                 (buffer (if current? (cons seg clipped) buffer)))
                            (if (or (and retired-seq
                                         (retirement-ends-here? seg retired))
                                    (and ceiling last (>= last ceiling)))
                                (begin
                                  (verify-retired! end)
                                  (finish-with origin end ranges
                                             (physical-of store writer origin retired highest)
                                             buffer torn errs quarantine retired versions
                                             ;; WHAT IS LEFT IN THE FILE
                                             ;; ABOVE THE BOUNDARY. Only
                                             ;; for a retired writer, and
                                             ;; only when the manifest
                                             ;; does not vouch for those
                                             ;; bytes -- when it does they
                                             ;; are history and the scan
                                             ;; never stopped here.
                                             (and retired-seq end
                                                  (= (car end) seg)
                                                  (> (bytevector-length clipped)
                                                     (cadr end))
                                                  (list seg (cadr end)))))
                                (loop (cdr ss) (if last (+ last 1) expect)
                                      ranges end torn buffer))))
                         ((torn)
                          ;; THE RECORDS BEFORE THE RESIDUAL ARE STILL
                          ;; HISTORY. Passing the incoming `end` through
                          ;; unchanged threw them away: a first segment
                          ;; holding a valid record 1 and an interrupted
                          ;; record 2 reported an extent of 0, because
                          ;; `end` had never been set -- the segment did
                          ;; not complete. The torn outcome carries the
                          ;; last valid seq precisely so the extent can
                          ;; stop AT it rather than before the segment.
                          (let* ((last (caddr outcome))
                                 (end (if last (list seg (cadr outcome) last) end)))
                            (finish-with origin end
                                         (add-range ranges seg (or expect 1)
                                                    (or last (- expect 1)))
                                         (physical-of store writer origin retired highest)
                                         (if current? (cons seg clipped) buffer)
                                         (list seg (cadr outcome)
                                               (or last (- expect 1)))
                                         errs quarantine retired versions #f)))
                         (else
                          ;; THE RECORDS BEFORE THE ERROR ARE STILL
                          ;; HISTORY -- the same defect the torn branch
                          ;; above was already fixed for, left standing
                          ;; in its sibling. A segment holding a valid
                          ;; record 1, a CRC-damaged record 2 and a
                          ;; valid record 3 reported an extent of 0 with
                          ;; no ranges at all, discarding record 1,
                          ;; which is CRC-valid and contiguous. The
                          ;; error stops the extent AT the last good
                          ;; record, it does not annul the segment.
                          ;;
                          ;; This is not the whole-segment exclusion a
                          ;; manifest-hash failure causes: there the
                          ;; file's identity is in doubt, so no record
                          ;; in it is history. Here the file is the
                          ;; right one and the damage is local.
                          (note-error! note! (cadr outcome))
                          (let ((last (caddr outcome)))
                            (finish-with origin
                                         (if last
                                             (list seg (cadddr outcome) last)
                                             end)
                                         (add-range ranges seg (or expect 1)
                                                    (or last (- expect 1)))
                                         (physical-of store writer origin retired highest)
                                         (if current? (cons seg clipped) buffer)
                                         torn errs quarantine retired
                                         versions #f)))))))))))))))))

  ;; A SEGMENT THAT CONTRIBUTED NOTHING HAS NO RANGE. Recording one
  ;; anyway produced entries like (2 4 3) -- "this segment holds
  ;; sequences 4 through 3" -- for a segment whose very first record was
  ;; rejected, or for a freshly rotated empty one. That is not a fact
  ;; about the store, it contradicts having stopped before the segment,
  ;; and delivery had to carry a special case to tolerate it.
  (define (add-range ranges seg from to)
    (if (and to from (>= to from))
        (cons (list seg from to) ranges)
        ranges))

  (define (note-error! note! e)
    (note! (log-error-kind e) (log-error-segment e) (log-error-offset e)
           (log-error-detail e)))

  (define (finish origin end-seq ranges phys buffer errs quarantine retired versions)
    (make-discovery origin #f #f end-seq ranges phys buffer #f
                 (reverse (vector-ref errs 0)) quarantine retired versions #f))

  ;; THE QUARANTINED SUFFIX IS EXCLUDED FROM THE EXTENT ITSELF, not
  ;; merely from snapshot eligibility -- otherwise ordinary replay
  ;; delivers it.
  ;;
  ;; It is excluded WHILE SCANNING, and this function no longer lowers
  ;; anything. Lowering end-seq here was the exclusion's second supplier:
  ;; end-segment, end-offset and the ranges kept describing history above
  ;; the fork, so the coordinates contradicted each other and delivery
  ;; followed the ranges into segments the fork had excluded. The extent
  ;; now arrives already correct in every coordinate, and there is one
  ;; place that decided it.
  (define (finish-with origin end ranges phys buffer torn errs quarantine retired versions tail)
    (make-discovery origin
                 (and end (car end)) (and end (cadr end)) (if end (caddr end) 0)
                 (reverse ranges) phys buffer torn
                 (reverse (vector-ref errs 0)) quarantine retired versions tail))

  ;; ---- the pieces validate leans on ----------------------------------------

  (define (read-manifest-safely store writer)
    (guard (e (#t 'malformed)) (read-manifest store writer)))

  (define (manifest-version store writer)
    (let ((p (writer-file store writer "published.sexp")))
      (and (file-exists? p) (crc32-hex (read-whole p)))))

  ;; A READ FAILURE IS NOT AN ABSENT FILE. Returning 'unreadable keeps
  ;; the two apart; answering with empty bytes would report a writer as
  ;; having no history when its history could not be read.
  (define (read-segment store writer seg)
    (guard (e (#t 'unreadable))
      (read-whole (string-append (writer-directory store writer)
                                 "/" (segment-file-name seg)))))

  ;; THE SEGMENT SET IS RE-ESTABLISHED INSIDE THE LOCK and the whole
  ;; tail is copied there: enumerating first and then queueing for the
  ;; lock reads a set that may be stale by the time it is granted, and a
  ;; reader that decided "N is current" before waiting would copy N and
  ;; never learn of a record committed before its own copy.
  ;;
  ;; THIS TAKES NO LOCK. The enclosing load owns one -- exclusive for a
  ;; session, shared for a standalone load -- for its whole duration,
  ;; and a helper that acquired its own would be the wrong shape twice
  ;; over: a second acquisition per writer instead of one lifetime for
  ;; the load, and, under held-exclusive, a shared acquisition inside an
  ;; exclusive session, which self-deadlocks because flock is per
  ;; descriptor. Measured before the fix: two flock events for a load
  ;; that must show one.
  (define (capture-tail store writer from-seg lock-context)
    (let ((got (let ((now (enumerate-segment-files store writer)))
                 (map (lambda (n) (cons n (read-segment store writer n)))
                      (filter (lambda (n) (>= n from-seg)) now)))))
      (for-each (lambda (e)
                  (unless (eq? (cdr e) 'unreadable)
                    (trace-event! 'copy
                                  (string-append (writer-directory store writer)
                                                 "/" (segment-file-name (car e)))
                                  (bytevector-length (cdr e)))))
                got)
      got))

  ;; physical-current is NOT end-*: L10 leaves N ending at seq 100 and a
  ;; freshly rotated N+1 empty, so the extent stays 100 while an append
  ;; goes to N+1. A mirrored or retired writer has no append target at
  ;; all, and saying so explicitly stops the highest file being taken
  ;; for a writable one.
  (define (physical-of store writer origin retired highest)
    (if (or (not (eq? origin 'local)) retired)
        'no-append-target
        (and highest
             (list highest
                   (guard (e (#t 0))
                     (file-size (string-append (writer-directory store writer)
                                               "/" (segment-file-name highest))))))))

  (define (retirement-ends-here? seg retired)
    (and retired (not (eq? (car retired) 'malformed)) (= seg (car retired))))

  ;; THE MARKER'S OFFSET IS CHECKED, IT NO LONGER CUTS. The seq is the
  ;; marker's primary coordinate and the scan ceiling is the one place
  ;; the extent is decided; clipping the bytes here as well made the
  ;; offset a second, disagreeing supplier of the same boundary. What is
  ;; left is the verification: an offset past the end of its own file is
  ;; recorded, and the extent is still whatever the sequences support.
  (define (check-retirement-offset! bytes seg retired note!)
    (when (retirement-ends-here? seg retired)
      (let ((off (cadr retired)))
        (when (> off (bytevector-length bytes))
          (note! 'retired-beyond-file seg off
                 (list (cons 'file-length (bytevector-length bytes))))))))

  ;; ---- the load transaction -------------------------------------------------

  ;; DELIVERY IS PROVISIONAL AND THE TRANSACTION IS THE WHOLE LOAD. A
  ;; writer spanning three segments has segments 1 and 2 applied by the
  ;; time segment 3 fails to open -- the lock worked, the state is
  ;; already in -- so "apply none of that writer" cannot be an ordinary
  ;; streaming branch. The reducer accumulates into a staging state that
  ;; no consumer sees; the load publishes once, at the end.
  ;;
  ;; THE GRANULARITY IS THE LOAD, NOT THE WRITER, because a snapshot
  ;; covering the failed writer must be rejected -- and the healthy
  ;; writers' history below its cut was deliberately skipped during
  ;; delivery, so removing the failed writer's rows cannot recover their
  ;; state. Only replay from an earlier baseline can.
  ;;
  ;; WHAT THIS CANNOT DO, said rather than implied: load-abort! discards
  ;; the staging root. It cannot undo a callback that mutated something
  ;; globally reachable, wrote a file, or leaked a staging reference.
  ;; That is the reducer's side of the provisional contract, and a
  ;; shallow copy is the easy way to break it -- a private root whose
  ;; nested mutable children still belong to committed state.

  (define-record-type load-session
    (fields store (mutable lock) (mutable prefixes) (mutable state)
            (mutable outcome) (mutable snapshot)))

  (define (log-open store)
    (open-load store 'acquire-shared))

  (define (log-open-in-session store)
    (open-load store 'held-exclusive))

  (define (open-load store lock-context)
    (unless (string? store)
      (assertion-violation 'log-open "store must be a path string" store))
    (let ((meta-path (string-append store "/meta.sexp")))
      (unless (file-exists? meta-path)
        (raise (make-log-error 'meta #f #f #f (list (cons 'path meta-path)))))
      (let ((meta (guard (e (#t #f))
                    (string->sexpr-extended (utf8->string (read-whole meta-path))))))
        (unless (and meta (list? meta) (format-1? meta))
          (raise (make-log-error 'meta #f #f #f
                                 (list (cons 'path meta-path) (cons 'supported 1)))))
        ;; THE SHARED LOCK BELONGS TO THE WHOLE LOAD -- enumeration and
        ;; every writer's discovery -- not to one discover-prefix call,
        ;; and every exit releases it.
        (let ((lock (if (eq? lock-context 'acquire-shared)
                        (lock-acquire! (string-append store "/lock") 'shared)
                        #f)))
          (guard (e (#t (when lock (lock-release! lock)) (raise e)))
            (let* ((writers (store-writers store))
                   (prefixes (map (lambda (w)
                                    (cons w (discover-prefix store w lock-context)))
                                  writers)))
              (let ((ls (make-load-session store lock prefixes '() 'open #f)))
                (load-session-snapshot-set! ls (select-snapshot store prefixes))
                ls)))))))


  ;; ---- the write session (section 5.2 / 5.2-prime) --------------------------

  ;; ONE ACTIVE OPERATION PER STORE, CHECKED IN THIS PROCESS. flock is
  ;; per open file description, so a second log-begin inside the first
  ;; would open its own descriptor and block forever waiting for a lock
  ;; this very thread is holding. A check cannot replace the lock -- it
  ;; says nothing about other processes -- but the lock cannot replace
  ;; the check either, because against yourself the lock does not fail,
  ;; it hangs. Publication and adopt take the same guard when they exist.
  (define active-operations (make-hashtable string-hash string=?))

  ;; KEYED BY THE DIRECTORY'S IDENTITY, NOT BY THE STRING. "/s" and
  ;; "/s/." and "/s/../s" are three keys and one directory, so a string
  ;; key let a nested call slip past the guard and then block on the
  ;; flock the outer call is holding -- the exact deadlock this check
  ;; exists to prevent, reachable by writing the path differently. The
  ;; path string is the fallback for a store whose directory cannot be
  ;; stat'ed, where nothing can be opened anyway.
  (define (store-key store)
    (guard (e (#t store))
      (call-with-values (lambda () (path-device-inode store))
        (lambda (dev ino)
          (string-append (number->string dev) ":" (number->string ino))))))

  (define (store-operation-active? store)
    (and (hashtable-ref active-operations (store-key store) #f) #t))

  (define (claim-store! store who)
    (let ((key (store-key store)))
      (when (hashtable-ref active-operations key #f)
        (raise (make-log-error 'active-operation #f #f #f
                               (list (cons 'store store) (cons 'attempted who)))))
      (hashtable-set! active-operations key #t)))

  (define (release-store! store)
    (hashtable-delete! active-operations (store-key store)))

  ;; The guard as a scope, for callers that are not log-begin. Releases on
  ;; every exit, including an escape, so a failed publication does not
  ;; wedge the store for the rest of the process.
  (define (with-store-operation store who thunk)
    (claim-store! store who)
    (let ((done (vector #f)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda () (let ((r (thunk))) (vector-set! done 0 #t) r))
        (lambda () (release-store! store)))))

  (define-record-type view
    (fields revision epoch writer expect-seq applied-cut))

  ;; THE FRAME CARRIES THE CONTEXT IT WAS PREPARED IN. Checking only the
  ;; sequence number and the current authority is not enough: a reset
  ;; changes the reduced state without changing either, so a frame
  ;; computed against the old state would still look current. seq and the
  ;; timestamp are the log layer's to assign and are deliberately absent.
  (define-record-type frame
    (fields view-id epoch writer expect-seq actor deps payload))

  ;; ONE SESSION, ONE LOCAL WRITER, resolved once at log-begin. Taking
  ;; the writer as an argument to session-view made "which writer does
  ;; this session write" a question with two suppliers -- the store's
  ;; owner file and whatever the caller passed -- and a frame naming a
  ;; different writer is then indistinguishable from a caller asking for
  ;; someone else's view.
  (define-record-type session
    (fields store lock (mutable load) on-deliver writer
            (mutable epoch) (mutable applied) (mutable revision)
            (mutable ended) (mutable poisoned)
            (mutable next-seq) (mutable unconfirmed)
            (mutable versions) (mutable reset-pending) (mutable rejected)
            (mutable delivered) (mutable barriered)))

  ;; DELIVERY IMPLIES DURABILITY, so the barrier is the session's
  ;; obligation and it runs before the first callback -- not per record,
  ;; and not after. What the reducer receives becomes the basis for deps
  ;; and snapshot cuts; a record it applied that a crash then removes
  ;; would leave those pointing at history that never existed. This
  ;; writer's own unflushed residue is included: it is the most likely
  ;; thing to be unflushed and the least likely to be noticed.
  (define (flush-file! path)
    (when (file-exists? path)
      (let ((fd (fd-open path '(read))))
        (dynamic-wind
          (lambda () (if #f #f))
          (lambda () (fsync! fd path))
          (lambda () (close-quietly fd))))))

  (define (takeover-barrier! store prefixes)
    (parameterize ((theourgia-stage 'deliver-barrier))
      (takeover-flush! store prefixes)))

  (define (takeover-flush! store prefixes)
    (for-each
      (lambda (entry)
        (let* ((writer (car entry))
               (p (cdr entry))
               (dir (writer-directory store writer))
               (segs (map car (discovery-segment-ranges p))))
          (unless (null? segs)
            (for-each (lambda (seg) (flush-file! (string-append dir "/" (segment-file-name seg))))
                      segs)
            ;; THE METADATA IS FLUSHED BY THE VERSION BARRIER, not here.
            ;; It is part of the durable frontier for the same reason --
            ;; a mirror's records are history because published.sexp says
            ;; so -- but it is keyed to the VERSION being depended on
            ;; rather than to a segment being delivered, and a fork at a
            ;; writer's first event would otherwise leave a directory
            ;; this loop never visits.
            (fsync-dir! dir))))
      prefixes))

  ;; THE LOCAL WRITER IS THE ONE THIS STORE OWNS. owner.sexp is written
  ;; by init and by adopt and never by a mirror, so it is the same fact
  ;; discovery uses to call an origin local -- asked once here rather
  ;; than re-derived at every append.
  (define (local-writer-of store ls)
    (let loop ((es (load-session-prefixes ls)))
      (cond
        ((null? es) #f)
        ((eq? (discovery-origin (cdar es)) 'local) (caar es))
        (else (loop (cdr es))))))

  ;; THE LOCK IS RELEASED BY THE SAME UNWIND THAT RELEASES THE GUARD.
  ;; A guard clause only sees exceptions: a callback that escapes by
  ;; invoking a continuation captured outside log-begin unwinds without
  ;; raising, and the first version cleared the store flag while leaving
  ;; the flock held -- the worst of both, since the next log-begin then
  ;; passes the guard and blocks forever on a lock no one will release.
  (define (log-begin store on-deliver)
    (unless (procedure? on-deliver)
      (assertion-violation 'log-begin "on-deliver must be a procedure" on-deliver))
    (claim-store! store 'log-begin)
    (let ((handed-over (vector #f))
          (held (vector #f)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let ((lock (lock-acquire! (string-append store "/lock") 'exclusive)))
            (vector-set! held 0 lock)
            (guard (e (#t (raise e)))
              (trace-event! 'enter-critical
                            (cons (string-append store "/lock") 'exclusive) #f)
              (let ((ls (open-load store 'held-exclusive)))
                (takeover-barrier! store (load-session-prefixes ls))
                (let* ((local (local-writer-of store ls))
                       (entry (and local (assoc local (load-session-prefixes ls))))
                       (s (make-session store lock ls on-deliver local
                                        0 '() 0 #f #f
                                        (and entry (+ 1 (discovery-end-seq (cdr entry))))
                                        #f
                                        (metadata-versions store)
                                        #f '() '() '())))
                  (metadata-barrier! s)
                  (deliver-into! s)
                  (vector-set! handed-over 0 #t)
                  s)))))
        (lambda ()
          (unless (vector-ref handed-over 0)
            (let ((lock (vector-ref held 0)))
              (when lock
                (vector-set! held 0 #f)
                (guard (e (#t (if #f #f))) (lock-release! lock))))
            (release-store! store))))))

  ;; THE APPLIED CURSOR MOVES ONLY ON THE REDUCER'S WORD. Reading a
  ;; record, checking it, and flushing it all leave it where it was:
  ;; those establish that the bytes are there, not that anything has been
  ;; applied to the state deps and cuts are computed against.
  ;; A REJECTED RECORD STOPS THAT WRITER, and only that writer. The
  ;; contract calls rejected "a record with no premise that can ever
  ;; arrive", so the records after it in the same writer have nothing to
  ;; be applied against -- delivering them anyway asks the reducer to
  ;; decide the same question again for every one of them. The scanner
  ;; has no way to skip a writer mid-stream, so the callback is
  ;; suppressed for the rest of that writer instead.
  (define (deliver-from-cut! s cut)
    (let ((on-deliver (session-on-deliver s))
          (stopped '()))
      (load-deliver! (session-load s) cut
        (lambda (writer seg off seq ts actor deps payload)
          (if (or (member writer stopped)
                  (let ((at (writer-rejected-at s writer))) (and at (>= seq at))))
              'suppressed
              (begin
                (session-delivered-set! s (cons (cons writer seq) (session-delivered s)))
                (let ((answer (on-deliver writer seg off seq ts actor deps payload)))
                  (cond
                    ((eq? answer 'applied) (note-applied! s writer seq))
                    ((and (pair? answer) (eq? (car answer) 'rejected))
                     ;; RECORDED, NOT MERELY NOTED FOR THIS PASS. A
                     ;; rejection the reducer returned is the same
                     ;; decision as one it announces later, and keeping
                     ;; it only in a local list meant a reload delivered
                     ;; the record again and asked the same unanswerable
                     ;; question.
                     (session-rejected-set!
                       s (cons (list writer seq
                                     (if (pair? (cdr answer)) (cadr answer) 'rejected))
                               (session-rejected s)))
                     (set! stopped (cons writer stopped))))
                  answer)))))))

  ;; ONE DELIVERY LOOP, AND THIS IS THE EMPTY-CUT CASE OF IT. Keeping a
  ;; second copy here meant the two disagreed the moment either changed:
  ;; the reload's loop recorded what it had delivered and this one did
  ;; not, so a rejection of a record delivered at log-begin was refused
  ;; as "never delivered".
  ;;
  ;; NO `apply` EVENT IN EITHER. Plan section 0 defines that op as step
  ;; 9's in-lock application, between the log fsync and the unlock;
  ;; emitting it for a delivery confirmation too gave one name two
  ;; meanings, and the delivery-time events then masked the commit-time
  ;; one in any row that deduplicated.
  (define (deliver-into! s) (deliver-from-cut! s '()))

  (define (note-applied! s writer seq)
    (let* ((applied (session-applied s))
           (entry (assoc writer applied)))
      (session-applied-set!
        s
        (if entry
            (map (lambda (e)
                   (if (string=? (car e) writer) (cons writer seq) e))
                 applied)
            (append applied (list (cons writer seq)))))))

  (define (session-applied-cut s) (session-applied s))

  ;; The reducer's out-of-band confirmation, for records it drained from
  ;; pending after the callback returned. A report from a superseded
  ;; epoch is refused rather than merged: it describes state that has
  ;; been discarded.
  (define (session-applied! s epoch cut)
    (check-live! 'session-applied! s)
    ;; NOTHING HAS BEEN DELIVERED IN THIS EPOCH YET. A report arriving
    ;; between the reset and its acknowledgement describes state built on
    ;; the epoch that was just discarded, or on nothing at all -- and
    ;; accepting it left the cursor holding a value the replay never
    ;; produced, so the reported frontier disagreed with the reducer.
    (when (session-reset-pending s)
      (raise (make-log-error 'reset-pending #f #f #f
                             (list (cons 'store (session-store s))))))
    (unless (eqv? epoch (session-epoch s))
      (raise (make-log-error 'stale-epoch #f #f #f
                             (list (cons 'reported epoch)
                                   (cons 'current (session-epoch s))))))
    (unless (and (list? cut)
                 (for-all (lambda (e)
                            (and (pair? e) (string? (car e))
                                 (integer? (cdr e)) (exact? (cdr e))))
                          cut))
      (assertion-violation 'session-applied! "cut must be an alist of writer to seq" cut))
    ;; A CUT NAMING A WRITER TWICE IS MALFORMED, not first-entry-wins.
    ;; The cursor update walked every entry and the gate check used
    ;; assoc, so ((A . 3) (A . 1)) stored 1 and cleared the gate for 3,
    ;; and the same pair in the other order did the opposite -- two
    ;; readers of one report disagreeing about what it said.
    (let loop ((es cut) (seen '()))
      (unless (null? es)
        (when (member (caar es) seen)
          (assertion-violation 'session-applied! "cut names a writer twice" cut))
        (loop (cdr es) (cons (caar es) seen))))
    ;; AND IT NEVER GOES BACKWARDS within an epoch: a delayed report of
    ;; an earlier sequence is stale, not a retraction. Retracting applied
    ;; state is what a reset is for.
    ;; A CONFIRMATION CANNOT EXCEED WHAT EXISTS. Reporting a sequence
    ;; the store does not hold used to be accepted, and the readiness
    ;; gate then compared "applied 99" against "reaches 2" and handed
    ;; out a preparation view for history that was never there. The
    ;; bound includes this session's own commits, because confirming a
    ;; record this session just wrote is legitimate even though the
    ;; discovery it was opened with ends earlier.
    (for-each (lambda (e)
                (let ((limit (available-through s (car e))))
                  (when (and limit (> (cdr e) limit))
                    (assertion-violation 'session-applied!
                      "confirmed past the end of that writer's history"
                      (list (car e) (cdr e) limit)))))
              cut)
    (for-each (lambda (e)
                (let ((have (assoc (car e) (session-applied s))))
                  (when (or (not have) (> (cdr e) (cdr have)))
                    (note-applied! s (car e) (cdr e)))))
              cut)
    (let ((mine (and (session-writer s) (assoc (session-writer s) cut)))
          (pending (session-unconfirmed s)))
      (when (and pending mine (>= (cdr mine) pending))
        (session-unconfirmed-set! s #f)))
    (session-revision-set! s (+ 1 (session-revision s)))
    (copy-cut (session-applied s)))

  ;; HANDED OUT AS A COPY. The applied cursor is the reducer's to move
  ;; through the two confirmation paths, and returning the session's own
  ;; alist made (set-cdr! (car cut) 99) a third path -- no epoch check,
  ;; no revision bump, no confirmation.
  (define (copy-cut cut) (map (lambda (e) (cons (car e) (cdr e))) cut))

  ;; How far that writer's history reaches as far as this session knows:
  ;; what discovery found, extended by anything this session has since
  ;; committed for its own writer.
  (define (available-through s writer)
    (let* ((entry (assoc writer (load-session-prefixes (session-load s))))
           (found (and entry (discovery-end-seq (cdr entry))))
           (mine (and (session-writer s) (string=? writer (session-writer s))
                      (session-next-seq s)
                      (- (session-next-seq s) 1))))
      (cond
        ((and found mine) (max found mine))
        (found found)
        (mine mine)
        (else #f))))

  ;; FOUR FRONTIERS, PER WRITER, and they are deliberately not one
  ;; number: read, validated, applied and durable answer different
  ;; questions and the gaps between them are where the interesting
  ;; failures live.
  (define (session-frontiers s)
    (check-live! 'session-frontiers s)
    (let ((ls (session-load s)))
      (map (lambda (entry)
             (let* ((writer (car entry))
                    (p (cdr entry))
                    (applied (assoc writer (session-applied s))))
               (list writer
                     ;; THE PHYSICAL CURSOR IS WHERE THE BYTES END, which
                     ;; is not where the validated prefix ends: a torn
                     ;; residue lies between them, and reporting the
                     ;; validated offset for both made the two frontiers
                     ;; that exist to differ report the same number.
                     (cons 'physical (discovery-physical-current p))
                     (cons 'contiguous (discovery-end-seq p))
                     (cons 'applied (if applied (cdr applied) 0))
                     (cons 'durable (discovery-end-seq p)))))
           (load-session-prefixes ls))))

  ;; NO USABLE PREPARATION VIEW UNTIL THE REDUCER HAS CONFIRMED. Between
  ;; a committed append and its confirmation the log layer knows the
  ;; record is on disk and does NOT know whether the state the next
  ;; record's facts would be computed from includes it.
  ;; NO USABLE VIEW WHILE A COMMITTED RECORD IS UNCONFIRMED. The log
  ;; layer knows the record is on disk; it does NOT know whether the
  ;; state the next record's facts would be computed from includes it.
  ;; Handing out a view here would let a caller compute deps and an
  ;; ordering against a reduction that has not seen its own predecessor.
  (define (session-retired? s)
    (let* ((writer (session-writer s))
           (entry (and writer (assoc writer (load-session-prefixes (session-load s))))))
      (and entry (discovery-retired (cdr entry)) #t)))

  ;; THE READINESS GATE. Two different ways the reducer can be behind,
  ;; and only one of them blocks: a record of THIS writer that was
  ;; delivered and answered `pending` means the append's own predecessor
  ;; has not been applied, so the facts the next record would carry
  ;; would be computed against a state that does not include it. Another
  ;; writer's pending records are unrelated history and are no reason to
  ;; refuse -- the log is not a total order.
  (define (predecessor-applied? s)
    (let* ((writer (session-writer s))
           (entry (and writer (assoc writer (load-session-prefixes (session-load s)))))
           (reach (and entry (discovery-end-seq (cdr entry))))
           (have (let ((e (and writer (assoc writer (session-applied s)))))
                   (if e (cdr e) 0))))
      (and reach (>= have reach))))

  (define (session-view s)
    (check-live! 'session-view s)
    (let ((writer (session-writer s)))
      (and writer
           (not (session-reset-pending s))
           (not (session-poisoned s))
           (not (session-retired? s))
           (not (session-unconfirmed s))
           (predecessor-applied? s)
           (session-next-seq s)
           (make-view (session-revision s) (session-epoch s) writer
                      (session-next-seq s)
                      (copy-cut (session-applied s))))))

  (define (check-live! who s)
    (unless (session? s)
      (assertion-violation who "not a session" s))
    (when (session-ended s)
      (assertion-violation who "this session has ended" (session-store s))))

  (define (log-end! s)
    (check-live! 'log-end! s)
    (session-ended-set! s #t)
    (let ((ls (session-load s)))
      (when (eq? (load-outcome ls) 'open) (load-commit! ls)))
    (lock-release! (session-lock s))
    (release-store! (session-store s))
    'ended)


  ;; THE CLOCK IS A SEAM. Record timestamps and the rotation age both
  ;; read it, and a test that cannot move time cannot reach the age
  ;; trigger at all -- it would have to wait an hour or assert nothing.
  (define log-clock
    (make-parameter
      wall-clock-ms
      (lambda (v)
        (unless (procedure? v)
          (assertion-violation 'log-clock "clock must be a procedure" v))
        v)))

  (define (now-ms) ((log-clock)))




  ;; ---- the metadata version barrier (L22) -----------------------------------

  ;; A VERSION MUST BE DURABLE BEFORE ANYTHING DEPENDS ON IT. The
  ;; manifest says which of a mirror's segments are history, the
  ;; quarantine says where a writer's history stops, the retirement says
  ;; that it stopped -- and a session that writes a record, or freezes a
  ;; snapshot, on the strength of one of those has made a claim that a
  ;; crash can erase while the record survives. Directory durability is
  ;; not content durability: the name being there says nothing about the
  ;; bytes.
  ;;
  ;; SEGMENT BARRIERS CANNOT STAND IN. If the fork is at a writer's very
  ;; first event the session may deliver nothing at all from it and
  ;; never touch that directory, so the flush has to be keyed to the
  ;; VERSION being depended on rather than to any record being read.
  ;;
  ;; ONCE PER VERSION PER SESSION. A confirmation is not carried across
  ;; log-end!: a new session re-establishes all of them, because neither
  ;; inode nor length nor hash proves that the write which produced those
  ;; bytes was ever flushed.
  (define metadata-files '("published.sexp" "retired.sexp" "quarantine.sexp"))

  ;; THE STAGE IS DECLARED HERE, like every other flush site. A barrier
  ;; that names no stage cannot be hit by a staged fault at all, so the
  ;; case for "a version that cannot be made durable refuses the write"
  ;; would pass without the failure ever happening.
  (define (metadata-barrier! s)
    (parameterize ((theourgia-stage 'deliver-barrier))
      (metadata-barrier-staged! s)))

  ;; WHAT WAS LAST FLUSHED FOR THIS FILE, not the set of everything ever
  ;; flushed for it. Remembering every version certified A, then B, then
  ;; A again as already durable -- and the second installation of A is a
  ;; different installation: an old certificate proves that those bytes
  ;; were once made durable, not that the rename which put them back has
  ;; been. A crash there restores B.
  (define (last-flushed s w name)
    (let loop ((es (session-barriered s)))
      (cond ((null? es) #f)
            ((and (equal? (car (car es)) w) (equal? (cadr (car es)) name))
             (caddr (car es)))
            (else (loop (cdr es))))))

  (define (note-flushed! s w name version)
    (session-barriered-set!
      s
      (cons (list w name version)
            (remp (lambda (e) (and (equal? (car e) w) (equal? (cadr e) name)))
                  (session-barriered s)))))

  (define (metadata-barrier-staged! s)
    (let* ((store (session-store s))
           (any (vector #f)))
      (for-each
        (lambda (w)
          (let ((dir (writer-directory store w))
                (touched (vector #f)))
            (for-each
              (lambda (name)
                (let ((version (file-version store w name)))
                  (when (and version (not (equal? version (last-flushed s w name))))
                    (flush-file! (writer-file store w name))
                    (note-flushed! s w name version)
                    (vector-set! touched 0 #t))))
              metadata-files)
            ;; The namespace entry as well as the contents: a version
            ;; whose file is flushed but whose name is not is a version
            ;; that can vanish whole.
            (when (vector-ref touched 0)
              (fsync-dir! dir)
              (vector-set! any 0 #t))))
        (store-writers store))
      ;; P1: THE WRITERS DIRECTORY ITSELF. A writer directory that
      ;; appeared mid-session has its own entry in writers/, and flushing
      ;; the contents of that directory says nothing about whether the
      ;; directory is still there after a crash -- a record whose deps
      ;; name that writer would then point at nothing.
      (when (vector-ref any 0)
        (fsync-dir! (string-append store "/writers")))
      ;; The store's own identity files are depended on by every append
      ;; and are not any writer's.
      (for-each
        (lambda (name)
          (let* ((path (string-append store "/" name))
                 (version (and (file-exists? path)
                               (bytevector->hex (sha256 (read-whole path))))))
            (when (and version (not (equal? version (last-flushed s "" name))))
              (flush-file! path)
              (note-flushed! s "" name version)
              (fsync-dir! store))))
        '("meta.sexp" "instance.sexp"))
      'barriered))

  ;; ---- metadata versions, reload and the epoch ------------------------------

  ;; THE THREE FILES A SESSION'S VIEW OF HISTORY RESTS ON, read cheaply
  ;; so that a change can be noticed on every append rather than only at
  ;; load. Quarantine moves a writer's boundary, retirement ends it, and
  ;; the manifest says which of a mirror's segments count -- a session
  ;; that missed any of them would go on writing against history the
  ;; store no longer agrees it has.
  ;; SHA-256, NOT CRC-32. These versions exist to answer "did this file
  ;; change", and a 32-bit checksum answers it wrongly often enough to
  ;; matter: two quarantine markers differing in their fork -- one
  ;; excluding a record, one not -- can be padded to share a CRC, and the
  ;; session then goes on writing against a boundary that has moved. CRC
  ;; is right where it is: detecting damage in a record it was computed
  ;; over, not deciding whether two files are the same file.
  ;;
  ;; OWNER CONTENT, NOT MERELY ITS PRESENCE. Rewriting owner.sexp with a
  ;; different instance leaves the path in place, and comparing only
  ;; existence let an authority change pass unnoticed.
  (define (file-version store writer name)
    (let ((p (writer-file store writer name)))
      (and (file-exists? p) (bytevector->hex (sha256 (read-whole p))))))

  (define (metadata-versions store)
    (map (lambda (w)
           (list w
                 (file-version store w "quarantine.sexp")
                 (file-version store w "retired.sexp")
                 (file-version store w "published.sexp")
                 (file-version store w "owner.sexp")))
         (store-writers store)))

  ;; RELOADED WHOLE, INSIDE THE LOCK THAT IS ALREADY HELD. An incremental
  ;; catch-up cannot express what a quarantine does: the boundary moves
  ;; BACKWARDS, and records the session has already delivered stop being
  ;; history. Nothing here takes or releases a lock -- the exclusive lock
  ;; is held throughout, and reaching for a shared one would deadlock
  ;; against it.
  (define (reload! s)
    (let* ((store (session-store s))
           (fresh (open-load store 'held-exclusive)))
      (for-each (lambda (e) (trace-event! 'catch-up (car e) #f))
                (load-session-prefixes fresh))
      (session-load-set! s fresh)
      ;; WHAT WAS DELIVERED IS A FACT ABOUT AN EPOCH. Carrying the list
      ;; across a reload let session-reject! accept a record that the new
      ;; boundary has removed and that this epoch never delivered.
      (session-delivered-set! s '())
      (session-versions-set! s (metadata-versions store))
      (session-epoch-set! s (+ 1 (session-epoch s)))
      (session-revision-set! s (+ 1 (session-revision s)))
      (let* ((writer (session-writer s))
             (entry (and writer (assoc writer (load-session-prefixes fresh)))))
        (session-next-seq-set! s (and entry (+ 1 (discovery-end-seq (cdr entry)))))
        (session-unconfirmed-set! s #f)
        ;; RESET IS NEEDED WHEN THE REDUCER HAS APPLIED WHAT NO LONGER
        ;; EXISTS. Its state was built from records the new boundary
        ;; excludes, so nothing short of discarding and replaying it can
        ;; be correct -- and the log layer must not deliver again until
        ;; the reducer says it has done so.
        (if (applied-beyond? s fresh)
            (begin
              (session-reset-pending-set! s #t)
              (session-applied-set! s '()))
            ;; NEW HISTORY HAS TO REACH THE REDUCER. A reload that needs
            ;; no reset still found records the reducer has not seen --
            ;; a mirror's newly published segment, say -- and without
            ;; this they were never delivered at all: not here, not on a
            ;; later append (the versions match again), and not through
            ;; reset-done (no reset is pending). A local record waiting
            ;; on one of them would wait forever.
            (deliver-from-cut! s (session-applied s)))
        (session-epoch s))))

  (define (applied-beyond? s ls)
    (let loop ((es (session-applied s)))
      (cond
        ((null? es) #f)
        (else
         (let* ((entry (assoc (caar es) (load-session-prefixes ls)))
                (reach (if entry (discovery-end-seq (cdr entry)) 0)))
           (if (> (cdar es) reach) #t (loop (cdr es))))))))

  (define (versions-changed? s)
    (not (equal? (session-versions s) (metadata-versions (session-store s)))))

  ;; THE HANDSHAKE, NOT A POLL. The log layer stops delivering until the
  ;; reducer confirms it has discarded the state built on the old epoch;
  ;; confirming a different epoch than the one in force is refused,
  ;; because it describes a discard of something else.
  (define (session-reset-done! s epoch)
    (check-live! 'session-reset-done! s)
    (unless (session-reset-pending s)
      (assertion-violation 'session-reset-done! "no reset is pending" (session-store s)))
    (unless (eqv? epoch (session-epoch s))
      (raise (make-log-error 'stale-epoch #f #f #f
                             (list (cons 'reported epoch)
                                   (cons 'current (session-epoch s))))))
    ;; THE REPLAY IS A DELIVERY TOO, so the versions it will deliver
    ;; against have to be durable first -- a failed barrier before the
    ;; reset must not be bypassed by acknowledging it.
    (metadata-barrier! s)
    (session-reset-pending-set! s #f)
    (deliver-into! s)
    (session-epoch s))

  ;; A RECORD THE REDUCER CANNOT USE, refused by the reducer rather than
  ;; damaged on disk. It stops that writer where it stands; the history
  ;; is untouched and nothing is written to integrity, because nothing
  ;; about the bytes is wrong.
  (define (session-reject! s epoch event reason)
    (check-live! 'session-reject! s)
    (unless (eqv? epoch (session-epoch s))
      (raise (make-log-error 'stale-epoch #f #f #f
                             (list (cons 'reported epoch)
                                   (cons 'current (session-epoch s))))))
    (unless (and (pair? event) (string? (car event))
                 (integer? (cdr event)) (exact? (cdr event)))
      (assertion-violation 'session-reject! "event must be (writer . seq)" event))
    (let* ((writer (car event))
           (seq (cdr event))
           (applied (let ((e (assoc writer (session-applied s)))) (if e (cdr e) 0)))
           (entry (assoc writer (load-session-prefixes (session-load s))))
           (reach (if entry (discovery-end-seq (cdr entry)) 0)))
      (cond
        ;; AN APPLIED EVENT CANNOT BE TAKEN BACK QUIETLY. Undoing it is a
        ;; causal rollback, which is what a reset and a new epoch are for.
        ((<= seq applied) (list 'refused 'already-applied))
        ;; NOR ONE THAT WAS NEVER DELIVERED. "Within the validated
        ;; extent" is not the same as "handed to the reducer": a record
        ;; discovered by a reload, or one after a writer already stopped,
        ;; is inside the extent and was never seen. Rejecting what you
        ;; have not been given would let a caller close a writer it has
        ;; not read.
        ((not (member (cons writer seq) (session-delivered s)))
         (list 'refused 'not-delivered))
        (else
         (session-rejected-set! s (cons (list writer seq reason) (session-rejected s)))
         (list 'rejected writer seq reason)))))

  ;; A REJECTED WRITER IS STOPPED, and stopped is a property of the
  ;; session rather than of one delivery pass -- recording the rejection
  ;; and then letting the next pass deliver that writer again would make
  ;; the rejection advice rather than a decision.
  ;; THE LOWEST REJECTED SEQUENCE, not merely "was anything rejected".
  ;; Suppressing the writer entirely threw away the valid prefix BELOW
  ;; the rejected record: after a reset the replay rebuilt state without
  ;; history that was never in question. The rejection stops that writer
  ;; where it stands, which is a boundary.
  (define (writer-rejected-at s writer)
    (let loop ((rs (session-rejected s)) (lowest #f))
      (cond
        ((null? rs) lowest)
        ((string=? (car (car rs)) writer)
         (let ((seq (cadr (car rs))))
           (loop (cdr rs) (if (or (not lowest) (< seq lowest)) seq lowest))))
        (else (loop (cdr rs) lowest)))))

  ;; ---- instance identity (section 4.1) --------------------------------------

  ;; FOUR FIELDS, ALL OF THEM, ON EVERY APPEND. Comparing the water mark
  ;; alone is not enough and the counterexample is concrete: P opens W,
  ;; Q adopts W into U, P then takes the lock -- W's last sequence still
  ;; equals the mark, the number check passes, and W has been retired.
  ;; The identity is re-read inside the lock rather than cached from
  ;; log-begin, because that is the window the adopt happens in.
  (define (read-instance store)
    (let ((path (string-append store "/instance.sexp")))
      (and (file-exists? path)
           (let ((d (guard (e (#t 'malformed))
                      (string->sexpr-extended (utf8->string (read-whole path))))))
             (if (eq? d 'malformed) 'malformed d)))))

  (define (alist-ref d key)
    (let loop ((xs (if (list? d) d '())))
      (cond
        ((null? xs) #f)
        ((and (list? (car xs)) (= 2 (length (car xs))) (eq? (caar xs) key))
         (cadr (car xs)))
        (else (loop (cdr xs))))))

  ;; -> ok | (mismatch field) | absent | malformed
  (define (verify-instance store)
    (let ((d (read-instance store)))
      (cond
        ((not d) 'absent)
        ((eq? d 'malformed) 'malformed)
        (else
         (call-with-values (lambda () (path-device-inode store))
           (lambda (dev ino)
             (cond
               ((not (equal? (alist-ref d 'machine) (machine-id))) (list 'mismatch 'machine))
               ((not (eqv? (alist-ref d 'device) dev)) (list 'mismatch 'device))
               ((not (eqv? (alist-ref d 'inode) ino)) (list 'mismatch 'inode))
               ((not (alist-ref d 'nonce)) (list 'mismatch 'nonce))
               ;; THE NONCE IS THE REGISTRY KEY, so "it is present" is
               ;; not enough. Changing it alone -- machine, device and
               ;; inode all still right -- moves the lookup to an entry
               ;; that does not exist, and the water mark that would have
               ;; refused a restored log is simply not found. owner.sexp
               ;; records which instance the writer belongs to; the two
               ;; must agree.
               ((let ((owned (owner-nonce store)))
                  (and owned (not (equal? owned (alist-ref d 'nonce)))))
                (list 'mismatch 'nonce))
               (else 'ok))))))))

  (define (owner-nonce store)
    (let loop ((ws (store-writers store)))
      (cond
        ((null? ws) #f)
        (else
         (let* ((path (writer-file store (car ws) "owner.sexp"))
                (d (and (file-exists? path)
                        (guard (e (#t #f))
                          (string->sexpr-extended (utf8->string (read-whole path)))))))
           (or (and d (alist-ref d 'instance)) (loop (cdr ws))))))))

  ;; The machine's own identity: a name plus a nonce minted once and kept
  ;; in the machine home, so that two machines that happen to share a
  ;; host name are still two machines.
  ;; MINTED ONCE PER MACHINE HOME, on first use. Two machines that
  ;; happen to share a host name are still two machines, so the identity
  ;; is a nonce rather than the name; it lives beside the registry
  ;; because that is the thing it qualifies.
  (define (machine-id)
    (let ((path (string-append (machine-home) "/machine.sexp")))
      (if (file-exists? path)
          (guard (e (#t "unknown"))
            (alist-ref (string->sexpr-extended (utf8->string (read-whole path))) 'machine))
          ;; MINTED UNDER THE MACHINE LOCK, and re-read inside it. Two
          ;; processes that both find the file missing would otherwise
          ;; both mint, and the loser's freshly initialised store fails
          ;; its own identity check on the very next append.
          (begin
            (ensure-machine-home!)
            (with-machine-lock
              (lambda ()
                (if (file-exists? path)
                    (guard (e (#t "unknown"))
                      (alist-ref (string->sexpr-extended (utf8->string (read-whole path)))
                                 'machine))
                    (let ((minted (string-append "m-" (number->string (process-id))
                                                 "-" (number->string (wall-clock-ms)))))
                      (atomic-write! path
                                     (string->utf8
                                       (string-append "((machine \"" minted "\"))\n"))
                                     'registry)
                      minted))))))))

  ;; WRITES THE STORE'S INSTANCE IDENTITY. This is what init and the
  ;; identity-mismatch branch of adopt install; it is here rather than in
  ;; a fixture because the four fields it records are the same four that
  ;; every append re-verifies, and two places writing them is two places
  ;; to disagree.
  ;; owner.sexp RECORDS WHICH INSTANCE THE WRITER BELONGS TO. Without it
  ;; the nonce in instance.sexp has nothing to be checked against, and a
  ;; swapped nonce silently moves the registry lookup to an entry that
  ;; does not exist -- so the water mark that should refuse a restored
  ;; log is simply not found.
  (define (owner-install! store writer nonce)
    (atomic-write! (writer-file store writer "owner.sexp")
                   (string->utf8
                     (string-append "((machine \"" (machine-id) "\")"
                                    " (instance \"" nonce "\"))\n"))
                   'registry)
    writer)

  (define (instance-install! store)
    (call-with-values (lambda () (path-device-inode store))
      (lambda (dev ino)
        (let ((nonce (string-append "n-" (number->string (process-id))
                                    "-" (number->string (wall-clock-ms)))))
          (atomic-write! (string-append store "/instance.sexp")
                         (string->utf8
                           (string-append "((machine \"" (machine-id) "\")"
                                          " (device " (number->string dev) ")"
                                          " (inode " (number->string ino) ")"
                                          " (nonce \"" nonce "\"))\n"))
                         'registry)
          nonce))))

  (define (instance-nonce store)
    (let ((d (read-instance store)))
      (and (list? d) (alist-ref d 'nonce))))

  (define (store-id-of store)
    (let ((d (guard (e (#t #f))
               (string->sexpr-extended (utf8->string (read-whole (string-append store "/meta.sexp")))))))
      (or (alist-ref d 'store-id) "unknown")))

  ;; ---- the machine registry (section 4.1) -----------------------------------

  ;; THE REGISTRY IS OUTSIDE THE STORE, and that is the whole point. An
  ;; in-place restore of a backup leaves the store's own metadata
  ;; consistent with itself -- same identity triple, same owner, an
  ;; earlier log -- so nothing inside the directory can tell that history
  ;; was rolled back. A water mark kept somewhere the backup did not
  ;; cover can.
  ;;
  ;; IT IS A PRECONDITION OF THE COMMIT, NOT A RECORD OF IT. The mark is
  ;; raised and made durable BEFORE the log write, so the only way the
  ;; two can disagree after a crash is registry-ahead-of-log, which is
  ;; refused and repaired by adopt. The other order would let a log entry
  ;; survive with no mark, and then a restore to just before it would
  ;; look legitimate.
  ;; READ ONCE PER OPERATION. Consulting the environment at every use
  ;; let one reservation read its registry from one home and write it
  ;; back to another, losing a mark a second process had raised in
  ;; between -- and the maximum-merge cannot recover a value that was
  ;; never read.
  (define current-machine-home (make-parameter #f))

  (define (home-now)
    (or (current-machine-home) (machine-home)))

  (define (registry-path) (string-append (home-now) "/instances.sexp"))
  (define (machine-lock-path) (string-append (home-now) "/lock"))

  ;; THE HOME'S OWN ENTRY HAS TO SURVIVE THE CRASH TOO. Creating the
  ;; directory and then flushing only files inside it leaves the whole
  ;; registry removable by the crash model while the log record it
  ;; vouches for survives -- a record with no mark, which is the one
  ;; state the registry exists to make impossible.
  (define (ensure-machine-home!)
    (let ((home (machine-home)))
      (unless (file-is-directory? home)
        (mkdir-p! home)
        (fsync-dir! (parent-of home)))
      (file-ensure! (machine-lock-path))
      home))

  (define (parent-of path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 1) "/")
        ((char=? (string-ref path i) #\/) (substring path 0 i))
        (else (loop (- i 1))))))

  (define (read-registry)
    (let ((path (registry-path)))
      (trace-event! 'registry-check path #f)
      (if (not (file-exists? path))
          '()
          (let ((d (guard (e (#t 'malformed))
                     (string->sexpr-extended (utf8->string (read-whole path))))))
            (cond
              ((eq? d 'malformed)
               (raise (make-log-error 'registry-malformed #f #f #f
                                      (list (cons 'path path)))))
              ((list? d) d)
              (else
               (raise (make-log-error 'registry-malformed #f #f #f
                                      (list (cons 'path path))))))))))

  ;; ENTRIES ARE (store-id instance writer seq state), keyed by the first
  ;; four; the mark only ever rises. "Only ever rises" is what makes a
  ;; concurrent reader-modifier safe under the machine lock: two
  ;; processes that both read 100 and write 101 and 102 cannot lose the
  ;; larger, because the merge takes the maximum rather than the later
  ;; write.
  (define (registry-entry reg store-id instance writer)
    (let loop ((es reg))
      (cond
        ((null? es) #f)
        ((and (list? (car es)) (>= (length (car es)) 4)
              (equal? (car (car es)) store-id)
              (equal? (cadr (car es)) instance)
              (equal? (caddr (car es)) writer))
         (car es))
        (else (loop (cdr es))))))

  (define (registry-mark reg store-id instance writer)
    (let ((e (registry-entry reg store-id instance writer)))
      (and e (cadddr e))))

  (define (registry-raise reg store-id instance writer seq)
    (let ((found (vector #f)))
      (let ((updated
              (map (lambda (e)
                     (if (and (list? e) (>= (length e) 4)
                              (equal? (car e) store-id)
                              (equal? (cadr e) instance)
                              (equal? (caddr e) writer))
                         (begin (vector-set! found 0 #t)
                                (list store-id instance writer
                                      (max seq (cadddr e))
                                      (if (>= (length e) 5) (list-ref e 4) 'active)))
                         e))
                   reg)))
        (if (vector-ref found 0)
            updated
            (append updated (list (list store-id instance writer seq 'active)))))))

  ;; THE MACHINE LOCK IS TAKEN AFTER THE STORE LOCK, ALWAYS. The order is
  ;; fixed so that two processes touching two stores cannot each hold one
  ;; of the pair and wait for the other.
  (define (with-machine-lock thunk)
    (ensure-machine-home!)
    ;; THE MACHINE HOME MAY NOT BE THE STORE. The store lock is already
    ;; held when this runs, so a home inside the store would make this
    ;; acquire the same file through a second descriptor and wait for a
    ;; lock this very call stack is holding.
    (when (store-lock-collision?)
      (assertion-violation 'with-machine-lock
        "THEOURGIA_HOME must not put the machine lock inside a store" (home-now)))
    (let ((lock (lock-acquire! (machine-lock-path) 'exclusive)))
      (dynamic-wind
        (lambda () (if #f #f))
        thunk
        (lambda () (guard (e (#t (if #f #f))) (lock-release! lock))))))

  ;; STEP 7: THE WATER MARK IS RECHECKED AND RAISED IN ONE CRITICAL
  ;; SECTION. Checking at load time is not enough -- another process can
  ;; adopt this writer in between, and the check that matters is the one
  ;; that happens on the way to this write.
  ;; THE STORE WHOSE LOCK IS ALREADY HELD, so that every path that takes
  ;; the machine lock can check for the collision -- including minting
  ;; the machine identity, which is where it first bit: identity
  ;; verification runs inside the store lock, and a home with no
  ;; machine.sexp mints one under the machine lock.
  (define current-store (make-parameter #f))

  (define (store-lock-collision?)
    (let ((store (current-store)))
      (and store
           (let ((a (guard (e (#t #f))
                      (call-with-values (lambda () (path-device-inode (machine-lock-path)))
                        (lambda (d i) (cons d i)))))
                 (b (guard (e (#t #f))
                      (call-with-values
                        (lambda () (path-device-inode (string-append store "/lock")))
                        (lambda (d i) (cons d i))))))
             (and a b (equal? a b))))))

  ;; INIT PUTS THE STORE IN THE MACHINE REGISTRY AT WATER MARK ZERO.
  ;; The registry is what stops two instances of one store from writing
  ;; past each other, and a store that is not in it is invisible to that
  ;; check until its first append -- so the window in which a second
  ;; instance could be made without anything noticing is exactly the
  ;; window between init and the first write.
  (define (store-register! store)
    (reserve! store (store-id-of store) (instance-nonce store)
              (local-writer-name store) 0))

  (define (local-writer-name store)
    (let loop ((ws (store-writers store)))
      (cond
        ((null? ws) #f)
        ((file-exists? (writer-file store (car ws) "owner.sexp")) (car ws))
        (else (loop (cdr ws))))))

  (define (reserve! store store-id instance writer seq)
    (parameterize ((current-machine-home (machine-home)))
      (with-machine-lock
      (lambda ()
        (let* ((reg (read-registry))
               (mark (registry-mark reg store-id instance writer)))
          (barrier! 'registry-read)
          (cond
            ((and mark (>= mark seq))
             (list 'registry-ahead mark))
            (else
             (let ((next (registry-raise reg store-id instance writer seq)))
               (trace-event! 'registry-write (registry-path) #f)
               (atomic-write! (registry-path)
                              (string->utf8 (string-append (sexpr->string-extended next) "\n"))
                              'registry)
               (barrier! 'after-reserve)
               'reserved))))))))

  ;; ---- session-append! (section 5.2, steps 1-9) -----------------------------

  ;; THE ORDER OF THE STEPS IS THE CONTRACT, not an implementation
  ;; detail. Validation and framing come BEFORE any maintenance, so that
  ;; input the store will refuse cannot leave a truncation or a rotation
  ;; behind: a caller who sends a record type and then sends a legal
  ;; value must not need adopt in between. Maintenance comes before the
  ;; write, so the write goes to a file whose tail is already sound.
  ;;
  ;; FIVE OUTCOMES, and they are distinguishable because the caller has
  ;; to do different things about them. "It failed" collapses "nothing
  ;; happened" together with "the bytes are on disk but unflushed", and
  ;; those differ by whether a retry can duplicate the record.
  (define (session-append! s frame)
    (check-live! 'session-append! s)
    (unless (frame? frame)
      (assertion-violation 'session-append! "not a frame" frame))
    (when (session-poisoned s)
      (raise (make-log-error 'writer-stopped #f #f #f
                             (list (cons 'store (session-store s))
                                   (cons 'remedy 'adopt)))))
    (barrier! 'before-append)
    ;; THE BARRIER COMES FIRST, BEFORE THE RELOAD. A reload delivers the
    ;; records the new metadata admits, and delivery implies durability --
    ;; so flushing afterwards means the reducer has already been handed
    ;; records whose manifest may not survive the crash. The versions are
    ;; on disk by now either way; what this decides is whether they are
    ;; durable before anything is computed from them.
    (let ((durable (guard (e (#t 'barrier-failed)) (metadata-barrier! s))))
      (if (eq? durable 'barrier-failed)
          (list 'refused-before-reserve 'metadata-not-durable)
          (begin
            (when (and (not (session-reset-pending s)) (versions-changed? s))
              (reload! s))
            (append-after-barrier! s frame)))))

  (define (append-after-barrier! s frame)
    (if (session-reset-pending s)
        ;; A STRUCTURED READINESS REFUSAL, naming the state. Not `unseen`
        ;; and not `unknown`: those are answers about the request, and
        ;; this is an answer about the session.
        (list 'refused-before-reserve 'reset-pending)
        (parameterize ((current-store (session-store s)))
          (append-under-store s frame))))

  (define (append-under-store s frame)
    (let ((refusal (binding-refusal s frame)))
      (if refusal
          (list 'refused-before-reserve refusal)
          (catch-up-and-append! s frame))))

  ;; EACH OF THE FOUR BINDINGS IS NAMED SEPARATELY. A frame prepared
  ;; against a superseded view is refused for a reason the caller can act
  ;; on: a stale revision means re-take the view and recompute, a wrong
  ;; writer means the frame was built for another store.
;; WHY THERE IS NO VIEW, named. Every caller that has to explain a
  ;; missing view asks this -- a second copy of the cascade would answer
  ;; "no local writer" for a session that has one and is merely waiting
  ;; to be confirmed, which is a different thing to do about it.
  (define (no-view-reason s)
    (cond
      ((session-reset-pending s) 'reset-pending)
      ((session-poisoned s) 'writer-stopped)
      ((session-retired? s) 'retired)
      ((session-unconfirmed s) 'not-ready)
      ((not (predecessor-applied? s)) 'predecessor-not-applied)
      (else 'no-local-writer)))

  (define (binding-refusal s frame)
    (let ((view (session-view s)))
      (cond
        ((not view) (no-view-reason s))
        ((not (eqv? (frame-epoch frame) (session-epoch s))) 'epoch)
        ((not (eqv? (frame-view-id frame) (view-revision view))) 'view)
        ((not (and (string? (frame-writer frame))
                   (string=? (frame-writer frame) (view-writer view))))
         'writer)
        ((not (eqv? (frame-expect-seq frame) (view-expect-seq view))) 'expect-seq)
        (else #f))))

  ;; ---- session-snapshot! (section 4.5-prime) --------------------------------

  ;; THE ENVELOPE IS ONE FREEZE. The view, the cut and the rows were
  ;; taken together by the reduction layer; this layer either writes
  ;; that, or refuses it. It never writes the rows under a smaller cut.
  ;;
  ;; AHEAD IS REFUSED, NOT DEGRADED. Trimming the cut to what this layer
  ;; can vouch for would pair a state computed over one set of records
  ;; with a cut naming a smaller one -- a snapshot that is internally a
  ;; lie, and one nothing downstream could detect. The reduction layer
  ;; recomputes at a smaller cut instead. It should not happen at all,
  ;; because delivery implies durability; a refusal here says that
  ;; invariant has been broken somewhere else.
  (define (session-snapshot! s envelope)
    (check-live! 'session-snapshot! s)
    (unless (and (list? envelope) (= 3 (length envelope)))
      (assertion-violation 'session-snapshot! "envelope must be (view cut rows)" envelope))
    (let ((v (car envelope)) (cut (cadr envelope)) (rows (caddr envelope)))
      (cond
        ((not (view? v)) (list 'refused 'not-a-view))
        ((not (valid-cut? cut)) (list 'refused 'malformed-cut))
        ((not (list? rows)) (list 'refused 'malformed-rows))
        ((snapshot-view-refusal s v) => (lambda (why) (list 'refused why)))
        ((cut-beyond-durable s cut)
         => (lambda (entry) (cons 'refused (cons 'cut-ahead entry))))
        (else (install-snapshot! s cut rows)))))

  ;; The same bindings a frame is checked against, minus the sequence
  ;; number: a snapshot does not claim a place in the log, so an
  ;; expect-seq that has moved on since the view was taken is not a
  ;; reason to refuse one.
  (define (snapshot-view-refusal s v)
    (let ((now (session-view s)))
      (cond
        ((not now) (no-view-reason s))
        ((not (eqv? (view-epoch v) (session-epoch s))) 'epoch)
        ((not (eqv? (view-revision v) (view-revision now))) 'stale-view)
        ((not (string=? (view-writer v) (view-writer now))) 'writer)
        (else #f))))

  ;; THE LOCAL WRITER'S DURABLE FRONTIER INCLUDES THIS SESSION'S OWN
  ;; APPENDS. `next-seq` only advances when an append comes back
  ;; committed -- that is, fsynced -- so one less than it is the highest
  ;; sequence this writer has on disk. Reading the frontier from the
  ;; load alone would stop at what was there when the session opened,
  ;; and every snapshot taken after a write would be refused.
  (define (durable-seq s writer)
    (if (and (session-writer s) (string=? writer (session-writer s)))
        (- (session-next-seq s) 1)
        (let ((e (assoc writer (load-session-prefixes (session-load s)))))
          (if e (discovery-end-seq (cdr e)) 0))))

  (define (cut-beyond-durable s cut)
    (let loop ((xs cut))
      (cond
        ((null? xs) #f)
        ((> (cdr (car xs)) (durable-seq s (car (car xs))))
         (list (list 'writer (car (car xs)))
               (list 'cut (cdr (car xs)))
               (list 'durable (durable-seq s (car (car xs))))))
        (else (loop (cdr xs))))))

  ;; A NEW FILE EACH TIME, NUMBERED ABOVE EVERY EXISTING ONE. Selection
  ;; tries the highest first and falls back to older ones, so replacing
  ;; a snapshot in place would destroy the fallback that section 4.5'
  ;; requires when the newest turns out to be unusable.
  (define (install-snapshot! s cut rows)
    (let* ((store (session-store s))
           (dir (string-append store "/snap"))
           (next (+ 1 (let loop ((ns (if (file-is-directory? dir)
                                         (map segment-file-number (directory-entries dir))
                                         '()))
                                 (best 0))
                        (cond ((null? ns) best)
                              ((and (car ns) (> (car ns) best)) (loop (cdr ns) (car ns)))
                              (else (loop (cdr ns) best))))))
           (path (string-append dir "/" (segment-file-name next))))
      (mkdir-p! dir)
      (snapshot-write! path cut rows)
      (list 'written path cut)))

  (define (catch-up-and-append! s frame)
    (let* ((store (session-store s))
           (writer (session-writer s)))
      ;; THE CATCH-UP IS INSIDE THE LOCK AND IT IS NOT OPTIONAL. Another
      ;; process may have appended and rotated since this session's last
      ;; look; the sequence number and the append target both come from
      ;; what is on disk now, not from what was there at log-begin.
      (trace-event! 'catch-up writer #f)
      (let ((p (discover-prefix store writer 'held-exclusive)))
        (cond
          ;; THE FORK IN THIS WRITER'S OWN HISTORY. Its predecessors are
          ;; no longer history, so an append that continues from them
          ;; would be building on a prefix the store has disavowed.
          ((discovery-quarantine p)
           (list 'refused-before-reserve 'quarantined))
          ((pair? (discovery-integrity p))
           (session-poisoned-set! s #t)
           (list 'refused-before-reserve 'integrity))
          ((not (eqv? (frame-expect-seq frame) (+ 1 (discovery-end-seq p))))
           (list 'refused-before-reserve 'expect-seq))
          (else (frame-and-write! s frame p))))))

  ;; STEP 5: EVERYTHING THAT CAN REFUSE THE INPUT HAPPENS HERE, before a
  ;; byte of the store changes. The line is built in full -- encoded,
  ;; serialised, checksummed -- and a failure at any point returns with
  ;; the store untouched.
  (define (frame-and-write! s frame p)
    (let* ((store (session-store s))
           (writer (session-writer s))
           (seq (frame-expect-seq frame))
           (ts (now-ms))
           (line (guard (e (#t 'unframable))
                   (trace-event! 'frame writer #f)
                   (encode-record seq ts (frame-actor frame) (frame-deps frame)
                                  (storable-encode (frame-payload frame))))))
      (if (eq? line 'unframable)
          (list 'refused-before-reserve 'unframable)
          (maintain-and-write! s frame p line seq))))

  ;; STEP 6: MAINTENANCE, and it touches this writer's current segment
  ;; and nothing else. A torn tail is the one shape a crash can leave
  ;; that is repaired rather than refused, and the repair is a truncation
  ;; to the last complete frame -- never a rewrite, never a sealed
  ;; segment.
  ;; STEP 7 SITS BETWEEN MAINTENANCE AND THE WRITE, and it is the only
  ;; step that touches anything outside the store. Its failure modes are
  ;; deliberately different from the write's: a refused reservation means
  ;; nothing was written and nothing will be, while a reservation that
  ;; succeeds and is then followed by a failed write leaves the mark
  ;; ahead of the log -- which the next load refuses and adopt repairs.
  ;; RETIREMENT IS NOT RE-DECIDED HERE. The binding refuses a retired
  ;; writer before this is reached -- session-view withholds the view and
  ;; binding-refusal names it -- and a second check of the same rule is a
  ;; second supplier that can disagree with the first.
  (define (reserve-then-write! s frame p line seq target target-path fresh?)
    (let* ((store (session-store s))
           (writer (session-writer s))
           (identity (verify-instance store)))
      (cond
        ((eq? identity 'malformed)
         (list 'refused-before-reserve 'instance-malformed))
        ;; A STORE WITH NO INSTANCE IDENTITY CANNOT BE WRITTEN. It has
        ;; never been through init, so there is nothing to compare a
        ;; copy against -- and the copy is what the identity exists to
        ;; catch.
        ((eq? identity 'absent)
         (list 'refused-before-reserve 'no-instance))
        ((pair? identity)
         (list 'refused-before-reserve (list 'instance (cadr identity))))
        (else
         (let ((outcome (reserve! store (store-id-of store) (instance-nonce store)
                                  writer seq)))
           (cond
             ((and (pair? outcome) (eq? (car outcome) 'registry-ahead))
              (list 'refused-before-reserve 'registry-ahead))
             (else (write-line! s frame line seq target target-path fresh?))))))))

  ;; THE STAGE COVERS MAINTENANCE AS WELL AS THE WRITE. Rotation's size
  ;; probe, its flushes and the torn-tail truncation are all part of
  ;; committing this record, and a fault aimed at the commit stage that
  ;; cannot reach them leaves those paths untestable -- the size probe
  ;; ran with no stage declared at all, so stat-fail@commit passed
  ;; straight through it.
  (define (maintain-and-write! s frame p line seq)
    (parameterize ((theourgia-stage 'commit))
      (maintain-and-write-staged! s frame p line seq)))

  (define (maintain-and-write-staged! s frame p line seq)
    (let* ((store (session-store s))
           (writer (session-writer s))
           (dir (writer-directory store writer))
           (phys (discovery-physical-current p)))
      (if (not (pair? phys))
          (list 'refused-before-reserve 'no-append-target)
          (let* ((seg (car phys))
                 (path (string-append dir "/" (segment-file-name seg)))
                 (torn (discovery-torn p)))
            ;; TRUNCATED THROUGH A DESCRIPTOR OPENED FOR WRITING, and
            ;; the descriptor is closed on the way through -- a failure
            ;; here must reach the caller rather than be swallowed by an
            ;; unwind, because a tail that was not repaired means the
            ;; next write lands after a partial record.
            (when (and torn (eqv? (car torn) seg))
              (let ((fd (fd-open path '(write))))
                (dynamic-wind
                  (lambda () (if #f #f))
                  (lambda () (ftruncate! fd (cadr torn) path))
                  (lambda () (fd-close fd)))))
            (let* ((rotated (maybe-rotate! store writer seg path p line))
                   (target (car rotated))
                   (target-path (cdr rotated)))
              (reserve-then-write! s frame p line seq target target-path
                                   (not (eqv? target seg))))))))

  ;; ROTATION IS A DIRECTORY-ENTRY TRANSACTION and its order is the
  ;; recoverable one: flush what is there, make the new entry, flush it,
  ;; flush the directory. Both visible states -- N+1 absent, N+1 present
  ;; and possibly empty -- are legal, which is what makes every point in
  ;; the sequence a safe place to stop.
  (define (maybe-rotate! store writer seg path p line)
    (if (not (rotation-due? path p line))
        (cons seg path)
        (let* ((next (+ seg 1))
               (next-path (string-append (writer-directory store writer)
                                         "/" (segment-file-name next))))
          (barrier! 'before-rotate-write)
          (let ((fd (fd-open path '(write))))
            (dynamic-wind (lambda () (if #f #f))
                          (lambda () (fsync! fd path))
                          (lambda () (fd-close fd))))
          (barrier! 'after-current-fsync)
          (file-ensure! next-path)
          (barrier! 'after-create-next)
          (let ((fd (fd-open next-path '(write))))
            (dynamic-wind (lambda () (if #f #f))
                          (lambda () (fsync! fd next-path))
                          (lambda () (fd-close fd))))
          (barrier! 'after-next-fsync)
          (fsync-dir! (writer-directory store writer))
          (barrier! 'after-dir-fsync)
          (cons next next-path))))

  ;; TWO TRIGGERS, AND AGE IS MEASURED FROM THE SEGMENT'S FIRST RECORD.
  ;; Not from the file's mtime, which moves with every append, and not
  ;; from its birth time, which is not portable. An empty segment has no
  ;; age.
  ;; A PROBE THAT CANNOT ANSWER IS NOT AN ANSWER OF "NO". Both of these
  ;; used to fall back to a value that means "not due" -- size zero, no
  ;; age -- so a transient failure reading the segment silently skipped a
  ;; rotation that was due and appended to a segment past its limit. A
  ;; failure here stops the append instead, which the caller can retry.
  (define (rotation-due? path p line)
    (let ((size (file-size path)))
      (or (>= (+ size (bytevector-length line)) segment-size-limit)
          (let ((first-ts (segment-first-ts path)))
            (and first-ts (>= (- (now-ms) first-ts) segment-age-limit))))))

  (define segment-size-limit 1048576)
  (define segment-age-limit 3600000)

  ;; ONLY THE FIRST LINE IS NEEDED, and reading the whole segment to get
  ;; it made every age check cost the segment's size. A malformed first
  ;; line means no age; an unreadable file is the caller's problem and
  ;; raises.
  (define (segment-first-ts path)
    (let ((bytes (read-first-line path)))
      (guard (e (#t #f))
        (and (> (bytevector-length bytes) 0)
             (let ((nl (find-newline bytes 0 (bytevector-length bytes))))
               (and nl
                    (let ((r (decode-line (subbytes bytes 0 (+ nl 1)))))
                      (and (eq? (car r) 'ok) (caddr r)))))))))

  ;; Enough bytes to hold a first line, not the whole file. A record is
  ;; a line and the first one is all the age needs; reading the segment
  ;; entire made every append pay for its size.
  (define (read-first-line path)
    (let ((port (open-file-input-port path))
          (open? (vector #t)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let ((b (get-bytevector-n port 4096)))
            (if (eof-object? b) (make-bytevector 0) b)))
        (lambda ()
          (when (vector-ref open? 0)
            (vector-set! open? 0 #f)
            (guard (e (#t (if #f #f))) (close-port port)))))))

  ;; STEP 8 AND 9: the write loop, the log flush, then the apply -- and
  ;; the apply is INSIDE the lock. A record applied after the lock was
  ;; released would be state derived from a store another process may
  ;; already have changed.
  ;; THE OUTCOME COMES FROM THE WRITE, NOT FROM MEASURING THE FILE. The
  ;; first version compared file-size before and after and guarded both
  ;; probes with a fallback -- so a probe that failed became evidence of
  ;; no progress, and "nothing was written" is exactly the answer that
  ;; makes the caller's retry duplicate a record that is already partly
  ;; on disk. The byte count now comes from the loop that wrote them.
  ;;
  ;; THE STAGE IS DECLARED HERE. Fault targeting is <fault>@<stage>, and
  ;; an append that names no stage cannot be hit by a commit-stage fault
  ;; at all: every short-write, EINTR and fsync-failure injection aimed
  ;; at the log would pass straight through and the case would go green
  ;; for the wrong reason.
  (define (write-line! s frame line seq target target-path fresh?)
    (parameterize ((theourgia-stage 'commit))
      (let* ((writer (session-writer s))
             (wrote (vector 0))
             (fd (fd-open target-path '(write append))))
        (let ((outcome
                (guard (e (#t 'write-failed))
                  (write-all! fd line target-path
                              (lambda (n) (vector-set! wrote 0 n)))
                  ;; THE LAST OF ROTATION'S STOPPING POINTS. Section 13'
                  ;; names it and nothing emitted it: the first write into
                  ;; a freshly created segment is a distinct place to
                  ;; lose power, because the entry for that segment may
                  ;; be durable while its first record is not.
                  (when fresh? (barrier! 'after-first-write-next))
                  'written)))
          (cond
            ((eq? outcome 'write-failed)
             (close-quietly fd)
             (if (= (vector-ref wrote 0) 0)
                 (list 'reserved-not-written seq)
                 (list 'partial-write seq (vector-ref wrote 0))))
            (else
             (let ((flushed (guard (e (#t #f)) (fsync! fd target-path) #t)))
               ;; THE CLOSE MUST NOT REPLACE THE OUTCOME. A close that
               ;; fails after a durable write would otherwise escape as
               ;; an exception, losing the fact that the record IS on
               ;; disk -- and the caller would resubmit it.
               (close-quietly fd)
               (if (not flushed)
                   (list 'written-fsync-failed seq)
                   (begin
                     (trace-event! 'apply (cons writer seq) #f)
                     (session-next-seq-set! s (+ 1 seq))
                     (session-unconfirmed-set! s seq)
                     (session-revision-set! s (+ 1 (session-revision s)))
                     (list 'committed seq target))))))))))

  (define (close-quietly fd) (guard (e (#t (if #f #f))) (fd-close fd)))

  ;; ---- snapshot selection ---------------------------------------------------

  ;; NEWEST FIRST, FALLING BACK. A void frame, a cut the log cannot
  ;; support, a cut reaching into quarantined history, or a malformed
  ;; cut all reject THAT snapshot and the next older one is tried --
  ;; falling back to an older valid snapshot rather than to nothing
  ;; (section 4.5-prime, L7).
  ;;
  ;; SUPPORT IS MEASURED FROM THE DISCOVERY RESULT. There is no second
  ;; scan here: coverage is each writer's end-seq, which already has the
  ;; quarantined suffix removed and the retirement boundary applied.
  (define (select-snapshot store prefixes)
    (let ((dir (string-append store "/snap"))
          (coverage (map (lambda (e) (cons (car e) (discovery-end-seq (cdr e))))
                         prefixes)))
      (if (not (file-is-directory? dir))
          (list #f #f 'absent)
          (let loop ((ns (reverse (list-sort <
                          (filter (lambda (n) n)
                                  (map segment-file-number (directory-entries dir))))))
                     (why 'absent))
            (if (null? ns)
                (list #f #f why)
                (let ((path (string-append dir "/" (segment-file-name (car ns)))))
                  (let-values (((cut rows) (snapshot-read path)))
                    (cond
                      ((not cut) (loop (cdr ns) rows))
                      ;; A CUT NAMING ONE WRITER TWICE IS MALFORMED, not
                      ;; a cut whose first entry wins: interpretation
                      ;; would depend on duplicate-key order.
                      ((duplicate-writer? cut)
                       (reject-snapshot path 'duplicate-writer)
                       (loop (cdr ns) 'duplicate-writer))
                      ((cut-crosses-quarantine? cut prefixes)
                       (reject-snapshot path 'quarantined)
                       (loop (cdr ns) 'quarantined))
                      ((not (snapshot-cut-supported? cut coverage))
                       (reject-snapshot path 'unsupported-cut)
                       (loop (cdr ns) 'unsupported-cut))
                      (else (list cut rows #f))))))))))

  (define (reject-snapshot path why)
    (trace-event! 'snapshot-read (cons path why) #f))

  (define (duplicate-writer? cut)
    (let loop ((xs cut) (seen '()))
      (cond
        ((null? xs) #f)
        ((member (caar xs) seen) #t)
        (else (loop (cdr xs) (cons (caar xs) seen))))))

  ;; A snapshot whose cut reaches at or past a writer's fork covers
  ;; history that is no longer admissible, so it is void -- rejecting it
  ;; is not the same as merely excluding the suffix from replay.
  (define (cut-crosses-quarantine? cut prefixes)
    (let loop ((xs cut))
      (cond
        ((null? xs) #f)
        (else
         (let* ((e (assoc (caar xs) prefixes))
                (q (and e (discovery-quarantine (cdr e))))
                (fork (and q (cadr q))))
           (if (and fork (>= (cdar xs) fork)) #t (loop (cdr xs))))))))

  (define (load-snapshot-cut ls) (car (load-session-snapshot ls)))
  (define (load-snapshot-rows ls) (cadr (load-session-snapshot ls)))
  (define (load-snapshot-reason ls) (caddr (load-session-snapshot ls)))

  (define (format-1? meta)
    (let loop ((xs meta))
      (cond
        ((null? xs) #f)
        ((and (list? (car xs)) (= 2 (length (car xs))) (eq? (caar xs) 'format))
         (eqv? (cadr (car xs)) 1))
        (else (loop (cdr xs))))))

  (define (load-prefix ls writer)
    (let ((e (assoc writer (load-session-prefixes ls)))) (and e (cdr e))))

  (define (load-writers ls) (map car (load-session-prefixes ls)))

  (define (load-integrity ls)
    (apply append
           (map (lambda (e) (map (lambda (x) (cons (car e) x))
                                 (discovery-integrity (cdr e))))
                (load-session-prefixes ls))))

  ;; A TERMINAL OPERATION IS TERMINAL, AND NEITHER MAY RUN WHILE
  ;; DELIVERY IS IN FLIGHT. Without these checks a callback could commit
  ;; from inside delivery -- releasing the lock while the remaining
  ;; segments were still being read against files a writer was now free
  ;; to change -- and a later commit could overwrite an abort, turning a
  ;; discarded load into a committed one.
  (define (check-terminal! who ls)
    (let ((o (load-session-outcome ls)))
      (cond
        ((eq? o 'delivering)
         (assertion-violation who "not permitted while delivery is in flight" o))
        ((eq? o 'open) (if #f #f))
        (else (assertion-violation who "this load has already finished" o)))))

  (define (load-commit! ls)
    (check-terminal! 'load-commit! ls)
    (load-session-outcome-set! ls 'committed)
    (release-load! ls)
    (load-session-state ls))

  (define (load-abort! ls reason)
    (check-terminal! 'load-abort! ls)
    (finish-abort! ls reason))

  (define (finish-abort! ls reason)
    (load-session-state-set! ls '())
    (load-session-outcome-set! ls (list 'aborted reason))
    (release-load! ls)
    (load-session-outcome ls))

  (define (release-load! ls)
    (let ((l (load-session-lock ls)))
      (when l (load-session-lock-set! ls #f) (lock-release! l))))

  (define (load-outcome ls) (load-session-outcome ls))

  ;; ---- the deliver pass -----------------------------------------------------

  ;; OPENS THE SEGMENT CONTAINING THE CUT DIRECTLY. That is what the
  ;; validate pass recorded segment-ranges for. Rescanning from the
  ;; beginning and suppressing everything below the cut would also be
  ;; two passes with bounded memory, but it is a different promise -- a
  ;; whole extra traversal that would otherwise appear unnoticed.
  ;;
  ;; end-seq IS THE LIMIT, and it comes from the discovery result rather
  ;; than from scanning until something fails. A verified retirement
  ;; boundary, and a quarantined suffix, both stop well-formed bytes
  ;; with neither a torn tail nor a record-level error: nothing in the
  ;; byte stream marks the stop, so only the discovered extent does.
  ;;
  ;; EVERY CALLBACK IS PROVISIONAL. Nothing here publishes anything; the
  ;; reducer accumulates into its own staging state and load-commit! is
  ;; the only publication point. A delivery read failure aborts the
  ;; WHOLE load, because a snapshot covering the failed writer must be
  ;; rejected and the healthy writers' history below its cut was
  ;; deliberately skipped -- removing the failed writer's rows cannot
  ;; recover their state.
  ;; AN EXCEPTION OR AN ESCAPE OUT OF DELIVERY ABORTS THE LOAD. Without
  ;; this a callback that raised left the session open with its lock
  ;; still held -- an exclusive writer waiting on a reader that had
  ;; already given up -- and load-commit! would still accept it.
  (define (load-deliver! ls cut on-deliver)
    (check-terminal! 'load-deliver! ls)
    (load-session-outcome-set! ls 'delivering)
    (let ((finished (vector #f)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let ((r (deliver-all ls cut on-deliver)))
            (vector-set! finished 0 #t)
            r))
        (lambda ()
          (unless (vector-ref finished 0)
            (when (eq? (load-session-outcome ls) 'delivering)
              (finish-abort! ls 'delivery-escaped)))))))

  (define (deliver-all ls cut on-deliver)
    (let loop ((ws (load-session-prefixes ls)))
      (cond
        ((null? ws)
         (when (eq? (load-session-outcome ls) 'delivering)
           (load-session-outcome-set! ls 'open))
         'delivered)
        (else
         (let* ((writer (caar ws))
                (p (cdar ws))
                (from (let ((e (assoc writer cut))) (if e (cdr e) 0)))
                (r (deliver-writer ls writer p from on-deliver)))
           (if (eq? r 'ok)
               (loop (cdr ws))
               (begin
                 (finish-abort! ls (list 'delivery-failed writer))
                 (list 'delivery-failed writer))))))))

  (define (segment-holding ranges seq)
    (let loop ((rs ranges))
      (cond
        ((null? rs) #f)
        ((and (<= (cadr (car rs)) seq) (<= seq (caddr (car rs)))) (caar rs))
        (else (loop (cdr rs))))))

  (define (deliver-writer ls writer p from on-deliver)
    (let ((limit (discovery-end-seq p))
          (store (load-session-store ls)))
      (cond
        ((eq? (discovery-origin p) 'incomplete-publication) 'ok)
        ((>= from limit) 'ok)
        (else
         (let* ((ranges (discovery-segment-ranges p))
                (start (or (segment-holding ranges (+ from 1))
                           (and (pair? ranges) (caar ranges)))))
           (if (not start)
               'ok
               (let loop ((rs (filter (lambda (r) (>= (car r) start)) ranges)))
                 (cond
                   ((null? rs) 'ok)
                   (else
                    (let* ((seg (caar rs))
                           (buf (discovery-current-buffer p))
                           (bytes (if (and buf (eqv? (car buf) seg))
                                      (cdr buf)
                                      (read-segment store writer seg))))
                      (cond
                        ((eq? bytes 'unreadable) 'failed)
                        (else
                         ;; THE SCANNER'S VERDICT IS THE POINT OF CALLING
                         ;; IT. Discarding it meant only a literal
                         ;; unreadable file aborted: malformed bytes in a
                         ;; later segment were skipped in silence and the
                         ;; load committed anyway. Delivery must fulfil
                         ;; the discovered extent or fail -- it may not
                         ;; deliver less and call that success.
                         (let* ((outcome
                                  (scan-segment bytes writer seg (cadr (car rs)) #t
                                    (lambda (off seq ts actor deps payload)
                                      ;; THE REDUCER'S RETURN VALUE MAY
                                      ;; NOT TRUNCATE DELIVERY. The
                                      ;; scanner stops on 'stop, and a
                                      ;; reducer that happened to return
                                      ;; that symbol would silently end
                                      ;; the segment early -- reported as
                                      ;; success, since the scan
                                      ;; completed. Delivery has no stop
                                      ;; protocol; swallow the value.
                                      (when (and (> seq from) (<= seq limit))
                                        (on-deliver writer seg off seq ts actor
                                                    deps payload))
                                      (if #f #f))))
                                (reached (if (eq? (car outcome) 'complete)
                                             (cadr outcome)
                                             (caddr outcome)))
                                (needed (min limit (caddr (car rs)))))
                           ;; DELIVERY MUST REACH THE DISCOVERED
                           ;; BOUNDARY. Not "did the scan raise": a
                           ;; shortened read, a truncating callback and a
                           ;; residual before the extent all complete
                           ;; without raising and all deliver less than
                           ;; validation promised. What makes them
                           ;; failures is the same thing in each case --
                           ;; the last record actually seen falls short
                           ;; of what this segment's range says it holds.
                           ;; Stopping exactly AT the boundary is correct
                           ;; and common: the extent stops there because
                           ;; validation met the same damage.
                           ;; Every range names at least one record, so
                           ;; a segment that yields none has failed to
                           ;; deliver what validation recorded. This used
                           ;; to carry a second clause excusing ranges
                           ;; whose end fell below their start; those are
                           ;; no longer constructed, and excusing an
                           ;; impossible shape only hides the day it
                           ;; becomes possible again.
                           (cond
                             ((and reached (>= reached needed)) (loop (cdr rs)))
                             (else 'failed)))))))))))))
))
)
