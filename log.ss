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
          discovery-versions discovery-clean?
          log-open log-open-in-session load-prefix load-writers load-integrity
          load-commit! load-abort! load-outcome load-deliver!
          load-snapshot-cut load-snapshot-rows load-snapshot-reason
          snapshot-write! snapshot-read snapshot-cut-supported?
          scan-segment
          atomic-write!
          segment-file-name segment-file-number
          store-writers writer-directory
          enumerate-segment-files current-segment-number
          read-manifest write-manifest! manifest-segments
          loadable-segments
          log-error? log-error-kind log-error-writer log-error-segment
          log-error-offset log-error-detail make-log-error)
  (import (chezscheme)
          (theourgia ffi)
          (theourgia trace)
          (only (theourgia crc32) crc32-hex)
          (only (igropyr crypto) sha256 bytevector->hex)
          (only (theourgia wire)
                sexpr->string-extended string->sexpr-extended decode-line
                escape-newlines))

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
            (let ((port (guard (e ((i/o-file-already-exists-error? e) #f)
                                  (#t (raise e)))
                          (open-file-output-port tmp))))
              (if (not port)
                  (loop (+ tries 1))
                  (begin (close-port port) tmp)))))))

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
                       (guard (e2 (#t (void))) (delete-file tmp))
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

  ;; The current segment is the highest-numbered file that EXISTS, not
  ;; the highest listed anywhere: rotation creates N+1 before anything
  ;; records it, and an empty N+1 is a legal recovery state.
  (define (current-segment-number store writer)
    (let ((ns (enumerate-segment-files store writer)))
      (and (pair? ns) (car (reverse ns)))))

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

  ;; ---- which segments participate in loading -------------------------------

  ;; THE LOCAL WRITER HAS NO MANIFEST AND NEEDS NONE. It owns the
  ;; directory; its segments are its own and nobody publishes them to
  ;; it. Any other writer's segment counts only if the manifest lists
  ;; it, so that a segment which was linked into place and never entered
  ;; the manifest -- a publication interrupted before its final step --
  ;; is ignored rather than adopted (L18 a).
  (define (loadable-segments store writer local?)
    (let ((present (enumerate-segment-files store writer)))
      (if local?
          present
          (let ((listed (manifest-segments (read-manifest store writer))))
            (filter (lambda (n) (memv n listed)) present)))))

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
            quarantine retired versions))

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
          (make-discovery origin #f #f 0 '() #f #f #f '() #f #f '())
          (validate store writer origin lock-context))))

  (define (validate store writer origin lock-context)
    (let* ((quarantine (quarantine-of store writer))
           (retired (retired-of store writer))
           (manifest (if (eq? origin 'local) #f (read-manifest-safely store writer)))
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
                (retired-seq (and retired (not (eq? (car retired) 'malformed))
                                  (caddr retired)))
                (ceiling (cond
                           ((and fork-ceiling retired-seq)
                            (min fork-ceiling retired-seq))
                           (fork-ceiling fork-ceiling)
                           (else retired-seq)))
                (listed (manifest-segments manifest))
                (segs (if (eq? origin 'local)
                          present
                          (filter (lambda (n) (memv n listed)) present)))
                (missing (if (eq? origin 'local)
                             '()
                             (filter (lambda (n) (not (memv n present))) listed)))
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
                          ((and end (= rseg (car end)) (= roff (cadr end))
                                (= rseq eseq))
                           (if #f #f))
                          (else
                           (note! 'retired-mismatch rseg roff
                                  (list (cons 'declared rseq)
                                        (cons 'reached eseq)))))))))
                (tail (and (eq? origin 'local) highest (not retired)
                           (capture-tail store writer highest lock-context))))
           ;; A ceiling below the first sequence admits nothing, and the
           ;; scanner has no way to decline a record it has already read:
           ;; returning 'stop stops AFTER the record, so this case has to
           ;; be answered before any segment is opened.
           (if (and ceiling (< ceiling 1))
             (finish-with origin #f '()
                          (physical-of store writer origin retired highest)
                          #f #f errs quarantine retired versions)
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
                             buffer torn errs quarantine retired versions))
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
                                  buffer torn errs quarantine retired versions))
                    ;; VERIFIED WHILE THE BYTES ARE IN HAND. Without it a
                    ;; published segment could be replaced by different
                    ;; content whose records each carry a correct CRC.
                    ;; Records inside a hash-mismatched file are repair
                    ;; EVIDENCE only: not in end-*, never delivered.
                    ((and want (not (string=? want (bytevector->hex (sha256 bytes)))))
                     (note! 'manifest-hash seg 0 (list (cons 'expected want)))
                     (finish-with origin end ranges
                                  (physical-of store writer origin retired highest)
                                  buffer torn errs quarantine retired versions))
                    (else
                     (let* ((clipped (begin
                                       (check-retirement-offset! bytes seg retired note!)
                                       bytes))
                            (outcome
                              (scan-segment clipped writer seg expect current?
                                            (lambda (off seq ts actor deps payload)
                                              ;; the scanner stops AFTER the
                                              ;; record it is told to stop on,
                                              ;; which is what makes the
                                              ;; ceiling inclusive
                                              (if (and ceiling (>= seq ceiling))
                                                  'stop
                                                  (if #f #f))))))
                       (case (car outcome)
                         ((complete)
                          (let* ((last (cadr outcome))
                                 (ranges (cons (list seg (or expect 1) (or last (- expect 1)))
                                               ranges))
                                 (end (if last
                                          (list seg (caddr outcome) last)
                                          end))
                                 (buffer (if current? (cons seg clipped) buffer)))
                            (if (or (retirement-ends-here? seg retired)
                                    (and ceiling last (>= last ceiling)))
                                (begin
                                  (verify-retired! end)
                                  (finish-with origin end ranges
                                             (physical-of store writer origin retired highest)
                                             buffer torn errs quarantine retired versions))
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
                                         (cons (list seg (or expect 1)
                                                     (or last (- expect 1)))
                                               ranges)
                                         (physical-of store writer origin retired highest)
                                         (if current? (cons seg clipped) buffer)
                                         (list seg (cadr outcome)
                                               (or last (- expect 1)))
                                         errs quarantine retired versions)))
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
                                         (cons (list seg (or expect 1)
                                                     (or last (- expect 1)))
                                               ranges)
                                         (physical-of store writer origin retired highest)
                                         (if current? (cons seg clipped) buffer)
                                         torn errs quarantine retired
                                         versions)))))))))))))))))

  (define (note-error! note! e)
    (note! (log-error-kind e) (log-error-segment e) (log-error-offset e)
           (log-error-detail e)))

  (define (finish origin end-seq ranges phys buffer errs quarantine retired versions)
    (make-discovery origin #f #f end-seq ranges phys buffer #f
                 (reverse (vector-ref errs 0)) quarantine retired versions))

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
  (define (finish-with origin end ranges phys buffer torn errs quarantine retired versions)
    (make-discovery origin
                 (and end (car end)) (and end (cadr end)) (if end (caddr end) 0)
                 (reverse ranges) phys buffer torn
                 (reverse (vector-ref errs 0)) quarantine retired versions))

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
                           (cond
                             ((and reached (>= reached needed)) (loop (cdr rs)))
                             ((and (not reached)
                                   (< (caddr (car rs)) (cadr (car rs))))
                              (loop (cdr rs)))
                             (else 'failed)))))))))))))
))
)
