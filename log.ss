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
          log-open log-replay log-integrity log-torn-tails
          log-adopted-snapshot log-quarantine-version log-writer-origin
          log-store log-writers
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
                   (number->string (get-process-id)) "-"
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
      (rename-file tmp path)
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
      (if (not (file-directory? dir))
          '()
          (sort string<? (filter writer-id? (directory-list dir))))))

  ;; A SEGMENT MUST BE A REGULAR FILE. The name check alone accepts a
  ;; fifo called 000002.sexp, and opening one for reading blocks until a
  ;; writer appears -- inside the shared lock, which would then be held
  ;; forever and block every exclusive operation on the store. A symlink
  ;; to an endless byte source is the same shape. Neither can be
  ;; recovered from by an exception handler, because nothing raises.
  (define (enumerate-segment-files store writer)
    (let ((dir (writer-directory store writer)))
      (if (not (file-directory? dir))
          '()
          (sort < (filter
                    (lambda (n) n)
                    (map (lambda (name)
                           (let ((n (segment-file-number name)))
                             (and n
                                  (file-regular? (string-append dir "/" name))
                                  n)))
                         (directory-list dir)))))))

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
                                          (list (cons 'bytes (- n start)))))))
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
                                                (list (cons 'reason 'seq-not-a-number)))))
                         ((not (and (integer? (caddr r)) (exact? (caddr r))))
                          (list 'integrity
                                (make-log-error 'frame writer segment start
                                                (list (cons 'reason 'ts-not-a-number)))))
                         ((and expect (not (= seq expect)))
                          (list 'integrity
                                (make-log-error 'seq writer segment start
                                                (list (cons 'expected expect)
                                                      (cons 'actual seq)))))
                         (else
                          (let ((v (deliver start seq (caddr r) (cadddr r)
                                            (list-ref r 4) (list-ref r 5))))
                            (if (eq? v 'stop)
                                (list 'complete seq end)
                                (loop end (+ seq 1) seq)))))))
                    ((bad-crc)
                     (list 'integrity
                           (make-log-error 'crc writer segment start
                                           (list (cons 'bytes (- end start))))))
                    ((frame-error)
                     (list 'integrity
                           (make-log-error 'frame writer segment start
                                           (list (cons 'reason (cadr r))))))
                    ;; decode-line answers torn only without a trailing
                    ;; newline, and this branch has one.
                    (else
                     (list 'integrity
                           (make-log-error 'frame writer segment start
                                           (list (cons 'reason (car r)))))))))))))))) 

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

  ;; ---- opening and replaying ----------------------------------------------

  ;; A READER TOUCHES NOTHING. Not a byte, not a name, not a timestamp:
  ;; invariant 9.7.2, and L4 asserts it by comparing the whole directory
  ;; listing and every file's hash before and after. That is why the lock
  ;; helpers refuse to create the lock file, why nothing here calls
  ;; atomic-write!, and why a read-only open performs NO takeover barrier
  ;; -- delivery-implies-durability is a property of a writing session,
  ;; and a reader has no dependencies to underwrite.
  ;;
  ;; WHICH WRITER MAY HAVE A TORN TAIL IS DECIDED BY THE FILE LAYOUT, not
  ;; by identity. A mirrored writer is one with a `published.sexp`: every
  ;; segment it offers is sealed, arrived whole, and a residual there is
  ;; damage. A writer with no manifest is local in origin -- this store
  ;; is where its records were written -- so its highest-numbered segment
  ;; is the one a crash could have interrupted. The rule is checkable
  ;; from the directory rather than inferred from an identity this layer
  ;; does not yet establish; piece five, which reads owner.sexp and the
  ;; instance, may narrow it and must not widen it.

  (define-record-type log-handle
    (fields store meta writers (mutable integrity) (mutable torn)
            (mutable quarantine) snapshot-cut snapshot-rows snapshot-reason))

  (define (log-store h) (log-handle-store h))
  (define (log-writers h) (log-handle-writers h))
  (define (log-integrity h) (reverse (log-handle-integrity h)))
  (define (log-torn-tails h) (reverse (log-handle-torn h)))
  (define (log-quarantine-version h writer)
    (let ((e (assoc writer (log-handle-quarantine h)))) (and e (cdr e))))

  (define (log-adopted-snapshot h)
    (if (log-handle-snapshot-cut h)
        (values (log-handle-snapshot-cut h) (log-handle-snapshot-rows h))
        (values #f (log-handle-snapshot-reason h))))

  (define (writer-file store writer name)
    (string-append (writer-directory store writer) "/" name))

  ;; ORIGIN IS DECIDED BY owner.sexp, WHICH ONLY THIS STORE WRITES.
  ;; The earlier rule -- "no manifest means local" -- had a hole that is
  ;; a legal state rather than a corner case: a mirror's FIRST
  ;; publication links the segment and then crashes before creating
  ;; published.sexp, leaving a writer with neither file. That was
  ;; classified local, its unpublished records were delivered, and its
  ;; highest segment's residual was treated as a recoverable torn tail.
  ;;
  ;; Three answers, not two. A directory with neither owner nor manifest
  ;; is an interrupted first publication: every segment is ignored, not
  ;; delivered and not read as a torn tail, so the next synchronisation
  ;; can finish what it started.
  (define (log-writer-origin h writer)
    (let ((store (log-handle-store h)))
      (cond
        ((file-exists? (writer-file store writer "owner.sexp")) 'local)
        ((file-exists? (writer-file store writer "published.sexp")) 'mirrored)
        (else 'incomplete-publication))))

  ;; call-with-port CLOSES ON A NORMAL RETURN ONLY. An I/O error part way
  ;; through a read, or a continuation escaping from the caller, leaves
  ;; the descriptor open -- and a caller that catches the error and
  ;; retries leaks one per attempt. dynamic-wind is what closes on every
  ;; exit, and every read in this file goes through here.
  (define (read-whole path)
    (let ((port (open-file-input-port path))
          (open? (box #t)))
      (dynamic-wind
        void
        (lambda ()
          (let ((b (get-bytevector-all port)))
            (if (eof-object? b) (make-bytevector 0) b)))
        (lambda ()
          (when (unbox open?)
            (set-box! open? #f)
            (guard (e (#t (void))) (close-port port)))))))

  ;; The version of a quarantine file is the checksum of its bytes: what
  ;; matters is whether it CHANGED since the version a decision was made
  ;; against (section 5.2's isolation barrier), and equality of content
  ;; is exactly that question.
  (define (quarantine-version store writer)
    (let ((p (writer-file store writer "quarantine.sexp")))
      (and (file-exists? p) (crc32-hex (read-whole p)))))

  ;; THE RETIRED PREFIX IS A STOPPING POINT, not a filter. retired.sexp
  ;; records the last valid (segment, offset, seq) of a writer that may
  ;; no longer continue here; history past it is evidence and is not
  ;; read (section 1.1). The shape read here is
  ;;   ((prefix <segment> <offset> <seq>) ...)
  ;; and the rest of the file is ignored by this layer.
  (define (retired-prefix store writer)
    (let ((p (writer-file store writer "retired.sexp")))
      (and (file-exists? p)
           ;; THE READ IS NOT GUARDED, ONLY THE PARSE. Swallowing an I/O
           ;; failure here answered "this writer was never retired",
           ;; which is the most dangerous possible wrong answer: it makes
           ;; a retired writer look active and exposes the evidence bytes
           ;; past its retirement boundary as history.
           (let ((d (let ((text (utf8->string (read-whole p))))
                      (guard (e (#t #f)) (string->sexpr-extended text)))))
             (and (list? d)
                  (let loop ((xs d))
                    (cond
                      ((null? xs) #f)
                      ((and (list? (car xs)) (= 4 (length (car xs)))
                            (eq? (caar xs) 'prefix))
                       (cdr (car xs)))
                      (else (loop (cdr xs))))))))))

  ;; Snapshots live in snap/ under the segment naming convention and are
  ;; tried newest first: a snapshot that is void, or whose cut the log
  ;; cannot support, falls back to an older one rather than to nothing
  ;; (section 4.5-prime, L7).
  (define (choose-snapshot store coverage)
    (let ((dir (string-append store "/snap")))
      (if (not (file-directory? dir))
          (values #f #f '() 'absent)
          (let loop ((ns (reverse (sort < (filter (lambda (n) n)
                                                  (map segment-file-number
                                                       (directory-list dir))))))
                     (last-reason 'absent))
            (if (null? ns)
                (values #f #f '() last-reason)
                (let ((path (string-append dir "/" (segment-file-name (car ns)))))
                  (let-values (((cut rows) (snapshot-read path)))
                    (cond
                      ((not cut) (loop (cdr ns) rows))
                      ((not (snapshot-cut-supported? cut coverage))
                       (trace-event! 'snapshot-read (cons path 'unsupported-cut) #f)
                       (loop (cdr ns) 'unsupported-cut))
                      (else (values (car ns) cut rows #f))))))))))

  ;; Coverage is what the store can speak for: for each writer, the last
  ;; seq its loadable segments could contain. Established WITHOUT
  ;; scanning, from the segment set alone, because a snapshot's cut has
  ;; to be judged before replay decides where to start.
  ;; COVERAGE IS MEASURED, NOT ASSUMED. An earlier version answered
  ;; 'unbounded for any writer that had segments at all, which
  ;; snapshot-cut-supported? then compared against a number so large
  ;; that every cut was supported -- the check was present, tested in
  ;; isolation, and dead in the only place it mattered. A snapshot
  ;; naming A.11 over a log holding ten records was adopted, and replay
  ;; then suppressed all ten and reported nothing.
  ;;
  ;; Only the LAST loadable segment is scanned, which is the smallest
  ;; amount of reading that can answer the question: the last valid seq
  ;; of a writer is in its last segment, and earlier segments cannot
  ;; raise it.
  (define (writer-coverage store writer origin)
    (let* ((segs (if (eq? origin 'incomplete-publication)
                     '()
                     (loadable-segments store writer (eq? origin 'local))))
           (retired (retired-prefix store writer)))
      (cond
        (retired (caddr retired))
        ((null? segs) 0)
        (else
         (let* ((last-seg (car (reverse segs)))
                (path (string-append (writer-directory store writer) "/"
                                     (segment-file-name last-seg)))
                (bytes (guard (e (#t (make-bytevector 0))) (read-whole path)))
                (top (box 0)))
           ;; Scanned with continuity switched off (expected seq #f):
           ;; this is asking how far the bytes reach, not whether they
           ;; are sound. Soundness is replay's answer and it is reported
           ;; separately.
           (scan-segment bytes writer last-seg #f #t
                         (lambda (off seq ts actor deps payload)
                           (when (and (integer? seq) (> seq (unbox top)))
                             (set-box! top seq))))
           (unbox top))))))

  (define (log-open store)
    (unless (string? store)
      (assertion-violation 'log-open "store must be a path string" store))
    (let* ((meta-path (string-append store "/meta.sexp"))
           (meta (if (file-exists? meta-path)
                     (guard (e (#t #f))
                       (string->sexpr-extended (utf8->string (read-whole meta-path))))
                     #f)))
      ;; THE VERSION IS CHECKED, NOT JUST THE PARSE (section 5.1 step 1).
      ;; A store written by a later format would otherwise be read under
      ;; this one's rules, which is the failure a version number exists
      ;; to prevent.
      (unless (and meta (list? meta)
                   (let loop ((xs meta))
                     (cond
                       ((null? xs) #f)
                       ((and (list? (car xs)) (= 2 (length (car xs)))
                             (eq? (caar xs) 'format))
                        (eqv? (cadr (car xs)) 1))
                       (else (loop (cdr xs))))))
        (raise (make-log-error 'meta #f #f #f
                               (list (cons 'path meta-path)
                                     (cons 'supported 1)))))
      (let* ((writers (store-writers store))
             (quarantine (map (lambda (w) (cons w (quarantine-version store w)))
                              writers))
             (coverage (map (lambda (w)
                              (cons w (writer-coverage
                                        store w
                                        (cond
                                ((file-exists? (writer-file store w "owner.sexp")) 'local)
                                ((file-exists? (writer-file store w "published.sexp"))
                                 'mirrored)
                                (else 'incomplete-publication)))))
                            writers)))
        (let-values (((n cut rows reason)
                      (choose-snapshot store coverage)))
          (make-log-handle store meta writers '() '() quarantine
                           cut rows (and (not cut) reason))))))

  ;; Delivery order is section 5.1's: per writer, segments ascending,
  ;; records in file order. `proc` is called as
  ;;   (proc writer segment offset seq ts actor deps payload)
  ;; and may answer stop to end the replay.
  (define (log-replay h proc)
    (let ((store (log-handle-store h)))
      (call/cc
        (lambda (done)
          (for-each
            (lambda (writer)
              (replay-writer h store writer proc done))
            (log-handle-writers h))))))

  (define (replay-writer h store writer proc done)
    (let ((origin (log-writer-origin h writer)))
      ;; An interrupted first publication is not history yet.
      (unless (eq? origin 'incomplete-publication)
        (replay-writer* h store writer origin proc done))))

  (define (replay-writer* h store writer origin proc done)
    (let* ((cut (log-handle-snapshot-cut h))
           (from (let ((e (and cut (assoc writer cut)))) (if e (cdr e) 0)))
           (local? (eq? origin 'local))
           (retired (retired-prefix store writer))
           (manifest (if local? #f (read-manifest store writer)))
           (present (enumerate-segment-files store writer))
           (listed (manifest-segments manifest))
           (segs (if local? present (filter (lambda (n) (memv n listed)) present)))
           (missing (if local? '() (filter (lambda (n) (not (memv n present))) listed)))
           (highest (and (pair? segs) (car (reverse segs)))))
      ;; A LISTED SEGMENT THAT IS NOT THERE IS EVIDENCE, NOT SILENCE. The
      ;; earlier code filtered the present files by the listed numbers
      ;; and threw away the other half of the comparison, so a manifest
      ;; promising segments 1 and 2 over a directory holding only 1
      ;; ended cleanly with no diagnostic at all.
      (let* ((stop-before (if (pair? missing) (car (sort < missing)) #f))
             (segs (if stop-before (filter (lambda (n) (< n stop-before)) segs) segs)))
        (let loop ((ss segs) (expect 1) (sealed-done #f))
          (cond
            ((null? ss)
             (when stop-before
               (log-handle-integrity-set!
                 h (cons (make-log-error 'manifest-missing-segment writer
                                         stop-before #f '())
                         (log-handle-integrity h)))))
            (else
             (let* ((seg (car ss))
                    (current? (and local? (eqv? seg highest) (not retired)))
                    ;; Reaching the believed-current segment switches to
                    ;; the locked read, which also picks up anything that
                    ;; was rotated into place while we queued.
                    (tail (and current? (read-tail-under-lock store writer seg)))
                    (ss (if tail (map car tail) ss))
                    (seg (car ss))
                    (bytes (if tail (cdr (car tail)) (read-sealed store writer seg)))
                    (want-hash (manifest-hash manifest seg)))
               (cond
                 ;; VERIFIED WHILE THE BYTES ARE ALREADY IN HAND. The scan
                 ;; reads every byte anyway, so the hash the manifest
                 ;; promised costs almost nothing to check here -- and
                 ;; without it a published segment could be replaced by
                 ;; different content whose records each carry a correct
                 ;; CRC, and replay would deliver the replacement.
                 ((and want-hash
                       (not (string=? want-hash (bytevector->hex (sha256 bytes)))))
                  (log-handle-integrity-set!
                    h (cons (make-log-error 'manifest-hash writer seg 0
                                            (list (cons 'expected want-hash)))
                            (log-handle-integrity h))))
                 (else
                  (let* ((bytes (if (and retired (= seg (car retired)))
                                    (clip bytes (cadr retired))
                                    bytes))
                         (outcome
                           (scan-segment
                             bytes writer seg expect current?
                             (lambda (off seq ts actor deps payload)
                               (if (<= seq from)
                                   (void)
                                   (let ((v (proc writer seg off seq ts actor
                                                  deps payload)))
                                     (if (eq? v 'stop) (done (void)) v)))))))
                    (case (car outcome)
                      ((complete)
                       (unless (and retired (= seg (car retired)))
                         (loop (cdr ss)
                               (if (cadr outcome) (+ (cadr outcome) 1) expect)
                               (or sealed-done (and tail #t)))))
                      ((torn)
                       (log-handle-torn-set!
                         h (cons (list writer seg (cadr outcome)
                                       (or (caddr outcome) (- expect 1)))
                                 (log-handle-torn h))))
                      ((integrity)
                       (log-handle-integrity-set!
                         h (cons (cadr outcome)
                                 (log-handle-integrity h))))))))))))))) 

  (define (clip bv end)
    (if (>= end (bytevector-length bv)) bv (subbytes bv 0 end)))

  ;; THE CURRENT SEGMENT IS COPIED UNDER A SHARED LOCK AND PARSED
  ;; AFTERWARDS (section 5.1). The lock is held for the copy alone: long
  ;; enough that a writer cannot append into the middle of what is being
  ;; read, short enough that parsing does not block one. A sealed
  ;; segment needs no lock -- nothing may write it.
  ;; THE SEGMENT SET IS RE-ESTABLISHED INSIDE THE LOCK, and the tail is
  ;; taken in one critical section. Enumerating first and then queueing
  ;; for the lock reads a set that may be stale by the time it is
  ;; granted: P holds the exclusive lock, rotates, commits into N+1 and
  ;; unlocks, and a reader that decided "the current segment is N"
  ;; before waiting copies N and never learns of a record committed
  ;; BEFORE its own copy. So everything from the believed-current
  ;; segment onward is enumerated and copied together, and what comes
  ;; back is what the store held at one instant.
  ;;
  ;; THE TRACE IS EMITTED AFTER THE LOCK IS RELEASED. Reporting inside
  ;; it puts a write to stderr -- which can block on a full pipe --
  ;; inside the region a writer is waiting on, so a slow reader of the
  ;; trace becomes a stalled writer.
  (define (read-tail-under-lock store writer from-seg)
    (let ((got (with-shared-lock (string-append store "/lock")
                 (lambda (fd)
                   (let ((now (enumerate-segment-files store writer)))
                     (map (lambda (n)
                            (cons n (read-whole
                                      (string-append (writer-directory store writer)
                                                     "/" (segment-file-name n)))))
                          (filter (lambda (n) (>= n from-seg)) now)))))))
      (for-each (lambda (e)
                  (trace-event! 'copy
                                (string-append (writer-directory store writer)
                                               "/" (segment-file-name (car e)))
                                (bytevector-length (cdr e))))
                got)
      got))

  (define (read-sealed store writer seg)
    (read-whole (string-append (writer-directory store writer)
                               "/" (segment-file-name seg))))
)
