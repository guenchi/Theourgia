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
  (export directory-entry-durable! present-or-unreadable-skip?
          discover-prefix
          discovery? discovery-origin discovery-end-segment discovery-end-offset discovery-end-seq
          discovery-segment-ranges discovery-physical-current discovery-current-buffer
          discovery-torn discovery-integrity discovery-quarantine discovery-retired
          discovery-versions discovery-retired-tail discovery-clean?
          unreadable-entry? unreadable-entry-path unreadable-entry-reason
          unreadable-entry-errno
          log-clock registry-path machine-lock-path instance-install!
          session-durable-seq session-durable-seq-set! session-commit!
          session-pending-count-set! note-written-for!
          session-write-started? session-written-events
          store-id-of adopt! continue-adopt! generation-chain-ok? adopt-needed? verify-instance
          log-publish! segment-sha
          registry-inside-store?
          store-register!
          session-retired? owner-install!
          session-reset-done! session-reject! session-reset-pending
          log-open log-open-in-session load-prefix load-writers load-integrity load-fingerprint
          load-unreadable load-listener-add! load-listener-remove! load-listener-of
          remember-load-unreadable! unreadable-behind incomplete-clause merge-unreadable
          load-refused? load-refused-condition tell-load-refused!
          tell-load-notes! remember-unreadable-notes!
          load-declaration-set! load-declaration-of
          session-delivered? snapshot-unreadable-notes
          local-writer-of
          log-begin log-end! session? session-store session-epoch session-writer
          session-append!
          session-frontiers session-view session-view-refusal session-applied! session-load
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
          store-writers writer-directory retired-successor retired-of
          store-state-snapshot
          barrier-artefacts barrier-required run-barrier! publish-durable?
          uncertain-path uncertain-derived uncertain-load uncertain-write!
          enumerate-segment-files
          read-manifest write-manifest! manifest-segments manifest-range
          log-error? log-error-kind log-error-writer log-error-segment
          log-error-offset log-error-detail make-log-error)
  (import (chezscheme)
          (theourgia ffi)
          (theourgia trace)
          (only (theourgia incomplete)
                declared? make-incomplete-reduction incomplete-reduction?
                incomplete-note-clauses incomplete-refused refusing-notes)
          (only (theourgia crc32) crc32-hex)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (theourgia wire)
                sexpr->string-extended string->sexpr-extended decode-line
                record-envelope-refusal
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
        (if (not (eq? (entry-type tmp) 'absent))
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
      (let ((fd (fd-open tmp '(write create)))
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
            (fsync! fd tmp stage)
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
      (fsync-dir! dir stage)
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
      (fsync-dir! (parent-directory path) stage)))

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

  ;; NEVER: writers/ ITSELF IS LISTED THE WAY EACH writers/<w>/ IS (F79),
  ;; by the one listing operation: absent (ENOENT, ENOTDIR) is no writers,
  ;; and any other failure raises unreadable-entry naming writers/. A
  ;; directory-type pre-check followed by a raw listing let a writers/ that
  ;; could be stat'ed but not listed escape as a bare condition, answered
  ;; (error internal ...).
  (define (store-writers store)
    (let ((names (list-entries (string-append store "/writers"))))
      (if (eq? names 'absent)
          '()
          (list-sort string<? (filter writer-id? names)))))

  ;; A SEGMENT MUST BE A REGULAR FILE. The name check alone accepts a
  ;; fifo called 000002.sexp, and opening one for reading blocks until a
  ;; writer appears -- inside the shared lock, which would then be held
  ;; forever and block every exclusive operation on the store. A symlink
  ;; to an endless byte source is the same shape. Neither can be
  ;; recovered from by an exception handler, because nothing raises.
  ;; A listing that fails is not an empty directory: it raises, and so does
  ;; a segment whose type cannot be read.
  (define (enumerate-segment-files store writer)
    (let* ((dir (writer-directory store writer))
           (names (list-entries dir)))
      (if (eq? names 'absent)
          '()
          (list-sort < (filter
                    (lambda (n) n)
                    (map (lambda (name)
                           (let ((n (segment-file-number name)))
                             (and n
                                  (eq? (entry-type (string-append dir "/" name)) 'regular)
                                  n)))
                         names))))))

  ;; ---- what this store looked like ----------------------------------------
  ;;
  ;; NEVER: A CHEAP READING THAT CHANGES WHEN ANYTHING DURABLE CHANGES, taken
  ;; WITHOUT opening the log and WITHOUT taking the store's lock. It
  ;; exists so a reader holding a value loaded earlier can ask "is this
  ;; still what is on the disk" between requests: opening the log to find
  ;; out would cost more than the read it is protecting, and would take
  ;; the very lock the arrangement exists to stay out of.
  ;;
  ;; NOTE: IT LIVES HERE BECAUSE THE LAYOUT LIVES HERE. Spelling these paths
  ;; out anywhere else would be a second place that knows where a
  ;; writer's manifest is -- and the day the layout moved, the copy would
  ;; go on watching files that no longer exist and report "unchanged"
  ;; for ever, which is the failure this is meant to catch.
  ;;
  ;; NOTE: A MISSING FILE IS A STATE, so it reads as #f rather than being
  ;; left out: a `retired.sexp` APPEARING is exactly the kind of change
  ;; this has to notice, and an entry that is simply absent from both
  ;; snapshots compares equal to itself.
  ;;
  ;; TWO READINGS PER WRITER DIRECTORY, because they catch two different
  ;; things: the DIRECTORY's own version changes when an entry is added
  ;; or removed (a new segment, a quarantine file appearing), and the
  ;; CURRENT segment's version changes when bytes are appended to it
  ;; without any entry changing. Neither one alone covers the other.
  ;; KEY: A SNAPSHOT RECORDS, IT DOES NOT RAISE OR SWALLOW. A path that is
  ;; not there is #f, as before; a path that cannot be read is the marker
  ;; (unreadable <path> <reason>), which is stable for as long as the
  ;; failure is unchanged, so two snapshots taken during it compare equal.
  (define (unreadable-marker e)
    (list 'unreadable (unreadable-entry-path e) (unreadable-entry-reason e)))

  (define (path-snapshot path)
    (cons path
          (guard (e ((unreadable-entry? e) (unreadable-marker e)))
            (if (eq? (entry-type path) 'absent)
                #f
                (guard (e2 ((fs-error? e2) #f)) (path-version path))))))

  (define (directory-snapshot path)
    (cons (path-snapshot path)
          (guard (e ((unreadable-entry? e) (unreadable-marker e)))
            (let ((names (list-entries path)))
              (if (eq? names 'absent)
                  '()
                  (map (lambda (name) (path-snapshot (string-append path "/" name)))
                       names))))))

  ;; EACH WRITER'S ENTRY IS KEYED BY ITS WRITER, `(writer . entry)` (F77c,
  ;; design review r2): the daemon's probe hands the markers in it to the
  ;; answer it is about to give, and a marker says which writer only if
  ;; its entry does. Two snapshots compare as before -- equal? on the
  ;; whole value -- so the key changes nothing about the probe's question.
  (define (store-state-snapshot store)
    (cons
      (path-snapshot (string-append store "/writers"))
      (map
        (lambda (writer)
          (cons writer
          ;; A writer that cannot be read is recorded as the marker, whole.
          (guard (e ((unreadable-entry? e) (unreadable-marker e)))
          (let* ((dir (writer-directory store writer))
                 (segments (enumerate-segment-files store writer))
                 (current (if (null? segments) #f (car (list-sort > segments)))))
            (list
              (path-snapshot dir)
              (map (lambda (name) (path-snapshot (string-append dir "/" name)))
                   '("published.sexp" "owner.sexp" "retired.sexp" "quarantine.sexp"))
              (if current
                  (path-snapshot (string-append dir "/" (segment-file-name current)))
                  (cons 'no-segment #f))
              (directory-snapshot (string-append dir "/damaged"))
              (directory-snapshot (string-append dir "/incoming")))))))
        (store-writers store))))

  ;; EVERY MARKER IN A SNAPSHOT, AS NOTES (F77c, design reviews r2 and r3):
  ;; `(writer path reason)` triples, the form load-unreadable gives, one per
  ;; marker at any depth of a writer's entry -- the whole writer, or a file
  ;; under it (its metadata, its current segment, damaged/, incoming/).
  ;; THE SNAPSHOT IS A FINGERPRINT, NOT A COMPLETENESS CHECK: it takes the
  ;; current segment's version (a stat, and an open and a seek for its size
  ;; -- no byte is read) and only lists the older ones, so an older segment
  ;; that cannot be read leaves no marker here, and neither does a current
  ;; one whose open and seek succeed and whose read would fail. What it finds is
  ;; an early notice; the guarantee is the published state's own notes.
  (define (snapshot-unreadable-notes snapshot)
    (define (markers x)
      (cond
        ((and (pair? x) (eq? (car x) 'unreadable) (pair? (cdr x)) (pair? (cddr x))
              (string? (cadr x)))
         (list (cdr x)))
        ((pair? x) (append (markers (car x)) (markers (cdr x))))
        (else '())))
    (if (pair? snapshot)
        (fold-left
          (lambda (acc entry)
            (if (and (pair? entry) (string? (car entry)))
                (fold-left (lambda (acc m)
                             (let ((note (list (car entry) (car m) (cadr m))))
                               (if (member note acc) acc (append acc (list note)))))
                           acc
                           (markers (cdr entry)))
                acc))
          '()
          (cdr snapshot))
        '()))

  ;; ---- the manifest -------------------------------------------------------

  ;; ((<segment number> "<hex sha256>" <first seq> <last seq>) ...),
  ;; ascending, written whole through atomic-write!. Section 9.6 fixes
  ;; the content and this fixes the shape.
  ;;
  ;; THE ENTRY CARRIES THE RANGE BECAUSE LAYOUT COORDINATES COME FROM
  ;; DECLARATIONS. Where a writer's history ends is a question about what
  ;; this store has said, not about which files happen to be readable at
  ;; the moment it is asked: a listed segment whose bytes are damaged is
  ;; history of a known extent awaiting repair. Reading the extent out of
  ;; the bytes instead left it unknowable exactly when it was needed, and
  ;; an incoming segment's fate then depended on damage somewhere else --
  ;; so the same bytes were accepted or refused according to what had
  ;; been damaged and in what order they arrived.
  ;;
  ;; THERE IS ONE SHAPE. A two-element entry is not an older manifest to
  ;; be read leniently; it is a manifest this build does not understand,
  ;; and accepting it would put a second format in the tree with nobody
  ;; to keep the two agreeing.
  ;;
  ;; A MISSING MANIFEST IS NOT AN EMPTY ONE. #f means this writer has no
  ;; manifest at all, which is the ordinary state of the local writer and
  ;; of a retired prefix; '() means a manifest exists and lists nothing,
  ;; which is a mirrored writer whose publications have all been
  ;; unfinished. The two lead to different decisions, so they are
  ;; different answers.
  (define (manifest-path store writer)
    (string-append (writer-directory store writer) "/published.sexp"))

  ;; UNREADABLE AND UNPARSEABLE ARE ONE FACT TO EVERY CALLER: this store
  ;; cannot say what this writer published. A file whose bytes will not
  ;; come back -- permissions, a bad device, a name that is now a
  ;; directory -- used to escape as the implementation's own i/o
  ;; condition, which reaches a caller as a broken tool rather than as a
  ;; broken manifest, and which no layer above knows to turn into an
  ;; integrity answer.
  (define (read-manifest store writer)
    (let* ((path (manifest-path store writer))
           (bytes (read-entry path)))
      (and (not (eq? bytes 'absent))
           (let* ((text (guard (e (#t (list 'unreadable (unreadable-reason e))))
                          (utf8->string bytes)))
                  (why (if (pair? text) (cadr text) "unparseable"))
                  (datum (if (pair? text)
                             'bad
                             (guard (e (#t 'bad)) (string->sexpr-extended text)))))
             (if (valid-manifest? datum)
                 datum
                 (raise (make-log-error 'manifest writer #f #f
                                        (list (cons 'path path)
                                              (cons 'reason why)))))))))

  (define (whole-number? v) (and (integer? v) (exact? v)))

  (define (valid-manifest? d)
    (and (list? d)
         (let loop ((xs d) (last 0))
           (or (null? xs)
               (let ((e (car xs)))
                 (and (list? e) (= 4 (length e))
                      (whole-number? (car e)) (> (car e) last)
                      (string? (cadr e))
                      (whole-number? (caddr e)) (whole-number? (cadddr e))
                      (> (caddr e) 0) (<= (caddr e) (cadddr e))
                      (loop (cdr xs) (car e))))))))

  (define (manifest-segments manifest)
    (if manifest (map car manifest) '()))

  (define (manifest-hash manifest n)
    (let ((e (and manifest (assv n manifest)))) (and e (cadr e))))

  (define (manifest-range manifest n)
    (let ((e (and manifest (assv n manifest))))
      (and e (cons (caddr e) (cadddr e)))))

  (define (write-manifest! store writer entries)
    (unless (valid-manifest? entries)
      (assertion-violation 'write-manifest!
                           "entries must be ascending (number hash first last) lists"
                           entries))
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
                         ;; THE ENVELOPE IS ASKED BEFORE THE SEQUENCE, and
                         ;; it is asked of the one rule every party shares
                         ;; (`record-envelope-refusal`, in wire.sc, where
                         ;; the reasons for each field are written). The
                         ;; sequence check below compares seq as a number,
                         ;; which is only safe once the envelope has said
                         ;; it is one.
                         ((record-envelope-refusal r)
                          => (lambda (reason)
                               (list 'integrity
                                     (make-log-error 'frame writer segment start
                                                     (list (cons 'reason reason)))
                                     last-seq start)))
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
    (if (not (entry-present? path))
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
    (fields origin
            (immutable end-segment raw-end-segment)
            (immutable end-offset raw-end-offset)
            (immutable end-seq raw-end-seq)
            (immutable segment-ranges raw-segment-ranges)
            (immutable physical-current raw-physical-current)
            (immutable current-buffer raw-current-buffer)
            (immutable torn raw-torn)
            integrity quarantine retired versions
            (immutable retired-tail raw-retired-tail)
            ;; THE ERROR THAT DECIDED THE EXTENT, or #f: the one integrity
            ;; error whose branch stopped discovery (a writer has at most one,
            ;; since discovery stops at the first). The diagnostics that leave
            ;; the extent alone are never it, and neither is a rule that
            ;; lowers the extent without detecting damage -- a quarantine or
            ;; retirement ceiling, the recoverable tail of the current
            ;; segment, an unlisted mirror file, an unfinished publication.
            cut))

  ;; KEY: AN UNREADABLE DISCOVERY HAS NO COORDINATES. A writer whose
  ;; directory could not be read has an origin of `unreadable` and one
  ;; `metadata-unreadable` note, and nothing else about it is known. Its
  ;; end is not 0 and its ranges are not empty -- those would be read as a
  ;; writer that has published nothing, which is the defect this exists to
  ;; stop. So every coordinate accessor RAISES `unreadable-entry`, with the
  ;; note's path and reason, when the origin is `unreadable`; only the
  ;; origin and the integrity answer on it. A consumer that wants to go on
  ;; without that writer asks the origin first, and says so where it does.
  ;;
  ;; NEVER: THE RAW ACCESSORS ARE USED ONLY HERE. The record's own accessors
  ;; are named raw-* and appear nowhere but in the definitions below; a
  ;; census row in the tests pins that, because one raw use elsewhere is a
  ;; consumer reading zeros again.
  (define (unreadable-note p)
    (let loop ((es (discovery-integrity p)))
      (cond ((null? es) #f)
            ((eq? (log-error-kind (car es)) 'metadata-unreadable) (car es))
            (else (loop (cdr es))))))

  (define (guarded-coordinate raw)
    (lambda (p)
      (when (eq? (discovery-origin p) 'unreadable)
        (let* ((note (unreadable-note p))
               (detail (if note (log-error-detail note) '()))
               (field (lambda (k) (let ((e (assq k detail))) (and e (cdr e))))))
          (raise (make-unreadable-entry (field 'path) (field 'reason) (field 'errno)))))
      (raw p)))

  (define discovery-end-segment (guarded-coordinate raw-end-segment))
  (define discovery-end-offset (guarded-coordinate raw-end-offset))
  (define discovery-end-seq (guarded-coordinate raw-end-seq))
  (define discovery-segment-ranges (guarded-coordinate raw-segment-ranges))
  (define discovery-physical-current (guarded-coordinate raw-physical-current))
  (define discovery-current-buffer (guarded-coordinate raw-current-buffer))
  (define discovery-torn (guarded-coordinate raw-torn))
  (define discovery-retired-tail (guarded-coordinate raw-retired-tail))

  ;; THE ONE WAY A DISCOVERY BECOMES `unreadable`: the path that failed, the
  ;; system's message for it, and its errno, in the note the unreadable
  ;; manifest row has always used.
  (define (unreadable-discovery writer path reason errno)
    (make-discovery 'unreadable #f #f 0 '() #f #f #f
                    (list (make-log-error 'metadata-unreadable writer #f #f
                                          (list (cons 'path path)
                                                (cons 'reason reason)
                                                (cons 'errno errno))))
                    #f #f '() #f #f))

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
  ;;
  ;; OWNER.SEXP IS READ, NOT ASKED ABOUT: a stat of it succeeds under a
  ;; directory that cannot be read, and only the read says whether it can
  ;; be. An unreadable-entry raised here reaches discover-prefix.
  (define (origin-of store writer)
    (cond
      ((not (eq? (read-entry (writer-file store writer "owner.sexp")) 'absent)) 'local)
      ((not (eq? (entry-type (writer-file store writer "published.sexp")) 'absent)) 'mirrored)
      (else 'incomplete-publication)))

  (define (writer-file store writer name)
    (string-append (writer-directory store writer) "/" name))

  ;; call-with-port closes on a NORMAL return only, so an I/O error part
  ;; way through a read would leak the descriptor and a caller that
  ;; retries would leak one per attempt.
  (define (read-whole path) (entry-bytes path))

  ;; A PRESENCE TEST THROUGH THE DOOR (F100 D1): absent is #f, a path whose
  ;; stat fails for any other reason raises unreadable-entry, never #f. The
  ;; native predicate this replaces read an unsearchable parent as absence.
  (define (entry-present? p) (not (eq? (entry-type p) 'absent)))

  ;; R2g'S SKIP, KEPT AS RULED (F77b), THROUGH THE DOOR (F100a). THREE SITES,
  ;; ONE RULE: local-writer-name here and working.sc's writer-for ask, of
  ;; every writer, whether its owner.sexp (and, for writer-for, its
  ;; retired.sexp) is there, and barrier-artefacts asks the same of every
  ;; writer's owner.sexp and retired.sexp; a file that cannot be stat'ed
  ;; reads as absent, as the native presence test read it. Converting
  ;; the test to a raise refused every write and every draft verb beside
  ;; any unreadable mirror (measured: F100a suite-1, U2c and the
  ;; unreadable-routes CONTROL). These are the only places an
  ;; unreadable-entry becomes a value by ruling; the census pins the one
  ;; definition.
  (define (present-or-unreadable-skip? path)
    (guard (e ((unreadable-entry? e) #f))
      (not (eq? (entry-type path) 'absent))))

  (define (quarantine-of store writer)
    (let* ((p (writer-file store writer "quarantine.sexp"))
           (bytes (read-entry p)))
      (and (not (eq? bytes 'absent))
           (let* ((version (crc32-hex bytes))
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
    (let* ((p (writer-file store writer "retired.sexp"))
           (bytes (read-entry p)))
      (and (not (eq? bytes 'absent))
           (let* ((version (crc32-hex bytes))
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
  ;; KEY: THE DIRECTORY IS LISTED FIRST, and anything that cannot be read
  ;; while deciding the origin or reading the metadata makes the answer
  ;; `unreadable`, with one note naming the path that failed -- the
  ;; directory itself under 000 and --x, the child under r--. Other writers
  ;; are unaffected. The early incomplete-publication answer is reached only
  ;; once the listing has succeeded: a directory that cannot be listed is
  ;; not a publication that never finished.
  (define (discover-prefix store writer lock-context)
    (guard (e ((unreadable-entry? e)
               (unreadable-discovery writer (unreadable-entry-path e)
                                     (unreadable-entry-reason e)
                                     (unreadable-entry-errno e))))
      (list-entries (writer-directory store writer))
      (let ((origin (origin-of store writer)))
        (if (eq? origin 'incomplete-publication)
            (make-discovery origin #f #f 0 '() #f #f #f '() #f #f '() #f #f)
            (validate store writer origin lock-context)))))

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
           ;; slot 0: the integrity errors, newest first; slot 1: the cut
           (errs (vector '() #f))
           (note! (lambda (kind seg off detail)
                    (vector-set! errs 0 (cons (make-log-error kind writer seg off detail)
                                              (vector-ref errs 0)))))
           ;; AN ERROR THAT DECIDES THE EXTENT: recorded like any other, and
           ;; kept as this writer's cut. Only the branches that stop here
           ;; call it.
           (cut! (lambda (kind seg off detail)
                   (note! kind seg off detail)
                   (vector-set! errs 1 (car (vector-ref errs 0))))))
      (cond
        ;; THE WRITER STOPS BEFORE IT, AND THE OPEN DOES NOT. Nothing
        ;; this writer holds is delivered, because what a store cannot
        ;; read it cannot promise; every other writer is unaffected, and
        ;; `check` reports the reason.
        ((unreadable-version? (cdr (assq 'manifest versions)))
         (let ((v (cdr (assq 'manifest versions))))
           (unreadable-discovery writer (cadr v) (caddr v) (cadddr v))))
        ((eq? manifest 'malformed)
         (cut! 'manifest #f #f '())
         (finish origin 0 '() #f #f errs quarantine retired versions))
        ((and retired (eq? (car retired) 'malformed))
         (cut! 'retired-malformed #f #f '())
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
                  (cut! 'manifest-missing-segment stop-before #f '()))
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
                    ((unreadable-segment? bytes)
                     (cut! 'segment-unreadable seg #f
                            (list (cons 'path (list-ref bytes 1))
                                  (cons 'reason (list-ref bytes 2))
                                  (cons 'errno (list-ref bytes 3))))
                     (finish-with origin end ranges
                                  (physical-of store writer origin retired highest)
                                  buffer torn errs quarantine retired versions #f))
                    ;; VERIFIED WHILE THE BYTES ARE IN HAND. Without it a
                    ;; published segment could be replaced by different
                    ;; content whose records each carry a correct CRC.
                    ;; Records inside a hash-mismatched file are repair
                    ;; EVIDENCE only: not in end-*, never delivered.
                    ((and want (not (string=? want (segment-sha bytes))))
                     (cut! 'manifest-hash seg 0 (list (cons 'expected want)))
                     (finish-with origin end ranges
                                  (physical-of store writer origin retired highest)
                                  buffer torn errs quarantine retired versions #f))
                    ;; AND THE RANGE IT DECLARES HAS TO BE THE RANGE IT
                    ;; HOLDS. With the hash matching, these bytes are the
                    ;; ones the manifest names, so a range that disagrees
                    ;; is the manifest contradicting itself -- and the
                    ;; declaration is what every layout decision is made
                    ;; from. It is excluded exactly as a hash mismatch is,
                    ;; because in both cases the store cannot say what
                    ;; this segment is.
                    ((and want
                          (let ((declared (manifest-range manifest seg))
                                (held (segment-edge-seqs bytes)))
                            (not (and declared held (equal? declared held)))))
                     (cut! 'manifest-range seg 0
                            (list (cons 'declared (manifest-range manifest seg))))
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
                          (note-error! cut! (cadr outcome))
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
                 (reverse (vector-ref errs 0)) quarantine retired versions #f
                 (vector-ref errs 1)))

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
                 (reverse (vector-ref errs 0)) quarantine retired versions tail
                 (vector-ref errs 1)))

  ;; ---- the pieces validate leans on ----------------------------------------

  ;; An unreadable manifest is not a malformed one: it goes on to discovery,
  ;; which states it.
  (define (read-manifest-safely store writer)
    (guard (e ((unreadable-entry? e) (raise e))
              (#t 'malformed))
      (read-manifest store writer)))

  ;; A METADATA FILE THAT WILL NOT OPEN IS NOT AN ABSENT ONE, and it is
  ;; not a broken tool either. Reading it is how this writer's version is
  ;; known, and a failure here used to let the implementation's own i/o
  ;; condition escape the whole discovery -- so a single unreadable
  ;; manifest took down the open of a store whose other writers were
  ;; perfectly readable, and reached the caller as an exception rather
  ;; than as a fact about one writer.
  ;;
  ;; The reason travels with it: what could not be read, and what the
  ;; operating system said about it. "Permission denied" is the sentence
  ;; that tells an operator what to do next, and it is the one thing an
  ;; integrity note cannot reconstruct later.
  (define (unreadable-reason e)
    (if (and (message-condition? e) (irritants-condition? e))
        (let ((irritants (condition-irritants e)))
          (let loop ((xs irritants) (text "unreadable"))
            (cond ((null? xs) text)
                  ((string? (car xs)) (loop (cdr xs) (car xs)))
                  (else (loop (cdr xs) text)))))
        "unreadable"))

  ;; -> #f when there is no manifest, its crc when there is, or
  ;; (unreadable <path> <reason> <errno>) when it cannot be read: a value,
  ;; so that a fingerprint can carry it.
  (define (manifest-version store writer)
    (let* ((p (writer-file store writer "published.sexp"))
           (r (guard (e ((unreadable-entry? e)
                         (list 'unreadable p (unreadable-entry-reason e)
                               (unreadable-entry-errno e))))
                (read-entry p))))
      (cond ((eq? r 'absent) #f)
            ((pair? r) r)
            (else (crc32-hex r)))))

  (define (unreadable-version? v)
    (and (pair? v) (eq? (car v) 'unreadable)))

  ;; A READ FAILURE IS NOT AN ABSENT FILE. Returning a marker keeps the
  ;; two apart; answering with empty bytes would report a writer as having
  ;; no history when its history could not be read.
  ;;
  ;; THE MARKER CARRIES WHAT FAILED: (unreadable <path> <reason> <errno>),
  ;; read through read-entry so the reason is the system's message and the
  ;; errno its name (K11). A segment the listing named and the read then
  ;; found absent is the same stop, with reason "absent": it went between
  ;; the two, and its records are not there to deliver.
  (define (read-segment store writer seg)
    (let ((path (string-append (writer-directory store writer) "/" (segment-file-name seg))))
      (guard (e ((unreadable-entry? e)
                 (list 'unreadable path (unreadable-entry-reason e) (unreadable-entry-errno e))))
        (let ((bytes (read-entry path)))
          (if (eq? bytes 'absent)
              (list 'unreadable path "absent" 'absent)
              bytes)))))

  (define (unreadable-segment? bytes)
    (and (pair? bytes) (eq? (car bytes) 'unreadable)))

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
                  (unless (unreadable-segment? (cdr e))
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
             ;; A SIZE THAT CANNOT BE READ IS NOT 0: the type is asked
             ;; first, so an unreadable segment raises and discovery
             ;; states it; only a segment that is not there is 0.
             (let ((path (string-append (writer-directory store writer)
                                        "/" (segment-file-name highest))))
               (if (eq? (entry-type path) 'absent)
                   (list highest 0)
                   ;; A CURRENT SEGMENT THAT IS THERE AND WILL NOT OPEN
                   ;; LEAVES THE WRITER WITH NO APPEND TARGET (K12), the
                   ;; answer a mirror already gets -- not #f, which means
                   ;; "no segment yet" and would start segment 1 over the
                   ;; one that cannot be read. The segment is named by its
                   ;; segment-unreadable note, the readable prefix stands,
                   ;; and the session gate refuses the writer. Only the
                   ;; size's unreadable-entry is answered here; the type
                   ;; question above still raises.
                   (guard (e ((unreadable-entry? e) 'no-append-target))
                     (list highest (file-size path))))))))

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

  ;; `barriered` IS WHAT THE DELIVERY BARRIER ALREADY FLUSHED. A session
  ;; opened over this load inherits it, so the metadata it would flush on
  ;; its first append is the metadata the delivery barrier has not
  ;; already made durable. Without that the two barriers flush the same
  ;; files twice -- harmless but dishonest, because a case that counts
  ;; flushes then reads a number that says nothing about how much work
  ;; the store actually needed.
  (define-record-type load-session
    (fields store (mutable lock) (mutable prefixes) (mutable state)
            (mutable outcome) (mutable snapshot) (mutable barriered)
            ;; WHY AN ABORTED LOAD ABORTED (F77c): the unreadable-entry that
            ;; stopped it, the scanner's verdict on readable bytes, or #f.
            ;; The outcome stays what it was; this is kept beside it.
            (mutable abort-cause)))

  ;; THE WRITERS A LOAD IS MISSING, in the order the load holds them: what
  ;; an answer built from this load must say (K10). Three ways to be
  ;; missing something: the whole writer could not be read (origin
  ;; unreadable, the note naming what failed); a segment could not be read
  ;; and stopped the writer where it stands (segment-unreadable, K11); or
  ;; readable damage CUT the writer's history -- its prefix is delivered,
  ;; the rest is not. The last two are the writer's discovery-cut, the one
  ;; error that decided its extent.
  (define (load-unreadable ls)
    (let ((store (load-session-store ls)))
      (fold-right
        (lambda (entry out)
          (let ((writer (car entry)) (p (cdr entry)))
            (cond
              ((eq? (discovery-origin p) 'unreadable)
               (let ((detail (log-error-detail (unreadable-note p))))
                 (cons (list writer (cdr (assq 'path detail)) (cdr (assq 'reason detail))) out)))
              ((discovery-cut p) => (lambda (e) (cons (cut-note store writer p e) out)))
              (else out))))
        '()
        (load-session-prefixes ls))))

  ;; THE NOTE FOR A CUT, ITS PATH AND REASON DERIVED FROM THE KIND, never
  ;; read from the error's detail (only segment-unreadable carries a path
  ;; there; a malformed manifest carries nothing). segment-unreadable keeps
  ;; the triple it has always had, the system's reason from its detail; any
  ;; other cut gains (cut <kind> <after>), after being the last sequence the
  ;; writer kept (0 when none). The path is the segment's file for a
  ;; segment's damage, the manifest for a malformed manifest or a segment it
  ;; lists and the directory lacks (such a segment's number need not have a
  ;; file name at all), and retired.sexp for a malformed retirement.
  (define (cut-note store writer p e)
    (let ((kind (log-error-kind e)) (detail (log-error-detail e)))
      (if (and (eq? kind 'segment-unreadable) (pair? detail) (assq 'path detail))
          (list writer (cdr (assq 'path detail)) (cdr (assq 'reason detail)))
          (list writer
                (case kind
                  ((manifest manifest-missing-segment) (manifest-path store writer))
                  ((retired-malformed) (writer-file store writer "retired.sexp"))
                  (else (string-append (writer-directory store writer) "/"
                                       (segment-file-name (log-error-segment e)))))
                (symbol->string kind)
                (list 'cut kind (discovery-end-seq p))))))

  ;; KEY: A LOAD TELLS THE REQUEST IT SERVES WHICH WRITERS IT COULD NOT
  ;; READ. The dispatcher registers a listener under the store string it
  ;; hands to the verb -- a copy of its own, so the key is that request's
  ;; and no other's -- and every load opened with that string reports to it.
  ;;
  ;; NEVER: NOT A PARAMETER. Chez parameters are per OS thread, and every
  ;; green thread on that thread shares them (render.sc, 7.6.36, measured);
  ;; the daemon answers requests in several processes at once, and a
  ;; parameter would hand one request's findings to another. The key is an
  ;; object only the one request holds.
  ;;
  ;; NOTE: A LOAD OPENED WITH ANY OTHER STRING -- one rebuilt from the path
  ;; rather than passed along -- reports to nobody. The census of this is
  ;; the cells asking each answer for its clause, not this comment.
  (define load-listeners (make-weak-eq-hashtable))
  (define (load-listener-add! store proc) (hashtable-set! load-listeners store proc))
  (define (load-listener-remove! store) (hashtable-delete! load-listeners store))
  (define (load-listener-of store) (hashtable-ref load-listeners store #f))

  ;; THE REQUEST'S DECLARATION RIDES ON THE SAME OBJECT (F77c, plan
  ;; amendment A2): the store string the dispatcher copied and owns. The
  ;; dispatcher sets it for the extent of each verb from the verb's class;
  ;; open-load reads it when its caller gave no declaration of its own. It is
  ;; lexical in the way the listener is -- the object is threaded from the
  ;; route as an argument -- and two requests never hold the same object. A
  ;; string nobody registered has none, so a load outside any request is
  ;; undeclared: it fails closed.
  (define load-declarations (make-weak-eq-hashtable))
  (define (load-declaration-set! store declaration)
    (if declaration
        (hashtable-set! load-declarations store declaration)
        (hashtable-delete! load-declarations store)))
  (define (load-declaration-of store) (hashtable-ref load-declarations store #f))
  ;; ONE ENTRY PER WRITER, the first report of it kept, in the order heard.
  (define (merge-unreadable known found)
    (fold-left (lambda (acc u) (if (assoc (car u) acc) acc (append acc (list u))))
               known found))

  ;; THE CLAUSE AN ANSWER CARRIES WHEN WHAT IT WAS BUILT FROM IS MISSING
  ;; SOMETHING (K10), from a list of (writer path reason); #f when nothing
  ;; is missing. One spelling, for every route that answers from a load:
  ;; rpc dispatch and the eval worker (K14).
  (define (incomplete-clause unreadable)
    (and (pair? unreadable)
         (cons 'incomplete (incomplete-note-clauses unreadable))))

  ;; A VALUE BUILT FROM A LOAD -- a reduction -- keeps what that load could
  ;; not read, for whoever answers from it later. Weak, so a value nobody
  ;; holds takes its entry with it.
  (define built-from-incomplete (make-weak-eq-hashtable))
  (define (remember-load-unreadable! value ls)
    (let ((found (load-unreadable ls)))
      (if (pair? found)
          (hashtable-set! built-from-incomplete value found)
          (hashtable-delete! built-from-incomplete value))))
  (define (unreadable-behind value)
    (hashtable-ref built-from-incomplete value '()))
  ;; A VALUE DERIVED FROM A REDUCTION KEEPS THE REDUCTION'S NOTES (F77c,
  ;; design review r1): a copy built from a base's rows is missing whatever
  ;; the base was missing, and a later judgment of the copy must see it.
  (define (remember-unreadable-notes! value notes)
    (if (pair? notes)
        (hashtable-set! built-from-incomplete value notes)
        (hashtable-delete! built-from-incomplete value)))

  ;; A LOAD THAT WAS REFUSED TELLS ITS LISTENER SO, with the condition it
  ;; was refused with (F77c). The request that owns the listener answers
  ;; with that condition's own answer, whatever a catch-all between the
  ;; load and the answer made of the raise. A listener tells the two
  ;; events apart by this record: notes arrive as a list of triples.
  (define-record-type load-refused (fields condition))
  (define (tell-load-refused! store c)
    (let ((listener (load-listener-of store)))
      (when listener (listener (make-load-refused c)))))

  ;; THE SAME REPORT FOR NOTES THAT DID NOT COME FROM A LOAD: a supplied
  ;; state unsealed by a declared consumer tells its listener what the
  ;; state is missing, exactly as a load would (F77c, design review r3).
  (define (tell-load-notes! store found)
    (let ((listener (load-listener-of store)))
      (when (and listener (pair? found))
        (listener found))))

  ;; THE DECLARATION IS AN OPTIONAL LAST ARGUMENT, and its absence is "not
  ;; declared" (F77c, plan amendment A1): a caller that says nothing is
  ;; refused an incomplete load and is unchanged on a complete one.
  (define (log-open store . declaration)
    (trace-event! 'log-open store #f)
    (open-load store 'acquire-shared (and (pair? declaration) (car declaration))))

  (define (log-open-in-session store . declaration)
    (open-load store 'held-exclusive (and (pair? declaration) (car declaration))))

  ;; ONE EXIT HANDLER OVER THE WHOLE LOAD (F77c; design review r5b). Every
  ;; condition that leaves open-load -- the metadata read, enumeration,
  ;; discovery, snapshot selection, the declaration check -- passes one
  ;; handler, and it is the only place that tells the listener "refused"
  ;; and the only place that releases what open-load acquired. The lock is
  ;; held in a cell that is #f until the acquisition succeeds and stays #f
  ;; for a lock the caller holds (held-exclusive): open-load never releases
  ;; a borrowed lock. On a normal return the lock passes to the load, and
  ;; load-commit! or finish-abort! releases it.
  ;;
  ;; THE ORDER IS NOTIFY, RELEASE, RE-RAISE (design review r6): a release
  ;; that fails is quiet, so it can neither skip the notification nor
  ;; replace the condition that is leaving.
  ;;
  ;; A LOAD MISSING A WRITER IS REFUSED TO A CALLER THAT DID NOT DECLARE IT
  ;; ACCEPTS ONE. The check comes right after discovery, before snapshot
  ;; selection, so a refusing note wins over a snapshot failure. A load that
  ;; is not refused tells its notes there, before the snapshot is read; a
  ;; snapshot failure after that is told refused as well, and the request's
  ;; answer is that refusal carrying the notes. The check only raises; the
  ;; handler does the rest.
  (define (open-load store lock-context declaration)
    (unless (string? store)
      (assertion-violation 'log-open "store must be a path string" store))
    (let ((owned (vector #f)))
      (guard (e (#t (open-load-refused! store e owned) (raise e)))
        (open-load-body store lock-context declaration owned))))

  (define (open-load-refused! store e owned)
    (dynamic-wind
      (lambda () (if #f #f))
      (lambda ()
        (when (or (unreadable-entry? e) (incomplete-reduction? e))
          (tell-load-refused! store e)))
      (lambda ()
        (let ((lock (vector-ref owned 0)))
          (when lock
            (vector-set! owned 0 #f)
            (guard (x (#t (if #f #f))) ((current-lock-release) lock)))))))

  (define (open-load-body store lock-context declaration owned)
    (let ((meta-path (string-append store "/meta.sexp")))
      (unless (entry-present? meta-path)
        (raise (make-log-error 'meta #f #f #f (list (cons 'path meta-path)))))
      ;; A meta.sexp THAT CANNOT BE READ IS NOT A MALFORMED ONE (F100b, a
      ;; prerequisite propagation): its unreadable-entry leaves, so the
      ;; point that answers names the file and the errno; only a datum
      ;; that does not parse is `log-error 'meta` below.
      (let ((meta (guard (e ((unreadable-entry? e) (raise e))
                            (#t #f))
                    (string->sexpr-extended (utf8->string (read-whole meta-path))))))
        (unless (and meta (list? meta) (format-1? meta))
          (raise (make-log-error 'meta #f #f #f
                                 (list (cons 'path meta-path) (cons 'supported 1)))))
        ;; THE SHARED LOCK BELONGS TO THE WHOLE LOAD -- enumeration and
        ;; every writer's discovery -- not to one discover-prefix call,
        ;; and every exit releases it.
        (let ((lock (if (eq? lock-context 'acquire-shared)
                        ((current-lock-acquire) (string-append store "/lock") 'shared)
                        #f)))
          (vector-set! owned 0 lock)
          (let* ((writers (store-writers store))
                 (prefixes (map (lambda (w)
                                  (cons w (discover-prefix store w lock-context)))
                                writers)))
            ;; THE WINDOW RIGHT AFTER DISCOVERY, before snapshot selection, the
            ;; declaration check and the delivery barrier, for the rows that
            ;; change a segment inside it (F77c D-barrier) or park two loads
            ;; side by side (D-interleave).
            (hold-point! 'after-discovery)
            (let ((ls (make-load-session store lock prefixes '() 'open #f '() #f)))
              ;; Only the notes that refuse this consumer refuse it -- a cut
              ;; refuses the strict declaration alone (incomplete.sc,
              ;; refusing-notes) -- and the refusal names EVERY note of the
              ;; load, so a cut beside an unreadable writer is never lost.
              ;;
              ;; BEFORE THE SNAPSHOT IS READ. Discovery has already found what
              ;; the load is missing, and a snapshot that cannot be read
              ;; aborts the load with a condition of its own: told here, the
              ;; notes reach the request's answer beside that refusal, as
              ;; they reach any answer given after a load; told after, they
              ;; were lost with the aborted load. A refusing note therefore
              ;; wins over an unreadable snapshot.
              (let* ((notes (load-unreadable ls))
                     (refusing (refusing-notes
                                 notes
                                 (or (eq? declaration incomplete-refused)
                                     (eq? (load-declaration-of store) incomplete-refused)))))
                ;; An explicit declaration only ADDS to the request's (A2).
                (when (and (pair? refusing)
                           (not (declared? declaration))
                           (not (declared? (load-declaration-of store))))
                  (raise (make-incomplete-reduction notes)))
                (tell-load-notes! store notes))
              (load-session-snapshot-set! ls (select-snapshot store prefixes))
              ls))))))


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
            ;; TWO FRONTIERS WHERE THERE WAS ONE. `next-seq` advances
            ;; when a record is WRITTEN; `durable-seq` only when a
            ;; barrier has made it so. They were the same number while
            ;; every append fsynced, and `durable-seq` below still read
            ;; `next-seq` with a comment saying the two were the same --
            ;; a sentence that deferring the fsync makes false in
            ;; silence, after which a snapshot can name a cut whose
            ;; records are not on disk.
            (mutable next-seq) (mutable durable-seq) (mutable unconfirmed)
            (mutable versions) (mutable reset-pending) (mutable rejected)
            (mutable delivered) (mutable barriered)
            ;; THE SEGMENTS THIS SESSION HAS WRITTEN INTO. The barrier at
            ;; the end of a request has to cover every one of them, and
            ;; "the current one" is not the same set: a rotation part way
            ;; through would leave the earlier segment out of the
            ;; closure, and the records in it not durable when the word
            ;; was said.
            (mutable touched)
            ;; THE RANGE THIS REQUEST IS AUTHORISED TO WRITE, `(start . end)`
            ;; or #f. The registry used to be read, raised and rewritten
            ;; ONCE PER RECORD; that is two more flushes a record, so
            ;; removing the log's fsync alone left the cost still growing
            ;; with the number of records. One reservation covers the
            ;; whole request.
            (mutable authorised)
            ;; HOW MANY RECORDS THE REQUEST IN HAND WILL WRITE, so that
            ;; its first append can reserve the whole range at once. It
            ;; is a count and not a range: the positions are not known
            ;; until the first append reaches the point where every check
            ;; that precedes a reservation has passed.
            (mutable pending-count)
            ;; WHETHER THIS SESSION HAS TOUCHED THE LOG, and where it
            ;; started if it has. A failure that reaches the caller means
            ;; one thing when bytes were written and the opposite when
            ;; none were, and nothing else in the session distinguishes
            ;; them: the sequence counter moves on a reservation, which
            ;; happens before any byte leaves.
            start-seq (mutable write-started)
            ;; WHY THIS SESSION HAS NO WRITER, WHEN THE REASON IS A WRITER
            ;; THAT COULD NOT BE READ: (writer-unreadable (path p) (reason r))
            ;; or #f. A store with no readable local writer and an unreadable
            ;; one may be looking at its own writer through a directory it
            ;; cannot read; it does not go on as if it had none.
            unreadable-refusal
            ;; WHAT THE CALLER DECLARED (F77c): incomplete-accepted or #f.
            ;; A reload inside the session opens its load with the same
            ;; declaration the session was begun with.
            declaration))

  ;; DELIVERY IMPLIES DURABILITY, so the barrier is the session's
  ;; obligation and it runs before the first callback -- not per record,
  ;; and not after. What the reducer receives becomes the basis for deps
  ;; and snapshot cuts; a record it applied that a crash then removes
  ;; would leave those pointing at history that never existed. This
  ;; writer's own unflushed residue is included: it is the most likely
  ;; thing to be unflushed and the least likely to be noticed.
  (define (flush-file! path stage)
    (unless (eq? (entry-type path) 'absent)
      (flush-existing! path stage)))

  ;; THE SAME FLUSH WITH THE QUESTION LEFT OUT. A caller that already
  ;; knows the file must be there does not want to ask again: between the
  ;; asking and the opening the answer can change, and `file-exists?`
  ;; returning #f then produces silence that is indistinguishable from a
  ;; flush that worked. Opening it asks the question at the only moment
  ;; the answer cannot go stale -- if the file is gone, the open fails
  ;; and the caller fails with it.
  ;; EVERY SEGMENT THIS OPENS IS ONE WHOSE RECORDS WILL BE DELIVERED, so
  ;; a failure to open one is a failure to make delivered history
  ;; durable, and it escapes.
  ;;
  ;; IT USED TO SKIP AN OPEN THAT FAILED, on the argument that a segment
  ;; nobody can open is already an integrity report elsewhere and raising
  ;; here would take the whole store down for one damaged file. Measured,
  ;; the argument is about a case that does not arise: a segment that
  ;; cannot be opened is EXCLUDED from `discovery-segment-ranges` -- it
  ;; is reported as integrity and its records are not delivered -- so
  ;; this loop never reaches it.
  ;;
  ;; WHAT THE SKIP ACTUALLY COVERED WAS THE TRANSIENT CASE, and covering
  ;; it was the defect: discovery reads and caches a dying writer's
  ;; segment, this open then fails, the skip passes, the directory fsync
  ;; succeeds -- and delivery hands out records from the cached bytes
  ;; that no flush ever reached. A power cut takes them, and they were
  ;; promised.
  (define (flush-readable! path stage)
    (let ((fd (fd-open path '(read))))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda () (fsync! fd path stage))
        (lambda () (close-quietly fd)))))

  (define (flush-existing! path stage)
    (let ((fd (fd-open path '(read))))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda () (fsync! fd path stage))
        (lambda () (close-quietly fd)))))

  (define (takeover-barrier! store prefixes . rest)
    (parameterize ((theourgia-stage 'deliver-barrier))
      (takeover-flush! store prefixes 'deliver-barrier
                       (if (pair? rest) (car rest) #f))))

  ;; THE BARRIER'S SCOPE IS THE DELIVERY'S SCOPE, and the cut is what
  ;; decides both. A segment lying entirely at or below the cut
  ;; contributes no record this delivery will hand out, so there is
  ;; nothing about it to promise -- and opening it anyway would undo the
  ;; property the cut exists for: a reader resuming from a cut does not
  ;; reopen the history below it.
  ;;
  ;; Flushing more than is delivered is not merely wasteful here; it is a
  ;; second rule about what a load covers, and the two would answer
  ;; differently the first time one of them changed.
  (define (takeover-flush! store prefixes stage . rest)
    (let ((cut (if (pair? rest) (car rest) #f)))
      (takeover-flush-from! store prefixes stage cut)))

  ;; THE BARRIER'S SCOPE IS THE WRITERS WHOSE DISCOVERY IS NOT `unreadable`.
  ;; An unreadable writer delivered nothing, so nothing of it needs making
  ;; durable; it is skipped, and the skip is recorded in the trace as
  ;; (barrier-skipped-unreadable <writer>). Without this a writer whose
  ;; directory cannot be listed would fail every load here.
  (define (takeover-flush-from! store prefixes stage cut)
    (for-each
      (lambda (entry)
        (if (eq? (discovery-origin (cdr entry)) 'unreadable)
            (trace-event! 'barrier-skipped-unreadable (car entry) #f)
            (takeover-flush-writer! store entry stage cut)))
      prefixes))

  (define (takeover-flush-writer! store entry stage cut)
    (for-each
      (lambda (entry)
        (let* ((writer (car entry))
               (p (cdr entry))
               (dir (writer-directory store writer))
               (from (let ((e (and cut (assoc writer cut)))) (if e (cdr e) 0)))
               (segs (map car (filter (lambda (r) (> (caddr r) from))
                                      (discovery-segment-ranges p)))))
          (unless (null? segs)
            (for-each (lambda (seg)
                        (flush-readable! (string-append dir "/" (segment-file-name seg))
                                         stage))
                      segs)
            ;; THE METADATA IS FLUSHED BY THE VERSION BARRIER, not here.
            ;; It is part of the durable frontier for the same reason --
            ;; a mirror's records are history because published.sexp says
            ;; so -- but it is keyed to the VERSION being depended on
            ;; rather than to a segment being delivered, and a fork at a
            ;; writer's first event would otherwise leave a directory
            ;; this loop never visits.
            (fsync-dir! dir stage))))
      (list entry)))

  ;; THE LOCAL WRITER IS THE ONE THIS STORE OWNS. owner.sexp is written
  ;; by init and by adopt and never by a mirror, so it is the same fact
  ;; discovery uses to call an origin local -- asked once here rather
  ;; than re-derived at every append.
;; THE LOCAL WRITER IS THE ONE THIS SESSION MAY WRITE, which after an
  ;; adopt is not the first local writer it finds. Both generations have
  ;; an owner.sexp -- that is what makes them local -- and the retired
  ;; one is local history this machine may no longer extend. Taking the
  ;; first left every session after an adopt holding the retired writer,
  ;; so `session-view` was #f and the store looked unwritable.
  (define (local-writer-of store ls)
    (let ((locals (filter (lambda (e) (eq? (discovery-origin (cdr e)) 'local))
                          (load-session-prefixes ls))))
      ;; NEVER: EVERY LOCAL WRITER RETIRED IS "NO LOCAL WRITER", NOT "THE
      ;; FIRST RETIRED ONE". The earlier fallback answered the head of the
      ;; list when the search ran out, and a client placing its cursor on
      ;; that name would be aiming at a writer it cannot write as -- an
      ;; unknown drawn as the reassuring answer.
      ;;
      ;; NEVER: AND THIS IS REACHABLE. An adopt that writes the retirement
      ;; and gets no further leaves a retirement with no successor; the
      ;; comment at the head of `retire-and-adopt!` says so in as many
      ;; words. The answer is now the same `#f` an empty store gives, so
      ;; `check` omits the clause and the client has to ask rather than
      ;; guess.
      (let loop ((es locals))
        (cond
          ((null? es) #f)
          ((not (retired-of store (car (car es)))) (car (car es)))
          (else (loop (cdr es)))))))

  ;; NEVER: THE WRITE PATH NEEDS THE RETIRED ONE, AND THE QUESTIONS ARE NOT THE
  ;; SAME. `local-writer-of` answers "is there a writer here I may write as",
  ;; which is what `check` reports and which is #f when every local writer is
  ;; retired. `log-begin` is asking something else -- WHICH WRITER IS THIS
  ;; SESSION ABOUT -- because it has to say WHY it refuses, and "retired" is
  ;; the answer that tells a client to adopt. Giving it the #f collapsed that
  ;; into a store with no writer at all: measured, `log13`'s row for a retired
  ;; writer whose sequence still matches raised `~s is not a string` instead of
  ;; refusing with `retired`.
  ;; -> (writer-unreadable (path p) (reason r)) for the first writer whose
  ;; discovery is `unreadable`, or #f. Asked only when no readable local
  ;; writer was found (R4's session gate).
  (define (unreadable-writer-refusal ls)
    (let loop ((es (load-session-prefixes ls)))
      (cond
        ((null? es) #f)
        ((eq? (discovery-origin (cdr (car es))) 'unreadable)
         (let ((d (log-error-detail (unreadable-note (cdr (car es))))))
           (list 'writer-unreadable
                 (list 'path (cdr (assq 'path d)))
                 (list 'reason (cdr (assq 'reason d))))))
        (else (loop (cdr es))))))

  ;; A LOCAL WRITER STOPPED BY A SEGMENT IT CANNOT READ IS NOT WRITTEN
  ;; (K12). Its readable prefix is delivered, but what lies past the stop
  ;; is unknown, and a record appended after it would be numbered against
  ;; history this store cannot see. The session refuses it in K9's shape,
  ;; naming the segment; it does not fall back to another local writer.
  (define (segment-unreadable-refusal ls writer)
    (let* ((entry (assoc writer (load-session-prefixes ls)))
           (note (and entry
                      (find (lambda (n)
                              (and (eq? (log-error-kind n) 'segment-unreadable)
                                   (pair? (log-error-detail n))
                                   (assq 'path (log-error-detail n))))
                            (discovery-integrity (cdr entry))))))
      (and note
           (let ((d (log-error-detail note)))
             (list 'writer-unreadable
                   (list 'path (cdr (assq 'path d)))
                   (list 'reason (cdr (assq 'reason d))))))))

  (define (session-writer-of store ls)
    (let ((locals (filter (lambda (e) (eq? (discovery-origin (cdr e)) 'local))
                          (load-session-prefixes ls))))
      (or (local-writer-of store ls)
          (and (pair? locals) (car (car locals))))))

  ;; THE LOCK IS RELEASED BY THE SAME UNWIND THAT RELEASES THE GUARD.
  ;; A guard clause only sees exceptions: a callback that escapes by
  ;; invoking a continuation captured outside log-begin unwinds without
  ;; raising, and the first version cleared the store flag while leaving
  ;; the flock held -- the worst of both, since the next log-begin then
  ;; passes the guard and blocks forever on a lock no one will release.
  (define (log-begin store on-deliver . declaration-opt)
    (define declaration (and (pair? declaration-opt) (car declaration-opt)))
    (unless (procedure? on-deliver)
      (assertion-violation 'log-begin "on-deliver must be a procedure" on-deliver))
    (claim-store! store 'log-begin)
    (let ((handed-over (vector #f))
          (held (vector #f)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let ((lock ((current-lock-acquire) (string-append store "/lock") 'exclusive)))
            (vector-set! held 0 lock)
            (guard (e (#t (raise e)))
              (trace-event! 'enter-critical
                            (cons (string-append store "/lock") 'exclusive) #f)
              ;; THE BARRIER IS NOT RUN HERE ANY MORE. It was this
              ;; session's, and now it is every delivery's -- `load-deliver!`
              ;; runs it for readers and for sessions alike, so running it
              ;; again here would be a second supplier of the same
              ;; promise, doing the same flushes twice and making a case
              ;; that counts them read differently for no reason the
              ;; store cares about.
              (let ((ls (open-load store 'held-exclusive declaration)))
                (let* ((found (session-writer-of store ls))
                       (stopped (and found (segment-unreadable-refusal ls found)))
                       (local (and (not stopped) found))
                       (refusal (or stopped (and (not local) (unreadable-writer-refusal ls))))
                       (entry (and local (assoc local (load-session-prefixes ls))))
                       (end (and entry (discovery-end-seq (cdr entry))))
                       (s (make-session store lock ls on-deliver local
                                        0 '() 0 #f #f
                                        (and end (+ 1 end))
                                        ;; AND DURABLE STARTS EQUAL TO
                                        ;; WRITTEN, because the delivery
                                        ;; barrier this load just ran is
                                        ;; what made the loaded tail
                                        ;; durable. Starting it at zero
                                        ;; would refuse every snapshot
                                        ;; until this session had written
                                        ;; something of its own.
                                        (or end 0)
                                        #f
                                        (metadata-versions store)
                                        #f '() '() '() '() #f 1
                                        (and end (+ end 1)) #f
                                        refusal declaration)))
                  ;; THE SESSION'S OWN METADATA BARRIER IS NOT RUN HERE
                  ;; EITHER. Its obligation was "before the first
                  ;; callback", and the delivery barrier now runs before
                  ;; every callback of every reader -- so this would be
                  ;; the same flushes a second time. What the session
                  ;; inherits instead is the RECORD of them, so its first
                  ;; append flushes what the delivery barrier did not.
                  (deliver-into! s)
                  ;; A SESSION DOES NOT WRITE ON A PARTIAL STATE (F77c, plan
                  ;; amendment A3): an initial delivery that an unreadable
                  ;; entry aborted is refused here, before any intent is
                  ;; resolved against it; the unwind below releases the lock.
                  (refuse-unreadable-abort! ls)
                  (session-barriered-set! s (load-session-barriered ls))
                  (vector-set! handed-over 0 #t)
                  s)))))
        (lambda ()
          (unless (vector-ref handed-over 0)
            (let ((lock (vector-ref held 0)))
              (when lock
                (vector-set! held 0 #f)
                (guard (e (#t (if #f #f))) ((current-lock-release) lock))))
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
    ;; A BASELINE THIS STORE CAN NO LONGER READ IS NOT A CALLER'S
    ;; MISTAKE. A session inherits the cut a previous one left -- from a
    ;; snapshot, or from a consumer that remembered it -- and between the
    ;; two the records it names may have become unreadable. Raising here
    ;; would hand that back as a broken tool and abort a session that has
    ;; done nothing wrong; the store instead goes to reset-pending
    ;; carrying WHY, and the next append refuses with it. Nothing claims
    ;; to have replayed what it could not read.
    ;;
    ;; A cut naming a sequence that never existed, with nothing wrong
    ;; with that writer, is still the caller's mistake and still raises.
    (for-each (lambda (e)
                (let ((limit (available-through s (car e))))
                  (when (and limit (> (cdr e) limit))
                    (let ((why (writer-integrity s (car e))))
                      (if why
                          (session-reset-pending-set!
                            s (list 'baseline-unreadable
                                    (list 'writer (car e))
                                    (list 'baseline (cdr e))
                                    (list 'readable limit)
                                    (list 'reason why)))
                          (assertion-violation 'session-applied!
                            "confirmed past the end of that writer's history"
                            (list (car e) (cdr e) limit)))))))
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
;; WHY THAT WRITER STOPS WHERE IT DOES. A baseline naming a sequence
  ;; beyond what this store can read is two different situations wearing
  ;; one shape: a caller reporting a sequence that never existed, which
  ;; is a bug in the caller, and history this store held yesterday and
  ;; cannot read today, which is a fact about the store. They are told
  ;; apart by asking whether that writer has integrity notes -- the
  ;; store's own record of having found something unreadable.
  (define (writer-integrity s writer)
    (let ((entry (assoc writer (load-session-prefixes (session-load s)))))
      (and entry
           (let ((notes (discovery-integrity (cdr entry))))
             (and (pair? notes) (log-error-kind (car notes)))))))

  ;; An unreadable writer has nothing known to be available: records that
  ;; depend on it wait. Its origin is asked before its end.
  (define (available-through s writer)
    (let* ((entry (assoc writer (load-session-prefixes (session-load s))))
           (found (and entry
                       (not (eq? (discovery-origin (cdr entry)) 'unreadable))
                       (discovery-end-seq (cdr entry))))
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
  ;; An unreadable writer's frontier is not a set of numbers: it is reported
  ;; as (<writer> (unreadable (path p) (reason r))), and its coordinates are
  ;; not asked.
  (define (session-frontiers s)
    (check-live! 'session-frontiers s)
    (let ((ls (session-load s)))
      (map (lambda (entry)
             (let* ((writer (car entry))
                    (p (cdr entry))
                    (applied (assoc writer (session-applied s))))
               (if (eq? (discovery-origin p) 'unreadable)
                   (let ((d (log-error-detail (unreadable-note p))))
                     (list writer (list 'unreadable (list 'path (cdr (assq 'path d)))
                                        (list 'reason (cdr (assq 'reason d))))))
               (list writer
                     ;; THE PHYSICAL CURSOR IS WHERE THE BYTES END, which
                     ;; is not where the validated prefix ends: a torn
                     ;; residue lies between them, and reporting the
                     ;; validated offset for both made the two frontiers
                     ;; that exist to differ report the same number.
                     (cons 'physical (discovery-physical-current p))
                     (cons 'contiguous (discovery-end-seq p))
                     (cons 'applied (if applied (cdr applied) 0))
                     (cons 'durable (discovery-end-seq p))))))
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

  (define (session-write-started? s) (session-write-started s))

  ;; THE POSITIONS THIS SESSION ACTUALLY TOOK, so a failure that cannot
  ;; say what succeeded can at least say where to look. It is derived
  ;; from the two counters rather than accumulated, because a list that
  ;; had to be appended to on every append is a second place that can be
  ;; wrong about the same fact.
  (define (session-written-events s)
    (let loop ((n (session-start-seq s)) (out '()))
      (if (or (not n) (not (session-next-seq s)) (>= n (session-next-seq s)))
          (reverse out)
          (loop (+ n 1) (cons (cons (session-writer s) n) out)))))

  ;; EVERY RELEASE HAPPENS EVEN WHEN AN EARLIER ONE FAILS. Three things
  ;; are being given back here and they are independent: finalising the
  ;; load, the store's lock, and the store itself. Written in sequence, a
  ;; raise in the first keeps the lock for the life of the process --
  ;; and the caller that raised is usually already handling a failure, so
  ;; the lock is lost exactly when the store is least well.
  (define (log-end! s)
    (check-live! 'log-end! s)
    (session-ended-set! s #t)
    (dynamic-wind
      (lambda () (if #f #f))
      (lambda ()
        (let ((ls (session-load s)))
          (when (eq? (load-outcome ls) 'open) (load-commit! ls))))
      (lambda ()
        (dynamic-wind
          (lambda () (if #f #f))
          (lambda () ((current-lock-release) (session-lock s)))
          (lambda () (release-store! (session-store s))))))
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
      (metadata-barrier-staged! s 'deliver-barrier)))

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

  (define (metadata-barrier-staged! s stage)
    (metadata-flush! (session-store s) stage
                     (lambda (w name) (last-flushed s w name))
                     (lambda (w name version) (note-flushed! s w name version))))

  ;; THE SAME FLUSH WITH THE MEMO MADE AN ARGUMENT. A session remembers
  ;; what it has already flushed, so a barrier it runs twice does the
  ;; work once; a reader has no session and no memo, and asking it to
  ;; invent one would be a second place that decides what "already
  ;; flushed" means. Both callers flush the same files for the same
  ;; reason; only the remembering differs.
  (define (metadata-flush! store stage remembered remember!)
    (let ((any (vector #f)))
      (for-each
        (lambda (w)
          ;; NEVER: A WRITER WHOSE DIRECTORY CANNOT BE LISTED IS SKIPPED, as
          ;; the delivery barrier skips it (K9), and traced the same way.
          ;; Searchable but not readable (--x), its files still read, so
          ;; they were flushed and then the directory itself was opened to
          ;; fsync it -- which needs read permission, and raised
          ;; durable-error out of every write beside such a mirror (F77b,
          ;; U2c mode 100). Its discovery already reports it unreadable.
          (if (guard (e ((unreadable-entry? e) #f))
                (list-entries (writer-directory store w))
                #t)
              (let ((dir (writer-directory store w))
                    (touched (vector #f)))
                (for-each
                  (lambda (name)
                    ;; A FILE THAT WILL NOT OPEN IS NOT FLUSHED HERE. Trying
                    ;; would raise out of a barrier that every session
                    ;; start runs, taking the whole store down for one
                    ;; writer's unreadable metadata. It is already recorded
                    ;; as unreadable where that fact belongs -- the reading
                    ;; side stops that writer, the writing side refuses its
                    ;; next append -- and neither of those can happen if
                    ;; this raises first.
                    (let ((version (file-version store w name)))
                      (when (and version
                                 (not (unreadable-version? version))
                                 (not (equal? version (remembered w name))))
                        (flush-file! (writer-file store w name) stage)
                        (remember! w name version)
                        (vector-set! touched 0 #t))))
                  metadata-files)
                ;; The namespace entry as well as the contents: a version
                ;; whose file is flushed but whose name is not is a version
                ;; that can vanish whole.
                (when (vector-ref touched 0)
                  (fsync-dir! dir stage)
                  (vector-set! any 0 #t)))
              (trace-event! 'barrier-skipped-unreadable w #f)))
        (store-writers store))
      ;; P1: THE WRITERS DIRECTORY ITSELF. A writer directory that
      ;; appeared mid-session has its own entry in writers/, and flushing
      ;; the contents of that directory says nothing about whether the
      ;; directory is still there after a crash -- a record whose deps
      ;; name that writer would then point at nothing.
      (when (vector-ref any 0)
        (fsync-dir! (string-append store "/writers") stage))
      ;; The store's own identity files are depended on by every append
      ;; and are not any writer's.
      (for-each
        (lambda (name)
          (let* ((path (string-append store "/" name))
                 (version (and (entry-present? path)
                               (segment-sha (read-whole path)))))
            (when (and version (not (equal? version (remembered "" name))))
              (flush-file! path stage)
              (remember! "" name version)
              (fsync-dir! store stage))))
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
  ;; THE SAME SENTINEL AS manifest-version, and for the same reason: a
  ;; metadata file that will not open is a fact about one writer, not an
  ;; exception that should leave a session start or a version comparison
  ;; by the exception path.
  ;; -> #f when the file is not there, its sha when it is, or
  ;; (unreadable <path> <reason> <errno>) when it cannot be read.
  (define (file-version store writer name)
    (let* ((p (writer-file store writer name))
           (r (guard (e ((unreadable-entry? e)
                         (list 'unreadable p (unreadable-entry-reason e)
                               (unreadable-entry-errno e))))
                (read-entry p))))
      (cond ((eq? r 'absent) #f)
            ((pair? r) r)
            (else (segment-sha r)))))

  ;; Which of this writer's metadata files cannot be read, if any.
  ;; NEVER: THE DIRECTORY IS LISTED FIRST, as R1's listing (R4; F77b). A
  ;; directory whose permission was lost after the session opened fails
  ;; every file below it, and each file's read names the file, not the
  ;; directory that is the cause. Listing it names the directory and its
  ;; reason; absence is left to the reads below, as before.
  (define (unreadable-metadata store writer)
    (or (guard (e ((unreadable-entry? e)
                   (list (unreadable-entry-path e) (unreadable-entry-reason e))))
          (list-entries (writer-directory store writer))
          #f)
        (unreadable-metadata-file store writer)))

  (define (unreadable-metadata-file store writer)
    (let loop ((names metadata-files))
      (cond
        ((null? names) #f)
        ((unreadable-version? (file-version store writer (car names)))
         (let ((v (file-version store writer (car names))))
           (list (cadr v) (caddr v))))
        (else (loop (cdr names))))))

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
           (fresh (open-load store 'held-exclusive (session-declaration s))))
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
        ;; A RELOAD RE-READS THE LOG, AND THE DELIVERY BARRIER RAN OVER
        ;; IT, so what the reload found is durable. Both frontiers move
        ;; together here for the same reason they start together at
        ;; log-begin -- and anything this session had written and not
        ;; made durable is not in the reload either.
        (session-durable-seq-set! s (if entry (discovery-end-seq (cdr entry)) 0))
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

  ;; A writer that has become unreadable is one whose applied records can no
  ;; longer be seen: if any of them was applied, the session is beyond what
  ;; the load shows, and a reset follows. Its origin is asked before its end.
  (define (applied-beyond? s ls)
    (let loop ((es (session-applied s)))
      (cond
        ((null? es) #f)
        (else
         (let* ((entry (assoc (caar es) (load-session-prefixes ls)))
                (reach (cond ((not entry) 0)
                             ((eq? (discovery-origin (cdr entry)) 'unreadable) 0)
                             (else (discovery-end-seq (cdr entry))))))
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
      (and (entry-present? path)
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
  ;;    | (refused owner-unreadable (path p) (reason r))
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
               (else
                (let ((owned (owner-nonce-or-refusal store)))
                  (cond
                    ((owner-unreadable? owned)
                     (list 'refused 'owner-unreadable
                           (list 'path (owner-unreadable-path owned))
                           (list 'reason (owner-unreadable-reason owned))))
                    ((and owned (not (equal? owned (alist-ref d 'nonce))))
                     (list 'mismatch 'nonce))
                    (else 'ok)))))))))))

  ;; -> the owner's nonce, #f, or an owner-unreadable record. "Is this
  ;; instance's ownership assertion intact?" cannot be answered yes or no
  ;; when the assertion cannot be read (R2i), so verify-instance answers
  ;; (refused owner-unreadable (path p) (reason r)) itself, and each of its
  ;; callers gives that as its own answer.
  ;; NEVER: THE REFUSAL IS A RECORD, NOT A LIST (F77b review 1). A nonce is
  ;; whatever datum owner.sexp holds, and a list headed `refused` made a
  ;; readable owner whose nonce was such a list answer as a refusal.
  (define-record-type owner-unreadable (fields path reason))
  (define (owner-nonce-or-refusal store)
    (guard (e ((unreadable-entry? e)
               (make-owner-unreadable (unreadable-entry-path e) (unreadable-entry-reason e))))
      (owner-nonce store)))

;; THE HEAD'S OWNER, not whichever writer happens to sort first. After
  ;; an identity-mismatch adopt the store holds owners from two
  ;; instances: the retired predecessors name the nonce the store had
  ;; when it was copied, and only the writer this machine may actually
  ;; extend names the current one. Taking the first left a store that
  ;; had just adopted failing its own identity check forever, because
  ;; the answer came from a generation that had been superseded.
  ;; "Which writer is this machine's" has one supplier.
  ;; NEVER: AN OWNER THAT CANNOT BE READ IS NOT AN OWNER THAT IS NOT THERE
  ;; (R2g, R2i; F77b). The read is the R1 operation: absence answers #f as
  ;; before, and any other failure raises unreadable-entry naming
  ;; owner.sexp, which verify-instance answers as owner-unreadable. Only a
  ;; file that was read and does not parse still answers #f, as it did.
  (define (owner-nonce store)
    (let ((head (local-writer-name store)))
      (and head
           (let* ((path (writer-file store head "owner.sexp"))
                  (bytes (read-entry path))
                  (d (and (not (eq? bytes 'absent))
                          (guard (e (#t #f))
                            (string->sexpr-extended (utf8->string bytes))))))
             (and d (alist-ref d 'instance))))))

  ;; The machine's own identity: a name plus a nonce minted once and kept
  ;; in the machine home, so that two machines that happen to share a
  ;; host name are still two machines.
  ;; MINTED ONCE PER MACHINE HOME, on first use. Two machines that
  ;; happen to share a host name are still two machines, so the identity
  ;; is a nonce rather than the name; it lives beside the registry
  ;; because that is the thing it qualifies.
  (define (machine-id)
    (let ((path (string-append (machine-home) "/machine.sexp")))
      (if (entry-present? path)
          (guard (e (#t "unknown"))
            (alist-ref (string->sexpr-extended (utf8->string (read-whole path))) 'machine))
          ;; MINTED UNDER THE MACHINE LOCK, and re-read inside it. Two
          ;; processes that both find the file missing would otherwise
          ;; both mint, and the loser's freshly initialised store fails
          ;; its own identity check on the very next append.
          (begin
            (ensure-machine-home! 'registry)
            (with-machine-lock
              (lambda ()
                (if (entry-present? path)
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

  ;; A STORE ID THAT IS NOT A STRING IS NOT A STORE ID. `format-1?`
  ;; checks the format field and nothing else, so metadata saying
  ;; `(store-id 7)` reaches here -- and a registry entry keyed by 7 is
  ;; one `water-mark-entry?` does not recognise, so `written` is never
  ;; raised for it and every reservation appends a duplicate. The store
  ;; would then acknowledge writes with no working rollback witness.
  ;;
  ;; "unknown" IS THE SAME ANSWER AS AN ABSENT ID, and deliberately: both
  ;; are "this metadata does not say", and neither is allowed to become a
  ;; key of a shape the rest of the file cannot read.
  (define (store-id-of store)
    (let* ((d (guard (e (#t #f))
                (string->sexpr-extended
                  (utf8->string (read-whole (string-append store "/meta.sexp"))))))
           (id (alist-ref d 'store-id)))
      (if (string? id) id "unknown")))

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
  (define (ensure-machine-home! stage)
    (let ((home (machine-home)))
      (unless (file-is-directory? home)
        (mkdir-p! home)
        (fsync-dir! (parent-of home) stage))
      (file-ensure! (machine-lock-path))
      home))

  (define (parent-of path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 1) "/")
        ((char=? (string-ref path i) #\/) (substring path 0 i))
        (else (loop (- i 1))))))

  ;; EVERY CALLER HOLDS THE MACHINE LOCK, which is what lets this upgrade
  ;; an old registry in place rather than teach every reader two shapes.
  (define (read-registry)
    (upgraded-registry (registry-as-read)))

  ;; THE REGISTRY AS IT IS ON DISK, NOT UPGRADED AND NEVER WRITTEN (R2h,
  ;; F77b). A read that upgrades a legacy registry writes it,
  ;; and a preflight must not write: adopt and its continuation decide on
  ;; this reading, and the upgrade happens, if at all, through
  ;; read-registry after every read has passed.
  ;; R1's read (F77b review 1): absent is the empty registry, as before, and
  ;; a registry this process cannot read raises unreadable-entry naming it;
  ;; only one that was read and does not parse is registry-malformed.
  (define (registry-as-read)
    (let* ((path (registry-path))
           (bytes (begin (trace-event! 'registry-check path #f) (read-entry path))))
      (if (eq? bytes 'absent)
          '()
          (let ((d (guard (e (#t 'malformed))
                     (string->sexpr-extended (utf8->string bytes)))))
            (cond
              ((eq? d 'malformed)
               (raise (make-log-error 'registry-malformed #f #f #f
                                      (list (cons 'path path)))))
              ((list? d) d)
              (else
               (raise (make-log-error 'registry-malformed #f #f #f
                                      (list (cons 'path path))))))))))

  ;; ONE SHAPE, AND THE OLD ONE IS CONVERTED RATHER THAN TOLERATED. A
  ;; water mark used to be a single number meaning both "authorised to
  ;; write here" and "written this far"; they are two facts now and a
  ;; five-element entry is the moment before anyone noticed. Its
  ;; `written` is its mark, because that is exactly what the mark meant
  ;; while the two were the same.
  ;;
  ;; IT IS CONVERTED ONCE, NOT READ LENIENTLY EVERY TIME. A format that
  ;; can be read two ways is a format two readers will eventually
  ;; disagree about -- and the disagreement would be about whether a
  ;; store rolled back, which is the question this file exists to answer.
  ;; Nothing is published yet, so no old dialect has to be kept.
  (define (upgraded-registry reg)
    (let ((changed (vector #f)))
      (let ((next (map (lambda (e)
                         (if (and (water-mark-entry? e) (= 5 (length e)))
                             (begin (vector-set! changed 0 #t)
                                    (append e (list (list-ref e 3))))
                             e))
                       reg)))
        (when (vector-ref changed 0)
          (write-registry! next))
        next)))

  ;; A WATER MARK ENTRY, TOLD FROM A GENERATION RECORD BY ITS HEAD. One
  ;; file holds both; a generation leads with the symbol `gen` and a
  ;; water mark with a store id, which is a string.
  (define (water-mark-entry? e)
    (and (list? e) (>= (length e) 5) (string? (car e))))

  ;; ENTRIES ARE (store-id instance writer authorised state written),
  ;; keyed by the first three.
  ;;
  ;; TWO FACTS, NOT ONE. `authorised` is how far a request has been given
  ;; leave to write; `written` is how far records actually reached the
  ;; disk. They were one number while every append reserved its own
  ;; sequence and flushed it, and splitting them is what lets a request
  ;; reserve its whole range in one go: a request that reserves five
  ;; positions and writes two leaves `authorised` above `written`, and
  ;; that is an ordinary state rather than the rollback the gate is
  ;; looking for.
  ;;
  ;; BOTH ONLY EVER RISE. That is what makes a concurrent
  ;; reader-modifier safe under the machine lock: two processes that both
  ;; read 100 and write 101 and 102 cannot lose the larger, because the
  ;; merge takes the maximum rather than the later write.
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

  ;; THE ENTRY registry-update WOULD CHANGE: a water mark under the key.
  ;; registry-entry above also answers a keyed row too short to be one,
  ;; which registry-update leaves alone -- so a caller that asks "is there
  ;; an entry to raise" and then raises through registry-update must ask
  ;; with this, or a malformed row answers yes and nothing is raised.
  (define (registry-water-mark reg store-id instance writer)
    (find (lambda (e)
            (and (water-mark-entry? e)
                 (equal? (car e) store-id)
                 (equal? (cadr e) instance)
                 (equal? (caddr e) writer)))
          reg))

  (define (registry-authorised reg store-id instance writer)
    (let ((e (registry-entry reg store-id instance writer)))
      (and e (list-ref e 3))))

  (define (registry-written reg store-id instance writer)
    (let ((e (registry-entry reg store-id instance writer)))
      (and e (list-ref e 5))))

  (define (registry-raise reg store-id instance writer seq)
    (registry-update reg store-id instance writer
                     (lambda (e) (list store-id instance writer
                                       (max seq (list-ref e 3))
                                       (list-ref e 4)
                                       (list-ref e 5)))
                     (list store-id instance writer seq 'active 0)))

  ;; HOW FAR RECORDS ACTUALLY REACHED THE DISK, raised by the barrier
  ;; that made them so and by nothing else. An entry that does not exist
  ;; yet cannot have written anything, so there is nothing to note.
  (define (registry-note-written reg store-id instance writer seq)
    (registry-update reg store-id instance writer
                     (written-raised store-id instance writer seq)
                     #f))

  ;; THE SAME RAISE, CREATING THE ENTRY WHEN THERE IS NONE: the count an
  ;; acknowledgement needs. `authorised` and `written` both start at seq,
  ;; in the documented shape (state `active` in field 4). Only the
  ;; acknowledgement of a flushed record (note-written-for!) creates.
  (define (registry-acknowledge-written reg store-id instance writer seq)
    (registry-update reg store-id instance writer
                     (written-raised store-id instance writer seq)
                     (list store-id instance writer seq 'active seq)))

  ;; ONE RAISE FOR BOTH: authorised and written each to seq by max, every
  ;; other field left alone.
  (define (written-raised store-id instance writer seq)
    (lambda (e) (list store-id instance writer
                      (max seq (list-ref e 3))
                      (list-ref e 4)
                      (max seq (list-ref e 5)))))

  ;; ONE MERGE, TWO CALLERS. Both raise a number in place and both have
  ;; to leave every other field of the entry alone; two copies of that
  ;; walk would drift the first time the entry gained a field.
  (define (registry-update reg store-id instance writer change absent)
    (let ((found (vector #f)))
      (let ((updated
              (map (lambda (e)
                     (if (and (water-mark-entry? e)
                              (equal? (car e) store-id)
                              (equal? (cadr e) instance)
                              (equal? (caddr e) writer))
                         (begin (vector-set! found 0 #t) (change e))
                         e))
                   reg)))
        (cond
          ((vector-ref found 0) updated)
          (absent (append updated (list absent)))
          (else updated)))))

  ;; THE MACHINE LOCK IS TAKEN AFTER THE STORE LOCK, ALWAYS. The order is
  ;; fixed so that two processes touching two stores cannot each hold one
  ;; of the pair and wait for the other.
  (define (with-machine-lock thunk)
    (ensure-machine-home! 'registry)
    ;; THE MACHINE HOME MAY NOT BE THE STORE. The store lock is already
    ;; held when this runs, so a home inside the store would make this
    ;; acquire the same file through a second descriptor and wait for a
    ;; lock this very call stack is holding.
    (when (store-lock-collision?)
      (assertion-violation 'with-machine-lock
        "THEOURGIA_HOME must not put the machine lock inside a store" (home-now)))
    (let ((lock ((current-lock-acquire) (machine-lock-path) 'exclusive)))
      (dynamic-wind
        (lambda () (if #f #f))
        thunk
        (lambda () (guard (e (#t (if #f #f))) ((current-lock-release) lock))))))

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

;; THE REGISTRY MUST NOT LIVE INSIDE A STORE, and "inside" means any
  ;; depth, not just "is". The registry is the one witness to a rollback
  ;; that does not travel with a backup of the store -- so putting it
  ;; under the store puts the witness inside the thing it is watching.
  ;; Restoring the store in place then restores the registry too, the
  ;; generation record vanishes along with the generation, the chain
  ;; agrees, and the rolled-back store writes on happily. It fails
  ;; silently: the store opens, writes, and reports itself healthy.
  ;;
  ;; ANCESTRY IS WALKED BY (device, inode), NOT BY COMPARING STRINGS. A
  ;; symlinked home whose target sits under the store has a path that
  ;; shares no prefix with it; stat'ing "<home>/.." resolves the link
  ;; first, so each step up is the real parent and an alias cannot get
  ;; past it.
  (define (dir-identity path)
    (guard (e (#t #f))
      (call-with-values (lambda () (path-device-inode path))
        (lambda (d i) (cons d i)))))

  ;; THE HOME MAY NOT EXIST YET -- it is created on first use, and this
  ;; question is asked before that. So the walk starts at the nearest
  ;; ancestor that does exist, found by trimming path components
  ;; lexically; from there every step is a stat, which is what makes a
  ;; symlink unable to hide.
  (define (nearest-existing path)
    (let loop ((p path) (n 0))
      (cond
        ((> n 64) #f)
        ((dir-identity p) p)
        (else
         (let ((cut (let scan ((i (- (string-length p) 1)))
                      (cond ((< i 1) #f)
                            ((char=? (string-ref p i) #\/) i)
                            (else (scan (- i 1)))))))
           (and cut (loop (substring p 0 cut) (+ n 1))))))))

  (define (registry-inside-store?)
    (let ((start (nearest-existing (home-now)))
          (home (home-now)))
      (and start
           ;; a home that IS a store is the other check's business, so
           ;; the walk only reports ancestors -- unless the home did not
           ;; exist, in which case its nearest existing ancestor is a
           ;; genuine ancestor and counts.
           (let ((skip-self (string=? start home)))
             (let loop ((p start) (depth 0))
               (cond
                 ((> depth 64) #f)
                 ((and (or (> depth 0) (not skip-self))
                       (entry-present? (string-append p "/meta.sexp")))
                  #t)
                 (else
                  (let* ((up (string-append p "/.."))
                         (a (dir-identity p))
                         (b (dir-identity up)))
                    (cond
                      ((not b) #f)
                      ((equal? a b) #f)
                      (else (loop up (+ depth 1))))))))))))

  ;; INIT PUTS THE STORE IN THE MACHINE REGISTRY AT WATER MARK ZERO.
  ;; The registry is what stops two instances of one store from writing
  ;; past each other, and a store that is not in it is invisible to that
  ;; check until its first append -- so the window in which a second
  ;; instance could be made without anything noticing is exactly the
  ;; window between init and the first write.
  (define (store-register! store)
    (reserve! store (store-id-of store) (instance-nonce store)
              (local-writer-name store) 0))

;; THE HEAD OF THIS MACHINE'S CHAIN: local, and not already retired.
  ;; After an adopt both generations have an owner.sexp -- that is what
  ;; makes them local -- so taking the first would name a writer this
  ;; machine may no longer extend, and adopting it again would branch
  ;; from a generation that has already been superseded.
  (define (local-writer-name store)
    (let ((locals (filter (lambda (w) (present-or-unreadable-skip? (writer-file store w "owner.sexp")))
                          (store-writers store))))
      (let loop ((ws locals))
        (cond
          ((null? ws) (if (null? locals) #f (car locals)))
          ((not (retired-of store (car ws))) (car ws))
          (else (loop (cdr ws)))))))

;; ---- publish: reading a segment record by record ---------------------------

  ;; SCAN-SEGMENT STOPS AT THE FIRST ERROR, which is the right answer to
  ;; "where does this writer's valid history end". Repair asks a
  ;; different question -- WHICH records inside a file that fails as a
  ;; whole are individually valid -- and section 4.4-prime says those are
  ;; not the same question: a segment failing validation is never grounds
  ;; for discarding the valid records inside it.
  ;; THE FRAMING RULE STILL HAS ONE SUPPLIER. This splits on the newline
  ;; and hands each line to `decode-line`, exactly as the scanner does;
  ;; what differs is that it carries on past a bad one.
  (define (segment-records bv)
    (let ((n (bytevector-length bv)))
      (let loop ((start 0) (out (quote ())))
        (if (>= start n)
            (reverse out)
            (let ((nl (find-newline bv start n)))
              (if (not nl)
                  (reverse (cons (list (quote torn) start n (subbytes bv start n) #f) out))
                  (let* ((end (+ nl 1))
                         (line (subbytes bv start end))
                         (r (decode-line line)))
                    (loop end
                          (cons (if (and (pair? r) (eq? (car r) (quote ok)))
                                    (list (cadr r) start end line #t)
                                    (list (quote bad) start end line #f))
                                out)))))))))

  ;; THE RANGE THE BYTES HOLD, WITHOUT PARSING ALL OF THEM. What the
  ;; manifest declares is a first and a last sequence, so the first and
  ;; last framed lines answer it. Reading every record to learn two
  ;; numbers made the check a second full pass over every listed segment
  ;; on every open -- work proportional to the history, repeated, to
  ;; compare two integers.
  (define (segment-edge-seqs bv)
    (let ((n (bytevector-length bv)))
      (and (> n 0)
           (let ((first-nl (find-newline bv 0 n)))
             (and first-nl
                  (let ((last-start
                          (let loop ((i (- n 2)))
                            (cond ((< i 0) 0)
                                  ((= 10 (bytevector-u8-ref bv i)) (+ i 1))
                                  (else (loop (- i 1)))))))
                    (and (= 10 (bytevector-u8-ref bv (- n 1)))
                         (let ((a (line-seq bv 0 (+ first-nl 1)))
                               (b (line-seq bv last-start n)))
                           (and a b (cons a b))))))))))

  (define (line-seq bv start end)
    (let ((r (decode-line (subbytes bv start end))))
      (and (pair? r) (eq? (car r) 'ok) (cadr r))))

  (define (rec-seq r) (car r))
  (define (rec-bytes r) (cadddr r))
  (define (rec-ok? r) (car (cddddr r)))
  (define (rec-end r) (caddr r))

  (define (valid-records rs) (filter rec-ok? rs))

  (define (records-contiguous? rs)
    (let loop ((xs rs) (prev #f))
      (cond
        ((null? xs) #t)
        ((not (rec-ok? (car xs))) #f)
        ((and prev (not (= (rec-seq (car xs)) (+ prev 1)))) #f)
        (else (loop (cdr xs) (rec-seq (car xs)))))))

  (define (find-record rs seq)
    (let loop ((xs rs))
      (cond ((null? xs) #f)
            ((and (rec-ok? (car xs)) (eqv? (rec-seq (car xs)) seq)) (car xs))
            (else (loop (cdr xs))))))

;; ---- publish: the dispatch (section 9.6) -----------------------------------

  ;; COMPARISON IS BY (writer, seq) AGAINST THAT WRITER'S HISTORY, never
  ;; by target filename: two forks may rotate at different points, so one
  ;; event can sit in different segment numbers. And by BYTES, never by
  ;; datum -- `storable-decode` is many-to-one on inputs nobody wrote and
  ;; CRC32 is 32 bits, so a divergent record with an equal datum would be
  ;; accepted silently.

  ;; Every valid record this writer has, across all its segments, as
  ;; (seq . bytes). The cross-segment divergence check needs this:
  ;; comparing only the target file cannot see a disagreement that falls
  ;; in a neighbouring segment.
  (define (writer-history store writer)
    (let loop ((ns (list-sort < (enumerate-segment-files store writer))) (out '()))
      (if (null? ns)
          out
          (let* ((path (string-append (writer-directory store writer)
                                      "/" (segment-file-name (car ns))))
                 ;; R1's read (F77b review 2): a sibling segment this store
                 ;; cannot read raises unreadable-entry naming it.
                 (rs (let ((b (read-entry path))) (if (eq? b 'absent) '() (segment-records b)))))
            (loop (cdr ns)
                  (append out
                          (map (lambda (r) (cons (rec-seq r) (rec-bytes r)))
                               (valid-records rs))))))))

  ;; HOW A SEGMENT'S HASH IS WRITTEN, in one place. The manifest, the
  ;; quarantine evidence, the caller's declaration and the command line
  ;; all have to mean the same string by it, and a second way of
  ;; computing it is a second answer waiting to disagree.
  (define (segment-sha bytes) (bytevector->hex (sha256 bytes)))

  (define (seq-range rs)
    (let ((vs (map rec-seq (valid-records rs))))
      (if (null? vs) #f (cons (apply min vs) (apply max vs)))))

  ;; THE LAYOUT GATE: can these two ranges be compared at all? It runs
  ;; before the validate/invalid split, because several rows below take
  ;; "shorter" and "longer" to mean prefix relations. Local 1-100 against
  ;; a candidate 90-110 is shorter by length and would have been answered
  ;; `incomplete`, silently discarding 101-110.
  (define (ranges-overlap? a b)
    (and a b (<= (car a) (cdr b)) (<= (car b) (cdr a))))

  (define (layout-conflict local-rs cand-rs target-exists?)
    (let ((lr (seq-range local-rs))
          (cr (seq-range cand-rs)))
      (cond
        ((not cr) 'empty-candidate)
        ((not lr) (and target-exists? 'target-occupied))
        ;; THE TWO CONFLICTS ARE DIFFERENT SITUATIONS AND SAY SO. A
        ;; candidate that shares no sequence with what is here has not
        ;; disagreed about anything -- the segment NUMBER is taken. A
        ;; candidate that overlaps but starts elsewhere cannot be
        ;; compared as a prefix at all, which is what the rows below
        ;; assume when they read "shorter" and "longer".
        ((not (ranges-overlap? lr cr)) 'target-occupied)
        ((not (= (car lr) (car cr))) 'not-start-aligned)
        ;; candidate shorter: A3 decides
        ((< (cdr cr) (cdr lr)) #f)
        (else #f))))

  (define (segment-range store writer n)
    (let ((path (string-append (writer-directory store writer)
                               "/" (segment-file-name n))))
      (and (entry-present? path)
           (seq-range (segment-records (read-whole path))))))

  ;; WHERE THE HISTORY BELOW THIS SEGMENT ENDS. Layout coordinates come
  ;; from what the store DECLARES, never from whichever files happen to
  ;; lie in the directory. Two declarations exist: the manifest lists the
  ;; segments this writer has published, with the range each one holds,
  ;; and the retirement record names the sequence a retained prefix ends
  ;; at. A file neither of them names is an orphan -- bytes that arrived
  ;; from somewhere and are not this writer's history -- so a writer with
  ;; nothing declared below ends at 0, and a candidate for its first
  ;; segment must begin at 1. Taking the end from the files instead let a
  ;; killed install, whose segment was written but never listed, supply a
  ;; history no reader can see.
  ;;
  ;; AND IT IS THE END OF THE TRAVERSABLE PREFIX, NOT THE HIGHEST
  ;; SEQUENCE DECLARED. They differ only when the declarations already
  ;; hold a hole, and there the difference decides whether the hole can
  ;; ever be filled: with segments 1 and 3 declared and 2 free, the
  ;; highest declared sequence would call a candidate for segment 2 an
  ;; overlap and refuse the one segment number that could repair the
  ;; history. Segment numbers only increase, so a refusal there is
  ;; permanent. Reading forward from 1 -- or from the end of a retired
  ;; prefix -- and stopping at the first break answers the question a
  ;; reader would ask, and keeps the repair reachable.
  (define (declared-end store writer segment)
    (let* ((m (read-manifest store writer))
           (listed (list-sort < (filter (lambda (n) (< n segment))
                                        (manifest-segments m)))))
      (let loop ((ns listed) (end (retired-end-below store writer segment)))
        (if (null? ns)
            end
            (let ((r (manifest-range m (car ns))))
              (if (and r (= (car r) (+ end 1)))
                  (loop (cdr ns) (cdr r))
                  end))))))

  ;; A RETIRED PREFIX IS DECLARED HISTORY THAT NO MANIFEST LISTS. The
  ;; record names the sequence the retained prefix ends at, which is the
  ;; only statement a store makes about a writer whose early records are
  ;; no longer where a reader would look for them. The file may hold more
  ;; than the prefix, and whatever reads beyond it is history too, so the
  ;; declaration is a floor rather than the answer.
  (define (retired-end-below store writer segment)
    (let ((r (retired-of store writer)))
      (if (and r (not (eq? (car r) 'malformed)) (< (car r) segment))
          (let ((br (segment-range store writer (car r))))
            (if br (max (caddr r) (cdr br)) (caddr r)))
          0)))

  ;; AND WOULD THE RESULT READ? Even with every record identical,
  ;; installing 1-8 as segment one when segment two holds 6-10 leaves a
  ;; reader meeting 6 again after 8 and stopping, hiding 9 and 10.
  ;;
  ;; A READER WALKS INTO THE CANDIDATE AS WELL AS OUT OF IT, so the
  ;; question has a second half, and the second half has two sides. A
  ;; candidate beginning past the end of what precedes it leaves the
  ;; sequences between them owned by nobody; one beginning before that
  ;; end repeats sequences a reader has already passed, and the reader
  ;; stops at the repeat. Only the forward half was written at first, and
  ;; a free segment number was therefore enough to make a floating
  ;; candidate read as a new segment: a writer whose history ended at 2
  ;; published a candidate of 20-21.
  ;;
  ;; The answer is a reason rather than a boolean because the three ask
  ;; for different repairs. Forward says this candidate is wrong for this
  ;; segment number; a gap says something between has not arrived yet; an
  ;; overlap says the sender and this store disagree about where this
  ;; segment starts. Each names the two numbers that disagree.
  (define (layout-read-problem store writer segment cand-rs)
    (let ((cr (seq-range cand-rs)))
      (and cr
           (let ((before (declared-end store writer segment)))
             (cond
               ((> (car cr) (+ before 1))
                (list 'gap-before-candidate
                      (list 'history-ends before)
                      (list 'candidate-starts (car cr))))
               ((< (car cr) (+ before 1))
                (list 'overlaps-preceding
                      (list 'history-ends before)
                      (list 'candidate-starts (car cr))))
               ((not (forward-reads-through? store writer segment cr))
                'would-not-read-through)
               (else #f))))))

  (define (forward-reads-through? store writer segment cr)
    (let* ((m (read-manifest store writer))
           (others (list-sort < (filter (lambda (n) (> n segment))
                                        (manifest-segments m)))))
      (let loop ((ns others) (highest (cdr cr)))
        (if (null? ns)
            #t
            (let ((r (manifest-range m (car ns))))
              (cond
                ((not r) (loop (cdr ns) highest))
                ;; a later segment must begin where the candidate ends
                ((= (car r) (+ highest 1)) (loop (cdr ns) (cdr r)))
                (else #f)))))))

  ;; Divergence, looked for across the writer's whole history.
  (define (first-divergence history cand-rs)
    (let loop ((xs (valid-records cand-rs)))
      (cond
        ((null? xs) #f)
        ((let ((mine (assv (rec-seq (car xs)) history)))
           (and mine (not (bytevector=? (cdr mine) (rec-bytes (car xs))))
                (rec-seq (car xs))))
         => (lambda (seq) seq))
        (else (loop (cdr xs))))))

;; ---- publish: the actions --------------------------------------------------

  ;; STAGING IS THE SHARED FIRST STEP of install and overwrite. Writing
  ;; it only under install left the repair path flushing the evidence
  ;; copy and the directory but never the bytes it was about to put in
  ;; place: flushing the evidence does not flush the replacement.
;; AN UNLABELLED DURABILITY POINT CANNOT BE REACHED BY ANY TEST. A
  ;; staged fault never matches a call site that declares no stage, so
  ;; the flush below -- and the evidence flush beside it -- were the only
  ;; two steps of an installation that no case could arm. They were not
  ;; untested by choice; they were untestable, and that is indis-
  ;; tinguishable from tested until someone tries to write the row.
  (define (stage-candidate! store writer bytes stage)
    ;; AN ARM POINT BEFORE PUBLISH'S FIRST STAGING WRITE (F100a, design D2):
    ;; a fixture parks the process here to change the store between the
    ;; checks that come before it and the writes that follow. It does
    ;; nothing unless THEOURGIA_BARRIER names it; F100d is its first user.
    (barrier! 'before-stage)
    (parameterize ((theourgia-stage stage))
    (let* ((target (string-append (writer-directory store writer) "/publish"))
           (tmp (temp-name-for target))
           (fd (fd-open tmp '(write create))))
      (dynamic-wind
        void
        (lambda ()
          (guard (e (#t (guard (e2 (#t (void))) (fd-close fd))
                        (guard (e2 (#t (void))) (unlink! tmp))
                        (raise e)))
            (write-all! fd bytes tmp))
          (fsync! fd tmp stage))
        (lambda () (guard (e (#t (void))) (fd-close fd))))
      tmp)))

  ;; A DIRECTORY MADE FOR THE FIRST TIME NEEDS ITS OWN ENTRY PERSISTED.
  ;; Fsyncing a directory does not persist its name in the directory
  ;; above it, so a crash can take the whole directory and everything in
  ;; it while the work that depended on it looks done.
  (define (ensure-directory! path)
    (unless (file-is-directory? path)
      (mkdir-p! path)
      (directory-entry-durable! path 'publish)))

  (define (install-segment! store writer segment tmp sha bytes)
    (let ((target (string-append (writer-directory store writer)
                                 "/" (segment-file-name segment))))
      ;; link(2) REFUSES TO OVERWRITE, so "publish never overwrites a
      ;; sealed segment" is enforced by the syscall rather than by a
      ;; check a race could pass.
      (link! tmp target)
      (unlink! tmp)
      ;; INJECTION ONLY (item 7): held after the link and the unlink and
      ;; before the directory flush, the store's scope open (A-record).
      (hold-point! 'publish-after-link)
      (directory-entry-durable! target 'publish)
      (add-to-manifest! store writer segment sha bytes)
      (list 'published segment)))

  ;; REPLACEMENT, WHICH IS THE ONLY OTHER EXCEPTION TO SECTION 9.7.8.
  ;; The original is never unlinked first: "move away, then install"
  ;; leaves a window with no file at all, and a reader in that window
  ;; sees a gap rather than a whole history.
  ;; THE ANSWER NAMES THE EVIDENCE IT LEFT. A replacement puts the bytes
  ;; it displaced under `damaged/` with a generated name, and the sender
  ;; is the one party that may want them -- to see what its peer had, or
  ;; to keep them before they are swept. Deriving that name on the other
  ;; side would be a second supplier of it, and the two would part
  ;; company the first time the naming rule changed here.
  ;;
  ;; It is reported for every replacement, not only repairs: an extension
  ;; displaces bytes for the same reason and leaves them in the same
  ;; place, and an answer that named the file in one case and not the
  ;; other would be describing the implementation rather than what
  ;; happened.
  (define (overwrite-segment! store writer segment tmp sha kind bytes)
    (let* ((dir (writer-directory store writer))
           (target (string-append dir "/" (segment-file-name segment)))
           (damaged (string-append dir "/damaged"))
           (kept (vector #f)))
      (when (entry-present? target)
        (ensure-directory! damaged)
        (let ((evidence (evidence-path damaged segment)))
          (vector-set! kept 0 evidence)
          (let ((fd (fd-open evidence '(write create))))
            (dynamic-wind void
              (lambda ()
                (parameterize ((theourgia-stage 'publish))
                  (write-all! fd (read-whole target) evidence)
                  (fsync! fd evidence 'publish)))
              (lambda () (guard (e (#t (void))) (fd-close fd)))))
          ;; THE EVIDENCE'S NAME, not only its contents. Its bytes are
          ;; already durable: they were flushed through the descriptor
          ;; that wrote them, just above. They were once flushed a second
          ;; time through a second descriptor, which was a second
          ;; supplier and not a second mechanism -- the same syscall on
          ;; the same inode, so whatever fails the first fails the
          ;; second. No case could tell that call's absence from its
          ;; presence, and being unfalsifiable was the argument for
          ;; removing it rather than for trusting it.
          ;;
          ;; Until this returns, nothing has been replaced and the whole
          ;; thing is retryable -- which is why the moment before it is
          ;; not "evidence durable".
          (directory-entry-durable! evidence 'publish)))
      (rename-over! tmp target)
      (directory-entry-durable! target 'publish)
      (add-to-manifest! store writer segment sha bytes)
      (if (vector-ref kept 0)
          (list kind segment (list 'evidence (vector-ref kept 0)))
          (list kind segment))))

  (define (evidence-path damaged segment)
    (let loop ((n 0))
      (let ((p (string-append damaged "/" (segment-file-name segment)
                              "." (number->string (wall-clock-ms))
                              "." (number->string n))))
        (if (entry-present? p) (loop (+ n 1)) p))))

  ;; THE RANGE IS TAKEN FROM THE BYTES BEING PUBLISHED, once, here. They
  ;; have passed their hash and their contiguity by the time anything is
  ;; installed, so this is the one moment at which the declaration and
  ;; what it describes are known to be the same thing.
  (define (add-to-manifest! store writer segment sha bytes)
    (let ((r (seq-range (segment-records bytes))))
      (unless r
        (assertion-violation 'add-to-manifest!
                             "a published segment holds no record" segment))
      (let* ((current (or (read-manifest store writer) '()))
             (without (remp (lambda (e) (eqv? (car e) segment)) current))
             (next (list-sort (lambda (a b) (< (car a) (car b)))
                              (cons (list segment sha (car r) (cdr r)) without))))
        (write-manifest! store writer next))))

  ;; THE FORK IS MONOTONE. An existing fork at five and a new
  ;; disagreement at eight must not move the marker to eight: that would
  ;; bring five, six and seven back to life.
  ;; A MARKER THAT IS THERE AND WILL NOT READ STOPS ITS OWN REPLACEMENT.
  ;; The fork this writes is the LOWEST sequence anyone has disagreed at,
  ;; so it is computed from the marker already on disk -- and a read
  ;; failure that answered "no marker" made the new disagreement's
  ;; sequence the whole answer. The fork would then RISE, and every
  ;; record between the old fork and the new one comes back out of
  ;; quarantine: bytes the store had set aside as unattributable become
  ;; history again, on the strength of a failed read.
  ;;
  ;; ABSENT AND UNREADABLE ARE DIFFERENT ANSWERS, which is the same
  ;; distinction the uncertainty readers had to learn. Absent means there
  ;; is no earlier fork and `seq` is right; unreadable means the earlier
  ;; fork exists and is not known, and nothing may be written over it.
  (define (quarantine! store writer seq theirs ours)
    (let* ((path (writer-file store writer "quarantine.sexp"))
           ;; R1's read (F77b review 2): a quarantine.sexp that cannot be
           ;; read raises unreadable-entry naming it; one that reads and does
           ;; not parse keeps the assertion below, as on the base.
           (bytes (read-entry path))
           (existing
             (if (eq? bytes 'absent)
                 'absent
                 (let ((d (guard (e (#t 'unreadable))
                            (string->sexpr-extended
                              (utf8->string bytes)))))
                   (if (list? d)
                       (let ((f (alist-ref d 'fork)))
                         (if (integer? f) f 'unreadable))
                       'unreadable))))
           (fork (cond
                   ((eq? existing 'absent) seq)
                   ((eq? existing 'unreadable)
                    (assertion-violation
                      'quarantine!
                      "a fork marker that will not read may not be replaced"
                      (list writer path)))
                   (else (min existing seq)))))
      (atomic-write! path
                     (string->utf8
                       (string-append "((format 1) (fork " (number->string fork) ")"
                                      " (ours \"" ours "\") (theirs \"" theirs "\"))\n"))
                     'publish)
      (list 'divergence (list 'fork fork))))

  ;; KEPT, NOT INSTALLED. The name is content-addressed so a re-arrival
  ;; is recognised without re-reading; the marker beside it is an empty
  ;; file whose existence says the BYTES passed -- per-record CRC and
  ;; internal sequence continuity, which are properties of the bytes
  ;; alone. The splice, the divergence check and the publish decision are
  ;; redone every time, against the history as it is now.
  ;; The bytes are read back and hashed rather than compared by length:
  ;; a truncation is the likely damage and a length check would catch it,
  ;; but a file of the right length and the wrong content is the damage
  ;; that a length check reads as healthy.
  ;; -> #t, #f, or (unreadable <path> <reason>).
  ;; NEVER: A RETAINED CANDIDATE THAT CANNOT BE READ IS NOT ONE THAT IS NOT
  ;; THERE (U9, F77b). Read with a catch-all, it answered #f and the keep
  ;; wrote a new file and renamed it over the one it could not read --
  ;; replacing evidence nobody had looked at. The read is R1's: absence is
  ;; #f, and any other failure is answered, so the keep can refuse.
  (define (kept-content-ok? path sha)
    (let ((held (guard (e ((unreadable-entry? e)
                           (list 'unreadable (unreadable-entry-path e)
                                 (unreadable-entry-reason e))))
                  (read-entry path))))
      (cond ((eq? held 'absent) #f)
            ((pair? held) held)
            (else (string=? sha (segment-sha held))))))

  (define (keep-incoming! store writer segment bytes sha why)
    (let* ((dir (string-append (writer-directory store writer) "/incoming"))
           (kept (string-append dir "/" (segment-file-name segment) "." sha ".seg"))
           (marker (string-append dir "/" (segment-file-name segment) "." sha ".ok")))
      (ensure-directory! dir)
      ;; THE NAME IS A CLAIM ABOUT THE CONTENT AND IS NOT THE CONTENT. The
      ;; file is named by the hash of the bytes it should hold, so an
      ;; interrupted earlier keep leaves a file with exactly the right
      ;; name and the wrong bytes -- and answering `kept' for it points
      ;; the sender at evidence that is not its candidate. What the name
      ;; says is checked against what the file holds, every time.
      ;;
      ;; AND THE REPLACEMENT IS ATOMIC. Writing in place would make the
      ;; window wider rather than narrower: a keep interrupted while
      ;; overwriting leaves neither the old bytes nor the new ones. The
      ;; candidate is staged under a temporary name, flushed, and renamed
      ;; over -- so the name only ever appears with whole content.
      ;; EVERY READ BEFORE ANY WRITE (F77b review 1): the marker's status is
      ;; read here, not after the candidate has been replaced, so a marker
      ;; this store cannot read refuses before anything is staged.
      ;; THE MARKER'S MEANING IS ITS EXISTENCE, so it is asked with R1's
      ;; type question, not read (review 2): present in any form is present,
      ;; as the base's file-exists? said; only a stat that fails refuses.
      (let* ((marker-state (guard (e ((unreadable-entry? e)
                                      (list 'unreadable (unreadable-entry-path e)
                                            (unreadable-entry-reason e))))
                             (entry-type marker)))
             (status (if (pair? marker-state) marker-state (kept-content-ok? kept sha))))
        (if (pair? status)
            (list 'refused 'kept-unreadable
                  (list 'path (cadr status)) (list 'reason (caddr status)))
            (begin
              (unless status
                (let ((tmp (temp-name-for kept)))
                  (let ((fd (fd-open tmp '(write create))))
                    (dynamic-wind void
                      (lambda ()
                        (parameterize ((theourgia-stage 'publish))
                          (write-all! fd bytes tmp)
                          (fsync! fd tmp 'publish)))
                      (lambda () (guard (e (#t (void))) (fd-close fd)))))
                  (rename-over! tmp kept))
                ;; the candidate is durable, file and name, BEFORE the verdict
                (directory-entry-durable! kept 'publish))
              (when (eq? marker-state 'absent)
                (atomic-write! marker (make-bytevector 0) 'publish))
              (list why (list 'kept kept) (list 'writer writer) (list 'segment segment)))))))

  ;; ---- uncertain intervals (section 7.3) ------------------------------------

  ;; A STRETCH OF A WRITER'S HISTORY THE STORE CANNOT VOUCH FOR, written
  ;; as `(writer low high)` with high = #f meaning it runs on without a
  ;; bound. `low` is the last position the store is still sure about, so
  ;; the stretch is the positions ABOVE it.
  ;;
  ;; `uncertain.sexp` IS A CACHE AND THE RECORDS ARE THE AUTHORITY. It is
  ;; written so that a reader need not re-derive the set on every call,
  ;; and it is read for nothing that is not also derivable -- which is
  ;; exactly what makes deleting it a recoverable event rather than a
  ;; loss of history.
  ;;
  ;; THE RECONCILIATION IS A UNION, AND IT IS NOT SYMMETRIC. An entry the
  ;; records imply and the cache lacks is the cache being wrong, and the
  ;; records win. An entry the cache holds and the records no longer
  ;; imply is the ordinary result of a repair: the torn tail was mended,
  ;; the divergence was verified, and the records are clean again --
  ;; while the fact that the store was once unsure THERE is exactly what
  ;; must not be forgotten, because a request whose possible positions
  ;; touch that stretch still cannot be told apart from one that ran. So
  ;; entries are added and never removed, and a cache that is a strict
  ;; superset of the derivation is not a disagreement at all.
  (define (uncertain-path store writer)
    (writer-file store writer "uncertain.sexp"))

  ;; WHAT THE RECORDS THEMSELVES SAY. Three sources are visible from a
  ;; writer's own files: a divergence, whose fork disowns everything from
  ;; that sequence up; a torn tail, above the last sequence that reads;
  ;; and a writer this store cannot read the beginning of at all, whose
  ;; whole domain is in doubt.
  ;;
  ;; The two adopt-time kinds -- the rollback interval and the
  ;; identity-mismatch interval -- are not RECOMPUTED here. Their
  ;; coordinates exist only at the moment an adopt holds them, and
  ;; working them out a second time from a store that has since moved on
  ;; would be a second supplier of a number only one place can compute.
  ;; They are read back from the retirement record, which is where adopt
  ;; put them, before the successor's owner existed. That is what makes
  ;; the cache a cache: every kind in it is now derivable from the
  ;; records, so deleting it costs a re-derivation and not a fact.
  ;;
  ;; THE RECORD IS CONSULTED EVEN WHEN THE PREFIX WILL NOT READ. A
  ;; writer whose beginning this store cannot read is already wholly in
  ;; doubt, and the adopt entry adds nothing to `(writer 0 #f)` -- but a
  ;; record naming a stretch is a fact about the writer either way, and
  ;; a derivation that returned early would make the two arms disagree
  ;; about what the store knows.
  ;; DISCOVERY FIRST. For a writer whose discovery is `unreadable` the
  ;; answer is the conservative (writer 0 #f) and nothing else is read: its
  ;; retirement record sits in the directory that could not be read, and
  ;; its coordinates are not there to ask.
  (define (uncertain-derived store writer)
    (let ((p (guard (e (#t #f)) (discover-prefix store writer 'held-exclusive))))
      (if (and p (eq? (discovery-origin p) 'unreadable))
          (list (list writer 0 #f))
          (uncertain-derived-readable store writer p))))

  (define (uncertain-derived-readable store writer p)
    (let ((recorded (retired-uncertain store writer)))
      (cond
        ((not p) (append recorded (list (list writer 0 #f))))
        ((eq? (discovery-origin p) 'incomplete-publication)
         (append recorded (list (list writer 0 #f))))
        (else
         (append
           recorded
           (let ((q (discovery-quarantine p)))
             (if (and q (pair? (cdr q)) (integer? (cadr q)) (exact? (cadr q)))
                 (list (list writer (- (cadr q) 1) #f))
                 '()))
           (if (discovery-torn p)
               (list (list writer (discovery-end-seq p) #f))
               '()))))))

  (define (uncertain-interval? x)
    (and (list? x) (= 3 (length x))
         (string? (car x))
         (integer? (cadr x)) (exact? (cadr x)) (>= (cadr x) 0)
         (or (not (caddr x))
             (and (integer? (caddr x)) (exact? (caddr x)) (>= (caddr x) (cadr x))))))

  ;; #f when there is no cache, `unreadable` when there is one and it is
  ;; not a list of intervals, otherwise the list. A file that parses into
  ;; something of the wrong shape is unreadable and not empty: an empty
  ;; list is a positive claim that nothing is uncertain.
  ;; -> #f, the symbol `unreadable`, or a list of intervals: a strict
  ;; contract, because its persistence callers write the list into a
  ;; retirement record, which must never hold a derived fallback.
  ;; `unreadable` is answered when the cache cannot be read AND when the
  ;; writer's discovery is `unreadable` -- a cache file that is not there
  ;; says nothing about a writer whose directory could not be read. The
  ;; path and the reason are not lost behind the bare symbol: they go to
  ;; the trace, as (uncertain-cache-unreadable (<path> . <reason>)).
  (define (uncertain-cached store writer)
    (let ((path (uncertain-path store writer)))
      (guard (e ((unreadable-entry? e)
                 (trace-event! 'uncertain-cache-unreadable
                               (cons (unreadable-entry-path e) (unreadable-entry-reason e)) #f)
                 'unreadable))
        (let ((p (discover-prefix store writer 'held-exclusive)))
          (if (eq? (discovery-origin p) 'unreadable)
              (let ((detail (log-error-detail (unreadable-note p))))
                (trace-event! 'uncertain-cache-unreadable
                              (cons (cdr (assq 'path detail)) (cdr (assq 'reason detail))) #f)
                'unreadable)
              (let ((bytes (read-entry path)))
                (and (not (eq? bytes 'absent))
                     (let ((d (guard (e (#t 'unreadable))
                                (string->sexpr-extended (utf8->string bytes)))))
                       (if (and (list? d) (for-all uncertain-interval? d)) d 'unreadable)))))))))

  ;; `(<intervals> <integrity or #f>)`. The intervals are what a caller
  ;; must test against; the integrity is what the store must report.
  ;;
  ;; AN ABSENT CACHE IS ONLY A PROBLEM WHEN THERE IS SOMETHING TO CACHE.
  ;; A writer with nothing uncertain about it has no file and needs
  ;; none, and reporting that as an integrity kind would make the
  ;; ordinary state of every healthy writer look like damage.
  (define (uncertain-load store writer)
    (let ((derived (uncertain-derived store writer))
          (cached (uncertain-cached store writer)))
      (cond
        ((not cached)
         (if (null? derived)
             (list '() #f)
             (list derived (list 'uncertain-cache 'absent))))
        ((eq? cached 'unreadable)
         (list derived (list 'uncertain-cache 'unreadable)))
        (else
         (let ((missing (filter (lambda (i) (not (member i cached))) derived)))
           (if (null? missing)
               (list cached #f)
               (list (append cached missing)
                     (list 'uncertain-cache 'stale missing))))))))

  (define (uncertain-write! store writer intervals stage)
    (unless (for-all uncertain-interval? intervals)
      (assertion-violation 'uncertain-write!
                           "every entry must be (writer low high)" intervals))
    (atomic-write! (uncertain-path store writer)
                   (string->utf8
                     (string-append (sexpr->string-extended intervals) "\n"))
                   stage))

  ;; ---- the recovery barrier (section 7.3) ----------------------------------

  ;; ONE TABLE, TWO CALLERS. Confirming a first execution and answering a
  ;; replay make the same promise -- that what the answer describes will
  ;; still be there after a power cut -- so they flush the same things in
  ;; the same order. Two tables would drift, and the drift would show up
  ;; only as a replay that promised more than the original execution had.
  ;;
  ;; THE ORDER IS THE RECOVERY DEPENDENCY CLOSURE, read outwards from the
  ;; record: the file holding it, the directory that names that file, the
  ;; manifest that admits the file to the writer's history, the identity
  ;; files of every generation on the path to it, the uncertain intervals
  ;; that say which parts of that path can be trusted, and the registry
  ;; that says this machine holds the store at all. Each is a premise of
  ;; the one before it: a record whose file is durable but whose
  ;; directory entry is not is a record no reader will find.
  ;;
  ;; THE ORDER IS DOCUMENTATION, NOT A PROTOCOL. It records which row is
  ;; a premise of which, and that is all: fsync establishes that an
  ;; operation has completed, it does not hold other writes back, so the
  ;; names here reach the disk in whatever order the filesystem chooses
  ;; and two of these rows share a directory anyway. Recovery ordering is
  ;; established where the mutations happen -- the candidate is durable
  ;; before it is linked, its entry before the manifest names it. This
  ;; barrier is the completion point that reasserts those obligations
  ;; before the answer is given, not a second attempt to sequence them.
  ;;
  ;; IT IS A LIST BEFORE IT IS AN ACTION. A caller can ask what the
  ;; barrier covers without performing it, which is how a case checks
  ;; that the table names what it should rather than that something was
  ;; flushed.
  (define (barrier-artefacts store writer segment)
    (let* ((dir (writer-directory store writer))
           (candidates
             (append
               (list (cons 'log-file
                           (string-append dir "/" (segment-file-name segment)))
                     (cons 'writer-directory dir)
                     (cons 'manifest (manifest-path store writer)))
               (apply append
                      (map (lambda (w)
                             (list (cons 'owner (writer-file store w "owner.sexp"))
                                   (cons 'retired (writer-file store w "retired.sexp"))))
                           (store-writers store)))
               (list (cons 'instance (string-append store "/instance.sexp"))
                     (cons 'uncertain (writer-file store writer "uncertain.sexp"))
                     (cons 'registry (registry-path))))))
      ;; THE TABLE IS WHAT IS THERE. A store with no retirement record
      ;; has nothing to promise about one, and listing the path anyway
      ;; would put a row in the table that no fault can be armed at.
      ;; AN ARTEFACT THAT CANNOT BE STAT'ED IS LEFT OUT, as the native
      ;; presence test left it out (F100a, the R2g rule's third site): a
      ;; mirror whose directory cannot be searched put its owner.sexp and
      ;; retired.sexp in every commit's barrier, and a raise here answered
      ;; every write beside it `unknown` after it had committed (measured:
      ;; F100a suite-1, U2c). What a barrier does with an unreadable
      ;; artefact it must touch is F100d's (design D2).
      (filter (lambda (entry) (present-or-unreadable-skip? (cdr entry))) candidates)))

  ;; THE ROWS THE ANSWER DEPENDS ON, named rather than assumed. Absence
  ;; and success are the same silence: flush-file! does nothing to a path
  ;; that is not there, which is right for every optional row -- a store
  ;; with no retirement record has nothing to promise about one -- and
  ;; wrong for these two. If the segment's own file or the directory
  ;; naming it is gone the barrier promises nothing, a fault armed at it
  ;; never fires, and the case reads green because nothing happened.
  ;;
  ;; The rest are premises of the answer where they exist and silent
  ;; where they do not; a store that has never had a local writer has no
  ;; instance file, and requiring one would refuse publishes to exactly
  ;; the stores that only ever receive.
  ;; WHICH ROWS ARE REQUIRED DEPENDS ON WHAT THE ANSWER CLAIMS, and the
  ;; two claims are not the same. A publish answer says the segment is
  ;; listed in the manifest, so an absent manifest is the violation of
  ;; the very invariant that gives the word its meaning. A commit answer
  ;; says a record is in this writer's own directory, where nothing
  ;; lists anything -- a local writer HAS no manifest, and requiring one
  ;; would refuse every write this store makes on its own behalf.
  ;;
  ;; ONE TABLE, KEYED BY THE CLAIM. Two constants would drift the first
  ;; time a row moved between them.
  (define barrier-required-table
    '((publish . (log-file writer-directory manifest))
      (commit . (log-file writer-directory))))

  (define (barrier-required claim)
    (let ((e (assq claim barrier-required-table)))
      (unless e
        (assertion-violation 'barrier-required "no such claim" claim))
      (cdr e)))

  ;; PERFORMED IN ORDER, UNDER THE STORE LOCK, and it answers with the
  ;; table it performed, so a caller reporting a replay can say what it
  ;; made durable rather than assert that it did.
  (define (run-barrier! store writer segment stage claim)
    (let ((table (barrier-artefacts store writer segment))
          (required (barrier-required claim)))
      (for-each
        (lambda (kind)
          (unless (assq kind table)
            (assertion-violation 'run-barrier! "required artefact absent" kind)))
        required)
      (for-each
        (lambda (entry)
          (let ((kind (car entry)) (path (cdr entry)))
            (if (eq? kind 'writer-directory)
                (begin
                  (fsync-dir! path stage)
                  ;; AND THE DIRECTORY'S OWN NAME. `writers/<w>` is
                  ;; created once, by `ensure-writer-directory!`, which
                  ;; flushes `writers/` only on the call that creates it
                  ;; -- so a first publish whose mkdir succeeded and
                  ;; whose flush did not leaves the directory present,
                  ;; its name not durable, and every retry skipping the
                  ;; flush because the directory is already there.
                  ;; Nothing else would ever repair it.
                  (directory-entry-durable! path stage))
                (begin
                  ;; THE REQUIRED ROWS ARE OPENED, NOT ASKED ABOUT. The
                  ;; table was filtered by existence a moment ago, and
                  ;; for a row the answer depends on, "it went away in
                  ;; between" must not read as "there was nothing to
                  ;; do".
                  (if (memq kind required)
                      (flush-existing! path stage)
                      (flush-file! path stage))
                  ;; the name as well as the contents: a file durable
                  ;; under a directory entry that is not is a file that
                  ;; can vanish whole.
                  (directory-entry-durable! path stage)))))
        table)
      table))

;; ---- log-publish! (section 9.6) --------------------------------------------

  (define (log-publish! store writer segment bytes sha)
    (claim-store! store 'publish)
    (let ((lock ((current-lock-acquire) (string-append store "/lock") 'exclusive)))
      (let ((answer (guard (e (#t ((current-lock-release) lock) (release-store! store) (raise e)))
                      (publish-locked! store writer segment bytes sha))))
        ((current-lock-release) lock)
        (release-store! store)
        answer)))

  ;; A WRITER THIS STORE HAS NEVER SEEN ARRIVES AS ITS FIRST CANDIDATE,
  ;; and until now the first thing that touched it was `fd-open` with
  ;; `create` -- which creates a file and not the directory above it, so
  ;; the first publish for a new writer failed on an open rather than
  ;; answering. The directory is made here, before anything is staged
  ;; into it, and the entry for it in `writers/` is made durable: a
  ;; segment whose directory is not durable is a segment a crash can
  ;; strand where no reader will look for it.
  (define (ensure-writer-directory! store writer)
    (let ((dir (writer-directory store writer)))
      (unless (file-is-directory? dir)
        (mkdir-p! dir)
        (directory-entry-durable! dir 'publish))))

  ;; #f when the manifest reads, or the detail of why it does not.
  ;; A MANIFEST THAT CANNOT BE READ answers with the path that failed and the
  ;; system's words for it, whichever form the failure took: a log-error
  ;; for a manifest that reads and will not parse, unreadable-entry for one
  ;; that will not read.
  (define (manifest-unreadable store writer)
    (guard (e ((unreadable-entry? e)
               (list (list 'path (unreadable-entry-path e))
                     (list 'reason (unreadable-entry-reason e))))
              ((log-error? e)
               (let ((detail (log-error-detail e)))
                 (list (list 'path (cdr (assq 'path detail)))
                       (list 'reason (let ((r (assq 'reason detail)))
                                       (if r (cdr r) "unreadable"))))))
              (#t (list (list 'path (manifest-path store writer))
                        (list 'reason "unreadable"))))
      (read-manifest store writer)
      #f))

  ;; A NAME NO READER WILL EVER LOOK AT IS NOT A WRITER. Writer ids are
  ;; eight characters of [0-9a-z] and `store-writers` filters the
  ;; directory by exactly that, so a segment published under any other
  ;; name is durable, listed in a manifest, and delivered to nothing --
  ;; while the sender is told `published` and deletes its own copy on the
  ;; strength of it. That is the inverse of "answering implies durable":
  ;; durable, and unreachable.
  ;;
  ;; It is asked before anything exists: no directory, no segment, no
  ;; manifest entry. And it asks the same predicate the reader uses,
  ;; because a second opinion about what a writer id is would put the two
  ;; back where they started.
  ;; THE FOUR OUTCOMES THAT LEAVE THE SEGMENT PUBLISHED, in one place.
  ;; The sender deletes its own copy on the strength of any of them, so
  ;; the set that means "durable" and the set that exits zero have to be
  ;; the same set; two lists of four symbols would agree until one of
  ;; them gained a fifth.
  (define (publish-durable? answer)
    (and (pair? answer)
         (memq (car answer) '(published idempotent repaired extended))
         #t))

  ;; THE BARRIER IS THE LAST THING BEFORE THE WORD. Every durable answer
  ;; passes it, the first execution and the replay alike: `idempotent` is
  ;; the replay of a publish, and it promises exactly what `published`
  ;; promised. Putting it here rather than in each branch is what makes
  ;; that true by construction -- a branch added later gets the barrier
  ;; because it returns one of the four, not because whoever wrote it
  ;; remembered.
  ;;
  ;; A refusal does not pass it. Nothing it describes is claimed to
  ;; survive a crash, and flushing on the way out of a refusal would put
  ;; the store's whole recovery closure on the path of every rejected
  ;; candidate.
  (define (publish-locked! store writer segment bytes sha)
    (if (not (writer-id? writer))
        (list 'refused 'invalid-writer (list 'writer writer))
        (let ((answer (publish-validated! store writer segment bytes sha)))
          (when (publish-durable? answer)
            (run-barrier! store writer segment 'publish 'publish))
          answer)))

  ;; THE FIRST RECORD WHOSE ENVELOPE IS REFUSED, as (reason . offset), or
  ;; #f. Only records that decode are asked: one that does not (a bad
  ;; CRC, a torn tail) keeps the handling it has, so a candidate holding a
  ;; bad-CRC record and a later envelope-bad one is answered here, at the
  ;; later one. A record from `segment-records` keeps only its seq, so its
  ;; line is decoded again to put the whole envelope to the shared rule.
  (define (first-envelope-refusal rs)
    (let loop ((rs rs))
      (if (null? rs)
          #f
          (let* ((rec (car rs))
                 (reason (and (rec-ok? rec)
                              (record-envelope-refusal (decode-line (cadddr rec))))))
            (if reason
                (cons reason (cadr rec))
                (loop (cdr rs)))))))

  (define (publish-validated! store writer segment bytes sha)
    (ensure-writer-directory! store writer)
    (let* ((dir (writer-directory store writer))
           (target (string-append dir "/" (segment-file-name segment)))
           ;; R1's read (F77b): a local copy this store cannot read raises
           ;; unreadable-entry naming it, which every route answers by
           ;; name; it is not a missing copy, and not an internal error.
           (local-bytes (let ((b (read-entry target))) (and (not (eq? b 'absent)) b)))
           (local-rs (if local-bytes (segment-records local-bytes) '()))
           (cand-rs (segment-records bytes))
           (history (writer-history store writer)))
      (cond
        ;; A MANIFEST THIS STORE CANNOT READ STOPS THE PUBLISH BEFORE IT
        ;; BEGINS. Every decision below is made from the manifest -- which
        ;; segments exist, what range each holds, whether this candidate
        ;; is already published -- so a publish that could not read it
        ;; would be deciding from an absence it mistook for an emptiness.
        ;; It refuses in the answer rather than by raising, because the
        ;; caller is a sync client with a candidate in hand and needs to
        ;; be told to come back, not handed an exception.
        ;;
        ;; Nothing is written: the candidate does not go to incoming/
        ;; either, because keeping evidence against a writer whose
        ;; manifest is unreadable adds a file to a directory nobody can
        ;; currently reason about.
        ((manifest-unreadable store writer)
         => (lambda (why) (cons 'refused (cons 'manifest-unreadable why))))
        ;; STEP 0, ABOVE EVERY OTHER RULE. An active local writer's
        ;; current segment is never replaced by sync.
        ((active-current-segment? store writer segment)
         (list 'refused 'active-writer-segment))
        ;; STEP 1. The caller's sha is checked here: a segment whose
        ;; records all pass CRC but whose sha is wrong would install
        ;; cleanly and be rejected by the next reader that reads the
        ;; manifest.
        ((not (string=? sha (segment-sha bytes)))
         (list 'error 'invalid-candidate 'sha-mismatch))
        ;; A RECORD THE READER WOULD REFUSE IS NOT PUBLISHED. `published`
        ;; and `idempotent` tell the sender to delete its copy, so neither
        ;; may be said of bytes this store's own reader will not read past
        ;; -- including bytes it already holds. The row comes before the
        ;; sequence is compared, because comparing a seq that is not a
        ;; number raises. It answers for the first refused record by its
        ;; byte offset in the candidate, and names no path.
        ((first-envelope-refusal cand-rs)
         => (lambda (refused)
              (list 'error 'invalid-candidate (car refused)
                    (list 'offset (cdr refused)))))
        ((not (records-contiguous? cand-rs))
         (list 'error 'invalid-candidate 'not-contiguous))
        ;; THE LAYOUT GATE, before the validate/invalid split: several
        ;; rows below read "shorter" and "longer" as prefix relations.
        ((layout-conflict local-rs cand-rs (and local-bytes #t))
         => (lambda (why)
              (keep-incoming! store writer segment bytes sha
                              (list 'segment-layout-conflict why))))
        ;; DIVERGENCE IS LOOKED FOR ACROSS THE WHOLE HISTORY, not just
        ;; this file: a disagreement can fall in a neighbouring segment.
        ((first-divergence history cand-rs)
         => (lambda (seq)
              (quarantine! store writer seq sha
                           (if local-bytes (segment-sha local-bytes) ""))))
        ;; AND THE RESULT HAS TO READ. Every record identical still is
        ;; not enough if installing leaves a later segment unreachable.
        ((layout-read-problem store writer segment cand-rs)
         => (lambda (why)
              (keep-incoming! store writer segment bytes sha
                              (list 'segment-layout-conflict why))))
        (else (publish-dispatch store writer segment bytes sha
                                local-bytes local-rs cand-rs)))))

  (define (publish-dispatch store writer segment bytes sha local-bytes local-rs cand-rs)
    (let* ((local-valid (valid-records local-rs))
           (local-whole (and local-bytes
                             (= (length local-valid) (length local-rs))
                             (records-contiguous? local-rs)))
           (lr (seq-range local-rs))
           (cr (seq-range cand-rs)))
      (cond
        ;; A: the local copy validates, or there is none
        ((not local-bytes) (do-install store writer segment bytes sha))
        (local-whole
         (cond
           ;; IDEMPOTENT MEANS FINISHED, and finished includes being in
           ;; the manifest with this hash. A killed install leaves bytes
           ;; that match and no manifest entry; answering idempotent
           ;; there would make "done" mean two different things.
           ;; IDEMPOTENT IS A PROMISE THE CALLER ACTS ON. A sync client
           ;; that is told the segment is already published deletes its
           ;; own copy on the strength of it, so "listed" has to mean
           ;; "listed durably" -- and the entry may have been written by
           ;; a process that died before flushing it. That is not done
           ;; here: this branch returns one of the four durable answers,
           ;; and `publish-locked!` runs the barrier over the manifest,
           ;; the segment and the entries naming them before the word
           ;; leaves the store.
           ((and (equal? lr cr) (published-with? store writer segment sha))
            (list 'idempotent segment))
           ((equal? lr cr) (do-install-over store writer segment bytes sha 'published))
           ((< (cdr cr) (cdr lr)) (list 'incomplete segment))
           ((retired-prefix-segment? store writer segment)
            (do-extend store writer segment bytes sha cand-rs))
           (else
            (keep-incoming! store writer segment bytes sha 'newer-history-unmergeable))))
        ;; B: the local copy fails validation
        (else
         (let ((missing (uncovered-record local-valid cand-rs)))
           (cond
             ;; ABSENCE IS NOT EVIDENCE OF DISAGREEMENT. Local records 1
             ;; and 3 against a candidate of 1 and 2 would destroy 3.
             (missing (list 'refused 'insufficient-coverage (list 'seq missing)))
             ;; B1 WINS OVER A4b: a damaged local copy that the candidate
             ;; covers is repaired even where the target is sealed.
             (else (do-repair store writer segment bytes sha cand-rs))))))))

  (define (uncovered-record local-valid cand-rs)
    (let loop ((xs local-valid))
      (cond
        ((null? xs) #f)
        ((not (find-record cand-rs (rec-seq (car xs)))) (rec-seq (car xs)))
        (else (loop (cdr xs))))))

  (define (published-with? store writer segment sha)
    (let ((m (read-manifest store writer)))
      (and m (equal? (manifest-hash m segment) sha))))

  (define (do-install store writer segment bytes sha)
    (let ((tmp (stage-candidate! store writer bytes 'publish)))
      (install-segment! store writer segment tmp sha bytes)))

  ;; The bytes are already there but the manifest is not: a killed
  ;; install or a killed repair. Finish it rather than call it done.
  (define (do-install-over store writer segment bytes sha kind)
    (add-to-manifest! store writer segment sha bytes)
    (list kind segment))

  (define (do-extend store writer segment bytes sha cand-rs)
    (let ((bad (retirement-coordinates-conflict store writer segment cand-rs)))
      (if bad
          (list 'refused 'retirement-coordinates bad)
          (let ((tmp (stage-candidate! store writer bytes 'publish)))
            (overwrite-segment! store writer segment tmp sha 'extended bytes)))))

  (define (do-repair store writer segment bytes sha cand-rs)
    (let ((bad (retirement-coordinates-conflict store writer segment cand-rs)))
      (if bad
          (list 'refused 'retirement-coordinates bad)
          (let ((tmp (stage-candidate! store writer bytes 'publish)))
            (overwrite-segment! store writer segment tmp sha 'repaired bytes)))))

  ;; RETIREMENT DECLARES WHERE A SEQUENCE ENDS. A replacement that
  ;; changes any earlier record's length moves that offset, and the
  ;; declaration silently stops being true -- so it is recomputed from
  ;; the candidate before anything is renamed.
  (define (retirement-coordinates-conflict store writer segment cand-rs)
    (let ((r (retired-of store writer)))
      (and r
           (not (eq? (car r) 'malformed))
           (eqv? (car r) segment)
           (let* ((seq (caddr r))
                  (declared-off (cadr r))
                  (rec (find-record cand-rs seq)))
             (cond
               ((not rec) (list 'sequence-absent seq))
               ((not (= (rec-end rec) declared-off))
                (list 'offset-moved (list 'declared declared-off)
                      (list 'candidate (rec-end rec))))
               (else #f))))))

  (define (retired-prefix-segment? store writer segment)
    (let ((r (retired-of store writer)))
      (and r (not (eq? (car r) 'malformed)) (eqv? (car r) segment))))

  ;; A LOCAL WRITER WHOSE CURRENT SEGMENT CANNOT BE SIZED MAY BE WRITING
  ;; ANY OF ITS SEGMENTS (K12): no-append-target here means "which one is
  ;; current is not known", not "none is", and a repair that replaced the
  ;; live segment is the thing this check exists to refuse.
  (define (active-current-segment? store writer segment)
    (and (entry-present? (writer-file store writer "owner.sexp"))
         (not (retired-of store writer))
         (let ((p (guard (e (#t #f)) (discover-prefix store writer 'held-exclusive))))
           (and p
                (let ((phys (discovery-physical-current p)))
                  (or (eq? phys 'no-append-target)
                      (and (pair? phys) (eqv? (car phys) segment))))))))

;; ---- generations (section 4.1) ---------------------------------------------

  ;; TWO KINDS OF RECORD IN ONE FILE, EACH SELF-DESCRIBING. A water mark
  ;; is `(store instance writer seq state)`; a generation leads with the
  ;; symbol `gen`. One machine lock then covers both, and "only ever
  ;; increases" is a rule about the water marks alone -- while a reader
  ;; tells the two apart by the tag rather than by counting slots, so
  ;; adding a field later cannot silently reinterpret an old file.
  (define (gen-record? e)
    (and (list? e) (= 9 (length e)) (eq? (car e) 'gen)))
  (define (gen-store e) (list-ref e 1))
  (define (gen-instance e) (list-ref e 2))
  (define (gen-tx e) (list-ref e 3))
  (define (gen-old e) (list-ref e 4))
  (define (gen-new e) (list-ref e 5))
  (define (gen-lost-from e) (list-ref e 6))
  (define (gen-lost-to e) (list-ref e 7))
  (define (gen-state e) (list-ref e 8))

  (define (generations-of reg store-id instance)
    (filter (lambda (e)
              (and (gen-record? e)
                   (equal? (gen-store e) store-id)
                   (equal? (gen-instance e) instance)))
            reg))

  (define (generation-with-tx reg tx)
    (let loop ((es reg))
      (cond ((null? es) #f)
            ((and (gen-record? (car es)) (equal? (gen-tx (car es)) tx)) (car es))
            (else (loop (cdr es))))))

  (define (make-gen store-id instance tx old new lost-from lost-to state)
    (list 'gen store-id instance tx old new lost-from lost-to state))

  (define (registry-put-generation reg entry)
    (let ((found (vector #f)))
      (let ((updated (map (lambda (e)
                            (if (and (gen-record? e) (equal? (gen-tx e) (gen-tx entry)))
                                (begin (vector-set! found 0 #t) entry)
                                e))
                          reg)))
        (if (vector-ref found 0) updated (append updated (list entry))))))

  (define (write-registry! reg)
    (trace-event! 'registry-write (registry-path) #f)
    (atomic-write! (registry-path)
                   (string->utf8 (string-append (sexpr->string-extended reg) "\n"))
                   'registry))

  ;; ---- the chain, checked wherever a write is authorised --------------------

  ;; NOT ONLY AT OPEN. A handle opened before somebody else adopted still
  ;; sees its own writer as healthy -- identity matches, owner matches,
  ;; no retirement marker, water mark where it left it -- so it reserves
  ;; the next sequence and writes. Checking at open leaves a gap exactly
  ;; as wide as a long-lived handle. This runs in the same machine-lock
  ;; section that re-checks the water mark, for the same reason that
  ;; check is there: what was true at open is not what authorises a
  ;; write.
  (define (generation-chain-ok? store reg store-id instance)
    (let loop ((gs (generations-of reg store-id instance)))
      (cond
        ((null? gs) #t)
        ((not (eq? (gen-state (car gs)) 'active)) (loop (cdr gs)))
        ;; an active generation the store no longer carries: it was
        ;; rolled back underneath us
        ((not (generation-present? store (car gs))) #f)
        (else (loop (cdr gs))))))

  (define (generation-present? store g)
    (let ((r (retired-of store (gen-old g))))
      (and r
           (not (eq? (car r) 'malformed))
           (equal? (list-ref r 3) (gen-tx g))
           (entry-present? (writer-file store (gen-new g) "owner.sexp")))))

;; ---- the adopt transaction (section 4.1) ----------------------------------

  ;; FOUR STEPS, EACH ONE atomic-write!, ALL INSIDE THE STORE'S EXCLUSIVE
  ;; LOCK. Steps 2 and 4 take the machine lock as well, in that order --
  ;; store then machine, everywhere.
  ;;
  ;;   1 retirement-prepared   writers/<old>/retired.sexp, tx, prefix, no successor
  ;;   2 generation-reserved   registry (gen ... pending)
  ;;   3a owner-installed      writers/<new>/owner.sexp, tx, predecessors
  ;;   3b successor-backfilled old's retired.sexp gains (successor <new>)
  ;;   4 transition-complete   registry entry becomes active
  ;;
  ;; A NEW WRITER MAY NOT WRITE UNTIL STEP 4 IS DURABLE. That is what
  ;; makes cancelling a pending generation safe: it cannot orphan a
  ;; committed record, because a pending generation was never authorised
  ;; to commit one.

  (define (new-transaction-id)
    (string-append "tx-" (number->string (process-id))
                   "-" (number->string (wall-clock-ms))
                   "-" (bytevector->hex
                         (sha256 (string->utf8
                                   (string-append (machine-id)
                                                  (number->string (wall-clock-ms))))))))

  ;; FORMAT 2 ALWAYS CARRIES THE STRETCHES, EVEN WHEN THERE ARE NONE,
  ;; and that is the whole point of the version number. Writing the
  ;; clause only when it is non-empty makes "no clause" mean two
  ;; opposite things -- this adopt lost nothing, and this record was
  ;; written before anyone thought to record it -- and every retirement
  ;; record on disk today is the second kind. A reader that cannot tell
  ;; them apart has to guess, and both guesses are wrong somewhere: read
  ;; as "nothing lost" it repeats the defect this version exists to
  ;; close, read as "everything in doubt" it condemns every healthy
  ;; store that ever adopted. The version answers instead of guessing.
  (define (retired-text tx seg off seq successor lost uncertain)
    (string-append
      "((format 2) (tx \"" tx "\")"
      " (prefix " (number->string seg) " " (number->string off) " " (number->string seq) ")"
      " (uncertain " (sexpr->string-extended uncertain) ")"
      (if successor (string-append " (successor \"" successor "\")") "")
      (if lost (string-append " (lost " (sexpr->string-extended lost) ")") "")
      ")\n"))

  ;; AMENDING THE RECORD REPLACES ONE CLAUSE AND KEEPS THE REST. The
  ;; rewrite this replaces rebuilt the record out of the fields the
  ;; caller remembered to pass, which drops every clause it does not
  ;; name -- silently, and while reporting success. That was survivable
  ;; while the only unnamed clause was one nothing ever wrote; it is not
  ;; survivable now that the record carries a stretch of history no
  ;; other file can supply.
  (define (retired-put-clause! store writer . clauses)
    (let* ((p (writer-file store writer "retired.sexp"))
           (d (and (entry-present? p)
                   (guard (e (#t #f))
                     (string->sexpr-extended (utf8->string (read-whole p)))))))
      (unless (list? d)
        (assertion-violation 'retired-put-clause!
                             "no readable retirement record to amend" writer))
      (atomic-write!
        p
        (string->utf8
          (string-append
            (sexpr->string-extended
              (append (filter (lambda (x)
                                (not (and (pair? x)
                                          (exists (lambda (c) (eq? (car x) (car c)))
                                                  clauses))))
                              d)
                      clauses))
            "\n"))
        'registry)))

  ;; THE ADOPT-TIME STRETCHES AS THE RECORD HOLDS THEM, read through the
  ;; same shape test the cache is read through and narrowed to entries
  ;; about this writer -- the record sits in this writer's directory, so
  ;; an entry naming another one could only have been put there by hand.
  ;; A record that cannot be read answers with no entries rather than
  ;; raising: its unreadability is already an integrity kind of its own,
  ;; reported where the chain is walked, and raising here would replace
  ;; that report with a failure at a different address.
  ;; WHAT A RECORD WITHOUT THE CLAUSE MEANS, decided by its version and
  ;; not by hope. A format 2 record always carries it, so its absence
  ;; there is a record somebody edited and the safe reading is the wide
  ;; one. A format 1 record predates the clause entirely: it may be the
  ;; retirement of a rollback that lost a stretch nobody wrote down, and
  ;; nothing on disk can now say whether it was. So everything above its
  ;; prefix stays in doubt -- open, because there is no top to name --
  ;; which is the same answer this file gives to every other writer it
  ;; cannot vouch for.
  ;;
  ;; IT COSTS AN UPGRADED STORE SOME `unknown` ANSWERS IT MAY NOT NEED,
  ;; and that is the right way round: the other reading hands back `ok`
  ;; for a request that may already have run.
  (define (retired-legacy-uncertain writer d)
    (let loop ((xs (if (list? d) d '())))
      (cond
        ((null? xs) (list (list writer 0 #f)))
        ((and (list? (car xs)) (= 4 (length (car xs)))
              (eq? (caar xs) 'prefix)
              (for-all (lambda (v) (and (integer? v) (exact? v))) (cdr (car xs))))
         (list (list writer (list-ref (car xs) 3) #f)))
        (else (loop (cdr xs))))))

  (define (retired-format d)
    (let loop ((xs (if (list? d) d '())))
      (cond
        ((null? xs) 1)
        ((and (list? (car xs)) (= 2 (length (car xs)))
              (eq? (caar xs) 'format)
              (integer? (cadr (car xs))))
         (cadr (car xs)))
        (else (loop (cdr xs))))))

  ;; THE SAME QUESTION ASKED BY SOMETHING ABOUT TO WRITE THE ANSWER
  ;; DOWN. A reader that cannot read the record answers the widest thing
  ;; it could mean, which is right for deciding what to vouch for and
  ;; wrong for deciding what to persist: written down, that guess becomes
  ;; the record, and no later successful read can take it back. So the
  ;; mutation path gets a reader that says `unreadable` instead of
  ;; guessing, and refuses to rewrite a record it could not read.
  (define (retired-uncertain-strict store writer)
    (let* ((p (writer-file store writer "retired.sexp"))
           (bytes (guard (e ((unreadable-entry? e) 'unreadable)) (read-entry p))))
      (if (eq? bytes 'absent)
          '()
          (let ((d (if (eq? bytes 'unreadable)
                       'unreadable
                       (guard (e (#t 'unreadable))
                         (string->sexpr-extended (utf8->string bytes))))))
            (cond
              ((eq? d 'unreadable) 'unreadable)
              ((not (list? d)) 'unreadable)
              (else
               (let loop ((xs d))
                 (cond
                   ((null? xs) (retired-legacy-uncertain writer d))
                   ((and (list? (car xs)) (= 2 (length (car xs)))
                         (eq? (caar xs) 'uncertain)
                         (list? (cadr (car xs))))
                    (if (>= (retired-format d) 2)
                        (filter (lambda (i)
                                  (and (uncertain-interval? i)
                                       (string=? (car i) writer)))
                                (cadr (car xs)))
                        (append
                          (filter (lambda (i)
                                    (and (uncertain-interval? i)
                                         (string=? (car i) writer)))
                                  (cadr (car xs)))
                          (retired-legacy-uncertain writer d))))
                   (else (loop (cdr xs)))))))))))

  ;; A RECORD THAT CANNOT BE READ IS NOT A MALFORMED ONE: unreadable-entry
  ;; goes up (uncertain-derived asks discovery before this is reached); a
  ;; record that reads and will not parse keeps the conservative answer.
  (define (retired-uncertain store writer)
    (let* ((p (writer-file store writer "retired.sexp"))
           (bytes (read-entry p)))
      (if (eq? bytes 'absent)
          '()
          (let ((d (guard (e (#t #f))
                     (string->sexpr-extended (utf8->string bytes)))))
            (if (not (list? d))
                (list (list writer 0 #f))
                (let loop ((xs d))
                  (cond
                    ((null? xs) (retired-legacy-uncertain writer d))
                    ((and (list? (car xs)) (= 2 (length (car xs)))
                          (eq? (caar xs) 'uncertain)
                          (list? (cadr (car xs))))
                     (if (>= (retired-format d) 2)
                         (filter (lambda (i)
                                   (and (uncertain-interval? i)
                                        (string=? (car i) writer)))
                                 (cadr (car xs)))
                         ;; A FORMAT 1 RECORD SOMEBODY AMENDED. The
                         ;; clause it now carries is true and the version
                         ;; still says the record predates the promise,
                         ;; so both readings are owed.
                         (append
                           (filter (lambda (i)
                                     (and (uncertain-interval? i)
                                          (string=? (car i) writer)))
                                   (cadr (car xs)))
                           (retired-legacy-uncertain writer d))))
                    (else (loop (cdr xs))))))))))

  ;; ADDING ONE, AS A UNION. Writing an entry that is already there
  ;; leaves the record as it was, which is what makes a resume safe to
  ;; run however many times a crash demands.
  ;; A WRITER READS THE STRICT ONE. This is the persistence boundary and
  ;; the rule is the same on both sides of it: a lenient reader answers
  ;; the widest thing the file could mean, which is right for deciding
  ;; and wrong for writing down. A one-shot read failure here would put
  ;; `(writer 0 #f)` into the record for good, and no later successful
  ;; read could lift it. Recovery amendments go through this, so the
  ;; strict gate in adopt does not cover them.
  (define (retired-add-uncertain! store writer interval)
    (when interval
      (let ((current (retired-uncertain-strict store writer)))
        (when (eq? current 'unreadable)
          (assertion-violation
            'retired-add-uncertain!
            "cannot amend a retirement record that will not read" writer))
        (unless (member interval current)
          ;; AND THE VERSION GOES UP WITH IT. `current` already carries
          ;; whatever a format 1 record's absence implied, so writing it
          ;; down makes the record say for itself what the fallback was
          ;; saying on its behalf -- and once it says so, the fallback
          ;; must stop, or the stretch would be counted from two places
          ;; for ever.
          (retired-put-clause! store writer
                               (list 'format 2)
                               (list 'uncertain
                                     (append current (list interval))))))))

  (define (step-retirement-prepared! store old tx seg off seq uncertain)
    (atomic-write! (writer-file store old "retired.sexp")
                   (string->utf8 (retired-text tx seg off seq #f #f uncertain))
                   'registry)
    tx)

  ;; THE PREDECESSOR'S UNCERTAIN STRETCH, PERSISTED BEFORE THE SUCCESSOR
  ;; EXISTS. Two of the adopt reasons leave a stretch of the old writer
  ;; that nobody can ever verify again, and this is the only moment their
  ;; coordinates are all in hand:
  ;;
  ;;   registry-ahead  the registry's water mark stands above the last
  ;;                   record the log holds, so records between the two
  ;;                   were authorised and then lost. `(end, mark]` --
  ;;                   closed, because the mark is a real top.
  ;;   identity        this store's identity does not match what its
  ;;                   owners say, so nothing above the prefix can be
  ;;                   attributed to it at all. `(end, +inf)` -- open,
  ;;                   because there is no top to name.
  ;;
  ;; IT IS WRITTEN BEFORE THE SUCCESSOR'S OWNER. A crash between the two
  ;; leaves an entry for a generation that never finished, which costs a
  ;; caller some `unknown` answers it did not need; a crash the other way
  ;; round would leave a finished adopt whose predecessor's lost stretch
  ;; nothing records, and a request that fell in it would be told to run
  ;; again.
  ;;
  ;; IT ADDS, NEVER REPLACES: an earlier adopt's entry is still true.
  ;;
  ;; AND IT GOES IN THE RETIREMENT RECORD FIRST. `uncertain.sexp` is
  ;; documented as a cache whose authority is the records, and for three
  ;; of the five kinds it is one -- they are re-derivable from a
  ;; writer's own files at any time. These two were not: their
  ;; coordinates existed only in the cache, so deleting the cache
  ;; deleted the history, and the store then reported itself healthy
  ;; about a stretch it could not vouch for. A request whose positions
  ;; fell in that stretch was told to run a second time.
  ;;
  ;; The record is where they belong: it is written before the
  ;; successor's owner exists, it is already in the barrier's recovery
  ;; closure, and it is the file whose absence already means "this
  ;; generation cannot be trusted". Putting them there does not make a
  ;; second place that COMPUTES the coordinates -- `adopt-uncertain-interval`
  ;; is still the only one -- it makes the cache a cache.
  (define (adopt-uncertain-interval old reason seq mark)
    (cond
      ((eq? reason 'registry-ahead) (and mark (> mark seq) (list old seq mark)))
      ((eq? reason 'identity) (list old seq #f))
      (else #f)))

  ;; AND THE CACHE WRITE READS THE CACHE, not a derivation of it.
  ;; `uncertain-load` asks the writer's files again and its derivation
  ;; answers `(writer 0 #f)` for any failure to read them -- so writing
  ;; its result back would put that fallback into the cache, where a
  ;; later adopt's union would find it readable and promote it into the
  ;; record. The chain is short and every link of it persists: the rule
  ;; is that nothing crosses into a file except what was already in one.
  (define (step-uncertain-prepared! store old interval)
    (when interval
      (let ((current (uncertain-cached store old)))
        (when (eq? current 'unreadable)
          (assertion-violation
            'step-uncertain-prepared!
            "cannot amend an uncertainty cache that will not read" old))
        (let ((have (or current '())))
          (uncertain-write! store old
                            (if (member interval have)
                                have
                                (append have (list interval)))
                            'registry)))))

  (define (step-generation-reserved! store store-id instance tx old new)
    (with-machine-lock
      (lambda ()
        (let ((reg (read-registry)))
          (write-registry!
            (registry-put-generation
              reg (make-gen store-id instance tx old new #f #f 'pending)))))))

  (define (step-owner-installed! store new nonce tx old)
    (mkdir-p! (writer-directory store new))
    (atomic-write! (writer-file store new "owner.sexp")
                   (string->utf8
                     (string-append "((machine \"" (machine-id) "\")"
                                    " (instance \"" nonce "\")"
                                    " (tx \"" tx "\")"
                                    " (predecessors (\"" old "\")))\n"))
                   'registry)
    ;; THE EMPTY FIRST SEGMENT IS PART OF MAKING THE WRITER EXIST. A
    ;; writer with an owner and no segment has no append target at all,
    ;; so every write is refused before it reserves -- correct about a
    ;; writer nobody finished making, and useless as the outcome of an
    ;; adopt. It carries no history, so creating it cannot corrupt
    ;; anything; its absence is what makes the new generation unusable.
    (let ((seg (string-append (writer-directory store new) "/" (segment-file-name 1))))
      (file-ensure! seg))
    (directory-entry-durable! (writer-directory store new) 'registry))

  ;; THE TRANSACTION IS NOT AN ARGUMENT ANY MORE. It was one only
  ;; because this step used to re-emit the whole record, and re-emitting
  ;; it is what the amendment above exists to stop: the record already
  ;; bears its transaction, and a step that restamps it can only ever
  ;; agree with the record or overwrite it.
  (define (step-successor-backfilled! store old new)
    (retired-put-clause! store old (list 'successor new)))

  (define (step-transition-complete! store-id instance tx)
    (with-machine-lock
      (lambda ()
        (let* ((reg (read-registry))
               (g (generation-with-tx reg tx)))
          (when g
            (write-registry!
              (registry-put-generation
                reg (make-gen store-id instance tx (gen-old g) (gen-new g)
                              (gen-lost-from g) (gen-lost-to g) 'active))))))))

  ;; ---- recovery -------------------------------------------------------------

  ;; THE NONCE TEST IS A GATE ON THE WHOLE TABLE, NOT A ROW IN IT. A
  ;; copied store presents "retired.sexp bears tx T, this instance's
  ;; registry has no T" -- which is the first row word for word, and the
  ;; first row's action is to resume. It would finish somebody else's
  ;; transaction, and finishing it proves nothing about THIS instance's
  ;; authority, because the owner it completes names their nonce.
  ;; Named for one writer, unlike `owner-nonce`, which answers for the
  ;; store by taking the first writer that has an owner at all. The gate
  ;; has to ask about a particular writer.
  (define (writer-owner-nonce store writer)
    (let ((p (writer-file store writer "owner.sexp")))
      (and (entry-present? p)
           (let ((d (guard (e (#t #f))
                      (string->sexpr-extended (utf8->string (read-whole p))))))
             (and (list? d) (alist-ref d 'instance))))))

  (define (writer-owner-tx store writer)
    (let ((p (writer-file store writer "owner.sexp")))
      (and (entry-present? p)
           (let ((d (guard (e (#t #f))
                      (string->sexpr-extended (utf8->string (read-whole p))))))
             (and (list? d) (transaction-of d))))))

  ;; What the store says about a transaction, without deciding anything.
  (define (transaction-state store reg store-id instance tx old)
    (let* ((r (retired-of store old))
           (g (generation-with-tx reg tx))
           (new (and g (gen-new g)))
           (owner? (and new (entry-present? (writer-file store new "owner.sexp"))))
           (successor (and r (not (eq? (car r) 'malformed)) (retired-successor store old))))
      (list (cons 'retired (and r (not (eq? (car r) 'malformed)) (equal? (list-ref r 3) tx)))
            (cons 'gen (and g (gen-state g)))
            (cons 'owner owner?)
            (cons 'successor (and successor #t)))))

  (define (retired-successor store writer)
    (let ((p (writer-file store writer "retired.sexp")))
      (and (entry-present? p)
           (let ((d (guard (e (#t #f))
                      (string->sexpr-extended (utf8->string (read-whole p))))))
             (and (list? d)
                  (let loop ((xs d))
                    (cond ((null? xs) #f)
                          ((and (list? (car xs)) (= 2 (length (car xs)))
                                (eq? (caar xs) 'successor))
                           (cadr (car xs)))
                          (else (loop (cdr xs))))))))))

  ;; ---- the driver -----------------------------------------------------------

  ;; WHY AN ADOPT IS NEEDED IS DERIVED FROM VERIFIED STATE, never passed
  ;; in. The conditions can hold at once, and taking one as THE reason
  ;; discards the protection the others give: a caller who says "damage"
  ;; skips the rollback check that only the registry can make.
  ;; IS THE NAME instance.sexp PRESENT? Asked of the NAME, with an lstat
  ;; (entry-name-type): a link is present, a dangling one included, where a
  ;; stat would follow it. The read side opens the same name through the
  ;; same file system, so on a case-folding volume the two agree, and only
  ;; search permission on the store is needed, never a listing. A failure
  ;; other than absence raises unreadable-entry naming the path. `stage` is
  ;; the fault stage the question is asked in: `presence` for the
  ;; inventory's, `presence-decision` for adopt-needed?'s.
  (define (instance-name-present? store stage)
    (parameterize ((theourgia-stage stage))
      (not (eq? (entry-name-type (string-append store "/instance.sexp")) 'absent))))

  (define (adopt-needed? store)
    (let* ((id (verify-instance store))
           (writer (local-writer-name store)))
      (cond
        ((and (pair? id) (eq? (car id) 'mismatch)) (list 'identity (cadr id)))
        ;; A CHECKOUT OF A STORE KEPT IN GIT HAS NO instance.sexp: its
        ;; local writer travelled, its identity did not, so adopt mints one
        ;; exactly as for a copied store. The NAME must be absent from the
        ;; store's directory: a present instance.sexp that parses to #f, or
        ;; a symlink that points nowhere, also verifies as absent -- a stat
        ;; follows the link -- and minting over either would replace an
        ;; entry this path cannot explain. The name's own presence (an lstat)
        ;; sees the link itself. The inventory has already asked it, so a
        ;; failure here is a race: it propagates, named by the request's
        ;; failure table.
        ((and (eq? id 'absent) writer (not (instance-name-present? store 'presence-decision)))
         (list 'identity 'instance-absent))
        ((not writer) (list 'no-local-writer))
        ((let ((r (retired-of store writer))) (and r #t)) (list 'retired))
        (else
         (let* ((store-id (store-id-of store))
                (instance (instance-nonce store))
                (reg (with-machine-lock (lambda () (read-registry)))))
           (cond
             ((not (generation-chain-ok? store reg store-id instance))
              (list 'missing-generation))
             ((registry-ahead-of-log store reg store-id instance writer)
              => (lambda (mark) (list 'registry-ahead mark)))
             ((writer-damaged? store writer) (list 'damage))
             (else #f)))))))

  ;; TWO DIFFERENT NUMBERS, AND THE DIFFERENCE IS THE WHOLE POINT.
  ;;
  ;; THE TEST IS ON `written`: records that reached the disk and are no
  ;; longer in the log are a rollback -- history this store was told it
  ;; had and no longer has. `authorised` above the log's end is not:
  ;; it is a request that reserved more positions than it used, which
  ;; every refusal does, and reading that as a rollback would demand an
  ;; adopt for a store nothing went wrong with.
  ;;
  ;; THE ANSWER IS `authorised`, because the caller is an adopt about to
  ;; record an uncertain stretch and the stretch has to cover every
  ;; position anyone was given leave to write -- including the ones a
  ;; lost request reserved and never reached. Answering `#t` and reading
  ;; the registry again in adopt would be two suppliers of one number.
  (define (registry-ahead-of-log store reg store-id instance writer)
    (let ((written (registry-written reg store-id instance writer))
          (authorised (registry-authorised reg store-id instance writer))
          (p (guard (e (#t #f)) (discover-prefix store writer 'held-exclusive))))
      (and written p (> written (discovery-end-seq p)) authorised)))

  (define (writer-damaged? store writer)
    (let ((p (guard (e (#t #f)) (discover-prefix store writer 'held-exclusive))))
      (and p (not (null? (discovery-integrity p))))))

  ;; CONTINUATION IS AUTOMATIC AND NEEDS NOBODY. Every crash state maps
  ;; to one action; the gate runs first.
  ;; NEVER: THE CONTINUATION READS THE WHOLE STORE BEFORE ITS FIRST WRITE,
  ;; ACROSS THE WHOLE CALL (R2h, F77b). A per-generation check would cancel
  ;; an earlier generation -- a registry write -- before refusing at a later
  ;; one, and reading the registry the ordinary way upgrades a legacy one,
  ;; which writes. It asks adopt-inventory, as adopt does: a refusal from
  ;; verify-instance or a discovery is its answer, and a read that fails
  ;; raises unreadable-entry naming the file, with nothing written.
  (define (continue-adopt! store)
    (or (adopt-inventory store)
        (continue-adopt-read! store)))

  (define (continue-adopt-read! store)
    (let* ((store-id (store-id-of store))
           (instance (instance-nonce store))
           (reg (with-machine-lock (lambda () (read-registry))))
           (old (local-writer-name store)))
      (cond
        ((not (and store-id instance old)) 'nothing-to-do)
        ;; IDENTITY FIRST. A copied store still carries the original's
        ;; instance.sexp, so its owners' nonces match trivially and the
        ;; gate below would let it through. The design's order is the
        ;; answer: an identity mismatch mints a new nonce BEFORE the
        ;; registry is consulted, and only then can "whose transaction
        ;; is this" be asked at all.
        ((let ((v (verify-instance store))) (and (pair? v) (eq? (car v) 'mismatch)))
         'identity-mismatch)
        ;; THE GATE: history whose owner names another instance is
        ;; imported, not interrupted. It runs before the table because
        ;; several rows match on the transaction id alone and would fire
        ;; on a foreign transaction before anything looked at whose it
        ;; was.
        ((imported-history? store instance) 'imported)
        ;; THE TABLE'S FIRST ROW IS STORE-DRIVEN, not registry-driven: a
        ;; crash between step 1 and step 2 leaves a marker the registry
        ;; has never heard of. A loop over the registry's own
        ;; generations cannot see that state at all.
        ((orphan-marker store reg store-id old)
         => (lambda (tx)
              (let ((new (derive-writer-id store tx)))
                (resume-uncertain! store reg store-id instance old)
                (step-generation-reserved! store store-id instance tx old new)
                (step-owner-installed! store new instance tx old)
                (step-successor-backfilled! store old new)
                (step-transition-complete! store-id instance tx)
                'resumed)))
        (else
         (let loop ((gs (generations-of reg store-id instance)) (did 'nothing-to-do))
           (if (null? gs)
               did
               (let* ((g (car gs))
                      (tx (gen-tx g))
                      (st (gen-state g)))
                 (cond
                   ;; terminal: never resumed, whatever the store holds
                   ((memq st '(cancelled lost active)) (loop (cdr gs) did))
                   ((eq? st 'pending)
                    (let ((s (transaction-state store reg store-id instance tx (gen-old g))))
                      (cond
                        ;; neither marker nor owner: this generation was
                        ;; never authorised to commit a record, so it can
                        ;; be cancelled without orphaning one
                        ((and (not (cdr (assq 'retired s))) (not (cdr (assq 'owner s))))
                         (cancel-generation! store-id instance tx g)
                         (loop (cdr gs) 'cancelled))
                        ((not (cdr (assq 'owner s)))
                         (resume-uncertain! store reg store-id instance (gen-old g))
                         (step-owner-installed! store (gen-new g) instance tx (gen-old g))
                         (step-successor-backfilled! store (gen-old g) (gen-new g))
                         (step-transition-complete! store-id instance tx)
                         (loop (cdr gs) 'resumed))
                        ((not (cdr (assq 'successor s)))
                         (step-successor-backfilled! store (gen-old g) (gen-new g))
                         (step-transition-complete! store-id instance tx)
                         (loop (cdr gs) 'resumed))
                        (else
                         (step-transition-complete! store-id instance tx)
                         (loop (cdr gs) 'resumed)))))
                   (else (loop (cdr gs) did))))))))))

  ;; RESUMING AN ADOPT MUST NOT SKIP THE ENTRY. A crash can land between
  ;; the retirement marker and the uncertain entry, so recovery cannot
  ;; assume the entry is there -- and it cannot read the reason either,
  ;; because nothing records one. What it can do is ask the same question
  ;; again: the registry's mark and the retired prefix are both still on
  ;; disk, so a rollback is still recognisable as such. The identity kind
  ;; never reaches here; that case leaves adopt before any of this.
  ;;
  ;; Writing an entry that is already there is harmless -- the write is
  ;; the union -- and that is the property that makes a resume safe to
  ;; run however many times a crash demands.
  (define (resume-uncertain! store reg store-id instance old)
    (let* ((r (retired-of store old))
           (seq (and r (not (eq? (car r) 'malformed)) (caddr r)))
           ;; THE TOP OF THE STRETCH IS `authorised`, as it is in adopt:
           ;; the entry has to cover every position anyone was given
           ;; leave to write, not only the ones that reached the disk.
           (mark (registry-authorised reg store-id instance old)))
      (when (and seq mark (> mark seq))
        (let ((interval (adopt-uncertain-interval old 'registry-ahead seq mark)))
          (retired-add-uncertain! store old interval)
          (step-uncertain-prepared! store old interval)))))

  ;; A retirement marker bearing a transaction the registry has never
  ;; recorded. Only for the local writer: another writer's marker is
  ;; mirrored history, not this machine's interrupted work.
  (define (orphan-marker store reg store-id writer)
    (let ((r (retired-of store writer)))
      (and r
           (not (eq? (car r) 'malformed))
           (list-ref r 3)
           (not (generation-with-tx reg (list-ref r 3)))
           (list-ref r 3))))

  (define (imported-history? store instance)
    (let loop ((ws (store-writers store)))
      (cond
        ((null? ws) #f)
        ((let ((n (writer-owner-nonce store (car ws))))
           (and n (writer-owner-tx store (car ws)) (not (equal? n instance))))
         #t)
        (else (loop (cdr ws))))))

  (define (cancel-generation! store-id instance tx g)
    (with-machine-lock
      (lambda ()
        (let ((reg (read-registry)))
          (write-registry!
            (registry-put-generation
              reg (make-gen store-id instance tx (gen-old g) (gen-new g)
                            (gen-lost-from g) (gen-lost-to g) 'cancelled)))))))

  ;; ADOPT REFUSES ON A HEALTHY STORE. It is the way out of a named
  ;; condition; a voluntary generation change splits history for
  ;; nothing. The refusal says which conditions were looked for.
  ;; ADOPT TAKES THE STORE LOCK ITSELF. Every step of the transaction has
  ;; to be inside it, and this is the entry point a person or a command
  ;; reaches. `continue-adopt!` is the other half and assumes the lock is
  ;; already held, because it runs on the write-open path that took it.
  (define (adopt! store)
    (claim-store! store 'adopt)
    (let ((lock ((current-lock-acquire) (string-append store "/lock") 'exclusive)))
      (let ((answer (guard (e (#t ((current-lock-release) lock) (release-store! store) (raise e)))
                      (adopt-locked! store))))
        ((current-lock-release) lock)
        (release-store! store)
        answer)))

  ;; ---- reading before writing (R2h, K13; F77b) --------------------------------
  ;;
  ;; NEVER: EVERY READ ADOPT DEPENDS ON HAPPENS BEFORE ITS FIRST WRITE, and a
  ;; read that fails refuses by name before anything is touched. adopt-needed?
  ;; reads the registry (a legacy one is upgraded, which writes), an identity
  ;; mismatch installs a new instance.sexp, and the retirement is written
  ;; before the cache is read -- each of those came before a refusal that
  ;; could only then find it could not read something.
  ;;
  ;; KEY: UNREADABLE IS NOT DAMAGED (K13). A segment or metadata file this
  ;; process cannot read leaves a read-failure note on the local writer's
  ;; discovery, and adopt-needed? would read that as damage and retire the
  ;; writer at its readable prefix -- permanently discarding a history that
  ;; is intact and returns with the permission. Adopt refuses such a writer.

  ;; -> #f, or the refusal adopt answers: (refused <kind> (path p) (reason r)),
  ;; kind owner-unreadable (verify-instance's own answer), segment-unreadable
  ;; or metadata-unreadable.
  (define (adopt-preflight store)
    (guard (e ((unreadable-entry? e)
               (unreadable-refusal (if (equal? (unreadable-entry-path e) (registry-path))
                                       'registry-unreadable
                                       'metadata-unreadable)
                                   e)))
      (adopt-inventory store)))

  ;; KEY: THE WHOLE STORE IS READ, NOT A LIST OF WHAT ADOPT IS THOUGHT TO
  ;; REACH (F77b review 1). A list of the head's files missed the registry,
  ;; a predecessor's retirement, a successor's owner, and the continuation's
  ;; discovery -- each a read that an adopt path makes after a write. So:
  ;; verify-instance (its refusal is the answer), the registry with R1's
  ;; read and no upgrade, and every writer's discovery read failures and its
  ;; owner, uncertainty and retirement files with R1's read. A writer that
  ;; is only a mirror is read too: the chain, imported-history? and
  ;; resume-uncertain! read other writers' files on their way, so an adopt
  ;; that cannot read them cannot know its writes are safe.
  ;; A discovery is asked only for READ failures (F77b review 2): an
  ;; unreadable-entry propagates and a read-failure note refuses, but any
  ;; other condition -- a readable file that does not parse, say a mirror's
  ;; quarantine.sexp holding ((fork bad)) -- is not a read failure, and is
  ;; left to adopt's own path, as on the base, which never read mirrors.
  (define (inventory-read-failure store writer)
    (let ((p (guard (e ((unreadable-entry? e) (raise e)) (#t #f))
               (discover-prefix store writer 'held-exclusive))))
      (and p (read-failure-refusal p))))

  ;; -> #f, a refusal answer, or a raised unreadable-entry naming the file.
  ;; THE NAME instance.sexp IS ASKED HERE TOO, beside verify-instance's stat:
  ;; the read pass mints nothing (verify-instance may still create this
  ;; machine's own record on a first run, as it always has), and an lstat
  ;; that fails raises inside the preflight's guard, answered
  ;; metadata-unreadable naming the path, as any unreadable inventory file
  ;; is.
  (define (adopt-inventory store)
    (let ((v (verify-instance store)))
      (if (and (pair? v) (eq? (car v) 'refused))
          v
          (begin
            (instance-name-present? store 'presence)
            (with-machine-lock (lambda () (registry-as-read)))
            (let loop ((ws (store-writers store)))
              (cond
                ((null? ws) #f)
                ((inventory-read-failure store (car ws)))
                (else
                 (read-entry (writer-file store (car ws) "owner.sexp"))
                 (adopt-reads! store (car ws))
                 (loop (cdr ws)))))))))

  (define (unreadable-refusal kind e)
    (list 'refused kind
          (list 'path (unreadable-entry-path e))
          (list 'reason (unreadable-entry-reason e))))

  ;; The first note on a discovery that is a READ failure, as a refusal. A
  ;; CRC, torn or framing note is damage and is not answered here: that is
  ;; what adopt exists for.
  (define (read-failure-refusal p)
    (let loop ((es (discovery-integrity p)))
      (cond
        ((null? es) #f)
        ((memq (log-error-kind (car es)) '(segment-unreadable metadata-unreadable))
         (let* ((detail (log-error-detail (car es)))
                (path (assq 'path detail))
                (reason (assq 'reason detail)))
           (list 'refused (log-error-kind (car es))
                 (list 'path (and path (cdr path)))
                 (list 'reason (and reason (cdr reason))))))
        (else (loop (cdr es))))))

  ;; The writer's uncertainty cache and retirement record, read with the R1
  ;; operation: absence is fine, and any other failure raises unreadable-entry
  ;; naming the file. What they hold is read again, as before, by the step
  ;; that uses it.
  (define (adopt-reads! store writer)
    (read-entry (uncertain-path store writer))
    (read-entry (writer-file store writer "retired.sexp")))

  (define (adopt-locked! store)
    (or (adopt-preflight store)
        (adopt-decided! store)))

  (define (adopt-decided! store)
    (let ((why (adopt-needed? store)))
      (if (not why)
          (list 'refused 'not-needed
                (list 'checked '(identity retired missing-generation registry-ahead damage)))
          (let* ((store-id (store-id-of store))
                 (old (local-writer-name store))
                 (instance (adopt-nonce store why))
                 (p (discover-prefix store old 'held-exclusive))
                 (seg (if (pair? (discovery-physical-current p))
                          (car (discovery-physical-current p))
                          1))
                 ;; WHERE THE VALID PREFIX ENDS, not where the file
                 ;; ends. The whole reason this adopt is happening is
                 ;; that there are bytes past the prefix; declaring the
                 ;; file length as the retirement offset points the
                 ;; marker at them, and the next load reports the marker
                 ;; and the scan disagreeing about where seq N finished.
;; A WRITER THAT HAS WRITTEN NOTHING HAS NO END OFFSET. That is
                 ;; the state of a generation adopted the moment after it
                 ;; was created -- an empty segment and no records -- and
                 ;; its retirement prefix is simply (1 0 0).
                 (off (or (discovery-end-offset p) 0))
                 (seq (or (discovery-end-seq p) 0))
                 (tx (new-transaction-id))
                 (new (derive-writer-id store tx)))
            (let* ((interval (adopt-uncertain-interval
                               old (car why) seq
                               (and (pair? (cdr why)) (cadr why))))
                   ;; STEP 1 WRITES THE WHOLE RECORD, SO IT HAS TO CARRY
                   ;; WHAT THE RECORD ALREADY SAID. An adopt that gets as
                   ;; far as this write and no further leaves a
                   ;; retirement with no successor -- and `adopt-needed?`
                   ;; answers `retired` for that, so the next adopt comes
                   ;; back here with a reason of its own that records
                   ;; nothing. Writing only the new interval then
                   ;; replaced a recorded stretch with `()`: the one
                   ;; place the coordinates still existed, overwritten by
                   ;; the retry of the very adopt that computed them.
                   ;;
                   ;; IT ADDS, NEVER REPLACES -- the same rule the cache
                   ;; has always followed, applied to the record that is
                   ;; now the authority. The union is taken over both,
                   ;; because a legacy record's stretch lives in the
                   ;; cache and must survive being written down.
                   ;;
                   ;; AND IT DOES NOT RE-DERIVE. `uncertain-load` asks the
                   ;; writer's own files again, and that derivation
                   ;; answers `(writer 0 #f)` for ANY failure to read
                   ;; them -- which is right for a reader deciding what
                   ;; it can vouch for, and wrong here, because here the
                   ;; answer is written down and becomes permanent. A
                   ;; transient failure to read a directory would bake an
                   ;; open stretch into the record for good, and no
                   ;; repair and no operator's determination could ever
                   ;; lift it. What is written down is only what is
                   ;; already written down: the record's own clause and
                   ;; the cache's contents.
                   ;; NEITHER SOURCE MAY BE GUESSED AT. If the record
                   ;; cannot be read, its clause is unknown and writing a
                   ;; replacement would put a guess where the history
                   ;; was. If the CACHE cannot be read, it may hold the
                   ;; only copy of a stretch -- a torn tail that was
                   ;; repaired lives on in the cache and nowhere else --
                   ;; and the step after this one overwrites the cache,
                   ;; so treating it as empty would take both copies at
                   ;; once. An adopt that cannot read what it is about to
                   ;; replace refuses; an operator can then repair or
                   ;; remove the file, and nothing has been lost in the
                   ;; meantime.
                   (recorded (retired-uncertain-strict store old))
                   (cached (uncertain-cached store old))
                   (ignored
                     (when (or (eq? recorded 'unreadable) (eq? cached 'unreadable))
                       (assertion-violation
                         'adopt!
                         "cannot adopt while a writer's uncertainty is unreadable"
                         (list old
                               (list 'retired (if (eq? recorded 'unreadable)
                                                  'unreadable 'readable))
                               (list 'cache (if (eq? cached 'unreadable)
                                                'unreadable 'readable))))))
                   (kept (append (if (eq? recorded 'unreadable) '() recorded)
                                 (if (or (not cached) (eq? cached 'unreadable)) '() cached)))
                   (carried
                     (let loop ((xs (if (and interval (not (member interval kept)))
                                        (append kept (list interval))
                                        kept))
                                (out '()))
                       (cond ((null? xs) out)
                             ((member (car xs) out) (loop (cdr xs) out))
                             (else (loop (cdr xs) (append out (list (car xs)))))))))
              ;; THE STRETCH IS PART OF STEP 1, not a step after it. A
              ;; crash between the record and a separate entry used to
              ;; leave a retirement whose lost stretch nothing recorded,
              ;; and there is no recovery that can recompute the
              ;; identity kind afterwards -- the mismatched identity is
              ;; gone by then. One atomic write carries both.
              (step-retirement-prepared! store old tx seg off seq carried)
              (step-uncertain-prepared! store old interval))
            (step-generation-reserved! store store-id instance tx old new)
            (step-owner-installed! store new instance tx old)
            (step-successor-backfilled! store old new)
            (step-transition-complete! store-id instance tx)
            (append
              (list 'adopted (list 'from old) (list 'to new)
                    (list 'prefix seg off seq) (list 'reason (car why)))
              ;; WHICH IDENTITY: beside the reason, the field that
              ;; did not match, or instance-absent for a checkout.
              (if (eq? (car why) 'identity)
                  (list (list 'identity (cadr why)))
                  '()))))))

  ;; A RECOVERY ADOPT KEEPS THE NONCE; AN IDENTITY MISMATCH MINTS ONE
  ;; FIRST, before the registry is consulted, so a copied store's old
  ;; nonce is never used to look up this instance's chain.
  (define (adopt-nonce store why)
    (if (eq? (car why) 'identity)
        (instance-install! store)
        (instance-nonce store)))

  (define (segment-length store writer seg)
    (let ((p (string-append (writer-directory store writer) "/" (segment-file-name seg))))
      (if (entry-present? p) (bytevector-length (read-whole p)) 0)))

  (define (derive-writer-id store tx)
    (let ((digits "0123456789abcdefghijklmnopqrstuvwxyz")
          (hex (bytevector->hex (sha256 (string->utf8 (string-append store "|" tx))))))
      (let loop ((i 0) (acc 0))
        (if (= i 12)
            (let build ((n 8) (v acc) (out '()))
              (if (= n 0)
                  (list->string out)
                  (build (- n 1) (div v 36) (cons (string-ref digits (mod v 36)) out))))
            (loop (+ i 1)
                  (+ (* acc 16)
                     (let ((c (string-ref hex i)))
                       (if (char<=? #\0 c #\9)
                           (- (char->integer c) (char->integer #\0))
                           (+ 10 (- (char->integer c) (char->integer #\a)))))))))))

  (define (authorised? s seq)
    (let ((a (session-authorised s)))
      (and a (>= seq (car a)) (<= seq (cdr a)))))

  ;; ONE RESERVATION FOR THE WHOLE REQUEST, AS A SESSION-BOUND
  ;; AUTHORISATION. Under the machine lock, atomically: the old water
  ;; mark must be exactly `start - 1` -- anything else is a rollback and
  ;; is refused -- and it is then raised to `end`.
  ;;
  ;; IT BELONGS TO THIS REQUEST AND LAPSES WITH IT. A retry does not
  ;; inherit it: the authorisation says "these positions are mine to
  ;; write now", and a later attempt has to ask again against whatever
  ;; the mark has become.
  ;;
  ;; Over-reserving is safe and under-reserving is not: the uncertain
  ;; interval a rollback records is `(durable, mark]`, so positions that
  ;; were reserved and never written are already covered by it.
  (define (session-authorise! s count)
    (let* ((store (session-store s))
           (v (session-view s))
           (start (and v (view-expect-seq v))))
      (cond
        ((or (not v) (not (> count 0))) 'nothing-to-authorise)
        (else
         (let ((outcome (reserve-range! store (store-id-of store) (instance-nonce store)
                                        (session-writer s) start (+ start count -1))))
           (if (eq? outcome 'reserved)
               (begin (session-authorised-set! s (cons start (+ start count -1)))
                      'reserved)
               outcome))))))

  ;; AN EMPTY OR BACKWARD RANGE IS NOT A RESERVATION. `[10, 9]` reserves
  ;; nothing and then lets a record be written at 10 outside it -- which
  ;; is the one thing a reservation exists to prevent. It is refused by
  ;; name rather than treated as a range of zero positions.
  (define (reserve-range! store store-id instance writer start end)
    (unless (and (integer? start) (integer? end) (>= end start))
      (assertion-violation 'reserve-range! "not a range" (list start end)))
    (reserve-range-locked! store store-id instance writer start end))

  (define (reserve-range-locked! store store-id instance writer start end)
    (parameterize ((current-machine-home (machine-home)))
      (with-machine-lock
        (lambda ()
          (let* ((reg (read-registry))
                 (written (registry-written reg store-id instance writer)))
            (barrier! 'registry-read)
            (cond
              ((registry-inside-store?) (list 'registry-inside-store))
              ((not (generation-chain-ok? store reg store-id instance))
               (list 'missing-generation))
              ;; THE CHECK IS AGAINST `written`, AND IT IS AN UPPER BOUND
              ;; RATHER THAN AN EQUALITY.
              ;;
              ;; `written` at or above this request's first position is a
              ;; rollback: the registry says records reached the disk
              ;; there and the log does not hold them. That is the
              ;; refusal, and it is the same one the single water mark
              ;; used to make.
              ;;
              ;; BELOW IT IS NOT AN ERROR. A registry that has never
              ;; heard of this writer reads 0 while the log already
              ;; stands at 2 -- every store whose registry was created
              ;; after its history, and every fixture that builds a log
              ;; by hand. Demanding equality would refuse all of them.
              ;;
              ;; `authorised` CANNOT BE THE TEST at all: a request
              ;; reserves before it knows how much of its range it will
              ;; use, and a refusal uses none, so `authorised` stands
              ;; above the next request's first position and testing
              ;; against it would let the first refusal in a store stop
              ;; every write after it.
              ((>= (or written 0) start)
               (list 'registry-ahead (or written 0)))
              (else
               ;; AND `written` CATCHES UP TO THE LOG WHILE WE ARE HERE.
               ;; It is raised in a second machine-lock section after the
               ;; barrier, so a crash between the two leaves records
               ;; durable and the registry counting fewer of them -- and
               ;; a restore to an older backup would then not be
               ;; detected, because the gate compares `written` with the
               ;; log's end and both would be low. Everything below
               ;; `start` is in the log and the delivery barrier of this
               ;; session's own open made it durable, so raising
               ;; `written` to `start - 1` states a fact rather than a
               ;; hope.
               ;;
               ;; IT ONLY EVER RAISES. A log SHORTER than `written` is
               ;; the rollback, and it was refused by the arm above; the
               ;; max here cannot lower the number that would have fired
               ;; it.
               (let* ((caught-up (registry-note-written reg store-id instance writer
                                                        (- start 1)))
                      (next (registry-raise caught-up store-id instance writer end)))
                 (trace-event! 'registry-write (registry-path) #f)
                 (atomic-write! (registry-path)
                                (string->utf8
                                  (string-append (sexpr->string-extended next) "\n"))
                                'registry)
                 'reserved))))))))

  (define (reserve! store store-id instance writer seq)
    (parameterize ((current-machine-home (machine-home)))
      (with-machine-lock
      (lambda ()
        (let* ((reg (read-registry))
               (mark (registry-authorised reg store-id instance writer)))
          (barrier! 'registry-read)
          (cond
            ;; THE GENERATION CHAIN IS RE-CHECKED HERE, in the same
            ;; machine-lock section that re-checks the water mark, and
            ;; for the same reason: what was true when the handle opened
            ;; is not what authorises a write. A handle opened before
            ;; somebody else adopted still sees its own writer as
            ;; healthy -- identity, owner, no retirement marker, water
            ;; mark where it left it -- and a check performed only at
            ;; open leaves a gap exactly as wide as that handle's life.
            ;; THE WITNESS MUST NOT BE INSIDE WHAT IT WATCHES. Checked
            ;; here rather than only at open for the same reason the
            ;; chain is: the environment can change under a live handle.
            ((registry-inside-store?) (list 'registry-inside-store))
            ((not (generation-chain-ok? store reg store-id instance))
             (list 'missing-generation))
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
    ;; WHAT THIS STORE CANNOT READ IT MAY NOT BUILD ON. A local writer
    ;; whose own metadata will not open cannot be flushed, cannot be
    ;; compared against the session's remembered versions, and cannot be
    ;; promised durable -- so the append is refused before anything is
    ;; reserved, naming the file and what the operating system said. The
    ;; reading side stops that writer before its records; the writing
    ;; side stops before its next one. Neither pretends.
    (let ((unreadable (unreadable-metadata (session-store s) (session-writer s))))
      (if unreadable
          (list 'refused-before-reserve 'metadata-unreadable
                (list 'path (car unreadable))
                (list 'reason (cadr unreadable)))
          ;; THE BARRIER COMES FIRST, BEFORE THE RELOAD. A reload delivers
          ;; the records the new metadata admits, and delivery implies
          ;; durability -- so flushing afterwards means the reducer has
          ;; already been handed records whose manifest may not survive the
          ;; crash. The versions are on disk by now either way; what this
          ;; decides is whether they are durable before anything is
          ;; computed from them.
          (let ((durable (guard (e (#t 'barrier-failed)) (metadata-barrier! s))))
            (if (eq? durable 'barrier-failed)
                (list 'refused-before-reserve 'metadata-not-durable)
                (begin
                  (when (and (not (session-reset-pending s)) (versions-changed? s))
                    (reload! s))
                  (append-after-barrier! s frame)))))))

  ;; A SESSION HANDS ITS STATE OUTWARD ONLY WHEN ITS LOAD WAS DELIVERED
  ;; (F77c; design reviews r4 and r5): not aborted, and no reset pending.
  ;; ONE predicate, asked by both points that hand a session's state out --
  ;; the publication after a write (store.sc publish-after) and the
  ;; snapshot (session-snapshot!). A partial state is never published and
  ;; never installed, whatever stopped its delivery.
  (define (session-delivered? s)
    (and (eq? (load-session-outcome (session-load s)) 'open)
         (not (session-reset-pending s))))

  (define (append-after-barrier! s frame)
    (if (session-reset-pending s)
        ;; A STRUCTURED READINESS REFUSAL, naming the state. Not `unseen`
        ;; and not `unknown`: those are answers about the request, and
        ;; this is an answer about the session.
        ;;
        ;; WHERE THE RESET CARRIES A REASON, the reason is the answer: a
        ;; session held back because its inherited baseline names records
        ;; this store can no longer read is in a different situation from
        ;; one waiting for a reducer to acknowledge a reset, and an
        ;; operator told only `reset-pending` cannot tell which.
        (if (pair? (session-reset-pending s))
            (cons 'refused-before-reserve (session-reset-pending s))
            (list 'refused-before-reserve 'reset-pending))
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
  ;; THE REASON TRAVELS AS FAR AS THE REFUSAL DOES. A session held back
  ;; because its inherited baseline names records this store can no
  ;; longer read carries why, and flattening that to the bare word
  ;; `reset-pending` here would lose it at the last step -- the caller
  ;; asks for a view, is told no, and cannot tell this from a reducer
  ;; that has yet to acknowledge a reset.
  (define (no-view-reason s)
    (cond
      ((session-reset-pending s)
       (let ((why (session-reset-pending s)))
         (if (pair? why) why 'reset-pending)))
      ((session-poisoned s) 'writer-stopped)
      ((session-retired? s) 'retired)
      ((session-unconfirmed s) 'not-ready)
      ;; A WRITER THAT COULD NOT BE READ, WITH NO READABLE LOCAL WRITER
      ;; BESIDE IT, is not "no local writer": it may be this store's own
      ;; (R4). The refusal names the path and the reason.
      ((session-unreadable-refusal s))
      ;; NEVER: NO WRITER AT ALL IS ITS OWN ANSWER, AND IT HAS TO COME FIRST.
      ;; `predecessor-applied?` asks whether this writer's predecessor is in
      ;; the state; with no writer there is no predecessor either, so the
      ;; cascade fell through to `predecessor-not-applied` -- a reason that
      ;; tells the caller to apply more records, which will never help.
      ;; Measured on two stores that have no writer this machine may use:
      ;;
      ;;   received (published in, local writer gone)  predecessor-not-applied
      ;;   every local writer retired, no successor    predecessor-not-applied
      ;;
      ;; and before `local-writer-of` stopped naming a retired writer the
      ;; second said `retired`, which at least named its condition. Both are
      ;; the same situation -- there is nobody here to write as -- so both
      ;; now say so, and `no-local-writer` stops being a branch nothing
      ;; could reach.
      ((not (session-writer s)) 'no-local-writer)
      ((not (predecessor-applied? s)) 'predecessor-not-applied)
      (else 'no-local-writer)))

  ;; The same answer a refused append would give, asked before there is
  ;; a frame to refuse.
  (define (session-view-refusal s)
    (and (not (session-view s)) (no-view-reason s)))

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
      ;; A SNAPSHOT NAMES RECORDS, SO IT MAKES THEM DURABLE FIRST. This
      ;; session may have appended and not yet reached its request's
      ;; barrier; the cut it is about to write would then name records
      ;; that are on no disk, and the refusal below would fire for a
      ;; reason the caller cannot act on -- its own unflushed tail. The
      ;; commit is the same one a request runs, so this is not a second
      ;; way of making things durable, only an earlier moment to run it.
      (cond
        ;; A SNAPSHOT NAMES RECORDS, SO IT MAKES THEM DURABLE FIRST. This
        ;; session may have appended and not yet reached its request's
        ;; barrier; the cut it is about to write would then name records
        ;; that are on no disk. The commit is the same one a request
        ;; runs -- not a second way of making things durable, only an
        ;; earlier moment to run it.
        ;;
        ;; AND A COMMIT THAT FAILED IS NOT ITSELF THE REFUSAL. It is an
        ;; attempt to extend the durable frontier; when it fails the
        ;; frontier simply has not moved, and the rule that already
        ;; exists -- a cut may not reach past it -- gives the caller the
        ;; actionable fact: how far this writer IS durable. Refusing
        ;; outright would also refuse a cut at records an earlier
        ;; request made durable, which this failure says nothing about.
        ((begin (guard (e (#t #f)) (session-commit! s)) #f) #f)
        ((not (session-delivered? s)) (list 'refused 'not-delivered))
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
  ;; APPENDS, and it is the one the session has made durable -- not the
  ;; one it has written. While every append fsynced the two were the
  ;; same number and this read `next-seq`; they are not the same any
  ;; more, and reading the written frontier here would let a snapshot
  ;; name a cut whose records are not on disk. That is a snapshot which
  ;; is internally a lie and which nothing downstream can detect.
  (define (durable-seq s writer)
    (if (and (session-writer s) (string=? writer (session-writer s)))
        (session-durable-seq s)
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
        ((and (pair? identity) (eq? (car identity) 'refused))
         (cons 'refused-before-reserve (cdr identity)))
        ((pair? identity)
         (list 'refused-before-reserve (list 'instance (cadr identity))))
        ;; A SEQUENCE INSIDE THIS REQUEST'S AUTHORISATION IS ALREADY
        ;; RESERVED. Going to the registry again would read a mark this
        ;; session itself raised and refuse the writer its own
        ;; reservation.
        ((authorised? s seq) (write-line! s frame line seq target target-path fresh?))
        (else
         ;; THE WHOLE REQUEST'S RANGE, TAKEN AT ITS FIRST APPEND AND NOT
         ;; BEFORE. Every check above this point -- the instance, the
         ;; generation chain, integrity, reset -- has to run first: a
         ;; reservation taken before them would raise `authorised` for a
         ;; request that is about to be refused for a reason that has
         ;; nothing to do with the registry, and would report that
         ;; refusal in the registry's words instead of its own.
         ;; THE RANGE IS NAMED ONCE. Writing `(+ seq count -1)` in both
         ;; the reservation and the session's record of it would be two
         ;; suppliers of one range -- and a mutation that changed only
         ;; the first left the session believing it was authorised for a
         ;; stretch the registry had never heard of.
         (let* ((last (+ seq (session-pending-count s) -1))
                (outcome (reserve-range! store (store-id-of store) (instance-nonce store)
                                         writer seq last)))
           (cond
             ((eq? outcome 'reserved)
              (session-authorised-set! s (cons seq last))
              (write-line! s frame line seq target target-path fresh?))
             ((and (pair? outcome) (eq? (car outcome) 'registry-ahead))
              (list 'refused-before-reserve 'registry-ahead))
             ;; A GENERATION THIS INSTANCE OWNS IS GONE FROM THE STORE.
             ;; Nothing inside the store says so -- identity, owner and
             ;; water mark all still agree -- which is why the registry
             ;; lives outside it and why this is checked here rather
             ;; than when the handle opened.
             ((and (pair? outcome) (eq? (car outcome) 'missing-generation))
              (list 'refused-before-reserve 'missing-generation))
             ((and (pair? outcome) (eq? (car outcome) 'registry-inside-store))
              (list 'refused-before-reserve 'registry-inside-store))
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
                          (lambda () (fsync! fd path 'commit))
                          (lambda () (fd-close fd))))
          (barrier! 'after-current-fsync)
          (file-ensure! next-path)
          (barrier! 'after-create-next)
          (let ((fd (fd-open next-path '(write))))
            (dynamic-wind (lambda () (if #f #f))
                          (lambda () (fsync! fd next-path 'commit))
                          (lambda () (fd-close fd))))
          (barrier! 'after-next-fsync)
          (fsync-dir! (writer-directory store writer) 'commit)
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
  (define (read-first-line path) (read-entry-range path 0 4096))

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
                  ;; MARKED BEFORE THE FIRST BYTE, not after the write
                  ;; returns. What the flag has to answer is "may there
                  ;; be bytes on the disk", and a write that raises
                  ;; halfway is the case where the answer is yes and the
                  ;; return value never arrives.
                  ;;
                  ;; AND THE SEGMENT IS COVERED FROM THE SAME MOMENT, for
                  ;; the same reason. `note-touched!` used to run only
                  ;; where the append succeeded, so a partial write left
                  ;; the segment out of the barrier's list: the caller
                  ;; was told `unknown` -- send it again and I will tell
                  ;; you whether it ran -- over bytes that no flush had
                  ;; been asked about. `unknown` is a promise that a
                  ;; resend will FIND them, and an unflushed byte is one
                  ;; a resend may not.
                  (session-write-started-set! s #t)
                  (note-touched! s target)
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
             ;; NO FSYNC HERE ANY MORE. It used to be one per record --
             ;; ten per `insert` on the real corpus, and on macOS each is
             ;; fsync plus F_FULLFSYNC at 5 to 20 ms, which is where 43
             ;; ms a record came from. The unit was wrong, not merely the
             ;; cost: what the store promises is that a REQUEST is
             ;; durable when it is answered, and the barrier at the end
             ;; of the request is where that promise is kept.
             ;;
             ;; SO `committed` HERE MEANS WRITTEN, NOT DURABLE, and the
             ;; two frontiers say which is which: `next-seq` moves now,
             ;; `durable-seq` when the barrier has covered every segment
             ;; this session touched. Nothing outside the session can see
             ;; the difference -- the lock is held -- and nothing inside
             ;; it answers a caller before the barrier.
             ;;
             ;; THE CLOSE MUST NOT REPLACE THE OUTCOME. A close that
             ;; fails after a write would otherwise escape as an
             ;; exception, losing the fact that the record IS on disk --
             ;; and the caller would resubmit it.
             (begin
               (close-quietly fd)
               (trace-event! 'apply (cons writer seq) #f)
               (session-next-seq-set! s (+ 1 seq))
               (note-touched! s target)
               (session-unconfirmed-set! s seq)
               (session-revision-set! s (+ 1 (session-revision s)))
               (list 'committed seq target))))))))

  (define (note-touched! s segment)
    (unless (memv segment (session-touched s))
      (session-touched-set! s (cons segment (session-touched s)))))

  ;; THE END OF A REQUEST, AND THE ONLY PLACE THE DURABLE FRONTIER MOVES.
  ;; Every segment this session wrote into goes through the recovery
  ;; closure; only when all of them have is what was written also
  ;; durable, and only then may a caller be told so.
  ;;
  ;; IT IS NOT CALLED FOR A SESSION THAT WROTE NOTHING. A request refused
  ;; before it reserved anything has made no promise, and putting the
  ;; store's whole recovery closure on the way out of every refusal would
  ;; charge refusals for a promise they do not make.
  (define (session-commit! s)
    (let ((segments (session-touched s))
          (writer (session-writer s)))
      (if (null? segments)
          'nothing-written
          (begin
            (for-each (lambda (seg)
                        (run-barrier! (session-store s) writer seg 'commit 'commit))
                      (list-sort < segments))
            ;; AND THE REGISTRY LEARNS HOW FAR RECORDS REACHED THE DISK.
            ;; It is raised HERE and nowhere else: `written` is the one
            ;; number the rollback gate trusts, so anything that moved it
            ;; before the barrier would be promising on the barrier's
            ;; behalf. One machine-lock section for the whole request --
            ;; a constant, not a cost that grows with the records.
            (note-written! s (- (session-next-seq s) 1))
            (session-durable-seq-set! s (- (session-next-seq s) 1))
            'committed))))

  (define (note-written! s seq)
    (note-written-for! (session-store s) (session-writer s) seq))

  ;; THE SAME RECONCILIATION WITHOUT A SESSION. A replay answers `ok` for
  ;; a record an earlier attempt wrote -- and that attempt may have
  ;; crashed between its barrier and this write, leaving the record
  ;; durable and the registry counting fewer. The replay flushes the
  ;; record and says so; if it did not also reconcile, an
  ;; ACKNOWLEDGED record could afterwards be restored away without the
  ;; rollback gate noticing, because the gate compares `written` with the
  ;; log's end and both would be low.
  ;;
  ;; It only ever raises, so a replay of an old record cannot lower a
  ;; frontier a later request established.
  ;; THE FRONTIER IS RAISED UNDER THE STORE'S REAL NAME OR NOT AT ALL.
  ;; `store-id-of` answers "unknown" when the metadata will not read and
  ;; `instance-nonce` answers #f -- both of which are honest for a reader
  ;; and neither of which is a key. Keyed by one of those, the update
  ;; would match none of the store's real entries and create one under a
  ;; name nothing reads: the write is acknowledged, the store's written
  ;; frontier never moves, and a store later restored to an earlier
  ;; sequence walks past the rollback check that frontier exists to fail.
  ;; The acknowledged request then runs a second time, which is the one
  ;; outcome this whole mechanism exists to prevent. So the identity is
  ;; refused by name before the registry is written.
  ;;
  ;; AN ACKNOWLEDGEMENT CREATES THE COUNT IT NEEDS. Under the store's real
  ;; identity, a record this log holds and the barrier just flushed is
  ;; counted even when no entry exists for (store-id, nonce, writer): after
  ;; an identity adopt every earlier writer is under a nonce the registry
  ;; holds no water mark for (only the adopt's generation), and a replay of
  ;; its record would otherwise never be answered. The writer may be a mirror: the entry records that this
  ;; instance acknowledged held history, not ownership -- it creates no
  ;; owner file and no generation record and lets no session write as that
  ;; writer. It is scoped to this instance's nonce, so a copy under another
  ;; instance never touches these entries. Called only AFTER the flush
  ;; returned (store.sc's barrier-for), so a failed flush creates nothing.
  ;; resume-uncertain! reads the entry's `authorised` like a reservation's.
  ;;
  ;; NEVER: AN ENTRY IS CREATED ONLY FOR AN INSTANCE THIS MACHINE MINTED.
  ;; Two conditions, and each refuses a case the other lets through:
  ;; - verify-instance answers ok. "This instance's nonce" is the key only
  ;;   when the instance really is this one: a copy that kept another
  ;;   store's instance.sexp carries that store's nonce, and counting under
  ;;   it would write into the other instance's entries.
  ;; - this machine's registry holds a generation record for (store-id,
  ;;   nonce): the adopt that minted the instance recorded it here, which is
  ;;   the witness that the registry has not been lost since. A registry
  ;;   that lost its entries (and so its generations) must not be re-seeded
  ;;   by a replay: the entry would start at the replayed record, and a
  ;;   store later restored to an earlier sequence would walk past the
  ;;   rollback check the lost entry existed to fail.
  ;; Otherwise the acknowledgement is refused by name, as it was before
  ;; entries could be created. An entry that exists is raised as before,
  ;; and that is the common case, so the checks are paid only when there is
  ;; none: the raise is tried under the machine lock first, and only an
  ;; absent entry leaves the lock, verifies the instance and takes it again
  ;; to look for the generation and create (an entry another process
  ;; created meanwhile is then raised by max, as any other). verify-instance
  ;; is asked OUTSIDE the machine lock: on a fresh machine home it mints the
  ;; machine id under that same lock.
  (define (note-written-for! store writer seq)
    ;; A RENDEZVOUS BEFORE THE IDENTITY IS READ, because no fault can
    ;; reach these two reads. They go through `read-whole`, which opens a
    ;; Chez port directly rather than the injected `fd-open`, so
    ;; `open-fail` passes straight over them -- and the state this guard
    ;; exists for needs the metadata to read at open and fail HERE. A
    ;; case parks the process at this point, moves the file aside, and
    ;; releases it.
    (barrier! 'written-identity-read)
    (let ((id (store-id-of store))
          (nonce (instance-nonce store)))
      (when (or (string=? id "unknown") (not nonce))
        (assertion-violation
          'note-written-for!
          "cannot raise the written frontier without the store's identity"
          (list store (list 'store-id id) (list 'instance nonce))))
      (let ((raised?
              (parameterize ((current-machine-home (machine-home)))
                (with-machine-lock
                  (lambda ()
                    (let ((reg (read-registry)))
                      (and (registry-water-mark reg id nonce writer)
                           (begin (write-registry! (registry-note-written reg id nonce writer seq))
                                  #t))))))))
        (unless raised?
          (unless (eq? (verify-instance store) 'ok)
            (assertion-violation
              'note-written-for!
              "no registry entry to raise the written frontier on"
              (list store id nonce writer seq)))
          (parameterize ((current-machine-home (machine-home)))
            (with-machine-lock
              (lambda ()
                (let ((reg (read-registry)))
                  (unless (or (registry-water-mark reg id nonce writer)
                              (pair? (generations-of reg id nonce)))
                    (assertion-violation
                      'note-written-for!
                      "no registry entry to raise the written frontier on"
                      (list store id nonce writer seq)))
                  (write-registry! (registry-acknowledge-written reg id nonce writer seq))))))))))

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
  ;; A LOAD WITH AN UNREADABLE WRITER TAKES NO SNAPSHOT BASELINE. Choosing
  ;; one reads every writer's end, and an unreadable writer has none; the
  ;; load replays instead, and its caller acknowledges or refuses the
  ;; replay like any other incomplete one. The origin is asked first, so no
  ;; coordinate of that writer is read here.
  (define (select-snapshot store prefixes)
    (if (exists (lambda (e) (eq? (discovery-origin (cdr e)) 'unreadable)) prefixes)
        (list #f #f 'writer-unreadable)
        (select-readable-snapshot store prefixes)))

  (define (select-readable-snapshot store prefixes)
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

  ;; A resident reduction may be reused only against the same authenticated
  ;; source bytes and metadata, while this read session holds the store lock.
  (define (load-fingerprint ls)
    (guard (e (#t #f))
      (let ((store (load-session-store ls)))
        (list (metadata-versions store)
              ;; AN UNREADABLE WRITER CONTRIBUTES WHAT IS KNOWN OF IT -- the
              ;; path and the reason -- so the fingerprint differs from every
              ;; one taken while the writer was readable, and no reuse
              ;; follows; its coordinates are not asked.
              (map (lambda (entry)
                     (let ((writer (car entry)) (p (cdr entry)))
                       (if (eq? (discovery-origin p) 'unreadable)
                           (let ((d (log-error-detail (unreadable-note p))))
                             (list writer 'unreadable (cdr (assq 'path d)) (cdr (assq 'reason d))
                                   (cdr (assq 'errno d))))
                       (list writer (discovery-origin p) (discovery-end-seq p)
                             (discovery-integrity p) (discovery-quarantine p) (discovery-retired p)
                             (map (lambda (range)
                                    (let ((bytes (read-segment store writer (car range))))
                                      (unless (bytevector? bytes) (raise 'unreadable-resident-source))
                                      (list range (segment-sha bytes)))) (discovery-segment-ranges p))))))
                   (load-session-prefixes ls))))))

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

  ;; A LOAD ABORTED BY AN ENTRY IT COULD NOT READ IS REFUSED BY THAT ENTRY
  ;; (F77c), so the table names it `unreadable`. ONE RULE, two places that
  ;; would otherwise use the load: a reader's load-commit! and a session's
  ;; log-begin (plan amendment A3, the build half of F124). Any other
  ;; aborted load goes on as it always has -- a reader meets
  ;; check-terminal!'s assertion, a session continues and the publication
  ;; gate withholds it: a readable cause (damaged, short or torn bytes) is
  ;; F125's, not this item's.
  (define (refuse-unreadable-abort! ls)
    (let ((o (load-session-outcome ls)) (cause (load-session-abort-cause ls)))
      (when (and (pair? o) (eq? (car o) 'aborted) (unreadable-entry? cause))
        (raise cause))))

  (define (load-commit! ls)
    (refuse-unreadable-abort! ls)
    (check-terminal! 'load-commit! ls)
    (load-session-outcome-set! ls 'committed)
    (release-load! ls)
    (load-session-state ls))

  (define (load-abort! ls reason)
    (check-terminal! 'load-abort! ls)
    (finish-abort! ls reason))

  ;; EVERY ABORT PASSES HERE, and this is where its cause is kept and where
  ;; a load stopped by an entry it could not read tells its listener so
  ;; (F77c) -- BEFORE the release, so a release that fails cannot skip the
  ;; notification (design review r6). The returned outcome is unchanged.
  (define (finish-abort! ls reason . cause-opt)
    (let ((cause (and (pair? cause-opt) (car cause-opt))))
      (load-session-abort-cause-set! ls cause)
      (when (unreadable-entry? cause)
        (tell-load-refused! (load-session-store ls) cause)))
    (load-session-state-set! ls '())
    (load-session-outcome-set! ls (list 'aborted reason))
    (release-load! ls)
    (load-session-outcome ls))

  (define (release-load! ls)
    (let ((l (load-session-lock ls)))
      (when l (load-session-lock-set! ls #f) ((current-lock-release) l))))

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
  ;; HOLDING A LOCK DOES NOT ESTABLISH DURABILITY. A writer that died left
  ;; an unflushed tail; another process takes the SHARED lock, reads
  ;; those bytes out of the page cache, delivers them, and computes a cut
  ;; over them -- and a power cut then takes them away. The reader has
  ;; promised a history that never reached the disk, which is the same
  ;; promise `log-begin` makes with its takeover barrier and which every
  ;; reader was making without one.
  ;;
  ;; BOTH BARRIERS, OR NEITHER. The records are one half; the manifest
  ;; and the quarantine marker are the other, because they are what admit
  ;; a segment to a writer's history -- a reader whose manifest is
  ;; unflushed delivers history a crash can un-admit. A shared-lock
  ;; reader and an exclusive writer do not coexist, so flushing a dead
  ;; writer's residue from the read side is safe.
  ;;
  ;; A FAILURE ABORTS THE DELIVERY. A reader that cannot make what it is
  ;; about to deliver durable has nothing it may say about it, and one
  ;; that skipped the flush and delivered anyway would be the whole
  ;; defect again with a warning attached.
  (define (deliver-barrier! ls cut)
    (let ((store (load-session-store ls)))
      (takeover-barrier! store (load-session-prefixes ls) cut)
      (metadata-flush! store 'deliver-barrier
                       (lambda (w name) (remembered-version ls w name))
                       (lambda (w name version) (remember-version! ls w name version)))))

  (define (remembered-version ls w name)
    (let loop ((es (load-session-barriered ls)))
      (cond ((null? es) #f)
            ((and (equal? (car (car es)) w) (equal? (cadr (car es)) name))
             (caddr (car es)))
            (else (loop (cdr es))))))

  (define (remember-version! ls w name version)
    (load-session-barriered-set!
      ls
      (cons (list w name version)
            (remp (lambda (e) (and (equal? (car e) w) (equal? (cadr e) name)))
                  (load-session-barriered ls)))))

  ;; THE FAILURE ESCAPES; IT IS NOT AN OUTCOME TO BE IGNORED. A caller
  ;; that read a return value would be free not to, and would then go on
  ;; writing on top of a state whose delivery never happened -- which is
  ;; worse than the defect this barrier was added for. The load is marked
  ;; aborted, its lock released, and the condition raised.
  (define (load-deliver! ls cut on-deliver)
    (check-terminal! 'load-deliver! ls)
    (guard (e (#t (finish-abort! ls 'deliver-barrier-failed e) (raise e)))
      (deliver-barrier! ls cut))
    ;; THE WINDOW BETWEEN THE BARRIER AND DELIVERY, for the rows that change
    ;; a segment after the barrier has read it (F77c D-deliver).
    (hold-point! 'after-barrier)
    (deliver-after-barrier! ls cut on-deliver))

  (define (deliver-after-barrier! ls cut on-deliver)
    (load-session-outcome-set! ls 'delivering)
    (let ((finished (vector #f))
          ;; THE CONDITION THAT ESCAPED, kept for finish-abort! (design
          ;; review r5): the unwind below sees no condition of its own. A
          ;; continuation that leaves without one keeps #f.
          (escaped (vector #f)))
      (dynamic-wind
        (lambda () (if #f #f))
        (lambda ()
          (let ((r (guard (e (#t (vector-set! escaped 0 e) (raise e)))
                     (deliver-all ls cut on-deliver))))
            (vector-set! finished 0 #t)
            r))
        (lambda ()
          (unless (vector-ref finished 0)
            (when (eq? (load-session-outcome ls) 'delivering)
              (finish-abort! ls 'delivery-escaped (vector-ref escaped 0))))))))

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
           ;; THE RETURNED VALUE IS UNCHANGED (test/log7.sc pins it); the
           ;; cause travels to finish-abort! beside it (F77c).
           (if (eq? r 'ok)
               (loop (cdr ws))
               (begin
                 (finish-abort! ls (list 'delivery-failed writer) (cadr r))
                 (list 'delivery-failed writer))))))))

  (define (segment-holding ranges seq)
    (let loop ((rs ranges))
      (cond
        ((null? rs) #f)
        ((and (<= (cadr (car rs)) seq) (<= seq (caddr (car rs)))) (caar rs))
        (else (loop (cdr rs))))))

  ;; AN UNREADABLE WRITER DELIVERS NOTHING, and the load goes on to the
  ;; others; the origin is asked before any coordinate is. Its note is in
  ;; the load's integrity, where everything that reports the load finds it.
  (define (deliver-writer ls writer p from on-deliver)
    (if (eq? (discovery-origin p) 'unreadable)
        'ok
        (deliver-readable-writer ls writer p from on-deliver)))

  (define (deliver-readable-writer ls writer p from on-deliver)
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
                        ;; A FAILURE CARRIES ITS CAUSE to deliver-all (F77c):
                        ;; the entry that could not be read, rebuilt as the
                        ;; condition ffi raises, or the scanner's verdict.
                        ((unreadable-segment? bytes)
                         (list 'failed (make-unreadable-entry (cadr bytes) (caddr bytes) (cadddr bytes))))
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
                             (else (list 'failed (list 'scanner outcome needed)))))))))))))))
))
)
