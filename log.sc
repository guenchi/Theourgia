#!chezscheme
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
  (export snapshot-write! snapshot-read snapshot-cut-supported?
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
          (only (theourgia wire)
                sexpr->string-extended string->sexpr-extended decode-line))

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
  (define (atomic-write! path bytes)
    (unless (string? path)
      (assertion-violation 'atomic-write! "path must be a string" path))
    (unless (bytevector? bytes)
      (assertion-violation 'atomic-write! "contents must be a bytevector" bytes))
    (let ((tmp (temp-name-for path))
          (dir (parent-directory path)))
      (let ((fd (guard (e (#t (raise e)))
                  (fd-open tmp '(write create)))))
        (guard (e (#t
                   (guard (e2 (#t (void))) (fd-close fd))
                   (guard (e2 (#t (void))) (delete-file tmp))
                   (raise e)))
          (write-all! fd bytes tmp)
          (fsync! fd tmp))
        (fd-close fd))
      (rename-file tmp path)
      (fsync-dir! dir)
      path))

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

  (define (enumerate-segment-files store writer)
    (let ((dir (writer-directory store writer)))
      (if (not (file-directory? dir))
          '()
          (sort < (filter (lambda (n) n)
                          (map segment-file-number (directory-list dir)))))))

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
           (let ((text (call-with-port (open-file-input-port path)
                         (lambda (p) (utf8->string (get-bytevector-all p))))))
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

  (define (write-manifest! store writer entries)
    (unless (valid-manifest? entries)
      (assertion-violation 'write-manifest!
                           "entries must be ascending (number . hash) pairs" entries))
    (atomic-write! (manifest-path store writer)
                   (string->utf8 (string-append (sexpr->string-extended entries) "\n"))))

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
    (let* ((body (call-with-string-output-port
                   (lambda (p)
                     (put-string p (sexpr->string-extended (list 'snapshot 1 cut)))
                     (put-char p #\newline)
                     (for-each (lambda (r)
                                 (put-string p (sexpr->string-extended r))
                                 (put-char p #\newline))
                               rows))))
           (bytes (string->utf8 body))
           (whole (string-append body
                                 (sexpr->string-extended
                                   (list 'end (crc32-hex bytes)))
                                 "\n")))
      (atomic-write! path (string->utf8 whole))))

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
        (let* ((bytes (call-with-port (open-file-input-port path) get-bytevector-all))
               (bytes (if (eof-object? bytes) (make-bytevector 0) bytes))
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
)
