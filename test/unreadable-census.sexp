;; The pin for unreadable-census.sc. Each entry is read from the source by
;; that file's scanner; what the scanner cannot read -- the category, the
;; segment and the reason -- is written here by hand, and the rows compare
;; everything else with the source.
;;
;; presence: (file enclosing-definitions name count action segment reason)
;;   action is keep (not a writer-layout path), binding (an import or
;;   export of the name), convert (a writer-layout question the design
;;   converts) or out-of-scope; segment is the one that converts it --
;;   an entry of segment b or c is a question still asked today.
;;
;; handlers: (file enclosing-definitions ordinal kind catches category
;;            segment note body)
;;   ordinal counts the handlers inside the same enclosing definitions in
;;   source order; catches is each guard clause's test, else as #t; body is
;;   the handler's own part of the form, (variable clause ...), read as
;;   data. category is the R1a decision; segment is the one that makes the
;;   body carry it out -- for b and c the body pinned is today's, and that
;;   segment will change it and this entry with it.
;;
;; raw-accessors: the discovery record's fields as written, and for each
;;   raw accessor the one wrapper allowed to use it.

(presence
  ("build.ss" () file-directory? 2 keep a "build.ss: build output and source paths, not writer layout")
  ("build.ss" (libraries) file-directory? 1 keep a "build.ss: build output and source paths, not writer layout")
  ("build.ss" (copy-programs!) file-directory? 1 keep a "build.ss: build output and source paths, not writer layout")
  ("build.ss" (copy-programs!) file-exists? 2 keep a "build.ss: build output and source paths, not writer layout")
  ("code-project.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("code-project.sc" () file-is-regular? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("code-project.sc" (directory-files walk) file-is-directory? 1 keep a "not writer layout: import-code input tree")
  ("code-project.sc" (directory-files walk) file-is-regular? 1 keep a "not writer layout: import-code input tree")
  ("code-project.sc" (directory-files) file-is-directory? 1 keep a "not writer layout: import-code input dir")
  ("daemon.sc" () file-is-regular? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("daemon.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("evidence-index.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("evidence-index.sc" (files) file-is-directory? 1 convert c "evidence inventory lists writer files (R2f)")
  ("ffi.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("ffi.sc" () file-is-regular? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("ffi.sc" (hold-point!) file-exists? 1 keep a "the hold seam's release file (injection only, F100b item 7): not writer layout; a test's own marker")
  ("ffi.sc" (file-is-directory?) file-is-directory? 1 convert a "the predicate itself: now entry-type, so an unreadable path raises (R1)")
  ("ffi.sc" (file-is-regular?) file-is-regular? 1 convert a "the predicate itself: now entry-type, so an unreadable path raises (R1)")
  ("ffi.sc" (mkdir-p!) file-is-directory? 2 convert a "generic helper mkdir-p!: the predicate is now entry-type and raises on an unreadable path (R1: converted unconditionally)")
  ("ffi.sc" (mkdir-one!) file-is-directory? 1 convert a "generic helper mkdir-one!: the predicate is now entry-type and raises on an unreadable path (R1)")
  ("ffi.sc" (rss-linux) file-exists? 1 keep a "not writer layout: /proc on linux")
  ("ffi.sc" (path-version) file-is-regular? 1 convert a "generic helper path-version: the predicate is now entry-type and raises on an unreadable path (R1)")
  ("log.sc" (ensure-machine-home!) file-is-directory? 1 keep a "not writer layout: machine home")
  ("log.sc" (ensure-directory!) file-is-directory? 1 convert b "ensure-directory! under the writer (incoming/quarantine)")
  ("log.sc" (ensure-writer-directory!) file-is-directory? 1 convert b "ensure-writer-directory! (publish)")
  ("log.sc" (install-snapshot!) file-is-directory? 1 keep a "not writer layout: snapshot directory; creation is refused on an incomplete reduction in c")
  ("log.sc" (select-readable-snapshot) file-is-directory? 1 keep a "not writer layout: the snapshot directory; select-snapshot answers writer-unreadable before this when any writer is unreadable (K8)")
  ("project.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("project.sc" (md-files) file-is-directory? 1 keep a "not writer layout: import-md input dir")
  ("project.sc" (require-md-directory) file-is-directory? 1 keep a "not writer layout: export-md/import-md refuse a path that is not a directory (F82, F83)")
  ("store.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("working.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  )

(handlers
  ("build.ss" (declares-a-library?) 1 guard
   (#t)
   unrelated a
   "build.ss: reading a build input"
   (e (#t #f)))
  ("build.ss" () 1 guard
   (#t)
   unrelated a
   "build.ss: build failure bookkeeping"
   (e (#t (sweep (cdr fs) (cons (car fs) deferred) built (cons (cons (name-of (car fs)) (if (and (condition? e) (message-condition? e)) (condition-message e) "?")) errs)))))
  ("core.sc" (forward-then-exit!) 1 guard
   (#t)
   unrelated a
   "socket forwarding"
   (e (#t (quote unreadable))))
  ("theourgiad.sc" (detach-step) 1 guard
   (#t)
   fact a
   "detach-failed, a named outcome (F100b item 5): the step, the log path and the errno, with the scope's record through with-written -- the log's creation when its open failed after it (NO4b), nothing when its directory could not be searched (NO4) -- written on STDERR (E2); exit 71"
   (e (#t (let ((code (detach-errno e))) (trace-event! (quote detach-failed) code #f) (guard (e2 (#t (trace-event! (quote detach-report-failed) #f #f))) (let ((err (current-error-port))) (write (with-written (list (quote error) (quote detach-failed) (list (quote step) step) (list (quote path) log-path) (list (quote errno) code)) (mutation-record)) err) (newline err) (flush-output-port err)))) (exit 71))))
  ("client.sc" (call-on-socket) 1 guard
   ((fs-error? e))
   unrelated a
   "socket"
   (e ((fs-error? e) (let ((code (fs-error-errno e))) (if (no-daemon-errno? code) (list (quote no-daemon) code) (list (quote not-sent) (list (quote error) (quote connect-failed) (list (quote path) path) (list (quote errno) code))))))))
  ("client.sc" (call-on-socket) 2 guard
   ((fs-error? e))
   unrelated a
   "socket"
   (e ((fs-error? e) (close-noting-failure fd) (list (quote transport-error) (fs-error-errno e)))))
  ("client.sc" (close-noting-failure) 1 guard
   ((fs-error? e))
   unrelated a
   "socket close"
   (e ((fs-error? e) (trace-event! (quote close-failed) (fs-error-errno e) #f) #f)))
  ("client.sc" (exchange-on) 1 guard
   ((and (fs-error? e) (= sent 0)))
   unrelated a
   "socket write"
   (e ((and (fs-error? e) (= sent 0)) (list (quote not-sent) (list (quote error) (quote write-failed) (list (quote path) path) (list (quote errno) (fs-error-errno e)))))))
  ("client.sc" (start-one!) 1 guard
   ((classify-failure e (quote ())))
   refuse a
   "F100b point 6 (M2 Q6): a filesystem failure of the client's own step (the log's length, the run directory, the spawn) is the table's answer over an EMPTY record, as serve-start-failed with (kind K); the client's own entries go only in client-written; anything else propagates"
   (e ((classify-failure e (quote ())) => (lambda (a) (own-start-failure a)))))
  ("client.sc" (connects?) 1 guard
   ((fs-error? e))
   unrelated a
   "socket connect"
   (e ((fs-error? e) #f)))
  ("client.sc" (socket-dir-refusal) 1 guard
   ((unreadable-entry? e))
   refuse a
   "socket-dir-refusal: a --socket whose directory cannot be searched answers (error unreadable (path ...) (reason ...)) before a daemon is started or anything created (F15)"
   (e ((unreadable-entry? e) (list (quote error) (quote unreadable) (list (quote path) (unreadable-entry-path e)) (list (quote reason) (unreadable-entry-reason e))))))
  ("project.sc" (export-md) 1 guard
   ((unreadable-entry? e) (fs-error? e))
   refuse a
   "export-md: a target whose listing, directory creation or file write fails -- the unreadable-entry and the door's durable-error this guard catches -- answers (error unreadable (path ...) (reason ...) (written ...)), not internal (F98; the durable clause replaced an i/o-error clause when the writes went through the door). Filed as REFUSE, and it differs from that category's definition (refused before any mutation) in one way: files already written to the export target stay there, and (written ...) names them. export-md does not write the store"
   (e ((unreadable-entry? e) (raise (unwritten (unreadable-entry-path e) (unreadable-entry-reason e) written))) ((fs-error? e) (raise (unwritten (fs-error-target e) (durable-failure-reason e) written)))))
  ("code-project.sc" (answer) 1 guard
   ((and (list? e) (pair? e) (eq? (car e) (quote error))))
   unrelated a
   "catches error lists only, never a condition"
   (e ((and (list? e) (pair? e) (eq? (car e) (quote error))) e)))
  ("code-project.sc" (capture-import) 1 guard
   (#t)
   propagate c
   "K2: capture-import lets incomplete-reduction and unreadable-entry through"
   (e (#t (projection-failure (quote unavailable-cut)))))
  ("code-suggest.sc" (split-suggest) 1 guard
   ((and (pair? e) (eq? (car e) (quote error))))
   unrelated a
   "error lists"
   (e ((and (pair? e) (eq? (car e) (quote error))) e)))
  ("code-suggest.sc" (split-suggest) 2 guard
   (#t)
   unrelated a
   "split-suggest input"
   (e (#t #f)))
  ("daemon.sc" (device-inode) 1 guard
   ((and (unreadable-entry? e) (memq (unreadable-entry-errno e) (quote (ENOENT ENOTDIR)))))
   fact a
   "socket device/inode: only an absent entry (ENOENT, ENOTDIR) is no inode; any other unreadable-entry, and anything else, propagates to the caller's table (F100b P2b; the other callers are M2's)"
   (e ((and (unreadable-entry? e) (memq (unreadable-entry-errno e) (quote (ENOENT ENOTDIR)))) #f)))
  ("daemon.sc" (main) 1 guard
   ((classify-failure e (mutation-record)))
   refuse a
   "F100b point 9: main's startup (the socket directory, the lock file, the lock, the socket-path probe) answers a filesystem failure by the one table with main's record; the answer is decided here and reported ONCE outside the scope by report-and-exit!, exit 75 (P9 a-c; M2a review r1, F1); anything else propagates"
   (e ((classify-failure e (mutation-record)) => (lambda (answer) (list (quote refuse) answer)))))
  ("daemon.sc" (watch-loop) 1 guard
   (#t)
   conservative a
   "F100b point 2 (M2a review r1, F1/F3): a combined startup report that cannot be written is traced, not reported again; the terminal startup-failed clause then writes the exiting line and leaves 75 itself (the startup-exit design)"
   (e (#t (trace-event! (quote startup-report-failed) #f #f))))
  ("daemon.sc" (unlink-own-socket!) 1 guard
   (#t)
   unrelated a
   "socket"
   (e (#t #f)))
  ("daemon.sc" (clear-stale-socket!) 1 guard
   (#t)
   unrelated a
   "socket"
   (e (#t #f)))
  ("daemon.sc" (probe-for-outside-change!) 1 guard
   (#t)
   fact c
   "refresh guard: failed snapshot visible in the answer (R1a)"
   (e (#t was)))
  ("daemon.sc" (store-loop) 1 guard
   (#t)
   propagate c
   "F100b point 2: the store process's startup scope hands every failure, with the scope's record, to the code after it, which sends (startup-failed <answer> <record>) to main and raises it again (P2-startup and its TWIN, F79-3); main writes the report. c makes the daemon declare acceptance of an incomplete reduction"
   (e (#t (cons e (mutation-record)))))
  ("daemon.sc" (store-loop) 2 guard
   (#t)
   fact c
   "refresh guard: failed reload visible (R1a)"
   (e (#t (if #f #f))))
  ("daemon.sc" (answer-for) 1 guard
   ((and (pair? e) (eq? (quote error) (car e))) (unreadable-entry? e) #t)
   refuse c
   "K1: the unreadable-entry branch answers (error unreadable (path p) (reason r)), added in F77a; the incomplete-reduction branch is F77c; neither is (error internal ...)"
   (e ((and (pair? e) (eq? (quote error) (car e))) e) ((unreadable-entry? e) (list (quote error) (quote unreadable) (list (quote path) (unreadable-entry-path e)) (list (quote reason) (unreadable-entry-reason e)))) (#t (list (quote error) (quote internal) (list (quote reason) (condition-text e))))))
  ("daemon.sc" (listener-loop) 1 guard
   (#t)
   unrelated a
   "listener"
   (e (#t (if #f #f))))
  ("daemon.sc" (ctx-drain!) 1 guard
   (#t)
   unrelated a
   "drain"
   (e (#t (if #f #f))))
  ("daemon.sc" (request-shape-error) 1 guard
   (#t)
   unrelated a
   "request shape"
   (e (#t #f)))
  ("daemon.sc" (parse-frame) 1 guard
   (#t)
   unrelated a
   "frame parse"
   (e (#t (quote not-a-datum))))
  ("datum-code.sc" (datum-source-read) 1 guard
   ((and (pair? e) (eq? (car e) (quote error))) #t)
   unrelated a
   "reader"
   (e ((and (pair? e) (eq? (car e) (quote error))) (raise e)) (#t (raise (quote (error bad-source (reason reader-rejected)))))))
  ("datum-metadata.sc" (datum-doc-marker?) 1 guard
   (#t)
   unrelated a
   "datum marker"
   (e (#t #f)))
  ("datum-project.sc" (answer) 1 guard
   ((and (pair? e) (eq? (car e) (quote error))))
   unrelated a
   "error lists"
   (e ((and (pair? e) (eq? (car e) (quote error))) e)))
  ("datum-project.sc" (control) 1 guard
   ((and (pair? e) (equal? (assq (quote reason) (filter pair? e)) (quote (reason invalid-marker)))))
   unrelated a
   "error lists"
   (e ((and (pair? e) (equal? (assq (quote reason) (filter pair? e)) (quote (reason invalid-marker)))) #f)))
  ("eval-supervise.sc" (first-datum) 1 guard
   (#t)
   unrelated a
   "eval"
   (e (#t #f)))
  ("eval-supervise.sc" (protocol-datums) 1 guard
   (#t)
   unrelated a
   "eval: a protocol line the worker wrote that no reader accepts ends the list of what it said (review r4, A1-1; it was finish's single read)"
   (e (#t (eof-object))))
  ("eval-supervise.sc" (absorb-frames) 1 guard
   (#t)
   unrelated a
   "eval"
   (e (#t #f)))
  ("eval-worker.sc" (answer) 1 guard
   ((worker-refusal? e) (classify-failure e (mutation-record)) (condition? e) #t)
   refuse a
   "eval: the worker's own refusals are a private record answered as they are (F92); a filesystem failure -- an entry the worker's load could not read, writers/ itself included (F79), or a durable-error -- answers by the one table with the worker's record (F100b point 5); a condition is eval-exception with the fixed message, and any other raised value is carried as data by raised-answer (F92, F93)"
   (e ((worker-refusal? e) (worker-refusal-answer e)) ((classify-failure e (mutation-record)) => (lambda (a) a)) ((condition? e) (quote (error eval-exception (kind raised) (message "Evaluation raised an exception")))) (#t (raised-answer e))))
  ("eval-worker.sc" (worker-step) 1 guard
   ((and (pair? e) (eq? (car e) (quote error))) (or (worker-refusal? e) (unreadable-entry? e) (fs-error? e) (condition? e)) #t)
   propagate a
   "eval: what the worker's own steps raise (the reader of the source and the cut, the store's load, the library lookup) answers as before F92: a list headed error is the refusal it names; a worker-refusal, an unreadable-entry, a durable-error (F100b) or a condition is re-raised to answer's clauses; anything else (log-error, a record) is the fixed-message answer, never a value the source raised (F92 review r1, S)"
   (e ((and (pair? e) (eq? (car e) (quote error))) (refuse! e)) ((or (worker-refusal? e) (unreadable-entry? e) (fs-error? e) (condition? e)) (raise e)) (#t (refuse! (quote (error eval-exception (kind raised) (message "Evaluation raised an exception")))))))
  ("evidence-index.sc" (load-checkpoint) 1 guard
   (#t)
   unrelated c
   "load-checkpoint reads the store-level index checkpoint, not a writer file"
   (e (#t #f)))
  ("evidence-index.sc" (verified-record) 1 guard
   (#t)
   propagate c
   "verified-record reads a writer segment; unreadable-entry propagates so the index answers unknown (R2f); a failed read of a readable file keeps #f"
   (failure (#t #f)))
  ("ffi.sc" (file-create-exclusive!) 1 guard
   ((i/o-file-already-exists-error? e) (fs-error? e) (unreadable-entry? e) #t)
   propagate a
   "re-raises; an existing name is #f; any other failure of the create is durable-error with the errno mapped from Chez's condition (F100a)"
   (e ((i/o-file-already-exists-error? e) #f) ((fs-error? e) (raise e)) ((unreadable-entry? e) (raise e)) (#t (raise (fs-err (quote create) path (condition-errno e))))))
  ("ffi.sc" (close-unwritten-port!) 1 guard
   ((unreadable-entry? e) #t)
   propagate a
   "the close of a port Chez opened to create a file and never wrote: its failure is raised again as unreadable-entry naming the path (F100a review r1, D6)"
   (e ((unreadable-entry? e) (raise e)) (#t (chez-unreadable! path e))))
  ("ffi.sc" (call-with-lock) 1 guard
   (#t)
   unrelated a
   "cleanup on an escape only: the lock's release is quiet so the condition already leaving is the one reported; on a normal exit the release is checked and raises (header obligation a; F100a review r1, D7)"
   (e (#t (void))))
  ("ffi.sc" (rss-linux) 1 guard
   (#t)
   unrelated a
   "/proc"
   (e (#t #f)))
  ("ffi.sc" (redirect-stdio!) 1 guard
   (#t)
   propagate a
   "re-raises after cleanup"
   (e (#t (when null-fd (fd-close null-fd)) (when log-fd (fd-close log-fd)) (raise e))))
  ("ffi.sc" (unix-socket-connect) 1 guard
   (#t)
   propagate a
   "re-raises after close"
   (e (#t (c-close fd) (raise e))))
  ("ffi.sc" (fault-announced) 1 guard
   (#t)
   unrelated a
   "fault announcement"
   (e (#t (void))))
  ("ffi.sc" (barrier!) 1 guard
   (#t)
   unrelated a
   "barrier injection"
   (e (#t (void))))
  ("ffi.sc" (file-ensure-body!) 1 guard
   ((fs-error? e) (unreadable-entry? e) #t)
   propagate a
   "re-raises; unreadable-entry passes unchanged; the create's own failure is durable-error with the errno mapped from Chez's condition (F100a)"
   (e ((fs-error? e) (raise e)) ((unreadable-entry? e) (raise e)) (#t (raise (fs-err (quote create) path (and (condition? e) (condition-errno e)))))))
  ("ffi.sc" (path-version) 1 guard
   ((unreadable-entry? e) #t)
   propagate a
   "path-version's times: Chez's condition is raised again as unreadable-entry naming the path, errno mapped (F100a D1)"
   (e ((unreadable-entry? e) (raise e)) (#t (let ((code (and (condition? e) (condition-errno e)))) (raise (make-unreadable-entry path (if code (c-strerror code) "the entry's times cannot be read") (and code (errno-reason code))))))))
  ("ffi.sc" (real-path) 1 guard
   (#t)
   unrelated a
   "real-path callers: client.sc store resolution and project.sc file-key (export dir); none under writers/<w>/"
   (e (#t #f)))
  ("ffi.sc" (file-is-socket?) 1 guard
   (#t)
   unrelated a
   "socket type"
   (e (#t #f)))
  ("ffi.sc" (lock-acquire!) 1 guard
   (#t)
   propagate a
   "re-raises"
   (e (#t (close-quietly fd) (raise e))))
  ("ffi.sc" (lock-try-acquire!) 1 guard
   (#t)
   propagate a
   "re-raises"
   (e (#t (close-quietly fd) (raise e))))
  ("log.sc" (present-or-unreadable-skip?) 1 guard
   ((unreadable-entry? e))
   conservative a
   "F77b R2g, by ruling (F100a): three sites, one rule -- log.sc's local-writer-name and barrier-artefacts and working.sc's writer-for read an owner.sexp or retired.sexp that cannot be stat'ed as absent, as the native presence test they replace did; the only places an unreadable entry still reads as absent"
   (e ((unreadable-entry? e) #f)))
  ("log.sc" (atomic-write!) 1 guard
   (#t)
   propagate b
   "atomic-write! re-raises after cleanup"
   (e (#t (when (unbox open?) (set-box! open? #f) (guard (e2 (#t (void))) (fd-close fd))) (guard (e2 (#t (void))) (unlink! tmp)) (raise e))))
  ("log.sc" (atomic-write!) 2 guard
   (#t)
   unrelated a
   "cleanup inside a re-raising handler"
   (e2 (#t (void))))
  ("log.sc" (atomic-write!) 3 guard
   (#t)
   unrelated a
   "cleanup inside a re-raising handler"
   (e2 (#t (void))))
  ("log.sc" (atomic-write!) 4 guard
   (#t)
   unrelated a
   "cleanup"
   (e (#t (void))))
  ("log.sc" (path-snapshot) 1 guard
   ((unreadable-entry? e))
   fact a
   "path-snapshot: an unreadable path is the marker (unreadable path reason) (R2c)"
   (e ((unreadable-entry? e) (unreadable-marker e))))
  ("log.sc" (path-snapshot) 2 guard
   ((fs-error? e2))
   unrelated a
   "an fs-error from path-version after entry-type answered a type: the path went between the two stats, and the answer is #f; unreadable-entry is not an fs-error and reaches the marker clause"
   (e2 ((fs-error? e2) #f)))
  ("log.sc" (directory-snapshot) 1 guard
   ((unreadable-entry? e))
   fact a
   "directory-snapshot: the marker (unreadable path reason) (R2c)"
   (e ((unreadable-entry? e) (unreadable-marker e))))
  ("log.sc" (store-state-snapshot) 1 guard
   ((unreadable-entry? e))
   fact a
   "store-state-snapshot: a writer that cannot be read is the marker, whole (R2c)"
   (e ((unreadable-entry? e) (unreadable-marker e))))
  ("log.sc" (read-manifest) 1 guard
   (#t)
   unrelated a
   "read-manifest: guards the utf8 decode of bytes already read; the read (read-entry) is outside and raises"
   (e (#t (list (quote unreadable) (unreadable-reason e)))))
  ("log.sc" (read-manifest) 2 guard
   (#t)
   unrelated a
   "read-manifest: guards the parse of bytes already read; bad raises the manifest integrity error"
   (e (#t (quote bad))))
  ("log.sc" (snapshot-read) 1 guard
   (#t)
   unrelated a
   "snapshot parse"
   (e (#t (quote bad))))
  ("log.sc" (snapshot-read) 2 guard
   (#t)
   unrelated a
   "snapshot parse"
   (e (#t (quote bad))))
  ("log.sc" (quarantine-of) 1 guard
   (#t)
   unrelated a
   "quarantine-of: guards the parse of bytes already read; the read (read-entry) is outside and raises to discovery (R2)"
   (e (#t #f)))
  ("log.sc" (retired-of) 1 guard
   (#t)
   unrelated a
   "retired-of: guards the parse of bytes already read and answers malformed; the read is outside and raises to discovery (R2)"
   (e (#t (quote malformed))))
  ("log.sc" (discover-prefix) 1 guard
   ((unreadable-entry? e))
   fact a
   "discovery: origin unreadable and one metadata-unreadable note naming the path, the reason and the errno (R2)"
   (e ((unreadable-entry? e) (unreadable-discovery writer (unreadable-entry-path e) (unreadable-entry-reason e) (unreadable-entry-errno e)))))
  ("log.sc" (read-manifest-safely) 1 guard
   ((unreadable-entry? e) #t)
   propagate a
   "read-manifest-safely: unreadable-entry propagates to discovery, which states it (R2); a parse failure stays malformed"
   (e ((unreadable-entry? e) (raise e)) (#t (quote malformed))))
  ("log.sc" (manifest-version) 1 guard
   ((unreadable-entry? e))
   fact a
   "manifest-version: the value (unreadable path reason errno) (R2c)"
   (e ((unreadable-entry? e) (list (quote unreadable) p (unreadable-entry-reason e) (unreadable-entry-errno e)))))
  ("log.sc" (read-segment) 1 guard
   ((unreadable-entry? e))
   fact a
   "read-segment: a segment read through read-entry that fails is the marker (unreadable path reason errno), which validation turns into a segment-unreadable note naming the segment (K11)"
   (e ((unreadable-entry? e) (list (quote unreadable) path (unreadable-entry-reason e) (unreadable-entry-errno e)))))
  ("log.sc" (physical-of) 1 guard
   ((unreadable-entry? e))
   fact a
   "physical-of: a current segment that is there and will not open leaves the writer no-append-target, the answer a mirror gets; the segment-unreadable note names it and the session gate refuses the writer (K12)"
   (e ((unreadable-entry? e) (quote no-append-target))))
  ("log.sc" (open-load) 1 guard
   ((unreadable-entry? e) #t)
   propagate a
   "open-load parses the store meta.sexp: an unreadable meta.sexp is raised to the answering point's table (F100b PRE-1/P5); a parse failure is #f as before"
   (e ((unreadable-entry? e) (raise e)) (#t #f)))
  ("log.sc" (open-load) 2 guard
   (#t)
   propagate c
   "re-raises after unlock"
   (e (#t (when lock ((current-lock-release) lock)) (raise e))))
  ("log.sc" (store-key) 1 guard
   (#t)
   unrelated a
   "store key"
   (e (#t store)))
  ("log.sc" (log-begin) 1 guard
   (#t)
   propagate b
   "re-raises"
   (e (#t (raise e))))
  ("log.sc" (log-begin) 2 guard
   (#t)
   unrelated a
   "cleanup"
   (e (#t (if #f #f))))
  ("log.sc" (file-version) 1 guard
   ((unreadable-entry? e))
   fact a
   "file-version: the value (unreadable path reason errno) (R2c)"
   (e ((unreadable-entry? e) (list (quote unreadable) p (unreadable-entry-reason e) (unreadable-entry-errno e)))))
  ("log.sc" (read-instance) 1 guard
   (#t)
   unrelated a
   "store instance.sexp"
   (e (#t (quote malformed))))
  ("log.sc" (owner-nonce-or-refusal) 1 guard
   ((unreadable-entry? e))
   refuse b
   "verify-instance's owner read: an owner.sexp that cannot be read is an owner-unreadable record, which verify-instance answers as (refused owner-unreadable (path p) (reason r)) (R2i, U8d, F77b)"
   (e ((unreadable-entry? e) (make-owner-unreadable (unreadable-entry-path e) (unreadable-entry-reason e)))))
  ("log.sc" (inventory-read-failure) 1 guard
   ((unreadable-entry? e) #t)
   propagate b
   "adopt's inventory asks each discovery only for read failures: unreadable-entry propagates to the refusal; any other condition from a readable file is left to adopt's own path, as on the base (F77b review 2)"
   (e ((unreadable-entry? e) (raise e)) (#t #f)))
  ("log.sc" (adopt-preflight) 1 guard
   ((unreadable-entry? e))
   refuse b
   "adopt's whole-store inventory: a read that fails refuses by name (registry-unreadable or metadata-unreadable) before any write (R2h, K13, F77b review 1)"
   (e ((unreadable-entry? e) (unreadable-refusal (if (equal? (unreadable-entry-path e) (registry-path)) (quote registry-unreadable) (quote metadata-unreadable)) e))))
  ("log.sc" (unreadable-metadata) 1 guard
   ((unreadable-entry? e))
   refuse b
   "the append gate lists the writer's directory first, and names it when it cannot be listed (R4, U2b, F77b)"
   (e ((unreadable-entry? e) (list (unreadable-entry-path e) (unreadable-entry-reason e)))))
  ("log.sc" (metadata-flush!) 1 guard
   ((unreadable-entry? e))
   fact b
   "the metadata barrier skips a writer whose directory cannot be listed, traced as barrier-skipped-unreadable, as the delivery barrier does (K9; U2c mode 100, F77b)"
   (e ((unreadable-entry? e) #f)))
  ("rpc.sc" (verb-table) 1 guard
   ((unreadable-entry? e))
   refuse b
   "publish's candidate: absent is no-candidate, any other failure is candidate-unreadable with the path and reason (U9, F77b)"
   (e ((unreadable-entry? e) (list (quote unreadable) (unreadable-entry-path e) (unreadable-entry-reason e)))))
  ("log.sc" (owner-nonce) 1 guard
   (#t)
   fact b
   "owner-nonce: a file that was read and does not parse answers #f, as it did; the read itself is R1's and outside this guard (F77b review 1)"
   (e (#t #f)))
  ("log.sc" (machine-id) 1 guard
   (#t)
   unrelated a
   "machine home"
   (e (#t "unknown")))
  ("log.sc" (machine-id) 2 guard
   (#t)
   unrelated a
   "machine home"
   (e (#t "unknown")))
  ("log.sc" (store-id-of) 1 guard
   (#t)
   unrelated a
   "store id"
   (e (#t #f)))
  ("log.sc" (registry-as-read) 1 guard
   (#t)
   unrelated b
   "registry-as-read parses the machine registry (machine home); read-registry upgrades what it reads"
   (e (#t (quote malformed))))
  ("log.sc" (with-machine-lock) 1 guard
   (#t)
   unrelated a
   "machine lock"
   (e (#t (if #f #f))))
  ("log.sc" (store-lock-collision?) 1 guard
   (#t)
   unrelated a
   "lock probe"
   (e (#t #f)))
  ("log.sc" (store-lock-collision?) 2 guard
   (#t)
   unrelated a
   "lock probe"
   (e (#t #f)))
  ("log.sc" (dir-identity) 1 guard
   (#t)
   unrelated a
   "dir identity"
   (e (#t #f)))
  ("log.sc" (stage-candidate!) 1 guard
   (#t)
   propagate b
   "stage-candidate! re-raises after cleanup (two handlers on this line)"
   (e (#t (guard (e2 (#t (void))) (fd-close fd)) (guard (e2 (#t (void))) (unlink! tmp)) (raise e))))
  ("log.sc" (stage-candidate!) 2 guard
   (#t)
   unrelated b
   "stage-candidate!: the inner cleanup guard, (void), on the same line as the outer handler"
   (e2 (#t (void))))
  ("log.sc" (stage-candidate!) 3 guard
   (#t)
   unrelated a
   "cleanup"
   (e2 (#t (void))))
  ("log.sc" (stage-candidate!) 4 guard
   (#t)
   unrelated a
   "cleanup"
   (e (#t (void))))
  ("log.sc" (overwrite-segment!) 1 guard
   (#t)
   unrelated a
   "cleanup"
   (e (#t (void))))
  ("log.sc" (quarantine!) 1 guard
   (#t)
   refuse b
   "quarantine!: an existing marker that will not read refuses before any write (an assertion today; becomes the typed refusal)"
   (e (#t (quote unreadable))))
  ("log.sc" (kept-content-ok?) 1 guard
   ((unreadable-entry? e))
   refuse b
   "kept-content-ok?: a retained candidate that cannot be read is answered as (unreadable path reason), and keep-incoming! refuses kept-unreadable with no rename over it (U9, F77b)"
   (e ((unreadable-entry? e) (list (quote unreadable) (unreadable-entry-path e) (unreadable-entry-reason e)))))
  ("log.sc" (keep-incoming!) 1 guard
   ((unreadable-entry? e))
   refuse b
   "keep-incoming! asks the .ok marker's presence (entry-type) before any staging -- existence, as the base; only a stat that fails refuses kept-unreadable, with nothing written (F77b reviews 1-3)"
   (e ((unreadable-entry? e) (list (quote unreadable) (unreadable-entry-path e) (unreadable-entry-reason e)))))
  ("log.sc" (keep-incoming!) 2 guard
   (#t)
   unrelated a
   "cleanup"
   (e (#t (void))))
  ("log.sc" (uncertain-derived) 1 guard
   (#t)
   fact a
   "uncertain-derived: (w 0 #f) for unreadable (R2b)"
   (e (#t #f)))
  ("log.sc" (uncertain-cached) 1 guard
   ((unreadable-entry? e))
   fact a
   "uncertain-cached: the symbol unreadable; the path and the reason go to the trace as uncertain-cache-unreadable (R2b)"
   (e ((unreadable-entry? e) (trace-event! (quote uncertain-cache-unreadable) (cons (unreadable-entry-path e) (unreadable-entry-reason e)) #f) (quote unreadable))))
  ("log.sc" (uncertain-cached) 2 guard
   (#t)
   unrelated a
   "uncertain-cached: guards the parse of bytes already read, and answers unreadable, not absence"
   (e (#t (quote unreadable))))
  ("log.sc" (log-publish!) 1 guard
   (#t)
   propagate b
   "log-publish! re-raises after unlock"
   (e (#t ((current-lock-release) lock) (release-store! store) (raise e))))
  ("log.sc" (manifest-unreadable) 1 guard
   ((unreadable-entry? e) (log-error? e) #t)
   refuse b
   "manifest-unreadable (publish): the unreadable-entry clause names the path and the reason (added in F77a)"
   (e ((unreadable-entry? e) (list (list (quote path) (unreadable-entry-path e)) (list (quote reason) (unreadable-entry-reason e)))) ((log-error? e) (let ((detail (log-error-detail e))) (list (list (quote path) (cdr (assq (quote path) detail))) (list (quote reason) (let ((r (assq (quote reason) detail))) (if r (cdr r) "unreadable")))))) (#t (list (list (quote path) (manifest-path store writer)) (list (quote reason) "unreadable")))))
  ("log.sc" (active-current-segment?) 1 guard
   (#t)
   refuse b
   "active-current-segment? (R2d)"
   (e (#t #f)))
  ("log.sc" (retired-put-clause!) 1 guard
   (#t)
   propagate b
   "retired-put-clause!: an unreadable record propagates instead of becoming the no-record assertion"
   (e (#t #f)))
  ("log.sc" (retired-uncertain-strict) 1 guard
   ((unreadable-entry? e))
   fact a
   "retired-uncertain-strict: unreadable, for a caller about to write the record (R2b)"
   (e ((unreadable-entry? e) (quote unreadable))))
  ("log.sc" (retired-uncertain-strict) 2 guard
   (#t)
   unrelated a
   "retired-uncertain-strict: guards the parse of bytes already read, and answers unreadable"
   (e (#t (quote unreadable))))
  ("log.sc" (retired-uncertain) 1 guard
   (#t)
   unrelated a
   "retired-uncertain: guards the parse of bytes already read; the read is outside, so unreadable-entry goes up (R2b)"
   (e (#t #f)))
  ("log.sc" (writer-owner-nonce) 1 guard
   (#t)
   refuse b
   "writer-owner-nonce owner-unreadable (R1a)"
   (e (#t #f)))
  ("log.sc" (writer-owner-tx) 1 guard
   (#t)
   refuse b
   "writer-owner-tx (R1a)"
   (e (#t #f)))
  ("log.sc" (retired-successor) 1 guard
   (#t)
   refuse b
   "retired-successor (R1a)"
   (e (#t #f)))
  ("log.sc" (registry-ahead-of-log) 1 guard
   (#t)
   propagate b
   "registry-ahead-of-log: discovery states unreadable and end-seq raises (R2d), so adopt refuses; the catch-all is narrowed"
   (e (#t #f)))
  ("log.sc" (writer-damaged?) 1 guard
   (#t)
   propagate b
   "writer-damaged?: as above; an unreadable discovery is not read as undamaged"
   (e (#t #f)))
  ("log.sc" (adopt!) 1 guard
   (#t)
   propagate b
   "adopt! re-raises after unlock"
   (e (#t ((current-lock-release) lock) (release-store! store) (raise e))))
  ("log.sc" (session-append!) 1 guard
   (#t)
   refuse b
   "session-append! metadata barrier: refused-before-reserve metadata-not-durable"
   (e (#t (quote barrier-failed))))
  ("log.sc" (session-snapshot!) 1 guard
   (#t)
   propagate c
   "session-snapshot!: a failed commit is swallowed today; unreadable-entry must not be, and no snapshot is installed after it"
   (e (#t #f)))
  ("log.sc" (frame-and-write!) 1 guard
   (#t)
   unrelated a
   "encoding"
   (e (#t (quote unframable))))
  ("log.sc" (segment-first-ts) 1 guard
   (#t)
   unrelated b
   "segment-first-ts: guards the decode of bytes already read; the read is outside"
   (e (#t #f)))
  ("log.sc" (write-line!) 1 guard
   (#t)
   unrelated b
   "write-line!: a write failure, not a read"
   (e (#t (quote write-failed))))
  ("log.sc" (close-quietly) 1 guard
   (#t)
   unrelated a
   "close"
   (e (#t (if #f #f))))
  ("log.sc" (load-fingerprint) 1 guard
   (#t)
   fact a
   "K8: an unreadable writer contributes (writer unreadable path reason errno); the catch-all answers #f, which store.sc reads as no reuse"
   (e (#t #f)))
  ("log.sc" (load-deliver!) 1 guard
   (#t)
   propagate b
   "load-deliver! re-raises (delivery barrier)"
   (e (#t (finish-abort! ls (quote deliver-barrier-failed)) (raise e))))
  ("markers.sc" (safe-utf8) 1 guard
   (#t)
   unrelated a
   "utf8"
   (e (#t #f)))
  ("markers.sc" (header-read) 1 guard
   (#t)
   unrelated a
   "header"
   (e (#t (projection-failure (quote invalid-header)))))
  ("mcp/server.sc" (catalogue) 1 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t #f)))
  ("mcp/server.sc" (faithful-id-text) 1 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t (quote no))))
  ("mcp/server.sc" (same-member-name?) 1 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t #f)))
  ("mcp/server.sc" (unpack) 1 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t (quote unreadable))))
  ("mcp/server.sc" (answer-one) 1 guard
   (#t)
   unrelated a
   "mcp"
   (inner (#t (quote bad))))
  ("mcp/server.sc" (answer-one) 2 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t (quote bad))))
  ("mcp/server.sc" (answer-one) 3 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t #f)))
  ("mcp/server.sc" (answer-one) 4 guard
   (#t)
   unrelated a
   "mcp"
   (e (#t (list (quote error) (quote null) -32603 "Core transport unavailable"))))
  ("request.sc" (membership) 1 guard
   (#t)
   unrelated a
   "membership"
   (ex (#t (quote invalid))))
  ("rpc.sc" (guarded) 1 guard
   ((and (list? e) (pair? e) (eq? (car e) (quote error))) (unreadable-entry? e) (fs-error? e) (log-error? e) #t)
   refuse c
   "K1, a mixed handler: refuse/c names its non-filesystem branches (a thrown refusal, log-error, the internal fallback); its filesystem branches PROPAGATE -- an unreadable-entry or a durable-error is raised to rpc-dispatch-parsed's table (F100b point 1; the unreadable-entry branch was answered here from F77a); the incomplete-reduction branch is F77c"
   (e ((and (list? e) (pair? e) (eq? (car e) (quote error))) e) ((unreadable-entry? e) (raise e)) ((fs-error? e) (raise e)) ((log-error? e) (describe-log-error e)) (#t (list (quote error) (quote internal) (list (quote condition) (cond ((and (vector? e) (= 3 (vector-length e)) (eq? (vector-ref e 0) (quote sexpr-error))) (vector-ref e 1)) ((message-condition? e) (condition-message e)) (else "unexpected failure")))))))
  ("rpc.sc" (parse-batch) 1 guard
   (#t)
   unrelated a
   "batch parse"
   (e (#t (quote unreadable))))
  ("store.sc" (stored->payload) 1 guard
   (#t)
   unrelated a
   "payload decode"
   (e (#t x)))
  ("store.sc" (viewed-fields) 1 guard
   (#t)
   unrelated a
   "defs index"
   (e (#t (set! defs-index-skipped (+ defs-index-skipped 1)) (quote ()))))
  ("store.sc" (block-names) 1 guard
   (#t)
   unrelated a
   "defs index"
   (e (#t (set! defs-index-skipped (+ defs-index-skipped 1)) (quote ()))))
  ("store.sc" (state-section) 1 guard
   (#t)
   unrelated c
   "state-section: an in-memory report over a state already obtained"
   (e (#t (list (quote state) (quote unavailable) (list (quote reason) (failure-text e))))))
  ("store.sc" (request-verdict) 1 guard
   (#t)
   fact c
   "request-verdict unknown (R2f)"
   (e (#t (list (quote unknown) (list (quote evidence-unavailable) (failure-text e))))))
  ("store.sc" (commit-then) 1 guard
   (#t)
   propagate b
   "commit-then: the thunk failure is kept and raised after the commit"
   (e (#t (vector (quote raised) e))))
  ("store.sc" (commit-then) 2 guard
   (#t)
   fact b
   "commit-then: a failed commit barrier answers (error unknown (commit-barrier-failed))"
   (e (#t (quote barrier-failed))))
  ("store.sc" (with-store-write) 1 guard
   (#t)
   fact b
   "with-store-write: after writing, answers (error unknown (execution-failed ...)); before, re-raises"
   (e (#t (if (session-write-started? s) (begin (guard (inner (#t #f)) (session-commit! s)) (list (list (quote error) (quote unknown) (list (quote execution-failed) (failure-text e)) (list (quote events) (session-written-events s))))) (raise e)))))
  ("store.sc" (with-store-write) 2 guard
   (#t)
   unrelated a
   "commit cleanup inside"
   (inner (#t #f)))
  ("store.sc" (with-store-write) 3 guard
   (#t)
   fact b
   "replay barrier: (error unknown (replay-barrier-failed ...)), with the record through with-written (F100b; replay creates nothing before the barrier, so NO2 reads it unchanged)"
   (e (#t (with-written (list (quote error) (quote unknown) (list (quote replay-barrier-failed) (failure-text e))) (mutation-record)))))
  ("store.sc" (with-store-write) 4 guard
   (#t)
   unrelated b
   "log-end! cleanup"
   (e (#t #f)))
  ("store.sc" (write-batch!) 1 guard
   (#t)
   fact b
   "receipt barrier: (error unknown (receipt-barrier-failed))"
   (e (#t (quote barrier-failed))))
  ("store.sc" (run-items!) 1 guard
   (#t)
   fact b
   "run-items!: after writing, (error unknown (interrupted ...)); before, re-raises"
   (e (#t (if (session-write-started? s) (list (quote error) (quote unknown) (list (quote interrupted) (failure-text e))) (raise e)))))
  ("store.sc" (write-plan-then!) 1 guard
   (#t)
   fact b
   "plan barrier: (error unknown (plan-barrier-failed))"
   (failure (#t #f)))
  ("store.sc" (barrier-for) 1 guard
   (#t)
   propagate b
   "barrier-for: an unreadable writer propagates instead of becoming the cannot-locate assertion"
   (e (#t #f)))
  ("store.sc" (run-intents!) 1 guard
   (#t)
   fact b
   "run-intents!: as run-items!"
   (e (#t (if (session-write-started? s) (list (quote error) (quote unknown) (list (quote interrupted) (failure-text e))) (raise e)))))
  ("store.sc" (store-snapshot!) 1 guard
   (#t)
   propagate c
   "store-snapshot! re-raises"
   (e (#t (log-end! s) (raise e))))
  ("store.sc" (store-snapshot!) 2 guard
   (#t)
   unrelated c
   "index-checkpoint! after a written snapshot: the checkpoint is a cache; a failed write leaves the next load to rebuild it; nothing reads absence from it"
   (e (#t #f)))
  ("store.sc" (store-check) 1 guard
   (#t)
   unrelated a
   "store-check: load-commit! cleanup"
   (e (#t #f)))
  ("text-code.sc" (definition-name) 1 guard
   (#t)
   unrelated a
   "text code"
   (e (#t #f)))
  ("text-code.sc" (source-prefix-size) 1 guard
   (#t)
   unrelated a
   "text code"
   (e (#t #f)))
  ("theourgia.sc" (read-envelope) 1 guard
   (#t)
   unrelated a
   "client envelope"
   (e (#t #f)))
  ("theourgia.sc" (piped-input) 1 guard
   (#t)
   unrelated a
   "stdin"
   (e (#t #f)))
  ("theourgia.sc" (piped-input) 2 guard
   (#t)
   unrelated a
   "stdin"
   (e (#t #f)))
  ("trace.sc" (trace-event!) 1 guard
   (#t)
   unrelated a
   "trace"
   (e (#t (if #f #f))))
  ("wire.sc" (wire-safe-symbol?) 1 guard
   (#t)
   unrelated a
   "symbol check"
   (e (#t #f)))
  ("wire.sc" (decode-line) 1 guard
   (#t)
   unrelated a
   "line parse"
   (e (#t parse-failed)))
  ("working.sc" (problem) 1 guard
   ((and (pair? e) (eq? (quote working-error) (car e))) (unreadable-entry? e) #t)
   fact b
   "working-unavailable with the reason; an unreadable-entry with its path and the system's reason (U10, F77b); each answer carries the working record through with-written (F100b NO1)"
   (e ((and (pair? e) (eq? (quote working-error) (car e))) (with-written (list (quote error) (quote working-unavailable) (list (quote reason) (cadr e))) (mutation-record))) ((unreadable-entry? e) (with-written (unreadable-answer e) (mutation-record))) (#t (with-written (list (quote error) (quote working-unavailable) (list (quote message) (if (message-condition? e) (condition-message e) "Working storage failed"))) (mutation-record)))))
  ("working.sc" (working-write!) 1 guard
   (#t)
   propagate b
   "working-write!: the historical reduction at the provenance cut; incomplete and unreadable propagate instead of reading as no parent; a bad cut stays #f"
   (failure (#t #f)))
  ("working.sc" (working-restore!) 1 guard
   (#t)
   propagate b
   "working-restore!: as working-write!; an unreadable writer is not no-text-for-version"
   (e (#t #f)))
  ("working.sc" (working-read) 1 guard
   (#t)
   unrelated b
   "working-read: utf8 decode of bytes in hand"
   (failure (#t #f)))
  ("working.sc" (behind-item) 1 guard
   (#t)
   unrelated b
   "behind-item: arithmetic over cuts"
   (e (#t #f)))
  ("working.sc" (retire!) 1 guard
   (#t)
   fact b
   "retire!: the draft lock cannot be taken -- the commit keeps its answer and carries (cleanup-failed (path draft.lock) (reason r)) (U10, F77b)"
   (failure (#t (cleanup-failed (lock-path store writer) failure))))
  ("working.sc" (retire!) 2 guard
   (#t)
   fact b
   "retire!, a commit that landed: one draft could not be retired -- the first such failure is the commit's (cleanup-failed (path p) (reason r)) clause (U10, F77b; review 2)"
   (failure (#t (cleanup-failed (path-for store writer (list-ref (car es) 3)) failure))))
  ("working.sc" (retire!) 3 guard
   (#t)
   fact b
   "retire!, a commit that landed nothing: a draft that could not be retired is not reported, as on the base (F77b review 2)"
   (failure (#t #f)))
  ("working.sc" (working-commit!) 1 guard
   ((unreadable-entry? e) #t)
   fact b
   "DEFERRED FACT to preflight (R1a); an unreadable-entry is also marked, so a commit without --req answers working-unavailable before no-draft (U10, F77b)"
   (e ((unreadable-entry? e) (set! read-failure (unreadable-answer e)) (set! unreadable-failure #t) (quote ())) (#t (set! read-failure (if (and (pair? e) (eq? (quote working-error) (car e))) (list (quote error) (quote working-unavailable) (list (quote reason) (cadr e))) (list (quote error) (quote working-unavailable) (list (quote message) (if (message-condition? e) (condition-message e) "A draft could not be read"))))) (quote ()))))
  ("working.sc" (working-commit!) 2 guard
   (#t)
   fact b
   "deferred read failure (R1a)"
   (e (#t (unless read-failure (set! read-failure (list (quote error) (quote working-unavailable) (list (quote message) (if (message-condition? e) (condition-message e) "A draft could not be read"))))) (quote ()))))
  ("core.sc" (eval-and-exit!) 1 guard
   ((classify-failure e (mutation-record)))
   refuse a
   "F100b point 3, the boot: a filesystem failure inside the scheduler answers by the one table with the boot's record (the pre-scheduler record handed in as its initial entries, AG-b-1/AG-b-2); anything else propagates"
   (e ((classify-failure e (mutation-record)) => (lambda (a) (finish a wire?)))))
  ("core.sc" (main) 1 guard
   ((classify-failure e (mutation-record)))
   refuse a
   "F100b point 3: a filesystem failure before the scheduler answers by the one table with main's record (P3-a); anything else propagates"
   (e ((classify-failure e (mutation-record)) => (lambda (a) (finish a wire-flag)))))
  ("daemon.sc" (dispatch-frame) 1 guard
   ((classify-failure e (quote ())))
   refuse a
   "F100b point 2b: the store check's stat failing answers the table's kind with an empty record, as the transport's refusal (P2b); anything else propagates"
   (e ((classify-failure e (quote ())) => (lambda (a) a))))
  ("rpc.sc" (rpc-dispatch-parsed) 1 guard
   ((classify-failure e (mutation-record)))
   refuse a
   "F100b point 1: the owning dispatch answers a filesystem failure by the one table with its record (P1-a, P1-b, P1-c); a nested dispatch opens no scope; anything else propagates"
   (e ((classify-failure e (mutation-record)) => (lambda (a) a))))
  ("theourgia.sc" (main) 1 guard
   ((classify-failure e (mutation-record)))
   refuse a
   "F100b point 4: the thin client answers a filesystem failure by the one table and exits 75 through refuse (P4); anything else propagates"
   (e ((classify-failure e (mutation-record)) => refuse)))
  ("daemon.sc" (report-and-exit!) 1 guard
   (#t)
   conservative a
   "F100b point 9 (M2a review r1, F1/F3): a failure of the startup report's own write is not reported again -- the report channel is what failed -- and the exit is 75 either way"
   (e (#t (exit 75))))
  ("daemon.sc" (watch-loop) 2 guard
   (#t)
   unrelated a
   "watch loop"
   (e (#t #f)))
  ("theourgiad.sc" (serve-and-exit!) 2 guard
   (#t)
   conservative a
   "F100b point 8 (M2a review r1, F1/F3): a failure of the report's own write is not reported again; exit 75"
   (e2 (#t (exit 75))))
  ("theourgiad.sc" (serve-and-exit!) 1 guard
   ((classify-failure e (mutation-record)))
   refuse a
   "F100b point 8: this program's own filesystem failure (the socket's derivation, the refusal checks, the detach) answers by the one table, written once as a startup report with the attempt clause, exit 75 (P8); anything else propagates"
   (e ((classify-failure e (mutation-record)) => (lambda (answer) (guard (e2 (#t (exit 75))) ((later (quote (theourgia daemon)) (quote write-report-line!)) 1 (append answer (list (list (quote attempt) attempt))))) (exit 75)))))
  ("daemon.sc" (leave) 1 guard
   (#t)
   conservative a
   "F100b M2a r2 F2: the exit code is decided before the tidying; a failed unlink probe is traced and the exit goes ahead (a tripwire: no row makes it fail)"
   (e (#t (trace-event! (quote leave-tidy-failed) #f #f))))
  ("daemon.sc" (leave) 2 guard
   (#t)
   conservative a
   "F100b M2a r2 F2: a failed lock release at exit is traced and the exit goes ahead; the exit closes the descriptor (a tripwire: no row makes it fail)"
   (e (#t (trace-event! (quote leave-tidy-failed) #f #f))))
  ("daemon.sc" (report-quietly) 1 guard
   (#t)
   conservative a
   "F100b M2a r3: a line main writes (exiting, serving, draining) cannot change an exit or end main; measured for the exiting line by P2-startup closed stdout (mutant MC-1), no row for serving and draining"
   (e (#t (trace-event! (quote report-failed) #f #f))))
  ("theourgiad.sc" (detach-step) 2 guard
   (#t)
   conservative a
   "F100b M2a r3: a stderr that cannot be written does not change detach-failed's exit 71 (NO4 closed stderr)"
   (e2 (#t (trace-event! (quote detach-report-failed) #f #f))))
  ("daemon.sc" (watch-loop) 3 guard
   (#t)
   conservative a
   "F100b M2a r4 (the startup-exit design R3): a lock-drop whose release raises does not end main -- in STARTING the boot's own store lock comes back here; traced; a tripwire: no row makes an unlock or close fail"
   (e (#t (trace-event! (quote lock-drop-failed) #f #f))))
  ("client.sc" (exited-answer) 1 guard
   ((unreadable-entry? e))
   refuse a
   "F100b point 6, TK5: the log, read after the daemon's exit under stage client, cannot be read -- the condition is kept and answered as (kind unreadable) naming the log, with the status"
   (e ((unreadable-entry? e) e)))
  ("client.sc" (line-datum) 1 guard
   (#t)
   unrelated a
   "select-report: a line whose bytes are not UTF-8 is not a report (peer text); not a filesystem read"
   (e (#t #f)))
  ("client.sc" (line-datum) 2 guard
   (#t)
   unrelated a
   "select-report: a line that does not parse, or has text after its datum (M2b1 review r1, F1), is skipped, not fatal (H3); not a filesystem read"
   (e (#t #f)))
  )

(raw-accessors
  (file "log.sc")
  (record discovery)
  (fields
    origin
    (immutable end-segment raw-end-segment)
    (immutable end-offset raw-end-offset)
    (immutable end-seq raw-end-seq)
    (immutable segment-ranges raw-segment-ranges)
    (immutable physical-current raw-physical-current)
    (immutable current-buffer raw-current-buffer)
    (immutable torn raw-torn)
    integrity
    quarantine
    retired
    versions
    (immutable retired-tail raw-retired-tail)
    )
  (wrappers
    (raw-end-segment discovery-end-segment)
    (raw-end-offset discovery-end-offset)
    (raw-end-seq discovery-end-seq)
    (raw-segment-ranges discovery-segment-ranges)
    (raw-physical-current discovery-physical-current)
    (raw-current-buffer discovery-current-buffer)
    (raw-torn discovery-torn)
    (raw-retired-tail discovery-retired-tail)
    ))

;; close: (enclosing-definitions callee ordinal category reason)
;;   every reference in ffi.sc to c-close, c-closedir, close-quietly,
;;   current-lock-release, close-port, close-input-port, close-output-port,
;;   fd-close and the forms that close what they open (call-with-input-file,
;;   call-with-output-file, with-input-from-file, with-output-to-file,
;;   call-with-port), and every unlock (c-flock ... LOCK_UN), by the close
;;   census of unreadable-census.sc (F100a, rulings H1, I and J). category is one of four: checked, escape,
;;   contention or out-of-scope; the line is the reader's, not pinned.
(close
  ((above-stdio) c-close 1 escape "the dup failed: the held descriptors are released and the durable-error is raised")
  ((above-stdio) c-close 2 escape "the dup failed: the original descriptor is released before the raise")
  ((above-stdio) c-close 3 out-of-scope "a dup landed above 2: the held low descriptors are released; process descriptors, D1's scope exclusion")
  ((above-stdio) c-close 4 out-of-scope "a dup landed above 2: the original descriptor is released; process descriptor, D1's scope exclusion")
  ((redirect-stdio!) close-quietly 1 out-of-scope "after the dup2s: the /dev/null descriptor is released; stdio, D1's scope exclusion")
  ((redirect-stdio!) close-quietly 2 out-of-scope "after the dup2s: the log descriptor is released; stdio, D1's scope exclusion")
  ((redirect-stdio!) fd-close 1 out-of-scope "inside the failure guard, before the re-raise: fd-close raises on its own failure, which would replace the condition leaving; stdio, D1's scope exclusion, the queue candidate")
  ((redirect-stdio!) fd-close 2 out-of-scope "inside the failure guard, before the re-raise: fd-close raises on its own failure, which would replace the condition leaving; stdio, D1's scope exclusion, the queue candidate")
  ((rss-linux) call-with-input-file 1 out-of-scope "the implicit close of /proc's statm, not a store entry; rss-linux is outside the door")
  ((unix-socket-connect) c-close 1 escape "setsockopt raised: the socket is released and the condition re-raised")
  ((unix-socket-connect) c-close 2 escape "connect failed: the socket is released before the raise")
  ((close-unwritten-port!) close-port 1 checked "the close of a port Chez opened to create and never wrote: its failure raises unreadable-entry")
  ((overwrite-entry!) close-quietly 1 escape "the unwind after-thunk, only when the normal path did not reach its fd-close")
  ((overwrite-entry!) fd-close 1 checked "the normal path's close of the written descriptor: durable-error on failure")
  ((read-entry/errno) c-close 1 checked "through read-closing: checked on the normal path, quiet on an escape")
  ((read-entry-range) c-close 1 checked "through read-closing: checked on the normal path, quiet on an escape")
  ((list-entries/errno) c-closedir 1 checked "through read-closing: checked on the normal path, quiet on an escape")
  ((fd-close) c-close 1 checked "the descriptor's own close: durable-error when written, unreadable-entry when not")
  ((close-quietly) c-close 1 escape "the quiet close itself; its category belongs to each caller, pinned by its own use (escape in the unwinds and guards, out-of-scope in redirect-stdio!)")
  ((fsync-dir!) c-close 1 checked "through read-closing: checked on the normal path, quiet on an escape")
  ((file-size) c-close 1 checked "through read-closing: checked on the normal path, quiet on an escape")
  ((lock-acquire!) close-quietly 1 escape "inside the guard that re-raises a failed flock")
  ((lock-try-acquire!) close-quietly 1 escape "inside the guard that re-raises a raising flock call")
  ((lock-try-acquire!) c-close 1 checked "the contention answer: checked, then #f (F100a review r2, ruling H1)")
  ((lock-try-acquire!) close-quietly 2 escape "a flock failure other than contention: released, then unreadable-entry raised")
  ((lock-release!) unlock 1 checked "the unlock's failure raises unreadable-entry after the close is attempted")
  ((lock-release!) c-close 1 checked "the close's failure raises unreadable-entry; close-fail reaches it")
  ((call-with-lock) current-lock-release 1 checked "the normal exit's release: its failure raises")
  ((call-with-lock) current-lock-release 2 escape "the unwind's release on an escape: quiet, so the condition leaving is the one reported")
  ((barrier!) close-port 1 out-of-scope "the injection FIFO's close, not a store entry; barrier! is the fault injection's, outside the door")
  )
