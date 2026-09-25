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
  ("cli.sc" (main) file-exists? 1 keep a "not writer layout: daemon socket path")
  ("client.sc" () file-exists? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("client.sc" (nearest-existing) file-exists? 1 keep a "not writer layout: client path search")
  ("client.sc" (log-length) file-exists? 1 keep a "not writer layout: daemon log file")
  ("code-project.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("code-project.sc" () file-is-regular? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("code-project.sc" (directory-files walk) file-is-directory? 1 keep a "not writer layout: import-code input tree")
  ("code-project.sc" (directory-files walk) file-is-regular? 1 keep a "not writer layout: import-code input tree")
  ("code-project.sc" (directory-files) file-is-directory? 1 keep a "not writer layout: import-code input dir")
  ("daemon.sc" () file-exists? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("daemon.sc" () file-is-regular? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("daemon.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("daemon.sc" (store-here?) file-exists? 1 keep a "not writer layout: store meta.sexp")
  ("evidence-index.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("evidence-index.sc" (files) file-is-directory? 1 convert c "evidence inventory lists writer files (R2f)")
  ("evidence-index.sc" (inventory) file-exists? 1 convert c "evidence inventory of writer files (R2f)")
  ("ffi.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("ffi.sc" () file-is-regular? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("ffi.sc" (file-is-directory?) file-is-directory? 1 convert a "the predicate itself: now entry-type, so an unreadable path raises (R1)")
  ("ffi.sc" (file-is-regular?) file-is-regular? 1 convert a "the predicate itself: now entry-type, so an unreadable path raises (R1)")
  ("ffi.sc" (mkdir-p!) file-is-directory? 2 convert a "generic helper mkdir-p!: the predicate is now entry-type and raises on an unreadable path (R1: converted unconditionally)")
  ("ffi.sc" (mkdir-one!) file-is-directory? 1 convert a "generic helper mkdir-one!: the predicate is now entry-type and raises on an unreadable path (R1)")
  ("ffi.sc" (rss-linux) file-exists? 1 keep a "not writer layout: /proc on linux")
  ("ffi.sc" (path-version) file-is-regular? 1 convert a "generic helper path-version: the predicate is now entry-type and raises on an unreadable path (R1)")
  ("log.sc" (store-writers) file-is-directory? 1 out-of-scope - "store-writers lists writers/ itself: R5, queued F79")
  ("log.sc" (snapshot-read) file-exists? 1 keep a "not writer layout: snapshot file under snap/")
  ("log.sc" (open-load) file-exists? 1 keep a "not writer layout: store meta.sexp")
  ("log.sc" (metadata-flush!) file-exists? 1 convert b "metadata-flush!: the delivery barrier (R1a)")
  ("log.sc" (read-instance) file-exists? 1 keep a "not writer layout: store instance.sexp")
  ("log.sc" (owner-nonce) file-exists? 1 convert b "owner-nonce reads the local writer owner.sexp (R2g)")
  ("log.sc" (machine-id) file-exists? 2 keep a "not writer layout: machine home machine-id")
  ("log.sc" (ensure-machine-home!) file-is-directory? 1 keep a "not writer layout: machine home")
  ("log.sc" (read-registry) file-exists? 1 keep a "not writer layout: machine registry (R2h: non-upgrading read is b)")
  ("log.sc" (registry-inside-store?) file-exists? 1 keep a "not writer layout: store meta.sexp")
  ("log.sc" (local-writer-name) file-exists? 1 convert b "local-writer-name scans owner.sexp (R2g)")
  ("log.sc" (writer-history) file-exists? 1 convert b "writer-history reads segments (publish)")
  ("log.sc" (segment-range) file-exists? 1 convert b "segment-range (publish)")
  ("log.sc" (ensure-directory!) file-is-directory? 1 convert b "ensure-directory! under the writer (incoming/quarantine)")
  ("log.sc" (overwrite-segment!) file-exists? 1 convert b "overwrite-segment! target")
  ("log.sc" (evidence-path) file-exists? 1 convert b "evidence-path under incoming/")
  ("log.sc" (quarantine!) file-exists? 1 convert b "quarantine! target")
  ("log.sc" (kept-content-ok?) file-exists? 1 convert b "kept-content-ok?: retained candidate (R1a hard case)")
  ("log.sc" (keep-incoming!) file-exists? 1 convert b "keep-incoming! marker")
  ("log.sc" (barrier-artefacts) file-exists? 1 convert b "barrier-artefacts: recovery barrier inventory (Implementation checks)")
  ("log.sc" (ensure-writer-directory!) file-is-directory? 1 convert b "ensure-writer-directory! (publish)")
  ("log.sc" (publish-validated!) file-exists? 1 convert b "publish-validated! local segment")
  ("log.sc" (active-current-segment?) file-exists? 1 convert b "active-current-segment? owner.sexp (R2d, publish)")
  ("log.sc" (generation-present?) file-exists? 1 convert b "generation-present? (adopt)")
  ("log.sc" (retired-put-clause!) file-exists? 1 convert b "retired-put-clause! (adopt)")
  ("log.sc" (step-owner-installed!) file-exists? 1 convert b "step-owner-installed! (adopt)")
  ("log.sc" (writer-owner-nonce) file-exists? 1 convert b "writer-owner-nonce: recovery read (R1a REFUSE)")
  ("log.sc" (writer-owner-tx) file-exists? 1 convert b "writer-owner-tx: recovery read")
  ("log.sc" (transaction-state) file-exists? 1 convert b "transaction-state owner.sexp (adopt)")
  ("log.sc" (retired-successor) file-exists? 1 convert b "retired-successor: recovery read")
  ("log.sc" (segment-length) file-exists? 1 convert b "segment-length (adopt)")
  ("log.sc" (install-snapshot!) file-is-directory? 1 keep a "not writer layout: snapshot directory; creation is refused on an incomplete reduction in c")
  ("log.sc" (select-readable-snapshot) file-is-directory? 1 keep a "not writer layout: the snapshot directory; select-snapshot answers writer-unreadable before this when any writer is unreadable (K8)")
  ("operation-packet.sc" (frozen-operation) file-exists? 1 keep a "not writer layout: store/operation-packets")
  ("project.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("project.sc" (nearest-existing) file-exists? 1 keep a "not writer layout: export-md output dir")
  ("project.sc" (md-files) file-is-directory? 1 keep a "not writer layout: import-md input dir")
  ("rpc.sc" (no-store?) file-exists? 1 keep a "not writer layout: store meta.sexp")
  ("rpc.sc" (verb-table) file-exists? 1 convert b "publish candidate path (R1a: absent = no-candidate, else candidate-unreadable)")
  ("store.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("store.sc" (foreign-writer) file-exists? 1 convert b "foreign-writer scans owner.sexp at init")
  ("store.sc" (store-init!) file-exists? 1 keep a "not writer layout: store meta.sexp")
  ("store.sc" (check-snapshots) file-exists? 1 keep a "not writer layout: snapshot dir in check (check is a)")
  ("working.sc" () file-is-directory? 1 binding a "import or re-export of the predicate; row (i) pins bindings")
  ("working.sc" (writer-for) file-exists? 2 convert b "writer-for owner.sexp and retired.sexp")
  ("working.sc" (entry-at) file-exists? 1 convert b "entry-at draft file")
  ("working.sc" (active-entries) file-is-directory? 1 convert b "active-entries PROPAGATES (R1a)")
  ("working.sc" (working-discard!) file-exists? 1 convert b "working-discard! draft file")
  ("working.sc" (working-list) file-exists? 1 convert b "working-list draft path")
  ("working.sc" (retire!) file-exists? 1 convert b "retire!: post-commit cleanup (R1a FACT)")
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
  ("cli.sc" (forward-then-exit!) 1 guard
   (#t)
   unrelated a
   "socket forwarding"
   (e (#t (quote unreadable))))
  ("cli.sc" (detach-step) 1 guard
   (#t)
   unrelated a
   "detach errno"
   (e (#t (let ((code (detach-errno e))) (trace-event! (quote detach-failed) code #f) (say (list (quote error) (quote detach-failed) (list (quote step) step) (list (quote path) log-path) (list (quote errno) code)))) (exit 71))))
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
   ((unreadable-entry? e))
   refuse a
   "start-one!: a serve.log that is there and cannot be opened -- its size is read before the guard below -- is kept as a value and answered as the same (error serve-start-failed (unreadable ...)) the guard below gives; any other failure of that read leaves as it did on a839eb1 (review r2, B1)"
   (e ((unreadable-entry? e) e)))
  ("client.sc" (start-one!) 2 guard
   ((fs-error? e) (unreadable-entry? e))
   refuse a
   "start-one!: a start that fails -- an fs-error, or a level of the log directory that cannot be searched (unreadable-entry since R1) -- answers (error serve-start-failed ...) naming it, never a raise out of call!"
   (e ((fs-error? e) (list (quote error) (quote serve-start-failed) (list (quote spawn) (fs-error-errno e)))) ((unreadable-entry? e) (list (quote error) (quote serve-start-failed) (list (quote unreadable) (list (quote path) (unreadable-entry-path e)) (list (quote reason) (unreadable-entry-reason e)))))))
  ("client.sc" (connects?) 1 guard
   ((fs-error? e))
   unrelated a
   "socket connect"
   (e ((fs-error? e) #f)))
  ("client.sc" (last-error-in) 1 guard
   (#t)
   unrelated a
   "daemon log read"
   (e (#t #f)))
  ("client.sc" (last-error-in) 2 guard
   (#t)
   unrelated a
   "daemon log read"
   (e (#t (quote unreadable))))
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
   (#t)
   unrelated a
   "socket device/inode"
   (e (#t #f)))
  ("daemon.sc" (main) 1 guard
   (#t)
   unrelated a
   "main"
   (e (#t #f)))
  ("daemon.sc" (watch-loop) 1 guard
   (#t)
   unrelated a
   "watch loop"
   (e (#t #f)))
  ("daemon.sc" (unlink-own-socket!) 1 guard
   (#t)
   unrelated a
   "socket"
   (e (#t #f)))
  ("daemon.sc" (occupied-by-a-non-socket?) 1 guard
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
   "store-loop reports the load failure and re-raises it; c makes the daemon declare acceptance of an incomplete reduction"
   (e (#t e)))
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
   ((and (pair? e) (eq? (car e) (quote error))) #t)
   unrelated a
   "eval"
   (e ((and (pair? e) (eq? (car e) (quote error))) e) (#t (quote (error eval-exception (kind raised) (message "Evaluation raised an exception"))))))
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
   ((i/o-file-already-exists-error? e) (fs-error? e) #t)
   propagate a
   "re-raises"
   (e ((i/o-file-already-exists-error? e) #f) ((fs-error? e) (raise e)) (#t (raise e))))
  ("ffi.sc" (mkdir-one!) 1 guard
   (#t)
   propagate a
   "mkdir-one!: catch-all then re-checks with the predicate; rewritten with R1"
   (e (#t (unless (file-is-directory? path) (raise (fs-err (quote mkdir) path #f))))))
  ("ffi.sc" (unlink!) 1 guard
   ((fs-error? e) #t)
   propagate a
   "re-raises"
   (e ((fs-error? e) (raise e)) (#t (raise (fs-err (quote unlink) path #f)))))
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
  ("ffi.sc" (file-ensure!) 1 guard
   ((fs-error? e) (unreadable-entry? e) #t)
   propagate a
   "re-raises; unreadable-entry passes unchanged"
   (e ((fs-error? e) (raise e)) ((unreadable-entry? e) (raise e)) (#t (raise (fs-err (quote create) path #f)))))
  ("ffi.sc" (file-size) 1 guard
   ((and (fs-error? e) (fs-error-errno e) (not (absence-errno? (fs-error-errno e)))))
   unrelated a
   "file-size: catches the open's durable error only; a non-absence errno is raised again as unreadable-entry naming the path (R1, K12), ENOENT and ENOTDIR stay the durable error"
   (e ((and (fs-error? e) (fs-error-errno e) (not (absence-errno? (fs-error-errno e)))) (unreadable! path (fs-error-errno e)))))
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
  ("log.sc" (read-whole) 1 guard
   (#t)
   unrelated a
   "read-whole: guards close-port in cleanup only; the read itself is outside it"
   (e (#t (if #f #f))))
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
   (#t)
   unrelated c
   "open-load parses the store meta.sexp, not a writer file"
   (e (#t #f)))
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
  ("log.sc" (owner-nonce) 1 guard
   (#t)
   refuse b
   "owner-nonce (R2g/R2i)"
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
  ("log.sc" (read-registry) 1 guard
   (#t)
   unrelated b
   "read-registry parses the machine registry (machine home)"
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
   (#t)
   refuse b
   "kept-content-ok? (R1a hard case)"
   (e (#t #f)))
  ("log.sc" (keep-incoming!) 1 guard
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
  ("log.sc" (read-first-line) 1 guard
   (#t)
   unrelated b
   "read-first-line: guards close-port in cleanup only"
   (e (#t (if #f #f))))
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
   ((and (list? e) (pair? e) (eq? (car e) (quote error))) (unreadable-entry? e) (log-error? e) #t)
   refuse c
   "K1: as daemon.sc answer-for: the unreadable-entry branch is F77a, the incomplete-reduction branch F77c"
   (e ((and (list? e) (pair? e) (eq? (car e) (quote error))) e) ((unreadable-entry? e) (list (quote error) (quote unreadable) (list (quote path) (unreadable-entry-path e)) (list (quote reason) (unreadable-entry-reason e)))) ((log-error? e) (describe-log-error e)) (#t (list (quote error) (quote internal) (list (quote condition) (cond ((and (vector? e) (= 3 (vector-length e)) (eq? (vector-ref e 0) (quote sexpr-error))) (vector-ref e 1)) ((message-condition? e) (condition-message e)) (else "unexpected failure")))))))
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
   "replay barrier: (error unknown (replay-barrier-failed ...))"
   (e (#t (list (quote error) (quote unknown) (list (quote replay-barrier-failed) (failure-text e))))))
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
   ((and (pair? e) (eq? (quote working-error) (car e))) #t)
   fact b
   "working-unavailable with the reason"
   (e ((and (pair? e) (eq? (quote working-error) (car e))) (list (quote error) (quote working-unavailable) (list (quote reason) (cadr e)))) (#t (list (quote error) (quote working-unavailable) (list (quote message) (if (message-condition? e) (condition-message e) "Working storage failed"))))))
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
   "retire!: cleanup-failed clause (R1a)"
   (failure (#t #f)))
  ("working.sc" (working-commit!) 1 guard
   (#t)
   fact b
   "DEFERRED FACT to preflight (R1a)"
   (e (#t (set! read-failure (if (and (pair? e) (eq? (quote working-error) (car e))) (list (quote error) (quote working-unavailable) (list (quote reason) (cadr e))) (list (quote error) (quote working-unavailable) (list (quote message) (if (message-condition? e) (condition-message e) "A draft could not be read"))))) (quote ()))))
  ("working.sc" (working-commit!) 2 guard
   (#t)
   fact b
   "deferred read failure (R1a)"
   (e (#t (unless read-failure (set! read-failure (list (quote error) (quote working-unavailable) (list (quote message) (if (message-condition? e) (condition-message e) "A draft could not be read"))))) (quote ()))))
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
