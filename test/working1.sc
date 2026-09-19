#!r6rs
(import (chezscheme) (theourgia rpc) (theourgia store)
        (theourgia reduce) (theourgia ffi))

(define failures 0)
(define (want name actual expected)
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/working-" (number->string (get-process-id))))
(when (file-exists? root) (error 'working1 "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))

;; KEY: THE WRITER IS NAMED HERE BECAUSE IT IS NO LONGER GUESSED. A draft
;; verb that was not told which writer it speaks for used to fall back to
;; this store's own local log writer, so two agents that never passed
;; `--writer` shared one draft space without either of them being told.
;; The core now refuses that call instead; naming the same writer the old
;; fallback would have chosen keeps every row below asking exactly what it
;; asked before.
;;
;; NEVER: AND ONLY WHERE IT WAS MISSING: a call that already names a writer
;; (WS-03/WS-04 use `draft002` to show drafts are scoped) must keep the
;; one it names, or those rows would stop being about two writers.
(define draft-verbs '(write restore drafts discard commit))

(define (wants-writer? verb args)
  (or (memq verb draft-verbs)
      (and (eq? verb 'read)
           (or (member "--working" args) (member "--working-info" args)))))

(define (call . args)
  (rpc-dispatch store
                (if (and (wants-writer? (car args) (cdr args))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define (insert title)
  (let ((a (call 'insert "--title" title "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define a (insert "A"))
(define b (insert "B"))
(define (src id)
  (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce store) id))))))
(define (hash id) (block-hash (open-and-reduce store) id))
(define (ok? answer) (rpc-ok? answer))
(define (kind answer) (if (and (pair? answer) (pair? (cdr answer))) (cadr answer) answer))
(define (snapshot dir)
  (apply append
    (map (lambda (name)
           (let ((p (string-append dir "/" name)))
             (cond
               ;; NEVER: NOT THE DRAFTS, AND NOT THE LOCK THAT SERIALISES
               ;; THEIR INSTALLATION. WS-01's property is that `write`
               ;; leaves the AUTHORITATIVE files byte-identical; a
               ;; zero-byte lock file, created once and never written
               ;; to, is no more authoritative than the drafts beside
               ;; it. It sits in the writer's directory rather than
               ;; among the drafts because a block id must not be able
               ;; to name it -- `discard` would otherwise delete it.
               ((member name '("working" "working-requests" "draft.lock")) '())
               ((file-directory? p) (snapshot p))
               (else (list (cons p (call-with-port (open-file-input-port p)
                                    get-bytevector-all)))))))
         (sort string<? (directory-list dir)))))
(define before (snapshot (string-append store "/writers")))
(define h0 (hash a))
(want "WS-01 write saves outside the log" (ok? (call 'write a "new")) #t)
(want "WS-01 authoritative files remain byte-identical" (snapshot (string-append store "/writers")) before)
(want "WS-02 default read remains committed" (src a) "old")
(want "WS-02 working read returns the draft" (call 'read a "--working") '(ok (text "new")))
(want "WS-14 a later write preserves the baseline" (ok? (call 'write a "newer")) #t)
(want "WS-03 another writer has an independent draft"
      (ok? (call 'write a "other" "--writer" "draft002")) #t)
(want "WS-03 writer reads its own draft" (call 'read a "--working" "--writer" "draft002") '(ok (text "other")))
(want "WS-04 discard is scoped" (ok? (call 'discard a "--writer" "draft002")) #t)
(want "WS-04 another writer's draft survives" (call 'read a "--working") '(ok (text "newer")))

(define cursor (string-append writer ":" (number->string (cdr (assoc writer (reduce-applied-cut (open-and-reduce store)))))))

;; KEY: A COMMIT CARRYING `--req` MUST NAME THE VERSION IT CONSUMES.
;;
;; §7.5.9: the request's identity is taken over the draft writer and the
;; ordered (block . version) list, so a retry from a new process can
;; rebuild it -- which it cannot do by reading a draft its own first
;; attempt retired. "Infer once and reuse" has nowhere to keep the
;; inference once the packet beside the drafts is gone.
(define (version-of id)
  (let ((r (call 'read id "--working-info")))
    (and (ok? r) (list-ref (assq 'projection (cdr r)) 4))))
(define v-a (version-of a))
(want "WS-05 NEGATIVE: --req without a version is refused"
      (kind (call 'commit a "--req" "needs-versions" "--cursor" cursor))
      'bad-request)
(want "WS-05 NEGATIVE: and it says which rule refused it"
      (caddr (call 'commit a "--req" "needs-versions" "--cursor" cursor))
      'req-needs-versions)
;; TWIN: THE RULE IS ABOUT `--req`, NOT ABOUT COMMITS. A commit without
;; a request id makes no retry promise and needs no version; whatever
;; else it answers, it does not answer this.
;;
;; NOTE: IT ASSERTS THE ABSENCE OF THIS REFUSAL RATHER THAN SUCCESS. `b`
;; has no draft here, and giving it one would commit it -- which the
;; rows further down are still about.
(want "WS-05 TWIN: without --req the version rule does not fire"
      (equal? 'req-needs-versions
              (let ((a (call 'commit b))) (and (pair? a) (pair? (cddr a)) (caddr a))))
      #f)

(define first (call 'commit a "--req" "working-request-1" "--cursor" cursor
                    "--working-version" v-a))
(want "WS-05 fresh commit succeeds" (ok? first) #t)
(want "WS-05 commit applies the saved bytes" (src a) "newer")
(want "WS-11 a new event changes the block hash" (equal? h0 (hash a)) #f)
(want "WS-16 successfully consumed work is absent from drafts" (call 'drafts) '(ok (items)))
(call 'set a "src" "third")
(define after-third (snapshot (string-append store "/writers")))
(define replay (call 'commit a "--req" "working-request-1" "--cursor" cursor))
(printf "replay observation ~s\n" replay)

;; NEVER: THREE ROWS ARE RETIRED HERE, AND THIS IS WHERE THEY WERE.
;;
;; WS-07 "identity is checked before the old baseline", WS-08 "frozen
;; replay still succeeds" and WS-25 "changed retry arguments are
;; refused" were all about the COMMIT PACKET: an immutable file written
;; beside the drafts, holding the request this commit had accepted and
;; the exact envelopes it had read, so a retry could be answered from it
;; after the drafts had been retired.
;;
;; §7.5.9 takes the packet away. The log holds both facts now -- the
;; plan freezes the declared text, and its `consumes` names the versions
;; the request took -- so a retry is answered by completing the frozen
;; plan. Until that path exists (`store.sc` answers `incomplete-request`
;; today) there is nothing for these rows to assert, and an expectation
;; edited to match the gap would be asserting the gap.
;;
;; THEIR SUCCESSORS, BY NAME, in the step that lands the completion
;; path:
;;
;;   WS-07, WS-08  -> W4" (plan persisted, members not executed, the
;;                   draft replaced by v2, retry R(v1) completes from
;;                   the plan's text and v2 stays a draft)
;;                   and W4' (after retirement, R(v1) replays and v2 is
;;                   untouched; R(v2) is a req-mismatch)
;;   WS-25         -> the fingerprint-component rows (a different
;;                   version, a different draft-writer, a reversed
;;                   pairing: each its own mismatch)
;;
;; WS-17 below is NOT retired: it asserts that a replay leaves a later
;; draft alone, which is still true and still worth a row.
(want "WS-08 a retry does not re-read a newer draft" (ok? (call 'write a "next")) #t)
;; NEVER: WS-17 IS RETIRED FOR THE SAME REASON AS WP-04's SECOND ROW: it
;; reads back the draft it has just written, with no retry in between,
;; so "a replay retains subsequent edits" is a label rather than a
;; measurement. SUCCESSOR: `plan-completion.sc`, W4'.

(call 'set a "src" "concurrent")
(define stale-before (snapshot (string-append store "/writers")))
(define stale (call 'commit a))
(want "WS-06 a stale baseline is refused" (kind stale) 'stale-baseline)
(want "WS-06 stale refusal names the block" (and (pair? stale) (assq 'block (filter pair? (cdr stale))) (cadr (assq 'block (filter pair? (cdr stale))))) a)
(want "WS-06 stale refusal preserves log bytes" (snapshot (string-append store "/writers")) stale-before)
(want "WS-14 ordinary write does not rebase" (begin (call 'write a "merged") (kind (call 'commit a))) 'stale-baseline)
;; KEY: A REBASE NAMES THE VERSION IT MERGED ONTO, and this row used to
;; take "now" instead.
;;
;; §7.5.11: `--rebase` carries `--based-on H1 --working-cut C1` -- the
;; version the merge was actually done against. "Now" is a claim the
;; store cannot check and the client did not make: between reading the
;; text they merged and saying `--rebase`, somebody else may have
;; committed, and recording THAT as the baseline means the next commit
;; overwrites it without being refused.
(define (cut-text) (format "~s" (reduce-applied-cut (open-and-reduce store))))
(want "WS-14 NEGATIVE: a bare --rebase is refused"
      (kind (call 'write a "merged" "--rebase")) 'bad-request)
(want "WS-14 NEGATIVE: and it says which rule refused it"
      (caddr (call 'write a "merged" "--rebase")) 'rebase-needs-baseline)
(want "WS-14 explicit rebase advances the baseline"
      (ok? (call 'write a "merged" "--rebase" "--based-on" (hash a) "--working-cut" (cut-text))) #t)
(want "WS-05 rebased commit succeeds" (ok? (call 'commit a)) #t)
(want "WS-05 rebased bytes are committed" (src a) "merged")

(call 'write a "A2")
(call 'write b "B2")
(call 'set b "src" "B-other")
(define multi-before (snapshot (string-append store "/writers")))
(want "WS-09 a stale final block refuses the whole selection" (kind (call 'commit a b)) 'stale-baseline)
(want "WS-09 no fresh prefix is appended" (snapshot (string-append store "/writers")) multi-before)
(want "WS-09 fresh first block stays unchanged" (src a) "merged")
(call 'write b "B-merged" "--rebase" "--based-on" (hash b) "--working-cut" (cut-text))
(want "WS-09 both fresh blocks commit" (ok? (call 'commit a b)) #t)
(want "WS-09 both contents are present" (list (src a) (src b)) '("A2" "B-merged"))

(call 'write a "A3")
(call 'set b "src" "unrelated")
(want "WS-10 unrelated edits do not stale a draft" (ok? (call 'commit a)) #t)
(want "WS-19 existing direct set remains unconditional" (ok? (call 'set a "src" "direct")) #t)
(want "WS-19 explicit stale --based-on is additive" (kind (call 'set a "src" "bad" "--based-on" h0)) 'stale-baseline)
(want "WS-26 unsafe writer paths are refused" (kind (call 'write a "bad" "--writer" "../bad")) 'bad-request)
(want "WS-26 absence falls back to committed" (call 'read b "--working" "--writer" "draft002") '(ok (text "unrelated")))
(printf "~a failures\nworking1 complete\n" failures)
(exit (if (= failures 0) 0 1))
