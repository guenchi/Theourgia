#!r6rs
(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce) (theourgia ffi))
(define failures 0)
(define (want name actual expected)
  (if (equal? actual expected) (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1)) (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/working-projection-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define (call . args) (rpc-dispatch store args "test"))
(define init (call 'init))
(define writer (cadr (assq 'writer (cdr init))))
(define inserted (call 'insert "--title" "A" "--text" "old"))
(define event (car (cadr (assq 'events (cdr inserted)))))
(define id (block-id (car event) (cdr event)))
(define cursor (string-append writer ":" (number->string (cdr event))))
(define first (call 'write id "first" "--writer" "window-a"))
(define version (cadr (assq 'version (cdr first))))
(define view (call 'read id "--working-info" "--writer" "window-a"))
(define projection (and (pair? view) (assq 'projection (filter pair? (cdr view)))))
(want "WP-01 snapshot body and version come from one envelope"
      (and projection (list-ref projection 4)) version)
(want "WP-01 snapshot includes the exact body" (and projection (list-ref projection 7)) "first")
(call 'write id "second" "--writer" "window-a")
(want "WP-02 queued old version never commits a newer draft"
      (cadr (call 'commit id "--writer" "window-a" "--working-version" version "--req" "R1" "--cursor" cursor))
      'working-version-changed)
(want "WP-02 refused version leaves committed bytes unchanged"
      (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce store) id))))) "old")
(define second (call 'write id "second" "--writer" "window-a"))
(define current-version (cadr (assq 'version (cdr second))))
(define args (list 'commit id "--writer" "window-a" "--working-version" current-version "--req" "R2" "--cursor" cursor))
(want "WP-03 selected current version commits" (rpc-ok? (apply call args)) #t)
(call 'write id "later" "--writer" "window-a")

;; ⛔ WP-04's FIRST ROW IS RETIRED, AND ITS NAME SAYS WHY: "immutable
;; packet replay". The packet is gone (§7.5.9); a retry after the drafts
;; were retired is answered by completing the frozen plan, and that path
;; lands with W4" in the step after this one. An expectation edited to
;; match the gap would be asserting the gap.
;;
;; SUCCESSOR: W4" (plan persisted, the draft replaced by a later one,
;; retry completes from the plan's text and the later draft survives).
;;
;; The second row is kept: "a later draft survives a retry" is still the
;; property, and it is the half this file is about.
;; ⛔ WP-04's SECOND ROW IS RETIRED TOO, AND FOR A PLAINER REASON THAN
;; THE FIRST: it never performed the replay it claims to guard.
;;
;; It writes "later" and then reads it back. Nothing retries in between,
;; so a build in which a replay DELETED the current draft would not move
;; it -- which is exactly the defect this batch found and fixed. A row
;; whose label names a behaviour it does not exercise is a guard nobody
;; has.
;;
;; SUCCESSOR: `plan-completion.ss`, W4' -- "the retry is a replay", "it
;; wrote nothing", "and the new draft is untouched" -- which does retry.
(define old-snapshot (assq 'projection (cdr (call 'read id "--working-info" "--writer" "window-new"))))
(define old-hash (list-ref old-snapshot 5))
(define old-cut (format "~s" (list-ref old-snapshot 6)))
(call 'set id "src" "external change")
(define transferred (call 'write id "copied draft" "--writer" "window-new"
                         "--based-on" old-hash "--working-cut" old-cut))
(want "WP-05 a new namespace accepts verified historical provenance" (rpc-ok? transferred) #t)
(want "WP-05 first save never silently rebases a displayed old projection"
      (cadr (call 'commit id "--writer" "window-new")) 'stale-baseline)
(want "WP-06 mismatched historical hash is refused before creating a note"
      (cadr (call 'write id "forged" "--writer" "window-forged"
                   "--based-on" "wrong" "--working-cut" old-cut)) 'invalid-working-baseline)
(want "WP-06 refused provenance leaves no working note"
      (call 'drafts "--writer" "window-forged") '(ok (items)))
(define (snapshot who) (assq 'projection (cdr (call 'read id "--working-info" "--writer" who))))
(define (next-write previous text)
  (call 'write id text "--writer" "window-sequential"
        "--based-on" (list-ref previous 5) "--working-cut" (format "~s" (list-ref previous 6))
        "--working-parent-writer" (list-ref previous 2) "--working-parent" (list-ref previous 4)))
(call 'write id "one" "--writer" "window-sequential")
(define one (snapshot "window-sequential"))
(want "WP-07 first sequential draft commits" (rpc-ok? (call 'commit id "--writer" "window-sequential")) #t)
(want "WP-07 next save accepts the proven committed parent" (rpc-ok? (next-write one "two")) #t)
(define two (snapshot "window-sequential"))
(want "WP-07 next save advances only to its own commit baseline" (rpc-ok? (call 'commit id "--writer" "window-sequential")) #t)
(call 'set id "src" "external after two")
(want "WP-08 next edit remains durable after an intervening external write" (rpc-ok? (next-write two "three")) #t)
(want "WP-08 parent proof never silently advances past the committed parent"
      (cadr (call 'commit id "--writer" "window-sequential")) 'stale-baseline)
(printf "~a failures\nworking-projection complete\n" failures)
(exit (if (= failures 0) 0 1))
