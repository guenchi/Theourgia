;; THE FILES ALLOWED TO IMPORT (igropyr ...), AND NOTHING ELSE IS.
;;
;; Every use this core makes of igropyr goes through one of these, so the
;; dependency has a single seam: replacing igropyr, or vendoring it again
;; the way Z did, changes these files and none of the thirty-four
;; libraries and fixtures that import `(theourgia digest)`.
;;
;; TWO RULES READ THIS ONE LIST, so adding a facade is one edit:
;;   * `facade-gate.ss` requires the set of root sources importing
;;     igropyr to equal this list exactly -- in BOTH directions, and it
;;     also requires each name here to exist as a root `.ss`;
;;   * `facades.ss` requires the copied definitions to be gone from them.
;;
;; IT IS A LIST OF NAMES, which is the kind of rule that stays silent
;; about what it forgot -- so the gate is what shouts, not this file.
;; SIX, SINCE THE E BATCH: the three that were always here, and three
;; that wrap what a daemon and an evaluator need -- the scheduler,
;; unix-domain sockets, and child processes.
(digest wire ffi sched net proc)
