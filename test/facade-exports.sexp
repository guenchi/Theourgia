;; WHAT THE FORWARDING LIBRARIES EXPORT, AND NOTHING ELSE.
;;
;; This is a copy of the table in the design (§7.6.20), kept here so a
;; cell can compare it against what the libraries ACTUALLY export. The
;; design is the truth; this file is the reading the tree can take.
;;
;; NOTE: ONE MORE EXPORT IS A FAILURE, not a convenience. A facade exists so
;; that replacing igropyr touches these files and no caller; every name
;; that leaks through is a name a caller can come to depend on, and the
;; seam stops being one. `conn-peer-ip` is the example that is NOT here:
;; it answers #f on a unix socket, and a daemon that reached for it
;; would be identifying clients by a value that is always #f.
;;
;; NEVER: THERE IS NO `adapter-count` ANY MORE, AND ITS ABSENCE IS THE
;; POINT (§7.6.32 G). A count this library maintained about itself could
;; be told that something was gone while it was still running -- which is
;; exactly what happened: a "fix" that deregistered a live adapter left
;; the leak in place and blinded the only instrument that could see it,
;; and two rows written for that leak went green together. A leak is now
;; read from the runtime's own counters (`process-count`, `conn-count`,
;; `proc-count`), which cannot be told anything.
;;
;; NEVER: `conn-on-close!` IS NO LONGER HERE EITHER. There is one hook slot
;; per connection and the adapter owns it: that hook is how a close this
;; library did not initiate -- a peer error, or the half-write branch
;; closing in the writer's process -- still ends the adapter. A consumer
;; that took the slot would silently take that away. What a consumer
;; gets instead is the adapter's `#(DOWN pid reason)`, which the runtime
;; delivers and no code here can fail to send.
;;
;; NEVER: `conn-set-owner!` IS NOT HERE, AND ITS ABSENCE IS THE POINT.
;; igropyr delivers a connection's bytes straight into the mailbox of
;; whatever process owns the conn (tcp.sc: `(deliver (conn-owner c)
;; (vector 'tcp-data bv))`), so whoever can set the owner can make
;; itself the owner and read igropyr's vector shapes directly -- which
;; is the seam this whole arrangement exists to close. `net` owns the
;; conn itself, through a per-connection adapter process, and hands the
;; caller translated lists. Exporting the setter would leave the shorter
;; road open beside the long one, and the shorter road is the one people
;; take. Ruled 2026-09-17 with the adapter design (§7.6.20/§7.6.22).
;;
;; NOTE: `proc` CARRIES TWO NAMES IT DOES NOT FORWARD. `worker-rss` is three
;; platform FFIs and `worker-alive?` is `kill(pid, 0)`; neither exists in
;; igropyr. They are here because the gate is about WHO MAY TOUCH
;; igropyr, not about a facade being a pure forwarder.
;;
;; NOTE: AND `proc` HAS NO read-stop/read-start, WHICH `net` DOES. Measured
;; in igropyr's own test/proc-stream.sc: a child's streams are read from
;; the moment it is spawned (P2 collects stdout without ever starting a
;; read; P1 has to stop reads explicitly before it can show a read-start
;; mattering). So the guardian needs no start -- and has no backpressure
;; over a child's output either, which is why the output budget is
;; enforced by killing. The omission is deliberate; without this note the
;; next reader files it as a gap and the cell refuses the "fix" without
;; saying why.
((sched
   start-scheduler spawn spawn&link receive send self monitor demonitor
   link kill process-alive? process-count process-monitor-count sleep-ms)
 ;; conn-write-observed! AND conn-open? ARE THE CHANGE STREAM'S: the first
 ;; writes and answers how many bytes libuv still holds for the connection
 ;; (a write the peer is not taking), the second says whether a connection
 ;; is still usable after a write failed, so a terminal line can be tried.
 (net
   listen! stop-listen! listener-open? connect! conn-read-start! conn-read-stop!
   conn-write! conn-write-observed! conn-open? conn-close! exchange
   conn-ref? conn-ref-conn conn-ref-pid listen-ref?)
 (proc
   spawn-worker! worker-write! worker-close-stdin! worker-kill!
   worker-close! worker-alive? worker-rss
   worker-ref? worker-ref-proc worker-ref-pid)
 ;; FOUR, AND THE THIRTEEN THAT ARE NOT HERE ARE THE POINT. The core
 ;; speaks JSON in one place -- the MCP shell, where JSON is the
 ;; JSON-RPC envelope and the payload inside it is S-expression text.
 ;; Reading a frame and writing a reply needs to parse, to print and to
 ;; look up; it never builds a document, so the whole `json-set` /
 ;; `json-drop` / `json-push` / `json-insert` / `json-update` family and
 ;; the three classifiers are deliberately absent.
 (json
   string->json json->string json-ref json-ref*))
