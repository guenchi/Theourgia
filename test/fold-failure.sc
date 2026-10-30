#!r6rs
;; Copyright 2018 - 2026 guenchi
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

;; A FOLD AT A NAMED CUT THAT FAILED IS ANSWERED AS THAT FAILURE.
;;
;; A working verb that names a cut -- write with --based-on and
;; --working-cut, the parent's cut a write names, restore, and the
;; structural drafts' new, delete and move -- folds the store at that cut,
;; and the fold takes the store's shared lock. Through a daemon the lock
;; is taken on a budget, and a store held past it raises (error
;; store-busy (path <store>/lock)). Each of those folds swallowed every
;; raise into "no state", so a write whose cut was fine was answered
;; invalid-working-baseline (reason cut-unusable) while the store was only
;; busy; restore said no-text-for-version; and the parent's fold reached
;; the working verbs' backstop and said "Working storage failed".
;;
;; A cut that cannot be used -- it does not parse, or the state it names
;; cannot be rebuilt -- is still cut-unusable; that is a value the fold
;; returns, not a raise, and two rows pin it.
;;
;; THE BUSY LOCK IS PRODUCED TWO WAYS. In this process, by the lock
;; strategy seam (current-lock-acquire), as the commit's own rows do: it
;; raises the daemon strategy's datum on the store lock's shared takes
;; after a given count, so a row can fail one fold and not the one before
;; it. The count is read first and asserted, never assumed. Through a
;; real daemon, by a process of this fixture holding the store lock
;; exclusively until the row releases it (at most about 60 s, an escape
;; bound): no clock decides whether the write met the held lock.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia ffi) (theourgia request) (theourgia wire) (theourgia log)
        (theourgia working))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define pid-text (number->string (get-process-id)))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/fold-failure-" pid-text))
(when (file-exists? root) (error 'fold-failure "Use a fresh test root" root))
(mkdir-p! root)
(define home (string-append root "/home"))
(putenv "THEOURGIA_HOME" home)

(define (fresh-store name)
  (let* ((store (string-append root "/" name))
         (init (rpc-dispatch store '(init) "test")))
    (cons store (cadr (assq 'writer (cdr init))))))
(define (call-in store writer . args)
  (rpc-dispatch store
                (if (or (memq (car args) '(write restore drafts discard commit))
                        (member "--working-info" args))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define (inserted a) (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev))))
(define (cut-of state) (format "~s" (reduce-applied-cut state)))

;; ---- the lock strategy seam ------------------------------------------------
;;
;; The store lock's SHARED takes are counted; past AFTER of them, each
;; raises what the daemon's strategy raises past its budget. Every other
;; take (exclusive, another path) goes to the plain strategy.
(define plain (current-lock-acquire))
(define (store-lock store) (string-append store "/lock"))
(define (with-shared-takes store after thunk)
  (let ((n 0))
    (let ((answer
           (parameterize ((current-lock-acquire
                            (lambda (path mode)
                              (if (and (eq? mode 'shared) (string=? path (store-lock store)))
                                  (begin
                                    (set! n (+ n 1))
                                    (if (and after (> n after))
                                        (raise (list 'error 'store-busy (list 'path path)))
                                        (plain path mode)))
                                  (plain path mode)))))
             (thunk))))
      (list n answer))))
;; -> the answer with the store busy after AFTER shared takes
(define (busy-after store after thunk) (cadr (with-shared-takes store after thunk)))
;; -> the number of shared takes THUNK makes with nothing busy
(define (takes store thunk) (car (with-shared-takes store #f thunk)))
(define (busy-answer store) (list 'error 'store-busy (list 'path (store-lock store))))
(define cut-unusable '(error invalid-working-baseline (reason cut-unusable)))

(define s1 (fresh-store "store"))
(define store (car s1))
(define writer (cdr s1))
(define (call . args) (apply call-in store writer args))
(define (state) (open-and-reduce store))
(define A (inserted (call 'insert "--title" "A" "--text" "old")))
(define (draft-file id) (string-append (writer-directory store writer) "/working/" id))

;; ---- FB-1 write --based-on: the fold at the named cut ----------------------

(define h0 (block-hash (state) A))
(define c0 (cut-of (state)))
(define st0 (state))
(want "FB-1 write --based-on with the store busy at its cut's fold answers store-busy, not cut-unusable"
      (busy-after store 0 (lambda () (working-write! store st0 writer A "mine" #f h0 c0)))
      (busy-answer store))
(want "FB-1 TWIN: and nothing was written" (file-exists? (draft-file A)) #f)

;; ---- FB-2 the cut that cannot be used is still cut-unusable ---------------
;;
;; PIN: what stays. A cut that does not parse is refused before any fold,
;; so a busy store does not change its answer; a well-formed cut naming a
;; writer this store never saw folds to a verdict, not a raise.
(want "FB-2 a cut that does not parse, with the store busy, is still cut-unusable"
      (busy-after store 0 (lambda () (working-write! store st0 writer A "mine" #f h0 "not-a-cut")))
      cut-unusable)
(want "FB-2 a cut this store cannot reach, with nothing busy, is still cut-unusable"
      (working-write! store st0 writer A "mine" #f h0 "((\"nosuchwr\" . 9))")
      cut-unusable)

;; ---- FB-3 the fold at the parent's cut -------------------------------------
;;
;; B is drafted and committed, so a later write can name that draft as its
;; parent (--working-parent-writer, --working-parent). The write folds
;; twice: at its own cut, then at the parent's. The first count is the
;; write's own fold alone (a wrong hash, refused after it); the second is
;; the same with the parent named. The busy row lets the first fold
;; through and fails the second.
(define B (inserted (call 'insert "--title" "B" "--text" "b0")))
(call 'write B "b1")
(define vb (list-ref (assq 'projection (cdr (call 'read B "--working-info"))) 4))
(define commit-b (call 'commit B))
(define st1 (state))
(define hb (block-hash st1 B))
(define cb (cut-of st1))
(define wrong (make-string 64 #\a))
(define own-fold (takes store (lambda () (working-write! store st1 writer B "b2" #f wrong cb))))
(define both-folds (takes store (lambda () (working-write! store st1 writer B "b2" #f wrong cb writer vb))))
(want "FB-3 READING: B's draft committed, its own fold takes the shared lock, and naming the parent adds a fold"
      (list (rpc-ok? commit-b) (> own-fold 0) (> both-folds own-fold))
      '(#t #t #t))
(want "FB-3 write with a parent, the store busy at the parent's fold only, answers store-busy"
      (busy-after store own-fold (lambda () (working-write! store st1 writer B "b2" #f hb cb writer vb)))
      (busy-answer store))

;; ---- FB-5 / FB-6 / FB-7 the structural drafts' cut -------------------------

(define src-dir (string-append root "/src"))
(mkdir-p! src-dir)
(call-with-output-file (string-append src-dir "/g.sc")
  (lambda (p) (put-string p "(define (g) 1)\n")))
(define imported (call 'import-code src-dir))
(define st2 (state))
(define c2 (cut-of st2))
(define holder (path-holder st2 "g.sc"))

(want "FB-5 new with the store busy at its cut's fold answers store-busy"
      (busy-after store 0 (lambda () (working-new! store st2 writer "n.sc" '() (string->utf8 "(define (n) 1)\n") c2)))
      (busy-answer store))
(define new-h (working-new! store st2 writer "h.sc" '() (string->utf8 "(define (h) 1)\n") c2))
(want "FB-5 TWIN: the same new with nothing busy is saved"
      (and (pair? new-h) (car new-h))
      'ok)

(want "FB-6 delete with the store busy at its cut's fold answers store-busy"
      (busy-after store 0 (lambda () (working-delete! store st2 writer holder c2)))
      (busy-answer store))
(want "FB-6 TWIN: the imported file has a holder, and its delete with nothing busy is saved"
      (list (rpc-ok? imported) (string? holder)
            (let ((a (working-delete! store st2 writer holder c2))) (and (pair? a) (car a))))
      '(#t #t ok))

;; THE MOVE LOADS THE STATE ITSELF before its fold (the draft lock held),
;; so the busy row lets that load's takes through and fails the fold.
(define load-takes (takes store (lambda () (open-and-reduce store))))
(define h-key (provisional-id "h.sc"))
(define h-version (cadr (assq 'version (cdr new-h))))
(want "FB-7 READING: a load takes the shared lock"
      (> load-takes 0)
      #t)
(want "FB-7 move with the store busy at its cut's fold answers store-busy"
      (busy-after store load-takes (lambda () (working-move-structural! store writer h-key h-version c2)))
      (busy-answer store))
(want "FB-7 TWIN: the same move with nothing busy moves it"
      (let ((a (working-move-structural! store writer h-key h-version c2))) (and (pair? a) (car a)))
      'ok)

;; ---- FB-4 restore: the text of an item that had no change ------------------
;;
;; A plan whose identity a second record claims is in conflict and its
;; consumption is revoked; restore brings the draft back. An item whose
;; bytes were the committed text carried no text in the plan, so restore
;; folds at the item's cut to read it. A store with such a conflict takes
;; no further write, so this has a store of its own.
(define s2 (fresh-store "store-r"))
(define rstore (car s2))
(define rwriter (cdr s2))
(define (rcall . args) (apply call-in rstore rwriter args))
(define C (inserted (rcall 'insert "--title" "C" "--text" "c")))
(rcall 'write C "c")
(define vc (list-ref (assq 'projection (cdr (rcall 'read C "--working-info"))) 4))
(define rcursor
  (string-append rwriter ":" (number->string (cdr (assoc rwriter (reduce-applied-cut (open-and-reduce rstore)))))))
(define rcommit (rcall 'commit C "--req" "R1" "--cursor" rcursor "--working-version" vc))
(define plan-ev
  (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) (store-evidence rstore (cons rwriter "R1"))))
(define rival
  (and plan-ev (encode-record 1 1789000000001 (ev-actor plan-ev) '() (storable-encode (ev-payload plan-ev)))))
(define rival-published (and rival (car (log-publish! rstore "rivalzzz" 1 rival (segment-sha rival)))))
(define revoked (rcall 'drafts))
(define st3 (open-and-reduce rstore))
(want "FB-4 READING: the unchanged draft was committed, the rival published, and drafts lists it revoked"
      (list (rpc-ok? rcommit) rival-published
            (map car (cdr (assq 'items (cdr revoked)))))
      '(#t published (revoked)))
(want "FB-4 restore with the store busy at the item's cut's fold answers store-busy, not no-text-for-version"
      (busy-after rstore 0 (lambda () (working-restore! rstore st3 rwriter vc)))
      (busy-answer rstore))
(define restored (with-shared-takes rstore #f (lambda () (working-restore! rstore st3 rwriter vc))))
(want "FB-4 TWIN: the same restore with nothing busy folds (it takes the shared lock) and restores"
      (list (> (car restored) 0) (and (pair? (cadr restored)) (car (cadr restored))))
      '(#t ok))

;; ---- FB-8 / FB-9 through a daemon -------------------------------------------
;;
;; A store served by a daemon of this tree, a block in it, and the command
;; line's write --based-on sent to that daemon; its answer is the first
;; datum on its stdout (stderr, which names the machine home, is kept
;; apart). For FB-8 a process of this
;; fixture holds the store lock exclusively until the row has the answer
;; (about 60 s at most, see THE HOLDER): the daemon's writer takes the
;; shared lock on its budget and gives up.
(define s3 (fresh-store "store-d"))
(define dstore (car s3))
(define D (inserted (rpc-dispatch dstore '(insert "--title" "D" "--text" "d") "test")))
(define dstate (open-and-reduce dstore))
(define hd (block-hash dstate D))
(define cd (cut-of dstate))
(define run-dir (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/ff-" pid-text))
(system (string-append "rm -rf " run-dir "; mkdir -p " run-dir))
(define sock (string-append run-dir "/d.sock"))
(define dlog (string-append root "/daemon.log"))
(define dpid-file (string-append root "/daemon.pid"))
(define (file-text p) (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (sleep-ms ms) (sleep (make-time 'time-duration (* ms 1000000) 0)))
(define (contains? text part)
  (let ((n (string-length text)) (m (string-length part)))
    (let loop ((i 0)) (cond ((> (+ i m) n) #f) ((string=? (substring text i (+ i m)) part) #t) (else (loop (+ i 1)))))))
(define (first-datum text)
  (guard (e (#t (list 'unreadable text)))
    (let ((d (read (open-string-input-port text)))) (if (eof-object? d) (list 'no-output) d))))

(system (string-append "THEOURGIA_HOME=" home " scheme --script ../theourgiad.sc serve " dstore
                       " --socket " sock " > " dlog " 2>&1 < /dev/null & echo $! > " dpid-file))
(define serving
  (let up ((k 0))
    (cond ((file-exists? sock) #t)
          ((> k 600) #f)
          (else (sleep-ms 50) (up (+ k 1))))))
(define (cli-write out cut)
  (system (string-append "env -u THEOURGIA_LOCAL THEOURGIA_HOME=" home " scheme --script ../theourgia.sc write " D
                         " mine --writer w1 --based-on " hd " --working-cut '" cut "' --store " dstore
                         " --socket " sock " > " out " 2> " out ".err < /dev/null"))
  (first-datum (file-text out)))

;; THE HOLDER: the store lock, exclusive, until the release file exists,
;; or after 1200 polls of 50 ms (about 60 s) at most, so a row that never
;; releases it leaves no holder behind. A hold that ran out before the
;; answer reads as a failed row, not a passed one: the write would then
;; meet a free lock and answer something other than store-busy.
(define holder-script (string-append root "/holder.sc"))
(define holder-log (string-append root "/holder.log"))
(define release (string-append root "/release"))
(call-with-output-file holder-script
  (lambda (port)
    (for-each (lambda (l) (display l port) (newline port))
      (list "(import (chezscheme) (only (theourgia ffi) lock-acquire! lock-release!))"
            (string-append "(define l (lock-acquire! \"" (store-lock dstore) "\" 'exclusive))")
            "(display \"HELD\\n\") (flush-output-port (current-output-port))"
            (string-append "(let wait ((k 0)) (unless (or (file-exists? \"" release "\") (> k 1200))"
                           " (sleep (make-time 'time-duration 50000000 0)) (wait (+ k 1))))")
            "(lock-release! l)"
            "(display \"RELEASED\\n\")"))))
(system (string-append "scheme --script " holder-script " > " holder-log " 2>&1 < /dev/null &"))
(define held
  (let up ((k 0))
    (cond ((contains? (file-text holder-log) "HELD") #t)
          ((> k 400) #f)
          (else (sleep-ms 25) (up (+ k 1))))))
(define t0 (real-time))
(define held-answer (cli-write (string-append root "/fb8.out") cd))
(define held-ms (- (real-time) t0))
(call-with-output-file release (lambda (p) (display "go" p)))
(define released
  (let up ((k 0))
    (cond ((contains? (file-text holder-log) "RELEASED") #t)
          ((> k 400) #f)
          (else (sleep-ms 25) (up (+ k 1))))))
(printf "reading: FB-8 the daemon's answer came after ~a ms\n" held-ms)
(want "FB-8 through a daemon, a write --based-on while the store lock is held past the budget answers store-busy, not cut-unusable"
      (list serving held held-answer released)
      (list #t #t (busy-answer dstore) #t))

(want "FB-9 through a daemon, a cut this store cannot reach is still cut-unusable"
      (cli-write (string-append root "/fb9.out") "((\"nosuchwr\" . 9))")
      cut-unusable)

;; THE DAEMON IS STOPPED BY ITS PID, and the row waits for it to be gone.
(define dpid (let ((t (file-text dpid-file))) (string->number (list->string (filter char-numeric? (string->list t))))))
(when dpid (system (string-append "kill " (number->string dpid) " 2>/dev/null")))
(define daemon-gone
  (let up ((k 0))
    (cond ((not dpid) #f)
          ((not (= 0 (system (string-append "kill -0 " (number->string dpid) " 2>/dev/null")))) #t)
          ((> k 200) (system (string-append "kill -9 " (number->string dpid) " 2>/dev/null")) #f)
          (else (sleep-ms 50) (up (+ k 1))))))
(want "FB-9 TWIN: the daemon this fixture started is gone once stopped" daemon-gone #t)
(system (string-append "rm -rf " run-dir))

(printf "rows: ~a\n~a failures\nfold-failure complete\n" rows bad)
(exit (if (zero? bad) 0 1))
