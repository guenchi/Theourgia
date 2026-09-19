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

;; Which socket a store's daemon listens on, and who decides.
;;
;; ⭐ ONE FUNCTION, IMPORTED BY BOTH SIDES. The daemon creates the socket
;; and every client looks for it; written twice they are two rules, and
;; the day they differ a client starts a second daemon for a store that
;; already has one.
;;
;; ⛔ AND IT WAS WRITTEN TWICE, WITH THE COPIES DISAGREEING. Until this
;; batch `cli.ss` and `mcp/server.ss` defaulted to `<store>/socket` while
;; `daemon.ss` had a run-root rule that nothing reached -- so the rule the
;; README described was never the rule that ran.

(import (chezscheme)
        (only (theourgia client) socket-path store-key run-root answer-field)
        (only (theourgia ffi) unix-socket-connect fd-open fd-close path-case-sensitive?))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define here (string-append "/tmp/csock-" (number->string (get-process-id))))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (remove-duplicates xs)
  (let loop ((xs xs) (seen '()))
    (cond ((null? xs) (reverse seen))
          ((member (car xs) seen) (loop (cdr xs) seen))
          (else (loop (cdr xs) (cons (car xs) seen))))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(system (string-append "rm -rf " here "; mkdir -p " here "/store " here "/other"))

;; ---- CS-1: one store, several spellings ---------------------------------------
;;
;; ⛔ FOUR SPELLINGS OF ONE DIRECTORY. Measured on the key this replaced:
;; it concatenated the spelling in front of the device and inode, so these
;; four produced FOUR keys while `stat` reported one device and one inode
;; for all of them -- and its comment claimed the opposite.
(define store (string-append here "/store"))
(system (string-append "ln -s " store " " here "/link"))

(want "CS-1 every spelling of one store gives one socket"
      (let ((keys (map store-key
                       (list store
                             (string-append store "/")
                             (string-append here "/link")
                             (string-append here "/./store")))))
        (if (apply string=? keys) 'one-key (list 'said keys)))
      'one-key)

(want "CS-1 TWIN: and a different store is a different socket"
      (if (string=? (store-key store) (store-key (string-append here "/other")))
          'SAME-KEY-FOR-TWO-STORES 'different)
      'different)

;; ⚠️ A STORE THAT DOES NOT EXIST YET STILL GETS ONE. `init` is a verb and
;; `serve` on a fresh directory is a thing people do; a key that raised
;; here would make the first call to a new store impossible.
(want "CS-1 a store that does not exist yet still has a key"
      (let ((k (store-key (string-append here "/not-created"))))
        (if (and (string? k) (= 16 (string-length k))) 'has-one (list 'said k)))
      'has-one)

;; ⚠️ A PATH THAT IS NOT ASCII. The key is taken over the UTF-8 bytes of
;; the resolved name, so this has to work rather than raise.
(define wide (string-append here "/\x5b58;\x50a8;"))
(system (string-append "mkdir -p " wide))

;; ⛔ THIS FIXTURE'S RUN ROOT IS ITS OWN, from here on. `socket-path`
;; reads it every time, and unset it is the user's real one.
(putenv "THEOURGIA_RUN" (string-append here "/run"))

(want "CS-1 a store whose path is not ASCII has a key, and its own"
      (let ((k (store-key wide)))
        (list (if (and (string? k) (= 16 (string-length k))) 'has-one (list 'said k))
              (if (string=? k (store-key store)) 'COLLIDES 'its-own)))
      '(has-one its-own))

;; ---- CS-2: the run root decides where, and it is read each time ----------------
(want "CS-2 the socket is under the run root, and THEOURGIA_RUN moves it"
      (let* ((before (socket-path store))
             (moved (begin (putenv "THEOURGIA_RUN" (string-append here "/elsewhere"))
                           (socket-path store))))
        ;; ⚠️ RESTORED TO THIS FIXTURE'S OWN RUN ROOT, and the two wrong
        ;; answers are both recorded here because both were tried.
        ;;
        ;; ⛔ NOT "": that left every later row computing a socket under
        ;; the filesystem root, and the daemon panicked at boot trying to
        ;; mkdir there -- a cell breaking the rows after it. (The product
        ;; now treats an empty value as unset, as the rest of the tree
        ;; already did.)
        ;;
        ;; ⛔ AND NOT `$HOME/.theourgia/run` EITHER, which is what it was
        ;; restored to next. That is the product's REAL default -- the
        ;; user's own run root -- and CS-3 below starts a daemon with no
        ;; `--socket`, so every run of this file bound a socket and left a
        ;; lock directory in it. Found by counting what was in there: two
        ;; directories holding nothing but a zero-byte `.socket.lock`,
        ;; whose keys matched no store that still existed.
        (putenv "THEOURGIA_RUN" (string-append here "/run"))
        (list (if (contains? moved (string-append here "/elsewhere")) 'moved (list 'said moved))
              (if (string=? before moved) 'IGNORED-THE-VARIABLE 'and-it-was-read)))
      '(moved and-it-was-read))

;; ⛔ NOT BESIDE THE STORE, which is the defect this replaces. `sun_path`
;; holds 104 bytes; a store-adjacent socket under a deep path simply fails
;; to bind, and the daemon then reports `listener-down` and looks broken.
(want "CS-2 the socket is not inside the store directory"
      (if (contains? (socket-path store) store) 'BESIDE-THE-STORE 'elsewhere)
      'elsewhere)

;; ---- CS-3: and the daemon agrees, because it imports the same function --------
;;
;; ⛔ THE ROW THAT MAKES THE OTHERS MEAN SOMETHING. Two copies of a rule
;; can each be self-consistent; what matters is that the process creating
;; the socket and the process looking for it compute the same path.
(define (cli args out-file . env)
  (system (string-append
            (if (pair? env) (car env) "")
            " CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
            "scheme --script ../cli.ss " args " > " out-file " 2>&1"))
  (file-text out-file))

(cli (string-append "init --store " store " --wire") (string-append here "/init.txt")
     "THEOURGIA_LOCAL=1")

(system (string-append "( THEOURGIA_TRACE=1 CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                       "scheme --script ../cli.ss serve " store
                       " > " here "/serve.txt 2>&1 & echo $! > " here "/pid )"))
(system "sleep 5")

(want "CS-3 the daemon binds the path the client computes"
      (let ((said (file-text (string-append here "/serve.txt"))))
        (list (if (contains? said (socket-path store)) 'same-path (list 'said said))
              (if (file-exists? (socket-path store)) 'and-it-is-there 'NOT-THERE)))
      '(same-path and-it-is-there))

;; ⛔ AND A CALL THAT NAMES THE STORE ANOTHER WAY REACHES THAT DAEMON.
;; ⚠️ The answer alone proves nothing -- a local run gives the same bytes
;; on purpose -- so the daemon's own dispatch trace is what is counted.
(define (dispatches)
  (let ((t (file-text (string-append here "/serve.txt"))))
    (let loop ((i 0) (n 0))
      (cond ((> (+ i 16) (string-length t)) n)
            ((string=? (substring t i (+ i 16)) "daemon-dispatch ") (loop (+ i 16) (+ n 1)))
            (else (loop (+ i 1) n))))))

(define before-calls (dispatches))

(for-each (lambda (spelling)
            (cli (string-append "outline --store " spelling " --wire")
                 (string-append here "/out.txt")))
          (list store (string-append store "/") (string-append here "/link")))
(system "sleep 1")

(want "CS-3 three spellings of the store all reach that one daemon"
      (let ((n (- (dispatches) before-calls)))
        (if (= n 3) 'three-dispatches (list 'dispatched n)))
      'three-dispatches)

;; ⛔ AND AN EXPLICIT `--socket` STILL WINS, so a caller who knows where a
;; daemon is can say so.
(want "CS-3 an explicit --socket is used instead of the computed one"
      (let ((said (cli (string-append "outline --store " store
                                      " --socket " here "/nothing-here --wire")
                       (string-append here "/explicit.txt"))))
        ;; Nothing is listening there, so this must NOT have been answered
        ;; by the daemon above; it falls back to answering locally.
        (list (if (contains? said "(ok (text") 'answered (list 'said said))
              (if (= (dispatches) (+ before-calls 3)) 'and-not-by-the-daemon 'THE-DAEMON-SAW-IT)))
      '(answered and-not-by-the-daemon))

;; ---- CS-4: the key does not depend on WHEN you ask -----------------------
;;
;; ⭐ A STORE'S KEY BEFORE IT EXISTS MUST BE ITS KEY AFTER. `init` creates
;; the store, so a caller who computes the socket path first and a caller
;; who computes it later are the ordinary case, not a corner. When the two
;; disagree the second caller finds no socket where it looked and runs the
;; store locally -- an answer, from the right store, with no sign that it
;; bypassed a daemon that was sitting there.
;;
;; ⛔ SELF-CONSISTENCY IS NOT THE PROPERTY. Returning the caller's raw
;; spelling unchanged would make the first row below green and is still
;; wrong; the TWIN pins the key to the RESOLVED name, which is what the
;; daemon will compute once the store is real. Both rows are needed.
;;
;; The symlink is built here rather than borrowed from the platform: on
;; macOS /tmp is already a symlink to /private/tmp and this would pass for
;; the wrong reason, and on a machine where it is not, it would not run.

(define ln-base (string-append here "/ln-probe"))
(system (string-append "mkdir -p " ln-base "/real; ln -s " ln-base "/real " ln-base "/link"))

(define via-link (string-append ln-base "/link/deep/a"))
(define via-real (string-append ln-base "/real/deep/a"))

(define key-before-link (store-key via-link))
(define key-before-real (store-key via-real))

(system (string-append "mkdir -p " ln-base "/real/deep/a"))

(define key-after-link (store-key via-link))
(define key-after-real (store-key via-real))

(want "CS-4 the key a store gets before it exists is the key it gets after"
      (if (string=? key-before-link key-after-link)
          'same
          (list 'before key-before-link 'after key-after-link))
      'same)

(want "CS-4 TWIN: and that key is the resolved store's, not the spelling asked for"
      (if (string=? key-before-link key-before-real)
          'same
          (list 'through-the-symlink key-before-link 'resolved key-before-real))
      'same)

;; The four agreeing at once is the whole claim: two spellings, two
;; instants, one socket.
(want "CS-4 two spellings and two instants give one key"
      (length (remove-duplicates
               (list key-before-link key-before-real key-after-link key-after-real)))
      1)

;; A trailing slash is the branch that climbs without consuming a name.
;; ⚠️ THIS ROW SAID "ABSENT" AND EXERCISED A DIRECTORY THAT EXISTS. It
;; ran after the `mkdir` above, so `real-path` succeeded and the climb --
;; the branch it was written to guard -- was never reached. The label was
;; the only thing making it about an absent store.
(define slashy (string-append ln-base "/real/deep/absent"))

(want "CS-4 a trailing slash on a store that really is absent keeps the same key"
      (if (string=? (store-key (string-append slashy "/")) (store-key slashy))
          'same
          (list 'slashed (store-key (string-append slashy "/"))
                'plain (store-key slashy)))
      'same)

;; ⛔ AND THE ROW KNOWS THE STORE IS ABSENT, rather than trusting the
;; label. Without this the row can drift back to testing a directory that
;; exists the moment something above it creates one.
(want "CS-4 TWIN: and that store really is absent"
      (if (file-exists? slashy) 'IT-EXISTS 'absent)
      'absent)

;; The present case is worth a row of its own, and is now labelled as one.
(want "CS-4 a trailing slash on a store that exists keeps the same key"
      (if (string=? (store-key (string-append via-link "/")) key-after-link)
          'same
          (list 'slashed (store-key (string-append via-link "/"))
                'plain key-after-link))
      'same)

;; ---- CS-5 the spelling below the resolved prefix ---------------------------
;;
;; ⛔ SAME DEFECT AS CS-4, DIFFERENT AXIS. The components below the
;; longest existing prefix were appended verbatim, so while the store did
;; not exist `<dir>/x/a`, `<dir>/x/./a` and `<dir>/x/y/../a` hashed to
;; three different keys -- and the moment the store appeared `realpath`
;; folded them and all three agreed. A caller that computed the path one
;; way before `init` and the other way after looked for two sockets.
;;
;; ⚠️ CS-4 DID NOT CATCH THIS. It varied WHEN the key was asked for and
;; held the spelling fixed; this varies the spelling. The two rows look
;; alike and ask different questions.
(define dots-base (string-append here "/dots"))
(define plain (string-append dots-base "/x/a"))
(define dotted (string-append dots-base "/x/./a"))
(define updown (string-append dots-base "/x/y/../a"))

(define key-while-absent (store-key dotted))

(want "CS-5 dot components below an absent store do not change its key"
      (length (remove-duplicates (list (store-key plain) key-while-absent (store-key updown))))
      1)

;; ⛔ AND IT IS THE KEY THE STORE GETS ONCE IT IS THERE, which is what
;; makes the folding right rather than merely self-consistent.
;;
;; ⚠️ THE ABSENT KEY IS TAKEN BEFORE THE `mkdir`, AND IT WAS NOT. Written
;; with both sides read afterwards, this compared two post-creation keys
;; -- it could not have failed, and the comment above it claimed exactly
;; the comparison it was not making.
(system (string-append "mkdir -p " plain))

(want "CS-5 TWIN: and it is the key the store has once it exists"
      (if (string=? key-while-absent (store-key plain)) 'same
          (list 'absent key-while-absent 'present (store-key plain)))
      'same)

;; ---- CS-6 the two cases lexical folding got wrong --------------------------
;;
;; ⛔ THE FIRST FIX FOR CS-5 FOLDED `.` AND `..` AS TEXT, justified by
;; "the prefix came from realpath so it holds no symlinks, and the
;; components below it do not exist and therefore cannot be any". The
;; second half does not follow, and these two rows are the cases that
;; falsify it. Each component is now applied to a path that is real as far
;; as it goes, and resolved again as it is applied.

(define cs6 (string-append here "/cs6"))
(system (string-append "mkdir -p " cs6 "/real; ln -s " cs6 "/real " cs6 "/link"))

;; ⛔ `..` CAN BRING BACK A COMPONENT THAT EXISTS. `link/../link/s` folds
;; textually to `link/s`, and `link` is a symlink -- which lexical folding
;; then never resolves, so the key was the link's and not the target's.
(want "CS-6 a `..` that brings an existing symlink back into the path still resolves it"
      (length (remove-duplicates
               (list (store-key (string-append cs6 "/link/../link/s"))
                     (store-key (string-append cs6 "/link/s"))
                     (store-key (string-append cs6 "/real/s")))))
      1)

;; ⛔ AND `..` MAY NOT CLIMB PAST THE ROOT. Folded textually it simply ran
;; out of components to pop and left the `..` in the name, so two
;; spellings of one absolute path hashed differently.
(want "CS-6 a `..` above the filesystem root does not change the path"
      (if (string=? (store-key (string-append "/../.." cs6 "/real/s"))
                    (store-key (string-append cs6 "/real/s")))
          'same
          (list 'climbed (store-key (string-append "/../.." cs6 "/real/s"))
                'plain (store-key (string-append cs6 "/real/s"))))
      'same)

;; ⚠️ AND THE TWIN THAT KEEPS THE FOLDING HONEST: two genuinely different
;; stores must still get different keys. Resolving everything to one name
;; would pass both rows above.
(want "CS-6 TWIN: two different stores still have different keys"
      (if (string=? (store-key (string-append cs6 "/real/s"))
                    (store-key (string-append cs6 "/real/other")))
          'COLLIDES 'distinct)
      'distinct)


;; ⛔ AND THE MISSING COMPONENT GOES BEFORE THE TRAVERSAL, or neither row
;; above tests the walker. In `link/../link/s` the whole of
;; `link/../link` exists, so `realpath` resolves it in one call and only
;; `s` ever reaches the code being tested: an implementation that folded
;; `.` and `..` as text and resolved nothing afterwards passes both. Put a
;; component that does NOT exist ahead of the `..` and the suffix walker
;; has to do the work -- and a lexical one answers the symlink's key
;; instead of its target's.
(want "CS-6 a `..` after a component that does not exist still resolves what follows"
      (if (string=? (store-key (string-append cs6 "/missing/../link/s"))
                    (store-key (string-append cs6 "/real/s")))
          'same
          (list 'walked-to (store-key (string-append cs6 "/missing/../link/s"))
                'target (store-key (string-append cs6 "/real/s"))))
      'same)

;; ---- CS-7 one store, one key, on a filesystem that folds case ------------
;;
;; ⛔ MEASURED DEFECT: the key changed when the store was created. On a
;; case-insensitive filesystem `Foo` and `foo` name ONE directory, but
;; while it does not exist yet `resolved-name` keeps whichever spelling
;; the caller typed -- two keys -- and once it exists `realpath` answers
;; the filesystem's own spelling for both. So a client that computed a key
;; before `init` and used it after was using a key nothing else would
;; compute. Measured on g-r3: absent `Foo`/`foo` -> two keys, and `foo`'s
;; key CHANGED once the store existed.
;;
;; ⚠️ THE ROWS ASK THE FILESYSTEM FIRST, because folding is right only
;; where two spellings really are one store.

(define cs7 (string-append here "/cs7"))
(system (string-append "mkdir -p " cs7))

(define cs7-folds (eq? #f (path-case-sensitive? cs7)))

(want "CS-7 the filesystem under the test directory was asked about case"
      (if (memq (path-case-sensitive? cs7) '(#t #f)) 'answered
          (list 'unknown-here (path-case-sensitive? cs7)))
      'answered)

;; ⚠️ IF THIS RUN IS ON A CASE-SENSITIVE FILESYSTEM the two rows below
;; assert the OTHER half -- two spellings are two stores -- which is the
;; same rule read from its other side. Neither is skipped.
(want "CS-7 two spellings of an absent store agree with what the filesystem says"
      (let ((a (store-key (string-append cs7 "/Foo")))
            (b (store-key (string-append cs7 "/foo"))))
        (if (eq? cs7-folds (string=? a b)) 'agrees (list 'folds cs7-folds 'same-key (string=? a b))))
      'agrees)

(want "CS-7 and the key a store had before it existed is the key it has after"
      (let* ((named (string-append cs7 "/Foo"))
             (other (string-append cs7 "/foo"))
             (before-named (store-key named))
             (before-other (store-key other)))
        (system (string-append "mkdir -p " named))
        (list (if (string=? before-named (store-key named)) 'named-unchanged
                  (list 'named-was before-named 'now (store-key named)))
              (if (string=? before-other (store-key other)) 'other-unchanged
                  (list 'other-was before-other 'now (store-key other)))))
      '(named-unchanged other-unchanged))

;; ⛔ THE TWIN, ON A FILESYSTEM THAT REALLY DOES DISTINGUISH THEM. A build
;; that folded everywhere would give one key to two different stores,
;; which is the worse direction, and nothing above would catch it: this
;; run's own directory folds. A case-sensitive volume is made for the
;; purpose.
;;
;; ⚠️ IT IS SKIPPED ONLY WHERE ONE CANNOT BE MADE, and says so with the
;; reason, because a row that quietly disappears is not a row.
(define cs-volume (string-append here "/cs.dmg"))
(define cs-mount (string-append here "/csmnt"))
(define cs-made?
  (and (zero? (system (string-append
                       "hdiutil create -size 10m -fs 'Case-sensitive HFS+' -volname CSKEY -quiet "
                       cs-volume " >/dev/null 2>&1")))
       (zero? (system (string-append
                        "hdiutil attach " cs-volume " -mountpoint " cs-mount
                        " -nobrowse -quiet >/dev/null 2>&1")))))

(if (not cs-made?)
    (printf "ok CS-7 TWIN skipped: no case-sensitive volume could be made here (hdiutil create/attach failed; on a platform without hdiutil, mount one by hand and set the path)\n")
    (begin
      (want "CS-7 TWIN: the made volume really does distinguish case"
            (path-case-sensitive? cs-mount)
            #t)
      (want "CS-7 TWIN: and there two spellings are two stores, before and after"
            (let* ((a (string-append cs-mount "/Foo"))
                   (b (string-append cs-mount "/foo"))
                   (absent (string=? (store-key a) (store-key b))))
              (system (string-append "mkdir -p " a " " b))
              (list (if absent 'COLLIDED-WHILE-ABSENT 'distinct-while-absent)
                    (if (string=? (store-key a) (store-key b))
                        'COLLIDED-ONCE-PRESENT 'distinct-once-present)))
            '(distinct-while-absent distinct-once-present))))

;; ---- CS-8 a refused connect keeps no descriptor --------------------------
;;
;; ⛔ MEASURED: 20 refused connects leaked 20 descriptors. `let` does not
;; order its bindings, so the socket was created before the address it
;; would be given, and a path too long for `sun_path` raised out with the
;; socket already open and nothing holding it. A client that retries --
;; which is the ordinary response to a refusal -- ran out of descriptors.
;;
;; ⚠️ MEASURED BY ASKING FOR ONE: the lowest free descriptor is what the
;; next open gets, so if nothing leaked it is the same number before and
;; after.
(define (next-free-fd)
  (let ((probe (guard (e (#t #f)) (fd-open "/dev/null" '(read)))))
    (when probe (fd-close probe))
    probe))

(define too-long-path (string-append "/tmp/" (make-string 120 #\x)))

(want "CS-8 a refused connect leaves no descriptor behind"
      (let ((before (next-free-fd)))
        (let loop ((i 0))
          (when (< i 20)
            (guard (e (#t #f)) (unix-socket-connect too-long-path 100))
            (loop (+ i 1))))
        (let ((after (next-free-fd)))
          (if (equal? before after) 'none-leaked (list 'before before 'after after))))
      'none-leaked)

;; ⛔ THE TWIN: the probe has to be able to see a leak at all. Holding one
;; descriptor open must move the answer, or the row above is measuring
;; nothing.
(want "CS-8 TWIN: and the measurement can see a descriptor that is held"
      (let* ((before (next-free-fd))
             (held (fd-open "/dev/null" '(read)))
             (during (next-free-fd)))
        (fd-close held)
        (if (equal? before during) 'BLIND 'sees-it))
      'sees-it)

(when cs-made?
  (system (string-append "hdiutil detach " cs-mount " -quiet >/dev/null 2>&1")))


;; ---- the client's import closure, asserted where the client is worked on --
;;
;; ⛔ `closures.ss` HAS THIS ROW, AND IT IS NOT WHERE THE WORK HAPPENS.
;; Anyone changing the client runs these suites; a stray import of the
;; core, the scheduler or the networking library would be caught only by
;; a file they had no reason to run, and only if the whole suite ran. The
;; walk itself is `import-walk.scm`, shared, so this is the same
;; measurement taken in an extra place -- ⛔ not a second implementation
;; of it.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define tree-root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/cli.ss")) up script-dir)))
(load (string-append script-dir "/import-walk.scm"))

(define import-graph
  (let loop ((names (source-files tree-root)) (out '()))
    (if (null? names)
        out
        (let* ((path (string-append tree-root "/" (car names)))
               (declared (declared-library-name path)))
          (loop (cdr names)
                (if (and (pair? declared) (eq? 'theourgia (car declared)) (pair? (cdr declared)))
                    (cons (cons (cadr declared)
                                (map cadr (filter (lambda (r) (pair? (cdr r)))
                                                  (imports-of-file 'theourgia path))))
                          out)
                    out))))))

(define (closure-from start)
  (let loop ((todo (list start)) (seen '()))
    (cond
      ((null? todo)
       (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
      ((memq (car todo) seen) (loop (cdr todo) seen))
      (else
       (let ((edges (cond ((assq (car todo) import-graph) => cdr) (else '()))))
         (loop (append edges (cdr todo)) (cons (car todo) seen)))))))

(want "IMPORTS the graph was read at all"
      (if (null? import-graph) 'NOTHING-WAS-READ 'read-the-tree)
      'read-the-tree)

(want "IMPORTS the client reaches neither the core nor the scheduler nor the network"
      (filter (lambda (n) (memq n '(rpc daemon sched net store log working reduce)))
              (closure-from 'client))
      '())

(want "IMPORTS and the client's closure is exactly what it should be"
      (closure-from 'client)
      '(client digest ffi render trace))

;; ⛔ AND THE PROGRAM'S OWN CLOSURE, not only the library's. A person runs
;; `theourgia.ss`; what IT reaches is a separate fact from what the
;; `client` library reaches, and the rows above are about the library.
;; `arguments` is allowed and the reason is written in `closures.ss`: the
;; program asks that table whether a verb reads standard input rather than
;; keeping a second copy of it, and the table reaches nothing else.
(want "IMPORTS and the client program's closure is exactly what it should be"
      (let ((program-imports
              (map cadr (filter (lambda (r) (pair? (cdr r)))
                                (imports-of-file 'theourgia
                                                 (string-append tree-root "/theourgia.ss"))))))
        (let loop ((todo program-imports) (seen '()))
          (cond
            ((null? todo)
             (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
            ((memq (car todo) seen) (loop (cdr todo) seen))
            (else
             (let ((edges (cond ((assq (car todo) import-graph) => cdr) (else '()))))
               (loop (append edges (cdr todo)) (cons (car todo) seen)))))))
      '(arguments client digest ffi render trace))



;; ---- CS-10 the one reader of the answer envelope --------------------------
;;
;; ⛔ THREE PROGRAMS READ THIS ENVELOPE -- the client program, the command
;; line and the MCP shell -- and each had its own copy of the field
;; lookup. `assq` raises on anything that is not a proper list of pairs, so
;; `(answer . broken)` escaped past the refusal written for it; the copies
;; were fixed one at a time and two of three were right. There is one
;; reader now, and these rows are its contract rather than any one
;; program's behaviour.
;;
;; ⚠️ THE LAST ROW IS THE ONE THAT IS EASY TO GET WRONG. Written as "scan
;; until a value satisfies the check", a peer sending the same field twice
;; chooses which of its values we read: `(answer (exit "x") (exit 0))` was
;; refused by the lookup this replaced and would be accepted by the
;; careless version of its replacement.
(want "CS-10 a field is read from an ordinary envelope"
      (answer-field '(answer (stdout "x") (exit 0)) 'stdout string?)
      "x")

(want "CS-10 a field that is not there is absent, not an error"
      (answer-field '(answer (stdout "x")) 'exit integer?)
      #f)

(want "CS-10 an envelope with no list in it is refused, not raised"
      (answer-field '(answer . broken) 'stdout string?)
      #f)

(want "CS-10 a member that is not a field is passed over"
      (answer-field '(answer junk (stdout "x")) 'stdout string?)
      "x")

(want "CS-10 a field whose value is the wrong shape is refused"
      (answer-field '(answer (exit "x")) 'exit integer?)
      #f)

(want "CS-10 the first field of a name decides, and a bad one is not skipped"
      (answer-field '(answer (exit "x") (exit 0)) 'exit integer?)
      #f)

(want "CS-10 a datum that is not an answer has no fields"
      (answer-field '(reply (stdout "x")) 'stdout string?)
      #f)

(system (string-append "kill $(cat " here "/pid) 2>/dev/null; sleep 1; rm -rf " here))
(printf "rows: ~a\n~a failures\nclient-socket complete\n" rows bad)
(exit (if (zero? bad) 0 1))
