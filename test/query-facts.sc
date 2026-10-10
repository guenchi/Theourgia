#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; The query language on a store: each fact relation against the provider
;; it comes from, the name-scoped rules over datum libraries and text code,
;; and the verb -- through the dispatcher and the command line, writing
;; nothing, loaded only when dispatched, and its relations equal to the
;; README's generated section.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce defs-index)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read state-block-ids block-hash block-id reduce-applied-cut)
        (only (theourgia render) render-wire render-human)
        (prefix (theourgia lifecycle) lc:)
        (only (theourgia json) string->json json->string json-ref*)
        (theourgia query))

(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (caught got) expected))))

(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (write-file! path text) (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/query-facts-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/s"))
(define (run . args) (rpc-dispatch store args "test"))
(define (state) (open-and-reduce store))
(define (new-id answer)
  (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define (ins! title text . under)
  (new-id (apply run 'insert "--title" title "--text" text (if (pair? under) (list "--under" (car under)) '()))))
(define (q goal) (let ((a (session-answer (make-query-session (state)) goal))) (if (eq? (car a) 'ok) (cadr a) a)))
(define (field-of-row id f) (let ((e (assq f (cdr (assq 'fields (state-read (state) id)))))) (and e (cdr e))))

;; ---- a seeded store --------------------------------------------------------------------------

(run 'init)
(define X (ins! "Xray" "xray alpha text"))
(define Y (ins! "Yankee" "yankee alpha beta" X))
(define Z (ins! "Zulu" "zulu refers to [[X]] here"))
(run 'set Z "src" (string-append "zulu refers to [[" X "]] here"))
(define D (ins! "Decision" "the ruling"))
(run 'set D "kind" "decision")
(run 'link Y "depends-on" X)
;; AN OLD BLOCK THE SEARCH LEAVES OUT: it matches alpha, and X supersedes it.
(define OLD (ins! "Old" "old alpha text"))
(run 'link X "supersedes" OLD)
;; DATUM LIBRARIES: (qa core) stored WITH a version, imported WITHOUT one by
;; (qa use); (qa other) defines the same name and is not imported.
(define src (string-append root "/lib"))
(system (string-append "mkdir -p '" src "'"))
(write-file! (string-append src "/core.sc") "(library (qa core (1 0)) (export f) (import (rnrs))\n(define (f) 1))\n")
(write-file! (string-append src "/use.sc") "(library (qa use) (export g) (import (rnrs) (qa core))\n(define (g) (f)))\n")
(write-file! (string-append src "/other.sc") "(library (qa other) (export f) (import (rnrs))\n(define (f) 2))\n")
(run 'import-code src "--datum")
;; TEXT CODE under no library: h defined in javascript, used by javascript and
;; by python (another language).
(run 'batch "((insert root #f ((kind . code) (lang . \"javascript\") (title . \"h def\") (name . h) (src . \"function h() { return 1 }\")))
              (insert root #f ((kind . code) (lang . \"javascript\") (title . \"h use\") (src . \"h()\")))
              (insert root #f ((kind . code) (lang . \"python\") (title . \"h py\") (src . \"h()\"))))")
;; TEXT CODE INSIDE A LIBRARY, in h's language: it must not get h, which has no library.
(define USE-ID (find (lambda (id) (and (eq? (field-of-row id 'kind) 'library) (equal? (field-of-row id 'name) '(qa use))))
                     (state-block-ids (state))))
(run 'batch (format "((insert ~s #f ((kind . code) (lang . \"javascript\") (title . \"inside\") (src . \"h()\"))))" USE-ID))

(define (id-where pred) (find pred (state-block-ids (state))))
(define (titled t) (id-where (lambda (id) (equal? (field-of-row id 'title) t))))
(define (lib-named n) (id-where (lambda (id) (and (eq? (field-of-row id 'kind) 'library) (equal? (field-of-row id 'name) n)))))
(define (def-record name lib)
  (let ((r (find (lambda (r) (and (eq? (car r) 'def) (equal? (cadr (assq 'library (cddr r))) lib)))
                 (hashtable-ref (defs-index (state)) name '()))))
    (and r (cadr r))))
(define CORE (lib-named '(qa core (1 0))))
(define USE (lib-named '(qa use)))
(define F-CORE (def-record "f" '(qa core (1 0))))
(define F-OTHER (def-record "f" '(qa other)))
(define G (def-record "g" '(qa use)))
(define HDEF (titled "h def"))
(define HUSE (titled "h use"))
(define HPY (titled "h py"))

(want "SETUP every seeded block is found"
      (map (lambda (x) (and (string? x) #t)) (list X Y Z D OLD CORE USE F-CORE F-OTHER G HDEF HUSE HPY))
      '(#t #t #t #t #t #t #t #t #t #t #t #t #t))

;; ---- Q6: each fact against its provider ---------------------------------------------------------

(want "Q6 kind and class: settled kind; the effective class by default (observation; a decision ruling)"
      (in-order (q (list 'kind X '?k)) (q (list 'class X '?c)) (q (list 'class D '?c)))
      '(((section)) ((observation)) ((ruling))))
(want "Q6 the kind of a block in conflict is conflict"
      (let ((s (let ((r (reduce-empty)))
                 (for-each (lambda (e) (apply reduce-apply! r e))
                           '(("a" 1 () (put ((kind . section) (title . "c"))))
                             ("b" 1 (("a" . 1)) (set "a.1" kind code))
                             ("c" 1 (("a" . 1)) (set "a.1" kind doc))))
                 r)))
        (cadr (session-answer (make-query-session s) '(kind "a.1" ?k))))
      '((conflict)))
(want "Q6 version is block-hash; title is the settled title"
      (in-order (equal? (q (list 'version X '?h)) (list (list (block-hash (state) X))))
                (q (list 'title X '?t)))
      '(#t (("Xray"))))
(want "Q6 under: a block at the top answers root; a child its parent"
      (in-order (q (list 'under X '?p)) (q (list 'under Y '?p)))
      (list '(("root")) (list (list X))))
(want "Q6 edge between two live blocks; ref from a text reference [[id]] in a live block"
      (in-order (q (list 'edge Y '?r '?to)) (q (list 'ref Z '?to)))
      (list (list (list 'depends-on X)) (list (list X))))
(want "Q6 library and imports by NAME, the version dropped: (qa core) stored as (qa core (1 0)) is imported as (qa core), and the join holds"
      (in-order (q (list 'library CORE '?n))
                (q '(and (imports "(qa use)" ?m) (library ?l ?m)))
                (equal? CORE "(qa core)"))
      (list '(("(qa core)")) (list (list "(qa core)" CORE)) #f))
(want "Q6 in-library: a datum definition's library by name; none for code under no library"
      (in-order (q (list 'in-library G '?n)) (q (list 'in-library HDEF '?n)))
      '((("(qa use)")) ((none))))
(want "Q6 def: a datum definition with its library's name; text code with library none"
      (in-order (q '(def ?d "(qa core)" "f")) (q '(def ?d none "h")))
      (list (list (list F-CORE)) (list (list HDEF))))
(want "Q6 uses-name is name use (syntactic); lang is a code block's language"
      (in-order (q (list 'uses-name G "f")) (q (list 'lang HDEF '?l)) (q (list 'lang HPY '?l)))
      '((()) (("javascript")) (("python"))))
(want "Q6 the lifecycle facts are the provider's: validity, decision-state, validity-reason, verified-by"
      (let ((L (lc:lifecycle (state))))
        (in-order (equal? (q (list 'validity X '?v)) (list (list (car (lc:validity-of L X)))))
                  (equal? (q (list 'decision-state D '?s)) (list (list (car (lc:decision-state-of L D)))))
                  (q '(validity-reason ?id ?why ?by))
                  (q (list 'verified-by D '?v))))
      (list #t #t (list (list OLD 'superseded-by X)) '()))
(want "Q6 moved for each end, on a reduction: the target edited, then the source"
      (let ((s (lambda more (let ((r (reduce-empty)))
                              (for-each (lambda (e) (apply reduce-apply! r e))
                                        (append '(("a" 1 () (put ((kind . section) (title . "x"))))
                                                  ("a" 2 () (put ((kind . decision) (title . "d"))))
                                                  ("a" 3 () (link "a.1" implements "a.2")))
                                                more))
                              r))))
        (in-order (cadr (session-answer (make-query-session (s '("a" 4 () (set "a.2" title "d2")))) '(moved ?a ?r ?b ?end)))
                  (cadr (session-answer (make-query-session (s '("a" 4 () (set "a.1" title "x2")))) '(moved ?a ?r ?b ?end)))))
      '((("a.1" implements "a.2" target)) (("a.1" implements "a.2" source))))
(want "Q6 score is exactly the search verb's hits with --all, row for row (the superseded block left out by both); no row for a block only one of two tokens matches"
      (let* ((hits (cdr (cadr (run 'search "alpha" "--all"))))
             (from-search (list-sort (lambda (a b) (string<? (car a) (car b))) (map (lambda (h) (list (cadr h) (caddr h))) hits)))
             (from-score (list-sort (lambda (a b) (string<? (car a) (car b)))
                                    (map (lambda (r) (list (car r) (cadr r))) (q '(score ?id "alpha" ?n))))))
        (in-order (equal? from-search from-score) (length from-score)
                  (map car (q '(score ?id "alpha beta" ?n)))))
      (list #t 2 (list Y)))

;; ---- Q7: the name-scoped rules --------------------------------------------------------------------

(want "Q7 def-for: the definition in the imported library, not the one in a library not imported, not the block itself"
      (in-order (q (list 'def-for G "f" '?d)) (q (list 'def-for F-CORE "f" '?d)))
      (list (list (list F-CORE)) '()))
(want "Q7 def-for of text code with no library: within one language only"
      (in-order (q (list 'def-for HUSE "h" '?d)) (q (list 'def-for HPY "h" '?d)))
      (list (list (list HDEF)) '()))
(want "Q7 a block inside a library does not get a definition that has no library, even in its language"
      (in-order (q (list 'in-library (titled "inside") '?n)) (q (list 'def-for (titled "inside") "h" '?d)))
      '((("(qa use)")) ()))

;; DEF-FOR'S EXCLUSIONS, on a second store: (qb use) imports four libraries.
;; (qb core) defines f and k, and k calls itself; (qb alt) defines f again,
;; so g's f is ambiguous; (qb sup) defines s and (qb inf) defines i, and each
;; definition is asked before and after it is superseded or made an inference.
(define store2 (string-append root "/s2"))
(define (run2 . args) (rpc-dispatch store2 args "test"))
(define (state2) (open-and-reduce store2))
(define (q2 goal) (let ((a (session-answer (make-query-session (state2)) goal))) (if (eq? (car a) 'ok) (cadr a) a)))
(define (def-record2 name lib)
  (let ((r (find (lambda (r) (and (eq? (car r) 'def) (equal? (cadr (assq 'library (cddr r))) lib)))
                 (hashtable-ref (defs-index (state2)) name '()))))
    (and r (cadr r))))
(define src2 (string-append root "/lib2"))
(system (string-append "mkdir -p '" src2 "'"))
(write-file! (string-append src2 "/core.sc") "(library (qb core) (export f k) (import (rnrs))\n(define (f) 1)\n(define (k) (k)))\n")
(write-file! (string-append src2 "/alt.sc") "(library (qb alt) (export f) (import (rnrs))\n(define (f) 2))\n")
(write-file! (string-append src2 "/sup.sc") "(library (qb sup) (export s) (import (rnrs))\n(define (s) 3))\n")
(write-file! (string-append src2 "/inf.sc") "(library (qb inf) (export i) (import (rnrs))\n(define (i) 4))\n")
(write-file! (string-append src2 "/use.sc")
             "(library (qb use) (export g) (import (rnrs) (qb core) (qb alt) (qb sup) (qb inf))\n(define (g) (f) (s) (i)))\n")
(run2 'init)
(run2 'import-code src2 "--datum")
(define G2 (def-record2 "g" '(qb use)))
(define K2 (def-record2 "k" '(qb core)))
(define S2 (def-record2 "s" '(qb sup)))
(define I2 (def-record2 "i" '(qb inf)))
(define F-CORE-B (def-record2 "f" '(qb core)))
(define F-ALT-B (def-record2 "f" '(qb alt)))
(want "SETUP the second store's definitions are found"
      (map string? (list G2 K2 S2 I2 F-CORE-B F-ALT-B))
      '(#t #t #t #t #t #t))
(want "Q7 def-for: not the block itself, though it uses the name it defines"
      (in-order (q2 (list 'uses-name K2 "k")) (q2 (list 'def-for K2 "k" '?d)))
      '((()) ()))
(want "Q7 ambiguous: a name two imported libraries define is ambiguous for its user, with both definitions found"
      (in-order (length (q2 (list 'def-for G2 "f" '?d))) (q2 (list 'ambiguous G2 '?n)))
      '(2 (("f"))))
(define s-before (q2 (list 'def-for G2 "s" '?d)))
(define i-before (q2 (list 'def-for G2 "i" '?d)))
(define NEWER (new-id (run2 'insert "--title" "newer" "--text" "replaces s")))
(run2 'link NEWER "supersedes" S2)
(run2 'set I2 "class" "inference")
(want "Q7 def-for: not a superseded definition, not a non-authoritative one (each found before)"
      (in-order s-before (q2 (list 'def-for G2 "s" '?d)) i-before (q2 (list 'def-for G2 "i" '?d)))
      (list (list (list S2)) '() (list (list I2)) '()))

;; DEF-FOR'S EXCLUSIONS IN THE NO-LIBRARY RULE, on the first store: text code
;; with no library, in javascript. h2 calls itself; k and m are defined apart
;; and called together, and each definition is asked before and after it is
;; superseded or made an inference.
(run 'batch "((insert root #f ((kind . code) (lang . \"javascript\") (title . \"h2 self\") (name . h2) (src . \"function h2() { return h2() }\")))
              (insert root #f ((kind . code) (lang . \"javascript\") (title . \"k def\") (name . k) (src . \"function k() { return 1 }\")))
              (insert root #f ((kind . code) (lang . \"javascript\") (title . \"m def\") (name . m) (src . \"function m() { return 2 }\")))
              (insert root #f ((kind . code) (lang . \"javascript\") (title . \"k m use\") (src . \"k(); m()\"))))")
(define H2SELF (titled "h2 self"))
(define KDEF (titled "k def"))
(define MDEF (titled "m def"))
(define KMUSE (titled "k m use"))
(want "Q7 def-for with no library: not the block itself, though it uses the name it defines"
      (in-order (q (list 'uses-name H2SELF "h2")) (q (list 'def-for H2SELF "h2" '?d)))
      '((()) ()))
(define k-before (q (list 'def-for KMUSE "k" '?d)))
(define m-before (q (list 'def-for KMUSE "m" '?d)))
(define NEWER-K (ins! "newer k" "replaces k"))
(run 'link NEWER-K "supersedes" KDEF)
(run 'set MDEF "class" "inference")
(want "Q7 def-for with no library: not a superseded definition, not a non-authoritative one (each found before)"
      (in-order k-before (q (list 'def-for KMUSE "k" '?d)) m-before (q (list 'def-for KMUSE "m" '?d)))
      (list (list (list KDEF)) '() (list (list MDEF)) '()))

;; ---- Q3: the same answers from a snapshot-seeded state --------------------------------------------------

(define before-snapshot (q '(depends* ?a ?b)))
(want "Q3 the same answers from a state seeded by a snapshot"
      (in-order (car (run 'snapshot)) (equal? before-snapshot (q '(depends* ?a ?b))) (length before-snapshot))
      '(ok #t 1))

;; ---- Q9: the verb ---------------------------------------------------------------------------------------

(define goal-text "(depends* ?a ?b)")
(define in-process (run 'query goal-text))
(define (cli-wire . args)
  (let* ((out (string-append root "/cli.out"))
         (rc (system (string-append "scheme --script ../core.sc " (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                    "--store " (quoted store) " --wire > " (quoted out) " 2>/dev/null < /dev/null"))))
    (if (= rc 0) (file-text out) (list 'FAILED rc (file-text out)))))
(define cut-before (reduce-applied-cut (state)))
(want "Q9 the verb answers ok with rows, vars, digest and cut"
      (in-order (car in-process) (map car (cddr in-process)))
      '(ok (vars digest cut)))
(want "Q9 the dispatcher and the command line answer the same bytes"
      (equal? (render-wire in-process) (cli-wire "query" goal-text))
      #t)

;; THE THREE ROUTES: the dispatcher in this process; the command line as a
;; client of a daemon serving the store, so the answer crosses the socket;
;; and the MCP shell, the answer read out of the tool result's text. The
;; printed answer is the same bytes on all three.
(define socket-dir (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket (string-append socket-dir "/qf-" (number->string (get-process-id)) ".sock"))
(define daemon-log (string-append root "/daemon.log"))
(define (sleep-ms n) (sleep (make-time 'time-duration (* n 1000000) 0)))
(define (start-daemon!)
  (system (string-append "THEOURGIA_TRACE=1 scheme --script ../theourgiad.sc serve " (quoted store) " --socket " (quoted socket)
                         " > " (quoted daemon-log) " 2>&1 < /dev/null &"))
  (let wait ((k 0))
    (cond ((file-exists? socket) 'up)
          ((> k 200) 'never-came-up)
          (else (sleep-ms 50) (wait (+ k 1))))))
;; EVERY DAEMON SERVING THIS STORE, the fixture's own and the one the MCP
;; shell starts for it: nothing this file starts outlives it. The pattern's
;; [c] keeps it from matching the shell that runs pgrep or pkill, whose own
;; command line holds the pattern.
(define (store-daemons)
  (let ((out (string-append root "/daemons.txt")))
    (system (string-append "pgrep -f " (quoted (string-append "theourgiad.s[c] serve " store " ")) " > " (quoted out) " 2>/dev/null"))
    (length (filter (lambda (l) (> (string-length l) 0))
                    (let loop ((p (open-input-string (file-text out))) (acc '()))
                      (let ((l (get-line p))) (if (eof-object? l) acc (loop p (cons l acc)))))))))
(define (stop-daemon!)
  (system (string-append "pkill -f " (quoted (string-append "theourgiad.s[c] serve " store " ")) " 2>/dev/null"))
  (let wait ((k 0)) (when (and (> (store-daemons) 0) (< k 100)) (sleep-ms 50) (wait (+ k 1)))))
(define (through-socket sock . args)
  (let* ((out (string-append root "/daemon-cli.out"))
         (rc (system (string-append "scheme --script ../core.sc " (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                    "--store " (quoted store) " --socket " (quoted sock) " --wire > " (quoted out) " 2>/dev/null < /dev/null"))))
    (if (= rc 0) (file-text out) (list 'FAILED rc))))
(define (through-daemon . args) (apply through-socket socket args))
(define (through-shell goal)
  (let* ((in (string-append root "/shell.in")) (out (string-append root "/shell.out")))
    (write-file! in (string-append
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}\n"
                      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                      "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_query\",\"arguments\":{\"argv\":"
                      (json->string (vector goal)) "}}}\n"))
    (system (string-append "THEOURGIA_LOCAL=1 scheme --script ../mcp/server.sc --store " (quoted store)
                           " < " (quoted in) " > " (quoted out) " 2>/dev/null"))
    (let* ((lines (let loop ((ls '()) (p (open-input-string (file-text out))))
                    (let ((l (get-line p))) (if (eof-object? l) (reverse ls) (loop (cons l ls) p)))))
           (reply (find (lambda (l) (let ((j (guard (e (#t #f)) (string->json l)))) (and j (equal? (json-ref* j "id" #f) 3)))) lines)))
      (if (not reply)
          (list 'NO-REPLY lines)
          (let* ((content (json-ref* (json-ref* (string->json reply) "result" #f) "content" #f)))
            (if (and (vector? content) (> (vector-length content) 0))
                (json-ref* (vector-ref content 0) "text" 'no-text)
                (list 'NO-CONTENT reply)))))))
;; THE DAEMON SAYS IT SERVED THE REQUEST: traced, it writes one line
;; `(trace daemon-dispatch <verb> #f)` per request it dispatches, and an
;; answer a client made in its own process writes none. That line, absent
;; before the call and present once after, is what shows the answer below
;; crossed the socket.
(define (served-queries) 
  (let ((t (file-text daemon-log)) (needle "(trace daemon-dispatch query"))
    (let loop ((i 0) (n 0))
      (cond ((> (+ i (string-length needle)) (string-length t)) n)
            ((string=? (substring t i (+ i (string-length needle))) needle) (loop (+ i 1) (+ n 1)))
            (else (loop (+ i 1) n))))))
(define daemon-state (start-daemon!))
(define served-before (served-queries))
(define via-daemon (through-daemon "query" goal-text))
(define served-after (served-queries))
(stop-daemon!)
(want "Q9 through the daemon: the command line as its client answers the same bytes as the dispatcher"
      (in-order daemon-state (equal? (render-wire in-process) via-daemon))
      '(up #t))
(want "Q9 CONTROL: the daemon served that request -- its trace has no query dispatch before the call and one after"
      (list served-before served-after)
      '(0 1))
(want "Q9 through the MCP shell: the tool result's text is the same bytes"
      (let ((t (through-shell goal-text)) (w (render-wire in-process)))
        (or (equal? t w) (list 'DIFFERS t w)))
      #t)
(stop-daemon!)
(want "Q9 no daemon of this store outlives the routes (the shell starts one)"
      (store-daemons)
      0)
(want "Q9 nothing is written: the applied cut is the same after the queries"
      (equal? cut-before (reduce-applied-cut (state)))
      #t)
(want "Q9 the human output: one row a line, values separated by a space"
      (render-human (run 'query (string-append "(edge \"" Y "\" ?r ?x)")))
      (string-append "depends-on \"" X "\"\n"))
(want "Q9 --relations lists the two tables, and README carries the section generated from them"
      (let ((items (cdr (cadr (run 'query "--relations")))))
        (in-order (length (filter (lambda (i) (eq? (car i) 'fact)) items))
                  (length (filter (lambda (i) (eq? (car i) 'rule)) items))
                  (string-contains? (file-text "../README.md") (query-relations-text))))
      '(24 19 #t))
(define child (string-append root "/ondemand.sc"))
(write-file! child
  (string-append
    "(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs))\n"
    "(register-verbs! extension-verbs)\n"
    "(define (loaded?) (and (member '(theourgia query) (library-list)) #t))\n"
    (format "(define store ~s)\n" store)
    "(rpc-dispatch store '(search \"alpha\") \"test\")\n"
    "(define before (loaded?))\n"
    "(rpc-dispatch store '(query \"(kind ?x section)\") \"test\")\n"
    "(write (list before (loaded?)))\n"))
(want "Q9 the library is absent until query is dispatched, and present after"
      (let ((out (string-append root "/ondemand.out")))
        (system (string-append "scheme --script " (quoted child) " > " (quoted out) " 2>/dev/null < /dev/null"))
        (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-string-input-port (file-text out)))))
      '(#f #t))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nquery-facts complete\n" rows bad)
(exit (if (= bad 0) 0 1))
