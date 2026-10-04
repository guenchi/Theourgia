#!chezscheme
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

;; read at a causal cut, and the causal coordinates of each log event.
;;
;; KEY: A CUT IS CAUSAL, NOT A MOMENT. `log` gives each event the cut right
;; after it (`cut`) and the one right before it (`past`): its premises, each
;; with its own, and nothing a writer did without depending on it. `read <id>
;; --cut <cut>` answers the block over the state at that cut. The rows build
;; the stores whose answers differ between a causal cut and a wall clock (a
;; mirrored writer whose event is EARLIER by its timestamp and unrelated),
;; between a transitive past and a direct one (a three-writer chain, and an
;; event with no dependencies of its own), and between a replay that seeds
;; from the snapshot and one that does not.
;;
;; THE DAEMON ROUTE IS READ BY ITS TRACE: `(trace routed read store)` for a
;; cut read, `connection` for a plain one; `log-open` counts the loads; and
;; `published` counts the folds the store process made before answering.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) store-resident-cache!)
        (only (theourgia ffi) real-path)
        (only (theourgia log) log-publish! segment-sha writer-directory)
        (only (theourgia client) serve-log-path socket-path call! request-frame)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia reduce) block-id))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/read-at-cut-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'read-at-cut "scratch directory already exists" root))
(system (string-append "mkdir -p " root))
(define sock-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define (sh . xs) (system (apply string-append xs)))
(define (chmod! mode path) (sh "chmod " mode " '" path "'"))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (text-of-file p)
  (if (file-exists? p)
      (let ((t (call-with-input-file p get-string-all))) (if (eof-object? t) "" t))
      ""))
(define (lines-of-text t)
  (let loop ((cs (string->list t)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (has-substring? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (prefix? p s) (and (<= (string-length p) (string-length s)) (string=? p (substring s 0 (string-length p)))))
(define (first-datum-of p)
  (guard (e (#t 'UNREADABLE)) (let ((d (call-with-input-file p read))) (if (eof-object? d) 'NO-ANSWER d))))
(define (mentions? x needle) (has-substring? (format "~s" x) needle))

;; An answer's clause of the given head, or #f.
(define (clause-of answer head)
  (and (pair? answer) (list? answer)
       (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr answer))))
(define (incomplete-paths answer)
  (let ((c (clause-of answer 'incomplete)))
    (if (not c)
        'no-clause
        (map (lambda (u) (let ((p (and (pair? u) (assq 'path (cdr u))))) (and p (cadr p))))
             (cdr c)))))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))
;; The text of an (ok (text ...)) answer, or #f.
(define (text-of a)
  (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (car (cadr a)) 'text)
       (cadr (cadr a))))

;; ---- stores --------------------------------------------------------------------

(define (ask store . req) (rpc-dispatch store req "test"))
(define (new-id answer)
  (let ((ev (and (pair? answer) (assq 'events (cdr answer)))))
    (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define n 0)
;; -> (store local-writer)
(define (fresh-store!)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n)))
         (st (string-append d "/store")))
    (sh "mkdir -p " st " " d "/home")
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((init (ask st 'init)))
      (list st (cadr (assq 'writer (cdr init)))))))
;; One record of another writer, published as that writer's segment `seg`.
(define (mirror! st writer seq ts deps payload)
  (let ((bytes (encode-record seq ts "peer" deps (storable-encode payload))))
    (car (log-publish! st writer seq bytes (segment-sha bytes)))))
(define (section title) (list 'put (list (cons 'kind 'section) (cons 'title title))))

;; A cut as the text --cut takes.
(define (cut-text cut) (format "~s" cut))
;; The log's entries, and one entry's clause.
(define (log-entries st . id) (let ((a (apply ask st 'log id))) (if (eq? (car a) 'ok) (cdr (cadr a)) a)))
(define (entry-clause e name) (cadr (assq name (cdr e))))
(define (entry-event e) (let ((ev (assq 'event (cdr e)))) (cons (cadr ev) (caddr ev))))
(define (entry-for st event)
  (find (lambda (e) (equal? (entry-event e) event)) (log-entries st)))
(define (sorted cut) (list-sort (lambda (x y) (string<? (car x) (car y))) cut))

;; =============================================================================
(printf "== RC-1 and RC-7: a block at each of its events, before and after a snapshot ==\n")
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c))
       (id (new-id (ask st 'insert "--under" "root" "--title" "T" "--text" "text-zero")))
       (_ (ask st 'set id "src" "text-one"))
       (_ (ask st 'set id "src" "text-two"))
       (es (log-entries st id))
       (cuts (map (lambda (e) (entry-clause e 'cut)) es))
       (at (lambda (cut) (text-of (ask st 'read id "--md" "--cut" (cut-text cut)))))
       (texts (lambda () (map (lambda (cut) (let ((t (at cut))) (and t (cond ((has-substring? t "text-two") 'two)
                                                                           ((has-substring? t "text-one") 'one)
                                                                           ((has-substring? t "text-zero") 'zero)
                                                                           (else t)))))
                              cuts)))
       (before (texts))
       (later (new-id (ask st 'insert "--under" "root" "--title" "Later")))
       (cut-before-later (entry-clause (car (reverse es)) 'cut))
       (later-at (lambda () (list (head-of (ask st 'read later "--cut" (cut-text cut-before-later)))
                                 (head-of (ask st 'read later)))))
       (later-before (later-at))
       (snap (head-of (ask st 'snapshot))))
    (want "RC-1 three events, and the block's text at each: as inserted, then each set"
          (list (length es) before)
          '(3 (zero one two)))
    (want "RC-1 after a snapshot holding all three events, the same three texts"
          (list snap (texts))
          '(ok (zero one two)))
    (want "RC-7 a block created after the cut is unknown at it and read now, before and after the snapshot"
          (list later-before (later-at))
          '(((error unknown-id) ok) ((error unknown-id) ok)))
    (want "RC-2 a lone writer's coordinates: past () then each event's predecessor, cut each event"
          (map (lambda (e) (list (entry-clause e 'past) (entry-clause e 'cut))) es)
          (list (list '() (list (cons w 1)))
                (list (list (cons w 1)) (list (cons w 2)))
                (list (list (cons w 2)) (list (cons w 3))))))

;; THE RESIDENT CACHE IS NOT A SEED EITHER: warmed on a later state, a read at
;; an earlier cut still replays from the beginning.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c))
       (_ (store-resident-cache! #t))
       (id (new-id (ask st 'insert "--under" "root" "--title" "R" "--text" "resident-zero")))
       (cut1 (entry-clause (entry-for st (cons w 1)) 'cut))
       (_ (ask st 'set id "src" "resident-one"))
       (later (new-id (ask st 'insert "--under" "root" "--title" "R-later")))
       (_ (ask st 'read id))
       (_ (ask st 'read id))
       (then (text-of (ask st 'read id "--md" "--cut" (cut-text cut1))))
       (later-then (head-of (ask st 'read later "--cut" (cut-text cut1)))))
  (store-resident-cache! #f)
  (want "RC-1R with the resident cache warm on a later state, a cut read answers the earlier text and no later block"
        (list (and then (has-substring? then "resident-zero")) (and then (has-substring? then "resident-one")) later-then)
        '(#t #f (error unknown-id))))

;; =============================================================================
(printf "== RC-2: transitive and implicit pasts ==\n")
;; A.1, B.1 depending on A.1, C.1 depending on B.1 only, and B.2 with no
;; dependencies of its own. The writer names are real ids (eight base36).
(let* ((c (fresh-store!)) (st (car c))
       (A "aaaaaaaa") (B "bbbbbbbb") (C "cccccccc")
       (published (list (mirror! st A 1 1757300000001 '() (section "a1"))
                        (mirror! st B 1 1757300000002 (list (cons A 1)) (section "b1"))
                        (mirror! st C 1 1757300000003 (list (cons B 1)) (section "c1"))
                        (mirror! st B 2 1757300000004 '() (section "b2"))))
       (coords (lambda (ev) (let ((e (entry-for st ev))) (and e (list (entry-clause e 'past) (entry-clause e 'cut)))))))
  (want "RC-2 CONTROL: the four mirrored records publish"
        published '(published published published published))
  (want "RC-2 C.1's past holds A.1 through B.1: a past that joined only the direct premise omits A"
        (coords (cons C 1))
        (list (list (cons A 1) (cons B 1)) (list (cons A 1) (cons B 1) (cons C 1))))
  (want "RC-2 B.2 with no dependencies still has B.1's past: the implicit predecessor's own past is joined"
        (coords (cons B 2))
        (list (list (cons A 1) (cons B 1)) (list (cons A 1) (cons B 2))))
  (want "RC-2 A.1, first and unrelated, has the empty past"
        (coords (cons A 1))
        (list '() (list (cons A 1)))))

;; =============================================================================
(printf "== RC-5: a causal cut is not a clock ==\n")
;; W1 local, W2 mirrored: W1.1, then W2.1 (unrelated, its timestamp given),
;; then W1.2, which depends on W2.1 because it was written after it arrived.
(define M "zzzzzzzz")
(define (two-writers! ts)
  (let* ((c (fresh-store!)) (st (car c)) (w (cadr c))
         (b1 (new-id (ask st 'insert "--under" "root" "--title" "W1 block")))
         (pub (mirror! st M 1 ts '() (section "W2 block")))
         (_ (ask st 'set b1 "title" "W1 block renamed")))
    (list st w b1 (block-id M 1) pub)))
(define (rc5-readings ts)
  (let* ((s (two-writers! ts)) (st (car s)) (w (cadr s)) (b1 (caddr s)) (b2 (cadddr s))
         (cut11 (entry-clause (entry-for st (cons w 1)) 'cut))
         (cut12 (entry-clause (entry-for st (cons w 2)) 'cut)))
    (list (list-ref s 4)
          (head-of (ask st 'read b2 "--cut" (cut-text cut11)))
          (head-of (ask st 'read b2 "--cut" (cut-text cut12)))
          (head-of (ask st 'read b1 "--cut" "()"))
          (map (lambda (k) (let ((e (entry-for st (cons w k)))) (list (entry-clause e 'past) (entry-clause e 'cut))))
               '(1 2))
          (list (list '() (list (cons w 1)))
                (list (sorted (list (cons w 1) (cons M 1))) (sorted (list (cons w 2) (cons M 1))))))))
(let ((early (rc5-readings 50)))
  (want "RC-5 W2's event, earlier by its clock, is not in W1.1's cut and is in W1.2's; at () every block is unknown"
        (list-head early 4)
        '(published (error unknown-id) ok (error unknown-id)))
  (want "RC-5 W1's coordinates, exactly: W1.2's past holds W2.1"
        (list-ref early 4) (list-ref early 5))
  (want "RC-5 a skewed clock (W2.1 later than W1.1 by its timestamp) changes no answer"
        (list-head (rc5-readings 9999999999999) 4)
        (list-head early 4)))

;; =============================================================================
(printf "== RC-3: cuts that are refused, and names that resolve ==\n")
(let* ((s (two-writers! 50)) (st (car s)) (w (cadr s)) (b1 (caddr s))
       (r (lambda (text) (ask st 'read b1 "--cut" text))))
  (want "RC-3 a seq not yet received answers cut-unavailable not-received"
        (r (cut-text (list (cons w 9))))
        '(error cut-unavailable (cut cut) (reason not-received)))
  (want "RC-3 a writer named twice answers cut-unavailable duplicate-writer"
        (r (cut-text (list (cons w 1) (cons w 1))))
        '(error cut-unavailable (cut cut) (reason duplicate-writer)))
  (want "RC-3 W1.2 without W2.1, which it requires, is received but not closed: not-closed"
        (r (cut-text (list (cons w 2))))
        '(error cut-unavailable (cut cut) (reason not-closed)))
  (want "RC-3 a malformed literal that names no tag answers unknown-tag, as diff does"
        (head-of (r "((oops"))
        '(error unknown-tag))
  (ask st 'tag "settled-here")
  (want "RC-3 a settled tag name resolves to its cut"
        (head-of (r "settled-here"))
        'ok)
  ;; THE MIRROR TECHNIQUE: a local tag, then another writer's tag of the
  ;; same name that does not depend on it -- two concurrent bindings.
  (ask st 'tag "twice")
  (mirror! st M 2 1757300000099 '() (list 'tag "twice" (list (cons w 1))))
  (want "RC-3 an unsettled tag is refused tag-unsettled"
        (head-of (r "twice"))
        '(error tag-unsettled)))

;; =============================================================================
(printf "== RC-4: what a cut read combines with ==\n")
;; The document's own stored bytes are front, heading-src and src: all three
;; are set before the cut and again after it, so --md at the cut has to give
;; each one's earlier value. The answers read live at the cut are kept and the
;; cut reads compared with them whole.
(let* ((c (fresh-store!)) (st (car c)) (w (cadr c))
       (id (new-id (ask st 'insert "--under" "root" "--title" "Doc" "--text" "body-zero")))
       (_ (ask st 'set id "heading-src" "# Heading-zero\n"))
       (_ (ask st 'set id "front" "front-zero\n"))
       (child (new-id (ask st 'insert "--under" id "--title" "Child" "--text" "child-zero")))
       (cut4 (entry-clause (entry-for st (cons w 4)) 'cut))
       (live-recursive (ask st 'read id "--recursive"))
       (live-plain (ask st 'read id))
       (_ (ask st 'set id "src" "body-one"))
       (_ (ask st 'set child "src" "child-one"))
       (_ (ask st 'set id "heading-src" "# Heading-one\n"))
       (_ (ask st 'set id "front" "front-one\n"))
       (cut (cut-text cut4)))
  (want "RC-4 --working, --working-info, --writer and --signature with --cut are refused incompatible-cut-options"
        (list (ask st 'read id "--cut" cut "--working")
              (ask st 'read id "--cut" cut "--working-info")
              (ask st 'read id "--cut" cut "--writer" w)
              (ask st 'read id "--cut" cut "--signature"))
        (make-list 4 '(error bad-request incompatible-cut-options)))
  (want "RC-4 --md at a cut answers the stored text of that time"
        (let ((t (text-of (ask st 'read id "--md" "--cut" cut))))
          (list (has-substring? t "body-zero") (has-substring? t "body-one")))
        '(#t #f))
  (want "RC-4 --recursive --md at a cut answers the subtree as of the cut"
        (let ((t (text-of (ask st 'read id "--recursive" "--md" "--cut" cut))))
          (list (has-substring? t "child-zero") (has-substring? t "child-one") (has-substring? t "body-one")))
        '(#t #f #f))
  (want "RC-4 --md at a cut gives that time's front and heading bytes, and now gives the later ones"
        (let ((then (text-of (ask st 'read id "--md" "--cut" cut))) (now (text-of (ask st 'read id "--md"))))
          (list (has-substring? then "front-zero") (has-substring? then "Heading-zero")
                (has-substring? then "front-one") (has-substring? then "Heading-one")
                (has-substring? now "front-one") (has-substring? now "Heading-one")))
        '(#t #t #f #f #t #t))
  (want "RC-4 --recursive without --md at a cut answers exactly what it answered live then: the records and their versions"
        (list (equal? (ask st 'read id "--recursive" "--cut" cut) live-recursive)
              (equal? (ask st 'read id "--recursive") live-recursive)
              (and (clause-of live-recursive 'versions) #t))
        '(#t #f #t))
  (want "RC-4 a plain read at a cut answers exactly what it answered live then, its version included"
        (list (equal? (ask st 'read id "--cut" cut) live-plain)
              (equal? (ask st 'read id) live-plain)
              (and (clause-of live-plain 'version) #t))
        '(#t #f #t)))

;; =============================================================================
(printf "== RC-D2: an unreadable writer outside the cut is still said ==\n")
(let* ((s (two-writers! 50)) (st (car s)) (w (cadr s)) (b1 (caddr s))
       (cut11 (entry-clause (entry-for st (cons w 1)) 'cut))
       (dir (writer-directory st M)))
  (chmod! "000" dir)
  (let ((a (caught (ask st 'read b1 "--cut" (cut-text cut11)))))
    (chmod! "700" dir)
    (want "RC-D2 a cut that excludes the mirror still carries the incomplete clause naming it (the limit, stated)"
          (list (head-of a) (incomplete-paths a))
          (list 'ok (list dir)))))

;; =============================================================================
(printf "== RC-6b, D1, D4: the daemon route ==\n")
(define run-root (string-append sock-base "/rac-" (number->string (get-process-id))))
(sh "mkdir -p " run-root)
(putenv "THEOURGIA_RUN" (string-append run-root "/run"))
(define (lines-of-command cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (lines-of-text (if (eof-object? t) "" t))))
(define (daemon-pids store)
  (let ((needle (string-append "theourgiad.sc serve " store)))
    (map (lambda (l) (string->number (car (filter (lambda (w) (> (string-length w) 0))
                                                   (let split ((cs (string->list l)) (cur '()) (acc '()))
                                                     (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
                                                           ((char=? (car cs) #\space) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                           (else (split (cdr cs) (cons (car cs) cur) acc))))))))
         (filter (lambda (l) (has-substring? l needle)) (lines-of-command "ps -ax -o pid= -o command=")))))
(define d-stores '())
(define (stop-daemon! store)
  (for-each (lambda (pid) (sh "kill -TERM " (number->string pid) " 2>/dev/null")) (daemon-pids store))
  (let loop ((k 0))
    (when (and (< k 50) (pair? (daemon-pids store))) (sh "sleep 0.1") (loop (+ k 1))))
  (for-each (lambda (pid) (sh "kill -KILL " (number->string pid) " 2>/dev/null")) (daemon-pids store)))
(let ((previous (base-exception-handler)))
  (base-exception-handler
    (lambda (e) (for-each stop-daemon! d-stores) (previous e))))
(define e-n 0)
;; A thin-client command, --wire; -> (rc datum).
(define (client env . args)
  (set! e-n (+ e-n 1))
  (let ((out (string-append root "/c" (number->string e-n) ".out")))
    (let ((rc (sh env " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc "
                  (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                  "--wire > " out " 2> " out ".err < /dev/null")))
      (list rc (first-datum-of out)))))
(define (serve-log st) (text-of-file (serve-log-path st)))
(define (trace-lines-with st op)
  (filter (lambda (l) (prefix? (string-append "(trace " op) l)) (lines-of-text (serve-log st))))
(define (count-lines st op) (length (trace-lines-with st op)))
(define traced "THEOURGIA_INJECT=on THEOURGIA_TRACE=1")
(define local "THEOURGIA_LOCAL=1")
(define (daemon-store!)
  (let ((s (two-writers! 50)))
    (set! d-stores (cons (car s) d-stores))
    s))

;; RC-6b: the two routes answer a cut read alike; the daemon's goes to the
;; store process and loads once.
(let* ((s (daemon-store!)) (st (car s)) (w (cadr s)) (b1 (caddr s))
       (cut12 (cut-text (entry-clause (entry-for st (cons w 2)) 'cut)))
       (_ (ask st 'tag "rc6b"))
       (_ (client traced "outline" "--store" st))
       (opens0 (count-lines st "log-open "))
       (d-literal (client traced "read" b1 "--cut" cut12 "--store" st))
       (opens1 (count-lines st "log-open "))
       (d-tag (client traced "read" b1 "--cut" "rc6b" "--store" st))
       (opens2 (count-lines st "log-open "))
       (d-plain (client traced "read" b1 "--store" st))
       (l-literal (client local "read" b1 "--cut" cut12 "--store" st))
       (l-tag (client local "read" b1 "--cut" "rc6b" "--store" st))
       (routes (map (lambda (l) l) (trace-lines-with st "routed read "))))
  (stop-daemon! st)
  (want "RC-6b the daemon and the command line answer a cut read alike, for a literal cut and for a settled tag"
        (list (car d-literal) (head-of (cadr d-literal)) (equal? (cadr d-literal) (cadr l-literal))
              (car d-tag) (equal? (cadr d-tag) (cadr l-tag)))
        '(0 ok #t 0 #t))
  (want "RC-6b each daemon cut read opens the log once: one restricted replay, no second fold"
        (list (- opens1 opens0) (- opens2 opens1))
        '(1 1))
  (want "RC-6b the cut reads are routed to the store process, the plain read answered at the connection"
        routes
        '("(trace routed read store)" "(trace routed read store)" "(trace routed read connection)")))

;; D1: a cut naming an event another process appended since the daemon's
;; last fold is answered: the store process folds and publishes first.
(let* ((s (daemon-store!)) (st (car s)) (w (cadr s)) (b1 (caddr s))
       (_ (client traced "outline" "--store" st))
       (published0 (count-lines st "published "))
       (late (new-id (ask st 'insert "--under" "root" "--title" "Late")))
       (late-cut (cut-text (entry-clause (entry-for st (cons w 3)) 'cut)))
       (a (client traced "read" late "--cut" late-cut "--store" st))
       (published1 (count-lines st "published ")))
  (stop-daemon! st)
  (want "RC-D1 an event appended by another process since the last fold, read at a cut naming it: the read, after exactly one more fold"
        (list (car a) (head-of (cadr a)) (mentions? (cadr a) "Late") (- published1 published0))
        '(0 ok #t 1)))

;; D1, the limit: a fold that fails keeps the previous publication, and the
;; cut is judged against it; the failure is traced as `(reload)`'s is.
(let* ((s (daemon-store!)) (st (car s)) (w (cadr s))
       (env (string-append traced " THEOURGIA_FAULT=reload-raise@conn"))
       (_ (client env "outline" "--store" st))
       (late (new-id (ask st 'insert "--under" "root" "--title" "Late")))
       (late-cut (cut-text (entry-clause (entry-for st (cons w 3)) 'cut)))
       (a (client env "read" late "--cut" late-cut "--store" st)))
  (stop-daemon! st)
  (want "RC-D1 when that fold fails: cut-unavailable not-received against the previous publication, and reload-failed traced"
        (list (cadr a) (exists (lambda (l) (has-substring? l "injected reload raise")) (trace-lines-with st "reload-failed")))
        '((error cut-unavailable (cut cut) (reason not-received)) #t)))

;; D4: a writer in the request's envelope is not the --writer option.
(let* ((s (daemon-store!)) (st (car s)) (w (cadr s)) (b1 (caddr s))
       (cut12 (cut-text (entry-clause (entry-for st (cons w 2)) 'cut)))
       (bare (client traced "read" b1 "--cut" cut12 "--store" st))
       (with (client (string-append traced " THEOURGIA_WRITER=" w) "read" b1 "--cut" cut12 "--store" st))
       (refused (map (lambda (opts)
                       (cadr (apply client (string-append traced " THEOURGIA_WRITER=" w)
                                    (append (list "read" b1 "--cut" cut12) opts (list "--store" st)))))
                     (list '("--working") '("--working-info") (list "--writer" w) '("--signature"))))
       (routes (trace-lines-with st "routed read ")))
  (stop-daemon! st)
  (want "RC-D4 an envelope writer answers a cut read as without one; the four incompatible options are refused at the store process, none in a writer process"
        (list (equal? (cadr bare) (cadr with)) (head-of (cadr bare)) refused routes)
        (list #t 'ok (make-list 4 '(error bad-request incompatible-cut-options))
              (make-list 6 "(trace routed read store)"))))

;; R5's early paths: a request refused before it is handed on is classified
;; `connection` -- arguments that do not parse, another store's request, a
;; frame that is not a datum (its verb unknown). The draining path is the
;; fourth and has no row here.
(let* ((s (daemon-store!)) (st (car s)) (b1 (caddr s))
       (_ (client traced "outline" "--store" st))
       ;; THE ANSWER ENVELOPE, (answer (stdout "...") (stderr ...) (exit ...)
       ;; (origin ...)), read from the bytes; the refusal is the datum its
       ;; stdout holds.
       (payload (lambda (r)
                  (guard (e (#t 'UNREADABLE))
                    (let* ((env (read (open-string-input-port (utf8->string (cadr r)))))
                           (out (assq 'stdout (cdr env))))
                      (read (open-string-input-port (cadr out)))))))
       (shape (call! (socket-path st) (request-frame st 'read (list b1 "--cut") '()) 5000))
       (other (call! (socket-path st) (request-frame root 'read (list b1) '()) 5000))
       (garbage (call! (socket-path st) (string->utf8 "(((\n") 5000))
       (routes (filter (lambda (l) (not (prefix? "(trace routed outline" l))) (trace-lines-with st "routed "))))
  (stop-daemon! st)
  (want "RC-R5 refused before being handed on, each answered and traced connection: arguments that do not parse, another store, no datum"
        (list (payload shape) (payload other) (payload garbage) routes)
        (list '(error bad-request missing-option-value "--cut")
              ;; `serving` is the store as the daemon was started with it,
              ;; verbatim; `asked` is the request frame's store, which travels
              ;; by its resolved spelling (a root such as /tmp/ or one under a
              ;; symlinked directory is not that spelling).
              (list 'error 'transport-store-mismatch (list 'serving st) (list 'asked (real-path root)))
              '(error bad-request (reason not-a-datum))
              '("(trace routed read connection)" "(trace routed read connection)" "(trace routed unknown connection)"))))

(for-each stop-daemon! d-stores)
;; =============================================================================
(printf "== RC-8: a cut that moves between its judgement and its replay ==\n")
;; NEVER: A READ DOES NOT ANSWER OK FOR A CUT IT DID NOT SERVE. A read at
;; a cut is judged on one state and replayed from a second opening of the
;; log. Held at read-cut-before-replay (the injection build's hold seam),
;; a fork marker is written for the mirrored writer in the window: its
;; records from the fork on are set aside, the replay stops short, and
;; the read is refused with the cut asked and the cut served. The twin
;; is held the same way with no marker and is served the cut it asked.
;; ROUTE is local, the command line reading the store itself, or daemon,
;; where the cut is judged on the daemon's publication, handed to the
;; store process as the supplied state, and the hold is in that process.
(define (held-cut-read! fork? route)
  (let* ((c (fresh-store!)) (st (car c))
         (Q "qqqqqqqq")
         (published (list (mirror! st Q 1 1757300000001 '() (section "q1"))
                          (mirror! st Q 2 1757300000002 '() (section "q2"))
                          (mirror! st Q 3 1757300000003 '() (section "q3"))))
         (cut (list (cons Q 3)))
         (release (string-append root "/hold-" (if fork? "moved" "still") "-" (symbol->string route)))
         (out (string-append release ".out")))
    (when (eq? route 'daemon) (set! d-stores (cons st d-stores)))
    (sh "( THEOURGIA_INJECT=on " (if (eq? route 'local) "THEOURGIA_LOCAL=1 " "")
        "THEOURGIA_HOLD=read-cut-before-replay:" release
        " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc read " (quoted (block-id Q 1))
        " --cut " (quoted (cut-text cut)) " --store " (quoted st) " --wire > " out " 2> " out ".err"
        " < /dev/null; echo $? > " out ".rc ) &")
    (let ((held (let wait ((k 0))
                  (cond ((file-exists? (string-append release ".held")) 'held)
                        ((> k 300) 'never-held)
                        (else (sh "sleep 0.1") (wait (+ k 1)))))))
      (when (and fork? (eq? held 'held))
        (call-with-output-file (string-append (writer-directory st Q) "/quarantine.sexp")
          (lambda (p) (put-string p "((format 1) (fork 2) (ours \"o\") (theirs \"t\"))\n"))
          'truncate))
      (sh "touch " release)
      (let wait ((k 0))
        (unless (or (file-exists? (string-append out ".rc")) (> k 700))
          (sh "sleep 0.1") (wait (+ k 1))))
      (let ((answer (first-datum-of out)))
        (when (eq? route 'daemon) (stop-daemon! st))
        (list published held answer)))))
(define rc8-Q "qqqqqqqq")
(for-each
  (lambda (route)
    (let ((r (held-cut-read! #t route)))
      (want (format "RC-8 ~a: a record set aside between the judgement and the replay: refused cut-moved, the cut asked and the cut served" route)
            (let ((a (caddr r)))
              (list (car r) (cadr r)
                    (and (pair? a) (list? a) (pair? (cdr a))
                         (list (car a) (cadr a) (assq 'asked (cddr a))
                               (let ((s (assq 'served (cddr a)))) (and s (list? (cadr s)) (assoc rc8-Q (cadr s))))))))
            (list '(published published published) 'held
                  (list 'error 'cut-moved (list 'asked (list (cons rc8-Q 3))) (cons rc8-Q 1)))))
    (let ((r (held-cut-read! #f route)))
      (want (format "RC-8 ~a TWIN: held the same way with nothing set aside, the read is served the cut it asked" route)
            (let ((a (caddr r)))
              (list (car r) (cadr r)
                    (and (pair? a) (eq? (car a) 'ok)
                         (let ((c (clause-of a 'cut))) (and c (list? (cadr c)) (assoc rc8-Q (cadr c)))))))
            (list '(published published published) 'held (cons rc8-Q 3)))))
  '(local daemon))

;; NEVER: A WRITER ASKED AT 0 IS SERVED AT 0. The replay delivers none of
;; its records, and the cut it reached names no entry for it; that is the
;; cut asked, not one that moved. Read on a store B has written to, then
;; again after B writes once more: both are served, the same block.
(let* ((c (fresh-store!)) (st (car c))
       (A "aaaaaaaa") (B "bbbbbbbb")
       (published (list (mirror! st A 1 1757300000001 '() (section "a1"))
                        (mirror! st B 1 1757300000002 '() (section "b1"))))
       (cut (cut-text (list (cons A 1) (cons B 0))))
       (first (ask st 'read (block-id A 1) "--cut" cut))
       (later (mirror! st B 2 1757300000003 '() (section "b2")))
       (second (ask st 'read (block-id A 1) "--cut" cut)))
  (want "RC-9 a cut naming a writer at 0 is served on the store as it is, and still after that writer writes"
        (list published (head-of first) later (head-of second)
              (and (pair? first) (pair? second) (pair? (cdr first)) (pair? (cdr second))
                   (equal? (cadr first) (cadr second))))
        '((published published) ok published ok #t)))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(sh "chmod -R u+rwX " root " 2>/dev/null")
(printf "read-at-cut complete\n")
(exit (if (= bad 0) 0 1))
