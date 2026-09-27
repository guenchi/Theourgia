;; Copyright 2018 - 2026 The Theourgia Authors
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

;; THE MUTATION RECORD IS KEYED BY THE ACTOR (F100 D1', amendment 8). A
;; parameter cell belongs to whichever process wrote it last and does not
;; survive a yield, so the record is looked up by `self`. Three rows, each
;; under the scheduler:
;;   (a) two actors, both scopes open before either writes, writing in
;;       turn: each record holds its own entries only;
;;   (b) an actor with no scope notes nothing, and `mutation-record` outside
;;       a scope answers the empty list;
;;   (c) a scope that spawns another actor to do the work establishes the
;;       scope inside that actor, handing it the entries so far
;;       (`with-mutation-record`'s optional second argument), and reads the
;;       combined record back.
(import (chezscheme) (theourgia ffi) (theourgia sched) (theourgia answers)
        ;; F100b M2a's two helper rows (H5, H7).
        (only (theourgia daemon) write-report-line!)
        (only (theourgia client) next-attempt-token select-report)
        ;; F100b M3a's helper rows (H6).
        (only (theourgia refusal) refusal-object refusal-result-json refusal-error-json)
        (only (theourgia json) string->json json->string json-ref*))

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
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define dir (string-append scratch-base "/facade-record-" (number->string (get-process-id))))
(mkdir-p! dir)
(define (under name) (string-append dir "/" name))

;; ONE FILE, CREATED AND WRITTEN THROUGH THE DOOR: its record is exactly
;; ((create p) (write p)); fsync and close note nothing.
;; THE TREE BY CONSTRUCTION (rulings J and K, RR3): make-one! reads back
;; what it wrote, and ANY failure -- the open, the write, the close, the
;; read-back, or bytes that differ -- raises its one marker,
;; (make-one!-failed path reason). Nothing in this file swallows it: an
;; actor answers it through raised-as, the main actor's rows through `want`.
;; So every row of this file asserts the tree through the marker; rows (a)
;; and (c) also project the bytes. A row that needs a write without the
;; read-back says so; none does today.
(define (bytes-of path) (utf8->string (entry-bytes path)))
(define (make-one! path)
  (let ((got (guard (e (#t (list 'raised (if (and (condition? e) (message-condition? e))
                                             (condition-message e)
                                             e))))
               (let ((fd (fd-open path '(write create))))
                 (write-all! fd (string->utf8 "r\n"))
                 (fd-close fd))
               (bytes-of path))))
    (unless (equal? got "r\n")
      (raise (list 'make-one!-failed path got)))))
;; AN ACTOR THAT RAISES ANSWERS WITH THE RAISE, so the row reads a wrong
;; answer rather than waiting out a timeout for a reply that never comes.
(define (raised-as e)
  (list 'raised (if (and (condition? e) (message-condition? e)) (condition-message e) e)))

;; ---- F100b H1, H2: the table and the aggregation rule (pure) -----------------
;;
;; (theourgia answers)' two helpers on fixed conditions and records, one row
;; per cell of the brief. The durable-error is ffi's vector
;; #(durable-error op (path . errno)); the unreadable-entry is ffi's own
;; constructor. Errnos: names for unreadable-entry, the numbers through ffi's
;; exported constants for durable-error (ruling D1).
(define (unreadable-at p errno-name reason) (make-unreadable-entry p reason errno-name))
(define (durable op p errno) (vector 'durable-error op (cons p errno)))
(define h-record '((mkdir "/a") (create "/a/b")))
(define no-such "No such file or directory")
(define denied "Permission denied")
(want "H1 unreadable-entry ENOENT, empty record -> absent"
      (classify-failure (unreadable-at "/p" 'ENOENT no-such) '())
      (list 'error 'absent '(path "/p") (list 'reason no-such) '(errno ENOENT)))
(want "H1 unreadable-entry ENOTDIR, empty record -> absent with ENOTDIR"
      (classify-failure (unreadable-at "/p" 'ENOTDIR "Not a directory") '())
      '(error absent (path "/p") (reason "Not a directory") (errno ENOTDIR)))
(want "H1 unreadable-entry EACCES, empty record -> unreadable"
      (classify-failure (unreadable-at "/p" 'EACCES denied) '())
      (list 'error 'unreadable '(path "/p") (list 'reason denied) '(errno EACCES)))
(want "H1 durable-error mkdir 13, empty record -> unwritable with op, the reason strerror's"
      (classify-failure (durable 'mkdir "/p" EACCES) '())
      (list 'error 'unwritable '(op mkdir) '(path "/p") (list 'reason denied) (list 'errno EACCES)))
(want "H1 durable-error dir-fsync 5, empty record -> unwritable (op dir-fsync)"
      (classify-failure (durable 'dir-fsync "/d" EIO) '())
      (list 'error 'unwritable '(op dir-fsync) '(path "/d") (list 'reason "Input/output error") (list 'errno EIO)))
(for-each
  (lambda (c)
    (let ((failure (car c)) (clauses (cdr c)))
      (want (string-append "H1 " (car clauses) " with a record -> incomplete, its clauses under failed, the record as written")
            (classify-failure failure h-record)
            (list 'error 'incomplete (cons 'failed (cdr clauses)) (list 'written h-record)))))
  (list (list (unreadable-at "/p" 'ENOENT no-such) "ENOENT" '(path "/p") (list 'reason no-such) '(errno ENOENT))
        (list (unreadable-at "/p" 'ENOTDIR "Not a directory") "ENOTDIR" '(path "/p") '(reason "Not a directory") '(errno ENOTDIR))
        (list (unreadable-at "/p" 'EACCES denied) "EACCES" '(path "/p") (list 'reason denied) '(errno EACCES))
        (list (durable 'mkdir "/p" EACCES) "durable mkdir" '(op mkdir) '(path "/p") (list 'reason denied) (list 'errno EACCES))
        (list (durable 'dir-fsync "/d" EIO) "durable dir-fsync" '(op dir-fsync) '(path "/d") '(reason "Input/output error") (list 'errno EIO))))
;; A durable-error with no errno is a short write (M2a review r1, F3):
;; the table answers it with a reason of its own instead of raising.
(want "H1 durable write with errno #f (a short write), empty record -> unwritable, reason \"short write\", errno #f"
      (classify-failure (durable 'write "/p" #f) '())
      '(error unwritable (op write) (path "/p") (reason "short write") (errno #f)))
(want "H1 durable create with errno #f (a Chez condition with no mapped errno), empty record -> unwritable, reason \"no errno\" (M2a review r2, F3)"
      (classify-failure (durable 'create "/p" #f) '())
      '(error unwritable (op create) (path "/p") (reason "no errno") (errno #f)))
(want "H1 what the table does not classify answers #f (the caller keeps its own answer)"
      (list (classify-failure (make-message-condition "x") '()) (classify-failure 'boom h-record))
      '(#f #f))
(define h-entries '((create "/l")))
;; Each input is the table's own shape for its kind: unwritable carries
;; its op, so a promotion that dropped a clause would show (F100b M1
;; review r1, F2).
(for-each
  (lambda (answer)
    (want (string-append "H2 " (symbol->string (cadr answer)) " with an empty record plus entries -> incomplete, every clause under failed")
          (combine-report answer h-entries)
          (list 'error 'incomplete (cons 'failed (cddr answer)) (list 'written h-entries))))
  (list '(error absent (path "/p") (reason "No such file or directory") (errno ENOENT))
        '(error unreadable (path "/p") (reason "r") (errno EACCES))
        (list 'error 'unwritable '(op mkdir) '(path "/p") (list 'reason denied) (list 'errno EACCES))))
(want "H2 incomplete with written W plus entries E -> written W then E"
      (combine-report '(error incomplete (failed (path "/p")) (written ((mkdir "/a")))) h-entries)
      '(error incomplete (failed (path "/p")) (written ((mkdir "/a") (create "/l")))))
(want "H2 any answer plus no entries is unchanged, and its written text identical"
      (let* ((a '(error unreadable (path "/p") (reason "r") (errno EACCES)))
             (b (combine-report a '())))
        (list (equal? a b) (string=? (format "~s" a) (format "~s" b))))
      '(#t #t))
;; A named outcome may carry atoms after its kind: store.sc answers
;; (error not-written reserved-not-written (sequence n)) (F100b M1 review
;; r1, F1: a lookup by assq raised on it).
(want "H2 a named outcome carrying an atom after its kind gains (written E) and keeps every clause"
      (list (with-written '(error not-written reserved-not-written (sequence 2)) h-entries)
            (combine-report '(error not-written reserved-not-written (sequence 2)) h-entries))
      (let ((a (list 'error 'not-written 'reserved-not-written '(sequence 2) (list 'written h-entries))))
        (list a a)))
(want "H2 a named outcome gains (written E) appended and keeps its name"
      (combine-report '(error eval-exception (kind raised) (message "m")) h-entries)
      (list 'error 'eval-exception '(kind raised) '(message "m") (list 'written h-entries)))
(want "H2 an answer already carrying written, plus no entries, is unchanged"
      (combine-report '(error working-unavailable (message "m") (written ((create "/w")))) '())
      '(error working-unavailable (message "m") (written ((create "/w")))))
(want "H2 a named outcome already carrying (written W) plus non-empty E -> ONE written clause holding W then E"
      (combine-report '(error working-unavailable (message "m") (written ((create "/w")))) h-entries)
      '(error working-unavailable (message "m") (written ((create "/w") (create "/l")))))
(want "H2 a success answer is never given a written clause"
      (combine-report '(ok (values (3))) h-entries)
      '(ok (values (3))))

;; ---- F100b M2a: the startup report's one write, and the attempt token ----
;;
;; H5 (brief item 3, E4): write-report-line! writes newline, the datum,
;; newline, as ONE write, and the caller's record holds exactly one entry
;; for it. The descriptor is opened outside the scope, so the scope holds
;; the report's write alone.
;; STATED LIMIT (M2a review r1, F6, ruled): "one call to write-once, read at
;; write-one!'s body, not measured". A write-one! that split a successful
;; write into two syscalls while keeping one note and one trace would leave
;; the file, the record and the fault as they are; no row here counts
;; syscalls (that would be a measurement of the kernel, not of the door).
(let* ((p (under "h5.log"))
       (fd (fd-open p '(write append create)))
       (record (with-mutation-record
                 (lambda ()
                   (write-report-line! fd '(error x (path "/p") (attempt "0123456789abcdef")))
                   (mutation-record)))))
  (fd-close fd)
  (want "H5 write-report-line! writes \\n + the datum + \\n exactly, and the record holds one write entry naming the file"
        (list (bytes-of p) record)
        (list "\n(error x (path \"/p\") (attempt \"0123456789abcdef\"))\n" (list (list 'write p)))))
;; H5b: under short-write@report the call raises durable-error (op write,
;; errno #f) and the file holds no complete report line -- only the seven
;; bytes the short write offered, never a second syscall's. Run as a child
;; process, since the fault is read from the environment at start.
(let* ((p (under "h5b.log"))
       (script (under "h5b.ss"))
       (out (under "h5b.out")))
  (call-with-output-file script
    (lambda (o)
      (write '(import (chezscheme) (theourgia ffi) (only (theourgia daemon) write-report-line!)) o)
      ;; Inside a record scope (M2a review r1, F8): the short write that
      ;; really ran is noted once, like a whole one.
      (write `(let ((fd (fd-open ,p '(write append create))))
                (write (with-mutation-record
                         (lambda ()
                           (list (guard (e ((fs-error? e) (list 'raised (fs-error-op e) (fs-error-errno e))))
                                   (parameterize ((theourgia-stage 'report))
                                     (write-report-line! fd '(error x (attempt "0123456789abcdef"))))
                                   'returned)
                                 (mutation-record)))))
                (fd-close fd))
             o)))
  (let ((rc (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=short-write@report scheme --script "
                                   script " > " out " 2> /dev/null < /dev/null"))))
    (want "H5b under short-write@report write-report-line! raises durable-error (op write, errno #f), notes the one write, and leaves no complete line: the seven offered bytes only"
          (list rc (guard (e (#t 'unread)) (call-with-input-file out read)) (bytes-of p))
          (list 0 (list '(raised write #f) (list (list 'write p))) "\n(error"))))
;; H5d (M2a review r1, F7): the port's buffered bytes are flushed before the
;; report, so they land before it: a child prints "pre" through the port,
;; writes a report to fd 1, prints "post", and its stdout reads in that
;; order.
(let* ((script (under "h5d.ss")) (out (under "h5d.out")))
  (call-with-output-file script
    (lambda (o)
      (write '(import (chezscheme) (only (theourgia daemon) write-report-line!)) o)
      (write '(begin (display "pre") (write-report-line! 1 '(error x (attempt #f))) (display "post")) o)))
  (let ((rc (system (string-append "scheme --script " script " > " out " 2> /dev/null < /dev/null"))))
    (want "H5d buffered port output lands before the report: stdout is pre, the framed report, post"
          (list rc (bytes-of out))
          (list 0 "pre\n(error x (attempt #f))\npost"))))
;; H7 (D11): two tokens from one process are distinct, each 16 lowercase hex.
(let* ((a (next-attempt-token)) (b (next-attempt-token))
       (hex16? (lambda (t) (and (string? t) (= 16 (string-length t))
                                (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))
                                         (string->list t))))))
  ;; The first eight characters are this process's pid (M2a review r1, F13).
  (want "H7 next-attempt-token twice in one process: two distinct strings of 16 lowercase hex, the first eight this pid"
        (list (hex16? a) (hex16? b) (string=? a b)
              (string->number (substring a 0 8) 16) (string->number (substring b 0 8) 16))
        (list #t #t #f (get-process-id) (get-process-id))))

;; ---- F100b M3a: refusal-json (item 8), H6 --------------------------------
;;
;; Each kind rendered as the MCP shell renders it, compared as PARSED JSON
;; with the members sorted by key, so the rows pin the members and their
;; values, not the order json->string happens to write them in.
(define (sorted-object x)
  (cond
    ((and (list? x) (pair? x) (for-all (lambda (m) (and (pair? m) (string? (car m)))) x))
     (list-sort (lambda (a b) (string<? (car a) (car b)))
                (map (lambda (m) (cons (car m) (sorted-object (cdr m)))) x)))
    ((vector? x) (vector-map sorted-object x))
    (else x)))
(define (rendered d . opt)
  (sorted-object (string->json (json->string (apply refusal-object d opt)))))
(define (datum-text d) (call-with-string-output-port (lambda (p) (write d p))))
(define (expect . members) (sorted-object members))
(let ((d '(error unreadable (path "/p") (reason "Permission denied") (errno EACCES))))
  (want "H6 unreadable: kind, path, reason, errno as its NAME, and the datum written; nothing else"
        (rendered d)
        (expect (cons "kind" "unreadable") (cons "path" "/p") (cons "reason" "Permission denied")
                (cons "errno" "EACCES") (cons "datum" (datum-text d)))))
(let ((d '(error unwritable (op mkdir) (path "/d") (reason "Permission denied") (errno 13))))
  (want "H6 unwritable: op, and a NUMBER errno stays a number"
        (rendered d)
        (expect (cons "kind" "unwritable") (cons "op" "mkdir") (cons "path" "/d")
                (cons "reason" "Permission denied") (cons "errno" 13) (cons "datum" (datum-text d)))))
(let ((d '(error incomplete (failed (path "/m") (reason "Permission denied") (errno EACCES))
                 (written ((create "/l") (link "/a" "/b"))))))
  (want "H6 incomplete: failed's clauses flattened to the top level, no `failed` member, written an array of arrays of strings"
        (rendered d)
        (expect (cons "kind" "incomplete") (cons "path" "/m") (cons "reason" "Permission denied")
                (cons "errno" "EACCES") (cons "written" (vector (vector "create" "/l") (vector "link" "/a" "/b")))
                (cons "datum" (datum-text d)))))
(let ((d '(error serve-start-failed (kind incomplete)
                 (failed (path "/m") (reason "Permission denied") (errno EACCES))
                 (written ((create "/k/.socket.lock"))) (attempt "0123456789abcdef") (exit 75)
                 (client-written ((mkdir "/k"))))))
  (want "H6 serve-start-failed: the kind its (kind K) clause names, the report's clauses, attempt, exit and client-written"
        (rendered d)
        (expect (cons "kind" "incomplete") (cons "path" "/m") (cons "reason" "Permission denied")
                (cons "errno" "EACCES") (cons "written" (vector (vector "create" "/k/.socket.lock")))
                (cons "attempt" "0123456789abcdef") (cons "exit" 75)
                (cons "client-written" (vector (vector "mkdir" "/k")))
                (cons "datum" (datum-text d)))))
(let ((d '(error serve-start-failed (kind exited) (signal 9) (attempt "0123456789abcdef"))))
  (want "H6 a signal status: signal, no exit"
        (rendered d)
        (expect (cons "kind" "exited") (cons "signal" 9) (cons "attempt" "0123456789abcdef")
                (cons "datum" (datum-text d)))))
(let ((d '(error store-not-found (store "/s") (attempt "0123456789abcdef"))))
  (want "H6 a clause item 8 does not name (store) is in the datum only; a short write's errno #f is null"
        (list (rendered d)
              (let ((w '(error unwritable (op write) (path "/w") (reason "short write") (errno #f))))
                (json-ref* (string->json (json->string (refusal-object w))) "errno")))
        (list (expect (cons "kind" "store-not-found") (cons "attempt" "0123456789abcdef") (cons "datum" (datum-text d)))
              'null)))
(let ((d '(error unreadable (path "/s/./store") (reason "Permission denied") (errno EACCES))))
  (want "H6 origin: a constructed transport refusal carries origin transport; the same datum without it carries none"
        (list (rendered d '() 'transport) (rendered d))
        (list (expect (cons "kind" "unreadable") (cons "path" "/s/./store") (cons "reason" "Permission denied")
                      (cons "errno" "EACCES") (cons "origin" "transport") (cons "datum" (datum-text d)))
              (expect (cons "kind" "unreadable") (cons "path" "/s/./store") (cons "reason" "Permission denied")
                      (cons "errno" "EACCES") (cons "datum" (datum-text d))))))
;; EVERY KIND A PRODUCER IN THIS TREE GIVES renders as itself, bare and
;; wrapped in serve-start-failed (F100b M3a reviews r1 F2, r2 F1).
;; THE LIST IS DERIVED FROM THE SOURCE where a kind is spelled as a literal
;; in one of five spellings: `(list 'error '<k>`, `` `(error <k> ``,
;; `'(error <k>`, `(list (quote error) (quote <k>` and `(cons 'error (cons
;; '<k>`, in the tree's .sc files and mcp/server.sc. A kind a producer adds
;; in one of these spellings is covered without an edit here; a sixth
;; spelling is not (M3b review r1, F2: the last two were found that way). The scan is a superset: a few of its
;; symbols are not refusals (mcp/server.sc's own `(list 'error 'null ...)`
;; frame, say), and they render as themselves too, which is all the row asks.
;; The kinds that are COMPUTED, not spelled, are listed with their producers:
;; answers.sc:91 absent (from an errno), :92 unwritable; client.sc:615
;; timeout and :635 exited (clauses the client builds). And one kind that
;; no producer gives: describe-refused, the P7 stub's fixture (its E12 row),
;; kept as an arbitrary kind.
(define (scanned-kinds)
  (let* ((files (append (map (lambda (n) (string-append "../" n))
                             (filter (lambda (n) (let ((k (string-length n)))
                                                   (and (> k 3) (string=? (substring n (- k 3) k) ".sc"))))
                                     (directory-list "..")))
                        (list "../mcp/server.sc")))
         (markers '("(list 'error '" "`(error " "'(error " "(list (quote error) (quote " "(cons 'error (cons '"))
         (kind-char? (lambda (c) (or (char<=? #\a c #\z) (char<=? #\0 c #\9) (char=? c #\-)))))
    (fold-left
      (lambda (acc f)
        (let ((t (call-with-input-file f get-string-all)))
          (fold-left
            (lambda (acc m)
              (let loop ((i 0) (acc acc))
                (let ((at (let scan ((j i))
                            (cond ((> (+ j (string-length m)) (string-length t)) #f)
                                  ((and (char=? (string-ref t j) (string-ref m 0))
                                        (string=? (substring t j (+ j (string-length m))) m))
                                   j)
                                  (else (scan (+ j 1)))))))
                  (if (not at)
                      acc
                      (let* ((from (+ at (string-length m)))
                             (to (let run ((k from)) (if (and (< k (string-length t)) (kind-char? (string-ref t k))) (run (+ k 1)) k)))
                             (k (substring t from to)))
                        (loop to (if (and (> (string-length k) 0) (char<=? #\a (string-ref k 0) #\z)
                                          (not (member (string->symbol k) acc)))
                                     (cons (string->symbol k) acc)
                                     acc)))))))
            acc markers)))
      '() files)))
(define producer-kinds
  (let ((scanned (scanned-kinds)))
    (append scanned
            (filter (lambda (k) (not (memq k scanned))) '(absent unwritable timeout exited describe-refused)))))
(want "H6 the kind scan finds the producers' literal kinds (a CONTROL on the scan itself: it must see these)"
      (filter (lambda (k) (not (memq k producer-kinds)))
              '(socket-dir-missing serve-busy serve-path-occupied store-not-found store-load-failed store-busy
                draining bad-request transport-unknown transport-store-mismatch unreadable incomplete
                ;; one kind from each of the two later spellings (store.sc)
                unknown-tag resolved-executed))
      '())
(want "H6 every kind a producer gives renders as its own kind, bare and wrapped in serve-start-failed"
      (filter (lambda (k)
                (not (and (equal? (cdr (assoc "kind" (refusal-object (list 'error k '(path "/p")))))
                                  (symbol->string k))
                          (equal? (cdr (assoc "kind" (refusal-object
                                                       (list 'error 'serve-start-failed (list 'kind k)
                                                             '(attempt "0123456789abcdef") '(exit 75)))))
                                  (symbol->string k)))))
              producer-kinds)
      '())
(let ((d '(error serve-start-failed (kind timeout) (pid 4242) (log "/k/serve.log"))))
  (want "H6 timeout (item 4): kind timeout; pid and log are not members of item 8, so they are in the datum only"
        (rendered d)
        (expect (cons "kind" "timeout") (cons "datum" (datum-text d)))))
(want "H6 unavailable: exactly kind and datum, nothing else (J3)"
      (rendered '(unavailable))
      (expect (cons "kind" "unavailable") (cons "datum" "(unavailable)")))
;; AGGREGATION (c): the MCP process's own entries join the answer and the
;; kind is recomputed. No production route has such entries (D22), so this
;; row is the combination's only coverage.
(let ((d '(error unreadable (path "/p") (reason "Permission denied") (errno EACCES))))
  (want "H6 aggregation (c): a non-empty record of the MCP process makes unreadable incomplete, its entries in written"
        (rendered d '((create "/x")))
        (let ((c '(error incomplete (failed (path "/p") (reason "Permission denied") (errno EACCES)) (written ((create "/x"))))))
          (expect (cons "kind" "incomplete") (cons "path" "/p") (cons "reason" "Permission denied")
                  (cons "errno" "EACCES") (cons "written" (vector (vector "create" "/x")))
                  (cons "datum" (datum-text c))))))
(let* ((d '(error unreadable (path "/p") (reason "Permission denied") (errno EACCES)))
       (obj (sorted-object (string->json (json->string (refusal-object d))))))
  (want "H6 the tools/call RESULT and the tools/list ERROR, parsed: text and data carry the datum and the object"
        (list (sorted-object (string->json (refusal-result-json d)))
              (sorted-object (string->json (refusal-error-json d))))
        (list (expect (cons "content" (vector (expect (cons "type" "text") (cons "text" (datum-text d)))))
                      (cons "isError" #t)
                      (cons "_meta" (expect (cons "refusal" obj))))
              (expect (cons "code" -32000) (cons "message" "core did not start") (cons "data" obj)))))

;; ---- F100b M2b1: select-report, waitpid-status, the hold seam ----------------
;;
;; H3 (item 3, D10, E10): select-report of bytes, a byte offset and a token.
(define T "0123456789abcdef")
(define (u8 s) (string->utf8 s))
(want "H3 offset 0, one line carrying T: that datum"
      (select-report (u8 "(error a (attempt \"0123456789abcdef\"))\n") 0 T)
      '(error a (attempt "0123456789abcdef")))
(want "H3 offset just after a newline, the second line carrying T: the second"
      (let* ((first-line "(error old (attempt \"0123456789abcdef\"))\n")
             (b (u8 (string-append first-line "(error new (attempt \"0123456789abcdef\"))\n"))))
        (select-report b (bytevector-length (u8 first-line)) T))
      '(error new (attempt "0123456789abcdef")))
(want "H3 an offset inside a line: the suffix carries T but began before a line boundary, and no later line does: #f"
      (let* ((text "(error a (x)) (error b (attempt \"0123456789abcdef\"))\n(error c (x))\n")
             (at (let loop ((i 0)) (if (string=? (substring text i (+ i 9)) " (error b") i (loop (+ i 1))))))
        (select-report (u8 text) at T))
      #f)
(want "H3 a datum carrying T with no terminating newline: #f"
      (select-report (u8 "(error a (attempt \"0123456789abcdef\"))") 0 T)
      #f)
(want "H3 two lines carrying T: the LAST"
      (select-report (u8 "(error one (attempt \"0123456789abcdef\"))\n(error two (attempt \"0123456789abcdef\"))\n") 0 T)
      '(error two (attempt "0123456789abcdef")))
(want "H3 another token's line then T's, and the reverse: T's both times"
      (list (select-report (u8 "(error x (attempt \"ffffffffffffffff\"))\n(error mine (attempt \"0123456789abcdef\"))\n") 0 T)
            (select-report (u8 "(error mine (attempt \"0123456789abcdef\"))\n(error x (attempt \"ffffffffffffffff\"))\n") 0 T))
      '((error mine (attempt "0123456789abcdef")) (error mine (attempt "0123456789abcdef"))))
(want "H3 a line that does not parse, then T's line: T's line (the bad line is skipped)"
      (select-report (u8 "(error broken (\n(error mine (attempt \"0123456789abcdef\"))\n") 0 T)
      '(error mine (attempt "0123456789abcdef")))
(want "H3 a multibyte prefix: a two-byte character and a newline, then (error b ...), at BYTE offset 3 selects (error b ...)"
      (select-report (u8 "\x00e9;\n(error b (attempt \"0123456789abcdef\"))\n") 3 T)
      '(error b (attempt "0123456789abcdef")))
;; M2b1 review r1, F4: T's report wholly BEFORE the offset is an earlier
;; start's, even when nothing after the offset carries T.
(want "H3 T's report wholly before the offset, only another token's after: #f"
      (let* ((first-line "(error old (attempt \"0123456789abcdef\"))\n")
             (b (u8 (string-append first-line "(error x (attempt \"ffffffffffffffff\"))\n"))))
        (select-report b (bytevector-length (u8 first-line)) T))
      #f)
;; M2b1 review r1, F1: a line is a report only if it is one datum.
(want "H3 a line whose report datum is followed by more text is not a report: the earlier whole line is chosen"
      (list (select-report (u8 "(error good (attempt \"0123456789abcdef\"))\n(error bad (attempt \"0123456789abcdef\")) (\n") 0 T)
            (select-report (u8 "(error good (attempt \"0123456789abcdef\"))\n(error bad (attempt \"0123456789abcdef\")) (x)\n") 0 T))
      '((error good (attempt "0123456789abcdef")) (error good (attempt "0123456789abcdef"))))
;; M2b1 review r1, F2: carrying the token is not enough; the shape is the
;; daemon's -- `(error <symbol> <clauses headed by symbols>)`, the attempt last.
;; M2b1 review r3: every condition of carries-token? and of line-datum's
;; readable-shape? gate has an input here that only it rejects (the table
;; in r4 NOTES). Some inputs make a weakened check RAISE rather than
;; answer, which the row reads as a value other than #f.
(want "H3 token-bearing lines of another shape are not reports: no kind, a non-symbol kind, attempt not last, a clause that is not a list, a clause headed by a non-symbol, a head other than error, a dotted clause, (error) alone, an improper report, an empty clause, a datum comment"
      (map (lambda (line) (select-report (u8 (string-append line "\n")) 0 T))
           (list "(error (attempt \"0123456789abcdef\"))"
                 "(error \"k\" (attempt \"0123456789abcdef\"))"
                 "(error k (attempt \"0123456789abcdef\") (x 1))"
                 "(error k junk (attempt \"0123456789abcdef\"))"
                 "(error k (123 x) (attempt \"0123456789abcdef\"))"
                 "(oops k (attempt \"0123456789abcdef\"))"
                 "(error k (x . 1) (attempt \"0123456789abcdef\"))"
                 "(error)"
                 "(error k (attempt \"0123456789abcdef\") . 1)"
                 "(error k () (attempt \"0123456789abcdef\"))"
                 "(error k #;(x) (attempt \"0123456789abcdef\"))"))
      '(#f #f #f #f #f #f #f #f #f #f #f))
;; The log's length before the spawn is past its end now: nothing in it is
;; this start's (the scan begins at the end), even a line carrying T.
;; The offset falls inside a last line that has no line break yet: the scan
;; starts at the end, not back at the beginning.
(want "H3 an offset inside an unterminated last line: #f, although an earlier line carries T"
      (let ((text "(error old (attempt \"0123456789abcdef\"))\n(error x"))
        (select-report (u8 text) (- (string-length text) 3) T))
      #f)
;; F111 (M3b): invalid UTF-8 is neither a raise nor a report. The decode has
;; no guard now (utf8->string gives U+FFFD, the M3a r1 reading), so a line
;; holding an invalid byte is refused by readable-shape?, and a later whole
;; line is still read.
(want "H3 invalid bytes: an invalid byte before T's report on one line is not a report; a line of a truncated sequence before T's line is skipped"
      (list (select-report (u8-list->bytevector (cons #xFF (bytevector->u8-list (u8 "(error a (attempt \"0123456789abcdef\"))\n")))) 0 T)
            (select-report (u8-list->bytevector (append (list #xE2 #x82 10) (bytevector->u8-list (u8 "(error b (attempt \"0123456789abcdef\"))\n")))) 0 T))
      '(#f (error b (attempt "0123456789abcdef"))))
(want "H3 an offset past the end of the bytes: #f, although the bytes carry T's report"
      (let ((b (u8 "(error a (attempt \"0123456789abcdef\"))\n")))
        (select-report b (+ (bytevector-length b) 10) T))
      #f)

;; H4 (item 4): waitpid-status on this process's own children.
(let* ((first (spawn-detached! (list "/bin/sh" "-c" "exit 0")))
       (second (spawn-detached! (list "/bin/sh" "-c" "sleep 1; exit 75")))
       (early (waitpid-status second))
       (later (let loop ((k 0))
                (let ((st (waitpid-status second)))
                  (if (or st (>= k 150)) st (begin (sleep (make-time 'time-duration 50000000 0)) (loop (+ k 1)))))))
       (killed (spawn-detached! (list "/bin/sh" "-c" "kill -9 $$")))
       (sig (let loop ((k 0))
              (let ((st (waitpid-status killed)))
                (if (or st (>= k 100)) st (begin (sleep (make-time 'time-duration 50000000 0)) (loop (+ k 1))))))))
  (waitpid-status first)
  (want "H4 waitpid-status: #f while the child runs, then (exit 75); a child killed by signal 9 is (signal 9)"
        (list early later sig)
        '(#f (exit 75) (signal 9)))
  (want "H4 a pid that is not a child raises durable-error with op waitpid and errno ECHILD"
        (guard (e ((fs-error? e) (list (fs-error-op e) (fs-error-errno e))))
          (waitpid-status 1))
        (list 'waitpid ECHILD)))

;; H8 (item 7, G10) and the Q8 pin: the hold seam in a child process armed
;; by THEOURGIA_HOLD (THEOURGIA_INJECT=on). The child writes `done` after the
;; hold point returns.
(define (hold-child! name env stage)
  (let ((script (under (string-append name ".ss")))
        (done (under (string-append name ".done")))
        (err (under (string-append name ".err"))))
    (call-with-output-file script
      (lambda (o)
        (write '(import (chezscheme) (theourgia ffi)) o)
        (write `(begin (hold-point! ',stage)
                       (call-with-output-file ,done (lambda (p) (write 'done p)))) o)))
    (system (string-append "THEOURGIA_INJECT=on " env " scheme --script " script " > /dev/null 2> " err " < /dev/null &"))
    (list done err)))
(define (wait-for path limit-ms)
  (let loop ((k 0))
    (cond ((file-exists? path) (* k 20))
          ((>= (* k 20) limit-ms) #f)
          (else (sleep (make-time 'time-duration 20000000 0)) (loop (+ k 1))))))
(let* ((p (under "h8-release"))
       (child (hold-child! "h8" (string-append "THEOURGIA_HOLD=client-scan:" p) 'client-scan))
       (held (wait-for (string-append p ".held") 5000))
       (early (begin (sleep (make-time 'time-duration 0 2)) (file-exists? (car child)))))
  (call-with-output-file p (lambda (o) (write 'go o)))
  (let ((after (wait-for (car child) 2000)))
    (want "H8 a held process creates <p>.held, does not proceed for 2 s, and proceeds after <p> is created"
          (list (and held #t) early (and after #t))
          '(#t #f #t))))
;; The wait is measured from the marker (M2b1 review r1, F6): a hold that
;; expired at once would also proceed and print the line.
(let* ((p (under "h8x-release"))
       (child (hold-child! "h8x" (string-append "THEOURGIA_HOLD=client-scan:" p " THEOURGIA_HOLD_MS=1000") 'client-scan))
       (held (wait-for (string-append p ".held") 8000))
       (after (and held (wait-for (car child) 8000))))
  (want "H8 with THEOURGIA_HOLD_MS=1000 and no release it proceeds at least 900 ms after <p>.held appears, and its stderr says (theourgia hold-expired client-scan)"
        (list (and after #t) (and after (>= after 900))
              ;; One of stderr's lines, exactly: an armed process also prints
              ;; its injection banner there.
              (let ((t (guard (e (#t "")) (call-with-input-file (cadr child) get-string-all))))
                (and (string? t)
                     (let split ((cs (string->list t)) (cur '()))
                       (cond ((null? cs) (string=? (list->string (reverse cur)) "(theourgia hold-expired client-scan)"))
                             ((char=? (car cs) #\newline)
                              (or (string=? (list->string (reverse cur)) "(theourgia hold-expired client-scan)")
                                  (split (cdr cs) '())))
                             (else (split (cdr cs) (cons (car cs) cur))))))))
        '(#t #t #t)))
;; Its own program and its own `done` path: h8.ss's `done` already exists
;; after the rows above, so running h8.ss again fails at that write with the
;; same 255 whether or not the load refuses the stage (M2b1 r1, ME-10).
;; The body calls hold-point! first: Chez runs a library's body only when
;; the program references one of its bindings, so a program that merely
;; imports (theourgia ffi) never reaches the check.
(let* ((script (under "h8u.ss"))
       (done (under "h8u.done")))
  (call-with-output-file script
    (lambda (o)
      (write '(import (chezscheme) (theourgia ffi)) o)
      (write `(begin (hold-point! 'client-scan)
                     (call-with-output-file ,done (lambda (p) (write 'done p)))) o)))
  (let ((rc (system (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=nowhere:" (under "h8u")
                                   " scheme --script " script " > /dev/null 2>&1 < /dev/null"))))
    (want "H8 an unknown hold stage is refused at load: rc 255, and the program's body never runs"
          (list rc (file-exists? done))
          '(255 #f))))
;; A PROGRAM THAT USES FFI AND NEVER HOLDS: the refusal is the library's,
;; not the first hold's (M2b1 review r1, F7). hold-sleeper-set! is the
;; daemon's entry before any actor exists; it reaches no hold point.
(define (refused-at-load? name env)
  (let ((script (under (string-append name ".ss")))
        (done (under (string-append name ".done"))))
    (call-with-output-file script
      (lambda (o)
        (write '(import (chezscheme) (theourgia ffi)) o)
        (write `(begin (hold-sleeper-set! (lambda (ms) (void)))
                       (call-with-output-file ,done (lambda (p) (write 'done p)))) o)))
    (let ((rc (system (string-append "THEOURGIA_INJECT=on " env
                                     " scheme --script " script " > /dev/null 2>&1 < /dev/null"))))
      (list rc (file-exists? done)))))
(want "H8 an unknown hold stage is refused when a program that never holds first uses ffi: rc 255, the body never runs"
      (refused-at-load? "h8n" (string-append "THEOURGIA_HOLD=nowhere:" (under "h8n")))
      '(255 #f))
;; M2b1 review r1, F3: a wait that could never expire, or could not be
;; compared, is refused like a stage.
(want "H8 THEOURGIA_HOLD_MS of +inf.0 and of 1+2i are refused at load: rc 255, the body never runs"
      (list (refused-at-load? "h8i" "THEOURGIA_HOLD_MS=+inf.0")
            (refused-at-load? "h8c" "THEOURGIA_HOLD_MS=1+2i"))
      '((255 #f) (255 #f)))
;; Q8: a hold inside an open record scope leaves the record without the
;; .held marker's creation while the marker exists (the lexical flag).
(let* ((p (under "q8-release"))
       (script (under "q8.ss"))
       (out (under "q8.out")))
  (call-with-output-file p (lambda (o) (write 'go o)))
  (call-with-output-file script
    (lambda (o)
      (write '(import (chezscheme) (theourgia ffi)) o)
      (write '(write (with-mutation-record (lambda () (hold-point! 'client-scan) (mutation-record)))) o)))
  (let ((rc (system (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=client-scan:" p
                                   " scheme --script " script " > " out " 2> /dev/null < /dev/null"))))
    (want "Q8 a hold inside an open scope: the record holds no entry for <p>.held, and <p>.held exists"
          (list rc (guard (e (#t 'unread)) (call-with-input-file out read)) (file-exists? (string-append p ".held")))
          '(0 () #t))))

(define scope-exits '(normal raise escape))
(define scope-enclosings '(none outer))
(define (cell-path tag part) (under (string-append tag "-" part)))
(define (scope-cell exit enclosing tag)
  (define (inner)
    (case exit
      ((normal)
       (with-mutation-record (lambda () (make-one! (cell-path tag "inner")))))
      ((raise)
       (guard (e ((eq? e 'cell-boom) 'caught))
         (with-mutation-record
           (lambda () (make-one! (cell-path tag "inner")) (raise 'cell-boom)))))
      ((escape)
       (call/cc
         (lambda (k)
           (with-mutation-record
             (lambda () (make-one! (cell-path tag "inner")) (k 'out))))))))
  (case enclosing
    ((none)
     (let ((pre (mutation-record)))
       (inner)
       (given-back pre (mutation-record))))
    ((outer)
     ;; THE OUTER RECORD, AND WHAT IS LEFT AFTER THE OUTER SCOPE HAS GONE
     ;; (ruling K; review r5): read inside, the outer record cannot see an
     ;; outer scope whose own exit leaves it installed.
     (let* ((pre (mutation-record))
            (rec (with-mutation-record
                   (lambda ()
                     (make-one! (cell-path tag "before"))
                     (inner)
                     (make-one! (cell-path tag "after"))
                     (mutation-record)))))
       (list rec (given-back pre (mutation-record)))))))
;; A FRESH ACTOR FINDS NO RECORD: each cell runs in an actor of its own, so
;; unlike facade-ffi's process-wide key, a record found before the scope is
;; itself a failure (one key shared between actors), and after the scope
;; the record must be () again.
(define (given-back pre post)
  (cond ((not (null? pre)) (list 'found-before pre))
        ((equal? post pre) '())
        (else (list 'left-behind post))))
(define (scope-cell-expect enclosing tag)
  (case enclosing
    ((none) '())
    ((outer)
     (list (list (list 'create (cell-path tag "before")) (list 'write (cell-path tag "before"))
                 (list 'create (cell-path tag "after")) (list 'write (cell-path tag "after")))
           '()))))

(start-scheduler
  (lambda ()
    (let ((main self))

      ;; ---- (a) two actors, disjoint records -----------------------------
      ;; BOTH SCOPES ARE OPEN BEFORE EITHER WRITES (F100a review r1). A
      ;; opens its scope and parks; B opens its scope and parks; then A
      ;; writes, then B writes, then each reports. With the record keyed by
      ;; the actor each holds only its own entries. With one key for both,
      ;; B's scope, installed last, would take A's write as well -- the
      ;; earlier shape, where A wrote before B's scope opened, stayed green
      ;; under that mistake.
      (let* ((a (spawn (lambda ()
                         (guard (e (#t (send main (list 'a-record (raised-as e)))))
                         (with-mutation-record
                           (lambda ()
                             (send main '(a-open))
                             (receive (after 5000 'no-go) (`(write) 'go))
                             (make-one! (under "rec-a"))
                             (send main '(a-wrote))
                             (receive (after 5000 'no-go) (`(report) 'go))
                             (send main (list 'a-record (mutation-record)))))))))
             (b (spawn (lambda ()
                         (guard (e (#t (send main (list 'b-record (raised-as e)))))
                         (with-mutation-record
                           (lambda ()
                             (send main '(b-open))
                             (receive (after 5000 'no-go) (`(write) 'go))
                             (make-one! (under "rec-b"))
                             (send main '(b-wrote))
                             (receive (after 5000 'no-go) (`(report) 'go))
                             (send main (list 'b-record (mutation-record))))))))))
        (receive (after 5000 'no-a-open) (`(a-open) 'ok))
        (receive (after 5000 'no-b-open) (`(b-open) 'ok))
        (send a '(write))
        (receive (after 5000 'no-a-wrote) (`(a-wrote) 'ok))
        (send b '(write))
        (receive (after 5000 'no-b-wrote) (`(b-wrote) 'ok))
        (send a '(report))
        (let ((a-rec (receive (after 5000 'no-a-record) (`(a-record ,r) r))))
          (send b '(report))
          (let ((b-rec (receive (after 5000 'no-b-record) (`(b-record ,r) r))))
            (want "F100-A record (a) two actors with both scopes open, writing in turn, each record only their own file"
                  (list a-rec b-rec (bytes-of (under "rec-a")) (bytes-of (under "rec-b")))
                  (list (list (list 'create (under "rec-a")) (list 'write (under "rec-a")))
                        (list (list 'create (under "rec-b")) (list 'write (under "rec-b")))
                        "r\n" "r\n")))))

      ;; ---- (b) no scope, no record ----------------------------------------
      (spawn (lambda ()
               (send main (list 'none-record
                                (guard (e (#t (raised-as e)))
                                  (make-one! (under "rec-none"))
                                  (mutation-record))))))
      (want "F100-A record (b) an actor with no scope notes nothing; mutation-record outside a scope is ()"
            (receive (after 5000 'no-none-record) (`(none-record ,r) r))
            '())

      ;; ---- (c) a scope hands its entries to the actor it spawns -----------
      ;; The spawning scope creates one file, spawns a worker with the entries
      ;; so far, the worker's scope creates a second, and the spawner reads
      ;; the combined record back from the worker.
      (want "F100-A record (c) a scope that spawns its work hands the entries so far and reads the combined record back"
            (list
             (with-mutation-record
              (lambda ()
                (make-one! (under "rec-c1"))
                (let ((so-far (mutation-record)))
                  (spawn (lambda ()
                           (guard (e (#t (send main (list 'c-record (raised-as e)))))
                             (with-mutation-record
                               (lambda ()
                                 (make-one! (under "rec-c2"))
                                 (send main (list 'c-record (mutation-record))))
                               so-far))))
                  (receive (after 5000 'no-c-record) (`(c-record ,r) r)))))
             (bytes-of (under "rec-c1"))
             (bytes-of (under "rec-c2")))
            (list (list (list 'create (under "rec-c1")) (list 'write (under "rec-c1"))
                        (list 'create (under "rec-c2")) (list 'write (under "rec-c2")))
                  "r\n" "r\n"))

      ;; ---- the exit matrix, each cell in its own actor --------------------
      ;; RR4 and RR5 (ruling J): the same six cells as facade-ffi's matrix --
      ;; a normal return, a raise caught outside, a continuation escape,
      ;; each with no scope around it or inside an outer one -- run under
      ;; the scheduler, one fresh actor per cell, so the key is the actor's
      ;; and a cell starts with no record. The actor answers from a guard.
      (for-each
        (lambda (exit)
          (for-each
            (lambda (enclosing)
              (let ((tag (string-append "actor-" (symbol->string exit) "-" (symbol->string enclosing))))
                (spawn (lambda ()
                         (send main (list 'cell (guard (e (#t (raised-as e)))
                                                  (scope-cell exit enclosing tag))))))
                (want (string-append "F100a exit matrix in an actor [" (symbol->string exit) " x "
                                     (symbol->string enclosing) "]: the scope gives back what was there before")
                      (receive (after 5000 'no-cell) (`(cell ,v) v))
                      (scope-cell-expect enclosing tag))))
            scope-enclosings))
        scope-exits)

      (system (string-append "rm -rf " dir))
      (printf "rows: ~a\n~a failures\nfacade-record complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
