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

;; NEVER: WP-04's FIRST ROW IS RETIRED, AND ITS NAME SAYS WHY: "immutable
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
;; NEVER: WP-04's SECOND ROW IS RETIRED TOO, AND FOR A PLAINER REASON THAN
;; THE FIRST: it never performed the replay it claims to guard.
;;
;; It writes "later" and then reads it back. Nothing retries in between,
;; so a build in which a replay DELETED the current draft would not move
;; it -- which is exactly the defect this batch found and fixed. A row
;; whose label names a behaviour it does not exercise is a guard nobody
;; has.
;;
;; SUCCESSOR: `plan-completion.sc`, W4' -- "the retry is a replay", "it
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
;; WP-09: A SRC THAT BEGINS WITH A UTF-8 BYTE-ORDER MARK (F261). A draft of
;; marked bytes reads back with the mark, under --working and under
;; --working-info; it commits, as its bytes; and the store read again from
;; its log (a fresh fold) and a plain read hold the same bytes, mark included.
;; The draft is written as a client sends it, text: U+FEFF in front, whose
;; UTF-8 is EF BB BF (a request's arguments are strings).
(define marked-text (string-append (string (integer->char #xFEFF)) "(define x 1)\n"))
(define marked (string->utf8 marked-text))
(define marked-id
  (let* ((a (call 'insert "--title" "M" "--text" "plain")) (ev (car (cadr (assq 'events (cdr a))))))
    (block-id (car ev) (cdr ev))))
(define (text-bytes answer)
  (let ((t (and (pair? answer) (eq? (car answer) 'ok) (assq 'text (filter pair? (cdr answer))))))
    (and t (string? (cadr t)) (string->utf8 (cadr t)))))
(define (holds-datum? tree x) (or (equal? tree x) (and (pair? tree) (or (holds-datum? (car tree) x) (holds-datum? (cdr tree) x)))))
(define marked-write (call 'write marked-id marked-text "--writer" "window-mark"))
(want "WP-09 PREMISE: the draft's bytes begin with the mark, and the draft is written"
      (list (equal? (list-head (bytevector->u8-list marked) 3) '(#xEF #xBB #xBF)) (rpc-ok? marked-write))
      '(#t #t))
(want "WP-09 a marked draft reads back under --working as text whose UTF-8 is the draft, mark included"
      (equal? (text-bytes (call 'read marked-id "--working" "--writer" "window-mark")) marked) #t)
(define marked-version (cadr (assq 'version (cdr marked-write))))
(want "WP-09 the text read back, written back as it is, is the same draft: the same version"
      (let* ((t (call 'read marked-id "--working" "--writer" "window-mark"))
             (text (and (pair? t) (eq? (car t) 'ok) (assq 'text (filter pair? (cdr t)))))
             (again (and text (call 'write marked-id (cadr text) "--writer" "window-mark"))))
        (and (pair? again) (eq? (car again) 'ok) (equal? (cadr (assq 'version (cdr again))) marked-version)))
      #t)
(want "WP-09 and under --working-info, the projection's text"
      (let* ((v (call 'read marked-id "--working-info" "--writer" "window-mark"))
             (p (and (pair? v) (assq 'projection (filter pair? (cdr v))))))
        (and p (string? (list-ref p 7)) (equal? (string->utf8 (list-ref p 7)) marked)))
      #t)
(define marked-commit (guard (e (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
                        (call 'commit marked-id "--writer" "window-mark")))
(want "WP-09 the marked draft commits" (rpc-ok? marked-commit) #t)
(want "WP-09 the store folded again from its log holds the marked bytes as the src"
      (cdr (assq 'src (cdr (assq 'fields (state-read (open-and-reduce store) marked-id))))) marked)
(want "WP-09 a plain read answers the marked bytes"
      (holds-datum? (call 'read marked-id) marked) #t)
(want "WP-09 another writer, with no draft, reads the committed marked src with the mark: --working, and --working-info's committed projection"
      (let* ((v (call 'read marked-id "--working-info" "--writer" "window-other"))
             (p (and (pair? v) (assq 'projection (filter pair? (cdr v))))))
        (list (equal? (text-bytes (call 'read marked-id "--working" "--writer" "window-other")) marked)
              (and p (list-ref p 1))
              (and p (string? (list-ref p 7)) (equal? (string->utf8 (list-ref p 7)) marked))))
      '(#t committed #t))
(define (holds-marked-text? a)
  (let find-text ((x a))
    (cond ((string? x) (let ((n (string-length marked-text)) (m (string-length x)))
                         (let loop ((i 0)) (cond ((> (+ i n) m) #f) ((string=? (substring x i (+ i n)) marked-text) #t) (else (loop (+ i 1)))))))
          ((pair? x) (or (find-text (car x)) (find-text (cdr x))))
          (else #f))))
(want "WP-09 read --md --recursive (the projection's rendering, project.sc) renders the committed marked src's text, mark included"
      (holds-marked-text? (call 'read marked-id "--md" "--recursive")) #t)
(want "WP-09 another writer writes the committed text back as it read it: drafts calls that draft fresh and unchanged"
      (let* ((t (call 'read marked-id "--working" "--writer" "window-back"))
             (text (and (pair? t) (eq? (car t) 'ok) (assq 'text (filter pair? (cdr t)))))
             (w (and text (call 'write marked-id (cadr text) "--writer" "window-back")))
             (d (call 'drafts "--writer" "window-back"))
             (items (and (pair? d) (eq? (car d) 'ok) (assq 'items (cdr d))))
             (mine (and items (find (lambda (it) (and (pair? it) (eq? (car it) 'draft) (equal? (assq 'block (cdr it)) (list 'block marked-id))))
                                    (cdr items)))))
        (and (pair? w) (eq? (car w) 'ok) mine
             (list (cadr (assq 'fresh (cdr mine))) (cadr (assq 'unchanged (cdr mine))))))
      '(#t #t))
(want "WP-09 read --md renders the committed marked src's text, mark included"
      (let ((a (call 'read marked-id "--md")))
        (let find-text ((x a))
          (cond ((string? x) (let ((n (string-length marked-text)) (m (string-length x)))
                               (let loop ((i 0)) (cond ((> (+ i n) m) #f) ((string=? (substring x i (+ i n)) marked-text) #t) (else (loop (+ i 1)))))))
                ((pair? x) (or (find-text (car x)) (find-text (cdr x))))
                (else #f))))
      #t)
;; src-text (store.sc), the judgement both sites use, on the shapes a request's
;; text cannot carry: a mark before malformed bytes, a cut mark, a mark alone.
;; Taken at run time, so a store that does not export it reads as this row's
;; red, not as a fixture that cannot load.
(define src-text-of
  (guard (e (#t #f)) (eval 'src-text (environment '(theourgia store)))))
(want "WP-09 src-text: a mark before a malformed byte, a cut mark, a mark alone, plain text, two marks, an interior U+FEFF"
      (and src-text-of
           (let ((feff (string (integer->char #xFEFF))))
             (list (src-text-of (bytevector #xEF #xBB #xBF #xFF)) (src-text-of (bytevector #xEF #xBB))
                   (equal? (src-text-of (bytevector #xEF #xBB #xBF)) feff) (src-text-of (string->utf8 "abc"))
                   (equal? (src-text-of (bytevector #xEF #xBB #xBF #xEF #xBB #xBF #x61)) (string-append feff feff "a"))
                   (equal? (src-text-of (string->utf8 (string-append "a" feff "b"))) (string-append "a" feff "b")))))
      '(#f #f #t "abc" #t #t))
;; A committed marked src in a stale-baseline refusal's since: its size is its
;; bytes. Two other writers commit marked drafts of 1024 and 1025 bytes over a
;; draft standing on the older baseline; that draft's commit is refused.
(define since-id
  (let* ((a (call 'insert "--title" "S" "--text" "s0")) (ev (car (cadr (assq 'events (cdr a))))))
    (block-id (car ev) (cdr ev))))
(define (marked-of n) (string-append (string (integer->char #xFEFF)) (make-string (- n 3) #\q)))
(call 'write since-id "mine" "--writer" "window-s1")
(call 'write since-id (marked-of 1024) "--writer" "window-s2")
(call 'commit since-id "--writer" "window-s2")
(call 'write since-id (marked-of 1025) "--writer" "window-s3")
(call 'commit since-id "--writer" "window-s3")
(want "WP-09 since: the 1025-byte marked body is elided by its bytes, the 1024-byte one handed back whole, as bytes"
      (let* ((r (call 'commit since-id "--writer" "window-s1"))
             (since (and (pair? r) (assq 'since (filter pair? (cdr r)))))
             (contents (and since (map caddr (cdr since)))))
        (list (and (pair? r) (list-head r (min 2 (length r))))
              (and contents (>= (length contents) 2) (list-head contents 2))))
      (list '(error stale-baseline)
            (list '(content elided 1025) (list 'set since-id 'src (string->utf8 (marked-of 1024))))))
(printf "~a failures\nworking-projection complete\n" failures)
(exit (if (= failures 0) 0 1))
