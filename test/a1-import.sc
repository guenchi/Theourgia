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

;; AN IMPORTED CORPUS EQUALS A LIST COMPUTED OUTSIDE THE PRODUCT, before and
;; after five fixed events, on three load paths.
;;
;; THE CORPUS AND THE LIST. test/a1/corpus is a small synthetic corpus with
;; the kinds a notes corpus holds (sections at levels 1-4 with skips, front
;; matter, a preamble, lists, quotes, fences of both kinds, indented code, a
;; table, HTML, references inside and outside code, a subdirectory, a file
;; with no heading and one with no final newline). test/a1/expected.py computes
;; the expected store from the Markdown rules, not from md.sc, and its two
;; outputs are checked in beside it. Every one of those files is pinned
;; below by sha256, so changing any of them is an edit this file shows.
;;
;; NEVER: NOT A PRIVATE CORPUS. A private notes corpus stays out of test/
;; and out of every output. The same rows run on one out of tree, by a
;; script that points THEOURGIA_A1_CORPUS and THEOURGIA_A1_EXPECTED at it;
;; the pins are then skipped, and the row says so.
;;
;; KEY: PLACES, NOT IDS. The expected list names a block `<path>#<n>` (0 the
;; document, 1.. its sections in file order). Ids are mapped to places
;; once, after the import, from the product's own paths and the preorder
;; of each document's subtree; a structure the product got wrong then
;; shows up as a wrong parent, index or field for some place.
;;
;; THE FIVE EVENTS (`set summary` becomes `set keywords`, there being
;; no `summary` field): set src, set keywords, move, link explains, set
;; title, as events.sexp lists them. A snapshot is taken after the third.
;;
;; THE THREE PATHS: (a) the writing process's own load; (b) a fresh process
;; with the snapshot moved aside; (c) a fresh process with the snapshot in
;; place, which must read it (trace). How many records each parses is
;; printed as a reading, not asserted. (b) and (c) are separate
;; processes because a restart is what they are about.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia rpc)
        (theourgia digest))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (want-1 label (caught got) (caught expected))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))

(define (env-or name default)
  (let ((v (getenv name))) (if (and (string? v) (> (string-length v) 0)) v default)))

(define private? (and (getenv "THEOURGIA_A1_CORPUS") #t))
(define corpus (env-or "THEOURGIA_A1_CORPUS" (string-append script-dir "/a1/corpus")))
(define expected-dir (env-or "THEOURGIA_A1_EXPECTED" (string-append script-dir "/a1")))

(define scratch
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/a1import-" (number->string (get-process-id))))
(when (file-exists? scratch)
  (assertion-violation 'a1-import "scratch directory already exists" scratch))
(system (string-append "mkdir -p '" scratch "'"))
(putenv "THEOURGIA_HOME" (string-append scratch "/home"))
(define store (string-append scratch "/store"))
(system (string-append "mkdir -p '" store "'"))

;; STEP TIMES, PRINTED: each step prints how long it took when it ENDS. A
;; step that runs long, or never ends, is therefore the one AFTER the last
;; step line printed, not the one that line names.
(define step-t0 (real-time))
(define (step! name)
  (let ((t (real-time)))
    (printf "   (step ~a: ~a ms)\n" name (- t step-t0))
    (set! step-t0 t)))

(define (file-bytes path)
  (call-with-port (open-file-input-port path)
    (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) (make-bytevector 0) b)))))
(define (read-datum path) (call-with-input-file path read))

;; ---- the pins ------------------------------------------------------------------
;;
;; NEVER: A CHANGED CORPUS OR LIST IS A VISIBLE EDIT. Regenerating the list and
;; moving on would make this row agree with whatever the generator last
;; said; the pins make every such change a line in this file's diff.
(define pins
  '(
    ("corpus/alpha.md" "89147dd6560edbd6a786c2716e68bf2fb1c86efcea14088172b10244de77107a")
    ("corpus/beta.md" "fa809b9e11e4bcf50a5f63632eadcf41b5b91bc0f4af3ea7a46e3b13f0c6c169")
    ("corpus/delta.md" "c2bf1b9d3608f9fb25c7d676dce92eeae003ea7a5f0ab8c3c0fcf257d42dda3e")
    ("corpus/gamma.md" "7c15070466e0fd6528aad168a91cd0630fb7a312b5cd49e06d50fde0acce6ce6")
    ("corpus/raw.md" "b8aad097777df9cae07da79f6cb007a3dec33421525e427ed11bd5b28f6df245")
    ("corpus/sub/epsilon.md" "737a594f2c9b2c04deb92cbcd805a24c89f4031415987634ac8314a13f4f2692")
    ("corpus/sub/zeta.md" "1b0eb336bc0af951f4c3d2424b6b3e41c1e59b96220769c960b99370cf43a294")
    ("after-import.sexp" "d3cfef7cdf2c3dff1424dbdd3b940d3e8ff4d479da4642e2e2358301d3283928")
    ("after-edits.sexp" "92d2937c99d73ea0359eec434b0424cc1a6dd81c81a11c059e356580cc2a5ac7")
    ("events.sexp" "2cbbceaf51672fcdf168dbfafa0e3920e4b363bfc3aad8777da75db388c58dd2")
    ("expected.py" "05f6b5c329ad613931edf595aec5ea08a096cc48a20113cd5e19b75af1142a36")
    ("dump.ss" "f65191865b5db222bb6d94b2e466414137eb3d021d5301f448b2269a79ac3c35")))

(printf "== A1: the corpus and the expected list ==\n")
(if private?
    (printf "   (THEOURGIA_A1_CORPUS is set: the pins are not checked, the corpus is not this tree's)\n")
    (want "A1-0 the corpus, the expected lists and events, their generator and the child load program are the pinned ones"
          (filter (lambda (p)
                    (not (equal? (guard (e (#t 'UNREADABLE))
                                   (bytevector->hex (sha256 (file-bytes (string-append script-dir "/a1/" (car p))))))
                                 (cadr p))))
                  pins)
          '()))

(define after-import (read-datum (string-append expected-dir "/after-import.sexp")))
(define after-edits (read-datum (string-append expected-dir "/after-edits.sexp")))
(define (section x name) (cdr (assq name (cdr x))))

;; ---- the import ----------------------------------------------------------------
(store-init! store)
(define imported (rpc-dispatch store (list 'import-md corpus) "a1"))
(step! "import")
;; rpc's import-md answers `(import <one answer per intent>)`; every one
;; of them has to be ok.
(define (has-error? x)
  (cond ((and (pair? x) (eq? (car x) 'error)) #t)
        ((pair? x) (or (has-error? (car x)) (has-error? (cdr x))))
        (else #f)))
(want "A1-1 PREMISE: the import answered, with no error among its answers"
      (if (and (pair? imported) (eq? (car imported) 'import) (not (has-error? imported))) 'ok imported)
      'ok)

(define (fields-of state id) (cdr (assq 'fields (state-read state id))))
;; The outline names the root `root`; whether as a symbol or a string is
;; not what this row is about, so both are the root.
(define (root? p) (or (eq? p 'root) (equal? p "root")))
(define (same-parent? a b) (or (equal? a b) (and (root? a) (root? b))))
;; THE CHILDREN OF EVERY BLOCK, FROM ONE OUTLINE, BUILT ONCE: filtering the
;; whole outline for each block visited makes mapping places quadratic in
;; blocks. The table keeps the outline's order, which is sibling order.
(define (children-table outline)
  (let ((t (make-hashtable string-hash string=?)))
    (for-each (lambda (r)
                (let ((p (if (root? (car r)) "root" (car r))))
                  (hashtable-update! t p (lambda (l) (cons (caddr r) l)) '())))
              outline)
    (vector-for-each (lambda (k) (hashtable-update! t k reverse '())) (hashtable-keys t))
    t))

;; place <-> id, from the state after the import
(define place->id (make-hashtable string-hash string=?))
(define id->place (make-hashtable string-hash string=?))
(let* ((state (open-and-reduce store))
       (kids (children-table (state-outline state)))
       (children (lambda (state parent) (hashtable-ref kids (if (root? parent) "root" parent) '()))))
  (for-each
    (lambda (doc)
      (let ((f (fields-of state doc)))
        (when (eq? (cdr (assq 'kind f)) 'doc)
          (let ((path (cdr (assq 'path f))) (n 0))
            (let walk ((id doc))
              (let ((place (string-append path "#" (number->string n))))
                (hashtable-set! place->id place id)
                (hashtable-set! id->place id place)
                (set! n (+ n 1))
                (for-each walk (children state id))))))))
    (children state "root")))

(step! "map places")
(define (place-of id) (hashtable-ref id->place id (list 'UNMAPPED id)))
(define (id-of place) (hashtable-ref place->id place #f))

(want "A1-2 PREMISE: every place in the expected list maps to a block, and every block to a place"
      (list (filter (lambda (row) (not (id-of (car row)))) (section after-import 'blocks))
            (- (hashtable-size id->place) (length (section after-import 'blocks))))
      '(() 0))

;; EVERY OCCURRENCE OF `from` IN `s`, REPLACED BY `to`, SCANNING FORWARD: the
;; search resumes after the text just written, never over it. A rescan
;; from the start never ends when a text is replaced by itself, which is
;; what a place the events name and the corpus lacks turns into (the
;; SELF-REPLACEMENT row below).
(define (replace-all s from to)
  (let ((n (string-length from)) (m (string-length s)))
    (if (= n 0)
        s
        (let loop ((i 0) (start 0) (out '()))
          (cond ((> (+ i n) m) (apply string-append (reverse (cons (substring s start m) out))))
                ((string=? (substring s i (+ i n)) from)
                 (loop (+ i n) (+ i n) (cons to (cons (substring s start i) out))))
                (else (loop (+ i 1) start out)))))))

;; A value as the expected list spells it: the id the first event wrote
;; into a text goes back to the place it stands for.
(define substitutions '())
(define (unsubstitute v)
  (if (string? v)
      (fold-left (lambda (s pair) (replace-all s (cdr pair) (car pair))) v substitutions)
      v))

;; The blocks of a dump (or of the live state) projected onto the expected
;; list's shape: for each expected row, the product's parent, index and the
;; fields the row names.
;; AND THE PROJECTION LOOKS THINGS UP IN TABLES BUILT ONCE, for the
;; same reason: a find over the outline and a filter of it per row was
;; quadratic in blocks.
(define (project blocks outline expected-rows)
  (let* ((parents (let ((t (make-hashtable string-hash string=?)))
                    (for-each (lambda (r) (hashtable-set! t (cadr r) (car r))) outline)
                    t))
         (by-parent (let ((t (make-hashtable string-hash string=?)))
                      (for-each (lambda (r)
                                  (let ((p (if (root? (car r)) "root" (car r))))
                                    (hashtable-update! t p (lambda (l) (cons (cadr r) l)) '())))
                                outline)
                      (vector-for-each (lambda (k) (hashtable-update! t k reverse '())) (hashtable-keys t))
                      t))
         (block-of (let ((t (make-hashtable string-hash string=?)))
                     (for-each (lambda (b) (hashtable-set! t (car b) b)) blocks)
                     t))
         (parent-of (lambda (id) (hashtable-ref parents id #f)))
         (siblings (lambda (p) (hashtable-ref by-parent (if (root? p) "root" p) '()))))
    (map (lambda (row)
           (let* ((place (car row)) (id (id-of place))
                  (b (and id (hashtable-ref block-of id #f)))
                  (fs (if b (cadr b) '()))
                  (p (and id (parent-of id))))
             (list place
                   (list 'parent (cond ((not p) 'NO-PARENT) ((root? p) "root") (else (place-of p))))
                   (list 'index (let ((sibs (if p (siblings p) '())))
                                  (let loop ((s sibs) (i 0))
                                    (cond ((null? s) 'NOT-AMONG-SIBLINGS)
                                          ((equal? (car s) id) i)
                                          (else (loop (cdr s) (+ i 1)))))))
                   (cons 'fields
                         (map (lambda (f)
                                (let ((p (assq (car f) fs)))
                                  (list (car f) (if p (unsubstitute (cdr p)) 'ABSENT))))
                              (cdr (cadddr row)))))))
         expected-rows)))

;; The live state in this process, in a dump's shape.
(define (live)
  (let ((state (open-and-reduce store)))
    (list (map (lambda (id) (list id (fields-of state id))) (map cadr (state-datum state)))
          (map (lambda (r) (list (car r) (caddr r))) (state-outline state)))))

;; A dump's differences from the expected rows: the rows that differ, each
;; as (place (got ...) (want ...)).
(define (differences got-rows want-rows)
  (fold-right (lambda (g w acc) (if (equal? g w) acc (cons (list (car w) (list 'got g) (list 'want w)) acc)))
              '() got-rows want-rows))

(printf "== A1: after the import ==\n")
(want "A1-3 after the import every block has the parent, index and fields the list gives it"
      (let ((l (live)))
        (differences (project (car l) (cadr l) (section after-import 'blocks))
                     (section after-import 'blocks)))
      '())

;; ---- the five events -------------------------------------------------------------
;; THE EVENTS ARE THE GENERATOR'S. events.sexp lists the ones it
;; applied, each `(k verb place ...)`: the in-tree corpus's fixed five, or
;; five chosen from another corpus by the rule in expected.py; an event the
;; corpus cannot supply is listed as skipped and not sent. A text's
;; `[[@<place>]]` is sent with that place's id, and read back the other way.
;; The snapshot is taken after the applied events with k below 3; the replay
;; after it is the applied events with k of 3 or more.
(define events-file
  (guard (x (#t #f)) (read-datum (string-append expected-dir "/events.sexp"))))
(define (events-part name)
  (let ((p (and (pair? events-file) (assq name (cdr events-file))))) (if p (cdr p) '())))
(define events-applied (events-part 'applied))
(printf "   (events: rule ~a, ~a applied, ~a skipped)\n"
        (let ((r (events-part 'rule))) (if (pair? r) (car r) 'none))
        (length events-applied) (length (events-part 'skipped)))

;; The places a text names as `[[@<place>]]`.
(define (named-places text)
  (let ((n (string-length text)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i 3) n) (reverse out))
            ((string=? (substring text i (+ i 3)) "[[@")
             (let close ((j (+ i 3)))
               (cond ((> (+ j 2) n) (reverse out))
                     ((string=? (substring text j (+ j 2)) "]]")
                      (let ((p (substring text (+ i 3) j)))
                        (loop (+ j 2) (if (member p out) out (cons p out)))))
                     (else (close (+ j 1))))))
            (else (loop (+ i 1) out))))))

(set! substitutions
  (apply append
         (map (lambda (e)
                (if (and (eq? (cadr e) 'set) (string? (list-ref e 4)))
                    (map (lambda (p) (cons (string-append "@" p) (or (id-of p) (string-append "@" p))))
                         (named-places (list-ref e 4)))
                    '()))
              events-applied)))
(define (substitute s)
  (fold-left (lambda (s pair) (replace-all s (car pair) (cdr pair))) s substitutions))

(want "SELF-REPLACEMENT a replacement of a text by itself ends and changes nothing"
      (list (replace-all "a @beta.md#1 b @beta.md#1" "@beta.md#1" "@beta.md#1")
            (replace-all "xax" "a" "aa"))
      '("a @beta.md#1 b @beta.md#1" "xaax"))

(define (ok? a) (and (pair? a) (eq? (car a) 'ok)))
(define (send e)
  (let ((verb (cadr e)) (args (cddr e)))
    (case verb
      ((set) (rpc-dispatch store (list 'set (id-of (car args)) (symbol->string (cadr args))
                                       (substitute (caddr args))) "a1"))
      ((move) (rpc-dispatch store (list 'move (id-of (car args)) (id-of (cadr args))
                                        "--after" (id-of (caddr args))) "a1"))
      ((link) (rpc-dispatch store (list 'link (id-of (car args)) (symbol->string (cadr args))
                                        (id-of (caddr args))) "a1"))
      (else (list 'error 'unknown-event-verb verb)))))
(step! "after-import rows")
(define before-snap (map send (filter (lambda (e) (< (car e) 3)) events-applied)))
(define snap (rpc-dispatch store (list 'snapshot) "a1"))
(define after-snap (map send (filter (lambda (e) (>= (car e) 3)) events-applied)))

(want "A1-4 PREMISE: the applied events and the snapshot answered ok"
      (map (lambda (a) (if (ok? a) 'ok a)) (append before-snap (list snap) after-snap))
      (map (lambda (a) 'ok) (append before-snap (list snap) after-snap)))

(define snap-path
  (let ((p (and (ok? snap) (assq 'snapshot (filter pair? (cdr snap))))))
    (and p (let ((s (cadr p))) (if (char=? (string-ref s 0) #\/) s (string-append store "/" s))))))
(define snap-cut
  (let ((c (and (ok? snap) (assq 'cut (filter pair? (cdr snap)))))) (and c (cadr c))))

;; ---- the three paths -------------------------------------------------------------
(define (dump-in-child)
  (let ((out (string-append scratch "/dump.txt")))
    (system (string-append "scheme --script '" script-dir "/a1/dump.ss' '" store "' > '" out "' 2> '"
                           scratch "/dump.err'"))
    (guard (e (#t (list 'dump (list 'UNREADABLE (call-with-input-file out get-string-all)))))
      (read-datum out))))

(define (dump-part d name)
  (let ((p (and (pair? d) (assq name (cdr d))))) (if p (cdr p) (list 'NO-PART name))))

(define (links-of d)
  (map (lambda (l) (list (place-of (car l)) (cadr l) (place-of (caddr l)))) (dump-part d 'links)))
(define (derived-of d)
  (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
             (map (lambda (e) (list (place-of (car e)) (place-of (cadr e)))) (dump-part d 'derived))))

(define path-b
  (and snap-path
       (begin (rename-file snap-path (string-append snap-path ".off"))
              (let ((d (dump-in-child)))
                (rename-file (string-append snap-path ".off") snap-path)
                d))))
(step! "events and path (b)")
(define path-c (dump-in-child))
(step! "path (c)")

(printf "== A1: after the five events, on three load paths ==\n")
(let ((l (live)))
  (want "A1-5 (a) the writing process's own load equals the list after the events"
        (differences (project (car l) (cadr l) (section after-edits 'blocks))
                     (section after-edits 'blocks))
        '()))
(want "A1-5 (b) a fresh process without the snapshot equals the list after the events"
      (differences (project (dump-part path-b 'blocks) (dump-part path-b 'outline) (section after-edits 'blocks))
                   (section after-edits 'blocks))
      '())
(want "A1-5 (c) a fresh process from the snapshot equals the list after the events"
      (differences (project (dump-part path-c 'blocks) (dump-part path-c 'outline) (section after-edits 'blocks))
                   (section after-edits 'blocks))
      '())

(want "A1-6 the explicit edge is the one link, on both restarts, and it carries an event pair"
      (list (links-of path-b) (links-of path-c)
            (map (lambda (l) (pair? (cadddr l))) (dump-part path-c 'links)))
      (list (section after-edits 'links) (section after-edits 'links) '(#t)))

;; NEVER: A REFERENCE IN CODE DOES NOT RESOLVE. The first event's text holds
;; the same `[[<id>]]` twice, once in a fence; after the import no text
;; names an id at all, so the derived set there is empty.
(want "A1-7 derived edges: none after the import; after the events exactly the one outside the fence"
      (list (section after-import 'derived) (derived-of path-b) (derived-of path-c))
      (list '() (section after-edits 'derived) (section after-edits 'derived)))

;; NEVER: THE SNAPSHOT PATH IS TAKEN, NOT ONLY EQUAL. Equality is what a load
;; that ignored the snapshot would also give; the trace says it was read,
;; and the replay holds only records after its cut. The count of records
;; parsed on each path is printed below as a reading, not asserted.
(want "A1-8 (c) the load read the snapshot (trace snapshot-read names it)"
      (let ((ls (dump-part path-c 'snapshot-read)))
        (list (pair? ls)
              (and snap-path (pair? ls)
                   (let ((name (let loop ((i (- (string-length snap-path) 1)))
                                 (if (char=? (string-ref snap-path i) #\/)
                                     (substring snap-path (+ i 1) (string-length snap-path))
                                     (loop (- i 1))))))
                     (exists (lambda (l) (let ((n (string-length name)) (m (string-length l)))
                                           (let scan ((i 0))
                                             (cond ((> (+ i n) m) #f)
                                                   ((string=? (substring l i (+ i n)) name) #t)
                                                   (else (scan (+ i 1)))))))
                             ls)))))
      '(#t #t))

(want "A1-8 (c) the replay after the snapshot holds as many records as applied events after its cut, each after the cut"
      (let ((replayed (dump-part path-c 'replayed)))
        (list (length replayed)
              (and (list? snap-cut)
                   (for-all (lambda (r)
                              (let ((c (assoc (car r) snap-cut)))
                                (or (not c) (> (cdr r) (cdr c)))))
                            replayed))))
      (list (length after-snap) #t))

;; A READING, NOT A ROW. On the in-tree store the snapshot path parses as
;; many records as the path without it, so "seeded from the snapshot, then
;; replayed from the start" cannot be told from a resume by this count.
;; Whether a resume skips parsing the segments before the cut is not
;; asserted here.
(printf "   (reading: records parsed with the snapshot ~a, without it ~a)\n"
        (car (dump-part path-c 'parsed)) (car (dump-part path-b 'parsed)))

(system (string-append "rm -rf '" scratch "'"))
(printf "rows: ~a\n~a failures\na1-import complete\n" rows bad)
(exit (if (zero? bad) 0 1))
