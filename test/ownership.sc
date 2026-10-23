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

;; OWNERSHIP: THE LOCAL WRITER IS THE ONE THIS INSTANCE OWNS.
;;
;; A writer is local when its owner.sexp is there, and the first local writer
;; in name order used to be extended. A writer directory copied in from
;; another checkout carries its owner.sexp, so whichever sorted first won,
;; and an append as a foreign writer was refused on its nonce (or, when only
;; the machine field differed, accepted as that writer). The ownership table
;; (log.sc) compares each owner's (machine instance) with this store's
;; instance.sexp (machine nonce), and where a live writer names this
;; instance the selectors pass over the live foreign and legacy ones; the
;; unknown ones (an owner that cannot be read) stay where they stand.
;; Verification is unchanged, so every refusal with no owned live
;; alternative stays as it was.
;;
;; THE STORES. Every copy here is a real copy of one store, adopted on this
;; machine (an identity adopt: a new nonce and a successor writer), so its
;; own writer is owned and its predecessor is retired. A writer is made
;; foreign by copying another copy's writer directory in; legacy by an
;; owner.sexp without an instance field; unknown by an owner.sexp that
;; cannot be read. Writer names are derived, so a row that needs one writer
;; to sort before another picks the copies whose names sort that way.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia working) working-new! working-delete!)
        (only (theourgia code-project) lang-fields code-field)
        (only (theourgia languages) language-for-path)
        (only (theourgia store) store-evidence)
        (only (theourgia request) actor-sub ev-actor ev-payload)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut state-block-ids state-revoked)
        (only (theourgia log) log-publish! segment-sha session-commit! store-writers log-begin log-end! session-view session-view-refusal
              session-append! session-applied! make-frame view-revision view-epoch view-writer view-expect-seq
              view-applied-cut))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/ownership-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'ownership "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(sh "mkdir -p " (quoted (string-append root "/home")))

(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a) (cond ((not (pair? a)) a) ((eq? (car a) 'error) (list 'error (cadr a))) (else (car a))))
(define (checked what a) (unless (eq? 'ok (head-of a)) (assertion-violation 'ownership what a)) a)
(define (clause name datum)
  (let ((c (and (pair? datum) (list? datum)
                (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr datum)))))
    (and c (cdr c))))
(define (clause1 name datum) (let ((c (clause name datum))) (and c (pair? c) (car c))))
(define (file-datum path) (guard (e (#t 'UNREADABLE)) (call-with-input-file path read)))
(define (file-bytes path)
  (guard (e (#t 'NO-FILE))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) #vu8() b))))
(define (writer-path st w . name) (apply string-append st "/writers/" w (map (lambda (x) (string-append "/" x)) name)))
(define (alist-ref d key)
  (let ((c (and (list? d) (find (lambda (x) (and (pair? x) (eq? (car x) key) (pair? (cdr x)))) d))))
    (and c (cadr c))))

(define n 0)
(define (fresh-dir!)
  (set! n (+ n 1))
  (string-append root "/s" (number->string n)))
;; THE ORIGINAL: one store with a few records, never written after copying
(define (store!)
  (let ((st (fresh-dir!)))
    (sh "mkdir -p " (quoted st))
    (checked "init" (ask st 'init))
    (set! block (car (car (clause1 'state (checked "insert" (ask st 'insert "--title" "one" "--text" "first"))))))
    (set! block2 (car (car (clause1 'state (checked "insert" (ask st 'insert "--title" "two" "--text" "second"))))))
    (sh "mkdir -p " (quoted (string-append root "/src")) " && printf '%s\\n' '(define y 2)' > "
        (quoted (string-append root "/src/held.sc")))
    (checked "import-code" (ask st 'import-code (string-append root "/src")))
    (set! file-holder (let ((state (open-and-reduce st)))
                        (find (lambda (id) (equal? (guard (e (#t #f)) (code-field state id 'path)) "held.sc"))
                              (state-block-ids state))))
    st))
;; A SECOND BLOCK, for the rows that hold two drafts of one writer
(define block2 #f)
;; A FILE HOLDER every copy holds, for the delete row
(define file-holder #f)
;; THE ONE BLOCK every copy holds, the target of the draft rows
(define block #f)
;; A COPY, adopted: a new nonce and a successor writer owned by it
(define (copy! from)
  (let ((st (fresh-dir!)))
    (sh "cp -R " (quoted from) " " (quoted st))
    (checked "adopt" (ask st 'adopt))
    st))
;; THE WRITER THE STORE OWNS, read from the files: owner naming instance.sexp's
;; machine and nonce, not retired
(define (owned-of st)
  (let ((inst (file-datum (string-append st "/instance.sexp"))))
    (find (lambda (w)
            (let ((o (file-datum (writer-path st w "owner.sexp"))))
              (and (list? o)
                   (equal? (alist-ref o 'instance) (alist-ref inst 'nonce))
                   (equal? (alist-ref o 'machine) (alist-ref inst 'machine))
                   (not (file-exists? (writer-path st w "retired.sexp"))))))
          (list-sort string<? (store-writers st)))))
(define (copy-writer! from to w)
  (sh "cp -R " (quoted (writer-path from w)) " " (quoted (writer-path to w))))
;; every segment of W's log, as bytes: "the log did not grow" compares two of these
(define (log-bytes st w)
  (let ((dir (writer-path st w)))
    (map (lambda (f) (cons f (file-bytes (string-append dir "/" f))))
         (list-sort string<? (filter (lambda (f) (and (> (string-length f) 5)
                                                       (string=? ".sexp" (substring f (- (string-length f) 5) (string-length f)))
                                                       (char-numeric? (string-ref f 0))))
                                     (directory-list dir))))))
;; -> the writer whose block an insert made, or the refusal
(define (insert-writer st title)
  (let ((a (ask st 'insert "--title" title "--text" title)))
    (if (eq? (head-of a) 'ok)
        (let* ((id (car (car (clause1 'state a))))
               (dot (let loop ((i 0)) (if (char=? (string-ref id i) #\.) i (loop (+ i 1))))))
          (list 'wrote-as (substring id 0 dot)))
        a)))
;; the owner lines `check` reports
(define (owner-lines st)
  (let ((a (ask st 'check)))
    (filter (lambda (c) (and (pair? c) (memq (car c) '(writer-owner-foreign writer-owner-unverified
                                                         instance-absent instance-unavailable))))
            (if (list? a) a '()))))
(define (local-writer st) (clause1 'local-writer (ask st 'check)))

;; THE REFERENCE COPIES, whose writers are laid into a row's store as foreign
;; ones; SORTED holds them by their own writer's name. A copy cannot itself be
;; copied for a row: a copied directory has a new inode, and verification
;; would refuse it before ownership came into it.
(define original (store!))
;; Each one's own writer holds a record of its own, so a row has a foreign
;; segment to damage. Names are derived from the path, so three copies whose
;; names leave room below the first and above the last are kept: a row that
;; needs a writer after the last reference one must be able to get one.
(define (reference-copy)
  (let loop ()
    (let ((c (copy! original)))
      (if (and (string<? "6" (owned-of c)) (string<? (owned-of c) "t"))
          (begin (checked "insert" (ask c 'insert "--title" "own" "--text" "own record")) c)
          (loop)))))
(define sorted (list-sort (lambda (a b) (string<? (owned-of a) (owned-of b)))
                          (map (lambda (i) (reference-copy)) '(1 2 3))))
(define (nth i) (list-ref sorted i))
(define (own i) (owned-of (nth i)))
;; A ROW'S STORE: a fresh adopted copy whose own writer satisfies OK?,
;; copied again until one does (writer names are derived from the path)
(define (copy-where ok?)
  (let loop ((k 0) (tried '()))
    (when (= k 200) (assertion-violation 'ownership "no copy with the writer order the row needs" (map own '(0 1 2)) tried))
    (let ((st (copy! original)))
      (if (ok? (owned-of st)) st (loop (+ k 1) (cons (owned-of st) tried))))))
(define (after i) (lambda (w) (string<? (own i) w)))
(define (before i) (lambda (w) (string<? w (own i))))
;; ... with the writer directories of the reference copies JS laid in
(define (with-foreign ok? . js)
  (let ((st (copy-where ok?)))
    (for-each (lambda (j) (copy-writer! (nth j) st (own j))) js)
    st))

;; ---- W6: a foreign live writer beside an owned live one ------------------

;; W6 (the row the defect was reported with): each copy holds the other's writer; each still writes as
;; its own, whichever sorts first, and lists the other's as foreign
(define w6-a (with-foreign (after 0) 0))
(define w6-b (copy-where (before 0)))
(copy-writer! w6-a w6-b (owned-of w6-a))
(want "W6 the copy whose own writer sorts after the foreign one writes as its own"
      (insert-writer w6-a "w6") (list 'wrote-as (owned-of w6-a)))
(want "W6 the copy whose own writer sorts first writes as its own"
      (insert-writer w6-b "w6") (list 'wrote-as (owned-of w6-b)))
(want "W6 check lists the foreign writer on each"
      (list (owner-lines w6-a) (owner-lines w6-b))
      (list (list (list 'writer-owner-foreign (own 0)))
            (list (list 'writer-owner-foreign (owned-of w6-a)))))
(want "W6 check's local writer is the owned one"
      (local-writer w6-a) (owned-of w6-a))

;; W6b: only foreign owners whose nonces differ: refused as today (P); check
;; lists them foreign (R)
(define w6b (with-foreign (after 0) 0))
(sh "rm -rf " (quoted (writer-path w6b (owned-of w6b))))
(define w6b-foreign (own 0))
(want "W6b only foreign owners: the append is refused on the nonce, as today"
      (head-of (insert-writer w6b "w6b")) '(error refused))
(want "W6b the refusal names the nonce"
      (let ((a (insert-writer w6b "w6b"))) (and (pair? a) (list-ref a 2)))
      '(instance nonce))
(want "W6b check lists the foreign writer"
      (owner-lines w6b) (list (list 'writer-owner-foreign w6b-foreign)))

;; W6b2: one writer, its owner's nonce current, only its machine field differing:
;; the append succeeds as today (P), adopt answers not-needed (P), check lists
;; it foreign (R)
(define w6b2 (copy-where (lambda (w) #t)))
(define w6b2-own (owned-of w6b2))
(let* ((p (writer-path w6b2 w6b2-own "owner.sexp"))
       (o (file-datum p)))
  (sh "printf '%s\\n' " (quoted (format "((machine ~s) (instance ~s))" "another-machine" (alist-ref o 'instance)))
      " > " (quoted p)))
(want "W6b2 machine-only difference: the append succeeds as that writer"
      (insert-writer w6b2 "w6b2") (list 'wrote-as w6b2-own))
(want "W6b2 adopt answers not-needed"
      (ask w6b2 'adopt)
      '(error not-needed (checked (identity retired missing-generation registry-ahead damage))))
(want "W6b2 check lists it foreign"
      (owner-lines w6b2) (list (list 'writer-owner-foreign w6b2-own)))

;; ---- unknown and legacy owners ------------------------------------------

;; the source text a plain `read` answers for a block
(define (block-src a)
  (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (list? (cadr a))
       (let* ((f (assq 'fields (cadr a))) (src (and f (list? (cdr f)) (assq 'src (cdr f)))))
         (and src (cdr src)))))
(define (refusal-of a) (and (pair? a) (eq? (car a) 'error) (> (length a) 2) (list-ref a 2)))
;; the first (path p) clause anywhere in the answer
(define (refusal-path a)
  (cond ((and (pair? a) (eq? (car a) 'path) (pair? (cdr a)) (string? (cadr a))) (cadr a))
        ((pair? a) (or (refusal-path (car a)) (refusal-path (cdr a))))
        (else #f)))
(define (unreadable! p) (sh "chmod 000 " (quoted p)))
(define (readable! p) (sh "chmod 644 " (quoted p)))
(define (legacy! st w) (sh "printf '%s\\n' '((machine \"legacy-machine\"))' > " (quoted (writer-path st w "owner.sexp"))))
(define (garbled! st w) (sh "printf '%s\\n' '(((' > " (quoted (writer-path st w "owner.sexp"))))

;; W6f (P): the candidate today's selection picks has an unreadable owner,
;; beside an owned live writer sorting after it: owner-unreadable, as today
(define w6f (with-foreign (after 0) 0))
(unreadable! (writer-path w6f (own 0) "owner.sexp"))
(let ((a (insert-writer w6f "w6f")))
  (want "W6f an unreadable owner ahead of the owned writer: owner-unreadable, as today"
        (list (head-of a) (refusal-of a) (refusal-path a))
        (list '(error refused) 'owner-unreadable (writer-path w6f (own 0) "owner.sexp"))))

;; W6g (R): F < O < U, U's owner unreadable: the owned writer is selected
;; (the unknown one sorting after changes nothing). The commit barrier flushes
;; every writer's owner.sexp and answers unknown (commit-barrier-failed) when
;; it cannot open one, today as on a store holding O and U alone; so the
;; answer is compared with that store's, and the logs say who was written
(define w6g (with-foreign (lambda (w) (and (string<? (own 0) w) (string<? w (own 2)))) 0 2))
(define w6g-alone (with-foreign (lambda (w) (string<? w (own 2))) 2))
(unreadable! (writer-path w6g (own 2) "owner.sexp"))
(unreadable! (writer-path w6g-alone (own 2) "owner.sexp"))
(define w6g-own (owned-of w6g))
(define w6g-f-before (log-bytes w6g (own 0)))
(define w6g-o-before (log-bytes w6g w6g-own))
(want "W6g foreign first, owned second, unknown last: answered as O and U alone are"
      (head-of (ask w6g 'insert "--title" "w6g" "--text" "w6g"))
      (head-of (ask w6g-alone 'insert "--title" "w6g" "--text" "w6g")))
(want "W6g the owned writer's log grew" (equal? (log-bytes w6g w6g-own) w6g-o-before) #f)
(want "W6g the foreign writer's log did not grow" (equal? (log-bytes w6g (own 0)) w6g-f-before) #t)

;; W6h (R): F < U < O: refused owner-unreadable naming U's file (today: the
;; nonce); U's file mended, the append succeeds as O
(define w6h (with-foreign (after 1) 0 1))
(define w6h-own (owned-of w6h))
(unreadable! (writer-path w6h (own 1) "owner.sexp"))
(let ((a (insert-writer w6h "w6h")))
  (want "W6h foreign, unknown, owned: refused owner-unreadable naming the unknown one's file"
        (list (head-of a) (refusal-of a) (refusal-path a))
        (list '(error refused) 'owner-unreadable (writer-path w6h (own 1) "owner.sexp"))))
(readable! (writer-path w6h (own 1) "owner.sexp"))
(want "W6h the unknown owner mended: written as the owned writer"
      (insert-writer w6h "w6h") (list 'wrote-as w6h-own))

;; W6h2 (R): F < M < O, M's owner readable and not parsing: legacy, passed
;; over, written as O; check reports M unverified. (P) M alone: selected and
;; accepted as today.
(define w6h2 (with-foreign (after 1) 0 1))
(garbled! w6h2 (own 1))
(want "W6h2 foreign, malformed owner, owned: written as the owned writer"
      (insert-writer w6h2 "w6h2") (list 'wrote-as (owned-of w6h2)))
(want "W6h2 check reports the foreign writer and the unverified one"
      (owner-lines w6h2)
      (list (list 'writer-owner-foreign (own 0)) (list 'writer-owner-unverified (own 1))))
(define w6h2-alone (copy-where (lambda (w) #t)))
(define w6h2-alone-w (owned-of w6h2-alone))
(garbled! w6h2-alone w6h2-alone-w)
(want "W6h2 a malformed owner alone: selected and accepted, as today"
      (insert-writer w6h2-alone "w6h2") (list 'wrote-as w6h2-alone-w))

;; W7 (R): a legacy owner (no instance field) sorting before an owned live
;; writer: the owned one is selected; check reports the legacy one
(define w7 (with-foreign (after 0) 0))
(legacy! w7 (own 0))
(want "W7 legacy first, owned second: written as the owned writer"
      (insert-writer w7 "w7") (list 'wrote-as (owned-of w7)))
(want "W7 check reports the legacy writer unverified"
      (owner-lines w7) (list (list 'writer-owner-unverified (own 0))))

;; W7a (P): legacy owners only: today's selection, the append succeeds
(define w7a (copy-where (lambda (w) #t)))
(define w7a-w (owned-of w7a))
(legacy! w7a w7a-w)
(want "W7a legacy owners only: the append succeeds as that writer"
      (insert-writer w7a "w7a") (list 'wrote-as w7a-w))

;; W7b (P+R): no instance.sexp: check reports instance-absent (R); adopt works (P)
(define w7b (copy-where (lambda (w) #t)))
(sh "rm -f " (quoted (string-append w7b "/instance.sexp")))
(want "W7b no instance.sexp: check reports instance-absent and no writer line"
      (owner-lines w7b) '((instance-absent (remedy adopt))))
(want "W7b adopt works"
      (head-of (ask w7b 'adopt)) 'ok)
(want "W7b after the adopt: no owner line"
      (owner-lines w7b) '())

;; ---- W10: verification is unchanged; selection is what changed ----------

(define (set-instance-field! st key value)
  (let* ((p (string-append st "/instance.sexp"))
         (d (file-datum p)))
    (sh "printf '%s\\n' " (quoted (format "~s" (map (lambda (c) (if (eq? (car c) key) (list key value) c)) d)))
        " > " (quoted p))))
(define (set-owner! st w machine instance)
  (sh "printf '%s\\n' " (quoted (format "((machine ~s) (instance ~s))" machine instance))
      " > " (quoted (writer-path st w "owner.sexp"))))

;; W7b2 (R): A READ THAT SUCCEEDED IS NOT A FAILED ONE.
;; instance.sexp readable, its machine field the symbol `unavailable` and its
;; nonce another: check answers its report, compares the fields as read and
;; lists the copy's own live writer foreign, and no instance-unavailable line
;; appears, whatever the nonce holds (G1: that line follows a failed read only)
(define (w7b2-check machine nonce)
  (let* ((st (copy-where (lambda (w) #t)))
         (own (owned-of st)))
    (set-instance-field! st 'machine machine)
    (set-instance-field! st 'nonce nonce)
    (let ((a (ask st 'check)))
      (list (and (pair? a) (car a))
            (equal? (owner-lines st) (list (list 'writer-owner-foreign own)))))))
(want "W7b2 machine `unavailable`, a string nonce: check lists the own writer foreign, no instance-unavailable line"
      (w7b2-check 'unavailable "n") '(check #t))
(want "W7b2 machine `unavailable`, a nonce shaped as a path and a reason: the same"
      (w7b2-check 'unavailable '("p" "r")) '(check #t))

;; W10 (P): instance.sexp's nonce changed alone: (instance nonce), refused
(define w10 (copy-where (lambda (w) #t)))
(set-instance-field! w10 'nonce "n-changed-alone")
(let ((a (insert-writer w10 "w10")))
  (want "W10 the nonce changed alone: refused on the nonce, as today"
        (list (head-of a) (refusal-of a)) '((error refused) (instance nonce))))
;; ... and adopt again completes, as an identity adopt interrupted between the
;; new nonce and the successor owner does today
(want "W10c an adopt over a changed nonce completes"
      (head-of (ask w10 'adopt)) 'ok)
(want "W10c and the next write lands in the successor it installed"
      (insert-writer w10 "w10") (list 'wrote-as (owned-of w10)))

;; W10b (R): F < O, both owners carrying the current nonce, only F's machine
;; differing: O is selected (a nonce-only comparison keeps F)
(define w10b (with-foreign (after 0) 0))
(set-owner! w10b (own 0) "another-machine"
            (alist-ref (file-datum (string-append w10b "/instance.sexp")) 'nonce))
(want "W10b the machine-only foreign writer first: written as the owned one"
      (insert-writer w10b "w10b") (list 'wrote-as (owned-of w10b)))
(want "W10b check lists it foreign"
      (owner-lines w10b) (list (list 'writer-owner-foreign (own 0))))

;; W10c (P): restore-and-pull. A' is a copy; B a copy of A'; A' pulls B's
;; successor and its predecessor's retirement: A's next write is refused on
;; the nonce, adopt-needed answers identity, and adopt mints a nonce
(define w10c-a (copy-where (lambda (w) #t)))
(define w10c-a-own (owned-of w10c-a))
(define w10c-b (let ((st (fresh-dir!)))
                 (sh "cp -R " (quoted w10c-a) " " (quoted st))
                 (checked "adopt" (ask st 'adopt))
                 st))
(copy-writer! w10c-b w10c-a (owned-of w10c-b))
(sh "cp " (quoted (writer-path w10c-b w10c-a-own "retired.sexp")) " "
    (quoted (writer-path w10c-a w10c-a-own "retired.sexp")))
(let ((a (insert-writer w10c-a "w10c")))
  (want "W10c the original copy after the pull: refused on the nonce, as today"
        (list (head-of a) (refusal-of a)) '((error refused) (instance nonce))))
(define w10c-nonce (alist-ref (file-datum (string-append w10c-a "/instance.sexp")) 'nonce))
(want "W10c adopt is an identity adopt"
      (head-of (ask w10c-a 'adopt)) 'ok)
(want "W10c and it minted a new nonce"
      (equal? w10c-nonce (alist-ref (file-datum (string-append w10c-a "/instance.sexp")) 'nonce))
      #f)

;; W10d (P): an unpublished writer copied in from another instance is read as
;; it is read where it came from
(define w10d-src (nth 2))
(define w10d-block
  (car (car (clause1 'state (checked "insert" (ask w10d-src 'insert "--title" "theirs" "--text" "their text"))))))
(define w10d (with-foreign (lambda (w) #t) 2))
(want "W10d the foreign writer's block reads as it reads at home"
      (ask w10d 'read w10d-block) (ask w10d-src 'read w10d-block))

;; W10f (R): healthy F < O: adopt answers not-needed and changes no identity,
;; owner, retirement or registry file
(define (identity-files st)
  (map (lambda (p) (cons p (file-bytes p)))
       (append (list (string-append st "/instance.sexp"))
               (apply append
                      (map (lambda (w) (list (writer-path st w "owner.sexp") (writer-path st w "retired.sexp")))
                           (list-sort string<? (store-writers st))))
               (files-under (string-append root "/home")))))
;; every regular file under DIR, by a listing the shell makes
(define (files-under dir) (find-listing dir "-type f"))
;; every entry under DIR, sockets included
(define (files-under-any dir) (find-listing dir "! -type d"))
(define (find-listing dir test)
  (let ((o (string-append root "/files-under.txt")))
    (sh "find " (quoted dir) " " test " | LC_ALL=C sort > " (quoted o))
    (call-with-input-file o
      (lambda (p) (let loop ((out '()))
                    (let ((l (get-line p))) (if (eof-object? l) (reverse out) (loop (cons l out)))))))))
(define w10f (with-foreign (after 0) 0))
(define w10f-before (identity-files w10f))
(want "W10f healthy foreign-first store: adopt is not needed"
      (ask w10f 'adopt)
      '(error not-needed (checked (identity retired missing-generation registry-ahead damage))))
(want "W10f and no identity, owner, retirement or registry file changed"
      (equal? w10f-before (identity-files w10f)) #t)

;; W7d (R): THE RETIRED PREDECESSOR: an adopted copy's
;; retired predecessor names the pre-adopt nonce and is not listed; un-retired
;; by hand, it is listed foreign
(define w7d (copy-where (lambda (w) #t)))
(define w7d-pred (find (lambda (w) (file-exists? (writer-path w7d w "retired.sexp"))) (store-writers w7d)))
(want "W7d an adopted copy: no owner line for its retired predecessor"
      (owner-lines w7d) '())
(sh "rm -f " (quoted (writer-path w7d w7d-pred "retired.sexp")))
(want "W7d the predecessor un-retired by hand is listed foreign"
      (owner-lines w7d) (list (list 'writer-owner-foreign w7d-pred)))

;; ---- W8: the supplied name on a write path ----------------------------

;; A FOREIGN DRAFT WRITER: a name with an owner.sexp naming another instance,
;; laid in by hand (a writer directory with an owner and no segment is a
;; local writer with no history)
(define (foreign-name! st name)
  (sh "mkdir -p " (quoted (writer-path st name)))
  (set-owner! st name "elsewhere" "n-elsewhere"))
(define F "frgnwrtr")
(define (draft! st w text)
  (rpc-dispatch st (list 'write block "-" "--writer" w) "test" #f #f (lambda () text) #f))
(define (draft-file st w) (writer-path st w "working" block))
(define (draft-file2 st w) (writer-path st w "working" block2))
(define (owned-aside! st) (sh "mv " (quoted (writer-path st (owned-of st) "owner.sexp")) " "
                              (quoted (string-append st "/owner.aside"))))

;; W8 (R): beside an owned live writer, a write as a foreign name is refused
;; with the remedy; (P) a session draft writer with no owner is accepted
(define w8 (copy-where (lambda (w) #t)))
(foreign-name! w8 F)
(want "W8 write --writer <foreign> beside an owned live writer: foreign-writer"
      (draft! w8 F "x") (list 'error 'foreign-writer (list 'writer F) '(remedy draft-writer)))
(want "W8 and no draft was written" (file-exists? (draft-file w8 F)) #f)
(want "W8 write --writer <a session draft writer> is accepted"
      (head-of (draft! w8 "sessionwriter" "x")) 'ok)

;; W8l (R): THE SUPPLIED NAME'S OWNER, WHATEVER THE NAME. A draft-writer name
;; outside the log writers' eight-character ids, with an owner.sexp naming
;; another instance, beside an owned live writer: refused foreign-writer,
;; no draft written; the same name with no owner.sexp is accepted
(define w8l (copy-where (lambda (w) #t)))
(foreign-name! w8l "draft-a")
(want "W8l write --writer draft-a, its owner naming another instance: foreign-writer"
      (draft! w8l "draft-a" "x") (list 'error 'foreign-writer (list 'writer "draft-a") '(remedy draft-writer)))
(want "W8l and no draft was written" (file-exists? (draft-file w8l "draft-a")) #f)
(want "W8l write --writer draft-b, no owner.sexp: accepted"
      (head-of (draft! w8l "draft-b" "x")) 'ok)

;; W8b (P): every owner foreign (none names this instance): accepted as today
(define w8b (copy-where (lambda (w) #t)))
(foreign-name! w8b F)
(set-owner! w8b (owned-of w8b) "elsewhere" "n-not-this-one")
(want "W8b no live owner names this instance: the foreign name is accepted"
      (head-of (draft! w8b F "x")) 'ok)

;; W8c (P): beside an owned but RETIRED writer: accepted as today
(define w8c (copy-where (lambda (w) #t)))
(foreign-name! w8c F)
(sh "printf '%s\\n' '((prefix 0 0 0))' > " (quoted (writer-path w8c (owned-of w8c) "retired.sexp")))
(want "W8c beside an owned retired writer: the foreign name is accepted"
      (head-of (draft! w8c F "x")) 'ok)

;; W8d (P): beside an owned live writer, a foreign-named draft written before
;; the owner names this instance stays readable and listed
(define w8d (copy-where (lambda (w) #t)))
(define w8d-own (owned-of w8d))
(foreign-name! w8d F)
(owned-aside! w8d)
(checked "a draft while no owner names this instance" (draft! w8d F "the foreign draft"))
(sh "mv " (quoted (string-append w8d "/owner.aside")) " " (quoted (writer-path w8d w8d-own "owner.sexp")))
(want "W8d read --working --writer <foreign> answers the draft"
      (ask w8d 'read block "--working" "--writer" F) '(ok (text "the foreign draft")))
(want "W8d drafts --writer <foreign> lists it"
      (map (lambda (d) (clause1 'block d))
           (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft)))
                   (let ((a (ask w8d 'drafts "--writer" F))) (or (clause 'items a) '()))))
      (list block))

;; W8f (R): discard, write --working-new and write --working-delete as the
;; foreign name beside an owned live writer: refused, its draft files
;; byte-identical afterwards (restore is W8f-restore below)
(define (cut-text st) (format "~s" (reduce-applied-cut (open-and-reduce st))))
(define (structural-new! st w path text)
  (working-new! st #f w path (lang-fields (language-for-path path)) (string->utf8 text) (cut-text st)))
(define (structural-files st w)
  (let ((d (writer-path st w "structural")))
    (if (file-exists? d) (map (lambda (f) (cons f (file-bytes (string-append d "/" f)))) (list-sort string<? (directory-list d))) '())))
(define w8f-bytes (file-bytes (draft-file w8d F)))
(want "W8f discard --writer <foreign>: refused"
      (head-of (ask w8d 'discard block "--writer" F)) '(error foreign-writer))
(want "W8f and the draft is byte-identical" (equal? (file-bytes (draft-file w8d F)) w8f-bytes) #t)
(want "W8f a structural discard --writer <foreign>: refused"
      (head-of (ask w8d 'discard "delete-x.y" "--writer" F)) '(error foreign-writer))
(want "W8f a new file as the foreign name: refused, no structural draft written"
      (list (head-of (structural-new! w8d F "new-file.sc" "(define x 1)\n")) (structural-files w8d F))
      '((error foreign-writer) ()))
(want "W8f a deleted file as the foreign name: refused, no structural draft written"
      (list (head-of (working-delete! w8d #f F file-holder (cut-text w8d))) (structural-files w8d F))
      '((error foreign-writer) ()))

;; W8f3 (P): discard as a session writer with no working area and no draft,
;; beside an owned live writer: success, and nothing created
(want "W8f3 discard of an absent draft as a fresh session writer: success"
      (ask w8d 'discard block "--writer" "nobodyyet") (list 'ok (list 'discarded block)))
(want "W8f3 and no directory was created for it"
      (file-exists? (writer-path w8d "nobodyyet")) #f)

;; W8g (R): an untracked commit as the foreign name of an unchanged
;; foreign-named draft: refused, the draft file byte-identical
(define w8g (copy-where (lambda (w) #t)))
(define w8g-own (owned-of w8g))
(foreign-name! w8g F)
(owned-aside! w8g)
(checked "an unchanged draft" (draft! w8g F "first"))
(sh "mv " (quoted (string-append w8g "/owner.aside")) " " (quoted (writer-path w8g w8g-own "owner.sexp")))
(define w8g-bytes (file-bytes (draft-file w8g F)))
(want "W8g commit --writer <foreign> of an unchanged draft: refused"
      (head-of (ask w8g 'commit block "--writer" F)) '(error foreign-writer))
(want "W8g and the draft is byte-identical" (equal? (file-bytes (draft-file w8g F)) w8g-bytes) #t)
(want "W8g a commit with an edit as the foreign name: refused too"
      (begin (owned-aside! w8g) (checked "an edit" (draft! w8g F "edited"))
             (sh "mv " (quoted (string-append w8g "/owner.aside")) " " (quoted (writer-path w8g w8g-own "owner.sexp")))
             (head-of (ask w8g 'commit block "--writer" F)))
      '(error foreign-writer))

;; ---- a session whose writer the table comes to pass over (reload!) -------

(define (append-one! s)
  (let ((v (session-view s)))
    (if v
        (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                       (view-expect-seq v) "agent:test" '() '(put "x.1" ())))
        (list 'no-view (session-view-refusal s)))))
(define (begin-session st) (log-begin st (lambda args 'applied)))
;; an append made durable and applied, as a request makes it, so the
;; session's next view is issued
(define (append-applied! s)
  (let* ((v (session-view s))
         (a (append-one! s)))
    (when (and (pair? a) (eq? (car a) 'committed))
      (session-commit! s)
      (session-applied! s (view-epoch v)
                        (cons (cons (view-writer v) (cadr a))
                              (filter (lambda (e) (not (equal? (car e) (view-writer v)))) (view-applied-cut v)))))
    a))
(define (view-writer-of st)
  (let* ((s (begin-session st))
         (v (session-view s))
         (w (if v (view-writer v) (list 'no-view (session-view-refusal s)))))
    (log-end! s)
    w))
(define (aside! st w) (sh "mv " (quoted (writer-path st w)) " " (quoted (string-append st "/aside-" w))))
(define (back! st w) (sh "mv " (quoted (string-append st "/aside-" w)) " " (quoted (writer-path st w))))

;; W6l (R): a session opened with only foreign F present; O's owned directory
;; copied in (F sorting first); the session's next write is refused
;; writer-superseded naming F and O, F's log did not grow; a new session
;; writes as O
(define w6l (with-foreign (after 0) 0))
(define w6l-own (owned-of w6l))
(aside! w6l w6l-own)
(define w6l-s (begin-session w6l))
(define w6l-before (log-bytes w6l (own 0)))
(want "W6l the session began on the foreign writer, the only one present"
      (let ((v (session-view w6l-s))) (and v (view-writer v))) (own 0))
(back! w6l w6l-own)
(want "W6l the next write through the same session: writer-superseded"
      (append-one! w6l-s)
      (list 'refused-before-reserve
            (list 'writer-superseded (list 'writer (own 0)) (list 'by w6l-own) '(remedy reopen))))
(log-end! w6l-s)
(want "W6l the foreign writer's log did not grow" (equal? (log-bytes w6l (own 0)) w6l-before) #t)
(want "W6l a new session is bound to the owned writer" (view-writer-of w6l) w6l-own)

;; W6n (R): the same with a legacy writer L (no instance field) as the only
;; owner-bearing one when the session began
(define w6n (with-foreign (after 0) 0))
(define w6n-own (owned-of w6n))
(legacy! w6n (own 0))
(aside! w6n w6n-own)
(define w6n-s (begin-session w6n))
(define w6n-before (log-bytes w6n (own 0)))
(back! w6n w6n-own)
(want "W6n a legacy session writer superseded when an owned live writer appears"
      (append-one! w6n-s)
      (list 'refused-before-reserve
            (list 'writer-superseded (list 'writer (own 0)) (list 'by w6n-own) '(remedy reopen))))
(log-end! w6n-s)
(want "W6n the legacy writer's log did not grow" (equal? (log-bytes w6n (own 0)) w6n-before) #t)
(want "W6n a new session is bound to the owned writer" (view-writer-of w6n) w6n-own)

;; W10e (R): A and B live, their owners carrying different nonces,
;; instance.sexp naming A's; a session writing as A; instance.sexp rewritten
;; to name B's nonce alone: the session's next write is refused
;; writer-superseded naming A and B (the reload instance.sexp triggers), and
;; a new session writes as B
(define w10e (with-foreign (lambda (w) #t) 0))
(define w10e-a (owned-of w10e))
(define w10e-s (begin-session w10e))
(want "W10e the session writes as A" (car (append-applied! w10e-s)) 'committed)
(define w10e-a-nonce (alist-ref (file-datum (writer-path w10e w10e-a "owner.sexp")) 'instance))
(define w10e-b-nonce (alist-ref (file-datum (writer-path w10e (own 0) "owner.sexp")) 'instance))
(define w10e-frame
  (let ((v (session-view w10e-s)))
    (make-frame (view-revision v) (view-epoch v) (view-writer v)
                (view-expect-seq v) "agent:test" '() '(put "x.1" ()))))
(define w10e-superseded
  (list 'writer-superseded (list 'writer w10e-a) (list 'by (own 0)) '(remedy reopen)))
(set-instance-field! w10e 'nonce w10e-b-nonce)
(want "W10e instance.sexp names B: the session's next write is writer-superseded"
      (append-one! w10e-s)
      (list 'refused-before-reserve w10e-superseded))
;; W10e2 (R): instance.sexp names A's nonce again and a frame taken before
;; the supersession is retried, so the session reloads once more: the
;; refusal the first reload recorded stands, the retried frame is refused
;; writer-superseded, no view is issued, and A's log did not grow
(define w10e-a-before (log-bytes w10e w10e-a))
(set-instance-field! w10e 'nonce w10e-a-nonce)
(want "W10e2 instance.sexp names A again: the retried frame is refused writer-superseded"
      (session-append! w10e-s w10e-frame)
      (list 'refused-before-reserve w10e-superseded))
(want "W10e2 and the session issues no view: still writer-superseded"
      (append-one! w10e-s)
      (list 'no-view w10e-superseded))
(want "W10e2 A's log did not grow" (equal? (log-bytes w10e w10e-a) w10e-a-before) #t)
(set-instance-field! w10e 'nonce w10e-b-nonce)
(guard (e (#t #f)) (session-commit! w10e-s))
(log-end! w10e-s)
(want "W10e a new session is bound to B" (view-writer-of w10e) (own 0))

;; ---- damage on the foreign head, damage on the owned writer -------------

(define (segments st w) (map car (log-bytes st w)))
;; the last segment holding bytes: a writer's newest segment can be empty
(define (last-segment st w)
  (writer-path st w (car (car (filter (lambda (e) (and (bytevector? (cdr e)) (> (bytevector-length (cdr e)) 0)))
                                      (reverse (log-bytes st w)))))))
(define (flip-byte! p)
  (let* ((b (file-bytes p)) (i (div (bytevector-length b) 2)))
    (bytevector-u8-set! b i (if (= (bytevector-u8-ref b i) 65) 66 65))
    (call-with-port (open-file-output-port p (file-options no-fail)) (lambda (o) (put-bytevector o b)))))
(define (deep-find key datum)
  (cond ((and (pair? datum) (eq? (car datum) key)) datum)
        ((pair? datum) (or (deep-find key (car datum)) (deep-find key (cdr datum))))
        (else #f)))

;; W6i (R): F < O, F foreign with an unreadable segment, a damaged segment or
;; a readable quarantine: the append succeeds as O (today: F is the head and
;; its own refusal); check reports F's segment
(define w6i (with-foreign (after 0) 0))
(unreadable! (last-segment w6i (own 0)))
(want "W6i the foreign head's segment unreadable: written as the owned writer"
      (insert-writer w6i "w6i") (list 'wrote-as (owned-of w6i)))
(want "W6i check still reports the foreign writer's segment"
      (and (deep-find 'segment-unreadable (ask w6i 'check)) #t) #t)
(define w6i-flip (with-foreign (after 0) 0))
(flip-byte! (last-segment w6i-flip (own 0)))
(want "W6i the foreign head's segment damaged: written as the owned writer"
      (insert-writer w6i-flip "w6i") (list 'wrote-as (owned-of w6i-flip)))
;; a readable quarantine of the writer whose segment SEG is, forking at its
;; first record
(define (quarantine! seg)
  (sh "printf '%s\\n' '((format 1) (fork 1))' > " (quoted (string-append (path-parent seg) "/quarantine.sexp"))))
(define w6i-q (with-foreign (after 0) 0))
(quarantine! (last-segment w6i-q (own 0)))
(define w6i-q-f-before (log-bytes w6i-q (own 0)))
(want "W6i the foreign head quarantined: written as the owned writer"
      (insert-writer w6i-q "w6i") (list 'wrote-as (owned-of w6i-q)))
;; (check does not name a readable quarantine: its per-writer entry is end,
;; torn and integrity, so it answers its report and lists F without a raise)
(want "W6i check answers its report with the quarantined foreign writer listed"
      (let ((a (ask w6i-q 'check)))
        (list (car a) (and (assoc (own 0) (clause1 'writers a)) #t)))
      '(check #t))
(want "W6i and the quarantined foreign writer's log did not grow"
      (equal? (log-bytes w6i-q (own 0)) w6i-q-f-before) #t)

;; W6i2 (R): F < O, O's last record depends on an F record the damage hides:
;; refused predecessor-not-applied, never written as O. W6i3 (P): O sorting
;; first, the same dependency: the same refusal, as today
(define w10d-like-block w10d-block)
;; (the set is a row of its own: with F sorting first it is written as O only
;; where the preference runs)
(define (depending-store label ok?)
  (let ((st (with-foreign ok? 2)))
    (want (string-append label " setup: O sets F's block")
          (head-of (ask st 'set w10d-like-block "--title" "O's title")) 'ok)
    (unreadable! (last-segment st (own 2)))
    st))
(define w6i2 (depending-store "W6i2" (after 2)))
(define w6i2-own-before (log-bytes w6i2 (owned-of w6i2)))
(let ((a (insert-writer w6i2 "w6i2")))
  (want "W6i2 O depends on a hidden F record: predecessor-not-applied"
        (and (deep-find 'predecessor-not-applied a) #t) #t)
  (want "W6i2 and nothing was written as O" (equal? (log-bytes w6i2 (owned-of w6i2)) w6i2-own-before) #t))
(define w6i3 (depending-store "W6i3" (before 2)))
(want "W6i3 O sorting first, the same dependency: predecessor-not-applied, as today"
      (and (deep-find 'predecessor-not-applied (insert-writer w6i3 "w6i3")) #t) #t)

;; W6j (R): O's own segment unreadable or damaged, or O quarantined, F
;; foreign and healthy ahead of it: refused with O's own refusal, the one O
;; alone gets (W6j2, P), never on F's nonce
;; -> (<refusal shape> <no log grew>)
(define (o-damaged ok? foreign? damage!)
  (let ((st (copy-where ok?)))
    (checked "O's own record" (ask st 'insert "--title" "o" "--text" "o"))
    (when foreign? (copy-writer! (nth 0) st (own 0)))
    (damage! (last-segment st (owned-of st)))
    (let* ((o (owned-of st))
           (before (list (log-bytes st o) (and foreign? (log-bytes st (own 0)))))
           (a (insert-writer st "w6j")))
      (list (refusal-shape a)
            (equal? before (list (log-bytes st o) (and foreign? (log-bytes st (own 0)))))))))
;; the head and the refusal's kind (its paths name each row's own store)
(define (refusal-shape a)
  (list (head-of a) (let ((r (refusal-of a))) (if (pair? r) (car r) r))))
;; THE REFUSALS O GETS ALONE (W6j2, P), pinned from base's answers: an
;; unreadable segment is no view (writer-unreadable), a damaged one is
;; refused integrity; nothing is written either way
(want "W6j2 O alone, its segment unreadable: writer-unreadable, nothing written"
      (o-damaged (lambda (w) #t) #f unreadable!) '(((error no-view) writer-unreadable) #t))
(want "W6j2 O alone, its segment damaged: integrity, nothing written"
      (o-damaged (lambda (w) #t) #f flip-byte!) '(((error refused) integrity) #t))
(want "W6j O's segment unreadable, F ahead: O's own refusal, no log grew"
      (o-damaged (after 0) #t unreadable!) '(((error no-view) writer-unreadable) #t))
(want "W6j O's segment damaged, F ahead: O's own refusal, no log grew"
      (o-damaged (after 0) #t flip-byte!) '(((error refused) integrity) #t))
(want "W6j2 O alone, quarantined: its quarantine refusal, nothing written"
      (o-damaged (lambda (w) #t) #f quarantine!) '(((error refused) quarantined) #t))
(want "W6j2 O sorting first, quarantined, F after it: the same refusal, nothing written"
      (o-damaged (before 0) #t quarantine!) '(((error refused) quarantined) #t))
(want "W6j O quarantined, F ahead: O's own refusal, no log grew"
      (o-damaged (after 0) #t quarantine!) '(((error refused) quarantined) #t))

;; W6k (R): F < O, F healthy, O owned and live with an unreadable
;; quarantine.sexp (discovery excludes O): refused writer-unreadable naming
;; O's file, F's log did not grow, the session never bound to F; check
;; answers its report with no local writer
(define w6k (with-foreign (after 0) 0))
(define w6k-q (writer-path w6k (owned-of w6k) "quarantine.sexp"))
(sh "printf '%s\\n' '()' > " (quoted w6k-q) " && chmod 000 " (quoted w6k-q))
(define w6k-f-before (log-bytes w6k (own 0)))
(let ((a (insert-writer w6k "w6k")))
  (want "W6k refused writer-unreadable naming the owned writer's file"
        (let ((u (deep-find 'writer-unreadable a))) (and u (cadr (assq 'path (cdr u)))))
        w6k-q))
(want "W6k the foreign writer's log did not grow" (equal? (log-bytes w6k (own 0)) w6k-f-before) #t)
;; (a strict session raises on the load O's quarantine makes incomplete)
(want "W6k a session is not bound to the foreign writer"
      (equal? (caught (view-writer-of w6k)) (own 0)) #f)
(want "W6k check answers its report, with no local writer"
      (let ((a (ask w6k 'check))) (list (car a) (clause1 'local-writer a)))
      '(check #f))

;; W6m (P): O owned and live sorting first, X another owner-bearing writer
;; whose retired.sexp cannot be read: O is selected and written. The commit
;; barrier flushes every writer's retired.sexp and answers unknown
;; (commit-barrier-failed) when it cannot open one, as today; the logs say
;; who was written
;; (X is whichever reference writer sorts after this copy's own)
(define w6m (copy-where (lambda (w) (exists (lambda (i) (string<? w (own i))) '(0 1 2)))))
(define w6m-own (owned-of w6m))
(define w6m-x (own (find (lambda (i) (string<? w6m-own (own i))) '(0 1 2))))
(copy-writer! (nth (find (lambda (i) (string=? (own i) w6m-x)) '(0 1 2))) w6m w6m-x)
(sh "printf '%s\\n' '((prefix 0 0 0))' > " (quoted (writer-path w6m w6m-x "retired.sexp")))
(unreadable! (writer-path w6m w6m-x "retired.sexp"))
(define w6m-o-before (log-bytes w6m w6m-own))
(define w6m-x-before (log-bytes w6m w6m-x))
(want "W6m an owned writer first, an unreadable retirement after it: the barrier's answer, as today"
      (head-of (ask w6m 'insert "--title" "w6m" "--text" "w6m")) '(error unknown))
(want "W6m the owned writer's log grew" (equal? (log-bytes w6m w6m-own) w6m-o-before) #f)
(want "W6m the other writer's log did not grow" (equal? (log-bytes w6m w6m-x) w6m-x-before) #t)
;; and with X sorting first: the selector's answer for X is today's (X's
;; retirement unknown, X is not passed over): refused owner-unreadable, the
;; note naming X's retired.sexp (the base's answer, read 2026-10-23)
(define w6m-first (with-foreign (after 0) 0))
(sh "printf '%s\\n' '((prefix 0 0 0))' > " (quoted (writer-path w6m-first (own 0) "retired.sexp")))
(unreadable! (writer-path w6m-first (own 0) "retired.sexp"))
(let ((a (ask w6m-first 'insert "--title" "w6m" "--text" "w6m")))
  (want "W6m X sorting first, its retirement unreadable: today's answer for X"
        (list (head-of a) (refusal-of a) (refusal-path a))
        (list '(error refused) 'owner-unreadable (writer-path w6m-first (own 0) "retired.sexp"))))

;; ---- retired owned writers, and the commit's cursor ---------------------

(define (flat d) (cond ((pair? d) (append (flat (car d)) (flat (cdr d)))) ((null? d) '()) (else (list d))))
(define (retire-by-hand! st w)
  (sh "printf '%s\\n' '((prefix 0 0 0))' > " (quoted (writer-path st w "retired.sexp"))))

;; W6d (P): a foreign live writer beside an owned RETIRED one: refused on the
;; nonce, as today (the retired owned writer authorizes nothing)
(define w6d (with-foreign (lambda (w) #t) 0))
(retire-by-hand! w6d (owned-of w6d))
(let ((a (insert-writer w6d "w6d")))
  (want "W6d foreign live beside owned retired: refused on the nonce, as today"
        (list (head-of a) (refusal-of a)) '((error refused) (instance nonce))))

;; W6e (P): every owner-bearing writer retired: the refusal is `retired`
(define w6e (copy-where (lambda (w) #t)))
(define w6e-own (owned-of w6e))
(retire-by-hand! w6e w6e-own)
(want "W6e every owner retired: refused retired, as today"
      (and (memq 'retired (flat (insert-writer w6e "w6e"))) #t) #t)

;; W6c (R): commit --writer <a draft writer> with a foreign live owner sorting
;; first and an owned live writer after it: the commit's cursor names the
;; owned log writer (writer-for's default), so the commit lands as it
(define w6c (with-foreign (after 0) 0))
(checked "a draft writer's edit" (draft! w6c "draftwriter" "edited by a draft writer"))
(define w6c-answer (ask w6c 'commit block "--writer" "draftwriter"))
(want "W6c the commit succeeds (its cursor names the owned writer)"
      (head-of w6c-answer) 'ok)
(want "W6c and the block holds the edit"
      (block-src (ask w6c 'read block)) "edited by a draft writer")

;; ---- W7c: the table's own read of instance.sexp fails ------------------

;; A CHILD PROCESS, because a fault is armed from the environment at start:
;; it answers one request with THEOURGIA_FAULT set
(define child-n 0)
(define (child-ask fault st . req)
  (set! child-n (+ child-n 1))
  (let ((script (string-append root "/child-" (number->string child-n) ".ss"))
        (out (string-append root "/child-" (number->string child-n) ".out")))
    (call-with-output-file script
      (lambda (p)
        (write '(import (chezscheme) (only (theourgia rpc) rpc-dispatch)) p)
        (write `(write (rpc-dispatch ,st ',req "test")) p)))
    (sh "THEOURGIA_INJECT=on THEOURGIA_FAULT=" (quoted fault) " scheme --script " (quoted script)
        " > " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null")
    (file-datum out)))
(define table-fault "open-fail@ownership:file=instance.sexp:errno=EIO")

;; W6g2, W6m2 (R): W6g's and W6m's stores with the fault aimed at the table's
;; read alone (the ownership stage), so the barrier's own flush of the file
;; is untouched and the append answers ok. The same fault on `check` shows it
;; reached the table: an unknown owner is not listed, an unknown retirement
;; is listed (its owner is foreign and not known to be retired).
(define (fault-on w file) (string-append "open-fail@ownership:file=" w "/" file ":errno=EIO"))
(define w6g2 (with-foreign (lambda (w) (and (string<? (own 0) w) (string<? w (own 2)))) 0 2))
(define w6g2-own (owned-of w6g2))
(define w6g2-f-before (log-bytes w6g2 (own 0)))
(let ((a (child-ask (fault-on (own 2) "owner.sexp") w6g2 'insert "--title" "w6g2" "--text" "w6g2")))
  (want "W6g2 F < O < U, U's owner unreadable to the table only: the append answers ok"
        (head-of a) 'ok)
  (want "W6g2 written as the owned writer"
        (and (pair? a) (assoc w6g2-own (or (clause1 'events a) '())) #t) #t))
(want "W6g2 the foreign writer's log did not grow" (equal? (log-bytes w6g2 (own 0)) w6g2-f-before) #t)
(want "W6g2 the fault reaches check's table: U is not listed"
      (map car (filter (lambda (c) (and (pair? c) (eq? (car c) 'writer-owner-foreign)))
                       (let ((a (child-ask (fault-on (own 2) "owner.sexp") w6g2 'check))) (if (list? a) a '()))))
      '(writer-owner-foreign))
(define w6m2 (copy-where (lambda (w) (exists (lambda (i) (string<? w (own i))) '(0 1 2)))))
(define w6m2-own (owned-of w6m2))
(define w6m2-x (own (find (lambda (i) (string<? w6m2-own (own i))) '(0 1 2))))
(copy-writer! (nth (find (lambda (i) (string=? (own i) w6m2-x)) '(0 1 2))) w6m2 w6m2-x)
(sh "printf '%s\\n' '((prefix 0 0 0))' > " (quoted (writer-path w6m2 w6m2-x "retired.sexp")))
(define w6m2-x-before (log-bytes w6m2 w6m2-x))
(let ((a (child-ask (fault-on w6m2-x "retired.sexp") w6m2 'insert "--title" "w6m2" "--text" "w6m2")))
  (want "W6m2 O first, X's retirement unreadable to the table only: the append answers ok"
        (head-of a) 'ok)
  (want "W6m2 written as the owned writer"
        (and (pair? a) (assoc w6m2-own (or (clause1 'events a) '())) #t) #t))
(want "W6m2 the other writer's log did not grow" (equal? (log-bytes w6m2 w6m2-x) w6m2-x-before) #t)
(want "W6m2 the fault reaches check's table: X, retirement unknown, is listed"
      (let ((a (child-ask (fault-on w6m2-x "retired.sexp") w6m2 'check)))
        (filter (lambda (c) (and (pair? c) (eq? (car c) 'writer-owner-foreign))) (if (list? a) a '())))
      (list (list 'writer-owner-foreign w6m2-x)))

;; W7c2 (R): THE WRITTEN WRITER IS VERIFIED TOO. One writer, its owner naming
;; another nonce (today: refused on the nonce); the table's presence test of
;; that owner failing once: the table names no head, and the session still
;; writes as the writer discovery found, whose own nonce the load read. The
;; append is refused on the nonce, as today, never accepted on an empty head.
;; (a store just made, so its one writer is the only owner-bearing one: an
;; adopted copy keeps its retired predecessor, which the table would name)
(define w7c2 (let ((st (fresh-dir!)))
               (sh "mkdir -p " (quoted st))
               (checked "init" (ask st 'init))
               (checked "insert" (ask st 'insert "--title" "one" "--text" "first"))
               st))
(define w7c2-own (owned-of w7c2))
(set-owner! w7c2 w7c2-own (alist-ref (file-datum (string-append w7c2 "/instance.sexp")) 'machine) "n-another")
(let ((a (child-ask (string-append "stat-fail@ownership:file=" w7c2-own "/owner.sexp") w7c2
                    'insert "--title" "w7c2" "--text" "w7c2")))
  (want "W7c2 the owner's presence test failing once: refused on the nonce, as today"
        (list (head-of a) (refusal-of a)) '((error refused) (instance nonce))))

;; W7c2b (R): A DISAGREEMENT IS NOT A NONCE. F < O, their owners naming F's
;; and O's nonces, instance.sexp naming the datum (divergent <O's> <F's>), so
;; neither owner names this instance; the table's presence test of F's owner
;; failing once: the table's head is O, discovery finds F, and the two
;; nonces differ. The append is refused on the nonce, whatever datum
;; instance.sexp holds, and nothing is written as F.
(define w7c2b (with-foreign (after 0) 0))
(define w7c2b-f-before (log-bytes w7c2b (own 0)))
(set-instance-field! w7c2b 'nonce
                     (list 'divergent
                           (alist-ref (file-datum (writer-path w7c2b (owned-of w7c2b) "owner.sexp")) 'instance)
                           (alist-ref (file-datum (writer-path w7c2b (own 0) "owner.sexp")) 'instance)))
(let ((a (child-ask (string-append "stat-fail@ownership:file=" (own 0) "/owner.sexp") w7c2b
                    'insert "--title" "w7c2b" "--text" "w7c2b")))
  (want "W7c2b head and written writer disagree, instance.sexp holding the disagreement: refused on the nonce"
        (list (head-of a) (refusal-of a)) '((error refused) (instance nonce))))
(want "W7c2b and F's log did not grow" (equal? (log-bytes w7c2b (own 0)) w7c2b-f-before) #t)

;; W7c3 (R): THE LOAD'S READ STANDS FOR THE REQUEST. F < O, the table's read
;; of F's owner failing once: F is unknown to the load, stays where it
;; stands and is the session's writer; the append is refused owner-unreadable
;; (the append path's refusal carries no path, as today), and nothing is
;; written as F. (A table read afresh at the append would see F foreign and
;; answer the nonce instead.)
(define w7c3 (with-foreign (after 0) 0))
(define w7c3-f-before (log-bytes w7c3 (own 0)))
(let ((a (child-ask (string-append "open-fail@ownership:file=" (own 0) "/owner.sexp:errno=EIO") w7c3
                    'insert "--title" "w7c3" "--text" "w7c3")))
  (want "W7c3 F's owner unreadable to the load once: refused owner-unreadable"
        (list (head-of a) (refusal-of a)) '((error refused) owner-unreadable)))
(want "W7c3 nothing was written as F" (equal? (log-bytes w7c3 (own 0)) w7c3-f-before) #t)

;; W10f2 (R): ADOPT DECIDES WITH ONE TABLE. Healthy F < O, the table's own
;; read of instance.sexp failing once: the table adopt builds under the store
;; lock has no identity, so nothing is preferred and adopt decides as today,
;; on F; the predecessor it retires is the head it verified, F, never O (a
;; table built afresh for the decision would read the identity and name O).
(define w10f2 (with-foreign (after 0) 0))
(define w10f2-own (owned-of w10f2))
(define w10f2-answer (child-ask table-fault w10f2 'adopt))
(want "W10f2 adopt under a failing identity read: adopts" (head-of w10f2-answer) 'ok)
(want "W10f2 the writer it retired is the foreign head it verified"
      (list (file-exists? (writer-path w10f2 (own 0) "retired.sexp"))
            (file-exists? (writer-path w10f2 w10f2-own "retired.sexp")))
      '(#t #f))

;; (P) reads answer as today
(define w7c (copy-where (lambda (w) #t)))
(want "W7c a targeted fault on the table's identity read: read answers as today"
      (child-ask table-fault w7c 'read block) (ask w7c 'read block))
(want "W7c and outline answers as today"
      (child-ask table-fault w7c 'outline) (ask w7c 'outline))
;; (R) check reports the instance unavailable
(want "W7c check reports instance-unavailable"
      (let ((a (child-ask table-fault w7c 'check)))
        (map car (filter (lambda (c) (and (pair? c) (memq (car c) '(instance-unavailable writer-owner-foreign))))
                         (if (list? a) a '()))))
      '(instance-unavailable))
;; a write keeps today's verification outcome: a healthy single-owner store
;; succeeds, a foreign-first store falls back to today's selection
(want "W7c a healthy single-owner store: the write succeeds"
      (head-of (child-ask table-fault w7c 'insert "--title" "w7c" "--text" "w7c")) 'ok)
(define w7c-f (with-foreign (after 0) 0))
(let ((a (child-ask table-fault w7c-f 'insert "--title" "w7c" "--text" "w7c")))
  (want "W7c foreign first: today's selection, refused on the nonce"
        (list (head-of a) (refusal-of a)) '((error refused) (instance nonce))))

;; (P) instance.sexp unreadable (mode 000): delivery refuses as today
(define w7c-0 (copy-where (lambda (w) #t)))
(unreadable! (string-append w7c-0 "/instance.sexp"))
(define w7c-0-answer (ask w7c-0 'read block))
(readable! (string-append w7c-0 "/instance.sexp"))
;; (the base's answer, read 2026-10-23: unreadable, naming instance.sexp)
(want "W7c instance.sexp mode 000: the read is refused by delivery's own read, as today"
      (list (head-of w7c-0-answer) (refusal-path w7c-0-answer))
      (list '(error unreadable) (string-append w7c-0 "/instance.sexp")))

;; ---- W8e: a tracked commit's replay is not gated, a new one is ----------

(define (cursor-of st)
  (let ((p (clause1 'position (ask st 'position))))
    (string-append (car p) ":" (number->string (cdr p)))))
(define (version-of st w id)
  (let ((a (ask st 'read id "--working-info" "--writer" w)))
    (list-ref (cadr a) 4)))
(define w8e (copy-where (lambda (w) #t)))
(define w8e-cursor (cursor-of w8e))
(checked "F's draft while F has no owner" (draft! w8e F "tracked edit"))
(define w8e-version (version-of w8e F block))
(define (w8e-commit req)
  (ask w8e 'commit block "--writer" F "--req" req "--cursor" w8e-cursor "--working-version" w8e-version))
(want "W8e the tracked commit succeeds while F has no owner" (head-of (w8e-commit "R1")) 'ok)
(foreign-name! w8e F)
(want "W8e the same commit retried after F gained a foreign owner: a replay"
      (let ((a (w8e-commit "R1"))) (list (head-of a) (and (memq 'replay (flat a)) #t)))
      '(ok #t))
(want "W8e a new tracked commit as F: refused foreign-writer"
      (head-of (ask w8e 'commit "--writer" F "--req" "R2" "--cursor" (cursor-of w8e)))
      '(error foreign-writer))

;; ---- W8f2: the discard's gate read and its unlink in one acquisition -----
;;
;; THE CONTENDER IS A COMMIT, not a write: no hook stops a write inside its
;; draft lock (write-after-create holds before the lock is taken), and one
;; that existed only for this row would not be worth a product line.
;; A commit as F parks inside F's draft lock (retiring its draft on BLOCK, at
;; the `retire-locked` barrier); a discard of F's other draft (BLOCK2) starts
;; and waits for the lock; F gains a foreign owner while it waits; the commit
;; is released. The discard's gate is asked inside the lock it then takes,
;; so it sees the owner and refuses, BLOCK2's draft kept. A gate read before
;; the lock passes while F has no owner and the discard removes the draft.
(define (child-cli env out . args)
  (set! child-n (+ child-n 1))
  (let ((script (string-append root "/cli-" (number->string child-n) ".ss")))
    (call-with-output-file script
      (lambda (p)
        (write '(import (chezscheme) (only (theourgia rpc) rpc-dispatch)) p)
        (write `(begin (write (rpc-dispatch ,(car args) ',(cdr args) "test")) (flush-output-port (current-output-port))) p)))
    (sh env " scheme --script " (quoted script) " > " (quoted out) " 2> " (quoted (string-append out ".err"))
        " < /dev/null &")))
(define (wait-for pred seconds)
  (let loop ((k 0))
    (cond ((pred) #t)
          ((>= k (* 10 seconds)) #f)
          (else (sleep (make-time 'time-duration 100000000 0)) (loop (+ k 1))))))
(define (non-empty? p) (and (file-exists? p) (> (bytevector-length (let ((b (file-bytes p))) (if (bytevector? b) b #vu8()))) 0)))
(define w8f2 (copy-where (lambda (w) #t)))
(define w8f2-fifo (string-append root "/w8f2.fifo"))
(sh "mkfifo " (quoted w8f2-fifo))
(checked "F's draft on block" (draft! w8f2 F "to commit"))
(checked "F's draft on block2"
         (rpc-dispatch w8f2 (list 'write block2 "-" "--writer" F) "test" #f #f (lambda () "to keep") #f))
(define w8f2-block2 (file-bytes (draft-file2 w8f2 F)))
(define w8f2-commit-out (string-append root "/w8f2-commit.out"))
(define w8f2-discard-out (string-append root "/w8f2-discard.out"))
;; TEXT inside the string S
(define (string-has? s text)
  (let ((n (string-length s)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) text) #t)
            (else (loop (+ i 1)))))))
;; TEXT inside the file at P
(define (contains? p text)
  (let ((b (file-bytes p)))
    (and (bytevector? b)
         (let* ((s (utf8->string b)) (n (string-length s)) (m (string-length text)))
           (let loop ((i 0))
             (cond ((> (+ i m) n) #f)
                   ((string=? (substring s i (+ i m)) text) #t)
                   (else (loop (+ i 1)))))))))
(child-cli (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_BARRIER=retire-locked:" (quoted w8f2-fifo))
           w8f2-commit-out w8f2 'commit block "--writer" F)
(want "W8f2 the commit is parked inside F's draft lock"
      (wait-for (lambda () (contains? (string-append w8f2-commit-out ".err") "retire-locked")) 30) #t)
(child-cli "THEOURGIA_INJECT=on THEOURGIA_TRACE=1" w8f2-discard-out w8f2 'discard block2 "--writer" F)
(want "W8f2 the discard waits for the lock"
      (wait-for (lambda () (contains? (string-append w8f2-discard-out ".err") "lock-wait")) 30) #t)
(want "W8f2 and has not answered" (non-empty? w8f2-discard-out) #f)
(foreign-name! w8f2 F)
(sh "printf go > " (quoted w8f2-fifo))
(want "W8f2 the commit completes" (wait-for (lambda () (non-empty? w8f2-commit-out)) 30) #t)
(want "W8f2 the discard answers" (wait-for (lambda () (non-empty? w8f2-discard-out)) 30) #t)
(want "W8f2 the discard's gate, read inside the lock, saw F's foreign owner"
      (head-of (file-datum w8f2-discard-out)) '(error foreign-writer))
(want "W8f2 and F's draft on block2 is byte-identical"
      (equal? (file-bytes (draft-file2 w8f2 F)) w8f2-block2) #t)
;; THE ACCEPTED HALF: the same interleaving with F's owner never installed.
;; The discard waits, then asks the gate and unlinks under ONE acquisition
;; of F's draft lock: its trace shows one flock of draft.lock, after its
;; lock-wait; a gate asked under an acquisition of its own shows two.
(define (lines-of-file p)
  (let ((b (file-bytes p)))
    (if (bytevector? b)
        (let loop ((cs (string->list (utf8->string b))) (cur '()) (out '()))
          (cond ((null? cs) (reverse (if (null? cur) out (cons (list->string (reverse cur)) out))))
                ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) out)))
                (else (loop (cdr cs) (cons (car cs) cur) out))))
        '())))
(define (draft-lock-flocks p w)
  (length (filter (lambda (l) (and (string-has? l "(trace flock ")
                                   (string-has? l (string-append "/writers/" w "/draft.lock"))))
                  (lines-of-file p))))
(define w8f2b (copy-where (lambda (w) #t)))
(define w8f2b-fifo (string-append root "/w8f2b.fifo"))
(sh "mkfifo " (quoted w8f2b-fifo))
(checked "F's draft on block" (draft! w8f2b F "to commit"))
(checked "F's draft on block2"
         (rpc-dispatch w8f2b (list 'write block2 "-" "--writer" F) "test" #f #f (lambda () "to discard") #f))
(define w8f2b-commit-out (string-append root "/w8f2b-commit.out"))
(define w8f2b-discard-out (string-append root "/w8f2b-discard.out"))
(child-cli (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_BARRIER=retire-locked:" (quoted w8f2b-fifo))
           w8f2b-commit-out w8f2b 'commit block "--writer" F)
(want "W8f2 accepted: the commit is parked inside F's draft lock"
      (wait-for (lambda () (contains? (string-append w8f2b-commit-out ".err") "retire-locked")) 30) #t)
(child-cli "THEOURGIA_INJECT=on THEOURGIA_TRACE=1" w8f2b-discard-out w8f2b 'discard block2 "--writer" F)
(want "W8f2 accepted: the discard waits for the lock"
      (wait-for (lambda () (contains? (string-append w8f2b-discard-out ".err") "lock-wait")) 30) #t)
(sh "printf go > " (quoted w8f2b-fifo))
(want "W8f2 accepted: the discard answers" (wait-for (lambda () (non-empty? w8f2b-discard-out)) 30) #t)
(want "W8f2 accepted: the draft is discarded" (head-of (file-datum w8f2b-discard-out)) 'ok)
(want "W8f2 accepted: and its file is gone" (file-exists? (draft-file2 w8f2b F)) #f)
(want "W8f2 accepted: one acquisition of F's draft lock covers the gate and the unlink"
      (draft-lock-flocks (string-append w8f2b-discard-out ".err") F) 1)

;; ---- W8h, W8h2: the completion of a persisted plan is not gated ---------

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define cli (string-append script-dir "/../core.sc"))
(define home (string-append root "/home"))
(include "crash-at.ss")
;; the product's own entry as a child, as the crashed request ran: -> its last datum
(define cli-n 0)
(define (cli-run args)
  (set! cli-n (+ cli-n 1))
  (let ((out (string-append root "/cli" (number->string cli-n) ".txt")))
    (system (string-append "env THEOURGIA_HOME=" (crash-at-quote home) " scheme --script " (crash-at-quote cli)
                           (apply string-append (map (lambda (a) (string-append " " (crash-at-quote a))) args))
                           " > " (crash-at-quote out) " 2>&1 < /dev/null"))
    (call-with-input-file out
      (lambda (p)
        (let loop ((last #f))
          (let ((x (guard (e (#t 'unreadable)) (read p))))
            (cond ((eof-object? x) last)
                  ((eq? x 'unreadable) last)
                  (else (loop x)))))))))

;; A tracked commit as the draft writer NAME, killed after its plan was
;; persisted; then a foreign owner for NAME laid in beside the owned live
;; writer; the same commit re-issued completes from the plan
(define (crashed-commit ok? name)
  (let* ((st (copy-where ok?))
         (cursor (cursor-of st)))
    (checked "the draft" (draft! st name "planned text"))
    (let* ((v (version-of st name block))
           (args (list "commit" block "--req" "R1" "--cursor" cursor "--working-version" v
                       "--writer" name "--store" st))
           (stopped (car (crash-at 2 root home cli args))))
      (foreign-name! st name)
      (list st stopped (lambda () (cli-run args))))))
;; W8h (P): O sorts first: the re-issued commit completes and retires the draft
(define w8h-name "zzzzzzzz")
(define w8h (crashed-commit (lambda (w) (string<? w w8h-name)) w8h-name))
(want "W8h the child stopped after the plan" (cadr w8h) 2)
(want "W8h the re-issued commit completes from the plan, not foreign-writer"
      (head-of ((caddr w8h))) 'ok)
(want "W8h and F's draft is retired" (file-exists? (draft-file (car w8h) w8h-name)) #f)
(want "W8h the block holds the planned text" (block-src (ask (car w8h) 'read block)) "planned text")
;; W8h2 (R): F sorts first: completion's remaining writes run as O
(define w8h2-name "00000000")
(define w8h2 (crashed-commit (lambda (w) (string<? w8h2-name w)) w8h2-name))
(want "W8h2 the child stopped after the plan" (cadr w8h2) 2)
(want "W8h2 the re-issued commit completes (its writes as the owned writer)"
      (head-of ((caddr w8h2))) 'ok)
(want "W8h2 the block holds the planned text" (block-src (ask (car w8h2) 'read block)) "planned text")

;; ---- W8k: a replay's acknowledgement verifies the preferred head --------

(define (occurrences p text)
  (let ((b (file-bytes p)))
    (if (not (bytevector? b))
        0
        (let* ((s (utf8->string b)) (n (string-length s)) (m (string-length text)))
          (let loop ((i 0) (k 0))
            (cond ((> (+ i m) n) k)
                  ((string=? (substring s i (+ i m)) text) (loop (+ i 1) (+ k 1)))
                  (else (loop (+ i 1) k))))))))
(define (registry-file) (string-append home "/instances.sexp"))
(define w8k-src (nth 0))
(define w8k-cursor (cursor-of w8k-src))
(define (w8k-request st)
  (ask st 'insert "--title" "tracked" "--text" "tracked" "--req" "R9" "--cursor" w8k-cursor))
(checked "F's tracked request at home" (w8k-request w8k-src))
;; F < O, F's directory (with the tracked record) laid in after the request
(define w8k (with-foreign (after 0) 0))
(define w8k-logs (list (log-bytes w8k (own 0)) (log-bytes w8k (owned-of w8k))))
(define w8k-marks (occurrences (registry-file) (string-append "\"" (own 0) "\"")))
(want "W8k the request replayed on the copy: a replay"
      (let ((a (w8k-request w8k))) (list (head-of a) (and (memq 'replay (flat a)) #t)))
      '(ok #t))
(want "W8k the registry gained F's acknowledgement"
      (> (occurrences (registry-file) (string-append "\"" (own 0) "\"")) w8k-marks) #t)
(want "W8k and no log grew"
      (equal? (list (log-bytes w8k (own 0)) (log-bytes w8k (owned-of w8k))) w8k-logs) #t)

;; ---- W8i: a resident daemon's draft gate reads its own table -----------
;;
;; A daemon serves the store; F's owner names this instance, and a draft
;; write as F is accepted. Then instance.sexp is rewritten to another nonce
;; and the owned writer O's owner reinstalled under it, with no request in
;; between: F's owner now names another instance while a live one names
;; this, and the next draft write as F is refused by the gate's own reading
;; of the files (the daemon's publication does not watch instance.sexp).
(define run-dir (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/own-" (number->string (get-process-id))))
(sh "rm -rf " (quoted run-dir) " && mkdir -p " (quoted run-dir))
(define (via-daemon out . args)
  (sh "THEOURGIA_RUN=" (quoted run-dir) " scheme --script " (quoted cli)
      (apply string-append (map (lambda (a) (string-append " " (quoted a))) args))
      " > " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null")
  (file-datum out))
(define w8i (copy-where (lambda (w) #t)))
(define w8i-own (owned-of w8i))
(define w8i-nonce (alist-ref (file-datum (string-append w8i "/instance.sexp")) 'nonce))
(define w8i-machine (alist-ref (file-datum (string-append w8i "/instance.sexp")) 'machine))
(sh "mkdir -p " (quoted (writer-path w8i F)))
(set-owner! w8i F w8i-machine w8i-nonce)
(define w8i-log (string-append root "/w8i-daemon.log"))
(sh "THEOURGIA_TRACE=1 THEOURGIA_RUN=" (quoted run-dir) " scheme --script " (quoted (string-append script-dir "/../theourgiad.sc"))
    " serve " (quoted w8i) " > " (quoted w8i-log) " 2>&1 < /dev/null & echo $! > " (quoted (string-append root "/w8i.pid")))
(want "W8i the daemon is listening"
      (wait-for (lambda () (pair? (filter (lambda (f) (string-has? f ".sock")) (files-under-any run-dir)))) 30) #t)
(define (w8i-ask n . args) (apply via-daemon (string-append root "/w8i-" (number->string n) ".out") args))
(want "W8i a draft write as F while F's owner names this instance: accepted"
      (head-of (w8i-ask 1 "write" block "first" "--writer" F "--store" w8i)) 'ok)
(set-instance-field! w8i 'nonce "n-rewritten")
(set-owner! w8i w8i-own w8i-machine "n-rewritten")
(want "W8i the next write as F: refused foreign-writer by the gate's own table"
      (w8i-ask 2 "write" block "second" "--writer" F "--store" w8i)
      (list 'error 'foreign-writer (list 'writer F) '(remedy draft-writer)))
;; (the command line prints a draft's text as it is: here the one word)
(want "W8i read --working --writer F still answers"
      (w8i-ask 3 "read" block "--working" "--writer" F "--store" w8i) 'first)
(want "W8i the requests were served by the daemon"
      (and (contains? w8i-log "write") #t) #t)
(sh "kill $(cat " (quoted (string-append root "/w8i.pid")) ") 2>/dev/null; sleep 1; rm -rf " (quoted run-dir))

;; ---- W8f, restore: a revoked draft is not put back as a foreign name -----
;;
;; F commits a tracked draft while it has no owner; a rival record claiming
;; the same request identity is published, both plans conflict and F's
;; draft is revoked (as revoke-restore.sc makes one). F then gains a foreign
;; owner beside the owned live writer: `restore` of that version as F is
;; refused, and no draft file appears.
(define w8r (copy-where (lambda (w) #t)))
(define w8r-own (owned-of w8r))
(checked "F's draft" (draft! w8r F "revoked text"))
(define w8r-v (version-of w8r F block))
(checked "F's tracked commit"
         (ask w8r 'commit block "--writer" F "--req" "RV" "--cursor" (cursor-of w8r) "--working-version" w8r-v))
(define w8r-plan
  (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) (store-evidence w8r (cons w8r-own "RV"))))
(define w8r-rival
  (encode-record 1 1789000000001 (ev-actor w8r-plan) '() (storable-encode (ev-payload w8r-plan))))
(want "W8f-restore the rival record publishes"
      (car (log-publish! w8r "rivalzzz" 1 w8r-rival (segment-sha w8r-rival))) 'published)
(want "W8f-restore F's draft is revoked"
      (and (find (lambda (r) (equal? w8r-v (cadr (car r)))) (state-revoked (open-and-reduce w8r) F)) #t) #t)
(foreign-name! w8r F)
(want "W8f-restore restore as the foreign name: refused"
      (ask w8r 'restore w8r-v "--writer" F) (list 'error 'foreign-writer (list 'writer F) '(remedy draft-writer)))
(want "W8f-restore and no draft file appeared" (file-exists? (draft-file w8r F)) #f)

;; ---- the end ------------------------------------------------------------
(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nownership complete\n" rows bad)
(exit (if (zero? bad) 0 1))
