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

;; A writer's directory this store cannot read is not a writer with nothing
;; in it.
;;
;; Before this change a directory at mode 000 answered exactly what a writer
;; that never published answers: origin incomplete-publication, end 0, no
;; integrity note -- for a mirror, and for the store's own local writer. The
;; rows below put each permission mode to discovery, check, the coordinate
;; accessors and the two refusals, and hold the answers that do not change
;; (a writer that never published, a readable writer) as controls.
;;
;; The typed condition is looked up at run time, so this file loads on a
;; tree without it; there, the rows that need it are red and the rest read.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch rpc-ok?)
        (theourgia log)
        (only (theourgia wire) encode-record))

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

(define (lookup name)
  (guard (e (#t #f)) (eval name (environment '(theourgia log)))))
(define unreadable-entry? (lookup 'unreadable-entry?))
(define unreadable-entry-path (lookup 'unreadable-entry-path))
(define unreadable-entry-reason (lookup 'unreadable-entry-reason))

;; THE SCRATCH ROOT FOLLOWS THEOURGIA_TEST_ROOT, and a directory already
;; there is refused rather than reused: a pid-named directory left by an
;; earlier run is how two fixtures went red in an unrelated gate (F71).
(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/unreadable-writer-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'unreadable-writer "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

;; A SOCKET PATH MUST BE SHORT (sun_path), so it goes under the runner's socket
;; root, not the scratch root; run alone, it falls back to /tmp as it always did.
(define sock-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

;; THE RUN ROOT IS THE FIXTURE'S FOR ITS WHOLE LIFE. A block that moves it
;; restores THIS value; restoring "" would send every later eval and daemon
;; to the user's own ~/.theourgia/run (the code session's reading, 09-25).
(system (string-append "mkdir -p " root "/run"))
(putenv "THEOURGIA_RUN" (string-append root "/run"))

(define n 0)
(define (fresh-store!)
  (set! n (+ n 1))
  (let ((d (string-append root "/s" (number->string n))))
    (system (string-append "mkdir -p " d "/store " d "/home"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((store (string-append d "/store")))
      (rpc-dispatch store '(init) "test")
      store)))
(define (ask store . req) (rpc-dispatch store req "test"))
(define (local-writer store)
  (let ((c (ask store 'check)))
    (cadr (assq 'local-writer (cdr c)))))
(define M "mirrorz9")
(define (publish-mirror! store)
  (let* ((bytes (encode-record 1 1757300000001 "a" '() '(set "t" x "x")))
         (sha (segment-sha bytes)))
    (log-publish! store M 1 bytes sha)))
(define (chmod! mode path) (system (string-append "chmod " mode " " path)))
(define (wdir store w) (writer-directory store w))

;; What discovery answers, in the parts every mode must agree on.
(define (origin-and-notes store w)
  (let ((p (discover-prefix store w #f)))
    (list (discovery-origin p)
          (map (lambda (e) (list (log-error-kind e)
                                 (let ((a (assq 'path (log-error-detail e)))) (and a (cdr a)))))
               (discovery-integrity p)))))

;; A coordinate asked of an unreadable discovery must raise the typed
;; condition carrying the note's path and a reason -- not answer 0.
(define (coordinate-answer p accessor)
  (guard (e ((and unreadable-entry? (unreadable-entry? e))
             (list 'unreadable-entry (unreadable-entry-path e)
                   (and (string? (unreadable-entry-reason e)) 'reason)))
            (#t (list 'other-condition)))
    (list 'answered (accessor p))))

(printf "== U3: CONTROL -- answers that must not change ==\n")
(let ((s (fresh-store!)))
  (want "CONTROL a writer that never published is incomplete-publication with no note"
        (origin-and-notes s "neverzz9")
        '(incomplete-publication ()))
  (publish-mirror! s)
  (want "CONTROL a readable mirror reads as mirrored with no note"
        (origin-and-notes s M)
        '(mirrored ())))

(printf "== U1: a mirror directory this store cannot read ==\n")
(for-each
  (lambda (mode)
    (let ((s (fresh-store!)))
      (publish-mirror! s)
      (let ((dir (wdir s M)))
        (chmod! (car mode) dir)
        (let ((got (caught (origin-and-notes s M))))
          (chmod! "700" dir)
          (want (format "U1 mode ~a: origin unreadable, one metadata-unreadable note naming ~a"
                        (car mode) (cadr mode))
                (if (or (not (pair? got)) (eq? (car got) 'RAISED))
                    got
                    (list (car got)
                           (map (lambda (k+p)
                                  (list (car k+p)
                                        (cond ((equal? (cadr k+p) dir) 'the-directory)
                                              ((and (string? (cadr k+p))
                                                    (> (string-length (cadr k+p)) (string-length dir))
                                                    (string=? (substring (cadr k+p) 0 (+ 1 (string-length dir)))
                                                              (string-append dir "/")))
                                               'a-child)
                                              (else (cadr k+p)))))
                                (cadr got))))
                (list 'unreadable (list (list 'metadata-unreadable (cadr mode)))))))))
  '(("000" the-directory) ("100" the-directory) ("400" a-child)))

(printf "== U1 without markers: a directory holding only a segment ==\n")
(let* ((s (fresh-store!))
       (w "barezzz9")
       (dir (wdir s w)))
  (system (string-append "mkdir -p " dir))
  (call-with-port (open-file-output-port (string-append dir "/" (segment-file-name 1)) (file-options no-fail))
    (lambda (p) (put-bytevector p (encode-record 1 1757300000001 "a" '() '(set "t" x "x")))))
  (chmod! "100" dir)
  (let ((got (caught (origin-and-notes s w))))
    (chmod! "700" dir)
    (want "U1 --x with no owner and no manifest: unreadable, not incomplete-publication"
          (if (and (pair? got) (not (eq? (car got) 'RAISED))) (car got) got)
          'unreadable)))

(printf "== U7b: an unreadable discovery has no coordinates ==\n")
(let ((s (fresh-store!)))
  (publish-mirror! s)
  (let ((dir (wdir s M)))
    (chmod! "000" dir)
    (let ((p (caught (discover-prefix s M #f))))
      (chmod! "700" dir)
      (for-each
        (lambda (a)
          (want (format "U7b ~a raises unreadable-entry with the directory and a reason" (car a))
                (if (and (pair? p) (eq? (car p) 'RAISED))
                    p
                    (coordinate-answer p (cdr a)))
                (list 'unreadable-entry dir 'reason)))
        (list (cons 'end-seq discovery-end-seq)
              (cons 'end-segment discovery-end-segment)
              (cons 'end-offset discovery-end-offset)
              (cons 'segment-ranges discovery-segment-ranges)
              (cons 'physical-current discovery-physical-current)
              (cons 'current-buffer discovery-current-buffer)
              (cons 'torn discovery-torn)
              (cons 'retired-tail discovery-retired-tail))))))

(printf "== U5: check reports the writer and still reports the others ==\n")
(let ((s (fresh-store!)))
  (publish-mirror! s)
  (ask s 'insert "--under" "root" "--title" "A")
  (let* ((local (local-writer s))
         (healthy (caught (let ((c (ask s 'check)))
                            (cadr (assoc local (cadr (assq 'writers (cdr c))))))))
         (dir (wdir s M)))
    (chmod! "000" dir)
    (let ((c (caught (ask s 'check))))
      (chmod! "700" dir)
      (want "U5 check completes, the local writer's entry is as on a healthy store, the mirror's names the directory"
            (and (pair? c) (eq? (car c) 'check)
                 (let* ((ws (cadr (assq 'writers (cdr c))))
                        (m (assoc M ws))
                        (text (format "~s" m)))
                   (list (equal? (cadr (assoc local ws)) healthy)
                         (and m #t)
                         (let loop ((i 0))
                           (and (<= (+ i (string-length dir)) (string-length text))
                                (or (string=? (substring text i (+ i (string-length dir))) dir)
                                    (loop (+ i 1))))))))
            '(#t #t #t)))))

(printf "== U6: restoring the permission restores the readings ==\n")
(let ((s (fresh-store!)))
  (publish-mirror! s)
  (let* ((dir (wdir s M))
         (before (origin-and-notes s M))
         (before-end (discovery-end-seq (discover-prefix s M #f))))
    (chmod! "000" dir)
    (let ((during (caught (origin-and-notes s M))))
      (chmod! "700" dir)
      (want "U6 the reading during damage WAS unreadable, and after restoration equals the reading before"
            (list (and (pair? during) (car during))
                  (equal? (origin-and-notes s M) before)
                  (= before-end (discovery-end-seq (discover-prefix s M #f))))
            '(unreadable #t #t)))))

(printf "== U8d: two refusals, two questions ==\n")
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (owner (string-append (wdir s local) "/owner.sexp")))
  (chmod! "000" owner)
  ;; let*: the verify question is asked BEFORE the write, whose refusal is
  ;; the second question; in one let the order was unspecified.
  (let* ((v (caught (verify-instance s)))
         (w (caught (ask s 'insert "--under" "root" "--title" "B"))))
    (chmod! "600" owner)
    (want "U8d verify-instance answers owner-unreadable"
          (if (and (pair? v) (pair? (cdr v))) (list (car v) (cadr v)) v)
          '(refused owner-unreadable))
    (want "U8d a write for the sole local writer answers writer-unreadable"
          (let ((text (format "~s" w)))
            (if (let loop ((i 0))
                  (and (<= (+ i 17) (string-length text))
                       (or (string=? (substring text i (+ i 17)) "writer-unreadable")
                           (loop (+ i 1)))))
                'names-writer-unreadable
                (list 'answered (if (pair? w) (car w) w))))
          'names-writer-unreadable)))

(define (contains? text word)
  (let loop ((i 0))
    (and (<= (+ i (string-length word)) (string-length text))
         (or (string=? (substring text i (+ i (string-length word))) word)
             (loop (+ i 1))))))
(define (end-of store w) (discovery-end-seq (discover-prefix store w #f)))

(printf "== U2: a fresh write when the only local writer cannot be read ==\n")
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (dir (wdir s local))
       (before (end-of s local)))
  (chmod! "000" dir)
  (let ((w (caught (ask s 'insert "--under" "root" "--title" "B"))))
    (chmod! "700" dir)
    (want "U2 the write names writer-unreadable, and the local writer's end is unchanged"
          (list (if (contains? (format "~s" w) "writer-unreadable")
                    'names-writer-unreadable
                    (list 'answered (if (pair? w) (list (car w) (and (pair? (cdr w)) (cadr w))) w)))
                (= before (end-of s local)))
          '(names-writer-unreadable #t))))

(printf "== U2b: permission lost after the session opened ==\n")
;; CONTROL FIRST: the same session, the same append, no permission change --
;; so that a refusal below is about the permission and not about how this
;; file drives a session.
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (sess (log-begin s (lambda args 'applied)))
       (v (session-view sess))
       (a (caught (and v (session-append! sess (make-frame (view-revision v) (view-epoch v)
                                                         (view-writer v) (view-expect-seq v)
                                                         "agent:test" '() '(put "u2b.1" ())))))))
  (caught (log-end! sess))
  (want "CONTROL U2b's session append commits when nothing is unreadable"
        (and (pair? a) (car a))
        'committed))
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (dir (wdir s local))
       (before (end-of s local))
       (sess (log-begin s (lambda args 'applied)))
       (v (session-view sess)))
  (chmod! "000" dir)
  (let ((a (caught (and v (session-append! sess (make-frame (view-revision v) (view-epoch v)
                                                          (view-writer v) (view-expect-seq v)
                                                          "agent:test" '() '(put "u2b.1" ())))))))
    (caught (log-end! sess))
    (chmod! "700" dir)
    (want "U2b the append is refused before reserve, naming the writer's directory"
          (if (and (pair? a) (eq? (car a) 'refused-before-reserve))
              (list (car a) (cadr a)
                    (let ((pth (assq 'path (cddr a)))) (and pth (equal? (cadr pth) dir))))
              a)
          '(refused-before-reserve metadata-unreadable #t))
    (want "U2b after restoring the permission, the next write lands at the original end + 1"
          (begin (ask s 'insert "--under" "root" "--title" "After")
                 (- (end-of s local) before))
          1)))

(printf "== U2c: a readable local writer beside an unreadable mirror ==\n")
;; CONTROL: a tracked write needs its cursor (without one it is refused as
;; bad-request req-without-cursor); on a store with nothing unreadable it
;; commits, so `unknown` below is about the unreadable mirror.
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (end (end-of s local))
       (a (ask s 'insert "--under" "root" "--title" "Tracked" "--req" "R-ctl"
               "--cursor" (string-append local ":" (number->string end)))))
  (want "CONTROL a tracked write with its cursor commits on a healthy store"
        (list (rpc-ok? a) (- (end-of s local) end))
        '(#t 1)))
(for-each
  (lambda (mode)
    (let* ((s (fresh-store!))
           (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
           (_ (publish-mirror! s))
           (mdir (wdir s M))
           (before (end-of s local)))
      (chmod! mode mdir)
      (let* ((seen (caught (origin-and-notes s M)))
             (untracked (caught (ask s 'insert "--under" "root" "--title" "Untracked")))
             (after-untracked (caught (end-of s local)))
             (tracked (caught (ask s 'insert "--under" "root" "--title" "Tracked" "--req" "R-u2c"
                                   "--cursor" (string-append local ":" (number->string after-untracked)))))
             (after-tracked (caught (end-of s local))))
        (chmod! "700" mdir)
        (want (format "U2c mode ~a: mirror reported unreadable; untracked write commits; tracked write is unknown and commits nothing" mode)
              (list (if (and (pair? seen) (not (eq? (car seen) 'RAISED))) (car seen) seen)
                    (if (rpc-ok? untracked) 'committed (list 'answered (if (pair? untracked) (car untracked) untracked)))
                    (and (number? after-untracked) (- after-untracked before))
                    (if (contains? (format "~s" tracked) "unknown") 'unknown
                        (list 'answered (if (pair? tracked) (car tracked) tracked)))
                    (and (number? after-tracked) (number? after-untracked) (- after-tracked after-untracked)))
              '(unreadable committed 1 unknown 0)))))
  '("000" "400" "100"))

(printf "== U9: publish's candidate path ==\n")
(let* ((s (fresh-store!))
       (outside (string-append root "/candidates"))
       (locked (string-append outside "/locked"))
       (file (string-append outside "/plain")))
  (system (string-append "mkdir -p " locked))
  (call-with-port (open-file-output-port (string-append locked "/seg") (file-options no-fail))
    (lambda (p) (put-bytevector p (encode-record 1 1757300000001 "a" '() '(set "t" x "x")))))
  (call-with-port (open-file-output-port file (file-options no-fail))
    (lambda (p) (put-bytevector p (string->utf8 "x"))))
  (chmod! "000" locked)
  (let ((unreadable (caught (ask s 'publish "mirrorz8" "1" (string-append locked "/seg")))))
    (chmod! "700" locked)
    (want "U9 a candidate under a directory this process cannot search is candidate-unreadable"
          (if (contains? (format "~s" unreadable) "candidate-unreadable") 'candidate-unreadable
              (list 'answered (if (pair? unreadable) (list (car unreadable) (and (pair? (cdr unreadable)) (cadr unreadable))) unreadable)))
          'candidate-unreadable))
  (want "U9 CONTROL a missing candidate is no-candidate, as today"
        (let ((a (ask s 'publish "mirrorz8" "1" (string-append outside "/absent"))))
          (contains? (format "~s" a) "no-candidate"))
        #t)
  (want "U9 CONTROL a candidate path through a regular file (ENOTDIR) is no-candidate, as today"
        (let ((a (ask s 'publish "mirrorz8" "1" (string-append file "/seg"))))
          (contains? (format "~s" a) "no-candidate"))
        #t))

(printf "== U1c: a present metadata file this store cannot read ==\n")
;; Each row asks the complete discovery result: the origin, the one note and
;; the file it names, and that a coordinate raises. The manifest row reads
;; `mirrored` with a note today (the existing unreadable-manifest row); the
;; others read as if the file were not there.
(define (complete-reading store w file)
  (let ((p (caught (discover-prefix store w #f))))
    (if (and (pair? p) (eq? (car p) 'RAISED))
        p
        (list (discovery-origin p)
              (map (lambda (e) (list (log-error-kind e)
                                     (let ((a (assq 'path (log-error-detail e))))
                                       (and a (equal? (cdr a) file) 'that-file))))
                   (discovery-integrity p))
              (car (coordinate-answer p discovery-end-seq))))))
(let* ((s (fresh-store!))
       (_ (publish-mirror! s))
       (file (string-append (wdir s M) "/published.sexp")))
  (chmod! "000" file)
  (let ((got (complete-reading s M file)))
    (chmod! "600" file)
    (want "U1c a manifest-only mirror whose published.sexp cannot be read"
          got
          '(unreadable ((metadata-unreadable that-file)) unreadable-entry))))
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (file (string-append (wdir s local) "/owner.sexp")))
  (chmod! "000" file)
  (let ((got (complete-reading s local file)))
    (chmod! "600" file)
    (want "U1c an owner-bearing local writer whose owner.sexp cannot be read"
          got
          '(unreadable ((metadata-unreadable that-file)) unreadable-entry))))
(let* ((s (fresh-store!))
       (_ (publish-mirror! s))
       (other (encode-record 1 1757300000001 "a" '() '(set "t" x "other")))
       (__ (log-publish! s M 1 other (segment-sha other)))
       (file (string-append (wdir s M) "/quarantine.sexp")))
  (want "CONTROL the diverging candidate wrote a quarantine marker"
        (file-exists? file)
        #t)
  (chmod! "000" file)
  (let ((got (complete-reading s M file)))
    (chmod! "600" file)
    (want "U1c a mirror whose quarantine.sexp cannot be read"
          got
          '(unreadable ((metadata-unreadable that-file)) unreadable-entry))))

(printf "== U7, U7c: uncertainty for an unreadable writer ==\n")
(let* ((s (fresh-store!))
       (_ (publish-mirror! s))
       (dir (wdir s M)))
  (chmod! "000" dir)
  (let ((origin (caught (discovery-origin (discover-prefix s M #f))))
        (derived (caught (uncertain-derived s M)))
        (loaded (caught (uncertain-load s M))))
    (chmod! "700" dir)
    (want "U7 in the same run discovery says unreadable and uncertain-derived holds (w 0 #f) without raising"
          (list origin
                (and (list? derived) (member (list M 0 #f) derived) #t))
          '(unreadable #t))
    (want "U7c the cache of an unreadable writer, absent or not, reads as unreadable, not absent"
          (if (and (pair? loaded) (pair? (cdr loaded))) (cadr loaded) loaded)
          '(uncertain-cache unreadable))))

(printf "== U4a: the store's snapshot records an unreadable writer ==\n")
(let* ((s (fresh-store!))
       (_ (publish-mirror! s))
       (dir (wdir s M))
       (entry-of (lambda (snap)
                   (let loop ((ws (store-writers s)) (es (cdr snap)))
                     (cond ((or (null? ws) (null? es)) #f)
                           ((equal? (car ws) M) (car es))
                           (else (loop (cdr ws) (cdr es)))))))
       (healthy (caught (entry-of (store-state-snapshot s)))))
  (want "CONTROL the healthy entry has the five parts log.sc builds"
        (and (list? healthy) (length healthy))
        5)
  (chmod! "000" dir)
  (let ((a (caught (store-state-snapshot s)))
        (b (caught (store-state-snapshot s))))
    (chmod! "700" dir)
    (want "U4a during the failure the entry is (unreadable <dir> <reason>), and two snapshots are equal"
          (if (and (pair? a) (eq? (car a) 'RAISED))
              a
              (let ((e (entry-of a)))
                (list (and (pair? e) (car e))
                      (and (pair? e) (pair? (cdr e)) (equal? (cadr e) dir))
                      (equal? a b))))
          '(unreadable #t #t))
    (want "U4a after restoration the entry has the healthy shape again"
          (let ((e (caught (entry-of (store-state-snapshot s)))))
            (and (list? e) (length e)))
          5)))

(printf "== U4g: the resident reuse fingerprint ==\n")
;; Reuse compares load-fingerprint (store.sc replay), which reads discovery
;; coordinates; under R2d those raise on an unreadable writer, so the
;; fingerprint must say `unreadable` for it instead -- and differ from the
;; healthy one, so nothing built while the writer was readable is reused.
(define (fingerprint store)
  (let ((ls (log-open store)))
    (let ((f (load-fingerprint ls)))
      (load-abort! ls 'probe)
      f)))
(define (mentions-unreadable-dir? f dir)
  (let ((text (format "~s" f)))
    (and (contains? text "unreadable") (contains? text dir))))
(for-each
  (lambda (mode)
    (let* ((s (fresh-store!))
           (_ (publish-mirror! s))
           (dir (wdir s M))
           (healthy (caught (fingerprint s))))
      (chmod! mode dir)
      (let ((damaged (caught (fingerprint s))))
        (chmod! "700" dir)
        (want (format "U4g mode ~a: the fingerprint is a value, differs from the healthy one, and names the unreadable directory" mode)
              (if (and (pair? damaged) (eq? (car damaged) 'RAISED))
                  damaged
                  (list (and damaged #t)
                        (not (equal? damaged healthy))
                        (mentions-unreadable-dir? damaged dir)))
              '(#t #t #t)))))
  '("000" "100"))

(printf "== P: the primitive, in process ==\n")
;; Names looked up at run time in (theourgia ffi), so this file still loads
;; on a tree without them.
(define (ffi-name name) (guard (e (#t #f)) (eval name (environment '(theourgia ffi)))))
(define entry-type (ffi-name 'entry-type))
(define read-entry (ffi-name 'read-entry))
(define list-entries (ffi-name 'list-entries))
(define unreadable-entry-errno (ffi-name 'unreadable-entry-errno))
(define (typed thunk)
  (guard (e ((and unreadable-entry? (unreadable-entry? e))
             (list 'unreadable-entry (unreadable-entry-path e)
                   (and unreadable-entry-errno (unreadable-entry-errno e))))
            (#t (list 'other (if (message-condition? e) (condition-message e) e))))
    (thunk)))
(let* ((d (string-append root "/prim"))
       (file (string-append d "/f"))
       (sub (string-append d "/locked"))
       (loop-a (string-append d "/la")))
  (system (string-append "mkdir -p " sub "; printf x > " file "; ln -s la " loop-a))
  (want "P ENOENT answers absent, for type, read and listing"
        (if (and entry-type read-entry list-entries)
            (list (entry-type (string-append d "/none"))
                  (read-entry (string-append d "/none"))
                  (list-entries (string-append d "/none")))
            'no-primitive)
        '(absent absent absent))
  (want "P ENOTDIR (a path through a regular file) answers absent"
        (if (and entry-type read-entry list-entries)
            (list (entry-type (string-append file "/x"))
                  (read-entry (string-append file "/x"))
                  (list-entries (string-append file "/x")))
            'no-primitive)
        '(absent absent absent))
  (want "P a regular file and a directory answer their type"
        (if entry-type (list (entry-type file) (entry-type d)) 'no-primitive)
        '(regular directory))
  (chmod! "000" sub)
  (want "P listing a directory at 000 raises unreadable-entry with its path and EACCES"
        (if list-entries (typed (lambda () (list-entries sub))) 'no-primitive)
        (list 'unreadable-entry sub 'EACCES))
  (want "P a type question below a directory at 000 raises unreadable-entry with EACCES"
        (if entry-type (typed (lambda () (entry-type (string-append sub "/x")))) 'no-primitive)
        (list 'unreadable-entry (string-append sub "/x") 'EACCES))
  (chmod! "700" sub)
  (want "P a symlink loop raises unreadable-entry with ELOOP"
        (if entry-type (typed (lambda () (entry-type (string-append loop-a "/x")))) 'no-primitive)
        (list 'unreadable-entry (string-append loop-a "/x") 'ELOOP)))

(printf "== P: the two fault kinds and stat-fail's errno, in a child process ==\n")
;; A fault is armed from THEOURGIA_FAULT when ffi loads, so each row runs a
;; child with it set, in the `commit` stage, and reads back one datum.
(define child (string-append root "/p-child.ss"))
(define child-forms
  ;; WRITTEN AS DATA, NOT AS A STRING: a program kept in a string literal put
  ;; `(define` at column 0 inside the literal, and the suite's structure
  ;; check counts every line-start define as a top-level one (F84).
  '((import (chezscheme) (theourgia ffi))
    (define op (cadr (command-line)))
    (define target (caddr (command-line)))
    (define (typed thunk)
      (guard (e ((unreadable-entry? e) (list 'unreadable-entry (unreadable-entry-errno e)))
                (#t (list 'other (if (message-condition? e) (condition-message e) e))))
        (thunk)))
    (write (parameterize ((theourgia-stage 'commit))
             (typed (lambda ()
                      (cond ((string=? op "read") (list 'bytes (bytevector-length (read-entry target))))
                            ((string=? op "list") (list 'names (length (list-entries target))))
                            (else (list 'type (entry-type target))))))))
    (newline)))
(call-with-port (open-file-output-port child (file-options no-fail) (buffer-mode block) (native-transcoder))
  (lambda (p) (for-each (lambda (f) (write f p) (newline p)) child-forms)))
(define (run-child fault op target)
  ;; STDERR APART: a child with a non-default THEOURGIA_HOME announces it on
  ;; stderr at start-up, and read would take that announcement as the answer.
  (let* ((out (string-append root "/p-child.out"))
         (err (string-append root "/p-child.err"))
         ;; INJECTION EXISTS ONLY IN A BUILD EXPANDED WITH THEOURGIA_INJECT=on
         ;; (ffi.sc); run from source, expansion is at load, so the child
         ;; needs both variables.
         (cmd (string-append (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT='" fault "' ") "")
                             "scheme --script " child " " op " " target " > " out " 2> " err)))
    (system cmd)
    (let ((d (guard (e (#t (eof-object))) (call-with-port (open-input-file out) read))))
      (if (eof-object? d)
          (list 'no-answer (let ((t (call-with-port (open-input-file err) get-string-all)))
                             (if (eof-object? t) "" (substring t (max 0 (- (string-length t) 200)) (string-length t)))))
          d))))
(let* ((d (string-append root "/pfault"))
       (big (string-append d "/big"))
       (many (string-append d "/many")))
  (system (string-append "mkdir -p " many
                         "; head -c 200000 /dev/zero > " big
                         "; for i in 1 2 3 4 5 6 7 8; do touch " many "/e$i; done"))
  (want "CONTROL (needs the primitive) P without a fault the child reads 200000 bytes and lists 8 names"
        (list (run-child #f "read" big) (run-child #f "list" many))
        '((bytes 200000) (names 8)))
  (want "P read-fail-after raises unreadable-entry with the injected errno, not a partial buffer"
        (run-child "read-fail-after@commit:file=big:errno=EIO" "read" big)
        '(unreadable-entry EIO))
  (want "P readdir-fail-after raises unreadable-entry with the injected errno, not a shorter list"
        (run-child "readdir-fail-after@commit:file=many:errno=EIO" "list" many)
        '(unreadable-entry EIO))
  (want "P the type operation with stat-fail:errno=EOVERFLOW raises unreadable-entry EOVERFLOW"
        (run-child "stat-fail@commit:file=big:errno=EOVERFLOW" "type" big)
        '(unreadable-entry EOVERFLOW))
  (want "P the type operation with stat-fail (default EIO) raises unreadable-entry EIO"
        (run-child "stat-fail@commit:file=big" "type" big)
        '(unreadable-entry EIO)))

(printf "== U4f: snapshot selection with an unreadable writer ==\n")
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A")
                     (ask s 'insert "--under" "root" "--title" "B")
                     (local-writer s)))
       (_ (publish-mirror! s))
       (snap (ask s 'snapshot))
       (selected (caught (let ((ls (log-open s)))
                           (let ((c (load-snapshot-cut ls))) (load-abort! ls 'probe) (and c #t)))))
       (dir (wdir s M)))
  (want "CONTROL on the healthy store the snapshot is taken and then selected"
        (list (rpc-ok? snap) selected)
        '(#t #t))
  (chmod! "000" dir)
  (let ((r (caught (let ((ls (log-open s)))
                     (let ((c (load-snapshot-cut ls))
                           (origin (discovery-origin (load-prefix ls M))))
                       (load-abort! ls 'probe)
                       (list origin (and c #t)))))))
    (chmod! "700" dir)
    (want "U4f with the mirror unreadable: log-open does not raise, the origin is unreadable, no snapshot baseline is selected"
          r
          '(unreadable #f))))

(printf "== K10: an answer built from an incomplete reduction says so ==\n")
;; Every rpc answer whose load had an unreadable writer carries
;; (incomplete (unreadable (writer w) (path p) (reason r)) ...), ok answers
;; and refusals alike; a healthy store's answers carry none.
(define (incomplete-dirs answer)
  (let ((c (and (pair? answer) (list? answer)
                (find (lambda (x) (and (pair? x) (eq? (car x) 'incomplete))) (cdr answer)))))
    (if (not c)
        'no-clause
        (map (lambda (u) (let ((pth (and (pair? u) (assq 'path (cdr u))))) (and pth (cadr pth))))
             (cdr c)))))
(define resident-cache! (guard (e (#t #f)) (eval 'store-resident-cache! (environment '(theourgia store)))))
(let* ((s (fresh-store!))
       (_ (begin (ask s 'insert "--under" "root" "--title" "A") (publish-mirror! s))))
  (want "CONTROL K10 on a healthy store an answer carries no incomplete clause"
        (incomplete-dirs (ask s 'outline))
        'no-clause)
  (let ((dir (wdir s M)))
    (chmod! "000" dir)
    (let* ((off (caught (ask s 'outline)))
           (on (caught (begin (when resident-cache! (resident-cache! #t))
                              (let ((a (ask s 'outline)) (b (ask s 'outline)))
                                (when resident-cache! (resident-cache! #f))
                                (list a b)))))
           (human (caught (let ((render (guard (e (#t #f)) (eval 'render-human (environment '(theourgia render))))))
                            (and render (render off))))))
      (chmod! "700" dir)
      (want "K10 cache off, a mirror at 000: outline answers ok and names the mirror's directory as incomplete"
            (list (and (pair? off) (car off)) (incomplete-dirs off))
            (list 'ok (list dir)))
      (want "K10 cache on, the same, asked twice (a reduction with an unreadable writer is never made resident, so both are replays)"
            (if (and (pair? on) (eq? (car on) 'RAISED))
                on
                (map (lambda (a) (list (and (pair? a) (car a)) (incomplete-dirs a))) on))
            (list (list 'ok (list dir)) (list 'ok (list dir))))
      (want "U4d the human rendering of that answer prints the incomplete clause, naming the directory"
            (and (string? human) (contains? human "incomplete") (contains? human dir))
            #t))))
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (dir (wdir s local))
       (id (let ((a (ask s 'insert "--under" "root" "--title" "B")))
             (let ((ev (and (pair? a) (assq 'state (cdr a))))) (and ev (car (car (cadr ev))))))))
  (chmod! "100" dir)
  (let ((outline (caught (ask s 'outline)))
        (read (caught (ask s 'read id))))
    (chmod! "700" dir)
    (want "K10 the local writer's directory at --x: outline and read each name it as incomplete, whatever their head"
          (list (incomplete-dirs outline) (incomplete-dirs read))
          (list (list dir) (list dir)))))

(printf "== K11: a segment this store cannot read ==\n")
;; FIRST, THAT THE DAMAGE HAPPENED: the file exists and is now unreadable
;; to this process. A chmod that failed quietly would leave every row below
;; reading a healthy store.
(define (unreadable-now? path)
  (and (file-exists? path)
       (guard (e (#t #t)) (call-with-port (open-file-input-port path) (lambda (p) #f)))))
(for-each
  (lambda (which)
    (let* ((s (fresh-store!))
           (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
           (_ (publish-mirror! s))
           (w (if (eq? which 'local) local M))
           (seg (string-append (wdir s w) "/" (segment-file-name 1))))
      (chmod! "000" seg)
      (let* ((damaged (unreadable-now? seg))
             (got (caught (origin-and-notes s w)))
             (outline (caught (ask s 'outline))))
        (chmod! "600" seg)
        (want (format "CONTROL K11 ~a: the segment exists and this process cannot open it" which)
              damaged
              #t)
        ;; K11 (revised): the writer stops where it stands, as an integrity
        ;; stop already does -- the origin does not change -- and the note
        ;; it already had now names the segment.
        (want (format "K11 ~a segment at 000: origin unchanged, one segment-unreadable note naming the segment" which)
              (if (or (not (pair? got)) (eq? (car got) 'RAISED))
                  got
                  (list (car got) (map (lambda (k+p) (list (car k+p) (equal? (cadr k+p) seg))) (cadr got))))
              (list (if (eq? which 'local) 'local 'mirrored) '((segment-unreadable #t))))
        (want (format "K11 ~a segment at 000: outline is answered, not a catch-all failure" which)
              (and (pair? outline) (car outline))
              'ok)
        (want (format "K11 ~a segment at 000: outline carries the incomplete clause naming the segment" which)
              (incomplete-dirs outline)
              (list seg)))))
  '(local mirror))

(printf "== K12: the local writer's current segment cannot be read ==\n")
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (seg (string-append (wdir s local) "/" (segment-file-name 1)))
       (before (end-of s local)))
  (chmod! "000" seg)
  (let ((damaged (unreadable-now? seg))
        (w (caught (ask s 'insert "--under" "root" "--title" "Past the hole"))))
    (chmod! "600" seg)
    (want "CONTROL K12 the local segment exists and this process cannot open it"
          damaged
          #t)
    (want "K12 a write names writer-unreadable and names the segment; after restoring, the local end is unchanged"
          (list (if (and (contains? (format "~s" w) "writer-unreadable") (contains? (format "~s" w) seg))
                    'names-writer-unreadable-and-the-segment
                    (list 'answered (if (pair? w) (list (car w) (and (pair? (cdr w)) (cadr w))) w)))
                (= before (end-of s local)))
          '(names-writer-unreadable-and-the-segment #t))))

(printf "== K8 client: a run root under a directory this process cannot search ==\n")
;; R1 converts mkdir-p! unconditionally; a level under an unsearchable
;; directory then raises unreadable-entry, and the client's start-one! must
;; answer it, as it answers every other failure to start, not let it out of
;; ensure-daemon! (the code session's reading: 89124d4 answered
;; (error serve-start-failed (spawn #f)); F77a before its fix raised).
(let* ((ensure-daemon! (guard (e (#t #f)) (eval 'ensure-daemon! (environment '(theourgia client)))))
       (d (string-append root "/client"))
       (fixture-run (getenv "THEOURGIA_RUN")))
  (system (string-append "mkdir -p " d "/run " d "/store"))
  (putenv "THEOURGIA_RUN" (string-append d "/run/inner"))
  (chmod! "000" (string-append d "/run"))
  (let ((a (caught (and ensure-daemon!
                        (ensure-daemon! '("/usr/bin/true") (string-append d "/store")
                                        (string-append sock-base "/uw-" (number->string (get-process-id)) ".sock"))))))
    (chmod! "700" (string-append d "/run"))
    (putenv "THEOURGIA_RUN" fixture-run)
    (want "K8 client: ensure-daemon! answers serve-start-failed naming the unreadable path, and does not raise"
          (if (and (pair? a) (not (eq? (car a) 'RAISED)))
              (list (car a) (and (pair? (cdr a)) (cadr a))
                    (and (contains? (format "~s" a) "unreadable") (contains? (format "~s" a) (string-append d "/run"))))
              a)
          '(error serve-start-failed #t))))

(printf "== K8 client, serve.log: an existing log this process cannot open ==\n")
;; start-one! reads the log's length BEFORE its guard (client.sc:491-493), and
;; under R1 file-size on a log this process cannot open raises
;; unreadable-entry, so on r3 the condition leaves ensure-daemon! (codex r2 B1,
;; class I). The fixed code answers the same serve-start-failed (unreadable
;; (path ...) (reason ...)) as the run-directory case.
(let* ((ensure-daemon! (guard (e (#t #f)) (eval 'ensure-daemon! (environment '(theourgia client)))))
       (serve-log-path (guard (e (#t #f)) (eval 'serve-log-path (environment '(theourgia client)))))
       (d (string-append root "/client-log"))
       (store (string-append d "/store"))
       (fixture-run (getenv "THEOURGIA_RUN")))
  (system (string-append "mkdir -p " store))
  (putenv "THEOURGIA_RUN" (string-append d "/run"))
  (let* ((lp (and serve-log-path (serve-log-path store)))
         (made (and lp (= 0 (system (string-append "mkdir -p \"$(dirname " lp ")\" && : > " lp " && chmod 000 " lp)))))
         (damaged (and made (file-exists? lp) (guard (e (#t #t)) (call-with-port (open-input-file lp) get-char) #f)))
         ;; THE ARGV LEAVES A MARK IF IT IS EVER STARTED, so "nothing was
         ;; spawned" is read from the file system and not inferred.
         (mark (string-append d "/spawned"))
         (a (caught (and ensure-daemon! lp
                         (ensure-daemon! (list "/bin/sh" "-c" (string-append "touch " mark)) store
                                         (string-append sock-base "/uwl-" (number->string (get-process-id)) ".sock"))))))
    (when lp (chmod! "600" lp))
    (putenv "THEOURGIA_RUN" fixture-run)
    (want "CONTROL K8 serve.log: the log exists and this process cannot open it"
          damaged
          #t)
    (want "K8 client: an existing serve.log this process cannot open answers serve-start-failed naming it with a reason, spawns nothing, and does not raise"
          (if (and (pair? a) (not (eq? (car a) 'RAISED)))
              (let* ((u (and (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'unreadable))) (cdr a))))
                     (path (and u (list? u) (find (lambda (x) (and (pair? x) (eq? (car x) 'path))) (cdr u))))
                     (reason (and u (list? u) (find (lambda (x) (and (pair? x) (eq? (car x) 'reason))) (cdr u)))))
                (list (car a) (and (pair? (cdr a)) (cadr a))
                      (and path (pair? (cdr path)) (equal? (cadr path) lp))
                      (and reason (pair? (cdr reason)) (string? (cadr reason)) (> (string-length (cadr reason)) 0))
                      (if (file-exists? mark) 'spawned 'nothing-spawned)))
              (if (pair? a) (list (car a)) a))
          '(error serve-start-failed #t #t nothing-spawned))))

(printf "== K14: the eval route says what its view could not see ==\n")
;; core.sc eval runs through eval-supervise and a worker, not rpc dispatch;
;; it is driven the way test/eval-local.sc drives it, with stdout read as
;; the answer and stderr kept apart.
(define (eval-answer store source . flags)
  (let ((out (string-append root "/eval.out"))
        (err (string-append root "/eval.err")))
    (system (string-append "THEOURGIA_LOCAL=1 scheme --script ../core.sc eval '" source "' --store " store
                           " --wire " (apply string-append (map (lambda (f) (string-append f " ")) flags))
                           "> " out " 2> " err))
    (let ((d (guard (e (#t (eof-object))) (call-with-port (open-input-file out) read))))
      (if (eof-object? d)
          (list 'no-answer (let ((t (call-with-port (open-input-file err) get-string-all)))
                             (if (eof-object? t) "" (substring t (max 0 (- (string-length t) 200)) (string-length t)))))
          d))))
(let* ((s (fresh-store!))
       (local (begin (ask s 'insert "--under" "root" "--title" "A") (local-writer s)))
       (dir (wdir s local)))
  (want "CONTROL K14 on a healthy store eval answers ok with no incomplete clause"
        (let ((a (eval-answer s "(+ 1 2)")))
          (if (and (pair? a) (eq? (car a) 'ok))
              (list 'ok (incomplete-dirs a))
              a))
        '(ok no-clause))
  (chmod! "100" dir)
  (let ((a (eval-answer s "(+ 1 2)")))
    (chmod! "700" dir)
    (want "K14 with the local writer's directory at --x, eval's answer names it as incomplete, whatever its head"
          (if (and (pair? a) (eq? (car a) 'error) (eq? (incomplete-dirs a) 'no-clause))
              a
              (incomplete-dirs a))
          (list dir)))
  ;; codex r2 A1 (class S): the worker passes a raised pair headed error
  ;; through as the answer (eval-worker.sc:189), and appends the incomplete
  ;; clause only to a proper list (:235); an improper pair from the evaluated
  ;; source therefore lost the clause. The source below spells the pair
  ;; without quote characters because it travels through a shell.
  (let ((improper "(raise (cons (string->symbol \"error\") (string->symbol \"custom\")))"))
    (want "CONTROL K14 a source that raises an improper error pair: on a healthy store the answer is that pair, as on a839eb1"
          (eval-answer s improper)
          '(error . custom))
    (chmod! "100" dir)
    (let ((a (eval-answer s improper)))
      (chmod! "700" dir)
      (want "K14 the same source with the local writer's directory at --x: the answer still names it as incomplete"
            (if (and (pair? a) (eq? (incomplete-dirs a) 'no-clause))
                a
                (incomplete-dirs a))
            (list dir))))
  ;; codex r4 A1-1 (class S): the supervisor's own answers -- a time, memory
  ;; or output limit, or a worker that died -- carried no clause, because the
  ;; notes lived in the worker and were said only inside its final answer.
  ;; Under the ruling (shape a) the worker says its notes as it hears them
  ;; and the supervisor appends the last to every answer it returns.
  (let ((loop-source "(let loop () (loop))"))
    ;; THE RESOURCE IS READ BY FIELD: a memory or output limit is also
    ;; eval-limit, and a row that read only the head would pass on it.
    (define (limit-resource a)
      (let ((r (and (pair? a) (list? a)
                    (find (lambda (x) (and (pair? x) (eq? (car x) 'resource))) (cdr a)))))
        (and r (pair? (cdr r)) (cadr r))))
    (want "CONTROL K14 a source that loops, under a 500 ms timeout, on a healthy store answers eval-limit time with no clause"
          (let ((a (eval-answer s loop-source "--timeout-ms" "500")))
            (list (and (pair? a) (car a)) (and (pair? a) (list? a) (pair? (cdr a)) (cadr a))
                  (limit-resource a) (incomplete-dirs a)))
          '(error eval-limit time no-clause))
    (chmod! "100" dir)
    (let ((a (eval-answer s loop-source "--timeout-ms" "500")))
      (chmod! "700" dir)
      (want "K14 the same looping source with the local writer's directory at --x: the eval-limit time answer names it as incomplete"
            (list (and (pair? a) (list? a) (pair? (cdr a)) (cadr a)) (limit-resource a)
                  (if (eq? (incomplete-dirs a) 'no-clause) a (incomplete-dirs a)))
            (list 'eval-limit 'time (list dir))))))

(system (string-append "chmod -R u+rwx " root " 2>/dev/null; rm -rf " root))
(printf "\n~a failures\nrows: ~a\nunreadable-writer complete\n" bad rows)
