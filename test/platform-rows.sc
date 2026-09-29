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

;; THE PLATFORM NUMBERS: THE ROWS, THE SELECTION, AND THE CODE THAT READS THEM.
;;
;; (theourgia platform-numbers) holds, per platform, what test/probe/layout.c
;; printed there, and the FFI takes every flag, errno, size and offset from
;; the running platform's row. These rows read the rows against their
;; archived readings, the selection against the machine, the refusal of an
;; unlisted platform in a child, the probe compiled here against this
;; platform's row, the code for any number written down again, and the
;; append bit through the log's own sequence of calls.
;;
;; NOTE: A FOREIGN ROW IS READ AS BYTES, NEVER HANDED TO THIS KERNEL. The
;; Linux and FreeBSD rows are reached in a fresh child through the test
;; seam THEOURGIA_PLATFORM_KEY, honoured only in a build expanded with
;; THEOURGIA_INJECT=on; the child prints what it built and the parent
;; compares.

(import (chezscheme) (theourgia platform-numbers) (theourgia ffi))

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

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/platformnumbers-" pid-text))

(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (sh . xs) (system (apply string-append xs)))
(define (sh-out . xs)
  (let* ((p (process (apply string-append xs))) (s (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? s) "" s)))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (eof-object? t) "" t))
      ""))
(define (data-of-text text)
  (guard (e (#t 'UNREADABLE))
    (let ((p (open-string-input-port text)))
      (let loop ((out '()))
        (let ((d (read p)))
          (if (eof-object? d) (reverse out) (loop (cons d out))))))))
(define (data-of-file path) (data-of-text (file-text path)))
(define (trim-newline s)
  (if (and (> (string-length s) 0) (char=? (string-ref s (- (string-length s) 1)) #\newline))
      (substring s 0 (- (string-length s) 1))
      s))
(define scheme-path (trim-newline (sh-out "command -v scheme")))

(sh "rm -rf " (quoted here) " && mkdir -p " (quoted here))

;; A CHILD: a script run from source with `env` in front, -> (rc stdout
;; stderr), stdout and stderr as text.
(define child-n 0)
(define (run-script env script)
  (set! child-n (+ child-n 1))
  (let* ((base (string-append here "/run-" (number->string child-n)))
         (rc (sh "env " env " " (quoted scheme-path) " --script " (quoted script)
                 " > " (quoted (string-append base ".out"))
                 " 2> " (quoted (string-append base ".err")) " < /dev/null")))
    (list (if (> rc 255) (quotient rc 256) rc)
          (file-text (string-append base ".out"))
          (file-text (string-append base ".err")))))
(define (child env text)
  (let ((script (string-append here "/child-" (number->string (+ child-n 1)) ".sc")))
    (call-with-output-file script (lambda (p) (put-string p text)))
    (run-script env script)))
(define forced "THEOURGIA_INJECT=on THEOURGIA_PLATFORM_KEY=")
;; The error datums a child wrote on stderr. A child that expands ffi.sc
;; with THEOURGIA_INJECT=on writes ffi.sc's banner there first; it is not
;; a datum of the refusal and is left out.
(define (errors-of text)
  (let ((d (data-of-text text)))
    (if (list? d) (filter (lambda (x) (and (pair? x) (eq? (car x) 'error))) d) d)))

;; The readings, as archived: the probe's output on each platform.
(define reading-files
  '("probe/readings/linux-ubuntu-latest.sexp" "probe/readings/linux-ubuntu-2404-arm.sexp"
    "probe/readings/freebsd-15-paris.sexp" "probe/readings/darwin-arm64-local.sexp"))

;; ---- P10: each row is its archived reading, every datum ----------------------

(want "PN-P10 the four rows are the four archived readings, datum for datum, in order"
      (map (lambda (f r) (equal? (data-of-file f) r)) reading-files (platform-readings))
      '(#t #t #t #t))
(want "PN-P10 the four rows' keys: Linux x86_64 and aarch64 with glibc, FreeBSD amd64, Darwin arm64"
      (map reading-key (platform-readings))
      '(("Linux" "x86_64" "glibc") ("Linux" "aarch64" "glibc") ("FreeBSD" "amd64" #f) ("Darwin" "arm64" #f)))

;; ---- P2: the selection on this machine -----------------------------------------
;;
;; A SECOND READING OF THE KEY, written here and not taken from the library:
;; the machine type's system and architecture, and on Linux the C library
;; named in this process's maps. The library's key must be the same.
(define (independent-key)
  (let* ((m (symbol->string (machine-type)))
         (has (lambda (s) (let ((n (string-length s)))
                            (let loop ((i 0))
                              (and (<= (+ i n) (string-length m))
                                   (or (string=? (substring m i (+ i n)) s) (loop (+ i 1))))))))
         (arch (cond ((has "arm64") "arm64") ((has "a6") "x86_64") (else m)))
         (system (cond ((has "osx") "Darwin") ((has "le") "Linux") ((has "fb") "FreeBSD") (else "unknown"))))
    (cond
      ((string=? system "Linux")
       (let ((maps (file-text "/proc/self/maps")))
         (list system (if (string=? arch "arm64") "aarch64" arch)
               (let ((in (lambda (s) (let ((n (string-length s)))
                                       (let loop ((i 0))
                                         (and (<= (+ i n) (string-length maps))
                                              (or (string=? (substring maps i (+ i n)) s) (loop (+ i 1)))))))))
                 (cond ((or (in "/libc.so.6") (in "/ld-linux-")) "glibc")
                       ((or (in "/ld-musl-") (in "/libc.musl-")) "musl")
                       (else "unknown"))))))
      ((string=? system "FreeBSD") (list system (if (string=? arch "x86_64") "amd64" arch) #f))
      (else (list system arch #f)))))
(want "PN-P2 the selected key is the one the machine type (and, on Linux, the mapped C library) names"
      (platform-key)
      (independent-key))
(want "PN-P2 the selected row is the one whose own lines name that key"
      (reading-key (platform-row))
      (platform-key))
;; THE NO-CHANGE CONTROL on this development machine (Darwin arm64): the row
;; gives st_mode where the code always read it. On any other platform the
;; row says which key it is and the pair it asserts is that row's own.
(printf "PN-P2 information: this platform is ~s\n" (platform-key))
(want "PN-P2 st_mode's offset and width: 4 and 2 on Darwin arm64 (the no-change control), else the row's"
      (list (platform-field 'stat 'st_mode 'offset) (platform-field 'stat 'st_mode 'size))
      (if (equal? (platform-key) '("Darwin" "arm64" #f))
          '(4 2)
          (list (platform-field 'stat 'st_mode 'offset) (platform-field 'stat 'st_mode 'size))))
(want "PN-P2 through that offset, a directory is a directory and this fixture is a regular file"
      (list (entry-type here) (entry-type "platform-rows.sc"))
      '(directory regular))

;; ---- L: the C library, from the text of /proc/self/maps ---------------------
;;
;; The classifier the selection runs on Linux, fed the lines a process's
;; maps would hold. Only a mapping's last path component names a library;
;; both families, or neither, read as unknown, which is refused.
(define (maps-line path) (string-append "7f0000000000-7f0000001000 r-xp 00000000 08:01 1234    " path "\n"))
(want "PN-L glibc on x86_64: libc.so.6 and ld-linux-x86-64.so.2 read as glibc"
      (maps-libc (string-append (maps-line "/usr/bin/scheme") (maps-line "/usr/lib/x86_64-linux-gnu/libc.so.6")
                                (maps-line "/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2") "7ffd0000-7ffd1000 rw-p 00000000 00:00 0    [stack]\n"))
      "glibc")
(want "PN-L musl: ld-musl-* reads as musl, and so does libc.musl-* alone"
      (list (maps-libc (string-append (maps-line "/usr/bin/scheme") (maps-line "/lib/ld-musl-aarch64.so.1")))
            (maps-libc (maps-line "/usr/lib/libc.musl-x86_64.so.1")))
      '("musl" "musl"))
(want "PN-L a directory named like a library names nothing: musl under /opt/libc.so.6-build reads as musl, not glibc"
      (maps-libc (string-append (maps-line "/opt/libc.so.6-build/bin/scheme") (maps-line "/lib/ld-musl-x86_64.so.1")))
      "musl")
(want "PN-L both families, neither (a static executable), and an empty text read as unknown"
      (list (maps-libc (string-append (maps-line "/lib/libc.so.6") (maps-line "/lib/ld-musl-x86_64.so.1")))
            (maps-libc (string-append (maps-line "/usr/local/bin/scheme") "7ffd0000-7ffd1000 r-xp 00000000 00:00 0    [vdso]\n"))
            (maps-libc ""))
      '("unknown" "unknown" "unknown"))
(want "PN-L an unlinked object's \" (deleted)\" suffix is not part of its name"
      (maps-libc (maps-line "/usr/lib/libc.so.6 (deleted)"))
      "glibc")

;; ---- P3: an unlisted platform is refused by name, in a child -------------------

;; NOTE: IMPORTING A LIBRARY DOES NOT RUN ITS BODY -- Chez runs it at the
;; first reference to one of its bindings -- so the child refers to one.
(define loads-table "(import (chezscheme) (theourgia platform-numbers))\n(platform-key)\n(display \"loaded\")\n(newline)\n")
(define refusal-darwin-x86
  '(error platform-unmeasured (system "Darwin") (machine "x86_64")
          (remedy "run test/probe/layout.c and add its row")))
(want "PN-P3 Darwin x86_64 (and so Rosetta) is refused: the datum on stderr, exit 75, nothing on stdout"
      (let ((r (child (string-append forced "Darwin/x86_64") loads-table)))
        (list (car r) (cadr r) (errors-of (caddr r))))
      (list 75 "" (list refusal-darwin-x86)))
(want "PN-P3 Linux x86_64 with musl is refused, and the datum names the C library"
      (let ((r (child (string-append forced "Linux/x86_64/musl") loads-table)))
        (list (car r) (cadr r) (errors-of (caddr r))))
      (list 75 ""
            (list '(error platform-unmeasured (system "Linux") (machine "x86_64") (libc "musl")
                          (remedy "run test/probe/layout.c and add its row")))))
(want "PN-P3 a program that loads the FFI (core.sc, no arguments) is refused before it prints anything"
      (let ((r (run-script (string-append forced "Darwin/x86_64") "../core.sc")))
        (list (car r) (cadr r) (errors-of (caddr r))))
      (list 75 "" (list refusal-darwin-x86)))
(want "PN-P3 the seam is the injection build's alone: with THEOURGIA_INJECT unset the key is not read"
      (let ((r (child "-u THEOURGIA_INJECT THEOURGIA_PLATFORM_KEY=Darwin/x86_64"
                      "(import (chezscheme) (theourgia platform-numbers))\n(write (platform-key))\n(newline)\n")))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (platform-key))))
;; the files read as data, top-level forms and a library's body
(define (forms-of path)
  (call-with-input-file path
    (lambda (p) (let loop ((out '())) (let ((x (read p))) (if (eof-object? x) (reverse out) (loop (cons x out))))))))
(define (library-body forms)
  (apply append
         (map (lambda (f) (if (and (pair? f) (eq? (car f) 'library)) (cdddr f) (list f))) forms)))
(define (def-name f)
  (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
       (if (pair? (cadr f)) (caadr f) (cadr f))))
;; The row above sees behaviour only; this one reads the code. The seam is
;; a meta-cond of exactly two clauses on a value read at EXPANSION, its
;; ordinary branch defines forced-key as the constant #f, and the
;; variable's name occurs as a string in the library's CODE exactly once,
;; inside the injection clause -- so an ordinary build has no code that
;; reads it, and a read anywhere else, at expansion or at run time, adds
;; to the count. Comments are not code and are not counted.
(define table-body (library-body (forms-of "../platform-numbers.sc")))
(define seam-form (find (lambda (f) (and (pair? f) (eq? (car f) 'meta-cond))) table-body))
;; how many times a string occurs as a datum in the forms, inside vectors too
(define (string-count x str)
  (let count ((x x))
    (cond ((string? x) (if (string=? x str) 1 0))
          ((pair? x) (+ (count (car x)) (count (cdr x))))
          ((vector? x) (count (vector->list x)))
          (else 0))))
(want "PN-P3 the seam in the text: an expansion-time meta-cond, #f in its ordinary branch, the variable read nowhere else"
      (list (and seam-form (cadr seam-form) (car (cadr seam-form)))
            (and seam-form (assq 'else (cdr seam-form)))
            (and (find (lambda (f) (and (pair? f) (eq? (car f) 'meta) (pair? (cdr f)) (eq? (cadr f) 'define)
                                        (pair? (cddr f)) (eq? (caddr f) 'inject-mode)))
                       table-body)
                 #t)
            (and seam-form (length (cdr seam-form)))
            (string-count table-body "THEOURGIA_PLATFORM_KEY")
            (and seam-form (string-count (cadr seam-form) "THEOURGIA_PLATFORM_KEY")))
      (list '(eq? inject-mode 'on) '(else (define (forced-key) #f)) #t 2 1 1))

;; ---- P4: the probe, compiled here, against this platform's row -----------------
;;
;; The instrument the rows came from, run where the suite runs. The kernel
;; release and the C library's version are the machine's, not the layout's,
;; and are left out of the comparison; everything else must be equal. Where
;; no C compiler exists the row says so and names the archived reading this
;; platform's row stands on.
(define (layout-view data)
  (let loop ((ds data) (out '()))
    (cond
      ((null? ds) (reverse out))
      ((and (pair? (car ds)) (eq? (caar ds) 'release)) (loop (cdr ds) out))
      ((and (pair? (car ds)) (eq? (caar ds) 'libc) (pair? (cdar ds)) (string? (cadar ds)))
       (let* ((s (cadar ds))
              (word (let w ((i 0)) (cond ((= i (string-length s)) s)
                                         ((char=? (string-ref s i) #\space) (substring s 0 i))
                                         (else (w (+ i 1)))))))
         (loop (cdr ds) (cons (list 'libc word) out))))
      (else (loop (cdr ds) (cons (car ds) out))))))
(define cc (trim-newline (sh-out "command -v cc 2>/dev/null")))
(if (string=? cc "")
    (begin
      (printf "PN-P4 information: no C compiler on this machine; this platform's row stands on ~a\n"
              (let ((i (let find ((rs (platform-readings)) (fs reading-files))
                         (cond ((null? rs) "no archived reading")
                               ((equal? (car rs) (platform-row)) (car fs))
                               (else (find (cdr rs) (cdr fs)))))))
                i))
      (want "PN-P4 no compiler here: the row is one of the archived readings, named above"
            (and (member (platform-row) (platform-readings)) #t)
            #t))
    (let* ((bin (string-append here "/layout-probe"))
           (built (sh (quoted cc) " -O0 -o " (quoted bin) " probe/layout.c 2> "
                      (quoted (string-append here "/cc.err"))))
           (out (if (= built 0) (data-of-text (sh-out (quoted bin))) 'NOT-BUILT)))
      (want "PN-P4 the probe, compiled and run here, prints this platform's row (release and libc version aside)"
            (if (eq? out 'NOT-BUILT) (list 'NOT-BUILT (file-text (string-append here "/cc.err"))) (layout-view out))
            (layout-view (platform-row)))))

;; ---- P5: sockaddr_un as bytes, on this row and on the forced ones -------------

(define (sockaddr-report-script path)
  (string-append "(import (chezscheme) (theourgia ffi) (theourgia platform-numbers))\n"
                 "(let ((sa (sockaddr-un \"" path "\")))\n"
                 "  (write (list (platform-key) (bytevector-length sa) (let loop ((i 0) (out '())) (if (= i 8) (reverse out) (loop (+ i 1) (cons (bytevector-u8-ref sa i) out))))))\n"
                 "  (newline))\n"))
(want "PN-P5 on the BSD layout (forced FreeBSD, and the child says so): 106 bytes, sun_len 106 at 0, family 1 at 1, the path at 2"
      (let ((r (child (string-append forced "FreeBSD/amd64") (sockaddr-report-script "/a/b"))))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (list '("FreeBSD" "amd64" #f) 106 (list 106 1 47 97 47 98 0 0)))))
(want "PN-P5 on the Linux layout (forced Linux x86_64, and the child says so): 110 bytes, no length byte, family 1 in two bytes at 0, the path at 2"
      (let ((r (child (string-append forced "Linux/x86_64/glibc") (sockaddr-report-script "/a/b"))))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (list '("Linux" "x86_64" "glibc") 110 (if (eq? (native-endianness) 'little) (list 1 0 47 97 47 98 0 0) (list 0 1 47 97 47 98 0 0))))))
(want "PN-P5 on this row: the address is the struct's size, and the path sits at sun_path's offset"
      (let ((sa (sockaddr-un "/a/b")) (off (platform-field 'sockaddr_un 'sun_path 'offset)))
        (list (bytevector-length sa) (utf8->string (let ((o (make-bytevector 4))) (bytevector-copy! sa off o 0 4) o))))
      (list (platform-struct-size 'sockaddr_un) "/a/b"))
(want "PN-P5 the probe's SUN_LEN sample is sun_path's offset plus the path's length, on every row"
      (map (lambda (r)
             (let* ((s (find (lambda (d) (and (pair? d) (eq? (car d) 'struct) (eq? (cadr d) 'sockaddr_un))) r))
                    (sl (find (lambda (d) (and (pair? d) (eq? (car d) 'sun-len))) (cddr s)))
                    (path (find (lambda (d) (and (pair? d) (eq? (car d) 'field) (eq? (cadr d) 'sun_path))) (cddr s))))
               (= (caddr sl) (+ (cadr (assq 'offset (cddr path))) (string-length (cadr sl))))))
           (platform-readings))
      '(#t #t #t #t))

;; ---- P9: the Linux numbers the BSD literals got wrong, through the table ------

(define linux-numbers-script
  (string-append "(import (chezscheme) (theourgia platform-numbers))\n"
                 "(write (list (platform-number 'O_APPEND) (platform-number 'O_CREAT) (platform-number 'O_TRUNC)\n"
                 "             (platform-number 'FIOCLEX) (platform-number '_SC_NPROCESSORS_ONLN)\n"
                 "             (platform-struct-value 'posix_spawn 'file-actions-size)\n"
                 "             (platform-field 'stat 'st_mode 'offset) (platform-field 'stat 'st_mode 'size)))\n"
                 "(newline)\n"))
(want "PN-P9 forced Linux x86_64: O_APPEND 1024, O_CREAT 64, O_TRUNC 512, FIOCLEX 0x5451, _SC_NPROCESSORS_ONLN 84, file actions 80, st_mode at 24 in 4"
      (let ((r (child (string-append forced "Linux/x86_64/glibc") linux-numbers-script)))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (list 1024 64 512 21585 84 80 24 4))))
(want "PN-P9 forced Linux aarch64: the same numbers, and st_mode at 16 -- the offset a system-wide rule got wrong"
      (let ((r (child (string-append forced "Linux/aarch64/glibc") linux-numbers-script)))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (list 1024 64 512 21585 84 80 16 4))))

;; ---- P7: the append bit, through the log's own sequence ----------------------
;;
;; log.sc's write-line! opens a segment with (fd-open path '(write append)),
;; writes the record with write-all!, and closes it, once per record. Two
;; records written that way must both be there, in order. Two writes on one
;; descriptor would pass without the append bit and prove nothing, so the
;; file is opened afresh for each.
(define append-path (string-append here "/append.log"))
(call-with-output-file append-path (lambda (p) (void)))
(for-each (lambda (record)
            (let ((fd (fd-open append-path '(write append))))
              (write-all! fd (string->utf8 record) append-path)
              (fd-close fd)))
          '("first record\n" "second\n"))
(want "PN-P7 two records appended through fd-open's append, write-all! and close, each on its own open: both, in order"
      (file-text append-path)
      "first record\nsecond\n")

;; ---- P6: no platform number written down in the code --------------------------
;;
;; The four files are read as data. Each form is walked with the name of
;; the top-level definition it sits in and the forms that enclose it, and
;; four things are checked.
;;
;; (a) THE READS, ENUMERATED. Every call of an accessor of the table, and
;; of ffi.sc's three struct helpers, is listed below with its file and its
;; definition (the list CONSUMERS.md gives, as the code has it), and the
;; code must hold exactly these: a number written back as a literal drops
;; its read from the list, a read of another name changes it, and a new
;; one adds to it. Each is red.
;;
;; (b) THE FORMS THE CODE USED TO USE must not appear:
;;   a (case platform-os ...) or (case (machine-kind) ...);
;;   an (eq? platform-os ...) outside the one definition of macos?;
;;   a definition of an UPPERCASE name, or of one of the lowercase names
;;   that held a platform number, whose value is an integer literal;
;;   a fixed-width native bytevector access, or a generic one
;;   (bytevector-uint-ref and its siblings, foreign-ref, foreign-set!) at a
;;   literal offset other than 0, inside the definitions that read or write
;;   a kernel struct -- their fields go through row-uint-ref and its
;;   siblings;
;;   machine-kind outside the definitions that choose a mechanism with it.
;;
;; (c) A NAME SOME ROW LACKS is read only under a guard that tests the
;; platform: inside a when, unless, if, and or cond clause whose test
;; names macos?, platform-os, machine-kind or platform-field-present?, and
;; not in that test; or in a procedure every call of which is so guarded
;; (rss-darwin, rss-freebsd). A name is lacking when a row prints it
;; absent: a constant, a struct, or a struct's field.
;;
;; (d) THE STRUCT HELPERS are exactly unsigned, signed and unsigned-set
;; reads at the row's offset and width.
;; every subform of every top-level form, with the top-level definition's
;; name and the enclosing forms, innermost first
(define (walk-sites file)
  (let ((out '()))
    (for-each
      (lambda (top)
        (let ((name (def-name top)))
          (let visit ((x top) (up '()))
            (when (pair? x)
              (set! out (cons (list file name x up) out))
              (for-each (lambda (c) (visit c (cons x up))) (if (list? x) x (list (car x) (cdr x))))))))
      (library-body (forms-of file)))
    (reverse out)))
(define scanned-files '("../ffi.sc" "../daemon.sc" "../eval-supervise.sc" "../mcp/server.sc"))
(define sites (apply append (map walk-sites scanned-files)))
(define (site-file s) (car s))
(define (site-name s) (cadr s))
(define (site-form s) (caddr s))
(define (site-up s) (cadddr s))

(define table-accessors
  '(platform-number platform-field platform-field-present? platform-struct-size platform-struct-value
    platform-type-size platform-wait-sample))
(define struct-helpers '(row-uint-ref row-sint-ref row-uint-set!))
(define (accessor-call? x)
  (and (list? x) (pair? x) (memq (car x) (append table-accessors struct-helpers)) #t))
(define (read-sites)
  (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
             (map (lambda (s) (list (site-file s) (site-name s) (site-form s)))
                  (filter (lambda (s) (and (accessor-call? (site-form s))
                                           (not (memq (site-name s) struct-helpers))))
                          sites))))
(define expected-read-sites
  '(("../daemon.sc" signal-term (platform-number (quote SIGTERM)))
    ("../eval-supervise.sc" sigkill (platform-number (quote SIGKILL)))
    ("../ffi.sc" AF_UNIX (platform-number (quote AF_UNIX)))
    ("../ffi.sc" EACCES (platform-number (quote EACCES)))
    ("../ffi.sc" EBADF (platform-number (quote EBADF)))
    ("../ffi.sc" ECHILD (platform-number (quote ECHILD)))
    ("../ffi.sc" EEXIST (platform-number (quote EEXIST)))
    ("../ffi.sc" EINTR (platform-number (quote EINTR)))
    ("../ffi.sc" EIO (platform-number (quote EIO)))
    ("../ffi.sc" EISDIR (platform-number (quote EISDIR)))
    ("../ffi.sc" ELOOP (platform-number (quote ELOOP)))
    ("../ffi.sc" EMFILE (platform-number (quote EMFILE)))
    ("../ffi.sc" ENAMETOOLONG (platform-number (quote ENAMETOOLONG)))
    ("../ffi.sc" ENOENT (platform-number (quote ENOENT)))
    ("../ffi.sc" ENOTDIR (platform-number (quote ENOTDIR)))
    ("../ffi.sc" EOVERFLOW (platform-number (quote EOVERFLOW)))
    ("../ffi.sc" EPERM (platform-number (quote EPERM)))
    ("../ffi.sc" EROFS (platform-number (quote EROFS)))
    ("../ffi.sc" EWOULDBLOCK (platform-number (quote EWOULDBLOCK)))
    ("../ffi.sc" FD_CLOEXEC (platform-number (quote FD_CLOEXEC)))
    ("../ffi.sc" FIOCLEX (platform-number (quote FIOCLEX)))
    ("../ffi.sc" F_GETFD (platform-number (quote F_GETFD)))
    ("../ffi.sc" LOCK_EX (platform-number (quote LOCK_EX)))
    ("../ffi.sc" LOCK_NB (platform-number (quote LOCK_NB)))
    ("../ffi.sc" LOCK_SH (platform-number (quote LOCK_SH)))
    ("../ffi.sc" LOCK_UN (platform-number (quote LOCK_UN)))
    ("../ffi.sc" O_APPEND (platform-number (quote O_APPEND)))
    ("../ffi.sc" O_RDONLY (platform-number (quote O_RDONLY)))
    ("../ffi.sc" O_RDWR (platform-number (quote O_RDWR)))
    ("../ffi.sc" O_WRONLY (platform-number (quote O_WRONLY)))
    ("../ffi.sc" RLIMIT_CPU (platform-number (quote RLIMIT_CPU)))
    ("../ffi.sc" SEEK_CUR (platform-number (quote SEEK_CUR)))
    ("../ffi.sc" SEEK_END (platform-number (quote SEEK_END)))
    ("../ffi.sc" SEEK_SET (platform-number (quote SEEK_SET)))
    ("../ffi.sc" SOCKADDR_UN_SIZE (platform-struct-size (quote sockaddr_un)))
    ("../ffi.sc" SOCK_STREAM (platform-number (quote SOCK_STREAM)))
    ("../ffi.sc" SOL_SOCKET (platform-number (quote SOL_SOCKET)))
    ("../ffi.sc" SO_RCVTIMEO (platform-number (quote SO_RCVTIMEO)))
    ("../ffi.sc" SO_SNDTIMEO (platform-number (quote SO_SNDTIMEO)))
    ("../ffi.sc" SUN_PATH_MAX (platform-field (quote sockaddr_un) (quote sun_path) (quote size)))
    ("../ffi.sc" SUN_PATH_OFFSET (platform-field (quote sockaddr_un) (quote sun_path) (quote offset)))
    ("../ffi.sc" S_IFDIR (platform-number (quote S_IFDIR)))
    ("../ffi.sc" S_IFLNK (platform-number (quote S_IFLNK)))
    ("../ffi.sc" S_IFMT (platform-number (quote S_IFMT)))
    ("../ffi.sc" S_IFREG (platform-number (quote S_IFREG)))
    ("../ffi.sc" S_IFSOCK (platform-number (quote S_IFSOCK)))
    ("../ffi.sc" WNOHANG (platform-number (quote WNOHANG)))
    ("../ffi.sc" _SC_NPROCESSORS_ONLN (platform-number (quote _SC_NPROCESSORS_ONLN)))
    ("../ffi.sc" dirent-name-offset (platform-field (quote dirent) (quote d_name) (quote offset)))
    ("../ffi.sc" flush! (platform-number (quote F_FULLFSYNC)))
    ("../ffi.sc" getrlimit (platform-struct-size (quote rlimit)))
    ("../ffi.sc" getrlimit (row-uint-ref bv (quote rlimit) (quote rlim_cur)))
    ("../ffi.sc" getrlimit (row-uint-ref bv (quote rlimit) (quote rlim_max)))
    ("../ffi.sc" make-pid-buffer (platform-type-size (quote pid_t)))
    ("../ffi.sc" path-case-sensitive? (platform-number (quote _PC_CASE_SENSITIVE)))
    ("../ffi.sc" path-device-inode (row-uint-ref buf (quote stat) (quote st_dev)))
    ("../ffi.sc" path-device-inode (row-uint-ref buf (quote stat) (quote st_ino)))
    ("../ffi.sc" path-executable? (platform-number (quote X_OK)))
    ("../ffi.sc" pid-buffer-ref (platform-type-size (quote pid_t)))
    ("../ffi.sc" real-path (platform-number (quote PATH_MAX)))
    ("../ffi.sc" reap-children! (platform-type-size (quote int)))
    ("../ffi.sc" rlimit-bytes (platform-struct-size (quote rlimit)))
    ("../ffi.sc" rlimit-bytes (row-uint-set! bv (quote rlimit) (quote rlim_cur) cur))
    ("../ffi.sc" rlimit-bytes (row-uint-set! bv (quote rlimit) (quote rlim_max) max))
    ("../ffi.sc" rss-darwin (platform-number (quote PROC_PIDTASKINFO)))
    ("../ffi.sc" rss-darwin (platform-struct-size (quote proc_taskinfo)))
    ("../ffi.sc" rss-darwin (row-uint-ref bv (quote proc_taskinfo) (quote pti_resident_size)))
    ("../ffi.sc" rss-freebsd (platform-number (quote CTL_KERN)))
    ("../ffi.sc" rss-freebsd (platform-number (quote KERN_PROC)))
    ("../ffi.sc" rss-freebsd (platform-number (quote KERN_PROC_PID)))
    ("../ffi.sc" rss-freebsd (platform-struct-size (quote kinfo_proc)))
    ("../ffi.sc" rss-freebsd (platform-type-size (quote int)))
    ("../ffi.sc" rss-freebsd (row-uint-ref buf (quote kinfo_proc) (quote ki_rssize)))
    ("../ffi.sc" size-entry (row-sint-ref buf (quote stat) (quote st_size)))
    ("../ffi.sc" sockaddr-un (platform-field-present? (quote sockaddr_un) (quote sun_len)))
    ("../ffi.sc" sockaddr-un (row-uint-set! sa (quote sockaddr_un) (quote sun_family) AF_UNIX))
    ("../ffi.sc" sockaddr-un (row-uint-set! sa (quote sockaddr_un) (quote sun_len) SOCKADDR_UN_SIZE))
    ("../ffi.sc" spawn-O_CREAT (platform-number (quote O_CREAT)))
    ("../ffi.sc" spawn-O_RDONLY (platform-number (quote O_RDONLY)))
    ("../ffi.sc" spawn-O_TRUNC (platform-number (quote O_TRUNC)))
    ("../ffi.sc" spawn-O_WRONLY (platform-number (quote O_WRONLY)))
    ("../ffi.sc" spawn-captured! (platform-struct-value (quote posix_spawn) (quote file-actions-size)))
    ("../ffi.sc" st-mode-of (row-uint-ref buf (quote stat) (quote st_mode)))
    ("../ffi.sc" stat-buffer-size (platform-struct-size (quote stat)))
    ("../ffi.sc" timeval-bytes (platform-struct-size (quote timeval)))
    ("../ffi.sc" timeval-bytes (row-uint-set! tv (quote timeval) (quote tv_sec) (div ms 1000)))
    ("../ffi.sc" timeval-bytes (row-uint-set! tv (quote timeval) (quote tv_usec) (* 1000 (mod ms 1000))))
    ("../ffi.sc" waitpid-status (platform-type-size (quote int)))
    ("../ffi.sc" waitpid-status (platform-type-size (quote int)))
    ("../mcp/server.sc" kill-at-deadline (platform-number (quote SIGKILL)))))
;; compared as MULTISETS: the same read twice in one definition is two
;; entries, and replacing one of them with a literal leaves one too few
(define (multiset-minus xs ys)
  (let loop ((xs xs) (ys ys) (out '()))
    (cond ((null? xs) (reverse out))
          ((member (car xs) ys) (loop (cdr xs) (let drop ((ys ys)) (if (equal? (car ys) (car xs)) (cdr ys) (cons (car ys) (drop (cdr ys))))) out))
          (else (loop (cdr xs) ys (cons (car xs) out))))))
(want "PN-P6 (a) the reads of the table and the struct helpers are exactly the enumerated ones, counted"
      (let ((got (read-sites)))
        (list (multiset-minus got expected-read-sites)
              (multiset-minus expected-read-sites got)))
      '(() ()))

;; (e) A NUMBER READ INTO A DEFINITION IS USED: a definition of a VALUE
;; that holds a read but that nothing refers to any more means its use was
;; written back as a literal while the read stayed, which (a) alone would
;; not see. A procedure's reads are its own use, and some are called only
;; from other files, so procedures are not counted.
;; references outside quotations, the defining form itself left out
(define (symbol-count sym forms)
  (let count ((x forms))
    (cond ((eq? x sym) 1)
          ((and (pair? x) (eq? (car x) 'quote)) 0)
          ((and (pair? x) (eq? (car x) 'define) (pair? (cdr x)) (eq? (cadr x) sym)) 0)
          ((pair? x) (+ (count (car x)) (count (cdr x))))
          (else 0))))
;; the whole files, a library's export clause included: an exported value is
;; used by whoever imports it
(define all-bodies (apply append (map forms-of scanned-files)))
;; a (define NAME <not a lambda>) at the top of a library's body -- looked
;; for in the BODIES: the whole files' top-level forms are the library
;; forms themselves, and a search there finds no definition at all
(define definition-forms (apply append (map (lambda (f) (library-body (forms-of f))) scanned-files)))
(define (value-definition? n)
  (exists (lambda (f) (and (pair? f) (eq? (car f) 'define) (pair? (cdr f)) (eq? (cadr f) n)
                           (pair? (cddr f)) (not (and (pair? (caddr f)) (eq? (car (caddr f)) 'lambda)))))
          definition-forms))
(define read-definitions
  (let ((out '()))
    (for-each (lambda (x) (let ((n (cadr x))) (when (and (symbol? n) (not (memq n out)) (value-definition? n))
                                                (set! out (cons n out)))))
              expected-read-sites)
    (reverse out)))
(want "PN-P6 (e) every definition that holds a read is referred to beyond its own definition"
      (filter (lambda (n) (zero? (symbol-count n all-bodies))) read-definitions)
      '())
(want "PN-P6 (e) CONTROL: the rule sees the value definitions it checks, and a use of one"
      (list (length read-definitions)
            (and (memq 'spawn-O_RDONLY read-definitions) #t)
            (> (symbol-count 'spawn-O_RDONLY all-bodies) 0))
      (list 54 #t #t))

(define (upper-name? s)
  (let ((t (symbol->string s)))
    (and (> (string-length t) 0)
         (for-all (lambda (c) (or (char-upper-case? c) (char-numeric? c) (memv c '(#\_ #\-)))) (string->list t))
         (exists char-upper-case? (string->list t)))))
(define formerly-lowercase-numbers
  '(st-size-offset dirent-name-offset stat-buffer-size st-dev-offset st-ino-offset signal-term sigkill
    spawn-O_RDONLY spawn-O_WRONLY spawn-O_CREAT spawn-O_TRUNC))
(define struct-definitions
  '(st-mode-of path-device-inode size-entry sockaddr-un timeval-bytes rlimit-bytes getrlimit rss-darwin rss-freebsd
    make-pid-buffer pid-buffer-ref waitpid-status))
(define mechanism-definitions '(process-rss-bytes spawn-detached! spawn-captured! path-case-sensitive? machine-kind))
(define fixed-width-accessors
  '(bytevector-u8-ref bytevector-u16-native-ref bytevector-u32-native-ref bytevector-u64-native-ref
    bytevector-s16-native-ref bytevector-s32-native-ref bytevector-s64-native-ref
    bytevector-u16-native-set! bytevector-u32-native-set! bytevector-u64-native-set! bytevector-u8-set!))
(define generic-accessors
  '(bytevector-uint-ref bytevector-sint-ref bytevector-uint-set! bytevector-sint-set! foreign-ref foreign-set!))
;; the offset is the second argument of a bytevector accessor and the
;; third of foreign-ref and foreign-set!, after the type and the address.
;; Offset 0 is a scalar buffer's one value (a pid, an int, a size_t, the
;; first word of a sysctl name) and is allowed: a struct field read back at
;; a literal 0 instead of through a helper drops its read from (a).
(define (literal-offset? x)
  (let ((i (if (memq (car x) '(foreign-ref foreign-set!)) 3 2)))
    (and (list? x) (> (length x) i)
         (let ((off (list-ref x i))) (and (integer? off) (exact? off) (not (zero? off)))))))
(define (site-finding s)
  (let ((file (site-file s)) (name (site-name s)) (x (site-form s)))
    (cond
      ((not (list? x)) #f)
      ((and (eq? (car x) 'case) (pair? (cdr x))
            (or (eq? (cadr x) 'platform-os) (equal? (cadr x) '(machine-kind))))
       (list 'case-on-platform file name))
      ((and (eq? (car x) 'eq?) (memq 'platform-os x) (not (eq? name 'macos?)))
       (list 'eq-platform-os file name))
      ((and (eq? (car x) 'define) (pair? (cdr x)) (symbol? (cadr x)) (pair? (cddr x))
            (or (upper-name? (cadr x)) (memq (cadr x) formerly-lowercase-numbers))
            (let ((v (caddr x))) (and (integer? v) (exact? v))))
       (list 'literal-number file (cadr x)))
      ((and (memq (car x) fixed-width-accessors) (memq name struct-definitions))
       (list 'fixed-width-struct-access file name (car x)))
      ((and (memq (car x) generic-accessors) (memq name struct-definitions) (literal-offset? x))
       (list 'literal-offset-struct-access file name (car x)))
      ((and (eq? (car x) 'machine-kind) (not (memq name mechanism-definitions)))
       (list 'machine-kind-outside-a-mechanism file name))
      (else #f))))
(want "PN-P6 (b) the four files hold no platform number of their own, by every form the code used to use"
      (filter (lambda (f) f) (map site-finding sites))
      '())

;; what some row prints absent: (constant NAME), (struct NAME), (field STRUCT NAME)
(define lacking
  (let ((out '()))
    (define (note! x) (unless (member x out) (set! out (cons x out))))
    (for-each
      (lambda (r)
        (for-each (lambda (e) (when (and (pair? e) (pair? (cdr e)) (eq? (cadr e) 'absent)) (note! (list 'constant (car e)))))
                  (cdr (assq 'constants r)))
        (for-each (lambda (d)
                    (when (and (pair? d) (eq? (car d) 'struct) (pair? (cdr d)))
                      (if (and (pair? (cddr d)) (eq? (caddr d) 'absent))
                          (note! (list 'struct (cadr d)))
                          (for-each (lambda (f)
                                      (when (and (pair? f) (eq? (car f) 'field) (pair? (cddr f)) (eq? (caddr f) 'absent))
                                        (note! (list 'field (cadr d) (cadr f)))))
                                    (cddr d)))))
                  r))
      (platform-readings))
    out))
(define (quoted-arg x i)
  (and (> (length x) i) (let ((a (list-ref x i))) (and (pair? a) (eq? (car a) 'quote) (pair? (cdr a)) (cadr a)))))
;; -> the lacking names a read depends on
(define (lacking-of x)
  (let* ((helper? (memq (car x) struct-helpers))
         (s (if helper? (quoted-arg x 2) (quoted-arg x 1)))
         (f (if helper? (quoted-arg x 3) (quoted-arg x 2))))
    (filter (lambda (l) (member l lacking))
            (case (car x)
              ((platform-number) (list (list 'constant s)))
              ((platform-field-present?) '())
              (else (list (list 'struct s) (list 'field s f)))))))
(define guard-names '(macos? platform-os machine-kind platform-field-present?))
(define (names-a-guard? x)
  (let search ((x x))
    (cond ((symbol? x) (and (memq x guard-names) #t))
          ((pair? x) (or (search (car x)) (search (cdr x))))
          (else #f))))
;; guarded when some enclosing form tests the platform and the read is not
;; inside the test itself: a when, unless, if or and whose first operand
;; names a guard, or a clause of a cond whose test does
(define (guarded? x up)
  (let loop ((child x) (up up))
    (and (pair? up)
         (let ((a (car up)) (parent (and (pair? (cdr up)) (cadr up))))
           (or (and (list? a) (pair? a) (memq (car a) '(when unless if and)) (pair? (cdr a))
                    (names-a-guard? (cadr a)) (not (eq? child (cadr a))))
               (and (list? parent) (pair? parent) (eq? (car parent) 'cond) (list? a) (pair? a)
                    (names-a-guard? (car a)) (not (eq? child (car a))))
               (loop a (cdr up)))))))
(define branch-procedures '(rss-darwin rss-freebsd))
;; every call of a procedure, its own definition's head aside
(define (calls-of name)
  (filter (lambda (s)
            (let ((x (site-form s)) (up (site-up s)))
              (and (list? x) (pair? x) (eq? (car x) name)
                   (not (and (pair? up) (pair? (car up)) (eq? (caar up) 'define) (eq? (cadar up) x))))))
          sites))
(define (absent-finding s)
  (let ((x (site-form s)))
    (and (accessor-call? x)
         (not (memq (site-name s) struct-helpers))
         (pair? (lacking-of x))
         (not (guarded? x (site-up s)))
         (not (and (memq (site-name s) branch-procedures)
                   (pair? (calls-of (site-name s)))
                   (for-all (lambda (c) (guarded? (site-form c) (site-up c))) (calls-of (site-name s)))))
         (list 'lacking-name-unguarded (site-file s) (site-name s) x))))
(want "PN-P6 (c) every read of a name some row lacks is under a guard that tests the platform"
      (filter (lambda (f) f) (map absent-finding sites))
      '())
(want "PN-P6 (c) CONTROL: the rule sees what it guards -- the lacking names, and reads of them under each guard shape"
      (list (and (member '(constant F_FULLFSYNC) lacking) (member '(struct kinfo_proc) lacking)
                 (member '(field sockaddr_un sun_len) lacking) #t)
            (length (filter (lambda (s) (and (accessor-call? (site-form s)) (pair? (lacking-of (site-form s)))
                                             (not (memq (site-name s) struct-helpers))))
                            sites)))
      (list #t 11))

(define ffi-sites (walk-sites "../ffi.sc"))
(define (defined-form name)
  (let ((s (find (lambda (s) (and (eq? (site-name s) name) (eq? (car (site-form s)) 'define) (eq? (def-name (site-form s)) name)))
                 ffi-sites)))
    (and s (site-form s))))
(want "PN-P6 (d) the struct helpers read unsigned, signed and write unsigned at the row's offset and width"
      (map defined-form struct-helpers)
      '((define (row-uint-ref bv sname fname)
          (bytevector-uint-ref bv (platform-field sname fname 'offset) (native-endianness) (platform-field sname fname 'size)))
        (define (row-sint-ref bv sname fname)
          (bytevector-sint-ref bv (platform-field sname fname 'offset) (native-endianness) (platform-field sname fname 'size)))
        (define (row-uint-set! bv sname fname value)
          (bytevector-uint-set! bv (platform-field sname fname 'offset) value (native-endianness) (platform-field sname fname 'size)))))

;; ---- P8: allocations are at least the row's sizes -------------------------------

(define (holds? form part)
  (let search ((x form))
    (or (equal? x part)
        (and (pair? x) (or (search (car x)) (search (cdr x)))))))
(want "PN-P8 the spawn's file actions are allocated at least at the row's size; the stat and realpath buffers at least theirs"
      (list (holds? (defined-form 'spawn-captured!)
                    '(foreign-alloc (max width 16 (platform-struct-value 'posix_spawn 'file-actions-size))))
            (holds? (defined-form 'stat-buffer-size) '(max 512 (platform-struct-size 'stat)))
            (holds? (defined-form 'real-path) '(max 4096 (platform-number 'PATH_MAX))))
      '(#t #t #t))
(want "PN-P8 and those minimums cover every row: file actions, struct stat, PATH_MAX"
      (map (lambda (r)
             (let ((fa (cadr (assq 'file-actions-size
                                   (cddr (find (lambda (d) (and (pair? d) (eq? (car d) 'struct) (eq? (cadr d) 'posix_spawn))) r)))))
                   (st (cadr (assq 'size (cddr (find (lambda (d) (and (pair? d) (eq? (car d) 'struct) (eq? (cadr d) 'stat))) r)))))
                   (pm (cadr (assq 'PATH_MAX (cdr (assq 'constants r))))))
               (list (>= (max 8 16 fa) fa) (>= (max 512 st) st) (>= (max 4096 pm) pm))))
           (platform-readings))
      '((#t #t #t) (#t #t #t) (#t #t #t) (#t #t #t)))

;; ---- signedness is the reader's, from the C type --------------------------------
;;
;; The rows carry no signedness. st_mode, st_dev and st_ino are read
;; unsigned, st_size (off_t) signed, and these are the forms that do it.
(want "PN-S st_mode, st_dev and st_ino are read unsigned and st_size signed, at the row's offset and width"
      (list (holds? (defined-form 'st-mode-of) '(row-uint-ref buf 'stat 'st_mode))
            (holds? (defined-form 'path-device-inode) '(row-uint-ref buf 'stat 'st_dev))
            (holds? (defined-form 'path-device-inode) '(row-uint-ref buf 'stat 'st_ino))
            (holds? (defined-form 'size-entry) '(row-sint-ref buf 'stat 'st_size)))
      '(#t #t #t #t))
(want "PN-S and on this row a file's size reads back as the bytes written"
      (let ((p (string-append here "/sized")))
        (call-with-output-file p (lambda (o) (put-string o "twelve bytes")))
        (size-entry p))
      12)

;; ---- the wait status: POSIX's decoding, checked by the rows' samples -------------

(want "PN-W waitpid-status decodes the signal as the low seven bits and the exit code as bits 8-15"
      (list (holds? (defined-form 'waitpid-status) '(fxand w #x7f))
            (holds? (defined-form 'waitpid-status) '(fxand (fxsra w 8) #xff)))
      '(#t #t))
(want "PN-W that decoding gives each row's own macro samples: WEXITSTATUS(0x0300) and WTERMSIG(0x0009)"
      (map (lambda (r)
             (let ((w (cdr (assq 'wait-status r))))
               (list (= (fxand (fxsra #x0300 8) #xff) (cadr (assq 'WEXITSTATUS-0x0300 w)))
                     (= (fxand #x0009 #x7f) (cadr (assq 'WTERMSIG-0x0009 w))))))
           (platform-readings))
      '((#t #t) (#t #t) (#t #t) (#t #t)))

(sh "rm -rf " (quoted here))
(printf "rows: ~a\n~a failures\nplatform-rows complete\n" rows bad)
(exit (if (zero? bad) 0 1))
