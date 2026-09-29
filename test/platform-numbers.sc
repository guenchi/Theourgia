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
      (list (entry-type here) (entry-type "platform-numbers.sc"))
      '(directory regular))

;; ---- P3: an unlisted platform is refused by name, in a child -------------------

(define loads-table "(import (chezscheme) (theourgia platform-numbers))\n(display \"loaded\")\n(newline)\n")
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
  (string-append "(import (chezscheme) (theourgia ffi))\n"
                 "(let ((sa (sockaddr-un \"" path "\")))\n"
                 "  (write (list (bytevector-length sa) (let loop ((i 0) (out '())) (if (= i 8) (reverse out) (loop (+ i 1) (cons (bytevector-u8-ref sa i) out))))))\n"
                 "  (newline))\n"))
(want "PN-P5 on the BSD layout (forced FreeBSD): 106 bytes, sun_len 106 at 0, family 1 at 1, the path at 2"
      (let ((r (child (string-append forced "FreeBSD/amd64") (sockaddr-report-script "/a/b"))))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (list 106 (list 106 1 47 97 47 98 0 0)))))
(want "PN-P5 on the Linux layout (forced Linux x86_64): 110 bytes, no length byte, family 1 in two bytes at 0, the path at 2"
      (let ((r (child (string-append forced "Linux/x86_64/glibc") (sockaddr-report-script "/a/b"))))
        (list (car r) (data-of-text (cadr r))))
      (list 0 (list (list 110 (if (eq? (native-endianness) 'little) (list 1 0 47 97 47 98 0 0) (list 0 1 47 97 47 98 0 0))))))
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
;; the top-level definition it sits in, and these must not appear:
;;   a (case platform-os ...) or (case (machine-kind) ...);
;;   an (eq? platform-os ...) outside the one definition of macos?;
;;   a definition of an UPPERCASE name, or of one of the lowercase names
;;   that held a platform number, whose value is an integer literal;
;;   a number some listed row prints `absent` read outside the definitions
;;   that run only on the platform that has it;
;;   a fixed-width native bytevector access inside the definitions that read
;;   or write a kernel struct (their fields go through row-uint-ref and its
;;   siblings), other than rss-freebsd's own sysctl name and length words;
;;   machine-kind outside the definitions that choose a mechanism with it.
(define (forms-of path)
  (call-with-input-file path
    (lambda (p) (let loop ((out '())) (let ((x (read p))) (if (eof-object? x) (reverse out) (loop (cons x out))))))))
(define (library-body forms)
  (apply append
         (map (lambda (f) (if (and (pair? f) (eq? (car f) 'library)) (cdddr f) (list f))) forms)))
(define (def-name f)
  (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
       (if (pair? (cadr f)) (caadr f) (cadr f))))
;; every subform of every top-level form, with the top-level definition's name
(define (walk-sites file)
  (let ((out '()))
    (for-each
      (lambda (top)
        (let ((name (def-name top)))
          (let visit ((x top))
            (when (pair? x)
              (set! out (cons (list file name x) out))
              (for-each visit (if (list? x) x (list (car x) (cdr x))))))))
      (library-body (forms-of file)))
    (reverse out)))
(define scanned-files '("../ffi.sc" "../daemon.sc" "../eval-supervise.sc" "../mcp/server.sc"))
(define sites (apply append (map walk-sites scanned-files)))
(define (upper-name? s)
  (let ((t (symbol->string s)))
    (and (> (string-length t) 0)
         (for-all (lambda (c) (or (char-upper-case? c) (char-numeric? c) (memv c '(#\_ #\-)))) (string->list t))
         (exists char-upper-case? (string->list t)))))
(define formerly-lowercase-numbers
  '(st-size-offset dirent-name-offset stat-buffer-size st-dev-offset st-ino-offset signal-term sigkill
    spawn-O_WRONLY spawn-O_CREAT spawn-O_TRUNC))
(define absent-somewhere
  (let ((names '()))
    (for-each (lambda (r)
                (let ((c (assq 'constants r)))
                  (for-each (lambda (e) (when (and (pair? e) (eq? (cadr e) 'absent) (not (memq (car e) names)))
                                          (set! names (cons (car e) names))))
                            (cdr c))))
              (platform-readings))
    names))
(define branch-definitions '(rss-darwin rss-freebsd path-case-sensitive? flush!))
(define struct-definitions
  '(st-mode-of path-device-inode size-entry sockaddr-un timeval-bytes rlimit-bytes getrlimit rss-darwin rss-freebsd))
(define mechanism-definitions '(process-rss-bytes spawn-detached! spawn-captured! path-case-sensitive? machine-kind))
(define fixed-width-accessors
  '(bytevector-u8-ref bytevector-u16-native-ref bytevector-u32-native-ref bytevector-u64-native-ref
    bytevector-s16-native-ref bytevector-s32-native-ref bytevector-s64-native-ref
    bytevector-u16-native-set! bytevector-u32-native-set! bytevector-u64-native-set! bytevector-u8-set!))
(define (site-finding s)
  (let ((file (car s)) (name (cadr s)) (x (caddr s)))
    (cond
      ((not (list? x)) #f)
      ((and (eq? (car x) 'case) (pair? (cdr x))
            (or (eq? (cadr x) 'platform-os) (equal? (cadr x) '(machine-kind))))
       (list 'case-on-platform file name))
      ((and (eq? (car x) 'eq?) (memq 'platform-os x) (not (eq? name 'macos?)))
       (list 'eq-platform-os file name))
      ((and (eq? (car x) 'define) (pair? (cdr x)) (symbol? (cadr x)) (pair? (cddr x)) (symbol? (cadr x))
            (or (upper-name? (cadr x)) (memq (cadr x) formerly-lowercase-numbers))
            (let ((v (caddr x))) (and (integer? v) (exact? v))))
       (list 'literal-number file (cadr x)))
      ((and (memq (car x) '(platform-number)) (pair? (cdr x)) (pair? (cadr x)) (eq? (car (cadr x)) 'quote)
            (memq (cadr (cadr x)) absent-somewhere) (not (memq name branch-definitions)))
       (list 'absent-number-outside-its-branch file name (cadr (cadr x))))
      ((and (memq (car x) fixed-width-accessors) (memq name struct-definitions)
            (not (and (eq? name 'rss-freebsd) (pair? (cdr x)) (memq (cadr x) '(mib len)))))
       (list 'fixed-width-struct-access file name (car x)))
      ((and (eq? (car x) 'machine-kind) (not (memq name mechanism-definitions)))
       (list 'machine-kind-outside-a-mechanism file name))
      (else #f))))
(want "PN-P6 the four files hold no platform number of their own, by every form the code used to use"
      (filter (lambda (f) f) (map site-finding sites))
      '())
(want "PN-P6 CONTROL: the scan sees the forms it looks for -- the platform-number calls it reads and the definitions it names"
      (list (and (> (length (filter (lambda (s) (eq? (car (caddr s)) 'platform-number)) sites)) 50) #t)
            (for-all (lambda (d) (and (exists (lambda (s) (eq? (cadr s) d)) sites) #t))
                     (append branch-definitions struct-definitions)))
      '(#t #t))

;; ---- P8: allocations are at least the row's sizes -------------------------------

(define ffi-sites (walk-sites "../ffi.sc"))
(define (defined-form name)
  (let ((s (find (lambda (s) (and (eq? (cadr s) name) (eq? (car (caddr s)) 'define) (eq? (def-name (caddr s)) name)))
                 ffi-sites)))
    (and s (caddr s))))
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
(printf "rows: ~a\n~a failures\nplatform-numbers complete\n" rows bad)
(exit (if (zero? bad) 0 1))
