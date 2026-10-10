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

;; Two rules about characters. Neither needs to know where a comment ends.
;;
;; RULE 1  no CJK script ANYWHERE in a code file. This library is published
;;         and its comments are written in English.
;; RULE 2  no emoji ANYWHERE in any regular file.
;;
;; KEY: RULE 1 USED TO BE ABOUT COMMENTS, AND THAT WAS THE MISTAKE. Asking
;; "is this line a comment" needs lexical analysis, and three rounds of
;; review found a further case every time: nested block comments, an opener
;; that is not at the start of a line, a C line comment continued over an
;; escaped newline, files whose lines end in CR, extensions the syntax table
;; did not list. This file's own opening warned that a rule needing a
;; scanner that knows where a string ends would be the seventh partial
;; reader of Scheme in this directory -- and the answer to that warning was
;; to build one.
;;
;; So rule 1 now uses the shape that makes rule 2 cheap: a class with no
;; legitimate use in these files. Measured across the whole tree, exactly
;; ONE line in a code file holds CJK outside a comment, and it is test data.
;; It is named below. A future line that genuinely needs Chinese must be
;; added to that list BY HAND, which is the point: the exception is visible
;; and passes through someone's judgement, instead of resting on a scanner
;; that guesses where a comment ended.
;;
;; KEY: THE GATE'S ERROR MAY ONLY FALL ON THE STRICT SIDE. A wrong refusal
;; is acceptable; a missed violation is not. Rule 1 will refuse Chinese in a
;; string, and that is the intended direction.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list (quote RAISED)))) e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

;; NOTE: WHAT THE FLOOR DOES AND DOES NOT ESTABLISH. It catches a walk that
;; found nothing or nearly nothing. NEVER: IT DOES NOT ESTABLISH THAT EVERY
;; FILE WAS EXAMINED -- a walk that silently dropped twenty files would
;; still clear it. The row that injects a production walker skipping a
;; directory is what measures that direction; complete coverage is not
;; something this gate proves, and saying so is better than implying it.
(define floor-files 200)

;; KEY: ONE PLACE DECIDES THE FLOOR, and the production row and both twins all
;; call it. The first version had each twin recompute the comparison itself,
;; and a review measured what that cost: replacing the production comparison
;; with #t left BOTH twins passing, so the rows added to prove the floor was
;; real could not see it switched off. A cell that restates the thing it is
;; meant to check is checking its own copy.
(define (floor-verdict n)
  (if (>= n floor-files) (quote looked) (list (quote scanned) n (quote floor) floor-files)))

(define (cleared-floor? n) (eq? (floor-verdict n) (quote looked)))

;; NEVER: EXCEPTIONS ARE NAMED, ONE LINE EACH, WITH A REASON. Adding one is
;; an edit somebody makes on purpose.
;;
;; KEY: AN EXCEPTION IS A FILE AND THE EXACT TEXT OF ONE LINE, NOT A LINE
;; NUMBER (F86). Keyed by number, any line inserted above the datum moved
;; it off the excepted number and turned ASCII-2 red, and a renumbering
;; was never checked against what the line says. Keyed by content, the
;; line may move anywhere in its file and stays excepted; if its text
;; changes by one character it is refused like any other. The CJK
;; character is written as an escape so that this file stays ASCII.
(define exceptions
  (list
    ;; The fixture writes a note whose text is deliberately not ASCII, to
    ;; show that a note's bytes survive the round trip unchanged. The
    ;; Chinese is the test datum itself.
    (cons "test/client-start.sc"
          "    (lambda (port) (display \"(note \x5B58;)\\n\" port)))")))

;; NEVER: THE EXCEPTION IS THE ONE PATH NAMED, NOT ANY PATH THAT ENDS IN IT.
;; The first version compared suffixes, and a review found it exempting
;; vendor/test/client-start.sc and contest/client-start.sc as well: anyone
;; adding a path whose last characters happened to match inherited the
;; excepted line's permission to hold Chinese. The walk is rooted at ".." so a path arrives
;; as "../test/client-start.sc"; that one prefix is stripped, and what is
;; left must equal the named path exactly.
(define (root-relative path)
  (if (and (>= (string-length path) 3) (string=? (substring path 0 3) "../"))
      (substring path 3 (string-length path))
      path))

(define (excepted? path text)
  (let ((rel (root-relative path)))
    (let loop ((xs exceptions))
      (and (pair? xs)
           (or (and (string=? text (cdr (car xs))) (string=? rel (car (car xs))))
               (loop (cdr xs)))))))

;; NOTE: BLOCKS ARE NAMED SO THAT ADDING ONE IS A DECISION, and every block
;; here has a character planted in the rows below: remove a block and a
;; name disappears from the expected list.
(define (cjk-script? c)
  (let ((n (char->integer c)))
    (or (= n #x3007)
        (and (>= n #x3040) (<= n #x30FF))
        (and (>= n #x31F0) (<= n #x31FF))
        (and (>= n #x2E80) (<= n #x2EFF))
        (and (>= n #x2F00) (<= n #x2FDF))
        (and (>= n #x3100) (<= n #x312F))
        (and (>= n #x3400) (<= n #x4DBF))
        (and (>= n #x4E00) (<= n #x9FFF))
        (and (>= n #x1100) (<= n #x11FF))
        (and (>= n #x3130) (<= n #x318F))
        (and (>= n #xAC00) (<= n #xD7AF))
        (and (>= n #xF900) (<= n #xFAFF))
        (and (>= n #xFE30) (<= n #xFE4F))
        (and (>= n #xFF00) (<= n #xFFEF))
        (and (>= n #x1AFF0) (<= n #x1B16F))
        (and (>= n #x20000) (<= n #x3FFFF)))))

(define (emoji-char? c)
  (let ((n (char->integer c)))
    (or (and (>= n #x2300) (<= n #x23FF))
        (and (>= n #x25A0) (<= n #x27BF))
        (and (>= n #x2900) (<= n #x297F))
        (and (>= n #x2B00) (<= n #x2BFF))
        (and (>= n #x1F000) (<= n #x1FAFF))
        (= n #x00A9) (= n #x00AE) (= n #x2122)
        (= n #x203C) (= n #x2049) (= n #x2139)
        (and (>= n #x2194) (<= n #x2199))
        (= n #x3030) (= n #x303D) (= n #x3297) (= n #x3299)
        (= n #xFE0F) (= n #x200D) (= n #x20E3))))

;; NEVER: A FAILED READ IS A FAILURE, AND SO IS TEXT THAT IS NOT UTF-8.
(define (text-of path)
  (let* ((tc (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))
         (p (open-file-input-port path (file-options) (buffer-mode block) tc)))
    (let ((t (guard (e (#t (close-port p) (raise e))) (get-string-all p))))
      (close-port p)
      (if (string? t) t ""))))

;; KEY: BOTH RULES NOW COVER EVERY FILE THE WALK READS. Rule 1 used to apply
;; only to files whose extension appeared in a list here, and a review showed
;; what that left out: `// .js`, `.ts`, `.cpp` and every extensionless script
;; passed, and so did this tree's own README.md, docs and .txt files -- the
;; markdown being the one place the project most deliberately keeps English,
;; since its Chinese manuals were moved out of the published repository on
;; purpose. Round three said it had removed the class of failures that needed
;; an extension table; that was true of the COMMENT SYNTAX table and false of
;; the reach, because the list survived here. An extension can still decide
;; how to read a file, but rule 1 no longer asks anything about comments, so
;; nothing is left for it to decide.

;; NEVER: A NAME IS SKIPPED ONLY WHEN IT IS A DIRECTORY, and only these
;; three. The earlier version tested the name before the entry's type, so a
;; REGULAR FILE called __pycache__ was skipped too. The policy these three
;; share: each holds machine-generated or vendored content that this tree
;; does not author and does not publish. Anything else is examined.
(define (skip-dir? name)
  (or (string=? name ".git")
      (string=? name "node_modules")
      (string=? name "__pycache__")))

(define links-skipped 0)

;; NOTE: LINKS ARE SKIPPED AND COUNTED. The tree holds `theourgia -> .`,
;; the self link that lets `(theourgia foo)` resolve when the tree is its
;; own library root; following it made an early version scan 7491 files
;; instead of 228. Counting is what keeps "skipped" from reading as "was
;; not there".
(define (walk-skipping skip-name)
  (lambda (dir yield)
    (let walk ((dir dir))
      (for-each
        (lambda (entry)
          (let ((path (string-append dir "/" entry)))
            (cond
              ((file-symbolic-link? path) (set! links-skipped (+ links-skipped 1)))
              ((file-directory? path)
               (unless (or (skip-dir? entry) (and skip-name (string=? entry skip-name)))
                 (walk path)))
              (else (yield path)))))
        (list-sort string<? (directory-list dir))))))

(define walk (walk-skipping #f))

;; NOTE: LINES ARE SPLIT ON CR AND LF BOTH, so a file written with either
;; ending is counted the same way. A review found the earlier version
;; splitting only on LF.
(define (split-lines t)
  (let loop ((i 0) (start 0) (out (quote ())))
    (cond
      ((>= i (string-length t)) (reverse (cons (substring t start i) out)))
      ((or (char=? (string-ref t i) #\newline) (char=? (string-ref t i) #\return))
       (let ((skip (if (and (char=? (string-ref t i) #\return)
                            (< (+ i 1) (string-length t))
                            (char=? (string-ref t (+ i 1)) #\newline))
                       2 1)))
         (loop (+ i skip) (+ i skip) (cons (substring t start i) out))))
      (else (loop (+ i 1) start out)))))

(define (any-char? pred s)
  (let loop ((i 0))
    (and (< i (string-length s))
         (or (pred (string-ref s i)) (loop (+ i 1))))))

;; KEY: THE WALKER IS AN ARGUMENT, so a row can hand in one that finds
;; nothing, or the production one with a directory removed, and point it at
;; the REAL tree.
(define (scan-with walker root)
  (let ((scanned 0) (emoji (quote ())) (cjk (quote ())) (unreadable (quote ())))
    (walker root
      (lambda (path)
        (let ((t (guard (e (#t (quote unreadable))) (text-of path))))
          (cond
            ((eq? t (quote unreadable)) (set! unreadable (cons path unreadable)))
            (else
             (set! scanned (+ scanned 1))
             ;; NEVER: ONE EXCEPTION EXCUSES ONE LINE. Keyed by content, an
             ;; exception would otherwise pass every copy of its line in
             ;; the file; the line-number key allowed exactly one. The
             ;; first occurrence is excepted and every later copy is
             ;; refused like any other CJK line.
             (let loop ((ls (split-lines t)) (n 1) (used (quote ())))
               (unless (null? ls)
                 (when (any-char? emoji-char? (car ls))
                   (set! emoji (cons (cons path n) emoji)))
                 (let* ((cjk? (any-char? cjk-script? (car ls)))
                        (free? (and cjk? (excepted? path (car ls))
                                    (not (member (car ls) used)))))
                   (when (and cjk? (not free?))
                     (set! cjk (cons (cons path n) cjk)))
                   (loop (cdr ls) (+ n 1) (if free? (cons (car ls) used) used))))))))))
    (list scanned emoji cjk unreadable)))

(define (scan root) (scan-with walk root))

(define (report label hits)
  (printf "~a: ~a\n" label (length hits))
  (let loop ((h (list-sort (lambda (a b) (string<? (car a) (car b))) hits)) (shown 0))
    (unless (or (null? h) (>= shown 12))
      (printf "   ~a:~a\n" (car (car h)) (cdr (car h)))
      (loop (cdr h) (+ shown 1))))
  (when (> (length hits) 12) (printf "   ... and ~a more\n" (- (length hits) 12))))

;; ---- the tree ----------------------------------------------------------------

(define result (scan ".."))
(define scanned (car result))

(printf "scanned ~a files under .. (floor ~a), skipped ~a symlinks, ~a named exception(s)\n"
        scanned floor-files links-skipped (length exceptions))
(report "lines holding an emoji" (cadr result))
(report "lines holding CJK script" (caddr result))
(when (pair? (cadddr result))
  (printf "files that could not be read as UTF-8: ~a\n" (length (cadddr result)))
  (for-each (lambda (p) (printf "   ~a\n" p)) (cadddr result)))

(want "ASCII-0 the gate looked at the tree at all"
      (floor-verdict scanned)
      (quote looked))
(want "ASCII-1 no line in this tree holds an emoji" (length (cadr result)) 0)
(want "ASCII-2 no line in this tree holds CJK script, but for the named exceptions"
      (length (caddr result)) 0)
(want "ASCII-3 every file read as UTF-8" (length (cadddr result)) 0)

;; ---- the gate, on cases where it must refuse ---------------------------------

(want "ASCII-0 TWIN: a walk that yields nothing is refused even on the real tree"
      (let ((r (scan-with (lambda (dir yield) (if #f #f)) "..")))
        (list (car r) (if (cleared-floor? (car r)) (quote PASSED-THE-FLOOR) (quote refused))))
      (list 0 (quote refused)))

;; NEVER: AND THE PRODUCTION WALKER IS MUTATED TOO, not only replaced by a
;; stub. A walker that quietly drops one directory is the shape a real
;; defect would take, and the row above cannot see it.
(want "ASCII-0 TWIN: the production walker with one directory removed sees fewer files, and the floor refuses"
      (let ((r (scan-with (walk-skipping "test") "..")))
        (list (< (car r) scanned)
              (if (cleared-floor? (car r)) (quote PASSED-THE-FLOOR) (quote refused))))
      (list #t (quote refused)))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/asciigate-" (number->string (get-process-id))))
(define (fresh! name)
  (let ((d (string-append here "/" name)))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    d))
(define (put! path text)
  (call-with-output-file path (lambda (p) (display text p))))

;; KEY: ONE CHARACTER FROM EVERY BLOCK, ONE PER LINE, AND THE ROW NAMES THE
;; LINES. Remove a block from either predicate and a NAME disappears. An
;; earlier version planted one character per rule, and a review showed that
;; narrowing the emoji class to that single character still passed.
(let ((d (fresh! "blocks")))
  (put! (string-append d "/e.sc")
        (string-append
          ";; 1 \x23F0;\n"        ;; 2300-23FF
          ";; 2 \x25FE;\n"        ;; 25A0-27BF
          ";; 3 \x2934;\n"        ;; 2900-297F
          ";; 4 \x2B50;\n"        ;; 2B00-2BFF
          ";; 5 \x1F600;\n"       ;; 1F000-1FAFF
          ";; 6 \xA9;\n"          ;; copyright
          ";; 7 \xAE;\n"          ;; registered
          ";; 8 \x2122;\n"        ;; trade mark
          ";; 9 \x203C;\n"        ;; double exclamation
          ";; 10 \x2049;\n"       ;; exclamation question
          ";; 11 \x2139;\n"       ;; information
          ";; 12 \x2194;\n"        ;; 2194-2199 arrows, unqualified
          ";; 13 \x3030;\n"        ;; wavy dash, unqualified
          ";; 14 \x303D;\n"        ;; part alternation mark, unqualified
          ";; 15 \x3297;\n"        ;; congratulations, unqualified
          ";; 16 \x3299;\n"        ;; secret, unqualified
          ";; 17 clean\n"))
  (put! (string-append d "/c.sc")
        (string-append
          ";; 1 \x3007;\n"        ;; ideographic number zero
          ";; 2 \x30A2;\n"        ;; kana
          ";; 3 \x31F0;\n"        ;; kana phonetic extensions
          ";; 4 \x3400;\n"        ;; extension A
          ";; 5 \x4E00;\n"        ;; unified
          ";; 6 \xF900;\n"        ;; compatibility
          ";; 7 \xFE30;\n"        ;; compatibility forms
          ";; 8 \xFF21;\n"        ;; fullwidth
          ";; 9 \x1B000;\n"       ;; kana supplement
          ";; 10 \x20000;\n"      ;; extension B
          ";; 11 \x2E80;\n"        ;; CJK radicals supplement
          ";; 12 \x2F00;\n"        ;; Kangxi radicals
          ";; 13 \x3105;\n"        ;; Bopomofo
          ";; 14 \xAC00;\n"        ;; Hangul syllables
          ";; 15 \x1100;\n"        ;; Hangul Jamo
          ";; 16 \x3131;\n"        ;; Hangul compatibility Jamo
          ";; 17 clean\n"))
  (let* ((r (scan d))
         (named (lambda (hits)
                  (list-sort string<?
                    (map (lambda (h) (string-append (car h) ":" (number->string (cdr h)))) hits)))))
    ;; NOTE: BOTH SIDES ARE SORTED THE SAME WAY. Written with the expected
    ;; list in numeric order, this row failed while every block WAS found:
    ;; the two lists held the same names and `:10` sorts before `:2`. The
    ;; scanner was right and the row's ordering was wrong -- a difference
    ;; in presentation reading as a difference in result.
    (want "ASCII TWIN: one character from every block of both classes is named"
          (list (car r) (named (cadr r)) (named (caddr r)) (length (cadddr r)))
          (list 2
                (named (map (lambda (n) (cons (string-append d "/e.sc") n))
                            (quote (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))
                (named (map (lambda (n) (cons (string-append d "/c.sc") n))
                            (quote (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))
                0))))

;; NEVER: A SKIPPED NAME IS A DIRECTORY'S NAME. The earlier walker tested
;; the name before the entry's type, so a REGULAR FILE called __pycache__
;; was skipped as well -- a name anyone can give a file, and the gate would
;; not have looked inside it. The directory of that name is still skipped,
;; and this row asks for both answers at once.
(let ((d (fresh! "skipped-names")))
  (system (string-append "mkdir -p " d "/__pycache__ " d "/sub"))
  (put! (string-append d "/__pycache__/inside.txt") ";; \x1F600;\n")
  (put! (string-append d "/sub/__pycache__") ";; \x1F600;\n")
  (want "ASCII TWIN: a regular file named __pycache__ is examined, a DIRECTORY of that name is not"
        (let ((r (scan d)))
          (list (car r) (map car (cadr r))))
        (list 1 (list (string-append d "/sub/__pycache__")))))

;; NEVER: A SPECIMEN FOR THE EXCEPTION IS SCANNED AS THE TREE IS SCANNED,
;; from a directory one level down with the root "..", so a path arrives as
;; "../test/client-start.sc" exactly as in the production walk. Scanned
;; from its absolute name, no specimen can ever match the named path: a
;; row that should be refused for its text is then refused for its path,
;; and stays green with the text rule removed (measured), while a row
;; that should pass is red for a reason that has nothing to do with the
;; line.
(define (scan-as-tree d)
  (system (string-append "mkdir -p " d "/scan-from"))
  (parameterize ((current-directory (string-append d "/scan-from")))
    (scan "..")))

;; NEVER: THE EXEMPTION BELONGS TO ONE PATH. A review replaced the exact
;; comparison with a suffix one and with a bare line test, and the round
;; that was supposed to cover the exception could not tell: it only ever
;; asked about the named file. These ask about paths that END in the named
;; one and about an unrelated file -- all must be refused, so either
;; mutation turns this row red.
;;
;; KEY: EACH SPECIMEN CARRIES THE EXCEPTED CONTENT. The exemption is keyed
;; by the line's text (F86), so a specimen holding some other CJK line
;; would be refused under both mutations and could not tell them apart:
;; the row has to ask its question where the answer differs. (When the key
;; was a line number the same rule put the CJK on that number.)
(define excepted-text (cdr (car exceptions)))
(define (filler n)
  (let loop ((i 0) (out (quote ())))
    (if (= i n) (apply string-append out) (loop (+ i 1) (cons ";; filler\n" out)))))
(let ((d (fresh! "exception-neighbours")))
  (system (string-append "mkdir -p " d "/vendor/test " d "/contest"))
  (for-each (lambda (rel) (put! (string-append d "/" rel)
                                (string-append (filler 16) excepted-text "\n")))
            (quote ("vendor/test/client-start.sc" "contest/client-start.sc" "unrelated.sc")))
  (want "ASCII-2 TWIN: the excepted line's content, in a path merely ending in the excepted one, is still refused"
        (length (caddr (scan-as-tree d))) 3))


;; NEVER: AND A NAMED EXCEPTION IS THE ONLY WAY A CJK LINE PASSES.
(let ((d (fresh! "exception")))
  (system (string-append "mkdir -p " d "/test"))
  (put! (string-append d "/test/client-start.sc")
        (string-append (filler 16) ";; \x4E2D;\n"))
  (want "ASCII-2 TWIN: a CJK line in the excepted file whose content is not the excepted line is refused"
        (length (caddr (scan-as-tree d))) 1))

;; F86: THE EXCEPTION FOLLOWS ITS LINE, AND NOTHING ELSE. Both rows start
;; from the real client-start.sc, so they ask about the line as it is.
;;
(define client-start-text
  (call-with-input-file "client-start.sc" get-string-all))
(define (replace-first text old new)
  (let ((n (string-length old)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) old)
             (string-append (substring text 0 i) new (substring text (+ i n) m)))
            (else (loop (+ i 1)))))))
(let ((d (fresh! "exception-moved")))
  (system (string-append "mkdir -p " d "/test"))
  (put! (string-append d "/test/client-start.sc")
        (string-append ";; one line inserted above everything\n" client-start-text))
  (want "F86-1 client-start.sc with one line inserted above the datum still passes: the exception moved with its line"
        (let ((r (scan-as-tree d))) (list (car r) (caddr r)))
        (list 1 (quote ()))))
(let ((d (fresh! "exception-copied")))
  (system (string-append "mkdir -p " d "/test"))
  (put! (string-append d "/test/client-start.sc")
        (string-append client-start-text excepted-text "\n"))
  (want "F86-3 a second copy of the excepted line in client-start.sc is refused: the exception excuses one line"
        (map (lambda (h) (root-relative (car h))) (caddr (scan-as-tree d)))
        (list "test/client-start.sc")))
(let ((d (fresh! "exception-changed")))
  (system (string-append "mkdir -p " d "/test"))
  (put! (string-append d "/test/client-start.sc")
        (or (replace-first client-start-text excepted-text
                           (or (replace-first excepted-text "\x5B58;" "\x5728;") "NO-CJK-IN-THE-EXCEPTION"))
            "NO-EXCEPTED-LINE-IN-CLIENT-START"))
  (want "F86-2 client-start.sc whose excepted line's content changes by one character fails, naming the file"
        (map (lambda (h) (root-relative (car h))) (caddr (scan-as-tree d)))
        (list "test/client-start.sc")))

(let ((d (fresh! "unreadable")))
  (put! (string-append d "/locked.sc") ";; ordinary\n")
  (system (string-append "chmod 000 " d "/locked.sc"))
  (let ((r (scan d)))
    (system (string-append "chmod 644 " d "/locked.sc"))
    (want "ASCII-3 TWIN: a file that cannot be read is reported, not skipped"
          (list (car r) (length (cadddr r)))
          (list 0 1))))

(let ((d (fresh! "notutf8")))
  (system (string-append "printf 'A\\377B\\n' > " d "/bytes.sc"))
  (let ((r (scan d)))
    (want "ASCII-3 TWIN: bytes that are not UTF-8 are unreadable, not replaced"
          (list (car r) (length (cadddr r)))
          (list 0 1))))

(let ((d (fresh! "crlf")))
  (system (string-append "printf ';; one\\r\\n;; \\344\\270\\255\\r\\n' > " d "/w.sc"))
  (want "ASCII-2 TWIN: a file whose lines end in CRLF is read line by line"
        (map cdr (caddr (scan d)))
        (quote (2))))

(system (string-append "rm -rf " here))

(printf "rows: ~a\n~a failures\nascii-only complete\n" rows bad)
(exit (if (zero? bad) 0 1))
