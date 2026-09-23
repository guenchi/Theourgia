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

;; `keywords`: a field a writer chooses to be found by.
;;
;; NEVER: THE FIELD IS TEXT AND STAYS TEXT. What the caller wrote is what
;; `read` gives back -- spacing, commas and all. The splitting into
;; tokens happens in `search`, which is the only reader that needs them;
;; a store holding a normalised form could not give back what was sent.

(import (chezscheme))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/f2-" (number->string (get-process-id))))
(define store (string-append here "/store"))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (quoted a)
  (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                                 (string->list a))) "'"))

(define (cli . args)
  (let ((out (string-append here "/out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../cli.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " --wire > " out " 2>&1"))
    (file-text out)))

(define (cli-plain . args)
  (let ((out (string-append here "/plain.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../cli.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " > " out " 2>&1"))
    (file-text out)))

(define (index-of text needle from)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i from))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))
(define (contains? text needle) (and (index-of text needle 0) #t))

(define (id-of answer)
  (let ((at (index-of answer "(state ((\"" 0)))
    (and at (let scan ((j (+ at 10)) (acc '()))
              (cond ((>= j (string-length answer)) #f)
                    ((char=? (string-ref answer j) #\") (list->string (reverse acc)))
                    (else (scan (+ j 1) (cons (string-ref answer j) acc))))))))

(system (string-append "rm -rf " here "; mkdir -p " store))
(cli "init")

;; ---- K5: what goes in is what comes out ---------------------------------------
;;
;; NEVER: BYTE FOR BYTE, INCLUDING THE SPACES. An implementation that split
;; the value into a list, trimmed it, or lower-cased it would pass a row
;; that only asked "are the words there".
(define odd-keywords "  Alpha, \x3b2; ,\x6c49;\x5b57;  ")
(define k5-id (id-of (cli "insert" "--title" "K5" "--text" "prose" "--keywords" odd-keywords)))

;; NOTE: THE ANSWER IS READ AS A DATUM, not compared as text. The wire may
;; write a non-ASCII character raw or as `\x3B2;`, and this fixture's own
;; `write` may choose the other one -- comparing the two renderings
;; compares the printers, not the field. Reading gives the string back
;; whichever way it travelled, which is also what makes this row survive
;; the change to raw UTF-8 on the wire.
(define (field-of answer name)
  (let* ((datum (with-input-from-string answer read))
         (body (and (pair? datum) (eq? 'ok (car datum)) (cadr datum)))
         (fields (and (pair? body) (assq 'fields body)))
         (e (and fields (assq name (cdr fields)))))
    (and e (cdr e))))

(want "K5 --keywords stores exactly the string it was given"
      (let ((got (field-of (cli "read" k5-id) 'keywords)))
        (if (equal? got odd-keywords) 'byte-for-byte (list 'said got)))
      'byte-for-byte)

;; NEVER: AND `set` REPLACES THE INDEX, NOT ONLY THE FIELD. An implementation
;; that indexed the words at insert time and never revisited them would
;; keep answering for the old ones.
(cli "set" k5-id "keywords" "replacement")

(want "K5 after set, the new word is found and the old one is not"
      (list (if (contains? (cli "search" "replacement") k5-id) 'new-word-hits (list 'said (cli "search" "replacement")))
            (if (contains? (cli "search" "Alpha") k5-id) 'OLD-WORD-STILL-HITS 'old-word-gone))
      '(new-word-hits old-word-gone))

;; ---- K1/K2: the scoring, against the rule in store.sc --------------------------
;;
;; NOTE: THE TWINS ARE THE ROW. A single "it was found" passes for an
;; implementation that scores keywords like source, or that turns the
;; query into an OR across tokens.
(define k1-id (id-of (cli "insert" "--title" "unrelated" "--text" "nothing here"
                          "--keywords" "ConCATenate, MAT")))

;; NEVER: A FIELD'S TIER IS THE BEST ANY TOKEN REACHES IN IT, and this block
;; carries TWO keywords. `cat` is buried inside `ConCATenate`, but `mat`
;; begins `MAT` just after the comma -- a word boundary -- so the keywords
;; field reaches the upper tier and scores 4 rather than the 3 it scored when
;; every field had a single value.
;;
;; The prediction made before this round said 3, on the reading that both
;; tokens were mid-word. One of them is not. The row below is the one that
;; tests the prediction that WAS right: a block whose keyword hit is only ever
;; mid-word still scores exactly what it scored before tiers existed.
(want "K1 a word only in keywords is found, and a keyword that BEGINS with a token scores 4"
      (let ((r (cli "search" "cAt mAt")))
        (list (if (contains? r (string-append "(hit \"" k1-id "\" 4 ")) 'scored-4 (list 'said r))
              (if (contains? r "\"ConCATenate, MAT\"") 'snippet-is-the-keywords 'WRONG-SNIPPET)))
      '(scored-4 snippet-is-the-keywords))

;; NEVER: AND THE TARGET HAS TO BE MID-WORD IN EVERY SENSE. The keyword above
;; is spelled `ConCATenate` to show that matching ignores case, and that
;; spelling puts a capital exactly where the token begins -- which the tier
;; rule reads as a camelCase seam, correctly, and scores 4. Measured: the
;; first version of this row used that same word and got 4 where it wanted 3.
;; The floor needs a word with no seam in it at all.
(define midword-id
  (id-of (cli "insert" "--title" "plain heading" "--keywords" "concatenate")))

(want "K1 TIER FLOOR: a keyword hit that is only mid-word scores what it scored before tiers"
      (let ((r (cli "search" "cat")))
        (if (contains? r (string-append "(hit \"" midword-id "\" 3 ")) 'scored-3-as-before
            (list 'said r)))
      'scored-3-as-before)

(want "K1 TWIN: every token must still hit, so a token that matches nothing gives nothing"
      (let ((r (cli "search" "cat absent")))
        (if (contains? r "(items)") 'no-hits (list 'said r)))
      'no-hits)

(define k2-id (id-of (cli "insert" "--title" "cat" "--text" "x" "--keywords" "mat")))

(want "K2 a title hit and a keywords hit add up, each at its own tier"
      (let ((r (cli "search" "cat mat")))
        (if (contains? r (string-append "(hit \"" k2-id "\" 7 ")) 'scored-7 (list 'said r)))
      'scored-7)

;; ---- K7: a keyword that is not ASCII ------------------------------------------
(define k7-id (id-of (cli "insert" "--title" "seven" "--text" "y" "--keywords" "\x6c49;\x5b57;, tail")))

(want "K7 a keyword outside ASCII is searchable"
      (if (contains? (cli "search" "\x6c49;\x5b57;") k7-id) 'found (list 'said (cli "search" "\x6c49;\x5b57;")))
      'found)

;; ---- K3: the listing, when it is asked ----------------------------------------
;;
;; NOTE: BOTH PRINTERS. `outline-text` draws the tree in one place and the
;; `orphans:` rows in another; a suffix added to one of them only is the
;; kind of thing a single-row cell does not see.
;; NOTE: TWO PARENTS, BECAUSE ONE OF THEM HAS TO SURVIVE. `del` is how an
;; orphan is made here -- a block whose parent is deleted -- and an
;; earlier version of this fixture deleted the only keyworded parent it
;; had, then asserted the tree still listed it. The tree row and the
;; orphan row need different blocks.
(define k3-parent (id-of (cli "insert" "--title" "Parent" "--keywords" "top, level")))
(define k3-victim (id-of (cli "insert" "--title" "Victim")))
(define k3-child (id-of (cli "insert" "--under" k3-victim "--title" "Child" "--keywords" "under, here")))
(cli "del" k3-victim)

(want "K3 --with-keywords prints them, and a block without the field prints no brackets"
      (let ((listing (cli-plain "outline" "--with-keywords")))
        (list (if (contains? listing "[top, level]") 'parent-has-them (list 'said listing))
              (if (contains? listing "[under, here]") 'orphan-row-has-them-too 'ONE-PRINTER-ONLY)
              (if (contains? listing "K5  []") 'EMPTY-BRACKETS 'no-empty-brackets)))
      '(parent-has-them orphan-row-has-them-too no-empty-brackets))

(want "K3 the depth limit and the suffix work together"
      (let ((listing (cli-plain "outline" "--depth" "1" "--with-keywords")))
        (if (contains? listing "[top, level]") 'depth-limited-rows-still-carry-them
            (list 'said listing)))
      'depth-limited-rows-still-carry-them)

;; ---- K4: and without the option, nothing moved --------------------------------
;;
;; NEVER: THE SHAPE IS FROZEN, NOT THE IDS. Block ids are generated, so a
;; byte comparison against a recorded listing would be a comparison
;; against a coin toss; the ids are replaced by a fixed marker and what
;; is compared is every other byte -- which is where a suffix, a changed
;; separator or a stray bracket would show.
(define (normalise-ids text)
  (let loop ((i 0) (out '()))
    (cond
      ((>= i (string-length text)) (list->string (reverse out)))
      ((and (char=? (string-ref text i) #\-)
            (< (+ i 2) (string-length text))
            (char=? (string-ref text (+ i 1)) #\space)
            (not (char=? (string-ref text (+ i 2)) #\space)))
       ;; "- <id>  " -> "- ID  "
       (let scan ((j (+ i 2)))
         (cond ((or (>= j (string-length text)) (char=? (string-ref text j) #\space))
                (loop j (append (list #\D #\I #\space #\-) out)))
               (else (scan (+ j 1))))))
      (else (loop (+ i 1) (cons (string-ref text i) out))))))

(want "K4 TWIN: the listing without the option carries no keywords at all"
      (let ((plain (cli-plain "outline")))
        (list (if (contains? plain "[") 'BRACKETS-BY-DEFAULT 'no-brackets)
              (if (contains? plain "top, level") 'KEYWORDS-BY-DEFAULT 'no-keywords)
              (if (contains? (normalise-ids plain) "- ID  Parent") 'same-row-shape
                  (list 'said (normalise-ids plain)))))
      '(no-brackets no-keywords same-row-shape))

;; ---- K6: a store written before the field existed ------------------------------
;;
;; NOTE: EVERY BLOCK HERE HAS NO `keywords`, which is what every store
;; written before this batch looks like.
(define old-store (string-append here "/old"))
(system (string-append "mkdir -p " old-store))
(define (old-cli . args)
  (let ((out (string-append here "/old.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../cli.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " old-store " > " out " 2>&1"))
    (file-text out)))
(old-cli "init")
(old-cli "insert" "--title" "Plain" "--text" "cat sat")

(want "K6 a store with no keywords anywhere searches and lists as it did"
      (list (if (contains? (old-cli "search" "cat") "hit") 'search-works (list 'said (old-cli "search" "cat")))
            (if (contains? (old-cli "outline") "[") 'BRACKETS 'listing-unchanged)
            (if (contains? (old-cli "outline" "--with-keywords") "[") 'BRACKETS-FROM-NOWHERE
                'and-asking-for-them-adds-nothing))
      '(search-works listing-unchanged and-asking-for-them-adds-nothing))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\nf2-keywords complete\n" rows bad)
(exit (if (zero? bad) 0 1))
