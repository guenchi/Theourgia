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

;; WHAT A TEXT FIELD'S VALUE MAY BE, AND WHETHER A QUERY CAN REACH IT.
;;
;; Two questions that were found to be one question. The write path had no
;; rule for the VALUE of a text field: `batch` carries intents as data, so
;; `(set <id> title tinvor)` stored a symbol and `(set <id> title 42)` stored
;; a number, both accepted. Neither could ever be found, because
;; `field-strings` answers the empty list for anything that is not a string.
;;
;; And the same sentence explains the other half. A code block imported in
;; text mode holds its source as a BYTEVECTOR, which `field-strings` also
;; drops -- so "a text block's source is not searched" was never about
;; searching. It was the same `else` branch throwing the value away one step
;; earlier.
;;
;; So the table below is not only a rule about writing. Every kind it admits
;; is a kind a reader has to be able to turn into text, and the last section
;; of this file derives its cases FROM the table so that admitting a kind
;; without teaching the reader about it cannot pass quietly.

(import (chezscheme) (theourgia reduce) (theourgia crc32) (theourgia store))
(include "condition-render.ss")
(include "forge-record.ss")

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
    ((_ e0) (guard (e (#t (list 'RAISED (condition->text e)))) e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define (lines-of-count text)
  (let loop ((i 0) (n 0))
    (cond ((= i (string-length text)) (if (> i 0) (+ n 1) n))
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))

(define (holds? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(printf "\n== the table itself ==\n")

;; THE TABLE IS READ FROM THE PRODUCT, and only its SHAPE is stated here.
;; A row that restated the three entries would be a second copy of the rule
;; and would agree with itself after the real one changed.
(want "FT-1 every entry names a field and a non-empty list of kind symbols"
      (list (pair? text-field-types)
            (for-all (lambda (e)
                       (and (pair? e) (symbol? (car e))
                            (pair? (cdr e)) (list? (cdr e))
                            (for-all symbol? (cdr e))))
                     text-field-types))
      (list #t #t))

;; THE FIELDS THE RULE COVERS ARE THE FIELDS SEARCH READS AS TEXT. If those
;; two lists ever part company, one of them is wrong, and this row is where
;; that shows. The right-hand side is written out because it is a CLAIM
;; about `store-search`, not a restatement of the table.
(want "FT-2 the rule covers exactly the fields that are searched as text"
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                 (map car text-field-types))
      '(keywords src title))

(printf "\n== a value the rule refuses ==\n")

(define (set-reason field value)
  (caller-payload-reason (list 'set "b1.1" field value)))
(define (put-reason alist)
  (caller-payload-reason (list 'put alist)))

;; THE REFUSAL NAMES THE FIELD, THE KIND IT GOT AND WHAT IT WOULD TAKE.
;; A refusal that said only "bad value" would leave the writer to guess
;; which of three fields and which of two problems.
(want "FT-3 a symbol in a title is refused, and the refusal says what it got and what is allowed"
      (set-reason 'title 'tinvor)
      '(field-value-not-text (field title) (kind symbol) (allowed (string))))

;; THE ROW THE SPELLING OF THE RULE DEPENDS ON. A rule written as "symbols
;; are refused" passes FT-3 and fails here, which is why the rule is written
;; as what a value MAY be.
(want "FT-4 a number in a title is refused too, which a rule about symbols would have missed"
      (set-reason 'title 42)
      '(field-value-not-text (field title) (kind integer) (allowed (string))))

(want "FT-5 the same for src and keywords, each naming itself"
      (list (set-reason 'src 'raxmid) (set-reason 'keywords 'hufpar))
      (list '(field-value-not-text (field src) (kind symbol) (allowed (string bytevector)))
            '(field-value-not-text (field keywords) (kind symbol) (allowed (string)))))

(printf "\n== a value the rule accepts ==\n")

(want "FT-6 a string is accepted in all three"
      (list (set-reason 'title "a plain title")
            (set-reason 'src "some source")
            (set-reason 'keywords "one, two"))
      (list #f #f #f))

;; BYTES IN `src` ARE THE IMPORTER'S OWN WRITE. Refusing them would refuse
;; every code block imported in text mode; measured over this repository,
;; that is 43 of 43 file blocks.
(want "FT-7 a bytevector is accepted in src, because that is how text mode carries a source"
      (set-reason 'src (string->utf8 "some source"))
      #f)

(want "FT-8 TWIN: and a bytevector is NOT accepted in a title or in keywords"
      (list (and (set-reason 'title (string->utf8 "x")) #t)
            (and (set-reason 'keywords (string->utf8 "x")) #t))
      (list #t #t))

(printf "\n== the fields the rule must not reach ==\n")

;; THE TWIN FOR THE OTHER DIRECTION. `kind`, `lang` and `mode` are symbols
;; and `level` is an integer -- by design, and measured: every one of the
;; 3531 blocks in a markdown import stores `kind` as a symbol and `level` as
;; an integer. A type rule that reached them would refuse the importers.
(want "FT-9 a symbol in kind, lang or mode is not refused BY THIS RULE"
      (list (set-reason 'lang 'scheme) (set-reason 'mode 'text))
      (list #f #f))

(want "FT-10 an integer in level is not refused"
      (set-reason 'level 3)
      #f)

;; A FIELD NOBODY HAS A RULE FOR STAYS AS IT WAS. The table judges the
;; fields it lists and no others; this is what says the rule did not quietly
;; become a whitelist for every field.
(want "FT-11 a field the table does not list is not judged here at all"
      (set-reason 'invented-field 'whatever)
      #f)


;; THE SPELLING THAT LOOKS RIGHT AND IS NOT.
;;
;; A field entry only has to be a pair whose name is a symbol, so BOTH
;; `(title . "Keep")` and `(title "Keep")` pass the shape rule -- and they
;; mean different things. The second one's value is the LIST `("Keep")`,
;; and nothing normalised it: measured against the library as it stood
;; before this round, that block was stored as
;;
;;     (fields (kind . section) (title "Keep"))
;;
;; with a list where the title goes. `field-strings` drops a list exactly as
;; it dropped a symbol, so the block had a title that no query could reach
;; and no verb could print as text. It is the same defect as the symbol
;; case, reached by a typo instead of by a type.
;;
;; The rule refuses it now, which is a behaviour change: seventeen payloads
;; across three fixtures used this spelling while testing something else
;; entirely, and each of them was quietly writing an unsearchable title. No
;; code outside the fixtures wrote it -- both importers use dotted pairs,
;; measured over 3531 and 43 blocks.
(want "FT-17 the two-element spelling is refused, because its value is a list and not text"
      (put-reason '((kind . section) (title "Keep")))
      '(field-value-not-text (field title) (kind pair) (allowed (string))))

(want "FT-18 TWIN: and the dotted spelling of the same intent is accepted"
      (put-reason '((kind . section) (title . "Keep")))
      #f)

(printf "\n== the same rule on the other route into a block ==\n")

;; `put` CARRIES A WHOLE FIELD LIST, and the rule has to see each of them.
;; A repair that only taught `set` would leave `insert` and the importers
;; writing whatever they liked.
(want "FT-12 a put is judged field by field, and stops at the first that is wrong"
      (put-reason '((title . "fine") (keywords . badword)))
      '(field-value-not-text (field keywords) (kind symbol) (allowed (string))))

(want "FT-13 TWIN: a put whose text fields are all well typed is not refused by this rule"
      (put-reason '((title . "fine") (keywords . "also fine") (kind . section)))
      #f)

;; THE TITLE'S OWN RULE STILL RUNS, on the strings that now reach it. The
;; type rule is in front of it, not instead of it.
(want "FT-14 a title that is a string but holds a line terminator is still refused, by the older rule"
      (set-reason 'title "two\nlines")
      'title-has-line-terminator)

(printf "\n== every kind the rule admits must be reachable by a query ==\n")

;; THE CASES ARE DERIVED FROM THE TABLE, NOT LISTED HERE. Admitting a kind
;; in `text-field-types` without teaching the reading side about it has to
;; be loud, and the only way to make it loud is to walk the table.
;;
;; WHAT IS LOCAL TO THIS FILE is how to SPELL a value of a given kind in a
;; batch intent -- a predicate cannot produce a value. A kind with no
;; spelling here does not quietly skip: it fails by name, which is the
;; difference between a list that shouts about what it is missing and one
;; that is silent about it.

(define cli
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/core.sc"))
         (above (string-append dir "/../core.sc")))
    (cond ((file-exists? beside) beside)
          ((file-exists? above) above)
          (else (assertion-violation 'field-types "core.sc is nowhere beside this fixture"
                                     (list beside above))))))

(define here
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/field-types-" (number->string (get-process-id)))))
    (unless (char=? #\/ (string-ref path 0))
      (assertion-violation 'field-types "the test root must be absolute" root))
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'field-types "the test root may use only letters, digits, / . - and _" root)))
        (loop (+ i 1))))
    path))

(define home (string-append here "/home"))
(define out-path (string-append here "/out.txt"))
(system (string-append "rm -rf '" here "'; mkdir -p '" home "'"))
(putenv "THEOURGIA_HOME" home)

(define (text-of path)
  (if (not (file-exists? path))
      ""
      (call-with-input-file path
        (lambda (p)
          (let loop ((out '()))
            (let ((c (read-char p)))
              (if (eof-object? c) (list->string (reverse out)) (loop (cons c out)))))))))

(define (run* stdin store . args)
  (let ((cmd (string-append
               (if stdin (string-append "printf '%s' '" stdin "' | ") "")
               "env -u THEOURGIA_INJECT THEOURGIA_HOME='" home "' "
               "scheme --script '" cli "' "
               (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
               "--store '" store "' > '" out-path "' 2>&1")))
    (list (system cmd) (text-of out-path))))
(define (run store . args) (apply run* #f store args))

(define n-store 0)
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append here "/s" (number->string n-store))))
    (system (string-append "rm -rf '" d "'; mkdir -p '" d "'"))
    (run d "init")
    d))

;; The block id out of an answer, read as data rather than scraped: the
;; first quoted string in an `insert` answer is the writer, not the block.
(define (new-id text)
  (let* ((p (open-string-input-port text))
         (last (let loop ((prev #f))
                 (let ((d (read p)))
                   (if (eof-object? d) prev (loop d)))))
         (st (and (pair? last) (assq 'state (cdr last)))))
    (and st (pair? (cdr st)) (caar (cadr st)))))

;; HOW A VALUE OF EACH KIND IS SPELLED IN A BATCH INTENT, which carries its
;; intents as DATA -- so a bytevector is written as a bytevector literal and
;; arrives as one.
(define (spelling kind word)
  (case kind
    ((string) (string-append "\"" word "\""))
    ((bytevector)
     (string-append "#vu8("
                    (let loop ((l (bytevector->u8-list (string->utf8 word))) (out ""))
                      (if (null? l)
                          out
                          (loop (cdr l) (string-append out (if (string=? out "") "" " ")
                                                       (number->string (car l))))))
                    ")"))
    (else #f)))

;; ONE WORD PER CASE, pairwise non-substring. A row in this file's ancestor
;; searched `odd` while a keyword read `odd-keyword`, and the half meant to
;; show a miss was answered by the other half.
(define words
  '("tinvor" "pesgul" "raxmid" "qolnev" "hufpar" "dewzin" "kabrunt" "zolmiquy"))

(define (reachable? field kind word)
  (let* ((d (fresh-store!))
         (id (new-id (cadr (run d "insert" "--title" "a carrier block"))))
         (v (spelling kind word)))
    (cond
      ((not v) (list 'NO-SPELLING-FOR-KIND kind))
      ((not id) (list 'NO-BLOCK-ID))
      (else
        (let* ((intent (string-append "((set \"" id "\" " (symbol->string field) " " v "))"))
               (w (run* intent d "batch"))
               (accepted (and (= 0 (car w)) (not (holds? (cadr w) "error"))))
               (s (run d "search" word))
               (found (holds? (cadr s) id)))
          (list (if accepted 'written 'REFUSED-THE-WRITE)
                (if found 'found 'NOT-FOUND)))))))

;; The cases, walked off the table.
(define cases
  (let loop ((es text-field-types) (ws words) (out '()))
    (cond
      ((null? es) (reverse out))
      (else
        (let inner ((kinds (cdr (car es))) (ws ws) (acc out))
          (if (null? kinds)
              (loop (cdr es) ws acc)
              (inner (cdr kinds) (cdr ws)
                     (cons (list (car (car es)) (car kinds) (car ws)) acc))))))))

(want "FT-15 CONTROL: the cases come from the table, and there is one per admitted kind"
      (length cases)
      (apply + (map (lambda (e) (length (cdr e))) text-field-types)))

(for-each
  (lambda (c)
    (let ((field (car c)) (kind (cadr c)) (word (caddr c)))
      (want (string-append "FT-16 " (symbol->string field) " written as a "
                           (symbol->string kind) " is accepted AND findable")
            (reachable? field kind word)
            '(written found))))
  cases)

(printf "\n== replay is not judged, and that is what keeps an existing store readable ==\n")

;; THE MOST IMPORTANT ROWS IN THIS FILE.
;;
;; The rule lives on the CALLER path. If it reached replay as well, then
;; every store already holding a symbol-valued title -- written by any build
;; before this one, where nothing refused it -- would stop reducing on the
;; day this rule shipped. That is not a refused write; it is a person's
;; library becoming unreadable, and nothing they did caused it.
;;
;; So the same value is offered by both routes and the two answers are
;; compared. A row that only checked the refusal would pass just as well on
;; a build that refused both.
(define replay-store (fresh-store!))
(define replay-id (new-id (cadr (run replay-store "insert" "--title" "a carrier block"))))
(forge-record! replay-store (string-append "(set \"" replay-id "\" title symtitle)"))

(want "FT-19 a record carrying a symbol title still replays, and the block is still there"
      (let ((r (run replay-store "read" replay-id)))
        (list (car r) (holds? (cadr r) "symtitle")))
      (list 0 #t))

(want "FT-20 TWIN: and the same value offered by a caller is refused"
      (and (set-reason 'title 'symtitle) #t)
      #t)

;; AND THE STORE IS NOT POISONED BY IT. A block holding a value this build
;; would not have written still takes ordinary writes afterwards -- so the
;; owner can repair it instead of being stuck with it.
(want "FT-21 a later write still lands on a block whose field came from outside"
      (car (run replay-store "set" replay-id "title" "a proper title"))
      0)

;; WHY THE RULE REFUSES IT AT THE DOOR, stated as a reading rather than as
;; a claim: the value survives replay, and it is still not text, so no query
;; can reach it. Keeping it readable is not the same as making it findable.
(define unfindable-store (fresh-store!))
(define unfindable-id (new-id (cadr (run unfindable-store "insert" "--title" "another carrier"))))
(forge-record! unfindable-store (string-append "(set \"" unfindable-id "\" title kabrunt)"))

(want "FT-22 the replayed symbol is kept and readable, and no query reaches it"
      (list (holds? (cadr (run unfindable-store "read" unfindable-id)) "kabrunt")
            (holds? (cadr (run unfindable-store "search" "kabrunt")) unfindable-id))
      (list #t #f))

;; A VALUE WITH NO PRODUCER AT ALL. `(conflict 5)` is refused by the caller
;; path and is never built by the reducer, so a record is the only way it
;; can exist. This row says the reader survives one -- which is the hazard
;; a bad stored shape used to be: one field of one block taking down every
;; query in the store.
(define odd-store (fresh-store!))
(define odd-id (new-id (cadr (run odd-store "insert" "--title" "carrier three"))))
(define neighbour-id (new-id (cadr (run odd-store "insert" "--title" "zolmiquy neighbour"))))
(forge-record! odd-store (string-append "(set \"" odd-id "\" title (conflict 5))"))

(want "FT-23 a shape with no producer arrives by record, and search still answers about the rest"
      (let ((r (run odd-store "search" "zolmiquy")))
        (list (car r) (holds? (cadr r) neighbour-id)))
      (list 0 #t))

(printf "\n== bytes the writer accepts that the reader cannot turn into text ==\n")

;; THE ONE KIND THE TABLE ADMITS THAT MAY STILL YIELD NOTHING.
;;
;; `src` takes a bytevector because a text-mode source is stored as one. The
;; rule cannot ask whether those bytes are UTF-8 without decoding the whole
;; field, which is the reading side's work moved to the writing side -- so it
;; admits them, and the reader decides. Every row above handed the store
;; VALID bytes: FT-7 builds its value with `string->utf8` and FT-16's
;; generator produces only well-formed UTF-8, so nothing had ever offered
;; this store bad bytes.
;;
;; WHAT MAKES THIS DIFFERENT FROM A SYMBOL, and why the row says so: a symbol
;; in a text field is refused at the door, and a replayed one is dropped in
;; SILENCE. Bad bytes are accepted and dropped LOUDLY -- `text-decode-skipped`
;; moves. The count is part of the assertion because "it is not silent" is
;; the property that makes this acceptable rather than a hole.
(define bytes-store (fresh-store!))
(define bytes-id (new-id (cadr (run bytes-store "insert" "--title" "a carrier for bad bytes"))))
(define bytes-neighbour (new-id (cadr (run bytes-store "insert" "--title" "wexlom neighbour"))))
(define bad-bytes-write
  (run* (string-append "((set \"" bytes-id "\" src #vu8(255 254 253 104 105)))")
        bytes-store "batch"))

(want "FT-28 bytes that are not UTF-8 are accepted into src, and the block keeps them"
      (list (car bad-bytes-write)
            (holds? (cadr (run bytes-store "read" bytes-id)) "#vu8(255 254 253"))
      (list 0 #t))

;; THE SEARCH RUNS IN THIS PROCESS, because the counter lives in the library
;; and a subprocess takes its own copy of it away with it. The rows around
;; this one go through the command line; this one cannot.
(want "FT-29 no query reaches them, and the store says so rather than dropping them in silence"
      (let* ((before (text-decode-skipped-count))
             (hits (store-search bytes-store "hi"))
             (moved (- (text-decode-skipped-count) before)))
        (list (length hits) (> moved 0)))
      (list 0 #t))

;; CONTROL: the store is not broken by them, which is the hazard a bad field
;; used to be -- one value taking down every query.
(want "FT-30 CONTROL: search still answers about the other blocks"
      (let ((r (run bytes-store "search" "wexlom")))
        (list (car r) (holds? (cadr r) bytes-neighbour)))
      (list 0 #t))

(printf "\n== a conflict the store built itself, not one written by hand ==\n")

;; EVERY CONFLICT IN THESE FIXTURES WAS FORGED UNTIL NOW -- by the caller
;; path before the type rule, by `forge-record!` after it. None was made the
;; way the store makes one, which is two writers setting the same field
;; without having seen each other. A forged conflict asks whether the reader
;; survives a shape from outside; a real one asks whether a legitimate merged
;; value is still read as text. They are different questions, so this row
;; sits beside the forged ones rather than replacing them.
;;
(define conflict-store (fresh-store!))
(define conflict-id (new-id (cadr (run conflict-store "insert" "--title" "a contested block"))))

;; The second writer's record is untracked, so it carries no cursor -- it had
;; seen nothing, which is what makes the two writes concurrent rather than
;; ordered. Two records from the SAME writer would be an ordered history and
;; the later one would simply win; that was tried first and the field held
;; one word, not a conflict.
(forge-record! conflict-store
               (string-append "(set \"" conflict-id "\" keywords \"griffel\")"))
(forge-writer-record! conflict-store "zzforged"
               (string-append "(set \"" conflict-id "\" keywords \"slantwise\")"))

(want "FT-31 CONTROL: the field really holds a conflict the reducer built, with both candidates"
      (let ((r (run conflict-store "read" conflict-id)))
        (list (car r) (holds? (text-of out-path) "(keywords conflict")))
      (list 0 #t))

;; AND BOTH CANDIDATES ARE READ AS TEXT. This is the property the forged
;; rows cannot establish: that a value the store itself produced, in the
;; shape it produces, is still searchable on every candidate.
(want "FT-32 both candidates of a real conflict are searchable"
      (let ((a (run conflict-store "search" "griffel"))
            (b (run conflict-store "search" "slantwise")))
        (list (car a) (holds? (cadr a) conflict-id)
              (car b) (holds? (cadr b) conflict-id)))
      (list 0 #t 0 #t))

(printf "\n== a conflict inside a conflict ==\n")

;; NOT PREDICTED, AND IT KEEPS A CELL ANYWAY.
;;
;; The behaviour is deliberate: a conflict candidate that is not text is
;; dropped, like any other value that is not text, and it is not counted --
;; the counter means "a readable block has a field that is not text", and a
;; shape that does not fit is not the same statement.
;;
;; The row is here because of how the value is REACHED. Replay can put a
;; conflict value into a field as a LITERAL; an ordinary later write then
;; makes the reducer wrap that literal as one candidate among others. Nobody
;; arrives at that combination by thinking about it.
;;
;; And the branch that handles it is doing real work. Rewrite the candidate
;; loop to assume candidates are strings -- an easy thing to do while
;; simplifying -- and this raises, from inside a read, on a block that looks
;; ordinary. Three assertions in one row: it does not raise, there is no
;; text, and the counter does not move.
(define nested-store (fresh-store!))
(define nested-id (new-id (cadr (run nested-store "insert" "--title" "a nested carrier"))))
(define nested-neighbour (new-id (cadr (run nested-store "insert" "--title" "quandel neighbour"))))
;; TWO WRITERS, for the same reason as the row above: the first version used
;; two records from one writer, which is an ordered history -- the second
;; write simply replaced the first and there was no nesting at all. The row
;; passed anyway, because a plain string behaves exactly as the row expects
;; a nested conflict to. A mutation that made the candidate loop assume
;; strings was what showed it: that mutation should have made this row
;; raise, and it survived.
(forge-record! nested-store
  (string-append "(set \"" nested-id "\" keywords (conflict ((\"tinvor\" \"x\" 1) (\"pesgul\" \"y\" 1))))"))
(forge-writer-record! nested-store "zznested"
  (string-append "(set \"" nested-id "\" keywords \"plainword\")"))

(want "FT-33 CONTROL: the field really holds a conflict whose candidate is itself a conflict"
      ;; THREE OPEN PARENS, NOT TWO. A candidate is `(<value> <writer> <seq>)`,
      ;; so a candidate whose VALUE is itself a conflict reads as
      ;; `(conflict (((conflict ...) "w" 3) ("plainword" "zznested" 1)))`.
      ;; The first version of this pattern looked for two and failed while
      ;; the construction was working perfectly.
      (let ((r (run nested-store "read" nested-id)))
        (list (car r) (holds? (text-of out-path) "(keywords conflict (((conflict")))
      (list 0 #t))

(want "FT-34 a conflict nested inside a conflict does not raise, has no text, and is not counted"
      (let* ((before (text-decode-skipped-count))
             (hit-nested (store-search nested-store "tinvor"))
             (hit-other (store-search nested-store "quandel"))
             (unchanged (= (text-decode-skipped-count) before)))
        (list (length hit-nested)
              (length hit-other)
              (if (pair? hit-other) (car (car hit-other)) 'NO-HIT)
              unchanged))
      (list 0 1 nested-neighbour #t))

(printf "\n== a request that is replayed is answered before the rule is asked ==\n")

;; WHY THIS SECTION EXISTS. `batch` accepts what a replay brings it -- that
;; was settled before this round -- and a rule added to the write path must
;; not quietly change it. A request whose receipt is already on the disk has
;; to be answered FROM the receipt, without its intents being run again, and
;; therefore without them being judged again.
;;
;; WHAT THESE ROWS CAN AND CANNOT BUILD. The interesting case is a receipt
;; recorded by an OLDER build, for a request that wrote a symbol when nothing
;; refused one. A fixture cannot make that: the request's identity includes a
;; digest of its own payload -- the row below measures that -- so forging such
;; a receipt means computing the digest, which is guessing at an envelope in
;; order to test what is inside it. That case was measured instead against the
;; pinned r7 delivery, by writing the store with that library and reading it
;; with this one, and the five readings are in the delivery notes. What is
;; HERE is the part a fixture can own: the replay path answers before the rule
;; is asked, and the rule does apply to a request that is not a replay.
(define req-store (fresh-store!))
(define req-id (new-id (cadr (run req-store "insert" "--title" "a carrier block"))))
(define req-writer
  (let loop ((i 0))
    (cond ((>= i (string-length req-id)) req-id)
          ((char=? (string-ref req-id i) #\.) (substring req-id 0 i))
          (else (loop (+ i 1))))))
(define (tracked! rid cursor-seq intents)
  (run* intents req-store "batch" "--req" rid
        "--cursor" (string-append req-writer ":" (number->string cursor-seq))))

(define first-send
  (tracked! "r-once" 1 (string-append "((set \"" req-id "\" title \"a legal title\"))")))
(define second-send
  (tracked! "r-once" 1 (string-append "((set \"" req-id "\" title \"a legal title\"))")))

(want "FT-24 the same request sent twice is answered from its receipt the second time"
      (list (car first-send)
            (car second-send)
            (holds? (cadr second-send) "(replay #t)"))
      (list 0 0 #t))

;; AND NOTHING RAN THE SECOND TIME. A replay that re-executed and happened to
;; be idempotent would read the same as one that did not, so the row asks the
;; log how many events the store holds rather than asking the answer.
(want "FT-25 and the second send added no event, so its intents were never run"
      ;; THE COUNT IS ASKED BEFORE AND AFTER A SEND KNOWN TO BE A REPLAY, and
      ;; the row asks that the count did not MOVE rather than that it equals
      ;; a particular number -- a number written here would have to be
      ;; rewritten whenever a row above this one writes one more record, and
      ;; the claim is about the difference.
      ;;
      ;; It also asks that the count is not zero. A reader that returned zero
      ;; for everything would satisfy "unchanged" perfectly.
      (let ((before (lines-of-count (cadr (run req-store "log")))))
        (tracked! "r-once" 1 (string-append "((set \"" req-id "\" title \"a legal title\"))"))
        (let ((after (lines-of-count (cadr (run req-store "log")))))
          (list (> before 0) (= before after))))
      (list #t #t))

;; THE CONTROL. Without it, every row above would pass on a build whose rule
;; had simply stopped working.
(want "FT-26 CONTROL: a request that is NOT a replay is judged, and refused by name"
      (let ((r (tracked! "r-fresh" 2 (string-append "((set \"" req-id "\" title symtitle))"))))
        (holds? (cadr r) "field-value-not-text"))
      #t)

;; AND A REQUEST IDENTITY COVERS ITS PAYLOAD. This is the row that says why
;; the older-build case cannot be built here: the same request id carrying
;; different intents is a mismatch, not a replay, so a receipt cannot be
;; borrowed for work it was not issued for.
(want "FT-27 the same request id with different intents is a mismatch, not a replay"
      (let ((r (tracked! "r-once" 1 (string-append "((set \"" req-id "\" title \"something else\"))"))))
        (holds? (cadr r) "req-mismatch"))
      #t)

(system (string-append "rm -rf '" here "'"))

(printf "rows: ~a\n~a failures\nfield-types complete\n" rows bad)
(exit (if (zero? bad) 0 1))
