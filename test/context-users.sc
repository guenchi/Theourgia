#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; `context` lists the users of the names its block's unit defines, and a
;; provisional id asked as a block is answered as one.
;;
;; U: (users (listed <k>) (of <n>) [(skipped ...)] (user <id> (name <n>)
;; (library <lib>|none) (mode <m>)) ...), after background, from the
;; name-use table `uses` reads over the request's own view: for each name a
;; block of T's unit defines, the blocks outside the unit that use it, by
;; name then id. Admitted after the hard material, between the dependents
;; and the relevant blocks.
;; P: an id of a provisional id's exact shape answers (error provisional-id
;; (id <id>) (reason not-committed)) wherever an id is resolved over a view
;; (rpc.sc's unknown-id, working-read), whoever asks, with or without
;; --working; any other shape stays unknown-id with its nearest ids.
;;
;; THE STORE. File F holds alpha (defines alpha) and gamma (defines gamma,
;; uses alpha); file G holds ga (uses alpha and beta) and du (a datum block
;; using alpha); library L (in file LF) holds lu (uses alpha and gamma);
;; file H holds lonely (defines lonely, used by nobody), solo (defines
;; solo), su (the only use of solo) and plain (uses nothing of ours). The
;; text blocks are JavaScript: in a text block a definition is a use of its
;; own name too, as `uses` says, which is why gamma uses gamma. lu, which
;; uses both of F's names, has a lower id than du: by name then id, lu is
;; listed under alpha before du and again under gamma after it.
(import (chezscheme) (theourgia rpc)
        (only (theourgia render) render-wire)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-block-ids block-id provisional-id)
        (only (theourgia working) working-new!)
        (only (theourgia render) render-human)
        (only (theourgia context) users-clause reserved-users excluded-clause reserved-excluded))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/context-users-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; ---- the store ------------------------------------------------------------------------------

(define S (string-append root "/s"))
(define W (cadr (assq 'writer (cdr (rpc-dispatch S '(init) "test")))))
(define (run . args) (rpc-dispatch S args "test"))
(define (run-as w . args) (rpc-dispatch S args "test" #f w))
(define (state) (open-and-reduce S))
(define (field-of id f)
  (let ((r (state-read (state) id)))
    (and r (let ((e (assq f (cdr (assq 'fields r))))) (and e (cdr e))))))
(define (titled t) (find (lambda (id) (equal? (field-of id 'title) t)) (state-block-ids (state))))
;; A file block at the top, by the insert verb (a batch inserts under a block).
(define (new-id answer)
  (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define (file! title)
  (let ((id (new-id (run 'insert "--under" "root" "--title" title))))
    (run 'set id "kind" "file")
    id))
;; A block inserted by batch, found again by its title (titles are unique here).
(define (insert! parent fields)
  (let ((a (run 'batch (format "((insert ~s #f ~s))" parent fields))))
    (or (titled (cdr (assq 'title fields))) (error 'insert! "not inserted" a))))
(define (js parent title name src)
  (insert! parent (append (list '(kind . code) '(lang . "javascript") (cons 'title title) (cons 'src src))
                          (if name (list (cons 'name (string->symbol name))) '()))))

(define F (file! "F"))
(define alpha (js F "alpha" "alpha" "function alpha() { return 1 }"))
(define gamma (js F "gamma" "gamma" "function gamma() { return alpha() }"))
(define G (file! "G"))
(define ga (js G "ga" #f "alpha(); beta()"))
(define LF (file! "LF"))
(define L (insert! LF '((kind . library) (mode . datum) (lang . chez) (title . "L") (name . (lib l)) (imports . ((rnrs))))))
(define lu (js L "lu" #f "alpha(); gamma()"))
(define du (insert! G '((kind . code) (mode . datum) (lang . chez) (title . "du") (body . (alpha 1)))))
(define H (file! "H"))
(define lonely (js H "lonely" "lonely" "function lonely() { return 0 }"))
(define solo (js H "solo" "solo" "function solo() { return 2 }"))
(define su (js H "su" #f "solo()"))
(define plain (js H "plain" #f "var q = 0"))

;; ---- reading an answer --------------------------------------------------------------------------

(define (ctx for budget . flags)
  (apply run 'context "--for" for "--budget" (if (number? budget) (number->string budget) budget) flags))
(define (ctx-as w for budget . flags)
  (apply run-as w 'context "--for" for "--budget" (if (number? budget) (number->string budget) budget) flags))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (users a) (clause a 'users))
;; the clause's counts and skipped, without the users
(define (users-head a)
  (let ((u (users a))) (and u (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'user)))) u))))
(define (users-list a)
  (let ((u (users a))) (and u (filter (lambda (x) (and (pair? x) (eq? (car x) 'user))) (cdr u)))))
(define (user id name lib . mode) (list 'user id (list 'name name) (list 'library lib) (list 'mode (if (pair? mode) (car mode) 'text))))
;; alpha's users outside a unit, as entries
(define (alpha-users . ids)
  (map (lambda (id) (cond ((equal? id lu) (user id "alpha" L)) ((equal? id du) (user id "alpha" 'none 'datum)) (else (user id "alpha" 'none))))
       (apply by-id ids)))
(define (by-id . ids) (list-sort string<? ids))
(define (clause-names a) (map car (filter pair? (cdr a))))
(define (section-ids a name) (let ((c (clause a name))) (and c (map car (cadr c)))))

;; ---- U1: the users of a name a block defines, with library and mode -----------------------------------

(define a-alpha (ctx alpha 100000))
(want "U1 alpha's users are every block outside its unit that uses alpha, by id: gamma (alpha's own file, outside the unit of one block), ga (no library), lu (library L) and du (a datum block)"
      (users-list a-alpha)
      (alpha-users gamma ga lu du))
(want "U1 the clause says it listed all of them, and it stands right after background"
      (list (users-head a-alpha)
            (let ((ns (clause-names a-alpha))) (cadr (memq 'background ns))))
      (list '(users (listed 4) (of 4)) 'users))
(want "U1 the users are what `uses alpha` answers, the unit's own block left out"
      (map cadr (users-list a-alpha))
      (filter (lambda (id) (not (equal? id alpha)))
              (map cadr (cdr (cadr (run 'uses "alpha"))))))
(want "U1 users are not premises and not versions: the receipt and the versions name none of them unless shown"
      (let ((shown (apply append (map (lambda (s) (or (section-ids a-alpha s) '())) '(constraints evidence to-verify counterexamples background))))
            (versions (map car (cadr (clause a-alpha 'versions))))
            (premises (map cadr (filter (lambda (p) (string? (cadr p))) (cdr (clause a-alpha 'receipt))))))
        (list (filter (lambda (id) (and (member id versions) (not (member id shown)) (not (equal? id alpha)))) (list gamma ga lu))
              (filter (lambda (id) (member id premises)) (list ga lu))))
      '(() ()))
(want "U1 the notes are the five queries' only: no users note"
      (filter (lambda (n) (and (pair? n) (eq? (car n) 'users))) (cadr (clause a-alpha 'notes)))
      '())

;; ---- U2: a name nobody uses ----------------------------------------------------------------------

(define a-lonely (ctx lonely 100000))
(want "U2 a block whose name no one uses: (users (listed 0) (of 0)) and no user entries, the clause still there"
      (users a-lonely)
      '(users (listed 0) (of 0)))
(want "U2 a block that defines nothing (ga): the clause is there, empty"
      (users (ctx ga 100000))
      '(users (listed 0) (of 0)))

;; ---- U3: a use inside T's own unit is not a user ------------------------------------------------------

(define a-F (ctx F 100000))
(want "U3 for the file F, whose unit holds alpha and gamma (T itself defines nothing): gamma's use of alpha is inside the unit and is not listed; alpha's users outside are ga, lu and du, gamma's lu; by name then id, so lu twice and du between"
      (users a-F)
      (append '(users (listed 4) (of 4)) (alpha-users ga lu du) (list (user lu "gamma" L))))
(want "U3 the same gamma is a user when T is alpha alone (its unit is alpha)"
      (and (member gamma (map cadr (users-list a-alpha))) #t)
      #t)

;; ---- U4: the working view's table under --working ----------------------------------------------------

;; writer uadd drafts plain to use solo; writer urem drafts su, solo's only
;; use, to use nothing of ours.
(define d-add (run 'write plain "solo(); var q = 0" "--writer" "uadd"))
(define d-rem (run 'write su "var z = 1" "--writer" "urem"))
(want "U4 the two drafts were written"
      (list (car d-add) (car d-rem))
      '(ok ok))
(want "U4 a draft that adds a use in another block: under --working that block is listed"
      (users (ctx-as "uadd" solo 100000 "--working"))
      (append '(users (listed 2) (of 2)) (map (lambda (id) (user id "solo" 'none)) (by-id su plain))))
(want "U4 a draft that removes the only use: under --working none is listed"
      (users (ctx-as "urem" solo 100000 "--working"))
      '(users (listed 0) (of 0)))
(define d-ren (run 'write alpha "function alphb() { return 1 }" "--writer" "uren"))
;; THE NAMES ARE THE VIEW'S DEF ROWS, and the view keeps a block's stored
;; name field: a draft replaces src only. So a draft that renames alpha's
;; text leaves the unit defining alpha, and its users are still listed.
;; Pinned as the view answers it; a view that derived the name from the
;; drafted text would change this row, not the users clause.
(want "U4 a draft that renames the definition's text: the view's def rows keep the stored name alpha, so alpha's users are listed under --working as without it"
      (list (car d-ren) (users-head (ctx-as "uren" alpha 100000 "--working")) (users-head (ctx-as "uren" alpha 100000)))
      '(ok (users (listed 4) (of 4)) (users (listed 4) (of 4))))
(want "U4 without --working the committed table answers, for either writer: su alone"
      (list (users (ctx-as "uadd" solo 100000)) (users (ctx-as "urem" solo 100000)))
      (let ((c (list 'users '(listed 1) '(of 1) (user su "solo" 'none)))) (list c c)))

;; ---- U5: the users go after the hard material ------------------------------------------------------

;; THE SMALLEST BUDGET AT WHICH EVERY HARD BLOCK OF F IS SHOWN: from the
;; reported minimum up, one token at a time. There every hard block is in,
;; nothing is missing, and no user fits.
(define F-min (let ((a (run 'context "--for" F "--budget" "1"))) (and (eq? (car a) 'error) (cadr (caddr a)))))
(define F-all-hard
  (and F-min
       (let loop ((b F-min))
         (let ((a (ctx F b)))
           (cond ((> b (+ F-min 2000)) #f)
                 ((and (eq? (car a) 'ok) (not (clause a 'insufficient))) b)
                 (else (loop (+ b 1))))))))
(want "U5 a smallest budget that shows every hard block of F exists above the minimum"
      (and F-all-hard (> F-all-hard F-min) #t)
      #t)
(want "U5 at it: every hard block is shown, the users are dropped and said so, (listed 0) (of 2), and counted excluded"
      (let ((a (ctx F F-all-hard)))
        (list (clause a 'insufficient) (users a)
              (let ((e (clause a 'excluded))) (and e (assq 'users (cdr e))))))
      '(#f (users (listed 0) (of 4)) (users 4)))
(want "U5 the excluded clause names users between dependents and relevant"
      (map car (cdr (clause (ctx F F-all-hard) 'excluded)))
      '(definitions evidence counterexamples dependents users relevant ancestors))
(want "U5 with room the same request lists all four"
      (users-head (ctx F 100000))
      '(users (listed 4) (of 4)))
;; A PARTIAL LIST: from the all-hard budget up, the first budget that lists
;; some users but not all. The first in order are the ones listed, the rest
;; are counted excluded, and the answer's used is its own rendered size.
(define F-partial
  (let loop ((b F-all-hard))
    (let* ((a (ctx F b)) (h (users-head a)))
      (cond ((> b (+ F-all-hard 2000)) #f)
            ((and h (> (cadr (cadr h)) 0)) (cons b a))
            (else (loop (+ b 1)))))))
(define (answer-bytes a)
  (+ 1 (string-length (let ((w (render-wire (filter (lambda (c) (not (and (pair? c) (eq? (car c) 'cut)))) a))))
                        (substring w 0 (- (string-length w) 1))))))
(want "U5 a budget that lists some: listed k of 4 with 0 < k < 4, the first k in order, and excluded users 4 - k"
      (let* ((a (cdr F-partial)) (k (cadr (cadr (users-head a)))))
        (list (< 0 k 4) (users-list a) (cadr (assq 'users (cdr (clause a 'excluded))))
              k))
      (let* ((a (cdr F-partial)) (k (cadr (cadr (users-head a)))))
        (list #t (list-head (append (alpha-users ga lu du) (list (user lu "gamma" L))) k) (- 4 k) k)))
(want "U5 its used is its rendered size in tokens, and that size fits the budget (ten times the bytes within thirty-six times the tokens)"
      (let* ((b (car F-partial)) (a (cdr F-partial)) (n (answer-bytes a)) (bud (clause a 'budget)))
        (list (cadr (assq 'used (cdr bud))) (<= (* 10 n) (* 36 b))))
      (let* ((a (cdr F-partial)) (n (answer-bytes a))) (list (div (+ n 3) 4) #t)))

;; ---- U10: a soft count elsewhere does not move the hard material ----------------------------------

;; ONE TOKEN ABOVE THE SMALLEST BUDGET THAT SHOWS EVERY HARD BLOCK OF F, a
;; block the table cannot read (a language with no catalogue entry) is
;; added far from F. The users clause is reserved at the counters' digits,
;; so the same request shows the same hard blocks at the same levels with
;; the same receipt; only the users clause (its skipped count), the used
;; number and the cut differ. Reserved at its own digits, the new skipped
;; clause would take the room a hard entry had there.
(define (without-soft a)
  (filter (lambda (c) (not (and (pair? c) (memq (car c) '(users budget cut))))) a))
(define u10-b (+ F-all-hard 1))
(define u10-before (ctx F u10-b))
(define klingon (insert! H '((kind . code) (lang . "klingon") (title . "k") (src . "qapla alpha"))))
(define u10-after (ctx F u10-b))
(want "U10 an unreadable block added elsewhere: the same hard blocks at the same levels and the same receipt; only the users clause gains its skipped count"
      (list (equal? (without-soft u10-before) (without-soft u10-after))
            (clause u10-before 'insufficient)
            (users-head u10-before) (users-head u10-after))
      '(#t #f (users (listed 0) (of 4)) (users (listed 0) (of 4) (skipped (no-language 1)))))

;; ---- U6: the human rendering -------------------------------------------------------------------

(define (lines text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text)) (reverse (if (< start i) (cons (substring text start i) out) out)))
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))
(want "U6 the human rendering prints users with its counts on its line, then one line per user: id, name, library, mode"
      (let* ((ls (lines (render-human a-alpha))) (at (member "users (listed 4) (of 4)" ls)))
        (and at (list-head at 5)))
      (cons "users (listed 4) (of 4)"
            (map (lambda (id) (format "  ~a alpha ~a ~a" id (if (equal? id lu) L "none") (if (equal? id du) "datum" "text")))
                 (by-id gamma ga lu du))))

;; ---- U7: what the table skips is said as `uses` says it ------------------------------------------------

(want "U7 a block of a language with no catalogue entry is skipped: the users clause carries uses' own skipped clause"
      (let ((u (users (ctx alpha 100000))) (s (find (lambda (c) (and (pair? c) (eq? (car c) 'skipped))) (cdr (run 'uses "alpha")))))
        (list (find (lambda (c) (and (pair? c) (eq? (car c) 'skipped))) (cdr u)) s))
      '((skipped (no-language 1)) (skipped (no-language 1))))
(want "U7 the skipped block is not listed as a user"
      (member klingon (map cadr (users-list (ctx alpha 100000))))
      #f)

;; ---- P: a provisional id ---------------------------------------------------------------------------

;; A NEW FILE DRAFTED BY uadd, through the working library as edit-sync
;; makes one.
(define new-path "drafted.js")
(define pid (provisional-id new-path))
(define drafted (working-new! S #f "uadd" new-path '((lang . javascript)) (string->utf8 "alpha()\n") #f))
(define refusal (list 'error 'provisional-id (list 'id pid) '(reason not-committed)))
(want "P0 the new-file draft was written, under its provisional id"
      (list (car drafted) (and (member pid (map (lambda (d) (cadr (assq 'provisional (cdr d))))
                                                (filter (lambda (d) (and (pair? d) (eq? (car d) 'new)))
                                                        (cdr (cadr (run-as "uadd" 'drafts))))))
                               #t))
      '(ok #t))
(define (target a) (and (clause a 'for) (car (cadr (clause a 'for)))))
(want "C1 context --for <provisional> --working, by the writer that drafted it: its working view holds the holder the commit would make, the target"
      (let ((a (ctx-as "uadd" pid 100000 "--working"))) (list (car a) (target a)))
      (list 'ok pid))
(want "C2 the same without --working answers provisional-id"
      (ctx-as "uadd" pid 100000)
      refusal)
(want "C3 read <provisional> --working, by that writer: the holder reads as a committed file holder reads"
      (run-as "uadd" 'read pid "--working")
      '(ok (text "")))
(want "C7 for that writer, read --working and context --working of the new file's piece read the piece"
      (list (run-as "uadd" 'read (string-append pid "/0") "--working")
            (target (ctx-as "uadd" (string-append pid "/0") 100000 "--working")))
      (list '(ok (text "alpha()\n")) (string-append pid "/0")))
(want "C4 an id of nearly the shape is still unknown-id with nearest: 55 and 57 digits, an uppercase digit, a lowercase non-hex letter, a dash among the digits, another prefix"
      (map (lambda (id) (let ((a (run 'read id))) (list (cadr a) (and (assq 'nearest (filter pair? (cdr a))) #t))))
           (list (substring pid 0 59) (string-append pid "0")
                 (string-append "new-A" (substring pid 5 60))
                 (string-append "new-g" (substring pid 5 60))
                 (string-append "new--" (substring pid 5 60))
                 (string-append "neu-" (substring pid 4 60))))
      '((unknown-id #t) (unknown-id #t) (unknown-id #t) (unknown-id #t) (unknown-id #t) (unknown-id #t)))
(want "C5 another writer gets the same answer, with and without --working, from read and context"
      (list (run-as "urem" 'read pid "--working") (run-as "urem" 'read pid)
            (ctx-as "urem" pid 100000 "--working") (ctx-as "urem" pid 100000))
      (list refusal refusal refusal refusal))
(want "C5 a provisional id nobody drafted answers the same: the drafts are not consulted"
      (run 'read (provisional-id "never-drafted.js"))
      (list 'error 'provisional-id (list 'id (provisional-id "never-drafted.js")) '(reason not-committed)))
(want "C6 the human rendering prints the refusal as the wire does"
      (render-human refusal)
      (format "~s\n" refusal))
(want "C8 other verbs that resolve an id over a view answer it alike: names and uses --under"
      (list (run 'names pid) (run 'uses "alpha" "--under" pid))
      (list refusal refusal))

;; ---- U9: what the clause costs, by number -----------------------------------------------------------

;; THE MINIMUM BUDGET IS ASSERTED AS A NUMBER, so the reservation's cost is
;; visible: the users clause at its full count and the excluded users
;; counter, the clause reserved at the counters' digits whatever its counts
;; are (so this number no longer depends on U10's unreadable block). On
;; master (ded1d15) the same request at the same point answered 399 tokens;
;; with the clause reserved at its own digits (freeze1) it was 418. The
;; insufficient clause's reduced list, reserved at one item for the one
;; hard block, takes it from 432 (bd78e7f) to 452.
(want "U9 lonely's minimum budget is 452 tokens (399 on master, 418 with the clause at its own digits, 432 with the clause at the counters' digits and the users counter; reduced's one item costs 20)"
      (run 'context "--for" lonely "--budget" "1")
      '(error budget-too-small (minimum 452)))

;; ---- U11: counts at and above the cap ---------------------------------------------------------------

;; NO STORE IN A FIXTURE REACHES THE CAP, so the clause is built directly
;; (context.sc exports users-clause and reserved-users for these rows). A
;; count above 999999 is shown at it and the clause says (capped), as the
;; excluded counters do; the clause's head, its user entries left out, never
;; prints longer than the reservation.
(define cap 999999)
(define one-user (user "x.1" "n" 'none))
(define (head-of c) (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'user)))) c))
(define (wire-chars x) (string-length (render-wire x)))
(define (cap-case k n skipped) (head-of (users-clause (make-list k one-user) n skipped)))
(define cap-cases
  (list (list 'listed-and-of-at-cap (cap-case cap cap #f))
        (list 'of-above (cap-case 0 (+ cap 1) #f))
        (list 'listed-and-of-above (cap-case (+ cap 1) (+ cap 1) #f))
        (list 'no-language-at-cap (cap-case 0 1 (list 'skipped (list 'no-language cap))))
        (list 'no-language-above (cap-case 0 1 (list 'skipped (list 'no-language (+ cap 1)))))
        (list 'unreadable-code-above (cap-case 0 1 (list 'skipped (list 'unreadable-code (+ cap 1)))))
        (list 'both-at-cap (cap-case 0 1 (list 'skipped (list 'no-language cap) (list 'unreadable-code cap))))
        (list 'both-above (cap-case (+ cap 1) (+ cap 1) (list 'skipped (list 'no-language (+ cap 1)) (list 'unreadable-code (+ cap 1)))))))
(want "U11 counts at the cap are shown as they are, with no marker: listed and of, one skipped kind, both"
      (map cadr (filter (lambda (c) (memq (car c) '(listed-and-of-at-cap no-language-at-cap both-at-cap))) cap-cases))
      (list '(users (listed 999999) (of 999999))
            '(users (listed 0) (of 1) (skipped (no-language 999999)))
            '(users (listed 0) (of 1) (skipped (no-language 999999) (unreadable-code 999999)))))
(want "U11 a count above the cap is shown at it and the clause says (capped): of alone, listed and of, either skipped kind, both"
      (map cadr (filter (lambda (c) (memq (car c) '(of-above listed-and-of-above no-language-above unreadable-code-above both-above))) cap-cases))
      (list '(users (listed 0) (of 999999) (capped))
            '(users (listed 999999) (of 999999) (capped))
            '(users (listed 0) (of 1) (skipped (no-language 999999)) (capped))
            '(users (listed 0) (of 1) (skipped (unreadable-code 999999)) (capped))
            '(users (listed 999999) (of 999999) (skipped (no-language 999999) (unreadable-code 999999)) (capped))))
(want "U11 no head prints longer than the reservation, and the largest is exactly it"
      (list (for-all (lambda (c) (<= (wire-chars (cadr c)) (wire-chars reserved-users))) cap-cases)
            (equal? (cadr (assq 'both-above cap-cases)) reserved-users))
      '(#t #t))

;; ---- U12: the excluded counters at and above the cap ------------------------------------------------

;; THE EXCLUDED CLAUSE IS BUILT DIRECTLY too (context.sc exports
;; excluded-clause and reserved-excluded for these rows): the counters in
;; their order, each count shown at most at the cap, a zero shown as 0, and
;; (capped) when any count is above the cap; no clause prints longer than
;; the reservation.
(define excluded-order '(definitions evidence counterexamples dependents users relevant ancestors))
(define (ex-counts . ns) (map cons excluded-order ns))
(define (ex-shown . ns) (cons 'excluded (map list excluded-order ns)))
(define excluded-cases
  (list (list 'all-zero (excluded-clause (ex-counts 0 0 0 0 0 0 0)))
        (list 'below (excluded-clause (ex-counts 17 0 3 0 0 2 1)))
        (list 'all-at-cap (excluded-clause (ex-counts cap cap cap cap cap cap cap)))
        (list 'first-above (excluded-clause (ex-counts (+ cap 1) 0 0 0 0 0 0)))
        (list 'last-above (excluded-clause (ex-counts 0 cap 0 0 0 0 (+ cap 1))))
        (list 'all-above (excluded-clause (ex-counts (+ cap 1) (+ cap 1) (+ cap 1) (+ cap 1) (+ cap 1) (+ cap 1) (* 1000 cap))))))
(define (excluded-case name) (cadr (assq name excluded-cases)))
(want "U12 counts at the cap are shown as they are, with no marker: all seven at the cap, and counts below it"
      (map excluded-case '(all-at-cap below))
      (list (ex-shown cap cap cap cap cap cap cap) (ex-shown 17 0 3 0 0 2 1)))
(want "U12 a count above the cap is shown at it and the clause says (capped): the first counter, the last (beside one at the cap), all"
      (map excluded-case '(first-above last-above all-above))
      (list (append (ex-shown cap 0 0 0 0 0 0) '((capped)))
            (append (ex-shown 0 cap 0 0 0 0 cap) '((capped)))
            (append (ex-shown cap cap cap cap cap cap cap) '((capped)))))
(want "U12 no clause prints longer than the reservation, and the all-above one is exactly it"
      (list (for-all (lambda (c) (<= (wire-chars (cadr c)) (wire-chars reserved-excluded))) excluded-cases)
            (equal? (excluded-case 'all-above) reserved-excluded))
      '(#t #t))
(want "U12 a zero count is shown as 0, not left out"
      (excluded-case 'all-zero)
      (ex-shown 0 0 0 0 0 0 0))

;; ---- U8: an outside user changing moves the users, not the receipt ------------------------------------

(define before (ctx alpha 100000))
(run 'set ga "src" "beta()")
(define after (ctx alpha 100000))
(want "U8 ga stops using alpha: the users lose ga, and the receipt (every premise, the five query digests included) is the same"
      (list (map cadr (users-list after)) (equal? (clause before 'receipt) (clause after 'receipt)))
      (list (by-id gamma lu du) #t))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\ncontext-users complete\n" rows bad)
(exit (if (= bad 0) 0 1))
