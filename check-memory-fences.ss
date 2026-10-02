;; Checks that the agents page shows the memory document's fences byte for byte.
;;
;;     scheme --script check-memory-fences.ss <agents.html> <document> <commit>
;;
;; A second reading, written apart from the generator. The headings the page
;; must show are listed here, not taken from the page. For each, the document
;; is read again: a heading is a line of one to six '#' and a space, outside any
;; fence; a fence opens with ``` or ~~~ and closes on a line that is exactly
;; the same three characters; a fence's text is each of its lines followed by a
;; newline. The page's fence for that heading must be that text as the site's
;; HTML escapes it, byte for byte. It refuses when a heading is missing, appears
;; twice, or has no fence; when a fence never closes or differs; when the
;; document indents a fence or a heading by one to three spaces, which this
;; reading does not take; when a fence is not inside the page section its
;; heading belongs to; when the prompt sections hold an HTML comment or a code
;; block, in any letter case, without the marker that names its heading; and
;; when the number compared is zero or differs from the number the page
;; declares or shows.
;;
;; THE INPUTS IT ACCEPTS ARE BOUNDED, and anything outside the bound is refused
;; by name rather than read. The document: fences are exactly ``` (with an info
;; string) or ~~~ and close on exactly the same three characters; a longer
;; marker, a closing line with anything after the marker, and a fence or heading
;; indented by one to three spaces are refused. The page: it is the site
;; renderer's output, which writes tags in lower case and writes no comment and
;; no template element; a page with an HTML comment, a template element, or a
;; section, pre or code tag in another letter case is refused.
;;
;; It reads the page's markup, not its styles: it checks the text the prompt
;; sections carry, and does not tell whether a style or a class hides them.

(import (chezscheme))

(define headings
  '(("## 1. Migrating an existing markdown memory" . "migrate")
    ("### 2.1 Turn auto-memory off" . "memory")
    ("### 2.2 Inject the outline at session start" . "memory")
    ("### 2.3 Tell the agent how to use it (CLAUDE.md)" . "memory")
    ("### 2.4 The MCP route instead of the shell" . "memory")))

(define args (command-line-arguments))
(unless (= (length args) 3)
  (display "usage: scheme --script check-memory-fences.ss <agents.html> <document> <commit>\n"
           (current-error-port))
  (exit 2))

(define (refuse . parts)
  (display "check-memory-fences: " (current-error-port))
  (for-each (lambda (p) (display p (current-error-port))) parts)
  (newline (current-error-port))
  (exit 1))

(define html (call-with-input-file (car args) get-string-all))
(define doc (call-with-input-file (cadr args) get-string-all))
(define commit (caddr args))

(define (find s sub start)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i start))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) i)
            (else (loop (+ i 1)))))))

(define (count-of s sub start end)
  (let loop ((i start) (n 0))
    (let ((j (find s sub i)))
      (if (and j (< j end)) (loop (+ j 1) (+ n 1)) n))))

(define (escape s)
  (let ((out (open-output-string)))
    (string-for-each
      (lambda (c)
        (case c
          ((#\&) (put-string out "&amp;"))
          ((#\<) (put-string out "&lt;"))
          ((#\>) (put-string out "&gt;"))
          ((#\") (put-string out "&quot;"))
          (else (put-char out c))))
      s)
    (get-output-string out)))

(define doc-lines
  (let loop ((i 0) (out '()))
    (let ((j (find doc "\n" i)))
      (if j
          (loop (+ j 1) (cons (substring doc i j) out))
          (reverse (if (= i (string-length doc)) out (cons (substring doc i (string-length doc)) out)))))))

(define (starts? p s)
  (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))

(define (indented-marker? s)
  (let ((n (let count ((i 0)) (if (and (< i (string-length s)) (< i 4) (char=? (string-ref s i) #\space)) (count (+ i 1)) i))))
    (and (<= 1 n 3)
         (let ((rest (substring s n (string-length s))))
           (or (starts? "```" rest) (starts? "~~~" rest) (atx-heading? rest))))))

(define (atx-heading? s)
  (let ((n (let count ((i 0)) (if (and (< i (string-length s)) (char=? (string-ref s i) #\#)) (count (+ i 1)) i))))
    (and (<= 1 n 6) (or (= n (string-length s)) (char=? (string-ref s n) #\space)))))

;; One pass over the document: for every heading outside a fence, the texts of
;; the fences under it, in order.
(define sections
  (let loop ((ls doc-lines) (fence #f) (body '()) (current #f) (acc '()) (table '()))
    (define (close-section)
      (if current (cons (cons current (reverse acc)) table) table))
    (cond
      ((null? ls)
       (when fence (refuse "a fence under " (or current "the document's start") " never closes"))
       (reverse (close-section)))
      ((and fence (not (string=? (car ls) fence)) (starts? fence (car ls)))
       (refuse "the document has a fence line this check does not read: " (car ls)))
      ((and (not fence) (or (starts? "````" (car ls)) (starts? "~~~~" (car ls))))
       (refuse "the document has a fence line this check does not read: " (car ls)))
      (fence
       (if (string=? (car ls) fence)
           (loop (cdr ls) #f '() current
                 (cons (apply string-append (map (lambda (l) (string-append l "\n")) (reverse body))) acc)
                 table)
           (loop (cdr ls) fence (cons (car ls) body) current acc table)))
      ((indented-marker? (car ls))
       (refuse "the document indents a fence or a heading, which this check does not read: " (car ls)))
      ((or (starts? "```" (car ls)) (starts? "~~~" (car ls)))
       (loop (cdr ls) (substring (car ls) 0 3) '() current acc table))
      ((atx-heading? (car ls))
       (loop (cdr ls) #f '() (car ls) '() (close-section)))
      (else (loop (cdr ls) #f body current acc table)))))

(define (document-fences heading)
  (let ((hits (filter (lambda (s) (string=? (car s) heading)) sections)))
    (cond ((null? hits) (refuse "the document has no heading " heading))
          ((pair? (cdr hits)) (refuse "the document has the heading " heading " more than once"))
          ((null? (cdar hits)) (refuse "the document has no fence under " heading))
          (else (cdar hits)))))

(define (count-ci s sub start end)
  (let ((m (string-length sub)))
    (let loop ((i start) (n 0))
      (cond ((> (+ i m) end) n)
            ((string-ci=? (substring s i (+ i m)) sub) (loop (+ i 1) (+ n 1)))
            (else (loop (+ i 1) n))))))

(when (find html "<!--" 0) (refuse "agents.html has an HTML comment"))
(when (> (count-ci html "<template" 0 (string-length html)) 0)
  (refuse "agents.html has a template element"))
(for-each
  (lambda (tag)
    (unless (= (count-ci html tag 0 (string-length html)) (count-of html tag 0 (string-length html)))
      (refuse "agents.html writes " tag " in another letter case")))
  '("<section" "</section" "<pre" "</pre" "<code" "</code"))

(define (section-span id)
  (let* ((a (or (find html (string-append "<section id=\"" id "\">") 0)
                (refuse "agents.html has no section " id)))
         (b (or (find html "</section>" a) (refuse "agents.html does not close section " id))))
    (cons a b)))
(define spans (map (lambda (id) (cons id (section-span id))) '("migrate" "memory")))
(define prompt-start (apply min (map cadr spans)))
(define prompt-end (apply max (map cddr spans)))
(define declared
  (let* ((key "data-fences=\"")
         (a (find html key 0)))
    (unless a (refuse "agents.html declares no fence count (data-fences)"))
    (when (find html key (+ a 1)) (refuse "agents.html declares a fence count more than once"))
    (let* ((from (+ a (string-length key))) (b (find html "\"" from)))
      (or (and b (string->number (substring html from b)))
          (refuse "agents.html declares a fence count that is not a number")))))

(define (page-fence heading section index)
  (let* ((open (string-append "<pre data-heading=\"" (escape heading) "\" data-index=\""
                              (number->string index) "\"><code>"))
         (span (cdr (assoc section spans)))
         (a (find html open (car span))))
    (and a (< a (cdr span))
         (let* ((from (+ a (string-length open))) (b (find html "</code></pre>" from)))
           (and b (substring html from b))))))

(define compared
  (let loop ((hs headings) (n 0))
    (if (null? hs)
        n
        (let* ((h (caar hs)) (section (cdar hs)) (want (document-fences h)))
          (let each ((ws want) (i 1))
            (unless (null? ws)
              (let ((got (page-fence h section i)))
                (unless got (refuse "section " section " of agents.html does not show fence " i " under " h))
                (unless (string=? got (escape (car ws)))
                  (refuse "fence " i " under " h " is not the document's, byte for byte"))
                (each (cdr ws) (+ i 1)))))
          (when (page-fence h section (+ (length want) 1))
            (refuse "agents.html shows more fences under " h " than the document has"))
          (loop (cdr hs) (+ n (length want)))))))

(define marked (count-of html "<pre data-heading=\"" prompt-start prompt-end))
(define shown (count-ci html "<pre" prompt-start prompt-end))
(when (> (count-of html "<!--" prompt-start prompt-end) 0)
  (refuse "agents.html has an HTML comment in its prompt sections"))

(when (= compared 0) (refuse "compared no fence"))
(unless (= shown marked)
  (refuse "agents.html shows " shown " code blocks in its prompt sections and marks " marked))
(unless (= compared marked)
  (refuse "compared " compared " fences, and the page shows " marked))
(unless (= compared declared)
  (refuse "compared " compared " fences, and the page declares " declared))
(display "agents.html: ")
(display compared)
(display " fences compared with docs/claude-code-memory.md at ")
(display commit)
(display ", byte for byte\n")
