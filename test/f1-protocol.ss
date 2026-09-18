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

;; The write protocol, in one place and reaching three readers.
;;
;; ⭐ THE POINT IS THAT THERE IS ONE COPY. `write-protocol` is exported by
;; `(theourgia rpc)`; the README's `## Writing for agents` section is that
;; string; the MCP shell's `insert` and `write` descriptions begin with
;; it. Three statements of a protocol are three protocols as soon as one
;; is edited, and the reader follows whichever they happened to meet.

(import (chezscheme)
        (only (theourgia rpc) write-protocol))

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
(define here (string-append "/tmp/f1-" (number->string (get-process-id))))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (index-of text needle from)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i from))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))

(define (contains? text needle) (and (index-of text needle 0) #t))

;; ---- DOC-P1: the README section IS the constant -------------------------------
;;
;; ⛔ READ FROM THE FILE ON DISK, NOT REGENERATED. A check that wrote the
;; section from the constant and then compared them would agree with
;; itself no matter what the committed README says, which is the one
;; thing this row exists to notice.
(define readme (file-text "../README.md"))

;; ⚠️ THE BLANK LINE BEFORE THE NEXT HEADING BELONGS TO THE DOCUMENT,
;; not to the protocol: Markdown needs it and the constant does not have
;; it. Taking it made the two differ by one byte -- a real difference,
;; and not the one this row is about.
(define section-body
  (let ((start (index-of readme "## Writing for agents\n\n" 0)))
    (and start
         (let* ((from (+ start (string-length "## Writing for agents\n\n")))
                (end (index-of readme "\n\n## " from)))
           (and end (substring readme from (+ end 1)))))))

(want "DOC-P1 the README's section is the core's protocol string, byte for byte"
      (if (equal? section-body write-protocol)
          'identical
          (list 'readme section-body 'constant write-protocol))
      'identical)

;; ---- DOC-P2: and the protocol still says the seven things ---------------------
;;
;; ⛔ DOC-P1 CANNOT SEE A RULE GO MISSING. It says the two copies agree;
;; delete a rule from the constant and regenerate the README and it stays
;; green. ⚠️ This list is the seventh copy on purpose -- it is a list of
;; what the protocol is FOR, kept outside the string so that dropping a
;; rule has to be done twice and deliberately.
(define seven-rules
  '(("one block answers one question" "answer one question on its own")
    ("a size bound, with a number"     "800 tokens")
    ("and the same bound in bytes"     "3000 bytes")
    ("a one-sentence title"            "one-sentence title")
    ("3 to 8 comma-separated keywords" "3 to 8 keywords, comma separated")
    ("the option that carries them"    "--keywords")
    ("placement under a parent"        "--under")
    ("the source bytes are not edited" "must not edit its prose")
    ("changes go through a draft"      "write and then commit")))

(want "DOC-P2 the protocol still states every rule it is for"
      (map car (filter (lambda (r) (not (contains? write-protocol (cadr r)))) seven-rules))
      '())

;; ---- MC-P1: the two writing tools carry it ------------------------------------
(define shell "../mcp/server.ss")

(define init
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                 "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                 "\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}"))
(define ready "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
(define listing "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}")

(system (string-append "rm -rf " here "; mkdir -p " here "/store"))
(system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                       "scheme --script ../cli.ss init --store " here "/store --wire > /dev/null 2>&1"))

;; ⚠️ THE REAL HANDSHAKE. `tools/list` before `notifications/initialized`
;; is refused by the shell, deliberately -- so a row that skipped it
;; would be reading an error and finding no tools in it.
(define tools-line
  (begin
    (call-with-output-file (string-append here "/in.jsonl")
      (lambda (p) (put-string p (string-append init "\n" ready "\n" listing "\n"))))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "scheme --script " shell " --store " here "/store < " here "/in.jsonl > "
                           here "/out.jsonl 2>" here "/err.txt"))
    ;; ⛔ BY THE REQUEST ID, NOT BY THE WORD `tools`. The initialize
    ;; result announces `"capabilities":{"tools":{}}` on the FIRST line,
    ;; so a search for that word finds the handshake and reports that
    ;; the shell offers no tools at all.
    (let* ((all (file-text (string-append here "/out.jsonl")))
           (at (index-of all "\"id\":2" 0)))
      (and at
           (let ((start (let loop ((i at)) (if (or (zero? i) (char=? (string-ref all (- i 1)) #\newline))
                                               i (loop (- i 1)))))
                 (end (or (index-of all "\n" at) (string-length all))))
             (substring all start end))))))

;; The description of one tool, taken out of the listing by name.
(define (description-of name)
  (let* ((key (string-append "\"name\":\"" name "\",\"description\":\""))
         (at (and tools-line (index-of tools-line key 0))))
    (and at
         (let loop ((i (+ at (string-length key))) (acc '()))
           (cond
             ((>= i (string-length tools-line)) #f)
             ((char=? (string-ref tools-line i) #\\)
              (loop (+ i 2) (cons (let ((c (string-ref tools-line (+ i 1))))
                                    (cond ((char=? c #\n) #\newline)
                                          ((char=? c #\t) #\tab)
                                          (else c)))
                                  acc)))
             ((char=? (string-ref tools-line i) #\") (list->string (reverse acc)))
             (else (loop (+ i 1) (cons (string-ref tools-line i) acc))))))))

(want "MC-P1 the listing was read at all"
      (if (and tools-line (contains? tools-line "theourgia_insert")) 'listed
          (list 'said (if tools-line (substring tools-line 0 (min 120 (string-length tools-line))) #f)))
      'listed)

(for-each
  (lambda (pair)
    (let ((d (description-of (car pair))))
      (want (string-append "MC-P1 " (car pair) " carries the protocol, then its own sentence")
            (list (if (and d (>= (string-length d) (string-length write-protocol))
                           (string=? (substring d 0 (string-length write-protocol)) write-protocol))
                      'protocol-first
                      (list 'said (and d (substring d 0 (min 60 (string-length d))))))
                  (if (and d (contains? d (cdr pair))) 'then-the-verb 'NO-VERB-SENTENCE))
            '(protocol-first then-the-verb))))
  (list (cons "theourgia_insert" "Execute the core insert command")
        (cons "theourgia_write" "Execute the core write command")))

;; ⛔ AND A TOOL THAT DOES NOT WRITE DOES NOT CARRY IT. Without this row
;; the two above are satisfied by a shell that prefixes every description
;; -- which would put the rules for writing a block in front of `read`,
;; `search` and everything else an agent is choosing between.
(want "MC-P1 TWIN: a reading tool keeps the plain sentence"
      (let ((d (description-of "theourgia_read")))
        (list (if (and d (not (contains? d "A block is the unit"))) 'no-protocol (list 'said d))
              (if (and d (contains? d "Execute the core read command")) 'plain-sentence 'MISSING)))
      '(no-protocol plain-sentence))

;; ---- and the shell's own README says where it comes from ----------------------
(want "MC-P1 the MCP README points at the core's string rather than restating it"
      (let ((t (file-text "../mcp/README.md")))
        (list (if (contains? t "write-protocol") 'names-the-constant 'MISSING)
              (if (contains? t "A block is the unit of writing") 'RESTATES-IT 'does-not-restate)))
      '(names-the-constant does-not-restate))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\nf1-protocol complete\n" rows bad)
(exit (if (zero? bad) 0 1))
