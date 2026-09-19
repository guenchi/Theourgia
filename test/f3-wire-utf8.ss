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

;; What an answer looks like when it is not all ASCII.
;;
;; KEY: THE WIRE CARRIES UTF-8 NOW, not `\x6C49;`. Measured by the worker:
;; escaping cost CJK 2.36 times the tokens, where the JSON envelope
;; around it costs 1.16 -- and the reader of these answers is an agent
;; paying by the token.
;;
;; NEVER: AND THE DISK DID NOT MOVE. A record is serialised by
;; `sexpr->string-extended` in `wire.ss`, which writes its own characters
;; and never consulted the print parameters. U4 is the guard: the two
;; paths are separate because somebody kept them separate, and only a
;; reading says they still are.

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
(define here (string-append "/tmp/f3-" (number->string (get-process-id))))
(define store (string-append here "/store"))
;; NOTE: SHORT: `sun_path` holds 104 bytes and this suite's usual scratch
;; path is longer, which makes a daemon report `listener-down` and look
;; broken.
(define socket (string-append "/tmp/f3s-" (number->string (get-process-id)) ".sock"))

(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (file-bytes path)
  (if (not (file-exists? path)) (bytevector)
      (call-with-port (open-file-input-port path) get-bytevector-all)))

(define (quoted a)
  (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                                 (string->list a))) "'"))

(define (cli-to out . args)
  (system (string-append
            "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
            "scheme --script ../cli.ss "
            (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
            "--store " store " --wire > " out " 2>&1"))
  (file-text out))

(define (cli . args) (apply cli-to (string-append here "/out.txt") args))

(define (index-of text needle from)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i from))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (loop (+ i 1)))))))
(define (contains? text needle) (and (index-of text needle 0) #t))

(define (bytes-contain? hay needle)
  (let ((n (bytevector-length needle)) (m (bytevector-length hay)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((let same ((j 0))
               (cond ((>= j n) #t)
                     ((= (bytevector-u8-ref hay (+ i j)) (bytevector-u8-ref needle j)) (same (+ j 1)))
                     (else #f)))
             #t)
            (else (loop (+ i 1)))))))

(system (string-append "rm -rf " here "; mkdir -p " store "; rm -f " socket))
(cli "init")

;; ---- U1: the bytes on the wire ------------------------------------------------
;;
;; NOTE: A SIMPLE FIXTURE FOR THE "NO `\\x`" HALF. The escape test below is
;; about a string that contains no backslash of its own; U2 carries the
;; awkward ones.
(define cjk "\x6c49;\x5b57;")
(define emoji "\x1f600;")
(define plain-id
  (let ((a (cli "insert" "--title" (string-append "CJK " cjk " " emoji) "--text" "body"
                "--keywords" cjk)))
    (let ((at (index-of a "(state ((\"" 0)))
      (and at (let scan ((j (+ at 10)) (acc '()))
                (cond ((>= j (string-length a)) #f)
                      ((char=? (string-ref a j) #\") (list->string (reverse acc)))
                      (else (scan (+ j 1) (cons (string-ref a j) acc)))))))))

(define wire-answer (cli "search" cjk))

(want "U1 an answer carries the characters themselves, not an escape for them"
      (list (if (contains? wire-answer cjk) 'raw-utf8 (list 'said wire-answer))
            (if (contains? wire-answer "\\x") 'STILL-ESCAPING 'no-escapes))
      '(raw-utf8 no-escapes))

;; ---- U2: and it reads back ----------------------------------------------------
;;
;; NEVER: THE AWKWARD ONES. A quote, a backslash, a newline inside the text,
;; and the six literal characters `\x41;` -- which an implementation that
;; unescaped the whole answer would turn into `A`. Every one of them has
;; to survive the round trip and the reader has to reach EOF after one
;; datum, so a broken frame shows as well as a broken character.
(define awkward (string-append "q\"uote \\ back " cjk "\nsecond line \\x41; end"))
(define awkward-id
  (let ((a (cli "insert" "--title" "U2" "--text" awkward)))
    (let ((at (index-of a "(state ((\"" 0)))
      (and at (let scan ((j (+ at 10)) (acc '()))
                (cond ((>= j (string-length a)) #f)
                      ((char=? (string-ref a j) #\") (list->string (reverse acc)))
                      (else (scan (+ j 1) (cons (string-ref a j) acc)))))))))

(define (field-of answer name)
  (let* ((port (open-string-input-port answer))
         (datum (read port))
         (after (read port))
         (body (and (pair? datum) (eq? 'ok (car datum)) (cadr datum)))
         (fields (and (pair? body) (assq 'fields body)))
         (e (and fields (assq name (cdr fields)))))
    (list (and e (cdr e)) (eof-object? after))))

(want "U2 every awkward character survives, and one datum is the whole answer"
      (field-of (cli "read" awkward-id) 'src)
      (list awkward #t))

;; ---- U4: and nothing about the disk changed -----------------------------------
;;
;; NEVER: THE WHOLE RECORD, NOT THE PAYLOAD. An implementation that changed
;; the escaping and updated the CRC to match would still decode to the
;; same datum -- only the bytes catch it. NOTE: The cross-core comparison
;; (this tree against the core before F3, same payload) is in NOTES; what
;; this row guards is that the stored form stays what it is.
;; NOTE: FOUND, NOT GUESSED. A segment lives at
;; `<store>/writers/<writer>/000001.sexp`, and an earlier version of this
;; row looked for it at the top of the store, found nothing, and reported
;; `NO-SEGMENT` -- which reads like a store that wrote nothing.
(define segment
  (let* ((wdir (string-append store "/writers"))
         (writers (if (file-exists? wdir) (directory-list wdir) '())))
    (let loop ((ws writers))
      (cond
        ((null? ws) #f)
        (else
         (let* ((dir (string-append wdir "/" (car ws)))
                (files (if (file-exists? dir) (directory-list dir) '()))
                (segs (filter (lambda (f)
                                (let ((n (string-length f)))
                                  (and (> n 5) (string=? (substring f (- n 5) n) ".sexp")
                                       (not (string=? f "owner.sexp")))))
                              files)))
           (if (pair? segs)
               (string-append dir "/" (car (list-sort string<? segs)))
               (loop (cdr ws)))))))))

(want "U4 TWIN: the record on disk holds the characters themselves, and no escape"
      (list (if segment 'found-a-segment 'NO-SEGMENT)
            (if (and segment (bytes-contain? (file-bytes segment) (string->utf8 cjk)))
                'raw-on-disk 'NOT-RAW)
            (if (and segment (bytes-contain? (file-bytes segment) (string->utf8 "\\x6C49;")))
                'ESCAPED-ON-DISK 'no-escape-on-disk))
      '(found-a-segment raw-on-disk no-escape-on-disk))

;; NEVER: AND THE BYTES DO NOT MOVE WHEN SOMETHING IS ONLY READ. A printer
;; that had reached the disk path would rewrite a record on the next
;; write to that segment; this pins the segment across a read and a
;; further write of ASCII-only content.
(define before-md5 (file-bytes segment))
(cli "read" plain-id)
(cli "insert" "--title" "ascii only" "--text" "plain")

(want "U4 TWIN: reading does not rewrite what is already stored"
      (if (bytes-contain? (file-bytes segment) before-md5) 'prefix-unchanged
          (list 'was (bytevector-length before-md5) 'now (bytevector-length (file-bytes segment))))
      'prefix-unchanged)

;; ---- U5 and U3: the other two routes ------------------------------------------
;;
;; NEVER: THREE ROUTES, ONE ANSWER. The point of collapsing the printers is
;; that these are the same bytes; a change made in the CLI alone would
;; leave the daemon and the shell spelling `\x6C49;`.
(system (string-append "( CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                       "scheme --script ../cli.ss serve " store " --socket " socket
                       " > " here "/serve.txt 2>&1 & echo $! > " here "/serve.pid )"))
(system "sleep 5")

(define via-daemon
  (let ((out (string-append here "/daemon.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
              "scheme --script ../cli.ss search " (quoted cjk)
              " --store " store " --socket " socket " --wire > " out " 2>&1"))
    (file-text out)))

;; NEVER: THE LOCAL ANSWER IS TAKEN AGAIN, HERE. Comparing against the one
;; captured for U1 compared two different stores: the block U2 inserts
;; also contains these characters, so the daemon saw one more hit and the
;; row reported a difference between the routes that was a difference
;; between two moments.
(define local-now (cli-to (string-append here "/local-now.txt") "search" cjk))

(want "U5 the daemon's answer is the local one, byte for byte"
      (if (string=? via-daemon local-now) 'identical
          (list 'daemon via-daemon 'local local-now))
      'identical)

(define mcp-text
  (let* ((in (string-append here "/mcp-in.jsonl"))
         (out (string-append here "/mcp-out.jsonl"))
         (init (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                              "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                              "\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}"))
         (call (string-append "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":"
                              "{\"name\":\"theourgia_search\",\"arguments\":{\"argv\":[\"" cjk "\"]}}}")))
    (call-with-output-file in
      (lambda (p) (put-string p (string-append init "\n"
                                               "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                                               call "\n"))))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
                           "THEOURGIA_RUN=" here "/run "
                           "scheme --script ../mcp/server.ss --store " store " < " in " > " out " 2>&1"))
    (file-text out)))

(want "U3 the MCP shell's text carries the characters, not an escape"
      (list (if (contains? mcp-text cjk) 'raw-in-the-json (list 'said (substring mcp-text (max 0 (- (string-length mcp-text) 120)) (string-length mcp-text))))
            (if (contains? mcp-text "x6C49") 'STILL-ESCAPING 'no-escapes))
      '(raw-in-the-json no-escapes))

(system (string-append "kill $(cat " here "/serve.pid) 2>/dev/null; sleep 1"))

;; ---- U6: what it saves, in bytes ----------------------------------------------
;;
;; NOTE: BYTES, NOT TOKENS, AND SAID SO. There is no tokeniser in this tree,
;; so this row reports the ratio it can actually measure -- the escaped
;; spelling against the raw one for the same answer. The token figure
;; (2.36x escaped, 1.16x for the JSON envelope) is the worker's, is
;; recorded in NOTES, and is NOT re-measured here.
(define escaped-length
  (let loop ((i 0) (n 0))
    (cond ((>= i (string-length wire-answer)) n)
          ((< (char->integer (string-ref wire-answer i)) 128) (loop (+ i 1) (+ n 1)))
          ;; a non-ASCII character would have been written `\xHHHH;`
          (else (loop (+ i 1) (+ n 7))))))

(want "U6 the raw spelling is shorter than the escaped one for the same answer"
      (if (< (string-length wire-answer) escaped-length) 'shorter
          (list 'raw (string-length wire-answer) 'escaped escaped-length))
      'shorter)

(printf "   (U6 same answer: ~a characters raw, ~a if escaped)\n"
        (string-length wire-answer) escaped-length)

(system (string-append "rm -rf " here "; rm -f " socket))
;; NEVER: THE SHELL STARTS A DAEMON NOW, so this fixture must say where its
;; run root is and must take down what it started. Before the shell was
;; rewritten it dispatched in its own process and started nothing, which
;; is why neither line was here. Measured without them: sockets and logs
;; under the user's real `$HOME/.theourgia/run`, and daemons still alive
;; minutes later.
(system (string-append "pkill -f 'serve " here "' 2>/dev/null"))
(system "sleep 1")

(printf "rows: ~a\n~a failures\nf3-wire-utf8 complete\n" rows bad)
(exit (if (zero? bad) 0 1))
