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

;; A DAEMON THAT NEVER HAD A SUBSCRIBER ANSWERS AS IT DID BEFORE THE STREAM.
;;
;; The guard is differential. One store is prepared, copied twice, and each
;; copy is served by a daemon: one built from the tree before the change
;; stream (THEOURGIA_BASE_LIBDIR, a library root holding that theourgia and
;; igropyr), one from this tree. The same script of requests is asked of
;; both and the answers compared byte for byte, a timestamp (a run of twelve
;; or more digits) read as one token. Then the trace of one read and one
;; commit is compared the same way, and the time of fifty commits is
;; measured three times on each.
;;
;; THE EXCEPTIONS ARE NAMED, and each is this row's own expectation:
;; `describe` (the catalogue gains subscribe and read's --rev), the
;; unknown-verb refusal (its list of verbs gains subscribe), and read's
;; usage answer (it shows --rev): each answer differs from the base's by
;; exactly that addition, removed from it the base's answer again.
;; Everything else must be equal.
;;
;; THE TWO DAEMONS' OWN PATHS differ by construction (two copies, two
;; sockets); each answer and trace has its daemon's store and socket read
;; as one token before comparing.
;;
;; WITHOUT A BASE TREE THIS IS NOT A READING, and it says so and why.

(import (chezscheme) (theourgia sched) (theourgia net))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define base-lib (getenv "THEOURGIA_BASE_LIBDIR"))
(unless (and (string? base-lib) (file-exists? (string-append base-lib "/theourgia/daemon.sc")))
  (printf "NOT A READING: THEOURGIA_BASE_LIBDIR must name a library root holding the base tree's theourgia/ and igropyr/ (it is ~s)~%" base-lib)
  (printf "change-stream-guard complete~%")
  (exit 2))
(define this-lib (getenv "CHEZSCHEMELIBDIRS"))

(define pid-text (number->string (get-process-id)))
(define scratch (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/csg-" pid-text))
(define sockets (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/csg-" pid-text))
(system (string-append "rm -rf " scratch " " sockets "; mkdir -p " scratch " " sockets))
(putenv "THEOURGIA_HOME" (string-append scratch "/home"))

(define (file-text p) (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (replace-all text from to)
  (let ((n (string-length from)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i n) (string-length text)) (apply string-append (reverse (cons (substring text i (string-length text)) out))))
            ((string=? (substring text i (+ i n)) from) (loop (+ i n) (cons to out)))
            (else (loop (+ i 1) (cons (string (string-ref text i)) out)))))))
(define (contains? text needle) (not (equal? (replace-all text needle "") text)))
;; D's store and socket become STORE and SOCKET, then every run of twelve or
;; more digits becomes "#"
(define (normalize-for d text)
  (normalize (replace-all (replace-all text (cadr d) "SOCKET") (car d) "STORE")))
(define (normalize text)
  (let loop ((i 0) (out '()))
    (cond ((>= i (string-length text)) (list->string (reverse out)))
          ((char-numeric? (string-ref text i))
           (let run ((j i)) (if (and (< j (string-length text)) (char-numeric? (string-ref text j))) (run (+ j 1))
                                (if (>= (- j i) 12) (loop j (cons #\# out))
                                    (loop j (append (reverse (string->list (substring text i j))) out))))))
          (else (loop (+ i 1) (cons (string-ref text i) out))))))

;; ---- one prepared store, two copies, two daemons -------------------------------
(define prepared (string-append scratch "/prepared"))
(system (string-append "mkdir -p " prepared))
(define (lib-env lib) (string-append "CHEZSCHEMELIBDIRS=" lib " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"))
(system (string-append (lib-env this-lib) " scheme --script ../theourgia.sc init --store " prepared " > /dev/null 2>&1 < /dev/null"))
(system (string-append (lib-env this-lib) " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc insert --title Seed --text seed --store " prepared " > /dev/null 2>&1 < /dev/null"))
(define (start! name lib)
  (let ((st (string-append scratch "/" name)) (sk (string-append sockets "/" name ".sock"))
        (rn (string-append scratch "/" name ".sc")) (lg (string-append scratch "/" name ".log")))
    (system (string-append "rm -rf " st "; cp -R " prepared " " st))
    (call-with-output-file rn
      (lambda (p) (for-each (lambda (l) (display l p) (newline p))
                            (list "(import (chezscheme) (theourgia daemon))"
                                  (string-append "(serve \"" st "\" \"" sk "\")")))))
    (system (string-append "THEOURGIA_TRACE=1 " (lib-env lib) " sh -c 'scheme --script " rn " > " lg " 2>&1' &"))
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg))
            ((> k 400) (printf "NOT A READING: the ~a daemon never created its socket~%~a~%" name (file-text lg)) (exit 2))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (stop! d) (system (string-append "pkill -f " (caddr d) " 2>/dev/null")))
(define (line-complete? bv)
  (let loop ((i 0)) (cond ((>= i (bytevector-length bv)) #f) ((= (bytevector-u8-ref bv i) 10) #t) (else (loop (+ i 1))))))
(define (ask-raw d verb args)
  (let* ((text (string-append "(request 1 " (format "~s" (car d)) " \"tester\" #f wire #f #f " (symbol->string verb)
                              (apply string-append (map (lambda (a) (string-append " " (format "~s" a))) args)) ")\n"))
         (r (exchange (cadr d) (string->utf8 text) line-complete? 15000)))
    (if (and (pair? r) (eq? 'answer (car r))) (utf8->string (cadr r)) (format "~s" r))))

;; THE SCRIPT: requests over the verbs a client uses, in an order where each
;; can depend on the last. Ids are the seed's and the ones made here, the
;; same on both copies (one writer, one sequence).
(define script
  '((outline) (insert "--title" "A" "--text" "alpha") (insert "--title" "B" "--text" "beta")
    (read "SEED") (read "SEED" "--md") (read "SEED" "--recursive") (outline "--depth" "3")
    (set "A" "src" "alpha two") (set "A" "title" "A2") (move "B" "A") (link "A" "relates" "B")
    (refs "A") (unlink "A" "relates" "B") (search "alpha") (grep "beta") (whereis "A2")
    (conflicts) (log) (write "A" "a draft" "--writer" "w1") (read "A" "--working" "--writer" "w1") (drafts "--writer" "w1")
    (discard "A" "--writer" "w1")
    (write "A" "a second draft" "--writer" "w2") (diagnostics "--writer" "w2") (commit "--writer" "w2")
    (restore "no-such-version" "--writer" "w2") (reach "A") (tag "named") (tag) (diff "named" "named")
    (batch "()") (snapshot) (check) (adopt) (log "A")
    (del "B") (read "B") (outline)
    (describe) (no-such-verb) (read)))
(define exceptions '(describe no-such-verb read-usage))
(define (resolve d args ids)
  (map (lambda (a) (let ((e (assoc a ids))) (if e (cdr e) a))) args))
(define (run-script d)
  ;; -> list of (label . normalized answer)
  (let loop ((ss script) (ids '()) (out '()))
    (if (null? ss) (reverse out)
        (let* ((req (car ss)) (verb (car req)) (args (resolve d (cdr req) ids))
               (answer (ask-raw d verb args))
               (label (cond ((and (eq? verb 'read) (null? args)) 'read-usage) (else verb)))
               (ids (cond ((and (eq? verb 'insert) (pair? (cdr req)))
                           (let* ((a (guard (e (#t #f)) (read (open-string-input-port answer))))
                                  (inner (and (pair? a) (assq 'stdout (cdr a)) (read (open-string-input-port (cadr (assq 'stdout (cdr a)))))))
                                  (ev (and (pair? inner) (assq 'events (cdr inner))))
                                  (id (and ev (let ((e (car (cadr ev)))) (string-append (car e) "." (number->string (cdr e)))))))
                             (if id (cons (cons (caddr req) id) ids) ids)))
                          (else ids))))
          (loop (cdr ss) ids (cons (cons label (normalize-for d answer)) out))))))

(start-scheduler
  (lambda ()
    (let* ((seed (let ((a (system (string-append (lib-env this-lib) " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc outline --wire --store " prepared " > " scratch "/seed.out 2>&1 < /dev/null"))))
                   (let* ((t (file-text (string-append scratch "/seed.out")))
                          (i (let find ((k 0)) (cond ((>= (+ k 1) (string-length t)) #f) ((char=? (string-ref t k) #\") k) (else (find (+ k 1)))))))
                     (and i (let close ((j (+ i 1))) (if (char=? (string-ref t j) #\") (substring t (+ i 1) j) (close (+ j 1))))))))
           (_ (set! script (map (lambda (r) (map (lambda (a) (if (equal? a "SEED") seed a)) r)) script)))
           (b (start! "base" base-lib))
           (n (start! "new" this-lib))
           (rb (run-script b))
           (rn (run-script n)))
      (want "F10-9 every answer of the script is byte for byte the base's, but the three named exceptions"
            (filter (lambda (x) x)
                    (map (lambda (x y) (and (not (memq (car x) exceptions)) (not (equal? (cdr x) (cdr y))) (list (car x) (cdr x) (cdr y))))
                         rb rn))
            '())
      ;; EACH EXCEPTION'S OWN EXPECTATION: the base's answer lacks the
      ;; addition, the new one has it, and taking it out of the new one gives
      ;; the base's back (for describe, which adds an entry with its own
      ;; text, the first two only, and read's usage inside it).
      (let ((old (lambda (k) (cdr (assq k rb)))) (new (lambda (k) (cdr (assq k rn))))
            (without-word (lambda (t w) (let ((a (replace-all t (string-append " " w) "")) (b (replace-all t (string-append w " ") "")))
                                          (list a b)))))
        (want "F10-9 the unknown-verb refusal differs by subscribe in its list of verbs, and by nothing else"
              (list (contains? (old 'no-such-verb) "subscribe") (contains? (new 'no-such-verb) "subscribe")
                    (and (member (old 'no-such-verb) (without-word (new 'no-such-verb) "subscribe")) #t))
              '(#f #t #t))
        (want "F10-9 read's usage answer differs by its --rev option, and by nothing else"
              (list (contains? (old 'read-usage) "--rev") (contains? (new 'read-usage) "--rev")
                    (equal? (replace-all (new 'read-usage) " (\\\"--rev\\\")" "") (old 'read-usage)))
              '(#f #t #t))
        (want "F10-9 describe gains subscribe with route stream, and read's --rev"
              (list (contains? (old 'describe) "subscribe") (contains? (new 'describe) "subscribe")
                    (contains? (new 'describe) "stream") (contains? (old 'describe) "--rev") (contains? (new 'describe) "--rev"))
              '(#f #t #t #f #t)))
      ;; THE TRACE of one read and one commit, each daemon's lines between two markers.
      (let ((trace-of (lambda (d)
                        (let* ((before (string-length (file-text (cadddr d)))))
                          (ask-raw d 'read (list seed))
                          (ask-raw d 'set (list seed "src" "traced"))
                          (sleep-ms 300)
                          (normalize-for d (substring (file-text (cadddr d)) before (string-length (file-text (cadddr d)))))))))
        (want "F10-9 the trace of a read and a commit is byte for byte the base's"
              (let ((tb (trace-of b)) (tn (trace-of n))) (if (equal? tb tn) 'equal (list tb tn)))
              'equal))
      ;; THE TIME of fifty commits, three runs each, interleaved.
      (let* ((time-50 (lambda (d k)
                        (let ((t0 (real-time)))
                          (let loop ((i 0)) (when (< i 50) (ask-raw d 'set (list seed "title" (string-append "t" (number->string k) "-" (number->string i)))) (loop (+ i 1))))
                          (- (real-time) t0))))
             (runs (let loop ((k 0) (out '())) (if (= k 3) (reverse out) (loop (+ k 1) (cons (list (time-50 b k) (time-50 n k)) out)))))
             (base-ms (map car runs)) (new-ms (map cadr runs))
             (median (lambda (xs) (cadr (list-sort < xs)))))
        (printf "   50 commits, ms: base ~s new ~s~%" base-ms new-ms)
        (want "F10-9 the median of fifty commits is within the base's measured spread: at most the slowest base run (three runs each)"
              (<= (median new-ms) (apply max base-ms))
              #t))
      (stop! b) (stop! n)
      (system (string-append "rm -rf " sockets))
      (printf "rows: ~a~%~a failures~%change-stream-guard complete~%" rows bad)
      (exit (if (= bad 0) 0 1)))))
