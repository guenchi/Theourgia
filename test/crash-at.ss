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

;; A REAL CRASH AT THE N-TH APPEND.
;;
;; (crash-at n root home cli args) runs the product's own entry, CLI (the
;; path of core.sc), as a child with ARGS (a list of strings) and
;;   THEOURGIA_INJECT=on THEOURGIA_BARRIER=before-append:<fifo> THEOURGIA_TRACE=1
;; THEOURGIA_HOME=HOME. Every append of the child then waits at the barrier
;; (log.sc) until a byte is written to the fifo. The helper releases the
;; first n-1 appends, one byte each and only after the child is seen
;; waiting at that one, and kills the child while it waits at the n-th.
;; The store is left exactly as a crash at that point would leave it.
;;
;; -> (barriers trace-path): how many barrier lines the child printed and
;; where its trace is. A caller asserts BARRIERS = n before it believes any
;; row about the crash: fewer means the child stopped earlier (or never
;; started), and every later row would be about something else.
;;
;; It adds no barrier and calls nothing but the product's entry. Each call
;; uses its own files under ROOT, named by a counter.
;; An optional last argument names a file the child reads as its standard
;; input (a batch's intents arrive there on the command line); without it
;; the child inherits the caller's.
;;
;; NOTE: EVERY WAIT IS BOUNDED. A fifo write with no reader blocks for
;; ever; the whole dance runs under one alarm, the shell kills its child
;; on any exit, and each wait for a barrier line is a bounded loop.

(define crash-at-count 0)

(define (crash-at-quote s)
  (string-append
    "'"
    (let loop ((cs (string->list s)) (out '()))
      (cond ((null? cs) (list->string (reverse out)))
            ((char=? (car cs) #\') (loop (cdr cs) (append (reverse (string->list "'\\''")) out)))
            (else (loop (cdr cs) (cons (car cs) out)))))
    "'"))

(define (crash-at n root home cli args . stdin-file)
  (set! crash-at-count (+ crash-at-count 1))
  (let* ((tag (string-append "crash" (number->string crash-at-count)))
         (fifo (string-append root "/" tag ".gate"))
         (trace (string-append root "/" tag ".trace"))
         (report (string-append root "/" tag ".report"))
         (runner (string-append root "/" tag ".sh"))
         (count-lines (string-append "grep -c barrier " (crash-at-quote trace) " 2>/dev/null"))
         ;; A trace not yet created counts as no barrier line.
         (wait-for (lambda (k)
                     (string-append
                       "i=0\n"
                       "while [ $i -lt 400000 ]; do c=$(" count-lines "); "
                       "[ \"${c:-0}\" -ge " (number->string k) " ] && break; i=$((i+1)); done\n"))))
    (call-with-port (open-file-output-port runner (file-options no-fail) 'block (native-transcoder))
      (lambda (p)
        (put-string p
          (string-append
            "#!/bin/sh\n"
            "rm -f " (crash-at-quote fifo) " " (crash-at-quote trace) "\n"
            "mkfifo " (crash-at-quote fifo) "\n"
            "child=\n"
            "trap 'test -n \"$child\" && kill -9 $child 2>/dev/null' EXIT\n"
            "THEOURGIA_HOME=" (crash-at-quote home) " THEOURGIA_INJECT=on "
            "THEOURGIA_BARRIER=before-append:" (crash-at-quote fifo) " THEOURGIA_TRACE=1 "
            "scheme --script " (crash-at-quote cli)
            (apply string-append (map (lambda (a) (string-append " " (crash-at-quote a))) args))
            (if (pair? stdin-file) (string-append " < " (crash-at-quote (car stdin-file))) "")
            " > /dev/null 2> " (crash-at-quote trace) " &\n"
            "child=$!\n"
            (let release ((k 1) (out ""))
              (if (>= k n)
                  out
                  (release (+ k 1)
                           (string-append out (wait-for k) "printf x > " (crash-at-quote fifo) "\n"))))
            (wait-for n)
            "kill -9 $child 2>/dev/null\n"
            "wait $child 2>/dev/null\n"
            "child=\n"
            count-lines " > " (crash-at-quote report) "\n"))))
    (system (string-append "chmod +x " (crash-at-quote runner)))
    (system (string-append "perl -e 'alarm 120; exec @ARGV' sh " (crash-at-quote runner) " > /dev/null 2>&1"))
    (list (guard (e (#t 'no-report))
            (call-with-input-file report
              (lambda (p) (let ((line (get-line p)))
                            (if (eof-object? line) 'empty (string->number line))))))
          trace)))
