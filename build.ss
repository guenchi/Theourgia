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

;; Compile every library in this tree to a `.so` beside a copy of its
;; name, in a directory of the caller's choosing.
;;
;;     scheme --script build.ss <library-root> <output-root>
;;
;; NOTE: BOTH ARGUMENTS ARE THE DIRECTORY THAT *CONTAINS* `theourgia/`, not
;; the source directory itself. That is what Chez means by a library
;; directory: `(theourgia store)` is looked up as `theourgia/store.sc`
;; underneath it. Passing the source directory compiled seven libraries
;; into a doubled path and stopped.
;;
;; KEY: THIS IS THE FORM THE THING SHIPS IN. Development runs from source;
;; a user gets compiled objects. Those differ in one way that matters
;; here: `core.sc` finds `(theourgia eval-supervise)`, `(theourgia sched)` and
;; `(theourgia net)` at RUN TIME, when an `eval` or a forwarded call asks for
;; them, and `theourgiad.sc` finds `(theourgia daemon)` the same way once its
;; arguments are checked; whether that works depends on the library being
;; FINDABLE -- which it is as a `.so` on the library path, and is not
;; inside a whole-program package that left it out for being statically
;; unreferenced.
;;
;; NEVER: SO THE LAZY LOADING NEEDS A READING IN THIS FORM TOO, not only from
;; source. `f0-ondemand.sc` takes one; `RUN.md` states the rule.
;;
;; NOTE: PRODUCTS DO NOT GO IN THE SOURCE TREE. A stale `.so` beside a `.sc`
;; is resolved in preference to it, so a tree holding both can be running
;; code nobody has edited for a week.

(import (chezscheme))

(define argv (command-line-arguments))

(when (< (length argv) 2)
  (display "usage: scheme --script build.ss <library-root> <output-root>\n"
           (console-error-port))
  (exit 1))

(define root (car argv))
(define out-root (cadr argv))
;; NEVER: THE DEPENDENCY IS COMPILED TOO, and it has to be. An object records
;; WHICH compilation instance of each import it was built against; a
;; theourgia object built against a compiled `(igropyr crypto)` refuses
;; to load beside a source one -- measured: `loading crypto.sc yielded a
;; different compilation instance of (igropyr crypto) from that required
;; by compiled (theourgia digest)`. A build that compiled only this tree
;; would produce objects that cannot be run.
(define packages '("igropyr" "theourgia"))

(unless (file-directory? out-root) (mkdir out-root))
(for-each (lambda (pkg)
            (let ((d (string-append out-root "/" pkg)))
              (unless (file-directory? d) (mkdir d))))
          packages)

;; NOTE: A LIBRARY, NOT EVERY SOURCE. `core.sc`, `theourgiad.sc` and `build.ss`
;; are programs --
;; they have no library form and `compile-library` refuses them. The test
;; is the file's own first form.
(define (declares-a-library? path)
  (guard (e (#t #f))
    (call-with-input-file path
      (lambda (p)
        (let loop ()
          (let ((x (read p)))
            (cond
              ((eof-object? x) #f)
              ((and (pair? x) (eq? (car x) 'library)) #t)
              (else (loop)))))))))

;; NOTE: BOTH SUFFIXES ARE STILL ACCEPTED. This tree and igropyr both
;; write `.sc` now; `.ss` stays here so an older tree still compiles.
(define (source-suffix? f)
  (let ((n (string-length f)))
    (and (> n 3)
         (or (string=? (substring f (- n 3) n) ".sc")
             (string=? (substring f (- n 3) n) ".sc")))))

;; Each entry is (package . filename), in the order they will be tried.
(define libraries
  (apply append
         (map (lambda (pkg)
                (let ((dir (string-append root "/" pkg)))
                  (if (not (file-directory? dir))
                      '()
                      (let loop ((files (list-sort string<? (directory-list dir))) (acc '()))
                        (cond
                          ((null? files) (reverse acc))
                          (else
                           (let ((f (car files)))
                             (if (and (source-suffix? f)
                                      (declares-a-library? (string-append dir "/" f)))
                                 (loop (cdr files) (cons (cons pkg f) acc))
                                 (loop (cdr files) acc)))))))))
              packages)))

;; NEVER: ONE MAPPING, SO THE OBJECTS LAND SOMEWHERE ELSE. `library-directories`
;; takes (source . object) pairs; with `compile-imported-libraries` on,
;; compiling one library compiles what it imports, and every product goes
;; to the object side.
;; NEVER: THE OBJECTS ARE THE ONLY THING ON THE PATH. Not "objects first":
;; only objects. If the source root is reachable, a library whose
;; dependency is not built yet compiles happily against the SOURCE of
;; it -- and when that dependency is compiled a moment later it becomes
;; a different compilation instance, which is refused at load time.
;; Measured inside the dependency itself: `loading util.so yielded a
;; different compilation instance of (igropyr util) from that required
;; by compiled (igropyr sexpr)`.
;;
;; With only objects reachable, an unbuilt dependency is simply NOT
;; FOUND, the sweep defers that library, and the order sorts itself out.
;; The file being compiled is named by an absolute path, so it does not
;; need to be on the path itself.
(library-directories (list (cons out-root out-root)))
(library-extensions '((".so" . ".so")))

;; NEVER: OFF, AND THIS IS THE WHOLE DIFFICULTY OF THIS FILE. With it on,
;; compiling a library also rebuilds everything it imports -- and the
;; rebuild is a NEW compilation instance, so objects written earlier
;; still refer to the old one. Chez then refuses at load time: `loading
;; wire.so yielded a different compilation instance of (theourgia wire)
;; from that required by compiled (theourgia reduce)`. Measured twice,
;; once with an explicit output path and once without, and dependency
;; order did not help -- the rebuild happens either way.
;;
;; With it off, a library whose imports are not built yet simply fails,
;; the sweep below defers it, and the next pass finds them. Each library
;; is compiled exactly once, against the objects the others will load.
;;
;; NOTE: igropyr IS NOT COMPILED HERE. It is a separate dependency with its
;; own build, and these objects load it from wherever the library path
;; says -- the same source the run will use.
(compile-imported-libraries #f)
(generate-wpo-files #f)

;; NEVER: IN DEPENDENCY ORDER, AND THE ORDER IS FOUND BY TRYING. Compiling
;; alphabetically meant a library was built before the ones it imports,
;; so those were rebuilt inline on every call and the objects ended up
;; referring to different compilation INSTANCES of each other -- Chez
;; says so: `loading wire.so yielded a different compilation instance of
;; (theourgia wire) from that required by compiled (theourgia reduce)`.
;;
;; NOTE: AND THE OUTPUT PATH IS ALWAYS GIVEN. Without one, `compile-library`
;; writes the object NEXT TO THE SOURCE -- measured, and it put 39 `.so`
;; files into the working tree, which is the one thing this file's own
;; header warns about.
;;
;; A pass compiles whatever it can; a library whose imports are not built
;; yet raises and is deferred. The loop ends when a pass makes no
;; progress, and says which ones never went through rather than claiming
;; a build that did not happen.
(define (object-for entry)
  (string-append out-root "/" (car entry) "/"
                 (substring (cdr entry) 0 (- (string-length (cdr entry)) 3)) ".so"))

(define (source-of entry)
  (string-append root "/" (car entry) "/" (cdr entry)))

(define (name-of entry) (string-append (car entry) "/" (cdr entry)))

;; NEVER: THE PROGRAMS ARE NOT LIBRARIES AND SO ARE NOT COMPILED, BUT THE
;; OUTPUT DIRECTORY IS USELESS WITHOUT THEM. `theourgia.sc` is the thin
;; client a user runs; it starts `theourgiad.sc` (the daemon) and `core.sc`
;; (every other role) BESIDE ITSELF, `core.sc` starts `eval-worker.sc` and,
;; for `eval --lang`, `eval-runner-exec.sc` the same way, and `mcp/server.sc`
;; is the MCP shell, which starts `theourgiad.sc` one level up.
;; Measured on an output directory built before this: with the source tree
;; off the library path -- which is the whole point of shipping objects --
;; running the thin client from the output gives
;;
;;     Exception in load: failed for <out>/theourgia/theourgia.sc:
;;     no such file or directory
;;
;; and putting only the thin client there moves the same failure one step
;; along, to the program it starts. They are found by path beside the
;; program, not by the library path, so no amount of `.so` makes up for their
;; absence.
;;
;; NOTE: THE RELATIVE LAYOUT IS PART OF IT. `mcp/server.sc` has to land in
;; a `mcp/` directory under the package, because that is where its own
;; `beside-this-program` arithmetic expects to start from.
(define programs '("theourgia.sc" "core.sc" "theourgiad.sc" "eval-worker.sc" "eval-runner-exec.sc" "mcp/server.sc"))

(define (copy-file! from to)
  (let ((in (open-file-input-port from))
        (out (open-file-output-port to (file-options no-fail))))
    (let loop ()
      (let ((bv (get-bytevector-n in 65536)))
        (unless (eof-object? bv)
          (put-bytevector out bv)
          (loop))))
    (close-port in)
    (close-port out)))

;; NEVER: A COPY THAT COPIED NOTHING MUST NOT LOOK LIKE A BUILD. It reports
;; the number, and a missing program is a failure here rather than a
;; puzzle for whoever runs the output directory a week later.
(define (copy-programs!)
  (let ((missing '()) (n 0))
    (for-each
      (lambda (rel)
        (let* ((from (string-append root "/theourgia/" rel))
               (to (string-append out-root "/theourgia/" rel))
               (slash (let scan ((i (- (string-length rel) 1)))
                        (cond ((< i 0) #f)
                              ((char=? (string-ref rel i) #\/) i)
                              (else (scan (- i 1)))))))
          (when slash
            (let ((d (string-append out-root "/theourgia/" (substring rel 0 slash))))
              (unless (file-directory? d) (mkdir d))))
          (if (file-exists? from)
              (begin (when (file-exists? to) (delete-file to))
                     (copy-file! from to)
                     (set! n (+ n 1)))
              (set! missing (cons rel missing)))))
      programs)
    (printf "copied ~a of ~a programs into ~a/theourgia\n" n (length programs) out-root)
    (when (pair? missing)
      (printf "MISSING PROGRAMS ~a:\n" (length missing))
      (for-each (lambda (r) (printf "  ~a\n" r)) (reverse missing))
      (exit 1))))

(let pass ((todo libraries) (done 0))
  (let sweep ((fs todo) (deferred '()) (built 0) (errs '()))
    (cond
      ((pair? fs)
       (guard (e (#t (sweep (cdr fs) (cons (car fs) deferred) built
                            (cons (cons (name-of (car fs))
                                        (if (and (condition? e) (message-condition? e))
                                            (condition-message e) "?"))
                                  errs))))
         (compile-library (source-of (car fs)) (object-for (car fs)))
         (sweep (cdr fs) deferred (+ built 1) errs)))
      ((null? deferred)
       (printf "compiled ~a of ~a libraries into ~a\n" (+ done built) (length libraries) out-root)
       (copy-programs!)
       (printf "build complete\n"))
      ((zero? built)
       ;; NEVER: NO PROGRESS MEANS IT IS NOT AN ORDERING PROBLEM. Say which
       ;; ones and why, rather than reporting a partial build as a build.
       (printf "compiled ~a of ~a libraries into ~a\n" (+ done built) (length libraries) out-root)
       (printf "COULD NOT COMPILE ~a:\n" (length deferred))
       (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p))) (reverse errs))
       (exit 1))
      (else (pass (reverse deferred) (+ done built))))))
