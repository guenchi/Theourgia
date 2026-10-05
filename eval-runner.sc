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

;;; (theourgia eval-runner) -- `eval --lang <l>` for a language other than
;;; Scheme: the store projected into a directory, the source written beside
;;; it, the language's runner run over it under the evaluation supervisor.
;;;
;;; KEY: TWO SIBLING DIRECTORIES UNDER ONE FRESH eval-<token>/ beneath the
;;; product's run root: tree/, the projection written by the code exporter
;;; with raw bytes (no markers), and source/, holding the source under the
;;; runner's one-component source-name. No exported path can reach source/
;;; -- the exporter keeps every path under tree/ -- so the two cannot
;;; collide and nothing has to be checked before the run. The runner's cwd
;;; is tree/.
;;;
;;; KEY: THE PROJECTION IS DECLARED. It is exported under the evaluation's
;;; own store key, which eval declares, so a store missing a writer projects
;;; what can be read and the runner runs; what the loads heard reaches the
;;; answer through the caller's one incomplete clause. A projection that
;;; cannot be made at all answers projection-failed with the exporter's own
;;; refusal inside, and nothing runs.
;;;
;;; NEVER: THE PROJECTION IS REMOVED AFTER THE ANSWER, WHATEVER THE ANSWER.
;;; A removal that fails is a trace line, eval-cleanup-failed, and does not
;;; change the answer.

(library (theourgia eval-runner)
  (export run-foreign-eval runner-refusal)
  (import (chezscheme)
          (only (theourgia client) run-root next-attempt-token)
          (only (theourgia ffi) mkdir-p! mkdir-exclusive! real-path overwrite-entry! unlink! rmdir! directory-entries
                entry-name-type trace-event! theourgia-stage with-mutation-record)
          (only (theourgia store) open-and-reduce)
          (only (theourgia log) log-error?)
          (only (theourgia rpc) describe-log-error)
          (only (theourgia working) working-state)
          (only (theourgia reduce) reduce-applied-cut)
          (only (theourgia code-project) export-code-view)
          (only (theourgia languages) language-for-name language-runner runner-valid? override-valid?
                runner-problem runner-with-override)
          (only (theourgia eval-supervise) supervise-eval))

  ;; A PATH MADE ABSOLUTE against this process's current directory: the
  ;; launcher and the runner run with a different cwd, so a relative path
  ;; handed to them would name something else. "" and "." are that
  ;; directory itself.
  (define (absolute p)
    (cond ((member p '("" ".")) (current-directory))
          ((char=? (string-ref p 0) #\/) p)
          (else (string-append (current-directory) "/" p))))

  (define (join-on parts c)
    (if (null? parts)
        ""
        (fold-left (lambda (acc p) (string-append acc (string c) p)) (car parts) (cdr parts))))

  ;; NEVER: THE LAUNCHER FINDS ITS LIBRARIES WHERE THIS PROCESS FOUND ITS
  ;; OWN, AND NOWHERE ELSE. Its CHEZSCHEMELIBDIRS and CHEZSCHEMELIBEXTS are
  ;; written from this process's (library-directories) and
  ;; (library-extensions), never from this process's environment: a
  ;; process started with --libdirs, or with the variable unset (Chez's
  ;; default is "."), would otherwise hand the launcher a search path that
  ;; resolves against its cwd, which is the projection's tree/ -- and a
  ;; store holding theourgia/ffi.sc would be imported by the launcher
  ;; before its interpreter check, before ready and outside its isolation.
  ;;
  ;; Every directory is made absolute against this process's cwd at this
  ;; moment, so neither "." nor any relative spelling reaches the launcher.
  ;; EVERY PAIR IS WRITTEN src::obj, never as the bare name when the two
  ;; are equal: that shorthand wrote ("" . "") as nothing, which reads back
  ;; as no entry at all. The entries are joined by ":", with no separator
  ;; at the end: a trailing one makes Chez append its defaults, "." among
  ;; them.
  ;;
  ;; NEVER: WHAT THE VARIABLES CANNOT CARRY IS REFUSED, NOT DROPPED OR
  ;; BENT, AND CHEZ ITSELF DECIDES WHAT THAT IS. Both parameters accept the
  ;; variable's text and read it exactly as the launcher will, so each
  ;; pair's text is read back through its parameter and must come back
  ;; equal to the pair, and then the whole joined text must come back equal
  ;; to the whole list. A name holding NUL is refused before that: the
  ;; parameter keeps it, but the environment is handed over as C strings,
  ;; which end there. run-foreign-eval asks launcher-library-refusal first,
  ;; before anything is made or spawned, and answers spawn-refused with one
  ;; of four reasons:
  ;;   library-directories-empty, library-extensions-empty -- an empty value
  ;;     reads as an empty list (measured; not as Chez's defaults), so the
  ;;     launcher would load nothing and fail with the cause hidden;
  ;;   library-directory-unrepresentable (directory <d>) -- e.g. a name
  ;;     holding ":", which reads back as two entries, one of them
  ;;     relative. It is reached from a command line: a process whose cwd
  ;;     holds ":" and that finds its libraries by the default ".", or by
  ;;     a relative --libdirs entry, has that cwd in its list once made
  ;;     absolute (L18);
  ;;   library-extension-unrepresentable (extension <e>) -- the same for
  ;;     an extension.
  ;; The empty lists and the extensions are reached in the rows by a
  ;; program that sets the parameter itself (L19): today's tripwires, not
  ;; command-line paths. The reading happens in the boot actor before any
  ;; other actor of this evaluation exists, and nothing imports a library
  ;; inside it, so the parameters' transient values reach nobody.
  (define (pair-text p) (string-append (car p) "::" (cdr p)))

  (define (library-path-text pairs)
    (join-on (map pair-text pairs) #\:))

  (define (reads-back? parameter text value)
    (guard (e (#t #f))
      (equal? (parameterize ((parameter text)) (parameter)) value)))

  (define (has-nul? s) (memv #\nul (string->list s)))

  ;; The name of a pair that the variable cannot carry, or #f: the SIDE
  ;; that fails is named -- a side holding NUL; else, when the pair's text
  ;; does not read back as the pair, the side whose own text, written as a
  ;; pair with itself, does not read back either (the source when both
  ;; would, the pair failing only as a whole).
  (define (failing-side parameter p)
    (let ((alone-ok? (lambda (n) (reads-back? parameter (pair-text (cons n n)) (list (cons n n))))))
      (cond ((has-nul? (car p)) (car p))
            ((has-nul? (cdr p)) (cdr p))
            ((reads-back? parameter (pair-text p) (list p)) #f)
            ((not (alone-ok? (car p))) (car p))
            ((not (alone-ok? (cdr p))) (cdr p))
            (else (car p)))))

  ;; The first failing side of the pairs, or #f when every pair and then
  ;; the whole joined text read back; a list failing only as a whole is
  ;; named by its first source.
  (define (unrepresentable-name parameter pairs)
    (let loop ((ps pairs))
      (cond ((null? ps)
             (if (reads-back? parameter (library-path-text pairs) pairs) #f (caar pairs)))
            ((failing-side parameter (car ps)) => (lambda (n) n))
            (else (loop (cdr ps))))))

  (define (launcher-directory-pairs)
    (map (lambda (p) (cons (absolute (car p)) (absolute (cdr p)))) (library-directories)))

  ;; The refusal the variables force, or #f.
  (define (launcher-library-refusal)
    (let ((dirs (launcher-directory-pairs)) (exts (library-extensions)))
      (cond ((null? dirs)
             '(error spawn-refused (reason library-directories-empty)))
            ((unrepresentable-name library-directories dirs)
             => (lambda (d) (list 'error 'spawn-refused '(reason library-directory-unrepresentable) (list 'directory d))))
            ((null? exts)
             '(error spawn-refused (reason library-extensions-empty)))
            ((unrepresentable-name library-extensions exts)
             => (lambda (e) (list 'error 'spawn-refused '(reason library-extension-unrepresentable) (list 'extension e))))
            (else #f))))

  (define (launcher-library-directories)
    (library-path-text (launcher-directory-pairs)))

  (define (launcher-library-extensions)
    (library-path-text (library-extensions)))

  ;; THE LAUNCHER'S ENVIRONMENT: what it needs to load and to find the
  ;; interpreter, nothing else. It keeps only PATH, HOME and LANG for the
  ;; interpreter itself.
  (define (launcher-environment)
    (append
      (let loop ((names '("PATH" "HOME" "LANG")) (acc '()))
        (cond ((null? names) (reverse acc))
              ((getenv (car names))
               => (lambda (v) (loop (cdr names) (cons (string-append (car names) "=" v) acc))))
              (else (loop (cdr names) acc))))
      (list (string-append "CHEZSCHEMELIBDIRS=" (launcher-library-directories))
            (string-append "CHEZSCHEMELIBEXTS=" (launcher-library-extensions)))))

  ;; "{file}" is the source's absolute path, "{dir}" the projection's, and
  ;; "{libdirs}" the launcher's own library path, each as a whole argument.
  (define (expand-argv argv file dir)
    (map (lambda (a)
           (cond ((string=? a "{file}") file)
                 ((string=? a "{dir}") dir)
                 ((string=? a "{libdirs}") (launcher-library-directories))
                 (else a)))
         argv))

  ;; THE RUNNER'S ENVIRONMENT PAIRS, as "NAME=VALUE". In a value "{file}",
  ;; "{dir}" and "{libdirs}" -- the launcher's own library path, as written
  ;; for it above -- are replaced WHEREVER THEY OCCUR, not only as the whole
  ;; value as in argv: a value is a path list, and the projection is joined
  ;; to other directories inside it. One scan, left to right; what a
  ;; placeholder is replaced by is not scanned again.
  (define (expand-value v file dir)
    (let ((places (list (cons "{file}" file) (cons "{dir}" dir)
                        (cons "{libdirs}" (launcher-library-directories))))
          (n (string-length v)))
      (let loop ((i 0) (out '()))
        (if (= i n)
            (apply string-append (reverse out))
            (let ((hit (find (lambda (p)
                               (let ((k (string-length (car p))))
                                 (and (<= (+ i k) n) (string=? (substring v i (+ i k)) (car p)))))
                             places)))
              (if hit
                  (loop (+ i (string-length (car hit))) (cons (cdr hit) out))
                  (loop (+ i 1) (cons (string (string-ref v i)) out))))))))
  (define (expand-env env file dir)
    (map (lambda (p) (string-append (car p) "=" (expand-value (cadr p) file dir))) env))

  ;; REMOVED DEPTH FIRST through the door, so the injection build can make a
  ;; removal fail (unlink-fail@eval-cleanup).
  ;;
  ;; NEVER: A SYMBOLIC LINK IS UNLINKED, NEVER ENTERED. The runner wrote
  ;; into tree/ and source/ and may have left a link to any directory on
  ;; the machine; asking entry-type, which follows the link, would walk
  ;; into that directory and remove what is there. entry-name-type answers
  ;; for the link itself.
  (define (remove-tree! path)
    (if (eq? (entry-name-type path) 'directory)
        (begin (for-each (lambda (n) (remove-tree! (string-append path "/" n))) (directory-entries path))
               (rmdir! path))
        (unlink! path)))

  (define (cleanup! dir)
    (guard (e (#t (trace-event! 'eval-cleanup-failed dir #f)))
      (parameterize ((theourgia-stage 'eval-cleanup))
        (remove-tree! dir))))

  ;; ---- the operator's runner, one variable per language ---------------------
  ;;
  ;; NEVER: WHAT RUNS IS WHAT THE OPERATOR NAMED, AND THE STORE NAMES
  ;; NOTHING. The runner is the language table's, or the one the language's
  ;; variable gives; that variable is read from this process's environment,
  ;; never from a block, a language entry in the store, or a projected file.
  ;; The store is data that may come from anyone. After exec, a projected
  ;; library the source imports runs with the runner's reach: that is what
  ;; the runner is for (README).
  ;;
  ;; KEY: ONE VARIABLE PER LANGUAGE THAT HAS ONE, AND ONE READER. Each row is
  ;; (lang variable how value): the variable's name, how its value meets the
  ;; table's runner, and the value as this process's environment holds it,
  ;; read by a literal getenv so the documentation's scanner finds the name.
  ;;   merge    -- chez, as it always was: one datum naming any non-empty
  ;;               set of argv, source-name and env, each replacing the
  ;;               table's field whole (env included); the merged runner is
  ;;               checked again.
  ;;   replace  -- every other language: one datum that is a whole runner,
  ;;               argv and source-name at least, and it is the runner; no
  ;;               field of the table's survives it.
  ;; EMPTY IS UNSET, as env-or reads every THEOURGIA_ variable.
  ;;
  ;; NEVER: A VALUE THAT DOES NOT READ, OR THAT THE CHECKS REFUSE, IS
  ;; REFUSED BY NAME, its own variable's, never replaced by the table's
  ;; default: the operator's line is the whole truth, and a runner they did
  ;; not name must not run in its place.
  (define (runner-variables)
    (list (list "chez" "THEOURGIA_RUNNER_CHEZ" 'merge (getenv "THEOURGIA_RUNNER_CHEZ"))
          (list "c" "THEOURGIA_RUNNER_C" 'replace (getenv "THEOURGIA_RUNNER_C"))
          (list "rust" "THEOURGIA_RUNNER_RUST" 'replace (getenv "THEOURGIA_RUNNER_RUST"))
          (list "go" "THEOURGIA_RUNNER_GO" 'replace (getenv "THEOURGIA_RUNNER_GO"))
          (list "java" "THEOURGIA_RUNNER_JAVA" 'replace (getenv "THEOURGIA_RUNNER_JAVA"))
          (list "typescript" "THEOURGIA_RUNNER_TYPESCRIPT" 'replace (getenv "THEOURGIA_RUNNER_TYPESCRIPT"))))

  (define (runner-text value)
    (and value (> (string-length value) 0) value))

  (define (runner-config-invalid variable detail)
    (list 'error 'bad-request '(reason runner-config-invalid)
          (list 'variable variable) (list 'detail detail)))

  ;; -> (list <datum>) when `text` is exactly one datum, else #f.
  (define (one-datum text)
    (guard (e (#t #f))
      (let* ((p (open-string-input-port text)) (d (read p)))
        (and (not (eof-object? d)) (eof-object? (read p)) (list d)))))

  ;; -> (values <runner> #f), or (values #f <refusal>).
  (define (resolved-runner lang)
    (let* ((table (language-runner (language-for-name lang)))
           (row (assoc lang (runner-variables)))
           (variable (and row (cadr row)))
           (text (and row (runner-text (cadddr row)))))
      (if (not text)
          (values table #f)
          (let ((d (one-datum text)))
            (cond
              ((not d) (values #f (runner-config-invalid variable 'not-one-datum)))
              ((eq? (caddr row) 'replace)
               (let ((problem (runner-problem (car d) '(argv source-name))))
                 (if problem
                     (values #f (runner-config-invalid variable (list 'field problem)))
                     (values (car d) #f))))
              ((not (override-valid? (car d)))
               (values #f (runner-config-invalid variable (list 'field (or (runner-problem (car d) '()) 'runner)))))
              (else
               ;; A TRIPWIRE: unreachable by construction today -- an override
               ;; that passes override-valid?, merged field by field into a
               ;; table runner that passed checked-catalogue or
               ;; register-language!, always holds a valid argv and source-name
               ;; -- and kept so that a later change to the checks cannot make
               ;; the merge the one path that is never checked.
               (let ((r (runner-with-override table (car d))))
                 (if (runner-valid? r)
                     (values r #f)
                     (values #f (runner-config-invalid variable
                                  (list 'field (runner-problem r '(argv source-name)))))))))))))

  ;; THE REFUSAL THE OPERATOR'S CONFIGURATION FORCES, or #f: core.sc asks it
  ;; before the evaluation takes an admission slot, so a runner that cannot
  ;; be resolved is answered as itself -- never as eval-busy behind a full
  ;; pool -- and makes nothing, the admission directory included.
  ;; run-foreign-eval resolves again from the same environment.
  (define (runner-refusal lang)
    (let-values (((runner refusal) (resolved-runner lang))) refusal))

  ;; NEVER: A PROJECTION DIRECTORY THE RUNNER'S LIBRARY PATH CANNOT CARRY IS
  ;; REFUSED, NOT BENT. A runner whose CHEZSCHEMELIBDIRS names "{dir}" gets
  ;; the resolved tree/ inside that variable, which Chez splits at ":"; a
  ;; tree/ whose text does not read back as itself would put another
  ;; directory, or a relative one, on the interpreter's path. It is decided
  ;; by the same read-back as the launcher's own pairs, on the path as
  ;; claimed, before anything is exported; the claimed directory is removed
  ;; on the way out as always.
  (define (has-text? s t)
    (let ((n (string-length s)) (k (string-length t)))
      (let loop ((i 0))
        (and (<= (+ i k) n) (or (string=? (substring s i (+ i k)) t) (loop (+ i 1)))))))

  (define (projection-directory-refusal env tree)
    (and (exists (lambda (p) (and (string=? (car p) "CHEZSCHEMELIBDIRS") (has-text? (cadr p) "{dir}"))) env)
         (unrepresentable-name library-directories (list (cons tree tree)))
         (list 'error 'spawn-refused '(reason projection-directory-unrepresentable) (list 'directory tree))))

  ;; The value under k in an options alist, #f when it is absent.
  (define (option-of options k)
    (let ((e (assq k options))) (and e (cdr e))))

  ;; (run-foreign-eval key lang source options) -> the answer, before the
  ;; caller merges what the process heard. options is an alist:
  ;;   working? writer timeout-ms memory-bytes output-bytes scheme launcher
  ;;
  ;; NOTE: THE SCRATCH DIRECTORY IS NOT A CHANGE THIS EVALUATION REPORTS. Its
  ;; creation, the projection written into it and its removal run in a
  ;; mutation-record scope of their own, so a refusal's written record says
  ;; what the evaluation did to what the caller owns -- the writer's draft
  ;; lock that the working view may create, as on the Scheme path -- and not
  ;; the scratch space it made and removed. The working view is therefore
  ;; taken OUTSIDE that scope.
  (define (run-foreign-eval key lang source options)
    (let-values (((runner config-refusal) (resolved-runner lang)))
      (let ((refusal (or config-refusal (launcher-library-refusal))))
        (if refusal
            refusal
            (let ((working (and (option-of options 'working?) (working-state key #f (option-of options 'writer)))))
              (if (and working (not (and (pair? working) (eq? 'ok (car working)))))
                  (append working '((during view)))
                  (with-mutation-record
                    (lambda () (in-scratch key lang runner source options working)))))))))

  ;; NEVER: CLEANUP REMOVES ONLY WHAT THIS EVALUATION MADE. The token is the
  ;; process id and a counter, so its name can repeat after the id is
  ;; reused, and something may already be there. eval-<token> is therefore
  ;; created EXCLUSIVELY (mkdir-exclusive!): a name already taken, by a file
  ;; or a directory, is left alone and the next token is tried, up to
  ;; scratch-tries names, after which the answer is spawn-refused (reason
  ;; scratch-unavailable). The dynamic-wind whose exit is cleanup! is entered
  ;; only once the directory is this evaluation's own.
  ;;
  ;; NEVER: THE RUN ROOT IS RESOLVED ONCE, AT THE CLAIM (real-path), and the
  ;; directory is claimed and removed through the resolved path. The run
  ;; root may be reached through a symbolic link the runner can rewrite;
  ;; removing through the unresolved path would follow the link wherever
  ;; the runner pointed it by then, and remove an eval-<token> there instead
  ;; of this evaluation's own. A run root that cannot be resolved answers
  ;; scratch-unavailable. What remains -- a real directory ABOVE the
  ;; resolved root replaced by a link during the run -- needs write access
  ;; to that directory's parent, and is inside the runner's reach (README).
  (define scratch-tries 8)

  (define (in-scratch key lang runner source options working)
    (let* ((argv (cadr (assq 'argv runner)))
           (env (let ((e (assq 'env runner))) (if e (cadr e) '())))
           (source-name (cadr (assq 'source-name runner)))
           (given-root (absolute (run-root))))
      (mkdir-p! given-root)
      (let ((root (real-path given-root)))
        (if (not root)
            '(error spawn-refused (reason scratch-unavailable))
            (let claim ((tries 1))
              (let ((dir (string-append root "/eval-" (next-attempt-token))))
                (if (eq? (mkdir-exclusive! dir) 'exists)
                    (if (< tries scratch-tries)
                        (claim (+ tries 1))
                        '(error spawn-refused (reason scratch-unavailable)))
                    (let* ((tree (string-append dir "/tree"))
                           (source-dir (string-append dir "/source"))
                           (file (string-append source-dir "/" source-name)))
                      (dynamic-wind
                        (lambda () #f)
                        (lambda ()
                          (or (projection-directory-refusal env tree)
                              (begin
                                (mkdir-p! tree)
                                (mkdir-p! source-dir)
                                (project-and-run key lang source options working argv env tree file))))
                        (lambda () (cleanup! dir)))))))))))

  ;; THE VIEW THE EXPORT READS, and the cut it used: the writer's working
  ;; view with --working (its baseline under its drafts), otherwise the
  ;; committed state now, loaded under the evaluation's key.
  (define (project-and-run key lang source options working argv env tree file)
    (let* ((seen #f)
           ;; A STORE THAT CANNOT BE LOADED AT ALL -- meta.sexp missing or
           ;; not a store's -- raises a log-error, which no answer here
           ;; names. It is turned into the answer the export verb gives for
           ;; it, and the exporter's own wrapper carries that out as its
           ;; refusal: projection-failed with (error meta (path ...)) inside.
           (view (lambda ()
                   (let ((state (if working
                                    (caddr working)
                                    (guard (e ((log-error? e) (raise (describe-log-error e))))
                                      (open-and-reduce key)))))
                     (set! seen state)
                     state)))
           (exported (export-code-view key tree #t view)))
      (if (not (and (pair? exported) (eq? 'ok (car exported))))
          (list 'error 'projection-failed exported)
          (begin
            (overwrite-entry! file (string->utf8 source))
            (let ((answer (supervise-eval
                            (list (cons 'runner-argv (expand-argv argv file tree))
                                  (cons 'runner-env (expand-env env file tree))
                                  (cons 'launcher (option-of options 'launcher))
                                  (cons 'scheme (option-of options 'scheme))
                                  (cons 'cwd tree)
                                  (cons 'env (launcher-environment))
                                  (cons 'timeout-ms (option-of options 'timeout-ms))
                                  (cons 'memory-bytes (option-of options 'memory-bytes))
                                  (cons 'output-bytes (option-of options 'output-bytes))))))
              (if (and (pair? answer) (eq? 'ok (car answer)))
                  (append answer
                          (list (list 'lang (string->symbol lang))
                                (cons 'projection (cdr exported))
                                (list 'working-view (and working (cadr working))
                                      (and seen (reduce-applied-cut seen))
                                      (if working (cadddr working) '()))))
                  answer))))))
)
