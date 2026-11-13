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

;; THE LOAD BUDGET'S STORE, MADE BY THE PRODUCT. A store of a given number
;; of records, laid out as the product lays them out: one import-code
;; request of half as many files writes a file block and a code block for
;; each, through the write path, so the records are the product's (its
;; actor, its dependencies, its timestamps) and the segments are rotated
;; where the product rotates them (1 MiB). The files are about 3.4 KB, so a
;; record averages about 2.5 KB, as the code store's do.
;;
;; TWICE, INTO TWO STORES. The second local store is the one a daemon
;; serves: the two are written in the same run under one machine home, and
;; a copy of one store written beside it would share its identity and
;; read, to the registry, as that store rolled back.
;;
;; AND A MIRRORED TWIN: a second store, made by `init`, into which every
;; segment of the first store's writer is published as another writer's
;; (log-publish!, which lists it in that writer's manifest). A load of the
;; twin reads the same records through the path a mirrored writer takes:
;; each segment's digest and its edge sequences checked against the
;; manifest.
;;
;; Included, not imported: the including file imports (chezscheme),
;; (theourgia rpc) and (theourgia log), and defines `lb-core` (the path of
;; core.sc) before it calls lb-generate!.
;;
;;   (lb-generate! root records) -> an alist:
;;     (local . <store>) (served . <store>) (twin . <store>)
;;     (writer . <the local store's writer, which the twin mirrors>)
;;     (served-writer . <the served store's writer>) (segments . <how many>)
;;     (ms (files . <ms>) (import . <ms>) (served . <ms>) (twin . <ms>))
;;   and raises when a step does not answer as it should.

(define (lb-file-text k)
  (let loop ((i 0) (out '()))
    (if (= i 48)
        (string-append "function f" (number->string k) "() {\n"
                       (apply string-append (reverse out))
                       "  return " (number->string k) ";\n}\n")
        (loop (+ i 1)
              (cons (string-append "  const v" (number->string i) " = \""
                                   (let ((piece (string-append "x" (number->string (+ (* k 53) i)))))
                                     (apply string-append (map (lambda (_) piece) (iota 9))))
                                   "\";\n")
                    out)))))

;; A path as one word of a shell command, whatever it holds: in single
;; quotes, each single quote in it closed, escaped and reopened.
(define (lb-quote s)
  (let loop ((cs (string->list s)) (out '(#\')))
    (cond ((null? cs) (list->string (reverse (cons #\' out))))
          ((char=? (car cs) #\') (loop (cdr cs) (append (reverse (string->list "'\\''")) out)))
          (else (loop (cdr cs) (cons (car cs) out))))))

(define (lb-write-files! dir n)
  (system (string-append "mkdir -p " (lb-quote dir)))
  (let loop ((k 0))
    (when (< k n)
      (call-with-output-file (string-append dir "/f" (number->string k) ".js")
        (lambda (p) (put-string p (lb-file-text k)))
        'replace)
      (loop (+ k 1)))))

(define (lb-ms-since t0) (- (real-time) t0))

(define (lb-must what answer)
  (unless (and (pair? answer) (eq? (car answer) 'ok))
    (raise (condition (make-error) (make-message-condition what) (make-irritants-condition (list answer)))))
  answer)

(define (lb-clause key answer)
  (let ((hit (and (pair? answer) (assq key (cdr answer))))) (and hit (cadr hit))))

(define (lb-file-bytes path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

(define (lb-segment-files store writer)
  (let loop ((k 1) (out '()))
    (let ((path (string-append (writer-directory store writer) "/" (segment-file-name k))))
      (if (file-exists? path) (loop (+ k 1) (cons path out)) (reverse out)))))

;; One store made by `init`, then the files imported into it by one
;; import-code request in a child (THEOURGIA_LOCAL=1, as the command line
;; runs it). -> the store's writer.
;; THE IMPORT IS SETUP, NOT A JUDGED ARM: it runs with THEOURGIA_DEGENERACY
;; unset, so a whole-suite gate's checks (sibling-order's, which recomputes the
;; structure per sibling question, costs an import of n files about n^3) do not
;; spend the fixture's limit building its stores; the arms keep the switch.
(define (lb-import! root files store tag)
  (let ((writer (lb-clause 'writer (lb-must (string-append "init of " tag)
                                            (rpc-dispatch store '(init) "tester")))))
    (system (string-append "env -u THEOURGIA_DEGENERACY THEOURGIA_LOCAL=1 scheme --script " (lb-quote lb-core) " import-code " (lb-quote files)
                           " --store " (lb-quote store) " --wire > " (lb-quote (string-append root "/import-" tag ".out"))
                           " 2>&1 < /dev/null"))
    writer))

(define (lb-generate! root records)
  (let* ((files (string-append root "/files"))
         (local (string-append root "/local"))
         (served (string-append root "/served"))
         (twin (string-append root "/twin"))
         (t0 (real-time))
         (_ (lb-write-files! files (div records 2)))
         (ms-files (lb-ms-since t0))
         (t1 (real-time))
         (writer (lb-import! root files local "local"))
         (ms-import (lb-ms-since t1))
         (t3 (real-time))
         (served-writer (lb-import! root files served "served"))
         (_ (printf "setup ran with THEOURGIA_DEGENERACY unset: the generator's two imports of ~a files (local, served)~%" (div records 2)))
         (ms-served (lb-ms-since t3))
         (t2 (real-time))
         (_ (lb-must "init of the twin" (rpc-dispatch twin '(init) "tester")))
         (segments (lb-segment-files local writer)))
    (let loop ((k 1) (paths segments))
      (unless (null? paths)
        (let* ((bytes (lb-file-bytes (car paths)))
               (answer (log-publish! twin writer k bytes (segment-sha bytes))))
          (unless (and (pair? answer) (eq? (car answer) 'published))
            (lb-must (format "publishing segment ~a into the twin" k) answer))
          (loop (+ k 1) (cdr paths)))))
    (list (cons 'local local) (cons 'served served) (cons 'twin twin) (cons 'writer writer)
          (cons 'served-writer served-writer) (cons 'segments (length segments))
          (list 'ms (cons 'files ms-files) (cons 'import ms-import) (cons 'served ms-served)
                (cons 'twin (lb-ms-since t2))))))
