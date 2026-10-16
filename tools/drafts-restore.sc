#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; Write back the drafts tools/drafts-dump.sc dumped, each with its own
;; recorded based-on and cut, so each comes back as the version that was
;; dumped: a draft's version is the hash of its bytes, its based-on and its
;; cut, and all three are the dumped ones.
;;
;;   scheme --script tools/drafts-restore.sc <store> <dir> --writer <W>
;;
;; THE SAME STORE OR A GIT CLONE OF IT: the store id the dump recorded must be
;; this store's (a clone keeps meta.sexp, so it keeps the id, and the block
;; identities and baseline history with it). The writer must be the one the
;; dump names.
;;
;; EVERY DRAFT IS CHECKED BEFORE ANY IS WRITTEN, and nothing is written when
;; one is refused:
;; - its bytes are the dumped ones (the recorded sha256) and they hash, with
;;   the recorded based-on and cut, to the recorded version;
;; - the block's committed hash has not moved since the dump. A write checks
;;   that the named baseline was the block's hash at the named cut, not that
;;   it still is: freshness is checked here, and a moved block is refused by
;;   name, for the coder to rebase;
;; - the recorded cut can be replayed here, and at it the block's hash is the
;;   recorded based-on;
;; - the writer's slot for the block is empty, or holds this very draft (the
;;   same version: same bytes, based-on and cut), which is then left as it is.
;;   A slot holding anything else is refused: a write into it would keep that
;;   draft's baseline, and the version would not be the dumped one.
;; After the writes, each answer's version is compared with the dumped one.
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (restored <n>) (already <n>)) and exits 0.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia reduce) draft-version block-hash reduce-applied-cut state-read)
        (only (theourgia store) open-and-reduce store-state-at-cut)
        (only (theourgia log) store-id-of)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) entry-bytes))

(define actor "drafts-restore")
(define (refuse kind . details) (raise (cons 'error (cons kind details))))
(define (clause name datum)
  (let ((c (and (list? datum) (assq name (cdr datum))))) (and c (cadr c))))

;; A CUT COMPARED AS A SET of (writer . seq) with seq above zero: the order a
;; cut lists its writers in is not part of what it says.
(define (cut-set c)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             (filter (lambda (e) (and (pair? e) (integer? (cdr e)) (> (cdr e) 0))) c)))

(define (read-manifest dir)
  (let ((m (guard (e (#t #f))
             (read (open-string-input-port (utf8->string (entry-bytes (string-append dir "/DRAFTS.sexp"))))))))
    (unless (and (pair? m) (eq? (car m) 'drafts) (list? m)) (refuse 'manifest-unreadable (list 'dir dir)))
    m))

;; -> ((<id> . <version>) ...), the slots the writer holds now.
(define (held-drafts store writer)
  (let ((a (rpc-dispatch store (list 'drafts "--writer" writer) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'drafts-refused a))
    (let ((items (let ((i (assq 'items (cdr a)))) (if i (cdr i) '()))))
      (map (lambda (d) (cons (clause 'block d) (clause 'version d)))
           (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) items)))))

;; -> 'write or 'already for one dumped draft, or a refusal.
(define (judge store dir state held d)
  (let* ((id (clause 'block d)) (version (clause 'version d))
         (based-on (clause 'based-on d)) (cut (clause 'cut d))
         (bytes (entry-bytes (string-append dir "/" (clause 'bytes d)))))
    (unless (equal? (bytevector->hex (sha256 bytes)) (clause 'sha256 d))
      (refuse 'bytes-changed (list 'block id)))
    (unless (equal? (draft-version bytes based-on cut) version)
      (refuse 'draft-unreproducible (list 'block id) (list 'version version)))
    (unless (state-read state id) (refuse 'unknown-block (list 'block id)))
    (let ((now (block-hash state id)))
      (unless (equal? now based-on)
        (refuse 'baseline-moved (list 'block id) (list 'based-on based-on) (list 'now now))))
    (let ((past (store-state-at-cut store #f (format "~s" cut))))
      (when (and (pair? past) (eq? (car past) 'error))
        (refuse 'cut-unusable (list 'block id) (list 'cut cut) (list 'answer past)))
      (unless (equal? (cut-set (reduce-applied-cut past)) (cut-set cut))
        (refuse 'cut-not-reproduced (list 'block id) (list 'cut cut)))
      (unless (equal? (block-hash past id) based-on)
        (refuse 'baseline-not-at-cut (list 'block id))))
    (let ((slot (assoc id held)))
      (cond ((not slot) 'write)
            ((equal? (cdr slot) version) 'already)
            (else (refuse 'slot-occupied (list 'block id) (list 'holds (cdr slot)) (list 'dumped version)))))))

(define (restore store dir writer)
  (let* ((m (read-manifest dir))
         (drafts (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (cdr m))))
    (unless (equal? (clause 'store m) (store-id-of store))
      (refuse 'other-store (list 'dumped (clause 'store m)) (list 'here (store-id-of store))))
    (unless (equal? (clause 'writer m) writer)
      (refuse 'other-writer (list 'dumped (clause 'writer m)) (list 'given writer)))
    (let* ((state (open-and-reduce store))
           (held (held-drafts store writer))
           (verdicts (map (lambda (d) (judge store dir state held d)) drafts)))
      (let loop ((ds drafts) (vs verdicts) (written 0) (already 0))
        (cond
          ((null? ds) (list 'ok (list 'restored written) (list 'already already)))
          ((eq? (car vs) 'already) (loop (cdr ds) (cdr vs) written (+ already 1)))
          (else
           (let* ((d (car ds)) (id (clause 'block d))
                  (text (utf8->string (entry-bytes (string-append dir "/" (clause 'bytes d)))))
                  ;; THE TEXT ARRIVES AS STANDARD INPUT, through the dispatcher's
                  ;; reader, not as an argument: a text beginning `--` would
                  ;; read as an option, and one that is exactly `-` as the
                  ;; placeholder for standard input.
                  (a (rpc-dispatch store (list 'write id "-" "--writer" writer
                                               "--based-on" (clause 'based-on d)
                                               "--working-cut" (format "~s" (clause 'cut d)))
                                   actor #f #f (lambda () text) #f)))
             (unless (and (pair? a) (eq? (car a) 'ok))
               (refuse 'write-refused (list 'block id) (list 'answer a) (list 'restored written)))
             (unless (equal? (clause 'version a) (clause 'version d))
               (refuse 'version-not-reproduced (list 'block id) (list 'dumped (clause 'version d))
                       (list 'written (clause 'version a)) (list 'restored (+ written 1))))
             (loop (cdr ds) (cdr vs) (+ written 1) already))))))))

(define (main args)
  (if (not (and (= 4 (length args)) (string=? (caddr args) "--writer")))
      (begin (write '(usage (drafts-restore <store> <dir> "--writer" <W>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (restore (car args) (cadr args) (cadddr args)))))
        (write a)
        (newline)
        (exit (if (eq? (car a) 'ok) 0 1)))))

(main (cdr (command-line)))
