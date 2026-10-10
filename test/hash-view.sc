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

;; A HASH IS OVER WHAT WAS WRITTEN. A VIEW IS OVER HOW WE READ IT TODAY.
;;
;; `register-language!` edits a table at runtime. Until v129 the derived
;; fields -- a text block's `name` and `doc` -- were produced inside the
;; reduction, so that table was part of what a stored block MEANT: adding
;; a language, or correcting one, could change `block-hash` for blocks
;; already in the log, and every `--based-on` taken against them would go
;; stale with nothing written anywhere to say why.
;;
;; THE ROWS ARE A PAIR, AND NEITHER ONE ALONE SAYS ANYTHING.
;;
;;   HV-01  registering a language does NOT change block-hash or
;;          state-hash.
;;   HV-02  registering that same language DOES change what `view-read`
;;          answers for the same block.
;;
;; HV-01 on its own is what a `register-language!` that did nothing at
;; all would produce, and that is the likelier defect: the entry is
;; rejected, or shadowed, or the block's `lang` never matched it. HV-02
;; is what makes HV-01 a statement about the split rather than about a
;; call that missed.
;;
;; HV-03 is the other direction: a byte of `src` is part of what was
;; written, so changing one MUST change the hash. Without it, a
;; `block-hash` that returned a constant would pass HV-01.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia view) (theourgia languages) (theourgia ffi))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))

;; BOTH SIDES OF A ROW ARE GUARDED, so a question that cannot be
;; answered comes back as a red row naming it rather than as a dead
;; fixture with no sentinel. `run-fixtures.sh` counts the fixtures here
;; whose `want` is a bare procedure, for that reason.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/hash-view-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define input (string-append root "/input"))
(mkdir-p! input)
(rpc-dispatch store '(init) "test")

(define (write! path source)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p (string->utf8 source)))))

;; THE LANGUAGE IS INVENTED HERE so that the rows are about registration
;; rather than about whichever of the ten shipped entries happens to
;; match. `zz` is not in the catalog and no fixture writes a `.zz` file.
;;
;; An entry is built by taking a shipped one and replacing three of its
;; properties, so this fixture does not carry a second copy of the entry
;; format: a change to that format reaches these rows the same way it
;; reaches the catalog.
(define (respell entry lang extensions heads)
  (map (lambda (p)
         (case (car p)
           ((lang) (list 'lang lang))
           ((extensions) (list 'extensions extensions))
           ((def-heads) (list 'def-heads heads))
           (else p)))
       entry))

(define scheme-entry (language-for-name "scheme"))
(want "HV-00 the catalog has the entry this fixture builds on"
      (and scheme-entry #t) #t)

;; TWO SPELLINGS OF THE SAME LANGUAGE. Both find a definition head in
;; the source below; they capture a DIFFERENT name from it.
(define source "(define (alpha x) x)\n")
(define spelling-a
  (respell scheme-entry "zz" '("zz") '("^\\(define\\s+\\(([^\\s()\\[\\]\";]+)")))
(define spelling-b
  (respell scheme-entry "zz" '("zz") '("^\\(([^\\s()\\[\\]\";]+)")))

(register-language! spelling-a)
(write! (string-append input "/demo.zz") source)
(define imported (rpc-dispatch store (list 'import-code input) "test"))
(want "HV-00 the invented language imports" (rpc-ok? imported) #t)

(define (state) (open-and-reduce store))

(define (field read id key)
  (let* ((b (read (state) id))
         (p (and b (assq key (cdr (assq 'fields b))))))
    (and p (cdr p))))

;; NOTE: IMPORTING ONE FILE STORES TWO BLOCKS: a `file` wrapper and the
;; `code` block inside it. The first version of this took the first id
;; `state-datum` returned, which is the WRAPPER -- and `view-read`
;; passes a non-`code` block through untouched, so every derived field
;; read as absent and HV-02 looked like a split that had gone too far.
;; The block is chosen by its `kind`, and HV-00 below refuses to
;; continue if there is not exactly one of them.
(define code-ids
  (filter (lambda (id) (eq? 'code (field state-read id 'kind)))
          (map cadr (state-datum (state)))))
(want "HV-00 exactly one code block was stored" (length code-ids) 1)
(define the-id (and (pair? code-ids) (car code-ids)))

(want "HV-00 and it was stored under the invented language"
      (field state-read the-id 'lang) 'zz)

(define name-before (field view-read the-id 'name))
(define hash-before (block-hash (state) the-id))
(define state-before (state-hash (state)))

(want "HV-00 the first spelling reads a name out of the source"
      name-before "alpha")

(register-language! spelling-b)

(define name-after (field view-read the-id 'name))
(define hash-after (block-hash (state) the-id))
(define state-after (state-hash (state)))

(want "HV-01 registering a language does not change block-hash"
      hash-after hash-before)
(want "HV-01 registering a language does not change state-hash"
      state-after state-before)

(want "HV-02 TWIN: it does change what the view answers"
      (list name-before name-after) '("alpha" "define"))

;; HV-01b. THE SAME QUESTION ALONG THE OTHER AXIS, AND IT IS HERE
;; BECAUSE TWO SEEDED DEFECTS SURVIVED WITHOUT IT.
;;
;; `register-language!` above REPLACES a same-named entry, so the table
;; keeps its length and each entry keeps its shape. A `block->datum`
;; seeded to carry `(length (language-table))`, and another seeded to
;; carry the length of each entry, both left every row above green:
;; the hash really did depend on the language table, and the only edit
;; the fixture made was one the dependency could not see. A cell is only
;; as strong as the axis its change moves along.
;;
;; Registering a language the table does not have moves the other axis.
;; Both seeds die here; the content-sensitive one dies in both places.
(define fresh-entry (respell scheme-entry "qq" '("qq") '("^\\(define")))
(define table-before (length (language-table)))
(register-language! fresh-entry)
(want "HV-01b TWIN: registering a new language really does grow the table"
      (> (length (language-table)) table-before) #t)
(want "HV-01b a new language does not change block-hash"
      (block-hash (state) the-id) hash-before)
(want "HV-01b a new language does not change state-hash"
      (state-hash (state)) state-before)

;; HV-03. THE OTHER DIRECTION, ON THE SAME BLOCK.
;;
;; NOTE: IT HAS TO BE THE SAME BLOCK. The first version of this imported a
;; second file with one byte changed and compared the two hashes --
;; which proves nothing: `block-hash` is taken over `block->datum`, and
;; that includes the block's `position` and `edges`, so TWO DISTINCT
;; BLOCKS NEVER HASH ALIKE whatever their source. Measured: two files
;; with byte-identical contents hashed differently, and the row that
;; expected them to match was the one that said so.
;;
;; So the source is changed in place, through the same verb an editor
;; would use, and the hash of one block is compared with itself before
;; and after. That is the exact shape of HV-01, with the thing being
;; changed swapped from the language table to the log.
(define src-before (field state-read the-id 'src))
(define hash-before-edit (block-hash (state) the-id))
(want "HV-03 the block is still the one HV-01 measured"
      (equal? hash-before-edit hash-before) #t)

(define edit (rpc-dispatch store (list 'set the-id "src" "(define (alphb x) x)\n") "test"))
(want "HV-03 the source edit is accepted" (rpc-ok? edit) #t)
(want "HV-03 and it really changed the stored source"
      (equal? (field state-read the-id 'src) src-before) #f)
(want "HV-03 CONTROL: one byte of src DOES change block-hash"
      (equal? (block-hash (state) the-id) hash-before-edit) #f)

;; AND THE DERIVED FIELDS ARE NOT WRITABLE, which is the other half of
;; the same rule: if `doc` could be set, it would be a stored field with
;; a second source of truth.
(want "HV-04 a derived field cannot be written"
      (cadr (rpc-dispatch store (list 'set the-id "doc" "wrong") "test"))
      'derived-field)

(printf "rows: ~a\n~a failures\nhash-view complete\n" rows bad)
