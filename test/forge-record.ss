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

;; A RECORD THAT ARRIVED FROM SOMEWHERE ELSE.
;;
;; The write path judges what a CALLER may write: a text field's value has
;; to be something a reader can turn into text. Replay judges nothing,
;; because a record already written -- by another build, by a later
;; version, or by something that went wrong -- has to remain readable. A
;; store this build cannot read is a store whose owner has lost it.
;;
;; That division leaves a gap in what a fixture can build. Values like
;; `(conflict 5)` have NO legitimate producer: the caller path refuses them
;; and the reducer never makes one, so the only way such a value reaches a
;; field is as a record from outside. Until this helper existed, a fixture
;; that wanted to ask "does the reader survive one" had to write it through
;; the caller path -- which is to say, through the one door that is
;; supposed to be shut.
;;
;; So this writes the record the way a foreign one arrives: appended to the
;; writer's segment, to be found by replay.
;;
;; THE CHECKSUM COVERS THE DATUM TEXT AND NOTHING ELSE, which was measured
;; rather than assumed. Against a real record written by the store:
;;
;;     want                : dec4e97e
;;     datum only          : dec4e97e      <- this one
;;     datum + newline     : 85b49e41
;;     whole line          : 3667b685
;;     whole line + newline: af33168a
;;
;; A helper that guessed here would write records the reader rejects as
;; corrupt, and a fixture using it would report "the reader refused a bad
;; shape" when what the reader refused was the helper.
;;
;; Needs `(theourgia crc32)` in the including fixture's imports.

;; The sequence number one past the highest the segment already holds.
;; Read out of the file rather than tracked, so a caller does not have to
;; know how many records the store wrote before it.
(define (forge-next-seq text)
  (let loop ((i 0) (best 0))
    (cond
      ((>= i (string-length text)) (+ best 1))
      ((char=? (string-ref text i) #\()
       (let* ((j (let scan ((k (+ i 1)))
                   (if (or (>= k (string-length text))
                           (char=? (string-ref text k) #\space))
                       k
                       (scan (+ k 1)))))
              (n (string->number (substring text (+ i 1) j))))
         (loop (+ i 1) (if (and n (exact? n) (integer? n) (> n best)) n best))))
      (else (loop (+ i 1) best)))))

;; -> the sequence number it wrote. `payload-text` is the payload as it
;; would be spelled in a record, for example
;;
;;     (set "b1.1" title (conflict 5))
(define (forge-record! store payload-text)
  (let* ((wdir (string-append store "/writers"))
         (writers (if (file-exists? wdir) (directory-list wdir) '())))
    (when (null? writers)
      (assertion-violation 'forge-record! "this store has no writer to append to" store))
    (forge-append! (string-append wdir "/" (car writers) "/000001.sexp") payload-text)))

;; THE SAME RECORD, APPENDED TO THE WRITER THE CALLER NAMES. forge-record!
;; takes the first directory under writers/, which is the store's own only
;; while it is the only one: a working view opened for another writer makes
;; a directory of its own, and the first entry is then whichever the
;; listing puts first. -> the sequence number written, or
;; (forge-refused (writer <name>)) when that writer has no segment, so the
;; row that asked compares the refusal rather than stopping.
(define (forge-record-as! store writer payload-text)
  (let ((seg (and (string? writer) (string-append store "/writers/" writer "/000001.sexp"))))
    (if (and seg (file-exists? seg))
        (forge-append! seg payload-text)
        (list 'forge-refused (list 'writer writer)))))

;; Appends one forged record to the segment `seg`. -> its sequence number.
(define (forge-append! seg payload-text)
  (let* ((bytes (call-with-port (open-file-input-port seg)
                  (lambda (i) (let ((b (get-bytevector-all i)))
                                (if (eof-object? b) (make-bytevector 0) b)))))
         (text (utf8->string bytes))
         (seq (forge-next-seq text))
         ;; THE ACTOR SAYS WHERE IT CAME FROM. A record forged by a
         ;; fixture is not one this store's writer produced, and a reading
         ;; that shows the log should say so rather than blend in.
         (datum (string-append "(" (number->string seq)
                               " 1789978200000 \"forged\" () " payload-text ")"))
         (line (string-append (crc32-hex (string->utf8 datum)) " " datum "\n")))
    (call-with-port (open-file-output-port seg (file-options no-fail no-truncate)
                                           (buffer-mode block))
      (lambda (o)
        (set-port-position! o (bytevector-length bytes))
        (put-bytevector o (string->utf8 line))))
    seq))

;; A RECORD FROM A SECOND WRITER, which is how a conflict really happens.
;;
;; Two writers setting one field without having seen each other is the only
;; way the reducer builds a conflict value -- every conflict in these
;; fixtures before this was written by hand, so nothing measured the real
;; path. It cannot be done through the command line: a store refuses a
;; second instance on the same machine (`(error refused (instance machine))`,
;; measured), which is the right behaviour and leaves a fixture no route.
;;
;; An UNTRACKED record carries no cursor, and no cursor means the writer had
;; seen nothing -- which is exactly the concurrency a conflict needs. So this
;; makes a second writer directory and puts one such record in it. Measured
;; on a block whose keywords writer A had already set:
;;
;;   (keywords conflict (("griffel" "teaap58b" 2) ("slantwise" "zzforged" 1)))
;;
;; NOTHING HERE IS A DIGEST. A tracked record carries one, and forging that
;; would be guessing at an envelope in order to test what is inside it. An
;; untracked record has no such field, which is why this route is honest.
(define (forge-writer-record! store writer payload-text)
  (let ((dir (string-append store "/writers/" writer)))
    (unless (file-exists? dir)
      (system (string-append "mkdir -p '" dir "'"))
      ;; The owner file is copied from the store's own writer: it names the
      ;; machine and instance, and a reader that finds none treats the
      ;; segment as unowned.
      (let ((wdir (string-append store "/writers")))
        (let loop ((ws (directory-list wdir)))
          (cond
            ((null? ws) #f)
            ((string=? (car ws) writer) (loop (cdr ws)))
            (else
              (system (string-append "cp '" wdir "/" (car ws) "/owner.sexp' '"
                                     dir "/owner.sexp'")))))))
    (let* ((datum (string-append "(1 1789986000000 \"forged\" () " payload-text ")"))
           (line (string-append (crc32-hex (string->utf8 datum)) " " datum "\n")))
      (call-with-port (open-file-output-port (string-append dir "/000001.sexp")
                                             (file-options no-fail) (buffer-mode block))
        (lambda (o) (put-bytevector o (string->utf8 line))))
      writer)))
