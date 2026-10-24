;; Copyright 2018 - 2026 guenchi
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

;; A FIXTURE'S WAIT ON A CHILD IS A CLOCK, NOT A COUNT.
;;
;; A fixture that starts a child and must not go on until the child has
;; reached a point -- a barrier line in its trace, a `lock-wait`, a first
;; line of output -- waits here. Each procedure returns shell text that
;; sets the shell variable `w` to `yes` when the point was seen and to
;; `no` when the bound passed first; the caller writes that text into the
;; script it runs, and reads `w` or ignores it.
;;
;; NOTE: THEY USED TO BE SHELL SPINS of a few thousand to a few hundred
;; thousand iterations. A spin measures the machine, not the time: on a
;; slow runner the child had not reached its point when the spin ran out,
;; and the fixture went on as if it had -- a writer read as never having
;; waited on a lock it was about to wait on, while the fixture alone was
;; green. A poll looks every 50 ms and gives up only once the bound has
;; passed, so what a row discriminates is unchanged and a slow machine
;; only makes the `yes` arrive later.
;;
;; THE BOUND IS ON CLOCK_MONOTONIC, as every deadline in launch.pl is: the
;; wall clock can be stepped (NTP, a manual set, a resume), and a step
;; forward would end the poll early, which is the false `no` this
;; replaces. The poll is one perl process, and it answers `no` only from a
;; look taken after BOUND monotonic seconds, never before. A poll that
;; cannot run answers nothing, so `w` is empty and matches no reading a
;; row wants.
;;
;; NOTE: AND IT CARRIES ITS OWN ALARM, BOUND + 10 SECONDS. It is a child of
;; the caller's shell, and an alarm on that shell reaches the shell, not
;; its children. A poll that never ended would otherwise outlive the
;; fixture; with the alarm it dies and prints nothing, so `w` is empty.
;; retire-race.sc's three rows read `w`, so there the row reads red.
;; cli1.sc's five waits ignore `w`: the fixture goes on, and its later
;; rows judge what the child did.
;;
;; WHAT IS LOOKED FOR. With WORDS, the file holds any one of them (a plain
;; substring, as `grep -q` with a fixed word, or `grep -qE 'a|b'`, matched).
;; With no words, the file has content -- the shell's `[ -s file ]`, for a
;; wait on a child's first output.
;;
;; THE ROWS THAT PIN THIS TEXT live in the fixtures that load it:
;; retire-race.sc's CONTROL pins the bound for every caller: a poll that
;; ignores its bound, or never answers no, reads red there. No row pins
;; the alarm by itself: a poll without it still ends at its bound.
;; cli1.sc's file-wait CONTROL pins the no-words form.
;;
;; It is loaded, not included, for the reason `import-walk.sc` gives: each
;; caller computes its own directory and hands `load` the path. It is a
;; `.ss`, as the other loaded helpers here are, so the runner -- which
;; runs every `*.sc` and `*.py` -- never takes it for a fixture.

;; KEY: THE ARGUMENTS GO INTO SINGLE QUOTES, SO A QUOTE IN ONE WOULD END
;; THEM. That is refused here, when the text is built, and so is a bound
;; that is not a whole number of seconds.
(define (trace-poll-check! who words file bound)
  (define (quote-free? s)
    (and (string? s)
         (let loop ((i 0))
           (or (= i (string-length s))
               (and (not (char=? (string-ref s i) #\'))
                    (loop (+ i 1)))))))
  (unless (and (list? words) (for-all quote-free? words))
    (assertion-violation who "every word must be a string with no single quote" words))
  (unless (quote-free? file)
    (assertion-violation who "the file must be a string with no single quote" file))
  (unless (and (integer? bound) (exact? bound) (>= bound 0))
    (assertion-violation who "the bound must be an exact whole number of seconds" bound)))

;; (trace-wait-text words file bound): shell text setting `w` to yes or no.
(define (trace-wait-text words file bound)
  (trace-poll-check! 'trace-wait-text words file bound)
  (string-append
    "w=$(perl -MTime::HiRes=clock_gettime,CLOCK_MONOTONIC,sleep -e '"
    "my ($bound, $file, @words) = @ARGV; alarm($bound + 10); "
    "my $end = clock_gettime(CLOCK_MONOTONIC) + $bound; "
    "while (1) { "
    "my $past = clock_gettime(CLOCK_MONOTONIC) >= $end; "
    "if (open(my $f, \"<\", $file)) { local $/; my $t = <$f>; close($f); "
    "if (defined($t) && (@words ? scalar(grep { index($t, $_) >= 0 } @words) : length($t) > 0)) "
    "{ print \"yes\"; exit 0; } } "
    "if ($past) { print \"no\"; exit 0; } "
    "sleep(0.05); }' "
    (number->string bound) " '" file "'"
    (apply string-append (map (lambda (word) (string-append " '" word "'")) words))
    ")\n"))

;; (trace-poll word trace-file report-file label bound): the wait for one
;; word, and "<label> <yes|no>" appended to REPORT-FILE.
(define (trace-poll word trace-file report-file label bound)
  (trace-poll-check! 'trace-poll (list word label) report-file bound)
  (string-append
    (trace-wait-text (list word) trace-file bound)
    "echo \"" label " $w\" >> '" report-file "'\n"))

;; (file-wait-text file bound): the wait for FILE to have content.
(define (file-wait-text file bound)
  (trace-wait-text '() file bound))
