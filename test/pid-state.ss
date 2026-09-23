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

;; IS A PROCESS GONE? One definition, included by every fixture that waits
;; for a process it started (facade-proc, cli-forward).
;;
;; THE RULE IS THE RUNNER'S alive() IN run-fixtures.sh, the same rule in
;; shell: gone only if a signal-0 probe (`kill -0`) fails, or if a ps that
;; EXITED 0 printed Z -- a zombie has ended and waits only to be reaped.
;; A ps that failed, or printed nothing, for a pid the probe still finds,
;; is NOT proof of anything, and the answer is "not gone", so a bounded
;; wait runs to its bound (codex r8 B1, r9 C2).
;;
;; EPERM FROM THE PROBE IS READ AS GONE, and that is right HERE ONLY: every
;; pid asked about is the run's own child, which the run may always signal,
;; so a probe refused permission means the pid now names someone else's
;; process and ours has gone. The same probe means ALIVE in a lock-owner
;; check, where the pid was never our child; this rule is not for that use.
;;
;; The ps runs under the same 5 s alarm as the runner's bps: a ps that hangs
;; is a ps that failed.
;;
;; NEVER: A SECOND COPY OF THIS RULE IN A FIXTURE. It was written three
;; times, and one of the copies read a failed ps as an exit.
(define (pid-gone? pid)
  (let ((n (number->string pid)))
    (zero? (system (string-append
                     "kill -0 " n " 2>/dev/null || exit 0; "
                     "st=$(perl -e 'alarm shift; exec { \"ps\" } \"ps\", @ARGV; exit 127' "
                     "5 -o stat= -p " n " 2>/dev/null) || exit 1; "
                     "case \"$st\" in Z*) exit 0;; esac; exit 1")))))
