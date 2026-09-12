#!/bin/sh
# EVERY SCRIPT IN THE DIRECTORY IS RUN, AND EVERY OUTCOME IS PRINTED BY
# NAME. A hand-written list is silent about the file it forgot, and this
# directory's list had forgotten smoke-wire -- red for days, because it
# counts `mismatches` and the list-runner grepped for `failures`.
#
# THE CLASS COMES FROM THE OUTCOME, NOT FROM READING THE SOURCE. Several
# fixtures call (command-line) to find their own path, so "takes
# arguments" cannot be decided by grep: a probe run with no arguments
# prints its usage line and stops, and that is what identifies it.
#
# THREE CRITERIA FOR A FIXTURE, ALL OF THEM: the sentinel "<name>
# complete" says it ran to its end; `failures` and `mismatches` are the
# two counters fixtures use and a non-zero either way is red; and a hard
# line (FAIL / MISMATCH / Exception) is red whatever the counters say.
out=$1
mkdir -p "$out"
bad=0; ran=0; libs=""; probes=""
for f in *.ss; do
  n=${f%.ss}
  if grep -q "^(library (theourgia" "$f"; then libs="$libs $n"; continue; fi
  perl -e 'alarm 900; exec @ARGV' scheme --script "$f" > "$out/$n.out" 2>&1
  rc=$?
  sent=$(grep -c "^$n complete" "$out/$n.out")
  # TWO USAGE SHAPES, BECAUSE THERE ARE TWO KINDS OF CALLER. A probe
  # writes a plain `usage:` line for a person; the CLI answers with one
  # S-expression on stdout whatever happens, so its usage is `(usage
  # ...)`. Forcing the CLI to the probes' shape would break its own
  # contract to make a runner simpler.
  if [ "$sent" = 0 ] && { grep -q "^usage: $n" "$out/$n.out" || grep -q "^(usage " "$out/$n.out"; }; then
    probes="$probes $n"; continue
  fi
  ran=$((ran+1))
  cnt=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/$n.out" | grep -vc "^0 ")
  hard=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/$n.out")
  if [ "$rc" != 0 ] || [ "$sent" = 0 ] || [ "$cnt" != 0 ] || [ "$hard" != 0 ]; then
    bad=$((bad+1))
    printf "RED %-20s rc=%-3s sentinel=%s counters=%s hard=%s\n" "$n" "$rc" "$sent" "$cnt" "$hard"
  fi
done
# AND THE CLASSES ARE COUNTED BACK -- WHICH DETECTS THE DIRECTORY
# CHANGING UNDER THE RUN, not a classifier that drops a file.
#
# The classifier cannot drop one: its last branch catches everything, so
# every script lands in exactly one of the three buckets and the sum
# always equals the count. What can differ is the DIRECTORY, because the
# total below is taken after the loop: a file added between the two makes
# the total larger, a file removed makes it smaller, and a run whose
# pending script is deleted dies with no output at all.
#
# Measured, by running two suites in this directory at once while one of
# them was adding and removing a control fixture. It produced `85 scripts
# in the directory, 86 classified` in one direction and a plausible
# "clean run that exits non-zero" in the other -- the second was reported
# as a defect in an unrelated gate before the cause was found. ONE RUNNER
# AT A TIME IN A DIRECTORY.
total=$(ls *.ss | wc -l | tr -d " ")
nlibs=$(echo $libs | wc -w | tr -d " ")
nprobes=$(echo $probes | wc -w | tr -d " ")
echo "fixtures run: $ran   not-green: $bad"
echo "libraries ($nlibs):$libs"
echo "probes, printed a usage line ($nprobes):$probes"
sum=$((ran + nlibs + nprobes))
if [ "$sum" != "$total" ]; then
  echo "UNACCOUNTED: $total scripts in the directory, $sum classified"
  exit 1
fi
echo "all $total scripts accounted for"

# A COUNT THAT IS PRINTED AND NOT RETURNED IS NOT A CHECK EITHER. `$bad`
# was incremented, printed, and never reached the exit status, so every
# caller that tested this runner's exit code was reading a constant: a
# delivery could be built, pinned and frozen with red fixtures inside it
# and nothing in the chain would object. The only thing standing between
# that and a bad delivery was a person reading the number.
if [ "$bad" != 0 ]; then
  echo "REFUSING: $bad fixture(s) not green"
  exit 2
fi

# A FIXTURE THAT CAN BE KILLED BY AN ANSWER IS NOT A FIXTURE. Rows read
# answers apart, so a seeded defect that changes an answer's SHAPE makes
# the accessor raise while a row is being computed -- and the file ends
# there, every row below it unrun, with no `FAIL` printed at all. Three
# defects were scored as crashes with no failures that way, for answers
# the store had got right and said plainly.
#
# THE GUARD IS `want` BEING A MACRO OVER BOTH SIDES, and every fixture
# that has rows needs it. Nothing else notices its absence: a file
# without it behaves identically until the day a mutant lands on it, and
# then it reports quiet instead of a kill. So the absence is counted
# here rather than left to be discovered.
ungirded=""
for f in *.ss; do
  grep -q "^ *(define-syntax want$" "$f" || continue
  # MATCHED AS A WHOLE LINE, NOT AS A SUBSTRING. Checked with
  # `grep -q "define-syntax caught"` this stayed silent for a file whose
  # macro had been renamed to `caught-disabled` -- the check's own first
  # reading was a false silence, which is the one failure a check of this
  # kind must not have.
  grep -q "^ *(define-syntax caught$" "$f" || ungirded="$ungirded ${f%.ss}"
done
if [ -n "$ungirded" ]; then
  echo "UNGUARDED FIXTURES (rows can end the file instead of failing):$ungirded"
  exit 1
fi
