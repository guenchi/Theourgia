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

# A FIXTURE MAY NOT SHARE A BASENAME WITH A LIBRARY, and this refuses
# before the run rather than after the delivery.
#
# A DELIVERY IS ONE FLAT DIRECTORY -- the libraries sit beside the
# fixtures -- so two files called `code-suggest.ss` cannot both be in it.
# Building one copied the fixtures and then the libraries into the same
# place and the second copy silently replaced the first; which of the two
# survived depended on the order, and NEITHER outcome is necessarily red:
# a missing library takes the whole suite down in a way that looks like a
# broken import, and a missing fixture is simply a smaller total that
# nothing compares against.
#
# IT ALSO BROKE A FIXTURE IN THE REPOSITORY, where the two directories
# are separate. `code-text-audit.ss` looks for a library beside itself
# before looking one level up, found `test/code-suggest.ss`, walked the
# fixture instead of the library, found nothing to complain about, and
# printed four green rows.
#
# WHERE IT CANNOT RUN, IT SAYS SO. In a delivery there is no separate
# library directory to compare against -- by then the clash has already
# happened -- so this prints that it did not run instead of printing
# nothing, which would read as a pass.
# WHAT COUNTS AS A LIBRARY DIRECTORY IS "IT HOLDS SOURCES", not "it
# holds cli.ss". Keying the whole comparison on one filename meant a
# parent full of libraries with that one file missing or renamed took
# the NOT CHECKED branch and the run continued.
# THE LIBRARY DIRECTORY IS CHECKED BEFORE ANYTHING RUNS. The core reaches
# igropyr through three facades, so a pin that holds only theourgia/ --
# or an igropyr/ that is there but cannot answer -- produces a hundred
# fixtures at rc=255, every one of them saying a library was not found.
# That reads as a broken tree. One line naming what is missing is the
# difference between a variable to fix and an afternoon of bisecting.
#
# THREE THINGS, BECAUSE THEY FAIL SEPARATELY: the directory is there, the
# library this core actually imports from is there, and it exports the
# name this core actually uses. The last one is what tells a WRONG
# igropyr from a missing one.
pinned=${THEOURGIA_LIBDIR:-$CHEZSCHEMELIBDIRS}
if [ -n "$pinned" ]; then
  first=$(printf %s "$pinned" | cut -d: -f1)
  if [ ! -d "$first/igropyr" ]; then
    echo "LIBRARY PATH: $first holds no igropyr/ -- the core imports (igropyr crypto),"
    echo "  (igropyr sexpr) and (igropyr platform) through its three facades."
    echo "REFUSING: nothing below this line would be a reading."
    exit 1
  fi
  if [ ! -f "$first/igropyr/crypto.sc" ]; then
    echo "LIBRARY PATH: $first/igropyr has no crypto.sc -- (igropyr crypto) cannot resolve."
    echo "REFUSING: nothing below this line would be a reading."
    exit 1
  fi
  if ! grep -q "sha256" "$first/igropyr/crypto.sc"; then
    echo "LIBRARY PATH: $first/igropyr/crypto.sc does not mention sha256 -- this is an"
    echo "  igropyr, but not one this core can use."
    echo "REFUSING: nothing below this line would be a reading."
    exit 1
  fi
  echo "library path: $first holds igropyr/ and theourgia/"
fi

libdir=..
if [ "$(cd "$libdir" && pwd)" != "$(pwd)" ] && ls "$libdir"/*.ss > /dev/null 2>&1; then
  clash=""
  for f in *.ss; do
    [ -f "$libdir/$f" ] && clash="$clash $f"
  done
  if [ -n "$clash" ]; then
    echo "NAME CLASH: fixtures sharing a basename with a library in $(cd "$libdir" && pwd):$clash"
    echo "REFUSING: a flat delivery cannot hold both copies."
    exit 1
  fi
  echo "fixture/library names: no clash against $(ls "$libdir"/*.ss | wc -l | tr -d " ") libraries"
else
  echo "fixture/library names: NOT CHECKED -- no separate library directory beside this one"
fi
# THE CHEAP GATE THAT NAMES A CAUSE RUNS BEFORE THE ONES THAT SHOW A
# SYMPTOM. This is the same repair as moving the structural gates above
# the verdict: a check is worth what it is worth AT THE MOMENT IT RUNS.
#
# `expansion-branches.ss` loads each expansion branch of this tree -- the
# one with THEOURGIA_INJECT unset and the one with it on -- and answers
# in about two seconds, naming the file and line when a branch will not
# build. Nothing else here asks that question early.
#
# MEASURED, AND THIS IS WHY IT IS FIRST. A facade change left
# `string-contains?` unbound inside the injected branch of `ffi.ss`.
# Every ordinary fixture stayed green. `cli1`, which sorts EARLIER than
# `expansion-branches`, starts two children that then died on load,
# spun out its bounded wait for them, and blocked forever writing to a
# fifo with no reader -- so the suite read the defect as a 900-second
# alarm, once per fault-injection fixture, three and a half hours before
# reaching the two-second gate that names it.
#
# ⛔ A RED PREFLIGHT STOPS THE RUN. Every later reading would be about a
# tree that cannot be built in one of the two shapes it ships in.
#
# It is NOT excluded from the loop below: it runs again there, as an
# ordinary fixture, so that the classifier still sees every script in
# this directory exactly once and the count-back gate stays true. Two
# seconds is a cheap price for leaving that invariant alone.
# ⛔ AND IT IS NOT JUDGED BY ITS EXIT STATUS. Measured: on a tree whose
# injected branch would not build, `expansion-branches.ss` printed the
# unbound identifier, the file and the line -- and exited 0. It has no
# `(exit ...)` at all, and neither do fifty-eight of the other fixtures
# here: THIS SUITE DECIDES ON OUTPUT, not on status, and the loop below
# says so at length. A preflight that trusted `if scheme --script ...`
# would have been green on the very tree that produced it.
#
# So the preflight applies the loop's own four measures, in one place
# rather than two: the sentinel, the hard lines, the counters, the
# status.
if [ -f expansion-branches.ss ]; then
  perl -e 'alarm 120; exec @ARGV' scheme --script expansion-branches.ss > "$out/preflight.out" 2>&1
  pf_rc=$?
  pf_sent=$(grep -c "^expansion-branches complete" "$out/preflight.out")
  pf_hard=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/preflight.out")
  pf_cnt=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/preflight.out" | grep -vc "^0 ")
  if [ "$pf_rc" = 0 ] && [ "$pf_sent" != 0 ] && [ "$pf_hard" = 0 ] && [ "$pf_cnt" = 0 ]; then
    echo "preflight: every expansion branch of this tree builds"
  else
    echo "PREFLIGHT RED (rc=$pf_rc sentinel=$pf_sent hard=$pf_hard counters=$pf_cnt)"
    echo "  -- an expansion branch of this tree does not build:"
    sed "s/^/  /" "$out/preflight.out"
    echo "REFUSING: nothing below this line would be a reading."
    exit 1
  fi
else
  echo "preflight: NOT CHECKED -- expansion-branches.ss is not in this directory"
fi

# THE SECOND PREFLIGHT, AND IT ANSWERS A QUESTION NOTHING ELSE ASKS.
#
# A MISSING CLOSER IS NOT A SYNTAX ERROR: the reader takes it, and the
# definitions after the short form become part of its body. What comes
# out is an "unbound identifier" naming something defined far below,
# reported where it is USED. Measured on daemon.ss, 2026-09-18: one `)`
# swallowed twenty-six definitions and the report was `unbound
# identifier directory-of at line 219`, a hundred and eighty lines from
# the cause -- and the whole file still balanced, because a second edit
# had one closer too many.
#
# ⛔ IT RUNS BEFORE THE LOOP for the same reason expansion-branches does:
# a tree in that state produces a hundred fixtures failing to import
# something, which reads as a broken environment. One line naming the
# file and the first definition that was swallowed is the difference.
#
# ⚠️ IT IS NOT IN `*.ss`, so it does not run again in the loop and the
# count-back below is untouched.
if [ -f structure.py ]; then
  perl -e 'alarm 120; exec @ARGV' python3 structure.py > "$out/structure.out" 2>&1
  st_rc=$?
  st_sent=$(grep -c "^structure complete" "$out/structure.out")
  st_hard=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/structure.out")
  st_cnt=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/structure.out" | grep -vc "^0 ")
  if [ "$st_rc" = 0 ] && [ "$st_sent" != 0 ] && [ "$st_hard" = 0 ] && [ "$st_cnt" = 0 ]; then
    echo "preflight: $(grep '^checked ' "$out/structure.out"), every definition where its parentheses say"
    grep "^NOT CHECKED " "$out/structure.out" | sed "s/^/  /"
  else
    echo "PREFLIGHT RED (rc=$st_rc sentinel=$st_sent hard=$st_hard counters=$st_cnt)"
    echo "  -- a form in this tree does not close where it looks like it does:"
    sed "s/^/  /" "$out/structure.out"
    echo "REFUSING: nothing below this line would be a reading."
    exit 1
  fi
else
  echo "preflight: NOT CHECKED -- structure.py is not in this directory"
fi

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
  # AND A USAGE LINE IS NOT A PLACE TO HIDE. The probe branch skipped
  # every failure check below it, so a script that printed its usage and
  # THEN failed was filed as a probe and the run exited zero. Measured on
  # the classifier alone, three ways: rc=255 with an exception after the
  # usage line; rc=1 with nothing else at all; and rc=0 with `1 failures`
  # printed after it. All three gave `bad=0 ran=0` and a clean exit.
  # A probe has to have ended cleanly BY EVERY MEASURE THIS RUNNER HAS --
  # no hard line, no non-zero counter, and one of the two exit statuses
  # this directory's probes actually use.
  #
  # ⛔ AND THE TWO ARE MEASURED, NOT CHOSEN. Requiring 0 alone reclassified
  # nine of the fourteen probes as red in one run -- `barrier-probe`,
  # `dirfault`, `dirflush`, `fault-file`, `fault-pipe`, `probe`, `row`,
  # `shared-lock` and `stagefault` all print `usage: <name>.ss ...` and
  # exit 2, which is their convention; `q8-report` and the five scripts
  # added this batch exit 0. Both are usage exits and neither is a
  # failure. Anything else -- including a signal, which is 128 and up --
  # is not a probe and goes through the checks below.
  hard_here=$(grep -c "^FAIL\|^MISMATCH\|^Exception" "$out/$n.out")
  cnt_here=$(grep -E "^[0-9]+ (failures|mismatches)" "$out/$n.out" | grep -vc "^0 ")
  if [ "$sent" = 0 ] && [ "$hard_here" = 0 ] && [ "$cnt_here" = 0 ] \
     && { [ "$rc" = 0 ] || [ "$rc" = 2 ]; } \
     && { grep -q "^usage: $n" "$out/$n.out" || grep -q "^(usage " "$out/$n.out"; }; then
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

# THE TWO STRUCTURAL CHECKS BELOW RUN BEFORE THE REFUSAL, NOT AFTER IT.
# They were written after it, and this suite has three fixtures that are
# red by design -- so `exit 2` fired first on every single run and
# NEITHER of them had ever executed. A check placed after an exit that
# always happens is a check that has never had a first reading. They ask
# about the SHAPE of the fixture set, which is a fact about the
# directory and not about whether today's run was green, so they belong
# ahead of the verdict.

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
# A FIXTURE THAT COUNTS ITS ROWS MUST HAVE A BASELINE LINE. The count is
# the only thing that tells "this mutant was caught" from "this mutant
# killed the file after four rows", and it is useless without a reading
# of the same file on unmutated code. `rows-baseline.txt` is a list of
# names, and a list of names is silent about the one it does not have:
# fourteen fixtures added in one batch printed a row count that nothing
# compared against, for as long as nobody counted the lines.
# THE TABLE ITSELF HAS TO BE THERE. `while read < missing-file` prints a
# redirection error, runs its body zero times, and leaves every variable
# empty -- which then reads as "everything matches".
if [ ! -r rows-baseline.txt ]; then
  echo "NO ROW BASELINE TABLE: rows-baseline.txt is missing or unreadable"
  exit 1
fi
missing=""
for f in *.ss; do
  n=${f%.ss}
  [ -f "$out/$n.out" ] || continue
  grep -q "^rows: " "$out/$n.out" || continue
  grep -q "^$n " rows-baseline.txt 2>/dev/null || missing="$missing $n"
done
if [ -n "$missing" ]; then
  echo "NO ROW BASELINE (the fixture counts rows and rows-baseline.txt does not list it):$missing"
  exit 1
fi

# AND A BASELINE LINE IS ABOUT A PARTICULAR VERSION OF A PARTICULAR FILE.
# The md5 is the column that says so, and nothing in a normal run had
# ever compared it: `vendored`'s hash had been wrong since the commit
# that removed the external dependency, and the table went on printing a
# confident number about a file it was no longer describing.
#
# ONLY THE MD5 IS A REFUSAL HERE. The other two columns are readings of a
# RUN, and this suite has fixtures whose output length depends on the
# machine -- `cli3`, `q8`, `smoke-ffi` and `wire-outside` all differ by a
# line or more between environments. A hash does not: it is a fact about
# the file. So a changed hash stops the run, and a changed count is
# printed for a person to read.
stale=""; drift=""
# `|| [ -n "$name" ]` KEEPS THE LAST LINE when the file does not end in a
# newline: `read` returns non-zero there although it has filled the
# variables, so the final record was silently skipped -- by the hash
# check, which is the one that refuses.
while read -r name rows lines digest || [ -n "$name" ]; do
  [ -n "$name" ] || continue
  [ -f "$name.ss" ] || { stale="$stale $name(gone)"; continue; }
  now=$(md5 -q "$name.ss")
  if [ "$now" != "$digest" ]; then stale="$stale $name"; continue; fi
  [ -f "$out/$name.out" ] || continue
  r=$(grep "^rows: " "$out/$name.out" | tail -1 | sed "s/^rows: //")
  [ -n "$r" ] || r="-"
  l=$(wc -l < "$out/$name.out" | tr -d " ")
  [ "$r" = "$rows" ] && [ "$l" = "$lines" ] || drift="$drift $name($rows/$lines->$r/$l)"
done < rows-baseline.txt
if [ -n "$stale" ]; then
  echo "ROW BASELINE IS ABOUT ANOTHER VERSION of these fixtures (md5 differs):$stale"
  exit 1
fi
if [ -n "$drift" ]; then
  echo "row baseline, counts that differ in this environment:$drift"
else
  echo "row baseline: every listed fixture matches its recorded hash and counts"
fi
# AND THE GATE ABOVE ONLY SEES FIXTURES THAT PRINT A COUNT. The ones that
# do not are invisible to it -- the same shape of silence it exists to
# close -- so they are counted and named here rather than left out.
counted=0; uncounted=""
for f in *.ss; do
  n=${f%.ss}
  [ -f "$out/$n.out" ] || continue
  case " $libs $probes " in *" $n "*) continue;; esac
  if grep -q "^rows: " "$out/$n.out"; then counted=$((counted+1))
  else uncounted="$uncounted $n"; fi
done
echo "row counts: $counted fixture(s) print one; $(echo $uncounted | wc -w | tr -d " ") do not:$uncounted"

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
# AND THE CHECK ABOVE ONLY LOOKS AT FIXTURES THAT DECLARED THE MACRO.
# A fixture whose `want` is a PROCEDURE has both arguments evaluated
# before the call, so it is unguarded by construction and invisible to a
# test that asks "did you declare `caught`". Twenty of them are in this
# directory. Making them red would redden most of the suite at once, so
# they are counted and named -- the absence is a reading rather than a
# silence, and the number is what a decision can be made against.
byproc=""
for f in *.ss; do
  grep -q "^ *(define (want " "$f" || continue
  byproc="$byproc ${f%.ss}"
done
echo "unguarded by construction ($(echo $byproc | wc -w | tr -d " ") fixtures define want as a procedure):$byproc"

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

