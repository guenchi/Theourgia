#!/bin/sh
# THE ROW BASELINE CHECK, AS SOMETHING THAT CAN BE RUN ON ITS OWN.
#
# It used to be a block inside `run-fixtures.sh`, which meant the only way
# to see what it says was to run the whole suite -- forty minutes for a
# reading about four lines of text. A check nobody can drive on made-up
# input is a check whose wording nobody ever measures, and that is exactly
# how the defect this file was split out to fix survived: the summary line
# claimed to have compared the COUNTS while the line under it named two
# fixtures whose counts did not match.
#
# Usage, from the fixture directory:
#
#     sh row-baseline-check.sh <output-directory>
#
# It reads `rows-baseline.txt` and `<output-directory>/<name>.out` from the
# directory it is run in, prints what it found, and exits 1 when it refuses.
#
# WHAT REFUSES AND WHAT DOES NOT. The md5 column is a fact about a file, so
# a changed hash means the recorded counts describe a file that no longer
# exists, and that refuses. The line column is a reading of a RUN, and this
# suite has fixtures whose output length depends on the machine -- so a line
# count that differs is printed for a person and does not refuse.
#
# THE ROW COLUMN REFUSES. A row is one assertion the file made, and on an
# unchanged file the number it makes does not depend on the machine: the
# suite on a second machine differed in lines for four fixtures and in rows
# only where the table was stale. A row count that differs on an unchanged
# hash is a table nobody corrected, or rows that stopped running -- and when
# both counts were readings, two entries stayed wrong for days with every
# gate green.
#
# EACH LINE SAYS WHICH COLUMN IT IS ABOUT. Both lines begin `row baseline,
# hashes:` or `row baseline, counts:` so that neither can be read as
# covering the other. The line that refuses is the one about hashes, and it
# must not mention counts at all: a reader quotes the summary line, and a
# summary line that asserts a check it did not perform is worse than no
# summary line, because it is believed.
out=$1
if [ -z "$out" ]; then
  echo "row-baseline-check.sh: needs the output directory as its first argument"
  exit 2
fi
# THE TABLE ITSELF HAS TO BE THERE. `while read < missing-file` prints a
# redirection error, runs its body zero times, and leaves every variable
# empty -- which then reads as "everything matches".
if [ ! -r rows-baseline.txt ]; then
  echo "NO ROW BASELINE TABLE: rows-baseline.txt is missing or unreadable"
  exit 1
fi

# A FIXTURE THAT COUNTS ITS ROWS MUST HAVE A BASELINE LINE. The count is
# the only thing that tells "this mutant was caught" from "this mutant
# killed the file after four rows", and it is useless without a reading of
# the same file on unmutated code. `rows-baseline.txt` is a list of names,
# and a list of names is silent about the one it does not have: fourteen
# fixtures added in one batch printed a row count that nothing compared
# against, for as long as nobody counted the lines.
missing=""
for f in *.sc; do
  n=${f%.sc}
  [ -f "$out/$n.out" ] || continue
  grep -q "^rows: " "$out/$n.out" || continue
  grep -q "^$n " rows-baseline.txt 2>/dev/null || missing="$missing $n"
done

# THE FINDINGS ARE COLLECTED AND PRINTED TOGETHER, and the refusal comes at
# the end. This used to exit early, which put the checks in SERIES: a run
# with two unnamed fixtures stopped before any md5 was compared, so sixteen
# fixtures whose contents had changed were never looked at. A missing NAME
# hid every stale HASH behind it.
stale=""; drift=""; rowsbad=""; matched=0
# `|| [ -n "$name" ]` KEEPS THE LAST LINE when the file does not end in a
# newline: `read` returns non-zero there although it has filled the
# variables, so the final record was silently skipped -- by the hash check,
# which is the one that refuses.
while read -r name rows lines digest || [ -n "$name" ]; do
  [ -n "$name" ] || continue
  [ -f "$name.sc" ] || { stale="$stale $name(gone)"; continue; }
  now=$(md5 -q "$name.sc")
  if [ "$now" != "$digest" ]; then stale="$stale $name"; continue; fi
  [ -f "$out/$name.out" ] || continue
  r=$(grep "^rows: " "$out/$name.out" | tail -1 | sed "s/^rows: //")
  [ -n "$r" ] || r="-"
  l=$(wc -l < "$out/$name.out" | tr -d " ")
  if [ "$r" != "$rows" ]; then
    rowsbad="$rowsbad $name($rows->$r)"
  elif [ "$l" = "$lines" ]; then
    matched=$((matched+1))
  else
    drift="$drift $name($rows/$lines->$r/$l)"
  fi
done < rows-baseline.txt

if [ -n "$missing" ]; then
  echo "NO ROW BASELINE (the fixture counts rows and rows-baseline.txt does not list it):$missing"
fi

# THE HASH LINE. It announces that it ran: silence here used to be
# indistinguishable from "this check did not get to run", which is exactly
# what had been happening.
if [ -n "$stale" ]; then
  echo "row baseline, hashes: these fixtures are NOT the version the table describes (md5 differs):$stale"
elif [ -n "$missing" ]; then
  echo "row baseline, hashes: every LISTED fixture is the version the table describes (the unlisted ones named above are compared against nothing)"
else
  echo "row baseline, hashes: every listed fixture is the version the table describes"
fi

# THE COUNT LINE, PRINTED EVERY TIME. It used to appear only when something
# drifted, which left the hash line as the only word on counts -- and that
# line said "and counts" whether or not any count had been compared. A
# check that is silent when it passes cannot be told from one that did not
# run, and that silence is what let the wrong claim stand.
if [ -n "$rowsbad" ]; then
  echo "row baseline, rows: these fixtures ran a different number of rows than the table records for the same file, which refuses:$rowsbad"
fi
if [ -n "$drift" ]; then
  echo "row baseline, counts: these differ in line count in this environment, which is a reading and not a refusal:$drift"
elif [ -n "$rowsbad" ]; then
  echo "row baseline, counts: no other difference; $matched compared fixture(s) match their recorded row and line counts"
else
  echo "row baseline, counts: all $matched compared fixture(s) match their recorded row and line counts"
fi

if [ -n "$stale" ] || [ -n "$missing" ] || [ -n "$rowsbad" ]; then exit 1; fi
exit 0
