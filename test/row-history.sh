#!/bin/sh
# ONE COMMAND THAT COUNTS A ROW'S HISTORY (F67).
#
# Usage, from anywhere:
#
#     sh test/row-history.sh "<row label>" [<directory>]
#
# It reads every file named *.out under the directory -- by default the
# archive beside this repository's checkout, ../../archive from this file --
# and, for each file whose lines name the label, prints how many of those
# lines begin `ok` and how many begin `FAIL`, then the totals.
#
# WHAT IT COUNTS IS APPEARANCES, NOT RUNS, and it says so in its output. A
# run whose outputs were copied into two places is counted twice, a cells/
# reading of one fixture counts beside a whole suite, and a label that is a
# prefix of another ("RS-1", "RS-14") matches both. The file names are
# printed so a reader can tell which is which; the counts alone cannot.
#
# A FILE THAT CANNOT BE READ IS NAMED, NOT SKIPPED, and so is a step that
# failed: the listing (find), its ordering (sort) and the count of each
# file (awk) each have their exit status read, and any that is not zero is printed and makes the
# script exit 1. A count that silently left out what it could not read
# would read as a smaller history rather than an unknown one; and a
# "total" printed after a listing that failed is a success message the
# commands never gave.
#
# THE LABEL IS COUNTED AS WRITTEN. It reaches awk through the environment,
# not `-v`, which would interpret backslashes; and it is printed with
# printf '%s', not echo, which would too. So what the first line says was
# counted is what was counted.
label=$1
if [ -z "$label" ]; then
  echo "usage: sh test/row-history.sh \"<row label>\" [<directory>]"
  exit 2
fi
here=$(cd "$(dirname "$0")" && pwd)
dir=${2:-$here/../../archive}
if [ ! -d "$dir" ]; then
  printf 'row-history: not a directory: %s\n' "$dir"
  exit 2
fi
work=$(mktemp -d "${TMPDIR:-/tmp}/row-history.XXXXXX") || {
  echo "row-history: could not make a scratch directory"
  exit 2
}
trap 'rm -rf "$work"' EXIT
printf 'row history of "%s" in %s\n' "$label" "$dir"
echo "(appearances of lines naming the label in *.out files -- NOT a count of runs)"
failed=0
find "$dir" -type f -name '*.out' -print > "$work/found"
find_rc=$?
if [ "$find_rc" != 0 ]; then
  echo "FAILED: listing *.out under $dir (find exit $find_rc); the files below are the ones it did list"
  failed=1
fi
sort "$work/found" > "$work/sorted"
sort_rc=$?
if [ "$sort_rc" != 0 ]; then
  echo "FAILED: sorting the list of *.out files (sort exit $sort_rc); the counts below may not cover every file"
  failed=1
fi
: > "$work/counts"
while IFS= read -r f; do
  if [ ! -r "$f" ]; then
    printf 'UNREADABLE %s\n' "$f" >> "$work/counts"
    continue
  fi
  ROW_LABEL=$label awk -v F="$f" '
    BEGIN { L = ENVIRON["ROW_LABEL"] }
    index($0, L) > 0 && /^ok/ { o++ }
    index($0, L) > 0 && /^FAIL/ { x++ }
    END { if (o + x > 0) printf "COUNT %d %d %s\n", o, x, F }
  ' "$f" >> "$work/counts"
  awk_rc=$?
  if [ "$awk_rc" != 0 ]; then
    printf 'UNREADABLE %s (awk exit %s)\n' "$f" "$awk_rc" >> "$work/counts"
    failed=1
  fi
done < "$work/sorted"
awk '
  /^UNREADABLE / { u++; print "  " $0; next }
  /^COUNT / {
    ok += $2; fail += $3; n++
    path = $0; sub(/^COUNT [0-9]+ [0-9]+ /, "", path)
    printf "  ok %d  FAIL %d  %s\n", $2, $3, path
  }
  END {
    printf "total: ok %d  FAIL %d  in %d files\n", ok, fail, n
    if (u > 0) { printf "UNREADABLE: %d files could not be read; the totals leave them out\n", u; exit 1 }
    if (n == 0) { print "no file names the label" }
  }
' "$work/counts"
summary_rc=$?
if [ "$summary_rc" != 0 ] || [ "$failed" != 0 ]; then
  exit 1
fi
exit 0
