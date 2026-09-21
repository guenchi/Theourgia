#!/bin/sh
# Does a mutant change anything the fixture's rows can see?
#
# This is not a fixture -- it prints no sentinel and the suite's `*.sc *.py`
# loop never reaches it. It sits here beside `env.sh` because a tool kept in a
# scratchpad disappears with the session that wrote it, and the next person
# then cannot read the one thing that matters about it: its precondition.
#
# WHY IT EXISTS. A mutation that changes the file is not a mutation that runs.
# Two mutants in one pass moved the file's md5 -- `landed: True` -- and touched
# no path the rows exercise: one padded an `and` chain with `#t`, one edited a
# branch those rows never take. Both printed `reds: 0`, which is exactly what a
# SURVIVOR prints, and telling them apart took a person reading the source.
#
# WHAT IT ESTABLISHES, AND WHAT IT DOES NOT. Identical output means NO
# OBSERVABLE DIFFERENCE IN THIS FIXTURE -- weaker than "it did not run". An
# edit that ran and had its result discarded, and an edit visible only to some
# other fixture, read exactly the same way. It says which survivors to
# diagnose first; it never says a mutant is dead.
#
# ITS PRECONDITION, AND WHY THE SELF-TEST HAS TWO HALVES. The comparison needs
# two runs of the same tree to agree. They do not agree raw: this fixture mints
# a fresh writer id per store and stamps log entries with the clock. So the
# volatile tokens are masked -- and the mask is then tested, in BOTH
# directions:
#
#   a mask too NARROW  -> two unmutated runs still differ  -> everything reads
#                         as "observable", including inert mutants. Annoying,
#                         but it never lies that a mutant is inert.
#   a mask too BROAD   -> a real difference is masked away -> a live mutant is
#                         reported inert. Quiet, and it does lie.
#
# The first version of this tool compared byte for byte and always said
# "differs"; the second masked `[a-z0-9]{8}` anywhere, which shredded real
# content -- including a sha256 the fixture prints -- and moved the failure to
# the dangerous side. **A repaired instrument fails in a new direction; ask
# which one.** So `selftest` takes two unmutated runs AND one run known to
# differ, and refuses to be used unless it gets both answers right.
#
# An instrument that cannot show its own zero reading does not get to give
# readings -- and one that cannot show a non-zero reading is worse.
#
# WHO RUNS THIS, AND WHY THAT IS WRITTEN DOWN. Every mutation pass calls
# `selftest` before its first mutant and `compare` after each one. That
# dependency is named here because a tool nothing calls is the same as no tool
# -- and this tree has the precedent: ten python fixtures sat in this directory
# for months without ever running. This one does not need a cell to keep it
# honest; it needs a CALLER. A pass that does not call it gets no labels, and a
# pass that calls it on a tree whose output has grown a new volatile field gets
# UNAVAILABLE, loudly, at the start rather than a wrong answer at the end.
#
# THE MASK COMES FROM THE PRODUCER, NOT FROM SAMPLES. A block id is
# `<writer>.<sequence>` and the sequence is base 36, which is written where ids
# are minted. A version of this mask was written by looking at ids that
# happened to be on screen -- all of them numeric -- and matched `\.[0-9]+`,
# so every id past `.9` went unmasked. The self-test caught it. Read the format
# where it is defined; do not infer it from what you have seen.
#
# Usage:
#   mutation-observability.sh selftest <base-a> <base-b> <known-different>
#   mutation-observability.sh compare  <mutant-out> <base-out>

STATE="${MUTOBS_STATE:-/tmp/theourgia-mutobs-state}"

mask () {
  # Only patterns with a SHAPE, never a bare run of characters: a block id, a
  # quoted writer name, a writer/sequence pair, a millisecond stamp, a path.
  # A block id is <writer>.<sequence> and the SEQUENCE IS BASE 36 -- `.9` is
  # followed by `.a`. A first attempt matched `\.[0-9]+` and left every id with
  # a letter in its sequence varying, which the self-test caught.
  sed -E 's/[a-z0-9]{8}\.[a-z0-9]+/<id>/g;
          s/"[a-z0-9]{8}"/"<writer>"/g;
          s/\([a-z0-9]{8} \. [0-9]+\)/(<writer> . N)/g;
          s/[a-z0-9]{8}\.[a-z0-9]+/<id>/g;
          s/\(ts [0-9]+\)/(ts T)/g;
          s#/tmp/[^ )"]*#<path>#g;
          s#machine-home [^)]*#machine-home <path>#g' "$1"
}

case "$1" in
  selftest)
    [ -f "$2" ] && [ -f "$3" ] && [ -f "$4" ] || {
      echo "  observability check: UNAVAILABLE (selftest needs two base runs and one known-different run)"
      echo unavailable > "$STATE"; exit 0; }
    mask "$2" > "$STATE.a"; mask "$3" > "$STATE.b"; mask "$4" > "$STATE.d"
    zero=no; nonzero=no
    cmp -s "$STATE.a" "$STATE.b" && zero=yes
    cmp -s "$STATE.a" "$STATE.d" || nonzero=yes
    if [ "$zero" = yes ] && [ "$nonzero" = yes ]; then
      echo available > "$STATE"
      echo "  observability check: AVAILABLE (two unmutated runs agree; a known-different run does not)"
    else
      echo unavailable > "$STATE"
      echo "  observability check: UNAVAILABLE"
      [ "$zero" = no ]    && echo "     the mask is too NARROW: two unmutated runs still differ"
      [ "$nonzero" = no ] && echo "     the mask is too BROAD: a run known to differ was masked into agreement"
      echo "     no mutant will be labelled."
    fi ;;
  compare)
    [ "$(cat "$STATE" 2>/dev/null)" = available ] || exit 0
    mask "$2" > "$STATE.m"; mask "$3" > "$STATE.base"
    if cmp -s "$STATE.m" "$STATE.base"; then
      echo "     NO OBSERVABLE DIFFERENCE in this fixture's output -- diagnose"
      echo "     before calling it a survivor (ran-and-discarded reads the same)"
    else
      echo "     (the edit reaches something these rows see)"
    fi ;;
  *) echo "usage: $0 selftest <base-a> <base-b> <known-different> | compare <mutant> <base>"; exit 2 ;;
esac
