# THE LIBRARY PATH IS PINNABLE, AND IT HAS TO HOLD BOTH TREES.
#
# The core imports (igropyr crypto), (igropyr sexpr) and (igropyr
# platform) -- through three facades of its own, never directly -- so a
# library directory with only theourgia/ in it resolves nothing past the
# first import. THEOURGIA_LIBDIR points at a directory holding BOTH
# igropyr/ and theourgia/, exported at known commits.
#
# WITHOUT IT the working trees are used, and the reading is only as
# stable as they are: another line of work edits igropyr, and a run that
# spans one of those edits is not a reading of anything.
#
# `run-fixtures.sh` checks the directory before it starts, because the
# failure mode otherwise is a hundred fixtures at rc=255 all saying
# `library (theourgia rpc) not found`, which reads as a broken tree
# rather than as an unset variable.
if [ -n "$THEOURGIA_LIBDIR" ]; then
  export CHEZSCHEMELIBDIRS="$THEOURGIA_LIBDIR"
else
  export CHEZSCHEMELIBDIRS=/Users/guenchi/Workshop
fi
export CHEZSCHEMELIBEXTS=".sc::.sls::.scm"
