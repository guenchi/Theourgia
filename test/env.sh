# THE LIBRARY PATH IS PINNABLE. The suite imports (igropyr ...), and that
# working tree is edited by another line of work -- a run that spans one
# of those edits is not a reading of anything. THEOURGIA_LIBDIR points at
# a directory holding igropyr/ and theourgia/ exported at known commits;
# without it, the working trees are used and the reading is only as
# stable as they are.
if [ -n "$THEOURGIA_LIBDIR" ]; then
  export CHEZSCHEMELIBDIRS="$THEOURGIA_LIBDIR"
else
  export CHEZSCHEMELIBDIRS=/Users/guenchi/Workshop
fi
export CHEZSCHEMELIBEXTS=".ss::.sls::.sc::.scm"
