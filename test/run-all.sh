#!/bin/sh
# Run the argument-free suites from source. Requires the igropyr checkout
# to be a sibling directory of this repository: (igropyr X) resolves to
# <libdir>/igropyr/X.sc and (theourgia X) to <libdir>/theourgia/X.sc, so
# the common parent directory is the single libdir that answers for both.
# No .so is loaded: CHEZSCHEMELIBEXTS below carries no object extension.
set -u
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
export CHEZSCHEMELIBDIRS="$root"
export CHEZSCHEMELIBEXTS=.chezscheme.sls::.no-obj:.ss::.no-obj:.sls::.no-obj:.scm::.no-obj:.sch::.no-obj:.sc::.no-obj
status=0
for f in smoke-crc32 smoke-wire smoke-wire-fuzz smoke-wire-trace smoke-ffi \
         regression verify-l4 verify-guard actor-check dep-check; do
  printf '== %s\n' "$f"
  if ! scheme --script "$here/$f.sc"; then
    printf '!! %s failed\n' "$f"
    status=1
  fi
done
printf '== not run here: fault-file.sc fault-pipe.sc shared-lock.sc (driven with arguments by the fault matrix and the lock test)\n'
exit $status
