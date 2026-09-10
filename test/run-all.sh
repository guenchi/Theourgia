#!/bin/sh
# Copyright 2026 guenchi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# Run the argument-free suites from source. Requires the igropyr checkout
# to be a sibling directory of this repository: (igropyr X) resolves to
# <libdir>/igropyr/X.sc and (theourgia X) to <libdir>/theourgia/X.ss, so
# the common parent directory is the single libdir that answers for both.
# No .so is loaded: CHEZSCHEMELIBEXTS below carries no object extension.
# Every suite must end by printing '<name> complete'; a run without that
# line is reported as not finished even when the exit status is zero.
# The sentinel says the suite finished, not that it passed: any FAIL row
# or a non-zero failure/mismatch count is red as well.
set -u
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
export CHEZSCHEMELIBDIRS="$root"
export CHEZSCHEMELIBEXTS=.chezscheme.sls::.no-obj:.ss::.no-obj:.sls::.no-obj:.scm::.no-obj:.sch::.no-obj:.sc::.no-obj
status=0
# The log fixtures inject faults through the expand-time gate, so the
# gate is opened for the whole run; nothing fires unless a fixture also
# names a fault.
export THEOURGIA_INJECT=on
for f in smoke-crc32 smoke-wire smoke-wire-fuzz smoke-wire-trace smoke-ffi \
         regression verify-l4 verify-guard actor-check dep-check \
         log1 log2 log3 log5 log6 log7 log8 log9 log10 log11 log12 log13 log14 \
         crash log15 log16 log17; do
  printf '== %s\n' "$f"
  out=$(scheme --script "$here/$f.ss" 2>&1); rc=$?
  printf '%s\n' "$out"
  if [ $rc -ne 0 ]; then
    printf '!! %s failed (exit %s)\n' "$f" "$rc"
    status=1
  elif ! printf '%s\n' "$out" | grep -q "^$f complete\$"; then
    printf '!! %s DID NOT FINISH: no completion sentinel (a crash is a detection, not a pass)\n' "$f"
    status=1
  elif printf '%s\n' "$out" | grep -Eq '^FAIL|[1-9][0-9]* (failures|mismatches)'; then
    printf '!! %s has red rows (the sentinel says it finished, not that it passed)\n' "$f"
    status=1
  fi
done
printf '== not run here: fault-file.ss fault-pipe.ss shared-lock.ss (driven with arguments by the fault matrix and the lock test)\n'
exit $status
