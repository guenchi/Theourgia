"""Compare independently generated projections from two real core processes."""
from pathlib import Path
import os
import subprocess
import tempfile

import paths

core, lib = paths.core(), paths.libdir()
area = paths.scratch('datum-process-')
store, source = area / 'store', area / 'source'
source.mkdir()
(source / 'library.sc').write_text('(library (process-fixture) (export x) (import (rnrs))\n;; preserved docs\n(define x #\\space))\n')
env = dict(os.environ, CHEZSCHEMELIBDIRS=str(lib), CHEZSCHEMELIBEXTS='.ss::.no-obj:.sc::.no-obj', THEOURGIA_HOME=str(area / 'home'))
def run(*args):
    r = subprocess.run(['scheme', '--script', str(core / 'cli.ss'), *args, '--store', str(store)], env=env, capture_output=True, timeout=20,stdin=subprocess.DEVNULL)
    assert r.returncode == 0, (r.stdout, r.stderr)
    return r.stdout
run('init')
run('import-code', str(source), '--datum')
run('export-code', str(area / 'one'), '--datum')
run('export-code', str(area / 'two'), '--datum')
assert (area / 'one/library.sc').read_bytes() == (area / 'two/library.sc').read_bytes()
print('ok CD-06 two independent processes generate identical source bytes')
before = run('outline')
run('import-code', str(area / 'two'), '--datum')
assert run('outline') == before
print('ok CD-06 generated source restores structure without a previous output directory')
print('datum-processes complete')
