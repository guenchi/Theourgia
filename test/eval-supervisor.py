"""Controlled clock/RSS twins exercise the real supervisor with finite workers."""
from pathlib import Path
import importlib.util
import os
import tempfile
import subprocess
import sys

import paths

core = paths.core()
area = paths.scratch('eval-supervisor-')
os.environ['THEOURGIA_HOME'] = str(area / 'home')
spec = importlib.util.spec_from_file_location('eval_product', core / 'local.py')
product = importlib.util.module_from_spec(spec)
spec.loader.exec_module(product)
subprocess.run(['scheme', '--script', str(core / 'cli.ss'), 'init', '--store', str(area/'store')], env=product.runtime_env(), capture_output=True, check=True,stdin=subprocess.DEVNULL)
bad = 0
def want(label, result, expected):
    global bad
    if result == expected: print('ok '+label)
    else:
        bad += 1
        print(f'FAIL {label}: {result!r} WANT {expected!r}')
args = ['--store', str(area/'store'), '--', '(+ 1 2)']
clock = product.time.monotonic
rss = product.rss_bytes
try:
    readings = iter([0])
    product.time.monotonic = lambda: next(readings, 10)
    product.rss_bytes = lambda pid: 0
    answer = product.evaluate(args)
    want('EV-02 controlled deadline transition terminates finite worker', b'(resource time)' in answer, True)
    product.time.monotonic = clock
    product.rss_bytes = lambda pid: 268435457
    answer = product.evaluate(args)
    want('EV-03 observed over-budget RSS terminates finite worker', b'(resource memory)' in answer, True)
    product.rss_bytes = lambda pid: 1
    answer = product.evaluate(args)
    want('EV-02 finite worker before deadline returns values', b'(values (3))' in answer, True)
finally:
    product.time.monotonic = clock
    product.rss_bytes = rss
print(f'{bad} failures\neval-supervisor complete')
sys.exit(bool(bad))
