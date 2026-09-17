"""Exercise the local supervisor, actual worker and capability boundaries."""
from pathlib import Path
import os
import subprocess
import sys
import tempfile
import time

import paths

# `lib` is rebound below as a block id, so the library path is `libdir`.
core, libdir = paths.core(), paths.libdir()
area = paths.scratch('eval-fixture-')
env = dict(os.environ, CHEZSCHEMELIBDIRS=str(libdir), CHEZSCHEMELIBEXTS='.ss::.no-obj:.sc::.no-obj', THEOURGIA_HOME=str(area / 'home'))
store = area / 'store'
subprocess.run(['scheme', '--script', str(core / 'cli.ss'), 'init', '--store', str(store)], env=env, capture_output=True, check=True,stdin=subprocess.DEVNULL)
bad = 0
# The raw output of this run is kept beside the run's other scratch.
transcript_path = paths.evidence('eval-local.log')
class Transcript:
    def __init__(self, stream):
        self.stream = stream
        self.file = transcript_path.open('w')
    def write(self, text):
        self.stream.write(text)
        self.file.write(text)
        self.file.flush()
    def flush(self):
        self.stream.flush()
        self.file.flush()
sys.stdout = Transcript(sys.stdout)
def want(label, actual, expected):
    global bad
    if actual == expected:
        print('ok ' + label, flush=True)
    else:
        bad += 1
        print(f'FAIL {label}: {actual!r} WANT {expected!r}', flush=True)
def evaluate(source, *args):
    started = time.monotonic()
    r = subprocess.run([sys.executable, str(core / 'local.py'), 'eval', '--store', str(store), *args, '--', source], env=env, capture_output=True, timeout=15,stdin=subprocess.DEVNULL)
    want('EV-transport result is one complete line', len(r.stdout.splitlines()), 1)
    return r.stdout.decode(), time.monotonic() - started
for source, values in [('(+ 1 2)', '(3)'), ('(values)', '()'), ('(values 1 2)', '(1 2)')]:
    out, _ = evaluate(source)
    want('EV-06 complete values ' + source, out, f'(ok (values {values}) (stdout "") (stderr ""))\n')
out, elapsed = evaluate('(let loop () (loop))', '--timeout-ms', '700')
want('EV-02 infinite loop returns product time limit', '(resource time)' in out, True)
want('EV-02 worker lifecycle finishes within external watchdog', elapsed < 4, True)
for source, kind in [('(lambda () 1)', 'procedure'), ('(let ((p (cons 1 #f))) (set-cdr! p p) p)', 'cycle'), ('(open-string-input-port "x")', 'port')]:
    out, _ = evaluate(source)
    want('EV-05 classified value ' + kind, '(kind ' + kind + ')' in out, True)
for source in ['(raise 42)', '(error "user" "broken")']:
    out, _ = evaluate(source)
    want('EV-07 exception is a bounded answer', out.startswith('(error eval-exception '), True)
out, _ = evaluate('#; #e1e99999999 (+ 1 2)')
want('EV-04 source numeric gate precedes evaluation', 'unsafe-numeric-token' in out, True)
out, _ = evaluate('(begin (display "(ok forged)\\n") 7)')
want('EV-09 user stdout cannot forge protocol', out, '(ok (values (7)) (stdout "(ok forged)\\n") (stderr ""))\n')
out, _ = evaluate('(let loop () (display "0123456789") (loop))', '--output-bytes', '2048')
want('EV-09 output quota produces a complete limit answer', '(resource output)' in out, True)
out, _ = evaluate('(begin (display "user-error" (current-error-port)) 8)')
want('EV-09 user stderr is isolated from core startup diagnostics', out, '(ok (values (8)) (stdout "") (stderr "user-error"))\n')
out, _ = evaluate('(begin (display "memory-loop-entered") (flush-output-port) (let loop ((xs (quote ()))) (loop (cons (make-bytevector 1048576 1) xs))))', '--memory-bytes', '268435456', '--timeout-ms', '8000')
want('EV-03 actual growing worker RSS triggers memory limit', '(resource memory)' in out, True)
want('EV-03 allocation body actually begins before memory refusal', 'memory-loop-entered' in out, True)
before = {str(p.relative_to(store)): p.read_bytes() for p in store.rglob('*') if p.is_file()}
for source in ['(open-file-output-port "escape")', '(system "true")', '(eval 1 (environment (quote (chezscheme))))', '(import (chezscheme))']:
    out, _ = evaluate(source)
    want('EV-08 unavailable capabilities cannot run', out.startswith('(error '), True)
after = {str(p.relative_to(store)): p.read_bytes() for p in store.rglob('*') if p.is_file()}
want('EV-08 evaluation cannot write committed store', after, before)
out, _ = evaluate('(store-cut)')
want('EV-10 committed snapshot is readable', out.startswith('(ok (values ('), True)
for verb in ['eval', 'not-a-verb']:
    r = subprocess.run(['scheme', '--script', str(core / 'rpc-worker.ss'), str(store), 'test', '--once'], input=('('+verb+' \"x\")\n').encode(), env=env, capture_output=True, timeout=10)
    want('EV-01 RPC unknown path ' + verb, b'(error unknown-verb ' in r.stdout, True)
source_dir = area / 'source'
source_dir.mkdir()
(source_dir / 'context.sc').write_text('(library (eval-test) (export x) (import (rnrs)) (define x 40))')
def cli(*args):
    r = subprocess.run(['scheme', '--script', str(core / 'cli.ss'), *args, '--store', str(store)], env=env, capture_output=True, timeout=15,stdin=subprocess.DEVNULL)
    assert r.returncode == 0, r.stdout + r.stderr
    return r.stdout.decode()
cli('import-code', str(source_dir), '--datum')
outline = cli('outline').splitlines()
lib, child = outline[0].split()[1], outline[1].split()[1]
old_cut, _ = evaluate('(store-cut)')
# The expression's single returned datum is the committed cut printed by Chez.
cut = old_cut.split('(values (', 1)[1].split(')) (stdout', 1)[0]
cli('set', child, 'body', '(define x 90)')
out, _ = evaluate('(+ x 2)', '--under', lib)
want('EV-10 selected library definitions are visible', '(values (92))' in out, True)
out, _ = evaluate('(+ x 2)', '--under', lib, '--cut', cut)
want('EV-10 explicit earlier cut retains old definitions', '(values (42))' in out, True)
out, _ = evaluate('(+ 2 3)', '--memory-bytes', '268435456')
want('EV-03 small allocation twin fits the same memory budget', '(values (5))' in out, True)
print(f'transcript {transcript_path}')
print(f'{bad} failures\neval-local complete')
sys.exit(bool(bad))
