"""Drive real Unix sockets and compare them to the same core renderer."""
from pathlib import Path
import concurrent.futures
import json
import os
import signal
import socket
import subprocess
import sys
import tempfile
import time

root = Path(__file__).resolve().parents[2]
area = Path(tempfile.mkdtemp(prefix='ed-', dir='/private/tmp'))
store = area / 's'
env = dict(os.environ, CHEZSCHEMELIBDIRS=str(root), CHEZSCHEMELIBEXTS='.ss::.no-obj:.sc::.no-obj', THEOURGIA_HOME=str(area/'home'))
script = root / 'theourgia/cli.ss'
local = root / 'theourgia/local.py'
bad = 0
log = []
def want(label, actual, expected):
    global bad
    if actual == expected:
        line = 'ok ' + label
    else:
        bad += 1
        line = f'FAIL {label}: {actual!r} WANT {expected!r}'
    log.append(line)
    print(line, flush=True)
def cli(*args, wire=True, force=False, trace=False):
    r = subprocess.run(['scheme', '--script', str(script), *args, '--store', str(store), *(['--wire'] if wire else [])], env=dict(env, THEOURGIA_LOCAL='1' if force else '', THEOURGIA_TRACE='1' if trace else ''), capture_output=True, timeout=20)
    return r
cli('init', wire=False)
server = None
try:
    server = subprocess.Popen([sys.executable, str(local), 'serve', str(store)], env=env, stdout=subprocess.PIPE, stderr=(area/'server.log').open('wb'))
    deadline = time.monotonic() + 8
    while not (store/'socket').exists() and server.poll() is None and time.monotonic() < deadline:
        time.sleep(.02)
    want('ED-start server enters listening state', (store/'socket').exists(), True)
    if (store/'socket').exists():
        def ask(frame, split=False):
            with socket.socket(socket.AF_UNIX) as peer:
                peer.settimeout(8)
                peer.connect(str(store/'socket'))
                if split:
                    for byte in frame:
                        peer.sendall(bytes([byte]))
                else:
                    peer.sendall(frame)
                answer = b''
                while not answer.endswith(b'\n'):
                    answer += peer.recv(65536)
                return answer
        # Successful and invalid argv travel through exactly the same dispatcher.
        for verb, args in [('outline', []), ('search', ['needle']), ('conflicts', []), ('check', []), ('refs', ['missing.1']), ('read', ['missing.1']), ('log', ['--count','0'])]:
            frame = ('(' + verb + ''.join(' '+json.dumps(a) for a in args) + ')\n').encode()
            want('ED-01 bytes '+verb, ask(frame), cli(verb,*args,force=True).stdout)
        want('ED-03 valid false datum reaches core shape validation', ask(b'#f\n'), b'(error bad-request not-a-list)\n')
        a=ask(b'(eval "(+ 1 2)")\n')
        b=ask(b'(bogus "(+ 1 2)")\n')
        want('ED-07 eval shares the generic unknown verb response', a.replace(b'"eval"',b'"bogus"'), b)
        alive=cli('outline',trace=True)
        want('ED-06 live socket client has no local log-open', b'(trace log-open ' in alive.stderr, False)
        want('ED-06 live socket client has forwarding witness', b'transport-forward' in alive.stderr, True)
        frame=b'(insert "--under" "root" "--title" "'+ '汉字😀'.encode()+b'" "--text" "line\\nsecond")\n'
        want('ED-02 fragmented UTF8 and escaped newline reach dispatcher', ask(frame,True).startswith(b'(ok '), True)
        for frame, reason in [(b'(outline) (insert)\n',b'framing'),(b'(outline "\xff")\n',b'invalid-utf8')]:
            want('ED-03 malformed frame '+reason.decode(),reason in ask(frame),True)
        before=cli('outline',force=True).stdout
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
            results=list(pool.map(ask,[b'(insert "--under" "root" "--title" "A")\n',b'(insert "--under" "root" "--title" "B")\n']))
        want('ED-04 concurrent writes both complete',all(a.startswith(b'(ok ') for a in results),True)
        want('ED-04 concurrent writes produce distinct answers',results[0]!=results[1],True)
        current=cli('outline',force=True).stdout
        want('ED-12 daemon sees independently loaded authoritative state',ask(b'(outline)\n'),current)
        server.send_signal(signal.SIGTERM)
        server.wait(timeout=8)
        want('ED-08 graceful drain exits successfully',server.returncode,0)
        want('ED-08 graceful drain removes its socket',(store/'socket').exists(),False)
        absent=cli('outline',trace=True)
        want('ED-06 absent socket falls back to local log-open',b'(trace log-open ' in absent.stderr,True)
        server=subprocess.Popen([sys.executable,str(local),'serve',str(store)],env=env,stderr=(area/'restart.log').open('wb'))
        deadline=time.monotonic()+8
        while not (store/'socket').exists() and server.poll() is None and time.monotonic()<deadline:time.sleep(.02)
        server.kill();server.wait()
        stale=cli('outline',trace=True)
        want('ED-10 dead socket permits pre-send local fallback',stale.stdout,current)
        server=subprocess.Popen([sys.executable,str(local),'serve',str(store)],env=env,stderr=(area/'takeover.log').open('wb'))
        deadline=time.monotonic()+8
        while time.monotonic()<deadline:
            try:
                a=ask(b'(outline)\n');break
            except (ConnectionRefusedError,FileNotFoundError):time.sleep(.02)
        want('ED-10 restart safely takes over dead socket',a,current)
        occupied=area/'occupied';occupied.write_bytes(b'keep')
        r=subprocess.run([sys.executable,str(local),'serve',str(store),'--socket',str(occupied)],env=env,capture_output=True,timeout=8)
        want('ED-14 ordinary file is never replaced',occupied.read_bytes(),b'keep')
        want('ED-14 ordinary path refusal names occupied reason',b'occupied' in r.stdout,True)
except (OSError,subprocess.TimeoutExpired) as exc:
    want('ED-device completed real socket schedules',str(exc),'completed')
finally:
    if server and server.poll() is None:
        server.send_signal(signal.SIGTERM)
        try:server.wait(timeout=8)
        except subprocess.TimeoutExpired:server.kill();server.wait()
log.append(f'{bad} failures\ndaemon complete')
print(log[-1])
(root/'implementation/evidence/daemon-latest.log').write_text('\n'.join(log)+'\n')
sys.exit(bool(bad))
