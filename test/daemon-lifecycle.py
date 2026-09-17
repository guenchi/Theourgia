"""Hold the actual store lock while testing drain and transport uncertainty."""
from pathlib import Path
import fcntl
import contextlib
import io
import importlib.util
import os
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time

import paths

# NO LIBRARY PIN HERE: this fixture takes its environment from the
# product's own runtime_env(), which puts the core's parent first on
# the library path. Binding paths.libdir() would name a pin the run
# does not honour.
core=paths.core()
sys.path.insert(0,str(core))
from local import runtime_env
from transport import exchange,encode_request
area=paths.scratch('ed-life-')
store=area/'s'
env=dict(runtime_env(),THEOURGIA_HOME=str(area/'home'),THEOURGIA_TRACE='1')
subprocess.run(['scheme','--script',str(core/'cli.ss'),'init','--store',str(store)],env=env,capture_output=True,check=True,stdin=subprocess.DEVNULL)
bad=0;logs=[]
def want(label,a,b):
 global bad
 line='ok '+label if a==b else f'FAIL {label}: {a!r} WANT {b!r}'
 if a!=b:bad+=1
 logs.append(line);print(line,flush=True)
def await_until(predicate):
 deadline=time.monotonic()+8
 while time.monotonic()<deadline:
  if predicate():return
  time.sleep(.01)
 raise TimeoutError('NO-READING: IPC boundary not reached')
def start(name):
 log=area/(name+'.log')
 process=subprocess.Popen([sys.executable,str(core/'local.py'),'serve',str(store)],env=env,stdout=subprocess.PIPE,stderr=log.open('wb'),stdin=subprocess.DEVNULL)
 await_until(lambda:(store/'socket').exists() or process.poll() is not None)
 assert process.poll() is None,log.read_text()
 return process,log
for force in (False,True):
 process,log=start('force' if force else 'drain')
 lock=open(store/'lock','r+b');fcntl.flock(lock,fcntl.LOCK_EX)
 peer=socket.socket(socket.AF_UNIX);peer.settimeout(8);peer.connect(str(store/'socket'))
 try:
  peer.sendall(b'(insert "--under" "root" "--title" "drained")\n')
  await_until(lambda:'daemon-dispatch' in log.read_text())
  process.send_signal(signal.SIGTERM)
  # Listener closure is observed independently of store progress.
  def refused():
   with socket.socket(socket.AF_UNIX) as probe:
    try:probe.connect(str(store/'socket'));return False
    except (ConnectionRefusedError,FileNotFoundError):return True
  await_until(refused)
  want('ED-08 new connections stop while committed request is draining',process.poll(),None)
  if force:
   process.send_signal(signal.SIGTERM)
   process.wait(timeout=4)
   want('ED-09 second signal bounds a blocked drain',process.returncode,75)
  else:
   fcntl.flock(lock,fcntl.LOCK_UN)
   data=b''
   while not data.endswith(b'\n'):
    part=peer.recv(65536)
    if not part:break
    data+=part
   want('ED-08 accepted request answers after lock release',data.startswith(b'(ok '),True)
   process.wait(timeout=4)
   want('ED-08 completed in-flight request permits exit zero',process.returncode,0)
 finally:
  lock.close();peer.close()
  if process.poll() is None:process.kill();process.wait()
# A real peer consumes the request and loses its answer. The local core remains
# available, so falling back would be observable as a new authoritative block.
path=area/'lost'
listener=socket.socket(socket.AF_UNIX);listener.bind(str(path));listener.listen(1)
seen=[]
def lose():
 peer,_=listener.accept()
 with peer:
  seen.append(peer.recv(65536))
 listener.close()
thread=threading.Thread(target=lose);thread.start()
before={str(p.relative_to(store)):p.read_bytes() for p in store.rglob('*') if p.is_file()}
trace=io.StringIO()
os.environ['THEOURGIA_TRACE']='1'
with contextlib.redirect_stderr(trace):
 code,answer=exchange(str(store),'test','wire',encode_request('insert',['--under','root','--title','must-not-redeliver']),str(path))
thread.join(timeout=4)
after={str(p.relative_to(store)):p.read_bytes() for p in store.rglob('*') if p.is_file()}
want('ED-11 lost-answer peer actually receives request bytes',bool(seen and seen[0]),True)
want('ED-11 connected lost answer reports uncertainty',b'transport-unknown' in answer,True)
want('ED-11 connected lost answer never retries through local core',after,before)
want('ED-11 no local dispatch after a connected lost answer','transport-local' in trace.getvalue(),False)
logs.append(f'{bad} failures\ndaemon-lifecycle complete');print(logs[-1])
transcript=paths.evidence('daemon-lifecycle.log')
transcript.write_text('\n'.join(logs)+'\n')
print(f'transcript {transcript}')
sys.exit(bool(bad))
