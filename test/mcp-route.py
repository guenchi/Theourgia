"""Compare MCP over the live daemon with its actual local process fallback."""
from pathlib import Path
import json
import os
import signal
import subprocess
import sys
import tempfile
import time
root=Path(__file__).resolve().parents[2]
area=Path(tempfile.mkdtemp(prefix='mc-route-',dir='/private/tmp'));store=area/'s'
env=dict(os.environ,CHEZSCHEMELIBDIRS=str(root),CHEZSCHEMELIBEXTS='.ss::.no-obj:.sc::.no-obj',THEOURGIA_HOME=str(area/'home'),THEOURGIA_TRACE='1')
subprocess.run(['scheme','--script',str(root/'theourgia/cli.ss'),'init','--store',str(store)],env=env,capture_output=True,check=True)
messages=[{'jsonrpc':'2.0','id':1,'method':'initialize','params':{'protocolVersion':'2025-11-25','capabilities':{},'clientInfo':{'name':'fixture','version':'1'}}},{'jsonrpc':'2.0','method':'notifications/initialized'},{'jsonrpc':'2.0','id':2,'method':'tools/call','params':{'name':'theourgia_outline','arguments':{'argv':[]}}}]
packet=b''.join(json.dumps(m).encode()+b'\n' for m in messages)
def call():
 return subprocess.run([sys.executable,str(root/'theourgia/mcp/server.py'),'--store',str(store)],input=packet,env=env,capture_output=True,timeout=20)
a=call()
assert b'transport-local' in a.stderr and b'log-open' in a.stderr,a.stderr
server_log=area/'server.log'
server=subprocess.Popen([sys.executable,str(root/'theourgia/local.py'),'serve',str(store)],env=env,stderr=server_log.open('wb'))
try:
 deadline=time.monotonic()+10
 while not (store/'socket').exists() and server.poll() is None and time.monotonic()<deadline:time.sleep(.02)
 assert (store/'socket').exists(),server_log.read_text()
 b=call()
 assert b'transport-forward' in b.stderr and b'log-open' not in b.stderr,b.stderr
 assert b.stdout==a.stdout,(a.stdout,b.stdout)
 assert 'daemon-dispatch' in server_log.read_text()
 print('ok MC-06 missing daemon starts the real local core')
 print('ok MC-06 live daemon receives requests without a local log-open')
 print('ok MC-06 both routes return exactly the same MCP frames')
 print('mcp-route complete')
 (root/'implementation/evidence/mcp-route.json').write_text(json.dumps({'local_stdout':a.stdout.decode(),'socket_stdout':b.stdout.decode(),'local_stderr':a.stderr.decode(),'socket_stderr':b.stderr.decode(),'daemon_trace':server_log.read_text()},indent=2)+'\n')
finally:
 server.send_signal(signal.SIGTERM)
 try:server.wait(timeout=8)
 except subprocess.TimeoutExpired:server.kill();server.wait()
