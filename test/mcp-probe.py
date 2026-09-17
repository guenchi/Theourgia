"""Add a compiled core verb and discover it through the unmodified MCP shell."""
from pathlib import Path
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile

root=Path(__file__).resolve().parents[2]
area=Path(tempfile.mkdtemp(prefix='mcp-probe-',dir=root/'.build'))
shutil.copytree(root/'theourgia',area/'theourgia',ignore=shutil.ignore_patterns('*.so','*.wpo','__pycache__'))
(area/'igropyr').symlink_to(root/'igropyr',target_is_directory=True)
edit=subprocess.run(['scheme','--script',str(root/'implementation/mutate-form.ss'),str(area/'theourgia/rpc.ss'),'verbs','(verb-table)',"(cons (cons (quote catalog-probe) (lambda (store actor args req options) (quote (ok catalog-probe)))) (verb-table))"],capture_output=True,text=True)
assert edit.returncode==0,edit.stdout+edit.stderr
store=area/'store'
env=dict(os.environ,CHEZSCHEMELIBDIRS=str(area),CHEZSCHEMELIBEXTS='.ss::.no-obj:.sc::.no-obj',THEOURGIA_HOME=str(area/'home'))
subprocess.run(['scheme','--script',str(area/'theourgia/cli.ss'),'init','--store',str(store)],env=env,capture_output=True,check=True)
requests=[{'jsonrpc':'2.0','id':1,'method':'initialize','params':{'protocolVersion':'2025-11-25','capabilities':{},'clientInfo':{'name':'fixture','version':'1'}}},
          {'jsonrpc':'2.0','method':'notifications/initialized'}, {'jsonrpc':'2.0','id':2,'method':'tools/list'},
          {'jsonrpc':'2.0','id':3,'method':'tools/call','params':{'name':'theourgia_catalog-probe','arguments':{'argv':[]}}}]
result=subprocess.run([sys.executable,str(area/'theourgia/mcp/server.py'),'--store',str(store)],input=''.join(json.dumps(r)+'\n' for r in requests),env=env,capture_output=True,text=True,timeout=30)
rows=[json.loads(line) for line in result.stdout.splitlines()]
assert result.returncode==0 and len(rows)==3,result.stdout+result.stderr
bad=0
def want(label,actual,expected):
 global bad
 if actual==expected:print('ok '+label)
 else:
  bad+=1
  print(f'FAIL {label}: {actual!r} WANT {expected!r}')
want('MC-01 newly compiled core verb appears without shell changes',
 'theourgia_catalog-probe' in [t['name'] for t in rows[1].get('result',{}).get('tools',[])],True)
want('MC-01 newly compiled core verb is callable through the shell',
 rows[2].get('result',{}).get('content',[{}])[0].get('text'),'(ok catalog-probe)\n')
print(f'{bad} failures\nmcp-probe complete')
(root/'implementation/evidence/mcp-probe.json').write_text(json.dumps({'source_sha256':hashlib.sha256((area/'theourgia/rpc.ss').read_bytes()).hexdigest(),'stdout':result.stdout,'stderr':result.stderr,'exit_code':result.returncode},indent=2)+'\n')

sys.exit(bool(bad))
