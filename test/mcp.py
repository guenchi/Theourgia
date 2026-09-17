"""MCP stdio envelopes are checked separately from unmodified core text."""
from pathlib import Path
import json
import os
import subprocess
import sys
import tempfile

import paths

# `core` is a local name further down, so the source directory is `src`.
src,lib=paths.core(),paths.libdir()
area=paths.scratch('mcp-')
store=area/'store'
env=dict(os.environ,CHEZSCHEMELIBDIRS=str(lib),CHEZSCHEMELIBEXTS='.ss::.no-obj:.sc::.no-obj',THEOURGIA_HOME=str(area/'home'))
subprocess.run(['scheme','--script',str(src/'cli.ss'),'init','--store',str(store)],env=env,capture_output=True,check=True,stdin=subprocess.DEVNULL)
bad=0;log=[]
def want(label,a,b):
 global bad
 line='ok '+label if a==b else f'FAIL {label}: {a!r} WANT {b!r}'
 if a!=b:bad+=1
 log.append(line);print(line,flush=True)
def run(messages,child_env=env):
 packet=b''.join(json.dumps(m,ensure_ascii=False).encode()+b'\n' for m in messages)
 child=subprocess.run([sys.executable,str(src/'mcp/server.py'),'--store',str(store)],input=packet,env=child_env,capture_output=True,timeout=90)
 try:return [json.loads(line) for line in child.stdout.splitlines()]
 except (UnicodeError,json.JSONDecodeError):return []
init=[{'jsonrpc':'2.0','id':1,'method':'initialize','params':{'protocolVersion':'2025-11-25','capabilities':{},'clientInfo':{'name':'fixture','version':'1'}}},{'jsonrpc':'2.0','method':'notifications/initialized'}]
responses=run(init+[{'jsonrpc':'2.0','id':2,'method':'tools/list'}])
want('MC-10 initialization and list return exactly two frames',len(responses),2)
if len(responses)==2 and 'result' in responses[1]:
 tools=responses[1]['result']['tools']
 names=[t['name'] for t in tools]
 want('MC-01 catalog is nonempty',bool(names),True)
 want('MC-05 eval is absent from tools', 'theourgia_eval' in names,False)
 want('MC-10 initialized response pins protocol version',responses[0]['result']['protocolVersion'],'2025-11-25')
 bad_argv=['--req','probe-request','--cursor','invalid']
 calls=[{'jsonrpc':'2.0','id':i+10,'method':'tools/call','params':{'name':name,'arguments':{'argv':bad_argv}}} for i,name in enumerate(names)]
 results=run(init+calls)[1:]
 want('MC-02 every exported verb receives a response',len(results),len(names))
 # EVERY CHILD HERE IS GIVEN AN EMPTY STDIN, and this is where that was
 # measured. `batch` reads its intents from standard input, so the
 # comparison call below inherited whatever stdin the fixture was started
 # with: from a terminal or a detached session it never reaches end of
 # file and the child waits forever. It surfaced as
 # `subprocess.TimeoutExpired` after 27 passing rows, with no sentinel
 # and no FAIL -- which reads as flakiness rather than as a hang. Run
 # with stdin at /dev/null the same command answers
 # `(error bad-request malformed-cursor)` at once.
 for name,result in zip(names,results):
  verb=name.removeprefix('theourgia_')
  core=subprocess.run(['scheme','--script',str(src/'cli.ss'),verb,*bad_argv,'--store',str(store),'--wire'],env=env,capture_output=True,timeout=15,stdin=subprocess.DEVNULL)
  returned=result.get('result',{})
  text=returned.get('content',[{}])[0].get('text','')
  want('MC-02 unmodified core bytes '+verb,text.encode(),core.stdout)
  want('MC-03 core refusal remains a successful text result '+verb,returned.get('isError'),False)
 reqs=[{'jsonrpc':'2.0','id':40,'method':'tools/call','params':{'name':'theourgia_eval','arguments':{'argv':[]}}},
       {'jsonrpc':'2.0','id':41,'method':'tools/call','params':{'name':'theourgia_unknown','arguments':{'argv':[]}}},
       {'jsonrpc':'2.0','id':42,'method':'tools/call','params':{'name':'theourgia_outline','arguments':{'argv':[4]}}},
       {'jsonrpc':'2.0','id':43,'method':'tools/call','params':{'name':'theourgia_outline','arguments':{'argv':[]}}}]
 got=run(init+reqs)[1:]
 if len(got)==4:
  want('MC-05 eval and another unknown tool share one error',got[0].get('error'),got[1].get('error'))
  want('MC-04 nonstring argv is a protocol envelope error',got[2].get('error',{}).get('code'),-32602)
  core=subprocess.run(['scheme','--script',str(src/'cli.ss'),'outline','--store',str(store),'--wire'],env=env,capture_output=True,timeout=15,stdin=subprocess.DEVNULL)
  want('MC-02 successful outline keeps final newline',got[3]['result']['content'][0]['text'].encode(),core.stdout)
 else:want('MC-device all control responses complete',len(got),4)
 broken=run(init+[{'jsonrpc':'2.0','id':2,'method':'tools/list'}],dict(env,THEOURGIA_SCHEME=str(area/'missing-scheme')))
 want('MC-03 missing core is a shell error channel',broken[-1].get('error',{}).get('code') if broken else None,-32603)
log.append(f'{bad} failures\nmcp complete');print(log[-1])
transcript=paths.evidence('mcp-latest.log')
transcript.write_text('\n'.join(log)+'\n')
print(f'transcript {transcript}')
sys.exit(bool(bad))
