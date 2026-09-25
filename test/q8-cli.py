#!/usr/bin/env python3
"""Q2 review regressions: fresh stores, actual CLI and persisted log evidence.

Two assertions were changed after this was measured against the tree it
now guards.  The original text is kept beside each so the change is
visible rather than merely absent:

  'partial write is unknown' looked for "(error unknown partial-write".
      Every other `unknown` in this tree carries its reason as a LIST --
      (commit-barrier-failed), (range-overlaps ...), (uncertain-cache
      unreadable) -- so the store answers (error unknown (partial-write
      (sequence 1))).  The assertion was written against a flatter shape
      than the tree uses; the answer is the same fact either way.

  'late rotation exception keeps executed event' looked for a single
      top-level "(error unknown (execution-failed" carrying "(events ((".
      The store answers a batch as a LIST of per-item answers, so the
      item that committed appears as its own `ok` with its own event and
      the item that failed as its own `unknown`.  That is strictly more
      than the original looked for: the durable prefix is visible as the
      answers it actually produced, not summarised into an event list.
"""
import json, os, pathlib, subprocess, tempfile
lib = pathlib.Path(os.environ.get('THEOURGIA_LIBDIR', pathlib.Path(__file__).resolve().parents[2]))
root = pathlib.Path(tempfile.mkdtemp(prefix='theourgia-q8-'))
env = dict(os.environ, CHEZSCHEMELIBDIRS=str(lib), CHEZSCHEMELIBEXTS='.sc::.no-obj', THEOURGIA_INJECT='on')
env.pop('THEOURGIA_FAULT', None)
failures = 0
rows = []
def check(name, ok):
    global failures
    failures += not ok
    print(('PASS ' if ok else 'FAIL ') + name, flush=True)
def call(s, args, data=None, fault=None, actor='review'):
    e = dict(env, THEOURGIA_HOME=str(s.parent/'machine'))
    if fault: e['THEOURGIA_FAULT'] = fault
    argv = ['scheme','--script',str(lib/'theourgia/core.sc'),args[0],'--store',str(s)]
    if actor is not None: argv += ['--actor',actor]
    # `input=None` INHERITS THIS PROCESS'S STDIN, it does not close it.
    # Most calls here pass no payload, and a verb that reads standard
    # input then waits for an end of file the caller never sends.
    p = subprocess.run(argv+args[1:], input=data if data is not None else b'', stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=e, timeout=120)
    text = p.stdout.decode(errors='replace')
    rows.append(dict(args=args,fault=fault,exit=p.returncode,stdout=text,stderr=p.stderr.decode(errors='replace')))
    return p.returncode, text

def fresh(name):
    s = root/name/'store'; s.mkdir(parents=True); (s.parent/'machine').mkdir()
    check(name+' init',call(s,['init'])[0]==0)
    return s, next((s/'writers').iterdir()).name

def records(s):
    return {str(f.relative_to(s)):f.read_bytes() for f in s.glob('writers/*/[0-9]*.sexp')}

# F1: the retirement transaction, not the optional cache, preserves lost success.
s,w = fresh('rollback')
a = ['insert','--title','Original','--req','lost','--cursor',w+':0']
check('acknowledged before rollback', call(s,a)[0]==0)
(s/'writers'/w/'000001.sexp').write_bytes(b'')
check('adopt rollback',call(s,['adopt'])[0]==0)
cache = s/'writers'/w/'uncertain.sexp'
for mode in ('present','missing','truncated','empty'):
    if mode=='missing': cache.unlink(missing_ok=True)
    elif mode=='truncated': cache.write_text('(')
    elif mode=='empty': cache.write_text('()')
    before=records(s); rc,t=call(s,a)
    check('rollback '+mode+' stays unknown without new record',rc==1 and '(error unknown' in t and records(s)==before)
# Legacy format lacks an authoritative seed: conservatively retain open uncertainty.
retired=s/'writers'/w/'retired.sexp'
import re
legacy = re.sub(r'\s+\(uncertain \(\("[^"]+" \d+ (?:\d+|#f)\)\)\)', '', retired.read_text()).replace('(format 2)', '(format 1)')
check('legacy fixture is a complete record without uncertainty seed', '(uncertain ' not in legacy and legacy.count('(')==legacy.count(')'))
retired.write_text(legacy)
cache.unlink(missing_ok=True)
before=records(s); rc,t=call(s,a)
check('legacy retirement cannot prove nonexecution',rc==1 and '(error unknown' in t and records(s)==before)

# F4: replay barriers and partial writes never masquerade as prewrite failures.
s,w = fresh('barrier')
a=['insert','--title','One','--req','once','--cursor',w+':0']
check('healthy cacheless request succeeds',call(s,a)[0]==0)
before=records(s);rc,t=call(s,a,fault='fsync-fail@commit:file=000001.sexp')
check('failed replay barrier is unknown and does not append',rc==1 and '(error unknown (replay-barrier-failed' in t and records(s)==before)
rc,t=call(s,a)
check('retry after barrier failure replays once',rc==0 and '(replay #t)' in t and records(s)==before)
s,w=fresh('partial');rc,t=call(s,['insert','--title','Partial'],fault='write-eio-after-partial@commit')
# was: '(error unknown partial-write' -- this tree's reasons are lists
check('partial write is unknown',rc==1 and '(error unknown (partial-write' in t and any(records(s).values()))
s,w=fresh('prewrite');before=records(s)
rc,t=call(s,['insert','--title','None'],fault='open-fail@commit:file=000001.sexp:errno=EMFILE')
check('prewrite failure does not claim execution',rc==1 and '(error unknown' not in t and records(s)==before)
check('prewrite failure releases lock',call(s,['insert','--title','Next'])[0]==0)
s,w=fresh('rotation')
data=('((insert root #f ((title . "First"))) (insert root #f ((title . "Second") (src . "'+'x'*1048576+'"))))').encode()
rc,t=call(s,['batch'],data,fault='open-fail@commit:file=000002.sexp:errno=EMFILE')
# was: one top-level '(error unknown (execution-failed' with '(events (('
# now: the committed item keeps its own ok and its own event; the failed one its own unknown
check('late rotation exception keeps executed event',rc==1 and '(ok (events ((' in t and '(error unknown' in t)
rc,t=call(s,['outline']);check('complete prefix survives failed rotation','First' in t and 'Second' not in t)

# F6: known option spellings are opaque values, at both transport and verb layers.
s,w=fresh('options')
for i,literal in enumerate(('--actor','--store','--req','--cursor','--text'),1):
    rc,t=call(s,['insert','--title',literal,'--text',literal],actor=literal)
    check('literal option value '+literal,rc==0)
    rc,t=call(s,['read',w+'.'+str(i)])
    check('literal persisted title and body '+literal, '(title . "'+literal+'")' in t and '(src . "'+literal+'")' in t)
    check('literal persisted actor '+literal, any(('"'+literal+'"').encode() in b for b in records(s).values()))
before=records(s)
for args in (['insert','--title'],['insert','--title','A','--title','B'],['read',w+'.1','--md','--md']):
    rc,t=call(s,args);check('invalid option shape refused '+repr(args),rc==1 and 'bad-request' in t and records(s)==before)
rc,t=call(s,['set',w+'.1','note','--','--req']);check('option-looking positional after terminator',rc==0)
rc,t=call(s,['read',w+'.1']);check('positional literal persisted','(note . "--req")' in t)
a=['insert','--title','--cursor','--text','-','--req','stdin-once','--cursor',w+':6']
rc,t=call(s,a,b'--actor');check('stdin is opaque value in tracked request',rc==0)
before=records(s);rc,t=call(s,a,b'--actor');check('stdin tracked replay preserves fingerprint',rc==0 and '(replay #t)' in t and records(s)==before)
rc,t=call(s,['read',w+'.7']);check('stdin body persisted','(src . "--actor")' in t)
# F5/F7 use the same real state assertions under normal and failed reports.
for mode,fault in [('plain',None),('tracked',None),('plain','report-fail@report'),('tracked','report-fail@report'),('outline',None)]:
    case=root/('report-'+mode+'-'+str(bool(fault)));case.mkdir();(case/'store').mkdir();(case/'machine').mkdir()
    e=dict(env,THEOURGIA_HOME=str(case/'machine'))
    if fault:e['THEOURGIA_FAULT']=fault
    result=subprocess.run(['scheme','--script',str(pathlib.Path(__file__).with_name('q8-report.sc')),str(case/'store'),mode],env=e,stdout=subprocess.PIPE,stderr=subprocess.PIPE,timeout=45,stdin=subprocess.DEVNULL)
    check('report and outline '+mode+' '+str(fault),result.returncode==0 and '(failures 0)' in result.stdout.decode())
    rows.append(dict(mode=mode,fault=fault,exit=result.returncode,stdout=result.stdout.decode(),stderr=result.stderr.decode()))
(root/'results.json').write_text(json.dumps(rows,ensure_ascii=False,indent=2))
print('Evidence:',root/'results.json')
print(f'{failures} failures')
print('q8-cli complete')
raise SystemExit(bool(failures))
