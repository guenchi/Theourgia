'use strict';
// The VS Code API and core latency are substituted. Activation, model,
// ownership, publication, chain and filesystem are the compiled product.
const fs = require('fs'), path = require('path'), os = require('os'), Module = require('module');
const out = path.join(__dirname, '..', '..', 'src');
const storage = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-extension-schedule-'));
// A CORE DIRECTORY THAT EXISTS, because the extension now reads it. The
// core's answers are substituted below and nothing here runs Chez -- but
// the settings check probes this path for `client.ss` or `client.so` and
// refuses a directory that holds neither, so a made-up path made every
// scenario in this file report unusable settings and open nothing. The
// probe is the point: this stands in for the directory form, not for the
// core.
const corePath = fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-extension-schedule-core-'));
fs.writeFileSync(path.join(corePath, require(path.join(out, 'config.js')).WITNESS_SOURCE), ';; stand-in\n', 'utf8');
const settings = {store:'/stores/A',corePath,scheme:'scheme',actor:'probe',libDirs:[],timeoutMs:1000};
let provider, configChanged, savedHandler, core, pickGate = null, requestGate = null, failWrite=false;
// Queue item 43 (ruling Q6): a gate per verb, each held by its own scenario step,
// optionally only for the request `match` accepts; releasing one with an answer
// replaces the stand-in's own answer to that request. And `afterRead`, which holds
// a Working.read after its reading is obtained and before it is returned.
const verbGates = {};let afterRead = null;
// NEVER: `channels` ALONE CANNOT SAY WHICH MESSAGE WAS WHICH. A scenario that
// shows several notices satisfies "an error was raised" with an unrelated one:
// measured in a twelfth review round, the earlier setup saves in the save
// scenarios raise their own errors, so the assertion about the wrong-store
// message held whatever channel that message used. `shown` pairs each text with
// its channel, which is what lets a cell ask about ONE of them.
const commands = new Map(), messages = [], channels = [], shown = [], requests = [], outputLines = [];
const workingNotes=new Map();let workingVersion=0;
const disposable = {dispose(){}};
function gate(){let release,enter;const promise=new Promise(r=>release=r),entered=new Promise(r=>enter=r);return {promise,release,entered,enter};}
const docs=[];
const vs = {
 EventEmitter:class{constructor(){this.event=()=>disposable;}fire(){}dispose(){}},
 TreeItem:class{constructor(label,collapsibleState){this.label=label;this.collapsibleState=collapsibleState;}},ThemeIcon:class{constructor(id){this.id=id;}},ThemeColor:class{},
 TreeItemCollapsibleState:{None:0,Collapsed:1},StatusBarAlignment:{Right:1},Uri:{file:p=>({fsPath:p})},
 languages:{setTextDocumentLanguage:async d=>d},
 window:{createStatusBarItem:()=>({show(){},dispose(){}}),createOutputChannel:()=>({appendLine(line){outputLines.push(line);},show(){},dispose(){}}),registerTreeDataProvider:(n,p)=>{provider=p;return disposable;},
  // NEVER: THE CHANNEL IS KEPT. All three pushed the bare text, so routing an
  // error through showInformationMessage left every observation identical and
  // no cell could tell a warning from an alarm. The other editor double was
  // repaired for this in an earlier round; this one was missed because that
  // repair was made where the finding pointed. `messages` keeps the text so
  // the cells that read it are unchanged; `channels` is the new reading.
  showErrorMessage:m=>{channels.push('error');shown.push({text:m,level:'error'});return messages.push(m);},
  showWarningMessage:m=>{channels.push('warning');shown.push({text:m,level:'warning'});messages.push(m);if(process.argv[3]==='integrity-show-throws')throw new Error('the editor threw');return process.argv[3]==='integrity-show-rejects'?Promise.reject(new Error('the editor refused the warning')):Promise.resolve(undefined);},
  showInformationMessage:m=>{channels.push('information');shown.push({text:m,level:'information'});return messages.push(m);},
  showQuickPick:async items=>{if(!pickGate)throw Error('Unexpected picker');pickGate.enter(items);return pickGate.promise;},showTextDocument:async d=>d},
 workspace:{textDocuments:docs,getConfiguration:()=>({get:(k,f)=>settings[k]??f}),onDidSaveTextDocument:f=>{savedHandler=f;return disposable;},
  onDidChangeConfiguration:f=>{configChanged=f;return disposable;},
  registerTextDocumentContentProvider:()=>disposable,onDidCloseTextDocument:()=>disposable,
  openTextDocument:async uri=>({uri,isDirty:false,getText:()=>fs.readFileSync(uri.fsPath,'utf8')})},
 commands:{registerCommand:(n,f)=>{commands.set(n,f);return disposable;}}
};
const originalLoad=Module._load;
Module._load=function(name,...rest){if(name==='vscode')return vs;return originalLoad.call(this,name,...rest);};
const activation=require(path.join(out,'activate.js')), originalActivate=activation.activateCore;
activation.activateCore=deps=>{core=originalActivate(deps);return core;};
const accepting=require(path.join(out,'accepting.js')), accept=accepting.acceptSave;
const acceptances=[], numbering=[];
accepting.acceptSave=parts=>{const result=accept(parts);acceptances.push(result);return result;};
const {Client}=require(path.join(out,'client.js'));
const {Working}=require(path.join(out,'working.js')),workingRead=Working.prototype.read;
Working.prototype.read=async function(...args){const reading=await workingRead.apply(this,args);if(afterRead){const held=afterRead;afterRead=null;held.enter();await held.promise;}return reading;};
Client.fromConfig=cfg=>new Client({kind:'schedule',send:async(verb,args)=>{
 requests.push({store:cfg.store,verb,args});
 if(requestGate && verb===requestGate.verb && (!requestGate.working || args.includes('--working-info'))){const held=requestGate;requestGate=null;held.enter();await held.promise;}
 const byVerb=verbGates[verb];
 if(byVerb&&(!byVerb.match||byVerb.match(args))){delete verbGates[verb];byVerb.enter();const replaced=await byVerb.promise;if(replaced)return {argv:[verb,...args],...replaced};}
 const title=cfg.store.endsWith('A')?'Alpha':'Beta';
 const writerAt=args.indexOf('--writer'),writer=writerAt<0?'default':args[writerAt+1],key=cfg.store+'|'+writer+'|'+args[0];
 if(verb==='write'){
  if(failWrite)return {argv:[verb,...args],rc:1,stdout:'(error working-unavailable)\n',stderr:''};
  const version='v'+(++workingVersion);workingNotes.set(key,{version,body:args[1]});
  return {argv:[verb,...args],rc:0,stdout:`(ok (saved "${args[0]}") (writer "${writer}") (version "${version}") (based-on "base"))\n`,stderr:''};
 }
 if(verb==='read'&&args.includes('--working-info')){
  const note=workingNotes.get(key),q=JSON.stringify;
  return {argv:[verb,...args],rc:0,stdout:`(ok (projection ${note?'working':'committed'} ${q(writer)} ${q(args[0])} ${note?q(note.version):'#f'} "base" () ${q(note?note.body:'body\n')} ${q('# '+title+'\n')}))\n`,stderr:''};
 }
 // Queue item 39: in notice-behind a commit succeeds and says who else landed; in
 // notice-refused the store refuses it. Every other scenario keeps `unknown`.
 if(verb==='commit'&&process.argv[3]==='notice-behind') return {argv:[verb,...args],rc:0,stdout:'(ok (items (ok (events (("w" . 1))) (state (("a.1" . "hhh"))) (cursor ("w" . 1)) (replay #f))) (behind (("w" . 1) ("other" . 2))))\n',stderr:''};
 if(verb==='commit'&&process.argv[3]==='notice-refused') return {argv:[verb,...args],rc:1,stdout:'(error cursor-unreachable (after ("w" . 999)) (writing ("w" . 8)))\n',stderr:''};
 if(verb==='commit') return {argv:[verb,...args],rc:1,stdout:'(error unknown (reason schedule))\n',stderr:''};
 // Queue item 48 (review r1 #2): the store reports a.2, a child of a.1, as a nested document.
 if(verb==='conflicts'&&process.argv[3]==='outline-nested') return {argv:[verb,...args],rc:0,stdout:'(nested-document "a.2")\n',stderr:''};

 const read=`(ok ((id . "a.1") (deleted . #f) (fields (heading-src . "# ${title}\\n") (src . "body\\n") (title . "${title}")) (position root . 0) (edges)))\n`;
 // NEVER: THE SUBTREE INCLUDES THE BLOCK IT IS UNDER. Measured against the
 // pinned core: `read <id> --recursive` answers with the block itself first
 // and then its descendants. This stand-in answered with the child alone,
 // which is a shape the core does not produce -- and the product now refuses
 // it, correctly, because an answer that never mentions the block is not an
 // answer about that block.
 const recursive=`((id . \"a.1\") (deleted . #f) (fields (heading-src . \"# ${title}\\n\") (src . \"body\\n\") (title . \"${title}\")) (position root . 0) (edges))\n((id . \"a.2\") (deleted . #f) (fields (heading-src . \"# ${title}\\n\") (src . \"body\\n\") (title . \"${title}\")) (position \"a.1\" . 0) (edges))\n`;
 const stdout=verb==='read'&&args.includes('--recursive')?recursive:verb==='conflicts'&&process.argv[3].includes('unknown')?'(error unavailable)\n':verb==='outline'?`- a.1  ${title}\n`:verb==='read'?read:verb==='check'?(process.argv[3].startsWith('integrity-show')?'(check (writers (("w" (end 0)))) (verdict damaged))\n':'(check (writers (("w" (end 0)))) (verdict ok))\n'):verb==='set'?(process.argv[3]==='durability-order'?'(ok (events (("w" . 1))) (state (("a.9" . "hhh"))) (cursor ("w" . 1)) (replay #f))\n':'(error unknown (reason schedule))\n'):'';
 // Queue item 46, E7: in durability-order a `set` is answered ok, so the settle writes the queue.
 const setOk=verb==='set'&&process.argv[3]==='durability-order';
 return {argv:[verb,...args],rc:verb==='set'&&!setOk||verb==='conflicts'&&process.argv[3].includes('unknown')?1:0,stdout,stderr:''};}});
function files(dir){if(!fs.existsSync(dir))return [];return fs.readdirSync(dir).flatMap(n=>{const p=path.join(dir,n);return fs.statSync(p).isDirectory()?files(p):[p];});}
function tree(dir){return Object.fromEntries(files(dir).map(p=>[path.relative(dir,p),fs.readFileSync(p).toString('hex')]));}
function change(store){settings.store=store;configChanged({affectsConfiguration:()=>true});}
// Queue item 43. What a record is, read from the file beside it (revision included),
// for comparing before and after.
function recordOf(file){const r=core.publisher.sidecarOf(file);return r===null?null:{revision:r.revision,phase:r.phase,written:r.written,nextSeq:r.nextSeq,highWater:r.highWater,outstanding:r.outstanding,projection:r.projection?{kind:r.projection.kind,id:r.projection.id,writer:r.projection.writer,version:r.projection.version,basedOn:r.projection.basedOn}:null};}
// Queue item 43 (design v4 V2): the outcome observed as a structure, not as a
// sentence. The extension's own Publisher is wrapped where it is exposed; each
// guard, reconciliation and publication records the stage it refused at and why
// (null when it went through).
function traced(){
 const trace=[],p=core.publisher,guard=p.reconciliationGuard.bind(p),by=p.reconcileBy.bind(p),publish=p.publish.bind(p);
 p.reconciliationGuard=(...args)=>{const because=guard(...args);trace.push({stage:'early',because});return because;};
 p.reconcileBy=(...args)=>{const done=by(...args);trace.push({stage:'late',because:done.done?null:done.because});return done;};
 p.publish=async(...args)=>{const outcome=await publish(...args);trace.push({stage:'publish',because:outcome.published?null:outcome.because});return outcome;};
 return trace;
}
// Queue item 43 (ruling Q6): a second Publisher of the SAME window -- the extension's
// session, a fresh Owners -- records a working write on the file, moving its record
// as a write this window makes outside the block chain would. With `text`, the file
// is given those bytes first. Answers the revision the write left.
function secondWrite(file,text){
 const {Publisher,digestOfBytes}=require(path.join(out,'publication.js')),{Owners}=require(path.join(out,'ownership.js')),{nodeFileOps}=require(path.join(out,'fsops.js'));
 const sessionId=path.relative(storage,file).split(path.sep)[1];
 const second=new Publisher(nodeFileOps,{isOpen:()=>false},{owners:new Owners(),sessionId});
 const held=second.sidecarOf(file);
 if(text!==undefined)fs.writeFileSync(file,text);
 const source={id:'second-origin',kind:'working',writer:'window-'+sessionId.toLowerCase(),version:'v-second',basedOn:'base',cut:'()'};
 if(!second.recordWorking(file,source,digestOfBytes(fs.readFileSync(file)),held.projection.id,held.revision))throw Error('The second publisher did not record its working write');
 return second.sidecarOf(file).revision;
}
function within(promise,ms){let timer;return Promise.race([promise.then(()=>true),new Promise(r=>{timer=setTimeout(()=>r(false),ms);})]).finally(()=>clearTimeout(timer));}
// A commit the store answers ok (the form notice-behind uses), for R8 and the save's twin.
const COMMIT_OK={rc:0,stdout:'(ok (items (ok (events (("w" . 1))) (state (("a.1" . "hhh"))) (cursor ("w" . 1)) (replay #f))) (behind (("w" . 1))))\n',stderr:''};
async function main(){
 const scenario=process.argv[3];
 // Queue item 33, C7: the integrity scenarios count unhandled rejections in this
 // process, from before activation. Only they do: every other scenario keeps Node's
 // default, which ends the process -- a tripwire in its own right.
 const unhandledSeen=new Set();
 if(scenario.startsWith('integrity-show'))process.on('unhandledRejection',(reason,promise)=>unhandledSeen.add(promise));
 await require(path.join(out,'extension.js')).activate({globalStorageUri:{fsPath:storage},subscriptions:[]});
 // Queue item 39: one save of the opened block, and what the editor was shown.
 if(scenario==='notice-behind'||scenario==='notice-refused'){
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));const body='# Alpha\nfirst A\n';fs.writeFileSync(a,body);
  shown.length=0;
  await savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>body});
  return {shown:shown.slice(),requests:requests.filter(r=>r.verb==='commit').length};
 }
 if(scenario.startsWith('save')){
  const number=core.publisher.takeSequence.bind(core.publisher);
  core.publisher.takeSequence=(...args)=>{numbering.push(args);return number(...args);};
  async function save(file,body){fs.writeFileSync(file,body);await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>body});}
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  await save(a,'# Alpha\nfirst A\n');
  change('/stores/B');await commands.get('theourgia.openBlock')('a.1');
  const b=files(storage).find(p=>p.endsWith('.md')&&!p.startsWith(path.dirname(a)+path.sep));
  await save(b,'# Beta\nfirst B\n');
  const queueA=core.outboxPath('/stores/A'),queueB=core.outboxPath('/stores/B');
  const beforeQueues=[queueA,queueB].map(p=>fs.readFileSync(p,'hex'));
  change('/stores/A');
  const body='# Alpha\nsecond A\n';fs.writeFileSync(a,body);
  const beforeMeta=fs.readFileSync(a+'.meta','hex');
  acceptances.length=0;numbering.length=0;
  const document={uri:{fsPath:a},isDirty:false,getText:()=>body};
  if(scenario==='save-working-failed'){
   failWrite=true;await savedHandler(document);
  }else if(scenario.startsWith('save-working-')){
   const wait=gate();requestGate={...wait,verb:'write'};
   const pending=savedHandler(document);await wait.entered;
   if(scenario==='save-working-switch')change('/stores/B');
   else new (require(path.join(out,'ownership.js')).Owners)().take(path.dirname(a),'another-owner',[]);
   wait.release();await pending;
  }else if(scenario==='save-early'){
   change('/stores/B');await savedHandler(document);
  }else{
   const blocker=gate();
   const blocking=core.chain.run(path.dirname(a),async()=>{blocker.enter();await blocker.promise;if(scenario==='save-prefix'){core.publisher.reconcile(a,'# ',body);}});
   await blocker.entered;
   const queued=gate(),run=core.chain.run.bind(core.chain);
   core.chain.run=(dir,work)=>{queued.enter();return run(dir,work);};
   const pending=savedHandler(document);await queued.entered;
   if(scenario==='save-chain')change('/stores/B');
   blocker.release();await blocking;await pending;
  }
  return {a,b,beforeQueues,afterQueues:[queueA,queueB].map(p=>fs.readFileSync(p,'hex')),beforeMeta,afterMeta:fs.readFileSync(a+'.meta','hex'),acceptances,numbering,messages,channels,shown};
 }
 if(scenario==='open-late-dirty'||scenario==='open-late-clean'){
  await commands.get('theourgia.openBlock')('a.1');
  const file=files(storage).find(p=>p.endsWith('.md'));
  const before=[file,file+'.meta'].map(p=>fs.readFileSync(p,'hex'));
  const wait=gate();requestGate={...wait,verb:'read',working:true};
  const opening=commands.get('theourgia.openBlock')('a.1');await wait.entered;
  docs.push({uri:{fsPath:file},isDirty:scenario==='open-late-dirty',getText:()=> 'unsaved buffer'});
  wait.release();await opening;
  return {file,before,after:[file,file+'.meta'].map(p=>fs.readFileSync(p,'hex')),messages,channels,shown};
 }
 // Queue item 43, R3 (T7): the record moves while an open's reading is being taken.
 // The second open is held after Working.read has its reading and before it
 // publishes; meanwhile a second Publisher of the same window writes the file and
 // records it. The open must be refused `record-moved` and leave that write alone.
 if(scenario==='open-moved'){
  const trace=traced();
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  const before=recordOf(a);
  const held=gate();afterRead=held;
  const opening=commands.get('theourgia.openBlock')('a.1');
  const entered=await within(held.entered,5000);
  const moved=entered?secondWrite(a,'# Alpha\nthe second write\n'):null;
  trace.length=0;shown.length=0;
  held.release();await opening;
  return {entered,before,moved,after:recordOf(a),file:fs.readFileSync(a,'utf8'),trace,shown:shown.slice()};
 }
 // Queue item 43, R4/R5 (item 47's measurement, restored as cells): a save lands
 // between reconcileBlock's two chain waits, while the pick is open. midpick-save
 // saves new text; midpick-aba saves new text and then puts the offered bytes back
 // without saving, so the bytes the second wait compares are the offered ones while
 // the record has moved.
 if(scenario==='midpick-save'||scenario==='midpick-aba'){
  const trace=traced();
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  fs.writeFileSync(a,'offered text\n');
  const beforePick=recordOf(a);
  pickGate=gate();const pending=commands.get('theourgia.reconcileBlock')(a);const choices=await pickGate.entered;
  const saved='# Alpha\nsaved while the pick was open\n';fs.writeFileSync(a,saved);
  await savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>saved});
  const afterSave=recordOf(a);
  if(scenario==='midpick-aba')fs.writeFileSync(a,'offered text\n');
  trace.length=0;shown.length=0;
  pickGate.release(choices[0]);const notice=await pending;
  const queueA=core.outboxPath('/stores/A');
  const queue=fs.existsSync(queueA)?JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>({req:e.req,state:e.state,expectation:e.record&&e.record.intent&&e.record.intent.expectation})):[];
  return {choice:choices[0]&&choices[0].action,notice,beforePick,afterSave,afterReconcile:recordOf(a),file:fs.readFileSync(a,'utf8'),queue,writes:requests.filter(r=>r.verb==='write').length,trace,shown:shown.slice()};
 }
 // Queue item 43, R6 (T9): a save lands after reconcileBlock's routing read and
 // before its first wait, and the offered bytes -- without the heading, so the
 // offer's route is taken -- are put back. The offer is built in the first wait
 // from the record the save left, and the second wait applies it: its one working
 // write names the save's working version as its parent.
 if(scenario==='midpick-prechain-save'){
  const trace=traced();
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  fs.writeFileSync(a,'offered text\n');
  const beforePick=recordOf(a);
  const run=core.chain.run.bind(core.chain);let landed=null;
  core.chain.run=async(dir,work)=>{
   if(path.resolve(dir)===path.resolve(path.dirname(a))){
    core.chain.run=run;
    const saved='# Alpha\nsaved before the first wait\n';fs.writeFileSync(a,saved);
    await savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>saved});
    landed=recordOf(a);
    fs.writeFileSync(a,'offered text\n');
   }
   return run(dir,work);
  };
  pickGate=gate();const pending=commands.get('theourgia.reconcileBlock')(a);const choices=await pickGate.entered;
  const atPick=requests.filter(r=>r.verb==='write').length;
  trace.length=0;shown.length=0;
  pickGate.release(choices[0]);const notice=await pending;
  return {choice:choices[0]&&choices[0].action,notice,beforePick,landed,after:recordOf(a),file:fs.readFileSync(a,'utf8'),reconcileWrites:requests.filter(r=>r.verb==='write').slice(atPick).map(r=>r.args),trace,shown:shown.slice()};
 }
 // Queue item 43, R6b (U3, V2): the record moves after the second wait's guard has
 // validated it and before the working note is written. A wrapper around the guard
 // has a second Publisher of the same window record a working write the moment the
 // guard answers "go on". The note must still be written against the offer's
 // projection, and the reconciliation refused `record-moved`, late.
 if(scenario==='midpick-afterguard'){
  const trace=traced();
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  const saved='# Alpha\nsaved first\n';fs.writeFileSync(a,saved);
  await savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>saved});
  fs.writeFileSync(a,'offered text\n');
  const beforePick=recordOf(a);
  pickGate=gate();const pending=commands.get('theourgia.reconcileBlock')(a);const choices=await pickGate.entered;
  const guard=core.publisher.reconciliationGuard;let second=null;
  core.publisher.reconciliationGuard=(...args)=>{const because=guard(...args);if(because===null&&second===null)second=secondWrite(a);return because;};
  const atPick=requests.filter(r=>r.verb==='write').length;
  trace.length=0;shown.length=0;
  pickGate.release(choices[0]);const notice=await pending;
  return {choice:choices[0]&&choices[0].action,notice,beforePick,second,after:recordOf(a),file:fs.readFileSync(a,'utf8'),reconcileWrites:requests.filter(r=>r.verb==='write').slice(atPick).map(r=>r.args),trace,shown:shown.slice()};
 }
 // Queue item 43, R8 (U6, V1) and its twin on the save path (ruling Q2): a
 // settlement written by this window's own drain, which does not wait on the block
 // chain, lands while a working write is in flight. Two saves leave two requests
 // pending; the first was sent for the first save's origin, which the second save
 // replaced, so the store's `ok` for it retires its number (saving.ts, the older-
 // origin write). Its commit is held until the working write has entered its own
 // gate; the record must have moved before that write is let go.
 if(scenario==='midpick-settlement'||scenario==='working-save-settlement'){
  const trace=traced();
  async function save(file,body){fs.writeFileSync(file,body);await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>body});}
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  await save(a,'# Alpha\nfirst save\n');
  await save(a,'# Alpha\nsecond save\n');
  const queueA=core.outboxPath('/stores/A'),pendingBefore=JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>({req:e.req,state:e.state}));
  const first=pendingBefore[0]&&pendingBefore[0].req;
  let pending=null,choices=null;
  if(scenario==='midpick-settlement'){
   fs.writeFileSync(a,'offered text\n');
   pickGate=gate();pending=commands.get('theourgia.reconcileBlock')(a);choices=await pickGate.entered;
  }
  const held=recordOf(a);
  const commitGate=Object.assign(gate(),{match:args=>args.includes(first)});verbGates.commit=commitGate;
  const retrying=commands.get('theourgia.retryOutbox')();
  const commitEntered=await within(commitGate.entered,5000);
  const writeGate=gate();verbGates.write=writeGate;
  trace.length=0;shown.length=0;messages.length=0;
  const third='# Alpha\nthird save\n';
  if(scenario==='midpick-settlement')pickGate.release(choices[0]);
  else{fs.writeFileSync(a,third);pending=savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>third});}
  const writeEntered=await within(writeGate.entered,5000);
  commitGate.release(COMMIT_OK);
  let advanced=false;
  for(let i=0;i<300&&!advanced;i++){await new Promise(r=>setTimeout(r,10));advanced=recordOf(a).revision>held.revision;}
  const atRelease=recordOf(a);
  writeGate.release();
  const notice=await pending;await retrying;
  const queueAfter=JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>({req:e.req,state:e.state}));
  return {commitEntered,writeEntered,advanced,pendingBefore,held,atRelease,after:recordOf(a),file:fs.readFileSync(a,'utf8'),queueAfter,notice,trace,shown:shown.slice(),messages:messages.slice()};
 }
 // Queue item 43, review r1 #1: a record written by another process after the first
 // wait's reconcile has left its exclusive section, and before anything else in that
 // wait. The Publisher's reconcileLeaving is wrapped so that, the moment it returns,
 // a second Publisher records a working write (the bytes unchanged). The offer was
 // built from the record before that write, so the second wait must refuse early.
 if(scenario==='midpick-afterreconcile'){
  const trace=traced();
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  fs.writeFileSync(a,'offered text\n');
  const beforePick=recordOf(a);
  const leaving=core.publisher.reconcileLeaving.bind(core.publisher);let second=null;
  core.publisher.reconcileLeaving=(...args)=>{const answer=leaving(...args);if(second===null)second=secondWrite(a);return answer;};
  pickGate=gate();const pending=commands.get('theourgia.reconcileBlock')(a);const choices=await pickGate.entered;
  trace.length=0;shown.length=0;
  pickGate.release(choices[0]);const notice=await pending;
  return {choice:choices[0]&&choices[0].action,notice,beforePick,second,after:recordOf(a),file:fs.readFileSync(a,'utf8'),writes:requests.filter(r=>r.verb==='write').length,trace,shown:shown.slice()};
 }
 // Queue item 43, review r1 #5 and #6: a chain callback queued AHEAD of an open or a
 // save moves the record (takeSequence) before the open's or the save's own wait
 // starts. What they name must be read inside their wait, so the open publishes and
 // the save is recorded; a revision read before the wait would be refused.
 if(scenario==='open-queued'||scenario==='queued-save'){
  const trace=traced();
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  const blocker=gate();
  const ahead=core.chain.run(path.dirname(a),async()=>{blocker.enter();await blocker.promise;core.publisher.takeSequence(a,'queued-ahead');});
  await blocker.entered;
  const queued=gate(),run=core.chain.run.bind(core.chain);
  core.chain.run=(dir,work)=>{core.chain.run=run;queued.enter();return run(dir,work);};
  const body='# Alpha\nsaved behind a queued move\n';
  if(scenario==='queued-save')fs.writeFileSync(a,body);
  const before=recordOf(a);
  trace.length=0;shown.length=0;messages.length=0;
  const pending=scenario==='open-queued'?commands.get('theourgia.openBlock')('a.1'):savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>body});
  const entered=await within(queued.entered,5000);
  blocker.release();await ahead;await pending;
  return {entered,before,after:recordOf(a),trace,shown:shown.slice(),messages:messages.slice()};
 }
 // Queue item 43, review r3 S1: the same settlement as R8, on the AUTOMATIC route. The file
 // already carries the heading, so the first wait reconciles it itself (a record write),
 // then writes the working note and records it against the revision reconcileLeaving
 // returned. An older-origin commit, held until that note's write has entered its gate,
 // settles in between; the note must not be recorded over the settlement's record.
 if(scenario==='autoreconcile-settlement'){
  const trace=traced();
  async function save(file,body){fs.writeFileSync(file,body);await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>body});}
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  await save(a,'# Alpha\nfirst save\n');
  await save(a,'# Alpha\nsecond save\n');
  const queueA=core.outboxPath('/stores/A'),pendingBefore=JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>({req:e.req,state:e.state}));
  const first=pendingBefore[0]&&pendingBefore[0].req;
  const leaving=core.publisher.reconcileLeaving.bind(core.publisher);let left=null;
  core.publisher.reconcileLeaving=(...args)=>{const answer=leaving(...args);if(left===null)left=recordOf(a);return answer;};
  const commitGate=Object.assign(gate(),{match:args=>args.includes(first)});verbGates.commit=commitGate;
  const retrying=commands.get('theourgia.retryOutbox')();
  const commitEntered=await within(commitGate.entered,5000);
  const writeGate=gate();verbGates.write=writeGate;
  trace.length=0;shown.length=0;messages.length=0;
  const pending=commands.get('theourgia.reconcileBlock')(a);
  const writeEntered=await within(writeGate.entered,5000);
  commitGate.release(COMMIT_OK);
  let advanced=false;
  for(let i=0;i<300&&!advanced;i++){await new Promise(r=>setTimeout(r,10));advanced=left!==null&&recordOf(a).revision>left.revision;}
  writeGate.release();
  const notice=await pending;await retrying;
  return {commitEntered,writeEntered,advanced,pendingBefore,left,after:recordOf(a),file:fs.readFileSync(a,'utf8'),notice,trace,shown:shown.slice(),messages:messages.slice()};
 }
 if(scenario.startsWith('reconcile')){
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  if(!a)throw Error('Opening A did not publish a block');
  change('/stores/B');await commands.get('theourgia.openBlock')('a.1');
  const b=files(storage).find(p=>p.endsWith('.md')&&!p.startsWith(path.dirname(a)+path.sep));
  if(!b)throw Error('Opening B did not publish a block');
  fs.writeFileSync(b,'B must survive\n');const beforeB=tree(path.dirname(b));
  change('/stores/A');fs.writeFileSync(a,'offered text\n');
  const beforeA=tree(path.dirname(a));
  pickGate=gate();const pending=commands.get('theourgia.reconcileBlock')(a);const choices=await pickGate.entered;
  let blocker;
  if(scenario.includes('chain')){
   blocker=gate();core.chain.run(path.dirname(a),async()=>{blocker.enter();await blocker.promise;});await blocker.entered;
   const queued=gate(), original=core.chain.run.bind(core.chain);
   core.chain.run=(dir,run)=>{queued.enter();return original(dir,run);};
   pickGate.release(choices[0]);await queued.entered;
  }
  if(scenario.includes('switch'))change('/stores/B');
  if(scenario.includes('dirty'))docs.push({uri:{fsPath:a},isDirty:true,getText:()=> 'unsaved buffer'});
  if(scenario.includes('changed'))fs.writeFileSync(a,'later edit\n');
  if(blocker)blocker.release();else pickGate.release(choices[0]);
  const notice=await pending;
  return {notice,a,b,beforeA,afterA:tree(path.dirname(a)),beforeB,afterB:tree(path.dirname(b)),messages,requests,channels};
 }
 // Queue item 48 (review r1 #2): the tree item the editor would draw for a nested document
 // under its parent -- the parent's and the child's -- with the icon and tooltip it carries.
 if(scenario==='outline-nested'){
  const roots=await provider.getChildren();
  const root=roots.find(n=>n.id==='a.1');
  const rootItem=root?provider.getTreeItem(root):null;
  const children=root?await provider.getChildren(root):[];
  const child=children.find(n=>n.id==='a.2');
  const item=child?provider.getTreeItem(child):null;
  return {rootCollapsible:rootItem&&rootItem.collapsibleState,childIds:children.map(n=>n.id),marks:child?child.marks:null,
   icon:item&&item.iconPath?item.iconPath.id:null,tooltip:item?item.tooltip:null};
 }
 if(scenario==='outline-old-click'){
  const nodes=await provider.getChildren();
  const item=provider.getTreeItem(nodes[0]);
  change('/stores/B');const before=requests.length;
  await commands.get(item.command.command)(...item.command.arguments);
  return {afterClickReads:requests.slice(before).filter(r=>r.verb==='read'),messages,channels,shown};
 }
 if(scenario.startsWith('outline')){
  requestGate=Object.assign(gate(),{verb:scenario.includes('children')?'read':'outline'});
  const held=requestGate;
  const pending=provider.getChildren(scenario.includes('children')?{id:'a.1'}:undefined);
  await held.entered;
  if(scenario.includes('switch'))change('/stores/B');
  if(scenario.includes('aba')){change('/stores/B');change('/stores/A');}
  held.release();const nodes=await pending;
  return {nodes,messages,requests};
 }
 if(scenario.startsWith('integrity-show')){
  for(let i=0;i<5;i++)await new Promise(r=>setImmediate(r));
  await new Promise(r=>setTimeout(r,50));
  return {lines:outputLines,unhandled:unhandledSeen.size,warnings:shown.filter(n=>n.level==='warning').map(n=>n.text)};
 }
 // Queue item 22, D4 and D4b: the queue directory's flush fails ONCE, after a
 // save has left an entry pending (the stand-in answers every commit unknown).
 // One-shot, because the window's own later drains write the same queue: a
 // flush that kept failing would raise the same sentence again and hide a
 // warning that was lost.
 if(scenario.startsWith('durability')){
  const fsops=require(path.join(out,'fsops.js')),sync=fsops.nodeFileOps.syncDirectory;
  const queueA=core.outboxPath('/stores/A');let armed=false,failed=0,failWhen=null,order=[];
  fsops.nodeFileOps.syncDirectory=d=>{
   const here=path.resolve(d)===path.resolve(path.dirname(queueA));
   if(here&&order.length>0){failed++;return order.shift();}
   if((armed||(failWhen&&failWhen()))&&here){armed=false;failWhen=null;failed++;return 'the disk said no';}
   return sync.call(fsops.nodeFileOps,d);};
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));const body='# Alpha\nfirst A\n';fs.writeFileSync(a,body);
  await savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>body});
  const pendingBefore=JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>e.state);
  shown.length=0;
  // D4f (folded from item 22's closing review): the awaited save's OWN warning.
  // A second save, whose enqueue is the write that fails -- the first time the
  // queue holds an entry still `queued` -- so the sentence rides on that save's
  // outcome, and only the save handler's own display of it can show it.
  // E7 (queue item 46): the window keeps the NEWER warning when the older one
  // reaches its sink later. The pending entry the prologue left is settled away
  // and one legacy `set` entry queued; the retry command's drain marks it sent
  // under a flush that fails "the older reason" (held on the outcome), the
  // stand-in answers the set ok, and the settle's resolve fails "the newer
  // reason", reported straight into the sink. The older arrives second.
  if(scenario==='durability-order'){
   const {Outbox}=require(path.join(out,'outbox.js')),q=new Outbox(queueA);q.load();
   for(const e of q.entries)q.resolve(e.req,null);
   q.enqueue({req:'legacy-order',cursor:'w:1',id:'a.9',field:'src',payload:'queued first\n',state:'queued',createdAt:0,lastError:null,importedBy:null});
   order=['the older reason','the newer reason'];
   await commands.get('theourgia.retryOutbox')();
   const after=JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>e.req);
   return {queue:queueA,pendingBefore,failed,left:order.length,after,shown:shown.slice()};
  }
  if(scenario==='durability-save'){
   failWhen=()=>fs.existsSync(queueA)&&JSON.parse(fs.readFileSync(queueA,'utf8')).entries.some(e=>e.state==='queued');
   const second='# Alpha\nsecond A\n';fs.writeFileSync(a,second);
   await savedHandler({uri:{fsPath:a},isDirty:false,getText:()=>second});
   return {queue:queueA,pendingBefore,failed,shown:shown.slice()};
  }
  if(scenario==='durability-retry'){
   // D4 is about the retry command's RELEASE (K6): the entry is parked first,
   // through the product's own queue, so the one failing flush is unparkAll's.
   const {Outbox}=require(path.join(out,'outbox.js')),held=new Outbox(queueA);held.load();
   held.markParked(held.entries[0].req,'parked for the cell');
   const parkedBefore=JSON.parse(fs.readFileSync(queueA,'utf8')).entries.map(e=>e.state);
   armed=true;await commands.get('theourgia.retryOutbox')();
   const afterRetry=shown.slice();shown.length=0;
   await commands.get('theourgia.showStatus')({ask:false});
   return {queue:queueA,pendingBefore,parkedBefore,failed,afterRetry,afterNext:shown.slice()};
  }
  if(scenario==='durability-rebuild'){
   const wait=gate();requestGate={...wait,verb:'commit'};
   const retrying=commands.get('theourgia.retryOutbox')();await wait.entered;
   change('/stores/A');const afterChange=shown.slice();
   armed=true;wait.release();await retrying;
   for(let i=0;i<5;i++)await new Promise(r=>setImmediate(r));
   return {queue:queueA,pendingBefore,failed,afterChange,shown:shown.slice()};
  }
  // D4b through the STARTUP retry (delivery review r2 of item 22, S1): a settings
  // change rebuilds and the new Saver's startup drain is held at its commit; a
  // second change replaces that Saver too; only then is the held commit
  // answered and the replaced Saver's mark written under the failing flush. What
  // shows it is the startup retry's own take -- no command runs here.
  // The second change is to store B, whose queue is empty (review r3, S1): its
  // startup drain is over before the held commit answers, so nothing but the
  // replaced Saver's own callback is left to show the sentence.
  if(scenario==='durability-startup'){
   const wait=gate();requestGate={...wait,verb:'commit'};
   change('/stores/A');await wait.entered;
   change('/stores/B');for(let i=0;i<20;i++)await new Promise(r=>setTimeout(r,10));
   const afterChange=shown.slice();
   armed=true;wait.release();
   for(let i=0;i<200&&failed===0;i++)await new Promise(r=>setTimeout(r,10));
   for(let i=0;i<20;i++)await new Promise(r=>setTimeout(r,10));
   return {queue:queueA,pendingBefore,failed,afterChange,shown:shown.slice()};
  }
 }
 throw Error('Unknown schedule');
}
main().then(result=>{fs.writeSync(1,JSON.stringify({complete:true,result})+'\n');}).catch(error=>{fs.writeSync(2,String(error.stack)+'\n');process.exitCode=1;});
