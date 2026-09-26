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
 TreeItem:class{constructor(label){this.label=label;}},ThemeIcon:class{},ThemeColor:class{},
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
Client.fromConfig=cfg=>new Client({kind:'schedule',send:async(verb,args)=>{
 requests.push({store:cfg.store,verb,args});
 if(requestGate && verb===requestGate.verb && (!requestGate.working || args.includes('--working-info'))){const held=requestGate;requestGate=null;held.enter();await held.promise;}
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
 if(verb==='commit') return {argv:[verb,...args],rc:1,stdout:'(error unknown (reason schedule))\n',stderr:''};

 const read=`(ok ((id . "a.1") (deleted . #f) (fields (heading-src . "# ${title}\\n") (src . "body\\n") (title . "${title}")) (position root . 0) (edges)))\n`;
 // NEVER: THE SUBTREE INCLUDES THE BLOCK IT IS UNDER. Measured against the
 // pinned core: `read <id> --recursive` answers with the block itself first
 // and then its descendants. This stand-in answered with the child alone,
 // which is a shape the core does not produce -- and the product now refuses
 // it, correctly, because an answer that never mentions the block is not an
 // answer about that block.
 const recursive=`((id . \"a.1\") (deleted . #f) (fields (heading-src . \"# ${title}\\n\") (src . \"body\\n\") (title . \"${title}\")) (position root . 0) (edges))\n((id . \"a.2\") (deleted . #f) (fields (heading-src . \"# ${title}\\n\") (src . \"body\\n\") (title . \"${title}\")) (position \"a.1\" . 0) (edges))\n`;
 const stdout=verb==='read'&&args.includes('--recursive')?recursive:verb==='conflicts'&&process.argv[3].includes('unknown')?'(error unavailable)\n':verb==='outline'?`- a.1  ${title}\n`:verb==='read'?read:verb==='check'?(process.argv[3].startsWith('integrity-show')?'(check (writers (("w" (end 0)))) (verdict damaged))\n':'(check (writers (("w" (end 0)))) (verdict ok))\n'):verb==='set'?'(error unknown (reason schedule))\n':'';
 return {argv:[verb,...args],rc:verb==='set'||verb==='conflicts'&&process.argv[3].includes('unknown')?1:0,stdout,stderr:''};}});
function files(dir){if(!fs.existsSync(dir))return [];return fs.readdirSync(dir).flatMap(n=>{const p=path.join(dir,n);return fs.statSync(p).isDirectory()?files(p):[p];});}
function tree(dir){return Object.fromEntries(files(dir).map(p=>[path.relative(dir,p),fs.readFileSync(p).toString('hex')]));}
function change(store){settings.store=store;configChanged({affectsConfiguration:()=>true});}
async function main(){
 const scenario=process.argv[3];
 // Queue item 33, C7: the integrity scenarios count unhandled rejections in this
 // process, from before activation. Only they do: every other scenario keeps Node's
 // default, which ends the process -- a tripwire in its own right.
 const unhandledSeen=new Set();
 if(scenario.startsWith('integrity-show'))process.on('unhandledRejection',(reason,promise)=>unhandledSeen.add(promise));
 await require(path.join(out,'extension.js')).activate({globalStorageUri:{fsPath:storage},subscriptions:[]});
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
 throw Error('Unknown schedule');
}
main().then(result=>{fs.writeSync(1,JSON.stringify({complete:true,result})+'\n');}).catch(error=>{fs.writeSync(2,String(error.stack)+'\n');process.exitCode=1;});
