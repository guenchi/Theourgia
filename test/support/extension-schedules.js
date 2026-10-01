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
// The banners the extension decorates editors with, and the status bar item it
// paints: what a person would see, recorded so a scenario can read it back.
// Where showTextDocument was asked to show something, and at which line.
const shownAt=[];
function sameUri(a,b){if(!a||!b)return false;if(a.fsPath!==undefined||b.fsPath!==undefined)return a.fsPath===b.fsPath;return a.scheme===b.scheme&&a.path===b.path&&a.query===b.query;}
// A document selected by a provider's selector, as VS Code selects one:
// scheme, language and a pattern's base, each only when the filter names it.
function selects(selector,doc){const filters=Array.isArray(selector)?selector:[selector];return filters.some(f=>(f.scheme===undefined||f.scheme===(doc.uri.scheme||'file'))&&(f.language===undefined||f.language===doc.languageId)&&(f.pattern===undefined||String(doc.uri.fsPath||'').startsWith(f.pattern.base)));}
const decorations=[];const definitionProviders=[];
// The content providers the extension registers, by scheme: what a read-only
// document's tab is given, asked as the editor asks it.
const contentProviders={};let commitSeq=0;
// The mode under interleaving and failure: q.1 and r.1 answer the mode a
// scenario sets; f.1's read is refused, g.1's request is rejected while
// rejectG is set, n.1 is gone while goneN is set; commits are taken while
// commitsTaken is set.
let qMode='text',rMode='text',rejectG=false,goneN=false,commitsTaken=false;
const MODE_RECORD=(id,mode)=>mode==='datum'
 ?`((id . "${id}") (deleted . #f) (fields (body define x 1) (kind . code) (lang . chez) (mode . datum) (name . x) (names x)) (position root . 0) (edges))`
 :`((id . "${id}") (deleted . #f) (fields (heading-src . "# Alpha\\n") (src . "body\\n") (title . "Alpha")) (position root . 0) (edges))`;
// A datum library and one definition in it, and the datum export the core
// writes for them (the layout measured on the pinned core: a `#!chezscheme`
// line, the header, the library's own lines, a marker per definition).
const DATUM_BLOCKS={
 'd.1':'((id . "d.1") (deleted . #f) (fields (exports alpha) (imports (rnrs)) (kind . library) (lang . chez) (mode . datum) (name probe d) (path . "probe.sls")) (position root . 0) (edges))',
 'd.2':'((id . "d.2") (deleted . #f) (fields (body define alpha 1) (doc . "") (kind . code) (lang . chez) (mode . datum) (name . alpha) (names alpha)) (position "d.1" . 0) (edges))',
 // For the mode at a write: another datum definition, a block whose mode
 // this build does not know, and a text section.
 'd.3':'((id . "d.3") (deleted . #f) (fields (body define beta 2) (doc . "") (kind . code) (lang . chez) (mode . datum) (name . beta) (names beta)) (position "d.1" . 1) (edges))',
 'o.1':'((id . "o.1") (deleted . #f) (fields (kind . code) (lang . chez) (mode . other) (src . "x")) (position root . 3) (edges))',
 'o.2':'((id . "o.2") (deleted . #f) (fields (kind . code) (lang . chez) (mode . other) (src . "y")) (position root . 5) (edges))',
 't.5':'((id . "t.5") (deleted . #f) (fields (heading-src . "# T\\n") (src . "body\\n") (title . "T")) (position root . 4) (edges))'};
const DATUM_FILE=`#!chezscheme\n;; @file ${Buffer.from('(code-projection 1 "s0000001" "d.1" (("w" . 2)) datum 0)').toString('hex')}\n(library (probe d)\n(export alpha)\n(import (rnrs))\n;; @block d.2\n(define alpha 1)\n)\n`;
// Documents a scenario shows as the editor holds them (a dirty draft, a view):
// openTextDocument answers these before reading a file.
const displayed=[];const statusItem={show(){},dispose(){}};
// Every document shown, with the language mode it was shown in.
const shownDocs=[];
// A real core's client, when a scenario builds one: the extension then talks to
// a real store through the shipping transport, and only the VS Code API here is
// the stand-in.
let realClient=null;
// Whether the incomplete-tree stand-in answers as a store missing a writer;
// turned off to show what a complete reading does to the marker.
let incompleteOn=true;
// The files view. The workspace's state as the extension keeps its view mode
// in it, per store; a store nobody chose a mode for answers `viewModeDefault`,
// which is Outline for every scenario but the files ones, so the scenarios
// written before the files view see the outline they were written against.
// The context keys set, the names typed into input boxes (and what each box
// asked), and the stores whose blocks carry paths.
const viewModes=new Map();let viewModeDefault='outline';const contexts={};const inputs=[],inputAsked=[];
const pathsOn=new Set(['/stores/A']);
// Each tree refresh the extension asked for. And the files view's store as
// state: each block's path and version per store, a `set` of the path taking
// a new version (refused as `changed` when it names an older one with
// --if-unchanged), and `storeSet` the same write made by somebody else.
const fired=[];const filesState=new Map();let filesCursor=40;
// A write of the workspace's state held until the scenario releases it, and
// writes of the path that the files store does not answer while it is down.
let updateGate=null,setsDown=false;
function filesOf(store){if(!filesState.has(store))filesState.set(store,{path:{'a.1':'docs/a.md','a.4':'src/x.scm'},version:{'a.1':'v1','a.2':'v1','a.3':'v1','a.4':'v1'}});return filesState.get(store);}
function storeSet(store,id,value){const st=filesOf(store);st.path[id]=value;st.version[id]='v'+(Number(st.version[id].slice(1))+1);filesCursor+=1;return filesCursor;}
const vs = {
 EventEmitter:class{constructor(){this.event=()=>disposable;}fire(){fired.push(1);}dispose(){}},
 TreeItem:class{constructor(label,collapsibleState){this.label=label;this.collapsibleState=collapsibleState;}},ThemeIcon:class{constructor(id){this.id=id;}},ThemeColor:class{},
 TreeItemCollapsibleState:{None:0,Collapsed:1},StatusBarAlignment:{Right:1},Uri:{file:p=>({fsPath:p}),from:o=>({...o,toString(){return `${o.scheme}:${o.path}?${o.query}`;}})},
 // Four numbers, or two positions, as the editor's own Range takes either.
 Range:class{constructor(a,b,c,d){if(typeof a==='object'){this.start=a;this.end=b;}else{this.start={line:a,character:b};this.end={line:c,character:d};}}},
 // The editor's definition dispatch: providers registered with their
 // selectors, and a document matched to them as VS Code matches one.
 languages:{setTextDocumentLanguage:async(d,language)=>Object.assign(d,{languageId:language}),registerDefinitionProvider:(selector,provider)=>{definitionProviders.push({selector,provider});return disposable;}},
 RelativePattern:class{constructor(base,pattern){this.base=base;this.pattern=pattern;}},
 Position:class{constructor(line,character){this.line=line;this.character=character;}},
 Location:class{constructor(uri,range){this.uri=uri;this.range=range;}},
 window:{createStatusBarItem:()=>statusItem,createTextEditorDecorationType:options=>({options,dispose(){}}),createOutputChannel:()=>({appendLine(line){outputLines.push(line);},show(){},dispose(){}}),registerTreeDataProvider:(n,p)=>{provider=p;return disposable;},
  // NEVER: THE CHANNEL IS KEPT. All three pushed the bare text, so routing an
  // error through showInformationMessage left every observation identical and
  // no cell could tell a warning from an alarm. The other editor double was
  // repaired for this in an earlier round; this one was missed because that
  // repair was made where the finding pointed. `messages` keeps the text so
  // the cells that read it are unchanged; `channels` is the new reading.
  showErrorMessage:m=>{channels.push('error');shown.push({text:m,level:'error'});return messages.push(m);},
  showWarningMessage:m=>{channels.push('warning');shown.push({text:m,level:'warning'});messages.push(m);if(process.argv[3]==='integrity-show-throws')throw new Error('the editor threw');return process.argv[3]==='integrity-show-rejects'?Promise.reject(new Error('the editor refused the warning')):Promise.resolve(undefined);},
  showInformationMessage:m=>{channels.push('information');shown.push({text:m,level:'information'});return messages.push(m);},
  showInputBox:async options=>{inputAsked.push(options);const next=inputs.shift();return typeof next==='function'?next(options):next;},
  showQuickPick:async items=>{if(!pickGate)throw Error('Unexpected picker');pickGate.enter(items);return pickGate.promise;},showTextDocument:async(d,options)=>{shownAt.push({fsPath:d&&d.fsPath!==undefined?d.fsPath:(d&&d.uri?d.uri.fsPath:null),line:options&&options.selection?options.selection.start.line:null});shownDocs.push({fsPath:d&&d.uri?d.uri.fsPath:null,languageId:d?d.languageId:null});return Object.assign(d,{setDecorations:(type,ranges)=>decorations.push({document:String(d.uri),before:type.options.before&&type.options.before.contentText,ranges})});}},
 workspace:{textDocuments:docs,getConfiguration:()=>({get:(k,f)=>settings[k]??f}),onDidSaveTextDocument:f=>{savedHandler=f;return disposable;},
  onDidChangeConfiguration:f=>{configChanged=f;return disposable;},
  registerTextDocumentContentProvider:(scheme,p)=>{contentProviders[scheme]=p;return disposable;},onDidCloseTextDocument:()=>disposable,
  // A document of a scheme a content provider serves is loaded from that
  // provider, as the editor loads one; a file is read from disk.
  openTextDocument:async uri=>{const shown=displayed.find(d=>sameUri(d.uri,uri));if(shown)return shown;
   if(uri.scheme!==undefined&&uri.scheme!=='file'&&contentProviders[uri.scheme]){const text=await contentProviders[uri.scheme].provideTextDocumentContent(uri);return {uri,isDirty:false,getText:()=>text};}
   return {uri,isDirty:false,getText:()=>fs.readFileSync(uri.fsPath,'utf8')};}},
 commands:{registerCommand:(n,f)=>{commands.set(n,f);return disposable;},
  // The editor's own dispatch for a definition request, over the providers
  // registered for the document; and a symbol provider that answers a
  // misleading line, which nothing that finds a definition may consult.
  executeCommand:async(id,...args)=>{
   if(id==='vscode.executeDefinitionProvider'){const [uri,position]=args;const doc=displayed.find(d=>sameUri(d.uri,uri));if(!doc)throw new Error('no displayed document for the definition request');const out=[];for(const {selector,provider} of definitionProviders){if(!selects(selector,doc))continue;const got=await provider.provideDefinition(doc,position,{});if(got)out.push(...(Array.isArray(got)?got:[got]));}return out;}
   if(id==='setContext'){contexts[args[0]]=args[1];return undefined;}
   if(id==='vscode.executeDocumentSymbolProvider')return [{name:'alpha',range:{start:{line:0,character:0},end:{line:0,character:0}}}];
   const f=commands.get(id);return f?f(...args):undefined;}}
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
Client.fromConfig=cfg=>realClient!==null?realClient:new Client({kind:'schedule',send:async(verb,args,input)=>{
 requests.push({store:cfg.store,verb,args,input:input??null});
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
 if(/^(queued|race|fail)-/.test(process.argv[3])){
  const ok=stdout=>({argv:[verb,...args],rc:0,stdout,stderr:''});
  if(verb==='read'&&args.length===1){
   if(args[0]==='q.1')return ok(`(ok ${MODE_RECORD('q.1',qMode)})\n`);
   if(args[0]==='r.1')return ok(`(ok ${MODE_RECORD('r.1',rMode)})\n`);
   if(args[0]==='f.1')return {argv:[verb,...args],rc:1,stdout:'(error unavailable (reason schedule))\n',stderr:''};
   if(args[0]==='g.1'){if(rejectG)throw new Error('the transport failed (schedule)');return ok(`(ok ${MODE_RECORD('g.1','text')})\n`);}
   if(args[0]==='n.1')return goneN?{argv:[verb,...args],rc:1,stdout:'(error unknown-id "n.1" (nearest))\n',stderr:''}:ok(`(ok ${MODE_RECORD('n.1','text')})\n`);
  }
  if(verb==='commit'&&commitsTaken){commitSeq+=1;return ok(`(ok (items (ok (events (("w" . ${commitSeq}))) (state ((${JSON.stringify(args[0])} . "h${commitSeq}"))) (cursor ("w" . ${commitSeq})) (replay #f))))\n`);}
 }
 // Queue item 39: in notice-behind a commit succeeds and says who else landed; in
 // notice-refused the store refuses it. Every other scenario keeps `unknown`.
 if(verb==='commit'&&process.argv[3]==='notice-behind') return {argv:[verb,...args],rc:0,stdout:'(ok (items (ok (events (("w" . 1))) (state (("a.1" . "hhh"))) (cursor ("w" . 1)) (replay #f))) (behind (("w" . 1) ("other" . 2))))\n',stderr:''};
 // A commit the store took while it could not read one writer: the answer
 // carries the clause after its own.
 if(verb==='commit'&&process.argv[3]==='incomplete-save') return {argv:[verb,...args],rc:0,stdout:'(ok (items (ok (events (("w" . 1))) (state (("a.1" . "hhh"))) (cursor ("w" . 1)) (replay #f))) (incomplete (unreadable (writer "zzzzzzzz") (path "/stores/A/writers/zzzzzzzz") (reason "Permission denied"))))\n',stderr:''};
 if(verb==='commit'&&process.argv[3]==='notice-refused') return {argv:[verb,...args],rc:1,stdout:'(error cursor-unreachable (after ("w" . 999)) (writing ("w" . 8)))\n',stderr:''};
 // The mode scenario: every commit is taken, so each save stands on its own.
 if(verb==='set'&&process.argv[3]==='datum-legacy'){commitSeq+=1;return {argv:[verb,...args],rc:0,stdout:`(ok (events (("w" . ${commitSeq}))) (state ((${JSON.stringify(args[0])} . "h${commitSeq}"))) (cursor ("w" . ${commitSeq})) (replay #f))\n`,stderr:''};}
 if(verb==='commit'&&process.argv[3]==='datum-legacy'){commitSeq+=1;return {argv:[verb,...args],rc:0,stdout:`(ok (items (ok (events (("w" . ${commitSeq}))) (state ((${JSON.stringify(args[0])} . "h${commitSeq}"))) (cursor ("w" . ${commitSeq})) (replay #f))))\n`,stderr:''};}
 if(verb==='commit') return {argv:[verb,...args],rc:1,stdout:'(error unknown (reason schedule))\n',stderr:''};
 // A store that could not read one writer: every answer carries the clause the
 // core appends, after the rows it did read.
 if(process.argv[3]==='incomplete-tree'&&incompleteOn){
  const clause='(incomplete (unreadable (writer "zzzzzzzz") (path "/stores/A/writers/zzzzzzzz") (reason "Permission denied")))';
  if(verb==='outline')return {argv:[verb,...args],rc:0,stdout:`(ok (text "- a.1  ${title}\\n") ${clause})\n`,stderr:''};
  if(verb==='conflicts')return {argv:[verb,...args],rc:0,stdout:`${clause}\n`,stderr:''};
  if(verb==='read'&&args.length===1)return {argv:[verb,...args],rc:0,stdout:`(ok ((id . "a.1") (deleted . #f) (fields (heading-src . "# ${title}\\n") (src . "body\\n") (title . "${title}")) (position root . 0) (edges)) ${clause})\n`,stderr:''};
 }
 // The files view: roots a.1 (a document) and a.3 (a section); under a.1 a
 // section a.2 holding a text file block a.4. Paths only in the stores in
 // `pathsOn`. Batches and writes of a path are taken, each a new record.
 if(process.argv[3].startsWith('files-')){
  const q=JSON.stringify,paths=pathsOn.has(cfg.store),ok=stdout=>({argv:[verb,...args],rc:0,stdout,stderr:''});
  const clause=process.argv[3]==='files-incomplete'?' (incomplete (unreadable (writer "zzzzzzzz") (path "/stores/A/writers/zzzzzzzz") (reason "Permission denied")))':'';
  const rec=(id,fields,pos)=>`((id . ${q(id)}) (deleted . #f) (fields ${fields}) (position ${pos}) (edges))`;
  const st=filesOf(cfg.store);
  const blocks={
   'a.1':rec('a.1',`(kind . doc)${paths?` (path . ${q(st.path['a.1'])})`:''} (title . ${q(title)})`,'root . 0'),
   'a.2':rec('a.2','(kind . section) (title . "Part")','"a.1" . 0'),
   'a.3':rec('a.3','(kind . section) (title . "Loose")','root . 1'),
   'a.4':rec('a.4',`(kind . file) (mode . text)${paths?` (path . ${q(st.path['a.4'])})`:''} (title . "x")`,'"a.2" . 0')};
  const under={'a.1':['a.1','a.2','a.4'],'a.2':['a.2','a.4'],'a.3':['a.3'],'a.4':['a.4']};
  if(verb==='outline')return ok(`(ok (text "- a.1  ${title}\\n- a.3  Loose\\n")${clause})\n`);
  if(verb==='conflicts')return ok(clause===''?'':`${clause.trim()}\n`);
  if(verb==='read'&&args.includes('--recursive')){const got=under[args[0]].map(id=>blocks[id]);return ok(args.includes('--wire')?`(ok (items ${got.join(' ')})${clause})\n`:`${got.join('\n')}\n`);}
  if(verb==='read')return ok(args.includes('--wire')?`(ok ${blocks[args[0]]} (version ${q(st.version[args[0]])}))\n`:`(ok ${blocks[args[0]]})\n`);
  if(verb==='batch')return ok('(batch ((ok (events (("w" . 40))) (state (("w.14" . "h"))) (cursor ("w" . 40)) (replay #f))) (done 1))\n');
  if(verb==='set'){
   if(setsDown)return {argv:[verb,...args],rc:1,stdout:'',stderr:'the store is not answering'};
   const guard=args.indexOf('--if-unchanged');
   if(guard>=0&&args[guard+1]!==st.version[args[0]])return {argv:[verb,...args],rc:1,stdout:`(error changed (current ${q(st.version[args[0]])}))\n`,stderr:''};
   const n=storeSet(cfg.store,args[0],args[2]);
   return ok(`(ok (events (("w" . ${n}))) (state ((${q(args[0])} . "h"))) (cursor ("w" . ${n})) (replay #f))\n`);
  }
 }
 // The datum view: a plain read of either datum block, and the datum export
 // written into the directory the extension names.
 if(process.argv[3].startsWith('datum-')){
  const ok=stdout=>({argv:[verb,...args],rc:0,stdout,stderr:''});
  if(verb==='read'&&args.length===1&&DATUM_BLOCKS[args[0]])return ok(`(ok ${DATUM_BLOCKS[args[0]]})\n`);
  if(verb==='export-code'&&args.includes('--datum')){fs.mkdirSync(args[0],{recursive:true});fs.writeFileSync(path.join(args[0],'probe.sls'),DATUM_FILE);return ok('(ok (files 1))\n');}
 }
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
 const stdout=verb==='read'&&args.includes('--recursive')?recursive:verb==='conflicts'&&process.argv[3].includes('unknown')?'(error unavailable)\n':verb==='outline'?(args.includes('--wire')?`(ok (text "- a.1  ${title}\\n"))\n`:`- a.1  ${title}\n`):verb==='read'?read:verb==='check'?(process.argv[3].startsWith('integrity-show')?'(check (writers (("w" (end 0)))) (verdict damaged))\n':'(check (writers (("w" (end 0)))) (verdict ok))\n'):verb==='set'?(process.argv[3]==='durability-order'?'(ok (events (("w" . 1))) (state (("a.9" . "hhh"))) (cursor ("w" . 1)) (replay #f))\n':'(error unknown (reason schedule))\n'):'';
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
 if(scenario.startsWith('files-'))viewModeDefault=undefined;
 await require(path.join(out,'extension.js')).activate({globalStorageUri:{fsPath:storage},subscriptions:[],
  workspaceState:{get:key=>viewModes.has(key)?viewModes.get(key):viewModeDefault,update:async(key,value)=>{viewModes.set(key,value);if(updateGate){const held=updateGate;updateGate=null;held.enter();await held.promise;}}}});
 // Queue item 39: one save of the opened block, and what the editor was shown.
 if(scenario==='notice-behind'||scenario==='notice-refused'||scenario==='incomplete-save'){
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
 // A listing from a store that could not read one writer, as the tree shows it:
 // each item's label, whether it opens anything, its icon, and what the window
 // raised meanwhile.
 const drawn=async()=>(await provider.getChildren()).map(n=>{const item=provider.getTreeItem(n);return {label:item.label,command:item.command?item.command.command:null,icon:item.iconPath?item.iconPath.id:null,contextValue:item.contextValue??null};});
 if(scenario==='incomplete-tree'){
  shown.length=0;
  const items=await drawn();
  const whileIncomplete={text:statusItem.text,tooltip:statusItem.tooltip};
  incompleteOn=false;
  const after=await drawn();
  return {items,shown:shown.slice(),status:whileIncomplete,after,statusAfter:{text:statusItem.text,tooltip:statusItem.tooltip}};
 }
 // The same through the REAL core: a real store with a second writer's
 // directory chmod 000, reached through the shipping transport and its daemon.
 // The editor host cannot read a tree item or a decoration back, so this
 // stand-in window is the only place those can be looked at.
 if(scenario==='incomplete-real'){
  const real=require(path.join(__dirname,'real-core.js'));
  const store=await real.RealStore.make();
  let locked=null;
  try{
   const made=await store.client.request('insert',['--under','root','--title','Alpha','--text','a needle here\n']);
   if(!made.ok)throw new Error(`the real store did not take a block: ${made.text}`);
   locked=path.join(store.store,'writers','zzzzzzzz');
   fs.mkdirSync(locked);fs.writeFileSync(path.join(locked,'000001.sexp'),'');fs.chmodSync(locked,0o000);
   realClient=new Client(store.transport());
   change(store.store);
   shown.length=0;
   const roots=await provider.getChildren();
   const items=roots.map(n=>{const item=provider.getTreeItem(n);return {label:item.label,command:item.command?item.command.command:null,icon:item.iconPath?item.iconPath.id:null};});
   const daemons=real.daemonsMatching(store.store).length;
   const runEntries=fs.readdirSync(store.runRoot).length;
   const block=roots.find(n=>n.incompleteText===undefined);
   if(block!==undefined)await commands.get('theourgia.openAsDocument')(block);
   return {local:store.env.THEOURGIA_LOCAL??null,daemons,runEntries,items,shown:shown.slice(),decorations:decorations.slice(),status:{text:statusItem.text}};
  }finally{
   if(locked!==null)fs.chmodSync(locked,0o755);
   realClient=null;
   store.dispose();
  }
 }
 // A REAL store holding text sources imported through the core's own
 // `import-code` (sent through the transport: this extension sends no import),
 // with the code blocks under each file, by the file's path.
 const importedStore=async(real,sources)=>{
  const {readBlock}=require(path.join(out,'blocks.js'));
  const store=await real.RealStore.make();
  const dir=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-bytes-'));
  for(const [name,bytes] of Object.entries(sources))fs.writeFileSync(path.join(dir,name),bytes);
  const transport=store.transport();
  const made=await transport.send('import-code',[dir]);
  if(made.rc!==0)throw new Error(`the sources were not imported: ${made.stdout}${made.stderr}`);
  const client=new Client(transport);
  const outline=await transport.send('outline',[]);
  const fileIds=outline.stdout.split('\n').map(l=>(/^- (\S+)/.exec(l)||[])[1]).filter(Boolean);
  const codeOf={};
  for(const id of fileIds){
   const r=await client.request('read',[id,'--recursive']);
   const blocks=r.answers.map(readBlock).filter(Boolean);
   const file=blocks.find(b=>b.id===id);
   const name=file?file.fields.get('path'):null;
   if(typeof name==='string')codeOf[path.basename(name)]=blocks.filter(b=>b.id!==id).map(b=>b.id);
  }
  return {store,dir,transport,client,codeOf};
 };
 const srcBytesOf=async(transport,id)=>{
  const {parseAnswers}=require(path.join(out,'wire.js'));const {readBlock}=require(path.join(out,'blocks.js'));
  const r=await transport.send('read',[id]);const form=parseAnswers(r.stdout)[0];const block=readBlock(form[1]);
  const src=block?block.fields.get('src'):null;return src instanceof Uint8Array?Buffer.from(src).toString('hex'):String(src);
 };
 // A text-mode block opened through the tree's command: the text it shows, in
 // the block's language; and a block whose bytes are not UTF-8, refused by name.
 // A datum block opened from the outline: a library, then a definition in it.
 // What was asked of the store, what the tab is given (and given again when
 // the editor restores it), where it opens, what was said, and what was
 // written to disk.
 if(scenario==='datum-open'){
  const datumDir=path.join(storage,'datum-view');
  // Every file outside the export's scratch, with its bytes: a file written
  // or rewritten by the open shows as a change.
  const outside=()=>Object.entries(tree(storage)).filter(([p])=>!p.startsWith('datum-view'+path.sep)).map(([p,hex])=>`${p} ${hex}`);
  const opened=[];
  for(const id of ['d.1','d.2']){
   const before=outside();requests.length=0;shown.length=0;
   const got=await commands.get('theourgia.openBlock')(id);
   const asked=requests.map(r=>[r.verb,...r.args.filter(a=>a.startsWith('--'))]);
   const views=contentProviders['theourgia-document'];
   const text=got?await views.provideTextDocumentContent(got.uri):null;
   const restoredUri=got?vs.Uri.from({scheme:got.uri.scheme,path:'/restored.sls',query:got.uri.query}):null;
   const restored=got?await views.provideTextDocumentContent(restoredUri):null;
   opened.push({id,asked,restoreAsked:requests.slice(asked.length).map(r=>r.verb),
    uri:got?{scheme:got.uri.scheme,path:got.uri.path,query:got.uri.query}:null,prefixLength:got?got.prefixLength:null,
    text,restored,shown:shown.slice(),written:outside().filter(p=>!before.includes(p)).map(p=>p.split(' ')[0]),
    line:(shownAt[shownAt.length-1]||{}).line??null,
    language:(shownDocs[shownDocs.length-1]||{}).languageId??null});
  }
  return {opened,file:DATUM_FILE,scratchLeft:fs.existsSync(datumDir)?fs.readdirSync(datumDir).length:0};
 }
 // The block's mode at the working write, recorded in a file's record. A text
 // file this build made saves with no read of its own; a file whose record has
 // no mode (as an earlier version made it) costs one read at its first save,
 // which the record then keeps; a datum block's file is refused before the
 // write is sent, and so is a block whose mode cannot be read; a
 // reconciliation of a datum block's file is refused the same way.
 if(scenario==='datum-legacy'){
  const metaOf=file=>JSON.parse(fs.readFileSync(file+'.meta','utf8'));
  const plainReads=from=>requests.slice(from).filter(r=>r.verb==='read'&&r.args.length===1).map(r=>r.args[0]);
  const verbsFrom=from=>requests.slice(from).map(r=>r.verb);
  const save=async(file,text)=>{fs.writeFileSync(file,text);const from=requests.length;shown.length=0;
   await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>text});
   return {reads:plainReads(from),verbs:verbsFrom(from),shown:shown.slice(),mode:metaOf(file).mode};};
  // A file as an earlier version made it: published from the working read,
  // with no mode in its record.
  const {Working}=require(path.join(out,'working.js'));
  const legacy=async(id,dirOfA)=>{
   const directory=path.join(path.dirname(dirOfA),id);
   const reading=await new Working(Client.fromConfig({...settings}),'window-legacy').read(id,'');
   const published=await core.publisher.publish({directory,storeId:'/stores/A',blockId:id,prefix:reading.prefix,
    text:reading.prefix+reading.body,cursor:null,projection:reading.source,expected:null});
   if(!published.published)throw new Error(`the legacy file for ${id} was not published: ${published.because}`);
   return {file:published.file,prefix:reading.prefix};
  };
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  const made={mode:metaOf(a).mode,first:await save(a,'# Alpha\nfirst A\n')};
  // The same file with its mode taken out of the record, as an older record reads.
  const record=metaOf(a);record.mode=null;fs.writeFileSync(a+'.meta',JSON.stringify(record,null,2)+'\n');
  const textLegacy={first:await save(a,'# Alpha\nsecond A\n')};
  textLegacy.second=await save(a,'# Alpha\nthird A\n');
  const d2=await legacy('d.2',path.dirname(a));
  const datumLegacy={before:metaOf(d2.file).mode??null,first:await save(d2.file,d2.prefix+'(define alpha 2)\n')};
  datumLegacy.second=await save(d2.file,d2.prefix+'(define alpha 3)\n');
  // The open of a datum block over a record of it that says text (written
  // before the block was datum): the read is datum, and the record says so.
  {const rec=metaOf(d2.file);rec.mode='text';fs.writeFileSync(d2.file+'.meta',JSON.stringify(rec,null,2)+'\n');}
  await commands.get('theourgia.openBlock')('d.2');
  datumLegacy.openedOver={mode:metaOf(d2.file).mode};
  const x9=await legacy('x.9',path.dirname(a));
  const unreadable={first:await save(x9.file,x9.prefix+'whatever\n')};
  const o1=await legacy('o.1',path.dirname(a));
  const unknownMode={first:await save(o1.file,o1.prefix+'whatever\n')};
  // A block of an unknown mode opened from the outline: its record starts
  // with no mode, and its first save asks the store and is refused.
  await commands.get('theourgia.openBlock')('o.2');
  const o2=files(storage).find(p=>path.basename(path.dirname(p))==='o.2'&&!p.endsWith('.meta')&&!p.includes(`${path.sep}.block-control`));
  const o2Prefix=o2?(core.publisher.sidecarOf(o2)||{prefix:''}).prefix:'';
  const firstOpen=o2?metaOf(o2).mode:'no file';
  // Its record made to say text, as one written before the block's mode
  // changed would; reopening reads the block and clears it.
  if(o2){const rec=metaOf(o2);rec.mode='text';fs.writeFileSync(o2+'.meta',JSON.stringify(rec,null,2)+'\n');}
  await commands.get('theourgia.openBlock')('o.2');
  unknownMode.opened={mode:firstOpen,reopened:o2?metaOf(o2).mode:'no file',save:o2?await save(o2,o2Prefix+'edited\n'):null};
  // A read that shows a block as not text clears every record of it at the
  // read, even when what follows is refused: o.2 reopened while its file is
  // dirty in the editor (the publication refuses), its record saying text.
  if(o2){const rec=metaOf(o2);rec.mode='text';fs.writeFileSync(o2+'.meta',JSON.stringify(rec,null,2)+'\n');}
  const dirty={uri:{fsPath:o2},isDirty:true,getText:()=>'unsaved'};docs.push(dirty);
  shown.length=0;await commands.get('theourgia.openBlock')('o.2');docs.splice(docs.indexOf(dirty),1);
  unknownMode.refusedReopen={mode:o2?metaOf(o2).mode:'no file',shown:shown.slice()};
  // A record with no projection, as a much earlier version wrote one.
  const unprojected=async id=>{const made=await legacy(id,path.dirname(a));const rec=metaOf(made.file);delete rec.projection;fs.writeFileSync(made.file+'.meta',JSON.stringify(rec,null,2)+'\n');return made;};
  const d3=await unprojected('d.3');
  const noProjectionDatum={first:await save(d3.file,d3.prefix+'(define beta 3)\n')};
  const t5=await unprojected('t.5');
  const noProjectionText={first:await save(t5.file,t5.prefix+'new body\n')};
  const d1=await legacy('d.1',path.dirname(a));
  fs.writeFileSync(d1.file,d1.prefix+'(library (probe d))\n');
  const from=requests.length;shown.length=0;
  await commands.get('theourgia.reconcileBlock')(d1.file);
  const reconciled={verbs:verbsFrom(from),shown:shown.slice(),mode:metaOf(d1.file).mode};
  // A reconciliation of o.1, whose record is made to say text while the
  // block's mode is one this build does not know: refused, nothing written,
  // and the record left with no mode.
  {const rec=metaOf(o1.file);rec.mode='text';fs.writeFileSync(o1.file+'.meta',JSON.stringify(rec,null,2)+'\n');}
  const from2=requests.length;shown.length=0;
  await commands.get('theourgia.reconcileBlock')(o1.file);
  reconciled.unknown={verbs:verbsFrom(from2),shown:shown.slice(),mode:metaOf(o1.file).mode};
  return {made,textLegacy,datumLegacy,unreadable,unknownMode,noProjectionDatum,noProjectionText,reconciled};
 }
 // A read attempt that does not show text clears every record of the block:
 // an open whose read is refused, an open of a block no longer in the store,
 // a reconciliation whose read is refused (each over a record saying text),
 // and the save gate's read rejected at the transport (a record with no mode).
 if(scenario==='fail-reads'){
  const metaOf=file=>JSON.parse(fs.readFileSync(file+'.meta','utf8'));
  const setMode=(file,mode)=>{const rec=metaOf(file);rec.mode=mode;fs.writeFileSync(file+'.meta',JSON.stringify(rec,null,2)+'\n');};
  const {Working}=require(path.join(out,'working.js'));
  await commands.get('theourgia.openBlock')('a.1');
  const a=files(storage).find(p=>p.endsWith('.md'));
  const made=async id=>{const directory=path.join(path.dirname(path.dirname(a)),id);
   const reading=await new Working(Client.fromConfig({...settings}),'window-legacy').read(id,'');
   const published=await core.publisher.publish({directory,storeId:'/stores/A',blockId:id,prefix:reading.prefix,
    text:reading.prefix+reading.body,cursor:null,projection:reading.source,expected:null});
   if(!published.published)throw new Error(`no file for ${id}: ${published.because}`);return {file:published.file,prefix:reading.prefix};};
  const f1=await made('f.1');setMode(f1.file,'text');
  shown.length=0;await commands.get('theourgia.openBlock')('f.1');
  const openRefused={mode:metaOf(f1.file).mode,shown:shown.slice()};
  const n1=await made('n.1');setMode(n1.file,'text');goneN=true;
  shown.length=0;await commands.get('theourgia.openBlock')('n.1');
  const openGone={mode:metaOf(n1.file).mode,shown:shown.slice()};
  setMode(f1.file,'text');
  shown.length=0;await commands.get('theourgia.reconcileBlock')(f1.file);
  const reconcileRefused={mode:metaOf(f1.file).mode,shown:shown.slice()};
  const g1=await made('g.1');rejectG=true;
  const from=requests.length;shown.length=0;const text=g1.prefix+'edited\n';fs.writeFileSync(g1.file,text);
  await savedHandler({uri:{fsPath:g1.file},isDirty:false,getText:()=>text});
  const gateRejected={verbs:requests.slice(from).map(r=>r.verb),shown:shown.slice(),mode:metaOf(g1.file).mode};
  return {openRefused,openGone,reconcileRefused,gateRejected};
 }
 // A save accepted while its block was text, queued behind an unresolved one;
 // then the block is read again, as datum (or, for the control, as text), and
 // the queue is retried with commits now taken.
 if(scenario==='queued-datum'||scenario==='queued-text'){
  await commands.get('theourgia.openBlock')('q.1');
  const q=files(storage).find(p=>path.basename(path.dirname(p))==='q.1'&&!p.endsWith('.meta')&&!p.includes(`${path.sep}.block-control`));
  const prefix=core.publisher.sidecarOf(q).prefix;
  for(const body of ['first\n','second\n']){const text=prefix+body;fs.writeFileSync(q,text);await savedHandler({uri:{fsPath:q},isDirty:false,getText:()=>text});}
  const queue=core.outboxPath('/stores/A');
  const queuedBefore=JSON.parse(fs.readFileSync(queue,'utf8')).entries.map(e=>e.state);
  if(scenario==='queued-datum')qMode='datum';
  await commands.get('theourgia.openBlock')('q.1');
  const modeAfterRead=JSON.parse(fs.readFileSync(q+'.meta','utf8')).mode;
  commitsTaken=true;
  const from=requests.length;
  await commands.get('theourgia.retryOutbox')();
  const after=JSON.parse(fs.readFileSync(queue,'utf8')).entries.map(e=>({state:e.state,lastError:e.lastError}));
  // The parked entry cleared (as a person would, by resolving it), and the
  // queue let go on: the entry that waited behind it meets the same check
  // at its own transmission.
  let cleared=null;
  if(scenario==='queued-datum'){
   const {Outbox}=require(path.join(out,'outbox.js')),q2=new Outbox(queue);q2.load();
   const first=q2.entries.find(e=>e.state==='parked');
   if(first)q2.resolve(first.req,null);
   const from2=requests.length;
   await commands.get('theourgia.retryOutbox')();
   cleared={sent:requests.slice(from2).map(r=>r.verb),after:JSON.parse(fs.readFileSync(queue,'utf8')).entries.map(e=>({state:e.state,lastError:e.lastError}))};
  }
  return {queuedBefore,modeAfterRead,sent:requests.slice(from).map(r=>r.verb),after,cleared};
 }
 // Two opens of one block: A reads it as text and is held between its reading
 // and its publication; B then reads it as datum (or, for the control, text).
 // The record A publishes says text only if no not-text read came between.
 if(scenario==='race-datum'||scenario==='race-text'){
  const held=gate();afterRead=held;
  const first=commands.get('theourgia.openBlock')('r.1');
  await held.entered;
  if(scenario==='race-datum')rMode='datum';
  const second=commands.get('theourgia.openBlock')('r.1');
  if(scenario==='race-datum')await second;
  else for(let i=0;i<20;i++)await new Promise(r=>setTimeout(r,10));
  held.release();await first;await second;
  const r=files(storage).find(p=>path.basename(path.dirname(p))==='r.1'&&!p.endsWith('.meta')&&!p.includes(`${path.sep}.block-control`));
  return {mode:r?JSON.parse(fs.readFileSync(r+'.meta','utf8')).mode:'no file'};
 }
 // A save's behind notice through the extension on a real core, after another
 // instance's segment is published into the store: the record left with no
 // mode, as 1.0.0 left its files (the save reads the block first), or with
 // text, as this release records it (no read). Every request of that save is
 // kept with the start of its answer, so a difference between the two can be
 // read off them.
 if(scenario==='behind-real-legacy'||scenario==='behind-real-text'){
  const real=require(path.join(__dirname,'real-core.js'));
  const store=await real.RealStore.make('behind-mode');
  const copy=path.join(store.root,'s2');
  try{
   const transport=store.transport();
   const idOf=async(title,text)=>{const made=await transport.send('insert',['--title',title,'--text',text]);
    const id=(/\(state \(\("([^"]+)"/.exec(made.stdout)||[])[1];if(!id)throw new Error(`no id for ${title}: ${made.stdout}`);return id;};
   const mine=await idOf('A','olda\n'),theirs=await idOf('B','oldb\n');
   const client=new Client(transport),log=[],request=client.request.bind(client);
   client.request=async(verb,args,input)=>{const answer=await request(verb,args,input);
    log.push({verb,args:(args||[]).map(a=>a.length>80?a.slice(0,80)+'...':a),ok:answer.ok,text:String(answer.text).slice(0,400)});return answer;};
   realClient=client;change(store.store);
   await commands.get('theourgia.openBlock')(mine);
   const file=files(storage).find(p=>path.basename(path.dirname(p))===mine&&!p.endsWith('.meta')&&!p.includes(`${path.sep}.block-control`));
   if(!file)throw new Error(`the block ${mine} was not opened into a file`);
   const prefix=core.publisher.sidecarOf(file).prefix;
   const saveText=async text=>{fs.writeFileSync(file,text);shown.length=0;await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>text});return shown.slice();};
   // One ordinary save first, while the store has one writer, so the queue
   // holds a cursor before a second instance exists (as T2 does, and why).
   const firstShown=await saveText(prefix+'edited here\n');
   fs.cpSync(store.store,copy,{recursive:true});
   const adopted=/\(to "([^"]+)"\)/.exec(store.cli(['adopt','--store',copy]));
   if(adopted===null)throw new Error('the copy was not adopted');
   const other=adopted[1];
   const wrote=/\(version "([^"]+)"\)/.exec(store.cli(['write',theirs,'from the copy','--store',copy,'--writer','w2']));
   if(wrote===null)throw new Error('the copy would not take a draft');
   store.cli(['commit',theirs,'--working-version',`${theirs}=${wrote[1]}`,'--store',copy,'--writer','w2']);
   const segments=fs.readdirSync(path.join(copy,'writers',other)).filter(n=>/^\d+\.sexp$/.test(n));
   if(segments.length!==1)throw new Error(`the copy's writer holds ${segments.length} segments`);
   const published=store.cli(['publish',other,'1',path.join(copy,'writers',other,segments[0]),'--store',store.store]);
   if(!/\(ok \(published 1\)\)/.test(published))throw new Error(`the segment was not published: ${published}`);
   const meta=JSON.parse(fs.readFileSync(file+'.meta','utf8'));
   if(scenario==='behind-real-legacy'){meta.mode=null;fs.writeFileSync(file+'.meta',JSON.stringify(meta,null,2)+'\n');}
   const modeBefore=JSON.parse(fs.readFileSync(file+'.meta','utf8')).mode;
   const from=log.length;
   const secondShown=await saveText(prefix+'edited again\n');
   return {mine,other,firstShown,modeBefore,second:{requests:log.slice(from),shown:secondShown},
    modeAfter:JSON.parse(fs.readFileSync(file+'.meta','utf8')).mode};
  }finally{realClient=null;store.dispose();fs.rmSync(copy,{recursive:true,force:true});}
 }
 // The same open against the real core: a library imported with --datum,
 // opened as the library and as its definition. Every request the client
 // sent is recorded, and the definition is read back afterwards.
 if(scenario==='datum-real'){
  const real=require(path.join(__dirname,'real-core.js'));
  const store=await real.RealStore.make();
  const dir=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-datum-'));
  const exportDir=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-datum-export-'));
  try{
   fs.writeFileSync(path.join(dir,'probe.sls'),'(library (probe d)\n  (export alpha)\n  (import (rnrs))\n  (define (alpha x) (+ x 1)))\n');
   const transport=store.transport();
   const made=await transport.send('import-code',[dir,'--datum']);
   if(made.rc!==0)throw new Error(`the library was not imported: ${made.stdout}${made.stderr}`);
   const outline=(await transport.send('outline',[])).stdout;
   const libId=(/^- (\S+)/m.exec(outline)||[])[1],defId=(/^  - (\S+)/m.exec(outline)||[])[1];
   if(!libId||!defId)throw new Error(`the outline holds no library and definition: ${outline}`);
   const exported=await transport.send('export-code',[exportDir,'--datum']);
   if(exported.rc!==0)throw new Error(`the datum export was refused: ${exported.stdout}${exported.stderr}`);
   const fileText=fs.readFileSync(path.join(exportDir,'probe.sls'),'utf8');
   const client=new Client(transport),sent=[],request=client.request.bind(client);
   client.request=async(verb,args,input)=>{sent.push([verb,...(args||[]).filter(a=>a.startsWith('--'))]);return request(verb,args,input);};
   realClient=client;change(store.store);
   const opened=[];
   for(const id of [libId,defId]){
    sent.length=0;shown.length=0;
    const got=await commands.get('theourgia.openBlock')(id);
    const text=got?await contentProviders['theourgia-document'].provideTextDocumentContent(got.uri):null;
    opened.push({id,sent:sent.slice(),text,prefixLength:got?got.prefixLength:null,shown:shown.slice(),line:(shownAt[shownAt.length-1]||{}).line??null});
   }
   // Go to definition on a call of the datum definition: the picker's
   // def is chosen, and the view opens at its line.
   vs.window.activeTextEditor={document:{uri:{fsPath:'/nowhere/use.ss'},languageId:'scheme',isDirty:false,getText:()=>'(alpha 1)\n'},selection:{active:{line:0,character:2}}};
   pickGate=gate();
   const running=commands.get('theourgia.goToDefinition')();
   const items=await Promise.race([pickGate.entered,running.then(()=>null)]);
   let definition=null;
   if(items!==null){const def=items.find(i=>i.record.kind==='def');pickGate.release(def);await running;
    const last=shownAt[shownAt.length-1]||null;definition={kinds:items.map(i=>i.record.kind),line:last?last.line:null};}
   pickGate=null;
   const after=(await transport.send('read',[defId,'--wire'])).stdout;
   return {libId,defId,fileText,opened,after,definition};
  }finally{realClient=null;store.dispose();fs.rmSync(dir,{recursive:true,force:true});fs.rmSync(exportDir,{recursive:true,force:true});}
 }
 if(scenario==='bytes-open'){
  const real=require(path.join(__dirname,'real-core.js'));
  const good=Buffer.from(';; \u4e2d\u6587\r\n(define alpha 1)\r\n','utf8');
  const bad=Buffer.from([0x28,0x64,0x65,0x66,0x69,0x6e,0x65,0x20,0x62,0x20,0x22,0xff,0x22,0x29,0x0a]);
  const imported=await importedStore(real,{'good.ss':good});
  try{
   // A text import skips a file whose bytes are not UTF-8 and lists it as
   // skipped, so the block that is not UTF-8 is written as other history can
   // hold one: one insert under the imported file, its src those bytes.
   const fileId=(await imported.transport.send('outline',[])).stdout.split('\n').map(l=>(/^- (\S+)/.exec(l)||[])[1]).filter(Boolean)[0];
   const made=await imported.transport.send('batch',[],`(insert ${JSON.stringify(fileId)} #f ((kind . code) (mode . text) (lang . scheme) (name . "b") (src . #vu8(${[...bad].join(' ')}))))`);
   const badId=(/\(state \(\("([^"]+)"/.exec(made.stdout)||[])[1];
   if(made.rc!==0||!badId)throw new Error(`the block that is not UTF-8 was not written: ${made.stdout}${made.stderr}`);
   imported.codeOf['bad.ss']=[badId];
   realClient=imported.client;
   change(imported.store.store);
   shown.length=0;
   await commands.get('theourgia.openBlock')(imported.codeOf['good.ss'][0]);
   const goodShown=shownDocs[shownDocs.length-1]||null;
   const goodFile=goodShown&&goodShown.fsPath?fs.readFileSync(goodShown.fsPath):null;
   const goodErrors=shown.filter(n=>n.level==='error');
   shown.length=0;
   const before=shownDocs.length;
   await commands.get('theourgia.openBlock')(imported.codeOf['bad.ss'][0]);
   return {codeOf:imported.codeOf,goodHex:good.toString('hex'),goodFileHex:goodFile?goodFile.toString('hex'):null,
    languageId:goodShown?goodShown.languageId:null,goodErrors,badShown:shown.slice(),badOpened:shownDocs.length-before};
  }finally{realClient=null;imported.store.dispose();fs.rmSync(imported.dir,{recursive:true,force:true});}
 }
 // The save round trip on the real core: a text-mode block opened, saved
 // unchanged and committed reads back as the same bytes and exports as the
 // same file; a buffer that begins with a byte-order mark is refused; a source
 // imported WITH a mark re-saves and exports with it.
 if(scenario==='bytes-save'){
  const real=require(path.join(__dirname,'real-core.js'));
  const src=Buffer.from(';; one\n(define alpha 1)\n','utf8');
  const bom=Buffer.concat([Buffer.from([0xef,0xbb,0xbf]),Buffer.from(';; bom\n(define beta 2)\n','utf8')]);
  const imported=await importedStore(real,{'alpha.ss':src,'bom.ss':bom});
  const exportDir=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-bytes-export-'));
  try{
   realClient=imported.client;
   change(imported.store.store);
   const alphaId=imported.codeOf['alpha.ss'][0];
   await commands.get('theourgia.openBlock')(alphaId);
   const file=shownDocs[shownDocs.length-1].fsPath;
   const text=fs.readFileSync(file,'utf8');
   shown.length=0;
   await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>text});
   const afterSave=shown.slice();
   const rereadHex=await srcBytesOf(imported.transport,alphaId);
   const bomId=imported.codeOf['bom.ss'][0];
   await commands.get('theourgia.openBlock')(bomId);
   const bomFile=shownDocs[shownDocs.length-1].fsPath;
   const bomText=fs.readFileSync(bomFile,'utf8');
   shown.length=0;
   await savedHandler({uri:{fsPath:bomFile},isDirty:false,getText:()=>bomText});
   const afterBomSave=shown.slice();
   const ex=await imported.transport.send('export-code',[exportDir,'--raw']);
   const exportedHex=ex.rc===0&&fs.existsSync(path.join(exportDir,'alpha.ss'))?fs.readFileSync(path.join(exportDir,'alpha.ss')).toString('hex'):`export rc ${ex.rc}: ${ex.stdout}${ex.stderr}`;
   const bomExportedHex=ex.rc===0&&fs.existsSync(path.join(exportDir,'bom.ss'))?fs.readFileSync(path.join(exportDir,'bom.ss')).toString('hex'):null;
   fs.writeFileSync(file,Buffer.concat([Buffer.from([0xef,0xbb,0xbf]),Buffer.from(text,'utf8')]));
   shown.length=0;
   await savedHandler({uri:{fsPath:file},isDirty:false,getText:()=>'\ufeff'+text});
   const markRefusal=shown.slice();
   return {srcHex:src.toString('hex'),rereadHex,exportedHex,afterSave,bomHex:bom.toString('hex'),bomFileStartsWithMark:bomText.charCodeAt(0)===0xfeff,
    afterBomSave,bomExportedHex,markRefusal};
  }finally{realClient=null;imported.store.dispose();fs.rmSync(imported.dir,{recursive:true,force:true});fs.rmSync(exportDir,{recursive:true,force:true});}
 }
 // A REAL store holding a datum library that exports `alpha` and a text block
 // defining `alpha` on its third line, both imported through the core's own
 // `import-code` (sent through the transport: this extension sends no import).
 const definitionStore=async real=>{
  const store=await real.RealStore.make();
  const lib=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-def-lib-'));
  fs.writeFileSync(path.join(lib,'probe.sls'),'(library (probe d)\n  (export alpha)\n  (import (rnrs)))\n');
  const text=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-def-text-'));
  fs.writeFileSync(path.join(text,'alpha.ss'),';; one\n;; two\n(define alpha 1)\n');
  const transport=store.transport();
  const a=await transport.send('import-code',[lib,'--datum']);
  if(a.rc!==0)throw new Error(`the library was not imported: ${a.stdout}${a.stderr}`);
  const b=await transport.send('import-code',[text]);
  if(b.rc!==0)throw new Error(`the text source was not imported: ${b.stdout}${b.stderr}`);
  return {store,dirs:[lib,text]};
 };
 // Go to definition as a command: the picker's rows, and where the chosen
 // definition opened, with the file it opened.
 if(scenario==='definition-command'){
  const real=require(path.join(__dirname,'real-core.js'));
  const {store,dirs}=await definitionStore(real);
  try{
   realClient=new Client(store.transport());
   change(store.store);
   vs.window.activeTextEditor={document:{uri:{fsPath:'/nowhere/use.md'},languageId:'markdown',isDirty:false,getText:()=>'(display alpha)\n'},selection:{active:{line:0,character:10}}};
   pickGate=gate();
   // NEVER: WAIT ON THE PICKER ALONE. A command that fails before it asks
   // leaves the picker unopened; waiting on it then ends the process with
   // the store's daemon still running, because the finally below never runs.
   const running=commands.get('theourgia.goToDefinition')();
   const items=await Promise.race([pickGate.entered,running.then(()=>null)]);
   if(items===null){pickGate=null;return {labels:[],kinds:[],opened:null,fileText:null,shown:shown.slice()};}
   const def=items.find(i=>i.record.kind==='def');
   pickGate.release(def);
   await running;
   pickGate=null;
   const last=shownAt[shownAt.length-1]||null;
   return {labels:items.map(i=>i.label),kinds:items.map(i=>i.record.kind),opened:last,fileText:last&&last.fsPath?fs.readFileSync(last.fsPath,'utf8'):null,shown:shown.slice()};
  }finally{realClient=null;store.dispose();for(const d of dirs)fs.rmSync(d,{recursive:true,force:true});}
 }
 // The editor's own Go to Definition, through its dispatch, from a block's
 // file, from a document view, and from the block's file with a dirty draft
 // one line longer than the store's.
 if(scenario==='definition-provider'){
  const real=require(path.join(__dirname,'real-core.js'));
  const {store,dirs}=await definitionStore(real);
  try{
   realClient=new Client(store.transport());
   change(store.store);
   const found=await realClient.request('whereis',['alpha','--wire']);
   const defId=found.answers.filter(i=>Array.isArray(i)&&i[0]&&i[0].name==='def').map(i=>i[1])[0];
   const opened=await commands.get('theourgia.openBlock')(defId);
   const file=opened.uri.fsPath;
   const fileText=fs.readFileSync(file,'utf8');
   const where=text=>{const lines=text.split('\n');const line=lines.findIndex(l=>l.includes('(define alpha'));return {line,character:lines[line].indexOf('alpha')+1};};
   const ask=async(doc,at)=>(await vs.commands.executeCommand('vscode.executeDefinitionProvider',doc.uri,new vs.Position(at.line,at.character))).map(l=>({fsPath:l.uri.fsPath,line:l.range.line}));
   const block={uri:{fsPath:file},languageId:'markdown',isDirty:false,getText:()=>fileText};
   displayed.push(block);
   const inBlock=await ask(block,where(fileText));
   displayed.length=0;
   const view={uri:{scheme:'theourgia-document',path:'/alpha.md',query:'q'},languageId:'markdown',isDirty:false,getText:()=>'# Alpha\n(display alpha)\n'};
   displayed.push(view);
   const inView=await ask(view,{line:1,character:10});
   displayed.length=0;
   const dirtyText=fileText.slice(0,opened.prefixLength)+';; a line of the draft\n'+fileText.slice(opened.prefixLength);
   const dirty={uri:{fsPath:file},languageId:'markdown',isDirty:true,getText:()=>dirtyText};
   displayed.push(dirty);docs.push(dirty);
   const inDraft=await ask(dirty,where(dirtyText));
   displayed.length=0;docs.length=0;
   return {file,defLine:where(fileText).line,inBlock,inView,inDraft,providers:definitionProviders.length};
  }finally{realClient=null;store.dispose();for(const d of dirs)fs.rmSync(d,{recursive:true,force:true});}
 }
 // The files view with the stand-in core: the rows a store opens with, the
 // mode each store keeps and the context key the title bar reads, a file's
 // children, and the three actions with the requests they sent and what they
 // said.
 const row=n=>{const item=provider.getTreeItem(n);return {id:item.id??null,label:item.label,command:item.command?item.command.command:null,contextValue:item.contextValue??null,description:item.description??null};};
 const modeOf=store=>viewModes.get(`theourgia.viewMode:${store}`)??null;
 if(scenario==='files-incomplete'){
  return {rows:(await provider.getChildren()).map(row)};
 }
 // A listing of an undecided store held at its first subtree read while the
 // person chooses Outline: the mode chosen meanwhile is the mode, and the
 // listing's own default is not remembered over it.
 if(scenario==='files-race'){
  verbGates.read=Object.assign(gate(),{match:args=>args.includes('--recursive')&&args.includes('--wire')});
  const held=verbGates.read;
  const pending=provider.getChildren();
  await held.entered;
  await commands.get('theourgia.showOutline')();
  const chosen=modeOf('/stores/A');
  held.release();
  const rows=(await pending).map(row);
  return {chosen,rows,remembered:modeOf('/stores/A'),context:contexts['theourgia.viewMode']??null};
 }
 // A listing of a store kept in Outline held at its outline read while the
 // person chooses Files: the mode chosen meanwhile is the mode in this branch
 // too, and the listing is made again in it.
 if(scenario==='files-race-outline'){
  viewModes.set('theourgia.viewMode:/stores/A','outline');
  verbGates.outline=gate();
  const held=verbGates.outline;
  const pending=provider.getChildren();
  await held.entered;
  await commands.get('theourgia.showFiles')();
  held.release();
  const rows=(await pending).map(row);
  return {rows,remembered:modeOf('/stores/A'),context:contexts['theourgia.viewMode']??null};
 }
 // A mode chosen while the listing of an undecided store is remembering the
 // mode it decided: the mode chosen is the mode, and the listing is made again.
 if(scenario==='files-race-remember'){
  updateGate=gate();
  const held=updateGate;
  const pending=provider.getChildren();
  await held.entered;
  await commands.get('theourgia.showOutline')();
  held.release();
  const rows=(await pending).map(row);
  return {rows:rows.map(x=>x.id),remembered:modeOf('/stores/A'),context:contexts['theourgia.viewMode']??null};
 }
 // A move of a.4 to lib made in the current store while its writes are not
 // answered, so the move is kept in the queue.
 const keptMove=async()=>{
  const src=await provider.getChildren((await provider.getChildren()).find(e=>e.directory&&e.directory.name==='src'));
  setsDown=true;inputs.push('lib');await commands.get('theourgia.moveToDirectory')(src[0]);setsDown=false;
 };
 // A kept move drained in front of another save, a new document: the store
 // refuses it as changed, and the window says so though nobody asked for it.
 if(scenario==='files-drain-changed'){
  await keptMove();
  storeSet('/stores/A','a.4','other/q.scm');
  shown.length=0;requests.length=0;
  const docs=(await provider.getChildren()).find(e=>e.directory&&e.directory.name==='docs');
  inputs.push('n2');await commands.get('theourgia.newFileHere')(docs);
  return {shown:shown.slice(),verbs:requests.filter(r=>r.verb==='set'||r.verb==='batch').map(r=>r.verb),path:filesOf('/stores/A').path['a.4']};
 }
 // The retry command's drain held at the kept move's send while the settings
 // change: to the same store (the notice is said and the view listed again),
 // and to another store (the notice is said; the view shown is not that
 // store's, so it is not listed again).
 if(scenario==='files-retry-rebuilt'){
  const retryHeld=async(other)=>{
   verbGates.set=gate();const held=verbGates.set;shown.length=0;
   const retrying=commands.get('theourgia.retryOutbox')();
   await held.entered;change(other);const firedAt=fired.length;held.release();await retrying;
   return {shown:shown.filter(n=>/changed since it was listed/.test(n.text)),refreshed:fired.length>firedAt};
  };
  await keptMove();
  storeSet('/stores/A','a.4','other/q.scm');
  const sameStore=await retryHeld('/stores/A');
  pathsOn.add('/stores/C');change('/stores/C');
  await keptMove();
  storeSet('/stores/C','a.4','other/q.scm');
  const otherStore=await retryHeld('/stores/A');
  return {sameStore,otherStore,pathA:filesOf('/stores/A').path['a.4'],pathC:filesOf('/stores/C').path['a.4']};
 }
 // A move queued while the store did not answer, drained by the retry
 // command after somebody else changed the path: the store refuses it as
 // changed, and the window says so and lists the view again.
 if(scenario==='files-retry-changed'){
  const src=await provider.getChildren((await provider.getChildren()).find(e=>e.directory&&e.directory.name==='src'));
  setsDown=true;requests.length=0;shown.length=0;
  inputs.push('lib');await commands.get('theourgia.moveToDirectory')(src[0]);
  const queued=shown.map(n=>n.level);
  storeSet('/stores/A','a.4','other/q.scm');
  setsDown=false;shown.length=0;
  const firedBefore=fired.length;
  await commands.get('theourgia.retryOutbox')();
  return {queued,retried:shown.slice(),refreshed:fired.length>firedBefore,
   sets:requests.filter(r=>r.verb==='set').map(r=>r.args.slice(r.args.indexOf('--if-unchanged'))),path:filesOf('/stores/A').path['a.4']};
 }
 // The store switched while a move's fresh read is on its way, and
 // while its prompt is open. Nothing is sent to either store.
 if(scenario==='files-switch'){
  const srcOf=async()=>provider.getChildren((await provider.getChildren()).find(e=>e.directory&&e.directory.name==='src'));
  const first=await srcOf();
  requests.length=0;shown.length=0;inputAsked.length=0;
  verbGates.read=Object.assign(gate(),{match:args=>args[0]==='a.4'&&args.includes('--wire')&&!args.includes('--recursive')});
  const held=verbGates.read;
  inputs.push('lib');
  const moving=commands.get('theourgia.moveToDirectory')(first[0]);
  await held.entered;change('/stores/B');held.release();await moving;
  const duringRead={sets:requests.filter(r=>r.verb==='set').map(r=>r.store),shown:shown.slice(),asked:inputAsked.length};
  inputs.length=0;
  change('/stores/A');
  const again=await srcOf();
  requests.length=0;shown.length=0;inputAsked.length=0;
  inputs.push(()=>{change('/stores/B');return 'lib';});
  await commands.get('theourgia.moveToDirectory')(again[0]);
  const duringPrompt={sets:requests.filter(r=>r.verb==='set').map(r=>r.store),shown:shown.slice(),asked:inputAsked.length};
  return {duringRead,duringPrompt,pathA:filesOf('/stores/A').path['a.4'],pathB:filesOf('/stores/B').path['a.4']};
 }
 if(scenario==='files-mode'){
  const first=await provider.getChildren();
  const dirOf=name=>first.find(e=>e.directory&&e.directory.name===name);
  const docsEls=await provider.getChildren(dirOf('docs')),srcEls=await provider.getChildren(dirOf('src'));
  const group=first.find(e=>e.pathless!==undefined);
  const A={rows:first.map(row),docs:docsEls.map(row),src:srcEls.map(row),group:group?(await provider.getChildren(group)).map(row):[],
   fileChildren:docsEls.length>0?(await provider.getChildren(docsEls[0])).map(n=>n.id):[],remembered:modeOf('/stores/A'),context:contexts['theourgia.viewMode']??null};
  requests.length=0;shown.length=0;inputAsked.length=0;
  inputs.push('n');await commands.get('theourgia.newFileHere')(dirOf('docs'));
  inputs.push('lib');await commands.get('theourgia.moveToDirectory')(srcEls[0]);
  // The row kept from before the move renames the block as it is
  // now -- the prompt shows lib/x.scm -- with the version the move made.
  inputs.push('y.scm');await commands.get('theourgia.renameFile')(srcEls[0]);
  const writes=()=>requests.filter(r=>['batch','set','move','insert'].includes(r.verb)).map(r=>({verb:r.verb,args:r.args,input:r.input}));
  const actions={requests:writes(),shown:shown.slice(),asked:inputAsked.map(o=>[o.prompt,o.value??null])};
  // The src directory row from the first listing, opened again after
  // the move: the row it draws still says src, and a move from it starts
  // from lib/y.scm.
  requests.length=0;shown.length=0;inputAsked.length=0;
  const reopened=await provider.getChildren(dirOf('src'));
  inputs.push('deep');await commands.get('theourgia.moveToDirectory')(reopened[0]);
  const oldDirectory={drawn:reopened.map(row),requests:writes(),shown:shown.slice(),asked:inputAsked.map(o=>[o.prompt,o.value??null])};
  // Somebody else writes the path while the prompt is open. The
  // write carries the version read before the prompt; the store refuses it
  // as changed, the notice says so and the view is refreshed.
  requests.length=0;shown.length=0;inputAsked.length=0;
  const firedBefore=fired.length;
  inputs.push(()=>{storeSet('/stores/A','a.4','other/q.scm');return 'z.scm';});
  await commands.get('theourgia.renameFile')(srcEls[0]);
  const raced={requests:writes(),shown:shown.slice(),refreshed:fired.length>firedBefore,path:filesOf('/stores/A').path['a.4'],version:filesOf('/stores/A').version['a.4']};
  // A settings change that keeps the store: rows listed before it
  // still act, reading the block afresh.
  change('/stores/A');
  requests.length=0;shown.length=0;inputAsked.length=0;
  inputs.push('w.scm');await commands.get('theourgia.renameFile')(srcEls[0]);
  const sameStore={requests:writes(),shown:shown.slice(),asked:inputAsked.map(o=>[o.prompt,o.value??null])};
  const docsOfA=dirOf('docs');
  change('/stores/B');
  const B={first:(await provider.getChildren()).map(row),firstRemembered:modeOf('/stores/B'),firstContext:contexts['theourgia.viewMode']??null};
  // Rows kept from store A, used after store B was listed: a
  // directory, a file node expanded, a file node moved, a file node opened
  // through its own command. Each is refused, and nothing names A's ids to B.
  requests.length=0;shown.length=0;inputAsked.length=0;
  inputs.push('stale');await commands.get('theourgia.newFileHere')(docsOfA);
  const docsChildren=(await provider.getChildren(docsOfA)).length;
  const nodeChildren=(await provider.getChildren(docsEls[0])).length;
  inputs.push('stale');await commands.get('theourgia.moveToDirectory')(srcEls[0]);
  const opened=provider.getTreeItem(docsEls[0]).command.arguments;
  await commands.get('theourgia.openBlock')(...opened);
  B.staleRow={sent:requests.filter(r=>r.verb==='batch'||r.verb==='set').length,
   askedOfB:requests.filter(r=>r.store==='/stores/B'&&r.verb!=='check').map(r=>[r.verb,r.args[0]]),
   shown:shown.slice(),docsChildren,nodeChildren,asked:inputAsked.length,opened};
  inputs.length=0;
  pathsOn.add('/stores/B');
  B.afterPath=(await provider.getChildren()).map(row);B.afterPathRemembered=modeOf('/stores/B');
  await commands.get('theourgia.showFiles')();
  B.toggled=(await provider.getChildren()).map(row);B.toggledRemembered=modeOf('/stores/B');B.toggledContext=contexts['theourgia.viewMode']??null;
  change('/stores/A');
  A.back=(await provider.getChildren()).map(row);
  await commands.get('theourgia.showOutline')();
  A.outlined=(await provider.getChildren()).map(row);A.outlinedRemembered=modeOf('/stores/A');A.outlinedContext=contexts['theourgia.viewMode']??null;
  // Outline nodes of store A, and a child expanded from one, used after
  // store B was listed: refused, and nothing named to B.
  const outlinedA=await provider.getChildren();
  const rootA=outlinedA.find(n=>n.id==='a.1');
  const childA=rootA?await provider.getChildren(rootA):[];
  change('/stores/B');
  await provider.getChildren();
  requests.length=0;shown.length=0;
  const outlineRoot=rootA?(await provider.getChildren(rootA)).length:null;
  const outlineChild=childA.length>0?(await provider.getChildren(childA[0])).length:null;
  const outlineStale={childIds:childA.map(n=>n.id),outlineRoot,outlineChild,
   askedOfB:requests.filter(r=>r.store==='/stores/B'&&r.verb!=='check').map(r=>[r.verb,r.args[0]]),shown:shown.slice()};
  return {A,B,actions,oldDirectory,raced,sameStore,outlineStale};
 }
 // The files view on a REAL store: documents, a text file moved under a
 // section, and a datum library, compared with what export-md, export-code
 // and export-code --datum write into one directory; a file node's children
 // against the outline's; a document at a path another holds; and a nested
 // document with a path, written into the log the way the core's own history
 // cells write one, since the write path refuses to make it.
 if(scenario==='files-real'){
  const real=require(path.join(__dirname,'real-core.js'));
  const {environmentFor}=require(path.join(out,'config.js')),{coreDirectoryAt}=require(path.join(out,'fsops.js'));
  const {StoreModel}=require(path.join(out,'model.js'));
  const store=await real.RealStore.make();
  const dirs=[];const made=prefix=>{const d=fs.mkdtempSync(path.join(os.tmpdir(),prefix));dirs.push(d);return d;};
  const walk=(dir,rel='')=>fs.readdirSync(path.join(dir,rel)).flatMap(n=>{const r=rel===''?n:`${rel}/${n}`;return fs.statSync(path.join(dir,r)).isDirectory()?[`${r}/`,...walk(dir,r)]:[r];});
  const errors=[];
  try{
   const transport=store.transport();
   const send=async(verb,args)=>{const r=await transport.send(verb,args);if(r.rc!==0)throw new Error(`${verb} ${args.join(' ')} answered ${r.rc}: ${r.stdout}${r.stderr}`);return r;};
   const md=made('theourgia-files-md-');fs.mkdirSync(path.join(md,'docs','b'),{recursive:true});
   fs.writeFileSync(path.join(md,'docs','a.md'),'# A\n\nalpha\n\n## Part one\n\nmore\n\n## Part two\n\nlast\n');
   fs.writeFileSync(path.join(md,'docs','b','c.md'),'# C\n\ngamma\n');
   fs.writeFileSync(path.join(md,'readme.md'),'# Readme\n\nFIRSTMARKER\n');
   fs.writeFileSync(path.join(md,'other.md'),'# Other\n\nSECONDMARKER\n');
   await send('import-md',[md]);
   const code=made('theourgia-files-code-');fs.mkdirSync(path.join(code,'src'));
   fs.writeFileSync(path.join(code,'src','x.scm'),';; x\n(define x 1)\n');
   await send('import-code',[code]);
   const lib=made('theourgia-files-lib-');
   fs.writeFileSync(path.join(lib,'probe.sls'),'(library (probe d)\n  (export alpha)\n  (import (rnrs))\n  (define alpha 1))\n');
   await send('import-code',[lib,'--datum']);
   realClient=new Client(transport);
   const model=new StoreModel(realClient);
   const blocksNow=async()=>(await model.filesReading()).blocks;
   const pathOf=b=>{const p=b.block.fields.get('path');return typeof p==='string'?p:null;};
   const kindOf=b=>{const k=b.block.fields.get('kind');return k&&k.name?k.name:null;};
   const x=(await blocksNow()).find(b=>kindOf(b)==='file'&&(pathOf(b)||'').endsWith('x.scm'));
   if(!x)throw new Error('the imported text file has no block with its path');
   const holder=await send('insert',['--title','Holder','--text','holder\n']);
   const holderId=(/\(state \(\("([^"]+)"/.exec(holder.stdout)||[])[1];
   if(!holderId)throw new Error(`the section's id was not in the answer: ${holder.stdout}`);
   await send('move',[x.block.id,holderId]);
   const exported=made('theourgia-files-export-');
   await send('export-md',[exported]);
   await send('export-code',[exported]);
   await send('export-code',[exported,'--datum']);
   change(store.store);
   const listed=[];
   const gather=async els=>{for(const e of els){if(e.directory){listed.push(`${e.directory.prefix}/`);await gather(await provider.getChildren(e));}else if(e.file)listed.push(e.file.path);}};
   const roots=await provider.getChildren();
   await gather(roots);
   const onDisk=walk(exported);
   const files=list=>list.filter(p=>!p.endsWith('/')).sort(),folders=list=>list.filter(p=>p.endsWith('/')).sort();
   // The document with the most children, as a file node and as a block of the outline.
   const all=await blocksNow();
   const docA=all.find(b=>kindOf(b)==='doc'&&pathOf(b)==='docs/a.md');
   const find=async(els,id)=>{for(const e of els){if(e.file&&e.id===id)return e;if(e.directory){const f=await find(await provider.getChildren(e),id);if(f)return f;}}return null;};
   const fileNode=docA?await find(roots,docA.block.id):null;
   const childrenOf=async el=>(await provider.getChildren(el)).map(n=>[n.id,n.title,n.marks]);
   const fileChildren=fileNode?await childrenOf(fileNode):[];
   // One level further, from the file node's first child: the imported
   // document's heading tree, known from the fixture's own text.
   const firstChild=fileNode?(await provider.getChildren(fileNode))[0]:undefined;
   const grandChildren=firstChild?(await provider.getChildren(firstChild)).map(n=>n.title):[];
   await commands.get('theourgia.showOutline')();
   const outlineRoot=(await provider.getChildren()).find(n=>docA&&n.id===docA.block.id);
   const outlineChildren=outlineRoot?await childrenOf(outlineRoot):[];
   // A second document at the path the first holds.
   const readme=all.find(b=>pathOf(b)==='readme.md'),other=all.find(b=>pathOf(b)==='other.md');
   if(!readme||!other)throw new Error('the two documents were not imported at their paths');
   await send('set',[other.block.id,'path','readme.md']);
   await commands.get('theourgia.showFiles')();
   const shared=(await provider.getChildren()).filter(e=>e.file&&e.file.path==='readme.md');
   const byId=[readme.block.id,other.block.id].sort();
   const noteOf=id=>{const e=shared.find(f=>f.id===id);return e?e.file.note:'absent';};
   const again=made('theourgia-files-export-');
   await send('export-md',[again]);
   const first=byId[0]===readme.block.id?'FIRSTMARKER':'SECONDMARKER';
   const duplicate={notes:byId.map(noteOf),firstMarker:first,exportedText:fs.existsSync(path.join(again,'readme.md'))?fs.readFileSync(path.join(again,'readme.md'),'utf8'):'(no readme.md)'};
   // The nested document: a record appended to the log directly, under a
   // section of docs/a.md, as the core's history cells do.
   const partId=(await provider.getChildren(fileNode||{id:docA.block.id})).map(n=>n.id)[0];
   const script=path.join(made('theourgia-files-nested-'),'nested.scm');
   fs.writeFileSync(script,[
    '(import (chezscheme) (only (theourgia reduce) block-id) (theourgia log))',
    `(define store ${JSON.stringify(store.store)})`,
    '(let* ((sess (log-begin store (lambda args (quote applied)))) (v (session-view sess)))',
    '  (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v) (view-expect-seq v) "files-view-fixture" (quote ())',
    `    (list (quote put) (list (cons (quote kind) (quote doc)) (cons (quote path) "nested.md") (cons (quote title) "Nested") (cons (quote src) "NESTEDMARKER\\n") (cons (quote parent) ${JSON.stringify(partId)}) (cons (quote ord) 0)))))`,
    '  (session-commit! sess)',
    '  (log-end! sess)',
    '  (display (block-id (view-writer v) (view-expect-seq v))))',
    ''].join('\n'));
   const {execFileSync}=require('child_process');
   let nestedId=null;
   try{nestedId=execFileSync(store.config.scheme,['--script',script],{env:environmentFor(store.config,store.env,coreDirectoryAt(store.config.corePath)),encoding:'utf8'}).trim();}
   catch(e){errors.push(`the nested fixture did not run: ${e.message}`);}
   // Whether the record reads back; and, when it does not, whether it does
   // once the store's daemon is stopped and the next request starts one that
   // loads the log from disk -- which is how history made elsewhere arrives.
   const readNested=async()=>nestedId===null?null:realClient.request('read',[nestedId]).then(a=>({ok:a.ok,text:a.text.trim().slice(0,400)}),e=>({ok:false,text:String(e&&e.message)}));
   const beforeRestart=await readNested();
   let afterRestart=null;
   if(beforeRestart!==null&&!beforeRestart.ok){
    const stopped=real.stopDaemonsFor(store.store);
    const until=Date.now()+10000;
    while(real.daemonsMatching(store.store).length>0&&Date.now()<until)await new Promise(r=>setTimeout(r,100));
    if(real.daemonsMatching(store.store).length>0)errors.push(`the store's daemon did not stop: ${JSON.stringify(stopped)}`);
    afterRestart=await readNested();
   }
   const nestedVisible=(afterRestart??beforeRestart)!==null&&(afterRestart??beforeRestart).ok;
   const afterNested=[];
   const gatherIds=async els=>{for(const e of els){if(e.directory)await gatherIds(await provider.getChildren(e));else if(e.file)afterNested.push(e.file.path);}};
   await gatherIds(await provider.getChildren());
   const third=made('theourgia-files-export-');
   await send('export-md',[third]);
   const exportedA=fs.existsSync(path.join(third,'docs','a.md'))?fs.readFileSync(path.join(third,'docs','a.md'),'utf8'):'';
   const underPart=(await provider.getChildren({id:partId})).map(n=>n.id);
   const nested={visible:nestedVisible,id:nestedId,beforeRestart,afterRestart,listed:afterNested.includes('nested.md'),exported:walk(third).includes('nested.md'),
    contentInParent:exportedA.includes('NESTEDMARKER'),underPart:nestedId!==null&&underPart.includes(nestedId)};
   // A new document made through the window's own command, on the real core:
   // what the window said, and whether the next listing has the file.
   const docsDir=(await provider.getChildren()).find(e=>e.directory&&e.directory.prefix==='docs');
   shown.length=0;
   inputs.push('fresh');
   if(docsDir)await commands.get('theourgia.newFileHere')(docsDir);
   const madeShown=shown.slice();
   const relisted=[];
   const gatherAgain=async els=>{for(const e of els){if(e.directory)await gatherAgain(await provider.getChildren(e));else if(e.file)relisted.push(e.file.path);}};
   await gatherAgain(await provider.getChildren());
   const fourth=made('theourgia-files-export-');
   await send('export-md',[fourth]);
   const created={found:docsDir!==undefined,shown:madeShown,listed:relisted.includes('docs/fresh.md'),exported:fs.existsSync(path.join(fourth,'docs','fresh.md'))};
   errors.push(...shown.filter(n=>n.level==='error').map(n=>n.text));
   // On the real core: the moved text file renamed through the window,
   // the write conditional on the version its fresh read answered; then the
   // same row renamed again while somebody else writes the path during the
   // prompt, which the store refuses as changed.
   const xNode=await find(await provider.getChildren(),x.block.id);
   shown.length=0;inputAsked.length=0;
   inputs.push('renamed.scm');
   if(xNode)await commands.get('theourgia.renameFile')(xNode);
   const renameShown=shown.slice(),renameAsked=inputAsked.map(o=>o.prompt);
   const fifth=made('theourgia-files-export-');
   await send('export-code',[fifth]);
   shown.length=0;inputAsked.length=0;
   inputs.push(async()=>{await send('set',[x.block.id,'path','src/elsewhere.scm']);return 'late.scm';});
   if(xNode)await commands.get('theourgia.renameFile')(xNode);
   const staleShown=shown.slice(),staleAsked=inputAsked.map(o=>o.prompt);
   const xNow=(await blocksNow()).find(b=>b.block.id===x.block.id);
   const guarded={found:xNode!==null,renameShown,renameAsked,exported:walk(fifth).filter(p=>p.endsWith('.scm')),staleShown,staleAsked,pathAfter:xNow?pathOf(xNow):null};
   return {errors,listed:files(listed),exported:files(onDisk),listedDirs:folders(listed),exportedDirs:folders(onDisk),
    fileChildren,grandChildren,outlineChildren,duplicate,nested,created,guarded};
  }finally{realClient=null;store.dispose();for(const d of dirs)fs.rmSync(d,{recursive:true,force:true});}
 }
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
