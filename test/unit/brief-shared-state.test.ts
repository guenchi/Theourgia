import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {spawn,spawnSync} from 'child_process';
import {activateCore} from '../../src/activate';
import {nodeFileOps} from '../../src/fsops';
import {Outbox} from '../../src/outbox';
import {Owners} from '../../src/ownership';
import {Publisher} from '../../src/publication';
import {Saving} from '../../src/saving';
import {strangersReceipt} from '../support/receipts';
const helper=path.join(__dirname,'../support/shared-state.js');
const scratch=()=>fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-shared-state-'));
function run(...args:string[]):any {
  const result=spawnSync(process.execPath,[helper,...args],{encoding:'utf8',timeout:15000});
  assert.strictEqual(result.status,0,result.stderr);return JSON.parse(result.stdout);
}
function held(...args:string[]) {
  const child=spawn(process.execPath,[helper,...args]);let buffer='',stderr='';
  const lines:any[]=[],waiters:Array<(value:any)=>void>=[];
  child.stderr.on('data',b=>stderr+=b);
  child.stdout.on('data',b=>{buffer+=b;for(;;){const n=buffer.indexOf('\n');if(n<0)break;const value=JSON.parse(buffer.slice(0,n));buffer=buffer.slice(n+1);const next=waiters.shift();if(next)next(value);else lines.push(value);}});
  const exit=new Promise<{code:number|null;signal:NodeJS.Signals|null}>((resolve,reject)=>{
    child.on('error',reject);child.on('close',(code,signal)=>{if(code!==0&&!signal)reject(new Error(stderr));else resolve({code,signal});});
  });
  return {child,exit,next:()=>lines.length?Promise.resolve(lines.shift()):new Promise<any>(resolve=>waiters.push(resolve))};
}
describe('XL actual shared sidecar, process identity and takeover boundaries',function(){
  this.timeout(60000);
  it('XL-08 a stopped writer blocks handoff; death releases the same lock inode',async()=>{
    const storage=scratch(),directory=path.join(storage,'sessions','S','store','a.1');
    const publisher=new Publisher(nodeFileOps,{isOpen:()=>false},{owners:new Owners(),sessionId:'A'});
    const first=await publisher.publish({directory,storeId:'store',blockId:'a.1',prefix:'',text:'body',cursor:null,expected:null});assert.ok(first.published);
    const a=held(directory,'sequence','A','hold');
    try {
      assert.strictEqual((await a.next()).kind,'held');
      const locks=path.join(storage,'.resource-locks'),lock=path.join(locks,fs.readdirSync(locks)[0]);
      const inode=fs.statSync(lock).ino,before=fs.readFileSync(first.file+'.meta','hex');
      a.child.kill('SIGSTOP');
      assert.strictEqual(run(directory,'owner','B').status,'RESOURCE_BUSY');
      assert.strictEqual(fs.readFileSync(first.file+'.meta','hex'),before);
      a.child.kill('SIGKILL');assert.strictEqual((await a.exit).signal,'SIGKILL');
      const takeover=run(directory,'owner','B');assert.ok(takeover.outcome.held);
      assert.strictEqual(fs.statSync(lock).ino,inode,'handoff did not replace the stable lock inode');
      assert.throws(()=>new Saving(nodeFileOps,{owners:new Owners(),sessionId:'A'}).releaseSend(first.file,1),/ownership|owner/i);
      assert.strictEqual(fs.readFileSync(first.file+'.meta','hex'),before,'late owner wrote the new owner sidecar');
      assert.deepStrictEqual(fs.readdirSync(directory).sort(),[path.basename(first.file),path.basename(first.file)+'.meta']);
    } finally {a.child.kill('SIGCONT');a.child.kill('SIGKILL');}
  });
  it('XL-05 two real activations cannot reuse a live session namespace',async()=>{
    const storage=scratch(),a=held(storage,'activate','same','hold');
    try {
      assert.strictEqual((await a.next()).kind,'held');
      const file=path.join(storage,'sessions/same/session.json'),before=fs.readFileSync(file,'hex');
      assert.strictEqual(run(storage,'activate','same','free').status,'SESSION_BUSY');
      assert.strictEqual(fs.readFileSync(file,'hex'),before);
      assert.strictEqual(run(storage,'activate','different','free').status,'ok');
      a.child.stdin.end('x');await a.next();await a.exit;
      assert.strictEqual(run(storage,'activate','same','free').status,'ok','a proven dead incarnation can be replaced');
    } finally {a.child.kill('SIGKILL');}
  });
  it('XL-09 import rechecks a real source that became live after the claim',async()=>{
    const storage=scratch();assert.strictEqual(run(storage,'activate','old','free').status,'ok');
    const core=activateCore({files:nodeFileOps,globalStorage:storage,documents:{isOpen:()=>false},stores:[],sessionId:'new'});
    const source=new Outbox(core.sessions.outboxPathFor('old','store'));
    source.enqueue({req:'R',cursor:'w:1',id:'a.1',field:'src',payload:'draft',state:'queued',createdAt:1,lastError:null,importedBy:null});
    const claim=await core.sessions.claim('old');assert.ok(claim.claimed);
    const before=fs.readFileSync(source.path,'hex'),a=held(storage,'activate','old','hold');
    try {
      assert.strictEqual((await a.next()).kind,'held');let adopted=0;
      assert.throws(()=>core.sessions.importFrom({deadSessionId:'old',file:claim.token,sequence:claim.sequence},
        {has:()=>false,adopt:()=>{adopted++;return strangersReceipt();}},'store'),/source session or its claim changed/);
      assert.strictEqual(adopted,0);assert.strictEqual(fs.readFileSync(source.path,'hex'),before);
    } finally {a.child.kill('SIGKILL');await a.exit;}
  });
  it('XL-10 migration refuses a live adopter and can take over after its death',async()=>{
    const storage=scratch();assert.strictEqual(run(storage,'activate','old','free').status,'ok');
    const core=activateCore({files:nodeFileOps,globalStorage:storage,documents:{isOpen:()=>false},stores:[],sessionId:'new'});
    const directory=core.sessions.directoryFor('old','store','a.1');
    const b=held(storage,'activate','adopter','hold');
    try {
      assert.strictEqual((await b.next()).kind,'held');
      assert.ok(new Owners().take(directory,'adopter',[]).held);
      assert.strictEqual(core.sessions.claimMigrationDestination(directory,new Owners()),false);
      b.child.kill('SIGKILL');await b.exit;
      assert.strictEqual(core.sessions.claimMigrationDestination(directory,new Owners()),true);
      const owner=new Owners().ownerOf(directory);assert.ok(owner.known);
      assert.strictEqual(owner.record?.sessionId,'new');
    } finally {b.child.kill('SIGKILL');}
  });
  /*
   * queue item 26: WHICH SIDECAR A TAKEOVER SAYS THE PREVIOUS OWNER OWES. The
   * new owner record carries, for the projection's sidecar, the previous
   * owner's stamp -- that is what lets a late write from the previous owner be
   * told apart. Passing no sidecar survived the whole suite (item 5, P8). With
   * no projection in the directory yet there is no sidecar anybody could owe,
   * and none is named.
   */
  for(const withProjection of [true,false])it(`XL-10b a takeover names ${withProjection?"the projection's sidecar":'no sidecar when there is no projection'} as owed by the previous owner`,async()=>{
    const storage=scratch();assert.strictEqual(run(storage,'activate','old','free').status,'ok');
    const core=activateCore({files:nodeFileOps,globalStorage:storage,documents:{isOpen:()=>false},stores:[],sessionId:'new'});
    const directory=core.sessions.directoryFor('old','store','a.1');
    const b=held(storage,'activate','adopter','hold');
    try {
      assert.strictEqual((await b.next()).kind,'held');
      assert.ok(new Owners().take(directory,'adopter',[]).held);
      let sidecar:string|null=null;
      if(withProjection){
        const adopter=new Publisher(nodeFileOps,{isOpen:()=>false},{owners:new Owners(),sessionId:'adopter'});
        const published=adopter.publishNow({directory,storeId:'store',blockId:'a.1',prefix:'',text:'body',cursor:null,expected:null});
        assert.ok(published.published,JSON.stringify(published));
        sidecar=`${(published as {file:string}).file}.meta`;
        assert.ok(fs.existsSync(sidecar),'the projection has no sidecar, so there is nothing to owe');
      }
      const adopted=new Owners().ownerOf(directory);assert.ok(adopted.known&&adopted.record);
      b.child.kill('SIGKILL');await b.exit;
      assert.strictEqual(core.sessions.claimMigrationDestination(directory,new Owners()),true);
      const owner=new Owners().ownerOf(directory);assert.ok(owner.known&&owner.record);
      assert.strictEqual(owner.record.sessionId,'new');
      const previous={sessionId:'adopter',generation:adopted.record.generation};
      assert.deepStrictEqual(owner.record.expected,sidecar===null?{}:{[sidecar]:[previous]},
        'the takeover did not name exactly the sidecar the previous owner owes');
    } finally {b.child.kill('SIGKILL');}
  });
});
