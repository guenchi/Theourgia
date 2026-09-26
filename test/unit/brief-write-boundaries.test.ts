import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {spawn,spawnSync} from 'child_process';
import {nodeFileOps} from '../../src/fsops';
import {Owners} from '../../src/ownership';
import {Publisher,writeSidecar} from '../../src/publication';
const helper=path.join(__dirname,'../support/shared-mutator.js');
const operations=['publishNow','recoverCurrent','reconcile','reconcileBy','recordWorking','takeSequence',
  'markLegacySend','clearLegacySend','acknowledge','releaseSend','recordAnswer','rewrite'];
function child(directory:string,operation:string,hold='hold'){
  const process=spawn(global.process.execPath,[helper,directory,operation,hold]);
  const rows:any[]=[],waiting:Array<(v:any)=>void>=[];let buffer='',error='';
  process.stderr.on('data',b=>error+=b);
  process.stdout.on('data',b=>{buffer+=b;for(;;){const n=buffer.indexOf('\n');if(n<0)break;const row=JSON.parse(buffer.slice(0,n));buffer=buffer.slice(n+1);const next=waiting.shift();if(next)next(row);else rows.push(row);}});
  const exit=new Promise<void>((resolve,reject)=>{process.on('error',reject);process.on('close',code=>code===0?resolve():reject(new Error(error)));});
  return {process,exit,next:()=>rows.length?Promise.resolve(rows.shift()):new Promise<any>(resolve=>waiting.push(resolve))};
}
describe('XL-07 every mutable current or sidecar entry protects its read and write',function(){
  this.timeout(60000);
  for(const operation of operations)it(`XL-07 ${operation} excludes a second real process before its first read`,async()=>{
    const storage=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-write-boundary-'));
    const directory=path.join(storage,'sessions/S/store/a.1'),owners=new Owners();
    const publisher=new Publisher(nodeFileOps,{isOpen:()=>false},{owners,sessionId:'A'});
    const published=publisher.publishNow({directory,storeId:'store',blockId:'a.1',prefix:'',text:'body',cursor:null,expected:null});assert.ok(published.published);
    publisher.takeSequence(published.file,'R1');
    const sidecar=publisher.sidecarOf(published.file);assert.ok(sidecar);
    if(operation==='recoverCurrent')writeSidecar(nodeFileOps,published.file,{...sidecar,phase:'publishing',prior:null});
    if(operation.includes('Legacy')){
      fs.writeFileSync(path.join(directory,'1.md'),'body');
      writeSidecar(nodeFileOps,path.join(directory,'1.md'),{...sidecar,legacySend:operation==='clearLegacySend'});
    }
    const a=child(directory,operation);let b:ReturnType<typeof child>|undefined;
    try {
      assert.strictEqual((await a.next()).kind,'read','the active operation never reached an actual FileOps read');
      b=child(directory,operation);const attempted=await b.next();
      a.process.stdin.end('x');const completed=await a.next();await a.exit;
      const afterA=fs.readdirSync(directory).sort().map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]);
      if(attempted.kind==='read'){b.process.stdin.end('x');await b.next();}else b.process.stdin.end();
      await b.exit;
      assert.strictEqual(completed.status,'ok',JSON.stringify(completed));
      assert.ok(completed.writes>0,'the control did not reach a real state-changing write');
      assert.strictEqual(attempted.kind,'result','the competitor entered an unprotected read');
      assert.strictEqual(attempted.status,'RESOURCE_BUSY',JSON.stringify(attempted));
      assert.strictEqual(attempted.reads,0,'the guard started after a read of shared state');
      assert.deepStrictEqual(fs.readdirSync(directory).sort().map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]),afterA);
      const retry=spawnSync(process.execPath,[helper,directory,'takeSequence','free'],{encoding:'utf8',timeout:15000});
      assert.strictEqual(retry.status,0,retry.stderr);
      assert.strictEqual(JSON.parse(retry.stdout).status,'ok','normal completion releases the guard');
    } finally {a.process.kill('SIGKILL');b?.process.kill('SIGKILL');}
  });
});

/*
 * queue item 43, review r2 #3: THE RECORD reconcileLeaving RETURNS IS READ UNDER
 * THE LEASE. Within one process the exclusive section is re-entrant, so only a
 * second process can tell "read under the lease" from "read after it". Child A
 * reconciles on the offer's route and is held at its first read of the record
 * -- the read of the record it returns; child B then asks for a number, and
 * must be turned away before it reads anything.
 */
describe('queue item 43 the record reconcileLeaving returns is read under the lease',function(){
  this.timeout(60000);
  it('turns a second process away while reconcileLeaving reads the record it returns',async()=>{
    const storage=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-write-boundary-'));
    const directory=path.join(storage,'sessions/S/store/a.1');
    const publisher=new Publisher(nodeFileOps,{isOpen:()=>false},{owners:new Owners(),sessionId:'A'});
    const published=publisher.publishNow({directory,storeId:'store',blockId:'a.1',prefix:'',text:'body',cursor:null,expected:null});assert.ok(published.published);
    const a=child(directory,'reconcileLeaving','hold-meta');
    try {
      assert.strictEqual((await a.next()).kind,'read','reconcileLeaving never read the record beside the file');
      const b=spawnSync(process.execPath,[helper,directory,'takeSequence','free'],{encoding:'utf8',timeout:15000});
      assert.strictEqual(b.status,0,b.stderr);
      const attempted=JSON.parse(b.stdout.trim().split('\n').pop() as string);
      a.process.stdin.end('x');const completed=await a.next();await a.exit;
      assert.strictEqual(completed.status,'ok',JSON.stringify(completed));
      assert.strictEqual(attempted.status,'RESOURCE_BUSY','a second process took the lease while the returned record was being read: '+JSON.stringify(attempted));
      assert.strictEqual(attempted.reads,0,'the second process read shared state');
    } finally {a.process.kill('SIGKILL');}
  });
});
