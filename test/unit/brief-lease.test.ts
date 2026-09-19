import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {spawn,spawnSync} from 'child_process';
import {Outbox} from '../../src/outbox';
/*
 * ⛔ THE LIST IS THE CENSUS, AND IT HAD A HOLE IN IT.
 *
 * `unparkAll` was missing, and so was its lock: a sixteenth review round
 * measured a controlled interleaving in which it read a queue, another
 * window added a save, and its commit wrote back the file it had read.
 * The save was gone and nothing failed. A list of names cannot shout
 * about the name that is not on it, so the name went on the list and
 * the operation went under the lock in the same change.
 */
const operations=['enqueue','clearCursor','setCursor','resolve','aboutToSend','markImported','markParked','markPending','unparkAll'];
function child(file:string,operation:string){
  const process=spawn(global.process.execPath,[path.join(__dirname,'../support/shared-queue.js'),file,operation,'hold']);
  const received:any[]=[],waiting:Array<(v:any)=>void>=[];let text='',error='';
  process.stderr.on('data',b=>{error+=b;});
  process.stdout.on('data',b=>{text+=b;for(;;){const at=text.indexOf('\n');if(at<0)break;const item=JSON.parse(text.slice(0,at));text=text.slice(at+1);const waiter=waiting.shift();if(waiter)waiter(item);else received.push(item);}});
  const exit=new Promise<void>((resolve,reject)=>{process.on('close',code=>code===0?resolve():reject(new Error(error)));process.on('error',reject);});
  return {process,exit,next:()=>received.length?Promise.resolve(received.shift()):new Promise<any>(resolve=>waiting.push(resolve))};
}
describe('XL-02/07 two actual processes cannot overlap queue read-modify-write',function(){
  this.timeout(90000);
  for(const operation of operations)it(`XL-07 ${operation} protects the final read through the write`,async()=>{
    const root=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-lease-')),file=path.join(root,'outbox.json');
    const box=new Outbox(file);box.setCursor('w:1');
    box.enqueue({req:'R1',cursor:'w:1',id:'a.1',field:'src',payload:'old',state:'queued',createdAt:1,lastError:null,importedBy:null});
    /*
     * ONE OPERATION NEEDS SOMETHING TO DO. `unparkAll` that releases
     * nothing commits nothing, and this row is about the write.
     */
    if(operation==='unparkAll')box.markParked('R1','parked');
    const a=child(file,operation);assert.strictEqual((await a.next()).kind,'read');
    const b=child(file,'enqueueB'),second=await b.next();
    a.process.stdin.end('x');const finishedA=await a.next();await a.exit;
    const afterA=JSON.parse(fs.readFileSync(file,'utf8'));
    if(second.kind==='read'){b.process.stdin.end('x');await b.next();}else b.process.stdin.end();
    await b.exit;
    assert.strictEqual(finishedA.complete,true);assert.strictEqual(finishedA.status,'ok');
    assert.strictEqual(second.kind,'result','competing process entered the protected read');
    assert.strictEqual(second.status,'busy','competing operation is a concrete busy refusal');
    assert.deepStrictEqual(JSON.parse(fs.readFileSync(file,'utf8')),afterA,'busy operation changed no bytes');
    const retry=spawnSync(process.execPath,[path.join(__dirname,'../support/shared-queue.js'),file,'enqueueB','free'],{encoding:'utf8',timeout:15000});
    assert.strictEqual(retry.status,0,retry.stderr);assert.strictEqual(JSON.parse(retry.stdout).status,'ok');
    const final=JSON.parse(fs.readFileSync(file,'utf8'));
    assert.deepStrictEqual(final.entries.slice(0,-1),afterA.entries,'retry preserves every prior entry');
    assert.strictEqual(final.entries.at(-1).req,'B');assert.strictEqual(final.cursor,afterA.cursor);
  });
});
