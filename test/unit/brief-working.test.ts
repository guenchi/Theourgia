import * as assert from 'assert';
import * as path from 'path';
import {Working} from '../../src/working';
import {RealStore} from '../support/real-core';
import {Outbox} from '../../src/outbox';
import {Saver} from '../../src/saver';
import {nodeFileOps} from '../../src/fsops';
import {Publisher, digestOfBytes} from '../../src/publication';
import {recordFor} from '../../src/record';
import {answerOf, readEvent} from '../../src/wire';
import {eventFromWrite} from '../../src/cursor';

describe('WS-28/31 W is the real plugin body authority',function(){
  this.timeout(120000);
  it('transfers a stale draft to a fresh window without rebasing or sharing its key',async()=>{
    const store=await RealStore.make('working-handoff');
    try {
      const inserted=await store.client.request('insert',['--title','A','--text','old']);
      const ev=readEvent((answerOf(inserted.answers[0],'ok')?.value('events') as unknown[])[0]);assert.ok(ev);
      const id=`${ev.writer}.${ev.seq}`,prefix='# A\n';
      const old=new Working(store.client,'window-old'),fresh=new Working(store.client,'window-fresh');
      const displayed=await old.read(id,prefix);
      await store.client.request('set',[id,'src','external']);
      const transferred=await fresh.write(id,'recovered',prefix,false,displayed.source);
      assert.strictEqual(transferred.source.basedOn,displayed.source.basedOn);
      assert.strictEqual(transferred.source.cut,displayed.source.cut);
      await old.write(id,'orphan late write',prefix);
      assert.strictEqual((await fresh.read(id,prefix)).body,'recovered');
      const result=await store.client.request('commit',[id,'--writer',fresh.writer]);
      assert.ok(result.text.includes('stale-baseline'),result.text);
    } finally {store.dispose();}
  });
  it('saved notes survive refresh, selected commits and later-note replay',async()=>{
    const store=await RealStore.make('working-plugin');
    try{
      const inserted=await store.client.request('insert',['--title','A','--text','old']);
      const events=answerOf(inserted.answers[0],'ok')?.value('events') as unknown[];
      const ev=readEvent(events[0]);assert.ok(ev);
      const id=`${ev.writer}.${ev.seq}`,prefix='# A\n';
      const a=new Working(store.client,'window-test-a'),b=new Working(store.client,'window-test-b');
      const saved=await a.write(id,'edited\n',prefix);
      assert.strictEqual((await b.read(id,prefix)).body,'old','other namespace sees committed');
      const publisher=new Publisher(nodeFileOps,{isOpen:()=>true,isDirty:()=>false});
      const directory=path.join(store.root,'projection');
      await publisher.publish({directory,storeId:store.store,blockId:id,prefix,text:prefix+saved.body,cursor:null,projection:saved.source});
      const reread=await a.read(id,prefix);
      const refreshed=await publisher.publish({directory,storeId:store.store,blockId:id,prefix,text:prefix+reread.body,cursor:null,projection:reread.source});
      assert.strictEqual(nodeFileOps.readText(refreshed.file as string),prefix+'edited\n');
      const queue=new Outbox(path.join(store.root,'outbox.json'));
      const saver=new Saver(store.client,queue,(req,answer)=>{
        if(answer.verdict==='confirmed')queue.resolve(req,answer.cursor);
        if(answer.verdict==='refused')queue.resolve(req,null);
      });
      const send=recordFor({req:'working-plugin-R1',store:store.store,storeHash:'probe',blockId:id,file:refreshed.file as string,
        projectionId:saved.source.id,rawDigest:digestOfBytes(prefix+saved.body),sentDigest:digestOfBytes(saved.body),prefixDigest:digestOfBytes(prefix),seq:1,
        intent:{verb:'commit',field:'src',body:saved.body,expectation:JSON.stringify({writer:a.writer,version:saved.source.version})}});
      const result=await saver.submit(send);
      assert.strictEqual(result.status,'saved',result.message);
      assert.strictEqual(queue.entries.length,0,'completed selected commit leaves no pending entry');
      assert.strictEqual((await a.read(id,prefix)).source.kind,'committed');
      assert.strictEqual((await b.read(id,prefix)).body,'edited\n');
      await a.write(id,'later\n',prefix);
      const direct=await store.client.request('commit',[id,'--writer',a.writer,'--working-version',saved.source.version,
        '--req',send.req,'--cursor',`${ev.writer}:${ev.seq}`]);
      assert.ok(direct.ok,direct.text);
      assert.ok(eventFromWrite(direct),'commit replay exposes the original event');
      assert.strictEqual((await a.read(id,prefix)).body,'later\n');
    } finally {store.dispose();}
  });
});
