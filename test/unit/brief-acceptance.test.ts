import * as assert from 'assert';
import * as path from 'path';
import {spawnSync} from 'child_process';
import {digestOfBytes} from '../../src/publication';
function schedule(name: string): any {
  const run=spawnSync(process.execPath,[path.join(__dirname,'../support/extension-schedules.js'),'',name],{encoding:'utf8',timeout:20000});
  assert.strictEqual(run.status,0,run.stdout+run.stderr);
  const result=JSON.parse(run.stdout.trim().split('\n').pop() as string);
  assert.strictEqual(result.complete,true);
  return result.result;
}
describe('XO save acceptance uses the chain-time identity and sidecar',function(){
  this.timeout(90000);
  for(const scenario of ['save-early','save-chain']){
    it(`XO-01/02/03 populated queues stay unchanged ${scenario}`,()=>{
      const result=schedule(scenario);
      for(const raw of result.beforeQueues) assert.ok(JSON.parse(Buffer.from(raw,'hex').toString()).entries.length>0);
      assert.deepStrictEqual(result.afterQueues,result.beforeQueues,'queue bytes');
      assert.strictEqual(result.afterMeta,result.beforeMeta,'sequence and outstanding remain unchanged');
      assert.deepStrictEqual(result.numbering,[],'numbering not entered');
      assert.strictEqual(result.acceptances.length,scenario==='save-chain'?1:0,'acceptance entry');
      if(scenario==='save-chain')assert.strictEqual(result.acceptances[0].because,'another-store','acceptance verdict');
      assert.ok(result.messages.some((m:string)=>m.includes('/stores/A')&&m.includes('/stores/B')));
      /*
       * KEY: AND IT WAS RAISED AS AN ALARM. The three message channels
       * were indistinguishable in this harness, so a save refused for
       * belonging to another store could have been shown as a quiet
       * information notice and every assertion here would have held.
       * Found in an eleventh review round, in the SECOND editor double --
       * the first had been repaired for this a round earlier.
       */
      /*
       * KEY: THE CHANNEL OF THIS MESSAGE, not of any message.
       *
       * `channels.includes('error')` was satisfied by the setup saves,
       * which raise errors of their own before the one this cell is
       * about -- measured in a twelfth review round. `shown` pairs each
       * text with the channel it went out on, so the question can be put
       * about the sentence that names both stores.
       */
      const wrongStore = (result.shown as Array<{ text: string; level: string }>).filter(
        (m) => m.text.includes('/stores/A') && m.text.includes('/stores/B')
      );
      assert.strictEqual(wrongStore.length, 1, 'the wrong-store message was not shown exactly once');
      assert.notStrictEqual(
        wrongStore[0].level,
        'information',
        'a save sent to the wrong store was announced as a quiet information notice'
      );
    });
  }
  it('XO-03 legal twin adds one A record and leaves B unchanged',()=>{
    const result=schedule('save-control');
    const entries=(raw:string)=>JSON.parse(Buffer.from(raw,'hex').toString()).entries;
    assert.strictEqual(entries(result.afterQueues[0]).length,entries(result.beforeQueues[0]).length+1);
    assert.strictEqual(result.afterQueues[1],result.beforeQueues[1]);
    assert.strictEqual(result.acceptances[0].accepted,true);
    assert.strictEqual(result.numbering.length,1);
  });
  it('XO-10 a preceding reconciliation changes the accepted prefix and body',()=>{
    const result=schedule('save-prefix');
    assert.strictEqual(result.acceptances[0].accepted,true);
    assert.strictEqual(result.acceptances[0].record.prefixDigest,digestOfBytes('# '),'accepted prefix');
    assert.strictEqual(result.acceptances[0].record.intent.body,'Alpha\nsecond A\n','accepted body');
  });
  for(const name of ['save-working-switch','save-working-owner','save-working-failed']){
    it(`XO W preparation cannot bypass final acceptance ${name}`,()=>{
      const result=schedule(name);
      assert.deepStrictEqual(result.afterQueues,result.beforeQueues,'both populated queues are unchanged');
      assert.deepStrictEqual(result.numbering,[],'no send sequence was spent');
      if(name==='save-working-switch'){
        assert.strictEqual(result.acceptances[0].because,'another-store');
      } else {
        assert.strictEqual(result.afterMeta,result.beforeMeta);
        assert.ok(result.messages.some((m:string)=>m.includes('not confirmed')||m.includes('working note')));
      }
    });
  }
  for(const dirty of [true,false]){
    it(`XC final publication observes the editor after W read dirty=${dirty}`,()=>{
      const result=schedule(dirty?'open-late-dirty':'open-late-clean');
      if(dirty){
        assert.deepStrictEqual(result.after,result.before);
        assert.ok(result.messages.some((m:string)=>m.includes('unsaved edits')));
      }else{
        assert.strictEqual(result.after[0],result.before[0]);
        assert.ok(!result.messages.some((m:string)=>m.includes('unsaved edits')));
      }
    });
  }
});
