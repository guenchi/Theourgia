import * as assert from 'assert';
import * as path from 'path';
import {spawnSync} from 'child_process';
function schedule(name: string): any {
  const run=spawnSync(process.execPath,[path.join(__dirname,'../support/extension-schedules.js'),'',name],{encoding:'utf8',timeout:20000});
  assert.strictEqual(run.status,0,run.stdout+run.stderr);
  const result=JSON.parse(run.stdout.trim().split('\n').pop() as string);
  assert.strictEqual(result.complete,true);
  return result.result;
}
describe('XG outline results retain generation ownership',function(){
  this.timeout(90000);
  for(const scenario of ['outline-switch','outline-aba','outline-children-switch','outline-children-unknown-switch']){
    it(`XG-01 late nodes are discarded ${scenario}`,()=>{
      assert.deepStrictEqual(schedule(scenario).nodes,[]);
    });
  }
  for(const scenario of ['outline-control','outline-children-control','outline-children-unknown-control']){
    it(`XG-02 same generation keeps actual nodes ${scenario}`,()=>{
      const result=schedule(scenario);
      assert.strictEqual(result.nodes.length,1);
      assert.strictEqual(result.nodes[0].title,'Alpha');
    });
  }
  it('XG-03 a previously drawn same-ID node cannot open the new store',()=>{
    const result=schedule('outline-old-click');
    assert.deepStrictEqual(result.afterClickReads,[]);
    assert.ok(result.messages.some((m:string)=>/outline|refresh/i.test(m)));
  });
});
