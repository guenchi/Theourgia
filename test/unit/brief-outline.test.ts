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

/*
 * queue item 48, review r1 #2: THE EDITOR IS SHOWN A NESTED DOCUMENT AS A MARKED CHILD. The model's cells read nodes;
 * this reads the tree item the extension hands the editor for the child the store reports as a nested document: the
 * warning icon and a tooltip naming the mark. The parent can be opened (review r1 #1, from the other side).
 */
describe('queue item 48 a nested document is drawn under its parent with its warning', function () {
  this.timeout(60000);
  it('draws the child with the warning icon and a tooltip naming nested-document, under a parent that opens', () => {
    const r = schedule('outline-nested');
    assert.strictEqual(r.rootCollapsible, 1, 'the parent cannot be opened (the stand-in spells Collapsed as 1)');
    assert.deepStrictEqual(r.childIds, ['a.2']);
    assert.deepStrictEqual(r.marks, ['nested-document']);
    assert.strictEqual(r.icon, 'warning', 'the nested document is drawn without its warning');
    assert.match(String(r.tooltip), /nested-document/, 'the tooltip does not name the mark');
  });
});

