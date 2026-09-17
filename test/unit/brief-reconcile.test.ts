import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { spawnSync } from 'child_process';
import { Publisher } from '../../src/publication';
import { Owners } from '../../src/ownership';
import { RecordingFs } from '../support/recording-fs';

function schedule(name: string): any {
  const run = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {encoding:'utf8', timeout:20000});
  assert.strictEqual(run.status, 0, run.stdout + run.stderr);
  const rows = run.stdout.trim().split('\n');
  const answer = JSON.parse(rows[rows.length - 1]);
  assert.strictEqual(answer.complete, true);
  return answer.result;
}

describe('XR reconcile keeps the offered source across waits', function () {
  this.timeout(90000);
  for (const name of ['reconcile-switch', 'reconcile-chain-switch', 'reconcile-control']) {
    it(`XR-01 captured target ${name}`, () => {
      const result = schedule(name);
      assert.deepStrictEqual(result.afterB, result.beforeB, 'The other store was modified');
      const contents: string[] = Object.entries(result.afterA).filter(([p])=>p.endsWith('.md')).map(([,b])=>Buffer.from(b as string,'hex').toString());
      assert.ok(contents.includes('# Alpha\noffered text\n'), 'The offered source was not used');
      assert.deepStrictEqual(result.requests.filter((r:any)=>r.verb==='write').map((r:any)=>r.store),['/stores/A'],'W writes retain the captured store across the picker');
    });
  }
  it('XR-07 dirty during confirmation refuses publication visibly', () => {
    const result = schedule('reconcile-dirty');
    assert.deepStrictEqual(result.afterA, result.beforeA);
    assert.strictEqual(result.notice.level, 'warning');
    assert.match(result.notice.text, /unsaved|dirty/);
  });
  it('XR-03 changed offer retains the later file and refuses', () => {
    const result = schedule('reconcile-changed');
    const contents = Object.entries(result.afterA).filter(([p])=>p.endsWith('.md')).map(([,b])=>Buffer.from(b as string,'hex').toString());
    assert.deepStrictEqual(contents, ['later edit\n']);
    assert.strictEqual(result.notice.level, 'warning');
  });
  it('XR-05 immediate ownership denial names the reason', async () => {
    const files = new RecordingFs(), directory = fs.mkdtempSync(path.join(os.tmpdir(), 'xr-owner-'));
    const owners = new Owners(files);
    const publisher = new Publisher(files, {isOpen:()=>false}, {owners,sessionId:'first'});
    const result = await publisher.publish({directory,storeId:'A',blockId:'a.1',prefix:'# A\n',text:'# A\nbody\n',cursor:null});
    assert.ok(result.published);
    if (!result.published) return;
    const other = new Publisher(files,{isOpen:()=>false},{owners,sessionId:'second'});
    const before = fs.readFileSync(result.file+'.meta');
    const refusal = other.reconcile(result.file,'# A\n','# A\nbody\n');
    assert.strictEqual((refusal as any).because,'not-ours');
    assert.deepStrictEqual(fs.readFileSync(result.file+'.meta'),before);
  });
});
