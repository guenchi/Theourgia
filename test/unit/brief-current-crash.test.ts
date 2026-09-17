import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import {spawnSync} from 'child_process';
import {digestOfBytes} from '../../src/publication';

describe('XC-05/06 actual process death at every projection write boundary',function(){
  this.timeout(180000);
  it('old/new complete bodies and source pairs survive a second recovery death',()=>{
    const base=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-current-kill-'));
    const child=path.join(__dirname,'../support/current-crash.js');
    const run=(root:string,mode:string,body:string,stop=0):any=>{
      const result=spawnSync(process.execPath,[child,root,mode,body,String(stop)],{encoding:'utf8',timeout:15000});
      if(stop){assert.strictEqual(result.signal,'SIGKILL',result.stdout+result.stderr);assert.ok(fs.existsSync(path.join(root,'stopped.json')));return null;}
      assert.strictEqual(result.status,0,result.stdout+result.stderr);
      const answer=JSON.parse(result.stdout);assert.strictEqual(answer.complete,true);return answer;
    };
    let firstDeaths=0,secondDeaths=0;
    for(const [oldText,newText] of [['# A\nold','# A\nnew and much longer'],['# A\nold and much longer','# A\nx']]){
      const seed=fs.mkdtempSync(path.join(base,'seed-'));run(seed,'replace',oldText);
      const good=fs.mkdtempSync(path.join(base,'good-'));fs.cpSync(seed,good,{recursive:true});
      const completed=run(good,'replace',newText);
      for(let stop=1;stop<=completed.trace.length;stop++){
        const crashed=fs.mkdtempSync(path.join(base,'first-'));fs.cpSync(seed,crashed,{recursive:true});run(crashed,'replace',newText,stop);firstDeaths++;
        const before=fs.readFileSync(path.join(crashed,'block','current.md'),'utf8');
        assert.ok(before===oldText||before===newText,'first death has a whole body');
        const inspect=fs.mkdtempSync(path.join(base,'inspect-'));fs.cpSync(crashed,inspect,{recursive:true});
        const recovered=run(inspect,'recover','');
        const verify=(observed:any)=>{
          assert.ok(observed.bytes===oldText||observed.bytes===newText,'whole body after recovery');
          if(observed.sidecar.phase==='published')assert.strictEqual(observed.sidecar.written,digestOfBytes(observed.bytes),'stable source pairs with actual body');
          else assert.strictEqual(observed.decision.send,false,'prepared source cannot be sent');
        };
        verify(recovered);
        for(let again=1;again<=recovered.trace.length;again++){
          const twice=fs.mkdtempSync(path.join(base,'second-'));fs.cpSync(crashed,twice,{recursive:true});run(twice,'recover','',again);secondDeaths++;
          verify(run(twice,'recover',''));
        }
      }
    }
    assert.ok(firstDeaths>20&&secondDeaths>20,'both boundary sets were actually exercised');
    process.stdout.write(`XC kill coverage ${firstDeaths} first deaths; ${secondDeaths} recovery deaths; complete\n`);
  });
});
