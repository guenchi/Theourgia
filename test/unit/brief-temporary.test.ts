import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import {execFileSync} from 'child_process';
import {nodeFileOps, FileOps} from '../../src/fsops';
import {activateCore} from '../../src/activate';
import {cleanupTemporary} from '../../src/temporary';
import {Publisher} from '../../src/publication';

describe('XU real cleanup observations',()=>{
  for(const change of ['present','absent','unknown']){
    it(`XU-01/02/05 session rename and cleanup fail before ${change} probe`,()=>{
      assert.notStrictEqual(process.getuid?.(),0,'The permission device requires an ordinary user');
      const storage=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-cleanup-'));
      let temporary='',parent='',original:unknown,removed:unknown;
      const probes:unknown[]=[];
      const files:FileOps={...nodeFileOps,
        writeDurably(file,text){nodeFileOps.writeDurably(file,text);if(file.includes('session.json.')){temporary=file;parent=path.dirname(file);fs.chmodSync(parent,0o555);}},
        rename(from,to){try{nodeFileOps.rename(from,to);}catch(e){original=e;throw e;}},
        unlink(file){try{nodeFileOps.unlink(file);}catch(e){
          removed=e;
          execFileSync(process.execPath,['-e',
            "const fs=require('fs');const [p,f,mode]=process.argv.slice(1);if(mode==='absent'){fs.chmodSync(p,0o755);fs.unlinkSync(f);}if(mode==='unknown')fs.chmodSync(p,0);",
            parent,file,change]);
          throw e;
        }},
        presenceOf(file){const result=nodeFileOps.presenceOf(file);probes.push(result);return result;}
      };
      let failure:NodeJS.ErrnoException|undefined;
      try{
        activateCore({files,globalStorage:storage,documents:{isOpen:()=>false},stores:[],sessionId:'s-test'});
      }catch(e){failure=e as NodeJS.ErrnoException;}finally{if(parent)fs.chmodSync(parent,0o755);}
      assert.ok(failure,'Product publication must fail');
      assert.strictEqual((original as NodeJS.ErrnoException).code,'EACCES','actual rename failure');
      assert.strictEqual((removed as NodeJS.ErrnoException).code,'EACCES','actual unlink failure');
      assert.strictEqual(failure.cause,original,'original cause');
      assert.strictEqual(failure.code,'EACCES');
      assert.deepStrictEqual(probes,[change==='unknown'?{known:false}:{known:true,there:change==='present'}]);
      assert.strictEqual(fs.existsSync(temporary),change!=='absent');
      if(change==='present')assert.ok(failure.message.endsWith(`The cleanup probe found an object at ${temporary}; verify it before removing it.`));
      if(change==='unknown')assert.ok(failure.message.endsWith(`The cleanup probe could not determine whether ${temporary} exists.`));
      if(change==='absent')assert.strictEqual(failure.message,(original as Error).message);
    });
  }
  it('XU-04 successful unlink does not probe a later object at the same name',()=>{
    const dir=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-recreated-')),file=path.join(dir,'ours.tmp');
    fs.writeFileSync(file,'original');
    let probes=0;
    const result=cleanupTemporary({...nodeFileOps,
      unlink(p){nodeFileOps.unlink(p);execFileSync(process.execPath,['-e',"require('fs').writeFileSync(process.argv[1],'replacement')",p]);},
      presenceOf(p){probes++;return nodeFileOps.presenceOf(p);}
    },file,new Error('primary'));
    assert.strictEqual(probes,0);
    assert.strictEqual(result.presence,'not-probed');
    assert.strictEqual(fs.readFileSync(file,'utf8'),'replacement');
  });
  for(const change of ['present','absent','unknown']){
    it(`XU-08/09 block replacement reaches actual failed rename and ${change} probe`,async()=>{
      assert.notStrictEqual(process.getuid?.(),0,'The permission device requires an ordinary user');
      const root=fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-block-cleanup-')),directory=path.join(root,'block');
      const request={directory,storeId:'A',blockId:'a.1',prefix:'',text:'old whole body',cursor:null,expected:null};
      const initial=new Publisher(nodeFileOps,{isOpen:()=>false});await initial.publish(request);
      // An empty prefix in block `a.1` is published as `a.1.md` (queue item 5).
      const current=path.join(directory,'a.1.md');let parent='',temporary='',original:unknown,removed:unknown;
      const probes:unknown[]=[];
      const files:FileOps={...nodeFileOps,
        rename(from,to){
          if(to===current){temporary=from;parent=path.dirname(from);fs.chmodSync(parent,0o555);}
          try{nodeFileOps.rename(from,to);}catch(e){if(to===current)original=e;throw e;}
        },
        unlink(file){try{nodeFileOps.unlink(file);}catch(e){removed=e;
          execFileSync(process.execPath,['-e',"const fs=require('fs');const [p,f,mode]=process.argv.slice(1);if(mode==='absent'){fs.chmodSync(p,0o755);fs.unlinkSync(f);}if(mode==='unknown')fs.chmodSync(p,0);",parent,file,change]);throw e;
        }},
        presenceOf(file){const found=nodeFileOps.presenceOf(file);probes.push(found);return found;}
      };
      let failure:NodeJS.ErrnoException|undefined;
      try{await new Publisher(files,{isOpen:()=>false}).publish({...request,text:'new whole body',expected:initial.revisionIn(directory)});}catch(e){failure=e as NodeJS.ErrnoException;}
      finally{if(parent)fs.chmodSync(parent,0o755);}
      assert.ok(failure,'actual replacement failed');
      assert.strictEqual((original as NodeJS.ErrnoException).code,'EACCES','actual body rename');
      assert.strictEqual((removed as NodeJS.ErrnoException).code,'EACCES','actual temporary unlink');
      assert.strictEqual(failure.cause,original);assert.strictEqual(failure.code,'EACCES');
      assert.deepStrictEqual(probes,[change==='unknown'?{known:false}:{known:true,there:change==='present'}]);
      assert.strictEqual(fs.readFileSync(current,'utf8'),'old whole body','canonical file never unlinked');
      assert.strictEqual(fs.existsSync(temporary),change!=='absent');
      assert.ok(initial.recoverCurrent(current),'prepared sidecar recovers the retained old source');
      assert.strictEqual(initial.sidecarOf(current)?.phase,'published');
    });
  }
});
