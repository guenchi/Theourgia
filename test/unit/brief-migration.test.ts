import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import {migrateLegacy,MigrationSources,migrationIdentity} from '../../src/migration';
import {Publisher,UNNUMBERED,writeSidecar,digestOfBytes} from '../../src/publication';
import {nodeFileOps,FileOps} from '../../src/fsops';
function setup(extraDraft=false): {directory:string;sources:MigrationSources} {
  const directory=path.join(fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-migrate-')),'a.1');fs.mkdirSync(directory);
  for(let n=1;n<=5;n++){
    const text=n===3?'# A\ndraft':extraDraft&&n===2?'# A\nanother':'# A\ncommitted';
    const file=path.join(directory,`${n}.md`);fs.writeFileSync(file,text);
    writeSidecar(nodeFileOps,file,{...UNNUMBERED,format:1,storeId:'A',blockId:'a.1',phase:'published',prefix:'# A\n',written:digestOfBytes('# A\ncommitted'),previous:null,
      acknowledgedRaw:null,sent:null,cursor:null,localOnly:false,unresolved:false,bodyHasCrlf:false});
  }
  return {directory,sources:{committed:{text:'# A\ncommitted',prefix:'# A\n',source:{id:'C',kind:'committed',writer:'w',version:'cut',basedOn:'H'}},
    working:{text:'# A\ndraft',prefix:'# A\n',source:{id:'W',kind:'working',writer:'w',version:'v1',basedOn:'H'}}}};
}
function args(directory:string,sources:MigrationSources,files:FileOps=nodeFileOps){
  return {files,publisher:new Publisher(files,{isOpen:()=>false}),directory,storeId:'A',blockId:'a.1',sourceIsSafe:async()=>true,sourceStillSafe:()=>true,claimDestination:()=>true,sources:async()=>sources,isDirty:()=>false,pending:()=>false};
}
const bytes=(directory:string)=>Object.fromEntries(fs.readdirSync(directory).map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]));
describe('XC-10/11/12/13/15 legacy migration preserves every original',()=>{
  it('the third draft is selected from verified W and all originals have a named archive',async()=>{
    const {directory,sources}=setup(),before=bytes(directory);
    const result=await migrateLegacy(args(directory,sources));assert.ok(result.migrated);
    assert.deepStrictEqual(fs.readdirSync(directory).sort(),['current.md','current.md.meta']);
    assert.strictEqual(fs.readFileSync(result.file,'utf8'),'# A\ndraft');
    assert.deepStrictEqual(bytes(result.archive),before);
  });
  for(const variant of ['many-drafts','W-failed','pending','active','changed']){
    it(`incomplete migration retains the source ${variant}`,async()=>{
      const {directory,sources}=setup(variant==='many-drafts'),before=bytes(directory),parts=args(directory,sources);
      if(variant==='W-failed')parts.sources=async()=>{throw new Error('Core unavailable');};
      if(variant==='pending')parts.pending=()=>true;
      if(variant==='active')parts.sourceIsSafe=async()=>false;
      if(variant==='changed')parts.sources=async()=>{fs.writeFileSync(path.join(directory,'3.md'),'later edit');return sources;};
      const result=await migrateLegacy(parts);assert.strictEqual(result.migrated,false);
      if(variant!=='changed')assert.deepStrictEqual(bytes(directory),before);
      else assert.strictEqual(fs.readFileSync(path.join(directory,'3.md'),'utf8'),'later edit');
      assert.ok(!result.migrated&&result.retained.includes(path.join(directory,'3.md')));
    });
  }
  it('a crash after the directory move resumes from the recorded source',async()=>{
    const {directory,sources}=setup(),before=bytes(directory);
    const faulty={...nodeFileOps,rename(from:string,to:string){nodeFileOps.rename(from,to);if(from===directory)throw new Error('process stopped after move');}};
    await assert.rejects(()=>migrateLegacy(args(directory,sources,faulty)),/process stopped/);
    assert.deepStrictEqual(migrationIdentity(nodeFileOps,directory),{storeId:'A',blockId:'a.1',prefix:'# A\n'},'restart can resolve the missing original through its journal');
    const resumed=await migrateLegacy(args(directory,sources));assert.ok(resumed.migrated);
    assert.deepStrictEqual(bytes(resumed.archive),before);
    assert.strictEqual(fs.readFileSync(resumed.file,'utf8'),'# A\ndraft');
  });
});
