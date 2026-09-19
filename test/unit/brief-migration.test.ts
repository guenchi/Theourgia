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

  /*
   * A JOURNAL THAT WILL NOT READ, FOR BOTH READERS OF IT.
   *
   * Two rounds repaired one shape in two places: `migrateLegacy` used to
   * `continue` past an unreadable journal and report `no-legacy-files`,
   * and `migrationIdentity` used to answer null, which its caller draws
   * as "not a legacy block of this store". Neither repair had a cell --
   * measured in a fifteenth review round, where reverting
   * `migrationIdentity` to `continue` left every other cell in the tree
   * green. The variants above all fail somewhere else: none of them
   * makes reading the journal fail.
   *
   * The fixture is the crash above, which leaves a real prepared
   * journal, plus a `readText` that refuses that one file the way a
   * directory this process may not search refuses it.
   */
  async function prepared(): Promise<{directory:string;sources:MigrationSources;control:string}> {
    const {directory,sources}=setup();
    const faulty={...nodeFileOps,rename(from:string,to:string){nodeFileOps.rename(from,to);if(from===directory)throw new Error('process stopped after move');}};
    await assert.rejects(()=>migrateLegacy(args(directory,sources,faulty)),/process stopped/);
    const control=path.join(path.dirname(directory),'.migration');
    assert.ok(fs.readdirSync(control).some((n)=>n.endsWith('.json')),'the fixture wrote no journal, so neither cell below is about anything');
    return {directory,sources,control};
  }

  function blindTo(control:string): FileOps {
    return {...nodeFileOps,
      readText(file:string):string{
        if(path.dirname(file)===control && file.endsWith('.json')){
          const denied=new Error(`EACCES: permission denied, open '${file}'`) as NodeJS.ErrnoException;
          denied.code='EACCES';
          throw denied;
        }
        return nodeFileOps.readText(file);
      }};
  }

  it('a journal that cannot be read is not a block that is not a migration',async()=>{
    const {directory,control}=await prepared();
    assert.throws(
      ()=>migrationIdentity(blindTo(control),directory),
      /could not be read/,
      'an unreadable journal answered null, which the caller draws as "there is nothing to migrate here"'
    );
    assert.deepStrictEqual(
      migrationIdentity(nodeFileOps,directory),
      {storeId:'A',blockId:'a.1',prefix:'# A\n'},
      'the twin: the same journal, readable, still resolves the block'
    );
  });

  it('a journal that cannot be read is refused by name, not reported as nothing to migrate',async()=>{
    const {directory,sources,control}=await prepared();
    const result=await migrateLegacy(args(directory,sources,blindTo(control)));
    assert.strictEqual(result.migrated,false);
    assert.ok(!result.migrated && result.because==='unreadable-migration-record',
      `an unreadable journal was reported as ${!result.migrated?result.because:'migrated'}`);
    /*
     * ⛔ "SOMETHING IN THAT DIRECTORY" IS NOT "THE FILE THAT FAILED".
     *
     * Measured in a sixteenth review round: returning
     * `retained: [path.join(control, 'not-the-journal.json')]` passed
     * this cell, because it asked only which directory the path was in.
     * What the user is told to look at has to be the file that would
     * not read.
     */
    const journal=path.join(control,fs.readdirSync(control).find((n)=>n.endsWith('.json')) as string);
    assert.ok(!result.migrated && result.retained.includes(journal),
      `the journal it could not read (${journal}) was not named among what it kept: ${!result.migrated?result.retained.join(', '):''}`);
  });
});
