import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {Publisher} from '../../src/publication';
import {RecordingFs} from '../support/recording-fs';
import {Saving} from '../../src/saving';
import {digestOfBytes} from '../../src/publication';
const scratch=()=>fs.mkdtempSync(path.join(os.tmpdir(),'theourgia-current-'));
const request=(directory:string,text:string)=>({directory,storeId:'/stores/A',blockId:'a.1',prefix:'# A\n',text,cursor:null});
// The projection's name for the heading `# A` and block `a.1`, chosen at the first publication (queue item 5).
const CANON='a-a.1.md';
describe('XC v20 current projection replaces C2 immutable publication',()=>{
  it('XC-01/02 same canonical path holds every successful clean update',async()=>{
    const directory=scratch(),files=new RecordingFs(),publisher=new Publisher(files,{isOpen:()=>true,isDirty:()=>false});
    for(let n=0;n<15;n++){
      const text='# A\n'+'value'.repeat(n+1);
      const answer=await publisher.publish(request(directory,text));
      assert.strictEqual(answer.published,true,'clean updates');
      assert.strictEqual(answer.file,path.join(directory,CANON),'canonical path');
      assert.strictEqual(fs.readFileSync(answer.file as string,'utf8'),text,'updated bytes');
      assert.deepStrictEqual(fs.readdirSync(directory).sort(),[CANON,`${CANON}.meta`],'canonical list');
    }
  });
  it('XC-03 dirty documents retain both disk and metadata',async()=>{
    const directory=scratch();let dirty=false;
    const publisher=new Publisher(new RecordingFs(),{isOpen:()=>true,isDirty:()=>dirty});
    const first=await publisher.publish(request(directory,'# A\nold'));
    assert.ok(first.published);
    const before=fs.readdirSync(directory).map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]);dirty=true;
    const answer=await publisher.publish(request(directory,'# A\nnew'));
    assert.strictEqual(answer.published,false,'dirty refusal');
    assert.deepStrictEqual(fs.readdirSync(directory).map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]),before,'dirty bytes');
  });
  it('XC-04 current has one rename and zero truncating writes per replacement',async()=>{
    const directory=scratch(),files=new RecordingFs(),publisher=new Publisher(files,{isOpen:()=>false});
    await publisher.publish(request(directory,'# A\nlong old contents'));
    files.entries.length=0;
    const answer=await publisher.publish(request(directory,'# A\nx'));
    assert.strictEqual(answer.file,path.join(directory,CANON));
    assert.strictEqual(files.countOf('writeText',answer.file as string),0,'current writeText');
    assert.strictEqual(files.countOf('writeDurably',answer.file as string),0,'current truncate');
    assert.strictEqual(files.countOf('rename',answer.file as string),1,'current commit rename');
  });
  it('XC-20 an unrecognized current file is preserved',async()=>{
    const directory=scratch(),file=path.join(directory,'current.md');fs.writeFileSync(file,'unique foreign bytes');
    const answer=await new Publisher(new RecordingFs(),{isOpen:()=>false}).publish(request(directory,'replacement'));
    assert.strictEqual(answer.published,false,'foreign refusal');
    assert.strictEqual(fs.readFileSync(file,'utf8'),'unique foreign bytes');
  });
  it('XC-22 legacy versions are retained and require migration',async()=>{
    const directory=scratch();for(let n=1;n<=5;n++)fs.writeFileSync(path.join(directory,`${n}.md`),n===3?'unique draft':`history ${n}`);
    const before=fs.readdirSync(directory).map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]);
    const publisher=new Publisher(new RecordingFs(),{isOpen:()=>false});
    const answer=await publisher.publish(request(directory,'# A\nnew'));
    assert.strictEqual(answer.published,false,'migration required');
    assert.strictEqual(publisher.latestIn(directory),null,'legacy is not current');
    assert.deepStrictEqual(fs.readdirSync(directory).map(n=>[n,fs.readFileSync(path.join(directory,n),'hex')]),before);
  });
  it('XC-17 equal bytes with a new origin cannot borrow the old receipt',async()=>{
    const directory=scratch(),files=new RecordingFs(),publisher=new Publisher(files,{isOpen:()=>false});
    const first=await publisher.publish(request(directory,'# A\nsame'));
    assert.ok(first.published);
    const file=first.file as string,origin=(publisher.sidecarOf(file) as any).projection;
    publisher.takeSequence(file,'R1');
    await publisher.publish(request(directory,'# A\nsame'));
    const before=publisher.sidecarOf(file);
    let dequeued=false;
    new Saving(files).recordAnswer(file,{req:'R1',cursor:'w:1',rawDigest:digestOfBytes('# A\nsame'),sentDigest:digestOfBytes('same'),mismatch:false,
      send:{seq:1,prefixDigest:digestOfBytes('# A\n'),by:'store',projectionId:origin.id} as any},()=>{dequeued=true;});
    const after=publisher.sidecarOf(file);
    assert.deepStrictEqual(after?.confirmed,before?.confirmed,'new projection confirmation');
    assert.deepStrictEqual((after as any).projection,(before as any).projection,'new origin');
    assert.strictEqual(after?.nextSeq,2,'numbering is not reset');
    assert.strictEqual(dequeued,true,'old request can settle without claiming the new projection');
  });
});
