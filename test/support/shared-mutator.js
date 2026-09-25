'use strict';
const fs=require('fs'),path=require('path');
const {nodeFileOps}=require('../../src/fsops');
const {Publisher,digestOfBytes}=require('../../src/publication');
const {Owners}=require('../../src/ownership');
const {Saving}=require('../../src/saving');
const [directory,operation,hold]=process.argv.slice(2);
// The block's projection, found as the product finds it; with none yet, the name a first
// publication with an empty prefix gives block `a.1` (queue item 5).
const file=operation.includes('Legacy')?path.join(directory,'1.md')
  :(new Publisher(nodeFileOps,{isOpen:()=>false}).latestIn(directory)||path.join(directory,'a.1.md'));
const initial=new Publisher(nodeFileOps,{isOpen:()=>false}).sidecarOf(file);
const owner=new Owners().ownerOf(directory);
const report=value=>fs.writeSync(1,JSON.stringify(value)+'\n');
let reads=0,writes=0;
const files={...nodeFileOps};
for(const name of ['readText','readBytes','list','readDirectory'])files[name]=(...args)=>{
  const result=nodeFileOps[name](...args);reads++;
  if(reads===1&&hold==='hold'){report({kind:'read'});fs.readSync(0,Buffer.alloc(1),0,1,null);}
  return result;
};
for(const name of ['writeDurably','writeText','rename','unlink','link'])files[name]=(...args)=>{writes++;return nodeFileOps[name](...args);};
try {
 const owners=new Owners(files),publisher=new Publisher(files,{isOpen:()=>false},{owners,sessionId:'A'});
 const saving=new Saving(files,{owners,sessionId:'A'}),digest=digestOfBytes('body');
 const source={id:'next-origin',kind:'working',writer:'window-a',version:'next-version',basedOn:'H0',cut:'()'};
 const calls={
  publishNow:()=>publisher.publishNow({directory,storeId:'store',blockId:'a.1',prefix:'',text:'next',cursor:null}),
  recoverCurrent:()=>publisher.recoverCurrent(file),
  reconcile:()=>publisher.reconcile(file,'','body'),
  reconcileBy:()=>publisher.reconcileBy(file,'take-store-version','','next','body'),
  recordWorking:()=>publisher.recordWorking(file,source,digest,initial.projection.id),
  takeSequence:()=>publisher.takeSequence(file,'R2'),
  markLegacySend:()=>publisher.markLegacySend(directory),
  clearLegacySend:()=>publisher.clearLegacySend(file),
  acknowledge:()=>publisher.acknowledge(file,digest,digest,'w:2'),
  releaseSend:()=>saving.releaseSend(file,1),
  recordAnswer:()=>saving.recordAnswer(file,{req:'R1',cursor:'w:2',rawDigest:digest,sentDigest:digest,mismatch:false,
    send:{seq:1,prefixDigest:digestOfBytes(''),by:'store',projectionId:initial.projection.id}}),
  rewrite:()=>owners.rewrite(directory,{...owner.record,expected:{}}),
 };
 if(!calls[operation])throw Error('Unknown shared mutator');
 const result=calls[operation]();report({kind:'result',status:'ok',result,reads,writes});
} catch(error){report({kind:'result',status:error.code??'error',error:String(error),reads,writes});}
