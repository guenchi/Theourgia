'use strict';
const fs=require('fs'),path=require('path');
const {Outbox}=require(path.join(__dirname,'../../src/outbox'));
const {nodeFileOps}=require(path.join(__dirname,'../../src/fsops'));
const [file,operation,gate]=process.argv.slice(2);
let paused=false;
const files={...nodeFileOps,readText(p){const text=nodeFileOps.readText(p);if(p===file&&gate==='hold'&&!paused){paused=true;fs.writeSync(1,JSON.stringify({kind:'read'})+'\n');fs.readSync(0,Buffer.alloc(1),0,1,null);}return text;}};
const box=new Outbox(file,files);
const entry=req=>({req,cursor:'w:1',id:'a.1',field:'src',payload:req,state:'queued',createdAt:1,lastError:null,importedBy:null});
try{
 const calls={enqueue:()=>box.enqueue(entry('A')),enqueueB:()=>box.enqueue(entry('B')),clearCursor:()=>box.clearCursor(),setCursor:()=>box.setCursor('w:9'),
  resolve:()=>box.resolve('R1','w:9'),aboutToSend:()=>box.aboutToSend('R1','w:9'),markImported:()=>box.markImported('R1','claim.1'),markParked:()=>box.markParked('R1','parked'),markPending:()=>box.markPending('R1','pending'),
  unparkAll:()=>box.unparkAll()};
 calls[operation]();fs.writeSync(1,JSON.stringify({kind:'result',complete:true,status:'ok'})+'\n');
}catch(e){fs.writeSync(1,JSON.stringify({kind:'result',complete:true,status:e.code==='RESOURCE_BUSY'?'busy':'error',error:String(e)})+'\n');}
