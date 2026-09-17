'use strict';
const fs=require('fs'),path=require('path');
const product=path.join(__dirname,'../../src');
const {nodeFileOps}=require(path.join(product,'fsops'));
const {Publisher}=require(path.join(product,'publication'));
const {Saving}=require(path.join(product,'saving'));
const [root,mode,body,stop]=process.argv.slice(2),directory=path.join(root,'block'),file=path.join(directory,'current.md');
const trace=[];let boundary=0;
function point(op,phase,args){
  trace.push({op,phase,args});boundary++;
  if(boundary===Number(stop)){fs.writeFileSync(path.join(root,'stopped.json'),JSON.stringify(trace));process.kill(process.pid,'SIGKILL');}
}
const files={...nodeFileOps};
for(const op of ['writeDurably','rename','syncDirectory','unlink'])files[op]=(...args)=>{point(op,'before',args.slice(0,2));const result=nodeFileOps[op](...args);point(op,'after',args.slice(0,2));return result;};
async function main(){
 const publisher=new Publisher(files,{isOpen:()=>false,isDirty:()=>false});
 let answer;
 if(mode==='replace')answer=await publisher.publish({directory,storeId:'store',blockId:'a.1',prefix:'# A\n',text:body,cursor:null});
 else answer=publisher.recoverCurrent(file);
 const sidecar=publisher.sidecarOf(file),bytes=fs.existsSync(file)?fs.readFileSync(file,'utf8'):null;
 const decision=new Saving(files).decide({file,isDirty:false,getText:()=>bytes},sidecar);
 fs.writeSync(1,JSON.stringify({complete:true,trace,answer,sidecar,bytes,decision})+'\n');
}
main().catch(e=>{fs.writeSync(2,String(e.stack));process.exitCode=1;});
