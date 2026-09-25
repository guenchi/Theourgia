'use strict';
const fs=require('fs'),path=require('path');
const {nodeFileOps}=require('../../src/fsops');
const {activateCore}=require('../../src/activate');
const {Publisher}=require('../../src/publication');
const {Owners}=require('../../src/ownership');
const [directory,operation,sessionId,hold]=process.argv.slice(2);
const report=value=>fs.writeSync(1,JSON.stringify(value)+'\n');
const pause=()=>{report({kind:'held'});fs.readSync(0,Buffer.alloc(1),0,1,null);};
try {
  if(operation==='activate') {
    const core=activateCore({files:nodeFileOps,globalStorage:directory,documents:{isOpen:()=>false},stores:[],sessionId});
    if(hold==='hold')pause();
    report({kind:'result',status:'ok',identity:core.identity});
  } else if(operation==='owner') {
    report({kind:'result',status:'ok',outcome:new Owners().take(directory,sessionId,[])});
  } else {
    // The block's projection, found as the product finds it (queue item 5).
    const file=new Publisher(nodeFileOps,{isOpen:()=>false}).latestIn(directory)||path.join(directory,'a.1.md');let paused=false;
    const files={...nodeFileOps,readText(p){const result=nodeFileOps.readText(p);if(p===file+'.meta'&&!paused){paused=true;pause();}return result;}};
    const publisher=new Publisher(files,{isOpen:()=>false},{owners:new Owners(files),sessionId});
    report({kind:'result',status:'ok',seq:publisher.takeSequence(file,'held-R')});
  }
} catch(error) {report({kind:'result',status:error.code??'error',error:String(error)});}
