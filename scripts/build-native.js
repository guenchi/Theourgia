'use strict';
const fs=require('fs'),os=require('os'),path=require('path'),{execFileSync}=require('child_process');
const root=path.join(__dirname,'..');
if(!['darwin','linux'].includes(process.platform))throw Error('The process-lock implementation currently requires macOS or Linux');
const candidates=[process.env.THEOURGIA_NODE_HEADERS,path.join(os.homedir(),'Library/Caches/node-gyp',process.versions.node,'include/node'),
  path.join(os.homedir(),'.cache/node-gyp',process.versions.node,'include/node'),'/usr/local/include/node','/usr/include/node'].filter(Boolean);
const include=candidates.find(p=>fs.existsSync(path.join(p,'node_api.h')));
if(!include)throw Error('Install Node N-API headers or set THEOURGIA_NODE_HEADERS to their include directory');
const output=path.join(root,'out/src/native/lease.node');fs.mkdirSync(path.dirname(output),{recursive:true});
execFileSync(process.env.CC||'cc',['-O2','-Wall','-Wextra','-Werror','-DNAPI_VERSION=3','-DNODE_GYP_MODULE_NAME=lease','-I',include,
  ...process.platform==='darwin'?['-bundle','-undefined','dynamic_lookup']:['-shared','-fPIC'],path.join(root,'native/lease.c'),'-o',output],{stdio:'inherit'});
process.stdout.write('build-native: N-API process lock compiled\n');
