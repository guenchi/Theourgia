'use strict';

// Run by npm before it packs (prepack): writes source.json, the commit of
// the theourgia submodule the package is made from. An installed package
// has no git, and the compile cache's key needs to know which sources the
// objects were built from. It refuses, by name, when the submodule is not
// checked out; it writes nothing else.

const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const root = path.resolve(__dirname, '..');

const rev = spawnSync('git', ['-C', path.join(root, 'theourgia'), 'rev-parse', 'HEAD'], { encoding: 'utf8' });
const commit = (rev.stdout || '').trim();
if (rev.status !== 0 || !/^[0-9a-f]{40}$/.test(commit) ||
    !fs.existsSync(path.join(root, 'theourgia', 'theourgia.sc'))) {
  process.stderr.write('source-pin: refused: submodule-not-checked-out: theourgia/ holds no checkout; ' +
    'run git submodule update --init\n');
  process.exit(1);
}
fs.writeFileSync(path.join(root, 'source.json'), JSON.stringify({ theourgia: commit }) + '\n');
process.stderr.write('source-pin: theourgia ' + commit + '\n');
