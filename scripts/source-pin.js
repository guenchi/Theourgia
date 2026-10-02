'use strict';

// Run by npm before it packs (prepack): writes source.json, the commit of
// the theourgia submodule the package is made from. An installed package
// has no git, and the compile cache's key needs to know which sources the
// objects were built from. It refuses, by name, when the submodule is not
// checked out or has changes of its own (the commit would not describe the
// files packed); it writes nothing else. The file is written beside and
// renamed into place, so a symbolic link already at that name is replaced,
// never written through.

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
const dirty = spawnSync('git', ['-C', path.join(root, 'theourgia'), 'status', '--porcelain', '--untracked-files=all'],
  { encoding: 'utf8' });
if (dirty.status !== 0 || (dirty.stdout || '').trim().length > 0) {
  process.stderr.write('source-pin: refused: submodule-dirty: theourgia/ has changes; the commit would not describe ' +
    'the files packed\n');
  process.exit(1);
}
const target = path.join(root, 'source.json');
const tmp = target + '.' + process.pid + '.tmp';
fs.writeFileSync(tmp, JSON.stringify({ theourgia: commit }) + '\n');
fs.renameSync(tmp, target);
process.stderr.write('source-pin: theourgia ' + commit + '\n');
