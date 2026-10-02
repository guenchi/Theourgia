'use strict';

// The gate npm runs before it publishes (prepublishOnly). It checks and
// never fixes: each refusal is named, and the first one found stops the
// publish with exit 1.
//
//   submodule-not-checked-out  theourgia/ holds no checkout
//   submodule-moved            theourgia/ is at a commit other than the one
//                              this branch records for it
//   submodule-dirty            theourgia/ has changes or untracked files
//   branch-dirty               this branch has changes or untracked files
//   compiled-object            a .so is somewhere in the package's tree
//   file-list-differs          the list npm would pack is not
//                              test/expected-files.txt

const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const root = path.resolve(process.argv[2] || path.join(__dirname, '..'));

function refuse(name, detail) {
  process.stderr.write('prepublish: refused: ' + name + ': ' + detail + '\n');
  process.exit(1);
}

// -> { status, out, err, raw }: out trimmed, raw as git printed it (the
// submodule status line's first character is its mark, a space when clean).
function git(args, cwd) {
  const r = spawnSync('git', args, { cwd: cwd || root, encoding: 'utf8' });
  return { status: r.status, out: (r.stdout || '').trim(), err: (r.stderr || '').trim(), raw: r.stdout || '' };
}

// The submodule's line in `git submodule status`: '-' not initialised, '+'
// at a commit other than the recorded one, 'U' in conflict, ' ' as recorded.
const status = git(['submodule', 'status', '--', 'theourgia']);
const mark = status.raw.length > 0 ? status.raw[0] : '-';
if (status.status !== 0 || mark === '-' || !fs.existsSync(path.join(root, 'theourgia', 'theourgia.sc'))) {
  refuse('submodule-not-checked-out', 'theourgia/ holds no checkout; run git submodule update --init');
}
if (mark !== ' ') {
  const recorded = git(['ls-tree', 'HEAD', 'theourgia']).out.split(/\s+/)[2] || '?';
  const actual = git(['rev-parse', 'HEAD'], path.join(root, 'theourgia')).out || '?';
  refuse('submodule-moved', 'theourgia/ is at ' + actual + ', the branch records ' + recorded);
}
const subDirty = git(['status', '--porcelain'], path.join(root, 'theourgia'));
if (subDirty.status !== 0 || subDirty.out.length > 0) {
  refuse('submodule-dirty', (subDirty.out.split('\n')[0] || subDirty.err) + (subDirty.out.includes('\n') ? ' ...' : ''));
}
const dirty = git(['status', '--porcelain', '--ignore-submodules=none']);
if (dirty.status !== 0 || dirty.out.length > 0) {
  refuse('branch-dirty', (dirty.out.split('\n')[0] || dirty.err) + (dirty.out.includes('\n') ? ' ...' : ''));
}

function findObjects(dir, rel, out) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    if (e.name === '.git' || e.name === 'node_modules') continue;
    const r = rel ? rel + '/' + e.name : e.name;
    if (e.isDirectory()) findObjects(path.join(dir, e.name), r, out);
    else if (e.name.endsWith('.so')) out.push(r);
  }
  return out;
}
const objects = findObjects(root, '', []);
if (objects.length > 0) refuse('compiled-object', objects.slice(0, 3).join(' ') + (objects.length > 3 ? ' ...' : ''));

const pack = spawnSync('npm', ['pack', '--dry-run', '--json', '--ignore-scripts'], { cwd: root, encoding: 'utf8' });
let packed;
try {
  packed = JSON.parse(pack.stdout)[0].files.map((f) => f.path);
} catch (e) {
  refuse('file-list-differs', 'npm pack --dry-run did not answer a file list (rc ' + pack.status + ')');
}
// source.json is written by prepack, which --ignore-scripts skips here; it
// is listed in `files` by name, so npm packs it whenever it exists.
if (!packed.includes('source.json')) packed.push('source.json');
packed.sort();
const expected = fs.readFileSync(path.join(root, 'test', 'expected-files.txt'), 'utf8').split('\n').filter((l) => l.length > 0);
const missing = expected.filter((f) => !packed.includes(f));
const extra = packed.filter((f) => !expected.includes(f));
if (missing.length > 0 || extra.length > 0) {
  refuse('file-list-differs', 'missing ' + JSON.stringify(missing.slice(0, 5)) + ', extra ' + JSON.stringify(extra.slice(0, 5)));
}
process.stderr.write('prepublish: the submodule at the recorded commit, both trees clean, no .so, ' +
  packed.length + ' files as expected\n');
