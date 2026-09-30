/*
 * Copyright 2018 - 2026 guenchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * THE STORE AS FILES.
 *
 * The tree view has a second mode that shows the directory tree export
 * would write, derived from the blocks' `path` field; a directory is only
 * a prefix of some file's path. These cells pin which blocks are files,
 * the grouping and its order, the blocks in no file, the enumeration from
 * the whole outline, the mode and its keeping, the actions -- a new
 * document is one `batch` through the Saver, a move or a rename one write
 * of `path` -- and, in the harness with a real core, the agreement with
 * what export writes.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { spawnSync } from 'child_process';
import { Block, readBlock } from '../../src/blocks';
import { Client } from '../../src/client';
import {
  CHANGED_SINCE_LISTED,
  Candidate,
  DUPLICATE_REFUSED,
  DirectoryEntry,
  FilesRow,
  NOT_EXPORTABLE,
  PATH_NOT_A_FILE,
  PATH_TAKEN,
  candidateOf,
  changedSinceRead,
  exportablePath,
  filesByDefault,
  filesTree,
  movedPath,
  newDocumentIntent,
  newDocumentPath,
  pathActionNotice,
  renamedPath,
  rowsOf
} from '../../src/directory-view';
import { StoreModel } from '../../src/model';
import { recordFor } from '../../src/record';
import { Outbox, OutboxWriteError } from '../../src/outbox';
import { Saver, Settle } from '../../src/saver';
import { CliTransport, RawResult } from '../../src/transport';
import { initWire, parseAnswers } from '../../src/wire';
import { FakeCore, ScriptedCall } from '../support/fake';
import { IGNORED_DURABILITY } from '../support/ignored-durability';

function record(id: string, fields: string, position = 'root . 0'): string {
  return `((id . "${id}") (deleted . #f) (fields ${fields}) (position ${position}) (edges))`;
}

function blockOf(text: string): Block {
  const block = readBlock(parseAnswers(`${text}\n`)[0]);
  assert.ok(block !== null, `not a block: ${text}`);
  return block as Block;
}

/*
 * A CANDIDATE AS THE LISTING MAKES ONE. A block at the root whose position
 * names a parent is an orphan: listed at the root, and not a root export
 * takes.
 */
function candidate(id: string, fields: string, atRoot: boolean, position?: string): Candidate {
  return candidateOf(blockOf(record(id, fields, position)), id.toUpperCase(), atRoot, atRoot && position === undefined);
}

/*
 * THE FIXTURE OF THE DESIGN'S ROW V1, one block per case, in outline order.
 */
function fixture(): Candidate[] {
  return [
    candidate('s1', '(kind . section) (title . "Holder")', true),
    candidate('r1', '(kind . doc) (path . "README.md")', true),
    candidate('d1', '(kind . doc) (path . "docs/a.md")', true),
    candidate('d2', '(kind . doc) (path . "docs/b/c.md")', true),
    candidate('f1', '(kind . file) (mode . text) (path . "src/x.scm")', true),
    candidate('l1', '(kind . library) (mode . datum) (path . "src/lib.sls")', true),
    candidate('f2', '(kind . file) (mode . text) (path . "src/y.scm")', false, '"s1" . 0'),
    candidate('s2', '(kind . section) (path . "notes/a.md")', true),
    candidate('o1', '(kind . section) (title . "Orphan")', true, '"gone.1" . 0'),
    candidate('o2', '(kind . doc) (path . "docs/orphan.md")', true, '"gone.2" . 0'),
    candidate('d3', '(kind . doc) (path . "../a.md")', true),
    candidate('d4', '(kind . doc) (path . "docs//e.md/")', true),
    candidate('d6', '(kind . doc) (path . "docs/dup.md")', true),
    candidate('d5', '(kind . doc) (path . "docs/dup.md")', true),
    candidate('f3', '(kind . file) (mode . text) (path . "src/dup.scm")', true),
    candidate('f4', '(kind . file) (mode . text) (path . "src/dup.scm")', true),
    candidate('l2', '(kind . library) (mode . text) (path . "src/l2.sls")', true),
    candidate('n1', '(kind . doc) (path . "docs/n.md")', false, '"d1" . 0')
  ];
}

/*
 * ONE LEVEL AS IT IS DRAWN: a directory by its name, a file by its label,
 * its block and what export does with it, the group by the blocks in it.
 */
function drawn(rows: FilesRow[]): string[] {
  return rows.map((row) =>
    'directory' in row
      ? `dir ${row.directory.name} (${row.directory.count})`
      : 'file' in row
        ? `file ${row.file.label} ${row.file.id}${row.file.note === null ? '' : ` [${row.file.note}]`}`
        : `group ${row.pathless.map((p) => p.id).join(' ')}`
  );
}

function directory(root: DirectoryEntry, prefix: string): DirectoryEntry {
  const found = (d: DirectoryEntry): DirectoryEntry | null =>
    d.prefix === prefix ? d : d.directories.map(found).find((x) => x !== null) ?? null;
  const d = found(root);
  assert.ok(d !== null, `no directory ${prefix}`);
  return d as DirectoryEntry;
}

describe('V1 the files are what export writes, grouped by directory, and nothing is hidden', () => {
  before(async () => {
    await initWire();
  });

  it('takes the exporter path rule as it is, without tidying a path', () => {
    assert.deepStrictEqual(
      ['docs/a.md', 'a.md', '../a.md', '/a.md', 'docs//e.md/', 'docs/./a.md', '', 'a\\b.md', 'a\u0000.md'].map(exportablePath),
      [true, true, false, false, false, false, false, false, false]
    );
  });

  it('draws directories before files at every level, sorted, with the counts below each', () => {
    const tree = filesTree(fixture());
    assert.deepStrictEqual(drawn(rowsOf(tree.root, tree.pathless)), [
      'dir docs (4)',
      'dir src (5)',
      'file README.md r1',
      'group s1 s2 o1 o2 d3 d4 l2'
    ]);
    assert.strictEqual(tree.root.count, 10);
    assert.deepStrictEqual(drawn(rowsOf(directory(tree.root, 'docs'))), [
      'dir b (1)',
      'file a.md d1',
      `file dup.md d5`,
      `file dup.md d6 [${PATH_TAKEN}]`
    ]);
    assert.deepStrictEqual(drawn(rowsOf(directory(tree.root, 'docs/b'))), ['file c.md d2']);
    /*
     * The two text files at one path make export-code refuse the whole code
     * export, so every text file says so; the library is another family.
     */
    assert.deepStrictEqual(drawn(rowsOf(directory(tree.root, 'src'))), [
      `file dup.scm f3 [${DUPLICATE_REFUSED}]`,
      `file dup.scm f4 [${DUPLICATE_REFUSED}]`,
      'file lib.sls l1',
      `file x.scm f1 [${DUPLICATE_REFUSED}]`,
      `file y.scm f2 [${DUPLICATE_REFUSED}]`
    ]);
  });

  it('labels a file by the last segment of its path and keeps the title beside it', () => {
    const tree = filesTree(fixture());
    const c = directory(tree.root, 'docs/b').files[0];
    assert.deepStrictEqual([c.label, c.path, c.title], ['c.md', 'docs/b/c.md', 'D2']);
  });

  it('keeps a file block below a section, and leaves a nested document and a pathless child out', () => {
    const tree = filesTree(fixture());
    assert.ok(directory(tree.root, 'src').files.some((f) => f.id === 'f2'), 'the file under a section is missing');
    const everywhere = JSON.stringify(tree);
    assert.ok(!everywhere.includes('"n1"'), 'a nested document was listed');
  });

  it('says why a block with a path is in no file, with the path as stored', () => {
    const tree = filesTree(fixture());
    const why = Object.fromEntries(tree.pathless.map((p) => [p.id, [p.note, p.raw]]));
    assert.deepStrictEqual(why, {
      s1: [null, null],
      s2: [PATH_NOT_A_FILE, 'notes/a.md'],
      o1: [null, null],
      o2: [PATH_NOT_A_FILE, 'docs/orphan.md'],
      d3: [NOT_EXPORTABLE, '../a.md'],
      d4: [NOT_EXPORTABLE, 'docs//e.md/'],
      l2: [PATH_NOT_A_FILE, 'src/l2.sls']
    });
  });

  it('takes only a string path, as the exporters do: bytes and a conflict are not paths, the name "conflict" is', () => {
    const bytes = Array.from(Buffer.from('docs/\u4e2d.md', 'utf8')).join(' ');
    const held = candidateOf(blockOf(record('b1', `(kind . doc) (path . #vu8(${bytes}))`)), 'B', true, true);
    assert.deepStrictEqual([held.path, held.pathProblem, held.raw], [null, 'not-text', 'docs/\u4e2d.md']);
    const conflicted = candidateOf(blockOf(record('b2', '(kind . doc) (path conflict ("a.md" "b.md"))')), 'B', true, true);
    assert.deepStrictEqual([conflicted.path, conflicted.pathProblem], [null, 'conflict']);
    const named = candidateOf(blockOf(record('b3', '(kind . doc) (path . "conflict")')), 'B', true, true);
    const tree = filesTree([held, conflicted, named]);
    assert.deepStrictEqual(tree.pathless.map((p) => [p.id, p.note, p.raw]), [
      ['b1', NOT_EXPORTABLE, 'docs/\u4e2d.md'],
      ['b2', 'path in conflict', null]
    ]);
    assert.deepStrictEqual(tree.root.files.map((f) => [f.id, f.path, f.note]), [['b3', 'conflict', null]]);
  });

  it('takes kind and mode as symbols only, as the exporters compare them', () => {
    const spelled = candidate('b4', '(kind . "doc") (path . "a.md")', true);
    assert.deepStrictEqual([spelled.kind, filesTree([spelled]).root.files.length], [null, 0]);
    assert.deepStrictEqual(filesTree([spelled]).pathless.map((p) => p.note), [PATH_NOT_A_FILE]);
    const moded = candidate('b5', '(kind . file) (mode . "text") (path . "x.scm")', true);
    assert.deepStrictEqual([moded.mode, filesTree([moded]).root.files.length], [null, 0]);
  });
});

/*
 * A FAMILY OF FILES IS WRITTEN WHOLE OR NOT AT ALL. export-code builds every
 * text file before writing one, and export-code --datum every library;
 * either refuses the whole family on one bad member. A row per cause.
 */
describe('V1 text files and libraries are each written as a family, or none is', () => {
  before(async () => {
    await initWire();
  });

  const good = () => candidate('g1', '(kind . file) (mode . text) (path . "src/good.scm")', true);
  const noted = (cs: Candidate[], id: string): string | null | undefined => {
    const find = (d: DirectoryEntry): string | null | undefined => {
      const here = d.files.find((f) => f.id === id);
      return here !== undefined ? here.note : d.directories.map(find).find((n) => n !== undefined);
    };
    return find(filesTree(cs).root);
  };

  it('writes the text family when every member has a safe path and text-mode code children with bytes', () => {
    const cs = [good(), candidate('c1', '(kind . code) (mode . text) (src . #vu8(59 10))', false, '"g1" . 0')];
    assert.strictEqual(noted(cs, 'g1'), null);
  });

  it('refuses every text file when one text file, anywhere, has a path export refuses', () => {
    const cs = [good(), candidate('b1', '(kind . file) (mode . text) (path . "../bad.scm")', false, '"s1" . 0')];
    assert.strictEqual(noted(cs, 'g1'), 'export refused: a text file with no path or one export refuses');
  });

  it('refuses every text file when one text file has no path at all', () => {
    const cs = [good(), candidate('b1', '(kind . file) (mode . text) (title . "loose")', true)];
    assert.strictEqual(noted(cs, 'g1'), 'export refused: a text file with no path or one export refuses');
  });

  it('refuses every text file when one has a child that is not a text-mode code block with bytes', () => {
    const section = [good(), candidate('c1', '(kind . section) (title . "S")', false, '"g1" . 0')];
    const asText = [good(), candidate('c1', '(kind . code) (mode . text) (src . "x")', false, '"g1" . 0')];
    for (const cs of [section, asText]) {
      assert.strictEqual(noted(cs, 'g1'), 'export refused: a block under a text file that is not a text-mode code block');
    }
  });

  it('refuses every library when two libraries share a path, or a library holds a child that is not datum code', () => {
    const lib = (id: string, p: string) => candidate(id, `(kind . library) (mode . datum) (path . "${p}")`, true);
    assert.strictEqual(noted([lib('l1', 'a.sls'), lib('l2', 'a.sls'), lib('l3', 'b.sls')], 'l3'), DUPLICATE_REFUSED);
    const child = candidate('c1', '(kind . code) (mode . text) (src . #vu8(1))', false, '"l3" . 0');
    assert.strictEqual(noted([lib('l3', 'b.sls'), child], 'l3'), 'export refused: a block under a library that is not a datum code block');
  });

  it("counts no block the outline puts at the root as a file's child, and no deleted block at all", () => {
    const promoted = candidate('c1', '(kind . section) (title . "cycle")', true, '"g1" . 0');
    assert.strictEqual(noted([good(), promoted], 'g1'), null, "a cycle's block was taken for the file's child");
    const tomb = candidateOf(
      blockOf('((id . "b1") (deleted . #t) (fields (kind . file) (mode . text) (path . "../bad.scm")) (position root . 0) (edges))'),
      'B1',
      true,
      true
    );
    assert.strictEqual(noted([good(), tomb], 'g1'), null, 'a deleted text file refused the family');
    const drawnTree = filesTree([good(), tomb]);
    assert.deepStrictEqual(
      [drawnTree.root.count, drawnTree.pathless.map((p) => p.id)],
      [1, []],
      'a deleted block was drawn'
    );
  });

  it('gives the reason the exporter meets first: a member\'s children before its path', () => {
    const both = candidate('b1', '(kind . file) (mode . text) (path . "../bad.scm")', true);
    const child = candidate('c1', '(kind . section) (title . "S")', false, '"b1" . 0');
    assert.strictEqual(noted([good(), both, child], 'g1'), 'export refused: a block under a text file that is not a text-mode code block');
  });

  it('shows a library whose child\'s doc the datum exporter would refuse as written: not modelled, by contract', () => {
    const lib = candidate('l1', '(kind . library) (mode . datum) (path . "a.sls")', true);
    const child = candidate('c1', '(kind . code) (mode . datum) (doc . ";;; @block x\\n")', false, '"l1" . 0');
    assert.strictEqual(noted([lib, child], 'l1'), null);
  });

  it('leaves the other families and the documents alone when one family is refused', () => {
    const cs = [
      good(),
      candidate('b1', '(kind . file) (mode . text) (path . "../bad.scm")', true),
      candidate('l1', '(kind . library) (mode . datum) (path . "a.sls")', true),
      candidate('d1', '(kind . doc) (path . "a.md")', true)
    ];
    assert.deepStrictEqual([noted(cs, 'l1'), noted(cs, 'd1')], [null, null]);
  });
});

describe('V1 the files are what export writes (continued)', () => {
  before(async () => {
    await initWire();
  });

  it('lists an orphan document with a path in no file: export-md takes only documents under the root', () => {
    const orphan = candidate('o2', '(kind . doc) (path . "docs/orphan.md")', true, '"gone.2" . 0');
    assert.deepStrictEqual([orphan.atRoot, orphan.exportRoot], [true, false]);
    const tree = filesTree([orphan]);
    assert.deepStrictEqual([tree.root.count, tree.pathless.map((p) => p.id)], [0, ['o2']]);
  });
});

/*
 * A CLIENT THAT ANSWERS FROM A TABLE, keyed by the request's words.
 */
function answering(table: Record<string, string>, sent: string[][]): Client {
  return new Client({
    kind: 'test',
    send: async (verb: string, args: string[]): Promise<RawResult> => {
      sent.push([verb, ...args]);
      const key = [verb, ...args].join(' ');
      const stdout = table[key];
      if (stdout === undefined) {
        return { argv: [verb, ...args], rc: 1, stdout: `(error unknown-verb ${verb})\n`, stderr: '' };
      }
      return { argv: [verb, ...args], rc: 0, stdout, stderr: '' };
    }
  });
}

describe('V1 the enumeration reads each root with its whole subtree, once', () => {
  before(async () => {
    await initWire();
  });

  it('finds a file block under a section nobody has opened, and reads no root twice', async () => {
    const sent: string[][] = [];
    const client = answering(
      {
        'outline --depth 1 --wire': '(ok (text "- s1  Holder\\n- r1  Readme\\n"))\n',
        conflicts: '',
        'read s1 --recursive --wire':
          `${record('s1', '(kind . section) (title . "Holder")')}\n` +
          `${record('f2', '(kind . file) (mode . text) (path . "src/y.scm") (title . "y")', '"s1" . 0')}\n`,
        'read r1 --recursive --wire': `${record('r1', '(kind . doc) (path . "README.md") (title . "Readme")')}\n`
      },
      sent
    );
    const reading = await new StoreModel(client).filesReading();
    assert.deepStrictEqual(
      reading.blocks.map((b) => [b.block.id, b.atRoot]),
      [
        ['s1', true],
        ['r1', true],
        ['f2', false]
      ]
    );
    assert.deepStrictEqual(reading.listing.nodes.map((n) => n.id), ['s1', 'r1']);
    const tree = filesTree(reading.blocks.map((b) => candidateOf(b.block, b.node.title, b.atRoot, b.exportRoot)));
    assert.deepStrictEqual(drawn(rowsOf(tree.root, tree.pathless)), ['dir src (1)', 'file README.md r1', 'group s1']);
    assert.deepStrictEqual(
      sent.filter((r) => r[0] === 'read'),
      [
        ['read', 's1', '--recursive', '--wire'],
        ['read', 'r1', '--recursive', '--wire']
      ],
      'the listing did not read each root once, with its subtree'
    );
  });
});

describe('V1 export roots come from the model, not from a helper', () => {
  before(async () => {
    await initWire();
  });

  it('reads an orphan document as listed at the root and not an export root', async () => {
    const sent: string[][] = [];
    const client = answering(
      {
        'outline --depth 1 --wire': '(ok (text "- o1  O\\n- t1  T\\n"))\n',
        conflicts: '(orphan "o1")\n',
        'read o1 --recursive --wire': `${record('o1', '(kind . doc) (path . "o.md") (title . "O")', '"gone.1" . 0')}\n`,
        'read t1 --recursive --wire': `${record('t1', '(kind . doc) (path . "t.md") (title . "T")')}\n`
      },
      sent
    );
    const reading = await new StoreModel(client).filesReading();
    assert.deepStrictEqual(
      reading.blocks.map((b) => [b.block.id, b.atRoot, b.exportRoot]),
      [
        ['o1', true, false],
        ['t1', true, true]
      ]
    );
    const tree = filesTree(reading.blocks.map((b) => candidateOf(b.block, b.node.title, b.atRoot, b.exportRoot)));
    assert.deepStrictEqual([tree.root.files.map((f) => f.id), tree.pathless.map((p) => p.id)], [['t1'], ['o1']]);
  });
});

describe('V3 the mode a store opens in', () => {
  before(async () => {
    await initWire();
  });

  it('is Files when a root carries a path and Outline when none does', () => {
    assert.strictEqual(filesByDefault(fixture()), true);
    assert.strictEqual(filesByDefault([candidate('s1', '(kind . section)', true)]), false);
    assert.strictEqual(
      filesByDefault([candidate('f2', '(kind . file) (mode . text) (path . "x.scm")', false, '"s1" . 0')]),
      false,
      'a path below the root decided the mode'
    );
  });
});

describe('V4 the paths an action writes', () => {
  it('names a new document in its directory, adds .md, and refuses a name that is not one segment', () => {
    assert.strictEqual(newDocumentPath('docs/b', 'notes'), 'docs/b/notes.md');
    assert.strictEqual(newDocumentPath('', 'notes.md'), 'notes.md');
    assert.strictEqual(newDocumentPath('docs', 'a/b'), null);
    assert.strictEqual(newDocumentPath('docs', '..'), 'docs/...md');
    assert.strictEqual(newDocumentPath('docs', '  '), null);
  });

  it('moves a file to another directory under its own name, and renames it in its own', () => {
    assert.strictEqual(movedPath('src/x.scm', 'lib/'), 'lib/x.scm');
    assert.strictEqual(movedPath('src/x.scm', ''), 'x.scm');
    assert.strictEqual(movedPath('src/x.scm', '../up'), null);
    assert.strictEqual(renamedPath('src/x.scm', 'y.scm'), 'src/y.scm');
    assert.strictEqual(renamedPath('x.scm', 'a/b'), null);
  });
});

function settling(outbox: Outbox): Settle {
  return (req, settlement) => {
    if (settlement.verdict === 'req-mismatch') {
      return;
    }
    outbox.resolve(req, settlement.verdict === 'confirmed' ? settlement.cursor : null);
  };
}

const CHECK = '(check (store "s") (writers (("w" (end 7) (torn #f) (integrity ())))) (snapshots ()) (registry outside-store) (verdict ok))\n';

/*
 * A BATCH OF ONE INSERT THE CORE TOOK. Sequence 40 is block `w.14` in the
 * core's naming, base 36; a reader that wrote the number in base ten would
 * name `w.40`.
 */
const TOOK = '(batch ((ok (events (("w" . 40))) (state (("w.14" . "hhh"))) (cursor ("w" . 40)) (replay #f))) (done 1))\n';

interface Rig {
  core: FakeCore;
  outbox: Outbox;
  saver: Saver;
}

function rig(calls: ScriptedCall[], outboxFile?: string): Rig {
  const core = new FakeCore([{ match: ['check'], stdout: CHECK, rc: 0 }, ...calls]);
  const outbox = new Outbox(outboxFile ?? core.outboxFile());
  outbox.load();
  const client = new Client(new CliTransport(core.config(), core.env()));
  return { core, outbox, saver: new Saver(client, outbox, settling(outbox), IGNORED_DURABILITY) };
}

/*
 * THE BATCHES SENT, each as its whole request: the argument vector and what
 * it put on standard input, where the core's `batch` reads its intents.
 */
function batches(core: FakeCore): Array<{ argv: string[]; input: string | null }> {
  return core.requestsWithInput().filter((r) => r.argv[0] === 'batch');
}

const INTENT = '(insert root #f ((kind . doc) (path . "docs/b/n.md") (title . "n")))';

describe('V4 a new document is one batch through the Saver', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('writes the insert intent with the kind, the path and the title', () => {
    assert.strictEqual(newDocumentIntent('docs/b/n.md', 'n'), INTENT);
    assert.strictEqual(
      newDocumentIntent('a "q".md', 'a "q"'),
      '(insert root #f ((kind . doc) (path . "a \\"q\\".md") (title . "a \\"q\\"")))'
    );
  });

  it('sends it with a request id and the cursor, and settles on done 1 with the new block and cursor', async () => {
    const r = rig([{ match: ['batch'], stdout: TOOK, rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.deepStrictEqual([outcome.status, outcome.id], ['saved', 'w.14']);
    const sent = batches(core);
    assert.strictEqual(sent.length, 1);
    assert.deepStrictEqual([sent[0].argv[1], sent[0].argv[3], sent[0].argv[4]], ['--req', '--cursor', 'w:7']);
    assert.ok(/^[0-9a-f-]{36}$/.test(sent[0].argv[2]), `the request id is not a uuid: ${sent[0].argv[2]}`);
    assert.strictEqual(sent[0].input, INTENT, 'the intents were not sent on standard input');
    assert.ok(!sent[0].argv.includes(INTENT), 'the intents went as an argument as well, which the core answers with its usage line');
    assert.deepStrictEqual(
      core.requestsWithInput().filter((q) => q.argv[0] !== 'batch').map((q) => q.input),
      [null],
      'a request other than the batch sent standard input'
    );
    assert.deepStrictEqual([r.outbox.pendingCount, r.outbox.cursor], [0, 'w:40']);
  });

  it('settles a replay on the receipt', async () => {
    const r = rig([{ match: ['batch'], stdout: '(batch ((ok (replay #t) (event ("w" . 40)))))\n', rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.strictEqual(outcome.status, 'replayed');
    assert.deepStrictEqual([r.outbox.pendingCount, r.outbox.cursor], [0, 'w:40']);
  });

  it('keeps the request when an answer without a count is not the replay receipt', async () => {
    const r = rig([{ match: ['batch'], stdout: '(batch ((ok (cursor ("w" . 40)) (replay #f))))\n', rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.strictEqual(outcome.status, 'pending');
    assert.match(outcome.message, /no count and no replay receipt/);
    assert.deepStrictEqual([r.outbox.pendingCount, r.outbox.cursor], [1, 'w:7']);
  });

  it('keeps the request when the receipt\'s event is not an event, whatever else the answer holds', async () => {
    const r = rig([{ match: ['batch'], stdout: '(batch ((ok (replay #t) (event #f) (cursor ("w" . 40)))))\n', rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.strictEqual(outcome.status, 'pending');
    assert.deepStrictEqual([r.outbox.pendingCount, r.outbox.cursor], [1, 'w:7']);
  });

  it('takes one or more insert intents only, and queues nothing otherwise', async () => {
    const r = rig([]);
    core = r.core;
    await assert.rejects(r.saver.batch('(set "a.1" title "x")'), /insert intents and nothing else/);
    await assert.rejects(r.saver.batch(''), /insert intents and nothing else/);
    assert.deepStrictEqual([r.outbox.pendingCount, batches(core).length], [0, 0]);
  });

  it('keeps the request when the count is not the number of intents', async () => {
    const r = rig([{ match: ['batch'], stdout: TOOK.replace('(done 1)', '(done 0)'), rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.strictEqual(outcome.status, 'pending');
    assert.match(outcome.message, /done 0 with 1 items for 1 intents/);
    assert.deepStrictEqual([r.outbox.pendingCount, r.outbox.cursor], [1, 'w:7']);
  });

  it('reads a declined intent as the refusal it is, by its name', async () => {
    const r = rig([{ match: ['batch'], stdout: '(batch ((error doc-must-be-top-level (parent "x.1"))) (done 0))\n', rc: 1 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.strictEqual(outcome.status, 'refused');
    assert.match(outcome.message, /doc-must-be-top-level/);
    assert.strictEqual(r.outbox.pendingCount, 0);
  });
});

describe('V7 the batch entry in the queue', () => {
  let core: FakeCore;
  before(async () => {
    await initWire();
  });
  afterEach(() => core?.dispose());

  it('survives a restart and is sent again whole', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-batch-')), 'outbox.json');
    const r = rig([{ match: ['batch'], exitWithoutAnswer: true, once: true }, { match: ['batch'], stdout: TOOK, rc: 0 }], file);
    core = r.core;
    assert.strictEqual((await r.saver.batch(INTENT)).status, 'pending');
    const restarted = new Outbox(file);
    restarted.load();
    assert.deepStrictEqual(
      restarted.entries.map((e) => [e.verb, e.payload, e.id, e.field, e.cursor, e.state]),
      [['batch', INTENT, '', '', 'w:7', 'pending']]
    );
    const saver = new Saver(new Client(new CliTransport(core.config(), core.env())), restarted, settling(restarted), IGNORED_DURABILITY);
    const retried = await saver.retry();
    assert.deepStrictEqual([retried[0].status, retried[0].id], ['saved', 'w.14']);
    const sent = batches(core);
    assert.strictEqual(sent.length, 2);
    assert.deepStrictEqual(sent[1], sent[0], 'the retry did not send the same request');
  });

  it('writes the queue as version 2 only while it holds a batch entry, so an older build refuses it by name', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-batch-')), 'outbox.json');
    const r = rig([{ match: ['batch'], exitWithoutAnswer: true, once: true }, { match: ['batch'], stdout: TOOK, rc: 0 }], file);
    core = r.core;
    assert.strictEqual((await r.saver.batch(INTENT)).status, 'pending');
    assert.strictEqual(JSON.parse(fs.readFileSync(file, 'utf8')).version, 2, 'a queue holding a batch entry is not version 2');
    await r.saver.retry();
    assert.strictEqual(r.outbox.pendingCount, 0);
    assert.strictEqual(JSON.parse(fs.readFileSync(file, 'utf8')).version, 1, 'a queue without one is not version 1');
  });

  it('holds a later save back while its outcome is unknown', async () => {
    const r = rig([
      { match: ['batch'], stdout: '(error unknown (reason schedule))\n', rc: 1 },
      { match: ['set'], stdout: '(ok (events (("w" . 9))) (state (("a.2" . "h"))) (cursor ("w" . 9)) (replay #f))\n', rc: 0 }
    ]);
    core = r.core;
    assert.strictEqual((await r.saver.batch(INTENT)).status, 'pending');
    const later = await r.saver.save('a.2', 'path', 'docs/z.md', 'v1');
    assert.strictEqual(later.status, 'pending');
    assert.deepStrictEqual(core.requests().filter((q) => q[0] === 'set'), [], 'a save went out behind an unknown batch');
  });

  it('settles with the id from the event when the state section is unavailable', async () => {
    const r = rig([
      {
        match: ['batch'],
        stdout: '(batch ((ok (events (("w" . 40))) (state unavailable (reason "r")) (cursor ("w" . 40)) (replay #f))) (done 1))\n',
        rc: 0
      }
    ]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.deepStrictEqual([outcome.status, outcome.id], ['saved', 'w.14']);
  });

  it('keeps the request when the state names a block the event does not', async () => {
    const r = rig([{ match: ['batch'], stdout: TOOK.replace('"w.14"', '"w.40"'), rc: 0 }]);
    core = r.core;
    const outcome = await r.saver.batch(INTENT);
    assert.strictEqual(outcome.status, 'pending');
    assert.match(outcome.message, /names w\.40 in its state and w\.14 by its event/);
  });

  it('refuses to load an entry that sends a verb this build does not know', () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-batch-')), 'outbox.json');
    const entry = { req: 'r', cursor: 'w:1', id: '', field: '', payload: '(x)', state: 'queued', verb: 'move' };
    fs.writeFileSync(file, JSON.stringify({ version: 1, cursor: 'w:1', entries: [entry] }));
    const outbox = new Outbox(file);
    assert.throws(() => outbox.load(), (e: unknown) => e instanceof OutboxWriteError && /verb this build does not know: move/.test((e as Error).message));
  });
});

/*
 * A STORE THAT KEEPS EACH BLOCK'S PATH AND VERSION. A write of the path takes
 * a new version; one sent with `--if-unchanged` naming an older version is
 * refused as `changed`, and one sent without it goes through. `write` is the
 * same write made by somebody else, and `down` makes writes answer nothing.
 */
class PathStore {
  public readonly sent: string[][] = [];
  public down = false;
  private readonly paths = new Map<string, string>();
  private readonly versions = new Map<string, number>();
  private event = 7;

  constructor(id: string, at: string) {
    this.paths.set(id, at);
    this.versions.set(id, 1);
  }

  public add(id: string, at: string): void {
    this.paths.set(id, at);
    this.versions.set(id, 1);
  }

  public pathOf(id: string): string | undefined {
    return this.paths.get(id);
  }

  public write(id: string, at: string): number {
    this.paths.set(id, at);
    this.versions.set(id, (this.versions.get(id) ?? 0) + 1);
    this.event += 1;
    return this.event;
  }

  public client(): Client {
    return new Client({
      kind: 'test',
      send: async (verb: string, args: string[]): Promise<RawResult> => {
        this.sent.push([verb, ...args]);
        const argv = [verb, ...args];
        if (verb === 'check') {
          return { argv, rc: 0, stdout: CHECK, stderr: '' };
        }
        if (verb !== 'set' || args[1] !== 'path') {
          return { argv, rc: 1, stdout: `(error unknown-verb ${verb})\n`, stderr: '' };
        }
        if (this.down) {
          return { argv, rc: 1, stdout: '', stderr: 'the store is not answering' };
        }
        const guard = args.indexOf('--if-unchanged');
        const current = `v${this.versions.get(args[0]) ?? 0}`;
        if (guard >= 0 && args[guard + 1] !== current) {
          return { argv, rc: 1, stdout: `(error changed (current "${current}"))\n`, stderr: '' };
        }
        const n = this.write(args[0], args[2]);
        return {
          argv,
          rc: 0,
          stdout: `(ok (events (("w" . ${n}))) (state (("${args[0]}" . "h"))) (cursor ("w" . ${n})) (replay #f))\n`,
          stderr: ''
        };
      }
    });
  }
}

function guards(store: PathStore): Array<string[] | null> {
  return store.sent
    .filter((q) => q[0] === 'set')
    .map((q) => (q.includes('--if-unchanged') ? q.slice(q.indexOf('--if-unchanged')) : null));
}

describe('a write of the path is conditional on the version it was read at', () => {
  before(async () => {
    await initWire();
  });

  it('reads the version beside the record, and refuses an answer without it', async () => {
    const block = record('a.4', '(kind . file) (mode . text) (path . "src/x.scm") (title . "x")');
    const sent: string[][] = [];
    const read = await new StoreModel(answering({ 'read a.4 --wire': `(ok ${block} (version "h1"))\n` }, sent)).blockWithVersion('a.4');
    assert.deepStrictEqual([read.block?.id, read.version], ['a.4', 'h1']);
    assert.deepStrictEqual(sent, [['read', 'a.4', '--wire']]);
    await assert.rejects(
      new StoreModel(answering({ 'read a.4 --wire': `(ok ${block})\n` }, [])).blockWithVersion('a.4'),
      /without its version/
    );
    await assert.rejects(
      new StoreModel(answering({ 'read a.4 --wire': `(ok ${block} (version h1))\n` }, [])).blockWithVersion('a.4'),
      /without its version/
    );
    await assert.rejects(
      new StoreModel(
        answering({ 'read a.4 --wire': `(ok ${block} (version unavailable (reason "nested past the depth band")))\n` }, [])
      ).blockWithVersion('a.4'),
      (e: unknown) =>
        e instanceof Error &&
        e.message === 'the store cannot version this block (a.4), so its path cannot be changed safely: nested past the depth band'
    );
  });

  it('drains a queued change against the version it was read at, and the store refuses it when the block moved on', async () => {
    const store = new PathStore('a.4', 'src/x.scm');
    const outbox = new Outbox(path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-expect-')), 'outbox.json'));
    outbox.load();
    const saver = new Saver(store.client(), outbox, settling(outbox), IGNORED_DURABILITY);
    store.down = true;
    assert.strictEqual((await saver.save('a.4', 'path', 'lib/x.scm', 'v1')).status, 'pending');
    store.down = false;
    store.write('a.4', 'other/q.scm');
    const [drained] = await saver.retry();
    assert.strictEqual(drained.status, 'refused');
    assert.strictEqual(changedSinceRead(drained), true, drained.message);
    assert.deepStrictEqual(pathActionNotice('moved to', drained, 'lib/x.scm'), { level: 'warning', text: CHANGED_SINCE_LISTED });
    assert.strictEqual(store.pathOf('a.4'), 'other/q.scm', 'the queued write overwrote the change made while it waited');
    assert.deepStrictEqual(guards(store), [
      ['--if-unchanged', 'v1'],
      ['--if-unchanged', 'v1']
    ], 'the version was not sent, or was refreshed when the queue drained');
    assert.strictEqual(outbox.pendingCount, 0);
  });

  it('keeps the version with a queued write through a restart, sends it, and writes the queue as version 2 meanwhile', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-expect-')), 'outbox.json');
    const store = new PathStore('a.4', 'src/x.scm');
    const outbox = new Outbox(file);
    outbox.load();
    store.down = true;
    const saver = new Saver(store.client(), outbox, settling(outbox), IGNORED_DURABILITY);
    assert.strictEqual((await saver.save('a.4', 'path', 'lib/x.scm', 'v1')).status, 'pending');
    const onDisk = JSON.parse(fs.readFileSync(file, 'utf8'));
    /*
     * NOTE: A BUILD FROM BEFORE THE FIELD IS NOT IN THIS TREE; what it does
     * with this file is its version check (src/outbox.ts at 5ab212b, line
     * 798: any version but 1 is refused, naming the version). The file says
     * 2, which is the version that check refuses.
     */
    assert.deepStrictEqual([onDisk.version, onDisk.entries[0].expect], [2, 'v1']);
    const restarted = new Outbox(file);
    restarted.load();
    assert.deepStrictEqual(restarted.entries.map((e) => [e.id, e.payload, e.expect]), [['a.4', 'lib/x.scm', 'v1']]);
    store.down = false;
    const again = new Saver(store.client(), restarted, settling(restarted), IGNORED_DURABILITY);
    const [sent] = await again.retry();
    assert.strictEqual(sent.status, 'saved');
    assert.deepStrictEqual(guards(store).slice(-1), [['--if-unchanged', 'v1']], 'the reloaded write went without its version');
    assert.strictEqual(store.pathOf('a.4'), 'lib/x.scm');
    assert.strictEqual(JSON.parse(fs.readFileSync(file, 'utf8')).version, 1, 'a queue without such an entry is not version 1');
  });

  it('never queues a write of the path without its version, and refuses one found on disk by name', async () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-expect-')), 'outbox.json');
    const store = new PathStore('a.4', 'src/x.scm');
    const outbox = new Outbox(file);
    outbox.load();
    const saver = new Saver(store.client(), outbox, settling(outbox), IGNORED_DURABILITY);
    await assert.rejects(saver.save('a.4', 'path', 'lib/x.scm'), /only with the version it was read at/);
    assert.deepStrictEqual([outbox.pendingCount, store.sent.filter((q) => q[0] === 'set').length], [0, 0]);
    const entry = { req: 'r', cursor: 'w:1', id: 'a.4', field: 'path', payload: 'x', state: 'queued' };
    fs.writeFileSync(file, JSON.stringify({ version: 1, cursor: 'w:1', entries: [entry] }));
    assert.throws(
      () => new Outbox(file).load(),
      (e: unknown) => e instanceof OutboxWriteError && /writes a path without the version it was read at/.test((e as Error).message)
    );
  });

  it('hands a kept write the store refused as stale to the hook, and not the asking write\'s own refusal', async () => {
    const store = new PathStore('a.4', 'src/x.scm');
    store.add('b.1', 'src/b.scm');
    const outbox = new Outbox(path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-stale-')), 'outbox.json'));
    outbox.load();
    const seen: string[][] = [];
    const saver = new Saver(store.client(), outbox, settling(outbox), {
      ...IGNORED_DURABILITY,
      stale: (outcomes) => seen.push(outcomes.map((o) => o.id))
    });
    store.down = true;
    assert.strictEqual((await saver.save('a.4', 'path', 'lib/x.scm', 'v1')).status, 'pending');
    store.down = false;
    store.write('a.4', 'other/q.scm');
    const own = await saver.save('b.1', 'path', 'lib/b.scm', 'v0');
    assert.strictEqual(changedSinceRead(own), true, own.message);
    assert.deepStrictEqual(seen, [['a.4']], 'the kept move drained in front of another save was not handed on, or the asking one was');
    assert.strictEqual(store.pathOf('a.4'), 'other/q.scm');
  });

  it('hands on a stale refusal when the drain ends by throwing after it', async () => {
    const store = new PathStore('a.4', 'src/x.scm');
    store.add('b.1', 'src/b.scm');
    const outbox = new Outbox(path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-stale-')), 'outbox.json'));
    outbox.load();
    const seen: string[][] = [];
    let failOn: string | null = null;
    const settle: Settle = (req, settlement) => {
      if (req === failOn) {
        throw new Error('the answer could not be recorded');
      }
      settling(outbox)(req, settlement);
    };
    const saver = new Saver(store.client(), outbox, settle, {
      ...IGNORED_DURABILITY,
      stale: (outcomes) => seen.push(outcomes.map((o) => o.id))
    });
    store.down = true;
    assert.strictEqual((await saver.save('a.4', 'path', 'lib/x.scm', 'v1')).status, 'pending');
    const second = await saver.save('b.1', 'path', 'lib/b.scm', 'v1');
    assert.strictEqual(second.status, 'pending');
    store.down = false;
    store.write('a.4', 'other/q.scm');
    failOn = second.req;
    await assert.rejects(saver.retry(), /could not be recorded/);
    assert.deepStrictEqual(seen, [['a.4']], 'the stale refusal was lost with the throw');
  });

  it('refuses a queued record that writes the path, and a queued batch of anything but inserts, by name', () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-expect-')), 'outbox.json');
    const record = {
      req: 'r',
      store: '/tmp/store-a',
      storeHash: 'h',
      blockId: 'a.4',
      file: '/tmp/session/a.4/1.md',
      rawDigest: 'raw',
      sentDigest: 'sent',
      prefixDigest: 'prefix',
      seq: 1,
      intent: { verb: 'set', field: 'path', expectation: null, body: 'lib/x.scm' }
    };
    const entry = { req: 'r', cursor: 'w:1', id: 'a.4', field: 'src', payload: 'x', state: 'queued', record };
    fs.writeFileSync(file, JSON.stringify({ version: 1, cursor: 'w:1', entries: [entry] }));
    assert.throws(
      () => new Outbox(file).load(),
      (e: unknown) => e instanceof OutboxWriteError && /carries a record that writes a path/.test((e as Error).message)
    );
    const batch = { req: 'r', cursor: 'w:1', id: '', field: '', payload: '(set "a.4" path "lib/x.scm")', state: 'queued', verb: 'batch' };
    fs.writeFileSync(file, JSON.stringify({ version: 2, cursor: 'w:1', entries: [batch] }));
    assert.throws(
      () => new Outbox(file).load(),
      (e: unknown) => e instanceof OutboxWriteError && /a batch of something other than insert intents/.test((e as Error).message)
    );
  });

  it('refuses to submit a record that writes the path, and queues nothing', async () => {
    const store = new PathStore('a.4', 'src/x.scm');
    const outbox = new Outbox(path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-expect-')), 'outbox.json'));
    outbox.load();
    const saver = new Saver(store.client(), outbox, settling(outbox), IGNORED_DURABILITY);
    const record = recordFor({
      req: '11111111-2222-3333-4444-555555555555',
      store: '/tmp/store-a',
      storeHash: 'h',
      blockId: 'a.4',
      file: '/tmp/session/a.4/1.md',
      rawDigest: 'raw',
      sentDigest: 'sent',
      prefixDigest: 'prefix',
      seq: 1,
      intent: { verb: 'set', field: 'path', expectation: null, body: 'lib/x.scm' }
    });
    await assert.rejects(saver.submit(record), /only with the version it was read at/);
    assert.deepStrictEqual([outbox.pendingCount, store.sent.filter((q) => q[0] === 'set').length], [0, 0]);
  });

  it('refuses to load a queued version that is not a string', () => {
    const file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-expect-')), 'outbox.json');
    const entry = { req: 'r', cursor: 'w:1', id: 'a.4', field: 'path', payload: 'x', state: 'queued', expect: 5 };
    fs.writeFileSync(file, JSON.stringify({ version: 2, cursor: 'w:1', entries: [entry] }));
    assert.throws(
      () => new Outbox(file).load(),
      (e: unknown) => e instanceof OutboxWriteError && /version to write against that is not a string/.test((e as Error).message)
    );
  });
});

/*
 * THE MANIFEST: a directory's menu holds only "new file here", a file's
 * adds move and rename to the block's own entries.
 */
describe('V4 the menus', () => {
  it('offers a directory only a new file, and a file row its move and rename', () => {
    const root = path.join(__dirname, '..', '..', '..');
    const manifest = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
    const entries = manifest.contributes.menus['view/item/context'] as Array<{ command: string; when: string }>;
    const offered = (value: string): string[] =>
      entries
        .filter((e) => {
          const exact = /viewItem == (\S+)/.exec(e.when);
          const pattern = /viewItem =~ \/(.*)\/$/.exec(e.when);
          return exact !== null ? exact[1] === value : pattern !== null && new RegExp(pattern[1]).test(value);
        })
        .map((e) => e.command)
        .sort();
    assert.deepStrictEqual(offered('theourgia.dir'), ['theourgia.newFileHere']);
    assert.deepStrictEqual(offered('theourgia.block.file'), [
      'theourgia.moveToDirectory',
      'theourgia.openAsDocument',
      'theourgia.renameFile'
    ]);
    assert.deepStrictEqual(offered('theourgia.pathless'), []);
  });
});

/*
 * THE HARNESS: the compiled extension against a stand-in VS Code API. The
 * editor host cannot read a tree item, a context key or the workspace's
 * state back, so this is where they are read.
 */
function schedule(name: string): any {
  const run = spawnSync(process.execPath, [path.join(__dirname, '../support/extension-schedules.js'), '', name], {
    encoding: 'utf8',
    timeout: 150000
  });
  assert.strictEqual(run.status, 0, run.stdout + run.stderr);
  const result = JSON.parse(run.stdout.trim().split('\n').pop() as string);
  assert.strictEqual(result.complete, true);
  return result.result;
}

describe('V3 V4 the files view in the window', function () {
  this.timeout(60000);
  let r: any;
  before(() => {
    r = schedule('files-mode');
  });

  it('opens a store with paths in Files, keeps it, and tells the title bar', () => {
    assert.deepStrictEqual(
      r.A.rows.map((x: any) => [x.label, x.contextValue]),
      [
        ['docs', 'theourgia.dir'],
        ['src', 'theourgia.dir'],
        ['not in any file', 'theourgia.pathless']
      ]
    );
    assert.deepStrictEqual([r.A.remembered, r.A.context], ['files', 'files']);
  });

  it('draws a file by its segment, opening its block, and a directory with no command', () => {
    assert.deepStrictEqual(r.A.docs, [
      { id: 'file:a.1', label: 'a.md', command: 'theourgia.openBlock', contextValue: 'theourgia.block.file', description: 'a.1' }
    ]);
    assert.deepStrictEqual(
      r.A.src.map((x: any) => [x.id, x.label]),
      [['file:a.4', 'x.scm']],
      'the file under a section was not listed before anything was opened'
    );
    assert.strictEqual(r.A.rows[0].command, null, 'a directory opens something');
    assert.deepStrictEqual(r.A.group.map((x: any) => x.id), ['a.3']);
    assert.deepStrictEqual(r.A.fileChildren, ['a.2'], "a file's children are not its outline children");
  });

  it('opens a store with no path in Outline, and does not switch when a path appears', () => {
    assert.deepStrictEqual(r.B.first.map((x: any) => x.id), ['a.1', 'a.3']);
    assert.deepStrictEqual([r.B.firstRemembered, r.B.firstContext], ['outline', 'outline']);
    assert.deepStrictEqual(r.B.afterPath.map((x: any) => x.id), ['a.1', 'a.3']);
    assert.strictEqual(r.B.afterPathRemembered, 'outline');
  });

  it('toggles and keeps each store its own mode', () => {
    assert.deepStrictEqual(r.B.toggled.map((x: any) => x.label), ['docs', 'src', 'not in any file']);
    assert.deepStrictEqual([r.B.toggledRemembered, r.B.toggledContext], ['files', 'files']);
    /*
     * The actions above moved a.4 to other/w.scm in the stand-in's store A,
     * so its directory is other now; the row is about the mode.
     */
    assert.deepStrictEqual(r.A.back.map((x: any) => x.label), ['docs', 'other', 'not in any file'], 'store A lost its mode');
    assert.deepStrictEqual(r.A.outlined.map((x: any) => x.id), ['a.1', 'a.3']);
    assert.deepStrictEqual([r.A.outlinedRemembered, r.A.outlinedContext], ['outline', 'outline']);
  });

  it('creates, moves and renames through the Saver, each with its request id and the cursor it moved to', () => {
    const writes = r.actions.requests;
    assert.deepStrictEqual(
      writes.map((w: any) => w.verb),
      ['batch', 'set', 'set'],
      'the actions did not send one batch and two writes of the path'
    );
    assert.strictEqual(writes[0].input, '(insert root #f ((kind . doc) (path . "docs/n.md") (title . "n")))');
    assert.strictEqual(writes[0].args.length, 4, 'the batch carried an argument besides --req and --cursor');
    assert.deepStrictEqual([writes[1].input, writes[2].input], [null, null], 'a write of the path sent standard input');
    assert.deepStrictEqual(writes[1].args.slice(0, 3), ['a.4', 'path', 'lib/x.scm']);
    assert.deepStrictEqual(writes[2].args.slice(0, 3), ['a.4', 'path', 'lib/y.scm']);
    /*
     * The first cursor is the one the store's check gave; each later one is
     * the record the write before it made -- the batch's own item, then the
     * move's. A write of the path carries, after them, the version it was
     * read at; the batch carries none.
     */
    assert.deepStrictEqual(writes.map((w: any) => w.args.indexOf('--req')), [0, 3, 3]);
    assert.deepStrictEqual(
      writes.slice(1).map((w: any) => w.args[w.args.indexOf('--cursor') + 1]),
      ['w:40', 'w:41']
    );
    assert.deepStrictEqual(
      writes.map((w: any) => (w.args.includes('--if-unchanged') ? w.args.slice(w.args.indexOf('--if-unchanged')) : null)),
      [null, ['--if-unchanged', 'v1'], ['--if-unchanged', 'v2']],
      'a write of the path did not carry the version it was read at, or the batch carried one'
    );
    assert.deepStrictEqual(r.actions.shown, [
      { level: 'information', text: 'Theourgia: created docs/n.md' },
      { level: 'information', text: 'Theourgia: moved to lib/x.scm' },
      { level: 'information', text: 'Theourgia: renamed to lib/y.scm' }
    ]);
  });

  it('renames through a row kept from before the move, from the path the block has now', () => {
    assert.deepStrictEqual(r.actions.asked.slice(1), [
      ['Directory to move src/x.scm to', 'src'],
      ['New name for lib/x.scm', 'x.scm']
    ]);
  });

  it('moves from a row an old directory draws again, from the path the block has now', () => {
    assert.deepStrictEqual(r.oldDirectory.drawn.map((x: any) => [x.id, x.label]), [['file:a.4', 'x.scm']]);
    assert.deepStrictEqual(r.oldDirectory.asked, [['Directory to move lib/y.scm to', 'lib']]);
    assert.deepStrictEqual(
      r.oldDirectory.requests.map((w: any) => [...w.args.slice(0, 3), ...w.args.slice(w.args.indexOf('--if-unchanged'))]),
      [['a.4', 'path', 'deep/y.scm', '--if-unchanged', 'v3']]
    );
    assert.deepStrictEqual(r.oldDirectory.shown, [{ level: 'information', text: 'Theourgia: moved to deep/y.scm' }]);
  });

  it('sends a write the store refuses when the block changed during the prompt, says so and refreshes', () => {
    assert.deepStrictEqual(
      r.raced.requests.map((w: any) => [...w.args.slice(0, 3), ...w.args.slice(w.args.indexOf('--if-unchanged'))]),
      [['a.4', 'path', 'deep/z.scm', '--if-unchanged', 'v4']]
    );
    assert.deepStrictEqual(r.raced.shown, [
      { level: 'warning', text: 'Theourgia: this item changed since it was listed; the view is refreshed' }
    ]);
    assert.strictEqual(r.raced.refreshed, true, 'the view was not refreshed');
    assert.deepStrictEqual([r.raced.path, r.raced.version], ['other/q.scm', 'v5'], 'the write made during the prompt was overwritten');
  });

  it('keeps rows listed before a settings change that kept the store', () => {
    assert.deepStrictEqual(r.sameStore.asked, [['New name for other/q.scm', 'q.scm']]);
    assert.deepStrictEqual(
      r.sameStore.requests.map((w: any) => [...w.args.slice(0, 3), ...w.args.slice(w.args.indexOf('--if-unchanged'))]),
      [['a.4', 'path', 'other/w.scm', '--if-unchanged', 'v5']]
    );
    assert.deepStrictEqual(r.sameStore.shown, [{ level: 'information', text: 'Theourgia: renamed to other/w.scm' }]);
  });

  it('refuses Outline nodes of another store, and a child expanded from one, and asks that store nothing', () => {
    const refused = { level: 'warning', text: 'Theourgia: this item was listed under another store. Refresh the view and select it again.' };
    assert.deepStrictEqual(r.outlineStale, {
      childIds: ['a.2'],
      outlineRoot: 0,
      outlineChild: 0,
      askedOfB: [],
      shown: [refused, refused]
    });
  });

  it("refuses rows kept from another store's listing -- expanded, acted on or opened -- and asks that store nothing", () => {
    const refused = { level: 'warning', text: 'Theourgia: this item was listed under another store. Refresh the view and select it again.' };
    assert.deepStrictEqual(r.B.staleRow, {
      sent: 0,
      askedOfB: [],
      shown: [refused, refused, refused, refused, refused],
      docsChildren: 0,
      nodeChildren: 0,
      asked: 0,
      opened: ['a.1', '/stores/A']
    });
  });
});

describe('a stale path change drained by the retry command', function () {
  this.timeout(60000);
  it('is refused by the store against the version it was read at, and the window says so and lists again', () => {
    const r = schedule('files-retry-changed');
    assert.deepStrictEqual(r.queued, ['warning'], 'the move was not kept while the store did not answer');
    assert.ok(r.sets.length >= 2, JSON.stringify(r.sets));
    assert.ok(r.sets.every((s: string[]) => s.length === 2 && s[0] === '--if-unchanged' && s[1] === 'v1'), JSON.stringify(r.sets));
    assert.ok(
      r.retried.some((n: any) => n.level === 'warning' && n.text === 'Theourgia: this item changed since it was listed; the view is refreshed'),
      JSON.stringify(r.retried)
    );
    assert.strictEqual(r.refreshed, true, 'the view was not listed again');
    assert.strictEqual(r.path, 'other/q.scm', 'the late move overwrote the change made while it waited');
  });
});

describe('a stale path change drained by any drain is said', function () {
  this.timeout(60000);
  const notice = { level: 'warning', text: 'Theourgia: this item changed since it was listed; the view is refreshed' };

  it('says so when a kept move is drained in front of another save', () => {
    const r = schedule('files-drain-changed');
    assert.deepStrictEqual(r.verbs, ['set', 'batch'], 'the kept move was not drained in front of the new document');
    assert.deepStrictEqual(r.shown, [notice, { level: 'information', text: 'Theourgia: created docs/n2.md' }]);
    assert.strictEqual(r.path, 'other/q.scm');
  });

  it('says so across a settings change during the retry, and lists again only the store still shown', () => {
    const r = schedule('files-retry-rebuilt');
    assert.deepStrictEqual(r.sameStore, { shown: [notice], refreshed: true }, 'a same-store settings change silenced it');
    assert.deepStrictEqual(r.otherStore, { shown: [notice], refreshed: false }, 'the view of another store was listed again');
    assert.deepStrictEqual([r.pathA, r.pathC], ['other/q.scm', 'other/q.scm']);
  });
});

describe('a mode chosen while the listing remembers its own is kept', function () {
  this.timeout(60000);
  it('lists in Outline, chosen while an undecided listing was remembering Files', () => {
    const r = schedule('files-race-remember');
    assert.deepStrictEqual(r.rows, ['a.1', 'a.3']);
    assert.deepStrictEqual([r.remembered, r.context], ['outline', 'outline']);
  });
});

describe('a store switched while a move waits sends nothing', function () {
  this.timeout(60000);
  let r: any;
  before(() => {
    r = schedule('files-switch');
  });

  it('refuses after the fresh read when the store changed while it was on its way, and asks nothing', () => {
    assert.deepStrictEqual(r.duringRead, {
      sets: [],
      shown: [{ level: 'warning', text: 'Theourgia: the store changed while the file was being read. Select the file again.' }],
      asked: 0
    });
  });

  it('refuses right before the send when the store changed while the prompt was open', () => {
    assert.deepStrictEqual(r.duringPrompt, {
      sets: [],
      shown: [{ level: 'warning', text: 'Theourgia: the store changed while the name was being asked for. Select the file again.' }],
      asked: 1
    });
    assert.deepStrictEqual([r.pathA, r.pathB], ['src/x.scm', 'src/x.scm']);
  });
});

describe('a mode chosen while the Outline listing was on its way is kept', function () {
  this.timeout(60000);
  it('lists the store in Files, chosen during an Outline listing', () => {
    const r = schedule('files-race-outline');
    assert.deepStrictEqual(r.rows.map((x: any) => x.label), ['docs', 'src', 'not in any file']);
    assert.deepStrictEqual([r.remembered, r.context], ['files', 'files']);
  });
});

describe('V3 a mode chosen while the first listing was on its way is kept', function () {
  this.timeout(60000);
  it('keeps Outline chosen during an undecided listing, over the default that listing would have given', () => {
    const r = schedule('files-race');
    assert.strictEqual(r.chosen, 'outline');
    assert.deepStrictEqual([r.remembered, r.context], ['outline', 'outline']);
    assert.deepStrictEqual(r.rows.map((x: any) => x.id), ['a.1', 'a.3']);
  });
});

describe('V6 the warning row stays first in Files mode', function () {
  this.timeout(60000);
  it('draws the incomplete row above the directories', () => {
    const r = schedule('files-incomplete');
    assert.deepStrictEqual(
      r.rows.map((x: any) => x.contextValue),
      ['theourgia.incomplete', 'theourgia.dir', 'theourgia.dir', 'theourgia.pathless']
    );
  });
});

describe('V5 V2 the files view agrees with export, on a real core', function () {
  this.timeout(240000);
  let r: any;
  before(() => {
    r = schedule('files-real');
  });

  it('lists the directories and files export writes, a moved text file included', () => {
    assert.deepStrictEqual(r.errors, [], JSON.stringify(r.errors));
    assert.ok(r.exported.length >= 4, `export wrote ${JSON.stringify(r.exported)}`);
    assert.ok(r.exported.some((p: string) => p.endsWith('x.scm')), 'the moved text file was not exported');
    assert.deepStrictEqual(r.listed, r.exported, 'the files view is not the tree export wrote');
    assert.deepStrictEqual(r.listedDirs, r.exportedDirs);
  });

  it("gives a file node the block's outline children, in order, with their marks", () => {
    /*
     * The imported document's two sections, as the fixture wrote them, in
     * their order and with no marks: known here, not read back through the
     * same code both sides share.
     */
    /*
     * The imported document's heading tree, from the fixture's own text:
     * one direct child, the section "A", and under it the two Parts in
     * order. Known here, not read back through the code both sides share.
     */
    assert.deepStrictEqual(r.fileChildren.map((c: any) => [c[1], c[2]]), [['A', []]], JSON.stringify(r.fileChildren));
    assert.deepStrictEqual(r.grandChildren, ['Part one', 'Part two']);
    assert.deepStrictEqual(r.fileChildren, r.outlineChildren);
  });

  it('lists a second document at a taken path as not exported, and export writes the first', () => {
    assert.deepStrictEqual(r.duplicate.notes, [null, 'not exported: path taken']);
    assert.ok(r.duplicate.exportedText.includes(r.duplicate.firstMarker), r.duplicate.exportedText);
  });

  it('makes a new document through the window on the real core, and lists it', () => {
    assert.ok(r.created.found, 'there was no docs directory to make it in');
    assert.deepStrictEqual(r.created.shown, [{ level: 'information', text: 'Theourgia: created docs/fresh.md' }]);
    assert.strictEqual(r.created.listed, true, 'the new document is not in the next listing');
    assert.strictEqual(r.created.exported, true, 'export-md did not write the new document');
  });

  /*
   * NOTE: THIS ROW NEEDS THE CORE'S VERSION CLAUSE on `read <id> --wire`. On
   * a core without it the rename is refused before anything is sent, as a
   * read this client cannot act on.
   */
  it('renames on a real core against the version read, and the store refuses a rename that raced a write', () => {
    assert.strictEqual(r.guarded.found, true, 'the moved text file has no row');
    assert.deepStrictEqual(r.guarded.renameAsked, ['New name for src/x.scm']);
    assert.deepStrictEqual(r.guarded.renameShown, [{ level: 'information', text: 'Theourgia: renamed to src/renamed.scm' }]);
    assert.ok(r.guarded.exported.includes('src/renamed.scm'), JSON.stringify(r.guarded.exported));
    assert.deepStrictEqual(r.guarded.staleAsked, ['New name for src/renamed.scm']);
    assert.deepStrictEqual(r.guarded.staleShown, [
      { level: 'warning', text: 'Theourgia: this item changed since it was listed; the view is refreshed' }
    ]);
    assert.strictEqual(r.guarded.pathAfter, 'src/elsewhere.scm', 'the rename overwrote the write made during its prompt');
  });

  it('writes no file for a nested document with a path, and lists none', () => {
    assert.strictEqual(r.nested.visible, true, `the fixture record was not read back, even from a daemon started after it: ${JSON.stringify(r.nested)}`);
    assert.strictEqual(r.nested.listed, false);
    assert.strictEqual(r.nested.exported, false);
    assert.strictEqual(r.nested.contentInParent, true, 'the nested content is not in the exported enclosing document');
    assert.strictEqual(r.nested.underPart, true, "the nested block is not among its parent's outline children");
  });
});
