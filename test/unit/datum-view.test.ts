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
 * A DATUM BLOCK IS SHOWN, NOT EDITED (src/datum-view.ts).
 *
 * Measured on the real core before this change: a datum block opened as an
 * empty buffer, and an edit saved through it was answered as saved and
 * never reached the definition. These cells hold the open to a read-only
 * view of the datum export: nothing written, nothing committed, the text
 * the export writes, at the block's own place in it.
 */

import * as assert from 'assert';
import { spawnSync } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import { Answer, Client } from '../../src/client';
import { Block } from '../../src/blocks';
import { DatumScratch, datumViewOf, isDatumBlock } from '../../src/datum-view';
import { Composed, DocumentTexts, documentQuery, readDocumentQuery } from '../../src/document-view';
import { readProjection } from '../../src/markers';
import { initWire, parseAnswers, wire } from '../../src/wire';

const HEADER = '(code-projection 1 "s0000001" "d.1" (("w" . 2)) datum 0)';
const FILE =
  `#!chezscheme\n;; @file ${Buffer.from(HEADER).toString('hex')}\n` +
  '(library (probe d)\n(export alpha)\n(import (rnrs))\n;; @block d.2\n(define alpha 1)\n)\n';

function blockWith(mode: unknown): Block {
  return { id: 'b.1', fields: new Map<string, unknown>(mode === undefined ? [] : [['mode', mode]]) } as unknown as Block;
}

/*
 * A CLIENT THAT ANSWERS THE EXPORT, and the disk it writes into: a map of
 * directories, each made, listed, read and removed as the view asks.
 */
function rig(answer: { stdout: string; rc?: number; files?: Record<string, string> } | Error) {
  const made: string[] = [];
  const removed: string[] = [];
  const disk = new Map<string, Record<string, string>>();
  const asked: string[][] = [];
  const client = {
    request: async (verb: string, args: string[]): Promise<Answer> => {
      asked.push([verb, ...args]);
      if (answer instanceof Error) {
        throw answer;
      }
      disk.set(args[0], answer.files ?? {});
      const rc = answer.rc ?? 0;
      return { argv: [verb, ...args], rc, ok: rc === 0, text: answer.stdout, answers: parseAnswers(answer.stdout) } as unknown as Answer;
    }
  } as unknown as Client;
  const scratch: DatumScratch = {
    make: () => {
      const directory = `/scratch/${made.length}`;
      made.push(directory);
      return directory;
    },
    files: (directory) => Object.keys(disk.get(directory) ?? {}).sort(),
    read: (directory, file) => new Uint8Array(Buffer.from((disk.get(directory) ?? {})[file], 'utf8')),
    remove: (directory) => {
      removed.push(directory);
    }
  };
  return { client, scratch, made, removed, asked };
}

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

describe('a datum block is shown read-only, as the datum export writes it', function () {
  this.timeout(20000);
  before(async () => {
    await initWire();
  });

  it('takes a block as datum by its mode, a symbol or a string, and nothing else', () => {
    const sym = wire().read('datum');
    assert.strictEqual(isDatumBlock(blockWith(sym)), true);
    assert.strictEqual(isDatumBlock(blockWith('datum')), true);
    assert.strictEqual(isDatumBlock(blockWith(wire().read('text'))), false);
    assert.strictEqual(isDatumBlock(blockWith(undefined)), false);
    assert.strictEqual(isDatumBlock(blockWith('Datum')), false);
  });

  it('shows a library as its file, from the first byte, and asks the store for the datum export only', async () => {
    const r = rig({ stdout: '(ok (files 1))\n', files: { 'probe.sls': FILE } });
    const view = await datumViewOf(r.client, 'd.1', r.scratch);
    assert.deepStrictEqual(view, { ok: true, file: 'probe.sls', text: FILE, offset: 0 });
    assert.deepStrictEqual(r.asked, [['export-code', '/scratch/0', '--datum']]);
    assert.deepStrictEqual(r.removed, r.made, 'the export directory was left behind');
  });

  it('shows a definition as the same file, opened where its source starts', async () => {
    const r = rig({ stdout: '(ok (files 1))\n', files: { 'probe.sls': FILE } });
    const view = await datumViewOf(r.client, 'd.2', r.scratch);
    assert.ok(view.ok, JSON.stringify(view));
    assert.strictEqual(view.text, FILE);
    assert.strictEqual(view.offset, FILE.indexOf('(define alpha 1)'));
  });

  it("says the store's refusal of the export, and leaves no directory", async () => {
    const r = rig({ stdout: '(error projection-invalid (reason duplicate-path))\n', rc: 1 });
    const view = await datumViewOf(r.client, 'd.2', r.scratch);
    assert.ok(!view.ok);
    assert.match(view.reason, /would not write its datum export: \(error projection-invalid \(reason duplicate-path\)\)/);
    assert.deepStrictEqual(r.removed, r.made);
  });

  it('says an export whose count the directory does not hold', async () => {
    const r = rig({ stdout: '(ok (files 2))\n', files: { 'probe.sls': FILE } });
    const view = await datumViewOf(r.client, 'd.2', r.scratch);
    assert.deepStrictEqual(view, { ok: false, reason: 'the datum export wrote 2 files and 1 are there' });
  });

  it('says a block no exported file places, rather than showing an empty view', async () => {
    const r = rig({ stdout: '(ok (files 1))\n', files: { 'probe.sls': FILE } });
    const view = await datumViewOf(r.client, 'd.9', r.scratch);
    assert.deepStrictEqual(view, { ok: false, reason: 'the datum export holds no file for d.9' });
  });

  it('removes the export directory when the request throws', async () => {
    const r = rig(new Error('the transport failed'));
    await assert.rejects(datumViewOf(r.client, 'd.2', r.scratch), /the transport failed/);
    assert.deepStrictEqual(r.removed, r.made);
    assert.strictEqual(r.made.length, 1);
  });

  it('counts the offset in characters when the text before the block is not ASCII', async () => {
    const wide =
      `#!chezscheme\n;; @file ${Buffer.from(HEADER).toString('hex')}\n` +
      '(library (probe d)\n(export alpha)\n(import (rnrs))\n;; \u03bb \u4e2d\u6587 \ud83d\ude00\n;; @block d.2\n(define alpha 1)\n)\n';
    const r = rig({ stdout: '(ok (files 1))\n', files: { 'probe.sls': wide } });
    const view = await datumViewOf(r.client, 'd.2', r.scratch);
    assert.ok(view.ok, JSON.stringify(view));
    assert.strictEqual(view.offset, wide.indexOf('(define alpha 1)'));
    assert.ok(Buffer.byteLength(wide.slice(0, view.offset)) > view.offset, 'the fixture has no multi-byte character before the block');
  });

  it('finds the right file and the right definition among two libraries with several definitions', async () => {
    const other = '(code-projection 1 "s0000001" "e.1" (("w" . 2)) datum 0)';
    const second =
      `#!chezscheme\n;; @file ${Buffer.from(other).toString('hex')}\n` +
      '(library (other e)\n(export beta gamma)\n(import (rnrs))\n;; @block e.2\n(define beta 2)\n;; @block e.3\n(define gamma 3)\n)\n';
    const r = rig({ stdout: '(ok (files 2))\n', files: { 'probe.sls': FILE, 'other.sls': second } });
    const gamma = await datumViewOf(r.client, 'e.3', r.scratch);
    assert.deepStrictEqual(gamma, { ok: true, file: 'other.sls', text: second, offset: second.indexOf('(define gamma 3)') });
    const library = await datumViewOf(r.client, 'e.1', r.scratch);
    assert.deepStrictEqual(library, { ok: true, file: 'other.sls', text: second, offset: 0 });
    const alpha = await datumViewOf(r.client, 'd.2', r.scratch);
    assert.ok(alpha.ok && alpha.file === 'probe.sls', JSON.stringify(alpha));
  });

  it('does not read a text header as a datum one', () => {
    const textHeader = '(code-projection 1 "s0000001" "t.1" (("w" . 2)) text 0)';
    const bytes = new Uint8Array(Buffer.from(`;; @file ${Buffer.from(textHeader).toString('hex')}\n;; @block t.2\nx\n`, 'utf8'));
    assert.ok(readProjection(bytes, 'x.scm').ok, 'the text reader does not take its own header');
    assert.ok(!readProjection(bytes, 'x.scm', 'datum').ok, 'the datum reader took a text header');
  });

  it('reads a datum header only when asked for the datum projection', () => {
    const bytes = new Uint8Array(Buffer.from(FILE, 'utf8'));
    const asText = readProjection(bytes, 'probe.sls');
    assert.ok(!asText.ok, 'a text reader took a datum header');
    const asDatum = readProjection(bytes, 'probe.sls', 'datum');
    assert.ok(asDatum.ok, JSON.stringify(asDatum));
    assert.strictEqual(asDatum.projection.fileId, 'd.1');
    assert.deepStrictEqual(asDatum.projection.blocks, ['d.2']);
  });

  it('carries the view in the address: a datum address reads back as one, a subtree address as before', () => {
    const datum = documentQuery('/s', 'd.2', 'datum');
    assert.deepStrictEqual(readDocumentQuery(datum), { store: '/s', id: 'd.2', view: 'datum' });
    const subtree = documentQuery('/s', 'a.1');
    assert.strictEqual(new URLSearchParams(subtree).get('view'), null, 'a subtree address gained a view');
    assert.deepStrictEqual(readDocumentQuery(subtree), { store: '/s', id: 'a.1', view: 'subtree' });
    assert.strictEqual(readDocumentQuery('store=%2Fs&id=a.1&view=other'), null);
  });

  it('composes a restored tab as the view its address names', async () => {
    const asked: string[] = [];
    const client = {} as Client;
    const subtree = async (_c: Client, id: string): Promise<Composed> => {
      asked.push(`subtree ${id}`);
      return { ok: true, title: id, text: `subtree of ${id}` };
    };
    const datum = async (_c: Client, id: string): Promise<Composed> => {
      asked.push(`datum ${id}`);
      return { ok: true, title: id, text: `datum of ${id}` };
    };
    const texts = new DocumentTexts(() => ({ client, store: '/s' }), subtree, datum);
    assert.strictEqual(await texts.textFor('x:/a', documentQuery('/s', 'd.2', 'datum')), 'datum of d.2');
    assert.strictEqual(await texts.textFor('x:/b', documentQuery('/s', 'a.1')), 'subtree of a.1');
    assert.deepStrictEqual(asked, ['datum d.2', 'subtree a.1']);
    const without = new DocumentTexts(() => ({ client, store: '/s' }), subtree);
    await assert.rejects(without.textFor('x:/c', documentQuery('/s', 'd.2', 'datum')), /names a datum view/);
  });
});

describe('opening a datum block from the outline writes nothing and shows the export', function () {
  this.timeout(180000);

  it('sends no write, commit or working read for a library or a definition, and shows the export read-only', () => {
    const r = schedule('datum-open');
    const notice = (id: string) =>
      `Theourgia: ${id} is a datum block: the store keeps its code as a datum, and this editor cannot ` +
      'write one yet, so it is shown read-only as the datum export writes it.';
    for (const [k, id] of ['d.1', 'd.2'].entries()) {
      const o = r.opened[k];
      assert.strictEqual(o.id, id);
      assert.deepStrictEqual(o.asked, [['read'], ['export-code', '--datum']], `${id}: ${JSON.stringify(o.asked)}`);
      assert.strictEqual(o.uri.scheme, 'theourgia-document', `${id} opened outside the read-only scheme`);
      assert.strictEqual(o.uri.path, '/probe.sls');
      assert.deepStrictEqual(readDocumentQuery(o.uri.query), { store: '/stores/A', id, view: 'datum' });
      assert.strictEqual(o.text, r.file, `${id}: the tab is not the export`);
      assert.strictEqual(o.restored, r.file, `${id}: a restored tab is not the export`);
      assert.deepStrictEqual(o.restoreAsked, ['export-code'], `${id}: a restored tab asked ${JSON.stringify(o.restoreAsked)}`);
      assert.strictEqual(o.prefixLength, id === 'd.1' ? 0 : r.file.indexOf('(define alpha 1)'));
      const lineOf = (offset: number) => r.file.slice(0, offset).split('\n').length - 1;
      assert.strictEqual(o.line, id === 'd.1' ? 0 : lineOf(r.file.indexOf('(define alpha 1)')), `${id} was not opened at its place`);
      assert.deepStrictEqual(o.shown, [{ text: notice(id), level: 'information' }]);
      assert.deepStrictEqual(o.written, [], `${id}: a file was written for the block`);
      assert.strictEqual(o.language, 'chez');
    }
    assert.strictEqual(r.scratchLeft, 0, 'an export directory was left behind');
  });

  it('on the real core: opens the library and the definition as the export, and the definition gains no src', () => {
    const r = schedule('datum-real');
    for (const [k, id] of [r.libId, r.defId].entries()) {
      const o = r.opened[k];
      assert.strictEqual(o.id, id);
      const verbs = o.sent.map((s: string[]) => s[0]);
      for (const forbidden of ['write', 'commit', 'set', 'batch']) {
        assert.ok(!verbs.includes(forbidden), `${id}: ${forbidden} was sent: ${JSON.stringify(o.sent)}`);
      }
      assert.ok(!o.sent.some((s: string[]) => s.includes('--working-info')), `${id}: a working read was sent`);
      assert.strictEqual(o.text, r.fileText, `${id}: the tab is not the datum export`);
      assert.deepStrictEqual(o.shown.filter((n: { level: string }) => n.level !== 'information'), []);
    }
    assert.strictEqual(r.opened[0].prefixLength, 0);
    assert.strictEqual(r.opened[1].line, r.fileText.slice(0, r.opened[1].prefixLength).split('\n').length - 1);
    assert.strictEqual(r.opened[0].line, 0);
    assert.ok(
      r.fileText.slice(r.opened[1].prefixLength).startsWith('(define (alpha x)'),
      `the definition does not open at its source: ${JSON.stringify(r.fileText.slice(r.opened[1].prefixLength, r.opened[1].prefixLength + 40))}`
    );
    assert.ok(!/\(src \. /.test(r.after), `the definition gained a src: ${r.after}`);
    assert.ok(r.definition !== null, 'go to definition did not offer the datum definition');
    assert.deepStrictEqual([...r.definition.kinds].sort(), ['def', 'export']);
    const defLine = r.fileText.split('\n').findIndex((l: string) => l.includes('(define (alpha x)'));
    assert.ok(defLine > 0, r.fileText);
    assert.strictEqual(r.definition.line, defLine, 'go to definition did not open the datum view at the definition');
  });
});

/*
 * THE MODE AT THE WORKING WRITE. A file's record says whether its block is
 * text or a datum; one made without a mode -- as an earlier version made
 * every file -- costs one read at its first save, which the record keeps.
 */
describe('a working write goes by the mode its file recorded, and learns it once', function () {
  this.timeout(180000);
  let r: any;
  const datumSentence = (id: string) =>
    `${id} was not sent: it is a datum block, whose code the store keeps as a datum, and this ` +
    'editor cannot write one yet. Your file is kept; open the block from the outline to see it ' +
    'read-only as the datum export writes it.';
  before(() => {
    r = schedule('datum-legacy');
  });

  it('saves a text file this version made with no read of its own', () => {
    assert.strictEqual(r.made.mode, 'text', 'the open did not record the mode');
    assert.deepStrictEqual(r.made.first.reads, [], JSON.stringify(r.made.first.verbs));
    assert.ok(r.made.first.verbs.includes('write') && r.made.first.verbs.includes('commit'), JSON.stringify(r.made.first.verbs));
    assert.deepStrictEqual(r.made.first.shown.filter((n: { level: string }) => n.level === 'error'), []);
  });

  it('reads a text block once for a file with no mode, records it, and does not read again', () => {
    const first = r.textLegacy.first;
    assert.deepStrictEqual(first.reads, ['a.1']);
    assert.ok(first.verbs.indexOf('read') < first.verbs.indexOf('write'), JSON.stringify(first.verbs));
    assert.strictEqual(first.mode, 'text');
    assert.deepStrictEqual(r.textLegacy.second.reads, [], JSON.stringify(r.textLegacy.second.verbs));
    for (const save of [first, r.textLegacy.second]) {
      assert.ok(save.verbs.includes('write') && save.verbs.includes('commit'), JSON.stringify(save.verbs));
      assert.deepStrictEqual(save.shown.filter((n: { level: string }) => n.level === 'error'), [], JSON.stringify(save.shown));
    }
  });

  it("refuses a datum block's file before the write, records datum, and refuses the next save without a read", () => {
    const { before, first, second } = r.datumLegacy;
    assert.strictEqual(before, null, 'the legacy file already had a mode');
    assert.deepStrictEqual(first.reads, ['d.2']);
    for (const save of [first, second]) {
      assert.ok(!save.verbs.includes('write') && !save.verbs.includes('commit'), JSON.stringify(save.verbs));
      assert.deepStrictEqual(save.shown, [{ text: `Theourgia: ${datumSentence('d.2')}`, level: 'error' }]);
      assert.strictEqual(save.mode, 'datum');
    }
    assert.deepStrictEqual(second.reads, [], JSON.stringify(second.verbs));
    assert.strictEqual(r.datumLegacy.openedOver.mode, 'datum', 'opening a datum block left a record of it saying text');
    const failed = r.unreadable.first;
    assert.deepStrictEqual(failed.reads, ['x.9']);
    assert.ok(!failed.verbs.includes('write'), JSON.stringify(failed.verbs));
    assert.ok(failed.shown.some((n: { text: string; level: string }) => n.level === 'error' && /the mode of x\.9 could not be read, so it is not saved as text/.test(n.text)), JSON.stringify(failed.shown));
    assert.strictEqual(failed.mode, null, 'a mode nobody read was recorded');
    const unknown = r.unknownMode.first;
    assert.deepStrictEqual(unknown.reads, ['o.1']);
    assert.ok(!unknown.verbs.includes('write'), JSON.stringify(unknown.verbs));
    assert.ok(unknown.shown.some((n: { text: string; level: string }) => n.level === 'error' && /the mode of o\.1 is not one this editor knows, so it is not saved as text/.test(n.text)), JSON.stringify(unknown.shown));
    assert.strictEqual(unknown.mode, null, 'an unknown mode was recorded as one');
    const opened = r.unknownMode.opened;
    assert.strictEqual(opened.mode, null, 'opening a block of an unknown mode recorded a mode for it');
    assert.strictEqual(opened.reopened, null, 'reopening kept a text mode the block no longer has');
    assert.strictEqual(r.unknownMode.refusedReopen.mode, null, 'a reopen whose publication was refused left the record saying text');
    assert.deepStrictEqual(opened.save.reads, ['o.2']);
    assert.ok(!opened.save.verbs.includes('write'), JSON.stringify(opened.save.verbs));
    assert.ok(opened.save.shown.some((n: { text: string; level: string }) => n.level === 'error' && /the mode of o\.2 is not one this editor knows/.test(n.text)), JSON.stringify(opened.save.shown));
  });

  it('gates a save through `set` -- a record with no projection -- the same way', () => {
    const datum = r.noProjectionDatum.first;
    assert.deepStrictEqual(datum.reads, ['d.3']);
    assert.ok(!datum.verbs.includes('set') && !datum.verbs.includes('write'), JSON.stringify(datum.verbs));
    assert.deepStrictEqual(datum.shown, [{ text: `Theourgia: ${datumSentence('d.3')}`, level: 'error' }]);
    assert.strictEqual(datum.mode, 'datum');
    const text = r.noProjectionText.first;
    assert.deepStrictEqual(text.reads, ['t.5']);
    assert.ok(text.verbs.indexOf('read') < text.verbs.indexOf('set'), JSON.stringify(text.verbs));
    assert.strictEqual(text.mode, 'text');
    assert.deepStrictEqual(text.shown.filter((n: { level: string }) => n.level === 'error'), [], JSON.stringify(text.shown));
  });

  it("refuses to reconcile a datum block's file or one of an unknown mode, writes nothing, and records what it read", () => {
    const c = r.reconciled;
    assert.ok(!c.verbs.includes('write') && !c.verbs.includes('commit'), JSON.stringify(c.verbs));
    assert.deepStrictEqual(c.shown, [{ text: `Theourgia: ${datumSentence('d.1')}`, level: 'error' }]);
    assert.strictEqual(c.mode, 'datum');
    const u = c.unknown;
    assert.ok(!u.verbs.includes('write') && !u.verbs.includes('commit'), JSON.stringify(u.verbs));
    assert.ok(u.shown.some((n: { text: string; level: string }) => n.level === 'error' && /the mode of o\.1 is not one this editor knows, so it is not reconciled/.test(n.text)), JSON.stringify(u.shown));
    assert.strictEqual(u.mode, null, 'a reconciliation left a text mode the block does not have');
  });
});

/*
 * TEXT IS SET ONLY WITH A READ IN HAND. Every place in src that can give a
 * record a mode is listed here with the read it rests on; a new one is red
 * until it is added, with its read. Clearing and datum are not listed: they
 * can only make a save read or refuse.
 */
describe('the places that can record a block as text', () => {
  const root = path.join(__dirname, '..', '..', '..', 'src');
  const SETTERS: Record<string, string> = {
    'extension.ts publish mode:openedMode ?? undefined,modeSeenAt:seenBefore':
      'openBlock: recordedModeOf(block) of the read just made, with the not-text reads seen before it',
    "extension.ts recordWorking ...,'text',seenAtRead)": 'reconcileBlock: the block read at its start, and only text goes on',
    "extension.ts reconcileBy ...,'text',seenAtRead)": 'reconcileBlock: the same read',
    'extension.ts recordWorking ...,working.mode,seenAtSave)': 'the save: Working.write answers text only when it read the block',
    'extension.ts recordText ...(file,unprojected.revision,seenAtSave)': 'the save with no projection: called only when requireText read the block'
  };
  /*
   * THE ARGUMENTS OF A CALL whose opening parenthesis ends just before
   * `from`, split at the commas of its own level and read to its matching
   * close; quoted text is skipped.
   */
  function argumentsAt(line: string, from: number): string[] {
    const args: string[] = [];
    let depth = 0;
    let quote: string | null = null;
    let current = '';
    for (let i = from; i < line.length; i += 1) {
      const c = line[i];
      if (quote !== null) {
        current += c;
        if (c === quote && line[i - 1] !== '\\') {
          quote = null;
        }
        continue;
      }
      if (c === "'" || c === '"' || c === '`') {
        quote = c;
        current += c;
      } else if (c === '(') {
        depth += 1;
        current += c;
      } else if (c === ')') {
        if (depth === 0) {
          args.push(current.trim());
          return args;
        }
        depth -= 1;
        current += c;
      } else if (c === ',' && depth === 0) {
        args.push(current.trim());
        current = '';
      } else {
        current += c;
      }
    }
    throw new Error(`no closing parenthesis on the line: ${line}`);
  }

  function setters(): string[] {
    const found: string[] = [];
    for (const name of fs.readdirSync(root).filter((n) => n.endsWith('.ts')).sort()) {
      const text = fs.readFileSync(path.join(root, name), 'utf8');
      for (const line of text.split('\n')) {
        if (/^\s*(public|private)\s/.test(line)) {
          continue;
        }
        const publish = /\.publish(?:Now)?\(\{[^}]*\bmode:([^,}]+)(?:,\s*modeSeenAt:([^,}]+))?/.exec(line);
        if (publish !== null) {
          found.push(`${name} publish mode:${publish[1].trim()}${publish[2] === undefined ? '' : `,modeSeenAt:${publish[2].trim()}`}`);
        }
        const call = /\.(recordWorking|reconcileBy|recordText)\(/.exec(line);
        if (call !== null) {
          const args = argumentsAt(line, call.index + call[0].length);
          const last = call[1] === 'recordText' ? `(${args.join(',')})` : `,${args.slice(-2).join(',')})`;
          found.push(`${name} ${call[1]} ...${last}`);
        }
      }
    }
    return found.sort();
  }

  it('finds exactly the listed setters, each with its read', () => {
    assert.deepStrictEqual(setters(), Object.keys(SETTERS).sort());
  });
});

/*
 * THE GATE'S READ AND A SAVE'S BEHIND NOTICE, on a real core. A save that
 * reads the block first (a record with no mode, as 1.0.0 left its files) and
 * one that does not (text recorded) differ only in that read; another
 * instance's segment is published into the store between the window's first
 * save and the one measured, and both saves must say so.
 */
describe('the behind notice of a save does not depend on the gate reading the block', function () {
  this.timeout(240000);
  const behindIn = (shown: Array<{ text: string; level: string }>, other: string) =>
    shown.find((n) => /since this save's baseline/.test(n.text) && n.text.includes(other));

  for (const [label, scenario, reads] of [
    ['a record with no mode, read before the write', 'behind-real-legacy', true],
    ['a record saying text, no read', 'behind-real-text', false]
  ] as const) {
    it(`names the other instance after ${label}`, () => {
      const r = schedule(scenario);
      assert.deepStrictEqual(r.firstShown.filter((n: { level: string }) => n.level === 'error'), [], JSON.stringify(r.firstShown));
      assert.strictEqual(r.modeBefore, reads ? null : 'text');
      const verbs = r.second.requests.map((q: { verb: string; args: string[] }) => [q.verb, ...q.args.filter((a) => a.startsWith('--'))].join(' '));
      const plainRead = r.second.requests.some((q: { verb: string; args: string[] }) => q.verb === 'read' && q.args.length === 1 && q.args[0] === r.mine);
      assert.strictEqual(plainRead, reads, `the save's requests: ${JSON.stringify(verbs)}`);
      assert.ok(verbs.some((v: string) => v.startsWith('commit')), `no commit was sent: ${JSON.stringify(verbs)}`);
      assert.ok(
        behindIn(r.second.shown, r.other) !== undefined,
        `the save did not say that ${r.other} landed behind it: shown ${JSON.stringify(r.second.shown)}; requests ${JSON.stringify(r.second.requests)}`
      );
      assert.strictEqual(r.modeAfter, 'text');
    });
  }
});

/*
 * AFTER ANY READ ATTEMPT OF A BLOCK BY THIS WINDOW, ITS RECORDS SAY TEXT ONLY
 * IF THAT READ SHOWED TEXT: a read that fails, answers no block, or is
 * rejected clears like one that shows datum; a queued write of the block's
 * text is not sent once a later read did not show text; and a text read
 * overtaken by a not-text one does not record text.
 */
describe('a read that does not show text leaves no record saying text', function () {
  this.timeout(180000);
  let failed: any;
  before(() => {
    failed = schedule('fail-reads');
  });

  it('clears the record when an open or a reconciliation cannot read the block, or finds it gone', () => {
    assert.strictEqual(failed.openRefused.mode, null, 'an open whose read was refused left text');
    assert.strictEqual(failed.openGone.mode, null, 'an open of a block no longer in the store left text');
    assert.strictEqual(failed.reconcileRefused.mode, null, 'a reconciliation whose read was refused left text');
  });

  it('refuses the save, sends no write and records no mode when the gate\'s read is rejected', () => {
    const g = failed.gateRejected;
    assert.ok(!g.verbs.includes('write') && !g.verbs.includes('commit'), JSON.stringify(g.verbs));
    assert.ok(
      g.shown.some((n: { text: string; level: string }) => n.level === 'error' && /the mode of g\.1 could not be read, so it is not saved as text/.test(n.text)),
      JSON.stringify(g.shown)
    );
    assert.strictEqual(g.mode, null);
  });

  it('parks a queued write of the text once the block is read as datum, and sends nothing', () => {
    const r = schedule('queued-datum');
    assert.strictEqual(r.modeAfterRead, 'datum');
    assert.ok(!r.sent.includes('commit') && !r.sent.includes('set'), JSON.stringify(r.sent));
    /*
     * THE FIRST IS PARKED BY NAME; THE REST OF THE BLOCK'S QUEUE WAITS BEHIND
     * IT, unsent, as every entry behind a parked one of the same block does
     * (`nextRunnable`). Each is asked again at its own transmission.
     */
    assert.ok(r.after.length >= 2, JSON.stringify(r.after));
    assert.strictEqual(r.after[0].state, 'parked', JSON.stringify(r.after));
    assert.match(String(r.after[0].lastError), /q\.1 has since been read as a datum block/);
    assert.ok(r.after.slice(1).every((e: { state: string }) => e.state === 'queued'), JSON.stringify(r.after));
    /*
     * AND ASKED AGAIN: the parked one cleared and the queue let go on, the
     * entry that waited is parked by the same check, and still nothing is sent.
     */
    assert.ok(r.cleared !== null && r.cleared.after.length === r.after.length - 1, JSON.stringify(r.cleared));
    assert.ok(!r.cleared.sent.includes('commit') && !r.cleared.sent.includes('set'), JSON.stringify(r.cleared.sent));
    assert.strictEqual(r.cleared.after[0].state, 'parked', JSON.stringify(r.cleared.after));
    assert.match(String(r.cleared.after[0].lastError), /q\.1 has since been read as a datum block/);
  });

  it('sends the same queued writes when the block is read as text again (the control)', () => {
    const r = schedule('queued-text');
    assert.strictEqual(r.modeAfterRead, 'text');
    assert.ok(r.sent.includes('commit'), JSON.stringify(r.sent));
    assert.ok(!r.after.some((e: { lastError: string }) => /has since been read/.test(String(e.lastError))), JSON.stringify(r.after));
  });

  it('records no text from an open whose read was overtaken by a read as datum', () => {
    assert.strictEqual(schedule('race-datum').mode, null);
  });

  it('records text when the overtaking read also shows text (the control)', () => {
    assert.strictEqual(schedule('race-text').mode, 'text');
  });
});
