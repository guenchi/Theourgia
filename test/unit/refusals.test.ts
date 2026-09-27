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
 * EVERY REFUSAL THE CORE CAN MAKE, AGAINST THE ONE TABLE THAT SORTS THEM.
 *
 * NOTE: THE LIST IS READ FROM THE CORE, NOT WRITTEN HERE.
 *
 * I wrote one by hand first, from the cells in `saver.test.ts` that
 * happened to exercise a refusal. It had seven names and looked
 * complete. The pinned core makes TWENTY-FIVE, several of them on the
 * write path, so a save could receive a refusal this client had never
 * heard of -- and the sorting table would have taken the default branch
 * with nobody the wiser. A list of what our own cells have seen is not a
 * list of what the other side can say. (The same lesson this tree
 * already carries as "the manifest is complete only against its own
 * list".)
 *
 * HOW IT IS READ: Chez reads actual forms from the core's top-level
 * source files, listed from the directory rather than from git.
 * The inventory recognizes literal error constructors and excludes comments.
 * Dynamic refusal constructors still need an exported core schema to make
 * this a complete semantic catalog; the source pin remains reproducible.
 *
 * NOTE: AND IT IS THE WRONG PLACE TO BE ASKING. Which refusals a verb can
 * produce is a fact the CORE knows; deducing it by reading its source is
 * a stopgap, and the main session has put a machine-readable table on
 * the core's queue. When that lands, both tables here are derived from
 * it rather than judged by me.
 */

import * as assert from 'assert';
import {execFileSync} from 'child_process';
import {readFileSync, readdirSync} from 'fs';
import * as path from 'path';
import { NOT_A_WRITES_ANSWER, classifyRefusal } from '../../src/saver';
import { initWire, parseAnswers } from '../../src/wire';

/*
 * THE PIN IS NAMED IN EVERY FAILURE. A reading taken against an
 * unrecorded version of the core is a reading nobody can repeat, and the
 * two tables are about one particular core.
 */
function coreSources(): { directory: string; files: string[] } {
  const directory = process.env.THEOURGIA_CORE;
  assert.ok(
    directory !== undefined && directory.length > 0,
    'THEOURGIA_CORE is not set, so the refusals this cell is about cannot be read from the core'
  );
  /*
   * NOTE: THE DIRECTORY IS LISTED, NOT THE INDEX. This read the core's
   * sources with `git ls-files`, which answers "what is tracked" and not
   * "what is there". Two readings, both taken:
   *
   *   * a core checkout that is not a git working tree answers `fatal:
   *     not a git repository`, and all three cells below fail with it.
   *     `git init` in that same copy made them pass -- a reading about
   *     the copy rather than about the core.
   *   * a core that IS a git checkout, with one library file added to
   *     the directory but not yet to the index, answers WITHOUT it, and
   *     that is the worse of the two because nothing goes red. Measured
   *     with the `working` source (a `.ss` file then) left untracked: the inventory fell from 47
   *     refusal kinds to 43, losing `invalid-working-baseline`,
   *     `no-draft`, `working-unavailable` and `working-version-changed`
   *     -- four refusals a save can actually receive -- and every cell
   *     here still passed, because each one asks about the kinds it
   *     found.
   *
   * Top level only, as before: the libraries live beside `theourgia.sc`,
   * and `test/` holds fixtures rather than core sources.
   *
   * NOTE: `.sc` AND `.ss` BOTH. The core renamed its sources to `.sc`
   * (276d9f2) and kept `build.ss` a script; a filter on one extension
   * read a renamed core as having no refusals at all -- measured on the
   * F46 pin, 0 kinds, which the first cell below turns red.
   */
  const files = readdirSync(directory as string, {withFileTypes: true})
    .filter(entry => entry.isFile() && (entry.name.endsWith('.sc') || entry.name.endsWith('.ss')))
    .map(entry => entry.name)
    .sort()
    .map(name => path.join(directory as string, name));
  assert.ok(files.length > 0, `no core sources under ${directory as string}`);
  return { directory: directory as string, files };
}

function kindsTheCoreMakes(): Map<string, string> {
  const {files}=coreSources();
  const output=execFileSync(process.env.THEOURGIA_SCHEME??'scheme',
    ['--script',path.join(__dirname,'../support/core-refusals.ss'),...files],{encoding:'utf8'});
  const found=new Map<string,string>();
  for(const row of output.trim().split('\n')) {
    const [file,kind]=row.split('\t');if(kind)found.set(kind,path.basename(file));
  }
  return found;
}

describe('U-ref every refusal the core can make is sorted by name, not by default', () => {
  before(async () => {
    await initWire();
  });

  /*
   * THE INSTRUMENT'S OWN FIRST READING. An AST walk that found nothing
   * would make every check below vacuous, and "no refusals found" is
   * exactly what a changed spelling in the core would produce.
   */
  it('finds the refusals in the core at all', () => {
    const kinds = kindsTheCoreMakes();
    assert.ok(
      kinds.size >= 20,
      `only ${kinds.size} refusal kinds were found in the core, which is too few to be reading ` +
        'its sources; the literal error constructors may have changed'
    );
  });

  it('has a verdict or a stated provenance for every one of them', () => {
    const kinds = kindsTheCoreMakes();
    const unaccounted: string[] = [];
    for (const [kind, where] of kinds) {
      if (NOT_A_WRITES_ANSWER[kind] !== undefined) {
        continue;
      }
      /*
       * NOTE: IT IS PUT THROUGH THE PRODUCT'S OWN CLASSIFIER, not compared
       * with a copy of its table. A cell that restated the table would
       * be checking its own copy -- and the mark is the product's way of
       * saying "nobody has looked at this one", which is precisely what
       * is being asked.
       */
      const answer = parseAnswers(`(error ${kind} (detail "x"))\n`)[0];
      const settlement = classifyRefusal(answer);
      if (settlement.verdict === 'refused' && settlement.unrecognised !== undefined) {
        unaccounted.push(`${kind} (${where})`);
      }
    }
    assert.deepStrictEqual(
      unaccounted,
      [],
      'the core can answer with these and nothing has sorted them: give each a verdict, or a row ' +
        'in NOT_A_WRITES_ANSWER saying where the core makes it'
    );
  });

  /*
   * AND THE EXCLUSION LIST DOES NOT OUTLIVE THE CORE. A row for a kind
   * the core no longer makes is a licence nobody asked for, kept where
   * the next reader will believe it.
   */
  it('keeps no excuse for a refusal the core no longer makes', () => {
    const kinds = kindsTheCoreMakes();
    const stale = Object.keys(NOT_A_WRITES_ANSWER).filter((kind) => !kinds.has(kind));
    assert.deepStrictEqual(stale, [], 'these kinds are excused and the core does not make them');
  });
});

/*
 * THE F100b RE-PIN'S TWO PROVENANCE ROWS STAY TRUE ONLY WHILE THIS EXTENSION
 * SENDS WHAT IT SENDS. `candidate-unreadable` is made only by the core's
 * `publish` verb and `socket-dir-missing` only for a `--socket` other than the
 * default (NOT_A_WRITES_ANSWER, saver.ts). Each cell below is a tripwire, not a
 * measurement: it is green today and turns red the day a request builder
 * starts sending the one or passing the other, which is the day those rows
 * become false.
 */
describe('re-pin f5ebd58: what this extension sends keeps two answers out of its reach', () => {
  const SRC = path.join(__dirname, '..', '..', '..', 'src');
  const sources = (): Array<{ name: string; text: string }> =>
    readdirSync(SRC)
      .filter((name) => name.endsWith('.ts'))
      .sort()
      .map((name) => ({ name, text: readFileSync(path.join(SRC, name), 'utf8') }));

  /*
   * THE VERBS THE REQUEST BUILDERS SEND, read from the calls, not from
   * client.ts's table of verbs it knows: every `.request(<first>, ...)`; a
   * quoted first argument is the verb, and a name is followed to its `const`
   * in the same file, whose quoted strings are the verbs it can hold. A first
   * argument of any other form is reported, not passed over.
   */
  function verbsSent(): { verbs: Set<string>; unread: string[] } {
    const verbs = new Set<string>();
    const unread: string[] = [];
    for (const { name, text } of sources()) {
      for (const call of text.matchAll(/\.request\(\s*([^,)]+)/g)) {
        const first = call[1].trim();
        const quoted = /^'([a-z-]+)'$/.exec(first);
        if (quoted !== null) {
          verbs.add(quoted[1]);
          continue;
        }
        const held = /^[A-Za-z_]\w*$/.test(first)
          ? new RegExp(`const ${first}\\s*(?::[^=]+)?=\\s*([^;]+);`).exec(text)
          : null;
        if (held === null) {
          unread.push(`${name}: ${first}`);
          continue;
        }
        for (const literal of held[1].matchAll(/'([a-z-]+)'/g)) {
          verbs.add(literal[1]);
        }
      }
    }
    return { verbs, unread };
  }

  it('sends no `publish` (the only verb that answers candidate-unreadable)', () => {
    const { verbs, unread } = verbsSent();
    assert.deepStrictEqual(unread, [], 'a request whose verb this cell cannot read');
    assert.ok(verbs.has('read') && verbs.has('commit'), `the scan did not find the requests it exists to read: ${[...verbs]}`);
    assert.strictEqual(verbs.has('publish'), false, 'this extension now sends publish, so candidate-unreadable can answer it');
  });

  /*
   * A `--socket` in an argument list is a quoted string, '--socket' or
   * "--socket"; the backquoted `--socket` of a comment or a provenance row is
   * prose and is not counted.
   */
  it('passes no `--socket` (the only way to be answered socket-dir-missing)', () => {
    const found = sources()
      .filter(({ text }) => /(['"])--socket\1/.test(text))
      .map(({ name }) => name);
    assert.ok(sources().length > 0, 'no source file was read');
    assert.deepStrictEqual(found, [], 'a source file spells --socket, so socket-dir-missing can answer it');
  });
});
