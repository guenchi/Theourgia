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
 * X1c ④: A COMMAND THE USER IS TOLD TO RUN HAS TO EXIST.
 *
 * Two refusals told the user to run "theourgia: Reconcile Block". The
 * manifest did not declare it, activation did not register it, and
 * nothing in production ever called the reconciliation the sentence was
 * describing -- so the only advice the extension gave led to an empty
 * palette, and every cell was green because the sentence was compared
 * with itself.
 *
 * These cells hold the three appearances together. The registration and
 * the sentence read one constant and cannot disagree by construction;
 * the manifest is JSON and cannot import a constant, so it is compared
 * here. The third appearance -- that the registered command is actually
 * reachable in a running editor -- is in the editor-hosted suite, where
 * the manifest the host loaded is the list it checks against.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import { COMMANDS } from '../../src/commands';
import { refusalNotice, statusLine, unrecordedNotice } from '../../src/status';

const root = path.join(__dirname, '..', '..', '..');

interface Contributed {
  command: string;
  title: string;
}

function manifestCommands(): Contributed[] {
  const text = fs.readFileSync(path.join(root, 'package.json'), 'utf8');
  const manifest = JSON.parse(text) as {
    contributes?: { commands?: Contributed[] };
  };
  return manifest.contributes?.commands ?? [];
}

describe('X1c the commands the extension offers', () => {
  /*
   * BOTH DIRECTIONS. One direction catches a command that was declared
   * and never registered; the other catches a command that was
   * registered and never declared, which is invisible in the palette.
   * Checking only the direction that happened to fail last time is how
   * a list stops shouting.
   */
  /*
   * ⚠️ IT COMPARES THE MANIFEST WITH THE SOURCE TABLE. It does not look
   * at registrations: removing the reconcile registration from
   * activation leaves this green, and the editor-hosted cell is what
   * catches that.
   */
  it('declares in the manifest exactly the commands the source names', () => {
    const declared = manifestCommands()
      .map((c) => `${c.command} :: ${c.title}`)
      .sort();
    const named = COMMANDS.map((c) => `${c.id} :: ${c.title}`).sort();
    assert.deepStrictEqual(declared, named, 'the manifest and the source name different commands');
  });

  it('gives every command an id under the extension name and a title a palette can show', () => {
    for (const command of COMMANDS) {
      assert.ok(command.id.startsWith('theourgia.'), `${command.id} is not this extension's`);
      assert.ok(command.title.startsWith('theourgia: '), `${command.title} would not be findable`);
      assert.ok(command.title.length > 'theourgia: '.length, `${command.id} has an empty title`);
    }
  });

  it('names no command twice', () => {
    assert.strictEqual(new Set(COMMANDS.map((c) => c.id)).size, COMMANDS.length);
    assert.strictEqual(new Set(COMMANDS.map((c) => c.title)).size, COMMANDS.length);
  });

  /*
   * THE CELL THAT WOULD HAVE CAUGHT IT. Every sentence this extension
   * shows is built in status.ts, and a sentence that quotes a command
   * title is telling the user to go and run it. Rendering the two that
   * do and checking the quoted title against the declared list is the
   * one comparison that does not compare the sentence with itself.
   */
  it('quotes only commands that exist in the sentences that tell the user to run one', () => {
    const titles = new Set(COMMANDS.map((c) => c.title));
    const sentences = [
      refusalNotice('a.1', '/s/1.md', { because: 'unresolved' }).text,
      unrecordedNotice('/s/1.md', 'req-mismatch').text,
      statusLine({
        store: '/s',
        actor: 'a',
        cursor: null,
        conflicts: 0,
        pending: 2,
        blocked: null
      }).tooltip
    ];
    let quoted = 0;
    for (const sentence of sentences) {
      for (const match of sentence.matchAll(/"([^"]+)"/g)) {
        quoted += 1;
        assert.ok(titles.has(match[1]), `${match[1]} is named by a message and is not a command`);
      }
    }
    /*
     * THE COUNT IS PART OF THE CHECK. A loop over no matches passes,
     * and a message that stopped quoting its command would take this
     * cell green with it -- leaving a refusal with no way out and a
     * green suite saying so.
     */
    assert.strictEqual(quoted, 3, 'a message stopped naming the command that resolves it');
  });

  /*
   * NO SECOND SPELLING ANYWHERE IN THE PRODUCT. The constants make the
   * registration and the sentence agree; they do nothing about a title
   * typed out again somewhere else, which is exactly the shape the
   * defect had. commands.ts is where the spellings live, so it is the
   * one file allowed to hold them.
   */
  /*
   * ⚠️ IT FLAGS ANY SPELLED-OUT TITLE, NOT ONLY A DECLARED ONE. The
   * first version only complained about strings that were already in
   * COMMANDS, so a literal naming a command that does NOT exist -- which
   * is the defect this whole file is about -- walked straight past it.
   * Found in review.
   *
   * AND IT WALKS THE WHOLE OF src. The first version read only the
   * immediate children, so a title written inside a subdirectory was
   * invisible to it.
   */
  it('holds every command title in one file and no other', () => {
    const offenders: string[] = [];
    const walk = (directory: string, shown: string): void => {
      for (const name of fs.readdirSync(directory)) {
        const full = path.join(directory, name);
        if (fs.statSync(full).isDirectory()) {
          walk(full, `${shown}${name}/`);
          continue;
        }
        if (!name.endsWith('.ts') || `${shown}${name}` === 'commands.ts') {
          continue;
        }
        const source = fs.readFileSync(full, 'utf8');
        for (const match of source.matchAll(/theourgia: [A-Z][A-Za-z ]*/g)) {
          offenders.push(`${shown}${name}: ${match[0].trimEnd()}`);
        }
      }
    };
    walk(path.join(root, 'src'), '');
    assert.deepStrictEqual(
      offenders,
      [],
      'a command title is spelled out away from commands.ts; if it names a command that does not ' +
        'exist, that is the defect this file is about'
    );
  });
});
