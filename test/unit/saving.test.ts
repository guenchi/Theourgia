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
 * C5 and C17: what a save sends, and the many reasons it sends nothing.
 *
 * ONE SNAPSHOT, TAKEN ONCE. The design's answer to torn reads is not to
 * read more carefully but to not read for content at all: when the
 * document is clean, `getText()` is the text of the last completed save,
 * and that single value is what goes out. The disk bytes are read only
 * to prove they are that text, encoded as UTF-8 with no byte-order mark.
 * (§12.19.4, §12.17.3)
 *
 * C17 NAMES THE IMPLEMENTATION THESE MUST KILL: one that verifies and
 * then reads the text a second time. Between the two reads the user can
 * type, so the bytes checked are not the bytes sent -- and every cell
 * that only compares the outcome would pass. So `getText` is counted.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { Sidecar } from '../../src/publication';
import { SaveDocument, Saving } from '../../src/saving';
import { RecordingFs } from '../support/recording-fs';

function scratch(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), 'theourgia-saving-'));
}

function baseline(prefix: string): Sidecar {
  return {
    format: 1,
    storeId: 's',
    blockId: 'a.2',
    phase: 'published',
    prefix,
    written: 'w',
    previous: null,
    acknowledgedRaw: null,
    sent: null,
    cursor: null,
    localOnly: false,
    unresolved: false
  };
}

/*
 * A document that counts how often its text was asked for, and can be
 * made to answer differently the second time -- which is how the
 * "verify then read again" implementation is caught.
 */
function document(file: string, texts: string[], isDirty = false): SaveDocument & { calls: number } {
  const doc = {
    file,
    isDirty,
    calls: 0,
    getText(): string {
      const text = texts[Math.min(doc.calls, texts.length - 1)];
      doc.calls += 1;
      return text;
    }
  };
  return doc;
}

describe('C5 only a clean document is sent, and only its snapshot', () => {
  it('sends nothing when the document has been edited since it was saved', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nnewer\n'], true), baseline('## Two\n'));
    assert.deepStrictEqual(decision, { send: false, refusal: { because: 'document-dirty' } });
  });

  it('sends the body when the disk bytes are that snapshot', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nbody\n']), baseline('## Two\n'));
    assert.ok(decision.send, `nothing was sent: ${JSON.stringify(decision)}`);
    assert.strictEqual(decision.send && decision.src, 'body\n');
  });

  it('refuses a byte-order mark rather than sending it as content', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), Buffer.from('## Two\nbody\n', 'utf8')]));
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nbody\n']), baseline('## Two\n'));
    assert.deepStrictEqual(decision, { send: false, refusal: { because: 'byte-order-mark' } });
  });

  it('refuses bytes that are not UTF-8', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    /*
     * WITH ITS MARK, which is how an editor writes UTF-16. The first
     * version of this cell wrote UTF-16LE without one and expected the
     * same answer; measuring showed that all-ASCII UTF-16LE is a run of
     * ASCII bytes separated by NULs and is therefore VALID UTF-8. That
     * file is still not sent -- the twin below pins which refusal
     * catches it -- but it cannot be recognised as UTF-16 here.
     */
    fs.writeFileSync(file, Buffer.concat([Buffer.from([0xff, 0xfe]), Buffer.from('## Two\nbody\n', 'utf16le')]));
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nbody\n']), baseline('## Two\n'));
    assert.deepStrictEqual(decision, { send: false, refusal: { because: 'not-utf8' } });
  });

  it('still sends nothing for UTF-16 with no mark, by a different refusal', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, Buffer.from('## Two\nbody\n', 'utf16le'));
    const decision = new Saving(new RecordingFs()).decide(
      document(file, ['## Two\nbody\n']),
      baseline('## Two\n')
    );
    assert.deepStrictEqual(decision, {
      send: false,
      refusal: { because: 'disk-differs-from-snapshot' }
    });
  });

  it('refuses when the disk holds only part of the file, and says so as its own reason', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbo', 'utf8');
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nbody\n']), baseline('## Two\n'));
    assert.deepStrictEqual(decision, {
      send: false,
      refusal: { because: 'disk-differs-from-snapshot' }
    });
  });

  it('refuses when the prefix the file starts with is not the one it was published with', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Renamed\nbody\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Renamed\nbody\n']), baseline('## Two\n'));
    assert.deepStrictEqual(decision, {
      send: false,
      refusal: { because: 'prefix-changed', prefix: '## Two\n' }
    });
  });

  it('refuses a file whose publication never finished', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const half = { ...baseline('## Two\n'), phase: 'publishing' as const };
    assert.deepStrictEqual(saving.decide(document(file, ['## Two\nbody\n']), half), {
      send: false,
      refusal: { because: 'publication-incomplete' }
    });
  });

  it('refuses a file with no record beside it rather than guessing a prefix', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    assert.deepStrictEqual(saving.decide(document(file, ['## Two\nbody\n']), null), {
      send: false,
      refusal: { because: 'no-sidecar' }
    });
  });

  /*
   * AND A BLOCK WITH NO PREFIX IS NOT A BLOCK THAT REFUSES NOTHING. An
   * empty prefix matches every text, so the refusal above can never fire
   * for it -- which is exactly the shape that turned a heading into body
   * in S1. The whole file is the body and that is correct here; the cell
   * exists so that the empty case is a decision rather than a fallthrough.
   */
  it('sends the whole file as the body when the block has no prefix', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, 'just a body\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['just a body\n']), baseline(''));
    assert.ok(decision.send);
    assert.strictEqual(decision.send && decision.src, 'just a body\n');
  });
});

describe('C17 the text is taken once and it is the text that is sent', () => {
  it('asks the document for its text exactly once', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const doc = document(file, ['## Two\nbody\n']);
    new Saving(new RecordingFs()).decide(doc, baseline('## Two\n'));
    assert.strictEqual(
      doc.calls,
      1,
      'the text was asked for more than once, so the bytes verified are not the bytes sent'
    );
  });

  /*
   * THE IMPLEMENTATION C17 NAMES. If the text is read again after the
   * check, a user who typed in between gets their half-finished line
   * sent instead of the text that was verified. The document here
   * answers differently the second time, so only a build that reads
   * twice can fail.
   */
  it('sends the text it verified, not a later reading of it', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const doc = document(file, ['## Two\nbody\n', '## Two\nthe user kept typing\n']);
    const decision = new Saving(new RecordingFs()).decide(doc, baseline('## Two\n'));
    assert.ok(decision.send, `nothing was sent: ${JSON.stringify(decision)}`);
    assert.strictEqual(
      decision.send && decision.src,
      'body\n',
      'what went out was a second reading of the document, not the snapshot that was checked'
    );
  });

  it('refuses bytes that decode as valid UTF-8 but are not the snapshot', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nsomething else entirely\n', 'utf8');
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nbody\n']), baseline('## Two\n'));
    assert.deepStrictEqual(decision, {
      send: false,
      refusal: { because: 'disk-differs-from-snapshot' }
    });
  });

  /*
   * MALFORMED BYTES DECODE WITHOUT COMPLAINT. Node replaces an invalid
   * sequence with U+FFFD and returns a perfectly good string, so a check
   * written as "decode it and compare" passes whenever the replacement
   * happens to land outside the compared region. The refusal has to be
   * about the BYTES.
   */
  it('refuses bytes that only decode by substituting a replacement character', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, Buffer.concat([Buffer.from('## Two\nbo', 'utf8'), Buffer.from([0xff, 0xfe]), Buffer.from('dy\n', 'utf8')]));
    const saving = new Saving(new RecordingFs());
    const decision = saving.decide(document(file, ['## Two\nbody\n']), baseline('## Two\n'));
    assert.deepStrictEqual(decision, { send: false, refusal: { because: 'not-utf8' } });
  });

  it('reports the digest of the bytes on disk, which is what a draft is judged against', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const decision = new Saving(new RecordingFs()).decide(
      document(file, ['## Two\nbody\n']),
      baseline('## Two\n')
    );
    assert.ok(decision.send);
    if (decision.send) {
      const expected = require('crypto')
        .createHash('sha256')
        .update(fs.readFileSync(file))
        .digest('hex');
      assert.strictEqual(
        decision.rawDigest,
        expected,
        'acknowledged-raw would not be the digest of the bytes a draft scan compares'
      );
    }
  });
});

/*
 * THE SEAM IS FILLED IN, AND ITS CURRENT VALUE IS WRITTEN DOWN.
 *
 * A later batch replaces the verb and adds an expectation token. This
 * cell exists so that the change is a change to one named thing rather
 * than a search through the save path -- and so that the day it happens,
 * exactly one cell goes red and says what it was.
 */
describe('what a save asks the store to do is named in one place', () => {
  it('asks for `set src` with no expectation in this batch', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    fs.writeFileSync(file, '## Two\nbody\n', 'utf8');
    const decision = new Saving(new RecordingFs()).decide(
      document(file, ['## Two\nbody\n']),
      baseline('## Two\n')
    );
    assert.ok(decision.send);
    if (decision.send) {
      assert.deepStrictEqual(decision.intent, { verb: 'set', field: 'src', expectation: null });
    }
  });
});

/*
 * C5: autosave. Every COMPLETED save fires the handler, and each one
 * that finds a clean document sends once.
 *
 * WHY IT NEEDS ITS OWN CELLS. Under `afterDelay` the editor saves on a
 * timer, so the handler runs far more often and often finds the document
 * already dirty again. A build that sent on a schedule of its own, or
 * that coalesced several saves into one, would pass every cell above --
 * those all look at a single save.
 */
describe('C5 autosave sends once per completed save, and only when clean', () => {
  it('sends for each completed save that leaves the document clean', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    const saving = new Saving(new RecordingFs());
    const sent: string[] = [];
    for (const text of ['## Two\nfirst\n', '## Two\nsecond\n', '## Two\nthird\n']) {
      fs.writeFileSync(file, text, 'utf8');
      const decision = saving.decide(document(file, [text]), baseline('## Two\n'));
      if (decision.send) {
        sent.push(decision.src);
      }
    }
    assert.deepStrictEqual(
      sent,
      ['first\n', 'second\n', 'third\n'],
      'three completed saves did not produce three sends'
    );
  });

  /*
   * AND THE ONES THAT FIND IT DIRTY SEND NOTHING -- which under autosave
   * is the common case, not the exception: the timer fires while the
   * user is still typing.
   */
  it('sends nothing for the saves that find the document already edited again', () => {
    const dir = scratch();
    const file = path.join(dir, '1.md');
    const saving = new Saving(new RecordingFs());
    let sends = 0;
    for (const [text, dirty] of [
      ['## Two\none\n', false],
      ['## Two\ntwo\n', true],
      ['## Two\nthree\n', true],
      ['## Two\nfour\n', false]
    ] as Array<[string, boolean]>) {
      fs.writeFileSync(file, text, 'utf8');
      if (saving.decide(document(file, [text], dirty), baseline('## Two\n')).send) {
        sends += 1;
      }
    }
    assert.strictEqual(sends, 2, 'a save was sent for a document that had been edited since');
  });
});
