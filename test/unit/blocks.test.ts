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
 * The buffer a block is opened into, and what saving it means. This is
 * the half of S1 and S2 that has no editor in it.
 */

import * as assert from 'assert';
import {
  Block,
  documentFor,
  hasFieldConflict,
  isTopLevel,
  parentOf,
  readBlock,
  splitDocument,
  titleOf
} from '../../src/blocks';
import { initWire, wire } from '../../src/wire';

const STORE = '/tmp/a-store';

function block(text: string): Block {
  const value = wire().read(text);
  const found = readBlock(value);
  assert.ok(found !== null, `not a block: ${text}`);
  return found as Block;
}

describe('a block read from the core', () => {
  before(async () => {
    await initWire();
  });

  it('reads an imported block, which has a heading of its own', () => {
    const b = block(
      '((id . "a.2") (deleted . #f) (fields (heading-src . "## Two\\n") (kind . section) (level . 2) (src . "body\\n\\n") (title . "Two")) (position "a.1" . 0) (edges))'
    );
    assert.strictEqual(b.id, 'a.2');
    assert.deepStrictEqual(b.placement, { kind: 'under', parent: 'a.1' });
    assert.strictEqual(b.ord, 0);
    assert.strictEqual(titleOf(b), 'Two');
    const document = documentFor(b, STORE);
    assert.strictEqual(document.headingSrc, '## Two\n');
    assert.strictEqual(document.src, 'body\n\n');
    assert.strictEqual(document.text, '## Two\nbody\n\n');
  });

  it('reads an inserted block, which has no heading at all', () => {
    const b = block(
      '((id . "a.2") (deleted . #f) (fields (kind . section) (src . "body") (title . "Two")) (position "a.1" . 0) (edges))'
    );
    const document = documentFor(b, STORE);
    assert.strictEqual(document.headingSrc, '');
    assert.strictEqual(document.text, 'body');
  });

  it('reads a block whose place is unsettled as neither root nor under anything', () => {
    const b = block(
      '((id . "a.2") (deleted . #f) (fields (title . "Two")) (position conflict 2) (edges))'
    );
    assert.deepStrictEqual(b.placement, { kind: 'unsettled', candidates: 2 });
    assert.strictEqual(
      b.ord,
      null,
      'the count of position candidates was handed back as a sibling index'
    );
    assert.strictEqual(
      isTopLevel(b),
      false,
      'a block that has been moved twice was read as being at the top level'
    );
    assert.strictEqual(parentOf(b), null);
  });

  it('reads a top-level block as a child of nothing', () => {
    const b = block('((id . "a.1") (deleted . #f) (fields (src . "")) (position root . 0) (edges))');
    assert.deepStrictEqual(b.placement, { kind: 'root' });
    assert.strictEqual(b.ord, 0);
  });

  it('falls back to the path when a file-level block has no title', () => {
    const b = block(
      '((id . "a.1") (deleted . #f) (fields (front . "") (kind . doc) (path . "doc.md") (src . "")) (position root . 0) (edges))'
    );
    assert.strictEqual(titleOf(b), 'doc.md');
  });

  it('sees a field whose value is a set of candidates as a conflict', () => {
    const b = block(
      '((id . "a.2") (deleted . #f) (fields (title conflict ("One" "Two"))) (position "a.1" . 0) (edges))'
    );
    assert.strictEqual(hasFieldConflict(b), true);
  });

  it('does not call an ordinary block conflicted', () => {
    const b = block('((id . "a.2") (deleted . #f) (fields (title . "Two")) (position "a.1" . 0) (edges))');
    assert.strictEqual(hasFieldConflict(b), false);
  });
});

describe('S1 and S2 the heading decides whether a save is sent', () => {
  before(async () => {
    await initWire();
  });

  const withHeading = {
    id: 'a.2',
    store: STORE,
    prefix: '## Two\n',
    front: '',
    headingSrc: '## Two\n',
    src: 'body\n',
    text: '## Two\nbody\n'
  };
  const withoutHeading = {
    id: 'a.2',
    store: STORE,
    prefix: '',
    front: '',
    headingSrc: '',
    src: 'body\n',
    text: 'body\n'
  };

  it('sends the body alone when only the body changed', () => {
    const result = splitDocument(withHeading, '## Two\nbody2\n');
    assert.deepStrictEqual(result, { ok: true, src: 'body2\n', normalised: false });
  });

  it('refuses when the heading line changed', () => {
    assert.deepStrictEqual(splitDocument(withHeading, '## Three\nbody\n'), {
      ok: false,
      reason: 'heading-changed'
    });
  });

  it('refuses when the heading line was removed', () => {
    assert.deepStrictEqual(splitDocument(withHeading, 'body\n'), {
      ok: false,
      reason: 'heading-changed'
    });
  });

  it('treats the whole buffer as the body for a block that has no heading', () => {
    const result = splitDocument(withoutHeading, 'line one\nline two\n');
    assert.deepStrictEqual(result, { ok: true, src: 'line one\nline two\n', normalised: false });
  });

  it('does not mistake the first body line of a headingless block for a heading', () => {
    const result = splitDocument(withoutHeading, '# looks like a heading\nrest\n');
    assert.strictEqual(result.ok, true);
    assert.strictEqual((result as { src: string }).src, '# looks like a heading\nrest\n');
  });

  it('keeps a body that is exactly the heading text of some other block', () => {
    const result = splitDocument(withHeading, '## Two\n## Two\n');
    assert.strictEqual((result as { src: string }).src, '## Two\n');
  });

  it('reads a buffer the editor rewrote with CRLF, and says that it did', () => {
    const result = splitDocument(withHeading, '## Two\r\nbody2\r\n');
    assert.deepStrictEqual(result, { ok: true, src: 'body2\n', normalised: true });
  });

  it('leaves carriage returns alone when the block itself had one', () => {
    const crlf = {
      id: 'a.2',
      store: STORE,
      prefix: '## Two\r\n',
      front: '',
      headingSrc: '## Two\r\n',
      src: 'body\r\n',
      text: '## Two\r\nbody\r\n'
    };
    const result = splitDocument(crlf, '## Two\r\nbody2\r\n');
    assert.deepStrictEqual(result, { ok: true, src: 'body2\r\n', normalised: false });
  });

  it('carries a body with quotes, backslashes and non-ascii through unchanged', () => {
    const body = 'a "quoted" \\ back slash 中文 😀\n';
    const result = splitDocument(withHeading, `## Two\n${body}`);
    assert.strictEqual((result as { src: string }).src, body);
  });
});

describe('a document block is opened with its front matter', () => {
  before(async () => {
    await initWire();
  });

  /*
   * MEASURED against a real store: importing a file whose front matter
   * is `---\ntitle: front matter here\n---\n` gives a file-level block
   * whose fields are `(front . "---\n...\n---\n") (kind . doc)
   * (path . "d.md") (src . "\n")`, and whose `read --md` prints the
   * front matter and then the body. Composing the buffer from
   * heading-src and src alone showed that document as a buffer holding
   * one newline, which invites pasting the front matter back in -- and
   * it would then exist twice, once in the field and once in the body.
   */
  const FRONT = '---\ntitle: front matter here\n---\n';

  it('shows the front matter, then the heading, then the body', () => {
    const b = block(
      `((id . "a.1") (deleted . #f) (fields (front . "---\\ntitle: front matter here\\n---\\n") (kind . doc) (path . "d.md") (src . "\\nbody\\n")) (position root . 0) (edges))`
    );
    const document = documentFor(b, STORE);
    assert.strictEqual(document.front, FRONT);
    assert.strictEqual(document.text, `${FRONT}\nbody\n`);
    assert.strictEqual(document.prefix, FRONT, 'the front matter is not part of the protected prefix');
  });

  it('sends only the body back when the front matter is left alone', () => {
    const document = {
      id: 'a.1',
      store: STORE,
      prefix: FRONT,
      front: FRONT,
      headingSrc: '',
      src: '\nbody\n',
      text: `${FRONT}\nbody\n`
    };
    const split = splitDocument(document, `${FRONT}\nedited\n`);
    assert.deepStrictEqual(split, { ok: true, src: '\nedited\n', normalised: false });
  });

  it('refuses when the front matter itself was edited', () => {
    const document = {
      id: 'a.1',
      store: STORE,
      prefix: FRONT,
      front: FRONT,
      headingSrc: '',
      src: '\nbody\n',
      text: `${FRONT}\nbody\n`
    };
    assert.deepStrictEqual(splitDocument(document, '---\ntitle: changed\n---\n\nbody\n'), {
      ok: false,
      reason: 'heading-changed'
    });
  });

  it('protects the front matter and the heading together when a block has both', () => {
    const document = {
      id: 'a.2',
      store: STORE,
      prefix: `${FRONT}## Two\n`,
      front: FRONT,
      headingSrc: '## Two\n',
      src: 'body\n',
      text: `${FRONT}## Two\nbody\n`
    };
    assert.strictEqual(splitDocument(document, `${FRONT}## Three\nbody\n`).ok, false);
    assert.deepStrictEqual(splitDocument(document, `${FRONT}## Two\nnew\n`), {
      ok: true,
      src: 'new\n',
      normalised: false
    });
  });
});

/*
 * THE CELLS THAT USED TO BE HERE REBUILT THE BASELINE THEMSELVES and
 * then checked the rebuild they had just performed. The rebuild lives in
 * the extension, beside the editor; a helper here that reimplements it
 * is a copy, and mutating the real line in extension.ts left both of
 * those cells green. They are gone rather than kept alongside the real
 * one -- a cell that cannot fail for the reason it names is worse than
 * an absent cell, because the count says it is covered.
 *
 * What replaced them: "a document block with front matter, edited twice"
 * in test/integration/extension.test.ts, which drives the real command,
 * the real save handler and the real rebuild. Mutating extension.ts
 * kills it.
 */
