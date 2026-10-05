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
 * A READ'S RECEIPT CHANGES NOTHING THIS EXTENSION READS.
 *
 * From theourgia 59e69f3 every read of the committed store ends with a
 * receipt: `(cut ((<writer> . <seq>) ...))` and `(versions ((<id> . <hash>)
 * ...))`, appended after the verb's own clauses, with the dispatcher's
 * `(incomplete ...)` still last (README, "The receipt"; rpc.sc `receipt`).
 * `outline` and `reach` carry the cut only, `search`, `grep` and `whereis`
 * the versions only. The human output is unchanged except for plain `read`
 * and `reach`, which are printed whole.
 *
 * Every reader of one of those answers was read for the receipt when it
 * arrived: each takes clauses by name, the block at position 1, or the items
 * the client unwraps by name, so none needed a change. These cells hold that:
 * each hands a reader the same answer without and with a receipt, in the
 * core's order, and asserts the reader answers the same. A reader that
 * later takes the last element, counts the clauses or compares the whole
 * form is red here.
 *
 * NEVER: THE RECEIPT IS NOT READ. Nothing here or in src takes a cut or a
 * version list from it. That is deliberate: what this extension would show
 * from a receipt is a separate decision, and until it is made the receipt
 * passes through unread.
 */

import * as assert from 'assert';
import { readBlock } from '../../src/blocks';
import { Client } from '../../src/client';
import { definitionsOf, leftOutOf } from '../../src/definition';
import { gatherContext } from '../../src/hover';
import { documentOf } from '../../src/document-view';
import { StoreModel } from '../../src/model';
import { RawResult } from '../../src/transport';
import { initWire } from '../../src/wire';
import { requireText } from '../../src/working';

function answering(stdout: string): Client {
  return new Client({
    kind: 'test',
    send: async (verb: string, args: string[]): Promise<RawResult> => ({ argv: [verb, ...args], rc: 0, stdout, stderr: '' })
  });
}

const BLOCK = '((id . "a.1") (deleted . #f) (fields (kind . doc) (title . "Doc") (src . "Body.\\n")) (position root . 0) (edges))';
const CHILD = '((id . "a.2") (deleted . #f) (fields (kind . section) (title . "Part") (src . "More.\\n")) (position "a.1" . 0) (edges))';
const CUT = '(cut (("w0000001" . 3) ("w0000002" . 7)))';
const VERSIONS = '(versions (("a.1" . "h1") ("a.2" . "h2")))';
const INCOMPLETE = '(incomplete (unreadable (writer "w0000003") (path "/s/w3") (reason "Permission denied")))';

describe("a read's receipt changes nothing this extension reads", () => {
  before(async () => {
    await initWire();
  });

  it('reads a plain read the same, the block at its place, as one datum', async () => {
    const before = `(ok ${BLOCK} (version "h1"))\n`;
    const after = `(ok ${BLOCK} (version "h1") ${CUT} (versions (("a.1" . "h1"))))\n`;
    assert.strictEqual(await requireText(answering(before), 'a.1', null), true);
    assert.strictEqual(await requireText(answering(after), 'a.1', null), true);
    const plain = await answering(after).request('read', ['a.1']);
    assert.strictEqual(plain.answers.length, 1, 'a plain read with its receipt is no longer one datum');
  });

  it('reads the version a guarded write needs from a read with a receipt', async () => {
    const before = `(ok ${BLOCK} (version "h1"))\n`;
    const after = `(ok ${BLOCK} (version "h1") ${CUT} (versions (("a.1" . "h1"))))\n`;
    const read = async (stdout: string) => {
      const got = await new StoreModel(answering(stdout)).blockWithVersion('a.1');
      return { id: got.block?.id ?? null, version: got.version };
    };
    assert.deepStrictEqual(await read(after), await read(before));
    assert.deepStrictEqual(await read(after), { id: 'a.1', version: 'h1' });
  });

  it('opens a recursive read the same, and still takes the incomplete clause after the receipt', async () => {
    const before = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS})\n`;
    const after = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS} ${CUT})\n`;
    const late = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS} ${CUT} ${INCOMPLETE})\n`;
    const ids = async (stdout: string) => {
      const answer = await answering(stdout).request('read', ['a.1', '--recursive', '--wire']);
      return { ids: answer.answers.map((b) => readBlock(b)?.id ?? null), notes: answer.notes?.length ?? 0 };
    };
    assert.deepStrictEqual(await ids(after), await ids(before));
    assert.deepStrictEqual(await ids(after), { ids: ['a.1', 'a.2'], notes: 0 });
    assert.deepStrictEqual(await ids(late), { ids: ['a.1', 'a.2'], notes: 1 }, 'the incomplete clause after the receipt was not taken');
    assert.deepStrictEqual(await documentOf(answering(after), 'a.1'), await documentOf(answering(before), 'a.1'));
    const late2 = await documentOf(answering(late), 'a.1');
    assert.strictEqual(late2.ok, true);
    assert.strictEqual(late2.ok && (late2.notes ?? []).length, 1, 'the document lost the note that came after the receipt');
  });

  it('reads whereis the same with the versions after its own clauses', async () => {
    /*
     * whereis's own clauses as rpc.sc's `scan-clauses` makes them, then the
     * versions.
     */
    const own =
      '(ok (items (def "a.1" (library (probe d)) (name alpha) (kind code)) (export "b.1" (library (probe d)) (name alpha))) ' +
      '(cut ()) (scanned (blocks 2) (fields (names exports)))';
    const before = `${own})\n`;
    const after = `${own} (versions (("a.1" . "h1") ("b.1" . "h2"))))\n`;
    const after2 = await definitionsOf(answering(after), 'alpha');
    assert.deepStrictEqual(after2, await definitionsOf(answering(before), 'alpha'));
    assert.strictEqual('found' in after2 && after2.found.length, 2);
  });

  it('reads an outline the same with the cut after its text', async () => {
    const text = '- a.1  Doc\\n  - a.2  Part\\n';
    const before = `(ok (text "${text}"))\n`;
    const after = `(ok (text "${text}") ${CUT})\n`;
    const read = async (stdout: string) => (await answering(stdout).request('outline', ['--depth', '1', '--wire'])).text;
    assert.strictEqual(await read(after), await read(before));
    assert.strictEqual(await read(after), '- a.1  Doc\n  - a.2  Part\n');
  });
});

/*
 * A BLOCK'S VALIDITY CHANGES NOTHING THIS EXTENSION READS EITHER.
 *
 * From theourgia 06a348b a plain read of a block that is not valid adds
 * `(validity <v> (<why> <block>) ...)` after its own clauses and before the
 * receipt; a recursive read adds `(validity ((<id> <v> ...) ...))` for the
 * blocks that are not valid; `whereis` adds `(excluded (blocks ...))` when
 * it left a block out and names a kept hit that needs review in the same
 * listed form (README, "Class and validity"; lifecycle.sc
 * `read-validity-clause`, `listed-validity-clause`, `search-clauses`). The
 * same readers as above are handed the same answers without and with them.
 *
 * NOTE: SEARCH AND GREP ARE HERE TOO. This extension asks both with
 * `--wire`, whose answer carries the `excluded` count and the listed
 * validity beside the items; the hits and the matches are read the same with
 * them, and the search's count of what was left out is read from the first.
 * What they do on a real store -- a superseded block drops out -- is read
 * against the real core in real-core.test.ts.
 */
describe("a block's validity changes nothing this extension reads", () => {
  before(async () => {
    await initWire();
  });

  const SUPERSEDED = '(validity superseded (superseded-by "b.2"))';
  const LISTED = '(validity (("a.2" needs-review (premise-superseded "b.3"))))';

  it('reads a plain read of a superseded block the same, its version included', async () => {
    const before = `(ok ${BLOCK} (version "h1") ${CUT} (versions (("a.1" . "h1"))))\n`;
    const after = `(ok ${BLOCK} (version "h1") ${SUPERSEDED} ${CUT} (versions (("a.1" . "h1"))))\n`;
    assert.strictEqual(await requireText(answering(after), 'a.1', null), true);
    const read = async (stdout: string) => {
      const got = await new StoreModel(answering(stdout)).blockWithVersion('a.1');
      return { id: got.block?.id ?? null, version: got.version };
    };
    assert.deepStrictEqual(await read(after), await read(before));
    assert.deepStrictEqual(await read(after), { id: 'a.1', version: 'h1' });
    const plain = await answering(after).request('read', ['a.1']);
    assert.strictEqual(plain.answers.length, 1, 'a plain read with its validity is no longer one datum');
  });

  it('opens a recursive read the same with the listed validity, and still takes the incomplete clause', async () => {
    const before = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS} ${CUT})\n`;
    const after = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS} ${LISTED} ${CUT})\n`;
    const late = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS} ${LISTED} ${CUT} ${INCOMPLETE})\n`;
    const ids = async (stdout: string) => {
      const answer = await answering(stdout).request('read', ['a.1', '--recursive', '--wire']);
      return { ids: answer.answers.map((b) => readBlock(b)?.id ?? null), notes: answer.notes?.length ?? 0 };
    };
    assert.deepStrictEqual(await ids(after), await ids(before));
    assert.deepStrictEqual(await ids(late), { ids: ['a.1', 'a.2'], notes: 1 }, 'the incomplete clause after the validity was not taken');
    assert.deepStrictEqual(await documentOf(answering(after), 'a.1'), await documentOf(answering(before), 'a.1'));
  });

  it('reads a search the same with an excluded count and a hit that needs review, and takes the count', async () => {
    const own =
      '(ok (items (hit "a.1" 7 "alpha here" (fields (src))) (hit "a.2" 3 "alpha there" (fields (title)))) ' +
      '(cut ()) (scanned (blocks 7) (fields (title keywords src names doc body)) (unreadable-blocks 0))';
    const before = `${own} (versions (("a.1" . "h1") ("a.2" . "h2"))))\n`;
    const after =
      `${own} (excluded (blocks (superseded 2) (refuted 3))) (validity (("a.2" needs-review (premise-moved "c.1")))) ` +
      '(versions (("a.1" . "h1") ("a.2" . "h2"))))\n';
    const got = await new StoreModel(answering(after)).searchReading('alpha');
    const plain = await new StoreModel(answering(before)).searchReading('alpha');
    assert.deepStrictEqual(got.hits, plain.hits);
    assert.deepStrictEqual(got.hits.map((h) => h.id), ['a.1', 'a.2']);
    assert.deepStrictEqual(got.leftOut, { superseded: 2, refuted: 3 });
    assert.strictEqual(plain.leftOut, null);
  });

  it("reads a grep the same with an excluded count and a hit that needs review, in the hover's mentions", async () => {
    const own =
      '(ok (items (match "m.1" 1 "alpha here") (match "m.2" 2 "alpha there")) ' +
      '(cut ()) (scanned (blocks 7) (fields (src doc body)) (unreadable-blocks 0))';
    const before = `${own} (versions (("m.1" . "h1") ("m.2" . "h2"))))\n`;
    const after =
      `${own} (excluded (blocks (superseded 2) (refuted 3))) (validity (("m.2" needs-review (premise-moved "c.1")))) ` +
      '(versions (("m.1" . "h1") ("m.2" . "h2"))))\n';
    const grepAnswering = (grep: string): Client =>
      new Client({
        kind: 'test',
        send: async (verb: string, args: string[]): Promise<RawResult> => ({
          argv: [verb, ...args],
          rc: verb === 'whereis' ? 1 : 0,
          /*
           * whereis of a name nothing defines is refused, as the core
           * refuses it; refs and the reads of the mentioned blocks get no
           * bytes, which the hover takes as no rows and no description
           */
          stdout: verb === 'grep' ? grep : verb === 'whereis' ? `(error unknown-name ${args[0]} (nearest))\n` : '',
          stderr: ''
        })
      });
    const mentions = async (grep: string): Promise<string[]> =>
      (await gatherContext(grepAnswering(grep), 't.1', 'alpha', false, () => false)).candidates
        .filter((c) => c.section === 'mention')
        .map((c) => c.id);
    assert.deepStrictEqual(await mentions(after), await mentions(before));
    assert.deepStrictEqual(await mentions(after), ['m.1', 'm.2']);
    /*
     * AND THE COUNT IS THERE TO READ: the hover shows none of it, and the
     * envelope the client hands back for the grep carries it whole.
     */
    const asked = await grepAnswering(after).request('grep', ['alpha', '--wire']);
    assert.deepStrictEqual(leftOutOf(asked.envelope), { superseded: 2, refuted: 3 });
    assert.strictEqual(leftOutOf((await grepAnswering(before).request('grep', ['alpha', '--wire'])).envelope), null);
  });

  it('reads whereis the same with an excluded count and a hit that needs review', async () => {
    const own =
      '(ok (items (def "a.1" (library (probe d)) (name alpha) (kind code)) (export "b.1" (library (probe d)) (name alpha))) ' +
      '(cut ()) (scanned (blocks 3) (fields (names exports)))';
    const before = `${own} (versions (("a.1" . "h1") ("b.1" . "h2"))))\n`;
    const after =
      `${own} (excluded (blocks (superseded 1) (refuted 0))) (validity (("b.1" needs-review (premise-moved "c.1")))) ` +
      '(versions (("a.1" . "h1") ("b.1" . "h2"))))\n';
    const got = await definitionsOf(answering(after), 'alpha');
    assert.deepStrictEqual(got, await definitionsOf(answering(before), 'alpha'));
    assert.strictEqual('found' in got && got.found.length, 2);
  });
});

/*
 * A READ ASKED WITH `--rev` IS PRINTED AS `--wire` PRINTS IT. From theourgia
 * 6593f78 an answer that carries `(rev <n> (daemon "<token>"))` is printed
 * whole in either mode, so the human route of `read --recursive --rev` is
 * one `(ok (items ...) ... (rev ...))` form, not one block per line, and
 * `read --md --rev` is an `(ok (text "...") ... (rev ...))` form, not the
 * text. This extension sends no `--rev`; these cells hold that a caller that
 * does is read the way the core printed it.
 */
describe('a read asked with --rev is read as the wire form', () => {
  before(async () => {
    await initWire();
  });

  const REV = '(rev 3 (daemon "1-2"))';

  it('opens a recursive read with --rev as its envelope, the blocks as the answers', async () => {
    const stdout = `(ok (items ${BLOCK} ${CHILD}) ${VERSIONS} ${CUT} ${REV})\n`;
    const answer = await answering(stdout).request('read', ['a.1', '--recursive', '--rev']);
    assert.deepStrictEqual(answer.answers.map((b) => readBlock(b)?.id ?? null), ['a.1', 'a.2']);
    assert.notStrictEqual(answer.envelope, null, 'the form carrying the rev was not kept as the envelope');
  });

  it('takes the text of a markdown read with --rev from its text clause', async () => {
    /*
     * As the core prints it: the text, then the receipt a markdown read
     * appends (its cut and the read block's version), then the rev.
     */
    const stdout = `(ok (text "# Doc\\n") ${CUT} (versions (("a.1" . "h1"))) ${REV})\n`;
    const answer = await answering(stdout).request('read', ['a.1', '--md', '--rev']);
    assert.strictEqual(answer.text, '# Doc\n');
    assert.strictEqual(answer.kind, 'text');
  });
});
