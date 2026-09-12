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
 * The vendored reader against the fixture that adjudicates it.
 *
 * THE FIXTURE IS THE ORACLE AND THIS TREE IS NOT. The vectors were
 * generated from (igropyr sexpr), the authority for the format; the copy
 * of the reader in this repository is a copy, and the only thing that
 * would notice if it were edited -- or if it were replaced with a newer
 * upstream that changed behaviour -- is this file.
 *
 * ONLY THE READ SIDE IS SWEPT HERE. This extension never writes wire
 * text; `write` is exercised because the fixture's accept set records
 * the canonical rewrite of every input it accepts, and a reader that
 * built a subtly different value would produce a different rewrite.
 */

import * as assert from 'assert';
import { createHash } from 'crypto';
import * as fs from 'fs';
import * as path from 'path';
import { SexprApi, initWire } from '../../src/wire';

interface ReadProbe {
  name: string;
  input_b64?: string;
  input_rule?: { prefix: string; repeat: string; count: number; suffix: string };
  canonical_b64?: string;
  canonical_rule?: { prefix: string; repeat: string; count: number; suffix: string };
  accepted: boolean;
  rewritable?: boolean;
  error?: string;
}

interface Fixture {
  authority: string;
  authority_commit: string;
  counts: { read: number; write: number; write_reject: number; anchors: number };
  read: ReadProbe[];
}

/*
 * THE ESCAPE TABLE IS THE SECOND FIXTURE, and it came with the widening
 * that made this reader accept what a conforming R6RS writer emits --
 * which is what the core's command line prints with. Its reject rows are
 * the control: a reader that simply stopped checking after a backslash
 * satisfies every accepting row and fails those.
 */
interface EscapeFixture {
  accept: { src: string; want: string; bytes: number[] }[];
  symbols: { src: string; sym_index: number; len: number; want: string; bytes: number[] }[];
  reject: { src: string; why: string }[];
  name_outside_grammar: { src: string; why: string }[];
}

const decoder = new TextDecoder('utf-8', { fatal: true });

function wireOf(
  probe: ReadProbe,
  b64Field: 'input_b64' | 'canonical_b64',
  ruleField: 'input_rule' | 'canonical_rule'
): string {
  const rule = probe[ruleField];
  if (rule) {
    return rule.prefix + rule.repeat.repeat(rule.count) + rule.suffix;
  }
  const b64 = probe[b64Field];
  assert.ok(b64 !== undefined, `${probe.name} has neither ${b64Field} nor ${ruleField}`);
  return decoder.decode(Uint8Array.from(Buffer.from(b64 as string, 'base64')));
}

describe('the vendored goeteia reader is the one the fixture adjudicates', () => {
  let sexpr: SexprApi;
  let fixture: Fixture;

  before(async () => {
    sexpr = await initWire();
    const file = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia', 'sexpr-vectors.json');
    fixture = JSON.parse(fs.readFileSync(file, 'utf8')) as Fixture;
  });

  /*
   * THE FIXTURES ARE THE ONES THE NOTE NAMES, BY DIGEST. Counting rows
   * against the fixture's own declared count proves only that someone
   * adjusted both together; a trimmed table with a trimmed count passed.
   * The reader's provenance note carries the md5 of each table it was
   * vendored with, so that is what they are checked against.
   */
  it('carries the fixtures the vendored reader names, byte for byte', () => {
    const dir = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia');
    const note = fs.readFileSync(path.join(dir, 'sexpr.mjs'), 'utf8');
    const claimed = /sexpr-vectors\.json \(md5 ([0-9a-f]{32})\) and sexpr-escape-vectors\.json \(md5 ([0-9a-f]{32})\)/.exec(
      note.replace(/\n\/\/ /g, ' ')
    );
    assert.ok(claimed !== null, 'the provenance note does not name both fixture digests');

    for (const [name, want] of [
      ['sexpr-vectors.json', claimed[1]],
      ['sexpr-escape-vectors.json', claimed[2]]
    ] as [string, string][]) {
      const digest = createHash('md5').update(fs.readFileSync(path.join(dir, name))).digest('hex');
      assert.strictEqual(digest, want, `${name} is not the table the vendored reader was taken with`);
    }
  });

  it('carries a whole fixture, not a trimmed one', () => {
    assert.strictEqual(fixture.authority, '(igropyr sexpr)');
    assert.strictEqual(
      fixture.read.length,
      fixture.counts.read,
      'the fixture declares a read-probe count that its own list does not match'
    );
    assert.ok(fixture.read.length >= 60, 'the read group is smaller than the generator recorded');
  });

  it('accepts every input the authority accepts, with the same canonical rewrite', () => {
    let rewritten = 0;
    let unwritable = 0;
    for (const probe of fixture.read) {
      if (!probe.accepted) {
        continue;
      }
      const value = sexpr.read(wireOf(probe, 'input_b64', 'input_rule'));
      if (probe.rewritable) {
        assert.strictEqual(
          sexpr.write(value),
          wireOf(probe, 'canonical_b64', 'canonical_rule'),
          `canonical rewrite differs for ${probe.name}`
        );
        rewritten += 1;
      } else {
        assert.throws(
          () => sexpr.write(value),
          sexpr.SexprError,
          `${probe.name}: the authority refuses to write this back and we do not`
        );
        unwritable += 1;
      }
    }
    assert.ok(rewritten > 0, 'no probe exercised the canonical rewrite');
    assert.ok(unwritable >= 2, 'no probe covers "parses but cannot be written"');
  });

  it('refuses every input the authority refuses, at the same position', () => {
    let checked = 0;
    for (const probe of fixture.read) {
      if (probe.accepted) {
        continue;
      }
      let caught: unknown = null;
      try {
        sexpr.read(wireOf(probe, 'input_b64', 'input_rule'));
      } catch (e) {
        caught = e;
      }
      assert.ok(caught, `${probe.name}: accepted an input the authority refuses`);
      assert.ok(
        caught instanceof sexpr.SexprError,
        `${probe.name}: threw ${(caught as Error).name}, not SexprError`
      );
      const declared = probe.error as string;
      const at = Number(declared.slice(declared.lastIndexOf('@') + 1));
      assert.strictEqual(
        (caught as Error & { position: number }).position,
        at,
        `${probe.name}: position disagrees with the authority`
      );
      checked += 1;
    }
    assert.ok(checked > 0, 'the fixture carries no reject probes');
  });

  /*
   * THE BYTES, NOT THE COMMENT ABOUT THE BYTES. This cell used to check
   * only that the provenance note was still present -- so an edited
   * reader that kept its note passed, which is the one thing a
   * provenance check exists to stop. The note says which md5 the
   * original had; that number is now compared against the file the note
   * is written in, with the note's own lines removed, so the two cannot
   * drift apart without this failing.
   */
  it('accepts every escape the authority accepts, byte for byte', () => {
    const file = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia', 'sexpr-escape-vectors.json');
    const table = JSON.parse(fs.readFileSync(file, 'utf8')) as EscapeFixture;
    assert.ok(table.accept.length >= 20, `the accept group holds only ${table.accept.length} rows`);

    const encoder = new TextEncoder();
    for (const row of table.accept) {
      const value = sexpr.read(row.src) as unknown[];
      /*
       * The rows are `(ok "...")`, so the string is the second element.
       * The first version of this line indexed INTO the string when it
       * found one, and compared a single character with the row's bytes.
       */
      const text = value[1];
      assert.strictEqual(typeof text, 'string', `${row.src}: the second element is not a string`);
      assert.deepStrictEqual(
        Array.from(encoder.encode(text as string)),
        row.bytes,
        `${row.src} (${row.want}) did not decode to the bytes the authority recorded`
      );
    }
  });

  it('reads the symbol escapes the authority records', () => {
    const file = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia', 'sexpr-escape-vectors.json');
    const table = JSON.parse(fs.readFileSync(file, 'utf8')) as EscapeFixture;
    assert.ok(table.symbols.length > 0, 'the symbol group is empty');

    const encoder = new TextEncoder();
    for (const row of table.symbols) {
      const value = sexpr.read(row.src) as unknown[];
      /*
       * `len` IS THE NUMBER OF ELEMENTS IN THE DATUM, not the length of
       * the symbol -- the first reading of this column cost a red cell.
       * It is worth checking for its own sake: an escape whose
       * semicolon swallowed the rest of the line would give a shorter
       * list, which is one of the rows here ("the escape's semicolon
       * ends the ESCAPE, not the token").
       */
      assert.strictEqual(
        value.length,
        row.len,
        `${row.src} (${row.want}): the datum has ${value.length} elements, not ${row.len}`
      );
      const symbol = value[row.sym_index] as { name: string };
      assert.ok(symbol instanceof sexpr.Sym, `${row.src}: element ${row.sym_index} is not a symbol`);
      assert.deepStrictEqual(
        Array.from(encoder.encode(symbol.name)),
        row.bytes,
        `${row.src} (${row.want}) did not decode to the bytes the authority recorded`
      );
    }
  });

  /*
   * THE CONTROL. Without these a reader that stopped checking after a
   * backslash would satisfy every row above.
   */
  it('still refuses what the authority refuses', () => {
    const file = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia', 'sexpr-escape-vectors.json');
    const table = JSON.parse(fs.readFileSync(file, 'utf8')) as EscapeFixture;
    assert.ok(table.reject.length >= 10, `the reject group holds only ${table.reject.length} rows`);

    for (const row of [...table.reject, ...table.name_outside_grammar]) {
      assert.throws(
        () => sexpr.read(row.src),
        sexpr.SexprError,
        `${row.src} was accepted, although the authority refuses it: ${row.why}`
      );
    }
  });

  /*
   * MEASURED, AND ONE OF THEM NOT AS REPORTED TO ME. An escaped astral
   * character was described as refused; it is refused in a SYMBOL, where
   * the decoded character is not a symbol character, and ACCEPTED in a
   * string, where it is an ordinary character. The capital `\X` is
   * refused in both: R6RS spells the inline hex escape with a lowercase
   * x and the reader follows it.
   */
  it('reads an escaped astral character in a string and refuses it in a symbol', () => {
    const inString = sexpr.read('(ok "a\\x1F600;b")') as unknown[];
    assert.strictEqual(inString[1], 'a\u{1F600}b');
    assert.throws(() => sexpr.read('(rel \\x1F600;)'), sexpr.SexprError);
  });

  it('refuses a hex escape spelled with a capital X', () => {
    assert.throws(() => sexpr.read('(ok "a\\X41;b")'), sexpr.SexprError);
    assert.throws(() => sexpr.read('(rel \\X41;)'), sexpr.SexprError);
    assert.strictEqual((sexpr.read('(ok "a\\x41;b")') as unknown[])[1], 'aAb');
  });

  it('refuses a decoded symbol name the bare syntax cannot hold', () => {
    for (const src of ['(rel \\x20;)', '(rel \\x28;)', '(rel 提及)', '(rel has\\x20;part)']) {
      assert.throws(() => sexpr.read(src), sexpr.SexprError, `${src} was accepted`);
    }
    assert.ok(sexpr.read('(rel has-part)'), 'an ordinary symbol was refused');
  });

  it('is the copy the note in it claims, byte for byte', () => {
    const file = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia', 'sexpr.mjs');
    const text = fs.readFileSync(file, 'utf8');

    const claimed = /rt\/sexpr\.mjs at commit ([0-9a-f]{7,40}), md5 ([0-9a-f]{32})/.exec(text);
    assert.ok(claimed !== null, 'the vendored reader does not name the commit and digest it came from');
    assert.ok(
      /VENDORED VERBATIM, NOT FORKED\. Source: https:\/\/github\.com\/guenchi\/Goeteia/.test(text),
      'the vendored reader has lost the note saying where it came from'
    );

    /*
     * The note is the only thing this copy adds, and it is a block of
     * whole comment lines inserted after the licence. Removing exactly
     * those lines has to give back the original file.
     */
    const noteLines = [
      /^\/\/ VENDORED VERBATIM, NOT FORKED\./,
      /^\/\/ rt\/sexpr\.mjs at commit /,
      /^\/\/ The package exports "\.\/sexpr" now, but the published/,
      /^\/\/ export, so a dependency on it would not resolve/,
      /^\/\/ a release carries the export, and is then deleted/,
      /^\/\/ Nothing below this line is edited\./,
      /^\/\/ sexpr-vectors\.json \(md5 /
    ];
    const lines = text.split('\n');
    const kept: string[] = [];
    let removed = 0;
    let blankAfterNote = false;
    for (const line of lines) {
      if (noteLines.some((r) => r.test(line))) {
        removed += 1;
        blankAfterNote = true;
        continue;
      }
      if (blankAfterNote && line === '') {
        blankAfterNote = false;
        continue;
      }
      blankAfterNote = false;
      kept.push(line);
    }
    assert.strictEqual(removed, noteLines.length, 'the provenance note is not the shape this cell strips');

    const digest = createHash('md5').update(kept.join('\n'), 'utf8').digest('hex');
    assert.strictEqual(
      digest,
      claimed?.[2],
      'the vendored reader has been edited: its bytes are not the md5 its own note claims'
    );
  });
});
