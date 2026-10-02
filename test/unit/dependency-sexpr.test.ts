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
 * The reader this extension depends on, against the tables that
 * adjudicate it.
 *
 * THE FIXTURE IS THE ORACLE AND THE DEPENDENCY IS NOT. The vectors were
 * generated from (igropyr sexpr), the authority for the format; the reader
 * is goeteia's, taken from the package, and this file's sweep is what
 * notices an upgrade of that package that changes how it reads anything
 * the fixtures cover (the real-core cell S14 also catches the escapes a
 * body may hold).
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
import { pathToFileURL } from 'url';
import { READER, SexprApi, initWire } from '../../src/wire';

/*
 * THE VERSION THESE TABLES WERE RUN AGAINST. Raising it is a decision,
 * not a formality: 1.7.2 reads a decoded symbol name through the
 * writer's own predicate, so names this tree once accepted are now
 * refused. Change it only together with a run of this whole suite.
 */
const PINNED_GOETEIA = '1.7.2';

/*
 * THE DIGEST OF THE FILE THAT LOADS. Kept beside the version because the
 * two answer different questions: the version says what npm installed,
 * this says what the loader actually resolves and reads.
 */
const READER_DIGEST = '45ed90f1b7c56de5d15e8fbbe6c3afc5';

/*
 * The compiler rewrites a literal `import()` into `require()` when it
 * emits CommonJS, and require cannot load an ES module here. The product
 * builds its import the same way and for the same reason.
 */
const dynamicImport = new Function('specifier', 'return import(specifier);') as (
  specifier: string
) => Promise<unknown>;

interface ReadProbe {
  name: string;
  input_b64?: string;
  input_rule?: { prefix: string; repeat: string; count: number; suffix: string };
  canonical_b64?: string;
  canonical_rule?: { prefix: string; repeat: string; count: number; suffix: string };
  accepted: boolean;
  rewritable?: boolean;
  write_error?: string;
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

describe('the goeteia reader this build depends on is the one the tables adjudicate', () => {
  let sexpr: SexprApi;
  let fixture: Fixture;

  before(async () => {
    sexpr = await initWire();
    const file = path.join(__dirname, '..', 'fixtures', 'goeteia', 'sexpr-vectors.json');
    fixture = JSON.parse(fs.readFileSync(file, 'utf8')) as Fixture;
  });

  /*
   * THE TABLES ARE THE ONES THESE READINGS WERE TAKEN AGAINST, BY
   * DIGEST. Counting rows against the table's own declared count proves
   * only that someone adjusted both together; a trimmed table with a
   * trimmed count passed. The digests used to come from the vendored
   * reader's own provenance note; the reader is a package now and
   * carries no such note, so they are written here -- which is where a
   * number that adjudicates this tree belongs anyway.
   */
  const TABLE_DIGESTS: [string, string][] = [
    ['sexpr-vectors.json', '0a6425ef929941f2c8ab2b871a889f7e'],
    ['sexpr-escape-vectors.json', '8ff54ea9f243e8f8684dac43138ae270']
  ];

  it('carries the tables the reader was checked against, byte for byte', () => {
    const dir = path.join(__dirname, '..', 'fixtures', 'goeteia');
    for (const [name, want] of TABLE_DIGESTS) {
      const digest = createHash('md5').update(fs.readFileSync(path.join(dir, name))).digest('hex');
      assert.strictEqual(digest, want, `${name} is not the table this reader was checked against`);
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

  /*
   * WHERE THE READER AND THE TABLE DISAGREE, NAMED ONE BY ONE.
   *
   * The tables were generated by `(igropyr sexpr)`, the authority for
   * this format. goeteia 1.7.2 refuses four inputs the tables record as
   * accepted. That is not something to paper over and not something to
   * decide silently, so each one is listed with what it is, and the
   * cells below assert that each is STILL REFUSED -- if a later version
   * starts accepting one again, this list fails and someone has to look
   * at it. A divergence that is merely skipped is a divergence nobody
   * will ever revisit.
   *
   * ALL FOUR ARE THE SAME MOVE, AND THE TABLE SAYS SO. Each of them is
   * recorded by the authority as `rewritable: false` with the write
   * error `symbol not wire-safe`: `+1`, `+5`, `.` and `(. a)` are read
   * by `(igropyr sexpr)` as SYMBOLS whose names it then refuses to write
   * back. `+1` is not an integer here, which is what it looks like and
   * is why it was worth checking. This version refuses them when it
   * reads instead of when it writes -- the same rule, applied earlier.
   *
   * SO THE DIVERGENCE IS CHECKED AGAINST ITS OWN REASON. The list is not
   * written out by hand: it is required to be exactly the set of probes
   * the authority accepts but will not write, and each one is required
   * to carry that write error. A future version that refuses something
   * for a DIFFERENT reason fails here rather than joining a list of
   * things somebody once decided were fine.
   */
  function refusedByThisReader(): string[] {
    const refused: string[] = [];
    for (const probe of fixture.read) {
      if (!probe.accepted) {
        continue;
      }
      try {
        sexpr.read(wireOf(probe, 'input_b64', 'input_rule'));
      } catch (e) {
        /*
         * THE KIND OF REFUSAL IS PART OF THE CLAIM. Collecting "it threw"
         * accepts a reader that fails on these four for an unrelated
         * reason -- a TypeError from a broken code path reads exactly
         * like a considered refusal, and this list would go on calling it
         * the tightening it is not.
         */
        assert.ok(
          e instanceof sexpr.SexprError,
          `${probe.name} was refused by a ${(e as Error)?.constructor?.name}, not by the reader`
        );
        refused.push(probe.name);
      }
    }
    return refused.sort();
  }

  it('refuses when it reads exactly what the authority refused when it wrote', () => {
    const notWireSafe = fixture.read
      .filter((p) => p.accepted && !p.rewritable)
      .map((p) => p.name)
      .sort();
    assert.ok(notWireSafe.length >= 2, 'the table no longer covers "parses but cannot be written"');
    assert.deepStrictEqual(
      refusedByThisReader(),
      notWireSafe,
      'this reader refuses a different set than the one the authority will not write'
    );
    for (const probe of fixture.read) {
      if (probe.accepted && !probe.rewritable) {
        assert.match(
          probe.write_error ?? '',
          /not wire-safe/,
          `${probe.name} is refused for some reason other than being unwritable`
        );
      }
    }
  });

  it('accepts every input the authority accepts, with the same canonical rewrite', () => {
    let rewritten = 0;
    for (const probe of fixture.read) {
      if (!probe.accepted) {
        continue;
      }
      /*
       * The four named above are asserted by their own cell. Reaching
       * them here would stop this loop at the first one and leave every
       * probe after it unread.
       */
      if (!probe.rewritable) {
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
      }
    }
    assert.ok(rewritten > 0, 'no probe exercised the canonical rewrite');
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
    const file = path.join(__dirname, '..', 'fixtures', 'goeteia', 'sexpr-escape-vectors.json');
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
    const file = path.join(__dirname, '..', 'fixtures', 'goeteia', 'sexpr-escape-vectors.json');
    const table = JSON.parse(fs.readFileSync(file, 'utf8')) as EscapeFixture;
    assert.ok(table.symbols.length > 0, 'the symbol group is empty');

    const encoder = new TextEncoder();
    for (const row of table.symbols) {
      /*
       * THE ONE ROW THIS READER DOES NOT AGREE WITH, AND THE STORE
       * DOES. `\x31;` is the symbol whose name is `1`, escaped so it is
       * not read as the number, and the table records it as accepted --
       * its note calls it the discriminating partner of the rows that
       * ARE refused, on the ground that "a reader that refused both is
       * too narrow".
       *
       * AND THE STORE AT THIS PIN SIDES WITH THE TABLE, WHICH WAS
       * MEASURED RATHER THAN ASSUMED. The core's wire-safety predicate
       * does answer false for `|1|` -- it round-trips the name through
       * igropyr's writer and gets the number 1 back -- but a false
       * answer selects a WRAPPED STORAGE FORM, `("#%sym" "1")`; it does
       * not refuse the write. `link a 1 b` stores the name and `read`
       * prints it as `\x31;`, so a store can hold a block this reader
       * will not read. Two earlier versions of this comment said the
       * opposite, once calling goeteia too narrow and once calling the
       * store the thing that refuses. Both were written without
       * measuring.
       *
       * IT DOES REACH THIS EXTENSION, and that is pinned by a cell that
       * builds the state with the store itself: `link <a> 1 <b>`, then
       * a read that fails and names the block. This paragraph said the
       * opposite until the measurement above was made; it is left
       * rewritten rather than deleted because the claim it made is the
       * one a reader would otherwise make again.
       */
      if (row.want.includes('SYMBOL 1')) {
        assert.throws(
          () => sexpr.read(row.src),
          sexpr.SexprError,
          'the reader now accepts a numeric-form symbol name; the divergence above is stale'
        );
        continue;
      }
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
    const file = path.join(__dirname, '..', 'fixtures', 'goeteia', 'sexpr-escape-vectors.json');
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

  /*
   * THE VERSION IS PART OF THE MEASUREMENT. What this suite proves, it
   * proves about one build of the reader. As a vendored file that was
   * pinned by its own md5; as a dependency the equivalent pin is the
   * installed version, and without it an `npm update` swaps the reader
   * for a different one while every cell here goes on passing -- the
   * tables would still adjudicate, but the answer would be about
   * something nobody chose.
   *
   * IT READS WHAT IS INSTALLED, not what package.json asks for. A range
   * says what was permitted; only node_modules says what ran.
   */
  it('runs against the version of the dependency this tree was checked against', () => {
    const installed = JSON.parse(
      fs.readFileSync(
        path.join(__dirname, '..', '..', '..', 'node_modules', 'goeteia', 'package.json'),
        'utf8'
      )
    ) as { version: string };
    assert.strictEqual(
      installed.version,
      PINNED_GOETEIA,
      'the installed reader is not the one these tables were run against'
    );
  });

  /*
   * AND THE BYTES THAT ACTUALLY LOAD, not the version beside them. A
   * version number in a package.json says what was installed; it says
   * nothing about which file the loader resolved, and pointing the
   * loader at a copy while leaving the metadata alone left every cell in
   * this file passing. The vendored reader was pinned by its own digest;
   * losing that when it became a dependency was a real loss of evidence,
   * and this is it restored: the specifier `goeteia/sexpr` is resolved
   * the same way the product resolves it, and the file that comes back
   * is digested.
   */
  /*
   * AND THE MODULE THAT `initWire` ACTUALLY RETURNED IS THAT FILE.
   *
   * The cell below digests the file the specifier resolves to, which is
   * a claim about the specifier and not about the loader: leaving the
   * exported name alone while pointing the import itself somewhere else
   * left every cell in this file passing. Node returns one module
   * instance per resolved URL, so importing the pinned path here and
   * comparing identity with what the product loaded settles it -- and
   * it settles it for a byte-identical copy too, which a digest cannot.
   */
  it('loaded that module and not another copy of it', async () => {
    const pinned = (await dynamicImport(
      pathToFileURL(require.resolve(READER)).href
    )) as SexprApi;
    assert.strictEqual(
      sexpr.read,
      pinned.read,
      'the reader in use is not the module the pinned specifier resolves to'
    );
  });

  it('loads the reader the specifier resolves to, byte for byte', () => {
    /*
     * THE SPECIFIER COMES FROM THE PRODUCT. Writing it out here again
     * would make this cell check its own copy of the name: a build
     * pointed at another file keeps passing, which is exactly what
     * happened the first time this was written.
     */
    const resolved = require.resolve(READER);
    assert.ok(
      resolved.includes(`${path.sep}node_modules${path.sep}goeteia${path.sep}`),
      `the specifier resolved outside the package: ${resolved}`
    );
    assert.strictEqual(
      createHash('md5').update(fs.readFileSync(resolved)).digest('hex'),
      READER_DIGEST,
      'the reader that loads is not the one these tables were run against'
    );
  });

  it('takes the reader from the package rather than from a copy in this tree', () => {
    /*
     * THE VENDORED COPY IS GONE, and its absence is checked rather than
     * assumed: a leftover would be found first by a relative loader and
     * would make every cell above a measurement of the wrong file.
     */
    assert.ok(
      !fs.existsSync(path.join(__dirname, '..', '..', '..', 'src', 'vendor')),
      'a vendored reader is still in the tree and may be what is being read'
    );
  });
});
