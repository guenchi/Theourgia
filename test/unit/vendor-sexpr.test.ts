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

  it('carries the whole fixture, not a trimmed one', () => {
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

  it('is the copy this repository recorded, not whatever is on the machine', () => {
    const file = path.join(__dirname, '..', '..', 'src', 'vendor', 'goeteia', 'sexpr.mjs');
    const text = fs.readFileSync(file, 'utf8');
    assert.ok(
      /VENDORED VERBATIM, NOT FORKED\. Source: https:\/\/github\.com\/guenchi\/Goeteia/.test(text),
      'the vendored reader has lost the note saying where it came from'
    );
    assert.ok(
      /rt\/sexpr\.mjs at commit [0-9a-f]{7,40}, md5 [0-9a-f]{32}/.test(text),
      'the vendored reader does not name the commit and digest it was taken from'
    );
  });
});
