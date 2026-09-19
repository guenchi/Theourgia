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
 * COMMENTS ARE ASCII. The code around them need not be.
 *
 * The rule is about what this repository writes, not about what it can
 * carry: several cells exist precisely to push multi-byte text through
 * the save path, and a check that forbade non-ASCII anywhere would have
 * to be switched off by the first person who wrote one. Measured at the
 * time of writing: 1104 non-ASCII characters outside comments, nearly
 * all of them in test data and in the sentences the extension shows.
 *
 * NOTE: WHERE A COMMENT ENDS IS ASKED OF THE COMPILER, not worked out
 * here. A rule of this kind invites a hand-written scanner -- find `//`,
 * find the end of the line -- and such a scanner is wrong about a `//`
 * inside a string, wrong about an apostrophe inside a comment, and wrong
 * in the direction that reports nothing. TypeScript is already a
 * dependency of this repository and it parses these files for a living,
 * so the comment ranges come from `ts.getLeadingCommentRanges` and
 * `ts.getTrailingCommentRanges` and this file contains no scanner at
 * all.
 */

import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as ts from 'typescript';

const root = path.join(__dirname, '..', '..', '..');
const LOOK_IN = ['src', 'test', 'scripts'];
const SKIP = new Set(['node_modules', 'out', '.git', '.vscode-test', 'fixtures', 'vendor']);

function sourceFiles(directory: string, found: string[] = []): string[] {
  for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
    if (SKIP.has(entry.name)) {
      continue;
    }
    const where = path.join(directory, entry.name);
    if (entry.isDirectory()) {
      sourceFiles(where, found);
    } else if (/\.(ts|js|mjs)$/.test(entry.name)) {
      found.push(where);
    }
  }
  return found;
}

export interface Offence {
  file: string;
  line: number;
  characters: string;
  text: string;
}

/*
 * THE RANGES ARE COLLECTED FROM EVERY NODE AND THEN MADE UNIQUE. A
 * comment between two statements is the leading trivia of one and the
 * trailing trivia of another, so the same span arrives twice; counting
 * it twice would report a file as twice as bad as it is and would make
 * the number in the failure useless for telling whether a sweep
 * finished.
 */
export function commentRanges(text: string, file: string): Array<[number, number]> {
  const parsed = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true);
  const seen = new Set<string>();
  const out: Array<[number, number]> = [];
  const take = (ranges: ts.CommentRange[] | undefined): void => {
    for (const range of ranges ?? []) {
      const key = `${range.pos}:${range.end}`;
      if (seen.has(key)) {
        continue;
      }
      seen.add(key);
      out.push([range.pos, range.end]);
    }
  };
  /*
   * NEVER: `forEachChild` IS NOT ENOUGH, and the gap is exactly the
   * shape a rule about comments has to see.
   *
   * It visits a node's semantic children and skips its punctuation, so a
   * comment that sits between two tokens with no node between them is
   * attached to a token nobody asks about. Found by an outside review
   * and reproduced here: a block comment between the braces of an empty
   * function body reported nothing, and so did the same comment inside
   * an empty parameter list, an empty array literal, an empty class body
   * and an empty if-block. A line comment was seen and a string was
   * correctly ignored, so the reader looked as though it worked.
   *
   * NOTE: THE SAMPLES ARE IN THE CELL BELOW AND NOT IN THIS COMMENT, for
   * a reason worth keeping: writing one here closes this comment at its
   * first delimiter and the file stops parsing. That is the same hazard
   * the trailing-comment rule exists for, met from the other side.
   *
   * `getChildren` includes every token, which is what makes the walk
   * complete. It is slower and it is a test.
   */
  const visit = (node: ts.Node): void => {
    take(ts.getLeadingCommentRanges(text, node.pos));
    take(ts.getTrailingCommentRanges(text, node.end));
    for (const child of node.getChildren(parsed)) {
      visit(child);
    }
  };
  visit(parsed);
  return out;
}

export function offencesIn(file: string, text: string): Offence[] {
  const out: Offence[] = [];
  const lineOf = (at: number): number => text.slice(0, at).split('\n').length;
  for (const [from, to] of commentRanges(text, file)) {
    for (const line of text.slice(from, to).split('\n')) {
      const odd = line.match(/[^\x00-\x7F]/g);
      if (odd === null) {
        continue;
      }
      out.push({
        file,
        line: lineOf(from + text.slice(from, to).indexOf(line)),
        characters: [...new Set(odd)].join(' '),
        text: line.trim()
      });
    }
  }
  return out;
}

describe('plugin-r2 comments are written in ASCII', function () {
  this.timeout(60000);

  it('has no non-ASCII character in any comment in src, test or scripts', function () {
    const offences: Offence[] = [];
    for (const where of LOOK_IN) {
      const directory = path.join(root, where);
      if (!fs.existsSync(directory)) {
        continue;
      }
      for (const file of sourceFiles(directory)) {
        offences.push(...offencesIn(file, fs.readFileSync(file, 'utf8')));
      }
    }
    assert.deepStrictEqual(
      offences.map((o) => `${path.relative(root, o.file)}:${o.line}  [${o.characters}]  ${o.text}`),
      [],
      'these comments hold characters outside ASCII. Emoji in particular are not used: write ' +
        'NEVER:, NOTE: and KEY: instead of the three symbols this tree used to mark the same ' +
        'three things, a section reference as "section 7.6.50", and an apostrophe as \'.'
    );
  });

  /*
   * KEY: THE INSTRUMENT IS MADE TO SPEAK BEFORE ITS SILENCE IS BELIEVED.
   *
   * An empty list from the cell above is worth nothing until something
   * has been seen to produce a non-empty one -- a check that reads every
   * file and finds nothing looks exactly like a check whose reader is
   * broken. This hands it a file with one offending comment and one
   * innocent string containing the same character, and requires the
   * first and not the second.
   */
  /*
   * KEY: AND THE PLACES THE FIRST WALK COULD NOT SEE.
   *
   * Each of these was measured returning nothing before the walk was
   * changed. They are one shape -- a comment between two tokens with no
   * node between them -- and they are listed rather than summarised
   * because a summary is what let the gap exist.
   */
  it('sees a comment that has no node beside it', function () {
    const hidden = [
      'function f(){ /* \u00a7 */ }',
      'function f(/* \u00a7 */) {}',
      'const a = [ /* \u00a7 */ ];',
      'class C { /* \u00a7 */ }',
      'if (x) { /* \u00a7 */ }'
    ];
    for (const sample of hidden) {
      assert.strictEqual(
        offencesIn('sample.ts', sample).length,
        1,
        `this comment was invisible to the reader: ${sample}`
      );
    }
  });

  it('finds a character in a comment and passes over the same one in a string', function () {
    const sample = [
      '/* a comment with a section sign § in it */',
      'const s = "a string with § in it";',
      '// and a line comment with ⚠ in it',
      'const t = `a template with ⚠`;'
    ].join('\n');
    const found = offencesIn('sample.ts', sample);
    assert.deepStrictEqual(
      found.map((o) => o.characters),
      ['§', '⚠'],
      'the reader either missed a comment or mistook a string for one'
    );
  });
});
