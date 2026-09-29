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
 * THE CORE'S OWN SMALL REGULAR EXPRESSIONS, transcribed from its regex.sc,
 * so that a question the core answers with one of them is answered here the
 * same way.
 *
 * NOTE: WHY NOT A JAVASCRIPT PATTERN. The core's engine differs from
 * JavaScript's where it matters: its `.` crosses everything but a line feed
 * and a carriage return; it refuses a text longer than 4096 characters; and
 * it counts its work and gives up past 30000 steps, which a caller that
 * guards the call takes as "no match". A pattern that looks the same in
 * JavaScript matches lines the core does not.
 *
 * Only whether a pattern matches is answered, not what it captured. The
 * core enumerates every match before answering, so its step count does not
 * depend on the order it visits them in, and neither does this one's.
 */

type Item = { kind: 'char'; c: number } | { kind: 'range'; a: number; b: number } | { kind: 'set'; set: 'space' | 'digit' | 'word' };

type Node =
  | { k: 'start' }
  | { k: 'end' }
  | { k: 'char'; c: number }
  | { k: 'any' }
  | { k: 'class'; negate: boolean; items: Item[] }
  | { k: 'alt'; alts: Node[] }
  | { k: 'seq'; items: Node[] }
  | { k: 'group'; body: Node }
  | { k: 'repeat'; min: number; max: number | null; body: Node };

/*
 * THE CORE REFUSED THE TEXT OR RAN OUT OF WORK: regex.sc raises, and the
 * callers this module serves take that as no match.
 */
class Refused extends Error {}

const cp = (s: string): number => s.codePointAt(0) as number;

/*
 * A PATTERN, compiled as regex-compile compiles it: groups, classes with
 * ranges, `\s` `\d` `\w` `\n` `\r` `\t`, `^` `$` `.`, and `*` `+` `?` with a
 * lazy `?` after them (which changes the order of matches, not whether
 * there is one).
 */
export function coreRegexCompile(source: string): Node {
  const chars = Array.from(source);
  let i = 0;
  const n = chars.length;
  const peek = (): string | null => (i < n ? chars[i] : null);
  const take = (): string | null => {
    const c = peek();
    i += 1;
    return c;
  };
  const fail = (why: string): never => {
    throw new Error(`regex: ${why}: ${source}`);
  };
  const escaped = (): Item => {
    const c = take();
    switch (c) {
      case 's':
        return { kind: 'set', set: 'space' };
      case 'd':
        return { kind: 'set', set: 'digit' };
      case 'w':
        return { kind: 'set', set: 'word' };
      case 'n':
        return { kind: 'char', c: 10 };
      case 'r':
        return { kind: 'char', c: 13 };
      case 't':
        return { kind: 'char', c: 9 };
      case null:
        return fail('trailing escape');
      default:
        return { kind: 'char', c: cp(c) };
    }
  };
  const klass = (): Node => {
    const negate = peek() === '^';
    if (negate) {
      take();
    }
    const items: Item[] = [];
    for (;;) {
      const c = peek();
      if (c === null) {
        return fail('unclosed class');
      }
      if (c === ']') {
        take();
        return { k: 'class', negate, items };
      }
      const a: Item = c === '\\' ? (take(), escaped()) : { kind: 'char', c: cp(take() as string) };
      if (a.kind === 'char' && peek() === '-' && i + 1 < n && chars[i + 1] !== ']') {
        take();
        const b: Item = peek() === '\\' ? (take(), escaped()) : { kind: 'char', c: cp(take() as string) };
        if (b.kind !== 'char') {
          return fail('invalid range');
        }
        items.push({ kind: 'range', a: a.c, b: b.c });
      } else {
        items.push(a);
      }
    }
  };
  const atom = (): Node => {
    const c = take();
    switch (c) {
      case '(': {
        let capture = true;
        if (peek() === '?') {
          take();
          if (take() !== ':') {
            fail('malformed pattern');
          }
          capture = false;
        }
        const body = alternative();
        if (take() !== ')') {
          fail('malformed pattern');
        }
        return capture ? { k: 'group', body } : body;
      }
      case '[':
        return klass();
      case '\\':
        return { k: 'class', negate: false, items: [escaped()] };
      case '^':
        return { k: 'start' };
      case '$':
        return { k: 'end' };
      case '.':
        return { k: 'any' };
      default:
        return { k: 'char', c: cp(c as string) };
    }
  };
  const repeated = (): Node => {
    const a = atom();
    const q = peek();
    if (q === '*' || q === '+' || q === '?') {
      take();
      if (peek() === '?') {
        take();
      }
      return { k: 'repeat', min: q === '+' ? 1 : 0, max: q === '?' ? 1 : null, body: a };
    }
    return a;
  };
  const sequence = (): Node => {
    const items: Node[] = [];
    while (peek() !== null && peek() !== ')' && peek() !== '|') {
      items.push(repeated());
    }
    return { k: 'seq', items };
  };
  const alternative = (): Node => {
    const alts = [sequence()];
    while (peek() === '|') {
      take();
      alts.push(sequence());
    }
    return { k: 'alt', alts };
  };
  const ast = alternative();
  if (i !== n) {
    fail('unmatched parenthesis');
  }
  return ast;
}

const ATOMS = new Set(['start', 'end', 'char', 'any', 'class']);

function itemMatches(item: Item, c: number): boolean {
  switch (item.kind) {
    case 'char':
      return item.c === c;
    case 'range':
      return item.a <= c && c <= item.b;
    case 'set':
      if (item.set === 'space') {
        return c === 32 || c === 9 || c === 10 || c === 13 || c === 12;
      }
      if (item.set === 'digit') {
        return c >= 48 && c <= 57;
      }
      return (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || (c >= 48 && c <= 57) || c === 95;
  }
}

/*
 * WHETHER THE PATTERN MATCHES THE TEXT FROM ITS START, as regex-match
 * answers, with its limits: false where the core would refuse.
 */
export function coreRegexMatches(ast: Node, text: string): boolean {
  const chars = Array.from(text, cp);
  const n = chars.length;
  if (n > 4096) {
    return false;
  }
  let budget = 30000;
  const tick = (): void => {
    budget -= 1;
    if (budget < 0) {
      throw new Refused();
    }
  };
  const run = (p: Node, pos: number): number[] => {
    tick();
    switch (p.k) {
      case 'start':
        return pos === 0 ? [pos] : [];
      case 'end':
        return pos === n ? [pos] : [];
      case 'char':
        return pos < n && chars[pos] === p.c ? [pos + 1] : [];
      case 'any':
        return pos < n && chars[pos] !== 10 && chars[pos] !== 13 ? [pos + 1] : [];
      case 'class':
        return pos < n && p.items.some((item) => itemMatches(item, chars[pos])) !== p.negate ? [pos + 1] : [];
      case 'alt':
        return p.alts.flatMap((alt) => run(alt, pos));
      case 'seq': {
        let states = [pos];
        for (const item of p.items) {
          states = states.flatMap((s) => run(item, s));
        }
        return states;
      }
      case 'group':
        return run(p.body, pos);
      case 'repeat': {
        /*
         * regex.sc recurses once per repetition. A body of one character
         * makes that a chain, walked here as a loop with the same steps --
         * one for each turn and one for each try of the body -- so a long
         * line does not need a JavaScript stack thousands of frames deep.
         * Any other body is walked as regex.sc walks it.
         */
        if (!ATOMS.has(p.body.k)) {
          const turn = (s: number, count: number): number[] => {
            tick();
            const here = count >= p.min ? [s] : [];
            if (p.max !== null && count >= p.max) {
              return here;
            }
            const later = run(p.body, s)
              .filter((t) => t > s)
              .flatMap((t) => turn(t, count + 1));
            return [...later, ...here];
          };
          return turn(pos, 0);
        }
        const out: number[] = [];
        let s = pos;
        let count = 0;
        for (;;) {
          tick();
          if (count >= p.min) {
            out.push(s);
          }
          if (p.max !== null && count >= p.max) {
            break;
          }
          const next = run(p.body, s);
          if (next.length === 0 || next[0] <= s) {
            break;
          }
          s = next[0];
          count += 1;
        }
        return out;
      }
    }
  };
  try {
    return run(ast, 0).length > 0;
  } catch (e) {
    if (e instanceof Refused) {
      return false;
    }
    throw e;
  }
}
