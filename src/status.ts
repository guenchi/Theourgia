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
 * What the status bar says, decided without a status bar.
 *
 * THE TEXT IS COMPUTED HERE SO THAT IT CAN BE READ BACK. A line composed
 * inline where it is displayed is a line no cell can look at, and the
 * facts it carries -- how many saves are unresolved, whether the store
 * has conflicts -- are exactly the ones a user would be misled by.
 */

/*
 * `conflicts` IS NULL WHEN NOBODY HAS BEEN ABLE TO ASK. Not zero: zero
 * is a store the core reported as having none, and a status bar that
 * draws the same thing for "none" and "the question could not be put"
 * is telling the user the better of the two every time it fails.
 */
import { StructuralMark } from './model';

export interface StatusFacts {
  store: string;
  actor: string;
  cursor: string | null;
  conflicts: number | null;
  /*
   * `pending` IS NULL WHEN THE QUEUE COULD NOT BE READ. Not zero: a
   * queue this build cannot read is a queue whose contents are unknown,
   * and it is exactly the state in which no save will go out. Reporting
   * zero there put "nothing waiting to be saved" on the screen of a user
   * whose saves had stopped -- the most reassuring possible sentence at
   * the least appropriate moment.
   */
  pending: number | null;
  blocked: string | null;
}

export interface StatusLine {
  text: string;
  tooltip: string;
  warning: boolean;
}

export function statusLine(facts: StatusFacts): StatusLine {
  const parts: string[] = ['$(book) theourgia'];
  if (facts.conflicts === null) {
    parts.push('$(question)');
  } else if (facts.conflicts > 0) {
    parts.push(`$(error) ${facts.conflicts}`);
  }
  if (facts.pending === null) {
    parts.push('$(warning) queue unreadable');
  } else if (facts.pending > 0) {
    parts.push(`$(cloud-upload) ${facts.pending}`);
  }
  if (facts.blocked !== null) {
    parts.push('$(circle-slash)');
  }
  const tooltip = [
    `store: ${facts.store}`,
    `actor: ${facts.actor}`,
    `cursor: ${facts.cursor ?? 'none yet'}`,
    facts.conflicts === null
      ? 'conflicts: unknown, the store could not be asked'
      : facts.conflicts > 0
        ? `${facts.conflicts} conflict(s) reported by the store`
        : 'no conflicts',
    facts.pending === null
      ? 'the outbox could not be read, so no save will be sent and what it holds is not known'
      : facts.pending > 0
        ? `${facts.pending} save(s) whose outcome is unknown; run "theourgia: Retry Pending Saves"`
        : 'nothing waiting to be saved',
    ...(facts.blocked === null ? [] : [`writing is blocked: ${facts.blocked}`])
  ].join('\n');
  return {
    text: parts.join(' '),
    tooltip,
    warning:
      facts.conflicts === null ||
      facts.conflicts > 0 ||
      facts.pending === null ||
      facts.pending > 0 ||
      facts.blocked !== null
  };
}

export type NoticeLevel = 'error' | 'warning' | 'information' | 'none';

export interface Notice {
  level: NoticeLevel;
  text: string;
}

/*
 * WHAT THE USER IS TOLD AFTER A SAVE, decided here rather than at the
 * call to the editor, so that a cell can read it. The three cases that
 * must not be silent are a refusal, a save whose outcome nobody knows,
 * and a save whose bytes were changed on the way out.
 */
export function saveNotice(
  outcome: { status: string; id: string; message: string },
  normalised: boolean
): Notice {
  if (outcome.status === 'refused' || outcome.status === 'blocked') {
    return { level: 'error', text: outcome.message };
  }
  if (outcome.status === 'pending') {
    return { level: 'warning', text: outcome.message };
  }
  if (normalised) {
    return {
      level: 'information',
      text:
        `${outcome.id} was saved with its line endings normalised to LF; the block itself ` +
        'holds none, and the editor is configured to write CRLF.'
    };
  }
  return { level: 'none', text: '' };
}

/*
 * THE REFUSAL HAS ITS OWN SENTENCE because it is the one case where
 * nothing was sent and the file still holds what the user wrote -- two
 * facts they need in the same breath.
 *
 * AND IT NAMES WHAT WAS ACTUALLY EDITED. A block's protected prefix is
 * its front matter and its heading, and a save that writes only `src`
 * can change neither -- but a sentence that always said "the heading
 * line" sent a user who had edited front matter, on a block that has no
 * heading, to look at something that is not there.
 */
export function prefixRefusedNotice(id: string, hasFront: boolean, hasHeading: boolean): Notice {
  const parts: string[] = [];
  if (hasFront) {
    parts.push('front matter');
  }
  if (hasHeading) {
    parts.push('heading line');
  }
  const what = parts.length === 0 ? 'text before the body' : parts.join(' or ');
  return {
    level: 'warning',
    text:
      `the ${what} of ${id} changed. This batch sends only the body, so nothing was sent; ` +
      'the file still holds what you wrote.'
  };
}

/*
 * A BUFFER OPENED AGAINST ANOTHER STORE IS NOT SAVED ANYWHERE. Sending
 * it to the store the settings now name would write this id into a store
 * where it means something else, or nothing; refusing silently would
 * leave the user believing their work had been stored.
 */
export function wrongStoreNotice(id: string, was: string, now: string): Notice {
  return {
    level: 'error',
    text:
      `${id} was opened from the store at ${was} and the current store is ${now}, ` +
      'so nothing was sent. Close this editor, or point theourgia.store back at the store ' +
      'this block came from.'
  };
}

/*
 * WHAT A MARKED ROW SAYS WHEN YOU HOVER IT, and it says WHICH marks --
 * plural, and together with a field conflict rather than instead of it.
 * A block can be an orphan AND a nested document, and it can be either
 * of those AND have a field with two candidate values; naming only the
 * first fact found tells the reader about one problem and hides the
 * rest, which is the same failure as naming none.
 * Four structural marks and a field conflict all used to draw one
 * warning icon with one sentence, so the tree told a reader "something
 * is wrong here" and never which of five things -- and the remedies are
 * not the same: an orphan has lost its parent, a cycle has a parent
 * chain that closes on itself, a nested document is a shape the write
 * path refuses to make, and a field conflict is two candidate values
 * waiting for someone to choose.
 *
 * IT IS A FUNCTION SO THAT A CELL CAN READ IT. A tooltip composed where
 * it is displayed is a sentence nothing can check, and the thing it
 * would be wrong about is which of the five the store actually said.
 */
export function nodeTooltip(
  id: string,
  marks: StructuralMark[] | null,
  fieldConflict: boolean
): string | null {
  const said: string[] = [];
  if (marks === null) {
    said.push('structural marks unavailable: the conflicts request was refused');
  } else if (marks.length > 0) {
    said.push(`the store reports this block as ${marks.join(' and ')}`);
  }
  if (fieldConflict) {
    said.push('it has a field with more than one candidate value');
  }
  return said.length === 0 ? null : `${id}: ${said.join('; ')}`;
}

/*
 * WHAT A RETRY REPORTS, and it reports a count that may not exist.
 *
 * THE SENTENCE IS COMPOSED HERE FOR THE REASON AT THE TOP OF THIS FILE.
 * It used to be built at the call to the editor out of
 * `saver.pendingCount`, whose type is `number | null` -- so a queue this
 * build cannot read put the word "null" in front of the user, inside a
 * sentence that otherwise reads like a count. Interpolating a value
 * whose absent case is a word is how "unknown" gets drawn as a fact.
 *
 * IT NAMES THE STORE because the count and the outcomes must be about
 * the same one. A retry that is still in flight when the settings change
 * has no business reporting the new store's queue, and a sentence with
 * no store in it cannot be caught doing so.
 */
export function retryNotice(
  store: string,
  resolved: number,
  total: number,
  pending: number | null
): Notice {
  const waiting =
    pending === null
      ? 'how many are still waiting is not known, because that queue could not be read'
      : `${pending} still waiting`;
  return {
    level: pending === null || pending > 0 ? 'warning' : 'information',
    text: `${resolved} of ${total} resolved for ${store}; ${waiting}.`
  };
}
