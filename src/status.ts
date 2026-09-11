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
export interface StatusFacts {
  store: string;
  actor: string;
  cursor: string | null;
  conflicts: number | null;
  pending: number;
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
  if (facts.pending > 0) {
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
    facts.pending > 0
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
 * THE HEADING REFUSAL HAS ITS OWN SENTENCE because it is the one case
 * where nothing was sent and the file still holds what the user wrote --
 * two facts they need in the same breath.
 */
export function headingRefusedNotice(id: string): Notice {
  return {
    level: 'warning',
    text:
      `the heading line of ${id} changed. Editing a title is not in this batch, so nothing ` +
      'was sent; the file still holds what you wrote.'
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
