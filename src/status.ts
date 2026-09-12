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
import { RECONCILE_BLOCK, RETRY_OUTBOX } from './commands';
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
        ? `${facts.pending} save(s) whose outcome is unknown; run "${RETRY_OUTBOX.title}"`
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

/*
 * A BUFFER WHOSE BASELINE THIS HOST DOES NOT HAVE.
 *
 * Saving sends the BODY, which means separating body from front matter
 * and heading -- and that separation is made against the text the store
 * last gave for this block. A file that already held changes when it
 * was opened, written by an older run or by another window, was not
 * written from anything this host saw: there is no text to measure it
 * against.
 *
 * GUESSING ONE IS THE DANGEROUS ANSWER. Using the reading just taken
 * from the store splits the user's text against a prefix it never had,
 * and when that prefix is empty the mismatch is not refused -- the
 * heading becomes body and is written into the block. So the save is
 * refused, and the sentence has to say what to do, because refusing
 * without that leaves the user with a buffer that silently never saves.
 */
export function unreconciledNotice(id: string, file: string): Notice {
  return {
    level: 'error',
    text:
      `${id} was not opened from the store, because ${file} already held changes this ` +
      'window did not write. Nothing can be sent from it: there is no version to measure ' +
      'the edit against. Copy anything you want to keep out of that file, delete it, and ' +
      'open the block again.'
  };
}

/*
 * WHAT RECONCILIATION SAYS WHEN IT IS OVER.
 *
 * The command is offered by a refusal, so its own outcome has to be
 * legible on the same terms: the user is told which text is now the
 * baseline and, when a new version was published, which file holds it.
 * A command that silently succeeds is indistinguishable from one that
 * silently did nothing, and this one is reached only by users who have
 * already been told something went wrong.
 */
export function reconciledNotice(id: string, file: string): Notice {
  return {
    level: 'information',
    text:
      `${id} is reconciled: ${file} already began with the block's heading, so what is there is ` +
      'now the baseline. The body counts as work the store has not seen, and saving sends it.'
  };
}

/*
 * THE CHOICE WAS MADE AND A NEW VERSION CARRIES IT. Neither action
 * rewrites or removes the file the user was looking at -- that file is
 * the only copy of what they typed -- so the sentence has to name the
 * new file, or they will keep editing the old one and wonder why
 * nothing is sent.
 */
export function reconcileChoiceNotice(
  id: string,
  action: 'prepend-prefix' | 'take-store-version',
  file: string
): Notice {
  if (action === 'prepend-prefix') {
    return {
      level: 'information',
      text:
        `${id}: your text was kept and the block's heading put in front of it, in ${file}. The ` +
        'file you were editing is untouched; edit and save the new one.'
    };
  }
  return {
    level: 'information',
    text:
      `${id}: the store's version was published as ${file}. The file you were editing is ` +
      'untouched, so nothing you wrote was lost; edit and save the new one.'
  };
}

/*
 * THE CHOICE COULD NOT BE CARRIED OUT. `reconcileBy` answers `done:
 * false` when the record beside the file is missing or when the editor
 * holds the path the new version would take -- neither of which the
 * user can guess from a command that simply returns.
 */
export function reconcileUnfinishedNotice(file: string): Notice {
  return {
    level: 'warning',
    text:
      `${file} was left exactly as it is: the new version could not be published. Close any other ` +
      'window holding this block and run the command again.'
  };
}

/*
 * THE COMMAND WAS RUN ON SOMETHING THAT IS NOT A BLOCK FILE. Saying so
 * is the whole answer; there is nothing here to repair.
 */
export function notABlockNotice(file: string): Notice {
  return {
    level: 'warning',
    text: `${file} is not a block file opened by this window, so there is nothing to reconcile.`
  };
}

/*
 * AN OPEN THAT WAS OVERTAKEN BY A SAVE.
 *
 * The rule that decides which reading of a block a save is measured
 * against admits a confirmed save ahead of every read that began before
 * it. That is a conservative policy rather than a claim about which is
 * fresher: a read that started earlier may still have sampled the store
 * after the save landed, and this refuses it anyway. Refusing is the
 * safe direction -- nothing is overwritten -- but it leaves the user
 * without the block they asked for, and a click that does nothing at
 * all is the one outcome they cannot act on.
 */
export function supersededNotice(id: string): Notice {
  return {
    level: 'information',
    text:
      `${id} was not opened: a save of it was confirmed while it was being read, so what ` +
      'came back was already out of date. Open it again to get the stored version.'
  };
}

/*
 * WHAT A REFUSED SAVE SAYS, ONE SENTENCE PER REASON.
 *
 * Eight refusals reach here and they need eight different things from
 * the user: one waits for the next keystroke, one wants the file's
 * encoding changed, one wants a heading put back, one wants a command
 * run. A single "could not save" would leave all of them looking like
 * the same dead end -- and the silent version of it looks exactly like
 * a save that worked, which is the failure this whole batch exists to
 * remove. (§12.17.3, §12.13.4, §12.11.7)
 */
export function refusalNotice(
  id: string,
  file: string,
  refusal:
    | { because: 'document-dirty' }
    | { because: 'byte-order-mark' }
    | { because: 'not-utf8' }
    | { because: 'disk-differs-from-snapshot' }
    | { because: 'prefix-changed'; prefix: string }
    | { because: 'no-sidecar' }
    | { because: 'unresolved' }
    | { because: 'publication-incomplete' }
    | { because: 'outside-session'; file: string }
): Notice {
  switch (refusal.because) {
    case 'document-dirty':
      /*
       * NOT AN ERROR AT ALL. The user carried on typing after the save,
       * so the next one will take the newer text. Saying nothing here
       * would be the silent case; saying it loudly would be noise.
       */
      return {
        level: 'none',
        text: `${id} was edited again while it was being saved; the next save will send it.`
      };
    case 'byte-order-mark':
      return {
        level: 'warning',
        text:
          `${id} was not sent: ${file} begins with a byte-order mark, and this batch stores block ` +
          'text as UTF-8 without one. Save the file as UTF-8 and try again.'
      };
    case 'not-utf8':
      return {
        level: 'warning',
        text:
          `${id} was not sent: ${file} is not UTF-8. Save the file as UTF-8 -- the store keeps block ` +
          'text in that encoding and nothing here guesses another.'
      };
    case 'disk-differs-from-snapshot':
      return {
        level: 'warning',
        text:
          `${id} was not sent this time: what is on disk is not yet what the editor holds. Nothing ` +
          'was changed; saving again will send it.'
      };
    case 'prefix-changed':
      return {
        level: 'warning',
        text:
          `${id} was not sent: the front matter or heading changed. This batch sends only the body, ` +
          'so nothing went out and the file still holds what you wrote.'
      };
    case 'publication-incomplete':
      return {
        level: 'warning',
        text:
          `${id} was not sent: its file and the record beside it do not yet describe one another. ` +
          'Open the block again to publish a fresh version.'
      };
    case 'unresolved':
      return {
        level: 'error',
        text:
          `${id} was not sent: ${file} holds a version neither this window nor the store wrote, so ` +
          `there is nothing to measure the edit against. Run "${RECONCILE_BLOCK.title}" to choose ` +
          'what to keep.'
      };
    case 'no-sidecar':
      return {
        level: 'error',
        text:
          `${id} was not sent: there is no record beside ${file} saying what it was based on. Open ` +
          'the block again to publish a version this window knows.'
      };
    case 'outside-session':
      return {
        level: 'none',
        text: `${refusal.file} is not a block file; nothing was sent.`
      };
  }
}

/*
 * WHEN THE STORE ANSWERED AND THE RECORD COULD NOT BE WRITTEN.
 *
 * The request stays in the queue, which is the safe direction -- it can
 * be retried and the store will answer `replay` -- but the user has to
 * know that the file beside it does not yet say so, because until it
 * does the block will keep being reported as holding unsent work.
 */
export function unrecordedNotice(
  file: string,
  because: 'not-acknowledged' | 'req-mismatch' | 'file-moved'
): Notice {
  if (because === 'file-moved') {
    return {
      level: 'warning',
      text:
        `The store accepted the save, but ${file} has changed since it was sent, so the record ` +
        'beside it was left alone. The request is kept and will be retried; save again when you ' +
        'are ready.'
    };
  }
  if (because === 'req-mismatch') {
    return {
      level: 'error',
      text:
        `The store reports a different request under this save's name. ${file} is left as it is ` +
        `and the request is kept; run "${RECONCILE_BLOCK.title}" to see what the store has.`
    };
  }
  return {
    level: 'warning',
    text:
      `The store answered, but the record beside ${file} could not be written, so this window ` +
      'still counts the save as unsent. The request is kept and will be retried.'
  };
}
