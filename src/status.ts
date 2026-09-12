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
import { TakeoverLedger } from './sessions';
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
 * THE FILE MOVED WHILE THE USER WAS CHOOSING.
 *
 * The three texts were shown, and by the time a choice came back one of
 * them was no longer what is on disk. Carrying the choice out anyway
 * publishes bytes the user was never shown while telling them their own
 * were kept, so nothing is done and the offer is withdrawn by name. It
 * is not an error: another window is allowed to write, and this is what
 * that looks like from here.
 */
export function reconcileStaleNotice(file: string): Notice {
  return {
    level: 'warning',
    text:
      `Nothing was done: ${file} changed while you were choosing, so what you picked is no longer ` +
      'what is there. Run the command again to see the current texts.'
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
 * THE SENTENCE A FORCED TAKEOVER HAS TO CARRY, AND THE ANSWER THAT
 * AUTHORISES IT.
 *
 * A window that left no readable record cannot be judged dead, so an
 * ordinary takeover refuses -- and would refuse for ever, which strands
 * that queue. The way out is to let the user decide, which means telling
 * them exactly what they are deciding:
 *
 *   - if that window is in fact still running, every request in its
 *     queue goes to the store a SECOND time;
 *   - the store settles the second by request identity and answers
 *     `replay`, so the work is not done twice;
 *   - what it costs is the transmission, not the change.
 *
 * ⚠️ ALL THREE SENTENCES OR NONE. Dropping the second turns a
 * manageable cost into what reads like data loss and nobody will ever
 * press it; dropping the first hides that there is a cost at all. The
 * confirmation word is returned rather than hard-coded at the call site
 * so that a cell can read what the user was actually asked.
 */
export const FORCE_CLAIM_CONFIRMATION = 'Take it over';

export function forceClaimNotice(sessionId: string, pending: number): Notice {
  return {
    level: 'warning',
    text:
      `${sessionId} left no record saying which process it was, so this window cannot tell ` +
      `whether it is still running. It holds ${pending} unsent request(s). Taking it over anyway ` +
      'is safe to attempt: if that window is in fact still running, each of those requests reaches ' +
      'the store twice, and the store recognises the second by its request id and answers "already ' +
      'applied" rather than doing the work again. What it costs is the sending, not the change.'
  };
}

/*
 * AND WHAT THE LISTING SAYS ABOUT A ROW NOBODY CAN JUDGE. The three
 * reasons are not the same news and the sentence has to say which: two
 * of them are permanent and one of them fixes itself.
 */
export function undecidableSessionNotice(
  sessionId: string,
  because:
    | 'start-time-unavailable'
    | 'start-time-unrecorded'
    | 'liveness-unobtainable'
    | 'record-missing'
    | 'record-unreadable'
): Notice {
  if (because === 'start-time-unrecorded') {
    /*
     * ⚠️ THIS ONE DOES NOT FIX ITSELF EITHER, and it used to be told to
     * wait. The record carries no start time -- written by an older
     * build, or on a platform that could not supply one -- so no later
     * attempt produces it. Nothing is offered: the window may well be
     * running, and its pid alone cannot tell us.
     */
    return {
      level: 'warning',
      text:
        `${sessionId}: its record does not say when that process started, so whether it is still ` +
        'running cannot be judged and waiting will not change that. Nothing is taken over ' +
        'automatically; if you know that window is gone, close this one and reopen it to start ' +
        'a fresh session.'
    };
  }
  if (because === 'liveness-unobtainable') {
    return {
      level: 'information',
      text:
        `${sessionId}: asking whether that process is running failed for a reason this window ` +
        'does not recognise, so nothing is offered for it; try again in a moment.'
    };
  }
  if (because === 'start-time-unavailable') {
    return {
      level: 'information',
      text:
        `${sessionId}: this machine could not be asked when that process started, so whether it ` +
        'is still running is not known yet. Nothing is offered for it; try again in a moment.'
    };
  }
  if (because === 'record-unreadable') {
    return {
      level: 'warning',
      text:
        `${sessionId}: the record beside it will not read, so whether it is still running cannot ` +
        'be judged. Nothing is taken over automatically; a build that understands that record may ' +
        'still be able to.'
    };
  }
  return {
    level: 'warning',
    text:
      `${sessionId}: it left no record saying which process it was, so whether it is still running ` +
      'cannot be judged and no amount of waiting will change that. Its queue can be taken over ' +
      'explicitly, and you will be told what that costs before anything is sent.'
  };
}

/*
 * WHAT THE RECOVERY COMMAND SAYS. Every one of these is here rather than
 * where it is displayed, for the reason the rest of this file exists: a
 * line composed at the call to the editor is a line no cell can read,
 * and these are the lines a user acts on.
 */
export function noOtherSessionsNotice(): Notice {
  return {
    level: 'information',
    text:
      'No other window has left anything here. Unsent work from a window that stopped would be ' +
      'listed by this command; there is none.'
  };
}

/*
 * A TAKEOVER OR A DISCARD THAT WAS TURNED AWAY, BY NAME. Every one of
 * these reasons is a decision somebody can act on, and "it did not work"
 * is not.
 */
export function refusedTakeoverNotice(
  sessionId: string,
  action: 'take-over' | 'force-take-over' | 'discard',
  because: string
): Notice {
  const what = action === 'discard' ? 'discarded' : 'taken over';
  if (because === 'session-alive') {
    return {
      level: 'warning',
      text: `${sessionId} was not ${what}: that window is still running.`
    };
  }
  if (because === 'already-claimed') {
    /*
     * ⚠️ IT DOES NOT SAY THE WORK IS BEING SENT. Whoever holds the claim
     * may have imported nothing -- this window reached that state itself
     * once -- so "it is being sent from there" would be a promise this
     * code has no way to keep.
     */
    return {
      level: 'warning',
      text:
        `${sessionId} was not ${what}: the takeover is held by a window this one cannot take it ` +
        'from -- either still running, or one this window cannot judge. Nothing here can move its ' +
        'work while that is true.'
    };
  }
  if (because === 'not-found') {
    return {
      level: 'information',
      text: `${sessionId} was not ${what}: there is nothing of it on this disk any more.`
    };
  }
  return {
    level: 'warning',
    text:
      `${sessionId} was not ${what}: this window cannot tell whether it is still running, and ` +
      'nothing is taken from a window that might be.'
  };
}

/*
 * WHAT A TAKEOVER MOVED.
 *
 * ⚠️ `skipped` COUNTS TWO DIFFERENT THINGS and the sentence must not
 * claim either: an entry already in this window's queue, and one the
 * source has marked as handed to some other claimant. An earlier comment
 * here said it meant the first, which a review pointed out is not
 * established.
 *
 * ⚠️ AND MOVING THEM IS NOT SENDING THEM. Nothing here starts a drain:
 * the entries wait for the next save or an explicit retry, and a
 * sentence promising they are on their way would be describing work that
 * has not been scheduled.
 */
export function adoptedNotice(
  sessionId: string,
  ledger: TakeoverLedger,
  nowhereToPutThem: boolean
): Notice {
  if (nowhereToPutThem) {
    /*
     * ⚠️ NOTHING WAS TAKEN OVER. This used to say it was: the command
     * claimed first and found out afterwards. It refuses before taking a
     * token now, so the sentence has to say that too.
     */
    return {
      level: 'warning',
      text:
        `${sessionId} was not taken over: there is no queue in this window to move its requests ` +
        'into, because no store is configured here. Set theourgia.store and run the command again.'
    };
  }
  /*
   * ⚠️ ONE SENTENCE PER NON-EMPTY BUCKET, AND NO BUCKET WITHOUT ONE.
   *
   * The ledger's rule is that everything the takeover saw is in exactly
   * one bucket; this is the other half of it. A bucket the report did
   * not mention would be work that vanished between the count and the
   * user -- the same defect as a count that swallowed it, one step
   * later. The cell that adds the buckets guards the first half; this
   * list guards the second, and a bucket added without a sentence here
   * shows up as an unexplained difference in what the user is told.
   */
  const parts: string[] = [
    `${ledger.imported} unsent request(s) are now in this window's queue.`
  ];
  if (ledger.skippedDuplicate > 0) {
    parts.push(`${ledger.skippedDuplicate} had already been carried across and were left alone.`);
  }
  if (ledger.leftOtherStore > 0) {
    parts.push(
      `${ledger.leftOtherStore} belong to other stores and are still there; configure that store ` +
        'and run this command again to bring them across.'
    );
  }
  if (ledger.leftUnknownStore > 0) {
    parts.push(
      `${ledger.leftUnknownStore} are in a queue from an older version of this extension, which ` +
        'does not record which store they were written for; this command will not move them into ' +
        'a store it cannot show they belong to.'
    );
  }
  if (ledger.unreadableQueue > 0) {
    parts.push(
      `${ledger.unreadableQueue} of its queue file(s) could not be read, so what is in them is ` +
        'not known and nothing was taken from them.'
    );
  }
  if (ledger.failedToMove > 0) {
    parts.push(
      `${ledger.failedToMove} could not be moved into this window and are still in that ` +
        'window’s queue; nothing was lost, and running this command again will try them.'
    );
  }
  if (ledger.movedButUnmarked > 0) {
    /*
     * ⚠️ THESE ARRIVED. The sentence for the bucket beside this one says
     * the opposite, and the two were one bucket until a review pointed
     * out that it described work which had in fact moved as work the
     * user should go looking for elsewhere.
     */
    parts.push(
      `${ledger.movedButUnmarked} arrived here, but the other window’s copy could not be marked ` +
        'as handed over; they will be offered again by a later takeover and recognised as ' +
        'already here, so nothing is sent twice.'
    );
  }
  parts.push(`They go out with the next save, or run "${RETRY_OUTBOX.title}" to send them now.`);
  return { level: 'information', text: `${sessionId}: ${parts.join(' ')}` };
}

/*
 * AND WHERE THE DISCARDED FILES WENT. The path is in the sentence
 * because "moved aside" without a destination is indistinguishable from
 * "deleted" to the person reading it.
 */
export function discardedNotice(sessionId: string, trash: string, liveAdopters: string[]): Notice {
  const open =
    liveAdopters.length === 0
      ? ''
      : ` ${liveAdopters.length} other window(s) still have documents open in it.`;
  return {
    level: 'information',
    text: `${sessionId} was moved to ${trash}. Nothing was deleted.${open}`
  };
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
  because: 'not-acknowledged' | 'req-mismatch' | 'file-moved' | 'split-changed'
): Notice {
  if (because === 'split-changed') {
    /*
     * ⚠️ NOT "THE HEADING CHANGED". The record can stop producing what
     * was sent for more than one reason -- the heading it splits at, or
     * what it says the block's own line endings are -- and naming only
     * the first would be telling the user to look at something that did
     * not move. Found in review.
     */
    return {
      level: 'warning',
      text:
        `The store accepted the save, but the record beside ${file} no longer describes it the ` +
        'same way, so what was sent is not what that record would send now and it was left alone. ' +
        'The request is kept and will be retried; nothing was lost.'
    };
  }
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
