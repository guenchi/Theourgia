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
 * WHAT THE USER CAN DO ABOUT ANOTHER WINDOW'S UNSENT WORK.
 *
 * `Sessions` has been able to list, take over and discard since this
 * batch began, and nothing showed any of it to anybody: the takeover a
 * refusal told the user about was reachable from code and from nowhere
 * else. This is the smallest surface that makes those answers reachable
 * -- one command, a list, an action, a confirmation -- and no more. No
 * tree view: every way out §12 names is a command.
 *
 * ⚠️ THE EDITOR IS BEHIND ONE NARROW INTERFACE AND NOT IMPORTED HERE.
 * Everything below is a decision about what to offer, what to say and
 * what to do, and a decision made inside a `vscode.window` call is a
 * decision no cell can read. `Chooser` is what the extension supplies
 * and what a cell supplies; the flow cannot tell them apart, so what a
 * cell exercises is what runs.
 *
 * ⚠️ THE SENTENCES THAT SAY WHAT AN ACTION COSTS ARE NOT WRITTEN HERE.
 * The forced takeover's warning and the two fixed discard notes come
 * from `status.ts` and `sessions.ts` unchanged, because a copy of the
 * one thing that has to be exactly right is a second place for it to be
 * wrong.
 *
 * ⚠️ THE LABELS AND THE COUNTS ARE WRITTEN HERE, and an earlier version
 * of this paragraph said otherwise -- it claimed every line the user
 * reads comes from elsewhere, which is false of the row descriptions,
 * the two decidable-liveness phrases, the action labels and details, the
 * list's placeholder and the discard button's word. A review said so.
 * They are here because they are about this list and nothing else reads
 * them; what is elsewhere is what states a cost.
 */
import {
  DISCARD_BACKUP_NOTE,
  DISCARD_REACH_NOTE,
  ImportTarget,
  OtherSession,
  Sessions
} from './sessions';
import {
  FORCE_CLAIM_CONFIRMATION,
  Notice,
  adoptedNotice,
  discardedNotice,
  forceClaimNotice,
  noOtherSessionsNotice,
  refusedTakeoverNotice,
  undecidableSessionNotice
} from './status';

export interface Choice<T> {
  label: string;
  description?: string;
  detail?: string;
  value: T;
}

/*
 * The editor, reduced to the three things this flow needs. `pick` and
 * `confirm` answer `undefined`/`false` when the user walks away, which
 * is not a failure and is the commonest outcome.
 */
export interface Chooser {
  pick<T>(items: Array<Choice<T>>, placeHolder: string): Promise<T | undefined>;
  confirm(text: string, confirmation: string): Promise<boolean>;
  say(notice: Notice): void;
}

export type RecoveryAction = 'take-over' | 'force-take-over' | 'discard';

/*
 * WHAT THE RUN DID, returned rather than only shown. A flow that spoke
 * only to the editor is a flow whose decisions no cell can read, and the
 * decisions are the whole of this file.
 */
export type RecoveryOutcome =
  | { did: 'nothing'; because: 'no-other-sessions' | 'cancelled' }
  | { did: 'take-over'; sessionId: string; imported: number; skipped: number }
  | { did: 'refused'; sessionId: string; action: RecoveryAction; because: string }
  | { did: 'discard'; sessionId: string; trash: string };

function counts(row: OtherSession): string {
  const drafts = row.drafts.length === 1 ? '1 draft' : `${row.drafts.length} drafts`;
  const pending =
    row.pendingEntries === 1 ? '1 unsent request' : `${row.pendingEntries} unsent requests`;
  const adopters =
    row.liveAdopters.length === 0
      ? ''
      : `, open in ${row.liveAdopters.length} other window(s)`;
  return `${drafts}, ${pending}${adopters}`;
}

/*
 * THE SENTENCE FOR A ROW. A window that cannot be judged gets the
 * sentence `status.ts` writes for that particular reason, because the
 * five reasons are not the same news and two of them will never resolve.
 * The two decidable cases are phrased here: they are a word apiece and
 * nothing else says them.
 */
function sentenceFor(row: OtherSession): string {
  if ('alive' in row.liveness) {
    return row.liveness.alive
      ? 'still running, so nothing can be taken from it'
      : 'not running; its unsent work can be taken over';
  }
  return undecidableSessionNotice(row.sessionId, row.liveness.because).text;
}

function actionsFor(row: OtherSession): Array<Choice<RecoveryAction>> {
  const out: Array<Choice<RecoveryAction>> = [];
  const judgedDead = 'alive' in row.liveness && !row.liveness.alive;
  if (judgedDead) {
    out.push({
      label: 'Take over its unsent requests',
      detail: 'Its queue is copied into this window and sent from here.',
      value: 'take-over'
    });
  }
  /*
   * THE EXPENSIVE ONE IS OFFERED ONLY WHERE IT IS THE ONLY WAY OUT --
   * a window that left no record, which no amount of waiting will make
   * judgeable. Offering it beside the ordinary takeover would teach the
   * user to reach for it.
   */
  if (row.forceable) {
    out.push({
      label: 'Take it over anyway',
      detail: 'This window cannot tell whether that one is still running.',
      value: 'force-take-over'
    });
  }
  out.push({
    label: 'Discard its recovery files',
    detail: 'Moves them aside. Nothing is deleted.',
    value: 'discard'
  });
  return out;
}

/*
 * THE WHOLE FLOW. It is one function because the decisions are one
 * decision: which windows there are, what may be done to this one, what
 * the user is told it costs, and what is then done.
 */
/*
 * WHERE A TAKEOVER'S ENTRIES GO.
 *
 * ⚠️ IT IS A FUNCTION AND NOT AN OBJECT, and it carries the name of its
 * store. Both were defects a review found. An object closing over a
 * queue is written to outside whatever serialises that queue, so a save
 * in flight erases the import while the source is already marked as
 * having handed it over -- the bytes survive and nothing ever offers
 * them again. And a destination with no store named took every one of
 * the dead window's queues, so requests written for one store were
 * dropped into another and would have been sent there.
 *
 * ⚠️ AND THIS PARAGRAPH NAMES THE FUNCTION RATHER THAN ASSERTING THE
 * STATE. The comment it replaces said the destination "is held by the
 * Saver's serial chain", which was a claim about how things stood and
 * was false -- nothing made it true and nothing would have noticed. What
 * can be checked by reading is which function opens the section, so that
 * is what is written: in production `run` is `Saver.adopt`, which goes
 * through `Saver.serialise` -- the same one `save` and `retry` use, and
 * which reloads the queue from disk after taking it. A reader who wants
 * to know whether the import is serialised follows that name; they
 * cannot follow an assertion.
 */
export interface Destination {
  storeHash: string;
  run<T>(work: (into: ImportTarget) => T): Promise<T>;
}

export async function chooseAndRecover(
  sessions: Sessions,
  chooser: Chooser,
  into: Destination | null
): Promise<RecoveryOutcome> {
  const rows = await sessions.others();
  if (rows.length === 0) {
    const notice = noOtherSessionsNotice();
    chooser.say(notice);
    return { did: 'nothing', because: 'no-other-sessions' };
  }
  const chosen = await chooser.pick(
    rows.map((row) => ({
      label: row.sessionId,
      description: counts(row),
      detail: sentenceFor(row),
      value: row
    })),
    'Another window’s unsent work'
  );
  if (chosen === undefined) {
    return { did: 'nothing', because: 'cancelled' };
  }
  const action = await chooser.pick(actionsFor(chosen), `${chosen.sessionId}`);
  if (action === undefined) {
    return { did: 'nothing', because: 'cancelled' };
  }
  return act(sessions, chooser, chosen, action, into);
}

async function act(
  sessions: Sessions,
  chooser: Chooser,
  row: OtherSession,
  action: RecoveryAction,
  into: Destination | null
): Promise<RecoveryOutcome> {
  if (action === 'discard') {
    /*
     * THE TWO FIXED SENTENCES TRAVEL WITH THE CONFIRMATION, not only
     * with the answer: they are what the user is deciding on, and a
     * listing that reflects the disk only is not the whole story.
     */
    const agreed = await chooser.confirm(
      `${row.sessionId}: ${DISCARD_BACKUP_NOTE} ${DISCARD_REACH_NOTE}`,
      'Discard'
    );
    if (!agreed) {
      return { did: 'nothing', because: 'cancelled' };
    }
    const outcome = await sessions.discard(row.sessionId);
    if (!outcome.discarded) {
      const notice = refusedTakeoverNotice(row.sessionId, 'discard', outcome.because);
      chooser.say(notice);
      return { did: 'refused', sessionId: row.sessionId, action, because: outcome.because };
    }
    chooser.say(discardedNotice(row.sessionId, outcome.trash, outcome.liveAdopters));
    return { did: 'discard', sessionId: row.sessionId, trash: outcome.trash };
  }

  /*
   * ⚠️ THE DESTINATION IS CHECKED BEFORE THE TOKEN IS TAKEN.
   *
   * Claiming first and then finding nowhere to put the entries left this
   * window holding a live claim over work it had not moved -- and the
   * advice it then gave, "configure a store and run the command again",
   * ran straight into that claim and was refused as `already-claimed`.
   * The way out was blocked by the attempt to use it. Found in review.
   */
  if (into === null) {
    const notice = adoptedNotice(row.sessionId, 0, 0, true);
    chooser.say(notice);
    return { did: 'refused', sessionId: row.sessionId, action, because: 'nowhere-to-put-them' };
  }
  const forced = action === 'force-take-over';
  if (forced) {
    /*
     * ⚠️ THE ONE CONFIRMATION THAT HAS TO CARRY A COST. The sentence is
     * `forceClaimNotice`'s, unchanged and unwrapped: it says the other
     * window may be running, that each request then reaches the store
     * twice, and that the store settles the second by request id rather
     * than doing the work again. Three statements, and a cell pins all
     * three.
     */
    const agreed = await chooser.confirm(
      forceClaimNotice(row.sessionId, row.pendingEntries).text,
      FORCE_CLAIM_CONFIRMATION
    );
    if (!agreed) {
      return { did: 'nothing', because: 'cancelled' };
    }
  }
  const won = await sessions.claim(row.sessionId, forced);
  if (!won.claimed) {
    const notice = refusedTakeoverNotice(row.sessionId, action, won.because);
    chooser.say(notice);
    return { did: 'refused', sessionId: row.sessionId, action, because: won.because };
  }
  /*
   * WINNING THE TOKEN IS NOT THE TAKEOVER. The entries have to be
   * copied, and they are copied inside `Saver.adopt` -- the section that
   * also holds saves of this window's own queue -- so the import cannot
   * race one in flight.
   *
   * ⚠️ AND ONE TAKEOVER NEED NOT FINISH THE JOB. The token names a
   * SESSION and that session may have a queue per store; this moves the
   * one belonging to the destination's store and says how many it left.
   * Running the command again with another store configured takes those,
   * because a claim the holder re-enters is not `already-claimed`.
   */
  const moved = await into.run((target) =>
    sessions.importFrom(
      { deadSessionId: row.sessionId, sequence: won.sequence, file: won.token },
      target,
      into.storeHash
    )
  );
  chooser.say(
    adoptedNotice(row.sessionId, moved.imported, moved.skipped, false, moved.leftBehind)
  );
  return {
    did: 'take-over',
    sessionId: row.sessionId,
    imported: moved.imported,
    skipped: moved.skipped
  };
}
