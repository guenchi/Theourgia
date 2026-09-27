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
import { Datum, answerOf, asInteger, cdrOf, isDotted, isList, readEvent } from './wire';
import { TakeoverLedger } from './sessions';
import { StoreVerdict, StructuralMark } from './model';
import { Unrecorded } from './saving';

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
  /*
   * WHY THE STORE COULD NOT BE ASKED, IN THE CORE'S OWN WORDS.
   *
   * KEY: `conflicts: null` ALREADY SAYS THAT SOMETHING WENT WRONG AND
   * CANNOT SAY WHAT. Measured: with a directory sitting where the
   * daemon's socket goes, the core answers
   * `(error serve-path-occupied (path "<run root>/<key>/socket"))` and
   * the thin client relays exactly that -- a sentence naming a directory
   * the user can remove. The extension threw it away and drew a question
   * mark, and a question mark is not something anybody can act on.
   *
   * NULL MEANS THE LAST ASKING WORKED. It is cleared by a success rather
   * than left standing, or the status bar would go on reporting a fault
   * that has been fixed -- which is the same defect the other way round.
   */
  unreachable: string | null;
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
        ? say(facts.conflicts, '1 conflict reported by the store', `${facts.conflicts} conflicts reported by the store`)
        : 'no conflicts',
    facts.pending === null
      ? 'the outbox could not be read, so no save will be sent and what it holds is not known'
      : facts.pending > 0
        ? say(
            facts.pending,
            `1 save whose outcome is unknown; run "${RETRY_OUTBOX.title}"`,
            `${facts.pending} saves whose outcome is unknown; run "${RETRY_OUTBOX.title}"`
          )
        : 'nothing waiting to be saved',
    ...(facts.blocked === null ? [] : [`writing is blocked: ${facts.blocked}`]),
    /*
     * THE WORDS GO LAST AND THEY ARE THE CORE'S. Summarising them here
     * would be a second opinion about a sentence that already names the
     * one thing to do about it.
     */
    ...(facts.unreachable === null ? [] : [`the store could not be reached: ${facts.unreachable}`])
  ].join('\n');
  return {
    text: parts.join(' '),
    tooltip,
    warning:
      facts.conflicts === null ||
      facts.conflicts > 0 ||
      facts.pending === null ||
      facts.pending > 0 ||
      facts.blocked !== null ||
      facts.unreachable !== null
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
/*
 * WHO LANDED WHILE THIS SAVE WAS BEING PREPARED. (design 7.5.22)
 *
 * A commit that succeeded can carry `(behind ((<writer> . <seq>) ...))`:
 * the log writers whose records reached the store after this save's
 * draft took its baseline. It is absent when the baseline is not behind,
 * and a commit carrying it SUCCEEDED. It exists to save the reader a
 * `drafts` round trip, not to ask anything of them.
 *
 * NOTE: IT NAMES OTHER INSTANCES OF THE STORE, NOT OTHER PEOPLE. Every
 * agent writing into one store on one machine appends through the same
 * log writer, so a colleague's commit is NOT what shows up here. What
 * shows up is a copy of the store that was adopted elsewhere and had a
 * segment of its log published back. The sentence below says "another
 * instance" for that reason: "somebody else has been writing" would be a
 * reading of this field that is wrong in the ordinary case.
 *
 * NOTE: AND THE WRITER THIS COMMIT ITSELF ADVANCED IS DROPPED. Measured
 * on a core pinned before F100b (877f0da or earlier): a commit whose own cursor is `("w" . 7)` answers
 * `(behind (("w" . 7)))` -- its own record, reported back as though it
 * were somebody's. The core's rule excludes the DRAFT writer's name and
 * the keys here are LOG writer ids, which are equal only when the two
 * happen to coincide; a window with its own `theourgia.writer` parts
 * them and the store starts naming itself. That is a defect in the core
 * and is being repaired there. This drops it whichever way the core
 * behaves, and the name to drop is read off this very answer's own
 * cursor rather than remembered anywhere.
 *
 * NOTE: SO THIS NEVER FAILS A SAVE. The core answers a successful commit
 * with no clause at all rather than an error when it cannot build one;
 * a reader that threw on an unexpected shape would turn that success
 * into a failure at the one moment somebody is watching. Every shape it
 * cannot read is answered `null`, which reads as "nothing to say".
 */
export function behindNotice(answer: Datum, ours: Datum = null): string | null {
  /*
   * NEVER: AND THE FORM HAS TO HAVE SAID `ok`. Measured in a twelfth
   * review round: `(garbage (behind (("other" . 2))))` produced the
   * whole sentence. This reader was written in the same batch that
   * repaired four others for the same shape and was not one of them,
   * because each repair was made where its finding pointed.
   */
  const form = answerOf(answer, 'ok');
  if (form === null) {
    return null;
  }
  /*
   * NOTE: BOTH REASONS REFUSE. An answer with no `behind` clause is one
   * that says nothing about what landed, and an answer carrying two is
   * one this build cannot read; neither produces a notice, and null is
   * what this reader says for both.
   */
  const rest = form.clause('behind');
  if (!rest.read || rest.items.length !== 1 || !isList(rest.items[0])) {
    return null;
  }
  /*
   * NULL WHEN THIS ANSWER NAMES NO CURSOR, and then nothing is dropped.
   * Removing a name on a guess would hide the one line this notice
   * exists to show.
   */
  const mine = writerOfCursor(ours);
  const landed: Array<{ writer: string; seq: number }> = [];
  for (const entry of rest.items[0]) {
    if (!isDotted(entry)) {
      return null;
    }
    /*
     * NOTE: ONE NAME BEFORE THE DOT, NOT THE FIRST OF SEVERAL. `(behind
     * (("w" . 3)))` is a pair; a clause carrying two items before the
     * dot is a shape this build does not know, and reading the first of
     * them would be a guess dressed as an answer.
     */
    if (entry.items.length !== 1) {
      return null;
    }
    const writer = entry.items[0];
    const seq = asInteger(cdrOf(entry));
    if (typeof writer !== 'string' || seq === null) {
      return null;
    }
    if (writer !== mine) {
      landed.push({ writer, seq });
    }
  }
  if (landed.length === 0) {
    return null;
  }
  /*
   * NOTE: ONE RECORD IS ITS OWN SENTENCE. `${n} records` is right for every
   * n except the one a user is most likely to meet first.
   */
  const each = landed.map((one) =>
    one.seq === 1 ? `${one.writer} has 1 record` : `${one.writer} has ${one.seq} records`
  );
  /*
   * NOTE: THE PREAMBLE CARRIES NO COUNT WORD OF ITS OWN. The per-writer
   * phrase already says "1 record" or "n records", and a fixed word
   * beside it would be a second place for the number's language to be
   * wrong -- and would make the singular read "1 record ... records".
   */
  return (
    `since this save's baseline, ${each.join(' and ')} in this store ` +
    '(another instance of it, not another agent on this machine)'
  );
}

/*
 * THE LOG WRITER AN ANSWER'S OWN CURSOR NAMES.
 *
 * `(cursor ("w" . 7))` is a pair whose head is the writer. Every shape
 * that is not that answers null, and a null drops nothing -- which is
 * the safe direction: a name wrongly dropped is a line the reader never
 * sees, and a name wrongly kept is a line that is merely redundant.
 */
/*
 * NEVER: THE SAME READER THE REST OF THIS BUILD USES FOR A CURSOR.
 *
 * This had its own, narrower one: it required a dotted pair, while
 * `readEvent` also accepts `("w" 7)` as a two-element list (wire.ts).
 * So an answer whose cursor took that shape had its writer go unread
 * here -- and an unread writer is one that is not dropped, which means
 * the notice named the writer the commit had just advanced. Measured in
 * a twelfth review round with `(ok (items (ok (cursor ("w" 7)) ...))
 * (behind (("w" . 7))))`: the save reported `w has 7 records`, about
 * itself.
 *
 * Two readers for one shape is two places for the shape to be wrong, and
 * the one that was wrong was the one nobody else used.
 */
function writerOfCursor(answer: Datum): string | null {
  const form = answerOf(answer, 'ok');
  if (form === null) {
    return null;
  }
  const rest = form.clause('cursor');
  if (!rest.read || rest.items.length !== 1) {
    return null;
  }
  const event = readEvent(rest.items[0]);
  return event === null ? null : event.writer;
}

/*
 * plugin-r3 item 14: THE STORE SAYS IT IS NOT SOUND, AND THE USER IS TOLD
 * ONCE.
 *
 * Asked once when a session starts on a store -- by `check`, which
 * exists for exactly this and carries more than a single word: the
 * verdict, and `integrity` and `torn` per writer. A clause that the core
 * plans to attach to every `--wire` answer (its F59) was considered and
 * set aside: the decision here is made once per session, so a signal that
 * arrives on every answer is the wrong cadence -- and this client asks
 * for `--wire` only when committing, so a session that only reads would
 * never have seen it.
 *
 * NOTE: WHAT THE SENTENCE DOES NOT SAY. It does not promise that saving
 * still works: on a store whose queue has no cursor yet, the bootstrap
 * asks `check` too and reads a non-zero exit as a failure, so a save can
 * be held back there. Reading and writing a damaged store is legal and
 * useful, and nothing here blocks either -- but a sentence that said so
 * would be a claim this function cannot check.
 *
 * Null for a sound store and for a verdict that could not be read: the
 * second is not drawn as "sound", it is left to the status bar, where
 * the conflict count asked at the same moment reports an unreachable
 * store in the core's own words.
 */
export function integrityNotice(store: string, verdict: StoreVerdict): Notice | null {
  if (!verdict.known || verdict.verdict === 'ok') {
    return null;
  }
  return {
    level: 'warning',
    text:
      `the store at ${store} reports its condition as "${verdict.verdict}". ` +
      "Run the core's `check` in that store to see what it found."
  };
}

export function saveNotice(
  outcome: { status: string; id: string; message: string },
  normalised: boolean
): Notice {
  if (outcome.status === 'refused' || outcome.status === 'blocked') {
    /*
     * NOTE: THE BLOCK IS NAMED, AND SO IS WHAT BECOMES OF THE EDIT.
     *
     * This was the core's sentence alone -- "the core refused the write:
     * cursor-unreachable" -- with no way to tell WHICH block it was
     * about when several are open, and no word about the text the user
     * had just typed. And for a while a refusal was not even reported
     * as one: the settler recorded it as an acknowledgement, so the
     * block stopped being counted as unsent and this sentence was the
     * only trace left of the edit. It is now the sentence a user acts
     * on, so it says the three things: which block, why the store said
     * no, and that their text is still there and still unsent.
     */
    return {
      level: 'error',
      text:
        `${outcome.id}: ${outcome.message}. Your text is still in the file and has not been ` +
        'saved to the store.'
    };
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
        'current file has been replaced. Save it to update your working note.'
    };
  }
  return {
    level: 'information',
    text:
      `${id}: the current file ${file} was replaced with the store's content you selected.`
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
 * THE RECORD MOVED WHILE THE PICK WAS OPEN. (queue item 43, design v3 U7)
 * The early refusal: the second wait found the record behind the file at
 * another revision than the one the offer was built from, and did nothing.
 * It names no cause -- a save is the usual one, not the only one -- and it
 * does not say "what you picked is no longer there": the bytes may be the
 * same ones (item 47).
 */
export function reconcileMovedNotice(file: string): Notice {
  return {
    level: 'warning',
    text: `Nothing was done: the record behind ${file} changed while you were choosing; pick again.`
  };
}

/*
 * THE CHOICE COULD NOT BE CARRIED OUT. `reconcileBy` answers `done:
 * false` when the record beside the file is missing or when the editor
 * holds the path the new version would take -- neither of which the
 * user can guess from a command that simply returns.
 */
export function reconcileUnfinishedNotice(file: string, because?: string): Notice {
  if (because === 'dirty-document') {
    return {level:'warning', text:`${file} has unsaved edits. The current file was not updated. Save or resolve those edits before reconciling again.`};
  }
  if (because === 'not-ours') {
    return {level:'warning', text:`${file} is owned by another session. The current file was not updated. Resolve ownership before reconciling again.`};
  }
  /*
   * THE LATE REFUSAL (queue item 43, design v3 U8): the record moved after the
   * second wait's check -- a settlement landing during the working write --
   * and `reconcileBy` refused. The working note may already have been written.
   */
  if (because === 'record-moved') {
    return {
      level: 'warning',
      text:
        `${file} was not replaced: its record changed while the reconciliation was being applied (the working ` +
        'note may have been written); reconcile again.'
    };
  }
  return {
    level: 'warning',
    text:
      `${file}: the current projection could not be updated. Keep the file and resolve its publication or ownership state before retrying.`
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
 * remove. (section 12.17.3, section 12.13.4, section 12.11.7)
 */
export function refusalNotice(
  id: string,
  file: string,
  refusal:
    | { because: 'document-dirty' }
    | { because: 'working-unavailable'; detail:string }
    | { because: 'byte-order-mark' }
    | { because: 'not-utf8' }
    | { because: 'disk-differs-from-snapshot' }
    | { because: 'prefix-changed'; prefix: string }
    | { because: 'no-sidecar' }
    | { because: 'unresolved' }
    | { because: 'publication-incomplete' }
    | { because: 'outside-session'; file: string }
): Notice {
  if (refusal.because === 'working-unavailable') return {level:'error',text:`${id}: the working note was not confirmed saved. Your file is retained. Retry saving after resolving: ${refusal.detail}`};
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
 * NOTE: ALL THREE SENTENCES OR NONE. Dropping the second turns a
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
      `whether it is still running. It holds ` +
      say(pending, '1 unsent request', `${pending} unsent requests`) +
      '. Taking it over anyway is safe to attempt: if that window is in fact still running, ' +
      say(
        pending,
        'that request reaches the store twice',
        'each of those requests reaches the store twice'
      ) +
      ', and the store recognises the second by its request id and answers "already applied" ' +
      'rather than doing the work again. What it costs is the sending, not the change.'
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
     * NOTE: THIS ONE DOES NOT FIX ITSELF EITHER, and it used to be told to
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
     * NOTE: IT DOES NOT SAY THE WORK IS BEING SENT. Whoever holds the claim
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
 * NOTE: `skipped` COUNTS TWO DIFFERENT THINGS and the sentence must not
 * claim either: an entry already in this window's queue, and one the
 * source has marked as handed to some other claimant. An earlier comment
 * here said it meant the first, which a review pointed out is not
 * established.
 *
 * NOTE: AND MOVING THEM IS NOT SENDING THEM. Nothing here starts a drain:
 * the entries wait for the next save or an explicit retry, and a
 * sentence promising they are on their way would be describing work that
 * has not been scheduled.
 */
/*
 * ONE OR MORE THAN ONE, AND THE WHOLE SENTENCE EITHER WAY.
 *
 * A report that says "1 belong to other stores" reads as written by
 * something that did not look at what it was saying, which is the
 * impression to avoid in the one message a user acts on.
 *
 * NOTE: AND THE FIRST REPAIR WAS A WORD, WHICH IS NOT WHERE THE PROBLEM
 * WAS. Fixing the leading verb left "1 was written for other stores and
 * ARE still THERE ... bring THEM across" -- and a review reading all
 * eight sentences at a count of one found every one of them broken the
 * same way, in the verb or in a later pronoun or in both. "request(s)"
 * is the same evasion in punctuation. So each bucket carries two
 * finished sentences and picks one; nothing is assembled out of
 * fragments, and the singular is not a plural with a word swapped.
 */
function say(n: number, one: string, many: string): string {
  return n === 1 ? one : many;
}

export function adoptedNotice(
  sessionId: string,
  ledger: TakeoverLedger,
  nowhereToPutThem: boolean
): Notice {
  if (nowhereToPutThem) {
    /*
     * NOTE: NOTHING WAS TAKEN OVER. This used to say it was: the command
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
   * NOTE: ONE SENTENCE PER NON-EMPTY BUCKET, AND NO BUCKET WITHOUT ONE.
   *
   * The ledger's rule is that everything the takeover saw is in exactly
   * one bucket; this is the other half of it. A bucket the report did
   * not mention would be work that vanished between the count and the
   * user -- the same defect as a count that swallowed it, one step
   * later. The cell that adds the buckets guards the first half; this
   * list guards the second, and a bucket added without a sentence here
   * shows up as an unexplained difference in what the user is told.
   */
  /*
   * NOTE: THE IMPORTED SENTENCE IS CONDITIONAL LIKE EVERY OTHER. It was
   * unconditional, so a report about another store's requests opened
   * with "0 unsent request(s) are now in this window's queue" -- a
   * sentence about nothing, in front of the one the user needed. The
   * empty-bucket rule had been written for the others and not for this
   * one. Found the moment the cells began comparing whole sentences
   * rather than looking for fragments in them.
   */
  const parts: string[] = [];
  if (ledger.imported > 0) {
    parts.push(
      say(
        ledger.imported,
        "1 unsent request is now in this window's queue.",
        `${ledger.imported} unsent requests are now in this window's queue.`
      )
    );
  }
  if (ledger.skippedDuplicate > 0) {
    parts.push(
      say(
        ledger.skippedDuplicate,
        '1 had already been carried across and was left alone.',
        `${ledger.skippedDuplicate} had already been carried across and were left alone.`
      )
    );
  }
  if (ledger.leftOtherStore > 0) {
    parts.push(
      say(
        ledger.leftOtherStore,
        '1 was written for another store and is still there; configure that store and run this ' +
          'command again to bring it across.',
        `${ledger.leftOtherStore} were written for other stores and are still there; configure ` +
          'those stores and run this command again to bring them across.'
      )
    );
  }
  if (ledger.leftUnknownStore > 0) {
    parts.push(
      say(
        ledger.leftUnknownStore,
        '1 is in a queue from an older version of this extension, which does not record which ' +
          'store it was written for; this command will not move it into a store it cannot show ' +
          'it belongs to.',
        `${ledger.leftUnknownStore} are in a queue from an older version of this extension, ` +
          'which does not record which store they were written for; this command will not move ' +
          'them into a store it cannot show they belong to.'
      )
    );
  }
  if (ledger.unreadableQueue > 0) {
    /*
     * NOTE: "COULD NOT BE INSPECTED", NOT "COULD NOT BE READ". A path under
     * an ancestry this process cannot search, or one that turns out not
     * to be a directory, fails the same way as a corrupt queue -- and
     * saying a FILE could not be read asserts that a file is there,
     * which none of those establish. Found in review.
     */
    /*
     * NOTE: AND THE SENTENCE FITS BOTH WAYS A PATH CAN FAIL. "Could not be
     * inspected" overstates a file that WAS opened and read and whose
     * contents did not validate; "could not be read" overstated a path
     * that holds no file at all. What is true of both, and is what the
     * user needs, is that no queue could be loaded from there and
     * nothing was taken. Found in review.
     */
    parts.push(
      say(
        ledger.unreadableQueue,
        'a usable queue could not be loaded from 1 of its paths, and nothing was taken from it.',
        `a usable queue could not be loaded from ${ledger.unreadableQueue} of its paths, and ` +
          'nothing was taken from them.'
      )
    );
  }
  if (ledger.failedToMove > 0) {
    parts.push(
      say(
        ledger.failedToMove,
        '1 could not be moved into this window and is still in that window’s queue; nothing was ' +
          'lost, and running this command again will try it.',
        `${ledger.failedToMove} could not be moved into this window and are still in that ` +
          'window’s queue; nothing was lost, and running this command again will try them.'
      )
    );
  }
  if (ledger.movedButUnmarked > 0) {
    /*
     * NOTE: THESE ARRIVED -- the destination was asked, not assumed. The
     * sentence for the bucket beside this one says the opposite, and the
     * two were one bucket until a review pointed out that it described
     * work which had in fact moved as work the user should go looking
     * for elsewhere.
     *
     * NOTE: AND IT NO LONGER PROMISES THAT NOTHING IS SENT TWICE. It did,
     * and that was more than this code can know: the other window's copy
     * is still unmarked, so a later takeover into a DIFFERENT queue can
     * carry it again and send it. What is true is what the store does
     * with the second one, which is the same guarantee the forced
     * takeover rests on.
     */
    /*
     * NOTE: "WAS NOT CONFIRMED", NOT "COULD NOT BE MARKED". When the
     * destination stored the entry and then threw, the mark was never
     * ATTEMPTED -- saying it failed sends the reader to look at
     * permissions on a file nothing tried to write. Found in review.
     */
    parts.push(
      say(
        ledger.movedButUnmarked,
        '1 arrived here, and the other window’s copy was not confirmed as handed over, so a ' +
          'later takeover may carry it again. The store recognises a request it has already ' +
          'applied by its id and does not do the work twice.',
        `${ledger.movedButUnmarked} arrived here, and the other window’s copy was not confirmed ` +
          'as handed over, so a later takeover may carry them again. The store recognises a ' +
          'request it has already applied by its id and does not do the work twice.'
      )
    );
  }
  if (ledger.outcomeUnknown > 0) {
    /*
     * NOTE: NEITHER ANSWER. The destination could not say whether it has
     * them, and this window is not going to choose the comfortable one
     * on its behalf.
     */
    parts.push(
      say(
        ledger.outcomeUnknown,
        '1 could not be accounted for: this window could not find out whether it arrived. Look ' +
          'at both queues before deciding anything about it.',
        `${ledger.outcomeUnknown} could not be accounted for: this window could not find out ` +
          'whether they arrived. Look at both queues before deciding anything about them.'
      )
    );
  }
  /*
   * NOTE: AND THE ADVICE ONLY WHEN THERE IS SOMETHING HERE TO SEND. It was
   * unconditional, so a report about another store's requests, or about
   * outcomes nobody could establish, ended by telling the user those
   * would go out with their next save -- of a queue that does not hold
   * them. Found in review.
   */
  if (ledger.imported > 0 || ledger.movedButUnmarked > 0) {
    /*
     * NOTE: THE ADVICE NAMES ITS SUBJECT BY WHERE IT IS.
     *
     * It said "They go out with the next save", and when the sentence
     * before it was the one about requests whose whereabouts nobody
     * could establish, "they" read as those -- an offer to send what
     * this window has just said it cannot find.
     *
     * NOTE: AND "THE ONES THAT ARRIVED" DID NOT FIX IT, which is worth the
     * space: that sentence still sits immediately after the
     * unknown-outcome one, where "arrived" can be read as "whichever of
     * those turn out to have arrived"; and after the sentence about
     * requests carried across on an earlier run, where "arrived" does
     * not say arrived WHERE, or WHEN. A description picks its referent
     * out of the paragraph and can lose. A place cannot: every reading
     * the two sentences above invite is about something that is
     * somewhere else. Found in review.
     *
     * NOTE: AND IT SAYS WHAT IS TRIED, NOT WHAT IS ACHIEVED. Naming the
     * place fixed the subject and then let the verb overstate:
     * "everything now in this window's queue goes out with the next
     * save" is a promise this layer cannot keep. A save works through
     * the queue from the front and STOPS at the first request it cannot
     * settle -- deliberately, so that nothing goes out past an
     * unresolved one, and `saver.test.ts` pins that in "holds everything
     * behind an entry nobody can resolve" and "carries on past a settled
     * entry and stops at one that was kept". So a window that already
     * holds an unresolved request takes the imported ones no further,
     * and the sentence would have been false about exactly the user
     * whose queue is already in trouble. Found in review.
     */
    /*
     * NOTE: AND THE SAVE'S ATTEMPT IS CONDITIONAL, SO THE COMMAND GOES
     * FIRST. "The next save will try this window's queue" was still more
     * than `save` does: it calls `ensureCursor` before it drains, and a
     * store it cannot reach makes it answer `blocked` without offering
     * the queue a single request -- while `retry` goes straight at the
     * queue. So the sentence recommends the route that works, and says
     * what the other one depends on instead of leaving the reader to
     * find out. Reproduced in review against the real Saver. Found in
     * review.
     */
    parts.push(
      `Run "${RETRY_OUTBOX.title}" to try this window's queue now. The next save tries it too, ` +
        'but only after reaching the store, so a store that cannot be reached leaves the queue ' +
        'untouched.'
    );
  }
  /*
   * AND WHAT ITS QUEUE WRITES COULD NOT PROMISE, after everything else.
   * (queue item 22, design v4, K12) A write whose rename landed and whose
   * directory flush failed counted as done, so every sentence above stands;
   * this says the queue it wrote may not survive a power cut, one sentence
   * per queue file. This report is the one place a takeover's warning is
   * shown, and it makes the notice a warning.
   */
  const durability = ledger.durabilityWarnings.map(
    (warning) => `${warning.charAt(0).toUpperCase()}${warning.slice(1)}.`
  );
  /*
   * AND A TAKEOVER THAT FOUND NOTHING SAYS THAT, rather than naming the
   * window and stopping. Every bucket being empty is an answer.
   */
  if (parts.length === 0 && durability.length === 0) {
    return {
      level: 'information',
      text: `${sessionId} was taken over and there was nothing in it to move.`
    };
  }
  return {
    level: durability.length === 0 ? 'information' : 'warning',
    text: `${sessionId}: ${[...parts, ...durability].join(' ')}`
  };
}

/*
 * AND WHERE THE DISCARDED FILES WENT. The path is in the sentence
 * because "moved aside" without a destination is indistinguishable from
 * "deleted" to the person reading it.
 */
/*
 * NOTE: THE STORE CHANGED WHILE THE USER WAS DECIDING.
 *
 * The takeover was set up against the queue this window had when the
 * command started, and by the time it held that queue the window was
 * writing somewhere else. Nothing was moved, and the sentence says all
 * three things the user needs: that nothing happened, why, and that the
 * way forward is to run the command again -- which is true because the
 * claim is re-enterable by the window holding it.
 *
 * IT DOES NOT SAY "TRY AGAIN" AND STOP THERE. A second run against the
 * new store is not a repeat of the first: it surveys the new store, and
 * the entries that were declined here are reported as another store's,
 * still waiting, in that bucket's own sentence.
 */
export function storeChangedNotice(sessionId: string): Notice {
  return {
    level: 'warning',
    text:
      `${sessionId} was not taken over: the store this window writes to changed while you were ` +
      'deciding, so nothing was moved and nothing was lost. Run the command again to take it ' +
      'over for the store that is configured now.'
  };
}

export function discardedNotice(sessionId: string, trash: string, liveAdopters: string[]): Notice {
  const open =
    liveAdopters.length === 0
      ? ''
      : ' ' +
        say(
          liveAdopters.length,
          '1 other window still has documents open in it.',
          `${liveAdopters.length} other windows still have documents open in it.`
        );
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
export function unrecordedNotice(file: string, because: Unrecorded): Notice {
  if (because === 'split-changed') {
    /*
     * NOTE: NOT "THE HEADING CHANGED". The record can stop producing what
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
  if (because === 'number-taken') {
    return {
      level: 'warning',
      text:
        `The store accepted the save, but the record beside ${file} already holds that send's ` +
        'number for a different request, so the two disagree about which send it was. The record ' +
        'is marked and the request is kept: run reconcile on the block to settle it.'
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
