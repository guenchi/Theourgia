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
 * What the store found, and what the store can do.
 *
 * NOTE: THE QUERY IS ONE ARGUMENT. `search <query>` takes a single one,
 * and the words in it are all required to match; handing the core two
 * words as two arguments gets `(usage (search <query>))` back, which is
 * a refusal about the command line rather than a search with no
 * results. Measured against the pinned core. The joining happens where
 * the request is built, so no caller can arrive at a different rule.
 *
 * NOTE: AND NOTHING HERE PARSES A RENDERING. `--wire` answers are data:
 * `(ok (items (hit <id> <score> <note>) ...))`. The keywords or the cut
 * of text in the third position may contain anything at all, and it is
 * carried as a value rather than cut out of a line.
 */

import { Datum, answerOf, asInteger, isList, isSym } from './wire';

export interface Hit {
  id: string;
  score: number;
  /*
   * THE LINE SHOWN UNDER THE TITLE. The core puts the block's keywords
   * here when the match was found in them and a cut of the text
   * otherwise, and does NOT say which it is doing. So this field is not
   * named `keywords`: a name that was right half the time would have the
   * next reader of this file believing something false about the other
   * half.
   */
  note: string;
  /*
   * WHICH FIELDS THE QUERY MATCHED, when the core says so.
   *
   * NOTE: UNDEFINED MEANS THE ANSWER DID NOT CARRY IT, not that the hit
   * matched nothing. A core older than the S batch answers a hit with
   * four elements and no `fields` clause at all, and `theourgia.corePath`
   * can always name one -- so the absence is a fact about the core, and
   * a caller that drew "matched no field" from it would be reading this
   * client's age rather than the store.
   */
  fields?: string[];
}

/*
 * NULL MEANS THE ANSWER WAS NOT READ, AND AN EMPTY ARRAY MEANS THE STORE
 * FOUND NOTHING. They are one collapse apart and they are opposite news:
 * the store saying it looked, and the store saying it did not.
 *
 * NEVER: AND THIS READER NO LONGER DECIDES ANYTHING FROM THE SHAPE OF WHAT
 * CAME BACK. Ruled by the main session after three review rounds found a
 * face of it.
 *
 * It used to unwrap an `(ok (items ...))` envelope when it saw one. That
 * is the defect `client.ts` was repaired for: whether an envelope was
 * asked for is a fact about the REQUEST, and what came back merely looks
 * a certain way. With both readers unwrapping, a search carrying
 * `--wire` had `interpret` remove one envelope and this remove a second.
 * Two guards were written here over those rounds and both were deleted
 * for having no case behind them -- the refusal one line below is wider
 * than either.
 *
 * So: this takes the answers `interpret` already opened. On the human
 * route the core prints one datum per hit and no hits is no output at
 * all, which is why an empty input is an empty answer and not an
 * unreadable one.
 */
export function hitsOf(data: Datum[]): Hit[] | null {
  /*
   * NEVER: AN `items` CLAUSE IS NOT AN ENVELOPE UNLESS THE FORM SAID `ok`.
   *
   * Found by an outside review and reproduced: this asked only whether
   * a clause of that name was present, so `(error (items))` -- a refusal
   * that happens to carry one -- was unwrapped into no hits, and the
   * user was told the store had looked and found nothing. The head is
   * what says whether this is an answer at all.
   */
  /*
   * NOTE: THE TWO GUARDS THAT WERE DELETED FROM HERE, kept as a record
   * because both were written in good faith and neither could fail.
   *
   * The first refused an item that was itself an `ok` form; the second
   * refused a duplicated `items` clause. Each was measured with a
   * mutation and each changed nothing, because the loop below already
   * refuses anything that is not a `hit`. With the unwrapping gone
   * neither has a subject at all.
   */
  const items = data;
  const out: Hit[] = [];
  for (const item of items) {
    /*
     * NEVER: A HIT THAT CARRIES MORE THAN THIS BUILD KNOWS IS STILL A HIT.
     *
     * This asked for a length of exactly four. The core's S batch adds a
     * fifth element -- `(fields (title keywords ...))`, which fields the
     * query matched -- and an exact length would have made every search
     * against that core answer "the search did not happen", for every
     * query, the day it landed. Told in advance by the main session and
     * done before it lands rather than after.
     *
     * Four is the minimum and the first four positions do not move, so a
     * sixth element some later core adds is read the same way. This is
     * the accepting end of the rule this tree keeps: take a wider range
     * than is needed, hand on a narrower one.
     *
     * NEVER: AND THERE IS NO LOWER BOUND HERE, BECAUSE IT HAD NO CASE.
     *
     * `item.length < 4` was written beside the relaxation and a mutation
     * run showed it changed nothing: a hit with fewer than four elements
     * has `item[3]` undefined, and the three checks below already refuse
     * it -- measured by loosening the bound to `< 3` and watching every
     * row stay green. That is the THIRD guard deleted from this function
     * for having no case behind it; the notes above record the other
     * two. The rule this function keeps being tested against is that a
     * refusal one line down is wider than the guard being added over it.
     */
    if (!isList(item)) {
      return null;
    }
    const hit = answerOf(item, 'hit');
    if (hit === null) {
      return null;
    }
    const id = item[1];
    const score = asInteger(item[2]);
    const note = item[3];
    if (typeof id !== 'string' || score === null || typeof note !== 'string') {
      return null;
    }
    /*
     * AND THE FIFTH IS READ THROUGH THE FORM WHOSE HEAD WAS CHECKED.
     *
     * NEVER: NOT THROUGH THE RECORD READER. The first version reached for
     * `clauseOfRecord(item.slice(4), 'fields')`, and the census over
     * headless readers refused it -- rightly: the tail of a `hit` is not
     * a record with no head, it is the remainder of a form whose head
     * this line has just verified, and the door held open for headless
     * values is not for it. Asking the form is also the reading that
     * refuses two `fields` clauses rather than taking the first.
     */
    const matched = hit.clause('fields');
    if (!matched.read && matched.because !== 'absent') {
      return null;
    }
    const fields = matched.read
      ? matched.items.filter((name): name is { name: string } => isSym(name)).map((n) => n.name)
      : undefined;
    if (matched.read && fields !== undefined && fields.length !== matched.items.length) {
      return null;
    }
    out.push(fields === undefined ? { id, score, note } : { id, score, note, fields });
  }
  return out;
}

/*
 * BEST FIRST, AND THE SAME ORDER EVERY TIME.
 *
 * KEY: TIES ARE BROKEN BY THE ID rather than left as they arrived. Two
 * blocks with one score are a common answer -- a two-word query matching
 * two keywords scores the same on both -- and an order that came from
 * the store's walk can differ between two runs of one search. A list
 * that draws itself differently each time is one a person cannot point
 * at, and a cell pinned to one of those orders passes until it does not.
 */
export function rankHits(hits: Hit[]): Hit[] {
  return [...hits].sort((a, b) => (b.score - a.score) || (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
}

/*
 * EVERY VERB THE CORE SAID IT HAS.
 *
 * NOTE: WHY THIS EXISTS AT ALL. The definition search wants `whereis`,
 * which the core does not have yet. This extension does not contribute a
 * command that answers "not implemented": that is a promise with nobody
 * responsible for it, and a stub is a thing nobody goes back to remove.
 * It asks instead, and the entry appears the day the verb does.
 *
 * NULL MEANS THE CATALOGUE COULD NOT BE READ -- a refused `describe`, a
 * shape this build does not know -- and an empty set means a core that
 * listed no verbs. Returning an empty set for the first would hide every
 * verb the core has, including the ones this extension has always used.
 */
export function knownVerbs(answer: Datum): Set<string> | null {
  /*
   * NEVER: A `verbs` CLAUSE IS NOT A CATALOGUE UNLESS THE FORM SAID `ok`.
   *
   * `hitsOf` was repaired for exactly this in an earlier round and this
   * reader was left as it was, because the repair was made where the
   * finding pointed. Measured in a tenth review round:
   * `(error (verbs))` answered an empty set and
   * `(garbage (verbs (search)))` answered a catalogue -- a refusal read
   * as "this core can do nothing", and a form nobody can parse read as
   * a list of what it can do.
   */
  const form = answerOf(answer, 'ok');
  if (form === null) {
    return null;
  }
  /*
   * NOTE: BOTH REASONS REFUSE. A `describe` with no catalogue and one
   * carrying two are both answers this build cannot take a list of
   * verbs from, and null is "I could not find out" -- which is what the
   * caller must tell apart from a core with no verbs.
   */
  const verbs = form.clause('verbs');
  if (!verbs.read) {
    return null;
  }
  const out = new Set<string>();
  for (const entry of verbs.items) {
    /*
     * A VERB ENTRY IS `(<name> (usage ...) ...)`, so its head IS the
     * name. Asking the decoder for a head it does not know yet is not
     * what it is for; what is needed here is to read one, and an entry
     * with no readable head is a catalogue this build cannot account
     * for.
     */
    if (!isList(entry) || entry.length === 0 || !isSym(entry[0])) {
      return null;
    }
    out.add((entry[0] as { name: string }).name);
  }
  return out;
}

/*
 * THE FLOW, WITH THE EDITOR HELD AT ARM'S LENGTH.
 *
 * What a search DOES -- refuse an empty query, say so when nothing
 * matched, open a single hit without asking, offer a list when there are
 * several -- is a set of decisions, and decisions that live inside a
 * command handler are decisions no cell can drive. The editor arrives
 * as three functions and the outcome is returned as well as shown.
 */
export interface Asking {
  ask(prompt: string): Promise<string | undefined>;
  pick(hits: Hit[], placeHolder: string): Promise<Hit | undefined>;
  say(text: string, level: 'information' | 'error'): void;
  open(id: string): Promise<void>;
}

export interface Searcher {
  search(query: string): Promise<Hit[]>;
}

export type SearchOutcome =
  | { did: 'nothing'; because: 'cancelled' | 'empty-query' | 'no-store' }
  | { did: 'nothing'; because: 'no-hits'; query: string }
  /*
   * NOTE: `of` IS HOW MANY WERE OFFERED, not how many match -- for the
   * reason written at the picker below.
   */
  | { did: 'opened'; id: string; query: string; of: number }
  | { did: 'failed'; query: string; because: string };

/*
 * NOTE: THE QUERY IS TRIMMED AND AN EMPTY ONE IS NOT SENT. A box the user
 * dismissed and a box they left blank both come back as nothing to look
 * for, and sending that to the core gets a usage line back -- a
 * complaint about a command line, shown to somebody who never typed one.
 */
export async function runSearch(
  model: Searcher | null,
  editor: Asking
): Promise<SearchOutcome> {
  if (model === null) {
    editor.say('theourgia has no store to search; check the settings.', 'error');
    return { did: 'nothing', because: 'no-store' };
  }
  const typed = await editor.ask('Find blocks whose title, keywords or text hold every word');
  if (typed === undefined) {
    return { did: 'nothing', because: 'cancelled' };
  }
  const query = typed.trim();
  if (query.length === 0) {
    return { did: 'nothing', because: 'empty-query' };
  }
  let hits: Hit[];
  try {
    hits = await model.search(query);
  } catch (e) {
    const because = e instanceof Error ? e.message : String(e);
    editor.say(`the search for "${query}" did not happen: ${because}`, 'error');
    return { did: 'failed', query, because };
  }
  if (hits.length === 0) {
    /*
     * NOTHING MATCHED IS AN ANSWER AND IT IS SAID. A quick pick with no
     * rows in it closes itself, which is what a cancelled search also
     * looks like.
     */
    editor.say(`nothing in the store matches "${query}".`, 'information');
    return { did: 'nothing', because: 'no-hits', query };
  }
  /*
   * ONE HIT OPENS. Asking somebody to choose between one thing is asking
   * them to agree with a decision already made.
   */
  if (hits.length === 1) {
    await editor.open(hits[0].id);
    return { did: 'opened', id: hits[0].id, query, of: 1 };
  }
  /*
   * NEVER: THE PLACEHOLDER SAYS HOW MANY IT IS SHOWING, NOT HOW MANY MATCH.
   *
   * It read `${hits.length} blocks match "${query}"`. From the core's C2
   * on, `search` answers the best ten by default, and the number cut off
   * is reported only by `(truncated (hits n))` under `--wire` -- which
   * this client does not ask for on a search. So the sentence would tell
   * a user that ten blocks match when forty-seven do, with no way here to
   * know better: what arrived, described as what exists.
   *
   * KEY: When a number's provenance is "how many I was handed", the
   * sentence may only say how many it is showing. "10 shown" is true on
   * every core; "10 match" and "found 10" are true only on a core that
   * does not cut.
   *
   * NOTE: AND THE REMEDY IS NOT TO FETCH EVERYTHING. `--all` would make the
   * old sentence true by asking for every hit so that a count can be
   * printed, which is the cost running the wrong way and gets worse as a
   * store grows. The total becomes available when this client asks for
   * `--wire` on a search -- and that is one change together with the
   * ruling that `hitsOf` does not open envelopes, not a special case for
   * one number. Ruled by the main session, 2026-09-21.
   */
  const chosen = await editor.pick(hits, `${hits.length} shown for "${query}", most relevant first`);
  if (chosen === undefined) {
    return { did: 'nothing', because: 'cancelled' };
  }
  await editor.open(chosen.id);
  return { did: 'opened', id: chosen.id, query, of: hits.length };
}
