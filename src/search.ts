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
 * ⚠️ THE QUERY IS ONE ARGUMENT. `search <query>` takes a single one,
 * and the words in it are all required to match; handing the core two
 * words as two arguments gets `(usage (search <query>))` back, which is
 * a refusal about the command line rather than a search with no
 * results. Measured against the pinned core. The joining happens where
 * the request is built, so no caller can arrive at a different rule.
 *
 * ⚠️ AND NOTHING HERE PARSES A RENDERING. `--wire` answers are data:
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
}

/*
 * NULL MEANS THE ANSWER WAS NOT READ, AND AN EMPTY ARRAY MEANS THE STORE
 * FOUND NOTHING. They are one collapse apart and they are opposite news:
 * the store saying it looked, and the store saying it did not.
 *
 * ⚠️ TWO SHAPES ARRIVE HERE, AND BOTH ARE READ. Measured against the
 * pinned core:
 *
 *     $ theourgia search "stale baseline"
 *     (hit "qqevbgov.1" 6 "concurrency, baseline, stale")
 *     $ theourgia search "stale baseline" --wire
 *     (ok (items (hit "qqevbgov.1" 6 "concurrency, baseline, stale")))
 *
 * This extension asks for neither mode by name, so it gets the first:
 * the client already treats `search` as a verb whose answer is a
 * sequence of items, exactly as it treats `refs`, `log` and
 * `conflicts`. The wrapper is accepted as well because accepting it
 * costs three lines and because the day this client does ask for
 * `--wire` -- it will, for the clause the human rendering drops -- the
 * search must not become the reason that change is hard.
 *
 * ⚠️ ON THE HUMAN ROUTE NO HITS IS NO OUTPUT AT ALL, so an empty list
 * arrives as an empty list and there is nothing to unwrap. That is why
 * an empty input is an empty answer here and not an unreadable one.
 */
export function hitsOf(data: Datum[]): Hit[] | null {
  /*
   * ⛔ AN `items` CLAUSE IS NOT AN ENVELOPE UNLESS THE FORM SAID `ok`.
   *
   * Found by an outside review and reproduced: this asked only whether
   * a clause of that name was present, so `(error (items))` -- a refusal
   * that happens to carry one -- was unwrapped into no hits, and the
   * user was told the store had looked and found nothing. The head is
   * what says whether this is an answer at all.
   */
  /*
   * ⛔ AND A SECOND GUARD HERE WOULD HAVE NO CASE BEHIND IT EITHER.
   *
   * The decoder tells "two of this clause" from "no such clause" since a
   * sixteenth review round, and three of its callers had been reading
   * the first as the second. One was written here -- refuse when `items`
   * came back duplicated -- and a mutation run showed it changed
   * nothing: `(ok (items ...) (items ...))` is one datum, so it falls
   * through as the item list, and `(ok ...)` is not a `hit`, so the loop
   * below already answers null. It was deleted rather than left looking
   * like a repair.
   *
   * That is the SECOND guard deleted from this function for having no
   * case behind it; the note above records the first. Both times the
   * reason was the same: the refusal one line below is wider than the
   * guard being added over it.
   */
  const envelope = data.length === 1 ? answerOf(data[0], 'ok') : null;
  const wrapped = envelope === null ? null : envelope.clause('items');
  const items = wrapped !== null && wrapped.read ? wrapped.items : data;
  /*
   * ⚠️ THIS READER DECIDES BY SHAPE, AND THAT IS THE DEFECT THIS
   * DELIVERY REPAIRED ELSEWHERE -- named here because removing it
   * changes what two cells expect.
   *
   * `client.ts` was repaired so that unwrapping turns on whether the
   * REQUEST asked for `--wire`; this is a second reader making the same
   * decision from the shape of what came back. A fifteenth review round
   * measured the consequence: for a search carrying `--wire`,
   * `interpret` removes one envelope and this removes a second, so
   * `(ok (items (ok (items (hit "a.1" 2 "x")))))` yields the hit.
   *
   * ⛔ A GUARD HERE WOULD HAVE NO CASE BEHIND IT. One was written --
   * refuse when the single item is itself an `ok` form -- and a mutation
   * run showed it changed nothing: that input is already refused one
   * line below, because `(ok ...)` is not a `hit`. It was deleted rather
   * than left looking like a repair.
   *
   * The repair is to stop unwrapping here at all, since this extension
   * does not ask for `--wire` on a search and the pinned core emits
   * nothing for a search with no hits on the human route. That is a
   * change to two cells' expectations, so it is in the delivery note for
   * a ruling.
   */
  const out: Hit[] = [];
  for (const item of items) {
    if (!isList(item) || answerOf(item, 'hit') === null || item.length !== 4) {
      return null;
    }
    const id = item[1];
    const score = asInteger(item[2]);
    const note = item[3];
    if (typeof id !== 'string' || score === null || typeof note !== 'string') {
      return null;
    }
    out.push({ id, score, note });
  }
  return out;
}

/*
 * BEST FIRST, AND THE SAME ORDER EVERY TIME.
 *
 * ⭐ TIES ARE BROKEN BY THE ID rather than left as they arrived. Two
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
 * ⚠️ WHY THIS EXISTS AT ALL. The definition search wants `whereis`,
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
   * ⛔ A `verbs` CLAUSE IS NOT A CATALOGUE UNLESS THE FORM SAID `ok`.
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
   * ⚠️ BOTH REASONS REFUSE. A `describe` with no catalogue and one
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
  | { did: 'opened'; id: string; query: string; of: number }
  | { did: 'failed'; query: string; because: string };

/*
 * ⚠️ THE QUERY IS TRIMMED AND AN EMPTY ONE IS NOT SENT. A box the user
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
  const chosen = await editor.pick(hits, `${hits.length} blocks match "${query}"`);
  if (chosen === undefined) {
    return { did: 'nothing', because: 'cancelled' };
  }
  await editor.open(chosen.id);
  return { did: 'opened', id: chosen.id, query, of: hits.length };
}
