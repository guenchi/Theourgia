import {Client} from './client';
import {readBlock} from './blocks';
import {recordedModeOf} from './datum-view';
import {BlockMode, ProjectionSource, digestOfBytes} from './publication';
import {Datum, answerOf, isList, isSym, wire} from './wire';

/*
 * A WORKING WRITE REFUSED BECAUSE THE BLOCK IS A DATUM. Its code is `body`;
 * a working write writes `src` beside it and changes nothing the store runs
 * (src/datum-view.ts). Nothing was sent.
 */
export class DatumBlockRefused extends Error {
  constructor(public readonly block: string) {
    super(`${block} is a datum block, and this editor cannot write one yet`);
  }
}

/*
 * A WRITE REFUSED BECAUSE THE BLOCK'S MODE COULD NOT BE TAKEN FOR TEXT: the
 * read failed, answered another block, or named a mode this build does not
 * know. Nothing was sent.
 */
export class ModeNotKnown extends Error {}

export interface WorkingProjection {
  source: ProjectionSource;
  body: string;
  prefix: string;
}

/*
 * THE DRAFT SPACE OF A WINDOW, from its session id: every draft a window
 * writes, every working read of its own drafts and every view of them it
 * analyses take the name from here, so no two places can spell it apart.
 */
export function windowWriter(sessionId: string): string {
  return `window-${sessionId.toLowerCase()}`;
}

// This is a working namespace, not an allocation of a physical log writer.
export class Working {
  constructor(private readonly client: Client, public readonly writer: string) {}

  public async read(block: string, prefix: string): Promise<WorkingProjection> {
    const answer = await this.client.request('read',[block,'--working-info','--writer',this.writer]);
    /*
     * NEVER: THE FORM THAT HOLDS THE PROJECTION HAS TO HAVE SAID `ok`.
     *
     * This took the clause out of whatever came back, checking only the
     * exit code, while `write` below checks the head of its own answer.
     * Measured in an eleventh review round: a successful answer reading
     * `(garbage (projection working "draft" "a.1" "v1" "base" () "body"
     * "prefix"))` produced a working projection that looked verified.
     * The same shape as the search, catalogue, block and envelope
     * readers -- five places, found one at a time.
     */
    const form =
      answer.ok && answer.answers.length > 0 ? answerOf(answer.answers[0], 'ok') : null;
    /*
     * NOTE: THE WHOLE CLAUSE, because its digest is this projection's
     * identity -- see `Form.whole`.
     */
    const clause = form === null ? null : form.whole('projection');
    const value = clause !== null && clause.read ? clause.items : null;
    if (!value || value.length !== 9 || !isSym(value[1]) ||
        !['working','committed'].includes(value[1].name) || value[2] !== this.writer || value[3] !== block ||
        !(value[4] === false || typeof value[4] === 'string') || typeof value[5] !== 'string' || typeof value[7] !== 'string' || typeof value[8] !== 'string') {
      throw new Error(`Working source for ${block} could not be verified: ${answer.text}`);
    }
    const kind = value[1].name as 'working'|'committed';
    if (kind === 'working' && typeof value[4] !== 'string') throw new Error('Working source has no version');
    return {
      source:{id:digestOfBytes(wire().write(value)+'\n'+prefix),kind,writer:this.writer,
        version:typeof value[4] === 'string'?value[4]:wire().write(value[6]),basedOn:value[5],cut:wire().write(value[6])},
      body:value[7],prefix:value[8]
    };
  }

  /*
   * THE ONE DOOR OF A WORKING WRITE. `mode` is what the file's record says
   * the block is: text writes as before, with no request of its own; datum
   * is refused before anything is sent; null -- a file made without a
   * recorded mode -- reads the block first, and a mode that cannot be read
   * is refused, because an unknown mode is not text. The answer carries
   * `text` only when this write read the block and found it text -- the one
   * case its caller may record it -- and nothing when it went by a mode the
   * record already held.
   */
  public async write(block: string, body: string, prefix: string, rebase=false, baseline?:ProjectionSource, mode: BlockMode|null=null): Promise<WorkingProjection & {mode?:'text'}> {
    if (baseline && !rebase && (!baseline.basedOn || !baseline.cut)) {
      throw new Error('The displayed working baseline has no verifiable cut; reconcile it explicitly before saving.');
    }
    const read = await requireText(this.client, block, mode);
    const answer=await this.client.request('write',[block,body,'--writer',this.writer,
      ...rebase?['--rebase']:baseline?['--based-on',baseline.basedOn as string,'--working-cut',baseline.cut as string,
        ...baseline.kind==='working'?['--working-parent-writer',baseline.writer,'--working-parent',baseline.version]:[]]:[]]);
    const saved=answer.ok&&answer.answers.length>0?answerOf(answer.answers[0],'ok'):null;
    const confirmedBlock=saved===null?null:saved.value('saved');
    const confirmedWriter=saved===null?null:saved.value('writer');
    if (!saved || confirmedBlock===null || !confirmedBlock.read || confirmedBlock.value!==block ||
        confirmedWriter===null || !confirmedWriter.read || confirmedWriter.value!==this.writer) {
      throw new Error(`The working note was not confirmed saved for ${block}: ${answer.text}`);
    }
    const stated=saved.value('version');
    const version=stated.read?stated.value:undefined;
    const reopened=await this.read(block,prefix);
    if (reopened.source.kind!=='working' || reopened.source.version!==version || reopened.body!==body) {
      throw new Error(`The working note changed before its saved bytes could be verified for ${block}`);
    }
    return read ? {...reopened,mode:'text'} : reopened;
  }


}

/*
 * THE MODE GATE, for a write of `src` by any route: a recorded text passes
 * with no request; a recorded datum is refused; no recorded mode reads the
 * block (`read <id>` answers `(ok <block>)`, and it has to be the block
 * asked for). A datum block is refused, and a block whose mode cannot be
 * read, or is one this build does not know, is refused too: an unknown mode
 * is not text. Nothing is sent before it passes. The answer says whether the
 * block was read (and found text) here.
 */
export async function requireText(client: Client, block: string, mode: BlockMode|null): Promise<boolean> {
  if (mode==='text') return false;
  if (mode==='datum') throw new DatumBlockRefused(block);
  let answer;
  try {
    answer = await client.request('read',[block]);
  } catch (error) {
    throw new ModeNotKnown(`the mode of ${block} could not be read, so it is not saved as text: ${String(error)}`);
  }
  const datum = answer.ok && answer.answers.length > 0 ? answer.answers[0] : null;
  const record = datum !== null && isList(datum) && datum.length >= 2 && answerOf(datum,'ok') !== null ? readBlock(datum[1]) : null;
  if (record === null || record.id !== block) {
    throw new ModeNotKnown(`the mode of ${block} could not be read, so it is not saved as text: ${answer.text.trim() || 'no answer'}`);
  }
  const read = recordedModeOf(record);
  if (read === 'datum') throw new DatumBlockRefused(block);
  if (read === null) {
    throw new ModeNotKnown(`the mode of ${block} is not one this editor knows, so it is not saved as text: ${wire().write(record.fields.get('mode') as Datum)}`);
  }
  return true;
}
