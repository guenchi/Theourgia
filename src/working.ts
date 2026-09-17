import {Client} from './client';
import {ProjectionSource, digestOfBytes} from './publication';
import {clause, clauseValue, headName, isSym, wire} from './wire';

export interface WorkingProjection {
  source: ProjectionSource;
  body: string;
  prefix: string;
}

// This is a working namespace, not an allocation of a physical log writer.
export class Working {
  constructor(private readonly client: Client, public readonly writer: string) {}

  public async read(block: string, prefix: string): Promise<WorkingProjection> {
    const answer = await this.client.request('read',[block,'--working-info','--writer',this.writer]);
    const value = answer.ok ? clause(answer.answers[0],'projection') : null;
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

  public async write(block: string, body: string, prefix: string, rebase=false, baseline?:ProjectionSource): Promise<WorkingProjection> {
    if (baseline && !rebase && (!baseline.basedOn || !baseline.cut)) {
      throw new Error('The displayed working baseline has no verifiable cut; reconcile it explicitly before saving.');
    }
    const answer=await this.client.request('write',[block,body,'--writer',this.writer,
      ...rebase?['--rebase']:baseline?['--based-on',baseline.basedOn as string,'--working-cut',baseline.cut as string,
        ...baseline.kind==='working'?['--working-parent-writer',baseline.writer,'--working-parent',baseline.version]:[]]:[]]);
    const datum=answer.answers[0];
    if (!answer.ok || headName(datum)!=='ok' || clauseValue(datum,'saved')!==block || clauseValue(datum,'writer')!==this.writer) {
      throw new Error(`The working note was not confirmed saved for ${block}: ${answer.text}`);
    }
    const version=clauseValue(datum,'version');
    const reopened=await this.read(block,prefix);
    if (reopened.source.kind!=='working' || reopened.source.version!==version || reopened.body!==body) {
      throw new Error(`The working note changed before its saved bytes could be verified for ${block}`);
    }
    return reopened;
  }
}
