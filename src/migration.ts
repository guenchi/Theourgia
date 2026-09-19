import * as path from 'path';
import {randomUUID} from 'crypto';
import {FileOps,withExclusive} from './fsops';
import {Publisher, ProjectionSource, sidecarFromDisk, digestOfBytes} from './publication';
import {replaceText} from './temporary';

export interface MigrationSources {
  committed: {text:string;prefix:string;source:ProjectionSource};
  working: {text:string;prefix:string;source:ProjectionSource} | null;
}
export type MigrationOutcome =
  | {migrated:true;file:string;archive:string}
  | {migrated:false;because:string;retained:string[]};

/*
 * ⛔ A JOURNAL THIS COULD NOT READ IS NOT A JOURNAL THAT DOES NOT NAME
 * THIS BLOCK.
 *
 * The catch walked past it and the function then answered null, which
 * the caller draws as "this is not a legacy block of the configured
 * store" -- so a prepared migration whose journal could not be opened
 * was reported to the user as nothing to migrate. Measured in a
 * fifteenth review round with EACCES; `migrateLegacy`, repaired a round
 * earlier, answers `unreadable-migration-record` for the same reader.
 * Two readers of one file, and this one gave the reassuring verdict.
 *
 * It throws. The caller has no branch for an unreadable journal and
 * inventing one here would be guessing what a person should be told;
 * the failure reaches `reportFailure`, which says what happened.
 */
export function migrationIdentity(files:FileOps,directory:string):{storeId:string;blockId:string;prefix:string}|null {
  const control=path.join(path.dirname(directory),'.migration');
  for(const name of files.list(control).filter(n=>n.endsWith('.json'))) {
    try {
      const plan=JSON.parse(files.readText(path.join(control,name)));
      if(plan.format===1 && plan.phase==='prepared' && plan.directory===directory &&
         typeof plan.storeId==='string' && typeof plan.blockId==='string' && typeof plan.selected?.prefix==='string') {
        return {storeId:plan.storeId,blockId:plan.blockId,prefix:plan.selected.prefix};
      }
    } catch (e) {
      throw new Error(
        `the migration journal at ${path.join(control,name)} could not be read, so whether this ` +
          `is a legacy block of a prepared migration is not known: ${(e as Error).message}`
      );
    }
  }
  return null;
}

// Migration retires only bytes that can already be rendered from core authority.
// Unknown originals remain named in place; it never writes successive drafts to one W key.
export async function migrateLegacy(parts: {
  files:FileOps; publisher:Publisher; directory:string; storeId:string; blockId:string;
  sourceIsSafe:()=>Promise<boolean>;
  sourceStillSafe:()=>boolean;
  claimDestination:()=>boolean;
  sources:()=>Promise<MigrationSources>;
  isDirty:(file:string)=>boolean;
  pending:()=>boolean;
}): Promise<MigrationOutcome> {
  const {files,directory}=parts;
  const control=path.join(path.dirname(directory),'.migration');
  // Resume a prepared move before interpreting the now-empty legacy location.
  for (const name of files.list(control).filter(n=>n.endsWith('.json'))) {
    const journal=path.join(control,name);
    let plan:any;
    /*
     * ⛔ A JOURNAL THIS COULD NOT READ IS NOT A JOURNAL THAT SAYS
     * NOTHING.
     *
     * `continue` walked past it, and with the legacy directory then
     * empty this reported `no-legacy-files` -- "there was nothing to
     * migrate" about a prepared move it had not been able to open.
     * Measured in a fourteenth review round with EACCES on the journal.
     * A record that will not parse is already refused below as
     * `invalid-migration-record`; one that will not READ is the same
     * kind of news.
     */
    try {plan=JSON.parse(files.readText(journal));} catch {
      return {migrated:false,because:'unreadable-migration-record',retained:[journal]};
    }
    if (plan?.directory!==directory || plan.phase!=='prepared') continue;
    if (plan.storeId!==parts.storeId || plan.blockId!==parts.blockId || typeof plan.archive!=='string' ||
        path.dirname(plan.archive)!==control || !Array.isArray(plan.originals) || !plan.selected ||
        typeof plan.selected.text!=='string' || typeof plan.selected.prefix!=='string' || !plan.selected.source?.id) {
      return {migrated:false,because:'invalid-migration-record',retained:[journal]};
    }
    const retained=[journal,plan.archive];
    if (!await parts.sourceIsSafe() || parts.pending()) return {migrated:false,because:'active-source-or-pending',retained};
    if (!files.exists(plan.archive)) continue;
    return withExclusive(directory, (): MigrationOutcome => {
    if (!parts.sourceStillSafe() || parts.pending()) return {migrated:false,because:'active-source-or-pending',retained};
    const current=path.join(directory,'current.md'),held=parts.publisher.sidecarOf(current);
    if (parts.isDirty(current)) return {migrated:false,because:'dirty-document',retained};
    if (held && held.projection?.id!==plan.selected.source.id) return {migrated:false,because:'projection-changed',retained};
    for (const original of plan.originals) {
      const archived=path.join(plan.archive,path.basename(original.file));
      if (!files.exists(archived) || digestOfBytes(files.readBytes(archived))!==original.digest) return {migrated:false,because:'archive-changed',retained};
    }
    try {
      if (!parts.claimDestination()) return {migrated:false,because:'not-ours',retained};
      if (held?.phase==='publishing') parts.publisher.recoverCurrent(current,{source:plan.selected.source,text:plan.selected.text});
      const result=parts.publisher.publishNow({directory,storeId:parts.storeId,blockId:parts.blockId,
        prefix:plan.selected.prefix,text:plan.selected.text,projection:plan.selected.source,cursor:null});
      if (!result.published) return {migrated:false,because:result.because,retained};
      replaceText(files,journal,JSON.stringify({format:1,phase:'complete',directory,archive:plan.archive,file:result.file})+'\n');
      return {migrated:true,file:result.file,archive:plan.archive};
    } catch (error) {return {migrated:false,because:`projection-incomplete: ${String(error)}`,retained};}
    });
  }
  const names=files.list(directory).filter(n=>/^\d+\.md(?:\.meta)?$/.test(n)).sort();
  const paths=names.map(n=>path.join(directory,n));
  const refuse=(because:string):MigrationOutcome=>({migrated:false,because,retained:paths});
  if (names.length===0) return refuse('no-legacy-files');
  if (names.some(n=>n.endsWith('.meta') && !names.includes(n.slice(0,-5)))) return refuse('unpaired-sidecar');
  if (!await parts.sourceIsSafe()) return refuse('active-source');
  if (parts.pending()) return refuse('pending-sends');
  const originals=paths.map(file=>({file,bytes:files.readBytes(file)}));
  let sources:MigrationSources;
  try {sources=await parts.sources();} catch {return refuse('W-required');}
  const rendered=[sources.committed,...sources.working?[sources.working]:[]];
  for (const original of originals.filter(o=>o.file.endsWith('.md'))) {
    if (parts.isDirty(original.file)) return refuse('dirty-document');
    const meta=originals.find(o=>o.file===original.file+'.meta');
    if (!meta) return refuse('unknown-origin');
    const read=sidecarFromDisk(meta.bytes.toString('utf8'));
    if (!read.read || read.sidecar.storeId!==parts.storeId || read.sidecar.blockId!==parts.blockId ||
        read.sidecar.phase!=='published' || read.sidecar.unresolved) return refuse('unknown-origin');
    if (read.sidecar.outstanding.length || read.sidecar.legacySend) return refuse('pending-sends');
    if (!rendered.some(r=>r.prefix===read.sidecar.prefix && Buffer.from(r.text,'utf8').equals(original.bytes))) {
      return refuse('unprotected-drafts');
    }
  }
  // Recheck after every asynchronous source read. A pending send keeps its original path.
  if (!await parts.sourceIsSafe()) return refuse('active-source');
  return withExclusive(directory, (): MigrationOutcome => {
  if (!parts.sourceStillSafe()) return refuse('active-source');
  if (parts.pending()) return refuse('pending-sends');
  if (files.list(directory).filter(n=>/^\d+\.md(?:\.meta)?$/.test(n)).sort().join('\0')!==names.join('\0')) return refuse('source-changed');
  for (const original of originals) {
    if (parts.isDirty(original.file) || !files.readBytes(original.file).equals(original.bytes)) return refuse('source-changed');
  }
  const token=randomUUID();
  const archive=path.join(control,`${path.basename(directory)}-${token}`);
  files.makeDirectory(control);
  const journal=path.join(control,`${token}.json`);
  const selected=sources.working ?? sources.committed;
  const plan={format:1,directory,archive,storeId:parts.storeId,blockId:parts.blockId,selected,originals:originals.map(o=>({file:o.file,digest:digestOfBytes(o.bytes)}))};
  replaceText(files,journal,JSON.stringify({...plan,phase:'prepared'})+'\n');
  // A unique one-time recovery archive is not used by any historical read path.
  files.rename(directory,archive);
  files.syncDirectory(path.dirname(directory));files.syncDirectory(control);
  try {
    if (!parts.claimDestination()) return {migrated:false,because:'not-ours',retained:[archive,journal]};
    const published=parts.publisher.publishNow({directory,storeId:parts.storeId,blockId:parts.blockId,
      prefix:selected.prefix,text:selected.text,projection:selected.source,cursor:null});
    if (!published.published) return {migrated:false,because:published.because,retained:[archive,journal]};
    replaceText(files,journal,JSON.stringify({format:1,phase:'complete',directory,archive,file:published.file})+'\n');
    return {migrated:true,file:published.file,archive};
  } catch (error) {
    return {migrated:false,because:`projection-incomplete: ${String(error)}`,retained:[archive,journal]};
  }
  });
}
