import {FileOps} from './fsops';
import * as path from 'path';
import {randomUUID} from 'crypto';

export function temporaryFor(files: FileOps, target: string): string {
  const directory = path.dirname(target);
  const staging = path.basename(target).startsWith('current.md')
    ? path.join(path.dirname(directory), '.projection-io', path.basename(directory)) : directory;
  files.makeDirectory(staging);
  return path.join(staging, `${path.basename(target)}.${process.pid}.${randomUUID()}.tmp`);
}

export function replaceText(files: FileOps, target: string, text: string): void {
  const temporary = temporaryFor(files, target);
  let promoted = false;
  try {
    files.writeDurably(temporary, text);
    files.rename(temporary, target);
    promoted = true;
    files.syncDirectory(path.dirname(target));
    if (path.dirname(temporary) !== path.dirname(target)) files.syncDirectory(path.dirname(temporary));
  } catch (original) {
    // Once renamed, the canonical object is evidence for recovery, never garbage.
    if (promoted) throw original;
    throw cleanupFailure(cleanupTemporary(files, temporary, original));
  }
}

export interface CleanupObservation {
  attempt: 'removed' | 'failed';
  presence: 'not-probed' | 'present' | 'absent' | 'unknown';
  path: string;
  original: unknown;
}

// Only the caller's unique, unpublished temporary may be passed here.
export function cleanupTemporary(files: FileOps, temporary: string, original: unknown): CleanupObservation {
  try {
    files.unlink(temporary);
    return {attempt:'removed',presence:'not-probed',path:temporary,original};
  } catch {
    let presence: CleanupObservation['presence'] = 'unknown';
    try {
      const found = files.presenceOf(temporary);
      presence = !found.known ? 'unknown' : found.there ? 'present' : 'absent';
    } catch {
      // An unavailable probe cannot establish absence.
    }
    return {attempt:'failed',presence,path:temporary,original};
  }
}

export function cleanupFailure(observation: CleanupObservation): NodeJS.ErrnoException {
  const {original, presence, path} = observation;
  const residue = presence === 'present'
    ? ` The cleanup probe found an object at ${path}; verify it before removing it.`
    : presence === 'unknown'
      ? ` The cleanup probe could not determine whether ${path} exists.` : '';
  const reason = original instanceof Error ? original.message : String(original);
  const failure = new Error(reason + residue, {cause:original}) as NodeJS.ErrnoException;
  if (original !== null && typeof original === 'object' && 'code' in original) {
    failure.code = (original as NodeJS.ErrnoException).code;
  }
  return failure;
}
