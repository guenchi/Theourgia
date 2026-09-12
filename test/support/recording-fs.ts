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
 * What this process did to each path.
 *
 * SEVERAL CELLS ASSERT AN ABSENCE -- that no published path was ever
 * unlinked or renamed -- and an absence is the one reading a broken
 * instrument gives for free. So this counts per PATH rather than in
 * total (the outbox legitimately unlinks its own temporary file, and a
 * total would either forbid that or excuse everything), and its own
 * cell proves it counts at all before any absence is believed.
 *
 * IT DELEGATES RATHER THAN SIMULATES. A stand-in file system would let
 * a cell pass against bytes no operating system ever saw; this performs
 * the real operation and records that it did.
 */

import * as path from 'path';
import { FileOps, nodeFileOps } from '../../src/fsops';

export type Operation = 'readText' | 'readBytes' | 'writeText' | 'makeDirectory' | 'rename' | 'unlink' | 'writeDurably' | 'syncDirectory' | 'exists' | 'link' | 'list' | 'isDirectory';

export interface Entry {
  op: Operation;
  file: string;
  to?: string;
}

export class RecordingFs implements FileOps {
  public readonly entries: Entry[] = [];
  private readonly inner: FileOps;

  constructor(inner: FileOps = nodeFileOps) {
    this.inner = inner;
  }

  /*
   * PATHS ARE COMPARED RESOLVED. A cell that asked about
   * `/tmp/x/../x/a.md` and a product that wrote `/tmp/x/a.md` are
   * talking about one file, and a count that said zero because the
   * spelling differed would be the most reassuring possible answer.
   */
  public countOf(op: Operation, file: string): number {
    const want = path.resolve(file);
    return this.entries.filter((e) => e.op === op && path.resolve(e.file) === want).length;
  }

  public touched(op: Operation): string[] {
    return this.entries.filter((e) => e.op === op).map((e) => path.resolve(e.file));
  }

  private record(op: Operation, file: string, to?: string): void {
    this.entries.push(to === undefined ? { op, file } : { op, file, to });
  }

  public readText(file: string): string {
    this.record('readText', file);
    return this.inner.readText(file);
  }

  public readBytes(file: string): Buffer {
    this.record('readBytes', file);
    return this.inner.readBytes(file);
  }

  public writeText(file: string, text: string): void {
    this.record('writeText', file);
    this.inner.writeText(file, text);
  }

  public makeDirectory(directory: string): void {
    this.record('makeDirectory', directory);
    this.inner.makeDirectory(directory);
  }

  /*
   * A RENAME IS RECORDED AGAINST BOTH PATHS. "This path was never
   * renamed" has to be false for the source and for a destination that
   * something was moved onto -- a count kept only against the source
   * would let a published file be overwritten by a rename and still
   * report zero.
   */
  public rename(from: string, to: string): void {
    this.record('rename', from, to);
    this.record('rename', to, from);
    this.inner.rename(from, to);
  }

  public unlink(file: string): void {
    this.record('unlink', file);
    this.inner.unlink(file);
  }

  public writeDurably(file: string, text: string): void {
    this.record('writeDurably', file);
    this.inner.writeDurably(file, text);
  }

  public syncDirectory(directory: string): void {
    this.record('syncDirectory', directory);
    this.inner.syncDirectory(directory);
  }

  public exists(file: string): boolean {
    this.record('exists', file);
    return this.inner.exists(file);
  }

  public link(existing: string, fresh: string): void {
    this.record('link', fresh, existing);
    this.inner.link(existing, fresh);
  }

  public readDirectory(
    directory: string
  ): { read: true; names: string[] } | { read: false; because: 'absent' | 'unreadable' } {
    this.record('list', directory);
    return this.inner.readDirectory(directory);
  }

  public list(directory: string): string[] {
    this.record('list', directory);
    return this.inner.list(directory);
  }

  public isDirectory(file: string): boolean {
    this.record('isDirectory', file);
    return this.inner.isDirectory(file);
  }
}
