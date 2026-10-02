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
 * A DATUM-MODE BLOCK IS SHOWN, NOT EDITED.
 *
 * A block `import-code --datum` made -- a library, or one definition in it
 * -- keeps its code as a datum in `body`, and has no `src`. The editor's
 * route opens a block from the core's working read, which projects `src`,
 * and saves by writing `src` back. Measured on the real core (pin
 * fe8c100): such a block opened as an empty buffer; an edit saved through
 * it was answered as saved, came
 * back on the next open, and never reached the definition, the export or
 * `whereis`, which all read `body`.
 *
 * So a datum block is opened as what the store makes of it: the library's
 * file as `export-code --datum` writes it, read-only, at the block's own
 * place in it. Editing a datum block needs a write path from text to
 * `body`, which this extension does not have yet.
 *
 * NOTHING HERE IMPORTS VS CODE, so that what the view holds is decided
 * where a cell can ask.
 */

import { Client } from './client';
import { Block } from './blocks';
import { readProjection } from './markers';
import { BlockMode } from './publication';
import { readExportedCount } from './supply';
import { isSym } from './wire';

/*
 * WHETHER A BLOCK KEEPS ITS CODE AS A DATUM. The store writes `mode` as a
 * symbol; a string is read the same way, as `kind` is in `languageModeOf`.
 */
export function isDatumBlock(block: Block): boolean {
  const mode = block.fields.get('mode');
  return (isSym(mode) && mode.name === 'datum') || mode === 'datum';
}

/*
 * A BLOCK'S MODE AS ITS RECORD SAYS IT: no mode is text (a prose block has
 * none), `text` and `datum` are themselves, and anything else -- a mode this
 * build does not know, or a field with two candidate values -- is null,
 * which a write does not take for text.
 */
export function recordedModeOf(block: Block): BlockMode | null {
  if (!block.fields.has('mode')) {
    return 'text';
  }
  const mode = block.fields.get('mode');
  const name = isSym(mode) ? mode.name : typeof mode === 'string' ? mode : null;
  return name === 'text' ? 'text' : name === 'datum' ? 'datum' : null;
}

/*
 * THE SENTENCE A DATUM BLOCK IS OPENED WITH, said once per open so that a
 * read-only tab is not taken for a fault.
 */
export function datumNotice(id: string): string {
  return (
    `${id} is a datum block: the store keeps its code as a datum, and this editor cannot ` +
    'write one yet, so it is shown read-only as the datum export writes it.'
  );
}

/*
 * THE EDITOR MODE OF A DATUM BLOCK: its `lang`, whatever its kind. A library
 * is not kind `code`, and `languageModeOf` would show it as markdown.
 */
export function datumLanguageOf(block: Block): string {
  const lang = block.fields.get('lang');
  return isSym(lang) ? lang.name : typeof lang === 'string' && lang.length > 0 ? lang : 'plaintext';
}

/*
 * A BLOCK'S LANG AS IT CARRIES IT, a symbol or a string; null when it has none.
 */
export function blockLang(block: Block): string | null {
  const lang = block.fields.get('lang');
  return isSym(lang) ? lang.name : typeof lang === 'string' && lang.length > 0 ? lang : null;
}

/*
 * THE LINE AND CHARACTER OF AN OFFSET IN A TEXT, as an editor counts them:
 * where a datum block's view is opened.
 */
export function positionOf(text: string, offset: number): { line: number; character: number } {
  const before = text.slice(0, Math.max(0, Math.min(offset, text.length)));
  const lines = before.split('\n');
  return { line: lines.length - 1, character: lines[lines.length - 1].length };
}

export type DatumView =
  | { ok: true; file: string; text: string; offset: number }
  | { ok: false; reason: string };

/*
 * THE DISK THE VIEW USES, passed in: a fresh directory for one export,
 * the files in it (relative, as `filesUnder` lists them), their bytes,
 * and the directory's removal. The extension passes fsops' `scratchUnder`.
 */
export interface DatumScratch {
  make(): string;
  files(directory: string): string[];
  read(directory: string, file: string): Uint8Array;
  remove(directory: string): void;
}

/*
 * THE FILE A DATUM BLOCK IS PROJECTED INTO, and where the block starts in
 * it. A library is its file's own block (the header names it); a
 * definition is a block of that file (a marker names it), and the view
 * opens at the first byte of its source -- in characters, as an editor
 * counts. The text is the file exactly as the export wrote it.
 *
 * NEVER: AN EMPTY VIEW FOR A BLOCK THE EXPORT DID NOT PLACE. Not finding it
 * is said, as a refusal of the export is.
 */
export async function datumViewOf(client: Client, id: string, scratch: DatumScratch): Promise<DatumView> {
  const directory = scratch.make();
  try {
    const exported = await client.request('export-code', [directory, '--datum']);
    if (!exported.ok) {
      return { ok: false, reason: `the store would not write its datum export: ${exported.text.trim()}` };
    }
    const written = readExportedCount(exported);
    const found = scratch.files(directory);
    if (written !== found.length) {
      return {
        ok: false,
        reason: `the datum export wrote ${written} file${written === 1 ? '' : 's'} and ${found.length} are there`
      };
    }
    for (const file of found) {
      const bytes = scratch.read(directory, file);
      /*
       * NOTE: THE DATUM FILE IS READ WITH THE TEXT PROJECTION'S MARKER
       * READER, for two answers only: which file holds the block, and
       * where its source starts. Both come from the header and the
       * `@block` lines, which the datum export writes in the same family.
       */
      const reading = readProjection(bytes, file, 'datum');
      if (!reading.ok) {
        continue;
      }
      const projection = reading.projection;
      const library = projection.fileId === id;
      /*
       * NEVER: THE FIRST PIECE NAMED BY THE BLOCK. The marker reader gives
       * the lines above the header -- the export's `#!chezscheme` -- to the
       * first block of the file, so the first definition's first piece
       * starts at byte 0. Its own source is the piece after its marker,
       * which is the last piece that names it.
       */
      const piece = [...projection.pieces].reverse().find((p) => p.kind === 'source' && p.id === id);
      if (!library && piece === undefined) {
        continue;
      }
      let text: string;
      let offset: number;
      try {
        const decoder = new TextDecoder('utf-8', { fatal: true });
        text = decoder.decode(bytes);
        offset = library || piece === undefined ? 0 : decoder.decode(bytes.subarray(0, piece.start)).length;
      } catch {
        return { ok: false, reason: `the datum export of ${file} is not UTF-8` };
      }
      return { ok: true, file, text, offset };
    }
    return { ok: false, reason: `the datum export holds no file for ${id}` };
  } finally {
    scratch.remove(directory);
  }
}
