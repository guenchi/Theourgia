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
 * What the code that ACTS on the baseline decision does with it.
 *
 * THE RULE HAD CELLS AND ITS CALLER HAD NONE. open.test.ts drives the
 * registry directly and would stay green with the caller ignoring every
 * answer it gives -- which is not a hypothetical: when `register` stopped
 * returning a boolean, the caller's `if (!admission)` went on compiling,
 * stopped being true, and let a losing open write the file, with the
 * whole editor-hosted suite green.
 *
 * THE EDITOR IS HANDED IN. Two overlapping opens cannot be arranged
 * through VS Code's own API -- its waits are not ours to widen -- so the
 * three editor operations arrive as functions, and a cell can hold the
 * first open inside `openDocument` while the second one runs to
 * completion.
 */

import * as assert from 'assert';
import { OpenBuffers } from '../../src/open';
import { EditorSurface, placeReading } from '../../src/placing';

interface Recorder {
  surface: EditorSurface<string>;
  opened: string[];
  revealed: string[];
  release: () => void;
  reached: Promise<void>;
}

/*
 * An editor whose `openDocument` can be held open, so that a cell can
 * decide what happens while one placement is inside it.
 */
function heldEditor(): Recorder {
  const opened: string[] = [];
  const revealed: string[] = [];
  let release = (): void => undefined;
  let arrive = (): void => undefined;
  const held = new Promise<void>((resolve) => {
    release = resolve;
  });
  const reached = new Promise<void>((resolve) => {
    arrive = resolve;
  });
  return {
    opened,
    revealed,
    release: () => release(),
    reached,
    surface: {
      openDocument: async (file: string) => {
        opened.push(file);
        arrive();
        await held;
        return file;
      },
      setLanguage: async () => undefined,
      reveal: async (document: string) => {
        revealed.push(document);
        return undefined;
      }
    }
  };
}

function plainEditor(): EditorSurface<string> {
  return {
    openDocument: async (file: string) => file,
    setLanguage: async () => undefined,
    reveal: async () => undefined
  };
}

describe('the reading that loses does not reach the file or the screen', () => {
  it('writes nothing and opens nothing when a newer reading already won', async () => {
    const buffers = new OpenBuffers<string>();
    const older = buffers.claim();
    const newer = buffers.claim();
    buffers.register('/f', 'newer', newer);

    const editor = heldEditor();
    let wrote = 0;
    const placement = await placeReading(
      buffers,
      '/f',
      'older',
      older,
      () => {
        wrote += 1;
      },
      editor.surface
    );

    assert.deepStrictEqual(placement, { placed: false, by: 'read' });
    assert.strictEqual(wrote, 0, 'the losing reading wrote its text to the file');
    assert.deepStrictEqual(editor.opened, [], 'the losing reading was put in front of the user');
    assert.strictEqual(buffers.get('/f'), 'newer', 'the losing reading became the baseline');
  });

  it('says a save was what outranked it, which is the case nothing else reports', async () => {
    const buffers = new OpenBuffers<string>();
    const started = buffers.claim();
    buffers.register('/f', 'read', started);
    buffers.confirmed('/f', 'saved');

    let wrote = 0;
    const placement = await placeReading(
      buffers,
      '/f',
      'read',
      started,
      () => {
        wrote += 1;
      },
      plainEditor()
    );

    assert.deepStrictEqual(placement, { placed: false, by: 'save' });
    assert.strictEqual(wrote, 0);
  });

  it('writes and shows the reading that wins', async () => {
    const buffers = new OpenBuffers<string>();
    let wrote = 0;
    const placement = await placeReading(
      buffers,
      '/f',
      'mine',
      buffers.claim(),
      () => {
        wrote += 1;
      },
      plainEditor()
    );
    assert.deepStrictEqual(placement, { placed: true });
    assert.strictEqual(wrote, 1, 'the winning reading did not write the file');
    assert.strictEqual(buffers.get('/f'), 'mine');
  });

  /*
   * THE INTERLEAVING ITSELF, which is what the editor being a parameter
   * buys. The first placement is held inside `openDocument` -- it has
   * already written and registered -- and the second runs to completion
   * while it waits. The first must not then be revived: its ticket is
   * lower, and what it would restore is an older reading.
   */
  it('leaves the baseline with the newer reading when an older open was held mid-flight', async () => {
    const buffers = new OpenBuffers<string>();
    const first = buffers.claim();
    const second = buffers.claim();
    const editor = heldEditor();
    const writes: string[] = [];

    const held = placeReading(buffers, '/f', 'first', first, () => writes.push('first'), editor.surface);
    await editor.reached;
    assert.strictEqual(buffers.get('/f'), 'first', 'the first reading never became the baseline');

    const later = await placeReading(
      buffers,
      '/f',
      'second',
      second,
      () => writes.push('second'),
      plainEditor()
    );
    assert.deepStrictEqual(later, { placed: true });

    editor.release();
    assert.deepStrictEqual(await held, { placed: true });

    assert.strictEqual(
      buffers.get('/f'),
      'second',
      'an open that was still inside the editor put its older reading back'
    );
    assert.deepStrictEqual(writes, ['first', 'second'], 'the writes did not happen in that order');
  });
});
