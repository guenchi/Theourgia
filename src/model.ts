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
 * What the tree shows, without a tree.
 *
 * NOTHING HERE IMPORTS VS CODE. A view is a rendering of this, and a
 * rendering is the part that cannot be run in a plain process -- so the
 * questions worth pinning (how many requests an expansion costs, whose
 * children a node has, what a row's title is) are asked of this file and
 * answered without an extension host.
 *
 * THE TOP LEVEL COMES FROM `outline --depth 1` AND A SUBTREE COMES FROM
 * `read <id> --recursive`. Not from one unlimited outline: a depth limit
 * stops the core's walk rather than filtering its output, which the core
 * says is the difference between an outline and a dump, and a client
 * that asked for everything at once would pay that on every refresh.
 *
 * A SUBTREE ANSWER IS FILTERED TO THE DIRECT CHILDREN AND KEPT IN THE
 * ORDER IT ARRIVED. The core answers in document order; sorting here
 * would be a second opinion about sibling order, and the core's own
 * `ord` is the first.
 */

import { Client } from './client';
import { Block, readBlock, titleOf, hasFieldConflict } from './blocks';
import { OutlineRow, parseOutline } from './outline';

export interface Node {
  id: string;
  title: string;
  marked: boolean;
  orphan: boolean;
  /*
   * WHETHER A NODE HAS CHILDREN IS NOT KNOWN UNTIL IT IS OPENED. The
   * outline was cut off at depth one and a block's own record does not
   * carry a child count, so every node is offered as expandable and an
   * expansion that finds nothing collapses again. Claiming to know would
   * mean hiding a subtree whenever the guess was wrong.
   */
  mayHaveChildren: boolean;
}

function nodeFromRow(row: OutlineRow): Node {
  return {
    id: row.id,
    title: row.title,
    marked: row.mark !== null,
    orphan: row.orphan,
    mayHaveChildren: true
  };
}

function nodeFromBlock(block: Block): Node {
  return {
    id: block.id,
    title: titleOf(block),
    marked: hasFieldConflict(block),
    orphan: false,
    mayHaveChildren: true
  };
}

export class StoreModel {
  private readonly client: Client;

  constructor(client: Client) {
    this.client = client;
  }

  public async roots(): Promise<Node[]> {
    const answer = await this.client.request('outline', ['--depth', '1']);
    return parseOutline(answer.text).map(nodeFromRow);
  }

  /*
   * ONE REQUEST PER EXPANSION. The answer holds the whole subtree and
   * this keeps only the direct children; asking again for each
   * grandchild when it is opened costs a request that already has its
   * answer, but caching that answer would mean showing a subtree as it
   * was before someone else wrote to it.
   */
  public async childrenOf(id: string): Promise<Node[]> {
    const answer = await this.client.request('read', [id, '--recursive']);
    const out: Node[] = [];
    for (const item of answer.answers) {
      const block = readBlock(item);
      if (block === null || block.id === id || block.parent !== id) {
        continue;
      }
      out.push(nodeFromBlock(block));
    }
    return out;
  }

  public async blockOf(id: string): Promise<Block | null> {
    const answer = await this.client.request('read', [id]);
    if (!answer.ok || answer.answers.length === 0) {
      return null;
    }
    const datum = answer.answers[0];
    /*
     * `read <id>` answers `(ok (<block>))`, so the block is inside the
     * answer rather than being it. Reading the answer itself as a block
     * would find no `id` entry and report the block as missing.
     */
    if (Array.isArray(datum) && datum.length >= 2) {
      return readBlock(datum[1]);
    }
    return null;
  }

  public async conflictCount(): Promise<number> {
    const answer = await this.client.request('conflicts', []);
    return answer.ok ? answer.answers.length : 0;
  }
}
