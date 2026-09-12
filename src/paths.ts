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
 * A NAME THAT IS BUILT INTO A PATH IS CHECKED IN ONE PLACE. (§13, r5-3)
 *
 * ⚠️ THE CHECK WAS WRITTEN FOR ONE CALLER AND THEN A SECOND APPEARED.
 *
 * `Sessions.outboxPathFor` composes a store's name into a path under a
 * session directory, and a name of `../live/a` addressed a LIVE
 * window's queue -- found in review, with a reproduction. The
 * tombstones §13 adds compose a store name AND a request id into a path
 * under the storage root, and the request id comes off another
 * session's queue file, which is not this program's to trust.
 *
 * Two copies of a rule about untrusted input is one copy that will be
 * repaired and one that will not. This is the rule, once.
 *
 * ⚠️ WHAT IT IS, EXACTLY: a name is one path component under BOTH
 * platforms' rules, and holds no control character. It is NOT a test of
 * filesystem validity -- a 256-byte name, `a?b`, or `CON` all pass here
 * and some filesystems will refuse them -- and it rejects a tab, which
 * POSIX allows. Production names are digests and uuids, so neither gap
 * is reachable from this extension; the point of writing it down is
 * that the next caller should not read this as "the filesystem will
 * take it".
 */

import * as path from 'path';

export interface NameKind {
  /*
   * How the name is referred to in the message, e.g. `a store name`.
   * The sentence is part of the check: "invalid path component" says
   * nothing about WHICH of the two names in a tombstone path was wrong.
   */
  noun: string;
  /*
   * Where the single directory sits, e.g. `the session`. It appears as
   * "a single directory inside <inside>".
   */
  inside: string;
}

export function checkOneComponent(name: string, kind: NameKind): void {
  /*
   * ⚠️ AND NO CONTROL CHARACTER. A NUL cannot occur in a filename on any
   * platform this runs on, and one in a store name got as far as the
   * read, where node refuses it -- and the takeover then counted that as
   * one more queue it could not inspect, reporting a path that was never
   * there. Found in review.
   */
  if (/[\u0000-\u001f]/.test(name)) {
    throw new Error(`${kind.noun} may not contain a control character; got ${JSON.stringify(name)}`);
  }
  /*
   * ⚠️ THE TEST IS WHAT THE NAME MUST BE, not a list of what it must not
   * contain: `basename` of a single path component is that component,
   * and of anything carrying a separator it is not. A blocklist is a
   * guess at the spellings somebody will try; this is the property.
   *
   * ⚠️ ON EVERY PLATFORM'S RULES, NOT ONLY THIS ONE'S. `path.basename`
   * on POSIX does not treat a backslash as a separator, so a name
   * carrying one passes there and is a path on Windows -- and these
   * names travel: the directory is written by whichever window made it
   * and read by whichever window recovers it. A name that is one
   * component here and two somewhere else is not a name.
   */
  const oneComponent = path.posix.basename(name) === name && path.win32.basename(name) === name;
  if (!oneComponent || name === '.' || name === '..') {
    throw new Error(
      `${kind.noun} must be the name of a single directory inside ${kind.inside}, not a path; ` +
        `got ${JSON.stringify(name)}`
    );
  }
}

/*
 * Compose a path from a root and names that were each checked. The
 * checking and the composing are one call, so that no caller can do the
 * second without the first.
 */
export function under(root: string, parts: Array<{ name: string; kind: NameKind }>): string {
  for (const part of parts) {
    checkOneComponent(part.name, part.kind);
  }
  return path.join(root, ...parts.map((p) => p.name));
}
