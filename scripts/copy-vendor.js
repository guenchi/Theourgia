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
 * The compiler emits JavaScript and copies nothing, so the golden
 * tables and the fake core -- none of which is TypeScript -- would be
 * missing from the build output and every run would fail at load. This
 * copies them and reports what it copied, because a copy step that
 * silently copies nothing is how a stale build starts reading green.
 *
 * THE READER ITSELF IS NO LONGER COPIED. It arrives as the published
 * `goeteia` package and is resolved from node_modules at run time.
 */

const fs = require('fs');
const path = require('path');

const root = path.join(__dirname, '..');
const files = [
  ['test/fixtures/goeteia/sexpr-vectors.json', 'out/test/fixtures/goeteia/sexpr-vectors.json'],
  ['test/fixtures/goeteia/sexpr-escape-vectors.json', 'out/test/fixtures/goeteia/sexpr-escape-vectors.json'],
  ['test/fake-core.js', 'out/test/fake-core.js'],
  ['test/support/host-child.js', 'out/test/support/host-child.js']
];

let copied = 0;
for (const [from, to] of files) {
  const source = path.join(root, from);
  if (!fs.existsSync(source)) {
    throw new Error(`copy-vendor: ${from} does not exist`);
  }
  const target = path.join(root, to);
  fs.mkdirSync(path.dirname(target), { recursive: true });
  fs.copyFileSync(source, target);
  copied += 1;
}
if (copied !== files.length) {
  throw new Error(`copy-vendor: copied ${copied} of ${files.length}`);
}
process.stdout.write(`copy-vendor: ${copied} files\n`);
