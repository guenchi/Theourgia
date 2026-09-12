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
 * The build output is removed before every compile.
 *
 * THE COMPILER ONLY ADDS. A source file that is renamed or deleted
 * leaves its old output behind, and the suite runner globs the output
 * directory -- so the previous version of a renamed test goes on running
 * beside the new one, against whatever the tree now holds. That happened
 * here: a suite renamed from vendor-sexpr to dependency-sexpr ran twice,
 * and the failures belonged to a file that no longer exists.
 *
 * It is cheap enough to do every time, and a build that is only
 * sometimes clean is the one that misleads.
 */

const fs = require('fs');
const path = require('path');

const out = path.join(__dirname, '..', 'out');
fs.rmSync(out, { recursive: true, force: true });
console.log(`clean: removed ${out}`);
