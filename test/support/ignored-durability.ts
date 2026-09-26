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
 * FOR A CELL THAT IS NOT ABOUT DURABILITY. (queue item 22, ruled Q3) A
 * Saver and a settler must be given somewhere to put a queue write's
 * durability warning, and the compiler checks that each is. A cell about
 * something else hands them this, which keeps nothing -- by name, so that
 * a reader sees the choice. A cell about durability builds a
 * `DurabilitySink` and reads it.
 */
export const IGNORED_DURABILITY = {
  durability: (_file: string, _text: string): void => undefined
};
