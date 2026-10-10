#!/bin/sh
# Copyright 2018 - 2026 guenchi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# THE OLD NAME OF contrib/mail-hook.sh, kept so a hooks file that names it
# still runs: the same handler, given the same arguments.

# Beside the script itself, not beside a link to it: a link is followed to
# the file it names.
self=$0
while [ -L "$self" ]; do
  link=$(readlink "$self")
  case $link in
    /*) self=$link ;;
    *) self=$(dirname "$self")/$link ;;
  esac
done
exec sh "$(dirname "$self")/mail-hook.sh" "$@"
