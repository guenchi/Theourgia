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

# A CODEX HOOK'S HANDLER: the unread mail of THEOURGIA_ACTOR in the store
# THEOURGIA_STORE names, as the hook's answer, or nothing when there is none.
#
#   codex-mail.sh <hook-event-name>
#
# <hook-event-name> is the event the hook is registered on (PostToolUse or
# UserPromptSubmit); the answer names it, as Codex requires. The mail is
# the mailbox goal of the README, asked with the command line, so the hook
# does not depend on an MCP shell being up. `theourgia` is the command on
# the PATH.

set -u
event=${1:?usage: codex-mail.sh <hook-event-name>}
actor=${THEOURGIA_ACTOR:?THEOURGIA_ACTOR names the reader}
goal="(and (field ?m \"to\" \"$actor\") (field ?m \"status\" \"unread\"))"

ids=$(theourgia query "$goal" --wire < /dev/null 2>/dev/null |
  grep -o -E '\(row "[^"]*"\)' | sed -E 's/^\(row "([^"]*)"\)$/\1/')
[ -n "$ids" ] || exit 0
blocks=$(printf '%s ' $ids | sed 's/ $//')

# A block id is letters, digits and dots, so it needs no escaping in JSON;
# the actor is the operator's own setting.
printf '{"hookSpecificOutput":{"hookEventName":"%s","additionalContext":"theourgia: unread mail for %s: %s. Read each block in the store (read <id>), act on it, then set its status to read."}}\n' \
  "$event" "$actor" "$blocks"
