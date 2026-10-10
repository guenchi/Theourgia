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

# THE DOORBELL: one line into a tmux pane when a block addressed to the
# pane's reader lands in a store.
#
#   doorbell.sh <store> <actor> <tmux-target>
#
# It follows the store's change stream. For every block a frame names as
# added, or as having its `to` field changed, it reads that block AT THE
# FRAME'S CUT and, when the block's `to` is <actor>, types
#
#   mail: block <id> at <cut>
#
# into the pane <tmux-target> (a tmux target: a window name, or
# session:window.pane). The message itself never travels this way; the
# reader reads the block at that cut. A reader busy in a turn learns of mail
# from its hooks instead (mail-hook.sh); this is for one idle at its
# prompt, where no hook runs.
#
# `theourgia` is the command on the PATH. When the stream ends -- the
# daemon stopped -- it subscribes again a second later.

set -u
if [ $# -ne 3 ]; then
  echo "usage: doorbell.sh <store> <actor> <tmux-target>" >&2
  exit 2
fi
store=$1
actor=$2
target=$3

# The frame's (cut ...) clause, as the literal `read --cut` takes. The
# frame's (from-cut ...) does not match: its opening is "(from-cut ".
frame_cut() {
  printf '%s\n' "$1" | sed -n -E 's/.*\(cut (\((\("[^"]*" \. [0-9]+\) ?)*\))\).*/\1/p'
}

# The ids of the blocks a frame adds, and of those whose `to` it changes.
frame_ids() {
  printf '%s\n' "$1" | grep -o -E '\((added "[^"]*"|changed "[^"]*" to)\)' |
    sed -E 's/^\([a-z]+ "([^"]*)".*/\1/'
}

addressed() {
  theourgia read "$1" --cut "$2" --wire --store "$store" < /dev/null 2>/dev/null |
    grep -q -F "(to . \"$actor\")"
}

while :; do
  theourgia subscribe changes 0 --wire --store "$store" < /dev/null |
  while IFS= read -r line; do
    case $line in
      "(changes "*) ;;
      *) continue ;;
    esac
    cut=$(frame_cut "$line")
    [ -n "$cut" ] || continue
    for id in $(frame_ids "$line"); do
      if addressed "$id" "$cut"; then
        tmux send-keys -t "$target" "mail: block $id at $cut" Enter
      fi
    done
  done
  sleep 1
done
