#!/usr/bin/env python3
# Copyright 2026 guenchi
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
#
# A SECOND IMPLEMENTATION OF THE TOKEN, in another language on purpose.
#
# It reads the reduction's contents in an UNORDERED, non-canonical dump
# and does the canonicalisation itself: the sorting rules of design 9.2,
# the datum shape, the printer's spacing, then sha256. Hashing the text
# the product already produced would only have checked that sha256 is
# sha256; the part worth cross-checking is whether two people reading
# section 9.2 arrive at the same bytes.
#
# TRANSPORT: every field is hex-encoded UTF-8. The first version split
# on tabs and newlines, which a value is entitled to contain -- a title
# with a tab in it crashed the script, and one with a newline silently
# produced a different record. A separator that can occur in the data is
# not a separator.
#
# Input (one record per line, space-separated, fields hex-encoded):
#   B <id> <deleted:0|1>
#   F <id> <field> <value-text> <writer> <seq>
#   P <id> <parent-text> <ord-text> <writer> <seq>
#   E <id> <rel> <to>
import sys, hashlib

def unhex(h):
    return bytes.fromhex(h).decode("utf-8")

blocks = {}

def block(bid):
    return blocks.setdefault(bid, {"deleted": False, "fields": {}, "pos": [], "edges": set()})

for line in sys.stdin:
    line = line.rstrip("\n")
    if not line:
        continue
    raw = line.split(" ")
    # Which fields are hex-encoded text and which are plain integers is
    # a property of the record type, not something to guess: decoding a
    # sequence number as hex is how the first version failed.
    hexed = {"B": [1], "F": [1, 2, 3, 4], "P": [1, 2, 3, 4], "E": [1, 2, 3]}
    parts = [unhex(x) if i in hexed.get(raw[0], []) else x
             for i, x in enumerate(raw)]
    kind = parts[0]
    if kind == "B":
        b = block(parts[1]); b["deleted"] = (parts[2] == "1")
    elif kind == "F":
        b = block(parts[1])
        b["fields"].setdefault(parts[2], []).append((parts[3], parts[4], int(parts[5])))
    elif kind == "P":
        b = block(parts[1])
        b["pos"].append((parts[2], parts[3], parts[4], int(parts[5])))
    elif kind == "E":
        b = block(parts[1]); b["edges"].add((parts[2], parts[3]))

# THE WRITER IS A STRING, so it prints with its quotes; and the printer
# collapses (x . (y . z)) into (x y . z), so a candidate comes out as
# (<value> <writer> . <seq>) rather than (<value> . ("w" . 1)). Both are
# facts about the canonical form that this implementation has to
# reproduce rather than approximate -- getting them wrong is what a
# second implementation is for.
def candidate(value_text, w, s):
    return '(%s "%s" . %d)' % (value_text, w, s)

out = []
for bid in sorted(blocks):                      # blocks by id
    b = blocks[bid]
    fs = []
    for name in sorted(b["fields"]):            # field names by string<?
        cands = sorted(b["fields"][name], key=lambda c: (c[1], c[2]))
        fs.append("(%s (%s))" % (name, " ".join(
            candidate(v, w, s) for (v, w, s) in cands)))
    ps = sorted(b["pos"], key=lambda c: (c[2], c[3]))
    pos = " ".join(candidate("(%s . %s)" % (p, o), w, s) for (p, o, w, s) in ps)
    es = " ".join("(%s . %s)" % (rel, to) for (rel, to) in sorted(b["edges"]))
    out.append("(block %s (fields (%s)) (position (%s)) (deleted %s) (edges (%s)))"
               % (bid, " ".join(fs), pos, "#t" if b["deleted"] else "#f", es))

text = "(" + " ".join(out) + ")"
if "--text" in sys.argv:
    print(text)
else:
    print(hashlib.sha256(text.encode()).hexdigest())
