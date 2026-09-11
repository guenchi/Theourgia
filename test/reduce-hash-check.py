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
# and does the whole canonicalisation itself: the sorting rules of
# design 9.2, the datum shape, AND the printer -- how a string is
# quoted, how a symbol is not, how a nested pair collapses. Hashing text
# the product already printed would only have checked that sha256 is
# sha256.
#
# WHAT MADE THE EARLIER VERSION ONLY A PARTIAL ORACLE: it received each
# value already rendered by the product's own writer and pasted it in.
# A defect in that writer therefore appeared identically on both sides
# and the two agreed. Values now arrive as typed terms and are rendered
# here, so the printer is under test rather than borrowed.
#
# TRANSPORT: every text is hex-encoded UTF-8. The first version split on
# tabs and newlines, which a value is entitled to contain -- a title
# with a tab crashed the script and one with a newline silently produced
# a different record. A separator that can occur in the data is not a
# separator.
#
# A TERM is one or more space-separated tokens in prefix order:
#   str:<hex>  sym:<hex>  int:<decimal>  rat:<num>/<den>  nil:
#   pair: <term> <term>
# Lists are pairs ending in nil, so there is no list token.
#
# Input, one record per line. The variable-length term is always LAST,
# so nothing after it can be mistaken for part of it:
#   B <id-term> <deleted:0|1>
#   F <id-term> <writer-term> <seq> <name-term> <value-term>
#   P <id-term> <writer-term> <seq> <place-term>
#   E <id-term> <rel-term> <to-term>
import sys, hashlib

def unhex(h):
    return bytes.fromhex(h).decode("utf-8")

def read_term(toks):
    t = toks.pop(0)
    kind, _, rest = t.partition(":")
    if kind == "str":  return ("str", unhex(rest))
    if kind == "sym":  return ("sym", unhex(rest))
    if kind == "int":  return ("int", int(rest))
    if kind == "rat":
        n, d = rest.split("/")
        return ("rat", int(n), int(d))
    if kind == "nil":  return ("nil",)
    if kind == "pair":
        a = read_term(toks)
        b = read_term(toks)
        return ("pair", a, b)
    raise SystemExit("unknown term token: %r" % t)

# THE PRINTER, REDERIVED. The codec escapes only the quote and the
# backslash inside a string: a tab and a newline go through as
# themselves, which is what makes the awkward-value case meaningful.
# A pair whose cdr is a pair collapses -- (a . (b . c)) prints as
# (a b . c) -- and a proper list ends without a dot.
def render(v):
    k = v[0]
    if k == "str":
        return '"' + v[1].replace("\\", "\\\\").replace('"', '\\"') + '"'
    if k == "sym":
        return v[1]
    if k == "int":
        return str(v[1])
    if k == "rat":
        return "%d/%d" % (v[1], v[2])
    if k == "nil":
        return "()"
    if k == "pair":
        parts = []
        cur = v
        while cur[0] == "pair":
            parts.append(render(cur[1]))
            cur = cur[2]
        if cur[0] == "nil":
            return "(" + " ".join(parts) + ")"
        return "(" + " ".join(parts) + " . " + render(cur) + ")"
    raise SystemExit("unrenderable term: %r" % (v,))

def text_of(v):
    # The sort keys are the VALUES, not their rendering: sorting on the
    # printed form would order "\"b\"" against "a" by the quote.
    if v[0] in ("str", "sym"):
        return v[1]
    raise SystemExit("expected a text term, got %r" % (v,))

blocks = {}

def block(bid):
    return blocks.setdefault(bid, {"deleted": False, "fields": {}, "pos": [], "edges": set()})

for line in sys.stdin:
    line = line.rstrip("\n")
    if not line:
        continue
    toks = line.split(" ")
    kind = toks.pop(0)
    if kind == "B":
        bid = text_of(read_term(toks))
        block(bid)["deleted"] = (toks.pop(0) == "1")
    elif kind == "F":
        bid = text_of(read_term(toks))
        writer = text_of(read_term(toks))
        seq = int(toks.pop(0))
        name = text_of(read_term(toks))
        value = read_term(toks)
        block(bid)["fields"].setdefault(name, []).append((value, writer, seq))
    elif kind == "P":
        bid = text_of(read_term(toks))
        writer = text_of(read_term(toks))
        seq = int(toks.pop(0))
        place = read_term(toks)
        block(bid)["pos"].append((place, writer, seq))
    elif kind == "E":
        bid = text_of(read_term(toks))
        rel = read_term(toks)
        to = read_term(toks)
        block(bid)["edges"].add((text_of(rel), text_of(to)))
    else:
        raise SystemExit("unknown record kind: %r" % kind)
    if toks:
        raise SystemExit("trailing tokens on a %s line: %r" % (kind, toks))

# A candidate is (value . (writer . seq)); the collapsing rule prints it
# as (<value> "<writer>" . <seq>).
def candidate(value, w, s):
    return render(("pair", value, ("pair", ("str", w), ("int", s))))

out = []
for bid in sorted(blocks):
    b = blocks[bid]
    fs = []
    for name in sorted(b["fields"]):
        cands = sorted(b["fields"][name], key=lambda c: (c[1], c[2]))
        fs.append("(%s (%s))" % (name, " ".join(candidate(v, w, s) for (v, w, s) in cands)))
    ps = sorted(b["pos"], key=lambda c: (c[1], c[2]))
    pos = " ".join(candidate(p, w, s) for (p, w, s) in ps)
    es = " ".join("(%s . %s)" % (rel, render(("str", to))) for (rel, to) in sorted(b["edges"]))
    out.append("(block %s (fields (%s)) (position (%s)) (deleted %s) (edges (%s)))"
               % (render(("str", bid)), " ".join(fs), pos,
                  "#t" if b["deleted"] else "#f", es))

text = "(" + " ".join(out) + ")"
if "--text" in sys.argv:
    print(text)
else:
    print(hashlib.sha256(text.encode()).hexdigest())
