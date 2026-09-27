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

# THE EXPECTED STORE FOR a1-import.sc, COMPUTED OUTSIDE THE PRODUCT.
#
# usage: expected.py <corpus-dir> <out-dir>
# writes <out-dir>/after-import.sexp and <out-dir>/after-edits.sexp.
#
# NEVER: WRITTEN FROM THE MARKDOWN RULES, NOT FROM md.sc. A heading is
# CommonMark's ATX heading (0-3 spaces of indent, 1-6 `#`, then a space or
# the end of the line; an optional closing run of `#` after a space is
# not part of the title). A fence is CommonMark's fenced code block (0-3
# spaces, three or more backticks or tildes; closed by the same character,
# at least as many, and nothing but spaces after). Front matter is the
# YAML convention: the file's first line is `---`, and it ends at the
# next line that is `---` or `...`. A line with four or more spaces of
# indent is never a heading. Where the product disagrees, the fixture's
# row is red, and the disagreement is triaged, not re-baselined.
#
# KEY: BLOCKS ARE NAMED BY PLACE, NOT BY ID. `<path>#<n>`: n = 0 is the
# document, n = 1.. its sections in file order. An id is `<writer>.<seq>`,
# and predicting a sequence number would be modelling the product's
# record numbering, which is the thing under test. The fixture maps ids to
# places from the product's own paths and section order.
#
# NOTE: A `[[...]]` KEY RESOLVES ONLY TO A BLOCK ID (store.sc,
# md-referrers). No text in the corpus names an id, so after the import
# there are no derived edges. The first event writes `[[@<place>]]`, which
# the fixture replaces with that place's id before sending it; the same
# lexeme inside a fence must not resolve.

import os
import signal
import sys
import time


def lines_of(text):
    out = []
    i = 0
    while i < len(text):
        j = text.find("\n", i)
        if j < 0:
            out.append(text[i:])
            break
        out.append(text[i:j + 1])
        i = j + 1
    return out


def indent(line):
    n = 0
    for c in line:
        if c == " ":
            n += 1
        else:
            break
    return n


def fence_of(line):
    body = line.rstrip("\n")
    k = indent(body)
    if k > 3:
        return None
    rest = body[k:]
    for ch in ("`", "~"):
        n = 0
        while n < len(rest) and rest[n] == ch:
            n += 1
        if n >= 3:
            if ch == "`" and "`" in rest[n:]:
                return None
            return (ch, n, rest[n:])
    return None


def closes(line, opener):
    f = fence_of(line)
    return f is not None and f[0] == opener[0] and f[1] >= opener[1] and f[2].strip(" ") == ""


def heading_of(line):
    body = line.rstrip("\n")
    k = indent(body)
    if k > 3:
        return None
    rest = body[k:]
    n = 0
    while n < len(rest) and rest[n] == "#":
        n += 1
    if n < 1 or n > 6:
        return None
    if n < len(rest) and rest[n] not in (" ", "\t"):
        return None
    content = rest[n:].strip(" \t")
    # The closing sequence: a run of `#` that is the whole content, or
    # that follows a space, with only spaces after it.
    stripped = content.rstrip("#")
    if stripped == "":
        content = ""
    elif stripped != content and stripped.endswith((" ", "\t")):
        content = stripped.rstrip(" \t")
    return (n, content)


def front_end(ls):
    if not ls or ls[0].rstrip("\n") != "---":
        return 0
    for i in range(1, len(ls)):
        if ls[i].rstrip("\n") in ("---", "..."):
            return i + 1
    return 0


def split(text):
    ls = lines_of(text)
    f = front_end(ls)
    heads = []
    fence = None
    for i in range(f, len(ls)):
        line = ls[i]
        if fence is not None:
            if closes(line, fence):
                fence = None
            continue
        op = fence_of(line)
        if op is not None:
            fence = op
            continue
        h = heading_of(line)
        if h is not None:
            heads.append((i, h))
    front = "".join(ls[:f])
    first = heads[0][0] if heads else len(ls)
    src = "".join(ls[f:first])
    sections = []
    for j, (i, (level, title)) in enumerate(heads):
        end = heads[j + 1][0] if j + 1 < len(heads) else len(ls)
        sections.append({"level": level, "title": title,
                         "heading-src": ls[i], "src": "".join(ls[i + 1:end])})
    return front, src, sections


def md_files(d, prefix=""):
    out = []
    for name in sorted(os.listdir(d)):
        full = os.path.join(d, name)
        rel = name if prefix == "" else prefix + "/" + name
        if os.path.isdir(full):
            out.extend(md_files(full, rel))
        elif len(name) > 3 and name.endswith(".md"):
            out.append(rel)
    return out


# A HARD BOUND PER FILE: a file that takes longer than this stops
# the run and names its INDEX, never its path -- the corpus may be private,
# and the run prints counts only.
PER_FILE_SECONDS = 60


class FileTooSlow(Exception):
    pass


def _too_slow(signum, frame):
    raise FileTooSlow()


def import_corpus(corpus, timings=None):
    blocks = {}
    order = {"root": []}
    signal.signal(signal.SIGALRM, _too_slow)
    for index, rel in enumerate(md_files(corpus)):
        t0 = time.monotonic()
        signal.alarm(PER_FILE_SECONDS)
        try:
            with open(os.path.join(corpus, rel), encoding="utf-8") as fh:
                text = fh.read()
            front, src, sections = split(text)
        except FileTooSlow:
            print("file %d took over %d s: stopped" % (index, PER_FILE_SECONDS))
            sys.exit(3)
        finally:
            signal.alarm(0)
        if timings is not None:
            timings.append(time.monotonic() - t0)
        doc = rel + "#0"
        blocks[doc] = {"kind": "doc", "path": rel, "front": front, "src": src, "parent": "root"}
        order["root"].append(doc)
        order[doc] = []
        stack = []
        for n, s in enumerate(sections, 1):
            key = "%s#%d" % (rel, n)
            while stack and stack[-1][0] >= s["level"]:
                stack.pop()
            parent = stack[-1][1] if stack else doc
            blocks[key] = {"kind": "section", "level": s["level"], "title": s["title"],
                           "heading-src": s["heading-src"], "src": s["src"], "parent": parent}
            order[parent].append(key)
            order[key] = []
            stack.append((s["level"], key))
    return {"blocks": blocks, "order": order, "links": []}


# THE FIVE EVENTS, BY PLACE. `set summary` has no field
# to set (no `summary` in reduce.sc or rpc.sc); `keywords` stands in.
EDIT_TEXT = "Edited body links [[@beta.md#1]].\n\n```\n[[@beta.md#1]]\n```\n"
EVENTS = [
    ("set", "alpha.md#2", "src", EDIT_TEXT),
    ("set", "beta.md#0", "keywords", "alpha, beta"),
    ("move", "sub/zeta.md#3", "alpha.md#1", "alpha.md#4"),
    ("link", "gamma.md#0", "explains", "alpha.md#1"),
    ("set", "raw.md#2", "title", "Raw end renamed"),
]


# NOT EVERY CORPUS HAS THESE PLACES. EVENTS name places in the
# in-tree corpus. On another corpus the five places are CHOSEN from the
# corpus itself by the rule in choose_events, so the edit half of the fixture is read
# there too; only when the corpus cannot supply them is an event skipped. The
# events actually used -- verb, places, field, text -- are written to
# events.sexp, and the fixture sends exactly those.
def choose_events(store):
    """The rule, deterministic: files in sorted order, sections in file order.
    P = the first section that has a child section; A = P's last child.
    M = the last section in file order that is a leaf, outside P's subtree,
    not an ancestor of P, and not A (it moves under P, after A).
    S = the first remaining section, K = the next one (S's text links K).
    T = the next remaining section (its title is set).
    D = the first document (its keywords are set); G = the second document,
    or D when there is one (G explains P). Answers None when the corpus has
    too few sections for the rule."""
    blocks, order = store["blocks"], store["order"]
    docs = order["root"]
    sections = []
    for d in docs:
        path = blocks[d]["path"]
        n = 1
        while "%s#%d" % (path, n) in blocks:
            sections.append("%s#%d" % (path, n))
            n += 1

    def subtree(k):
        out = [k]
        for c in order.get(k, []):
            out.extend(subtree(c))
        return out

    def ancestors(k):
        out = []
        while blocks[k]["parent"] != "root":
            k = blocks[k]["parent"]
            out.append(k)
        return out

    parents = [k for k in sections if order.get(k)]
    if not parents:
        return None
    P = parents[0]
    A = order[P][-1]
    inside = set(subtree(P)) | set(ancestors(P))
    movable = [k for k in sections if not order.get(k) and k not in inside and k != A]
    if not movable:
        return None
    M = movable[-1]
    rest = [k for k in sections if k not in (P, A, M)]
    if len(rest) < 3:
        return None
    S, K, T = rest[0], rest[1], rest[2]
    D = docs[0]
    G = docs[1] if len(docs) > 1 else docs[0]
    text = "Edited body links [[@%s]].\n\n```\n[[@%s]]\n```\n" % (K, K)
    return [("set", S, "src", text),
            ("set", D, "keywords", "alpha, beta"),
            ("move", M, P, A),
            ("link", G, "explains", P),
            ("set", T, "title", "Renamed by A1")]


def event_places(e):
    if e[0] == "set":
        return [e[1]]
    if e[0] == "move":
        return [e[1], e[2], e[3]]
    return [e[1], e[3]]


def apply_events(store, events, applied):
    for k, e in enumerate(events):
        if any(p not in store["blocks"] for p in event_places(e)):
            continue
        applied.append(k)
        if e[0] == "set":
            store["blocks"][e[1]][e[2]] = e[3]
        elif e[0] == "move":
            _, key, parent, after = e
            old = store["blocks"][key]["parent"]
            store["order"][old].remove(key)
            sibs = store["order"][parent]
            sibs.insert(sibs.index(after) + 1, key)
            store["blocks"][key]["parent"] = parent
        elif e[0] == "link":
            store["links"].append((e[1], e[2], e[3]))
    return store


def code_spans_removed(line):
    out = []
    i = 0
    while i < len(line):
        if line[i] == "`":
            run = 0
            while i + run < len(line) and line[i + run] == "`":
                run += 1
            j = i + run
            close = None
            while j < len(line):
                if line[j] == "`":
                    r = 0
                    while j + r < len(line) and line[j + r] == "`":
                        r += 1
                    if r == run:
                        close = j
                        break
                    j += r
                else:
                    j += 1
            if close is None:
                out.append(line[i:i + run])
                i += run
            else:
                i = close + run
        else:
            out.append(line[i])
            i += 1
    return "".join(out)


def ref_keys(text):
    keys = []
    fence = None
    for line in lines_of(text):
        if fence is not None:
            if closes(line, fence):
                fence = None
            continue
        op = fence_of(line)
        if op is not None:
            fence = op
            continue
        if indent(line) >= 4:
            continue
        s = code_spans_removed(line)
        i = 0
        while True:
            a = s.find("[[", i)
            if a < 0:
                break
            b = s.find("]]", a + 2)
            if b < 0:
                break
            inner = s[a + 2:b].split("|")[0].strip(" ")
            if inner.endswith(".md"):
                inner = inner[:-3]
            keys.append(inner)
            i = b + 2
    return keys


# A derived edge from a block to the place its `[[@<place>]]` names.
def derived(store):
    edges = set()
    for key, b in store["blocks"].items():
        for k in ref_keys(b.get("src", "")):
            if k.startswith("@") and k[1:] in store["blocks"] and k[1:] != key:
                edges.add((key, k[1:]))
    return sorted(edges)


def sx(x):
    if isinstance(x, str):
        out = ['"']
        for c in x:
            if c == "\\":
                out.append("\\\\")
            elif c == '"':
                out.append('\\"')
            elif c == "\n":
                out.append("\\n")
            elif c == "\t":
                out.append("\\t")
            elif ord(c) < 32 or ord(c) > 126:
                out.append("\\x%x;" % ord(c))
            else:
                out.append(c)
        out.append('"')
        return "".join(out)
    if isinstance(x, bool):
        return "#t" if x else "#f"
    if isinstance(x, int):
        return str(x)
    if isinstance(x, tuple) and len(x) == 2 and x[0] == "SYM":
        return x[1]
    return "(" + " ".join(sx(e) for e in x) + ")"


def sym(s):
    return ("SYM", s)


def render(store):
    rows = []
    for key in sorted(store["blocks"]):
        b = store["blocks"][key]
        parent = b["parent"]
        index = store["order"][parent].index(key)
        fields = [[sym(name), b[name]] for name in sorted(b) if name != "parent"]
        fields = [[sym("kind"), sym(b["kind"])] if f[0][1] == "kind" else f for f in fields]
        rows.append([key, [sym("parent"), parent], [sym("index"), index], [sym("fields")] + fields])
    return sx([sym("a1-expected"),
               [sym("blocks")] + rows,
               [sym("links")] + [list(l[:1]) + [sym(l[1])] + list(l[2:]) for l in store["links"]],
               [sym("derived")] + [list(e) for e in derived(store)]]) + "\n"


def main():
    corpus, out = sys.argv[1], sys.argv[2]
    os.makedirs(out, exist_ok=True)
    timings = []
    store = import_corpus(corpus, timings)
    with open(os.path.join(out, "after-import.sexp"), "w", encoding="ascii") as fh:
        fh.write(render(store))
    if all(p in store["blocks"] for e in EVENTS for p in event_places(e)):
        rule, events = "fixed", EVENTS
    else:
        chosen = choose_events(store)
        rule, events = ("chosen", chosen) if chosen else ("fixed", EVENTS)
    applied = []
    edited = render(apply_events(store, events, applied))
    with open(os.path.join(out, "after-edits.sexp"), "w", encoding="ascii") as fh:
        fh.write(edited)

    def event_sx(k, e):
        return [k, sym(e[0])] + [sym(x) if (e[0] == "set" and i == 2) or (e[0] == "link" and i == 2) else x
                                 for i, x in enumerate(e[1:], 1)]
    with open(os.path.join(out, "events.sexp"), "w", encoding="ascii") as fh:
        fh.write(sx([sym("events"), [sym("rule"), sym(rule)],
                     [sym("applied")] + [event_sx(k, events[k]) for k in applied],
                     [sym("skipped")] + [k for k in range(len(events)) if k not in applied]]) + "\n")
    slowest = max(timings) if timings else 0.0
    print("%d blocks, %d files, events %s, %d of %d applied, slowest file %.3f s"
          % (len(store["blocks"]), len(md_files(corpus)), rule, len(applied), len(events), slowest))
    return 0


if __name__ == "__main__":
    sys.exit(main())
