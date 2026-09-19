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

# WHERE THE PARENTHESES ACTUALLY PUT THINGS.
#
# A MISSING CLOSER IS NOT A SYNTAX ERROR. The reader is perfectly happy:
# the definitions after the short form simply become part of its body.
# The first thing anyone hears about it is an "unbound identifier"
# naming something defined much further down -- reported at the place
# that USES it, never at the place that swallowed it.
#
# MEASURED, on daemon.sc, 2026-09-18: one `)` missing at the end of
# `watch-loop` swallowed twenty-six later definitions, and Chez said
# `unbound identifier directory-of at line 219` -- a hundred and eighty
# lines from the cause. The whole file's parentheses still balanced,
# because a second edit had one closer too many, so the cheapest check
# anyone would reach for said nothing at all.
#
# TWO READINGS, AND THE SECOND IS THE ONE THAT LOCATES IT:
#   * the depth at end of file says THAT something is wrong;
#   * the depth at the start of every line that opens a top-level
#     definition says WHERE -- the first definition not at the expected
#     depth is the first one that was swallowed, and the form before it
#     is the one that failed to close.
#
# NEVER: IT REFUSES TO JUDGE WHAT IT CANNOT READ. This scanner knows about
# strings, escapes, line comments and character literals; it does NOT
# know about `#|` block comments or `#;` datum comments. A file using
# either is reported as NOT CHECKED, by name and with the reason, and
# never quietly measured with the wrong rules -- a scanner that is wider
# than its comment is worse than no scanner, because the place it is
# wrong is the place nobody looks.

import os
import sys


def scan(text):
    """Returns (depth_at_eof, [(line, depth) for each top-level define],
    unsupported_reason_or_None). `defines` covers both a library body
    (indent two) and a script (indent zero); the caller picks."""
    i = 0
    line = 1
    depth = 0
    in_string = in_comment = False
    at_line_start = True
    line_start = 0
    opens = []
    library_at_top = False
    while i < len(text):
        c = text[i]
        if c == "\n":
            line += 1
            in_comment = False
            at_line_start = True
            i += 1
            line_start = i
            continue
        if not in_comment and not in_string:
            if text.startswith("#|", i) or text.startswith("#;", i):
                return (depth, opens, library_at_top,
                        "uses %s, which this scanner does not read"
                        % text[i:i + 2])
        if at_line_start and not c.isspace():
            # NOTE: THE INDENT IS MEASURED FROM THE START OF THE LINE, not
            # from wherever the scan happens to be. Reading it off the
            # slice at the current position makes every first-on-its-line
            # `(define` look like column zero, whatever its indent -- and
            # the gate then reports every internal definition in the tree
            # as a swallowed one. Measured: twenty-four such reports on a
            # tree with nothing wrong with it.
            indent = i - line_start
            if text.startswith("(define", i):
                opens.append((line, depth, indent))
            if text.startswith("(library ", i):
                library_at_top = True
            at_line_start = False
        if in_comment:
            i += 1
            continue
        if in_string:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_string = False
            i += 1
            continue
        if c == ";":
            in_comment = True
            i += 1
            continue
        if c == '"':
            in_string = True
            i += 1
            continue
        if c == "#" and i + 1 < len(text) and text[i + 1] == "\\":
            i += 3
            continue
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        i += 1
    return depth, opens, library_at_top, None


def check(path):
    """(problems, skipped_reason)"""
    with open(path, encoding="utf-8") as handle:
        text = handle.read()
    depth, opens, library, unsupported = scan(text)
    if unsupported:
        return [], unsupported
    problems = []
    if depth != 0:
        problems.append("parentheses do not balance: %d %s at end of file"
                        % (abs(depth), "unclosed" if depth > 0 else "too many closers"))
    # A library body's definitions sit at depth 1 and indent 2; a script's
    # sit at depth 0 and indent 0. Only the ones at the file's own shape
    # are judged, so an internal `(define` indented further is left alone.
    want_indent = 2 if library else 0
    want_depth = 1 if library else 0
    nth = 0
    for line, at, indent in opens:
        if indent != want_indent:
            continue
        nth += 1
        if at != want_depth:
            problems.append(
                "the %d%s top-level define, at line %d, is at depth %d and not %d"
                " -- the form before it never closed"
                % (nth, ordinal(nth), line, at, want_depth))
            break
    return problems, None


def ordinal(n):
    if 10 <= n % 100 <= 20:
        return "th"
    return {1: "st", 2: "nd", 3: "rd"}.get(n % 10, "th")


# NEVER: THE GATE PROVES IT CAN STILL GO RED, ON EVERY RUN. "163 files, 0
# failures" is what a working gate prints and also what a gate that
# matches nothing prints, and one of the two ways of being wrong here is
# silent for as long as the tree happens to be sound. Measured: an
# earlier version of this file read the indent from the wrong place and
# reported twenty-four sound files as broken; the opposite mistake would
# have reported nothing, for ever, and looked exactly like this one does
# now.
SOUND = """(library (probe)
  (export a)
  (import (rnrs))
  (define (a x)
    (+ x 1))
  (define (b y)
    (* y 2)))
"""

SWALLOWED = """(library (probe)
  (export a)
  (import (rnrs))
  (define (a x)
    (+ x 1)
  (define (b y)
    (* y 2)))
"""


def self_check():
    """Returns a list of problems with the gate itself."""
    problems = []
    depth, opens, library, unsupported = scan(SOUND)
    if unsupported or depth != 0:
        problems.append("the sound sample does not read as sound (depth %d)" % depth)
    if not library:
        problems.append("the sound sample was not recognised as a library")
    if [1 for _, at, indent in opens if indent == 2 and at != 1]:
        problems.append("the sound sample reports a definition at the wrong depth")
    depth, opens, library, unsupported = scan(SWALLOWED)
    caught = [line for line, at, indent in opens if indent == 2 and at != 1]
    if not caught:
        problems.append("a definition swallowed by a short form was NOT detected"
                        " -- this gate can no longer fail and says nothing")
    return problems


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    parent = os.path.dirname(here)
    # NOTE: AND THE SUBDIRECTORIES THAT HOLD SOURCES. `mcp/` was outside this
    # list until the shell moved into it, so the one file added that batch
    # was the one file this gate could not see -- a gate that scans "the
    # tree" and means "two directories" is wrong in the place nobody
    # looks. Named rather than walked: a walk would pull in scratch
    # directories and pinned copies that are not this tree's sources.
    files = []
    for directory in (parent, here, os.path.join(parent, "mcp")):
        if not os.path.isdir(directory):
            continue
        for name in sorted(os.listdir(directory)):
            if name.endswith(".sc") or name.endswith(".sls"):
                files.append(os.path.join(directory, name))
    if not files:
        print("FAIL structure: no Scheme sources found to check")
        print("1 failures")
        print("structure complete")
        return 1
    failures = 0
    for problem in self_check():
        print("FAIL structure gate itself: %s" % problem)
        failures += 1
    skipped = []
    for path in files:
        shown = os.path.relpath(path, parent)
        try:
            problems, reason = check(path)
        except (OSError, UnicodeDecodeError) as problem:
            print("FAIL %s: could not be read -- %s" % (shown, problem))
            failures += 1
            continue
        if reason:
            skipped.append((shown, reason))
            continue
        for problem in problems:
            print("FAIL %s: %s" % (shown, problem))
            failures += 1
    print("checked %d files" % (len(files) - len(skipped)))
    # NEVER: A SKIP IS NAMED, WITH ITS REASON. A count of what ran is silent
    # about the file it did not read, and a file this gate cannot read is
    # exactly where a broken one would sit unnoticed.
    if skipped:
        for shown, reason in skipped:
            print("NOT CHECKED %s: %s" % (shown, reason))
    else:
        print("not checked: none")
    print("%d failures" % failures)
    print("structure complete")
    return 1 if failures else 0


sys.exit(main())
