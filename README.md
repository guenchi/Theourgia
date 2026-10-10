# Theourgia

A block-graph store where agents and people write together.
Licensed under the Apache License, Version 2.0. See LICENSE.

## Installing

**Requirements: Chez Scheme, libuv, and igropyr.** Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1 (the datum
printer is measured identical on these; on any other version the verbs that print a datum refuse with
`(error unsupported-printer-version)`). On Homebrew the Chez binary is `chez`, not `scheme`; use that
name wherever `scheme` appears below, and set `THEOURGIA_SCHEME=chez`: the programs start each other (the daemon, `core.sc`, `eval`'s worker) with `THEOURGIA_SCHEME`, else `scheme`.

**Platforms: the ones with a measured row.** The numbers the code hands
to the kernel or reads back from it that differ between platforms --
open flags, errno values, signal numbers, struct sizes and field offsets
-- are taken from a table of MEASURED rows, `platform-numbers.sc`, one per
platform. What POSIX fixes stays in the code (permission bits, the wait
status encoding, the file-type mask operation), a C scalar the rows do
not measure (`size_t`) takes Chez's width for its machine type, and the
signal names an evaluation reports are those whose numbers every row
shares. The rows: macOS on arm64 and on x86_64, Linux on x86_64
and on aarch64 with glibc, and FreeBSD 15 on amd64. The macOS x86_64 row
was read under Rosetta and equals the arm64 row number for number.
FreeBSD 15 on amd64 has a measured row, but the daemon does not start there yet (reading its
own environment fails intermittently); it is expected in a later
release. The row is chosen when
the programs start, from Chez's machine type and, on Linux, from the C
library the process has mapped. On any other platform -- another
architecture, a Linux with musl, a static executable, a sandbox that
hides `/proc` -- every program
(`theourgia.sc`, `core.sc`, `theourgiad.sc`, the MCP shell `mcp/server.sc`,
and `eval --lang`'s launcher) writes

    (error platform-unmeasured (system "<s>") (machine "<m>") (remedy "run test/probe/layout.c and add its row"))

on the standard error, with `(libc "<l>")` for a Linux, and exits 75
before it does anything else; it is never run on a neighbouring
platform's numbers. The remedy is literal: compile
`test/probe/layout.c` there, run it, and add its output as a row. NOTE:
The first Linux runs found seven numbers the code then held as literals
wrong there, one of which (`O_APPEND`) wrote every log record over the
start of its segment.

**Build.** `build.ss` compiles every library -- this tree and igropyr -- into a directory of objects,
and copies the programs beside them:

    scheme --script build.ss <library-root> <output-root>

Both arguments are the directory that *contains* `theourgia/` and `igropyr/`, not the source directory
itself.

**Run.** Point Chez's two variables at the output and run the client from it:

    export CHEZSCHEMELIBDIRS=<output-root>
    export CHEZSCHEMELIBEXTS=".so"
    scheme --script <output-root>/theourgia/theourgia.sc init --store /path/to/store

A short wrapper on the PATH that sets those two variables and execs the program is all an install
needs: `theourgia` for `theourgia/theourgia.sc`, `theourgia-mcp` for `theourgia/mcp/server.sc`, and
`theourgiad` for `theourgia/theourgiad.sc`.

**The programs.** `theourgia.sc` is the command you run. It answers through a daemon, and starts
`theourgiad.sc` beside itself when none is running; `init`, `eval` and any request under
`THEOURGIA_LOCAL=1` it hands to `core.sc`. `core.sc` starts `eval-worker.sc` for `eval` and
`eval-runner-exec.sc` for `eval --lang`. `mcp/server.sc` is the MCP shell and starts `theourgiad.sc`
one level up, and runs `core.sc`, also one level up, as its own child for `eval`. `theourgiad.sc` takes only `serve`. The programs find each other by path, not through
the library path, so keep the output directory as `build.ss` lays it out.

NOTE: **The shipped form is this directory of objects, not one file.** `eval`, forwarding and the
daemon load libraries by name at run time, and a whole-program package would drop them for being
statically unreferenced. Objects start about twelve times faster than source, because they do not
re-expand the libraries on every call.

**From source, for development.** Name a directory holding both `theourgia/` and `igropyr/`, and run
the programs directly:

    export CHEZSCHEMELIBDIRS=/path/to/that/directory
    export CHEZSCHEMELIBEXTS=".sc:.ss:.sls:.scm"
    scheme --script theourgia.sc <verb> [...]

NEVER: **The products do not belong in the source tree.** A stale `.so` beside a `.sc` is resolved in
preference to it, so a tree holding both can be running code nobody has edited for a week.

## Quick start

    theourgia init --template project

One store is one project. The `project` template gives it four roots and seven relations, and records
them in the store as its template block (`template export` prints it):

- `design` — the document `design.md`; record each decision as a block of kind `decision` under it.
- `tasks` — the document `tasks.md`; record each task as a block of kind `task` under it, with a
  `status` of todo, doing, done or dropped.
- `docs/` — documents for readers are top-level docs whose path starts with `docs/`.
- `code/` — code is imported so that the paths of its files start with `code/`.

The relations are `implements` (a task or code → the decision it carries out), `depends-on` (a
task or code → one it needs first), `supersedes` (a decision → the one it replaces), `refutes` (a
decision or doc → one it shows is wrong), `verifies` (a test → the code or decision it checks),
`conflicts-with` (two decisions that cannot both hold) and `documents` (a doc → the code or
decision it explains). Each is a name `link` accepts; the reserved names (`ref`, `uses`, `calls`,
`guards`) are not among them. Any other relation name still links; these are the ones the template
names. The first six have an effect of their own; the template declares `documents` as a listed
edge with no effect (see [`relation`](#relation)), and the rule `cover`, citation coverage (see
[`rule`](#rule)): a write in a project store that carries `--premises` and links a block, by a
relation of kind `depends-on`, to one its premises do not hold is refused. `describe`, answered by the daemon, lists the store's roots and relations, the store's declared relations as `(declared-relations (<name> <kind> [(from <selector>)] [(to <selector>)]) ...)` when it has any, and the
MCP tools that write carry the roots' sentences after the writing protocol.

`theourgia init` without a template is the advanced form: the store has no shape until you give it one.

## Two ways to run

**Standalone.** Every verb opens the store, answers, and exits. Nothing
is left running, and two commands that overlap are two processes taking
the store's lock in turn.

**With a daemon.** `theourgia serve <store>`, which runs the daemon program
`theourgiad.sc serve <store>`, holds the store open and
answers over a unix socket. A client finds that socket by the same rule
the daemon used to create it, so nothing needs to be told where it is.
`theourgia.sc` sends an ordinary verb to the daemon when one is there,
and starts one and asks again when none is; `core.sc` sends it to the
daemon when one is there and answers it in its own process when none is.

NEVER: **The daemon's answer is the CLI's answer, byte for byte.** Both call
the same dispatcher; no verb, answer or error shape exists in one and not
the other. Set `THEOURGIA_LOCAL=1` to skip the socket and answer in
process even when a daemon is running.

## Global options

NOTE: **These are accepted by EVERY verb**, and they are here rather than in
each verb's line because repeating them thirty-eight times would say they
were somehow special to each one.

| option | what it does |
|---|---|
| `--store <dir>` | which store. Falls back to `THEOURGIA_STORE`, then `.` |
| `--actor <name>` | who the request is from. Falls back to `THEOURGIA_ACTOR`, then `USER`, then `cli` |
| `--socket <path>` | reach a daemon at this path instead of the default |
| `--wire` | print the answer as one S-expression per line rather than for a person to read |
| `--req <id>` | the request's identity, so a retry is recognised as the same request rather than a second one |
| `--cursor <w:n>` | the position this request is composed against |
`THEOURGIA_WRITER` is the writer a request is for when it names no writer of its own; unset or empty, there is none, and it never falls back to the actor. `THEOURGIA_WIRE=1` does what `--wire` does, for `eval` only.

`THEOURGIA_LOCAL=1` answers in this process even when a daemon's socket
is there. NOTE: It is a debugging path: it skips the daemon rather than
doing something the daemon cannot.

`--store`, `--actor`, `--wire` and `--socket` say where a request goes and are consumed before it is sent; a request that reaches a server still naming one (a raw socket request, or an MCP tool call) is refused with `(error bad-request transport-option-in-rpc (option "<option>") (belongs-to shell))`, naming the first one written. Every MCP tool's description says the tool is bound to one store and actor.

## Writing for agents

A block is the unit of writing: one block should answer one question on its own.
Keep a block under about 800 tokens (roughly 3000 bytes); split a longer one.
Give every block a one-sentence title.
Give every block 3 to 8 keywords, comma separated, with --keywords.
Place a block under the parent its source or subject puts it under, with --under.
Do not rewrite the source bytes: splitting a document must not edit its prose.
Change a block with write and then commit, through a draft, rather than replacing it.
Hold a writer id from one agent at a time: a later session may bind the same id and carry on with its drafts, but two agents writing one draft at once overwrite each other silently.
Record a decision as a block of kind decision, and link each block that implements it with link <block> implements <decision>.
Open a session with commitments --open, which lists the decisions not yet implemented, done or dropped.
Set class with set <id> class inference for what you concluded and did not verify, and with set <id> class external for material from outside; neither is ever treated as a ruling.
To replace a ruling, write the new one and link <new> supersedes <old>; to record that something is wrong, link <evidence> refutes <claim>.
Say what your work rests on with link <work> depends-on <premise>: when the premise changes, the work is marked for review.
After checking that an implementation still carries out its decision, link <impl> implements <decision> again: linking again is how you say you checked.

## Reading a store

Every verb prints one S-expression per item on stdout; the exit code is the verdict.
**Empty output with a zero code is an answer** — no references, no hits, no
differences — and is never an error.

### `read`

    (read <id> ("--md") ("--recursive") ("--writer" <name>)
          ("--working") ("--working-info") ("--signature") ("--cut" <cut>) ("--rev"))

It answers:

    (ok <block> (version "<hash>") (cut <cut>) (versions ((<id> . "<hash>"))))
    (ok (items <block> ...) (versions ((<id> . "<hash>") ...)) (cut <cut>))
    (ok (text "<markdown>") (cut <cut>) (versions ((<id> . "<hash>") ...)))

What a block says. Without options it is one block as data; `--md` gives its own
bytes — the heading line it was written with, then its body.

**The version is the token `--if-unchanged` compares**: the block's hash in the
state that was read, so a caller that read a block can write it back only if
its block hash still matches the one it read. It comes after the block, so a reader of the block
reads what it read before. `--recursive` gives one pair per block in item order,
after the items. A deleted block has no version, read alone or in a subtree.
`--working` answers as it did.

A code block's derived fields -- a text block's `name` and `doc`, read
through the language table, and a datum block's `names` and `name` -- are
read when it is shown. When that reading raises, `read` (one block or a
subtree) and `outline`, which shows its title, are refused as any raise is,
with the block named after the condition:
`(error internal (condition "...") (id <id>))`.

**The receipt: which state was read, and which version of each block.** A read of
the committed store ends with `(cut <cut>)`, the applied cut of the state it read
(for `--cut`, the state at that cut), and `(versions ((<id> . "<hash>") ...))`,
one pair for every block whose content the answer shows or whose id it lists as a
result, in order of first appearance and each once; a block that cannot be hashed
is `(<id> unavailable (reason ...))` and a deleted one is left out. Both come after
the answer's own clauses, so a reader of what came before reads what it read
before. `read` (plain, `--md`, `--recursive`), `refs`, `commitments`, `tasks`,
`names` and `uses` carry both; `search`, `grep` and `whereis`, which already said
their cut, add the versions; `outline` and `reach` answer structure and carry the
cut only. The receipt names the state the rows were read from: a request handed
a state (the daemon hands each request its published one) is answered from it,
except `search` and `grep`, which read the store themselves and answer what a
fresh read of it gives -- rows and receipt alike. A clause added after the verb
has answered, `(incomplete ...)` today, follows the receipt.
A working read, a draft listing, `log`, `diff`, `conflicts`, `check`,
`diagnostics`, `describe` and the exports carry none. The human output of every
verb is unchanged except for the two printed whole, plain `read` and `reach`,
whose human output shows their clauses. A block whose value is nested too
deeply to hash is still read; its version is `(version unavailable (reason ...))`,
as a write's state section says when it cannot describe a block.

`--signature` answers the signature an editor supplied for the block (see
"Derived data from an editor"):

    (ok (signature "<text>") (stale <n>) (via (vscode "<version>" "<languageId>")))
    (ok (signature absent) (stale <n>) (via))

from the committed store's facts, or with `--working` from the writer's own
facts judged against its working view; with no table at all it is
`(ok (signature absent))`. A fact that no longer matches what it was computed
from is not used, and `(stale <n>)` counts those about this block. `--signature`
with `--md`, `--recursive` or `--working-info` is refused with
`(error bad-request incompatible-signature-options)`.

A file-level block holds almost nothing of its own: its body is the front matter
and whatever sits above the first heading, which in most documents is nothing at all.
`--recursive` asks for the subtree instead — the block and every block under it,
in document order. `grep --under` searches exactly the same blocks: both ask one
walk of the outline, which stops at nothing. With `--md` as well, that is the
document: for a **top-level** document, the same text `export-md` writes to the
file, from the same renderer, so a read and a round trip cannot disagree.

A document is a top-level block of kind `doc`. A block of kind `doc` under another
block is a shape the write path refuses to create — `insert`, `move` and `set`
answer `(error doc-must-be-top-level (parent <id>))` — but one can arrive from
history written before that rule or from another store. When one does, nothing
treats it specially: `read --recursive`, `grep --under` and `export-md` see a
block like any other under its parent. `export-md` writes its title and body
into the file of the top-level document above it when that document is
written; when there is none, or it is not written, the block goes with it and
is counted in `skipped` like any other. Two places tell a reader it is there:
`conflicts` reports it as `nested-document`, and if its body was written and it
holds front matter, which has nowhere to go in the middle of another document's
file, the export answer names it under `fields-not-written` (see `export-md`).
Asked for on its own, `read <it> --md` still gives its front matter.

A section asked for on its own does not carry the document's front matter, which
belongs to the document. Its body carries the blank line that separated it from
whatever followed in the file, so a subtree lifted out of the middle of a document
ends with one — put it back where it came from and the file is reproduced exactly.

Neither `--md` nor `--recursive` takes a value and both are stripped before the id
is read, so the two orders are the same request.

**`--working` reads the writer's own view**: the committed store with that
writer's drafts laid over the blocks they cover. `--writer` names whose;
without it the writer is the one this caller was given. A block with no
draft still reads as committed in the same answer -- the overlay replaces
the blocks it covers, not the view. `--working-info` adds what the view
is made of rather than changing what is read. `--working` or `--working-info` with `--md` or `--recursive` is refused with `(error bad-request incompatible-working-options)`.

**`--rev` names the publication the answer was built from**, for a program that
follows the store with `subscribe`: an `ok` answer gains `(rev <n> (daemon
"<token>"))` after its other clauses, where `<n>` is the daemon's publication
number and `<token>` the daemon's incarnation, the same pair a change frame
carries. Only a daemon has publications: on the local route, or for a state no
daemon published, it answers `(error bad-request (reason no-published-state))`.
It is refused with `--cut` (`incompatible-cut-options`) and with `--working-info`
or `--signature` (`incompatible-rev-options`); with `--working` it names the
publication under the writer's draft. An answer that carries a rev is printed
whole, as `--wire` prints it, in either mode.

**`--cut` reads the block as it was at a causal cut**: a tag name or a cut written
out as `(("writer" . 12) ...)`, as `diff` takes them -- typically an entry's `cut`
from `log`, which is the store right after that event. The answer is the one a
read gives without `--cut`, over the state at that cut, `--md` and `--recursive`
included; a block that did not exist yet answers `unknown-id`, and at the empty cut
`()` every block does. An event's `cut` from `log` holds the event and its premises
and nothing else (its `past` holds the premises alone), so a writer the event did
not depend on is absent from it whatever its clock said; a cut written out by
hand holds whatever it names, provided every event in it has its premises with it.
The cut is judged as `diff` judges it: a name no tag has answers `unknown-tag`, an
unsettled tag `tag-unsettled`, and a cut naming an event not yet received, a writer
twice, or an event without its premises `(error cut-unavailable (cut cut) (reason
not-received|duplicate-writer|not-closed))`. Each such read replays the log from
its beginning to the cut. When the replay does not reach the cut that was judged
-- a record in it was set aside as a writer's fork between the judgement and the
replay -- the read is refused `(error cut-moved (asked <cut>) (served <cut>))`,
naming the cut asked and the one the replay reached, and never answers ok for a
cut it did not serve. Through the daemon, a cut naming an event another
process wrote after the daemon last read the store is answered: the daemon
reads the store again first. An event written after that check may not be
seen by this request (`not-received`); it is answered once the daemon has read
the store again. If that reading fails, the daemon keeps answering from what it
had read before. `--working`, `--working-info`, `--writer` and
`--signature` ask about a writer's draft or an editor's present facts and are
refused with it: `(error bad-request incompatible-cut-options)`. The version is the
block's version AT the cut; given to `--if-unchanged` it is refused unless the
block has not changed since. When a writer cannot be read, the answer's
`incomplete` clause names it as every read's does, even when that writer is not in
the cut.

### `refs`

    (refs <id>)

It answers:

    (ref (from <id>) (rel <rel>) (via link|md))

A text reference's rel is always `ref`. Rows are ordered by the referring id, then by rel. An id the store does not know is refused with `(error unknown-id <id> (nearest ...))`.

What points **at** this block, from the two places a reference can live. A `link`
record is an edge someone wrote and `unlink` removes it; a reference in the text is a
sentence, and nothing removes it but editing the sentence. Each line says which it
came from, and they are never merged: you can unlink an edge, you cannot unlink a
sentence. Text references are derived at read time rather than stored as edges,
because materialising them would give one fact two suppliers that can then disagree.

A block's own out-edges are not references to it; those are part of what `read`
returns about the block.

A third place is an editor's: the calls it supplied (see "Derived data from
an editor"). Those rows come after the store's own, and their via is the
provenance that supplied them:

    (ref (from <id>) (rel calls) (via (vscode "<version>" "<languageId>")))

Once a calls table was read the answer carries `(stale <n>)`, the supplied
calls into this block that were dropped because what they were computed from
has changed, and `(via ...)` for the rows it shows.

### `reach`

    (reach <id> ("--rel" <rel>) ("--depth" <n>))

It answers:

    (ok (reached ((<id> <depth>) ...)) (stale <n>) (via ...))

The blocks this block reaches over the edges an editor supplied under
`<rel>` (`calls` when not given), following them outward, up to `<n>` hops
(1 when not given). The block itself is at depth 0; every block is listed
once, at the fewest hops that reach it, so a cycle ends. An edge written with
`link` is not followed: those are `refs`' to show. A relation no supply
produces edges of -- today every name but `calls` -- is refused with
`(error unknown-relation (rel <rel>))`. Once a calls table was read,
`(stale <n>)` counts the supplied edges out of the blocks the walk went on
from that were dropped because what they were computed from has changed,
and `(via ...)` names the provenances of the edges that reached a block;
with no table the answer is `(ok (reached ((<id> 0))))`.

### `search`

    (search <query> ("--all") ("--all-validity"))

It answers:

    (hit <id> <score> "<snippet>" (fields (<field> ...)))
    --all           every hit, not just the best ten
    --all-validity  superseded and refuted blocks too (see "Class and validity")

The fifth part names the fields THIS block matched in. It is not the
`fields` of the `scanned` clause, which names the fields the search read and
is the same for every answer; this one differs from block to block and is
always a subset of it.

Ten hits come back unless `--all` is given, and when there are more the
answer carries

    (truncated (hits <n>))

with `<n>` the number not shown. The count is named for the same reason
grep's is: a bare number after a clause name means whatever the verb decided
it meant.

Whitespace splits the query into tokens and **every** token must hit. A hit is a
case-insensitive substring, so `cat` finds `concatenate`.

Text is compared after normalising: NFKC, and a space written at every boundary
between CJK and Latin characters, so a CJK word typed against a Latin one is the
same query as the two with a space between them. A CJK token of two characters or
more is matched by its bigrams, which finds more than a plain substring would.

Each field scores in two tiers: a hit that begins a word — at the start, after a
non-word character, or at a camelCase seam — outranks one buried inside a longer
word. Keywords score 4 or 3, title 3 or 2, src 2 or 1. A name a block defines is
not prose and scores above all of them TOGETHER: 12 for the whole name, 10 for a
name that begins with the query. A match in `doc` or in the printed body of a datum
scores 1, so the most a block with no name match can reach is
keywords 4 + title 3 + src 2 + doc 1 + body 1 = 11. An exact definition is above
that; a prefix match ties with it and the tie is broken by id, which is the
intended answer -- a name that merely BEGINS with the query is not obviously a
better result than a page about the query.
Fields are counted once each however many times they match.
Order is score descending then id ascending — total, so two runs over one store agree.
The query is only ever text: nothing in it reaches a numeric parser.

Keywords an editor supplied (see "Derived data from an editor") are
searched only for a block with no keywords of its own author's, and score as
src does, 2 or 1, below any author's keywords; a hit found there names
`derived-keywords` among its fields. When a table of such keywords was read,
the `scanned` clause names `derived-keywords` too, and the answer ends with
`(stale <n>)`, the keyword facts it could not use, and `(via ...)` for the
keywords behind the hits it shows.

Only a `code` or `library` block has names at all -- a name is something defined
or carried, not a field anyone may set -- so `search` and `whereis` ask the same
question of the same blocks. A deleted block is not searched: the set of blocks
that exist is the outline.

#### What a `--wire` answer says about the scan

`search` and `grep` carry, after their items, the cut they read as `(cut ((<writer> . <seq>) ...))` and then:

    (scanned (blocks <n>) (fields (<field> ...)) (unreadable-blocks <m>))

`<n>` is how many live blocks THIS answer looked at, which is not always how
many the store holds: a query with no tokens looks at nothing and says `0`
over a store full of blocks, and `grep --under` counts only the subtree it
was given. `<m>` is how many of those `<n>` held text that could not be
decoded, counted once per block however many of its fields were affected.
`<m>` is a fact about THIS answer; it does not accumulate. The clause names
its unit because a bare `unreadable` could as easily have counted lines,
fields or bytes.

A successful `whereis` carries `scanned` without `unreadable-blocks`: it
consults the definitions index rather than reading block text, and a count
that could only ever be zero would point a caller at something playing no
part in the answer. A `whereis` that does not know the name answers an error
with the nearest names it does know, and carries no `scanned` at all.

When `search` finds nothing it also carries

    (coverage (defs (names <n>)) (names-from (datum lexical)))

`<n>` is how many names the definitions index holds. Zero beside a non-zero
`blocks` is the ordinary case for prose, and it has **two** sources this
answer cannot tell apart: a store with nothing nameable in it, and a store
whose nameable blocks could not be read -- the index guards each block and
contributes no names for one it could not parse. Naming the second is
scheduled work; until it lands, `(names 0)` does not say which of the two it
is. Whether a block is nameable is decided by what is IN it, not by the flag
it was imported with. `grep` gets no `coverage` clause -- it consults no
index, and
`scanned` already says what it looked at.

None of these clauses appear without `--wire`. The human rendering of an
answer with items writes the items and nothing else, but for the `(excluded ...)`
line when validity left out every hit (see "Class and validity" below) and an
`(incomplete ...)` line.

### `grep`

    (grep <pattern> ("--under" <id>) ("--all") ("--all-validity"))

It answers:

    (match <id> <line-no> "<text>")
    --under <id>    only the block given and what is under it; an id that names no block matches nothing
    --all           no limit on how many lines come back
    --all-validity  lines of superseded and refuted blocks too

`search` finds blocks; `grep` finds lines. It prints every line of a live
block's text that contains the pattern, in outline order, with the number of
the line within that block.

The pattern is **literal**. A `.` matches a dot and nothing else; there is no
regular expression syntax here.

Matching ignores case. The text searched is a block's `src` together with the
`doc` and the printed body derived from it, in that order, and the line number
counts through that sequence.

Two limits keep one block from crowding out the rest: at most 20 lines from any
one block, and at most 200 lines in all. When either applies the answer carries

    (truncated (lines <n>) (blocks <m>))

where `<n>` is how many matching lines were not shown and `<m>` how many blocks
matched without a single line of theirs appearing. `--all` removes both limits.
The counts are named because a bare number after a clause means whatever the
verb decides it means, and a reader takes clauses by name.

Both `grep` and `search` are answered against the same blocks: a deleted block
has no lines.




### `whereis`

    (whereis <name> ("--all-validity"))

It answers:

    (def <id> (library <lib>) (name <sym>) (kind code))
    (export <lib-id> (library <lib>) (name <sym>))
    --all-validity  records in superseded and refuted blocks too

Says where a name is. A `def` record is a block that defines it; an `export` record
is a library that carries it in its export list, which means this library re-exports
the name and the definition is somewhere else — possibly in another package
entirely. Definitions come first.

The name must match whole and EXACTLY: these are Scheme identifiers, where two
spellings that differ by case are two different names, so
the lookup does not fold case. A name given in the wrong case is refused like any
other name that is not there -- and the right spelling is normally one edit away,
so it comes back under `nearest`.

A name that is nowhere is refused with `(error unknown-name <name> (nearest ...))`,
naming up to five of the closest names it does know: those sharing a prefix first,
then those containing the query, then those within a couple of typing errors.

### `log`

    (log (<id>))

It answers:

    (entry (event <writer> <seq>) (ts <ms>) (actor "<name>") (verb <verb>)
           (cut ((<writer> . <seq>) ...)) (past ((<writer> . <seq>) ...)))

What this store has applied, in the order it was delivered. `cut` is the causal cut
right after the event and `past` the one right before it: the event's premises,
each with its own premises, and nothing a writer did without depending on it.
`read <id> --cut <cut>` gives a block as the event left it, and `diff <past> <cut>`
what the event changed. Given an id, only the
records that **named** that block: a record whose premises mention it has not touched
it, and neither has one whose text mentions it.

This verb replays the whole log rather than reading from the snapshot, and is
therefore the slow one. Two facts force it: the timestamp and actor exist only as a
record is delivered, and a snapshot-seeded read never delivers what the snapshot
already covers — which would produce a log that silently began in the middle.

### `tag`

    (tag (<name>) ("--premises" <datum>))

It answers:

    (tag (name "<name>") (cut ((<writer> . <seq>) ...)))
    (tag (name "<name>") (cut ((<writer> . <seq>) ...)) (event <writer> <seq>) (unsettled #t))

With a name, binds it to the cut this store has **applied** — not the frontier it has
discovered, which would name records no reader of this store could produce. The cut is
taken before the record exists, so a tag is never inside the cut it binds, and an edit
written afterwards does not move it.

Without a name, lists what is bound. Writing a name again supersedes the earlier tag
**causally**: a session that had seen the first wins, while two written concurrently
both stand and are reported as unsettled with their event ids rather than with a
winner chosen here. Both records remain in the log either way; a tag is not a
uniqueness constraint.

### `diff`

    (diff <cut> <cut>)

It answers:

    (added <id>) | (removed <id>) | (changed <id> <field>)

Each side is a tag name or a cut written out as `(("writer" . 12) ...)`. Two states
are built by reading to each cut — not the current state with something subtracted,
because the only way to know what the store said at a moment is to read it then.

The projection is title, src, level, parent, ord and the edge set: a tombstone is
`removed`, and an edge-set change is `(changed <id> links)`. Comparing only the scalar
fields would call two states identical when an edge had moved.

A cut literal is parsed by shape and never handed to `read` — the reader implements
the whole numeric syntax, and `#e` with a large exponent asks it to build an integer
of any size from eleven characters of argument. A cut this store cannot reach is
refused as `(error cut-unavailable (cut from|to) (reason ...))` rather than truncated to the part it can reach. A name that is not a tag is refused as `(error unknown-tag <name> (cut from|to))`, and a name whose tags are unsettled as `(error tag-unsettled <name> (cut from|to))` rather than resolved to one of them.

### `describe`

    (describe)

What the verbs are, what each is for, and the protocol for writing. Answers

    (ok (verbs (<verb> (usage <form>) (description <text>) (protocol <bool>) (route <route>)) ...)
        (protocol "<the writing protocol>"))

for something that has to ask rather than be told: the MCP shell builds its
tool list from this, so a tool description and the verb it describes cannot
drift apart.

It reads a table and runs nothing. NEVER: In process it does not open the store, take a lock
or write a byte. When the store has a template, the daemon adds `(template (roots ...)
(relations ...))` from the state it publishes; there describe is a read like any other, which may
check the store's files for a change made from outside and reload, and never writes. The in-process
route does not open a store to describe it.

NOTE: **The table needs no store; asking a daemon for it does.** Answered in
process — which is what `theourgia describe` does when it runs the server
locally — it works with no store at all. Asked over a socket, the request goes
to *that store's* daemon, and a daemon for a store that does not exist cannot
start: the answer is then `(error store-not-found (store <path>))`, relayed
from the daemon. The two are not in conflict; they are different questions,
and this paragraph exists because the first sentence on its own read as a
promise the second breaks.

Each entry also carries `route`, which says who carries that verb out:
`daemon` for a verb a client sends over the socket, `local` for one the client
runs in its own process, `child` for one run by `core.sc` as a child of the caller (the
command line execs it, the MCP shell runs it as its child). `init` is `local`, because it is what creates the
store there would otherwise be nothing to send to; `eval` is `child`.

The `protocol` flag on an entry says that verb's description carries the
writing protocol text. It is set on `insert` and `write`. NOTE: It does not mean
"this verb changes the store" — `set` does that and is not marked.

### Verbs added as data

`commitments`, `tasks` and `template` are not in the core's own table: each is an entry in
`(theourgia extensions)`, registered by `theourgia` and the daemon before a request is parsed, and
its library is loaded only when the verb is called. An entry is

    (verb usage description protocol? daemon value-options flag-options (library . name) [declaration])

and a registered verb's route is always `daemon`. The optional last field says what the verb does
with a load that could not read a writer: `accept` (the default: it answers and its answer carries
`(incomplete ...)`), `refuse` (it refuses such a load, as `export-md` and the other verbs that write
from what they read do), or `(refuse <action> ...)` (it refuses only when its first argument is one
of those actions: `template` declares `(refuse "apply")`). Any other value, for example `maybe`, is
refused when the entry is registered.

### `conflicts`

    (conflicts)

It answers:

    (conflict <id> cycle|unplaced) | (orphan <id>) | (pending (event <w> <seq>) (missing <w> <seq>))
      | (nested-document <id>) | (relation-contested <name> (candidates (<value> <w> <seq>) ...))
      | (rule-contested <name> (candidates (<value> <w> <seq>) ...))
      | (duplicate-path <path> (ids (<id> ...)))
      | (unknown-verb (event <w> <seq>) (verb <v>)) | (malformed-record (event <w> <seq>) (reason ...))
      | (cut <writer> <path> <kind> <after>)

A record this build does not understand, or cannot apply, is listed with its event; a writer whose history was cut by damage is listed once as `cut`. A relation name whose declarations disagree is listed as `relation-contested` with every surviving declaration (see [`relation`](#relation)), and a rule as `rule-contested` (see [`rule`](#rule)).

What the store holds and cannot show: blocks in a structural conflict, blocks whose
parent was deleted or never arrived, and records still waiting for premises. A record
waiting on several premises is listed once for **each** — stopping at the first would
send an operator to fetch one record and leave them where they started. A section with
nothing in it prints nothing. A path that two alive datum libraries, or two alive text
files, both hold is listed as `duplicate-path` with every id that holds it: an export
would write both to one file, and refuses.

The structural conflicts are read from the same place `outline` reads them, so the two
cannot disagree; `outline` prints the mark as a fourth column on that row.

### `commitments`

    (commitments ["--open"] ["--all"] ["--drifted"] ["--since" <cut>] ["--under" <id>])

A decision is a block of kind `decision`, and it is owed until it is discharged in
one of two ways: a block implements it, said with

    theourgia link <impl> implements <decision>

(the implementation is the source, the decision the target), or its `status` field
reads `done` or `dropped`. `status` is read leniently, as the symbol or the string
(`set` writes strings, `batch` may write symbols); any other value, a conflict or
no `status` at all leaves the decision open, and the answer says which. An edge of
another relation does not discharge it, and neither does an `implements` edge whose
source block was deleted: the answer names that source under `implementer-deleted`.

Time is a cut, never a sequence number. A decision's **origin** is the cut just after
the write that created the block. An implementation has **drifted** when its content
changed since the edge, as "Class and validity" below defines it: no single `implements`
link of it, and no single content write of the decision, was made by a writer who had
seen all of the implementation's current content. Its content is every field except the
bookkeeping ones -- `status`, `batch`, `keywords`, `class` and `slug` -- which say where a
block stands, not what it says: a task set to done after it was linked has not drifted,
and moving a block or linking to it is not a change either. Two writers who each saw a
part of the change do not make one who saw all of it. Editing the decision having seen
the change, or linking again, attests anew. A draft that is not committed is not a
change. Which decisions are discharged, who implements them and who has drifted are
read from the one provider that also answers validity.

By default it lists the open decisions; `--all` lists every decision. `--drifted`
keeps those with at least one drifted implementation, `--since <cut>` those whose
origin the cut does not cover, and `--under <id>` those in that block's subtree, the
block included (an unknown id answers `unknown-id`). In a store with a template, the
decisions listed by default are those under the root the template names for commitments
(`design` in the project template), and the answer ends with `(scope <root-id> (outside <n>))`,
n being how many rows the same request would list with `--under root` that this answer does not;
`--under root` lists the whole store. In a store without
a template nothing is narrowed and no scope item is added. Oldest origin first: decisions
whose origins are not ordered against each other are listed in a fixed order and say
so under `concurrent-with`. It answers:

    (decision <id> (title "<t>") (status <s>) (origin <cut>)
      (implemented-by (<impl> <cut>) ...) [(drifted (<impl> <cut>) ...)]
      [(implementer-deleted <impl> ...)] [(concurrent-with <id> ...)])
    (skipped <id> <reason>)

The cut beside an implementation under `implemented-by` is when it was linked; under
`drifted`, when it last changed. A block that looks like a decision but cannot be
read as one is not left out silently: it is listed as `skipped`, with the reason, under
every option, for example a kind spelt as the string `"decision"`
(`kind-not-a-symbol`) or a block that a `set` or `move` named but nothing created (`no-origin`). The verb writes
nothing.

### `tasks`

    (tasks ["--status" <status>] ["--batch" <batch>] ["--under" <id>])

Lists the blocks of kind `task`: by default those under the root the store's template names for
tasks, with `--under <id>` that block's subtree, and with `--under root` or in a store without a
template, every task. When the template chose the scope, the last item is
`(scope <root-id> (outside <n>))`, n being how many rows the same request (filters included)
would list with `--under root` that this answer does not. It answers

    (task <id> (title "<t>") (status <s>) (batch <text>|absent) (implements (<id> live|tombstoned) ...)
      [(unlinked)])

`status` is read like a decision's: todo, doing, done or dropped as a symbol or a string; anything
else is printed as `(status unreadable "<text>")`. `(unlinked)` means the task implements no live
decision: an edge to a deleted block, or of another relation, does not link it. `--status` takes one
of the four words, and `--batch` matches the `batch` field's text exactly. An `implements` edge from
a task discharges its decision only once the task's status is done.

### `names`

    (names <id>)

Which names a code block uses, and which libraries a library block imports, as the
stored code shows them. It answers:

    (ok (items (name <sym>) ... (import <lib>) ... (import-unreadable <spec>) ...)
        (name-use syntactic [(lexing whole-text)] | none (reason <r>)))

The answer is **syntactic**: what the text shows, never what a compiler resolves.

A datum block's stored body is walked as data. A symbol is a use unless the form it
sits in binds it: `lambda`'s formals, the `let` family's names, `do`'s variables,
`guard`'s variable, `rec`'s name, and the definitions at the head of a body, which
bind in the whole body. `define`, `define-syntax`, `define-record-type` and
`define-values` are definitions only as the block's own form or at the head of a body;
`(define (f x) ...)` does not bind `f` in its own body, so a recursive call is a use.
The known forms are `quote`, `quasiquote` (its unquoted parts are walked), `lambda`,
`case-lambda`, `let`, `let*`, `letrec`, `letrec*`, `let-values`, `let*-values`, `do`,
`begin`, `if`, `and`, `or`, `when`, `unless`, `set!`, `cond`, `case`, `guard`,
`parameterize`, `rec` and `foreign-procedure`; their own keyword is not a use, nor
`else` at the head of a `cond`, `case` or `guard` clause, nor `=>` as the second
element of a `cond` or `guard` clause as written (`case` has no arrow: `=>` there is a
name), nor `case`'s data or `foreign-procedure`'s types and calling convention. A known
form whose shape does not fit its rule is walked as the fallback walks it: the shape is
the form's own (its arity, binders and clauses) as R6RS writes it, or as Chez Scheme
accepts it where Chez is wider (a `case` clause with one datum, an `else` clause anywhere
in a `case`), and a body is its definitions and then at least one expression, a `begin`
splicing only before the first expression. What stands in one of the form's expression
positions is judged where the walk reaches it, not by the enclosing rule. A block's top
form may be a `begin`, which splices. Every
other form, a macro of the code's own included, is walked whole: its keyword and every
name in it count as uses -- a pattern variable of `syntax-rules` too. That is the
**over-approximation**. A name that only a macro's expansion introduces is not seen:
the **under-approximation**.

A text block's uses are every token of its text that matches its language's
`identifier` pattern (the language table holds one per language), **comments and
strings included** and keywords too: the answer says `(lexing whole-text)`. A token
starts where the pattern first matches, so in `9abc` the token is `abc`, and is
the longest match there whatever the pattern's alternation order prefers, read up to 4096 characters (a longer one is read as more
than one). A block whose language has no entry answers
`(name-use none (reason no-language))`.

A library block, a program's included, answers its imports, each spec reduced to the
library it names (`only`, `except`, `prefix`, `rename`, `for` and the `library`
wrapper removed, a version reference dropped); a spec that does not reduce is listed as
`import-unreadable`. Any other block answers `none` with the reason: `not-code`,
`deleted`, `no-language` or `unreadable-code` (a stored block that cannot be read
this way). An unknown id answers `unknown-id`. Nothing is evaluated or written.

### `uses`

    (uses <name> ["--under" <id>])

The live code blocks that use a name, compared whole and exactly (case matters,
`car` is not `cart`), one row per block, ordered by id:

    (ok (items (use <id> (library <lib-id>|none) (mode datum|text)) ...)
        (name-use syntactic) [(skipped (no-language <n>) (unreadable-code <n>))])

`library` is the nearest enclosing library block, or `none`. `skipped` counts the
blocks in scope that could not be read, by reason, and is there only when there are
some. With `--under <id>` the blocks in that block's subtree, the block included (an
unknown id answers `unknown-id`).

**It is not find-references.** A use is listed whether or not anything defines the
name, and whichever library defines it; nothing is resolved. In a text block a
definition is also a use -- `function target() {}` is listed by `uses target` -- the
distinction between defining and using exists only for datum code. Each request reads
every block of the state it answers from; nothing is kept between requests. Through
the daemon, `names` and `uses` are answered by its store process from a fold of the
store taken for the request, so a commit made outside the daemon is in the next
answer.

### Class and validity

A block's `class` says what kind of statement it is: `observation`, `inference`,
`ruling`, `verification` or `external`. A block with no class is a `ruling` if it is a
decision and an `observation` otherwise. Observations, rulings and verifications are
**authoritative**; inferences, external material, a class in conflict and one that
cannot be read are not.

Six relations have an effect on whether a block is still in force; every other
relation is only an edge.

| relation | from -> to | effect |
|---|---|---|
| `supersedes` | newer -> older | the older block is superseded |
| `refutes` | evidence -> claim | the claim is refuted while it has not changed since; once it has, it needs review |
| `depends-on` | dependent -> premise | the dependent needs review when the premise is deleted, superseded, refuted, needs review, or changed since |
| `implements` | implementation -> decision | discharges the decision (see `commitments`); the implementation needs review when either end changed since, or the decision is deleted, superseded, refuted or needs review |
| `verifies` | check -> subject | positive evidence while the subject has not changed since |
| `conflicts-with` | block <-> block | none, unless proposed |

A store may give another name one of these effects by declaring it (see
[`relation`](#relation)): `relation cites --as depends-on` makes a `cites` edge
read as `depends-on`, so a review needs review when a cited paper is refuted
or changed since; `answers` declared as `implements` discharges a decision (a
task's done is still its stored status). The edge keeps its name -- edges,
queries and answers print it -- and the rules read its kind. A name declared
`nothing` is a listed edge with no effect. A name never declared, retired, or
contested is a plain edge.

**Changed since** is causal, not a matter of clocks: an end of an edge has changed since
the edge when no single link of the edge, and no single content write of the other end,
was made by a writer who had seen all of that end's current content. Linking again is
how you attest. `supersedes`, `refutes` and `conflicts-with` take effect only from an
authoritative block; from another they are proposals, and the target needs review with
the reason `proposed-supersedes`, `proposed-refutes` or `proposed-conflicts-with`. A
deleted block has no effect, and neither has an edge from a block to itself.

A block's **validity** is one of `superseded`, `refuted`, `needs-review` and `valid`, in
that order of precedence, with its reasons, each `(<why> <block>)`: `superseded-by`,
`refuted-by`, `refutation-moved`, the three `proposed-` reasons, `premise-gone`,
`premise-superseded`, `premise-refuted`, `premise-needs-review`, `premise-moved` and
`implementation-moved`. A reason caused through a declared name carries it, `(<why>
<block> (relation <name>))`, for example `(premise-moved <id> (relation cites))`, and so
does a `premise-needs-review` passed on through a declared name; one caused through one
of the six is as above. `context` builds a block's validity from the rows of its five
queries, whose `validity-reason` fact is the reason without the clause, so its sections
print the kind's reason without `(relation <name>)`. A decision's state is one of `closed`, `open`, `review`,
`verified` and `implemented`, the first that holds in that order.

A plain `read` of a block that is not valid adds `(validity <v> (<why> <block>) ...)`
after its own clauses, and `read --recursive` adds `(validity ((<id> <v> (<why> <block>)
...) ...))` for the blocks that are not valid; no clause means valid, and `read` never
hides a block. `search`, `grep` and `whereis` leave out superseded and refuted blocks and
say so with `(excluded (blocks (superseded <n>) (refuted <n>)))`, counted in blocks; a hit
on a block that needs review is returned and named in `(validity ((<id> needs-review
(<why> <block>) ...) ...))`; `--all-validity` leaves nothing out and names every hit that
is not valid. Their human output shows the hits in force and nothing else: to see the
superseded and refuted ones, ask with `--all-validity`. While a hit is left it gives no
count of what it left out, so every line is a hit. When everything was left out it
prints the `(excluded ...)` line -- then an `(incomplete ...)` line, if the store missed
a writer -- and exits 0: so for a `whereis` of a name whose every record is in a
superseded or refuted block (a name the store does not know is `unknown-name` instead),
and for a `search` or a `grep` whose every hit was left out, where one that matched
nothing prints nothing. The `validity` clause, and `excluded` beside a hit, are in the
`--wire` answer only. The human output is for a person: a program reads these verbs,
and every other, with `--wire`. These clauses come before the receipt. A store that links none of the six relations, and no name declared as one of them, answers every read and
search exactly as before.

### `query`

    (query (<goal>) ("--relations"))

A language of queries and rules over the committed state at one cut, and
nothing else: it reads no log, runs no code, writes nothing, and takes no
rule over the wire. A TERM is a constant (a string, a symbol, an exact
integer, or a list of those as the second argument of `member`), a
variable (a symbol starting with `?`), or `_`, a variable never reported
and never joined. A GOAL is a fact relation, a rule of the library, a test,
or `(and <goal> ...)`. A rule body is a conjunction read left to right;
several rules with one head are a disjunction, and a rule may call itself.
There is no negation: every answer is the least model of the rules, and
the same whatever order the facts were written in.

It answers every binding of the goal's named variables, in the order they
first appear, one row each:

    (row <value> ...)
    (vars (?v ...)) (digest "<hex>") (cut <cut>) [(name-use syntactic)]

Each row is rendered as a block hash renders a block; rows are
deduplicated and sorted by those bytes, and the digest is the sha256 of
the rows joined by newlines, so the same bindings at a later cut have the
same digest. A goal with no variable answers one empty row when it holds.
`(name-use syntactic)` says a `uses-name` fact was consulted. The human
output prints one row a line, its values separated by a space. Refusals
name what is wrong: `not-a-goal`, `unknown-relation` (with the known
names), `wrong-arity`, `not-a-term` (a term its position does not take),
`test-variable-unbound` (a test's variable, `_` included, that no earlier
goal binds), `unbound-argument` (the text of `score`), `query-budget` (more
than a million tuples of new work in one query, never a partial answer),
`unrenderable` (a value past the codec's nesting).

A library whose name two writers contest has no name: no `library`,
`in-library`, `def` or `imports` fact speaks one for it, so no rule that
goes through a name reaches the code under it, and a query says nothing
about it rather than something false.

What a query costs, measured on two stores with nothing else running: on a
memory store of 3037 blocks, `unsettled-for` from its root (8030 tuples)
takes about 165 ms; on this repository's own libraries imported as a datum
store (2522 blocks, 17855 name uses), `def-for` for its most name-using
block (28570 tuples) takes 394 ms, against 376 ms before every read asked
the reducer whether a field is contested. `depends*` was measured empty on
both: neither store holds a link.

`query --relations` lists the two tables below as items, and then the
store's declared relations, one `(declared <name> <kind> (from <selector>)
(to <selector>))` each, or `(declared <name> (contested))`. This section is
generated from the two tables, and docs-check compares the two.

A rule about an effect asks the edge's KIND: `edge-kind` and `moved-kind`
answer an edge under a declared name as its kind, so `depends`, `supersedes*`,
`contradicts`, `replaces`, `evidence-for` and `hard` read a `cites` declared as
`depends-on` as they read `depends-on`. `edge` and `moved` are the literal
facts: a query by the declared name still finds its edges. `edge-kind` and
`moved-kind` are views of the `edge` and `moved` builds and cost what those
cost.

Fact relations:

- `kind/2` -- every live block, and its settled kind, else none, conflict or unreadable
- `class/2` -- every live block, and its effective class
- `validity/2` -- every live block, and its validity: valid, needs-review, refuted or superseded
- `version/2` -- every live block that can be hashed, and its version
- `title/2` -- a block whose title is a settled string, and that title
- `field/3` -- every settled field of a live block: its name, and its value as stored
- `edge/3` -- every surviving edge between two live blocks: from, relation, to
- `edge-kind/3` -- an edge whose relation has a kind, built-in or declared, `nothing` included: from, that kind, to
- `relation/2` -- every relation with an effect kind: the six by their own names, and each name declared in force with its kind, nothing included
- `under/2` -- a live block and its settled parent, "root" at the top
- `ref/2` -- a text reference [[id]] in a live block's text to a live block
- `library/2` -- a library block and its name
- `in-library/2` -- every code block, and the name of the library containing it, or none; nothing under a library whose name is contested
- `imports/2` -- a library, by name, and a library name it imports
- `def/3` -- a definition record: the block, its library's name or none, the name; nothing in a library whose name is contested
- `uses-name/2` -- a code block and a name it uses (name use, syntactic)
- `lang/2` -- a code block's language
- `validity-reason/3` -- each reason of a block that is not valid, and the block that causes it
- `decision-state/2` -- a live decision, and its state: closed, open, review, verified or implemented
- `moved/4` -- an edge of an effect-bearing relation one of whose ends moved, and that end
- `moved-kind/4` -- a moved edge with its relation's kind in place of its name, and the end that moved
- `verified-by/2` -- a live block, and a live block with a current verifies edge to it
- `verified-moved/2` -- a live block, and a live block with a verifies edge to it that it has moved past
- `score/3` -- exactly the hits search <text> returns with no cap, each with its score; text required

Tests, on bound values only: `=/2`, `/=/2`, `string</2`, `member/2`.

Rules of the library:

- `depends/2` -- a depends-on or implements edge from a to b
- `depends*/2` -- depends, transitively
- `under*/2` -- b is an ancestor of a (walks up from a bound a)
- `within/2` -- d is inside m (walks down from a bound m)
- `supersedes*/2` -- a supersedes b, through a chain too
- `affects/2` -- b depends on a, transitively
- `in-force/1` -- x is valid or needs review
- `authoritative/1` -- x's effective class is observation, ruling or verification
- `unit/2` -- u is t or inside t
- `scope-of/2` -- m is in t's unit, or what the unit depends on, transitively
- `unsettled/1` -- a decision whose state is open or review
- `unsettled-for/2` -- an unsettled, in-force, authoritative decision in t's scope or inside it
- `scope-name/2` -- a library name visible to b: its own library and those it imports
- `def-for/3` -- a definition d of a name b uses, in b's scope, in force and authoritative
- `ambiguous/2` -- b uses a name two such definitions answer
- `contradicts/2` -- a and b conflict, both in force and authoritative, either way round
- `replaces/2` -- c supersedes or refutes o
- `evidence-for/2` -- v verifies c currently, or implements it
- `hard/4` -- what context must show for t: the block h, its role, and the block it is about

### `context`

    (context "--for" <id> "--budget" <tokens> ("--all-validity"))

`context --for <id> --budget <tokens>` answers the material to read before
working on a block, within a budget, with the receipt a commit gives back.
`--for` takes any live block T; its UNIT is T and everything under it, its
SCOPE the unit and what the unit depends on, transitively. It writes
nothing, reads no log, runs no user code and keeps nothing between
requests. An unknown or deleted id is answered as `read` answers it.

What MUST be shown is the answer of five queries of the query language,
run in one session with T's id written into each:

    (and (hard T ?h ?role ?about) (validity ?h ?v))
    (and (hard T ?h _ _) (validity-reason ?h ?why ?by))
    (and (unsettled-for T ?d) (decision-state ?d ?s))
    (and (unsettled-for T ?d) (decision-state ?d review) (moved-kind ?i implements ?d ?end))
    (and (scope-of T ?a) (contradicts ?a ?b))

The hard sections and the notes are built from their rows and from the
records of the blocks the first one names, and from nothing else. Each
hard block is in exactly one place, read from the top:

| the block is | it is entered in |
|---|---|
| T itself | `for`, whatever its class or validity |
| not in force (superseded or refuted) | `to-verify` |
| in force, authoritative | `constraints` |
| in force, not authoritative | `evidence` |

Every entry carries each why it has -- `(why unit)`, `(why member)`, `(why
unsettled <state>)`, `(why supersedes <m>)`, `(why cause <m>)`, `(why
implementer <d> <end>)`, `(why stale-verification <m>)` -- and, when it is not
valid, `(validity <v> (<why> <by>) ...)`. `notes` hold `(nogood <a> <b>
conflicts-with)` once for each pair that contradicts in the scope, and
`(stale-verification <m> verifies <c>)` for each block `m` in the scope whose
`verifies` edge's target `c` has moved past it (`verified-moved`, the
lifecycle's word: past every name of the edge): `c` is a hard block, about `m`,
so the receipt holds it and a verification going stale refuses an older one.

What else fits follows, never in a hard section and never in the receipt,
each candidate once, an earlier step winning: in `background`, the
definitions of the names a hard code block in force uses, one step, each
of several candidates for one name marked `(ambiguous)`; in `background`,
the blocks that verify or implement a constraint; in `counterexamples`, the
blocks a constraint superseded or refuted that are not in force; in
`background`, the blocks that rest on a hard block, the blocks relevant to
T's title and text by score, and the ancestors of hard blocks, nearest
first. Without `--all-validity` only candidates in force are taken;
with it the others are taken too, marked with their validity.

An entry is shown at a level: `identity`, `(<id> (kind <k>) (title "<t>")
(version <h>) (level identity) <why> ... [(validity ...)] [(names <n>
...)])`; `summary`, which adds `(summary "<text>")`, the first 240 bytes of
the block's text cut back to the end of a line; `full`, which adds `(block
<record>)`, the record a plain `read` answers.

THE BUDGET is tokens of four bytes over the answer as the wire renders it,
every clause but `(cut ...)` counted; nine tenths of it are usable, and an
`ok` answer without its cut is never larger. The envelope is reserved at
its largest first, from the request and the hard set alone: the whole
receipt, the notes, the budget clause at the budget's own digits,
`insufficient` at twenty of the longest hard entries, and the six excluded
counters at six digits, and the `(incomplete ...)` clause the
dispatcher appends when a writer could not be read, which a caller
receives too. A budget that cannot hold that and T is refused with
`(error budget-too-small (minimum <tokens>))`, the smallest budget that
can. A hard block that cannot be hashed has no premise to give, and the
answer is refused with `(error unhashable-hard-block (id <id> ...))`. Then, greedily and in
one pass: the hard blocks at identity in HARD ORDER -- T; the rest of the
unit in document order; the other members by distance from the unit over
depends edges, ties by id; then the others by role (unsettled, supersedes,
cause, implementer, stale), by id -- one that does not fit named in
`(insufficient (missing ((<id> <bytes>) ...)) [(more <n> (bytes <b>))])`,
at most twenty named, every later one still tried; then one pass of
upgrades in hard order, to full when it fits, else to summary; then the
preferred material in what room is left, one that does not fit at identity
counted in `(excluded (definitions <n>) (evidence <n>) (counterexamples
<n>) (dependents <n>) (relevant <n>) (ancestors <n>))`. Which hard blocks
are shown, and at which level, depends on nothing but the rows of the five
queries, the hard blocks' records and the request.

    (ok (for <entry>)
        (constraints (<entry> ...)) (evidence (<entry> ...))
        (to-verify (<entry> ...)) (counterexamples (<entry> ...))
        (background (<entry> ...)) (notes (<note> ...))
        [(insufficient ...)] (excluded ...)
        (budget (tokens <t>) (used <u>) (reserve <r>))
        (receipt <premise> ...) (cut <cut>) (versions (...))
        [(name-use syntactic)])

`used` is the answer's own size in tokens and `reserve` the tenth held
back, so used plus reserve is at most tokens. THE RECEIPT, `(receipt
(premise <id> <h>) ... (premise (query <goal>) <digest>) ...)`, holds a
block premise for EVERY hard block, shown or named as missing, and the
five goals with their digests; `commit --premises` takes it verbatim, and
refuses it once anything that decides the hard material has changed. The
preferred entries are not in it; their versions are in `(versions ...)`,
as on every read. `(name-use syntactic)` says the definitions step
consulted name use.

The human output prints each section's name and one line per entry, the
notes one per line, the `insufficient`, `excluded` and `budget` clauses as
`--wire` writes them, and then the receipt on one line, the clause exactly as
`--wire` writes it, ready for `commit --premises`; `(incomplete ...)`, when a
writer could not be read, follows it. The cut and the versions are `--wire`'s
only.

## Following a store

### `subscribe`

    (subscribe <stream> <rev> (<token>))

`<stream>` is `changes`, the one stream there is. A daemon verb that keeps its
connection: `theourgia subscribe changes 0` prints
the acceptance and then one line for each publication the daemon makes, as it
makes it, until the stream ends. It has no local route (`(error bad-request
(reason needs-daemon))`), and the MCP shell, which is request and response, does
not offer it.

The acceptance is `(ok (subscribed <rev>) (daemon "<token>") (current <n>))`,
with `(incomplete ...)` when the publication is missing a writer. `<token>` is
the daemon's incarnation, `<pid>-<start-ms>`, new at each start; `<n>` is its
current publication. Revision 0 subscribes from now and replays nothing: read
what you need after subscribing. A resume, `subscribe changes <rev> <token>`,
replays every publication after `<rev>` that the daemon still holds (it keeps
the last 256 while anyone is subscribed).

Every line after the acceptance is a bare datum. A publication is

    (changes (rev <n>) (daemon "<token>") (from-cut <cut>) (cut <cut>)
             (items <item> ...) [(incomplete ...)])

and its items say what changed from the publication before it: `(added <id>)`
and `(removed <id>)` (such a block carries no field or position items in that
frame -- read it whole); `(changed <id> <field>)`, `(changed <id> position)`,
`(changed <id> parent)`, `(changed <id> ord)`; `(conflict <id> <what>)` and
`(resolved <id> <what>)` for a field, a position, or the structure (`cycle`,
`orphan`, `unplaced`, `nested`); `(edge-added <from> <rel> <to> (event <writer>
. <seq>))` and `(edge-removed <from> <rel> <to>)`. A reload that fails is
reported as `(error store-unreadable (reason ...))`, with no revision, and the
stream goes on; so does a further request on the connection, answered `(error
bad-request (reason subscribed))`.

A commit made outside the daemon -- another process writing to the store
directly -- arrives too: while anyone is subscribed, the daemon compares the
store with what it published once a second, so such a commit reaches a
subscriber within about a second. With nobody subscribed it does not look.

**Apply frames by revision, not by cut.** Subscribe first, then `read --rev`
each object you follow, and keep the revision it answered. A frame whose rev is
at most an object's revision is already in it; the next one is applied; one
further ahead is a gap: read the object again. A read whose daemon token is not
the subscription's means the daemon restarted: subscribe afresh with 0 and read
again. The cuts in a frame are informational (a retraction can move the applied
cut backwards).

The refusals: `(error bad-request invalid-revision)` (not decimal digits, or
past the current publication), `(error bad-request missing-token)` (a resume
without a token), `(error changes-unavailable (reason daemon-restarted)
(current <n>) (daemon "<token>"))` (another incarnation's token), `(error
changes-unavailable (reason window) (oldest <r>) (current <n>) (daemon
"<token>"))` (a revision of the resume no longer held). The stream ends with
one terminal line: `(error draining)` when the daemon stops (exit 0 once the
connection closes after it); `(error changes-unavailable (reason lagging)
(current <n>) (daemon "<token>"))` when the reader fell 64 publications behind;
`(error transport-unknown (reason store-actor-down))` or `(reason
write-failed)`. Those two and lagging exit 1 when they arrive. A connection
that closes with no terminal, or in the middle of a line, or a line that stops
arriving for 5 s, prints `(error transport-error lost-stream)` and exits 1; a
line past the answer limit prints `(error transport-error answer-too-large)`.
A stream with nothing to say waits as long as it has to.

## Mail and review between sessions

Sessions on one machine -- of any vendor, through the command line or the
MCP shell -- talk to each other through a store. There is no message verb
and no message record: a message is a block, and reading one's mail is a
query.

### The mailbox

A message is a block of kind `doc` with two fields: `to`, the reader's
actor name as a string, and `status`, `"unread"` when it is sent and
`"read"` once the reader has read it. An `about` edge may name the block it
concerns. The sender writes `status` once, in the record that creates the
block; after that only the reader sets it, so the reader's `"read"`
supersedes the sender's `"unread"` in causal order. Reading one's mail is
the goal

    (and (field ?m "to" "codex") (field ?m "status" "unread"))

given to `query`, with the reader's own name in place of `codex`. A status
two of the reader's sessions set concurrently is contested: the message
leaves this query (a contested field is no fact) and is listed by
`conflicts` until the reader sets it again. It is not lost; its notice is
late.

### `scope`

    (scope <dir> "--cut" <cut> "--roots" <id> "--for" <actor>)

Makes a new store at `<dir>` for one reader, holding exactly what it is
asked to review and nothing else. `--cut` is a causal cut, a literal or a
tag name; `--roots` names a block, and is given once per root; `--for` is
the reader's actor name. All three are required.

The new store holds the LETTER COPY -- a `doc` with `to` (the reader),
`status` `"unread"`, `cut` (the cut's literal, as text), `roots` (the root
ids, a list of strings) and `scope` (the absolute path of `<dir>`) -- and
then one block for each block under the roots as it was at the cut: the
roots in the order `--roots` gives them, each at the top level after the
letter copy, and under each root its blocks in outline order, each under its
parent's copy. Each copy carries the block's settled fields
and two more, `origin` (the block's id in this store) and `origin-cut`.
A field contested at the cut is not copied, and is listed as `(contested
<id> <field>)` -- read from the value's shape, so a field whose one value
has exactly a contested field's shape is taken for one; the derived fields a code block's view adds (`name` and
`doc` in text mode, `name` and `names` in datum mode) are not copied, and
the new store derives them again. An edge between two copied blocks is
copied between their copies; an edge from a copied block to one outside the
roots is dropped and listed as `(dropped-edge <from> <rel> <to>)`. An edge
into the roots from a block outside them is not read, and so is neither
copied nor listed. Tags are not copied. Roots that overlap are refused
(`roots-overlap`), and so is a root retired at the cut (`root-deleted`). A
block whose position at the cut is part of a cycle is copied at the top
level.

The letter copy then gets `baseline`, the new store's applied cut once the
copies are in, as text: `read --cut <baseline>` in the new store serves the
copies as they were written, whatever the reader edits later. Last, ONE
record is written in this store -- the LETTER, a top-level `doc` with the
same `to`, `status`, `cut`, `roots` and `scope` -- and its id is set on the
letter copy as `letter`. The answer is

    (ok (scope <dir>) (cut <cut>) (letter <id>) (letter-copy <id>)
        (baseline <cut>) (blocks <n>) (edges <n>) (dropped-edges <n>)
        (contested-fields <n>) <dropped-edge> ... <contested> ...)

An existing `<dir>` is refused with `(error scope-exists (dir <dir>))`
before anything is read or written. The cut is read through this store's
own route, so a cut it cannot serve is its refusal (`cut-moved`,
`cut-unavailable`, `unknown-id`) and nothing is written anywhere. Every
block and edge the scope would write is first put to the write's own
validator, at the place it would be written (a `doc` only at the top
level); one it would refuse refuses the whole scope as `(error
scope-unwritable (items ...))`, and nothing is written. A failure after the
new store exists leaves it as it is and answers `(error scope-failed (dir
<dir>) (step import|baseline|letter) ...)`.

`scope` is a program over ordinary requests, and like `eval` its route is
`child`: it runs in the command line's own program, or as the MCP shell's
child, and never in a daemon. It sends each store its own requests, through
that store's daemon when one serves it and in process otherwise. It is not
a verb a daemon has, so one asked for it answers `unknown-verb`. The MCP
shell offers it as a tool; name `<dir>` by an absolute path there.

### `review-results`

    (review-results)

In a store `scope` made: the reader's answer, as one datum. The letter copy
is the first block the store made -- first causally: after an adoption the
store has a second writer, and the letter copy is the one first put every
other writer's first put has in its past. The RESULTS are every live block that
has no `origin`, is not the letter copy, and was created by the letter's
`to` -- the actor of the record that created it, not the writer, which
adoption changes. So a block the author added to the scoped store is not a
result, and neither is an edited copy.

    (ok (results (letter <main-letter-id>) (to <actor>)
                 (blocks (block <id> (parent <id>|letter)
                                (fields ((<field> <value> <actor>) ...))
                                (about ((origin <id>)|(scoped <id>) ...))) ...))
        (no-about <id>) ... (missing <id> <field>) ...)

Each result's fields are its settled ones, each with the actor of the
record that set it; its `parent` is another result's id or `letter`; an
`about` edge is given as `(origin <id>)` when its target is a copy, else
`(scoped <id>)`. A VERDICT is a result directly under the letter copy and
a FINDING a result under a verdict: a finding with no `about` edge is
listed as `(no-about <id>)`, and a verdict without one of `model`,
`approval`, `sandbox` and `effort` as `(missing <id> <field>)`. Nothing is
refused for either. A store that `scope` did not make is refused
(`not-a-scoped-store`), and so is one whose letter copy has no `letter`
yet (`letter-not-set`).

### `collect-into`

    (collect-into <letter-id> "--results" <datum>)

Writes a `review-results` datum under the letter `<letter-id>` in this
store, in two writes, each planned from the store as it is when the write
holds the lock. The first inserts every result not collected before,
under the letter or under its parent's copy, with its fields and one more,
`scoped-id`; the second links each `about` edge that is not already there,
to the origin id itself, or to the copy of a result. A copy is found by
the `scoped-id` of the record that created it, not by its current fields
or place, so a moved copy, or one whose field was changed since, is still
the same copy; two copies of one result refuse with `(error
collect-ambiguous (scoped-id <id>) (copies (<id> ...)))`. The records are
the reader's: their actor is the datum's `to`. Neither write carries the request's identity, as a
tracked write does: the second would be read as a replay of the first and
not run. What makes a retry safe is the plan each write makes from the
store as it finds it under the lock. A result whose parent is a result
collected in the same call goes after it, whatever order the datum lists
them in. The answer is

    (ok (inserted <n>) (linked <n>) (already <n>) (unresolved <id> <target>) ...
        (foreign-field <id> <field> <actor>) ...)

`already` counts the edges that were there; `unresolved` names an `about`
target that has no copy here to link -- a block in the scoped store that is
neither a copy nor a result, such as a reader block deleted before it was
ever collected, or a block the author added there -- and nothing is linked
for it; `foreign-field` names each
field another actor set on a result in the scoped store, which travels
here under the reader's name. A second `collect-into` with the same datum
inserts and links nothing; one stopped between its two writes, run again,
inserts nothing and links what is missing; two at once take the store's
lock in turn.

### `collect`

    (collect <dir>)

Asks the store at `<dir>` for its `review-results` and gives them to this
store's `collect-into`, each request on its own store's route, and answers
`collect-into`'s answer with the `no-about` and `missing` lists after it.
It writes nothing in `<dir>`. Like `scope`, its route is `child`: it runs
in the command line's program or as the MCP shell's child, never in a
daemon.

### A review round

The author runs `scope <dir> --cut <cut> --roots <id> --for codex`, and
writes the questions on the letter copy in `<dir>`. The reader, in a
FRESH session pointed at `<dir>` (its MCP shell started with `--store
<dir>`, or `THEOURGIA_STORE=<dir>`), reads the copies at the baseline,
writes a verdict section under the letter copy, with `model`, `approval`,
`sandbox` and `effort`, and a finding section under the verdict for each
finding, each with an `about` edge to the copy it concerns. The author
runs `collect <dir>` and finds the verdict and the findings under the
letter, their `about` edges on the original blocks. A finding's block
still holds at the head of this store when `read --cut <cut>` and a read
at the head agree, for as long as this store can serve the cut.

### Codex as a reader

Codex reaches a store through the MCP shell: `codex mcp add theourgia --
<the command that starts the shell>`, with `THEOURGIA_ACTOR=codex` in its
environment, gives its sessions the store's verbs as tools. Three routes
then carry one message to it, and each one missing leaves the others:

- THE HOOKS. `contrib/codex-hooks.json` runs `contrib/codex-mail.sh` on
  `PostToolUse` (during a turn) and `UserPromptSubmit` (at the start of
  one). The script asks the store named by `THEOURGIA_STORE` for the
  unread mail of `THEOURGIA_ACTOR` and prints the hook's answer naming
  each block, or nothing when there is none. Copy the file to
  `~/.codex/hooks.json` with the script's path filled in.
- THE DOORBELL, for a Codex idle at its prompt, where no hook runs.
  `contrib/doorbell.sh <store> <actor> <tmux-target>` subscribes to the
  store's changes and, for every block added, or whose `to` changed, that
  is addressed to `<actor>` at the frame's cut, types one line into the
  tmux pane `<tmux-target>`: `mail: block <id> at <cut>`. The message
  itself never travels this way; the reader reads it at that cut.
- THE READER'S OWN QUERY, the goal above, at any time.

`contrib/AGENTS.md` is the text for the reader's side: how to read and
answer mail, and the discipline both sides follow.

### What this does not protect against

The scoped store keeps a COOPERATING reader from seeing more than its
roots -- the authors' notes, ledgers and earlier rounds are not in it. It
does not keep out a reader that opens the main store's directory, states
another actor's name, or reads the filesystem: on one machine the user
owns every session, and an actor is a stated name. That is a cooperation
assumption, not a property of locality. A block's text or a field may
name an id outside the roots as a string. A reader's edits to the copies
are not collected; the baseline keeps the copies as they were written.

### Where this runs

| | this program | a hosted service |
|---|---|---|
| transport | a local socket and the local filesystem | a network socket, relayed |
| participants | sessions on one host, of any vendor | sessions on any host |
| a reader's scope | a scoped store on the local filesystem | a scoped store served to a remote writer |
| liveness | the change stream over the local socket | the change stream relayed |
| more than one machine | asynchronous: a store kept in git is read on any clone | live |

Nothing here puts a network socket into this program. A store cloned by
git onto a second machine is read there; writing to one store from two
machines is not built.

## Making and changing blocks

Every verb here writes, and every write is one request with one answer.
The store's lock is taken for the write and released; nothing is held
across two verbs.

### `init`

    (init ["--template" <name>] ["--template-file" <template-file>])

Creates a store in the directory named by `--store`, and answers with the
store's id and the writer the caller was given. With `--template` (a built-in: `project` or
`memory`) or `--template-file`, it then applies that template, as `template apply` does, and the
answer also carries `(template <name>)` and `(created (<slug> <id>) ...)`. Each relation the template
names that is not one of the six and not reserved is declared `nothing`, as `template apply` does. A
store made without a template has only the six. A template that cannot be
used is refused before any store is made. A filesystem failure while the template is applied is
answered like any other, and the new store may exist; a refusal of the template's roots carries
`(store-created)`. It also writes
`<store>/.gitignore`; see [A store in git](#a-store-in-git).
The answer is `(ok (store <id>) (writer <writer>))`. A directory that already holds a store is refused `(error already-initialised (store <dir>))`, and one holding another instance's writer is refused `(error foreign-writer (writer <w>) (remedy adopt))`.

### `template`

    (template <action> [<name>] ["--file" <template-file>] ["--premises" <datum>])

`template apply <name>` (a built-in) or `template apply --file <template-file>` gives an existing
store a template: it creates the template block and each document root the store does not have,
and answers `(ok (created (<slug> <id>) ...))`. It never changes a block that exists. A root is
found by its `slug` field and its path: exactly one live top-level document with the root's slug at
the root's path is the root, and is kept. Anything else refuses and creates nothing:
`template-present`, `slug-conflict` (two live blocks with the slug), `template-mismatch` (the block
with the slug is not a top-level document at that path) or `path-occupied` (a top-level document
without the slug holds the path). Deleted blocks do not count. A name is only ever a built-in's
name; a file is named with `--file`. Each relation the template names that is not one of the six
with an effect and not reserved, and that the store has not declared, is declared `nothing` in the
same request: `project` declares `documents`.

`template export` prints the store's template block. The template is data in that block: change it
with `write` and `commit` like any block, and insert a new root's document; nothing else needs to
change. A template block that cannot be read (it is not one datum `(template 1 ...)`, it has no
roots, or there are two template blocks) is listed by `conflicts` as `(template <reason>)`, and
every verb behaves as in a store without one. Only a TOP-LEVEL block of kind `template` is the
store's template; one under another block is not read. A template whose roots repeat a slug or a
path, or name a root `template`, does not read either.

Importing a document does not make decisions or tasks: a heading re-imported into `design.md` that
matches an existing decision keeps it a decision, and a new heading becomes a section.

### `insert`

    (insert ("--under" <id>) ("--after" <id>) "--title" <text> ("--text" <text>)
            ("--keywords" <text>) ("--premises" <datum>))

Adds a block under an existing one. Without --under the block goes under root.
`--after` places it among that parent's children; without it the block goes
last. `--text` gives the block its `src` in the same request.

`--keywords` gives it the words it should be found by -- three to eight
of them, comma separated, as the protocol above says. NEVER: The value is
stored as TEXT, exactly as it was written: `read` gives back the same
string, spacing and commas included. Splitting it into words happens in
`search`, which is the only reader that needs them; a store holding a
normalised form could not give back what was sent. `set <id> keywords
<text>` changes it afterwards.

### `set`

    (set <id> <field> <value> ("--if-unchanged" <version>) ("--based-on" <version>)
         ("--premises" <datum>))

Replaces one field of one block. `--if-unchanged` makes the write
conditional: the store refuses it if the block has moved on from that
version, so a caller that read, thought, and came back cannot overwrite
what happened in between.
A refused `--if-unchanged` answers `(error changed (current <version>))`. `--based-on <version>` also checks the version, but its refusal is `(error stale-baseline (block <id>) (based-on <version>) (now <version>) (since ...))`. Its `since` lists the most recent records that touched the block -- not only those after the named version, which `set` does not use to select them -- at most eight: the newest first, then the rest oldest first. When more were left out, the refusal also carries `(truncated #t)` and a `(retrieve (log <id>) (read <id>))` clause; when none is listed, it carries `(reason candidate-set-changed)` and `(conflicts <id>)`. Given with no `<value>`, `set <id> <field>` makes the field absent.

Most fields take the text as given. `kind` does not: it is a symbol from a
fixed set -- `code`, `section`, `file`, `doc`, `library`, `decision`, `task`,
`template` -- and a
spelling outside that set is refused, with the legal set in the answer, rather
than stored. A kind stored as text would match nothing and the block would
simply stop behaving like what it said it was. The same set applies to a kind
written through `batch`, and to nothing else: a record already in a store
keeps whatever kind it carries, including one a later version introduced.

`class` is such a field too: a word from `observation`, `inference`, `ruling`,
`verification`, `external`, converted and refused by the same rule as `kind`, with the legal set in the refusal
(`(error bad-request class-not-known (class "<spelling>") (known (...)))`). It says what
kind of statement a block is; see "Class and validity".

### `move`

    (move <id> <parent> ("--after" <id>) ("--premises" <datum>))

Re-parents a block. `root` is spelled as the word, not as an id. A block
cannot be moved under itself or under one of its own descendants: that
move is refused as `(error would-cycle (id <id>) (parent <parent>)
(through (<parent> ... <id>)))`, naming the chain, and nothing is
written. Two writers whose moves are each legal on their own can still
make a cycle together; that one is shown by `conflicts` after the merge.

### `del <id>`

    (del <id> ("--premises" <datum>))

Marks a block deleted. The block and its history stay in the log -- what
changes is what the outline and the reads answer.
The deletion is permanent: nothing brings a deleted block back.

### `link <from> <rel> <to>`

    (link <from> <rel> <to> ("--premises" <datum>))

Adds a typed edge between two blocks. `<rel>` is a name of the caller's
choosing; the store does not interpret it. The answer carries
`(matched 1)` when that edge already existed and `(matched 0)` when it is
new.

Four names are reserved for facts an editor supplies (see "Derived data
from an editor"): `ref`, `uses`, `calls` and `guards`. `link` and `unlink`
refuse them with `(error reserved-relation (relation <rel>))`. A record
written under one of them before they were reserved still applies, and
`check` names it.

### `unlink <from> <rel> <to>`

    (unlink <from> <rel> <to> ("--premises" <datum>))

Removes that edge. NOTE: The three positionals are the same three `link`
takes, in the same order, and getting them out of order is not an error
the store can see -- but the answer says what it found: `(matched 1)` when
the edge was there, `(matched 0)` when it was not. A `(matched 0)` unlink
is still recorded: an edge another writer made that this one has not seen
is not removed by it.

### `relation`

    (relation <name> ("--as" <kind>) ("--from" <selector>) ("--to" <selector>) ("--retire")
              ("--premises" <datum>))

Declares what a relation name does in this store. `--as` takes one of the six
relations with an effect (`supersedes`, `refutes`, `depends-on`, `implements`,
`verifies`, `conflicts-with`): the name's edges then follow that relation's
rules, and a reason they cause names the relation. `--as nothing` declares a
listed edge with no effect. `--retire` makes the name a plain edge again. The
six are never declared: `(error relation-is-built-in (relation <name>))`; a
reserved name is refused `(error reserved-relation (relation <rel>))`; a kind
outside the seven is refused `(error bad-request kind-not-known (kind
"<spelling>") (known (...)))`.

`--from` and `--to` say which blocks each end is meant for, as the clauses
of a selector: `(kind doc)`, or `(kind doc) (field slot "result")`. They are
stored with the declaration, printed by `query --relations`, and held: a write
that adds an edge under the name whose end the selector does not match, or that
changes the kind or a selector's field of a block at such an edge, writes
nothing and is refused with every mismatch in one answer,

    (error bad-request relation-endpoint
      (failures ((relation <name>) (edge <from> <name> <to>) (end from|to) (expected <selector>)) ...))

judged on the state the write would produce, so a block created and linked in
one `batch` is judged with the kind the batch gives it. A query that cannot be
evaluated while they are judged refuses the write `(error refused
relation-endpoint-unevaluable (reason <the refusal>))`. An edge that predates
the declaration is listed by `check`.

A declaration is a record: a replay, a rebuild and a snapshot give the same
table. Its value is the whole declaration -- the kind and both selectors, or
retired. The same value as the one in force answers `(ok (unchanged))` and
writes nothing (a retirement of a name with none in force too); another value
is a new record, in force from it on, and an edge linked before it takes its
effect from then on. Two writers who declare one name differently without
seeing each other, or retire it beside a declaration, leave it CONTESTED,
whatever the order the records arrive in: it has no effect, `conflicts` lists
it with every surviving declaration, and `query --relations` prints it
`(contested)`, until a writer who has seen them all declares it again. A
tracked request -- one sent with `--req`, a tracked `batch`'s items and a
plan's members among them -- writes a declaration as given, since its
receipt counts each record; the same value written again changes nothing,
agreement being by value.

### `rule`

    (rule <name> ("--on" <kind>) ("--where" <goal>) ("--must" <goal>) ("--must-not" <goal>)
          ("--builtin" <name>) ("--retire") ("--premises" <datum>))

Declares a rule of this store: a statement about the blocks a write touches,
written as goals of the query language (see `query`) in which `?w` stands for
the block. `--on` names a kind the rule is about, once per kind; `--where`, when
given, selects among those blocks (a block the selector does not match is not
judged); `--must` says the goal has at least one row for the block, `--must-not`
that it has none. Exactly one of the two. `--builtin citation-coverage` enables
the one built-in rule by name; `--retire` ends a rule.

    rule review-cites --on doc --where '(field ?w "slot" "review")' --must '(edge ?w cites ?s)'

The goals are checked when the rule is declared, as a query's are: an unknown
relation, a wrong arity or a misplaced term is refused, and so is a relation
that reads anything outside the store's records -- `score`, `uses-name`, `def`
and the rules over them (`def-for`, `ambiguous`): `(error bad-request
rule-relation-not-allowed (relation <r>))`. A rule's answer is then a function
of the records and the rules alone. A kind outside the store's kinds is refused
`kind-not-known`. A rule is of one of two classes, decided by its goals and
stored with it: a WRITE rule names a fact only a rule check has -- `kind+`,
`field+`, `edge+`, `edge-kind+` (the write's own view of the blocks it
touches), `cited`, `unread`, `receipt-carried` -- and a STATE rule names none.

A rule is a record, its value the whole rule in one form:

    (rule <name> ((class state|write) (on <kind> ...) [(where <goal>)] (must <goal>)|(must-not <goal>)))

or `((builtin <name>))`, or `retired`. A `rule` intent in a `batch` is written
only in that form, the class included, and one in any other is refused
`(error bad-request rule-not-in-its-form (form <the form>))`. Liveness and
conflict are a declared relation's: the same rule again answers `(ok
(unchanged))` and writes nothing, another value replaces the one in force, and
two writers who declare one name differently without seeing each other leave it
CONTESTED -- not evaluated, and listed by `conflicts` as `(rule-contested <name>
(candidates ...))` -- until a writer who has seen both declares it again. A
tracked request (`--req`, a tracked `batch`'s items and a plan's members) writes
a rule as given, since its receipt counts each record, as it does a
declaration. A rule's value may not hold a plan's marker `("#%new" <k>)`, and is
plain data: a goal written with a datum label (`#0=`), which makes shared or
cyclic structure, is refused. `?w` is bound to the target before `where` and
the goal are asked, so a test may name it first. A
replay, a rebuild and a snapshot keep the rules. `describe`, answered by the
daemon, lists the rules in force as `(declared-rules (<name> <value>) ...)` when
there is one; `check` audits them (see `check`).

A RULE JUDGES EVERY COMMITTED WRITE MADE AFTER IT, on every route -- the verbs
that write, `batch`, `commit`, the imports, `def`, `template apply` and the
completion of a plan already written. A write's TARGETS are the blocks whose
records it adds: created, set, moved, and either end of a linked or unlinked
edge, live after the write (a block it deletes is no target). A delete that
takes effect unlinks every edge of its block as far as a reader of edges can
tell, so the live far
end of each of the deleted block's edges, in either direction, is a target too:
a review whose only cited result is deleted is judged in that write, as it is
when the edge is unlinked. A rule applies to
a target whose kind, after the write, it lists, and that its `where` selects;
`--must` holds when its goal has a row with `?w` bound to the target,
`--must-not` when it has none. A STATE rule is asked of the state the write
would produce; a WRITE rule's plain facts read the state before the write, and
its `+` facts the state after, for the targets; `cited` is a `depends-on` edge
from a target the write adds, `unread` a cited block the write's `--premises`
does not hold, `receipt-carried` that the write carried `--premises`. The state
the write would produce is the store's own: the write runs once against a copy
of the store's state with nothing written, is judged, and then runs for real.
A write that fails a rule writes nothing and is refused with every failing pair,
targets by id, rules by name:

    (error refused rule-violation
      (failures ((rule <name>) (block <id>) (goal <goal, ?w bound>) (expected some|none) (rows <n>)
                 [(witness <row> ...)]) ...))

A `--must-not`'s witness is at most ten of its rows, the count exact. A rule that
cannot be evaluated -- one budget of query work for the whole write, or a query's
refusal -- refuses the write `(error refused rule-unevaluable (rule <name>)
(block <id>) (reason <the refusal>))`, never as ok. A created block is named by
the id the write would have given it. Every route answers these refusals as they
are, as it answers a premise's; a `batch` does not put them in its items, a
`template apply` not in `template-apply-failed`, an `import-md` not in its
`import` clause. A contested or retired rule
judges nothing, and a rule judges no write made before it. A write is judged by
the rules and typed relations in force BEFORE it: one that declares or changes
a rule or a relation is judged by those already there, and the writes after it
by what it declared.

`--builtin citation-coverage` is the rule
`(must-not (and (receipt-carried) (cited ?w ?s) (unread ?s)))` on every kind: a
write that carried `--premises` and links a target, by a relation of kind
`depends-on`, to a block its premises do not hold is refused, the witness rows
`(citation-not-read (block <id>) (cites <id>))`. A write without `--premises` is
not covered; a project that wants the receipt always carried declares
`(must (receipt-carried))` beside it. The project template declares it as
`cover`; a store made otherwise declares it with `rule cover --builtin
citation-coverage`.

A store with no rule and no typed relation makes no copy and judges nothing:
every write is as it was.

### `def`

    (def <name> ("--under" <library>) <source> ("--premises" <datum>))

Defines one datum by name inside a datum library block, adding it after the library's last child. `<source>` must be exactly one form, and that form must define `<name>`; otherwise it is refused `expected-one-form` or `name-mismatch`. Without `--under`, the store's only datum library is used; if there is not exactly one, the request is refused `library-required`. A name one of the library's children already defines is refused `(error name-exists (name <name>) (ids (<id> ...)))`: `def` does not replace a definition.

A datum block takes no draft: `write` and `commit` refuse one with
`draft-on-datum-unsupported` and `(use def)`, because a draft is text and would
land as a src beside the body that export, `whereis` and `eval` read.

### `outline`

    (outline ["--depth" <n>] ["--with-keywords"] ["--with-signatures"])

Prints the store's block tree as indented text, answered as `(ok (text "..."))`: one line per block, `- <id>  <title>`, indented two spaces per level. `--depth` stops at that many levels.
A row whose position is in structural conflict carries the conflict's mark after its title, and a row that is also an orphan carries `orphan`. Orphans the tree did not draw are listed after it under `orphans:`, each with its subtree, under the same `--depth` limit, so `--depth 0` prints nothing. A `--depth` that is not a count is answered with the usage form.

`--with-keywords` appends `  [<keywords>]` to each row that has them. NOTE:
A block without the field prints no brackets: empty ones would say the
writer chose no keywords, which is a different thing from a store written
before the field existed. Without the option the listing is unchanged.

`--with-signatures` appends `  :: <signature>` to each row an editor
supplied a signature for (after the keywords' brackets when both are asked
for), from the committed store's facts. Once a table was read the answer
ends with `(stale <n>)`, the signature facts it could not use, and
`(via ...)` for the signatures it printed.

## Drafts, and committing them

A **draft** is a block's proposed next version, held under a writer's own
name and visible to nobody else until it is committed. A writer with
drafts has a **working view**: the committed store, with its own drafts
laid over the blocks they cover.

NEVER: **A writer id is held by one live agent at a time.** Two agents may
use one id across TIME -- a later session binds the same id and its
`drafts` shows what the earlier one left -- but not at the same moment.

NOTE: **If two do hold it at once, the later write replaces the earlier one
and the core does not say so.** Measured: two clients writing a draft on
one block under one writer id; the second write answered `ok`, and the
first client's own `drafts` then reported the SECOND client's version as
its own. Nothing in any answer distinguishes that from a normal write.

**So a second process that wants to change the same draft takes a copy
of it** rather than sharing the id, on the same baseline:

    theourgia read <id> --working-info --writer w1
    theourgia write <id> "<text>" --writer w2 --based-on <based-on> --working-cut <cut>

The read answers `(projection working <writer> <id> <version> <based-on>
<cut> "<text>" "<prefix>")`, and the write records w2's draft on that
baseline: its answer names it, `(based-on <based-on>)`. `--based-on`
without `--working-cut` is refused `(error invalid-working-baseline (reason
cut-unusable))`, and a writer that already holds a draft of the block keeps
that draft's baseline. `restore` does not make this copy: it reads only the
caller's own revoked drafts. From there each writes its own, and the two
meet at `commit` through `--based-on`.

NEVER: **There is no machinery behind this rule** -- no record of who holds an
id, no lock, and nothing refuses a second process. It is a convention,
stated here because the failure it prevents is silent.

### `write`

    (write <block> <bytes> ("--writer" <name>) ("--based-on" <version>)
           ("--working-cut" <cut>) ("--working-parent-writer" <name>)
           ("--working-parent" <version>) ("--rebase"))

Puts a draft in that writer's slot for that block. A draft's version is
the name of its content -- `sha256(bytes || based-on || cut)` -- so the
same bytes written twice are the same version. A write into a slot that already holds a live draft keeps that draft's `based-on` and cut. `--rebase` moves a draft onto the committed version the caller merged onto, which it must name with both `--based-on <version>` and `--working-cut <cut>`, or it is refused `(error bad-request rebase-needs-baseline)`. A named baseline that cannot be checked is refused `(error invalid-working-baseline (reason cut-unusable|block-not-at-cut|hash-not-at-cut))`. The answer is `(ok (saved <block>) (writer <w>) (version <v>) (based-on <v>))`.

A draft is text, and a datum block's code is its body, not a src: `write` on a
block whose mode is `datum` is refused `(error bad-request
draft-on-datum-unsupported (block <id>) (use def))`, and `commit` refuses a draft
whose block has become a datum block the same way, naming the first such block
and writing nothing. No other write leaves a datum block holding a src either:
`set <id> src <text>` on a datum block and `set <id> mode datum` on a block
holding a src are refused the same way, and an `insert` whose fields carry
`(mode . datum)` and a src is refused the same way without the block clause,
since it has no id yet. A `batch` is refused whole, before anything is written,
when it holds such an intent or a pair of intents that could leave a datum block
holding a src, in any order: a src given to a block another intent makes datum,
or mode datum on a block another intent gives a src. A block the batch inserts
is addressed with `(from k)`; a bare id the store does not hold counts as datum
when an insert of the batch carries mode datum, and as holding a src when one
carries a src. The batch is read as a set, so it refuses more than running it
would: taking a block's src away and then making it datum takes two requests,
and a batch that would have stopped earlier on another refusal answers this one.
(`set <id> src` with no value still runs and takes away a src written before
the rule.)
`restore` of a revoked draft on a datum block is refused the same way and
writes no draft. The refusal points to `def`, the verb
that writes a datum definition.

### `restore`

    (restore <version> ("--writer" <name>))

Takes a version's bytes out of the log and puts them back as a draft.
NOTE: It is its own verb rather than a flag on `write`, because it takes a
version and NO bytes: the bytes come from the log.

A version is looked up in that writer's own revoked consumptions -- the
drafts a commit took and a later retraction gave back -- so `--writer`
selects whose list is searched, and the same version can be unknown to
one writer and restorable by another.

### The working view on disk

`export-md --working` and `export-code --working` write the working view:
the committed side and the drafts are the ones `eval --working` uses (no
`--cut`, no `--latest`). Two things differ today: a code block's text keeps
its stored bytes in the files where eval reads it as a string, and a datum
block's draft -- one written before `write` refused a datum block -- is written
as its body, which eval does not do. The
committed side is PINNED the same way as eval's -- it
is the state at the join of the writer's own drafts' cuts, so a commit
another writer made after those drafts is not in the files -- and every
block with a draft is written as that draft. A writer with no drafts gets
the current committed state, byte for byte what the plain export writes.

A successful answer is the plain export's answer with two clauses after
it: `(cut <cut>)`, the committed cut the view stands on, and `(working #t)`.
A refusal carries neither.
Without `--working` neither clause is there and the answer is unchanged.

A draft replaces a block's text; it cannot add a block (`write` refuses an
id the store does not hold), so the files hold the committed blocks and no
others. `--cut` is not accepted by the export verbs.

### `drafts`

    (drafts ("--writer" <name>))

Lists that writer's live drafts. Each carries `block`, `writer`,
`version`, `based-on`, `now` (what the committed block is at this
moment), `fresh` (whether `based-on` is still `now`) and `unchanged`
(#t when the draft is fresh and its bytes are the same as what is committed; a stale draft is never unchanged).
After the drafts it lists the writer's revoked consumptions, drafts a commit took and a retraction gave back, whose slot is empty now: `(revoked (block <id>) (writer <w>) (version <v>) (based-on <v>) (plan-event <e>))`.

Once an editor has supplied diagnostics for the writer (see "Derived data
from an editor"), each draft also carries `(diagnostics <n>)`, the
diagnostics on its block that are still fresh in the writer's working view,
and the answer ends with `(stale <n>)` and `(via ...)` for them. A writer with
no such table gets the answer it always got.

### `diagnostics`

    (diagnostics ("--writer" <name>))

It answers:

    (ok (items (diagnostic <id> <severity> "<message>" (at <start> <end>)) ...))

The diagnostics an editor supplied for this writer (`supply diagnostics
--for <writer>`), judged against the writer's working view. `<severity>` is
`error`, `warning`, `information` or `hint`. `(at <start> <end>)` is a byte
range of the block's own src, so an edit of another block does not move it;
a range the editor gave inside a marker line, or across two blocks, is
`(at unmappable)`. Items are ordered by block and then by start, an unmappable one first among its block's. Once a table
was read the answer ends with `(stale <n>)`, the diagnostics dropped because
what they were computed from has changed, and `(via ...)` for those it lists.

### `discard`

    (discard <block> ("--writer" <name>))

Drops that writer's draft for that block. The committed block is
untouched.

### `commit`

    (commit (<block> ...) ("--writer" <name>) ("--working-version" <block>=<version>)
            ("--premises" <datum>))

Turns drafts into committed versions. Naming no block commits all of that
writer's drafts. `--working-version` may be given once per block to say
which version of the draft is being committed; with exactly one block
named, the bare version may be given.
A commit that carries `--req` must name a version for every block it consumes, or it is refused `(error bad-request req-needs-versions (blocks <id> ...))`. A named block with no draft is refused `(error no-draft (blocks <id> ...))`, and a draft that is no longer the version named is refused `(error working-version-changed (blocks ...))`. A draft whose baseline the block has moved on from is refused `stale-baseline`. A commit that repeats a request this store has already applied is answered from that request's record, without these checks: its `items` hold `(ok (replay #t) (event ...))`. What makes two requests the same, and what `req-mismatch` answers, is in *Requests and replay* below; for `commit`, the blocks are compared as a set, so naming them in another order is the same request. Every refusal that `commit` itself makes carries `(usage ...)`, the commit form; when the store missed a writer, an `(incomplete ...)` clause follows it. The checks the dispatcher makes before it -- among them the transport options, the store's presence and the `--req` and `--cursor` rules -- answer without it: `commit --req <id>` with no `--cursor` is `(error bad-request req-without-cursor)`.

NOTE: **The answer may carry `(behind ((<writer> . <seq>) ...))`** -- other
writers who landed after this writer's drafts were taken. It is
informational, and it is **absent** when nothing moved rather than
present and empty: a field that is always there says nothing.

Beside it, over the same writers, `(behind-records ((<writer> . <n>) ...))`
says how far: `<n>` is that writer's records since the baseline -- its
current `<seq>` minus the one the join of the committed drafts' cuts holds
for it, or the whole of `<seq>` when the baseline holds none. `<seq>` in
`behind` is a position, not a count; read `behind-records` for a count.

PREMISES: **what a reader was given is checked when its result is accepted.**
Every committed write -- `insert`, `set`, `move`, `del`, `link`, `unlink`,
`tag`, `batch`, `commit`, `import-code`, `import-md`, `def` and `template
apply` -- takes `--premises <datum>`, a list of `(premise <id> <version>)`
and `(premise (query <goal>) <digest>)`, and the clauses a read answers,
given back as they came: `(versions ((<id> . <version>) ...))`, `(cut ...)`
and `(receipt ...)`. A `(receipt ...)` clause given alone, exactly as a
read printed it, is taken as the list of its forms. A form the store
cannot check is refused
`(error bad-request premise-not-understood (form "<spelling>"))`, and two
versions for one block `premise-inconsistent`, before anything else of the
request is looked at.
The set is checked once, under the store's lock, before the first record
of a fresh request; when it no longer holds, the request writes nothing
and is refused with every premise that failed, in the order given:

    (error premise-changed
      (block <id> (expected <v>) (current <v> | deleted | unknown) [(since (<writer> . <seq>) ...)]) ...
      (query <goal> (expected <digest>) (current <digest>)) ...)

`since`, when the set carried a cut, names the events that changed the
block after it. A query that cannot be evaluated is `(error
premise-unevaluable (query <goal>) (reason ...))`. A request's identity
never includes its premises, so the same request retried with a new
receipt is the same request. A replay, or the completion of a request whose
plan was already written, is not checked again, and its answer says so:
`(premises not-checked (reason replay | not-fresh))`. So does a request
that fails after its write began and before the check was asked; one that
fails before it began, a lock that cannot be taken among them, wrote nothing
and carries no clause. A commit that writes nothing asks the same check of
the state it read.

A WRITE IS JUDGED IN THIS ORDER, and refused at the first that fails: the
request understood; its premises (`premise-changed`); the verb's own check
(`stale-baseline`); the typed relations' ends (`relation-endpoint`, see
`relation`); the rules (`rule-violation`, `rule-unevaluable`, see `rule`). A
write that fails its premises and a rule answers `premise-changed`.

## Importing, exporting, and splitting

### `import-md`

    (import-md <dir> ("--allow-delete") ("--premises" <datum>))

Reads a directory of Markdown into the store. Without `--allow-delete` a
file that has disappeared from the directory leaves its blocks alone: the
other files are still imported, and the answer lists the absent ones
under absent, `(import <results> (absent (files (<path> ...))))`. With
`--allow-delete` those documents are deleted after the imports, and the
answer lists the ones whose deletion ran under
`(deleted (files (<path> ...)))`. Either clause is there only when it has
a file to name. A section that has disappeared from a file that is still
there is refused as `would-delete` until `--allow-delete` is given.

A file that is not UTF-8 text, or that holds a NUL byte, is skipped by the
rule `import-code`'s text mode uses: it is not imported, and the answer
lists it, in the order the directory was walked, under
`(skipped (<path> ...))`, after the absent or deleted clause and only when
something was skipped. A skipped file touches nothing: a document whose
file it is keeps its blocks, and is neither absent nor deleted.

Each heading in a file that is already in the store is matched to a
section this way: a heading preceded by a `--with-ids` marker is the block
the marker names; the other headings are paired with the document's
remaining sections by position when the counts are equal and every pair
has the same level and title; otherwise each is matched by its level and
title, the body deciding between sections that share both; a heading
matched by none of these is a new section. So a heading edited in the file
is a new section, and the section it replaced is missing: `would-delete`
until `--allow-delete`, which deletes it. To rename a section and keep its
id, `set` its `title` in the store, or edit a file exported `--with-ids`. A
block of another kind under the document (a `code` block, say) is written
into the file by `export-md`, and a heading matches it only when the
heading and body are exactly what the export wrote for it; it is never
missing. A matched section is moved where the file puts it, under the
heading above it and in the file's order, when the file arranges it
differently from what `export-md` writes; a nesting the heading levels
cannot show (a section moved under another of the same level) is kept
through an untouched export, and a new section written right after such a
section goes beside it, under the same section. Before a missing section is
deleted, the blocks under it that the file does not describe are moved to
its nearest ancestor that stays (a nested document whose own file is there
goes to the top level), and the `would-delete` refusal names them under
`(holds (<id> ...))`.

### `export-md`

    (export-md <dir> ("--with-ids") ("--working") ("--writer" <name>))

Writes the store out as Markdown. `--with-ids` writes each section's id
before its heading, and a document is found again by its path, so the
result can be imported back onto the same blocks.

`--working` writes the writer's working view instead of the committed store
(see "The working view on disk" above); `--writer` selects whose drafts, with
`--working`, and is accepted and ignored without it.

The answer counts the files written. Every block this projection should have
written and did not is listed after that count, with a reason:
`kind-not-a-symbol` for a kind stored as text, `kind-absent` for a block with
no kind at all, `not-in-any-document` for a block that belongs to no document
-- which happens when blocks have been moved into a cycle, since the reduction
resolves that by relocating them -- and `path-conflict` for a document whose
`path` another document already claimed, which also names the winner:
`(<id> path-conflict (with <id>) (subtree <n>))`. Where two documents claim
one path the first by id is written, so which one survives does not depend on
the order the blocks happen to come back in. A caller reading `ok` and a
number is never being told less than the whole story.
The directory must already exist: `export-md` and `import-md` refuse one that does not with `(error projection-invalid (reason not-a-directory) (dir <dir>))` and do not create it. A file that cannot be written stops the export with `(error unreadable (path <path>) (reason "...") (written (<path> ...)))`, which names the files already written.

A document's `path` is checked before anything is written, by the same rule
the code projection uses: it must be relative and non-empty, with no `.`,
`..` or empty component, no NUL and no backslash. A path that fails, or that
leads out of the directory you named once symlinks are resolved, is not
written, and neither is its subtree. The export still answers `ok` and writes the other documents; the document is listed in `skipped` as
`(<id> path-not-usable (path "<path>") (subtree <n>))`, with the path quoted back as you gave it.

Two documents conflict when they would write the same FILE, which is asked of
the filesystem rather than compared as text: on a volume that folds case
`doc.md` and `DOC.md` are one file and one of them is reported, and on a
volume that does not they are two documents and two files.

Each entry carries the number of blocks below it that went unwritten too:
`(<id> <reason> (subtree <n>))`. When a document cannot be written its whole
subtree goes with it, so the cause is named once rather than a line per block,
and entries never nest -- which means `1 + n` added up over the entries is the
number of blocks lost, each counted once.

Blocks that belong to another projection, such as `code`, are not listed: they
were addressed elsewhere, not skipped.

A block that WAS written but held something its file could not carry is named
in a clause of its own, after `skipped`: `(fields-not-written (<id> front) ...)`,
one entry per written block whose stored front matter reached no file. Only a
top-level document's front matter is written, at the top of its file, so this
names any other written block that holds one: a block of kind `doc` below the
root (see `read`), or a section whose front was set with `set`. It is not in `skipped`,
because the block's body was written and the `1 + n` sum over `skipped` counts
lost blocks only. The clause is absent when there is nothing to name.

### `import-code`

    (import-code <dir> ("--allow-delete") ("--datum") ("--symbols" <symbols-file>) ("--premises" <datum>))

Reads a directory of source into the store. `--datum` reads it as data --
one block per top-level form -- rather than as text. With `--datum`, the
whole-line ; comments directly above a form become its doc; a ; comment
inside a form is dropped, and the answer warns with its line and column. A
#| |# block comment, and any comment inside a datum discarded with #;, is
dropped with neither. With `--datum`, only Scheme files are read: those the
language table gives to Scheme by extension (ss, sc, scm, sls, matched
exactly); every other file the directory walk returns (it does not
enter a name that starts with a dot) is listed, in the order it was walked,
in the answer's skipped clause, which is there only when something
was skipped. A file the reader refuses is named in the refusal's path
clause. Text mode, without `--datum`, skips a file that is not UTF-8 text
or that holds a NUL byte, and lists it in the same skipped clause; it is
decided by the bytes, not the name, so a source file in a legacy 8-bit
encoding or in UTF-16 is skipped and listed, not imported, unless its
bytes happen to be valid UTF-8 with no NUL. A UTF-8 byte-order mark at the
start of a file is text: the file is imported, its bytes stored as they
are, mark included. With `--datum` a file that begins
with a mark is refused `invalid-utf8`, as it always was.
In text mode, a file that carries the projection header `export-code` writes is matched back to its file block and children. A child the file no longer holds is refused `(error projection-invalid (reason would-delete) (ids (<id> ...)))` until `--allow-delete` is given, which deletes it. A file with no header, whether hand-written or exported `--raw`, is always imported as a new file block, even when another block already holds its path. `--allow-delete` has no effect with `--datum`.

**`--symbols`: a first import split at an editor's symbols.** Without
markers a text file is imported as one block; `split-suggest` writes a
review copy with marker lines, and importing that copy splits it.
`--symbols <symbols-file>` takes the manual step out of a FIRST import: a
file the symbols file names, and that carries no marker line, is split at
its symbols into code blocks under its file block, exactly the blocks a
marked import of the same cuts makes. A file the symbols file does not name
imports as before. A file that carries marker lines -- one `export-code`
wrote, whose file block exists, or a review copy -- follows its markers and
nothing else: identity is proved by markers alone, and names and order
still only suggest. Such a file is listed in the answer's
`(symbols-ignored (<path> ...))`. A code block's name is derived from its
source, as for any text block; the symbol's name is not stored.

The symbols file is `split-suggest`'s, one section per file: each section
is a header that names its file by its path under `<dir>`, as the walk
spells it, then that file's symbol lines.

    (symbols (path "<path>") (digest "<sha256 of the file>") (source (vscode "<version>" "<languageId>")) (top-level #t))
    (symbol <start> <end> <kind> "<name>")

Every start is a cut, and every end is taken to the first line start at
or after it, or the file's end, so what follows a symbol on its last line
stays with it; an end already at a line start or at the file's end stays
where it is. Ends
are normalised and starts are not: a start must already be the first byte
of its line, or the file is refused `symbols-not-a-line-start`. What
lies before the first symbol, between two, or after the last is a block of
its own: the blocks' bytes run together to the file. A cut at the end of
the protected prefix (a shebang, a coding line, a byte-order mark) is
dropped, the prefix staying with the first block.

A file's section is checked with the file, as `split-suggest` checks its
symbols file (`symbols-stale`, `symbols-empty`, `symbols-past-end`,
`symbols-not-a-boundary`, `symbols-unordered`, `symbols-overlap`,
`symbols-not-a-line-start`), and then every cut is put to the language's
scanner. The first failure, in this order, refuses that file:
`(error symbols-unchecked (reason no-language-entry | no-suggest-profile))`
when no scanner can say what is top level in it; `(error symbols-in-prefix
(at <n>))` for a cut inside the protected prefix; `(error
symbols-unscanned (reason <r>) (at <n>))` when the scanner cannot follow
the file (`unbalanced`, `unclosed-quote`, `lexically-uncertain`); and
`(error symbols-not-top-level (at <n>))` for the first cut inside a body,
a string, a block comment or an open bracket. Two more cuts are inside: one
after a line whose code ends in a continuation token (`=>`, `=`, `+`, `-`,
`*`, `/`, `.`, `,`, `(`, `[`, `{`, `&&`, `||`, `?`, `:`, or the language's
escape, `\` in Python; in the shell only `|`, `&&`, `||` and `\`, since `/`,
`=`, `:` and `*` end ordinary shell words such as `cd /`; in Scheme only the escape, since
`-`, `+` and `.` are identifier characters and a line continues only through an
open bracket) -- the code read
without its line comment, and a line that ends in a string, or inside or at
the close of a block comment, not continuing, so `// The end.` before a
definition is no continuation. A token counts only as a whole operator: the
`+` of `x++`, the `.` of `1.` or of Python's `...` (Rust's `0..` does continue), and a line whose last
character the escape makes literal (`echo a\|`, but not `echo \\|`, whose pipe
is real) do not continue. And, in an indented language such as Python, a blank line -- empty,
or spaces and tabs only -- whose next non-blank line is indented, since a
blank line inside a body has no indentation of its own. A blank line between
two top-level definitions is the top level.
The continuation test is not made in Markdown, which is prose. A
refused file is not
imported at all -- no block is written for it -- and the import of the
other files goes on; the answer lists it with its refusal in
`(symbols-refused ((<path> <refusal>) ...))`, with, after the walk's
files, a section that names no text file the walk found, as `(<path>
(error symbols-no-file))`. Both clauses are there only when they list
something, and only on an answer that succeeded, as `skipped` is.

The symbols file itself is read before the directory, and a file that
cannot be read as sections refuses the request: `(error symbols-malformed
(line <n>))` for a line that does not read or is neither a header nor a
symbol line, a symbol line before any header, a path that is not a plain
relative path, or a second section for a path; `(error symbols-empty)` for
a file with no section. `--symbols` with `--datum` is refused `(error
bad-request incompatible-import-options)`.

With `--datum`, a file WITHOUT a projection header, whose path an alive
datum library in the store already holds, updates that library rather
than creating a second one: its forms are matched to the library's children as an
exported file's would be, and nothing is deleted. A child the file does
not hold is kept, and the answer names it in `(kept (ids (<id> ...)))`. A
file WITH a header is the whole library, as before, and a child it omits
is deleted. A path two libraries already hold is refused, and nothing is
written; see "A path several blocks hold" below. The libraries that held each path
when the directory was read are checked again when the import writes; if
they changed, the import is refused `(error stale-baseline (path <path>)
(reason changed-claimants))`.

### `export-code`

    (export-code <dir> ("--raw") ("--datum") ("--working") ("--writer" <name>))

Writes the store out as source. Without `--datum` it writes the text-mode
files -- the blocks `import-code` made without `--datum`; a store with no
text-mode files answers `(ok (files 0))`. `--raw` writes each file as its blocks' bytes run together, with no header or marker lines. Such a file cannot be imported back onto the same blocks: `import-code` treats it as a new file.
NEVER: `--datum` and `--raw` together are
refused with `(error bad-request incompatible-projection-options)`: they
are two different projections and there is no answer to "both".

`--working` writes the writer's working view instead of the committed store,
in any of the three projections (see "The working view on disk" above);
`--writer` selects whose drafts, with `--working`, and is accepted and
ignored without it.

#### A path several blocks hold

An export that meets a path two or more blocks hold -- two alive datum
libraries, or two alive text files -- and a raw `import-code --datum` of
such a path, refuse once, naming every such path in the store:

    (error projection-invalid (reason duplicate-path) (path <path>) (ids (<id> ...))
           (paths ((<path> (ids (<id> ...))) ...)) (remedy del-all-but-one-per-path))

`path` and `ids` are the first one met; `paths` is all of them, as `check`
lists them. The remedy is one `del <id>` for every holder but one, per path.
It does not say which to keep: there is no oldest holder across writers,
and the newer copy may be the corrected one. Tell the copies apart with
`read <id> --recursive` (a plain `read` shows a library's own fields, not
its children's bodies, and an export refuses while the duplicates are
there) before any `del`, which is permanent.

### `split-suggest`

    (split-suggest <file> ("--output" <review-file>) ("--symbols" <symbols-file>))

Proposes where a file could be split into blocks. It writes no record;
what it produces is the review file. The answer is `(ok (working-path <review-file>) (boundaries (<n>
...)) (warnings (...)) (cuts-from <supplier>))`: the byte offsets where the
blocks start, and which supplier chose them -- `regex` when the language's
definition patterns did, or the symbols file's source. A file that begins
with a UTF-8 byte-order mark is cut as it would be without one, and every
boundary, and the offset of an uncertain token, counts the mark. With
`--symbols`, the file's first line starts at byte 0, not after the mark,
so a symbol starting at byte 3 is refused `symbols-not-a-line-start`.
Without `--output`, the review file is written beside the file as `<file>.review-<pid>-<ms>-<n><ext>`. An existing file is never overwritten: an `--output` that exists is refused `(error projection-invalid (reason output-exists))`. A file that changed while it was being scanned is refused `input-changed`.

With `--symbols`, the cuts come from a list of the file's top-level
symbols that an editor collected, in place of the definition patterns.
Only the question "does a definition start on this line" changes hands:
the comment lines above a definition still go with it, and a shebang, BOM
or coding line still stays with the first block, by the same rules as
without it, so where the two suppliers agree they cut in the same place.
A symbol whose start the scanner sees inside a string, a block comment or
an open bracket is not a cut, and the warnings say so, `(symbol-not-top-level
(at <n>))`; nor is a start inside the protected prefix, `(symbol-in-prefix
(at <n>))`. For a file the language table does not know, or whose language
has no suggestion profile, the starts are the cuts as given, with the
warning `(no-language-entry)` or `(no-suggest-profile)`. The symbols' kinds
are reported in the warnings as `(symbol-kinds <kind> ...)`; they do not
choose cuts.

The symbols file holds one datum per line, read as data (nothing in it
runs): a header, then one line per symbol, in the file's order:

    (symbols (digest "<sha256 of the file>") (source (vscode "<version>" "<languageId>")) (top-level #t))
    (symbol <start> <end> <kind> "<name>")

`<kind>` is one of VS Code's SymbolKind names, lower-cased and written as
one word -- `file module namespace package class method property field
constructor enum interface function variable constant string number
boolean array object key null enummember struct event operator
typeparameter` -- and anything else is a malformed line; a producer emits
exactly these. `<start>` and `<end>` are byte offsets into the file exactly as it is on
disk (a byte-order mark and every carriage return counted); `<start>` is
the first byte of its line. The digest is of those same bytes, so a list
made from an unsaved editor buffer, or from a file that changed since,
is refused rather than trusted. A list that cannot be trusted is refused
by name, never folded into one block; the first failure in this order is
the one named:

- `(error symbols-malformed (line <n>))`: a missing header, a line that
  does not read or has the wrong shape, a kind outside the list above, an
  offset that is not an exact non-negative integer, or a start not below
  its end;
- `(error symbols-stale (digest-expected <d>) (digest-found <d>))`;
- `(error symbols-empty)`: a header and no symbols;
- `(error symbols-past-end (at <n>))`: a start at or past the end of the
  file, or an end past it;
- `(error symbols-not-a-boundary (at <n>))`: an offset inside a UTF-8
  character;
- `(error symbols-unordered)`: starts not strictly ascending;
- `(error symbols-overlap (at <n>))`: a range that starts before the
  previous one ends;
- `(error symbols-not-a-line-start (at <n>))`.

## Derived data from an editor

An editor that has the store's source open computes facts the store does
not: a declaration's signature, which function calls which, a compiler's
diagnostics. `supply` keeps such facts in the store's directory, under
`<store>/derived/`, outside the event log: no record is written, the reducer and
`check` never read them, and the store's `.gitignore` keeps them out of
git. They are as fresh as their last supply.

### `supply`

    (supply <kind> <file> ("--for" <writer>) ("--clear"))

`<kind>` is `signatures`, `calls` or `diagnostics`. `<file>` is a supply
file an editor wrote from an `export-code` projection of the committed
store, or, with `--for <writer>`, of that writer's working view
(`export-code --working --writer <writer>`). The answer is
`(ok (supplied (facts <n>) (files <n>)))`: the facts kept, and the files
named in `replaces`, a file whose facts were emptied included.

NEVER: **`supply` is for text-mode code only.** Its facts are about the text
projection `export-code` writes without `--datum`; a datum library is not
projected as text, and the names in it are the store's own to resolve (`eval
--under`, `whereis`). A listed path that only a datum library holds is refused
`(error supply-not-text-mode (file <path>) (block <id>))`, naming the library.

The file is one S-expression per line; an empty line is skipped, and a
line is numbered as the file is. Line 1 is the header:

    (supply <kind> (writer "<writer>") (language "<languageId>")
            (source (vscode "<version>"))
            (files ((<path> "<sha256>") ...)) (replaces (<path> ...)))

`writer` is `-` for the committed store and must be the `--for` given;
`files` is every file the projection wrote, with the sha256 of its bytes;
`replaces` names the files whose facts this supply replaces. Every other
line is one fact, by kind: `signature` and `keywords` lines in a `signatures` supply, `calls` lines in a `calls` supply, and `diagnostic` lines in a `diagnostics` supply, where `<severity>` is `error`, `warning`, `information` or `hint`:

    (signature <id> "<text>" (kind <k>) (depends (<id> ...)))
    (keywords <id> ("<word>" ...) (depends (<id> ...)))
    (calls <from-id> <to-id> (depends (<id> ...)))
    (diagnostic <id> <severity> "<message>" (range <start> <end>) (depends (<id> ...)))

`depends` lists the blocks the fact was computed from, the fact's own
block first. A diagnostic's range is a half-open byte range in the
projected file; the store maps it to the block's own src bytes, undoing
the projection's markers and escapes, or to `(at unmappable)` when it
falls in a marker or spans more than one block.

The store projects the view again as `export-code` would and checks every
listed file against it before it keeps anything. What it refuses:

| answer | when |
|---|---|
| `(error supply-stale (file <path>))` | a listed file's sha256 is not the store's projection of it: a draft written or a commit made since, or a buffer that was not the file |
| `(error supply-not-text-mode (file <path>) (block <id>))` | the listed path is not in the text projection and a datum library holds it; checked with the digests, file by file |
| `supply-malformed (line 1) (reason header)` | line 1 is not a header |
| `supply-malformed (line 1) (reason kind-mismatch)` | the header's kind is not the command's |
| `supply-malformed (line 1) (reason writer-mismatch)` | the header's writer is not `--for` (or `-` without it) |
| `supply-malformed (line 1) (reason replaces-not-listed)` | a path in `replaces` is not in `files` |
| `supply-malformed (line <n>) (reason unreadable)` | the line is not one S-expression |
| `supply-malformed (line <n>) (reason fact-shape)` | the line is not a fact of this kind |
| `supply-malformed (line <n>) (reason depends-not-led-by-subject)` | `depends` does not begin with the fact's own block |
| `supply-malformed (line <n>) (reason unknown-id)` | a block the fact names is in no projected file |
| `supply-malformed (line <n>) (reason dependency-file-not-listed)` | a block it depends on is in a file `files` does not list |
| `supply-malformed (line <n>) (reason own-file-not-replaced)` | the fact's own block is in a file `replaces` does not name |
| `supply-malformed (line <n>) (reason range-outside-file)` | a diagnostic's range ends past its file |
| `supply-malformed (line <n>) (reason id-range-mismatch)` | a diagnostic's range maps to a block other than its `<id>` |

`supply-malformed` answers are `(error supply-malformed (line <n>) (reason
<r>))`. A view the exporter refuses (a path two blocks hold, an unsafe
path) is refused with the exporter's own answer. Nothing is written until
the whole file passes. The first refusal met is the answer, checked in this order: the header, then every line's shape, then the exporter's own answer, then `supply-stale`, then `replaces-not-listed`, then each fact against the view in line order.

Facts are kept per kind, writer and language, in
`derived/<kind>-<writer>-<languageId>.sexp`, with every byte of the writer
and the language outside `[A-Za-z0-9_.]` written `%XX` (so the committed
store's `-` is `%2D`). A supply keeps the facts about the files it does
not replace. `--clear` removes the table the header names and answers
`(ok (cleared (kind <k>) (writer <w>) (language <l>)))`. A table that does
not read, or whose checksum or identity is wrong, is absent as a whole.

A fact is used only while what it was computed from is unchanged: the src
bytes of every block in `depends`, and, for every file those blocks are
in, the language's comment wrapping and the ordered list of the file's
blocks. A fact whose inputs changed is not used, and the answers that
read facts count it. A change in a file a fact does not list is not seen
until the next supply.

Every answer that consulted a table of facts ends, after its own clauses,
with `(stale <n>)` and then `(via <provenance> ...)`, and only then: an
answer that read no table is the answer it was before tables existed.
`stale` counts the stale facts among those that answer consulted -- the
facts about one block for `read --signature`, the calls into one block for
`refs`, the whole table for `outline` and `search` -- so each verb's count
is its own; it may be 0. `via` names the distinct provenances of the facts
the answer used, and may be empty; a stale fact's provenance is never named.
A signature fact's `(kind <k>)` is kept in the table and not shown.
Without `--wire`, a listing (`outline`) prints the two clauses after its
text; an answer that is a list of items (`search`, `refs`, `diagnostics`,
`drafts`) prints only its items, so a reader that takes every line as an
item is not handed one that is not, and the clauses are in `--wire`.

The writer named `-` is the committed store's name in these tables, so the
readers of a writer's own facts -- `read --signature --working` and
`diagnostics` -- refuse it, `(error reserved-writer (writer "-"))`, and
`drafts` for it carries no diagnostics count.

## Checking, snapshotting, adopting

### `check`

    (check)

Reads the store and reports what does not hold together, as `(check (store <id>) (local-writer <w>) (writers ...) (snapshots ...) (registry inside-store|outside-store) (notes ...) ... (verdict <v>))`. Each writer carries its `end`, `torn` and `integrity` notes (`unreadable` where a writer could not be read). Each snapshot is `usable` with its cut, or `unusable` with why, which does not change the verdict. `local-writer` is present only when the store has a writer this copy may write as. A path two alive
datum libraries, or two alive text files, both hold is reported under
`(paths ((duplicate-path <path> (ids (<id> ...))) ...))`, a clause that is
there only when there is such a path. It is not damage -- the log is whole;
what the store holds is two identities for one path -- but an export
refuses it, so the verdict says it. An applied `link` or `unlink` record
under a reserved relation name (`ref`, `uses`, `calls`, `guards`), written
before the names were reserved, is listed under
`(reserved-relations ((<from> <rel> <to> (event <writer> <seq>)) ...))`,
again only when there is one; it does not change the verdict. A live block
whose mode is `datum` and which carries a `src` -- a draft committed onto it
before `commit` refused one -- is listed under `(datum-with-src (<id> ...))`,
only when there is one; nothing that runs or exports it reads that src, and the
verdict is unchanged. The rules in force are audited under `(rules ...)`, only
when there is one: each state rule over every live block of a kind it lists,
`(rule-violation (rule <name>) (block <id>) (goal ...) (expected ...) (rows <n>)
[(witness ...)])` for each block it fails, `(rule-unevaluable (rule <name>)
(block <id>) (reason ...))` when it cannot be evaluated; a write rule, the
built-in among them, listed once as `(rule-skipped (rule <name>) (reason
write-rule))`, since no write is in hand. An edge under a typed relation whose
end does not match its selector is listed under `(relation-endpoints
(relation-endpoint (relation <name>) (edge ...) (end from|to) (expected
<selector>)) ...)`. Neither changes the verdict. The verdict
is `damaged` when a writer's log fails its integrity check, the reduction
could not apply a record, or the registry is inside the store; otherwise
`duplicates` when there is a paths clause; otherwise `ok`. Any verdict but `ok` exits 1. The
way out of `duplicates` is under "A path several blocks hold".

### `snapshot`

    (snapshot)

Writes a snapshot of the current reduction and answers
`(ok (snapshot "<path>") (cut <cut>))`, `<path>` being the new file under `<store>/snap/`, numbered above every snapshot already there. Later opens start from it instead of
replaying the whole log.
A store with no local writer to write as is refused `(error no-local-writer)`; a cut that reaches past what is durable is refused `(error cut-ahead ...)`. It also writes the evidence checkpoint, `request-index.sexp`.

### `adopt`

    (adopt)

Takes over a store that belongs to another machine's registry entry --
after a move or a restore, where the store is here but its recorded owner
is not. When the reason is the store's identity, the answer says which
part of it: `(reason identity) (identity <field>)`, where `<field>` is
`machine`, `device`, `inode`, `nonce`, or `instance-absent` for a checkout
that has no `instance.sexp` yet.
The answer is `(ok (from <old-writer>) (to <new-writer>) (prefix <segment> <offset> <seq>) (reason <r>))`. `<r>` is `identity`, or one of the recovery reasons: `retired` (an adopt left unfinished), `missing-generation`, `registry-ahead` (the log is behind what the registry saw written) or `damage` (the writer's log fails its integrity check). A store that needs none of these is refused `(error not-needed (checked (identity retired missing-generation registry-ahead damage)))`. A store file it cannot read is refused by kind (`owner-unreadable`, `segment-unreadable`, `metadata-unreadable`, `registry-unreadable`) with `(path <p>) (reason <r>)`, and nothing is written.

## A store in git

A store can live in a git repository for backup, restore and moving it
between machines, with one writer. `init` writes `<store>/.gitignore`, and
it keeps out what belongs to this copy of the store rather than to its
history:

    /instance.sexp
    /request-index.sexp
    /snap/
    /derived/
    /writers/*/working/
    /writers/*/draft.lock
    *.tmp-*
    !/writers/*/incoming/*

`instance.sexp` is this copy's identity. `request-index.sexp` is a
checkpoint of the evidence index; it records this instance's files and is
rebuilt from the log, and written again at the next `snapshot`. Snapshots
are rebuilt the same way. `derived/` holds the facts an editor supplied;
the next `supply` gives them back. `supply` adds the line to a store's
`.gitignore` that lacks it, and creates the file when there is none. The working areas and draft locks are one
machine's drafts. The `*.tmp-*` files are the temporaries an atomic write
keeps after a failure, for a reader -- except under a writer's `incoming/`,
which the last line brings back: a candidate staged there and kept after a
failure is evidence a replay is answered from, so it travels with the log.

Everything else travels: `meta.sexp`, `lock`, and every writer's
segments and records (`owner.sexp`, `published.sexp`, `retired.sexp`,
`quarantine.sexp`, `uncertain.sexp`), with `damaged/`, `incoming/` and
`blobs/`. Git carries no empty directory, so those three travel when they
hold something; a missing one reads as empty.

Add the store as a whole, never named files:

    git add <store>

A segment rolls over after 1 MiB or an hour, so a list of named files
misses the segment that rolled since. A store made before this `.gitignore`
existed takes the file by hand.

Restoring or moving is: clone, read, adopt, write. A clone has no
`instance.sexp`, so reads work and the first write is refused:

    (error refused no-instance (remedy adopt))

`adopt` gives the checkout an identity of its own, retires the old
generation of the writer and installs its successor, and writes from then
on land in the new generation. A checkout is adopted this way -- for a
missing `instance.sexp` -- only when that name is absent from the store's
directory, not merely unreadable or a link that points nowhere. The name is asked with an lstat, which needs search permission on the directory, not a listing. If that fails for any reason but absence, `adopt` refuses, `(error metadata-unreadable (path <p>) (reason <r>))`, and writes nothing. (A copy whose `instance.sexp` does not
match, such as the stale copy below, is adopted for that mismatch.)

A request answered before the restore is replayed on the checkout once it
is adopted: a retry of it answers as the original store would, that it has
already run, `(ok (replay #t) (event (<writer> . <n>)))` -- the record is
still in the log, so it is flushed and counted for this instance, even when
its writer is retired or a mirror. Before `adopt` the checkout has no
identity of its own yet, and the retry answers `(error unknown
(replay-barrier-failed ...))`: `unknown` is the store declining to guess,
not a failure of the request; adopt the checkout, or check the store's
contents before sending it again.

A copy that still exists under its old identity keeps writing to the old
generation. Once it pulls the restored copy's commit, it holds the
successor's owner and the predecessor's retirement, and its next write is
refused by name:

    (error refused (instance nonce))

It has to adopt, or stop. Git will also show its own segment as diverged:
two copies writing the same store is not what this supports. Writing from
two machines is a separate design.

The `lock` file is part of the store and is never recreated except by
`init`. If a checkout lacks it, a read in the calling process answers
`absent` on the lock's path; through a daemon, the start fails as
`(error serve-start-failed (kind store-busy) ...)`, which is misleading --
nobody holds the store, the lock file is missing. Either way,
`touch <store>/lock` restores it.

## Requests and replay

The block-editing verbs — `insert`, `set`, `move`, `del`, `link`, `unlink` —
accept a request identity. A client that did not hear the answer can send the
same request again and be told what happened, rather than having to choose
between doing the work twice and not doing it at all.

`batch` carries one too, and is covered below, and so do `commit`, `import-code` and `def`. So does `tag <name>`, which
writes a record — but not `tag` with no argument, which only lists what is
there. Trackability is a property of the **request**, not of the verb's name.

**Every other request refuses the options** — `(error bad-request
req-not-tracked <verb>)`. The store-level verbs and the read-only ones have no
replay to offer, and taking an identity only to drop it would leave a caller
believing it was protected. Refusing says so.
Of these checks, `--req` without `--cursor` comes first: it is `req-without-cursor` on any verb the dispatcher answers, so `req-not-tracked` answers only a request that carries both. `--cursor` alone is `cursor-without-req` on any such verb. Checks the dispatcher makes earlier answer before any of these -- among them an unknown verb, a transport option (`transport-option-in-rpc`), a source it expected on standard input, and a store that is not there (for every verb but `init` and `describe`). `eval` is not one of the dispatcher's verbs: `core.sc` runs it itself, from the command line or as the MCP shell's child, and never sends it to the dispatcher, so it accepts `--req` and `--cursor` as options and checks neither.

    theourgia insert --title "One" --req <id> --cursor <writer>:<seq>

**Both options or neither.** `--req` alone is `(error bad-request
req-without-cursor)` and `--cursor` alone is `(error bad-request
cursor-without-req)`. Neither is accepted quietly, because a caller who supplied
one believes it is protected and is not: the id names the request and the cursor
says where the store stood when the client decided to send it, and the store
needs both to say anything about a request it cannot find.

A write with neither option behaves exactly as it did before and is not tracked.

**The cursor is `--cursor <writer>:<sequence>`** and is parsed by shape.
Anything else — no colon, an empty or non-numeric sequence — is `(error
bad-request malformed-cursor)` (so is an empty writer, or a sequence of more than 18 digits). It is spelled `--cursor` and not `--after`
because `--after` already names the sibling a new block is placed behind, on
`insert` and `move`; one spelling cannot carry both, because the dispatcher
must consume the retry cursor before any verb sees its arguments. A request id is 1 to 64 characters of `[A-Za-z0-9._-]`; any other id is `(error bad-request
malformed-req-id)`.

### What a request is answered with

These are the answer **categories**; each carries further evidence, shown here
abbreviated as `...`. An RPC `batch` answer is wrapped once more, as
`(batch (<answer> ...))`.

| Answer | Meaning |
|---|---|
| `(ok ...)` | the request had not run; it ran now. |
| `(ok ... (replay #t) (event (<writer> . <seq>)))` | this exact request has already run. No new record was appended. The event names the record the store is standing behind. |
| `(error req-mismatch ...)` | that id already names a **different** request. No record was appended. |
| `(error cursor-unreachable (after (<writer> . <seq>)) (writing ...))` | the cursor names a position the record could not have been written at, so the store cannot reason about where the request would have landed. |
| `(error resolved-executed ...)` | an operator has recorded a determination for this request, and it says the work was done. |
| `(error unknown <why>)` | the store cannot tell whether the request ran. It never guesses. |
| `(error not-written reserved-not-written (sequence <n>))` | the append was reserved and nothing reached the log. Sending the request again runs it. |
| `(error incomplete-request ...)` | some of the request's sub-operations are on disk and its plan record cannot be found to finish it from. |

`unknown` always says why, and the reason is something an operator can act on.
They fall into three families, and this is not the whole list — the store adds a
reason wherever it finds a new way to be unsure:

- **history in doubt** — `(range-overlaps (<writer> <from> <to>))`: a stretch the
  request could have occupied cannot be read, so the store cannot say whether the
  request is in it.
- **metadata unreadable** — `(uncertain-cache unreadable)`,
  `(retirement-unreadable <writer>)`, `(chain-unreadable)`: something the answer
  depends on could not be read.
- **evidence that does not add up**, for a batch — `(receipt-absent)`,
  `(receipt-coverage <i>)`, `(receipt-unlinked <event>)`,
  `(item-index-missing <event>)`, `(plan-order <indices>)`,
  `(evidence-names-missing <i>)`: records were found that no correct execution
  produces, so the store will not guess which story it is looking at.

A store that answered `ok` to any of these would be guessing, and the whole point
of holding a request id is to not have to.

An answer built from history the store could not wholly read carries an
`incomplete` clause, one entry per writer it is missing:

    (incomplete (unreadable (writer <w>) (path <p>) (reason <r>)) ...
                (cut (writer <w>) (path <p>) (reason <r>) (kind <kind>) (after <n>)) ...)

`unreadable` names a writer that could not be read.
`cut` names a writer whose history was cut by damage the store could read --
a segment whose hash or declared range the manifest does not vouch for, a
listed segment that is missing, a record whose check fails or that does not
parse or follow its predecessor, a sealed segment that ends mid-record, a
malformed manifest or retirement -- with the kind of damage (its name is
also the reason) and `after`, the last sequence kept. The records up to
`after` are delivered; nothing after the cut is. A verb that must not act
on a partial history -- `export-code`,
`export-md`, `import-md`, `import-code`, `def`, `snapshot`, `supply` -- refuses instead,
`(error incomplete-reduction (notes ...))` with the same entries, before
it writes a record or a snapshot. A cut refuses nothing else: every other
operation reads a cut store as it always has (a write whose own writer's
history is damaged is refused `integrity`), and every answer given after a
load carries the clause, a refusal included.
A `cut` of kind `segment-unreadable` is the exception: the writer was stopped
by a segment the store could not read (its path and the system's reason are
named), so the records up to `after` are delivered but what lies past it is
unknown, not known to be damaged -- so it refuses wherever `unreadable` does,
not only where a cut does.
`conflicts` lists each cut, a `segment-unreadable` one included, as
`(cut <writer> <path> <kind> <after>)`. The
torn end of a writer's current segment after a crash is recovery, not
damage, and is not reported.

The question is asked of the **successor chain**, not of one writer. A cursor on
a writer that has since been retired is measured against the generations that
succeeded it too, because a request written against a retired cursor could have
landed on any of them.

### An answer is durable when it is given

A request answered `ok` has already been made durable. The cost is **per
request, not per record**: a request of nine sub-operations is flushed the same
number of times as a request of five. A single-intent request runs one barrier;
a tracked batch runs two, because its receipt is made durable before its items
are written.

A replay performs the barrier again before repeating the word, because saying
"that already happened" is the same promise as saying it happened the first
time.

So a crash cannot leave a client holding an `ok` for a record that is not on
disk. It can leave a client holding **no** answer, which is what `--req` is for.

A request of several sub-operations -- a commit, an import, a `def` -- writes a
plan record first and then its members. A crash between them leaves the plan on
disk with members missing, and a retry with the same `--req` finishes it from the
plan's frozen entries, putting what the plan inserts under the blocks its earlier
members made. Before it writes anything, the retry checks that nobody else has
written a block a missing member would write since the request was admitted, and,
for a commit, that the block's hash is still the one the draft started from. If
either fails it writes none of them and answers `stale-baseline`, bare, as a fresh
commit does; and, first, a commit's member that would set the src of a block that
is now a datum block -- a plan written before `commit` refused such a draft -- is
refused as a fresh commit refuses it, `(error bad-request
draft-on-datum-unsupported (block <id>) (use def))`, and nothing is written. Each
refusal comes with a `(completion (plan <event>) (present <index> ...) (of <n>))` clause
naming the members that are applied. The refusal is about the store as it is now:
each retry is judged again, and completes once the other record has been taken
back. A client that will not wait rebases the drafts the answer names and sends
them under a new request id. If a member the retry writes makes another record
applied, the members still missing are checked again before the next one; and if
the plan, or the member just written, is not applied, the retry stops with
`(error unknown (not-applied <event>) (completion ...))`.

Which envelope carries the refusal or the `unknown` depends on whether the retry
has written a member yet. Before the first one, it is the whole answer, bare.
Once the retry has written a member, the answer is a `batch`,
`(batch (<answer> ...) (done <n>))`: the answers of the members this retry wrote,
then, last, the refusal or the `unknown`, with `<n>` counting the `ok`s. That
holds for a `stale-baseline` the retry reaches when it checks the missing
members again after a write as much as for the `unknown`. Either way the command
exits 1.

### `batch`

    (batch <intents> ("--premises" <datum>))

Intents are read from standard input, one wrapping list or several top-level
forms:

    echo '((insert root #f ((kind . section) (title . "One")))
           (insert (from 0) #f ((kind . section) (title . "Two"))))' \
      | theourgia batch
Text that does not read as data is `(error bad-request unreadable-intents)`. Intents given both as an argument and on standard input are answered with the usage line, not with one of the two chosen.

A field is a **pair**: `(title . "One")`. Written `(title "One")` the value is
the list `("One")`, which is a different thing and is stored as one.
`(from <n>)` names the block made by intent `<n>` of the same batch, counted from 0. It is accepted as an `insert`'s or a `move`'s parent or predecessor, and as either end of a `link` or an `unlink`, so one batch can create a block and link it. A reference that is not `(from <index>)` is `malformed-intent` (`back-reference-not-a-form`, `back-reference-not-an-index`), and one naming no intent that made a block is `(error no-such-intent <n>)`.

An intent is the shape the library takes, not a shorthand, and each verb has
its own:

    (insert <parent> <predecessor> <fields>)
    (set    <id> <field> [<value>])          field is a symbol
    (move   <id> <parent> <predecessor>)
    (link   <from> <rel> <to>)               rel is a symbol
    (unlink <from> <rel> <to>)
    (del    <id>)
    (tag    <name>)

Any intent with a subject — `set`, `move`, `del`, `link`, `unlink` — may be
wrapped as `(expect <hash> <intent>)`, which refuses the work if that block's
hash is no longer the one given. `insert` and `tag` have no subject and answer
`(error no-subject)` if wrapped. The hash must be a string: a wrapper holding
anything else is `malformed-intent`, not a wrapper that quietly does nothing.
A hash that no longer matches is answered `(error changed (current <hash>))`, and a subject that does not exist or is deleted is answered `unknown-id` or `deleted`.

An item the store will not accept is answered `(error malformed-intent
(<reason> <clause> ...))` for that item — never raised. The clauses say what the reason is about, such as `(spelling "<the datum>")`, `(argument <position>)`, or `(verb <v>) (given <n>) (needs <m>)`; the item is not sent back. The reason names the
rule, because more than one can apply to the same item and they do not send you
to the same place:

| Reason | |
|---|---|
| `intent-not-a-form`, `verb-not-a-symbol` | it is not a form headed by a verb |
| `too-few-arguments` | shorter than that verb reads |
| `not-an-id` | a value that could not be an id where one belongs |
| `name-not-a-symbol` | a field or relation name that is not a symbol |
| `fields-not-a-list`, `field-entry-not-a-pair`, `field-name-not-a-symbol` | the field collection's shape |
| `field-name-reserved` | `parent` or `ord`, which the store computes |
| `field-name-repeated` | the same field name twice |
| `intent-not-a-proper-list` | the item, or its `expect` wrapper, is not a proper list |
| `expect-too-short`, `expectation-not-a-hash` | the `expect` wrapper |
| `title-has-line-terminator`, `title-has-control-character` | a title must be one line |
| `level-not-a-heading-level` | a `level` that is not 1–6 |

A record the store was going to write but cannot apply is refused the same way,
with the reason from the reducer's own vocabulary — `id-not-a-string`,
`tag-name-not-a-string`, `relation-not-a-symbol`, `parent-not-an-id`,
`ord-not-a-number`, `field-value-not-text`, `kind-not-known`, `symbol-not-wire-safe` — and only the reason is shown, with its own clauses when it has any. Neither the payload nor the item is shown.

A form headed by a symbol this build does not know is a different answer,
`(error unknown-verb (spelling "<verb>"))`: it may be a record shape a newer build writes,
and calling it malformed would send an operator to repair something that only
needs a newer binary.

Note that the empty text is a batch of **no** intents, which writes nothing and
is not an error, while the text `()` is a batch of **one** empty intent, which
is malformed. They are different requests and are answered differently.

A **tracked** batch of more than one item — one carrying `--req` and `--cursor` —
writes a **receipt** first and then its items, and each item is its own request
under an identity derived from the batch's, so a retry can be told not merely
how far the batch got but exactly which items are on disk. Sending a completed
one again appends no record and answers `(ok ... (replay #t) (event (<writer>
. <seq>)))` naming its **last** item: that is the furthest point the promise has
to reach.

The receipt records, for each item, the cursor that item was written against.

A tracked request of a **single** intent has no receipt — it takes the
single-record path — but it is still tracked, and repeating it is still a
replay. A tracked request that produces **no** intent writes one record, an empty plan, which is its identity's durable evidence; sending it again replays that plan and appends nothing.

An **untracked** batch has neither a receipt nor replay protection; repeating
its text does the work again.

What the store can **conclude** about a batch it has partial evidence of. These
are the classifier's verdicts about what is on disk; nothing resumes a `planned`
or `partial` batch by itself — that is for the client holding the request:

| State | Meaning |
|---|---|
| not committed | the batch provably did not begin. It is safe to execute. |
| planned | the receipt is there and no item is. |
| partial | a prefix of the items is on disk and the rest provably did not run. |
| committed | every item is on disk. |
| unknown | one of the above cannot be established. |

`partial` is the strongest of these and the store will not claim it lightly: every
missing item must be clear of every stretch of history in doubt, the writer chain
must read to its end, and nothing anywhere — including records set aside as
superseded — may mention an item said to be missing. Failing any of those the
answer is `unknown` with the reason, never `partial`.

### What the machine registry records

Two frontiers per `(store-id, instance, writer)`, and they are not the same
fact: how far this store has been given leave to write, and how far its records
have actually reached the disk. The second is what refuses a store whose history
has gone backwards — restored from a backup, say — while the first lets a request
reserve the room it might need before it knows how much of it it will use.

The second can lag behind records that really are durable: a crash between a
record's flush and the registry's update leaves it low. A replay of such a record
raises it, which is why answering a replay is a write even though it appends
nothing.

### Two limits worth knowing

**A retirement record written before this version says less than one written
after it.** From this version a retirement carries the stretches the adopt could
not vouch for, and carries them even when there are none — so an absent clause
means "written by an older build" rather than "nothing was lost". Nothing on disk
can now say which of those an older record meant, so the older ones are read the
only safe way: everything above the retired prefix is in doubt, without an upper
bound.

The cost is real and it is one-directional. A request whose cursor falls in such
a stretch is answered `unknown` every time it is sent, and an operator's
`not-executed` resolution cannot release it, because a resolution may only exempt
a stretch with a top and this one has none. It does not block a replay when
matching evidence exists, and it does not affect a request on the writer that
succeeded the retired one. Stores created from this version forward never produce
the older form.

**A caller that captures a continuation inside its own procedure and re-enters it
after the write has finished will end the session twice.** The second end raises
and is suppressed, so no answer is lost, but the store's lock accounting is not
written to survive that shape. Do not resume a captured continuation out of a
`with-store-write` procedure.

## Sync: the `publish` verb

### `publish <writer> <segment> <file> [<sha256>]`

    (publish <writer> <segment> <file> [<sha256>])

A store receives another store's history one segment at a time. `publish` hands the
bytes of one segment to the log layer and prints what it decided, as a single
S-expression on stdout; the exit code is zero only for the four outcomes that leave
the segment published. Those four are printed inside `(ok ...)`, as
`(ok (published 1))`; every refusal is printed inside `(error ...)`, as
`(error (incomplete 1))`, except the ones already headed `error`, which are printed
as they stand: `(error invalid-candidate sha-mismatch)`. The tables below show the
inner answer.

The hash is the **sender's declaration**. Given one, it is checked against the bytes
before anything about the bytes is decided, so a segment that was damaged in transit is refused
rather than installed and discovered later by a reader. Given none, the bytes on
disk are taken as their own: computing the hash here from the same bytes would
compare a number with itself. The hash is compared after, among other things, these,
in this order: the usage form's checks; the candidate file must be there and readable
(`no-candidate`, `candidate-unreadable`); the store is taken for the publish (another
operation in progress refuses it); the writer name must be valid; the writer's
directory is made if it is missing; this store's own copy of that segment and every
other segment it holds for that writer must be readable; the manifest must be
readable; and the active-writer rule.

`publish` **waits** for the store lock rather than failing. Another process
publishing, or a reader holding the shared lock, blocks this call until the store is
free. A client told "busy" would have to re-send to learn whether its bytes were
refused or merely queued, and those are different facts about its bytes.

### What it answers

The decision is the log layer's, not the command's. Four outcomes leave the segment
published and exit zero:

| answer | meaning |
|---|---|
| `(published <n>)` | the candidate is installed and listed in the manifest. |
| `(idempotent <n>)` | these exact bytes are already installed **and** already listed with this hash; there is nothing left to do. |
| `(repaired <n> (evidence <path>))` | the local copy failed validation, the candidate covers every valid record it still held, and it has been replaced. |
| `(extended <n> (evidence <path>))` | the segment holds a retired prefix and the candidate continues it; the retirement coordinates were re-checked against the candidate first. |

Both replacements name the file the displaced bytes were copied to, under
`writers/<writer>/damaged/`. The sender is the
one party that may want them, and deriving that name on its side would be a second
supplier of it — the two would part company the first time the naming rule changed.

The rest refuse, and exit non-zero:

| answer | meaning |
|---|---|
| `(incomplete <n>)` | the candidate is a prefix of what is already here; accepting it would discard records. |
| `(newer-history-unmergeable (kept <path>) (writer <w>) (segment <n>))` | the candidate is longer than a sealed segment, which is not a segment this store may replace. |

NOTE: the two refusals whose candidate is kept under `incoming/` -- this one and
`segment-layout-conflict` -- also carry `(kept <path>) (writer <w>) (segment <n>)`:
`((segment-layout-conflict <reason>) (kept <path>) (writer <w>) (segment <n>))`.
| `(divergence (fork <seq>))` | a record disagrees with one this writer's history already holds; the fork is recorded at the lowest such sequence and never moves up. |
| `(refused insufficient-coverage (seq <n>))` | the local copy is damaged, but the candidate does not cover record `<n>`, which is still valid here. Absence is not evidence of disagreement. |
| `(refused active-writer-segment)` | the writer is this store's own active writer, and the segment is its current one -- or the store finds no segment it could append to for that writer, in which case every segment of that writer is refused. A writer with no segment yet is not refused here. This is checked above every rule about the candidate's content; the checks listed with the hash above, from the usage form to the manifest, come before it. |
| `(refused retirement-coordinates <detail>)` | the candidate would move the byte offset at which the retired prefix's last sequence ends, so the declaration would silently stop being true. `<detail>` is `(offset-moved (declared <n>) (candidate <n>))`, or `(sequence-absent <seq>)` when the candidate does not hold that sequence at all. |
| `(error invalid-candidate sha-mismatch)` | the bytes do not hash to the declared value. |
| `(error invalid-candidate not-contiguous)` | the candidate's own sequences jump, repeat or go backwards. |
| `(segment-layout-conflict <reason>)` | the candidate cannot sit at this segment number. The reasons are below. |
| `(refused invalid-writer (writer <w>))` | the writer name is not eight characters of `[0-9a-z]`; nothing is written. |
| `(refused manifest-unreadable (path <p>) (reason <r>))` | this writer's `published.sexp` cannot be read or parsed; nothing is written, not even to `incoming/`. |
| `(error invalid-candidate <reason> (offset <n>))` | a record's envelope is refused (`seq-not-a-number`, `ts-not-a-number`, `actor-malformed`, `deps-malformed`, `payload-not-a-form`); `<n>` is that record's byte offset in the candidate. |
| `(refused kept-unreadable (path <p>) (reason <r>))` | a copy already kept under `incoming/` cannot be read, so it is not replaced. |
| `(error no-candidate (path <p>))` | the file does not exist. |
| `(error candidate-unreadable (path <p>) (reason <r>))` | the file exists and cannot be read. |

A wrong number of arguments, or a segment that is not a positive integer, answers
the usage form.

### Where a candidate may sit

A writer's segments are read in ascending number and their records concatenated; a
reader stops at the first sequence that does not continue the last. So a candidate
that is perfectly formed, and that agrees with everything this store holds, can
still be unpublishable because of **where** it would sit. Each reason names the two
numbers that disagree, because each asks for a different repair:

| reason | meaning |
|---|---|
| `(gap-before-candidate (history-ends <a>) (candidate-starts <b>))` | the candidate begins past the end of the declared history below it, leaving the sequences between them owned by nobody. Something in between has not arrived yet. |
| `(overlaps-preceding (history-ends <a>) (candidate-starts <b>))` | the candidate begins at or before that end, repeating sequences a reader has already passed. The sender and this store disagree about where this segment starts. |
| `would-not-read-through` | the segment above this one does not begin where the candidate ends, so installing it would hide everything beyond. This candidate is wrong for this segment number. |
| `target-occupied` | the segment number already holds records that share no sequence with the candidate. |
| `not-start-aligned` | the candidate overlaps what is here but starts elsewhere, so the two cannot be compared as prefixes at all. |
| `empty-candidate` | the candidate holds no valid record. |

**Segment numbers need not be dense; sequences must be.** A reader walks segments in
number order and records in sequence order, so a gap between segment *numbers* costs
nothing — there is no record in it to miss — while a gap between *sequences* is
history nobody holds. A candidate that continues the sequence may therefore take any
free segment number, which is what lets a publisher choose one without knowing which
numbers its peer has already used.
A segment is stored as a six-digit file name (`000001.sexp`), so segment numbers run
from 1 to 999999. A larger number of up to eighteen digits is not refused by the usage
check but fails as an internal error; one of nineteen digits or more is answered with
the usage form, as any argument that is not a count is.

**Layout coordinates come from what the store declares, never from whichever files
happen to be in the directory.** Two declarations exist: the manifest, which lists
the segments this writer has published and the range each holds, and the retirement
record, which names the sequence a retained prefix ends at. A file that neither of
them names is an orphan — bytes that arrived from somewhere and are not this
writer's history — and contributes nothing, so a writer with nothing declared below
a segment ends at 0 and a candidate for its first segment must begin at 1. A killed
install, whose segment was written but never listed, is exactly such an orphan.

Where history ends is the end of the **traversable** declared prefix, not the highest
declared sequence. The two differ only when the declarations already hold a hole, and
there the difference decides whether the hole can ever be repaired: segment numbers
only increase, so refusing the one candidate that could fill a gap refuses it
permanently.

#### `published.sexp`

The manifest lists what a writer has published:

    ((<segment number> "<hex sha256>" <first seq> <last seq>) ...)

ascending by segment number, written whole. The entry carries the range because
layout decisions are made from declarations: a listed segment whose bytes are damaged
is history of a known extent awaiting repair, and reading its extent from the bytes
instead would make it unknowable exactly when it is needed — an incoming segment's
fate would then depend on damage elsewhere, and on the order in which things arrived.

There is one shape. An entry of two elements is not an older manifest to be read
leniently; it is a manifest this build does not understand, and it is reported as an
integrity error rather than accepted.

A missing manifest is not an empty one: absent means this writer has no manifest at
all, which is the ordinary state of a local writer, while `()` means a manifest
exists and lists nothing.

On opening, a listed segment whose bytes do not hash to the declared value is
excluded from the writer's history, and so is one whose declared range disagrees with
the range its bytes hold — with the hash matching, a disagreeing range is the manifest
contradicting itself, and in both cases the store cannot say what the segment is.

#### `incoming/`

A candidate refused as `segment-layout-conflict` or `newer-history-unmergeable` is
**kept, not installed**; no other refusal keeps anything. Its bytes are written under
`writers/<writer>/incoming/` as `<segment file>.<sha256>.seg`, named by their own hash, with an empty marker file
beside them recording that the bytes passed the checks that are properties of the
bytes alone — per-record CRC and internal sequence continuity. Nothing is written at
the segment's own name and nothing is added to the manifest.

The name is content-addressed so that the same candidate arriving again is recognised
without being re-read. The decision itself is **not** cached: the splice, the
divergence check and the publish decision are redone on every call, against the
history as it is at that moment. A gap refused today becomes publishable once the
missing middle arrives, and the kept bytes are then accepted unchanged.

### The recovery barrier

Four answers leave the segment published, and a sender deletes its own copy on the
strength of any of them. So all four have to promise the same thing, and they promise
it by going through one table:
(The same table is also run for `commit`, whose answer asserts only the segment
file and its directory, so a local writer with no manifest is not refused.)

| row | the file |
|---|---|
| `log-file` | the segment itself |
| `writer-directory` | the entry naming it |
| `manifest` | `published.sexp`, which admits the segment to the writer's history |
| `owner` | each generation's `owner.sexp` |
| `retired` | each generation's `retired.sexp` |
| `instance` | the store's `instance.sexp` |
| `uncertain` | the writer's `uncertain.sexp` |
| `registry` | `instances.sexp`, outside the store |

The order is documentation, not a protocol. An fsync establishes that an operation has
completed; it does not hold other writes back, so these names reach the disk in
whatever order the filesystem chooses, and two of the rows share a directory in any
case. Recovery ordering is established where the mutations happen — the candidate is
durable before it is linked, and its directory entry before the manifest names it. The
barrier is the completion point that reasserts those obligations before the answer is
given, not a second attempt to sequence them.

Read outwards from the record, each row is a premise of the one before it: a record
whose file is durable but whose directory entry is not is a record no reader will
find, and a segment whose manifest entry is lost is a file this store will treat as an
orphan. Every file row is flushed and then has the directory entry naming it made
durable; the directory row is fsynced as a directory.

`idempotent` is the **replay** of a publish, and it is the reason the table is one
table. A replay writes nothing — the bytes and the manifest entry are already exactly
right — so if it made its promise from a second list, the two would agree until one of
them changed, and the disagreement would surface only as a replay that promised more
than the original execution had. The barrier runs in one place, on any of the four
answers, so a branch added later is covered because it returns one of the four and not
because whoever wrote it remembered.

A refusal does not pass the barrier. Nothing it describes is claimed to survive a
crash, and flushing the store's whole recovery closure on the way out of every
rejected candidate would charge refusals for a promise they do not make.

The table holds what is there: a store with no retirement record has nothing to
promise about one, and listing the path anyway would put a row in it that no test can
arm a fault at. Three rows are not optional in that way — the segment file, the
directory naming it, and the manifest listing it — because every one of the four
answers asserts all three. Their absence stops the barrier rather than passing through
it silently, since absence and success are the same silence: a flush asked to make a
file durable that is not there does nothing, and says nothing about it. For the same
reason those three are opened rather than asked about, so that a file removed between
the moment the table was built and the moment it is flushed fails the open instead of
being skipped quietly.

`writers/<writer>` is created once, and the call that creates it is the only one that
makes its name durable — a retry finds the directory already there and skips the flush.
So the directory row makes its own entry durable as well as flushing the directory:
a mkdir that succeeded and a flush that did not would otherwise leave a name nothing
ever repairs. The remaining ancestors need no row, because every file row flushes the
directory naming that file, which covers `writers/<writer>` from the segment, the store
from `instance.sexp`, and the registry's own directory from `instances.sexp`.

The owner and retirement rows are taken for **every** generation, not only the one
being published to, so the cost of a durable answer grows with the number of writers a
store holds and is paid under the exclusive lock. That is a measured cost and not a
defect: narrowing it to the generations on the path to this writer is sound only once
the metadata a recovery actually reads has been written down, and it has not been.

### Durability stages

Every call that makes something durable names the stage it belongs to, from the
list `known-stages` in `ffi.sc` — `deliver-barrier`, `commit`, `registry`,
`publish`, `snapshot`, `repair`, `report`, `working`, `index`, `eval-cleanup`,
`presence`, `presence-decision`, `mcp-wait`, `mcp-signal`, `admission`, `derived`,
and `conn` and `client`, which are fault points rather than flushes — and the
stage is a required argument, not a decoration:

    (fsync! fd subject stage)
    (fsync-dir! path stage)
    (atomic-write! path bytes stage)
    (directory-entry-durable! path stage)
    (stage-candidate! store writer bytes stage)

Fault injection matches on the stage, and a staged fault never matches a call that
declares none. That makes the list of stages **the list of steps a test can reach**:
an unlabelled flush is not merely untested, it is untestable, and in every log that
is indistinguishable from a step that passed. Two such calls sat in the publish path
until a case was written that tried to arm them. Making the stage a required argument
is what turns the rule from something to remember into something that cannot be
written wrongly; `test/stage-gate.sc` reads the sources and checks that every one of
these calls names a stage from the list.

Run the suites from inside `test/`, with `THEOURGIA_LIBDIR` naming a directory that
holds both `theourgia/` and `igropyr/` (see `test/RUN.md`):

    cd test && . ./env.sh && sh run-fixtures.sh <output-directory>

The run's preflight, `test/expansion-branches.sc`, imports the libraries the
programs import statically, in a child under each value of `THEOURGIA_INJECT`,
and stops the run when one does not build. It loads no library entered on
demand -- one a program or the dispatcher reaches through `environment` when a
verb asks for it -- so an unbound identifier in such a library passes the
preflight. `test/f0-ondemand.sc` loads each of them in a child, in both
branches: every library a source hands, quoted, to `environment` or to the
programs' `later`, and every extension verb's handler library, read from the
sources rather than listed.

## Evaluating against a store

### `eval`

    (eval ("--lang" <language>) ("--cut" <cut>) ("--under" <library-id>)
          ("--working") ("--latest") ("--writer" <name>) ("--timeout-ms" <n>)
          ("--memory-bytes" <n>) ("--output-bytes" <n>) <source>)

Evaluates one expression against what the store holds, in a child process
that is given nothing else: no filesystem, no network, no way to reach
the committed store except by reading it.
With no `<source>` argument the source is read from standard input; when
that is empty too, the answer is the usage form, `(usage (eval ...))` -- except
that with every slot of the pool held it is `eval-busy`, since the slot is taken
before standard input is read. A source
longer than 1048576 characters is refused `(error bad-source (reason input-limit))`.
`--under` takes the id of a datum library block, not the library's name.

**The answer is one complete line**, always, and one of:

    (ok (values (<v> ...)) (working-view <writer> <cut> <drafts>)
        (stdout "...") (stderr "..."))
    (error eval-limit (resource time|memory|output) (limit <n>) (stdout "...") [(truncated #t)])
    (error eval-exception (kind raised) (message "..."))
    (error eval-exception (kind raised) (value <v>))
    (error eval-exception (kind raised) (reason unwritable-value) (type <t>))
    (error bad-request (reason ...) (usage (eval ...)))
    (error bad-request (reason eval-arguments) (option "<name>" (reason <r>)) (usage (eval ...)))
    (error eval-busy (slots <K>) (waited-ms <n>))
    (error eval-value (kind <t>) (index <i>))
    (error bad-source (reason input-limit|expected-one-form|expected-one-cut))
    (error eval-context (reason unavailable-cut|library-required))
    (error eval-context (reason draft-on-datum-unsupported) (block <id>))
    (error eval-denied (operation library-import))
    (error eval-worker-exit (status <n>|unknown))
    (error eval-worker-unavailable (reason ...))
    (error spawn-refused (reason ...))

`eval-value` is a value the source RETURNED that cannot be written back; `<t>` is
`procedure`, `port`, `cycle`, `size` or `unsupported` -- the kinds of
`unwritable-value` below, except that a value too large is `size` here and
`too-large` there. `<i>` is its position among the values.

`draft-on-datum-unsupported` is a `--working` view in which the library named
by `--under`, or one of its definitions, holds a draft: a datum block's text
is its body, and a draft is not read as one. `<id>` is the block holding it.
A writer whose view holds no such draft evaluates as before. `write` refuses
to make such a draft; a store written before that can still hold one.

NEVER: **What the evaluation prints is DATA, carried in a field.** Text that
reads exactly like an answer still arrives inside `(stdout ...)`; it can
never be mistaken for the answer itself.

NEVER: **So is what the source raises.** An answer is `(ok ...)` or an
`(error <name> ...)` of the worker's or the supervisor's, and nothing the
source does changes that shape. A raised condition is answered as above,
with a fixed message. Any other raised value arrives inside `(value <v>)`, an
`(error ...)`-shaped list included, when it can be written and read back;
otherwise the answer says `(reason unwritable-value)` and a `<t>` of
`procedure`, `port`, `cycle`, `too-large` or `unsupported`, and the value
itself is not sent.

A limit the option cannot take names the option, inside the `option` clause:
`<r>` is `not-a-number` (it is not a whole number), `not-positive`, or
`out-of-range` followed by `(range <low> <high>)`.

**The limits, and their bounds**: `--timeout-ms` 1..60000 (default 3000),
`--memory-bytes` 1 MiB..2 GiB (default 256 MiB), `--output-bytes`
128..1 MiB (default 65536). For a Scheme evaluation the output quota counts
the DECODED characters the user printed, across both streams -- what the
user printed, not what the framing made of it. A `--lang` runner's quota
counts bytes (below). When the output quota stops an evaluation, its
`(stdout ...)` carries at most the limit in the quota's own unit -- characters
for Scheme, bytes for a runner, cut on a character boundary -- and the answer
ends with `(truncated #t)` when anything was cut. NOTE: The memory budget is read by sampling the child's resident size
every 50ms, so it applies to an evaluation that lasts at least that long;
one that finishes sooner has already ended.

**`(working-view <writer> <cut> <drafts>)` is always present.** Its
`<cut>` is always the cut the evaluation actually used -- it is the
coordinate the run can be repeated from, and "at some cut" is not one.
Without `--working` the writer position is `#f` and the draft table is
empty; the cut is still the committed state that was read.

NEVER: **`--working` is PINNED by default.** The view stands on the state the
writer's OWN drafts record -- the join of their cuts -- so a commit
another writer made after those drafts does not walk into it, and the
same unchanged draft answers the same way twice. `--latest` releases the
pin and evaluates against the current committed state. An explicit
`--cut` wins over both, and `--cut` together with `--latest` is refused
with `(error bad-request (reason cut-and-latest) ...)` rather than
resolved by a precedence rule -- a rule would make one of the two
spellings silently do nothing.

**How many run at once.** Every evaluation first takes one of K slots of
its run root's pool -- K is `THEOURGIA_EVAL_SLOTS`, else the number of online
processors -- and holds it until its process ends; a slot is an exclusive
lock on a file under `<run root>/admission/`, released by the system when
the holder ends, a killed one included. The descriptor is never inherited by
the worker or the runner. With no slot free the evaluation waits, trying
at once and then every 100 ms, measured on the monotonic clock: after the
first attempt, each attempt needs a fresh reading below its own
`--timeout-ms`, though an attempt begun just before it can finish just
after. Each sleep is cut to what is left of the budget, and the refusal
comes once a reading reaches it, which scheduling can put somewhat after:
`(error eval-busy (slots K) (waited-ms n))`. That wait is not part of the
evaluation's own deadline. A K that is not a positive integer is
`(error bad-request (reason eval-slots))`. The refusals that need no store --
`cut-and-latest`, `eval-arguments`, and for `--lang` `lang-and-cut`,
`lang-and-under`, `no-runner` and `runners-disabled` -- come before the slot,
so a refused request does not queue. The pool bounds cooperating clients of
ONE run root, the command line and MCP shells alike; it is not a machine-wide
bound: two run roots are two pools, two processes given different K on one
root are not reconciled, and a descendant an evaluation orphaned is not
counted once its core.sc has ended.

**Whose working view.** `--writer <name>` names the writer; without it,
`THEOURGIA_WRITER` when it is set and not empty; never the actor -- the
rule every other draft verb follows, and the same in the cut, the view and a
`--lang` runner's projection. With neither, `--working` is refused
`writer-required`, `(during cut)`, or `(during view)` under `--latest`.
NOTE: A change: `eval` used to read `--writer` alone, so `eval --working`
with `THEOURGIA_WRITER` set and no `--writer` was refused `writer-required`;
it now reads that writer's drafts.

**Through the MCP shell** `eval` is the tool `theourgia_eval`: the shell runs
`core.sc eval` with the caller's argv as its own child and returns the answer
byte for byte, so this section holds there too, with two differences: the
transport options, `--wire` among them, are refused
(`transport-option-in-rpc`), since the shell sets the store and the mode;
and without `--writer` the writer is the shell's session writer
(`mcp/README.md`, "Eval").

**The answer's mode.** `--wire` on the command line, else
`THEOURGIA_WIRE=1` in the environment, and for `eval` only. The variable is
for a caller that runs `core.sc` as its child and passes the caller's
argv through untouched -- the MCP shell does -- so the mode travels without
adding a word to that argv.

#### `--lang`: another language

`eval --lang <language> <source>` runs the source with the RUNNER the
language table names for that language -- `node` for `javascript` and
`typescript`, `python3` for `python`, `go run` for `go`, the compiler and
then the program it built for `rust` and `c`, `java` for `java`, `sh` for
`shell`, `scheme --script` for `chez` -- over a projection of the store.
`--lang scheme` is the evaluation above, unchanged, with all its options;
`--lang chez` runs Scheme outside that sandbox (below).

**Runners are off** unless the operator sets `THEOURGIA_RUNNERS=on` in the
environment of the process that runs `eval`; otherwise the answer is
`(error runners-disabled)`. A runner reaches the projection and whatever
its interpreter can reach on this machine: the Scheme sandbox above does
not apply to it. The interpreter is started with an environment made of
`PATH`, `HOME` and `LANG` alone: nothing of the calling process's
environment is inherited. The launcher that starts it is a Scheme program;
it finds its libraries where the calling process found its own -- its
`CHEZSCHEMELIBDIRS` and `CHEZSCHEMELIBEXTS` are written from that process's
library directories (made absolute) and extensions, never from its
environment -- so nothing the projection holds is ever loaded as a library
by the launcher; a runner configured to search it, as `chez` is, loads from
it after exec, and that is the runner's reach.

The store is projected by `export-code` with raw bytes (no markers) -- the
committed state, or with `--working` the writer's working view -- into
`tree/` of a fresh `eval-<token>/` under the run root, and the source is
written into its sibling `source/`. Fresh means this evaluation created
the directory itself: a name already taken, by a file or a directory, is
left alone and the next token is tried, and after eight taken names the
answer is `(error spawn-refused (reason scratch-unavailable))`. The run
root is made if missing and then resolved once, when the directory is
claimed (its real path; one that cannot be resolved answers
`scratch-unavailable` too -- unless making it already failed, as on a loop
of symbolic links, which answers the filesystem refusal that names the
entry and its path), and the directory is removed through that resolved
path, so a runner that rewrites a symbolic link on the way to the run root
does not redirect the removal.
Replacing a real directory above the run root with a link needs write
access to that directory's parent and is within the runner's reach
described above. The runner's working directory is `tree/`, so the source
reads projected files by relative paths; `{file}`, the source's path, is
absolute. A relative module import from the source (`import './a.mjs'`, a
Python sibling module) resolves against `source/`, not `tree/`: build
absolute paths from the working directory for those. The directory is
removed after the answer; a symbolic link the runner left in it is removed
as a link and never followed. A removal that fails leaves the directory in
place and the answer unchanged, and says so by the trace line
`eval-cleanup-failed` when tracing is on. It is scratch: when a refusal
carries a `written` clause, that clause names what this process changed in
the store -- the draft lock a `--working` view may create -- never the
directory made and removed around the run. What the runner itself writes,
by any path it can reach, is not recorded there: it is part of the reach
described above.

    (ok (exit <n> | (signal <name>)) (stdout "...") (stderr "...")
        (lang <language>) (projection (files <k>))
        (working-view <writer> <cut> <drafts>))

There is no `values` clause: another language returns no datum, so print
what you want back. A non-zero exit is data, not an error. The output
quota counts BYTES here, both streams together, and stops the runner the
moment they pass it. However the runner ends, its whole process group is
sent SIGKILL before the answer. When it exits on its own, the group is
also waited for, up to 2 seconds, until no member is left, so a child it
left running in the background is gone when the answer arrives; a group
still there after the wait is reported by the trace line
`eval-group-survived`, and the answer goes out anyway. On a limit the
group is signalled and not waited for. A descendant that left the group
itself, by `setsid`, is out of reach either way. A store missing a writer
projects what can be read, and the answer carries the `incomplete` clause
naming it.

The refusals: `(error bad-request (reason lang-and-cut))` for `--cut` or
`--latest`, `(reason lang-and-under)` for `--under`, `(reason no-runner)`
with `(lang <language>)` for a language the table does not know or one with no
runner (`markdown` has none); `(error projection-failed <the export's
answer>)` when the store cannot be projected, and nothing runs -- a store whose `meta.sexp` is
missing, or is not a store's, answers the export verb's own `(error meta
(path ...))` inside it -- with `--working` the view meets it first and
answers `(error working-unavailable ...)` instead; an unreadable
`meta.sexp` answers as any unreadable entry does; `(error spawn-refused
(reason interpreter-missing))` when the interpreter is not an executable
on `PATH`, found before anything ran; `(error spawn-refused (reason
platform-unmeasured))` when the launcher exits 75 before it is ready, the
platform table's refusal (see Installing). Four more `spawn-refused` reasons
come before anything is made, when the launcher's `CHEZSCHEMELIBDIRS` or
`CHEZSCHEMELIBEXTS` cannot carry the calling process's library path as it
is: `library-directories-empty` and `library-extensions-empty` (an empty
value reads as an empty list, so the launcher would load nothing);
`library-directory-unrepresentable` with `(directory <d>)` and
`library-extension-unrepresentable` with `(extension <e>)` for a name
whose text Chez does not read back as itself -- one holding `:` (a working
directory whose name holds `:` is one, when the libraries are found by the
default `.`) -- or that holds a NUL character. If the interpreter
disappears between that check and its start, the answer is `(exit 127)` --
the one answer a runner could also produce itself.

A runner is an `argv`, a `source-name` and, optionally, an `env`: a list of
`(<name> <value>)` pairs added to the interpreter's environment after it is
cleared to `PATH`, `HOME` and `LANG` (those three cannot be named). In
`argv`, `{file}`, `{dir}` and `{libdirs}` -- the launcher's own library
path, as written for it -- are replaced only as whole arguments; in an
`env` value the same three are replaced wherever they occur, since a value
is a path list. No string of a runner may hold a NUL character, and the
interpreter's name may not be empty. `source-name` is one path component: not
empty, not `.` or `..`, and without `/`. An `env` name may not be empty or hold
`=`. An interpreter name holding `/` is taken as a path; any other is searched
for on `PATH`.

#### `--lang typescript`, `go`, `rust`, `c` and `java`

Each runs with what a standard install of its language provides, and no
project around the source:

- `typescript`: `node {file}`, the source written as `__eval.mts`. Node
  runs a TypeScript file by stripping its types from 22.18 and 23.6 on; an
  older `node` exits non-zero with its own message in `stderr`.
- `go`: `go run {file}`, as `eval.go`; the source is a `package main`
  with a `main`. Not `__eval.go`: the go tool leaves out a file whose name
  begins with `_` or `.`, even one named on its command line.
- `rust`: `rustc` builds `__eval.rs` into `__eval.rs.bin` beside it in
  `source/`, and the program runs in its place:
  `(argv ("sh" "-c" "rustc -o \"$1.bin\" \"$1\" && exec \"$1.bin\"" "rust" "{file}"))`.
  A source that does not compile answers the compiler's non-zero exit and
  its `stderr`; one that does answers the program's own. The interpreter
  is `sh`, so a missing `rustc` is the shell's `(exit 127)`, not
  `interpreter-missing`.
- `c`: the same with `cc`, as `__eval.c`.
- `java`: `java {file}`, Java's single-file source launch, as
  `__eval.java`; the file's first class holds `main`.

The binary is removed with `eval-<token>/`. What a compiler writes
elsewhere, Go's build cache under `HOME` among it, is the operator's.

The operator replaces any of the five with that language's variable:
`THEOURGIA_RUNNER_TYPESCRIPT`, `THEOURGIA_RUNNER_GO`,
`THEOURGIA_RUNNER_RUST`, `THEOURGIA_RUNNER_C` or `THEOURGIA_RUNNER_JAVA`,
read as `THEOURGIA_RUNNER_CHEZ` is, from the environment of the process
that runs `eval` and never from the store; an empty value is unset. Unlike
chez's, the value is a WHOLE runner: one datum holding `argv` and
`source-name`, and `env` if it needs one, which replaces the table's runner
entire. Nothing of the default survives it: a value naming `argv` alone is
refused with `(detail (field source-name))`, not completed from the table.
A value that is not exactly one datum, or that a runner's checks refuse,
answers `(error bad-request (reason runner-config-invalid) (variable
"THEOURGIA_RUNNER_GO") (detail ...))`, naming its own language's variable,
before the evaluation takes an admission slot, and nothing runs. TypeScript
through `tsx`:
`THEOURGIA_RUNNER_TYPESCRIPT='((argv ("tsx" "{file}")) (source-name "__eval.ts"))'`.
`javascript`, `python` and `shell` have no variable: their runner is the
table's.

#### `--lang chez`: Scheme outside the sandbox

`chez` runs Scheme source with the interpreter or compiler the operator
configures, over the projection, with the libraries the store holds on its
path: a library imported with `import-code` (`lib/a.sc` holding
`(library (lib a) ...)`) is reached by `(import (lib a))`. It claims no file
extension; files stay `scheme`'s, and `--lang scheme` stays the sandbox.
Its default runner is
`(runner ((argv ("scheme" "--script" "{file}")) (source-name "__eval.ss")
(env (("CHEZSCHEMELIBDIRS" "{dir}:{libdirs}")
("CHEZSCHEMELIBEXTS" ".sc:.ss:.sls:.scm")))))`,
so the projection is searched first and then the calling process's own
library directories, and `.sc` comes first among the extensions (single
colons: in Chez `a::b` pairs a source extension with an object one). It is
behind the same gate as every other runner, with the same answer, limits,
`--working`, refusals and cleanup, and it reaches whatever the operator's
Chez reaches on the machine.

The operator configures it with `THEOURGIA_RUNNER_CHEZ`, read from the
environment of the process that runs `eval` and never from the store:
one datum naming any of `argv`, `source-name` and `env`. The fields it
names replace the table's, field by field, and a named field replaces the
table's whole: `(env ())` leaves the interpreter `PATH`, `HOME` and `LANG`
alone. An empty value is unset. A value that is not exactly one datum, or
that a runner's checks refuse, answers `(error bad-request (reason
runner-config-invalid) (variable "THEOURGIA_RUNNER_CHEZ") (detail ...))`,
naming the field (`(detail (field argv))`) or `(detail not-one-datum)`, and
nothing runs: a default never runs in place of what the operator named. It
is answered before the evaluation takes an admission slot, so a full pool
does not hide it behind `eval-busy`. An interpreter:
`THEOURGIA_RUNNER_CHEZ='((argv ("petite" "--script" "{file}")))'`.

A compiler pipeline is an `argv` too. Since `{file}` is replaced only as a
whole argument, it goes to the shell as `$1`:

    THEOURGIA_RUNNER_CHEZ='((argv ("sh" "-c" "goeteia build \"$1\" && ./a.out" "compile" "{file}")))'

theourgia adds no build step and no cache, and removes only `eval-<token>/`:
what a pipeline writes elsewhere is the operator's. Pointing `chez` at
another implementation means replacing `argv`, `env` and `source-name`. A
store cannot choose the runner or what the launcher loads; but a projected
library the source imports runs with the runner's reach, which is what this
runner is for.

When the runner's `CHEZSCHEMELIBDIRS` names `{dir}` and the projection's
directory, as resolved at the claim, cannot be carried in that variable (a
run root whose path holds `:`), the answer is `(error spawn-refused (reason
projection-directory-unrepresentable) (directory <d>))`, before anything is
exported, and the claimed `eval-<token>/` is removed.

## Serving a store

### `serve`

    (serve (<store>) ("--socket" <path>) ("--detach" "--log" <path>) ("--attempt" <token>))

Holds the store open and answers requests over a unix socket until it is
told to stop. The store may be given as a positional or as `--store`.
The program is `theourgiad.sc`; it takes this verb and no other, and
answers any other with `(error bad-request (reason not-a-daemon-verb)
(usage ...))`, this usage inside the error, exit 1. It reads `--store`,
`--socket`, `--detach`, `--log` and `--attempt`, and refuses any other option,
`--wire` and the other verbs' shared options included, with `(error
bad-request (reason unknown-option) (option "<the option>") (usage ...))`,
exit 1, before anything is opened or bound. A shared option given without
its value, or given twice, is refused first by the argument parser, as
for every verb: `(error bad-request missing-option-value "<the option>")`
or `(error bad-request duplicate-option "<the option>")`, followed by the
usage on its own line, exit 1. A store whose name starts
with `--` is given as `--store <store>` or after `--`; the clients that
start a daemon pass a store that starts with `-` as `--store <store>`,
and any other store as the positional.

`--attempt <token>` is not for a person: it is the token a client passes
for this one start, and the daemon echoes it as the last clause of every
startup report it writes, so the client can tell its own start's report
from another's in the shared log. A start made by hand has none, and its
reports end with `(attempt #f)`.

A `--socket` given by hand is not given a directory. An empty one answers
`(error bad-socket-path (reason empty))`. One whose directory does not
exist answers `(error socket-dir-missing (dir <dir>))`. Both exit 2,
before `--detach` and before anything is created. Only the default
socket's directory is made by the daemon.

**A store replaced under it.** Stop the daemon before switching the git
branch of a repository that holds its store, or restoring the store's files
in any other way: the daemon holds the store, and a swap under it makes the
working tree's store and the branch's fork. The guard for when it was not is
one rule. The daemon remembers each writer's current segment as it last
published it, by path. A write asks first, under the store's lock, about the
writer it appends for: when that segment, at that path, is no longer the same
file and is shorter than it was, the write is refused before anything is
appended,

    (error refused store-replaced (segment "<path>") (remedy restart-the-daemon))

and every write for that writer after it is refused the same way -- even after
a read has made the daemon read the store again, and even if the segment grows
past its old size -- until the daemon is started again on the store as it now
is. A publication whose current segment for a writer is another path, and
an earlier segment replaced, are not seen by this rule; neither is a writer
that takes no appends. Appends by another process only make the segment
longer, and a rotation leaves it as it was, so neither is taken for a swap.
Nor is the daemon's own write that cuts a torn tail back before it appends:
what the daemon itself left is what it knows. Any other process that puts a
shorter segment at that path -- a repair that replaces it whole, say --
changes the store under the daemon as a checkout does, and is refused the
same way, with no exemption: run such tools with the daemon stopped.

**Where the socket is.** With no `--socket`, it goes at
`<run-root>/<key>/socket`, where the run root is `THEOURGIA_RUN` or
`$HOME/.theourgia/run` and the key is the first 16 hex digits of the
sha256 of the store's resolved path. One function computes it, in
`(theourgia client)`, and both the daemon and every client import that
one -- written twice they would be two rules, and the day they differed a
client would start a second daemon for a store that already had one.

The key is the store's RESOLVED path, so every spelling of one store
reaches one socket, and it is the same key before the store exists as
after: the longest existing prefix is resolved and the components below
it are appended. NOTE: That last part is not a detail. `init` creates the
store, so a caller computing the socket path first and a caller computing
it afterwards are the ordinary case; when those two disagreed, the second
found no socket where it looked and ran the store locally instead --
giving a correct answer, from the right store, with no sign that it had
bypassed a daemon sitting right there.

NOTE: The reason it is the run root rather than beside the store:
`sun_path` holds 104 bytes on macOS and FreeBSD (108 on Linux), and a
store may sit anywhere and be arbitrarily deep, so a store-adjacent
socket under a long path simply fails to bind and the daemon reports
`listener-down`.
The address a client connects with is built from the platform's row --
a one-byte `sun_len` and `sun_family` on macOS and FreeBSD, a two-byte
`sun_family` and no length byte on Linux -- and the length it passes is
the whole struct on every platform.

**Detaching.** `--detach` is for a launcher, not for a person: the
process leaves the caller's session and replaces its standard streams,
and it does NOT fork. Typed at a prompt it stops there and prints
nothing, because the output it would have shown has already gone to the
log. It requires `--log <path>`, and refuses without one, answering `(error detach-needs-a-log (usage ...))`, exit 71 -- a detached
daemon with nowhere to write is one whose every startup refusal is lost,
and the client that started it could then only report that it did not
come up. The log is opened for APPEND, so several starts against one
store share it and a failed start's reason is still there afterwards.
In order: leave the session, replace the streams, then take the lock and
open the socket. A `serve` without `--detach` does none of it. If
leaving the session or opening the log fails, it writes `(error
detach-failed (step setsid|log) (path <log>) (errno <e>))` on stderr and
exits 71. The clients that start a daemon pass `--detach --log
<run-root>/<key>/serve.log`.

**Starting when one is already there.** The lock attempt never waits: a
second daemon answers `(error serve-busy (path <socket>))` and exits 75.
If the socket path is occupied by something that is not a socket it
answers `(error serve-path-occupied (path <socket>))` and exits 75. Both
are startup reports: a `(written ...)` clause follows when this start
created the lock file, and `(attempt ...)` comes last. Any other
filesystem failure while starting is reported the same way, also with
exit 75. A
LEFTOVER socket file whose daemon is gone is cleared and taken over --
the lock, not the file, is what decides which of those two it is. When it
is up it reports `(serving (store <path>) (socket <path>))`.

**`SIGTERM` drains; a second one stops.** The first signal lets work that
has already begun run to its end, within a budget of five seconds, while
refusing new requests with `(error draining)`. It reports `(draining
(in-flight <n>))`, then `(exiting (reason drained))` and exits 0. A drain
still running at five seconds reports `(exiting (reason drain-timeout)
(in-flight <n>))` and exits 75. A second signal stops immediately with
`(exiting (reason second-signal))`, exit 75 -- a drain that is taking too
long is exactly when somebody needs to be able to end it.

**`(error store-busy (path <path>))`** is an answer, not a crash: a
request waited for the store's lock past its five-second budget because
something else was holding it. That is an ordinary state of the world.

**While a daemon serves a store, write through it.** The daemon answers
reads from the state it last folded and published; it does not watch the
store. Before each answer from that state it checks whether the store
changed underneath it -- a record another process appended directly,
through `(theourgia log)` or `THEOURGIA_LOCAL=1`, or a segment a pull
brought in -- and if so asks for the store to be folded again. The answer
that noticed the change is still given from the earlier state, so a read
of the new record can answer `unknown-id` once; the reads after the fold
is published see it. A write is not answered from that state: the store
process decides it on the store as it stands under the lock. A fold that
fails keeps the earlier state. Writing one store from two machines is a
separate design.

**`THEOURGIA_TRACE=1`** makes the daemon, like any process started with it, write its filesystem and
dispatch events as `(trace <op> <path> <detail>)` lines on stderr.

## The MCP shell

`theourgia-mcp` speaks MCP `2025-11-25` over stdio in front of the same
dispatcher, one tool per catalogue verb it can carry out -- those routed to
the daemon, and the ones routed `child` (`eval`, `scope` and `collect`), run
as its own child. See
[`mcp/README.md`](mcp/README.md) -- what a tool returns, why a core
refusal comes back as a successful result, and how the shell branches on
the transport's tag rather than on the answer's text. A tool's result text is
exactly what the command line prints with `--wire`, its final newline
included (the query fixture compares the two byte for byte).

To make an agent keep its memory in a store -- moving a markdown memory in,
and the session hook, `CLAUDE.md` text and MCP registration that point Claude
Code at the store -- see
[`docs/claude-code-memory.md`](docs/claude-code-memory.md).

## Environment variables

| variable | read by | what it does |
|---|---|---|
| `CHEZSCHEMELIBDIRS`, `CHEZSCHEMELIBEXTS` | Chez itself | where the libraries are found. No source file here reads them; `eval --lang` WRITES them for its launcher, from the running process's own library directories and extensions (see `--lang`). |
| `THEOURGIA_STORE` | `theourgia.sc`, `core.sc`, `theourgiad.sc` | the store to use when `--store` (and, for `serve`, the positional) is absent. Falls back to `.`. The MCP shell sets it to its own store for the `eval` child it runs |
| `THEOURGIA_ACTOR` | `theourgia.sc`, `core.sc`, `mcp/server.sc` | who the requests are from when `--actor` is absent. Empty is unset. Falls back to `USER`, then `cli`. The MCP shell sets it to its own actor for the child it runs |
| `THEOURGIA_SOCKET` | `core.sc` | for `scope` and `collect`, the main store's daemon socket when `--socket` is absent. Empty is unset. The MCP shell sets it to its own socket for the child it runs, so the child reaches the daemon the shell does |
| `THEOURGIA_WRITER` | `core.sc`, `theourgia.sc`, `mcp/server.sc` | whose drafts a request reads and writes, `eval --working`'s view included. Unset or empty, the command line sends no writer, and a draft verb without `--writer` is refused `writer-required`; the MCP shell takes `--writer`, else derives a writer for the session (`mcp/README.md`, "Whose drafts"). The shell checks it at start and answers its usage line, exit 2, for a name a writer cannot have |
| `THEOURGIA_HOME` | `ffi.sc` | where the machine registry (`instances.sexp`) and its lock live. Empty is unset. Falls back to `$HOME/.theourgia` (`/tmp/.theourgia` with no `HOME`). When set, every process announces it on stderr at load as `(theourgia machine-home <value>)`, since it lets a process ignore a rollback the registry exists to catch |
| `THEOURGIA_RUN` | `client.sc` (for `theourgia.sc`, `theourgiad.sc`, the daemon and `mcp/server.sc`) | the run root holding daemon sockets, their logs and the MCP shell's `eval` call directories. Empty is unset. Falls back to `$HOME/.theourgia/run`, or `/tmp/.theourgia/run` with no `HOME` |
| `THEOURGIA_LOCAL` | `theourgia.sc`, `core.sc` | `1` answers in process even when a daemon's socket is there. The thin client hands the request to `core.sc` for any non-empty value, but `core.sc` stays in process only for `1`; for any other value it still forwards to a daemon whose socket is there |
| `THEOURGIA_EVAL_SLOTS` | `eval-admission.sc` | how many evaluations one run root runs at once (the admission pool, `### eval`); a positive integer. Unset or empty, the number of online processors. Anything else answers `(error bad-request (reason eval-slots))` |
| `THEOURGIA_EVAL_SLOTS_DEFAULT` | `ffi.sc` | a test seam: a positive integer read in place of the online processors when `THEOURGIA_EVAL_SLOTS` is unset or empty, so a row can ask for a pool of a known size on any machine. Empty, it is unset; any other value that is not a positive integer answers `(error bad-request (reason eval-slots))` |
| `THEOURGIA_WIRE` | `core.sc` | `1` makes `eval` answer in wire form, as `--wire` does; read for `eval` only, and any other value, or none, leaves the mode to `--wire`. The MCP shell sets it for the `eval` child it runs |
| `THEOURGIA_MCP_PREPARATION_MS` | `mcp/server.sc` | a test seam: the preparation allowance, in milliseconds, in how long the MCP shell waits for an `eval` child (twice the timeout plus this; 70000 when unset). A value that is not a positive integer is refused at start with the usage line, exit 2 |
| `THEOURGIA_RUNNERS` | `core.sc` | `on` turns on `eval --lang`'s runners for another language; any other value, or none, leaves them off (`runners-disabled`). For an MCP caller the environment that counts is the MCP shell's -- the host's configuration for it -- since the shell's `eval` child inherits it; the daemon's is never consulted |
| `THEOURGIA_RUNNER_CHEZ` | `eval-runner.sc` | the operator's runner for `eval --lang chez`: one datum naming any of `argv`, `source-name` and `env`, each replacing the language table's field whole. Empty is unset; a value that does not read or that the checks refuse answers `runner-config-invalid` and nothing runs. Read from the environment of the process that runs `eval`, never from the store |
| `THEOURGIA_RUNNER_C` | `eval-runner.sc` | the operator's runner for `eval --lang c`: one datum that is a whole runner, `argv` and `source-name` at least, replacing the language table's runner entire; nothing of the default survives it. Empty is unset; a value that does not read or that the checks refuse answers `runner-config-invalid` naming this variable, and nothing runs. Read from the environment of the process that runs `eval`, never from the store |
| `THEOURGIA_RUNNER_RUST` | `eval-runner.sc` | the operator's runner for `eval --lang rust`: one datum that is a whole runner, `argv` and `source-name` at least, replacing the language table's runner entire; nothing of the default survives it. Empty is unset; a value that does not read or that the checks refuse answers `runner-config-invalid` naming this variable, and nothing runs. Read from the environment of the process that runs `eval`, never from the store |
| `THEOURGIA_RUNNER_GO` | `eval-runner.sc` | the operator's runner for `eval --lang go`: one datum that is a whole runner, `argv` and `source-name` at least, replacing the language table's runner entire; nothing of the default survives it. Empty is unset; a value that does not read or that the checks refuse answers `runner-config-invalid` naming this variable, and nothing runs. Read from the environment of the process that runs `eval`, never from the store |
| `THEOURGIA_RUNNER_JAVA` | `eval-runner.sc` | the operator's runner for `eval --lang java`: one datum that is a whole runner, `argv` and `source-name` at least, replacing the language table's runner entire; nothing of the default survives it. Empty is unset; a value that does not read or that the checks refuse answers `runner-config-invalid` naming this variable, and nothing runs. Read from the environment of the process that runs `eval`, never from the store |
| `THEOURGIA_RUNNER_TYPESCRIPT` | `eval-runner.sc` | the operator's runner for `eval --lang typescript`: one datum that is a whole runner, `argv` and `source-name` at least, replacing the language table's runner entire; nothing of the default survives it. Empty is unset; a value that does not read or that the checks refuse answers `runner-config-invalid` naming this variable, and nothing runs. Read from the environment of the process that runs `eval`, never from the store |
| `THEOURGIA_SCHEME` | `theourgia.sc`, `core.sc`, `mcp/server.sc` | the Chez binary every program starts its Scheme children with: the thin client's `core.sc` and daemon, the MCP shell's daemon and `eval` child, and `eval`'s worker. A tree started under a particular Chez therefore starts its children under the same one. Falls back to `scheme` |
| `THEOURGIA_TRACE` | `ffi.sc` | `1` writes filesystem and dispatch events to stderr. NOTE: Read once when the library loads, so it is set per PROCESS and cannot be turned on by a call |

**Test-only. Nine of them -- `THEOURGIA_FAULT`, `THEOURGIA_NOFLOCK`,
`THEOURGIA_BARRIER`, `THEOURGIA_HOLD`, `THEOURGIA_HOLD_MS`,
`THEOURGIA_PLATFORM_KEY`, `THEOURGIA_SEND_BUFFER`, `THEOURGIA_READ_CHUNK` and
`THEOURGIA_PROBE` -- are read only
by a build made with `THEOURGIA_INJECT=on`; an ordinary build does not read
them at all.**

| variable | what it does |
|---|---|
| `THEOURGIA_INJECT` | `on` at EXPANSION time builds the fault-injection branches and says `theourgia: EXPANDING WITH FAULT INJECTION ON` on stderr. With it unset or `off` there is no fault code in the object at all -- not a disabled branch, none. Any other value is refused at expansion |
| `THEOURGIA_FAULT` | `<fault>@<stage>` picks which fault, at run time, in a build that has them |
| `THEOURGIA_NOFLOCK` | `1` removes the product's lock while keeping the barrier, so rows asserting mutual exclusion can be shown to fail without it. NEVER: Exists only inside the `THEOURGIA_INJECT=on` branch |
| `THEOURGIA_BARRIER` | `<name>:<fifo>` parks a process at a named point until a controller writes to the fifo |
| `THEOURGIA_HOLD` | the fixtures' hold seam: `<stage>:<path>`, several joined by `;`. At a named stage the process creates `<path>.held`, without recording it, and waits, polling every 20 ms, until `<path>` exists. The stages are `client-scan`, `report-write`, `bind`, `write-after-create`, `publish-after-link`, `store-start`, `after-discovery` (a load, right after discovery), `after-barrier` (a load, between the delivery barrier and delivery), `mcp-child-wait` (the MCP shell, after starting an `eval` child and before its first poll), `eval-admission` (an evaluation, after making its pool's slot files and before it tries their locks) `reload-before-publish` (the daemon's store process, between a refold and its publication) and `read-cut-before-replay` (a read at a cut, after the cut is judged and before the replay to it). An unknown stage or a malformed entry is refused when the library loads |
| `THEOURGIA_HOLD_MS` | how long a hold waits before it goes on anyway and writes `(theourgia hold-expired <stage>)` on stderr: an exact non-negative integer of milliseconds, 30000 when unset; anything else is refused when the library loads |
| `THEOURGIA_PROBE` | `off` starts a daemon without its once-a-second probe for outside changes, so a change folds only when something asks for it (a read, a write, a refresh) and a fixture's frames do not race a timer fold; unset, the probe runs as always; any other value is refused when the library loads. Never set in use |
| `THEOURGIA_PLATFORM_KEY` | `<system>/<machine>[/<libc>]` replaces the platform key the table would select (`platform-numbers.sc`), read once when the table loads, so a fixture can run a FRESH child as if on another platform -- its layouts are built and read back as bytes, never handed to this kernel -- or as an unlisted one, which is refused with exit 75 |
| `THEOURGIA_SEND_BUFFER` | an exact positive integer: a daemon presses each accepted connection's send buffer to that many bytes, so a row can make a write the daemon cannot finish (`subscribe`'s blocked-write rows); refused at load if it is anything else |
| `THEOURGIA_READ_CHUNK` | an exact positive integer: the streaming client reads at most that many bytes at a time, so a row can cut its lines where it likes; refused at load if it is anything else |
| `THEOURGIA_BASE_LIBDIR` | `test/change-stream-guard.sc`'s base: a library root holding the base tree's `theourgia/` and `igropyr/`, whose daemon the guard compares with this tree's. Unset, the guard prints why and runs nothing. Read only by `test/` |
| `THEOURGIA_TEST_ROOT` | where fixtures may create stores and write transcripts. Under `test/run-fixtures.sh` it is a directory the runner makes for the run, `<base>/run-<token>`, and removes at its end. Read only by `test/` |
| `THEOURGIA_TEST_SOCK` | where fixtures put sockets, and the lock file and `serve.log` the product keeps beside one: a directory the runner makes with `mktemp -d /tmp/ths.XXXXXX`, at most 20 bytes so a socket path fits in `sun_path`, and removes at its end. Read only by `test/` |
| `THEOURGIA_SUITE_TOKEN` | set by `test/run-fixtures.sh` to the run's token, so every process the run starts carries it and a leak is counted by it. Read by no library |
| `THEOURGIA_LIBDIR` | read by `test/env.sh` and `test/paths.py`, not by any library. It is what makes a suite reading a PINNED one |
| `THEOURGIA_F54_SKIP_LOAD` | `1` makes `test/startup-ratio.sc` skip C3's loaded pass (one busy loop per core for about 30 s), and C3's rows then read `load-not-granted`, a red. For alone runs on a machine others are using; the suite never sets it |
| `THEOURGIA_A1_CORPUS` | `test/a1-import.sc` reads another Markdown corpus than the in-tree synthetic one (`test/a1/corpus`); the sha256 pins are then not checked. Set by an out-of-tree script that runs the same rows on a private corpus and prints only verdicts and counts. Read only by `test/` |
| `THEOURGIA_A1_EXPECTED` | the directory holding that corpus's expected lists, written by `test/a1/expected.py`; `test/a1` when unset. Read only by `test/` |
| `THEOURGIA_MCP_X8` | `1` makes `test/mcp-shell.sc` run X8, eval past the socket's 30 s answer deadline (about 33 s of sleep); without it the row prints SKIP and runs nothing. Read only by `test/` |
| `THEOURGIA_FIXTURE_LIMIT` | the time limit of one fixture under `test/run-fixtures.sh`, in whole seconds; 900 when unset. Set only by tests that exercise the limit itself. Read by the runner, not by any library |
| `THEOURGIA_RUNNER_NORMALISED` | `test/run-fixtures.sh`'s own marker: the runner sets it to its pid and re-execs itself once through perl with signals 1 to 31, KILL and STOP aside, at their defaults and an empty mask, and unsets it as soon as it is back, so a runner started from inside a fixture does the same. Never set by hand. Read by the runner, not by any library |
| `TMPDIR` | the system's temporary directory, not one of ours. `test/run-fixtures.sh` makes its private snapshot directory there (`mktemp -d -t ths-snap.<token>`), and `test/runner-self.sc` reads it to find that directory. Read by no library |

## KNOWN OPEN

Each of these is a thing this tree does not do, recorded with where it
was found rather than left for a reader to discover.

  * **The machine registry never retires an entry.** Every daemon start
    rewrites `<home>/instances.sexp` whole, and entries for stores that
    have been deleted stay in it marked `active`. Measured on the
    development machine: 319 KB, 5421 records, all `active`.
  * **The export verbs take no `--cut`.** `export-md --working` and
    `export-code --working` stand on the writer's pinned baseline, as
    `eval --working` does without options; a historical cut with drafts
    over it (`eval --cut ... --working`) has no export. The rows for the
    working exports are in `test/export-working.sc`.
  * **Twenty-two fixtures define `want` as a procedure**, which evaluates
    both arguments before the call: a row that raises ends the file
    rather than failing. `run-fixtures.sh` counts and names them on every
    run. New fixtures use the `want`/`caught` macro pair instead.
