# theourgia

Agent-and-human shared block-graph knowledge base. R6RS Scheme; Chez Scheme is the reference platform, and the portable core is meant to compile to wasm and to move to other implementations. Platform-specific code (file primitives, locks) lives in its own library.

Licensed under the Apache License, Version 2.0. See LICENSE.

Design documents are maintained separately; this repository holds the implementation and its tests.

## Installing

**One set of prerequisites: Chez Scheme, and igropyr.** There is no
Python anywhere in this, and no build step -- the verbs run from source:

    scheme --script theourgia.sc <verb> [...]

**Three programs, one per role.** `theourgia.sc` is the command you run.
It answers through a daemon, and starts the daemon program,
`theourgiad.sc`, beside itself when none is running; `init`, `eval` and
any request under `THEOURGIA_LOCAL=1` it hands to `core.sc`. `core.sc`
answers one request in its own process -- the in-process route, which
forwards to a running daemon unless `THEOURGIA_LOCAL=1`. `theourgiad.sc`
takes only `serve`.

Chez finds the libraries through its own two variables, which must name a
directory holding both `theourgia/` and `igropyr/`:

    export CHEZSCHEMELIBDIRS=/path/to/that/directory
    export CHEZSCHEMELIBEXTS=".sc::.sls::.scm"
    scheme --script theourgia.sc init --store /path/to/store

**Running it compiled.** `build.ss` compiles every library -- this tree
and its dependency -- into a directory of objects:

    scheme --script build.ss <library-root> <output-root>

where both arguments are the directory that *contains* `theourgia/`.
Point `CHEZSCHEMELIBDIRS` at the output and `CHEZSCHEMELIBEXTS` at
`.so`. NOTE: Objects start about twelve times faster than source, because
they do not re-expand the libraries on every call.

NEVER: **The products do not belong in the source tree.** A stale `.so`
beside a `.sc` is resolved in preference to it, so a tree holding both
can be running code nobody has edited for a week.

NOTE: **Packaging the whole program into one file is not done yet.** That
form would drop a library nothing statically references -- and `eval`
and forwarding are reached at run time by name, so they would stop
resolving; so would the daemon, which `theourgiad.sc` loads by name once
its arguments are checked.
It is recorded as F13; until then the shipped form is a directory of
objects, which those routes do work in.

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
each verb's line because repeating them thirty-three times would say they
were somehow special to each one.

| option | what it does |
|---|---|
| `--store <dir>` | which store. Falls back to `THEOURGIA_STORE`, then `.` |
| `--actor <name>` | who the request is from. Falls back to `USER`, then `cli` |
| `--socket <path>` | reach a daemon at this path instead of the default |
| `--wire` | print the answer as one S-expression per line rather than for a person to read |
| `--req <id>` | the request's identity, so a retry is recognised as the same request rather than a second one |
| `--cursor <w:n>` | the position this request is composed against |

`THEOURGIA_LOCAL=1` answers in this process even when a daemon's socket
is there. NOTE: It is a debugging path: it skips the daemon rather than
doing something the daemon cannot.

## Writing for agents

A block is the unit of writing: one block should answer one question on its own.
Keep a block under about 800 tokens (roughly 3000 bytes); split a longer one.
Give every block a one-sentence title.
Give every block 3 to 8 keywords, comma separated, with --keywords.
Place a block under the parent its source or subject puts it under, with --under.
Do not rewrite the source bytes: splitting a document must not edit its prose.
Change a block with write and then commit, through a draft, rather than replacing it.
Hold a writer id from one agent at a time: a later session may bind the same id and carry on with its drafts, but two agents writing one draft at once overwrite each other silently.

## Reading a store

Every verb prints one S-expression per item on stdout; the exit code is the verdict.
**Empty output with a zero code is an answer** — no references, no hits, no
differences — and is never an error.

### `read`

    (read <id> ("--md") ("--recursive") ("--writer" <name>)
          ("--working") ("--working-info"))

    (ok (<block>))        |  (ok (items <block> ...))  |  (ok (text "<markdown>"))

What a block says. Without options it is one block as data; `--md` gives its own
bytes — the heading line it was written with, then its body.

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
is made of rather than changing what is read.

### `refs`

    (refs <id>)

    (ref (from <id>) (rel <rel>) (via link|md))

What points **at** this block, from the two places a reference can live. A `link`
record is an edge someone wrote and `unlink` removes it; a reference in the text is a
sentence, and nothing removes it but editing the sentence. Each line says which it
came from, and they are never merged: you can unlink an edge, you cannot unlink a
sentence. Text references are derived at read time rather than stored as edges,
because materialising them would give one fact two suppliers that can then disagree.

A block's own out-edges are not references to it; those are part of what `read`
returns about the block.

### `search`

    (search <query> ("--all"))

    (hit <id> <score> "<snippet>" (fields (<field> ...)))
    --all           every hit, not just the best ten

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

Only a `code` or `library` block has names at all -- a name is something defined
or carried, not a field anyone may set -- so `search` and `whereis` ask the same
question of the same blocks. A deleted block is not searched: the set of blocks
that exist is the outline.

#### What a `--wire` answer says about the scan

`search` and `grep` carry, after their items:

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
answer with items writes the items and nothing else.

### `grep`

    (grep <pattern> ("--under" <id>) ("--all"))

    (match <id> <line-no> "<text>")
    --under <id>    only the block given and what is under it
    --all           no limit on how many lines come back

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


Order is score descending then id ascending — total, so two runs over one store agree.
The query is only ever text: nothing in it reaches a numeric parser.

### `whereis`

    (whereis <name>)

    (def <id> (library <lib>) (name <sym>) (kind code))
    (export <lib-id> (library <lib>) (name <sym>))

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

    (entry (event <writer> <seq>) (ts <ms>) (actor "<name>") (verb <verb>))

What this store has applied, in the order it was delivered. Given an id, only the
records that **named** that block: a record whose premises mention it has not touched
it, and neither has one whose text mentions it.

This verb replays the whole log rather than reading from the snapshot, and is
therefore the slow one. Two facts force it: the timestamp and actor exist only as a
record is delivered, and a snapshot-seeded read never delivers what the snapshot
already covers — which would produce a log that silently began in the middle.

### `tag`

    (tag (<name>))

    (tag (name "<name>") (cut ((<writer> . <seq>) ...)))

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
refused as `cut-unavailable` rather than truncated to the part it can reach.

### `describe`

    (describe)

What the verbs are, what each is for, and the protocol for writing. Answers

    (ok (verbs (<verb> (usage <form>) (description <text>) (protocol <bool>)) ...)
        (protocol "<the writing protocol>"))

for something that has to ask rather than be told: the MCP shell builds its
tool list from this, so a tool description and the verb it describes cannot
drift apart.

It reads a table and runs nothing. NEVER: It does not open the store, take a lock
or write a byte.

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
runs in its own process. `init` is `local`, because it is what creates the
store there would otherwise be nothing to send to.

The `protocol` flag on an entry says that verb's description carries the
writing protocol text. It is set on `insert` and `write`. NOTE: It does not mean
"this verb changes the store" — `set` does that and is not marked.

### `conflicts`

    (conflicts)

    (conflict <id> cycle|unplaced) | (orphan <id>) | (pending (event <w> <seq>) (missing <w> <seq>))

What the store holds and cannot show: blocks in a structural conflict, blocks whose
parent was deleted or never arrived, and records still waiting for premises. A record
waiting on several premises is listed once for **each** — stopping at the first would
send an operator to fetch one record and leave them where they started. A section with
nothing in it prints nothing.

The structural conflicts are read from the same place `outline` reads them, so the two
cannot disagree; `outline` prints the mark as a fourth column on that row.

## Making and changing blocks

Every verb here writes, and every write is one request with one answer.
The store's lock is taken for the write and released; nothing is held
across two verbs.

### `init`

    (init)

Creates a store in the directory named by `--store`, and answers with the
store's id and the writer the caller was given.

### `insert`

    (insert "--under" <id> ("--after" <id>) "--title" <text> ("--text" <text>)
            ("--keywords" <text>))

Adds a block under an existing one. `--after` places it among that
parent's children; without it the block goes last. `--text` gives the
block its `src` in the same request.

`--keywords` gives it the words it should be found by -- three to eight
of them, comma separated, as the protocol above says. NEVER: The value is
stored as TEXT, exactly as it was written: `read` gives back the same
string, spacing and commas included. Splitting it into words happens in
`search`, which is the only reader that needs them; a store holding a
normalised form could not give back what was sent. `set <id> keywords
<text>` changes it afterwards.

### `set`

    (set <id> <field> <value> ("--if-unchanged" <version>) ("--based-on" <version>))

Replaces one field of one block. `--if-unchanged` makes the write
conditional: the store refuses it if the block has moved on from that
version, so a caller that read, thought, and came back cannot overwrite
what happened in between.

Most fields take the text as given. `kind` does not: it is a symbol from a
fixed set -- `code`, `section`, `file`, `doc`, `library`, `decision` -- and a
spelling outside that set is refused, with the legal set in the answer, rather
than stored. A kind stored as text would match nothing and the block would
simply stop behaving like what it said it was. The same set applies to a kind
written through `batch`, and to nothing else: a record already in a store
keeps whatever kind it carries, including one a later version introduced.

### `move`

    (move <id> <parent> ("--after" <id>))

Re-parents a block. `root` is spelled as the word, not as an id.

### `del <id>`

    (del <id>)

Marks a block deleted. The block and its history stay in the log -- what
changes is what the outline and the reads answer.

### `link <from> <rel> <to>`

    (link <from> <rel> <to>)

Adds a typed edge between two blocks. `<rel>` is a name of the caller's
choosing; the store does not interpret it.

### `unlink <from> <rel> <to>`

    (unlink <from> <rel> <to>)

Removes that edge. NOTE: The three positionals are the same three `link`
takes, in the same order, and getting them out of order is not an error
the store can see.

### `def`

    (def <name> ("--under" <library>) <source>)

Defines one datum by name, optionally inside a library block.

### `outline`

    (outline ["--depth" <n>] ["--with-keywords"])

Prints the store's block tree as indented text: one line per block, its
`<id>.<version>` and its title. `--depth` stops at that many levels.

`--with-keywords` appends `  [<keywords>]` to each row that has them. NOTE:
A block without the field prints no brackets: empty ones would say the
writer chose no keywords, which is a different thing from a store written
before the field existed. Without the option the listing is unchanged.

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
of it** rather than sharing the id:

    theourgia drafts --writer w1              # read the version
    theourgia restore <version> --writer w2   # same bytes, under w2

From there each writes its own, and the two meet at `commit` through
`--based-on`.

NEVER: **There is no machinery behind this rule** -- no record of who holds an
id, no lock, and nothing refuses a second process. It is a convention,
stated here because the failure it prevents is silent.

### `write`

    (write <block> <bytes> ("--writer" <name>) ("--based-on" <version>)
           ("--working-cut" <cut>) ("--working-parent-writer" <name>)
           ("--working-parent" <version>) ("--rebase"))

Puts a draft in that writer's slot for that block. A draft's version is
the name of its content -- `sha256(bytes || based-on || cut)` -- so the
same bytes written twice are the same version. `--rebase` moves a draft
onto a newer committed parent.

### `restore`

    (restore <version> ("--writer" <name>))

Takes a version's bytes out of the log and puts them back as a draft.
NOTE: It is its own verb rather than a flag on `write`, because it takes a
version and NO bytes: the bytes come from the log.

A version is looked up in that writer's own revoked consumptions -- the
drafts a commit took and a later retraction gave back -- so `--writer`
selects whose list is searched, and the same version can be unknown to
one writer and restorable by another.

### `drafts`

    (drafts ("--writer" <name>))

Lists that writer's live drafts. Each carries `block`, `writer`,
`version`, `based-on`, `now` (what the committed block is at this
moment), `fresh` (whether `based-on` is still `now`) and `unchanged`
(whether the draft's bytes differ from what is committed).

### `discard`

    (discard <block> ("--writer" <name>))

Drops that writer's draft for that block. The committed block is
untouched.

### `commit`

    (commit (<block> ...) ("--writer" <name>) ("--working-version" <block>=<version>))

Turns drafts into committed versions. Naming no block commits all of that
writer's drafts. `--working-version` may be given once per block to say
which version of the draft is being committed; with exactly one block
named, the bare version may be given.

NOTE: **The answer may carry `(behind ((<writer> . <seq>) ...))`** -- other
writers who landed after this writer's drafts were taken. It is
informational, and it is **absent** when nothing moved rather than
present and empty: a field that is always there says nothing.

## Importing, exporting, and splitting

### `import-md`

    (import-md <dir> ("--allow-delete"))

Reads a directory of Markdown into the store. Without `--allow-delete` a
file that has disappeared from the directory leaves its blocks alone.

### `export-md`

    (export-md <dir> ("--with-ids"))

Writes the store out as Markdown. `--with-ids` keeps each block's id in
the text, so the result can be imported back onto the same blocks.

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

A document's `path` is checked before anything is written, by the same rule
the code projection uses: it must be relative and non-empty, with no `.`,
`..` or empty component, no NUL and no backslash. A path that fails, or that
leads out of the directory you named once symlinks are resolved, is refused
with `path-not-usable` and the path you gave quoted back, and nothing is
written for it.

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

    (import-code <dir> ("--allow-delete") ("--datum"))

Reads a directory of source into the store. `--datum` reads it as data --
one block per top-level form -- rather than as text. With `--datum`, the
whole-line ; comments directly above a form become its doc; a ; comment
inside a form is dropped, and the answer warns with its line and column. A
#| |# block comment, and any comment inside a datum discarded with #;, is
dropped with neither.

### `export-code`

    (export-code <dir> ("--raw") ("--datum"))

Writes the store out as source. NEVER: `--datum` and `--raw` together are
refused with `(error bad-request incompatible-projection-options)`: they
are two different projections and there is no answer to "both".

### `split-suggest`

    (split-suggest <file> ("--output" <review-file>))

Proposes where a file could be split into blocks. It suggests; it does
not write.

## Checking, snapshotting, adopting

### `check`

    (check)

Reads the store and reports what does not hold together.

### `snapshot`

    (snapshot)

Writes a snapshot of the current reduction and answers
`(ok (snapshot <n>) (cut <cut>))`. Later opens start from it instead of
replaying the whole log.

### `adopt`

    (adopt)

Takes over a store that belongs to another machine's registry entry --
after a move or a restore, where the store is here but its recorded owner
is not.

## Requests and replay

The block-editing verbs — `insert`, `set`, `move`, `del`, `link`, `unlink` —
accept a request identity. A client that did not hear the answer can send the
same request again and be told what happened, rather than having to choose
between doing the work twice and not doing it at all.

`batch` carries one too, and is covered below. So does `tag <name>`, which
writes a record — but not `tag` with no argument, which only lists what is
there. Trackability is a property of the **request**, not of the verb's name.

**Every other request refuses the options** — `(error bad-request
req-not-tracked <verb>)`. The store-level verbs and the read-only ones have no
replay to offer, and taking an identity only to drop it would leave a caller
believing it was protected. Refusing says so.

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
bad-request malformed-cursor)`. It is spelled `--cursor` and not `--after`
because `--after` already names the sibling a new block is placed behind, on
`insert` and `move`; one spelling cannot carry both, because the dispatcher
must consume the retry cursor before any verb sees its arguments. An id outside the permitted format is `(error bad-request
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

### `batch`

    (batch <intents>)

Intents are read from standard input, one wrapping list or several top-level
forms:

    echo '((insert root #f ((kind . section) (title . "One")))
           (insert (from 0) #f ((kind . section) (title . "Two"))))' \
      | theourgia batch

A field is a **pair**: `(title . "One")`. Written `(title "One")` the value is
the list `("One")`, which is a different thing and is stored as one.

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

An item the store will not accept is answered `(error malformed-intent
(<reason> <what-was-sent>))` for that item — never raised. The reason names the
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
`ord-not-a-number` — and what is shown is the payload rather than the item.

A form headed by a symbol this build does not know is a different answer,
`(error unknown-verb <verb>)`: it may be a record shape a newer build writes,
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
replay. A tracked request that produces **no** intent appends nothing at all.

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
the segment published.

The hash is the **sender's declaration**. Given one, it is checked against the bytes
before anything else happens, so a segment that was damaged in transit is refused
rather than installed and discovered later by a reader. Given none, the bytes on
disk are taken as their own: computing the hash here from the same bytes would
compare a number with itself.

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

Both replacements name the file the displaced bytes were copied to. The sender is the
one party that may want them, and deriving that name on its side would be a second
supplier of it — the two would part company the first time the naming rule changed.

The rest refuse, and exit non-zero:

| answer | meaning |
|---|---|
| `(incomplete <n>)` | the candidate is a prefix of what is already here; accepting it would discard records. |
| `newer-history-unmergeable` | the candidate is longer than a sealed segment, which is not a segment this store may replace. |
| `(divergence (fork <seq>))` | a record disagrees with one this writer's history already holds; the fork is recorded at the lowest such sequence and never moves up. |
| `(refused insufficient-coverage (seq <n>))` | the local copy is damaged, but the candidate does not cover record `<n>`, which is still valid here. Absence is not evidence of disagreement. |
| `(refused active-writer-segment)` | the segment is this store's own active writer's current segment. This is checked above every other rule. |
| `(refused retirement-coordinates <detail>)` | the candidate would move the byte offset at which the retired prefix's last sequence ends, so the declaration would silently stop being true. |
| `(error invalid-candidate sha-mismatch)` | the bytes do not hash to the declared value. |
| `(error invalid-candidate not-contiguous)` | the candidate's own sequences jump, repeat or go backwards. |
| `(segment-layout-conflict <reason>)` | the candidate cannot sit at this segment number. The reasons are below. |

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

A refused candidate is **kept, not installed**. Its bytes are written under
`writers/<writer>/incoming/`, named by their own hash, with an empty marker file
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

Every call that makes something durable names the stage it belongs to —
`deliver-barrier`, `commit`, `registry`, `publish`, `snapshot`, `repair` — and the
stage is a required argument, not a decoration:

    (fsync! fd subject stage)
    (fsync-dir! path stage)
    (atomic-write! path bytes stage)
    (directory-entry-durable! path stage)

Fault injection matches on the stage, and a staged fault never matches a call that
declares none. That makes the list of stages **the list of steps a test can reach**:
an unlabelled flush is not merely untested, it is untestable, and in every log that
is indistinguishable from a step that passed. Two such calls sat in the publish path
until a case was written that tried to arm them. Making the stage a required argument
is what turns the rule from something to remember into something that cannot be
written wrongly; `test/stage-gate.sc` reads the sources and checks that every one of
these calls names a stage from the list.

Run the suites from source (igropyr must be a sibling checkout):

    sh test/run-all.sh

## Evaluating against a store

### `eval`

    (eval ("--cut" <cut>) ("--under" <library>) ("--working") ("--latest")
          ("--writer" <name>) ("--timeout-ms" <n>) ("--memory-bytes" <n>)
          ("--output-bytes" <n>) <source>)

Evaluates one expression against what the store holds, in a child process
that is given nothing else: no filesystem, no network, no way to reach
the committed store except by reading it.

**The answer is one complete line**, always, and one of:

    (ok (values (<v> ...)) (working-view <writer> <cut> <drafts>)
        (stdout "...") (stderr "..."))
    (error eval-limit (resource time|memory|output) (limit <n>) (stdout "..."))
    (error eval-exception (kind raised) (message "..."))
    (error eval-exception (kind raised) (value <v>))
    (error eval-exception (kind raised) (reason unwritable-value) (type <t>))
    (error bad-request (reason ...) (usage (eval ...)))

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

**The limits, and their bounds**: `--timeout-ms` 1..60000 (default 3000),
`--memory-bytes` 1 MiB..2 GiB (default 256 MiB), `--output-bytes`
128..1 MiB (default 65536). The output quota counts DECODED user bytes
across both streams -- what the user printed, not what the framing made
of it. NOTE: The memory budget is read by sampling the child's resident size
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
`sun_path` holds 104 bytes on macOS and FreeBSD, and a store may sit
anywhere and be arbitrarily deep, so a store-adjacent socket under a long
path simply fails to bind and the daemon reports `listener-down`.

**Detaching.** `--detach` is for a launcher, not for a person: the
process leaves the caller's session and replaces its standard streams,
and it does NOT fork. Typed at a prompt it stops there and prints
nothing, because the output it would have shown has already gone to the
log. It requires `--log <path>`, and refuses without one -- a detached
daemon with nowhere to write is one whose every startup refusal is lost,
and the client that started it could then only report that it did not
come up. The log is opened for APPEND, so several starts against one
store share it and a failed start's reason is still there afterwards.
In order: leave the session, replace the streams, then take the lock and
open the socket. A `serve` without `--detach` does none of it.

**Starting when one is already there.** The lock attempt never waits: a
second daemon answers `(error serve-busy (path <socket>))` and exits 75.
If the socket path is occupied by something that is not a socket it
answers `(error serve-path-occupied (path <socket>))` and exits 75. A
LEFTOVER socket file whose daemon is gone is cleared and taken over --
the lock, not the file, is what decides which of those two it is. When it
is up it reports `(serving (store <path>) (socket <path>))`.

**`SIGTERM` drains; a second one stops.** The first signal lets work that
has already begun run to its end, within a budget of five seconds, while
refusing new requests with `(error draining)`. A second signal stops
immediately -- a drain that is taking too long is exactly when somebody
needs to be able to end it.

**`(error store-busy (path <path>))`** is an answer, not a crash: a
request waited for the store's lock past its five-second budget because
something else was holding it. That is an ordinary state of the world.

**`THEOURGIA_TRACE=1`** makes the daemon write its filesystem and
dispatch events as `(trace <op> <path> <detail>)` lines on stderr.

## The MCP shell

`theourgia-mcp` speaks MCP `2025-11-25` over stdio in front of the same
dispatcher, one tool per verb in `rpc-verbs`. See
[`mcp/README.md`](mcp/README.md) -- what a tool returns, why a core
refusal comes back as a successful result, and how the shell branches on
the transport's tag rather than on the answer's text.

## Environment variables

| variable | read by | what it does |
|---|---|---|
| `CHEZSCHEMELIBDIRS`, `CHEZSCHEMELIBEXTS` | Chez itself | where the libraries are found. Not read by any source file here. |
| `THEOURGIA_STORE` | `core.sc`, `theourgiad.sc` | the store to use when `--store` is absent. Falls back to `.` |
| `THEOURGIA_ACTOR` | `core.sc`, `mcp/server.sc` | who the requests are from. Falls back to `USER`, then `cli` |
| `THEOURGIA_HOME` | `ffi.sc` | where the machine registry and its lock live. Falls back to `HOME` |
| `THEOURGIA_RUN` | `daemon.sc` | the run root holding daemon sockets. Falls back to `$HOME/.theourgia/run` |
| `THEOURGIA_LOCAL` | `core.sc` | `1` answers in process even when a daemon's socket is there |
| `THEOURGIA_SCHEME` | `core.sc` | the Chez binary to start `eval`'s worker with, so a tree started under a particular Chez starts its children under the same one. Falls back to `scheme` |
| `THEOURGIA_TRACE` | `ffi.sc` | `1` writes filesystem and dispatch events to stderr. NOTE: Read once when the library loads, so it is set per PROCESS and cannot be turned on by a call |

**Test-only. Five of them -- `THEOURGIA_FAULT`, `THEOURGIA_NOFLOCK`,
`THEOURGIA_BARRIER`, `THEOURGIA_HOLD` and `THEOURGIA_HOLD_MS` -- are read only
by a build made with `THEOURGIA_INJECT=on`; an ordinary build does not read
them at all.**

| variable | what it does |
|---|---|
| `THEOURGIA_INJECT` | `on` at EXPANSION time builds the fault-injection branches. With it unset or `off` there is no fault code in the object at all -- not a disabled branch, none |
| `THEOURGIA_FAULT` | `<fault>@<stage>` picks which fault, at run time, in a build that has them |
| `THEOURGIA_NOFLOCK` | `1` removes the product's lock while keeping the barrier, so rows asserting mutual exclusion can be shown to fail without it. NEVER: Exists only inside the `THEOURGIA_INJECT=on` branch |
| `THEOURGIA_BARRIER` | `<name>:<fifo>` parks a process at a named point until a controller writes to the fifo |
| `THEOURGIA_HOLD` | the fixtures' hold seam: `<stage>:<path>`, several joined by `;`. At a named stage the process creates `<path>.held`, without recording it, and waits, polling every 20 ms, until `<path>` exists. The stages are `client-scan`, `report-write`, `bind`, `write-after-create`, `publish-after-link` and `store-start`. An unknown stage or a malformed entry is refused when the library loads |
| `THEOURGIA_HOLD_MS` | how long a hold waits before it goes on anyway and writes `(theourgia hold-expired <stage>)` on stderr: an exact non-negative integer of milliseconds, 30000 when unset; anything else is refused when the library loads |
| `THEOURGIA_TEST_ROOT` | where fixtures may create stores and write transcripts. Under `test/run-fixtures.sh` it is a directory the runner makes for the run, `<base>/run-<token>`, and removes at its end. Read only by `test/` |
| `THEOURGIA_TEST_SOCK` | where fixtures put sockets, and the lock file and `serve.log` the product keeps beside one: a directory the runner makes with `mktemp -d /tmp/ths.XXXXXX`, at most 20 bytes so a socket path fits in `sun_path`, and removes at its end. Read only by `test/` |
| `THEOURGIA_SUITE_TOKEN` | set by `test/run-fixtures.sh` to the run's token, so every process the run starts carries it and a leak is counted by it. Read by no library |
| `THEOURGIA_LIBDIR` | read by `test/env.sh` and `test/paths.py`, not by any library. It is what makes a suite reading a PINNED one |
| `THEOURGIA_F54_SKIP_LOAD` | `1` makes `test/startup-ratio.sc` skip C3's loaded pass (one busy loop per core for about 30 s), and C3's rows then read `load-not-granted`, a red. For alone runs on a machine others are using; the suite never sets it |
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
  * **`W11-export-working` and `W11-cut-plus-working` have no cells.**
    They belong to verbs the working-view fixture does not drive
    (`export-code`, and `eval --cut` combined with a view beyond the one
    row that covers it). Recorded in `test/eval-working.sc` beside the
    rows that do exist.
  * **Twenty fixtures define `want` as a procedure**, which evaluates
    both arguments before the call: a row that raises ends the file
    rather than failing. `run-fixtures.sh` counts and names them on every
    run. New fixtures use the `want`/`caught` macro pair instead.
