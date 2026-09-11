# theourgia for VS Code

Browse a [theourgia](https://github.com/guenchi/theourgia) block store in the side bar,
open a block in an editor, and save it back as one `set`.

Licensed under the Apache License, Version 2.0. See LICENSE.

## What this batch does

* An **outline tree**: the top level comes from `outline --depth 1`, and opening a node
  asks `read <id> --recursive` and shows the blocks directly under it. A block in a
  structural conflict is marked, and so is one whose parent is gone.
* **Opening a block**: `read <id>` gives the block, and its `heading-src` and `src`
  fields are put in a markdown buffer, in that order. One file per (store, block), so
  opening a block twice reaches the same document.
* **Saving**: one `set <id> src <body>` carrying a request id and a cursor, sent through
  an outbox that is written to disk before the request goes out.
* A **status bar** entry with the store, the actor, the cursor, the number of conflicts
  and the number of saves whose outcome is not known.

## What it does not do yet

* No graph view, no link editing, no title editing. Editing the heading line of a block
  is refused with a message rather than half-applied; changing a title is
  `set <id> title`, which is not in this batch.
* No daemon. `theourgia.transport` has a `socket` setting and selecting it refuses every
  request, rather than falling back to the command line and making the setting a lie.
* No `--if-unchanged`. The core does not yet return a block's hash from `read`, so a save
  cannot say which version it was composed against. Until it does, two people editing one
  block from two machines will not be told; the store still records both records.
* No cache. Every view asks the core.

## Settings

| Setting | What it is |
|---|---|
| `theourgia.corePath` | The directory holding `cli.ss`. Required. |
| `theourgia.libDirs` | Extra directories for `CHEZSCHEMELIBDIRS`, after `corePath`. The core imports `(igropyr crypto)`, `(igropyr platform)` and `(igropyr sexpr)`, so the directory holding `igropyr/` belongs here or the core exits before reading an argument. |
| `theourgia.store` | The store directory, passed as `--store`. Required. |
| `theourgia.actor` | The name recorded with every write. Defaults to the OS user name. |
| `theourgia.scheme` | The Chez Scheme executable. Defaults to `scheme`. |
| `theourgia.timeoutMs` | How long one request may take before the child process is stopped. Defaults to 30000. |
| `theourgia.transport` | `cli` (the default) or `socket` (not implemented). |

## How a request is made

The command line takes its verb from the first argument and only then scans for options,
so the argument vector is

    <scheme> --script <corePath>/cli.ss <verb> <args...> --store <store> --actor <actor>

An option placed before the verb is taken *as* the verb, and the core answers
`(error unknown-verb ...)`.

`cli.ss` prints three kinds of answer and marks none of them: an `(ok (text ...))` answer
is printed as its own bytes, an `(ok (items ...))` answer as one datum per line with no
wrapper, and everything else as one datum. So this extension keeps a table of which verb
answers which way (`src/client.ts`), and that table is the one place where it holds an
opinion the core also holds. Over a socket the whole answer would arrive and the table
would not be needed.

**The exit code is the verdict.** An answer beginning with `ok` that came with a non-zero
exit is not a confirmed write.

## Saving, and what happens when the answer is lost

A save is written into the outbox — request id, cursor, block, body — **before** it is
sent. Then:

* `ok` naming the record it wrote: the entry is dropped and the cursor moves.
* `ok` naming **no** record: the entry is **kept** and this is reported as a defect. Every
  write the core accepts says which record it appended, and an answer with neither a
  `cursor` nor an `event` leaves the next save with nothing to be composed against.
* `(error unknown <why>)`, a timeout, or a core that printed nothing: the entry is
  **kept**. The store may hold the record; asking again with the same request id is the
  only way to find out. "theourgia: Retry Pending Saves" sends the same bytes again —
  the same id, the same cursor, the same body, not whatever the buffer now holds.
* Any other refusal: the entry is dropped and the refusal is shown, because retrying
  cannot change it.

Saves are sent one at a time, and an entry nobody can resolve holds the ones behind it
rather than being stepped over. That serialisation belongs to the queue file rather than to
any one object: the extension builds a fresh saver whenever a setting changes, over the same
outbox, without waiting for the old one to finish.

The queue itself never changes before the file does: every change is written and only then
adopted, because a queue that changed in memory and not on disk is worse than one that
changed in neither — the next call sees the new state, believes it was recorded, and acts
on it. The file is written, flushed, renamed and the directory flushed, so that what
survives a machine losing power is what the user was told had been recorded. And only a
file that is *not there* is an empty queue: every other reason a read can fail leaves open
the question of what was recorded, so the outbox refuses to be written to at all rather
than replacing a file it could not read.

An entry is `queued` until the moment it goes out and `sent` from then on, and that is
written down before the request leaves. A queued entry may still have its cursor corrected
to the position the previous answer established; a sent one may not, because the cursor is
part of the request's identity and changing it would turn a retry into a different request
wearing the first one's id. A host killed between the send and the answer leaves a `sent`
entry behind, and that is the state the distinction exists for.

The first cursor, before any write has been answered, comes from `check`: a store with one
writer has no ambiguity. `check` does not say which writer is local, so a store with more
than one — one that was adopted or copied — refuses to be written to from here rather than
guessing.

## The file a block is edited in

One file per (store, block), so opening a block twice reaches the same document. Beside it
is a marker recording the digest of what the store last agreed the file held.

**The editor's dirty flag does not answer "is there work here".** The save handler runs on
`onDidSaveTextDocument`, after the bytes have reached disk — so a save the core *refused*
leaves a clean buffer holding text the store has not got, and reopening the block would
overwrite it. A second editor window computes the same path for the same block and is
invisible to this one. The marker sees both: a file whose contents do not match it is not
reloaded from the store, and the user is told why.

## What it refuses to guess at

An answer in a shape this client has not met stops the request rather than being trimmed
to the part it understood. An outline line that does not parse, a block field in an
unfamiliar shape, an answer line that will not read — each of those, skipped, would show a
store with one block missing, a block with one field missing, or a shorter list of
conflicts, and every one of those reads like good news. The same goes for the status bar:
a conflict count that could not be fetched shows as unknown, never as zero, and changing
the store setting clears it rather than carrying the old store's answer across.

Requests are awaited, and a setting can change while one is in flight. Every request
records which generation of the settings it was made under and is dropped if that
generation has been replaced — otherwise a block read from one store would be written into
a file named after another, and saved into it.

**Two cases the outline refusal does not catch**, both reproduced on a real store. A title
ending in `  conflict` is read as a conflict mark, so a sound block is shown as conflicted.
A title whose second line looks like a row (`second line\n- fake.1  invented`) produces a
row for a block that does not exist, with no refusal at all. The root cause is that the
outline is a text rendering with no escaping, and the fix belongs in the core; both are
pinned in `test/unit/shapes.test.ts` as what this client does until it lands.

## The s-expression reader

`src/vendor/goeteia/sexpr.mjs` is a verbatim copy of goeteia's `rt/sexpr.mjs`, held to
`sexpr-vectors.json`, the golden fixture generated from `(igropyr sexpr)` — the authority
for this wire format. The copy exists only because goeteia's package does not export the
deep path yet; when it does, the copy goes and a dependency takes its place. Do not edit
it. `test/unit/vendor-sexpr.test.ts` sweeps the whole read side of the fixture.

**Known gap.** That reader accepts the string escapes `\n \t \r \" \\`; Chez's writer,
which is what `cli.ss` prints with, also emits `\a \b \f \v` and `\xHH;`, and escapes
awkward symbols the same way. A block whose body contains a form feed is therefore stored
happily by the core and cannot be read back by this extension. Widening the reader is
queued upstream in goeteia; `S14` in `test/unit/real-core.test.ts` is red until it lands
and is the cell that will notice.

## Running the cells

```sh
npm install

# the parser, transport, outline, block, cursor and save cells
npm run test:unit

# the same, plus the ones that need the core itself (O3, S7, S14)
THEOURGIA_CORE=/path/to/theourgia THEOURGIA_LIBDIRS=/path/holding/igropyr npm run test:unit

# the cells that need an editor
THEOURGIA_CORE=/path/to/theourgia THEOURGIA_LIBDIRS=/path/holding/igropyr npm run test:integration
```

**This extension needs a core that has request tracking.** Every save carries `--req <id>
--cursor <writer>:<seq>`, and a core without those options answers `(usage (set <id> <field>
<value>))` — measured against theourgia at 842cf46, where the tokens `--req`, `--cursor`,
`tracked-request` and `req-not-tracked` do not appear in `cli.ss` or `rpc.ss` at all. The
outbox is built on that feature, so against such a core every save is refused. A usage line
can also mean an argument the core did not expect, so the message names both.

**A nested document is reported but not shown.** The core lists one under `conflicts`, and
its recursive walk stops at a doc-kind child — `project.ss` says "a walk stops at one",
because a nested document has its own file and descending would write its sections twice. It
is therefore absent from its parent's expansion, and `nested-document` is not a mark that
puts a block in the root listing either, so the only sign of one is the conflict count. What
a nested document *means* is still open in the core's own design; this is pinned as current
behaviour, not endorsed.

**Point `THEOURGIA_CORE` at a copy nobody is editing.** The core is somebody else's working
tree, and a suite that reads one is only as stable as the editing going on in it — a run of
these cells once reported `the core exited 255 without an answer`, and a probe built to fish
for it caught `Exception: variable t is not bound` at the exact second another session wrote
`store.ss`. Neither was a defect in the core; both were a half-written file being read. The
cells take a digest of the core at the start and check it at the end, so a reading taken
against a moving tree says so instead of looking like a flake.

The cells that need the core **fail** when it is not there; they do not skip. A skip and a
pass are the same colour, and the two situations — "this works" and "nobody has checked" —
should not be.
