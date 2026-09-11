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

* `ok`, or a replay of the same request: the entry is dropped and the cursor moves.
* `(error unknown <why>)`, a timeout, or a core that printed nothing: the entry is
  **kept**. The store may hold the record; asking again with the same request id is the
  only way to find out. "theourgia: Retry Pending Saves" sends the same bytes again —
  the same id, the same cursor, the same body, not whatever the buffer now holds.
* Any other refusal: the entry is dropped and the refusal is shown, because retrying
  cannot change it.

Saves are sent one at a time, and an entry nobody can resolve holds the ones behind it
rather than being stepped over.

The first cursor, before any write has been answered, comes from `check`: a store with one
writer has no ambiguity. `check` does not say which writer is local, so a store with more
than one — one that was adopted or copied — refuses to be written to from here rather than
guessing.

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

The cells that need the core **fail** when it is not there; they do not skip. A skip and a
pass are the same colour, and the two situations — "this works" and "nobody has checked" —
should not be.
