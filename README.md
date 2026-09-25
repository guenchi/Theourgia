# theourgia for VS Code

Browse a [theourgia](https://github.com/guenchi/theourgia) block store in the side bar,
open a block in an editor, and save through a durable working draft and selected `commit`.

Licensed under the Apache License, Version 2.0. See LICENSE.

## What this batch does

* An **outline tree**: the top level comes from `outline --depth 1`, and opening a node
  asks `read <id> --recursive` and shows the blocks directly under it. A block in a
  structural conflict is marked, and so is one whose parent is gone.
* **Opening a block**: `read <id>` gives the block, and its `heading-src` and `src`
  fields are put in a markdown buffer, in that order. One file per (store, block), so
  opening a block twice reaches the same document.
* **Saving**: `write` stores the body in this window's working namespace. A verified
  readback supplies the immutable version selected by `commit`, carrying a request id
  and cursor through an outbox written before transmission.
* A **status bar** entry with the store, the actor, the cursor, the number of conflicts
  and the number of saves whose outcome is not known. When the store cannot be reached
  at all, the tooltip carries the core's own sentence rather than a question mark: a
  refusal such as `serve-path-occupied (path "...")` names a directory you can remove.
* **Searching**: the magnifying glass in the outline's title bar, or
  `theourgia: Search Blocks`, asks the core's `search` for blocks whose title, keywords
  or text hold every word you type. One hit opens; several are offered in a list, best
  score first, each showing its keywords or a cut of its text.
* **Keywords in the outline**: a block that has any shows them beside its id. They come
  from the block's own record, not from `outline --with-keywords` -- see *What it
  refuses to guess at*.

## What it does not do yet

* No graph view, no link editing, no title editing. Editing the heading line of a block
  is refused with a message rather than half-applied; changing a title is
  `set <id> title`, which is not in this batch.
* **Going to a definition by name** is not here. It wants the core's `whereis`, which
  the core does not have yet. Rather than contribute a command that answers "not
  implemented", this extension asks the core what it can do -- `describe` returns the
  catalogue -- so the entry will appear when the verb does. Nothing needs removing when
  it lands.
* **A store that more than one instance has written to needs a core that names its local
  writer.** A store gets a second log writer when a copy of it is adopted elsewhere and a
  segment published back. A core from F45 on says which writer is local
  (`(local-writer "<id>")` in its `check` answer), and a window writes as that one. With an
  older core, which does not say, a window with no cursor of its own is told so and refuses
  to write rather than guess; one that was already writing keeps its cursor and carries on.
  An answer naming a local writer its own listing does not hold is refused as contradicting
  itself.
* This extension has no direct socket adapter of its own, and does not want one.
  The socket belongs to the core's `(theourgia client)`, and the thin client is
  its adapter; a second implementation of that envelope in TypeScript would be a
  third packer of the same bytes, kept in step by hand. The unimplemented adapter
  that used to sit here has been removed along with its cell -- when the
  `transport` setting stopped offering `socket`, nothing could reach either. The
  setting itself is gone too: its other value ran `cli.ss`, which the core has split
  by role, and the thin client is now the only program this extension runs.
* Working drafts retain their original block hash and causal cut. Commit refuses a
  stale baseline; accepting a new baseline is an explicit reconcile/rebase action.
* No cache. Every view asks the core.

### Recovering another window's unsent work: where it stops

`theourgia: Other Sessions` lists the windows that left something behind and
offers to take it over or to discard it. Four limits are deliberate, and each
of them is a decision rather than an oversight:

* **One store per takeover.** A window keeps a queue per store, because a queue
  carries one cursor and a cursor belongs to one store. A takeover moves the queue
  belonging to the store this window has configured, says how many requests it
  left in the others, and can be run again with a different store configured —
  the claim is re-entered by the window that holds it. What it will not do is
  move another store's requests into this one's queue, which would send them to a
  store the user never named.
* **A queue from before stores had their own directories is left alone.** Nothing
  can establish which store it was for, and guessing would be the cross-store
  write above. It is counted among what was left behind and named in the message.
* **The destination is checked for existence before the claim, and read after
  it.** A queue that turns out to be unreadable therefore takes the token before
  it fails. Repairing it and running the command again works, because the claim is
  re-entrant.
* **Changing `theourgia.store` while the list is open** leaves the entries in the
  store that was configured when the command started.

## Settings

| Setting | What it is |
|---|---|
| `theourgia.corePath` | The directory holding the core. Required. Either form works -- see below. |
| `theourgia.libDirs` | Extra directories for `CHEZSCHEMELIBDIRS`, after `corePath`. The core imports `(igropyr crypto)`, `(igropyr platform)` and `(igropyr sexpr)`, so the directory holding `igropyr/` belongs here or the core exits before reading an argument. |
| `theourgia.store` | The store directory, passed as `--store`. Required. |
| `theourgia.actor` | The name recorded with every write. Defaults to the OS user name. |
| `theourgia.writer` | The draft space this window writes into. Defaults to the actor. See *One agent, one writer id*. |
| `theourgia.scheme` | The Chez Scheme executable. Defaults to `scheme`. |
| `theourgia.timeoutMs` | How long one request may take before the child process is stopped. Defaults to 30000. |

### A checkout or a product directory

`theourgia.corePath` may be either a checkout of the core -- the library sources beside
its programs `theourgia.sc`, `theourgiad.sc` and `core.sc` -- or a directory the core's
`build.ss` produced, holding the compiled libraries beside the same programs. **The
extension reads the directory and decides**: a `client.sc` in it means sources, a
`client.so` means products, and the
extension sets `CHEZSCHEMELIBEXTS` accordingly. A directory with neither is refused by
name rather than left to fail inside Chez with a message about a library.

It is worth pointing it at a product directory. Measured on one machine, one request:
about 460 ms reading the core from source, about 40 to 50 ms from a product directory,
about 30 ms once a daemon is up.

### One agent, one writer id

A writer id is a draft space. Two windows that share one keep overwriting each other's
unsent drafts of the same block, silently -- a later session may bind an id and carry on
with its drafts, which is what makes recovery possible and is also what makes sharing
one dangerous. Give a second window its own `theourgia.writer` when it should keep
separate drafts of the same blocks.

The id is passed to the core through the environment, as `THEOURGIA_WRITER`, together
with `THEOURGIA_ACTOR`. It is deliberately not spliced into the argument vector: the
verbs that do not take a `--writer` option refuse one, and a client that added it
everywhere would turn ordinary requests into usage lines.

## How a request is made

This extension runs the core's **thin client**, `theourgia.sc`, and nothing else. The
thin client finds the daemon for the store, or starts one (`theourgiad.sc`, which it
launches itself), and sends it the request; the extension holds
no socket code and no envelope of its own. Starting, stopping, the exit codes and the
words a failure is reported in all belong to the client, and the extension relays them.

The command line takes its verb from the first argument and only then scans for options,
so the argument vector is

    <scheme> --script <corePath>/theourgia.sc <verb> <args...> --store <store> --actor <actor>

An option placed before the verb is taken *as* the verb, and the core answers
`(error unknown-verb ...)`.

The core prints three kinds of answer and marks none of them: an `(ok (text ...))` answer
is printed as its own bytes, an `(ok (items ...))` answer as one datum per line with no
wrapper, and everything else as one datum. So this extension keeps a table of which verb
answers which way (`src/client.ts`), and that table is the one place where it holds an
opinion the core also holds. Over a socket the whole answer would arrive and the table
would not be needed.

**The exit code is the verdict.** An answer beginning with `ok` that came with a non-zero
exit is not a confirmed write. Exit code 75 is the thin client's own: the request was
refused before the store saw it, and the answer says why.

### One request asks for the machine rendering

A `commit` is sent with `--wire`. The human rendering drops every clause beside `items`,
and one of those clauses is `(behind ((<writer> . <seq>) ...))` -- which log writers have
landed records since this save's draft took its baseline. With `--wire` the answer
arrives wrapped, `(ok (items (ok ...)) (behind ...))`, so the client hands the item on as
the answer exactly as before and puts the form round it beside it.

`behind` names **other instances of the store**, not other people. Every agent writing
into one store on one machine appends through the same log writer, so a colleague's
commit does not appear here; what appears is a copy of the store that was adopted
elsewhere and had a segment of its log published back. The notice says so.

## Saving, and what happens when the answer is lost

A save first becomes a durable W draft. The outbox then records request id, cursor,
block, body and selected working namespace/version **before** commit is sent. Then:

* `ok` naming the record it wrote: the entry is dropped and the cursor moves.
* `ok` naming **no** record: the entry is **kept** and this is reported as a defect. Every
  write the core accepts says which record it appended, and an answer with neither a
  `cursor` nor an `event` leaves the next save with nothing to be composed against.
* `(error unknown <why>)`, a bare `(error unreadable ...)`, a timeout, or a core that
  printed nothing: the entry is **kept**. The store may hold the record; asking again with the same request id is the
  only way to find out. "theourgia: Retry Pending Saves" sends the same bytes again —
  the same id, the same cursor, the same body, not whatever the buffer now holds.
* A refusal only a person can answer -- the store directory is missing, the socket path
  is too long, or `(error refused (instance ...))`, a store created under another
  `THEOURGIA_HOME` or moved since: the entry is **kept and parked**, and later saves of
  the same block wait behind it. Changing a setting sends it again, and so does
  "theourgia: Retry Pending Saves" once the cause is fixed outside the editor; if it is
  not fixed, it is parked again.
* Any other refusal: the entry is dropped and the refusal is shown, with everything the
  core said after its name (and in words, the remedy the core names), because retrying
  cannot change it.

Saves are sent one at a time, and an entry nobody can resolve holds the ones behind it
rather than being stepped over. That serialisation belongs to the queue file rather than to
any one object: the extension builds a fresh saver whenever a setting changes, over the same
outbox, without waiting for the old one to finish.

The queue itself never changes before the file does: every change is written and only then
adopted, because a queue that changed in memory and not on disk is worse than one that
changed in neither — the next call sees the new state, believes it was recorded, and acts
on it. The file is written, flushed, renamed and the directory flushed, so that what
survives a machine losing power is, as far as this client can arrange it, what the user was
told had been recorded. That is an effort, not a guarantee: the directory flush is allowed
to fail silently, because some file systems refuse it and the bytes are already down by
then — so a failure there is indistinguishable from a refusal, and neither stops the save. And only a
file that is *not there* is an empty queue: every other reason a read can fail leaves open
the question of what was recorded, so the outbox refuses to be written to at all rather
than replacing a file it could not read.

An entry is `queued` until the moment it goes out and `sent` from then on, and that is
written down before the request leaves. A queued entry may still have its cursor corrected
to the position the previous answer established; a sent one may not, because the cursor is
part of the request's identity and changing it would turn a retry into a different request
wearing the first one's id. A host killed between the send and the answer leaves a `sent`
entry behind, and that is the state the distinction exists for.

Windows normally have separate session and working paths. Recovery can still reach an
old session's current file, sidecar and queue. A stable kernel lock covers each participating
read-modify-write and final ownership check. Contention reports busy; retry after the
other operation finishes. Locks have no lease timeout and are never unlinked. Process
death releases them. User dialogs and core requests run outside these critical sections.

The managed session layout shares one lock for all its block/queue operations, including
discard. This is local process exclusion, not a distributed filesystem locking protocol.
An editor or external tool that writes these files does not participate in this lock.

The first cursor, before any write has been answered, comes from `check`: a store with one
writer has no ambiguity. `check` does not say which writer is local, so a store with more
than one — one that was adopted or copied — refuses to be written to from here rather than
guessing.

## The file a block is edited in

The current layout is `sessions/<session>/<store>/<block>/current.md` with
`current.md.meta`. Refresh installs one complete temporary by rename; it does not add a
numbered version. Owner records and replacement temporaries live in sibling control
directories. History belongs to the core log and exported Git projections.

**The editor's dirty flag does not answer "is there work here".** The save handler runs on
`onDidSaveTextDocument`, after the bytes have reached disk — so a save the core *refused*
leaves a clean buffer holding text the store has not got. Dirty buffers are never refreshed.
A clean buffer can refresh from its verified W source; a prepared or ambiguous source
cannot be sent. Each projection has an identity independent of its byte hash, so a late
receipt cannot confirm a newer source merely because the bytes match.

Sequential saves advance their baseline only with proof that the displayed W version
was committed. The next draft uses that commit's causal cut, retaining any intervening
external write as a stale-baseline refusal.

`theourgia: Migrate Legacy Block Files` explicitly migrates a selected numbered block.
The command retains an archive of every original file and resumes from its journal after
interruption. Pending sends, live owners, dirty buffers, changed inputs, and drafts whose
bytes cannot be verified against committed/W data stop migration. Multiple unprotected
old drafts remain intact for manual handling; the command never overwrites them into one
working slot. Re-run on the original selected path to resume an interrupted archive step.

## Native lock build

The current native lock build supports macOS and Linux. `npm run compile` requires a C
compiler and Node N-API headers; set `THEOURGIA_NODE_HEADERS` to the directory containing
`node_api.h` if automatic discovery fails. The build does not download headers. N-API v3
allows the same platform/architecture binary to load in the tested Node and VS Code hosts.
Windows support has not been implemented or tested. Directory-fsync limitations described
above still apply; process locking does not strengthen power-loss durability.

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

**Two cases the outline's text cannot express**, both reproduced on a real store: a title
ending in `  conflict`, and a title whose second line looks like a row
(`second line\n- fake.1  invented`). Neither reaches the tree any more — the outline is read
only for ids and depth, the title comes from `read <id>` and the mark from `conflicts`, and
every top-level id is confirmed against the store, so a forged row is a refusal naming the id
and its line. The root cause is still that the outline is a rendering with no escaping, and
the fix belongs in the core; until then this client does not read anything from it that a
title could forge.

**Keywords are read the same way.** `outline --with-keywords` prints them, and this
extension does not read them from there: a title holding two spaces and a bracket can
forge that field as easily as it can forge a mark. The block is already being read for
its title, and its record carries its keywords, so they arrive by the channel a title
cannot reach.

## The s-expression reader

`src/vendor/goeteia/sexpr.mjs` is a verbatim copy of goeteia's `rt/sexpr.mjs`, held to
`sexpr-vectors.json`, the golden fixture generated from `(igropyr sexpr)` — the authority
for this wire format. The copy exists only because goeteia's package does not export the
deep path in a published release yet. Do not edit it: `test/unit/vendor-sexpr.test.ts`
sweeps both fixtures and checks the file's own bytes against the digest its provenance note
claims, so an edited copy that kept its note fails.

It is held to two fixtures, both copied beside it: `sexpr-vectors.json`, generated from
`(igropyr sexpr)`, and `sexpr-escape-vectors.json`, which covers the escapes a conforming
R6RS writer emits — `\a \b \f \v` and `\xHH;`, in strings and in symbols. That second
table exists because of a gap this extension hit: the reader used to accept only
`\n \t \r \" \\`, so a block whose body contained a form feed was stored happily by the
core and could never be read back. `S14` in `test/unit/real-core.test.ts` was red for as
long as that was true and is now the guard against it reopening.

**Why it is still a copy.** goeteia's package exports `./sexpr` now, but the published
1.7.1 predates that export, so depending on it would not resolve. When a release carries
the export, this copy goes and a dependency takes its place.

## What the cells do not cover

Recorded here rather than left to be rediscovered. Each of these is a place where a cell
exists and proves less than its name suggests, or where no cell exists at all.

* **The sentence a successful save shows.** `behind` is measured end to end -- a store,
  a copy of it adopted elsewhere, a segment published back, and the next commit through
  this extension's own save path carrying the notice -- but that is a cell against the
  core, not a cell inside an editor. The notice reaches a user through
  `showInformationMessage`, and nothing in an editor-hosted run can read one. What is
  covered is that the clause arrives, is read, drops the writer the commit itself
  advanced, and becomes the sentence; what is not covered is that the sentence is put on
  the screen.

* **A daemon that cannot be started, during a save.** The editor-hosted cell for this
  obstructs the socket path and reads the words back out of the status this extension
  hands to a caller. It gets there through the conflict count's own asking, because a
  save that meets the obstruction never reaches the queue -- the request that fails is
  the one that asks where the cursor is, before anything is written down. That is the
  right behaviour and it means the save path's own report of the failure, which goes to
  a message box, is not what the cell reads.

* **The test host's own storage.** The editor-hosted runner empties the extension's
  `globalStorage` under its profile before every start, and refuses to start if it
  could not. That guard follows symbolic links in the profile path and treats a
  directory it cannot read as absent. It is a harness pointed at a directory it
  created itself; it is not hardened against a profile somebody else prepared.

* **A save the *core* refused.** The editor-hosted cell named for a refused save exercises
  a refusal this client makes: a changed heading is turned away by `splitDocument` before
  the saver is reached. A save the store itself rejects travels a different path, and
  nothing here walks it — an implementation that preserved locally refused edits while
  overwriting ones the store turned down would pass every cell in this tree.
* **Atomic replacement and flushing.** The disk-failure cells make the queue's parent
  directory unusable, so they fail at the directory check that precedes the temporary
  file. They establish that a failed write does not damage the queue; they do *not*
  establish that the write is a temporary file, a flush and a rename, which is what the
  code does. A writer that truncated the real file in place, or omitted `fsync`, would
  pass them.
* **Showing a buffer from a store the user has just left.** `openBlock` used to check the
  settings generation after each of its editor waits as well as after the core read. Those
  five checks could not be guarded: the waits are VS Code calls, not core requests, so no
  stand-in core widens them, and a configuration change begun from inside
  `onDidOpenTextDocument` — which does fire during the first of them — has not reached the
  extension by the time the wait resolves. That was measured. Rather than leave five
  guards nothing could make fail, they were removed; what remains is that the buffer
  carries the store it was read from, so a save into a differently configured store is
  refused by name. The residue is cosmetic: a buffer from the old store can still appear
  after the settings change.

  The three generation checks that remain each have a cell that fails when the check is
  removed — `refreshConflicts` (C6), `openBlock` (C1), the retry report — verified one at
  a time by replacing each with a condition that is always false. `OutlineProvider` keeps
  two more comparisons of its own, which those three cells do not speak for.

  **What replaced them is not another check.** Removing the five exposed an older hazard
  they had been hiding: two opens of one block can overlap, and whichever finished last
  became the baseline a save is measured against — which has nothing to do with which
  reading the store answered most recently. A baseline older than the buffer has a prefix
  the buffer no longer starts with, and a prefix that fails to match is *not* refused: the
  heading is taken for body and written into the block. Each open now takes a ticket
  before it reads and cannot register over a newer one (`src/open.ts`). That rule lives
  outside `activate` precisely so a cell can drive the interleaving the editor cannot be
  made to produce.

* **The three lines that hand VS Code to the placement step.** `src/open.ts` decides which
  reading of a block a save is measured against and `src/placing.ts` acts on that decision;
  both are driven directly by cells, because the interleavings they exist for cannot be
  produced through the editor's API. What is left uncovered is the adapter: the three
  closures in `openBlock` that forward `openTextDocument`, `setTextDocumentLanguage` and
  `showTextDocument`. A cell hands `placeReading` three functions of its own, so nothing
  checks that the real ones are wired to the right VS Code calls.

  This shape is why that separation exists. For one round the decision was acted on inside
  `activate`, and when `register` stopped returning a boolean the call site's
  `if (!admission)` kept compiling and stopped firing — every answer was a non-empty string,
  so a losing open would have gone on writing the file, with all 17 editor-hosted cells
  green. It was caught by reading. The answer is now an object whose falsy reading is a
  field, so the compiler finds that mistake, and the step it guards has its own cells.

* **What a retry actually displayed.** The retry cells read the notice the command decided
  on and returned. Whether it reached the screen, and whether the status bar was repainted,
  is not observed: deleting the call that shows it would leave them passing.
* **Nested-document visibility.** The tree does not mark a nested document, because the
  core's own handling of the shape is still being decided. The mark is read and carried;
  what the tree should draw for it is not settled.

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

**The cells start daemons, and they are accounted for.** Every fixture that reaches a
real core gives the core a run root and a home of its own under a temporary directory,
and stops by pid the daemons that directory gave rise to. Two gates stand behind that
rather than anybody's memory: one cell asserts that a fixture's daemon is visible before
it is stopped and gone afterwards, and a hook over the whole suite asserts that no
process of this run is left and that your own `~/.theourgia/run` did not **grow**. Growth,
not total: whatever is in that directory when a run starts belongs to whoever put it
there.

It is growth and pids because it has happened. When these cells were first turned onto
the shipping transport the fixtures set no run root, and one run left thirteen daemons
running and thirteen directories in the user's own. Nothing went red. It was cleaned up
by hand.

**This extension needs a core that has request tracking.** Every save carries `--req <id>
--cursor <writer>:<seq>`, and a core without those options answers `(usage (set <id> <field>
<value>))` — measured against theourgia at 842cf46, where the tokens `--req`, `--cursor`,
`tracked-request` and `req-not-tracked` do not appear in `cli.ss` or `rpc.ss` at all. The
outbox is built on that feature, so against such a core every save is refused. A usage line
can also mean an argument the core did not expect, so the message names both.

**A nested document is reported but not shown.** The core lists one under `conflicts`, and
its recursive walk stops at a doc-kind child — `project.sc` says "a walk stops at one",
because a nested document has its own file and descending would write its sections twice. It
is therefore absent from its parent's expansion, and `nested-document` is not a mark that
puts a block in the root listing either — so while its parent is alive, the only sign of one
is the conflict count. Delete that parent and it becomes an orphan as well, and *that* mark
does put it in the root listing, where it shows with both marks. What a nested document
*means* is still open in the core's own design; this is pinned as current behaviour, not
endorsed.

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
