# theourgia for VS Code

Browse a [theourgia](https://github.com/guenchi/theourgia) block store in the side bar,
open a block in an editor, and save through a durable working draft and selected `commit`.

Licensed under the Apache License, Version 2.0. See LICENSE.

## Before you start

The extension drives the theourgia core installed on this machine: it needs
Chez Scheme and the theourgia programs, and asks the core through its own
command line. Install the core first (from source today, `build.ss` into a
directory of objects; a Homebrew tap arrives with the core's 1.0 release),
then point the settings `theourgia.corePath` (the core's own `theourgia/`
directory, objects or sources, the one holding `theourgia.sc`) and `theourgia.scheme` (the
Chez executable: `scheme` by default, or `chez` on a Homebrew machine, whose
formula installs it under that name) at it, and `theourgia.store` at a
store made with `theourgia init`. One store is one machine's: the core
serves it through one daemon, and two machines writing one store is not
this design.

**Versions.** The core must be at least commit 9f806bb of the theourgia
repository (the `read --wire` version clause that move and rename check);
on an older core those two commands are refused by name. The three supply
commands need commit 5230bb6 or later, which has `supply`; an older core
answers it as an unknown verb, and the command shows that answer. The core
runs on Chez Scheme 10.1.0, 10.3.0 or 10.4.1 from commit 8818b37 (10.1.0
only before it), and on 10.4.0 as well from b742ac7; with another Chez its datum export, `def` and `eval`
refuse (`unsupported-printer-version`). VS Code 1.138 is
what the suites run on, and the extension claims nothing older.

**Platforms.** 1.0.0 is packaged for macOS on Apple Silicon only: the
extension's save queue takes a file lock through a small native module,
built for the machine that packages the extension, and this first package
was built here. Packages for Intel macOS and for Linux (x86-64 and arm64)
are built by the repository's workflow, `.github/workflows/package.yml`,
each on its own platform, and one is published only when the unit suite has
passed on every target, against a real core. Today it cannot pass on Intel
macOS: the core has no measured platform numbers for that machine and refuses
to start there, so the workflow publishes nothing until the core measures it. There is no Windows package:
the lock uses `flock`, and a Windows lock is a piece of work of its own.

The extension's source is the `vscode` branch of https://github.com/guenchi/Theourgia.

## What this batch does

* An **outline tree**: the top level comes from `outline --depth 1`, and opening a node
  asks `read <id> --recursive` and shows the blocks directly under it. A block in a
  structural conflict is marked, and so is one whose parent is gone.
* **Opening a block**: the block's text is read from this window's working view
  (`read <id> --working-info --writer window-<session>`) -- this window's draft of it if
  there is one, the committed block otherwise -- and its heading line and body are put in
  the block's file, in that order, shown as markdown, or in the editor mode a code block's
  `lang` names (see *Code blocks*). A datum block opens read-only instead. One file per
  (store, block), so opening a block twice reaches the same document.
* **Opening a subtree as one document**: right-click a node in the outline and choose
  `Theourgia: Open as Document`. It asks `read <id> --recursive --wire` and composes
  the block and everything under it into one markdown document. **It is a read-only,
  composed view** under its own scheme, `theourgia-document` -- not a projection file,
  not on disk, and never written back: it cannot be edited or saved in place. (VS Code's
  Save As still writes a copy to a file you name; a copy saved over a block's projection
  file changes that file like any other edit would.) To change a block, open that block
  from the outline and edit its projection. Each block's heading
  level is its depth under the opened block (`#` for the block itself), the headings in
  a block's own body move down by the same depth (not inside fenced code), and anything
  past `######` stays at `######`. A line
  `<!-- theourgia block <id> depth <n> -->` in front of each block marks where it starts:
  a rendered preview hides it, the editor shows it, and it carries the depth that the
  heading cannot past six levels. Headings written with an underline (`===`, `---`) are
  not moved. If any block in the subtree cannot be placed -- deleted, an unsettled
  position, a parent missing from the answer, a title or body that is not text -- the
  document is not opened, and the message names those blocks. Opening it again reads the
  store again.
* **Saving**: `write` stores the body in this window's working namespace. A verified
  readback supplies the immutable version selected by `commit`, carrying a request id
  and cursor through an outbox written before transmission.
* A **status bar** entry with the store, the actor, the cursor, the number of conflicts
  and the number of saves whose outcome is not known. When the store cannot be reached
  at all, the tooltip carries the core's own sentence rather than a question mark: a
  refusal such as `serve-path-occupied (path "...")` names a directory you can remove.
* **Searching**: the magnifying glass in the outline's title bar, or
  `Theourgia: Search Blocks`, asks the core's `search` for blocks whose title, keywords
  or text hold every word you type. One hit opens; several are offered in a list, best
  score first, each showing its keywords or a cut of its text.
* **Keywords in the outline**: a block that has any shows them beside its id. They come
  from the block's own record, not from `outline --with-keywords` -- see *What it
  refuses to guess at*.

## What it does not do yet

* No graph view, no link editing, no title editing. Editing the heading line of a block
  is refused with a message rather than half-applied; changing a title is
  `set <id> title`, which is not in this batch.
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
* No cache, but one: every view asks the core, except that the hover keeps its answers for
  fifteen seconds (see *What the store holds about a name*).

### Recovering another window's unsent work: where it stops

`Theourgia: Other Sessions` lists the windows that left something behind and
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
| `theourgia.corePath` | The core's own `theourgia/` directory, the one holding `theourgia.sc`. Required. Either form works -- see below. |
| `theourgia.libDirs` | Extra directories for `CHEZSCHEMELIBDIRS`, after `corePath`. The directory holding `corePath` is searched after them (the extension adds it itself), so a copy named here comes first. The core imports `(igropyr crypto)`, `(igropyr platform)` and `(igropyr sexpr)`, so the directory holding `igropyr/` belongs here, unless it is the one holding `corePath`, or the core exits before reading an argument. |
| `theourgia.store` | The store directory, passed as `--store`. Required. |
| `theourgia.actor` | The name recorded with every write. Defaults to the OS user name. |
| `theourgia.writer` | Passed to the core as `THEOURGIA_WRITER`, the draft space a verb sent without `--writer` uses. Defaults to the actor. Nothing this extension sends uses it: every request that takes a writer names one -- this window's own, or, when recovering or migrating another session's work, that session's (a migration also reads the committed text under a fresh `migration-<uuid>` name). See *Each window, its own draft space*. |
| `theourgia.scheme` | The Chez Scheme executable. Defaults to `scheme`; Homebrew installs it as `chez`. |
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

### Each window, its own draft space

A writer id is a draft space. Each window writes its drafts under one of its own,
`window-<session>`, named after the window's session, and passes it as `--writer` on
every working read and write it sends. So two windows editing the same block keep
separate drafts, and a window's unsent work is reached from another only through
`Theourgia: Other Sessions`, which takes it over by that name.

`theourgia.writer` is passed to the core through the environment, as `THEOURGIA_WRITER`,
together with `THEOURGIA_ACTOR`: the draft space a verb sent without `--writer` uses. It
is deliberately not spliced into the argument vector: the verbs that do not take a
`--writer` option refuse one, and a client that added it everywhere would turn ordinary
requests into usage lines. Nothing this extension sends uses it: the working reads and
writes, `commit` and the diagnostics' export carry `--writer`, and `supply` its `--for`,
always with a name -- this window's own, or, when it recovers or migrates another
session's work, that session's (a take-over commits under the name the other window
recorded, and a migration also reads the committed text under a fresh
`migration-<uuid>` name).

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

* The queue file written but its directory not flushed: the save stands -- the entry is
  there and is sent -- and a warning follows the save's notice, saying the entry may not
  survive the machine losing power.
* `ok` naming the record it wrote: the entry is dropped and the cursor moves.
* `ok` naming **no** record: the entry is **kept** and this is reported as a defect. Every
  write the core accepts says which record it appended, and an answer with neither a
  `cursor` nor an `event` leaves the next save with nothing to be composed against.
* `(error unknown <why>)`, a bare `(error unreadable ...)`, a timeout, or a core that
  printed nothing: the entry is **kept**. The store may hold the record; asking again with the same request id is the
  only way to find out. "Theourgia: Retry Pending Saves" sends the same bytes again —
  the same id, the same cursor, the same body, not whatever the buffer now holds.
* A refusal only a person can answer -- the store directory is missing, the socket path
  is too long, or `(error refused (instance ...))`, a store created under another
  `THEOURGIA_HOME` or moved since: the entry is **kept and parked**, and later saves of
  the same block wait behind it. Changing a setting sends it again, and so does
  "Theourgia: Retry Pending Saves" once the cause is fixed outside the editor; if it is
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
told had been recorded. That is an effort, not a guarantee. A directory that will not open for
flushing is passed over silently, because some file systems refuse it and the bytes are
already down by then. A flush that is attempted and fails does not stop the save either, but
it is no longer silent when a save is queued: the save stands and a warning follows it. And only a
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
writer has no ambiguity, and in a store with more than one -- one that was adopted or
copied -- `check` names the local writer (`(local-writer "<id>")`) and the cursor is that
writer's. A core too old to name it is refused rather than guessed at; see *What it does
not do yet*.

## The file a block is edited in

The current layout is `sessions/<session>/<store>/<block>/<slug>-<block>.md` with its
sidecar `<slug>-<block>.md.meta`, where the slug comes from the block's title when it is
first published (`<block>.md` when the title leaves none) and is never changed after. A
directory is read by its sidecar, not by its name, so one an older build left as
`current.md` is still read. Refresh installs one complete temporary by rename; it does not add a
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

`Theourgia: Migrate Legacy Block Files` explicitly migrates a selected numbered block.
The command retains an archive of every original file and resumes from its journal after
interruption. Pending sends, live owners, dirty buffers, changed inputs, and drafts whose
bytes cannot be verified against committed/W data stop migration. Multiple unprotected
old drafts remain intact for manual handling; the command never overwrites them into one
working slot. Re-run on the original selected path to resume an interrupted archive step.

## Packaging, and the gate that checks a package

Package with dependency detection on:

```
npx vsce package --allow-missing-repository
```

**Not with `--no-dependencies`.** That flag leaves out every file under `node_modules`,
the s-expression reader included, whatever `.vscodeignore` re-includes -- and
`activate()` loads the reader before anything else, so a package built that way installs
and never starts. (A package built on 2026-09-19 did exactly that.)

`node scripts/package-gate.js` (with `THEOURGIA_CORE` and `THEOURGIA_LIBDIRS` set, as for
the real-core cells) builds a package the way above and reports, without judging:
whether the files the extension needs to start are in it, and whether they would be with
`--no-dependencies`; the install into an editor that is not yours (the one
`@vscode/test-electron` keeps in `.vscode-test/`, with a profile and an extensions
directory of its own, or the editor `THEOURGIA_TEST_CODE` names); the activation line in that editor's `exthost.log`; and one save
made through the installed extension to a temporary store -- the store's log for the
block before and after, and whether the saved line is in the store. What it makes for the
run -- the package, the editor's profile and extensions, the store, the core's run and
home directories -- is under one temporary directory, removed at the end with the count
said. Two things it leaves, as any build does: the repository's `out/`, which it compiles,
and the editor `@vscode/test-electron` downloads into `.vscode-test/` if it is not there
yet. It removes every `VSCODE_*` and `ELECTRON_*` variable from its environment first --
the editor would otherwise take its profile from `VSCODE_APPDATA` or `VSCODE_PORTABLE`
before `--user-data-dir` -- and says which it removed. Lines it
prints start with `[gate]`; the rest is the editor's own output. It exits non-zero only
when a reading could not be taken.

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

The reader is goeteia's `sexpr` module, from the `goeteia` package (pinned at 1.7.2 in
`package.json`), loaded by the one name `src/wire.ts` gives it (`READER`).
`test/unit/dependency-sexpr.test.ts` sweeps the module that name loads against two fixtures
in `test/fixtures/goeteia`: `sexpr-vectors.json`, the golden fixture generated from
`(igropyr sexpr)` -- the authority for this wire format -- and
`sexpr-escape-vectors.json`, which covers the escapes a conforming
R6RS writer emits — `\a \b \f \v` and `\xHH;`, in strings and in symbols. That second
table exists because of a gap this extension hit: the reader used to accept only
`\n \t \r \" \\`, so a block whose body contained a form feed was stored happily by the
core and could never be read back. `S14` in `test/unit/real-core.test.ts` was red for as
long as that was true and is now the guard against it reopening.


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
  reading the store answered most recently. Now an open's working read and its publication of
  the block's file run on the save chain keyed by the block's directory, so those steps of two
  opens of one block, and a save of it, run one after the other (`PathChain`; the cell "does
  not let two publications of one block interleave"); the block's record is read before that
  step and the editor calls come after it. The publication's record beside the file holds the
  prefix the text was read with and the digest of the text written, and the publication names
  the record it found before reading; a record that moved meanwhile refuses the publication as
  `record-moved` (cells in `test/unit/publication.test.ts`).

* **The editor calls after a publication.** `openBlock` opens the published file, sets its
  language and shows it with VS Code's own calls, directly. The editor-hosted cells that
  open a block read the editor's language and text back, for a markdown block; for a code
  block, the mode its `lang` chooses is read back only by a cell with a stand-in editor
  (`test/support/extension-schedules.js`), not by a real one.

* **What a retry actually displayed.** The retry cells read the notice the command decided
  on and returned. Whether it reached the screen, and whether the status bar was repainted,
  is not observed: deleting the call that shows it would leave them passing.

## Running the cells

```sh
npm install

# the parser, transport, outline, block, cursor and save cells
npm run test:unit

# the same, plus the ones that need the core itself (every cell that names a real core)
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

**A nested document is shown under its parent, with its mark.** The write path refuses a
document anywhere but the top level, so a nested one exists only in history made before that
rule or elsewhere. The pinned core (theourgia 59e69f3, as 5230bb6, 9f806bb, 659fea2 and cba98ae before it) treats it as a block like any
other: it is in its parent's recursive read, and it is reported once under `conflicts` as
`nested-document`. So it appears in the outline where it is, as a child carrying that mark,
never hidden. `nested-document` is not a mark that puts a block in the root listing; delete
the parent and the block becomes an orphan as well, and *that* mark does put it there, where
it shows with both marks. (An older core's walk stopped at such a child and the outline could
not show it; queue item 48 moved these cells to the current behaviour.)

**When a writer cannot be read.** A store whose log for one writer cannot be read -- a
directory without permission, a disk that answers with an error -- still answers every read
with everything the other writers wrote, and says which writer it could not see. The
extension shows those rows as usual and says so beside them, as information and never as an
error: the outline's first row, above the blocks; the search's message, or the picker's title
when there are several hits; a line before the text of a document or a block opened from it;
a message after a save the store took; and `incomplete` in the status bar until a reading
sees every writer again. Each names the writer, the path and the reason. While a writer is
missing, the marks on blocks are shown as not known, and the conflict count is the other
writers' count, since the missing writer's conflicts are not in it. Saving goes on as usual.
A writer whose log is damaged partway is read up to the damage: its records up to the last good
one are in the reading, and the note says so -- the record it was read up to, the log's own name
for the damage, the path and the reason. A note of any other kind is not one this extension knows
how to say, and the reading is refused by name rather than shown as complete.

**Code blocks.** A text-mode code block -- one `import-code` made from a source file -- keeps its
source as bytes, and it opens as that source, decoded as UTF-8, in the editor mode its `lang` names
(plain text when the editor knows no such mode). Line ends are kept as they are. A block whose bytes
are not UTF-8 does not open: the store says so by name (`non-text-projection`), and a view that reads
such a field refuses with `text-not-utf8`, the field, the block and the offset of the first byte that
does not decode. A file that begins with a byte-order mark is not saved.

A datum-mode block -- a library `import-code --datum` made, or one definition in it -- keeps its
code as a datum, not as text, and this extension cannot write one yet. It opens read-only: the
library's file as `export-code --datum` writes it, at the block's own place, with a note saying why.
Nothing is written or committed for it. Go to definition on a datum definition opens the same view at
its `(define ...)`. A block's file records the block's mode when it is opened; a file opened before the
mode was recorded is checked once at its first save, and a datum block's file is refused there, with
nothing sent, rather than saved where the store would not run it.

**Go to definition.** On a name in a block, "Theourgia: Go to Definition" -- or the editor's own Go to
Definition -- asks the store's `whereis` which block defines it. One answer opens that block at the
line of its `(define ...)`, found in the text as it is shown, a draft included; several are listed with
their library and kind, or `export` for a library that exports the name, which opens at the library's first
line. A name nobody defines is said, with the nearest names the store knows. The line search reads `;` and
`#| |#` comments as comments; a `#;` datum comment is not recognised.

**What the store holds about a name.** Resting the pointer on a name in a block's file, a composed
document or a datum view adds, beside the language's own hover, what the store holds about it: blocks
that link to the block under the pointer or to the block that defines the name (with the relation),
blocks that refer to them in their text, and prose blocks (documents, sections, decisions, tasks) that
mention the name as a whole word. Each line gives the title, the kind and the first sentence, and opens
the block; past five lines, the last one lists them all. The hover only reads, and only from the store
the document came from; in Scheme the name is read as a Scheme identifier, elsewhere as the editor's
word. Answers are kept for fifteen seconds, or until this window saves, changes the tree or its
settings.

**Suggest a split of a source file.** On a source file on disk (not a block's own file), "Theourgia:
Suggest a Split of This File" asks the core's `split-suggest` where the file could be divided into
blocks, and opens the review file the core writes; nothing is recorded. The cuts come from the
editor's own symbols for the file -- the top-level ones the language's symbol provider gives, sent as
`--symbols` -- and the message beside the review says so (`cuts from (vscode "<version>"
"<languageId>")`), with the core's warnings; with no symbols (no provider, or none yet) the core's own
definition patterns choose, and the message says `regex`. A dirty file is saved first, and nothing is
sent if the save fails. The positions are counted in the file as it is on disk, a byte-order mark and
carriage returns included, and the file's digest goes with them; if the file changes while its symbols
are being collected, or before the core reads it, nothing is cut (`symbols-stale`, shown as the core
says it). Each symbol is sent at the start of its line, two symbols on one line as one, and a comment
above a definition goes with it, as without symbols. A file whose lines end in CR alone is not cut by
symbols: the core takes a line start to follow a line feed, and refuses (`symbols-not-a-line-start`). Which provider named a symbol cannot be said: the
editor merges them.

**Supplying what the editor knows.** Three commands hand the store facts the editor's language
support computes, which the core keeps beside the store and never in its log (the core's README,
"Derived data from an editor"): "Theourgia: Supply Signatures and Keywords" and "Theourgia: Supply
Calls" for the committed store, and "Theourgia: Supply Diagnostics" for this window, from its own draft
space's working view, so the diagnostics are of what the window is editing. Each projects the store with `export-code` into a directory in this extension's own
storage, emptied first, opens the projected files without showing them, asks the editor's providers,
and sends one `supply` per language with every projected file listed by its digest and every file of
that language named as replaced, a file with no fact included (so its old facts clear).
- A block's signature is the detail of its most representative top-level symbol -- a function, method
  or constructor, then a type, then anything else, the first by position among equals -- else from
  that symbol's hover: the editor's hovers are read one at a time and the first that gives a line
  wins; a hover gives its first line of code (part by part, the first non-blank line inside the part's
  first fence, or of a part the server marks with its language), else its first line that is not
  blank, a fence or a rule, with heading marks and backticks taken off. Else none. A server that
  writes its signature as plain text above a fenced example in the same hover gives the example. Its
  keywords are the words of the names it declares (itself and its direct children), split at case
  changes, underscores and digits. A call is an edge from the call hierarchy to another block of the
  same projection; a call into the same block or into anything else gives none. A diagnostic keeps its
  severity and its byte range in the projected file.
- Which block a position is in is read off the projection's own marker lines. A fact depends on its
  own block and every other block of its file (a call on the target's file too), so the store drops it
  when any of them changes; the facts are as fresh as the last supply, and nothing supplies them on its
  own.
- If a projected document changes while the facts are collected, nothing is sent and the command says
  which file. Diagnostics are taken once they have not changed for 1 s, and at most after 10 s, when
  the message says the analysis may be incomplete. A refusal is shown by name: `supply-stale` asks for
  the command again; `supply-malformed` is this extension's defect, and its supply file is kept.
- The projection is outside the workspace, so a language server may see it with less context than a
  workspace folder (no project configuration); a folder of the person's choosing is a later option.

**Browsing by file.** The tree has two modes, switched from its title bar: **Files**, the directory
tree export would write, and **Outline**, the store's own parents and order. The directories are not
kept anywhere; they are read from the `path` field of the blocks export writes as files -- a document
at the top level, a text-mode file block wherever it sits, and a datum-mode library -- so a directory
appears with its first file and goes with its last. Directories come before files, each sorted by name;
a file is labelled with the last part of its path, and its title is in the tooltip. A path is taken as
export takes it, never tidied: one export refuses (`../a.md`, `docs//e.md/`) puts its block under
**not in any file** with the path in the tooltip, beside the top-level blocks with no path, those with a
path that are not a file, and the orphans. Two documents with one path are both listed and the second by
id says `not exported: path taken`; two text files or two libraries with one path both say
`export refused: duplicate path`, as export refuses the whole export. A document below the top level
is never a file, whatever its path: export writes its content into the enclosing document. A store
opens in Files when any of its top-level blocks carries a path and in Outline otherwise, decided the
first time it is shown and kept for that store; it does not change by itself when a path appears
later. On a directory, **New File Here** makes a markdown document at that path (one `batch` through the
save queue, so an answer that is lost is kept and sent again); on a file, **Move to Directory** and
**Rename File** write its `path`. A directory has nothing to open, and there is no new or deleted
directory: a directory is only ever a prefix. The Files listing reads each top-level block with its
whole subtree, where the Outline listing reads each alone. A path is a path only when it is stored as a
string, as the exporters take it, and a document is a file only when export takes it as a root: an
orphan document with a path is listed in no file. Text files are written as a family, as export-code
writes them: one text file with no safe path, one path two text files share, or one block under a text
file that is not a text-mode code block, and every text file says `export refused` with the reason;
datum libraries likewise. Two of the datum exporter's refusals are not predicted here and are answered only
when exporting: a library whose child's doc holds a projection marker (`marker-in-doc`) or is not whole-line
comments (`invalid-doc`); such a library is shown as written. A row names a block and nothing more: Move
and Rename read the block afresh (`read <id> --wire`, whose answer carries the block's version), offer its
current path, and write the new one with `--if-unchanged <version>`, so a block changed after that read --
by another window, or while the prompt was open -- is refused by the store as `changed`; the window says
"this item changed since it was listed; the view is refreshed" and lists it again. A row, a node or an
open kept from a listing of another store is refused before anything is read, and an action whose store
was switched while its read or its prompt waited sends nothing; once queued, a write belongs to the store
it was read from. A settings change that keeps the store refuses nothing. A write of the path is never
queued without the version it was read at. A
queue holding a new document not yet settled, or a write kept with the version it was read at, is written
as version 2, which an earlier build of this extension refuses by name rather than misreading or sending
without its condition; every other queue is written as version 1, as before. A kept write is sent with
the same version however late it goes, so it is refused if the block has changed meanwhile; whichever
send drains it -- the retry command, the drain at startup, or the drain in front of another save -- the
window says so, and lists the view again when that store is still the one shown.

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
