# Changelog

## VS Code extension 1.1.1 — 2026-10-09

*The extension's own version, `theourgia.theourgos`, published on the Marketplace for macOS on arm64 and x86_64 and Linux on x86_64 and arm64.* Import with Symbols, on a core pinned at theourgia be42914.

### Added

- Import with Symbols: a folder or a source file is imported into the store with the core's `import-code --symbols`, and the first import of each file is split at the editor's top-level symbols into a file block and its code blocks, with no review copy first. The report in the output channel places each new block at its lines, and gives a file the core refused to split with the line and column of the byte it names. A file with unsaved edits stops the command before anything is sent.

### The core it runs

- The core is pinned at theourgia be42914, ten commits after 1.1.0, which adds `import-code --symbols`.
- The core checks every cut of a first import split at symbols: a cut must fall at the top level of the file, outside its protected prefix (a shebang, a coding line, a byte-order mark), so a symbol that starts inside a body, a string, a block comment or an open bracket refuses that file instead of cutting it. The report names the refusal: `symbols-not-top-level`, `symbols-unscanned`, `symbols-in-prefix`, `symbols-unchecked` or `symbols-no-file`.
- The extension's unit suite passed against that core on darwin-arm64, darwin-x64, linux-x64 and linux-arm64 (CI run 37816045917).

## VS Code extension 1.1.0 — 2026-10-06

*The extension's own version, `theourgia.theourgos`, on the Marketplace for macOS on arm64 and x86_64 and Linux on x86_64 and arm64.* The core pinned at theourgia 1.1.0.

### Changed

- The extension is Theourgos: package `theourgos` under the publisher `theourgia`, id `theourgia.theourgos`, packages named `theourgos-<target>.vsix`. Its commands and settings keep their `theourgia.` names. A 1.0.0 installed by hand from a `.vsix` built before this change has the id `theourgia.theourgia`, which VS Code treats as another extension: let its pending saves go through, or discard them, then uninstall it before installing 1.1.0, since both register the same commands.
- The core is pinned at theourgia 1.1.0 (3a4ac93).
- The setting `theourgia.writer` is removed: each window has written its drafts under a name of its own since before 1.0.0, so it decided nothing. A `settings.json` that still sets it shows an unknown setting, and nothing changes.

### Fixed

- The search view answered "the store did not answer the search" for every query that found anything, since 1.0.0. A search now lists its hits.
- A datum-mode block opened as an empty buffer, and an edit saved through it was answered as saved and never reached the code. It now opens read-only, as the datum export writes its library's file, and a file opened under 1.0.0 on a datum block is refused on save instead of appearing to save.
- A save the core refuses because its block is a datum block, or because a log segment was replaced under the store's daemon, now says which, and for the second says to restart the store's daemon and save again.
- Search and the hover's mentions ask the core with `--wire`. When every hit is in a superseded or refuted block, the search view says the words are found only in blocks that are not in force, rather than that nothing matches.
- The package for macOS on x86_64 passes its unit suite against the core it is pinned to, and 1.1.0 is on the Marketplace for all four targets; 1.0.0 was packaged for Apple Silicon only.

## 1.1.0 — 2026-10-05

*26 commits.* Queries and context for agents, read receipts and write premises, class and validity, a change stream, runners for five more languages, and a measured row for macOS on x86_64.

### New verbs

- `query <goal>` answers every binding of a goal's variables over the committed state at one cut. Its facts are twenty relations the store already answers, combined by nineteen rules kept as data; `query --relations` lists both. A query reads no log, runs no code and writes nothing; one whose new work passes a million tuples is refused, never answered in part.
- `context --for <id> --budget <tokens>` answers what to read before working on a block, within a budget counted as four bytes a token of the answer as the wire prints it, its cut left out, with a receipt that a commit can be given back as its premises.
- `subscribe changes <rev> [<token>]` keeps a connection to the daemon and prints one line for each publication: blocks added and removed, fields, positions and parents changed, conflicts entered and resolved, edges added and removed. A commit made outside the daemon reaches subscribers within about a second. The MCP shell does not offer it, and without a daemon it answers `needs-daemon`.
- `commitments` lists the decisions still owed: a decision is open until a live block implements it (a task only once it is done) or its status is done or dropped. `--drifted` names implementations whose content has changed since the link, unless a later link or edit of the decision has seen the change; status, batch, keywords, class and slug do not count.
- `init --template` or `--template-file`, `template apply` and `template export` make a store from a template of document roots and relations, or give one to a store; the built-in templates are `project` and `memory`. `tasks` lists blocks of kind task.
- `names <id>` lists the names a code block uses and the libraries a library block imports; `uses <name>` lists the live code blocks that use a name.

### New options and clauses

- `read <id> --cut <cut>` reads a block at a causal cut, written out or as a tag; every `log` entry gives the event's `cut` and `past`.
- `read --rev` adds the daemon publication the answer was read from, `(rev <n> (daemon "<token>"))`, so a subscriber can apply frames by revision.
- Read receipts: `(cut ...)`, the state a read was answered from, and `(versions ...)`, the version of each block it shows or lists, at the end of an answer. `read`, `refs`, `commitments`, `tasks`, `names` and `uses` carry both; `search`, `grep` and `whereis` add the versions; `outline` and `reach` carry the cut.
- `--premises <datum>` on every committed write -- insert, set, move, del, link, unlink, tag, batch, commit, import-code, import-md, def and template apply -- with a list of block versions and query digests, or a `receipt` as `context` prints it. When they no longer hold, nothing is written and the write is refused by name.
- A block's `class`: observation, inference, ruling, verification or external. The relations supersedes, refutes, depends-on, implements, verifies and conflicts-with decide whether a block is valid, superseded, refuted or needs review. A plain `read` of a block that is not valid adds `(validity ...)`; `search`, `grep` and `whereis` leave out superseded and refuted blocks, and `--all-validity` keeps them.
- `eval --lang` has default runners for `typescript` (node), `go` (go run), `rust` and `c` (compiled, then run) and `java` (single-file launch). `THEOURGIA_RUNNER_TYPESCRIPT`, `THEOURGIA_RUNNER_GO`, `THEOURGIA_RUNNER_RUST`, `THEOURGIA_RUNNER_C` and `THEOURGIA_RUNNER_JAVA` each replace one with a whole runner. Runners still need `THEOURGIA_RUNNERS=on`.

### Platforms

- macOS on x86_64 has its own measured row, read under Rosetta, and is no longer refused; there `stat` and `lstat` bind their `$INODE64` symbols.

### Changed behaviour you may notice

- When validity leaves out every hit, the human output of `search`, `grep` and `whereis` now prints the `(excluded ...)` line instead of nothing. A script that took empty output to mean no match should read `--wire`.
- `write` on a block whose mode is datum is refused `draft-on-datum-unsupported`, and `commit` refuses a draft that would put text on one. Both used to answer ok, and the edit never took effect. Change a datum block with `def`; `check` reports a datum block that already carries text.
- A daemon refuses every later write to a writer's log once the segment it was appending to is replaced by a shorter one -- a git checkout of another branch under a running daemon -- until it is restarted. Restart the daemon after switching the store's branch.
- The answers of `read`, `refs`, `commitments`, `tasks`, `names`, `uses`, `search`, `grep`, `whereis`, `outline` and `reach` end with receipt clauses. The human output is unchanged except for plain `read` and `reach`, which print whole; a script that compares whole `--wire` answers will see the new clauses at the end.
- Superseded and refuted blocks are left out of `search`, `grep` and `whereis` by default in a store that links those relations. Ask with `--all-validity` to see them.
- Starting from source takes about 1% longer: the code the premises check adds is compiled at every start (1.1%, the median of three runs of 40 interleaved pairs); the commitments registry added 0.5% before it.

### Fixed

- The daemon sampled a publication's snapshot after its fold had released the store's lock, so a commit by another process in between could go unseen until something else changed.
- A retry that completes an interrupted plan -- a commit, an import, a def -- wrote its missing members over what other requests had written to the same blocks since, and answered ok. It now checks first and answers `stale-baseline` when a block has moved. An interrupted import that created a file can now be completed; it used to answer `malformed-intent`.
- `import-md` replaced bytes that were not UTF-8 with U+FFFD and imported the file changed. A file that is not valid UTF-8, or holds a NUL byte, is now skipped and listed under `(skipped ...)`, as `import-code` does.
- `read --cut` could answer ok from a smaller cut when a record was quarantined between the cut check and the replay; it now refuses `cut-moved`.
- A code block whose derived fields raised when shown was answered `(error internal ...)` with no block named; the answer now names it.
- A value written once that merely had a conflict's shape was read as a conflict by `search`, `grep`, references, `commitments` and `tasks`; a field is contested now only when two or more of its candidates survive.
- A record definition with a malformed field spec could raise when the store read a block's names; it now binds only its type's names. The name walk's shape rules follow what Chez accepts.

### Documentation

- README corrected against the code, with a new opening line.
- The agent-memory guide, `docs/claude-code-memory.md`, checked against the code.

## 1.0.0 — 2026-09-30

*225 commits.* The first release: a block-graph store where agents and people write together, with a command line, a daemon, an MCP shell and a VS Code extension.

### The store

- A store is a directory of plain files. Each writer appends to its own segments, and the store can live in a git repository; `init` writes its `.gitignore`.
- 38 verbs, each with a usage line and a one-sentence description that `describe` answers: init, insert, set, move, del, link, unlink, def, outline, read, refs, reach, search, grep, whereis, log, tag, diff, describe, conflicts, write, restore, commit, drafts, diagnostics, discard, batch, import-md, export-md, import-code, export-code, split-suggest, supply, adopt, check, snapshot, publish and eval.
- Writing goes through drafts: `write` keeps a draft in the writer's own space, and `commit` installs it; a draft whose baseline the block has moved on from is refused `stale-baseline`.
- The block-editing verbs, `batch` and `tag <name>` accept a request identity, so a client that missed the answer can send the request again and be told what happened.
- Code and prose sit in one graph: `import-md` and `import-code` read a directory into blocks (Scheme as data with `--datum`), and `export-md` and `export-code` write them back out.
- `eval` evaluates one expression against the store in a child process with no filesystem and no network; another language runs with `--lang` where the operator has set `THEOURGIA_RUNNERS=on`.
- `supply` keeps what an editor computed (signatures, calls, diagnostics) in `<store>/derived/`, outside the log.
- `publish` receives another store's history one segment at a time.

### The daemon

- `theourgia serve` holds a store open and answers over a unix socket; the command line starts one when none is running, and finds its socket by the same rule the daemon used to create it.
- It answers through the same dispatcher as the command line. `THEOURGIA_LOCAL=1` answers in process instead.

### The MCP shell

- `theourgia-mcp --store <path>` speaks MCP `2025-11-25` over stdio: one tool per verb routed to the daemon, and `eval`, run as the shell's own child.
- A core refusal comes back as a successful tool result.
- Each session gets a writer of its own unless `THEOURGIA_WRITER` or `--writer` fixes one.

### The command line

- `theourgia` is the command; `theourgiad`, the daemon program, and `theourgia-mcp`, the MCP shell, sit beside it.
- `build.ss` compiles the tree and igropyr into a directory of objects, which start about twelve times faster than source.
- Every verb takes `--store`, `--actor`, `--socket`, `--wire`, `--req` and `--cursor`.

### Platforms and requirements

- Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1, libuv and igropyr. On another Chez version the verbs that print a datum refuse with `unsupported-printer-version`.
- Platforms with a measured row: macOS on arm64, and Linux on x86_64 and on aarch64 with glibc. On any other platform every program exits 75 with `platform-unmeasured`.

### Published to

- Homebrew: the tap `guenchi/theourgia`, whose formula builds this release with Homebrew's Chez Scheme.
- npm: `theourgia` 1.0.0.
- The VS Code extension, at its own version 1.0.0, packaged for macOS on Apple Silicon.

### Known not to work

- macOS on Intel, or under Rosetta: the core has no measured row there and exits 75 with `platform-unmeasured`.
- FreeBSD 15 on amd64 has a measured row, but the daemon does not start there yet.
- The machine registry never retires an entry: entries for deleted stores stay in it, marked active.
- The export verbs take no `--cut`: a historical cut with drafts over it has no export.
