# Changelog

## Unreleased

*9 commits since 1.0.0, on master and not yet released.* What the next release will carry so far.

### Added

- `commitments` lists the decisions still owed, and a verb can be registered as data rather than in the core's own table.
- `read <id> --cut <cut>` reads a block as it was at a causal cut, and every `log` entry gives the event's cut.
- The project template: `init --template`, `template apply` and `template export`, and the `tasks` verb.
- Name use: the names a code block uses, and the blocks that use a name.

### Fixed

- The daemon samples a publication's snapshot no later than its state.

### Documentation

- README corrected against the code, with a new opening line.

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
