# Theourgia manual

## Installing

Requirements: Chez Scheme, igropyr, and libuv. No build step when run from source; objects are one command.

Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1 (the datum printer is measured identical on these; any other version refuses the verbs that print a datum with `(error unsupported-printer-version)`).

Platforms: the ones with a measured row in `platform-numbers.sc` -- macOS on arm64, Linux on x86_64 and on aarch64 with glibc, and FreeBSD on amd64 (the row was measured on FreeBSD 15, and it is chosen by machine type alone, so another FreeBSD release on amd64 gets it too). On any other platform (another architecture, macOS on x86_64 or under Rosetta, a Linux with musl) every program writes `(error platform-unmeasured ...)` and exits 75 before it does anything else.

macOS: `brew install chezscheme libuv`
Debian / Ubuntu: `apt install chezscheme libuv1-dev`
Fedora: `dnf install chez-scheme libuv-devel`
Arch: `pacman -S chez-scheme libuv`
FreeBSD: `pkg install chez-scheme libuv`

### Homebrew

On macOS and Linux, a Homebrew tap installs everything in one step:

```
brew tap guenchi/theourgia
brew install theourgia
```

### npm

`npm i -g theourgia` installs the wrapper scripts and the sources. It only checks the requirements; when one is missing it prints the platform's install line.

The Homebrew tap and the npm package are not published yet; until they are, install from source as below.

### From source

Get a tagged tarball of Theourgia and place igropyr beside it under one library root:

```
library-root/
  theourgia/
  igropyr/
```

Compile both into one output directory. On a Homebrew machine the interpreter is `chez`; the lines below say `scheme` for the upstream build.

```
scheme --script theourgia/build.ss library-root objects
```

The three wrapper scripts -- `theourgia`, `theourgia-mcp`, and `theourgiad` -- expect `CHEZSCHEMELIBDIRS` set to the objects directory and `CHEZSCHEMELIBEXTS` set to `.so`.

Objects start about twelve times faster than source (measured: 50 ms from objects, 572 ms from source). Never place them in the source tree -- a stale `.so` beside a `.sc` is resolved in preference to it.

### Connecting a client

Create the store first, once: `theourgia init --store <path>`. The MCP shell has no local route and does not offer `init` as a tool. Then register `theourgia-mcp --store <path>` in the client's MCP configuration. One store per project or per person, on one machine.

## Quick start

```
theourgia init --template project
```

One store is one project. The `project` template gives it four roots and seven relations, and records them in the store as its template block, which `template export` prints:

| Root | What goes there |
|---|---|
| `design` | The document `design.md`; record each decision as a block of kind `decision` under it. |
| `tasks` | The document `tasks.md`; record each task as a block of kind `task` under it, with a `status` of todo, doing, done or dropped. |
| `docs/` | Documents for readers are top-level docs whose path starts with `docs/`. |
| `code/` | Code is imported so that the paths of its files start with `code/`. |

The relations are `implements` (a task or code → the decision it carries out), `depends-on` (a task or code → one it needs first), `supersedes` (a decision → the one it replaces), `refutes` (a decision or doc → one it shows is wrong), `verifies` (a test → the code or decision it checks), `conflicts-with` (two decisions that cannot both hold) and `documents` (a doc → the code or decision it explains). Each is a name `link` accepts; any other relation name still links, and these are the ones the template names. `describe`, answered by the daemon, lists the store's roots and relations, and the MCP tools that write carry the roots' sentences after the writing protocol.

The built-in templates are `project` and `memory`; `--template-file` takes one of your own. `theourgia init` without a template is the advanced form: the store has no shape until you give it one.

### A store's template

`template apply <name>`, or `template apply --file <template-file>`, gives an existing store a template: it creates the template block and each document root the store does not have, and never changes a block that exists. A root is found by its slug and its path. When something else holds a root's place -- a template already, two blocks with the slug, a block with the slug that is not a top-level document at that path, or a document without the slug at the path -- it refuses and creates nothing.

The template is data in its block: change it with write and commit like any block, and insert a new root's document; nothing else needs to change. A template block that cannot be read is listed by `conflicts`, and every verb then behaves as in a store without one. Importing a document does not make decisions or tasks.

### Decisions and tasks

A decision is a block of kind `decision`. It is owed until a block implements it -- `theourgia link <impl> implements <decision>` -- or its `status` reads done or dropped. `commitments` lists the decisions still owed, and `--all` every decision. An implementation has drifted when its own fields changed at a cut that neither its implements link nor the decision's latest edit covers: it was edited after it was linked. `--drifted` keeps the decisions with such an implementation, `--since <cut>` those whose creation the cut does not cover, and `--under <id>` those in a block's subtree. A write to a bookkeeping field (`status`, `batch`, `keywords`, `class`, `slug`) is not a change: a task set to done after it was linked has not drifted.

`tasks` lists the blocks of kind `task`, each with its status, its batch, what it implements, and `(unlinked)` when it implements no live decision; `--status` and `--batch` filter them. A task's implements edge discharges its decision only once the task is done.

In a store with a template, both verbs list by default what is under the template's root -- `design` for decisions and `tasks` for tasks in the project template -- and end with `(scope <root-id> (outside <n>))`, n being how many more rows `--under root` would list. In a store without a template nothing is narrowed and no scope clause is added. Neither verb writes.

## What a store is

### A store

A store is a directory, and everything in it is a plain file. meta.sexp names the store. Each writer owns a directory under writers/, holding one numbered segment file per run of writing: one record per line, a CRC and an S-expression, readable by a person and diffable by git. Large payloads go to blobs/. A writer's published.sexp records which of its segments have been handed to other machines.

```
store/
  meta.sexp            the store's identity
  instance.sexp        which machine and directory this copy is
  lock                 zero bytes; only its existence is used
  .gitignore           written by init: what stays out of git
  writers/
    f0sjkuzn/
      owner.sexp       who this writer is
      000001.sexp      one record per line: a CRC and an S-expression
      published.sexp   which segments have been handed to other machines
      incoming/        published segments kept but not installed
      working/         this writer's drafts -- not part of the log
  blobs/               payloads too large to sit in a record
  snap/                a cached reduction; delete it and it is rebuilt
  derived/             facts an editor supplied; the next supply rebuilds them
```

### A block

A block is the smallest unit of content. It has a stable id, written <writer>.<base36 ordinal> -- f0sjkuzn.au is one. It has fields: kind, title, src, keywords, and whatever else its kind calls for. It has a position: a parent and an ordinal among that parent's children. And it has edges: named, directed relations to other blocks.

```
$ theourgia read f0sjkuzn.au

(ok ((id . "f0sjkuzn.au")
     (deleted . #f)
     (fields (keywords . "retry, supervisor, worker")
             (kind . section)
             (src . "...")
             (title . "Why the retry is three deep"))
     (position "f0sjkuzn.a" . 3)
     (edges ((ref . "f0sjkuzn.b2")))))
```

### The log is the only truth

State is a deterministic reduction over a fixed set of events. Given the same log you get the same state, on any machine, in any order of reading. Everything else -- the snapshot in snap/, the search index, an outline, a markdown export -- is derived. Deleting a derived artifact costs a rebuild and nothing else, which is why none of them has to be kept correct by hand.

#### Deletion keeps the history

del retires a block: the reduction stops treating it as live, and the record of its life stays in the log. A block that was written by mistake can be taken out of the view without taking it out of the history, and what was written before the mistake is still readable at an earlier cut.

#### Reading the past

`log` gives every event its cut: `cut` is the causal cut right after the event, and `past` the one right before it -- the event's premises, each with its own premises, and nothing a writer did without depending on it. `read <id> --cut <cut>` gives a block as it was at a cut, written out as `(("writer" . 12) ...)` or as a tag name, and `diff <past> <cut>` gives what an event changed. A block that did not exist yet answers `unknown-id`. A cut naming an event not received, a writer twice, or an event without its premises is refused `cut-unavailable`; `--working`, `--working-info`, `--writer` and `--signature` ask about the present and are refused with `--cut`. Each such read replays the log from its beginning to the cut.

### One graph over code and prose

The split between a code repository and a documentation store is a filesystem accident, not a property of the knowledge. A section of a design, a Scheme macro, a function in another language and a decision record are all blocks: each has its own id, its own history and its own edges, and each can be read without reading whatever it sits next to. import-code reads a directory of source into the store; with --datum it reads Scheme as data rather than as text. A Scheme definition imported as a library block is a block the store can evaluate against and follow by name; for other languages the editor supplies what its language server knows (signatures, keywords, call edges, diagnostics), and every answer built on those facts names the editor that supplied them and counts the facts the source has since outrun.

### One answer shape

Every verb answers with one line: an S-expression that begins with ok or error. --wire asks for the machine-facing spelling of it. There is no second, prettier output mode that an agent has to parse differently, and no rendering markup in the way -- the reply is the same data the store holds, and the agent can judge the size of what it is about to read before it reads it.

### Read receipts

A read of the committed store ends with a receipt: `(cut <cut>)`, the state the answer was read from, and `(versions ((<id> . "<hash>") ...))`, the version of each block it shows. A version is the token `--if-unchanged` compares, so a block read and written back with it is refused if it changed in between. `read` (plain, `--md`, `--recursive`), `refs`, `commitments`, `tasks`, `names` and `uses` carry both clauses; `search`, `grep` and `whereis`, which already give their cut, add the versions; `outline` and `reach` carry the cut only. A working read, a draft listing, `log`, `diff`, `conflicts`, `check`, `diagnostics`, `describe` and the exports carry none. The human output shows the receipt only for a plain `read` and for `reach`. Recall does not need either clause; a write that must not overwrite a later change does.

### Class and validity

A block's `class` says what kind of statement it is: `observation`, `inference`, `ruling`, `verification` or `external`. With no class, a decision is a ruling and anything else an observation. Observations, rulings and verifications are authoritative; inferences and external material are not.

Blocks are linked with six relations: `supersedes`, `refutes`, `depends-on`, `implements`, `verifies` and `conflicts-with`. From those links each block has a validity, `valid`, `needs-review`, `superseded` or `refuted`; README's "Class and validity" gives the rules. Only an authoritative block can supersede or refute another. From any other block, `supersedes`, `refutes` and `conflicts-with` are only proposals.

A plain `read` of a block that is not valid adds a `(validity ...)` clause, and `read` never hides a block. `search`, `grep` and `whereis` leave out superseded and refuted blocks; the `(excluded ...)` clause that counts them is in the `--wire` answer only, and the human output shows the hits in force and nothing else -- nothing at all when every hit was left out, except an `(incomplete ...)` line if the store missed a writer. `--all-validity` leaves nothing out and names every hit that is not valid. A store that links none of the six relations answers as before.

### Writers

Every agent and every person writes under a writer id. The id is what block ids are built from, so a block carries the identity of who wrote it for as long as it exists. One writer id is held by one live agent at a time -- that is a rule in the documentation, not a mechanism in the code; a later session may bind the same id and carry on with its drafts.

### Drafts, then commit

write puts a proposed next version of a block into the writing agent's own space. It is not in the log and nobody else can see it. commit, in describe's own words, installs a writer's drafts into the store as one change; the process that owns the store carries commits out one at a time, so the outcome of a race is decided rather than interleaved. With --based-on, the commit states the version the draft was made against. If that premise has moved, the commit is refused.

#### A refusal hands back the winner

A refused commit does not simply say no. It answers stale-baseline, and it names the version the draft was made against, the version the block carries now, and the records applied since -- the winner's content among them. The agent that lost the race does not have to go and fetch the new state to find out what happened: it already has what it needs to merge and send the commit again.

### One store per scope

One store per project or per person, on one machine. The machine registry tracks which stores live where; adopt transfers a store after a move or a restore.

### The daemon

Every verb can run standalone: open the store, answer, exit. For a session that sends many verbs, serve holds the store open and answers over a unix socket. The client finds the socket by the same rule the daemon used to create it, so nothing needs to be told where it is. The daemon's answer is the CLI's answer, byte for byte; both call the same dispatcher, and no verb or answer shape exists in one and not the other, except eval: the daemon does not run it -- sent over the socket, it is answered unknown-verb -- and the CLI runs it in core.sc as its own child. When no daemon is running, the CLI starts one beside itself and asks again; `THEOURGIA_LOCAL=1` skips the socket and answers in process, for debugging. The daemon answers reads from the state it last folded, and can be one commit behind an outside change: a read that notices a record appended by another process, or a segment a git pull brought in, is still answered from that state and asks for a fresh fold, and the reads after the fold see the change. A write is never answered from that state: it is decided on the store as it stands under the lock. While a daemon serves a store, write through it.

### Put the store in git

A store is plain files: meta.sexp, the lock, each writer's numbered segments with one record per line and its owner.sexp and published.sexp, and the blobs. Committing them commits the store itself. There is no export step, nothing to project, and nothing that has to be kept in step with the content, because the content is what was committed. A segment file's diff is the records that were appended: the revision of these pages that corrected four figures shows up in git as 15 added lines and no deleted ones, the log being append-only.

#### What to exclude, and what to keep

init writes `<store>/.gitignore`, which keeps out what belongs to this copy of the store rather than to its history:

```
/instance.sexp
/request-index.sexp
/snap/
/derived/
/writers/*/working/
/writers/*/draft.lock
*.tmp-*
!/writers/*/incoming/*
```

instance.sexp is this copy's identity. request-index.sexp and snap/ are rebuilt from the log, and derived/ by the next supply. The working areas and draft locks are one machine's drafts. The `*.tmp-*` files are what an atomic write keeps after a failure -- except under a writer's incoming/, which the last line brings back, because a candidate kept there is evidence a replay is answered from.

Track the lock. It is zero bytes and only its existence is used, but a clone without it answers store-busy to every verb, reads included -- and init will not create it in a store that exists.

Add the store as a whole, never named files -- `git add <store>` -- because a segment rolls over after 1 MiB, and a list of named files misses the one that rolled since.

#### What a clone can do, and when

```
$ git clone <repository> site && cd site
$ theourgia outline --store store
- 8pniyrtk.j  Home
- 8pniyrtk.k  Why not markdown
  ...

$ theourgia insert --store store --under root --title "..."
(error refused (instance inode))

$ theourgia adopt --store store
(ok (from "8pniyrtk") (to "x9a3vtll") (prefix 1 68042 113) (reason identity))

$ theourgia insert --store store --under root --title "..."
(ok (events (("x9a3vtll" . 1))) (state (("x9a3vtll.1" . "bf28cd40..."))) ...)
```

> **What adopt does to a clone** instance.sexp binds a store to the machine and the directory it was made in, so the first write from a clone is refused rather than accepted into a second copy of the same writer's log. A clone that carries another copy's instance.sexp is refused for the mismatch, as above; a clone without one -- what the .gitignore init writes gives -- is refused `(error refused no-instance (remedy adopt))`. adopt mints a new writer id and leaves the history where it is: the clone keeps every block it was given, and its own records go under the new id from then on. Reading never needed any of this -- outline, read, search and eval answer from a fresh clone straight away. Every answer above came from a clone of this site; the outline is abridged and the titles passed to insert are left out, and nothing else is changed.

## 43 verbs

Every verb below, with its usage line and its one-sentence description, is rendered from what the store answers to describe. Nothing on this page is typed by hand, so it cannot drift from the binary that produced it. The tag on the right of each signature says where the verb runs: local in the client process, daemon over the socket, or child: a process the caller runs itself, so the store's server never runs user code.

### Reading a store

```
read <id> [--md] [--recursive] [--writer <name>] [--working] [--working-info] [--signature] [--cut <cut>]
```

Read one block: its fields, or its text. With --signature, the signature an editor supplied for it, of the committed store or, with --working, of the writer's working view. With --cut, the block as it was at a causal cut (a literal or a tag name): the same answer over that cut's state, or unknown-id where the block did not exist yet. (daemon)

```
refs <id>
```

List the relations a block takes part in. (daemon)

```
search <query> [--all] [--all-validity]
```

Find blocks whose title, keywords or text match every word given. (daemon)

```
grep <pattern> [--under <id>] [--all] [--all-validity]
```

List the lines that contain a pattern, literally. (daemon)

```
whereis <name> [--all-validity]
```

Say where a name is defined, and which libraries carry it. (daemon)

```
log [<id>]
```

Show the changes recorded, for the store or for one block. (daemon)

```
outline [--depth <n>] [--with-keywords] [--with-signatures]
```

List the blocks as a tree of titles. (daemon)

```
tag [<name>]
```

Name the current cut, or list the names already given. (daemon)

```
diff <cut> <cut>
```

Report what changed between two cuts. (daemon)

```
describe
```

List the verbs, what each is for, and the writing protocol. (daemon)

```
commitments [--open] [--all] [--drifted] [--since <cut>] [--under <id>]
```

List the decisions still owed: by default those neither implemented (an incoming implements edge) nor marked done or dropped; with --all every decision. --drifted keeps those with an implementation whose latest change was seen neither by its implements link nor by the decision's latest edit. (daemon)

```
tasks [--status <status>] [--batch <batch>] [--under <id>]
```

List the tasks: by default those under the root the store's template names for tasks, else every task; each with its status (todo, doing, done or dropped), its batch, what it implements, and (unlinked) when it implements no live decision. (daemon)

```
names <id>
```

Which names a code block uses, as its stored code shows them, and which libraries a library block imports. A datum block's body is walked as data: a symbol is a use unless something in the form binds it, and an unknown macro's operands count as uses. A text block's uses are every token its language's identifier pattern matches, comments and strings included. No name is resolved. (daemon)

```
uses <name> [--under <id>]
```

The live code blocks that use a name, compared whole and exactly, one row per block with its library and mode; with --under, those inside that block. It is not find-references: a use is listed whether or not anything defines the name, and a text block that defines the name also lists it. (daemon)

### Making and changing blocks

```
init [--template <name>] [--template-file <template-file>]
```

Create a store in this directory; with --template (a built-in such as project) or --template-file, then apply that template to it. (local)

```
template <action> [<name>] [--file <template-file>]
```

Apply a template to this store (apply <name>, or apply --file <template-file>): create the template block and each document root the store lacks, changing nothing that exists. Or print the store's template (export). (daemon)

```
insert [--under <id>] [--after <id>] --title <text> [--text <text>] [--keywords <text>]
```

Add a block under a parent, with a title and optional text. (daemon)

```
set <id> <field> <value> [--if-unchanged <version>] [--based-on <version>]
```

Replace one field of one block. (daemon)

```
move <id> <parent> [--after <id>]
```

Move a block to another parent, optionally after a sibling. (daemon)

```
del <id>
```

Retire a block. Its history stays. (daemon)

```
link <from> <rel> <to>
```

Record a named relation between two blocks. (daemon)

```
unlink <from> <rel> <to>
```

Remove a named relation between two blocks. (daemon)

```
def <name> [--under <library>] <source>
```

Define or replace one named definition. (daemon)

### Drafts, and committing them

```
write <block> <bytes> [--writer <name>] [--based-on <version>] [--working-cut <cut>] [--working-parent-writer <name>] [--working-parent <version>] [--rebase]
```

Save a draft of a block in a writer's own space, without committing it. (daemon)

```
restore <version> [--writer <name>]
```

Take an earlier version of a draft back into a writer's space. (daemon)

```
drafts [--writer <name>]
```

List the drafts a writer is holding. (daemon)

```
discard <block> [--writer <name>]
```

Throw away a writer's draft of a block. (daemon)

```
commit [<block> ...] [--writer <name>] [--working-version <block>=<version>]
```

Install a writer's drafts into the store as one change. (daemon)

### Importing, exporting, splitting

```
import-md <dir> [--allow-delete]
```

Read a directory of markdown into the store. (daemon)

```
export-md <dir> [--with-ids] [--working] [--writer <name>]
```

Write the store out as markdown. (daemon)

```
import-code <dir> [--allow-delete] [--datum]
```

Read a directory of source into the store. With --datum, the whole-line ; comments directly above a form become its doc; a ; comment inside a form is dropped, and the answer warns with its line and column. A #| |# block comment, and any comment inside a datum discarded with #;, is dropped with neither. With --datum, only Scheme files are read: those the language table gives to Scheme by extension (ss, sc, scm, sls, matched exactly); every other file the directory walk returns (it does not enter a name that starts with a dot) is listed, in the order it was walked, in the answer's skipped clause, which is there only when something was skipped. A file the reader refuses is named in the refusal's path clause. Text mode, without --datum, skips a file that is not UTF-8 text or that holds a NUL byte, and lists it in the same skipped clause; it is decided by the bytes, not the name, so a source file in a legacy 8-bit encoding or in UTF-16 is skipped and listed, not imported, unless its bytes happen to be valid UTF-8 with no NUL. (daemon)

```
export-code <dir> [--raw] [--datum] [--working] [--writer <name>]
```

Write the store's source back out to a directory. (daemon)

```
split-suggest <file> [--output <review-file>] [--symbols <symbols-file>]
```

Propose where a long file could be divided into blocks. With --symbols, the cuts come from an editor's list of the file's top-level symbols instead of the language's definition patterns; the answer's cuts-from says which. (daemon)

### Checking, syncing, serving

```
check
```

Read the whole store and report whether it is sound. (daemon)

```
snapshot
```

Record the current state as a reduction that can be reopened quickly. (daemon)

```
adopt
```

Take in records that are on disk but not yet in the log. (daemon)

```
conflicts
```

List blocks whose writers disagree. (daemon)

```
publish <writer> <segment> <file> [<sha256>]
```

Publish a segment of a writer's log. (daemon)

```
batch <intents>
```

Carry out several changes as one request. (daemon)

### Evaluating

```
eval [--lang <language>] [--cut <cut>] [--under <library>] [--working] [--latest] [--writer <name>] [--timeout-ms <n>] [--memory-bytes <n>] [--output-bytes <n>] <source>
```

Evaluate source against the store, or against a writer's working view with --working: Scheme by default, another language with --lang, whose runner runs only where the operator has set THEOURGIA_RUNNERS=on. It runs as a child process of the caller, never in the store's server. (child)

### Derived data from an editor

```
supply <kind> <file> [--for <writer>] [--clear]
```

Keep facts an editor computed from an export-code projection -- signatures, calls, diagnostics -- beside the store, never in it. The file's header names the projection it was made from; every file it lists is checked against the store's own re-projection, of the committed state or, with --for, of that writer's working view. A fact is used only while the blocks it depends on still project as they did. With --clear, the table the header names is removed. (daemon)

```
reach <id> [--rel <rel>] [--depth <n>]
```

List the blocks a block reaches over the edges an editor supplied (calls, by default), outward, up to --depth hops (1 by default); the block itself is at depth 0. (daemon)

```
diagnostics [--writer <name>]
```

List the diagnostics an editor supplied for a writer's working view, by block and then by start, each at a byte range of its block's own src. (daemon)

## Options every verb takes

These are accepted by every verb, and are left out of each usage line above.

| Option | What it does |
|---|---|
| `--store <dir>` | Which store. Falls back to `THEOURGIA_STORE`, then `.` |
| `--actor <name>` | Who the request is from. Falls back to `THEOURGIA_ACTOR`, then `USER`, then `cli` |
| `--socket <path>` | Reach a daemon at this path instead of the default |
| `--wire` | Print the answer as one S-expression per line rather than for a person to read |
| `--req <id>` | The request's identity, so a retry is recognised as the same request rather than a second one |
| `--cursor <w:n>` | The position this request is composed against |

A request that reaches a server naming `--store`, `--actor`, `--wire` or `--socket` inside it is refused `transport-option-in-rpc`: those say where a request goes, and it has already gone.

| Variable | What it does |
|---|---|
| `THEOURGIA_STORE` | The store when `--store` is absent |
| `THEOURGIA_ACTOR` | Who the requests are from when `--actor` is absent |
| `THEOURGIA_WRITER` | Whose drafts a request reads and writes when `--writer` is absent |
| `THEOURGIA_HOME` | Where the machine registry and its lock live. Falls back to `$HOME/.theourgia`, or `/tmp/.theourgia` when `HOME` is unset |
| `THEOURGIA_RUN` | The run root holding daemon sockets. Falls back to `$HOME/.theourgia/run` |
| `THEOURGIA_LOCAL` | `1` answers in process even when a daemon is there |
| `THEOURGIA_SCHEME` | The Chez binary `eval` starts its child with. Falls back to `scheme` |
| `THEOURGIA_EVAL_SLOTS` | How many evaluations one run root runs at once. Falls back to the number of online processors |
| `THEOURGIA_RUNNERS` | `on` turns on `eval --lang`'s runners for another language |
| `THEOURGIA_RUNNER_CHEZ` | The operator's runner for `eval --lang chez`, replacing the language table's fields |
| `THEOURGIA_TRACE` | `1` writes filesystem and dispatch events to stderr |

## Working with agents

### The MCP shell

`theourgia-mcp --store <path> [--socket <path>] [--actor <name>] [--writer <name>]` speaks MCP `2025-11-25` over stdio. Create the store with `theourgia init` first, then register the shell once:

```
claude mcp add theourgia -- theourgia-mcp --store /path/to/store --actor <name>
```

Or in a client's JSON configuration:

```json
{
  "mcpServers": {
    "theourgia": {
      "command": "theourgia-mcp",
      "args": ["--store", "/path/to/store", "--actor", "<name>"]
    }
  }
}
```

One tool per verb the shell can carry out -- every verb routed to the daemon, and `eval` -- named `theourgia_<verb>`, each taking `{"argv": [...]}` with the command line's own arguments as an array of strings. The shell asks the store's daemon for the catalogue on every call and never caches it, so a verb added to a newer build appears without restarting the server. `theourgia_insert` and `theourgia_write` carry the core's writing protocol in their descriptions.

The core's answer arrives as S-expression text, byte for byte. A core refusal is a successful tool result, not a JSON-RPC error: `(error unknown-id ...)` is the store's answer to the question asked, delivered with `isError: false`. Only the shell's own failures use the JSON-RPC error channel: a frame it could not parse, a frame past the size limit, or a connection that was made and then lost. A lost connection says the request may have been carried out, because it may have been; a request that reached nobody says it was not. A store condition the shell meets before the verb runs -- a daemon that would not start, a refused or unreadable catalogue -- is a tool result with `isError: true` and the refusal in `_meta.refusal`: read the text, not the flag.

`eval` is offered as `theourgia_eval`. The shell runs `core.sc eval` as its own child, not through the daemon: the store's server never runs user code. The call's optional `stdin` string becomes the child's standard input; without one it reads `/dev/null`. Transport options (`--store`, `--wire`, `--socket`, `--actor`) are refused with `transport-option-in-rpc`; the writer defaults to the session's unless `--writer` is given; a language other than Scheme runs only where the shell was started with `THEOURGIA_RUNNERS=on`; a lost evaluation is a protocol error naming the reason. `init` is not offered, because the store must exist before the MCP server starts.

The catalogue comes from whichever daemon is serving the store, and a client may read `tools/list` only once per session. After a core upgrade, stop the store's daemon first — the next request starts one from the new core — then restart the agent's session. A session restarted while the old daemon still runs is given the old catalogue and sees none of the new verbs.

### The writing protocol

describe carries this text once, beside the verbs, and the MCP shell puts it in the descriptions of `theourgia_insert` and `theourgia_write`; in a store with a template, the tools that write carry the roots' sentences after it:

```
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
```

### One writer per session

A writer id is held by one live agent at a time. On the command line it is `--writer`, else `THEOURGIA_WRITER`; with neither, a draft verb is refused `writer-required`. The MCP shell decides it once, when it starts: `THEOURGIA_WRITER` when set and not empty, else `--writer`, else one derived for the session, `<actor>-<start>-<pid>` -- the actor reduced to what a writer id may hold, the start in milliseconds in base 36, the shell's process id. The initialize response names it, and a tool call's own `--writer` overrides it for that call only. A name a writer cannot have (1 to 128 characters of `a-z`, `0-9`, `.`, `_`, `-`, and not `.` or `..`) stops the shell at start with its usage line, exit 2.

A later session may bind the same id and carry on with its drafts, by starting with `THEOURGIA_WRITER=<that name>`, but two agents writing under one id at the same moment overwrite each other silently. There is no lock and no refusal: the rule is stated here, and the failure it prevents is silent. Derived writers differ between sessions, so the risk comes from a fixed name: a host that sets one `--writer` or `THEOURGIA_WRITER` for several agents makes them share it.

### One store per machine

One store per project or per person, on one machine. The machine registry records which stores are where. Moving a store to another machine requires adopt, which transfers its entry in the registry. Two machines writing one store is not supported.

### The memory recipe

To use a store as persistent memory for Claude Code sessions: turn off the built-in auto-memory (CLAUDE_CODE_DISABLE_AUTO_MEMORY=1 or in settings.json), register the store as an MCP server, and inject an outline at session start with a SessionStart hook that runs theourgia outline --depth 1. CLAUDE.md then carries the recall and write instructions: search to find, read to retrieve, insert to add (under the right section, with title, keywords, and text following the block protocol), and write+commit to update an existing block rather than duplicating it. The full recipe is in docs/claude-code-memory.md in the repository.

## The VS Code extension

### Before you start

The extension drives the Theourgia core installed on the machine, and needs VS Code 1.138.0 or later. Install the core first (from source for now: the Homebrew tap and the npm package are not published yet), then configure the settings:

| Setting | What it is |
|---|---|
| `theourgia.corePath` | The directory holding the core: a checkout, or a directory built by the core's build.ss. The extension tells which by looking for `client.sc` or `client.so`, and refuses a directory with neither. Required. |
| `theourgia.libDirs` | Extra directories for `CHEZSCHEMELIBDIRS`, after `corePath`. The directory holding `corePath` is searched after them (the extension adds it itself), so a copy named here comes first. The core imports igropyr, so the directory holding igropyr belongs here, unless it is the one holding `corePath`. |
| `theourgia.store` | The store directory, passed as `--store`. Required. |
| `theourgia.actor` | The name recorded with every write. Defaults to the OS user name. |
| `theourgia.writer` | Passed to the core as `THEOURGIA_WRITER`, and the writer whose working view Supply Diagnostics reads. Defaults to the actor. |
| `theourgia.scheme` | The Chez Scheme executable. Defaults to `scheme`. |
| `theourgia.timeoutMs` | How long to wait for one request before killing the child process (SIGTERM, then SIGKILL two seconds later). Defaults to 30000. |

Until `corePath` names a checkout or a built directory and `store` is set, the outline is empty, the status bar shows a gear whose tooltip names each setting at fault, and every command answers that they have to be set first. `store` is only checked for being set: a path that does not exist passes, and the requests go to the core.

Each request runs the core's client as a child process, `<scheme> --script <corePath>/theourgia.sc <verb> ... --store <store> --actor <actor>`, with `CHEZSCHEMELIBDIRS`, `CHEZSCHEMELIBEXTS`, `THEOURGIA_ACTOR`, `THEOURGIA_WRITER` and `THEOURGIA_SCHEME` set from the settings. The client starts the store's daemon when none is running. A core that cannot find a library is answered with advice to add its directory to `libDirs`; a platform the core has no row for is answered with the core's own remedy.

### Installing the extension

Install the `.vsix` package for your platform from the command line:

```
code --install-extension theourgia-darwin-arm64.vsix
```

The packages are named `theourgia-<target>.vsix`. The Visual Studio Marketplace listing arrives when the extension is published there.

### What it does

An outline tree of the store appears in the side bar. Expanding a node lists the blocks directly under it; a row shows the block's keywords, and an icon for a conflict, an orphan or an unknown mark. Clicking a block opens it. A prose block opens as a markdown buffer: its heading, then this window's working draft of its body. The heading is not edited there -- a save that changed it is refused `prefix-changed`.

Saving writes a working draft first, under a writer of this window's own (`window-<session>`), reads it back, and then sends a commit carrying the draft's version. The request id, cursor and record are written to disk before the commit is sent, so a save whose answer is lost -- a timeout, a lost connection -- stays pending and is retried: at startup, on a settings change, or with "Retry Pending Saves". A commit is refused `stale-baseline` when the block itself has changed since the draft's baseline. When other writers have written since, but not to this block, the commit is accepted and the save names those writers; the number shown beside each, worded as records since the save's baseline, is that writer's latest record number, not a count of the records added since. A commit the core refuses (`stale-baseline`, `working-version-changed`, `changed`) is shown as an error naming the refusal and its clauses, and says the text is still in the file and has not been saved; nothing is merged for you. Before anything is sent, a save is refused for a byte-order mark, bytes that are not UTF-8, a file on disk that differs from the editor, or a changed heading.

"Reconcile Block" is for a block file holding a version neither this window nor the store wrote. If the file still begins with the block's heading, it keeps the text as a draft against the store without asking. Otherwise it offers two choices -- keep my text, with the block's heading in front of it, or take the store's version -- and publishes a new version either way; nothing is deleted.

Right-clicking a node and choosing "Open as Document" composes the block and everything under it into one read-only markdown document. The opened block's heading is level 1 and each level under it one deeper, down to level 6; headings inside a body are shifted down with it, and each block is preceded by a `<!-- theourgia block <id> depth <n> -->` marker. A block that is deleted, cannot be placed, appears twice or is not text refuses the whole document rather than leaving a gap. The document is composed again each time the command runs.

The tree has a second mode, Files, showing the directory tree that export would write, with a group for blocks that are in no file. A store whose root blocks carry paths opens in Files mode; "Show Files" and "Show Outline" switch, and the choice is kept per store. "New File Here" creates a root `doc` block with that path (`.md` appended) and title. "Move to Directory" and "Rename File" read the block's version when they run and set its path with `--if-unchanged`: a block that changes before the answer is refused, and the view refreshes.

A text-mode code block opens as its source in the language its `lang` field names, or as plain text without one. A datum-mode block -- a library `import-code --datum` made, or one definition in it -- opens read-only: the library's file as `export-code --datum` writes it, at the block's own place, with a note saying why. The extension cannot write a datum block yet, and nothing is written or committed for it; Go to Definition on a datum definition opens the same view at its `(define ...)`. "Go to Definition", as a command and as the editor's own, asks the store's `whereis` which block defines a name, offers a choice when several do, and the nearest names when none does. "Suggest a Split" asks the core where a source file on disk could be divided into blocks, using the editor's own symbols for the cuts when it has them and the language's patterns otherwise, and opens the review file the core wrote; nothing is recorded in the store.

Resting the pointer on a name in a block's file, a composed document or a datum view adds, beside the language's own hover, what the store holds about it: blocks that link to the block under the pointer or to the block that defines the name (with the relation), blocks that refer to them in their text, and prose blocks -- documents, sections, decisions, tasks -- that mention the name as a whole word. Each line gives the title, the kind and the first sentence, and opens the block; past five lines, the last one lists them all. The hover only reads, from the store the document came from, and its answers are kept for fifteen seconds, or until this window saves, changes the tree or its settings.

"Search Blocks" asks for words that must all match: one hit opens straight away, several give a list with scores. "Show Store Status" and the status bar show the store, actor, conflict count, pending and blocked saves. "Other Sessions" lists the unsent saves another window left behind, to take over or discard. "Migrate Legacy Block Files" moves verified block files of an earlier version into a recovery archive. The store is checked with `check` each time the extension connects to it -- at start and after a settings change -- and a bad verdict is warned about once per session; the extension's log is the "Theourgia" output channel.

When a writer's log cannot be read, the outline shows the blocks the other writers' logs can still place -- a block whose records depend on the unreadable writer's stays pending and is not shown -- and says which one is missing, the path and the reason. The note is carried by the outline, the status bar, search, Go to Definition and the banner of an opened block or document, and it belongs to the last reading: the next complete reading clears it.

### Supplying what the editor knows

Three commands hand the store facts that the editor's language support computes: "Theourgia: Supply Signatures and Keywords", "Theourgia: Supply Calls" and "Theourgia: Supply Diagnostics". Each projects the store's code with `export-code` into the extension's own storage, asks VS Code's providers for that language (symbols, hover, call hierarchy, diagnostics), and hands the facts to the core with `supply`, which keeps them beside the blocks with the editor and version they came from. The language's own extension must be installed: for C, clangd.

Signatures, keywords and calls are taken from the committed store. A block's signature is the detail of its most representative top-level symbol -- a function, method or constructor, then a type, then anything else -- else the first line of code in that symbol's hover; a call into the same block gives no edge. Diagnostics are `theourgia.writer`'s, taken from its working view, and are collected once the analysis has been quiet for a second, at most after ten; a supply taken at the cap says the analysis may be incomplete. Facts are grouped by VS Code's language id, and VS Code files `.h` under C++, so a header's facts sit in the cpp table. A run stops if a projected file or document changes, or the store changes, while it is collecting. The supply file is removed afterwards, except one the core refused `supply-malformed`, which is kept to be read.

An agent reads the facts through MCP; from the command line they come back from `read <id> --signature`, `refs <id>`, `reach <id>`, `search <word>` and `diagnostics --writer <w>`. Each answer names the source of its facts (`via`) and how many of them are stale (`stale`). The facts are as fresh as the last supply: after a source changes, supply again.

1.0 does not show these facts inside the editor; that is a later release.

### Platforms

Packages are built for macOS on Apple Silicon and on Intel, and Linux on x86_64 and arm64. The core has no measured row for macOS on Intel yet, so there the extension installs but the core refuses to start (`platform-unmeasured`). The extension takes a file lock through a small native module (`flock`) built on each target's own runner. There is no Windows package.

## Limits

Two agents on one writer id: silent overwrite, no detection. The rule is in the documentation; the code does not enforce it.

Two machines on one store: not supported. A store belongs to one machine's registry; moving it to another requires adopt.

Platforms: only those with a measured row run -- macOS on arm64, Linux on x86_64 and aarch64 with glibc, FreeBSD on amd64 (measured on FreeBSD 15; the row is chosen by machine type, not by release). Anything else exits 75 with platform-unmeasured; the remedy is to run test/probe/layout.c there and add its row.

Exports at a cut: export-md and export-code take no --cut. They write the committed state, or with --working the writer's working view; only a --working export's answer names the cut its view stands on, and an earlier cut has no export.

Runners (--lang): the Scheme evaluator runs in a sandboxed child process with no filesystem and no network. A runner for another language (node, python3, sh, or chez, which runs Scheme outside the sandbox with the store's libraries on its path) gets a projected copy of the store's code files in a temporary directory and the machine's own interpreter, so it has the reach of a local script. Runners are off by default (THEOURGIA_RUNNERS=on).

