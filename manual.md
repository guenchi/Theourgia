# Theourgia manual

## Installing

One set of prerequisites: Chez Scheme, igropyr, and libuv. No build step when run from source; objects are one command.

Chez Scheme 10.x, measured on 10.1 and 10.3; the minimum is an open release item.

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

The tap arrives with the 1.0 release.

### npm

`npm i -g theourgia` installs the wrapper scripts and the sources. It only checks the prerequisites; when one is missing it prints the platform's install line. The npm package arrives with the 1.0 release.

### From source

Get a tagged tarball of Theourgia and place igropyr beside it under one library root:

```
library-root/
  theourgia/
  igropyr/
```

Compile both into one output directory:

```
scheme --script theourgia/build.ss library-root objects
```

The three wrapper scripts -- `theourgia`, `theourgia-mcp`, and `theourgiad` -- expect `CHEZSCHEMELIBDIRS` set to the objects directory and `CHEZSCHEMELIBEXTS` set to `.so`.

Objects start about twelve times faster than source (measured: 50 ms from objects, 572 ms from source). Never place them in the source tree -- a stale `.so` beside a `.sc` is resolved in preference to it.

### Connecting a client

Register `theourgia-mcp --store <path>` in the client's MCP configuration. One store per project or per person, on one machine.

## What a store is

### A store

A store is a directory, and everything in it is a plain file. meta.sexp names the store. Each writer owns a directory under writers/, holding one numbered segment file per run of writing: one record per line, a CRC and an S-expression, readable by a person and diffable by git. Large payloads go to blobs/. published.sexp records what has been handed to other machines.

```
store/
  meta.sexp            the store's identity
  instance.sexp        which machine and directory this copy is
  writers/
    f0sjkuzn/
      owner.sexp       who this writer is
      000001.sexp      one record per line: a CRC and an S-expression
      working/         this writer's drafts -- not part of the log
  blobs/               payloads too large to sit in a record
  published.sexp       what has been handed to other machines
  snap/                a cached reduction; delete it and it is rebuilt
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

### One graph over code and prose

The split between a code repository and a documentation store is a filesystem accident, not a property of the knowledge. A section of a design, a Scheme macro, a function in another language and a decision record are all blocks: each has its own id, its own history and its own edges, and each can be read without reading whatever it sits next to. import-code reads a directory of source into the store; with --datum it reads Scheme as data rather than as text. A Scheme definition imported as a library block is a block the store can evaluate against and follow by name; for other languages the editor supplies what its language server knows (signatures, keywords, call edges, diagnostics), and every answer built on those facts names the editor that supplied them and counts the facts the source has since outrun.

### One answer shape

Every verb answers with one line: an S-expression that begins with ok or error. --wire asks for the machine-facing spelling of it. There is no second, prettier output mode that an agent has to parse differently, and no rendering markup in the way -- the reply is the same data the store holds, and the agent can judge the size of what it is about to read before it reads it.

### Writers

Every agent and every person writes under a writer id. The id is what block ids are built from, so a block carries the identity of who wrote it for as long as it exists. One writer id is held by one live agent at a time -- that is a rule in the documentation, not a mechanism in the code; a later session may bind the same id and carry on with its drafts.

### Drafts, then commit

write puts a proposed next version of a block into the writing agent's own space. It is not in the log and nobody else can see it. commit, in describe's own words, installs a writer's drafts into the store as one change; the process that owns the store carries commits out one at a time, so the outcome of a race is decided rather than interleaved. With --based-on, the commit states the version the draft was made against. If that premise has moved, the commit is refused.

#### A refusal hands back the winner

A refused commit does not simply say no. It answers stale-baseline, and it names the version the draft was made against, the version the block carries now, and the records applied since -- the winner's content among them. The agent that lost the race does not have to go and fetch the new state to find out what happened: it already has what it needs to merge and send the commit again.

### One store per scope

One store per project or per person, on one machine. The machine registry tracks which stores live where; adopt transfers a store after a move or a restore.

### The daemon

Every verb can run standalone: open the store, answer, exit. For a session that sends many verbs, serve holds the store open and answers over a unix socket. The client finds the socket by the same rule the daemon used to create it, so nothing needs to be told where it is. The daemon's answer is the CLI's answer, byte for byte; both call the same dispatcher, and no verb or answer shape exists in one and not the other. When no daemon is running, the CLI starts one beside itself and asks again. The daemon answers one commit behind an outside change: a record appended by another process or a git pull is applied before the next request, so what the daemon says is never staler than the previous commit.

### Put the store in git

A store is plain files: meta.sexp, one numbered segment per writer with one record per line, the blobs, and published.sexp. Committing them commits the knowledge base itself. There is no export step, nothing to project, and nothing that has to be kept in step with the content, because the content is what was committed. A segment file's diff is the records that were appended: the revision of these pages that corrected four figures shows up in git as 15 added lines and no deleted ones, the log being append-only.

#### What to exclude, and what to keep

```
# Track store/lock. It is zero bytes and only its existence is used,
# but a clone without it answers store-busy to every verb, reads
# included -- and init will not create it in a store that exists.

# A writer's drafts are that writer's own space, not part of the log.
store/writers/*/working/
store/writers/*/draft.lock

# The snapshot is a cached reduction of the log; it rebuilds itself.
store/snap/
```

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

> **What adopt does to a clone** instance.sexp binds a store to the machine and the directory it was made in, so the first write from a clone is refused rather than accepted into a second copy of the same writer's log. adopt mints a new writer id and leaves the history where it is: the clone keeps every block it was given, and its own records go under the new id from then on. Reading never needed any of this -- outline, read, search and eval answer from a fresh clone straight away. Every answer above came from a clone of this site; the outline is abridged and the titles passed to insert are left out, and nothing else is changed.

## 32 verbs

Every verb below, with its usage line and its one-sentence description, is rendered from what the store answers to describe. Nothing on this page is typed by hand, so it cannot drift from the binary that produced it. The tag on the right of each signature says where the verb runs: local in the client process, or daemon over the socket.

### Reading a store

```
read <id> [--md] [--recursive] [--writer <name>] [--working] [--working-info]
```

Read one block: its fields, or its text. (daemon)

```
refs <id>
```

List the relations a block takes part in. (daemon)

```
search <query>
```

Find blocks whose title, keywords or text match every word given. (daemon)

```
log [<id>]
```

Show the changes recorded, for the store or for one block. (daemon)

```
outline [--depth <n>] [--with-keywords]
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

### Making and changing blocks

```
init
```

Create a store in this directory. (local)

```
insert --under <id> [--after <id>] --title <text> [--text <text>] [--keywords <text>]
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
write <block> <bytes> [--writer <name>] [--based-on <version>] [--rebase]
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
export-md <dir> [--with-ids]
```

Write the store out as markdown. (daemon)

```
import-code <dir> [--allow-delete] [--datum]
```

Read a directory of source into the store. (daemon)

```
export-code <dir> [--raw] [--datum]
```

Write the store's source back out to a directory. (daemon)

```
split-suggest <file> [--output <review-file>]
```

Propose where a long file could be divided into blocks. (daemon)

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

## Working with agents

### The MCP shell

theourgia-mcp --store <path> speaks MCP over stdio. One tool per verb, named theourgia_<verb>, with a single argv array as arguments. The shell asks the core what verbs exist on every call, so a verb added to a newer build appears without restarting the server. Core refusals are successful tool results, not JSON-RPC errors: the agent reads the store's own words, not a transport-level failure. eval is not offered as a tool; init is not offered either, because the store must already exist before the MCP server starts.

### One writer per session

A writer id is held by one live agent at a time. A later session may bind the same id and carry on with its drafts, but two agents writing under one id at the same moment overwrite each other silently. There is no lock and no refusal: the rule is stated here, and the failure it prevents is silent. A host giving several agents the same actor must give each its own --writer.

### The memory recipe

To use a store as persistent memory for Claude Code sessions: turn off the built-in auto-memory (CLAUDE_CODE_DISABLE_AUTO_MEMORY=1 or in settings.json), register the store as an MCP server, and inject an outline at session start with a SessionStart hook that runs theourgia outline --depth 1. CLAUDE.md then carries the recall and write instructions: search to find, read to retrieve, insert to add (under the right section, with title, keywords, and text following the block protocol), and write+commit to update an existing block rather than duplicating it. The full recipe is in docs/claude-code-memory.md in the repository.

## Limits

Two agents on one writer id: silent overwrite, no detection. The rule is in the documentation; the code does not enforce it.

Two machines on one store: not supported. A store belongs to one machine's registry; moving it to another requires adopt.

Linux: the platform library compiles and the test suite passes. macOS and FreeBSD are the platforms with production readings; Linux's are pending.

Runners (--lang): the Scheme evaluator runs in a sandboxed child process with no filesystem and no network. A runner for another language (node, python3, sh) gets a projected copy of the store's code files in a temporary directory and the machine's own interpreter, so it has the reach of a local script. Runners are off by default (THEOURGIA_RUNNERS=on).

