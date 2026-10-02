# Theourgia as an agent's memory

Two prompts, both meant to be copied as they are. The first migrates an
existing markdown memory into a store. The second makes Claude Code (or any
agent with a shell or MCP) use the store as its memory instead of markdown
files. Both assume the `theourgia` client is on `PATH` and a store exists
(`theourgia init` in the store directory, once). `theourgia init --template memory`
makes it with three roots, `lessons`, `decisions` and `references`, and the
agent's write tools then say where each kind of entry goes.

Status: first measured on 2026-09-18 against a real Claude Code memory of 158
files (about 430 KB), with one more measurement on 2026-09-28 (large imports,
section 1.1); every command here last checked against the code on 2026-10-02.
Treat the prompts as working drafts, not as a finished product surface.

---

## 1. Migrating an existing markdown memory

Give this to an agent together with the list of files to migrate. One agent
per group of files works, and several agents may insert into one store at the
same time. `THEOURGIA_WRITER` does not give each agent its own ids: it names
the agent's stream of DRAFTS (write, restore, discard, drafts), while
`insert` commits at once and every block it makes takes its id from the
store's own writer, created by `init` (one per copy of the store, whatever
the number of agents); each record carries the agent's `THEOURGIA_ACTOR`.

```
You are moving an existing markdown memory into a theourgia store through
its command-line client, following the store's write protocol. The store is
the target of everything you write; the markdown files are read-only source.

Environment (every shell):
  export THEOURGIA_ACTOR=<your-name> THEOURGIA_WRITER=<your-name>
  STORE=<path of the store>
  theourgia <verb> --store $STORE ... --wire
When THEOURGIA_HOME is set and not empty, the first stderr line is the
banner `(theourgia machine-home ...)`; ignore it.

Write protocol:
  - A block is the unit of writing: one block answers one question on its
    own. Keep a block under about 800 tokens (roughly 3000 bytes); split a
    longer one into blocks that each stand alone.
  - Give every block a one-line title and 3 to 8 keywords, comma separated,
    the words a future reader would actually type when searching.
  - Place a block under the parent its subject puts it under (--under).
  - Do not rewrite the source bytes: do not polish, translate or drop
    sentences. Frontmatter stays out of the body; its description may become
    the title, its name and type belong in the keywords.
  - A memory file is usually one block. A file holding several independent
    dated instances that together exceed the limit is split by instance,
    each block titled with the file's name and the instance's date; an
    instance whose heading has no date is titled with the heading's text.
    An instance larger than the limit stays one block: do not cut it in the
    middle of what it argues.

Per file, one insert, run from a script as an argument list -- never through
a shell, which would change a body holding a backtick or a $:
  ["theourgia", "insert", "--store", STORE, "--under", "<parent-id>",
   "--title", "<title>", "--keywords", "<k1>, <k2>, ...",
   "--text", "<body verbatim>", "--wire"]
The body is one element of that list, whatever its size (a 9 KB body has
gone through this way). A success answers
  (ok (events ...) (state (("<id>" . "<hash>"))) ...)
with the id and the hash as strings; record <id> without the quotes. Record
any other answer verbatim, with the command minus the body; retry at most
once.

Ledger (write it to <ledger path> as JSON):
  {"group": ..., "files": [{"file": ..., "chars": n,
     "blocks": [{"id": ..., "title": ..., "keywords": ..., "bytes": n}],
     "refusals": [{"cmd": ..., "answer": ...}]}],
   "totals": {"files": n, "blocks": n, "refusals": n, "wall_seconds": n},
   "friction": ["one sentence per thing that was awkward: command line,
                 answers, protocol, speed"]}
Every id in the ledger comes from a real answer; never write a placeholder.

Do not modify any source file. Do not touch git. Do not kill processes. Do
nothing to the store except insert under the parents you were given. If the
client fails more than three times in a row, stop, record, report.
```

Give each agent its parent block ids up front: in a store made with
`init --template memory`, the ids of the roots `lessons`, `decisions` and
`references`, which `init` names in its answer's `(created ...)`; in a store
without a template, create the top-level sections yourself with
`insert --under root`. Give it also its own writer name and its own ledger
path.

### 1.1 Large directories go in file by file, not by one `import-md`

`import-md <dir>` reads a whole directory of Markdown in one request. For a
memory of any size, use the per-file prompt above instead. Measured in the
night of 2026-09-28: `import-md` of a 6.2 MB directory of 856 files takes the daemon
longer than the command-line client waits for an answer (30 seconds), so the
client answers

    (error transport-unknown (reason 35))

while the daemon goes on and completes the import. The reason is the error
number of the socket read that ran out the client's 30-second wait: EAGAIN,
which is 35 on macOS; another platform prints its own number. That answer
means "the outcome is unknown", not "nothing happened": do not retry the
import on it.
Look at what arrived (`outline --depth 1`, `search`) before doing anything
else. The per-file prompt keeps every request small, gives an id per file to
the ledger, and lets several agents share the work.

### 1.2 Editing through Markdown files, and re-importing

A store can also be edited as files: `mkdir -p <dir>` (export-md writes into
a directory that exists and does not create one), `export-md <dir>
--with-ids`, edit the files, `import-md <dir>`. `--with-ids` writes each
section's id before its heading, and a document is found again by its path,
so the import lands on the same blocks. What the import does with what is
missing:

- A file that is no longer in the directory leaves its blocks alone; the
  answer names it, `(absent (files (<path> ...)))`. Deleting those documents
  takes `--allow-delete`.
- A section that has disappeared from a file that is still there is refused
  as `would-delete` until `--allow-delete` is given. The refusal names, under
  `(holds (<id> ...))`, the blocks under that section the file does not
  describe; they are moved up to the nearest section that stays before it is
  deleted.
- A heading edited in the file is a new section, and the one it replaced is
  missing (so `would-delete`). To rename a section and keep its id, `set <id>
  title "<new title>"` in the store, or edit a file exported `--with-ids`,
  where the heading's marker keeps the match.

`export-md <dir> --working --writer <name>`, into a directory made the same
way, writes one writer's working view instead of the committed store: its
drafts over the store at the cut they were written on, so a commit made after
them is not in it.

---

## 2. Using theourgia as Claude Code's memory

Three pieces: turn the markdown memory off, inject the store's outline at
session start, and tell the agent how to recall and how to write. Two more
are optional: the MCP route (2.4) and keeping the store in git (2.5).

### 2.1 Turn auto-memory off

Either of these (documented by Claude Code):

```
CLAUDE_CODE_DISABLE_AUTO_MEMORY=1
```

or in `settings.json`:

```json
{ "autoMemoryEnabled": false }
```

Without this the agent still gets the first 200 lines / 25 KB of `MEMORY.md`
at every session start and keeps writing markdown files.

### 2.2 Inject the outline at session start

A `SessionStart` hook runs a command and puts its stdout into the agent's
context. Point it at the store:

```json
{
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup",
        "hooks": [
          {
            "type": "command",
            "command": "theourgia outline --store /path/to/memory-store --depth 1"
          }
        ]
      }
    ]
  }
}
```

Add a second entry with `"matcher": "compact"` if the outline should return
after context compaction (`SessionStart` also matches `resume`, `clear` and
`fork`). `--depth 1` gives the top-level sections only; the agent goes deeper
with `search` and `read` when it needs to.

To open every session with the decisions still owed, add a second command to
the same `startup` entry:

```json
{
  "type": "command",
  "command": "theourgia commitments --open --store /path/to/memory-store"
}
```

This page gives the line; installing the hook is done in your own settings.

### 2.3 Tell the agent how to use it (CLAUDE.md)

Copy this into the project's `CLAUDE.md`:

```
## Memory lives in theourgia, not in markdown files

The store at /path/to/memory-store is my memory. The markdown memory
directory is retired: do not read it and do not write to it.

Identity: export THEOURGIA_ACTOR=<agent name> and THEOURGIA_WRITER=<agent
name> before the first call. One writer id is held by one live agent at a
time; a later session may bind the same id and carry on with its drafts.
Two agents that need to edit the same draft copy it instead of sharing the
id, keeping its baseline:
  theourgia read --store $STORE <id> --working-info --writer w1
  theourgia write --store $STORE <id> "<text>" --writer w2 \
    --based-on <based-on> --working-cut <cut>
The read answers (projection working w1 <id> <version> <based-on> <cut>
"<text>" ...); w2 must not hold a draft of that block already.

Recall, before starting a task:
  theourgia search --store $STORE "<two or three words from the task>"
  theourgia read --store $STORE <id>              # a hit that looks relevant
  theourgia read --store $STORE <id> --recursive  # a block and all under it
search takes one query: quote the words, or it answers with its usage line.
The first read ends with (cut ...), the state it read, and (versions ...),
the block's version; read --recursive prints its blocks alone. Recall needs
neither clause.
Read what the search returns; do not page through the whole store. If a
search finds nothing, try the other words a past self would have used, then
stop: absence of a memory is an answer.

Write, when something worth keeping happens (a correction, a decision, a
fact about the environment, a lesson):
  theourgia insert --store $STORE --under <section id> --title "<one line>" \
    --keywords "<3-8 words a future reader would search for>" \
    --text "<the fact, then Why: and How to apply:>"
One block answers one question; keep it under about 800 tokens. Put it under
the section it belongs to (the outline shows them). Before inserting, search
for an existing block on the same fact and change that one instead of adding
a duplicate: its text with a draft and a commit,
  theourgia write --store $STORE <id> "<new text>"
  theourgia commit --store $STORE <id>
(the draft is based on the block as committed now), its title or keywords
with
  theourgia set --store $STORE <id> keywords "<words>"
Keep the markdown habits that still apply: say why, say how to apply, link
related blocks by id in the text.

Do not put into the store what the repository already records (code
structure, git history, CLAUDE.md itself), and nothing that only matters to
the current conversation.
```

### 2.4 The MCP route instead of the shell

If the agent host speaks MCP rather than a shell, register the shell once.
The shell takes the store, and optionally the actor, as arguments. The host
configures nothing per session: each session gets its own writer id, so two
sessions, or two agents with the same actor name, do not share a draft
stream unless the host gives them one fixed name (within one machine's
process ids; see the limit in `mcp/README.md`):

```
claude mcp add theourgia -- \
  theourgia-mcp --store /path/to/memory-store --actor <agent name>
```

or, in an MCP configuration file:

```json
{
  "mcpServers": {
    "theourgia": {
      "command": "theourgia-mcp",
      "args": ["--store", "/path/to/memory-store", "--actor", "<agent name>"]
    }
  }
}
```

The writer id is decided once, when the shell starts: `THEOURGIA_WRITER` if
the host sets it (and it is not empty); otherwise `--writer <name>`;
otherwise one derived for the session, `<actor>-<start>-<pid>` (the actor
reduced to what a writer id may hold, the start instant in milliseconds in
base 36, the shell's process id). The `initialize` answer's instructions end
with a sentence naming it. A host sets a writer only when it wants a fixed
name. The drafts of an ended session stay under its writer: read them with
`drafts --writer <that writer>`, or take them over by starting the next
session with `THEOURGIA_WRITER=<that writer>` (or `--writer`). A name a
writer cannot have is refused when the shell starts, with its usage line and
exit 2. The details are in `mcp/README.md`, "Whose drafts".

The store must exist first (`theourgia init`, by hand): the shell has no
local route and does not offer `init` as a tool. The tool list and the write
protocol then come from the store itself (`describe`); the CLAUDE.md text
above stays the same with `theourgia_<verb>` tools, each taking
`{"argv": [...]}`, in place of the commands, except its Identity line: over
MCP the shell sets the writer, and the instructions say which. A search query
is one string in that list: `{"argv": ["two or three words"]}`.

`eval` is a tool too, `theourgia_eval`: the shell runs it on the store's
machine as its own child, with the same argv the command line would take, and
the writer of `eval --working` is the session's writer unless the call gives
`--writer`. A language other than
Scheme runs only if the host started the shell with `THEOURGIA_RUNNERS=on`.
An evaluation that could not be seen through -- killed at its deadline, or
ending without an answer -- is a protocol error that says execution may be
unknown and carries the reason (`mcp/README.md`, "Eval").

Every answer arrives as a tool result. A refusal by the store --
`(error unknown-id ...)`, say -- is the answer to the question asked: a
result with `isError: false` whose text begins `(error`, so read the text,
not the flag. A store that could not start, or a daemon that declined the
request, is a result with `isError: true` and the refusal in
`_meta.refusal`. Only the shell's own failures are protocol errors: a frame
it could not read or that was too large, a tool catalogue it could not read
(the tool was then not sent), and a daemon that took the request and lost
the answer.

### 2.5 Keeping the memory store in git

A store can live in a git repository for backup and for moving it between
machines, with one writer. `init` writes `<store>/.gitignore`, which keeps
out what belongs to this copy rather than to its history (its identity, its
checkpoints, its drafts). Add the store as a whole, never named files:

    git add <store>

A clone or checkout reads at once, and its first write is refused:

    (error refused no-instance (remedy adopt))

Run `theourgia adopt --store <store>` once in the checkout; writes land from
then on. The README's "A store in git" section has the details.
