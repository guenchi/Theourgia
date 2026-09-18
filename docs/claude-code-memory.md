# theourgia as an agent's memory

Two prompts, both meant to be copied as they are. The first migrates an
existing markdown memory into a store. The second makes Claude Code (or any
agent with a shell or MCP) use the store as its memory instead of markdown
files. Both assume the `theourgia` client is on `PATH` and a store exists
(`theourgia init` in the store directory, once).

Status: measured on 2026-09-18 against a real Claude Code memory of 158 files
(about 430 KB); the numbers are in the project's archive. Treat the prompts as
working drafts, not as a finished product surface.

---

## 1. Migrating an existing markdown memory

Give this to an agent together with the list of files to migrate. One agent
per group of files works; several agents may write into one store at the same
time, each under its own writer id.

```
You are moving an existing markdown memory into a theourgia store through
its command-line client, following the store's write protocol. The store is
the target of everything you write; the markdown files are read-only source.

Environment (every shell):
  export THEOURGIA_ACTOR=<your-name> THEOURGIA_WRITER=<your-name>
  STORE=<path of the store>
  theourgia <verb> --store $STORE ... --wire
The first stderr line `(theourgia machine-home ...)` is a banner; ignore it.

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
    each block titled with the file's name.

Per file:
  theourgia insert --store $STORE --under <parent-id> --title "<title>" \
    --keywords "<k1>, <k2>, ..." --text "<body verbatim>" --wire
A success answers (ok (events ...) (state ((<id> . <hash>))) ...); record
<id>. Record any other answer verbatim, with the command minus the body;
retry at most once.

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

Give each agent its parent block ids up front (create the top-level sections
yourself with `insert --under root`), its own writer name, and its own ledger
path.

---

## 2. Using theourgia as Claude Code's memory

Three pieces: turn the markdown memory off, inject the store's outline at
session start, and tell the agent how to recall and how to write.

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
after context compaction. `--depth 1` gives the top-level sections only; the
agent goes deeper with `search` and `read` when it needs to.

### 2.3 Tell the agent how to use it (CLAUDE.md)

Copy this into the project's `CLAUDE.md`:

```
## Memory lives in theourgia, not in markdown files

The store at /path/to/memory-store is my memory. The markdown memory
directory is retired: do not read it and do not write to it.

Identity: export THEOURGIA_ACTOR=<agent name> and THEOURGIA_WRITER=<agent
name> before the first call. One writer id is held by one live agent at a
time; a later session may bind the same id and carry on with its drafts.
Two agents that need to edit the same draft copy it (drafts --writer w1,
restore <version> --writer w2) instead of sharing the id.

Recall, before starting a task:
  theourgia search --store $STORE <two or three words from the task>
  theourgia read --store $STORE <id>          # the hits that look relevant
  theourgia outline --store $STORE <id>       # to see what sits under a block
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
for an existing block on the same fact and update that one with `write`
followed by `commit --based-on` rather than adding a duplicate. Keep the
markdown habits that still apply: say why, say how to apply, link related
blocks by id in the text.

Do not put into the store what the repository already records (code
structure, git history, CLAUDE.md itself), and nothing that only matters to
the current conversation.
```

### 2.4 The MCP route instead of the shell

If the agent host speaks MCP rather than a shell, register the shell once:

```
claude mcp add theourgia -- theourgia-mcp --store /path/to/memory-store \
  --actor <agent name> --writer <agent name>
```

The tool list and the write protocol then come from the store itself
(`describe`); the CLAUDE.md text above stays the same with `theourgia_<verb>`
tools in place of the commands. A host that gives several agents the same
actor name must still give each its own `--writer`.
