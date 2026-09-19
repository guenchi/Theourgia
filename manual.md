# theourgia manual

The model a store holds, and every verb it answers to. This file is generated from the store it describes: the prose is its blocks and the reference is what `describe` answers, so neither can drift from the thing it documents.

## The model

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

The split between a code repository and a documentation store is a filesystem accident, not a property of the knowledge. A section of a design, a Scheme macro, a function in another language and a decision record are all blocks: each has its own id, its own history and its own edges, and each can be read without reading whatever it sits next to. import-code reads a directory of source into the store; with --datum it reads Scheme as data rather than as text.

#### A name is a block

In a Scheme library block this is literal rather than a metaphor. A definition is a block, and evaluating an expression against that library is evaluating the definitions those blocks hold -- so a name resolves to its definition block, log <id> gives that definition's history, and an explicit edge ties it to the design that motivated it. The same three questions about one name -- what it is, how it got that way, why it exists -- are three reads rather than three searches.

#### Other languages, more modestly

Source in other languages goes in as text blocks cut at top-level regions, with names extracted as well as the language table allows. They are blocks like any other: addressable, linkable, with their own history. What they do not get is the resolution above -- a name in Python source is not a binding the store can follow to a block.

### Backlinks cost one query

refs <id> answers what points at this block, from the two places a reference can live: an edge somebody wrote with link, and a mention in the text of another block. Each line says which of the two it came from, and they are never merged -- an edge can be removed with unlink, a sentence can only be edited. One small query in place of walking a repository and a documentation tree looking for the places that mention a thing.

```
$ theourgia link koou2buo.2 implements koou2buo.1
$ theourgia insert --under root --title "Mentions by id" \
    --text "See [[koou2buo.1]] for the reasoning."

$ theourgia refs koou2buo.1
(ok (items (ref (from "koou2buo.2") (rel implements) (via link))
           (ref (from "koou2buo.4") (rel ref) (via md))))
```

> **What refs resolves, exactly** Measured, because this is easy to over-claim. A mention resolves by id: a block whose text contains [[koou2buo.1]] shows up as (ref (from ...) (rel ref) (via md)). A mention by title does not -- a block written with [[The design]] did not appear in refs for the block titled The design. An explicit edge shows up as (ref (from ...) (rel implements) (via link)) under whatever relation name it was given. So renaming a block leaves explicit edges and id mentions intact, because both are by id; what does not survive a rename is a reference that was only ever a name.

### One query surface

Every verb answers with one line: an S-expression that begins with ok or error. --wire asks for the machine-facing spelling of it. There is no second, prettier output mode that an agent has to parse differently, and no rendering markup in the way -- the reply is the same data the store holds, and the agent can judge the size of what it is about to read before it reads it.

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
outline [--depth <n>] [--with-keywords]
```

List the blocks as a tree of titles. (daemon)

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
tag [<name>]
```

Name the current cut, or list the names already given. (daemon)

```
diff <cut> <cut>
```

Report what changed between two cuts. (daemon)

```
conflicts
```

List blocks whose writers disagree. (daemon)

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
publish <writer> <segment> <file> [<sha256>]
```

Publish a segment of a writer's log. (daemon)

```
batch <intents>
```

Carry out several changes as one request. (daemon)

