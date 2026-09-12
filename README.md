# theourgia

Agent-and-human shared block-graph knowledge base. R6RS Scheme; Chez Scheme is the reference platform, and the portable core is meant to compile to wasm and to move to other implementations. Platform-specific code (file primitives, locks) lives in its own library.

Licensed under the Apache License, Version 2.0. See LICENSE.

Design documents are maintained separately; this repository holds the implementation and its tests.

## Reading a store

Every verb prints one S-expression per item on stdout; the exit code is the verdict.
**Empty output with a zero code is an answer** — no references, no hits, no
differences — and is never an error.

### `read <id> [--md] [--recursive]`

    (ok (<block>))        |  (ok (items <block> ...))  |  (ok (text "<markdown>"))

What a block says. Without options it is one block as data; `--md` gives its own
bytes — the heading line it was written with, then its body.

A file-level block holds almost nothing of its own: its body is the front matter
and whatever sits above the first heading, which in most documents is nothing at all.
`--recursive` asks for the subtree instead — the block and every block under it **up
to a nested document**, which has its own file and is read on its own, in
document order. With `--md` as well, that is the document: for a **top-level**
document, the same text `export-md` writes to the file, from the same renderer, so a
read and a round trip cannot disagree.

The qualifier is real. A document nested under another document is a shape the
write path refuses to create — `insert` answers `(error doc-must-be-top-level
(parent <id>))` — but one can arrive by import or from another machine. When one
does, `export-md` gives it **its own file**, named by its `path`, and stops the
ancestor's walk there so its sections are written once rather than twice;
`conflicts` reports it under `nested-documents`. Read recursively it is still a
document and keeps its front matter. What is still open is what a nested
document *means*, not what the renderer does with it: until that is
settled, only top-level documents round-trip.

A section asked for on its own does not carry the document's front matter, which
belongs to the document. Its body carries the blank line that separated it from
whatever followed in the file, so a subtree lifted out of the middle of a document
ends with one — put it back where it came from and the file is reproduced exactly.

Neither option takes a value and both are stripped before the id is read, so the two
orders are the same request.

### `refs <id>`

    (ref (from <id>) (rel <rel>) (via link|md))

What points **at** this block, from the two places a reference can live. A `link`
record is an edge someone wrote and `unlink` removes it; a reference in the text is a
sentence, and nothing removes it but editing the sentence. Each line says which it
came from, and they are never merged: you can unlink an edge, you cannot unlink a
sentence. Text references are derived at read time rather than stored as edges,
because materialising them would give one fact two suppliers that can then disagree.

A block's own out-edges are not references to it; those are part of what `read`
returns about the block.

### `search <query>`

    (hit <id> <score> "<snippet>")

Whitespace splits the query into tokens and **every** token must hit. A hit is a
case-insensitive substring, so `cat` finds `concatenate`. Title is worth 2 and src 1,
counted once each however many times they match, so a block hit in both scores 3.
Order is score descending then id ascending — total, so two runs over one store agree.
The query is only ever text: nothing in it reaches a numeric parser.

### `log [<id>]`

    (entry (event <writer> <seq>) (ts <ms>) (actor "<name>") (verb <verb>))

What this store has applied, in the order it was delivered. Given an id, only the
records that **named** that block: a record whose premises mention it has not touched
it, and neither has one whose text mentions it.

This verb replays the whole log rather than reading from the snapshot, and is
therefore the slow one. Two facts force it: the timestamp and actor exist only as a
record is delivered, and a snapshot-seeded read never delivers what the snapshot
already covers — which would produce a log that silently began in the middle.

### `tag [<name>]`

    (tag (name "<name>") (cut ((<writer> . <seq>) ...)))

With a name, binds it to the cut this store has **applied** — not the frontier it has
discovered, which would name records no reader of this store could produce. The cut is
taken before the record exists, so a tag is never inside the cut it binds, and an edit
written afterwards does not move it.

Without a name, lists what is bound. Writing a name again supersedes the earlier tag
**causally**: a session that had seen the first wins, while two written concurrently
both stand and are reported as unsettled with their event ids rather than with a
winner chosen here. Both records remain in the log either way; a tag is not a
uniqueness constraint.

### `diff <cut> <cut>`

    (added <id>) | (removed <id>) | (changed <id> <field>)

Each side is a tag name or a cut written out as `(("writer" . 12) ...)`. Two states
are built by reading to each cut — not the current state with something subtracted,
because the only way to know what the store said at a moment is to read it then.

The projection is title, src, level, parent, ord and the edge set: a tombstone is
`removed`, and an edge-set change is `(changed <id> links)`. Comparing only the scalar
fields would call two states identical when an edge had moved.

A cut literal is parsed by shape and never handed to `read` — the reader implements
the whole numeric syntax, and `#e` with a large exponent asks it to build an integer
of any size from eleven characters of argument. A cut this store cannot reach is
refused as `cut-unavailable` rather than truncated to the part it can reach.

### `conflicts`

    (conflict <id> cycle|unplaced) | (orphan <id>) | (pending (event <w> <seq>) (missing <w> <seq>))

What the store holds and cannot show: blocks in a structural conflict, blocks whose
parent was deleted or never arrived, and records still waiting for premises. A record
waiting on several premises is listed once for **each** — stopping at the first would
send an operator to fetch one record and leave them where they started. A section with
nothing in it prints nothing.

The structural conflicts are read from the same place `outline` reads them, so the two
cannot disagree; `outline` prints the mark as a fourth column on that row.

## Requests and replay

The block-editing verbs — `insert`, `set`, `move`, `del`, `link`, `unlink` —
accept a request identity. A client that did not hear the answer can send the
same request again and be told what happened, rather than having to choose
between doing the work twice and not doing it at all.

`batch` carries one too, and is covered below. So does `tag <name>`, which
writes a record — but not `tag` with no argument, which only lists what is
there. Trackability is a property of the **request**, not of the verb's name.

**Every other request refuses the options** — `(error bad-request
req-not-tracked <verb>)`. The store-level verbs and the read-only ones have no
replay to offer, and taking an identity only to drop it would leave a caller
believing it was protected. Refusing says so.

    theourgia insert --title "One" --req <id> --cursor <writer>:<seq>

**Both options or neither.** `--req` alone is `(error bad-request
req-without-cursor)` and `--cursor` alone is `(error bad-request
cursor-without-req)`. Neither is accepted quietly, because a caller who supplied
one believes it is protected and is not: the id names the request and the cursor
says where the store stood when the client decided to send it, and the store
needs both to say anything about a request it cannot find.

A write with neither option behaves exactly as it did before and is not tracked.

**The cursor is `--cursor <writer>:<sequence>`** and is parsed by shape.
Anything else — no colon, an empty or non-numeric sequence — is `(error
bad-request malformed-cursor)`. It is spelled `--cursor` and not `--after`
because `--after` already names the sibling a new block is placed behind, on
`insert` and `move`; one spelling cannot carry both, because the dispatcher
must consume the retry cursor before any verb sees its arguments. An id outside the permitted format is `(error bad-request
malformed-req-id)`.

### What a request is answered with

These are the answer **categories**; each carries further evidence, shown here
abbreviated as `...`. An RPC `batch` answer is wrapped once more, as
`(batch (<answer> ...))`.

| Answer | Meaning |
|---|---|
| `(ok ...)` | the request had not run; it ran now. |
| `(ok ... (replay #t) (event (<writer> . <seq>)))` | this exact request has already run. No new record was appended. The event names the record the store is standing behind. |
| `(error req-mismatch ...)` | that id already names a **different** request. No record was appended. |
| `(error cursor-unreachable (after (<writer> . <seq>)) (writing ...))` | the cursor names a position the record could not have been written at, so the store cannot reason about where the request would have landed. |
| `(error resolved-executed ...)` | an operator has recorded a determination for this request, and it says the work was done. |
| `(error unknown <why>)` | the store cannot tell whether the request ran. It never guesses. |

`unknown` always says why, and the reason is something an operator can act on.
They fall into three families, and this is not the whole list — the store adds a
reason wherever it finds a new way to be unsure:

- **history in doubt** — `(range-overlaps (<writer> <from> <to>))`: a stretch the
  request could have occupied cannot be read, so the store cannot say whether the
  request is in it.
- **metadata unreadable** — `(uncertain-cache unreadable)`,
  `(retirement-unreadable <writer>)`, `(chain-unreadable)`: something the answer
  depends on could not be read.
- **evidence that does not add up**, for a batch — `(receipt-absent)`,
  `(receipt-coverage <i>)`, `(receipt-unlinked <event>)`,
  `(item-index-missing <event>)`, `(plan-order <indices>)`,
  `(evidence-names-missing <i>)`: records were found that no correct execution
  produces, so the store will not guess which story it is looking at.

A store that answered `ok` to any of these would be guessing, and the whole point
of holding a request id is to not have to.

The question is asked of the **successor chain**, not of one writer. A cursor on
a writer that has since been retired is measured against the generations that
succeeded it too, because a request written against a retired cursor could have
landed on any of them.

### An answer is durable when it is given

A request answered `ok` has already been made durable. The cost is **per
request, not per record**: a request of nine sub-operations is flushed the same
number of times as a request of five. A single-intent request runs one barrier;
a tracked batch runs two, because its receipt is made durable before its items
are written.

A replay performs the barrier again before repeating the word, because saying
"that already happened" is the same promise as saying it happened the first
time.

So a crash cannot leave a client holding an `ok` for a record that is not on
disk. It can leave a client holding **no** answer, which is what `--req` is for.

### `batch`

Intents are read from standard input, one wrapping list or several top-level
forms:

    echo '((insert root #f ((kind . section) (title . "One")))
           (insert (from 0) #f ((kind . section) (title . "Two"))))' \
      | theourgia batch

A field is a **pair**: `(title . "One")`. Written `(title "One")` the value is
the list `("One")`, which is a different thing and is stored as one.

An intent is the shape the library takes, not a shorthand, and each verb has
its own:

    (insert <parent> <predecessor> <fields>)
    (set    <id> <field> [<value>])          field is a symbol
    (move   <id> <parent> <predecessor>)
    (link   <from> <rel> <to>)               rel is a symbol
    (unlink <from> <rel> <to>)
    (del    <id>)
    (tag    <name>)

Any intent with a subject — `set`, `move`, `del`, `link`, `unlink` — may be
wrapped as `(expect <hash> <intent>)`, which refuses the work if that block's
hash is no longer the one given. `insert` and `tag` have no subject and answer
`(error no-subject)` if wrapped. The hash must be a string: a wrapper holding
anything else is `malformed-intent`, not a wrapper that quietly does nothing.

An item the store will not accept is answered `(error malformed-intent
(<reason> <what-was-sent>))` for that item — never raised. The reason names the
rule, because more than one can apply to the same item and they do not send you
to the same place:

| Reason | |
|---|---|
| `intent-not-a-form`, `verb-not-a-symbol` | it is not a form headed by a verb |
| `too-few-arguments` | shorter than that verb reads |
| `not-an-id` | a value that could not be an id where one belongs |
| `name-not-a-symbol` | a field or relation name that is not a symbol |
| `fields-not-a-list`, `field-entry-not-a-pair`, `field-name-not-a-symbol` | the field collection's shape |
| `field-name-reserved` | `parent` or `ord`, which the store computes |
| `field-name-repeated` | the same field name twice |
| `intent-not-a-proper-list` | the item, or its `expect` wrapper, is not a proper list |
| `expect-too-short`, `expectation-not-a-hash` | the `expect` wrapper |
| `title-has-line-terminator`, `title-has-control-character` | a title must be one line |
| `level-not-a-heading-level` | a `level` that is not 1–6 |

A record the store was going to write but cannot apply is refused the same way,
with the reason from the reducer's own vocabulary — `id-not-a-string`,
`tag-name-not-a-string`, `relation-not-a-symbol`, `parent-not-an-id`,
`ord-not-a-number` — and what is shown is the payload rather than the item.

A form headed by a symbol this build does not know is a different answer,
`(error unknown-verb <verb>)`: it may be a record shape a newer build writes,
and calling it malformed would send an operator to repair something that only
needs a newer binary.

Note that the empty text is a batch of **no** intents, which writes nothing and
is not an error, while the text `()` is a batch of **one** empty intent, which
is malformed. They are different requests and are answered differently.

A **tracked** batch of more than one item — one carrying `--req` and `--cursor` —
writes a **receipt** first and then its items, and each item is its own request
under an identity derived from the batch's, so a retry can be told not merely
how far the batch got but exactly which items are on disk. Sending a completed
one again appends no record and answers `(ok ... (replay #t) (event (<writer>
. <seq>)))` naming its **last** item: that is the furthest point the promise has
to reach.

The receipt records, for each item, the cursor that item was written against.

A tracked request of a **single** intent has no receipt — it takes the
single-record path — but it is still tracked, and repeating it is still a
replay. A tracked request that produces **no** intent appends nothing at all.

An **untracked** batch has neither a receipt nor replay protection; repeating
its text does the work again.

What the store can **conclude** about a batch it has partial evidence of. These
are the classifier's verdicts about what is on disk; nothing resumes a `planned`
or `partial` batch by itself — that is for the client holding the request:

| State | Meaning |
|---|---|
| not committed | the batch provably did not begin. It is safe to execute. |
| planned | the receipt is there and no item is. |
| partial | a prefix of the items is on disk and the rest provably did not run. |
| committed | every item is on disk. |
| unknown | one of the above cannot be established. |

`partial` is the strongest of these and the store will not claim it lightly: every
missing item must be clear of every stretch of history in doubt, the writer chain
must read to its end, and nothing anywhere — including records set aside as
superseded — may mention an item said to be missing. Failing any of those the
answer is `unknown` with the reason, never `partial`.

### What the machine registry records

Two frontiers per `(store-id, instance, writer)`, and they are not the same
fact: how far this store has been given leave to write, and how far its records
have actually reached the disk. The second is what refuses a store whose history
has gone backwards — restored from a backup, say — while the first lets a request
reserve the room it might need before it knows how much of it it will use.

The second can lag behind records that really are durable: a crash between a
record's flush and the registry's update leaves it low. A replay of such a record
raises it, which is why answering a replay is a write even though it appends
nothing.

### Two limits worth knowing

**A retirement record written before this version says less than one written
after it.** From this version a retirement carries the stretches the adopt could
not vouch for, and carries them even when there are none — so an absent clause
means "written by an older build" rather than "nothing was lost". Nothing on disk
can now say which of those an older record meant, so the older ones are read the
only safe way: everything above the retired prefix is in doubt, without an upper
bound.

The cost is real and it is one-directional. A request whose cursor falls in such
a stretch is answered `unknown` every time it is sent, and an operator's
`not-executed` resolution cannot release it, because a resolution may only exempt
a stretch with a top and this one has none. It does not block a replay when
matching evidence exists, and it does not affect a request on the writer that
succeeded the retired one. Stores created from this version forward never produce
the older form.

**A caller that captures a continuation inside its own procedure and re-enters it
after the write has finished will end the session twice.** The second end raises
and is suppressed, so no answer is lost, but the store's lock accounting is not
written to survive that shape. Do not resume a captured continuation out of a
`with-store-write` procedure.

## Sync: the `publish` verb

    theourgia publish <writer> <segment> <file> [<sha256>]

A store receives another store's history one segment at a time. `publish` hands the
bytes of one segment to the log layer and prints what it decided, as a single
S-expression on stdout; the exit code is zero only for the four outcomes that leave
the segment published.

The hash is the **sender's declaration**. Given one, it is checked against the bytes
before anything else happens, so a segment that was damaged in transit is refused
rather than installed and discovered later by a reader. Given none, the bytes on
disk are taken as their own: computing the hash here from the same bytes would
compare a number with itself.

`publish` **waits** for the store lock rather than failing. Another process
publishing, or a reader holding the shared lock, blocks this call until the store is
free. A client told "busy" would have to re-send to learn whether its bytes were
refused or merely queued, and those are different facts about its bytes.

### What it answers

The decision is the log layer's, not the command's. Four outcomes leave the segment
published and exit zero:

| answer | meaning |
|---|---|
| `(published <n>)` | the candidate is installed and listed in the manifest. |
| `(idempotent <n>)` | these exact bytes are already installed **and** already listed with this hash; there is nothing left to do. |
| `(repaired <n> (evidence <path>))` | the local copy failed validation, the candidate covers every valid record it still held, and it has been replaced. |
| `(extended <n> (evidence <path>))` | the segment holds a retired prefix and the candidate continues it; the retirement coordinates were re-checked against the candidate first. |

Both replacements name the file the displaced bytes were copied to. The sender is the
one party that may want them, and deriving that name on its side would be a second
supplier of it — the two would part company the first time the naming rule changed.

The rest refuse, and exit non-zero:

| answer | meaning |
|---|---|
| `(incomplete <n>)` | the candidate is a prefix of what is already here; accepting it would discard records. |
| `newer-history-unmergeable` | the candidate is longer than a sealed segment, which is not a segment this store may replace. |
| `(divergence (fork <seq>))` | a record disagrees with one this writer's history already holds; the fork is recorded at the lowest such sequence and never moves up. |
| `(refused insufficient-coverage (seq <n>))` | the local copy is damaged, but the candidate does not cover record `<n>`, which is still valid here. Absence is not evidence of disagreement. |
| `(refused active-writer-segment)` | the segment is this store's own active writer's current segment. This is checked above every other rule. |
| `(refused retirement-coordinates <detail>)` | the candidate would move the byte offset at which the retired prefix's last sequence ends, so the declaration would silently stop being true. |
| `(error invalid-candidate sha-mismatch)` | the bytes do not hash to the declared value. |
| `(error invalid-candidate not-contiguous)` | the candidate's own sequences jump, repeat or go backwards. |
| `(segment-layout-conflict <reason>)` | the candidate cannot sit at this segment number. The reasons are below. |

### Where a candidate may sit

A writer's segments are read in ascending number and their records concatenated; a
reader stops at the first sequence that does not continue the last. So a candidate
that is perfectly formed, and that agrees with everything this store holds, can
still be unpublishable because of **where** it would sit. Each reason names the two
numbers that disagree, because each asks for a different repair:

| reason | meaning |
|---|---|
| `(gap-before-candidate (history-ends <a>) (candidate-starts <b>))` | the candidate begins past the end of the declared history below it, leaving the sequences between them owned by nobody. Something in between has not arrived yet. |
| `(overlaps-preceding (history-ends <a>) (candidate-starts <b>))` | the candidate begins at or before that end, repeating sequences a reader has already passed. The sender and this store disagree about where this segment starts. |
| `would-not-read-through` | the segment above this one does not begin where the candidate ends, so installing it would hide everything beyond. This candidate is wrong for this segment number. |
| `target-occupied` | the segment number already holds records that share no sequence with the candidate. |
| `not-start-aligned` | the candidate overlaps what is here but starts elsewhere, so the two cannot be compared as prefixes at all. |
| `empty-candidate` | the candidate holds no valid record. |

**Segment numbers need not be dense; sequences must be.** A reader walks segments in
number order and records in sequence order, so a gap between segment *numbers* costs
nothing — there is no record in it to miss — while a gap between *sequences* is
history nobody holds. A candidate that continues the sequence may therefore take any
free segment number, which is what lets a publisher choose one without knowing which
numbers its peer has already used.

**Layout coordinates come from what the store declares, never from whichever files
happen to be in the directory.** Two declarations exist: the manifest, which lists
the segments this writer has published and the range each holds, and the retirement
record, which names the sequence a retained prefix ends at. A file that neither of
them names is an orphan — bytes that arrived from somewhere and are not this
writer's history — and contributes nothing, so a writer with nothing declared below
a segment ends at 0 and a candidate for its first segment must begin at 1. A killed
install, whose segment was written but never listed, is exactly such an orphan.

Where history ends is the end of the **traversable** declared prefix, not the highest
declared sequence. The two differ only when the declarations already hold a hole, and
there the difference decides whether the hole can ever be repaired: segment numbers
only increase, so refusing the one candidate that could fill a gap refuses it
permanently.

### `published.sexp`

The manifest lists what a writer has published:

    ((<segment number> "<hex sha256>" <first seq> <last seq>) ...)

ascending by segment number, written whole. The entry carries the range because
layout decisions are made from declarations: a listed segment whose bytes are damaged
is history of a known extent awaiting repair, and reading its extent from the bytes
instead would make it unknowable exactly when it is needed — an incoming segment's
fate would then depend on damage elsewhere, and on the order in which things arrived.

There is one shape. An entry of two elements is not an older manifest to be read
leniently; it is a manifest this build does not understand, and it is reported as an
integrity error rather than accepted.

A missing manifest is not an empty one: absent means this writer has no manifest at
all, which is the ordinary state of a local writer, while `()` means a manifest
exists and lists nothing.

On opening, a listed segment whose bytes do not hash to the declared value is
excluded from the writer's history, and so is one whose declared range disagrees with
the range its bytes hold — with the hash matching, a disagreeing range is the manifest
contradicting itself, and in both cases the store cannot say what the segment is.

### `incoming/`

A refused candidate is **kept, not installed**. Its bytes are written under
`writers/<writer>/incoming/`, named by their own hash, with an empty marker file
beside them recording that the bytes passed the checks that are properties of the
bytes alone — per-record CRC and internal sequence continuity. Nothing is written at
the segment's own name and nothing is added to the manifest.

The name is content-addressed so that the same candidate arriving again is recognised
without being re-read. The decision itself is **not** cached: the splice, the
divergence check and the publish decision are redone on every call, against the
history as it is at that moment. A gap refused today becomes publishable once the
missing middle arrives, and the kept bytes are then accepted unchanged.

### The recovery barrier

Four answers leave the segment published, and a sender deletes its own copy on the
strength of any of them. So all four have to promise the same thing, and they promise
it by going through one table:

| row | the file |
|---|---|
| `log-file` | the segment itself |
| `writer-directory` | the entry naming it |
| `manifest` | `published.sexp`, which admits the segment to the writer's history |
| `owner` | each generation's `owner.sexp` |
| `retired` | each generation's `retired.sexp` |
| `instance` | the store's `instance.sexp` |
| `uncertain` | the writer's `uncertain.sexp` |
| `registry` | `instances.sexp`, outside the store |

The order is documentation, not a protocol. An fsync establishes that an operation has
completed; it does not hold other writes back, so these names reach the disk in
whatever order the filesystem chooses, and two of the rows share a directory in any
case. Recovery ordering is established where the mutations happen — the candidate is
durable before it is linked, and its directory entry before the manifest names it. The
barrier is the completion point that reasserts those obligations before the answer is
given, not a second attempt to sequence them.

Read outwards from the record, each row is a premise of the one before it: a record
whose file is durable but whose directory entry is not is a record no reader will
find, and a segment whose manifest entry is lost is a file this store will treat as an
orphan. Every file row is flushed and then has the directory entry naming it made
durable; the directory row is fsynced as a directory.

`idempotent` is the **replay** of a publish, and it is the reason the table is one
table. A replay writes nothing — the bytes and the manifest entry are already exactly
right — so if it made its promise from a second list, the two would agree until one of
them changed, and the disagreement would surface only as a replay that promised more
than the original execution had. The barrier runs in one place, on any of the four
answers, so a branch added later is covered because it returns one of the four and not
because whoever wrote it remembered.

A refusal does not pass the barrier. Nothing it describes is claimed to survive a
crash, and flushing the store's whole recovery closure on the way out of every
rejected candidate would charge refusals for a promise they do not make.

The table holds what is there: a store with no retirement record has nothing to
promise about one, and listing the path anyway would put a row in it that no test can
arm a fault at. Three rows are not optional in that way — the segment file, the
directory naming it, and the manifest listing it — because every one of the four
answers asserts all three. Their absence stops the barrier rather than passing through
it silently, since absence and success are the same silence: a flush asked to make a
file durable that is not there does nothing, and says nothing about it. For the same
reason those three are opened rather than asked about, so that a file removed between
the moment the table was built and the moment it is flushed fails the open instead of
being skipped quietly.

`writers/<writer>` is created once, and the call that creates it is the only one that
makes its name durable — a retry finds the directory already there and skips the flush.
So the directory row makes its own entry durable as well as flushing the directory:
a mkdir that succeeded and a flush that did not would otherwise leave a name nothing
ever repairs. The remaining ancestors need no row, because every file row flushes the
directory naming that file, which covers `writers/<writer>` from the segment, the store
from `instance.sexp`, and the registry's own directory from `instances.sexp`.

The owner and retirement rows are taken for **every** generation, not only the one
being published to, so the cost of a durable answer grows with the number of writers a
store holds and is paid under the exclusive lock. That is a measured cost and not a
defect: narrowing it to the generations on the path to this writer is sound only once
the metadata a recovery actually reads has been written down, and it has not been.

### Durability stages

Every call that makes something durable names the stage it belongs to —
`deliver-barrier`, `commit`, `registry`, `publish`, `snapshot`, `repair` — and the
stage is a required argument, not a decoration:

    (fsync! fd subject stage)
    (fsync-dir! path stage)
    (atomic-write! path bytes stage)
    (directory-entry-durable! path stage)

Fault injection matches on the stage, and a staged fault never matches a call that
declares none. That makes the list of stages **the list of steps a test can reach**:
an unlabelled flush is not merely untested, it is untestable, and in every log that
is indistinguishable from a step that passed. Two such calls sat in the publish path
until a case was written that tried to arm them. Making the stage a required argument
is what turns the rule from something to remember into something that cannot be
written wrongly; `test/stage-gate.ss` reads the sources and checks that every one of
these calls names a stage from the list.

Run the suites from source (igropyr must be a sibling checkout):

    sh test/run-all.sh
