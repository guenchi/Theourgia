# Running this delivery

Everything here was run from inside this directory, against a library path built
from **these files only**. To repeat that:

    sh run-fixtures.sh <output-directory>

with `THEOURGIA_LIBDIR` pointing at a directory holding `theourgia/` (the
library sources from this delivery) and `igropyr/`. `env.sh` sets the library
path from that variable; without it the working trees are used and the reading
is only as stable as they are.

The runner prints three lines you should read rather than skim:

    fixtures run: <n>   not-green: <n>
    libraries (<n>): ...
    probes, printed a usage line (<n>): ...
    all <n> scripts accounted for

**Every script in the directory is accounted for**, by outcome rather than by a
hand-written list. A list is silent about the file it forgot; this directory's
list had forgotten one for days, and it was red the whole time.

## What the green means, and what it does not

A fixture is green on three criteria together: the sentinel `<name> complete`
says it reached its end, both counters (`failures` and `mismatches`) are zero,
and no hard line (`FAIL` / `MISMATCH` / `Exception`) appears. Any one alone is
not enough — a file that stops in the middle prints no failures.

Alongside the suite, this batch was checked by seeding each fixed defect back
into the source and requiring a named row to go red. At the time of delivery
that set holds **80 seeded defects**, with the base verified against the working
tree before every one, and every anchor checked to match its target exactly once
before the run begins.

> **That number proves the 77 things it names, and nothing wider.** It is not
> evidence that the input boundary is complete. Across the reviews of this
> batch, most of the completeness gaps were found by the reviewer, not by the
> suite and not by me. Read it as "these specific regressions are guarded",
> never as "this is validated".

## Three things this harness learned the hard way

**A count that is printed and not compared is not a check.** The script that
builds a delivery reported how many libraries it had copied; it copied six of
eleven, said so, and nobody compared the number to anything. Sixty-five fixtures
went red for want of imports. It now compares the count and refuses.

**A seeded defect rots against the code it targets.** Each one is written as a
literal piece of source, so a refactor can make it stop matching — and a defect
that matches nothing does not announce that it has stopped checking anything; it
simply never goes red again, while the total keeps printing. Seven of them had
gone silent by the end of this batch, for ordinary reasons: an indentation
changed, a function moved to a new library, a procedure gained a parameter. The
run now verifies every anchor before it starts and stops if one matches zero
times or more than once. **A dead check has to become a red, not an absence** —
nobody counts the guards that are missing.

**A run killed between applying a defect and restoring it leaves the defect in
place**, and every later reading is then measured against a version nobody
chose — which does not look like an error, it looks like a table of verdicts.
The harness verifies the base matches the tree before each seeded defect and
stops the whole run, naming both hashes, if it does not.

## A hang is the failure this harness reports worst

`log.ss` carries a sentence about taking a lock against yourself: *"a lock does
not fail, it hangs, and a hang is the failure a suite reports worst. It hung the
first time this was wired up."* It hung again on 2026-09-12, in a probe, because
a raise got past the line that released the session and the next session waited
for a lock the same process already held. The readings taken from the live
process are in `evidence/f4probe-hang.txt`; the fix is the session release moved
into a `dynamic-wind` cleanup, where no exit can skip it.

It went unnoticed for an hour and a half because the readings in use came from a
different run of the same probe. A hang writes no output and counts no failure —
it leaves a process. So every fault-injection probe here runs under a timeout,
which turns a hang into a reading, and the end of a round looks at what
processes are left rather than only at what files were written.

## What this batch has not established

Asked of the reviewer at the end of the durability thread: is there a category of
question none of the findings touched? The answer was a category, not a defect —
**systematic crash consistency at the storage level.**

What was examined here is particular crash windows and particular failure
answers. What has *not* been established is that every disk state permitted by
interrupted writes and flush ordering preserves the contract after reopening.
The concrete form of the question: across `atomic-write!`, `run-barrier!` and
registry reconciliation, can power loss leave durable record bytes whose
directory entry, discovery metadata, dependency history or rollback witness is
missing?

Answering it needs a storage model or a power-cut harness that explores those
persistence boundaries, reopens, and resends the exact request — checking that
acknowledged work stays discoverable and uncertain work cannot silently execute
twice, including a second crash during recovery. **Killing the process does not
reach it**: the kernel's dirty pages survive, so the interesting states are never
produced.

This is named because an unasked question is easy to mistake for an answered one.

## What a fixture owes the round, and four ways this suite did not pay it

Three seeded defects in the last round came back `CRASHED` with **no failures**
at all. The store had answered every one of them correctly and said so plainly;
the fixtures died while reading the answers. A file that ends early still
reports what it found first, so the round scored one of them as a clean kill
while 37 of its rows never ran.

One rule covers all of it: **a fixture must be able to report, whatever the
program under test answers.** It was broken four ways, and each way is now
closed:

- a row took the answer apart before establishing its shape — three of that
  row's four components asked `(eq? (car a) 'ok)` first and the fourth did not,
  which is invisible whenever the answer comes back `ok`;
- `want` was a procedure, so both of its arguments were evaluated before it
  could guard anything. It is now a macro, and it guards **both** sides: a row
  whose expectation is derived from the program's own answer can fail to compute
  just as the answer can;
- a top-level binding between rows is outside every row's guard, and a raise
  there still ends the file;
- a helper's failure marker was a symbol where all of its callers needed a
  string, so the marker could not travel its own contract.

**And a fifth face of it, with a different failure mode:** a row that waits for
the program to reach some state needs its own bound. Under one seeded defect a
fixture waits for a child that can no longer report, and only the runner's alarm
ends it. The verdict is then right and the coverage past that point is gone.

### Rows are counted, and the count is compared against a reading

`rows-baseline.txt` records, per fixture, how many rows ran on unmutated code
and that fixture's md5. There is no static number to compare against: 85 of this
suite's rows are written inside loops and case tables, so counting `(want` in
the source is neither an upper nor a lower bound — measured, not assumed.

The baseline is a reading of the fixtures, not an outside source, so editing a
fixture makes it silently wrong about that file. The md5 is what turns that into
a red that says its own name rather than a confident wrong number. **It does not
replace the verdict**: whether a mutant was caught is settled by the failures,
and a stale row baseline cannot make that wrong.

## Two rows that were green for reasons unrelated to what they assert

Both were found because an unrelated change perturbed the surrounding code, not
because anyone was checking.

**A baseline read after the action it was supposed to bracket.** Two rows bound
`before` and the action in the same `let`, whose initialiser order R6RS does not
specify. One had been passing for months on the order the compiler happened to
pick, and changed answer the day something nearby changed. The other is worse:
in the unlucky order it compares a size with itself, so it would pass while
testing nothing, for as long as it existed.

## A kill is not evidence for a cell unless that cell is among the killers

A cell was written for a defect, its inverse was seeded, and the round went red
— every surface fact agreed. Counting which rows went red showed **none of them
was the new cell**: the mutation had been seeded at one of the two places that
produce that answer, and the cell exercises the other. It died to unrelated rows
asserting the answer's shape.

The mutation must be in the path the cell exercises, and the way to know is to
read the titles of the failing rows rather than the count.

## One seeded defect the round cannot kill

`U1b` replaces `store-supplied-fields` with the literal `'(parent ord)` it is
defined as. Nothing distinguishes the two: the mutant computes the same answer
for every input, and no cell can be written that fails against it and passes
against the store. It is recorded rather than chased.

It is still worth having in the list. The defect it stands for is not "this
answer is wrong" but "the same fact is now written in two places" — the failure
arrives later, when one of them is edited. What a surviving `U1b` says is that
the single definition is doing its job; a round in which it were ever *killed*
would mean the two copies had already come apart.

## A refusal is addressed to a program, and had to be readable by it

The refusals this store gives were explained by sending the caller's intent
back. For the refusals that were RIGHT, that made them unreadable: the datum a
refusal is about is exactly the datum the wire layer will not write, so
`symbol-not-wire-safe` came back spelling the relation `\x31;` and the client
reported a transport error at that byte instead of the reason. **A refusal its
only reader cannot parse is a refusal nobody can act on.**

The echo was a symptom. The detector already knew which symbol was bad — it had
just tested it — and returned the bare word, so the answer carried the whole
intent back to make up for what the reason could not say. Keeping what the
detector found makes the echo unnecessary, and the unreadable answer disappears
as a consequence rather than being patched around:

    (error malformed-intent (symbol-not-wire-safe (where relation) (spelling "1")))
    (error malformed-intent (too-few-arguments (verb insert) (given 1) (needs 3)))
    (error malformed-intent (not-an-id (argument 1) (spelling "7")))

A spelling is always a string, so this is safe by construction rather than by
anyone remembering to be careful. Symbols are spelled by their name, because the
syntax needed to write one down again (`\x31;`) is readable and still answers a
question nobody asked.

**The rule is wider than the report that prompted it.** Applying it to every
error answer found a second entrance nobody had reported — a refused verb came
back as the caller's own symbol, so `show me` produced an answer that could not
be read. The cells check the refusals through the product's own reader rather
than against a description of the whitelist, so a change to what the wire
accepts reaches them without anyone updating a copy of it.

## Three defects in the instruments, not in the store

None of these changes what the store does. Each changes what a round is able
to SAY about it, and each is written here by its mechanism rather than by the
symptom it produced, because the symptom in all three cases was silence.

**A fixture's wait is bounded on one side only.** `cli1`'s lock-contention
section spins for a bounded number of iterations waiting for a holder to reach
its barrier, and then writes a byte into a fifo to release it. The spin has a
bound; the write does not. *Reproduce:* break option parsing (the `F6a` seed)
so the holder never reaches the barrier -- the spin expires and the fifo write
blocks forever, because nothing ever opened the other end. On healthy code the
holder always arrives, so the unbounded half is never reached and cannot be
seen. *Next batch:* the release needs the same kind of bound the wait has, and
a fixture that gives up waiting must say so as a failed row rather than stop.

**The round's driver can be held open by a process it never started.**
`subprocess.run` reads the child's pipe until every writer closes it. The
alarm bounds the child; it does not bound the child's children, and an
orphaned grandchild inherits that pipe. *Reproduce:* any mutant that makes a
fixture spawn a shell that outlives it -- the direct child becomes a zombie,
the round's own child is gone, and the driver still waits. Measured once at
one hour fifty minutes, ended by killing the grandchild by hand, after which
the round continued and scored the mutant normally. *Next batch:* put each
fixture in its own process group and kill the group on timeout.

**A row that waits on the program needs its own bound.** This is the rule the
first item breaks, stated so that the next fixture does not have to rediscover
it: a row that waits for the program under test to reach some state must be
able to stop waiting and report. Otherwise the only outcome is the runner's
timeout, and a timeout reads as flakiness rather than as a kill.

### The row count reaches 42 of the 62 fixtures

A row baseline exists only for fixtures that report through `want`. The other
twenty -- `q8` among them, and the `smoke-*` and `verify-*` family -- count
`mismatches` instead and print no row total.

**Measured, because the difference matters:** every one of those twenty prints
its closing sentinel, so a file that ends early is still caught, as
`sentinel=False`. What is missing for them is the count, not the detection: a
mutant that kills such a fixture after some of its rows is scored correctly and
says nothing about how much stopped running. No fixture in this suite lacks a
sentinel, so there is no file here that can die quietly and read as zero red.

*Next batch:* give those twenty a row total of their own, so the coverage note
covers them too.

### And the coverage note has a blind spot of the same shape

`died after row N of M` is printed only when a count was printed. A fixture
that ends early never reaches the line that prints it, so the case the note
exists for is the one it cannot describe: it shows `rows=None/M`. The count
could be inferred from the `ok`/`FAIL` lines already in the output. Also next
batch, for the same reason as the others -- changing the driver mid-batch
costs the comparability of the round it is measuring.

## Two limits this delivery does not close

**Retirement records written by earlier builds say less than new ones.** A
retirement now always carries the stretches its adopt could not vouch for, even
when there are none, so an absent clause means "older build" rather than "nothing
lost". The older ones are therefore read the widest way: everything above the
retired prefix is in doubt, with no upper bound. A request whose cursor falls
there is answered `unknown` every time, and an operator's `not-executed`
resolution cannot release it, because a resolution may only exempt a stretch that
has a top. Stores created from this version forward never produce the older form.

**A caller that captures a continuation inside its own write procedure and
re-enters it afterwards ends the session twice.** The second end raises and is
suppressed, so no answer is lost, but the lock accounting is not written for that
shape. This is recorded rather than fixed.
