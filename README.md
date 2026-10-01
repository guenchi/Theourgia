# ci

This branch holds only the test harness; it shares no history with `master`.

To run the core's whole suite on a commit of this repository (the commit must
be pushed, on any branch):

    git fetch origin ci
    git worktree add /tmp/theourgia-ci origin/ci    # once
    cd /tmp/theourgia-ci && git checkout -B ci origin/ci
    git commit --allow-empty -m "suite <commit>"
    git push origin ci        # rejected? git pull --rebase origin ci, push again

Each pushed commit starts one run. Push one commit at a time: a push carrying
several commits runs only the last. The run's artifact holds the suite log,
every fixture's output, and each not-green fixture run again alone.

## Reading a run

The job is green when the suite ran to its end and every not-green fixture is
in `known-red.txt` with only hard lines of its known class (rows that depend
on this runner's speed, or on a terminal it does not have). `verdict.txt` in
the artifact lists each not-green fixture as known or UNEXPECTED. A green job
is not the whole gate: the fixtures named in known-red.txt, and the start-up alternation, are
still run on the development machine, alone, which takes minutes. A red job
is read from verdict.txt: an UNEXPECTED fixture is run on the development
machine before anything is concluded from it.
