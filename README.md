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
