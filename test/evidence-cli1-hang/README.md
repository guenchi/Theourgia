# `cli1` wedged, 2026-09-17

Captured from the live process of a suite run that had been stopped at
`cli1.ss` for four minutes, before it was killed. The defect it was
reacting to was a missing `string-contains?` in the injected branch of
`ffi.ss`; `cli1` could not say so.

    sh.txt          EARLY 0 / WAITED 0 -- neither child was ever observed
    p-locked.trace  two lines; the second is the load exception
    q-locked.trace  the same, for the second child
    holder.ss       the child both of them ran

`sh.txt` is the whole reading: the fixture's bounded spin (`i < 4000`)
ran out because the children were already dead, and the byte it then
writes to `gate-locked` went to a fifo with no reader, which blocks
until the runner's 900-second alarm.

NEVER: THE FIXTURE REPORTS THIS AS `(0 0)`, which reads as "the reader did
not wait for the lock" -- a statement about lock behaviour, from a run in
which neither process reached the library. That is why the first guess at
the cause was machine load. See RUN.md, "A hang is the failure this
harness reports worst".
