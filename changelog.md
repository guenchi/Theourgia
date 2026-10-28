# Changelog

## 1.3.0 — 2026-10-28

*336 commits.* Manteia, the rule language, completed (kinds, stored derived relations, aggregation and stratified negation, incremental advance); structural drafts; a review channel that carries a frozen candidate; the daemon answering from its publication with one session kept across requests, started by the MCP shell and leaving when idle; a store identity that survives a remount; the reducer's pending queue and gates made linear; and the store's git bridge (import-git, publish-landing, the edit tools).

### Manteia, the rule language

- Column kinds for the rule language, checked statically; kind containment normalises both sides; goals dispatched to one handler per kind in three environments.
- Stored derived relations, declared by a verb (`derive`) and checked where they are used; a resent rule is its replay; recursive relations kept whole; availability by one search over the reach, the shortest through-chain reported; a relation's through-chain built once a table.
- `not`, `count`, `sum`, `min` and `max` over a goal; stored relations stratified; a form's table named by its occurrence and group; emptiness read goal by goal.
- `session-advance`: a session advanced to a later state, its positive strata maintained by delete-and-rederive, the auxiliary session's work charged to the advance; each callee collected once.
- An `and` nested in an `and` is its goals in place; `supply` from any tool with its version.
- The README's language chapter, construct by construct, its example answers measured.
- A text-mode Scheme file's library and imports are read from its header through its file holder (`in-library`, `imports` and the new `file-imports`, all external), by a new bounded reader library, `(theourgia reader)`; a `holder` fact relates a code block to its file holder, and `def-for`'s no-library clause holds only within one holder, so a script's own definitions serve its own pieces and nothing else. `uses-name` for a text block of a language with a reader binds what its complete forms bind, `(syntactic (reading forms))`. `context` folds a name's definitions past the first two into a `folded` clause, takes `--names <n>` (repeatable, reported in `asked`), scopes a file holder or any non-code target to the code blocks under it, and takes `--budget min+<t>`; the session trace counts, per call, definitions-index builds, fact builds by relation, preferred candidates by step, and whether the session was reused or why it was dropped.

### Daemon

- `query` and `context` are published reads of the store process: the publication made current first, no replay while the store has not changed; a query after an outside write sees it (the `theourgia` hooks rely on it).
- One query session kept by the daemon across requests: reused on the same publication, advanced when the publication moves forward, replaced otherwise; `serve --session-size <entries>` (0 disables); `(name-use syntactic)` reported per answer; the one exception to byte-identity with the local route: a query the local route refuses for its budget may be answered by the daemon.
- A store whose directory is removed is drained and the daemon exits `store-gone`; an absent store creates nothing.
- A plain command starts no daemon. With nobody listening, the command line starts `core.sc --frame` as a child, writes it the request frame a daemon would have been sent and reads its answer as a daemon's (the same 32 MiB ceiling, no answer deadline, so the request waits for the store's lock), and nothing is left running; a daemon is started by `serve`, or by the MCP shell for the session it serves. A request answered in a child meets the in-process route's differences: `describe` adds no template clause, the lock is waited for rather than refused `store-busy`, the store is read as it is rather than as last published, `--rev` is refused `no-published-state` and `subscribe` `needs-daemon`. A daemon's connection and the child read, check and dispatch a frame through one library, `(theourgia frame)`. A store without its lock file answers `absent` on a plain command; `serve-start-failed` is only the MCP shell's start relaying `serve`'s answer.
- `search` answers over the state the dispatcher hands it, as every read verb does, instead of opening and reducing the store on each call, so a resident daemon no longer folds the whole store again for every search. Over a connection it answers the publication as it stands, as `read` does.
- `serve --idle-exit <seconds>` and `--idle-grace-ms <milliseconds>` (default 200, at most 5000, a larger grace refused `too-large`): a daemon leaves by itself after that long with no connection open and no request running. It unlinks its public link first, takes for the grace the connections the kernel had already completed, and exits 0 with `(idle-exit (after <seconds>) (refused-accepts <n>) (callback-raised <n>))`; a late connection cancels the exit (`idle-resumed`), and a resume that cannot happen drains, exiting 75 with `(idle-exit (reason resume-failed <cause>))`. A daemon started with the options names them in its `serving` line.
- The MCP shell is the opener: it starts its daemon with `--idle-exit` from `THEOURGIA_IDLE_EXIT` (1800 seconds when unset, 0 never; any other value refused `bad-environment` before a start) and `--idle-grace-ms 200`, gives each request one deadline and at most two attempts (a `serve-busy` start retried after 100 ms, a daemon gone before the request's own connect started once more), and then answers `connect-failed` with `(carried-out no)`; a written request is never sent again.
- The socket path is a symbolic link to the daemon's private socket, `<socket>.d/<pid>`, in a directory of its own whose `.endpoint` marker reserves it for the endpoint. A start reclaims what an earlier daemon of the same endpoint left; it is refused `serve-path-reserved` inside another endpoint's marked directory, `socket-dir-missing` where it would make such a directory, and `socket-path-too-long`, exit 2, when the path leaves no room for the private name. A bind or publish that fails after the store is ready exits `listener-down`.
- Every drain waits for its listener to hand over the connections it accepted, so a connection accepted just before a drain is answered `draining` or served, never closed with nothing.

### Reducer and admission

- Gated arrivals: an arrival's work no longer grows with the number of standing gates (the square of the standing gates per arrival before, the whole fold the cube).
- The pending queue indexed by what each record waits for: arrivals wake only their waiters; a seq or dep outside the log's envelope refused at `reduce-apply!`; two copies of one event leave the heap in arrival order.
- An arrival's work independent of the standing gates (the gates as a table in admission; the notes exact); the declaration tables indexed by name and replayed in linear time; the blocks and the pasts found by id; the block structure maintained, not recomputed; the outline's order total.
- Admission judges only what an arrival can change; a single record naming a plan is judged again when its plan's binding changes.

### Structural drafts

- A draft can create a file or delete one: `working-new` and `working-delete` beside the block drafts, each with its own version and cut; `commit --working-new <provisional>=<version>` and `--working-delete <holder>=<version>`; the commit makes the holder and its pieces by the construction import-git shares, in one batch with the block drafts, and answers `(created ...)` and `(pieces ...)`, also on a replay. edit-sync turns an added file into a new-file draft and a removed one into a delete draft, refusing paths outside the store route's domain (dot-named, not text, a mode other than 100644, a case collision in the resulting tree) and any live structural draft the export does not account for (`draft-changed`, remedy: export again). A new file is shown by the export only (not by outline, context or eval --working).

### The review channel

- `scope --freeze <dir>` reviews a frozen candidate (the copies hold the drafts' bytes, a `before` section the committed text) and writes no main letter; `--packet` adds the questions; `--roots all` copies the whole tree; `--home <h>` gives the scoped store its own home so the reviewer writes its verdict there. review-results answers `pending` until a verdict exists and refuses an incomplete one by name; sources must include a candidate copy. `collect-into --letter new --map <LANDED.sexp> --to-writer <W>` brings the verdict into the main store after the landing, resolving provisional ids and pieces, and turns the reviewer's proposals into drafts for writer W. Cuts are compared as sets. The mail handler is now contrib/mail-hook.sh (codex-mail.sh still runs it), and contrib/claude-hooks.json runs it for Claude Code with `THEOURGIA_ACTOR=claude`; it exits 1 with a line when its query fails, since a failed query is not an empty mailbox.

### Store tools and the git bridge

- `import-git`: a git range absorbed into the store as committed blocks and a tag; one comparison-domain judgement for publication and import.
- `publish-landing`: the files a landing changed published from the store as one git commit; `landing-commit` tags the start of a landing; `edit-export` and `edit-sync` turn an agent's file edits into a writer's drafts; dump and restore a writer's drafts; the freeze as one script checking the view its tree came from.
- `changed-files`: the files whose exported bytes or presence differ between two cuts; `locate`: a file's byte or line range, or one diff hunk, to the blocks holding it; `position`: the local writer's last sequence; a named tag's answer carries the cut it bound.
- Exports at a past cut (`export-code`, `export-md --cut`), judged against the daemon's publication as a read at a cut is.
- A draft whose base fell behind says so: `drafts --writer W` names each after its items, `(behind <block> (based-on <v>) (committed <v>))`. `landing-freeze` brings every draft onto the applied cut before it dumps (a draft whose block did not change is moved, one whose block changed is merged by a line-based three-way merge, or the freeze stops with `drafts-behind-conflict` naming every conflicting block), so a record committed after the drafts, a review letter included, no longer leaves every draft to be rebased by hand; `landing-freeze --check <dir>` validates a frozen directory and answers current, refreeze or the conflict, so a landing script can stop before its start tag.
- `landing-freeze` moves a structural draft behind an unrelated commit onto the applied cut: a new file whose path no live file holds there, or a delete whose file's baseline there is still the draft's `based-on`, is rewritten in place at the applied cut with the same bytes, path and language and a version recomputed for that cut, under one hold of the writer's draft lock and only while it is still the version the freeze judged (a draft written again since is refused `structural-changed`, not overwritten). Nothing is removed first: a refusal or a failure before the rename leaves the entry where it was, a failed directory flush after it leaves the whole new entry at the applied cut, and either way the freeze is refused `structural-move-refused`. `REBASED.sexp` records the move as `(moved <key> (from-cut <cut>) (to-cut <cut>))`. A new file whose path a live file holds at the applied cut, or a delete whose file is gone or has changed there, still stops the freeze with `drafts-behind-conflict`.

### Reading and rendering

- A text-mode code block's src and doc given as text; a src held as bytes renders and is measured as its text; a draft beginning with a UTF-8 byte-order mark commits as its bytes; `whereis` nearest finds a name in another case.
- `search` over a long query, and the relevant step of `context --for`, cost what the hits cost: a scan drops a block at the first token none of its fields takes and compares characters where they stand, without a copy per position, and the relevant step asks with each token of the text once. On a development store of 17,000 blocks, the context of a 30 KB block went from 420 s to under 4 s, with the same hits, scores and snippets for every query.
- `context --working` assembles the material over the request writer's working view, the committed state at the drafts' cut with that writer's drafts laid over it, in a fresh query session, never the daemon's kept one. The answer carries `(working "<writer>")` after `for`, counted in the budget; a refusal of the view (`writer-required`, `invalid-working-writer`, `working-unavailable`, `working-draft-unreadable`) is the answer, and an id the view does not hold is answered as `read` answers it over that view. Drafts are not changes, so validity and the other hard queries answer the committed rows; the premises of drafted blocks are the overlay's and are not handed to a commit.

### Store identity

- A remount is not a move: the device is compared last, and a store whose directory changed only its device (a reboot can do that) is written as the same instance, `instance.sexp` rewritten with the live device under the store lock and the change kept in a history of the last eight remounts; `adopt` on such a store answers `(ok (reason remount) (device <old> <new>))` without deriving or retiring a writer. A copy elsewhere has another inode and is refused for it as before. The limit: a block-level clone of a whole volume keeps its inode numbers and is taken for a remount; `cp -R` makes new files, and adopting that copy gives a store of its own.
- `check` reports the store's identity as `(instance (machine <m>) (device <d>) (inode <i>) (nonce <n>) [(remounts ...)])`, and a writer whose highest segment is empty after its last record's segment as `(empty-segment <n>)`, the state an interrupted rotation leaves; neither changes the verdict.

### Log, ffi, platform

- An append keeps its own writer's prefix; the tail check lists segments without a stat per entry; the kept prefix seeds past an older empty segment.
- `path-stat` and `fd-stat`; a child receives this process's environment on every platform, FreeBSD included; reading the environment fails as a spawn failure.
- A test-only timing seam on the write path (the wrappers a library of their own, decided at expansion).

### Transport and CLI

- `--help` and `-h` answer the verb's usage and touch nothing; the verb libraries entered on first use; a registered verb handed the dispatcher's helpers; a transport option on the stream's route refused by name; a trace line written whole.
- Programs compiled as program objects at optimize-level 2.

### Fixed

- `split-suggest` reads Scheme character literals, so `#\(` opens nothing and a file with one is no longer a single block; a Scheme library's body is cut at its definitions.
- A `define-values` definition names its formals, so `whereis` finds it as a definition.
- A marked export whose last body row would be a marker line but for a trailing carriage return, followed by a pad, now escapes that row; such a file decoded as `invalid-padding` before.
- `context` finds the blocks relevant to a committed text held as bytes.
- `adopt` on a store with no local writer is refused `no-local-writer`, before any step; it answered `internal`.
- `write` given only half a baseline names the missing half: `--based-on` alone is refused `cut-missing`, `--working-cut` alone `based-on-missing`, a working parent's likewise.
- An error raised out of a verb is answered with the dispatch's clause on every route; the command line ended on an uncaught exception.
- `diff` checks that each replay covers its cut, and is refused `cut-moved` when one does not.
- An endpoint selector whose field clause held `_` matched nothing, so every link under its relation was refused `relation-endpoint`. `_` now holds for any value of a field the block carries (a contested field holds none), and a new declaration that writes a variable there is refused `selector-value-not-literal`. An earlier declaration with `_` now holds for any value; nothing stored is reinterpreted.
- The MCP shell refused `tools/list` whenever it carried params, answering "Pagination is not available", so a client that sends params with no cursor (Codex does) could not start; it now serves the whole list and refuses only a cursor with a value. `contrib/doorbell.sh` sends the Enter as its own keystroke after the line, which a combined send left unsubmitted in some terminals.
- A batch of a few hundred inserts under a rule naming `field+` was refused `rule-unevaluable` (`query-budget`, `field+`) before a selector was asked: a write's facts (`kind+`, `field+`, `edge+`, `edge-kind+`) were read by scanning the whole relation and charged its full size on every dispatch. They are now read by their bound arguments and charged the rows matched each time the goal is dispatched; an over-budget join is still refused.
- A writer directory copied in from another checkout that sorted first was taken as the local writer, so this copy's writes went into another instance's log. The local writer is now chosen by ownership: a live writer whose `owner.sexp` names the `(machine nonce)` of this copy's `instance.sexp` is preferred by every write, `position`, a commit's cursor and `adopt`, and name order decides only when none does; a request whose writer comes to be passed over while it runs is refused `writer-superseded` (remedy `reopen`). The verbs that change drafts refuse a `--writer` owned by another instance (`foreign-writer`, remedy `draft-writer`), and `check` reports `writer-owner-foreign`, `writer-owner-unverified`, `instance-absent` and `instance-unavailable` without changing its verdict.
- A write refused for the store's identity left an empty next segment behind, and the `adopt` that followed named a prefix in no segment, which `check` then read as damage for ever. A write is now admitted before it cuts a torn tail or rotates, and `adopt`'s prefix names the segment of the last applied record.
- A working view asked for a writer with no drafts, through `context`, `eval`, an export or the drafts listing, no longer creates the writer's area and lock file: the view looks first, without the lock, for the writer's `working/` or `structural/` directory, and with neither answers the committed state and creates nothing (`read --working` never created it).

### Tests

- The test runner gives fixtures a home of their own: `HOME` is the run's scratch home and `THEOURGIA_HOME` is unset, so no fixture writes the user's machine registry; the test home links the `.rustup` and `.cargo` directories of the runner's own home when they exist, so toolchains that resolve from `HOME` still run.
- The fixture launcher reads the anchor's process group back instead of trusting an errno: a failed `setpgid` with the group set goes on with a note, another group stops the launch as `ANCHOR GROUP NOT SET`, and only `ESRCH` is `ANCHOR DIED`. Its test-only `--fault-setpgid` gains the `anchor-skips` and `anchor-late` modes.
- The interleave row of `unreadable-acknowledge-routes` waits for the search's answer on the monotonic clock instead of a count of shell sleeps, which a slow runner stretched past the daemon's hold.
- Two `context-verb` rows pin a working answer's used tokens against what it rendered, and that its hard queries are asked at the drafts' cut.
- The change-stream guard masks the device and inode in `check`'s instance clause, which differ between any two store directories, and nothing else.

### Known at this release

- The median start-up, 668 ms over ten runs, is above its 637 ms budget; recorded, with a profiling item open.

## 1.2.0 — 2026-10-11

*156 commits.* Mail and review between sessions of any vendor through a store, rules and relation effects declared as data and judged before every write, a first import cut at an editor's symbols, stale verifications named by `context`, and a run of fixes found by the first two measurements on the real tree.

### New verbs

- `scope <dir> --cut <cut> --roots <id> --for <actor>` makes a store for one reader holding exactly the blocks under the roots as they were at the cut, with a letter copy (`to`, `status`, `cut`, `roots`, `scope`, `baseline`) and one letter in this store; `review-results` reads the reader's verdict and findings back as one datum; `collect-into <letter> --results <datum>` and `collect <dir>` bring them under the letter here, each finding's `about` edge on the original block. A message is a block of kind `doc` with `to` and `status`; reading one's mail is a query. `contrib/` holds the Codex hooks, the doorbell and the reader's AGENTS.md. `THEOURGIA_SOCKET` names the main store's daemon for `scope` and `collect`.
- `relation <name> --as <kind> [--from <selector>] [--to <selector>]` declares what a relation name does in this store: one of the six relations with an effect, or `nothing`; the lifecycle reads the declarations, and a reason names the relation.
- `rule <name> --on <kind> [--where <goal>] --must|--must-not <goal>` declares a rule of this store in the query language; every committed write is judged against the rules before it is written, by a rehearsal of the write on a clone of the state, and refused with the rule's name. `--builtin citation-coverage` enables the built-in rule. `template apply` installs a template's rules with its roots; the `project` template declares `cover`.
- `import-code --symbols <symbols-file>` splits the FIRST import of a file with no marker lines at the symbols an editor collected, into code blocks under its file block; a file that carries marker lines follows its markers.

### New options and clauses

- `context` names a stale verification: for a block in scope whose `verifies` edge's target has moved past it, the notes carry `(stale-verification <m> verifies <c>)` and the receipt holds the target, read through the new query relation `verified-moved`; the human output prints the receipt on one line, as `--wire` writes it.
- `commit` answers `behind-records`, how far each writer that moved since the writer's baseline is, in records.
- `check` answers `(undecodable-text ((<id> <field> ...) ...))` naming the blocks whose stored text cannot be decoded; a text block beginning with a UTF-8 byte-order mark is text, and `search` and `grep` read it.
- `eval` refuses an output limit by naming the option, cuts what a limit carries to the limit, and takes `--under` a library id; `eval --working` refuses a draft on a datum block by name.
- `set <id> mode datum` from the command line stores the symbol, as `batch` does.
- A datum block takes no `src` by any intent's spelling or order; a batch is read as a set for the datum rule.

### Changed behaviour you may notice

- A delete's targets include the live far ends of the deleted block's edges, so a rule over a review that cites a result judges the deletion of that result.
- A field set to the symbol `omit` reads as itself; it was read as absent.
- The history's order is delivery order, and the comments and the two readers of it say so and agree on every route.
- A plan another writer made is read as its members are declared: five readers unwrap an `expect` wrapper through one procedure; a revoked plan's wrapped member is restored from its text.
- The daemon's own repair of a torn tail replaces what it knows of the segment, and records the snapshot only when it is at least the cut's length; what it knew of the segment is kept only when the segment shrank or its new snapshot cannot be read.
- The transport refusal of the MCP shell names its option, and the tools say they are bound to a store.
- `def` refuses a name the library already defines.

### Fixed

- `supply` refuses a datum library's file by name instead of answering stale.
- `state-revoked` compares versions as the strings they are.
- A write declaration too short for its verb is asked its shape first, instead of being read as a different request.
- An insert refused for both datum and `src` carries no block clause; `eval` states its busy pool before usage.
- `check`'s reserved-relation records are sorted by event, the same on every route.

### Test-only hooks

- `THEOURGIA_PROBE=off` starts a test daemon without its timer probe; read only by an injection build, any other value refused at load.

### Documentation

- The README gains "Mail and review between sessions" (the mailbox, `scope`, `review-results`, `collect-into`, `collect`, a review round, Codex as a reader, what this does not protect against, where this runs), the `relation` and `rule` sections, the rules' place in `template`, and the environment variable table's new rows.

## VS Code extension 1.1.1 — 2026-10-09

*The extension's own version, `theourgia.theourgos`, published on the Marketplace for macOS on arm64 and x86_64 and Linux on x86_64 and arm64.* Import with Symbols, on a core pinned at theourgia be42914.

### Added

- Import with Symbols: a folder or a source file is imported into the store with the core's `import-code --symbols`, and the first import of each file is split at the editor's top-level symbols into a file block and its code blocks, with no review copy first. The report in the output channel places each new block at its lines, and gives a file the core refused to split with the line and column of the byte it names. A file with unsaved edits stops the command before anything is sent.

### The core it runs

- The core is pinned at theourgia be42914, ten commits after 1.1.0, which adds `import-code --symbols`.
- The core checks every cut of a first import split at symbols: a cut must fall at the top level of the file, outside its protected prefix (a shebang, a coding line, a byte-order mark), so a symbol that starts inside a body, a string, a block comment or an open bracket refuses that file instead of cutting it. The report names the refusal: `symbols-not-top-level`, `symbols-unscanned`, `symbols-in-prefix`, `symbols-unchecked` or `symbols-no-file`.
- The extension's unit suite passed against that core on darwin-arm64, darwin-x64, linux-x64 and linux-arm64 (CI run 37816045917).

## VS Code extension 1.1.0 — 2026-10-06

*The extension's own version, `theourgia.theourgos`, on the Marketplace for macOS on arm64 and x86_64 and Linux on x86_64 and arm64.* The core pinned at theourgia 1.1.0.

### Changed

- The extension is Theourgos: package `theourgos` under the publisher `theourgia`, id `theourgia.theourgos`, packages named `theourgos-<target>.vsix`. Its commands and settings keep their `theourgia.` names. A 1.0.0 installed by hand from a `.vsix` built before this change has the id `theourgia.theourgia`, which VS Code treats as another extension: let its pending saves go through, or discard them, then uninstall it before installing 1.1.0, since both register the same commands.
- The core is pinned at theourgia 1.1.0 (3a4ac93).
- The setting `theourgia.writer` is removed: each window has written its drafts under a name of its own since before 1.0.0, so it decided nothing. A `settings.json` that still sets it shows an unknown setting, and nothing changes.

### Fixed

- The search view answered "the store did not answer the search" for every query that found anything, since 1.0.0. A search now lists its hits.
- A datum-mode block opened as an empty buffer, and an edit saved through it was answered as saved and never reached the code. It now opens read-only, as the datum export writes its library's file, and a file opened under 1.0.0 on a datum block is refused on save instead of appearing to save.
- A save the core refuses because its block is a datum block, or because a log segment was replaced under the store's daemon, now says which, and for the second says to restart the store's daemon and save again.
- Search and the hover's mentions ask the core with `--wire`. When every hit is in a superseded or refuted block, the search view says the words are found only in blocks that are not in force, rather than that nothing matches.
- The package for macOS on x86_64 passes its unit suite against the core it is pinned to, and 1.1.0 is on the Marketplace for all four targets; 1.0.0 was packaged for Apple Silicon only.

## 1.1.0 — 2026-10-05

*26 commits.* Queries and context for agents, read receipts and write premises, class and validity, a change stream, runners for five more languages, and a measured row for macOS on x86_64.

### New verbs

- `query <goal>` answers every binding of a goal's variables over the committed state at one cut. Its facts are twenty relations the store already answers, combined by nineteen rules kept as data; `query --relations` lists both. A query reads no log, runs no code and writes nothing; one whose new work passes a million tuples is refused, never answered in part.
- `context --for <id> --budget <tokens>` answers what to read before working on a block, within a budget counted as four bytes a token of the answer as the wire prints it, its cut left out, with a receipt that a commit can be given back as its premises.
- `subscribe changes <rev> [<token>]` keeps a connection to the daemon and prints one line for each publication: blocks added and removed, fields, positions and parents changed, conflicts entered and resolved, edges added and removed. A commit made outside the daemon reaches subscribers within about a second. The MCP shell does not offer it, and without a daemon it answers `needs-daemon`.
- `commitments` lists the decisions still owed: a decision is open until a live block implements it (a task only once it is done) or its status is done or dropped. `--drifted` names implementations whose content has changed since the link, unless a later link or edit of the decision has seen the change; status, batch, keywords, class and slug do not count.
- `init --template` or `--template-file`, `template apply` and `template export` make a store from a template of document roots and relations, or give one to a store; the built-in templates are `project` and `memory`. `tasks` lists blocks of kind task.
- `names <id>` lists the names a code block uses and the libraries a library block imports; `uses <name>` lists the live code blocks that use a name.

### New options and clauses

- `read <id> --cut <cut>` reads a block at a causal cut, written out or as a tag; every `log` entry gives the event's `cut` and `past`.
- `read --rev` adds the daemon publication the answer was read from, `(rev <n> (daemon "<token>"))`, so a subscriber can apply frames by revision.
- Read receipts: `(cut ...)`, the state a read was answered from, and `(versions ...)`, the version of each block it shows or lists, at the end of an answer. `read`, `refs`, `commitments`, `tasks`, `names` and `uses` carry both; `search`, `grep` and `whereis` add the versions; `outline` and `reach` carry the cut.
- `--premises <datum>` on every committed write -- insert, set, move, del, link, unlink, tag, batch, commit, import-code, import-md, def and template apply -- with a list of block versions and query digests, or a `receipt` as `context` prints it. When they no longer hold, nothing is written and the write is refused by name.
- A block's `class`: observation, inference, ruling, verification or external. The relations supersedes, refutes, depends-on, implements, verifies and conflicts-with decide whether a block is valid, superseded, refuted or needs review. A plain `read` of a block that is not valid adds `(validity ...)`; `search`, `grep` and `whereis` leave out superseded and refuted blocks, and `--all-validity` keeps them.
- `eval --lang` has default runners for `typescript` (node), `go` (go run), `rust` and `c` (compiled, then run) and `java` (single-file launch). `THEOURGIA_RUNNER_TYPESCRIPT`, `THEOURGIA_RUNNER_GO`, `THEOURGIA_RUNNER_RUST`, `THEOURGIA_RUNNER_C` and `THEOURGIA_RUNNER_JAVA` each replace one with a whole runner. Runners still need `THEOURGIA_RUNNERS=on`.

### Platforms

- macOS on x86_64 has its own measured row, read under Rosetta, and is no longer refused; there `stat` and `lstat` bind their `$INODE64` symbols.

### Changed behaviour you may notice

- When validity leaves out every hit, the human output of `search`, `grep` and `whereis` now prints the `(excluded ...)` line instead of nothing. A script that took empty output to mean no match should read `--wire`.
- `write` on a block whose mode is datum is refused `draft-on-datum-unsupported`, and `commit` refuses a draft that would put text on one. Both used to answer ok, and the edit never took effect. Change a datum block with `def`; `check` reports a datum block that already carries text.
- A daemon refuses every later write to a writer's log once the segment it was appending to is replaced by a shorter one -- a git checkout of another branch under a running daemon -- until it is restarted. Restart the daemon after switching the store's branch.
- The answers of `read`, `refs`, `commitments`, `tasks`, `names`, `uses`, `search`, `grep`, `whereis`, `outline` and `reach` end with receipt clauses. The human output is unchanged except for plain `read` and `reach`, which print whole; a script that compares whole `--wire` answers will see the new clauses at the end.
- Superseded and refuted blocks are left out of `search`, `grep` and `whereis` by default in a store that links those relations. Ask with `--all-validity` to see them.
- Starting from source takes about 1% longer: the code the premises check adds is compiled at every start (1.1%, the median of three runs of 40 interleaved pairs); the commitments registry added 0.5% before it.

### Fixed

- The daemon sampled a publication's snapshot after its fold had released the store's lock, so a commit by another process in between could go unseen until something else changed.
- A retry that completes an interrupted plan -- a commit, an import, a def -- wrote its missing members over what other requests had written to the same blocks since, and answered ok. It now checks first and answers `stale-baseline` when a block has moved. An interrupted import that created a file can now be completed; it used to answer `malformed-intent`.
- `import-md` replaced bytes that were not UTF-8 with U+FFFD and imported the file changed. A file that is not valid UTF-8, or holds a NUL byte, is now skipped and listed under `(skipped ...)`, as `import-code` does.
- `read --cut` could answer ok from a smaller cut when a record was quarantined between the cut check and the replay; it now refuses `cut-moved`.
- A code block whose derived fields raised when shown was answered `(error internal ...)` with no block named; the answer now names it.
- A value written once that merely had a conflict's shape was read as a conflict by `search`, `grep`, references, `commitments` and `tasks`; a field is contested now only when two or more of its candidates survive.
- A record definition with a malformed field spec could raise when the store read a block's names; it now binds only its type's names. The name walk's shape rules follow what Chez accepts.

### Documentation

- README corrected against the code, with a new opening line.
- The agent-memory guide, `docs/claude-code-memory.md`, checked against the code.

## 1.0.0 — 2026-09-30

*225 commits.* The first release: a block-graph store where agents and people write together, with a command line, a daemon, an MCP shell and a VS Code extension.

### The store

- A store is a directory of plain files. Each writer appends to its own segments, and the store can live in a git repository; `init` writes its `.gitignore`.
- 38 verbs, each with a usage line and a one-sentence description that `describe` answers: init, insert, set, move, del, link, unlink, def, outline, read, refs, reach, search, grep, whereis, log, tag, diff, describe, conflicts, write, restore, commit, drafts, diagnostics, discard, batch, import-md, export-md, import-code, export-code, split-suggest, supply, adopt, check, snapshot, publish and eval.
- Writing goes through drafts: `write` keeps a draft in the writer's own space, and `commit` installs it; a draft whose baseline the block has moved on from is refused `stale-baseline`.
- The block-editing verbs, `batch` and `tag <name>` accept a request identity, so a client that missed the answer can send the request again and be told what happened.
- Code and prose sit in one graph: `import-md` and `import-code` read a directory into blocks (Scheme as data with `--datum`), and `export-md` and `export-code` write them back out.
- `eval` evaluates one expression against the store in a child process with no filesystem and no network; another language runs with `--lang` where the operator has set `THEOURGIA_RUNNERS=on`.
- `supply` keeps what an editor computed (signatures, calls, diagnostics) in `<store>/derived/`, outside the log.
- `publish` receives another store's history one segment at a time.

### The daemon

- `theourgia serve` holds a store open and answers over a unix socket; the command line starts one when none is running, and finds its socket by the same rule the daemon used to create it.
- It answers through the same dispatcher as the command line. `THEOURGIA_LOCAL=1` answers in process instead.

### The MCP shell

- `theourgia-mcp --store <path>` speaks MCP `2025-11-25` over stdio: one tool per verb routed to the daemon, and `eval`, run as the shell's own child.
- A core refusal comes back as a successful tool result.
- Each session gets a writer of its own unless `THEOURGIA_WRITER` or `--writer` fixes one.

### The command line

- `theourgia` is the command; `theourgiad`, the daemon program, and `theourgia-mcp`, the MCP shell, sit beside it.
- `build.ss` compiles the tree and igropyr into a directory of objects, which start about twelve times faster than source.
- Every verb takes `--store`, `--actor`, `--socket`, `--wire`, `--req` and `--cursor`.

### Platforms and requirements

- Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1, libuv and igropyr. On another Chez version the verbs that print a datum refuse with `unsupported-printer-version`.
- Platforms with a measured row: macOS on arm64, and Linux on x86_64 and on aarch64 with glibc. On any other platform every program exits 75 with `platform-unmeasured`.

### Published to

- Homebrew: the tap `guenchi/theourgia`, whose formula builds this release with Homebrew's Chez Scheme.
- npm: `theourgia` 1.0.0.
- The VS Code extension, at its own version 1.0.0, packaged for macOS on Apple Silicon.

### Known not to work

- macOS on Intel, or under Rosetta: the core has no measured row there and exits 75 with `platform-unmeasured`.
- FreeBSD 15 on amd64 has a measured row, but the daemon does not start there yet.
- The machine registry never retires an entry: entries for deleted stores stay in it, marked active.
- The export verbs take no `--cut`: a historical cut with drafts over it has no export.
