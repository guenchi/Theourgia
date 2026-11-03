# Changelog

## 1.3.3 — 2026-11-03

*6 commits.* `outline` answers the rows its text is printed from, so a program reads the outline as data, not by parsing it. An import with `--symbols` cuts a file where `split-suggest --symbols` cuts it, takes an editor's end to where code begins instead of refusing the file, and prints what it skipped and refused without `--wire`. `read --ast` gives every block node and every reference its byte span in the block's `src`, and a `raw-md` node its bytes as written and the references in it: a reader that pinned the old tree's shape must take the new one.

### Outline

- `outline` answers `(ok (text "...") (rows <row> ...) ...)`: the rows are the answer's second element, one per line of the text in the text's order, present even when there are none (`(rows)`), and every clause after them (`stale` and `via` with `--with-signatures`, the receipt, `working` with `--working`, the dispatcher's `incomplete`) keeps its order. A row is `(row <id> <kind> <mode> <path> <label> <depth> <mark> <orphan> <group> (conflicts (<field> ...)) [(keywords "<s>")] [(signature "<s>")])`: `kind`, `mode` and `path` are the block's view values, `#f` when absent and a contested field's `(conflict ...)` datum; `label` is what the line prints after the id; `depth` the line's level; `mark` the structural conflict's mark or `#f`; `orphan` `#t` for an orphan; `group` `tree` or `orphans`, the section the line is under; `conflicts` the fields whose value is contested, in the view's order. The `keywords` and `signature` clauses are there exactly when the line prints that column. The text is the one printed before, and the human form prints the text alone.

### Import

- `import-code`'s human form prints `skipped`, `symbols-ignored`, `symbols-imported-whole` and `symbols-refused` after the items, one line each as the wire writes them, so a run that refused every file says so; before, it printed nothing and answered rc 0. `import-code --datum` prints its `skipped` clause and `uses` its `(skipped (no-language <n>) (unreadable-code <n>))` counts the same way. The wire form is unchanged.
- With `--symbols` the starts are cut as `split-suggest --symbols` cuts them: the language's scanner takes each as a definition, the run of comment lines directly above it (a JSDoc) goes with it instead of becoming an untitled block of its own, and a cut at the end of the protected prefix is dropped.
- An end is never refused. It is taken to the first line start at or after it where the scanner sees code begin at the top level (not a blank line, not a comment line, not inside a statement, a string or a bracket), and it is a cut only when that line comes before the next block's start or the file's end. So the rest of a statement an editor's range stops inside (`const { a } = require('x') as {`, which an editor ends after the name) stays with its symbol, where before the file was refused `symbols-not-top-level`; the blank lines after a definition and a comment no definition takes go with the block above; and a block between two symbols holds code.
- A start the scanner does not see at the top level is refused as before, now naming the symbol: `(error symbols-not-top-level (at <n>) (symbol "<name>"))`. Where the definition patterns derive no name, a block's title is the editor's name of the first symbol whose start lies within it.
- The cuts, the ends and the titles are found in one pass over the symbols, and the scanner keeps the starts in a table rather than searching them on every line: in process, for 1000, 4000 and 8000 JSDoc'd functions of five lines, the cuts took 21, 355 and 1399 ms and take 7, 29 and 54 ms.

### `read --ast`

This changes the tree's shape. **Migration:** a reader of `--ast` that matched nodes by position takes a `(span <start> <end>)` after each block node's scalar elements and after a reference's target, and a `raw-md` node's text, span and `(refs ...)`; one that finds the first element headed `span` needs no other change. The README's `--ast` section is the reference.

- Every block node and every reference carries `(span <start> <end>)`: half-open byte offsets into the UTF-8 encoding of the block's `src`, right after the node's scalar elements and before its children. The block nodes are `(para (span s e) <inline> ...)`, `(list <ordered?> <start> (span s e) (item (span s e) <block> ...) ...)`, `(code <fence> <lang> "<text>" (span s e))`, `(quote (span s e) <block> ...)`, `(hr (span s e))` and `(raw-md "<text>" (span s e) (refs <ref> ...))`; a reference is `(ref "<lexeme>" "<key>" <target> (span s e))`. A block's span runs from the start of its first line to the end of its last line's terminator, container prefixes and interior lines included; a reference's from the first byte of `[[` to the byte after `]]`, counted on `src` as written, before escapes and entities are decoded. Strings, `em`, `strong`, `code`, `link` and `br` carry none.
- `raw-md` is verbatim: its text is exactly the bytes of its span, CRLF and every container prefix kept, where before it was the text with the prefixes taken off. Its `(refs ...)` are the references in it, found and resolved as in a paragraph (an indented code block's is empty); before, the tree held no reference inside a `raw-md`.
- Printed back with the spans left out, a parsed tree parses to the same tree, with two known exceptions the README names: an item whose line carries a marker of the other kind before its own, and an `em` or `strong` holding a literal `*` or `_` or nested in another (the parser closes it at the first eligible delimiter run). The printer writes a `raw-md` text as it is, separates two lists of the same kind by two blank lines so they read back as two, and leaves a line a code span or a reference begins unescaped.
- For a program that imports `(theourgia md)`: `md-refs` is gone; `md-ref-nodes` answers the parser's references in document order, each `(ref "<lexeme>" "<key>" (span s e))`. The `ref` relation and the references a block is linked from read it instead of lexing the text a second time.

### Tests

- New fixtures for the outline rows (27 rows over two stores, through a daemon, the CLI and the MCP shell) for the symbols import (21 rows, among them the cost of N and 4N symbols) and for the tree's spans (72 rows: byte endpoints on written sources, the round trip over the a1 corpus and over a 3087-source markdown corpus read when `THEOURGIA_MD_CORPUS_STORE` names a copy of it, and the cost of `refs` over it); client-start's start row builds its socket under the runner's socket root, which keeps the path within the socket length limit, and tmp-paths reads every Scheme file with Chez's reader to hold socket paths to that root; verb-registry's base takes import-code's new describe entry; the guard expect file is emptied by the landing that follows each one listing it.

## 1.3.2 — 2026-11-01

*14 commits.* Splitting and import with symbols reach `.tsx` and `.jsx`: two language entries whose scan follows JSX, and one ECMAScript function head shared by the four JavaScript and TypeScript entries, taking `export default` and `async`. Beside them: `read --ast` answers a block's body as a parsed tree, each reference resolved to the live block it names; a writer's working view holds its new files and hides its deleted ones (`outline` gains `--working`); `context` names every hard block it shows below full; and a client waiting on a daemon gives up on silence rather than on a clock, while the daemon keeps a working request alive.

### Languages

- `.tsx` (`typescriptreact`) and `.jsx` (`javascriptreact`) have language entries, named as the editor names them, with no runner. In them the scan follows JSX: an element where an operand is expected is read through its tags, its attribute strings (no escape character, and a string may span lines), its children and its `{...}` containers, nested elements and fragments included, with comments allowed between any two tokens of a tag; no line inside an element is a cut. The arrangement of a tag's tokens is not checked and closing tags match by depth, not by name. What the JSX scan does not follow makes the file one block with the warning `lexically-uncertain`, at the first offset any live reading meets it: in `.tsx`, type parameters where an operand is expected and a function type there; in both, type arguments on a tag name, an element as an attribute's value, a character no tag token starts with, a `<` followed by neither a name nor `>`, and `>` or `}` as text. An import with `--symbols` imports such a file whole and lists it under `symbols-imported-whole` as `symbols-unscanned`, with the scan's reason. Plain JavaScript and TypeScript are scanned as before.
- The four entries that read JavaScript or TypeScript share one function head: a declaration may be exported, exported as the default, and `async`, so `export default function f(`, `export async function f(` and `export default async function f(` derive the name `f`, and such a line is a cut for `split-suggest`. Where no name is derived (in TypeScript a class or a const; in JavaScript or TypeScript an `export default` of anything but a function), an import with `--symbols` still titles the block with the editor's name for the symbol, a title later edits do not update.

### Read

- `read <id> --ast` answers `(ok (block <id> <entries> (hash <h>) (ast <node> ...)) <validity> <receipt>)`: the plain read's record after its id, the block's version, and its body as the tree the markdown projection builds, for a block of a kind it writes (`doc`, `section`, `decision`, `task`). The block nodes are `para`, `list`, `code`, `quote`, `hr` and `raw-md` (which keeps whole what the parser does not take apart: a table, an HTML block, a footnote, an indented code block); the inline nodes are strings, `em`, `strong`, `code`, `link`, `br` and `(ref "<lexeme>" "<key>" <target>)`, the target the id of the live block the reference names, resolved by the bracket text with its `.md` kept and then by the key, each with a leading `ent:` removed, so `[[X]]`, `[[X.md]]`, `[[X|shown]]` and `[[ent:X]]` all name a live `X`; `#f` when none is live. With `--recursive` the items come in `read --recursive`'s order; `--cut` reads the tree as it was at the cut, references included. A deleted block, or a block of another kind, answers `(ast #f)`; a `src` that is not text (bytes that are not UTF-8, or a value in conflict), `(ast #f (note src-not-text))`. `--ast` with `--md`, `--working`, `--working-info` or `--signature` is refused `(error bad-request incompatible-ast-options)`. The grammar grows: source spans on every node and resolution by a document's name follow, and a reader pins the shape it read.

### Working views

- A writer's working view holds its new files and hides its deleted ones. `read --working`, `context --working`, `outline --working` and an `eval --working` in another language than Scheme read a new file as the blocks its commit would make: the holder, named by the provisional id, placed after the last top-level holder, and its pieces `<provisional>/<K>` in K order, split as the commit splits the file; several new files follow the order `drafts` lists them. `read --working` answers the holder `""` and a piece as its bytes, and `--working-info` on either names the version the view gives it, with `based-on` `#f`. A deleted file's holder and the children its commit deletes are absent, and asked for with `--working` are `(error unknown-id <id> (reason deleted-in-working) (holder <holder>))`. `context --working` can take a piece of a new file as its target, and a committed block that uses a name the piece defines finds it among that name's definitions. A provisional id asked over any other view (without `--working`, in another writer's view, after the commit) is still `(error provisional-id (id "<id>") (reason not-committed))`.
- `outline --working` prints the request writer's working view and ends the answer with `(working "<writer>")`; a refusal of the view is the answer; its signatures are still the committed store's, so a new file's pieces print none. `drafts` lists the structural drafts after the revoked consumptions, by key: the deletes, then the new files by provisional id.

### Context

- `insufficient` gains a `reduced` list, `(reduced ((<id> <level> (full <bytes>)) ...) [(more <n>)] [(capped)])`, naming every hard block shown below full, the target first and the rest in hard order, with the level it is shown at and the length its entry would have at full; twenty are named and the rest counted, and a length above 999999 is shown at 999999 with `(capped)` at the list's end. The clause is absent exactly when every hard block is shown in full, so a target shown at summary always says so. The envelope reserves the list from the request and the hard set alone; the minimum budget of a one-block request grows by the clause holding one item, about twenty tokens.

### Daemon and clients

- A client waiting on a daemon gives up on silence, not on a clock: when nothing has arrived for 30 s since the last byte that moved, it answers `(error transport-unknown (reason silent) (window 30000) (elapsed <ms>))`; the request may have been carried out and is never sent again; a send that expires with nothing written is `not-sent`. There is no total bound: while a request's work reports progress (`export-code` per file gathered, projected and written; `import-code` per file read and captured; a commit per intent applied; a fold per segment validated and delivered) the daemon writes one keepalive space every 5 s, which the client strips before the answer's first byte. A verb or step with no progress point, one unit of work longer than 30 s, and a request queued behind another still go silent after 30 s. A subscription gets no keepalive.
- A client asks for keepalives by sending envelope version 2; version 1 gets the same bytes as before and none. The command line, `channel`, the MCP shell and the stream client send version 2. A daemon that predates version 2 refuses the frame with `(error bad-request (reason malformed-envelope))` and closes, and on exactly that refusal the client sends the same request once more at version 1 on a new connection; this daemon refuses a version-2 frame it cannot use with `(version 2)` in the refusal, never retried. The MCP shell answers a tool call whose daemon went silent with the JSON-RPC error `-32603` and the data object `{"reason":"silent","window":<ms>,"elapsed":<ms>}`.

### Tests

- Fixtures verify a pid found by `ps`, read from a pid file or taken from `$!` before they signal it, through one shared include, and a static audit fixture holds each converted file's remaining bare sends to a declared list; fixture helpers judge an insert answer's head before reading it; the change-stream guard judges the base's executor lines instead of counting none; the excluded clause's cap in `context` and its `(capped)` marker have rows that can go red, the answer unchanged; the guard expect file is emptied by the landing that follows the one listing it.

## 1.3.1 — 2026-10-30

*31 commits.* Splitting and import with symbols on real TypeScript and JavaScript: the ECMAScript scan no longer falls back to one block at a division, a regular expression, a template or a generic (on a real TypeScript tree of 132 files, 28 were split by symbol before and all 132 after), a file the scan cannot check is imported whole instead of being left out, a symbol-cut block carries the symbol's name where the language derives none, and JSX-looking text in strings, comments and regular expressions no longer makes plain JavaScript fall back. Beside it: the daemon yields during blocking file I/O, `context` lists the users of a unit's names, the review channel keeps every text of a packet section, a failed commit says why, and the store tools refuse a few wrong landings by name.

### Import with symbols

- Splitting and `import --symbols`: the ECMAScript scan keeps every reading the text has not disproved (a `/` read both as a division and as a regular expression, template and interpolation frames, TypeScript's postfix `!` and type-argument `>`) and takes a line as top level only when every surviving reading does, so divisions, regular expressions, templates and generics no longer make a file fall back with `lexically-uncertain`; past 32 readings the file falls back as before.
- A file the scan cannot check (no language entry, or text the scanner does not follow) is imported whole and listed under `symbols-imported-whole`; a file whose cut was checked and found wrong is still left out, under `symbols-refused`.
- A symbol-cut block the language does not name is titled by the symbol the editor gave.
- JSX in plain JavaScript is refused by position, not by text: a `<` where an operand is expected is an `opener`, and a `/` read as a division whose next token is `>` is a `self-close`; markup in strings, comments, templates and regular expressions passes.
- In the table scan of the other languages, a comment no longer ends the statement it follows.

### Daemon

- Blocking file I/O yields to the scheduler; a probe defers while a publication is in progress.
- A public name spelled like another endpoint's lock is refused.

### Context and working views

- `context` gains a `users` clause after `background`: the live code blocks outside the unit that use a name it defines, each listed whole or counted under the excluded counter `users`; the clause is reserved at the counters' digits, so which hard blocks are shown still depends on the hard material only.
- A draft new file's provisional id is answered `(error provisional-id (id ..) (reason not-committed))` by every verb that resolves an id over a view; with `--working`, a refusal of the view still comes first.
- A fold that fails at a named cut is answered in its own class (a busy lock as `store-busy`), not as an unusable cut; `context`'s description says that under `--working` a drafted block's premises are the working view's versions.

### Store and store tools

- A failed commit's answer carries a `(failed ...)` clause naming its cause, where it said only `commit-barrier-failed`.
- `publish-landing` refuses a landing whose datum libraries changed and names a library with no path by its id; a freeze stays current when nothing that lands has changed; `landing-freeze --tree` compares the freeze with the tested edit directory.
- `import-git` follows the domain for a range instead of refusing a binary.
- `supply` and `init` keep an existing `.gitignore` and refuse one that is not a file; `supply --clear` writes nothing it need not; the edit tools find the edit directory's sidecar by one lexical fold.

### Review channel

- A packet section may carry several `(text ...)` entries: joined in packet order with one empty line; `(text)` and `(text #f)` carry none; a non-string text refuses the packet as `packet-malformed`.

### Tests

- The change-stream guard runs real daemons on a real seed and reads answers whole; the guard names the daemon program by a define the spawn reading sees; two timing rows wait for the event, not a clock; the scan-cost row reads medians over three pairs; the two-opener daemon rows read the held loser by pid; the two context base rows leave the users clause out of both sides; eval-local binds its runs before their probes; signal!'s identity guard has its own rows; the mcp-shell stub appends its log line before it answers.

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
