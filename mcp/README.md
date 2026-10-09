# The MCP shell

`theourgia-mcp --store <path> [--socket <path>] [--actor <name>] [--writer <name>]`
speaks MCP `2025-11-25` over stdio, in front of the same dispatcher the
command line uses. These are the options the shell reads; the command
line's common options (`--req`, `--cursor`, `--wire`) and the daemon's
(`--log`, `--attempt`, `--detach`) are parsed too, as they always were,
and change nothing here.

    scheme --script mcp/server.sc --store /path/to/store

## Whose drafts

Every request the shell sends carries a writer, and it is decided once,
when the shell starts: `THEOURGIA_WRITER` when it is set and not empty;
otherwise `--writer <name>`; otherwise a writer derived for this session,
`<actor>-<start>-<pid>`. `<actor>` is the shell's actor in lower case,
every character a writer id may not hold made `-`, runs of `-` made one,
`-` and `.` taken off both ends, `agent` if nothing is left, at most 40
characters; `<start>` is the moment the shell started, in milliseconds,
in base 36; `<pid>` is the shell's process id. For example
`guenchi-m0k3f1a2-84389`. A host configures nothing per session unless it
wants a fixed name.

The writer is said to the client: `initialize`'s `instructions` ends with
"This session's writer is <w>; drafts left by an earlier session are read
with `drafts --writer <that session's writer>`." A tool call's own
`--writer` overrides the session's writer for that call and only that
call. A later session takes over an earlier one's drafts by starting with
`THEOURGIA_WRITER=<that name>` (or `--writer`).

A writer given by the environment or by `--writer` is checked when the
shell starts, the one not chosen too: a name a writer cannot have (1 to
128 characters of `a-z`, `0-9`, `.`, `_`, `-`, and not `.` or `..`), or
`--writer` without a value, answers the usage line on stderr and exit 2
before any frame is read. An empty `THEOURGIA_WRITER` counts as unset.

NOTE: Two sessions alive at once differ by pid, among processes sharing
one pid namespace (one machine, outside containers, in general). A
derived name repeats only if a pid is reused in the same millisecond of a
clock that went back -- after a reboot with the clock reset, say -- and
then the new session binds the old session's drafts. That is the limit,
stated rather than claimed away.

## What a tool returns

The core's answer, as S-expression **text**, byte for byte. The JSON is
the JSON-RPC envelope and nothing else: this shell does not read the
answer, reformat it, or decide anything from it.

A **core refusal is a successful result** — `(error unknown-id …)` is the
answer to the question that was asked, so it comes back with
`isError: false`. Only the shell's own failures use the JSON-RPC error
channel: a frame it could not parse, a frame past the limit, or a daemon
that took the request and then lost the answer. Translating core
refusals into MCP errors would tell a client its command could not be
run when it ran and was refused.

The shell branches on the **transport's tag**, never on the answer's
text: a core answer whose text happens to read `(error transport-unknown
…)` is still an answer.

## Writing for agents

The `theourgia_insert` and `theourgia_write` tools carry the core's
write protocol in their `description`, ahead of the sentence naming the
verb. NEVER: It is not written here and not written in the shell: it is
`write-protocol`, exported by `(theourgia rpc)`, and the README's
`## Writing for agents` section is the same string. An agent choosing a
tool from `tools/list` reads the description and nothing else, so the
rules for writing a block have to be in it.

NOTE: Only those two tools carry it. It is about writing a block, and on
`read` or `search` it would be noise in the place an agent is choosing
from.

## Before you start it

NOTE: **Create the store first.** This shell has no local route: everything it
lists but its child verbs (eval, scope and collect), it sends to that store's
daemon -- and it asks that daemon for the tool list even before an eval --
and a daemon for a store that does not exist cannot start. Run

    theourgia init --store <path>

once, by hand or from the host, before starting the shell. `init` is
deliberately **not** offered as a tool — see below.

## Tools

One per verb the core's `describe` reports as routed to the daemon or to a
child, asked again on every call — a verb added to the core is a tool this
shell can see without a restart.

NEVER: **A verb this shell cannot carry out is not offered.** `describe` marks each
verb `daemon`, `local` or `child`. `daemon` verbs are sent to the store's
server; the `child` verbs, `eval`, `scope` and `collect`, are run as this
shell's own child (see "Eval" below); the ones marked `local` are the ones a client runs in its
own process, and this shell has no such route. `init` is the case that
matters: offered as a tool it could never succeed, because it is what creates
the store there would otherwise be no daemon for — and an agent reading the
tool list would have been told a capability existed.

* name: `theourgia_<verb>`. A verb of `[A-Za-z0-9_.-]` keeps its
  spelling; anything else, and anything already starting `x_`, is carried
  as `theourgia_x_<hex>`. The `x_` exclusion is what keeps the mapping
  injective. A name over 128 characters is left out of the catalogue.
* arguments: `{"argv": ["…"]}`, `additionalProperties: false`. Every item
  must be a string; each reaches the core byte for byte, and nothing on
  the way hands them to a shell.
* `eval` is a tool, `theourgia_eval`, carried out as this shell's child
  (see "Eval").
* `scope` and `collect` are tools too, carried out the same way: the child
  is `core.sc scope` or `core.sc collect`, a client of every store it
  touches, never the daemon. Its environment carries this shell's actor
  (`THEOURGIA_ACTOR`) and daemon socket (`THEOURGIA_SOCKET`), so its
  requests are this shell's. Name `<dir>` by an absolute path: the child
  runs in a directory of this shell's own.

## Eval

`theourgia_eval` runs `core.sc eval` as a CHILD of this shell, the same
program the command line execs for `theourgia eval`, and returns its answer
byte for byte. The daemon never runs user code, and the shell loads no core.

* **The child's argv is the caller's, verbatim and alone**:
  `<THEOURGIA_SCHEME or scheme> --script <core.sc beside this program> <verb>
  <argv>`, the verb being the tool's (`eval`, `scope`, `collect`). The
  store, the wire mode, this session's writer and this shell's actor and
  daemon socket travel in its environment -- `THEOURGIA_STORE`,
  `THEOURGIA_WIRE=1`, `THEOURGIA_WRITER=<the session's writer>`,
  `THEOURGIA_ACTOR` and `THEOURGIA_SOCKET`, each replacing any binding of
  that name -- so a parse refusal is the one the command line gives for the
  same argv. A caller's own `--writer v` still wins. Its cwd is the shell's.
* **The transport is not the caller's to name.** An argv that parses with
  an option `--store`, `--actor`, `--wire` or `--socket` answers
  `(error bad-request transport-option-in-rpc (option "<option>") (belongs-to
  shell))`, naming the first one written, as a result and runs nothing -- the
  same datum the daemon's route answers. Every tool's description says the
  tool is bound to one store and actor, and the query tool's shows a goal
  form, `(score ?b "<text>" ?s)`. Positional text that only spells one,
  and anything after `--`, is not refused. A NUL byte in an argument or in
  `stdin` answers `(error bad-request nul-in-argument)` and runs nothing.
* **Its input is the call's.** The tool's `stdin` becomes its standard
  input; without one it reads `/dev/null`. It never reads this shell's own
  input.
* **The gate is this shell's environment.** A language other than Scheme
  runs only if the shell was started with `THEOURGIA_RUNNERS=on` -- the
  host's server configuration, the operator's word for that client. No
  argument, source text or envelope field can set it, and the daemon's
  environment is never consulted.
* **The quotas are core.sc's**, `--timeout-ms`, `--memory-bytes` and
  `--output-bytes`, with its bounds and defaults. The shell waits for the
  child for twice the evaluation's timeout plus a preparation allowance of
  70 s -- a chosen allowance, not a measured bound -- and polls it every
  25 ms. `THEOURGIA_MCP_PREPARATION_MS` replaces the allowance (a positive
  integer; a test seam).

What comes back:

* the child exited 0 or 1 with a non-empty answer that is UTF-8: that
  answer as the tool's text, `isError: false`, whatever it says;
* an empty answer, another exit code, a signal, bytes that are not UTF-8,
  or an answer that could not be read: the JSON-RPC error "Core answer
  unavailable; execution may be unknown", with `data: {reason, diag}` --
  `reason` one of `exit <n>`, `signal <s>`, `empty-answer`, `not-utf8`,
  `answer-unreadable`, `wait-failed <errno>`, `signal-failed <errno>`,
  `deadline <ms>`, `deadline-unreaped <ms>`, and `diag` the last 4096 bytes
  of the child's standard error, or null when they cannot be read or are
  not UTF-8. It is never re-sent. The child's standard error is never part
  of an answer.
* nothing ran: `core.sc` missing (`core-missing`), a program that could not
  be started (`spawn-failed`), or no call directory (`scratch-unavailable`)
  -- the "not sent" error.

NEVER: **What a deadline leaves.** At the deadline the child is killed with
SIGKILL and its status polled for 2 s. The supervisor dies without running
its exits, so its worker's or runner's process group is not signalled and its
scratch is not removed: a CPU-bound survivor ends at its CPU ceiling, if the
kernel accepted one; a sleeping or blocked one has no finite bound from here;
a descendant that left the group had none before either. A child whose status
was not collected is collected by a later call's reaping, once it has exited;
without another call there is no promise it is.

**The call's files.** Each call claims a directory
`<run root>/<store key>/mcp-<shell pid>-<n>` with mode 0700, trying the next
`<n>` when the name is taken, up to 8; its stdin, answer and diagnostics
files are there, and all of it is removed on every outcome. A removal that
fails is written to the shell's own stderr -- not a channel to the host --
and the directory is left.

Calls are served one at a time, eval included: the next frame is read after
the call has ended -- when the child's status was collected, or when the
shell stopped owning it (a failed wait or signal, or a deadline it could not
see through) -- so EOF is seen after that. A child the shell stopped owning
may still be running; a host that closes the shell abruptly likewise leaves
the child to its own bounds.

## One route to a daemon, and what happens when it is not there

Every request but eval's goes over the socket in the envelope `request-frame` packs —
the same one the command line sends, from the same procedure.

NOTE: **This shell no longer dispatches in its own process.** It used to, when it
could not reach a daemon, which meant every shell loaded the whole core to
serve its first call — the cost the client/server split exists to avoid. With
nothing listening it now *starts* a daemon, exactly as the command line does.

Three outcomes, and they are deliberately different sentences:

* **the request reached nobody and a daemon started** — it is sent, once.
* **the request reached nobody and a daemon would not start** — a JSON-RPC
  error carrying *the server's own reason*, and saying explicitly that the
  request was not carried out. Nothing was sent, so nothing ran.
* **the connection was made and then lost** — the request may have been
  carried out, so it is **not** re-sent and the caller is told the answer
  could not be obtained. This is the only case that says "may be unknown",
  and it says so because it is the only case where it is true.

## Framing

One JSON object per line on stdin, one per line on stdout. The input
limit is 1 MiB **per frame, counting the terminator**; a frame past it is
refused and the shell exits non-zero. A request split across writes — in
the middle of a UTF-8 sequence included — is reassembled; two requests in
one write are answered separately. Notifications are never answered.
Calls are served one at a time. At EOF the call in flight is answered
before the shell exits.
