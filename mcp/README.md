# The MCP shell

`theourgia-mcp --store <path> [--socket <path>] [--actor <name>]` speaks
MCP `2025-11-25` over stdio, in front of the same dispatcher the command
line uses.

    scheme --script mcp/server.ss --store /path/to/store

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
verb. ⛔ It is not written here and not written in the shell: it is
`write-protocol`, exported by `(theourgia rpc)`, and the README's
`## Writing for agents` section is the same string. An agent choosing a
tool from `tools/list` reads the description and nothing else, so the
rules for writing a block have to be in it.

⚠️ Only those two tools carry it. It is about writing a block, and on
`read` or `search` it would be noise in the place an agent is choosing
from.

## Before you start it

⚠️ **Create the store first.** This shell has no local route: everything it
lists, it sends to that store's daemon, and a daemon for a store that does not
exist cannot start. Run

    theourgia init --store <path>

once, by hand or from the host, before starting the shell. `init` is
deliberately **not** offered as a tool — see below.

## Tools

One per verb the core's `describe` reports as routed to the daemon, asked
again on every call — a verb added to the core is a tool this shell can see
without a restart.

⛔ **A verb this shell cannot carry out is not offered.** `describe` marks each
verb `daemon` or `local`; the ones marked `local` are the ones a client runs
in its own process, and this shell has no such route. `init` is the case that
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
* `eval` is not a tool. Calling it is indistinguishable from calling a
  verb that does not exist.

## One route, and what happens when it is not there

Every request goes over the socket in the envelope `request-frame` packs —
the same one the command line sends, from the same procedure.

⚠️ **This shell no longer dispatches in its own process.** It used to, when it
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
