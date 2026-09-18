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

## Tools

One per verb in `rpc-verbs`, asked again on every call — a verb added to
the core is a tool this shell can see without a restart.

* name: `theourgia_<verb>`. A verb of `[A-Za-z0-9_.-]` keeps its
  spelling; anything else, and anything already starting `x_`, is carried
  as `theourgia_x_<hex>`. The `x_` exclusion is what keeps the mapping
  injective. A name over 128 characters is left out of the catalogue.
* arguments: `{"argv": ["…"]}`, `additionalProperties: false`. Every item
  must be a string; each reaches the core byte for byte, and nothing on
  the way hands them to a shell.
* `eval` is not a tool. Calling it is indistinguishable from calling a
  verb that does not exist.

## Two routes, one answer

With a daemon on the socket the request goes over it in the envelope
`request-frame` packs — the same one the command line sends, from the
same procedure. Without one it is dispatched in this process. No child
process on either path, and the two answers are identical.

A socket path with nobody behind it means the request reached nobody, so
it is run locally. A connection that was made and then lost is different:
the request may have been carried out, so it is **not** re-run and the
client is told the answer could not be obtained.

## Framing

One JSON object per line on stdin, one per line on stdout. The input
limit is 1 MiB **per frame, counting the terminator**; a frame past it is
refused and the shell exits non-zero. A request split across writes — in
the middle of a UTF-8 sequence included — is reassembled; two requests in
one write are answered separately. Notifications are never answered.
Calls are served one at a time. At EOF the call in flight is answered
before the shell exits.
