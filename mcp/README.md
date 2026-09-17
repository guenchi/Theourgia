Run `python3 theourgia/mcp/server.py --store /absolute/store` as a local stdio
server. A running daemon is preferred; pre-send connection failure starts the
one-shot core CLI worker. No SDK dependency or network service is installed.

The shell implements the pinned MCP
[2025-11-25 lifecycle](https://modelcontextprotocol.io/specification/2025-11-25/basic/lifecycle),
[stdio framing](https://modelcontextprotocol.io/specification/2025-11-25/basic/transports)
and [tool envelopes](https://modelcontextprotocol.io/specification/2025-11-25/server/tools).
Every tool accepts `{ "argv": ["string", "arguments"] }`. The tool's job is to
return a core command answer, so even a core refusal is successful text content
with `isError: false`. Transport/protocol failures use JSON-RPC errors. Unknown
names, including eval, take the same unknown-tool path. Eval is local CLI only.

The catalog comes from the connected worker's `rpc-verbs` at each list/call;
there is no hand-maintained verb list. Arguments are passed as strings, with no
shell interpolation or business validation. Output text retains every byte and
its final newline. Notifications have no response. Calls are processed serially;
EOF drains the current bounded request, then exits. Cancellation does not revoke
or automatically retry a core write.

Input frames are limited to 1 MiB. The shared transport limits collected answers
to 32 MiB, including its hex envelope overhead, and waits at most 30 seconds.
Exceeding an output/time limit is an explicit transport error; no prefix is
reported as a complete success, and execution may already have occurred.
Pagination and HTTP transport are outside this stdio adapter.
