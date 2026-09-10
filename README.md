# theourgia

Agent-and-human shared block-graph knowledge base. R6RS Scheme; Chez Scheme is the reference platform, and the portable core is meant to compile to wasm and to move to other implementations. Platform-specific code (file primitives, locks) lives in its own library.

Licensed under the Apache License, Version 2.0. See LICENSE.

Design documents are maintained separately; this repository holds the implementation and its tests.

Run the suites from source (igropyr must be a sibling checkout):

    sh test/run-all.sh
