# Theourgia

A block-graph store where agents and people write together: the `theourgia` command,
its daemon, and an MCP shell, packaged for npm. Licensed under the Apache
License, Version 2.0 (see LICENSE). It depends on the igropyr package,
under the same license.

## What it installs

- `theourgia`: the command you run. It answers through a daemon and starts
  one beside itself when none is running.
- `theourgia-mcp`: the MCP shell, speaking the protocol on its standard
  input and output.
- `theourgiad`: the daemon program, started by the other two.

The package carries the sources of Theourgia 1.2.0, and installs igropyr
1.8.2 from npm as its one dependency. It does not carry Chez Scheme or
libuv.

## Requirements

Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1, and libuv. Older
distribution packages of Chez will be refused: the datum printer is
measured on these versions only, and the programs say so with
`(error unsupported-printer-version)` on any other.

- macOS: `brew install chezscheme libuv`
- Debian, Ubuntu: `sudo apt install chezscheme libuv1`
- Fedora: `sudo dnf install chez-scheme libuv`
- Arch: `sudo pacman -S chez-scheme libuv`

The programs look for Chez as `THEOURGIA_SCHEME` if it is set, and
otherwise as `scheme`, `chez` (Homebrew's name) or `chezscheme` on `PATH`.
Whichever is found is the one the daemon and every evaluation run under.

Node 18 or later runs the three small wrappers. The one npm dependency is
igropyr, the library the core is written on.

## Platforms

macOS on arm64, and Linux on x86_64 and aarch64 with glibc. macOS on Intel
(or under Rosetta) is refused by name, with a sentence naming the open item:
igropyr looks for libuv only under Homebrew's arm64 prefix. FreeBSD has a measured row in the
core, but its daemon does not start there yet; it is expected in a later
release.

## Installing

    npm install -g theourgia
    theourgia init --store ~/notes
    theourgia insert --title "a first block" --store ~/notes
    theourgia outline --store ~/notes

For an MCP client, the server command is `theourgia-mcp` (with
`--store <path>` naming the store).

## The compiled objects

The programs run from compiled objects, which start about twelve times
faster than the sources. They are built once for each Chez binary, on
install when Chez is already there, or else on the first run, which says
so on standard error and prints nothing on standard output. They live in

    ${XDG_CACHE_HOME:-$HOME/.cache}/theourgia/<version>-<machine type>-<key>/

where the key covers the theourgia commit, igropyr's version and the Chez
binary, so a change to any of them builds again. Removing that directory is
safe: the next run builds it again.
`npm install --ignore-scripts` works the same way, leaving the build to
the first run.

## Building this package

The package's own files live on the `npm` branch of the Theourgia
repository, and the sources it carries are not copied there: `theourgia/`
is a git submodule, a pointer to a commit of the same repository (GitHub
shows it as `theourgia @ <commit>`). After a fresh clone:

    git submodule update --init

A release takes three steps:

    git -C theourgia checkout <tag>        # move the pointer
    git add theourgia && git commit        # record it on the branch
    npm version <new version> && npm publish

`npm publish` first runs a gate that refuses, each by name, a submodule
that is not checked out, one at a commit other than the one the branch
records, a dirty submodule or branch, a compiled object anywhere, and a
packed file list that differs from `test/expected-files.txt`. It fixes
nothing. Packing writes `source.json`, the submodule's commit, which the
installed package reads for its compile cache. That cache's key covers the
package version, the submodule commit, igropyr's version, and the Chez
binary's version and machine type; it does not read the sources' contents,
so a dependency edited by hand without a change of version is not detected.
