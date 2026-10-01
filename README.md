# theourgia

A block-graph store that agents and people write into together: the `theourgia` command,
its daemon, and an MCP shell, packaged for npm. Licensed under the Apache
License, Version 2.0 (see LICENSE); the igropyr library it ships with is
under the same license (vendor/igropyr/LICENSE).

## What it installs

- `theourgia`: the command you run. It answers through a daemon and starts
  one beside itself when none is running.
- `theourgia-mcp`: the MCP shell, speaking the protocol on its standard
  input and output.
- `theourgiad`: the daemon program, started by the other two.

The package carries the sources of Theourgia 1.0.0 and of the igropyr
revision it was released with. It does not carry Chez Scheme or libuv.

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

Node 18 or later runs the three small wrappers; there are no npm
dependencies.

## Platforms

macOS on arm64, and Linux on x86_64 and aarch64 with glibc. macOS on Intel
(or under Rosetta) is refused by name. FreeBSD has a measured row in the
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

and removing that directory is safe: the next run builds it again.
`npm install --ignore-scripts` works the same way, leaving the build to
the first run.

## Building this package

The package's sources live on the `npm` branch of the Theourgia
repository. `vendor/` is not committed; `sh scripts/assemble.sh` fills it
from the release tag and the pinned igropyr revision, and refuses if
either does not resolve to the expected commit. Then `npm pack`.
