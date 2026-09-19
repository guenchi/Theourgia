"""Where a Python fixture finds the core, the library path and its scratch.

THE FIXTURES ARE NOT RUN BY `run-fixtures.sh`. It runs `*.sc`; these are
`*.py`, started by hand, and `RUN.md` says how. That is why their paths
went wrong quietly: nothing in the suite's exit status covers them.

THREE DIRECTORIES, AND THEY ARE NOT THE SAME ONE. They were all derived
from `Path(__file__).resolve().parents[2]`, which in the tree these
fixtures were written in held `theourgia/`, `igropyr/` and
`implementation/` side by side:

  * the CORE sources -- `cli.sc`, `daemon.sc`, `mcp/server.sc`. That is
    `parents[1]`, always, and it does not depend on what sits beside the
    repository.
  * the LIBRARY PATH handed to Chez. `THEOURGIA_LIBDIR` when the caller
    pinned one, exactly as `env.sh` does for the Scheme fixtures, so a
    run can be a reading of a known export rather than of whatever the
    working trees hold this minute.
  * a SCRATCH directory. It was `<parents[2]>/.build`, which does not
    exist outside that one tree -- `mkdtemp` then raised before the
    first row.

AND THE EVIDENCE GOES WHERE THE RUN'S OTHER OUTPUT GOES. Five of these
fixtures ended by writing a log into `<parents[2]>/implementation/`,
a directory belonging to a different repository; `daemon` had already
printed 25 `ok` rows and `daemon-lifecycle` 9 when they died on that
line, so the failure was the report of a run that had succeeded.
"""
from pathlib import Path
import os
import sys
import tempfile

_here = Path(__file__).resolve().parent


def core():
    """The directory holding cli.sc, daemon.sc and mcp/."""
    return _here.parent


# THE SUFFIXES ARE THE ONES THE CALLERS ACTUALLY SEARCH FOR. Every
# fixture here sets CHEZSCHEMELIBEXTS to `.sc::.no-obj`, so
# a directory whose libraries are all `.sls` would satisfy a wider test
# and still resolve nothing for these runs.
_LIBRARY_SUFFIXES = ('.sc',)


def _source_dirs(value):
    """The SOURCE directories in a Chez library path.

    A SEARCH PATH MAY HOLD SEVERAL DIRECTORIES, and `env.sh` forwards
    this value to CHEZSCHEMELIBDIRS unchanged, so the check has to read
    it the way Chez does. Two readings were wrong before this one:

      * treating the whole string as one directory refused
        `/tmp:/Users/guenchi/Workshop`, which Chez resolves perfectly;
      * splitting on every separator and keeping the non-empty pieces
        accepted `/tmp::/Users/guenchi/Workshop`, which Chez does NOT
        resolve: a doubled separator makes the second half an OBJECT
        directory for the first, not a source directory of its own.
        Measured -- Chez reported `(("/tmp" . "/Users/guenchi/Workshop"))`
        and the import failed.
    """
    parts = str(value).split(os.pathsep)
    out, i = [], 0
    while i < len(parts):
        source = parts[i]
        if i + 2 < len(parts) and parts[i + 1] == '':
            i += 3
        else:
            i += 1
        # AN EMPTY COMPONENT IS THE CURRENT DIRECTORY, not nothing. Chez
        # resolves `/tmp:` against `.` as its second entry, and dropping
        # the empty piece made this refuse a pin Chez imports from.
        out.append(Path(source) if source else Path('.'))
    return out


def _holds_libraries(directory):
    """Whether `theourgia/` under this directory could answer an import.

    EXISTENCE IS NOT ENOUGH: a plain file named `theourgia`, or an empty
    directory of that name, satisfies `exists()` and resolves nothing.
    """
    here = directory / 'theourgia'
    if not here.is_dir():
        return False
    # IT ASKS FOR THE LIBRARY THE CORE IMPORTS FIRST, BY NAME AND SIZE.
    # `cli.sc` imports `(theourgia rpc)` before anything else, so a
    # directory without a non-empty `rpc` source cannot start a run
    # whatever else is in it. `is_file()` matters: a DIRECTORY named
    # `rpc.sc` has the suffix too.
    #
    # NOTE: IT DOES NOT READ THE FILE, ON PURPOSE. A version of this matched
    # `(library (theourgia rpc)` with a regular expression, and a regular
    # expression is the wrong instrument for Scheme in both directions at
    # once: it accepted the declaration written inside a line comment, a
    # block comment, a datum comment and a string -- none of which
    # declares anything -- and it refused two that Chez accepts, one with
    # a comment between `theourgia` and `rpc` and one carrying a version,
    # `(library (theourgia rpc (1)) ...)`. Both directions were measured.
    # A check that is wrong both ways is worse than a smaller one whose
    # limit is written down.
    #
    # NOTE: SO THIS IS A PRE-FLIGHT, NOT A PROOF. `rpc.sc` itself imports
    # `(theourgia store)`, `(theourgia reduce)` and `(theourgia log)`;
    # answering "will this import" means running Chez, which is the thing
    # this check exists to avoid doing once per row. What it rules out is
    # a directory that was never a library directory at all.
    for suffix in _LIBRARY_SUFFIXES:
        source = here / f'rpc{suffix}'
        try:
            if source.is_file() and source.stat().st_size > 0:
                return True
        except OSError:
            continue
    return False


def libdir():
    """The value to hand Chez as CHEZSCHEMELIBDIRS, verbatim.

    IT REFUSES A DIRECTORY THAT CANNOT RESOLVE `(theourgia ...)`. The
    default -- the grandparent of this file -- is right in the
    repository, where `theourgia/` sits beside it, and wrong inside a
    delivery, where the sources are under `core/`. Unchecked, that came
    back as `Exception: library (theourgia rpc) not found` from a child
    process, one row at a time, after the fixture had already built a
    store.
    """
    # THE PIN IS RETURNED AS IT WAS GIVEN. Running it through `Path`
    # normalised it, and normalising a SEARCH PATH changes what it means:
    # `/Users/guenchi/Workshop:/` came back as `/Users/guenchi/Workshop:`,
    # whose second entry Chez then read as `.` -- the current directory,
    # silently added to a pin whose whole purpose is to be exact.
    pinned = os.environ.get('THEOURGIA_LIBDIR')
    chosen = pinned if pinned else str(_here.parent.parent)
    if not any(_holds_libraries(p) for p in _source_dirs(chosen)):
        raise RuntimeError(
            f'no source directory in {chosen} holds a theourgia/ library '
            f'directory, so (theourgia ...) cannot be resolved from it; set '
            f'THEOURGIA_LIBDIR to one that does (see test/RUN.md)')
    return chosen


def _igropyr_in(directory):
    """Whether `igropyr/` under this directory can answer (igropyr crypto).

    The same shape as `_holds_libraries`, and for the same reason: a
    plain file or an empty directory of that name satisfies `exists()`
    and resolves nothing. `crypto.sc` is asked for because it is the
    first igropyr library the core reaches, through `digest.sc`.
    """
    here = directory / 'igropyr'
    if not here.is_dir():
        return False
    for suffix in ('.sc', '.sls'):
        source = here / f'crypto{suffix}'
        try:
            if source.is_file() and source.stat().st_size > 0:
                return True
        except OSError:
            continue
    return False


def igropyr():
    """The `igropyr/` directory of the library path this run is pinned to.

    A FIXTURE THAT STAGES ITS OWN COPY OF THE CORE NEEDS THIS. The core
    imports `(igropyr crypto)`, `(igropyr sexpr)` and `(igropyr
    platform)` through three facades of its own, so a staging directory
    holding only `theourgia/` resolves nothing past the first import.

    NOTE: IT DID NOT USED TO. `mcp-probe.py` carried a comment saying "the
    core imports no such library any more, so leaving one there would
    let a reintroduced dependency resolve and go unremarked" -- true of
    the tree it was written against, false since the copies became
    forwards, and the fixture failed with `cli.sc init` exiting 255.
    """
    for candidate in _source_dirs(libdir()):
        if _igropyr_in(candidate):
            return (candidate / 'igropyr').resolve()
    raise RuntimeError(
        f'no source directory in {libdir()} holds an igropyr/ with a crypto '
        f'source; the core cannot resolve (igropyr crypto) from it (see test/RUN.md)')


def test_root():
    """The directory a run may create things under, made if absent."""
    root = Path(os.environ.get('THEOURGIA_TEST_ROOT') or tempfile.gettempdir())
    root.mkdir(parents=True, exist_ok=True)
    return root


# A UNIX SOCKET PATH IS SHORTER THAN A FILE PATH, and that is what
# decides where a fixture may put its store. `sun_path` holds 104 bytes
# on this platform (108 on Linux); a store at
# `<scratch>/<prefix><8 random>/store/socket` has to fit inside it.
#
# MEASURED, by moving these fixtures to a scratch root 95 characters
# long: `connect` raised ENAMETOOLONG, which is not one of the three
# errnos the transport treats as "no daemon here", so every call came
# back `(error transport-unavailable)` and `mcp-probe` reported the core
# catalog as unavailable -- a sentence about the store, produced by the
# length of a directory name. The fixture that hit it had been written
# with `dir='/private/tmp'` hard-coded, which is the same discovery made
# once already and not written down.
SOCKET_LIMIT = 104
SOCKET_TAIL = len('/store/socket')


def _fits(root, prefix):
    """Whether <root>/<prefix><8 random>/store/socket fits in sun_path.

    THE BUDGET IS IN BYTES OF THE ABSOLUTE PATH, AND IT RESERVES THE NUL.
    Three ways the first version of this got it wrong, each measured by
    letting `socket.connect` answer:

      * `sun_path` holds 104 bytes INCLUDING the terminating NUL, so a
        path of exactly 104 is one too long; a 72-byte root was accepted
        and rejected by connect as 'AF_UNIX path too long'.
      * it counted characters. `'/tmp/' + 'X'*30` in a three-byte-per-
        character script is 95 bytes, not 35, and connect rejected the
        121-byte result.
      * it measured the root as given. A relative root passes any budget
        and `mkdtemp` hands back an absolute path, which is what the
        socket is opened on; `.` was accepted and produced 106 bytes.
    """
    full = Path(root).resolve() / (prefix + 'X' * 8)
    return len(str(full).encode(sys.getfilesystemencoding())) + SOCKET_TAIL + 1 <= SOCKET_LIMIT


def scratch(prefix):
    """A fresh directory for one fixture, short enough to hold a socket.

    The run's test root is preferred, so a run stays inside the place
    the caller pinned; when that root is too long for a socket path the
    system temporary directory is used instead, and the substitution is
    said on stderr rather than made silently.
    """
    requested = os.environ.get('THEOURGIA_TEST_ROOT')
    chosen = None
    for candidate in (requested, tempfile.gettempdir(), '/tmp'):
        if candidate and _fits(candidate, prefix):
            chosen = Path(candidate).resolve()
            break
    if chosen is None:
        raise RuntimeError(
            f'no directory is short enough to hold <dir>/{prefix}XXXXXXXX/store/socket '
            f'within the {SOCKET_LIMIT}-byte unix socket limit')
    # THE SUBSTITUTION IS REPORTED BY COMPARING RESOLVED PATHS. Compared
    # as strings, a root written with a trailing slash announced that it
    # had been replaced by itself.
    if requested and chosen != Path(requested).resolve():
        print(f'paths: THEOURGIA_TEST_ROOT ({len(str(Path(requested).resolve()))} bytes resolved) '
              f'cannot hold a unix socket path; using {chosen} for {prefix}*',
              file=sys.stderr)
    chosen.mkdir(parents=True, exist_ok=True)
    return Path(tempfile.mkdtemp(prefix=prefix, dir=chosen))


def evidence(name):
    """The path a fixture writes its transcript to, directory made.

    The path is returned rather than opened so the caller can name it in
    its own output: a transcript nobody can find is not evidence.
    """
    directory = test_root() / 'evidence'
    directory.mkdir(parents=True, exist_ok=True)
    return directory / name
