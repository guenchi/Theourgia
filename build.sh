#!/bin/sh
# Render this site out of the theourgia store in store/.
#
# Every page is a procedure in the store's "site generator" library block,
# and every word a page prints comes from a block in the same store. This
# script knows neither: it asks the store for the library, evaluates one
# procedure per page, and writes the string it gets back to a file. Adding
# a page means adding a def and one line below.
#
# Needs: the theourgia client and Chez Scheme on PATH. Override with
# THEOURGIA, SCHEME and STORE in the environment.
set -e
cd "$(dirname "$0")"

THEOURGIA=${THEOURGIA:-theourgia}
SCHEME=${SCHEME:-scheme}
STORE=${STORE:-store}
# Absolute, always. A daemon identifies the store it serves by the path
# STRING it was asked for, while the client picks its socket by the
# resolved path -- so a client saying "store" reaches the daemon already
# serving "/.../store" and is answered (error transport-store-mismatch).
# The build then fails only when somebody else has the store open, which
# is the worst way for it to fail.
if [ ! -d "$STORE" ]; then
    echo "build.sh: no store directory at $STORE" >&2
    exit 1
fi
STORE=$(cd "$STORE" && pwd)
TMP=${TMPDIR:-/tmp}/theourgia-ws-$$
trap 'rm -f "$TMP"' EXIT

# Asked twice on purpose. A read through a running daemon can answer from
# the state it held a moment ago -- measured: the first read after a write
# made on the local path misses it and the second sees it -- so a single
# lookup can come back empty for a block that is there. Two empty answers
# mean the block really is absent.
find_lib() {
    "$THEOURGIA" outline --store "$STORE" 2>/dev/null |
        awk '$3 == "site" && $4 == "generator" { print $2; exit }'
}
LIB=$(find_lib)
[ -n "$LIB" ] || LIB=$(find_lib)
if [ -z "$LIB" ]; then
    echo "build.sh: no block titled 'site generator' in $STORE" >&2
    exit 1
fi
echo "generator: $LIB"

# render <procedure> <output file>
render() {
    "$THEOURGIA" eval --store "$STORE" --under "$LIB" "($1)" \
        --output-bytes 1048576 --timeout-ms 20000 --wire 2>/dev/null > "$TMP"
    printf '%s\n' "(let ((a (with-input-from-file \"$TMP\" read)))
       (if (and (pair? a) (eq? (car a) 'ok))
           (call-with-port (open-file-output-port \"$2\"
                             (file-options no-fail)
                             (buffer-mode block)
                             (native-transcoder))
             (lambda (p) (display (car (cadr (cadr a))) p)))
           (begin (display \"build.sh: $1 answered \" (current-error-port))
                  (write a (current-error-port))
                  (newline (current-error-port))
                  (exit 1))))" | "$SCHEME" -q
    echo "  $2 ($(wc -c < "$2" | tr -d ' ') bytes)"
}

render page-index      index.html
render page-why        why.html
render page-manual     manual.html
render manual-md       manual.md
render page-agents     agents.html
render page-changelog  changelog.html
render changelog-md    changelog.md
render favicon         favicon.svg
# .gitignore is rendered too, from the same block the home page's
# concurrency section shows, so the file and the page cannot drift apart.
render gitignore-file  .gitignore
