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
TMP=${TMPDIR:-/tmp}/theourgia-ws-$$
trap 'rm -f "$TMP"' EXIT

LIB=$("$THEOURGIA" outline --store "$STORE" 2>/dev/null |
      awk '$3 == "site" && $4 == "generator" { print $2; exit }')
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
render page-model      model.html
render page-concurrency concurrency.html
render page-agents     agents.html
render page-reference  reference.html
render page-changelog  changelog.html
render favicon         favicon.svg
# .gitignore is rendered too, from the same block the concurrency page
# shows, so the file and the page cannot drift apart.
render gitignore-file  .gitignore
