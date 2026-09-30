#!/bin/sh
# Fills vendor/ with the two sources this package ships: theourgia at its
# release tag and igropyr at the revision the release was measured with.
# Both come from `git archive` of the exact commits; test/, any compiled
# object and any .gitignore are left out (npm never packs a file by that
# name, so leaving it here would make vendor/ differ from what ships). Each
# pin must resolve to the expected commit, or nothing is written: vendor/
# is built in a temporary sibling and moved into
# place only when both are complete, and an existing vendor/ is removed
# first, so a refusal leaves no vendor/ at all.
#
# usage: sh scripts/assemble.sh
#
#   THEOURGIA_REPO  a clone of guenchi/Theourgia holding the tag (default:
#                   the repository this script is checked out in)
#   IGROPYR_REPO    a clone of guenchi/Igropyr, or its URL (default:
#                   https://github.com/guenchi/Igropyr)
#   THEOURGIA_TAG, THEOURGIA_COMMIT, IGROPYR_REV
#                   the pins; the defaults are this release's, and the
#                   variables exist so the refusal can be tested
set -eu

here=$(cd "$(dirname "$0")/.." && pwd)
tag=${THEOURGIA_TAG:-v1.0.0}
want_theourgia=${THEOURGIA_COMMIT:-f50525f8e343ad072b57aeea5986af9c51d78a99}
igropyr_rev=${IGROPYR_REV:-56ca0db9c8bb1c32bafa1e1472852a6186ada31b}
want_igropyr=56ca0db9c8bb1c32bafa1e1472852a6186ada31b
theourgia_repo=${THEOURGIA_REPO:-$here}
igropyr_repo=${IGROPYR_REPO:-https://github.com/guenchi/Igropyr}

refuse() {
  echo "assemble: $*" >&2
  exit 1
}

rm -rf "$here/vendor"
work=$(mktemp -d "$here/.vendor.XXXXXX")
trap 'rm -rf "$work"' EXIT

got_theourgia=$(git -C "$theourgia_repo" rev-parse --verify --quiet "$tag^{commit}" 2>/dev/null) ||
  refuse "tag $tag does not resolve in $theourgia_repo"
[ "$got_theourgia" = "$want_theourgia" ] ||
  refuse "tag $tag is $got_theourgia, not the release commit $want_theourgia"

case "$igropyr_repo" in
  http*|git@*)
    git clone --quiet "$igropyr_repo" "$work/igropyr-repo" || refuse "cannot clone $igropyr_repo"
    igropyr_repo="$work/igropyr-repo" ;;
esac
got_igropyr=$(git -C "$igropyr_repo" rev-parse --verify --quiet "$igropyr_rev^{commit}" 2>/dev/null) ||
  refuse "igropyr revision $igropyr_rev does not resolve in $igropyr_repo"
[ "$got_igropyr" = "$want_igropyr" ] ||
  refuse "igropyr revision $igropyr_rev is $got_igropyr, not the pinned commit $want_igropyr"

mkdir -p "$work/vendor/theourgia" "$work/vendor/igropyr"
git -C "$theourgia_repo" archive --format=tar "$got_theourgia" | tar -x -C "$work/vendor/theourgia"
git -C "$igropyr_repo" archive --format=tar "$got_igropyr" | tar -x -C "$work/vendor/igropyr"
rm -rf "$work/vendor/theourgia/test" "$work/vendor/igropyr/test"
find "$work/vendor" \( -name '*.so' -o -name .gitignore \) -type f -exec rm -f {} +

for f in theourgia/theourgia.sc theourgia/build.ss theourgia/LICENSE igropyr/LICENSE; do
  [ -f "$work/vendor/$f" ] || refuse "the assembled tree has no $f"
done
printf '%s %s\n%s %s\n' theourgia "$got_theourgia" igropyr "$got_igropyr" > "$work/vendor/PINS"

mv "$work/vendor" "$here/vendor"
echo "assemble: vendor/ holds theourgia $tag ($got_theourgia) and igropyr $got_igropyr" >&2
