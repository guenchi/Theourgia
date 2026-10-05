#!/bin/bash
# The package's cells (the brief's "Cells v2"). Each cell runs in its own
# scratch HOME, cache, prefix and store, prints one PASS or FAIL line with
# what it read, and keeps its raw outputs under $CELLS_OUT. A daemon a cell
# starts is stopped by the end of the cell.
#
# usage: bash test/cells.sh [tarball]
#   CELLS       the cells to run, e.g. "N2 N2b N6 N10" (default: all)
#   CELLS_OUT   where the readings go (default: ./readings)
#   CELLS_CHEZ  the real Chez binary (default: the first of scheme, chez,
#               chezscheme on PATH)
#   The package's sources are the submodule theourgia/ (a checkout of the
#   commit this branch records); igropyr comes from npm as a dependency.
set -u

here=$(cd "$(dirname "$0")/.." && pwd)
CELLS=${CELLS:-"P1 N1 N1b P2 N2 N2b P3 N3 N3bc N4 N4b N5 N5b N5c N6 N7 N7b N8 P4 N10 P5 N11 N12 N13 N15"}
R=${CELLS_OUT:-$here/readings}
# NEVER UNDER $TMPDIR: on macOS it is a long path under /var/folders, and a
# store's socket under a scratch HOME there passes the 104-byte limit of a
# socket's address, which the core refuses by name (socket-path-too-long).
W=$(mktemp -d /tmp/tc.XXXXXX)
mkdir -p "$R"
NODE=$(command -v node)
NPM=$(command -v npm)
REAL=${CELLS_CHEZ:-$(command -v scheme || command -v chez || command -v chezscheme)}
TGZ=${1:-}
fails=0

say() { echo "$*" | tee -a "$R/SUMMARY.txt"; }
verdict() {
  if [ "$1" = 0 ]; then say "PASS $2"; else say "FAIL $2"; fails=$((fails + 1)); fi
}
mtime() { perl -e 'print((stat $ARGV[0])[9])' "$1"; }
stop_daemons() {
  pkill -f "theourgiad.sc serve $W" 2>/dev/null
  sleep 1
  pkill -9 -f "theourgiad.sc serve $W" 2>/dev/null
  true
}
# a directory holding node and the named links or shims, nothing else
bindir() {
  local d=$W/bin.$1; mkdir -p "$d"; ln -sf "$NODE" "$d/node"; echo "$d"
}
# a shim that records its argv (one argument per line, between markers)
# and optionally its THEOURGIA_SCHEME, then execs the real Chez
shim() {
  local path=$1 log=$2
  cat > "$path" <<EOF
#!/bin/sh
{ echo "=== \$0 THEOURGIA_SCHEME=\${THEOURGIA_SCHEME:-unset}"; for a in "\$@"; do printf '[%s]\n' "\$a"; done; } >> "$log"
exec "$REAL" "\$@"
EOF
  chmod +x "$path"
}
# a scratch HOME and cache for one cell; exports them
scratch() {
  T=$W/$1; mkdir -p "$T/home"
  export HOME=$T/home XDG_CACHE_HOME=$T/cache
  unset THEOURGIA_SCHEME
}
install_into() {
  local prefix=$1; shift
  "$NPM" install -g --prefix "$prefix" "$@" "$TGZ" > "$prefix.install.out" 2> "$prefix.install.err"
}
has() { case " $CELLS " in *" $1 "*) return 0;; *) return 1;; esac; }

say "cells start $(date '+%H:%M:%S') on $(uname -sm); node $("$NODE" --version); npm $(npm --version); chez $REAL $("$REAL" --version 2>&1)"

if [ -z "$TGZ" ]; then
  (cd "$here" && npm pack --pack-destination "$W" > "$R/pack.out" 2> "$R/pack.err")
  TGZ=$W/theourgia-1.1.0.tgz
fi
[ -f "$TGZ" ] || { say "FAIL no tarball"; exit 1; }
PKGBIN_DIR=$(bindir chez); ln -sf "$REAL" "$PKGBIN_DIR/$(basename "$REAL")"
# NO SYSTEM DIRECTORY ON A CELL'S PATH, because one could hold a Chez of its
# own and "no Chez" or "only chez" would not mean what it says. The base is
# a directory of links to the named tools the cells and npm use, checked to
# hold no Chez by any of the three names. npm is called by its absolute path.
BASEPATH=$W/sys; mkdir -p "$BASEPATH"
for t in sh bash env ls cat grep awk sed tr wc head tail cut sort uniq tee xargs find rm mkdir chmod ln cp mv \
         touch stat date seq cmp diff sleep ps pgrep pkill kill perl git tar gzip dirname basename readlink uname \
         printf test expr id true false; do
  p=$(PATH=/usr/bin:/bin:/usr/sbin:/sbin command -v "$t" 2>/dev/null)
  case "$p" in /*) ln -sf "$p" "$BASEPATH/$t";; esac
done
for n in scheme chez chezscheme; do
  [ -e "$BASEPATH/$n" ] && { echo "FAIL the cells' base PATH holds $n"; exit 1; }
done

# ---- N1 inventory: the packed list equals test/expected-files.txt; package.json
if has N1; then
  tar -tzf "$TGZ" | sed 's#^package/##' | LC_ALL=C sort > "$R/N1-files.txt"
  tar -xzOf "$TGZ" package/package.json > "$W/n1-package.json"
  diff "$here/test/expected-files.txt" "$R/N1-files.txt" > "$R/N1-diff.txt"; d=$?
  node -e '
    const p = require(process.argv[1]);
    const files = require("fs").readFileSync(process.argv[2], "utf8").split("\n");
    const want = (c, m) => { if (!c) { console.log("package.json: " + m); process.exitCode = 1; } };
    want(p.name === "theourgia", "name");
    want(p.version === "1.1.0", "version");
    want(JSON.stringify(p.os) === JSON.stringify(["darwin", "linux"]), "os");
    want(JSON.stringify(p.cpu) === JSON.stringify(["arm64", "x64"]), "cpu");
    want(p.engines && p.engines.node === ">=18", "engines");
    const bins = Object.entries(p.bin || {});
    want(bins.length === 3 && bins.every(([k, f]) => files.includes(f)), "bin: three, each to a packed file");
    want(p.bin.theourgia === "bin/theourgia.js" && p.bin["theourgia-mcp"] === "bin/theourgia-mcp.js" &&
      p.bin.theourgiad === "bin/theourgiad.js", "bin names");
    want(p.scripts && p.scripts.postinstall === "node scripts/postinstall.js", "scripts.postinstall");
    want(p.scripts && p.scripts.prepublishOnly === "node scripts/prepublish.js", "scripts.prepublishOnly");
    want(p.scripts && p.scripts.prepack === "node scripts/source-pin.js", "scripts.prepack");
    want(JSON.stringify(p.dependencies) === JSON.stringify({ igropyr: "1.8.2" }), "dependencies: igropyr 1.8.2, exactly");
    for (const k of ["devDependencies", "optionalDependencies", "peerDependencies",
      "bundleDependencies", "bundledDependencies"]) want(!(k in p), "no " + k);
    want(files.includes("README.md") && files.includes("LICENSE"), "README.md and LICENSE at the root");
    want(p.homepage === "https://theourgia.dev", "homepage");
    want(p.bugs === "https://github.com/guenchi/Theourgia/issues", "bugs");
    want(p.repository && p.repository.type === "git" &&
      p.repository.url === "git+https://github.com/guenchi/Theourgia.git", "repository");
  ' "$W/n1-package.json" "$R/N1-files.txt" > "$R/N1-package.txt"; j=$?
  verdict $((d + j)) "N1 inventory: $(wc -l < "$R/N1-files.txt" | tr -d ' ') files, list diff $(wc -l < "$R/N1-diff.txt" | tr -d ' ') lines; package.json $( [ $j = 0 ] && echo as specified || cat "$R/N1-package.txt" | tr '\n' ' '); tarball $(wc -c < "$TGZ" | tr -d ' ') bytes"
fi

# ---- N1b the packed sources: md5 for md5 against a git archive of the commit the
# branch records for theourgia/, filtered as `files` filters it; source.json
# names that commit; and no igropyr is carried.
if has N1b; then
  X=$W/n1b; mkdir -p "$X/pkg" "$X/t"
  tar -xzf "$TGZ" -C "$X/pkg"
  recorded=$(git -C "$here" ls-tree HEAD theourgia | awk '{print $3}')
  git -C "$here/theourgia" archive "$recorded" | tar -x -C "$X/t"
  (cd "$X/t" && find . -type f | sed 's#^\./##' | grep -v -E '^[^/]+\.sc$|^build\.ss$|^LICENSE$|^README\.md$|^mcp/' | while read -r f; do rm -f "$f"; done)
  find "$X/t" -type d -empty -delete
  sums() { (cd "$1" && find . -type f | LC_ALL=C sort | while read -r f; do echo "$(node -e 'process.stdout.write(require("crypto").createHash("md5").update(require("fs").readFileSync(process.argv[1])).digest("hex"))' "$f") $f"; done); }
  sums "$X/pkg/package/theourgia" > "$R/N1b-packed.md5"; sums "$X/t" > "$R/N1b-archive.md5"
  diff "$R/N1b-archive.md5" "$R/N1b-packed.md5" > "$R/N1b-diff.txt"; a=$?
  pinned=$(node -e 'process.stdout.write(String(require(process.argv[1]).theourgia))' "$X/pkg/package/source.json" 2>/dev/null)
  [ "$pinned" = "$recorded" ]; b=$?
  igr=$(find "$X/pkg/package" -name 'crypto.sc' -path '*igropyr*' | wc -l | tr -d ' ')
  bad=$(cat "$R/N1b-packed.md5" "$R/N1b-archive.md5" | grep -cv '^[0-9a-f]\{32\} ')
  [ "$bad" = 0 ] && [ -s "$R/N1b-packed.md5" ] && [ "$igr" = 0 ] && [ ${#recorded} = 40 ]; c=$?
  verdict $((a + b + c)) "N1b packed sources: theourgia/ $(wc -l < "$R/N1b-packed.md5" | tr -d ' ') files vs the archive of $recorded $(wc -l < "$R/N1b-archive.md5" | tr -d ' '), diff $(wc -l < "$R/N1b-diff.txt" | tr -d ' ') lines; source.json names ${pinned:-nothing}; igropyr files carried $igr"
fi

# ---- P1 a fresh clone of the branch, its submodule initialised: npm pack --dry-run
# lists test/expected-files.txt exactly (source.json is written by prepack),
# with no test/, no vendor/ and no .so -- and the submodule's files at all.
if has P1; then
  X=$W/p1; git clone -q "$here" "$X/clone"
  git -C "$X/clone" config submodule.theourgia.url "$here/theourgia"
  git -C "$X/clone" -c protocol.file.allow=always submodule update --init -q > "$R/P1-submodule.out" 2>&1
  (cd "$X/clone" && "$NPM" pack --dry-run --json > "$R/P1-pack.json" 2> "$R/P1-pack.err"); prc=$?
  node -e 'for (const f of JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"))[0].files) console.log(f.path)' "$R/P1-pack.json" | LC_ALL=C sort > "$R/P1-files.txt"
  diff "$here/test/expected-files.txt" "$R/P1-files.txt" > "$R/P1-diff.txt"; d=$?
  sub=$(grep -c '^theourgia/' "$R/P1-files.txt"); forbidden=$(grep -c -E '(^|/)test/|^vendor/|\.so$' "$R/P1-files.txt")
  [ $prc = 0 ] && [ $d = 0 ] && [ "$sub" -gt 50 ] && [ "$forbidden" = 0 ]; v=$?
  verdict $v "P1 pack from a fresh clone: rc $prc, $(wc -l < "$R/P1-files.txt" | tr -d ' ') files, list diff $(wc -l < "$R/P1-diff.txt" | tr -d ' ') lines; theourgia/ files $sub; test/, vendor/ or .so $forbidden"
fi

# ---- P2 an install into an empty prefix pulls igropyr 1.8.2 from npm, found from
# the package as require finds it, and a store answers; the second run
# compiles nothing (N4 reads the cache's mtimes in full).
if has P2; then
  scratch p2; P=$T/prefix
  (export PATH=$(dirname "$NODE"):$PKGBIN_DIR:$BASEPATH; install_into "$P"); irc=$?
  ver=$(node -e 'const m = require.resolve("igropyr/package.json", { paths: [process.argv[1]] }); process.stdout.write(require(m).version + " " + m)' "$P/lib/node_modules/theourgia" 2>/dev/null)
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > "$R/P2-first.out" 2> "$R/P2-first.err"; echo $? > "$R/P2-first.rc"
   "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/P2-second.out" 2> "$R/P2-second.err"; echo $? > "$R/P2-second.rc")
  b2=$(grep -c 'compiling once' "$R/P2-second.err")
  a1=$(grep -c '(ok (store' "$R/P2-first.out"); a2=$(grep -c '^(ok' "$R/P2-second.out")
  [ $irc = 0 ] && [ "${ver%% *}" = 1.8.2 ] && [ "$(cat "$R/P2-first.rc")" = 0 ] && [ "$(cat "$R/P2-second.rc")" = 0 ] && [ "$b2" = 0 ] &&
    [ "$a1" = 1 ] && [ "$a2" = 1 ]; v=$?
  verdict $v "P2 install: rc $irc; igropyr $ver; init rc $(cat "$R/P2-first.rc") answered $a1, outline rc $(cat "$R/P2-second.rc") answered $a2, builds on the second run $b2"
  stop_daemons
fi

# ---- N2 install and use, and N2b the daemon program, with a PATH of the prefix and Chez only
if has N2 || has N2b; then
  scratch n2; P=$T/prefix
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH; install_into "$P"); irc=$?
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > "$R/N2-init.out" 2> "$R/N2-init.err"
   "$P/bin/theourgia" insert --title N2-MARKER --store "$T/s" > "$R/N2-insert.out" 2> "$R/N2-insert.err"
   "$P/bin/theourgia" outline --store "$T/s" > "$R/N2-outline.out" 2> "$R/N2-outline.err"; echo $? > "$R/N2-outline.rc")
  grep -q N2-MARKER "$R/N2-outline.out" && [ "$(cat "$R/N2-outline.rc")" = 0 ]; m=$?
  has N2 && verdict $((irc + m)) "N2 install and use: install rc $irc, outline rc $(cat "$R/N2-outline.rc"), the block $( [ $m = 0 ] && echo shown || echo MISSING)"
  stop_daemons
  if has N2b; then
    (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
     "$P/bin/theourgia" init --store "$T/d" > /dev/null 2>&1
     "$P/bin/theourgiad" serve "$T/d" > "$R/N2b-daemon.out" 2> "$R/N2b-daemon.err" & wrapper=$!
     for i in $(seq 1 100); do [ -S "$(ls -d "$HOME/.theourgia/run/"*/socket 2>/dev/null | head -1)" ] && break; sleep 0.1; done
     child=$(pgrep -P "$wrapper" | head -1)
     "$P/bin/theourgia" outline --store "$T/d" > "$R/N2b-outline.out" 2> "$R/N2b-outline.err"; echo $? > "$R/N2b-outline.rc"
     ps -ax -ww -o pid= -o command= | grep "theourgiad.sc serve $T/d" | grep -v grep > "$R/N2b-ps.txt"
     alive=no; kill -0 "$wrapper" 2>/dev/null && alive=yes
     echo "$wrapper ${child:-none} $alive" > "$R/N2b-pids.txt")
    read -r wpid cpid alive < "$R/N2b-pids.txt"
    n=$(wc -l < "$R/N2b-ps.txt" | tr -d ' '); dpid=$(awk '{print $1}' "$R/N2b-ps.txt" | head -1)
    grep -q "/theourgia/theourgiad.sc serve $T/d" "$R/N2b-ps.txt" && [ "$n" = 1 ] && [ "$dpid" = "$cpid" ] && [ "$alive" = yes ] &&
      [ "$(cat "$R/N2b-outline.rc")" = 0 ]; v=$?
    verdict $v "N2b theourgiad: outline rc $(cat "$R/N2b-outline.rc") through it; daemons for the store $n, pid $dpid, the theourgiad wrapper's child $cpid, the wrapper still running $alive; ps: $(head -1 "$R/N2b-ps.txt" | sed 's/^ *[0-9]* //' | cut -c1-140)"
    stop_daemons
  fi
fi

# ---- P3 nested and hoisted: a local install leaves igropyr hoisted beside the
# package; moved under the package's own node_modules it is nested. Each
# layout, with its own cache, builds and answers -- and the hoisted copy is
# gone in the nested run, so the build cannot have read it.
if has P3; then
  for layout in hoisted nested; do
    scratch p3-$layout; D=$T/project; mkdir -p "$D"
    (cd "$D" && export PATH=$(dirname "$NODE"):$PKGBIN_DIR:$BASEPATH && "$NPM" install --ignore-scripts "$TGZ" > "$T/install.out" 2> "$T/install.err"); echo $? > "$R/P3-$layout-install.rc"
    if [ $layout = nested ]; then
      mkdir -p "$D/node_modules/theourgia/node_modules"
      mv "$D/node_modules/igropyr" "$D/node_modules/theourgia/node_modules/igropyr"
    fi
    where=$(cd "$D" && find node_modules -name package.json -path '*igropyr/package.json' -not -path '*/igropyr/*/*' | tr '\n' ' ')
    (export PATH=$D/node_modules/.bin:$PKGBIN_DIR:$BASEPATH
     "$D/node_modules/.bin/theourgia" init --store "$T/s" > "$R/P3-$layout.out" 2> "$R/P3-$layout.err"; echo $? > "$R/P3-$layout.rc")
    echo "$layout install rc $(cat "$R/P3-$layout-install.rc"), igropyr at $where, init rc $(cat "$R/P3-$layout.rc"), built $(grep -c 'compiling once' "$R/P3-$layout.err")" >> "$R/P3.txt"
    stop_daemons
  done
  [ "$(cat "$R/P3-hoisted.rc")" = 0 ] && [ "$(cat "$R/P3-nested.rc")" = 0 ] &&
    [ "$(grep -c '(ok (store' "$R/P3-hoisted.out")" = 1 ] && [ "$(grep -c '(ok (store' "$R/P3-nested.out")" = 1 ] &&
    [ "$(grep -c 'compiling once' "$R/P3-hoisted.err")" = 1 ] && [ "$(grep -c 'compiling once' "$R/P3-nested.err")" = 1 ] &&
    grep -q '^hoisted .*igropyr at node_modules/igropyr/package.json ' "$R/P3.txt" &&
    grep -q '^nested .*igropyr at node_modules/theourgia/node_modules/igropyr/package.json ' "$R/P3.txt"; v=$?
  verdict $v "P3 layouts: $(tr '\n' ';' < "$R/P3.txt")"
fi

# ---- N3 no Chez: exit 69, the exact sentence, stdout empty; postinstall exit 0, same stderr
if has N3; then
  scratch n3; P=$T/prefix; install_into "$P" --ignore-scripts
  B=$(bindir n3)
  (export PATH=$B:$BASEPATH
   "$P/bin/theourgia" outline --store "$T/s" > "$R/N3-run.out" 2> "$R/N3-run.err"; echo $? > "$R/N3-run.rc"
   node "$P/lib/node_modules/theourgia/scripts/postinstall.js" > "$R/N3-post.out" 2> "$R/N3-post.err"; echo $? > "$R/N3-post.rc")
  if [ "$(uname -s)" = Darwin ]; then line='brew install chezscheme libuv'; else line=$(node -e 'console.log(require(process.argv[1]).installLineFor("linux", require("fs").readFileSync("/etc/os-release","utf8")))' "$P/lib/node_modules/theourgia/lib/theourgia.js"); fi
  printf '%s\n' "theourgia: Chez Scheme was not found (THEOURGIA_SCHEME is unset, and none of scheme, chez or chezscheme is on PATH); install Chez Scheme and libuv, for example: $line" \
    "theourgia: Theourgia needs Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1; older distribution packages will be refused." > "$R/N3-expected.err"
  cmp -s "$R/N3-expected.err" "$R/N3-run.err"; a=$?
  cmp -s "$R/N3-expected.err" "$R/N3-post.err"; b=$?
  [ "$(cat "$R/N3-run.rc")" = 69 ] && [ "$(cat "$R/N3-post.rc")" = 0 ] && [ ! -s "$R/N3-run.out" ] && [ ! -s "$R/N3-post.out" ]; c=$?
  verdict $((a + b + c)) "N3 no Chez: run rc $(cat "$R/N3-run.rc") stderr $( [ $a = 0 ] && echo equals || echo DIFFERS) stdout $(wc -c < "$R/N3-run.out" | tr -d ' ') bytes; postinstall rc $(cat "$R/N3-post.rc") stderr $( [ $b = 0 ] && echo equals || echo DIFFERS)"
fi

# ---- N3b, N3c the pure functions
if has N3bc; then
  node "$here/test/unit.js" > "$R/N3bc-unit.txt" 2>&1; u=$?
  verdict $u "N3b N3c install lines and platform refusal: $(tail -1 "$R/N3bc-unit.txt"), $(grep -c '^ok' "$R/N3bc-unit.txt") cases"
fi

# ---- N4 --ignore-scripts: the first run builds, the second does not touch the cache
if has N4 || has N5; then
  scratch n4; P4=$W/n4/prefix; install_into "$P4" --ignore-scripts
fi
if has N4; then
  T=$W/n4
  before=$(ls "$XDG_CACHE_HOME/theourgia" 2>/dev/null | wc -l | tr -d ' ')
  (export PATH=$P4/bin:$PKGBIN_DIR:$BASEPATH
   "$P4/bin/theourgia" init --store "$T/s" > "$R/N4-first.out" 2> "$R/N4-first.err"; echo $? > "$R/N4-first.rc")
  (cd "$XDG_CACHE_HOME/theourgia" && find . -type f -exec perl -e 'for (@ARGV) { print "$_ ", (stat $_)[9], "\n" }' {} + | LC_ALL=C sort) > "$R/N4-mtimes-1.txt"
  sleep 1
  (export PATH=$P4/bin:$PKGBIN_DIR:$BASEPATH
   "$P4/bin/theourgia" outline --wire --store "$T/s" > "$R/N4-second.out" 2> "$R/N4-second.err"; echo $? > "$R/N4-second.rc")
  (cd "$XDG_CACHE_HOME/theourgia" && find . -type f -exec perl -e 'for (@ARGV) { print "$_ ", (stat $_)[9], "\n" }' {} + | LC_ALL=C sort) > "$R/N4-mtimes-2.txt"
  b1=$(grep -c 'compiling once' "$R/N4-first.err"); b2=$(grep -c 'compiling once' "$R/N4-second.err")
  cmp -s "$R/N4-mtimes-1.txt" "$R/N4-mtimes-2.txt"; same=$?
  nfiles=$(wc -l < "$R/N4-mtimes-1.txt" | tr -d ' ')
  [ "$before" = 0 ] && [ "$b1" = 1 ] && [ "$b2" = 0 ] && [ $same = 0 ] && [ "$nfiles" -ge 100 ] &&
    [ "$(cat "$R/N4-first.rc")" = 0 ] && [ "$(cat "$R/N4-second.rc")" = 0 ] &&
    grep -q '(ok (store' "$R/N4-first.out" && grep -q '^(ok' "$R/N4-second.out"; v=$?
  verdict $v "N4 --ignore-scripts: cache before $before; first run built $b1, rc $(cat "$R/N4-first.rc"); second built $b2, rc $(cat "$R/N4-second.rc"); $(wc -l < "$R/N4-mtimes-1.txt" | tr -d ' ') cache files, mtimes $( [ $same = 0 ] && echo unchanged || echo CHANGED)"
  stop_daemons
fi

# ---- N4b postinstall warms: the cache exists before any wrapper call
if has N4b; then
  scratch n4b; P=$T/prefix
  (export PATH=$(dirname "$NODE"):$PKGBIN_DIR:$BASEPATH; install_into "$P"); irc=$?
  n=$(ls "$XDG_CACHE_HOME/theourgia" 2>/dev/null | grep -v '^\.' | wc -l | tr -d ' ')
  [ $irc = 0 ] && [ "$n" = 1 ]; v=$?
  verdict $v "N4b postinstall warms: install rc $irc; cache directories before any run: $n"
fi

# ---- N5 two first runs at once on an empty cache
if has N5; then
  scratch n5
  (export PATH=$P4/bin:$PKGBIN_DIR:$BASEPATH
   "$P4/bin/theourgia" init --store "$T/a" > "$R/N5-a.out" 2> "$R/N5-a.err" & pa=$!
   "$P4/bin/theourgia" init --store "$T/b" > "$R/N5-b.out" 2> "$R/N5-b.err" & pb=$!
   wait $pa; echo $? > "$R/N5-a.rc"; wait $pb; echo $? > "$R/N5-b.rc")
  dirs=$(ls "$XDG_CACHE_HOME/theourgia" | wc -l | tr -d ' '); tmps=$(ls -a "$XDG_CACHE_HOME/theourgia" | grep -c -E '^\.(build|lib)-')
  oka=$(grep -c '(ok' "$R/N5-a.out"); okb=$(grep -c '(ok' "$R/N5-b.out")
  [ "$(cat "$R/N5-a.rc")" = 0 ] && [ "$(cat "$R/N5-b.rc")" = 0 ] && [ "$oka" = 1 ] && [ "$okb" = 1 ] && [ "$dirs" = 1 ] && [ "$tmps" = 0 ]; v=$?
  verdict $v "N5 two first runs: rc $(cat "$R/N5-a.rc") and $(cat "$R/N5-b.rc"), answers ok $oka and $okb; builds announced $(cat "$R/N5-a.err" "$R/N5-b.err" | grep -c 'compiling once'); cache directories $dirs; temporary left $tmps"
  stop_daemons
fi

# ---- N5b a builder killed mid-build leaves no final directory; the next run builds
if has N5b; then
  scratch n5b; P=$T/prefix; install_into "$P" --ignore-scripts
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > "$R/N5b-killed.out" 2> "$R/N5b-killed.err" & wrapper=$!
   seen=no
   for i in $(seq 1 400); do
     if ls -a "$XDG_CACHE_HOME/theourgia" 2>/dev/null | grep -q '^\.build-'; then
       # matched on the scratch directory's name: Node resolves the
       # package's path, so on macOS the builder's argv says /private/tmp.
       # The match is on build.ss's own path, inside the package: its first
       # argument is the temporary library root in the cache.
       b=$(pgrep -f "/$(basename "$W")/n5b/prefix/.*build\.ss" | head -1)
       if [ -n "$b" ]; then kill -9 "$b" "$wrapper"; seen=yes; break; fi
     fi
     sleep 0.02
   done
   wait $wrapper 2>/dev/null
   echo "$seen" > "$R/N5b-seen.txt"
   ls -a "$XDG_CACHE_HOME/theourgia" > "$R/N5b-after-kill.txt"
   "$P/bin/theourgia" init --store "$T/s2" > "$R/N5b-next.out" 2> "$R/N5b-next.err"; echo $? > "$R/N5b-next.rc")
  finals=$(grep -v '^\.' "$R/N5b-after-kill.txt" | wc -l | tr -d ' ')
  [ "$(cat "$R/N5b-seen.txt")" = yes ] && [ "$finals" = 0 ] && [ "$(cat "$R/N5b-next.rc")" = 0 ] && grep -q '(ok' "$R/N5b-next.out" &&
    [ "$(grep -c 'compiling once' "$R/N5b-next.err")" = 1 ]; v=$?
  verdict $v "N5b interrupted build: killed mid-build $(cat "$R/N5b-seen.txt"); final directories after the kill $finals ($(grep '^\.build-' "$R/N5b-after-kill.txt" | wc -l | tr -d ' ') temporary); next run rc $(cat "$R/N5b-next.rc"), built $(grep -c 'compiling once' "$R/N5b-next.err")"
  stop_daemons
fi

# ---- N5c a build that fails: exit 70, its last lines, no final directory; recovery
if has N5c; then
  scratch n5c; P=$T/prefix; install_into "$P" --ignore-scripts
  cat > "$T/badchez" <<EOF
#!/bin/sh
case "\$*" in
  *build.ss*) echo "N5C-SHIM-FIRST-LINE"; echo "N5C-SHIM-LAST-LINE: the build was made to fail" >&2; exit 3 ;;
esac
exec "$REAL" "\$@"
EOF
  chmod +x "$T/badchez"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   THEOURGIA_SCHEME=$T/badchez "$P/bin/theourgia" init --store "$T/s" > "$R/N5c-fail.out" 2> "$R/N5c-fail.err"; echo $? > "$R/N5c-fail.rc"
   ls -a "$XDG_CACHE_HOME/theourgia" > "$R/N5c-after-fail.txt"
   "$P/bin/theourgia" init --store "$T/s" > "$R/N5c-next.out" 2> "$R/N5c-next.err"; echo $? > "$R/N5c-next.rc"
   rm -rf "$XDG_CACHE_HOME"
   THEOURGIA_SCHEME=$T/badchez node "$P/lib/node_modules/theourgia/scripts/postinstall.js" > "$R/N5c-post.out" 2> "$R/N5c-post.err"; echo $? > "$R/N5c-post.rc"
   "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/N5c-post-next.out" 2> "$R/N5c-post-next.err"; echo $? > "$R/N5c-post-next.rc")
  finals=$(grep -v '^\.' "$R/N5c-after-fail.txt" | wc -l | tr -d ' ')
  [ "$(cat "$R/N5c-fail.rc")" = 70 ] && grep -q 'N5C-SHIM-LAST-LINE' "$R/N5c-fail.err" && [ ! -s "$R/N5c-fail.out" ] && [ "$finals" = 0 ] &&
    [ "$(cat "$R/N5c-next.rc")" = 0 ] && grep -q '(ok (store' "$R/N5c-next.out" && [ "$(grep -c 'compiling once' "$R/N5c-next.err")" = 1 ] &&
    [ "$(cat "$R/N5c-post.rc")" = 0 ] && grep -q 'N5C-SHIM-LAST-LINE' "$R/N5c-post.err" && grep -q 'the first run will try again' "$R/N5c-post.err" &&
    [ ! -s "$R/N5c-post.out" ] &&
    [ "$(cat "$R/N5c-post-next.rc")" = 0 ] && grep -q '(ok' "$R/N5c-post-next.out" && [ "$(grep -c 'compiling once' "$R/N5c-post-next.err")" = 1 ]; v=$?
  verdict $v "N5c build failure: rc $(cat "$R/N5c-fail.rc"), the shim's last line $(grep -c 'N5C-SHIM-LAST-LINE' "$R/N5c-fail.err"), stdout $(wc -c < "$R/N5c-fail.out" | tr -d ' ') bytes, final directories $finals; next run rc $(cat "$R/N5c-next.rc"); postinstall rc $(cat "$R/N5c-post.rc"), then a run rc $(cat "$R/N5c-post-next.rc")"
  stop_daemons
fi

# ---- N6 MCP: initialize and an eval call; stdout only protocol lines; on a first run that builds
if has N6; then
  scratch n6; P=$T/prefix; install_into "$P" --ignore-scripts
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2>&1
   rm -rf "$XDG_CACHE_HOME"
   node "$here/test/mcp-talk.js" "$P/bin/theourgia-mcp" "$T/s" > "$R/N6.json" 2> "$R/N6.err")
  node -e '
    const r = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
    const ok = r.initialized && r.notJson.length === 0 && typeof r.evalText === "string" && r.evalText.includes("3") && r.evalIsError === false;
    console.log((ok ? "ok" : "bad") + " " + JSON.stringify({ initialized: r.initialized, stdoutLines: r.stdoutLines, notJson: r.notJson.length, evalText: r.evalText, isError: r.evalIsError }));
    process.exit(ok ? 0 : 1);
  ' "$R/N6.json" > "$R/N6.txt" 2>&1; v=$?
  [ "$(grep -c 'compiling once' "$R/N6.err")" = 1 ] || v=1
  verdict $v "N6 MCP: $(cat "$R/N6.txt" | cut -c1-220); built on this run $(grep -c 'compiling once' "$R/N6.err")"
  stop_daemons
fi

# ---- N7 THEOURGIA_SCHEME wins over a scheme on PATH; the program, daemon and eval child all use it
if has N7; then
  scratch n7; P=$T/prefix; install_into "$P" --ignore-scripts
  B=$(bindir n7); shim "$T/named" "$T/named.log"; shim "$B/scheme" "$T/path.log"
  (export PATH=$P/bin:$B:$BASEPATH THEOURGIA_SCHEME=$T/named
   "$P/bin/theourgia" init --store "$T/s" > "$R/N7-init.out" 2> "$R/N7-init.err"
   "$P/bin/theourgia" insert --title N7 --store "$T/s" > "$R/N7-insert.out" 2> "$R/N7-insert.err"
   "$P/bin/theourgia" eval --store "$T/s" '(+ 1 2)' > "$R/N7-eval.out" 2> "$R/N7-eval.err")
  cp "$T/named.log" "$R/N7-named.log"; cp "$T/path.log" "$R/N7-path.log" 2>/dev/null || : > "$R/N7-path.log"
  main=$(grep -c '^\[.*theourgia/theourgia.sc\]$' "$R/N7-named.log"); daemon=$(grep -c '^\[.*theourgia/theourgiad.sc\]$' "$R/N7-named.log")
  worker=$(grep -c '^\[.*eval-worker.sc\]$' "$R/N7-named.log"); pathuse=$(wc -c < "$R/N7-path.log" | tr -d ' ')
  [ "$main" -ge 1 ] && [ "$daemon" -ge 1 ] && [ "$worker" -ge 1 ] && [ "$pathuse" = 0 ] && grep -q '(ok' "$R/N7-eval.out"; v=$?
  verdict $v "N7 THEOURGIA_SCHEME wins: in its shim's log the program $main, the daemon $daemon, the eval child $worker times; the PATH scheme shim's log $pathuse bytes; eval $(head -c 60 "$R/N7-eval.out" | tr '\n' ' ')"
  stop_daemons
fi

# ---- N7b discovery exports the absolute path: PATH holds only chezscheme
if has N7b; then
  scratch n7b; P=$T/prefix; install_into "$P" --ignore-scripts
  B=$(bindir n7b); shim "$B/chezscheme" "$T/cs.log"
  (export PATH=$P/bin:$B:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2> "$R/N7b-init.err"
   "$P/bin/theourgia" insert --title N7b --store "$T/s" > "$R/N7b-insert.out" 2> "$R/N7b-insert.err")
  cp "$T/cs.log" "$R/N7b-cs.log"
  dline=$(grep -B2 '^\[.*theourgiad.sc\]$' "$R/N7b-cs.log" | grep '^===' | tail -1)
  case "$dline" in *"THEOURGIA_SCHEME=$B/chezscheme") v=0;; *) v=1;; esac
  verdict $v "N7b discovery exports the absolute path: the daemon's record: ${dline:-none}"
  stop_daemons
fi

# ---- N8 order: scheme before chez; chez alone; chezscheme alone
if has N8; then
  scratch n8; P=$T/prefix; install_into "$P" --ignore-scripts
  B=$(bindir n8both); shim "$B/scheme" "$T/s.log"; shim "$B/chez" "$T/c.log"
  (export PATH=$P/bin:$B:$BASEPATH; "$P/bin/theourgia" init --store "$T/s1" > "$R/N8-both.out" 2> "$R/N8-both.err"; echo $? > "$R/N8-both.rc")
  # shims, not links: Chez finds its boot files by the name it was run as,
  # so a link named chezscheme to a binary installed as scheme cannot start
  B2=$(bindir n8chez); shim "$B2/chez" "$T/c2.log"
  (export PATH=$P/bin:$B2:$BASEPATH; "$P/bin/theourgia" init --store "$T/s2" > "$R/N8-chez.out" 2> "$R/N8-chez.err"; echo $? > "$R/N8-chez.rc")
  B3=$(bindir n8cs); shim "$B3/chezscheme" "$T/cs3.log"
  (export PATH=$P/bin:$B3:$BASEPATH; "$P/bin/theourgia" init --store "$T/s3" > "$R/N8-cs.out" 2> "$R/N8-cs.err"; echo $? > "$R/N8-cs.rc")
  s=$(wc -c < "$T/s.log" | tr -d ' '); c=$( [ -f "$T/c.log" ] && wc -c < "$T/c.log" | tr -d ' ' || echo 0)
  c2=$(wc -c < "$T/c2.log" | tr -d ' '); cs3=$(wc -c < "$T/cs3.log" | tr -d ' ')
  [ "$s" -gt 0 ] && [ "$c" = 0 ] && [ "$(cat "$R/N8-both.rc")" = 0 ] && [ "$(cat "$R/N8-chez.rc")" = 0 ] && [ "$c2" -gt 0 ] &&
    [ "$(cat "$R/N8-cs.rc")" = 0 ] && [ "$cs3" -gt 0 ]; v=$?
  verdict $v "N8 order: with both, scheme's log $s bytes and chez's $c; chez alone rc $(cat "$R/N8-chez.rc") (its log $c2 bytes); chezscheme alone rc $(cat "$R/N8-cs.rc") (its log $cs3 bytes)"
  stop_daemons
fi

# ---- P4 the prepublish gate refuses each case by name, in a clone of the branch
# with its submodule initialised; the clean clone passes it.
if has P4; then
  X=$W/p4; : > "$R/P4.txt"
  gate_case() {
    local c=$1 D=$X/$1
    # A cell's HOME is a scratch one, with no git identity: the commits
    # name their author here, or they fail and leave the tree dirty.
    local commit="git -C $D -c user.name=cells -c user.email=cells@localhost commit"
    git clone -q "$here" "$D"; git -C "$D" config submodule.theourgia.url "$here/theourgia"
    [ "$c" = not-checked-out ] || git -C "$D" -c protocol.file.allow=always submodule update --init -q > /dev/null 2>&1
    case $c in
      moved) git -C "$D/theourgia" checkout -q HEAD~1 ;;
      submodule-dirty) echo x >> "$D/theourgia/README.md" ;;
      branch-dirty) echo x >> "$D/README.md" ;;
      object) touch "$D/lib/stale.so"; git -C "$D" add lib/stale.so; $commit -qm "a committed object" ;;
      list) echo "theourgia/extra.sc" >> "$D/test/expected-files.txt"; $commit -qam "a list that differs" ;;
      source-json) ln -s README.md "$D/source.json" ;;
    esac
    (cd "$D" && PATH=$(dirname "$NODE"):$(dirname "$NPM"):$BASEPATH node scripts/prepublish.js > "$R/P4-$c.out" 2> "$R/P4-$c.err"); echo "$c rc $? $(head -1 "$R/P4-$c.err")" >> "$R/P4.txt"
  }
  for c in clean not-checked-out moved submodule-dirty branch-dirty object list source-json; do gate_case $c; done
  # AND npm publish RUNS THE GATE: a dry run of the publish in a clone whose
  # submodule is moved stops with the gate's refusal (a dry run runs the
  # lifecycle scripts and sends nothing).
  D=$X/publish; git clone -q "$here" "$D"; git -C "$D" config submodule.theourgia.url "$here/theourgia"
  git -C "$D" -c protocol.file.allow=always submodule update --init -q > /dev/null 2>&1
  git -C "$D/theourgia" checkout -q HEAD~1
  (cd "$D" && PATH=$(dirname "$NODE"):$(dirname "$NPM"):$BASEPATH "$NPM" publish --dry-run > "$R/P4-publish.out" 2> "$R/P4-publish.err"); prc=$?
  grep -q 'prepublish: refused: submodule-moved:' "$R/P4-publish.err" "$R/P4-publish.out"; pg=$?
  echo "publish-dry-run rc $prc gate $( [ $pg = 0 ] && echo refused || echo NOT-RUN)" >> "$R/P4.txt"
  v=0
  for pair in "not-checked-out submodule-not-checked-out" "moved submodule-moved" "submodule-dirty submodule-dirty" \
              "branch-dirty branch-dirty" "object compiled-object" "list file-list-differs" \
              "source-json source-json-not-a-file"; do
    set -- $pair
    grep -q "^$1 rc 1 prepublish: refused: $2:" "$R/P4.txt" || v=1
  done
  grep -q '^clean rc 0 ' "$R/P4.txt" || v=1
  grep -q '^publish-dry-run rc [1-9][0-9]* gate refused$' "$R/P4.txt" || v=1
  verdict $v "P4 prepublish gate: $(cut -d' ' -f1-3,6 "$R/P4.txt" | tr '\n' ';')"
fi

# ---- N10 objects, not source: every .sc of the installed package's theourgia/ and of igropyr removed
if has N10; then
  scratch n10; P=$T/prefix
  (export PATH=$(dirname "$NODE"):$PKGBIN_DIR:$BASEPATH; install_into "$P")
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2>&1
   n=$(find "$P/lib/node_modules" -name '*.sc' | wc -l | tr -d ' ')
   find "$P/lib/node_modules" -name '*.sc' -exec rm -f {} +
   echo "$n $(find "$P/lib/node_modules" -name '*.sc' | wc -l | tr -d ' ')" > "$R/N10-removed.txt"
   "$P/bin/theourgia" insert --title N10-MARKER --store "$T/s" > "$R/N10-insert.out" 2> "$R/N10-insert.err"
   "$P/bin/theourgia" outline --store "$T/s" > "$R/N10-outline.out" 2> "$R/N10-outline.err"; echo $? > "$R/N10.rc")
  read -r was now < "$R/N10-removed.txt"
  [ "$now" = 0 ] && [ "$was" -gt 0 ] && [ "$(cat "$R/N10.rc")" = 0 ] && grep -q N10-MARKER "$R/N10-outline.out"; v=$?
  verdict $v "N10 objects, not source: removed $was .sc files (left $now); insert then outline rc $(cat "$R/N10.rc"), the block $(grep -c N10-MARKER "$R/N10-outline.out")"
  stop_daemons
fi

# ---- P5 the key covers igropyr's version and the theourgia commit: each changed
# in the installed package makes the next run build once more, into a
# directory of its own.
if has P5; then
  scratch p5; P=$T/prefix; install_into "$P" --ignore-scripts
  M=$(node -e 'process.stdout.write(require.resolve("igropyr/package.json", { paths: [process.argv[1]] }))' "$P/lib/node_modules/theourgia")
  # THE BUILDER IS COUNTED, not only the announcement: Chez is a shim that
  # logs its argv, and a run that compiled started build.ss once.
  shim "$T/chez" "$T/chez.log"
  run5() { : > "$T/chez.log"
           (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
            "$P/bin/theourgia" init --store "$T/$1" > "$R/P5-$1.out" 2> "$R/P5-$1.err"; echo $? > "$R/P5-$1.rc")
           grep -c '/build\.ss\]$' "$T/chez.log" > "$R/P5-$1.builds"; }
  dirs() { ls "$XDG_CACHE_HOME/theourgia" | grep -v '^\.' | wc -l | tr -d ' '; }
  run5 a; d1=$(dirs)
  node -e 'const f = process.argv[1]; const p = require(f); p.version = "1.8.2-p5"; require("fs").writeFileSync(f, JSON.stringify(p))' "$M"
  run5 b; d2=$(dirs)
  node -e 'require("fs").writeFileSync(process.argv[1], JSON.stringify({ theourgia: "0".repeat(40) }))' "$P/lib/node_modules/theourgia/source.json"
  run5 c; d3=$(dirs)
  built="$(grep -c 'compiling once' "$R/P5-a.err") $(grep -c 'compiling once' "$R/P5-b.err") $(grep -c 'compiling once' "$R/P5-c.err")"
  ran="$(cat "$R/P5-a.builds") $(cat "$R/P5-b.builds") $(cat "$R/P5-c.builds")"
  [ "$built" = "1 1 1" ] && [ "$ran" = "1 1 1" ] && [ "$d1 $d2 $d3" = "1 2 3" ] && [ "$(cat "$R/P5-a.rc")$(cat "$R/P5-b.rc")$(cat "$R/P5-c.rc")" = 000 ]; v=$?
  verdict $v "P5 key: builds announced $built, build.ss started $ran; cache directories $d1 $d2 $d3; rc $(cat "$R/P5-a.rc") $(cat "$R/P5-b.rc") $(cat "$R/P5-c.rc")"
  stop_daemons
fi

# ---- N11 the cache's location and key
if has N11; then
  scratch n11; P=$T/prefix; install_into "$P" --ignore-scripts
  cat > "$T/otherversion" <<EOF
#!/bin/sh
[ "\$1" = --version ] && { echo "10.1.0-other" >&2; exit 0; }
exec "$REAL" "\$@"
EOF
  chmod +x "$T/otherversion"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2> "$R/N11-first.err"
   THEOURGIA_SCHEME=$T/otherversion "$P/bin/theourgia" outline --store "$T/s" > "$R/N11-other.out" 2> "$R/N11-other.err")
  ls "$XDG_CACHE_HOME/theourgia" > "$R/N11-dirs.txt"; ls "$HOME/.cache/theourgia" > "$R/N11-home.txt" 2>/dev/null
  n=$(wc -l < "$R/N11-dirs.txt" | tr -d ' '); h=$(wc -l < "$R/N11-home.txt" | tr -d ' ')
  [ "$n" = 2 ] && [ "$h" = 0 ]; v=$?
  verdict $v "N11 cache location and key: under XDG_CACHE_HOME $n directories ($(tr '\n' ' ' < "$R/N11-dirs.txt")), under HOME/.cache $h"
  stop_daemons
fi

# ---- N12 argv and exit codes pass through unchanged
if has N12; then
  scratch n12; P=$T/prefix; install_into "$P" --ignore-scripts
  shim "$T/rec" "$T/rec.log"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/rec
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2>&1
   : > "$T/rec.log"
   "$P/bin/theourgia" insert --title 'two words' --text 'say "hi" and '"'"'bye'"'"'' '' --store "$T/s" > "$R/N12-insert.out" 2> "$R/N12-insert.err"
   cp "$T/rec.log" "$R/N12-rec.log"
   "$P/bin/theourgia" read nosuch.1 --store "$T/s" > "$R/N12-wrapped.out" 2>&1; echo $? > "$R/N12-wrapped.rc"
   dir=$(ls -d "$XDG_CACHE_HOME/theourgia/"*/ | head -1)
   CHEZSCHEMELIBDIRS=$dir CHEZSCHEMELIBEXTS=.so "$REAL" --script "$dir/theourgia/theourgia.sc" read nosuch.1 --store "$T/s" > "$R/N12-direct.out" 2>&1; echo $? > "$R/N12-direct.rc")
  awk '/^=== /{if (b ~ /\[insert\]/ && !done) {printf "%s", b; done=1} b=""; next} {b=b $0 "\n"} END {if (b ~ /\[insert\]/ && !done) printf "%s", b}' "$R/N12-rec.log" > "$R/N12-argv.txt"
  printf '%s\n' "[--script]" "[$(ls -d "$XDG_CACHE_HOME/theourgia/"*/ | head -1)theourgia/theourgia.sc]" "[insert]" "[--title]" "[two words]" "[--text]" "[say \"hi\" and 'bye']" "[]" "[--store]" "[$T/s]" > "$R/N12-argv-expected.txt"
  sed -i.bak 's#//theourgia/#/theourgia/#' "$R/N12-argv-expected.txt"; rm -f "$R/N12-argv-expected.txt.bak"
  cmp -s "$R/N12-argv-expected.txt" "$R/N12-argv.txt"; a=$?
  [ "$(cat "$R/N12-wrapped.rc")" = "$(cat "$R/N12-direct.rc")" ] && [ "$(cat "$R/N12-wrapped.rc")" != 0 ]; b=$?
  # a child killed by a signal: the wrapper ends the same way, SIGPIPE
  # included (Node ignores that one, so it is passed on as 128 + 13)
  cat > "$T/dies" <<EOF
#!/bin/sh
case "\$1" in --script) kill -\$N12_SIGNAL \$\$ ;; esac
exec "$REAL" "\$@"
EOF
  chmod +x "$T/dies"
  sigs=""
  for sig in PIPE TERM; do
    (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/dies N12_SIGNAL=$sig
     "$P/bin/theourgia" outline --store "$T/s" > /dev/null 2>&1; echo $? > "$R/N12-signal-$sig.rc")
    sigs="$sigs $sig:$(cat "$R/N12-signal-$sig.rc")"
  done
  [ "$(cat "$R/N12-signal-PIPE.rc")" = 141 ] && [ "$(cat "$R/N12-signal-TERM.rc")" = 143 ]; c=$?
  verdict $((a + b + c)) "N12 argv, exit and signals: killed by SIGPIPE, SIGTERM the wrapper exits$sigs (141, 143 wanted); the program's argv $( [ $a = 0 ] && echo equals the caller\'s || echo DIFFERS); a refused verb exits $(cat "$R/N12-wrapped.rc") wrapped and $(cat "$R/N12-direct.rc") direct"
  stop_daemons
fi

# ---- N13 no package manager is ever run
if has N13; then
  # every mention of a package manager in any file the package runs; the
  # only ones allowed are the install lines installLineFor returns as text
  grep -rnE "\b(brew|apt|apt-get|dnf|pacman|yum|port)\b" "$here/lib" "$here/bin" "$here/scripts" > "$R/N13-hits.txt"
  lo=$(grep -n '^function installLineFor' "$here/lib/theourgia.js" | cut -d: -f1)
  hi=$(awk -v lo="$lo" 'NR > lo && /^}/ {print NR; exit}' "$here/lib/theourgia.js")
  # a hit is allowed only as returned text inside installLineFor: a line
  # that is a return of a string or a string's continuation, and that
  # function may not spawn or exec anything at all
  outside=$(awk -F: -v f="$here/lib/theourgia.js" -v lo="$lo" -v hi="$hi" '!($1 == f && $2 > lo && $2 < hi)' "$R/N13-hits.txt" | grep -v '^[^:]*:[0-9]*: *//' | wc -l | tr -d ' ')
  notext=$(awk -F: -v f="$here/lib/theourgia.js" -v lo="$lo" -v hi="$hi" '$1 == f && $2 > lo && $2 < hi' "$R/N13-hits.txt" | cut -d: -f3- | grep -vcE "return '|^ *'")
  calls=$(sed -n "${lo},${hi}p" "$here/lib/theourgia.js" | grep -cE "spawn|exec|child_process|require\(")
  inside=$(wc -l < "$R/N13-hits.txt" | tr -d ' ')
  grep -rnE "(spawn|exec)[A-Za-z]*\(" "$here/lib" "$here/bin" "$here/scripts" > "$R/N13-calls.txt"
  [ "$outside" = 0 ] && [ "$notext" = 0 ] && [ "$calls" = 0 ] && [ "$inside" -ge 4 ]; v=$?
  verdict $v "N13 no package manager: $inside mentions in lib/, bin/, scripts/: $outside outside installLineFor (lines $lo-$hi), $notext inside it that are not returned text; calls inside it $calls; spawn/exec calls in all $(wc -l < "$R/N13-calls.txt" | tr -d ' ')"
fi

# ---- N15 a warm cache runs no Chez before the program; a replaced binary is probed again
# The shim here is a compiled executable, not a script: a script is probed
# on every call by design (N15b), since what it starts can change under it.
# It logs its argv and execs the real Chez. Built with cc; without cc the
# cell is skipped by name.
cshim() {
  local out=$1 log=$2 replaced=$3 next=${4:-} self=${5:-}
  cat > "$W/cshim.c" <<'EOF'
#include <stdio.h>
#include <string.h>
#include <unistd.h>
int main(int argc, char **argv) {
  FILE *f = fopen(SHIM_LOG, "a");
  if (f) {
    fprintf(f, "=== %s\n", argv[0]);
    for (int i = 1; i < argc; i++) fprintf(f, "[%s]\n", argv[i]);
    fclose(f);
  }
#ifdef SHIM_NEXT
  if (argc > 1 && strcmp(argv[1], "--version") == 0 && access(SHIM_NEXT, F_OK) == 0) rename(SHIM_NEXT, SHIM_SELF);
#endif
  if (SHIM_REPLACED && argc > 1 && strcmp(argv[1], "--version") == 0) {
    fprintf(stderr, "10.1.0-replaced\n");
    return 0;
  }
  argv[0] = SHIM_REAL;
  execv(SHIM_REAL, argv);
  return 127;
}
EOF
  if [ -n "$next" ]; then
    cc -O0 -o "$out" -DSHIM_LOG="\"$log\"" -DSHIM_REAL="\"$REAL\"" -DSHIM_REPLACED="$replaced" \
       -DSHIM_NEXT="\"$next\"" -DSHIM_SELF="\"$self\"" "$W/cshim.c"
  else
    cc -O0 -o "$out" -DSHIM_LOG="\"$log\"" -DSHIM_REAL="\"$REAL\"" -DSHIM_REPLACED="$replaced" "$W/cshim.c"
  fi
}
if has N15; then
  if ! command -v cc > /dev/null 2>&1; then
    say "SKIP N15 the key remembered: no C compiler (cc) on PATH to build a native shim; install one to run this cell"
  else
    scratch n15; P=$T/prefix; install_into "$P" --ignore-scripts
    cshim "$T/chez" "$T/chez.log" 0
    (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
     "$P/bin/theourgia" init --store "$T/s" > /dev/null 2> "$R/N15-cold.err"
     "$P/bin/theourgia" insert --title N15 --store "$T/s" > /dev/null 2>&1
     : > "$T/chez.log"
     "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/N15-warm.out" 2> "$R/N15-warm.err"; echo $? > "$R/N15-warm.rc"
     cp "$T/chez.log" "$R/N15-warm.log")
    # the replacement is compiled here, outside the cell's PATH, which holds no cc
    rm -f "$T/chez"
    cshim "$T/chez" "$T/chez.log" 1
    (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
     "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/N15-replaced.out" 2> "$R/N15-replaced.err"; echo $? > "$R/N15-replaced.rc")
    calls=$(grep -c '^===' "$R/N15-warm.log")
    program=$(grep -c '^\[.*theourgia/theourgia.sc\]$' "$R/N15-warm.log")
    dirs=$(ls "$XDG_CACHE_HOME/theourgia" | wc -l | tr -d ' ')
    records=$(ls "$XDG_CACHE_HOME/theourgia/.chez" 2>/dev/null | grep -c '\.json$')
    [ "$calls" = 1 ] && [ "$program" = 1 ] && [ "$(cat "$R/N15-warm.rc")" = 0 ] && grep -q '^(ok' "$R/N15-warm.out" &&
      [ "$dirs" = 2 ] && [ "$(cat "$R/N15-replaced.rc")" = 0 ] && [ "$(grep -c 'compiling once' "$R/N15-replaced.err")" = 1 ]; v=$?
    verdict $v "N15 the key remembered: a warm call started Chez $calls time(s), the program $program; a replaced binary built $(grep -c 'compiling once' "$R/N15-replaced.err") new cache (directories now $dirs); records $records"
    stop_daemons
  fi
fi

# ---- N15d a binary replaced while it is probed: the facts are discarded and it is probed again
if has N15 && command -v cc > /dev/null 2>&1; then
  scratch n15d; P=$T/prefix; install_into "$P" --ignore-scripts
  cshim "$T/next" "$T/chez.log" 1
  cshim "$T/chez" "$T/chez.log" 0 "$T/next" "$T/chez"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
   "$P/bin/theourgia" init --store "$T/s" > "$R/N15d.out" 2> "$R/N15d.err"; echo $? > "$R/N15d.rc")
  cp "$T/chez.log" "$R/N15d.log"
  (cd "$XDG_CACHE_HOME/theourgia/.chez" 2>/dev/null && cat ./*.json 2>/dev/null) > "$R/N15d-records.txt"
  records=$(ls "$XDG_CACHE_HOME/theourgia/.chez" 2>/dev/null | grep -c '\.json$')
  replaced=$(grep -c '10.1.0-replaced' "$R/N15d-records.txt")
  versions=$(grep -c '^\[--version\]$' "$R/N15d.log")
  [ "$(cat "$R/N15d.rc")" = 0 ] && grep -q '(ok (store' "$R/N15d.out" && [ "$records" = 1 ] && [ "$replaced" = 1 ] && [ "$versions" = 2 ]; v=$?
  verdict $v "N15d replaced mid-probe: init rc $(cat "$R/N15d.rc"); --version probes $versions (a retry); records $records, of them the replacement's $replaced"
  stop_daemons
fi

# ---- N15e a script replaced while it is probed: its facts are checked the same way
if has N15; then
  scratch n15e; P=$T/prefix; install_into "$P" --ignore-scripts
  shim "$T/chez" "$T/chez.log"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2>&1)
  cat > "$T/next" <<EOF
#!/bin/sh
{ echo "=== next"; for a in "\$@"; do printf '[%s]\n' "\$a"; done; } >> "$T/chez.log"
[ "\$1" = --version ] && { echo "10.1.0-replaced" >&2; exit 0; }
exec "$REAL" "\$@"
EOF
  cat > "$T/chez.new" <<EOF
#!/bin/sh
{ echo "=== first"; for a in "\$@"; do printf '[%s]\n' "\$a"; done; } >> "$T/chez.log"
[ "\$1" = --version ] && [ -e "$T/next" ] && mv "$T/next" "$T/chez"
exec "$REAL" "\$@"
EOF
  chmod +x "$T/next" "$T/chez.new"; mv "$T/chez.new" "$T/chez"
  : > "$T/chez.log"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
   "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/N15e.out" 2> "$R/N15e.err"; echo $? > "$R/N15e.rc")
  cp "$T/chez.log" "$R/N15e.log"
  versions=$(grep -c '^\[--version\]$' "$R/N15e.log")
  dirs=$(ls "$XDG_CACHE_HOME/theourgia" | wc -l | tr -d ' ')
  records=$(ls "$XDG_CACHE_HOME/theourgia/.chez" 2>/dev/null | grep -c '\.json$')
  built=$(grep -c 'compiling once' "$R/N15e.err")
  [ "$(cat "$R/N15e.rc")" = 0 ] && grep -q '^(ok' "$R/N15e.out" && [ "$versions" = 2 ] && [ "$dirs" = 2 ] && [ "$built" = 1 ] && [ "$records" = 0 ]; v=$?
  verdict $v "N15e script replaced mid-probe: rc $(cat "$R/N15e.rc"); --version probes $versions (a retry); new caches built $built (directories now $dirs); records $records"
  stop_daemons
fi

# ---- N15f a binary that may be executed but not read is probed on every call, never remembered
if has N15 && command -v cc > /dev/null 2>&1; then
  scratch n15f; P=$T/prefix; install_into "$P" --ignore-scripts
  cshim "$T/chez" "$T/chez.log" 0
  chmod 111 "$T/chez"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
   "$P/bin/theourgia" init --store "$T/s" > "$R/N15f-init.out" 2> "$R/N15f-init.err"; echo $? > "$R/N15f-init.rc"
   : > "$T/chez.log"
   "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/N15f.out" 2> "$R/N15f.err"; echo $? > "$R/N15f.rc")
  cp "$T/chez.log" "$R/N15f.log"
  chmod 755 "$T/chez"
  versions=$(grep -c '^\[--version\]$' "$R/N15f.log")
  records=$(ls "$XDG_CACHE_HOME/theourgia/.chez" 2>/dev/null | grep -c '\.json$')
  [ "$(cat "$R/N15f-init.rc")" = 0 ] && [ "$(cat "$R/N15f.rc")" = 0 ] && grep -q '^(ok' "$R/N15f.out" && [ "$versions" = 1 ] && [ "$records" = 0 ]; v=$?
  verdict $v "N15f execute-only binary: init rc $(cat "$R/N15f-init.rc"), warm rc $(cat "$R/N15f.rc"); --version probes on the warm call $versions; records $records"
  stop_daemons
fi

# ---- N15c a record that cannot be written is skipped, never an error
if has N15; then
  scratch n15c; P=$T/prefix; install_into "$P" --ignore-scripts
  mkdir -p "$XDG_CACHE_HOME/theourgia/.chez"; chmod 000 "$XDG_CACHE_HOME/theourgia/.chez"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH
   "$P/bin/theourgia" init --store "$T/s" > "$R/N15c.out" 2> "$R/N15c.err"; echo $? > "$R/N15c.rc")
  chmod 700 "$XDG_CACHE_HOME/theourgia/.chez"
  [ "$(cat "$R/N15c.rc")" = 0 ] && grep -q '(ok (store' "$R/N15c.out"; v=$?
  verdict $v "N15c an unwritable record directory: init rc $(cat "$R/N15c.rc"), answered $(grep -c '(ok (store' "$R/N15c.out")"
  stop_daemons
fi

# ---- N15b a script named as Chez is probed on every call, never remembered
if has N15; then
  scratch n15b; P=$T/prefix; install_into "$P" --ignore-scripts
  shim "$T/chez" "$T/chez.log"
  (export PATH=$P/bin:$PKGBIN_DIR:$BASEPATH THEOURGIA_SCHEME=$T/chez
   "$P/bin/theourgia" init --store "$T/s" > /dev/null 2>&1
   "$P/bin/theourgia" insert --title N15b --store "$T/s" > /dev/null 2>&1
   : > "$T/chez.log"
   "$P/bin/theourgia" outline --wire --store "$T/s" > "$R/N15b.out" 2> "$R/N15b.err"; echo $? > "$R/N15b.rc"
   cp "$T/chez.log" "$R/N15b.log")
  calls=$(grep -c '^===' "$R/N15b.log"); probes=$(grep -c '^\[--version\]$' "$R/N15b.log")
  records=$(ls "$XDG_CACHE_HOME/theourgia/.chez" 2>/dev/null | grep -c '\.json$')
  [ "$calls" = 3 ] && [ "$probes" = 1 ] && [ "$records" = 0 ] && [ "$(cat "$R/N15b.rc")" = 0 ]; v=$?
  verdict $v "N15b a script is not remembered: a warm call started it $calls times ($probes --version probe), records $records"
  stop_daemons
fi

stop_daemons
say "cells end $(date '+%H:%M:%S'): $fails failed; daemons left $(pgrep -f "theourgiad.sc serve $W" | wc -l | tr -d ' ')"
rm -rf "$W"
exit $(( fails > 0 ? 1 : 0 ))
