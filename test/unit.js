'use strict';

// Unit cells for the two pure functions: the install line chosen from an
// os-release text (N3b) and the platform refusal (N3c). Prints one line per
// case and exits non-zero when any differs.

const lib = require('../lib/theourgia');

let bad = 0;
function want(label, got, expected) {
  const same = JSON.stringify(got) === JSON.stringify(expected);
  if (!same) bad += 1;
  process.stdout.write((same ? 'ok ' : 'FAIL ') + label + (same ? '' : ': ' + JSON.stringify(got) + ' WANT ' +
    JSON.stringify(expected)) + '\n');
}

const debian = 'PRETTY_NAME="Debian GNU/Linux 12 (bookworm)"\nNAME="Debian GNU/Linux"\nID=debian\n';
const ubuntu = 'NAME="Ubuntu"\nVERSION="24.04 LTS (Noble Numbat)"\nID=ubuntu\nID_LIKE=debian\n';
const fedora = 'NAME="Fedora Linux"\nVERSION="40 (Workstation Edition)"\nID=fedora\n';
const arch = 'NAME="Arch Linux"\nID=arch\n';
const unknown = 'NAME="Some Linux"\nID=somelinux\n';
const generic = 'sudo apt install chezscheme libuv1 (Debian, Ubuntu), sudo dnf install chez-scheme libuv (Fedora), ' +
  'or sudo pacman -S chez-scheme libuv (Arch)';

want('N3b Debian', lib.installLineFor('linux', debian), 'sudo apt install chezscheme libuv1');
want('N3b Ubuntu', lib.installLineFor('linux', ubuntu), 'sudo apt install chezscheme libuv1');
want('N3b Fedora', lib.installLineFor('linux', fedora), 'sudo dnf install chez-scheme libuv');
want('N3b Arch', lib.installLineFor('linux', arch), 'sudo pacman -S chez-scheme libuv');
want('N3b a single-quoted ID', lib.installLineFor('linux', "ID='fedora'\n"), 'sudo dnf install chez-scheme libuv');
want('N3b darwin', lib.installLineFor('darwin', ''), 'brew install chezscheme libuv');
want('N3b unknown os-release: the generic sentence', lib.installLineFor('linux', unknown), generic);
want('N3b no os-release at all: the generic sentence', lib.installLineFor('linux', ''), generic);

const refusal = lib.platformRefusal('darwin', 'x64');
want('N3c darwin/x64 is refused with exit 75', refusal && refusal.code, 75);
want('N3c and the line is the core\'s datum', refusal && refusal.lines, [
  '(error platform-unmeasured (system "Darwin") (machine "x86_64") (remedy "run test/probe/layout.c and add its row"))',
]);
for (const [p, a] of [['darwin', 'arm64'], ['linux', 'x64'], ['linux', 'arm64']]) {
  want('N3c ' + p + '/' + a + ' is accepted', lib.platformRefusal(p, a), null);
}

process.stdout.write(bad + ' failures\n');
process.exit(bad === 0 ? 0 : 1);
