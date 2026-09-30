'use strict';

// The wrappers' shared logic: find Chez, refuse a platform the core has no
// row for, build the compiled objects once per Chez into a keyed cache, and
// run a program from them. Nothing here writes to stdout: the MCP shell's
// stdout is the protocol, and the thin client's is the answer.

const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const { spawn, spawnSync } = require('child_process');

const packageRoot = path.resolve(__dirname, '..');
const vendorDir = path.join(packageRoot, 'vendor');
const packageVersion = require(path.join(packageRoot, 'package.json')).version;

const EX_UNAVAILABLE = 69;
const EX_SOFTWARE = 70;
const EX_TEMPFAIL = 75;

// retry: the failure was a build, which the next run attempts again
class WrapperExit extends Error {
  constructor(code, lines, retry) {
    super(lines.join('\n'));
    this.code = code;
    this.lines = lines;
    this.retry = retry === true;
  }
}

function say(line) {
  process.stderr.write(line + '\n');
}

// macOS on x86_64, native or under Rosetta, has no measured row: the core
// refuses it at start-up with this datum and exit 75, and the wrapper says
// the same before anything is built. Other combinations are kept out by the
// package's os and cpu fields. A pure function of the two values, so it is
// tested without the hardware. -> null, or { code, lines }
function platformRefusal(platform, arch) {
  if (platform === 'darwin' && arch === 'x64') {
    return {
      code: EX_TEMPFAIL,
      lines: [
        '(error platform-unmeasured (system "Darwin") (machine "x86_64") ' +
          '(remedy "run test/probe/layout.c and add its row"))',
      ],
    };
  }
  return null;
}

function checkPlatform() {
  const refusal = platformRefusal(process.platform, process.arch);
  if (refusal) throw new WrapperExit(refusal.code, refusal.lines);
}

function isExecutableFile(p) {
  try {
    fs.accessSync(p, fs.constants.X_OK);
    return fs.statSync(p).isFile();
  } catch (e) {
    return false;
  }
}

function onPath(name) {
  const dirs = (process.env.PATH || '').split(path.delimiter).filter((d) => d.length > 0);
  for (const d of dirs) {
    const candidate = path.resolve(d, name);
    if (isExecutableFile(candidate)) return candidate;
  }
  return null;
}

// The platform's install line, a pure function of the platform and the text
// of /etc/os-release (its ID and ID_LIKE fields), so it is tested on sample
// texts. An unknown system gets the three lines together.
function installLineFor(platform, osRelease) {
  if (platform === 'darwin') return 'brew install chezscheme libuv';
  const field = (name) => {
    const m = osRelease.match(new RegExp('^' + name + '=(.*)$', 'm'));
    return m ? m[1].trim().replace(/^(["'])(.*)\1$/, '$2').toLowerCase() : '';
  };
  const ids = (field('ID') + ' ' + field('ID_LIKE')).split(/\s+/);
  if (ids.includes('debian') || ids.includes('ubuntu')) return 'sudo apt install chezscheme libuv1';
  if (ids.includes('fedora') || ids.includes('rhel')) return 'sudo dnf install chez-scheme libuv';
  if (ids.includes('arch')) return 'sudo pacman -S chez-scheme libuv';
  return 'sudo apt install chezscheme libuv1 (Debian, Ubuntu), sudo dnf install chez-scheme libuv (Fedora), ' +
    'or sudo pacman -S chez-scheme libuv (Arch)';
}

function installLine() {
  let osRelease = '';
  if (process.platform !== 'darwin') {
    try {
      osRelease = fs.readFileSync('/etc/os-release', 'utf8');
    } catch (e) {
      osRelease = '';
    }
  }
  return installLineFor(process.platform, osRelease);
}

function missingChez(what) {
  return new WrapperExit(EX_UNAVAILABLE, [
    'theourgia: ' + what + '; install Chez Scheme and libuv, for example: ' + installLine(),
    'theourgia: Theourgia needs Chez Scheme 10.1.0, 10.3.0, 10.4.0 or 10.4.1; ' +
      'older distribution packages will be refused.',
  ]);
}

// THEOURGIA_SCHEME when set, else the first of scheme, chez and chezscheme
// on PATH (Homebrew names the binary chez). -> an absolute path.
function findChez() {
  const named = process.env.THEOURGIA_SCHEME;
  if (named !== undefined && named !== '') {
    const p = named.includes('/') ? path.resolve(named) : onPath(named);
    if (p && isExecutableFile(p)) return p;
    throw missingChez('THEOURGIA_SCHEME names ' + named + ', which is not an executable file');
  }
  for (const name of ['scheme', 'chez', 'chezscheme']) {
    const p = onPath(name);
    if (p) return p;
  }
  throw missingChez('Chez Scheme was not found (THEOURGIA_SCHEME is unset, and none of scheme, chez ' +
    'or chezscheme is on PATH)');
}

function chezEnv() {
  const env = Object.assign({}, process.env);
  delete env.CHEZSCHEMELIBDIRS;
  delete env.CHEZSCHEMELIBEXTS;
  return env;
}

// The Chez binary's version and machine type, from the binary itself: two
// starts of it. -> { version, machine }
function probeChez(chez) {
  const version = spawnSync(chez, ['--version'], { encoding: 'utf8', env: chezEnv() });
  const machine = spawnSync(chez, ['-q'], {
    input: '(display (machine-type))\n(exit)\n',
    encoding: 'utf8',
    env: chezEnv(),
  });
  if (version.status !== 0 || machine.status !== 0) {
    throw new WrapperExit(EX_SOFTWARE, [
      'theourgia: ' + chez + ' did not answer --version and (machine-type); is it Chez Scheme?',
    ]);
  }
  return { version: (version.stdout + version.stderr).trim(), machine: machine.stdout.trim() };
}

// The file's identity: its real path and its stat (device, inode, size,
// modification time). -> an object, or null when it cannot be read
function identityOf(chez) {
  try {
    const real = fs.realpathSync(chez);
    const st = fs.statSync(real);
    return { path: real, dev: st.dev, ino: st.ino, size: st.size, mtimeMs: st.mtimeMs };
  } catch (e) {
    return null;
  }
}

// A script (a file that starts with #!) may start any Chez at all, and a
// change to the one it starts leaves the script's own identity as it was.
function isScript(file) {
  let fd;
  try {
    fd = fs.openSync(file, 'r');
    const head = Buffer.alloc(2);
    return fs.readSync(fd, head, 0, 2, 0) === 2 && head.toString('latin1') === '#!';
  } catch (e) {
    return true;
  } finally {
    if (fd !== undefined) fs.closeSync(fd);
  }
}

// THE PROBE IS REMEMBERED PER BINARY, because it costs two starts of Chez on
// every call and a call through the wrapper is otherwise one start. The
// record is keyed by the binary's identity, so a changed or replaced binary
// is a miss and is probed again. A script is never remembered: it is
// probed on every call, since what it starts can change under it. The
// identity is read again after the probe and the record is written only if
// it is still the same file, so a binary replaced while it was being probed
// does not leave its facts under the other's identity. The record is
// written to a temporary name and renamed; a record that does not read, or
// does not describe this file, is a miss, and a record that cannot be
// written is skipped -- neither is ever an error. The records live in .chez/
// under the cache base.
function chezFacts(chez) {
  const identity = identityOf(chez);
  if (identity === null || isScript(identity.path)) return probeChez(chez);
  const dir = path.join(cacheBase(), '.chez');
  const name = crypto.createHash('sha256').update(JSON.stringify(identity)).digest('hex').slice(0, 24) + '.json';
  const record = path.join(dir, name);
  try {
    const known = JSON.parse(fs.readFileSync(record, 'utf8'));
    if (JSON.stringify(known.identity) === JSON.stringify(identity) &&
        typeof known.version === 'string' && typeof known.machine === 'string') {
      return { version: known.version, machine: known.machine };
    }
  } catch (e) {
    // no record, or one that does not parse: a miss
  }
  const facts = probeChez(chez);
  if (JSON.stringify(identityOf(chez)) !== JSON.stringify(identity)) return facts;
  const tmp = record + '.' + process.pid + '.' + crypto.randomBytes(4).toString('hex');
  try {
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(tmp, JSON.stringify({ identity, version: facts.version, machine: facts.machine }));
    fs.renameSync(tmp, record);
  } catch (e) {
    try {
      fs.rmSync(tmp, { force: true });
    } catch (e2) {
      return facts;
    }
  }
  return facts;
}

// What the objects depend on: this package's version and pins, the Chez
// binary's version and its machine type. A different Chez gets its own
// directory rather than objects it cannot load.
function cacheKey(chez) {
  const facts = chezFacts(chez);
  let pins = '';
  try {
    pins = fs.readFileSync(path.join(vendorDir, 'PINS'), 'utf8');
  } catch (e) {
    throw new WrapperExit(EX_SOFTWARE, ['theourgia: this package has no vendor/ tree; it was not assembled']);
  }
  const digest = crypto.createHash('sha256')
    .update([packageVersion, pins, facts.version, facts.machine].join('\n'))
    .digest('hex')
    .slice(0, 16);
  return packageVersion + '-' + facts.machine + '-' + digest;
}

function cacheBase() {
  const xdg = process.env.XDG_CACHE_HOME;
  const base = xdg && path.isAbsolute(xdg) ? xdg : path.join(os.homedir(), '.cache');
  return path.join(base, 'theourgia');
}

function lastLines(text, n) {
  const lines = text.split('\n').filter((l) => l.length > 0);
  return lines.slice(-n);
}

// The objects for this Chez, built once. The build goes into a temporary
// sibling and is renamed into place, which is atomic: a directory under the
// key is always a complete build. Two first runs at once both build, one
// rename wins, and the other discards its copy and uses the winner's. The
// build's own output is captured and shown only when it fails.
function ensureCache(chez, announce) {
  const base = cacheBase();
  const dir = path.join(base, cacheKey(chez));
  if (fs.existsSync(path.join(dir, 'theourgia', 'theourgia.sc'))) return dir;
  fs.mkdirSync(base, { recursive: true });
  const tmp = fs.mkdtempSync(path.join(base, '.build-'));
  try {
    if (announce) say('theourgia: compiling once for this Chez, about 10 seconds, into ' + dir);
    const build = spawnSync(chez, ['--script', path.join(vendorDir, 'theourgia', 'build.ss'), vendorDir, tmp], {
      encoding: 'utf8',
      env: chezEnv(),
      stdio: ['ignore', 'pipe', 'pipe'],
      maxBuffer: 64 * 1024 * 1024,
    });
    if (build.status !== 0 || !fs.existsSync(path.join(tmp, 'theourgia', 'theourgia.sc'))) {
      const tail = lastLines((build.stdout || '') + '\n' + (build.stderr || ''), 12);
      throw new WrapperExit(EX_SOFTWARE, ['theourgia: the compiled objects could not be built:']
        .concat(tail.map((l) => '  ' + l)), true);
    }
    try {
      fs.renameSync(tmp, dir);
    } catch (e) {
      if (!fs.existsSync(path.join(dir, 'theourgia', 'theourgia.sc'))) throw e;
    }
    return dir;
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
}

function report(e) {
  if (e instanceof WrapperExit) {
    for (const line of e.lines) say(line);
    return e.code;
  }
  say('theourgia: ' + (e && e.message ? e.message : String(e)));
  return EX_SOFTWARE;
}

// Runs one of the programs from the cache: the arguments unchanged, the
// three streams inherited, and the child's exit status -- or its signal --
// passed through as this process's own.
function run(program) {
  let chez;
  let dir;
  try {
    checkPlatform();
    chez = findChez();
    dir = ensureCache(chez, true);
  } catch (e) {
    process.exit(report(e));
  }
  const env = Object.assign({}, process.env, {
    CHEZSCHEMELIBDIRS: dir,
    CHEZSCHEMELIBEXTS: '.so',
    THEOURGIA_SCHEME: chez,
  });
  const child = spawn(chez, ['--script', path.join(dir, 'theourgia', program)].concat(process.argv.slice(2)), {
    stdio: 'inherit',
    env,
  });
  const forward = ['SIGINT', 'SIGTERM', 'SIGHUP'];
  const handlers = forward.map((sig) => {
    const h = () => {
      try {
        child.kill(sig);
      } catch (e) {
        return;
      }
    };
    process.on(sig, h);
    return [sig, h];
  });
  child.on('error', (e) => {
    say('theourgia: could not start ' + chez + ': ' + e.message);
    process.exit(EX_SOFTWARE);
  });
  child.on('exit', (code, signal) => {
    for (const [sig, h] of handlers) process.removeListener(sig, h);
    // A SIGNAL IS PASSED ON AS ITSELF where Node lets it end this process,
    // and as 128 + its number where it does not: Node ignores SIGPIPE, so
    // re-raising that one would leave the wrapper exiting 0 for a child
    // that was killed. The timer is kept referenced so the fallback runs.
    if (signal) {
      const number = os.constants.signals[signal] || 0;
      process.exitCode = 128 + number;
      setTimeout(() => process.exit(128 + number), 200);
      if (signal !== 'SIGPIPE') process.kill(process.pid, signal);
      return;
    }
    process.exit(code === null ? EX_SOFTWARE : code);
  });
}

module.exports = {
  run, checkPlatform, platformRefusal, installLineFor, findChez, ensureCache, report, WrapperExit,
};
