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

class WrapperExit extends Error {
  constructor(code, lines) {
    super(lines.join('\n'));
    this.code = code;
    this.lines = lines;
  }
}

function say(line) {
  process.stderr.write(line + '\n');
}

// macOS on x86_64, native or under Rosetta, has no measured row: the core
// refuses it at start-up with this datum and exit 75, and the wrapper says
// the same before anything is built. Other combinations are kept out by the
// package's os and cpu fields.
function checkPlatform() {
  if (process.platform === 'darwin' && process.arch === 'x64') {
    throw new WrapperExit(EX_TEMPFAIL, [
      '(error platform-unmeasured (system "Darwin") (machine "x86_64") ' +
        '(remedy "run test/probe/layout.c and add its row"))',
    ]);
  }
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

function installLine() {
  if (process.platform === 'darwin') return 'brew install chezscheme libuv';
  let osRelease = '';
  try {
    osRelease = fs.readFileSync('/etc/os-release', 'utf8');
  } catch (e) {
    osRelease = '';
  }
  const field = (name) => {
    const m = osRelease.match(new RegExp('^' + name + '=(.*)$', 'm'));
    return m ? m[1].replace(/^"|"$/g, '').toLowerCase() : '';
  };
  const ids = (field('ID') + ' ' + field('ID_LIKE')).split(/\s+/);
  if (ids.includes('debian') || ids.includes('ubuntu')) return 'sudo apt install chezscheme libuv1';
  if (ids.includes('fedora') || ids.includes('rhel')) return 'sudo dnf install chez-scheme libuv';
  if (ids.includes('arch')) return 'sudo pacman -S chez-scheme libuv';
  return 'sudo apt install chezscheme libuv1 (Debian, Ubuntu), sudo dnf install chez-scheme libuv (Fedora), ' +
    'or sudo pacman -S chez-scheme libuv (Arch)';
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

// What the objects depend on: this package's version and pins, the Chez
// binary's version and its machine type. A different Chez gets its own
// directory rather than objects it cannot load.
function cacheKey(chez) {
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
  let pins = '';
  try {
    pins = fs.readFileSync(path.join(vendorDir, 'PINS'), 'utf8');
  } catch (e) {
    throw new WrapperExit(EX_SOFTWARE, ['theourgia: this package has no vendor/ tree; it was not assembled']);
  }
  const chezVersion = (version.stdout + version.stderr).trim();
  const machineType = machine.stdout.trim();
  const digest = crypto.createHash('sha256')
    .update([packageVersion, pins, chezVersion, machineType].join('\n'))
    .digest('hex')
    .slice(0, 16);
  return packageVersion + '-' + machineType + '-' + digest;
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
        .concat(tail.map((l) => '  ' + l)));
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
    if (signal) {
      const number = os.constants.signals[signal] || 0;
      setTimeout(() => process.exit(128 + number), 1000).unref();
      process.kill(process.pid, signal);
      return;
    }
    process.exit(code === null ? EX_SOFTWARE : code);
  });
}

module.exports = { run, checkPlatform, findChez, ensureCache, report, WrapperExit };
