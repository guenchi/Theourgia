'use strict';

// Checks and warms, and never fails the install: the platform, Chez, and
// the compiled objects for that Chez. Whatever goes wrong is said on stderr
// and left to the first run, which tries again. It never runs a package
// manager, and `npm install --ignore-scripts` leaves all of it to that run.

const lib = require('../lib/theourgia');

try {
  lib.checkPlatform();
  const chez = lib.findChez();
  lib.ensureCache(chez, true);
  process.stderr.write('theourgia: ready, compiled for ' + chez + '\n');
} catch (e) {
  lib.report(e);
  process.stderr.write('theourgia: the first run will try again.\n');
}
process.exit(0);
