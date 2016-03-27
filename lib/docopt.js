import * as PS from './docopt.purs.js'
const docopt = PS['Docopt.FFI'];

docopt.run(
  docopt.defaultOptions,
  `
  Usage:
    prog foo bar
  `
)();
