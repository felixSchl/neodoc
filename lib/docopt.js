import * as PS from './docopt.purs.js'
const docopt = PS['Docopt.FFI'];

export function run(...args) {
  return docopt.run(...args)();
}
