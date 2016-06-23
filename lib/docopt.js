import * as PS from './docopt.purs.min.js'
const docopt = PS['Docopt.FFI'];

export function run(...args) {
  return docopt.run(...args)();
}

export function parse(...args) {
  return docopt.parse(...args)();
}
