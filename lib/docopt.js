import * as PS from './docopt.purs.js'
const docopt = PS['Docopt.FFI'];

export default function run(...args) {
  return docopt.run(...args)();
}
