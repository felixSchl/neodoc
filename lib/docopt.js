import * as PS from './docopt.purs.min.js'
const docopt = PS['Docopt.FFI'];

export function run(spec, opts) {
  if (typeof spec === 'string') {
    return docopt.run(spec, opts)();
  } else {
    return docopt.runFromSpec(spec, opts)();
  }
}

export function parse(...args) {
  return docopt.parse(...args)();
}
