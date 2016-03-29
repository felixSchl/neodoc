import * as PS from './docopt.purs.js'
const docopt = PS['Docopt.FFI'];

export default function run(...args) {
  return docopt.run(...args)();
}

console.log(run(
  `
  Usage: prog --foo=<x>

  Options:
    -f, --foo  blah [env: FOOBAR]
  `
, { argv: [], env: { 'FOOBAR': 'baz' } }
))
