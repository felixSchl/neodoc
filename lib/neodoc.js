import * as PS from './neodoc.purs.min.js'
const docopt = PS['Neodoc'];

export function run(spec, opts) {
  return docopt.runJS(spec, opts)();
}

export function parse(...args) {
  return docopt.parseHelpTextJS(...args)();
}

export default { run, parse };
