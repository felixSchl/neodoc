var neodocLib = require('./lib.js')

module.exports.run = function (helpOrSpec, options = {}) {
  options.argv = (options.argv || process.argv || []).slice(2)
  options.env = Object.entries(options.env || process.env || {})

  if (helpOrSpec) {
    if (typeof helpOrSpec === 'string') {
      return neodocLib.runStringJs(helpOrSpec)(options)
    }
    else if (typeof helpOrSpec === 'object') {
      return neodocLib.runSpecJs(helpOrSpec)(options)
    }
  }

  throw new Error('Either provide a help text or a command specification')
}
