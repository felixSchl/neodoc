var neodocLib = require('./lib.js')

module.exports.run = function (helpOrSpec, options = {}) {
  options.argv =
    (Array.isArray(options.argv)
      ? options.argv
      : (process.argv || [])
    ).slice(2)
  options.env = Object.entries(
    options.env && typeof options.env == 'object'
      ? options.env
      : (process.env || {})
    )

  var result

  if (helpOrSpec) {
    if (typeof helpOrSpec === 'string') {
      result = neodocLib.runStringJs(helpOrSpec)(options)
    }
    else if (typeof helpOrSpec === 'object') {
      result = neodocLib.runSpecJs(helpOrSpec)(options)
    }
  }

  if (!result ) {
    throw new Error('Either provide a help text or a command specification')
  }

  if (result.errors) {
    throw new Error(result.errors)
  }

  return Array.isArray(result)
    ? Object.fromEntries(result)
    : result
}
