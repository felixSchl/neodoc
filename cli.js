const neodoc = require('./lib.js')

helpText = `
Usage: neodoc [options]

Options:
  --dont-exit  Do not exit upon error or when parsing --help or --version.
  --env  Override process.env
  --argv  Override process.argv
  --options-first  Parse until the first command or <positional> argument, then collect the rest into an array, given the help indicates another, repeatable, positional argument.
  --smart-options  Enable parsing groups that "look like" options as options.
  --stop-at  Stop parsing at the given options, i.e. [ -n ].
  --require-flags  Require flags be present in the input. In neodoc, flags are optional by default and can be omitted.
  --lax-placement  Relax placement rules. Positionals and commands are no longer solid anchors.
  --version-flags  An array of flags that trigger the special version behavior: Print the program version and exit with code 0.
  --version  The version to print for the special version behavior.
  --help-flags  An array of flags that trigger the special help behavior: Print the full program help text and exit with code 0.
  --repeatable-options  Allow options to be repeated even if the spec does not explicitly allow this.
  --transforms-presolve  An array of functions to be called prior to "solving" the input.
  --transforms-postsolve  An array of functions to be called after "solving" the input, just prior to passing the spec to the arg-parser.
  --allow-unknown  Collect unknown options under a special key ? instead of failing.

Examples:
  neodoc --options-first < help.txt
`

console.info(
  neodoc.run(helpText, {laxPlacement: true})
)
