# Changelog

> Please note that all these tags mark releases that are available on npm with the
> respective version number - unless otherwise noted.

## [1.4.0] - 2017-02-17

### Changes

* Fix [#89]: `[options]` are now expanded without a surrounding group which used
  to cause patterns being discarded to eagerly due to partial matches

## [1.3.0] - 2016-11-02

### New features

* It is now possible to capture any unknown options, while still validating
  known options and positional/command arguments. With `opts.allowUnknown`
  toggled on, any unknown options are collected into a special key "?" in the
  output mapping. Note that, unknown input is collected "verbatim", i.e. as it
  was passed on the command line. This closes [#42].

## [1.2.0] - 2016-10-31

This release focused mostly on performance. While performance is drastically
improved with this release, it won't be the end of things in a continous effort
to get neodoc as snappy as possible, while retaining it's strong parsing
capabilities and feature set.

### Changes

* Update to purescript 0.10.x
* Improve overall performance [#81]
    * To give an idea, the uglify example now runs about 140ms faster than
      before!
    * Optimize large parts of the code-base (lexer, solver, arg-parser)
    * Pre-trim descriptions section to speed up descriptions lexing
    * Let go of purescript-parsing, use own parser everywhere
    * Avoid partial function application where feasible, especially if function
      takes many arguments.
    * Optimize many parser combinators to work especially well with chars
    * Run the compiled output through the closure compiler to bring down require
      times. This comes at the cost of readable output, but provides a rough
      20ms boost requiring neodoc, which is now around 50ms on the machines I
      tested on. Still not ideal, maybe we can shed more bloat.

## [1.1.0] - 2016-10-22

### Fix

* Fix [#72]: Add special flags to spec implicitely

### Changes

* Improve special flags behavior with `opts.dontExit`. Instead of just returning
  a string, return the output and add a special key `.help` for `opts.helpFlags`
  and `.version` for `opts.versionFlags`, respectively.
* Allow dots in option names
* Improve `OptionAlias` JS representation. It is now just a string. This makes
  it easy to write, read and validate `opts.helpFlags` and `opts.versionFlags`.

## [1.0.2] - 2016-10-20

### Changes

* Fix [#80]: Allow slashes in command names

## [1.0.1] - 2016-10-20

### Fix

* Fix bug in foreign instance, where `opts.repeatableOptions` would not be read
  properly

## [1.0.0] - 2016-10-19 :tada:

The first major release of neodoc.

### Where the project is at

As a run up to v1, I decided to spend a lot of my time refactoring the code-base
for various reasons. Primarily the core data structures that were employed prior
to the refactor were insufficiently flexible and, looking back, clearly the
wrong choice. The new core data structure, which resembles a tree, is by far
more elegant and flexible, allows transformations and traversals and is more
obviously "correct". Secondly, I acknowledged that the code was hard to follow
and hindered adoption / contributors from getting into the code base. The most
important pieces of the codebase are now documented and hopefully approachable
enough.

### New features

* Neodoc now sports a couple of hooks that allow devs to hook into the
  transformation of the spec. See `opts.transforms` in the README for more
  information.
* `--version` will now simply return the version as a string if `opts.dontExit`
  is true.
* `--help` will now simply return the help as a string if `opts.dontExit` is
  true.
* A new option `options.repeatableOptions` is introduced that allows consuming
  excess options. Many command line utilities do not care about excess options,
  so now neodoc can be told to not care either. This means that after
  successfully parsing a bunch of options, any option may-reoccur again.
* Better error reporting. The parser now tracks the deepest error across all
  branches, making for more intuitive error messages.
* The spec is now being canonicalised internally which helps create (a) better
  error messages and (b) to speed up the parser.
* **considerably** better performance under difficult configurations and edge
  cases, i.e. when combining `opts.laxPlacement` with `opts.requiredFlags` and
  so on.

### Changes

* The "specification" format has changed drastically. If you've been using
  `neodoc.parse`, it's output is now entirely different and likewise the input
  to run expects the new `neodoc.parse` output. If you used this method to
  change the spec on-the-fly, you should have a look at the new
  `opts.transforms` option which allows you to easily hook into the
  transformation of the neodoc spec.

### Internals

* **Major** overhaul of the internals. The core data structures have changed and
  are now more composable.
  * The arg parser has been rewritten from the ground up, with next to none code
    sharing. The goal was a more efficient, simpler and better documented
    solution that is easier to reason about and debug.
  * Instead of representing a group as an argument, we now have a `Layout` data
    type which looks more like a classical tree:
    ```haskell
    data Layout a = Group IsOptional IsRepeatable Branches | Elem a
    ```
    It turns out that this structure holds true throught all the various stages
    of transformation.
  * There's finally a `Spec` datatype that formalises what a specification _is_:
    ```haskell
    type Branch a = NonEmpty List a
    type Toplevel a = List (Branch a)
    newtype Spec a = Spec {
      program      :: String
    , layouts      :: NonEmpty List (Toplevel a)
    , descriptions :: List Description
    , helpText     :: String
    , shortHelp    :: String
    }
    ```
  * A custom parser monad for arg-parsing that does **not** use monad
    transformers. It enables neodoc to better handle errors and should generally
    run faster. It allows the parser to keep read-only state, state that
    isolated in alternatives and when backtracking, as well as state that is
    "mutable", i.e. state that survives failures. In terms of running faster,
    the worst case that previously required adding in a cache now runs fine and
    the cache was removed.
  * Branches in groupings as well as top-levels are now encoded at the type
    level as `NonEmpty`
  * Move away from records to ADTs for speed and ease of use in many scenarios
  * Use `AsForeign` and `IsForeign` for FFI needs
  * Improve test bed readability all over, make it more easy to add tests
  * Overall add more comments and document entire modules in an effort to make
    the code "contributable"

## [0.10.1] - 2016-08-18

### Fixes

* Fix [#70] - Ignore ANSI escape codes in parser. This allows the neodoc to
  be colored. For example:
  ```
  neodoc.run(`
  ${chalk.blue('Usage:')}
      prog <command> [<args>...]
  `);
  ```
  Thanks [@matthewmueller] for reporting
* Fix [#71] - Do not trim help text. This allows the developer to keep some left
  padding on the help text. The leading and trailing newlines are still removed,
  however, in order to make working with JS template strings easy.
  Thanks [@matthewmueller] for reporting
* Fix optional positionals in lax-placement mode.
  For example:
  ```
  usage: prog [foo] -a
  ```
  Would fail: `-a foo` before this patch because `[foo]` simply fails at `-a`
  and gets omitted. The added tests cover these cases.
* Fix 'stop-at' not working in groups in certain cases

## [0.10.0] - 2016-08-12

### New features

* Fix [#48] - Implement special support for `--help` and `--version`. If `--help`
  is encountered, print the original, full help text. The flags that trigger
  this behavior can be overridden using `options.helpFlags` (ex. `{ helpFlags:
  ["-h"] }`). To disable this behavior, pass it the empty list. Likewise, if
  `--version` is met, print the version of the CLI. The version can be
  explicitly set via `options.version`, but neodoc will otherwise fall back to
  look for a package.json of the executing main module (see `require.main`).
  The flags that trigger this behavior can be overriden using
  `options.versionFlags`. Again, use the empty list to turn this behavior of
  entirely.

#### Changes

* Only parse repeating option-arguments repeatedly if the first argument is
  **NOT** bound explicitly. Ex:
  ```
  usage: prog --foo=<bar>... qux

  $ prog --foo=10 qux => {"--foo": [10], "qux": true }
  $ prog --foo 10 qux => error! missing 'qux' (got consumed by --foo).
  ```

#### Fixes

* Fix case where parsing became extremely slow when `options.laxPlacement` was
  enabled.

## [0.9.2] - 2016-07-26

### Fixes

* Fix repeating positionals / commands in `options.laxPlacement` mode.

## [0.9.1] - 2016-07-26

### Fixes

* Fix `options.laxPlacement` - Ensure the positioning among positionals and
  commands remains in-tact.

## [0.9.0] - 2016-07-23

### New features

* Fix [#24] - Add an option `options.laxPlacement` that relaxes the rules around
  argument positioning. Options can now appear anywhere in the input.
* Fix [#64] - Options may now specify more than two aliases. Consider: `-h, -?,
  --help`

### Changes

* **Drastically improve arg parser performance** by caching previous parses.
  This turned out to make a big difference because of the sheer amount of
  permutations the parser has to deal with, parsing the same input many times
  over.

## [0.8.0] - 2016-07-17

### New features

* Fix [#61] - It is now possible to require flags be present in the input via
  `option.requireFlags`
* It is no possible to run from a spec, previously generated by `neodoc.parse`.
  This allows `neodoc.run(neodoc.parse(help), options)` style usage with the
  primary use-cases of caching and spec alteration in mind. Caching the
  `neodoc.parse` output as JSON, for example, would have a massive impact on
  the time it takes to parse user input. We are talking ~30x faster.

### Fixes

* Fix [#62] - remove unnecessary newlines lines from help output

### Changes

* Error messages have further **drastically improved**
* Neodoc now fails to parse the usage section if two usage rows use a different
  program name
* Neodoc now prints the program name in front of the error message
* Neodoc now prints the error message first, and then the usage. _Does this need
  to be configurable?_

## [0.7.0] - 2016-07-03

### Fixes

* Fix [#55] - Bind adjacent singleton group (optional and required) to the
  option to the left of it, if the options description indicates so.

### Changes

* Parse optional adjacent arg in option description section. For example:
  ```
  usage: prog [options]
  options:
    -f [BAR]  ...
  ```
* Neodoc now performs even a litte faster.
* Improve error messages for unknown options. Check left-over arguments on
  argv to see if they appear anywhere in spec and if not, show a different
  error message indicating that the argument is unknown.
* Parse option-arguments repeatedly &mdash; **BREAKING CHANGE**\
  \
  Parse option-arguments repeatedly if the actual option-argument is said
  to repeat (via `...`). This way multiple arguments can be passed right
  after the option:
  ```
  usage: prog --foo=ARG...
  ```
  This would be able to match `$prog --foo 1 2 3` and yield: `{"--foo": [
  1, 2, 3 ]}`. The problem, however, is detecting when to stop parsing
  since it will eagerly consume any positionals and arguments required for
  a successful parse.
  \
  \
  The way it's implemented in this commit, the user would have the option
  to opt out of this behavior by wrapping the option in a singleton group:
  ```
  usage: prog (--foo=ARG)...
  ```
  This way, in order to repeat the option, the user would have to bring
  enter multiple `--foo`: `$prog --foo 1 --foo 2 --foo 3`.
  \
  \
  Note, however that even with the former usage, the latter is possble:
  `$prog --foo 1 2 --foo 3` would be equivalent to `$prog --foo 1 2 3`


## [0.6.1] - 2016-06-24

### Changes

* Neodoc now performs even a litte faster.

## [0.6.0] - 2016-06-23

### Changes

* Neodoc now performs a lot faster, especially around lexing and parsing.

## [0.5.0] - 2016-06-17

### New features

* `options.stopAt` has been expanded to also work on options.
  Note that if an option has an alias, `stop-at` will cause to stop parsing for
  any of it's aliases. So if `stop-at` is set to '["-n"]', and "-n" is defined
  as "-n, --never", the parser will stop both for "-n" and "--never" (whichever
  is met earlier).

### Fixes

* Fixes **major bug** where the positional/command `foo` would resolve the same
  internal key as `--foo`, yielding the wrong value in the output map!

### Changes

* Disallow `--foo... BAR` if `--foo` is said to take an option-argument

### Internals

* Rewrite the entire arg parser module!

## [0.4.0] - 2016-06-07

### New features

* Add `neodoc.parse` which effectively exposes the neodoc spec to JS.
* Add support for a custom EOA (end of arguments separator). This allows to stop
  parsing at a given positional/command argument: `options.stopAt`. **Does not
  require `options.optionsFirst`**

### Fixes

* Fix [#41] - Parse fails at non-empty option section that contains no options
* Fix entire groups not repeating properly

### Changes

* Print back the original usage section ([#10])
* Dedent help output
* Remove positional argument aliases from output map &mdash; **BREAKING CHANGE**
  The reasoning is to keep the output canonical and clean.
* Improve the options parser:
    * Allow for (commas are optional)
      ```
      -f, --foo
      --foo, -f
      -f=ARG, --foo # no argument-mismatch error
      --foo, -f=ARG # no argument-mismatch error
      ```
    * Do not swallow bad parses and provide meaningful error messages
* Allow `-?` as a valid flag

### Internals

* A big refactor took place. Things moved, got renamed, made into Records and so
  on. The codebase is a lot tidier now.

## [0.3.0] - 2016-05-25

### Fixes

* Fix [#31] - Allow `[--]` to denote optional `--`
* Fix [#33] - Allow to omit option-argument in either option or usage section if
  it is denoted as optional in at least one section.
* Fix [#34] - Fix option parser tripping over `--` in option an options
  description text in the options section.
* Lift strict alignment rules of options in the description section.
  A new option or positionals description is assumed if it is *less* indented
  than the preceding text block of descriptions

## [0.2.1] - 2016-05-11

### Changes

* Update README to contain a link to the playground

## [0.2.0] - 2016-05-10

### Changes

* Inherit repeatability in nested groups
* Populate values at parser level &mdash; **BREAKING CHANGE**
  \
  \
  This is a fairly large re-think of how argument values are fetched from
  the their various input sources. Before, values would be loaded during
  a step after parsing, where the matched usage branch would be completely
  unrolled, unified and then populated. This had the disadvantage that one
  could not tell which of the mutually exclusive branches produced a
  result, since one value might arrive from ARGV and another one from the
  `[default: ...]` tag, or the environment, etc. (See [#8])
  \
  \
  Now, values are loaded at parse-time, removing these issues. Further,
  "empty" values are elided from the output, representing the user *not
  trying* to match these keys (as opposed to choosing to set them
  explicitly). For example, the user might pass the value 'false' to an
  option and that value will be retained. If the option however yields
  false because there was no user input and because 'false' is its' empty
  fall-back, the value will be omitted. The same goes for matching
  repeating elements into an array. At least one element needs to be
  matched before a value will be yielded.

## [0.1.0] - 2016-05-05

:tada: This marks the first minor release for neodoc. Things are stabilising.

### Changes

* Fail fatally if an option's required option-argument is missing from the
  input. The parser won't even attempt to try any other branches &mdash; it will
  fail immediately. The same applies for providing an argument to an option that
  does not take an argument.

### Internals

* Run the compat spec parser on a trampoline to avoid stack overflows

## [0.0.15] - 2016-05-03

### Changes

* Stricter lexing of options:
    * Disallow spaces between `-f` and `[=OPTARG]` (only allow `-f[=OPTARG]`
      without any spaces).
    * Disallow '-' after '--' and '-' to make them distinct
    * Disallow '-' after any option
    * Disallow '[' and '(' right after any option

### Fixes

* Fix number value parser for user input
* Short options - parse option-argument values if the option-argument was
  assigned explicitly

## [0.0.14] - 2016-05-02

### Changes

* Do not interpret quoted options, e.g.: `prog '-x = foo'` would currently yield
  `-x=foo`. As of this version, it will simply yield the string `-x = foo`.
  Explanation: Since the shell does the argument splitting for us, we receive as
  an argument the string `"-x = foo"` as opposed to three arguments `-x`, `=`
  and `foo` (which would, rightfully, result in `-x="=" foo`).
* Enforce EOF at end of usage section
* Ensure `--[no-tags]` won't parse for now since flag negation behavior is not
  yet implemented and the semantics will change once it is.
* Improve overall error reporting

## [0.0.13] - 2016-05-01

### Fixes

* Allow termination via `options-first` inside a group. Parsing will simply
  halt, no matter how deep in a group once a positional / command argument is
  encountered ([#21])
* Fix ambiguous fallbacks not resolved across groups ([#22])

## [0.0.12] - 2016-04-27

### Fixes

* Fix repeating option not being required at least once.

### Changes

* Values parsed on argv that have commas in them are no longer parsed into a
  list of values. This behavior was too specific, since who decides what the
  delimiter should be? Developers are advised to either a) use repeating
  arguments or b) split the value themselves after parsing. This change does
  not, however, have an impact on the parsing of the `[default]` tag - those
  values will still be parsed into lists since it's value is in the developer's
  control._This could be revised, if necessary_.
* Select fallbacks in ambiguous parses based on a score, where environment
  values based fallbacks rank higher than provided `[default]` values, etc.

## [0.0.11] - 2016-04-26

### New features

* Implement `smart-opts`. This is a new behavior that allows explicitly
  (as opposed to loosely) binding an option-argument to an option, just by
  placing it into a singleton group. For example: `usage: foo [-o FILE]`. In
  this example, given that `options.smartOpts` is `true`, no additional option
  description section is required in order to bind `FILE` to `-o`.

### Changes

* Lift the constraint on positionals / commands qualifying as candidates for
  `options-first`. Before, it was required that this argument be indicated as
  optional. This requirement has now been lifted.

## [0.0.10] - 2016-04-25

### New features

* Implement proper `[options]` expansion. This means that `[options]` is now
  aware of it's surroundings and won't expand options already present.
  This allows to intermix `[options]` with explicitly mentioned options.

## [0.0.9] - 2016-04-16

### New features

* It is now possible to indicate the repeatability of an option in the option
  description section.

### Changes

* In case of ambigious parses across group branches, select the left-most match.

## [0.0.8] - 2016-04-18

### New features

* Add the `options.dontExit` option to `neodoc.run` (defaults to `false`).
  Turning this option on will cause neodoc to throw an error, rather than
  quitting the program.

### Changes

* Error messages now longer show squiggly error lines. The behavior was neither
  aesthetically pleasing nor very helpful as a tool. Render simple error
  messages instead
* Improve error message for mutually exclusive branches in a group
* Add examples to the codebase
* Restrict angle names to not contain a opening angle (`<`). This means that the
  following name is now illegal: `<<foo>`. There is no technical reason to this
  limitation and was introduced merely to avoid subtle parse errors. _This could
  be revised, if necessary_.
* Disallow specifying an option twice in the options section
* Disallow associating an option-argument with a non-trailing stacked option
* Strictly check the adjacent arguments for the correct argument

## [0.0.7] - 2016-04-13

### Changes

* Relax the parsing of "free" groups &mdash; remove the restriction that in
  order for a group to be considered "free" it must be a singleton group.
  Now any group whose branches are all "free" is considered "free" itself.

### Internals

* Add more tests - increase scenario coverage

## [0.0.6] - 2016-04-11

Only internal changes and fix ups. Notably, simplify the lexer module by
removing all references to the `word` lexer which has been previously
decomissioned. Also avoid parsing the `Garbage` token when lexing the usage
section &mdash; let it fail at the lexing stage.

## [0.0.5] - 2016-04-08

### New features

* Add 'options-first' feature
* Add support for optional arguments

### Internals

* Add more tests - increase scenario coverage
* Overhaul the parser generator

## [0.0.4] - 2016-04-05

### Changes

* Print only usage section upon error as opposed to complete help text

## [0.0.3] - 2016-04-03

### Changes

* Lift the restriction on command and positional names. Names can now have
  dashes, underscores and dots

## [0.0.2] - 2016-04-02

:tada: This marks the first generally available release of neodoc

[@matthewmueller]: https://github.com/matthewmueller

[#89]: https://github.com/felixSchl/neodoc/issues/89
[#81]: https://github.com/felixSchl/neodoc/issues/81
[#80]: https://github.com/felixSchl/neodoc/issues/80
[#79]: https://github.com/felixSchl/neodoc/issues/79
[#78]: https://github.com/felixSchl/neodoc/issues/78
[#77]: https://github.com/felixSchl/neodoc/issues/77
[#76]: https://github.com/felixSchl/neodoc/issues/76
[#75]: https://github.com/felixSchl/neodoc/issues/75
[#74]: https://github.com/felixSchl/neodoc/issues/74
[#73]: https://github.com/felixSchl/neodoc/issues/73
[#72]: https://github.com/felixSchl/neodoc/issues/72
[#71]: https://github.com/felixSchl/neodoc/issues/71
[#70]: https://github.com/felixSchl/neodoc/issues/70
[#69]: https://github.com/felixSchl/neodoc/issues/69
[#68]: https://github.com/felixSchl/neodoc/issues/68
[#67]: https://github.com/felixSchl/neodoc/issues/67
[#66]: https://github.com/felixSchl/neodoc/issues/66
[#65]: https://github.com/felixSchl/neodoc/issues/65
[#64]: https://github.com/felixSchl/neodoc/issues/64
[#63]: https://github.com/felixSchl/neodoc/issues/63
[#62]: https://github.com/felixSchl/neodoc/issues/62
[#61]: https://github.com/felixSchl/neodoc/issues/61
[#60]: https://github.com/felixSchl/neodoc/issues/60
[#59]: https://github.com/felixSchl/neodoc/issues/59
[#58]: https://github.com/felixSchl/neodoc/issues/58
[#57]: https://github.com/felixSchl/neodoc/issues/57
[#56]: https://github.com/felixSchl/neodoc/issues/56
[#55]: https://github.com/felixSchl/neodoc/issues/55
[#54]: https://github.com/felixSchl/neodoc/issues/54
[#53]: https://github.com/felixSchl/neodoc/issues/53
[#52]: https://github.com/felixSchl/neodoc/issues/52
[#51]: https://github.com/felixSchl/neodoc/issues/51
[#50]: https://github.com/felixSchl/neodoc/issues/50
[#49]: https://github.com/felixSchl/neodoc/issues/49
[#48]: https://github.com/felixSchl/neodoc/issues/48
[#47]: https://github.com/felixSchl/neodoc/issues/47
[#46]: https://github.com/felixSchl/neodoc/issues/46
[#45]: https://github.com/felixSchl/neodoc/issues/45
[#44]: https://github.com/felixSchl/neodoc/issues/44
[#43]: https://github.com/felixSchl/neodoc/issues/43
[#42]: https://github.com/felixSchl/neodoc/issues/42
[#41]: https://github.com/felixSchl/neodoc/issues/41
[#40]: https://github.com/felixSchl/neodoc/issues/40
[#39]: https://github.com/felixSchl/neodoc/issues/39
[#38]: https://github.com/felixSchl/neodoc/issues/38
[#37]: https://github.com/felixSchl/neodoc/issues/37
[#36]: https://github.com/felixSchl/neodoc/issues/36
[#35]: https://github.com/felixSchl/neodoc/issues/35
[#34]: https://github.com/felixSchl/neodoc/issues/34
[#33]: https://github.com/felixSchl/neodoc/issues/33
[#32]: https://github.com/felixSchl/neodoc/issues/32
[#31]: https://github.com/felixSchl/neodoc/issues/31
[#30]: https://github.com/felixSchl/neodoc/issues/30
[#29]: https://github.com/felixSchl/neodoc/issues/29
[#28]: https://github.com/felixSchl/neodoc/issues/28
[#27]: https://github.com/felixSchl/neodoc/issues/27
[#26]: https://github.com/felixSchl/neodoc/issues/26
[#25]: https://github.com/felixSchl/neodoc/issues/25
[#24]: https://github.com/felixSchl/neodoc/issues/24
[#23]: https://github.com/felixSchl/neodoc/issues/23
[#22]: https://github.com/felixSchl/neodoc/issues/22
[#21]: https://github.com/felixSchl/neodoc/issues/21
[#20]: https://github.com/felixSchl/neodoc/issues/20
[#19]: https://github.com/felixSchl/neodoc/issues/19
[#18]: https://github.com/felixSchl/neodoc/issues/18
[#17]: https://github.com/felixSchl/neodoc/issues/17
[#16]: https://github.com/felixSchl/neodoc/issues/16
[#15]: https://github.com/felixSchl/neodoc/issues/15
[#14]: https://github.com/felixSchl/neodoc/issues/14
[#13]: https://github.com/felixSchl/neodoc/issues/13
[#12]: https://github.com/felixSchl/neodoc/issues/12
[#11]: https://github.com/felixSchl/neodoc/issues/11
[#10]: https://github.com/felixSchl/neodoc/issues/10
[#9]: https://github.com/felixSchl/neodoc/issues/9
[#8]: https://github.com/felixSchl/neodoc/issues/8
[#7]: https://github.com/felixSchl/neodoc/issues/7
[#6]: https://github.com/felixSchl/neodoc/issues/6
[#5]: https://github.com/felixSchl/neodoc/issues/5
[#4]: https://github.com/felixSchl/neodoc/issues/4
[#3]: https://github.com/felixSchl/neodoc/issues/3
[#2]: https://github.com/felixSchl/neodoc/issues/2
[#1]: https://github.com/felixSchl/neodoc/issues/1

[1.4.0]: https://github.com/felixschl/neodoc/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/felixschl/neodoc/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/felixschl/neodoc/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/felixschl/neodoc/compare/v1.0.2...v1.1.0
[1.0.2]: https://github.com/felixschl/neodoc/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/felixschl/neodoc/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/felixschl/neodoc/compare/v0.10.1...v1.0.0
[0.10.1]: https://github.com/felixschl/neodoc/compare/v0.10.0...v0.10.1
[0.10.0]: https://github.com/felixschl/neodoc/compare/v0.9.2...v0.10.0
[0.9.2]: https://github.com/felixschl/neodoc/compare/v0.9.1...v0.9.2
[0.9.1]: https://github.com/felixschl/neodoc/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/felixschl/neodoc/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/felixschl/neodoc/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/felixschl/neodoc/compare/v0.6.1...v0.7.0
[0.6.1]: https://github.com/felixschl/neodoc/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/felixschl/neodoc/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/felixschl/neodoc/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/felixschl/neodoc/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/felixschl/neodoc/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/felixschl/neodoc/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/felixschl/neodoc/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/felixschl/neodoc/compare/v0.0.15...v0.1.0
[0.0.15]: https://github.com/felixschl/neodoc/compare/v0.0.14...v0.0.15
[0.0.14]: https://github.com/felixschl/neodoc/compare/v0.0.13...v0.0.14
[0.0.13]: https://github.com/felixschl/neodoc/compare/v0.0.12...v0.0.13
[0.0.12]: https://github.com/felixschl/neodoc/compare/v0.0.11...v0.0.12
[0.0.11]: https://github.com/felixschl/neodoc/compare/v0.0.10...v0.0.11
[0.0.10]: https://github.com/felixschl/neodoc/compare/v0.0.9...v0.0.10
[0.0.9]: https://github.com/felixschl/neodoc/compare/v0.0.8...v0.0.9
[0.0.8]: https://github.com/felixschl/neodoc/compare/v0.0.7...v0.0.8
[0.0.7]: https://github.com/felixschl/neodoc/compare/v0.0.6...v0.0.7
[0.0.6]: https://github.com/felixschl/neodoc/compare/v0.0.5...v0.0.6
[0.0.5]: https://github.com/felixschl/neodoc/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/felixschl/neodoc/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/felixschl/neodoc/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/felixSchl/neodoc/releases/tag/v0.0.2
