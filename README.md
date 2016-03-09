# Docopt #

> [WIP] JS/Purescript implementation of docopt, available on npm.

[![Build Status](https://travis-ci.org/felixSchl/docopt.svg?branch=development)](https://travis-ci.org/felixSchl/docopt)

This is a fairly ambitious implementation of docopt _plus more_. The idea of
docopt is great, but the language is still rather informal. It's a fine balance
walk between keeping the docopt text fairly prose, but structured enough to
extract meaningful information.

_For those unfamiliar with docopt, [refer to the original first][docopt-orig]._

## Project goals ##

* Provide a declarative way to author command lines. Rather than deriving the
  help text from some DSL, derive it from a human readable, prose-like EDSL,
  located e.g. in the project's README. **This guarantees documentation and
  implementation never diverge, because they simply can't.**
* Provide **very good error reporting** for users of the CLI and at least decent
  error reporting for developers authoring the docopt text.
* The manually crafted docopt text must be **readable, standardised, yet
  flexible and powerful** enough to handle a fair set of use-cases.
* A solid interface for use in **regular javascript**. Purescript should merely
  be an implementation detail.
* **Solid test coverage**

## Project status ##

* [x] Scan the docopt text for usage sections and 0 or more description sections
* [x] Lex and parse usage sections
* [x] Lex and parse description sections
* [x] Resolve ambiguities by combining the parsed usage and description sections
* [x] Generate parser from the solved program specification
* [x] Lex and parse user input on the CLI
* [x] Transform the parsed args into something more useful
* [ ] Provide seamless interface to be called from JS
* [ ] Provide typescript typings
* [ ] Fix all warnings
* [x] Read arguments from env vars
* [x] Implement special arguments
    * [x] `--` (end of args) not yet implemented
    * [x] `-` (stdin)
    * [x] `[options]`

Known issues to work through:

* [x] ~~Options that were not provided through argv but that have defaults, are
      currently not present in the output~~
* [x] ~~Options that have a default value and are provided through argv without an
      argument should produce an error if the argument type is not a boolean
      (toggle)~~

Further, the wishlist looks somewhat like this:

* Put the "source" of a parsed option's value into the output, e.g. "env",
  "default", "user"
* Read options from config file
* Make commands first class citizens, enabling easy subcommands, inheriting
  options and all that.
* Allow for `--foo[=<bar>]` syntax (git style).
* Provide warnings. This would mean a largish refactor to use either a custom
  monad, or the Writer monad, stacked on top of the Either monad.
* Allow boolean negation. Let `--foo` be an option of type boolean, implicitly
  allow it's negation `--no-foo`.
* Refactor `Language.Docopt.ParserGen.Parser.genBranchParser` to use manual
  recursive iteration, rather than a fold, like in `Language.Docopt.Solver`.
* Rewrite recursive functions to be in tail position, using
  `purescript-tailrec` and consider trampolining using `purescript-free`.

## Contributing ##

**Contributions are highly encouraged and welcome** &mdash; I really want to see
this project finished so I can use it :smile:

```sh
$ bower install
$ pulp test
```

During development, `pulp --watch test` (or `pulp -w test`) is extremely useful
to get immediate feedback.

### Implementation ###

> A quick overview of the implementation for potential contributors

The project can roughly be broken up into 4 distinct areas of work:

1. Scanning the docopt text:
    1. Derive at least 1 usage section
    1. Derive 0 or more description sections
1. Parsing the Specification
    1. Lex and parse the usage section
    1. Lex and parse any description sections
1. Solve the parsed Specification into it's canonical, unambigious form
1. Generate a parser from the Specification
1. Lex and parse the user input
    1. Lex and parse the user input
    1. Transform into a usable form

### Dev Notes ###

#### Reading options from config files ####

> Options should fall back values read from file.

Todo:

* [ ] Design/implement ...

Problems:

* The `--config` option (or similar) needs to be documented, in docopt. This
  is impossible without running twice, because in order to parse, all values
  and fall backs need to be already discovered.

---


## Ideas for a future past the initial relase ##

* Write syntax plugin for vim
* First class commands description sections (and `[options...]`)
    * Consider the following:
        ```sh
        Usage:
            git branch [options...] [branch-options...]
            #            ^                ^
            #            |                |
            #            |                `- Denotes common options
            #            |
            #            `- denotes options specific to `branch` command

        Common Options: # <-- Identify as match for `options` by substring
          -h, --help  Show this help and exit

        Branch Options: # <-- Identify as match for `branch-options` by substring
          -D <name>  Delete the branch identified by <name>
        ```
    * Or the following (multiple usage):
        ```sh
        Usage:
            git branch

        Git-branch usage: # <-- Identify as match for `branch` by substring
            git branch [options...] [-D]

        Git-branch options: # <-- Identify as match for `branch` by substring
            -D <name>  Delete the branch identified by <name>
        ```
        This approach is slightly more verbose, but might be easier for
        normalization for large projects across different files?


[docopt-orig]: http://docopt.org
