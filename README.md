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
* [ ] Transform the parsed args into something more useful
* [ ] Provide seamless interface to be called from JS
* [ ] Provide typescript typings

Known issues to work through:

* [ ] Options that were not provided through argv but that have defaults, are
      currently not present in the output
* [ ] Options that have a default value and are provided through argv without an
      argument should produce an error if the argument type is not a boolean
      (toggle)

Further, the wishlist looks somewhat like this:

* Allow boolean negation. Let `--foo` be an option of type boolean, implicitly
  allow it's negation `--no-foo`.
* Refactor the `Docopt.Spec.Parser.Usage.OptionStack` constructor to use
  `NonEmpty` from `purescript-nonempty`
* Options should be able to specify default values straight in the
  Usage section, i.e.: `Usage: foo --bar=100`. This *will* have an impact on the
  solver implementation, however.
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
    1. **Transform into a usable form**

<sub>\*Highlighted items are yet-to-be-done.</sub>


---


## Ideas for a future past the initial relase ##

* Write syntax plugin for vim
* **Formalise the options** text layout in order to add more helpful information
  about options, consider:
  ```sh
  Usage: program --foo

  Options:
    -f, --foo  this is foo and does something great
                [default: 100]
                [implies: --qux, --baz=10]
                [choices: 0|10|100]
  ```
* **Make commands first-class citizens**. Many utilities break their
  functionality across sub-commands. Consider git _(shortened for clarity)_:
  ```sh
  $ git --help
  usage: git [--version] [--help] <command> [<args>]

  $ git branch --help
  SYNOPSIS
     git branch [--color[=<when>] | --no-color] [-r | -a]
             [--list] [-v [--abbrev=<length> | --no-abbrev]]
             [--column[=<options>] | --no-column]
             [(--merged | --no-merged | --contains) [<commit>]] [<pattern>...]
     git branch [--set-upstream | --track | --no-track] [-l] [-f] <branchname> [<start-point>]
  ```

  In this example `<command>` was `branch`.
  This means that `git --version --help branch` &mdash; albeit non-sensical
  &mdash; is a valid way to use git.

  This could perhaps be documented as such:

  ```sh
  usage: git [--version] [--help] [-C <path>] [-c name=value]
          [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
          [-p|--paginate|--no-pager] [--no-replace-objects] [--bare]
          [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
          <command> [<args>]

  The most commonly used git commands are:
    add        Add file contents to the index
    bisect     Find by binary search the change that introduced a bug
    branch     List, create, or delete branches
    [...]
    rm         Remove files from the working tree and from the index
    show       Show various types of objects

  'git help -a' and 'git help -g' lists available subcommands and some
  concept guides. See 'git help <command>' or 'git help <concept>'
  to read about a specific subcommand or concept.
  ```

  This has the following implications:
    * How to detect a commands section? Something in the title?
    * Is `<command>` special?

[docopt-orig]: http://docopt.org
