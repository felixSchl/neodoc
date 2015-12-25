# Docopt #

> [WIP] Purescript implementation of docopt

This is a fairly ambitious implementation of docopt _plus more_. The idea of
docopt is great, but the language is still rather informal. It's a fine balance
walk between keeping the docopt text fairly prose, but structured enough to
extract meaningful information.

For those unfamiliar with docopt, [refer to the original first][docopt-orig].

## Project goals ##

* There should be **very good error reporting** for users and at least decent
  error reporting for developers authoring the docopt text.
* The manually crafted docopt text must be **readable, standardised, yet
  flexible and powerful** enough to handle a fair set of use-cases.
* Should have a solid interface for use in **regular javascript**. Purescript
  should merely be an implementation detail.
* **Solid test coverage**

## Project status ##

The project is coming together slowly, but steadily. Three of major hurdles have
been \*mostly\* overcome: Parsing of the specification, generating a parser
from the specification and in part applying it to the user input. Much of the
work has been placed into testing the components, which in turn has helped to
achieve faster turn-arounds and more lasting / solid results.

The most urgent areas that need to be worked on are:

* Specification solving: A pure function, roughly of shape:
  `Usage -> [ Option ] -> Either Error Specification`. This function needs
  to run in the either monad in order to capture errors during solving, i.e.
  a name clash or conflicting defaults, etc.
* Transform the parsed input into a datastructure useful to the user:
  `{ [key]: value }`, where options that have aliases, will be present
  multiple times.
* Low priority: Options should be able to specify default values straight in the
  Usage section, i.e.: `Usage: foo --bar=100`. This *will* have an impact on the
  solver implementation, however.

## Implementation ##

The project can roughly be broken up into 4 distinct areas of work:

1. Parsing the Specification
    1. Breaking apart the text into sections:
        * At least one usage section
        * Zero or more option sections
    1. Lex and parse the usage section
    1. Lex and parse any option sections
1. **Solve the parsed Specification into it's canonical, unambigious form**
1. Generate a parser from the Specification
1. Lex and parse the user input

<sub>Highlighted items are yet-to-be-done.</sub>

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
