<div align="center">
    <h1>
        <strong><sub>&lt;</sub> neodoc <sub>&gt;</sub></strong>
    </h1>
    <em><sub></sub></em>
</div>

<p align="center">
  <a href="https://travis-ci.org/felixSchl/docopt">
    <img src="https://travis-ci.org/felixSchl/docopt.svg?branch=development?style=flat-square"
         alt="Build Status">
  </a>
<br/>

<p align="center">
    <b><a href="#about">About</a></b>
  | <b><a href="#installation">Installation</a></b>
  | <b><a href="#usage">Usage</a></b>
  | <b><a href="#project-goals">Goals</a></b>
  | <b><a href="#project-status">Status</a></b>
  | <b><a href="#contributing">Contributing</a></b>
  | <b><a href="#license">License</a></b>
</p>

<br/>

## About

<strong>&lt;neodoc&gt;</strong> is a revised implementation of the [docopt
language][docopt-orig] for node. In brief, it offers a unique way to author
command lines by writing the command line's help text first and then deriving
a matching parser from it, which can then be applied to user input. The
advantages are numerous:

* Documentation and implementation are always in sync
* Great discoverability for the user
* Provokes thinking about the user first
* No awkward, unreadable EDSL

This implementation features **error reporting**, both for users and developers,
reading values from **environment variables**, type coercion and much more. For
an (in-)comprehensive comparison to the original, click
[here](#deviations-from-the-original).

## Installation ##

```sh
npm install --save neodoc
```

## Usage ##

> Basic usage example. For more detail on what neodoc can do, have a look at
> `testcases.docopt` in the repo.

1. Given this node.js program:
    ```javascript
    #!/usr/bin/env node

    import neodoc from 'neodoc';
    const argv = neodoc.run(`
      Naval Fate.

      Usage:
        naval_fate ship new <name>...
        naval_fate ship <name> move <x> <y> [--speed=<kn>]
        naval_fate ship shoot <x> <y>
        naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
        naval_fate -h | --help
        naval_fate --version

      Options:
        -h --help     Show this screen.
        --version     Show version.
        --speed=<kn>  Speed in knots [default: 10].
        --moored      Moored (anchored) mine.
        --drifting    Drifting mine.
    `);

    console.log(argv);
    ```

2. We can provide various input:

    ```bash
    $ ./prog ship new foo bar baz
      {'<name>': ['foo', 'bar', 'baz'],
       'NAME': ['foo', 'bar', 'baz'],
       'new': true,
       'ship': true}

    $ ./prog ship foo move 10 10 --speed
      Naval Fate.

      Usage:
        naval_fate ship new <name>...
        naval_fate ship <name> move <x> <y> [--speed=<kn>]
        naval_fate ship shoot <x> <y>
        naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
        naval_fate -h | --help
        naval_fate --version

      Options:
        -h --help     Show this screen.
        --version     Show version.
        --speed=<kn>  Speed in knots [default: 10].
        --moored      Moored (anchored) mine.
        --drifting    Drifting mine.

      Trailing input: --speed:
      > ship foo move 10 10 --speed
                            ^^^^^^^
    ```

## Project goals ##

> Overview of the short- and longterm goals of this project

* Provide a declarative way to author command lines. Rather than deriving the
  help text from some EDSL, derive the CLI from a human readable, prose-like help
  text, located e.g. in the project's README. **This guarantees documentation and
  implementation never diverge, because they simply can't.**
* Provide **very good error reporting** for users of the CLI and at least decent
  error reporting for developers authoring the docopt text.
* The manually crafted docopt text must be **readable, standardised, yet
  flexible and powerful** enough to handle a fair set of use-cases.
* A solid interface for use in **regular javascript**. Purescript should merely
  be an implementation detail.
* **Solid test coverage**
* Sensible compatibility with original docopt.

### Deviations from the original ###

> This implementation tries to be compatible where sensible, but does cut ties
> when it comes down to it. The universal docopt test suite has been adjusted
> accordingly.

* **Better Error reporting.** Let the user of your utility know why his input
  was rejected.
* **No abbreviations:**
  `--ver` does not match `--verbose`.
  <sub>[(mis-feature in the original implementation)](https://github.com/docopt/docopt/issues/104)</sub>
* **Alias matches.** If `--verbose` yields a value, so will `-v` <sub>(given
  that's the assigned alias)</sub>. Likewise, `FOO` yields value `<foo>` as
  well as `FOO`, and `<foo>` yields `FOO` and `<foo>`.
* **Flags are optional,** always. There's no reason to force the user to
  explicitely pass an option that takes no argument. The absence of the flag
  speaks - the flag will be set to `false`. This is also the case for flags
  inside required groups. E.g.: The group `(-a -b)` will match inputs `-a -b`,
  `-ab`, `-ba`, `-b -a`, `-b`, `-a` and the empty input.
* **All arguments in a group are always required**. This is regardless of
  whether or not the group itself is required or not, i.e.:

  ```sh
  Usage: prog [<name> <type>]
  ```

  will fail `prog foo`, but pass `prog foo bar`. The rational being that this is
  more general, since if the opposite behaviour (any match) was desired, it
  could be expressed as such:

  ```sh
  Usage: prog [[<name>] [<type>]]
  ```

  **note:** this rule excludes flags/switches and options that have default
  values (or other fallback values).
* **There is no `null`** in the resulting value map. `null` simply means not
  matched - so the key is omitted from the resulting value map. <sub>(this is
  still under consideration)</sub>
* **Environment variables**. Options can fall back to environment variables,
  if they are not explicitely defined. The order of evaluation is:
    1. User input (per `process.argv`)
    1. Environment variables (per `[env: ...]` tag)
    1. Option defaults (per `[default: ...]` tag)

## Project status ##

### Beta Target ####

> The work to be done to release a usable product with some known caveats.
> This release will serve as way to collect feedback and plan improvements for
> subsequent releases based on the feedback.

* [x] Scan the docopt text for usage sections and 0 or more description sections
* [x] Lex and parse usage sections
* [x] Lex and parse description sections
* [x] Resolve ambiguities by combining the parsed usage and description sections
* [x] Generate parser from the solved program specification
* [x] Lex and parse user input on the CLI
* [x] Transform the parsed args into something more useful
* [x] Run against docopt test-suite <sub>99% done</sub>
* [x] Provide developer and user error reporting
* [x] Provide seamless interface to be called from JS
* [x] Read arguments from env vars
* [x] Implement special arguments
    * [x] `--` (end of args) not yet implemented
    * [x] `-` (stdin)
    * [x] `[options]`

### Post-Beta Roadmap ####

> Overview of where docopt is headed, ordered (somewhat) by estimated priority.

* [ ] Implement optional option arguments: `-a [foo]`.
* [ ] Implement `--help` and `--version`. The developer will be able to specify
  the option that will trigger the `--help` and `--version` convenience
  functions, with fallbacks to `--help` and `--version`.
  * [ ] Implement `--help`. If matched, print the docopt text.
  * [ ] Implement `--version`. If matched, print the npm package version.
* [ ] Read options from config file
* [ ] Allow for `--foo[=<bar>]` syntax (git style).
* [ ] Add type validation via tags, e.g.: `[type: string]`
* [ ] Auto-infer types when not specified (e.g. numbers, strings, booleans)
* [ ] Allow flag negation sintax `--[no-]foo`: `--foo`, `--no-foo`, `-f`, `+f`

### Wishlist ###

> A list of things that are desired, but have not found a place on the roadmap.

* Fix all purescript warnings
* Custom validations (see `[type: ...]` tag)
* Provide typescript typings
* Read options from config file
* Read options from prompt (Add a `[prompt]` tag)
* Syntax plugins (vim, ...)
* Put the "source" of a parsed option's value into the output, e.g. "env",
  "default", "user"
* Consider  `+o` syntax: `-o, --option` and it's negation: `--no-option` or
  `+o`.
* Make commands first class citizens, enabling easy subcommands, inheriting
  options and all that.
* Provide warnings. This would mean a largish refactor to use either a custom
  monad, or the Writer monad, stacked on top of the Either monad.
* Allow boolean negation. Let `--foo` be an option of type boolean, implicitly
  allow it's negation `--no-foo`.
* Refactor `Language.Docopt.ParserGen.Parser.genBranchParser` to use manual
  recursive iteration, rather than a fold, like in `Language.Docopt.Solver`.
* Rewrite recursive functions to be in tail position, using
  `purescript-tailrec` and consider trampolining using `purescript-free`.

## Contributing ##

**Contributions are highly encouraged and welcome**.
Bug fixes and clean ups need not much coordination, but if you're interested
in contributing a feature, contact me at felixschlitter@gmail.com and we can
get the ball rolling &mdash; There's plenty to do. To get up and running, run:

```sh
npm i && bower i && npm test
```

NB: Purescript is declared a devDependency in package.json, so no need to bring
your own. Also, during development, `npm run watch` is extremely useful to get
immediate feedback.

## License ##

<strong>&lt;neodoc&gt;</strong> is released under the **MIT LICENSE**.
See file `LICENSE` for a more detailed description of it's terms.

---

### Implementation overview ###

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

Options:
* Can this be a developer-provided file or lookup rather than on the command
  line?
* Associate some option or positional argument with a config file lookup. This
  would end in some "special" code, where the parser is run twice (first, prove
  the associated option / positional arg was parsed, ignoring any failures for
  the time being, then run again with the config file default values).

[docopt-orig]: http://docopt.org
