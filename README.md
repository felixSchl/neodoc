<div align="center">
    <h1>
        <strong>&lt;neodoc&gt;</strong>
    </h1>
    <em>Beautiful, handcrafted commandlines</em>
</div>

<hr>

<p align="center">
  <a href="https://travis-ci.org/felixSchl/neodoc">
    <img src="https://travis-ci.org/felixSchl/neodoc.svg"
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

![preview](https://raw.githubusercontent.com/felixSchl/felixSchl.github.io/master/neodoc/neodoc.png)

<hr>

<br/>

## About

<strong>&lt;neodoc&gt;</strong> is a revised implementation of the [docopt
language][docopt-orig] for node. In brief, it offers a unique way to author
command lines by writing the command line's help text first and then deriving
a matching parser from it, which can then be applied to user input. The
advantages are numerous:

* No stagnant documentation - **your help-text is _necessarily_ correct**
* No awkward, unreadable EDSL - **beautiful, hand-crafted help texts instead**

This implementation features **error reporting**, both for users and developers,
reading values from **environment variables**, type coercion and much more. For
an (in-)comprehensive comparison to the original, click
[here](#deviations-from-the-original).

## Features ##

* Derive command line interface from help text
* Helpful error messages for developer and user
* _Options-first_ parsing to compose large programs (see example below)
* Fallback to alternate values:
    * User input -> Environment -> Defaults
* Convenient, concise and widely accepted POSIX-style syntax
    * _Mostly_ compatible with a typical `git <command> --help` output

## Installation ##

```sh
npm install --save neodoc
```

## Usage ##

### neodoc.run(docopt, opts)

Parse and apply the given docopt help text. If no options are provided, apply
it to `process.argv` and `process.env`. The result is a mapping of key -> value,
where the key is the canonical form of the option and its alias, if available.

Options:

* `opts.env` - Override `process.env`
* `opts.argv` - Override `process.argv`
* `opts.optionsFirst` - Parse until the first `command` or `<positional>`
   argument, then collect the rest into an array, given the help indicates
   another, repeatable, positional argument, e.g. : `[options] <ommand>
   [<args>...]`
* `opts.smartOptions` - Enable parsing groups that "look like" flags as such,
  for example, parse `[-f ARG...]` as `[-f=ARG...]`

```javascript
#!/usr/bin/env node

// (!) This is runnable code and mimics git. The git help below is the output of
//     `git --help` with a couple of very minor alterations (couple of
//      characters) which are also being addressed.
// (!) Keep in in mind that these strings below could be kept in your README or
//     manpage SYNOPSIS.

const neodoc = require('neodoc');

const args = neodoc.run(`
usage: git [--version] [--help] [-C=<path>] [-c<name=value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [-p|--paginate|--no-pager] [--no-replace-objects] [--bare]
           [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
           <command> [<args>...]
`, { optionsFirst: true });

if (args['<command>'] === 'remote') {
    const remoteArgs = neodoc.run(`
    usage:
        git remote [-v | --verbose]
        git remote add [-t=<branch>] [-m=<master>] [-f] [--tags|--no-tags]
                        [--mirror=<fetch|push>] <name> <url>
        git remote rename <old> <new>
        git remote remove <name>
        git remote set-head <name> (-a | --auto | -d | --delete | <branch>)
        git remote set-branches [--add] <name> <branch>...
        git remote set-url [--push] <name> <newurl> [<oldurl>]
        git remote set-url --add [--push] <name> <newurl>
        git remote set-url --delete [--push] <name> <url>
        git remote [-v | --verbose] show [-n] <name>...
        git remote prune [-n | --dry-run] <name>...
        git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]
    `, { argv: ['remote'].concat(args['<args>']) })

    // ...
} else { /* ... */ }
```


## Project goals ##

> Overview of the short- and long-term goals of this project

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
* **POSIX compatibility**

### Deviations from the original ###

> This implementation tries to be compatible where sensible, but does cut ties
> when it comes down to it. The universal docopt test suite has been adjusted
> accordingly.

* **Better Error reporting.** Let the user of your utility know why his input
  was rejected.
* **Optional arguments.** Neodoc understands `--foo[=BAR]` (or `-f[=<bar>]`) as
  an option that can be provided either with or without an argument.
* **Alias matches.** If `--verbose` yields a value, so will `-v` <sub>(given
  that's the assigned alias)</sub>. Likewise, `FOO` yields value `<foo>` as
  well as `FOO`, and `<foo>` yields `FOO` and `<foo>`.
* **Flags are optional,** always. There's no reason to force the user to
  explicitly pass an option that takes no argument. The absence of the flag
  speaks - the flag will be set to `false`. This is also the case for flags
  inside required groups. E.g.: The group `(-a -b)` will match inputs `-a -b`,
  `-ab`, `-ba`, `-b -a`, `-b`, `-a` and the empty input.
* **All arguments in a group are always required**. This is regardless of
  whether or not the group itself is required or not - once you start matching
  into the group, elements in the group become required for the match to
  succeed. Consider:

  ```sh
  Usage: prog [<name> <type>]
  ```

  will fail `prog foo`, but pass `prog foo bar`. The rationale being that this is
  more general, since if the opposite behaviour (any match) was desired, it
  could be expressed as such:

  ```sh
  Usage: prog [[<name>] [<type>]]
  ```

  **note:** this rule excludes flags/switches and options that have default
  values (or other fallback values).
* **No abbreviations:**
  `--ver` does not match `--verbose`.
  <sub>[(mis-feature in the original implementation)](https://github.com/docopt/docopt/issues/104)</sub>
* **There is no `null`** in the resulting value map. `null` simply means not
  matched - so the key is omitted from the resulting value map. <sub>(this is
  still under consideration)</sub>
* **Smart-options**. Options can be inferred from groups that "look like"
  options: `Usage: foo [-f FILE]` would then expand to `Usage: foo [-f=FILE]`
* **Environment variables**. Options can fall back to environment variables,
  if they are not explicitly defined. The order of evaluation is:
    1. User input (per `process.argv`)
    1. Environment variables (per `[env: ...]` tag)
    1. Option defaults (per `[default: ...]` tag)

## Project status ##

> **Neodoc** is fully usable and well-tested. If you find there is something
> broken or unintuitive about neodoc, please do [open an issue][issue-tracker].
> The following gives on overview of where docopt is headed.

### Beta Target (complete) ####

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

* [x] Implement "options-first"
* [x] Improve scanner speed
* [ ] [POSIX compatibility][POSIX] (opt-in/out)
    * [ ] "Options-first" on by default (guideline 9)
    * [ ] Required option arguments be separate from the option: `-f BAR`
        * [x] Update notation in usage syntax (disallow no spaces)
    * [ ] Optional option arguments be the same argument as the option: `-fBAR`
        * [x] Update notation in usage syntax (disallow spaces)
    * [x] Denote options in singleton groups: `[-f BAR]` means `[-f=BAR]` (no
        option description required)
    * [ ] Allow passing negative numbers as option arguments
    * [ ] Implement guideline 8 (also see 11)
* [x] Improve error messages
  * [x] Improve error messages when matching free groups / options (be more
        specific than "Trailing input")
* [x] Implement optional option arguments: `-a [=foo]`.
* [ ] Implement `--help` and `--version`. The developer will be able to specify
  the option that will trigger the `--help` and `--version` convenience
  functions, with fallbacks to `--help` and `--version`.
  * [ ] Implement `--help`. If matched, print the docopt text.
  * [ ] Implement `--version`. If matched, print the npm package version.
* [ ] Read options from config file
* [x] Allow for `--foo[=<bar>]` syntax (git style).
* [x] Auto-infer types when not specified (e.g. numbers, strings, booleans)
* [x] Implement `[-f BAR]` flag syntax ("smart-options")
* [ ] Allow flag negation syntax `--[no-]foo`: `--foo`, `--no-foo`, `-f`, `+f`
* [x] Fix all purescript (pscid) warnings

### Wishlist ###

> A list of things that are desired, but have not found a place on the roadmap.

* Provide typescript typings
* Read options from config file
* Add ranges support: `-[0-9]` would expand to `-0123456789` (or `-1`, `-2` ...).
  For a usecase, see [here](https://github.com/rupa/v/blob/master/v#L6)
* Read options from prompt (Add a `[prompt]` tag)
* Syntax plugins (vim, ...)
* Put the "source" of a parsed option's value into the output, e.g. "env",
  "default", "user"
* Consider  `+o` syntax: `-o, --option` and its negation: `--no-option` or
  `+o`.
* Provide warnings. This would mean a largish refactor to use either a custom
  monad, or the Writer monad, stacked on top of the Either monad.
* Refactor `Language.Docopt.ParserGen.Parser.genBranchParser` to use manual
  recursive iteration, rather than a fold, like in `Language.Docopt.Solver`.
* Rewrite recursive functions to be in tail position, using
  `purescript-tailrec` and consider trampolining using `purescript-free`.

## Contributing ##

**Contributions are highly encouraged and welcome**.
Bug fixes and clean ups do no need much coordination, but if you're interested
in contributing a feature, contact me at felixschlitter@gmail.com and we can
get the ball rolling &mdash; There's plenty to do. To get up and running, run:

```sh
npm i && bower i && npm test
```

NB: Purescript is declared a devDependency in package.json, so no need to bring
your own. Also, during development, `npm run watch` is extremely useful to get
immediate feedback.

### Implementation overview ###

> A quick overview of the implementation for potential contributors

The project can roughly be broken up into several distinct areas of work:

1. Scanning the docopt text:
    1. Derive at least 1 usage section
    1. Derive 0 or more description sections
1. Parsing the Specification
    1. Lex and parse the usage section
    1. Lex and parse any description sections
1. Solve the parsed Specification into its canonical, unambigious form
1. Generate a parser from the Specification
1. Lex and parse the user input
    1. Lex and parse the user input
    1. Transform into a usable form

## License ##

<strong>&lt;neodoc&gt;</strong> is released under the **MIT LICENSE**.
See file `LICENSE` for a more detailed description of its terms.

---

### Dev Notes ###

#### Reading options from config files ####

> Options should fall back to values read from file.

Options:
* Can this be a developer-provided file or lookup rather than on the command
  line?
* Associate some option or positional argument with a config file lookup. This
  would end in some "special" code, where the parser is run twice (first, prove
  the associated option / positional arg was parsed, ignoring any failures for
  the time being, then run again with the config file default values).

[docopt-orig]: http://docopt.org
[POSIX]: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
[issue-tracker]: https://github.com/felixSchl/neodoc/issues
