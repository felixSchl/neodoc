<div align="center">
    <h1>
        <strong>&lt;neodoc&gt;</strong>
    </h1>
    <em>Beautiful, handcrafted command line interfaces</em>
</div>

<hr>

<p align="center">
  <a href="https://www.npmjs.com/package/neodoc">
    <img src="https://badge.fury.io/js/neodoc.svg"
         alt="NPM package">
  </a>
  <a href="https://travis-ci.org/felixSchl/neodoc">
    <img src="https://travis-ci.org/felixSchl/neodoc.svg?branch=development"
         alt="Build Status">
  </a>
  <a href="https://ci.appveyor.com/project/felixSchl/neodoc">
    <img src="https://ci.appveyor.com/api/projects/status/hjchg7in2l74by1d/branch/development?svg=true"
         alt="Build Status (appveyor)">
  </a>
  <a href="https://gitter.im/felixSchl/neodoc">
    <img src="https://badges.gitter.im/felixSchl/neodoc.svg"
         alt="Join the chat at https://gitter.im/felixSchl/neodoc">
  </a>
<br/>

<p align="center">
    <b><a href="#about">About</a></b>
  | <b><a href="#features">Features</a></b>
  | <b><a href="#installation">Installation</a></b>
  | <b><a href="#usage">Usage</a></b>
  | <b><a href="#language-overview-and-terminology">Language overview</a></b>
  | <b><a href="#license">License</a></b>
  | <b><a href="https://felixschl.github.com/neodoc">Playground <sup>new</sup></a></b>
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/felixSchl/felixSchl.github.io/master/neodoc/neodoc.png"
       alt="preview" />
</p>

<hr>

<br/>

## About

<strong>&lt;neodoc&gt;</strong> is a revised implementation of the [docopt
language][docopt-orig] for node. In brief, it offers a unique way to author
command lines by writing the command line's help text first and then deriving
a matching parser from it, which can then be applied to user input. The
advantages are numerous:

* **No boilerplate** - just write your help-text
* Full control over **beautiful, hand-crafted help texts**
* Documentation comes first - **hence your users come first**
* Documentation is always right - **your help-text is _necessarily_ correct**
* Version-controlled help-text - **the help-text becomes a regular part of your codebase**

This implementation features **error reporting**, both for users and developers,
reading values from **environment variables**, type coercion and much more. For
an (in-)comprehensive comparison to the original, [click
here](#deviations-from-the-original). To take neodoc for a ride, [click
here][playground].

> **_A note to potential adopters and contributors:_** Neodoc is divided into two
distinct parts &mdash; parsing the specification and parsing the argv, given the
specificiation. Theoretically, the origin of the specification does not matter
and the argv parser could be used standalone as it offers a more "correct" parse
than most cli parsers out there, since it parses the input guided by the
specification, rather than parsing the input and then matching it to the
specification. See the "Features" section below. If neodoc finds adoption, I
would not be surprised to see projects mimicking a yargs-like interface that use
the neodoc parser, even though it somewhat defies the original idea of docopt.

## Features ##

* Derive command line interface from help text
* Helpful error messages for both developers and users
* _Options-first_ parsing to compose large programs (see git example)
* Fallback to alternate values: `Argv -> Environment -> Defaults -> Empty`
* Convenient, concise and widely accepted POSIX-style syntax
    * `-f[=ARG], --foo[=ARG]` options
    * `<arg>`, `ARG` positionals
    * `clone`, `pull`, etc. commands
    * `[<arg>]` optional groupings
    * `(<arg>])` required groupings
    * `[-f ARG]` POSIX-style flags
    * `-f[=ARG]...` repeating elements
    * `--` end of options separator
    * `-` stdin marker
    * 99% compatible with a typical `git <command> --help` output
    * <a href="#language-overview-and-terminology"><strong>Full overview of the language &rarr;</strong></a>
* Stop parsing at any option and collect successive input as the argument to
  that option. Similar to `--` but for named options (and their aliases).
* Specification parsing (help-text parsing) is separated from argv parsing
  and can be used for other projects outside of neodoc. _Work is underway to
  make the argv parser usable from JS as well_.
* Count repeated flags
* Parses values into primitive JS types (bool, string, number)
* Correct and smart argument parsing. For example, neodoc has absolutely no
  problem parsing this input: `tar -xvzfsome-dir/some-file`, given a
  specification of: `usage: tar [-xvzf FILE]` while other parses would not know
  that the option stack ends at `-f` and falsly parse this as `-x -v -z -f -s -o
  -m -e=-dir/some-file` at best.


## Installation ##

```sh
npm install --save neodoc
```

## Usage ##

### neodoc.run(help | spec, opts)

Parse and apply the given docopt help text. Alternatively, pass the output of
`neodoc.parse`. If no options are provided, apply it to `process.argv` and
`process.env`. The result is a mapping of key -> value, where the key is the
canonical form of the option and its alias, if available.

Options:

* `opts.dontExit` - Do not exit upon error or when parsing `--help` or
    `--version`. Instead throw and error / return the value.
* `opts.env` - Override `process.env`
* `opts.argv` - Override `process.argv`
* `opts.optionsFirst` - Parse until the first `command` or `<positional>`
   argument, then collect the rest into an array, given the help indicates
   another, repeatable, positional argument, e.g. : `[options] <ommand>
   [<args>...]`
* `opts.smartOptions` - Enable parsing groups that "look like" options as
  options. For example: `[-f ARG...]` means `[-f=ARG...]`
* `opts.stopAt` - Stop parsing at the given options, i.e. `[ -n ]`. It's value
  will be the rest of argv.
* `opts.requireFlags` - Require flags be present in the input. In neodoc, flags
  are optional by default and can be omitted. This option forces the user to
  pass flags explicitly, failing the parse otherwise.
* `opts.laxPlacement` - Relax placement rules. Positionals and commands are no
  longer solid anchors. The order amongs them, however, remains fixed. This
  implies that options can appear anywhere.
* `opts.versionFlags` - An array of flags that trigger the special version
  behavior: Print the program version and exit with code 0.
* `opts.version` - The version to print for the special version behavior.
  Defaults to finding the version of the nearest package.json file, relative
  to the executing main module. Note that disk IO is only performed if
  `opts.versionFlags` is non-empty and `opts.version` is not set.
* `opts.helpFlags` - An array of flags that trigger the special help
  behavior: Print the full program help text and exit with code 0.
* `opts.repeatableOptions` - Allow options to be repeated even if the spec does
  not explicitly allow this. This "loosens" up the parser to accept more input
  and makes for a more intuitive command line. _Please note:_ repeatability
  is still subject to chunking (use `opts.laxPlacement` to relax this further).
* `opts.transforms.presolve` - an array of functions to be called prior to
  "solving" the input. This function takes the spec as it's only parameter.
  At this point, the spec is mostly untouched by neodoc with the exception of
  smart-options which runs as a fixed transform prior to user-provided callbacks
  if `smart-options` is true. Transforms that need to be aware of option stacks
  and `[...-options]` references should run here as this information is lost
  during the solving transforms.
* `opts.transforms.postsolve` - an array of functions to be called after
  "solving" the input, just prior to passing the spec to the arg-parser. This
  function takes the spec as it's only parameter. At this point, the spec has
  been fully solved, expanded and canonicalised.
* `opts.allowUnknown` - Collect unknown options under a special key `?` instead
  of failing. Useful to send an unknown subset of options to another program.

For example:

```javascript
#!/usr/bin/env node

const neodoc = require('neodoc');

const args = neodoc.run(`
usage: git [--version] [--help] [-C <path>] [-c <name=value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [-p|--paginate|--no-pager] [--no-replace-objects] [--bare]
           [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
           <command> [<args>...]
`, { optionsFirst: true, smartOptions: true });

if (args['<command>'] === 'remote') {
    const remoteArgs = neodoc.run(`
    usage:
        git remote [-v | --verbose]
        git remote add [-t <branch>] [-m <master>] [-f] [--tags|--no-tags]
                        [--mirror=<fetch|push>] <name> <url>
        git remote rename <old> <new>
        git remote remove <name>
        git remote [-v | --verbose] show [-n] <name>...
        git remote prune [-n | --dry-run] <name>...
        git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]
    `, { argv: ['remote'].concat(args['<args>']), smartOptions: true })

    // ...
} else { /* ... */ }
```

See the examples folder for a more sophisticated version of the above example.

### neodoc.parse(help, opts)

Parse the docopt text and derive the specification along with some meta information.
The specification is the canonical representation of the CLI as described by it's
help text and can be used for building parsers etc. The output is a plain JS object
and can be serialized. The output can further be passed to `neodoc.run`. This avoids
neodoc having to parse and solve the original help text again, since parsing JSON
is a order of magnitude faster to parse.

## Language overview and terminology

This section gives an overview over the neodoc cli **specification** language.
Keywords are highlighted.

The over-arching format could be described as follows:

```sh
Usage:  <program> [<argument>...] [| <argument> [<argument>...]]
[ [or:] <program> [<argument>...] [| <argument> [<argument>...]]
]*

[options:
    [<argument> [<description and meta tags>]
    ]*
]*
```

Where `<argument>` may be any of the arguments described in the following
subsections.

A full example:

```sh
usage: git fetch [options] [<repository> [<refspec>...]]
   or: git fetch [options] <group>
   or: git fetch --multiple [options] [(<repository> | <group>)...]
   or: git fetch --all [options]

options:
    -v, --verbose         be more verbose
    -q, --quiet           be more quiet
    --all                 fetch from all remotes
    -a, --append          append to .git/FETCH_HEAD instead of overwriting
    --upload-pack <path>  path to upload pack on remote end
    -f, --force           force overwrite of local branch
    -m, --multiple        fetch from multiple remotes
    -t, --tags            fetch all tags and associated objects
    [...]
```

### 1. Arguments

At the heart of the language are command line arguments. There are three
fundamental types of arguments: `options`, `positional` arguments and
`commands`. Options are arguments that start with either a single or a double
dash ('-'), commands are literal matches of a certain string and positionals
constitute everything else.  Read on below for more detail on each argument
type.

#### 1.1. Options

Options are those arguments that start with either one or two dashes. They are
referred to as "short" and "long" options respectively throughout this document
and the source code of neodoc.

Options may take an potentially optional option-argument. Options that do not
are referred to as flags. Options that do specify an option-argument but declare
it as being optional may behave as flags if an argument could not be consumed at
runtime.

The following is true for all options:

* Options may take an optional "option-argument"
* Options may be repeated using `...`
* Adjacent options are not fixed in position: `-a -b` is equivalent to `-b -a`.
  Likewise, `-ab` is equivalent to `-ba`. This also holds true for options that
  take option-arguments.
* Options that are repeated collect values into an array
* Flags that are repeated count the number of occurrences
* Flags that are not repeated simply yield `true` if present
* Flags and options with an optional option-argument can always be omitted from
  the input. They simply won't yield anything in the output value mapping.
* Options may alias a short (one-character) form with a long form, e.g.: `-v,
  --verbose`
* Options that take an argument can specify a `[default: <value>]` in the option
  section as fallback.
* Options that take an argument can specify a `[env: <key>]` in the option
  section as fallback.

##### 1.1.1. Long options

**Long options** are lead by two dashes and may take an potentially optional
option-argument.\
For example:

* `--long <ARG>` the option-argument is loosely bound
* `--long <ARG>` the option-argument is loosely bound and optional. <sub>([#55][issue-55])</sub>
* `--long=<ARG>` the option-argument is explicitly bound
* `--long[=<ARG>]` the option-argument is explicitly bound an optional
* `[--long <ARG>]` the option-argument is explicitly bound via the `smart-options` setting
* `[--long [<ARG>]]` the option-argument is loosely bound via the `smart-options` setting and optional

Note that all of the above forms could be followed by a `...`, indicating that
this option may appear one or more times. The repeated occurrence does not
necessarily need to be adjacent to the previous match.  Repeated occurrences
are collected into an array or into a count if the option qualifies as a flag.

Note that successive dashes are allowed: `--very-long-option`.

##### 1.1.2. Short options

**Short options** are lead by one dash and may take an potentially optional
option-argument. A short option is a one character identifier, but can be
"stacked".

For example:

* `-a <ARG>` the option-argument is loosely bound to `-a`
* `-a=<ARG>` the option-argument is explicitly bound to `-a`
* `-a<ARG>` the option-argument is explicitly bound to `-a`
* `-aARG` the option-argument is **loosely** bound to `-a`
* `-a [<ARG>]` the option-argument is loosely bound to `-a`. <sub>([#55][issue-55])</sub>
* `-a=<ARG>` the option-argument is explicitly bound to `-a`
* `-a[=<ARG>]` the option-argument is explicitly bound to `-a` an optional
* `[-a <ARG>]` the option-argument is explicitly bound to `-a` via the `smart-options` setting
* `[-a [<ARG>]]` the option-argument is loosely bound to `-a` via the `smart-options` setting and optional

Note, however that only the last option in the "option stack" may actually bind
an argument:

* `-abc` is equivalent to `-a -b -c`
* `-abc <ARG>` is equivalent to `-a -b -c <ARG>`
* `-abc=<ARG>` is equivalent to `-a -b -c=<ARG>`
* `-abc[=<ARG>]` is equivalent to `-a -b -c=<ARG>`

...essentially nothing changes when options are stacked. Key is that only the
last option in the stack may bind and consume arguments.

Again, note that all of the above forms could be followed by a `...`, indicating
that this option may appear one or more times. It is important to note that the
repeatability is assigned to **all** options in the stack! Repeated occurrences
are collected into an array or into a count if the option qualifies as a flag
(hence for all but the last options in the stack).

##### 1.1.3. Option-arguments

**Option-arguments** are arguments bound to options. If an option is said to
take an option argument that is not optional, any attempt to match an option
without the argument will result in an immediate parse error. Should an
option-argument be declared optional and not matched during parsing, it may be
treated as a flag and be substituted.

###### 1.1.3.1. Option-argument bindings

* "loose" binding: the option-argument is in adjacent position, but
  needs to be confirmed in the 'options' section. Should
  confirmation not take place, the adjacent argument is treated as a
  positional.
* "explicit" binding: the option-argument is explicitly bound due to
  a lack of whitespace, an equal sign or through 'smart-options'.

##### 1.1.4 The option secion

The option section gives a chance to add more information about options, such
as their default value, their alias or their backing environment variable.
Furthermore, options appearing in the option section may also indicate if the
option is supposed to be repeatable or not. There is more information on this
topic in section "1.7 - References - [options]".

* An alias is assigned via `-v, --verbose`
* A default value is assigned via `[default: value]`
* An environment variable is assigned via `[env: MY_KEY]`

For example:

```
options:
    -f, --foo BAR  This is foo bar. [env: FOO_BAR] [default: 123]
```

The text is pretty flexible and can be arranged as the author pleases. For
example:

```
options:
    -f, --foo BAR...
        This is foo bar.
        [env: FOO_BAR] [default: 123]
```

#### 1.2. Positionals

**Positionals** are arguments that do not lead with any dashes. The position
of their occurrence matters and options are "bounded" by them in that an option
declared before an positional argument may not occur after that positional. <sub>([#24][issue-24])</sub>
Positional arguments are distinguished from _commands_ by being either enclosed
in angled brackets or being all upper case.

For example:
* `<ARG>` is a positional element named `<ARG>`
* `ARG` is a positional element named `ARG`
* `[<ARG>]` is an optional positional element named `<ARG>`
* `[ARG]` is an optional positional element named `ARG`
* `[<ARG>]...` is an optional positional element named `<ARG>` that repeats
* `<ARG>...` is a positional element named `<ARG>` that repeats

Positional arguments either yield a single value if not repeated or an array of
values if repeated. Note that contrary to options, repetition must occur
directly adjacent to the previous match. <sub>([#24][issue-24])</sub>

#### 1.3. Commands

**Commands** are a specialized form of positionals that require to be matched
literally, including casing. All other rules that apply to positionals apply to
commands. They yield a boolean indicating their presence or a count indicating
the number of their occurrences if repeated.

For example:
* `command` must be matched with input "command" on argv
* `command...` must be matched on ore more times with input "command" on argv

#### 1.4. EOA - end-of-arguments

The **EOA (end-of-arguments)** is understood as the separator between known and
unknown arguments. The eoa is typically `--` but any option can become one by
using the 'stop-at' setting.\o

For example:
* `--`
* `-- ARGS`
* `-- ARGS...`
* `[-- ARGS...]`
* `[-- [ARGS...]]`
* ...and so on &mdash; they all have the same meaning.

#### 1.5. Stdin marker

The **stdin** flag is a special, unnamed short-option: `-`. It's presence
indicates that the program should be reading from standard input.

#### 1.6. Groups

Groups are the only recursive argument type. Groups describe one or more
mutually exclusive sets of arguments called "branches". At least one branch
needs to yield a successful parse for the group to succeed.

For example:

* `(foo | bar qux)` means either match command `foo` or command `bar`
  directly followed by command `qux`.
* `[foo | bar qux]` means either match command `foo` or command `bar`
  directly followed by command `qux`, but backtrack on failure and ignore the
  group.
* `(foo | bar qux)...` means either match command `foo` or command `bar`
  directly followed by command `qux`, repeatedly. During repetition another
  branch can be matched, so this is valid: `foo bar qux bar qux foo`. The output
  is: `{ "foo": 2, "bar": 2, "qux": 2 }`.

The following is true for all groups:

* Groups can be repeated: `...`
* Groups can be optional using brackets: `[ foo ]`
* Groups can be required using parenthesis: `( foo )`
* Groups must not be empty
* Groups must contain 1 or more branches
* Groups succeed if at least one branch succeeds
* Multiple successful branch matches are weighted and scored

##### 1.6.1. Matching branches

**Branches** describe multiple mutually exclusive ways to parse a valid program.
Branches can appear at the top-level or in any group. Since branches are
mutually exclusive, only one branch can ever succeed. If multiple branches
succeed, the highest scoring winner is elected. Generally, the depth of the
parse within the branch (that is how deep into the branch the parse succeeded)
as well as the weighting of the matched arguments matters. Arguments that were
substituted by values in environment variables, or by their defaults or empty
values, will have a lower ranking score than those that were read from argv.

#### 1.7. References - [options]

This is not a real argument and not part of the canonical specification. It is
used to indicate that the entire "options" section should be expanded in it's
place. Since this approach lacks information about the relation between options,
options are all expanded as optional and are exchangeable with adjacent options
<sub>([#57][issue-57])</sub>. One exception to this rule is where an option that
is defined in the option section also appears directly adjacent to the
`[options]` reference tag.

For example:

```
usage: prog [options] (-f | -b)
options:
    -f foo
    -b bar
```

This program won't accept the input `-f -b` as `-f` and `-b` are declared
mutually exclusive from one another.

Likewise:

```
usage: prog [options] --foo ARG
options:
    -f, --foo ARG
```

Here, `--foo` won't be expanded again and hence remain required.

---

## Deviations from the original ###

> This implementation tries to be compatible where sensible, but does cut ties
> when it comes down to it. The universal docopt test suite has been adjusted
> accordingly.

* **Better Error reporting.** Let the user of your utility know why input was
  rejected and how to fix it
* **Optional arguments.** Neodoc understands `--foo[=BAR]` (or `-f[=<bar>]`) as
  an option that can be provided either with or without an argument.
* **Alias matches.** If `--verbose` yields a value, so will `-v` <sub>(given
  that's the assigned alias)</sub>.
* **Flags are optional by default**. There's arguably no reason to force the
  user to explicitly pass an option that takes no argument as the absence of the
  flag speaks &mdash; the key will be omitted from the output. This is also the
  case for flags inside required groups. E.g.: The group `(-a -b)` will match
  inputs `-a -b`, `-ab`, `-ba`, `-b -a`, `-b`, `-a` and the empty input. To
  disable this behaviour, enable `options.requireFlags` (see `neodoc.run`).\
  Please note that the default behaviour may change in a future version of
  neodoc &mdash; refer to #61.
* **All arguments in a group are always required**. This is regardless of
  whether or not the group itself is required - once you start matching into the
  group, all elements that are indicated as required have to be matched, either
  by value or via fallbacks.\
  For example:
  ```sh
  Usage: prog [<name> <type>]
  ```
  will fail `prog foo`, but pass `prog foo bar`. The rationale being that this is
  more general, since if the opposite behaviour (any match) was desired, it
  could be expressed as such:
  ```sh
  Usage: prog [[<name>] [<type>]]
  ```
* **No abbreviations:**
  `--ver` does not match `--verbose`.
  <sub>[(mis-feature in the original implementation)](https://github.com/docopt/docopt/issues/104)</sub>
* **There is no `null`** in the resulting value map. `null` simply means not
  matched - so the key is omitted from the resulting value map.
* **Smart-options**. Options can be inferred from groups that "look like"
  options: `Usage: foo [-f FILE]` would then expand to `Usage: foo [-f=FILE]`
* **Environment variables**. Options can fall back to environment variables,
  if they are not explicitly defined. The order of evaluation is:
    1. User input (per `process.argv`)
    1. Environment variables (per `[env: ...]` tag)
    1. Option defaults (per `[default: ...]` tag)
* **Stricter document layout**. Neodoc imposes more restrictions on the format
  of the help text in order to achieve certain goals, such as:
  * Neodoc allows associating option aliases over newlines:

    ```
    options:
     -f,
     --foo this is foo
    ```

  * Neodoc does **not** require 2 spaces between option and argument. Instead,
    only those arguments that visually "look like" arguments are considered for
    binding (i.e. all-caps: `ARG` and in-angles: `<arg>`):

    ```
    options:
     -f,
     --foo ARG
    ```

    Should there be any ambiguity, the option can also be explicitly bound:

    ```
    options:
     -f,
     --foo=ARG
    ```

    The same is true for optional arguments:

    ```
    options:
     -f,
     --foo [ARG]
     -b,
     --bar[=ARG]
    ```

  * Neodoc is more conservative on learning about options in order to prevent
    subtly introducing options:

    ```
    usage: prog [options]

    options:

    --foo this is foo, and it is similar to some-command's
            --bar in that it does qux.
    ```

    Here, the author is talking about `--bar` in another context, so it should
    not be considered an option to the program. Neodoc figures this out based on
    indentation of the previous description start.
  * Neodoc allows interspersing spaces in between usage layouts:

    ```
    usage:
      prog foo bar

      prog qux woo
    ```

    it is important to not that in this format (above), identation of the layout
    **is required**.
    or:

    ```
    usage: prog foo bar

       or: prog qux woo
    ```

   * Neodoc requires an `Options:` section title in order to parse any options
     (see [#76][issue-76] for discussion). **note: I am interested in a proposal
     on how to lift this requirement**.

## License ##

<strong>&lt;neodoc&gt;</strong> is released under the **MIT LICENSE**.
See file `LICENSE` for a more detailed description of its terms.


[docopt-orig]: http://docopt.org
[POSIX]: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
[issue-tracker]: https://github.com/felixSchl/neodoc/issues
[playground]: https://felixschl.github.com/neodoc
[issue-55]: https://github.com/felixSchl/neodoc/issues/55
[issue-24]: https://github.com/felixSchl/neodoc/issues/24
[issue-57]: https://github.com/felixSchl/neodoc/issues/57
[issue-76]: https://github.com/felixSchl/neodoc/issues/76
