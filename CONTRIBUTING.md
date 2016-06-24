# Contributing #

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

> A quick overview of the implementation for potential contributors

The project can roughly be broken up into several distinct areas of work:

1. Scanning the docopt text:
    1. Derive at least 1 usage section
    1. Derive 0 or more description sections
1. Parsing the Specification
    1. Lex and parse the usage section
    1. Lex and parse any description sections
1. Solve the parsed Specification into its canonical, unambigious form
1. Derive a parser from the Specification and apply it to user input
    1. Lex and parse the user input
    1. Apply fallback values
    1. Transform into a usable form

