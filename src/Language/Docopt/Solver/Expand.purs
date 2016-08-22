{-
# Expand singleton groups of exclusively optional arguments into many groups.

> NOTE: this changes the semantics of the spec based on options.
>       This must happen prior to parsing, but not after solving, i.e.
>       it must not be cached this way in order to retain the ability for
>       different behaviours based on e.g. 'options.requireFlags'

Consider:

* [-abc]      => [-a] [-b] [-c]                   (if not 'options.requireFlags)
* [-abc foo]  => [-abc foo]           (note: no expansion because of positional)
* [-abc|-def] => [-abc|-def]          (note: no expansion because not singleton)
* [[a] [b]]   => [a] [b]
* [-abc]...   => [-a]... [-b]... [-c]...          (if not 'options.requireFlags)

The idea is that this should make parsing a bit more intuitive for the end-user
as a partial match won't necessarily consume the entire group.
-}

module Language.Docopt.Solver.Expand where
