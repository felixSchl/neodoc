{ name =
    "docopt"
, dependencies =
    [ "aff"
    , "arrays"
    , "assert"
    , "bifunctors"
    , "console"
    , "control"
    , "datetime"
    , "debug"
    , "effect"
    , "either"
    , "exceptions"
    , "foreign"
    , "free"
    , "generics-rep"
    , "globals"
    , "integers"
    , "lists"
    , "maybe"
    , "nonempty"
    , "ordered-collections"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "spec"
    , "strings"
    , "template-strings"
    , "transformers"
    , "yarn"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
