let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20191005/packages.dhall sha256:ba287d858ada09c4164792ad4e643013b742c208cbedf5de2e35ee27b64b6817

let overrides = {=}

let additions =
  { yarn =
      { dependencies =
          [ "strings"
          , "arrays"
          , "generics-rep"
          , "partial"
          , "unicode"
          ]
      , repo =
          "https://github.com/thimoteus/purescript-yarn"
      , version =
          "v4.0.0"
      }
  , template-strings =
      { dependencies =
          [ "functions"
          , "tuples"
          ]
      , repo =
          "https://github.com/purescripters/purescript-template-strings"
      , version =
          "v5.1.0"
      }
  }

in  upstream // overrides // additions
