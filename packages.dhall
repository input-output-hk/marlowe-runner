let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221201/packages.dhall
        sha256:d1a68fa15709eaa686515eb5b9950d82c743f7bf73e3d87a4abe9e1be6fda571

in  upstream

  with json-helpers =
      mkPackage
        [ "aff"
        , "argonaut-codecs"
        , "argonaut-core"
        , "arrays"
        , "bifunctors"
        , "contravariant"
        , "control"
        , "effect"
        , "either"
        , "enums"
        , "foldable-traversable"
        , "foreign-object"
        , "maybe"
        , "newtype"
        , "ordered-collections"
        , "prelude"
        , "profunctor"
        , "psci-support"
        , "quickcheck"
        , "record"
        , "spec"
        , "spec-quickcheck"
        , "transformers"
        , "tuples"
        , "typelevel-prelude"
        ]
        "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
        "60615c36abaee16d8dbe09cdd0e772e6d523d024"
  with marlowe =
      mkPackage
        [ "argonaut"
        , "argonaut-codecs"
        , "argonaut-core"
        , "arrays"
        , "bifunctors"
        , "bigints"
        , "contravariant"
        , "control"
        , "datetime"
        , "effect"
        , "either"
        , "enums"
        , "foldable-traversable"
        , "foreign-object"
        , "functions"
        , "integers"
        , "json-helpers"
        , "lists"
        , "maybe"
        , "newtype"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "profunctor-lenses"
        , "strings"
        , "transformers"
        , "tuples"
        , "typelevel-prelude"
        , "unfoldable"
        ]
        "https://github.com/input-output-hk/purescript-marlowe.git"
        "fb588282974a1aac67299b6ce37c818b156e28ab"
  with typelevel-eval =
      mkPackage
        [ "bifunctors"
        , "console"
        , "contravariant"
        , "effect"
        , "leibniz"
        , "psci-support"
        , "profunctor"
        , "record"
        , "typelevel-prelude"
        , "tuples"
        ]
        "https://github.com/natefaubion/purescript-typelevel-eval"
        "v0.5.0"
  with row-joins =
      mkPackage
        [ "console", "effect", "prelude", "typelevel-eval" ]
        "https://github.com/paluh/purescript-row-joins.git"
        "c129c7531a0ccf2b82ac1e57ab796b3df17614ff"
  with refined =
      mkPackage
        [ "argonaut", "effect", "prelude", "quickcheck", "typelevel" ]
        "https://github.com/danieljharvey/purescript-refined.git"
        "v1.0.0"
  with js-unsafe-stringify =
      mkPackage
        ([] : List Text)
        "https://github.com/paluh/purescript-js-unsafe-stringify.git"
        "v0.2.1"
  with js-object =
      mkPackage
        [ "aff"
        , "effect"
        , "heterogeneous"
        , "prelude"
        , "typelevel-prelude"
        , "contravariant"
        , "newtype"
        , "record"
        , "unsafe-coerce"
        ]
        "https://github.com/purescript-codegen/purescript-js-object.git"
        "73db55f89744b032f44c9ec49804f46e3ee63ed7"
        
  with atleast =
      mkPackage
        [ "arrays"
        , "effect"
        , "enums"
        , "fast-vect"
        , "foldable-traversable"
        , "integers"
        , "maybe"
        , "partial"
        , "prelude"
        , "quickcheck"
        , "unsafe-coerce"
        ]
        "https://github.com/paluh/purescript-atleast.git"
        "efcb32a1708578ad6ca0f6d05ff3d0ce290eb57f"
  with polyform =
      mkPackage
        [ "js-unsafe-stringify"
        , "newtype"
        , "ordered-collections"
        , "variant"
        , "profunctor"
        , "invariant"
        , "foreign-object"
        , "run"
        , "transformers"
        , "validation"
        , "foreign"
        ]
        "https://github.com/purescript-polyform/polyform.git"
        "v0.9.2"

  with polyform-batteries-core =
      mkPackage
        [ "debug"
        , "decimals"
        , "filterable"
        , "numbers"
        , "polyform"
        , "prelude"
        , "record-extra"
        , "test-unit"
        ]
        "https://github.com/purescript-polyform/batteries-core.git"
        "v0.3.0"

  with polyform-batteries-urlencoded =
      mkPackage
        [ "argonaut"
        , "console"
        , "debug"
        , "effect"
        , "form-urlencoded"
        , "polyform-batteries-core"
        , "psci-support"
        , "spec"
        ]
        "https://github.com/purescript-polyform/batteries-urlencoded.git"
        "v0.4.1"

  with
    -- react-bootstrap = ../purescript-react-bootstrap/spago.dhall as Location
    react-bootstrap = mkPackage
      [ "aff", "arrays", "atleast", "bifunctors", "console", "control"
      , "convertable-options", "datetime", "datetime-iso", "debug", "decimals"
      , "effect", "either", "enums", "foldable-traversable", "formatters"
      , "functors", "halogen-subscriptions", "identity", "integers"
      , "js-timers", "maybe", "newtype", "nullable", "ordered-collections"
      , "parsing", "polyform", "polyform-batteries-core"
      , "polyform-batteries-urlencoded", "prelude", "profunctor", "random"
      , "react-basic", "react-basic-dom", "react-basic-hooks", "record", "refs"
      , "safe-coerce", "strings", "tailrec", "transformers", "tuples"
      , "typelevel-prelude", "undefined-is-not-a-problem", "unsafe-coerce"
      , "validation", "web-html"
      ]
      "https://github.com/paluh/purescript-react-bootstrap.git"
      "39c055d3a3d4fadca6afba31326fdf27d315aec8"

  with react-basic-hooks =
      mkPackage
        [ "aff"
        , "aff-promise"
        , "bifunctors"
        , "console"
        , "control"
        , "datetime"
        , "effect"
        , "either"
        , "exceptions"
        , "foldable-traversable"
        , "functions"
        , "indexed-monad"
        , "integers"
        , "maybe"
        , "newtype"
        , "now"
        , "nullable"
        , "ordered-collections"
        , "prelude"
        , "react-basic"
        , "refs"
        , "tuples"
        , "type-equality"
        , "unsafe-coerce"
        , "unsafe-reference"
        , "web-html"
        ]
        "https://github.com/purescript-react/purescript-react-basic-hooks.git"
        "v8.2.0"

  with
    react-bootstrap = mkPackage
      [ "aff", "arrays", "atleast", "bifunctors", "console", "control"
      , "convertable-options", "datetime", "datetime-iso", "debug", "decimals"
      , "effect", "either", "enums", "foldable-traversable", "formatters"
      , "functors", "halogen-subscriptions", "identity", "integers"
      , "js-timers", "maybe", "newtype", "nullable", "ordered-collections"
      , "parsing", "polyform", "polyform-batteries-core"
      , "polyform-batteries-urlencoded", "prelude", "profunctor", "random"
      , "react-basic", "react-basic-dom", "react-basic-hooks", "record", "refs"
      , "safe-coerce", "strings", "tailrec", "transformers", "tuples"
      , "typelevel-prelude", "undefined-is-not-a-problem", "unsafe-coerce"
      , "validation", "web-html"
      ]
      "https://github.com/paluh/purescript-react-bootstrap.git"
      "39c055d3a3d4fadca6afba31326fdf27d315aec8"
      
  with datetime-iso =
      mkPackage
        [ "aff"
        , "argonaut"
        , "argonaut-codecs"
        , "argonaut-core"
        , "arrays"
        , "bifunctors"
        , "datetime"
        , "effect"
        , "either"
        , "enums"
        , "foldable-traversable"
        , "maybe"
        , "newtype"
        , "parsing"
        , "partial"
        , "prelude"
        , "spec"
        , "strings"
        , "transformers"
        ]
        "https://github.com/paluh/purescript-datetime-iso"
        "c7b75bb16e0543687fc4c9b21c34a49cdaa22f0c"

  with foreign-generic =
      mkPackage
      [ "effect", "foreign", "foreign-object", "ordered-collections"
      , "exceptions", "record", "identity"
      ]
      "https://github.com/paluh/purescript-foreign-generic.git"
      "a5c23d29e72619624978446293ac9bb45ccd2fde"

  with cardano-multiplatform-lib =
      mkPackage
        [ "argonaut"
        , "arraybuffer-types"
        , "arrays"
        , "debug"
        , "effect"
        , "either"
        , "exceptions"
        , "foldable-traversable"
        , "js-object"
        , "js-promise-aff"
        , "lists"
        , "marlowe"
        , "maybe"
        , "newtype"
        , "nullable"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "refs"
        , "strings"
        , "transformers"
        , "tuples"
        , "typelevel-prelude"
        , "undefined-is-not-a-problem"
        , "web-encoding"
        ]
        "https://github.com/input-output-hk/purescript-cardano-multiplatform-lib.git"
        "3929298b6a650f15d9d270b91b4415b9b5457601"

  with cardano-wallet-client =
      mkPackage
        [ "argonaut"
        , "arraybuffer-types"
        , "arrays"
        , "debug"
        , "effect"
        , "either"
        , "exceptions"
        , "foreign-generic"
        , "foldable-traversable"
        , "js-object"
        , "js-promise-aff"
        , "lists"
        , "marlowe"
        , "maybe"
        , "newtype"
        , "nullable"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "refs"
        , "strings"
        , "transformers"
        , "tuples"
        , "typelevel-prelude"
        , "undefined-is-not-a-problem"
        , "web-encoding"
        ]
        "https://github.com/input-output-hk/purescript-cardano-wallet-client.git"
        "v0.0.1"

  -- with marlowe-runtime-client = ../purescript-marlowe-runtime-client/spago.dhall as Location
  with marlowe-runtime-client =
     mkPackage
       [ "aff"
       , "aff-promise"
       , "argonaut"
       , "argonaut-codecs"
       , "argonaut-core"
       , "arraybuffer"
       , "arraybuffer-types"
       , "arrays"
       , "atleast"
       , "avar"
       , "bifunctors"
       , "bigints"
       , "cardano-multiplatform-lib"
       , "checked-exceptions"
       , "console"
       , "control"
       , "convertable-options"
       , "datetime"
       , "datetime-iso"
       , "debug"
       , "decimals"
       , "effect"
       , "either"
       , "enums"
       , "errors"
       , "exceptions"
       , "fetch"
       , "fetch-core"
       , "filterable"
       , "foldable-traversable"
       , "foreign"
       , "foreign-generic"
       , "foreign-object"
       , "formatters"
       , "free"
       , "freeap"
       , "functions"
       , "functors"
       , "halogen-subscriptions"
       , "heterogeneous"
       , "http-methods"
       , "identity"
       , "indexed-monad"
       , "integers"
       , "js-date"
       , "js-object"
       , "js-promise"
       , "js-promise-aff"
       , "js-timers"
       , "js-unsafe-stringify"
       , "lists"
       , "marlowe"
       , "maybe"
       , "monad-loops"
       , "newtype"
       , "node-process"
       , "nonempty"
       , "now"
       , "nullable"
       , "ordered-collections"
       , "parallel"
       , "parsing"
       , "partial"
       , "posix-types"
       , "prelude"
       , "profunctor"
       , "profunctor-lenses"
       , "random"
       , "record"
       , "refs"
       , "row-joins"
       , "safe-coerce"
       , "strings"
       , "tailrec"
       , "transformers"
       , "tuples"
       , "typelevel-eval"
       , "typelevel-prelude"
       , "uint"
       , "undefined-is-not-a-problem"
       , "unfoldable"
       , "unsafe-coerce"
       , "uri"
       , "validation"
       , "variant"
       , "web-dom"
       , "web-encoding"
       , "web-file"
       , "web-html"
       ]
       "https://github.com/input-output-hk/purescript-marlowe-runtime-client.git"
       "v0.0.7"
  with
    errors =
      mkPackage
        [ "control", "effect", "either", "maybe", "prelude"
        , "transformers"
        ]
        "https://github.com/CarstenKoenig/purescript-errors.git"
        "edfd8b1a285920e725a7fec32e736c5d59561772"

