module Component.MarloweYaml where

import Prelude

import Contrib.Language.Marlowe.Core.V1 (compareMarloweJsonKeys)
import Contrib.ReactSyntaxHighlighter (yamlSyntaxHighlighter)
import Data.Function.Uncurried (mkFn2)
import JsYaml as JsYaml
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic (JSX)
import React.Basic.DOM.Simplified.Generated as DOM

sortMarloweKeys :: String -> String -> JsYaml.JsOrdering
sortMarloweKeys a b = JsYaml.toJsOrdering $ compareMarloweJsonKeys a b

marloweYaml :: V1.Contract -> JSX
marloweYaml contract =
  DOM.div { className: "child-pre-sixty-vh child-pre-m-0 child-pre-px-2 child-pre-y-0 child-pre-bg-transparent" }
    [ yamlSyntaxHighlighter contract { sortKeys: mkFn2 sortMarloweKeys } ]


