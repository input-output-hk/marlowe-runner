module Contrib.ReactSyntaxHighlighter where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, stringifyWithIndent)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import JsYaml as JsYaml
import React.Basic (JSX, ReactComponent, element)

type SyntaxHighlighterPropsRow =
  ( language :: String
  , children :: String
  , wrapLongLines :: Opt Boolean
  )

foreign import _SyntaxHighlighter :: ReactComponent { | SyntaxHighlighterPropsRow }

syntaxHighlighter
  :: forall props
   . NoProblem.Coerce { | props } { | SyntaxHighlighterPropsRow }
  => { | props }
  -> JSX
syntaxHighlighter props = element _SyntaxHighlighter (NoProblem.coerce props)

jsonSyntaxHighlighter :: forall a. EncodeJson a => a -> JSX
jsonSyntaxHighlighter a = syntaxHighlighter { language: "json", children: stringifyWithIndent 2 $ encodeJson a, wrapLongLines: true }

yamlSyntaxHighlighter :: forall a. EncodeJson a => a -> JSX
yamlSyntaxHighlighter a = syntaxHighlighter { language: "yaml", children: JsYaml.dump (encodeJson a) { indent: 2 }, wrapLongLines: true}
