module Contrib.Data.Argonaut.Record where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJsonField, decodeJsonField)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert) as Record
import Row.Joins.Outer (NULL, OuterJoin')
import Type.Eval (class Eval)
import Type.Eval.Tuple (Tuple')
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

type DecodeRecordFn r = Object Json -> Either JsonDecodeError { | r }
