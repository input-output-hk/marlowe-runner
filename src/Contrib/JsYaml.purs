module JsYaml where

import Prelude

import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Unsafe.Coerce (unsafeCoerce)

foreign import data JsOrdering :: Type

lessThan :: JsOrdering
lessThan = unsafeCoerce (-1)

equal :: JsOrdering
equal = unsafeCoerce 0

greaterThan :: JsOrdering
greaterThan = unsafeCoerce 1

toJsOrdering :: Ordering -> JsOrdering
toJsOrdering = case _ of
  LT -> lessThan
  EQ -> equal
  GT -> greaterThan

type Options =
  { indent :: Opt Int
  , sortKeys :: Opt (Fn2 String String JsOrdering)
  }

foreign import dumpImpl :: Fn2 Json Options String

dump
  :: forall opts
   . NoProblem.Coerce { | opts } Options
  => { | opts }
  -> Json
  -> String
dump opts json = do
  let
    opts' = NoProblem.coerce opts
  runFn2 dumpImpl json opts'
