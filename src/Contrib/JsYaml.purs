module JsYaml where

import Prelude

import Contrib.JsonBigInt (BigIntJson, unsafeToJson)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Prim.Row as Row
import Record as Record
import Type.Prelude (Proxy(..))
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

foreign import data Schema :: Type

type Options =
  { indent :: Opt Int
  , replacer :: Opt (Fn2 String Json Json)
  , sortKeys :: Opt (Fn2 String String JsOrdering)
  , schema :: Schema
  }

foreign import bigIntSchema :: Schema

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

dumpBigIntJson
  :: forall opts
   . Row.Lacks "schema" opts
  => Row.Cons "schema" Schema opts (schema :: Schema | opts)
  => NoProblem.Coerce { schema :: Schema | opts } Options
  => { | opts }
  -> BigIntJson
  -> String
dumpBigIntJson opts json = do
  let
    opts' :: { schema :: Schema | opts }
    opts' = Record.insert (Proxy @"schema") bigIntSchema opts

    opts'' = NoProblem.coerce opts'
  runFn2 dumpImpl (unsafeToJson json) opts''
