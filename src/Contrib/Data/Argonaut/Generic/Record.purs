module Contrib.Data.Argonaut.Generic.Record where

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
import Prim.RowList (class RowToList)
import Record (get, insert) as Record
import Row.Joins.Outer (NULL, OuterJoin')
import Type.Eval (class Eval)
import Type.Eval.Tuple (Tuple')
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

type DecodeRecordFn r = Object Json -> Either JsonDecodeError { | r }

newtype DecodeStep decoders = DecodeStep { | decoders }

type DecodeJsonFieldFn a = Maybe Json -> Maybe (Either JsonDecodeError a)

instance
  ( IsSymbol l
  , Row.Lacks l r
  , Row.Cons l a r r'
  , DecodeJsonField a
  ) =>
  FoldingWithIndex (DecodeStep d) (Proxy l) (DecodeRecordFn r) (Proxy (Tuple' NULL a)) (DecodeRecordFn r') where
  foldingWithIndex _ l acc _ = \obj -> do
    r <- acc obj
    let
      key = reflectSymbol l
    case decodeJsonField $ Object.lookup key obj of
      Just val -> do
        val' <- lmap (AtKey key) val
        Right $ Record.insert l val' r
      Nothing ->
        Left $ AtKey key MissingValue
else instance
  FoldingWithIndex (DecodeStep d) (Proxy l) (DecodeRecordFn r) (Proxy (Tuple' a NULL)) (DecodeRecordFn r) where
  foldingWithIndex _ _ acc _ = acc
else instance
  ( IsSymbol l
  , Row.Lacks l r
  , Row.Cons l a r r'
  , Row.Cons l (DecodeJsonFieldFn a) _d d
  ) =>
  FoldingWithIndex (DecodeStep d) (Proxy l) (DecodeRecordFn r) (Proxy (Tuple' (DecodeJsonFieldFn a) a)) (DecodeRecordFn r') where
  foldingWithIndex (DecodeStep d) l acc _ = \obj -> do
    r <- acc obj
    let
      key = reflectSymbol l
      decodeJsonFieldFn = Record.get l d
    case decodeJsonFieldFn $ Object.lookup key obj of
      Just val -> do
        val' <- lmap (AtKey key) val
        Right $ Record.insert l val' r
      Nothing ->
        Left $ AtKey key MissingValue

-- | The belowe type classes are aliases which hide the gory type level details (row join, folding etc.)
--
-- An nearly complete example could look like this:
--  ```
--  type Result =
--    { int :: Int
--    , string :: String
--    , decimal :: Decimal
--    }
--
--  main :: Effect Unit
--  main = do
--    let
--      json :: Json
--      json = A.fromObject $ Object.fromHomogeneous
--        { int: A.fromNumber 8.0
--        , string: A.fromString "test"
--        , decimal: A.fromString "0.8"
--        }
--
--      decodeDecimal :: Json -> Either JsonDecodeError Decimal
--      decodeDecimal = decodeFromString (String.trimStart >>> Decimal.fromString)
--
--      -- Field decoders follow internal argonaut strategy and work over `Maybe`
--      decoders = { decimal: map decodeDecimal :: Maybe _ -> Maybe _ }
--
--    traceM $ ((decodeRecord decoders json) :: Either JsonDecodeError Result)
--  ```
--
class DecodeRecord decoders r where
  decodeRecord :: { | decoders } -> Json -> Either JsonDecodeError { | r }

instance
  ( Eval (OuterJoin' decoders r) join
  , RowToList join joinL
  , HFoldlWithIndex (DecodeStep decoders) (DecodeRecordFn ()) (Proxy joinL) (DecodeRecordFn r)
  ) =>
  DecodeRecord decoders r where
  decodeRecord decoders = do
    let
      empty :: DecodeRecordFn ()
      empty _ = Right {}
      decodeObject = hfoldlWithIndex (DecodeStep decoders) empty (Proxy :: Proxy joinL)
    \json -> do
      obj <- decodeJson json
      decodeObject obj

-- This helper works over a `newtype` with `Record` value inside - the above example should be
-- nearly the same but we could have:
--  ```
--  newtype Result = Result
--    { int :: Int
--    , string :: String
--    , decimal :: Decimal
--    }
--
-- ...
--
-- main = do
--  ...
--  traceM $ ((decodeNewtypedRecord decoders json) :: Either JsonDecodeError Result)
-- ```
--
class DecodeNewtypedRecord decoders n where
  decodeNewtypedRecord :: { | decoders } -> Json -> Either JsonDecodeError n

instance
  ( Newtype n { | r }
  , Eval (OuterJoin' decoders r) join
  , RowToList join joinL
  , HFoldlWithIndex (DecodeStep decoders) (DecodeRecordFn ()) (Proxy joinL) (DecodeRecordFn r)
  ) =>
  DecodeNewtypedRecord decoders n where
  decodeNewtypedRecord decoders = map wrap <$> decodeRecord decoders

