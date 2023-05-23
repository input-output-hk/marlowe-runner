module Contrib.Data.Argonaut.Decode.Record where

import Prelude

import Contrib.Record.BuilderT (BuilderT(..))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError)
import Data.Argonaut as A
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Prim.Row as Row
import Record as R
import Type.Proxy (Proxy)

getField
  :: forall a l r r'
   . DecodeJson a
  => IsSymbol l
  => Row.Lacks l r
  => Row.Cons l a r r'
  => Object Json
  -> Proxy l
  -> BuilderT (Either JsonDecodeError) { | r } { | r' } Unit
getField obj l = BuilderT do
  v <- A.getField obj (reflectSymbol l)
  pure $ (R.insert l v /\ unit)

infixl 7 getField as .:

getFieldOptional
  :: forall a l r r'
   . DecodeJson a
  => IsSymbol l
  => Row.Lacks l r
  => Row.Cons l (Maybe a) r r'
  => Object Json
  -> Proxy l
  -> BuilderT (Either JsonDecodeError) { | r } { | r' } Unit
getFieldOptional obj l = BuilderT do
  v <- A.getFieldOptional obj (reflectSymbol l)
  pure $ (R.insert l v /\ unit)

infix 7 getFieldOptional as .:!

getFieldOptional'
  :: forall a l r r'
   . DecodeJson a
  => IsSymbol l
  => Row.Lacks l r
  => Row.Cons l (Maybe a) r r'
  => Object Json
  -> Proxy l
  -> BuilderT (Either JsonDecodeError) { | r } { | r' } Unit
getFieldOptional' obj l = BuilderT do
  v <- A.getFieldOptional' obj (reflectSymbol l)
  pure $ (R.insert l v /\ unit)

infix 7 getFieldOptional' as .:?
