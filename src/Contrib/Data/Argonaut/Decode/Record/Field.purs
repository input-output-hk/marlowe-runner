module Contrib.Data.Argonaut.Decode.Record.Field where

import Prelude

import Contrib.Record.BuilderT (BuilderT(..), execBuilderT)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut as A
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Record as R
import Type.Proxy (Proxy)

type DecodeM = ReaderT (Object Json) (Either JsonDecodeError)

type RecordBuilderM r r' = BuilderT DecodeM { | r } { | r' }

askFieldOptional :: forall r. String -> RecordBuilderM r r (Maybe Json)
askFieldOptional fieldName = BuilderT do
  obj <- ask
  pure $ (identity /\ Object.lookup fieldName obj)

askObject :: forall r. RecordBuilderM r r (Object Json)
askObject = BuilderT do
  obj <- ask
  pure (identity /\ obj)

askField :: forall r. String -> RecordBuilderM r r Json
askField fieldName = BuilderT do
  obj <- ask
  lift $ Decoders.getField (\json -> pure (identity /\ json)) obj fieldName

liftEither :: forall a r. Either JsonDecodeError a -> RecordBuilderM r r a
liftEither v = BuilderT do
  v' <- lift v
  pure (identity /\ v')

insertProp
  :: forall a l r r'
   . IsSymbol l
  => Row.Lacks l r
  => Row.Cons l a r r'
  => Proxy l
  -> a
  -> BuilderT DecodeM { | r } { | r' } Unit
insertProp l a = BuilderT $ do
  pure (R.insert l a /\ unit)

decodeJsonProp
  :: forall a l r r'
   . IsSymbol l
  => Row.Lacks l r
  => Row.Cons l a r r'
  => DecodeJson a
  => Proxy l
  -> BuilderT DecodeM { | r } { | r' } Unit
decodeJsonProp l = Ix.do
  obj <- askObject
  v <- liftEither $ A.getField obj (reflectSymbol l)
  insertProp l v

execRecordBuilderM :: forall a r. Json -> RecordBuilderM () r a -> Either JsonDecodeError { | r }
execRecordBuilderM json builder = do
  obj <- decodeJson json
  runReaderT (execBuilderT builder {}) obj

decodeField
  :: forall a l r r'
   . IsSymbol l
  => Row.Lacks l r
  => Row.Cons l a r r'
  => Proxy l
  -> (Json -> Either JsonDecodeError a)
  -> RecordBuilderM r r' Unit
decodeField l decode = BuilderT do
  obj <- ask
  json <- lift $ A.getField obj (reflectSymbol l)
  v <- lift $ decode json
  pure $ (R.insert l v /\ unit)

infixl 7 decodeField as :=

decodeFieldDefault
  :: forall a l r r'
   . IsSymbol l
  => Row.Lacks l r
  => Row.Cons l a r r'
  => Proxy l
  -> (Json -> Either JsonDecodeError a)
  -> a
  -> RecordBuilderM r r' Unit
decodeFieldDefault l decode default = BuilderT do
  obj <- ask
  let
    l' = reflectSymbol l
  json <- lift $ A.getFieldOptional obj l'
  v <- lift $ lmap (AtKey l') $ maybe (pure default) decode json
  pure $ (R.insert l v /\ unit)

infixl 1 decodeFieldDefault as :=!

-- null | missing -> Nothing
decodeFieldOptional
  :: forall a l r r'
   . IsSymbol l
  => Row.Lacks l r
  => Row.Cons l (Maybe a) r r'
  => Proxy l
  -> (Json -> Either JsonDecodeError a)
  -> RecordBuilderM r r' Unit
decodeFieldOptional l decode = BuilderT do
  obj <- ask
  let
    l' = reflectSymbol l
  json <- lift $ A.getFieldOptional' obj l'
  v <- lift $ lmap (AtKey l') $ for json decode
  pure $ (R.insert l v /\ unit)

infixl 7 decodeFieldOptional as :=?

decodeFieldOptionalDefault
  :: forall a l r r'
   . IsSymbol l
  => Row.Lacks l r
  => Row.Cons l (Maybe a) r r'
  => Proxy l
  -> (Json -> Either JsonDecodeError a)
  -> a
  -> RecordBuilderM r r' Unit
decodeFieldOptionalDefault l decode default = BuilderT do
  obj <- ask
  let
    l' = reflectSymbol l
  json <- lift $ A.getFieldOptional obj l'
  v <- lift $ lmap (AtKey l') $ maybe (pure default) decode json
  pure $ (R.insert l (Just v) /\ unit)

infixl 7 decodeFieldOptionalDefault as :=?!
