module CardanoMultiplatformLib
  ( askLib
  , asksLib
  , importLib
  , module Exports
  , GarbageCollector
  , allocate
  , bech32FromCbor
  , bech32FromCborHex
  , bech32FromString
  , runGarbageCollector
  , transactionWitnessSetFromBytes
  ) where

import Prelude

import CardanoMultiplatformLib.Address (Address, AddressObject, addressObject, address) as Exports
import CardanoMultiplatformLib.Address (AddressObject, addressObject)
import CardanoMultiplatformLib.Address as Address
import CardanoMultiplatformLib.Lib (Lib)
import CardanoMultiplatformLib.Lib (Lib) as Exports
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Transaction as Transaction
import CardanoMultiplatformLib.Types (Bech32, Cbor, CborHex, cborHexToCbor, unsafeBech32)
import CardanoMultiplatformLib.Types (CborHex(..), Bech32, cborToCborHex, cborHexToHex, bech32ToString) as Exports
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (sequence_)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import JS.Object (EffectMth0, JSObject, runEffectMth0)
import Promise.Aff (Promise, toAff)
import Type.Prelude (Proxy(..))

-- TODO: Move to Lib module
foreign import importLibImpl :: Effect (Nullable (Promise Lib))

importLib :: Aff (Maybe Lib)
importLib = liftEffect importLibImpl >>= Nullable.toMaybe >>> case _ of
  Nothing -> pure Nothing
  Just promise ->
    (Just <$> toAff promise) `catchError` const (pure Nothing)

type Ctx = { lib :: Lib, frees :: Ref (List (Effect Unit)) }

-- | StateT is not sufficient because it is not exception
-- | safe. We need to use `Ref` to store the release actions.
-- |
-- | FIXME: We should probably introduce a scope phantom
-- | to avoid leaks.
newtype GarbageCollector a = GarbageCollector
  (ReaderT Ctx Effect a)

derive newtype instance Functor GarbageCollector
derive newtype instance Apply GarbageCollector
derive newtype instance Applicative GarbageCollector
derive newtype instance Bind GarbageCollector
derive newtype instance Monad GarbageCollector
derive newtype instance MonadEffect GarbageCollector

runGarbageCollector :: forall a. Lib -> GarbageCollector a -> Effect a
runGarbageCollector lib (GarbageCollector action) = do
  freesRef <- Ref.new List.Nil
  let
    release = do
      frees <- Ref.read freesRef
      sequence_ frees
    run = do
      a <- runReaderT action { frees: freesRef, lib }
      release
      pure a
  run `catchError` \err -> do
    release
    throwError err

-- | The API allocates objects which provide `free` method.
-- | We use it to release the resources.
type UnmanagedObject r = JSObject (free :: EffectMth0 Unit | r)

allocate :: forall r t. Newtype t (UnmanagedObject r) => Effect t -> GarbageCollector t
allocate alloc = GarbageCollector do
  freesRef <- asks _.frees
  obj <- liftEffect alloc
  let
    jsobj = unwrap obj
    _free = Proxy :: Proxy "free"
  liftEffect $ Ref.modify_ (List.Cons (runEffectMth0 _free jsobj)) freesRef
  pure obj

askLib :: GarbageCollector Lib
askLib = GarbageCollector do
  asks _.lib

asksLib :: forall a. (Lib.Props -> a) -> GarbageCollector a
asksLib f = askLib <#> (f <<< Lib.props)

transactionWitnessSetFromBytes :: Cbor TransactionWitnessSetObject -> GarbageCollector TransactionWitnessSetObject
transactionWitnessSetFromBytes twCbor = do
  { "TransactionWitnessSet": tws } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  allocate $ Transaction.transactionWitnessSet.from_bytes tws twCbor

bech32FromCbor :: Cbor AddressObject -> Opt String -> GarbageCollector Bech32
bech32FromCbor cbor prefix = do
  { "Address": addrClass } <- GarbageCollector $ asks (Lib.props <<< _.lib)
  addrObject <- allocate $ Address.address.from_bytes addrClass cbor
  liftEffect $ addressObject.to_bech32 addrObject prefix

bech32FromCborHex :: CborHex AddressObject -> Opt String -> GarbageCollector Bech32
bech32FromCborHex cborHex prefix = do
  let
    cbor = cborHexToCbor cborHex
  bech32FromCbor cbor prefix

bech32FromString :: Lib -> String -> Effect (Maybe Bech32)
bech32FromString lib addrStr = do
  let
    { "Address": addressClass } = Lib.props lib
  Address.address.is_valid_bech32 addressClass addrStr >>=
    if _ then
      pure $ Just $ unsafeBech32 addrStr
    else
      pure Nothing
