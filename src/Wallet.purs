module Wallet
  ( Api
  , ApiError
  , ApiForeignErrors
  , Bytes(..)
  , Cbor(..)
  , Coin
  , DataSignError
  , HashObject32(..)
  , SomeAddress(..)
  , SignTxError
  , Transaction(..)
  , TxSignError
  , TransactionUnspentOutput
  , UnknownError
  , Wallet
  , apiVersion
  , cardano
  , enable
  , enable_
  , eternl
  , fromSomeAddress
  , gerowallet
  , getBalance
  , getChangeAddress
  , getCollateral
  , getNetworkId
  , getRewardAddresses
  , getUnusedAddresses
  , getUsedAddresses
  , getUtxos
  , icon
  , isEnabled
  , isEnabled_
  , lace
  , name
  , nami
  , signData
  , signTx
  , submitTx
  , yoroi
  ) where

import Prelude

import CardanoMultiplatformLib (AddressObject, Bech32, CborHex, bech32FromCborHex, bech32FromString, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionHashObject, TransactionObject, TransactionUnspentOutputObject, TransactionWitnessSetObject, ValueObject)
import CardanoMultiplatformLib.Types (unsafeCborHex)
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..), either, hush, note)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.NonEmpty (singleton)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Undefined.NoProblem (undefined)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign, ForeignError(..))
import Foreign as Foreign
import Foreign.Generic.Internal as Foreign.Generic
import Foreign.Index as Foreign.Index
import Foreign.Object (Object, lookup)
import HexString as HexString
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectProp, JSObject)
import JS.Object.Generic (mkFFI)
import Prim.TypeError (class Warn, Text)
import Promise (Rejection, resolve, thenOrCatch) as Promise
import Promise.Aff (Promise)
import Promise.Aff (Promise, toAffE) as Promise
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (Window)

data TransactionUnspentOutput

data Coin

data Transaction

data HashObject32

type ApiError r =
  ( invalidRequest :: String
  , internalError :: String
  , refused :: String
  , accountChange :: String
  | r
  )

type DataSignError r =
  ( proofGeneration :: String
  , addressNotPK :: String
  , userDeclined :: String
  | r
  )

type TxSendError r =
  ( refused :: String
  , failure :: String
  | r
  )

type TxSignError r =
  ( proofGeneration :: String
  , userDeclined :: String
  | r
  )

type ApiForeignErrors r =
  ( foreignErrors :: NonEmptyList ForeignError
  | r
  )

type UnknownError r =
  ( unknownError :: Foreign
  | r
  )

_invalidRequest :: Proxy "invalidRequest"
_invalidRequest = Proxy

_internalError :: Proxy "internalError"
_internalError = Proxy

_refused :: Proxy "refused"
_refused = Proxy

_failure :: Proxy "failure"
_failure = Proxy

_accountChange :: Proxy "accountChange"
_accountChange = Proxy

_proofGeneration :: Proxy "proofGeneration"
_proofGeneration = Proxy

_addressNotPK :: Proxy "addressNotPK"
_addressNotPK = Proxy

_userDeclined :: Proxy "userDeclined"
_userDeclined = Proxy

toApiError :: forall e r. { info :: String, code :: Int | e } -> Maybe (Variant (| ApiError + r))
toApiError = case _ of
  { info, code: -1 } -> Just $ Variant.inj _invalidRequest info
  { info, code: -2 } -> Just $ Variant.inj _internalError info
  { info, code: -3 } -> Just $ Variant.inj _refused info
  { info, code: -4 } -> Just $ Variant.inj _accountChange info
  _ -> Nothing

toDataSignError :: forall e r. { info :: String, code :: Int | e } -> Maybe (Variant (| DataSignError + r))
toDataSignError = case _ of
  { info, code: 1 } -> Just $ Variant.inj _proofGeneration info
  { info, code: 2 } -> Just $ Variant.inj _addressNotPK info
  { info, code: 3 } -> Just $ Variant.inj _userDeclined info
  _ -> Nothing

toTxSendError :: forall e r. { info :: String, code :: Int | e } -> Maybe (Variant (| TxSendError + r))
toTxSendError = case _ of
  { info, code: 1 } -> Just $ Variant.inj _refused info
  { info, code: 2 } -> Just $ Variant.inj _failure info
  _ -> Nothing

toTxSignError :: forall e r. { info :: String, code :: Int | e } -> Maybe (Variant (| TxSignError + r))
toTxSignError = case _ of
  { info, code: 1 } -> Just $ Variant.inj _proofGeneration info
  { info, code: 2 } -> Just $ Variant.inj _userDeclined info
  _ -> Nothing

unknownError :: forall r. Foreign -> Variant (| UnknownError + r)
unknownError = Variant.inj (Proxy :: Proxy "unknownError")

foreignErrors :: forall r. NonEmptyList ForeignError -> Variant (| ApiForeignErrors + r)
foreignErrors = Variant.inj (Proxy :: Proxy "foreignErrors")

lookupForeign :: forall a. String -> Object a -> Either (NonEmptyList ForeignError) a
lookupForeign str obj = note (NonEmptyList (singleton $ ForeignError $ "Missing " <> str)) $ lookup str obj

readWalletError :: Foreign -> Either (NonEmptyList ForeignError) { info :: String, code :: Int }
readWalletError rejection = runExcept do
  obj <- Foreign.Generic.readObject rejection
  info' <- except $ lookupForeign "info" obj
  code' <- except $ lookupForeign "code" obj

  info <- Foreign.readString info'
  code <- Foreign.readInt code'
  pure { info, code }

newtype Cbor :: forall k. k -> Type
newtype Cbor a = Cbor String

instance Show (Cbor a) where
  show (Cbor s) = "(Cbor " <> show s <> ")"

newtype Bytes = Bytes String

type Api = JSObject
  ( getNetworkId :: EffectMth0 (Promise Int)
  , getUtxos :: EffectMth0 (Promise (Nullable (Array (CborHex TransactionUnspentOutputObject))))
  , getCollateral :: EffectMth1 (Cbor Coin) (Promise (Nullable (Array (Cbor TransactionUnspentOutput))))
  , getBalance :: EffectMth0 (Promise (CborHex ValueObject))
  , getUsedAddresses :: EffectMth0 (Promise (Array SomeAddress))
  , getUnusedAddresses :: EffectMth0 (Promise (Array (CborHex AddressObject)))
  , getChangeAddress :: EffectMth0 (Promise SomeAddress)
  , getRewardAddresses :: EffectMth0 (Promise (Array (CborHex AddressObject)))
  , signTx :: EffectMth2 (CborHex TransactionObject) Boolean (Promise (CborHex TransactionWitnessSetObject))
  , signData :: EffectMth2 (CborHex AddressObject) Bytes (Promise Bytes)
  , submitTx :: EffectMth1 (CborHex TransactionObject) (Promise (CborHex TransactionHashObject))
  )

_Api
  :: { getBalance :: Api -> Effect (Promise (CborHex ValueObject))
     , getChangeAddress :: Api -> Effect (Promise SomeAddress)
     , getCollateral :: Api -> Cbor Coin -> Effect (Promise (Nullable (Array (Cbor TransactionUnspentOutput))))
     , getNetworkId :: Api -> Effect (Promise Int)
     , getRewardAddresses :: Api -> Effect (Promise (Array (CborHex AddressObject)))
     , getUnusedAddresses :: Api -> Effect (Promise (Array (CborHex AddressObject)))
     , getUsedAddresses :: Api -> Effect (Promise (Array SomeAddress))
     , getUtxos :: Api -> Effect (Promise (Nullable (Array (CborHex TransactionUnspentOutputObject))))
     , signData :: Api -> CborHex AddressObject -> Bytes -> Effect (Promise Bytes)
     , signTx :: Api -> CborHex TransactionObject -> Boolean -> Effect (Promise (CborHex TransactionWitnessSetObject))
     , submitTx :: Api -> CborHex TransactionObject -> Effect (Promise (CborHex TransactionHashObject))
     }
_Api = mkFFI (Proxy :: Proxy Api)

-- FIXME: newtype this
type Wallet = JSObject
  ( enable :: EffectMth0 (Promise Api)
  , isEnabled :: EffectMth0 (Promise Boolean)
  , apiVersion :: EffectProp String
  , name :: EffectProp String
  , icon :: EffectProp String
  )

_Wallet
  :: { apiVersion :: Wallet -> Effect String
     , enable :: Wallet -> Effect (Promise Api)
     , icon :: Wallet -> Effect String
     , isEnabled :: Wallet -> Effect (Promise Boolean)
     , name :: Wallet -> Effect String
     }
_Wallet = mkFFI (Proxy :: Proxy Wallet)

type Cardano = JSObject
  ( eternl :: EffectProp (Nullable Wallet)
  , gerowallet :: EffectProp (Nullable Wallet)
  , lace :: EffectProp (Nullable Wallet)
  , nami :: EffectProp (Nullable Wallet)
  , yoroi :: EffectProp (Nullable Wallet)
  )

_Cardano
  :: { eternl :: Cardano -> Effect (Nullable Wallet)
     , gerowallet :: Cardano -> Effect (Nullable Wallet)
     , lace :: Cardano -> Effect (Nullable Wallet)
     , nami :: Cardano -> Effect (Nullable Wallet)
     , yoroi :: Cardano -> Effect (Nullable Wallet)
     }
_Cardano = mkFFI (Proxy :: Proxy Cardano)

-- | Manually tested and works with Nami (after a delay)
-- |
-- | The Nami and Yoroi browser extensions injects themselves into the
-- | running website with
-- | ```js
-- | window.cardano = { ...window.cardano, nami = stuff }
-- | ```
cardano :: Window -> Effect (Maybe Cardano)
cardano w = do
  eProp <- runExceptT $ Foreign.Index.readProp "cardano" $ Foreign.unsafeToForeign w
  case eProp of
    Left e -> throw $ show e
    Right prop
      | Foreign.isUndefined prop -> pure Nothing
      | otherwise -> pure $ Just $ Foreign.unsafeFromForeign prop

eternl :: Cardano -> Effect (Maybe Wallet)
eternl = map Nullable.toMaybe <<< _Cardano.eternl

gerowallet :: Cardano -> Effect (Maybe Wallet)
gerowallet = map Nullable.toMaybe <<< _Cardano.gerowallet

-- | Not yet manually tested.
lace :: Cardano -> Effect (Maybe Wallet)
lace = map Nullable.toMaybe <<< _Cardano.lace

-- | Manually tested and works with Nami.
-- |
-- | Remember that the Nami browser extension injects itself with
-- | ```js
-- | window.cardano = { ...window.cardano, nami = stuff }
-- | ```
-- | after a delay so if you want to wait for it with an artificial delay,
-- | you have to preceed the delay before invoking `cardano` rather than
-- | this procedure.
nami :: Cardano -> Effect (Maybe Wallet)
nami = map Nullable.toMaybe <<< _Cardano.nami

-- | Not yet manually tested.
yoroi :: Cardano -> Effect (Maybe Wallet)
yoroi = map Nullable.toMaybe <<< _Cardano.yoroi

-- | Manually tested and works with Nami.
apiVersion :: Wallet -> Effect String
apiVersion = _Wallet.apiVersion

-- | Manually tested and works with Nami.
enable_ :: Warn (Text "enable_ is deprecated, use enable instead") => Wallet -> Aff Api
enable_ = Promise.toAffE <<< _Wallet.enable

-- | Manually tested and works with Nami.
enable :: forall r. Wallet -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) Api)
enable = toAffEitherE rejectionAPIError <<< _Wallet.enable

-- | Manually tested and works with Nami.
icon :: Wallet -> Effect String
icon = _Wallet.icon

-- | Manually tested and works with Nami.
isEnabled_ :: Warn (Text "isEnabled_ is deprecated, use isEnabled instead") => Wallet -> Aff Boolean
isEnabled_ = Promise.toAffE <<< _Wallet.isEnabled

-- | Manually tested and works with Nami.
isEnabled :: forall r. Wallet -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) Boolean)
isEnabled = toAffEitherE rejectionAPIError <<< _Wallet.isEnabled

-- | Manually tested and works with Nami.
name :: Wallet -> Effect String
name = _Wallet.name

-- | Manually tested and works with Nami.
getNetworkId :: forall r. Api -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) Int)
getNetworkId = toAffEitherE rejectionAPIError <<< _Api.getNetworkId

-- | Manually tested and works with Nami.
getBalance :: forall r. Api -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) (CborHex ValueObject))
getBalance = toAffEitherE rejectionAPIError <<< _Api.getBalance

-- | Manually tested and works with Nami.
getChangeAddress :: forall r. Api -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) SomeAddress)
getChangeAddress = toAffEitherE rejectionAPIError <<< _Api.getChangeAddress

-- | Manually tested and works with Nami.
getCollateral :: forall r. Api -> Cbor Coin -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) (Array (Cbor TransactionUnspentOutput)))
getCollateral api = map (map (fold <<< Nullable.toMaybe)) <<< toAffEitherE rejectionAPIError <<< _Api.getCollateral api

-- | Manually tested and works with Nami.
getRewardAddresses :: forall r. Api -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) (Array (CborHex AddressObject)))
getRewardAddresses = toAffEitherE rejectionAPIError <<< _Api.getRewardAddresses

-- | Manually tested and works with Nami.
getUnusedAddresses :: forall r. Api -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) (Array (CborHex AddressObject)))
getUnusedAddresses = toAffEitherE rejectionAPIError <<< _Api.getUnusedAddresses

-- Most wallets return cbor hex of an AddrssObject as an output even though it is
-- against the spec which says that it should be a Bech32 string.
newtype SomeAddress = SomeAddress String

fromSomeAddress :: CardanoMultiplatformLib.Lib -> SomeAddress -> Effect (Maybe Bech32)
fromSomeAddress lib (SomeAddress s) = do
  let
    parseString = bech32FromString lib s
    parseCborHex = runGarbageCollector lib do
      for (HexString.hex s) \hex -> do
        let
          cborHex = unsafeCborHex hex
        bech32FromCborHex cborHex undefined

  (<|>) <$> parseString <*> parseCborHex

-- | Manually tested and works with Nami.
getUsedAddresses :: forall r. Api -> Aff (Either (Variant (| ApiError + ApiForeignErrors + UnknownError r)) (Array SomeAddress))
getUsedAddresses api = toAffEitherE rejectionAPIError <<< _Api.getUsedAddresses $ api

-- | Manually tested and works with Nami.
getUtxos
  :: forall r
   . Api
  -> Aff (Either (Maybe Number /\ (Variant (| ApiError + ApiForeignErrors + UnknownError r))) (Maybe (Array (CborHex TransactionUnspentOutputObject))))
getUtxos = map (map Nullable.toMaybe) <<< toAffEitherE rejectionPaginateError <<< _Api.getUtxos

signData :: forall r. Api -> CborHex AddressObject -> Bytes -> Aff (Either (Variant (| ApiError + DataSignError + ApiForeignErrors + UnknownError r)) Bytes)
signData api address = toAffEitherE rejectionDataSignError <<< _Api.signData api address

rejectionToForeign :: Promise.Rejection -> Foreign
rejectionToForeign = unsafeCoerce

rejectionAPIError :: forall r. Promise.Rejection -> Variant (| ApiError + ApiForeignErrors + UnknownError r)
rejectionAPIError rejection =
  let
    x :: Foreign
    x = rejectionToForeign rejection
  in
    either foreignErrors (fromMaybe' (\_ -> unknownError x) <<< toApiError) $ readWalletError x

rejectionDataSignError :: forall r. Promise.Rejection -> Variant (| ApiError + DataSignError + ApiForeignErrors + UnknownError r)
rejectionDataSignError rejection =
  let
    x :: Foreign
    x = rejectionToForeign rejection
  in
    readWalletError x # either foreignErrors \e ->
      fromMaybe' (\_ -> unknownError x) $ toApiError e <|> toDataSignError e

rejectionTxSignError :: forall r. Promise.Rejection -> Variant (| ApiError + TxSignError + ApiForeignErrors + UnknownError r)
rejectionTxSignError rejection =
  let
    x :: Foreign
    x = rejectionToForeign rejection
  in
    readWalletError x # either foreignErrors \e ->
      fromMaybe' (\_ -> unknownError x) $ toApiError e <|> toTxSignError e

rejectionTxSendError :: forall r. Promise.Rejection -> Variant (| ApiError + TxSendError + ApiForeignErrors + UnknownError r)
rejectionTxSendError rejection =
  let
    x :: Foreign
    x = rejectionToForeign rejection
  in
    readWalletError x # either foreignErrors \e ->
      fromMaybe' (\_ -> unknownError x) $ toApiError e <|> toTxSendError e

rejectionPaginateError :: forall r. Promise.Rejection -> Maybe Number /\ (Variant (| ApiError + ApiForeignErrors + UnknownError r))
rejectionPaginateError rejection =
  let
    x :: Foreign
    x = rejectionToForeign rejection

    maxSize :: Maybe Number
    maxSize = hush $ runExcept $ Foreign.readNumber x
  in
    maxSize /\ (either foreignErrors (fromMaybe' (\_ -> unknownError x) <<< toApiError) $ readWalletError x)

toAffEither :: forall a err. (Promise.Rejection -> err) -> Promise.Promise a -> Aff (Either err a)
toAffEither customCoerce p = makeAff \cb ->
  mempty <$
    Promise.thenOrCatch
      (\a -> Promise.resolve <$> cb (Right (Right a)))
      (\e -> Promise.resolve <$> cb (Right (Left (customCoerce e))))
      p

toAffEitherE :: forall a err. (Promise.Rejection -> err) -> Effect (Promise a) -> Aff (Either err a)
toAffEitherE coerce f = liftEffect f >>= toAffEither coerce

type SignTxError r = ApiError + TxSignError + ApiForeignErrors + UnknownError + r

signTx
  :: forall r
   . Api
  -> CborHex TransactionObject
  -> Boolean
  -> Aff (Either (Variant (SignTxError + r)) (CborHex TransactionWitnessSetObject))
signTx api cbor = toAffEitherE rejectionTxSignError <<< _Api.signTx api cbor

submitTx
  :: forall r
   . Api
  -> CborHex TransactionObject
  -> Aff (Either (Variant (| ApiError + TxSendError + ApiForeignErrors + UnknownError r)) (CborHex TransactionHashObject))
submitTx api = toAffEitherE rejectionTxSendError <<< _Api.submitTx api
