module Component.CreateContract.Machine where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.InputHelper (rolesInContract)
import Component.Types (WalletInfo(..))
import Contrib.Fetch (FetchError)
import Control.Monad.Error.Class (catchError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsError, PostContractsRequest(..), PostContractsResponseContent(..), PutContractRequest(PutContractRequest), ResourceWithLinks, RolesConfig, Runtime(Runtime), ServerURL, Tags(..), TextEnvelope(TextEnvelope), toTextEnvelope)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Simplified.Generated as S
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet as Wallet
import WalletContext (WalletContext(..), walletContext)

type ClientError' = ClientError PostContractsError

data ContractData = ContractData
  { contract :: V1.Contract
  , tags :: Tags
  , rolesConfig :: Maybe RolesConfig
  , walletAddresses ::
      { changeAddress :: Bech32
      , usedAddresses :: Array Bech32
      }
  -- , collateralUTxOs :: Array TxOutRef
  }

type WalletAddresses = { usedAddresses :: Array Bech32, changeAddress :: Bech32 }

type RequiredWalletContext = WalletAddresses

-- | React and UI agnostic (modulo error messages) state machine for contract creation.
-- | This state machine is pretty linear and all state which contains `errors` can be retried.
data State
  = DefiningContract
  | DefiningRoleTokens
      { contract :: V1.Contract
      , tags :: Tags
      , roleNames :: NonEmptyArray V1.TokenName
      , errors :: Maybe String
      }
  | FetchingRequiredWalletContext
      { contract :: V1.Contract
      , tags :: Tags
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe String
      }
  | CreatingTx
      { contract :: V1.Contract
      , tags :: Tags
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe String
      , reqWalletContext :: RequiredWalletContext
      }
  | SigningTx
      { contract :: V1.Contract
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe String
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      }
  | SubmittigTx
      { contract :: V1.Contract
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe String
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      }
  | ContractCreated
      { contract :: V1.Contract
      , rolesConfig :: Maybe RolesConfig
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      }

data Action
  = TriggerSubmission V1.Contract Tags
  | DefineRoleTokens
  | DefineRoleTokensFailed String
  | DefineRoleTokensSucceeded RolesConfig
  | FetchRequiredWalletContext
  | FetchRequiredWalletContextFailed String
  | FetchRequiredWalletContextSucceeded RequiredWalletContext
  | CreateTx
  | CreateTxFailed String
  | CreateTxSucceeded
      (ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint))
  | SignTx
  | SignTxFailed String
  | SignTxSucceeded (CborHex TransactionWitnessSetObject)
  | SubmitTx
  | SubmitTxFailed String
  | SubmitTxSucceeded

type Env =
  { connectedWallet :: WalletInfo Wallet.Api
  , cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , runtime :: Runtime
  }

-- | A create contract state machine implementation
step :: State -> Action -> State
step state action = do
  case state of
    DefiningContract -> case action of
      TriggerSubmission contract tags -> case Array.uncons (rolesInContract contract) of
        Nothing -> FetchingRequiredWalletContext { contract, tags, rolesConfig: Nothing, errors: Nothing }
        Just { head, tail } ->
          DefiningRoleTokens { contract, tags, roleNames: Array.NonEmpty.cons' head tail, errors: Nothing }
      _ -> state
    DefiningRoleTokens { contract, tags, roleNames, errors: Just _ } -> case action of
      DefineRoleTokens -> DefiningRoleTokens { contract, tags, roleNames, errors: Nothing }
      _ -> state
    DefiningRoleTokens { contract, tags, roleNames, errors: Nothing } -> case action of
      DefineRoleTokensFailed error -> DefiningRoleTokens { contract, tags, roleNames, errors: Just error }
      DefineRoleTokensSucceeded rolesConfig -> FetchingRequiredWalletContext { contract, tags, rolesConfig: Just rolesConfig, errors: Nothing }
      _ -> state
    FetchingRequiredWalletContext r@{ errors: Just _ } -> case action of
      FetchRequiredWalletContext -> FetchingRequiredWalletContext $ r{ errors = Nothing }
      _ -> state
    FetchingRequiredWalletContext r -> case action of
      FetchRequiredWalletContextFailed error -> FetchingRequiredWalletContext $ r { errors = Just error }
      FetchRequiredWalletContextSucceeded reqWalletContext -> CreatingTx $ Record.insert (Proxy :: Proxy "reqWalletContext") reqWalletContext r
      _ -> state
    CreatingTx r@{ contract, reqWalletContext, errors: Just _ } -> case action of
      CreateTx -> CreatingTx $ r { errors = Nothing }
      _ -> state
    CreatingTx r@{ contract, rolesConfig } -> case action of
      CreateTxFailed err -> CreatingTx $ r { errors = Just err }
      CreateTxSucceeded res -> do
        SigningTx $ { contract, createTxResponse: res, errors: Nothing, rolesConfig }
      _ -> state
    SigningTx r@{ contract, createTxResponse, errors: Just _ } -> case action of
      SignTx -> SigningTx $ r { errors = Nothing }
      _ -> state
    SigningTx r@{ contract, createTxResponse: createTxResponse, rolesConfig } -> case action of
      SignTxFailed err -> SigningTx $ r { errors = Just err }
      SignTxSucceeded txWitnessSet -> SubmittigTx { contract, createTxResponse, errors: Nothing, txWitnessSet, rolesConfig }
      _ -> state
    SubmittigTx r@{ contract, createTxResponse, txWitnessSet, errors: Just _ } -> case action of
      SubmitTx -> SubmittigTx $ r { errors = Nothing }
      _ -> state
    SubmittigTx { contract, createTxResponse, txWitnessSet, rolesConfig } -> case action of
      SubmitTxFailed err -> SubmittigTx { contract, createTxResponse, errors: Just err, txWitnessSet, rolesConfig }
      SubmitTxSucceeded -> ContractCreated { contract, createTxResponse, rolesConfig }
      _ -> state
    (ContractCreated _) -> state

-- | This is an initial action which we should trigger
-- | when we want to start the contract creation process.
triggerSubmission :: V1.Contract -> Tags -> Action
triggerSubmission = TriggerSubmission

initialState :: State
initialState = DefiningContract

data WalletRequest
  = FetchWalletContextRequest
      { cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
      , walletInfo :: WalletInfo Wallet.Api
      }
  | SignTxRequest
      { walletInfo :: WalletInfo Wallet.Api
      , tx :: TextEnvelope TransactionObject
      }

data RuntimeRequest
  = CreateTxRequest
      { contract :: V1.Contract
      , tags :: Tags
      , rolesConfig :: Maybe RolesConfig
      , reqWalletContext :: RequiredWalletContext
      , runtime :: Runtime
      }
  | SubmitTxRequest
      { txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      , runtime :: Runtime
      }

data Request
  = WalletRequest WalletRequest
  | RuntimeRequest RuntimeRequest

-- based on the `driver` let's write this function.
nextRequest :: Env -> State -> Maybe Request
nextRequest env = do
  let
    { cardanoMultiplatformLib, connectedWallet: walletInfo, runtime } = env
  case _ of
    FetchingRequiredWalletContext { errors: Nothing } ->
      Just $ WalletRequest $ FetchWalletContextRequest { cardanoMultiplatformLib, walletInfo }
    CreatingTx { contract, tags, reqWalletContext, errors: Nothing, rolesConfig } ->
      Just $ RuntimeRequest $ CreateTxRequest { contract, tags, reqWalletContext, runtime, rolesConfig }
    SigningTx { createTxResponse: { resource: PostContractsResponseContent response }, errors: Nothing } -> do
      let
        { tx } = response
      Just $ WalletRequest $ SignTxRequest { walletInfo, tx }
    SubmittigTx { createTxResponse, txWitnessSet, errors: Nothing } ->
      Just $ RuntimeRequest $ SubmitTxRequest { txWitnessSet, createTxResponse, runtime }
    _ -> Nothing

-- We want to rewrite driver logic here based on the request type
requestToAffAction :: Request -> Aff Action
requestToAffAction = case _ of
  WalletRequest walletRequest -> case walletRequest of
    FetchWalletContextRequest { cardanoMultiplatformLib, walletInfo } -> do
      let
        WalletInfo { wallet } = walletInfo
      possibleWalletAddresses <- liftAff $ (Right <$> walletContext cardanoMultiplatformLib wallet) `catchError` (pure <<< Left)
      case possibleWalletAddresses of
        Left err -> pure $ FetchRequiredWalletContextFailed $ show err
        Right (WalletContext { changeAddress: Just changeAddress, usedAddresses }) -> do
          pure $ FetchRequiredWalletContextSucceeded { changeAddress, usedAddresses }
        Right (WalletContext { changeAddress: Nothing }) -> pure $ FetchRequiredWalletContextFailed "Wallet does not have a change address"
    SignTxRequest { walletInfo, tx } -> do
      let
        WalletInfo { wallet } = walletInfo
      liftAff $ sign wallet tx >>= case _ of
        Left err -> pure $ SignTxFailed $ unsafeStringify err
        Right txWitnessSet -> pure $ SignTxSucceeded txWitnessSet
  RuntimeRequest runtimeRequest -> case runtimeRequest of
    CreateTxRequest { contract, tags, reqWalletContext, runtime, rolesConfig } -> do
      let
        Runtime { serverURL, root } = runtime
        contractData = ContractData
          { contract
          , tags
          , rolesConfig
          , walletAddresses: reqWalletContext
          }
      liftAff $ create contractData serverURL root >>= case _ of
        Right res -> pure $ CreateTxSucceeded res
        Left err -> pure $ CreateTxFailed $ show err
    SubmitTxRequest { txWitnessSet, createTxResponse, runtime } -> do
      let
        Runtime { serverURL } = runtime
      liftAff $ submit txWitnessSet serverURL createTxResponse.links.contract >>= case _ of
        Right _ -> pure SubmitTxSucceeded
        Left err -> pure $ SubmitTxFailed $ show err

driver :: Env -> State -> Maybe (Aff Action)
driver env state = do
  request <- nextRequest env state
  pure $ requestToAffAction request

-- Lower level helpers
create
  :: ContractData
  -> ServerURL
  -> ContractsEndpoint
  -> Aff (Either ClientError' { resource :: PostContractsResponseContent, links :: { contract :: ContractEndpoint } })
create contractData serverUrl contractsEndpoint = do
  let
    ContractData { contract, tags, rolesConfig, walletAddresses: { changeAddress, usedAddresses } } = contractData
    req = PostContractsRequest
      { metadata: mempty
      -- , version :: MarloweVersion
      , roles: rolesConfig
      , tags
      , contract
      , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
      , changeAddress: changeAddress
      , addresses: usedAddresses <> [ changeAddress ]
      , collateralUTxOs: []
      }
  post' serverUrl contractsEndpoint req

submit
  :: CborHex TransactionWitnessSetObject
  -> ServerURL
  -> ContractEndpoint
  -> Aff (Either FetchError Unit)
submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutContractRequest textEnvelope
  put' serverUrl contractEndpoint req

sign
  :: Wallet.Api
  -> TextEnvelope TransactionObject
  -> Aff
       ( Either
           (Variant (Wallet.SignTxError ()))
           (CborHex TransactionWitnessSetObject)
       )
sign walletApi tx = do
  let
    TextEnvelope { cborHex: txCborHex } = tx
  Wallet.signTx walletApi txCborHex false

