module Component.CreateContract.Machine where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject) as R
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.InputHelper (rolesInContract)
import Component.Types (ErrorReport, WalletInfo(..), mkErrorReport)
import Contrib.ErrorToJson (errorToJson)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (encodeJson)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now as Instant
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web (SafetyErrorInfo) as R
import Marlowe.Runtime.Web.Client (ClientError, clientErrorToJson, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsError, PostContractsRequest(..), PostContractsResponseContent(..), PutContractRequest(PutContractRequest), ResourceWithLinks, RolesConfig, Runtime(Runtime), ServerURL, Tags, TextEnvelope(TextEnvelope), encodeApiError, toTextEnvelope)
import Marlowe.Runtime.Web.Types (TextEnvelope, TxOutRef) as R
import React.Basic (JSX)
import React.Basic.DOM as D
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet as Wallet
import WalletContext (WalletContext(..), walletContext)

type ClientError' = ClientError PostContractsError

newtype PostContractsResponseContent' =
  PostContractsResponseContent'
    { contractId :: R.TxOutRef
    , tx :: R.TextEnvelope R.TransactionObject
    }

data CreateError
  = SafetyErrors (NonEmptyArray R.SafetyErrorInfo)
  | ClientError ClientError'

data MachineError
  = CreateError CreateError
  | OtherError (ErrorReport JSX)

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
      , errors :: Maybe (ErrorReport JSX)
      }
  | FetchingRequiredWalletContext
      { contract :: V1.Contract
      , tags :: Tags
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe (ErrorReport JSX)
      }
  | CreatingTx
      { contract :: V1.Contract
      , tags :: Tags
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe CreateError
      , reqWalletContext :: RequiredWalletContext
      }
  | SigningTx
      { contract :: V1.Contract
      , rolesConfig :: Maybe RolesConfig
      , tags :: Tags
      , errors :: Maybe (ErrorReport JSX)
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent' (contract :: ContractEndpoint)
      }
  | SubmittigTx
      { contract :: V1.Contract
      , rolesConfig :: Maybe RolesConfig
      , errors :: Maybe (ErrorReport JSX)
      , tags :: Tags
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent' (contract :: ContractEndpoint)
      }
  | ContractCreated
      { contract :: V1.Contract
      , rolesConfig :: Maybe RolesConfig
      , tags :: Tags
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent' (contract :: ContractEndpoint)
      , submittedAt :: Instant
      }

stateErrors :: State -> Maybe MachineError
stateErrors DefiningContract = Nothing
stateErrors (DefiningRoleTokens { errors }) = OtherError <$> errors
stateErrors (FetchingRequiredWalletContext { errors }) = OtherError <$> errors
stateErrors (CreatingTx { errors }) = CreateError <$> errors
stateErrors (SigningTx { errors }) = OtherError <$> errors
stateErrors (SubmittigTx { errors }) = OtherError <$> errors
stateErrors (ContractCreated {}) = Nothing

data Action
  = TriggerSubmission V1.Contract Tags
  | DefineRoleTokens
  | DefineRoleTokensFailed (ErrorReport JSX)
  | DefineRoleTokensSucceeded RolesConfig
  | FetchRequiredWalletContext
  | FetchRequiredWalletContextFailed (ErrorReport JSX)
  | FetchRequiredWalletContextSucceeded RequiredWalletContext
  | CreateTx
  | CreateTxFailed CreateError
  | CreateTxSucceeded
      (ResourceWithLinks PostContractsResponseContent' (contract :: ContractEndpoint))
  | SignTx
  | SignTxFailed (ErrorReport JSX)
  | SignTxSucceeded (CborHex TransactionWitnessSetObject)
  | SubmitTx
  | SubmitTxFailed (ErrorReport JSX)
  | SubmitTxSucceeded Instant

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
      DefineRoleTokensFailed error -> DefiningRoleTokens { contract, tags, roleNames, errors: Just $ error }
      DefineRoleTokensSucceeded rolesConfig -> FetchingRequiredWalletContext { contract, tags, rolesConfig: Just rolesConfig, errors: Nothing }
      _ -> state
    FetchingRequiredWalletContext r@{ errors: Just _ } -> case action of
      FetchRequiredWalletContext -> FetchingRequiredWalletContext $ r { errors = Nothing }
      _ -> state
    FetchingRequiredWalletContext r -> case action of
      FetchRequiredWalletContextFailed error -> FetchingRequiredWalletContext $ r { errors = Just error }
      FetchRequiredWalletContextSucceeded reqWalletContext -> CreatingTx
        $ Record.set (Proxy :: Proxy "errors") Nothing
        $ Record.insert (Proxy :: Proxy "reqWalletContext") reqWalletContext
            r
      _ -> state
    CreatingTx r@{ errors: Just _ } -> case action of
      CreateTx -> CreatingTx $ r { errors = Nothing }
      _ -> state
    CreatingTx r@{ contract, rolesConfig, tags } -> case action of
      CreateTxFailed errors -> CreatingTx $ r { errors = Just errors }
      CreateTxSucceeded createTxResponse ->
        SigningTx $ { contract, createTxResponse, errors: Nothing, rolesConfig, tags }
      _ -> state
    SigningTx r@{ errors: Just _ } -> case action of
      SignTx -> SigningTx $ r { errors = Nothing }
      _ -> state
    SigningTx r@{ contract, createTxResponse: createTxResponse, rolesConfig, tags } -> case action of
      SignTxFailed err -> SigningTx $ r { errors = Just err }
      SignTxSucceeded txWitnessSet -> SubmittigTx { contract, createTxResponse, errors: Nothing, txWitnessSet, rolesConfig, tags }
      _ -> state
    SubmittigTx r@{ errors: Just _ } -> case action of
      SubmitTx -> SubmittigTx $ r { errors = Nothing }
      _ -> state
    SubmittigTx { contract, createTxResponse, txWitnessSet, rolesConfig, tags } -> case action of
      SubmitTxFailed err -> SubmittigTx { contract, createTxResponse, errors: Just err, txWitnessSet, rolesConfig, tags }
      SubmitTxSucceeded submittedAt -> ContractCreated
        { contract, createTxResponse, rolesConfig, submittedAt, tags }
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
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent' (contract :: ContractEndpoint)
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
    SigningTx { createTxResponse: { resource: PostContractsResponseContent' response }, errors: Nothing } -> do
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
        Left err -> do
          let
            json = errorToJson err
            err' = D.text <$> mkErrorReport "Failed to fetch wallet context" Nothing (Just json)
          pure $ FetchRequiredWalletContextFailed err'
        Right Nothing -> do
          let
            err = D.text <$> mkErrorReport "Wallet doesn't have change address" Nothing Nothing
          pure $ FetchRequiredWalletContextFailed err
        Right (Just (WalletContext { changeAddress, usedAddresses })) -> do
          pure $ FetchRequiredWalletContextSucceeded { changeAddress, usedAddresses }
    SignTxRequest { walletInfo, tx } -> do
      let
        WalletInfo { wallet } = walletInfo
      liftAff $ sign wallet tx >>= case _ of
        Left err -> do
          let
            json = encodeJson { "strigified": unsafeStringify err }
            err' = D.text <$> mkErrorReport "Failed to sign transaction" Nothing (Just json)
          pure $ SignTxFailed err'
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
      -- FIXME: Handle aff excpetions
      liftAff $ create contractData serverURL root >>= case _ of
        Right { links, resource } -> case resource of
          PostContractsResponseSafetyErrors errors ->
            pure $ CreateTxFailed $ SafetyErrors errors
          PostContractsResponseContent res -> do
            let
              resource' = PostContractsResponseContent' res
            pure $ CreateTxSucceeded { links, resource: resource' }
        Left err -> pure $ CreateTxFailed $ ClientError err
    SubmitTxRequest { txWitnessSet, createTxResponse, runtime } -> do
      let
        Runtime { serverURL } = runtime
        action = liftAff $ submit txWitnessSet serverURL createTxResponse.links.contract >>= case _ of
          Right _ -> do
            now <- liftEffect $ Instant.now
            pure $ SubmitTxSucceeded now
          Left err -> do
            let
              json = clientErrorToJson encodeApiError err
              err' = D.text <$> mkErrorReport "Failed to submit transaction" Nothing (Just json)
            pure $ SubmitTxFailed err'
      action `catchError` \err -> do
        let
          json = errorToJson err
          err' = D.text <$> mkErrorReport "Failed to submit transaction" Nothing (Just json)
        pure $ SubmitTxFailed err'

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
  -> Aff (Either (ClientError String) Unit)
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

