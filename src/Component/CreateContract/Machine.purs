module Component.CreateContract.Machine where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.Types (WalletInfo(..))
import Contrib.Fetch (FetchError)
import Control.Monad.Error.Class (catchError)
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
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsError, PostContractsRequest(..), PostContractsResponseContent(..), PutContractRequest(PutContractRequest), Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), ResourceWithLinks, toTextEnvelope)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Simplified.Generated as S
import Wallet as Wallet
import WalletContext (WalletContext(..), walletContext)

type ClientError' = ClientError PostContractsError

data ContractData = ContractData
  { contract :: V1.Contract
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
  | FetchingRequiredWalletContext
      { contract :: V1.Contract
      , errors :: Maybe String
      }
  | CreatingTx
      { contract :: V1.Contract
      , errors :: Maybe String
      , reqWalletContext :: RequiredWalletContext
      }
  | SigningTx
      { contract :: V1.Contract
      , errors :: Maybe String
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      }
  | SubmittigTx
      { contract :: V1.Contract
      , errors :: Maybe String
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      }
  | ContractCreated
      { contract :: V1.Contract
      , createTxResponse :: ResourceWithLinks PostContractsResponseContent (contract :: ContractEndpoint)
      }

data Action
  = TriggerSubmission V1.Contract
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
      TriggerSubmission contract -> FetchingRequiredWalletContext { contract, errors: Nothing }
      _ -> state
    FetchingRequiredWalletContext { contract, errors: Just _ } -> case action of
      FetchRequiredWalletContext -> FetchingRequiredWalletContext { contract, errors: Nothing }
      _ -> state
    FetchingRequiredWalletContext { contract } -> case action of
      FetchRequiredWalletContextFailed error -> FetchingRequiredWalletContext $ { errors: Just error, contract }
      FetchRequiredWalletContextSucceeded reqWalletContext -> CreatingTx { contract, errors: Nothing, reqWalletContext }
      _ ->  state
    CreatingTx { contract, reqWalletContext, errors: Just _ } -> case action of
      CreateTx -> CreatingTx { contract, errors: Nothing, reqWalletContext }
      _ -> state
    CreatingTx { contract, reqWalletContext } -> case action of
      CreateTxFailed err -> CreatingTx { contract, errors: Just err, reqWalletContext }
      CreateTxSucceeded res -> do
        SigningTx { contract, errors: Nothing, createTxResponse: res }
      _ -> state
    SigningTx { contract, createTxResponse, errors: Just _ } -> case action of
      SignTx -> SigningTx { contract, errors: Nothing, createTxResponse }
      _ -> state
    SigningTx { contract, createTxResponse: createTxResponse } -> case action of
      SignTxFailed err -> SigningTx { contract, errors: Just err, createTxResponse }
      SignTxSucceeded txWitnessSet -> SubmittigTx { contract, createTxResponse, errors: Nothing, txWitnessSet }
      _ -> state
    SubmittigTx { contract, createTxResponse, txWitnessSet, errors: Just _ } -> case action of
      SubmitTx -> SubmittigTx { contract, createTxResponse, errors: Nothing, txWitnessSet }
      _ -> state
    SubmittigTx { contract, createTxResponse, txWitnessSet } -> case action of
      SubmitTxFailed err -> SubmittigTx { contract, createTxResponse, errors: Just err, txWitnessSet }
      SubmitTxSucceeded -> ContractCreated { contract, createTxResponse }
      _ -> state
    (ContractCreated _) -> state

-- | This is an initial action which we should trigger
-- | when we want to start the contract creation process.
triggerSubmission :: V1.Contract -> Action
triggerSubmission = TriggerSubmission

initialState ::  State
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
    CreatingTx { contract, reqWalletContext, errors: Nothing } ->
      Just $ RuntimeRequest $ CreateTxRequest { contract, reqWalletContext, runtime }
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
    CreateTxRequest { contract, reqWalletContext, runtime } -> do
      let
        Runtime { serverURL, root } = runtime
        contractData = ContractData
          { contract
          , walletAddresses: reqWalletContext
          }
      traceM "WTF?"
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
    ContractData { contract, walletAddresses: { changeAddress, usedAddresses } } = contractData
    req = PostContractsRequest
      { metadata: mempty
      -- , version :: MarloweVersion
      , roles: Nothing
      , tags: mempty -- TODO: use instead of metadata
      , contract
      , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
      , changeAddress: changeAddress
      , addresses: usedAddresses <> [ changeAddress ]
      , collateralUTxOs: []
      }
  traceM "WTF:"
  traceM usedAddresses
  traceM changeAddress

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


-- | We want to describe in details what kind of data we are gathering
-- | when we are performing a given transtition (state determines the next transition in our case)
-- | The output should be readable to the developer which should understand the whole flow.
-- | Let's use standard react-basic JSX functions like: DOM.div { className: "foo" } [ DOM.text "bar" ]
stateToDetailedDescription :: State -> JSX
stateToDetailedDescription state = case state of
  DefiningContract -> DOM.div_
    [ S.p {} $ DOM.text "We are in the initial state, we are waiting for the user to trigger the contract creation process."
    , S.p {} $ DOM.text "When we get the correct contract value (JSON) we gonna use it as a part of the request to the marlowe-runtime."
    ]
  FetchingRequiredWalletContext { errors: Nothing } -> DOM.div_
    [ S.p {} $ DOM.text "We are fetching the required wallet context."
    , S.p {} $ DOM.text "marlowe-runtime requires information about wallet addresses so it can pick UTxO to pay for the initial transaction."
    , S.p {} $ DOM.text $
        "To gain the address set from the wallet we use CIP-30 `getUsedAddresses` method and reencoding them from lower "
        <> "level cardano CBOR hex into Bech32 (`addr_test...`)."
    ]
  FetchingRequiredWalletContext { errors: Just error } -> DOM.div_
    [ S.p {} $ DOM.text "It seems that the provided wallet is lacking addresses or failed to execute the method:"
    , S.p {} $ DOM.text error
    ]
  CreatingTx { errors: Nothing } -> DOM.div_
    [ S.p {} $ DOM.text "We are using the marlowe-runtime to create the initial transaction."
    ]
  CreatingTx { reqWalletContext, errors: Just error } -> DOM.div_
    [ S.p {} $ DOM.text "It seems that the marlowe-runtime failed to create the initial transaction:"
    , S.p {} $ DOM.text error
    , S.p {} $ DOM.text "The wallet context we used:"
    , S.p {} $ DOM.text $ unsafeStringify reqWalletContext
    ]
  SigningTx { errors: Nothing } -> DOM.div_
    [ S.p {} $ DOM.text "We are signing the initial transaction."
    ]
  SigningTx { errors: Just error } -> DOM.div_
    [ S.p {} $ DOM.text "It seems that the wallet failed to sign the initial transaction:"
    , S.p {} $ DOM.text error
    ]
  SubmittigTx { errors: Nothing } -> DOM.div_
    [ S.p {} $ DOM.text "We are submitting the initial transaction."
    ]
  SubmittigTx { errors: Just error } -> DOM.div_
    [ S.p {} $ DOM.text "It seems that the marlowe-runtime failed to submit the initial transaction:"
    , S.p {} $ DOM.text error
    ]
  ContractCreated _ -> DOM.div_
    [ S.p {} $ DOM.text "The contract was created successfully."
    ]

