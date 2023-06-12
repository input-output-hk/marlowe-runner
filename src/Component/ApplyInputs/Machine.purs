module Component.ApplyInputs.Machine where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.InputHelper (ChoiceInput, DepositInput, NotifyInput)
import Component.Types (WalletInfo(..))
import Contrib.Fetch (FetchError)
import Control.Monad.Error.Class (catchError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (fromDuration)
import Data.Time.Duration as Time.Duration
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (PostContractsError, PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), ResourceWithLinks, Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Partial.Unsafe (unsafeCrashWith)
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

type AllInputsChoices = Either
  V1.Contract
  { deposits :: Maybe (NonEmptyArray DepositInput)
  , choices :: Maybe (NonEmptyArray ChoiceInput)
  , notifies :: Maybe NotifyInput
  }

data InputChoices
  = DepositInputs (Array DepositInput)
  | ChoiceInputs (Array ChoiceInput)
  | SpecificNotifyInput NotifyInput

data State
  = ChoosingInputType
      { errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | PickingInput
      { errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | FetchingRequiredWalletContext
      { allInputsChoices :: AllInputsChoices
      , transactionsEndpoint :: TransactionsEndpoint
      , inputChoices :: InputChoices
      , errors :: Maybe String
      , input :: V1.Input
      }
  | CreatingTx
      { errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , transactionsEndpoint :: TransactionsEndpoint
      , inputChoices :: InputChoices
      , input :: V1.Input
      , requiredWalletContext :: RequiredWalletContext
      }
  | SigningTx
      { errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: V1.Input
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      }
  | SubmittingTx
      { errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: V1.Input
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      }
  | InputApplied
      { allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: V1.Input
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      }

data Action
  = ChooseInputType
      { allInputsChoices :: AllInputsChoices
      , transactionEndpoint :: TransactionsEndpoint
      }
  | ChooseInputTypeFailed String
  | ChooseInputTypeSucceeded InputChoices
  | PickInput
  | PickInputFailed String
  | PickInputSucceeded V1.Input
  | FetchRequiredWalletContext
  | FetchRequiredWalletContextFailed String
  | FetchRequiredWalletContextSucceeded RequiredWalletContext
  | CreateTx
  | CreateTxFailed String
  | CreateTxSucceeded (ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint))
  | SignTx
  | SignTxFailed String
  | SignTxSucceeded (CborHex TransactionWitnessSetObject)
  | SubmitTx
  | SubmitTxFailed String
  | SubmitTxSucceeded

step :: State -> Action -> State
step state action = do
  case state of
    ChoosingInputType r@{ errors: Just _ } -> case action of
      ChooseInputType _ -> ChoosingInputType $ r { errors = Nothing }
      _ -> state
    ChoosingInputType r@{ allInputsChoices, transactionsEndpoint } -> case action of
      ChooseInputTypeFailed error -> ChoosingInputType $ r { errors = Just error }
      ChooseInputTypeSucceeded inputChoices -> PickingInput
        { errors: Nothing
        , allInputsChoices
        , inputChoices
        , transactionsEndpoint
        }
      _ -> state
    PickingInput r@{ errors: Just _ } -> case action of
      PickInput -> PickingInput $ r { errors = Nothing }
      _ -> state
    PickingInput r@{ allInputsChoices, inputChoices, transactionsEndpoint } -> case action of
      PickInputFailed error -> PickingInput $ r { errors = Just error }
      PickInputSucceeded input -> FetchingRequiredWalletContext
        { errors: Nothing
        , allInputsChoices
        , inputChoices
        , input
        , transactionsEndpoint
        }
      _ -> state
    FetchingRequiredWalletContext r@{ errors: Just _ } -> case action of
      FetchRequiredWalletContext -> FetchingRequiredWalletContext $ r { errors = Nothing }
      _ -> state
    FetchingRequiredWalletContext r@{ allInputsChoices, transactionsEndpoint, inputChoices, input } -> case action of
      FetchRequiredWalletContextFailed error -> FetchingRequiredWalletContext $ r { errors = Just error }
      FetchRequiredWalletContextSucceeded requiredWalletContext -> CreatingTx
        { errors: Nothing
        , allInputsChoices
        , inputChoices
        , input
        , requiredWalletContext
        , transactionsEndpoint
        }
      _ -> state
    CreatingTx r@{ errors: Just _ } -> case action of
      CreateTx -> CreatingTx $ r { errors = Nothing }
      _ -> state
    CreatingTx r@{ allInputsChoices, inputChoices, input } -> case action of
      CreateTxFailed error -> CreatingTx $ r { errors = Just error }
      CreateTxSucceeded createTxResponse -> SigningTx { errors: Nothing, allInputsChoices, inputChoices, input, createTxResponse }
      _ -> state
    SigningTx r@{ errors: Just _ } -> case action of
      SignTx -> SigningTx $ r { errors = Nothing }
      _ -> state
    SigningTx r@{ allInputsChoices, inputChoices, input, createTxResponse } -> case action of
      SignTxFailed error -> SigningTx $ r { errors = Just error }
      SignTxSucceeded txWitnessSet -> SubmittingTx { errors: Nothing, allInputsChoices, inputChoices, input, createTxResponse, txWitnessSet }
      _ -> state
    SubmittingTx r@{ errors: Just _ } -> case action of
      SubmitTx -> SubmittingTx $ r { errors = Nothing }
      _ -> state
    SubmittingTx r@{ allInputsChoices, inputChoices, input, createTxResponse, txWitnessSet } -> case action of
      SubmitTxFailed error -> SubmittingTx $ r { errors = Just error }
      SubmitTxSucceeded ->
        InputApplied { allInputsChoices, inputChoices, input, txWitnessSet, createTxResponse }
      _ -> state
    InputApplied _ -> state

type Env =
  { connectedWallet :: WalletInfo Wallet.Api
  , cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , runtime :: Runtime
  }

initialState :: AllInputsChoices -> TransactionsEndpoint -> State
initialState allInputsChoices transactionsEndpoint = ChoosingInputType { errors: Nothing, allInputsChoices, transactionsEndpoint }

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
      { allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: V1.Input
      , requiredWalletContext :: RequiredWalletContext
      , serverURL :: ServerURL
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | SubmitTxRequest
      { txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      , serverURL :: ServerURL
      }

data Request
  = WalletRequest WalletRequest
  | RuntimeRequest RuntimeRequest

nextRequest :: Env -> State -> Maybe Request
nextRequest env = do
  let
    { cardanoMultiplatformLib, connectedWallet: walletInfo, runtime } = env
    Runtime { serverURL } = runtime
  case _ of
    PickingInput { errors: Nothing } ->
      Just $ WalletRequest $ FetchWalletContextRequest { cardanoMultiplatformLib, walletInfo }
    CreatingTx { errors: Nothing, inputChoices, input, allInputsChoices, requiredWalletContext, transactionsEndpoint } -> do
      Just $ RuntimeRequest $ CreateTxRequest
        { allInputsChoices
        , inputChoices
        , input
        , requiredWalletContext
        , serverURL
        , transactionsEndpoint
        }
    SigningTx { errors: Nothing, createTxResponse } -> do
      let
        { resource: PostTransactionsResponse { tx } } = createTxResponse
      Just $ WalletRequest $ SignTxRequest { walletInfo, tx }
    SubmittingTx { errors: Nothing, createTxResponse, txWitnessSet } ->
      Just $ RuntimeRequest $ SubmitTxRequest { txWitnessSet, createTxResponse, serverURL }
    _ -> Nothing

-- We want to rewrite driver logic here based on the request type
requestToAffAction :: Request -> Aff Action
requestToAffAction = case _ of
  WalletRequest walletRequest -> case walletRequest of
    FetchWalletContextRequest { cardanoMultiplatformLib, walletInfo } -> do
      let
        WalletInfo { wallet } = walletInfo
      possibleWalletAddresses <- (Right <$> walletContext cardanoMultiplatformLib wallet) `catchError` (pure <<< Left)
      case possibleWalletAddresses of
        Left err -> pure $ FetchRequiredWalletContextFailed $ show err
        Right (WalletContext { changeAddress: Just changeAddress, usedAddresses }) -> do
          pure $ FetchRequiredWalletContextSucceeded { changeAddress, usedAddresses }
        Right (WalletContext { changeAddress: Nothing }) -> pure $ FetchRequiredWalletContextFailed "Wallet does not have a change address"
    SignTxRequest { walletInfo, tx } -> do
      let
        WalletInfo { wallet } = walletInfo
      sign wallet tx >>= case _ of
        Left err -> pure $ SignTxFailed $ unsafeStringify err
        Right txWitnessSet -> pure $ SignTxSucceeded txWitnessSet
  RuntimeRequest runtimeRequest -> case runtimeRequest of
    CreateTxRequest { input, requiredWalletContext, serverURL, transactionsEndpoint } -> do
      create [input] requiredWalletContext serverURL transactionsEndpoint >>= case _ of
        Right res -> pure $ CreateTxSucceeded res
        Left err -> pure $ CreateTxFailed $ show err
    SubmitTxRequest { txWitnessSet, createTxResponse, serverURL } -> do
      submit txWitnessSet serverURL createTxResponse.links.transaction >>= case _ of
        Right _ -> pure $ SubmitTxSucceeded
        Left err -> pure $ SubmitTxFailed $ show err

driver :: Env -> State -> Maybe (Aff Action)
driver env state = do
  request <- nextRequest env state
  pure $ requestToAffAction request

-- Lower level helpers
create
  :: Array V1.Input
  -> WalletAddresses
  -> ServerURL
  -> TransactionsEndpoint
  -> Aff (Either ClientError' { resource :: PostTransactionsResponse, links :: { transaction :: TransactionEndpoint } })
create inputs walletAddresses serverUrl transactionsEndpoint = do
  nowInstant <- liftEffect $ now
  let
    nowPosixMilliseconds = unInstant nowInstant
    oneHour = fromDuration $ Time.Duration.Hours 1.0
    inOneHourInstant = case instant (nowPosixMilliseconds <> oneHour) of
      Just instant' -> instant'
      Nothing -> unsafeCrashWith "Component.ApplyInputs.Machine: Failed to add one hour to the current time"
  let
    { changeAddress, usedAddresses } = walletAddresses

    -- newtype PostTransactionsRequest = PostTransactionsRequest
    --   { inputs :: Array V1.Input
    --   , invalidBefore :: DateTime
    --   , invalidHereafter :: DateTime
    --   , metadata :: Metadata
    --   , tags :: Tags
    --   , changeAddress :: Bech32
    --   , addresses :: Array Bech32
    --   , collateralUTxOs :: Array TxOutRef
    --   }
    req = PostTransactionsRequest
      { metadata: mempty
      , invalidBefore: toDateTime nowInstant
      , invalidHereafter: toDateTime inOneHourInstant
      -- , version :: MarloweVersion
      , inputs
      , tags: mempty -- TODO: use instead of metadata
      , changeAddress: changeAddress
      , addresses: usedAddresses
      , collateralUTxOs: []
      }
  post' serverUrl transactionsEndpoint req

submit
  :: CborHex TransactionWitnessSetObject
  -> ServerURL
  -> TransactionEndpoint
  -> Aff (Either FetchError Unit)
submit witnesses serverUrl transactionEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutTransactionRequest textEnvelope
  put' serverUrl transactionEndpoint req

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
