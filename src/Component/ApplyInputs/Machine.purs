module Component.ApplyInputs.Machine where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.InputHelper (ChoiceInput, DepositInput, NotifyInput, nextChoice, nextDeposit, nextNotify, nextTimeoutAdvance)
import Component.Types (WalletInfo(..))
import Contrib.Data.DateTime.Instant (millisecondsFromNow)
import Contrib.Fetch (FetchError)
import Control.Alternative as Alternative
import Control.Monad.Error.Class (catchError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Either (Either(..), isLeft)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Time.Duration (Milliseconds(..), fromDuration)
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

type WalletAddresses = { usedAddresses :: Array Bech32, changeAddress :: Bech32 }

type RequiredWalletContext = WalletAddresses

type AllInputsChoices = Either
  V1.Contract
  { deposits :: Maybe (NonEmptyArray DepositInput)
  , choices :: Maybe (NonEmptyArray ChoiceInput)
  , notify :: Maybe NotifyInput
  }

canDeposit :: AllInputsChoices -> Boolean
canDeposit (Right { deposits }) = isJust deposits
canDeposit _ = false

canChoose :: AllInputsChoices -> Boolean
canChoose (Right { choices }) = isJust choices
canChoose _ = false

canNotify :: AllInputsChoices -> Boolean
canNotify (Right { notify }) = isJust notify
canNotify _ = false

canAdvance :: AllInputsChoices -> Boolean
canAdvance possibleInputs = isLeft possibleInputs

data InputChoices
  = DepositInputs (NonEmptyArray DepositInput)
  | ChoiceInputs (NonEmptyArray ChoiceInput)
  | SpecificNotifyInput NotifyInput
  | AdvanceContract V1.Contract


newtype AutoRun = AutoRun Boolean

data State
  = PresentingContractDetails
      { marloweContext :: MarloweContext
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | FetchingRequiredWalletContext
      { autoRun :: AutoRun
      , marloweContext :: MarloweContext
      , transactionsEndpoint :: TransactionsEndpoint
      , errors :: Maybe String
      }
  | ChoosingInputType
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , requiredWalletContext :: RequiredWalletContext
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | PickingInput
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , requiredWalletContext :: RequiredWalletContext
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | CreatingTx
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , transactionsEndpoint :: TransactionsEndpoint
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , requiredWalletContext :: RequiredWalletContext
      }
  | SigningTx
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      }
  | SubmittingTx
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      }
  | InputApplied
      { autoRun :: AutoRun
      , allInputsChoices :: AllInputsChoices
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      }

autoRunFromState :: State -> Maybe AutoRun
autoRunFromState = case _ of
  PresentingContractDetails _ -> Nothing
  FetchingRequiredWalletContext { autoRun } -> Just autoRun
  ChoosingInputType { autoRun } -> Just autoRun
  PickingInput { autoRun } -> Just autoRun
  CreatingTx { autoRun } -> Just autoRun
  SigningTx { autoRun } -> Just autoRun
  SubmittingTx { autoRun } -> Just autoRun
  InputApplied { autoRun } -> Just autoRun

data Action
  = FetchRequiredWalletContext
      { autoRun :: AutoRun
      , marloweContext :: MarloweContext
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | FetchRequiredWalletContextFailed String
  | FetchRequiredWalletContextSucceeded
      { allInputsChoices :: AllInputsChoices
      , requiredWalletContext :: RequiredWalletContext
      }
  | ChooseInputType
  | ChooseInputTypeFailed String
  | ChooseInputTypeSucceeded InputChoices
  | PickInput
  | PickInputFailed String
  | PickInputSucceeded (Maybe V1.Input)
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
    PresentingContractDetails _ -> case action of
      FetchRequiredWalletContext { autoRun, marloweContext, transactionsEndpoint } -> FetchingRequiredWalletContext
        { autoRun
        , marloweContext
        , transactionsEndpoint
        , errors: Nothing
        }
      _ -> state
    FetchingRequiredWalletContext { errors: Just _ } -> case action of
      FetchRequiredWalletContext { autoRun, marloweContext, transactionsEndpoint } ->
        FetchingRequiredWalletContext $ { autoRun, marloweContext, transactionsEndpoint, errors: Nothing }
      _ -> state
    FetchingRequiredWalletContext r@{ autoRun, transactionsEndpoint } -> case action of
      FetchRequiredWalletContextFailed error -> FetchingRequiredWalletContext $ r { errors = Just error }
      FetchRequiredWalletContextSucceeded { requiredWalletContext, allInputsChoices } -> ChoosingInputType
        { autoRun
        , errors: Nothing
        , allInputsChoices
        , requiredWalletContext
        , transactionsEndpoint
        }
      _ -> state
    ChoosingInputType r@{ errors: Just _ } -> case action of
      ChooseInputType -> ChoosingInputType r { errors = Nothing }
      _ -> state
    ChoosingInputType r@{ allInputsChoices, autoRun, requiredWalletContext, transactionsEndpoint } -> case action of
      ChooseInputTypeFailed error -> ChoosingInputType $ r { errors = Just error }
      ChooseInputTypeSucceeded inputChoices -> PickingInput
        { autoRun
        , errors: Nothing
        , allInputsChoices
        , inputChoices
        , requiredWalletContext
        , transactionsEndpoint
        }
      _ -> state
    PickingInput r@{ errors: Just _ } -> case action of
      PickInput -> PickingInput $ r { errors = Nothing }
      _ -> state
    PickingInput r@{ allInputsChoices, autoRun, inputChoices, requiredWalletContext, transactionsEndpoint } -> case action of
      PickInputFailed error -> PickingInput $ r { errors = Just error }
      PickInputSucceeded input -> CreatingTx
        { autoRun
        , errors: Nothing
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
    CreatingTx r@{ allInputsChoices, autoRun, inputChoices, input } -> case action of
      CreateTxFailed error -> CreatingTx $ r { errors = Just error }
      CreateTxSucceeded createTxResponse -> SigningTx
        { autoRun, errors: Nothing, allInputsChoices, inputChoices, input, createTxResponse }
      _ -> state
    SigningTx r@{ errors: Just _ } -> case action of
      SignTx -> SigningTx $ r { errors = Nothing }
      _ -> state
    SigningTx r@{ allInputsChoices, autoRun, inputChoices, input, createTxResponse } -> case action of
      SignTxFailed error -> SigningTx $ r { errors = Just error }
      SignTxSucceeded txWitnessSet -> SubmittingTx
        { autoRun, errors: Nothing, allInputsChoices, inputChoices, input, createTxResponse, txWitnessSet }
      _ -> state
    SubmittingTx r@{ errors: Just _ } -> case action of
      SubmitTx -> SubmittingTx $ r { errors = Nothing }
      _ -> state
    SubmittingTx r@{ allInputsChoices, autoRun, inputChoices, input, createTxResponse, txWitnessSet } -> case action of
      SubmitTxFailed error -> SubmittingTx $ r { errors = Just error }
      SubmitTxSucceeded ->
        InputApplied { allInputsChoices, autoRun, inputChoices, input, txWitnessSet, createTxResponse }
      _ -> state
    InputApplied _ -> state

type Env =
  { connectedWallet :: WalletInfo Wallet.Api
  , cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , runtime :: Runtime
  }

initialState :: MarloweContext -> TransactionsEndpoint -> State
initialState marloweContext transactionsEndpoint = PresentingContractDetails do
  { marloweContext, transactionsEndpoint }

type MarloweContext =
  { contract :: V1.Contract
  , initialContract :: V1.Contract
  , state :: V1.State
  }

data WalletRequest
  = FetchWalletContextRequest
      { marloweContext :: MarloweContext
      , cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
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
      , input :: Maybe V1.Input
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
nextRequest env state = do
  let
    { cardanoMultiplatformLib, connectedWallet: walletInfo, runtime } = env
    Runtime { serverURL } = runtime
    AutoRun autoRun = fromMaybe (AutoRun false) $ autoRunFromState state
  Alternative.guard autoRun
  case state of
    PresentingContractDetails { marloweContext } ->
      Just $ WalletRequest $ FetchWalletContextRequest { marloweContext, cardanoMultiplatformLib, walletInfo }
    FetchingRequiredWalletContext { marloweContext, errors: Nothing } ->
      Just $ WalletRequest $ FetchWalletContextRequest { marloweContext, cardanoMultiplatformLib, walletInfo }
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
    FetchWalletContextRequest { cardanoMultiplatformLib, marloweContext, walletInfo } -> do
      let
        WalletInfo { wallet } = walletInfo
      possibleWalletAddresses <- (Right <$> walletContext cardanoMultiplatformLib wallet) `catchError` (pure <<< Left)
      case possibleWalletAddresses of
        Left err -> pure $ FetchRequiredWalletContextFailed $ show err
        Right (WalletContext { changeAddress: Just changeAddress, usedAddresses }) -> liftEffect $ do
          invalidBefore <- millisecondsFromNow (Milliseconds (Int.toNumber $ (-10) * 60 * 1000))
          invalidHereafter <- millisecondsFromNow (Milliseconds (Int.toNumber $ 5 * 60 * 1000))
          let
            { contract, state } = marloweContext
            timeInterval = V1.TimeInterval invalidBefore invalidHereafter
            environment = V1.Environment { timeInterval }
            allInputsChoices = case nextTimeoutAdvance environment state contract of
              Just advanceContinuation -> Left advanceContinuation
              Nothing -> do
                let
                  deposits = NonEmpty.fromArray $ nextDeposit environment state contract
                  choices = NonEmpty.fromArray $ nextChoice environment state contract
                  notify = NonEmpty.head <$> NonEmpty.fromArray (nextNotify environment state contract)
                Right { deposits, choices, notify }

          pure $ FetchRequiredWalletContextSucceeded
            { requiredWalletContext: { changeAddress, usedAddresses }
            , allInputsChoices
            }
        Right (WalletContext { changeAddress: Nothing }) -> pure $ FetchRequiredWalletContextFailed "Wallet does not have a change address"
    SignTxRequest { walletInfo, tx } -> do
      let
        WalletInfo { wallet } = walletInfo
      sign wallet tx >>= case _ of
        Left err -> pure $ SignTxFailed $ unsafeStringify err
        Right txWitnessSet -> pure $ SignTxSucceeded txWitnessSet
  RuntimeRequest runtimeRequest -> case runtimeRequest of
    CreateTxRequest { input, requiredWalletContext, serverURL, transactionsEndpoint } -> do
      let
        inputs = foldMap Array.singleton input
      create inputs requiredWalletContext serverURL transactionsEndpoint >>= case _ of
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
    oneMinuteAgo = fromDuration $ Time.Duration.Minutes (-1.0)
    inOneHourInstant = case instant (nowPosixMilliseconds <> oneHour) of
      Just instant' -> instant'
      Nothing -> unsafeCrashWith "Component.ApplyInputs.Machine: Failed to subtract one minute to the current time"
    beforeOneHourInstant = case instant (nowPosixMilliseconds <> oneMinuteAgo) of
      Just instant' -> instant'
      Nothing -> unsafeCrashWith "Component.ApplyInputs.Machine: Failed to add one hour to the current time"
  let
    { changeAddress, usedAddresses } = walletAddresses

    req = PostTransactionsRequest
      { metadata: mempty
      , invalidBefore: toDateTime beforeOneHourInstant
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
  Wallet.signTx walletApi txCborHex true
