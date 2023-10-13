module Component.ApplyInputs.Machine where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.InputHelper (ChoiceInput, DepositInput, NotifyInput, nextChoice, nextDeposit, nextNotify, nextTimeoutAdvance)
import Component.Types (WalletInfo(..))
import Contrib.Data.DateTime.Instant (millisecondsFromNow, unsafeInstant)
import Contrib.Fetch (FetchError)
import Control.Alt ((<|>))
import Control.Alternative as Alternative
import Control.Monad.Error.Class (catchError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime.Instant (Instant, toDateTime, unInstant)
import Data.Either (Either(..), isLeft)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (PostContractsError, PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), ResourceWithLinks, Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Wallet as Wallet
import WalletContext (WalletContext(..), walletContext)

type ClientError' = ClientError PostContractsError

type WalletAddresses = { usedAddresses :: Array Bech32, changeAddress :: Bech32 }

-- FIXME: wallet roles
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
  -- = PresentingContractDetails
  --     { marloweContext :: MarloweContext
  --     , transactionsEndpoint :: TransactionsEndpoint
  --     }
  = FetchingRequiredWalletContext
      { autoRun :: AutoRun
      , marloweContext :: MarloweContext
      , transactionsEndpoint :: TransactionsEndpoint
      , errors :: Maybe String
      }
  | ChoosingInputType
      { autoRun :: AutoRun
      , errors :: Maybe String
      , marloweContext :: MarloweContext
      , allInputsChoices :: AllInputsChoices
      , environment :: V1.Environment
      , requiredWalletContext :: RequiredWalletContext
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | PickingInput
      { autoRun :: AutoRun
      , errors :: Maybe String
      , marloweContext :: MarloweContext
      , allInputsChoices :: AllInputsChoices
      , environment :: V1.Environment
      , inputChoices :: InputChoices
      , requiredWalletContext :: RequiredWalletContext
      , transactionsEndpoint :: TransactionsEndpoint
      }
  | CreatingTx
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , environment :: V1.Environment
      , transactionsEndpoint :: TransactionsEndpoint
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , newMarloweContext :: MarloweContext
      , requiredWalletContext :: RequiredWalletContext
      }
  | SigningTx
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , environment :: V1.Environment
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , newMarloweContext :: MarloweContext
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      }
  | SubmittingTx
      { autoRun :: AutoRun
      , errors :: Maybe String
      , allInputsChoices :: AllInputsChoices
      , environment :: V1.Environment
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , newMarloweContext :: MarloweContext
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      }
  | InputApplied
      { autoRun :: AutoRun
      , allInputsChoices :: AllInputsChoices
      , environment :: V1.Environment
      , inputChoices :: InputChoices
      , input :: Maybe V1.Input
      , newMarloweContext :: MarloweContext
      , txWitnessSet :: CborHex TransactionWitnessSetObject
      , createTxResponse :: ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint)
      , submittedAt :: Instant
      }

stateInputChoices :: State -> Maybe InputChoices
stateInputChoices = case _ of
  FetchingRequiredWalletContext _ -> Nothing
  ChoosingInputType {} -> Nothing
  PickingInput { inputChoices } -> Just inputChoices
  CreatingTx { inputChoices } -> Just inputChoices
  SigningTx { inputChoices } -> Just inputChoices
  SubmittingTx { inputChoices } -> Just inputChoices
  InputApplied { inputChoices } -> Just inputChoices

stateErrors :: State -> Maybe String
stateErrors = case _ of
  FetchingRequiredWalletContext { errors } -> errors
  ChoosingInputType { errors } -> errors
  PickingInput { errors } -> errors
  CreatingTx { errors } -> errors
  SigningTx { errors } -> errors
  SubmittingTx { errors } -> errors
  InputApplied _ -> Nothing

stateEnvironment :: State -> Maybe V1.Environment
stateEnvironment = case _ of
  FetchingRequiredWalletContext _ -> Nothing
  ChoosingInputType {} -> Nothing
  PickingInput { environment } -> Just environment
  CreatingTx { environment } -> Just environment
  SigningTx { environment } -> Just environment
  SubmittingTx { environment } -> Just environment
  InputApplied { environment } -> Just environment

autoRunFromState :: State -> AutoRun
autoRunFromState = case _ of
  -- PresentingContractDetails _ -> Nothing
  FetchingRequiredWalletContext { autoRun } -> autoRun
  ChoosingInputType { autoRun } -> autoRun
  PickingInput { autoRun } -> autoRun
  CreatingTx { autoRun } -> autoRun
  SigningTx { autoRun } -> autoRun
  SubmittingTx { autoRun } -> autoRun
  InputApplied { autoRun } -> autoRun

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
      , environment :: V1.Environment
      }
  | ChooseInputType
  | ChooseInputTypeFailed String
  | ChooseInputTypeSucceeded InputChoices
  | PickInput
  | PickInputFailed String
  | PickInputSucceeded { input :: Maybe V1.Input, newMarloweContext :: MarloweContext }
  | CreateTx
  | CreateTxFailed String
  | CreateTxSucceeded (ResourceWithLinks PostTransactionsResponse (transaction :: TransactionEndpoint))
  | SignTx
  | SignTxFailed String
  | SignTxSucceeded (CborHex TransactionWitnessSetObject)
  | SubmitTx
  | SubmitTxFailed String
  | SubmitTxSucceeded Instant

step :: State -> Action -> State
step state action = do
  case state of
    -- PresentingContractDetails _ -> case action of
    --   FetchRequiredWalletContext { autoRun, marloweContext, transactionsEndpoint } -> FetchingRequiredWalletContext
    --     { autoRun
    --     , marloweContext
    --     , transactionsEndpoint
    --     , errors: Nothing
    --     }
    --   _ -> state
    FetchingRequiredWalletContext { errors: Just _ } -> case action of
      FetchRequiredWalletContext { autoRun, marloweContext, transactionsEndpoint } ->
        FetchingRequiredWalletContext $ { autoRun, marloweContext, transactionsEndpoint, errors: Nothing }
      _ -> state
    FetchingRequiredWalletContext r@{ autoRun, transactionsEndpoint, marloweContext } -> case action of
      FetchRequiredWalletContextFailed error -> FetchingRequiredWalletContext $ r { errors = Just error }
      FetchRequiredWalletContextSucceeded { requiredWalletContext, allInputsChoices, environment } -> case allInputsChoices of
        Left contract -> do
          let inputChoices = AdvanceContract contract
          PickingInput
            { autoRun
            , errors: Nothing
            , allInputsChoices
            , environment
            , inputChoices
            , marloweContext
            , requiredWalletContext
            , transactionsEndpoint
            }
        Right { deposits, choices, notify } -> do
          let
            inputTypesCount = do
              let
                countInputType :: forall a. Maybe a -> Int
                countInputType = maybe 0 (const 1)
              countInputType deposits + countInputType choices + countInputType notify

            possibleInputChoices :: Maybe InputChoices
            possibleInputChoices =
              (DepositInputs <$> deposits)
                <|> (ChoiceInputs <$> choices)
                <|> (SpecificNotifyInput <$> notify)

          case inputTypesCount, possibleInputChoices of
            1, Just inputChoices -> PickingInput
              { autoRun
              , errors: Nothing
              , allInputsChoices
              , environment
              , inputChoices
              , marloweContext
              , requiredWalletContext
              , transactionsEndpoint
              }
            _, _ -> ChoosingInputType
              { autoRun
              , errors: Nothing
              , allInputsChoices
              , environment
              , marloweContext
              , requiredWalletContext
              , transactionsEndpoint
              }
      _ -> state
    ChoosingInputType r@{ errors: Just _ } -> case action of
      ChooseInputType -> ChoosingInputType r { errors = Nothing }
      _ -> state
    ChoosingInputType r@{ allInputsChoices, environment, marloweContext, autoRun, requiredWalletContext, transactionsEndpoint } -> case action of
      ChooseInputTypeFailed error -> ChoosingInputType $ r { errors = Just error }
      ChooseInputTypeSucceeded inputChoices -> PickingInput
        { autoRun
        , errors: Nothing
        , allInputsChoices
        , environment
        , inputChoices
        , marloweContext
        , requiredWalletContext
        , transactionsEndpoint
        }
      _ -> state
    PickingInput r@{ errors: Just _ } -> case action of
      PickInput -> PickingInput $ r { errors = Nothing }
      _ -> state
    PickingInput r@{ allInputsChoices, environment, autoRun, inputChoices, requiredWalletContext, transactionsEndpoint } -> case action of
      PickInputFailed error -> PickingInput $ r { errors = Just error }
      PickInputSucceeded { input, newMarloweContext } -> CreatingTx
        { autoRun
        , errors: Nothing
        , allInputsChoices
        , newMarloweContext
        , environment
        , inputChoices
        , input
        , requiredWalletContext
        , transactionsEndpoint
        }
      _ -> state
    CreatingTx r@{ errors: Just _ } -> case action of
      CreateTx -> CreatingTx $ r { errors = Nothing }
      _ -> state
    CreatingTx r@{ allInputsChoices, newMarloweContext, environment, autoRun, inputChoices, input } -> case action of
      CreateTxFailed error -> CreatingTx $ r { errors = Just error }
      CreateTxSucceeded createTxResponse -> SigningTx
        { autoRun, errors: Nothing, allInputsChoices, newMarloweContext, environment, inputChoices, input, createTxResponse }
      _ -> state
    SigningTx r@{ errors: Just _ } -> case action of
      SignTx -> SigningTx $ r { errors = Nothing }
      _ -> state
    SigningTx r@{ allInputsChoices, environment, newMarloweContext, autoRun, inputChoices, input, createTxResponse } -> case action of
      SignTxFailed error -> SigningTx $ r { errors = Just error }
      SignTxSucceeded txWitnessSet -> SubmittingTx
        { autoRun, errors: Nothing, allInputsChoices, newMarloweContext, environment, inputChoices, input, createTxResponse, txWitnessSet }
      _ -> state
    SubmittingTx r@{ errors: Just _ } -> case action of
      SubmitTx -> SubmittingTx $ r { errors = Nothing }
      _ -> state
    SubmittingTx r@{ allInputsChoices, environment, newMarloweContext, autoRun, inputChoices, input, createTxResponse, txWitnessSet } -> case action of
      SubmitTxFailed error -> SubmittingTx $ r { errors = Just error }
      SubmitTxSucceeded submittedAt ->
        InputApplied { allInputsChoices, environment, newMarloweContext, autoRun, inputChoices, input, txWitnessSet, createTxResponse, submittedAt }
      _ -> state
    InputApplied _ -> state

type Env =
  { connectedWallet :: WalletInfo Wallet.Api
  , cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , runtime :: Runtime
  }

initialState :: MarloweContext -> TransactionsEndpoint -> AutoRun -> State
initialState marloweContext transactionsEndpoint autoRun = FetchingRequiredWalletContext do
  { autoRun, marloweContext, transactionsEndpoint, errors: Nothing }

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
      , environment :: V1.Environment
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
    AutoRun autoRun = autoRunFromState state
  Alternative.guard autoRun
  case state of
    -- PresentingContractDetails { marloweContext } ->
    --   Just $ WalletRequest $ FetchWalletContextRequest { marloweContext, cardanoMultiplatformLib, walletInfo }
    FetchingRequiredWalletContext { marloweContext, errors: Nothing } ->
      Just $ WalletRequest $ FetchWalletContextRequest { marloweContext, cardanoMultiplatformLib, walletInfo }
    CreatingTx { errors: Nothing, inputChoices, input, allInputsChoices, environment, requiredWalletContext, transactionsEndpoint } -> do
      Just $ RuntimeRequest $ CreateTxRequest
        { allInputsChoices
        , environment
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

nextTimeout :: V1.Contract -> Maybe V1.Timeout
nextTimeout = case _ of
  V1.When _ timeout _ -> Just timeout
  _ -> Nothing

-- This is pretty arbitrary choice - we should keep track which inputs are relevant
-- during the further steps.
mkEnvironment :: V1.Contract -> Effect V1.Environment
mkEnvironment contract = do
  n <- now
  inTenMinutes <- millisecondsFromNow (Milliseconds (Int.toNumber $ 10 * 60 * 1000))
  twoMinutesAgo <- millisecondsFromNow (Milliseconds (Int.toNumber $ -2 * 60 * 1000))
  let
    timeInterval = case nextTimeout contract of
      Just timeout | n < timeout -> do
        let
          timeout' = unsafeInstant $ unInstant timeout <> (Milliseconds (-1.0))
        V1.TimeInterval twoMinutesAgo (min inTenMinutes timeout')
      _ -> V1.TimeInterval twoMinutesAgo inTenMinutes
  let
    environment = V1.Environment { timeInterval }
  pure environment

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
        Right Nothing -> pure $ FetchRequiredWalletContextFailed "Wallet does not have a change address"
        Right (Just (WalletContext { changeAddress, usedAddresses })) -> liftEffect $ do
          -- TODO: investingate if this is good strategy. We should probably migrate to something similiar to the
          -- next endpoint implementation.
          environment <- mkEnvironment marloweContext.contract
          let
            { contract, state } = marloweContext
            allInputsChoices = case nextTimeoutAdvance environment state contract of
              Just advanceContinuation -> do
                Left advanceContinuation
              Nothing -> do
                let
                  deposits = NonEmpty.fromArray $ nextDeposit environment state contract
                  choices = NonEmpty.fromArray $ nextChoice environment state contract
                  notify = NonEmpty.head <$> NonEmpty.fromArray (nextNotify environment state contract)
                Right { deposits, choices, notify }

          pure $ FetchRequiredWalletContextSucceeded
            { requiredWalletContext: { changeAddress, usedAddresses }
            , allInputsChoices
            , environment
            }
    SignTxRequest { walletInfo, tx } -> do
      let
        WalletInfo { wallet } = walletInfo
        action = sign wallet tx >>= case _ of
          Left err -> pure $ SignTxFailed $ unsafeStringify err
          Right txWitnessSet -> pure $ SignTxSucceeded txWitnessSet
      action `catchError` (pure <<< SignTxFailed <<< show)
  RuntimeRequest runtimeRequest -> case runtimeRequest of
    CreateTxRequest { input, environment, requiredWalletContext, serverURL, transactionsEndpoint } -> do
      let
        inputs = foldMap Array.singleton input
        action = create inputs environment requiredWalletContext serverURL transactionsEndpoint >>= case _ of
          Right res -> pure $ CreateTxSucceeded res
          Left err -> pure $ CreateTxFailed $ show err
      action `catchError` (pure <<< CreateTxFailed <<< show)
    SubmitTxRequest { txWitnessSet, createTxResponse, serverURL } -> do
      let
        action = submit txWitnessSet serverURL createTxResponse.links.transaction >>= case _ of
          Right _ -> do
            n <- liftEffect $ now
            pure $ SubmitTxSucceeded n
          Left err -> pure $ SubmitTxFailed $ show err
      action `catchError` (pure <<< SubmitTxFailed <<< show)

driver :: Env -> State -> Maybe (Aff Action)
driver env state = do
  request <- nextRequest env state
  pure $ requestToAffAction request

-- Lower level helpers
create
  :: Array V1.Input
  -> V1.Environment
  -> WalletAddresses
  -> ServerURL
  -> TransactionsEndpoint
  -> Aff (Either ClientError' { resource :: PostTransactionsResponse, links :: { transaction :: TransactionEndpoint } })
create inputs environment walletAddresses serverUrl transactionsEndpoint = do
  let
    V1.Environment { timeInterval } = environment
    V1.TimeInterval invalidBefore invalidHereafter = timeInterval
    { changeAddress, usedAddresses } = walletAddresses

    req = PostTransactionsRequest
      { metadata: mempty
      , invalidBefore: toDateTime invalidBefore
      , invalidHereafter: toDateTime invalidHereafter
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
