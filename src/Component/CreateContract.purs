module Component.CreateContract where

import Prelude

import CardanoMultiplatformLib (bech32FromString, bech32ToString)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (Bech32)
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.CreateContract.Machine as Machine
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM, WalletInfo)
import Component.Widgets (link, spinner)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.React.Basic.Hooks.UseMooreMachine (useMooreMachine)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (StatelessBootstrapFormSpec, booleanField)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringifyWithIndent)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..), split, trim)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError)
import Marlowe.Runtime.Web.Types (ContractEndpoint, Metadata(..), PostContractsError, RoleTokenConfig(..), RolesConfig(..), Tags(..))
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Validator (liftFn)
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import React.Basic (fragment) as DOOM
import React.Basic.DOM as DOOM
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, Ref, component, fragment, readRef, useRef, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import Wallet as Wallet
import WalletContext (WalletContext(..))
import Web.DOM.Node (Node)
import Web.File.File (File)
import Web.File.FileList (FileList)
import Web.File.FileList as FileList
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement

type Props =
  { onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

newtype AutoRun = AutoRun Boolean

type Result = V1.Contract /\ Tags /\ AutoRun

contractFieldId :: FieldId
contractFieldId = FieldId "contract-json"

mkContractFormSpec :: (Maybe V1.Contract /\ AutoRun) -> StatelessBootstrapFormSpec Effect Query Result
mkContractFormSpec (possibleInitialContract /\ (AutoRun initialAutoRun)) = FormSpecBuilder.evalBuilder Nothing $ ado
  contract <- StatelessFormSpecBuilders.textArea
    { missingError: "Please provide contract terms JSON value"
    , initial: case possibleInitialContract of
        Nothing -> ""
        Just initialContract -> stringifyWithIndent 2 $ encodeJson initialContract
    , label: Just $ DOOM.text "Contract JSON"
    , touched: isJust possibleInitialContract
    , validator: requiredV' $ Validator.liftFnEither \jsonString -> do
        json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
        lmap (Array.singleton <<< show) (decodeJson json)
    , rows: 15
    , name: Just contractFieldId
    }

  tags <- StatelessFormSpecBuilders.textInput
    { helpText: Just $ DOOM.div_
        [ DOOM.text "Tags"
        ]
    , initial: ""
    , label: Just $ DOOM.text "Tags"
    , touched: false
    , validator: liftFn case _ of
        Nothing -> Tags Map.empty
        Just tags ->
          ( Tags $ Map.singleton runLiteTag
              (encodeJson $ map (encodeJson <<< trim) $ split (Pattern ",") tags)
          )
    }

  autoRun <- AutoRun <$> do
    -- FIXME: This should be documented I left this as an example of more hard core lifting of validator
    -- let
    --   toAutoRun = liftBuilderM $ pure $ liftValidator $ liftFnM \value -> do
    --       let
    --         value' = AutoRun value
    --       -- onAutoRunChange value'
    --       pure value'
    booleanField
      { label: DOOM.text "Auto run"
      , helpText: DOOM.text "Whether to run the contract creation process automatically"
      , initial: initialAutoRun
      }
  in
    contract /\ tags /\ autoRun

mkRolesConfigForm :: NonEmptyArray String -> CardanoMultiplatformLib.Lib -> StatelessBootstrapFormSpec Effect Query RolesConfig
mkRolesConfigForm roleNames cardanoMultiplatformLib = FormSpecBuilder.evalBuilder Nothing $ Mint <<< Map.fromFoldable <$> for roleNames \roleName -> ado
  address <- StatelessFormSpecBuilders.textInput
    { missingError: "Please provide an address for a role token"
    , helpText: Just $ DOOM.div_
        [ DOOM.text "Role token destination address"
        ]
    , initial: ""
    , label: Just $ DOOM.text roleName
    , touched: false
    , validator: requiredV' $ Validator.liftFnMMaybe (const $ pure [ "Invalid address" ]) \str -> do
        bech32FromString cardanoMultiplatformLib str
    }
  in (roleName /\ (RoleTokenSimple address))

type ClientError' = ClientError PostContractsError

foreign import _loadFile :: File -> Promise (Nullable String)

loadFile :: File -> Aff (Maybe String)
loadFile = map Nullable.toMaybe <<< Promise.toAff <<< _loadFile

hoistMaybe :: forall m a. Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT <<< pure

mkLoadFileButtonComponent :: MkComponentM ({ onFileload :: Maybe String -> Effect Unit } -> JSX)
mkLoadFileButtonComponent =
  liftEffect $ component "LoadFileButton" \{ onFileload } -> React.do
    {- Working example in raw HTML:
      <script>
      const onfile = () => {
        const fr = new FileReader()
        fr.onload = e => {
          console.log("hey ho")
          console.log(e.target.result)
        }
        fr.readAsText(document.getElementById("yo").files[0])
      }
      </script>
      <input id="yo" type="file" onchange="onfile()" />
    -}
    ref :: Ref (Nullable Node) <- useRef Nullable.null

    let
      onChange :: Effect Unit
      onChange = map (fromMaybe unit) $ runMaybeT do
        node :: Node <- MaybeT $ Nullable.toMaybe <$> readRef ref
        inputElement :: HTMLInputElement <- hoistMaybe $ HTMLInputElement.fromNode node
        files :: FileList <- MaybeT $ HTMLInputElement.files inputElement
        file :: File <- hoistMaybe $ FileList.item 0 files
        liftEffect $ launchAff_ $ (liftEffect <<< onFileload) =<< loadFile file

    pure $ DOOM.input { type: "file", onChange: handler_ onChange, ref }

machineProps (AutoRun autoRun) connectedWallet cardanoMultiplatformLib runtime = do
  let
    env = { connectedWallet, cardanoMultiplatformLib, runtime }
  { initialState: Machine.initialState
  , step: Machine.step
  , driver:
      if autoRun then Machine.driver env
      else const Nothing
  , output: identity
  }

data CurrentRun
  = Automatic
  -- This boolean indicates whether we are actually performing the request
  -- at the moment. This is useful to avoid double clicking and show throbber
  | Manual Boolean

type RoleProps =
  { onDismiss :: Effect Unit
  , onSuccess :: RolesConfig -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , roleNames :: NonEmptyArray String
  }

mkRoleTokensComponent :: MkComponentM (RoleProps -> JSX)
mkRoleTokensComponent = do
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  liftEffect $ component "RoleTokensAssignment" \{ onDismiss, onSuccess, roleNames } -> React.do
    let
      formSpec = mkRolesConfigForm roleNames cardanoMultiplatformLib

      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right roleAssignments) /\ _) -> onSuccess roleAssignments
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    pure do
      let
        fields = StatelessFormSpec.renderFormSpec formSpec formState
        formBody = DOM.div { className: "form-group" } fields
        formActions = DOOM.fragment
          [ DOM.div { className: "row" } $
              [ DOM.div { className: "col-6 text-start" } $
                  [ link
                      { label: DOOM.text "Cancel"
                      , onClick: onDismiss
                      , showBorders: true
                      , extraClassNames: "me-3"
                      }
                  ]
              , DOM.div { className: "col-6 text-end" } $
                  [ DOM.button
                      do
                        let
                          disabled = case result of
                            Just (V (Right _) /\ _) -> false
                            _ -> true
                        { className: "btn btn-primary"
                        , onClick: onSubmit'
                        , disabled
                        }
                      [ R.text "Ok" ]
                  ]
              ]
          ]
      wrappedContentWithFooter formBody formActions

runLiteTag :: String
runLiteTag = "run-lite"

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  let
    initialAutoRun = AutoRun true

  roleTokenComponent <- mkRoleTokensComponent

  liftEffect $ component "CreateContract" \{ connectedWallet, onSuccess, onDismiss } -> React.do
    currentRun /\ setCurrentRun <- React.useState' Nothing
    { state: submissionState, applyAction, reset: resetStateMachine } <- do
      let
        props = machineProps initialAutoRun connectedWallet cardanoMultiplatformLib runtime
      useMooreMachine props

    formSpec <- React.useMemo unit \_ -> mkContractFormSpec (Nothing /\ initialAutoRun)

    let
      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right (contract /\ tags /\ autoRun)) /\ _) -> do
          let
            props = machineProps autoRun connectedWallet cardanoMultiplatformLib runtime
          applyAction' <- resetStateMachine (Just props)
          case autoRun of
            AutoRun true -> do
              setCurrentRun $ Just $ Automatic
            AutoRun false -> do
              setCurrentRun $ Just $ Manual false
          applyAction' $ Machine.TriggerSubmission contract tags
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    possibleWalletInfo <- React.useContext walletInfoCtx
    React.useEffect (_.changeAddress <<< un WalletContext <<< snd <$> possibleWalletInfo) $ do
      case possibleWalletInfo of
        Just (_ /\ (WalletContext { changeAddress: Just changeAddress })) -> do
          { multiChoiceTest: initialContract } <- liftEffect $ mkInitialContracts changeAddress
          case Map.lookup contractFieldId formState.fields of
            Just { touched, onChange } -> do
              when (not $ un Disj touched) do
                onChange [ stringifyWithIndent 2 $ encodeJson initialContract ]
            Nothing -> pure unit
        _ -> pure unit
      pure (pure unit)

    pure $ case submissionState of
      Machine.DefiningContract -> do
        let
          fields = StatelessFormSpec.renderFormSpec formSpec formState
          formBody = DOM.div { className: "form-group" } fields
          formActions = fragment
            [ DOM.div { className: "row" } $
                [ DOM.div { className: "col-6 text-start" } $
                    [ link
                        { label: DOOM.text "Cancel"
                        , onClick: onDismiss
                        , showBorders: true
                        , extraClassNames: "me-3"
                        }
                    ]
                , DOM.div { className: "col-6 text-end" } $
                    [ DOM.button
                        do
                          let
                            disabled = case result of
                              Just (V (Right _) /\ _) -> false
                              _ -> true
                          { className: "btn btn-primary"
                          , onClick: onSubmit'
                          , disabled
                          }
                        [ R.text "Submit" ]
                    ]
                ]
            ]

        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: wrappedContentWithFooter
              formBody
              formActions
          }

      Machine.DefiningRoleTokens { roleNames } -> do
        let
          onSuccess' :: RolesConfig -> Effect Unit
          onSuccess' rolesConfig =
            let
              action = Machine.DefineRoleTokensSucceeded rolesConfig
            in
              applyAction action

        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: roleTokenComponent { onDismiss: pure unit, onSuccess: onSuccess', connectedWallet, roleNames }
          }
      Machine.ContractCreated { contract, createTxResponse } -> do
        let
          { links: { contract: contractEndpoint } } = createTxResponse
        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: wrappedContentWithFooter
              do marloweYaml contract
              do
                DOOM.fragment
                  [ DOM.div { className: "row" } $
                      [ DOM.div { className: "col-12 text-end" } $
                          [ DOM.button
                              { className: "btn btn-primary"
                              , onClick: handler_ (onSuccess contractEndpoint)
                              }
                              [ R.text "Done" ]
                          ]
                      ]
                  ]
          }

      machineState -> do
        let
          machineEnv = { connectedWallet, cardanoMultiplatformLib, runtime }
          possibleRequest = currentRun >>= case _ of
            Manual _ -> do
              Machine.driver machineEnv machineState
            _ -> Nothing

          body = fragment
            [ do
                let
                  StepIndex index = (machineStateToStepIndex machineState)
                if index < machineStepsCardinality then do
                  let
                    stepPercent = Int.ceil $ (Int.toNumber (index - 1) / Int.toNumber (machineStepsCardinality - 1)) * 100.0
                    style = css { width: show stepPercent <> "%" }
                  DOM.div { className: "progress mb-3" } $ do
                    DOOM.div { className: "progress-bar", style, children: [] }
                else mempty
            , case currentRun of
                Just (Manual true) -> do
                  DOM.div { className: "d-flex justify-content-center" } $ spinner Nothing
                _ -> mempty
            ]

          formActions = case possibleRequest of
            Nothing -> mempty
            Just request -> DOOM.fragment
              [ DOM.div { className: "row" } $
                  [ DOM.div { className: "col-6 text-start" } $
                      [ link
                          { label: DOOM.text "Cancel"
                          , onClick: onDismiss
                          , showBorders: true
                          , extraClassNames: "me-3"
                          }
                      ]
                  , DOM.div { className: "col-6 text-end" } $
                      [ DOM.button
                          { className: "btn btn-primary"
                          , disabled: case currentRun of
                              Just (Manual b) -> b
                              _ -> false
                          , onClick: handler_ do
                              setCurrentRun (Just $ Manual true)
                              launchAff_ do
                                action <- request
                                liftEffect $ do
                                  applyAction action
                                  setCurrentRun (Just $ Manual false)
                          }
                          [ R.text "Run" ]
                      ]
                  ]
              ]
        BodyLayout.component
          { title: stateToTitle submissionState
          , description: stateToDetailedDescription submissionState
          , content: wrappedContentWithFooter body formActions
          }

stateToTitle :: Machine.State -> String
stateToTitle state = case state of
  Machine.DefiningContract -> "Defining contract"
  Machine.DefiningRoleTokens {} -> "Defining role tokens"
  Machine.FetchingRequiredWalletContext {} -> "Fetching required wallet context"
  Machine.CreatingTx {} -> "Creating transaction"
  Machine.SigningTx {} -> "Signing transaction"
  Machine.SubmittigTx {} -> "Submitting transaction"
  Machine.ContractCreated {} -> "Contract created"

-- To display progress bar
newtype StepIndex = StepIndex Int

machineStepsCardinality :: Int
machineStepsCardinality = 7

machineStateToStepIndex :: Machine.State -> StepIndex
machineStateToStepIndex state = StepIndex $ case state of
  Machine.DefiningContract -> 1
  Machine.DefiningRoleTokens {} -> 2
  Machine.FetchingRequiredWalletContext {} -> 3
  Machine.CreatingTx {} -> 4
  Machine.SigningTx {} -> 5
  Machine.SubmittigTx {} -> 6
  Machine.ContractCreated {} -> 7

-- | We want to describe in details what kind of data we are gathering
-- | when we are performing a given transtition (state determines the next transition in our case)
-- | The output should be readable to the developer which should understand the whole flow.
-- | Let's use standard react-basic JSX functions like: DOM.div { className: "foo" } [ DOOM.text "bar" ]
stateToDetailedDescription :: Machine.State -> JSX
stateToDetailedDescription state = case state of
  Machine.DefiningContract -> DOOM.div_
    [ DOM.p {} $ DOOM.text "We are currently in the initial state, awaiting the user to initiate the contract creation process."
    , DOM.p {} $ DOOM.text "When we receive the correct contract value in JSON format, it will be utilized as part of the request to the Marlowe runtime"
    , DOM.p {} $ DOOM.text "As a user, you have two options for providing the contract:"
    , DOM.ul {}
        [ DOM.li {} $ DOOM.text "Enter a valid contract in JSON format in the input field to the right"
        , DOM.li {} $ DOOM.text "Upload a contract JSON file using the \"Upload\" button."
        ]
    , DOM.p { className: "h3 fw-bold py-3" } $ DOOM.text "How to Provide a Contract in JSON Format:"
    , DOM.p {}
        [ DOOM.text "Please provide a contract in a JSON format. To generate it, you can use a Marlowe library for your language of choice (for example, "
        , DOM.a { href: "https://github.com/input-output-hk/marlowe-ts-sdk", target: "_blank", className: "white-color" } [ DOOM.text " marlowe-ts-sdk" ]
        , DOOM.text "), or you can use the "
        , DOM.a { href: "https://play.marlowe.iohk.io/#/", target: "_blank", className: "white-color" } [ DOOM.text " Marlowe Playground" ]
        , DOOM.text " to generate it. After creating a contract in the simulator within the Marlowe Playground, you can use the \"Download JSON\" button to obtain the contract in JSON format. Once you have the JSON file, you can either enter it in the input field to the right or upload it using the \"Upload\" button."
        ]
    ]
  Machine.DefiningRoleTokens {} -> DOOM.div_
    [ DOM.p {} $ DOOM.text "NOT IMPLEMENTED YET"
    ]
  Machine.FetchingRequiredWalletContext { errors: Nothing } -> DOOM.div_
    [ DOM.p {}
        [ DOOM.text "We are currently fetching the required wallet context for creating the Marlowe Contract on chain." ]
    , DOM.p {}
        [ DOOM.text "The marlowe-runtime requires information about wallet addresses in order to select the appropriate UTxOs to pay for the initial transaction. To obtain the set of addresses from the wallet, we utilize the "
        , DOM.code {} [ DOOM.text "getUsedAddresses" ]
        , DOOM.text " method from CIP-30. The addresses are then re-encoded from the lower-level Cardano CBOR hex format into Bech32 format ("
        , DOM.code {} [ DOOM.text "addr_test..." ]
        , DOOM.text ")."
        ]
    , DOM.p {}
        [ DOOM.text "Please wait while we fetch the wallet context. This process may take a few moments." ]
    ]
  Machine.FetchingRequiredWalletContext { errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the provided wallet is lacking addresses or failed to execute the method:"
    , DOM.p {} $ DOOM.text error
    ]
  Machine.CreatingTx { errors: Nothing } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "Utilizing the Marlowe-runtime, this interface enables you to generate an initial transaction. The generated transaction needs to be signed using the wallet you've connected. By doing so, you are authorizing and verifying the transaction's intent and ensuring its secure execution."
    , DOM.p {} $ DOOM.text "Please review all the details carefully before proceeding with the transaction confirmation."
    ]
  Machine.CreatingTx { reqWalletContext, errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the marlowe-runtime failed to create the initial transaction:"
    , DOM.p {} $ DOOM.text error
    , DOM.p {} $ DOOM.text "The wallet context we used:"
    , DOM.p {} $ DOOM.text $ unsafeStringify reqWalletContext
    ]
  Machine.SigningTx { errors: Nothing } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "You are currently in the process of digitally signing your initial transaction. This step is critical in validating the transaction's authenticity, confirming that it has indeed originated from you. By signing, you are ensuring the transaction's integrity and non-repudiation."
    , DOM.p {} $ DOOM.text "Carefully review all details to confirm they are correct before finalizing your signature."
    ]
  Machine.SigningTx { errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the wallet failed to sign the initial transaction:"
    , DOM.p {} $ DOOM.text error
    ]
  Machine.SubmittigTx { errors: Nothing } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "You have now reached the transaction submission phase. Having signed your initial transaction, it's time to submit it into the system for processing. This step essentially sends the transaction to the network where it's queued for inclusion in the blockchain. Please ensure all details are correct. Once submitted, the transaction is irreversible and will be permanently recorded."
    , DOM.p {} $ DOOM.text "Your transaction journey is almost complete. Press 'Submit' when you are ready."
    ]
  Machine.SubmittigTx { errors: Just error } -> DOOM.div_
    [ DOM.p {} $ DOOM.text "It seems that the marlowe-runtime failed to submit the initial transaction:"
    , DOM.p {} $ DOOM.text error
    ]
  Machine.ContractCreated _ -> DOOM.div_
    [ DOM.p {} $ DOOM.text "Congratulations! Your contract has been successfully created and recorded on the blockchain. This marks the successful completion of your transaction, now encapsulated into a secure, immutable contract. From here, the contract's terms will govern the further actions and transactions. You may want to keep a record of the contract details for future reference. Remember, the blockchain's nature of immutability makes this contract permanent and transparent."
    , DOM.p {} $ DOOM.text "Thank you for using our platform, and feel free to create more contracts as needed."
    ]

-- | Let's use error information and other details of the state to describe the sitution.
-- | Let's use standard react-basic JSX functions like: DOM.div { className: "foo" } [ DOOM.text "bar" ]
stateToDescription :: Machine.State -> JSX
stateToDescription state = case state of
  Machine.DefiningContract -> DOOM.text "Please define your contract."
  Machine.DefiningRoleTokens { errors } -> case errors of
    Nothing -> DOOM.text "Defining role tokens."
    Just err -> DOOM.text $ "Defining role tokens failed: " <> err
  Machine.FetchingRequiredWalletContext { errors } -> case errors of
    Nothing -> DOOM.text "Fetching required wallet context."
    Just err -> DOOM.text $ "Fetching required wallet context failed: " <> err
  Machine.CreatingTx { errors } -> case errors of
    Nothing -> DOOM.text "Creating transaction."
    Just err -> DOOM.text $ "Creating transaction failed: " <> err
  Machine.SigningTx { errors } -> case errors of
    Nothing -> DOOM.text "Signing transaction."
    Just err -> DOOM.text $ "Signing transaction failed: " <> err
  Machine.SubmittigTx { errors } -> case errors of
    Nothing -> DOOM.text "Submitting transaction."
    Just err -> DOOM.text $ "Submitting transaction failed: " <> err
  Machine.ContractCreated {} -> DOOM.text "Contract created."

three :: BigInt
three = BigInt.fromInt 3

four :: BigInt
four = BigInt.fromInt 4

mkInitialContracts
  :: Bech32
  -> Effect
       { -- brianContract :: V1.Contract
         multiChoiceTest :: V1.Contract
       }
mkInitialContracts bech32 = do
  nowMilliseconds <- unInstant <$> now
  let
    timeout = case instant (nowMilliseconds <> Milliseconds (Int.toNumber $ 20 * 60 * 1000)) of
      Just i -> i
      Nothing -> unsafeCrashWith "Invalid instant"

  pure
    { -- brianContract: brianContract bech32
      multiChoiceTest: mkMultiChoiceTest bech32 timeout
    }

brianContract :: Bech32 -> V1.Contract
brianContract bech32 = do
  let
    address = bech32ToString bech32
    timeout = BigInt.fromString "1684937880000"
    possibleContract = decodeJson $
      encodeJson { "when": [ { "then": { "when": [ { "then": { "when": [ { "then": "close", "case": { "notify_if": true } } ], "timeout_continuation": "close", "timeout": timeout }, "case": { "for_choice": { "choice_owner": { "address": address }, "choice_name": "Release" }, "choose_between": [ { "to": 1, "from": 1 } ] } } ], "timeout_continuation": "close", "timeout": timeout }, "case": { "party": { "address": address }, "of_token": { "token_name": "", "currency_symbol": "" }, "into_account": { "address": address }, "deposits": 10000000 } } ], "timeout_continuation": "close", "timeout": timeout }
  case possibleContract of
    Left err -> unsafeCrashWith $ "Failed to decode contract: " <> show err
    Right contract -> contract

mkMultiChoiceTest :: Bech32 -> Instant -> V1.Contract
mkMultiChoiceTest bech32 timeout = do
  let
    address = bech32ToString bech32
  V1.When
    [ V1.Case
        ( V1.Choice
            ( V1.ChoiceId "Everything is alright"
                (V1.Address address)
            )
            [ (V1.Bound zero zero)
            ]
        )
        V1.Close
    , V1.Case
        ( V1.Choice
            (V1.ChoiceId "Report problem" (V1.Address address))
            [ (V1.Bound one one) ]
        )
        V1.Close
    , V1.Case
        ( V1.Choice
            (V1.ChoiceId "Choice between 1-3" (V1.Address address))
            [ (V1.Bound one three) ]
        )
        V1.Close
    , V1.Case
        ( V1.Choice
            (V1.ChoiceId "Choice between 1-4" (V1.Address address))
            [ (V1.Bound one four) ]
        )
        V1.Close
    ]
    timeout
    V1.Close
