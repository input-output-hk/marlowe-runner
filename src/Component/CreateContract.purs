module Component.CreateContract where

import Prelude

import CardanoMultiplatformLib (bech32FromString, bech32ToString)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (Bech32)
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.CreateContract.Machine as Machine
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM, WalletInfo, ContractJsonString(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widgets (OutlineColoring(..), SpinnerOverlayHeight(..), buttonOutlinedClassNames, link, spinnerOverlay)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.Polyform.FormSpecBuilder (FormSpecBuilderT)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecBuilder as StatelessFormSpecBuilder
import Contrib.Polyform.FormSpecs.StatelessFormSpec (StatelessFormSpec)
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatlessFormSpec
import Contrib.React.Basic.Hooks.UseMooreMachine (useMooreMachine)
import Contrib.React.MarloweGraph (marloweGraph)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (FieldLayout(..), LabelSpacing(..), StatelessBootstrapFormSpec)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Control.Error.Util (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (decodeJson, encodeJson, jsonParser, parseJson, stringifyWithIndent)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..), hush)
import Data.Foldable as Foldable
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Identity (Identity)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid as Monoid
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..), split, trim)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError)
import Marlowe.Runtime.Web.Types (PostContractsError, PostContractsResponseContent(..), RoleTokenConfig(..), RolesConfig(..), Tags(..))
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Validator (liftFn)
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import React.Basic (fragment) as DOOM
import React.Basic.DOM (div_, hr, img, input, span_, text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, Ref, component, fragment, readRef, useRef, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)
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
  , onError :: String -> Effect Unit
  , onSuccess :: ContractInfo.ContractCreated -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , possibleInitialContract :: Maybe ContractJsonString
  }

newtype AutoRun = AutoRun Boolean

type Result = V1.Contract /\ Tags /\ AutoRun

contractFieldId :: FieldId
contractFieldId = FieldId "contract-json"

tagFieldId :: FieldId
tagFieldId = FieldId "tags"

autoRunFieldId :: FieldId
autoRunFieldId = FieldId "auto-run"

type LabeledFormSpec validatorM = StatelessFormSpec validatorM (Array (FieldId /\ JSX)) String

fullWidthLayout = MultiColumn { sm: Col12Label, md: Col12Label, lg: Col12Label }

contractSection contract state =
  tabs { fill: false, justify: false, defaultActiveKey: "source", variant: Tabs.variant.pills } do
    let
      renderTab props children = tab props $ DOM.div { className: "pt-4 w-100 h-vh50 overflow-auto" } children
    [ renderTab
        { eventKey: eventKey "graph"
        , disabled: true
        , title: DOOM.span_
            -- [ Icons.toJSX $ unsafeIcon "diagram-2"
            [ DOOM.text " Source graph"
            ]
        }
        [ marloweGraph { contract: contract } ]
    , renderTab
        { eventKey: eventKey "source"
        , disabled: false
        , title: DOOM.span_
            -- [ Icons.toJSX $ unsafeIcon "filetype-yml"
            [ DOOM.text " Source code"
            ]
        }
        [ marloweYaml contract ]
    ]

mkContractFormSpec :: (Maybe ContractJsonString /\ AutoRun) -> LabeledFormSpec Effect Query Result
mkContractFormSpec (possibleInitialContract /\ (AutoRun initialAutoRun)) = FormSpecBuilder.evalBuilder Nothing $ do
  let
    -- We put subforms JSX into a Map so we can control rendering order etc.
    labelSubform
      :: forall a
       . FieldId
      -> FormSpecBuilderT Identity (StatelessBootstrapFormSpec Effect) Query a
      -> FormSpecBuilderT Identity (LabeledFormSpec Effect) Query a
    labelSubform name formSpecBuilder = do
      let
        label :: forall err i m. StatelessFormSpec m (Array JSX) err i ~> StatelessFormSpec m (Array (FieldId /\ JSX)) err i
        label = StatlessFormSpec.mapRender $ map \subformJSX ->
          name /\ subformJSX
      StatelessFormSpecBuilder.hoistFormSpec label formSpecBuilder
  ado
    contract <- labelSubform contractFieldId $ StatelessFormSpecBuilders.textArea
      { missingError: "Please provide contract terms JSON value"
      , initial: case possibleInitialContract of
          Nothing -> ""
          Just (ContractJsonString initialContract) -> formatPossibleJSON initialContract
      , label: mempty
      , layout: fullWidthLayout
      , touched: isJust possibleInitialContract
      , validator: requiredV' $ Validator.liftFnEither \jsonString -> do
          json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
          lmap (Array.singleton <<< show) (decodeJson json)
      , rows: 15
      , name: Just contractFieldId
      , inputExtraClassName: NoProblem.opt "font-monospace"
      }

    tags <- labelSubform tagFieldId $ StatelessFormSpecBuilders.textInput
      { layout: MultiColumn { sm: Col12Label, md: Col12Label, lg: Col12Label }
      , initial: ""
      , label: Just $ DOOM.text "Add tags"
      , placeholder: "tag1, tag2, tag3"
      , touched: false
      , validator: liftFn case _ of
          Nothing -> Tags Map.empty
          Just tags ->
            ( Tags $ Map.singleton runLiteTag
                (encodeJson $ map (encodeJson <<< trim) $ split (Pattern ",") tags)
            )
      , name: Just tagFieldId
      }

    -- autoRun <- map AutoRun $ labelSubform autoRunFieldId $ booleanField
    --   { label: DOOM.text ""
    --   , helpText: DOOM.text "Auto-run contract"
    --   , initial: initialAutoRun
    --   , name: autoRunFieldId
    --   }
    in
      contract /\ tags /\ (AutoRun true)

mkRolesConfigForm :: NonEmptyArray String -> CardanoMultiplatformLib.Lib -> StatelessBootstrapFormSpec Effect Query RolesConfig
mkRolesConfigForm roleNames cardanoMultiplatformLib = FormSpecBuilder.evalBuilder Nothing $ Mint <<< Map.fromFoldable <$> for (NonEmptyArray.sort roleNames) \roleName -> ado
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

formatPossibleJSON :: String -> String
formatPossibleJSON str = fromMaybe str do
  json <- hush $ jsonParser str
  pure $ stringifyWithIndent 2 $ encodeJson json

mkLoadFileHiddenInputComponent :: MkComponentM ({ onFileload :: Maybe String -> Effect Unit, id :: String, accept :: String } -> JSX)
mkLoadFileHiddenInputComponent =
  liftEffect $ component "LoadFileButton" \{ id, onFileload, accept } -> React.do
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
      loadFile :: File -> Aff (Maybe String)
      loadFile = map Nullable.toMaybe <<< Promise.toAff <<< _loadFile

      onChange :: Effect Unit
      onChange = map (fromMaybe unit) $ runMaybeT do
        node :: Node <- MaybeT $ Nullable.toMaybe <$> readRef ref
        inputElement :: HTMLInputElement <- hoistMaybe $ HTMLInputElement.fromNode node
        files :: FileList <- MaybeT $ HTMLInputElement.files inputElement
        file :: File <- hoistMaybe $ FileList.item 0 files
        liftEffect $ launchAff_ $ (liftEffect <<< onFileload) =<< loadFile file

    pure $ DOOM.input
      { type: "file", accept, id, onChange: handler_ onChange, ref, className: "d-none" }

machineProps (AutoRun autoRun) connectedWallet cardanoMultiplatformLib onStateTransition runtime = do
  let
    env = { connectedWallet, cardanoMultiplatformLib, runtime }
  { initialState: Machine.initialState
  , onStateTransition
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
              [ DOM.div { className: "col-6 text-end" } $
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
              , backToContractListLink onDismiss
              ]
          ]
      wrappedContentWithFooter formBody formActions

runLiteTag :: String
runLiteTag = "run-lite"

backToContractListLink :: Effect Unit -> JSX
backToContractListLink onDismiss = do
  DOM.div { className: "col-12 text-center" } $
    [ link
        { label: DOM.b {} [ DOOM.text "Back to contract list" ]
        , onClick: onDismiss
        , showBorders: false
        , extraClassNames: "mt-3"
        }
    ]

-- We want to construct `ContractInfo.ContractCreated` and call `onSuccess` only
onStateTransition onSuccess _ prevState (Machine.ContractCreated { contract, createTxResponse, submittedAt, tags }) = do
  let
    { resource: PostContractsResponseContent { contractId }
    , links: { contract: contractEndpoint }
    } = createTxResponse

    contractCreated = ContractInfo.ContractCreated
      { contract
      , contractEndpoint
      , contractId
      , submittedAt
      , tags
      }
  onSuccess contractCreated
-- FIXME: paluh: handle error reporting
onStateTransition _ onErrors prev next = do
  void $ for (Machine.stateErrors next) onErrors

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  let
    initialAutoRun = AutoRun true

  roleTokenComponent <- mkRoleTokensComponent
  loadFileHiddenInputComponent <- mkLoadFileHiddenInputComponent

  liftEffect $ component "CreateContract" \{ connectedWallet, onSuccess, onError, onDismiss, possibleInitialContract } -> React.do
    let
      onStateTransition' = onStateTransition onSuccess onError

    currentRun /\ setCurrentRun <- React.useState' Nothing
    { state: submissionState, applyAction, reset: resetStateMachine } <- do
      let
        props = machineProps initialAutoRun connectedWallet cardanoMultiplatformLib onStateTransition' runtime
      useMooreMachine props

    formSpec <- React.useMemo unit \_ -> mkContractFormSpec (possibleInitialContract /\ initialAutoRun)

    let
      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right (contract /\ tags /\ autoRun)) /\ _) -> do
          let
            props = machineProps autoRun connectedWallet cardanoMultiplatformLib onStateTransition' runtime
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
        Just (_ /\ (WalletContext { changeAddress })) -> do
          { multiChoiceTest: initialContract } <- liftEffect $ mkInitialContracts changeAddress
          case Map.lookup contractFieldId formState.fields of
            Just { touched, onChange } -> do
              when (not $ un Disj touched) do
                onChange [ stringifyWithIndent 2 $ encodeJson initialContract ]
            Nothing -> pure unit
        _ -> pure unit
      pure (pure unit)

    let
      fields = StatelessFormSpec.renderFormSpec formSpec formState
      lookupSubform fieldId = fromMaybe mempty do
        Foldable.lookup fieldId fields

      formBody = DOM.div { className: "form-group" }
        [ DOM.div { className: "row mb-4" }
            $ DOM.div { className: "col-12 text-end" }
            $ do
                let inputId = "contract-file-upload"
                [ DOM.label { htmlFor: inputId, className: buttonOutlinedClassNames OutlinePrimaryColoring "" }
                    [ R.text "Upload JSON" ]
                , loadFileHiddenInputComponent
                    { onFileload: case _ of
                        Just str -> do
                          let
                            allFields = formState.fields
                          void $ for (Map.lookup contractFieldId allFields) \{ onChange } -> do
                            let
                              str' = formatPossibleJSON str
                            onChange [ str' ]

                        Nothing -> traceM "No file"
                    , id: inputId
                    , accept: "application/json"
                    }
                ]
        , lookupSubform contractFieldId
        , DOOM.hr {}
        , lookupSubform tagFieldId
        -- , lookupSubform autoRunFieldId
        ]
      formActions = fragment
        [ DOM.div { className: "row mt-5" } $
            [ DOM.div { className: "col-12" } $
                [ DOM.button
                    do
                      let
                        disabled = case result of
                          Just (V (Right _) /\ _) -> false
                          _ -> true
                      { className: "btn btn-primary w-100"
                      , onClick: onSubmit'
                      , disabled
                      }
                    [ R.text "Submit contract"
                    , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                    ]
                ]
            , backToContractListLink onDismiss
            ]
        ]

      content useSpinner = do
        let
          title = stateToTitle submissionState
          description = stateToDetailedDescription submissionState
          formContent =
            [ DOM.div { className: "ps-3" }
              [ formBody
              , formActions
              ]
            ] <> Monoid.guard useSpinner [ spinnerOverlay Spinner100VH ]
        -- Essentially this is a local copy of `BodyLayout.component` but
        -- we use `relative` positioning for the form content instead.
        -- Should we just use it there?
        DOM.div { className: "container" } $ do
          DOM.div { className: "row min-height-100vh d-flex flex-row align-items-stretch no-gutters" } $
            [ DOM.div { className: "pe-3 col-3 background-color-primary-light overflow-auto d-flex flex-column justify-content-center pb-3" } $
                [ DOM.div { className: "fw-bold font-size-2rem my-3" } $ title
                , DOM.div { className: "font-size-1rem" } $ description
                ]
            , DOM.div { className: "col-9 bg-white position-relative" } formContent
            ]

    pure $ case submissionState of
      Machine.DefiningContract -> content false
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
          , content: roleTokenComponent
              { onDismiss
              , onSuccess: onSuccess'
              , connectedWallet
              , roleNames
              }
          }
      _ -> content true
      -- machineState -> do
      --   let
      --     machineEnv = { connectedWallet, cardanoMultiplatformLib, runtime }
      --     possibleRequest = currentRun >>= case _ of
      --       Manual _ -> do
      --         Machine.driver machineEnv machineState
      --       _ -> Nothing

      --     body = fragment
      --       [ do
      --           let
      --             StepIndex index = (machineStateToStepIndex machineState)
      --           if index < machineStepsCardinality then do
      --             let
      --               stepPercent = Int.ceil $ (Int.toNumber (index - 1) / Int.toNumber (machineStepsCardinality - 1)) * 100.0
      --               style = css { width: show stepPercent <> "%" }
      --             DOM.div { className: "progress mb-3" } $ do
      --               DOOM.div { className: "progress-bar", style, children: [] }
      --           else mempty
      --       , case currentRun of
      --           Just (Manual true) -> do
      --             DOM.div { className: "d-flex justify-content-center" } $ spinner Nothing
      --           _ -> mempty
      --       ]

      --     formActions = case possibleRequest of
      --       Nothing -> mempty
      --       Just request -> DOOM.fragment
      --         [ DOM.div { className: "row" } $
      --             [ DOM.div { className: "col-12 text-end" } $
      --                 [ DOM.button
      --                     { className: "btn btn-primary"
      --                     , disabled: case currentRun of
      --                         Just (Manual b) -> b
      --                         _ -> false
      --                     , onClick: handler_ do
      --                         setCurrentRun (Just $ Manual true)
      --                         launchAff_ do
      --                           action <- request
      --                           liftEffect $ do
      --                             applyAction action
      --                             setCurrentRun (Just $ Manual false)
      --                     }
      --                     [ R.text "Run" ]
      --                 ]
      --             , backToContractListLink onDismiss
      --             ]
      --         ]
      --   BodyLayout.component
      --     { title: stateToTitle submissionState
      --     , description: stateToDetailedDescription submissionState
      --     , content: wrappedContentWithFooter body formActions
      --     }

stateToTitle :: Machine.State -> JSX
stateToTitle state = case state of
  _ -> DOM.div {}
    [ DOM.div { className: "mb-3" } $ DOOM.img { src: "/images/magnifying_glass.svg" }
    , DOM.span { className: "mb-3" } $ DOOM.text "Create and submit your contract"
    ]
  -- Machine.DefiningRoleTokens {} -> DOM.h3 {} $ DOOM.text "Defining role tokens"
  -- Machine.FetchingRequiredWalletContext {} -> DOM.h3 {} $ DOOM.text "Fetching required wallet context"
  -- Machine.CreatingTx {} -> DOM.h3 {} $ DOOM.text "Creating transaction"
  -- Machine.SigningTx {} -> DOM.h3 {} $ DOOM.text "Signing transaction"
  -- Machine.SubmittigTx {} -> DOM.h3 {} $ DOOM.text "Submitting transaction"
  -- Machine.ContractCreated {} -> DOM.h3 {} $ DOOM.text "Contract created"

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
  _ -> DOM.div { className: "pe-3 mb-3" }
    [ DOM.p {} $ DOOM.text "Review your contract details before setting the terms to run it. Check the code and all details, this is your last chance to correct any errors before the contract is permanently live."
    ]
  -- Machine.DefiningRoleTokens {} -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "NOT IMPLEMENTED YET"
  --   ]
  -- Machine.FetchingRequiredWalletContext { errors: Nothing } -> DOOM.div_
  --   [ DOM.p {}
  --       [ DOOM.text "We are currently fetching the required wallet context for creating the Marlowe Contract on chain." ]
  --   , DOM.p {}
  --       [ DOOM.text "The marlowe-runtime requires information about wallet addresses in order to select the appropriate UTxOs to pay for the initial transaction. To obtain the set of addresses from the wallet, we utilize the "
  --       , DOM.code {} [ DOOM.text "getUsedAddresses" ]
  --       , DOOM.text " method from CIP-30. The addresses are then re-encoded from the lower-level Cardano CBOR hex format into Bech32 format ("
  --       , DOM.code {} [ DOOM.text "addr_test..." ]
  --       , DOOM.text ")."
  --       ]
  --   , DOM.p {}
  --       [ DOOM.text "Please wait while we fetch the wallet context. This process may take a few moments." ]
  --   ]
  -- Machine.FetchingRequiredWalletContext { errors: Just error } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "It seems that the provided wallet is lacking addresses or failed to execute the method:"
  --   , DOM.p {} $ DOOM.text error
  --   ]
  -- Machine.CreatingTx { errors: Nothing } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "Utilizing the Marlowe-runtime, this interface enables you to generate an initial transaction. The generated transaction needs to be signed using the wallet you've connected. By doing so, you are authorizing and verifying the transaction's intent and ensuring its secure execution."
  --   , DOM.p {} $ DOOM.text "Please review all the details carefully before proceeding with the transaction confirmation."
  --   ]
  -- Machine.CreatingTx { reqWalletContext, errors: Just error } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "It seems that the marlowe-runtime failed to create the initial transaction:"
  --   , DOM.p {} $ DOOM.text error
  --   , DOM.p {} $ DOOM.text "The wallet context we used:"
  --   , DOM.p {} $ DOOM.text $ unsafeStringify reqWalletContext
  --   ]
  -- Machine.SigningTx { errors: Nothing } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "You are currently in the process of digitally signing your initial transaction. This step is critical in validating the transaction's authenticity, confirming that it has indeed originated from you. By signing, you are ensuring the transaction's integrity and non-repudiation."
  --   , DOM.p {} $ DOOM.text "Carefully review all details to confirm they are correct before finalizing your signature."
  --   ]
  -- Machine.SigningTx { errors: Just error } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "It seems that the wallet failed to sign the initial transaction:"
  --   , DOM.p {} $ DOOM.text error
  --   ]
  -- Machine.SubmittigTx { errors: Nothing } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "You have now reached the transaction submission phase. Having signed your initial transaction, it's time to submit it into the system for processing. This step essentially sends the transaction to the network where it's queued for inclusion in the blockchain. Please ensure all details are correct. Once submitted, the transaction is irreversible and will be permanently recorded."
  --   , DOM.p {} $ DOOM.text "Your transaction journey is almost complete. Press 'Submit' when you are ready."
  --   ]
  -- Machine.SubmittigTx { errors: Just error } -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "It seems that the marlowe-runtime failed to submit the initial transaction:"
  --   , DOM.p {} $ DOOM.text error
  --   ]
  -- Machine.ContractCreated _ -> DOOM.div_
  --   [ DOM.p {} $ DOOM.text "Congratulations! Your contract has been successfully created and recorded on the blockchain. This marks the successful completion of your transaction, now encapsulated into a secure, immutable contract. From here, the contract's terms will govern the further actions and transactions. You may want to keep a record of the contract details for future reference. Remember, the blockchain's nature of immutability makes this contract permanent and transparent."
  --   , DOM.p {} $ DOOM.text "Thank you for using our platform, and feel free to create more contracts as needed."
  --   ]

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
