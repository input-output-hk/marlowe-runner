module Component.CreateContract where

import Prelude

import CardanoMultiplatformLib (bech32FromString, bech32ToString)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (Bech32)
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.CreateContract.Machine as Machine
import Component.Types (MkComponentM, WalletInfo, ContractJsonString(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widgets (OutlineColoring(..), backToContractListLink, buttonOutlinedClassNames)
import Component.Widgets (submitButton) as DOM
import Component.Widgets as Widgets
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.Polyform.FormSpecBuilder (FormSpecBuilderT)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecBuilder as StatelessFormSpecBuilder
import Contrib.Polyform.FormSpecs.StatelessFormSpec (StatelessFormSpec)
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatlessFormSpec
import Contrib.React.Basic.Hooks.UseMooreMachine (MooreMachineSpec, useMooreMachine)
import Contrib.React.MarloweGraph (marloweGraph)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (FieldLayout(..), LabelSpacing(..), StatelessBootstrapFormSpec)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Control.Error.Util (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (decodeJson, encodeJson, jsonParser, parseJson, stringifyWithIndent)
import Data.Argonaut as A
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
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Client (ClientError)
import Marlowe.Runtime.Web.Types (PostContractsError, PostContractsResponseContent(..), RoleTokenConfig(..), RolesConfig(..), Tags(..))
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Validator (liftFn)
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import React.Basic (fragment) as DOOM
import React.Basic.DOM (div_, hr, img, input, text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated (button, div, h3, label, p, span) as DOM
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
  , walletContext :: WalletContext
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

fullWidthLayout :: FieldLayout
fullWidthLayout = MultiColumn { sm: Col12Label, md: Col12Label, lg: Col12Label }

mkContractFormSpec :: (Bech32 /\ Maybe ContractJsonString /\ AutoRun) -> LabeledFormSpec Effect Query Result
mkContractFormSpec (changeAddress /\ possibleInitialContract /\ (AutoRun _)) = FormSpecBuilder.evalBuilder Nothing $ do
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
          json <- lmap (\_ -> [ "Invalid JSON value" ]) $ parseJson jsonString
          -- lmap (Array.singleton <<< show) (decodeJson json)
          lmap (\_ -> [ "Invalid Marlowe contract JSON" ]) (decodeJson json)
      , rows: 15
      , name: Just contractFieldId
      , inputExtraClassName: NoProblem.opt "font-monospace"
      , role: NoProblem.opt "textarea"
      , "aria-label": NoProblem.opt "contract-input"
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
            ( Tags $ Map.fromFoldable
                [ runnerTag /\ (encodeJson $ map (encodeJson <<< trim) $ split (Pattern ",") tags)
                , runnerCreatorTag changeAddress /\ A.jsonNull
                ]
            )
      , name: Just tagFieldId
      }

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

machineProps
  :: AutoRun
  -> WalletInfo Wallet.Api
  -> CardanoMultiplatformLib.Lib
  -> (Machine.State -> Machine.State -> Effect Unit)
  -> Runtime
  -> MooreMachineSpec Machine.State Machine.Action Machine.State
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
                      [ R.text "Mint and send tokens"
                      , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                      ]
                  ]
              , backToContractListLink onDismiss
              ]
          ]
      wrappedContentWithFooter formBody formActions

runnerTag :: String
runnerTag = "run-lite"

runnerCreatorTag :: Bech32 -> String
runnerCreatorTag creator = runnerTag <> "-" <> bech32ToString creator

-- We want to construct `ContractInfo.ContractCreated` and call `onSuccess` only
mkOnStateTransition
  :: (ContractInfo.ContractCreated -> Effect Unit)
  -> (String -> Effect Unit)
  -> Machine.State
  -> Machine.State
  -> Effect Unit
mkOnStateTransition onSuccess _ _ (Machine.ContractCreated { contract, createTxResponse, submittedAt, tags }) = do
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
mkOnStateTransition _ onErrors _ next = do
  void $ for (Machine.stateErrors next) onErrors

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx
  logger <- asks _.logger

  let
    initialAutoRun = AutoRun true

  roleTokenComponent <- mkRoleTokensComponent
  loadFileHiddenInputComponent <- mkLoadFileHiddenInputComponent

  liftEffect $ component "CreateContract" \{ walletContext, connectedWallet, onSuccess, onError, onDismiss, possibleInitialContract } -> React.do
    let
      onStateTransition = mkOnStateTransition onSuccess onError

    _ /\ setCurrentRun <- React.useState' Nothing
    { state: submissionState, applyAction, reset: resetStateMachine } <- do
      let
        props = machineProps initialAutoRun connectedWallet cardanoMultiplatformLib onStateTransition runtime
      useMooreMachine props

    let
      WalletContext { changeAddress } = walletContext

    formSpec <- React.useMemo unit \_ -> mkContractFormSpec (changeAddress /\ possibleInitialContract /\ initialAutoRun)

    let
      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right (contract /\ tags /\ autoRun)) /\ _) -> do
          let
            props = machineProps autoRun connectedWallet cardanoMultiplatformLib onStateTransition runtime
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
                [ DOM.label { htmlFor: inputId, className: buttonOutlinedClassNames OutlinePrimaryColoring "", role: "button" }
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

                        Nothing -> logger "No file"
                    , id: inputId
                    , accept: "application/json"
                    }
                ]
        , lookupSubform contractFieldId
        , DOOM.hr {}
        , lookupSubform tagFieldId
        ]
      formActions = fragment
        [ DOM.div { className: "row mt-5" }
            [ DOM.div { className: "col-12" } do
                let
                  disabled = case result of
                    Just (V (Right _) /\ _) -> false
                    _ -> true
                DOM.submitButton
                  { onClick: onSubmit'
                  , disabled
                  }
                  $ fragment
                      [ R.text "Submit contract"
                      , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                      ]
            , backToContractListLink onDismiss
            ]
        ]

      contractFormContent useSpinner = do
        let
          title = stateToTitle submissionState
          description = DOM.div { className: "pe-3 mb-3" }
            [ DOM.p {} $ DOOM.text
                "Review your contract details before setting the terms to run it. Check the code and all details, this is your last chance to correct any errors before the contract is permanently live."
            ]
          formContent =
            [ DOM.div { className: "ps-3" }
                [ formBody
                , formActions
                ]
            ]

          content =
            Widgets.renderOverlay { active: useSpinner, top: -2 } $ Array.singleton $
              tabs { fill: false, justify: false, defaultActiveKey: "code", variant: Tabs.variant.pills } do
                [ tab
                    { eventKey: eventKey "graph"
                    , title: DOOM.text " Source graph"
                    , disabled: case result of
                        Just (V (Right _) /\ _) -> false
                        _ -> true
                    }
                    $ DOM.div { className: "w-100 mt-4 h-vh50 border border-1 rounded p-1" }
                    $ case result of
                        Just (V (Right (contract /\ _ /\ _)) /\ _) -> do
                          [ marloweGraph { contract: contract } ]
                        _ ->
                          [ DOM.div { className: "text-center" } $ DOOM.text "Please fill in the form and submit to see the source graph" ]
                , tab
                    { eventKey: eventKey "code"
                    , disabled: false
                    , title: DOOM.text "Code"
                    }
                    $ DOM.div { className: "w-100 mt-4" }
                    $ formContent
                ]
        BodyLayout.component
          { title
          , description
          , content
          }

    pure $ case submissionState of
      Machine.DefiningContract -> contractFormContent false
      Machine.DefiningRoleTokens { roleNames } -> do
        let
          onSuccess' :: RolesConfig -> Effect Unit
          onSuccess' rolesConfig =
            let
              action = Machine.DefineRoleTokensSucceeded rolesConfig
            in
              applyAction action

        BodyLayout.component
          { title: DOM.div { className: "px-3 mx-3 fw-bold" }
              [ DOOM.img { src: "/images/twotone_wallet.svg" }
              , DOM.h3 { className: "fw-bold" } $ DOOM.text "Mint and send role tokens"
              ]
          , description: DOM.div { className: "px-3 mx-3" }
              [ DOM.p {} [ DOOM.text "This contract uses roles represented as NFTs. They are minted during the creation step and sent to participants. Please provide the addresses of the participants who should receive the role tokens." ]
              ]
          , content: roleTokenComponent
              { onDismiss
              , onSuccess: onSuccess'
              , connectedWallet
              , roleNames
              }
          }
      _ -> contractFormContent true

stateToTitle :: Machine.State -> JSX
stateToTitle state = case state of
  _ -> DOM.div {}
    [ DOM.div { className: "mb-3" } $ DOOM.img { src: "/images/magnifying_glass.svg" }
    , DOM.span { className: "mb-3" } $ DOOM.text "Create and submit your contract"
    ]

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
