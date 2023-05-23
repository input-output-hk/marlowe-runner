module Component.EventList where

import Prelude

import Actus.Domain (ContractTerms)
import Actus.Domain.BusinessEvents as Actus.BussinessEvents
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.Modal (mkModal)
import Component.Types (ActusContractRole(..), CashFlowInfo(..), ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, UserCashFlowDirection(..), UserContractRole(..), WalletInfo(..))
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (link, linkWithIcon)
import Component.Widgets.Form (mkBooleanField)
import Contrib.Data.BigInt.PositiveBigInt (PositiveBigInt(..))
import Contrib.Fetch (FetchError)
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Table (striped) as Table
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Types as Bootstrap
import Control.Monad.Reader.Class (asks)
import Data.Array (elem, singleton, toUnfoldable)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap, foldl)
import Data.Formatter.DateTime (formatDateTime)
import Data.Function (on)
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (un, unwrap)
import Data.String (toUpper)
import Data.Time.Duration as Duration
import Data.Tuple (snd)
import Debug (traceM)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Language.Marlowe.Core.V1.Semantics.Types (Case(..), Contract(..), Input(..), InputContent(..), Party, Token)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (genContract')
import Marlowe.Runtime.Web (post')
import Marlowe.Runtime.Web.Client (merkleize, put')
import Marlowe.Runtime.Web.Types (PostMerkleizationRequest(..), PostMerkleizationResponse(..), PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), Runtime(..), ServerURL, TextEnvelope(..), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (fragment) as DOOM
import React.Basic.DOM (input, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useContext, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (WalletContext(..), walletAddresses)

data NewInput
  = Creating
  | Submitting Contract
  | SubmissionError
  | SubmissionsSuccess

type EventListState =
  { newInput ::
      Maybe
        { party :: V1.Party
        , token :: Token
        , value :: BigInt.BigInt
        , transactionsEndpoint :: TransactionsEndpoint
        -- , contractTerms :: ContractTerms
        -- , cashFlowInfo :: List CashFlowInfo
        , marloweInfo :: Maybe MarloweInfo
        }
  }

type Props =
  { contractList :: Array ContractInfo
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

testingApply :: Boolean
testingApply = false

submit :: CborHex TransactionWitnessSetObject -> ServerURL -> TransactionEndpoint -> Aff (Either FetchError Unit)
submit witnesses serverUrl transactionEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""

    req = PutTransactionRequest textEnvelope
  put' serverUrl transactionEndpoint req

data OrderBy
  = OrderByCreationDate
  | OrderByActusContractId
  | OrderByLastUpdateDate

derive instance Eq OrderBy

mkEventList :: MkComponentM (Props -> JSX)
mkEventList = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  booleanField <- liftEffect mkBooleanField
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  liftEffect $ component "EventList" \{ contractList, connectedWallet } -> React.do
    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing }
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }
    showOnlyMyContracts /\ updateShowOnlyMyContracts <- useState false
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)

    let
      filterOwnContracts = Array.filter (isJust <<< _.userContractRole <<< un ContractInfo)
      filterContracts = if showOnlyMyContracts then filterOwnContracts else identity
      -- contractList' = filterContracts do
      --   let
      --     -- Quick and dirty hack to display just submited contracts as first
      --     someFutureBlockNumber = Runtime.BlockNumber 2058430
      --     sortedContracts = case ordering.orderBy of
      --       OrderByCreationDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.createdAt)) contractList
      --       OrderByActusContractId -> Array.sortBy (compare `on` (ContractInfo.actusContractId)) contractList
      --       OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contractList
      --   if ordering.orderAsc then sortedContracts
      --   else Array.reverse sortedContracts

      onEdit { party, token, value, transactionsEndpoint, marloweInfo } = do
        updateState _ { newInput = Just { party, token, value, transactionsEndpoint, marloweInfo } }

      onApplyInputs { party, token, value, transactionsEndpoint, marloweInfo } cw = handler_ do
        now <- nowDateTime
        -- FIXME: move aff flow into `useAff` on the component level
        launchAff_ $ do
          case possibleWalletContext of
            Just { changeAddress: Just changeAddress } -> do

              addresses <- walletAddresses cardanoMultiplatformLib cw

              let
                inputs = singleton $ NormalInput (IDeposit party party token value)

                invalidBefore = fromMaybe now $ adjust (Duration.Minutes (-2.0)) now
                invalidHereafter = fromMaybe now $ adjust (Duration.Minutes 2.0) now
                collateralUTxOs = []

                req = PostTransactionsRequest
                  { inputs
                  , invalidBefore
                  , invalidHereafter
                  , metadata: mempty
                  , tags: mempty
                  , changeAddress
                  , addresses
                  , collateralUTxOs
                  }

              post' runtime.serverURL transactionsEndpoint req
                >>= case _ of
                  Right ({ resource: PostTransactionsResponse postTransactionsResponse, links: { transaction: transactionEndpoint } }) -> do
                    traceM postTransactionsResponse
                    let
                      { tx } = postTransactionsResponse
                      TextEnvelope { cborHex: txCborHex } = tx
                    Wallet.signTx cw txCborHex true >>= case _ of
                      Right witnessSet -> do
                        submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                          Right _ -> do
                            traceM "Successfully submitted the transaction"
                            liftEffect $ msgHubProps.add $ Success $ DOOM.text $ "Successfully submitted a transaction"
                          -- liftEffect $ onSuccess contractEndpoint
                          Left err -> do
                            traceM "Error while submitting the transaction"
                            liftEffect $ msgHubProps.add $ Error $ DOOM.text $ "Error while submitting the transaction"
                            traceM err

                      Left err -> do
                        traceM err
                        pure unit

                    pure unit
                  Left _ -> do
                    traceM token
                    traceM $ BigInt.toString value
                    traceM "error"
                    pure unit

              pure unit
            _ -> do
              -- Note: this happens, when the contract is in status `Unsigned`
              pure unit

        updateState _ { newInput = Nothing }

    traceM contractList

    pure $
      DOM.div {}
        [ case state.newInput, connectedWallet of
            Just input@{ token, value, party }, Just (WalletInfo { wallet: cw }) -> modal $
              { body:
                  DOM.form {} $
                    [ DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Amount"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: BigInt.toString (BigInt.abs value)
                            }
                        ]
                    , DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Token"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: show token
                            }
                        ]
                    , DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Party"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: partyToString party
                            }
                        ]
                    ]

              , onDismiss: updateState _ { newInput = Nothing }
              , title: text "Apply inputs"
              , footer: DOOM.fragment
                  [ link
                      { label: DOOM.text "Cancel"
                      , onClick: updateState _ { newInput = Nothing }
                      , showBorders: true
                      }
                  , DOM.button
                      { className: "btn btn-primary"
                      , onClick: onApplyInputs input { value = BigInt.abs value } cw
                      }
                      [ R.text "Submit" ]
                  ]
              }
            _, _ -> mempty

        , DOM.div { className: "row justify-content-end" } do
            let
              disabled = isNothing connectedWallet
              -- showMyContracts = booleanField
              --   { disabled
              --   , initialValue: true
              --   , label: DOOM.text "Show my cash flows"
              --   , onToggle: const $ pure unit
              --   }
              showMyContractsSwitch = do
                let
                  id = "show-my-contracts"
                DOM.div { className: "form-check form-switch text-end px-3 py-2" }
                  [ DOOM.input
                      { className: "float-none form-check-input me-1"
                      , type: "checkbox"
                      , checked: showOnlyMyContracts
                      , id
                      -- This `if` covers single checkbox case
                      , onChange: handler_ do
                          updateShowOnlyMyContracts not
                      , disabled
                      , name: "radio"
                      }
                  , DOM.label
                      { className: "form-check-label text-start", htmlFor: id }
                      [ text "Show only my contracts" ]
                  ]

            DOM.div { className: "col-3 text-end" } do
              if disabled then do
                let
                  tooltipJSX = tooltip {} (DOOM.text "This switch gonna be active when you connect your wallet")
                overlayTrigger
                  { overlay: tooltipJSX
                  , placement: Bootstrap.placement.bottom
                  }
                  -- Disabled button doesn't trigger the hook,
                  -- so we wrap it in a `span`
                  [ showMyContractsSwitch ]
              else
                showMyContractsSwitch

        , table { striped: Table.striped.boolean true, hover: true } $
            [ DOM.thead {} do
                let
                  orderingTh = Table.orderingHeader ordering updateOrdering
                  th label = DOM.th { className: "text-center text-muted" } [ text label ]
                [ DOM.tr {}
                    [ orderingTh (DOOM.text "Contract Id") OrderByActusContractId
                    , do
                        let
                          label = DOOM.fragment [ DOOM.text "Contract created" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                        orderingTh label OrderByCreationDate
                    , th "Type"
                    , th "Date"
                    , if showOnlyMyContracts then
                        th "Your balance"
                      else
                        th "Amount"
                    , th "Currency"
                    -- , th "Execute debug"
                    , th "Action"
                    ]
                ]
            , DOM.tbody {}
                $ Array.fromFoldable
                $ map
                    ( \ci@(ContractInfo contractInfo@{ endpoints, userContractRole, marloweInfo }) ->
                        let
                          cashFlowInfo = Lazy.force contractInfo.cashFlowInfo
                          tdCentered = DOM.td { className: "text-center" }
                          formatAmount amount = show $ BigInt.toNumber amount / 1000000.0
                          step { prevExecuted, result } (CashFlowInfo { cashFlow, sender, token, value, transaction, userCashFlowDirection }) = do
                            let
                              cf = unwrap cashFlow
                              party = case sender of
                                ActusParty -> contractInfo.party
                                ActusCounterParty -> contractInfo.counterParty
                              item = DOM.tr {}
                                [ DOM.td {} [ text $ cf.contractId ]
                                , tdCentered [ text $ foldMap show $ map (un Runtime.BlockNumber <<< _.blockNo <<< un Runtime.BlockHeader) $ ContractInfo.createdAt ci ]
                                , tdCentered [ text $ Actus.BussinessEvents.description cf.event ]
                                , tdCentered [ foldMap text $ hush (formatDateTime "YYYY-DD-MM HH:mm:ss" cf.paymentDay) ]
                                , do
                                    let
                                      moneyInfoStr /\ cellStyle =
                                        if showOnlyMyContracts then
                                          case userCashFlowDirection of
                                            Just (IncomingFlow /\ PositiveBigInt absValue) -> ("+" <> formatAmount absValue) /\ "table-success"
                                            Just (OutgoingFlow /\ PositiveBigInt absValue) -> ("-" <> formatAmount absValue) /\ "table-danger"
                                            Just (InternalFlow /\ PositiveBigInt absValue) -> ("=" <> formatAmount absValue) /\ "table-light"
                                            _ -> "" /\ ""
                                        else
                                          formatAmount value /\ ""

                                    --   if elem cf.currency currenciesWith6Decimals
                                    --   then show <$> (((_ / 1000000.0) <<< BigInt.toNumber) <$> evalVal cf.amount)
                                    --   else BigInt.toString <$> (evalVal $ DivValue cf.amount (Constant $ BigInt.fromInt 1000000))
                                    -- ]
                                    -- [ text $ fromMaybe "" $
                                    -- ]
                                    DOM.td { className: "text-end " <> cellStyle }
                                      [ text $ moneyInfoStr ]
                                , tdCentered [ text $ if elem (toUpper cf.currency) [ "", "ADA" ] then "₳" else cf.currency ]
                                -- , tdCentered $ Array.singleton $ text $
                                --     "user role: " <> show userContractRole <> ", sender: " <> show sender <> ", transaction endpoint: " <> show (isJust endpoints.transactions)
                                , tdCentered $ Array.singleton $ case endpoints.transactions of
                                    Just transactionsEndpoint -> case transaction of
                                      Just (Runtime.TxHeader tx) -> do
                                        let
                                          txId = un Runtime.TxId tx.transactionId
                                        DOM.a
                                          { className: "btn btn-link text-decoration-none text-reset text-decoration-underline-hover"
                                          , target: "_blank"
                                          , href: "https://preprod.cardanoscan.io/transaction/" <> txId
                                          }
                                          [ DOM.span { className: "me-1" } $ Icons.toJSX Icons.eye
                                          , text "Tx details"
                                          ]
                                      Nothing -> do
                                        let
                                          button = Lazy.defer \_ -> linkWithIcon
                                            { icon: Icons.bullsEye
                                            , label: DOOM.text "Execute"
                                            , onClick: onEdit { party, token, value, transactionsEndpoint, marloweInfo }
                                            , disabled: not prevExecuted
                                            }

                                        -- button = DOM.button
                                        --   { onClick: onEdit { party, token, value, transactionsEndpoint }
                                        --   , className: "btn btn-secondary btn-sm",
                                        --   }
                                        --   "Execute"
                                        case sender, userContractRole of
                                          _, Just BothParties -> Lazy.force button
                                          ActusParty, Just ContractParty -> Lazy.force button
                                          ActusCounterParty, Just ContractCounterParty -> Lazy.force button
                                          _, _ -> mempty
                                    Nothing -> mempty
                                ]
                            { prevExecuted: isJust transaction, result: List.Cons item result }
                        in
                          Array.reverse $ Array.fromFoldable $ _.result $ foldl step { prevExecuted: true, result: List.Nil } cashFlowInfo
                    )
                    contractList
            ]
        ]

continuationHash :: Contract -> Maybe String
continuationHash (When [ MerkleizedCase _ h ] _ _) = Just h
continuationHash _ = Nothing

partyToString :: Party -> String
partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role
