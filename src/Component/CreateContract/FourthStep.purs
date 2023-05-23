module Component.CreateContract.FourthStep where

import Prelude

import Actus.Domain (ContractTerms(..))
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.CreateContract.Types (FourthStepBaseRow)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Map as Map
import Data.Newtype (class Newtype)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata (Metadata(..), actusMetadataKey)
import Marlowe.Runtime.Web.Client (ClientError(..), merkleize, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent(..), PostMerkleizationRequest(..), PostMerkleizationResponse(..), PutContractRequest(..), Runtime(..), ServerURL, TextEnvelope(..), toTextEnvelope)
import Marlowe.Runtime.Web.Types as RT
import React.Basic (fragment) as DOOM
import React.Basic.DOM (tbody_, td_, text, tr_) as DOOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler)
import React.Basic.Hooks (JSX, component, useState', (/\))
import React.Basic.Hooks as React
import Type.Row (type (+))
import Wallet as Wallet

newtype ContractData = ContractData { | FourthStepBaseRow + () }

derive instance Newtype ContractData _

type Props =
  { contractData :: ContractData
  , inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

data SubmissionStep
  = Creating
  | Created (Either String PostContractsResponseContent)
  | Signing (Either String PostContractsResponseContent)
  | Signed (Either ClientError PostContractsResponseContent)

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "SubmitContract" \{ contractData: contractData@(ContractData contractDataRec), connectedWallet, inModal, onDismiss, onSuccess } -> React.do
    step /\ setStep <- useState' Creating
    let
      onSubmit f = handler preventDefault \_ -> do
        traceM "ON SUBMIT CLICKED"
        launchAff_ $ do
          f contractData runtime.serverURL runtime.root >>= case _ of
            Right res@{ resource: PostContractsResponseContent postContractsResponse, links: { contract: contractEndpoint } } -> do
              let
                { contractId, tx } = postContractsResponse
                TextEnvelope { cborHex: txCborHex } = tx
                lib = Lib.props cardanoMultiplatformLib
                txCbor = cborHexToCbor txCborHex
              traceM "Successfully created a transaction"
              let
                WalletInfo { wallet: walletApi } = connectedWallet
              Wallet.signTx walletApi txCborHex false >>= case _ of
                Right witnessSet -> do
                  submit witnessSet runtime.serverURL contractEndpoint >>= case _ of
                    Right _ -> do
                      liftEffect $ onSuccess contractEndpoint
                    Left err -> do
                      traceM "Error while submitting the transaction"
                      traceM err
                Left err -> do
                  traceM "Failed to sign transaction"
                  traceM err

            Left err ->
              traceM $ "Error: " <> show err
    pure $ do
      let
        body = table { striped: Table.striped.boolean true, responsive: Table.responsive.sm, size: Table.sm } do
          let
            ContractTerms ct = contractDataRec.contractTerms
            row header value = DOOM.tr_ do
              [ DOM.th { scope: "row" } [ DOOM.text header ]
              , DOOM.td_
                  [ DOM.div { className: "text-truncate w-32rem" } [ DOOM.text value ] ]
              ]
          DOOM.tbody_ do
            [ row "Party (you)" (partyToString $ contractDataRec.party)
            , row "Counterparty" (partyToString $ contractDataRec.counterParty)
            , row "Contract type" (show ct.contractType)
            ]
        -- FIXME: Introduce nice rendering of a contract here
        -- <> case ct.contractType of
        --       PAM -> [ row "Notional" (show ct.notional)
        --              , row "Currency" (show ct.currency)
        --              , row "Interest rate" (show ct.interestRate)
        --              , row "Interest rate cycle" (show ct.interestRateCycle)
        --              -- , row "Day count convention" (show ct.dayCountConvention)
        --              , row "Maturity date" (show ct.maturityDate)
        --              , row "PAM schedule" (show ct.pamSchedule)
        --              ]
        --       _ -> mempty
        footer = DOOM.fragment
          [ link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          , DOM.button
              do
                { className: "btn btn-primary"
                , onClick: onSubmit create
                }
              [ DOOM.text "Submit" ]
          , DOM.button
              do
                { className: "btn btn-primary"
                , onClick: onSubmit createMerkleized
                }
              [ DOOM.text "Submit merkleized" ]
          ]

      if inModal then modal
        { title: DOOM.text "Submit contract"
        , onDismiss
        , body
        , footer
        , size: Modal.Large
        }
      else
        DOOM.fragment
          [ body
          , footer
          ]

createMerkleized :: ContractData -> ServerURL -> ContractsEndpoint -> Aff _
createMerkleized contractData serverUrl contractsEndpoint = do
  let
    ContractData { contractTerms, contract, party, counterParty, changeAddress, usedAddresses } = contractData
    metadata = RT.Metadata $ Map.singleton actusMetadataKey $ encodeJson $ Metadata
      { contractTerms: contractTerms
      , party
      , counterParty
      }

    merkleizationReq = PostMerkleizationRequest { contract }

  merkleize serverUrl merkleizationReq >>= case _ of
    Right { payload } -> do
      let
        PostMerkleizationResponse { contract: merkleized } = payload

        req = PostContractsRequest
          { metadata
          -- , version :: MarloweVersion
          -- , roles :: Maybe RolesConfig
          , tags: mempty -- TODO: use instead of metadata
          , contract: merkleized
          , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
          , changeAddress: changeAddress
          , addresses: usedAddresses <> [ changeAddress ]
          , collateralUTxOs: []
          }

      post' serverUrl contractsEndpoint req
    Left err -> do
      traceM err
      pure $ Left MerkleizationError

create :: ContractData -> ServerURL -> ContractsEndpoint -> Aff _
create contractData serverUrl contractsEndpoint = do
  let
    ContractData { contractTerms, contract, party, counterParty, changeAddress, usedAddresses } = contractData
    metadata = RT.Metadata $ Map.singleton actusMetadataKey $ encodeJson $ Metadata
      { contractTerms: contractTerms
      , party
      , counterParty
      }

    req = PostContractsRequest
      { metadata
      -- , version :: MarloweVersion
      -- , roles :: Maybe RolesConfig
      , tags: mempty -- TODO: use instead of metadata
      , contract
      , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
      , changeAddress: changeAddress
      , addresses: usedAddresses <> [ changeAddress ]
      , collateralUTxOs: []
      }

  post' serverUrl contractsEndpoint req

submit :: CborHex TransactionWitnessSetObject -> ServerURL -> ContractEndpoint -> Aff _
submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutContractRequest textEnvelope
  put' serverUrl contractEndpoint req

partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role
