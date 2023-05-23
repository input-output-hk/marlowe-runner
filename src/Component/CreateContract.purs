module Component.CreateContract where

import Prelude

import Component.CreateContract.FirstStep as FirstStep
import Component.CreateContract.FourthStep as FourthStep
import Component.CreateContract.SecondStep as SecondStep
import Component.CreateContract.ThirdStep as ThirdStep
import Component.CreateContract.Types (ContractFormTypeChoice(..), WizzardStep(..))
import Component.Types (MkComponentM, WalletInfo)
import Control.Monad.Reader.Class (asks)
import Effect (Effect)
import Effect.Class (liftEffect)
import Marlowe.Runtime.Web.Types (ContractEndpoint)
import React.Basic (JSX)
import React.Basic.Hooks (component, useState', (/\))
import React.Basic.Hooks as React
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet as Wallet

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

-- FIXME: add notifications to the wizzard
-- msgHub@(MessageHub msgHubProps) <- asks _.msgHub
-- during handling
--             \reson ->
--                 for_ reason \err ->
--                   msgHubProps.add $ Error $ DOOM.text err
--                 Nothing -> pure unit

-- FIXME: add submit testing to the wizzard
-- useEffectOnce $ do
--   when testingSubmit do
--     let
--       -- nami-work
--       myAddress = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

--       -- nami-test
--       counterPartyAddress = "addr_test1qrp6m3r307r3d73t6vjnqssqmj9deqcprkm5v0yhuyvyfgm6fftzwd90f7aanfwl28s4efxxt3252p3uet87klt2aj4qzgw242"

--       UseForm.Form { validator } = mkForm cardanoMultiplatformLib

--       query = Query.fromHomogeneous
--         { "party": [ myAddress ]
--         , "counter-party": [ counterPartyAddress ]
--         , "contract-terms": [ initialJson ]
--         }

--     runValidator validator query >>= case _ of
--       V (Right result) -> do
--         updateState _ { newContract = Just $ Submitting result }
--       V (Left err) -> do
--         logger $ unsafeStringify err
--         pure unit
--   pure (pure unit)

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  firstStepComponent <- FirstStep.mkComponent
  secondStepComponent <- SecondStep.mkComponent
  thirdStepComponent <- ThirdStep.mkComponent
  fourthStepComponent <- FourthStep.mkComponent

  liftEffect $ component "ContractForm" \{ connectedWallet, onSuccess, onDismiss, inModal } -> React.do
    step /\ setStep <- useState' FirstStep -- (SecondStep AmortizingLoans)
    pure $ case step of
      FirstStep -> firstStepComponent
        { onDismiss
        , onSuccess: \result -> do
            setStep $ SecondStep result
        , inModal
        }
      SecondStep contractFormTypeChoice -> secondStepComponent
        { contractFormTypeChoice
        , onDismiss
        , onSuccess: \contractTerms -> do
            setStep $ ThirdStep { contractTerms, contractFormTypeChoice }
        , inModal
        }
      ThirdStep input@{ contractTerms } -> thirdStepComponent
        { contractTerms
        , connectedWallet
        , onDismiss
        , onSuccess: \r -> do
            let
              r' = Record.merge
                r
                input
            setStep $ FourthStep r'
        , inModal: true
        }
      FourthStep r -> do
        let
          r' = Record.delete (Proxy :: Proxy "contractFormTypeChoice") r
          contractData = FourthStep.ContractData r'
        fourthStepComponent
          { contractData
          , connectedWallet
          , onSuccess: \result -> do
              onSuccess result
          , onDismiss
          , inModal
          }

