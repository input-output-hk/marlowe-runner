module Component.InputHelper where

import Prelude

import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Maybe (Maybe(..))
import Language.Marlowe.Core.V1.Semantics (evalObservation, evalValue)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Action(..), Bound, Case(..), ChoiceId, Contract(..), Environment, Party, State, Token)

data DepositInput = DepositInput AccountId Party Token BigInt.BigInt (Maybe Contract)
data ChoiceInput  = ChoiceInput ChoiceId (Array Bound) (Maybe Contract)
data NotifyInput  = NotifyInput (Maybe Contract)

-- | Contract expected to be in a quiescent state
nextDeposit :: Environment -> State -> Contract -> Array DepositInput
nextDeposit env state contract = nextInputs' contract 
  where
  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _             = []

  nextInputsCase (Case (Deposit accId p tok val) cont) = [DepositInput accId p tok (evalValue env state val) (Just cont)]
  nextInputsCase (MerkleizedCase (Deposit accId p tok val) _) = [DepositInput accId p tok (evalValue env state val) Nothing]
  nextInputsCase _ = []

nextChoice :: Environment -> State -> Contract -> Array ChoiceInput
nextChoice _ _ contract = nextInputs' contract
  where
  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _             = []

  nextInputsCase (Case (Choice chid bnds) cont) = [ChoiceInput chid bnds (Just cont)]
  nextInputsCase (MerkleizedCase (Choice chid bnds) _) = [ChoiceInput chid bnds Nothing]
  nextInputsCase _ = []

nextNotify :: Environment -> State -> Contract -> Array NotifyInput
nextNotify env state contract = nextInputs' contract
  where
  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _             = []

  nextInputsCase (Case (Notify obs) cont) | evalObservation env state obs = [NotifyInput (Just cont)]
  nextInputsCase (MerkleizedCase (Notify obs) _) | evalObservation env state obs = [NotifyInput Nothing]
  nextInputsCase _ = []
