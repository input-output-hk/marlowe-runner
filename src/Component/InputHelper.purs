module Component.InputHelper where

import Prelude

import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Marlowe.Core.V1.Folds (MapStep(..), foldMapContract)
import Language.Marlowe.Core.V1.Semantics (evalObservation, evalValue, reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Action(..), Bound, Case(..), ChoiceId(..), Contract(..), Environment(..), Observation(..), Party(..), Payee(..), State, TimeInterval(..), Token, TokenName, Value(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1

data DepositInput = DepositInput AccountId Party Token BigInt.BigInt (Maybe Contract)
data ChoiceInput  = ChoiceInput ChoiceId (Array Bound) (Maybe Contract)
data NotifyInput  = NotifyInput (Maybe Contract)

toIDeposit :: DepositInput -> Maybe V1.InputContent
toIDeposit (DepositInput accId p tok val (Just _)) = Just $ V1.IDeposit accId p tok val
toIDeposit _ = Nothing

toIChoice :: ChoiceInput -> BigInt.BigInt -> Maybe V1.InputContent
toIChoice (ChoiceInput chid _ (Just _)) value = do
  Just $ V1.IChoice chid value
toIChoice _ _ = Nothing

toINotify :: NotifyInput -> Maybe V1.InputContent
toINotify (NotifyInput (Just _)) = Just V1.INotify
toINotify _ = Nothing

-- FIXME: We should probably rise errors here
reduceContract :: Environment -> State -> Contract -> (Contract /\ State)
reduceContract env state contract = case reduceContractUntilQuiescent env state contract of
    V1.ContractQuiescent _ _ _ s c -> c /\ s
    _ -> contract /\ state

-- | Contract expected to be in a quiescent state
nextDeposit :: Environment -> State -> Contract -> Array DepositInput
nextDeposit env state contract = nextInputs' contract'
  where
  contract' /\ state' = reduceContract env state contract

  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _             = []

  nextInputsCase (Case (Deposit accId p tok val) cont) = [DepositInput accId p tok (evalValue env state' val) (Just cont)]
  nextInputsCase (MerkleizedCase (Deposit accId p tok val) _) = [DepositInput accId p tok (evalValue env state' val) Nothing]
  nextInputsCase _ = []

nextChoice :: Environment -> State -> Contract -> Array ChoiceInput
nextChoice env state contract = nextInputs' contract'
  where
  contract' /\ _ = reduceContract env state contract

  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _             = []

  nextInputsCase (Case (Choice chid bnds) cont) = [ChoiceInput chid bnds (Just cont)]
  nextInputsCase (MerkleizedCase (Choice chid bnds) _) = [ChoiceInput chid bnds Nothing]
  nextInputsCase _ = []

nextNotify :: Environment -> State -> Contract -> Array NotifyInput
nextNotify env state contract = nextInputs' contract'
  where
  contract' /\ state' = reduceContract env state contract

  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _             = []

  nextInputsCase (Case (Notify obs) cont) | evalObservation env state' obs = [NotifyInput (Just cont)]
  nextInputsCase (MerkleizedCase (Notify obs) _) | evalObservation env state' obs = [NotifyInput Nothing]
  nextInputsCase _ = []

-- | There is a corner case in here:
-- | * we return continuation if we are in `When` (so we for example return `Close`).
-- | * we return `Close` when the input contract is `Close` :-)
nextTimeoutAdvance :: Environment -> State -> Contract -> Maybe Contract
nextTimeoutAdvance env@(Environment { timeInterval: TimeInterval startTime _ }) state contract = nextTimeoutAdvance' contract'
  where
  contract' /\ _ = reduceContract env state contract

  nextTimeoutAdvance' (When _ t contract') = if startTime >= t
    then Just contract'
    else Nothing
  nextTimeoutAdvance' Close = Just Close
  nextTimeoutAdvance' _ = Nothing

-- Either possibly incomplete list of role tokens or a complete list.
-- rolesInContract :: Contract -> Either (Array TokenName) (Array TokenName)
rolesInContract :: Contract -> Array TokenName
rolesInContract = Array.nub <<< foldMapContract (MapStep
  { mapCase: rolesCases
  , mapContract: rolesContract
  , mapObservation: rolesObservation
  , mapValue: rolesValue
  })
  where
  rolesObservation :: Observation -> Array TokenName
  rolesObservation (ChoseSomething (ChoiceId _ (Role t))) = [t]
  rolesObservation _ = []

  rolesValue :: Value -> Array TokenName
  rolesValue (AvailableMoney (Role t) _) = [t]
  rolesValue (ChoiceValue (ChoiceId _ (Role t))) = [t]
  rolesValue _ = []

  rolesCases :: Case -> Array TokenName
  rolesCases (Case (Deposit (Role t1) (Role t2) _ _) _) = [t1, t2]
  rolesCases (Case (Deposit (Role t1) _ _ _) _) = [t1]
  rolesCases (Case (Deposit _ (Role t2) _ _) _) = [t2]
  rolesCases (Case (Deposit _ _ _ _) _) = []
  rolesCases (Case (Choice (ChoiceId _ (Role t)) _) _) = [t]
  rolesCases (Case (Choice _ _) _) = []
  rolesCases (Case (Notify _) _) = []
  rolesCases (MerkleizedCase (Deposit (Role t1) (Role t2) _ _) _) = [t1, t2]
  rolesCases (MerkleizedCase (Deposit (Role t1) _ _ _) _) = [t1]
  rolesCases (MerkleizedCase (Deposit _ (Role t2) _ _) _) = [t2]
  rolesCases (MerkleizedCase (Deposit _ _ _ _) _) = []
  rolesCases (MerkleizedCase (Choice (ChoiceId _ (Role t)) _) _) = [t]
  rolesCases (MerkleizedCase (Choice _ _) _) = []
  rolesCases (MerkleizedCase (Notify _) _) = []

  rolesContract :: Contract -> Array TokenName
  rolesContract Close = []
  rolesContract (When _ _ _) = []
  rolesContract (Pay (Role t1) (Party (Role t2)) _ _ _) = [t1,t2]
  rolesContract (Pay (Role t) _ _ _ _) = [t]
  rolesContract (Pay _ (Party (Role t)) _ _ _) = [t]
  rolesContract (Pay _ _ _ _ _) = []
  rolesContract (If _ _ _) = []
  rolesContract (Let _ _ _) = []
  rolesContract (Assert _ _) = []
