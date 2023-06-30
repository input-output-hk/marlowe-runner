module Component.InputHelper where

import Prelude

import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either)
import Data.Foldable (foldM)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Marlowe.Core.V1.Folds (MapStep(..), foldMapContract)
import Language.Marlowe.Core.V1.Semantics (applyCases, evalObservation, reduceContractStep) as V1
import Language.Marlowe.Core.V1.Semantics (evalObservation, evalValue, reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Action(..), Bound, Case(..), ChoiceId(..), Contract(..), Environment(..), Observation(..), Party(..), Payee(..), State, TimeInterval(..), Token, TokenName, Value(..), Address)
import Language.Marlowe.Core.V1.Semantics.Types (ApplyResult(..), Contract, Environment(..), Input(..), InputContent(..), ReduceResult(..), ReduceStepResult(..), State, TimeInterval) as V1

data DepositInput = DepositInput AccountId Party Token BigInt.BigInt (Maybe Contract)
data ChoiceInput = ChoiceInput ChoiceId (Array Bound) (Maybe Contract)
data NotifyInput = NotifyInput (Maybe Contract)

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
  nextInputs' _ = []

  nextInputsCase (Case (Deposit accId p tok val) cont) = [ DepositInput accId p tok (evalValue env state' val) (Just cont) ]
  nextInputsCase (MerkleizedCase (Deposit accId p tok val) _) = [ DepositInput accId p tok (evalValue env state' val) Nothing ]
  nextInputsCase _ = []

nextChoice :: Environment -> State -> Contract -> Array ChoiceInput
nextChoice env state contract = nextInputs' contract'
  where
  contract' /\ _ = reduceContract env state contract

  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _ = []

  nextInputsCase (Case (Choice chid bnds) cont) = [ ChoiceInput chid bnds (Just cont) ]
  nextInputsCase (MerkleizedCase (Choice chid bnds) _) = [ ChoiceInput chid bnds Nothing ]
  nextInputsCase _ = []

nextNotify :: Environment -> State -> Contract -> Array NotifyInput
nextNotify env state contract = nextInputs' contract'
  where
  contract' /\ state' = reduceContract env state contract

  nextInputs' (When cs _ _) = Array.concat $ map nextInputsCase cs
  nextInputs' _ = []

  nextInputsCase (Case (Notify obs) cont) | evalObservation env state' obs = [ NotifyInput (Just cont) ]
  nextInputsCase (MerkleizedCase (Notify obs) _) | evalObservation env state' obs = [ NotifyInput Nothing ]
  nextInputsCase _ = []

-- | There is a corner case in here:
-- | * we return continuation if we are in `When` (so we for example return `Close`).
-- | * we return `Close` when the input contract is `Close` :-)
nextTimeoutAdvance :: Environment -> State -> Contract -> Maybe Contract
nextTimeoutAdvance env@(Environment { timeInterval: TimeInterval startTime _ }) state contract = nextTimeoutAdvance' contract'
  where
  contract' /\ _ = reduceContract env state contract

  nextTimeoutAdvance' (When _ t contract') =
    if startTime >= t then Just contract'
    else Nothing
  nextTimeoutAdvance' Close = Just Close
  nextTimeoutAdvance' _ = Nothing

-- Either possibly incomplete list of role tokens or a complete list.
-- rolesInContract :: Contract -> Either (Array TokenName) (Array TokenName)
rolesInContract :: Contract -> Array TokenName
rolesInContract = Array.nub <<< foldMapContract
  ( MapStep
      { mapCase: rolesCases
      , mapContract: rolesContract
      , mapObservation: rolesObservation
      , mapValue: rolesValue
      }
  )
  where
  rolesObservation :: Observation -> Array TokenName
  rolesObservation (ChoseSomething (ChoiceId _ (Role t))) = [ t ]
  rolesObservation _ = []

  rolesValue :: Value -> Array TokenName
  rolesValue (AvailableMoney (Role t) _) = [ t ]
  rolesValue (ChoiceValue (ChoiceId _ (Role t))) = [ t ]
  rolesValue _ = []

  rolesCases :: Case -> Array TokenName
  rolesCases (Case (Deposit (Role t1) (Role t2) _ _) _) = [ t1, t2 ]
  rolesCases (Case (Deposit (Role t1) _ _ _) _) = [ t1 ]
  rolesCases (Case (Deposit _ (Role t2) _ _) _) = [ t2 ]
  rolesCases (Case (Deposit _ _ _ _) _) = []
  rolesCases (Case (Choice (ChoiceId _ (Role t)) _) _) = [ t ]
  rolesCases (Case (Choice _ _) _) = []
  rolesCases (Case (Notify _) _) = []
  rolesCases (MerkleizedCase (Deposit (Role t1) (Role t2) _ _) _) = [ t1, t2 ]
  rolesCases (MerkleizedCase (Deposit (Role t1) _ _ _) _) = [ t1 ]
  rolesCases (MerkleizedCase (Deposit _ (Role t2) _ _) _) = [ t2 ]
  rolesCases (MerkleizedCase (Deposit _ _ _ _) _) = []
  rolesCases (MerkleizedCase (Choice (ChoiceId _ (Role t)) _) _) = [ t ]
  rolesCases (MerkleizedCase (Choice _ _) _) = []
  rolesCases (MerkleizedCase (Notify _) _) = []

  rolesContract :: Contract -> Array TokenName
  rolesContract Close = []
  rolesContract (When _ _ _) = []
  rolesContract (Pay (Role t1) (Party (Role t2)) _ _ _) = [ t1, t2 ]
  rolesContract (Pay (Role t) _ _ _ _) = [ t ]
  rolesContract (Pay _ (Party (Role t)) _ _ _) = [ t ]
  rolesContract (Pay _ _ _ _ _) = []
  rolesContract (If _ _ _) = []
  rolesContract (Let _ _ _) = []
  rolesContract (Assert _ _) = []

addressesInContract :: Contract -> Array Address
addressesInContract = Array.nub <<< foldMapContract
  ( MapStep
      { mapCase: addressesCases
      , mapContract: addressesContract
      , mapObservation: addressesObservation
      , mapValue: addressesValue
      }
  )
  where
  addressesObservation :: Observation -> Array TokenName
  addressesObservation (ChoseSomething (ChoiceId _ (Address t))) = [ t ]
  addressesObservation _ = []

  addressesValue :: Value -> Array TokenName
  addressesValue (AvailableMoney (Address t) _) = [ t ]
  addressesValue (ChoiceValue (ChoiceId _ (Address t))) = [ t ]
  addressesValue _ = []

  addressesCases :: Case -> Array TokenName
  addressesCases (Case (Deposit (Address t1) (Address t2) _ _) _) = [ t1, t2 ]
  addressesCases (Case (Deposit (Address t1) _ _ _) _) = [ t1 ]
  addressesCases (Case (Deposit _ (Address t2) _ _) _) = [ t2 ]
  addressesCases (Case (Deposit _ _ _ _) _) = []
  addressesCases (Case (Choice (ChoiceId _ (Address t)) _) _) = [ t ]
  addressesCases (Case (Choice _ _) _) = []
  addressesCases (Case (Notify _) _) = []
  addressesCases (MerkleizedCase (Deposit (Address t1) (Address t2) _ _) _) = [ t1, t2 ]
  addressesCases (MerkleizedCase (Deposit (Address t1) _ _ _) _) = [ t1 ]
  addressesCases (MerkleizedCase (Deposit _ (Address t2) _ _) _) = [ t2 ]
  addressesCases (MerkleizedCase (Deposit _ _ _ _) _) = []
  addressesCases (MerkleizedCase (Choice (ChoiceId _ (Address t)) _) _) = [ t ]
  addressesCases (MerkleizedCase (Choice _ _) _) = []
  addressesCases (MerkleizedCase (Notify _) _) = []

  addressesContract :: Contract -> Array TokenName
  addressesContract Close = []
  addressesContract (When _ _ _) = []
  addressesContract (Pay (Address t1) (Party (Address t2)) _ _ _) = [ t1, t2 ]
  addressesContract (Pay (Address t) _ _ _ _) = [ t ]
  addressesContract (Pay _ (Party (Address t)) _ _ _) = [ t ]
  addressesContract (Pay _ _ _ _ _) = []
  addressesContract (If _ _ _) = []
  addressesContract (Let _ _ _) = []
  addressesContract (Assert _ _) = []

-- computePath :: [InputContent /\ TimeInterval] → Contract → State → [Maybe Int]
-- computePath inputs contract state = foldr step 

-- reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult

data ExecutionBranch
  = WhenBranch (Maybe Int)
  | IfBranch Boolean

whenCaseBranch :: Int -> ExecutionBranch
whenCaseBranch = WhenBranch <<< Just

whenTimeoutBranch :: ExecutionBranch
whenTimeoutBranch = WhenBranch Nothing

ifTrueBranch :: ExecutionBranch
ifTrueBranch = IfBranch true

ifFalseBranch :: ExecutionBranch
ifFalseBranch = IfBranch false

type InputExecutionPath = NonEmptyArray (ExecutionBranch /\ Contract /\ State)

-- Given an input and contract compute the branching execution path.
-- This is a substep of the `executionPath` computation.
inputExecutionPath :: (Maybe V1.InputContent /\ V1.TimeInterval) -> Contract -> State -> Either String InputExecutionPath
inputExecutionPath (possibleInputContent /\ timeInterval) = do
  let
    env = V1.Environment { timeInterval }
  case _, _ of
    contract@(When cases _ _), state -> do
      case V1.reduceContractStep env state contract of
        V1.AmbiguousTimeIntervalReductionError -> throwError "AmbiguousTimeIntervalReductionError"
        V1.NotReduced -> case possibleInputContent of
          Just inputContent -> do
            let
              applied = Array.catMaybes $ cases `mapWithIndexFlipped` \idx c -> do
                let
                  cases' = c List.: List.Nil
                case V1.applyCases env state (V1.NormalInput inputContent) cases' of
                  V1.Applied _ state' contract' -> Just (whenCaseBranch idx /\ contract' /\ state')
                  _ -> Nothing
            case Array.head applied of
              Just res -> pure $ Array.NonEmpty.singleton res
              Nothing -> throwError $ "No match found for input " <> show inputContent <> " in when with cases " <> show cases
          Nothing -> throwError "Expecting an input"
        V1.Reduced _ _ state' contract' -> do
          Array.NonEmpty.cons' (whenTimeoutBranch /\ contract' /\ state') <$> case possibleInputContent of
            Just inputContent -> Array.NonEmpty.toArray <$> inputExecutionPath (Just inputContent /\ timeInterval) contract' state'
            Nothing -> pure []
    contract@(If observation _ _), state -> do
      let
        idx =
          if V1.evalObservation env state observation then ifTrueBranch
          else ifFalseBranch
      case V1.reduceContractStep env state contract of
        V1.AmbiguousTimeIntervalReductionError -> throwError "AmbiguousTimeIntervalReductionError"
        V1.NotReduced -> throwError "Contract already reduced"
        V1.Reduced _ _ state' contract' -> do
          Array.NonEmpty.cons' (idx /\ contract' /\ state') <$> case possibleInputContent of
            Just inputContent -> Array.NonEmpty.toArray <$> inputExecutionPath (Just inputContent /\ timeInterval) contract' state'
            Nothing -> pure []

    contract, state -> case V1.reduceContractStep (Environment { timeInterval }) state contract of
      V1.AmbiguousTimeIntervalReductionError -> throwError "AmbiguousTimeIntervalReductionError"
      V1.NotReduced -> throwError "Contract already reduced"
      V1.Reduced _ _ state' contract' -> do
        Array.NonEmpty.cons' (ifTrueBranch /\ contract' /\ state') <$> case possibleInputContent of
          Just inputContent -> Array.NonEmpty.toArray <$> inputExecutionPath (Just inputContent /\ timeInterval) contract' state'
          Nothing -> pure []

type ExecutionPath = List ((Maybe V1.InputContent /\ V1.TimeInterval) /\ InputExecutionPath)

executionPath :: Array ((Maybe V1.InputContent) /\ V1.TimeInterval) -> V1.Contract -> V1.State -> Either String ExecutionPath
executionPath inputs contract state = do
  let
    initialAcc :: { contract :: V1.Contract, state:: V1.State, executionPath :: ExecutionPath }
    initialAcc = { contract, state, executionPath: List.Nil }

    step acc input = do
      sub <- inputExecutionPath input acc.contract acc.state
      let
        (_ /\ contract' /\ state') = Array.NonEmpty.head sub
      pure { contract: contract', state: state', executionPath: List.Cons (input /\ sub) acc.executionPath }
  { executionPath: ep } <- foldM step initialAcc inputs
  pure $ List.reverse ep

