-- | Given ACTUS contract terms, cashflows are projected based on risk factors
module Actus.Core
  ( genProjectedCashflows
  ) where

import Prelude

import Actus.Domain (class ActusOps, CT(..), CashFlow(..), ContractState(..), ContractTerms(..), DS(..), EventType(..), RiskFactors, ShiftedDay)
import Actus.Model.ContractSchedule (maturity, schedule)
import Actus.Model.Payoff (CtxPOF, payoff)
import Actus.Model.StateInitialization (initializeState)
import Actus.Model.StateTransition (CtxSTF, stateTransition)
import Control.Alt ((<|>))
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.DateTime (DateTime)
import Data.Enum (fromEnum)
import Data.Enum.Generic (class GenericBoundedEnum, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), concat, concatMap, dropWhile, filter, filterM, groupBy, mapMaybe, sortBy, tail, unzip, zip, (..), (:))
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Semigroup.Foldable (foldl1)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))

-- |'genProjectedCashflows' generates a list of projected cashflows for
-- given contract terms and provided risk factors. The function returns
-- an empty list, if building the initial state given the contract terms
-- fails or in case there are no cash flows.
genProjectedCashflows
  :: forall a b
   . ActusOps a
  => EuclideanRing a
  => Show b
  => Eq b
  =>
  -- | Party and Counter-party for the contract
  b /\ b
  ->
  -- | Risk factors as a function of event type and time
  (EventType -> DateTime -> RiskFactors a)
  ->
  -- | Contract terms
  ContractTerms
  ->
  -- | List of projected cash flows
  List (CashFlow a b)
genProjectedCashflows parties riskFactors contractTerms =
  let
    context = buildCtx riskFactors contractTerms
    cashFlows = runReader (genProjectedPayoffs contractTerms) context
  in
    netting contractTerms $ genCashflow parties contractTerms <$> cashFlows
  where
  netting :: ContractTerms -> List (CashFlow a b) -> List (CashFlow a b)
  netting (ContractTerms { deliverySettlement: Just DS_S }) = netCashflows
  netting _ = \x -> x

  groupCashflows :: List (CashFlow a b) -> List (NonEmptyList (CashFlow a b))
  groupCashflows cf = groupBy f cf
    where
    f (CashFlow a) (CashFlow b) =
      a.event == b.event
        && a.paymentDay == b.paymentDay
        && a.party == b.party
        && a.counterparty == b.counterparty
        && a.currency == b.currency
        && a.contractId == b.contractId

  netCashflows cf = map (foldl1 plus) $ groupCashflows cf
    where
    plus :: CashFlow a b -> CashFlow a b -> CashFlow a b
    plus (CashFlow a) (CashFlow b) = CashFlow $
      a
        { amount = a.amount + b.amount
        , notional = a.notional + b.notional
        }

-- | Bulid the context allowing to perform state transitions
buildCtx
  :: forall a
   . ActusOps a
  => EuclideanRing a
  =>
  -- | Risk factors as a function of event type and time
  (EventType -> DateTime -> RiskFactors a)
  ->
  -- | Contract terms
  ContractTerms
  ->
  -- | Context
  CtxSTF a
buildCtx riskFactors contractTerms =
  { contractTerms
  , fpSchedule: (_.calculationDay <$> schedule FP contractTerms)
  , -- init & stf rely on the fee payment schedule
    prSchedule: (_.calculationDay <$> schedule PR contractTerms)
  , -- init & stf rely on the principal redemption schedule
    ipSchedule: (_.calculationDay <$> schedule IP contractTerms)
  , -- init & stf rely on the interest payment schedule
    maturity: (maturity contractTerms)
  , riskFactors
  }

-- |Generate cash flows
genCashflow
  :: forall a b
   . ActusOps a
  => EuclideanRing a
  => Show b
  =>
  -- | Party and Counter-party for the contract
  b /\ b
  ->
  -- | Contract terms
  ContractTerms
  ->
  -- | Projected payoff
  Event /\ ContractState a /\ a
  ->
  -- | Projected cash flow
  CashFlow a b
genCashflow (party /\ counterparty) (ContractTerms { contractId, contractRole, currency, settlementCurrency }) ((event /\ { paymentDay, calculationDay }) /\ ContractState { nt } /\ amount) =
  CashFlow
    { contractId
    , party
    , counterparty
    , paymentDay
    , calculationDay
    , event
    , contractRole
    , amount
    , notional: nt
    , currency: fromMaybe "" $ settlementCurrency <|> currency
    }

-- |Generate projected cash flows
genProjectedPayoffs
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ContractTerms
  -> Reader (CtxSTF a) (List (Event /\ ContractState a /\ a))
genProjectedPayoffs = genProjectedPayoffs' <<< genSchedule

-- |Generate projected cash flows
genProjectedPayoffs'
  :: forall a
   . ActusOps a
  => EuclideanRing a
  =>
  -- | Events
  List Event
  ->
  -- | Projected cash flows
  Reader (CtxSTF a) (List (Event /\ ContractState a /\ a))
genProjectedPayoffs' events =
  do
    states <- initializeState >>= genStates events
    (eventTypes /\ filteredStates) <- unzip <$> filterM filtersStates (zip (fromMaybe Nil $ tail events) states)

    payoffs <- genPayoffs eventTypes filteredStates
    pure $ zip eventTypes $ zip filteredStates payoffs

-- |Generate schedules
genSchedule
  ::
     -- | Contract terms
     ContractTerms
  ->
  -- | Schedule
  List Event
genSchedule contractTerms =
  sortBy (comparing \(ev /\ { paymentDay }) -> (paymentDay /\ ev)) $ genFixedSchedule contractTerms

genFixedSchedule
  ::
     -- | Contract terms
     ContractTerms
  ->
  -- | Schedule
  List Event
genFixedSchedule contractTerms@(ContractTerms { terminationDate, statusDate }) =
  filter filtersSchedules <<< postProcessSchedules <<< sortBy (comparing \(ev /\ { paymentDay }) -> (paymentDay /\ ev)) $ event : concatMap scheduleEvent allElements
  where
  event :: Event
  event = AD /\ { calculationDay: statusDate, paymentDay: statusDate }

  allElements :: forall b rep. Generic b rep => GenericBoundedEnum rep => GenericTop rep => GenericBottom rep => List b
  allElements = mapMaybe genericToEnum (idxFrom .. idxTo)
    where
    idxFrom = genericFromEnum (genericBottom :: b)
    idxTo = genericFromEnum (genericTop :: b)

  scheduleEvent ev = map (ev /\ _) $ schedule ev contractTerms

  filtersSchedules :: Event -> Boolean
  filtersSchedules (_ /\ { calculationDay }) = isNothing terminationDate || Just calculationDay <= terminationDate

  postProcessSchedules :: List Event -> List Event
  postProcessSchedules =
    let
      trim :: List Event -> List Event
      trim = dropWhile (\(_ /\ { calculationDay }) -> calculationDay < statusDate)

      regroup :: List Event -> List (NonEmptyList Event)
      regroup = groupBy (\(_ /\ { calculationDay: l }) (_ /\ { calculationDay: r }) -> l == r)

      overwrite :: List (NonEmptyList Event) -> List (NonEmptyList Event)
      overwrite = map (NonEmptyList.sortBy $ comparing $ \(ev /\ _) -> fromEnum ev)
    in
      concat <<< map toList <<< overwrite <<< regroup <<< trim

type Event = EventType /\ ShiftedDay

-- |Generate states
genStates
  :: forall a
   . ActusOps a
  => EuclideanRing a
  =>
  -- | Schedules
  List Event
  ->
  -- | Initial state
  ContractState a
  ->
  -- | New states
  Reader (CtxSTF a) (List (ContractState a))
genStates ((eventType /\ { calculationDay }) : events) state =
  do
    nextState <- stateTransition eventType calculationDay state
    nextStates <- genStates events nextState
    pure (nextState : nextStates)
genStates Nil _ = pure Nil

filtersStates
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ((EventType /\ ShiftedDay) /\ ContractState a)
  -> Reader (CtxSTF a) Boolean
filtersStates ((event /\ { calculationDay }) /\ _) =
  do
    contractTerms@(ContractTerms { contractType, purchaseDate, maturityDate, amortizationDate }) <- _.contractTerms <$> ask
    pure $ case contractType of
      PAM -> isNothing purchaseDate || Just calculationDay >= purchaseDate
      LAM -> isNothing purchaseDate || event == PRD || Just calculationDay > purchaseDate
      NAM -> isNothing purchaseDate || event == PRD || Just calculationDay > purchaseDate
      ANN ->
        let
          b1 = isNothing purchaseDate || event == PRD || Just calculationDay > purchaseDate
          b2 = let m = maturityDate <|> amortizationDate <|> maturity contractTerms in isNothing m || Just calculationDay <= m
        in
          b1 && b2

-- |Generate payoffs
genPayoffs
  :: forall a r
   . ActusOps a
  => EuclideanRing a
  =>
  -- | States with schedule
  List Event
  ->
  -- | States with schedule
  List (ContractState a)
  ->
  -- | Payoffs
  Reader (CtxPOF a r) (List a)
genPayoffs events states = traverse calculatePayoff $ zip events states
  where
  calculatePayoff ((event /\ { calculationDay }) /\ state) = payoff (event /\ calculationDay) state
