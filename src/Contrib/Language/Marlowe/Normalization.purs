module Contrib.Language.Marlowe.Normalization where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types as V1

fix :: forall a. Eq a => (a -> a) -> a -> a
fix f = tailRec \a -> let result = f a in if result == a then Done a else Loop result

-- | Rewrite rules for Value
rewriteValue :: V1.State -> V1.Value -> V1.Value
rewriteValue state@(V1.State s) =
  let
    loop :: V1.Value -> V1.Value
    loop = case _ of
      v@(V1.Constant _) -> v
      v@V1.TimeIntervalStart -> v
      v@V1.TimeIntervalEnd -> v
      v@(V1.UseValue var) -> maybe v V1.Constant $ Map.lookup var s.boundValues
      v@(V1.ChoiceValue var) -> maybe v V1.Constant $ Map.lookup var s.choices
      v@(V1.AvailableMoney account token) -> maybe v V1.Constant $ Map.lookup (account /\ token) s.accounts
      V1.AddValue (V1.Constant a) (V1.Constant b) -> V1.Constant (a + b)
      V1.AddValue a b -> V1.AddValue (loop a) (loop b)
      V1.SubValue (V1.Constant a) (V1.Constant b) -> V1.Constant (a - b)
      V1.SubValue a b -> V1.SubValue (loop a) (loop b)
      V1.MulValue (V1.Constant a) (V1.Constant b) -> V1.Constant (a * b)
      V1.MulValue a b -> V1.MulValue (loop a) (loop b)
      V1.DivValue (V1.Constant a) (V1.Constant b) -> V1.Constant (a / b)
      V1.DivValue a b -> V1.DivValue (loop a) (loop b)
      V1.NegValue (V1.Constant a) -> V1.Constant (-a)
      V1.NegValue a -> V1.NegValue (loop a)
      V1.Cond V1.TrueObs a _ -> loop a
      V1.Cond V1.FalseObs _ b -> loop b
      V1.Cond obs a b -> V1.Cond (rewriteObservation state obs) (loop a) (loop b)
  in
    fix loop

-- | Rewrite rules for Observation
rewriteObservation :: V1.State -> V1.Observation -> V1.Observation
rewriteObservation state@(V1.State s) =
  let
    loop :: V1.Observation -> V1.Observation
    loop = case _ of
      V1.AndObs V1.TrueObs b -> loop b
      V1.AndObs V1.FalseObs _ -> V1.FalseObs
      V1.AndObs a b -> V1.AndObs (loop a) (loop b)
      V1.OrObs V1.FalseObs b -> loop b
      V1.OrObs V1.TrueObs _ -> V1.TrueObs
      V1.OrObs a b -> V1.OrObs (loop a) (loop b)
      V1.NotObs V1.TrueObs -> V1.FalseObs
      V1.NotObs V1.FalseObs -> V1.TrueObs
      V1.NotObs a -> V1.NotObs (loop a)
      V1.ChoseSomething var
        | Map.member var s.choices -> V1.TrueObs
        | otherwise -> V1.FalseObs
      V1.ValueGE (V1.Constant a) (V1.Constant b)
        | a >= b -> V1.TrueObs
        | otherwise -> V1.FalseObs
      V1.ValueGE a b -> V1.ValueGE (rewriteValue state a) (rewriteValue state b)
      V1.ValueGT (V1.Constant a) (V1.Constant b)
        | a > b -> V1.TrueObs
        | otherwise -> V1.FalseObs
      V1.ValueGT a b -> V1.ValueGT (rewriteValue state a) (rewriteValue state b)
      V1.ValueLT (V1.Constant a) (V1.Constant b)
        | a < b -> V1.TrueObs
        | otherwise -> V1.FalseObs
      V1.ValueLT a b -> V1.ValueLT (rewriteValue state a) (rewriteValue state b)
      V1.ValueLE (V1.Constant a) (V1.Constant b)
        | a <= b -> V1.TrueObs
        | otherwise -> V1.FalseObs
      V1.ValueLE a b -> V1.ValueLE (rewriteValue state a) (rewriteValue state b)
      V1.ValueEQ (V1.Constant a) (V1.Constant b)
        | a == b -> V1.TrueObs
        | otherwise -> V1.FalseObs
      V1.ValueEQ a b -> V1.ValueEQ (rewriteValue state a) (rewriteValue state b)
      v@V1.TrueObs -> v
      v@V1.FalseObs -> v
  in
    fix loop

-- | Rewrite rules for Case
rewriteCase :: V1.State -> V1.Case -> V1.Case
rewriteCase state =
  let
    loop :: V1.Case -> V1.Case
    loop = case _ of
      V1.Case action contract -> V1.Case action (rewriteContract state contract)
      v@(V1.MerkleizedCase _ _) -> v
  in
    fix loop

-- | Rewrite rules for Contract
rewriteContract :: V1.State -> V1.Contract -> V1.Contract
rewriteContract state@(V1.State s) =
  let
    loop :: V1.Contract -> V1.Contract
    loop = case _ of
      v@V1.Close -> v
      v@(V1.Assert _ _) -> v
      V1.Pay accountid payee token val a -> V1.Pay accountid payee token (rewriteValue state val) (loop a)
      V1.If V1.TrueObs a _ -> loop a
      V1.If V1.FalseObs _ b -> loop b
      V1.If obs a b -> V1.If (rewriteObservation state obs) (loop a) (loop b)
      -- TODO 😎 This needs to be cooler:
      V1.When cases t a -> V1.When (rewriteCase state <$> cases) t (loop a)
      V1.Let var (V1.Constant val) a -> rewriteContract (V1.State s { boundValues = Map.insert var val s.boundValues }) a
      V1.Let var val a -> V1.Let var (rewriteValue state val) (loop a)
  in
    fix loop

{- DEMO
spago repl

exit
:q
:r

:paste
t = unsafePartial $ fromJust (Date.canonicalDate <$> (toEnum 2023) <*> (toEnum 6) <*> (toEnum 1))
state = State { accounts: Map.empty, choices: Map.empty, boundValues: Map.empty, minTime: fromDate t }
c = Constant <<< fromInt
ada = Token "" ""

exampleContract1 :: Contract
exampleContract1 =
  If (AndObs (ValueGT (c 2) (c 1)) FalseObs)
    (Pay (Role "A") (Party (Role "B")) ada (AddValue (c 5) (c 4)) Close)
    (Pay (Role "C") (Party (Role "D")) ada (AddValue (c 1) (c 2)) Close)

exampleContract2 :: Contract
exampleContract2 =
  Let (ValueId "x") (c 4)
    (Pay (Role "A") (Party (Role "B")) ada (AddValue (c 5) (UseValue (ValueId "x"))) Close)

rewriteContract state exampleContract1
rewriteContract state exampleContract2
-}
