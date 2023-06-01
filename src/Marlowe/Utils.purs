module Marlowe.Utils where

import Prelude

import Language.Marlowe.Core.V1.Folds (MapStep(..), foldMapContract)
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId(..), Contract(..), Observation(..), Party(..), Payee(..), TokenName, Value(..))

mapContract :: Contract -> Array TokenName
mapContract (Pay (Role t1) (Party (Role t2)) _ _ _) = [ t1, t2 ]
mapContract (Pay _ (Party (Role t)) _ _ _) = [ t ]
mapContract (Pay (Role t) _ _ _ _) = [ t ]
mapContract _ = []

mapObservation :: Observation -> Array TokenName
mapObservation (ChoseSomething (ChoiceId _ (Role t))) = [ t ]
mapObservation _ = []

mapValue :: Value -> Array TokenName
mapValue (AvailableMoney (Role t) _) = [ t ]
mapValue (ChoiceValue (ChoiceId _ (Role t))) = [ t ]
mapValue _ = []

getRoleTokenStep :: MapStep (Array TokenName)
getRoleTokenStep = MapStep
  { mapCase: const mempty
  , mapContract
  , mapObservation
  , mapValue
  }

allRolesInContract :: Contract -> Array TokenName
allRolesInContract = foldMapContract getRoleTokenStep
