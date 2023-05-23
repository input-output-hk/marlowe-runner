module Contrib.Polyform.Validator where

import Prelude

import Data.Maybe (Maybe)
import Polyform (Validator)
import Polyform.Validator (liftFnMaybe)

liftMaybe :: forall e i m o. Applicative m => e -> Maybe o -> Validator m e i o
liftMaybe err mv = liftFnMaybe (const err) (const mv)

