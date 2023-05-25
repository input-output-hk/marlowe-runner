module Contrib.Language.Marlowe.Core.V1 where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

compareMarloweJsonKeys :: String -> String -> Ordering
compareMarloweJsonKeys = do
  let
    -- Tiny optimization - let's cache in the closure the ordering
    marloweKeysOrdering :: Map String Int
    marloweKeysOrdering = Map.fromFoldable $ mapWithIndex (flip (/\))
      [
      -- deposit:
        "party"
      , "deposits"
      , "of_token"
      , "into_account"
      -- when:
      , "when"
      , "timeout"
      , "timeout_continuation"
      -- choice:
      , "for_choice"
      , "choose_between"
      -- pay:
      , "pay"
      , "token"
      , "from_account"
      , "to"
      , "then"
      ]
  \a b -> do
    let
      possibleOrdering = do
        aV <- Map.lookup a marloweKeysOrdering
        bV <- Map.lookup b marloweKeysOrdering
        pure $ compare aV bV
    -- Lazily compute the fallback, default ordering
    case possibleOrdering of
      Just ordering -> ordering
      Nothing -> compare a b

