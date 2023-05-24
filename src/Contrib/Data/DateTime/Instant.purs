module Contrib.Data.DateTime.Instant where

import Prelude

import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Int as Int
import Data.Interval (Duration(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import JS.Unsafe.Stringify (unsafeStringify)
import Partial.Unsafe (unsafeCrashWith)

unsafeInstant :: Milliseconds -> Instant
unsafeInstant t = case instant t of
    Just i -> i
    Nothing -> unsafeCrashWith $ "Contrib.Data.DateTime.Instant.unsafeInstant: invalid instant value:" <> unsafeStringify t

unsafeInstantFromInt :: Int -> Instant
unsafeInstantFromInt = unsafeInstant <<< Milliseconds <<< Int.toNumber

millisecondsFromNow :: Milliseconds -> Effect Instant
millisecondsFromNow d = do
  n <- unInstant <$> now
  pure $ unsafeInstant $ n <> d

