module Runner.Contrib.Effect.Aff where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff, delay)

withTimeout :: forall a. Milliseconds -> Aff a -> Aff (Maybe a)
withTimeout milliseconds action = do
  sequential $ parallel (delay milliseconds $> Nothing) <|> parallel (Just <$> action)

-- Ignore the fact if the action timed out or not
withTimeout_ :: Milliseconds -> Aff Unit -> Aff Unit
withTimeout_ milliseconds action = do
  sequential $ parallel action <|> parallel (delay milliseconds)
