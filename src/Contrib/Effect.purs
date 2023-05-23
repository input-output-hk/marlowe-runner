module Contrib.Effect where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (throwError)
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

bracket :: forall a b. Effect a -> (a -> Effect Unit) -> (a -> Effect b) -> Effect b
bracket acquire release action = do
  resource <- acquire
  b <- action resource `catchError` \error -> do
    void $ release resource
    throwError error
  release resource
  pure b

liftEither :: forall a m err. MonadEffect m => Show err => Either err a -> m a
liftEither = either (liftEffect <<< throw <<< show) pure
