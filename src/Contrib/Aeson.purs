module Contrib.Aeson where

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

type Reviver = String -> Json -> Json

type ResultHandlers a = { failure :: String -> a, success :: Json -> a}

newtype JsonString = JsonString String

foreign import parseImpl :: forall a. Fn3 (ResultHandlers a) Reviver JsonString a

eitherHandlers :: ResultHandlers (Either String Json)
eitherHandlers = { failure: Left, success: Right }

parse :: Reviver -> JsonString -> Either String Json
parse = runFn3 parseImpl eitherHandlers
