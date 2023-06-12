module Contrib.Data.Argonaut.Traversals where

import Prelude

import Data.Argonaut (Json, caseJsonArray, caseJsonObject, fromArray, fromObject)
import Data.Traversable (traverse)

traverseJson :: forall f. Monad f => (Json -> f Json) -> Json -> f Json
traverseJson f = do
  let
    traverseArray = do
      let
        go arr = fromArray <$> (traverse f arr)
      \json -> caseJsonArray (pure json) go json
    traverseObject = do
      let
        go obj = fromObject <$> (traverse f obj)
      \json -> caseJsonObject (pure json) go json
  traverseObject <=< traverseArray

rewriteBottomUp :: forall m. Monad m => (Json -> m Json) -> Json -> m Json
rewriteBottomUp f = do
  let
    visitor json = do
      json' <- traverseJson visitor json
      f json'
  visitor

rewriteTopDown :: forall m. Monad m => (Json -> m Json) -> Json -> m Json
rewriteTopDown f = do
  let
    visitor json = do
      json' <- f json
      traverseJson visitor json'
  visitor
