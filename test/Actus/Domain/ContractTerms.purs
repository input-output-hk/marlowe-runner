module Test.Actus.Domain.ContractTerms where

import Prelude

import Actus.Domain (ContractTerms)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, jsonParser)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..), either)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "Actus.Domain.ContractTerms" do
    describe "decodeJson" do
      it "actus-tests-lam.json" do
        jsonStr <- readTextFile UTF8 "./test/Actus/Domain/actus-tests-lam.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        (fixtures :: Object Json) <- either (throwError <<< error <<< show) pure do
          decodeJson json >>= traverse \lamJson -> do
            obj <- decodeJson lamJson
            termsJson <- obj .: "terms"
            pure $ termsJson

        forWithIndex_ fixtures \lamId termsJson -> do
          let
            (terms :: Either JsonDecodeError ContractTerms) = decodeJson termsJson
          case terms of
            Left err -> fail (lamId <> ": " <> show err)
            Right _ -> pure unit

      pending "feature complete"
