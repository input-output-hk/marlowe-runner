module Test.Marlowe.Web.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, fromObject, fromString, jsonParser, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Effect.Exception (error)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (ContractHeader, ContractState, ResourceLink, ResourceWithLinks, Tx, TxHeader, GetContractsResponse, decodeResourceWithLink)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "Types" do
    describe "decodeJson" do
      it "ContractHeader" do
        jsonStr <- readTextFile UTF8 "./test/Marlowe/Runtime/Web/contract-headers.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        (contractsWithLinksJson :: Array Json) <- either (throwError <<< error <<< show) pure do
            obj <- decodeJson json
            obj .: "results"

        for_ contractsWithLinksJson \contractWithLinksJson -> do
          let
            contracts :: Either JsonDecodeError GetContractsResponse
            contracts = decodeResourceWithLink (map decodeJson) contractWithLinksJson
          case contracts of
            Left err -> do
              let
                errJson = fromObject $ Object.fromHomogeneous { json: contractWithLinksJson, err: fromString $ show err }
              fail $ stringify errJson
            Right _ -> pure unit

      it "ContractState" do
        jsonStr <- readTextFile UTF8 "./test/Marlowe/Runtime/Web/contract-state.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        let
          contractState :: Either JsonDecodeError (ResourceWithLinks ContractState (transactions :: ResourceLink (Array TxHeader)))
          contractState = decodeResourceWithLink (map decodeJson) json
        case contractState of
          Left err -> do
            let
              errJson = fromObject $ Object.fromHomogeneous { json, err: fromString $ show err }
            fail $ stringify errJson
          Right _ -> pure unit

      it "TxHeader" do
        jsonStr <- readTextFile UTF8 "./test/Marlowe/Runtime/Web/tx-headers.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr
        (txHeadersJsonArr :: Array Json) <- either (throwError <<< error <<< show) pure do
            obj <- decodeJson json
            obj .: "results"

        for_ txHeadersJsonArr \txHeaderJson -> do
          let
            txHeader :: Either JsonDecodeError (ResourceWithLinks TxHeader (transaction :: ResourceLink Tx))
            txHeader = decodeResourceWithLink (map decodeJson) txHeaderJson
          case txHeader of
            Left err -> do
              let
                errJson = fromObject $ Object.fromHomogeneous { json, err: fromString $ show err }
              fail $ stringify errJson
            Right _ -> pure unit

      it "Tx" do
        jsonStr <- readTextFile UTF8 "./test/Marlowe/Runtime/Web/tx.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        let
          tx :: Either JsonDecodeError (ResourceWithLinks Tx (previous :: ResourceLink Tx))
          tx = decodeResourceWithLink (map decodeJson) json
        case tx of
          Left err -> do
            let
              errJson = fromObject $ Object.fromHomogeneous { json, err: fromString $ show err }
            fail $ stringify errJson
          Right _ -> pure unit
