module Test.Marlowe.Web.Client where

import Prelude

import CardanoMultiplatformLib.Types (unsafeBech32)
import Component.ContractTemplates.Escrow as Escrow
import Contrib.Bip39 as Bip39
import Contrib.Data.DateTime.Instant (unsafeInstant)
import Contrib.Fetch as Fetch
import Control.Monad.Except (throwError)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, jsonParser, stringify)
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (Millisecond)
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Common as String
import Data.Time.Duration (Milliseconds(..))
import Debug (traceM)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Language.Marlowe.Core.V1.Semantics.Types (Ada(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError(..), post')
import Marlowe.Runtime.Web.Types (ApiError(..), MarloweVersion(..), Metadata(..), PostContractsRequest(..), PostContractsResponseContent(..), ServerURL(..), api)
import Marlowe.Runtime.Web.Types as RT
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Unit (walkSuite)
import Unsafe.Coerce (unsafeCoerce)

bigInt :: _
bigInt = BigInt.fromInt

bound :: Int -> Int -> V1.Bound
bound l u = V1.Bound (bigInt l) (bigInt u)

handlePostContractError = case _ of
  (FetchError (Fetch.InvalidStatusCode res) :: ClientError String) -> do
    traceM "STATUS CODE ERROR"
    traceM $ res.status
    traceM $ res.statusText
    body <- res.text
    traceM "BODY:"
    traceM body
    fail $ String.joinWith "\n"
      [ "Status: " <> show res.status
      , "Status text: " <> res.statusText
      , "Body: " <> body
      ]
  FetchError (Fetch.InvalidBodyEncoding { body, parsingError }) -> do
    traceM "FETCH ERROR"
    traceM $ "Body: " <> body
    traceM $ "Error: " <> show parsingError
    fail $ "Error: " <> show parsingError
  FetchError fetchErr -> do
    traceM "FETCH ERROR"
    traceM $ "Error: " <> show fetchErr
    fail $ "Error: " <> show fetchErr
  ResponseDecodingError json jsonErr -> do
    traceM "RESPONSE DECODING ERROR"
    traceM $ "Error: " <> stringify json
    fail $ "Error: " <> show jsonErr
  HealthCheckError msg -> do
    traceM "HEALTH CHECK ERROR"
    traceM $ "Message: " <> msg
    fail $ "Message: " <> msg
  MerkleizationError -> do
    traceM "MERKLEIZATION ERROR"
    fail "Merkleization error"
  -- data ClientError err
  --   = FetchError FetchError
  --   | ResponseDecodingError JsonDecodeError
  --   | HealthCheckError String
  --   | MerkleizationError
  --   | ServerApiError (ApiError err)
  -- newtype ApiError error = ApiError
  --   { message :: String
  --   , error :: error
  --   , details :: Json
  --   }
  ServerApiError { error: ApiError apiErr } -> do
    traceM "SERVER API ERROR"
    traceM $ "Message: " <> apiErr.message
    traceM $ "Error: " <> apiErr.error
    traceM $ "Details: " <> stringify apiErr.details
    fail $ String.joinWith "\n"
      [ "Message: " <> apiErr.message
      , "Error: " <> show apiErr.error
      , "Details: " <> stringify apiErr.details
      ]

spec :: ServerURL -> Spec Unit
spec serverUrl@(ServerURL serverUrlStr) = do
  describe ("The client" <> serverUrlStr) do
    it "Accepts signed transaction" do
      mnemonicStr <- liftEffect $ Bip39.generateMnemonic Bip39.strength."192" <#> Bip39.mnemonicToString
      traceM $ "mnemonic: " <> mnemonicStr
      traceM $ "mnemonic length: " <> show (Array.length (String.split (String.Pattern " ") mnemonicStr))

    it "POST contract correctly" do
      let
        -- addr = Address "addr_xvk12wjl5zcq8dd4q7he36667aqvcwm9sjhqpk3vyu625g3tcfex5sckf35hyu3vnhveyqrqvtrvff6m0jqu6xfus5lx5att4h2g7pteqrgu04hjs"
        -- addr = Address "00bf05a62e0a25a1cde8b6f3b5b0d33ea60fde9a9ec8f615169493c7a90f1e33e7772682a03adde020ba989d97339c9b3f32a516aa056a9c7c"
        -- addr = Address "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"
        addr = unsafeBech32 "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"
        req = PostContractsRequest
         { metadata: mempty
         -- , version :: MarloweVersion
         -- , roles :: Maybe RolesConfig
         , contract: V1.Close
         , minUTxODeposit: Lovelace (BigInt.fromInt 2_000_000)
         , changeAddress: addr
         , addresses: [addr]
         , collateralUTxOs: []
         , roles: Nothing
         , tags: mempty
         , accounts: {}
         }

      post' serverUrl api req >>= case _ of
        Right { resource: PostContractsResponseContent res } -> do
          traceM res
          pure unit

        Right { resource: PostContractsResponseSafetyErrors errors } -> do
          traceM "SAFETY ERRORS:"
          traceM errors
          fail $ "Safety errors"
        Left err ->
          handlePostContractError err

  it "POST contract with metadata" do
    let
      fakeTimeout = unsafeInstant (Milliseconds 1_000_000.0)
      buyerAddress = "addr_test1qqe94c7z039ceta3xevcagwwh0l8ahmy90883nqm5edknmyhwefmaav7gfzuuck7c27y6fdp4vzgezrmmts3x3jp989s5f6lqr"
      sellerAddress = "addr_test1qrwl8cukwn7tazx5aee4ynzgj0edp6un878htr5fpgmjk3yhwefmaav7gfzuuck7c27y6fdp4vzgezrmmts3x3jp989se3tc7f"
      mediatorAddress = "addr_test1qrwl8cukwn7tazx5aee4ynzgj0edp6un878htr5fpgmjk3yhwefmaav7gfzuuck7c27y6fdp4vzgezrmmts3x3jp989se3tc7f"

      marloweContract = Escrow.mkAddressBasedEscrowContract
        { price: bigInt 1_000_000
        , paymentDeadline: fakeTimeout
        , complaintResponseDeadline: fakeTimeout
        , complaintDeadline: fakeTimeout
        , mediationDeadline: fakeTimeout
        , buyerAddress
        , sellerAddress
        , mediatorAddress
        }
      addr = unsafeBech32 "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"
      req = PostContractsRequest
        { metadata: RT.Metadata $ Map.singleton 1698 (Argonaut.fromString "Marlowe contract with metadata")
        , tags: mempty
        , roles: Nothing
        , contract: marloweContract
        , minUTxODeposit: Lovelace (BigInt.fromInt 2_000_000)
        , changeAddress: addr
        , addresses: [ addr ]
        , collateralUTxOs: []
        , accounts: {}
        }
    post' serverUrl api req >>= case _ of
      Right res -> do
        traceM res
        pure unit
      Left error -> handlePostContractError error

-- it "GET contracts" do
--  contracts <- getItems' serverUrl api Nothing `catchError` \err -> do
--    log "Get contracts error: "
--    log $ unsafeStringify err
--    throwError err

--  (hush contracts >>= Array.head) # case _ of
--    Just getContractsResponse -> do
--      traceM getContractsResponse
--      pure unit
--      -- contract <- fetchContract serverUrl contractHeader.links.contract
--      -- transactionHeaders <- fetchTransactionHeaders serverUrl contract.links.transactions
--      -- case head transactionHeaders of
--      --  Just transactionHeader -> do
--      --     transaction <- fetchTransaction serverUrl transactionHeader.links.transaction
--      --     let (Tx tx) = transaction.resource
--      --     case tx.block of
--      --            Just _ -> pure unit
--      --            _ -> fail "Expected block"
--      --  _ -> fail "Expected transaction"
--    _ -> fail "Expected contract"

-- it "GET transactions" do
--  (contracts :: Array GetContractsResponse) <- getItems' serverUrl api Nothing >>= Effect.liftEither

--  let
--    contracts' = Array.catMaybes $ contracts <#> \c@{ resource, links } -> do
--      transactions <- links.transactions
--      pure $ c { links { transactions = transactions } }

--  void $ for contracts' \{ links } -> do
--    txs <- getItems' serverUrl links.transactions Nothing
--    case txs of
--      Right txs' -> do
--        traceM $ "Transactions: " <> show (Array.length txs')
--        pure unit
--      Left (FetchError (InvalidStatusCode res)) -> do
--        traceM $ "Invalid status code: " <> show res.status
--        body <- res.text
--        traceM $ "error body" <> body
--        pure unit
--      _ -> pure unit
--    traceM txs
--    traceM "transactions fetched correctly"

-- (hush contracts >>= List.head) # case _ of
--       Just getContractsResponse -> do
--         traceM getContractsResponse
--         pure unit
--         -- contract <- fetchContract serverUrl contractHeader.links.contract
--         -- transactionHeaders <- fetchTransactionHeaders serverUrl contract.links.transactions
--         -- case head transactionHeaders of
--         --  Just transactionHeader -> do
--         --     transaction <- fetchTransaction serverUrl transactionHeader.links.transaction
--         --     let (Tx tx) = transaction.resource
--         --     case tx.block of
--         --            Just _ -> pure unit
--         --            _ -> fail "Expected block"
--         --  _ -> fail "Expected transaction"
--       _ -> fail "Expected contract"

