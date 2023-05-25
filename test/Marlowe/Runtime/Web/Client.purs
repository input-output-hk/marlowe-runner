module Test.Marlowe.Web.Client where

import Prelude

import Contrib.Bip39 as Bip39
import Data.Array as Array
import Data.String as String
import Debug (traceM)
import Effect.Class (liftEffect)
import Marlowe.Runtime.Web.Types (ServerURL(..))
import Test.Spec (Spec, describe, it)

spec :: ServerURL -> Spec Unit
spec serverUrl@(ServerURL serverUrlStr) = do
  describe ("The client" <> serverUrlStr) do
    it "Accepts signed transaction" do
      mnemonicStr <- liftEffect $ Bip39.generateMnemonic Bip39.strength."192" <#> Bip39.mnemonicToString
      traceM $ "mnemonic: " <> mnemonicStr
      traceM $ "mnemonic length: " <> show (Array.length (String.split (String.Pattern " ") mnemonicStr))

    -- it "POST contract correctly" do
    --    let
    --      -- addr = Address "addr_xvk12wjl5zcq8dd4q7he36667aqvcwm9sjhqpk3vyu625g3tcfex5sckf35hyu3vnhveyqrqvtrvff6m0jqu6xfus5lx5att4h2g7pteqrgu04hjs"
    --      -- addr = Address "00bf05a62e0a25a1cde8b6f3b5b0d33ea60fde9a9ec8f615169493c7a90f1e33e7772682a03adde020ba989d97339c9b3f32a516aa056a9c7c"
    --      -- addr = Address "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"
    --      addr = unsafeBech32 "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"
    --      req = PostContractsRequest
    --        { metadata: mempty
    --        -- , version :: MarloweVersion
    --        -- , roles :: Maybe RolesConfig
    --        , contract: V1.Close
    --        , minUTxODeposit: Lovelace (BigInt.fromInt 2_000_000)
    --        , changeAddress: addr
    --        , addresses: [addr]
    --        , collateralUTxOs: []
    --        }
    --    post' serverUrl api req >>= case _ of
    --      Right _ -> do
    --        pure unit
    --      Left (FetchError (InvalidStatusCode res)) -> do
    --        traceM "STATUS CODE ERROR"
    --        traceM $ res.status
    --        traceM $ res.statusText
    --        body <- res.text
    --        traceM "BODY:"
    --        traceM body

    --      Left err -> do
    --         traceM "Other error"
    --         traceM err
    --         pure unit
    --        -- fail $ "Error: " <> show err

    -- it "POST contract with metadata" do
    --   jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/ex_pam1.json"
    --   json <- either (throwError <<< error) pure $ jsonParser jsonStr

    --   let
    --     (terms :: Either JsonDecodeError ContractTerms) = decodeJson json
    --     -- addr1 = V1.Address "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"
    --     -- addr2 = V1.Address "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"
    --     -- nami preview
    --     -- addr1 = V1.Address "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"
    --     -- yoroi preprod
    --     addr1 = V1.Address "addr_test1qqe94c7z039ceta3xevcagwwh0l8ahmy90883nqm5edknmyhwefmaav7gfzuuck7c27y6fdp4vzgezrmmts3x3jp989s5f6lqr"
    --     addr2 = V1.Address "addr_test1qrwl8cukwn7tazx5aee4ynzgj0edp6un878htr5fpgmjk3yhwefmaav7gfzuuck7c27y6fdp4vzgezrmmts3x3jp989se3tc7f"

    --   case terms of
    --     Left err -> fail ("Parsing error: " <> show err)
    --     Right contract -> do
    --       let
    --         metadataJson = encodeJson $ Metadata { contractTerms: contract, party: addr1, counterParty: addr2 }
    --         addr = unsafeBech32 "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"
    --         cashflows = genProjectedCashflows (addr1 /\ addr2) (defaultRiskFactors contract) contract
    --         marloweContract = genContract contract cashflows
    --         req = PostContractsRequest
    --           { metadata: RT.Metadata $ Map.singleton actusMetadataKey metadataJson
    --           -- , version :: MarloweVersion
    --           -- , roles :: Maybe RolesConfig
    --           , contract: marloweContract
    --           , minUTxODeposit: Lovelace (BigInt.fromInt 2_000_000)
    --           , changeAddress: addr
    --           , addresses: [ addr ]
    --           , collateralUTxOs: []
    --           }
    --       post' serverUrl api req >>= case _ of
    --         Right ({ resource: PostContractsResponseContent res }) -> do
    --           traceM res
    --           pure unit
    --         Left (FetchError (InvalidStatusCode res)) -> do
    --           traceM "STATUS CODE ERROR"
    --           traceM $ res.status
    --           traceM $ res.statusText
    --           body <- res.text
    --           traceM "BODY:"
    --           traceM body
    --         Left _ -> do
    --           traceM "OTHER error"
    --           pure unit

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
