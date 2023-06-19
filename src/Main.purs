module Main where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Effect as Effect
import Contrib.JsonBigInt as JsonBigInt
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Foreign.Object (Object)
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Streaming (MaxPages(..), PollingInterval(..), RequestInterval(..))
import Marlowe.Runtime.Web.Streaming as Streaming
import Marlowe.Runtime.Web.Types (BlockHeader(..), BlockNumber(..), ContractHeader(..), ServerURL(..))
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Config =
  { marloweWebServerUrl :: ServerURL
  , develMode :: Boolean
  , aboutMarkdown :: String
  }

decodeConfig :: JsonParser Config
decodeConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .: "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  aboutMarkdown <- obj .: "aboutMarkdown"
  pure
    { marloweWebServerUrl: ServerURL marloweWebServerUrl
    , develMode
    , aboutMarkdown
    }

main :: Json -> Effect Unit
main configJson = do
  config <- Effect.liftEither $ decodeConfig configJson

  JsonBigInt.patchers.patchStringify
  JsonBigInt.patchers.patchParse

  let
    logger :: String -> Effect Unit
    logger =
      if config.develMode then Console.log
      else const (pure unit)
    runtime = Marlowe.Runtime.Web.runtime config.marloweWebServerUrl

  doc :: HTMLDocument <- document =<< window
  container :: Element <- maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)
  reactRoot <- createRoot container
  launchAff_ do
    contractStream <- do
      let
        reqInterval = RequestInterval (Milliseconds 50.0)
        pollInterval = PollingInterval (Milliseconds 120_000.0)
        filterContracts getContractResponse = case un ContractHeader getContractResponse.resource of
          { block: Nothing } -> true
          -- { block: Just (BlockHeader { blockNo: BlockNumber blockNo }) } -> blockNo >= 798887
          { block: Just (BlockHeader { blockNo: BlockNumber blockNo }) } -> blockNo > 882910 -- 877295 -- 798887
        maxPages = Just (MaxPages 1)
      Streaming.mkContractsWithTransactions pollInterval reqInterval filterContracts maxPages config.marloweWebServerUrl

    CardanoMultiplatformLib.importLib >>= case _ of
      Nothing -> liftEffect $ logger "Cardano serialization lib loading failed"
      Just cardanoMultiplatformLib -> do
        walletInfoCtx <- liftEffect $ createContext Nothing
        msgHubComponent /\ msgHub <- liftEffect $ mkMessageHub
        let
          mkAppCtx =
            { cardanoMultiplatformLib
            , walletInfoCtx
            , logger
            , contractStream
            , msgHub
            , runtime
            , aboutMarkdown: config.aboutMarkdown
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ msgHubComponent [ app unit ]
