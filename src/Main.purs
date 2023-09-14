module Main where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Component.Types (Slotting(..))
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Effect as Effect
import Contrib.JsonBigInt as JsonBigInt
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.BigInt.Argonaut as BigInt
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Types (ServerURL(..))
import Partial.Unsafe (unsafePartial)
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
  , network :: String
  , aboutMarkdown :: String
  }

decodeConfig :: JsonParser Config
decodeConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .: "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  network <- obj .: "network"
  aboutMarkdown <- obj .: "aboutMarkdown"
  pure
    { marloweWebServerUrl: ServerURL marloweWebServerUrl
    , develMode
    , network
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
    -- FIXME: Slotting numbers have to be provided by Marlowe Runtime
    slotting =
      case config.network of
        "mainnet" -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1591566291000" }
        _ -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1666656000000" }

  doc :: HTMLDocument <- document =<< window
  container :: Element <- maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)
  reactRoot <- createRoot container
  launchAff_ do

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
            , msgHub
            , runtime
            , aboutMarkdown: config.aboutMarkdown
            , slotting
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ msgHubComponent [ app unit ]
