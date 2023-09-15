module Main where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.CreateContract (ContractJsonString(..))
import Component.MessageHub (mkMessageHub)
import Component.Types (Slotting(..))
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Effect as Effect
import Contrib.JsonBigInt as JsonBigInt
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..), hush)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Foreign.NullOrUndefined (null) as Foreign
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Client (uriOpts)
import Marlowe.Runtime.Web.Types (HealthCheck(..), NetworkId(..), ServerURL(..))
import Parsing as Parsing
import Partial.Unsafe (unsafePartial)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import URI (RelativeRef(..), URI(..)) as URI
import URI.Extra.QueryPairs (QueryPairs(..)) as URI
import URI.URIRef as URIRef
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.History (DocumentTitle(..))
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window (document)
import Web.HTML.Window as Window

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

-- We extract a possible contract json from the URL here:
processInitialURL :: Effect (Maybe ContractJsonString)
processInitialURL = do
  location <- window >>= Window.location
  href <- Location.href location
  let
    possibleUriRef = Parsing.runParser href (URIRef.parser uriOpts)
    href' /\ possibleContract = fromMaybe (href /\ Nothing) do
      uriRef <- hush $ possibleUriRef
      let
        extractContractJson possibleOrigQuery = do
          URI.QueryPairs queryPairs <- possibleOrigQuery
          contractJsonString <- join $ Foldable.lookup "contract" queryPairs
          let
            queryPairs' = Array.filter ((/=) "contract" <<< fst) queryPairs
          pure (URI.QueryPairs queryPairs' /\ ContractJsonString contractJsonString)

      uriRef' /\ c <- case uriRef of
        Right (URIRef.RelativeRef relativePart query fragment) -> do
          query' /\ contractJsonString <- extractContractJson query
          pure (Right (URI.RelativeRef relativePart (Just query') fragment) /\ contractJsonString)
        Left (URI.URI scheme hp query fragment) -> do
          query' /\ contractJsonString <- extractContractJson query
          pure (Left (URI.URI scheme hp (Just query') fragment) /\ contractJsonString)
      pure (URIRef.print uriOpts uriRef' /\ Just c)
  -- Location.setHref href' location
  when (href' /= href) do
    w <- window
    history <- Window.history w
    title <- Window.document w >>= HTMLDocument.title
    History.replaceState Foreign.null (DocumentTitle title) (History.URL href') history

  pure possibleContract

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
    runtime@(Marlowe.Runtime.Web.Runtime { serverURL }) = Marlowe.Runtime.Web.runtime config.marloweWebServerUrl

  -- We do this URL processing here because the future URL routing will initialized here as well.
  possibleInitialContract <- processInitialURL

  doc :: HTMLDocument <- document =<< window
  container :: Element <- maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)
  reactRoot <- createRoot container
  launchAff_ do
    HealthCheck { networkId } <- Marlowe.Runtime.Web.getHealthCheck serverURL >>= case _ of
      Left err -> liftEffect $ throw $ unsafeStringify err
      Right healthCheck -> pure healthCheck

    let
      -- FIXME: Slotting numbers have to be provided by Marlowe Runtime
      slotting = case networkId of
          Mainnet -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1591566291000" }
          _ -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1666656000000" }


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
        liftEffect $ renderRoot reactRoot $ msgHubComponent [ app { possibleInitialContract } ]
