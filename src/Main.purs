module Main where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Component.Types (ConfigurationError(RuntimeNotResponding), ContractJsonString(..), Page(..))
import Contrib.Cardano (Slotting(..))
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Effect as Effect
import Contrib.Fetch (fetchEither)
import Contrib.JsonBigInt as JsonBigInt
import Contrib.LZString (decompressFromURI)
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
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
import Foreign (Foreign)
import Foreign.NullOrUndefined (null) as Foreign
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Client (uriOpts)
import Marlowe.Runtime.Web.Types (HealthCheck(..), NetworkId(..), NetworkMagic(..), ServerURL(..))
import Parsing as Parsing
import Partial.Unsafe (unsafePartial)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import URI (RelativeRef(..), URI(..)) as URI
import URI.Extra.QueryPairs (QueryPairs(..)) as URI
import URI.URIRef as URIRef
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Element (getAttribute, setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.History (DocumentTitle(..))
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window (alert, document)
import Web.HTML.Window as Window

type BuildConfig =
  { marloweWebServerUrl :: Maybe ServerURL
  , develMode :: Boolean
  }

decodeBuildConfig :: JsonParser BuildConfig
decodeBuildConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .:? "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  pure
    { marloweWebServerUrl: ServerURL <$> marloweWebServerUrl
    , develMode
    }

type Config =
  { marloweWebServerUrl :: ServerURL
  , develMode :: Boolean
  }

decodeConfig :: JsonParser Config
decodeConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .: "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  pure
    { marloweWebServerUrl: ServerURL marloweWebServerUrl
    , develMode
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
          compressedContractJsonString <- (join $ Foldable.lookup "contract" queryPairs)
          contractJsonString <- decompressFromURI compressedContractJsonString
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

configURL :: String
configURL = "/config.json"

-- FIXME:
-- Currently `setPage` is triggered bottom up
-- but it should be triggered top down from the brower and routing events
-- Introduce: https://github.com/robertdp/purescript-web-router
mkSetPage :: String -> Element -> { setPage :: Page -> Effect Unit, setPageClass :: Page -> Effect Unit }
mkSetPage origClasses appContainer = do
  let
    setPageClass :: Page -> Effect Unit
    setPageClass ContractListPage =
      setAttribute "class" (origClasses <> " contract-list-page") appContainer
    setPageClass (CreateContractPage _) = do
      setAttribute "class" (origClasses <> " create-contract-page") appContainer
    setPageClass LoginPage =
      setAttribute "class" (origClasses <> " login-page") appContainer
    setPageClass OtherPage =
      setAttribute "class" "" appContainer

    setPage :: Page -> Effect Unit
    setPage page = do
      setPageClass page
  { setPage, setPageClass }

main :: Json -> Effect Unit
main configJson = launchAff_ do
  buildConfig <- liftEffect do
    JsonBigInt.patchers.patchStringify
    JsonBigInt.patchers.patchParse
    liftEffect $ Effect.liftEither $ decodeBuildConfig configJson

  let
    throw' msg = liftEffect $ do
      window >>= alert msg
      throw msg

  config <- do
    fetchEither configURL {} [ 200, 404 ] identity >>= case _ of
      Right res -> case res.status, buildConfig of
        404, { marloweWebServerUrl: Just marloweWebServerUrl } ->
          pure { marloweWebServerUrl, develMode: buildConfig.develMode }
        404, _ -> do
          throw' "Incomplete configuration - please create a config.json file in the root of the project - more info in the README"
        200, _ -> do
          possibleConfig <- do
            json <- res.json <#> (unsafeCoerce :: Foreign -> Json)
            pure $ decodeConfig json
          case possibleConfig of
            Left err -> do
              throw' $ "Error parsing '/config.json': " <> show err
            Right config -> pure config
        _, _ -> do
          throw' $ "Unexpected status code fetching '/config.json': " <> show res.status
      Left err -> do
        throw' $ "Error fetching '/config.json': " <> unsafeStringify err
  let
    logger :: String -> Effect Unit
    logger =
      if config.develMode then Console.log
      else const (pure unit)
    runtime@(Marlowe.Runtime.Web.Runtime { serverURL }) = Marlowe.Runtime.Web.runtime config.marloweWebServerUrl

  -- We do this URL processing here because the future URL routing will initialized here as well.
  possibleInitialContract <- liftEffect processInitialURL

  doc :: HTMLDocument <- liftEffect $ document =<< window
  appContainer :: Element <- liftEffect $ maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)

  reactRoot <- liftEffect $ createRoot appContainer

  networkId /\ possibleConfigurationError <- Marlowe.Runtime.Web.getHealthCheck serverURL >>= case _ of
    Left err -> pure ((Testnet (NetworkMagic 1)) /\ Just (RuntimeNotResponding serverURL $ unsafeStringify err))
    Right (HealthCheck { networkId }) -> pure (networkId /\ Nothing)

  let
    -- FIXME: Slotting numbers have to be provided by Marlowe Runtime
    slotting = case networkId of
      Mainnet -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1591566291000" }
      Testnet (NetworkMagic 1) -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1655683200000" }
      _ -> Slotting { slotLength: BigInt.fromInt 1000, slotZeroTime: unsafePartial $ fromJust $ BigInt.fromString "1666656000000" }

  CardanoMultiplatformLib.importLib >>= case _ of
    Nothing -> liftEffect $ logger "Cardano serialization lib loading failed"
    Just cardanoMultiplatformLib -> do
      walletInfoCtx <- liftEffect $ createContext Nothing
      msgHubComponent /\ msgHub <- liftEffect $ mkMessageHub
      let
        mkAppCtx =
          { cardanoMultiplatformLib
          , develMode: config.develMode
          , walletInfoCtx
          , logger
          , msgHub
          , runtime
          , slotting
          }

      origClasses <- liftEffect $ fromMaybe "" <$> getAttribute "class" appContainer
      let
        { setPage, setPageClass } = mkSetPage origClasses appContainer

      liftEffect $ setPageClass LoginPage

      app <- liftEffect $ runReaderT mkApp mkAppCtx
      liftEffect $ renderRoot reactRoot $ msgHubComponent [ app { possibleConfigurationError, possibleInitialContract, setPage } ]
