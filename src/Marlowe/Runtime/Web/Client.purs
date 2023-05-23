module Marlowe.Runtime.Web.Client where

import Prelude

import Contrib.Data.Argonaut (JsonParser)
import Contrib.Data.Argonaut.Generic.Record (class DecodeRecord, DecodeJsonFieldFn)
import Contrib.Fetch (FetchError, fetchEither, jsonBody)
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch (RequestMode(..))
import Fetch.Core.Headers (Headers, toArray)
import Marlowe.Runtime.Web.Types (class EncodeHeaders, class EncodeJsonBody, class ToResourceLink, GetContractsResponse, IndexEndpoint(..), PostMerkleizationRequest(..), PostMerkleizationResponse(..), ResourceEndpoint(..), ResourceLink(..), ResourceWithLinks, ResourceWithLinksRow, ServerURL(..), decodeResourceWithLink, encodeHeaders, encodeJsonBody, toResourceLink)
import Prim.Row (class Lacks) as Row
import Record as R
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous) as Row

data ClientError
  = FetchError FetchError
  | ResponseDecodingError JsonDecodeError
  | MerkleizationError

derive instance Generic ClientError _
instance Show ClientError where
  show = genericShow

type GetResourceResponse res = Either ClientError res

allowedStatusCodes :: Array Int
allowedStatusCodes = [ 200, 201, 202, 206 ]

decodeResponse :: forall a. DecodeJson a => JsonParser a
decodeResponse json = do
  obj <- decodeJson json
  res <- obj .: "results"
  decodeJson res

newtype Range = Range String

getResource
  :: forall a extraHeaders
   . DecodeJson a
  => Row.Lacks "Access-Control-Request-Headers" extraHeaders
  => Row.Homogeneous ("Access-Control-Request-Headers" :: String | extraHeaders) String
  => ServerURL
  -> ResourceLink a
  -> { | extraHeaders }
  -> Aff (GetResourceResponse { headers :: Headers, payload :: a, status :: Int })
getResource (ServerURL serverUrl) (ResourceLink path) extraHeaders = do
  let
    url = serverUrl <> "/" <> path

    reqHeaders =
      R.insert (Proxy :: Proxy "Access-Control-Request-Headers") "Range, Accept"
        -- $ R.insert (Proxy :: Proxy "Accept") "application/json"
        $ extraHeaders

  runExceptT do
    let
      decode :: Json -> Either JsonDecodeError a
      decode json = do
        decodeResponse json <|> decodeJson json
    res@{ status, headers: resHeaders } <- ExceptT $ fetchEither url { headers: reqHeaders, mode: Cors } allowedStatusCodes FetchError
    lift (jsonBody res) >>= decode >>> case _ of
      Left err -> throwError (ResponseDecodingError err)
      Right payload -> pure { payload, headers: resHeaders, status }

merkleize
  :: ServerURL
  -> PostMerkleizationRequest
  -> Aff
       ( Either ClientError
           { headers :: Headers
           , payload :: PostMerkleizationResponse
           , status :: Int
           }
       )
merkleize (ServerURL serverUrl) req = runExceptT do
  let
    url = serverUrl <> "/contracts/merkleize"
    body = stringify $ encodeJsonBody req

    headers :: { "Accept" :: String, "Content-Type" :: String }
    headers =
      { "Accept": "application/json"
      , "Content-Type": "application/json"
      }

    decode :: Json -> Either JsonDecodeError PostMerkleizationResponse
    decode json = do
      decodeResponse json <|> decodeJson json

  res@{ status, headers: resHeaders } <- ExceptT $ fetchEither url { method: POST, body, headers } allowedStatusCodes FetchError
  lift (jsonBody res) >>= decode >>> case _ of
    Left err -> throwError (ResponseDecodingError err)
    Right payload -> pure { payload, headers: resHeaders, status }

getPage
  :: forall a
   . DecodeJson a
  => ServerURL
  -> ResourceLink a
  -> Maybe Range
  -> Aff (GetResourceResponse ({ page :: a, nextRange :: Maybe Range }))
getPage serverUrl path possibleRange = runExceptT do
  { headers, payload, status } <- ExceptT
    $ case possibleRange of
        Just range -> getResource serverUrl path { "Range": coerce range }
        Nothing -> getResource serverUrl path {}
  pure
    { page: payload
    , nextRange:
        if status == 206 then map Range $ lookup (CaseInsensitiveString "Next-Range")
          $ fromFoldable
          $ map (lmap CaseInsensitiveString)
          $ toArray headers
        else Nothing
    }

-- TODO generalize
foldMapMContractPages
  :: forall endpoint
   . ToResourceLink endpoint (Array GetContractsResponse)
  => ServerURL
  -> endpoint
  -> Maybe Range
  -> (Array GetContractsResponse -> Aff (Array GetContractsResponse))
  -> Aff (Either ClientError (Array GetContractsResponse))
foldMapMContractPages serverUrl endpoint start f =
  foldMapMPages' serverUrl endpoint (f <<< _.page) start

data FoldPageStep = FetchPage (Maybe Range) | StopFetching

foldMapMPages
  :: forall a b m
   . DecodeJson a
  => MonadAff m
  => Monoid b
  => ServerURL
  -> ResourceLink a
  -> ({ page :: a, currRange :: Maybe Range } -> m b)
  -> Maybe Range
  -> m (GetResourceResponse b)
foldMapMPages serverUrl path f startRange = do
  bs <- runExceptT $ flip unfoldrM (FetchPage startRange) case _ of
    StopFetching -> pure Nothing
    FetchPage currRange -> do
      { page, nextRange } <- ExceptT $ liftAff $ getPage serverUrl path currRange
      b <- lift $ f { page, currRange }
      pure $ Just case nextRange of
        Just _ -> b /\ FetchPage nextRange
        Nothing -> b /\ StopFetching
  pure (fold <$> bs)

getPages
  :: forall a m
   . DecodeJson a
  => MonadAff m
  => ServerURL
  -> ResourceLink a
  -> Maybe Range
  -> m (GetResourceResponse (List { page :: a, currRange :: Maybe Range }))
getPages serverUrl path = foldMapMPages serverUrl path (pure <<< List.singleton)

getPages'
  :: forall endpoint a m
   . DecodeJson a
  => MonadAff m
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> Maybe Range
  -> m (GetResourceResponse (List { page :: a, currRange :: Maybe Range }))
getPages' serverUrl endpoint = getPages serverUrl (toResourceLink endpoint)

getItems
  :: forall f t b
   . DecodeJson b
  => MonadAff f
  => ToResourceLink t b
  => Monoid b
  => ServerURL
  -> t
  -> Maybe Range
  -> f (Either ClientError b)
getItems serverUrl endpoint range = do
  getPages serverUrl (toResourceLink endpoint) range <#> case _ of
    Left err -> Left err
    Right pages -> Right $ foldMap _.page pages

getItems'
  :: forall f endpoint b
   . MonadAff f
  => DecodeJson b
  => ToResourceLink endpoint b
  => Monoid b
  => ServerURL
  -> endpoint
  -> Maybe Range
  -> f (Either ClientError b)
getItems' serverUrl endpoint range = do
  getPages' serverUrl endpoint range <#> case _ of
    Left err -> Left err
    Right pages -> Right $ foldMap _.page pages

getResource'
  :: forall a extraHeaders endpoint
   . DecodeJson a
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Access-Control-Request-Headers" extraHeaders
  => Row.Homogeneous ("Access-Control-Request-Headers" :: String | extraHeaders) String
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> Record extraHeaders
  -> Aff (GetResourceResponse { headers :: Headers, payload :: a, status :: Int })
getResource' serverUrl path = getResource serverUrl (toResourceLink path)

getPage'
  :: forall a endpoint
   . DecodeJson a
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> Maybe Range
  -> Aff (GetResourceResponse ({ page :: a, nextRange :: Maybe Range }))
getPage' serverUrl path = getPage serverUrl (toResourceLink path)

foldMapMPages'
  :: forall a b m t
   . DecodeJson a
  => MonadAff m
  => Monoid b
  => ToResourceLink t a
  => ServerURL
  -> t
  -> ({ currRange :: Maybe Range, page :: a } -> m b)
  -> Maybe Range
  -> m (Either ClientError b)
foldMapMPages' serverUrl path = foldMapMPages serverUrl (toResourceLink path)

post
  :: forall postRequest postResponse postResponseLinks getResponse getResponseLinks extraHeaders
   . DecodeJson postResponse
  => EncodeHeaders postRequest extraHeaders
  => EncodeJsonBody postRequest
  => DecodeRecord (resource :: DecodeJsonFieldFn postResponse) (ResourceWithLinksRow postResponse postResponseLinks)
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks
  -> postRequest
  -> Aff (GetResourceResponse (ResourceWithLinks postResponse postResponseLinks))
post (ServerURL serverUrl) (IndexEndpoint (ResourceLink path)) req = runExceptT do
  let
    url = serverUrl <> "/" <> path
    body = stringify $ encodeJsonBody req

    headers :: {"Content-Type" :: String | extraHeaders }
    headers =
      -- R.insert (Proxy :: Proxy "Accept") "application/json"
        R.insert (Proxy :: Proxy "Content-Type") "application/json"
          $ (encodeHeaders req :: { | extraHeaders })

  response <- ExceptT $ fetchEither url { method: POST, body, headers } allowedStatusCodes FetchError
  (lift (jsonBody response)) >>= decodeResourceWithLink (map decodeJson :: Maybe _ -> Maybe _) >>> case _ of
    Left err -> throwError (ResponseDecodingError err)
    Right payload -> pure payload

post'
  :: forall t postRequest postResponse postResponseLinks getResponse getResponseLinks extraHeaders
   . Newtype t (IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks)
  => DecodeJson postResponse
  => DecodeRecord (resource :: DecodeJsonFieldFn postResponse) (ResourceWithLinksRow postResponse postResponseLinks)
  => EncodeHeaders postRequest extraHeaders
  => EncodeJsonBody postRequest
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> t
  -> postRequest
  -> Aff (Either ClientError (ResourceWithLinks postResponse postResponseLinks))
post' serverUrl endpoint req = do
  let
    endpoint' = unwrap endpoint
  post serverUrl endpoint' req

put
  :: forall links putRequest getResponse extraHeaders
   . EncodeHeaders putRequest extraHeaders
  => EncodeJsonBody putRequest
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> ResourceEndpoint putRequest getResponse links
  -> putRequest
  -> Aff (Either FetchError Unit)
put (ServerURL serverUrl) (ResourceEndpoint (ResourceLink path)) req = runExceptT do
  let
    url = serverUrl <> "/" <> path
    body = stringify $ encodeJsonBody req

    headers :: { "Content-Type" :: String | extraHeaders }
    headers =
      -- R.insert (Proxy :: Proxy "Accept") "application/json"
      R.insert (Proxy :: Proxy "Content-Type") "application/json"
        $ (encodeHeaders req :: { | extraHeaders })
  void $ ExceptT $ fetchEither url { method: PUT, body, headers } allowedStatusCodes identity

put'
  :: forall links putRequest getResponse extraHeaders t
   . EncodeHeaders putRequest extraHeaders
  => EncodeJsonBody putRequest
  => Newtype t (ResourceEndpoint putRequest getResponse links)
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> t
  -> putRequest
  -> Aff (Either FetchError Unit)
put' serverUrl endpoint req = do
  let
    endpoint' = unwrap endpoint
  put serverUrl endpoint' req
