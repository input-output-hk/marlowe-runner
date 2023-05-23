module Contrib.Fetch where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Promise as Promise
import Data.Argonaut (Json)
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Fetch.Core as Core
import Fetch.Core.Request as CoreRequest
import Fetch.Internal.Request (class ToCoreRequestOptions, HighlevelRequestOptions, new)
import Fetch.Internal.Request as Request
import Fetch.Internal.Response (Response)
import Fetch.Internal.Response as Response
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type StatusCode = Int

data FetchError
  = InvalidStatusCode Response
  | FetchError Error

derive instance Generic FetchError _
instance Show FetchError where
  show (InvalidStatusCode _) = "InvalidStatusCode"
  show (FetchError error) = "FetchError " <> show error

fetchEither
  :: forall input output thruIn thruOut headers err
   . Union input thruIn (HighlevelRequestOptions headers String)
  => Union output thruOut CoreRequest.UnsafeRequestOptions
  => ToCoreRequestOptions input output
  => String
  -> { | input }
  -> Array StatusCode
  -> (FetchError -> err)
  -> Aff (Either err Response)
fetchEither url r allowedStatusCodes handleError = runExceptT do
  let
    fetch = do
      request <- liftEffect $ new url $ Request.convert r
      cResponse <- Promise.toAffE $ Response.promiseToPromise <$> Core.fetch request
      pure $ Response.convert cResponse
  res <- ExceptT $ (Right <$> fetch) `catchError` \err -> do
    pure $ Left $ handleError $ FetchError err

  if res.status `A.elem` allowedStatusCodes then pure res
  else throwError $ handleError $ InvalidStatusCode res

-- For the full safety we should introduce a newtype wrapper for the Response record
jsonBody :: Response -> Aff Json
jsonBody response = unsafeCoerce <$> response.json
