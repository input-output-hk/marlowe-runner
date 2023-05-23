module Test.Marlowe.Runtime.Web where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Marlowe.Runtime.Web.Types (ServerURL(..))
import Test.Marlowe.Web.Client as Client
import Test.Marlowe.Web.Types as Types
import Test.Spec (Spec, describe, it)
import Test.Spec as Spec

_MARLOWE_WEB_SERVER_URL :: String
_MARLOWE_WEB_SERVER_URL = "MARLOWE_WEB_SERVER_URL"

spec :: Maybe ServerURL -> Spec Unit
spec possibleServerURL = do
  describe "Marlowe.Web" $ Spec.parallel do
    -- Types.spec
    case possibleServerURL of
      Just serverURL -> Client.spec serverURL
      Nothing ->
        it ("Skipping client tests as " <> _MARLOWE_WEB_SERVER_URL <> " env var is not set") do
         pure unit


