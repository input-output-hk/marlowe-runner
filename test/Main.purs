module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (ServerURL(..))
import Node.Process (getEnv)
import Test.Marlowe.Runtime.Web (_MARLOWE_WEB_SERVER_URL)
import Test.Marlowe.Runtime.Web as Web
import Test.Spec as Spec
import Test.Spec.Reporter (specReporter)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = do

  serverUrlStr <- getEnv <#> Object.lookup _MARLOWE_WEB_SERVER_URL >>> map ServerURL
  launchAff_ $ do
    let
      config = defaultConfig { timeout = Just (Milliseconds 10000.0) }

    runSpec' config [ consoleReporter, specReporter ] $ do
      Spec.parallel do
        Web.spec serverUrlStr
--Test.Contrib.Data.Map.spec
