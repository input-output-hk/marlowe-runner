module JsYaml where

import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn2, runFn2)

type Options = { indent :: Int }

foreign import dumpImpl :: Fn2 Json Options String

dump :: Json -> { indent :: Int } -> String
dump json = runFn2 dumpImpl json
