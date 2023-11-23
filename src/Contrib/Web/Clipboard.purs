module Contrib.Web.Clipboard where

import Prelude
import Effect (Effect)
import Data.Undefined.NoProblem (Opt, opt)
import Web.Clipboard as Clipboard
import Web.Clipboard (Clipboard)
import Web.HTML (Navigator)

clipboard :: Navigator -> Effect (Opt Clipboard)
clipboard n = do
  c <- Clipboard.clipboard n
  -- This makes it safe
  pure $ opt c
