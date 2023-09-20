module Contrib.Cardano where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, instant)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Marlowe.Runtime.Web.Types (SlotNumber(..))

newtype Slotting = Slotting
  { slotLength :: BigInt
  , slotZeroTime :: BigInt
  }

slotToTimestamp :: Slotting -> SlotNumber -> Instant
slotToTimestamp (Slotting { slotZeroTime, slotLength }) (SlotNumber n) = fromMaybe bottom do
  let
    slotNo = BigInt.fromInt n
    timestamp = BigInt.toNumber $ slotZeroTime + (slotNo * slotLength)
  instant $ Milliseconds timestamp
