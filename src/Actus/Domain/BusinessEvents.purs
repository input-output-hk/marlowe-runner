module Actus.Domain.BusinessEvents where

import Prelude

import Contrib.Data.Argonaut (decodeJsonEnumWith, encodeJsonEnumWith)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum, class BoundedEnum)
import Data.Enum.Generic (genericCardinality, genericPred, genericSucc, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | ACTUS event types, https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-event.json
data EventType
  = IED -- ^ Initial Exchange
  | FP -- ^ Fee Payment
  | PR -- ^ Principal Redemption
  | PD -- ^ Principal Drawing
  | PY -- ^ Penalty Payment
  | PP -- ^ Principal Prepayment (unscheduled event)
  | IP -- ^ Interest Payment
  | IPFX -- ^ Interest Payment Fixed Leg
  | IPFL -- ^ Interest Payment Floating Leg
  | IPCI -- ^ Interest Capitalization
  | CE -- ^ Credit Event
  | RRF -- ^ Rate Reset Fixing with Known Rate
  | RR -- ^ Rate Reset Fixing with Unknown Rate
  | PRF -- ^ Principal Payment Amount Fixing
  | DV -- ^ Dividend Payment
  | PRD -- ^ Purchase
  | MR -- ^ Margin Call
  | TD -- ^ Termination
  | SC -- ^ Scaling Index Fixing
  | IPCB -- ^ Interest Calculation Base Fixing
  | MD -- ^ Maturity
  | XD -- ^ Exercise
  | STD -- ^ Settlement
  | PI -- ^ Principal Increase
  | AD -- ^ Monitoring

derive instance Generic EventType _
derive instance Eq EventType
derive instance Ord EventType

instance Show EventType where
  show = genericShow

description :: EventType -> String
description IED = "Initial Exchange"
description FP = "Fee Payment"
description PR = "Principal Redemption"
description PD = "Principal Drawing"
description PY = "Penalty Payment"
description PP = "Principal Prepayment (unscheduled event)"
description IP = "Interest Payment"
description IPFX = "Interest Payment Fixed Leg"
description IPFL = "Interest Payment Floating Leg"
description IPCI = "Interest Capitalization"
description CE = "Credit Event"
description RRF = "Rate Reset Fixing with Known Rate"
description RR = "Rate Reset Fixing with Unknown Rate"
description PRF = "Principal Payment Amount Fixing"
description DV = "Dividend Payment"
description PRD = "Purchase"
description MR = "Margin Call"
description TD = "Termination"
description SC = "Scaling Index Fixing"
description IPCB = "Interest Calculation Base Fixing"
description MD = "Maturity"
description XD = "Exercise"
description STD = "Settlement"
description PI = "Principal Increase"
description AD = "Monitoring"

instance Enum EventType where
  succ = genericSucc
  pred = genericPred

instance Bounded EventType where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum EventType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance EncodeJson EventType where
  encodeJson = encodeJsonEnumWith identity

instance DecodeJson EventType where
  decodeJson = decodeJsonEnumWith identity
