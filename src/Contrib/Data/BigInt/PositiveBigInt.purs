module Contrib.Data.BigInt.PositiveBigInt where

import Prelude

import Data.BigInt.Argonaut (BigInt(..))
import Data.BigInt.Argonaut as BigInt
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype PositiveBigInt = PositiveBigInt BigInt

derive instance Newtype PositiveBigInt _
derive newtype instance Eq PositiveBigInt
derive newtype instance Ord PositiveBigInt
derive newtype instance Semiring PositiveBigInt

fromBigInt :: BigInt -> Maybe PositiveBigInt
fromBigInt n =
  if n >= BigInt.fromInt 0 then Just (PositiveBigInt n)
  else Nothing

