module Contrib.Polyform.Batteries.BigInt where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Polyform.Batteries (Dual', Validator', error) as Batteries
import Polyform.Dual (dual) as Dual
import Polyform.Validator (liftFnMaybe) as Validator
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

_bigIntExpected = Proxy :: Proxy "bigIntExpected"

type BigIntExpected e = (bigIntExpected :: String | e)

validator :: forall e m. Applicative m => Batteries.Validator' m (BigIntExpected + e) String BigInt
validator = Validator.liftFnMaybe (Batteries.error _bigIntExpected $ append "Expecting a string but got: ") BigInt.fromString

dual :: forall e m. Applicative m => Batteries.Dual' m (BigIntExpected + e) String BigInt
dual = Dual.dual validator (pure <<< show)
