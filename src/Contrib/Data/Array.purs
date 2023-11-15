module Runner.Contrib.Data.Array where

import Data.BigInt.Argonaut (BigInt)

foreign import rangeBigInt :: BigInt -> BigInt -> Array BigInt

