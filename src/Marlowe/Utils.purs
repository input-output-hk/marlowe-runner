module Marlowe.Utils where

import Prelude

import Data.BigInt.Argonaut as BigInt
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Value(..))
import Language.Marlowe.Core.V1.Traversals
  ( Visitor(..)
  , rewriteContractBottomUp
  , rewriteContractTopDown
  )

valueOne :: Value
valueOne = Constant $ BigInt.fromInt 1

valueZero :: Value
valueZero = Constant $ BigInt.fromInt 0

-- | Rewrite rules for Value
-- Note: The rewrite rules for `Value` might change the semantics as
-- evaluation of Value implies integer arthmetics.
rewriteValue :: Value -> Value
rewriteValue (NegValue (NegValue a)) = rewriteValue a
rewriteValue (NegValue (SubValue a b)) = rewriteValue $ SubValue b a
rewriteValue (AddValue a b) | b == valueZero = rewriteValue a
rewriteValue (AddValue a b) | a == valueZero = rewriteValue b
rewriteValue (SubValue a b) | b == valueZero = rewriteValue a
rewriteValue (SubValue a b) | a == valueZero = rewriteValue $ NegValue b
rewriteValue (MulValue a b) | b == valueOne = rewriteValue a
rewriteValue (MulValue a b) | a == valueOne = rewriteValue b
rewriteValue (MulValue _ b) | b == valueZero = valueZero
rewriteValue (MulValue a _) | a == valueZero = valueZero
rewriteValue (DivValue a b) | b == valueOne = rewriteValue a
rewriteValue (DivValue a _) | a == valueZero = valueZero
rewriteValue (MulValue a (DivValue b c)) | a == c = rewriteValue b
rewriteValue (MulValue (DivValue b c) a) | a == c = rewriteValue b
rewriteValue (DivValue (MulValue b c) a) | a == b = rewriteValue c
rewriteValue v = v

-- | Rewrite a Contract
rewrite :: Contract -> Contract
rewrite contract =
  let (ident :: Identity Contract) = rewrite' contract in unwrap ident

rewrite' :: forall m. Monad m => Contract -> m Contract
rewrite' = rewriteContractTopDown visitor >=> rewriteContractBottomUp
  visitor
  where
  visitor = Visitor { onCase, onContract, onObservation, onValue }
  onCase = pure
  onContract = pure
  onObservation = pure
  onValue = pure <<< rewriteValue
