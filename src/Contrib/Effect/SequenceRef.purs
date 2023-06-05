module Contrib.Effect.SequenceRef where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype SequenceRef a = SequenceRef (Ref a)

new :: forall a. a -> Effect (SequenceRef a)
new seed = do
  ref <- Ref.new seed
  pure $ SequenceRef ref

next :: forall a. Semiring a => SequenceRef a -> Effect a
next (SequenceRef ref) = Ref.modify (_ + one) ref

