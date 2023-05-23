module Contrib.Data.Lens.Generic.Record where

import Control.Category ((<<<))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (class Strong)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList) as RL
import Record (insert) as Record
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- | Because we share profunctor type variable `p` accross all the
-- | lenses in the resulting records these alias can be useful and you can use it like:
-- | ```
-- | _ContractState
-- |   :: forall a p
-- |    . Strong p
-- |   => { feac :: Lens'' p (ContractState a) a
-- |      , xa :: Lens'' p (ContractState a) (Maybe a)
-- |      , xd :: Lens'' p (ContractState a) (Maybe DateTime)
-- |      }
-- | _ContractState = mkNewtyped1Lenses (Proxy :: Proxy ContractState)
-- | ```
type Lens'' :: forall k. (k -> k -> Type) -> k -> k -> Type
type Lens'' p s a = p a a -> p s s

data MkLensesStep :: forall k. k -> Type
data MkLensesStep p = MkLensesStep

instance
  ( IsSymbol l
  , Row.Lacks l lenses
  , Row.Cons l a r1_ r1
  , Row.Cons l b r1_ r2
  , Row.Cons l (p a b -> p { | r1 } { | r2 }) lenses lenses'
  , Strong p
  ) =>
  FoldingWithIndex (MkLensesStep p) (Proxy l) { | lenses } (Proxy value) { | lenses' } where
  foldingWithIndex _ l acc _ = Record.insert l (prop l) acc

-- -- | We get a record of polymorphic lenses out of this
-- -- | the signature is horrible but... we can autogenerate it.
-- ls = mkLenses (Proxy :: Proxy { a :: Int, b :: String, c :: Number })
--
-- strA :: _
-- strA = Lens.view ls.a { a: "not an int" }
--
-- intA :: _
-- intA = Lens.set ls.a 8 { a: "string" }
mkLenses
  :: forall p r rl rout
   . Strong p
  => RL.RowToList r rl
  => HFoldlWithIndex (MkLensesStep p) {} (Proxy rl) { | rout }
  => Proxy { | r }
  -> { | rout }
mkLenses _ = hfoldlWithIndex (MkLensesStep :: MkLensesStep p) {} (Proxy :: Proxy rl)

data MkNewtypedLensesStep :: forall k. k -> Type -> Type
data MkNewtypedLensesStep p n = MkNewtypedLensesStep

instance
  ( IsSymbol l
  , Newtype n { | r }
  , Row.Lacks l lenses
  , Row.Cons l a r_ r
  , Row.Cons l (p a a -> p n n) lenses lenses'
  , Strong p
  ) =>
  FoldingWithIndex (MkNewtypedLensesStep p n) (Proxy l) { | lenses } (Proxy value) { | lenses' } where
  foldingWithIndex _ l acc _ = Record.insert l (_Newtype <<< prop l) acc

mkNewtypedLenses
  :: forall n p r rl rout
   . Strong p
  => Newtype n { | r }
  => RL.RowToList r rl
  => HFoldlWithIndex (MkNewtypedLensesStep p n) {} (Proxy rl) { | rout }
  => Proxy n
  -> { | rout }
mkNewtypedLenses _ = hfoldlWithIndex (MkNewtypedLensesStep :: MkNewtypedLensesStep p n) {} (Proxy :: Proxy rl)

data MkNewtyped1LensesStep :: forall k. k -> (Type -> Type) -> Type -> Type
data MkNewtyped1LensesStep p n a = MkNewtyped1LensesStep

instance
  ( IsSymbol l
  , Newtype (n a) { | r }
  , Row.Lacks l lenses
  , Row.Cons l x r_ r
  , Row.Cons l (p x x -> p (n a) (n a)) lenses lenses'
  , Strong p
  ) =>
  FoldingWithIndex (MkNewtyped1LensesStep p n a) (Proxy l) { | lenses } (Proxy value) { | lenses' } where
  foldingWithIndex _ l acc _ = Record.insert l (_Newtype <<< prop l) acc

mkNewtyped1Lenses
  :: forall a n p r rl rout
   . Strong p
  => Newtype (n a) { | r }
  => RL.RowToList r rl
  => HFoldlWithIndex (MkNewtyped1LensesStep p n a) {} (Proxy rl) { | rout }
  => Proxy n
  -> { | rout }
mkNewtyped1Lenses _ = hfoldlWithIndex (MkNewtyped1LensesStep :: MkNewtyped1LensesStep p n a) {} (Proxy :: Proxy rl)
