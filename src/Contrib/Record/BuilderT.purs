module Contrib.Record.BuilderT where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Monad.Indexed (class IxMonad)
import Control.Bind.Indexed (class IxBind)
import Data.Bifunctor (lmap)
import Data.Functor.Indexed (class IxFunctor)
import Data.Newtype (un, class Newtype)
import Data.Profunctor.Strong ((***))
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested ((/\), type (/\))

newtype BuilderT m r r' a = BuilderT (m ((r -> r') /\ a))

derive instance Newtype (BuilderT m r r' a) _

instance Functor m => IxFunctor (BuilderT m) where
  imap f (BuilderT a) = BuilderT $ map f <$> a

instance Apply m => IxApply (BuilderT m) where
  iapply (BuilderT f) (BuilderT a) = BuilderT $ apply (uncurry (***) <<< lmap (>>>) <$> f) a

instance Applicative m => IxApplicative (BuilderT m) where
  ipure a = BuilderT $ pure (identity /\ a)

instance (Applicative m, Bind m) => IxBind (BuilderT m) where
  ibind (BuilderT ma) f = BuilderT $ do
    (r2r' /\ a) <- ma
    (r'2r'' /\ b) <- un BuilderT $ f a
    pure (r'2r'' <<< r2r' /\ b)

instance (Monad m) => IxMonad (BuilderT m)

instance Functor m => Functor (BuilderT m i i) where
  map f (BuilderT ma) = BuilderT $ map f <$> ma

instance Alt m => Alt (BuilderT m i i) where
  alt (BuilderT ma) (BuilderT ma') = BuilderT (ma <|> ma')

execBuilderT :: forall a i o m. Applicative m => BuilderT m i o a -> i -> m o
execBuilderT (BuilderT mt) i = ((#) i <<< fst) <$> mt
