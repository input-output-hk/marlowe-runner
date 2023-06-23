module Contrib.Polyform.FormSpecBuilder where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.FormURLEncoded.Query (FieldId(..))
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)

type IdCounter = Int

newtype FieldIdPrefix = FieldIdPrefix String

_fullPrefix :: FieldIdPrefix -> String
_fullPrefix (FieldIdPrefix prefix) = prefix <> "-"

prefixString :: FieldIdPrefix -> String -> String
prefixString prefix str = _fullPrefix prefix <> str

prefixFieldId :: FieldIdPrefix -> FieldId -> FieldId
prefixFieldId prefix (FieldId fieldId) = FieldId $ prefixString prefix fieldId

type FormSpecBuilderM builderM = ReaderT (Maybe FieldIdPrefix) (StateT IdCounter builderM)

newtype FormSpecBuilderT :: forall k1 k2. (Type -> Type) -> (k1 -> k2 -> Type) -> k1 -> k2 -> Type
newtype FormSpecBuilderT builderM formSpec i o = FormSpecBuilderT
  (Compose (FormSpecBuilderM builderM) (formSpec i) o)

type FormSpecBuilder :: forall k1 k2. (k1 -> k2 -> Type) -> k1 -> k2 -> Type
type FormSpecBuilder = FormSpecBuilderT Identity

formSpecBuilderT
  :: forall builderM formSpec i o
   . FormSpecBuilderM builderM (formSpec i o)
  -> FormSpecBuilderT builderM formSpec i o
formSpecBuilderT = FormSpecBuilderT <<< Compose

liftBuilderM
  :: forall builderM formSpec i o
   . Monad builderM
  => builderM (formSpec i o)
  -> FormSpecBuilderT builderM formSpec i o
liftBuilderM = formSpecBuilderT <<< lift <<< lift

derive instance Newtype (FormSpecBuilderT builderM formSpec i o) _
derive newtype instance (Applicative builderM, Functor (formSpec i)) => Functor (FormSpecBuilderT builderM formSpec i)
derive newtype instance (Monad builderM, Apply (formSpec i)) => Apply (FormSpecBuilderT builderM formSpec i)
derive newtype instance (Monad builderM, Applicative (formSpec i)) => Applicative (FormSpecBuilderT builderM formSpec i)
instance (Monad builderM, Semigroupoid formSpec) => Semigroupoid (FormSpecBuilderT builderM formSpec) where
  compose (FormSpecBuilderT (Compose builder1)) (FormSpecBuilderT (Compose builder2)) = formSpecBuilderT do
    formSpec1 <- builder1
    formSpec2 <- builder2
    pure $ compose formSpec1 formSpec2

instance (Monad builderM, Category formSpec) => Category (FormSpecBuilderT builderM formSpec) where
  identity = formSpecBuilderT $ pure identity

unFormSpecBuilder
  :: forall builderM i o formSpec
   . FormSpecBuilderT builderM formSpec i o
  -> FormSpecBuilderM builderM (formSpec i o)
unFormSpecBuilder (FormSpecBuilderT (Compose builder)) = builder

genId
  :: forall builderM
   . Monad builderM
  => FormSpecBuilderM builderM String
genId = do
  possiblePrefix <- ask
  let
    prefix str = case possiblePrefix of
      Nothing -> str
      Just pref -> prefixString pref str
  counter <- get
  put (counter + 1)
  let id = prefix $ show counter
  pure id

evalBuilderT
  :: forall builderM formSpec i o
   . Functor builderM
  => Maybe FieldIdPrefix
  -> FormSpecBuilderT builderM formSpec i o
  -> builderM (formSpec i o)
evalBuilderT possiblePrefix (FormSpecBuilderT (Compose m)) = flip evalStateT 0 <<< runReaderT m $ possiblePrefix

evalBuilder
  :: forall formSpec i o
   . Maybe FieldIdPrefix
  -> FormSpecBuilderT Identity formSpec i o
  -> formSpec i o
evalBuilder possiblePrefix = un Identity <<< evalBuilderT possiblePrefix

evalBuilder' :: forall formSpec i o. FormSpecBuilder formSpec i o -> formSpec i o
evalBuilder' = evalBuilder Nothing

hoistFormSpec
  :: forall builderM formSpec formSpec' i o
   . Functor builderM
  => (formSpec i ~> formSpec' i)
  -> FormSpecBuilderT builderM formSpec i o
  -> FormSpecBuilderT builderM formSpec' i o
hoistFormSpec f (FormSpecBuilderT (Compose builder)) = FormSpecBuilderT $ Compose $ f <$> builder

