module Contrib.Polyform.FormSpecs.StatefulFormSpec where

import Prelude

import Contrib.Polyform.FormSpecs.StatelessFormSpec (StatelessFormSpec(..))
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.FormURLEncoded.Query as Query
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Validators as Validators
import Polyform.Validator as Validator
import Type.Row (type (+))

type FieldStateRow err r =
  ( errors :: Maybe (Array err /\ Array String) -- `Maybe` indicates if a field was validated
  , onChange :: Array String -> Effect Unit
  , touched :: Disj Boolean
  , value :: Array String
  | FieldInitialsRow
      + r
  )

type FieldState err = { | FieldStateRow err () }

type RenderFieldFn err doc = FieldState err -> doc

type FieldsState err = Map FieldId (FieldState err)

type FormState st err =
  { fields :: FieldsState err
  , errors :: Maybe (UrleEncoded.Errors err /\ Query)
  , query :: Query
  | st
  }


type FieldInitialsRow r =
  ( name :: FieldId
  , initial :: Array String
  -- Should we treat the field as "touched" from the begining
  | r
  )

type FieldInitials =
  { touched :: Boolean
  | FieldInitialsRow ()
  }

type RenderFn :: Row Type -> Type -> Type -> Type
type RenderFn st err doc = FormState st err -> doc

newtype StatefulFormSpec m st doc err i o = StatefulFormSpec
  { fields :: Array FieldInitials
  , validator :: UrlEncoded.Validator (StateT st m) err i o
  , render :: RenderFn (state :: st) err doc
  }

renderForm :: forall doc err i o m st. StatefulFormSpec m st doc err i o -> FormState (state :: st) err -> doc
renderForm (StatefulFormSpec { render }) = render

mapRender :: forall doc doc' err i o m st. (doc -> doc') -> StatefulFormSpec st m doc err i o -> StatefulFormSpec st m doc' err i o
mapRender f (StatefulFormSpec { fields, validator, render }) = StatefulFormSpec { fields, validator, render: f <<< render }

derive instance Monad m => Functor (StatefulFormSpec m st doc err i)

instance (Monad m, Semigroup doc) => Apply (StatefulFormSpec m st doc err i) where
  apply (StatefulFormSpec { fields: fields1, validator: validator1, render: render1 }) (StatefulFormSpec { fields: fields2, validator: validator2, render: render2 }) =
    StatefulFormSpec
      { fields: fields1 <> fields2
      , validator: apply validator1 validator2
      , render: (<>) <$> render1 <*> render2
      }

instance (Monad m, Monoid doc) => Applicative (StatefulFormSpec m st doc err i) where
  pure a = StatefulFormSpec { fields: [], validator: pure a, render: const mempty }

instance (Monad m, Semigroup doc) => Semigroupoid (StatefulFormSpec m st doc err) where
  compose (StatefulFormSpec { fields: fields1, validator: validator1, render: render1 }) (StatefulFormSpec { fields: fields2, validator: validator2, render: render2 }) =
    StatefulFormSpec
      { fields: fields1 <> fields2
      , validator: compose validator1 validator2
      , render: (<>) <$> render2 <*> render1
      }

instance (Monad m, Monoid doc) => Category (StatefulFormSpec m st doc err) where
  identity = StatefulFormSpec { fields: mempty, validator: identity, render: mempty }

liftValidator :: forall m doc err i o st. Monoid doc => UrlEncoded.Validator (StateT st m) err i o -> StatefulFormSpec m st doc err i o
liftValidator validator = StatefulFormSpec { fields: [], validator, render: const mempty }

liftStatelessFormSpec :: forall m doc err i o st. Monad m => Monoid doc => StatelessFormSpec m doc err i o -> StatefulFormSpec m st doc err i o
liftStatelessFormSpec (StatelessFormSpec { fields, validator, render }) = StatefulFormSpec
  { fields
  , validator: Validator.hoist lift validator
  , render: \{ fields: fs, errors, query } -> render { fields: fs, errors, query }
  }

hoist :: forall doc err m m' i o st. Functor m => (StateT st m ~> StateT st m') -> StatefulFormSpec m st doc err i o -> StatefulFormSpec m' st doc err i o
hoist f (StatefulFormSpec { fields, validator, render }) =
  StatefulFormSpec { fields, validator: Validator.hoist f validator, render }

type InputFieldStateRow err r =
  ( errors :: Maybe (Array err /\ Array String) -- `Maybe` indicates if a field was validated
  , onChange :: String -> Effect Unit
  , touched :: Boolean
  , value :: String
  | FieldInitialsRow
      + r
  )

type InputState err = { | InputFieldStateRow err () }

type RenderInputFn err doc = InputState err -> doc

toInputState :: forall err. FieldState err -> InputState err
toInputState fieldState =
  { errors: fieldState.errors
  , onChange: fieldState.onChange <<< Array.singleton
  , touched: un Disj fieldState.touched
  , value: fromMaybe "" $ Array.head fieldState.value
  , name: fieldState.name
  , initial: fieldState.initial
  }

toFormValidator
  :: forall a m err st
   . Monad m
  => FieldId
  -> Batteries.Validator (StateT st m) err (Maybe String) a
  -> UrlEncoded.Validator (StateT st m) err Query a
toFormValidator name fieldValidator = do
  let
    validator' = fieldValidator <<< Validator.liftFn \query -> do
      value <- Query.lookup name query
      Array.head value
  UrlEncoded.fromValidator name validator'


input
  :: forall a doc err m st
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Batteries.Validator (StateT st m) err (Maybe String) a
  -> StatefulFormSpec m st doc err Query a
input name initial render touched validator = StatefulFormSpec
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: toFormValidator name validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

optInput
  :: forall a doc err m st
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Validators.SingleValueFieldValidator (StateT st m) err a
  -> StatefulFormSpec m st doc err Query (Maybe a)
optInput name initial render touched validator = StatefulFormSpec
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: Validators.optional name $ validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

multiSelect
  :: forall a doc err m st
   . Monad m
  => FieldId
  -> Array String
  -> err
  -> RenderFn (state :: st) err doc
  -> Boolean
  -> Batteries.Validator (StateT st m) err (NonEmptyArray String) (NonEmptyArray a)
  -> StatefulFormSpec m st doc err Query (NonEmptyArray a)
multiSelect name initial err render touched validator = StatefulFormSpec
  { fields: [ { name, initial, touched } ]
  , validator: Validators.requiredMulti name err $ validator
  , render
  }


