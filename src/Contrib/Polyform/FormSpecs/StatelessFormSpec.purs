module Contrib.Polyform.FormSpecs.StatelessFormSpec where

import Prelude

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

type FieldInitialsRow r =
  ( name :: FieldId
  , initial :: Array String
  | r
  )

type FieldInitials =
  { touched :: Boolean
  | FieldInitialsRow ()
  }

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

type FormState err =
  { fields :: FieldsState err
  , errors :: Maybe (UrleEncoded.Errors err /\ Query)
  , query :: Query
  }

type RenderFn err doc = FormState err -> doc

newtype StatelessFormSpec m doc err i o = StatelessFormSpec
  { fields :: Array FieldInitials
  , validator :: UrlEncoded.Validator m err i o
  , render :: RenderFn err doc
  }

renderFormSpec :: forall doc err i o m. StatelessFormSpec m doc err i o -> FormState err -> doc
renderFormSpec (StatelessFormSpec { render }) = render

mapRender :: forall doc doc' err i o m. (doc -> doc') -> StatelessFormSpec m doc err i o -> StatelessFormSpec m doc' err i o
mapRender f (StatelessFormSpec { fields, validator, render }) = StatelessFormSpec { fields, validator, render: f <<< render }

derive instance Applicative m => Functor (StatelessFormSpec m doc err i)

instance (Monad m, Semigroup doc) => Apply (StatelessFormSpec m doc err i) where
  apply (StatelessFormSpec { fields: fields1, validator: validator1, render: render1 }) (StatelessFormSpec { fields: fields2, validator: validator2, render: render2 }) =
    StatelessFormSpec
      { fields: fields1 <> fields2
      , validator: apply validator1 validator2
      , render: (<>) <$> render1 <*> render2
      }

instance (Monad m, Monoid doc) => Applicative (StatelessFormSpec m doc err i) where
  pure a = StatelessFormSpec { fields: [], validator: pure a, render: const mempty }

instance (Monad m, Semigroup doc) => Semigroupoid (StatelessFormSpec m doc err) where
  compose (StatelessFormSpec { fields: fields1, validator: validator1, render: render1 }) (StatelessFormSpec { fields: fields2, validator: validator2, render: render2 }) =
    StatelessFormSpec
      { fields: fields1 <> fields2
      , validator: compose validator1 validator2
      , render: (<>) <$> render2 <*> render1
      }

instance (Monad m, Monoid doc) => Category (StatelessFormSpec m doc err) where
  identity = StatelessFormSpec { fields: mempty, validator: identity, render: mempty }

liftValidator :: forall m doc err i o. Monoid doc => UrlEncoded.Validator m err i o -> StatelessFormSpec m doc err i o
liftValidator validator = StatelessFormSpec { fields: [], validator, render: const mempty }

hoist :: forall doc err m m' i o. Functor m => (m ~> m') -> StatelessFormSpec m doc err i o -> StatelessFormSpec m' doc err i o
hoist f (StatelessFormSpec { fields, validator, render }) =
  StatelessFormSpec { fields, validator: Validator.hoist f validator, render }

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
  :: forall a m err
   . Monad m
  => FieldId
  -> Batteries.Validator m err (Maybe String) a
  -> UrlEncoded.Validator m err Query a
toFormValidator name fieldValidator = do
  let
    validator' = fieldValidator <<< Validator.liftFn \query -> do
      value <- Query.lookup name query
      Array.head value
  UrlEncoded.fromValidator name validator'

-- Do we use these?

input
  :: forall a doc err m
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Batteries.Validator m err (Maybe String) a
  -> StatelessFormSpec m doc err Query a
input name initial render touched validator = StatelessFormSpec
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: toFormValidator name validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

optInput
  :: forall a doc err m
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Validators.SingleValueFieldValidator m err a
  -> StatelessFormSpec m doc err Query (Maybe a)
optInput name initial render touched validator = StatelessFormSpec
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: Validators.optional name $ validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

multiSelect
  :: forall a doc err m
   . Monad m
  => FieldId
  -> Array String
  -> err
  -> RenderFn err doc
  -> Boolean
  -> Batteries.Validator m err (NonEmptyArray String) (NonEmptyArray a)
  -> StatelessFormSpec m doc err Query (NonEmptyArray a)
multiSelect name initial err render touched validator = StatelessFormSpec
  { fields: [ { name, initial, touched } ]
  , validator: Validators.requiredMulti name err $ validator
  , render
  }

