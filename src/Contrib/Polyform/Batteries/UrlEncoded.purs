module Contrib.Polyform.Batteries.UrlEncoded where

import Prelude

import Data.Array as Array
import Data.Either (note)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.FormURLEncoded.Query as Query
import Data.Maybe (Maybe(..))
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Validator as Validator

fieldForm
  :: forall a m err
   . Monad m
  => FieldId
  -> Batteries.Validator m err (Maybe String) a
  -> UrlEncoded.Validator m err Query a
fieldForm name fieldValidator = do
  let
    validator' = fieldValidator <<< Validator.liftFn \query -> do
      value <- Query.lookup name query
      Array.head value
  UrlEncoded.fromValidator name validator'

requiredV msg v = v <<< Validator.liftFnEither (note [ msg ])

requiredV' v = requiredV "This field is required" v
