module Contrib.React.Bootstrap.Form.Control where

import Prelude

import Contrib.React.HTMLAttributes (HTMLAttributes, InputHTMLAttributes)
import Data.Maybe (Maybe(..))
import Debug (traceM)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Value :: Type

value
  :: { array :: Array String -> Value
     , number :: Number -> Value
     , string :: String -> Value
     }
value =
  { "string": unsafeCoerce :: String -> Value
  , "number": unsafeCoerce :: Number -> Value
  , "array": unsafeCoerce :: Array String -> Value
  }

type BaseProps controlValue extraProps = HTMLAttributes + InputHTMLAttributes +
  ( htmlSize :: Int
  , as :: String
  , size :: String
  , plaintext :: Boolean
  , readOnly :: Boolean
  , disabled :: Boolean
  , value :: controlValue
  , onChange :: EventHandler
  , name :: String
  , isValid :: Boolean
  , isInvalid :: Boolean
  | extraProps
  )

-- | Direct translation from ts to purescript
-- | TODO: Add more handy / safe wrappers.
-- | `type` is only relevant when `as` is an `input`.
type Props_control = BaseProps String ("type" :: String)

foreign import _Control :: ReactComponent { | Props_control }

_internalcontrol :: forall attrs attrs_. Row.Union attrs attrs_ Props_control => ReactComponent { | attrs }
_internalcontrol = unsafeCoerce _Control

control
  :: forall attrs attrs_
   . Row.Union attrs attrs_ Props_control
  => Record attrs
  -> JSX
control props = element _internalcontrol props

type Props_inputtext = BaseProps String ()

textInput
  :: forall attrs attrs' attrs_
   . Row.Union attrs' attrs_ Props_control
  => Row.Union attrs (type :: String, as :: String) (type :: String, as :: String | attrs)
  => Row.Nub (type :: String, as :: String | attrs) attrs'
  => Record attrs
  -> JSX
textInput props = element _internalcontrol props'
  where
  props' :: { | attrs' }
  props' = Record.merge props { "type": "text", "as": "input" }

-- There is a bug in react-bootstrap type definitions because
-- types miss the "rows" and "cols" attributes.
type Props_textArea = BaseProps String ("type" :: String, rows :: Int, cols :: Int)

_internalTextArea :: forall attrs attrs_. Row.Union attrs attrs_ Props_textArea => ReactComponent { | attrs }
_internalTextArea = unsafeCoerce _Control

textArea
  :: forall attrs attrs' attrs_
   . Row.Union attrs' attrs_ Props_textArea
  => Row.Nub (as :: String | attrs) attrs'
  => Record attrs
  -> JSX
textArea props = element _internalTextArea props'
  where
  props' :: { | attrs' }
  props' = Record.merge { "as": "textarea" } props

-- There is a bug in react-bootstrap type definitions because
-- types miss the "rows" and "cols" attributes.
type Props_select = BaseProps String (multiple :: Boolean)

_internalSelect :: forall attrs attrs_. Row.Union attrs attrs_ Props_select => ReactComponent { | attrs }
_internalSelect = unsafeCoerce _Control
