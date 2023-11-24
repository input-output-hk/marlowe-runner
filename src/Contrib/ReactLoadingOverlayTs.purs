module Runner.Contrib.ReactLoadingOverlayTs where

import Data.Nullable (Nullable)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Foreign.Object (Object)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM

foreign import data LoadingOverlay_ :: Type

-- | Type and safe constructors for spinner sum type: `boolean | react node`
foreign import data Spinner :: Type

spinner :: { boolean :: Boolean -> Spinner, jsx :: JSX -> Spinner }
spinner = { boolean: unsafeCoerce, jsx: unsafeCoerce }

type SetStyles = Object String -> Object String

type Styles =
  { overlay :: Opt SetStyles
  , content :: Opt SetStyles
  , spinner :: Opt SetStyles
  , wrapper :: Opt SetStyles
  }

type Props =
  { active :: Opt Boolean
  , fadeSpeed :: Opt Int
  , onClick :: Opt (EventHandler)
  , className :: Opt String
  , classNamePrefix :: Opt String
  , spinner :: Opt Spinner
  , text :: Opt JSX
  , styles :: Opt Styles
  , ref :: Opt (Nullable DOM.Node)
  }

foreign import loadingOverlayImpl :: Props -> Array JSX -> JSX

loadingOverlay
  :: forall props
   . NoProblem.Coerce { | props } Props
  => { | props }
  -> Array JSX
  -> JSX
loadingOverlay props children = do
  let
    props' = NoProblem.coerce props
  loadingOverlayImpl props' children
