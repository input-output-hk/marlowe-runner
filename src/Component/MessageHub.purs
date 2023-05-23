module Component.MessageHub where

import Prelude

import Component.Types (Message, MessageContent(..), MessageHub(..), MessageId)
import Contrib.React.Bootstrap (alert)
import Contrib.React.Bootstrap.Collapse (collapse, dimension)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Types (variant)
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (clearTimeout, setTimeout)
import Halogen.Subscription (notify)
import Halogen.Subscription as Subscription
import React.Basic (JSX, createContext, provider)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (div_) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, readRef, useContext, useEffect, useRef, useState, useState', writeRef, (/\))
import React.Basic.Hooks as R
import Utils.React.Basic.Hooks (useEmitter, useStateRef')

renderMsg :: (MessageId -> Effect Unit) -> String -> Message -> JSX
renderMsg onClose extraClassName { id, msg } = case msg of
  Info msg' -> alert { className, variant: variant.info, dismissible: true, transition: false, onClose: onClose' }
    [ icon Icons.infoCircleFill, msg' ]
  Success msg' -> alert { className, variant: variant.success, dismissible: true, onClose: onClose' }
    [ icon Icons.checkCircleFill, msg' ]
  Warning msg' -> alert { className, variant: variant.warning, dismissible: true, onClose: onClose' }
    [ icon Icons.exclamationTriangleFill, msg' ]
  Error msg' -> alert { className, variant: variant.danger, dismissible: true, onClose: onClose' }
    [ icon Icons.exclamationTriangleFill, msg' ]
  where
  className = extraClassName <> " d-flex align-items-center"
  icon = DOM.span { className: "me-2" } <<< Icons.toJSX
  onClose' = onClose id

mkMessagePreview :: Effect (MessageHub -> JSX)
mkMessagePreview = component "MessageBox" \(MessageHub { ctx, remove }) -> R.do
  msgs <- useContext ctx
  currMsg /\ setCurrMsg <- useState' Nothing
  visible /\ setVisible <- useState' true
  timeoutId /\ setTimeoutId <- useState' Nothing

  let
    currHead = List.head msgs

  let
    maybeNew = do
      { id: hId } <- currHead
      { id: cId } <- currMsg
      pure $ hId > cId
    newItem = fromMaybe (isNothing currMsg && isJust currHead) maybeNew

  useEffect (_.id <$> currHead) do
    setCurrMsg currHead
    void $ for timeoutId \tid ->
      liftEffect $ clearTimeout tid
    setTimeoutId Nothing

    if newItem then do
      setVisible true
      tId <- setTimeout 1200 do
        setVisible false
      setTimeoutId $ Just tId
    else do
      setVisible false

    pure $ void $ for timeoutId \id ->
      liftEffect $ clearTimeout id

  let
    onClose id = do
      remove id

  pure case currMsg of
    Nothing -> mempty :: JSX
    Just msg ->
      collapse
        { appear: true
        , dimension: dimension.height
        , "in": visible
        , timeout: Milliseconds 2000.0
        , mountOnEnter: true
        , unmountOnExit: true
        } $ DOOM.div_ [ renderMsg onClose "shadow-lg" msg ]

mkMessageBox :: Effect (MessageHub -> JSX)
mkMessageBox = component "MessageBox" \(MessageHub { ctx, remove }) -> R.do
  msgs <- useContext ctx
  let
    onClose id = remove id
  pure $ DOOM.fragment $ Array.fromFoldable $ map (renderMsg onClose "") $ msgs

data Action
  = Add MessageContent
  | Remove MessageId

-- | Slightly unsafe (you have to wrap your app in the hub component) but convenient API.

mkMessageHub :: Effect ((Array JSX -> JSX) /\ MessageHub)
mkMessageHub = do
  { emitter, listener } <- Subscription.create
  msgCtx <- createContext List.Nil

  hubComponent <- component "MessageHub" \children -> R.do
    stateId /\ bumpId <- R.do
      id /\ updateId <- useState 0
      pure $ id /\ (updateId $ add 1)

    idRef <- useStateRef' stateId

    messages /\ updateMessages <- useState List.Nil
    useEmitter emitter case _ of
      Add msg -> do
        bumpId
        currId <- readRef idRef
        let
          msg' = { id: currId, msg }
        updateMessages $ List.Cons msg'
      Remove id -> do
        updateMessages $ List.filter (\{ id: id' } -> id /= id')

    pure
      $ provider msgCtx messages
      $ children

  let
    add msg = notify listener (Add msg)
    remove id = notify listener (Remove id)

  pure (hubComponent /\ MessageHub { remove, add, ctx: msgCtx })
