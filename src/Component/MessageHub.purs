module Component.MessageHub where

import Prelude

import Component.Types (Message(..), MessageEntry, MessageHub(..), MessageId)
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
import React.Basic (fragment) as D
import React.Basic.DOM (br, div_) as D
import React.Basic.DOM.Simplified.Generated as S
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, readRef, useContext, useEffect, useState, useState', (/\))
import React.Basic.Hooks as R
import ReactBootstrap (alert)
import ReactBootstrap.Collapse (collapse, dimension)
import ReactBootstrap.Types (variant)
import Utils.React.Basic.Hooks (useEmitter, useStateRef')

data RenderingContext = InboxMessage | ToastMessage

type Handlers =
  { onClose :: MessageId -> Effect Unit
  , onExpand :: Effect Unit
  }

newtype Expanded = Expanded Boolean

renderMsg :: Handlers -> String -> Expanded -> RenderingContext -> MessageEntry -> JSX
renderMsg { onClose, onExpand } extraClassName (Expanded expanded) rCtx { id, msg } = case msg of
  Info msg' -> alert { className, variant: variant.info, dismissible: true, transition: false, onClose: onClose', "data-testId": testId } $
    -- [ icon Icons.infoCircleFill, msg' ]
    body msg'
  Success msg' -> alert { className, variant: variant.success, dismissible: true, onClose: onClose', "data-testId": testId } $
    -- [ icon Icons.checkCircleFill, msg' ]
    body msg'
  Warning msg' -> alert { className, variant: variant.warning, dismissible: true, onClose: onClose', "data-testId": testId } $
    -- [ icon Icons.exclamationTriangleFill, msg' ]
    body msg'
  Error msg' -> alert { className, variant: variant.danger, dismissible: true, onClose: onClose', "data-testId": testId } $
    -- [ icon Icons.exclamationTriangleFill, msg' ]
    body msg'
  where
  rCtxPrefix = case rCtx of
    InboxMessage -> "inbox"
    ToastMessage -> "toast"
  testId = case msg of
    Info _ -> rCtxPrefix <> "-info-msg"
    Success _ -> rCtxPrefix <> "-success-msg"
    Warning _ -> rCtxPrefix <> "-warning-msg"
    Error _ -> rCtxPrefix <> "-error-msg"
  body msgContent = case msgContent.description of
    Nothing -> [ msgContent.msg ]
    Just desc ->
      if expanded then
        [ msgContent.msg
        , D.br {}
        , desc
        ]
      else
        [ msgContent.msg
        , D.br {}
        , S.a { onClick: handler_ onExpand }
            [ "Show details" ]
        ]
  colorClasses = case msg of
    Info _ -> "border-info"
    Success _ -> "border-success"
    Warning _ -> "border-warning"
    Error _ -> "border-danger"
  className = extraClassName <> " py-2 shadow-sm d-flex align-items-center border border-1 rounded text-color-dark " <> colorClasses
  -- icon = DOM.span { className: "me-2" } <<< Icons.toJSX
  onClose' = onClose id

mkMessagePreview :: Effect (MessageHub -> JSX)
mkMessagePreview = component "MessageBox" \(MessageHub { ctx, remove }) -> R.do
  msgs <- useContext ctx
  currMsg /\ setCurrMsg <- useState' Nothing
  visible /\ setVisible <- useState' true
  expanded /\ setExpanded <- useState' false
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
      setExpanded false
      tId <- setTimeout 1200 do
        -- FIXME: bring back auto close behavior
        -- setVisible false
        pure unit
      setTimeoutId $ Just tId
    else do
      setVisible false

    pure $ void $ for timeoutId \id ->
      liftEffect $ clearTimeout id

  let
    onClose id = do
      remove id
    handlers =
      { onClose, onExpand: setExpanded true }

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
        } $ D.div_ [ renderMsg handlers "" (Expanded expanded) ToastMessage msg ]

mkMessageBox :: Effect (MessageHub -> JSX)
mkMessageBox = component "MessageBox" \(MessageHub { ctx, remove }) -> R.do
  msgs <- useContext ctx
  let
    onClose id = remove id
    handlers = { onClose, onExpand: pure unit }
  pure $ D.fragment $ Array.fromFoldable $ map (renderMsg handlers "" (Expanded false) InboxMessage) $ msgs

data Action
  = Add Message
  | Remove MessageId
  | RemoveAll

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
      RemoveAll -> do
        updateMessages $ const List.Nil

    pure
      $ provider msgCtx messages
      $ children

  let
    add msg = notify listener (Add msg)
    remove id = notify listener (Remove id)

  pure (hubComponent /\ MessageHub { remove, add, ctx: msgCtx })
