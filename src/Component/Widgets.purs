module Component.Widgets where

import Prelude

import Component.MarloweYaml (marloweStateYaml, marloweYaml)
import Contrib.React.Svg (loadingSpinnerLogo)
import ConvertableOptions (defaults, class Defaults)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Undefined.NoProblem (Opt, fromOpt)
import Data.Undefined.NoProblem (fromMaybe, toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Foreign.Object as Object
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Prim.Row as Row
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Simplified.Generated (b, button, div, span) as DOM
import React.Basic.Events (SyntheticEvent, handler)
import ReactBootstrap (overlayTrigger, tooltip)
import ReactBootstrap.Icons (Icon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Tooltip (TooltipPropsRow)
import ReactBootstrap.Types (Placement)
import Record as Record
import Runner.Contrib.ReactLoadingOverlayTs as LoadingOverlay
import Type.Proxy (Proxy(..))

spinner :: Maybe JSX -> JSX
spinner possibleBody = DOM.div
  { className: "spinner-border"
  , role: "status"
  }
  [ DOM.span { className: "visually-hidden" }
      [ fromMaybe (DOOM.text "Loading...") possibleBody ]
  ]

type LinkOptionalPropsRow =
  ( extraClassNames :: String
  , disabled :: Boolean
  , showBorders :: Boolean
  , tooltipText :: Maybe String
  , tooltipPlacement :: Maybe Placement
  )

defaultLinkOptionalProps :: { | LinkOptionalPropsRow }
defaultLinkOptionalProps =
  { extraClassNames: ""
  , disabled: false
  , showBorders: false
  , tooltipText: Nothing
  , tooltipPlacement: Nothing
  }

type LinkProps =
  ( label :: JSX
  , onClick :: Effect Unit
  | LinkWithIconOptionalProps
  )

link
  :: forall provided
   . Defaults { | LinkOptionalPropsRow } { | provided } { | LinkProps }
  => { | provided }
  -> JSX
link provided = do
  let
    { label, extraClassNames, onClick, disabled, showBorders } =
      defaults defaultLinkOptionalProps provided
    borderClasses =
      if showBorders then " border border-1 bg-light-hover"
      else " text-decoration-underline-hover"
    extraClassNames' = " " <> extraClassNames <> borderClasses <>
      if disabled then " disabled"
      else ""
  DOM.button
    { className: "btn btn-link text-decoration-none text-reset" <> extraClassNames'
    , onClick: handler preventDefault (const $ onClick)
    , type: "button"
    }
    [ label ]

type LinkWithIconOptionalProps = LinkOptionalPropsRow

type LinkWithIconProps =
  ( icon :: Icon
  , label :: JSX
  , onClick :: Effect Unit
  | LinkWithIconOptionalProps
  )

-- FIXME: We should just call `link` here and not repeat the code
linkWithIcon
  :: forall provided
   . Defaults { | LinkWithIconOptionalProps } { | provided } { | LinkWithIconProps }
  => { | provided }
  -> JSX
linkWithIcon provided = do
  let
    { icon, label, extraClassNames, onClick, disabled, showBorders, tooltipText, tooltipPlacement } =
      defaults defaultLinkOptionalProps provided
    borderClasses =
      if showBorders then " border border-1 bg-light-hover"
      else " text-decoration-underline-hover"
    extraClassNames' = " " <> extraClassNames <> borderClasses <>
      if disabled then " disabled"
      else ""
    button = DOM.button
      { className: "btn btn-link " <> extraClassNames'
      , onClick: handler preventDefault (const $ onClick)
      , type: "button"
      }
      [ Icons.toJSX icon
      , label
      ]
  case tooltipText of
    Just text -> do
      let
        placement :: Opt Placement
        placement = NoProblem.fromMaybe tooltipPlacement

        tooltipJSX :: { | TooltipPropsRow } -> JSX
        tooltipJSX props = tooltip props (DOOM.text text)
      DOM.div
        { className: "d-inline-block" }
        [ overlayTrigger
            { overlay: tooltipJSX
            , placement
            }
            \props -> DOM.span props [ button ]
        ]
    Nothing -> button

buttonWithIcon
  :: forall provided
   . Defaults { | LinkWithIconOptionalProps } { | provided } { | LinkWithIconProps }
  => { | provided }
  -> JSX
buttonWithIcon provided = do
  let
    { icon, label, extraClassNames, onClick, disabled } =
      defaults defaultLinkOptionalProps provided
    extraClassNames' = " " <> extraClassNames <>
      if disabled then " disabled"
      else ""
  DOM.button
    { className: "btn" <> extraClassNames'
    , onClick: handler preventDefault (const $ onClick)
    , type: "button"
    }
    [ Icons.toJSX icon
    , label
    ]

-- This is a strange naming convention - mixture of theming and particular use case.
-- I'm not able to do any better right now.
data OutlineColoring
  = OutlinePrimaryColoring -- We use this for `Advance`
  | OutlineInactiveColoring -- We use this for `Syncing`
  | OutlineWithdrawColoring -- We use this for `Withdraw`

outlineColoringToClassName :: OutlineColoring -> String
outlineColoringToClassName = case _ of
  OutlinePrimaryColoring -> "btn-outline-primary background-color-primary-light background-color-primary-hover font-color-white-hover"
  -- { className: "border border-dark rounded bg-white text-dark d-inline-block py-1 px-3 fw-bold" }
  OutlineInactiveColoring -> "border-dark text-darg bg-secondary"
  OutlineWithdrawColoring -> "btn-outline-danger color-withdrawal color-withdrawal-hover background-color-withdrawal-light background-color-withdrawal-hover"

type ButtonOutlinedProps =
  { coloring :: OutlineColoring
  , label :: JSX
  , onClick :: Opt (Effect Unit)
  , extraClassNames :: Opt String
  -- FIXME: Add tooltip support
  , disabled :: Opt Boolean
  , tooltipText :: Opt String
  , tooltipPlacement :: Opt Placement
  }

buttonOutlinedClassNames :: OutlineColoring -> String -> String
buttonOutlinedClassNames coloring extraClassNames =
  "btn font-weight-bold " <> outlineColoringToClassName coloring <> " " <> extraClassNames

buttonOutlined :: forall props. NoProblem.Coerce props ButtonOutlinedProps => props -> JSX
buttonOutlined props = do
  let
    props' :: ButtonOutlinedProps
    props' = NoProblem.coerce props
    { coloring, label, onClick } = props'
    button = DOM.button
      { className: buttonOutlinedClassNames coloring $ fromOpt "" props'.extraClassNames
      , onClick: handler preventDefault (const $ fromOpt (pure unit) onClick)
      , type: "button"
      , disabled: fromOpt false props'.disabled
      }
      [ label ]

  case NoProblem.toMaybe props'.tooltipText of
    Just tooltipText -> do
      let
        tooltipJSX p = tooltip p (DOOM.text tooltipText)
        button' p = DOM.span p [ button ]

      overlayTrigger
        { overlay: tooltipJSX
        , placement: props'.tooltipPlacement
        }
        button'
    Nothing -> button

buttonOutlinedPrimary
  :: forall props
   . NoProblem.Coerce { coloring :: OutlineColoring | props } ButtonOutlinedProps
  => Row.Lacks "coloring" props
  => Row.Cons "coloring" OutlineColoring props (coloring :: OutlineColoring | props)
  => { | props }
  -> JSX
buttonOutlinedPrimary props = do
  let
    props' :: { coloring :: OutlineColoring | props }
    props' = Record.insert (Proxy :: Proxy "coloring") OutlinePrimaryColoring props
  buttonOutlined props'

buttonOutlinedInactive
  :: forall props
   . NoProblem.Coerce { coloring :: OutlineColoring | props } ButtonOutlinedProps
  => Row.Lacks "coloring" props
  => Row.Cons "coloring" OutlineColoring props (coloring :: OutlineColoring | props)
  => { | props }
  -> JSX
buttonOutlinedInactive props = do
  let
    props' :: { coloring :: OutlineColoring | props }
    props' = Record.insert (Proxy :: Proxy "coloring") OutlineInactiveColoring props
  buttonOutlined props'

buttonOutlinedWithdraw
  :: forall props
   . NoProblem.Coerce { coloring :: OutlineColoring | props } ButtonOutlinedProps
  => Row.Lacks "coloring" props
  => Row.Cons "coloring" OutlineColoring props (coloring :: OutlineColoring | props)
  => { | props }
  -> JSX
buttonOutlinedWithdraw props = do
  let
    props' :: { coloring :: OutlineColoring | props }
    props' = Record.insert (Proxy :: Proxy "coloring") OutlineWithdrawColoring props
  buttonOutlined props'

data SpinnerOverlayHeight = Spinner100VH | SpinnerHeight100Percent

heightToClassName :: SpinnerOverlayHeight -> String
heightToClassName = case _ of
  Spinner100VH -> "min-height-100vh"
  SpinnerHeight100Percent -> "min-height-100-percent"

spinnerOverlay :: SpinnerOverlayHeight -> JSX
spinnerOverlay height = do
  let
    heightClassName = heightToClassName height
  DOM.div
    { className: "margin-top-minus-1 position-absolute top-0 w-100 d-flex justify-content-center align-items-center blur-bg z-index-sticky " <> heightClassName }
    $ loadingSpinnerLogo {}

marlowePreview :: V1.Contract -> JSX
marlowePreview contract = DOM.div
  { className: "overflow-auto hide-vertical-scroll border border-1 rounded w-100" }
  [ marloweYaml contract ]

marloweStatePreview :: V1.State -> JSX
marloweStatePreview state = DOM.div
  { className: "overflow-auto hide-vertical-scroll border border-1 rounded w-100" }
  [ marloweStateYaml state ]

submitButton
  :: { disabled :: Boolean
     , onClick :: EffectFn1 SyntheticEvent Unit
     }
  -> JSX
  -> JSX
submitButton { disabled, onClick } label = DOM.button
  do
    { className: "btn btn-primary w-100 py-2"
    , onClick: onClick
    , disabled
    }
  label

backToContractListLink :: Effect Unit -> JSX
backToContractListLink onDismiss = do
  DOM.div { className: "col-12 text-center" } $
    [ link
        { label: DOM.b {} [ DOOM.text "Back to contract list" ]
        , onClick: onDismiss
        , showBorders: false
        , extraClassNames: "mt-3"
        }
    ]

type LoadingOverlayProps =
  { active :: Opt Boolean
  , showSpinner :: Opt Boolean
  , top :: Opt Int
  }

renderOverlay
  :: forall props
   . NoProblem.Coerce props LoadingOverlayProps
  => props
  -> Array JSX
  -> JSX
renderOverlay props body = do
  let
    props' :: LoadingOverlayProps
    props' = NoProblem.coerce props

    overlaySpinner =
      if fromOpt true props'.showSpinner then LoadingOverlay.spinner.jsx $ loadingSpinnerLogo {}
      else LoadingOverlay.spinner.boolean false
    overlayStyles = do
      let
        overlay orig = do
          let
            -- heightValue = case possibleHeight of
            --   Nothing -> "100%"
            --   Just height -> show height <> "px"
            new = Object.fromHomogeneous
              { "background-color": "rgba(255, 255, 255, 0.9) !important"
              , top: (show $ fromOpt 0 props'.top) <> "px !important"
              }
            override = Object.unionWith (\_ n -> n)
          orig `override` new
      { overlay }
  LoadingOverlay.loadingOverlay
    { className: "blur-bg rounded", active: props'.active, spinner: overlaySpinner, styles: overlayStyles }
    body

-- newtype UseLoadingOverlay hooks = UseLoadingOverlay
--   ( UseState (Maybe Number) hooks
--     # UseState (Maybe DOM.Node)
--     # UseRef (Nullable DOM.Node)
--     # UseLayoutEffect Unit
--     # UseResizeObserver
--   )
-- 
-- 
-- derive instance Newtype (UseLoadingOverlay hooks) _
-- 
-- -- You can use this render to wrap a single element. Every element should have different key.
-- useLoadingOverlay :: forall props. NoProblem.Coerce props LoadingOverlayProps => Hook UseLoadingOverlay (props -> Array JSX -> JSX)
-- useLoadingOverlay = React.coerceHook React.do
--   possibleHeight /\ setHeight <- useState' Nothing
--   possibleNode /\ setNode <- useState' Nothing
--   wrapperRef <- useRef null
-- 
--   useLayoutEffect unit do
--     possibleRenderedNode <- readRefMaybe wrapperRef
--     setNode possibleRenderedNode
--     let
--       possibleElement = do
--         node <- possibleRenderedNode
--         DOM.Element.fromNode node
--     case possibleElement of
--       Just elem -> do
--         domRect <- DOM.Element.getBoundingClientRect elem
--         setHeight $ Just domRect.height
--       Nothing -> pure unit
--     pure $ pure unit
-- 
--   useResizeObserver $ { target: toNullable possibleNode, callback: _ } \entry -> do
--     setHeight $ Just entry.contentRect.height
-- 
--   pure $ \props body -> renderOverlay possibleHeight wrapperRef props body
-- 
-- -- --     setHeight(target.current.getBoundingClientRect())
-- -- --   }, [target])
-- -- 
-- -- 
-- -- 
-- -- -- import * as React from 'react'
-- -- -- import useResizeObserver from '@react-hook/resize-observer'
-- -- -- 
-- -- -- const useSize = (target) => {
-- -- --   const [size, setHeight] = React.useState()
-- -- -- 
-- -- --   React.useLayoutEffect(() => {
-- -- --     setHeight(target.current.getBoundingClientRect())
-- -- --   }, [target])
-- -- -- 
-- -- --   // Where the magic happens
-- -- --   useResizeObserver(target, (entry) => setHeight(entry.contentRect))
-- -- --   return size
-- -- -- }
-- -- -- 
-- -- -- const App = () => {
-- -- --   const target = React.useRef(null)
-- -- --   const size = useSize(target)
-- -- --   return (
-- -- --     <pre ref={target}>
-- -- --       {JSON.stringify({width: size.width, height: size.height}, null, 2)}
-- -- --     </pre>
-- -- --   )
-- -- -- }
-- -- 
