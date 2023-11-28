module Hooks.ReactHookz.UseResizeObserver where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Debug (traceM)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import React.Basic.Hooks (Hook, unsafeHook)
import Web.DOM as DOM
import Web.ResizeObserver (ResizeObserverEntry)

-- * target RefObject<Element> | Element | null - element to track.
-- * callback (entry: ResizeObserverEntry) => void - Callback that will be invoked on resize.
-- * enabled boolean (default: true) - Whether resize observer is enabled or n

-- | Props for useResizeObserver hook
type UseResizeObserverProps =
  { target :: Nullable DOM.Node
  , callback :: (ResizeObserverEntry -> Effect Unit)
  , enabled :: Opt Boolean
  }

foreign import data UseResizeObserver :: Type -> Type

foreign import useResizeObserver_ :: EffectFn3 (Nullable DOM.Node) (ResizeObserverEntry -> Effect Unit) (Opt Boolean) Unit

useResizeObserver
  :: forall props
   . NoProblem.Coerce props UseResizeObserverProps
  => props
  -> Hook UseResizeObserver Unit
useResizeObserver props = do
  let
    props' :: UseResizeObserverProps
    props' = NoProblem.coerce props
  unsafeHook do
    runEffectFn3 useResizeObserver_ props'.target props'.callback props'.enabled
