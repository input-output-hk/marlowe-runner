module Contrib.React.Markdown where

import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

-- export type ReactMarkdownProps = {
--   node: Element
--   children: ReactNode[]
--   /**
--    * Passed when `options.rawSourcePos` is given
--    */
--   sourcePosition?: Position
--   /**
--    * Passed when `options.includeElementIndex` is given
--    */
--   index?: number
--   /**
--    * Passed when `options.includeElementIndex` is given
--    */
--   siblingCount?: number
-- }

type Props_markdown =
  (children :: Array JSX)

foreign import _Markdown :: ReactComponent { | Props_markdown }

_internalmarkdown :: forall attrs attrs_. Row.Union attrs attrs_ Props_markdown => ReactComponent { | attrs }
_internalmarkdown = unsafeCoerce _Markdown

markdown
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_markdown
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
markdown props children = element _internalmarkdown propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
