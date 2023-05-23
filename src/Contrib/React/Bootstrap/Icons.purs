module Contrib.React.Bootstrap.Icons
  ( Icon
  , toJSX
  , arrowDownShort
  , arrowUpShort
  , bell
  , bellFill
  , bellSlash
  , bellSlashFill
  , bullsEye
  , cashStack
  , checkCircle
  , checkCircleFill
  , listOl
  , exclamationTriangle
  , exclamationTriangleFill
  , eye
  , eyeFill
  , infoSquare
  , infoCircle
  , infoCircleFill
  , fileEarmarkPlus
  , files
  , wallet2
  ) where

import Prelude

import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX)

unsafeIcon :: String -> Icon
unsafeIcon name = Icon $ DOM.span
  { className: "bi-" <> name
  }
  ([] :: Array JSX)

newtype Icon = Icon JSX

toJSX :: Icon -> JSX
toJSX (Icon jsx) = jsx

listOl :: Icon
listOl = unsafeIcon "list-ol"

arrowDownShort :: Icon
arrowDownShort = unsafeIcon "arrow-down-short"

arrowUpShort :: Icon
arrowUpShort = unsafeIcon "arrow-up-short"

bell :: Icon
bell = unsafeIcon "bell"

bellFill :: Icon
bellFill = unsafeIcon "bell-fill"

bellSlash :: Icon
bellSlash = unsafeIcon "bell-slash"

bellSlashFill :: Icon
bellSlashFill = unsafeIcon "bell-slash-fill"

bullsEye :: Icon
bullsEye = unsafeIcon "bullseye"

cashStack :: Icon
cashStack = unsafeIcon "cash-stack"

checkCircle :: Icon
checkCircle = unsafeIcon "check-circle"

checkCircleFill :: Icon
checkCircleFill = unsafeIcon "check-circle-fill"

exclamationTriangle :: Icon
exclamationTriangle = unsafeIcon "exclamation-triangle"

exclamationTriangleFill :: Icon
exclamationTriangleFill = unsafeIcon "exclamation-triangle-fill"

eye :: Icon
eye = unsafeIcon "eye"

eyeFill :: Icon
eyeFill = unsafeIcon "eye-fill"

fileEarmarkPlus :: Icon
fileEarmarkPlus = unsafeIcon "file-earmark-plus"

files :: Icon
files = unsafeIcon "files"

infoSquare :: Icon
infoSquare = unsafeIcon "info-square"

infoCircle :: Icon
infoCircle = unsafeIcon "info-circle"

infoCircleFill :: Icon
infoCircleFill = unsafeIcon "info-circle-fill"

wallet2 :: Icon
wallet2 = unsafeIcon "wallet2"

