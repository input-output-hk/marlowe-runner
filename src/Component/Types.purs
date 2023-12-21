module Component.Types
  ( AutoClose(..)
  , BrowserCapabilities(..)
  , ContractJsonString(..)
  , ConfigurationError(..)
  , ErrorReport(..)
  , ErrorReportLevel(..)
  , MkContextBase(..)
  , MkComponentMBase(..)
  , MkComponentM
  , MessageHub(..)
  , Message(..)
  , MessageContent'(..)
  , MessageEntry(..)
  , MessageId(..)
  , Page(..)
  , WalletInfo(..)
  , errorMsg
  , errorReportFromMsg
  , errorReportToMessage
  , messageToErrorReport
  , mkErrorReport
  , mkJSXErrorReport
  , module Exports
  ) where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.Types.ContractInfo (ContractInfo(..)) as Exports
import Contrib.Cardano (Slotting)
import Control.Monad.Reader (ReaderT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJsonField)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Types (ServerURL)
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX, ReactContext)
import React.Basic.DOM as D
import Wallet as Wallet
import WalletContext (WalletContext)
import Web.Clipboard (Clipboard)

newtype WalletInfo wallet = WalletInfo
  { name :: String
  , icon :: String
  , isEnabled :: Boolean
  , apiVersion :: String
  , wallet :: wallet
  }

derive instance Newtype (WalletInfo wallet) _

type MessageContent' =
  { msg :: JSX
  , description :: Maybe JSX
  , details :: Maybe Json
  }

data Message
  = Info MessageContent'
  | Success MessageContent'
  | Warning MessageContent'
  | Error MessageContent'

errorMsg :: String -> Message
errorMsg = Error <<< { msg: _, description: Nothing, details: Nothing } <<< D.text

type MessageId = Int

type MessageEntry =
  { id :: MessageId
  , msg :: Message
  }

newtype AutoClose = AutoClose Boolean

derive instance Newtype AutoClose _

newtype MessageHub = MessageHub
  -- { add :: AutoClose -> MessageEntry -> Effect Unit
  { add :: Message -> Effect Unit
  , remove :: MessageId -> Effect Unit
  , ctx :: ReactContext (List MessageEntry)
  }

-- We use this to report problems on the app level.
data ErrorReportLevel
  = ErrorReportWarning
  | ErrorReportError
  | ErrorReportCritical

derive instance Eq ErrorReportLevel
derive instance Ord ErrorReportLevel

instance EncodeJson ErrorReportLevel where
  encodeJson = encodeJson <<< case _ of
    ErrorReportWarning -> "warning"
    ErrorReportError -> "error"
    ErrorReportCritical -> "critical"

instance DecodeJson ErrorReportLevel where
  decodeJson = decodeJson >=> case _ of
    "warning" -> pure ErrorReportWarning
    "error" -> pure ErrorReportError
    "critical" -> pure ErrorReportCritical
    str -> Left $ TypeMismatch $ "Unknown error report level: " <> show str

newtype ErrorReport doc = ErrorReport
  { severity :: ErrorReportLevel
  , msg :: doc
  , description :: Maybe doc
  , details :: Maybe Json
  }

derive instance Newtype (ErrorReport doc) _
derive instance Functor ErrorReport
derive instance Foldable ErrorReport
derive instance Traversable ErrorReport
derive newtype instance EncodeJson doc => EncodeJson (ErrorReport doc)
derive newtype instance (DecodeJson doc, DecodeJsonField doc) => DecodeJson (ErrorReport doc)

mkErrorReport :: forall doc. doc -> Maybe doc -> Maybe Json -> ErrorReport doc
mkErrorReport msg description details = ErrorReport
  { severity: ErrorReportError
  , msg
  , description
  , details
  }

mkJSXErrorReport :: String -> Maybe String -> Maybe Json -> ErrorReport JSX
mkJSXErrorReport msg description details = mkErrorReport msg description details <#> D.text

errorReportFromMsg :: forall doc. doc -> ErrorReport doc
errorReportFromMsg = ErrorReport
  <<< { severity: ErrorReportError, msg: _, description: Nothing, details: Nothing }

errorReportToMessage :: ErrorReport JSX -> Message
errorReportToMessage (ErrorReport { severity, msg, description, details }) = do
  let
    description' = description
    r = { msg, description: description', details }
  case severity of
    ErrorReportWarning -> Warning r
    ErrorReportError -> Error r
    ErrorReportCritical -> Error r

messageToErrorReport :: Message -> Maybe (ErrorReport JSX)
messageToErrorReport = do
  let
    errorReport severity msg description details = ErrorReport
      { severity, msg, description: description, details }
  case _ of
    Error { msg, description, details } -> do
      Just $ errorReport ErrorReportError msg description details
    Warning { msg, description, details } ->
      Just $ errorReport ErrorReportWarning msg description details
    _ -> Nothing

type BrowserCapabilities =
  { clipboard :: Maybe Clipboard
  }

type MkContextBase r =
  { browserCapabilities :: BrowserCapabilities
  , cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , walletInfoCtx :: ReactContext (Maybe (WalletInfo Wallet.Api /\ WalletContext))
  -- FIXME: use more advanced logger so we use levels and setup app verbosity.
  , logger :: String -> Effect Unit
  , runtime :: Runtime
  , msgHub :: MessageHub
  , slotting :: Slotting
  , develMode :: Boolean
  , networkId :: Runtime.NetworkId

  | r
  }

-- We use this monad during creation of the components.
-- This gives us ability to pass down "static" data.
-- which is not changing during the lifetime of the component.
-- `props` can change.
type MkComponentMBase r = ReaderT (MkContextBase r) Effect

type MkComponentM = MkComponentMBase ()

newtype ContractJsonString = ContractJsonString String

derive instance Eq ContractJsonString
derive instance Newtype ContractJsonString _

data Page
  = ContractListPage
  | LoginPage
  | CreateContractPage (Maybe ContractJsonString)
  | OtherPage

derive instance Eq Page

data ConfigurationError = RuntimeNotResponding ServerURL String

