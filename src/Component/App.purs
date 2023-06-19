module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.Footer (footer)
import Component.LandingPage (mkLandingPage)
import Component.MessageHub (mkMessageBox, mkMessagePreview)
import Component.Modal (Size(..), mkModal)
import Component.Types (ContractInfo(..), MessageContent(Success, Info), MessageHub(MessageHub), MkComponentMBase, WalletInfo(..))
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.Data.Map (New(..), Old(..), additions, deletions) as Map
import Contrib.Halogen.Subscription (MinInterval(..))
import Contrib.Halogen.Subscription (bindEffect, foldMapThrottle) as Subscription
import Contrib.React.Svg (SvgUrl(..), svgImg)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map (catMaybes, empty, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now (now)
import Halogen.Subscription (Emitter) as Subscription
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsEvent, ContractWithTransactionsMap, ContractWithTransactionsStream(..))
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic as ReactContext
import React.Basic.DOM (div, img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, readRef, useEffect, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Offcanvas (offcanvas)
import ReactBootstrap.Offcanvas as Offcanvas
import Record as Record
import Type.Prelude (Proxy(..))
import Utils.React.Basic.Hooks (useEmitter', useLoopAff, useStateRef, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))
import WalletContext as WalletContext
import Web.HTML (window)

-- | Debugging helpers which allow us to automatically connect wallet
data WalletBrand
  = Lace
  | Yoroi
  | Nami
  | Eternl

instance Show WalletBrand where
  show Yoroi = "Yoroi"
  show Nami = "Nami"
  show Lace = "Lace"
  show Eternl = "Eternl"

autoConnectWallet :: WalletBrand -> (WalletInfo Wallet.Api -> Effect Unit) -> Aff Unit
autoConnectWallet walletBrand onSuccess = liftEffect (window >>= Wallet.cardano) >>= case _ of
  Nothing -> do
    -- We use this function in development mode, so we can just throw an error
    liftEffect $ throw $ "Missing \"cardano\" window attr"
  Just cardano -> do
    let
      extractWallet = case walletBrand of
        Lace -> Wallet.lace
        Nami -> Wallet.nami
        Yoroi -> Wallet.yoroi
        Eternl -> Wallet.eternl
    liftEffect (extractWallet cardano) >>= traverse walletInfo >>= case _ of
      Nothing -> do
        liftEffect $ throw $ "Unable to extract wallet " <> show walletBrand
      Just walletInfo@(WalletInfo { wallet }) -> do
        Wallet.enable wallet >>= case _ of
          Right walletApi -> do
            let
              walletInfo' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) walletInfo
            liftEffect $ onSuccess walletInfo'
          -- FIXME: paluh - handle error
          Left _ -> pure unit

-- | Use this switch to autoconnect the wallet for testing.
debugWallet :: Maybe WalletBrand
debugWallet = Just Nami -- Eternl -- Nami -- Nothing

data DisplayOption = Default | About

derive instance Eq DisplayOption

type ContractInfoMap = Map Runtime.ContractId ContractInfo

-- On the app level we keep the previous wallet context
-- so we can reuse pieces of contract info from the previous
-- state in a safe manner.
newtype AppContractInfoMap = AppContractInfoMap
  { walletContext :: Maybe WalletContext
  , map :: ContractInfoMap
  }

mkApp :: MkComponentMBase () (Unit -> JSX)
mkApp = do
  landingPage <- mkLandingPage
  messageBox <- liftEffect $ mkMessageBox
  messagePreview <- liftEffect $ mkMessagePreview
  modal <- liftEffect $ mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  subcomponents <- do
    contractListComponent <- mkContractList
    -- eventListComponent <- mkEventList
    connectWallet <- mkConnectWallet
    pure { contractListComponent, connectWallet, messageBox }

  (ContractWithTransactionsStream contractStream) <- asks _.contractStream

  throttledEmitter :: Subscription.Emitter (List ContractWithTransactionsEvent) <- liftEffect $
    Subscription.foldMapThrottle (List.singleton) (MinInterval $ Milliseconds 1_000.0) contractStream.emitter

  initialVersion <- liftEffect now

  walletInfoCtx <- asks _.walletInfoCtx
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  about <- asks _.aboutMarkdown

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    let
      walletInfoName = _.name <<< un WalletInfo <$> possibleWalletInfo

    possibleWalletInfoRef <- useStateRef walletInfoName possibleWalletInfo
    possibleWalletContext /\ setWalletContext <- useState' Nothing
    possibleWalletContextRef <- useStateRef' possibleWalletContext

    useLoopAff walletInfoName (Milliseconds 20_000.0) do
      pwi <- liftEffect $ readRef possibleWalletInfoRef
      pwc <- liftEffect $ readRef possibleWalletContextRef
      case pwi, pwc of
        Nothing, Nothing -> pure unit
        Nothing, Just _ -> do
          liftEffect $ setWalletContext Nothing
        Just (WalletInfo walletInfo), _ -> do
          let
            action = do
              walletContext <- WalletContext.walletContext cardanoMultiplatformLib walletInfo.wallet
              liftEffect $ setWalletContext $ Just walletContext
          action `catchError` \_ -> do
            -- FIXME: Report back (to the reporting backend) a wallet problem?
            traceM "ERROR during wallet context construction"
            pure unit

    configuringWallet /\ setConfiguringWallet <- useState' false
    checkingNotifications /\ setCheckingNotifications <- useState' false
    displayOption /\ setDisplayOption <- useState' Default

    -- We are ignoring contract events for now and we update the whole contractInfo set.
    upstreamVersion <- useEmitter' initialVersion (Subscription.bindEffect (const $ now) throttledEmitter)
    upstreamVersionRef <- useStateRef' upstreamVersion

    -- Let's use versioning so we avoid large comparison.
    (version /\ contractMap) /\ setContractMap <- useState' (upstreamVersion /\ AppContractInfoMap { walletContext: possibleWalletContext, map: Map.empty })
    idRef <- useStateRef version contractMap

    useEffect (upstreamVersion /\ possibleWalletContext) do
      updates <- contractStream.getLiveState
      old <- readRef idRef
      newVersion <- readRef upstreamVersionRef
      let
        new = updateAppContractInfoMap old possibleWalletContext updates
        _map (AppContractInfoMap { map }) = map

        old' = _map old
        new' = _map new

        (additionsNumber :: Int) = length $ Map.additions (Map.Old old') (Map.New new')
        (deletionsNumber :: Int) = length $ Map.deletions (Map.Old old') (Map.New new')

      when (deletionsNumber > 0 || additionsNumber > 0) do
        msgHubProps.add $ Info $ DOOM.text $
          "New contracts: " <> show additionsNumber <> ", deleted contracts: " <> show deletionsNumber

      setContractMap (newVersion /\ new)
      pure $ pure unit

    -- -- This causes a lot of re renders - we avoid it for now by
    -- -- enforcing manual offcanvas toggling.
    -- -- FIXME: expose the msgs emitter and use it to detect
    -- -- when message box is empty.
    -- msgs <- useContext msgHubProps.ctx
    -- useEffect (List.null msgs) do
    --   when (List.null msgs) do
    --     setCheckingNotifications false
    --   pure $ pure unit

    useAff unit $ for debugWallet \walletBrand ->
      autoConnectWallet walletBrand \walletInfo -> do
        liftEffect $ setWalletInfo $ Just walletInfo

    let
      AppContractInfoMap { map: contracts } = contractMap

    pure $ case possibleWalletInfo of
      Nothing -> landingPage { setWalletInfo: setWalletInfo <<< Just }
      _ -> provider walletInfoCtx ((/\) <$> possibleWalletInfo <*> possibleWalletContext) $ Array.singleton $ DOM.div { className: "mt-6" } $
        [ DOM.div { className: "fixed-top" }
            [ DOM.nav { className: "navbar mb-lg-3 navbar-expand-sm navbar-light bg-light shadow-bottom" } $
                DOM.div { className: "container-fluid" }
                  [ DOM.a { href: "#", className: "navbar-brand" }
                      [ svgImg { src: marloweLogoUrl } ]
                  , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                      [ DOM.ul { className: "navbar-nav gap-2" }
                          [ DOM.li { className: "nav-item" } $ ReactContext.consumer msgHubProps.ctx \msgs ->
                              [ linkWithIcon
                                  { icon: if List.null msgs then unsafeIcon "bell-slash disabled-color h5" else unsafeIcon "bell-fill primary-color h5"
                                  , label: mempty
                                  , extraClassNames: "nav-link"
                                  , tooltipText: Just $ if List.null msgs then "No new notifications" else "You have new notifications"
                                  , onClick: setCheckingNotifications true
                                  , disabled: List.null msgs
                                  }
                              ]
                          -- FIXME: This should be moved to submenu
                          -- , DOM.li { className: "nav-item" } $
                          --     linkWithIcon
                          --       { icon: Icons.cashStack
                          --       , label: DOOM.text "Cash flows"
                          --       , extraClassNames: "nav-link"
                          --       , onClick: pure unit
                          --       }
                          , DOM.li { className: "nav-item" } $
                              case possibleWalletInfo of
                                Just (WalletInfo wallet) -> link
                                  { label: DOM.span { className: "h5" }
                                      [ DOOM.img { src: wallet.icon, alt: wallet.name, className: "w-1_2rem me-1" }
                                      , DOOM.span_ [ DOOM.text $ wallet.name <> " wallet" ]
                                      ]
                                  , extraClassNames: "nav-link"
                                  , onClick: setConfiguringWallet true
                                  }
                                Nothing -> linkWithIcon
                                  { icon: Icons.wallet2
                                  , label: DOOM.text "Connect Wallet"
                                  , extraClassNames: "nav-link"
                                  , onClick: setConfiguringWallet true
                                  }
                          ]
                      ]
                  ]
            , DOM.div { className: "container-xl" }
                $ DOM.div { className: "row" }
                $ messagePreview msgHub
            ]
        , ReactContext.consumer msgHubProps.ctx \_ ->
            pure $ offcanvas
              { onHide: setCheckingNotifications false
              , placement: Offcanvas.placement.end
              , show: checkingNotifications -- && (not $ List.null msgs)
              , scroll: false
              }
              [ DOM.div { className: "p-3 overflow-auto" } $ messageBox msgHub
              ]
        , Monoid.guard (displayOption == About)
            $ modal
            $
              { onDismiss: setDisplayOption Default
              , title: DOOM.text "Marlowe Run Light"
              , body: DOOM.div { dangerouslySetInnerHTML: { __html: about } }
              , size: Large
              }
        , Monoid.guard configuringWallet do
            let
              jsx = subcomponents.connectWallet
                { currentlyConnected: possibleWalletInfo
                , onWalletConnect: \result -> do
                    case result of
                      ConnectWallet.Connected walletInfo -> do
                        let
                          WalletInfo { name } = walletInfo
                        msgHubProps.add $ Success $ DOOM.text $ "Connected to " <> name
                        setWalletInfo (Just walletInfo)
                      ConnectWallet.ConnectionError _ -> pure unit
                      ConnectWallet.NoWallets -> pure unit
                    setConfiguringWallet false
                , onDismiss: setConfiguringWallet false
                , inModal: true
                }
            jsx
        , do
            let
              contractArray = Array.fromFoldable contracts
            subcomponents.contractListComponent { contracts: contractArray, connectedWallet: possibleWalletInfo }
              -- renderTab props children = tab props $ DOM.div { className: "row pt-4" } children

        --          [ tabs { fill: true, justify: true, defaultActiveKey: "contracts" }
        --              [ renderTab
        --                  { eventKey: "contracts"
        --                  , title: DOM.div
        --                      { className: "text-body" }
        --                      [ DOM.span { className: "me-2" } $ Icons.toJSX Icons.files
        --                      , DOOM.text "Contracts"
        --                      ]
        --                  }
        --                  $ subcomponents.contractListComponent { contractList: contractArray, connectedWallet: possibleWalletInfo }
        --              -- , renderTab
        --              --     { eventKey: "cash-flows"
        --              --     , title: DOM.div
        --              --         { className: "text-body" }
        --              --         [ DOM.span { className: "me-2" } $ Icons.toJSX Icons.arrowDownShort
        --              --         , DOOM.text "Apply Inputs"
        --              --         ]
        --
        --              --     }
        --              --     $ subcomponents.eventListComponent { contractList: contractArray, connectedWallet: possibleWalletInfo }
        --              ]
        --          ]
        , footer
        ]

-- TODO: Currently we ignore role tokens.
updateAppContractInfoMap :: AppContractInfoMap -> Maybe WalletContext -> ContractWithTransactionsMap -> AppContractInfoMap
updateAppContractInfoMap (AppContractInfoMap { walletContext: prevWalletContext, map: prev }) walletContext updates = do
  let
    walletChanged = prevWalletContext /= walletContext
    usedAddresses = fromMaybe [] $ _.usedAddresses <<< un WalletContext <$> walletContext

    map = Map.catMaybes $ updates <#> \{ contract: { resource: contractHeader@(Runtime.ContractHeader { contractId, block }), links: endpoints }, contractState, transactions } -> do
      let
        marloweInfo = do
          Runtime.ContractState contractState' <- contractState
          pure $ MarloweInfo
            { initialContract: contractState'.initialContract
            , state: contractState'.state
            , currentContract: contractState'.currentContract
            }

      case contractId `Map.lookup` prev of
        Just (ContractInfo contractInfo) -> do
          pure $ ContractInfo $ contractInfo
            { marloweInfo = marloweInfo
            , _runtime
                { contractHeader = contractHeader
                , transactions = transactions
                }
            }
        Nothing -> do
          let
            Runtime.ContractHeader { contractId } = contractHeader
          pure $ ContractInfo $
            { contractId
            , endpoints
            , marloweInfo
            , _runtime: { contractHeader, transactions }
            }
  AppContractInfoMap { walletContext, map }

marloweLogoUrl :: SvgUrl
marloweLogoUrl = SvgUrl "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTYwIiBoZWlnaHQ9IjQyIiB2aWV3Qm94PSIwIDAgMTYwIDQyIiBmaWxsPSJub25lIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGRhdGEtbG9nbz0idHJ1ZSI+PGcgY2xpcC1wYXRoPSJ1cmwoI2xvZ29fX2EpIj48cGF0aCBkPSJNNjkuNzEgMjIuMzUydjkuMTJoLTIuOXYtOC43MzRjMC0yLjgzNi0xLjg2OS0zLjc3Mi0zLjQ0OS0zLjc3Mi0xLjU4IDAtMy40MTcuOTM2LTMuNDE3IDMuNzcydjguNzM1aC0yLjkwMXYtOC43MzVjMC0yLjgzNi0xLjg2OS0zLjc3Mi0zLjQ4LTMuNzcyLTEuNjEgMC0zLjQxNy45MzYtMy40MTcgMy43NzJ2OC43MzVoLTIuOTAxVjE2LjY0NmgyLjkwMXYxLjc3MmMxLjA2My0xLjM4NyAyLjY3NC0yIDQuMjU0LTIgMS44NjggMCAzLjgwMy43MSA0LjggMi42MTMgMS4xMy0xLjkzNSAzLjE2LTIuNjEzIDQuOTk4LTIuNjEzIDIuNzA4IDAgNS41MSAxLjQ4NCA1LjUxIDUuOTNsLjAwMy4wMDNaTTg3LjQ2OSAxNi42NXYxNC44MjZoLTIuOTAxdi0yLjI4OWMtMS4wOTUgMS41OC0yLjgwNSAyLjU0Ny01LjA5NCAyLjU0Ny00LjE4OCAwLTcuMTg1LTMuMjU2LTcuMTg1LTcuNjcxIDAtNC40MTUgMi45OTctNy42NCA3LjE4NS03LjY0IDIuMjg5IDAgMy45OTYuOTY3IDUuMDk0IDIuNTEydi0yLjI4OGgyLjkwMXYuMDAzWm0tMi45MDEgNy40MTNjMC0yLjg2Ny0xLjg2OS00Ljg5Ny00LjcwNS00Ljg5Ny0yLjgzNSAwLTQuNzM5IDIuMDMtNC43MzkgNC44OTcgMCAyLjg2NyAxLjg3IDQuOTMyIDQuNzQgNC45MzJzNC43MDQtMi4wOTYgNC43MDQtNC45MzJaTTk4LjY1IDE2LjQyM3YyLjc0Yy0yIC4wMy01LjIyLjktNS4yMiA0LjcwNHY3LjYwNWgtMi45MDJWMTYuNjQ2aDIuOTAydjIuNzRjMS4xNi0yLjAzIDMuMTU5LTIuOTMyIDUuMjItMi45Njd2LjAwNFpNMTAwLjc0NiAzMS40NzZWMTAuNTI0aDIuOTMybC0uMDMxIDIwLjk0OWgtMi45MDF2LjAwM1pNMTA1Ljk1MyAyNC4wNjNjMC00LjQ4IDMuMTI1LTcuNzAyIDcuNjA2LTcuNzAyczcuNTQgMy4yMjQgNy41NCA3LjcwMmMwIDQuNDc3LTMuMDk0IDcuNzAyLTcuNTQgNy43MDJzLTcuNjA2LTMuMjg3LTcuNjA2LTcuNzAyWm0xMi4yOCAwYzAtMi44MzYtMS44NjktNC44OTctNC42NzQtNC44OTctMi44MDUgMC00LjcwNSAyLjA2MS00LjcwNSA0Ljg5NyAwIDIuODM2IDEuODY5IDQuOTMyIDQuNzA1IDQuOTMyIDIuODM2IDAgNC42NzQtMi4xMjcgNC42NzQtNC45MzJaTTE0NC4yODEgMTYuNjVsLTQuOTMxIDE0LjgyNmgtMi44NjdsLTMuNDgtMTAuMjQ5LTMuNTEzIDEwLjI0OWgtMi44NjdsLTQuODY2LTE0LjgyNmgzLjAyOGwzLjQ0OCAxMC43MzQgMy43MzgtMTAuNzM0aDIuMTI3bDMuNjc1IDEwLjczNCAzLjQ4LTEwLjczNGgzLjAyOFpNMTU5LjI0NSAyNC4wNjN2LjgzNmgtMTEuNjY3Yy4yODkgMi42NDMgMi4xNTggNC4zNSA0LjczOSA0LjM1IDIuMTI3IDAgMy40NzktLjk2NyA0LjE4OC0ybDIuMDMxIDEuNDVjLTEuMjkxIDEuODY4LTMuNTE0IDMuMDYzLTYuMjE5IDMuMDYzLTQuNTEyIDAtNy41NC0zLjI4Ny03LjU0LTcuNzAzIDAtNC40MTUgMi45MDEtNy42NyA3LjM0Ny03LjY3IDQuNDQ3IDAgNy4xMjQgMy4yNTUgNy4xMjQgNy42N2wtLjAwMy4wMDRaTTE0Ny41NzggMjNoOC45NThjLS4zNTQtMi40ODItMS45MzQtNC4yODktNC40MTUtNC4yODlzLTQuMTg4IDEuNzA3LTQuNTQzIDQuMjg5WiIgZmlsbD0idmFyKC0tdGV4dC1jb2xvciwjMUMxQzFDKSI+PC9wYXRoPjxwYXRoIGQ9Ik0yMS4yMTcgMS43NDggMTguMTg1IDAgMCAxMC41djIxbC42OTkuNDAzIDIuMzMzIDEuMzQ1TDE4LjE4NSA0Mmw2LjA2NC0zLjV2LTIxbC0xMi4xMjUtNy0zLjAzMiAxLjc0OCAxMi4xMjUgN3YxNy41TDE4LjE4NSAzOC41IDMuMDMyIDI5Ljc0OHYtMTcuNUwxOC4xODUgMy41bDE1LjE1NiA4Ljc0OHYyMWwzLjAzMi0xLjc0OHYtMjFMMjEuMjE3IDEuNzQ4WiIgZmlsbD0iIzUxMUNGNyI+PC9wYXRoPjxwYXRoIGQ9Im0xNS4xNTYgOC43NDggMTIuMTIxIDd2MjFMMzAuMzEgMzVWMTRMMTguMTg1IDdsLTMuMDI5IDEuNzQ4WiIgZmlsbD0iIzUxMUNGNyI+PC9wYXRoPjwvZz48ZGVmcz48Y2xpcFBhdGggaWQ9ImxvZ29fX2EiPjxwYXRoIGZpbGw9IiNmZmYiIGQ9Ik0wIDBoMTU5LjI0NXY0MkgweiI+PC9wYXRoPjwvY2xpcFBhdGg+PC9kZWZzPjwvc3ZnPg=="
