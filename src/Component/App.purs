module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
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
-- debugWallet = Just Eternl -- Nami -- Nothing
debugWallet = Nothing

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

data Route = LandingPage | AppPage

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
    currentRoute /\ setCurrentRoute <- useState' LandingPage

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

    pure $ case currentRoute of
      LandingPage -> landingPage { routeToApp: setCurrentRoute AppPage }
      _ -> provider walletInfoCtx ((/\) <$> possibleWalletInfo <*> possibleWalletContext) $ Array.singleton $ DOM.div { className: "mt-6" } $
        [ DOM.div { className: "fixed-top" }
            [ DOM.nav { className: "navbar mb-lg-3 navbar-expand-sm navbar-light bg-light" } $
                DOM.div { className: "container-xl" }
                  [ DOM.a { href: "#", className: "navbar-brand" }
                      [ svgImg { src: marloweLogoUrl, height: "30px", className: "me-2" }
                      , DOOM.text "marlowe"
                      ]
                  , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                      [ DOM.ul { className: "navbar-nav gap-2" }
                          [ DOM.li { className: "nav-item" } $
                              linkWithIcon
                                { icon: Icons.infoSquare
                                , label: (DOOM.text "About")
                                , extraClassNames: "nav-link"
                                , onClick: setDisplayOption About
                                }
                          , DOM.li { className: "nav-item" } $ ReactContext.consumer msgHubProps.ctx \msgs ->
                              [ linkWithIcon
                                  { icon: if List.null msgs then Icons.bellSlash else Icons.bellFill
                                  , label: DOOM.text "Notifications"
                                  , extraClassNames: "nav-link"
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
                                  { label: DOOM.span_
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
        , DOM.div { className: "container-xl" } do
            let
              -- renderTab props children = tab props $ DOM.div { className: "row pt-4" } children
              contractArray = Array.fromFoldable contracts
            subcomponents.contractListComponent { contracts: contractArray, connectedWallet: possibleWalletInfo }

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
marloweLogoUrl = SvgUrl "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMzciIGhlaWdodD0iNDMiIHZpZXdCb3g9IjAgMCAzNyA0MyIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZD0iTTIxLjU1MzIgMS43NzIyMkwxOC40ODI1IDBMMC4wNjMyMzI0IDEwLjYzNDlWMzEuOTAzMUwwLjc3MDkwNSAzMi4zMTE2TDMuMTMzODcgMzMuNjc1M0wxOC40ODI1IDQyLjUzNzlMMjQuNjIyMyAzOC45OTM1VjM1LjQ0NzVWMTcuNzIzOEwxMi4zNDI3IDEwLjYzNDlMOS4yNzM2MyAxMi40MDcxTDIxLjU1MzIgMTkuNDk2VjM3LjIxOTdMMTguNDgyNSAzOC45OTM1TDMuMTMzODcgMzAuMTMwOFYxMi40MDcxTDE4LjQ4MjUgMy41NDQ0NUwzMy44MzI3IDEyLjQwNzFWMzMuNjc1M0wzNi45MDE4IDMxLjkwMzFWMTAuNjM0OUwyMS41NTMyIDEuNzcyMjJaIiBmaWxsPSIjNTExQ0Y3Ii8+CjxwYXRoIGQ9Ik0xNS40MTM4IDguODYyNTVMMjcuNjkzMyAxNS45NTE0VjM3LjIxOTZMMzAuNzYyNSAzNS40NDc0VjE0LjE3OTJMMTguNDgyOSA3LjA4ODgxTDE1LjQxMzggOC44NjI1NVoiIGZpbGw9IiM1MTFDRjciLz4KPC9zdmc+Cg=="
