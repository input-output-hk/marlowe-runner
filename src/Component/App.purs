module Component.App where

import Prelude

import CardanoMultiplatformLib.Types (Bech32)
import Component.Assets.Svgs (marloweLogoUrl)
import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (ModalAction(..), mkContractList)
import Component.CreateContract (ContractJsonString)
import Component.Footer (footer)
import Component.Footer as Footer
import Component.LandingPage (mkLandingPage)
import Component.MessageHub (mkMessageBox, mkMessagePreview)
import Component.Modal (Size(..), mkModal)
import Component.Types (ContractInfo(..), MessageContent(Success), MessageHub(MessageHub), MkComponentMBase, WalletInfo(..))
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.Cardano (AssetId(..), NonAdaAssets(..), nonAdaAssets)
import Contrib.React.Svg (svgImg)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Loops (untilJust)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Map (Map)
import Data.Map (catMaybes, keys) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, supervise)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Language.Marlowe.Core.V1.Semantics (emptyState) as V1
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsMap, ContractWithTransactionsStream(..), MaxPages(..), PollingInterval(..), RequestInterval(..))
import Marlowe.Runtime.Web.Streaming as Streaming
import Marlowe.Runtime.Web.Types (BlockHeader(..), BlockNumber(..), ContractHeader(..), PolicyId(..), Runtime(..))
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic as ReactContext
import React.Basic.DOM (div, img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, readRef, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Offcanvas (offcanvas)
import ReactBootstrap.Offcanvas as Offcanvas
import Record as Record
import Type.Prelude (Proxy(..))
import Utils.React.Basic.Hooks (useLoopAff, useStateRef, useStateRef')
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
debugWallet = Just Nami -- Just Lace -- Nami -- Eternl -- Nami -- Nothing

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

type Props = { possibleInitialContract :: Maybe ContractJsonString }

mkApp :: MkComponentMBase () (Props -> JSX)
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

  walletInfoCtx <- asks _.walletInfoCtx
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  about <- asks _.aboutMarkdown
  Runtime runtime <- asks _.runtime

  liftEffect $ component "App" \props -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    let
      walletInfoName = _.name <<< un WalletInfo <$> possibleWalletInfo

    possibleWalletInfoRef <- useStateRef walletInfoName possibleWalletInfo
    possibleWalletContext /\ setWalletContext <- useState' Nothing
    possibleWalletContextRef <- useStateRef' possibleWalletContext

    possibleContractMap /\ setContractMap <- useState' Nothing

    let
      walletCtx = un WalletContext <$> possibleWalletContext

    useAff ((\w -> w.usedAddresses /\ w.changeAddress) <$> walletCtx) do
      let
        (usedAddresses :: Array Bech32) = fromMaybe [] $ _.usedAddresses <$> walletCtx
        (tokens :: Array AssetId) = map (uncurry AssetId) <<< fromMaybe [] $ Array.fromFoldable <<< Map.keys <<< un NonAdaAssets <<< nonAdaAssets <<< _.balance <$> walletCtx

        reqInterval = RequestInterval (Milliseconds 50.0)
        pollInterval = PollingInterval (Milliseconds 60_000.0)
        filterContracts getContractResponse = case un ContractHeader getContractResponse.resource of
          { block: Nothing } -> true
          { block: Just (BlockHeader { blockNo: BlockNumber blockNo }) } -> blockNo > 909000 -- 904279
        maxPages = Just (MaxPages 1)
        params = { partyAddresses: usedAddresses, partyRoles: tokens, tags: [] }

      ContractWithTransactionsStream contractStream <- Streaming.mkContractsWithTransactions pollInterval reqInterval params filterContracts maxPages runtime.serverURL
      supervise do
        void $ forkAff do
          untilJust do
            updates <- liftEffect $ contractStream.getLiveState
            let
              new = mkAppContractInfoMap possibleWalletContext updates
            liftEffect $ setContractMap $ Just new
            delay (Milliseconds 1_000.0)
            pure Nothing

        contractStream.start

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
              liftEffect $ setWalletContext walletContext
          action `catchError` \_ -> do
            -- FIXME: Report back (to the reporting backend) a wallet problem?
            traceM "ERROR during wallet context construction"
            pure unit

    configuringWallet /\ setConfiguringWallet <- useState' false
    checkingNotifications /\ setCheckingNotifications <- useState' false
    displayOption /\ setDisplayOption <- useState' Default

    -- TODO: re-introduce notifications
    -- We are ignoring contract events for now and we update the whole contractInfo set.
    --    upstreamVersion <- useEmitter' initialVersion (Subscription.bindEffect (const now) emitter)
    --    upstreamVersionRef <- useStateRef' upstreamVersion
    --
    --    -- Let's use versioning so we avoid large comparison.
    --    (version /\ contractMap) /\ setContractMap <- useState' (upstreamVersion /\ AppContractInfoMap { walletContext: possibleWalletContext, map: Map.empty })
    --    idRef <- useStateRef version contractMap
    --
    --    useEffect (upstreamVersion /\ possibleWalletContext) do
    --      updates <- contractStream.getLiveState
    --      old <- readRef idRef
    --      newVersion <- readRef upstreamVersionRef
    --      let
    --        new = updateAppContractInfoMap old possibleWalletContext updates
    --        _map (AppContractInfoMap { map }) = map
    --
    --        old' = _map old
    --        new' = _map new
    --
    --        (additionsNumber :: Int) = length $ Map.additions (Map.Old old') (Map.New new')
    --        (deletionsNumber :: Int) = length $ Map.deletions (Map.Old old') (Map.New new')
    --
    --      when (deletionsNumber > 0 || additionsNumber > 0) do
    --        msgHubProps.add $ Info $ DOOM.text $
    --          "Update: "
    --            <> (if deletionsNumber == 0 then "" else show deletionsNumber <> " contracts removed")
    --            <> (if deletionsNumber > 0 && additionsNumber > 0 then ", " else "")
    --            <> (if additionsNumber == 0 then "" else show additionsNumber <> " contracts discovered")
    --            <> "."
    --
    --
    --      setContractMap (newVersion /\ new)
    --      pure $ pure unit

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
      possibleContracts = do
        AppContractInfoMap { map: contracts } <- possibleContractMap
        pure contracts

    pure $ case possibleWalletInfo of
      Nothing -> landingPage { setWalletInfo: setWalletInfo <<< Just }
      _ -> provider walletInfoCtx ((/\) <$> possibleWalletInfo <*> possibleWalletContext) $
        [ DOM.nav { className: "navbar navbar-expand-sm navbar-light bg-light shadow-bottom fixed-top" } $
            DOM.div { className: "container-fluid" }
              [ DOM.a { href: "#", className: "navbar-brand p-0" }
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
        , DOM.div { className: "position-fixed mt-2 position-left-50 transform-translate-x--50 z-index-popover" }
            $ DOM.div { className: "container-xl" }
            $ DOM.div { className: "row" }
            $ messagePreview msgHub
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
              contractArray = Array.fromFoldable <$> possibleContracts
            subcomponents.contractListComponent
              { possibleContracts: contractArray
              -- if version == initialVersion then Nothing
              -- else Just contractArray
              , connectedWallet: possibleWalletInfo
              , possibleInitialModalAction: (NewContract <<< Just) <$> props.possibleInitialContract
              }
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
        , footer (Footer.Fixed true)
        ]

mkAppContractInfoMap :: Maybe WalletContext -> ContractWithTransactionsMap -> AppContractInfoMap
mkAppContractInfoMap walletContext updates = do
  let
    -- walletCtx = un WalletContext <$> walletContext
    -- (usedAddresses :: Array String) = map bech32ToString $ fromMaybe [] $ _.usedAddresses <$> walletCtx
    -- (tokens :: Array String) = map Cardano.assetIdToString $ fromMaybe [] $ Array.fromFoldable <<< Map.keys <<< un Cardano.Value <<< _.balance <$> walletCtx

    map = Map.catMaybes $ updates <#> \{ contract: { resource: contractHeader@(Runtime.ContractHeader { contractId, roleTokenMintingPolicyId, tags }), links: endpoints }, contractState, transactions } -> do
      let
        marloweInfo = do
          Runtime.ContractState contractState' <- contractState
          pure $ MarloweInfo
            { initialContract: contractState'.initialContract
            , currencySymbol: case roleTokenMintingPolicyId of
                PolicyId "" -> Nothing
                PolicyId policyId -> Just $ policyId
            , state: contractState'.state
            , currentContract: contractState'.currentContract
            , initialState: V1.emptyState -- FIXME: No initial state on the API LEVEL?
            , unclaimedPayouts: contractState'.unclaimedPayouts
            }

      -- let
      --   keepContract =
      --     case marloweInfo of
      --       Just (MarloweInfo { initialContract })
      --         | (not $ Array.null $ Array.intersect usedAddresses (addressesInContract initialContract))
      --           || (not $ Array.null $ Array.intersect tokens (rolesInContract initialContract)) -> Just true
      --       Just _ -> Just false
      --       _ -> Nothing

      let Runtime.ContractHeader { contractId } = contractHeader

      pure $ ContractInfo $
        { contractId
        , endpoints
        , marloweInfo
        , tags
        , _runtime: { contractHeader, transactions }
        }

  AppContractInfoMap { walletContext, map }
