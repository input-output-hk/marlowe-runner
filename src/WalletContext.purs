module WalletContext where

import Prelude

import CardanoMultiplatformLib (Bech32, addressObject, allocate, asksLib, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Wallet (fromSomeAddress)
import Wallet as Wallet

newtype WalletContext = WalletContext
  { -- , balance :: V1.Assets
    changeAddress :: Maybe Bech32
  , usedAddresses :: Array Bech32
  }

derive instance Newtype WalletContext _
derive newtype instance Show WalletContext
derive newtype instance Eq WalletContext
derive newtype instance Ord WalletContext

changeAddress :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Maybe Bech32)
changeAddress cardanoMultiplatformLib wallet = do
  possibleChangeAddress <- Wallet.getChangeAddress wallet
  case possibleChangeAddress of
    Right someAddr -> liftEffect do
      fromSomeAddress cardanoMultiplatformLib someAddr
    _ -> pure Nothing

walletAddresses :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Array Bech32)
walletAddresses cardanoMultiplatformLib wallet = do
  possibleUsedAddresses <- Wallet.getUsedAddresses wallet
  possibleUTxOs <- Wallet.getUtxos wallet

  case possibleUsedAddresses, possibleUTxOs of
    Right addresses, Right (Just utxos) -> do
      utxoAddresses' <- liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
        for utxos \utxo -> do
          let
            utxo' = cborHexToCbor utxo
          unspentTxOutObj <- allocate $ transactionUnspentOutput.from_bytes _TransactionUnspentOutput utxo'
          txOutObj <- allocate $ transactionUnspentOutputObject.output unspentTxOutObj
          addressObj <- allocate $ transactionOutputObject.address txOutObj
          liftEffect $ addressObject.to_bech32 addressObj NoProblem.undefined

      addresses' <- liftEffect $ Array.catMaybes <$> for addresses \someAddress -> do
        fromSomeAddress cardanoMultiplatformLib someAddress

      pure $ Array.nub $ utxoAddresses' <> addresses'
    _, _ -> do
      pure []

walletContext :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff WalletContext
walletContext cardanoMultiplatformLib wallet = do
  usedAddresses <- walletAddresses cardanoMultiplatformLib wallet
  chAddr <- changeAddress cardanoMultiplatformLib wallet
  pure $ WalletContext
    { changeAddress: chAddr
    , usedAddresses
    }
