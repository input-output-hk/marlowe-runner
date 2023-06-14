module WalletContext where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex, addressObject, allocate, asksLib, runGarbageCollector, valueFromCbor)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionUnspentOutputObject, ValueObject, transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject)
import CardanoMultiplatformLib.Types (Cbor, cborHexToCbor)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt.Argonaut
import Data.Either (Either(..), fromRight)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Wallet (fromSomeAddress)
import Wallet as Wallet


newtype WalletContext = WalletContext
  { balance :: Map.Map String (Map.Map String BigInt.Argonaut.BigInt)
  , changeAddress :: Maybe Bech32
  , usedAddresses :: Array Bech32
  }

derive instance Newtype WalletContext _
derive newtype instance Show WalletContext
derive newtype instance Eq WalletContext
derive newtype instance Ord WalletContext

walletBalance :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Map.Map String (Map.Map String BigInt.Argonaut.BigInt))
walletBalance cardanoMultiplatformLib wallet = do
  Wallet.getBalance wallet >>= case _ of
    Right valueCborHex -> do
      let
        valueCbor :: Cbor ValueObject
        valueCbor = cborHexToCbor valueCborHex
      liftEffect $ runGarbageCollector cardanoMultiplatformLib $ valueFromCbor valueCbor
    Left _ -> pure Map.empty

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

  let
    addresses = fromRight [] possibleUsedAddresses
    utxos = fromRight [] (map fold possibleUTxOs)
  utxoAddresses' <- liftEffect $ runGarbageCollector cardanoMultiplatformLib do
    _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
    for utxos \(utxo :: CborHex TransactionUnspentOutputObject) -> do
      let
        utxo' = cborHexToCbor utxo
      unspentTxOutObj <- allocate $ transactionUnspentOutput.from_bytes _TransactionUnspentOutput utxo'
      txOutObj <- allocate $ transactionUnspentOutputObject.output unspentTxOutObj
      addressObj <- allocate $ transactionOutputObject.address txOutObj
      liftEffect $ addressObject.to_bech32 addressObj NoProblem.undefined
  addresses' <- liftEffect $ Array.catMaybes <$> for addresses \someAddress -> do
    fromSomeAddress cardanoMultiplatformLib someAddress
  pure $ Array.nub $ utxoAddresses' <> addresses'

walletContext :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff WalletContext
walletContext cardanoMultiplatformLib wallet = do
  balance <- walletBalance cardanoMultiplatformLib wallet
  usedAddresses <- walletAddresses cardanoMultiplatformLib wallet
  chAddr <- changeAddress cardanoMultiplatformLib wallet

  pure $ WalletContext
    { balance
    , changeAddress: chAddr
    , usedAddresses
    }
