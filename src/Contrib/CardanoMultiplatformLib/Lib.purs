module CardanoMultiplatformLib.Lib
  ( props
  , Lib
  , Props
  ) where

import CardanoMultiplatformLib.Address as Address
import CardanoMultiplatformLib.Transaction as Transaction

type Props =
  { "Address" :: Address.Address
  , "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  , "TransactionUnspentOutput" :: Transaction.TransactionUnspentOutput
  }

newtype Lib = Lib
  { "Address" :: Address.Address
  , "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  , "TransactionUnspentOutput" :: Transaction.TransactionUnspentOutput
  }

props :: Lib -> Props
props (Lib r) = r

