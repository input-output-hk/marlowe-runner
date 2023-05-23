module Component.CreateContract.Types where

import Prelude

import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows)
import Marlowe.Runtime.Web.Types (TxOutRef)
import Type.Row (type (+))

data ContractFormTypeChoice
  = AmortizingLoans AmortizingLoanChoice
  | JsonForm
  | Bonds
  | BulletLoans
  | CapitalizingLoans
  | ZeroCouponBonds

derive instance Eq ContractFormTypeChoice
derive instance Ord ContractFormTypeChoice
derive instance Generic ContractFormTypeChoice _
instance Show ContractFormTypeChoice where
  show = genericShow

instance Enum ContractFormTypeChoice where
  succ = genericSucc
  pred = genericPred

instance Bounded ContractFormTypeChoice where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum ContractFormTypeChoice where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data AmortizingLoanChoice
  = PrincipalAtMaturity
  | LinearAmortizer
  | NegativeAmortizer
  | Annuity

derive instance Eq AmortizingLoanChoice
derive instance Ord AmortizingLoanChoice
derive instance Generic AmortizingLoanChoice _
instance Show AmortizingLoanChoice where
  show = genericShow

instance Enum AmortizingLoanChoice where
  succ = genericSucc
  pred = genericPred

instance Bounded AmortizingLoanChoice where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum AmortizingLoanChoice where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

type ContractFormTypeChoiceRow r =
  ( contractFormTypeChoice :: ContractFormTypeChoice
  | r
  )

type ThirdStepBaseRow r =
  ( contractTerms :: ContractTerms
  | r
  )

type FourthStepBaseRow r =
  ( cashFlows :: CashFlows
  , contract :: V1.Contract
  , counterParty :: V1.Party
  , party :: V1.Party

  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  , collateralUTxOs :: Array TxOutRef
  | ThirdStepBaseRow + r
  )

data WizzardStep
  = FirstStep
  | SecondStep ContractFormTypeChoice
  | ThirdStep { | ThirdStepBaseRow + ContractFormTypeChoiceRow + () }
  | FourthStep { | FourthStepBaseRow + ContractFormTypeChoiceRow + () }

