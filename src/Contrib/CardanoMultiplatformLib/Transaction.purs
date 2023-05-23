module CardanoMultiplatformLib.Transaction where

import Prelude

import CardanoMultiplatformLib.Address (AddressObject)
import CardanoMultiplatformLib.Types (Cbor, JsonString)
import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Newtype (class Newtype)
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectMth3, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

-- export class Transaction {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Transaction}
-- */
--   static from_bytes(bytes: Uint8Array): Transaction;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionJSON}
-- */
--   to_js_value(): TransactionJSON;
-- /**
-- * @param {string} json
-- * @returns {Transaction}
-- */
--   static from_json(json: string): Transaction;
-- /**
-- * @returns {TransactionBody}
-- */
--   body(): TransactionBody;
-- /**
-- * @returns {TransactionWitnessSet}
-- */
--   witness_set(): TransactionWitnessSet;
-- /**
-- * @returns {boolean}
-- */
--   is_valid(): boolean;
-- /**
-- * @returns {AuxiliaryData | undefined}
-- */
--   auxiliary_data(): AuxiliaryData | undefined;
-- /**
-- * @param {boolean} valid
-- */
--   set_is_valid(valid: boolean): void;
-- /**
-- * @param {TransactionBody} body
-- * @param {TransactionWitnessSet} witness_set
-- * @param {AuxiliaryData | undefined} auxiliary_data
-- * @returns {Transaction}
-- */
--   static new(body: TransactionBody, witness_set: TransactionWitnessSet, auxiliary_data?: AuxiliaryData): Transaction;
-- }

newtype AuxiliaryData = AuxiliaryData (JSObject ())

derive instance Newtype AuxiliaryData _

newtype Transaction = Transaction
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionObject) TransactionObject
      , from_json :: EffectMth1 JsonString TransactionObject
      , new :: EffectMth3 TransactionBodyObject TransactionWitnessSetObject (Opt AuxiliaryData) TransactionObject
      )
  )

derive instance Newtype Transaction _

transaction
  :: { from_bytes :: Transaction -> Cbor TransactionObject -> Effect TransactionObject
     , from_json :: Transaction -> JsonString -> Effect TransactionObject
     , new :: Transaction -> TransactionBodyObject -> TransactionWitnessSetObject -> Opt AuxiliaryData -> Effect TransactionObject
     }
transaction = mkNewtypedFFI (Proxy :: Proxy Transaction)

newtype TransactionObject = TransactionObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor TransactionObject)
      , to_json :: EffectMth0 JsonString
      , auxiliary_data :: EffectMth0 (Opt AuxiliaryData)
      -- | Clone the tx body
      , body :: EffectMth0 TransactionBodyObject
      )
  )

derive instance Newtype TransactionObject _

transactionObject
  :: { free :: TransactionObject -> Effect Unit
     , to_bytes :: TransactionObject -> Effect (Cbor TransactionObject)
     , to_json :: TransactionObject -> Effect JsonString
     , auxiliary_data :: TransactionObject -> Effect (Opt AuxiliaryData)
     , body :: TransactionObject -> Effect TransactionBodyObject
     }
transactionObject = mkNewtypedFFI (Proxy :: Proxy TransactionObject)

-- export class TransactionBody {
--   free(): void;
--   to_bytes(): Uint8Array;
--
--   static from_bytes(bytes: Uint8Array): TransactionBody;
--
--   to_js_value(): TransactionBodyJSON;
--
--   static from_json(json: string): TransactionBody;
--
--   inputs(): TransactionInputs;
--
--   outputs(): TransactionOutputs;
--
--   fee(): BigNum;
--
--   ttl(): BigNum | undefined;
--
--   set_certs(certs: Certificates): void;
--
--   certs(): Certificates | undefined;
--
--   set_withdrawals(withdrawals: Withdrawals): void;
--
--   withdrawals(): Withdrawals | undefined;
--
--   set_update(update: Update): void;
--
--   update(): Update | undefined;
--
--   set_auxiliary_data_hash(auxiliary_data_hash: AuxiliaryDataHash): void;
--
--   auxiliary_data_hash(): AuxiliaryDataHash | undefined;
--
--   set_validity_start_interval(validity_start_interval: BigNum): void;
--
--   validity_start_interval(): BigNum | undefined;
--
--   set_mint(mint: Mint): void;
--
--   mint(): Mint | undefined;
--
--   multiassets(): Mint | undefined;
--
--   set_script_data_hash(script_data_hash: ScriptDataHash): void;
--
--   script_data_hash(): ScriptDataHash | undefined;
--
--   set_collateral(collateral: TransactionInputs): void;
--
--   collateral(): TransactionInputs | undefined;
--
--   set_required_signers(required_signers: Ed25519KeyHashes): void;
--
--   required_signers(): Ed25519KeyHashes | undefined;
--
--   set_network_id(network_id: NetworkId): void;
--
--   network_id(): NetworkId | undefined;
--
--   set_collateral_return(collateral_return: TransactionOutput): void;
--
--   collateral_return(): TransactionOutput | undefined;
--
--   set_total_collateral(total_collateral: BigNum): void;
--
--   total_collateral(): BigNum | undefined;
--
--   set_reference_inputs(reference_inputs: TransactionInputs): void;
--
--   reference_inputs(): TransactionInputs | undefined;
--
--   static new(inputs: TransactionInputs, outputs: TransactionOutputs, fee: BigNum, ttl?: BigNum): TransactionBody;
-- }

newtype TransactionBody = TransactionBody
  ( JSObject
      ( from_bytes :: EffectMth1 Uint8Array TransactionBodyObject
      , from_json :: EffectMth1 JsonString TransactionBodyObject
      )
  )

derive instance Newtype TransactionBody _

transactionBody
  :: { from_bytes :: TransactionBody -> Uint8Array -> Effect TransactionBodyObject
     , from_json :: TransactionBody -> JsonString -> Effect TransactionBodyObject
     }
transactionBody = mkNewtypedFFI (Proxy :: Proxy TransactionBody)

newtype TransactionBodyObject = TransactionBodyObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_js_value :: EffectMth0 Json -- TransactionBodyJSON
      )
  )

derive instance Newtype TransactionBodyObject _

transactionBodyObject
  :: { free :: TransactionBodyObject -> Effect Unit
     , to_js_value :: TransactionBodyObject -> Effect Json
     }
transactionBodyObject = mkNewtypedFFI (Proxy :: Proxy TransactionBodyObject)

-- export class TransactionWitnessSet {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionWitnessSet}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionWitnessSet;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionWitnessSetJSON}
-- */
--   to_js_value(): TransactionWitnessSetJSON;
-- /**
-- * @param {string} json
-- * @returns {TransactionWitnessSet}
-- */
--   static from_json(json: string): TransactionWitnessSet;
-- /**
-- * @param {Vkeywitnesses} vkeys
-- */
--   set_vkeys(vkeys: Vkeywitnesses): void;
-- /**
-- * @returns {Vkeywitnesses | undefined}
-- */
--   vkeys(): Vkeywitnesses | undefined;
-- /**
-- * @param {NativeScripts} native_scripts
-- */
--   set_native_scripts(native_scripts: NativeScripts): void;
-- /**
-- * @returns {NativeScripts | undefined}
-- */
--   native_scripts(): NativeScripts | undefined;
-- /**
-- * @param {BootstrapWitnesses} bootstraps
-- */
--   set_bootstraps(bootstraps: BootstrapWitnesses): void;
-- /**
-- * @returns {BootstrapWitnesses | undefined}
-- */
--   bootstraps(): BootstrapWitnesses | undefined;
-- /**
-- * @param {PlutusV1Scripts} plutus_v1_scripts
-- */
--   set_plutus_v1_scripts(plutus_v1_scripts: PlutusV1Scripts): void;
-- /**
-- * @returns {PlutusV1Scripts | undefined}
-- */
--   plutus_v1_scripts(): PlutusV1Scripts | undefined;
-- /**
-- * @param {PlutusList} plutus_data
-- */
--   set_plutus_data(plutus_data: PlutusList): void;
-- /**
-- * @returns {PlutusList | undefined}
-- */
--   plutus_data(): PlutusList | undefined;
-- /**
-- * @param {Redeemers} redeemers
-- */
--   set_redeemers(redeemers: Redeemers): void;
-- /**
-- * @returns {Redeemers | undefined}
-- */
--   redeemers(): Redeemers | undefined;
-- /**
-- * @param {PlutusV2Scripts} plutus_v2_scripts
-- */
--   set_plutus_v2_scripts(plutus_v2_scripts: PlutusV2Scripts): void;
-- /**
-- * @returns {PlutusV2Scripts | undefined}
-- */
--   plutus_v2_scripts(): PlutusV2Scripts | undefined;
-- /**
-- * @returns {TransactionWitnessSet}
-- */
--   static new(): TransactionWitnessSet;
-- }

newtype TransactionWitnessSet = TransactionWitnessSet
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionWitnessSetObject) TransactionWitnessSetObject
      , from_json :: EffectMth1 JsonString TransactionWitnessSetObject
      )
  )

derive instance Newtype TransactionWitnessSet _

transactionWitnessSet
  :: { from_bytes :: TransactionWitnessSet -> (Cbor TransactionWitnessSetObject) -> Effect TransactionWitnessSetObject
     , from_json :: TransactionWitnessSet -> JsonString -> Effect TransactionWitnessSetObject
     }
transactionWitnessSet = mkNewtypedFFI (Proxy :: Proxy TransactionWitnessSet)

newtype TransactionWitnessSetObject = TransactionWitnessSetObject (JSObject (free :: EffectMth0 Unit))

derive instance Newtype TransactionWitnessSetObject _

transactionWitnessObject
  :: { free :: TransactionWitnessSetObject -> Effect Unit
     }
transactionWitnessObject = mkNewtypedFFI (Proxy :: Proxy TransactionWitnessSetObject)

-- export class TransactionOutput {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionOutput}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionOutput;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionOutputJSON}
-- */
--   to_js_value(): TransactionOutputJSON;
-- /**
-- * @param {string} json
-- * @returns {TransactionOutput}
-- */
--   static from_json(json: string): TransactionOutput;
-- /**
-- * @returns {Address}
-- */
--   address(): Address;
-- /**
-- * @returns {Value}
-- */
--   amount(): Value;
-- /**
-- * @returns {Datum | undefined}
-- */
--   datum(): Datum | undefined;
-- /**
-- * @param {Datum} data
-- */
--   set_datum(data: Datum): void;
-- /**
-- * @returns {ScriptRef | undefined}
-- */
--   script_ref(): ScriptRef | undefined;
-- /**
-- * @param {ScriptRef} script_ref
-- */
--   set_script_ref(script_ref: ScriptRef): void;
-- /**
-- * @param {Address} address
-- * @param {Value} amount
-- * @returns {TransactionOutput}
-- */
--   static new(address: Address, amount: Value): TransactionOutput;
-- }
foreign import data ValueObject :: Type

newtype TransactionOutput = TransactionOutput
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionOutputObject) TransactionOutputObject
      , from_json :: EffectMth1 JsonString TransactionOutputObject
      , new :: EffectMth2 AddressObject ValueObject TransactionOutputObject
      )
  )

derive instance Newtype TransactionOutput _

transactionOutput
  :: { from_bytes :: TransactionOutput -> (Cbor TransactionOutputObject) -> Effect TransactionOutputObject
     , from_json :: TransactionOutput -> JsonString -> Effect TransactionOutputObject
     , new :: TransactionOutput -> AddressObject -> ValueObject -> Effect TransactionOutputObject
     }
transactionOutput = mkNewtypedFFI (Proxy :: Proxy TransactionOutput)

newtype TransactionOutputObject = TransactionOutputObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , address :: EffectMth0 AddressObject
      )
  )

derive instance Newtype TransactionOutputObject _

transactionOutputObject
  :: { free :: TransactionOutputObject -> Effect Unit
     , address :: TransactionOutputObject -> Effect AddressObject
     }
transactionOutputObject = mkNewtypedFFI (Proxy :: Proxy TransactionOutputObject)

-- export class TransactionUnspentOutput {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionUnspentOutput}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionUnspentOutput;
-- /**
-- * @param {TransactionInput} input
-- * @param {TransactionOutput} output
-- * @returns {TransactionUnspentOutput}
-- */
--   static new(input: TransactionInput, output: TransactionOutput): TransactionUnspentOutput;
-- /**
-- * @returns {TransactionInput}
-- */
--   input(): TransactionInput;
-- /**
-- * @returns {TransactionOutput}
-- */
--   output(): TransactionOutput;
-- }

-- FIXME: missing binding.
foreign import data TransactionInputObject :: Type

newtype TransactionUnspentOutput = TransactionUnspentOutput
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor TransactionUnspentOutputObject) TransactionUnspentOutputObject
      , new :: EffectMth2 TransactionInputObject TransactionOutput TransactionUnspentOutputObject
      )
  )

derive instance Newtype TransactionUnspentOutput _

transactionUnspentOutput
  :: { from_bytes :: TransactionUnspentOutput -> (Cbor TransactionUnspentOutputObject) -> Effect TransactionUnspentOutputObject
     , new :: TransactionUnspentOutput -> TransactionInputObject -> TransactionOutput -> Effect TransactionUnspentOutputObject
     }
transactionUnspentOutput = mkNewtypedFFI (Proxy :: Proxy TransactionUnspentOutput)

newtype TransactionUnspentOutputObject = TransactionUnspentOutputObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , input :: EffectMth0 TransactionInputObject
      , output :: EffectMth0 TransactionOutputObject
      )
  )

derive instance Newtype TransactionUnspentOutputObject _

transactionUnspentOutputObject
  :: { free :: TransactionUnspentOutputObject -> Effect Unit
     , input :: TransactionUnspentOutputObject -> Effect TransactionInputObject
     , output :: TransactionUnspentOutputObject -> Effect TransactionOutputObject
     }
transactionUnspentOutputObject = mkNewtypedFFI (Proxy :: Proxy TransactionUnspentOutputObject)

-- Just a stub
foreign import data TransactionHashObject :: Type
