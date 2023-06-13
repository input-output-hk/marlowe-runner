module CardanoMultiplatformLib.Transaction where

import Prelude

import CardanoMultiplatformLib.Address (AddressObject)
import CardanoMultiplatformLib.Types (Bech32, Cbor, CborHex, JsonString)
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

-- export class Value {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Value}
-- */
--   static from_bytes(bytes: Uint8Array): Value;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {ValueJSON}
-- */
--   to_js_value(): ValueJSON;
-- /**
-- * @param {string} json
-- * @returns {Value}
-- */
--   static from_json(json: string): Value;
-- /**
-- * @param {BigNum} coin
-- * @returns {Value}
-- */
--   static new(coin: BigNum): Value;
-- /**
-- * @param {MultiAsset} multiasset
-- * @returns {Value}
-- */
--   static new_from_assets(multiasset: MultiAsset): Value;
-- /**
-- * @returns {Value}
-- */
--   static zero(): Value;
-- /**
-- * @returns {boolean}
-- */
--   is_zero(): boolean;
-- /**
-- * @returns {BigNum}
-- */
--   coin(): BigNum;
-- /**
-- * @param {BigNum} coin
-- */
--   set_coin(coin: BigNum): void;
-- /**
-- * @returns {MultiAsset | undefined}
-- */
--   multiasset(): MultiAsset | undefined;
-- /**
-- * @param {MultiAsset} multiasset
-- */
--   set_multiasset(multiasset: MultiAsset): void;
-- /**
-- * @param {Value} rhs
-- * @returns {Value}
-- */
--   checked_add(rhs: Value): Value;
-- /**
-- * @param {Value} rhs_value
-- * @returns {Value}
-- */
--   checked_sub(rhs_value: Value): Value;
-- /**
-- * @param {Value} rhs_value
-- * @returns {Value}
-- */
--   clamped_sub(rhs_value: Value): Value;
-- /**
-- * note: values are only partially comparable
-- * @param {Value} rhs_value
-- * @returns {number | undefined}
-- */
--   compare(rhs_value: Value): number | undefined;
-- }

newtype Value = Value
  (JSObject
    ( from_bytes :: EffectMth1 (Cbor ValueObject) ValueObject
    , from_json :: EffectMth1 JsonString ValueObject
    , new_from_assets :: EffectMth1 MultiAssetObject ValueObject
    -- , new :: EffectMth1 BigNum Value
    )
  )

derive instance Newtype Value _

value
  :: { from_bytes :: Value -> (Cbor ValueObject) -> Effect ValueObject
     , from_json :: Value -> JsonString -> Effect ValueObject
     , new_from_assets :: Value -> MultiAssetObject -> Effect ValueObject
     -- , new :: BigNum -> Effect Value
     }
value = mkNewtypedFFI (Proxy :: Proxy Value)

newtype ValueObject = ValueObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 (Cbor ValueObject)
      , to_json :: EffectMth0 JsonString
      , to_js_value :: EffectMth0 Json
      , is_zero :: EffectMth0 Boolean
      -- , coin :: EffectMth0 BigNum
      -- , set_coin :: EffectMth1 BigNum Unit
      , multiasset :: EffectMth0 (Opt MultiAssetObject)
      , set_multiasset :: EffectMth1 MultiAssetObject Unit
      , checked_add :: EffectMth1 ValueObject ValueObject
      , checked_sub :: EffectMth1 ValueObject ValueObject
      , clamped_sub :: EffectMth1 ValueObject ValueObject
      , compare :: EffectMth1 ValueObject (Opt Int)
      )
  )

derive instance Newtype ValueObject _

valueObject
  :: { free :: ValueObject -> Effect Unit
     , to_bytes :: ValueObject -> Effect (Cbor ValueObject)
     , to_json :: ValueObject -> Effect JsonString
     , to_js_value :: ValueObject -> Effect Json
     , is_zero :: ValueObject -> Effect Boolean
     -- , coin :: ValueObject -> Effect BigNum
     -- , set_coin :: ValueObject -> BigNum -> Effect Unit
     , multiasset :: ValueObject -> Effect (Opt MultiAssetObject)
     , set_multiasset :: ValueObject -> MultiAssetObject -> Effect Unit
     , checked_add :: ValueObject -> ValueObject -> Effect ValueObject
     , checked_sub :: ValueObject -> ValueObject -> Effect ValueObject
     , clamped_sub :: ValueObject -> ValueObject -> Effect ValueObject
     , compare :: ValueObject -> ValueObject -> Effect (Opt Int)
     }
valueObject = mkNewtypedFFI (Proxy :: Proxy ValueObject)

-- export class MultiAsset {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {MultiAsset}
-- */
--   static from_bytes(bytes: Uint8Array): MultiAsset;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {MultiAssetJSON}
-- */
--   to_js_value(): MultiAssetJSON;
-- /**
-- * @param {string} json
-- * @returns {MultiAsset}
-- */
--   static from_json(json: string): MultiAsset;
-- /**
-- * @returns {MultiAsset}
-- */
--   static new(): MultiAsset;
-- /**
-- * the number of unique policy IDs in the multiasset
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * set (and replace if it exists) all assets with policy {policy_id} to a copy of {assets}
-- * @param {ScriptHash} policy_id
-- * @param {Assets} assets
-- * @returns {Assets | undefined}
-- */
--   insert(policy_id: ScriptHash, assets: Assets): Assets | undefined;
-- /**
-- * all assets under {policy_id}, if any exist, or else None (undefined in JS)
-- * @param {ScriptHash} policy_id
-- * @returns {Assets | undefined}
-- */
--   get(policy_id: ScriptHash): Assets | undefined;
-- /**
-- * sets the asset {asset_name} to {value} under policy {policy_id}
-- * returns the previous amount if it was set, or else None (undefined in JS)
-- * @param {ScriptHash} policy_id
-- * @param {AssetName} asset_name
-- * @param {BigNum} value
-- * @returns {BigNum | undefined}
-- */
--   set_asset(policy_id: ScriptHash, asset_name: AssetName, value: BigNum): BigNum | undefined;
-- /**
-- * returns the amount of asset {asset_name} under policy {policy_id}
-- * If such an asset does not exist, 0 is returned.
-- * @param {ScriptHash} policy_id
-- * @param {AssetName} asset_name
-- * @returns {BigNum}
-- */
--   get_asset(policy_id: ScriptHash, asset_name: AssetName): BigNum;
-- /**
-- * returns all policy IDs used by assets in this multiasset
-- * @returns {ScriptHashes}
-- */
--   keys(): ScriptHashes;
-- /**
-- * removes an asset from the list if the result is 0 or less
-- * does not modify this object, instead the result is returned
-- * @param {MultiAsset} rhs_ma
-- * @returns {MultiAsset}
-- */
--   sub(rhs_ma: MultiAsset): MultiAsset;
-- }

newtype MultiAsset = MultiAsset
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor MultiAsset) MultiAssetObject
    , from_json :: EffectMth1 JsonString MultiAssetObject
    , new :: EffectMth0 MultiAssetObject
    )
  )

derive instance Newtype MultiAsset _

multiAsset
  :: { from_bytes :: MultiAsset -> Cbor MultiAsset -> Effect MultiAssetObject
     , from_json :: MultiAsset -> JsonString -> Effect MultiAssetObject
     , new :: MultiAsset -> Effect MultiAssetObject
     }
multiAsset = mkNewtypedFFI (Proxy :: Proxy MultiAsset)

newtype MultiAssetObject = MultiAssetObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor MultiAssetObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , insert :: EffectMth2 ScriptHash Assets (Opt Assets)
    , get :: EffectMth1 ScriptHash (Opt Assets)
    -- , set_asset :: EffectMth3 ScriptHash AssetName BigNum (Opt BigNum)
    -- , get_asset :: EffectMth2 ScriptHash AssetName BigNum
    , keys :: EffectMth0 ScriptHashes
    , sub :: EffectMth1 MultiAssetObject MultiAssetObject
    )
  )

derive instance Newtype MultiAssetObject _

multiAssetObject
  :: { free :: MultiAssetObject -> Effect Unit
     , to_bytes :: MultiAssetObject -> Effect (Cbor MultiAssetObject)
     , to_json :: MultiAssetObject -> Effect JsonString
     , to_js_value :: MultiAssetObject -> Effect Json
     , len :: MultiAssetObject -> Effect Int
     , insert :: MultiAssetObject -> ScriptHash -> Assets -> Effect (Opt Assets)
     , get :: MultiAssetObject -> ScriptHash -> Effect (Opt Assets)
     -- , set_asset :: MultiAssetObject -> ScriptHash -> AssetName -> BigNum -> Effect (Opt BigNum)
     -- , get_asset :: MultiAssetObject -> ScriptHash -> AssetName -> Effect BigNum
     , keys :: MultiAssetObject -> Effect ScriptHashes
     , sub :: MultiAssetObject -> MultiAssetObject -> Effect MultiAssetObject
     }
multiAssetObject = mkNewtypedFFI (Proxy :: Proxy MultiAssetObject)

-- export class ScriptHash {
--   free(): void;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {ScriptHash}
-- */
--   static from_bytes(bytes: Uint8Array): ScriptHash;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {string} prefix
-- * @returns {string}
-- */
--   to_bech32(prefix: string): string;
-- /**
-- * @param {string} bech_str
-- * @returns {ScriptHash}
-- */
--   static from_bech32(bech_str: string): ScriptHash;
-- /**
-- * @returns {string}
-- */
--   to_hex(): string;
-- /**
-- * @param {string} hex
-- * @returns {ScriptHash}
-- */
--   static from_hex(hex: string): ScriptHash;
-- }

newtype ScriptHash = ScriptHash
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor ScriptHashObject) ScriptHashObject
    , from_bech32 :: EffectMth1 Bech32 ScriptHashObject
    , from_hex :: EffectMth1 (CborHex ScriptHashObject) ScriptHashObject
    )
  )

derive instance Newtype ScriptHash _

scriptHash
  :: { from_bytes :: ScriptHash -> Cbor ScriptHashObject -> Effect ScriptHashObject
     , from_bech32 :: ScriptHash -> Bech32 -> Effect ScriptHashObject
     , from_hex :: ScriptHash -> CborHex ScriptHashObject -> Effect ScriptHashObject
     }
scriptHash = mkNewtypedFFI (Proxy :: Proxy ScriptHash)

newtype ScriptHashObject = ScriptHashObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor ScriptHashObject)
    , to_bech32 :: EffectMth1 Bech32 String
    , to_hex :: EffectMth0 (CborHex ScriptHashObject)
    )
  )

derive instance Newtype ScriptHashObject _

scriptHashObject
  :: { free :: ScriptHashObject -> Effect Unit
     , to_bytes :: ScriptHashObject -> Effect (Cbor ScriptHashObject)
     , to_bech32 :: ScriptHashObject -> Bech32 -> Effect String
     , to_hex :: ScriptHashObject -> Effect (CborHex ScriptHashObject)
     }
scriptHashObject = mkNewtypedFFI (Proxy :: Proxy ScriptHashObject)

-- export class ScriptHashes {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {ScriptHashes}
-- */
--   static from_bytes(bytes: Uint8Array): ScriptHashes;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {ScriptHashesJSON}
-- */
--   to_js_value(): ScriptHashesJSON;
-- /**
-- * @param {string} json
-- * @returns {ScriptHashes}
-- */
--   static from_json(json: string): ScriptHashes;
-- /**
-- * @returns {ScriptHashes}
-- */
--   static new(): ScriptHashes;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {number} index
-- * @returns {ScriptHash}
-- */
--   get(index: number): ScriptHash;
-- /**
-- * @param {ScriptHash} elem
-- */
--   add(elem: ScriptHash): void;
-- }

newtype ScriptHashes = ScriptHashes
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor ScriptHashesObject) ScriptHashesObject
    , from_json :: EffectMth1 JsonString ScriptHashesObject
    , new :: EffectMth0 ScriptHashesObject
    )
  )

derive instance Newtype ScriptHashes _

scriptHashes
  :: { from_bytes :: ScriptHashes -> Cbor ScriptHashesObject -> Effect ScriptHashesObject
     , from_json :: ScriptHashes -> JsonString -> Effect ScriptHashesObject
     , new :: ScriptHashes -> Effect ScriptHashesObject
     }
scriptHashes = mkNewtypedFFI (Proxy :: Proxy ScriptHashes)

newtype ScriptHashesObject = ScriptHashesObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor ScriptHashesObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , get :: EffectMth1 Int ScriptHash
    , add :: EffectMth1 ScriptHash Unit
    )
  )

derive instance Newtype ScriptHashesObject _

scriptHashesObject
  :: { free :: ScriptHashesObject -> Effect Unit
     , to_bytes :: ScriptHashesObject -> Effect (Cbor ScriptHashesObject)
     , to_json :: ScriptHashesObject -> Effect JsonString
     , to_js_value :: ScriptHashesObject -> Effect Json
     , len :: ScriptHashesObject -> Effect Int
     , get :: ScriptHashesObject -> Int -> Effect ScriptHash
     , add :: ScriptHashesObject -> ScriptHash -> Effect Unit
     }
scriptHashesObject = mkNewtypedFFI (Proxy :: Proxy ScriptHashesObject)


-- export class Assets {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {Assets}
-- */
--   static from_bytes(bytes: Uint8Array): Assets;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AssetsJSON}
-- */
--   to_js_value(): AssetsJSON;
-- /**
-- * @param {string} json
-- * @returns {Assets}
-- */
--   static from_json(json: string): Assets;
-- /**
-- * @returns {Assets}
-- */
--   static new(): Assets;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {AssetName} key
-- * @param {BigNum} value
-- * @returns {BigNum | undefined}
-- */
--   insert(key: AssetName, value: BigNum): BigNum | undefined;
-- /**
-- * @param {AssetName} key
-- * @returns {BigNum | undefined}
-- */
--   get(key: AssetName): BigNum | undefined;
-- /**
-- * @returns {AssetNames}
-- */
--   keys(): AssetNames;
-- }

newtype Assets = Assets
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor AssetsObject) AssetsObject
    , from_json :: EffectMth1 JsonString AssetsObject
    , new :: EffectMth0 AssetsObject
    )
  )

derive instance Newtype Assets _

assets
  :: { from_bytes :: Assets -> Cbor AssetsObject -> Effect AssetsObject
     , from_json :: Assets -> JsonString -> Effect AssetsObject
     , new :: Assets -> Effect AssetsObject
     }
assets = mkNewtypedFFI (Proxy :: Proxy Assets)

newtype AssetsObject = AssetsObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor AssetsObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    -- , insert :: EffectMth2 AssetName BigNum (Opt BigNum)
    -- , get :: EffectMth1 AssetName (Opt BigNum)
    , keys :: EffectMth0 AssetNames
    )
  )

derive instance Newtype AssetsObject _

assetsObject
  :: { free :: AssetsObject -> Effect Unit
     , to_bytes :: AssetsObject -> Effect (Cbor AssetsObject)
     , to_json :: AssetsObject -> Effect JsonString
     , to_js_value :: AssetsObject -> Effect Json
     , len :: AssetsObject -> Effect Int
     -- , insert :: AssetsObject -> AssetName -> BigNum -> Effect (Opt BigNum)
     -- , get :: AssetsObject -> AssetName -> Effect (Opt BigNum)
     , keys :: AssetsObject -> Effect AssetNames
     }
assetsObject = mkNewtypedFFI (Proxy :: Proxy AssetsObject)

-- export class AssetName {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {AssetName}
-- */
--   static from_bytes(bytes: Uint8Array): AssetName;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AssetNameJSON}
-- */
--   to_js_value(): AssetNameJSON;
-- /**
-- * @param {string} json
-- * @returns {AssetName}
-- */
--   static from_json(json: string): AssetName;
-- /**
-- * @param {Uint8Array} name
-- * @returns {AssetName}
-- */
--   static new(name: Uint8Array): AssetName;
-- /**
-- * @returns {Uint8Array}
-- */
--   name(): Uint8Array;
-- }

newtype AssetName = AssetName
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor AssetNameObject) AssetNameObject
    , from_json :: EffectMth1 JsonString AssetNameObject
    , new :: EffectMth1 Uint8Array AssetNameObject
    )
  )

derive instance Newtype AssetName _

assetName ::
  { from_bytes :: AssetName -> Cbor AssetNameObject -> Effect AssetNameObject
  , from_json :: AssetName -> JsonString -> Effect AssetNameObject
  , new :: AssetName -> Uint8Array -> Effect AssetNameObject
  }
assetName = mkNewtypedFFI (Proxy :: Proxy AssetName)

newtype AssetNameObject = AssetNameObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor AssetNameObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , name :: EffectMth0 Uint8Array
    )
  )

derive instance Newtype AssetNameObject _

assetNameObject
  :: { free :: AssetNameObject -> Effect Unit
     , to_bytes :: AssetNameObject -> Effect (Cbor AssetNameObject)
     , to_json :: AssetNameObject -> Effect JsonString
     , to_js_value :: AssetNameObject -> Effect Json
     , name :: AssetNameObject -> Effect Uint8Array
     }
assetNameObject = mkNewtypedFFI (Proxy :: Proxy AssetNameObject)


-- export class AssetNames {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {AssetNames}
-- */
--   static from_bytes(bytes: Uint8Array): AssetNames;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AssetNamesJSON}
-- */
--   to_js_value(): AssetNamesJSON;
-- /**
-- * @param {string} json
-- * @returns {AssetNames}
-- */
--   static from_json(json: string): AssetNames;
-- /**
-- * @returns {AssetNames}
-- */
--   static new(): AssetNames;
-- /**
-- * @returns {number}
-- */
--   len(): number;
-- /**
-- * @param {number} index
-- * @returns {AssetName}
-- */
--   get(index: number): AssetName;
-- /**
-- * @param {AssetName} elem
-- */
--   add(elem: AssetName): void;
-- }

newtype AssetNames = AssetNames
  ( JSObject
    ( from_bytes :: EffectMth1 (Cbor AssetNamesObject) AssetNamesObject
    , from_json :: EffectMth1 JsonString AssetNamesObject
    , new :: EffectMth0 AssetNamesObject
    )
  )

derive instance Newtype AssetNames _

assetNames ::
  { from_bytes :: AssetNames -> Cbor AssetNamesObject -> Effect AssetNamesObject
  , from_json :: AssetNames -> JsonString -> Effect AssetNamesObject
  , new :: AssetNames -> Effect AssetNamesObject
  }
assetNames = mkNewtypedFFI (Proxy :: Proxy AssetNames)

newtype AssetNamesObject = AssetNamesObject
  ( JSObject
    ( free :: EffectMth0 Unit
    , to_bytes :: EffectMth0 (Cbor AssetNamesObject)
    , to_json :: EffectMth0 JsonString
    , to_js_value :: EffectMth0 Json
    , len :: EffectMth0 Int
    , get :: EffectMth1 Int AssetName
    , add :: EffectMth1 AssetName Unit
    )
  )

derive instance Newtype AssetNamesObject _

assetNamesObject ::
  { free :: AssetNamesObject -> Effect Unit
  , to_bytes :: AssetNamesObject -> Effect (Cbor AssetNamesObject)
  , to_json :: AssetNamesObject -> Effect JsonString
  , to_js_value :: AssetNamesObject -> Effect Json
  , len :: AssetNamesObject -> Effect Int
  , get :: AssetNamesObject -> Int -> Effect AssetName
  , add :: AssetNamesObject -> AssetName -> Effect Unit
  }
assetNamesObject = mkNewtypedFFI (Proxy :: Proxy AssetNamesObject)


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



