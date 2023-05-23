module CardanoMultiplatformLib.Address where

import Prelude

import CardanoMultiplatformLib.Types (Bech32, Cbor)
import Data.Argonaut (Json)
import Data.Newtype (class Newtype)
import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

-- export class Address {
--   free(): void;
-- /**
-- * @param {Uint8Array} data
-- * @returns {Address}
-- */
--   static from_bytes(data: Uint8Array): Address;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {AddressJSON}
-- */
--   to_js_value(): AddressJSON;
-- /**
-- * @param {string} json
-- * @returns {Address}
-- */
--   static from_json(json: string): Address;
-- /**
-- * header has 4 bits addr type discrim then 4 bits network discrim.
-- * Copied from shelley.cddl:
-- *
-- * base address
-- * bits 7-6: 00
-- * bit 5: stake cred is keyhash/scripthash
-- * bit 4: payment cred is keyhash/scripthash
-- * bits 3-0: network id
-- * 
-- * pointer address
-- * bits 7-5: 010
-- * bit 4: payment cred is keyhash/scripthash
-- * bits 3-0: network id
-- * 
-- * enterprise address
-- * bits 7-5: 010
-- * bit 4: payment cred is keyhash/scripthash
-- * bits 3-0: network id
-- *
-- * reward addresses:
-- * bits 7-5: 111
-- * bit 4: credential is keyhash/scripthash
-- * bits 3-0: network id
-- *
-- * byron addresses:
-- * bits 7-4: 1000
-- * bits 3-0: unrelated data (recall: no network ID in Byron addresses)
-- * @returns {number}
-- */
--   header(): number;
-- /**
-- * @param {number} header
-- * @param {number} kind
-- * @returns {boolean}
-- */
--   static header_matches_kind(header: number, kind: number): boolean;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {string | undefined} prefix
-- * @returns {string}
-- */
--   to_bech32(prefix?: string): string;
-- /**
-- * @param {string} bech_str
-- * @returns {Address}
-- */
--   static from_bech32(bech_str: string): Address;
-- /**
-- *
-- *     * Note: bech32-encoded Byron addresses will also pass validation here
-- *     
-- * @param {string} bech_str
-- * @returns {boolean}
-- */
--   static is_valid_bech32(bech_str: string): boolean;
-- /**
-- * @param {string} base58
-- * @returns {boolean}
-- */
--   static is_valid_byron(base58: string): boolean;
-- /**
-- * @param {string} bech_str
-- * @returns {boolean}
-- */
--   static is_valid(bech_str: string): boolean;
-- /**
-- * @returns {number}
-- */
--   network_id(): number;
-- /**
-- * @returns {ByronAddress | undefined}
-- */
--   as_byron(): ByronAddress | undefined;
-- /**
-- * @returns {RewardAddress | undefined}
-- */
--   as_reward(): RewardAddress | undefined;
-- /**
-- * @returns {PointerAddress | undefined}
-- */
--   as_pointer(): PointerAddress | undefined;
-- /**
-- * @returns {EnterpriseAddress | undefined}
-- */
--   as_enterprise(): EnterpriseAddress | undefined;
-- /**
-- * @returns {BaseAddress | undefined}
-- */
--   as_base(): BaseAddress | undefined;
-- /**
-- * Note: by convention, the key inside reward addresses are considered payment credentials
-- * @returns {StakeCredential | undefined}
-- */
--   payment_cred(): StakeCredential | undefined;
-- /**
-- * Note: by convention, the key inside reward addresses are NOT considered staking credentials
-- * Note: None is returned pointer addresses as the chain history is required to resolve its associated cred
-- * @returns {StakeCredential | undefined}
-- */
--   staking_cred(): StakeCredential | undefined;
-- }

newtype Address = Address
  ( JSObject
      ( from_bytes :: EffectMth1 (Cbor AddressObject) AddressObject
      , from_json :: EffectMth1 String AddressObject
      , from_bech32 :: EffectMth1 String AddressObject
      , header_matches_kind :: EffectMth2 Number Number Boolean
      , is_valid_bech32 :: EffectMth1 String Boolean
      , is_valid_byron :: EffectMth1 String Boolean
      , is_valid :: EffectMth1 String Boolean
      )
  )

derive instance Newtype Address _

address
  :: { from_bytes :: Address -> (Cbor AddressObject) -> Effect AddressObject
     , from_json :: Address -> String -> Effect AddressObject
     , from_bech32 :: Address -> String -> Effect AddressObject
     , header_matches_kind :: Address -> Number -> Number -> Effect Boolean
     , is_valid_bech32 :: Address -> String -> Effect Boolean
     , is_valid_byron :: Address -> String -> Effect Boolean
     , is_valid :: Address -> String -> Effect Boolean
     }
address = mkNewtypedFFI (Proxy :: Proxy Address)

newtype AddressObject = AddressObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_json :: EffectMth0 String
      , to_js_value :: EffectMth0 Json -- AddressJSON
      , header :: EffectMth0 Number
      , to_bytes :: EffectMth0 (Cbor AddressObject)
      , to_bech32 :: EffectMth1 (Opt String) Bech32
      , network_id :: EffectMth0 Number
      -- , as_byron :: EffectMth0 (Maybe ByronAddressObject)
      -- , as_reward :: EffectMth0 (Maybe RewardAddressObject)
      -- , as_pointer :: EffectMth0 (Maybe PointerAddressObject)
      -- , as_enterprise :: EffectMth0 (Maybe EnterpriseAddressObject)
      -- , as_base :: EffectMth0 (Maybe BaseAddressObject)
      -- , payment_cred :: EffectMth0 (Maybe StakeCredentialObject)
      -- , staking_cred :: EffectMth0 (Maybe StakeCredentialObject)
      )
  )

derive instance Newtype AddressObject _

addressObject
  :: { free :: AddressObject -> Effect Unit
     , to_json :: AddressObject -> Effect String
     , to_js_value :: AddressObject -> Effect Json -- AddressJSON
     , header :: AddressObject -> Effect Number
     , to_bytes :: AddressObject -> Effect (Cbor AddressObject)
     , to_bech32 :: AddressObject -> Opt String -> Effect Bech32
     , network_id :: AddressObject -> Effect Number
     -- , as_byron :: AddressObject -> EffectMth0 (Maybe ByronAddressObject)
     -- , as_reward :: AddressObject -> EffectMth0 (Maybe RewardAddressObject)
     -- , as_pointer :: AddressObject -> EffectMth0 (Maybe PointerAddressObject)
     -- , as_enterprise :: AddressObject -> EffectMth0 (Maybe EnterpriseAddressObject)
     -- , as_base :: AddressObject -> EffectMth0 (Maybe BaseAddressObject)
     -- , payment_cred :: AddressObject -> EffectMth0 (Maybe StakeCredentialObject)
     -- , staking_cred :: AddressObject -> EffectMth0 (Maybe StakeCredentialObject)
     }
addressObject = mkNewtypedFFI (Proxy :: Proxy AddressObject)

-- foreign import data Address :: Type
--
-- -- | We allocate the `Address` in the memory. If we don't `free` it up then
-- -- | we gonna leak memory.
-- foreign import fromBytesImpl :: EffectFn2 Lib Uint8Array Address
-- 
-- fromBytes :: Lib -> Uint8Array -> Effect Address
-- fromBytes lib bytes = runEffectFn2 fromBytesImpl lib bytes
-- 
-- foreign import fromBech32Impl :: EffectFn2 Lib String Address
-- 
-- fromBech32 :: Lib -> Bech32 -> Effect Address
-- fromBech32 lib (Bech32 bech32) = runEffectFn2 fromBech32Impl lib bech32
-- 
-- newtype Bech32 = Bech32 String
-- 
-- bech32ToString :: Bech32 -> String
-- bech32ToString (Bech32 str) = str
-- 
-- foreign import toBech32Impl :: EffectFn1 Address Bech32
-- 
-- toBech32 :: Address -> Effect Bech32
-- toBech32 = runEffectFn1 toBech32Impl
-- 
-- foreign import toJsonImpl :: EffectFn1 Address Json
-- 
-- toJson :: Address -> Effect Json
-- toJson = runEffectFn1 toJsonImpl
-- 
-- foreign import freeImpl :: EffectFn1 Address Unit
-- 
-- free :: Address -> Effect Unit
-- free = runEffectFn1 freeImpl

-- bech32FromBytes :: Lib -> Uint8Array -> Effect (Maybe Bech32)
-- bech32FromBytes lib bytes = do
--   let
--     go = Effect.bracket (fromBytes lib bytes) free \addr ->
--       toBech32 addr
--   (Just <$> go) `catchError` (const $ pure Nothing)
-- 
-- bech32FromHex :: Lib -> String -> Effect (Maybe Bech32)
-- bech32FromHex lib str = case HexString.hex str <#> HexString.decode of
--   Just bytes -> bech32FromBytes lib bytes
--   Nothing -> pure Nothing
-- 
-- foreign import isValidBech32Impl :: EffectFn2 Lib String Boolean
-- isValidBech32 :: Lib -> String -> Effect Boolean
-- isValidBech32 lib str = runEffectFn2 isValidBech32Impl lib str

