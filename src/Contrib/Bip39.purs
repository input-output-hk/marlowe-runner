module Contrib.Bip39
  ( Strength
  , strength
  , mnemonicToString
  , Mnemonic
  , generateMnemonic
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Strength :: Int -> Type

strength
  :: { "128" :: Strength 128
     , "160" :: Strength 160
     , "192" :: Strength 192
     , "224" :: Strength 224
     , "256" :: Strength 256
     , "512" :: Strength 512
     }
strength =
  { "128": unsafeCoerce 128 -- 12 words
  , "160": unsafeCoerce 160 -- 15 words
  , "192": unsafeCoerce 192 -- 18 words
  , "224": unsafeCoerce 224 -- 21 words
  , "256": unsafeCoerce 256 -- 24 words
  , "512": unsafeCoerce 512 -- 48 words
  }

newtype Mnemonic :: Int -> Type
newtype Mnemonic strength = Mnemonic String

mnemonicToString :: forall s. Mnemonic s -> String
mnemonicToString (Mnemonic s) = s

foreign import generateMnemonicImpl :: forall (s :: Int). EffectFn1 (Strength s) (Mnemonic s)

generateMnemonic :: forall (s :: Int). Strength s -> Effect (Mnemonic s)
generateMnemonic = runEffectFn1 generateMnemonicImpl

