{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Tx
  ( -- transaction
    Tx(..)
  , TxBody(..)
  , TxOut(..)
  , TxIn(..)
  , TxId(..)
  , txUpdate
  , inputs
  , outputs
  , certs
  , wdrls
  , txfee
  , ttl
  , body
  , witnessVKeySet
  , witnessMSigMap
    -- witness data
  , WitVKey(..)
  , MultiSignatureScript
  , validateScript
  , hashScript
  , txwitsScript
  , extractKeyHash
  , extractScriptHash
  )
where


import           Keys (AnyKeyHash, KeyHash, hashKey, undiscriminateKeyHash)

import           Cardano.Binary (ToCBOR (toCBOR), encodeWord8)

import           Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm)

import           Data.Word (Word8)

import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           TxData (Credential (..), MultiSig (..), ScriptHash (..), StakeCredential, Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxOut (..), WitVKey (..), body, certs,
                     inputs, outputs, ttl, txUpdate, txfee, wdrls, witnessMSigMap, witnessVKeySet
                     )

-- | Typeclass for multis-signature script data types. Allows for script
-- validation and hashing.
class (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, ToCBOR a) =>
  MultiSignatureScript a hashAlgo dsignAlgo where
  validateScript :: a -> Tx hashAlgo dsignAlgo -> Bool
  hashScript :: a -> ScriptHash hashAlgo dsignAlgo

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript
  :: MultiSig hashAlgo dsignAlgo
  -> Set (KeyHash hashAlgo dsignAlgo)
  -> Bool
evalNativeMultiSigScript (RequireSignature hk) vhks = Set.member hk vhks
evalNativeMultiSigScript (RequireAllOf msigs) vhks =
  all (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireAnyOf msigs) vhks =
  any (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireMOf m msigs) vhks =
  m <= sum [if evalNativeMultiSigScript msig vhks then 1 else 0 | msig <- msigs]

-- | Script validator for native multi-signature scheme.
validateNativeMultiSigScript
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => MultiSig hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig vhks
  where witsSet = _witnessVKeySet tx
        vhks    = Set.map (\(WitVKey vk _) -> hashKey vk) witsSet

-- | Hashes native multi-signature script, appending the 'nativeMultiSigTag' in
-- front and then calling the script CBOR function.
hashNativeMultiSigScript
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => MultiSig hashAlgo dsignAlgo
  -> ScriptHash hashAlgo dsignAlgo
hashNativeMultiSigScript msig =
  ScriptHash $ hashWithSerialiser (\x -> encodeWord8 nativeMultiSigTag
                                          <> toCBOR x) msig

-- | Magic number representing the tag of the native multi-signature script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
nativeMultiSigTag :: Word8
nativeMultiSigTag = 0

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo) =>
  MultiSignatureScript (MultiSig hashAlgo dsignAlgo) hashAlgo dsignAlgo where
  validateScript = validateNativeMultiSigScript
  hashScript = hashNativeMultiSigScript

-- | Multi-signature script witness accessor function for Transactions
txwitsScript
  :: Tx hashAlgo dsignAlgo
  -> Map.Map (ScriptHash hashAlgo dsignAlgo) (MultiSig hashAlgo dsignAlgo)
txwitsScript = _witnessMSigMap

extractKeyHash
  :: [StakeCredential hashAlgo dsignAlgo]
  -> [AnyKeyHash hashAlgo dsignAlgo]
extractKeyHash =
  mapMaybe (\case
                KeyHashObj hk -> Just $ undiscriminateKeyHash hk
                GenesisHashObj hk -> Just $ undiscriminateKeyHash hk
                _ -> Nothing)

extractScriptHash
  :: [StakeCredential hashAlgo dsignAlgo]
  -> [ScriptHash hashAlgo dsignAlgo]
extractScriptHash =
  mapMaybe (\case
                ScriptHashObj hk -> Just hk
                _ -> Nothing)
