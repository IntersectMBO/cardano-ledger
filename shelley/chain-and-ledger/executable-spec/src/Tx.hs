{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , txwitsVKey
  , txwitsScript
  , extractKeyHash
  , extractScriptHash
  )
where


import           Keys

import           Cardano.Binary (ToCBOR (toCBOR), encodeWord8)

import           Cardano.Crypto.Hash (HashAlgorithm, hashWithSerialiser)

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm)

import           Data.Word (Word8)

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

import           TxData

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
  all (flip evalNativeMultiSigScript vhks) msigs
evalNativeMultiSigScript (RequireAnyOf msigs) vhks =
  any (flip evalNativeMultiSigScript vhks) msigs
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

-- | Witness accessor function for Transactions
txwitsVKey
  :: (DSIGNAlgorithm dsignAlgo)
  => Tx hashAlgo dsignAlgo
  -> Map.Map (VKey dsignAlgo) (Sig dsignAlgo (TxBody hashAlgo dsignAlgo))
txwitsVKey tx =
  Map.fromList $ map (\(WitVKey vk sig) -> (vk, sig)) (Set.toList $ _witnessVKeySet tx)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript
  :: Tx hashAlgo dsignAlgo
  -> Map.Map (ScriptHash hashAlgo dsignAlgo) (MultiSig hashAlgo dsignAlgo)
txwitsScript tx = _witnessMSigMap tx

extractKeyHash :: [StakeObject hashAlgo dsignAlgo] -> [KeyHash hashAlgo dsignAlgo]
extractKeyHash l =
  Maybe.catMaybes $ map (\so -> case so of
                                 KeyHashStake hk -> Just hk
                                 _ -> Nothing) l

extractScriptHash :: [StakeObject hashAlgo dsignAlgo] -> [ScriptHash hashAlgo dsignAlgo]
extractScriptHash l =
  Maybe.catMaybes $ map (\so -> case so of
                                 ScriptHashStake hk -> Just hk
                                 _ -> Nothing) l
