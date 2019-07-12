{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MultiSig
  ( MultiSig(..)
  , toCBOR
  , fromCBOR
  , evalNativeMultiSigScript
  , MultiSignatureScript
  , validateScript
  )
where

import           UTxO (Tx (..), WitVKey (..))

import qualified Data.Set as Set

import           Keys

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, encodeWord)

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm)
import           Cardano.Crypto.Hash (HashAlgorithm)

data MultiSig hashAlgo dsignAlgo =
    SingleSig (KeyHash hashAlgo dsignAlgo)
  | MultiSig Int [MultiSig hashAlgo dsignAlgo]

instance (DSIGNAlgorithm dsignAlgo, HashAlgorithm hashAlgo) =>
  ToCBOR (MultiSig hashAlgo dsignAlgo) where
  toCBOR (SingleSig hk) = encodeListLen 2 <> encodeWord 0 <> toCBOR hk
  toCBOR (MultiSig th msigs) =
    encodeListLen 3 <> encodeWord 1 <> toCBOR th <> toCBOR msigs

instance (DSIGNAlgorithm dsignAlgo, HashAlgorithm hashAlgo) =>
  FromCBOR (MultiSig hashAlgo dsignAlgo) where
  fromCBOR = do
    _ <- decodeListLen
    ctor <- decodeWord
    if ctor == 0
      then do
       hk <- KeyHash <$> fromCBOR
       pure $ SingleSig hk
      else do
       th <- fromCBOR
       msigs <- fromCBOR
       pure $ MultiSig th msigs

evalNativeMultiSigScript
  :: MultiSig hashAlgo dsignAlgo
  -> Set.Set (KeyHash hashAlgo dsignAlgo)
  -> Bool

evalNativeMultiSigScript (SingleSig hk) vhks = Set.member hk vhks
evalNativeMultiSigScript (MultiSig th msigs) vhks =
  th <= sum [if evalNativeMultiSigScript msig vhks then 1 else 0 | msig <- msigs]

class (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo) =>
  MultiSignatureScript a hashAlgo dsignAlgo where
  validateScript :: a -> Tx hashAlgo dsignAlgo -> Bool

validateNativeMultiSigScript
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => MultiSig hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig vhks
  where witsSet = _witnessSet tx
        vhks    = Set.map (\(WitVKey vk _) -> hashKey vk) witsSet

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo) =>
  MultiSignatureScript (MultiSig hashAlgo dsignAlgo) hashAlgo dsignAlgo where
  validateScript = validateNativeMultiSigScript
