{-# LANGUAGE LambdaCase #-}

module Address
  ( Addr(..)
  , Ix
  , Ptr(..)
  , mkRwdAcnt
  )
where

import           Data.Typeable           (Typeable)
import           Data.Word               (Word8)
import           Numeric.Natural         (Natural)

import           Cardano.Binary          (ToCBOR(toCBOR), encodeListLen)
import           Cardano.Crypto.Hash   (HashAlgorithm)

import           Delegation.PoolParams (RewardAcnt(..))
import           Keys
import           Slot (Slot(..))

-- |An address for UTxO.
data Addr hashAlgo dsignAlgo
  = AddrTxin
      { _payHK :: KeyHash hashAlgo dsignAlgo
      , _stakeHK :: KeyHash hashAlgo dsignAlgo
      }
  | AddrPtr
      { _stakePtr :: Ptr
      }
  deriving (Show, Eq, Ord)

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (Addr hashAlgo dsignAlgo)
 where
  toCBOR = \case
    AddrTxin payHK stakeHK ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR payHK
        <> toCBOR stakeHK
    AddrPtr stakePtr ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR stakePtr

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr Slot Ix Ix
  deriving (Show, Eq, Ord)

instance ToCBOR Ptr where
  toCBOR (Ptr slot txIx certIx) =
    encodeListLen 3
      <> toCBOR slot
      <> toCBOR txIx
      <> toCBOR certIx

mkRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkRwdAcnt keys = RewardAcnt $ hashKey $ vKey keys
