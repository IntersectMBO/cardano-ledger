{-# LANGUAGE DataKinds #-}

module Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  , toAddr
  , toCred
  )
where

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Keys (DSIGNAlgorithm, KeyDiscriminator (..), KeyPair, hashKey, vKey)
import           TxData (Addr (..), Credential (..), RewardAcnt (..))

mkVKeyRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair 'Regular dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkVKeyRwdAcnt keys = RewardAcnt $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt
  :: Credential hashAlgo dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkRwdAcnt script@(ScriptHashObj _) = RewardAcnt script
mkRwdAcnt key@(KeyHashObj _) = RewardAcnt key
mkRwdAcnt (GenesisHashObj _) =
  error "cannot construct reward account with genesis key"

toAddr
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => (KeyPair 'Regular dsignAlgo, KeyPair 'Regular dsignAlgo)
  -> Addr hashAlgo dsignAlgo
toAddr (payKey, stakeKey) = AddrBase (toCred payKey) (toCred stakeKey)

toCred
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => KeyPair 'Regular dsignAlgo
  -> Credential hashAlgo dsignAlgo
toCred k = KeyHashObj . hashKey $ vKey k
