{-# LANGUAGE DataKinds #-}

module Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  )
where

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Keys (DSIGNAlgorithm, KeyDiscriminator (..), KeyPair, hashKey, vKey)
import           TxData (Credential (..), RewardAcnt (..))

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
