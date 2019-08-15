{-# LANGUAGE DataKinds #-}

module Address
  ( mkVKeyRwdAcnt
  )
where

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Keys (KeyDiscriminator(..), DSIGNAlgorithm, KeyPair, hashKey, vKey)
import           TxData (Credential (..), RewardAcnt (..))

mkVKeyRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair 'Regular dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkVKeyRwdAcnt keys = RewardAcnt $ KeyHashObj (hashKey $ vKey keys)
