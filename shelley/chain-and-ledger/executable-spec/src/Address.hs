{-# LANGUAGE LambdaCase #-}

module Address
  ( mkVKeyRwdAcnt
  )
where

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Keys
import           TxData

mkVKeyRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkVKeyRwdAcnt keys = RewardAcnt $ KeyHashObj (hashKey $ vKey keys)
