{-# LANGUAGE LambdaCase #-}

module Address
  ( mkRwdAcnt
  )
where

import           Cardano.Crypto.Hash   (HashAlgorithm)

import           Keys
import           TxData

mkRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkRwdAcnt keys = RewardAcnt $ KeyHashStake (hashKey $ vKey keys)
