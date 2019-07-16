{-# LANGUAGE LambdaCase #-}

module Address
  ( mkRwdAcnt
  )
where

import           Cardano.Crypto.Hash   (HashAlgorithm)

import           Delegation.PoolParams (RewardAcnt(..))
import           Keys

mkRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkRwdAcnt keys = RewardAcnt $ hashKey $ vKey keys
