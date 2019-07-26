module Address
  ( mkVKeyRwdAcnt
  )
where

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Keys (DSIGNAlgorithm, KeyPair, hashKey, vKey)
import           TxData (Credential (..), RewardAcnt (..))

mkVKeyRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkVKeyRwdAcnt keys = RewardAcnt $ KeyHashObj (hashKey $ vKey keys)
