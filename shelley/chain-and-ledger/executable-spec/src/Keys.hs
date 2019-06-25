module Keys
  ( Owner(..)
  , SKey(..)
  , VKey(..)
  , VKeyGenesis(..)
  , GKeys(..)
  , Dms(..)
  , KeyPair(..)
  , keyPair
  , HashKey(..)
  , Sig
  , hashKey
  , hashGenesisKey
  , sign
  , verify
  , KESig
  , signKES
  , verifyKES
  )
where

import           Numeric.Natural       (Natural)

import           Ledger.Core           (VKey(..)
                                       , VKeyGenesis(..)
                                       , Owner(..)
                                       , SKey(..)
                                       , Sig(..)
                                       , Hash
                                       , hash
                                       , KeyPair(..)
                                       , keyPair)

import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set

-- |The hash of public Key
newtype HashKey = HashKey Hash deriving (Show, Eq, Ord)

data KESig a = KESig (Sig a) Natural deriving (Show, Eq, Ord)

-- |Hash a given public key
hashKey :: VKey -> HashKey
hashKey key = HashKey $ hash key

-- |Produce a digital signature
sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

-- |Produce a key evolving signature
signKES :: SKey -> a -> Natural -> KESig a
signKES (SKey k) d n = KESig (Sig d k) n

-- |Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

-- |Verify a key evolving signature
verifyKES :: Eq a => VKey -> a -> KESig a -> Natural -> Bool
verifyKES (VKey vk) vd (KESig (Sig sd sk) m) n = vk == sk && vd == sd && m == n

newtype Dms = Dms (Map.Map VKeyGenesis VKey)
  deriving (Show, Ord, Eq)

newtype GKeys = GKeys (Set.Set VKeyGenesis)
  deriving (Show, Ord, Eq)

hashGenesisKey :: VKeyGenesis -> HashKey
hashGenesisKey = HashKey . hash
