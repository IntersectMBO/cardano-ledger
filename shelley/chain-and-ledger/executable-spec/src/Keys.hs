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
  , sign
  , verify
  , KESig
  , signKES
  , verifyKES
  -- | conversion between Byron / Shelley
  , keyByronToShelley
  , keyShelleyToByron
  , genesisKeyByronToShelley
  , genesisKeyShelleyToByron
  )
where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS
import           Numeric.Natural       (Natural)

import qualified Ledger.Core           as Byron (VKey(..)
                                                , VKeyGenesis(..)
                                                , Owner(..))

import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGenesis = VKeyGenesis VKey
  deriving (Eq, Ord, Show)

-- |Representation of the owner of keypair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

-- |Private/Secret Key
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Public Key
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

-- |Key Pair
data KeyPair = KeyPair
  {sKey :: SKey, vKey :: VKey} deriving (Show, Eq, Ord)

keyPair :: Owner -> KeyPair
keyPair owner = KeyPair (SKey owner) (VKey owner)

-- |The hash of public Key
newtype HashKey = HashKey (Digest SHA256) deriving (Show, Eq, Ord)

-- |A digital signature
data Sig a = Sig a Owner deriving (Show, Eq, Ord)

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

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess VKeyGenesis where
  length (VKeyGenesis vk)        = (BA.length . BS.pack . show) vk
  withByteArray (VKeyGenesis vk) = (BA.withByteArray . BS.pack . show) vk

newtype Dms = Dms (Map.Map VKeyGenesis VKey)
  deriving (Show, Eq)

newtype GKeys = GKeys (Set.Set VKeyGenesis)
  deriving (Show, Eq)

keyByronToShelley :: Byron.VKey -> VKey
keyByronToShelley (Byron.VKey (Byron.Owner owner)) =
  VKey (Owner $ fromIntegral owner)

keyShelleyToByron :: VKey -> Byron.VKey
keyShelleyToByron (VKey (Owner owner)) =
  Byron.VKey $ (Byron.Owner $ fromIntegral owner)

genesisKeyByronToShelley :: Byron.VKeyGenesis -> VKeyGenesis
genesisKeyByronToShelley (Byron.VKeyGenesis vk) = VKeyGenesis (keyByronToShelley vk)

genesisKeyShelleyToByron :: VKeyGenesis -> Byron.VKeyGenesis
genesisKeyShelleyToByron (VKeyGenesis vk) = Byron.VKeyGenesis (keyShelleyToByron vk)
