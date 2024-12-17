{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Hashes (
  -- * Hashing algorithms
  HASH,
  ADDRHASH,

  -- * Era-independent hash type identifiers.

  -- | Various identifiers in the ledger are hashes of particular structures.
  -- While the structures may change from era to era, the hash will remain the
  -- same, and we can refer to the hash of, say, a transaction, without knowing
  -- the actual transaction type. As such, we define a number of these hashes
  -- here.
  -- $eraIndep
  EraIndependentTxBody,
  EraIndependentBlockHeader,
  EraIndependentBlockBody,
  EraIndependentMetadata,
  EraIndependentScript,
  EraIndependentData,
  EraIndependentScriptData,
  EraIndependentTxAuxData,
  EraIndependentPParamView,
  EraIndependentScriptIntegrity,

  -- * Hashes

  -- ** `@DSIGN@ Verification Key Hashes
  KeyHash (..),
  KeyRole (..),
  hashKey,

  -- ** Script Hashes
  ScriptHash (..),
  DataHash,

  -- ** @VRF@ Verification Key Hashes
  KeyRoleVRF (..),
  VRFVerKeyHash (..),
  toVRFVerKeyHash,
  fromVRFVerKeyHash,

  -- ** Genesis @DSIGN@ and @VRF@ Verification Key hashes
  GenDelegPair (..),
  GenDelegs (..),

  -- * SafeHash

  -- In cardano-ledger, hashing a type @X@ is based upon the serialization of @X@. Serialization is
  -- based upon the 'EncCBOR' and DecCBOR type classes, and the values produced by 'EncCBOR' instances for a
  -- particular type, are not necessarily unique. For this reason, when an @X@ object comes
  -- over the network in serialized form, we must preserve the original bytes that arrived over
  -- the network, otherwise when the system hashes that object, the hash in the ledger, and the hash of
  -- that object from the other side of the network may not agree. The module 'Cardano.Ledger.SafeHash'
  -- introduces the 'SafeToHash' type class that ensures that types with a @(SafeToHash X)@ instance store the
  -- original bytes that arrived over the network for the value of @X@. The recommended way to store the
  -- original bytes is to use the type 'MemoBytes', although there are
  -- a few types that store their original bytes in other ways. In order to encourage the use of 'MemoBytes'
  -- newtypes defined as a 'MemoBytes' get the to derive 'SafeToHash' instances for free.

  --
  -- $SAFE
  SafeHash,
  SafeToHash (..),

  -- ** Creating SafeHash

  --
  -- $MAKE
  HashAnnotated,
  hashAnnotated,
  unsafeMakeSafeHash,

  -- ** Other operations

  --
  -- $OTHER
  castSafeHash,
  extractHash,
  indexProxy,
)
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import Cardano.HeapWords (HeapWords (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Keys.Internal (DSIGN, HasKeyRole, KeyRole (..), VKey (..))
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Short (ShortByteString, fromShort)
import qualified Data.ByteString.Short as SBS (length)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | Hashing algorithm used for hashing everything, except addresses, for which `ADDRHASH`
-- is used.
type HASH = Hash.Blake2b_256

-- | Hashing algorithm used for hashing cryptographic keys and scripts. As the type
-- synonym name alludes, this is the hashing algorithm used for addresses.
type ADDRHASH = Hash.Blake2b_224

--   $eraIndep
--
--   Hashes carry around a phantom type parameter to identify the sort of thing
--   they are hashing. This is useful to allow us to distinguish, say, a place
--   where we expect the hash for a block from the hash for a script. However,
--   the exact structure that makes up a "block" will differ from era to era. We
--   still want to share the same namespace for the identifiers. Consequently we
--   define some era-independent indices here.

data EraIndependentTxBody

data EraIndependentBlockHeader

data EraIndependentBlockBody

data EraIndependentMetadata

data EraIndependentTxAuxData

data EraIndependentScript

data EraIndependentData

type DataHash = SafeHash EraIndependentData

data EraIndependentScriptData

data EraIndependentPParamView

data EraIndependentScriptIntegrity

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype KeyHash (r :: KeyRole) = KeyHash
  {unKeyHash :: Hash.Hash ADDRHASH (DSIGN.VerKeyDSIGN DSIGN)}
  deriving (Show, Eq, Ord)
  deriving newtype
    ( NFData
    , NoThunks
    , Generic
    , ToCBOR
    , FromCBOR
    , EncCBOR
    , DecCBOR
    , ToJSONKey
    , FromJSONKey
    , ToJSON
    , FromJSON
    , Default
    )

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey :: VKey kd -> KeyHash kd
hashKey (VKey vk) = KeyHash $ DSIGN.hashVerKeyDSIGN vk

--------------------------------------------------------------------------------
-- Script Hashes
--------------------------------------------------------------------------------

newtype ScriptHash
  = ScriptHash (Hash.Hash ADDRHASH EraIndependentScript)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype
    ( NFData
    , NoThunks
    , ToCBOR
    , FromCBOR
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , ToJSONKey
    , FromJSONKey
    )

--------------------------------------------------------------------------------
-- VRF Key Hashes
--------------------------------------------------------------------------------

data KeyRoleVRF
  = StakePoolVRF
  | GenDelegVRF
  | BlockIssuerVRF

-- | Discriminated hash of VRF Verification Key
newtype VRFVerKeyHash (r :: KeyRoleVRF) = VRFVerKeyHash
  {unVRFVerKeyHash :: Hash.Hash HASH KeyRoleVRF}
  deriving (Show, Eq, Ord)
  deriving newtype
    ( NFData
    , NoThunks
    , Generic
    , ToCBOR
    , FromCBOR
    , EncCBOR
    , DecCBOR
    , ToJSONKey
    , FromJSONKey
    , ToJSON
    , FromJSON
    , Default
    )

toVRFVerKeyHash :: Hash.Hash HASH (VRF.VerKeyVRF v) -> VRFVerKeyHash (r :: KeyRoleVRF)
toVRFVerKeyHash = VRFVerKeyHash . Hash.castHash

fromVRFVerKeyHash :: VRFVerKeyHash (r :: KeyRoleVRF) -> Hash.Hash HASH (VRF.VerKeyVRF v)
fromVRFVerKeyHash = Hash.castHash . unVRFVerKeyHash

--------------------------------------------------------------------------------
-- Genesis Keys Hashes
--------------------------------------------------------------------------------

-- TODO: Move to cardano-ledger-shelley, whenever CertState will become era parametric
data GenDelegPair = GenDelegPair
  { genDelegKeyHash :: !(KeyHash 'GenesisDelegate)
  , genDelegVrfHash :: !(VRFVerKeyHash 'GenDelegVRF)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks GenDelegPair

instance NFData GenDelegPair

instance EncCBOR GenDelegPair where
  encCBOR (GenDelegPair hk vrf) =
    encodeListLen 2 <> encCBOR hk <> encCBOR vrf

instance DecCBOR GenDelegPair where
  decCBOR = do
    decodeRecordNamed
      "GenDelegPair"
      (const 2)
      (GenDelegPair <$> decCBOR <*> decCBOR)
  {-# INLINE decCBOR #-}

instance ToJSON GenDelegPair where
  toJSON (GenDelegPair d v) =
    Aeson.object
      [ "delegate" .= d
      , "vrf" .= v
      ]

instance FromJSON GenDelegPair where
  parseJSON =
    Aeson.withObject "GenDelegPair" $ \obj ->
      GenDelegPair
        <$> obj .: "delegate"
        <*> obj .: "vrf"

newtype GenDelegs = GenDelegs
  { unGenDelegs :: Map (KeyHash 'Genesis) GenDelegPair
  }
  deriving (Eq, EncCBOR, DecCBOR, NoThunks, NFData, Generic, FromJSON, ToJSON)
  deriving (Show) via Quiet GenDelegs

--------------------------------------------------------------------------------
-- Safe Hashes
--------------------------------------------------------------------------------

-- | A 'SafeHash' is a hash of something that is safe to hash. Such types store their own
-- serialisation bytes. The prime example is @('MemoBytes' t)@, but other examples are
-- things that consist of only ByteStrings (i.e. they are their own serialization) or for
-- some other reason store their original bytes.
--
-- We do NOT export the constructor 'SafeHash', but instead export other functions such as
-- 'hashAnnotated' and 'extractHash' which have constraints that limit their application
-- to types which preserve their original serialization bytes.
newtype SafeHash i = SafeHash (Hash.Hash HASH i)
  deriving
    (Show, Eq, Ord, NoThunks, NFData, SafeToHash, HeapWords, ToCBOR, FromCBOR, EncCBOR, DecCBOR)

deriving instance ToJSON (SafeHash i)

deriving instance FromJSON (SafeHash i)

instance Default (SafeHash i) where
  def = unsafeMakeSafeHash def

-- | Extract the hash out of a 'SafeHash'
extractHash :: SafeHash i -> Hash.Hash HASH i
extractHash (SafeHash h) = h

-- MAKE

-- | Don't use this except in Testing to make Arbitrary instances, etc.
--   Defined here, only because the Constructor is in scope here.
unsafeMakeSafeHash :: Hash.Hash HASH i -> SafeHash i
unsafeMakeSafeHash = SafeHash

-- =====================================================================

-- | Only Types that preserve their serialisation bytes are members of the
--   class 'SafeToHash'. There are only a limited number of primitive direct
--   instances of 'SafeToHash', all but two of them are present in this file. Instead
--   of making explicit instances, we almost always use a newtype (around a type @S@)
--   where their is already an instance @(SafeToHash S)@. In that case the newtype
--   has its SafeToHash instance derived using newtype deriving. The prime example of @s@ is 'MemoBytes'.
--   The only exceptions are the legacy Shelley types: @Metadata@ and @ShelleyTx@, that
--   preserve their serialization bytes
--   using a different mechanism than the use of 'MemoBytes'.  'SafeToHash' is a superclass
--   requirement of the classes 'HashAnnotated' which
--   provide more convenient ways to construct SafeHashes than using 'makeHashWithExplicitProxys'.
class SafeToHash t where
  -- | Extract the original bytes from 't'
  originalBytes :: t -> ByteString

  originalBytesSize :: t -> Int
  originalBytesSize = BS.length . originalBytes

  makeHashWithExplicitProxys :: Proxy i -> t -> SafeHash i

  -- | Build a @(SafeHash index)@ value given a proxy determining @i@, and the
  --   value to be hashed.
  makeHashWithExplicitProxys _ x = SafeHash $ Hash.castHash (Hash.hashWith originalBytes x)

-- There are a limited number of direct instances. Everything else should come
-- from newtype deriving.

instance SafeToHash ShortByteString where
  originalBytes = fromShort
  originalBytesSize = SBS.length

instance SafeToHash ByteString where
  originalBytes x = x

-- If one looks at the deriving clause in the definitions of SafeHash, we see that we
-- derive that it is SafeToHash. We can derive this instance because SafeHash is
-- a newtype around (Hash.Hash c i) which is a primitive SafeToHash type.

instance Hash.HashAlgorithm h => SafeToHash (Hash.Hash h i) where
  originalBytes = Hash.hashToBytes

-- | Types that are 'SafeToHash', AND have both of the following two invariants,
--   are made members of the HashAnnotated class. The preconditions are:
--
--   1. The type uniquely determines the 'index' type tag of (SafeHashrypto index)
--   2. The type uniquely determines the 'crypto' type of (SafeHashrytop index)
--
--   The 'SafeToHash' and the 'HashAnnotated' classes are designed so that their
--   instances can be easily derived (because their methods have default methods
--   when the type is a newtype around a type that is 'SafeToHash'). For example,
class SafeToHash x => HashAnnotated x i | x -> i where
  -- TODO: move outside of type class
  indexProxy :: x -> Proxy i
  indexProxy _ = Proxy @i

  -- | Create a @('SafeHash' i crypto)@,
  -- given @(Hash.HashAlgorithm (HASH crypto))@
  -- and  @(HashAnnotated x i crypto)@ instances.
  hashAnnotated :: x -> SafeHash i
  hashAnnotated = makeHashWithExplicitProxys (Proxy @i)
  {-# INLINE hashAnnotated #-}

-- OTHER

-- | To change the index parameter of SafeHash (which is a phantom type) use castSafeHash
castSafeHash :: forall i j. SafeHash i -> SafeHash j
castSafeHash (SafeHash h) = SafeHash (Hash.castHash h)
