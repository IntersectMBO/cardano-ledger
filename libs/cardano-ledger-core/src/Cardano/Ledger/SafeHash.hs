{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In cardano-ledger, hashing a type @X@ is based upon the serialization of @X@. Serialization is
--   based upon the 'EncCBOR' and DecCBOR type classes, and the values produced by 'EncCBOR' instances for a
--   particular type, are not necessarily unique. For this reason, when an @X@ object comes
--   over the network in serialized form, we must preserve the original bytes that arrived over
--   the network, otherwise when the system hashes that object, the hash in the ledger, and the hash of
--   that object from the other side of the network may not agree. The module 'Cardano.Ledger.SafeHash'
--   introduces the 'SafeToHash' type class that ensures that types with a @(SafeToHash X)@ instance store the
--   original bytes that arrived over the network for the value of @X@. The recommended way to store the
--   original bytes is to use the type 'MemoBytes', although there are
--   a few types that store their original bytes in other ways. In order to encourage the use of 'MemoBytes'
--   newtypes defined as a 'MemoBytes' get the to derive 'SafeToHash' instances for free.
module Cardano.Ledger.SafeHash (
  -- * SafeHash and SafeToHash

  --
  -- $SAFE
  SafeHash,
  SafeToHash (..),

  -- * Creating SafeHash

  --
  -- $MAKE
  HashAnnotated,
  hashAnnotated,
  HashWithCrypto (..),
  unsafeMakeSafeHash,

  -- * Other operations

  --
  -- $OTHER
  castSafeHash,
  extractHash,
  indexProxy,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.HeapWords (HeapWords (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Plain (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Short (ShortByteString, fromShort)
import qualified Data.ByteString.Short as SBS (length)
import Data.Default (Default (..))
import Data.Typeable
import NoThunks.Class (NoThunks (..))

-- ==========================================================

-- SAFE

-- | A 'SafeHash' is a hash of something that is safe to hash. Such types store
--     their own serialisation bytes. The prime example is @('MemoBytes' t)@, but other
--     examples are things that consist of only ByteStrings (i.e. they are their own serialization)
--     or for some other reason store their original bytes.
--
--     We do NOT export the constructor 'SafeHash', but instead export other functions
--     such as 'hashWithCrypto, 'hashAnnotated' and 'extractHash' which have constraints
--     that limit their application to types which preserve their original serialization
--     bytes.
newtype SafeHash index = SafeHash (Hash.Hash HASH index)
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving newtype instance
  Hash.HashAlgorithm HASH =>
  SafeToHash (SafeHash index)

deriving newtype instance HeapWords (SafeHash i)

deriving instance (Typeable index, Crypto c) => ToCBOR (SafeHash index)

deriving instance (Typeable index, Crypto c) => FromCBOR (SafeHash index)

deriving instance (Typeable index, Crypto c) => EncCBOR (SafeHash index)

deriving instance (Typeable index, Crypto c) => DecCBOR (SafeHash index)

deriving instance Crypto c => ToJSON (SafeHash index)

deriving instance Crypto c => FromJSON (SafeHash index)

instance Crypto c => Default (SafeHash i) where
  def = unsafeMakeSafeHash def

-- | Extract the hash out of a 'SafeHash'
extractHash :: SafeHash i -> Hash.Hash HASH i
extractHash (SafeHash h) = h

-- MAKE

-- | Don't use this except in Testing to make Arbitrary instances, etc.
--   Defined here, only because the Constructor is in scope here.
unsafeMakeSafeHash :: Hash.Hash HASH index -> SafeHash index
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
--   requirement of the classes 'HashAnnotated' and 'HashWithCrypto' (below) which
--   provide more convenient ways to construct SafeHashes than using 'makeHashWithExplicitProxys'.
class SafeToHash t where
  -- | Extract the original bytes from 't'
  originalBytes :: t -> ByteString

  originalBytesSize :: t -> Int
  originalBytesSize = BS.length . originalBytes

  makeHashWithExplicitProxys ::
    Hash.HashAlgorithm HASH =>
    Proxy c ->
    Proxy index ->
    t ->
    SafeHash index

  -- | Build a @(SafeHashrypto index)@ value given to proxies (determining @i@ and @crypto@), and the
  --   value to be hashed.
  makeHashWithExplicitProxys _ _ x = SafeHash $ Hash.castHash (Hash.hashWith originalBytes x)

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

instance Hash.HashAlgorithm c => SafeToHash (Hash.Hash c i) where
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
--
-- @
--   newtype T era = T S
--      deriving Eq
--      deriving newtype SafeToHash -- Uses {-# LANGUAGE DerivingStrategies #-}
--
--   instance HashAnnotated (T era) Index (Crypto era)
-- @
--
-- After these declarations. One specialization of 'hashAnnotated' is
--    @(hashAnnotated :: Era e => T e -> SafeHash (Crypto e) Index)@
class SafeToHash x => HashAnnotated x index c | x -> index c where
  indexProxy :: x -> Proxy index
  indexProxy _ = Proxy @index

  -- | Create a @('SafeHash' i crypto)@,
  -- given @(Hash.HashAlgorithm (HASH crypto))@
  -- and  @(HashAnnotated x i crypto)@ instances.
  hashAnnotated :: Hash.HashAlgorithm HASH => x -> SafeHash index
  hashAnnotated = makeHashWithExplicitProxys (Proxy @c) (Proxy @index)
  {-# INLINE hashAnnotated #-}

-- ========================================================================

-- | Create @('SafeHash' index crypto)@ values, used when the type being hashed:
--   @x@ determines the @index@ tag but not the @crypto@ tag of @x@
class SafeToHash x => HashWithCrypto x index | x -> index where
  -- | Create a @('SafeHash' index crypto)@ value from @x@, the @proxy@ determines the crypto.
  hashWithCrypto ::
    forall c.
    Hash.HashAlgorithm HASH =>
    Proxy c ->
    x ->
    SafeHash index
  hashWithCrypto proxy = makeHashWithExplicitProxys proxy (Proxy @index)
  {-# INLINE hashWithCrypto #-}

-- ======================================================================

-- OTHER

-- | To change the index parameter of SafeHash (which is a phantom type) use castSafeHash
castSafeHash :: forall i j. SafeHash i -> SafeHash j
castSafeHash (SafeHash h) = SafeHash (Hash.castHash h)
