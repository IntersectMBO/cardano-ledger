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

module Cardano.Ledger.SafeHash
  ( SafeHash,
    SafeToHash (..),
    castSafeHash,
    HashAnnotated,
    hashAnnotated,
    indexProxy,
    HashWithCrypto (..),
    HasAlgorithm,
    extractHash,
    hashSafeList,
    Safe (..),
    unsafeMakeSafeHash,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Prelude (HeapWords (..))
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Foldable (fold)
import Data.MemoBytes (MemoBytes (..))
import Data.Typeable
import NoThunks.Class (NoThunks (..))

-- ==========================================================

-- | A SafeHash is a hash of something that is safe to hash. Such types store
--     their own serialisation bytes. The prime example is (MemoBytes t), but other
--     examples are things that consist of only ByteStrings.
--
--     We do NOT export the constructor SafeHash, but instead export other functions
--     such as 'hashWithCrypto, 'hashAnnotated' and 'extractHash' which have constraints
--     that limit their application to types which preserve their original serialization
--     bytes.
newtype SafeHash crypto index = SafeHash (Hash.Hash (CC.HASH crypto) index)
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving newtype instance
  Hash.HashAlgorithm (CC.HASH crypto) =>
  SafeToHash (SafeHash crypto index)

deriving newtype instance HeapWords (Hash.Hash (CC.HASH c) i) => HeapWords (SafeHash c i)

deriving instance (Typeable index, CC.Crypto c) => ToCBOR (SafeHash c index)

deriving instance (Typeable index, CC.Crypto c) => FromCBOR (SafeHash c index)

type HasAlgorithm c = Hash.HashAlgorithm (CC.HASH c)

extractHash :: SafeHash crypto i -> Hash.Hash (CC.HASH crypto) i
extractHash (SafeHash h) = h

-- | To change the index parameter of SafeHash (which is a phantom type) use castSafeHash
castSafeHash :: forall i j c. SafeHash c i -> SafeHash c j
castSafeHash (SafeHash h) = SafeHash (Hash.castHash h)

-- Don't use this except in Testing to make Arbitrary instances, etc.
unsafeMakeSafeHash :: Hash.Hash (CC.HASH crypto) index -> SafeHash crypto index
unsafeMakeSafeHash = SafeHash

-- =====================================================================

-- | Only Types that preserve their serialisation bytes are members of the
--   class SafeToHash. There are only a limited number of primitive direct
--   instances of SafeToHash, all but two of them are present in this file. Instead
--   of making explicit instances, we almost always use a newtype (around a type S)
--   where their is already an instance (SafeToHash S). In that case the newtype
--   has its SafeToHash instance derived using newtype deriving. The only exceptions
--   are the legacy Shelley types: Metadata and Tx, that preserve their serialisation bytes
--   using a different mechanism than MemoBytes.  SafeToHash is a superclass
--   requirement of the classes HashAnnotated and HashWithCrypto (below) which
--   provide more convenient ways to construct SafeHashes than using makeHashWithExplicitProxys.
class SafeToHash t where
  originalBytes :: t -> ByteString
  makeHashWithExplicitProxys :: HasAlgorithm c => Proxy c -> Proxy index -> t -> SafeHash c index
  makeHashWithExplicitProxys _ _ x = SafeHash $ Hash.castHash (Hash.hashWith originalBytes x)

-- There are a limited number of direct instances. Everything else should come
-- from newtype deriving.

instance SafeToHash (MemoBytes t) where
  originalBytes = fromShort . memobytes

instance SafeToHash ShortByteString where
  originalBytes x = fromShort x

instance SafeToHash ByteString where
  originalBytes x = x

-- If one looks at the deriving clause in the definitions of SafeHash, we see that we
-- derive that it is SafeToHash. We can derive this instance because SafeHash is
-- a newtype around (Hash.Hash c i) which is a primitive SafeToHash type.

instance Hash.HashAlgorithm c => SafeToHash (Hash.Hash c i) where
  originalBytes (Hash.UnsafeHash b) = fromShort b

-- =====================================================================
{- Types that are SafeToHash, AND have both of the following two invariants,
     are made members of the HashAnnotated class. The preconditions are:
     1) The type uniquely determines the 'index' type tag of (SafeHash crypto index)
     2) The type uniquely determines the 'crypto' type of (SafeHash crytop index)

     The SafeToHash and the HashAnnotated classes are designed so that their
     instances can be easily derived (because their methods have default methods
     when the type is a newtype around a type that is SafeToHash). For example,
     given (SafeToHash S) then:

     newtype T era = T S
        deriving Eq
        deriving newtype SafeToHash   -- Uses {-# LANGUAGE DerivingStrategies #-}

     instance HashAnnotated (T era) Index (Crypto era)

     After these declarations. One specialization of 'hashAnnotated' is
     hashAnnotated :: Era e => T e -> SafeHash (Crypto e) Index
-}

-- | Determine the index from the type 'x'
class SafeToHash x => HashAnnotated x index crypto | x -> index crypto where
  indexProxy :: x -> Proxy index
  indexProxy _ = Proxy @index

hashAnnotated :: forall c i x. (HasAlgorithm c, HashAnnotated x i c) => x -> SafeHash c i
hashAnnotated = makeHashWithExplicitProxys (Proxy @c) (Proxy @i)

-- ========================================================================

-- | When the type being hashed: 'x' determines the 'index' tag but not the 'crypto'
class SafeToHash x => HashWithCrypto x index | x -> index where
  hashWithCrypto :: forall crypto. HasAlgorithm crypto => Proxy crypto -> x -> SafeHash crypto index
  hashWithCrypto proxy y = makeHashWithExplicitProxys proxy (Proxy @index) y

-- ======================================================================

-- | Sometimes one wants to hash multiple things, simply by concatenating
--   all the bytes. This abstraction allows one to do that safely.
data Safe where
  Safe :: SafeToHash x => x -> Safe

instance SafeToHash Safe where
  originalBytes (Safe x) = originalBytes x

hashSafeList :: HasAlgorithm c => Proxy c -> Proxy index -> [Safe] -> SafeHash c index
hashSafeList pc pindex xs = makeHashWithExplicitProxys pc pindex (fold $ originalBytes <$> xs)
