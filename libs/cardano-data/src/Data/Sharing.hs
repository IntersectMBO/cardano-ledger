{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Sharing
  ( FromSharedCBOR (..),
    Interns (..),
    Intern (..),
    fromSharedLensCBOR,
    fromSharedPlusLensCBOR,
    fromNotSharedCBOR,
    interns,
    internsFromMap,
    internsFromVMap,
    toMemptyLens,
    fromShareCBORfunctor,
  )
where

import Cardano.Binary (Decoder, FromCBOR (..), decodeListLen, dropMap)
import Control.Monad (void)
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.BiMap (BiMap (..), biMapFromMap)
import Data.Coders (decodeMap, decodeVMap, invalidKey)
import Data.Compact.ViewMap (VB, VMap, VP)
import qualified Data.Compact.ViewMap as VMap
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal
import Data.Primitive.Types (Prim)
import Lens.Micro

-- =======================================

-- | This is an abstract interface that does the interning. In other words it
-- does the actual sharing by looking up the supplied value in some existing
-- data structure and uses that value instead. Relying on this interface gives us
-- the benefit of ignoring the type of underlying data structure and allows us
-- to compose many `Intern`s with the monoidal interface provided by `Interns`
-- wrapper. In order to create an `Intern` see the `internsFromMap` or
-- `internsFromVMap` functions.
data Intern a = Intern
  { -- | Function that will do the interning. If value is not available then
    -- `Nothing` is returned.
    internMaybe :: a -> Maybe a,
    -- | Used for sorting. Normally set to the size of the underlying data
    -- structure. Keeping interns sorted with respect to how many elements
    -- is in the underlying data structure in theory gives a better chance of
    -- successful intern hit sooner rather than later.
    internWeight :: !Int
  }

newtype Interns a = Interns [Intern a]
  deriving (Monoid)

interns :: Interns k -> k -> k
interns (Interns []) !k = k -- opimize for common case when there are no interns
interns (Interns is) !k = go is
  where
    go [] = k
    go (x : xs) =
      case internMaybe x k of
        Just kx -> kx
        Nothing -> go xs
{-# INLINE interns #-}

internsFromMap :: Ord k => Map k a -> Interns k
internsFromMap m =
  Interns
    [ Intern
        { internMaybe = \k ->
            let go Tip = Nothing
                go (Bin _ kx _ l r) =
                  case compare k kx of
                    LT -> go l
                    GT -> go r
                    EQ -> Just kx
             in go m,
          internWeight = Map.size m
        }
    ]

internsFromVMap :: Ord k => VMap VB kv k a -> Interns k
internsFromVMap m =
  Interns
    [ Intern
        { internMaybe = \k -> VMap.internMaybe k m,
          internWeight = VMap.size m
        }
    ]

instance Semigroup (Interns a) where
  (<>) is1 (Interns []) = is1
  (<>) (Interns []) is2 = is2
  (<>) (Interns is1) (Interns is2) =
    Interns (F.foldr insertIntoSortedInterns is2 is1)
    where
      insertIntoSortedInterns i [] = [i]
      insertIntoSortedInterns i (a : as)
        | internWeight a > internWeight i = a : insertIntoSortedInterns i as
        | otherwise = i : a : as

class Monoid (Share a) => FromSharedCBOR a where
  {-# MINIMAL (fromSharedCBOR | fromSharedPlusCBOR) #-}
  type Share a :: Type
  type Share a = ()

  -- | Whenever `fromShareCBOR` is being used for defining the instance this
  -- function should return the state that can be added whenever user invokes
  -- `fromSharedPlusCBOR`. `mempty` is returned by default.
  getShare :: a -> Share a
  getShare _ = mempty

  -- | Utilize sharing when decoding, but do not add anything to the state for
  -- future sharing.
  fromSharedCBOR :: Share a -> Decoder s a
  fromSharedCBOR s = fst <$> runStateT fromSharedPlusCBOR s

  -- | Deserialize with sharing and add to the state that is used for sharing. Default
  -- implementation will add value returned by `getShare` for adding to the
  -- state.
  fromSharedPlusCBOR :: StateT (Share a) (Decoder s) a
  fromSharedPlusCBOR = do
    s <- get
    x <- lift $ fromSharedCBOR s
    x <$ put (getShare x <> s)

fromSharedLensCBOR ::
  FromSharedCBOR b =>
  SimpleGetter bs (Share b) ->
  StateT bs (Decoder s) b
fromSharedLensCBOR l = do
  s <- get
  lift $ fromSharedCBOR (s ^. l)

-- | Using this function it is possible to compose two lenses. One will extract
-- a value and another will used it for placing it into a empty monoid. Here is
-- an example of how a second element of a tuple can be projected on the third
-- element of a 3-tuple.
--
-- > toMemptyLens _3 _2 == lens (\(_, b) -> (mempty, mempty, b)) (\(a, _) (_, _, b) -> (a, b))
--
-- Here is an example where we extract a second element of a tuple and insert it at
-- third position of a three tuple while all other elements are set to `mempty`:
--
-- >>> import Lens.Micro
-- >>> ("foo","bar") ^. toMemptyLens _3 _2 :: (Maybe String, (), String)
-- (Nothing,(),"bar")
--
-- In the opposite direction of extracting the third element of a 3-tuple and
-- replacing the second element of the tuple the setter is being applied to
--
-- >>> ("foo","bar") & toMemptyLens _3 _2 .~ (Just "baz", (), "booyah") :: (String, String)
-- ("foo","booyah")
toMemptyLens :: Monoid a => Lens' a b -> Lens' c b -> Lens' c a
toMemptyLens lto lfrom =
  lens (\s -> mempty & lto .~ (s ^. lfrom)) (\s a -> s & lfrom .~ (a ^. lto))

-- | Just like `fromSharedPlusCBOR`, except allows to transform the shared state
-- with a lens.
fromSharedPlusLensCBOR ::
  FromSharedCBOR b =>
  Lens' bs (Share b) ->
  StateT bs (Decoder s) b
fromSharedPlusLensCBOR l = do
  s <- get
  (x, k) <- lift $ runStateT fromSharedPlusCBOR (s ^. l)
  x <$ put (s & l .~ k)

-- | Use `FromSharedCBOR` class while ignoring sharing
fromNotSharedCBOR :: FromSharedCBOR a => Decoder s a
fromNotSharedCBOR = fromSharedCBOR mempty

deriving newtype instance FromSharedCBOR a => FromSharedCBOR (Identity a)

instance (Ord k, FromCBOR k, FromCBOR v) => FromSharedCBOR (Map k v) where
  type Share (Map k v) = (Interns k, Interns v)
  fromSharedCBOR (kis, vis) = do
    decodeMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)
  getShare !m = (internsFromMap m, mempty)

instance (Ord k, FromCBOR k, FromCBOR v) => FromSharedCBOR (VMap VB VB k v) where
  type Share (VMap VB VB k v) = (Interns k, Interns v)
  fromSharedCBOR (kis, vis) = do
    decodeVMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)
  getShare !m = (internsFromVMap m, mempty)

instance (Ord k, FromCBOR k, FromCBOR v, Prim v) => FromSharedCBOR (VMap VB VP k v) where
  type Share (VMap VB VP k v) = Interns k
  fromSharedCBOR kis = do
    decodeVMap (interns kis <$> fromCBOR) fromCBOR
  getShare !m = internsFromVMap m

-- ==============================================================================
-- These BiMap instances are adapted from the FromCBOR instances in Data.Coders

instance (Ord a, Ord b, FromCBOR a, FromCBOR b) => FromSharedCBOR (BiMap b a b) where
  type Share (BiMap b a b) = (Interns a, Interns b)
  fromSharedCBOR share =
    decodeListLen >>= \case
      1 -> biMapFromMap <$> fromSharedCBOR share
      -- Previous encoding of 'BiMap' encoded both the forward and reverse
      -- directions. In this case we skip the reverse encoding. Note that,
      -- further, the reverse encoding was from 'b' to 'a', not the current 'b'
      -- to 'Set a', and hence the dropper reflects that.
      2 -> do
        !x <- biMapFromMap <$> fromSharedCBOR share
        dropMap (void $ fromCBOR @b) (void $ fromCBOR @a)
        return x
      k -> invalidKey (fromIntegral k)
  getShare (MkBiMap m1 m2) = (internsFromMap m1, internsFromMap m2)

-- | Share every item in a functor, have deserializing it
fromShareCBORfunctor :: (FromCBOR (f b), Functor f) => Interns b -> Decoder s (f b)
fromShareCBORfunctor kis = do
  sm <- fromCBOR
  pure (interns kis <$> sm)
