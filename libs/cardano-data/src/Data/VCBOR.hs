{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.VCBOR
  ( FromSharedVCBOR (..),
    Interns (..),
    Intern (..),
    fromSharedLensVCBOR,
    fromSharedPlusLensVCBOR,
    fromNotSharedVCBOR,
    interns,
    internsFromMap,
    internsFromVMap,
    toMemptyLens,
  )
where

import Cardano.Binary (Decoder, DecoderError, FromCBOR (..), decodeFullDecoder, decodeListLen, dropMap)
import Control.Monad (void, (<$!>))
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.BiMap (BiMap (..), biMapFromMap)
import qualified Data.ByteString.Lazy as BSL
import Data.Coders (decode, decodeMap, decodeVMap, invalidKey)
import qualified Data.Foldable as F
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal
import Data.Primitive.Types (Prim)
import Data.Proxy
import Data.Reflection
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import GHC.TypeLits
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

class Monoid (Share a) => Shareable a where
  type Share a :: Type
  type Share a = ()

  -- | Whenever `fromShareVCBOR` is being used for defining the instance this
  -- function should return the state that can be added whenever user invokes
  -- `fromSharedPlusVCBOR`. `mempty` is returned by default.
  getShare :: a -> Share a
  getShare _ = mempty

class Shareable a => FromSharedVCBOR a where
  {-# MINIMAL (fromSharedVCBOR | fromSharedPlusVCBOR) #-}

  -- | Utilize sharing when decoding, but do not add anything to the state for
  -- future sharing.
  fromSharedVCBOR :: Share a -> VDecoder v s a
  fromSharedVCBOR s = fst <$> runStateT fromSharedPlusVCBOR s

  -- | Deserialize with sharing and add to the state that is used for sharing. Default
  -- implementation will add value returned by `getShare` for adding to the
  -- state.
  fromSharedPlusVCBOR :: StateT (Share a) (VDecoder v s) a
  fromSharedPlusVCBOR = do
    s <- get
    x <- lift $ fromSharedVCBOR s
    x <$ put (getShare x <> s)

fromSharedLensVCBOR ::
  FromSharedVCBOR b =>
  SimpleGetter bs (Share b) ->
  StateT bs (VDecoder v s) b
fromSharedLensVCBOR l = do
  s <- get
  lift $ fromSharedVCBOR (s ^. l)

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

-- | Just like `fromSharedPlusVCBOR`, except allows to transform the shared state
-- with a lens.
fromSharedPlusLensVCBOR ::
  FromSharedVCBOR b =>
  Lens' bs (Share b) ->
  StateT bs (VDecoder v s) b
fromSharedPlusLensVCBOR l = do
  s <- get
  (x, k) <- lift $ runStateT fromSharedPlusVCBOR (s ^. l)
  x <$ put (s & l .~ k)

-- | Use `FromSharedVCBOR` class while ignoring sharing
fromNotSharedVCBOR :: FromSharedVCBOR a => VDecoder v s a
fromNotSharedVCBOR = fromSharedVCBOR mempty

instance Ord k => Shareable (Map k a) where
  getShare !m = (internsFromMap m, mempty)
  type Share (Map k a) = (Interns k, Interns a)

instance (Ord k, FromCBOR k, FromCBOR a) => FromSharedVCBOR (Map k a) where
  fromSharedVCBOR (kis, vis) =
    VDecoder $
      decodeMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)

-- instance (Ord k, FromCBOR k, FromCBOR a) => FromSharedVCBOR 2 (Map k a) where
--   fromSharedVCBOR (kis, vis) =
--     VDecoder $
--       decodeMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)

instance Ord k => Shareable (VMap VB VB k a) where
  type Share (VMap VB VB k a) = (Interns k, Interns a)
  getShare !m = (internsFromVMap m, mempty)

instance (Ord k, FromCBOR k, FromCBOR a) => FromSharedVCBOR (VMap VB VB k a) where
  fromSharedVCBOR (kis, vis) =
    VDecoder $ decodeVMap (interns kis <$> fromCBOR) (interns vis <$> fromCBOR)

instance Ord k => Shareable (VMap VB VP k a) where
  type Share (VMap VB VP k a) = Interns k
  getShare !m = internsFromVMap m

instance (Ord k, FromCBOR k, FromCBOR a, Prim a) => FromSharedVCBOR (VMap VB VP k a) where
  fromSharedVCBOR kis =
    VDecoder $ decodeVMap (interns kis <$> fromCBOR) fromCBOR

data Ver
  = Ver1
  | Ver2
  | Ver3
  | Ver4
  | Ver5
  | Ver6
  | Ver7

data MajProtVer (v :: Ver) where
  MajProtVer1 :: MajProtVer 'Ver1
  MajProtVer2 :: MajProtVer 'Ver2
  MajProtVer3 :: MajProtVer 'Ver3
  MajProtVer4 :: MajProtVer 'Ver4
  MajProtVer5 :: MajProtVer 'Ver5
  MajProtVer6 :: MajProtVer 'Ver6
  MajProtVer7 :: MajProtVer 'Ver7

reifyVer :: Ver -> (forall v. MajProtVer v -> a) -> a
reifyVer v f =
  case v of
    Ver1 -> f MajProtVer1
    Ver2 -> f MajProtVer2
    Ver3 -> f MajProtVer3
    Ver4 -> f MajProtVer4
    Ver5 -> f MajProtVer5
    Ver6 -> f MajProtVer6
    Ver7 -> f MajProtVer7

decodeFull ::
  forall a.
  (FromSharedVCBOR a) =>
  Integer ->
  BSL.ByteString ->
  Either DecoderError a
decodeFull n bs = reifyNat n decodeWithVersion
  where
    decodeWithVersion :: forall (v :: Nat). Proxy v -> Either DecoderError a
    decodeWithVersion _ =
      decodeFullDecoder "VCBOR" (unVDecoder (fromNotSharedVCBOR :: VDecoder v s a)) bs

-- decodeFull ::
--   forall v a.
--   (FromVCBOR v a) =>
--   BSL.ByteString ->
--   Proxy v ->
--   Either DecoderError a
-- decodeFull bs _ = decodeFullDecoder "VCBOR" (unVDecoder (fromVCBOR @v)) bs

applyDecoder :: Integer -> BSL.ByteString -> (forall v s. VDecoder v s a) -> Either DecoderError a
applyDecoder n bs dec =
  case n of
    1 -> decodeFullDecoder "VCBOR" (unVDecoder (dec @1)) bs
    2 -> decodeFullDecoder "VCBOR" (unVDecoder (dec @2)) bs

t :: Either DecoderError (Map Int Int)
t = decodeFull 3 "" :: Either DecoderError (Map Int Int)

-- instance (forall v. (FromVCBOR v (Map k a))) => FromVCBORn (Map k a) where
--   fromVCBORn _ = fromVCBOR

-- where
--   f :: (forall v. (FromVCBOR v a) =>
--   f = decodeFull bs

-- applyDecoder' ::
--      forall v a. FromVCBOR v a
--   => Integer
--   -> BSL.ByteString
--   -> Either DecoderError a
-- applyDecoder' n bs = applyDecoder n bs (fromVCBOR @v)

-- ==============================================================================
-- These BiMap instances are adapted from the FromCBOR instances in Data.Coders

instance (Ord a, Ord b) => Shareable (BiMap b a b) where
  type Share (BiMap b a b) = (Interns a, Interns b)
  getShare (MkBiMap m1 m2) = (internsFromMap m1, internsFromMap m2)

instance
  (Ord a, Ord b, FromCBOR a, FromCBOR b, FromSharedVCBOR (Map a b)) =>
  FromSharedVCBOR (BiMap b a b)
  where
  fromSharedVCBOR share =
    VDecoder decodeListLen >>= \case
      1 -> biMapFromMap <$> fromSharedVCBOR share
      -- Previous encoding of 'BiMap' encoded both the forward and reverse
      -- directions. In this case we skip the reverse encoding. Note that,
      -- further, the reverse encoding was from 'b' to 'a', not the current 'b'
      -- to 'Set a', and hence the dropper reflects that.
      2 -> do
        !x <- biMapFromMap <$> fromSharedVCBOR share
        VDecoder $ dropMap (void $ fromCBOR @b) (void $ fromCBOR @a)
        return x
      k -> VDecoder $ invalidKey (fromIntegral k)

-- | Share every item in a functor, have deserializing it
fromShareVCBORfunctor :: (FromCBOR (f b), Monad f) => Interns b -> Decoder s (f b)
fromShareVCBORfunctor kis = do
  sm <- fromCBOR
  pure (interns kis <$!> sm)
