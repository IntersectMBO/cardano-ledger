{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.Decoding.Sharing (
  DecShareCBOR (..),
  Interns (..),
  Intern (..),
  decShareLensCBOR,
  decSharePlusLensCBOR,
  decNoShareCBOR,
  interns,
  internsFromMap,
  internsFromVMap,
  toMemptyLens,
  decShareMonadCBOR,
)
where

import Cardano.Ledger.Binary.Decoding.DecCBOR
import Cardano.Ledger.Binary.Decoding.Decoder
import Control.Monad ((<$!>))
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import qualified Data.Foldable as F
import Data.Kind
import qualified Data.Map.Strict as Map (size)
import Data.Map.Strict.Internal (Map (..))
import Data.Primitive.Types (Prim)
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
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
  { internMaybe :: a -> Maybe a
  -- ^ Function that will do the interning. If value is not available then
  -- `Nothing` is returned.
  , internWeight :: !Int
  -- ^ Used for sorting. Normally set to the size of the underlying data
  -- structure. Keeping interns sorted with respect to how many elements
  -- is in the underlying data structure in theory gives a better chance of
  -- successful intern hit sooner rather than later.
  }

newtype Interns a = Interns [Intern a]
  deriving (Monoid)

interns :: Interns k -> k -> k
interns (Interns []) !k = k -- optimize for common case when there are no interns
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
             in go m
        , internWeight = Map.size m
        }
    ]

internsFromVMap :: Ord k => VMap VB kv k a -> Interns k
internsFromVMap m =
  Interns
    [ Intern
        { internMaybe = \k -> VMap.internMaybe k m
        , internWeight = VMap.size m
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

class Monoid (Share a) => DecShareCBOR a where
  {-# MINIMAL (decShareCBOR | decSharePlusCBOR) #-}
  type Share a :: Type
  type Share a = ()

  -- | Whenever `fromShareCBOR` is being used for defining the instance this
  -- function should return the state that can be added whenever user invokes
  -- `decSharePlusCBOR`. `mempty` is returned by default.
  getShare :: a -> Share a
  getShare _ = mempty

  -- | Utilize sharing when decoding, but do not add anything to the state for
  -- future sharing.
  decShareCBOR :: Share a -> Decoder s a
  decShareCBOR s = fst <$> runStateT decSharePlusCBOR s

  -- | Deserialize with sharing and add to the state that is used for sharing. Default
  -- implementation will add value returned by `getShare` for adding to the
  -- state.
  decSharePlusCBOR :: StateT (Share a) (Decoder s) a
  decSharePlusCBOR = do
    s <- get
    x <- lift $ decShareCBOR s
    x <$ put (getShare x <> s)

decShareLensCBOR ::
  DecShareCBOR b =>
  SimpleGetter bs (Share b) ->
  StateT bs (Decoder s) b
decShareLensCBOR l = do
  s <- get
  lift $ decShareCBOR (s ^. l)

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

-- | Just like `decSharePlusCBOR`, except allows to transform the shared state
-- with a lens.
decSharePlusLensCBOR ::
  DecShareCBOR b =>
  Lens' bs (Share b) ->
  StateT bs (Decoder s) b
decSharePlusLensCBOR l = do
  s <- get
  (x, k) <- lift $ runStateT decSharePlusCBOR (s ^. l)
  x <$ put (s & l .~ k)

-- | Use `DecShareCBOR` class while ignoring sharing
decNoShareCBOR :: DecShareCBOR a => Decoder s a
decNoShareCBOR = decShareCBOR mempty

instance (Ord k, DecCBOR k, DecCBOR v) => DecShareCBOR (Map k v) where
  type Share (Map k v) = (Interns k, Interns v)
  decShareCBOR (kis, vis) = do
    decodeMap (interns kis <$> decCBOR) (interns vis <$> decCBOR)
  getShare !m = (internsFromMap m, mempty)

instance (Ord k, DecCBOR k, DecCBOR v) => DecShareCBOR (VMap VB VB k v) where
  type Share (VMap VB VB k v) = (Interns k, Interns v)
  decShareCBOR (kis, vis) = do
    decodeVMap (interns kis <$> decCBOR) (interns vis <$> decCBOR)
  getShare !m = (internsFromVMap m, mempty)

instance (Ord k, DecCBOR k, DecCBOR v, Prim v) => DecShareCBOR (VMap VB VP k v) where
  type Share (VMap VB VP k v) = Interns k
  decShareCBOR kis = do
    decodeVMap (interns kis <$> decCBOR) decCBOR
  getShare !m = internsFromVMap m

-- | Share every item in a functor, have deserializing it
decShareMonadCBOR :: (DecCBOR (f b), Monad f) => Interns b -> Decoder s (f b)
decShareMonadCBOR kis = do
  sm <- decCBOR
  pure (interns kis <$!> sm)
