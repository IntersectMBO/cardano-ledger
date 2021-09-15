{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Manual generators.
--
-- This module provides functions to convert hedgehog 'Gen's to and from a
-- 'Manual' generators, and functions to manipulate these manual generators.
module Hedgehog.Extra.Manual
  ( Manual (Manual),
    unManual,
    toManual,
    fromManual,
    dontShrink,

    -- * Combinators
    sized,
    replicate,
    interleave,

    -- * Auxiliary
    wrapTreeT,
    unwrapTreeT,
  )
where

import Control.Monad (ap, liftM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (Identity))
import Data.Maybe (catMaybes, mapMaybe)
import Hedgehog (Gen, Seed, Size)
import Hedgehog.Internal.Gen (GenT (GenT))
import qualified Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Tree (NodeT (NodeT), TreeT (TreeT), nodeChildren, nodeValue)
import Prelude hiding (replicate)

newtype Manual a = Manual {unManual :: Size -> Seed -> a}

toManual :: Gen a -> Manual (TreeT (MaybeT Identity) a)
toManual (GenT f) = Manual f

fromManual :: Manual (TreeT (MaybeT Identity) a) -> Gen a
fromManual (Manual f) = GenT f

dontShrink :: Gen a -> Manual (Maybe a)
dontShrink = fmap (fmap nodeValue . coerce) . toManual

instance Functor Manual where
  fmap = liftM

instance Applicative Manual where
  pure x = Manual $ \_ _ -> x
  (<*>) = ap

instance Monad Manual where
  return = pure
  Manual x >>= f = Manual $ \size seed ->
    case Seed.split seed of
      (sx, sf) -> unManual (f (x size sx)) size sf

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

sized :: (Size -> Manual a) -> Manual a
sized f = Manual $ \size seed -> unManual (f size) size seed

-- | A version of 'Control.Monad.replicateM' specific to 'Manual'.
replicate :: forall a. Int -> Manual a -> Manual [a]
replicate n (Manual f) = Manual $ \size seed ->
  let go :: Int -> Seed -> [a]
      go 0 _ = []
      go !n' s = case Seed.split s of
        (s', s'') -> f size s' : go (n' - 1) s''
   in go n seed

interleave :: [TreeT (MaybeT Identity) a] -> TreeT (MaybeT Identity) [a]
interleave = coerce (Just . interleave' . catMaybes)

interleave' :: [NodeT (MaybeT Identity) a] -> NodeT (MaybeT Identity) [a]
interleave' ts =
  NodeT (map nodeValue ts) $
    concat
      [ [ wrapTreeT . Just $ interleave' ts'
          | chunkSize <- chunkSizes,
            ts' <- removes chunkSize ts
        ],
        [ wrapTreeT . Just $ interleave' (xs ++ [y'] ++ zs)
          | (xs, y, zs) <- splits ts,
            y' <- mapMaybe unwrapTreeT (nodeChildren y)
        ]
      ]
  where
    -- Chunks we try to remove from the list
    --
    -- For example, if the list has length 10, @chunkSizes = [10,5,2,1]@
    chunkSizes :: [Int]
    chunkSizes = takeWhile (> 0) $ iterate (`div` 2) (length ts)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

wrapTreeT :: Maybe (NodeT (MaybeT Identity) a) -> TreeT (MaybeT Identity) a
wrapTreeT = coerce

unwrapTreeT :: TreeT (MaybeT Identity) a -> Maybe (NodeT (MaybeT Identity) a)
unwrapTreeT = coerce

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (x : xs) = ([], x, xs) : fmap (\(as, b, cs) -> (x : as, b, cs)) (splits xs)

-- | @removes n@ splits a list into chunks of size n and returns all possible
-- lists where one of these chunks has been removed.
--
-- Examples:
--
-- > removes 1 [1..3] == [[2,3],[1,3],[1,2]]
-- > removes 2 [1..4] == [[3,4],[1,2]]
-- > removes 2 [1..5] == [[3,4,5],[1,2,5],[1,2,3,4]]
-- > removes 3 [1..5] == [[4,5],[1,2,3]]
--
-- Note that the last chunk we delete might have fewer elements than @n@.
removes :: forall a. Int -> [a] -> [[a]]
removes k = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = xs2 : map (xs1 ++) (go xs2)
      where
        (xs1, xs2) = splitAt k xs
