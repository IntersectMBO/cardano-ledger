{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for external types/classes.

module Test.Cardano.Util.Orphans where

import           Cardano.Prelude

import qualified Crypto.Random as Rand
import           Data.Time.Units (Microsecond, Millisecond, Second,
                     TimeUnit (..), convertUnit)

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QC


data Five a = Five a a a a a
    deriving (Functor, Foldable, Traversable)

five :: a -> Five a
five a = Five a a a a a

instance Rand.MonadRandom Gen where
    getRandomBytes n = do
        Five a b c d e <- sequenceA . five $ QC.choose (minBound, maxBound)
        pure $ fst $ Rand.randomBytesGenerate n (Rand.drgNewTest (a,b,c,d,e))

instance Arbitrary Millisecond where
    arbitrary = fromMicroseconds <$> QC.choose (0, 600 * 1000 * 1000)
    shrink = QC.shrinkIntegral

instance Arbitrary Microsecond where
    arbitrary = fromMicroseconds <$> QC.choose (0, 600 * 1000 * 1000)
    shrink = QC.shrinkIntegral

instance Arbitrary Second where
    arbitrary = convertUnit @Microsecond <$> QC.arbitrary
    shrink = QC.shrinkIntegral
