{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for external types/classes

module Test.Cardano.Prelude.Orphans
       (
       ) where

import           Cardano.Prelude

import qualified Crypto.Random as Rand

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC


data Five a = Five a a a a a
    deriving (Functor, Foldable, Traversable)

five :: a -> Five a
five a = Five a a a a a

instance Rand.MonadRandom Gen where
    getRandomBytes n = do
        Five a b c d e <- sequenceA . five $ QC.choose (minBound, maxBound)
        pure $ fst $ Rand.randomBytesGenerate n (Rand.drgNewTest (a,b,c,d,e))
