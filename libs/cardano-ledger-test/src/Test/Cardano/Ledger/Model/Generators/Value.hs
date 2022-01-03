{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Model.Generators.Value where

import Cardano.Ledger.Coin (Coin (..))
import Data.Group (Group (..))
import Data.Group.GrpMap (traverseGrpMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Sum (..))
import Data.Semigroup.Foldable (fold1)
import QuickCheck.GenT
  ( Gen,
    choose,
    frequency,
  )
import Test.Cardano.Ledger.Model.Value
  ( ModelValueF (..),
  )

-- | divide a value into several "chunks"
-- y = unfoldModelValue minCoin x
-- PREC: minCoin =< coin x
-- POSTC: fold y === x .&&. all ((minCoin =<) . coin) y
unfoldModelValue :: forall x. Ord x => Coin -> ModelValueF x -> Gen (NonEmpty (ModelValueF x))
unfoldModelValue (Coin minValue) = go
  where
    splitMA :: Sum Integer -> Gen (Sum Integer)
    splitMA (Sum a) =
      frequency
        [ (1, pure (Sum a)),
          (1, Sum <$> choose (0, a)),
          (1, pure (mempty))
        ]

    go :: ModelValueF x -> Gen (NonEmpty (ModelValueF x))
    go m@(ModelValueF (Coin ada, ma))
      | ada <= 2 * minValue = pure (pure m)
      | otherwise = do
        adaL <- Coin <$> choose (minValue, ada - minValue)
        maL <- traverseGrpMap splitMA ma
        let adaR = Coin ada ~~ adaL
            maR = ma ~~ maL
            m' = (pure (ModelValueF (adaL, maL)) <> pure (ModelValueF (adaR, maR)))
        frequency
          [ (10, pure m'),
            (1, fold1 <$> traverse go m')
          ]
