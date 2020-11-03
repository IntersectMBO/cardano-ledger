{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- orphan Arbitrary instances

-- | This module is usually imported for its Arbitrary instances
module Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators where

import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Ledger.Era(Era(..))
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf, RequireTimeExpire, RequireTimeStart),
  )
import Data.Sequence.Strict (fromList)
import Shelley.Spec.Ledger.Keys (KeyHash (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Test.Shelley.Spec.Ledger.Serialisation.Generators(mkDummyHash) -- imports arbitray instance for MultiSig
import Test.Tasty.QuickCheck hiding (scale)

-- ================================================================

-- ================================================================================
-- Some generators for Timelock

genSlot :: Gen (StrictMaybe SlotNo)
genSlot = oneof [ pure SNothing, (SJust . SlotNo) <$> choose (0,10)]

maxTimelockDepth :: Int
maxTimelockDepth = 3

maxTimelockListLens :: Int
maxTimelockListLens = 5

sizedTimelock :: (Era era) => Int -> Gen (Timelock era)
sizedTimelock 0 = (RequireSignature . KeyHash . mkDummyHash) <$> arbitrary
sizedTimelock n =
  oneof
    [ (RequireSignature . KeyHash . mkDummyHash) <$> arbitrary,
      RequireAllOf <$> (fromList <$> resize maxTimelockListLens (listOf (sizedTimelock (n -1)))),
      RequireAnyOf <$> (fromList <$> resize maxTimelockListLens (listOf (sizedTimelock (n -1)))),
      do subs <-  resize maxTimelockListLens (listOf (sizedTimelock (n -1)))
         let i = length subs
         RequireMOf <$> choose (0,i) <*> pure (fromList subs),
      RequireTimeStart <$> genSlot,
      RequireTimeExpire <$> genSlot
    ]

instance (Era era) => Arbitrary (Timelock era) where
  arbitrary = sizedTimelock maxTimelockDepth
