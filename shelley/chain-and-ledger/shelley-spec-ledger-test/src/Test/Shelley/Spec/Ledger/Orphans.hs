{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Orphans () where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Crypto (DSIGN)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley (ShelleyBased)
import Shelley.Spec.Ledger.BlockChain (Block (..), TxSeq (..))
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState
  ( EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.STS.Chain
  ( ChainState (..),
  )
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
  )
import Test.Shelley.Spec.Ledger.Utils (Split (..), HedGen (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Test.QuickCheck ( Arbitrary (..), choose )
import Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis (genCoin)

-- We need this here for the tests, but should not be in the actual library because
-- a Num instance for this type does not make sense in the general case.
deriving instance Num (DSIGN.VerKeyDSIGN (DSIGN crypto)) => Num (VKey kd crypto)

deriving instance ShelleyBased era => Eq (UTxOState era)

deriving instance ShelleyBased era => Eq (UTxO era)

deriving instance ShelleyBased era => Eq (ChainState era)

deriving instance ShelleyBased era => Eq (NewEpochState era)

deriving instance ShelleyBased era => Eq (EpochState era)

deriving instance ShelleyBased era => Eq (LedgerState era)

deriving instance ShelleyBased era => Eq (Tx era)

deriving instance ShelleyBased era => Eq (Block era)

deriving instance ShelleyBased era => Eq (TxSeq era)


-- ===============================================================================
-- Generating random transactions requires splitting Values into multiple Values
-- with the same underlying amount of Coin. This property is crucial to generating
-- transactions which have the preservation of ADA property. (vsplit n v) breaks
-- v into n different values, and one remainder Coin, where the sum of the Coin
-- in the original value, and the sum of the underlying Coin in the list plus the
-- remainder coin are equal.
-- Given:    let (vs,coin) = split n value
-- Then:     (coin value) == sum(map coin vs) <+> coin

-- We introduce a new class Split which supplies this operation.
-- As new kinds of values become instances of the Val class, and we want to generate
-- transactions over these values, we will have to add additional instances here.

instance Split Coin where
  vsplit (Coin n) 0 = ([], Coin n)
  vsplit (Coin n) m 
    | m Prelude.<= 0 = error "must split coins into positive parts"
    | otherwise = (take (fromIntegral m) (repeat (Coin (n `div` m))), Coin (n `rem` m))

instance HedGen Coin where
  genHeg = genCoin

instance Arbitrary Coin where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = Coin <$> choose (0, 1000)

deriving instance (Split (Core.VALUE era)) => Split (Core.Value era)
-- ============================================================

deriving instance (Arbitrary (Core.VALUE era)) => Arbitrary (Core.Value era)
instance (HedGen (Core.VALUE era)) => HedGen (Core.Value era) where
  genHeg = Core.Value <$> genHeg
