{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Orphans () where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Crypto (DSIGN)
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
