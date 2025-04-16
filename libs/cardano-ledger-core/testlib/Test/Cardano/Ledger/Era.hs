{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Era (
  EraTest,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis
import Cardano.Ledger.State
import Data.Functor.Identity
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.TreeDiff ()

class
  ( -- Core
    EraTx era
  , EraTxOut era
  , EraTxBody era
  , EraTxAuxData era
  , EraTxWits era
  , EraScript era
  , EraPParams era
  , EraSegWits era
  , EraTxCert era
  , -- State
    EraCertState era
  , EraGov era
  , EraStake era
  , EraUTxO era
  , -- Other
    EraGenesis era
  , -- Arbitrary Core
    Arbitrary (Tx era)
  , Arbitrary (TxOut era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (TxWits era)
  , Arbitrary (Script era)
  , -- , Arbitrary (PParams era)
    Arbitrary (PParamsHKD Identity era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (TxSeq era) -- TODO: probably don't need this
  , Arbitrary (TxCert era)
  , Arbitrary (Value era)
  , -- Arbitrary State
    Arbitrary (CertState era)
  , Arbitrary (GovState era)
  , Arbitrary (InstantStake era)
  , -- , Arbitrary (UTxO era)
    -- Arbitrary Other
    Arbitrary (Genesis era)
  , -- ToExpr Core
    ToExpr (Tx era)
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxAuxData era)
  , ToExpr (TxWits era)
  , ToExpr (Script era)
  , -- , ToExpr (PParams era)
    ToExpr (PParamsHKD Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (TxSeq era) -- TODO: probably don't need this
  , ToExpr (TxCert era)
  , ToExpr (Value era)
  , -- ToExpr State
    ToExpr (CertState era)
  , ToExpr (GovState era)
  , ToExpr (InstantStake era)
  , -- , ToExpr (UTxO era)
    -- ToExpr Other
    ToExpr (Genesis era)
  ) =>
  EraTest era
