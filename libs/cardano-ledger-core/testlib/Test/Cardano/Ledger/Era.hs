{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Cardano.Ledger.Era (
  EraTest (..),
  GenSize (..),
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (CostModels)
import Cardano.Ledger.State
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import Data.Typeable
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.TreeDiff ()
import Data.Word (Word64)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)

-- | Constants that determine how big a GenState is generated.
data GenSize = GenSize
  { treasury :: !Integer
  , reserves :: !Integer
  , startSlot :: !Word64
  , slotDelta :: !(Word64, Word64)
  , blocksizeMax :: !Integer
  , collInputsMax :: !Natural
  , spendInputsMax :: !Int
  , refInputsMax :: !Int
  , utxoChoicesMax :: !Int
  , certificateMax :: !Int
  , withdrawalMax :: !Int
  , oldUtxoPercent :: !Int -- between 0-100, 10 means pick an old UTxO 10% of the time
  , maxStablePools :: !Int
  , invalidScriptFreq :: !Int -- percentage
  , regCertFreq :: !Int
  , delegCertFreq :: !Int
  }
  deriving (Show, Generic)

instance ToExpr GenSize

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
  , -- Arbitrary Core
    Arbitrary (Tx era)
  , Arbitrary (TxOut era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (TxWits era)
  , Arbitrary (Script era)
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (TxCert era)
  , Arbitrary (Value era)
  , -- Arbitrary State
    Arbitrary (CertState era)
  , Arbitrary (GovState era)
  , Arbitrary (InstantStake era)
  , -- ToExpr Core
    ToExpr (Tx era)
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxAuxData era)
  , ToExpr (TxWits era)
  , ToExpr (Script era)
  , ToExpr (PParamsHKD Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (TxCert era)
  , ToExpr (Value era)
  , -- ToExpr State
    ToExpr (CertState era)
  , ToExpr (GovState era)
  , ToExpr (InstantStake era)
  , -- TranslationContext
    Eq (TranslationContext era)
  , Show (TranslationContext era)
  , Typeable (TranslationContext era)
  , ToJSON (TranslationContext era)
  , FromJSON (TranslationContext era)
  , Arbitrary (TranslationContext era)
  ) =>
  EraTest era
  where
  validTxOut :: Map ScriptHash (Script era) -> TxOut era -> Bool
  zeroCostModels :: CostModels
  genPParams :: GenSize -> Gen (PParams era)
