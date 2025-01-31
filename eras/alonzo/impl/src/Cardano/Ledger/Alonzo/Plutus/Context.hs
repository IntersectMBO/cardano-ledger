{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Recursive definition constraints of `EraPlutusContext` and `EraPlutusTxInfo` lead to a wrongful
-- redundant constraint warning in the definition of `lookupTxInfoResult`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Alonzo.Plutus.Context (
  LedgerTxInfo (..),
  EraPlutusTxInfo (..),
  EraPlutusContext (..),
  toPlutusWithContext,
  lookupTxInfoResultImpossible,

  -- * Language dependent translation
  PlutusTxInfo,
  PlutusTxCert,
  PlutusScriptPurpose,
  PlutusScriptContext,
)
where

import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript,
  AsIxItem (..),
  PlutusPurpose,
  PlutusScript (..),
  hoistPlutusPurpose,
  toAsItem,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (getSpendingDatum))
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (
  CostModel,
  Data,
  ExUnits,
  Language (..),
  Plutus,
  PlutusArgs,
  PlutusLanguage,
  PlutusRunnable,
  PlutusScriptContext,
  PlutusWithContext (..),
  SLanguage (..),
  isLanguage,
 )
import Cardano.Ledger.State (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Stack
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

-- | All information that is necessary from the ledger to construct Plutus' TxInfo.
data LedgerTxInfo era = LedgerTxInfo
  { ltiProtVer :: !ProtVer
  , ltiEpochInfo :: !(EpochInfo (Either Text))
  , ltiSystemStart :: !SystemStart
  , ltiUTxO :: !(UTxO era)
  , ltiTx :: !(Tx era)
  }

class (PlutusLanguage l, EraPlutusContext era) => EraPlutusTxInfo (l :: Language) era where
  toPlutusTxCert ::
    proxy l ->
    ProtVer ->
    TxCert era ->
    Either (ContextError era) (PlutusTxCert l)

  toPlutusScriptPurpose ::
    proxy l ->
    ProtVer ->
    PlutusPurpose AsIxItem era ->
    Either (ContextError era) (PlutusScriptPurpose l)

  toPlutusTxInfo ::
    proxy l ->
    LedgerTxInfo era ->
    Either (ContextError era) (PlutusTxInfo l)

  toPlutusArgs ::
    proxy l ->
    ProtVer ->
    PlutusTxInfo l ->
    PlutusPurpose AsIxItem era ->
    Maybe (Data era) ->
    Data era ->
    Either (ContextError era) (PlutusArgs l)

class
  ( AlonzoEraScript era
  , Eq (ContextError era)
  , Show (ContextError era)
  , NFData (ContextError era)
  , NoThunks (ContextError era)
  , EncCBOR (ContextError era)
  , DecCBOR (ContextError era)
  , ToJSON (ContextError era)
  ) =>
  EraPlutusContext era
  where
  type ContextError era = (r :: Type) | r -> era

  -- | This data type family is used to memoize the results of `toPlutusTxInfo`, so the outcome can
  -- be shared between execution of different scripts with the same language version.
  data TxInfoResult era :: Type

  -- | Construct `PlutusTxInfo` for all supported languages in this era.
  mkTxInfoResult :: LedgerTxInfo era -> TxInfoResult era

  -- | `TxInfo` for the same language can be shared between executions of every script of the same
  -- version in a single transaction.
  --
  -- /Note/ - The `EraPlutusTxInfo` is here only to enforce this function is not called with an
  -- unsupported plutus language version.
  lookupTxInfoResult ::
    EraPlutusTxInfo l era =>
    SLanguage l -> TxInfoResult era -> Either (ContextError era) (PlutusTxInfo l)

  mkPlutusWithContext ::
    PlutusScript era ->
    ScriptHash ->
    PlutusPurpose AsIxItem era ->
    LedgerTxInfo era ->
    TxInfoResult era ->
    (Data era, ExUnits) ->
    CostModel ->
    Either (ContextError era) PlutusWithContext

toPlutusWithContext ::
  forall l era.
  (EraPlutusTxInfo l era, AlonzoEraUTxO era) =>
  Either (Plutus l) (PlutusRunnable l) ->
  ScriptHash ->
  PlutusPurpose AsIxItem era ->
  LedgerTxInfo era ->
  TxInfoResult era ->
  (Data era, ExUnits) ->
  CostModel ->
  Either (ContextError era) PlutusWithContext
toPlutusWithContext script scriptHash plutusPurpose lti txInfoResult (redeemerData, exUnits) costModel = do
  let slang = isLanguage @l
      maybeSpendingDatum =
        getSpendingDatum (ltiUTxO lti) (ltiTx lti) (hoistPlutusPurpose toAsItem plutusPurpose)
  txInfo <- lookupTxInfoResult slang txInfoResult
  plutusArgs <-
    toPlutusArgs slang (ltiProtVer lti) txInfo plutusPurpose maybeSpendingDatum redeemerData
  pure $
    PlutusWithContext
      { pwcProtocolVersion = pvMajor (ltiProtVer lti)
      , pwcScript = script
      , pwcScriptHash = scriptHash
      , pwcArgs = plutusArgs
      , pwcExUnits = exUnits
      , pwcCostModel = costModel
      }

-- | Helper function to use when implementing `lookupTxInfoResult` for plutus languages that are not
-- supported by the era.
lookupTxInfoResultImpossible ::
  (HasCallStack, EraPlutusTxInfo l era) => SLanguage l -> Either (ContextError era) (PlutusTxInfo l)
lookupTxInfoResultImpossible slang =
  error $ "Impossible: Attempt to lookup TxInfoResult for an unsupported language: " <> show slang

-- =============================================
-- Type families that specify Plutus types that are different from one version to another

type family PlutusTxCert (l :: Language) where
  PlutusTxCert 'PlutusV1 = PV1.DCert
  PlutusTxCert 'PlutusV2 = PV2.DCert
  PlutusTxCert 'PlutusV3 = PV3.TxCert

type family PlutusScriptPurpose (l :: Language) where
  PlutusScriptPurpose 'PlutusV1 = PV1.ScriptPurpose
  PlutusScriptPurpose 'PlutusV2 = PV2.ScriptPurpose
  PlutusScriptPurpose 'PlutusV3 = PV3.ScriptPurpose

type family PlutusTxInfo (l :: Language) where
  PlutusTxInfo 'PlutusV1 = PV1.TxInfo
  PlutusTxInfo 'PlutusV2 = PV2.TxInfo
  PlutusTxInfo 'PlutusV3 = PV3.TxInfo
