{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Alonzo.Plutus.Context (
  LedgerTxInfo (..),
  EraPlutusTxInfo (..),
  EraPlutusContext (..),
  toPlutusWithContext,

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
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
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
  toPlutusTxCert :: proxy l -> TxCert era -> Either (ContextError era) (PlutusTxCert l)

  toPlutusScriptPurpose ::
    proxy l ->
    PlutusPurpose AsIxItem era ->
    Either (ContextError era) (PlutusScriptPurpose l)

  toPlutusTxInfo ::
    proxy l ->
    LedgerTxInfo era ->
    Either (ContextError era) (PlutusTxInfo l)

  toPlutusArgs ::
    proxy l ->
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

  mkPlutusWithContext ::
    PlutusScript era ->
    ScriptHash (EraCrypto era) ->
    PlutusPurpose AsIxItem era ->
    LedgerTxInfo era ->
    (Data era, ExUnits) ->
    CostModel ->
    Either (ContextError era) (PlutusWithContext (EraCrypto era))

toPlutusWithContext ::
  forall l era.
  (EraPlutusTxInfo l era, AlonzoEraUTxO era) =>
  Either (Plutus l) (PlutusRunnable l) ->
  ScriptHash (EraCrypto era) ->
  PlutusPurpose AsIxItem era ->
  LedgerTxInfo era ->
  (Data era, ExUnits) ->
  CostModel ->
  Either (ContextError era) (PlutusWithContext (EraCrypto era))
toPlutusWithContext script scriptHash plutusPurpose lti (redeemerData, exUnits) costModel = do
  let proxy = Proxy @l
      spendingDatum =
        getSpendingDatum (ltiUTxO lti) (ltiTx lti) (hoistPlutusPurpose toAsItem plutusPurpose)
  txInfo <- toPlutusTxInfo proxy lti
  plutusArgs <- toPlutusArgs proxy txInfo plutusPurpose spendingDatum redeemerData
  pure $
    PlutusWithContext
      { pwcProtocolVersion = pvMajor (ltiProtVer lti)
      , pwcScript = script
      , pwcScriptHash = scriptHash
      , pwcArgs = plutusArgs
      , pwcExUnits = exUnits
      , pwcCostModel = costModel
      }

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
