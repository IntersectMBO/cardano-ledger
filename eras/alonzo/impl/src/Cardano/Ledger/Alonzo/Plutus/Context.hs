{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusTxInfo (..),
  EraPlutusContext (..),
  mkPlutusLanguageContext,

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
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..), PlutusLanguage)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Text (Text)
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as P (ToData, toData)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4

class (PlutusLanguage l, EraPlutusContext era) => EraPlutusTxInfo (l :: Language) era where
  toPlutusTxCert :: proxy l -> TxCert era -> Either (ContextError era) (PlutusTxCert l)

  toPlutusScriptPurpose ::
    proxy l ->
    PlutusPurpose AsIxItem era ->
    Either (ContextError era) (PlutusScriptPurpose l)

  toPlutusTxInfo ::
    proxy l ->
    PParams era ->
    EpochInfo (Either Text) ->
    SystemStart ->
    UTxO era ->
    Tx era ->
    Either (ContextError era) (PlutusTxInfo l)

  toPlutusScriptContext ::
    proxy l ->
    PlutusTxInfo l ->
    PlutusPurpose AsIxItem era ->
    Either (ContextError era) (PlutusScriptContext l)

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

  mkPlutusScriptContext ::
    PlutusScript era ->
    PlutusPurpose AsIxItem era ->
    PParams era ->
    EpochInfo (Either Text) ->
    SystemStart ->
    UTxO era ->
    Tx era ->
    Either (ContextError era) (Data era)

mkPlutusLanguageContext ::
  (EraPlutusTxInfo l era, P.ToData (PlutusScriptContext l)) =>
  proxy l ->
  PlutusPurpose AsIxItem era ->
  PParams era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx era ->
  Either (ContextError era) (Data era)
mkPlutusLanguageContext proxy scriptPurpose pp epochInfo sysStart utxo tx = do
  txInfo <- toPlutusTxInfo proxy pp epochInfo sysStart utxo tx
  scriptContext <- toPlutusScriptContext proxy txInfo scriptPurpose
  pure $ Data $ P.toData scriptContext

-- =============================================
-- Type families that specify Plutus types that are different from one version to another

type family PlutusTxCert (l :: Language) where
  PlutusTxCert 'PlutusV1 = PV1.DCert
  PlutusTxCert 'PlutusV2 = PV2.DCert
  PlutusTxCert 'PlutusV3 = PV3.TxCert
  PlutusTxCert 'PlutusV4 = PV4.TxCert

type family PlutusScriptPurpose (l :: Language) where
  PlutusScriptPurpose 'PlutusV1 = PV1.ScriptPurpose
  PlutusScriptPurpose 'PlutusV2 = PV2.ScriptPurpose
  PlutusScriptPurpose 'PlutusV3 = PV3.ScriptPurpose
  PlutusScriptPurpose 'PlutusV4 = PV4.ScriptPurpose

type family PlutusScriptContext (l :: Language) where
  PlutusScriptContext 'PlutusV1 = PV1.ScriptContext
  PlutusScriptContext 'PlutusV2 = PV2.ScriptContext
  PlutusScriptContext 'PlutusV3 = PV3.ScriptContext
  PlutusScriptContext 'PlutusV4 = PV4.ScriptContext

type family PlutusTxInfo (l :: Language) where
  PlutusTxInfo 'PlutusV1 = PV1.TxInfo
  PlutusTxInfo 'PlutusV2 = PV2.TxInfo
  PlutusTxInfo 'PlutusV3 = PV3.TxInfo
  PlutusTxInfo 'PlutusV4 = PV4.TxInfo
