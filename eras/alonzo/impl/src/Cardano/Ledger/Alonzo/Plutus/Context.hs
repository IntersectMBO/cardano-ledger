{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusTxInfo (..),
  EraPlutusContext (..),
  mkPlutusLanguageContext,
)
where

import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, PlutusScript (..))
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..), PlutusLanguage)
import Cardano.Ledger.Plutus.TxInfo
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Text (Text)
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as P (ToData, toData)

class (PlutusLanguage l, AlonzoEraScript era) => EraPlutusTxInfo (l :: Language) era where
  toPlutusTxCert :: proxy l -> TxCert era -> Either (ContextError era) (PlutusTxCert l)

  toPlutusScriptPurpose ::
    proxy l ->
    ScriptPurpose era ->
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
    ScriptPurpose era ->
    Either (ContextError era) (PlutusScriptContext l)

class
  ( AlonzoEraScript era
  , Eq (ContextError era)
  , Show (ContextError era)
  , NFData (ContextError era)
  , NoThunks (ContextError era)
  , EncCBOR (ContextError era)
  , DecCBOR (ContextError era)
  ) =>
  EraPlutusContext era
  where
  data ContextError era :: Type

  mkPlutusScriptContext ::
    PlutusScript era ->
    ScriptPurpose era ->
    PParams era ->
    EpochInfo (Either Text) ->
    SystemStart ->
    UTxO era ->
    Tx era ->
    Either (ContextError era) (Data era)

mkPlutusLanguageContext ::
  (EraPlutusTxInfo l era, P.ToData (PlutusScriptContext l)) =>
  proxy l ->
  ScriptPurpose era ->
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
