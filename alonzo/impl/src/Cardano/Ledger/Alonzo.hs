{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo
  ( AlonzoEra,
    Era,
    proxy,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    Tx,
  )
where

import Cardano.Ledger.Alonzo.Data (AuxiliaryData)
import Cardano.Ledger.Alonzo.PParams (PParams, PParamsUpdate, updatePParams)
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (Tx)
import Cardano.Ledger.Alonzo.TxBody (TxBody, TxOut)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..), ValidateAuxiliaryData (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as EraModule
import qualified Cardano.Ledger.Mary.Value as V (Value)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Constraints (UsesPParams (..), UsesValue)
import Data.Proxy (Proxy (..))

-- | The Alonzo era
data AlonzoEra c

instance
  (CC.Crypto c) =>
  EraModule.Era (AlonzoEra c)
  where
  type Crypto (AlonzoEra c) = c

type instance Core.TxOut (AlonzoEra c) = TxOut (AlonzoEra c)

type instance Core.TxBody (AlonzoEra c) = TxBody (AlonzoEra c)

type instance Core.TxOut (AlonzoEra c) = TxOut (AlonzoEra c)

type instance Core.Value (AlonzoEra c) = V.Value c

type instance Core.Script (AlonzoEra c) = Script (AlonzoEra c)

type instance Core.AuxiliaryData (AlonzoEra c) = AuxiliaryData (AlonzoEra c)

type instance Core.PParams (AlonzoEra c) = PParams (AlonzoEra c)

type instance Core.Tx (AlonzoEra c) = Tx (AlonzoEra c)

instance CC.Crypto c => UsesValue (AlonzoEra c)

instance
  (CC.Crypto c) =>
  UsesPParams (AlonzoEra c)
  where
  type
    PParamsDelta (AlonzoEra c) =
      PParamsUpdate (AlonzoEra c)

  mergePPUpdates _ = updatePParams

instance CC.Crypto c => ValidateAuxiliaryData (AlonzoEra c) where
  hashAuxiliaryData x = AuxiliaryDataHash (hashAnnotated x)
  validateAuxiliaryData = error ("NO validateAuxiliaryData yet.") -- TODO Fill this in

proxy :: Proxy (AlonzoEra c)
proxy = Proxy

type Era c = AlonzoEra c

type Value era = V.Value (EraModule.Crypto era)
