{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo
  ( AlonzoEra,
    Self,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
  )
where

import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..), getPlutusData)
import Cardano.Ledger.Alonzo.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    updatePParams,
  )
import qualified Cardano.Ledger.Alonzo.Rules.Bbody as Alonzo (AlonzoBBODY)
import qualified Cardano.Ledger.Alonzo.Rules.Ledger as Alonzo (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (UtxosFailure))
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (AlonzoUTXO)
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo (UTXOS, constructValidated)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (WrappedShelleyEraFailure))
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Alonzo (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts (Script (..), isPlutusScript)
import Cardano.Ledger.Alonzo.Tx
  ( ValidatedTx (..),
    body',
    wits',
  )
import Cardano.Ledger.Alonzo.TxBody (TxBody, TxOut (..), vldt')
import Cardano.Ledger.Alonzo.TxInfo (validPlutusdata, validScript)
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo (TxSeq (..), hashTxSeq)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txwitsVKey'))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..), ValidateAuxiliaryData (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as EraModule
import qualified Cardano.Ledger.Mary.Value as V (Value)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (nativeMultiSigTag)
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxOut (..),
    UsesValue,
  )
import Cardano.Ledger.ShelleyMA.Timelocks (evalTimelock)
import Cardano.Ledger.Tx (Tx (Tx))
import Control.Arrow (left)
import Control.Monad.Except (liftEither, runExcept)
import qualified Data.Set as Set
import qualified Shelley.Spec.Ledger.API as API
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    PState (..),
  )
import Shelley.Spec.Ledger.Metadata (validMetadatum)
import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley
import Shelley.Spec.Ledger.STS.Ledger
  ( LedgerEnv (..),
    LedgerPredicateFailure (UtxowFailure),
  )
import qualified Shelley.Spec.Ledger.STS.Mir as Shelley
import qualified Shelley.Spec.Ledger.STS.Newpp as Shelley
import qualified Shelley.Spec.Ledger.STS.Ocert as Shelley
import qualified Shelley.Spec.Ledger.STS.Overlay as Shelley
import qualified Shelley.Spec.Ledger.STS.Rupd as Shelley
import qualified Shelley.Spec.Ledger.STS.Snap as Shelley
import qualified Shelley.Spec.Ledger.STS.Tick as Shelley
import qualified Shelley.Spec.Ledger.STS.Upec as Shelley
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (UtxoFailure))
import qualified Shelley.Spec.Ledger.Tx as Shelley
import Shelley.Spec.Ledger.TxBody (witKeyHash)

-- =====================================================

-- | The Alonzo era
data AlonzoEra c

instance
  ( CC.Crypto c,
    era ~ AlonzoEra c
  ) =>
  EraModule.Era (AlonzoEra c)
  where
  type Crypto (AlonzoEra c) = c

instance API.PraosCrypto c => API.ApplyTx (AlonzoEra c) where
  applyTx globals (LedgerEnv slot _ix pp _accnt) (utxostate, dpstate) tx =
    do
      (utxostate2, vtx) <-
        liftEither
          . left
            ( API.ApplyTxError
                . fmap
                  ( UtxowFailure
                      . WrappedShelleyEraFailure
                      . UtxoFailure
                      . UtxosFailure
                  )
            )
          . runExcept
          $ Alonzo.constructValidated globals utxoenv utxostate tx
      pure ((utxostate2, dpstate), vtx)
    where
      delegs = (_genDelegs . _dstate) dpstate
      stake = (_pParams . _pstate) dpstate
      utxoenv = API.UtxoEnv slot pp stake delegs

  applyTxInBlock = undefined

  extractTx ValidatedTx {body, wits, auxiliaryData} =
    Tx body wits auxiliaryData

instance API.PraosCrypto c => API.ApplyBlock (AlonzoEra c)

instance (API.PraosCrypto c) => API.GetLedgerView (AlonzoEra c)

instance (CC.Crypto c) => Shelley.ValidateScript (AlonzoEra c) where
  isNativeScript x = not (isPlutusScript x)
  scriptPrefixTag script =
    if isPlutusScript script
      then "\x01"
      else nativeMultiSigTag -- "\x00"
  validateScript (TimelockScript timelock) tx =
    evalTimelock
      vhks
      (vldt' (body' tx))
      timelock
    where
      vhks = Set.map witKeyHash (txwitsVKey' (wits' tx))
  -- TODO check if instead we should filter plutus scripts before calling
  validateScript (PlutusScript _) _tx = True

-- To run a PlutusScript use Cardano.Ledger.Alonzo.TxInfo(runPLCScript)
-- To run any Alonzo Script use Cardano.Ledger.Alonzo.PlutusScriptApi(evalScripts)
-- hashScript x = ...  We use the default method for hashScript

instance
  ( CC.Crypto c
  ) =>
  API.CanStartFromGenesis (AlonzoEra c)
  where
  -- type AdditionalGenesisConfig era = ()
  -- initialState :: ShelleyGenesis era -> AdditionalGenesisConfig era -> NewEpochState era
  initialState _ _ = error "TODO: implement initialState"

instance (CC.Crypto c) => UsesTxOut (AlonzoEra c) where
  makeTxOut _proxy addr val = TxOut addr val Shelley.SNothing

type instance Core.TxOut (AlonzoEra c) = TxOut (AlonzoEra c)

type instance Core.TxBody (AlonzoEra c) = TxBody (AlonzoEra c)

type instance Core.Value (AlonzoEra c) = V.Value c

type instance Core.Script (AlonzoEra c) = Script (AlonzoEra c)

type instance Core.AuxiliaryData (AlonzoEra c) = AuxiliaryData (AlonzoEra c)

type instance Core.PParams (AlonzoEra c) = PParams (AlonzoEra c)

type instance Core.Witnesses (AlonzoEra c) = TxWitness (AlonzoEra c)

instance CC.Crypto c => UsesValue (AlonzoEra c)

instance
  (CC.Crypto c) =>
  UsesPParams (AlonzoEra c)
  where
  type
    PParamsDelta (AlonzoEra c) =
      PParamsUpdate (AlonzoEra c)

  mergePPUpdates _ = updatePParams

instance CC.Crypto c => ValidateAuxiliaryData (AlonzoEra c) c where
  hashAuxiliaryData x = AuxiliaryDataHash (hashAnnotated x)
  validateAuxiliaryData (AuxiliaryData metadata scrips plutusdata) =
    all validMetadatum metadata
      && all validScript scrips
      && all (validPlutusdata . getPlutusData) plutusdata

instance CC.Crypto c => EraModule.SupportsSegWit (AlonzoEra c) where
  type TxSeq (AlonzoEra c) = Alonzo.TxSeq (AlonzoEra c)
  type TxInBlock (AlonzoEra c) = ValidatedTx (AlonzoEra c)
  fromTxSeq = Alonzo.txSeqTxns
  toTxSeq = Alonzo.TxSeq
  hashTxSeq = Alonzo.hashTxSeq

instance API.PraosCrypto c => API.ShelleyBasedEra (AlonzoEra c)

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Alonzo

type instance Core.EraRule "UTXOS" (AlonzoEra c) = Alonzo.UTXOS (AlonzoEra c)

type instance Core.EraRule "UTXO" (AlonzoEra c) = Alonzo.AlonzoUTXO (AlonzoEra c)

type instance Core.EraRule "UTXOW" (AlonzoEra c) = Alonzo.AlonzoUTXOW (AlonzoEra c)

type instance Core.EraRule "LEDGER" (AlonzoEra c) = Alonzo.AlonzoLEDGER (AlonzoEra c)

type instance Core.EraRule "BBODY" (AlonzoEra c) = Alonzo.AlonzoBBODY (AlonzoEra c)

-- Rules inherited from Shelley

type instance Core.EraRule "DELEG" (AlonzoEra c) = API.DELEG (AlonzoEra c)

type instance Core.EraRule "DELEGS" (AlonzoEra c) = API.DELEGS (AlonzoEra c)

type instance Core.EraRule "DELPL" (AlonzoEra c) = API.DELPL (AlonzoEra c)

type instance Core.EraRule "EPOCH" (AlonzoEra c) = Shelley.EPOCH (AlonzoEra c)

type instance Core.EraRule "LEDGERS" (AlonzoEra c) = API.LEDGERS (AlonzoEra c)

type instance Core.EraRule "MIR" (AlonzoEra c) = Shelley.MIR (AlonzoEra c)

type instance Core.EraRule "NEWEPOCH" (AlonzoEra c) = API.NEWEPOCH (AlonzoEra c)

type instance Core.EraRule "NEWPP" (AlonzoEra c) = Shelley.NEWPP (AlonzoEra c)

type instance Core.EraRule "OCERT" (AlonzoEra c) = Shelley.OCERT (AlonzoEra c)

type instance Core.EraRule "OVERLAY" (AlonzoEra c) = Shelley.OVERLAY (AlonzoEra c)

type instance Core.EraRule "POOL" (AlonzoEra c) = API.POOL (AlonzoEra c)

type instance Core.EraRule "POOLREAP" (AlonzoEra c) = API.POOLREAP (AlonzoEra c)

type instance Core.EraRule "PPUP" (AlonzoEra c) = API.PPUP (AlonzoEra c)

type instance Core.EraRule "RUPD" (AlonzoEra c) = Shelley.RUPD (AlonzoEra c)

type instance Core.EraRule "SNAP" (AlonzoEra c) = Shelley.SNAP (AlonzoEra c)

type instance Core.EraRule "TICK" (AlonzoEra c) = Shelley.TICK (AlonzoEra c)

type instance Core.EraRule "TICKF" (AlonzoEra c) = Shelley.TICKF (AlonzoEra c)

type instance Core.EraRule "TICKN" (AlonzoEra _c) = API.TICKN

type instance Core.EraRule "UPEC" (AlonzoEra c) = Shelley.UPEC (AlonzoEra c)

-- Self-Describing type synomyms

type Self c = AlonzoEra c

type Value era = V.Value (EraModule.Crypto era)
