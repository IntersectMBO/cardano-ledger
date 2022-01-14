{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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

import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..))
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    updatePParams,
  )
import qualified Cardano.Ledger.Alonzo.Rules.Bbody as Alonzo (AlonzoBBODY)
import qualified Cardano.Ledger.Alonzo.Rules.Ledger as Alonzo (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (AlonzoUTXO)
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo (UTXOS)
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Alonzo (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts (Script (..), isPlutusScript)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), minfee)
import Cardano.Ledger.Alonzo.TxBody (TxBody, TxOut (TxOut), getAlonzoTxOutEitherAddr)
import Cardano.Ledger.Alonzo.TxInfo (validScript)
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo (TxSeq (..), hashTxSeq)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..), ValidateAuxiliaryData (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as EraModule
import Cardano.Ledger.Keys (GenDelegs (GenDelegs))
import qualified Cardano.Ledger.Mary.Value as V (Value)
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (nativeMultiSigTag)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Constraints
  ( UsesPParams (..),
    UsesTxOut (..),
    UsesValue,
  )
import Cardano.Ledger.Shelley.EpochBoundary
import Cardano.Ledger.Shelley.Genesis (genesisUTxO, sgGenDelegs, sgMaxLovelaceSupply, sgProtocolParams)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    smartUTxOState,
    _genDelegs,
  )
import Cardano.Ledger.Shelley.Metadata (validMetadatum)
import qualified Cardano.Ledger.Shelley.Rules.Epoch as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Mir as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Newpp as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Rupd as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Snap as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Tick as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Upec as Shelley
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import Cardano.Ledger.Shelley.UTxO (balance)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed)
import Cardano.Ledger.ShelleyMA.Timelocks (validateTimelock)
import Cardano.Ledger.Val (Val (inject), coin, (<->))
import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (TRC))
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set

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

  getTxOutEitherAddr = getAlonzoTxOutEitherAddr

instance API.ShelleyEraCrypto c => API.ApplyTx (AlonzoEra c) where
  reapplyTx globals env state vtx =
    let res =
          flip runReader globals
            . applySTSNonStatic
              @(Core.EraRule "LEDGER" (AlonzoEra c))
            $ TRC (env, state, API.extractTx vtx)
     in liftEither . left API.ApplyTxError $ res

instance API.ShelleyEraCrypto c => API.ApplyBlock (AlonzoEra c)

instance (CC.Crypto c) => Shelley.ValidateScript (AlonzoEra c) where
  isNativeScript x = not (isPlutusScript x)
  scriptPrefixTag script =
    case script of
      (TimelockScript _) -> nativeMultiSigTag -- "\x00"
      (PlutusScript PlutusV1 _) -> "\x01"
      (PlutusScript PlutusV2 _) -> "\x02"
  validateScript (TimelockScript script) tx = validateTimelock @(AlonzoEra c) script tx
  validateScript (PlutusScript _ _) _tx = True

-- To run a PlutusScript use Cardano.Ledger.Alonzo.TxInfo(runPLCScript)
-- To run any Alonzo Script use Cardano.Ledger.Alonzo.PlutusScriptApi(evalScripts)
-- hashScript x = ...  We use the default method for hashScript

instance
  ( CC.Crypto c
  ) =>
  API.CanStartFromGenesis (AlonzoEra c)
  where
  type AdditionalGenesisConfig (AlonzoEra c) = AlonzoGenesis

  initialState sg ag =
    NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( EpochState
          (AccountState (Coin 0) reserves)
          emptySnapShots
          ( LedgerState
              ( smartUTxOState
                  initialUtxo
                  (Coin 0)
                  (Coin 0)
                  def
              )
              (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
          )
          (extendPPWithGenesis pp ag)
          (extendPPWithGenesis pp ag)
          def
      )
      SNothing
      (PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        coin $
          inject (word64ToCoin (sgMaxLovelaceSupply sg))
            <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance CC.Crypto c => UsesTxOut (AlonzoEra c) where
  makeTxOut _proxy addr val = TxOut addr val SNothing

instance CC.Crypto c => API.CLI (AlonzoEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses (ValidatedTx b ws aux iv) newWits = ValidatedTx b ws' aux iv
    where
      ws' = ws {txwitsVKey = Set.union newWits (txwitsVKey ws)}

  evaluateMinLovelaceOutput pp out =
    Coin $ utxoEntrySize out * unCoin (_coinsPerUTxOWord pp)

type instance Core.Tx (AlonzoEra c) = ValidatedTx (AlonzoEra c)

type instance Core.TxOut (AlonzoEra c) = TxOut (AlonzoEra c)

type instance Core.TxBody (AlonzoEra c) = TxBody (AlonzoEra c)

type instance Core.Value (AlonzoEra c) = V.Value c

type instance Core.Script (AlonzoEra c) = Script (AlonzoEra c)

type instance Core.AuxiliaryData (AlonzoEra c) = AuxiliaryData (AlonzoEra c)

type instance Core.PParams (AlonzoEra c) = PParams (AlonzoEra c)

type instance Core.Witnesses (AlonzoEra c) = TxWitness (AlonzoEra c)

type instance Core.PParamsDelta (AlonzoEra c) = PParamsUpdate (AlonzoEra c)

instance CC.Crypto c => UsesValue (AlonzoEra c)

instance (CC.Crypto c) => UsesPParams (AlonzoEra c) where
  mergePPUpdates _ = updatePParams

instance CC.Crypto c => ValidateAuxiliaryData (AlonzoEra c) c where
  hashAuxiliaryData x = AuxiliaryDataHash (hashAnnotated x)
  validateAuxiliaryData (AuxiliaryData metadata scrips) =
    all validMetadatum metadata
      && all validScript scrips

instance CC.Crypto c => EraModule.SupportsSegWit (AlonzoEra c) where
  type TxSeq (AlonzoEra c) = Alonzo.TxSeq (AlonzoEra c)
  fromTxSeq = Alonzo.txSeqTxns
  toTxSeq = Alonzo.TxSeq
  hashTxSeq = Alonzo.hashTxSeq
  numSegComponents = 4

instance API.ShelleyEraCrypto c => API.ShelleyBasedEra (AlonzoEra c)

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

type instance Core.EraRule "POOL" (AlonzoEra c) = API.POOL (AlonzoEra c)

type instance Core.EraRule "POOLREAP" (AlonzoEra c) = API.POOLREAP (AlonzoEra c)

type instance Core.EraRule "PPUP" (AlonzoEra c) = API.PPUP (AlonzoEra c)

type instance Core.EraRule "RUPD" (AlonzoEra c) = Shelley.RUPD (AlonzoEra c)

type instance Core.EraRule "SNAP" (AlonzoEra c) = Shelley.SNAP (AlonzoEra c)

type instance Core.EraRule "TICK" (AlonzoEra c) = Shelley.TICK (AlonzoEra c)

type instance Core.EraRule "TICKF" (AlonzoEra c) = Shelley.TICKF (AlonzoEra c)

type instance Core.EraRule "UPEC" (AlonzoEra c) = Shelley.UPEC (AlonzoEra c)

-- Self-Describing type synomyms

type Self c = AlonzoEra c

type Value era = V.Value (EraModule.Crypto era)

type PParamsDelta era = PParamsUpdate era
