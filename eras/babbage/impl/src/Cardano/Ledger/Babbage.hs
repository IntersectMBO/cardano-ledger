{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babbage
  ( BabbageEra,
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

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..), ValidateAuxiliaryData (..))
import Cardano.Ledger.Babbage.Data (AuxiliaryData (..))
import Cardano.Ledger.Babbage.Genesis
import Cardano.Ledger.Babbage.Language (Language (..))
import Cardano.Ledger.Babbage.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    updatePParams,
  )
import qualified Cardano.Ledger.Babbage.Rules.Bbody as Babbage (BabbageBBODY)
import qualified Cardano.Ledger.Babbage.Rules.Ledger as Babbage (BabbageLEDGER)
import Cardano.Ledger.Babbage.Rules.Utxo (utxoEntrySize)
import qualified Cardano.Ledger.Babbage.Rules.Utxo as Babbage (BabbageUTXO)
import qualified Cardano.Ledger.Babbage.Rules.Utxos as Babbage (UTXOS)
import qualified Cardano.Ledger.Babbage.Rules.Utxow as Babbage (BabbageUTXOW)
import Cardano.Ledger.Babbage.Scripts (Script (..), isPlutusScript)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..), minfee)
import Cardano.Ledger.Babbage.TxBody (TxBody, TxOut (..), Datum(..))
import Cardano.Ledger.Babbage.TxInfo (validScript)
import qualified Cardano.Ledger.Babbage.TxSeq as Babbage (TxSeq (..), hashTxSeq)
import Cardano.Ledger.Babbage.TxWitness (TxWitness (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as EraModule
import Cardano.Ledger.Keys (GenDelegs (GenDelegs))
import qualified Cardano.Ledger.Mary.Value as V (Value)
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Rules.ValidationMode
  ( applySTSNonStatic,
  )
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
    IncrementalStake (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
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

-- | The Babbage era
data BabbageEra c

instance
  ( CC.Crypto c,
    era ~ BabbageEra c
  ) =>
  EraModule.Era (BabbageEra c)
  where
  type Crypto (BabbageEra c) = c

instance API.PraosCrypto c => API.ApplyTx (BabbageEra c) where
  reapplyTx globals env state vtx =
    let res =
          flip runReader globals
            . applySTSNonStatic
              @(Core.EraRule "LEDGER" (BabbageEra c))
            $ TRC (env, state, API.extractTx vtx)
     in liftEither . left API.ApplyTxError $ res

instance API.PraosCrypto c => API.ApplyBlock (BabbageEra c)

instance (API.PraosCrypto c) => API.GetLedgerView (BabbageEra c)

instance (CC.Crypto c) => Shelley.ValidateScript (BabbageEra c) where
  isNativeScript x = not (isPlutusScript x)
  scriptPrefixTag script =
    case script of
      (TimelockScript _) -> nativeMultiSigTag -- "\x00"
      (PlutusScript PlutusV1 _) -> "\x01"
      (PlutusScript PlutusV2 _) -> "\x02"
  validateScript (TimelockScript script) tx = validateTimelock @(BabbageEra c) script tx
  validateScript (PlutusScript _ _) _tx = True

-- To run a PlutusScript use Cardano.Ledger.Babbage.TxInfo(runPLCScript)
-- To run any Babbage Script use Cardano.Ledger.Babbage.PlutusScriptApi(evalScripts)
-- hashScript x = ...  We use the default method for hashScript

instance
  ( CC.Crypto c
  ) =>
  API.CanStartFromGenesis (BabbageEra c)
  where
  type AdditionalGenesisConfig (BabbageEra c) = BabbageGenesis

  initialState sg ag =
    NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( EpochState
          (AccountState (Coin 0) reserves)
          emptySnapShots
          ( LedgerState
              ( UTxOState
                  initialUtxo
                  (Coin 0)
                  (Coin 0)
                  def
                  (IStake mempty mempty)
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

instance (CC.Crypto c) => UsesTxOut (BabbageEra c) where
  makeTxOut _proxy addr val = TxOut addr val NoDatum

instance CC.Crypto c => API.CLI (BabbageEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses (ValidatedTx b ws aux iv) newWits = ValidatedTx b ws' aux iv
    where
      ws' = ws {txwitsVKey = Set.union newWits (txwitsVKey ws)}

  evaluateMinLovelaceOutput pp out =
    Coin $ utxoEntrySize out * unCoin (_coinsPerUTxOWord pp)

type instance Core.Tx (BabbageEra c) = ValidatedTx (BabbageEra c)

type instance Core.TxOut (BabbageEra c) = TxOut (BabbageEra c)

type instance Core.TxBody (BabbageEra c) = TxBody (BabbageEra c)

type instance Core.Value (BabbageEra c) = V.Value c

type instance Core.Script (BabbageEra c) = Script (BabbageEra c)

type instance Core.AuxiliaryData (BabbageEra c) = AuxiliaryData (BabbageEra c)

type instance Core.PParams (BabbageEra c) = PParams (BabbageEra c)

type instance Core.Witnesses (BabbageEra c) = TxWitness (BabbageEra c)

type instance Core.PParamsDelta (BabbageEra c) = PParamsUpdate (BabbageEra c)

instance CC.Crypto c => UsesValue (BabbageEra c)

instance (CC.Crypto c) => UsesPParams (BabbageEra c) where
  mergePPUpdates _ = updatePParams

instance CC.Crypto c => ValidateAuxiliaryData (BabbageEra c) c where
  hashAuxiliaryData x = AuxiliaryDataHash (hashAnnotated x)
  validateAuxiliaryData (AuxiliaryData metadata scrips) =
    all validMetadatum metadata
      && all validScript scrips

instance CC.Crypto c => EraModule.SupportsSegWit (BabbageEra c) where
  type TxSeq (BabbageEra c) = Babbage.TxSeq (BabbageEra c)
  fromTxSeq = Babbage.txSeqTxns
  toTxSeq = Babbage.TxSeq
  hashTxSeq = Babbage.hashTxSeq
  numSegComponents = 4

instance API.PraosCrypto c => API.ShelleyBasedEra (BabbageEra c)

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Babbage

type instance Core.EraRule "UTXOS" (BabbageEra c) = Babbage.UTXOS (BabbageEra c)

type instance Core.EraRule "UTXO" (BabbageEra c) = Babbage.BabbageUTXO (BabbageEra c)

type instance Core.EraRule "UTXOW" (BabbageEra c) = Babbage.BabbageUTXOW (BabbageEra c)

type instance Core.EraRule "LEDGER" (BabbageEra c) = Babbage.BabbageLEDGER (BabbageEra c)

type instance Core.EraRule "BBODY" (BabbageEra c) = Babbage.BabbageBBODY (BabbageEra c)

-- Rules inherited from Shelley

type instance Core.EraRule "DELEG" (BabbageEra c) = API.DELEG (BabbageEra c)

type instance Core.EraRule "DELEGS" (BabbageEra c) = API.DELEGS (BabbageEra c)

type instance Core.EraRule "DELPL" (BabbageEra c) = API.DELPL (BabbageEra c)

type instance Core.EraRule "EPOCH" (BabbageEra c) = Shelley.EPOCH (BabbageEra c)

type instance Core.EraRule "LEDGERS" (BabbageEra c) = API.LEDGERS (BabbageEra c)

type instance Core.EraRule "MIR" (BabbageEra c) = Shelley.MIR (BabbageEra c)

type instance Core.EraRule "NEWEPOCH" (BabbageEra c) = API.NEWEPOCH (BabbageEra c)

type instance Core.EraRule "NEWPP" (BabbageEra c) = Shelley.NEWPP (BabbageEra c)

type instance Core.EraRule "POOL" (BabbageEra c) = API.POOL (BabbageEra c)

type instance Core.EraRule "POOLREAP" (BabbageEra c) = API.POOLREAP (BabbageEra c)

type instance Core.EraRule "PPUP" (BabbageEra c) = API.PPUP (BabbageEra c)

type instance Core.EraRule "RUPD" (BabbageEra c) = Shelley.RUPD (BabbageEra c)

type instance Core.EraRule "SNAP" (BabbageEra c) = Shelley.SNAP (BabbageEra c)

type instance Core.EraRule "TICK" (BabbageEra c) = Shelley.TICK (BabbageEra c)

type instance Core.EraRule "TICKF" (BabbageEra c) = Shelley.TICKF (BabbageEra c)

type instance Core.EraRule "UPEC" (BabbageEra c) = Shelley.UPEC (BabbageEra c)

-- Self-Describing type synomyms

type Self c = BabbageEra c

type Value era = V.Value (EraModule.Crypto era)

type PParamsDelta era = PParamsUpdate era
