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
    TxBody,
    Script,
    AuxiliaryData,
  )
where

import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.Rules.Bbody as Alonzo (AlonzoBBODY)
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)
import Cardano.Ledger.Alonzo.Scripts (Script (..), isPlutusScript)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), validScript)
import qualified Cardano.Ledger.Alonzo.TxSeq as Alonzo (TxSeq (..), hashTxSeq)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..), ValidateAuxiliaryData (..))
import Cardano.Ledger.Babbage.Genesis
import Cardano.Ledger.Babbage.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    updatePParams,
  )
import Cardano.Ledger.Babbage.Rules.Ledger (BabbageLEDGER)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUTXO)
import Cardano.Ledger.Babbage.Rules.Utxos (BabbageUTXOS)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUTXOW)
import Cardano.Ledger.Babbage.Scripts (babbageInputDataHashes, babbageTxScripts)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..), minfee)
import Cardano.Ledger.Babbage.TxBody (Datum (..), TxBody, TxOut (TxOut), getBabbageTxOutEitherAddr)
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfo)
import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as EraModule
import Cardano.Ledger.Keys (GenDelegs (GenDelegs))
import qualified Cardano.Ledger.Mary.Value as Mary (Value)
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (nativeMultiSigTag)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.API.Validation (ShelleyEraCrypto)
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
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed)
import Cardano.Ledger.ShelleyMA.Timelocks (validateTimelock)
import Cardano.Ledger.Val (Val (inject), coin, (<->))
import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (TRC))
import qualified Data.Compact.SplitMap as SplitMap
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set
import GHC.Records (HasField (..))

-- =====================================================

instance (ShelleyEraCrypto c) => API.ApplyTx (BabbageEra c) where
  reapplyTx globals env state vtx =
    let res =
          flip runReader globals
            . applySTSNonStatic
              @(Core.EraRule "LEDGER" (BabbageEra c))
            $ TRC (env, state, API.extractTx vtx)
     in liftEither . left (API.ApplyTxError) $ res

instance ShelleyEraCrypto c => API.ApplyBlock (BabbageEra c)

instance ShelleyEraCrypto c => API.ShelleyBasedEra (BabbageEra c)

-- | The Alonzo era
data BabbageEra c

instance
  ( CC.Crypto c
  ) =>
  EraModule.Era (BabbageEra c)
  where
  type Crypto (BabbageEra c) = c

  getTxOutEitherAddr = getBabbageTxOutEitherAddr

instance (CC.Crypto c) => Shelley.ValidateScript (BabbageEra c) where
  isNativeScript x = not (isPlutusScript x)
  scriptPrefixTag script =
    case script of
      (TimelockScript _) -> nativeMultiSigTag -- "\x00"
      (PlutusScript PlutusV1 _) -> "\x01"
      (PlutusScript PlutusV2 _) -> "\x02"
  validateScript (TimelockScript script) tx = validateTimelock @(BabbageEra c) script tx
  validateScript (PlutusScript _ _) _tx = True

instance
  ( CC.Crypto c
  ) =>
  API.CanStartFromGenesis (BabbageEra c)
  where
  type AdditionalGenesisConfig (BabbageEra c) = AlonzoGenesis

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

instance CC.Crypto c => UsesTxOut (BabbageEra c) where
  makeTxOut _proxy addr val = TxOut addr val NoDatum SNothing

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

type instance Core.Value (BabbageEra c) = Mary.Value c

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
  validateAuxiliaryData pv (AuxiliaryData metadata scrips) =
    all validMetadatum metadata
      && all (validScript pv) scrips

instance CC.Crypto c => EraModule.SupportsSegWit (BabbageEra c) where
  type TxSeq (BabbageEra c) = Alonzo.TxSeq (BabbageEra c)
  fromTxSeq = Alonzo.txSeqTxns
  toTxSeq = Alonzo.TxSeq
  hashTxSeq = Alonzo.hashTxSeq
  numSegComponents = 4

instance CC.Crypto c => ExtendedUTxO (BabbageEra c) where
  txInfo = babbageTxInfo
  inputDataHashes = babbageInputDataHashes
  txscripts = babbageTxScripts
  getAllowedSupplimentalDataHashes txbody (UTxO utxo) =
    Set.fromList [dh | out <- outs, SJust dh <- [getField @"datahash" out]]
    where
      newOuts = toList $ getField @"outputs" txbody
      referencedOuts = SplitMap.elems $ SplitMap.restrictKeysSet utxo (getField @"referenceInputs" txbody)
      outs = newOuts <> referencedOuts

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- Rules inherited from Alonzo

type instance Core.EraRule "UTXOS" (BabbageEra c) = BabbageUTXOS (BabbageEra c)

type instance Core.EraRule "UTXO" (BabbageEra c) = BabbageUTXO (BabbageEra c)

type instance Core.EraRule "UTXOW" (BabbageEra c) = BabbageUTXOW (BabbageEra c)

type instance Core.EraRule "LEDGER" (BabbageEra c) = BabbageLEDGER (BabbageEra c)

type instance Core.EraRule "BBODY" (BabbageEra c) = Alonzo.AlonzoBBODY (BabbageEra c)

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

-- =================================================
