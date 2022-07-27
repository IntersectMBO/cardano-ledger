{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo
  ( AlonzoEra,
    AlonzoTxOut,
    MaryValue,
    AlonzoTxBody,
    AlonzoScript,
    AlonzoAuxiliaryData,
    AlonzoPParams,
    AlonzoPParamsUpdate,
    reapplyAlonzoTx,

    -- * Deprecated
    Self,
    Cardano.Ledger.Alonzo.TxBody.TxOut,
    Value,
    Cardano.Ledger.Alonzo.TxBody.TxBody,
    Cardano.Ledger.Alonzo.Scripts.Script,
    Cardano.Ledger.Alonzo.Data.AuxiliaryData,
    Cardano.Ledger.Alonzo.PParams.PParams,
    PParamsDelta,
  )
where

import Cardano.Ledger.Alonzo.Data (AlonzoAuxiliaryData, Datum (..))
import qualified Cardano.Ledger.Alonzo.Data (AuxiliaryData)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.PParams
  ( AlonzoPParams,
    AlonzoPParamsHKD (..),
    AlonzoPParamsUpdate,
  )
import qualified Cardano.Ledger.Alonzo.PParams (PParams)
import Cardano.Ledger.Alonzo.PlutusScriptApi (getDatumAlonzo)
import Cardano.Ledger.Alonzo.Rules (utxoEntrySize)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), Script)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), alonzoInputHashes, minfee)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..), AlonzoTxBody, AlonzoTxOut)
import qualified Cardano.Ledger.Alonzo.TxBody (TxBody, TxOut)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), alonzoTxInfo)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..), Globals)
import Cardano.Ledger.Coin
import Cardano.Ledger.Core hiding (PParamsDelta, Value)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (DSignable, GenDelegs (GenDelegs), Hash)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import Cardano.Ledger.Serialization (mkSized)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.API.Mempool
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
import Cardano.Ledger.Shelley.UTxO (balance)
import Cardano.Ledger.ShelleyMA.Rules (consumed)
import Cardano.Ledger.Val (Val (inject), coin, (<->))
import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (TRC))
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Extras (view)

-- =====================================================

reapplyAlonzoTx ::
  forall era m.
  (API.ApplyTx era, MonadError (ApplyTxError era) m) =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Validated (Tx era) ->
  m (MempoolState era)
reapplyAlonzoTx globals env state vtx =
  let res =
        flip runReader globals
          . applySTSNonStatic
            @(EraRule "LEDGER" era)
          $ TRC (env, state, API.extractTx vtx)
   in liftEither . left API.ApplyTxError $ res

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyTx (AlonzoEra c) where
  reapplyTx = reapplyAlonzoTx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyBlock (AlonzoEra c)

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
      ()
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        coin $
          inject (word64ToCoin (sgMaxLovelaceSupply sg))
            <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance CC.Crypto c => API.CLI (AlonzoEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses (AlonzoTx b ws aux iv) newWits = AlonzoTx b ws' aux iv
    where
      ws' = ws {txwitsVKey = Set.union newWits (txwitsVKey ws)}

  evaluateMinLovelaceOutput pp out =
    Coin $ utxoEntrySize out * unCoin (_coinsPerUTxOWord pp)

instance CC.Crypto c => ExtendedUTxO (AlonzoEra c) where
  txInfo = alonzoTxInfo
  inputDataHashes = alonzoInputHashes
  txscripts _ = txscripts' . view witsTxL
  getAllowedSupplimentalDataHashes txBody _ =
    Set.fromList
      [ dh
        | txOut <- allOuts txBody,
          SJust dh <- [txOut ^. dataHashTxOutL]
      ]
  getDatum = getDatumAlonzo
  getTxOutDatum txOut =
    case txOut ^. dataHashTxOutL of
      SNothing -> NoDatum
      SJust dh -> DatumHash dh
  allOuts txBody = toList $ txBody ^. outputsTxBodyL
  allSizedOuts = map mkSized . allOuts

-- Self-Describing type synomyms

type Self c = AlonzoEra c

{-# DEPRECATED Self "Use `AlonzoEra` instead" #-}

type Value era = MaryValue (Crypto era)

{-# DEPRECATED Value "Use `MaryValue` instead" #-}

type PParamsDelta era = AlonzoPParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `AlonzoPParamsUpdate` instead" #-}
