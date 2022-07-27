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

module Cardano.Ledger.Babbage
  ( BabbageEra,
    BabbageTxOut,
    BabbageTxBody,
    AlonzoScript,
    AlonzoAuxiliaryData,

    -- * Deprecated
    Self,
    TxOut,
    TxBody,
    Script,
    AuxiliaryData,
  )
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.Data (AlonzoAuxiliaryData (..), AuxiliaryData)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), Script)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Genesis (AlonzoGenesis, extendPPWithGenesis)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.Rules (babbageMinUTxOValue)
import Cardano.Ledger.Babbage.Tx
  ( AlonzoTx (..),
    babbageInputDataHashes,
    babbageTxScripts,
    getDatumBabbage,
    minfee,
  )
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody (referenceInputsTxBodyL, sizedCollateralReturnTxBodyL, sizedOutputsTxBodyL),
    BabbageTxBody,
    BabbageTxOut (BabbageTxOut),
    TxBody,
    TxOut,
    dataHashTxOutL,
  )
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfo)
import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Coin (Coin (Coin), word64ToCoin)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable, GenDelegs (GenDelegs), Hash)
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Serialization (mkSized)
import qualified Cardano.Ledger.Shelley.API as API
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
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.ShelleyMA.Rules (consumed)
import Cardano.Ledger.Val (Val (inject), coin, (<->))
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set
import Lens.Micro

-- =====================================================

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyTx (BabbageEra c) where
  reapplyTx = reapplyAlonzoTx

instance (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyBlock (BabbageEra c)

instance CC.Crypto c => API.CanStartFromGenesis (BabbageEra c) where
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

instance CC.Crypto c => API.CLI (BabbageEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses (AlonzoTx b ws aux iv) newWits = AlonzoTx b ws' aux iv
    where
      ws' = ws {txwitsVKey = Set.union newWits (txwitsVKey ws)}

  evaluateMinLovelaceOutput pp out = babbageMinUTxOValue pp (mkSized out)

instance CC.Crypto c => ExtendedUTxO (BabbageEra c) where
  txInfo = babbageTxInfo
  inputDataHashes = babbageInputDataHashes
  txscripts = babbageTxScripts
  getAllowedSupplimentalDataHashes txBody (UTxO utxo) =
    Set.fromList [dh | txOut <- outs, SJust dh <- [txOut ^. dataHashTxOutL]]
    where
      newOuts = allOuts txBody
      referencedOuts = Map.elems $ Map.restrictKeys utxo (txBody ^. referenceInputsTxBodyL)
      outs = newOuts <> referencedOuts
  getDatum = getDatumBabbage
  getTxOutDatum (BabbageTxOut _ _ datum _) = datum
  allSizedOuts txBody = toList (txBody ^. sizedOutputsTxBodyL) <> collOuts
    where
      collOuts = case txBody ^. sizedCollateralReturnTxBodyL of
        SNothing -> []
        SJust x -> [x]

-- Self-Describing type synomyms

type Self c = BabbageEra c

{-# DEPRECATED Self "Use `BabbageEra` instead" #-}

-- =================================================
