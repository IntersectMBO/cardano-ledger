{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Genesis (AlonzoGenesis, extendPPWithGenesis)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Tx
  ( babbageInputDataHashes,
    babbageTxScripts,
    getDatumBabbage,
  )
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody (referenceInputsTxBodyL, sizedCollateralReturnTxBodyL, sizedOutputsTxBodyL),
    BabbageTxBody,
    BabbageTxOut,
    TxBody,
    TxOut,
    dataHashTxOutL,
  )
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfo)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable, Hash)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Rules (consumed)
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

  initialState = API.initialStateFromGenesis extendPPWithGenesis

instance CC.Crypto c => API.CLI (BabbageEra c) where
  evaluateConsumed = consumed

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
  allSizedOuts txBody = toList (txBody ^. sizedOutputsTxBodyL) <> collOuts
    where
      collOuts = case txBody ^. sizedCollateralReturnTxBodyL of
        SNothing -> []
        SJust x -> [x]

-- Self-Describing type synomyms

type Self c = BabbageEra c

{-# DEPRECATED Self "Use `BabbageEra` instead" #-}

-- =================================================
