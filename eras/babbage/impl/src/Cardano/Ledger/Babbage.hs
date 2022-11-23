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
  ( Babbage,
    BabbageEra,
    BabbageTxOut,
    BabbageTxBody,
    AlonzoScript,
    AlonzoTxAuxData,

    -- * Deprecated
    Self,
    TxOut,
    TxBody,
    Script,
    AuxiliaryData,
  )
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.Data (AlonzoTxAuxData (..), AuxiliaryData)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), Script)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..))
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Genesis (AlonzoGenesis, extendPPWithGenesis)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.Tx
  ( babbageTxScripts,
    getDatumBabbage,
  )
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody (..),
    BabbageTxBody,
    BabbageTxOut,
    TxBody,
    TxOut,
    dataHashTxOutL,
  )
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfo)
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable, Hash)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Set as Set
import Lens.Micro

type Babbage = BabbageEra StandardCrypto

-- =====================================================

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyTx (BabbageEra c) where
  reapplyTx = reapplyAlonzoTx

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyBlock (BabbageEra c)

instance Crypto c => API.CanStartFromGenesis (BabbageEra c) where
  type AdditionalGenesisConfig (BabbageEra c) = AlonzoGenesis

  initialState = API.initialStateFromGenesis extendPPWithGenesis

instance Crypto c => ExtendedUTxO (BabbageEra c) where
  txInfo = babbageTxInfo
  txscripts = babbageTxScripts
  getAllowedSupplimentalDataHashes txBody (UTxO utxo) =
    Set.fromList [dh | txOut <- outs, SJust dh <- [txOut ^. dataHashTxOutL]]
    where
      newOuts = map sizedValue $ toList $ txBody ^. allSizedOutputsTxBodyF
      referencedOuts = Map.elems $ Map.restrictKeys utxo (txBody ^. referenceInputsTxBodyL)
      outs = newOuts <> referencedOuts
  getDatum = getDatumBabbage

-- Self-Describing type synomyms

type Self c = BabbageEra c

{-# DEPRECATED Self "Use `BabbageEra` instead" #-}

-- =================================================
