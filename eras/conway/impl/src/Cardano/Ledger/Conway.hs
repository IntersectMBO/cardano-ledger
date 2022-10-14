{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway
  ( Conway,
    ConwayEra,
  )
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..))
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Tx (babbageTxScripts, getDatumBabbage)
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfo)
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (extendPPWithGenesis)
import Cardano.Ledger.Conway.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxBody (BabbageEraTxBody (..))
import Cardano.Ledger.Conway.TxOut (AlonzoEraTxOut (..))
import Cardano.Ledger.Conway.UTxO ()
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

type Conway = ConwayEra StandardCrypto

-- =====================================================

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyTx (ConwayEra c) where
  reapplyTx = reapplyAlonzoTx

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyBlock (ConwayEra c)

instance Crypto c => API.CanStartFromGenesis (ConwayEra c) where
  type AdditionalGenesisConfig (ConwayEra c) = AlonzoGenesis

  initialState = API.initialStateFromGenesis extendPPWithGenesis

instance Crypto c => ExtendedUTxO (ConwayEra c) where
  txInfo = babbageTxInfo
  txscripts = babbageTxScripts
  getAllowedSupplimentalDataHashes txBody (UTxO utxo) =
    Set.fromList [dh | txOut <- outs, SJust dh <- [txOut ^. dataHashTxOutL]]
    where
      newOuts = map sizedValue $ toList $ txBody ^. allSizedOutputsTxBodyF
      referencedOuts = Map.elems $ Map.restrictKeys utxo (txBody ^. referenceInputsTxBodyL)
      outs = newOuts <> referencedOuts
  getDatum = getDatumBabbage
