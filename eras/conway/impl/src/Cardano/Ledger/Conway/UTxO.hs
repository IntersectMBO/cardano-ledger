{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.UTxO () where

import Cardano.Ledger.Alonzo.UTxO (
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsHashesNeeded,
  getAlonzoScriptsNeeded,
 )
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..), EraTxBody (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance (VotingProcedure (..))
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO)
import Data.Foldable (Foldable (..))
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.))

getConwayScriptsNeeded ::
  ConwayEraTxBody era =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getConwayScriptsNeeded utxo txb = getAlonzoScriptsNeeded utxo txb <> voteScripts
  where
    getMaybeScript VotingProcedure {..} = case vProcRoleKeyHash of
      ScriptHashObj sh -> Just (error "TODO Add the voting script purpose here once that gets added", sh)
      _ -> Nothing

    voteScripts =
      AlonzoScriptsNeeded . mapMaybe getMaybeScript . toList $
        txb ^. votingProceduresTxBodyL

instance Crypto c => EraUTxO (ConwayEra c) where
  type ScriptsNeeded (ConwayEra c) = AlonzoScriptsNeeded (ConwayEra c)

  getConsumedValue = getConsumedMaryValue

  getScriptsNeeded = getConwayScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded
