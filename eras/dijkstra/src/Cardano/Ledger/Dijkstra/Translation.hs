{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Dijkstra.Translation () where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTx (..), EraTx (mkBasicTx))
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Core (
  EraTx (auxDataTxL, bodyTxL, witsTxL),
  TranslateEra (..),
  translateEraThroughCBOR,
 )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Dijkstra.TxBody ()
import Cardano.Ledger.Dijkstra.TxWits ()
import Lens.Micro ((&), (.~), (^.))

newtype Tx era = Tx (Core.Tx era)

instance TranslateEra DijkstraEra Tx where
  type TranslationError DijkstraEra Tx = DecoderError
  translateEra _ctxt (Tx tx) = do
    -- Note that this does not preserve the hidden bytes field of the transaction.
    -- This is under the premise that this is irrelevant for TxInBlocks, which are
    -- not transmitted as contiguous chunks.
    txBody <- translateEraThroughCBOR $ tx ^. bodyTxL
    txWits <- translateEraThroughCBOR $ tx ^. witsTxL
    auxData <- mapM translateEraThroughCBOR (tx ^. auxDataTxL)
    let isValidTx = tx ^. isValidTxL
        newTx =
          mkBasicTx txBody
            & witsTxL .~ txWits
            & isValidTxL .~ isValidTx
            & auxDataTxL .~ auxData
    pure $ Tx newTx
