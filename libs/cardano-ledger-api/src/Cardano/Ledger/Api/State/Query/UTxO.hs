module Cardano.Ledger.Api.State.Query.UTxO (
  queryUTxOFull,
  queryUTxOByAddress,
  queryUTxOByTxIn,
) where

import Cardano.Ledger.Address (Addr, compactAddr)
import Cardano.Ledger.Core (EraTxOut, addrEitherTxOutL)
import Cardano.Ledger.Shelley.LedgerState (
  CanSetUTxO (utxoL),
  NewEpochState,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
 )
import Cardano.Ledger.State (UTxO (..), txInsFilter)
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))

-- | Query the full UTxO set.
--
-- __Warning:__ This is an expensive query — it returns the entire
-- UTxO map. Prefer 'queryUTxOByAddress' or 'queryUTxOByTxIn' when
-- possible.
queryUTxOFull ::
  NewEpochState era ->
  UTxO era
queryUTxOFull nes = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxoL

-- | Query UTxO entries filtered by address.
--
-- Returns only UTxO entries whose address is in the given set. An empty
-- set returns an empty UTxO (use 'queryUTxOFull' for everything).
queryUTxOByAddress ::
  EraTxOut era =>
  NewEpochState era ->
  Set Addr ->
  UTxO era
queryUTxOByAddress nes addrSet =
  UTxO $ Map.filter checkAddr fullUTxO
  where
    UTxO fullUTxO = queryUTxOFull nes
    compactAddrSet = Set.map compactAddr addrSet
    checkAddr out =
      case out ^. addrEitherTxOutL of
        Left addr -> addr `Set.member` addrSet
        Right cAddr -> cAddr `Set.member` compactAddrSet

-- | Query UTxO entries filtered by transaction input.
--
-- Returns only UTxO entries whose 'TxIn' is in the given set. An empty
-- set returns an empty UTxO (use 'queryUTxOFull' for everything).
queryUTxOByTxIn ::
  NewEpochState era ->
  Set TxIn ->
  UTxO era
queryUTxOByTxIn nes = txInsFilter (queryUTxOFull nes)
