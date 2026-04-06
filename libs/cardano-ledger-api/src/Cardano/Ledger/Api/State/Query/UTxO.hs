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
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:1259
--   answerShelleyTraversingQueries case for GetUTxOWhole
--
-- __Warning:__ This is an expensive query — it returns the entire UTxO map.
-- Prefer 'queryUTxOByAddress' or 'queryUTxOByTxIn' when possible.
--
-- /O(1)/
queryUTxOFull ::
  NewEpochState era ->
  UTxO era
queryUTxOFull nes = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxoL

-- | Query UTxO entries filtered by address.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:1258
--   answerShelleyTraversingQueries case for GetUTxOByAddress
--
-- Returns only UTxO entries whose address is in the given set.
-- An empty set returns an empty UTxO (use 'queryUTxOFull' for everything).
--
-- @
--   O(u * log(k))
-- @
-- where,
--   (u) is the size of the full UTxO map, Map.filter with Set.member per entry
--   (k) is the size of the address set, Set.member lookup per UTxO entry
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
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:1180
--   answerShelleyLookupQueries case for GetUTxOByTxIn
--
-- Returns only UTxO entries whose 'TxIn' is in the given set.
-- An empty set returns an empty UTxO (use 'queryUTxOFull' for everything).
--
-- @
--   O(u + k)
-- @
-- where,
--   (u) is the size of the full UTxO map, Map.restrictKeys
--   (k) is the size of the TxIn set, Map.restrictKeys
queryUTxOByTxIn ::
  NewEpochState era ->
  Set TxIn ->
  UTxO era
queryUTxOByTxIn nes = txInsFilter (queryUTxOFull nes)
