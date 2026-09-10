module Cardano.Ledger.Plutus (
  module Cardano.Ledger.Plutus.CostModels,
  module Cardano.Ledger.Plutus.Data,
  module Cardano.Ledger.Plutus.ExUnits,
  module Cardano.Ledger.Plutus.Language,
  module Cardano.Ledger.Plutus.TxInfo,
  module Cardano.Ledger.Plutus.Evaluate,
  assocMapToList,
  assocMapKeys,
  assocMapElems,
) where

import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.Evaluate
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Plutus.TxInfo
import qualified PlutusTx.AssocMap as AssocMap

assocMapToList :: AssocMap.Map k v -> [(k, v)]
assocMapToList = AssocMap.toList

assocMapKeys :: AssocMap.Map k v -> [k]
assocMapKeys = map fst . assocMapToList

assocMapElems :: AssocMap.Map k v -> [v]
assocMapElems = map snd . assocMapToList
