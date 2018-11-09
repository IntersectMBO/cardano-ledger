module Cardano.Chain.Txp.Undo
  ( TxpUndo
  , TxUndo
  )
where

import Cardano.Prelude

import Cardano.Chain.Txp.TxOutAux


type TxpUndo = [TxUndo]

-- | Particular undo needed for transactions
--
--   Just means we know transaction input, hence know TxOutAux corresponding to
--   it, Nothing otherwise.
type TxUndo = NonEmpty (Maybe TxOutAux)
