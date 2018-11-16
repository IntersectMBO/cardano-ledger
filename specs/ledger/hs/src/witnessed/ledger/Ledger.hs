{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Ledger where

import Control.Lens
import Control.State.Transition
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Ledger.Abstract
import Ledger.Simple (UTXO, utxoInductive)
import qualified Ledger.Simple
import UTxO

-- | UTXO transition system
data UTXOW

instance STS UTXOW where
  type State UTXOW = UTxO
  type Signal UTXOW = TxWits
  type Environment UTXOW = Ledger.Simple.ProtocolConstants
  data PredicateFailure UTXOW
    = UtxoFailure [PredicateFailure UTXO]
    | InsufficientWitnesses
    deriving (Eq, Show)

  rules =
    [ Rule [] $ Base (UTxO Map.empty)
    , Rule
      [ SubTrans st1
      , Predicate $ \(pc, utxo, tw) -> witnessed tw utxo
      ]
      ( Extension . Transition $
        \jc -> subTransResult jc st1
      )
    ]
    where
      st1 = EmbeddedTransition (to $ \(env, st, sig) -> (env, st, body sig)) utxoInductive

instance Embed UTXO UTXOW where
  wrapFailed = UtxoFailure

-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> hash key == pay
  _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> UTxO -> PredicateResult UTXOW
witnessed (TxWits tx wits) utxo =
  if Set.size wits == Set.size ins && all (hasWitness wits) ins
    then Passed
    else Failed InsufficientWitnesses
 where
  ins = inputs tx
  hasWitness witnesses input =
    isJust $ find (isWitness tx input utxo) witnesses
  isWitness tx' input unspent (Wit key sig) =
    verify key tx' sig && authTxin key input unspent
