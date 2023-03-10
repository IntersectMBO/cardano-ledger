{-# OPTIONS_HADDOCK hide, not-home #-}

-- | Provide Tools for debugging
--   Feel free to add new things as they are developed.
module Cardano.Ledger.Shelley.Internal (
  trace,
  totalAdaPotsES,
  compareAdaPots,
  producedTxBody,
  consumedTxBody,
  showCred,
  showIR,
  showKeyHash,
  showListy,
  showMap,
  showWithdrawal,
  showSafeHash,
  synopsisCert,
  synopsisCoinMap,
  showTxCerts,
)
where

import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  consumedTxBody,
  producedTxBody,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Rules.Reports (
  showCred,
  showIR,
  showKeyHash,
  showListy,
  showMap,
  showSafeHash,
  showTxCerts,
  showWithdrawal,
  synopsisCert,
  synopsisCoinMap,
 )
import Cardano.Ledger.Val ((<->))
import Debug.Trace (trace)

-- ==============================================
-- Compare two AdaPots, item by item

pad :: Int -> String -> String
pad n x = x ++ replicate (n - length x) ' '

compareAdaPots :: String -> AdaPots -> String -> AdaPots -> String
compareAdaPots xlabel x ylabel y =
  unlines
    [ pad n "field" ++ pad n xlabel ++ pad n ylabel ++ pad n "difference"
    , oneline "treasuryAdaPot" treasuryAdaPot
    , oneline "reservesAdaPot" reservesAdaPot
    , oneline "rewardsAdaPot" rewardsAdaPot
    , oneline "utxoAdaPot" utxoAdaPot
    , oneline "keyDepositAdaPot" keyDepositAdaPot
    , oneline "poolDepositAdaPot" poolDepositAdaPot
    , oneline "depositsAdaPot" depositsAdaPot
    , oneline "feesAdaPot" feesAdaPot
    ]
  where
    n = 25
    oneline name f =
      pad n name ++ pad n (show (f x)) ++ pad n (show (f y)) ++ pad n (show (f y <-> f x))
