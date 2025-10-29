{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tools for reporting things in readable manner. Used in Rules to implement
--   STS 'renderAssertionViolation' methods, and in Tests.
module Cardano.Ledger.Shelley.Rules.Reports (
  showCred,
  showIR,
  showKeyHash,
  showListy,
  showMap,
  showWithdrawal,
  showSafeHash,
  synopsisCoinMap,
  showTxCerts,
  produceEqualsConsumed,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.State (EraCertState (..), InstantaneousRewards (..), UTxO (..))
import Data.Foldable (fold, toList)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))

-- ===============================================
-- Reporting Certificates

showCred :: Credential x -> String
showCred (ScriptHashObj (ScriptHash x)) = show x
showCred (KeyHashObj (KeyHash x)) = show x

showKeyHash :: KeyHash x -> String
showKeyHash (KeyHash hash) = take 10 (show hash)

showCerts :: Show (TxCert era) => [TxCert era] -> String
showCerts certs = unlines (map (("  " ++) . show) certs)

showTxCerts :: EraTxBody era => TxBody t era -> String
showTxCerts txb = case toList (txb ^. certsTxBodyL) of
  [] -> "No TxCerts in this TxBody\n" ++ show txb
  certs -> showCerts certs

-- | Display a synopsis of a map to Coin
synopsisCoinMap :: Maybe (Map.Map k Coin) -> String
synopsisCoinMap (Just m) = "Count = " ++ show (Map.size m) ++ ",  total = " ++ show (fold m)
synopsisCoinMap Nothing = "SYNOPSIS NOTHING"

-- ===============================================
-- Printing Produced == Consumed

produceEqualsConsumed ::
  (EraTxBody era, EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody TopTx era ->
  String
produceEqualsConsumed pp dpstate utxo txb =
  let consumedValue = consumedTxBody txb pp dpstate utxo
      producedValue = producedTxBody txb pp dpstate
   in ( "\n (Produce = Consumed) Report\n  certificates\n"
          ++ showTxCerts txb
          ++ "\n  withdrawals "
          ++ show (fold . unWithdrawals $ txb ^. withdrawalsTxBodyL)
          ++ show producedValue
          ++ show consumedValue
      )

-- ========================

showMap :: (t1 -> [Char]) -> (t2 -> [Char]) -> Map.Map t1 t2 -> String
showMap showKey showVal m = unlines (map showpair (Map.toList m))
  where
    showpair (key, val) = showKey key ++ " -> " ++ showVal val

showListy :: Foldable t => (a -> String) -> t a -> String
showListy showElem list = unlines (map showElem (toList list))

showRewardAcct :: RewardAccount -> [Char]
showRewardAcct (RewardAccount {raNetwork = network, raCredential = cred}) =
  show network ++ " " ++ showCred cred

showWithdrawal :: Withdrawals -> String
showWithdrawal (Withdrawals m) = showMap (("   " ++) . showRewardAcct) show m

showIR :: InstantaneousRewards -> String
showIR (InstantaneousRewards m n x y) =
  unlines
    [ "IRReseves " ++ showMap (("   " ++) . take 10 . showCred) show m
    , "IRTreasury " ++ showMap (("   " ++) . take 10 . showCred) show n
    , "DeltaReserves " ++ show x
    , "DeltaTreasury " ++ show y
    ]

showSafeHash :: SafeHash i -> String
showSafeHash x = take 12 (show (extractHash x))
