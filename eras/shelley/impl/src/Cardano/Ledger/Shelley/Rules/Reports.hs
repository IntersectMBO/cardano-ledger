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
)
where

import Cardano.Ledger.CertState (
  CertState (..),
  InstantaneousRewards (..),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Foldable (fold, toList)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))

-- ===============================================
-- Reporting Certificates

showCred :: Credential x c -> String
showCred (ScriptHashObj (ScriptHash x)) = show x
showCred (KeyHashObj (KeyHash x)) = show x

showKeyHash :: KeyHash c x -> String
showKeyHash (KeyHash hash) = take 10 (show hash)

showCerts :: Show (TxCert era) => [TxCert era] -> String
showCerts certs = unlines (map (("  " ++) . show) certs)

showTxCerts :: EraTxBody era => TxBody era -> String
showTxCerts txb = showCerts (toList (txb ^. certsTxBodyL))

-- | Display a synopsis of a map to Coin
synopsisCoinMap :: Maybe (Map.Map k Coin) -> String
synopsisCoinMap (Just m) = "Count = " ++ show (Map.size m) ++ ",  total = " ++ show (fold m)
synopsisCoinMap Nothing = "SYNOPSIS NOTHING"

-- ===============================================
-- Printing Produced == Consumed

produceEqualsConsumed ::
  EraTxBody era =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
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

showRewardAcct :: RewardAccount c -> [Char]
showRewardAcct (RewardAccount {raNetwork = network, raCredential = cred}) =
  show network ++ " " ++ showCred cred

showWithdrawal :: Withdrawals c -> String
showWithdrawal (Withdrawals m) = showMap (("   " ++) . showRewardAcct) show m

showIR :: InstantaneousRewards c -> String
showIR (InstantaneousRewards m n x y) =
  unlines
    [ "IRReseves " ++ showMap (("   " ++) . take 10 . showCred) show m
    , "IRTreasury " ++ showMap (("   " ++) . take 10 . showCred) show n
    , "DeltaReserves " ++ show x
    , "DeltaTreasury " ++ show y
    ]

showSafeHash :: SafeHash c i -> String
showSafeHash x = take 12 (show (extractHash x))
