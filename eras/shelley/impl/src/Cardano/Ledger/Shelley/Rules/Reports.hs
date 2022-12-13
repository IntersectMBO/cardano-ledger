{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tools for reporting things in readable manner. Used in Rules to implement
--   STS 'renderAssertionViolation' methods, and in Tests.
module Cardano.Ledger.Shelley.Rules.Reports
  ( showCred,
    showIR,
    showKeyHash,
    showListy,
    showMap,
    showWithdrawal,
    showSafeHash,
    synopsisCert,
    synopsisCoinMap,
    trim,
    showTxCerts,
    produceEqualsConsumed,
  )
where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), EraCrypto)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    InstantaneousRewards (..),
  )
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Foldable (fold, toList)
import qualified Data.Map.Strict as Map
import GHC.Records (HasField (..))
import Lens.Micro

-- ===============================================
-- Reporting Certificates

trim :: Int -> String -> String
trim n s = take n s

showCred :: Credential x c -> String
showCred (ScriptHashObj (ScriptHash x)) = show x
showCred (KeyHashObj (KeyHash x)) = show x

synopsisCert :: DCert c -> String
synopsisCert x = case x of
  DCertDeleg (RegKey cred) -> "RegKey " ++ trim 10 (showCred cred)
  DCertDeleg (DeRegKey cred) -> "DeRegKey " ++ trim 10 (showCred cred)
  DCertDeleg (Delegate _) -> "Delegation"
  DCertPool (RegPool pool) -> let KeyHash hash = ppId pool in "RegPool " ++ trim 10 (show hash)
  DCertPool (RetirePool khash e) -> "RetirePool " ++ showKeyHash khash ++ " " ++ show e
  DCertGenesis _ -> "GenesisCert"
  DCertMir _ -> "MirCert"

showKeyHash :: KeyHash c x -> String
showKeyHash (KeyHash hash) = trim 10 (show hash)

showCerts :: [DCert c] -> String
showCerts certs = unlines (map (("  " ++) . synopsisCert) certs)

showTxCerts :: ShelleyEraTxBody era => Core.TxBody era -> String
showTxCerts txb = showCerts (toList (txb ^. certsTxBodyG))

-- | Display a synopsis of a map to Coin
synopsisCoinMap :: Maybe (Map.Map k Coin) -> String
synopsisCoinMap (Just m) = "Count = " ++ show (Map.size m) ++ ",  total = " ++ show (fold m)
synopsisCoinMap Nothing = "SYNOPSIS NOTHING"

-- ===============================================
-- Printing Produced == Consumed

produceEqualsConsumed ::
  ( ShelleyEraTxBody era,
    HasField "_poolDeposit" pp Coin,
    HasField "_keyDeposit" pp Coin
  ) =>
  pp ->
  DPState (EraCrypto era) ->
  UTxO era ->
  Core.TxBody era ->
  String
produceEqualsConsumed pp dpstate utxo txb =
  let consumedValue = consumedTxBody txb pp dpstate utxo
      producedValue = producedTxBody txb pp dpstate
   in ( "\n (Produce = Consumed) Report\n  certificates\n"
          ++ showTxCerts txb
          ++ "\n  withdrawals "
          ++ show (fold . unWdrl $ txb ^. wdrlsTxBodyL)
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

showRewardAcct :: RewardAcnt c -> [Char]
showRewardAcct (RewardAcnt {getRwdNetwork = network, getRwdCred = cred}) =
  show network ++ " " ++ (showCred cred)

showWithdrawal :: Wdrl c -> String
showWithdrawal (Wdrl m) = showMap (("   " ++) . showRewardAcct) show m

showIR :: InstantaneousRewards c -> String
showIR (InstantaneousRewards m n x y) =
  unlines
    [ "IRReseves " ++ showMap (("   " ++) . trim 10 . showCred) show m,
      "IRTreasury " ++ showMap (("   " ++) . trim 10 . showCred) show n,
      "DeltaReserves " ++ show x,
      "DeltaTreasury " ++ show y
    ]

showSafeHash :: SafeHash c i -> String
showSafeHash x = trim 12 (show (extractHash x))
