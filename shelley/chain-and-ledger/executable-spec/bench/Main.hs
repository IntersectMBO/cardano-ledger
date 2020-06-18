module Main where

import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, whnf)
import Data.Word (Word64)
import Shelley.Spec.Ledger.LedgerState (DPState (..), UTxOState (..))
import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( initUTxO, -- How to precompute env for the UTxO transactions
    ledgerDeRegisterStakeKeys,
    ledgerDelegateManyKeysOnePool,
    ledgerReRegisterStakePools,
    ledgerRegisterStakeKeys,
    ledgerRegisterStakePools,
    ledgerRetireStakePools,
    ledgerRewardWithdrawals,
    ledgerSpendOneGivenUTxO,
    ledgerSpendOneUTxO,
    ledgerStateWithNkeysMpools, -- How to precompute env for the Stake Delegation transactions
    ledgerStateWithNregisteredKeys, -- How to precompute env for the StakeKey transactions
    ledgerStateWithNregisteredPools, -- How to compute an initial state with N StakePools
  )

-- =================================================
-- Spending 1 UTxO

includes_init_SpendOneUTxO :: IO ()
includes_init_SpendOneUTxO =
  defaultMain
    [ bgroup "Spend 1 UTXO with initialization" $
        fmap
          (\n -> bench (show n) $ whnf ledgerSpendOneUTxO n)
          [50, 500, 5000, 50000]
    ]

profileUTxO :: IO ()
profileUTxO = do
  putStrLn "Enter profiling"
  let ans = ledgerSpendOneGivenUTxO (initUTxO 500000)
  putStrLn ("Exit profiling " ++ show ans)

-- ==========================================
-- Registering Stake Keys

touchDPState :: DPState crypto -> Int
touchDPState (DPState _x _y) = 1

touchUTxOState :: Shelley.Spec.Ledger.LedgerState.UTxOState cryto -> Int
touchUTxOState (UTxOState _utxo _deposited _fees _ppups) = 2

profileCreateRegKeys :: IO ()
profileCreateRegKeys = do
  putStrLn "Enter profiling stake key creation"
  let state = ledgerStateWithNregisteredKeys 1 500000 -- using 75,000 and 100,000 causes
  -- mainbench: internal error: PAP object entered!
  -- (GHC version 8.6.5 for x86_64_unknown_linux)
  -- Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
  let touch (x, y) = touchUTxOState x + touchDPState y
  putStrLn ("Exit profiling " ++ show (touch state))

-- ==========================================
-- Registering Pools

profileCreateRegPools :: Word64 -> IO ()
profileCreateRegPools size = do
  putStrLn "Enter profiling pool creation"
  let state = ledgerStateWithNregisteredPools 1 size
  let touch (x, y) = touchUTxOState x + touchDPState y
  putStrLn ("Exit profiling " ++ show (touch state))

-- =================================================
-- Some things we might want to profile.

-- main = profileUTxO
-- main = includes_init_SpendOneUTxO
-- main = profileCreateRegPools 10000
-- main = profileCreateRegPools 100000

-- =========================================================

varyState ::
  NFData state =>
  String ->
  Word64 ->
  [Word64] ->
  (Word64 -> Word64 -> state) ->
  (Word64 -> Word64 -> state -> ()) ->
  Benchmark
varyState tag fixed changes initstate action =
  bgroup ("state/" ++ tag ++ "/constant") $ map runAtSize changes
  where
    runAtSize n =
      env
        (return $ initstate 1 n)
        (\state -> bench (show n) (whnf (action fixed fixed) state))

varyInput ::
  NFData state =>
  String ->
  (Word64, Word64) ->
  [(Word64, Word64)] ->
  (Word64 -> Word64 -> state) ->
  (Word64 -> Word64 -> state -> ()) ->
  Benchmark
varyInput tag fixed changes initstate action =
  bgroup ("input/" ++ tag ++ "/growing") $ map runAtSize changes
  where
    runAtSize n =
      env
        (return $ initstate (fst fixed) (snd fixed))
        (\state -> bench (show n) (whnf (action (fst n) (snd n)) state))

varyDelegState ::
  NFData state =>
  String ->
  Word64 ->
  [Word64] ->
  (Word64 -> Word64 -> state) ->
  (Word64 -> Word64 -> state -> ()) ->
  Benchmark
varyDelegState tag fixed changes initstate action =
  bgroup ("state/" ++ tag ++ "/growing") $ map runAtSize changes
  where
    runAtSize n =
      env
        (return $ initstate n n)
        (\state -> bench (show n) (whnf (action 1 fixed) state))

-- =============================================================================

main :: IO ()
main =
  defaultMain $
    [ bgroup "vary input size" $
        [ varyInput "deregister key" (1, 5000) [(1, 50), (1, 500), (1, 5000)] ledgerStateWithNregisteredKeys ledgerDeRegisterStakeKeys,
          varyInput "register key" (20001, 20501) [(1, 20), (1, 200), (1, 2000)] ledgerStateWithNregisteredKeys ledgerRegisterStakeKeys,
          varyInput "withdrawal" (1, 5000) [(1, 50), (1, 500), (1, 5000)] ledgerStateWithNregisteredKeys ledgerRewardWithdrawals,
          varyInput "register pool" (1, 5000) [(1, 50), (1, 500), (1, 5000)] ledgerStateWithNregisteredPools ledgerRegisterStakePools,
          varyInput "reregister pool" (1, 5000) [(1, 50), (1, 500), (1, 5000)] ledgerStateWithNregisteredPools ledgerReRegisterStakePools,
          varyInput "retire pool" (1, 5000) [(1, 50), (1, 500), (1, 5000)] ledgerStateWithNregisteredPools ledgerRetireStakePools,
          varyInput "manyKeysOnePool" (5000, 5000) [(1, 50), (1, 500), (1, 5000)] ledgerStateWithNkeysMpools ledgerDelegateManyKeysOnePool
        ],
      bgroup "vary initial state" $
        [ varyState "spendOne" 1 [50, 500, 5000] (\_ n -> initUTxO (fromIntegral n)) (\_ _ -> ledgerSpendOneGivenUTxO),
          varyState "register key" 5001 [50, 500, 5000] ledgerStateWithNregisteredKeys ledgerRegisterStakeKeys,
          varyState "deregister key" 50 [50, 500, 5000] ledgerStateWithNregisteredKeys ledgerDeRegisterStakeKeys,
          varyState "withdrawal" 50 [50, 500, 5000] ledgerStateWithNregisteredKeys ledgerRewardWithdrawals,
          varyState "register pool" 5001 [50, 500, 5000] ledgerStateWithNregisteredPools ledgerRegisterStakePools,
          varyState "reregister pool" 5001 [50, 500, 5000] ledgerStateWithNregisteredPools ledgerReRegisterStakePools,
          varyState "retire pool" 50 [50, 500, 5000] ledgerStateWithNregisteredPools ledgerRetireStakePools,
          varyDelegState "manyKeysOnePool" 50 [50, 500, 5000] ledgerStateWithNkeysMpools ledgerDelegateManyKeysOnePool
        ]
    ]
