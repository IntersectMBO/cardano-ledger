{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import BenchUTxOAggregate (expr, genTestCase)
import BenchValidation
  ( applyBlock,
    benchValidate,
    benchreValidate,
    genUpdateInputs,
    sizes,
    updateAndTickChain,
    updateChain,
    validateInput,
  )
-- How to precompute env for the UTxO transactions

-- How to precompute env for the Stake Delegation transactions
-- How to precompute env for the StakeKey transactions
-- How to compute an initial state with N StakePools

import Cardano.Ledger.Era (Crypto (..))
import Cardano.Slotting.Slot (EpochSize (..))
import Control.DeepSeq (NFData)
import Control.Iterate.SetAlgebra (dom, forwards, keysEqual, (▷), (◁))
import Control.Iterate.SetAlgebraInternal (compile, compute, run)
import Criterion.Main
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    env,
    nf,
    nfIO,
    whnf,
    whnfIO,
  )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Shelley.Spec.Ledger.Bench.Gen
  ( genBlock,
    genChainState,
    genTx,
  )
import Shelley.Spec.Ledger.Bench.Rewards (createRUpd, genChainInEpoch)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.EpochBoundary (SnapShot (..))
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import Shelley.Spec.Ledger.Keys (KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    PState (..),
    UTxOState (..),
    stakeDistr,
  )
import Shelley.Spec.Ledger.Rewards (likelihood)
import Shelley.Spec.Ledger.UTxO (UTxO)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck.Gen as QC
import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( initUTxO,
    ledgerDeRegisterStakeKeys,
    ledgerDelegateManyKeysOnePool,
    ledgerReRegisterStakePools,
    ledgerRegisterStakeKeys,
    ledgerRegisterStakePools,
    ledgerRetireStakePools,
    ledgerRewardWithdrawals,
    ledgerSpendOneGivenUTxO,
    ledgerSpendOneUTxO,
    ledgerStateWithNkeysMpools,
    ledgerStateWithNregisteredKeys,
    ledgerStateWithNregisteredPools,
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

-- ==========================================================

eqf ::
  String ->
  (Map.Map Int Int -> Map.Map Int Int -> Bool) ->
  Int ->
  Benchmark
eqf name f n =
  bgroup
    (name ++ " " ++ show n)
    (map runat [n, n * 10, n * 100, n * 1000])
  where
    runat m =
      env
        ( return $
            Map.fromList
              [ (k, k)
                | k <- [1 .. m]
              ]
        )
        (\state -> bench (show m) (whnf (f state) state))

mainEq :: IO ()
mainEq =
  defaultMain $
    [ bgroup "KeysEqual tests" $
        [ eqf "keysEqual" keysEqual (100 :: Int),
          eqf
            "keys x == keys y"
            (\x y -> Map.keys x == Map.keys y)
            (100 :: Int)
        ]
    ]

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

touchDPState :: DPState era -> Int
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

-- ============================================
-- Profiling N keys and M pools

profileNkeysMPools :: IO ()
profileNkeysMPools = do
  putStrLn "Enter N keys and M Pools"
  let unit =
        ledgerDelegateManyKeysOnePool
          50
          500
          (ledgerStateWithNkeysMpools 5000 500)
  putStrLn ("Exit profiling " ++ show unit)

-- ==========================================
-- Registering Pools

profileCreateRegPools :: Word64 -> IO ()
profileCreateRegPools size = do
  putStrLn "Enter profiling pool creation"
  let state = ledgerStateWithNregisteredPools 1 size
  let touch (x, y) = touchUTxOState x + touchDPState y
  putStrLn ("Exit profiling " ++ show (touch state))

-- ==========================================
-- Epoch Boundary

profileEpochBoundary :: IO ()
profileEpochBoundary =
  defaultMain $
    [ bgroup "aggregate stake" $
        epochAt <$> benchParameters
    ]
  where
    benchParameters :: [Int]
    benchParameters = [10000, 100000, 1000000]

epochAt :: Int -> Benchmark
epochAt x =
  env (QC.generate (genTestCase x (10000 :: Int))) $
    \arg ->
      bgroup
        ("UTxO=" ++ show x ++ ",  address=" ++ show (10000 :: Int))
        [ bench "Using maps" (whnf action2m arg)
        ]

action2m :: Era era => (DState era, PState era, UTxO era) -> SnapShot era
action2m (dstate, pstate, utxo) = stakeDistr utxo dstate pstate

dstate' :: DState C
pstate' :: PState C
utxo' :: UTxO C
(dstate', pstate', utxo') =
  unsafePerformIO $
    QC.generate (genTestCase 1000000 (5000 :: Int))

profile_Maps :: Int -> IO ()
profile_Maps _x = do
  let snap = stakeDistr utxo' dstate' pstate'
  putStrLn
    ( "Size = " ++ show (Map.size (EB._delegations snap)) ++ " "
        ++ show (Map.size (_poolParams snap))
    )

{- At least while running in GHCI Maps use less allocation than lists
*Main> profile_Lists 1
Size = 122 61
(0.24 secs, 630,016,752 bytes)

*Main> profile_Maps 1
Size = 122 61
(0.23 secs, 519,132,520 bytes)

Compiled, Maps also seem to be a little bit faster. Maps win

benchmarking aggregate stake/UTxO=1000000,  address=10000/Using lists
time                 292.9 ms   (269.6 ms .. 316.3 ms)

benchmarking aggregate stake/UTxO=1000000,  address=10000/Using maps
time                 280.6 ms   (256.0 ms .. 297.3 ms)

-}

-- =================================================================

-- | Benchmarks for the various validation transitions exposed by the API
validGroup :: Benchmark
validGroup =
  bgroup "validation" $
    [ runAtUTxOSize 1000,
      runAtUTxOSize 100000,
      runAtUTxOSize 1000000
    ]
  where
    runAtUTxOSize n =
      bgroup (show n) $
        [ env (validateInput n) $ \arg ->
            bgroup
              "block"
              [ bench "applyBlockTransition" (nfIO $ benchValidate arg),
                bench "reapplyBlockTransition" (nf benchreValidate arg)
              ],
          env (genUpdateInputs n) $ \arg ->
            bgroup
              "protocol"
              [ bench "updateChainDepState" (nf updateChain arg),
                bench
                  "updateAndTickChainDepState"
                  (nf updateAndTickChain arg)
              ]
        ]

profileValid :: IO ()
profileValid = do
  state <- validateInput 10000
  let ans = sum [applyBlock state n | n <- [1 .. 10000 :: Int]]
  putStrLn (show ans)
  pure ()

-- ========================================================
-- Profile algorithms for  ((dom d ◁ r) ▷ dom rg)

domainRangeRestrict :: IO ()
domainRangeRestrict =
  defaultMain $
    [ bgroup "domain-range restict" $
        drrAt <$> benchParameters
    ]
  where
    benchParameters :: [Int]
    benchParameters = [1000, 10000, 100000]

drrAt :: Int -> Benchmark
drrAt x =
  env (expr x) $
    \arg ->
      bgroup
        ("size=" ++ show x)
        [ bench "compute" (whnf alg1 arg),
          bench "run . compile" (whnf alg2 arg)
        ]

alg1 :: (Map Int Int, Map Int Char, Map Char Char) -> Map Int Char
alg1 (d, r, rg) = compute ((dom d ◁ r) ▷ dom rg)

alg2 :: (Map Int Int, Map Int Char, Map Char Char) -> Map Int Char
alg2 (d, r, rg) = run $ compile ((dom d ◁ r) ▷ dom rg)

-- =================================================
-- Some things we might want to profile.

-- main :: IO()
-- main = profileUTxO
-- main = includes_init_SpendOneUTxO
-- main:: IO ()
-- main = profileCreateRegPools 10000
-- main = profileCreateRegPools 100000
-- main = profileNkeysMPools
-- main = profile_stakeDistr
-- main = profileEpochBoundary

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
-- main=profileValid
main =
  defaultMain $
    [ bgroup "vary input size" $
        [ varyInput
            "deregister key"
            (1, 5000)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNregisteredKeys
            ledgerDeRegisterStakeKeys,
          varyInput
            "register key"
            (20001, 25001)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNregisteredKeys
            ledgerRegisterStakeKeys,
          varyInput
            "withdrawal"
            (1, 5000)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNregisteredKeys
            ledgerRewardWithdrawals,
          varyInput
            "register pool"
            (1, 5000)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNregisteredPools
            ledgerRegisterStakePools,
          varyInput
            "reregister pool"
            (1, 5000)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNregisteredPools
            ledgerReRegisterStakePools,
          varyInput
            "retire pool"
            (1, 5000)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNregisteredPools
            ledgerRetireStakePools,
          varyInput
            "manyKeysOnePool"
            (5000, 5000)
            [(1, 50), (1, 500), (1, 5000)]
            ledgerStateWithNkeysMpools
            ledgerDelegateManyKeysOnePool
        ],
      bgroup "vary initial state" $
        [ varyState
            "spendOne"
            1
            [50, 500, 5000]
            (\_m n -> initUTxO (fromIntegral n))
            (\_m _ -> ledgerSpendOneGivenUTxO),
          varyState
            "register key"
            5001
            [50, 500, 5000]
            ledgerStateWithNregisteredKeys
            ledgerRegisterStakeKeys,
          varyState
            "deregister key"
            50
            [50, 500, 5000]
            ledgerStateWithNregisteredKeys
            ledgerDeRegisterStakeKeys,
          varyState
            "withdrawal"
            50
            [50, 500, 5000]
            ledgerStateWithNregisteredKeys
            ledgerRewardWithdrawals,
          varyState
            "register pool"
            5001
            [50, 500, 5000]
            ledgerStateWithNregisteredPools
            ledgerRegisterStakePools,
          varyState
            "reregister pool"
            5001
            [50, 500, 5000]
            ledgerStateWithNregisteredPools
            ledgerReRegisterStakePools,
          varyState
            "retire pool"
            50
            [50, 500, 5000]
            ledgerStateWithNregisteredPools
            ledgerRetireStakePools,
          varyDelegState
            "manyKeysOnePool"
            50
            [50, 500, 5000]
            ledgerStateWithNkeysMpools
            ledgerDelegateManyKeysOnePool
        ],
      bgroup "vary utxo at epoch boundary" $
        (epochAt <$> [5000, 50000, 500000]),
      bgroup "domain-range restict" $ drrAt <$> [10000, 100000, 1000000],
      validGroup,
      -- Benchmarks for the various generators
      bgroup "gen" $
        [ env
            (genChainState 100000)
            ( \cs ->
                bgroup
                  "block"
                  [ bench "genBlock" $ whnfIO $ genBlock cs
                  ]
            ),
          bgroup
            "genTx"
            [ bench "1000" $ whnfIO $ genTx 1000
            ]
        ],
      bgroup "rewards" $
        [ env
            (generate $ genChainInEpoch 5)
            ( \cs ->
                bench "createRUpd" $ whnf (createRUpd testGlobals) cs
            ),
          bench "likelihood" $ whnf (likelihood 1234 0.1) (EpochSize 10000)
        ]
    ]
