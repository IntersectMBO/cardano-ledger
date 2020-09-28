{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module BenchUTxOAggregate where

import Cardano.Ledger.Core (toCompact)
import qualified Cardano.Ledger.Val as Val
import Control.Iterate.SetAlgebra (Bimap, biMapFromList, dom, (▷), (◁))
import Control.Iterate.SetAlgebraInternal (compile, compute, run)
import qualified Data.ByteString.Short as SBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    serialiseAddr,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Keys (GenDelegs (..), KeyHash (..), KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( DState (..),
    InstantaneousRewards (..),
    PState (..),
  )
import Shelley.Spec.Ledger.TxBody
  ( PoolParams (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
  )
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Examples.Cast (alicePoolParams)
import Test.Shelley.Spec.Ledger.Serialisation.Generators (mkDummyHash)

genTestCase ::
  Int -> -- The size of the utxo
  Int -> -- the number of addresses
  Gen (DState C, PState C, UTxO C)
genTestCase numUTxO numAddr = do
  addrs <- (sequence $ replicate numAddr arbitrary) :: Gen [Addr C]
  let packedAddrs = Seq.fromList addrs
  txOuts <- sequence $
    replicate numUTxO $ do
      i <- choose (0, numAddr -1)
      let addr = Seq.index packedAddrs i
      pure $ TxOutCompact (SBS.toShort $ serialiseAddr addr) (toCompact $ Val.inject (Coin $ fromIntegral i))
  let mktxid i = TxId $ mkDummyHash i
  let mktxin i = TxIn (mktxid i) (fromIntegral i)
  let utxo = Map.fromList $ zip (mktxin <$> [1 ..]) txOuts
      liveptrs :: [Ptr]
      liveptrs = [p | (TxOut (Addr _ _ (StakeRefPtr p)) _) <- txOuts]
      m = length liveptrs `div` 2
  moreptrs <- (sequence $ replicate m arbitrary) :: Gen [Ptr]
  creds <- (sequence $ replicate (m + m) arbitrary) :: Gen [Credential 'Staking C]
  let ptrs' :: Bimap Ptr (Credential 'Staking C)
      ptrs' = biMapFromList (\new _old -> new) (zip (liveptrs ++ moreptrs) creds)
  rewards <- sequence (replicate (3 * (numUTxO `div` 4)) arbitrary) :: Gen [(Credential 'Staking C, Coin)]
  let rewards' :: Map (Credential 'Staking C) Coin
      rewards' = Map.fromList rewards

  keyhash <- sequence (replicate 400 arbitrary) :: Gen [KeyHash 'StakePool C]
  let delegs = Map.fromList (zip creds (cycle (take 200 keyhash)))
  let pp = alicePoolParams
  let poolParams = Map.fromList (zip keyhash (replicate 400 pp))
  let (dstate, pstate) = makeStatePair rewards' delegs ptrs' poolParams
  pure (dstate, pstate, UTxO utxo)

makeStatePair ::
  Map (Credential 'Staking era) Coin ->
  Map (Credential 'Staking era) (KeyHash 'StakePool era) ->
  Bimap Ptr (Credential 'Staking era) ->
  Map (KeyHash 'StakePool era) (PoolParams era) ->
  (DState era, PState era)
makeStatePair rewards' delegs ptrs' poolParams =
  ( DState rewards' delegs ptrs' Map.empty (GenDelegs Map.empty) (InstantaneousRewards Map.empty Map.empty),
    PState poolParams Map.empty Map.empty
  )

-- ====================================================================================
-- operations to benchmark different algorithms for executing ((dom d ◁ r) ▷ dom rg)

big :: Int -> Gen Int
big n = (choose (1, n))

pair :: Gen a -> Gen b -> Gen (a, b)
pair g1 g2 = do x <- g1; y <- g2; pure (x, y)

char :: Gen Char
char = arbitrary

list :: Int -> Gen a -> Gen [a]
list m g = sequence [g | _x <- [1 .. m]]

expr :: Int -> IO (Map Int Int, Map Int Char, Map Char Char)
expr n = do d <- domain; r <- rel; rg <- range; pure (d, r, rg)
  where
    rel :: IO (Map Int Char)
    rel = fmap Map.fromList $ generate $ list n (pair (big 1000000) char)
    domain :: IO (Map Int Int)
    domain = fmap Map.fromList $ generate $ list (div n 7) (pair (big 1000000) (big 10))
    range :: IO (Map Char Char)
    range = fmap Map.fromList $ generate $ list (max 10 (div n 2000)) (pair char char)

test23 :: Map Int Int -> Map Int Char -> Map Char Char -> Bool
test23 domain relation range = compute exp2 == (run (compile exp2))
  where
    exp2 = ((dom domain ◁ relation) ▷ dom range)

-- quickCheck test23
