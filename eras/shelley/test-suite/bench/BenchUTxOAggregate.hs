{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BenchUTxOAggregate where

import Cardano.Ledger.Address (Addr (..), compactAddr)
import Cardano.Ledger.BaseTypes (mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (toCompact)
import Cardano.Ledger.Core (Era (..))
import Cardano.Ledger.Credential (
  Credential (..),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (GenDelegs (..), KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (
  DState (..),
  InstantaneousRewards (..),
  PState (..),
 )
import Cardano.Ledger.Shelley.Scripts ()
import Cardano.Ledger.Shelley.TxBody (
  PoolParams (..),
  ShelleyTxOut (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (
  UTxO (..),
 )
import qualified Cardano.Ledger.Val as Val
import Control.Iterate.SetAlgebra (compile, compute, run)
import Control.Monad (replicateM)
import Control.SetAlgebra (dom, (▷), (◁))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Word (Word16)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Examples.Cast (alicePoolParams)
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck

genTestCase ::
  Int -> -- The size of the utxo
  Int -> -- the number of addresses
  Gen (DState (ShelleyEra StandardCrypto), PState (ShelleyEra StandardCrypto), UTxO (ShelleyEra StandardCrypto))
genTestCase numUTxO numAddr = do
  addrs :: [Addr StandardCrypto] <- replicateM numAddr arbitrary
  let packedAddrs = Seq.fromList addrs
  txOuts <- replicateM numUTxO $ do
    i <- choose (0, numAddr - 1)
    let addr = Seq.index packedAddrs i
    pure $
      TxOutCompact
        (compactAddr addr)
        (fromJust $ toCompact $ Val.inject (Coin $ fromIntegral i))
  let mktxid i = TxId (unsafeMakeSafeHash (mkDummyHash (i :: Int)))
  let mktxin i = TxIn (mktxid i) (mkTxIxPartial (toInteger (fromIntegral i :: Word16)))
  let utxo = Map.fromList $ zip (mktxin <$> [1 ..]) txOuts
      liveptrs :: [Ptr]
      liveptrs = [p | ShelleyTxOut (Addr _ _ (StakeRefPtr p)) _ <- txOuts]
      m = length liveptrs `div` 2
  moreptrs :: [Ptr] <- replicateM m arbitrary
  creds :: [Credential 'Staking StandardCrypto] <- replicateM (m + m) arbitrary
  let ptrs' :: Map Ptr (Credential 'Staking StandardCrypto)
      ptrs' = Map.fromList (zip (liveptrs ++ moreptrs) creds)
  rewards :: [(Credential 'Staking StandardCrypto, Coin)] <- replicateM (3 * (numUTxO `div` 4)) arbitrary
  let rewards' :: Map (Credential 'Staking StandardCrypto) Coin
      rewards' = Map.fromList rewards

  keyhash :: [KeyHash 'StakePool StandardCrypto] <- replicateM 400 arbitrary
  let delegs = Map.fromList (zip creds (cycle (take 200 keyhash)))
  let pp = alicePoolParams
  let poolParams = Map.fromList (zip keyhash (replicate 400 pp))
  let (dstate, pstate) = makeStatePair rewards' delegs ptrs' poolParams
  pure (dstate, pstate, UTxO utxo)

makeStatePair ::
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Map Ptr (Credential 'Staking (EraCrypto era)) ->
  Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
  (DState era, PState era)
makeStatePair rewards' delegs ptrs' poolParams =
  ( DState
      (UM.unify (Map.map rdPair rewards') delegs ptrs')
      Map.empty
      (GenDelegs Map.empty)
      (InstantaneousRewards Map.empty Map.empty mempty mempty)
  , PState poolParams Map.empty Map.empty Map.empty
  )

rdPair :: Coin -> UM.RDPair
rdPair coin = UM.RDPair (UM.compactCoinOrError coin) (UM.CompactCoin 1)

-- ====================================================================================
-- operations to benchmark different algorithms for executing ((dom d ◁ r) ▷ dom rg)

big :: Int -> Gen Int
big n = choose (1, n)

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
