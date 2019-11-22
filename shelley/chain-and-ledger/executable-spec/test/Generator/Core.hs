{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generator.Core
  ( findPayKeyPair
  , genBool
  , genCoin
  , genInteger
  , genNatural
  , genTxOut
  , genUtxo0
  , increasingProbabilityAt
  , mkGenesisLedgerState
  , traceKeyPairs
  , traceVRFKeyPairs
  , someKeyPairs
  , pickStakeKey
  , toAddr
  , toCred)
  where

import           Cardano.Crypto.VRF (deriveVerKeyVRF, genKeyVRF)
import           Crypto.Random (drgNewTest, withDRG)
import           Data.Tuple (swap)
import           Data.Word (Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Address (toAddr, toCred)
import           Coin (Coin (..))
import           Keys (pattern KeyPair, hashKey, vKey)
import           LedgerState (pattern LedgerState, genesisCoins, genesisState)
import           MockTypes (Addr, DPState, KeyPair, KeyPairs, LedgerEnv, SignKeyVRF, TxOut, UTxO,
                     UTxOState, VKey, VerKeyVRF)
import           Numeric.Natural (Natural)
import           Test.Utils (mkKeyPair)
import           Tx (pattern TxOut)
import           TxData (pattern AddrBase, pattern KeyHashObj)

genBool :: Gen Bool
genBool = Gen.enumBounded

genInteger :: Integer -> Integer -> Gen Integer
genInteger lower upper = Gen.integral $ Range.linear lower upper

-- | Generator for a natural number between 'lower' and 'upper'
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = Gen.integral $ Range.linear lower upper

mkKeyPairs :: Word64 -> (KeyPair, KeyPair)
mkKeyPairs n
  = (mkKeyPair_ (2*n), mkKeyPair_ (2*n+1))
  where
    mkKeyPair_ n_ = (uncurry KeyPair . swap) (mkKeyPair (n_,n_,n_,n_,n_))

-- | Constant list of KeyPairs intended to be used in the generators.
traceKeyPairs :: KeyPairs
traceKeyPairs = mkKeyPairs <$> [1 .. 150]

-- | Select between _lower_ and _upper_ keys from 'traceKeyPairs'
someKeyPairs :: Int -> Int -> Gen KeyPairs
someKeyPairs lower upper =
  take
    <$> Gen.integral (Range.linear lower upper)
    <*> Gen.shuffle traceKeyPairs

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPair :: Addr -> KeyPairs -> KeyPair
findPayKeyPair (AddrBase (KeyHashObj addr) _) keyList =
    case matches of
      []    -> error "findPayKeyPair: could not find a match for the given address"
      (x:_) -> fst x
    where
      matches = filter (\(pay, _) -> addr == hashKey (vKey pay)) keyList
findPayKeyPair _ _ = error "findPayKeyPair: expects only AddrBase addresses"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs -> Gen VKey
pickStakeKey keys = vKey . snd <$> Gen.element keys

-- | Generates a list of coins for the given 'Addr' and produced a 'TxOut' for each 'Addr'
genTxOut :: [Addr] -> Gen [TxOut]
genTxOut addrs = do
  ys <- genCoinList 100 10000 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- | Generates a list of 'Coin' values of length between 'lower' and 'upper'
-- and with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper =
  Gen.list (Range.linear lower upper) $ genCoin minCoin maxCoin

genCoin :: Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> Gen.integral (Range.exponential minCoin maxCoin)

genUtxo0 :: Int -> Int -> Gen UTxO
genUtxo0 lower upper = do
  genesisKeys <- someKeyPairs lower upper
  outs <- genTxOut (fmap toAddr genesisKeys)
  return (genesisCoins outs)

mkGenesisLedgerState
  :: LedgerEnv
  -> Gen (UTxOState, DPState)
mkGenesisLedgerState _ = do
  utxo0 <- genUtxo0 5 10
  let (LedgerState utxoSt dpSt _) = genesisState utxo0
  pure (utxoSt, dpSt)

-- | Generate values the given distribution in 90% of the cases, and values at
-- the bounds of the range in 10% of the cases.
--
-- This can be used to generate enough extreme values. The exponential and
-- linear distributions provided by @hedgehog@ will generate a small percentage
-- of these (0-1%).
increasingProbabilityAt
  :: Gen a
  -> (a, a)
  -> Gen a
increasingProbabilityAt gen (lower, upper)
  = Gen.frequency [ (5, pure lower)
                  , (90, gen)
                  , (5, pure upper)
                  ]

-- | A pre-populated space of VRF keys for use in the generators.
traceVRFKeyPairs :: [(SignKeyVRF, VerKeyVRF)]
traceVRFKeyPairs = [body (0,0,0,0,i) | i <- [1 .. 50]]
 where
  body seed = fst . withDRG (drgNewTest seed) $ do
    sk <- genKeyVRF
    return (sk, deriveVerKeyVRF sk)
