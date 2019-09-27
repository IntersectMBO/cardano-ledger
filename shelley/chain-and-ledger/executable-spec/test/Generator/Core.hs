{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generator.Core
  ( findPayKeyPair
  , genTxOut
  , genUtxo0
  , mkGenesisLedgerState
  , traceKeyPairs
  , someKeyPairs
  , pickStakeKey
  , toAddr
  , toCred)
  where

import           Data.Tuple (swap)
import           Data.Word (Word64)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Address (toAddr, toCred)
import           Coin (Coin (..))
import           Examples (mkKeyPair)
import           Keys (pattern KeyPair, hashKey, vKey)
import           LedgerState (pattern LedgerState, genesisCoins, genesisState)
import           MockTypes (Addr, DPState, KeyPair, KeyPairs, LedgerEnv, TxOut, UTxO, UTxOState,
                     VKey)
import           Tx (pattern TxOut)
import           TxData (pattern AddrBase, pattern KeyHashObj)

mkKeyPairs :: Word64 -> (KeyPair, KeyPair)
mkKeyPairs n
  = (mkKeyPair_ (2*n), mkKeyPair_ (2*n+1))
  where
    mkKeyPair_ n_ = (uncurry KeyPair . swap) (mkKeyPair (n_,n_,n_,n_,n_))

-- | Constant list of KeyPairs intended to be used in the generators.
traceKeyPairs :: KeyPairs
traceKeyPairs = mkKeyPairs <$> [1 .. 50]

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
genCoinList minCoin maxCoin lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.exponential minCoin maxCoin)
  return (Coin <$> xs)

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
