{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generator.Core.QuickCheck
  ( findPayKeyPair
  , findPayScript
  , genBool
  , genCoin
  , genCoinList
  , genInteger
  , genNatural
  , genWord64
  , genTxOut
  , genUtxo0
  , increasingProbabilityAt
  , mkGenesisLedgerState
  , numCoreNodes
  , coreKeyPairs
  , traceKeyPairs
  , traceVRFKeyPairs
  , traceMSigScripts
  , traceMSigCombinations
  , someKeyPairs
  , someScripts
  , pickStakeKey
  , toAddr
  , toCred)
  where

import           Cardano.Crypto.VRF (deriveVerKeyVRF, genKeyVRF)
import           Control.Monad (replicateM)
import           Crypto.Random (drgNewTest, withDRG)
import qualified Data.List as List (findIndex, (\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import           Data.Tuple (swap)
import           Data.Word (Word64)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Address (scriptsToAddr, toAddr, toCred)
import           Coin (Coin (..))
import           ConcreteCryptoTypes (Addr, CoreKeyPair, DPState, GenKeyHash, KeyHash, KeyPair,
                     KeyPairs, LEDGER, MultiSig, SignKeyVRF, TxOut, UTxO, UTxOState, VKey,
                     VerKeyVRF)
import           Control.State.Transition (IRC)
import           Keys (pattern KeyPair, hashAnyKey, hashKey, sKey, vKey)
import           LedgerState (pattern LedgerState, genesisCoins, genesisState)
import           Numeric.Natural (Natural)
import           Test.Utils (mkGenKey, mkKeyPair)
import           Tx (pattern TxOut, hashScript)
import           TxData (pattern AddrBase, pattern KeyHashObj, pattern RequireAllOf,
                     pattern RequireAnyOf, pattern RequireMOf, pattern RequireSignature,
                     pattern ScriptHashObj)

genBool :: Gen Bool
genBool = QC.arbitraryBoundedRandom

genInteger :: Integer -> Integer -> Gen Integer
genInteger lower upper = QC.choose (lower, upper)

-- | Generator for a natural number between 'lower' and 'upper'
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = fromInteger <$> QC.choose (lower', upper')
 where
  lower' = fromIntegral lower
  upper' = fromIntegral upper

-- | Generator for a Word64 between 'lower' and 'upper'
genWord64 :: Word64 -> Word64 -> Gen Word64
genWord64 lower upper = fromIntegral
  <$> genNatural (fromIntegral lower) (fromIntegral upper)

mkKeyPairs :: Word64 -> (KeyPair, KeyPair)
mkKeyPairs n
  = (mkKeyPair_ (2*n), mkKeyPair_ (2*n+1))
  where
    mkKeyPair_ n_ = (uncurry KeyPair . swap) (mkKeyPair (n_,n_,n_,n_,n_))

-- | Constant list of KeyPairs intended to be used in the generators.
traceKeyPairs :: KeyPairs
traceKeyPairs = mkKeyPairs <$> [1 .. 150]

numCoreNodes :: Word64
numCoreNodes = 7

-- | Multi-Sig Scripts based on the `traceKeyPairs` key pairs
traceMSigScripts :: [(MultiSig, MultiSig)]
traceMSigScripts = map mkScriptsFromKeyPair traceKeyPairs

-- | Combine a list of multisig pairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
traceMSigCombinations :: [(MultiSig, MultiSig)] -> [(MultiSig, MultiSig)]
traceMSigCombinations msigs =
  if length msigs < 3 then error "length of input msigs must be at least 3"
  else foldl (++) [] $
       do
         (k1, k2) <- msigs
         (k3, k4) <- msigs List.\\ [(k1, k2)]
         (k5, k6) <- msigs List.\\ [(k1, k2), (k3, k4)]

         let payOneOf   = RequireAnyOf [k1, k3, k5]
             stakeOneOf = RequireAnyOf [k2, k4, k6]
             payAllOf   = RequireAllOf [k1, k3, k5]
             stakeAllOf = RequireAllOf [k2, k4, k6]
             payMOf     = RequireMOf 2 [k1, k3, k5] -- TODO create from 1 to all
             stakeMOf   = RequireMOf 2 [k2, k4, k6]

         pure [(payOneOf, stakeOneOf), (payAllOf, stakeAllOf), (payMOf, stakeMOf)]

mkScriptsFromKeyPair :: (KeyPair, KeyPair) -> (MultiSig, MultiSig)
mkScriptsFromKeyPair (k0, k1) = (mkScriptFromKey k0, mkScriptFromKey k1)

mkScriptFromKey :: KeyPair -> MultiSig
mkScriptFromKey = (RequireSignature . hashAnyKey . vKey)

-- Pairs of (genesis key, node cold key)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodes :: [(CoreKeyPair, KeyPair)]
coreNodes = [ ( (toKeyPair . mkGenKey) (x, 0, 0, 0, 0)
              , (toKeyPair . mkKeyPair) (x, 0, 0, 0, 1))
            | x <- [1001..1000+numCoreNodes]]
          where
            toKeyPair (sk,vk) = KeyPair {sKey = sk, vKey = vk}

coreKeyPairs :: [CoreKeyPair]
coreKeyPairs = fst . unzip $ coreNodes

-- | Select between _lower_ and _upper_ keys from 'traceKeyPairs'
someKeyPairs :: Int -> Int -> Gen KeyPairs
someKeyPairs lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle traceKeyPairs

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first 5 multi-sig scripts of `traceMSigScripts`.
someScripts :: Int -> Int -> Gen [(MultiSig, MultiSig)]
someScripts lower upper =
  take
  <$> QC.choose (lower, upper)
  <*> QC.shuffle (traceMSigCombinations $ take 5 traceMSigScripts)

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

-- | Find first matching script for address.
findPayScript :: Addr -> [(MultiSig, MultiSig)] -> (MultiSig, MultiSig)
findPayScript (AddrBase (ScriptHashObj scriptHash) _) scripts =
  case List.findIndex (\(pay, _) -> scriptHash == hashScript pay) scripts of
    Nothing -> error "findPayScript: could not find matching script for given address"
    Just i  -> scripts !! i
findPayScript _ _ = error "findPayScript: unsupported address format"

-- | Select one random verification staking key from list of pairs of KeyPair.
pickStakeKey :: KeyPairs -> Gen VKey
pickStakeKey keys = vKey . snd <$> QC.elements keys

-- | Generates a list of coins for the given 'Addr' and produced a 'TxOut' for each 'Addr'
--
-- Note: we need to keep the initial utxo coin sizes large enough so that
-- when we simulate sequences of transactions, we have enough funds available
-- to include certificates that require deposits.
genTxOut :: [Addr] -> Gen [TxOut]
genTxOut addrs = do
  ys <- genCoinList 1000 10000 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- | Generates a list of 'Coin' values of length between 'lower' and 'upper'
-- and with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper = do
  len <- QC.choose (lower, upper)
  replicateM len $ genCoin minCoin maxCoin

-- TODO this should be an exponential distribution, not constant
genCoin :: Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> QC.choose (minCoin, maxCoin)

genUtxo0 :: Int -> Int -> Gen UTxO
genUtxo0 lower upper = do
  genesisKeys <- someKeyPairs lower upper
  genesisScripts <- someScripts lower upper
  outs <- genTxOut (fmap toAddr genesisKeys ++ fmap scriptsToAddr genesisScripts)
  return (genesisCoins outs)

genesisDelegs0 :: Map GenKeyHash KeyHash
genesisDelegs0
  = Map.fromList
      [ (hashVKey gkey, hashVKey pkey)
      | (gkey, pkey) <- coreNodes]
  where
    hashVKey = hashKey . vKey

-- | Generate initial state for the LEDGER STS using the STS environment.
--
-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC LEDGER' (the "initial rule context") instead of simply 'LedgerEnv'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisLedgerState
  :: IRC LEDGER
  -> Gen (Either a (UTxOState, DPState))
mkGenesisLedgerState _ = do
  utxo0 <- genUtxo0 5 10
  let (LedgerState utxoSt dpSt) = genesisState genesisDelegs0 utxo0
  pure $ Right (utxoSt, dpSt)

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
  = QC.frequency [ (5, pure lower)
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
