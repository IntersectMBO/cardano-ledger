{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Pre-generated items to use in traces.
--
--   Functions in this module make specific assumptions about the sets of keys
--   involved, and thus cannot be used as generic generators.
module Test.Shelley.Spec.Ledger.Generator.Presets
  ( keySpace,
    genEnv,
    genUtxo0,
    genesisDelegs0,
  )
where

import Cardano.Crypto.VRF (deriveVerKeyVRF, genKeyVRF)
import Crypto.Random (drgNewTest, withDRG)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.Address (scriptsToAddr, toAddr)
import Shelley.Spec.Ledger.Keys
  ( hashKey,
    sKey,
    vKey,
    pattern KeyPair,
  )
import Shelley.Spec.Ledger.LedgerState (genesisCoins)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( CoreKeyPair,
    GenKeyHash,
    KeyHash,
    KeyPairs,
    MultiSigPairs,
    SignKeyVRF,
    UTxO,
    VerKeyVRF,
  )
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
    defaultConstants
  )
import Test.Shelley.Spec.Ledger.Generator.Core
import Test.Shelley.Spec.Ledger.Utils
  ( maxKESIterations,
    mkGenKey,
    mkKESKeyPair,
    mkKeyPair,
    mkVRFKeyPair,
    slotsPerKESIteration,
  )

-- | Example generator environment, consisting of default constants and an
-- corresponding keyspace.
genEnv :: GenEnv
genEnv = GenEnv
  (keySpace defaultConstants)
  defaultConstants

-- | Example keyspace for use in generators
keySpace :: Constants -> KeySpace
keySpace c =
  KeySpace
    (coreNodeKeys c)
    (keyPairs c)
    (mSigCombinedScripts c)
    vrfKeyPairs

-- | Constant list of KeyPairs intended to be used in the generators.
keyPairs :: Constants -> KeyPairs
keyPairs Constants {maxNumKeyPairs} = mkKeyPairs <$> [1 .. maxNumKeyPairs]

-- | A pre-populated space of VRF keys for use in the generators.
vrfKeyPairs :: [(SignKeyVRF, VerKeyVRF)]
vrfKeyPairs = [body (0, 0, 0, 0, i) | i <- [1 .. 50]]
  where
    body seed = fst . withDRG (drgNewTest seed) $ do
      sk <- genKeyVRF
      return (sk, deriveVerKeyVRF sk)

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: Constants -> Int -> Int -> Gen KeyPairs
someKeyPairs c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (keyPairs c)

mSigBaseScripts :: Constants -> MultiSigPairs
mSigBaseScripts c = mkMSigScripts (keyPairs c)

mSigCombinedScripts :: Constants -> MultiSigPairs
mSigCombinedScripts c@(Constants {numBaseScripts}) =
  mkMSigCombinations . take numBaseScripts $ mSigBaseScripts c

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts`.
someScripts :: Constants -> Int -> Int -> Gen MultiSigPairs
someScripts c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (mSigCombinedScripts c)

-- Pairs of (genesis key, node keys)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodeKeys :: Constants -> [(CoreKeyPair, AllPoolKeys)]
coreNodeKeys Constants {maxSlotTrace, numCoreNodes} =
  [ ( (toKeyPair . mkGenKey) (x, 0, 0, 0, 0),
      let (skCold, vkCold) = mkKeyPair (x, 0, 0, 0, 1)
       in AllPoolKeys
            (toKeyPair (skCold, vkCold))
            (mkVRFKeyPair (x, 0, 0, 0, 2))
            [ ( KESPeriod (fromIntegral (iter * fromIntegral maxKESIterations)),
                mkKESKeyPair (x, 0, 0, fromIntegral iter, 3)
              )
              | iter <-
                  [ 0
                    .. ( 1
                           + div
                             maxSlotTrace
                             ( fromIntegral
                                 (maxKESIterations * slotsPerKESIteration)
                             )
                       )
                  ]
            ]
            (hashKey vkCold)
    )
    | x <- [1001 .. 1000 + numCoreNodes]
  ]
  where
    toKeyPair (sk, vk) = KeyPair {sKey = sk, vKey = vk}

genUtxo0 :: Constants -> Gen UTxO
genUtxo0 c@Constants{minGenesisUTxOouts, maxGenesisUTxOouts} = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts c minGenesisUTxOouts maxGenesisUTxOouts
  outs <- genTxOut c (fmap toAddr genesisKeys ++ fmap scriptsToAddr genesisScripts)
  return (genesisCoins outs)

genesisDelegs0 :: Constants -> Map GenKeyHash KeyHash
genesisDelegs0 c =
  Map.fromList
    [ (hashVKey gkey, hashVKey (cold pkeys))
      | (gkey, pkeys) <- coreNodeKeys c
    ]
  where
    hashVKey = hashKey . vKey
