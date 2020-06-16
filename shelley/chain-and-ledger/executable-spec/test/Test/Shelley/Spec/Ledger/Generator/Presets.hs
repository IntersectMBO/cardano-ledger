{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pre-generated items to use in traces.
--
--   Functions in this module make specific assumptions about the sets of keys
--   involved, and thus cannot be used as generic generators.
module Test.Shelley.Spec.Ledger.Generator.Presets
  ( coreNodeKeys,
    keySpace,
    genEnv,
    genUtxo0,
    genesisDelegs0,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Shelley.Spec.Ledger.Address (scriptsToAddr, toAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Keys
  ( HashType (RegularHash),
    KeyRole (..),
    KeyRoleHashType,
    coerceKeyRole,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState (genesisCoins)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( GenDelegPair,
    GenesisKeyPair,
    KeyHash,
    KeyPairs,
    MultiSigPairs,
    UTxO,
    hashKeyVRF,
    pattern GenDelegPair,
    pattern KeyPair,
  )
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
    defaultConstants,
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
genEnv :: HashAlgorithm h => proxy h -> GenEnv h
genEnv _ =
  GenEnv
    (keySpace defaultConstants)
    defaultConstants

-- | Example keyspace for use in generators
keySpace :: HashAlgorithm h => Constants -> KeySpace h
keySpace c =
  KeySpace
    (coreNodeKeys c)
    (genesisDelegates c)
    (stakePoolKeys c)
    (keyPairs c)
    (mSigCombinedScripts c)

-- | Constant list of KeyPairs intended to be used in the generators.
keyPairs :: Constants -> KeyPairs h
keyPairs Constants {maxNumKeyPairs} = mkKeyPairs <$> [1 .. maxNumKeyPairs]

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: Constants -> Int -> Int -> Gen (KeyPairs h)
someKeyPairs c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (keyPairs c)

mSigBaseScripts :: HashAlgorithm h => Constants -> MultiSigPairs h
mSigBaseScripts c = mkMSigScripts (keyPairs c)

mSigCombinedScripts :: HashAlgorithm h => Constants -> MultiSigPairs h
mSigCombinedScripts c@(Constants {numBaseScripts}) =
  mkMSigCombinations . take numBaseScripts $ mSigBaseScripts c

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts`.
someScripts :: HashAlgorithm h => Constants -> Int -> Int -> Gen (MultiSigPairs h)
someScripts c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (mSigCombinedScripts c)

-- Pairs of (genesis key, node keys)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodeKeys :: HashAlgorithm h => Constants -> [(GenesisKeyPair h, AllIssuerKeys h 'GenesisDelegate)]
coreNodeKeys c@Constants {numCoreNodes} =
  [ ( (toKeyPair . mkGenKey) (x, 0, 0, 0, 0),
      issuerKeys c 0 x
    )
    | x <- [1001 .. 1000 + numCoreNodes]
  ]
  where
    toKeyPair (sk, vk) = KeyPair vk sk

genUtxo0 :: HashAlgorithm h => Constants -> Gen (UTxO h)
genUtxo0 c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts} = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    genTxOut
      c
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr Testnet) genesisScripts)
  return (genesisCoins outs)

-- Pre-generate a set of keys to use for genesis delegates.
genesisDelegates :: HashAlgorithm h => Constants -> [AllIssuerKeys h 'GenesisDelegate]
genesisDelegates c =
  [ issuerKeys c 20 x
    | x <- [0 .. 50]
  ]

-- Pre-generate a set of keys to use for stake pools.
stakePoolKeys :: HashAlgorithm h => Constants -> [AllIssuerKeys h 'StakePool]
stakePoolKeys c =
  [ issuerKeys c 10 x
    | x <- [0 .. 50]
  ]

-- | Generate all keys for any entity which will be issuing blocks.
issuerKeys ::
  (HashAlgorithm h, KeyRoleHashType r ~ 'RegularHash) =>
  Constants ->
  -- | Namespace parameter. Can be used to differentiate between different
  --   "types" of issuer.
  Word64 ->
  Word64 ->
  AllIssuerKeys h r
issuerKeys Constants {maxSlotTrace} ns x =
  let (skCold, vkCold) = mkKeyPair (x, 0, 0, 0, ns + 1)
   in AllIssuerKeys
        { cold = KeyPair vkCold skCold,
          hot =
            [ ( KESPeriod (fromIntegral (iter * fromIntegral maxKESIterations)),
                mkKESKeyPair (x, 0, 0, fromIntegral iter, ns + 3)
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
            ],
          vrf = mkVRFKeyPair (x, 0, 0, 0, ns + 2),
          hk = hashKey vkCold
        }

genesisDelegs0 :: HashAlgorithm h => Constants -> Map (KeyHash h 'Genesis) (GenDelegPair h)
genesisDelegs0 c =
  Map.fromList
    [ ( hashVKey gkey,
        GenDelegPair
          (coerceKeyRole $ hashVKey (cold pkeys))
          (hashKeyVRF . snd . vrf $ pkeys)
      )
      | (gkey, pkeys) <- coreNodeKeys c
    ]
  where
    hashVKey = hashKey . vKey
