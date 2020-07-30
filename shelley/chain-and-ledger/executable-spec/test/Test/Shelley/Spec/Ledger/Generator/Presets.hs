{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Shelley.Spec.Ledger.Address (scriptsToAddr, toAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (KeyPair (..))
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    coerceKeyRole,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( GenDelegPair,
    GenesisKeyPair,
    KeyPairs,
    MultiSigPairs,
    UTxO,
    hashKeyVRF,
    pattern GenDelegPair,
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
genEnv :: Crypto c => proxy c -> GenEnv c
genEnv _ =
  GenEnv
    (keySpace defaultConstants)
    defaultConstants

-- | Example keyspace for use in generators
keySpace :: Crypto c => Constants -> KeySpace c
keySpace c =
  KeySpace
    (coreNodeKeys c)
    (genesisDelegates c)
    (stakePoolKeys c)
    (keyPairs c)
    (mSigCombinedScripts c)

-- | Constant list of KeyPairs intended to be used in the generators.
keyPairs :: Crypto c => Constants -> KeyPairs c
keyPairs Constants {maxNumKeyPairs} = mkKeyPairs <$> [1 .. maxNumKeyPairs]

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: Crypto c => Constants -> Int -> Int -> Gen (KeyPairs c)
someKeyPairs c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (keyPairs c)

mSigCombinedScripts :: Constants -> MultiSigPairs c
mSigCombinedScripts _ = []

{- TODO re-enable after the script embargo has been lifted
 -
 - mSigBaseScripts :: Crypto c => Constants -> MultiSigPairs c
 - mSigBaseScripts c = mkMSigScripts (keyPairs c)
 -
 - mSigCombinedScripts :: Crypto c => Constants -> MultiSigPairs c
 - mSigCombinedScripts c@(Constants {numBaseScripts}) =
 -   mkMSigCombinations . take numBaseScripts $ mSigBaseScripts c
 -}

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts`.
someScripts :: Constants -> Int -> Int -> Gen (MultiSigPairs c)
someScripts c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (mSigCombinedScripts c)

-- Pairs of (genesis key, node keys)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodeKeys :: Crypto c => Constants -> [(GenesisKeyPair c, AllIssuerKeys c 'GenesisDelegate)]
coreNodeKeys c@Constants {numCoreNodes} =
  [ ( (toKeyPair . mkGenKey) (x, 0, 0, 0, 0),
      issuerKeys c 0 x
    )
    | x <- [1001 .. 1000 + numCoreNodes]
  ]
  where
    toKeyPair (sk, vk) = KeyPair vk sk

genUtxo0 :: Crypto c => Constants -> Gen (UTxO c)
genUtxo0 c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts} = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    genTxOut
      c
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr Testnet) genesisScripts)
  return (genesisCoins outs)

-- Pre-generate a set of keys to use for genesis delegates.
genesisDelegates :: Crypto c => Constants -> [AllIssuerKeys c 'GenesisDelegate]
genesisDelegates c =
  [ issuerKeys c 20 x
    | x <- [0 .. 50]
  ]

-- Pre-generate a set of keys to use for stake pools.
stakePoolKeys :: Crypto c => Constants -> [AllIssuerKeys c 'StakePool]
stakePoolKeys c =
  [ issuerKeys c 10 x
    | x <- [0 .. 50]
  ]

-- | Generate all keys for any entity which will be issuing blocks.
issuerKeys ::
  (Crypto c) =>
  Constants ->
  -- | Namespace parameter. Can be used to differentiate between different
  --   "types" of issuer.
  Word64 ->
  Word64 ->
  AllIssuerKeys c r
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

genesisDelegs0 :: Crypto c => Constants -> Map (KeyHash 'Genesis c) (GenDelegPair c)
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
