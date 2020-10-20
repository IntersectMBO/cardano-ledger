{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Shelley.Spec.Ledger.Address (scriptsToAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.LedgerState
  ( KeyPairs,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
    defaultConstants,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
import Test.Shelley.Spec.Ledger.Utils
  (ShelleyTest,  MultiSigPairs,
    maxKESIterations,
    mkKESKeyPair,
    mkVRFKeyPair,
    slotsPerKESIteration,
  )
import qualified Cardano.Ledger.Shelley as Shelley

-- | Example generator environment, consisting of default constants and an
-- corresponding keyspace.
genEnv :: Shelley.TxBodyConstraints era => proxy era -> GenEnv era
genEnv _ =
  GenEnv
    (keySpace defaultConstants)
    defaultConstants

-- | Example keyspace for use in generators
keySpace ::
  (Shelley.TxBodyConstraints era) =>
  Constants ->
  KeySpace era
keySpace c =
  KeySpace
    (coreNodeKeys c)
    (genesisDelegates c)
    (stakePoolKeys c)
    (keyPairs c)
    (mSigCombinedScripts c)

-- | Constant list of KeyPairs intended to be used in the generators.
keyPairs :: CC.Crypto crypto => Constants -> KeyPairs crypto
keyPairs Constants {maxNumKeyPairs} = mkKeyPairs <$> [1 .. maxNumKeyPairs]

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: CC.Crypto crypto => Constants -> Int -> Int -> Gen (KeyPairs crypto)
someKeyPairs c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (keyPairs c)

mSigBaseScripts :: Era era => Constants -> MultiSigPairs era
mSigBaseScripts c = mkMSigScripts (keyPairs c)

mSigCombinedScripts :: Era era => Constants -> MultiSigPairs era
mSigCombinedScripts c@(Constants {numBaseScripts}) =
  mkMSigCombinations . take numBaseScripts $ mSigBaseScripts c

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts`.
someScripts :: Era era => Constants -> Int -> Int -> Gen (MultiSigPairs era)
someScripts c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (mSigCombinedScripts c)

-- Pairs of (genesis key, node keys)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodeKeys ::
  CC.Crypto crypto =>
  Constants ->
  [(KeyPair 'Genesis crypto, AllIssuerKeys crypto 'GenesisDelegate)]
coreNodeKeys c@Constants {numCoreNodes} =
  [ ( (toKeyPair . mkGenKey) (x, 0, 0, 0, 0),
      issuerKeys c 0 x
    )
    | x <- [1001 .. 1000 + numCoreNodes]
  ]
  where
    toKeyPair (sk, vk) = KeyPair vk sk

genUtxo0 :: (ShelleyTest era) => QC.Gen (Core.Value era) -> Constants -> Gen (UTxO era)
genUtxo0 gv c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts} = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    genTxOut
      gv
      c
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr Testnet) genesisScripts)
  return (genesisCoins outs)

-- Pre-generate a set of keys to use for genesis delegates.
genesisDelegates :: CC.Crypto crypto => Constants -> [AllIssuerKeys crypto 'GenesisDelegate]
genesisDelegates c =
  [ issuerKeys c 20 x
    | x <- [0 .. 50]
  ]

-- Pre-generate a set of keys to use for stake pools.
stakePoolKeys :: CC.Crypto crypto => Constants -> [AllIssuerKeys crypto 'StakePool]
stakePoolKeys c =
  [ issuerKeys c 10 x
    | x <- [0 .. 50]
  ]

-- | Generate all keys for any entity which will be issuing blocks.
issuerKeys ::
  (CC.Crypto crypto) =>
  Constants ->
  -- | Namespace parameter. Can be used to differentiate between different
  --   "types" of issuer.
  Word64 ->
  Word64 ->
  AllIssuerKeys crypto r
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

genesisDelegs0 ::
  CC.Crypto crypto =>
  Constants ->
  Map (KeyHash 'Genesis crypto) (GenDelegPair crypto)
genesisDelegs0 c =
  Map.fromList
    [ ( hashVKey gkey,
        GenDelegPair
          (coerceKeyRole $ hashVKey (cold pkeys))
          (hashVerKeyVRF . snd . vrf $ pkeys)
      )
      | (gkey, pkeys) <- coreNodeKeys c
    ]
  where
    hashVKey = hashKey . vKey
