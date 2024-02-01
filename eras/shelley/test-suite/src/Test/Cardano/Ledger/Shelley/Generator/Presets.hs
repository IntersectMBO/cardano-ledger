{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pre-generated items to use in traces.
--
--   Functions in this module make specific assumptions about the sets of keys
--   involved, and thus cannot be used as generic generators.
module Test.Cardano.Ledger.Shelley.Generator.Presets (
  coreNodeKeys,
  keySpace,
  genEnv,
  genesisDelegs0,
  someKeyPairs,
  keyPairs,
  scriptSpace,
)
where

import Cardano.Ledger.Core (EraScript, hashScript)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  KeyHash,
  KeyRole (..),
  coerceKeyRole,
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), allScripts, someKeyPairs)
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (keyPairs)
import Test.Cardano.Ledger.Shelley.Utils (
  maxKESIterations,
  mkKESKeyPair,
  mkVRFKeyPair,
  slotsPerKESIteration,
 )

-- =================================================================

-- | Example generator environment, consisting of default constants and an
-- corresponding keyspace.
genEnv ::
  forall era.
  EraGen era =>
  Proxy era ->
  Constants ->
  GenEnv era
genEnv _ constants =
  GenEnv
    (keySpace constants)
    (scriptSpace @era (genEraTwoPhase3Arg @era) (genEraTwoPhase2Arg @era))
    constants

-- | An Example Script space for use in Trace generators
scriptSpace ::
  forall era.
  EraScript era =>
  [TwoPhase3ArgInfo era] ->
  [TwoPhase2ArgInfo era] ->
  ScriptSpace era
scriptSpace scripts3 scripts2 =
  ScriptSpace
    scripts3
    scripts2
    (Map.fromList [(hashScript @era (getScript3 s), s) | s <- scripts3])
    (Map.fromList [(hashScript @era (getScript2 s), s) | s <- scripts2])

-- | Example keyspace for use in generators
keySpace ::
  forall era.
  EraGen era =>
  Constants ->
  KeySpace era
keySpace c =
  KeySpace
    (coreNodeKeys c)
    (genesisDelegates c)
    (stakePoolKeys c)
    (keyPairs c)
    (allScripts @era c)

-- Pairs of (genesis key, node keys)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodeKeys ::
  Crypto c =>
  Constants ->
  [(KeyPair 'Genesis c, AllIssuerKeys c 'GenesisDelegate)]
coreNodeKeys c@Constants {numCoreNodes} =
  [ ( (toKeyPair . mkGenKey) (RawSeed x 0 0 0 0)
    , issuerKeys c 0 x
    )
  | x <- [1001 .. 1000 + numCoreNodes]
  ]
  where
    toKeyPair (sk, vk) = KeyPair vk sk

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
  Crypto c =>
  Constants ->
  -- | Namespace parameter. Can be used to differentiate between different
  --   "types" of issuer.
  Word64 ->
  Word64 ->
  AllIssuerKeys c r
issuerKeys Constants {maxSlotTrace} ns x =
  let (skCold, vkCold) = mkKeyPair (RawSeed x 0 0 0 (ns + 1))
      iters =
        0
          :| [ 1
             .. 1
              + ( maxSlotTrace
                    `div` fromIntegral (maxKESIterations * slotsPerKESIteration)
                )
             ]
   in AllIssuerKeys
        { aikCold = KeyPair vkCold skCold
        , aikHot =
            fmap
              ( \iter ->
                  ( KESPeriod (fromIntegral (iter * fromIntegral maxKESIterations))
                  , mkKESKeyPair (RawSeed x 0 0 (fromIntegral iter) (ns + 3))
                  )
              )
              iters
        , aikVrf = mkVRFKeyPair (RawSeed x 0 0 0 (ns + 2))
        , aikColdKeyHash = hashKey vkCold
        }

genesisDelegs0 ::
  Crypto c =>
  Constants ->
  Map (KeyHash 'Genesis c) (GenDelegPair c)
genesisDelegs0 c =
  Map.fromList
    [ ( hashVKey gkey
      , GenDelegPair
          (coerceKeyRole . hashVKey $ aikCold pkeys)
          (hashVerKeyVRF . vrfVerKey $ aikVrf pkeys)
      )
    | (gkey, pkeys) <- coreNodeKeys c
    ]
  where
    hashVKey = hashKey . vKey
