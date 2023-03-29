{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Federation
-- Description : Core Nodes for Shelley ledger examples
--
-- The genesis/core nodes for Shelley Ledger Examples.
module Test.Cardano.Ledger.Shelley.Examples.Federation (
  numCoreNodes,
  coreNodeSK,
  coreNodeVK,
  coreNodeIssuerKeys,
  coreNodeKeysBySchedule,
  genDelegs,
)
where

import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Core (EraCrypto, EraPParams (..), PParams (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  KeyHash (..),
  KeyRole (..),
  SignKeyDSIGN,
  VKey (..),
  coerceKeyRole,
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Cardano.Protocol.TPraos.Rules.Overlay (
  OBftSlot (..),
  lookupInOverlaySchedule,
 )
import qualified Data.List
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), vKey)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  VRFKeyPair (..),
 )
import Test.Cardano.Ledger.Shelley.Utils

-- | Number of Core Node
numCoreNodes :: Word64
numCoreNodes = 7

mkAllCoreNodeKeys ::
  Crypto c =>
  Word64 ->
  AllIssuerKeys c r
mkAllCoreNodeKeys w =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (RawSeed w 0 0 0 2))
    ((KESPeriod 0, mkKESKeyPair (RawSeed w 0 0 0 3)) NE.:| [])
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (RawSeed w 0 0 0 1)

coreNodes ::
  forall c.
  Crypto c =>
  [ ( (SignKeyDSIGN c, VKey 'Genesis c)
    , AllIssuerKeys c 'GenesisDelegate
    )
  ]
coreNodes =
  [ (mkGenKey (RawSeed x 0 0 0 0), mkAllCoreNodeKeys x)
  | x <- [101 .. 100 + numCoreNodes]
  ]

-- === Signing (Secret) Keys
-- Retrieve the signing key for a core node by providing
-- a number in the range @[0, ... ('numCoreNodes'-1)]@.
coreNodeSK :: forall c. Crypto c => Int -> SignKeyDSIGN c
coreNodeSK = fst . fst . (coreNodes @c !!)

-- | === Verification (Public) Keys
-- Retrieve the verification key for a core node by providing
-- a number in the range @[0, ... ('numCoreNodes'-1)]@.
coreNodeVK :: forall c. Crypto c => Int -> VKey 'Genesis c
coreNodeVK = snd . fst . (coreNodes @c !!)

-- | === Block Issuer Keys
-- Retrieve the block issuer keys (cold, VRF, and hot KES keys)
-- for a core node by providing
-- a number in the range @[0, ... ('numCoreNodes'-1)]@.
coreNodeIssuerKeys ::
  forall c.
  Crypto c =>
  Int ->
  AllIssuerKeys c 'GenesisDelegate
coreNodeIssuerKeys = snd . (coreNodes @c !!)

-- | === Keys by Overlay Schedule
-- Retrieve all the keys associated with a core node
-- for a given slot and protocol parameters.
-- It will return an error if there is not a core node scheduled
-- for the given slot.
coreNodeKeysBySchedule ::
  forall era.
  (HasCallStack, EraPParams era) =>
  PParams era ->
  Word64 ->
  AllIssuerKeys (EraCrypto era) 'GenesisDelegate
coreNodeKeysBySchedule pp slot =
  case lookupInOverlaySchedule
    firstSlot
    (Map.keysSet genDelegs)
    (pp ^. ppDG)
    (activeSlotCoeff testGlobals)
    slot' of
    Nothing -> error $ "coreNodesForSlot: Cannot find keys for slot " <> show slot
    Just NonActiveSlot -> error $ "coreNodesForSlot: Non-active slot " <> show slot
    Just (ActiveSlot gkh) ->
      case Data.List.find (\((_, gk), _) -> hashKey gk == gkh) coreNodes of
        Nothing ->
          error $
            "coreNodesForSlot: Cannot find key hash in coreNodes: "
              <> show gkh
        Just ((_, _), ak) -> ak
  where
    slot' = SlotNo slot
    firstSlot = slotFromEpoch . epochFromSlotNo $ slot'

-- | === Genesis Delegation Mapping
-- The map from genesis/core node (verification) key hashes
-- to their delegate's (verification) key hash.
genDelegs ::
  forall c.
  Crypto c =>
  Map (KeyHash 'Genesis c) (GenDelegPair c)
genDelegs =
  Map.fromList
    [ ( hashKey $ snd gkey
      , ( GenDelegPair
            (coerceKeyRole . hashKey . vKey $ aikCold pkeys)
            (hashVerKeyVRF . vrfVerKey $ aikVrf pkeys)
        )
      )
    | (gkey, pkeys) <- coreNodes
    ]
