{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Federation
-- Description : Core Nodes for Shelley ledger examples
--
-- The genesis/core nodes for Shelley Ledger Examples.
module Test.Shelley.Spec.Ledger.Examples.Federation
  ( -- | Number of Core Node
    numCoreNodes,
    -- | = Core Node Keys
    --
    -- === Signing (Secret) Keys
    -- Retrieve the signing key for a core node by providing
    -- a number in the range @[0, ... ('numCoreNodes'-1)]@.
    coreNodeSK,
    -- | === Verification (Public) Keys
    -- Retrieve the verification key for a core node by providing
    -- a number in the range @[0, ... ('numCoreNodes'-1)]@.
    coreNodeVK,
    -- | === Block Issuer Keys
    -- Retrieve the block issuer keys (cold, VRF, and hot KES keys)
    -- for a core node by providing
    -- a number in the range @[0, ... ('numCoreNodes'-1)]@.
    coreNodeIssuerKeys,
    -- | === Keys by Overlay Schedule
    -- Retrieve all the keys associated with a core node
    -- for a given slot and protocol parameters.
    -- It will return an error if there is not a core node scheduled
    -- for the given slot.
    coreNodeKeysBySchedule,
    -- | === Genesis Delegation Mapping
    -- The map from genesis/core node (verification) key hashes
    -- to their delegate's (verification) key hashe.
    genDelegs,
    -- | === Overlay Schedule
    -- Retrieve the overlay schedule for a given epoch and protocol parameters.
    overlayScheduleFor,
  )
where

import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    SignKeyDSIGN,
    VKey (..),
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( OBftSlot (..),
    overlaySchedule,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams
  ( PParams,
  )
import Shelley.Spec.Ledger.Slot
  ( EpochNo (..),
    SlotNo (..),
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
  )
import Test.Shelley.Spec.Ledger.Utils

numCoreNodes :: Word64
numCoreNodes = 7

mkAllCoreNodeKeys ::
  (Crypto c) =>
  Word64 ->
  AllIssuerKeys c r
mkAllCoreNodeKeys w =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (w, 0, 0, 0, 2))
    [(KESPeriod 0, mkKESKeyPair (w, 0, 0, 0, 3))]
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (w, 0, 0, 0, 1)

coreNodes :: forall c. Crypto c => [((SignKeyDSIGN c, VKey 'Genesis c), AllIssuerKeys c 'GenesisDelegate)]
coreNodes = [(mkGenKey (x, 0, 0, 0, 0), mkAllCoreNodeKeys x) | x <- [101 .. 100 + numCoreNodes]]

coreNodeSK :: forall c. Crypto c => Int -> SignKeyDSIGN c
coreNodeSK = fst . fst . (coreNodes @c !!)

coreNodeVK :: forall c. Crypto c => Int -> VKey 'Genesis c
coreNodeVK = snd . fst . (coreNodes @c !!)

coreNodeIssuerKeys :: forall c. Crypto c => Int -> AllIssuerKeys c 'GenesisDelegate
coreNodeIssuerKeys = snd . (coreNodes @c !!)

coreNodeKeysForSlot ::
  forall c.
  (HasCallStack, Crypto c) =>
  Map SlotNo (OBftSlot c) ->
  Word64 ->
  AllIssuerKeys c 'GenesisDelegate
coreNodeKeysForSlot overlay slot = case Map.lookup (SlotNo slot) overlay of
  Nothing -> error $ "coreNodesForSlot: Cannot find keys for slot " <> show slot
  Just NonActiveSlot -> error $ "coreNodesForSlot: Non-active slot " <> show slot
  Just (ActiveSlot gkh) ->
    case Data.List.find (\((_, gk), _) -> hashKey gk == gkh) coreNodes of
      Nothing -> error $ "coreNodesForSlot: Cannot find key hash in coreNodes: " <> show gkh
      Just ((_, _), ak) -> ak

overlayScheduleFor :: Crypto c => EpochNo -> PParams -> Map SlotNo (OBftSlot c)
overlayScheduleFor e pp =
  runShelleyBase $
    overlaySchedule
      e
      (Map.keysSet genDelegs)
      pp

coreNodeKeysBySchedule ::
  (HasCallStack, Crypto c) =>
  PParams ->
  Word64 ->
  AllIssuerKeys c 'GenesisDelegate
coreNodeKeysBySchedule = coreNodeKeysForSlot . fullOSched
  where
    fullOSched pp = Map.unions $ [overlayScheduleFor e pp | e <- [0 .. 10]]

genDelegs :: forall c. Crypto c => Map (KeyHash 'Genesis c) (GenDelegPair c)
genDelegs =
  Map.fromList
    [ ( hashKey $ snd gkey,
        ( GenDelegPair
            (coerceKeyRole . hashKey . vKey $ cold pkeys)
            (hashVerKeyVRF . snd . vrf $ pkeys)
        )
      )
      | (gkey, pkeys) <- coreNodes
    ]
