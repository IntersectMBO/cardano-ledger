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
  ( numCoreNodes,
    coreNodeSK,
    coreNodeVK,
    coreNodeIssuerKeys,
    coreNodeKeysBySchedule,
    genDelegs,
    overlayScheduleFor,
  )
where

import Cardano.Ledger.Era (Era)
import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
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
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.OverlaySchedule
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

-- | Number of Core Node
numCoreNodes :: Word64
numCoreNodes = 7

mkAllCoreNodeKeys ::
  (Era era) =>
  Word64 ->
  AllIssuerKeys era r
mkAllCoreNodeKeys w =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (w, 0, 0, 0, 2))
    [(KESPeriod 0, mkKESKeyPair (w, 0, 0, 0, 3))]
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (w, 0, 0, 0, 1)

coreNodes ::
  forall era.
  Era era =>
  [ ( (SignKeyDSIGN era, VKey 'Genesis era),
      AllIssuerKeys era 'GenesisDelegate
    )
  ]
coreNodes =
  [ (mkGenKey (x, 0, 0, 0, 0), mkAllCoreNodeKeys x)
    | x <- [101 .. 100 + numCoreNodes]
  ]

-- === Signing (Secret) Keys
-- Retrieve the signing key for a core node by providing
-- a number in the range @[0, ... ('numCoreNodes'-1)]@.
coreNodeSK :: forall era. Era era => Int -> SignKeyDSIGN era
coreNodeSK = fst . fst . (coreNodes @era !!)

-- | === Verification (Public) Keys
-- Retrieve the verification key for a core node by providing
-- a number in the range @[0, ... ('numCoreNodes'-1)]@.
coreNodeVK :: forall era. Era era => Int -> VKey 'Genesis era
coreNodeVK = snd . fst . (coreNodes @era !!)

-- | === Block Issuer Keys
-- Retrieve the block issuer keys (cold, VRF, and hot KES keys)
-- for a core node by providing
-- a number in the range @[0, ... ('numCoreNodes'-1)]@.
coreNodeIssuerKeys ::
  forall era.
  Era era =>
  Int ->
  AllIssuerKeys era 'GenesisDelegate
coreNodeIssuerKeys = snd . (coreNodes @era !!)

coreNodeKeysForSlot ::
  forall era.
  (HasCallStack, Era era) =>
  Map SlotNo (OBftSlot era) ->
  Word64 ->
  AllIssuerKeys era 'GenesisDelegate
coreNodeKeysForSlot overlay slot = case Map.lookup (SlotNo slot) overlay of
  Nothing -> error $ "coreNodesForSlot: Cannot find keys for slot " <> show slot
  Just NonActiveSlot -> error $ "coreNodesForSlot: Non-active slot " <> show slot
  Just (ActiveSlot gkh) ->
    case Data.List.find (\((_, gk), _) -> hashKey gk == gkh) coreNodes of
      Nothing ->
        error $
          "coreNodesForSlot: Cannot find key hash in coreNodes: "
            <> show gkh
      Just ((_, _), ak) -> ak

-- | === Overlay Schedule
-- Retrieve the overlay schedule for a given epoch and protocol parameters.
overlayScheduleFor :: Era era => EpochNo -> PParams era -> OverlaySchedule era
overlayScheduleFor e pp =
  runShelleyBase $
    overlaySchedule
      e
      (Map.keysSet genDelegs)
      pp

-- | === Keys by Overlay Schedule
-- Retrieve all the keys associated with a core node
-- for a given slot and protocol parameters.
-- It will return an error if there is not a core node scheduled
-- for the given slot.
coreNodeKeysBySchedule ::
  (HasCallStack, Era era) =>
  PParams era ->
  Word64 ->
  AllIssuerKeys era 'GenesisDelegate
coreNodeKeysBySchedule = coreNodeKeysForSlot . fullOSched
  where
    fullOSched pp =
      Map.unions $
        [ overlayScheduleToMap $
            overlayScheduleFor e pp
          | e <- [0 .. 10]
        ]

-- | === Genesis Delegation Mapping
-- The map from genesis/core node (verification) key hashes
-- to their delegate's (verification) key hash.
genDelegs ::
  forall era.
  Era era =>
  Map (KeyHash 'Genesis era) (GenDelegPair era)
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
