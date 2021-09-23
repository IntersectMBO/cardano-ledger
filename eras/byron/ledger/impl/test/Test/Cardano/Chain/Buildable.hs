{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Buildable
  ( tests,
  )
where

import Cardano.Chain.Common
  ( Attributes (Attributes),
    UnparsedFields (UnparsedFields),
  )
import Cardano.Prelude
import Formatting (Buildable, build, sformat)
import Hedgehog (PropertyT, eval, property)
import Test.Cardano.Chain.Block.Gen
  ( genBlockSignature,
    genBlockWithEpochSlots,
    genHeader,
  )
import qualified Test.Cardano.Chain.Block.Gen as Block
import qualified Test.Cardano.Chain.Common.Gen as Common
import qualified Test.Cardano.Chain.Delegation.Gen as Delegation
import qualified Test.Cardano.Chain.Genesis.Gen as Genesis
import Test.Cardano.Chain.Slotting.Gen (feedPMEpochSlots)
import qualified Test.Cardano.Chain.Slotting.Gen as Slotting
import qualified Test.Cardano.Chain.UTxO.Gen as UTxO
import qualified Test.Cardano.Chain.Update.Gen as Update
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, eachOfTS)

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

tests :: TSGroup
tests = $$discoverPropArg

-- | Check that the 'Buildable' instance for @a@ doesn't throw exceptions
isBuildable :: Buildable a => a -> PropertyT IO ()
isBuildable = void . eval . sformat build

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

ts_prop_blockIsBuildable :: TSProperty
ts_prop_blockIsBuildable =
  eachOfTS 100 (feedPM genBlockWithEpochSlots) isBuildable

ts_prop_blockProofIsBuildable :: TSProperty
ts_prop_blockProofIsBuildable = eachOfTS 100 (feedPM Block.genProof) isBuildable

ts_prop_headerIsBuildable :: TSProperty
ts_prop_headerIsBuildable =
  eachOfTS
    100
    (feedPMEpochSlots (Slotting.genWithEpochSlots genHeader))
    isBuildable

ts_prop_blockSignatureIsBuildable :: TSProperty
ts_prop_blockSignatureIsBuildable =
  eachOfTS 100 (feedPMEpochSlots genBlockSignature) isBuildable

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

ts_prop_addrAttributesIsBuildable :: TSProperty
ts_prop_addrAttributesIsBuildable =
  eachOfTS 100 Common.genAddrAttributes isBuildable

ts_prop_addrSpendingData :: TSProperty
ts_prop_addrSpendingData = eachOfTS 100 Common.genAddrSpendingData isBuildable

ts_prop_addressIsBuildable :: TSProperty
ts_prop_addressIsBuildable = eachOfTS 100 Common.genAddress isBuildable

ts_prop_attributesUnitIsBuildable :: TSProperty
ts_prop_attributesUnitIsBuildable =
  const . property $ isBuildable (Attributes () (UnparsedFields mempty))

ts_prop_blockCountIsBuildable :: TSProperty
ts_prop_blockCountIsBuildable = eachOfTS 100 Common.genBlockCount isBuildable

ts_prop_chainDifficultyIsBuildable :: TSProperty
ts_prop_chainDifficultyIsBuildable =
  eachOfTS 100 Common.genChainDifficulty isBuildable

ts_prop_keyHashIsBuildable :: TSProperty
ts_prop_keyHashIsBuildable = eachOfTS 100 Common.genKeyHash isBuildable

ts_prop_lovelaceIsBuildable :: TSProperty
ts_prop_lovelaceIsBuildable = eachOfTS 100 Common.genLovelace isBuildable

ts_prop_lovelacePortionIsBuildable :: TSProperty
ts_prop_lovelacePortionIsBuildable =
  eachOfTS 100 Common.genLovelacePortion isBuildable

ts_prop_merkleRootIsBuildable :: TSProperty
ts_prop_merkleRootIsBuildable =
  eachOfTS 100 (Common.genMerkleRoot (pure ())) isBuildable

ts_prop_networkMagicIsBuildable :: TSProperty
ts_prop_networkMagicIsBuildable =
  eachOfTS 100 Common.genNetworkMagic isBuildable

ts_prop_txFeePolicyIsBuildable :: TSProperty
ts_prop_txFeePolicyIsBuildable = eachOfTS 100 Common.genTxFeePolicy isBuildable

ts_prop_txSizeLinearIsBuildable :: TSProperty
ts_prop_txSizeLinearIsBuildable =
  eachOfTS 100 Common.genTxSizeLinear isBuildable

--------------------------------------------------------------------------------
-- Delegation
--------------------------------------------------------------------------------

ts_prop_delegationCertificateIsBuildable :: TSProperty
ts_prop_delegationCertificateIsBuildable =
  eachOfTS 100 (feedPM Delegation.genCertificate) isBuildable

ts_prop_delegationPayloadIsBuildable :: TSProperty
ts_prop_delegationPayloadIsBuildable =
  eachOfTS 100 (feedPM Delegation.genPayload) isBuildable

--------------------------------------------------------------------------------
-- Genesis
--------------------------------------------------------------------------------

ts_prop_genesisKeyHashesIsBuildable :: TSProperty
ts_prop_genesisKeyHashesIsBuildable =
  eachOfTS 100 Genesis.genGenesisKeyHashes isBuildable

ts_prop_genesisNonAvvmBalancesIsBuildable :: TSProperty
ts_prop_genesisNonAvvmBalancesIsBuildable =
  eachOfTS 100 Genesis.genGenesisNonAvvmBalances isBuildable

--------------------------------------------------------------------------------
-- Slotting
--------------------------------------------------------------------------------

ts_prop_epochAndSlotCountIsBuildable :: TSProperty
ts_prop_epochAndSlotCountIsBuildable =
  eachOfTS
    100
    (Slotting.genEpochSlots >>= Slotting.genEpochAndSlotCount)
    isBuildable

ts_prop_epochNumberIsBuildable :: TSProperty
ts_prop_epochNumberIsBuildable =
  eachOfTS 100 Slotting.genEpochNumber isBuildable

ts_prop_epochSlotsIsBuildable :: TSProperty
ts_prop_epochSlotsIsBuildable = eachOfTS 100 Slotting.genEpochSlots isBuildable

ts_prop_slotCountIsBuildable :: TSProperty
ts_prop_slotCountIsBuildable = eachOfTS 100 Slotting.genSlotCount isBuildable

ts_prop_slotNumberIsBuilable :: TSProperty
ts_prop_slotNumberIsBuilable = eachOfTS 100 Slotting.genSlotNumber isBuildable

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

ts_prop_applicationNameIsBuildable :: TSProperty
ts_prop_applicationNameIsBuildable =
  eachOfTS 100 Update.genApplicationName isBuildable

ts_prop_installerHashIsBuildable :: TSProperty
ts_prop_installerHashIsBuildable =
  eachOfTS 100 Update.genInstallerHash isBuildable

ts_prop_updatePayloadIsBuildable :: TSProperty
ts_prop_updatePayloadIsBuildable =
  eachOfTS 100 (feedPM Update.genPayload) isBuildable

ts_prop_proposalIsBuiladble :: TSProperty
ts_prop_proposalIsBuiladble =
  eachOfTS 100 (feedPM Update.genProposal) isBuildable

ts_prop_protocolParametersIsBuildable :: TSProperty
ts_prop_protocolParametersIsBuildable =
  eachOfTS 100 Update.genProtocolParameters isBuildable

ts_prop_protocolParametersUpdateIsBuildable :: TSProperty
ts_prop_protocolParametersUpdateIsBuildable =
  eachOfTS 100 Update.genProtocolParametersUpdate isBuildable

ts_prop_protocolVersionIsBuildable :: TSProperty
ts_prop_protocolVersionIsBuildable =
  eachOfTS 100 Update.genProtocolVersion isBuildable

ts_prop_softforkRuleIsBuildable :: TSProperty
ts_prop_softforkRuleIsBuildable =
  eachOfTS 100 Update.genSoftforkRule isBuildable

ts_prop_softwareVersionIsBuildable :: TSProperty
ts_prop_softwareVersionIsBuildable =
  eachOfTS 100 Update.genSoftforkRule isBuildable

ts_prop_systemTagIsBuildable :: TSProperty
ts_prop_systemTagIsBuildable = eachOfTS 100 Update.genSystemTag isBuildable

ts_prop_voteIsBuildable :: TSProperty
ts_prop_voteIsBuildable = eachOfTS 100 (feedPM Update.genVote) isBuildable

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

ts_prop_txIsBuildable :: TSProperty
ts_prop_txIsBuildable = eachOfTS 100 UTxO.genTx isBuildable

ts_prop_txInIsBuildable :: TSProperty
ts_prop_txInIsBuildable = eachOfTS 100 UTxO.genTxIn isBuildable

ts_prop_txOutIsBuildable :: TSProperty
ts_prop_txOutIsBuildable = eachOfTS 100 UTxO.genTxOut isBuildable

ts_prop_txAuxIsBuildable :: TSProperty
ts_prop_txAuxIsBuildable = eachOfTS 100 (feedPM UTxO.genTxAux) isBuildable

ts_prop_txProofIsBuildable :: TSProperty
ts_prop_txProofIsBuildable = eachOfTS 100 (feedPM UTxO.genTxProof) isBuildable

ts_prop_txInWitnessIsBuildable :: TSProperty
ts_prop_txInWitnessIsBuildable =
  eachOfTS 100 (feedPM UTxO.genTxInWitness) isBuildable
