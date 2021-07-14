{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Auxiliary definitions to make working with the Byron ledger easier
module Cardano.Chain.Byron.API.Validation
  ( applyChainTick,
    validateBlock,
    validateBoundary,
  )
where

import qualified Cardano.Chain.Block as CC
import Cardano.Chain.Byron.API.Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as D.Iface
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as U.Iface
import qualified Cardano.Chain.ValidationMode as CC
import Cardano.Prelude

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkEpochEnvironment ::
  Gen.Config ->
  CC.ChainValidationState ->
  CC.EpochEnvironment
mkEpochEnvironment cfg cvs =
  CC.EpochEnvironment
    { CC.protocolMagic =
        reAnnotateMagicId $
          Gen.configProtocolMagicId cfg,
      CC.k = Gen.configK cfg,
      CC.allowedDelegators = allowedDelegators cfg,
      CC.delegationMap = delegationMap,
      -- The 'currentEpoch' required by the epoch environment is the /old/
      -- epoch (i.e., the one in the ledger state), so that we can verify that
      -- the new epoch indeed is after the old.
      CC.currentEpoch =
        CC.slotNumberEpoch
          (Gen.configEpochSlots cfg)
          (CC.cvsLastSlot cvs)
    }
  where
    delegationMap :: Delegation.Map
    delegationMap = D.Iface.delegationMap $ CC.cvsDelegationState cvs

mkBodyState :: CC.ChainValidationState -> CC.BodyState
mkBodyState cvs =
  CC.BodyState
    { CC.utxo = CC.cvsUtxo cvs,
      CC.updateState = CC.cvsUpdateState cvs,
      CC.delegationState = CC.cvsDelegationState cvs
    }

mkBodyEnvironment ::
  Gen.Config ->
  Update.ProtocolParameters ->
  CC.SlotNumber ->
  CC.BodyEnvironment
mkBodyEnvironment cfg params slotNo =
  CC.BodyEnvironment
    { CC.protocolMagic = reAnnotateMagic $ Gen.configProtocolMagic cfg,
      CC.utxoConfiguration = Gen.configUTxOConfiguration cfg,
      CC.k = Gen.configK cfg,
      CC.allowedDelegators = allowedDelegators cfg,
      CC.protocolParameters = params,
      -- The 'currentEpoch' for validating a block should be the /current/
      -- epoch (that is, the epoch of the block), /not/ the old epoch
      -- (from the ledger state). This is to make sure delegation certificates
      -- are for the /next/ epoch.
      CC.currentEpoch =
        CC.slotNumberEpoch
          (Gen.configEpochSlots cfg)
          slotNo
    }

-- | Apply chain tick
--
-- This is the part of block processing that depends only on the slot number of
-- the block: We update
--
-- * The update state
-- * The delegation state
-- * The last applied slot number
--
-- NOTE: The spec currently only updates the update state here; this is not good
-- enough. Fortunately, updating the delegation state and slot number here
-- (currently done in body processing) is at least /conform/ spec, as these
-- updates are conform spec. See
--
-- <https://github.com/input-output-hk/cardano-ledger-specs/issues/1046>
-- <https://github.com/input-output-hk/ouroboros-network/issues/1291>
applyChainTick ::
  Gen.Config ->
  CC.SlotNumber ->
  CC.ChainValidationState ->
  CC.ChainValidationState
applyChainTick cfg slotNo cvs =
  cvs
    { CC.cvsUpdateState =
        CC.epochTransition
          (mkEpochEnvironment cfg cvs)
          (CC.cvsUpdateState cvs)
          slotNo,
      CC.cvsDelegationState =
        D.Iface.tickDelegation
          currentEpoch
          slotNo
          (CC.cvsDelegationState cvs)
    }
  where
    currentEpoch = CC.slotNumberEpoch (Gen.configEpochSlots cfg) slotNo

-- | Validate header
--
-- NOTE: Header validation does not produce any state changes; the only state
-- changes arising from processing headers come from 'applyChainTick'.
validateHeader ::
  MonadError CC.ChainValidationError m =>
  CC.ValidationMode ->
  U.Iface.State ->
  CC.AHeader ByteString ->
  m ()
validateHeader validationMode updState hdr =
  flip runReaderT validationMode $
    CC.headerIsValid updState hdr

validateBody ::
  MonadError CC.ChainValidationError m =>
  CC.ValidationMode ->
  CC.ABlock ByteString ->
  CC.BodyEnvironment ->
  CC.BodyState ->
  m CC.BodyState
validateBody validationMode block bodyEnv bodyState =
  flip runReaderT validationMode $
    CC.updateBody bodyEnv bodyState block

validateBlock ::
  MonadError CC.ChainValidationError m =>
  Gen.Config ->
  CC.ValidationMode ->
  CC.ABlock ByteString ->
  CC.HeaderHash ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
validateBlock cfg validationMode block blkHash cvs = do
  validateHeader validationMode updState (CC.blockHeader block)
  bodyState' <- validateBody validationMode block bodyEnv bodyState
  return
    cvs
      { CC.cvsLastSlot = CC.blockSlot block,
        CC.cvsPreviousHash = Right $! blkHash,
        CC.cvsUtxo = CC.utxo bodyState',
        CC.cvsUpdateState = CC.updateState bodyState',
        CC.cvsDelegationState = CC.delegationState bodyState'
      }
  where
    updState = CC.cvsUpdateState cvs
    bodyEnv =
      mkBodyEnvironment
        cfg
        (getProtocolParams cvs)
        (CC.blockSlot block)
    bodyState = mkBodyState cvs

-- | Apply a boundary block
--
-- NOTE: The `cvsLastSlot` calculation must match the one in 'abobHdrSlotNo'.
validateBoundary ::
  MonadError CC.ChainValidationError m =>
  Gen.Config ->
  CC.ABoundaryBlock ByteString ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
validateBoundary cfg blk cvs = do
  -- TODO: Unfortunately, 'updateChainBoundary' doesn't take a hash as an
  -- argument but recomputes it.
  cvs' <- CC.updateChainBoundary cvs blk
  return
    cvs'
      { CC.cvsLastSlot = CC.boundaryBlockSlot epochSlots (CC.boundaryEpoch hdr)
      }
  where
    hdr = CC.boundaryHeader blk
    epochSlots = Gen.configEpochSlots cfg
