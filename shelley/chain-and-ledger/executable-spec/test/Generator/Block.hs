{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Generator.Block
  ( genBlock
  )
  where

import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import           Data.Word (Word64)
import           Test.QuickCheck (Gen, choose)

import           ConcreteCryptoTypes (Block, ChainState, CoreKeyPair, KeyHash, KeyPair, LEDGERS)
import           Control.State.Transition.Trace.Generator.QuickCheck (sigGen)
import           Generator.Core.QuickCheck (AllPoolKeys (..), NatNonce (..), genNatural, mkBlock,
                     zero)
import           Generator.LedgerTrace.QuickCheck ()
import           Keys (GenDelegs (..), hashKey, vKey)
import           LedgerState (esAccountState, esLState, esPp, nesEs, nesOsched, _delegationState,
                     _dstate, _genDelegs, _reserves)
import           Slot (BlockNo (..), SlotNo (..))
import           STS.Chain (chainEpochNonce, chainHashHeader, chainNes, chainSlotNo)
import           STS.Ledgers (LedgersEnv (..))

genBlock
  :: SlotNo
  -> ChainState
  -> [(CoreKeyPair, AllPoolKeys)] -- core node keys
  -> Map KeyHash KeyPair -- indexed keys By StakeHash
  -> Gen Block
genBlock _slotNo chainSt coreNodeKeys keysByStakeHash = do
  nextSlot <- genNextSlot
  mkBlock
    <$> pure (chainHashHeader chainSt)
    <*> pure issuerKeys
    <*> toList <$> genTxs nextSlot
    <*> pure nextSlot
    <*> pure chainDifficulty
    <*> pure (chainEpochNonce chainSt)
    <*> genBlockNonce
    <*> genPraosLeader
    <*> pure kesPeriod
  where
    ledgerSt = (esLState . nesEs . chainNes) chainSt
    osched = (nesOsched . chainNes) chainSt
    (GenDelegs genesisDelegs) = (_genDelegs . _dstate . _delegationState) ledgerSt

    -- TODO @uroboros pick the first core node each time for now
    origIssuerKeys = snd (coreNodeKeys !! 0)
    issuerKeys = case Map.lookup (chainSlotNo chainSt) osched of
      Nothing ->
        error "TODO @uroboros make Praos block"
      Just Nothing ->
        error "TODO @uroboros don't make a block in a NonActive Slot"
      Just (Just gkey) ->
        case Map.lookup gkey genesisDelegs of
          Nothing ->
            error "genBlock: NoGenesisStakingOVERLAY"
          Just gKeyHash ->
            -- if GenesisDelegate certs changed a delegation to a new key
            case Map.lookup gKeyHash keysByStakeHash of
              Nothing ->
                -- then we use the original keys (which have not been changed by a genesis delegation)
                origIssuerKeys
              Just updatedCold ->
                -- if we find the pre-hashed key in keysByStakeHash, we use it instead of the original cold key
                origIssuerKeys {cold = updatedCold, hk = (hashKey . vKey) updatedCold}

    -- TODO @uroboros "90 days of slots" (constant)
    kesPeriod = 0

    -- TODO @uroboros
    genPraosLeader = pure zero

    chainDifficulty = BlockNo 1 -- used only in consensus

    -- we assume small gaps in slot numbers
    genNextSlot = SlotNo . (unSlotNo (chainSlotNo chainSt) +) <$> choose (1, 5)

    genBlockNonce = NatNonce <$> genNatural 1 100

    genTxs s = do
      let pParams = (esPp . nesEs . chainNes) chainSt
          reserves = (_reserves . esAccountState . nesEs . chainNes) chainSt
          ledgerEnv = LedgersEnv s pParams reserves

      n <- choose (1, 10)
      sigGen @LEDGERS (n :: Word64) ledgerEnv ledgerSt
