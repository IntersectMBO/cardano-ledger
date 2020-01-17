{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Generator.Block
  ( genBlock
  )
  where

import           Data.Foldable (toList)
import           Data.Word (Word64)
import           Test.QuickCheck (Gen, choose)

import           ConcreteCryptoTypes (Block, ChainState, CoreKeyPair, LEDGERS)
import           Control.State.Transition.Trace.Generator.QuickCheck (sigGen)
import           Generator.Core.QuickCheck (AllPoolKeys (..), NatNonce (..), genNatural,
                     mkBlock, zero)
import           Generator.LedgerTrace.QuickCheck ()
import           LedgerState (esAccountState, esLState, esPp, nesEs, _reserves)
import           Slot (BlockNo (..), SlotNo (..))
import           STS.Chain (chainEpochNonce, chainHashHeader, chainNes, chainSlotNo)
import           STS.Ledgers (LedgersEnv (..))

genBlock
  :: SlotNo
  -> ChainState
  -> [(CoreKeyPair, AllPoolKeys)] -- core node keys
  -> Gen Block
genBlock _slotNo chainSt coreNodeKeys =
  mkBlock
    <$> pure (chainHashHeader chainSt)
    <*> pure issuerKeys
    <*> toList <$> genTxs
    <*> genNextSlot
    <*> pure chainDifficulty
    <*> pure (chainEpochNonce chainSt)
    <*> genBlockNonce
    <*> genPraosLeader
    <*> pure kesPeriod
  where
    -- TODO @uroboros pick the first core node each time for now
    issuerKeys = snd (coreNodeKeys !! 0)

    -- TODO @uroboros "90 days of slots" (constant)
    kesPeriod = 0

    -- TODO @uroboros
    genPraosLeader = pure zero

    chainDifficulty = BlockNo 1 -- used only in consensus

    -- we assume small gaps in slot numbers
    genNextSlot = SlotNo . (unSlotNo (chainSlotNo chainSt) +) <$> choose (1, 10)

    genBlockNonce = NatNonce <$> genNatural 1 100

    genTxs = do
      let ledgerSt = (esLState . nesEs . chainNes) chainSt
          pParams = (esPp . nesEs . chainNes) chainSt
          reserves = (_reserves . esAccountState . nesEs . chainNes) chainSt
          ledgerEnv = LedgersEnv (chainSlotNo chainSt) pParams reserves

      n <- choose (1, 10)
      sigGen @LEDGERS (n :: Word64) ledgerEnv ledgerSt
