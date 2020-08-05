{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Generator.Block
  ( genBlock,
  )
where

import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Iterate.SetAlgebra (dom, eval, range)
import Control.State.Transition.Extended (TRC (..))
import Control.State.Transition.Trace.Generator.QuickCheck (sigGen)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Data.List as List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API
  ( Block,
    ChainState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    GenDelegPair,
    LEDGERS,
    LedgerState (..),
    NewEpochState (..),
    OBftSlot (..),
    OCertEnv (..),
    PParams' (..),
    TICK,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( activeSlotCoeff,
    activeSlotVal,
    intervalValue,
    (⭒),
  )
import Shelley.Spec.Ledger.BlockChain (LastAppliedBlock (..))
import Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..), PoolDistr (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyHash,
    KeyRole (..),
    coerceKeyRole,
    genDelegKeyHash,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( getGKeys,
    overlaySchedule,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..), currentIssueNo, kesPeriod)
import Shelley.Spec.Ledger.STS.Ledgers (LedgersEnv (..))
import Shelley.Spec.Ledger.STS.Tick (TickEnv (..))
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC (choose, shuffle)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    GenEnv (..),
    KeySpace (..),
    NatNonce (..),
    genNatural,
    genWord64,
    getKESPeriodRenewalNo,
    mkBlock,
    mkOCert,
  )
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger ()
import Test.Shelley.Spec.Ledger.Utils
  ( applySTSTest,
    maxKESIterations,
    runShelleyBase,
    testGlobals,
    unsafeMkUnitInterval,
  )

nextCoreNode ::
  Map SlotNo (OBftSlot c) ->
  Map SlotNo (OBftSlot c) ->
  SlotNo ->
  (SlotNo, KeyHash 'Genesis c)
nextCoreNode os nextOs s =
  let getNextOSlot os' =
        let (_, nextSlots) = Map.split s os'
         in listToMaybe [(slot, k) | (slot, ActiveSlot k) <- Map.toAscList nextSlots]
   in case getNextOSlot os of
        Nothing ->
          -- there are no more active overlay slots this epoch
          case getNextOSlot nextOs of
            Nothing -> error "TODO - handle d=0"
            Just n -> n
        Just n -> n

-- | Find the next active Praos slot.
getPraosSlot ::
  SlotNo ->
  SlotNo ->
  Map SlotNo (OBftSlot c) ->
  Map SlotNo (OBftSlot c) ->
  Maybe SlotNo
getPraosSlot start tooFar os nos =
  let schedules = os `Map.union` nos
   in List.find (not . (`Map.member` schedules)) [start .. tooFar -1]

genBlock ::
  forall c.
  Mock c =>
  GenEnv c ->
  ChainState c ->
  Gen (Block c)
genBlock
  ge@(GenEnv KeySpace_ {ksStakePools, ksIndexedGenDelegates} _)
  chainSt = do
    let os = (nesOsched . chainNes) chainSt
        dpstate = (_delegationState . esLState . nesEs . chainNes) chainSt
        pp = (esPp . nesEs . chainNes) chainSt
        (EpochNo e) = (nesEL . chainNes) chainSt
        (GenDelegs cores) = (_genDelegs . _dstate) dpstate
        nextOs =
          runShelleyBase $
            overlaySchedule
              (EpochNo $ e + 1)
              (Map.keysSet cores)
              pp
        (nextOSlot, gkh) = nextCoreNode os nextOs slot

    {- Our slot selection strategy uses the overlay schedule.
     - Above we calculated the next available core node slot
     - Note that we will need to do something different
     - when we start allowing d=0, and there is no such next core slot.
     - If there are no current stake pools, as determined by the pd mapping
     - (pools to relative stake), then we take the next core node slot.
     - Note that the mapping of registered stake pools is different, ie
     - the one in PState, since news pools will not yet be a part of
     - a snapshot and are therefore not yet ready to make blocks.
     - Otherwise, if there are active pools, we generate a small increase
     - from the current slot, and then take the first slot from this point
     - that is either available for Praos or is a core node slot.
     -}

    lookForPraosStart <- genSlotIncrease
    let poolParams = (Map.toList . Map.filter ((> 0) . individualPoolStake) . unPoolDistr . nesPd . chainNes) chainSt
    poolParams' <- take 1 <$> QC.shuffle poolParams

    -- Look for a good future slot to make a block for.
    -- We will find a slot number, a stake value, and an Either type.
    --
    -- If we choose an overlay slot, we return this slot number,
    -- a stake value of 0 (the value here does not matter since
    -- the overlay slots are not subject to the VRF checks), and Left ghk,
    -- where gkh is the hash of the genesis key in this overlay position.
    -- We cannot lookup these genesis keys until we run TICK to see if
    -- the genesis key delegation has changed.
    --
    -- Otherwise, if we choose a Praos solt, we return the chosen slot number,
    -- and the corresponding stake pool's stake and Right AllIssuerKeys for
    -- some chose stake pool that has non-zero stake.
    let (nextSlot, poolStake, ks) = case poolParams' of
          [] -> (nextOSlot, 0, Left gkh)
          (pkh, IndividualPoolStake stake _) : _ -> case getPraosSlot lookForPraosStart nextOSlot os nextOs of
            Nothing -> (nextOSlot, 0, Left gkh)
            Just ps ->
              let apks =
                    fromMaybe
                      (error "Cannot find stake pool key")
                      $ List.find
                        (\x -> hk x == pkh)
                        ksStakePools
               in (ps, stake, Right apks)

    let kp@(KESPeriod kesPeriod_) = runShelleyBase $ kesPeriod nextSlot
        cs = chainOCertIssue chainSt

    -- ran genDelegs
    let nes = chainNes chainSt
        nes' = runShelleyBase $ applySTSTest @(TICK c) $ TRC (TickEnv (getGKeys nes), nes, nextSlot)

    case nes' of
      Left pf -> error ("genBlock TICK rule failed - " <> show pf)
      Right _nes' -> do
        let NewEpochState _ _ _ es _ _ _ = _nes'
            EpochState acnt _ ls _ pp' _ = es
            GenDelegs gds = _genDelegs . _dstate . _delegationState . esLState . nesEs $ _nes'
            -- Keys need to be coerced to block issuer keys
            keys :: AllIssuerKeys c 'BlockIssuer
            keys = case ks of
              -- We chose an overlay slot, and need to lookup the given
              -- keys from the genesis key hash.
              Left ghk -> coerce gkeys ghk gds
              -- We chose a Praos slot, and have everything we need.
              Right ks' -> coerce ks'
            genesisVKHs = Set.map genDelegKeyHash $ range gds
            n' =
              currentIssueNo
                (OCertEnv (eval (dom poolParams)) genesisVKHs)
                cs
                ((coerceKeyRole . hashKey . vKey . cold) keys)
            m = getKESPeriodRenewalNo keys kp
            hotKeys = drop (fromIntegral m) (hot keys)
            keys' = keys {hot = hotKeys}
            issueNumber =
              if n' == Nothing
                then error "no issue number available"
                else fromIntegral m
            oCert = mkOCert keys' issueNumber ((fst . head) hotKeys)
            epochNonce =
              if (nesEL nes) < (nesEL _nes') -- if it is a new epoch
                then (chainCandidateNonce chainSt) ⭒ (chainPrevEpochNonce chainSt) ⭒ (_extraEntropy pp')
                else chainEpochNonce chainSt

        mkBlock
          <$> pure hashheader
          <*> pure keys'
          <*> toList
          <$> genTxs pp' acnt ls nextSlot
          <*> pure nextSlot
          <*> pure (block + 1)
          <*> pure epochNonce
          <*> genBlockNonce
          <*> genPraosLeader poolStake
          <*> pure kesPeriod_
          -- This seems to be trying to work out the start of the KES "era", e.g. the KES period in which this key starts to be valid.
          <*> pure (fromIntegral (m * fromIntegral maxKESIterations))
          <*> pure oCert
    where
      (block, slot, hashheader) = case chainLastAppliedBlock chainSt of
        Origin -> error "block generator does not support from Origin"
        At (LastAppliedBlock b s hh) -> (b, s, hh)
      gkeys ::
        KeyHash 'Genesis c ->
        Map (KeyHash 'Genesis c) (GenDelegPair c) ->
        AllIssuerKeys c 'GenesisDelegate
      gkeys gkey gds =
        fromMaybe
          (error "genBlock: lookup of Genesis Delegate cold key hash failed")
          ( Map.lookup gkey gds
              >>= (flip Map.lookup) ksIndexedGenDelegates . genDelegKeyHash
          )
      genPraosLeader stake =
        if stake >= 0 && stake <= 1
          then do
            -- we subtract one from the numerator for a non-zero stake e.g. for a
            -- stake of 3/20, we would go with 2/20 and then divide by a random
            -- integer in [1,10]. This value is guaranteed to be below the ϕ
            -- function for the VRF value comparison and generates a valid leader
            -- value for Praos.
            let (stNumer, stDenom) = (fromIntegral $ numerator stake, fromIntegral $ denominator stake)
            let stake' =
                  if stake > 0
                    then (stNumer - 1) % stDenom
                    else stNumer % stDenom
                asc = activeSlotCoeff testGlobals
            n <- genWord64 1 10
            pure
              ( unsafeMkUnitInterval
                  ( (stake' / fromIntegral n)
                      * ((intervalValue . activeSlotVal) asc)
                  )
              )
          else error "stake not in [0; 1]"
      -- we assume small gaps in slot numbers
      genSlotIncrease = SlotNo . (lastSlotNo +) <$> QC.choose (1, 5)
      lastSlotNo = unSlotNo slot
      genBlockNonce = NatNonce <$> genNatural 1 100
      genTxs pp reserves ls s = do
        let ledgerEnv = LedgersEnv s pp reserves

        sigGen @(LEDGERS c) ge ledgerEnv ls
