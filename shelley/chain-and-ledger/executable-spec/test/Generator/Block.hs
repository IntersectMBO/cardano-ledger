{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Generator.Block
  ( genBlock
  )
  where

import           Data.Foldable (toList)
import qualified Data.List as List (find)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC (choose, discard, shuffle)

import           ConcreteCryptoTypes (Block, ChainState, CoreKeyPair, GenKeyHash, KeyHash, KeyPair,
                     LEDGERS, TICK)
import           Control.State.Transition.Extended (TRC (..), applySTS)
import           Control.State.Transition.Trace.Generator.QuickCheck (sigGen)
import           Delegation.Certificates (PoolDistr (..))
import           Generator.Core.QuickCheck (AllPoolKeys (..), NatNonce (..), genNatural,
                     getKESPeriodRenewalNo, mkBlock, mkOCert, traceVRFKeyPairsByHash, zero)
import           Generator.LedgerTrace.QuickCheck ()
import           Keys (GenDelegs (..), hashKey, vKey)
import           Ledger.Core (dom, range)
import           LedgerState (pattern EpochState, pattern NewEpochState, esAccountState, esLState,
                     esPp, getGKeys, nesEL, nesEs, nesOsched, nesPd, overlaySchedule,
                     _delegationState, _dstate, _genDelegs, _reserves)
import           OCert (KESPeriod (..), currentIssueNo, kesPeriod)
import           Slot (EpochNo (..), SlotNo (..))
import           STS.Chain (chainBlockNo, chainEpochNonce, chainHashHeader, chainNes,
                     chainOCertIssue, chainSlotNo)
import           STS.Ledgers (LedgersEnv (..))
import           STS.Ocert (pattern OCertEnv)
import           STS.Tick (TickEnv (..))
import           Test.Utils (maxKESIterations, runShelleyBase)

nextCoreNode
  :: Map SlotNo (Maybe GenKeyHash)
  -> Map SlotNo (Maybe GenKeyHash)
  -> SlotNo
  -> (SlotNo, GenKeyHash)
nextCoreNode os nextOs s =
  let getNextOSlot os' =
        let osGen = Map.toAscList $ Map.mapMaybe id os'
            nextSlots = filter ((> s) . fst) osGen
        in if null nextSlots then Nothing else Just $ head nextSlots
  in
  case getNextOSlot os of
    Nothing -> -- there are no more active overlay slots this epoch
      case getNextOSlot nextOs of
        Nothing -> error "TODO - handle d=0"
        Just n -> n
    Just n -> n

-- | Find the next active Praos slot.
getPraosSlot
  :: SlotNo
  -> SlotNo
  -> Map SlotNo (Maybe GenKeyHash)
  -> Map SlotNo (Maybe GenKeyHash)
  -> Maybe SlotNo
getPraosSlot start tooFar os nos =
  let schedules = os `Map.union` nos
  in List.find (not . (`Map.member` schedules)) [start .. tooFar-1]

genBlock
  :: SlotNo
  -> ChainState
  -> [(CoreKeyPair, AllPoolKeys)] -- core node keys
  -> Map KeyHash KeyPair -- indexed keys By StakeHash
  -> Gen Block
genBlock sNow chainSt coreNodeKeys keysByStakeHash = do
  let os = (nesOsched . chainNes) chainSt
      s = chainSlotNo chainSt
      dpstate = (_delegationState . esLState . nesEs . chainNes) chainSt
      pp = (esPp . nesEs . chainNes) chainSt
      (EpochNo e) = (nesEL . chainNes) chainSt
      (GenDelegs cores) = (_genDelegs . _dstate) dpstate
      nextOs = runShelleyBase $ overlaySchedule
        (EpochNo $ e + 1) (Map.keysSet cores) pp
      (nextOSlot, gkey) = nextCoreNode os nextOs s

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
  let poolParams = (Map.toList . Map.filter ((> 0) . fst) . unPoolDistr . nesPd . chainNes) chainSt
  poolParams' <- take 1 <$> QC.shuffle poolParams
  let (nextSlot, keys) = case poolParams' of
        []       -> (nextOSlot, gkeys gkey)
        (pkh, (_, vrfkey)):_ -> case getPraosSlot lookForPraosStart nextOSlot os nextOs of
                      Nothing -> (nextOSlot, gkeys gkey)
                      Just ps -> let apks = AllPoolKeys
                                       { cold = (keysByStakeHash Map.! pkh)
                                       , vrf  = (traceVRFKeyPairsByHash Map.! vrfkey)
                                       , hot  = (hot $ gkeys gkey)
                                                -- ^^ TODO @jc - don't use the genesis hot key
                                       , hk   = pkh
                                       }
                                 in (ps, apks)

  if nextSlot > sNow
    then QC.discard
    else do

    let kp@(KESPeriod kesPeriod_) = runShelleyBase (kesPeriod $ nextSlot)
        cs = chainOCertIssue chainSt

        -- ran genDelegs
        genDelegationKeys = range cores

        n' = currentIssueNo
             (OCertEnv (dom poolParams) genDelegationKeys)
             cs
             ((hashKey . vKey . cold) keys)

        m = getKESPeriodRenewalNo keys kp

        hotKeys = drop (fromIntegral m) (hot keys)
        keys' = keys { hot = hotKeys }
        oCert =
          case n' of
            Nothing -> error "no issue number available"
            Just _ ->
              mkOCert keys' (fromIntegral m) ((fst . head) hotKeys)

    let nes  = chainNes chainSt
        nes' = runShelleyBase $ (applySTS @TICK $ TRC (TickEnv (getGKeys nes), nes, nextSlot))

    case nes' of
      Left _ -> QC.discard
      Right _nes' -> do
        let NewEpochState _ _ _ es _ _ _ = _nes'
            EpochState _ _ ls _          = es
        mkBlock
          <$> pure (chainHashHeader chainSt)
          <*> pure keys'
          <*> toList <$> genTxs ls nextSlot
          <*> pure nextSlot
          <*> pure (chainBlockNo chainSt + 1)
          <*> pure (chainEpochNonce chainSt)
          <*> genBlockNonce
          <*> genPraosLeader
          <*> pure kesPeriod_
          <*> pure (fromIntegral (m * fromIntegral maxKESIterations))
          <*> pure oCert
  where
    ledgerSt = (esLState . nesEs . chainNes) chainSt
    (GenDelegs genesisDelegs) = (_genDelegs . _dstate . _delegationState) ledgerSt

    origIssuerKeys h = case List.find (\(k, _) -> (hashKey . vKey) k == h) coreNodeKeys of
                         Nothing -> error "couldn't find corresponding core node key"
                         Just k  -> snd k
    gkeys gkey =
        case Map.lookup gkey genesisDelegs of
          Nothing ->
            error "genBlock: NoGenesisStakingOVERLAY"
          Just gKeyHash ->
            -- if GenesisDelegate certs changed a delegation to a new key
            case Map.lookup gKeyHash keysByStakeHash of
              Nothing ->
                -- then we use the original keys (which have not been changed by a genesis delegation)
                origIssuerKeys gkey
              Just updatedCold ->
                -- if we find the pre-hashed key in keysByStakeHash, we use it instead of the original cold key
                (origIssuerKeys gkey) {cold = updatedCold, hk = (hashKey . vKey) updatedCold}

    -- TODO @uroboros
    genPraosLeader = pure zero

    -- we assume small gaps in slot numbers
    genSlotIncrease = SlotNo . (lastSlotNo +) <$> QC.choose (1, 5)
    lastSlotNo = unSlotNo (chainSlotNo chainSt)

    genBlockNonce = NatNonce <$> genNatural 1 100

    genTxs ls s = do
      let pParams = (esPp . nesEs . chainNes) chainSt
          reserves = (_reserves . esAccountState . nesEs . chainNes) chainSt
          ledgerEnv = LedgersEnv s pParams reserves

      n <- QC.choose (1, 10)
      sigGen @LEDGERS (n :: Word64) ledgerEnv ls
