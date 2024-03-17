{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Generator.Block (
  genBlock,
  genBlockWithTxGen,
  tickChainState,
)
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BHeaderView (bhviewBSize, bhviewHSize)
import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Crypto (VRF)
import Cardano.Ledger.Shelley.API hiding (vKey)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Protocol.TPraos.API
import Cardano.Protocol.TPraos.BHeader (
  BHeader (..),
  LastAppliedBlock (..),
  hashHeaderToNonce,
  makeHeaderView,
  mkSeed,
  seedL,
 )
import Cardano.Protocol.TPraos.OCert (KESPeriod (..), OCertEnv (..), currentIssueNo, kesPeriod)
import Cardano.Protocol.TPraos.Rules.Overlay (OBftSlot (..), lookupInOverlaySchedule)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState (..))
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.SetAlgebra (dom, eval)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Data.List as List (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Core.KeyPair (vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (
  Mock,
 )
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (..),
  GenEnv (..),
  KeySpace (..),
  getKESPeriodRenewalNo,
  mkBlock,
  mkOCert,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (
  epochFromSlotNo,
  maxKESIterations,
  runShelleyBase,
  slotFromEpoch,
  testGlobals,
 )
import Test.Cardano.Protocol.TPraos.Create (VRFKeyPair (..))
import Test.Control.State.Transition.Trace.Generator.QuickCheck (sigGen)
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (Gen, discard)
import qualified Test.QuickCheck as QC (choose)

-- ======================================================

-- | Type alias for a transaction generator
type TxGen era =
  PParams era ->
  AccountState ->
  LedgerState era ->
  SlotNo ->
  Gen (Seq (Tx era))

-- | Generate a valid block.
genBlock ::
  forall era.
  ( MinLEDGER_STS era
  , ApplyBlock era
  , Mock (EraCrypto era)
  , GetLedgerView era
  , QC.HasTrace (EraRule "LEDGERS" era) (GenEnv era)
  , EraGen era
  ) =>
  GenEnv era ->
  ChainState era ->
  Gen (Block (BHeader (EraCrypto era)) era)
genBlock ge = genBlockWithTxGen genTxs ge
  where
    genTxs :: TxGen era
    genTxs pp reserves ls s = do
      let ledgerEnv = LedgersEnv @era s pp reserves
      block <- sigGen @(EraRule "LEDGERS" era) ge ledgerEnv ls
      genEraTweakBlock @era pp block

genBlockWithTxGen ::
  forall era.
  ( Mock (EraCrypto era)
  , GetLedgerView era
  , ApplyBlock era
  , EraGen era
  ) =>
  TxGen era ->
  GenEnv era ->
  ChainState era ->
  Gen (Block (BHeader (EraCrypto era)) era)
genBlockWithTxGen
  genTxs
  ge@(GenEnv KeySpace_ {ksStakePools, ksIndexedGenDelegates} _scriptspace _)
  origChainState = do
    -- Firstly, we must choose a slot in which to lead.
    -- Caution: the number of slots we jump here will affect the number
    -- of epochs that a chain of blocks will span
    firstConsideredSlot <- (slot +) . SlotNo <$> QC.choose (5, 10)
    let (nextSlot, chainSt, issuerKeys) =
          fromMaybe
            (error "Cannot find a slot to create a block in")
            $ selectNextSlotWithLeader ge origChainState firstConsideredSlot

    -- Now we need to compute the KES period and get the set of hot keys.
    let NewEpochState _ _ _ es _ _ _ = chainNes chainSt
        EpochState acnt ls _ _ = es
        pp = es ^. curPParamsEpochStateL
        kp@(KESPeriod kesPeriod_) = runShelleyBase $ kesPeriod nextSlot
        cs = chainOCertIssue chainSt
        m = getKESPeriodRenewalNo issuerKeys kp
        hotKeys =
          fromMaybe
            ( error $
                "No more hot keys left. Tried dropping "
                  ++ show m
                  ++ " from: "
                  ++ show (aikHot issuerKeys)
            )
            (NE.nonEmpty $ NE.drop (fromIntegral m) (aikHot issuerKeys))
        keys = issuerKeys {aikHot = hotKeys}

        -- And issue a new ocert
        n' =
          currentIssueNo
            ( OCertEnv
                (Set.fromList $ aikColdKeyHash <$> ksStakePools)
                (eval (dom ksIndexedGenDelegates))
            )
            cs
            (coerceKeyRole . hashKey . vKey $ aikCold issuerKeys)
        issueNumber =
          if n' == Nothing
            then error "no issue number available"
            else fromIntegral m
        oCert = mkOCert keys issueNumber (fst $ NE.head hotKeys)
    theBlock <-
      mkBlock
        <$> pure hashheader
        <*> pure keys
        <*> toList
        <$> genTxs pp acnt ls nextSlot
        <*> pure nextSlot
        <*> pure (block + 1)
        <*> pure (chainEpochNonce chainSt)
        <*> pure kesPeriod_
        -- This seems to be trying to work out the start of the KES "era",
        -- e.g. the KES period in which this key starts to be valid.
        <*> pure (fromIntegral (m * fromIntegral maxKESIterations))
        <*> pure oCert
    let hView = makeHeaderView (bheader theBlock)
    if bhviewBSize hView <= pp ^. ppMaxBBSizeL && bhviewHSize hView <= fromIntegral (pp ^. ppMaxBHSizeL)
      then pure theBlock
      else discard
    where
      -- This is safe to take form the original chain state, since we only tick
      -- it forward; no new blocks will have been applied.
      (block, slot, hashheader) = case chainLastAppliedBlock origChainState of
        Origin -> error "block generator does not support from Origin"
        At (LastAppliedBlock b s hh) -> (b, s, hh)

selectNextSlotWithLeader ::
  forall era.
  ( Mock (EraCrypto era)
  , EraGen era
  , GetLedgerView era
  , ApplyBlock era
  ) =>
  GenEnv era ->
  ChainState era ->
  -- Starting slot
  SlotNo ->
  Maybe (SlotNo, ChainState era, AllIssuerKeys (EraCrypto era) 'BlockIssuer)
selectNextSlotWithLeader
  (GenEnv KeySpace_ {ksStakePools, ksIndexedGenDelegates} _ _)
  origChainState
  startSlot =
    List.find (const True) . catMaybes $
      selectNextSlotWithLeaderThisEpoch
        <$> (startSlot : [slotFromEpoch x | x <- [startEpoch + 1 .. startEpoch + 4]])
    where
      -- If we can't find a leader in the next N Epochs, some thing is wrong, N=4 should be large enough.

      startEpoch = epochFromSlotNo startSlot
      selectNextSlotWithLeaderThisEpoch ::
        -- Slot number whence we begin our search
        SlotNo ->
        Maybe (SlotNo, ChainState era, AllIssuerKeys (EraCrypto era) 'BlockIssuer)
      selectNextSlotWithLeaderThisEpoch fromSlot =
        findJust selectLeaderForSlot [fromSlot .. toSlot]
        where
          currentEpoch = epochFromSlotNo fromSlot
          toSlot = slotFromEpoch (currentEpoch + 1) - 1

          findJust _ [] = Nothing
          findJust f (x : xs) = case f x of
            Just (chainSt, y) -> Just (x, chainSt, y)
            Nothing -> findJust f xs

      -- Try to select a leader for the given slot
      selectLeaderForSlot ::
        SlotNo ->
        Maybe (ChainState era, AllIssuerKeys (EraCrypto era) 'BlockIssuer)
      selectLeaderForSlot slotNo =
        (chainSt,)
          <$> case lookupInOverlaySchedule firstEpochSlot (Map.keysSet cores) d f slotNo of
            Nothing ->
              coerce
                <$> List.find
                  ( \(AllIssuerKeys {aikVrf, aikColdKeyHash}) ->
                      isLeader aikColdKeyHash (vrfSignKey aikVrf)
                  )
                  ksStakePools
            Just (ActiveSlot x) ->
              coerce $
                Map.lookup x cores
                  >>= \y -> Map.lookup (genDelegKeyHash y) ksIndexedGenDelegates
            _ -> Nothing
        where
          chainSt = tickChainState slotNo origChainState
          epochNonce = chainEpochNonce chainSt
          poolDistr = unPoolDistr . nesPd . chainNes $ chainSt
          dpstate = (lsCertState . esLState . nesEs . chainNes) chainSt
          (GenDelegs cores) = (dsGenDelegs . certDState) dpstate
          firstEpochSlot = slotFromEpoch (epochFromSlotNo slotNo)
          f = activeSlotCoeff testGlobals
          getUnitInterval :: PParams era -> UnitInterval
          getUnitInterval pp = pp ^. ppDG
          d = (getUnitInterval . view curPParamsEpochStateL . nesEs . chainNes) chainSt

          isLeader poolHash vrfKey =
            let y = VRF.evalCertified @(VRF (EraCrypto era)) () (mkSeed seedL slotNo epochNonce) vrfKey
                stake = maybe 0 individualPoolStake $ Map.lookup poolHash poolDistr
             in case lookupInOverlaySchedule firstEpochSlot (Map.keysSet cores) d f slotNo of
                  Nothing -> checkLeaderValue (VRF.certifiedOutput y) stake f
                  Just (ActiveSlot x) | coerceKeyRole x == poolHash -> True
                  _ -> False

-- | The chain state is a composite of the new epoch state and the chain dep
-- state. We tick both.
tickChainState ::
  (GetLedgerView era, ApplyBlock era) =>
  SlotNo ->
  ChainState era ->
  ChainState era
tickChainState
  slotNo
  ChainState
    { chainNes
    , chainOCertIssue
    , chainEpochNonce
    , chainEvolvingNonce
    , chainCandidateNonce
    , chainPrevEpochNonce
    , chainLastAppliedBlock
    } =
    let cds =
          ChainDepState
            { csProtocol = PrtclState chainOCertIssue chainEvolvingNonce chainCandidateNonce
            , csTickn = TicknState chainEpochNonce chainPrevEpochNonce
            , csLabNonce = case chainLastAppliedBlock of
                Origin -> NeutralNonce
                At (LastAppliedBlock {labHash}) -> hashHeaderToNonce labHash
            }
        lv = either (error . show) id $ futureLedgerView testGlobals chainNes slotNo
        isNewEpoch = epochFromSlotNo slotNo /= nesEL chainNes
        ChainDepState {csProtocol, csTickn} =
          tickChainDepState testGlobals lv isNewEpoch cds
        PrtclState ocertIssue evNonce candNonce = csProtocol
        nes' = applyTick testGlobals chainNes slotNo
     in ChainState
          { chainNes = nes'
          , chainOCertIssue = ocertIssue
          , chainEpochNonce = ticknStateEpochNonce csTickn
          , chainEvolvingNonce = evNonce
          , chainCandidateNonce = candNonce
          , chainPrevEpochNonce = ticknStatePrevHashNonce csTickn
          , chainLastAppliedBlock = chainLastAppliedBlock
          }
