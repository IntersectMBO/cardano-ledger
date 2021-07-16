{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Byron.API
  ( genApplyMempoolPayloadErr,
    ts_mempoolValidation,
    ts_roundTripApplyMempoolPayloadErrCompat,
    ts_scheduledDelegations,
    tests,
  )
where

import qualified Byron.Spec.Chain.STS.Block as STS
import Byron.Spec.Chain.STS.Rule.Chain (CHAIN)
import qualified Byron.Spec.Chain.STS.Rule.Epoch as STS
import qualified Byron.Spec.Chain.STS.Rule.SigCnt as STS
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.STS.UTXO as STS
import qualified Byron.Spec.Ledger.STS.UTXOW as STS
import qualified Byron.Spec.Ledger.Update as Spec
import Cardano.Binary (fromCBOR, toCBOR)
import Cardano.Chain.Block
  ( BlockValidationMode (..),
    ChainValidationError (..),
    ChainValidationState (..),
    HeaderHash,
    initialChainValidationState,
  )
import Cardano.Chain.Byron.API
  ( ApplyMempoolPayloadErr (..),
    applyChainTick,
    applyMempoolPayload,
    getDelegationMap,
    previewDelegationMap,
    reAnnotateUsing,
    validateBlock,
  )
import Cardano.Chain.Genesis (configSlotSecurityParam)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)
import Cardano.Chain.Slotting (SlotCount (..), SlotNumber (..))
import Cardano.Chain.UTxO (TxValidationMode (..))
import Cardano.Chain.ValidationMode (ValidationMode (..))
import Cardano.Crypto (ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as H
import Cardano.Prelude
import qualified Control.State.Transition as STS
import qualified Control.State.Transition.Generator as STS
import qualified Control.State.Transition.Trace as STS
import Data.Coerce (coerce)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hedgehog (Gen, Group (..), annotateShow, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Binary.Helpers.GoldenRoundTrip (roundTripsCBORShow)
import Test.Cardano.Chain.Block.Model (elaborateAndUpdate, elaborateBlock)
import qualified Test.Cardano.Chain.Delegation.Gen as Dlg
import Test.Cardano.Chain.Elaboration.Block
  ( AbstractToConcreteIdMaps (..),
    abEnvToCfg,
  )
import Test.Cardano.Chain.Elaboration.Delegation (elaborateDCert)
import Test.Cardano.Chain.Elaboration.Update (elaborateUpdateProposal, elaborateVote)
import Test.Cardano.Chain.UTxO.Gen (genUTxOValidationError)
import Test.Cardano.Chain.UTxO.Model
  ( elaborateInitialUTxO,
    elaborateTxWitsBSWithMap,
  )
import qualified Test.Cardano.Chain.Update.Gen as UpdateIface
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TSGroup, TSProperty, eachOfTS, withTestsTS)

tests :: TSGroup
tests scenario =
  Group
    "Test.Cardano.Chain.Byron.API"
    [ ("ts_chainTick", ts_chainTick scenario),
      ("ts_roundTripApplyMempoolPayloadErrCompat", ts_roundTripApplyMempoolPayloadErrCompat scenario),
      ("ts_scheduledDelegations", ts_scheduledDelegations scenario),
      ("ts_mempoolValidation", ts_mempoolValidation scenario)
    ]

ts_roundTripApplyMempoolPayloadErrCompat :: TSProperty
ts_roundTripApplyMempoolPayloadErrCompat =
  eachOfTS
    20
    (feedPM genApplyMempoolPayloadErr)
    roundTripsCBORShow

genApplyMempoolPayloadErr :: ProtocolMagicId -> Gen ApplyMempoolPayloadErr
genApplyMempoolPayloadErr pm =
  Gen.choice
    [ MempoolTxErr <$> genUTxOValidationError,
      MempoolDlgErr <$> Dlg.genError,
      MempoolUpdateProposalErr <$> UpdateIface.genError pm,
      MempoolUpdateVoteErr <$> UpdateIface.genError pm
    ]

setupChainValidationState ::
  STS.Trace CHAIN ->
  (ChainValidationState, Genesis.Config, AbstractToConcreteIdMaps)
setupChainValidationState sampleTrace =
  let chainEnv@(_, abstractInitialUTxO, _, _, _) = STS._traceEnv sampleTrace
      config = abEnvToCfg chainEnv
      (initialUTxO, txIdMap) = elaborateInitialUTxO abstractInitialUTxO
      initialAbstractToConcreteIdMaps = mempty {transactionIds = txIdMap}
      initialStateNoUTxO = either (panic . show) identity $ initialChainValidationState config
      initialState = initialStateNoUTxO {cvsUtxo = initialUTxO}
      (cvs, abstractToConcreteIdMaps) =
        either (panic . show) identity $
          foldM
            (elaborateAndUpdate config)
            (initialState, initialAbstractToConcreteIdMaps)
            (STS.preStatesAndSignals STS.OldestFirst sampleTrace)
   in (cvs, config, abstractToConcreteIdMaps)

-- | getDelegationMap . applyChainTick slot == previewDelegationMap slot
ts_scheduledDelegations :: TSProperty
ts_scheduledDelegations = withTestsTS 100 . property $ do
  let traceLength = 10 :: Word64
  sampleTrace <- forAll $ STS.trace @CHAIN () traceLength
  let (cvs, config, _) = setupChainValidationState sampleTrace
      n = unSlotNumber . cvsLastSlot $ cvs
      k = unSlotCount . configSlotSecurityParam $ config
  slotNumber <- forAll $ SlotNumber <$> Gen.word64 (Range.linear n (n + 2 * k - 1))
  let tickedDelegationMap = getDelegationMap $ applyChainTick config slotNumber cvs
      anachronisticDelegationMap = previewDelegationMap slotNumber cvs
  tickedDelegationMap === anachronisticDelegationMap

-- | Given three slots, a < b < c, ticking from a to b and then b to c
-- | should be the same as ticking from a to c.
ts_chainTick :: TSProperty
ts_chainTick = withTestsTS 100 . property $ do
  let traceLength = 10 :: Word64
  sampleTrace <- forAll $ STS.trace @CHAIN () traceLength
  let (cvs, config, _) = setupChainValidationState sampleTrace
      n0 = unSlotNumber . cvsLastSlot $ cvs
      k = unSlotCount . configSlotSecurityParam $ config
  n2 <- forAll $ Gen.word64 (Range.linear n0 (n0 + 2 * k))
  n1 <- forAll $ Gen.word64 (Range.linear n0 n2)
  let tick n = applyChainTick config (SlotNumber n)
  (tick n2 . tick n1) cvs === tick n2 cvs

-- | A transaction should validate in the mempool at a given slot when
--   it validates in a block issued for that same slot.
ts_mempoolValidation :: TSProperty
ts_mempoolValidation = withTestsTS 100 . property $ do
  let traceLength = 10 :: Word64
  sampleTrace <- forAll $ STS.trace @CHAIN () traceLength
  let (Spec.Slot slot, utxo0, _, _, blockCount) = STS._traceEnv sampleTrace
      abstractChainState@(_stateSlot, allowedDelegators, h, utxoState, diState, upiState) =
        STS.lastState sampleTrace
      (cvs, config, abstractToConcreteIdMaps) = setupChainValidationState sampleTrace
      pm = Genesis.configProtocolMagicId config
      txIdMap = transactionIds abstractToConcreteIdMaps
      upIdMap = proposalIds abstractToConcreteIdMaps
  genSlot <- forAll $ (slot +) <$> Gen.integral (Range.linear 1 20)
  let nextSlot = Spec.Slot genSlot
      pparams = Spec.protocolParameters upiState
      utxoEnv = STS.UTxOEnv utxo0 pparams
  transaction <- forAll $ STS.sigGen @STS.UTXOW utxoEnv utxoState
  let txAux = fst $ elaborateTxWitsBSWithMap txIdMap transaction
      mempoolTx = MempoolTx txAux

  let dsEnv =
        Spec.DSEnv
          { Spec._dSEnvAllowedDelegators = Set.fromList . toList $ allowedDelegators,
            Spec._dSEnvEpoch = STS.sEpoch nextSlot blockCount,
            Spec._dSEnvSlot = nextSlot,
            Spec._dSEnvK = blockCount
          }
  dcert <- forAll $ head <$> Spec.dcertsGen dsEnv diState
  let mempoolDCert = addAnnotation . MempoolDlg . elaborateDCert pm <$> dcert

  let upiEnv :: Spec.UPIEnv
      upiEnv =
        ( nextSlot,
          Spec._dIStateDelegationMap diState,
          blockCount,
          fromIntegral $ length allowedDelegators
        )
  (uProp, vote) <- forAll $ fmap (take 1) <$> Spec.updateProposalAndVotesGen upiEnv upiState

  let mempoolUpdatePropAndVote = case uProp of
        Nothing -> []
        Just up ->
          let up' = elaborateUpdateProposal pm up
              upIdMap' = Map.insert (Spec._upId up) (H.serializeCborHash up') upIdMap
              vote' = elaborateVote pm upIdMap' <$> vote
           in addAnnotation
                <$> [MempoolUpdateProposal up']
                <> (MempoolUpdateVote <$> vote')

  let mempoolPayloads =
        [mempoolTx]
          <> mempoolUpdatePropAndVote
          <> maybeToList mempoolDCert

  issuerKey <-
    let env :: STS.Environment STS.SIGCNT
        env =
          ( pparams,
            Spec._dIStateDelegationMap diState,
            blockCount
          )
     in forAll $ STS.issuer env allowedDelegators
  aBlockVersion <- forAll $ Spec.protocolVersionEndorsementGen upiEnv upiState
  let abstractBlock =
        STS.mkBlock
          h
          nextSlot
          issuerKey
          aBlockVersion
          (maybeToList dcert)
          uProp
          vote
          [transaction]
      concreteBlock =
        elaborateBlock
          config
          cvs
          abstractToConcreteIdMaps
          abstractChainState
          abstractBlock

  let validationMode = ValidationMode BlockValidation TxValidation
      apply1 ::
        ChainValidationState ->
        AMempoolPayload ByteString ->
        Either ApplyMempoolPayloadErr ChainValidationState
      apply1 c mp = applyMempoolPayload validationMode config (SlotNumber genSlot) mp c
      headerHash = coerce (H.serializeCborHash (0 :: Int)) :: HeaderHash
      applyMempoolPayloadResult = foldM apply1 cvs mempoolPayloads
      validateBlockResult =
        validateBlock config validationMode concreteBlock headerHash cvs ::
          Either ChainValidationError ChainValidationState

  annotateShow applyMempoolPayloadResult
  annotateShow validateBlockResult
  isRight validateBlockResult === isRight applyMempoolPayloadResult
  where
    addAnnotation :: MempoolPayload -> AMempoolPayload ByteString
    addAnnotation = reAnnotateUsing toCBOR fromCBOR
