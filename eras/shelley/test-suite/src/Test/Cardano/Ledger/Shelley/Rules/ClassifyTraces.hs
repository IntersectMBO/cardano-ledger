{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces (
  onlyValidLedgerSignalsAreGenerated,
  onlyValidChainSignalsAreGenerated,
  relevantCasesAreCovered,
  propAbstractSizeBoundsBytes,
  propAbstractSizeNotTooBig,
) where

import Cardano.Ledger.BaseTypes (Globals, StrictMaybe (..), epochInfoPure)
import Cardano.Ledger.Binary.Plain as Plain (serialize')
import Cardano.Ledger.Block (Block (..), bheader)
import Cardano.Ledger.Shelley.API (
  Addr (..),
  Credential (..),
  ShelleyLEDGER,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Shelley.PParams (
  Update (..),
  pattern ProposedPPUpdates,
  pattern Update,
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.TxCert (
  isDelegation,
  isGenesisDelegation,
  isRegPool,
  isReservesMIRCert,
  isRetirePool,
  isTreasuryMIRCert,
 )
import Cardano.Ledger.Slot (SlotNo (..), epochInfoSize)
import Cardano.Protocol.TPraos.BHeader (
  BHeader,
  bhbody,
  bheaderSlotNo,
 )
import Cardano.Slotting.Slot (EpochSize (..))
import Control.State.Transition (STS (State))
import Control.State.Transition.Extended (Environment, Signal)
import qualified Data.ByteString as BS
import Data.Foldable (foldMap', toList)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Semigroup (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (mkGenesisLedgerState)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Utils
import Test.Control.State.Transition.Trace (
  Trace,
  TraceOrder (OldestFirst),
  traceSignals,
 )
import Test.Control.State.Transition.Trace.Generator.QuickCheck (
  forAllTraceFromInitState,
  onlyValidSignalsAreGeneratedFromInitState,
  traceFromInitState,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Confidence (certainty),
  Property,
  checkCoverageWith,
  conjoin,
  cover,
  forAllBlind,
  property,
  stdConfidence,
  withMaxSuccess,
 )
import Test.Tasty (TestTree)
import qualified Test.Tasty.QuickCheck as TQC

-- =================================================================

relevantCasesAreCovered ::
  forall era.
  ( EraGen era
  , EraStake era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  Int ->
  TestTree
relevantCasesAreCovered n =
  TQC.testProperty
    "Chain and Ledger traces cover the relevant cases"
    (TQC.withMaxSuccess n prop)
  where
    prop = do
      let tl = 100
      checkCoverageWith stdConfidence {certainty = 1_000_000} $
        forAllBlind
          ( traceFromInitState @(CHAIN era)
              testGlobals
              tl
              (genEnv @era @MockCrypto p defaultConstants)
              genesisChainSt
          )
          relevantCasesAreCoveredForTrace
    p :: Proxy era
    p = Proxy
    genesisChainSt = Just $ mkGenesisChainState (genEnv @era @MockCrypto p defaultConstants)

relevantCasesAreCoveredForTrace ::
  forall era.
  ( ChainProperty era
  , ShelleyEraTxBody era
  ) =>
  Trace (CHAIN era) ->
  Property
relevantCasesAreCoveredForTrace tr = do
  let blockTxs :: Block (BHeader MockCrypto) era -> [Tx era]
      blockTxs (Block _ blockBody) = toList $ blockBody ^. txSeqBlockBodyL
      bs = traceSignals OldestFirst tr
      txs = concatMap blockTxs bs
      certsByTx_ = certsByTx @era txs
      certs_ = concat certsByTx_

      classifications =
        [
          ( "there is at least 1 certificate for every 2 transactions"
          , length txs < 2 * length certs_
          , 60
          )
        ,
          ( "there is at least 1 RegKey certificate for every 10 transactions"
          , length txs < 10 * length (filter isRegStakeTxCert certs_)
          , 60
          )
        ,
          ( "there is at least 1 DeRegKey certificate for every 20 transactions"
          , length txs < 20 * length (filter isUnRegStakeTxCert certs_)
          , 60
          )
        ,
          ( "there is at least 1 Delegation certificate for every 10 transactions"
          , length txs < 10 * length (filter isDelegation certs_)
          , 60
          )
        ,
          ( "there is at least 1 Genesis Delegation certificate for every 20 transactions"
          , length txs < 20 * length (filter isGenesisDelegation certs_)
          , 60
          )
        ,
          ( "there is at least 1 RetirePool certificate for every 10 transactions"
          , length txs < 10 * length (filter isRetirePool certs_)
          , 60
          )
        ,
          ( "there is at least 1 MIR certificate (spending Reserves) for every 60 transactions"
          , length txs < 60 * length (filter isReservesMIRCert certs_)
          , 40
          )
        ,
          ( "there is at least 1 MIR certificate (spending Treasury) for every 60 transactions"
          , length txs < 60 * length (filter isTreasuryMIRCert certs_)
          , 40
          )
        ,
          ( "there is at least 1 RegPool certificate for every 10 transactions"
          , length txs < 10 * length (filter isRegPool certs_)
          , 60
          )
        ,
          ( "at least 10% of TxOuts are scripts"
          , 0.1 < txScriptOutputsRatio (view outputsTxBodyL . view bodyTxL <$> txs)
          , 20
          )
        ,
          ( "at least 10% of `ShelleyTxCertDeleg` certificates have script credentials"
          , 0.1 < scriptCredentialCertsRatio certs_
          , 60
          )
        ,
          ( "at least 1 in 10 transactions have a reward withdrawal"
          , length txs < 10 * length (filter (hasWithdrawal @era) txs)
          , 60
          )
        ,
          ( "at least 1 in 20 transactions have non-trivial protocol param updates"
          , length txs < 20 * length (filter (hasPParamUpdate @era) txs)
          , 60
          )
        ,
          ( "at least 1 in 20 transactions have metadata"
          , length txs < 20 * length (filter (hasMetadata @era) txs)
          , 60
          )
        ,
          ( "at least 5 epochs in a trace, 20% of the time"
          , 5 <= epochsInTrace bs
          , 20
          )
        ]

  conjoin $ cover_ <$> classifications
  where
    cover_ (label, predicate, coveragePc) =
      cover coveragePc predicate label (property ())

-- | Ratio of certificates with script credentials to the number of certificates
-- that could have script credentials.
scriptCredentialCertsRatio :: ShelleyEraTxCert c => [TxCert c] -> Double
scriptCredentialCertsRatio certs =
  ratioInt haveScriptCerts couldhaveScriptCerts
  where
    haveScriptCerts =
      length $
        filter
          ( \case
              RegTxCert (ScriptHashObj _) -> True
              UnRegTxCert (ScriptHashObj _) -> True
              DelegStakeTxCert (ScriptHashObj _) _ -> True
              _ -> False
          )
          certs
    couldhaveScriptCerts =
      length $
        filter
          ( \case
              RegTxCert _ -> True
              UnRegTxCert _ -> True
              DelegStakeTxCert _ _ -> True
              _ -> False
          )
          certs

-- | Extract the certificates from the transactions
certsByTx ::
  ( ShelleyEraTxBody era
  , EraTx era
  ) =>
  [Tx era] ->
  [[TxCert era]]
certsByTx txs = toList . view certsTxBodyL . view bodyTxL <$> txs

ratioInt :: Int -> Int -> Double
ratioInt x y =
  fromIntegral x / fromIntegral y

-- | Transaction has script locked TxOuts
txScriptOutputsRatio ::
  forall era.
  EraTxOut era =>
  [StrictSeq (TxOut era)] ->
  Double
txScriptOutputsRatio txoutsList =
  ratioInt
    (sum (map countScriptOuts txoutsList))
    (sum (map length txoutsList))
  where
    countScriptOuts :: StrictSeq (TxOut era) -> Int
    countScriptOuts =
      getSum
        . foldMap'
          ( \out -> case out ^. addrTxOutL of
              Addr _ (ScriptHashObj _) _ -> Sum 1
              _ -> Sum 0
          )

hasWithdrawal :: (ShelleyEraTxBody era, EraTx era) => Tx era -> Bool
hasWithdrawal tx = not . null $ unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)

hasPParamUpdate :: (ShelleyEraTxBody era, EraTx era) => Tx era -> Bool
hasPParamUpdate tx = ppUpdates (tx ^. bodyTxL . updateTxBodyL)
  where
    ppUpdates SNothing = False
    ppUpdates (SJust (Update (ProposedPPUpdates ppUpd) _)) = Map.size ppUpd > 0

hasMetadata :: EraTx era => Tx era -> Bool
hasMetadata tx = f (tx ^. bodyTxL . auxDataHashTxBodyL)
  where
    f SNothing = False
    f (SJust _) = True

onlyValidLedgerSignalsAreGenerated ::
  forall era ledger.
  ( EraGen era
  , EraStake era
  , QC.HasTrace ledger (GenEnv MockCrypto era)
  , QC.BaseEnv ledger ~ Globals
  , State ledger ~ LedgerState era
  , Show (Environment ledger)
  , Show (Signal ledger)
  , EraGov era
  ) =>
  TestTree
onlyValidLedgerSignalsAreGenerated =
  TQC.testProperty "Only valid Ledger STS signals are generated" prop
  where
    prop =
      withMaxSuccess 200 $
        onlyValidSignalsAreGeneratedFromInitState
          @ledger
          testGlobals
          100
          ge
          genesisLedgerSt
    p :: Proxy era
    p = Proxy
    ge = genEnv @era @MockCrypto p defaultConstants
    genesisLedgerSt = Just $ mkGenesisLedgerState ge

-- | Check that the abstract transaction size function
-- actually bounds the number of bytes in the serialized transaction.
propAbstractSizeBoundsBytes ::
  forall era.
  ( EraGen era
  , EraGov era
  , EraStake era
  , QC.HasTrace (ShelleyLEDGER era) (GenEnv MockCrypto era)
  ) =>
  Property
propAbstractSizeBoundsBytes = property $ do
  let tl = 100
      numBytes = toInteger . BS.length . Plain.serialize'
  forAllTraceFromInitState @(ShelleyLEDGER era)
    testGlobals
    tl
    (genEnv @era @MockCrypto p defaultConstants)
    genesisLedgerSt
    $ \tr -> do
      let txs :: [Tx era]
          txs = traceSignals OldestFirst tr
      all (\tx -> txSizeBound tx >= numBytes tx) txs
  where
    p :: Proxy era
    p = Proxy
    genesisLedgerSt = Just $ mkGenesisLedgerState (genEnv @era @MockCrypto p defaultConstants)

-- | Check that the abstract transaction size function
-- is not off by an acceptable order of magnitude.
propAbstractSizeNotTooBig ::
  forall era.
  ( EraGen era
  , EraGov era
  , EraStake era
  , QC.HasTrace (ShelleyLEDGER era) (GenEnv MockCrypto era)
  ) =>
  Property
propAbstractSizeNotTooBig = property $ do
  let tl = 100
      -- The below acceptable order of magnitude may not actually be large enough.
      -- For small transactions, estimating the size of an encoded uint as 5
      -- may mean that our size is more like five times too big.
      -- It will be interesting to see the test fail with
      -- an acceptableMagnitude of three, though.
      acceptableMagnitude = (3 :: Integer)
      numBytes = toInteger . BS.length . Plain.serialize'
      notTooBig tx = txSizeBound tx <= acceptableMagnitude * numBytes tx
  forAllTraceFromInitState @(ShelleyLEDGER era)
    testGlobals
    tl
    (genEnv @era @MockCrypto p defaultConstants)
    genesisLedgerSt
    $ \tr -> do
      let txs :: [Tx era]
          txs = traceSignals OldestFirst tr
      all notTooBig txs
  where
    p :: Proxy era
    p = Proxy
    genesisLedgerSt = Just $ mkGenesisLedgerState (genEnv @era @MockCrypto p defaultConstants)

onlyValidChainSignalsAreGenerated ::
  forall era.
  ( EraGen era
  , EraGov era
  , EraStake era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  TestTree
onlyValidChainSignalsAreGenerated =
  TQC.testProperty "Only valid CHAIN STS signals are generated" prop
  where
    prop =
      withMaxSuccess 100 $
        onlyValidSignalsAreGeneratedFromInitState @(CHAIN era)
          testGlobals
          100
          (genEnv @era @MockCrypto p defaultConstants)
          genesisChainSt
    p :: Proxy era
    p = Proxy
    genesisChainSt = Just (mkGenesisChainState (genEnv @era @MockCrypto p defaultConstants))

-- | Counts the epochs spanned by this trace
epochsInTrace :: forall era. [Block (BHeader MockCrypto) era] -> Int
epochsInTrace bs'
  | Just bs <- nonEmpty bs' =
      let
        fromEpoch = atEpoch . blockSlot $ NE.head bs
        toEpoch = atEpoch . blockSlot $ NE.last bs
        EpochSize slotsPerEpoch =
          epochInfoSize (epochInfoPure testGlobals) $
            error "Impossible: Fixed epoch size does not care about current epoch number"
        blockSlot = bheaderSlotNo . bhbody . bheader
        atEpoch (SlotNo s) = s `div` slotsPerEpoch
       in
        fromIntegral $ toEpoch - fromEpoch + 1
  | otherwise = 0

-- | Convenience Function to bound the txsize function.
-- | It can be helpful for coin selection.
txSizeBound ::
  forall era.
  EraTx era =>
  Tx era ->
  Integer
txSizeBound tx = numInputs * inputSize + numOutputs * outputSize + rest
  where
    uint = 5
    smallArray = 1
    hashLen = 32
    hashObj = 2 + hashLen
    addrHashLen = 28
    addrHeader = 1
    address = 2 + addrHeader + 2 * addrHashLen
    txBody = tx ^. bodyTxL
    numInputs = toInteger . length $ txBody ^. inputsTxBodyL
    inputSize = smallArray + uint + hashObj
    numOutputs = toInteger . length $ txBody ^. outputsTxBodyL
    outputSize = smallArray + uint + address
    rest = tx ^. sizeTxF
