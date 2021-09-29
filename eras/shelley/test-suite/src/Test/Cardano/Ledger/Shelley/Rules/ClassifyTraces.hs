{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces
  ( onlyValidLedgerSignalsAreGenerated,
    onlyValidChainSignalsAreGenerated,
    relevantCasesAreCovered,
    propAbstractSizeBoundsBytes,
    propAbstractSizeNotTooBig,
  )
where

import Cardano.Binary (ToCBOR, serialize')
import Cardano.Ledger.BaseTypes (Globals, StrictMaybe (..), epochInfo)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, SupportsSegWit (fromTxSeq), TxSeq)
import Cardano.Ledger.Shelley.API
  ( Addr (..),
    Credential (..),
    DCert (..),
    DelegCert (..),
    Delegation (..),
    LEDGER,
  )
import Cardano.Ledger.Shelley.BlockChain (Block (..), bheader)
import Cardano.Ledger.Shelley.Constraints (UsesTxBody)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( isDeRegKey,
    isDelegation,
    isGenesisDelegation,
    isRegKey,
    isRegPool,
    isReservesMIRCert,
    isRetirePool,
    isTreasuryMIRCert,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( DPState,
    UTxOState,
    txsizeBound,
  )
import Cardano.Ledger.Shelley.PParams
  ( Update (..),
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Cardano.Ledger.Shelley.PParams as PParams (Update)
import Cardano.Ledger.Shelley.TxBody (TxIn, Wdrl (..))
import Cardano.Ledger.Slot (SlotNo (..), epochInfoSize)
import Cardano.Protocol.TPraos.BHeader
  ( bhbody,
    bheaderSlotNo,
  )
import Cardano.Slotting.Slot (EpochSize (..))
import Control.State.Transition (STS (State))
import Control.State.Transition.Extended (Environment, Signal)
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (OldestFirst),
    traceSignals,
  )
import Control.State.Transition.Trace.Generator.QuickCheck
  ( forAllTraceFromInitState,
    onlyValidSignalsAreGeneratedFromInitState,
    traceFromInitState,
  )
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import qualified Data.ByteString as BS
import Data.Default.Class (Default)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Records (HasField (..), getField)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (mkGenesisLedgerState)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Utils
import Test.QuickCheck
  ( Property,
    checkCoverage,
    conjoin,
    cover,
    forAllBlind,
    property,
    withMaxSuccess,
  )

-- =================================================================

relevantCasesAreCovered ::
  forall era.
  ( EraGen era,
    Default (State (Core.EraRule "PPUP" era)),
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "update" (Core.TxBody era) (StrictMaybe (PParams.Update era))
    -- HasField "address" (Core.TxOut era) (Addr (Crypto era))
  ) =>
  Property
relevantCasesAreCovered = do
  let tl = 100
  checkCoverage $
    forAllBlind
      (traceFromInitState @(CHAIN era) testGlobals tl (genEnv p) genesisChainSt)
      relevantCasesAreCoveredForTrace
  where
    p :: Proxy era
    p = Proxy
    genesisChainSt = Just $ mkGenesisChainState (genEnv p)

relevantCasesAreCoveredForTrace ::
  forall era.
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "update" (Core.TxBody era) (StrictMaybe (PParams.Update era))
    --  HasField "address" (Core.TxOut era) (Addr (Crypto era))
  ) =>
  Trace (CHAIN era) ->
  Property
relevantCasesAreCoveredForTrace tr = do
  let blockTxs :: Block era -> [Core.Tx era]
      blockTxs (Block' _ txSeq _) = toList (fromTxSeq @era txSeq)
      bs = traceSignals OldestFirst tr
      txs = concat (blockTxs <$> bs)
      certsByTx_ = certsByTx @era txs
      certs_ = concat certsByTx_

      classifications =
        [ ( "there is at least 1 certificate for every 2 transactions",
            length txs < 2 * length certs_,
            60
          ),
          ( "there is at least 1 RegKey certificate for every 10 transactions",
            length txs < 10 * length (filter isRegKey certs_),
            60
          ),
          ( "there is at least 1 DeRegKey certificate for every 20 transactions",
            length txs < 20 * length (filter isDeRegKey certs_),
            60
          ),
          ( "there is at least 1 Delegation certificate for every 10 transactions",
            length txs < 10 * length (filter isDelegation certs_),
            60
          ),
          ( "there is at least 1 Genesis Delegation certificate for every 20 transactions",
            length txs < 20 * length (filter isGenesisDelegation certs_),
            60
          ),
          ( "there is at least 1 RetirePool certificate for every 10 transactions",
            length txs < 10 * length (filter isRetirePool certs_),
            60
          ),
          ( "there is at least 1 MIR certificate (spending Reserves) for every 60 transactions",
            length txs < 60 * length (filter isReservesMIRCert certs_),
            40
          ),
          ( "there is at least 1 MIR certificate (spending Treasury) for every 60 transactions",
            length txs < 60 * length (filter isTreasuryMIRCert certs_),
            40
          ),
          ( "there is at least 1 RegPool certificate for every 10 transactions",
            length txs < 10 * length (filter isRegPool certs_),
            60
          ),
          ( "at least 10% of transactions have script TxOuts",
            0.1 < txScriptOutputsRatio (Proxy @era) (map (getField @"outputs" . getField @"body") txs),
            20
          ),
          ( "at least 10% of `DCertDeleg` certificates have script credentials",
            0.1 < scriptCredentialCertsRatio certs_,
            60
          ),
          ( "at least 1 in 10 transactions have a reward withdrawal",
            length txs < 10 * length (filter (hasWithdrawal @era) txs),
            60
          ),
          ( "at least 1 in 20 transactions have non-trivial protocol param updates",
            length txs < 20 * length (filter (hasPParamUpdate @era) txs),
            60
          ),
          ( "at least 1 in 20 transactions have metadata",
            length txs < 20 * length (filter (hasMetadata @era) txs),
            60
          ),
          ( "at least 5 epochs in a trace, 20% of the time",
            5 <= epochsInTrace bs,
            20
          )
        ]

  conjoin $ cover_ <$> classifications
  where
    cover_ (label, predicate, coveragePc) =
      cover coveragePc predicate label (property ())

-- | Ratio of certificates with script credentials to the number of certificates
-- that could have script credentials.
scriptCredentialCertsRatio :: [DCert crypto] -> Double
scriptCredentialCertsRatio certs =
  ratioInt haveScriptCerts couldhaveScriptCerts
  where
    haveScriptCerts =
      length $
        filter
          ( \case
              DCertDeleg (RegKey (ScriptHashObj _)) -> True
              DCertDeleg (DeRegKey (ScriptHashObj _)) -> True
              DCertDeleg (Delegate (Delegation (ScriptHashObj _) _)) -> True
              _ -> False
          )
          certs
    couldhaveScriptCerts =
      length $
        filter
          ( \case
              DCertDeleg _ -> True
              _ -> False
          )
          certs

-- | Extract the certificates from the transactions
certsByTx ::
  forall era.
  ( HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "body" (Core.Tx era) (Core.TxBody era)
  ) =>
  [Core.Tx era] ->
  [[DCert (Crypto era)]]
certsByTx txs = toList . (getField @"certs") . getField @"body" <$> txs

ratioInt :: Int -> Int -> Double
ratioInt x y =
  fromIntegral x / fromIntegral y

-- | Transaction has script locked TxOuts
txScriptOutputsRatio ::
  forall era.
  HasField "address" (Core.TxOut era) (Addr (Crypto era)) =>
  Proxy era ->
  [StrictSeq (Core.TxOut era)] ->
  Double
txScriptOutputsRatio _ txoutsList =
  ratioInt
    (sum (map countScriptOuts txoutsList))
    (sum (map length txoutsList))
  where
    countScriptOuts :: StrictSeq (Core.TxOut era) -> Int
    countScriptOuts txouts =
      sum $
        fmap
          ( \out -> case (getField @"address" (out :: Core.TxOut era)) of
              Addr _ (ScriptHashObj _) _ -> 1
              _ -> 0
          )
          txouts

hasWithdrawal ::
  ( HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "body" (Core.Tx era) (Core.TxBody era)
  ) =>
  Core.Tx era ->
  Bool
hasWithdrawal x = (not . null . unWdrl . (getField @"wdrls") . getField @"body") x

hasPParamUpdate ::
  ( HasField "update" (Core.TxBody era) (StrictMaybe (PParams.Update era)),
    HasField "body" (Core.Tx era) (Core.TxBody era)
  ) =>
  Core.Tx era ->
  Bool
hasPParamUpdate tx =
  ppUpdates . getField @"update" . getField @"body" $ tx
  where
    ppUpdates SNothing = False
    ppUpdates (SJust (Update (ProposedPPUpdates ppUpd) _)) = Map.size ppUpd > 0

hasMetadata ::
  forall era.
  ( UsesTxBody era
  ) =>
  Core.Tx era ->
  Bool
hasMetadata tx =
  f . getField @"adHash" . getField @"body" $ tx
  where
    f SNothing = False
    f (SJust _) = True

onlyValidLedgerSignalsAreGenerated ::
  forall era ledger.
  ( EraGen era,
    ChainProperty era,
    QC.HasTrace ledger (GenEnv era),
    Default (State (Core.EraRule "PPUP" era)),
    QC.BaseEnv ledger ~ Globals,
    State ledger ~ (UTxOState era, DPState (Crypto era)),
    Show (Environment ledger),
    Show (Signal ledger)
  ) =>
  Property
onlyValidLedgerSignalsAreGenerated =
  withMaxSuccess 200 $
    onlyValidSignalsAreGeneratedFromInitState
      @ledger
      testGlobals
      100
      ge
      genesisLedgerSt
  where
    p :: Proxy era
    p = Proxy
    ge = genEnv p
    genesisLedgerSt = Just $ mkGenesisLedgerState ge

-- | Check that the abstract transaction size function
-- actually bounds the number of bytes in the serialized transaction.
propAbstractSizeBoundsBytes ::
  forall era.
  ( EraGen era,
    ChainProperty era,
    QC.HasTrace (LEDGER era) (GenEnv era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    ToCBOR (Core.Tx era), -- Arises from propAbstractSizeNotTooBig (which serializes)
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  Property
propAbstractSizeBoundsBytes = property $ do
  let tl = 100
      numBytes = toInteger . BS.length . serialize'
  forAllTraceFromInitState @(LEDGER era)
    testGlobals
    tl
    (genEnv p)
    genesisLedgerSt
    $ \tr -> do
      let txs :: [Core.Tx era]
          txs = traceSignals OldestFirst tr
      all (\tx -> txsizeBound (Proxy @era) tx >= numBytes tx) txs
  where
    p :: Proxy era
    p = Proxy
    genesisLedgerSt = Just $ mkGenesisLedgerState (genEnv p)

-- | Check that the abstract transaction size function
-- is not off by an acceptable order of magnitude.
propAbstractSizeNotTooBig ::
  forall era.
  ( EraGen era,
    ChainProperty era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    ToCBOR (Core.Tx era), -- We need to serialize it to get its size.
    QC.HasTrace (LEDGER era) (GenEnv era),
    Default (State (Core.EraRule "PPUP" era))
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
      numBytes = toInteger . BS.length . serialize'
      notTooBig txb = txsizeBound (Proxy @era) txb <= acceptableMagnitude * numBytes txb
  forAllTraceFromInitState @(LEDGER era)
    testGlobals
    tl
    (genEnv p)
    genesisLedgerSt
    $ \tr -> do
      let txs :: [Core.Tx era]
          txs = traceSignals OldestFirst tr
      all notTooBig txs
  where
    p :: Proxy era
    p = Proxy
    genesisLedgerSt = Just $ mkGenesisLedgerState (genEnv p)

onlyValidChainSignalsAreGenerated ::
  forall era.
  ( EraGen era,
    Default (State (Core.EraRule "PPUP" era)),
    QC.HasTrace (CHAIN era) (GenEnv era),
    Show (TxSeq era)
  ) =>
  Property
onlyValidChainSignalsAreGenerated =
  withMaxSuccess 100 $
    onlyValidSignalsAreGeneratedFromInitState @(CHAIN era)
      testGlobals
      100
      (genEnv p)
      genesisChainSt
  where
    p :: Proxy era
    p = Proxy
    genesisChainSt = Just $ mkGenesisChainState (genEnv p)

-- | Counts the epochs spanned by this trace
epochsInTrace :: forall era. Era era => [Block era] -> Int
epochsInTrace [] = 0
epochsInTrace bs =
  fromIntegral $ toEpoch - fromEpoch + 1
  where
    fromEpoch = atEpoch . blockSlot $ head bs
    toEpoch = atEpoch . blockSlot $ last bs
    EpochSize slotsPerEpoch = runShelleyBase $ (epochInfoSize . epochInfo) testGlobals undefined
    blockSlot = bheaderSlotNo . bhbody . bheader
    atEpoch (SlotNo s) = s `div` slotsPerEpoch
