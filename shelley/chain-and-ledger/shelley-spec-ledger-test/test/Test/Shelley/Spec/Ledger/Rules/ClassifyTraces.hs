{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Rules.ClassifyTraces
  ( onlyValidLedgerSignalsAreGenerated,
    onlyValidChainSignalsAreGenerated,
    relevantCasesAreCovered,
    propAbstractSizeBoundsBytes,
    propAbstractSizeNotTooBig,
  )
where

import Cardano.Binary (serialize')
import Cardano.Ledger.Era (Era)
import Cardano.Slotting.Slot (EpochSize (..))
import qualified Control.State.Transition.Extended
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
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import Shelley.Spec.Ledger.API
  ( Addr (..),
    CHAIN,
    ChainState,
    Credential (..),
    DCert (..),
    DelegCert (..),
    Delegation (..),
    LEDGER,
    TxOut (..),
  )
import Shelley.Spec.Ledger.BaseTypes (Globals (epochInfo), StrictMaybe (..))
import Shelley.Spec.Ledger.BlockChain
  ( Block (..),
    TxSeq (..),
    bhbody,
    bheaderSlotNo,
  )
import Shelley.Spec.Ledger.Delegation.Certificates
  ( isDeRegKey,
    isDelegation,
    isGenesisDelegation,
    isRegKey,
    isRegPool,
    isReservesMIRCert,
    isRetirePool,
    isTreasuryMIRCert,
  )
import Shelley.Spec.Ledger.LedgerState
  ( DPState,
    UTxOState (..),
    txsizeBound,
  )
import Shelley.Spec.Ledger.PParams
  ( pattern ProposedPPUpdates,
    pattern Update,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..), epochInfoSize)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxBody
  ( TxBody (..),
    Wdrl (..),
  )
import Test.QuickCheck
  ( Property,
    checkCoverage,
    conjoin,
    cover,
    forAllBlind,
    property,
    withMaxSuccess,
  )
import qualified Test.QuickCheck.Gen as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger (mkGenesisLedgerState)
import Test.Shelley.Spec.Ledger.Utils
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Crypto (Crypto)

genesisChainState ::
  forall era a.
  ShelleyTest era =>
  Maybe
    ( Control.State.Transition.Extended.IRC (CHAIN era) ->
      QC.Gen
        ( Either
            a
            (ChainState era)
        )
    )
genesisChainState = Just $ mkGenesisChainState (geConstants (genEnv p))
  where
    p :: Proxy era
    p = Proxy

genesisLedgerState ::
  forall c a.
  Crypto c =>
  Maybe
    ( Control.State.Transition.Extended.IRC (LEDGER (ShelleyEra c)) ->
      QC.Gen
        ( Either
            a
            ( UTxOState (ShelleyEra c),
              DPState (ShelleyEra c)
            )
        )
    )
genesisLedgerState = Just $ mkGenesisLedgerState (geConstants (genEnv p))
  where
    p :: Proxy (ShelleyEra c)
    p = Proxy

relevantCasesAreCovered :: Property
relevantCasesAreCovered = do
  let tl = 100
  checkCoverage $
    forAllBlind
      (traceFromInitState @(CHAIN C) testGlobals tl (genEnv p) genesisChainState)
      relevantCasesAreCoveredForTrace
  where
    p :: Proxy C
    p = Proxy

relevantCasesAreCoveredForTrace ::
  forall era.
  (ShelleyTest era) =>
  Trace (CHAIN era) ->
  Property
relevantCasesAreCoveredForTrace tr = do
  let blockTxs :: Block era -> [Tx era]
      blockTxs (Block _ (TxSeq txSeq)) = toList txSeq
      bs = traceSignals OldestFirst tr
      txs = concat (blockTxs <$> bs)
      certsByTx_ = certsByTx txs
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
            0.1 < txScriptOutputsRatio (map (_outputs . _body) txs),
            20
          ),
          ( "at least 10% of `DCertDeleg` certificates have script credentials",
            0.1 < scriptCredentialCertsRatio certs_,
            60
          ),
          ( "at least 1 in 10 transactions have a reward withdrawal",
            length txs < 10 * length (filter hasWithdrawal txs),
            60
          ),
          ( "at least 1 in 20 transactions have non-trivial protocol param updates",
            length txs < 20 * length (filter hasPParamUpdate txs),
            60
          ),
          ( "at least 1 in 20 transactions have metadata",
            length txs < 20 * length (filter hasMetaData txs),
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
scriptCredentialCertsRatio :: [DCert era] -> Double
scriptCredentialCertsRatio certs =
  ratioInt haveScriptCerts couldhaveScriptCerts
  where
    haveScriptCerts =
      ( length $
          filter
            ( \case
                DCertDeleg (RegKey (ScriptHashObj _)) -> True
                DCertDeleg (DeRegKey (ScriptHashObj _)) -> True
                DCertDeleg (Delegate (Delegation (ScriptHashObj _) _)) -> True
                _ -> False
            )
            certs
      )
    couldhaveScriptCerts =
      length $
        filter
          ( \case
              DCertDeleg _ -> True
              _ -> False
          )
          certs

-- | Extract the certificates from the transactions
certsByTx :: ShelleyTest era => [Tx era] -> [[DCert era]]
certsByTx txs = toList . _certs . _body <$> txs

ratioInt :: Int -> Int -> Double
ratioInt x y =
  fromIntegral x / fromIntegral y

-- | Transaction has script locked TxOuts
txScriptOutputsRatio :: ShelleyTest era => [StrictSeq (TxOut era)] -> Double
txScriptOutputsRatio txoutsList =
  ratioInt
    (sum (map countScriptOuts txoutsList))
    (sum (map length txoutsList))
  where
    countScriptOuts txouts =
      sum $
        fmap
          ( \case
              TxOut (Addr _ (ScriptHashObj _) _) _ -> 1
              _ -> 0
          )
          txouts

hasWithdrawal :: ShelleyTest era => Tx era -> Bool
hasWithdrawal = not . null . unWdrl . _wdrls . _body

hasPParamUpdate :: ShelleyTest era => Tx era -> Bool
hasPParamUpdate tx =
  ppUpdates . _txUpdate . _body $ tx
  where
    ppUpdates SNothing = False
    ppUpdates (SJust (Update (ProposedPPUpdates ppUpd) _)) = Map.size ppUpd > 0

hasMetaData :: ShelleyTest era => Tx era -> Bool
hasMetaData tx =
  f . _mdHash . _body $ tx
  where
    f SNothing = False
    f (SJust _) = True

onlyValidLedgerSignalsAreGenerated :: Property
onlyValidLedgerSignalsAreGenerated =
  withMaxSuccess 200 $
    onlyValidSignalsAreGeneratedFromInitState @(LEDGER C) testGlobals 100 (genEnv p) genesisLedgerState
  where
    p :: Proxy C
    p = Proxy

-- | Check that the abstract transaction size function
-- actually bounds the number of bytes in the serialized transaction.
propAbstractSizeBoundsBytes :: Property
propAbstractSizeBoundsBytes = property $ do
  let tl = 100
      numBytes = toInteger . BS.length . serialize'
  forAllTraceFromInitState @(LEDGER C) testGlobals tl (genEnv p) genesisLedgerState $ \tr -> do
    let txs :: [Tx C]
        txs = traceSignals OldestFirst tr
    all (\tx -> txsizeBound tx >= numBytes tx) txs
  where
    p :: Proxy C
    p = Proxy

-- | Check that the abstract transaction size function
-- is not off by an acceptable order of magnitude.
propAbstractSizeNotTooBig :: Property
propAbstractSizeNotTooBig = property $ do
  let tl = 100
      -- The below acceptable order of magnitude may not actually be large enough.
      -- For small transactions, estimating the size of an encoded uint as 5
      -- may mean that our size is more like five times too big.
      -- It will be interesting to see the test fail with
      -- an acceptableMagnitude of three, though.
      acceptableMagnitude = (3 :: Integer)
      numBytes = toInteger . BS.length . serialize'
      notTooBig txb = txsizeBound txb <= acceptableMagnitude * numBytes txb
  forAllTraceFromInitState @(LEDGER C) testGlobals tl (genEnv p) genesisLedgerState $ \tr -> do
    let txs :: [Tx C]
        txs = traceSignals OldestFirst tr
    all notTooBig txs
  where
    p :: Proxy C
    p = Proxy

onlyValidChainSignalsAreGenerated :: Property
onlyValidChainSignalsAreGenerated =
  withMaxSuccess 100 $
    onlyValidSignalsAreGeneratedFromInitState @(CHAIN C) testGlobals 100 (genEnv p) genesisChainState
  where
    p :: Proxy C
    p = Proxy

-- | Counts the epochs spanned by this trace
epochsInTrace :: forall era. Era era => [Block era] -> Int
epochsInTrace [] = 0
epochsInTrace bs =
  fromIntegral $ toEpoch - fromEpoch + 1
  where
    fromEpoch = atEpoch . blockSlot $ head bs
    toEpoch = atEpoch . blockSlot $ last bs
    EpochSize slotsPerEpoch = runShelleyBase $ (epochInfoSize . epochInfo) testGlobals undefined
    blockSlot (Block bh _) = (bheaderSlotNo . bhbody) bh
    atEpoch (SlotNo s) = s `div` slotsPerEpoch
