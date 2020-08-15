{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Cardano.Slotting.Slot (EpochSize (..))
import qualified Control.State.Transition.Extended
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (OldestFirst),
    traceLength,
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
import Shelley.Spec.Ledger.Coin
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
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.PParams
  ( PParamsUpdate,
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..), epochInfoSize)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxData
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
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger (mkGenesisLedgerState)
import Test.Shelley.Spec.Ledger.Utils
import Test.Shelley.Spec.Ledger.Serialisation.Generators()

genesisChainState ::
  Maybe
    ( Control.State.Transition.Extended.IRC (CHAIN C Coin) ->
      QC.Gen
        ( Either
            a
            (ChainState C Coin)
        )
    )
genesisChainState = Just $ mkGenesisChainState (geConstants (genEnv p))
  where
    p :: Proxy C
    p = Proxy

genesisLedgerState ::
  Maybe
    ( Control.State.Transition.Extended.IRC (LEDGER C Coin) ->
      QC.Gen
        ( Either
            a
            ( UTxOState C Coin,
              DPState C
            )
        )
    )
genesisLedgerState = Just $ mkGenesisLedgerState (geConstants (genEnv p))
  where
    p :: Proxy C
    p = Proxy

relevantCasesAreCovered :: Property
relevantCasesAreCovered = do
  let tl = 100
  checkCoverage $
    forAllBlind
      (traceFromInitState @(CHAIN C Coin) testGlobals tl (genEnv p) genesisChainState)
      relevantCasesAreCoveredForTrace
  where
    p :: Proxy C
    p = Proxy

relevantCasesAreCoveredForTrace ::
  Trace (CHAIN C Coin) ->
  Property
relevantCasesAreCoveredForTrace tr = do
  let p :: Proxy C
      p = Proxy
      GenEnv _ c@Constants {maxCertsPerTx} = genEnv p
      blockTxs (Block _ (TxSeq txSeq)) = toList txSeq
      bs = traceSignals OldestFirst tr
      txs = concat (blockTxs <$> bs)
      tl' = traceLength tr
      certs_ = allCerts txs
      classifications =
        [ ( "there is at least 1 certificate for every 3 transactions",
            tl' < 1 * length certs_,
            60
          ),
          ( "there is at least 1 RegKey certificate for every 10 transactions",
            tl' < 10 * length (filter isRegKey certs_),
            60
          ),
          ( "there is at least 1 DeRegKey certificate for every 10 transactions",
            tl' < 10 * length (filter isDeRegKey certs_),
            60
          ),
          ( "there is at least 1 Delegation certificate for every 10 transactions",
            tl' < 10 * length (filter isDelegation certs_),
            60
          ),
          ( "there is at least 1 Genesis Delegation certificate for every 20 transactions",
            tl' < 20 * length (filter isGenesisDelegation certs_),
            60
          ),
          ( "there is at least 1 RetirePool certificate for every 10 transactions",
            tl' < 10 * length (filter isRetirePool certs_),
            60
          ),
          ( "there is at least 1 Reserves MIR certificate (spending Reserves) for every 60 transactions",
            tl' < 60 * length (filter isReservesMIRCert certs_),
            40
          ),
          ( "there is at least 1 MIR certificate (spending Treasury) for every 60 transactions",
            tl' < 60 * length (filter isTreasuryMIRCert certs_),
            40
          ),
          ( "at most 60% of transactions have no certificates",
            0.6 > noCertsRatio (certsByTx txs),
            60
          ),
          ( "at least 10% of transactions have " <> show maxCertsPerTx <> " certificates",
            0.1 < maxCertsRatio c (certsByTx txs),
            60
          ),
          ( "there is at least 1 RegPool certificate for every 10 transactions",
            tl' < 10 * length (filter isRegPool certs_),
            60
          ),
          {- TODO re-enable after the script embargo has been lifted
           -
           - in other words, turn "0 == txScriptOutputsRatio" back to "0.1 < txScriptOutputsRatio "
          -}
          ( "at least 10% of transactions have script TxOuts",
            0 == txScriptOutputsRatio (map (_outputs . _body) txs),
            20
          ),
          {- TODO re-enable after the script embargo has been lifted
           -
           - in other words, turn "0 == scriptCredentialCertsRatio" back to "0.1 < scriptCredentialCertsRatio "
          -}
          ( "at least 10% of `DCertDeleg` certificates have script credentials",
            0 == scriptCredentialCertsRatio certs_,
            60
          ),
          ( "at least 10% of transactions have a reward withdrawal",
            0.1 < withdrawalRatio txs,
            60
          ),
          ( "at least 5% of transactions have non-trivial protocol param updates",
            0.95 > noPPUpdateRatio (ppUpdatesByTx txs),
            60
          ),
          ( "at least 5% of transactions have metadata",
            0.05 < metaDataRatio (metaDataByTx txs),
            60
          ),
          ( "at least 2 epoch changes in trace, 10% of the time",
            2 <= epochBoundariesInTrace bs,
            10
          )
        ]

  conjoin $ cover_ <$> classifications
  where
    cover_ (label, predicate, coveragePc) =
      cover coveragePc predicate label (property ())

-- | Ratio of certificates with script credentials to the number of certificates
-- that could have script credentials.
scriptCredentialCertsRatio :: [DCert C] -> Double
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
certsByTx :: [Tx C Coin] -> [[DCert C]]
certsByTx txs = toList . _certs . _body <$> txs

-- | Flattended list of DCerts for the given transactions
allCerts :: [Tx C Coin] -> [DCert C]
allCerts = concat . certsByTx

-- | Ratio of the number of empty certificate groups and the number of groups
noCertsRatio :: [[DCert C]] -> Double
noCertsRatio = lenRatio (filter null)

-- | Ratio of the number of certificate groups of max size and the number of groups
maxCertsRatio :: Constants -> [[DCert C]] -> Double
maxCertsRatio Constants {maxCertsPerTx} = lenRatio (filter ((== maxCertsPerTx) . fromIntegral . length))

-- | Extract the metadata from the transactions
metaDataByTx :: [Tx C Coin] -> [[MetaDataHash C]]
metaDataByTx txs =
  f . _mdHash . _body <$> txs
  where
    f SNothing = []
    f (SJust md) = [md]

-- | Ratio of the number of transactions with metadata and total transactions
metaDataRatio :: [[MetaDataHash C]] -> Double
metaDataRatio = lenRatio (filter (not . null))

-- | Extract non-trivial protocol param  updates from the given transactions
ppUpdatesByTx :: [Tx C Coin] -> [[PParamsUpdate]]
ppUpdatesByTx txs = ppUpdates . _txUpdate . _body <$> txs
  where
    ppUpdates SNothing = mempty
    ppUpdates (SJust (Update (ProposedPPUpdates ppUpd) _)) = Map.elems ppUpd

-- | Ratio of the number of empty PParamsUpdate to Updates
noPPUpdateRatio :: [[PParamsUpdate]] -> Double
noPPUpdateRatio = lenRatio (filter null)

ratioInt :: Int -> Int -> Double
ratioInt x y =
  fromIntegral x / fromIntegral y

-- | Transaction has script locked TxOuts
txScriptOutputsRatio :: [StrictSeq (TxOut C Coin)] -> Double
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

-- | Transaction has a reward withdrawal
withdrawalRatio :: [Tx C Coin] -> Double
withdrawalRatio = lenRatio (filter $ not . null . unWdrl . _wdrls . _body)

-- | Transforms the list and returns the ratio of lengths of
-- the transformed and original lists.
lenRatio :: ([a] -> [b]) -> [a] -> Double
lenRatio f xs =
  ratioInt
    (length (f xs))
    (length xs)

onlyValidLedgerSignalsAreGenerated :: Property
onlyValidLedgerSignalsAreGenerated =
  withMaxSuccess 200 $
    onlyValidSignalsAreGeneratedFromInitState @(LEDGER C Coin) testGlobals 100 (genEnv p) genesisLedgerState
  where
    p :: Proxy C
    p = Proxy

-- | Check that the abstract transaction size function
-- actually bounds the number of bytes in the serialized transaction.
propAbstractSizeBoundsBytes :: Property
propAbstractSizeBoundsBytes = property $ do
  let tl = 100
      numBytes = toInteger . BS.length . serialize'
  forAllTraceFromInitState @(LEDGER C Coin) testGlobals tl (genEnv p) genesisLedgerState $ \tr -> do
    let txs :: [Tx C Coin]
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
  forAllTraceFromInitState @(LEDGER C Coin) testGlobals tl (genEnv p) genesisLedgerState $ \tr -> do
    let txs :: [Tx C Coin]
        txs = traceSignals OldestFirst tr
    all notTooBig txs
  where
    p :: Proxy C
    p = Proxy

onlyValidChainSignalsAreGenerated :: Property
onlyValidChainSignalsAreGenerated =
  withMaxSuccess 100 $
    onlyValidSignalsAreGeneratedFromInitState @(CHAIN C Coin) testGlobals 100 (genEnv p) genesisChainState
  where
    p :: Proxy C
    p = Proxy

epochBoundariesInTrace :: [Block C Coin] -> Int
epochBoundariesInTrace bs =
  length $
    filter atEpochBoundary (blockSlot <$> bs)
  where
    EpochSize slotsPerEpoch = runShelleyBase $ (epochInfoSize . epochInfo) testGlobals undefined
    blockSlot (Block bh _) = (bheaderSlotNo . bhbody) bh
    atEpochBoundary (SlotNo s) = s `rem` slotsPerEpoch == 0
