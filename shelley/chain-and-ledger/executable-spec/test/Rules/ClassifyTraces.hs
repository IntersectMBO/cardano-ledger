{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Rules.ClassifyTraces
  ( onlyValidLedgerSignalsAreGenerated
  , relevantCasesAreCovered)
  where

import           Data.Foldable (toList)
import           Data.Word (Word64)
import           Test.QuickCheck (Property, checkCoverage, conjoin, cover, property, withMaxSuccess)

import           Control.State.Transition.Trace (TraceOrder (OldestFirst), traceLength,
                     traceSignals)
import           Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState,
                     onlyValidSignalsAreGeneratedFromInitState)
import           Delegation.Certificates (isDeRegKey, isDelegation, isGenesisDelegation, isRegKey,
                     isRegPool, isRetirePool)
import           Generator.Core.QuickCheck (mkGenesisLedgerState)
import           Generator.LedgerTrace.QuickCheck ()
import           MockTypes (DCert, LEDGER, Tx)
import           TxData (_body, _certs)

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withMaxSuccess 500 . property $ do
  let tl = 100
  forAllTraceFromInitState @LEDGER tl tl (Just mkGenesisLedgerState) $ \tr -> do
    let txs :: [Tx]
        txs = traceSignals OldestFirst tr
        certs_ = allCerts txs

    checkCoverage $ conjoin [
       cover_ 75
             (traceLength tr <= 5 * length certs_)
             "there is at least 1 certificate for every 5 transactions"

     , cover_ 75
              (traceLength tr <= 20 * length (filter isRegKey certs_))
              "there is at least 1 RegKey certificate for every 10 transactions"

     , cover_ 75
              (traceLength tr <= 20 * length (filter isDeRegKey certs_))
              "there is at least 1 DeRegKey certificate for every 20 transactions"

     , cover_ 75
              (traceLength tr <= 20 * length (filter isDelegation certs_))
              "there is at least 1 Delegation certificate for every 10 transactions"

     , cover_ 75
              (traceLength tr <= 40 * length (filter isGenesisDelegation certs_))
              "there is at least 1 Genesis Delegation certificate for every 40 transactions"

     , cover_ 75
              (traceLength tr <= 20 * length (filter isRegPool certs_))
              "there is at least 1 RegPool certificate for every 10 transactions"

     , cover_ 75
              (traceLength tr <= 20 * length (filter isRetirePool certs_))
              "there is at least 1 RetirePool certificate for every 20 transactions"

     , cover_ 25
              (0.75 >= noCertsRatio (certsByTx txs))
              "at most 75% of transactions have no certificates"
     ]
    where
      cover_ pc b s = cover pc b s (property ())

-- | Extract the certificates from the transactions
certsByTx :: [Tx] -> [[DCert]]
certsByTx txs = toList . _certs . _body <$> txs

-- | Flattended list of DCerts for the given transactions
allCerts :: [Tx] -> [DCert]
allCerts = concat . certsByTx

-- | Ratio of the number of empty certificate groups and the number of groups
noCertsRatio :: [[DCert]] -> Double
noCertsRatio = lenRatio (filter null)

ratioInt :: Int -> Int -> Double
ratioInt x y
  = fromIntegral x / fromIntegral y

-- | Transforms the list and returns the ratio of lengths of
-- the transformed and original lists.
lenRatio :: ([a] -> [b]) -> [a] -> Double
lenRatio f xs
  = ratioInt (length (f xs))
             (length xs)

onlyValidLedgerSignalsAreGenerated :: Property
onlyValidLedgerSignalsAreGenerated = withMaxSuccess 200 $
    onlyValidSignalsAreGeneratedFromInitState @LEDGER 100 (100::Word64) (Just mkGenesisLedgerState)
