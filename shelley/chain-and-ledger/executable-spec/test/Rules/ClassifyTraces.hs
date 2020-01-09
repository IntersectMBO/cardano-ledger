{-# LANGUAGE LambdaCase #-}
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

import           ConcreteCryptoTypes (DCert, LEDGER, Tx, TxOut)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst), traceLength,
                     traceSignals)
import           Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState,
                     onlyValidSignalsAreGeneratedFromInitState)
import           Delegation.Certificates (isDeRegKey, isDelegation, isGenesisDelegation,
                     isInstantaneousRewards, isRegKey, isRegPool, isRetirePool)
import           Generator.Core.QuickCheck (mkGenesisLedgerState)
import           Generator.LedgerTrace.QuickCheck ()
import           Test.Utils
import           TxData (pattern AddrBase, pattern DCertDeleg, pattern DeRegKey, pattern Delegate,
                     pattern Delegation, pattern RegKey, pattern ScriptHashObj, pattern TxOut,
                     _body, _certs, _outputs)

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withMaxSuccess 500 . property $ do
  let tl = 100
  forAllTraceFromInitState @LEDGER testGlobals tl tl (Just mkGenesisLedgerState) $ \tr -> do
    let txs :: [Tx]
        txs = traceSignals OldestFirst tr
        certs_ = allCerts txs

    checkCoverage $ conjoin [
       cover_ 60
             (traceLength tr <= 5 * length certs_)
             "there is at least 1 certificate for every 5 transactions"

     , cover_ 60
              (traceLength tr <= 20 * length (filter isRegKey certs_))
              "there is at least 1 RegKey certificate for every 10 transactions"

     , cover_ 60
              (traceLength tr <= 20 * length (filter isDeRegKey certs_))
              "there is at least 1 DeRegKey certificate for every 20 transactions"

     , cover_ 60
              (traceLength tr <= 20 * length (filter isDelegation certs_))
              "there is at least 1 Delegation certificate for every 10 transactions"

     , cover_ 60
              (traceLength tr <= 40 * length (filter isGenesisDelegation certs_))
              "there is at least 1 Genesis Delegation certificate for every 40 transactions"

     , cover_ 60
              (traceLength tr <= 20 * length (filter isRegPool certs_))
              "there is at least 1 RegPool certificate for every 10 transactions"

     , cover_ 60
              (traceLength tr <= 20 * length (filter isRetirePool certs_))
              "there is at least 1 RetirePool certificate for every 20 transactions"

     , cover_ 40
              (traceLength tr <= 50 * length (filter isInstantaneousRewards certs_))
              "there is at least 1 MIR certificate for every 50 transactions"

     , cover_ 25
              (0.75 >= noCertsRatio (certsByTx txs))
              "at most 75% of transactions have no certificates"
     , cover_ 25
              (0.25 <= txScriptOutputsRatio (map (_outputs . _body) txs))
              "at least 25% of transactions have script TxOuts"
     , cover_ 10
              (0.25 <= scriptCredentialCertsRatio certs_)
              "at least 25% of `DCertDeleg` certificates have script credentials"
     ]
    where
      cover_ pc b s = cover pc b s (property ())


-- | Ratio of certificates with script credentials to the number of certificates
-- that could have script credentials.
scriptCredentialCertsRatio :: [DCert] -> Double
scriptCredentialCertsRatio certs =
  ratioInt haveScriptCerts couldhaveScriptCerts
  where haveScriptCerts =
          (length $ filter
            (\case
                DCertDeleg (RegKey (ScriptHashObj _))                  -> True
                DCertDeleg (DeRegKey (ScriptHashObj _))                -> True
                DCertDeleg (Delegate (Delegation (ScriptHashObj _) _)) -> True
                _                                                      -> False)
            certs)
        couldhaveScriptCerts =
          length $ filter (\case
                               DCertDeleg _ -> True
                               _            -> False) certs

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

-- | Transaction has script locked TxOuts
txScriptOutputsRatio :: [[TxOut]] -> Double
txScriptOutputsRatio txoutsList =
  ratioInt
   (sum (map countScriptOuts txoutsList))
   (sum (map length txoutsList))
  where countScriptOuts txouts =
          sum $ map (\case
                        TxOut (AddrBase (ScriptHashObj _) _) _ -> 1
                        _ -> 0) txouts

-- | Transforms the list and returns the ratio of lengths of
-- the transformed and original lists.
lenRatio :: ([a] -> [b]) -> [a] -> Double
lenRatio f xs
  = ratioInt (length (f xs))
             (length xs)

onlyValidLedgerSignalsAreGenerated :: Property
onlyValidLedgerSignalsAreGenerated = withMaxSuccess 200 $
    onlyValidSignalsAreGeneratedFromInitState @LEDGER testGlobals 100 (100::Word64) (Just mkGenesisLedgerState)
