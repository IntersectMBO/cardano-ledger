{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Rules.ClassifyTraces
  ( onlyValidLedgerSignalsAreGenerated
  , onlyValidChainSignalsAreGenerated
  , relevantCasesAreCovered
  , propAbstractSizeBoundsBytes
  , propAbstractSizeNotTooBig)
  where

import qualified Data.ByteString as BS
import           Data.Foldable (toList)
import           Data.Word (Word64)
import           Test.QuickCheck (Property, checkCoverage, conjoin, cover, property, withMaxSuccess)

import           Cardano.Binary (serialize')
import           ConcreteCryptoTypes (CHAIN, DCert, LEDGER, Tx, TxOut)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst), traceLength,
                     traceSignals)
import           Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState,
                     onlyValidSignalsAreGeneratedFromInitState)
import           Delegation.Certificates (isDeRegKey, isDelegation, isGenesisDelegation,
                     isInstantaneousRewards, isRegKey, isRegPool, isRetirePool)
import           Generator.ChainTrace (mkGenesisChainState)
import           Generator.LedgerTrace.QuickCheck (mkGenesisLedgerState)
import           LedgerState (txsize)
import           Test.Utils
import           TxData (pattern AddrBase, pattern DCertDeleg, pattern DeRegKey, pattern Delegate,
                     pattern Delegation, pattern RegKey, pattern ScriptHashObj, pattern TxOut,
                     _body, _certs, _outputs, _wdrls)

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

     , cover_ 40
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
              (0.1 <= txScriptOutputsRatio (map (_outputs . _body) txs))
              "at least 10% of transactions have script TxOuts"
     , cover_ 10
              (0.1 <= scriptCredentialCertsRatio certs_)
              "at least 10% of `DCertDeleg` certificates have script credentials"
     , cover_ 60
              (0.1 <= withdrawalRatio txs)
              "at least 10% of transactions have a reward withdrawal"
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

-- | Transaction has a reward withdrawal
withdrawalRatio :: [Tx] -> Double
withdrawalRatio = lenRatio (filter $ not . null . _wdrls . _body)

-- | Transforms the list and returns the ratio of lengths of
-- the transformed and original lists.
lenRatio :: ([a] -> [b]) -> [a] -> Double
lenRatio f xs
  = ratioInt (length (f xs))
             (length xs)

onlyValidLedgerSignalsAreGenerated :: Property
onlyValidLedgerSignalsAreGenerated = withMaxSuccess 200 $
    onlyValidSignalsAreGeneratedFromInitState @LEDGER testGlobals 100 (100::Word64) (Just mkGenesisLedgerState)

-- | Check that the abstract transaction size function
-- actually bounds the number of bytes in the serialized transaction.
propAbstractSizeBoundsBytes :: Property
propAbstractSizeBoundsBytes = property $ do
  let tl = 100
      numBytes = toInteger . BS.length . serialize'
  forAllTraceFromInitState @LEDGER testGlobals tl tl (Just mkGenesisLedgerState) $ \tr -> do
    let txs :: [Tx]
        txs = traceSignals OldestFirst tr
    all (\tx -> txsize tx >= numBytes tx) txs

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
      notTooBig txb = txsize txb <= acceptableMagnitude * numBytes txb
  forAllTraceFromInitState @LEDGER testGlobals tl tl (Just mkGenesisLedgerState) $ \tr -> do
    let txs :: [Tx]
        txs = traceSignals OldestFirst tr
    all notTooBig txs

onlyValidChainSignalsAreGenerated :: Property
onlyValidChainSignalsAreGenerated = withMaxSuccess 300 $
  onlyValidSignalsAreGeneratedFromInitState @CHAIN testGlobals 300 (10::Word64) (Just mkGenesisChainState)
