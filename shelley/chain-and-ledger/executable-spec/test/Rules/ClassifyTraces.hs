{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Rules.ClassifyTraces where

import           Data.Foldable (toList)
import qualified Data.Set as Set
import           Hedgehog (Property, cover, forAll, property, withTests)

import           Control.State.Transition.Generator (traceOfLengthWithInitState)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst), traceLength,
                     traceSignals)

import           Delegation.Certificates (isDeRegKey, isRegKey, isRegPool)
import           Generator.Core (mkGenesisLedgerState)
import           Generator.LedgerTrace ()
import           MockTypes (DCert, LEDGER, Tx)
import           TxData (_body, _certs, _inputs, _outputs)

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withTests 500 $ property $ do
  let tl = 100
  tr <- forAll (traceOfLengthWithInitState @LEDGER tl mkGenesisLedgerState)

  let txs = traceSignals OldestFirst tr
      certs_ = allCerts txs

  cover 75
        "there is at least 1 certificate for every 5 transactions"
        (traceLength tr <= 5 * length certs_)

  cover 75
        "there is at least 1 RegKey certificate for every 5 transactions"
        (traceLength tr <= 5 * length (filter isRegKey certs_))

  cover 75
        "there is at least 1 DeRegKey certificate for every 5 transactions"
        (traceLength tr <= 5 * length (filter isDeRegKey certs_))

  cover 75
        "there is at least 1 RegPool certificate for every 20 transactions"
        (traceLength tr <= 20 * length (filter isRegPool certs_))

  cover 25
        "at most 75% of transactions have no certificates"
        (0.75 >= noCertsRatio (certsByTx txs))

-- | Extract the certificates from the transactions
certsByTx :: [Tx] -> [[DCert]]
certsByTx txs = (toList . _certs . _body) <$> txs

-- | Flattended list of DCerts for the given transactions
allCerts :: [Tx] -> [DCert]
allCerts = concat . certsByTx

-- | Ratio of the number of empty certificate groups and the number of groups
noCertsRatio :: [[DCert]] -> Double
noCertsRatio = lenRatio (filter null)

-- | Returns the average number of inputs and outputs for a list of transactions.
avgInputsOutputs :: [Tx] -> (Double, Double)
avgInputsOutputs txs
  = case length txs of
      0 -> (0,0)
      n -> ( nrInputs  / fromIntegral n
           , nrOutputs / fromIntegral n)
  where
    nrInputs = fromIntegral $ sum (Set.size . _inputs . _body <$> txs)
    nrOutputs = fromIntegral $ sum (length . _outputs . _body <$> txs)

ratioInt :: Int -> Int -> Double
ratioInt x y
  = fromIntegral x / fromIntegral y

-- | Transforms the list and returns the ratio of lengths of
-- the transformed and original lists.
lenRatio :: ([a] -> [b]) -> [a] -> Double
lenRatio f xs
  = ratioInt (length (f xs))
             (length xs)
