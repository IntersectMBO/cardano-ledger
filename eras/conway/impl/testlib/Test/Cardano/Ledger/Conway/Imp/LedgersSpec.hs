{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.LedgersSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (ConwayLedgersPredFailure (..))
import Cardano.Ledger.Plutus (withSLanguage)
import Data.Foldable (for_)
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (purposeIsWellformedNoDatum)

spec ::
  forall era.
  ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEDGERS" $ do
  it "BodyRefScriptsSizeTooBig" $ do
    lang <- elements $ eraLanguages @era
    plutusScript <- withSLanguage lang $ mkPlutusScript @era . purposeIsWellformedNoDatum
    let scriptSize = originalBytesSize plutusScript
    pp <- getsPParams id

    -- Determine a number of transactions and a number of times the reference script
    -- needs to be included as an input in each transaction, such that:
    --  * the total exceeds the maximum allowed refScript size per block
    --  * the refScript size per individual transaction doesn't exceed maxRefScriptSizePerTx
    --  * the number of reference inputs doesn't make the transaction exceed maxTxSize
    let
      maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
      maxRefScriptSizePerBlock = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG
      -- we're capping the number of scripts per transaction so that we don't exceed transaction ppMaxTxSizeL
      maxScriptsPerTx = 100
      maxSingle = min maxRefScriptSizePerTx (maxScriptsPerTx * scriptSize)

    txScriptCounts <-
      genNumAdditionsExceeding
        scriptSize
        maxSingle
        maxRefScriptSizePerBlock

    txs <- for txScriptCounts $ \n -> do
      -- Instead of using the rootTxIn, we're creating an input for each transaction
      -- so that transactions that will be submitted in a block
      -- can be submitted independently from the ones that prepared the
      -- reference inputs
      txIn <- freshKeyAddr_ >>= \addr -> sendCoinTo addr (Coin 100_000_000)
      refIns <- replicateM n $ produceRefScript (fromPlutusScript plutusScript)
      pure $ mkTxWithInputsAndRefInputs [txIn] refIns

    submitFailingBlock
      txs
      [ injectFailure
          ( BodyRefScriptsSizeTooBig $
              Mismatch
                { mismatchSupplied = scriptSize * sum txScriptCounts
                , mismatchExpected = maxRefScriptSizePerBlock
                }
          )
      ]

  it "BodyRefScriptsSizeTooBig with reference scripts in the same block" $ do
    lang <- elements $ eraLanguages @era
    Just plutusScript <- pure $ withSLanguage lang $ \slang ->
      mkPlutusScript @era $ purposeIsWellformedNoDatum slang
    let scriptSize = originalBytesSize plutusScript

    pp <- getsPParams id
    let
      maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
      maxRefScriptSizePerBlock = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG
      maxScriptsPerTx = 100
      maxSingle = min maxRefScriptSizePerTx (maxScriptsPerTx * scriptSize)
    txScriptCounts <-
      genNumAdditionsExceeding
        scriptSize
        maxSingle
        maxRefScriptSizePerBlock

    let
      -- These txs will be grouped into a block
      buildTxs = for_ txScriptCounts $ \n -> do
        refIns <- replicateM n $ produceRefScript (fromPlutusScript plutusScript)
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . referenceInputsTxBodyL .~ Set.fromList refIns

    withTxsInFailingBlock
      buildTxs
      [ injectFailure
          ( BodyRefScriptsSizeTooBig $
              Mismatch
                { mismatchSupplied = scriptSize * sum txScriptCounts
                , mismatchExpected = maxRefScriptSizePerBlock
                }
          )
      ]

-- Generate a list of integers such that the sum of their multiples by scale is greater than toExceed
-- and each individual value multiplied by the scale is smaller than maxSingle
genNumAdditionsExceeding :: Int -> Int -> Int -> ImpTestM era [Int]
genNumAdditionsExceeding sc maxSingle toExceed = go 0 []
  where
    go tot !acc
      | tot > toExceed = return $ reverse acc
      | otherwise = do
          x <- choose (1, min (toExceed `div` sc) (maxSingle `div` sc))
          let !newTot = tot + x * sc
          go newTot (x : acc)
