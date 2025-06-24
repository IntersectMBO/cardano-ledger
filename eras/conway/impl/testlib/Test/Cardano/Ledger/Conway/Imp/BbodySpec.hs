{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.BbodySpec (
  spec,
) where

import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (BlocksMade (..), Mismatch (..), ProtVer (..))
import Cardano.Ledger.Block
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure (..),
  maxRefScriptSizePerBlock,
  maxRefScriptSizePerTx,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  Event,
  ShelleyBbodyState (..),
 )
import Control.Monad (forM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (purposeIsWellformedNoDatum)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , ToExpr (Event (EraRule "BBODY" era))
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "BodyRefScriptsSizeTooBig" $ do
    Just plutusScript <- pure $ mkPlutusScript @era $ purposeIsWellformedNoDatum SPlutusV2
    let scriptSize = originalBytesSize plutusScript

    -- Determine a number of transactions and a number of times the reference script
    -- needs to be included as an input in each transaction,
    -- in order for the total to exceed the maximum allowed refScript size per block,
    -- while the refScript size per individual transaction doesn't exceed maxRefScriptSizePerTx
    txScriptCounts <-
      genNumAdditionsExceeding
        scriptSize
        maxRefScriptSizePerTx
        maxRefScriptSizePerBlock

    let mkTxWithNScripts n = do
          -- Instead of using the rootTxIn, we are creating an input for each transaction
          -- that we subsequently need to submit,
          -- so that we can submit them independently of each other.
          txIn <- freshKeyAddr_ >>= \addr -> sendCoinTo addr (Coin 8_000_000)
          refIns <- replicateM n $ produceRefScript (fromPlutusScript plutusScript)
          pure $ mkTxWithRefInputs txIn (NE.fromList refIns)

    txs <- do
      forM txScriptCounts $ \n -> do
        mkTxWithNScripts n
          >>= fixupFees
          >>= updateAddrTxWits

    let expectedTotalRefScriptSize = scriptSize * sum txScriptCounts
    predFailures <- expectLeftExpr =<< tryRunBBODY txs
    predFailures
      `shouldBe` NE.fromList
        [ injectFailure
            ( BodyRefScriptsSizeTooBig $
                Mismatch
                  { mismatchSupplied = expectedTotalRefScriptSize
                  , mismatchExpected = maxRefScriptSizePerBlock
                  }
            )
        ]
  where
    tryRunBBODY txs = do
      let txSeq = toTxSeq @era $ SSeq.fromList txs
      nes <- use impNESL
      let ls = nes ^. nesEsL . esLStateL
          pp = nes ^. nesEsL . curPParamsEpochStateL
      kh <- freshKeyHash
      slotNo <- use impLastTickG
      let bhView =
            BHeaderView
              { bhviewID = kh
              , bhviewBSize = fromIntegral $ bBodySize (ProtVer (eraProtVerLow @era) 0) txSeq
              , bhviewHSize = 0
              , bhviewBHash = hashTxSeq txSeq
              , bhviewSlot = slotNo
              }
      tryRunImpRule @"BBODY"
        (BbodyEnv pp (nes ^. chainAccountStateL))
        (BbodyState ls (BlocksMade Map.empty))
        (Block bhView txSeq)

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
