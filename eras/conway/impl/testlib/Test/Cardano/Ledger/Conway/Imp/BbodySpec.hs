{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.BbodySpec (
  spec,
) where

import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (BlocksMade (..), ProtVer (..))
import Cardano.Ledger.Block
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure (..),
  maxRefScriptSizePerBlock,
  maxRefScriptSizePerTx,
 )
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.SafeHash (originalBytesSize)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyState (..),
 )
import Cardano.Ledger.TxIn
import Control.Monad (forM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsWithDatum)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , EraSegWits era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "BBODY" $ do
  it "BodyRefScriptsSizeTooBig" $ do
    Just (script :: Script era) <- pure largeScript
    let scriptSize = originalBytesSize script

    -- Determine a number of transactions and a number of times the reference script
    -- needs to be included as an input in each transaction,
    -- in order for the total to exceed the maximum allowed refScript size per block,
    -- while the refScript size per individual transaction doesn't exceed maxRefScriptSizePerTx
    txScriptCounts <-
      genNumAdditionsExceeding
        scriptSize
        maxRefScriptSizePerTx
        maxRefScriptSizePerBlock

    let expectedTotalRefScriptSize = scriptSize * sum txScriptCounts
    txs <- do
      -- Instead of using the rootTxIn, we are creating an input for each transaction
      -- that we subsequently need to submit,
      -- so that we can submit them independently of each other.
      forM txScriptCounts $ \n -> do
        txIn <- mkTxIn
        mkTxWithNScripts txIn script n
          >>= fixupFees
          >>= updateAddrTxWits

    let txSeq = toTxSeq @era $ SSeq.fromList txs
    nes <- use impNESL
    let ls = nes ^. nesEsL . esLStateL
        pp = nes ^. nesEsL . curPParamsEpochStateL
        account = nes ^. nesEsL . esAccountStateL
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
    Left predFailures <-
      tryRunImpRule @"BBODY"
        (BbodyEnv pp account)
        (BbodyState ls (BlocksMade Map.empty))
        (UnsafeUnserialisedBlock bhView txSeq)
    predFailures
      `shouldBe` NE.fromList
        [ injectFailure
            ( BodyRefScriptsSizeTooBig
                expectedTotalRefScriptSize
                maxRefScriptSizePerBlock
            )
        ]
  where
    mkTxIn :: ImpTestM era (TxIn (EraCrypto era))
    mkTxIn = do
      addr <- freshKeyAddr_
      sendCoinTo addr (Coin 1_000_000)

    largeScript :: Maybe (Script era)
    largeScript = do
      script <- mkPlutusScript @era $ alwaysFailsWithDatum SPlutusV2
      pure $ fromPlutusScript script

    mkTxWithNScripts :: TxIn (EraCrypto era) -> Script era -> Int -> ImpTestM era (Tx era)
    mkTxWithNScripts txIn script n = do
      txIns <- replicateM n (produceRefScript script)
      pure $
        mkBasicTx $
          mkBasicTxBody
            & referenceInputsTxBodyL .~ Set.fromList txIns
            & inputsTxBodyL .~ [txIn]

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
