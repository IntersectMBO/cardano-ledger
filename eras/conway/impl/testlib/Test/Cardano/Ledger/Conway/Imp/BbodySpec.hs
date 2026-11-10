{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.BbodySpec (spec, conwayOnlySpec) where

import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (Mismatch (..), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.BlockBody (ConwayBlockBodyLimits (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (ConwayBbodyPredFailure (..))
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Plutus (hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersEnv (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import Cardano.Ledger.Slot (epochFromSlot)
import Cardano.Ledger.State (chainAccountStateL)
import Control.State.Transition
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsWithDatum,
  purposeIsWellformedNoDatum,
 )

conwayOnlySpec ::
  forall era.
  ( ConwayEraImp era
  , State (EraRule "LEDGERS" era) ~ LedgerStateWithLimits era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
conwayOnlySpec = describe "BBODY" $ do
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

  it "Reference script sizes accumulated by LEDGERS" $ do
    script <- RequireSignature @era <$> freshKeyHash
    let scriptSize = originalBytesSize script
    scriptSpendIn <- impAddNativeScript script >>= produceScript
    scriptSpendIn2 <- impAddNativeScript script >>= produceScript

    -- We want to verify that LEDGERS accumulates the total size of the reference scripts
    -- used by a list of transactions.
    -- To do this, we submit the transactions one by one, checking the size each contributes
    -- against the UTxO it is applied to - which is the one LEDGERS will measure it against -
    -- and then restore the original state, meaning the transactions are not actually applied.
    -- Finally, we run LEDGERS over all of them and check that the size it accumulates is the
    -- sum of the individual ones.
    txsWithSizes <- simulateThenRestore $ do
      let expectRefScriptSize expectedSize txAction = do
            utxo <- getUTxO
            tx <- txAction
            txNonDistinctRefScriptsSize utxo tx `shouldBe` expectedSize
            pure (tx, expectedSize)

      -- producing a reference script contributes nothing
      refScriptTx1 <-
        expectRefScriptSize 0 $
          produceRefScriptsTx (fromNativeScript script :| [])
      let refScriptTx1In = txInAt 0 (fst refScriptTx1)
      refScriptTx2 <-
        expectRefScriptSize 0 $
          produceRefScriptsTx (fromNativeScript script :| [])
      -- spend script using the reference script
      spendScriptWithRefScriptTx <-
        expectRefScriptSize scriptSize $
          submitTx $
            mkTxWithInputsAndRefInputs [scriptSpendIn] [refScriptTx1In]
      -- spend using two ref inputs, which are counted separately even though they hold the
      -- same script
      spendScriptWithTwoRefScriptsTx <-
        expectRefScriptSize (2 * scriptSize) $
          submitTx $
            mkTxWithInputsAndRefInputs [scriptSpendIn2] [refScriptTx1In, txInAt 0 (fst refScriptTx2)]
      -- spend the root utxo
      rootIn <- fst <$> getImpRootTxOut
      spendRootUtxoTx <-
        expectRefScriptSize scriptSize $
          submitTx $
            mkTxWithInputsAndRefInputs [rootIn] [refScriptTx1In]
      -- spending the output that holds the reference script also counts it, since the size
      -- is measured over inputs and reference inputs alike
      spendRefScriptTx <-
        expectRefScriptSize scriptSize $
          submitTx $
            mkTxWithInputsAndRefInputs [refScriptTx1In] []

      pure
        [ refScriptTx1
        , refScriptTx2
        , spendScriptWithRefScriptTx
        , spendScriptWithTwoRefScriptsTx
        , spendRootUtxoTx
        , spendRefScriptTx
        ]

    -- Applying them all through LEDGERS accumulates exactly the sum of those sizes
    let (txs, sizes) = unzip txsWithSizes
    limits <- accumulateBlockBodyLimits txs
    cbblRefScriptsSize limits `shouldBe` sum sizes

spec ::
  forall era.
  ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "BBODY" $ do
  it "Use a reference script in a collateral output" $ do
    -- produce an utxo with a failing script
    lang <- elements $ eraLanguages @era
    failingPlutusTxIn <- withSLanguage lang $ \slang -> do
      let plutus = alwaysFailsWithDatum slang
      produceScript $ hashPlutusScript plutus

    -- produce a utxo with a succeeding script
    script <- RequireSignature @era <$> freshKeyHash
    scriptTxIn <- impAddNativeScript script >>= produceScript
    let scriptSize = originalBytesSize script

    -- prepare a txout with the succeeding script as reference script
    ProtVer pv _ <- getProtVer
    collRefScriptTxOut <- do
      addr <-
        if pv < natVersion @12
          then freshKeyAddr_
          else freshKeyAddrNoPtr_
      pure $ mkBasicTxOut addr mempty & referenceScriptTxOutL .~ pure (fromNativeScript script)

    -- Create the collateral input up front: the fixup would otherwise create it inside the
    -- simulation below, where it would be rolled back before LEDGERS re-applies the transactions
    collateralInput <- makeCollateralInput

    txs <- simulateThenRestore $ do
      -- submit an invalid transaction which attempts to consume the failing script
      -- and specifies as collateral return the txout with reference script
      utxo <- getUTxO
      createCollateralTx <-
        -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1029
        -- TODO: Re-enable after issue is resolved, by removing this override
        withDisabledPostSubmitTxHook $
          submitPhase2Invalid $
            mkBasicTx
              ( mkBasicTxBody
                  & inputsTxBodyL .~ [failingPlutusTxIn]
                  & collateralInputsTxBodyL .~ [collateralInput]
                  & collateralReturnTxBodyL .~ pure collRefScriptTxOut
              )
      -- the transaction that produces the collateral output contributes nothing itself
      txNonDistinctRefScriptsSize utxo createCollateralTx `shouldBe` 0

      -- consume the script, passing the collateral return as reference input
      let refScriptTxIn = txInAt 1 createCollateralTx
      useCollateralTx <- submitTx $ mkTxWithInputsAndRefInputs [scriptTxIn] [refScriptTxIn]
      pure [createCollateralTx, useCollateralTx]

    -- Applying the invalid transaction creates its collateral return, so the reference
    -- script it carries is counted for the next transaction in the block
    limits <- accumulateBlockBodyLimits txs
    cbblRefScriptsSize limits `shouldBe` scriptSize

-- | Apply the transactions through the LEDGERS rule, from the current ledger state, and
-- return the block body limits that it accumulates for them.
accumulateBlockBodyLimits ::
  forall era.
  ( ShelleyEraImp era
  , State (EraRule "LEDGERS" era) ~ LedgerStateWithLimits era
  ) =>
  [Tx TopTx era] ->
  ImpTestM era (BlockBodyLimits era)
accumulateBlockBodyLimits txs = do
  nes <- getsNES id
  slotNo <- use impCurSlotNoG
  epochNo <- runShelleyBase $ epochFromSlot slotNo
  let ledgersEnv =
        LedgersEnv
          slotNo
          epochNo
          (nes ^. nesEsL . curPParamsEpochStateL)
          (nes ^. chainAccountStateL)
  (LedgerStateWithLimits _ limits, _) <-
    expectRightExpr
      =<< tryRunImpRule @"LEDGERS"
        ledgersEnv
        (LedgerStateWithLimits (nes ^. nesEsL . esLStateL) mempty)
        (Seq.fromList txs)
  pure limits

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
