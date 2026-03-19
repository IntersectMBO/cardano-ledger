{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.BbodySpec (spec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (Mismatch (..), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure (..),
  totalRefScriptSizeInBlock,
 )
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import Data.Foldable (for_)
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsNoDatum, purposeIsWellformedNoDatum)

spec ::
  forall era.
  ConwayEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "BodyRefScriptsSizeTooBig" $ do
    plutusScript <- mkPlutusScript @era $ purposeIsWellformedNoDatum SPlutusV2
    let scriptSize = originalBytesSize plutusScript
    pp <- getsPParams id

    -- Determine a number of transactions and a number of times the reference script
    -- needs to be included as an input in each transaction,
    -- in order for the total to exceed the maximum allowed refScript size per block,
    -- while the refScript size per individual transaction doesn't exceed maxRefScriptSizePerTx
    let
      maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
      maxRefScriptSizePerBlock = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG

    txScriptCounts <-
      genNumAdditionsExceeding
        scriptSize
        maxRefScriptSizePerTx
        maxRefScriptSizePerBlock

    txs <- for txScriptCounts $ \n -> do
      -- Instead of using the rootTxIn, we're creating an input for each transaction
      -- so that transactions that will be submitted in a block
      -- can be submitted independently from the ones that prepared the
      -- reference inputs
      txIn <- freshKeyAddr_ >>= \addr -> sendCoinTo addr (Coin 100_000_000)
      refIns <- replicateM n $ produceRefScript (fromPlutusScript plutusScript)
      pure $ mkTxWithRefInputs txIn (NE.fromList refIns)

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

  it "BodyRefScriptsSizeTooBig with reference scripts in the same block" $
    whenMajorVersionAtLeast @11 $ do
      Just plutusScript <- pure $ mkPlutusScript @era $ purposeIsWellformedNoDatum SPlutusV2
      let scriptSize = originalBytesSize plutusScript

      pp <- getsPParams id
      let
        maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
        maxRefScriptSizePerBlock = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG
      txScriptCounts <-
        genNumAdditionsExceeding
          scriptSize
          maxRefScriptSizePerTx
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

  it "totalRefScriptSizeInBlock" $ do
    script <- RequireSignature @era <$> freshKeyHash
    let scriptSize = originalBytesSize script
    scriptSpendIn <- impAddNativeScript script >>= produceScript
    scriptSpendIn2 <- impAddNativeScript script >>= produceScript
    protVer <- getsPParams ppProtocolVersionL

    -- We want to verify that the total size of reference scripts in a list of transactions
    -- remains unchanged before and after applying them to the ledger state.
    -- To do this, we generate the expected transactions, simulate submitting them to obtain
    -- their individual reference script sizes, and then restore the original state -
    -- meaning the transactions are not actually applied.
    -- Finally, we check that the accumulated sizes from both before and after match.
    txsWithSizes <- simulateThenRestore $ do
      let mkTxWithExpectedSize expectedSize txAction = do
            tx <- txAction
            totalRefScriptSizeInBlock protVer [tx] <$> getUTxO `shouldReturn` expectedSize
            pure (tx, expectedSize)

      -- submit reference scripts
      refScriptTx1 <-
        mkTxWithExpectedSize 0 $
          produceRefScriptsTx (fromNativeScript script :| [])
      let refScriptTx1In = txInAt 0 (fst refScriptTx1)
      refScriptTx2 <-
        mkTxWithExpectedSize 0 $
          produceRefScriptsTx (fromNativeScript script :| [])
      -- spend script using the reference script
      spendScriptWithRefScriptTx <-
        mkTxWithExpectedSize scriptSize $
          submitTxWithRefInputs scriptSpendIn [refScriptTx1In]
      -- spend using two ref inputs
      spendScriptWithTwoRefScriptsTx <-
        mkTxWithExpectedSize (2 * scriptSize) $
          submitTxWithRefInputs scriptSpendIn2 [refScriptTx1In, txInAt 0 (fst refScriptTx2)]
      -- spend the root utxo
      rootIn <- fst <$> getImpRootTxOut
      spendRootUtxoTx <-
        mkTxWithExpectedSize scriptSize $
          submitTxWithRefInputs rootIn [refScriptTx1In]
      -- spend the reference script itself
      -- We must check the size without submitting the transaction,
      -- since applying it removes the reference script from the UTxO
      spendRefScriptTx <-
        mkTxWithExpectedSize scriptSize $
          fixupTx $
            mkTxWithRefInputs refScriptTx1In (NE.fromList [refScriptTx1In])

      let txsWithRefScriptSizes =
            [ refScriptTx1
            , refScriptTx2
            , spendScriptWithRefScriptTx
            , spendScriptWithTwoRefScriptsTx
            , spendRootUtxoTx
            , spendRefScriptTx
            ]

      -- check and return the accumulated reference script size of all transactions,
      -- so we can check that the same sum for the unapplied transactions matches
      let (txs, sizes) = unzip txsWithRefScriptSizes
      totalRefScriptSizeInBlock protVer (SSeq.fromList txs) <$> getUTxO `shouldReturn` sum sizes

      pure txsWithRefScriptSizes

    -- for each prefix of the list, the accumulated sum should match the sum of the applied transactions
    for_ (drop 1 $ inits txsWithSizes) $ \prefix -> do
      let (txs, sizes) = unzip prefix
          expectedSize = if isPostV10 protVer then sum sizes else 0
      totalRefScriptSizeInBlock protVer (SSeq.fromList txs) <$> getUTxO `shouldReturn` expectedSize

  -- disabled in conformance because submiting phase2-invalid transactions are not supported atm
  -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/910
  -- TODO: Re-enable after issue is resolved, by removing this override
  disableInConformanceIt "Use a reference script in a collateral output" $ do
    protVer <- getsPParams ppProtocolVersionL

    -- produce an utxo with a failing script
    failingPlutusTxIn <- do
      let plutus = alwaysFailsNoDatum SPlutusV3
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

    txs <- simulateThenRestore $ do
      -- submit an invalid transaction which attempts to consume the failing script
      -- and specifies as collateral return the txout with reference script
      createCollateralTx <-
        submitPhase2Invalid $
          mkBasicTx
            ( mkBasicTxBody
                & inputsTxBodyL .~ [failingPlutusTxIn]
                & collateralReturnTxBodyL .~ pure collRefScriptTxOut
            )
      totalRefScriptSizeInBlock protVer [createCollateralTx] <$> getUTxO `shouldReturn` 0

      -- consume the script, passing the output from the previous collateral as reference input
      let refScriptTxIn = txInAt 1 createCollateralTx
      useCollateralTx <- submitTxWithRefInputs scriptTxIn [refScriptTxIn]
      totalRefScriptSizeInBlock protVer [createCollateralTx, useCollateralTx]
        <$> getUTxO `shouldReturn` scriptSize
      pure [createCollateralTx, useCollateralTx]

    totalRefScriptSizeInBlock protVer (SSeq.fromList txs)
      <$> getUTxO
        `shouldReturn` (if isPostV10 protVer then scriptSize else 0)
  where
    isPostV10 protVer = pvMajor protVer >= natVersion @11

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
