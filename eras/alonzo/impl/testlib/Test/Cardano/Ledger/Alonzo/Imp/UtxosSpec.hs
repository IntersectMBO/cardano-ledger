{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (LedgerTxInfo (..), toPlutusTxInfoForPurpose)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  CollectError (NoCostModel),
  TransactionScriptFailure (RedeemerPointsToUnknownScriptHash),
  evalTxExUnits,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo (transTxOut)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..), eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Inject (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), filterMultiAsset)
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  PlutusLanguage,
  SLanguage (..),
  hashData,
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Plutus.TxInfo (transSafeHash, transTxIn)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Shelley.Scripts (ShelleyEraScript (..))
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Control.Monad.Reader (asks)
import Data.Either (isLeft)
import qualified Data.Foldable as F
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Lens.Micro (set, to, (%~), (&), (.~), (<>~), (^.), _2)
import Lens.Micro.Mtl (use)
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsWithDatum,
  alwaysSucceedsWithDatum,
  datumIsWellformed,
  inputsOutputsAreNotEmptyWithDatum,
  purposeIsWellformedWithDatum,
  redeemerSameAsDatum,
  txInfoTranslationSpecScript,
 )

spec :: forall era. AlonzoEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  it
    "transaction validity interval has closed upper bound when protocol version < 9 and open otherwise"
    $ do
      ei <- use $ impGlobalsL . to epochInfo
      ss@(SystemStart sysStart) <- use $ impGlobalsL . to systemStart
      SlotNo currentSlot <- use impCurSlotNoG
      protVer <- getProtVer
      utxo <- getUTxO
      let txValidity = 7200
          -- We must provide a non-Nothing upper bound so that the "closed" vs "open" case can be tested.
          interval = ValidityInterval SNothing $ SJust $ SlotNo $ currentSlot + txValidity
          startPOSIX = floor $ utcTimeToPOSIXSeconds sysStart
          expectedUpperBound = (startPOSIX + fromIntegral (currentSlot + txValidity)) * 1000
          tx :: Tx TopTx era
          tx = mkBasicTx mkBasicTxBody & bodyTxL . vldtTxBodyL .~ interval
          lti =
            LedgerTxInfo
              { ltiProtVer = protVer
              , ltiEpochInfo = ei
              , ltiSystemStart = ss
              , ltiUTxO = utxo
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
      case toPlutusTxInfoForPurpose SPlutusV1 lti (SpendingPurpose AsPurpose) of
        Left e -> assertFailure $ "No translation error was expected, but got: " <> show e
        Right txInfo ->
          PV1.txInfoValidRange txInfo
            `shouldBe` PV1.Interval
              (PV1.LowerBound PV1.NegInf True)
              ( PV1.UpperBound
                  ( PV1.Finite
                      (PV1.POSIXTime expectedUpperBound)
                  )
                  (pvMajor protVer < natVersion @9) -- The upper bound.
              )

  forM_ (eraLanguages @era) $ \lang ->
    describe (show lang) $
      withSLanguage lang $ \slang -> do
        let redeemerSameAsDatumHash = hashPlutusScript $ redeemerSameAsDatum slang
            alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang

        let scripts =
              [ ("redeemerSameAsDatum", redeemerSameAsDatum)
              , ("purposeIsWellformedWithDatum", purposeIsWellformedWithDatum)
              , ("datumIsWellformed", datumIsWellformed)
              , ("inputsOutputsAreNotEmptyWithDatum", inputsOutputsAreNotEmptyWithDatum)
              ]

        describe "ExUnits" $ do
          it "Calculate ExUnits" $ do
            let
              overrideExUnits tx = do
                pp <- getsNES $ nesEsL . curPParamsEpochStateL
                utxo <- getUTxO
                Globals {epochInfo, systemStart} <- use impGlobalsL
                purposeUnits <-
                  either (fail . show) pure . sequence $
                    evalTxExUnits pp tx utxo epochInfo systemStart
                pure $ tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ spliceUnits purposeUnits
              spliceUnits =
                Map.merge
                  Map.dropMissing -- Ignore purposes not already in the redeemers
                  Map.preserveMissing -- Don't touch purposes not being updated
                  (Map.zipWithMatched $ \_ -> set _2) -- Replace the units, keep the datum
              redoAddrWits = updateAddrTxWits . (witsTxL . addrTxWitsL .~ mempty)

            txIn <- produceScript alwaysSucceedsWithDatumHash
            withPostFixup (overrideExUnits >=> fixupPPHash >=> redoAddrWits) $
              submitTx_ $
                mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

          it "Attempt to calculate ExUnits with an invalid tx" $ do
            txIn <- produceScript alwaysSucceedsWithDatumHash
            let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

            txFixed <- (tx &) =<< asks iteFixup
            logToExpr txFixed

            let
              twiddleIx (SJust (SpendingPurpose (AsIx 0))) = SpendingPurpose (AsIx 1)
              twiddleIx _ = SpendingPurpose (AsIx 0)
              badPurpose =
                twiddleIx $
                  redeemerPointer (txFixed ^. bodyTxL) (SpendingPurpose $ AsItem txIn)
              du = (Data $ PV1.I 42, ExUnits 5000 5000)
              txBorked =
                txFixed
                  & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert badPurpose du
            logToExpr txBorked

            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            utxo <- getUTxO
            Globals {epochInfo, systemStart} <- use impGlobalsL
            let report = evalTxExUnits pp txBorked utxo epochInfo systemStart
            logToExpr report

            Map.filter isLeft report
              `shouldBe` Map.singleton badPurpose (Left (RedeemerPointsToUnknownScriptHash badPurpose))

        describe "Spending scripts with a Datum" $ do
          forM_ scripts $ \(name, script) -> do
            it name $ do
              let sHash = hashPlutusScript (script slang)
              txIn0 <- produceScript sHash
              submitTxAnn_ "Submit a transaction that consumes the script output" $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL
                    .~ Set.singleton txIn0
              passEpoch

        it "Valid transaction marked as invalid" $ do
          let tx = mkBasicTx mkBasicTxBody & isValidTxL .~ IsValid False
          submitFailingTx tx [injectFailure (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]

        it "Invalid transaction marked as valid" $ do
          txIn <- produceScript . hashPlutusScript $ alwaysFailsWithDatum slang
          submitPhase2Invalid_ $ mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

        it "Invalid plutus script fails in phase 2" $ do
          txIn0 <- produceScript redeemerSameAsDatumHash
          exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
          submitTxAnn_ "Submitting consuming transaction" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0
              & isValidTxL .~ IsValid False
              & witsTxL . rdmrsTxWitsL . unRedeemersL
                .~ Map.singleton (mkSpendingPurpose $ AsIx 0) (Data $ P.I 32, exUnits)

        describe "Scripts pass in phase 2" $ do
          let scripts' = drop 1 scripts
          forM_ scripts' $ \(name, script) -> do
            it name $ do
              let sHash = hashPlutusScript (script slang)
              txIn0 <- produceScript sHash
              submitTxAnn_ "Submitting consuming transaction" $
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0

        it "No cost model" $ do
          txIn <- produceScript alwaysSucceedsWithDatumHash
          let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL <>~ [txIn]
          modifyPParams $ ppCostModelsL .~ mempty
          submitFailingTx
            tx
            [injectFailure (CollectErrors [NoCostModel lang])]

        it "Scripts with bootstrap addresses pass" $
          when (eraProtVerLow @era <= eraProtVerHigh @AlonzoEra) $ do
            mkTxWithPlutusAndBootstrapAddress slang >>= submitTx_

        txInfoTranslationSpec slang

-- | Tests the full Plutus evaluation pipeline E2E (translation -> script
-- execution -> ledger acceptance) by passing expected values via the redeemer
-- and verifying them on-chain.
--
-- The script uses a 'Constr' redeemer @(tag :: Integer, payload :: [BuiltinData])@
-- where the tag selects which TxInfo field to check.
txInfoTranslationSpec ::
  forall era l.
  ( AlonzoEraImp era
  , PlutusLanguage l
  ) =>
  SLanguage l ->
  SpecWith (ImpInit (LedgerSpec era))
txInfoTranslationSpec slang =
  describe "TxInfo translation" $ do
    -- FIXME How to run this as property test?
    it "txInfoInputs" $ alonzoBasedTxInfoInputsTranslationSpec slang
    it "txInfoOutputs" $ alonzoBasedTxInfoOutputsTranslationSpec slang

-- | Tag 0: verify txInfoInputs outRefs match expected refs
alonzoBasedTxInfoInputsTranslationSpec ::
  forall era l.
  (AlonzoEraImp era, PlutusLanguage l) =>
  SLanguage l ->
  ImpTestM era ()
alonzoBasedTxInfoInputsTranslationSpec slang =
  alonzoBasedTxInfoTranslationSpec slang id $ \tx -> do
    let inputs = Set.toList $ tx ^. bodyTxL . inputsTxBodyL
        txOutRefsData = case slang of
          SPlutusV3 -> PV1.toData $ fmap transV3TxIn inputs
          _ -> PV1.toData $ fmap transTxIn inputs
    pure $ PV1.Constr 0 [txOutRefsData]
  where
    -- Translate a TxIn to PV3.TxOutRef. PV3 uses a different TxId encoding
    -- (newtype-derived, no Constr wrapper) compared to V1/V2.
    transV3TxIn :: TxIn -> PV3.TxOutRef
    transV3TxIn (TxIn txid (TxIx txIx)) =
      PV3.TxOutRef
        (PV3.TxId (transSafeHash (unTxId txid)))
        (toInteger txIx)

-- | Tag 1: verify txInfoOutputs match expected outputs
alonzoBasedTxInfoOutputsTranslationSpec ::
  forall era l.
  (AlonzoEraImp era, PlutusLanguage l) =>
  SLanguage l ->
  ImpTestM era ()
alonzoBasedTxInfoOutputsTranslationSpec slang = do
  -- Generate a minting policy id in order to create `TxOut`s that have multiple
  -- assets in the value.
  mintPolicyId <- genMintPolicyId
  txOuts <- resize 50 $ listOf1 (genTxOut mintPolicyId)

  -- Compute the number of 'mintPolicyId' tokens that are going to be minted in
  -- the tx, so that the tx balances.
  let totalMint = getTotalMintedValue mintPolicyId txOuts
      modifyTx tx =
        tx
          & bodyTxL . outputsTxBodyL .~ fromList txOuts
          & bodyTxL . mintTxBodyL .~ totalMint

  alonzoBasedTxInfoTranslationSpec slang modifyTx $ \tx -> do
    let outputs = F.toList $ tx ^. bodyTxL . outputsTxBodyL
        v1Outputs = mapMaybe Alonzo.transTxOut outputs
        outputsData = case slang of
          SPlutusV1 -> PV1.toData v1Outputs
          -- For future Plutus versions that are included in Alonzo based
          -- transactions, we need to translate 'PV1.TxOut' to
          -- 'PV2.TxOut'. This is safe, because 'PV2.TxOut' extends `PV1.TxOut'
          -- by simply adding inline datums and reference scripts.
          _ -> PV1.toData $ fmap v1TxOutToV2TxOut v1Outputs
    pure $ PV1.Constr 1 [outputsData]
  where
    getTotalMintedValue mintPolicyId =
      F.foldMap
        ( filterMultiAsset (\p _ _ -> p == mintPolicyId)
            . (\case MaryValue _ ma -> ma)
            . (^. valueTxOutL)
        )
    genTxOut mintPolicyId = do
      addr <- genAddr
      amount <- choose (2_000_000, 100_000_000)
      ma <- oneof [pure mempty, genPositiveMultiAsset mintPolicyId]
      datumM <- arbitrary @(StrictMaybe (Data era))
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      pure $
        ensureMinCoinTxOut pp $
          mkBasicTxOut addr (inject (MaryValue (Coin amount) ma))
            & dataHashTxOutL .~ fmap hashData datumM
    genAddr :: ImpTestM era Addr
    genAddr
      -- We can only use Bootstrap addresses in AlonzoEra.
      | eraProtVerLow @era < natVersion @7 =
          oneof
            [ freshKeyAddr_
            , AddrBootstrap <$> freshBootstapAddress
            ]
      | otherwise = freshKeyAddr_
    genPositiveMultiAsset :: PolicyID -> ImpTestM era MultiAsset
    genPositiveMultiAsset policyId = do
      assetName <- arbitrary
      amount <- choose (1, 1000)
      pure $ MultiAsset $ Map.singleton policyId $ Map.singleton assetName amount
    genMintPolicyId :: ImpTestM era PolicyID
    genMintPolicyId = do
      keyHash <- freshKeyHash
      PolicyID <$> impAddNativeScript (mkRequireSignature keyHash)
    -- Convert a V1 TxOut to a V2 TxOut. Valid for outputs without inline datums
    -- or reference scripts.
    v1TxOutToV2TxOut :: PV1.TxOut -> PV2.TxOut
    v1TxOutToV2TxOut (PV1.TxOut addr val mDH) =
      PV2.TxOut addr val (maybe PV2.NoOutputDatum PV2.OutputDatumHash mDH) Nothing

-- | Common harness for TxInfo translation tests. Takes a tx modifier and a
-- function that computes the tagged redeemer payload from the post-fixup tx.
alonzoBasedTxInfoTranslationSpec ::
  forall era l.
  (AlonzoEraImp era, PlutusLanguage l) =>
  SLanguage l ->
  (Tx TopTx era -> Tx TopTx era) ->
  (Tx TopTx era -> ImpTestM era PV1.Data) ->
  ImpTestM era ()
alonzoBasedTxInfoTranslationSpec slang modifyTx mkRedeemerData = do
  txIn <- produceScript . hashPlutusScript $ txInfoTranslationSpecScript slang
  withCustomFixup (\_ -> customFixup txIn) $
    submitTxAnn_ "Submitting consuming transaction" $
      modifyTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
  where
    -- Based on alonzoFixupTx, but with overrideRedeemer inserted at two points.
    -- The redeemer must be set before alonzoFixupFees so the fee calculation
    -- accounts for the actual (potentially large) redeemer size. It must be set
    -- again after fixupRedeemers because alonzoFixupFees adds a change output,
    -- which changes the outputs the redeemer encodes.
    customFixup txIn =
      addNativeScriptTxWits
        >=> fixupAuxDataHash
        >=> addCollateralInput
        >=> addRootTxIn
        >=> fixupScriptWits
        >=> fixupOutputDatums
        >=> fixupDatums
        >=> fixupRedeemerIndices
        >=> fixupTxOuts
        -- Set redeemer before fee calculation in order to get more accurate fee
        -- value.
        >=> insertRedeemer txIn
        >=> alonzoFixupFees
        >=> fixupRedeemers
        -- Update redeemer after change output is added in the transaction
        -- outputs. Necessary for the TxInfo translation tests.
        >=> updateRedeemer txIn
        >=> fixupPPHash
        >=> updateAddrTxWits
    insertRedeemer txIn tx = do
      redeemerData <- Data <$> mkRedeemerData tx
      case redeemerPointer (tx ^. bodyTxL) (SpendingPurpose $ AsItem txIn) of
        SNothing -> assertFailure "Could not find spending purpose for txIn"
        SJust purpose -> do
          -- Unfortunately, `fixupRedeemer` doesn't set the maxExUnits for
          -- redeemers already part of the tx redeemers, so we need to do it
          -- here.
          maxExUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
          let update = Map.insertWith (\(d, _) (_, eu) -> (d, eu)) purpose (redeemerData, maxExUnits)
          pure $ tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ update
    updateRedeemer txIn tx = do
      redeemerData <- Data <$> mkRedeemerData tx
      case redeemerPointer (tx ^. bodyTxL) (SpendingPurpose $ AsItem txIn) of
        SNothing -> assertFailure "Could not find spending purpose for txIn"
        SJust purpose ->
          pure $
            tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.adjust (\(_, eu) -> (redeemerData, eu)) purpose
