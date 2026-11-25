{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.PlutusSpec (spec) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  NonZero,
  PositiveInterval,
  ProtVer (..),
  StrictMaybe,
  UnitInterval,
  getVersion,
  strictMaybeToMaybe,
  unboundRational,
 )
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Cardano.Slotting.Slot (SlotInterval (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Numeric.Natural (Natural)
import PlutusCore.Default (DefaultFun (..), DefaultUni)
import PlutusCore.MkPlc (mkConstant, mkIterAppNoAnn)
import qualified PlutusCore.Version as PLC
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Plutus (plutusLanguageOf, showPlcVersion, testingEvaluationContext)
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundTripPlutusDataSpec, sameAsToBuiltinDataSpec)
import UntypedPlutusCore (DeBruijn (..), Program (Program), Term (..))
import UntypedPlutusCore.MkUPlc (UVarDecl (..), mkIterLamAbs)

spec :: Spec
spec = do
  describe "Plutus" $ do
    costModelsSpec
    exUnitsSpec
    dataSpec
    decodePlutusRunnableSpec
    evaluatePlutusSpec

costModelsSpec :: Spec
costModelsSpec = do
  describe "CostModels" $ do
    prop "flattenCostModels . mkCostModelsLenient" $ \valid unknown -> do
      let cms1Flat = flattenCostModels valid <> unknown
          cms2Flat = unknown <> flattenCostModels valid
      cms1 <- mkCostModelsLenient cms1Flat
      cms2 <- mkCostModelsLenient cms2Flat
      flattenCostModels cms1 `shouldBe` cms1Flat
      flattenCostModels cms2 `shouldBe` cms2Flat

exUnitsSpec :: Spec
exUnitsSpec = do
  describe "ExUnits" $ do
    prop "Round-trip to ExBudget" exUnitsToExBudgetRoundTrip
    prop "Round-trip from ExBudget" exBudgetToExUnitsRoundTrip

-- ExUnits should remain intact when translating to and from the Plutus ExBudget type
exUnitsToExBudgetRoundTrip :: Gen Property
exUnitsToExBudgetRoundTrip = do
  e <- arbitrary
  let result = exBudgetToExUnits $ transExUnits e
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e

-- Plutus ExBudget should remain intact when translating to and from the ExUnits type
exBudgetToExUnitsRoundTrip :: Gen Property
exBudgetToExUnitsRoundTrip = do
  e <- arbitrary
  let result = transExUnits <$> exBudgetToExUnits e
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e

dataSpec :: Spec
dataSpec = describe "ToPlutusData" $ do
  describe "RoundTrip" $ do
    roundTripPlutusDataSpec @Version
    roundTripPlutusDataSpec @ProtVer
    roundTripPlutusDataSpec @Rational
    roundTripPlutusDataSpec @UnitInterval
    roundTripPlutusDataSpec @NonNegativeInterval
    roundTripPlutusDataSpec @PositiveInterval
    roundTripPlutusDataSpec @CostModels
    roundTripPlutusDataSpec @ExUnits
    roundTripPlutusDataSpec @Prices
    roundTripPlutusDataSpec @Coin
    roundTripPlutusDataSpec @(CompactForm Coin)
    roundTripPlutusDataSpec @CoinPerByte
    roundTripPlutusDataSpec @MaxPledgeLeverage
    roundTripPlutusDataSpec @Word32
    roundTripPlutusDataSpec @Word16
    roundTripPlutusDataSpec @Word8
    roundTripPlutusDataSpec @EpochInterval
    roundTripPlutusDataSpec @SlotInterval
    roundTripPlutusDataSpec @Natural
    roundTripPlutusDataSpec @Integer
    roundTripPlutusDataSpec @Word
    roundTripPlutusDataSpec @(NonZero Integer)
    roundTripPlutusDataSpec @[Word]
    roundTripPlutusDataSpec @[Word8]
    roundTripPlutusDataSpec @(Map Word Version)
  describe "Same as ToData" $ do
    describe "PlutusV4" $ do
      sameAsToBuiltinDataSpec @Integer id
      sameAsToBuiltinDataSpec @Word8 toInteger
      sameAsToBuiltinDataSpec @Word16 toInteger
      sameAsToBuiltinDataSpec @Word32 toInteger
      sameAsToBuiltinDataSpec @Natural toInteger
      sameAsToBuiltinDataSpec @Rational PV4.fromHaskellRatio
      sameAsToBuiltinDataSpec @UnitInterval (PV4.fromHaskellRatio . unboundRational)
      sameAsToBuiltinDataSpec @NonNegativeInterval (PV4.fromHaskellRatio . unboundRational)
      sameAsToBuiltinDataSpec @PositiveInterval (PV4.fromHaskellRatio . unboundRational)
      sameAsToBuiltinDataSpec @MaxPledgeLeverage $
        fmap (PV4.fromHaskellRatio . unboundRational)
          . strictMaybeToMaybe
          . unMaxPledgeLeverage
      sameAsToBuiltinDataSpec @Coin (PV4.Lovelace . unCoin)
      sameAsToBuiltinDataSpec @(CompactForm Coin) (PV4.Lovelace . unCoin . fromCompact)
      sameAsToBuiltinDataSpec @CoinPerByte (PV4.Lovelace . unCoin . fromCompact . unCoinPerByte)
      sameAsToBuiltinDataSpec @(StrictMaybe Integer) strictMaybeToMaybe
      sameAsToBuiltinDataSpec @[Integer] id
      sameAsToBuiltinDataSpec @(Map Integer Integer) (PV4.unsafeFromList . Map.toList)
      sameAsToBuiltinDataSpec $ \ProtVer {pvMajor, pvMinor} -> PV4.ProtocolVersion (getVersion pvMajor) (toInteger pvMinor)

-- | Only programs consisting of a single builtin are checked, so this does not cover new constant
-- types or term constructors.
decodePlutusRunnableSpec :: Spec
decodePlutusRunnableSpec =
  describe "decodePlutusRunnable" $ do
    describe "deserialises to the correct Plutus version for any given protocol version" $ do
      forM_ [minBound .. maxBound :: Language] $ \lang -> do
        describe (show lang) $
          withSLanguage lang $ \(_ :: SLanguage l) ->
            forM_ PLC.knownVersions $ \plcVersion -> do
              describe ("PLC Version " <> showPlcVersion plcVersion) $
                forM_ [plutusLanguageIntroducedIn @l .. maxBound :: Version] $ \protVer -> do
                  it ("ProtVer " <> show protVer) $ do
                    plutusLedgerLang <- expectRight $ plutusLanguageOf lang
                    forM_ [minBound .. maxBound :: DefaultFun] $ \f -> do
                      let bs = P.serialiseUPLC (Program () plcVersion (Builtin () f))
                          actualLedgerRes = plutusRunnableResult $ decodePlutusRunnable protVer (Plutus @l $ PlutusBinary bs)
                          actualPlutusRes = P.deserialiseScript plutusLedgerLang (P.MajorProtocolVersion $ getVersion protVer) bs
                      -- We can't simply use `actualLedgerRes `shouldBe` actualPlutusRes`, because
                      -- on failure the flat decoder renders raw memory addresses of its input
                      -- buffer into the error message (`BadEncoding (Ptr, S {currPtr = ...})`).
                      -- Each call decodes from a freshly allocated buffer, so two otherwise
                      -- identical errors never compare equal. Instead we require both sides to
                      -- agree on success or failure, and compare the results only on success.
                      case (actualLedgerRes, actualPlutusRes) of
                        (Right ledger, Right plutus) -> ledger `shouldBe` plutus
                        (Left _, Left _) -> pure ()
                        _ ->
                          expectationFailure $
                            "Ledger and Plutus disagree on "
                              <> show f
                              <> ",  ledger: "
                              <> show actualLedgerRes
                              <> ",  plutus: "
                              <> show actualPlutusRes

evaluatePlutusSpec :: Spec
evaluatePlutusSpec =
  describe "evaluatePlutusRunnable" $ do
    describe "evaluates with the correct Plutus version for any given protocol version" $ do
      forM_ [minBound .. maxBound :: Language] $ \lang -> do
        let evalCtx = testingEvaluationContext lang
            budget = transExUnits (ExUnits 10_000_000 10_000_000_000)
        describe (show lang) $
          withSLanguage lang $ \(slang :: SLanguage l) -> do
            let (args, argsData) = testPlutusArgs slang
            forM_ PLC.knownVersions $ \plcVersion -> do
              describe ("PLC Version " <> showPlcVersion plcVersion) $
                forM_ [plutusLanguageIntroducedIn @l .. maxBound :: Version] $ \protVer -> do
                  it ("ProtVer " <> show protVer) $ do
                    plutusLedgerLang <- expectRight $ plutusLanguageOf lang
                    forM_ (evaluationSpecTestScripts (length argsData)) $ \(scriptName, term) -> do
                      let bs = P.serialiseUPLC (Program () plcVersion term)
                          majorProtVer = P.MajorProtocolVersion $ getVersion protVer
                          maxMajorProtVer = P.MajorProtocolVersion $ getVersion (maxBound :: Version)
                      -- Deserialise at the latest protocol version, so that the script can be evaluated
                      -- at every protocol version, including those where it would not deserialise.
                      -- This exercises the evaluation-time checks, which depend on the ledger language.
                      -- Deserialisation itself is covered by `decodePlutusRunnableSpec`.
                      forM_ @(Either P.ScriptDecodeError) (P.deserialiseScript plutusLedgerLang maxMajorProtVer bs) $ \sfe -> do
                        let actualLedgerRes = evaluatePlutusRunnable @l protVer P.Quiet evalCtx budget sfe args
                            actualPlutusRes =
                              P.evaluateScriptRestricting plutusLedgerLang majorProtVer P.Quiet evalCtx budget sfe argsData
                        -- Unlike in `decodePlutusRunnableSpec`, evaluation errors contain no
                        -- memory addresses, so the results, including errors, can be compared
                        -- exactly.
                        unless (actualLedgerRes == actualPlutusRes) $
                          expectationFailure $
                            "Ledger and Plutus disagree on evaluating "
                              <> scriptName
                              <> ",  ledger: "
                              <> show actualLedgerRes
                              <> ",  plutus: "
                              <> show actualPlutusRes
                        let actualLedgerBudgetRes = evaluatePlutusRunnableBudget @l protVer P.Quiet evalCtx sfe args
                            actualPlutusBudgetRes =
                              P.evaluateScriptCounting plutusLedgerLang majorProtVer P.Quiet evalCtx sfe argsData
                        unless (actualLedgerBudgetRes == actualPlutusBudgetRes) $
                          expectationFailure $
                            "Ledger and Plutus disagree on the budget of "
                              <> scriptName
                              <> ",  ledger: "
                              <> show actualLedgerBudgetRes
                              <> ",  plutus: "
                              <> show actualPlutusBudgetRes

-- | Scripts that accept the given number of arguments. Plutus V1 and V2 accept any result,
-- while starting with V3 only `()` is a successful result, which allows us to detect when
-- a script is evaluated with the wrong Plutus ledger language.
evaluationSpecTestScripts :: Int -> [(String, Term DeBruijn DefaultUni DefaultFun ())]
evaluationSpecTestScripts arity =
  [ ("always succeeds with ()", mkIterLamAbs scriptArgs unit)
  ,
    ( "chooseUnit returning ()"
    , mkIterLamAbs scriptArgs $ mkIterAppNoAnn (Force () (Builtin () ChooseUnit)) [unit, unit]
    )
  ,
    ( "ifThenElse on equalsData returning ()"
    , mkIterLamAbs scriptArgs $
        mkIterAppNoAnn
          (Force () (Builtin () IfThenElse))
          [mkIterAppNoAnn (Builtin () EqualsData) [scriptContextVar, scriptContextVar], unit, unit]
    )
  ,
    ( "addInteger returning an integer"
    , mkIterLamAbs scriptArgs $
        mkIterAppNoAnn (Builtin () AddInteger) [mkConstant () (1 :: Integer), mkConstant () (2 :: Integer)]
    )
  ]
  where
    unit = mkConstant () ()
    -- Last script argument, i.e. the script context
    scriptContextVar = Var () (DeBruijn 1)
    -- One binder per argument the script is applied to. With DeBruijn names, binders are always index 0.
    -- For V1,V2, the arity is 2: [redeemer, scriptcontext] (3 for spending scripts, which adds the datum first)
    -- For V3,V4, the arity is 1: [scriptcontext]
    scriptArgs = replicate arity (UVarDecl () (DeBruijn 0))

-- | Minimal script arguments for every language, together with their expected `P.Data`
-- representation that Plutus applies the script to.
--
-- The script contexts are built by hand, rather than translated from a transaction, so that the
-- `P.Data` passed to Plutus is computed independently of the ledger. This way, the test also
-- checks that the ledger converts `PlutusArgs` to the arguments Plutus expects.
testPlutusArgs :: SLanguage l -> (PlutusArgs l, [P.Data])
testPlutusArgs = \case
  SPlutusV1 ->
    ( PlutusV1Args (LegacyPlutusArgs2 redeemer scriptContextV1)
    , [redeemer, PV1.toData scriptContextV1]
    )
  SPlutusV2 ->
    ( PlutusV2Args (LegacyPlutusArgs2 redeemer scriptContextV2)
    , [redeemer, PV2.toData scriptContextV2]
    )
  SPlutusV3 -> (PlutusV3Args scriptContextV3, [PV3.toData scriptContextV3])
  SPlutusV4 -> (PlutusV4Args scriptContextV4, [PV4.toData scriptContextV4])
  where
    redeemer = PV1.I 0
    currencySymbol = PV1.CurrencySymbol ""
    scriptContextV1 =
      PV1.ScriptContext
        { PV1.scriptContextTxInfo =
            PV1.TxInfo
              { PV1.txInfoInputs = []
              , PV1.txInfoOutputs = []
              , PV1.txInfoFee = mempty
              , PV1.txInfoMint = mempty
              , PV1.txInfoDCert = []
              , PV1.txInfoWdrl = []
              , PV1.txInfoValidRange = PV1.always
              , PV1.txInfoSignatories = []
              , PV1.txInfoData = []
              , PV1.txInfoId = PV1.TxId ""
              }
        , PV1.scriptContextPurpose = PV1.Minting currencySymbol
        }
    scriptContextV2 =
      PV2.ScriptContext
        { PV2.scriptContextTxInfo =
            PV2.TxInfo
              { PV2.txInfoInputs = []
              , PV2.txInfoReferenceInputs = []
              , PV2.txInfoOutputs = []
              , PV2.txInfoFee = mempty
              , PV2.txInfoMint = mempty
              , PV2.txInfoDCert = []
              , PV2.txInfoWdrl = PV2.unsafeFromList []
              , PV2.txInfoValidRange = PV2.always
              , PV2.txInfoSignatories = []
              , PV2.txInfoRedeemers = PV2.unsafeFromList []
              , PV2.txInfoData = PV2.unsafeFromList []
              , PV2.txInfoId = PV2.TxId ""
              }
        , PV2.scriptContextPurpose = PV2.Minting currencySymbol
        }
    scriptContextV3 =
      PV3.ScriptContext
        { PV3.scriptContextTxInfo =
            PV3.TxInfo
              { PV3.txInfoInputs = []
              , PV3.txInfoReferenceInputs = []
              , PV3.txInfoOutputs = []
              , PV3.txInfoFee = PV3.Lovelace 0
              , PV3.txInfoMint = PV3.emptyMintValue
              , PV3.txInfoTxCerts = []
              , PV3.txInfoWdrl = PV3.unsafeFromList []
              , PV3.txInfoValidRange = PV3.always
              , PV3.txInfoSignatories = []
              , PV3.txInfoRedeemers = PV3.unsafeFromList []
              , PV3.txInfoData = PV3.unsafeFromList []
              , PV3.txInfoId = PV3.TxId ""
              , PV3.txInfoVotes = PV3.unsafeFromList []
              , PV3.txInfoProposalProcedures = []
              , PV3.txInfoCurrentTreasuryAmount = Nothing
              , PV3.txInfoTreasuryDonation = Nothing
              }
        , PV3.scriptContextRedeemer = PV3.Redeemer (PV3.dataToBuiltinData redeemer)
        , PV3.scriptContextScriptInfo = PV3.MintingScript currencySymbol
        }
    scriptContextV4 =
      PV4.ScriptContext
        { PV4.scriptContextTxInfo =
            PV4.TxInfo
              { PV4.txInfoId = PV4.TxId ""
              , PV4.txInfoSubTxIx = Nothing
              , PV4.txInfoInputs = []
              , PV4.txInfoReferenceInputs = []
              , PV4.txInfoOutputs = []
              , PV4.txInfoMint = PV4.emptyMintValue
              , PV4.txInfoTxCerts = []
              , PV4.txInfoWithdrawals = PV4.unsafeFromList []
              , PV4.txInfoDirectDeposits = PV4.unsafeFromList []
              , PV4.txInfoAccountBalanceIntervals = PV4.AccountBalanceIntervals (PV4.unsafeFromList [])
              , PV4.txInfoValidRange = PV4.POSIXTimeRange Nothing Nothing
              , PV4.txInfoGuards = []
              , PV4.txInfoRequiredTopLevelGuards = PV4.unsafeFromList []
              , PV4.txInfoRedeemers = PV4.unsafeFromList []
              , PV4.txInfoData = PV4.unsafeFromList []
              , PV4.txInfoVotes = PV4.unsafeFromList []
              , PV4.txInfoProposalProcedures = []
              , PV4.txInfoCurrentTreasuryAmount = Nothing
              , PV4.txInfoTreasuryDonation = PV4.Lovelace 0
              }
        , PV4.scriptContextRedeemer = PV4.Redeemer (PV3.dataToBuiltinData redeemer)
        , PV4.scriptContextScriptInfo = PV4.MintingScript currencySymbol
        , PV4.scriptContextScriptHash = PV4.ScriptHash ""
        }
