{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Invalid (spec, alonzoEraSpecificSpec) where

import Cardano.Ledger.Allegra.Scripts (AllegraEraScript (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (CollectErrors),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (TxDats (..), unRedeemersL)
import Cardano.Ledger.BaseTypes (Mismatch (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
  hashData,
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState (epochStateStakePoolsL, nesEsL)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (<>~), (^.))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysSucceedsNoDatum,
  alwaysSucceedsWithDatum,
  redeemerSameAsDatum,
 )

spec :: forall era. AlonzoEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "Invalid transactions" $ do
  it "Phase 1 script failure" $ do
    -- Script will be invalid because slot 100 will be in the future
    scriptHash <- impAddNativeScript $ mkTimeStart 100
    txIn <- produceScript scriptHash
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    submitFailingTx tx [injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]]

  let resetAddrWits tx = updateAddrTxWits $ tx & witsTxL . addrTxWitsL .~ []
      fixupResetAddrWits = fixupPPHash >=> resetAddrWits

  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let redeemerSameAsDatumHash = hashPlutusScript $ redeemerSameAsDatum slang
            alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang

        it "MissingRedeemers" $ do
          let scriptHash = redeemerSameAsDatumHash
          txIn <- produceScript scriptHash
          let missingRedeemer = mkSpendingPurpose $ AsItem txIn
          let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
          withPostFixup (fixupResetAddrWits . (witsTxL . rdmrsTxWitsL .~ mempty)) $
            submitFailingTx
              tx
              [ injectFailure $
                  MissingRedeemers [(missingRedeemer, scriptHash)]
              , injectFailure $
                  CollectErrors [NoRedeemer missingRedeemer]
              ]

        it "MissingRequiredDatums" $ do
          txIn <- produceScript redeemerSameAsDatumHash
          let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
          let missingDatum = hashData @era (Data (P.I 3))
          withPostFixup (fixupResetAddrWits . (witsTxL . datsTxWitsL .~ mempty)) $
            submitFailingTx
              tx
              [injectFailure $ MissingRequiredDatums [missingDatum] []]

        it "NotAllowedSupplementalDatums" $ do
          txIn <- produceScript redeemerSameAsDatumHash
          let extraDatumHash = hashData @era (Data (P.I 30))
          let extraDatum = Data (P.I 30)
          let tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL .~ [txIn]
                  & witsTxL . datsTxWitsL .~ TxDats (Map.singleton extraDatumHash extraDatum)
          submitFailingTx
            tx
            [injectFailure $ NotAllowedSupplementalDatums [extraDatumHash] []]

        describe "PPViewHashesDontMatch" $ do
          let
            testHashMismatch badHash = do
              txIn <- produceScript redeemerSameAsDatumHash
              goodHashTx <- fixupTx $ mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
              badHashTx <-
                resetAddrWits $ goodHashTx & bodyTxL . scriptIntegrityHashTxBodyL .~ badHash
              let goodHash = goodHashTx ^. bodyTxL . scriptIntegrityHashTxBodyL
              withNoFixup $
                submitFailingTx
                  badHashTx
                  [ injectFailure $
                      PPViewHashesDontMatch Mismatch {mismatchSupplied = badHash, mismatchExpected = goodHash}
                  ]

          it "Mismatched" . whenMajorVersionAtMost @10 $
            testHashMismatch . SJust =<< arbitrary
          it "Missing" . whenMajorVersionAtMost @10 $
            testHashMismatch SNothing

        it "UnspendableUTxONoDatumHash" $ do
          txIn <- impAnn "Produce script at a txout with a missing datahash" $ do
            let scriptHash = redeemerSameAsDatumHash
            let addr = mkAddr scriptHash StakeRefNull
            let tx =
                  mkBasicTx mkBasicTxBody
                    & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr mempty]
            let resetDataHash = dataHashTxOutL .~ SNothing
            let resetTxOutDataHash =
                  bodyTxL . outputsTxBodyL
                    %~ ( \case
                           h :<| r -> resetDataHash h :<| r
                           _ -> error "Expected non-empty outputs"
                       )

            txInAt 0
              <$> withPostFixup
                (fixupResetAddrWits <$> resetTxOutDataHash)
                (submitTx tx)
          let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
          -- PlutusV3 no longer requires a spending Datum, but it should still fail since the
          -- actual script expects it
          if lang >= PlutusV3
            then submitPhase2Invalid_ tx
            else submitFailingTx tx [injectFailure $ UnspendableUTxONoDatumHash [txIn]]

        it "Missing phase-2 script witness" $ do
          let scriptHash = alwaysSucceedsWithDatumHash
          txIn <- produceScript scriptHash
          let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
              dropScriptWitnesses =
                pure
                  . (witsTxL . scriptTxWitsL .~ mempty)
                  . (witsTxL . datsTxWitsL .~ mempty)
                  . (witsTxL . rdmrsTxWitsL .~ mempty)
              resetScriptHash = pure . (bodyTxL . scriptIntegrityHashTxBodyL .~ SNothing)
          withPostFixup (dropScriptWitnesses >=> resetScriptHash >=> resetAddrWits) $
            submitFailingTx tx [injectFailure $ MissingScriptWitnessesUTXOW [scriptHash]]

        it "Redeemer with incorrect purpose" $ do
          let scriptHash = alwaysSucceedsWithDatumHash
          txIn <- produceScript scriptHash
          let tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . inputsTxBodyL .~ [txIn]
                  & witsTxL . rdmrsTxWitsL . unRedeemersL .~ mintingRedeemers
              mintingRedeemers = Map.singleton (mkMintingPurpose $ AsIx 0) (Data $ P.I 32, ExUnits 0 0)
              isSpender = isJust . toSpendingPurpose @era @AsIx
              removeSpenders = Map.filterWithKey (const . not . isSpender)
              dropSpendingRedeemers = pure . (witsTxL . rdmrsTxWitsL . unRedeemersL %~ removeSpenders)
          withPostFixup (dropSpendingRedeemers >=> fixupPPHash >=> resetAddrWits) $
            submitFailingTx
              tx
              [ injectFailure $
                  ExtraRedeemers [mkMintingPurpose $ AsIx 0]
              , injectFailure $
                  MissingRedeemers [(mkSpendingPurpose $ AsItem txIn, scriptHash)]
              , injectFailure $
                  CollectErrors [NoRedeemer $ mkSpendingPurpose $ AsItem txIn]
              ]

        it "Missing witness for collateral input" $ do
          let scriptHash = alwaysSucceedsWithDatumHash
          scriptInput <- produceScript scriptHash
          (collateralHash, collateralAddr) <- freshKeyAddr
          collateralInput <- sendCoinTo collateralAddr $ Coin 3_000_000
          let
            tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL <>~ [scriptInput]
                & bodyTxL . collateralInputsTxBodyL <>~ [collateralInput]
            isOtherWitness wit = witVKeyHash wit /= asWitness collateralHash
            dropCollateralWitness = witsTxL . addrTxWitsL %~ Set.filter isOtherWitness
          withPostFixup (pure . dropCollateralWitness) $
            submitFailingTx
              tx
              [injectFailure $ MissingVKeyWitnessesUTXOW [asWitness collateralHash]]

        -- Post-Alonzo eras produce additional post-Alonzo predicate failures that we can't include here
        unless (lang > eraMaxLanguage @AlonzoEra) $ do
          describe "Extra Redeemer" $ do
            let
              testPurpose purpose = do
                txIn <- produceScript alwaysSucceedsWithDatumHash
                let redeemer = (Data $ P.I 442, ExUnits 443 444) -- Needs to be unique
                    tx =
                      mkBasicTx mkBasicTxBody
                        & bodyTxL . inputsTxBodyL <>~ [txIn]
                        & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert purpose redeemer
                txFixed <- fixupTx tx
                -- The `Ix` of the redeemer may have been changed by `fixupRedeemerIndices`
                let fixedRedeemers = txFixed ^. witsTxL . rdmrsTxWitsL . unRedeemersL
                    extraRedeemers = Map.keys $ Map.filter (== redeemer) fixedRedeemers
                withNoFixup $
                  submitFailingTx
                    txFixed
                    [injectFailure $ ExtraRedeemers extraRedeemers]

            it "Minting" $
              testPurpose (mkMintingPurpose $ AsIx 2)
            it "Spending" $
              testPurpose (mkSpendingPurpose $ AsIx 99)

alonzoEraSpecificSpec ::
  forall era.
  (AlonzoEraImp era, ShelleyEraTxCert era) =>
  SpecWith (ImpInit (LedgerSpec era))
alonzoEraSpecificSpec = describe "Invalid transactions" $ do
  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang
            alwaysSucceedsNoDatumHash = hashPlutusScript $ alwaysSucceedsNoDatum slang

        it "No ExtraRedeemers on same script certificates" $ do
          Positive n <- arbitrary
          replicateM_ n $ freshKeyHash >>= registerPool
          pools <- getsNES $ nesEsL . epochStateStakePoolsL
          poolId <- elements $ Map.keys pools
          let scriptHash = alwaysSucceedsNoDatumHash
              cred = ScriptHashObj scriptHash
              certs =
                [ mkRegTxCert cred
                , mkDelegStakeTxCert cred poolId
                , mkUnRegTxCert cred
                ]
          tx <- submitTx $ mkBasicTx (mkBasicTxBody & certsTxBodyL .~ certs)
          let redeemers = tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL
          Map.keys redeemers
            `shouldBe` [ mkCertifyingPurpose $ AsIx 1
                       , mkCertifyingPurpose $ AsIx 2
                       ]

        -- Post-Alonzo eras produce additional post-Alonzo predicate failures that we can't include here
        unless (lang > eraMaxLanguage @AlonzoEra) $ do
          describe "Extra Redeemer" $ do
            it "Multiple equal plutus-locked certs" $ do
              let scriptHash = alwaysSucceedsWithDatumHash
              Positive n <- arbitrary
              replicateM_ n $ freshKeyHash >>= registerPool
              pools <- getsNES $ nesEsL . epochStateStakePoolsL
              poolId <- elements $ Map.keys pools
              let cred = ScriptHashObj scriptHash
                  certs =
                    [ mkRegTxCert cred --               0: Doesn't require a redeemer
                    , mkDelegStakeTxCert cred poolId -- 1: Needs a redeemer
                    , mkDelegStakeTxCert cred poolId -- 2: Duplicate, ignored, no redeemer needed
                    ]
                  redeemer = (Data (P.I 32), ExUnits 15_000 5_000_000)
                  redeemers = Map.fromList [(mkCertifyingPurpose (AsIx i), redeemer) | i <- [1 .. 2]]
                  tx =
                    mkBasicTx mkBasicTxBody
                      & bodyTxL . certsTxBodyL <>~ certs
                      & witsTxL . rdmrsTxWitsL . unRedeemersL <>~ redeemers
              submitFailingTx
                tx
                [injectFailure $ ExtraRedeemers [mkCertifyingPurpose (AsIx 2)]]
