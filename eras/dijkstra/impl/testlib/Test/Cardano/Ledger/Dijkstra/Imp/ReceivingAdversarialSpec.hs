{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.ReceivingAdversarialSpec (
  spec,
  structuralSpec,
  concreteEvaluatorSpec,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (CollectError (..))
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL, unTxDatsL)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes (Inject (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraSubUtxowPredFailure (..))
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Keys (WitVKey (WitVKey), asWitness, witVKeyHash)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  Data (..),
  Datum (..),
  ExUnits (..),
  Plutus,
  SLanguage (..),
  dataToBinaryData,
  hashData,
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.State (UTxO (..))
import Control.Monad ((>=>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import qualified PlutusLedgerApi.V1 as P
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysFailsNoDatum,
  alwaysSucceedsNoDatum,
  receivingEvenDatum,
 )

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "CIP-160 Receiving adversarial integration" $ do
  structuralSpec @era
  concreteEvaluatorSpec @era

-- | Thirteen cases compare structural checks and state transitions with the
-- executable specification, including a declared-invalid collateral-only path.
structuralSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
structuralSpec = describe "Structural validation and state transitions" $ do
  it "rejects a Receiving key signature over a different body" $ do
    key <- freshKeyHash @Payment
    pair <- getKeyPair (asWitness key)
    staleHash <- arbitrary
    let tx =
          mkBasicTx $
            mkBasicTxBody
              & outputsTxBodyL
                .~ [mkCoinTxOut (AddrProtected Testnet (KeyHashObj key) StakeRefNull) (Coin 2_000_000)]
        replaceSignature =
          pure
            . ( witsTxL . addrTxWitsL %~ \wits ->
                  Set.filter ((/= asWitness key) . witVKeyHash) wits <> mkWitnessesVKey staleHash [pair]
              )
    withPostFixup replaceSignature $
      submitFailingTx tx [injectFailure $ Conway.InvalidWitnessesUTXOW [vKey pair]]

  it "requires a Receiving child's key signature to cover that child's body" $ do
    key <- freshKeyHash @Payment
    pair <- getKeyPair (asWitness key)
    staleHash <- arbitrary
    let child =
          mkBasicTx $
            mkBasicTxBody
              & outputsTxBodyL
                .~ [mkCoinTxOut (AddrProtected Testnet (KeyHashObj key) StakeRefNull) (Coin 2_000_000)]
        replaceSignature =
          pure
            . ( witsTxL . addrTxWitsL %~ \wits ->
                  Set.filter ((/= asWitness key) . witVKeyHash) wits <> mkWitnessesVKey staleHash [pair]
              )
    withPostFixupSubTxs replaceSignature $
      submitFailingSubTx child [injectFailure $ SubInvalidWitnessesUTXOW @era [vKey pair]]

  it "invalidates signatures when protection is toggled after signing" $ do
    key <- freshKeyHash @Payment
    pair <- getKeyPair (asWitness key)
    let ordinary = Addr Testnet (KeyHashObj key) StakeRefNull
        protected = AddrProtected Testnet (KeyHashObj key) StakeRefNull
        tx = mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [mkCoinTxOut ordinary (Coin 2_000_000)]
        toggleProtection fixed =
          pure $
            fixed
              & witsTxL
                . addrTxWitsL
                <>~ mkWitnessesVKey (hashAnnotated (fixed ^. bodyTxL)) [pair]
              & bodyTxL
                . outputsTxBodyL
                . ix 0
                . addrTxOutL
                .~ protected
    withPostFixup toggleProtection $
      submitFailingTxM tx $ \fixed -> do
        invalid <-
          expectJust $ NE.nonEmpty [vk | WitVKey vk _ <- Set.toList (fixed ^. witsTxL . addrTxWitsL)]
        pure [injectFailure $ Conway.InvalidWitnessesUTXOW invalid]

  it "requires spending authorization after a protected key output was authorized at creation" $ do
    key <- freshKeyHash @Payment
    let address = AddrProtected Testnet (KeyHashObj key) StakeRefNull
    created <-
      submitTx $
        mkBasicTx $
          mkBasicTxBody
            & outputsTxBodyL
              .~ [mkCoinTxOut address (Coin 2_000_000)]
    let input = txInAt 0 created
        spending = mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [input]
        dropSpendingSignature = pure . (witsTxL . addrTxWitsL %~ Set.filter ((/= asWitness key) . witVKeyHash))
    UTxO entries <- getUTxO
    fmap (^. addrTxOutL) (Map.lookup input entries) `shouldBe` Just address
    withPostFixup dropSpendingSignature $
      submitFailingTx
        spending
        [injectFailure $ Conway.MissingVKeyWitnessesUTXOW (NES.singleton (asWitness key))]
    submitTx_ spending

  it "cannot witness Receiving with a reference script only on the newly created output" $ do
    sh <- impAddNativeScript (RequireAllOf [])
    let script = fromNativeScript @era (RequireAllOf [])
        out =
          mkCoinTxOut (AddrProtected Testnet (ScriptHashObj sh) StakeRefNull) (Coin 2_000_000)
            & referenceScriptTxOutL
              .~ SJust script
        tx = mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [out]
        removeWitness = pure . (witsTxL . scriptTxWitsL %~ Map.delete sh)
    withPostFixup removeWitness $
      submitFailingTx tx [injectFailure $ Conway.MissingScriptWitnessesUTXOW (NES.singleton sh)]

  it "uses one native script for minting and Receiving at a protected base address" $ do
    sh <- impAddNativeScript (RequireAllOf [])
    staking <- KeyHashObj <$> freshKeyHash
    let assetName = AssetName "receiving"
        asset = MultiAsset (Map.singleton (PolicyID sh) (Map.singleton assetName 1))
        address = AddrProtected Testnet (ScriptHashObj sh) (StakeRefBase staking)
        tx =
          mkBasicTx $
            mkBasicTxBody
              & mintTxBodyL .~ asset
              & outputsTxBodyL .~ [mkBasicTxOut address (MaryValue (Coin 3_000_000) asset)]
    created <- submitTx tx
    UTxO entries <- getUTxO
    fmap (^. addrTxOutL) (Map.lookup (txInAt 0 created) entries) `shouldBe` Just address
    created ^. witsTxL . rdmrsTxWitsL . unRedeemersL `shouldBe` mempty

  it "checks every protected grouped output and ignores an ordinary destination with the same hash" $ do
    let plutus = receivingEvenDatum SPlutusV4
        sh = hashPlutusScript plutus
        ordinaryOdd =
          mkCoinTxOut (Addr Testnet (ScriptHashObj sh) StakeRefNull) (Coin 2_000_000)
            & datumTxOutL
              .~ inlineDatum 3
    tx <- receivingTx plutus [inlineDatum 2, inlineDatum 4]
    fixed <- fixupTx (tx & bodyTxL . outputsTxBodyL %~ (SSeq.|> ordinaryOdd))
    length [() | ReceivingPurpose _ <- Map.keys (fixed ^. witsTxL . rdmrsTxWitsL . unRedeemersL)]
      `shouldBe` 1
    withNoFixup (submitTx_ fixed)

  it "does not require a hashed output datum's preimage when Receiving deliberately ignores it" $ do
    let datum = Data @era (P.I 17)
        datumHash = DatumHash (hashData datum)
    tx <- receivingTx (alwaysSucceedsNoDatum SPlutusV4) [datumHash]
    withPostFixup (fixupPPHash . (witsTxL . datsTxWitsL .~ mempty) >=> rederiveAddrTxWits) $
      submitTx_ tx

  it "allows a hashed output datum's preimage as a supplemental witness" $ do
    let datum = Data @era (P.I 18)
    tx <- receivingTx (alwaysSucceedsNoDatum SPlutusV4) [DatumHash (hashData datum)]
    withPostFixup
      ( fixupPPHash . (witsTxL . datsTxWitsL . unTxDatsL .~ Map.singleton (hashData datum) datum)
          >=> rederiveAddrTxWits
      )
      $ submitTx_ tx

  it "reports the absent Receiving redeemer instead of repairing it" $ do
    let plutus = alwaysSucceedsNoDatum SPlutusV4
        sh = hashPlutusScript plutus
        missing = ReceivingPurpose (AsItem sh)
    tx <- receivingTx plutus [NoDatum]
    withPostFixup (fixupPPHash . (witsTxL . rdmrsTxWitsL .~ mempty) >=> rederiveAddrTxWits) $
      submitFailingTx
        tx
        [ injectFailure $ Conway.CollectErrors [NoRedeemer missing]
        , injectFailure $ Alonzo.MissingRedeemers [(missing, sh)]
        ]

  it "rejects an out-of-range Receiving redeemer alongside the valid one" $ do
    tx <- receivingTx (alwaysSucceedsNoDatum SPlutusV4) [NoDatum]
    let extra = ReceivingPurpose (AsIx maxBound)
        insertExtra =
          witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.insert extra (Data (P.I 0), ExUnits 0 0)
    withPostFixup (fixupPPHash . insertExtra >=> rederiveAddrTxWits) $
      submitFailingTx
        tx
        [ injectFailure $ Alonzo.ExtraRedeemers [extra]
        , injectFailure $
            Conway.CollectErrors [BadTranslation (inject (RedeemerPointerPointsToNothing extra))]
        ]

  it "uses a reference script on a consumed ordinary key input to authorize Receiving" $ do
    tx <- receivingTx (alwaysSucceedsNoDatum SPlutusV4) [NoDatum]
    let references = tx ^. bodyTxL . referenceInputsTxBodyL
    submitTx_ (tx & bodyTxL . inputsTxBodyL <>~ references & bodyTxL . referenceInputsTxBodyL .~ mempty)
  it "applies only collateral effects for a grouped Receiving failure declared phase-2 invalid" $ do
    tx <- receivingTx (receivingEvenDatum SPlutusV4) [inlineDatum 2, inlineDatum 3]
    fixed <- fixupTx tx
    UTxO before <- getUTxO
    failed <- withNoFixup $ submitTx (fixed & isPhase2ValidTxL .~ Phase2Invalid)
    UTxO final <- getUTxO
    let ordinaryCount = SSeq.length (failed ^. bodyTxL . outputsTxBodyL)
    forM_ [0 .. ordinaryCount - 1] $ \outputIndex ->
      Map.member (txInAt outputIndex failed) final `shouldBe` False
    forM_ (Set.toList (failed ^. bodyTxL . inputsTxBodyL)) $ \input -> do
      Map.member input before `shouldBe` True
      Map.lookup input final `shouldBe` Map.lookup input before
    forM_ (Set.toList (failed ^. bodyTxL . collateralInputsTxBodyL)) $ \input -> do
      Map.member input before `shouldBe` True
      Map.member input final `shouldBe` False
    case failed ^. bodyTxL . collateralReturnTxBodyL of
      SJust returned -> Map.lookup (txInAt ordinaryCount failed) final `shouldBe` Just returned
      SNothing -> Map.member (txInAt ordinaryCount failed) final `shouldBe` False

-- | These cases require the concrete Plutus evaluator. The Dijkstra conformance
-- adapter sets extValidPlutusScript from txtopIsValid (the transaction's declared
-- flag); it does not evaluate UPLC and cannot independently detect a validity
-- mismatch or prove that a particular grouped output caused script failure.
-- These cases remain enabled in the full ledger suite. Conformance executes
-- structuralSpec instead, including the supported declared-invalid state path.
concreteEvaluatorSpec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
concreteEvaluatorSpec = describe "Concrete Plutus evaluator outcomes" $ do
  it "creates a protected output under Receiving and spends it under the same validator" $ do
    let plutus = receivingEvenDatum SPlutusV4
        sh = hashPlutusScript plutus
        protected = AddrProtected Testnet (ScriptHashObj sh) StakeRefNull
    created <- submitTx =<< receivingTx plutus [inlineDatum 2]
    let input = txInAt 0 created
    UTxO afterCreation <- getUTxO
    fmap (^. addrTxOutL) (Map.lookup input afterCreation) `shouldBe` Just protected
    spent <- submitTx $ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [input]
    let pointers = Map.keys (spent ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
    length [() | SpendingPurpose _ <- pointers] `shouldBe` 1
    [() | ReceivingPurpose _ <- pointers] `shouldBe` []
    UTxO afterSpending <- getUTxO
    Map.member input afterSpending `shouldBe` False

  it
    "a valid first grouped output cannot hide an odd second output; failure creates no ordinary output"
    $ do
      tx <- receivingTx (receivingEvenDatum SPlutusV4) [inlineDatum 2, inlineDatum 3]
      fixed <- fixupTx tx
      before <- getUTxO
      failure <- impScriptPredicateFailure fixed
      withNoFixup $ submitFailingTx fixed [injectFailure failure]
      getUTxO >>= (`shouldBe` before)
      failed <- withNoFixup $ submitTx (fixed & isPhase2ValidTxL .~ Phase2Invalid)
      UTxO final <- getUTxO
      forM_ [0 .. SSeq.length (failed ^. bodyTxL . outputsTxBodyL) - 1] $ \outputIndex ->
        Map.member (txInAt outputIndex failed) final `shouldBe` False

  it "rejects claimed-invalid Receiving when every script succeeds, with no state effect" $ do
    tx <- receivingTx (alwaysSucceedsNoDatum SPlutusV4) [NoDatum]
    fixed <- fixupTx tx
    before <- getUTxO
    withNoFixup $
      submitFailingTx
        (fixed & isPhase2ValidTxL .~ Phase2Invalid)
        [injectFailure $ Alonzo.ValidationTagMismatch Phase2Invalid Alonzo.PassedUnexpectedly]
    getUTxO >>= (`shouldBe` before)

  it "rejects claimed-valid Receiving when its script fails, with no state effect" $ do
    tx <- receivingTx (alwaysFailsNoDatum SPlutusV4) [NoDatum]
    fixed <- fixupTx tx
    before <- getUTxO
    failure <- impScriptPredicateFailure fixed
    withNoFixup $ submitFailingTx fixed [injectFailure failure]
    getUTxO >>= (`shouldBe` before)

inlineDatum :: Era era => Integer -> Datum era
inlineDatum = Datum . dataToBinaryData . Data . P.I

-- Supplying datum/redeemer/budget and collateral explicitly keeps the contract
-- negatives outside fixups that could synthesize a datum or author a different
-- Receiving domain. The script is available only through a prior UTxO.
receivingTx ::
  forall era. DijkstraEraImp era => Plutus 'PlutusV4 -> [Datum era] -> ImpTestM era (Tx TopTx era)
receivingTx plutus datums = do
  script <- fromPlutusScript <$> mkPlutusScript plutus
  referenceAddress <- freshKeyAddrNoPtr_
  referenceTx <-
    submitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL
          . outputsTxBodyL
          .~ [mkCoinTxOut referenceAddress (Coin 3_000_000) & referenceScriptTxOutL .~ SJust script]
  collateral <- makeCollateralInput
  let sh = hashPlutusScript plutus
      protected datum =
        mkCoinTxOut (AddrProtected Testnet (ScriptHashObj sh) StakeRefNull) (Coin 3_000_000)
          & datumTxOutL
            .~ datum
  pure $
    mkBasicTx mkBasicTxBody
      & bodyTxL
        . outputsTxBodyL
        .~ SSeq.fromList (protected <$> datums)
      & bodyTxL
        . referenceInputsTxBodyL
        .~ [txInAt 0 referenceTx]
      & bodyTxL
        . collateralInputsTxBodyL
        .~ [collateral]
      & witsTxL
        . rdmrsTxWitsL
        . unRedeemersL
        .~ Map.singleton (ReceivingPurpose (AsIx 0)) (Data (P.I 0), ExUnits 5_000_000 2_000_000_000)
